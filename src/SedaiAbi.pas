unit SedaiAbi;

{$mode objfpc}{$H+}
{$codepage UTF8}
{$asmmode intel}

// ⭐⭐ THE CALLING CONVENTION, WRITTEN HERE INSTEAD OF LINKED IN.
//
// ⛔ WHY THIS EXISTS. Until 8 Sep 2026 the FFI called through libffi, and the note in SedaiFFI.pas
// argued for it: the SysV AMD64 classification is the SYSTEM's knowledge, it changes per architecture,
// and getting it wrong is silent. The owner's decision that day overrides that reading - no dependency
// on libffi, on Windows OR on Linux - and the cost is lower than it looked, for a reason worth writing
// down: the ABI work splits in two, and only one half is hard.
//
//   THE HARD HALF is classification - which eightbyte of a struct goes to which register file. It is
//   ordinary Pascal here, it is exercised by a probe library built for the purpose, and it is WRONG
//   SILENTLY: a struct passed badly does not raise, it answers wrong numbers.
//   THE EASY HALF is the trampoline - load these registers, copy this block to the stack, call. Forty
//   lines of assembly per ABI, and this project already emits x86-64 machine code by hand in two other
//   places (SedaiAot, SedaiJit).
//
// ⭐ THE SHAPE THAT MAKES IT TESTABLE: **the assembly knows nothing about the ABI.** It is handed a
// frame that says "put these values in the integer registers, these in the SSE ones, copy these words
// to the stack, call, and bring back RAX/RDX/XMM0/XMM1". Every rule about WHICH value goes where lives
// in Pascal, where it can be read and probed. A mistake in the classification is a mistake in ordinary
// code; only a mistake in the forty lines is a mistake in assembly.
//
// Covers the SysV AMD64 ABI (Linux and every other System V x86-64) and the Win64 ABI, both x86-64
// only. On any other architecture AbiAvailable answers False with a reason and the FFI is simply
// absent - exactly the state a machine without libffi used to be in.

interface

uses
  SysUtils;

type
  { What a value IS to a calling convention: not a BASIC type and not a C type, but the set of things
    the two ABIs actually distinguish between. }
  TAbiKind = (akVoid, akS8, akU8, akS16, akU16, akS32, akU32, akS64, akU64,
              akFloat, akDouble, akPointer, akStruct);
  { ⛔⛔ THERE IS DELIBERATELY NO akLongDouble, AND THAT IS A DECISION, NOT AN OVERSIGHT.
    A C `long double` on SysV x86-64 is an 80-bit x87 value in a 16-byte slot, and it does not travel in
    the integer or the SSE file at all: it is pushed on the **x87 stack**, which this trampoline does
    not touch, and it is returned in ST(0). Adding it to this enum without also teaching both
    trampolines the x87 register stack would put a value nowhere the callee looks - silently, and with
    the frame misaligned behind it, which is the failure that then shows up in the NEXT call instead of
    this one. On Win64 it is not even 80 bits: MSVC makes `long double` an alias for `double`, so the
    same declaration means two different things on the two targets.
    ⇒ It is REFUSED BY NAME in SedaiForeignDecl, so a program that declares one gets a diagnostic that
    says "long double" rather than "unknown type". Closing it means teaching the x87 stack to both
    trampolines, and that is a piece of work with its own probe, not a line added here. }

  TAbiType = class;
  TAbiTypeArray = array of TAbiType;

  { A type as the ABI sees it. A struct carries its fields in declaration order and is laid out by the
    C rules - each field at the next multiple of its own alignment, the whole rounded up to the largest
    alignment - because what these describe ARE C structs.
    ⚠️ Not FreeBASIC's layout: this side of the boundary is C's. }
  TAbiType = class
  private
    FKind: TAbiKind;
    FSize, FAlign: Integer;
    FFields: TAbiTypeArray;
    FOffsets: array of Integer;
  public
    constructor CreatePrimitive(AKind: TAbiKind);
    constructor CreateStruct(const AFields: array of TAbiType);
    property Kind: TAbiKind read FKind;
    property Size: Integer read FSize;
    property Align: Integer read FAlign;
    function FieldCount: Integer;
    function Field(I: Integer): TAbiType;
    function FieldOffset(I: Integer): Integer;
    function IsFloatKind: Boolean;
  end;

  EAbiError = class(Exception);

  { The handler a closure calls: the same shape the callers already speak - a pointer per argument, and
    a buffer to write the result into. AUser is whatever the closure was created with. }
  TAbiClosureFun = procedure(ARet: Pointer; AArgs: PPointer; AUser: Pointer);

  { ⭐ A BASIC procedure handed to C as a function pointer, which is the opposite direction from
    AbiCall: C decides when to call, and it arrives with the arguments already in registers.

    ⛔ It needs EXECUTABLE MEMORY, and only about twenty bytes of it. The generated code does one thing
    - put this closure's context in R11 (SysV) or R10 (Win64), both scratch registers no argument uses,
    and jump to a FIXED trampoline written in Pascal assembly. Everything hard (saving the argument
    registers, finding the stack arguments, calling back into Pascal) is in that fixed trampoline,
    where it can be read and reviewed. Generating the whole thing per closure would put the difficult
    part in bytes nobody can read.
    ⚠️ On a W^X system the page is made writable, filled, and only then made executable. }
  TAbiClosure = class
  private
    FBlock: Pointer;          // the executable page
    FBlockSize: PtrUInt;
    FRet: TAbiType;
    FArgs: TAbiTypeArray;
    FHandler: TAbiClosureFun;
    FUser: Pointer;
    FCode: Pointer;
    FReady: Boolean;
  public
    constructor Create(ARet: TAbiType; const AArgs: array of TAbiType;
                       AHandler: TAbiClosureFun; AUser: Pointer);
    destructor Destroy; override;
    property Code: Pointer read FCode;    // pass THIS to the C function
    property Ready: Boolean read FReady;
  end;

{ True when this build can make a foreign call at all. }
function AbiAvailable: Boolean;
function AbiUnavailableReason: string;

{ Call Fn with the given signature. Values holds one POINTER PER ARGUMENT, each to that argument's
  storage - the convention libffi used and the one the callers already speak. RetBuf receives the
  result and may be nil for a void return.
  ⛔ Raises rather than guessing when a type cannot be classified: a wrong answer here is silent. }
procedure AbiCall(Fn: Pointer; ARet: TAbiType; const AArgs: array of TAbiType;
                  const AValues: array of Pointer; ARetBuf: Pointer);

implementation

uses
  {$IFDEF WINDOWS}Windows{$ELSE}BaseUnix{$ENDIF};   // executable memory for the closures, and nothing else

const
  MAX_ARGS = 64;

{ ============================ TAbiType ============================ }

constructor TAbiType.CreatePrimitive(AKind: TAbiKind);
begin
  inherited Create;
  FKind := AKind;
  case AKind of
    akVoid:                       begin FSize := 0; FAlign := 1; end;
    akS8, akU8:                   begin FSize := 1; FAlign := 1; end;
    akS16, akU16:                 begin FSize := 2; FAlign := 2; end;
    akS32, akU32, akFloat:        begin FSize := 4; FAlign := 4; end;
  else
    begin FSize := 8; FAlign := 8; end;      // 64-bit integers, Double, every pointer
  end;
end;

constructor TAbiType.CreateStruct(const AFields: array of TAbiType);
var
  i, Ofs: Integer;
begin
  inherited Create;
  FKind := akStruct;
  SetLength(FFields, Length(AFields));
  SetLength(FOffsets, Length(AFields));
  FAlign := 1;
  Ofs := 0;
  for i := 0 to High(AFields) do
  begin
    FFields[i] := AFields[i];
    if AFields[i].Align > FAlign then FAlign := AFields[i].Align;
    // C layout: pad up to this field's own alignment.
    if (Ofs mod AFields[i].Align) <> 0 then
      Inc(Ofs, AFields[i].Align - (Ofs mod AFields[i].Align));
    FOffsets[i] := Ofs;
    Inc(Ofs, AFields[i].Size);
  end;
  // ...and the whole is rounded up to the largest member alignment, which is what makes an array of
  // the struct step by a whole number of alignments.
  if (Ofs mod FAlign) <> 0 then Inc(Ofs, FAlign - (Ofs mod FAlign));
  FSize := Ofs;
end;

function TAbiType.FieldCount: Integer;
begin
  Result := Length(FFields);
end;

function TAbiType.Field(I: Integer): TAbiType;
begin
  Result := FFields[I];
end;

function TAbiType.FieldOffset(I: Integer): Integer;
begin
  Result := FOffsets[I];
end;

function TAbiType.IsFloatKind: Boolean;
begin
  Result := FKind in [akFloat, akDouble];
end;

{ ====================== The frame the assembly reads ======================

  ⛔ THE OFFSETS ARE WRITTEN TWICE - once as the record, once as the constants the assembly uses - and
  the two are CHECKED against each other at startup. There is no way to make the assembler read a
  Pascal record's layout, and a silently wrong offset here would put an argument in the wrong register:
  the exact failure this whole unit exists to avoid. }

type
  PAbiFrame = ^TAbiFrame;
  TAbiFrame = packed record
    Fn: Pointer;                          // 0
    IntArgs: array[0..5] of PtrUInt;      // 8   RDI RSI RDX RCX R8 R9 (SysV) / RCX RDX R8 R9 (Win64)
    SseArgs: array[0..7] of QWord;        // 56  raw BITS, not a Double - see the note at AbiCall
    NSse: PtrUInt;                        // 120 -> AL, the vector-register count SysV varargs read
    Stack: Pointer;                       // 128 block of 8-byte words to copy
    NStack: PtrUInt;                      // 136
    RetRax: PtrUInt;                      // 144
    RetRdx: PtrUInt;                      // 152
    RetXmm0: QWord;                       // 160
    RetXmm1: QWord;                       // 168
  end;

const
  OF_FN     = 0;
  OF_INT    = 8;
  OF_SSE    = 56;
  OF_NSSE   = 120;
  OF_STACK  = 128;
  OF_NSTACK = 136;
  OF_RAX    = 144;
  OF_RDX    = 152;
  OF_XMM0   = 160;
  OF_XMM1   = 168;
  ABI_FRAME_SIZE = 176;

{$IF DEFINED(CPUX86_64)}
  {$DEFINE ABI_SUPPORTED}
{$ENDIF}

{$IFDEF ABI_SUPPORTED}
{$IFDEF WINDOWS}

procedure AbiCallRaw(F: PAbiFrame); assembler; nostackframe;
// Win64. The frame pointer arrives in RCX. Four argument slots, and every one of them is BOTH an
// integer register and an SSE register - which is why the caller fills both: a variadic callee reads
// the integer half of a double, and a prototyped one reads the SSE half, and nothing tells us which
// this is. Thirty-two bytes of shadow space belong to the callee and must be there before the call.
asm
  push rbp
  mov  rbp, rsp
  push rbx
  mov  rbx, rcx
  and  rsp, -16
  // Space: 32 shadow bytes plus the overflow words, rounded up to 16.
  mov  rcx, [rbx + OF_NSTACK]
  lea  rax, [rcx*8 + 32]
  add  rax, 15
  and  rax, -16
  sub  rsp, rax
  // Copy the overflow words to [rsp+32], in order.
  test rcx, rcx
  jz   @@nostack
  mov  rsi, [rbx + OF_STACK]
  xor  rdx, rdx
@@scopy:
  mov  rax, [rsi + rdx*8]
  mov  [rsp + rdx*8 + 32], rax
  inc  rdx
  dec  rcx
  jnz  @@scopy
@@nostack:
  movq xmm0, [rbx + OF_SSE +  0]
  movq xmm1, [rbx + OF_SSE +  8]
  movq xmm2, [rbx + OF_SSE + 16]
  movq xmm3, [rbx + OF_SSE + 24]
  mov  rdx, [rbx + OF_INT +  8]
  mov  r8,  [rbx + OF_INT + 16]
  mov  r9,  [rbx + OF_INT + 24]
  mov  r10, [rbx + OF_FN]
  mov  rcx, [rbx + OF_INT +  0]
  call r10
  mov  [rbx + OF_RAX], rax
  mov  [rbx + OF_RDX], rdx
  movq [rbx + OF_XMM0], xmm0
  movq [rbx + OF_XMM1], xmm1
  lea  rsp, [rbp - 8]
  pop  rbx
  pop  rbp
  ret
end;

{$ELSE}

procedure AbiCallRaw(F: PAbiFrame); assembler; nostackframe;
// SysV AMD64. The frame pointer arrives in RDI - which is also the first argument register, so it is
// moved into RBX (callee-saved, and therefore still ours after the call) and RDI is loaded LAST.
// ⛔ The stack must be 16-byte aligned AT THE CALL. `and rsp,-16` establishes it, and the extra 8 bytes
// when the word count is odd keep it: an odd number of pushes would otherwise leave it at 8, and a
// callee using aligned SSE moves faults on that - a crash that depends on how many arguments there are.
asm
  push rbp
  mov  rbp, rsp
  push rbx
  mov  rbx, rdi
  and  rsp, -16
  mov  rcx, [rbx + OF_NSTACK]
  mov  rax, rcx
  and  rax, 1
  shl  rax, 3
  sub  rsp, rax
  test rcx, rcx
  jz   @@nostack
  mov  rsi, [rbx + OF_STACK]
  lea  rsi, [rsi + rcx*8]
@@spush:
  sub  rsi, 8
  push qword [rsi]
  dec  rcx
  jnz  @@spush
@@nostack:
  movq xmm0, [rbx + OF_SSE +  0]
  movq xmm1, [rbx + OF_SSE +  8]
  movq xmm2, [rbx + OF_SSE + 16]
  movq xmm3, [rbx + OF_SSE + 24]
  movq xmm4, [rbx + OF_SSE + 32]
  movq xmm5, [rbx + OF_SSE + 40]
  movq xmm6, [rbx + OF_SSE + 48]
  movq xmm7, [rbx + OF_SSE + 56]
  mov  rsi, [rbx + OF_INT +  8]
  mov  rdx, [rbx + OF_INT + 16]
  mov  rcx, [rbx + OF_INT + 24]
  mov  r8,  [rbx + OF_INT + 32]
  mov  r9,  [rbx + OF_INT + 40]
  mov  rax, [rbx + OF_NSSE]
  mov  r10, [rbx + OF_FN]
  mov  rdi, [rbx + OF_INT +  0]
  call r10
  mov  [rbx + OF_RAX], rax
  mov  [rbx + OF_RDX], rdx
  movq [rbx + OF_XMM0], xmm0
  movq [rbx + OF_XMM1], xmm1
  lea  rsp, [rbp - 8]
  pop  rbx
  pop  rbp
  ret
end;

{$ENDIF}
{$ENDIF}


{ ================= The classification, which is the whole difficulty =================

  ⛔ THIS IS THE HALF THAT FAILS SILENTLY. Everything below decides WHICH register a value travels in;
  get it wrong and the callee reads a different register, does not fault, and answers a wrong number.
  It is why job/tests/tools/ffi/probe.c builds three structs that classify DIFFERENTLY on purpose. }

type
  TEightClass = (ecNone, ecInteger, ecSSE, ecMemory);
  TEightPair = array[0..1] of TEightClass;

function MergeClass(A, B: TEightClass): TEightClass;
// The SysV merge rule, in the order the specification states it: MEMORY wins over everything, then
// INTEGER over SSE, and NO_CLASS is the identity. "INTEGER over SSE" is the one that is easy to get
// backwards and it is exactly what makes { int; double; } take one register of each file.
begin
  if A = B then Exit(A);
  if A = ecNone then Exit(B);
  if B = ecNone then Exit(A);
  if (A = ecMemory) or (B = ecMemory) then Exit(ecMemory);
  Result := ecInteger;
end;

procedure ClassifyInto(T: TAbiType; BaseOfs: Integer; var Cl: TEightPair);
var
  i, Lo, Hi: Integer;
  K: TEightClass;
begin
  if T.Kind = akStruct then
  begin
    for i := 0 to T.FieldCount - 1 do
      ClassifyInto(T.Field(i), BaseOfs + T.FieldOffset(i), Cl);
    Exit;
  end;
  if T.IsFloatKind then K := ecSSE else K := ecInteger;
  Lo := BaseOfs div 8;
  Hi := (BaseOfs + T.Size - 1) div 8;
  // Past the second eightbyte means the aggregate is larger than 16 bytes, which is MEMORY; a field
  // that STRADDLES a boundary (only possible when packed) taints both halves.
  if (Lo > 1) or (Hi > 1) then
  begin
    Cl[0] := ecMemory; Cl[1] := ecMemory;
    Exit;
  end;
  Cl[Lo] := MergeClass(Cl[Lo], K);
  if Hi <> Lo then Cl[Hi] := MergeClass(Cl[Hi], K);
end;

function ClassifySysV(T: TAbiType; out Cl: TEightPair; out NEight: Integer): Boolean;
// False means MEMORY: the value travels through memory, not registers.
//
// ⚠️ THE SIZE TEST HERE IS REDUNDANT WITH ClassifyInto, ON PURPOSE, and that is worth stating because
// it makes a sabotage of it come out GREEN: widening the 16 to 24 changes nothing, since a field
// landing past the second eightbyte still forces MEMORY inside ClassifyInto. Two guards for one rule
// is cheap; a green sabotage that means "the code was redundant" is not the same as one that means
// "the probe is too weak", and only reading them apart tells you which you have.
begin
  Cl[0] := ecNone; Cl[1] := ecNone;
  NEight := (T.Size + 7) div 8;
  if (T.Size > 16) or (T.Size = 0) then Exit(False);
  ClassifyInto(T, 0, Cl);
  Result := not ((Cl[0] = ecMemory) or (Cl[1] = ecMemory));
end;

function LoadIntArg(T: TAbiType; V: Pointer): PtrUInt;
// A narrow integer reaches a 64-bit register SIGN- or ZERO-extended by its own signedness. The ABI
// leaves the upper bits unspecified and the callee reads only the low half, so this is belt and
// braces - but it is also what makes a value printed from the frame in a diagnostic read right.
begin
  case T.Kind of
    akS8:  Result := PtrUInt(Int64(PShortInt(V)^));
    akU8:  Result := PByte(V)^;
    akS16: Result := PtrUInt(Int64(PSmallInt(V)^));
    akU16: Result := PWord(V)^;
    akS32: Result := PtrUInt(Int64(PLongInt(V)^));
    akU32: Result := PLongWord(V)^;
  else
    Result := PQWord(V)^;                       // 64-bit integers and every pointer
  end;
end;

function LoadSseArg(T: TAbiType; V: Pointer): QWord;
// ⚠️ Reading only FOUR bytes for a float is about the SOURCE, not the destination: the callee reads the
// low 32 bits either way, so widening this read to eight is invisible to every probe - it is wrong
// because it reads four bytes PAST the caller's variable, which is a fault only at the end of a page.
// (Sabotaged 8 Sep 2026 and green, for exactly that reason.)
// ⛔ RAW BITS, NOT A CONVERTED VALUE. A "float" argument occupies the LOW 32 BITS of the XMM register
// as a 32-bit float - so widening it to a Double here would hand the callee the mantissa of a double
// where it reads a float. That is why SseArgs is an array of QWord and not of Double: the assembly
// moves eight bytes, and what those bytes MEAN is decided here.
begin
  if T.Kind = akFloat then Result := PLongWord(V)^
  else Result := PQWord(V)^;
end;


{ ============================ Closures ============================

  The saved-register block the fixed trampoline builds, and which AbiClosureEntry reads. Offsets are
  from the block's start; the trampoline writes them with literal displacements, so they are written
  twice here too and checked at startup for the same reason the call frame's are. }

const
{$IFDEF WINDOWS}
  CLO_NGP   = 4;
  CLO_NSSE  = 4;
{$ELSE}
  CLO_NGP   = 6;
  CLO_NSSE  = 8;
{$ENDIF}
  CLO_GP    = 0;                          // CLO_NGP words
  CLO_SSE   = CLO_NGP * 8;                // CLO_NSSE words
  CLO_RAX   = CLO_SSE + CLO_NSSE * 8;
  CLO_XMM0  = CLO_RAX + 8;
  CLO_BLOCK = CLO_XMM0 + 8;               // ...then padding, decided per ABI below

type
  PClosureCtx = ^TClosureCtx;
  TClosureCtx = record
    Owner: TAbiClosure;
  end;

procedure AbiClosureEntry(Ctx: PClosureCtx; Saved: PByte; StackArgs: PByte); cdecl; forward;

{$IFDEF ABI_SUPPORTED}
{$IFDEF WINDOWS}

procedure AbiClosureTrampoline; assembler; nostackframe;
// Win64. R10 carries the context (the generated stub put it there); the four argument slots are in
// RCX RDX R8 R9 and XMM0..3, and the callee owns 32 bytes of shadow space above the return address.
// 0x68 = 4*8 GP + 4*8 SSE + 8 RAX + 8 XMM0 = 0x50, plus 32 shadow for the inner call = 0x70; rounded
// so that RSP is 16-aligned at the inner call.
asm
  sub  rsp, $78
  mov  [rsp + $20 + 0], rcx
  mov  [rsp + $20 + 8], rdx
  mov  [rsp + $20 + 16], r8
  mov  [rsp + $20 + 24], r9
  movq [rsp + $20 + 32], xmm0
  movq [rsp + $20 + 40], xmm1
  movq [rsp + $20 + 48], xmm2
  movq [rsp + $20 + 56], xmm3
  mov  rcx, r10
  lea  rdx, [rsp + $20]
  lea  r8,  [rsp + $80]        // $78 + 8 = past the return address: the first stack argument
  call AbiClosureEntry
  mov  rax,  [rsp + $20 + 64]
  movq xmm0, [rsp + $20 + 72]
  add  rsp, $78
  ret
end;

{$ELSE}

procedure AbiClosureTrampoline; assembler; nostackframe;
// SysV AMD64. R11 carries the context; the six integer and eight SSE argument registers are saved
// verbatim, and the arguments that did not fit sit above the return address.
// $B8 = 184: 6*8 + 8*8 + 8 + 8 = 128 used, the rest padding chosen so that RSP - which is 8 (mod 16)
// on entry - is 0 (mod 16) at the inner call.
asm
  sub  rsp, $B8
  mov  [rsp + 0],  rdi
  mov  [rsp + 8],  rsi
  mov  [rsp + 16], rdx
  mov  [rsp + 24], rcx
  mov  [rsp + 32], r8
  mov  [rsp + 40], r9
  movq [rsp + 48],  xmm0
  movq [rsp + 56],  xmm1
  movq [rsp + 64],  xmm2
  movq [rsp + 72],  xmm3
  movq [rsp + 80],  xmm4
  movq [rsp + 88],  xmm5
  movq [rsp + 96],  xmm6
  movq [rsp + 104], xmm7
  mov  rdi, r11
  mov  rsi, rsp
  lea  rdx, [rsp + $C0]        // $B8 + 8 = past the return address
  call AbiClosureEntry
  mov  rax,  [rsp + 112]
  movq xmm0, [rsp + 120]
  add  rsp, $B8
  ret
end;

{$ENDIF}
{$ENDIF}

procedure AbiClosureEntry(Ctx: PClosureCtx; Saved: PByte; StackArgs: PByte); cdecl;
// Undo, for ONE call, exactly what AbiCall does when it makes one: walk the declared arguments in
// order, following the same register-allocation rules, and hand the user a pointer to each.
//
// ⛔ THE RULES MUST BE THE SAME ONES, not a second copy of them - a closure that disagrees with the
// caller about where argument three lives is the same silent failure in the other direction. They are
// short enough to be read side by side, and the probe calls BOTH halves in one program.
var
  C: TAbiClosure;
  Ptrs: array[0..MAX_ARGS - 1] of Pointer;
  Slots: array[0..MAX_ARGS - 1] of QWord;      // where a value is rebuilt when it is not contiguous
  RetBuf: array[0..15] of Byte;
  i, k, NGP, NSS, NStack, NEight, NeedGP, NeedSS, w: Integer;
  T: TAbiType;
  Cl: TEightPair;
  RetCl: TEightPair;
  RetN: Integer;
  RetInMem: Boolean;
  MemRet: Pointer;
begin
  C := Ctx^.Owner;
  NGP := 0; NSS := 0; NStack := 0;
  MemRet := nil;
  FillChar(RetBuf, SizeOf(RetBuf), 0);
  FillChar(Slots, SizeOf(Slots), 0);

{$IFDEF WINDOWS}
  RetInMem := (C.FRet.Kind = akStruct) and not (C.FRet.Size in [1, 2, 4, 8]);
  if RetInMem then begin MemRet := Pointer(PQWord(Saved + CLO_GP)^); Inc(NGP); end;
  for i := 0 to High(C.FArgs) do
  begin
    T := C.FArgs[i];
    if NGP < CLO_NGP then
    begin
      if T.IsFloatKind then Ptrs[i] := Saved + CLO_SSE + NGP * 8
      else Ptrs[i] := Saved + CLO_GP + NGP * 8;
      // A struct of 1/2/4/8 bytes arrived BY VALUE in the register; a larger one arrived as a pointer.
      if (T.Kind = akStruct) and not (T.Size in [1, 2, 4, 8]) then
        Ptrs[i] := Pointer(PQWord(Saved + CLO_GP + NGP * 8)^);
      Inc(NGP);
    end
    else
    begin
      Ptrs[i] := StackArgs + NStack * 8;
      if (T.Kind = akStruct) and not (T.Size in [1, 2, 4, 8]) then
        Ptrs[i] := Pointer(PQWord(StackArgs + NStack * 8)^);
      Inc(NStack); Inc(NGP);
    end;
  end;
{$ELSE}
  RetInMem := False;
  if C.FRet.Kind <> akVoid then RetInMem := not ClassifySysV(C.FRet, RetCl, RetN);
  if RetInMem then begin MemRet := Pointer(PQWord(Saved + CLO_GP)^); Inc(NGP); end;
  for i := 0 to High(C.FArgs) do
  begin
    T := C.FArgs[i];
    if T.Kind = akStruct then
    begin
      if ClassifySysV(T, Cl, NEight) then
      begin
        NeedGP := 0; NeedSS := 0;
        for k := 0 to NEight - 1 do
          if Cl[k] = ecSSE then Inc(NeedSS) else Inc(NeedGP);
        if (NGP + NeedGP <= 6) and (NSS + NeedSS <= 8) then
        begin
          // The eightbytes are in different register files: rebuild the struct contiguously.
          for k := 0 to NEight - 1 do
            if Cl[k] = ecSSE then
            begin
              Slots[i * 2 + k] := PQWord(Saved + CLO_SSE + NSS * 8)^; Inc(NSS);
            end
            else
            begin
              Slots[i * 2 + k] := PQWord(Saved + CLO_GP + NGP * 8)^; Inc(NGP);
            end;
          Ptrs[i] := @Slots[i * 2];
          Continue;
        end;
      end;
      w := (T.Size + 7) div 8;
      Ptrs[i] := StackArgs + NStack * 8;
      Inc(NStack, w);
      Continue;
    end;
    if T.IsFloatKind then
    begin
      if NSS < 8 then begin Ptrs[i] := Saved + CLO_SSE + NSS * 8; Inc(NSS); end
      else begin Ptrs[i] := StackArgs + NStack * 8; Inc(NStack); end;
    end
    else
    begin
      if NGP < 6 then begin Ptrs[i] := Saved + CLO_GP + NGP * 8; Inc(NGP); end
      else begin Ptrs[i] := StackArgs + NStack * 8; Inc(NStack); end;
    end;
  end;
{$ENDIF}

  if RetInMem then C.FHandler(MemRet, @Ptrs[0], C.FUser)
  else C.FHandler(@RetBuf[0], @Ptrs[0], C.FUser);

  // Put the result where the ABI says the caller will look for it. The trampoline reloads RAX and XMM0
  // from these two slots on its way out.
  PQWord(Saved + CLO_RAX)^ := 0;
  PQWord(Saved + CLO_XMM0)^ := 0;
  if RetInMem then
  begin
    PQWord(Saved + CLO_RAX)^ := PtrUInt(MemRet);   // the callee returns the hidden pointer in RAX
    Exit;
  end;
  if C.FRet.Kind = akVoid then Exit;
{$IFDEF WINDOWS}
  if C.FRet.IsFloatKind then Move(RetBuf[0], (Saved + CLO_XMM0)^, 8)
  else Move(RetBuf[0], (Saved + CLO_RAX)^, 8);
{$ELSE}
  if C.FRet.Kind = akStruct then
  begin
    NGP := 0; NSS := 0;
    for k := 0 to RetN - 1 do
      if RetCl[k] = ecSSE then
      begin
        if NSS = 0 then Move(RetBuf[k * 8], (Saved + CLO_XMM0)^, 8);
        Inc(NSS);
      end
      else
      begin
        if NGP = 0 then Move(RetBuf[k * 8], (Saved + CLO_RAX)^, 8);
        Inc(NGP);
      end;
  end
  else if C.FRet.IsFloatKind then Move(RetBuf[0], (Saved + CLO_XMM0)^, 8)
  else Move(RetBuf[0], (Saved + CLO_RAX)^, 8);
{$ENDIF}
end;

{ ---- executable memory, which is the one thing a closure cannot do without ---- }

function AllocExecutable(Size: PtrUInt): Pointer; forward;
procedure FreeExecutable(P: Pointer; Size: PtrUInt); forward;
{$IFNDEF WINDOWS}procedure MakeExecutable(P: Pointer; Size: PtrUInt); forward;{$ENDIF}

constructor TAbiClosure.Create(ARet: TAbiType; const AArgs: array of TAbiType;
                               AHandler: TAbiClosureFun; AUser: Pointer);
var
  i: Integer;
  Stub: PByte;
  Ctx: PClosureCtx;
begin
  inherited Create;
  FReady := False;
  FRet := ARet;
  SetLength(FArgs, Length(AArgs));
  for i := 0 to High(AArgs) do FArgs[i] := AArgs[i];
  FHandler := AHandler;
  FUser := AUser;
  {$IFNDEF ABI_SUPPORTED}
  Exit;
  {$ELSE}
  // One page holds the context and the stub, in that order: the stub needs the context's ADDRESS
  // baked into it, so they are allocated together and the address is known before the bytes are written.
  FBlockSize := 4096;
  FBlock := AllocExecutable(FBlockSize);
  if FBlock = nil then Exit;
  Ctx := PClosureCtx(FBlock);
  Ctx^.Owner := Self;
  Stub := PByte(FBlock) + 64;
  FCode := Stub;
  // movabs r11/r10, <ctx> ; movabs rax, <trampoline> ; jmp rax
  {$IFDEF WINDOWS}
  Stub[0] := $49; Stub[1] := $BA;                 // mov r10, imm64
  {$ELSE}
  Stub[0] := $49; Stub[1] := $BB;                 // mov r11, imm64
  {$ENDIF}
  PQWord(Stub + 2)^ := QWord(PtrUInt(Ctx));
  Stub[10] := $48; Stub[11] := $B8;               // mov rax, imm64
  PQWord(Stub + 12)^ := QWord(PtrUInt(@AbiClosureTrampoline));
  Stub[20] := $FF; Stub[21] := $E0;               // jmp rax
  {$IFNDEF WINDOWS}MakeExecutable(FBlock, FBlockSize);{$ENDIF}
  FReady := True;
  {$ENDIF}
end;

destructor TAbiClosure.Destroy;
begin
  if FBlock <> nil then FreeExecutable(FBlock, FBlockSize);
  inherited Destroy;
end;

{$IFDEF ABI_SUPPORTED}

procedure AbiCall(Fn: Pointer; ARet: TAbiType; const AArgs: array of TAbiType;
                  const AValues: array of Pointer; ARetBuf: Pointer);
var
  F: TAbiFrame;
  Words: array[0..2 * MAX_ARGS + 8] of QWord;
  RetBytes: array[0..15] of Byte;
  NStack, NGP, NSS, i, k, w, NEight, Need, NeedGP, NeedSS: Integer;
  RetCl, Cl: TEightPair;
  RetN: Integer;
  RetInMem: Boolean;
  T: TAbiType;
  V: Pointer;
  {$IFDEF WINDOWS}
  Copies: array[0..MAX_ARGS - 1] of Pointer;
  NCopies: Integer;
  {$ENDIF}
begin
  if Length(AArgs) <> Length(AValues) then
    raise EAbiError.CreateFmt('a foreign call was prepared for %d arguments and given %d',
                              [Length(AArgs), Length(AValues)]);
  if Length(AArgs) > MAX_ARGS then
    raise EAbiError.CreateFmt('%d arguments is more than this call path carries', [Length(AArgs)]);
  FillChar(F, SizeOf(F), 0);
  FillChar(Words, SizeOf(Words), 0);
  FillChar(RetBytes, SizeOf(RetBytes), 0);
  F.Fn := Fn;
  NStack := 0; NGP := 0; NSS := 0;

{$IFDEF WINDOWS}
  // ---------------------------------------------------------------- Win64
  // Positional: argument n uses slot n, whatever its kind. Anything that is not 1, 2, 4 or 8 bytes is
  // passed as a POINTER TO A COPY the caller makes - which is why the copies are held here until the
  // call returns.
  NCopies := 0;
  RetInMem := (ARet.Kind = akStruct) and not (ARet.Size in [1, 2, 4, 8]);
  if RetInMem then
  begin
    F.IntArgs[0] := PtrUInt(ARetBuf);
    NGP := 1;
  end;
  for i := 0 to High(AArgs) do
  begin
    T := AArgs[i]; V := AValues[i];
    if (T.Kind = akStruct) and not (T.Size in [1, 2, 4, 8]) then
    begin
      GetMem(Copies[NCopies], T.Size);
      Move(V^, Copies[NCopies]^, T.Size);
      V := @Copies[NCopies];              // the ARGUMENT is now the address of the copy
      Inc(NCopies);
      k := 8;                             // ...and it travels as a pointer
    end
    else
      k := T.Size;
    if NGP < 4 then
    begin
      if T.IsFloatKind then
      begin
        F.SseArgs[NGP] := LoadSseArg(T, V);
        // ⛔ AND THE SAME BITS IN THE INTEGER REGISTER. A variadic callee (printf) reads the integer
        // half; a prototyped one reads the SSE half. Nothing in a declaration tells us which this is,
        // and filling both is exactly what a compiler does when it has no prototype.
        F.IntArgs[NGP] := PtrUInt(F.SseArgs[NGP]);
      end
      else if T.Kind = akStruct then
      begin
        F.IntArgs[NGP] := 0;
        Move(V^, F.IntArgs[NGP], k);      // 1, 2, 4 or 8 bytes, right-justified in the register
      end
      else
        F.IntArgs[NGP] := LoadIntArg(T, V);
      Inc(NGP);
    end
    else
    begin
      Words[NStack] := 0;
      if T.Kind = akStruct then Move(V^, Words[NStack], k)
      else if T.IsFloatKind then Words[NStack] := LoadSseArg(T, V)
      else Words[NStack] := LoadIntArg(T, V);
      Inc(NStack);
      Inc(NGP);                            // a stack argument still consumes its positional slot
    end;
  end;
  F.NSse := 0;
{$ELSE}
  // ------------------------------------------------------------ SysV AMD64
  RetInMem := False;
  if ARet.Kind <> akVoid then
    RetInMem := not ClassifySysV(ARet, RetCl, RetN);
  if RetInMem then
  begin
    // The hidden pointer is argument ZERO and it consumes an integer register.
    F.IntArgs[0] := PtrUInt(ARetBuf);
    NGP := 1;
  end;
  for i := 0 to High(AArgs) do
  begin
    T := AArgs[i]; V := AValues[i];
    if T.Kind = akStruct then
    begin
      if ClassifySysV(T, Cl, NEight) then
      begin
        // ⛔ ALL OF IT IN REGISTERS, OR NONE. Half in registers and half on the stack is not a shape
        // this ABI has: when the eightbytes do not all fit, the whole aggregate goes to memory.
        NeedGP := 0; NeedSS := 0;
        for k := 0 to NEight - 1 do
          if Cl[k] = ecSSE then Inc(NeedSS) else Inc(NeedGP);
        if (NGP + NeedGP <= 6) and (NSS + NeedSS <= 8) then
        begin
          for k := 0 to NEight - 1 do
          begin
            if Cl[k] = ecSSE then
            begin
              F.SseArgs[NSS] := PQWord(PByte(V) + k * 8)^;
              Inc(NSS);
            end
            else
            begin
              F.IntArgs[NGP] := 0;
              w := T.Size - k * 8; if w > 8 then w := 8;
              Move((PByte(V) + k * 8)^, F.IntArgs[NGP], w);
              Inc(NGP);
            end;
          end;
          Continue;
        end;
      end;
      // MEMORY: the bytes go on the stack, rounded up to whole words.
      w := (T.Size + 7) div 8;
      Move(V^, Words[NStack], T.Size);
      Inc(NStack, w);
      Continue;
    end;
    if T.IsFloatKind then
    begin
      if NSS < 8 then begin F.SseArgs[NSS] := LoadSseArg(T, V); Inc(NSS); end
      else begin Words[NStack] := LoadSseArg(T, V); Inc(NStack); end;
    end
    else
    begin
      if NGP < 6 then begin F.IntArgs[NGP] := LoadIntArg(T, V); Inc(NGP); end
      else begin Words[NStack] := LoadIntArg(T, V); Inc(NStack); end;
    end;
  end;
  F.NSse := NSS;      // -> AL, which a variadic callee reads to know how many XMM registers are live
{$ENDIF}

  F.Stack := @Words[0];
  F.NStack := NStack;
  AbiCallRaw(@F);

{$IFDEF WINDOWS}
  for i := 0 to NCopies - 1 do FreeMem(Copies[i]);
{$ENDIF}

  // ------------------------------------------------------------- the result
  if (ARet.Kind = akVoid) or (ARetBuf = nil) or RetInMem then Exit;
{$IFDEF WINDOWS}
  // Win64: a scalar float or double comes back in XMM0; everything else - including a struct of 1, 2,
  // 4 or 8 bytes - comes back in RAX.
  if ARet.IsFloatKind then PQWord(@RetBytes[0])^ := F.RetXmm0
  else PQWord(@RetBytes[0])^ := F.RetRax;
{$ELSE}
  if ARet.Kind = akStruct then
  begin
    // Recompose from the eightbytes, taking INTEGER ones from RAX then RDX and SSE ones from XMM0 then
    // XMM1 - each file in its own order, which is why { int; double; } comes back as RAX + XMM0.
    NGP := 0; NSS := 0;
    for k := 0 to RetN - 1 do
      if RetCl[k] = ecSSE then
      begin
        if NSS = 0 then PQWord(@RetBytes[k * 8])^ := F.RetXmm0
                   else PQWord(@RetBytes[k * 8])^ := F.RetXmm1;
        Inc(NSS);
      end
      else
      begin
        if NGP = 0 then PQWord(@RetBytes[k * 8])^ := F.RetRax
                   else PQWord(@RetBytes[k * 8])^ := F.RetRdx;
        Inc(NGP);
      end;
  end
  else if ARet.IsFloatKind then PQWord(@RetBytes[0])^ := F.RetXmm0
  else PQWord(@RetBytes[0])^ := F.RetRax;
{$ENDIF}
  // Exactly Size bytes: the caller's buffer may be no wider than the declared type.
  Move(RetBytes[0], ARetBuf^, ARet.Size);
end;

{$ELSE}

procedure AbiCall(Fn: Pointer; ARet: TAbiType; const AArgs: array of TAbiType;
                  const AValues: array of Pointer; ARetBuf: Pointer);
begin
  raise EAbiError.Create(AbiUnavailableReason);
end;

{$ENDIF}

function AbiAvailable: Boolean;
begin
  {$IFDEF ABI_SUPPORTED}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function AbiUnavailableReason: string;
begin
  {$IFDEF ABI_SUPPORTED}
  Result := '';
  {$ELSE}
  Result := 'foreign calls need a calling convention written for this architecture, and only ' +
            'x86-64 (SysV and Win64) has one here';
  {$ENDIF}
end;

{ ---- executable memory ----
  ⛔ WRITABLE AND EXECUTABLE ARE ASKED FOR SEPARATELY where the system separates them. On Linux the page
  is mapped read/write, filled, and only then turned read/execute: a W^X kernel refuses a mapping that
  is both, and asking for both is also how a process invites every exploit that needs a place to write
  code. Windows has no such split by default, so PAGE_EXECUTE_READWRITE is one call. }

{$IFDEF ABI_SUPPORTED}
{$IFDEF WINDOWS}
function AllocExecutable(Size: PtrUInt): Pointer;
begin
  Result := VirtualAlloc(nil, Size, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
end;

procedure FreeExecutable(P: Pointer; Size: PtrUInt);
begin
  VirtualFree(P, 0, MEM_RELEASE);
end;
{$ELSE}
function AllocExecutable(Size: PtrUInt): Pointer;
begin
  Result := fpmmap(nil, Size, PROT_READ or PROT_WRITE, MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
  if Result = Pointer(-1) then Result := nil;
end;

procedure MakeExecutable(P: Pointer; Size: PtrUInt);
begin
  fpmprotect(P, Size, PROT_READ or PROT_EXEC);
end;

procedure FreeExecutable(P: Pointer; Size: PtrUInt);
begin
  fpmunmap(P, Size);
end;
{$ENDIF}
{$ELSE}
function AllocExecutable(Size: PtrUInt): Pointer;
begin
  Result := nil;
end;

procedure FreeExecutable(P: Pointer; Size: PtrUInt);
begin
end;
{$ENDIF}

procedure CheckFrameLayout;
// ⛔ THE ONE CHECK THAT MAKES THE DUPLICATED OFFSETS SAFE. The record and the constants the assembly
// uses are written twice because no assembler can read a Pascal record's layout - and a wrong offset
// would put an argument in the wrong register, which is the silent failure this unit exists to
// prevent. Compared here, at startup, where it costs nothing and cannot be forgotten.
var
  F: TAbiFrame;
  B: PByte;
  procedure Want(const AWhat: string; AGot, AWant: PtrInt);
  begin
    if AGot <> AWant then
      raise EAbiError.CreateFmt('SedaiAbi: the frame offset for %s is %d but the assembly reads %d - ' +
        'the record and the OF_* constants have gone out of step', [AWhat, AGot, AWant]);
  end;
begin
  B := @F;
  Want('Fn',      PByte(@F.Fn)         - B, OF_FN);
  Want('IntArgs', PByte(@F.IntArgs[0]) - B, OF_INT);
  Want('SseArgs', PByte(@F.SseArgs[0]) - B, OF_SSE);
  Want('NSse',    PByte(@F.NSse)       - B, OF_NSSE);
  Want('Stack',   PByte(@F.Stack)      - B, OF_STACK);
  Want('NStack',  PByte(@F.NStack)     - B, OF_NSTACK);
  Want('RetRax',  PByte(@F.RetRax)     - B, OF_RAX);
  Want('RetRdx',  PByte(@F.RetRdx)     - B, OF_RDX);
  Want('RetXmm0', PByte(@F.RetXmm0)    - B, OF_XMM0);
  Want('RetXmm1', PByte(@F.RetXmm1)    - B, OF_XMM1);
  Want('the whole frame', SizeOf(TAbiFrame), ABI_FRAME_SIZE);
end;

initialization
  CheckFrameLayout;

end.
