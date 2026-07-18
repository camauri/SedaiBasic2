unit SedaiJit;

{ ============================================================================
  SedaiJit - native code generation for hot loops (VM performance, milestone J2/J3).

  Compiles a hot bytecode loop [HeaderPC..EndPC] to native x86-64 that operates
  DIRECTLY on the VM register banks (IntRegs / FloatRegs), removing per-instruction
  dispatch. Integer ops use the general registers; float ops use SSE2 (addsd/mulsd/
  subsd on xmm), so a float-heavy loop runs at near-native speed.

  Calling convention of a compiled loop (Win64 / SysV both pass the two pointers in
  the first two integer arg registers; the emitter picks the right ones):
      function(IntRegs, FloatRegs: PInt64): PtrInt;   // returns the exit bytecode PC

  CONSERVATIVE BY DESIGN (bit-identical or bail): CompileLoop returns nil unless
  EVERY instruction in the range is in the supported set and every branch either
  stays in the range or is a clean exit. Anything else -> the loop is left to the
  interpreter. The supported set is grown as milestones land (arrays, sqrt, div...).

  Register file (Win64 non-volatile saved in the prologue): rbx = IntRegs base,
  rsi = FloatRegs base. rax/rcx scratch (integer), xmm0/xmm1 scratch (float).
  ============================================================================ }

{$mode objfpc}{$H+}

interface

uses
  SysUtils, SedaiBytecodeTypes
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF UNIX}, BaseUnix{$ENDIF};

type
  // A compiled loop: call with the two register-bank base pointers + the array descriptor table
  // (24 bytes/array: IntData ptr, FloatData ptr, Count as Int64); returns the exit PC.
  TNativeLoopFn = function(IntRegs, FloatRegs: PInt64; ArrDesc: Pointer): PtrInt;

  TX86Emitter = class
  private
    FBuf: array of Byte;
    FLen: Integer;
    procedure Ensure(N: Integer);
  public
    procedure Emit8(B: Byte);
    procedure Emit32(V: LongWord);
    procedure Emit64(V: QWord);
    procedure EmitBytes(const B: array of Byte);
    // [base+disp32] memory operand: emit the opcode bytes, then ModRM (mod=10) + disp32.
    procedure MemOp(const Op: array of Byte; RegField, BaseReg: Byte; Disp: LongWord);
    procedure Patch32(AtOffset: Integer; V: LongWord);
    procedure PatchByte(AtOffset: Integer; V: Byte);
    function Bytes: PByte;
    property Len: Integer read FLen;
  end;

  TExecMem = class
  private
    FPtr: Pointer;
    FSize: PtrUInt;
  public
    constructor Create(const Code: TX86Emitter);
    destructor Destroy; override;
    property Ptr: Pointer read FPtr;
  end;

// Compile the loop body [HeaderPC..EndPC] (inclusive) to native code. Ins points at instruction 0.
// ProgLen is the whole program's instruction count (NativeOff/InRange are indexed by absolute PC so an
// inlined callee's PCs resolve too). TrueVal is the VM's TRUE value baked into integer comparisons.
// AllowUnsafe = MODERN dialect and no forced bounds-check: only then may array access / sqrt / div be
// compiled (their MODERN edge semantics -- OOB->default, div0->IEEE, sqrt(neg)->NaN -- match the native
// SSE forms; CLASSIC would raise, so the loop bails). XferIntBase/XferFloatBase are the (stable) base
// addresses of Ctx.XferInt[0]/Ctx.XferFloat[0], baked as immediates so bcXferStore/Load and inlined SUB
// calls (bcCallSub, milestone J6) need no extra parameter. Returns a TExecMem whose Ptr is a
// TNativeLoopFn, or nil if the loop is not compilable.
function CompileLoop(Ins: Pointer; HeaderPC, EndPC, ProgLen: Integer; TrueVal: Int64;
                     AllowUnsafe: Boolean; XferIntBase, XferFloatBase: PtrUInt): TExecMem;

// J2 self-test: emit  a+b  and call it, proving the emit->exec->call pipeline.
function JitSelfTest(out Msg: string): Boolean;

// Diagnostic (set to the last opcode/PC processed by CompileLoop): when a loop bails, these hold the
// culprit. Read by BuildJitLoops under the JIT_DIAG env var. Not thread-safe; diagnostics only.
var
  JitDiagCurOp: Word = 0;
  JitDiagCurPC: Integer = -1;

implementation

// x86-64 register numbers
const
  RAX = 0; RCX = 1; RDX = 2; RBX = 3; RSI = 6; R8 = 8;
  R9 = 9; R10 = 10; R11 = 11; R12 = 12; R13 = 13; R14 = 14; R15 = 15;
  XMM0 = 0; XMM1 = 1;

{ ---------------- executable memory ---------------- }

function AllocExec(Size: PtrUInt): Pointer;
begin
  {$IFDEF WINDOWS}
  Result := VirtualAlloc(nil, Size, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
  {$ELSE}
  {$IFDEF UNIX}
  Result := Fpmmap(nil, Size, PROT_READ or PROT_WRITE or PROT_EXEC,
                   MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
  if Result = Pointer(-1) then Result := nil;
  {$ELSE}
  Result := nil;
  {$ENDIF}
  {$ENDIF}
end;

procedure FreeExec(P: Pointer; Size: PtrUInt);
begin
  if P = nil then Exit;
  {$IFDEF WINDOWS}
  VirtualFree(P, 0, MEM_RELEASE);
  {$ELSE}
  {$IFDEF UNIX}
  Fpmunmap(P, Size);
  {$ENDIF}
  {$ENDIF}
end;

{ ---------------- TX86Emitter ---------------- }

procedure TX86Emitter.Ensure(N: Integer);
begin
  if FLen + N > Length(FBuf) then SetLength(FBuf, (FLen + N) * 2 + 64);
end;

procedure TX86Emitter.Emit8(B: Byte);
begin Ensure(1); FBuf[FLen] := B; Inc(FLen); end;

procedure TX86Emitter.Emit32(V: LongWord);
begin Emit8(V and $FF); Emit8((V shr 8) and $FF); Emit8((V shr 16) and $FF); Emit8((V shr 24) and $FF); end;

procedure TX86Emitter.Emit64(V: QWord);
begin Emit32(LongWord(V and $FFFFFFFF)); Emit32(LongWord((V shr 32) and $FFFFFFFF)); end;

procedure TX86Emitter.EmitBytes(const B: array of Byte);
var i: Integer;
begin for i := 0 to High(B) do Emit8(B[i]); end;

procedure TX86Emitter.MemOp(const Op: array of Byte; RegField, BaseReg: Byte; Disp: LongWord);
begin
  EmitBytes(Op);
  Emit8($80 or ((RegField and 7) shl 3) or (BaseReg and 7));   // ModRM mod=10 (disp32)
  Emit32(Disp);
end;

procedure TX86Emitter.Patch32(AtOffset: Integer; V: LongWord);
begin
  FBuf[AtOffset]     := V and $FF;
  FBuf[AtOffset + 1] := (V shr 8) and $FF;
  FBuf[AtOffset + 2] := (V shr 16) and $FF;
  FBuf[AtOffset + 3] := (V shr 24) and $FF;
end;

procedure TX86Emitter.PatchByte(AtOffset: Integer; V: Byte);
begin FBuf[AtOffset] := V; end;

function TX86Emitter.Bytes: PByte;
begin if FLen > 0 then Result := @FBuf[0] else Result := nil; end;

{ ---------------- TExecMem ---------------- }

constructor TExecMem.Create(const Code: TX86Emitter);
begin
  FSize := Code.Len;
  FPtr := AllocExec(FSize);
  if (FPtr <> nil) and (Code.Len > 0) then Move(Code.Bytes^, FPtr^, Code.Len);
end;

destructor TExecMem.Destroy;
begin FreeExec(FPtr, FSize); inherited Destroy; end;

{ ---------------- loop compiler ---------------- }

type
  PBcInstr = ^TBytecodeInstruction;
  TFixup = record
    PatchOff: Integer;    // byte offset of the rel32 field to patch
    TargetPC: Integer;    // bytecode PC to jump to (or -1 = epilogue)
  end;

function CompileLoop(Ins: Pointer; HeaderPC, EndPC, ProgLen: Integer; TrueVal: Int64;
                     AllowUnsafe: Boolean; XferIntBase, XferFloatBase: PtrUInt): TExecMem;
var
  E: TX86Emitter;
  NativeOff: array of Integer;      // absolute bytecode PC -> native offset (sized ProgLen)
  InRange: array of Boolean;        // absolute PC belongs to the compiled code (caller range + inlined callees)
  Fixups: array of TFixup;
  NFix: Integer;
  Prog: PBcInstr;
  pc: Integer;
  I: PBcInstr;
  Dd, S1, S2: LongWord;              // register byte offsets (index*8)
  EpilogueOff: Integer;
  d, target: Integer;
  // --- inlined SUB calls (J6): each inlinable bcCallSub in the caller range becomes an inline copy of the
  // callee body [EntryPC..ReturnPC], emitted all-memory (InCallee) around a native FramePush/Pop. ---
  InCallee: Boolean;                 // True while emitting a callee body: everything memory-homed, no cache
  CallPC, CallEntry, CallRet, CallSaveN: array of Integer;   // parallel arrays, one entry per call site
  NCall: Integer;
  ScratchBytes: Integer;             // stack bytes reserved for the deepest call site's bank save/restore
  CurSaveN: Integer;                 // save size of the call site being emitted (read by its ReturnSub)
  // Float register allocation (J4): map a VM float reg -> a native xmm (2..7, no REX) or -1 (memory).
  // Only the volatile-plus-two set xmm2..xmm7 is used; xmm6/xmm7 are callee-saved so they are spilled to
  // the stack in the prologue when allocated. Integer VM regs are allocated to r9..r15 (see ILoc below).
  FLoc: array of Integer;
  FMaxReg, NextXmm, fi: Integer;
  SaveX6, SaveX7: Boolean;
  // Integer register allocation (J5): map a VM int reg -> a native GPR (r9..r15) or -1 (memory-homed).
  // Pool order: r9/r10/r11 (volatile) first, then r12..r15 (callee-saved, push/pop'd when used).
  ILoc: array of Integer;
  IMaxReg, NextGpr, ii, gpr: Integer;
  IntPool: array[0..6] of Integer;
  SaveGpr: array[0..15] of Boolean;
  GprUsed: array[0..15] of Boolean;  // which native GPRs are claimed (int alloc + array-base cache)
  // Array base/count caching (J5c LICM): a compiled loop's array descriptor is fixed for the whole
  // native invocation, so the base pointer and element count are loop-invariant. Cache them in the GPRs
  // left free after int allocation, removing two descriptor loads from every array access.
  CArrId: array of Integer;      // distinct array ids used in the loop
  CArrOff: array of Integer;     // descriptor base offset: 0 (int arrays) or 8 (float arrays)
  CArrUses: array of Integer;    // access count (for priority)
  CArrBase: array of Integer;    // assigned GPR holding the base pointer, or -1
  CArrCount: array of Integer;   // assigned GPR holding the element count, or -1
  NCArr, ci, cj, ct: Integer;

  procedure AddFixup(AOff, ATarget: Integer);
  begin
    if NFix >= Length(Fixups) then SetLength(Fixups, NFix * 2 + 8);
    Fixups[NFix].PatchOff := AOff;
    Fixups[NFix].TargetPC := ATarget;
    Inc(NFix);
  end;

  // Emit a rel32 jump/branch with a placeholder, register a fixup to TargetPC (or -1 for epilogue).
  procedure JmpRel(TargetPC: Integer);
  begin
    E.Emit8($E9);                    // jmp rel32
    AddFixup(E.Len, TargetPC);
    E.Emit32(0);
  end;
  procedure JccRel(CC: Byte; TargetPC: Integer);
  begin
    E.Emit8($0F); E.Emit8(CC);       // 0F 8x  (jcc rel32)
    AddFixup(E.Len, TargetPC);
    E.Emit32(0);
  end;

  // --- float register-allocation aware operand access (J4) ---
  // movsd Wx, <VM float reg vmreg>  (reg-reg if allocated to an xmm, else load from [rsi+off]).
  // In callee-inline mode every VM reg is memory-homed (the caller's FLoc must not be consulted).
  procedure FLoad(Wx, vmreg: Integer);
  begin
    if InCallee then
      E.MemOp([$F2, $0F, $10], Wx, RSI, LongWord(vmreg) * 8)
    else if FLoc[vmreg] >= 0 then
    begin
      if FLoc[vmreg] <> Wx then
        E.EmitBytes([$F2, $0F, $10, $C0 or (Wx shl 3) or FLoc[vmreg]])    // movsd Wx, xmm_src
    end
    else
      E.MemOp([$F2, $0F, $10], Wx, RSI, LongWord(vmreg) * 8);             // movsd Wx, [rsi+off]
  end;
  // <op>sd Wx, <VM float reg vmreg>
  procedure FOp(const SseOp: array of Byte; Wx, vmreg: Integer);
  begin
    if InCallee then
      E.MemOp(SseOp, Wx, RSI, LongWord(vmreg) * 8)
    else if FLoc[vmreg] >= 0 then
      E.EmitBytes([SseOp[0], SseOp[1], SseOp[2], $C0 or (Wx shl 3) or FLoc[vmreg]])
    else
      E.MemOp(SseOp, Wx, RSI, LongWord(vmreg) * 8);
  end;
  // store working xmm Wx -> VM float reg dest
  procedure FStore(vmreg, Wx: Integer);
  begin
    if InCallee then
      E.MemOp([$F2, $0F, $11], Wx, RSI, LongWord(vmreg) * 8)
    else if FLoc[vmreg] >= 0 then
    begin
      if FLoc[vmreg] <> Wx then
        E.EmitBytes([$F2, $0F, $10, $C0 or (FLoc[vmreg] shl 3) or Wx])    // movsd xmm_dst, Wx
    end
    else
      E.MemOp([$F2, $0F, $11], Wx, RSI, LongWord(vmreg) * 8);             // movsd [rsi+off], Wx
  end;

  // --- integer GPR register-allocation helpers (J5) ---
  // Reg-reg instruction: REX.W (+R if regField>=8)(+B if rmReg>=8), Op..., ModRM(mod=11, regField, rmReg).
  procedure EmitRR(const Op: array of Byte; regField, rmReg: Integer);
  var rex: Byte; k: Integer;
  begin
    rex := $48;
    if regField >= 8 then rex := rex or $04;    // REX.R
    if rmReg    >= 8 then rex := rex or $01;     // REX.B
    E.Emit8(rex);
    for k := 0 to High(Op) do E.Emit8(Op[k]);
    E.Emit8($C0 or ((regField and 7) shl 3) or (rmReg and 7));
  end;
  // mov <native dst>, <native src>   (89 = mov r/m64,r64 : reg field = src, rm field = dst)
  procedure MovRR(dst, src: Integer);
  begin EmitRR([$89], src, dst); end;
  // mov <native reg>, imm64
  procedure MovImm64(natreg: Integer; imm: Int64);
  var rex: Byte;
  begin
    rex := $48; if natreg >= 8 then rex := rex or $01;   // REX.B
    E.Emit8(rex); E.Emit8($B8 or (natreg and 7)); E.Emit64(QWord(imm));
  end;
  // mov <native reg>, [rbx+disp]   (entry load; REX.W + REX.R if reg>=8; rbx base, mod=10 disp32)
  procedure LoadRegMem(natreg: Integer; disp: LongWord);
  var rex: Byte;
  begin
    rex := $48; if natreg >= 8 then rex := rex or $04;   // REX.R
    E.Emit8(rex); E.Emit8($8B);
    E.Emit8($80 or ((natreg and 7) shl 3) or RBX);        // mod=10 reg=natreg rm=rbx(3)
    E.Emit32(disp);
  end;
  // mov [rbx+disp], <native reg>   (exit store)
  procedure StoreRegMem(natreg: Integer; disp: LongWord);
  var rex: Byte;
  begin
    rex := $48; if natreg >= 8 then rex := rex or $04;   // REX.R
    E.Emit8(rex); E.Emit8($89);
    E.Emit8($80 or ((natreg and 7) shl 3) or RBX);
    E.Emit32(disp);
  end;
  // Load VM int reg `vmreg` into scratch native reg `scr` (rax/rcx, always < 8).
  procedure ILoad(scr, vmreg: Integer);
  begin
    if (not InCallee) and (ILoc[vmreg] >= 0) then MovRR(scr, ILoc[vmreg])
    else E.MemOp([$48, $8B], scr, RBX, LongWord(vmreg) * 8);
  end;
  // Store scratch native reg `scr` (< 8) into VM int reg `vmreg`.
  procedure IStore(vmreg, scr: Integer);
  begin
    if (not InCallee) and (ILoc[vmreg] >= 0) then MovRR(ILoc[vmreg], scr)
    else E.MemOp([$48, $89], scr, RBX, LongWord(vmreg) * 8);
  end;
  // ALU op  scr <op> vmreg  (MemForm = full memory-form bytes incl. the $48 REX; scr is rax/rcx < 8).
  procedure IOp(const MemForm: array of Byte; scr, vmreg: Integer);
  var rest: array of Byte; k: Integer;
  begin
    if (not InCallee) and (ILoc[vmreg] >= 0) then
    begin
      SetLength(rest, Length(MemForm) - 1);      // drop MemForm[0] = $48 REX (EmitRR rebuilds it)
      for k := 1 to High(MemForm) do rest[k - 1] := MemForm[k];
      EmitRR(rest, scr, ILoc[vmreg]);
    end
    else
      E.MemOp(MemForm, scr, RBX, LongWord(vmreg) * 8);
  end;

  // al holds 0/1 -> IntRegs[Dest] := (al<>0) ? TrueVal : 0 (shared by the int and float comparisons).
  procedure CmpBoolToDest;
  begin
    E.EmitBytes([$0F, $B6, $C0]);           // movzx eax,al   (rax = 0/1)
    if TrueVal = -1 then
      E.EmitBytes([$48, $F7, $D8])          // neg rax        (0/-1)
    else if TrueVal <> 1 then
      begin E.EmitBytes([$48, $69, $C0]); E.Emit32(LongWord(TrueVal and $FFFFFFFF)); end;  // imul rax,rax,imm32
    IStore(I^.Dest, RAX);                   // dest := rax
  end;

  // Integer comparison Rd = (Rs1 <cc> Rs2) ? TrueVal : 0
  procedure IntCmp(SetCC: Byte);
  begin
    ILoad(RAX, I^.Src1);                    // mov rax, src1
    IOp([$48, $3B], RAX, I^.Src2);          // cmp rax, src2
    E.EmitBytes([$0F, SetCC, $C0]);         // setcc al
    CmpBoolToDest;
  end;

  // Float comparison Rd = (Rs1 <cc> Rs2) ? TrueVal : 0, with the interpreter's ORDERED IEEE semantics
  // (a NaN operand makes <,<=,>,>=,= false and <> true). ucomisd A,B sets CF=1 if A<B (or unordered),
  // ZF=1 if A==B (or unordered), PF=1 if unordered. Lt/Le/Gt/Ge reduce to seta/setae with an operand swap
  // (seta/setae are false when unordered); Eq/Ne need the parity flag to exclude/include the NaN case.
  // Kind: 0=Lt 1=Le 2=Gt 3=Ge 4=Eq 5=Ne.
  procedure FloatCmp(Kind: Integer);
  begin
    FLoad(XMM0, I^.Src1);                    // xmm0 = a
    FLoad(XMM1, I^.Src2);                    // xmm1 = b
    case Kind of
      0: begin E.EmitBytes([$66, $0F, $2E, $C8]); E.EmitBytes([$0F, $97, $C0]); end;  // a<b : ucomisd b,a ; seta  al
      1: begin E.EmitBytes([$66, $0F, $2E, $C8]); E.EmitBytes([$0F, $93, $C0]); end;  // a<=b: ucomisd b,a ; setae al
      2: begin E.EmitBytes([$66, $0F, $2E, $C1]); E.EmitBytes([$0F, $97, $C0]); end;  // a>b : ucomisd a,b ; seta  al
      3: begin E.EmitBytes([$66, $0F, $2E, $C1]); E.EmitBytes([$0F, $93, $C0]); end;  // a>=b: ucomisd a,b ; setae al
      4: begin E.EmitBytes([$66, $0F, $2E, $C1]);                                     // a=b : ucomisd a,b
               E.EmitBytes([$0F, $94, $C0]); E.EmitBytes([$0F, $9B, $C1]);            //       sete al ; setnp cl
               E.EmitBytes([$20, $C8]); end;                                          //       and al,cl (equal AND ordered)
      5: begin E.EmitBytes([$66, $0F, $2E, $C1]);                                     // a<>b: ucomisd a,b
               E.EmitBytes([$0F, $95, $C0]); E.EmitBytes([$0F, $9A, $C1]);            //       setne al ; setp cl
               E.EmitBytes([$08, $C8]); end;                                          //       or al,cl (not-equal OR unordered)
    end;
    CmpBoolToDest;
  end;

  // Float op:  Rd = Rs1 <sse> Rs2   (compute in xmm0, honouring register allocation of the operands)
  procedure FloatBin(const SseOp: array of Byte);
  begin
    FLoad(XMM0, I^.Src1);
    FOp(SseOp, XMM0, I^.Src2);
    FStore(I^.Dest, XMM0);
  end;

  // mov <reg>, [r8 + disp32]   (REX.W + REX.B; r8 = array descriptor base)
  // mov <reg (rax/rcx/rdx, <8)>, [r8+disp]   (REX.W + REX.B for r8 base)
  procedure R8Load(RegField: Byte; Disp: LongWord);
  begin
    E.Emit8($49);
    E.Emit8($8B);
    E.Emit8($80 or ((RegField and 7) shl 3));   // modrm mod=10 reg=RegField rm=000 (r8 low bits)
    E.Emit32(Disp);
  end;
  // mov <native reg (0..15)>, [r8+disp]   (adds REX.R for r9..r15; used to load cached bases/counts)
  procedure R8LoadR(reg: Integer; Disp: LongWord);
  var rex: Byte;
  begin
    rex := $49;                                  // REX.W + REX.B (base r8)
    if reg >= 8 then rex := rex or $04;          // REX.R
    E.Emit8(rex); E.Emit8($8B);
    E.Emit8($80 or ((reg and 7) shl 3));         // mod=10 reg=reg rm=000 (r8)
    E.Emit32(Disp);
  end;

  // --- array base/count caching (J5c): index into the per-loop cache, or -1 if this array is not cached ---
  function CArrIdx(ArrayId: Integer): Integer;
  var q: Integer;
  begin
    Result := -1;
    if InCallee then Exit;    // callee arrays never use the caller's base/count cache registers
    for q := 0 to NCArr - 1 do
      if CArrId[q] = ArrayId then begin Result := q; Exit; end;
  end;
  // Emit `cmp rcx, <count>` using the cached count reg if present, else reloading Count into rdx.
  procedure ArrCountCmp(ArrayId, CountReg: Integer);
  begin
    if CountReg >= 0 then
      EmitRR([$3B], RCX, CountReg)                          // cmp rcx, CountReg
    else
    begin
      R8Load(RDX, LongWord(ArrayId) * 24 + 16);             // mov rdx, Count
      E.EmitBytes([$48, $39, $D1]);                          // cmp rcx, rdx
    end;
  end;
  // Return the register holding the array base: the cached one, or rdx after reloading from the descriptor.
  function ArrBaseReg(ArrayId, Off, CachedBase: Integer): Integer;
  begin
    if CachedBase >= 0 then Result := CachedBase
    else begin R8Load(RDX, LongWord(ArrayId) * 24 + LongWord(Off)); Result := RDX; end;
  end;
  // Emit a load/store of xmm0/rax to [BaseReg + rcx*8].  BaseReg may be rdx or any r8..r15 (REX.B).
  procedure ArrDataAccess(IsFloat, IsStore: Boolean; BaseReg: Integer);
  var sib: Byte;
  begin
    sib := $C8 or (BaseReg and 7);               // scale=8 (11), index=rcx (001), base=BaseReg&7
    if IsFloat then
    begin
      E.Emit8($F2);
      if BaseReg >= 8 then E.Emit8($41);         // REX.B
      E.Emit8($0F);
      if IsStore then E.Emit8($11) else E.Emit8($10);
      E.Emit8($04); E.Emit8(sib);                // movsd xmm0, [base+rcx*8]  (or store)
    end
    else
    begin
      if BaseReg >= 8 then E.Emit8($49) else E.Emit8($48);   // REX.W (+B)
      if IsStore then E.Emit8($89) else E.Emit8($8B);
      E.Emit8($04); E.Emit8(sib);                // mov rax, [base+rcx*8]  (or store)
    end;
  end;

  // FloatRegs[Dst] := arr[idx]  (OOB -> 0.0, MODERN). ArrayId is a compile-time constant.
  procedure ArrLoadF(ArrayId, IdxReg, DstReg: Integer);
  var pOOB, pDone, ix, baseR: Integer;
  begin
    ix := CArrIdx(ArrayId);
    ILoad(RCX, IdxReg);                                     // rcx := index (reg or [rbx+idx])
    if ix >= 0 then ArrCountCmp(ArrayId, CArrCount[ix]) else ArrCountCmp(ArrayId, -1);
    E.EmitBytes([$73, $00]); pOOB := E.Len - 1;             // jae oob (unsigned: idx<0 or >=Count)
    if ix >= 0 then baseR := ArrBaseReg(ArrayId, 8, CArrBase[ix]) else baseR := ArrBaseReg(ArrayId, 8, -1);
    ArrDataAccess(True, False, baseR);                      // movsd xmm0, [base+rcx*8]
    E.EmitBytes([$EB, $00]); pDone := E.Len - 1;            // jmp done
    E.PatchByte(pOOB, Byte(E.Len - (pOOB + 1)));
    E.EmitBytes([$0F, $57, $C0]);                            // xorps xmm0,xmm0  -> 0.0
    E.PatchByte(pDone, Byte(E.Len - (pDone + 1)));
    FStore(DstReg, XMM0);                                    // FloatRegs[Dst] := xmm0 (reg or memory)
  end;

  // arr[idx] := FloatRegs[Val]  (OOB -> dropped, MODERN).
  procedure ArrStoreF(ArrayId, IdxReg, ValReg: Integer);
  var pSkip, ix, baseR: Integer;
  begin
    ix := CArrIdx(ArrayId);
    ILoad(RCX, IdxReg);                                     // rcx := index (reg or [rbx+idx])
    if ix >= 0 then ArrCountCmp(ArrayId, CArrCount[ix]) else ArrCountCmp(ArrayId, -1);
    E.EmitBytes([$73, $00]); pSkip := E.Len - 1;           // jae skip
    if ix >= 0 then baseR := ArrBaseReg(ArrayId, 8, CArrBase[ix]) else baseR := ArrBaseReg(ArrayId, 8, -1);
    FLoad(XMM0, ValReg);                                     // xmm0 := FloatRegs[Val] (reg or memory)
    ArrDataAccess(True, True, baseR);                       // movsd [base+rcx*8], xmm0
    E.PatchByte(pSkip, Byte(E.Len - (pSkip + 1)));
  end;

  // IntRegs[Dst] := arr[idx]  (OOB -> 0, MODERN). Int arrays are Int64-per-element (IntData at desc+0);
  // the interpreter stores the raw register value (narrowing happens on a separate op), so this is exact.
  procedure ArrLoadI(ArrayId, IdxReg, DstReg: Integer);
  var pOOB, pDone, ix, baseR: Integer;
  begin
    ix := CArrIdx(ArrayId);
    ILoad(RCX, IdxReg);                                     // rcx := index
    if ix >= 0 then ArrCountCmp(ArrayId, CArrCount[ix]) else ArrCountCmp(ArrayId, -1);
    E.EmitBytes([$73, $00]); pOOB := E.Len - 1;            // jae oob (unsigned: idx<0 or >=Count)
    if ix >= 0 then baseR := ArrBaseReg(ArrayId, 0, CArrBase[ix]) else baseR := ArrBaseReg(ArrayId, 0, -1);
    ArrDataAccess(False, False, baseR);                    // mov rax, [base+rcx*8]
    E.EmitBytes([$EB, $00]); pDone := E.Len - 1;           // jmp done
    E.PatchByte(pOOB, Byte(E.Len - (pOOB + 1)));
    E.EmitBytes([$48, $31, $C0]);                            // xor rax,rax  -> 0
    E.PatchByte(pDone, Byte(E.Len - (pDone + 1)));
    IStore(DstReg, RAX);                                    // IntRegs[Dst] := rax (reg or memory)
  end;

  // arr[idx] := IntRegs[Val]  (OOB -> dropped, MODERN).
  procedure ArrStoreI(ArrayId, IdxReg, ValReg: Integer);
  var pSkip, ix, baseR: Integer;
  begin
    ix := CArrIdx(ArrayId);
    ILoad(RCX, IdxReg);                                     // rcx := index
    if ix >= 0 then ArrCountCmp(ArrayId, CArrCount[ix]) else ArrCountCmp(ArrayId, -1);
    E.EmitBytes([$73, $00]); pSkip := E.Len - 1;           // jae skip
    if ix >= 0 then baseR := ArrBaseReg(ArrayId, 0, CArrBase[ix]) else baseR := ArrBaseReg(ArrayId, 0, -1);
    ILoad(RAX, ValReg);                                     // rax := IntRegs[Val]
    ArrDataAccess(False, True, baseR);                     // mov [base+rcx*8], rax
    E.PatchByte(pSkip, Byte(E.Len - (pSkip + 1)));
  end;

  // Scan the loop for FLOAT register operands. Mark=False: compute FMaxReg. Mark=True: flag each used
  // float reg as -2 in FLoc (candidate for allocation).
  procedure ScanF(Mark: Boolean);
  var q: Integer; J: PBcInstr;
    procedure T(r: Word);
    begin
      if Mark then FLoc[r] := -2
      else if r > FMaxReg then FMaxReg := r;
    end;
  begin
    for q := HeaderPC to EndPC do
    begin
      J := @Prog[q];
      case J^.OpCode of
        bcLoadConstFloat, bcIntToFloat: T(J^.Dest);
        bcCopyFloat, bcMathSqr: begin T(J^.Dest); T(J^.Src1); end;
        bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat: begin T(J^.Dest); T(J^.Src1); T(J^.Src2); end;
        bcArrayLoadFloat:  T(J^.Dest);               // Dest = loaded float
        bcArrayStoreFloat: T(J^.Dest);               // Dest = stored VALUE (float)
        bcXferStoreFloat:  T(J^.Src1);               // Src1 = value moved to the transfer slot
        bcXferLoadFloat:   T(J^.Dest);               // Dest = value moved from the transfer slot
        bcCmpLtFloat, bcCmpLeFloat, bcCmpGtFloat, bcCmpGeFloat, bcCmpEqFloat, bcCmpNeFloat:
          begin T(J^.Src1); T(J^.Src2); end;         // float operands (Dest is an int reg -> ScanI)
      end;
    end;
  end;

  // Scan the loop for INTEGER register operands. Mark=False: compute IMaxReg. Mark=True: flag each
  // used int reg as -2 in ILoc (allocation candidate). CAUTION: for bcArrayLoad/StoreFloat, Src1 is
  // the array id (a constant), NOT a register -- only Src2 (the index) is an int register.
  procedure ScanI(Mark: Boolean);
  var q: Integer; J: PBcInstr;
    procedure T(r: Word);
    begin
      if Mark then ILoc[r] := -2
      else if r > IMaxReg then IMaxReg := r;
    end;
  begin
    for q := HeaderPC to EndPC do
    begin
      J := @Prog[q];
      case J^.OpCode of
        bcLoadConstInt: T(J^.Dest);
        bcCopyInt: begin T(J^.Dest); T(J^.Src1); end;
        bcAddInt, bcSubInt, bcMulInt: begin T(J^.Dest); T(J^.Src1); T(J^.Src2); end;
        bcCmpLtInt, bcCmpLeInt, bcCmpGtInt, bcCmpGeInt, bcCmpEqInt, bcCmpNeInt:
          begin T(J^.Dest); T(J^.Src1); T(J^.Src2); end;
        bcIntToFloat: T(J^.Src1);                    // int input (Dest is float)
        bcArrayLoadFloat, bcArrayStoreFloat: T(J^.Src2);   // Src2 = index; Src1 is the array id
        bcArrayLoadInt, bcArrayStoreInt: begin T(J^.Dest); T(J^.Src2); end;  // Dest=result/value, Src2=index
        bcXferStoreInt: T(J^.Src1);                  // Src1 = value moved to the transfer slot
        bcXferLoadInt:  T(J^.Dest);                  // Dest = value moved from the transfer slot
        bcCmpLtFloat, bcCmpLeFloat, bcCmpGtFloat, bcCmpGeFloat, bcCmpEqFloat, bcCmpNeFloat:
          T(J^.Dest);                                // float compare writes an int result reg
        bcJumpIfZero, bcJumpIfNotZero: T(J^.Src1);
      end;
    end;
  end;

  // Collect the distinct arrays accessed in the loop (with their descriptor base offset and use count),
  // so the invariant base/count loads can be hoisted into registers (J5c). Src1 of an array op is the
  // array id (a constant), Src2 the index.
  procedure ScanArr;
  var q, k, off, aid: Integer; J: PBcInstr;
  begin
    NCArr := 0;
    for q := HeaderPC to EndPC do
    begin
      J := @Prog[q];
      case J^.OpCode of
        bcArrayLoadFloat, bcArrayStoreFloat: off := 8;
        bcArrayLoadInt,   bcArrayStoreInt:   off := 0;
      else
        continue;
      end;
      aid := J^.Src1;
      k := 0;
      while (k < NCArr) and (CArrId[k] <> aid) do Inc(k);
      if k = NCArr then
      begin
        Inc(NCArr);
        SetLength(CArrId, NCArr); SetLength(CArrOff, NCArr); SetLength(CArrUses, NCArr);
        SetLength(CArrBase, NCArr); SetLength(CArrCount, NCArr);
        CArrId[k] := aid; CArrOff[k] := off; CArrUses[k] := 0;
        CArrBase[k] := -1; CArrCount[k] := -1;
      end;
      Inc(CArrUses[k]);
    end;
  end;

  // Save VM register bank [0,N) qwords (BankBase = rbx for int / rsi for float) to the stack scratch area
  // at [rsp+Disp+i*8]. Replicates FramePush for an inlined SUB call. Uses rax (temp) and rcx (counter);
  // the loop body is < 128 bytes so the back-branch is a rel8. N < 2^31.
  procedure EmitSaveBank(BankBase: Byte; Disp: LongWord; N: Integer);
  var loopStart: Integer;
  begin
    if N <= 0 then Exit;
    E.EmitBytes([$31, $C9]);                                       // xor ecx,ecx
    loopStart := E.Len;
    E.EmitBytes([$48, $8B, $04, Byte($C8 or (BankBase and 7))]);   // mov rax,[BankBase+rcx*8]
    E.EmitBytes([$48, $89, $84, $CC]); E.Emit32(Disp);            // mov [rsp+Disp+rcx*8],rax
    E.EmitBytes([$48, $FF, $C1]);                                 // inc rcx
    E.EmitBytes([$48, $81, $F9]); E.Emit32(LongWord(N));          // cmp rcx,N
    E.EmitBytes([$7C, $00]);                                      // jl rel8 -> loopStart
    E.PatchByte(E.Len - 1, Byte(loopStart - E.Len));
  end;
  // Restore VM register bank [0,N) from the stack scratch (the FramePop counterpart).
  procedure EmitRestoreBank(BankBase: Byte; Disp: LongWord; N: Integer);
  var loopStart: Integer;
  begin
    if N <= 0 then Exit;
    E.EmitBytes([$31, $C9]);                                       // xor ecx,ecx
    loopStart := E.Len;
    E.EmitBytes([$48, $8B, $84, $CC]); E.Emit32(Disp);           // mov rax,[rsp+Disp+rcx*8]
    E.EmitBytes([$48, $89, $04, Byte($C8 or (BankBase and 7))]);  // mov [BankBase+rcx*8],rax
    E.EmitBytes([$48, $FF, $C1]);                                 // inc rcx
    E.EmitBytes([$48, $81, $F9]); E.Emit32(LongWord(N));          // cmp rcx,N
    E.EmitBytes([$7C, $00]);                                      // jl rel8 -> loopStart
    E.PatchByte(E.Len - 1, Byte(loopStart - E.Len));
  end;

  // Return the call-site index for a bcCallSub at absolute PC apc (populated by BuildCallSites), or -1
  // if that call was found non-inlinable (then the op stays unsupported and the loop bails).
  function FindCallSite(apc: Integer): Integer;
  var k: Integer;
  begin
    Result := -1;
    for k := 0 to NCall - 1 do
      if CallPC[k] = apc then begin Result := k; Exit; end;
  end;

  // Emit native code for one bytecode instruction at absolute PC apc. Returns False (bail) on any
  // unsupported opcode or a bcCallSub that could not be inlined. A bcCallSub emits an inline copy of the
  // callee body wrapped in a native FramePush/Pop; the callee is compiled all-memory (InCallee).
  function EmitOne(apc: Integer): Boolean;
  var cs, cpc: Integer;
  begin
    Result := False;
    I := @Prog[apc];
    JitDiagCurOp := I^.OpCode; JitDiagCurPC := apc;   // last op seen -> the culprit if we bail below
    Dd := LongWord(I^.Dest) * 8;
    S1 := LongWord(I^.Src1) * 8;
    S2 := LongWord(I^.Src2) * 8;
    case I^.OpCode of
      bcLoadConstInt:
        if (not InCallee) and (ILoc[I^.Dest] >= 0) then
          MovImm64(ILoc[I^.Dest], I^.Immediate)                    // mov gpr, imm64
        else
        begin
          MovImm64(RAX, I^.Immediate);                             // mov rax, imm64
          E.MemOp([$48, $89], RAX, RBX, Dd);                       // mov [rbx+d],rax
        end;
      bcLoadConstFloat:
        begin
          E.EmitBytes([$48, $B8]); E.Emit64(QWord(I^.Immediate));  // mov rax, rawbits
          E.EmitBytes([$66, $48, $0F, $6E, $C0]);                  // movq xmm0, rax
          FStore(I^.Dest, XMM0);                                   // -> xmm reg or [rsi+d]
        end;
      bcCopyInt:
        if (not InCallee) and (ILoc[I^.Dest] >= 0) and (ILoc[I^.Src1] >= 0) then
          MovRR(ILoc[I^.Dest], ILoc[I^.Src1])   // reg-reg copy in one move
        else
        begin
          ILoad(RAX, I^.Src1);
          IStore(I^.Dest, RAX);
        end;
      bcCopyFloat:
        begin
          FLoad(XMM0, I^.Src1);
          FStore(I^.Dest, XMM0);
        end;
      bcAddInt:
        begin
          ILoad(RAX, I^.Src1);
          IOp([$48, $03], RAX, I^.Src2);            // add rax, src2
          IStore(I^.Dest, RAX);
        end;
      bcSubInt:
        begin
          ILoad(RAX, I^.Src1);
          IOp([$48, $2B], RAX, I^.Src2);            // sub rax, src2
          IStore(I^.Dest, RAX);
        end;
      bcMulInt:
        begin
          ILoad(RAX, I^.Src1);
          IOp([$48, $0F, $AF], RAX, I^.Src2);       // imul rax, src2
          IStore(I^.Dest, RAX);
        end;
      bcAddFloat: FloatBin([$F2, $0F, $58]);        // addsd
      bcSubFloat: FloatBin([$F2, $0F, $5C]);        // subsd
      bcMulFloat: FloatBin([$F2, $0F, $59]);        // mulsd
      // Array / sqrt / div: MODERN edge semantics only (AllowUnsafe); otherwise bail.
      bcDivFloat: if AllowUnsafe then FloatBin([$F2, $0F, $5E]) else Exit;   // divsd (IEEE = MODERN)
      bcMathSqr:
        if AllowUnsafe then
        begin
          FOp([$F2, $0F, $51], XMM0, I^.Src1);          // sqrtsd xmm0, <s1>
          FStore(I^.Dest, XMM0);
        end
        else Exit;
      bcIntToFloat:
        begin
          if (not InCallee) and (ILoc[I^.Src1] >= 0) then
          begin
            gpr := ILoc[I^.Src1];
            E.Emit8($F2);
            if gpr >= 8 then E.Emit8($49) else E.Emit8($48);   // REX.W (+B)
            E.EmitBytes([$0F, $2A]);
            E.Emit8($C0 or (XMM0 shl 3) or (gpr and 7));        // cvtsi2sd xmm0, gpr
          end
          else
          begin
            E.EmitBytes([$F2, $48, $0F, $2A]);                  // cvtsi2sd xmm0, [rbx+s1]
            E.Emit8($80 or (XMM0 shl 3) or RBX); E.Emit32(S1);
          end;
          FStore(I^.Dest, XMM0);
        end;
      bcArrayLoadFloat:  if AllowUnsafe then ArrLoadF(I^.Src1, I^.Src2, I^.Dest) else Exit;
      bcArrayStoreFloat: if AllowUnsafe then ArrStoreF(I^.Src1, I^.Src2, I^.Dest) else Exit;
      bcArrayLoadInt:    if AllowUnsafe then ArrLoadI(I^.Src1, I^.Src2, I^.Dest) else Exit;
      bcArrayStoreInt:   if AllowUnsafe then ArrStoreI(I^.Src1, I^.Src2, I^.Dest) else Exit;
      bcCmpLtInt: IntCmp($9C);                      // setl
      bcCmpLeInt: IntCmp($9E);                      // setle
      bcCmpGtInt: IntCmp($9F);                      // setg
      bcCmpGeInt: IntCmp($9D);                      // setge
      bcCmpEqInt: IntCmp($94);                      // sete
      bcCmpNeInt: IntCmp($95);                      // setne
      bcCmpLtFloat: FloatCmp(0);
      bcCmpLeFloat: FloatCmp(1);
      bcCmpGtFloat: FloatCmp(2);
      bcCmpGeFloat: FloatCmp(3);
      bcCmpEqFloat: FloatCmp(4);
      bcCmpNeFloat: FloatCmp(5);
      // Transfer registers (args / result): XferInt/XferFloat bases are baked as immediates into rdx.
      bcXferStoreInt:
        begin
          ILoad(RAX, I^.Src1);
          MovImm64(RDX, Int64(XferIntBase));
          E.MemOp([$48, $89], RAX, RDX, LongWord(I^.Immediate) * 8);       // mov [rdx+slot*8], rax
        end;
      bcXferStoreFloat:
        begin
          FLoad(XMM0, I^.Src1);
          MovImm64(RDX, Int64(XferFloatBase));
          E.MemOp([$F2, $0F, $11], XMM0, RDX, LongWord(I^.Immediate) * 8);  // movsd [rdx+slot*8], xmm0
        end;
      bcXferLoadInt:
        begin
          MovImm64(RDX, Int64(XferIntBase));
          E.MemOp([$48, $8B], RAX, RDX, LongWord(I^.Immediate) * 8);        // mov rax, [rdx+slot*8]
          IStore(I^.Dest, RAX);
        end;
      bcXferLoadFloat:
        begin
          MovImm64(RDX, Int64(XferFloatBase));
          E.MemOp([$F2, $0F, $10], XMM0, RDX, LongWord(I^.Immediate) * 8);  // movsd xmm0, [rdx+slot*8]
          FStore(I^.Dest, XMM0);
        end;
      // Block-scoped record marks: a no-op here -- a loop we compile allocates no records (bcRecordNew is
      // unsupported and bails), so RecordCount is invariant across the mark and reclaiming to it is exact.
      bcRecMarkPush, bcRecMarkPop: ;
      // Inlined SUB call (J6): FramePush (native bank save) + inline callee body (all-memory) + FramePop.
      bcCallSub:
        begin
          cs := FindCallSite(apc);
          if cs < 0 then Exit;                       // not inlinable -> bail
          CurSaveN := CallSaveN[cs];
          EmitSaveBank(RBX, 0, CurSaveN);                       // save int bank [0,CurSaveN)
          EmitSaveBank(RSI, LongWord(CurSaveN) * 8, CurSaveN);  // save float bank [0,CurSaveN)
          InCallee := True;
          for cpc := CallEntry[cs] to CallRet[cs] do
          begin
            NativeOff[cpc] := E.Len;
            if not EmitOne(cpc) then begin InCallee := False; Exit; end;
          end;
          InCallee := False;
        end;
      bcReturnSub:
        if InCallee then
        begin
          EmitRestoreBank(RBX, 0, CurSaveN);                       // restore int bank
          EmitRestoreBank(RSI, LongWord(CurSaveN) * 8, CurSaveN);  // restore float bank -> fall through
        end
        else
          Exit;                                      // a bare RETURN at loop top level is not compilable
      bcJump:
        begin
          target := Integer(I^.Immediate);
          if InRange[target] then JmpRel(target)
          else
          begin
            E.EmitBytes([$B8]); E.Emit32(LongWord(target));   // mov eax, target (exit PC)
            JmpRel(-1);                                        // jmp epilogue
          end;
        end;
      bcJumpIfZero, bcJumpIfNotZero:
        begin
          target := Integer(I^.Immediate);
          ILoad(RAX, I^.Src1);                      // rax := condition (reg or [rbx+s1])
          E.EmitBytes([$48, $85, $C0]);             // test rax,rax
          if InRange[target] then
          begin
            if I^.OpCode = bcJumpIfZero then JccRel($84, target)   // jz
            else JccRel($85, target);                              // jnz
          end
          else
          begin
            // Conditional EXIT: skip over the exit sequence when the branch is NOT taken.
            if I^.OpCode = bcJumpIfZero then E.EmitBytes([$75, $00])   // jnz short +len(exit)
            else E.EmitBytes([$74, $00]);                             // jz  short +len(exit)
            d := E.Len;                               // start of the exit sequence
            E.EmitBytes([$B8]); E.Emit32(LongWord(target));   // mov eax, target
            JmpRel(-1);                                        // jmp epilogue
            E.PatchByte(d - 1, Byte(E.Len - d));               // patch the skip displacement
          end;
        end;
    else
      // Unsupported opcode -> bail: the whole loop stays interpreted.
      Exit;
    end;
    Result := True;
  end;

  // Pre-scan the caller range for inlinable bcCallSub sites: locate each callee's single ReturnSub,
  // reject nested calls / string / record ops, compute the frame save size, and mark the callee PCs as
  // in-range so their internal jumps resolve as internal (not loop exits).
  procedure BuildCallSites;
  var q, r, ep, rp, md: Integer; J: PBcInstr; ok: Boolean;
  begin
    NCall := 0;
    ScratchBytes := 0;
    for q := HeaderPC to EndPC do
    begin
      if Prog[q].OpCode <> bcCallSub then Continue;
      ep := Integer(Prog[q].Immediate);
      if (ep < 0) or (ep >= ProgLen) then Continue;    // out of range -> not inlinable
      rp := -1; ok := True; md := 0; r := ep;
      while r < ProgLen do
      begin
        J := @Prog[r];
        if J^.OpCode = bcReturnSub then begin rp := r; Break; end;
        case J^.OpCode of
          bcCallSub, bcCallSubIndirect,                 // no nested calls in V1
          bcXferStoreString, bcXferLoadString,          // no string transfer
          bcRecordNew, bcRecordNewArray: begin ok := False; Break; end;   // no record allocation
        end;
        if J^.Dest > md then md := J^.Dest;
        Inc(r);
      end;
      if (rp < 0) or (not ok) then Continue;
      Inc(NCall);
      SetLength(CallPC, NCall); SetLength(CallEntry, NCall);
      SetLength(CallRet, NCall); SetLength(CallSaveN, NCall);
      CallPC[NCall - 1] := q; CallEntry[NCall - 1] := ep; CallRet[NCall - 1] := rp;
      CallSaveN[NCall - 1] := md + 1;                  // save [0,maxDest+1) of each bank
      for r := ep to rp do InRange[r] := True;
      if (md + 1) * 16 > ScratchBytes then ScratchBytes := (md + 1) * 16;
    end;
  end;

begin
  Result := nil;
  Prog := PBcInstr(Ins);
  if (HeaderPC < 0) or (EndPC < HeaderPC) then Exit;

  // Quick reject: no CALL / RETURN / anything that leaves the loop frame may appear (handled by the
  // op whitelist below), but also refuse loops longer than a sane cap.
  if EndPC - HeaderPC > 4096 then Exit;

  if ProgLen <= EndPC then Exit;      // NativeOff/InRange are indexed by absolute PC
  E := TX86Emitter.Create;
  SetLength(NativeOff, ProgLen);
  SetLength(InRange, ProgLen);
  for d := 0 to ProgLen - 1 do InRange[d] := False;
  for d := HeaderPC to EndPC do InRange[d] := True;
  InCallee := False;
  BuildCallSites;                     // find inlinable bcCallSub, mark callee ranges, size the stack scratch
  NFix := 0;

  // --- float register allocation (J4): assign xmm2..xmm7 to the used VM float regs, spill the rest ---
  FMaxReg := -1;
  ScanF(False);                             // compute FMaxReg
  SetLength(FLoc, FMaxReg + 2);
  for fi := 0 to High(FLoc) do FLoc[fi] := -1;
  if FMaxReg >= 0 then ScanF(True);         // mark used regs as -2
  NextXmm := 2; SaveX6 := False; SaveX7 := False;
  for fi := 0 to FMaxReg do
    if FLoc[fi] = -2 then
    begin
      if NextXmm <= 7 then
      begin
        FLoc[fi] := NextXmm;
        if NextXmm = 6 then SaveX6 := True;
        if NextXmm = 7 then SaveX7 := True;
        Inc(NextXmm);
      end
      else
        FLoc[fi] := -1;                     // overflow -> memory-homed
    end;

  // --- integer GPR allocation (J5): r9/r10/r11 (volatile) then r12..r15 (callee-saved) ---
  IntPool[0] := R9;  IntPool[1] := R10; IntPool[2] := R11;
  IntPool[3] := R12; IntPool[4] := R13; IntPool[5] := R14; IntPool[6] := R15;
  for gpr := 0 to 15 do SaveGpr[gpr] := False;
  IMaxReg := -1;
  ScanI(False);                             // compute IMaxReg
  SetLength(ILoc, IMaxReg + 2);
  for ii := 0 to High(ILoc) do ILoc[ii] := -1;
  if IMaxReg >= 0 then ScanI(True);         // mark used regs as -2
  NextGpr := 0;
  for ii := 0 to IMaxReg do
    if ILoc[ii] = -2 then
    begin
      if NextGpr <= 6 then
      begin
        ILoc[ii] := IntPool[NextGpr];
        if IntPool[NextGpr] >= 12 then SaveGpr[IntPool[NextGpr]] := True;  // callee-saved
        Inc(NextGpr);
      end
      else
        ILoc[ii] := -1;                     // overflow -> memory-homed
    end;

  // --- array base/count caching (J5c LICM): hand the GPRs left free after int allocation to the
  // loop-invariant array base pointers and counts, most-used arrays first (base then count each). ---
  NCArr := 0;
  if AllowUnsafe then                        // arrays are only compiled under AllowUnsafe anyway
  begin
    for gpr := 0 to 15 do GprUsed[gpr] := False;
    for ii := 0 to IMaxReg do
      if ILoc[ii] >= 0 then GprUsed[ILoc[ii]] := True;
    ScanArr;
    // selection sort the parallel arrays by use count, descending (NCArr is tiny)
    for ci := 0 to NCArr - 2 do
      for cj := ci + 1 to NCArr - 1 do
        if CArrUses[cj] > CArrUses[ci] then
        begin
          ct := CArrId[ci];    CArrId[ci]    := CArrId[cj];    CArrId[cj]    := ct;
          ct := CArrOff[ci];   CArrOff[ci]   := CArrOff[cj];   CArrOff[cj]   := ct;
          ct := CArrUses[ci];  CArrUses[ci]  := CArrUses[cj];  CArrUses[cj]  := ct;
        end;
    // assign base then count for each array in priority order, from the free r9..r15
    for ci := 0 to NCArr - 1 do
    begin
      for ct := 0 to 6 do                    // base
        if (CArrBase[ci] < 0) and (not GprUsed[IntPool[ct]]) then
        begin
          CArrBase[ci] := IntPool[ct]; GprUsed[IntPool[ct]] := True;
          if IntPool[ct] >= 12 then SaveGpr[IntPool[ct]] := True;
        end;
      for ct := 0 to 6 do                    // count
        if (CArrCount[ci] < 0) and (not GprUsed[IntPool[ct]]) then
        begin
          CArrCount[ci] := IntPool[ct]; GprUsed[IntPool[ct]] := True;
          if IntPool[ct] >= 12 then SaveGpr[IntPool[ct]] := True;
        end;
    end;
  end;

  try
    // --- prologue ---  (Win64: rcx=IntRegs, rdx=FloatRegs; SysV: rdi/rsi)
    E.EmitBytes([$53]);                       // push rbx
    E.EmitBytes([$56]);                       // push rsi
    // Save the callee-saved GPRs (r12..r15) that got allocated (both ABIs: r12..r15 are non-volatile).
    if SaveGpr[R12] then E.EmitBytes([$41, $54]);   // push r12
    if SaveGpr[R13] then E.EmitBytes([$41, $55]);   // push r13
    if SaveGpr[R14] then E.EmitBytes([$41, $56]);   // push r14
    if SaveGpr[R15] then E.EmitBytes([$41, $57]);   // push r15
    {$IFDEF WINDOWS}
    E.EmitBytes([$48, $89, $CB]);             // mov rbx, rcx    (arg0 = IntRegs)
    E.EmitBytes([$48, $89, $D6]);             // mov rsi, rdx    (arg1 = FloatRegs)
    // arg2 (ArrDesc) is already in r8 on Win64.
    {$ELSE}
    E.EmitBytes([$48, $89, $FB]);             // mov rbx, rdi    (arg0 = IntRegs)
    E.EmitBytes([$48, $89, $F6]);             // mov rsi, rsi    (arg1 = FloatRegs, already in rsi)
    E.EmitBytes([$49, $89, $D0]);             // mov r8, rdx     (arg2 = ArrDesc)
    {$ENDIF}

    // Save the callee-saved xmm6/xmm7 (Win64) if they were allocated, then load the allocated VM float
    // regs from memory into their native xmm.
    if SaveX6 or SaveX7 then
    begin
      E.EmitBytes([$48, $83, $EC, $10]);                          // sub rsp, 16
      if SaveX6 then E.EmitBytes([$F2, $0F, $11, $74, $24, $00]); // movsd [rsp],   xmm6
      if SaveX7 then E.EmitBytes([$F2, $0F, $11, $7C, $24, $08]); // movsd [rsp+8], xmm7
    end;
    for fi := 0 to FMaxReg do
      if FLoc[fi] >= 0 then
        E.MemOp([$F2, $0F, $10], FLoc[fi], RSI, LongWord(fi) * 8);  // movsd xmm_alloc, [rsi+fi*8]
    // Load the allocated VM int regs from memory into their native GPR.
    for ii := 0 to IMaxReg do
      if ILoc[ii] >= 0 then
        LoadRegMem(ILoc[ii], LongWord(ii) * 8);                     // mov gpr, [rbx+ii*8]
    // Load the cached array base pointers / counts from the descriptor (r8) -- loop-invariant.
    for ci := 0 to NCArr - 1 do
    begin
      if CArrBase[ci]  >= 0 then R8LoadR(CArrBase[ci],  LongWord(CArrId[ci]) * 24 + LongWord(CArrOff[ci]));
      if CArrCount[ci] >= 0 then R8LoadR(CArrCount[ci], LongWord(CArrId[ci]) * 24 + 16);
    end;

    // Reserve stack scratch for inlined SUB frame save/restore (sits below the xmm6/7 save area; rsp is
    // stable through the body so the scratch is at a fixed [rsp+0..ScratchBytes) offset).
    if ScratchBytes > 0 then
    begin
      E.EmitBytes([$48, $81, $EC]); E.Emit32(LongWord(ScratchBytes));   // sub rsp, ScratchBytes
    end;

    // --- body --- (each instruction is emitted by EmitOne; a bcCallSub emits its callee inline)
    for pc := HeaderPC to EndPC do
    begin
      NativeOff[pc] := E.Len;
      if not EmitOne(pc) then Exit;
    end;

    // Fall-through past the last body instruction is also a loop exit to EndPC+1.
    E.EmitBytes([$B8]); E.Emit32(LongWord(EndPC + 1));   // mov eax, EndPC+1
    JmpRel(-1);

    // --- epilogue --- (rax already holds the exit PC; the writebacks below do not touch rax)
    EpilogueOff := E.Len;
    // Write the allocated VM int regs back to memory so the interpreter sees their final values.
    for ii := 0 to IMaxReg do
      if ILoc[ii] >= 0 then
        StoreRegMem(ILoc[ii], LongWord(ii) * 8);                   // mov [rbx+ii*8], gpr
    // Write the allocated float regs back to memory so the interpreter sees their final values.
    for fi := 0 to FMaxReg do
      if FLoc[fi] >= 0 then
        E.MemOp([$F2, $0F, $11], FLoc[fi], RSI, LongWord(fi) * 8);  // movsd [rsi+fi*8], xmm_alloc
    // Release the inlined-call scratch (brings rsp back to the xmm6/7 save area).
    if ScratchBytes > 0 then
    begin
      E.EmitBytes([$48, $81, $C4]); E.Emit32(LongWord(ScratchBytes));   // add rsp, ScratchBytes
    end;
    // Restore callee-saved xmm and the stack.
    if SaveX6 or SaveX7 then
    begin
      if SaveX6 then E.EmitBytes([$F2, $0F, $10, $74, $24, $00]); // movsd xmm6, [rsp]
      if SaveX7 then E.EmitBytes([$F2, $0F, $10, $7C, $24, $08]); // movsd xmm7, [rsp+8]
      E.EmitBytes([$48, $83, $C4, $10]);                          // add rsp, 16
    end;
    // Restore the callee-saved GPRs (reverse of the prologue push order).
    if SaveGpr[R15] then E.EmitBytes([$41, $5F]);   // pop r15
    if SaveGpr[R14] then E.EmitBytes([$41, $5E]);   // pop r14
    if SaveGpr[R13] then E.EmitBytes([$41, $5D]);   // pop r13
    if SaveGpr[R12] then E.EmitBytes([$41, $5C]);   // pop r12
    E.EmitBytes([$5E]);          // pop rsi
    E.EmitBytes([$5B]);          // pop rbx
    E.EmitBytes([$C3]);          // ret

    // --- patch fixups ---
    for pc := 0 to NFix - 1 do
    begin
      if Fixups[pc].TargetPC = -1 then
        target := EpilogueOff
      else
        target := NativeOff[Fixups[pc].TargetPC];   // absolute PC (covers inlined-callee targets too)
      E.Patch32(Fixups[pc].PatchOff, LongWord(target - (Fixups[pc].PatchOff + 4)));
    end;

    Result := TExecMem.Create(E);
    if Result.Ptr = nil then FreeAndNil(Result);
  finally
    E.Free;
  end;
end;

{ ---------------- self-test ---------------- }

type
  TAddFn = function(A, B: Int64): Int64;

function JitSelfTest(out Msg: string): Boolean;
var E: TX86Emitter; Mem: TExecMem; Fn: TAddFn; r1, r2, r3: Int64;
begin
  Result := False; Msg := '';
  E := TX86Emitter.Create;
  try
    {$IFDEF WINDOWS}
    E.EmitBytes([$48, $89, $C8, $48, $01, $D0, $C3]);   // mov rax,rcx; add rax,rdx; ret
    {$ELSE}
    E.EmitBytes([$48, $89, $F8, $48, $01, $F0, $C3]);   // mov rax,rdi; add rax,rsi; ret
    {$ENDIF}
    Mem := TExecMem.Create(E);
    try
      if Mem.Ptr = nil then begin Msg := 'exec alloc failed'; Exit; end;
      Fn := TAddFn(Mem.Ptr);
      r1 := Fn(3, 4); r2 := Fn(-10, 100); r3 := Fn(1000000000000, 2000000000000);
      if (r1 = 7) and (r2 = 90) and (r3 = 3000000000000) then
      begin Msg := Format('native add() OK: 3+4=%d, -10+100=%d, 1e12+2e12=%d', [r1, r2, r3]); Result := True; end
      else Msg := Format('native add() WRONG: %d,%d,%d', [r1, r2, r3]);
    finally Mem.Free; end;
  finally E.Free; end;
end;

end.
