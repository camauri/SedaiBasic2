unit SedaiX86Emitter;

{ ============================================================================
  SedaiX86Emitter - x86-64 machine-code emission primitives + executable memory.

  Extracted verbatim from SedaiJit so the loop JIT and the AOT backend (plan B,
  job/docs/PIANO_B1_AOT_DESIGN.md) share one encoder. Behavior-neutral: byte
  emission, back-patching, and the RWX allocation policy are exactly the ones
  the JIT has always used.

  TX86Emitter is a growable byte buffer with little-endian emit helpers and a
  universal [base+disp32] memory-operand encoder (ModRM mod=10). TExecMem copies
  a finished buffer into executable memory (PAGE_EXECUTE_READWRITE / PROT_EXEC;
  no W^X phase - an explicit, documented choice for x86-64 self-contained code).
  ============================================================================ }

{$mode objfpc}{$H+}

interface

uses
  SysUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF UNIX}, BaseUnix{$ENDIF};

// x86-64 register numbers (hardware encoding; REX.R/B extend to r8-r15)
const
  RAX = 0; RCX = 1; RDX = 2; RBX = 3; RSP = 4; RBP = 5; RSI = 6; RDI = 7; R8 = 8;
  R9 = 9; R10 = 10; R11 = 11; R12 = 12; R13 = 13; R14 = 14; R15 = 15;
  XMM0 = 0; XMM1 = 1;

{ ---- ABI register classes (C2) -------------------------------------------------------
  Which registers survive a call is the ONLY thing that differs between the two ABIs we
  target, and it differs a lot: Win64 keeps xmm6-xmm15 across a call, System V keeps NO
  xmm register at all. Anything that emits a call (the runtime-helper lowering that gives
  the AOT full program coverage) must ask these, never hard-code a register list -- and
  must not be "fixed" by allocating only callee-saved registers, which would give Linux
  systematically worse codegen than Windows.
  GPR shorthand: 1 shl <reg number>. -------------------------------------------------- }
const
  // Callee-saved (survive a call). Win64: rbx rbp rdi rsi r12-r15. SysV: rbx rbp r12-r15
  // (rdi/rsi are argument registers there, hence volatile).
  {$IFDEF WINDOWS}
  ABI_GPR_CALLEE_SAVED = (1 shl RBX) or (1 shl RBP) or (1 shl RDI) or (1 shl RSI) or
                         (1 shl R12) or (1 shl R13) or (1 shl R14) or (1 shl R15);
  ABI_XMM_FIRST_CALLEE_SAVED = 6;    // xmm6..xmm15 survive a call
  ABI_SHADOW_SPACE = 32;             // bytes the caller must reserve for the callee
  {$ELSE}
  ABI_GPR_CALLEE_SAVED = (1 shl RBX) or (1 shl RBP) or
                         (1 shl R12) or (1 shl R13) or (1 shl R14) or (1 shl R15);
  ABI_XMM_FIRST_CALLEE_SAVED = 16;   // none: every xmm is caller-saved in System V
  ABI_SHADOW_SPACE = 0;
  {$ENDIF}
  // Integer argument registers, in order (arg0, arg1, ...).
  {$IFDEF WINDOWS}
  ABI_ARG0 = RCX; ABI_ARG1 = RDX; ABI_ARG2 = R8; ABI_ARG3 = R9;
  {$ELSE}
  ABI_ARG0 = RDI; ABI_ARG1 = RSI; ABI_ARG2 = RDX; ABI_ARG3 = RCX;
  {$ENDIF}

function GprIsCalleeSaved(Reg: Integer): Boolean; inline;
function XmmIsCalleeSaved(Reg: Integer): Boolean; inline;

type
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

implementation

{ ---------------- ABI register classes ---------------- }

function GprIsCalleeSaved(Reg: Integer): Boolean;
begin
  Result := (Reg >= 0) and (Reg <= 15) and ((ABI_GPR_CALLEE_SAVED shr Reg) and 1 <> 0);
end;

function XmmIsCalleeSaved(Reg: Integer): Boolean;
begin
  Result := (Reg >= ABI_XMM_FIRST_CALLEE_SAVED) and (Reg <= 15);
end;

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

end.
