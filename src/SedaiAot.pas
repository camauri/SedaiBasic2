unit SedaiAot;

{ ============================================================================
  SedaiAot - AOT backend on the SSA IR (plan B, job/docs/PIANO_B1_AOT_DESIGN.md).

  Stage S3 (this file, survey only): slice the flat SSA program into function
  regions and classify each against the B1 scalar op set, so we know - before
  writing any codegen - how many real functions the B1 subset covers and what
  the top bail reasons are. Codegen lands in S4.

  A "function region" is a contiguous run of basic blocks: the module body from
  block 0 ('_entry') up to the first 'PROC_' labeled block, then one region per
  'PROC_' label (the IR has no per-procedure structure; the label prefix is the
  only delimiter). Instruction ordinals are counted over EVERY instruction of
  EVERY block in program order - the exact walk TBytecodeCompiler.Compile uses
  to build the SSA->PC map, so ordinals index that map directly.

  Diagnostics: set AOT_DIAG=1 to print one line per region (NATIVE / BAIL with
  the culprit op) plus a summary; output goes to stderr so program output
  stays byte-comparable.
  ============================================================================ }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SedaiSSATypes, SedaiBytecodeTypes, SedaiX86Emitter;

type
  // Per-call context of a compiled function: base pointers of the ACTIVE execution
  // context's transfer banks. Built by the interpreter once per Run and passed on
  // every call - never baked into the code (a worker thread must see its own banks).
  TAotCtx = record
    XferInt: PInt64;     // offset 0
    XferFloat: PInt64;   // offset 8
  end;
  PAotCtx = ^TAotCtx;

  // A compiled function: same bank pointers as the loop JIT plus the AOT context in
  // the third argument register. Returns the bytecode PC where the interpreter
  // resumes (the function's bcReturnSub / bcEnd, or a deopt PC).
  TNativeFuncFn = function(IntRegs, FloatRegs: PInt64; Ctx: PAotCtx): PtrInt;

  TAotFuncEntry = record
    EntryPC: Integer;
    Mem: TExecMem;       // ownership passes to the caller (the VM frees it)
  end;
  TAotFuncs = array of TAotFuncEntry;

type
  TAotRegion = record
    Name: string;                    // 'MAIN' or the procedure name (PROC_ suffix)
    FirstBlock, LastBlock: Integer;  // inclusive block-index range in SSAProg.Blocks
    FirstOrdinal: Integer;           // ordinal of the region's first SSA instruction
    InstrCount: Integer;             // SSA instructions in the region
    EntryPC: Integer;                // final bytecode PC of the first emitted instruction (-1 = none)
    Eligible: Boolean;               // every op is in the B1 scalar set and all jumps stay inside
    BailReason: string;              // first offender ('' when eligible)
  end;
  TAotRegions = array of TAotRegion;

// Slice into regions and classify against the B1 scalar set. Prog supplies the
// SSA->PC map (entry PCs and the cross-check that the map lines up with ProcMap).
function AotSliceAndClassify(SSAProg: TSSAProgram; Prog: TBytecodeProgram): TAotRegions;

// AOT_DIAG=1 printout: per-region verdict + summary + map cross-check warnings.
procedure AotSurvey(SSAProg: TSSAProgram; Prog: TBytecodeProgram);

// Compile every eligible region to native code (B1a: static frequency register
// assignment, deopt only for trapping ops). TrueVal is the VM's TRUE (-1);
// the dialect comes from Prog.ModernMode. Diag prints per-region compile results.
function AotCompileProgram(SSAProg: TSSAProgram; Prog: TBytecodeProgram;
                           TrueVal: Int64; Diag: Boolean): TAotFuncs;

implementation

uses TypInfo;

// The B1 op set: scalar int/float compute + control flow + the Xfer scalar forms
// (parameter prologue / result epilogue) + frame record marks (no-ops in a function
// that owns no records, which the classifier guarantees by excluding record ops).
function IsB1Op(Op: TSSAOpCode): Boolean;
begin
  case Op of
    ssaLoadConstInt, ssaLoadConstFloat,
    ssaCopyInt, ssaCopyFloat,
    ssaAddInt, ssaSubInt, ssaMulInt, ssaDivInt, ssaModInt, ssaNegInt,
    ssaDivUInt, ssaModUInt,
    ssaAddFloat, ssaSubFloat, ssaMulFloat, ssaDivFloat, ssaNegFloat,
    ssaIntToFloat, ssaFloatToInt, ssaFloatRound, ssaNarrowInt, ssaNarrowSingle,
    ssaCmpEqInt, ssaCmpNeInt, ssaCmpLtInt, ssaCmpGtInt, ssaCmpLeInt, ssaCmpGeInt,
    ssaCmpLtUInt, ssaCmpGtUInt, ssaCmpLeUInt, ssaCmpGeUInt,
    ssaCmpEqFloat, ssaCmpNeFloat, ssaCmpLtFloat, ssaCmpGtFloat, ssaCmpLeFloat, ssaCmpGeFloat,
    ssaBitwiseAnd, ssaBitwiseOr, ssaBitwiseXor, ssaBitwiseNot,
    ssaShl, ssaShr, ssaShrUInt,
    ssaMathSqr,
    ssaLabel, ssaNop, ssaJump, ssaJumpIfZero, ssaJumpIfNotZero,
    ssaXferLoadInt, ssaXferLoadFloat, ssaXferStoreInt, ssaXferStoreFloat,
    ssaReturnSub, ssaEnd, ssaStop,
    ssaRecMarkPush, ssaRecMarkPop:
      Result := True;
  else
    Result := False;
  end;
end;

function OpName(Op: TSSAOpCode): string;
begin
  Result := GetEnumName(TypeInfo(TSSAOpCode), Ord(Op));
end;

function AotSliceAndClassify(SSAProg: TSSAProgram; Prog: TBytecodeProgram): TAotRegions;
var
  Regions: TAotRegions;
  NRegions: Integer;
  BlockOrdinal: array of Integer;   // block index -> ordinal of its first instruction
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  RegionLabels: TStringList;        // labels of the current region's blocks (jump containment)
  i, j, Ordinal, r, o: Integer;

  procedure StartRegion(const AName: string; ABlock, AOrdinal: Integer);
  begin
    if NRegions >= Length(Regions) then SetLength(Regions, NRegions * 2 + 8);
    with Regions[NRegions] do
    begin
      Name := AName;
      FirstBlock := ABlock;
      LastBlock := ABlock;
      FirstOrdinal := AOrdinal;
      InstrCount := 0;
      EntryPC := -1;
      Eligible := True;
      BailReason := '';
    end;
    Inc(NRegions);
  end;

begin
  Result := nil;
  if (SSAProg = nil) or (SSAProg.Blocks.Count = 0) then Exit;
  SetLength(Regions, 8);
  NRegions := 0;

  // Pass 0: block -> ordinal of first instruction (ordinals count every instruction
  // of every block in program order, matching the bytecode compiler's emission walk).
  SetLength(BlockOrdinal, SSAProg.Blocks.Count);
  Ordinal := 0;
  for i := 0 to SSAProg.Blocks.Count - 1 do
  begin
    BlockOrdinal[i] := Ordinal;
    Inc(Ordinal, SSAProg.Blocks[i].Instructions.Count);
  end;

  // Pass 1: slice at PROC_ labels.
  StartRegion('MAIN', 0, 0);
  for i := 0 to SSAProg.Blocks.Count - 1 do
  begin
    Block := SSAProg.Blocks[i];
    if (i > 0) and (Copy(Block.LabelName, 1, 5) = 'PROC_') then
      StartRegion(Copy(Block.LabelName, 6, MaxInt), i, BlockOrdinal[i]);
    Regions[NRegions - 1].LastBlock := i;
  end;
  SetLength(Regions, NRegions);

  // Pass 2: classify each region.
  RegionLabels := TStringList.Create;
  try
    RegionLabels.Sorted := True;
    RegionLabels.Duplicates := dupIgnore;
    for r := 0 to NRegions - 1 do
    begin
      RegionLabels.Clear;
      for i := Regions[r].FirstBlock to Regions[r].LastBlock do
        if SSAProg.Blocks[i].LabelName <> '' then
          RegionLabels.Add(SSAProg.Blocks[i].LabelName);

      o := Regions[r].FirstOrdinal;
      for i := Regions[r].FirstBlock to Regions[r].LastBlock do
      begin
        Block := SSAProg.Blocks[i];
        for j := 0 to Block.Instructions.Count - 1 do
        begin
          Instr := Block.Instructions[j];
          Inc(Regions[r].InstrCount);
          // Entry PC = first instruction of the region that emitted bytecode.
          if (Regions[r].EntryPC < 0) and (Prog <> nil) then
            Regions[r].EntryPC := Prog.GetSsaPc(o);
          if Regions[r].Eligible then
          begin
            if not IsB1Op(Instr.OpCode) then
            begin
              Regions[r].Eligible := False;
              Regions[r].BailReason := OpName(Instr.OpCode);
            end
            // A jump leaving the region means the region is not a self-contained
            // function (interleaved code, computed flow): not compilable as a unit.
            else if ((Instr.OpCode = ssaJump) or (Instr.OpCode = ssaJumpIfZero) or
                     (Instr.OpCode = ssaJumpIfNotZero)) and
                    (Instr.Dest.Kind = svkLabel) and
                    (RegionLabels.IndexOf(Instr.Dest.LabelName) < 0) then
            begin
              Regions[r].Eligible := False;
              Regions[r].BailReason := 'jump-out:' + Instr.Dest.LabelName;
            end;
          end;
          Inc(o);
        end;
      end;
    end;
  finally
    RegionLabels.Free;
  end;
  Result := Regions;
end;

procedure AotSurvey(SSAProg: TSSAProgram; Prog: TBytecodeProgram);
var
  Regions: TAotRegions;
  r, NElig: Integer;
  ProcAtEntry: string;
begin
  Regions := AotSliceAndClassify(SSAProg, Prog);
  NElig := 0;
  for r := 0 to High(Regions) do
  begin
    with Regions[r] do
    begin
      if Eligible then
      begin
        Inc(NElig);
        WriteLn(ErrOutput, Format('[AOT] %-24s blocks=%-4d instrs=%-5d entryPC=%-6d NATIVE',
                                  [Name, LastBlock - FirstBlock + 1, InstrCount, EntryPC]));
      end
      else
        WriteLn(ErrOutput, Format('[AOT] %-24s blocks=%-4d instrs=%-5d entryPC=%-6d BAIL %s',
                                  [Name, LastBlock - FirstBlock + 1, InstrCount, EntryPC, BailReason]));
      // Cross-check the S2 plumbing: the proc map must agree on who owns the entry PC.
      if (Prog <> nil) and (EntryPC >= 0) and (Name <> 'MAIN') then
      begin
        ProcAtEntry := Prog.GetProcNameAt(EntryPC);
        if ProcAtEntry <> Name then
          WriteLn(ErrOutput, Format('[AOT] WARNING: SSA->PC map says %s starts at PC %d but ProcMap owner there is "%s"',
                                    [Name, EntryPC, ProcAtEntry]));
      end;
    end;
  end;
  WriteLn(ErrOutput, Format('[AOT] survey: %d/%d regions eligible (B1 scalar set)',
                            [NElig, Length(Regions)]));
end;

{ ============================ B1a code generation ============================ }

// Compile one eligible region to native x86-64. The encodings mirror the loop
// JIT's validated forms (SedaiJit); operands come from post-regalloc SSA values
// whose bank indexes are composed through the register-compaction remap, so the
// code reads/writes exactly the registers the interpreter uses. Returns nil on
// any condition the B1a subset cannot honor (the region stays interpreted).
function AotCompileRegion(SSAProg: TSSAProgram; Prog: TBytecodeProgram;
                          const Region: TAotRegion; TrueVal: Int64;
                          Modern: Boolean; out BailWhy: string): TExecMem;
const
  IntPool: array[0..6] of Integer = (R9, R10, R11, R12, R13, R14, R15);
type
  TFix = record PatchOff, TargetBlock: Integer; end;  // TargetBlock -1 = epilogue
var
  E: TX86Emitter;
  ILoc, FLoc: array of Integer;         // final VM reg -> native reg (or -1)
  IUse, FUse: array of Integer;         // usage counts
  MaxIReg, MaxFReg: Integer;
  IAllocd, FAllocd: array of Integer;   // allocated VM regs in pool order
  NIAlloc, NFAlloc: Integer;
  SaveGpr: array[R12..R15] of Boolean;
  SaveX6, SaveX7: Boolean;
  BlockOff: array of Integer;           // block index -> native offset
  Fixups: array of TFix;
  NFix: Integer;
  EpiOff: Integer;
  OK: Boolean;
  HasRecMark, HasDeopt: Boolean;
  Cur: TSSAInstruction;
  CurOrd: Integer;                      // ordinal of Cur (indexes the SSA->PC map)
  LabelIdx: TStringList;                // region-local label -> block-list index

  procedure Fail(const Why: string);
  begin
    if OK then BailWhy := Why;
    OK := False;
  end;

  // Mapped bytecode PC of the current instruction; Fail when the map has none.
  function NeedPC: Integer;
  begin
    Result := Prog.GetSsaPc(CurOrd);
    if Result < 0 then Fail('no-pc:' + OpName(Cur.OpCode));
  end;

  // Final interpreter bank index of a register operand (bail on shape surprises).
  function IReg(const V: TSSAValue): Integer;
  begin
    Result := 0;
    if (V.Kind <> svkRegister) or (V.RegType <> srtInt) then
      Fail('operand:' + OpName(Cur.OpCode))
    else begin
      Result := Prog.AotRemapIntReg(V.RegIndex);
      if Result < 0 then Fail('unmapped-reg:' + OpName(Cur.OpCode));
    end;
  end;
  function FReg(const V: TSSAValue): Integer;
  begin
    Result := 0;
    if (V.Kind <> svkRegister) or (V.RegType <> srtFloat) then
      Fail('operand:' + OpName(Cur.OpCode))
    else begin
      Result := Prog.AotRemapFloatReg(V.RegIndex);
      if Result < 0 then Fail('unmapped-reg:' + OpName(Cur.OpCode));
    end;
  end;
  function CInt(const V: TSSAValue): Int64;
  begin
    Result := 0;
    if V.Kind <> svkConstInt then Fail('const-operand:' + OpName(Cur.OpCode))
    else Result := V.ConstInt;
  end;

  { --- emission helpers (mirrors of the loop JIT's validated encoders) --- }

  procedure AddFixup(AOff, ABlock: Integer);
  begin
    if NFix >= Length(Fixups) then SetLength(Fixups, NFix * 2 + 8);
    Fixups[NFix].PatchOff := AOff;
    Fixups[NFix].TargetBlock := ABlock;
    Inc(NFix);
  end;
  procedure JmpRel(TargetBlock: Integer);
  begin
    E.Emit8($E9); AddFixup(E.Len, TargetBlock); E.Emit32(0);
  end;
  procedure JccRel(CC: Byte; TargetBlock: Integer);
  begin
    E.Emit8($0F); E.Emit8(CC); AddFixup(E.Len, TargetBlock); E.Emit32(0);
  end;
  // Exit to the interpreter at absolute bytecode PC (deopt and normal exits alike):
  // the epilogue flushes the allocated registers, so the interpreter resumes coherent.
  procedure ExitTo(apc: Integer);
  begin
    E.EmitBytes([$B8]); E.Emit32(LongWord(apc));   // mov eax, apc
    JmpRel(-1);                                     // jmp epilogue
  end;

  procedure EmitRR(const Op: array of Byte; regField, rmReg: Integer);
  var rex: Byte; k: Integer;
  begin
    rex := $48;
    if regField >= 8 then rex := rex or $04;
    if rmReg    >= 8 then rex := rex or $01;
    E.Emit8(rex);
    for k := 0 to High(Op) do E.Emit8(Op[k]);
    E.Emit8($C0 or ((regField and 7) shl 3) or (rmReg and 7));
  end;
  procedure MovRR(dst, src: Integer);
  begin EmitRR([$89], src, dst); end;
  procedure MovImm64(natreg: Integer; imm: Int64);
  var rex: Byte;
  begin
    rex := $48; if natreg >= 8 then rex := rex or $01;
    E.Emit8(rex); E.Emit8($B8 or (natreg and 7)); E.Emit64(QWord(imm));
  end;
  procedure LoadRegMem(natreg: Integer; disp: LongWord);   // mov natreg,[rbx+disp]
  var rex: Byte;
  begin
    rex := $48; if natreg >= 8 then rex := rex or $04;
    E.Emit8(rex); E.Emit8($8B);
    E.Emit8($80 or ((natreg and 7) shl 3) or RBX); E.Emit32(disp);
  end;
  procedure StoreRegMem(natreg: Integer; disp: LongWord);  // mov [rbx+disp],natreg
  var rex: Byte;
  begin
    rex := $48; if natreg >= 8 then rex := rex or $04;
    E.Emit8(rex); E.Emit8($89);
    E.Emit8($80 or ((natreg and 7) shl 3) or RBX); E.Emit32(disp);
  end;
  function IAlloc(vmreg: Integer): Integer;
  begin
    if vmreg <= MaxIReg then Result := ILoc[vmreg] else Result := -1;
  end;
  procedure ILoad(scr, vmreg: Integer);       // scratch (rax/rcx/rdx) := VM int reg
  var n: Integer;
  begin
    n := IAlloc(vmreg);
    if n >= 0 then MovRR(scr, n)
    else E.MemOp([$48, $8B], scr, RBX, LongWord(vmreg) * 8);
  end;
  procedure IStore(vmreg, scr: Integer);
  var n: Integer;
  begin
    n := IAlloc(vmreg);
    if n >= 0 then MovRR(n, scr)
    else E.MemOp([$48, $89], scr, RBX, LongWord(vmreg) * 8);
  end;
  procedure IOp(const MemForm: array of Byte; scr, vmreg: Integer);
  var rest: array of Byte; k, n: Integer;
  begin
    n := IAlloc(vmreg);
    if n >= 0 then
    begin
      SetLength(rest, Length(MemForm) - 1);   // drop the $48 REX; EmitRR rebuilds it
      for k := 1 to High(MemForm) do rest[k - 1] := MemForm[k];
      EmitRR(rest, scr, n);
    end
    else
      E.MemOp(MemForm, scr, RBX, LongWord(vmreg) * 8);
  end;
  function FAlloc(vmreg: Integer): Integer;
  begin
    if vmreg <= MaxFReg then Result := FLoc[vmreg] else Result := -1;
  end;
  procedure FLoad(Wx, vmreg: Integer);
  var n: Integer;
  begin
    n := FAlloc(vmreg);
    if n >= 0 then
    begin
      if n <> Wx then E.EmitBytes([$F2, $0F, $10, $C0 or (Wx shl 3) or n]);
    end
    else E.MemOp([$F2, $0F, $10], Wx, RSI, LongWord(vmreg) * 8);
  end;
  procedure FOp(const SseOp: array of Byte; Wx, vmreg: Integer);
  var n: Integer;
  begin
    n := FAlloc(vmreg);
    if n >= 0 then
      E.EmitBytes([SseOp[0], SseOp[1], SseOp[2], $C0 or (Wx shl 3) or n])
    else
      E.MemOp(SseOp, Wx, RSI, LongWord(vmreg) * 8);
  end;
  procedure FStore(vmreg, Wx: Integer);
  var n: Integer;
  begin
    n := FAlloc(vmreg);
    if n >= 0 then
    begin
      if n <> Wx then E.EmitBytes([$F2, $0F, $10, $C0 or (n shl 3) or Wx]);
    end
    else E.MemOp([$F2, $0F, $11], Wx, RSI, LongWord(vmreg) * 8);
  end;

  // al holds 0/1 -> dest := TrueVal/0 (dest = current instruction's Dest int reg).
  procedure CmpBoolToDest;
  begin
    E.EmitBytes([$0F, $B6, $C0]);                       // movzx eax,al
    if TrueVal = -1 then
      E.EmitBytes([$48, $F7, $D8])                      // neg rax
    else if TrueVal <> 1 then
    begin E.EmitBytes([$48, $69, $C0]); E.Emit32(LongWord(TrueVal and $FFFFFFFF)); end;
    IStore(IReg(Cur.Dest), RAX);
  end;
  procedure IntCmp(SetCC: Byte);
  begin
    ILoad(RAX, IReg(Cur.Src1));
    IOp([$48, $3B], RAX, IReg(Cur.Src2));               // cmp rax, src2
    E.EmitBytes([$0F, SetCC, $C0]);                     // setcc al
    CmpBoolToDest;
  end;
  // Kind: 0=Lt 1=Le 2=Gt 3=Ge 4=Eq 5=Ne (ordered IEEE, NaN-correct - JIT J8 pattern).
  procedure FloatCmp(Kind: Integer);
  begin
    FLoad(XMM0, FReg(Cur.Src1));
    FLoad(XMM1, FReg(Cur.Src2));
    case Kind of
      0: begin E.EmitBytes([$66, $0F, $2E, $C8]); E.EmitBytes([$0F, $97, $C0]); end;
      1: begin E.EmitBytes([$66, $0F, $2E, $C8]); E.EmitBytes([$0F, $93, $C0]); end;
      2: begin E.EmitBytes([$66, $0F, $2E, $C1]); E.EmitBytes([$0F, $97, $C0]); end;
      3: begin E.EmitBytes([$66, $0F, $2E, $C1]); E.EmitBytes([$0F, $93, $C0]); end;
      4: begin E.EmitBytes([$66, $0F, $2E, $C1]);
               E.EmitBytes([$0F, $94, $C0]); E.EmitBytes([$0F, $9B, $C1]);
               E.EmitBytes([$20, $C8]); end;
      5: begin E.EmitBytes([$66, $0F, $2E, $C1]);
               E.EmitBytes([$0F, $95, $C0]); E.EmitBytes([$0F, $9A, $C1]);
               E.EmitBytes([$08, $C8]); end;
    end;
    CmpBoolToDest;
  end;
  procedure FloatBin(const SseOp: array of Byte);
  begin
    FLoad(XMM0, FReg(Cur.Src1));
    FOp(SseOp, XMM0, FReg(Cur.Src2));
    FStore(FReg(Cur.Dest), XMM0);
  end;
  // Signed div/mod with the interpreter's raise semantics via deopt (JIT J10 pattern).
  procedure DivModSigned(apc: Integer; WantRemainder: Boolean);
  var p1, p2: Integer;
  begin
    ILoad(RAX, IReg(Cur.Src1));
    ILoad(RCX, IReg(Cur.Src2));
    E.EmitBytes([$48, $85, $C9]);                 // test rcx,rcx
    E.EmitBytes([$75, $00]); p1 := E.Len - 1;     // jnz +skip
    ExitTo(apc);                                   // /0 -> interpreter raises
    E.PatchByte(p1, Byte(E.Len - (p1 + 1)));
    // INT64_MIN / -1 faults in idiv; the interpreter's FPC result needs a deopt too.
    E.EmitBytes([$48, $83, $F9, $FF]);            // cmp rcx,-1
    E.EmitBytes([$75, $00]); p1 := E.Len - 1;     // jnz +ok
    MovImm64(RDX, Int64($8000000000000000));
    E.EmitBytes([$48, $39, $D0]);                 // cmp rax,rdx
    E.EmitBytes([$75, $00]); p2 := E.Len - 1;     // jnz +ok
    ExitTo(apc);
    E.PatchByte(p2, Byte(E.Len - (p2 + 1)));
    E.PatchByte(p1, Byte(E.Len - (p1 + 1)));
    E.EmitBytes([$48, $99]);                      // cqo
    E.EmitBytes([$48, $F7, $F9]);                 // idiv rcx
    if WantRemainder then IStore(IReg(Cur.Dest), RDX)
    else IStore(IReg(Cur.Dest), RAX);
  end;
  procedure DivModUnsigned(apc: Integer; WantRemainder: Boolean);
  var p1: Integer;
  begin
    ILoad(RAX, IReg(Cur.Src1));
    ILoad(RCX, IReg(Cur.Src2));
    E.EmitBytes([$48, $85, $C9]);                 // test rcx,rcx
    E.EmitBytes([$75, $00]); p1 := E.Len - 1;     // jnz +skip
    ExitTo(apc);
    E.PatchByte(p1, Byte(E.Len - (p1 + 1)));
    E.EmitBytes([$31, $D2]);                      // xor edx,edx
    E.EmitBytes([$48, $F7, $F1]);                 // div rcx
    if WantRemainder then IStore(IReg(Cur.Dest), RDX)
    else IStore(IReg(Cur.Dest), RAX);
  end;
  // ArithShr64 / LogicalShr64 saturating semantics (NOT the hardware masked shift).
  procedure ShrSat(Arith: Boolean);
  var pKeep, pDo, pDone: Integer;
  begin
    ILoad(RAX, IReg(Cur.Src1));
    ILoad(RCX, IReg(Cur.Src2));
    E.EmitBytes([$48, $85, $C9]);                 // test rcx,rcx
    E.EmitBytes([$7E, $00]); pKeep := E.Len - 1;  // jle @done (count<=0 -> value)
    E.EmitBytes([$48, $83, $F9, $40]);            // cmp rcx,64
    E.EmitBytes([$7C, $00]); pDo := E.Len - 1;    // jl @shift
    if Arith then
      E.EmitBytes([$48, $C1, $F8, $3F])           // sar rax,63 (saturate to sign)
    else
      E.EmitBytes([$31, $C0]);                    // xor eax,eax (saturate to 0)
    E.EmitBytes([$EB, $00]); pDone := E.Len - 1;  // jmp @done
    E.PatchByte(pDo, Byte(E.Len - (pDo + 1)));
    if Arith then
      E.EmitBytes([$48, $D3, $F8])                // sar rax,cl
    else
      E.EmitBytes([$48, $D3, $E8]);               // shr rax,cl
    E.PatchByte(pDone, Byte(E.Len - (pDone + 1)));
    E.PatchByte(pKeep, Byte(E.Len - (pKeep + 1)));
    IStore(IReg(Cur.Dest), RAX);
  end;
  // Load the Xfer bank base (slot 0 = XferInt, 1 = XferFloat) from the AOT ctx (r8) into rdx.
  procedure LoadXferBase(FloatBank: Boolean);
  begin
    if FloatBank then E.MemOp([$49, $8B], RDX, R8, 8)
    else E.MemOp([$49, $8B], RDX, R8, 0);
  end;

  { --- prescan: usage counts, deopt needs, structural checks --- }
  procedure Prescan;
  var
    b, j, o: Integer;
    Blk: TSSABasicBlock;
    Ins: TSSAInstruction;
    procedure CountVal(const V: TSSAValue);
    var r: Integer;
    begin
      if V.Kind <> svkRegister then Exit;
      case V.RegType of
        srtInt: begin
          r := Prog.AotRemapIntReg(V.RegIndex);
          if r < 0 then begin Fail('unmapped-reg'); Exit; end;
          if r > MaxIReg then MaxIReg := r;
          if r >= Length(IUse) then SetLength(IUse, r * 2 + 16);
          Inc(IUse[r]);
        end;
        srtFloat: begin
          r := Prog.AotRemapFloatReg(V.RegIndex);
          if r < 0 then begin Fail('unmapped-reg'); Exit; end;
          if r > MaxFReg then MaxFReg := r;
          if r >= Length(FUse) then SetLength(FUse, r * 2 + 16);
          Inc(FUse[r]);
        end;
        else Fail('string-operand');
      end;
    end;
  begin
    o := Region.FirstOrdinal;
    for b := Region.FirstBlock to Region.LastBlock do
    begin
      Blk := SSAProg.Blocks[b];
      for j := 0 to Blk.Instructions.Count - 1 do
      begin
        Ins := Blk.Instructions[j];
        case Ins.OpCode of
          ssaLabel, ssaNop: ;
          ssaRecMarkPush, ssaRecMarkPop: HasRecMark := True;
          ssaJump: ;
          ssaJumpIfZero, ssaJumpIfNotZero: CountVal(Ins.Src1);
          ssaReturnSub, ssaEnd, ssaStop:
            if Prog.GetSsaPc(o) < 0 then Fail('no-pc-exit');
          ssaDivInt, ssaModInt, ssaDivUInt, ssaModUInt:
          begin
            HasDeopt := True;
            if Prog.GetSsaPc(o) < 0 then Fail('no-pc-trap');
            CountVal(Ins.Dest); CountVal(Ins.Src1); CountVal(Ins.Src2);
          end;
          ssaDivFloat, ssaMathSqr:
          begin
            if not Modern then
            begin
              HasDeopt := True;
              if Prog.GetSsaPc(o) < 0 then Fail('no-pc-trap');
            end;
            CountVal(Ins.Dest); CountVal(Ins.Src1);
            if Ins.OpCode = ssaDivFloat then CountVal(Ins.Src2);
          end;
          ssaXferLoadInt, ssaXferLoadFloat: CountVal(Ins.Dest);
          ssaXferStoreInt, ssaXferStoreFloat: CountVal(Ins.Src1);
          ssaNarrowInt: begin CountVal(Ins.Dest); CountVal(Ins.Src1); end;
        else
          begin
            CountVal(Ins.Dest);
            if Ins.Src1.Kind = svkRegister then CountVal(Ins.Src1);
            if Ins.Src2.Kind = svkRegister then CountVal(Ins.Src2);
            if Ins.Src3.Kind = svkRegister then CountVal(Ins.Src3);
          end;
        end;
        if not OK then Exit;
        Inc(o);
      end;
    end;
    // A mid-function deopt hands the REST of the invocation to the interpreter; skipped
    // RecMark pushes would then unbalance the record-mark stack -> not compilable.
    if HasRecMark and HasDeopt then Fail('recmark+deopt');
    // The region's last instruction must leave natively (no fall-through off the end).
    Blk := SSAProg.Blocks[Region.LastBlock];
    if Blk.Instructions.Count = 0 then Fail('empty-last-block')
    else
    begin
      // (Explicit comparisons: TSSAOpCode has >256 values, so a set constructor is illegal.)
      Ins := Blk.Instructions[Blk.Instructions.Count - 1];
      if not ((Ins.OpCode = ssaReturnSub) or (Ins.OpCode = ssaEnd) or
              (Ins.OpCode = ssaStop) or (Ins.OpCode = ssaJump)) then
        Fail('open-region-end');
    end;
  end;

  procedure Allocate;
  var r, k, best, bestUse: Integer;
      Taken: array of Boolean;
  begin
    // Ints: most-used first onto r9..r15 (greedy selection sort over use counts).
    SetLength(Taken, MaxIReg + 1);
    for k := 0 to High(IntPool) do
    begin
      best := -1; bestUse := 0;
      for r := 0 to MaxIReg do
        if (not Taken[r]) and (IUse[r] > bestUse) then begin best := r; bestUse := IUse[r]; end;
      if best < 0 then Break;
      Taken[best] := True;
      ILoc[best] := IntPool[k];
      IAllocd[NIAlloc] := best; Inc(NIAlloc);
      if IntPool[k] >= R12 then SaveGpr[IntPool[k]] := True;
    end;
    // Floats: most-used first onto xmm2..xmm7.
    SetLength(Taken, 0); SetLength(Taken, MaxFReg + 1);
    for k := 2 to 7 do
    begin
      best := -1; bestUse := 0;
      for r := 0 to MaxFReg do
        if (not Taken[r]) and (FUse[r] > bestUse) then begin best := r; bestUse := FUse[r]; end;
      if best < 0 then Break;
      Taken[best] := True;
      FLoc[best] := k;
      FAllocd[NFAlloc] := best; Inc(NFAlloc);
      if k = 6 then SaveX6 := True;
      if k = 7 then SaveX7 := True;
    end;
  end;

  procedure EmitInstruction;
  var d, w: Integer;
      apc: Integer;
      p1: Integer;
      bits: Int64;
  begin
    case Cur.OpCode of
      ssaLabel, ssaNop, ssaRecMarkPush, ssaRecMarkPop: ;

      ssaLoadConstInt:
      begin
        if Cur.Src1.Kind <> svkConstInt then begin Fail('const-shape'); Exit; end;
        d := IReg(Cur.Dest); if not OK then Exit;
        if IAlloc(d) >= 0 then MovImm64(IAlloc(d), Cur.Src1.ConstInt)
        else begin MovImm64(RAX, Cur.Src1.ConstInt); IStore(d, RAX); end;
      end;
      ssaLoadConstFloat:
      begin
        if Cur.Src1.Kind <> svkConstFloat then begin Fail('const-shape'); Exit; end;
        bits := PInt64(@Cur.Src1.ConstFloat)^;
        MovImm64(RAX, bits);
        E.EmitBytes([$66, $48, $0F, $6E, $C0]);        // movq xmm0, rax
        FStore(FReg(Cur.Dest), XMM0);
      end;

      ssaCopyInt: begin ILoad(RAX, IReg(Cur.Src1)); IStore(IReg(Cur.Dest), RAX); end;
      ssaCopyFloat: begin FLoad(XMM0, FReg(Cur.Src1)); FStore(FReg(Cur.Dest), XMM0); end;

      ssaAddInt: begin ILoad(RAX, IReg(Cur.Src1)); IOp([$48, $03], RAX, IReg(Cur.Src2)); IStore(IReg(Cur.Dest), RAX); end;
      ssaSubInt: begin ILoad(RAX, IReg(Cur.Src1)); IOp([$48, $2B], RAX, IReg(Cur.Src2)); IStore(IReg(Cur.Dest), RAX); end;
      ssaMulInt: begin ILoad(RAX, IReg(Cur.Src1)); IOp([$48, $0F, $AF], RAX, IReg(Cur.Src2)); IStore(IReg(Cur.Dest), RAX); end;
      ssaNegInt: begin ILoad(RAX, IReg(Cur.Src1)); E.EmitBytes([$48, $F7, $D8]); IStore(IReg(Cur.Dest), RAX); end;
      ssaDivInt:  begin apc := NeedPC; if OK then DivModSigned(apc, False); end;
      ssaModInt:  begin apc := NeedPC; if OK then DivModSigned(apc, True); end;
      ssaDivUInt: begin apc := NeedPC; if OK then DivModUnsigned(apc, False); end;
      ssaModUInt: begin apc := NeedPC; if OK then DivModUnsigned(apc, True); end;

      ssaAddFloat: FloatBin([$F2, $0F, $58]);
      ssaSubFloat: FloatBin([$F2, $0F, $5C]);
      ssaMulFloat: FloatBin([$F2, $0F, $59]);
      ssaDivFloat:
      begin
        if Modern then FloatBin([$F2, $0F, $5E])
        else
        begin
          // CLASSIC raises on a zero divisor: catch +-0.0 (bits shifted left of the sign
          // are 0) and deopt so the interpreter reproduces the raise. NaN goes native.
          apc := NeedPC; if not OK then Exit;
          FLoad(XMM0, FReg(Cur.Src1));
          FLoad(XMM1, FReg(Cur.Src2));
          E.EmitBytes([$66, $48, $0F, $7E, $C8]);      // movq rax, xmm1
          E.EmitBytes([$48, $D1, $E0]);                // shl rax,1 (drop sign)
          E.EmitBytes([$75, $00]); p1 := E.Len - 1;    // jnz +ok
          ExitTo(apc);
          E.PatchByte(p1, Byte(E.Len - (p1 + 1)));
          E.EmitBytes([$F2, $0F, $5E, $C1]);           // divsd xmm0, xmm1
          FStore(FReg(Cur.Dest), XMM0);
        end;
      end;
      ssaNegFloat:
      begin
        FLoad(XMM0, FReg(Cur.Src1));
        MovImm64(RAX, Int64($8000000000000000));
        E.EmitBytes([$66, $48, $0F, $6E, $C8]);        // movq xmm1, rax
        E.EmitBytes([$66, $0F, $57, $C1]);             // xorpd xmm0, xmm1
        FStore(FReg(Cur.Dest), XMM0);
      end;
      ssaMathSqr:
      begin
        FLoad(XMM0, FReg(Cur.Src1));
        if not Modern then
        begin
          // CLASSIC raises on Sqr(neg): sign bit set (incl. -0.0, where the interpreter
          // is also the safe path) -> deopt.
          apc := NeedPC; if not OK then Exit;
          E.EmitBytes([$66, $48, $0F, $7E, $C0]);      // movq rax, xmm0
          E.EmitBytes([$48, $85, $C0]);                // test rax, rax
          E.EmitBytes([$79, $00]); p1 := E.Len - 1;    // jns +ok
          ExitTo(apc);
          E.PatchByte(p1, Byte(E.Len - (p1 + 1)));
        end;
        E.EmitBytes([$F2, $0F, $51, $C0]);             // sqrtsd xmm0, xmm0
        FStore(FReg(Cur.Dest), XMM0);
      end;

      ssaIntToFloat:
      begin
        ILoad(RAX, IReg(Cur.Src1));
        E.EmitBytes([$F2, $48, $0F, $2A, $C0]);        // cvtsi2sd xmm0, rax
        FStore(FReg(Cur.Dest), XMM0);
      end;
      ssaFloatToInt:
      begin
        FLoad(XMM0, FReg(Cur.Src1));
        if Modern then E.EmitBytes([$F2, $48, $0F, $2D, $C0])   // cvtsd2si (round-to-even)
        else E.EmitBytes([$F2, $48, $0F, $2C, $C0]);            // cvttsd2si (truncate)
        IStore(IReg(Cur.Dest), RAX);
      end;
      ssaFloatRound:
      begin
        FLoad(XMM0, FReg(Cur.Src1));
        E.EmitBytes([$F2, $48, $0F, $2D, $C0]);        // cvtsd2si (CINT: round-to-even)
        IStore(IReg(Cur.Dest), RAX);
      end;
      ssaNarrowSingle:
      begin
        FLoad(XMM0, FReg(Cur.Src1));
        E.EmitBytes([$F2, $0F, $5A, $C0]);             // cvtsd2ss xmm0, xmm0
        E.EmitBytes([$F3, $0F, $5A, $C0]);             // cvtss2sd xmm0, xmm0
        FStore(FReg(Cur.Dest), XMM0);
      end;
      ssaNarrowInt:
      begin
        w := CInt(Cur.Src3); if not OK then Exit;
        ILoad(RAX, IReg(Cur.Src1));
        case w of
          1: E.EmitBytes([$48, $0F, $BE, $C0]);        // movsx rax, al
          2: E.EmitBytes([$0F, $B6, $C0]);             // movzx eax, al
          3: E.EmitBytes([$48, $0F, $BF, $C0]);        // movsx rax, ax
          4: E.EmitBytes([$0F, $B7, $C0]);             // movzx eax, ax
          5: E.EmitBytes([$48, $63, $C0]);             // movsxd rax, eax
          6: E.EmitBytes([$89, $C0]);                  // mov eax, eax (zero upper)
        end;                                           // else: full width, no-op
        IStore(IReg(Cur.Dest), RAX);
      end;

      ssaBitwiseAnd: begin ILoad(RAX, IReg(Cur.Src1)); IOp([$48, $23], RAX, IReg(Cur.Src2)); IStore(IReg(Cur.Dest), RAX); end;
      ssaBitwiseOr:  begin ILoad(RAX, IReg(Cur.Src1)); IOp([$48, $0B], RAX, IReg(Cur.Src2)); IStore(IReg(Cur.Dest), RAX); end;
      ssaBitwiseXor: begin ILoad(RAX, IReg(Cur.Src1)); IOp([$48, $33], RAX, IReg(Cur.Src2)); IStore(IReg(Cur.Dest), RAX); end;
      ssaBitwiseNot: begin ILoad(RAX, IReg(Cur.Src1)); E.EmitBytes([$48, $F7, $D0]); IStore(IReg(Cur.Dest), RAX); end;
      ssaShl:
      begin
        // FPC shl on x86-64 = hardware shl (count masked mod 64): native matches exactly.
        ILoad(RAX, IReg(Cur.Src1)); ILoad(RCX, IReg(Cur.Src2));
        E.EmitBytes([$48, $D3, $E0]);                  // shl rax, cl
        IStore(IReg(Cur.Dest), RAX);
      end;
      ssaShr: ShrSat({Arith=} Modern);   // MODERN arithmetic, CLASSIC logical (both saturating)
      ssaShrUInt: ShrSat(False);

      ssaCmpEqInt: IntCmp($94); ssaCmpNeInt: IntCmp($95);
      ssaCmpLtInt: IntCmp($9C); ssaCmpGtInt: IntCmp($9F);
      ssaCmpLeInt: IntCmp($9E); ssaCmpGeInt: IntCmp($9D);
      ssaCmpLtUInt: IntCmp($92); ssaCmpGtUInt: IntCmp($97);
      ssaCmpLeUInt: IntCmp($96); ssaCmpGeUInt: IntCmp($93);
      ssaCmpLtFloat: FloatCmp(0); ssaCmpLeFloat: FloatCmp(1);
      ssaCmpGtFloat: FloatCmp(2); ssaCmpGeFloat: FloatCmp(3);
      ssaCmpEqFloat: FloatCmp(4); ssaCmpNeFloat: FloatCmp(5);

      ssaJump, ssaJumpIfZero, ssaJumpIfNotZero:
      begin
        // Resolve the target through the region's OWN label->index map (built from the
        // actual list positions): TSSABasicBlock.BlockIndex is stamped at SSA construction
        // and goes stale when later passes (LICM pre-headers) insert blocks mid-list.
        if Cur.Dest.Kind <> svkLabel then begin Fail('jump-shape'); Exit; end;
        d := LabelIdx.IndexOf(Cur.Dest.LabelName);
        if d < 0 then begin Fail('jump-target'); Exit; end;
        d := PtrInt(LabelIdx.Objects[d]);
        if Cur.OpCode = ssaJump then JmpRel(d)
        else
        begin
          ILoad(RAX, IReg(Cur.Src1)); if not OK then Exit;
          E.EmitBytes([$48, $85, $C0]);                // test rax, rax
          if Cur.OpCode = ssaJumpIfZero then JccRel($84, d)
          else JccRel($85, d);
        end;
      end;

      ssaXferLoadInt:
      begin
        w := CInt(Cur.Src3); if not OK then Exit;
        LoadXferBase(False);
        E.MemOp([$48, $8B], RAX, RDX, LongWord(w) * 8);   // mov rax,[rdx+slot*8]
        IStore(IReg(Cur.Dest), RAX);
      end;
      ssaXferLoadFloat:
      begin
        w := CInt(Cur.Src3); if not OK then Exit;
        LoadXferBase(True);
        E.MemOp([$F2, $0F, $10], XMM0, RDX, LongWord(w) * 8);
        FStore(FReg(Cur.Dest), XMM0);
      end;
      ssaXferStoreInt:
      begin
        w := CInt(Cur.Src3); if not OK then Exit;
        LoadXferBase(False);
        ILoad(RAX, IReg(Cur.Src1));
        E.MemOp([$48, $89], RAX, RDX, LongWord(w) * 8);   // mov [rdx+slot*8],rax
      end;
      ssaXferStoreFloat:
      begin
        w := CInt(Cur.Src3); if not OK then Exit;
        LoadXferBase(True);
        FLoad(XMM0, FReg(Cur.Src1));
        E.MemOp([$F2, $0F, $11], XMM0, RDX, LongWord(w) * 8);
      end;

      ssaReturnSub, ssaEnd, ssaStop:
      begin
        apc := NeedPC; if not OK then Exit;
        ExitTo(apc);   // interpreter executes the bcReturnSub/bcEnd itself (FramePop etc.)
      end;
    else
      Fail('op:' + OpName(Cur.OpCode));
    end;
  end;

var
  b, j, k, TargetOff: Integer;
  Blk: TSSABasicBlock;
begin
  Result := nil;
  LabelIdx := nil;
  BailWhy := '';
  OK := True;
  HasRecMark := False; HasDeopt := False;
  MaxIReg := -1; MaxFReg := -1;
  SetLength(IUse, 16); SetLength(FUse, 16);
  NFix := 0; NIAlloc := 0; NFAlloc := 0;
  SaveX6 := False; SaveX7 := False;
  FillChar(SaveGpr, SizeOf(SaveGpr), 0);

  CurOrd := Region.FirstOrdinal;   // Prescan uses its own ordinal; keep for NeedPC in emission
  Prescan;
  if not OK then Exit;

  SetLength(ILoc, MaxIReg + 1); for k := 0 to MaxIReg do ILoc[k] := -1;
  SetLength(FLoc, MaxFReg + 1); for k := 0 to MaxFReg do FLoc[k] := -1;
  SetLength(IAllocd, Length(IntPool)); SetLength(FAllocd, 6);
  Allocate;

  // Region-local label -> block-list index (see the jump case for why not BlockIndex).
  LabelIdx := TStringList.Create;
  LabelIdx.Sorted := True;
  LabelIdx.Duplicates := dupIgnore;
  for k := Region.FirstBlock to Region.LastBlock do
    if SSAProg.Blocks[k].LabelName <> '' then
      LabelIdx.AddObject(SSAProg.Blocks[k].LabelName, TObject(PtrInt(k)));

  E := TX86Emitter.Create;
  try
    SetLength(BlockOff, SSAProg.Blocks.Count);
    for k := 0 to High(BlockOff) do BlockOff[k] := -1;

    // Prologue (leaf function; Win64: rcx=IntRegs rdx=FloatRegs r8=AotCtx; SysV: rdi/rsi/rdx).
    E.Emit8($53);                                    // push rbx
    E.Emit8($56);                                    // push rsi
    for k := R12 to R15 do
      if SaveGpr[k] then begin E.Emit8($41); E.Emit8($54 + (k - R12)); end;
    {$IFDEF WINDOWS}
    E.EmitBytes([$48, $89, $CB]);                    // mov rbx, rcx
    E.EmitBytes([$48, $89, $D6]);                    // mov rsi, rdx
    {$ELSE}
    E.EmitBytes([$48, $89, $FB]);                    // mov rbx, rdi
    E.EmitBytes([$49, $89, $D0]);                    // mov r8, rdx
    {$ENDIF}
    if SaveX6 or SaveX7 then
    begin
      E.EmitBytes([$48, $83, $EC, $10]);             // sub rsp, 16
      if SaveX6 then E.EmitBytes([$F2, $0F, $11, $34, $24]);        // movsd [rsp], xmm6
      if SaveX7 then E.EmitBytes([$F2, $0F, $11, $7C, $24, $08]);   // movsd [rsp+8], xmm7
    end;
    // Entry loads of the allocated registers.
    for k := 0 to NIAlloc - 1 do
      LoadRegMem(ILoc[IAllocd[k]], LongWord(IAllocd[k]) * 8);
    for k := 0 to NFAlloc - 1 do
      E.MemOp([$F2, $0F, $10], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);

    // Body: blocks in order (fall-through preserved by contiguous emission).
    CurOrd := Region.FirstOrdinal;
    for b := Region.FirstBlock to Region.LastBlock do
    begin
      Blk := SSAProg.Blocks[b];
      BlockOff[b] := E.Len;
      for j := 0 to Blk.Instructions.Count - 1 do
      begin
        Cur := Blk.Instructions[j];
        EmitInstruction;
        if not OK then Exit;
        Inc(CurOrd);
      end;
    end;

    // Epilogue: rax already holds the exit PC; flush allocated regs and return.
    EpiOff := E.Len;
    for k := 0 to NIAlloc - 1 do
      StoreRegMem(ILoc[IAllocd[k]], LongWord(IAllocd[k]) * 8);
    for k := 0 to NFAlloc - 1 do
      E.MemOp([$F2, $0F, $11], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
    if SaveX6 or SaveX7 then
    begin
      if SaveX6 then E.EmitBytes([$F2, $0F, $10, $34, $24]);        // movsd xmm6, [rsp]
      if SaveX7 then E.EmitBytes([$F2, $0F, $10, $7C, $24, $08]);   // movsd xmm7, [rsp+8]
      E.EmitBytes([$48, $83, $C4, $10]);             // add rsp, 16
    end;
    for k := R15 downto R12 do
      if SaveGpr[k] then begin E.Emit8($41); E.Emit8($5C + (k - R12)); end;
    E.Emit8($5E);                                    // pop rsi
    E.Emit8($5B);                                    // pop rbx
    E.Emit8($C3);                                    // ret

    // Patch jump fixups (block targets or the epilogue).
    for k := 0 to NFix - 1 do
    begin
      if Fixups[k].TargetBlock = -1 then TargetOff := EpiOff
      else TargetOff := BlockOff[Fixups[k].TargetBlock];
      if TargetOff < 0 then begin Fail('fixup-target'); Exit; end;
      E.Patch32(Fixups[k].PatchOff, LongWord(TargetOff - (Fixups[k].PatchOff + 4)));
    end;

    Result := TExecMem.Create(E);
    if Result.Ptr = nil then begin FreeAndNil(Result); Fail('exec-alloc'); end;
  finally
    E.Free;
    LabelIdx.Free;
    if (Result = nil) and (BailWhy = '') then BailWhy := 'unknown';
  end;
end;

function AotCompileProgram(SSAProg: TSSAProgram; Prog: TBytecodeProgram;
                           TrueVal: Int64; Diag: Boolean): TAotFuncs;
var
  Regions: TAotRegions;
  r, n: Integer;
  Mem: TExecMem;
  Why: string;
begin
  Result := nil;
  n := 0;
  Regions := AotSliceAndClassify(SSAProg, Prog);
  SetLength(Result, Length(Regions));
  for r := 0 to High(Regions) do
  begin
    if not Regions[r].Eligible then Continue;
    if Regions[r].EntryPC < 0 then Continue;
    Mem := AotCompileRegion(SSAProg, Prog, Regions[r], TrueVal, Prog.ModernMode, Why);
    if Mem <> nil then
    begin
      Result[n].EntryPC := Regions[r].EntryPC;
      Result[n].Mem := Mem;
      Inc(n);
      if Diag then
        WriteLn(ErrOutput, Format('[AOT] compiled %-24s entryPC=%d', [Regions[r].Name, Regions[r].EntryPC]));
    end
    else if Diag then
      WriteLn(ErrOutput, Format('[AOT] compile-bail %-20s (%s)', [Regions[r].Name, Why]));
  end;
  SetLength(Result, n);
end;

end.
