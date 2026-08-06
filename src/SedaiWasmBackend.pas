unit SedaiWasmBackend;

{ ============================================================================
  SedaiWasmBackend - SSA -> WebAssembly module.

  Step 3 of job/docs/PIANO_WASM.md, following the mapping measured in sec.5-bis
  of that plan. It runs where the bytecode compiler runs: after PHI elimination
  and register allocation, on the same TSSAProgram.

  The four decisions it implements, all of them measured rather than assumed:

  1. Banks map to types. srtInt -> i64 (our integers ARE Int64; i32 would
     change the semantics), srtFloat -> f64, srtString -> i32, which is a
     HANDLE and not the string - the string runtime is class C and lives in
     linear memory.
  2. Registers become LOCALS, not slots in linear memory. Measured: the largest
     program in the corpus needs 530 locals in total, which an engine carries
     without noticing. In linear memory every access would be a load and a
     store that the browser's JIT cannot keep in a machine register - the
     interpreter again, minus the dispatch.
  3. One WASM function per BASIC procedure, so recursion runs on the ENGINE's
     call stack. Natively that is the frame protocol, the standing performance
     lead; here it costs nothing. This is only practical because the calling
     convention already exists: the caller stores into slot k of bank b
     (ssaXferStore*), the callee loads from the same slot (ssaXferLoad*), and
     the two agree because both ask ParamBankAndSlot over the same parameter
     list. The convention IS a signature - it is read, not invented.
  4. A register used by more than one region becomes a WASM GLOBAL, and the
     count is reported rather than buried.

  And the rule that shapes every refusal: there is no deopt. In the browser
  there is no interpreter to fall back into, so an opcode this backend does not
  cover must make the compilation FAIL WITH A MESSAGE, never emit something
  that runs and lies.
  ============================================================================ }

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TypInfo, SedaiSSATypes, SedaiWasmEmitter, SedaiWasmControl;

type
  { Where a register lives in the emitted function. }
  TWasmRegLoc = record
    IsGlobal: Boolean;
    Index: LongWord;
  end;

  TWasmBackend = class
  private
    FProg: TSSAProgram;
    FModern: Boolean;
    FModule: TWasmModule;
    FError: string;
    FGlobalCount: Integer;

    // --- partition ------------------------------------------------------
    FRegionOf: array of Integer;            // block index -> region
    FRegionFirst, FRegionLast: array of Integer;
    FRegionName: array of string;
    FRegionCount: Integer;

    // --- per (bank, register index), addressed by a flat id -------------
    FMaxReg: array[TSSARegisterType] of Integer;
    FUseRegion: array[TSSARegisterType] of array of Integer;  // 0 none, r+1 one, -1 many
    FIsGlobal: array of Boolean;            // flat id -> lives in a WASM global
    FGlobalIdx: array of LongWord;
    FRegionUses: array of array of Boolean; // region -> flat id
    FLocalIdx: array of array of LongWord;  // region -> flat id -> local index
    FCurRegion: Integer;

    // --- per region -----------------------------------------------------
    FParamCount: array of array[TSSARegisterType] of Integer;
    FResultBank: array of Integer;          // -1 none, else Ord(TSSARegisterType)
    FTypeIdx: array of LongWord;
    FFuncIdx: array of LongWord;
    FCalls: array of array of Integer;      // region -> regions it calls
    FRecursive: array of Boolean;
    FUsesGlobal: array of Boolean;

    // --- transfer slots -------------------------------------------------
    { The transfer bank is modelled as one local per (bank, slot), not as
      staging that only exists at a call site. SUB inlining maps a callee's
      registers into its caller and leaves the Store/Load pair BEHIND, inside a
      single region - so a load of slot 0 is not always "read parameter 0", and
      treating it that way read the dispatch state instead. As slot locals both
      spellings mean the same thing, and a real call just copies its parameters
      into them on entry. }
    FSlotCount: array of array[TSSARegisterType] of Integer;

    // --- emission state -------------------------------------------------
    FStateLocal: LongWord;
    FResultTmp: array[TSSARegisterType] of LongWord;
    FSlotBase: array[TSSARegisterType] of LongWord;

    FBankBase: array[TSSARegisterType] of Integer;
    FFlatCount: Integer;
    FUpExposed: array of array of Boolean;   // region -> flat register id

    function Fail(const Msg: string): Boolean;
    function FlatId(const V: TSSAValue): Integer;
    procedure ComputeUpExposed;
    function BlockOfLabel(const AName: string): Integer;
    function BuildPartition: Boolean;
    procedure NoteRegister(const V: TSSAValue; Region: Integer);
    function ClassifyRegisters: Boolean;
    function BuildSignatures: Boolean;
    function DetectRecursion: Boolean;
  private
    FUsesPrint: Boolean;
    FImportCount: LongWord;
    FWriteFunc, FPrintIntFunc, FPrintUIntFunc, FPrintNlFunc: LongWord;
    procedure ScanForPrint;
    procedure EmitPrintHelpers;
    function EmitRegion(R: Integer): Boolean;
    function EmitInstr(B: TWasmBuf; Instr: TSSAInstruction; R: Integer): Boolean;
    procedure LoadReg(B: TWasmBuf; const V: TSSAValue);
    procedure StoreReg(B: TWasmBuf; const V: TSSAValue);
    procedure BoolToBasic(B: TWasmBuf);
    function OpName(Op: TSSAOpCode): string;
  public
    constructor Create(AProgram: TSSAProgram; AModern: Boolean);
    destructor Destroy; override;
    function Compile: Boolean;
    procedure SaveToFile(const Path: string);
    property ErrorMessage: string read FError;
    { How many registers had to be promoted to globals because more than one
      region touches them. Reported, not buried: if it is ever large the
      mapping needs revisiting, not tolerating. }
    property GlobalCount: Integer read FGlobalCount;
    property RegionCount: Integer read FRegionCount;
    property Module: TWasmModule read FModule;
  end;

const
  WASM_XFER_RESULT_SLOT = 255;   // mirrors SedaiSSA.XFER_RESULT_SLOT

implementation

const
  BankType: array[TSSARegisterType] of TWasmValType = (wvtI64, wvtF64, wvtI32);

  { Linear memory layout for PRINT. Digits are built BACKWARDS from SCRATCH_END,
    which is why the buffer is addressed from its end. }
  SCRATCH_END  = 32;      // one past the last byte of the digit scratch
  CONST_SPACE  = 64;      // a literal ' '
  CONST_NL     = 65;      // a literal LF

{ ---------------- PRINT ----------------

  The host import is a BYTE SINK - write(ptr, len) - and nothing else. The
  formatting is ours, emitted here, because BASIC's number spacing is a dialect
  rule (TConsoleBehavior.FormatInt): a leading space stands in for the sign when
  the value is non-negative, and Commodore adds a trailing space where FreeBASIC
  does not. Handing an i64 to JS and letting it call String(n) would produce
  output that differs from the native run in exactly the places this project
  measures byte for byte - the plan rules that out, and it is the whole reason
  the sink is this narrow. }

procedure TWasmBackend.ScanForPrint;
var
  i, j: Integer;
  Blk: TSSABasicBlock;
begin
  FUsesPrint := False;
  for i := 0 to FProg.Blocks.Count - 1 do
  begin
    Blk := FProg.Blocks[i];
    for j := 0 to Blk.Instructions.Count - 1 do
      if TSSAInstruction(Blk.Instructions[j]).OpCode in
         [ssaPrintInt, ssaPrintIntLn, ssaPrintNewLine, ssaPrintUInt] then
      begin
        FUsesPrint := True;
        Exit;
      end;
  end;
end;

procedure TWasmBackend.EmitPrintHelpers;
{ printInt(v: i64): format v the way TConsoleBehavior.FormatInt does, then hand
  the bytes to the sink.

    p := SCRATCH_END
    neg := v < 0
    u := neg ? 0 - v : v          (unsigned, so Low(Int64) works: its negation
                                   wraps to the right magnitude)
    if u = 0 then *--p := '0'
    else while u <> 0 do *--p := '0' + u mod 10; u := u div 10
    *--p := neg ? '-' : ' '
    write(p, SCRATCH_END - p)
    [CLASSIC only] write(CONST_SPACE, 1) }
var
  B: TWasmBuf;
  TVoidI64, TVoid: LongWord;
begin
  TVoidI64 := FModule.TypeIndex([wvtI64], []);
  TVoid := FModule.TypeIndex([], []);

  B := TWasmBuf.Create;
  try
    // locals: 1 = p (i32), 2 = u (i64), 3 = neg (i32)
    B.I32Const(SCRATCH_END); B.LocalSet(1);
    B.LocalGet(0); B.I64Const(0); B.Op(wopI64LtS); B.LocalSet(3);

    B.LocalGet(3);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(0); B.LocalGet(0); B.Op(wopI64Sub); B.LocalSet(2);
    B.Op(wopElse);
      B.LocalGet(0); B.LocalSet(2);
    B.EndOp;

    B.LocalGet(2); B.Op(wopI64Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
      B.LocalGet(1); B.I32Const(Ord('0')); B.OpMem(wopI32Store8, 0, 0);
    B.Op(wopElse);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(2); B.Op(wopI64Eqz); B.BrIf(1);
          B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
          B.LocalGet(1);
          B.LocalGet(2); B.I64Const(10); B.Op(wopI64RemU); B.Op(wopI32WrapI64);
          B.I32Const(Ord('0')); B.Op(wopI32Add);
          B.OpMem(wopI32Store8, 0, 0);
          B.LocalGet(2); B.I64Const(10); B.Op(wopI64DivU); B.LocalSet(2);
          B.Br(0);
        B.EndOp;
      B.EndOp;
    B.EndOp;

    // the one prefix character: '-' when negative, otherwise the space that
    // stands in for the sign
    B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
    B.LocalGet(3);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(1); B.I32Const(Ord('-')); B.OpMem(wopI32Store8, 0, 0);
    B.Op(wopElse);
      B.LocalGet(1); B.I32Const(Ord(' ')); B.OpMem(wopI32Store8, 0, 0);
    B.EndOp;

    B.LocalGet(1);
    B.I32Const(SCRATCH_END); B.LocalGet(1); B.Op(wopI32Sub);
    B.Call(FWriteFunc);

    if not FModern then
    begin
      // Commodore/MSX/QB put a space AFTER the number; FreeBASIC does not.
      B.I32Const(CONST_SPACE); B.I32Const(1); B.Call(FWriteFunc);
    end;

    FModule.AddFunction(TVoidI64, [wvtI32, wvtI64, wvtI32], B);
  finally
    B.Free;
  end;

  { printUInt(v: i64): the SAME digits, but the affixes are NOT the same. The FB
    manual's Print page, under "Differences from QB", says unsigned numbers are
    printed without a space before them - so FreeBASIC gives an unsigned neither
    the sign padding nor a trailing space, while Commodore gives it both.
    TConsoleBehavior.FormatUInt is the spec; getting this wrong would print
    something that looks right and is off by a space. }
  B := TWasmBuf.Create;
  try
    B.I32Const(SCRATCH_END); B.LocalSet(1);
    B.LocalGet(0); B.LocalSet(2);

    B.LocalGet(2); B.Op(wopI64Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
      B.LocalGet(1); B.I32Const(Ord('0')); B.OpMem(wopI32Store8, 0, 0);
    B.Op(wopElse);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(2); B.Op(wopI64Eqz); B.BrIf(1);
          B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
          B.LocalGet(1);
          B.LocalGet(2); B.I64Const(10); B.Op(wopI64RemU); B.Op(wopI32WrapI64);
          B.I32Const(Ord('0')); B.Op(wopI32Add);
          B.OpMem(wopI32Store8, 0, 0);
          B.LocalGet(2); B.I64Const(10); B.Op(wopI64DivU); B.LocalSet(2);
          B.Br(0);
        B.EndOp;
      B.EndOp;
    B.EndOp;

    if not FModern then
    begin
      // Commodore keeps the leading space an unsigned would get as a positive
      // number, and the trailing one. FreeBASIC gives it neither.
      B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
      B.LocalGet(1); B.I32Const(Ord(' ')); B.OpMem(wopI32Store8, 0, 0);
    end;

    B.LocalGet(1);
    B.I32Const(SCRATCH_END); B.LocalGet(1); B.Op(wopI32Sub);
    B.Call(FWriteFunc);
    if not FModern then
    begin
      B.I32Const(CONST_SPACE); B.I32Const(1); B.Call(FWriteFunc);
    end;

    FModule.AddFunction(TVoidI64, [wvtI32, wvtI64, wvtI32], B);
  finally
    B.Free;
  end;

  B := TWasmBuf.Create;
  try
    B.I32Const(CONST_NL); B.I32Const(1); B.Call(FWriteFunc);
    FModule.AddFunction(TVoid, [], B);
  finally
    B.Free;
  end;
end;

constructor TWasmBackend.Create(AProgram: TSSAProgram; AModern: Boolean);
begin
  inherited Create;
  FProg := AProgram;
  FModern := AModern;
  FModule := TWasmModule.Create;
end;

destructor TWasmBackend.Destroy;
begin
  FModule.Free;
  inherited Destroy;
end;

function TWasmBackend.Fail(const Msg: string): Boolean;
begin
  if FError = '' then FError := Msg;
  Result := False;
end;

function TWasmBackend.OpName(Op: TSSAOpCode): string;
begin
  Result := GetEnumName(TypeInfo(TSSAOpCode), Ord(Op));
end;

function TWasmBackend.BlockOfLabel(const AName: string): Integer;
var
  i: Integer;
begin
  for i := 0 to FProg.Blocks.Count - 1 do
    if SameText(FProg.Blocks[i].LabelName, AName) then Exit(i);
  Result := -1;
end;

{ ---------------- 1. partition into procedure regions ---------------- }

function TWasmBackend.BuildPartition: Boolean;
// Procedure bodies are contiguous block regions past the module's END, reachable
// only through ssaCallSub (LowerDeferredProcedures). So the entries partition
// the block list, and region 0 is the module itself.
var
  i, j, k, B, N: Integer;
  Blk: TSSABasicBlock;
  Instr: TSSAInstruction;
  IsEntry: array of Boolean;
  Target: Integer;
begin
  N := FProg.Blocks.Count;
  if N = 0 then Exit(Fail('the program has no basic blocks'));
  SetLength(IsEntry, N);
  for i := 0 to N - 1 do IsEntry[i] := False;

  for i := 0 to N - 1 do
  begin
    Blk := FProg.Blocks[i];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := TSSAInstruction(Blk.Instructions[j]);
      if (Instr.OpCode = ssaCallSub) and (Instr.Dest.Kind = svkLabel) then
      begin
        B := BlockOfLabel(Instr.Dest.LabelName);
        if B < 0 then
          Exit(Fail(Format('call to "%s", which is not a block in this program',
                           [Instr.Dest.LabelName])));
        IsEntry[B] := True;
      end
      else if Instr.OpCode = ssaCallSubIndirect then
        Exit(Fail('ssaCallSubIndirect (a function pointer call) is not covered yet: ' +
                  'it needs the function table and a signature for the callee'));
    end;
  end;
  IsEntry[0] := True;                       // region 0 is the module

  SetLength(FRegionOf, N);
  FRegionCount := 0;
  for i := 0 to N - 1 do
  begin
    if IsEntry[i] then
    begin
      Inc(FRegionCount);
      SetLength(FRegionFirst, FRegionCount);
      SetLength(FRegionLast, FRegionCount);
      SetLength(FRegionName, FRegionCount);
      FRegionFirst[FRegionCount - 1] := i;
      FRegionName[FRegionCount - 1] := FProg.Blocks[i].LabelName;
    end;
    FRegionOf[i] := FRegionCount - 1;
    FRegionLast[FRegionCount - 1] := i;
  end;
  if FRegionName[0] = '' then FRegionName[0] := 'main';

  // A jump that leaves its region would break the dispatch, which only knows
  // the blocks of one function. Refuse rather than emit a wrong branch.
  for i := 0 to N - 1 do
  begin
    Blk := FProg.Blocks[i];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := TSSAInstruction(Blk.Instructions[j]);
      if (Instr.Dest.Kind = svkLabel) and
         (Instr.OpCode in [ssaJump, ssaJumpIfZero, ssaJumpIfNotZero]) then
      begin
        Target := BlockOfLabel(Instr.Dest.LabelName);
        if Target < 0 then
          Exit(Fail(Format('jump to unknown label "%s"', [Instr.Dest.LabelName])));
        if FRegionOf[Target] <> FRegionOf[i] then
          Exit(Fail(Format('a jump crosses a procedure boundary: block "%s" (%s) -> "%s" (%s)',
                    [Blk.LabelName, FRegionName[FRegionOf[i]],
                     Instr.Dest.LabelName, FRegionName[FRegionOf[Target]]])));
      end;
    end;
  end;

  SetLength(FParamCount, FRegionCount);
  SetLength(FSlotCount, FRegionCount);
  SetLength(FResultBank, FRegionCount);
  SetLength(FTypeIdx, FRegionCount);
  SetLength(FFuncIdx, FRegionCount);
  SetLength(FCalls, FRegionCount);
  SetLength(FRecursive, FRegionCount);
  SetLength(FUsesGlobal, FRegionCount);
  for i := 0 to FRegionCount - 1 do
  begin
    for k := 0 to Ord(High(TSSARegisterType)) do
    begin
      FParamCount[i][TSSARegisterType(k)] := 0;
      FSlotCount[i][TSSARegisterType(k)] := 0;
    end;
    FResultBank[i] := -1;
    FRecursive[i] := False;
    FUsesGlobal[i] := False;
  end;
  Result := True;
end;

{ ---------------- 2. registers: local, or global if shared ---------------- }

procedure TWasmBackend.NoteRegister(const V: TSSAValue; Region: Integer);
var
  RT: TSSARegisterType;
begin
  if V.Kind <> svkRegister then Exit;
  RT := V.RegType;
  if V.RegIndex > FMaxReg[RT] then
  begin
    FMaxReg[RT] := V.RegIndex;
    SetLength(FUseRegion[RT], FMaxReg[RT] + 1);
  end;
  while Length(FUseRegion[RT]) <= V.RegIndex do
    SetLength(FUseRegion[RT], Length(FUseRegion[RT]) + 1);
  if FUseRegion[RT][V.RegIndex] = 0 then
    FUseRegion[RT][V.RegIndex] := Region + 1        // 0 = untouched, so bias by one
  else if FUseRegion[RT][V.RegIndex] <> Region + 1 then
    FUseRegion[RT][V.RegIndex] := -1;               // more than one region
end;

function TWasmBackend.FlatId(const V: TSSAValue): Integer;
begin
  if V.Kind <> svkRegister then Exit(-1);
  Result := FBankBase[V.RegType] + V.RegIndex;
end;

procedure TWasmBackend.ComputeUpExposed;
{ Which registers carry a value INTO a region, i.e. are read on some path before
  being written there. That is the question that separates "two procedures share
  a value" from "the allocator reused a number for two unrelated temporaries" -
  and getting it wrong the conservative way refuses perfectly good programs,
  which is what asking only "does more than one region mention it" did.

  Standard live-in, restricted to the region: Gen = read before written in the
  block, Kill = written in the block, LiveIn = Gen + (LiveOut - Kill). Successors
  outside the region are dropped, which is sound here because jumps never cross a
  region (BuildPartition refuses that) - the only cross-region edge is a call,
  and a call's effect on the callee's registers is what the classification is
  deciding, not something to propagate through. }
var
  r, i, j, b, s, f, N, First, Last, Idx: Integer;
  Blk, Succ: TSSABasicBlock;
  Instr: TSSAInstruction;
  Gen, Kill, LiveIn, LiveOut: array of array of Boolean;
  Changed: Boolean;

  procedure MarkUse(const V: TSSAValue; Blk: Integer);
  var fid: Integer;
  begin
    fid := FlatId(V);
    if (fid >= 0) and (not Kill[Blk][fid]) then Gen[Blk][fid] := True;
  end;

begin
  N := FProg.Blocks.Count;
  SetLength(Gen, N); SetLength(Kill, N); SetLength(LiveIn, N); SetLength(LiveOut, N);
  for i := 0 to N - 1 do
  begin
    SetLength(Gen[i], FFlatCount);   SetLength(Kill[i], FFlatCount);
    SetLength(LiveIn[i], FFlatCount); SetLength(LiveOut[i], FFlatCount);
    for j := 0 to FFlatCount - 1 do
    begin
      Gen[i][j] := False; Kill[i][j] := False;
      LiveIn[i][j] := False; LiveOut[i][j] := False;
    end;
  end;

  for i := 0 to N - 1 do
  begin
    Blk := FProg.Blocks[i];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := TSSAInstruction(Blk.Instructions[j]);
      MarkUse(Instr.Src1, i);
      MarkUse(Instr.Src2, i);
      MarkUse(Instr.Src3, i);
      Idx := FlatId(Instr.Dest);
      if Idx >= 0 then Kill[i][Idx] := True;
    end;
  end;

  repeat
    Changed := False;
    for i := N - 1 downto 0 do
    begin
      Blk := FProg.Blocks[i];
      for s := 0 to Blk.Successors.Count - 1 do
      begin
        Succ := TSSABasicBlock(Blk.Successors[s]);
        b := -1;
        for j := 0 to N - 1 do
          if FProg.Blocks[j] = Succ then begin b := j; Break; end;
        if (b < 0) or (FRegionOf[b] <> FRegionOf[i]) then Continue;
        for f := 0 to FFlatCount - 1 do
          if LiveIn[b][f] and not LiveOut[i][f] then
          begin
            LiveOut[i][f] := True;
            Changed := True;
          end;
      end;
      for f := 0 to FFlatCount - 1 do
        if (Gen[i][f] or (LiveOut[i][f] and not Kill[i][f])) and not LiveIn[i][f] then
        begin
          LiveIn[i][f] := True;
          Changed := True;
        end;
    end;
  until not Changed;

  SetLength(FUpExposed, FRegionCount);
  for r := 0 to FRegionCount - 1 do
  begin
    SetLength(FUpExposed[r], FFlatCount);
    First := FRegionFirst[r];
    Last := FRegionLast[r];
    for f := 0 to FFlatCount - 1 do FUpExposed[r][f] := LiveIn[First][f];
    // A block inside the region that no in-region edge reaches (a procedure's
    // epilogue reached only by a return path, say) would otherwise hide its own
    // live-in, so fold every block whose in-region predecessors are none.
    for i := First to Last do
      if i <> First then
      begin
        Blk := FProg.Blocks[i];
        b := 0;
        for s := 0 to Blk.Predecessors.Count - 1 do
        begin
          Succ := TSSABasicBlock(Blk.Predecessors[s]);
          for j := First to Last do
            if FProg.Blocks[j] = Succ then begin Inc(b); Break; end;
        end;
        if b = 0 then
          for f := 0 to FFlatCount - 1 do
            if LiveIn[i][f] then FUpExposed[r][f] := True;
      end;
  end;
end;

function TWasmBackend.ClassifyRegisters: Boolean;
var
  i, j, r: Integer;
  RT: TSSARegisterType;
  Blk: TSSABasicBlock;
  Instr: TSSAInstruction;
  Init: TWasmBuf;
  Carried: Boolean;
begin
  for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
  begin
    FMaxReg[RT] := -1;
    SetLength(FUseRegion[RT], 0);
  end;

  for i := 0 to FProg.Blocks.Count - 1 do
  begin
    Blk := FProg.Blocks[i];
    r := FRegionOf[i];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := TSSAInstruction(Blk.Instructions[j]);
      NoteRegister(Instr.Dest, r);
      NoteRegister(Instr.Src1, r);
      NoteRegister(Instr.Src2, r);
      NoteRegister(Instr.Src3, r);
    end;
  end;

  // Flat register ids, so liveness and the per-region maps can use plain arrays.
  FFlatCount := 0;
  for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
  begin
    FBankBase[RT] := FFlatCount;
    Inc(FFlatCount, FMaxReg[RT] + 1);
  end;
  ComputeUpExposed;

  SetLength(FRegionUses, FRegionCount);
  for r := 0 to FRegionCount - 1 do
  begin
    SetLength(FRegionUses[r], FFlatCount);
    for i := 0 to FFlatCount - 1 do FRegionUses[r][i] := False;
  end;
  for i := 0 to FProg.Blocks.Count - 1 do
  begin
    Blk := FProg.Blocks[i];
    r := FRegionOf[i];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := TSSAInstruction(Blk.Instructions[j]);
      if FlatId(Instr.Dest) >= 0 then FRegionUses[r][FlatId(Instr.Dest)] := True;
      if FlatId(Instr.Src1) >= 0 then FRegionUses[r][FlatId(Instr.Src1)] := True;
      if FlatId(Instr.Src2) >= 0 then FRegionUses[r][FlatId(Instr.Src2)] := True;
      if FlatId(Instr.Src3) >= 0 then FRegionUses[r][FlatId(Instr.Src3)] := True;
    end;
  end;

  // A register mentioned by two regions is NOT necessarily a value that crosses:
  // the allocator reuses a number for unrelated values with disjoint live ranges,
  // and in that case each region can keep its own local. What forces a global is
  // a register that carries a value IN - read on some path before being written.
  // Asking the coarse question instead refused programs that are perfectly fine,
  // starting with any recursive procedure.
  FGlobalCount := 0;
  SetLength(FIsGlobal, FFlatCount);
  SetLength(FGlobalIdx, FFlatCount);
  for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
    for i := 0 to FMaxReg[RT] do
    begin
      j := FBankBase[RT] + i;
      FIsGlobal[j] := False;
      FGlobalIdx[j] := 0;
      Carried := False;
      if FUseRegion[RT][i] = -1 then
        for r := 0 to FRegionCount - 1 do
          if FRegionUses[r][j] and FUpExposed[r][j] then Carried := True;
      if Carried then
      begin
        Init := TWasmBuf.Create;
        try
          case RT of
            srtFloat:  Init.F64Const(0);
            srtString: Init.I32Const(0);
          else
            Init.I64Const(0);
          end;
          FIsGlobal[j] := True;
          FGlobalIdx[j] := FModule.DefineGlobal(BankType[RT], True, Init);
        finally
          Init.Free;
        end;
        Inc(FGlobalCount);
      end;
    end;

  for r := 0 to FRegionCount - 1 do
    for j := 0 to FFlatCount - 1 do
      if FRegionUses[r][j] and FIsGlobal[j] then FUsesGlobal[r] := True;
  Result := True;
end;

{ ---------------- 3. signatures, read off the transfer slots ---------------- }

function TWasmBackend.BuildSignatures: Boolean;
var
  i, j, r, Slot, Target, TR: Integer;
  RT: TSSARegisterType;
  Blk: TSSABasicBlock;
  Instr: TSSAInstruction;
  Params: TWasmValTypeArray;
  Res: TWasmValTypeArray;
  n, p: Integer;

  procedure Widen(Region: Integer; Bank: TSSARegisterType; ASlot: Integer);
  begin
    if ASlot = WASM_XFER_RESULT_SLOT then Exit;
    if ASlot + 1 > FParamCount[Region][Bank] then FParamCount[Region][Bank] := ASlot + 1;
  end;

  procedure NoteSlot(Region: Integer; Bank: TSSARegisterType; ASlot: Integer);
  begin
    if ASlot = WASM_XFER_RESULT_SLOT then Exit;
    if ASlot + 1 > FSlotCount[Region][Bank] then FSlotCount[Region][Bank] := ASlot + 1;
  end;

  function SlotOf(Instr: TSSAInstruction; out ASlot: Integer): Boolean;
  begin
    Result := Instr.Src3.Kind = svkConstInt;
    if Result then ASlot := Integer(Instr.Src3.ConstInt) else ASlot := -1;
  end;

  function XferBank(Op: TSSAOpCode; out Bank: TSSARegisterType): Boolean;
  begin
    Result := True;
    case Op of
      ssaXferStoreInt, ssaXferLoadInt:       Bank := srtInt;
      ssaXferStoreFloat, ssaXferLoadFloat:   Bank := srtFloat;
      ssaXferStoreString, ssaXferLoadString: Bank := srtString;
    else
      Bank := srtInt; Result := False;
    end;
  end;

begin
  // The callee's loads and the caller's stores both name (bank, slot), so the
  // signature is the union of the two - a parameter the body never reads still
  // has to be in the type, or the call site would not match.
  for i := 0 to FProg.Blocks.Count - 1 do
  begin
    Blk := FProg.Blocks[i];
    r := FRegionOf[i];
    Target := -1;
    for j := Blk.Instructions.Count - 1 downto 0 do
    begin
      Instr := TSSAInstruction(Blk.Instructions[j]);
      if (Instr.OpCode = ssaCallSub) and (Instr.Dest.Kind = svkLabel) then
      begin
        Target := FRegionOf[BlockOfLabel(Instr.Dest.LabelName)];
        // record the call-graph edge while we are here
        n := Length(FCalls[r]);
        SetLength(FCalls[r], n + 1);
        FCalls[r][n] := Target;
      end
      else if XferBank(Instr.OpCode, RT) then
      begin
        if not SlotOf(Instr, Slot) then
          Exit(Fail(Format('a transfer slot is not a constant in block "%s"', [Blk.LabelName])));
        NoteSlot(r, RT, Slot);      // this region needs a local for that slot
        if Instr.OpCode in [ssaXferLoadInt, ssaXferLoadFloat, ssaXferLoadString] then
        begin
          // a load of the RESULT slot belongs to the caller, a load of any other
          // slot is the callee reading a parameter
          if (Slot = WASM_XFER_RESULT_SLOT) and (Target >= 0) then
            FResultBank[Target] := Ord(RT);
          // A non-result LOAD says nothing about this region's arity. SUB
          // inlining leaves the callee's loads behind in its caller, so reading
          // them as "my parameters" gave Hypot(Double, Double) an integer first
          // parameter - the one Keep took. Only the CALL SITES know the arity.
        end
        else
        begin
          if Slot = WASM_XFER_RESULT_SLOT then
            FResultBank[r] := Ord(RT)              // the callee writes its result
          else if Target >= 0 then
            Widen(Target, RT, Slot);               // staged for the call below
          // A non-result store with no call after it in the same block means the
          // staging was split from its call. Ignoring it is safe: the CALLEE's
          // loads already fix the signature, and both sides read the same count -
          // crediting it to this region instead would give the module itself
          // parameters it does not have.
        end;
      end;
    end;
  end;

  for r := 0 to FRegionCount - 1 do
  begin
    SetLength(Params, 0);
    p := 0;
    for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
    begin
      for n := 0 to FParamCount[r][RT] - 1 do
      begin
        SetLength(Params, p + 1);
        Params[p] := BankType[RT];
        Inc(p);
      end;
    end;
    if r = 0 then
    begin
      // The module entry takes nothing and returns nothing. The parameter COUNTS
      // have to be cleared too, not just the signature: EmitRegion lays the
      // locals out behind them, so leaving them set shifts every local index in
      // main by however many arguments some call happened to stage.
      SetLength(Params, 0);
      for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
        FParamCount[0][RT] := 0;
      FResultBank[0] := -1;
    end;
    if FResultBank[r] >= 0 then
    begin
      SetLength(Res, 1);
      Res[0] := BankType[TSSARegisterType(FResultBank[r])];
    end
    else
      SetLength(Res, 0);
    FTypeIdx[r] := FModule.TypeIndex(Params, Res);
  end;
  Result := True;
end;

function TWasmBackend.DetectRecursion: Boolean;
var
  r, i, k, Changed: Integer;
  Reach: array of array of Boolean;
begin
  SetLength(Reach, FRegionCount);
  for r := 0 to FRegionCount - 1 do
  begin
    SetLength(Reach[r], FRegionCount);
    for i := 0 to FRegionCount - 1 do Reach[r][i] := False;
    for i := 0 to High(FCalls[r]) do Reach[r][FCalls[r][i]] := True;
  end;
  repeat
    Changed := 0;
    for r := 0 to FRegionCount - 1 do
      for i := 0 to FRegionCount - 1 do
        if Reach[r][i] then
          for k := 0 to FRegionCount - 1 do
            if Reach[i][k] and not Reach[r][k] then
            begin
              Reach[r][k] := True;
              Inc(Changed);
            end;
  until Changed = 0;

  for r := 0 to FRegionCount - 1 do
    FRecursive[r] := Reach[r][r];

  // A global is one storage location for the whole program, so a recursive
  // procedure holding a value there would have its own activation clobbered by
  // the next one. That is exactly the case the VM answers with a copying frame,
  // and there is no deopt here to fall back on - refuse, and say which.
  for r := 0 to FRegionCount - 1 do
    if FRecursive[r] and FUsesGlobal[r] then
      Exit(Fail(Format('procedure "%s" is recursive AND touches a register shared with ' +
                       'another procedure, which has to become a WASM global - one storage ' +
                       'location cannot hold two activations', [FRegionName[r]])));
  Result := True;
end;

{ ---------------- 4. lowering ---------------- }

procedure TWasmBackend.LoadReg(B: TWasmBuf; const V: TSSAValue);
var
  f: Integer;
begin
  f := FlatId(V);
  if FIsGlobal[f] then B.GlobalGet(FGlobalIdx[f])
                  else B.LocalGet(FLocalIdx[FCurRegion][f]);
end;

procedure TWasmBackend.StoreReg(B: TWasmBuf; const V: TSSAValue);
var
  f: Integer;
begin
  f := FlatId(V);
  if FIsGlobal[f] then B.GlobalSet(FGlobalIdx[f])
                  else B.LocalSet(FLocalIdx[FCurRegion][f]);
end;

procedure TWasmBackend.BoolToBasic(B: TWasmBuf);
// A WASM comparison yields i32 0/1; BASIC's TRUE is -1 (FTrueValue is built as
// -1 and nothing ever changes it). Widen, then flip the sign.
begin
  B.Op(wopI64ExtendI32S);
  B.I64Const(-1);
  B.Op(wopI64Mul);
end;

function TWasmBackend.EmitInstr(B: TWasmBuf; Instr: TSSAInstruction; R: Integer): Boolean;

  procedure Bin(Opcode: Byte);
  begin
    LoadReg(B, Instr.Src1);
    LoadReg(B, Instr.Src2);
    B.Op(Opcode);
    StoreReg(B, Instr.Dest);
  end;

  procedure Cmp(Opcode: Byte);
  begin
    LoadReg(B, Instr.Src1);
    LoadReg(B, Instr.Src2);
    B.Op(Opcode);
    BoolToBasic(B);
    StoreReg(B, Instr.Dest);
  end;

  procedure Un(Opcode: Byte);
  begin
    LoadReg(B, Instr.Src1);
    B.Op(Opcode);
    StoreReg(B, Instr.Dest);
  end;

var
  Slot: Integer;
  RT: TSSARegisterType;
begin
  Result := True;
  case Instr.OpCode of
    ssaLabel, ssaNop: ;                     // no code of their own
    ssaPrintEnd: ;                          // resets C128 reverse mode; nothing to do here

    ssaPrintInt:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FPrintIntFunc);
      end;
    ssaPrintIntLn:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FPrintIntFunc);
        B.Call(FPrintNlFunc);
      end;
    ssaPrintNewLine:
      B.Call(FPrintNlFunc);
    ssaPrintUInt:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FPrintUIntFunc);
      end;
    { PRINT's semicolon separator. Every dialect preset in the tree sets
      SemicolonAction to saNoSpace - all eight of them - so this emits nothing.
      Written out rather than folded in with the other no-ops, because that is a
      MEASURED fact about the presets and not an assumption about the language:
      the property is writable, and if a preset ever asks for a space this is the
      place that has to grow one. }
    ssaPrintSemicolon: ;

    ssaModFloat:
      begin
        // The VM raises "Float modulo by zero"; f64.div would quietly answer NaN
        // and let the program carry on, which is the one thing the backend must
        // never do. The trap is the loud equivalent of the interpreter stopping.
        LoadReg(B, Instr.Src2);
        B.F64Const(0);
        B.Op(wopF64Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.Op(wopUnreachable);
        B.EndOp;
        // x - floor(x / y) * y, exactly as the interpreter computes it
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        B.Op(wopF64Div);
        B.Op(wopF64Floor);
        LoadReg(B, Instr.Src2);
        B.Op(wopF64Mul);
        B.Op(wopF64Sub);
        StoreReg(B, Instr.Dest);
      end;

    ssaNarrowInt:
      begin
        // Width codes are NarrowInt64's: 1=s8 2=u8 3=s16 4=u16 5=s32 6=u32,
        // anything else is the identity.
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('ssaNarrowInt without a constant width code'));
        LoadReg(B, Instr.Src1);
        case Instr.Src3.ConstInt of
          1: B.Op(wopI64Extend8S);
          2: begin B.I64Const($FF); B.Op(wopI64And); end;
          3: B.Op(wopI64Extend16S);
          4: begin B.I64Const($FFFF); B.Op(wopI64And); end;
          5: B.Op(wopI64Extend32S);
          6: begin B.I64Const($FFFFFFFF); B.Op(wopI64And); end;
        end;
        StoreReg(B, Instr.Dest);
      end;

    ssaPhi:
      Exit(Fail('a PHI survived into the backend: PHI elimination must run first'));

    ssaLoadConstInt:
      begin
        if Instr.Src1.Kind = svkConstInt then B.I64Const(Instr.Src1.ConstInt)
        else Exit(Fail('ssaLoadConstInt without an integer constant'));
        StoreReg(B, Instr.Dest);
      end;
    ssaLoadConstFloat:
      begin
        if Instr.Src1.Kind = svkConstFloat then B.F64Const(Instr.Src1.ConstFloat)
        else if Instr.Src1.Kind = svkConstInt then B.F64Const(Instr.Src1.ConstInt)
        else Exit(Fail('ssaLoadConstFloat without a numeric constant'));
        StoreReg(B, Instr.Dest);
      end;

    ssaCopyInt, ssaCopyFloat, ssaCopyString:
      begin
        LoadReg(B, Instr.Src1);
        StoreReg(B, Instr.Dest);
      end;

    ssaAddInt:  Bin(wopI64Add);
    ssaSubInt:  Bin(wopI64Sub);
    ssaMulInt:  Bin(wopI64Mul);
    // i64.div_s / rem_s TRAP on a zero divisor where the VM raises a BASIC error.
    // Both stop the program loudly; the diagnostic differs and that is recorded,
    // not papered over.
    ssaDivInt:  Bin(wopI64DivS);
    ssaModInt:  Bin(wopI64RemS);
    ssaDivUInt: Bin(wopI64DivU);
    ssaModUInt: Bin(wopI64RemU);
    ssaNegInt:
      begin
        // there is no i64.neg
        B.I64Const(0);
        LoadReg(B, Instr.Src1);
        B.Op(wopI64Sub);
        StoreReg(B, Instr.Dest);
      end;

    ssaAddFloat: Bin(wopF64Add);
    ssaSubFloat: Bin(wopF64Sub);
    ssaMulFloat: Bin(wopF64Mul);
    ssaDivFloat: Bin(wopF64Div);
    ssaNegFloat: Un(wopF64Neg);

    ssaBitwiseAnd: Bin(wopI64And);
    ssaBitwiseOr:  Bin(wopI64Or);
    ssaBitwiseXor: Bin(wopI64Xor);
    ssaBitwiseNot:
      begin
        LoadReg(B, Instr.Src1);
        B.I64Const(-1);
        B.Op(wopI64Xor);
        StoreReg(B, Instr.Dest);
      end;
    ssaShl:     Bin(wopI64Shl);
    ssaShr:     Bin(wopI64ShrS);
    ssaShrUInt: Bin(wopI64ShrU);

    ssaCmpEqInt: Cmp(wopI64Eq);
    ssaCmpNeInt: Cmp(wopI64Ne);
    ssaCmpLtInt: Cmp(wopI64LtS);
    ssaCmpGtInt: Cmp(wopI64GtS);
    ssaCmpLeInt: Cmp(wopI64LeS);
    ssaCmpGeInt: Cmp(wopI64GeS);
    ssaCmpLtUInt: Cmp(wopI64LtU);
    ssaCmpGtUInt: Cmp(wopI64GtU);
    ssaCmpLeUInt: Cmp(wopI64LeU);
    ssaCmpGeUInt: Cmp(wopI64GeU);
    ssaCmpEqFloat: Cmp(wopF64Eq);
    ssaCmpNeFloat: Cmp(wopF64Ne);
    ssaCmpLtFloat: Cmp(wopF64Lt);
    ssaCmpGtFloat: Cmp(wopF64Gt);
    ssaCmpLeFloat: Cmp(wopF64Le);
    ssaCmpGeFloat: Cmp(wopF64Ge);

    ssaIntToFloat: Un(wopF64ConvertI64S);
    ssaFloatToInt:
      begin
        // The IMPLICIT conversion is dialect-dependent: FreeBASIC rounds half to
        // even (which is exactly f64.nearest), Commodore v7 truncates. Saturating
        // truncation, because the trapping form would kill the module over a NaN.
        LoadReg(B, Instr.Src1);
        if FModern then B.Op(wopF64Nearest);
        B.TruncSat(wopfcI64TruncSatF64S);
        StoreReg(B, Instr.Dest);
      end;
    ssaFloatRound:
      begin
        LoadReg(B, Instr.Src1);
        B.Op(wopF64Nearest);
        B.TruncSat(wopfcI64TruncSatF64S);
        StoreReg(B, Instr.Dest);
      end;

    ssaMathSqr: Un(wopF64Sqrt);
    ssaMathAbs: Un(wopF64Abs);
    ssaMathInt: Un(wopF64Floor);
    ssaMathFix: Un(wopF64Trunc);

    ssaXferStoreInt, ssaXferStoreFloat, ssaXferStoreString:
      begin
        case Instr.OpCode of
          ssaXferStoreFloat:  RT := srtFloat;
          ssaXferStoreString: RT := srtString;
        else
          RT := srtInt;
        end;
        Slot := Integer(Instr.Src3.ConstInt);
        LoadReg(B, Instr.Src1);
        if Slot = WASM_XFER_RESULT_SLOT then B.LocalSet(FResultTmp[RT])
                                         else B.LocalSet(FSlotBase[RT] + LongWord(Slot));
      end;

    ssaXferLoadInt, ssaXferLoadFloat, ssaXferLoadString:
      begin
        case Instr.OpCode of
          ssaXferLoadFloat:  RT := srtFloat;
          ssaXferLoadString: RT := srtString;
        else
          RT := srtInt;
        end;
        Slot := Integer(Instr.Src3.ConstInt);
        if Slot = WASM_XFER_RESULT_SLOT then
          B.LocalGet(FResultTmp[RT])          // the callee's result, parked after the call
        else
          B.LocalGet(FSlotBase[RT] + LongWord(Slot));
        StoreReg(B, Instr.Dest);
      end;
  else
    Exit(Fail(Format('%s is not covered by the WASM backend yet (line %d)',
                     [OpName(Instr.OpCode), Instr.SourceLine])));
  end;
end;

function TWasmBackend.EmitRegion(R: Integer): Boolean;
var
  D: TWasmDispatch;
  Blk: TSSABasicBlock;
  Instr: TSSAInstruction;
  Body, B: TWasmBuf;
  i, j, k, First, Last, N, Target, P: Integer;
  RT: TSSARegisterType;
  Locals: TWasmValTypeArray;
  Terminated: Boolean;
  CalleeRegion: Integer;

  procedure PushArgs(Callee: Integer);
  // The arguments are already in this region's slot locals, put there by the
  // ssaXferStore* that staged them; WASM just wants them on the stack in
  // signature order.
  var
    Bank: TSSARegisterType;
    s: Integer;
  begin
    for Bank := Low(TSSARegisterType) to High(TSSARegisterType) do
      for s := 0 to FParamCount[Callee][Bank] - 1 do
        if s < FSlotCount[R][Bank] then
          B.LocalGet(FSlotBase[Bank] + LongWord(s))
        else
          case Bank of                       // a parameter this caller never staged
            srtFloat:  B.F64Const(0);
            srtString: B.I32Const(0);
          else
            B.I64Const(0);
          end;
  end;

begin
  First := FRegionFirst[R];
  Last := FRegionLast[R];
  N := Last - First + 1;

  // locals: parameters, then the dispatch state, the three result temporaries,
  // and finally one per register this region owns
  P := FParamCount[R][srtInt] + FParamCount[R][srtFloat] + FParamCount[R][srtString];
  FStateLocal := LongWord(P);
  FResultTmp[srtInt] := LongWord(P + 1);
  FResultTmp[srtFloat] := LongWord(P + 2);
  FResultTmp[srtString] := LongWord(P + 3);
  SetLength(Locals, 4);
  Locals[0] := wvtI32;                       // dispatch state
  Locals[1] := wvtI64; Locals[2] := wvtF64; Locals[3] := wvtI32;
  // one local per transfer slot this region mentions
  k := P + 4;
  for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
  begin
    FSlotBase[RT] := LongWord(k);
    for i := 0 to FSlotCount[R][RT] - 1 do
    begin
      SetLength(Locals, Length(Locals) + 1);
      Locals[High(Locals)] := BankType[RT];
      Inc(k);
    end;
  end;
  // Every register this region touches and that is not a global gets a local
  // HERE - a register two regions use for unrelated values needs its own local
  // in each, which is the whole point of not making it a global.
  SetLength(FLocalIdx[R], FFlatCount);
  for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
    for i := 0 to FMaxReg[RT] do
    begin
      j := FBankBase[RT] + i;
      if FRegionUses[R][j] and (not FIsGlobal[j]) then
      begin
        FLocalIdx[R][j] := LongWord(k);
        SetLength(Locals, Length(Locals) + 1);
        Locals[High(Locals)] := BankType[RT];
        Inc(k);
      end;
    end;
  FCurRegion := R;

  D := TWasmDispatch.Create(N, FStateLocal);
  Body := TWasmBuf.Create;
  try
    // Prologue: copy the parameters into the slot locals the body reads.
    j := 0;
    for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
      for i := 0 to FParamCount[R][RT] - 1 do
      begin
        Body.LocalGet(LongWord(j));
        Body.LocalSet(FSlotBase[RT] + LongWord(i));
        Inc(j);
      end;

    for i := 0 to N - 1 do
    begin
      Blk := FProg.Blocks[First + i];
      B := D.Body(i);
      Terminated := False;
      for j := 0 to Blk.Instructions.Count - 1 do
      begin
        Instr := TSSAInstruction(Blk.Instructions[j]);
        case Instr.OpCode of
          ssaJump:
            begin
              Target := BlockOfLabel(Instr.Dest.LabelName) - First;
              D.EmitGotoTerminal(i, Target);
              Terminated := True;
            end;
          ssaJumpIfZero, ssaJumpIfNotZero:
            begin
              Target := BlockOfLabel(Instr.Dest.LabelName) - First;
              if i + 1 >= N then
                Exit(Fail(Format('a conditional jump in block "%s" has no following block',
                                 [Blk.LabelName])));
              LoadReg(B, Instr.Src1);
              B.Op(wopI64Eqz);               // i32: "the value is zero"
              if Instr.OpCode = ssaJumpIfZero then
                D.EmitBranch(i, Target, i + 1)
              else
                D.EmitBranch(i, i + 1, Target);
              Terminated := True;
            end;
          ssaCallSub:
            begin
              CalleeRegion := FRegionOf[BlockOfLabel(Instr.Dest.LabelName)];
              PushArgs(CalleeRegion);
              B.Call(FFuncIdx[CalleeRegion]);
              if FResultBank[CalleeRegion] >= 0 then
                B.LocalSet(FResultTmp[TSSARegisterType(FResultBank[CalleeRegion])]);
            end;
          ssaReturnSub:
            begin
              if FResultBank[R] >= 0 then
                B.LocalGet(FResultTmp[TSSARegisterType(FResultBank[R])]);
              B.Op(wopReturn);
              Terminated := True;
            end;
          ssaEnd, ssaStop:
            begin
              if FResultBank[R] >= 0 then
                B.LocalGet(FResultTmp[TSSARegisterType(FResultBank[R])]);
              B.Op(wopReturn);
              Terminated := True;
            end;
        else
          if not EmitInstr(B, Instr, R) then Exit(False);
        end;
        if Terminated then Break;
      end;
      if not Terminated then
      begin
        if i + 1 < N then D.EmitGotoTerminal(i, i + 1)
        else
        begin
          if FResultBank[R] >= 0 then
            D.Body(i).LocalGet(FResultTmp[TSSARegisterType(FResultBank[R])]);
          D.Body(i).Op(wopReturn);
        end;
      end;
    end;

    D.Emit(Body, 0);
    // The region is left only by an explicit return, so anything after it is
    // unreachable - but the validator still wants the function to type-check.
    Body.Op(wopUnreachable);
    FFuncIdx[R] := FModule.AddFunction(FTypeIdx[R], Locals, Body);
    Result := True;
  finally
    Body.Free;
    D.Free;
  end;
end;

function TWasmBackend.Compile: Boolean;
var
  r: Integer;
begin
  FError := '';
  if not BuildPartition then Exit(False);

  // Imports own the low indices, so they must be declared before the first
  // DEFINITION - and ClassifyRegisters defines globals.
  ScanForPrint;
  FImportCount := 0;
  if FUsesPrint then
  begin
    FWriteFunc := FModule.ImportFunc('env', 'write',
                                     FModule.TypeIndex([wvtI32, wvtI32], []));
    FImportCount := 1;
  end;

  SetLength(FLocalIdx, FProg.Blocks.Count);   // sized by region below
  if not ClassifyRegisters then Exit(False);
  SetLength(FLocalIdx, FRegionCount);
  if not BuildSignatures then Exit(False);
  if not DetectRecursion then Exit(False);

  if FUsesPrint then
  begin
    FModule.DefineMemory(1, 0);
    FModule.DataSegment(CONST_SPACE, PByte(PAnsiChar(' '#10)), 2);
    FModule.ExportMemory('memory');
  end;

  // Functions have to be numbered before any of them is emitted, because a call
  // names its callee by index and a procedure may be called before it is built.
  for r := 0 to FRegionCount - 1 do
    FFuncIdx[r] := FImportCount + LongWord(r);
  // The order here must match the order EmitPrintHelpers adds them in.
  FPrintIntFunc := FImportCount + LongWord(FRegionCount);
  FPrintUIntFunc := FPrintIntFunc + 1;
  FPrintNlFunc := FPrintIntFunc + 2;

  for r := 0 to FRegionCount - 1 do
    if not EmitRegion(r) then Exit(False);
  if FUsesPrint then EmitPrintHelpers;

  FModule.ExportFunc('main', FFuncIdx[0]);
  for r := 1 to FRegionCount - 1 do
    FModule.ExportFunc(FRegionName[r], FFuncIdx[r]);
  Result := True;
end;

procedure TWasmBackend.SaveToFile(const Path: string);
begin
  FModule.SaveToFile(Path);
end;

end.
