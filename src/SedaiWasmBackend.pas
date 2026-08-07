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

  TSSAValueArray = array of TSSAValue;

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
    FRawTmp: LongWord;            // i64: a raw pointer being decoded
    FGfxP, FGfxN: LongWord;       // i32: the ScreenRes fill cursor and counter

    // --- graphics -------------------------------------------------------
    FUsesGfx: Boolean;
    FScrW, FScrH: LongWord;       // WASM globals holding the screen geometry
    FFbBase: LongWord;            // WASM global: where SCREENRES put the framebuffer

    // --- strings and the heap -------------------------------------------
    FUsesStr: Boolean;            // the program has string values
    FUsesHeap: Boolean;           // ... or graphics: either way it allocates
    FHeapTop: LongWord;           // WASM global: the bump pointer
    FHeapBase: LongWord;          // where the bump starts, after the literals
    FConstId: array of Integer;   // pool ids of the literals, in layout order
    FConstAddr: array of LongWord;
    FConstBytes: AnsiString;      // the data segment, already laid out
    FAllocFunc, FStrNewFunc, FStrCatFunc, FStrSubFunc, FStrCmpFunc,
    FStrAscFunc, FStrChrFunc, FStrRightFunc, FStrMidFunc,
    FPrintStrFunc: LongWord;

    // --- arrays ---------------------------------------------------------
    FUsesArr: Boolean;
    FUsesRec: Boolean;              // the program builds UDT records
    FArrDescOf: array of LongWord;   // array index -> its descriptor's address
    FArrTmp: LongWord;               // i32 scratch: the running element product
    FRecTmp: LongWord;               // i32 scratch: a record handle being addressed
    FArrLoad, FArrStore: array[TSSARegisterType] of LongWord;
    FArrLBoundFunc, FArrUBoundFunc: LongWord;

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
    function ConstAddrOf(const V: TSSAValue): LongWord;
    function ExtraOperands(Instr: TSSAInstruction): TSSAValueArray;
    procedure EmitArrayHelpers;
    procedure EmitRawAddr(B: TWasmBuf);
    procedure EmitPrintHelpers;
    procedure EmitHeapHelpers;
    procedure EmitStringHelpers;
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

  { ---- the linear memory map ----------------------------------------------

    0 .. 3      the EMPTY STRING, and it costs nothing. A string handle is the
                address of a [i32 len][bytes] header, linear memory starts
                zeroed, and a fresh WASM local is 0 - so handle 0 reads as
                length 0, which is exactly what an unassigned BASIC string is.
                ⛔ Nothing may ever be written there.
    4 .. 63     the PRINT digit scratch, built BACKWARDS from SCRATCH_END.
    64, 65      a literal ' ' and a literal LF.
    1024 ..     the string LITERALS, one [i32 len][bytes] header each, laid out
                at compile time into one data segment.
    FHeapBase   where the bump allocator starts - the first 4-aligned address
                after the literals. }
  EMPTY_STR    = 0;
  SCRATCH_END  = 64;      // one past the last byte of the digit scratch
  CONST_SPACE  = 64;      // a literal ' '
  CONST_NL     = 65;      // a literal LF
  STR_CONST_BASE = 1024;  // the first string literal

  { The two REGIONS a tagged raw pointer can name (SedaiSSATypes): region 0 is
    the byte heap Allocate returns, region 1 the framebuffer SCREENPTR returns.
    A pointer carries a byte OFFSET, never an address, so each region has a base
    and the offset is added to it. Region 0's base is FHeapBase; region 1's is a
    GLOBAL, because the framebuffer is now bump-allocated by SCREENRES like
    everything else. ⭐ That is what keeps strings and graphics from colliding:
    a fixed FB_BASE right above the heap capped the heap at one page, and a
    fixed base above a growing heap cannot exist when the framebuffer's size is
    only known at run time. One allocator, one arena, no partition to get wrong. }
  { The framebuffer's initial contents are not zero: a fresh ScreenRes fills
    every pixel with $000000FF (ClearCurrentMode, SedaiGraphicsMemory), and
    linear memory starts zeroed, so the fill has to be emitted or the very first
    byte of every comparison would differ.
    ⚠️ MEASURED TWICE. The first reading said $FF000000, because the probe
    PRINTED before reading it back - and in graphics mode PRINT renders into the
    framebuffer, so the probe was reading its own output. Any measurement of the
    initial buffer has to happen before the first character is printed. }
  FB_CLEAR     = 255;          // $000000FF

{ ---------------- PRINT ----------------

  The host import is a BYTE SINK - write(ptr, len) - and nothing else. The
  formatting is ours, emitted here, because BASIC's number spacing is a dialect
  rule (TConsoleBehavior.FormatInt): a leading space stands in for the sign when
  the value is non-negative, and Commodore adds a trailing space where FreeBASIC
  does not. Handing an i64 to JS and letting it call String(n) would produce
  output that differs from the native run in exactly the places this project
  measures byte for byte - the plan rules that out, and it is the whole reason
  the sink is this narrow. }

procedure TWasmBackend.EmitRawAddr(B: TWasmBuf);
{ Takes a tagged raw pointer (i64) off the stack and leaves the linear address
  (i32) it names. The pointer carries a byte OFFSET in its low 61 bits and a
  region selector in bit 61, so the address is offset + the region's base -
  chosen with a select rather than a branch. }
begin
  B.LocalTee(FRawTmp);
  B.I64Const(RAWPTR_OFS_MASK);
  B.Op(wopI64And);
  B.Op(wopI32WrapI64);
  B.GlobalGet(FFbBase);
  B.I32Const(LongInt(FHeapBase));
  B.LocalGet(FRawTmp);
  B.I64Const(RAWPTR_REGION_FB);
  B.Op(wopI64And);
  B.Op(wopI64Eqz);
  B.Op(wopI32Eqz);          // 1 when the region bit is set -> the framebuffer
  B.Op(wopSelect);
  B.Op(wopI32Add);
end;

procedure TWasmBackend.ScanForPrint;
{ One pass that answers everything the module SHAPE depends on: does the program
  print, does it draw, does it hold strings - and which literals it holds, which
  have to be laid out before the first byte of code is emitted because their
  addresses are immediates.
  ⚠️ The order matters more than it looks: imports own the low function indices,
  so what the module imports has to be known before anything is defined. }
var
  i, j, k: Integer;
  Blk: TSSABasicBlock;
  Ins: TSSAInstruction;
  Addr: LongWord;
  S: AnsiString;

  procedure NoteConst(const V: TSSAValue);
  var
    m: Integer;
  begin
    if V.Kind <> svkConstString then Exit;
    for m := 0 to High(FConstId) do
      if FConstId[m] = V.ConstStringId then Exit;   // the pool already dedups
    SetLength(FConstId, Length(FConstId) + 1);
    FConstId[High(FConstId)] := V.ConstStringId;
  end;

begin
  FUsesPrint := False;
  FUsesGfx := False;
  FUsesStr := False;
  FUsesArr := False;
  FUsesRec := False;
  SetLength(FConstId, 0);
  for i := 0 to FProg.Blocks.Count - 1 do
  begin
    Blk := FProg.Blocks[i];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Ins := TSSAInstruction(Blk.Instructions[j]);
      case Ins.OpCode of
        ssaPrintInt, ssaPrintIntLn, ssaPrintNewLine, ssaPrintUInt:
          FUsesPrint := True;
        ssaPrintString, ssaPrintStringLn:
          begin FUsesPrint := True; FUsesStr := True; end;
        ssaGfxScreenRes, ssaGfxScreenPtr, ssaGfxScreenInfo,
        ssaRawLoadInt, ssaRawStoreInt:
          FUsesGfx := True;
        ssaLoadConstString, ssaStrConcat, ssaStrLen, ssaStrLeft, ssaStrRight,
        ssaStrMid, ssaStrAsc, ssaStrAscMid, ssaStrChr,
        ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString:
          FUsesStr := True;
        ssaArrayDim, ssaArrayLoad, ssaArrayStore, ssaArrayLBound, ssaArrayUBound:
          FUsesArr := True;
        ssaRecordNew:
          FUsesRec := True;
        ssaRecordLoadString, ssaRecordStoreString:
          begin FUsesRec := True; FUsesStr := True; end;
      end;
      NoteConst(Ins.Src1); NoteConst(Ins.Src2); NoteConst(Ins.Src3);
    end;
  end;
  FUsesHeap := FUsesStr or FUsesGfx or FUsesArr or FUsesRec;

  // Lay the literals out: one [i32 len][bytes] header each, 4-aligned so a
  // handle is always aligned the way an i32.load wants it.
  FConstBytes := '';
  SetLength(FConstAddr, Length(FConstId));
  Addr := STR_CONST_BASE;
  for k := 0 to High(FConstId) do
  begin
    FConstAddr[k] := Addr;
    S := AnsiString(SSAPoolGet(FConstId[k]));
    SetLength(FConstBytes, Length(FConstBytes) + 4 + Length(S));
    PLongWord(@FConstBytes[Length(FConstBytes) - 3 - Length(S)])^ := LongWord(Length(S));
    if Length(S) > 0 then
      Move(S[1], FConstBytes[Length(FConstBytes) - Length(S) + 1], Length(S));
    Inc(Addr, LongWord(4 + Length(S)));
    while (Addr and 3) <> 0 do
    begin
      FConstBytes := FConstBytes + #0;
      Inc(Addr);
    end;
  end;

  { The ARRAY DESCRIPTORS come next, one per declared array, and their addresses
    are CONSTANTS - the array index is a compile-time number in every operand
    that names one, so nothing has to be looked up at run time.
      +0 base   the element block (0 before DIM)
      +4 total  element count, which is also the bounds check
      +8 dims   how many dimensions are allocated (0 before DIM)
      +16.. (lb, size) per dimension
    ⭐ Descriptors, not one global per array, because LBOUND/UBOUND take the
    dimension in a REGISTER: the helper has to index the bounds at run time, and
    a global cannot be indexed. }
  SetLength(FArrDescOf, FProg.GetArrayCount);
  if FUsesArr then
    for k := 0 to FProg.GetArrayCount - 1 do
    begin
      FArrDescOf[k] := Addr;
      Inc(Addr, LongWord(16 + 8 * FProg.GetArray(k).DimCount));
      while (Addr and 3) <> 0 do Inc(Addr);
    end;
  FHeapBase := Addr;
end;

function TWasmBackend.ExtraOperands(Instr: TSSAInstruction): TSSAValueArray;
{ The operands an instruction READS without naming them in Src1..Src3.

  ⛔ ssaArrayDim is the whole reason this exists. Its bounds do not travel in its
  operand slots: they live in the program's ARRAY METADATA as bare register
  NUMBERS, and the instruction carries them only as PhiSources so that DCE will
  not delete the code that computes them. Every walk over operands is therefore
  blind to them - and the failure is silent, because the array still allocates,
  just with whatever happened to be in a local that was never given a value.
  So there is one place that knows about them, and the three walks ask it. }
var
  Info: TSSAArrayInfo;
  d: Integer;

  procedure Add(RT: TSSARegisterType; Idx: Integer);
  begin
    if Idx < 0 then Exit;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := MakeSSARegister(RT, Idx);
  end;

begin
  SetLength(Result, 0);
  if Instr.OpCode <> ssaArrayDim then Exit;
  if Instr.Src1.Kind <> svkArrayRef then Exit;
  if (Instr.Src1.ArrayIndex < 0) or (Instr.Src1.ArrayIndex >= FProg.GetArrayCount) then Exit;
  Info := FProg.GetArray(Instr.Src1.ArrayIndex);
  for d := 0 to Info.DimCount - 1 do
  begin
    if (d <= High(Info.Dimensions)) and (Info.Dimensions[d] = 0) and
       (d <= High(Info.DimRegisters)) then
      Add(Info.DimRegTypes[d], Info.DimRegisters[d]);
    if d <= High(Info.LowerBoundRegisters) then
      Add(srtInt, Info.LowerBoundRegisters[d]);
  end;
end;

function TWasmBackend.ConstAddrOf(const V: TSSAValue): LongWord;
var
  k: Integer;
begin
  for k := 0 to High(FConstId) do
    if FConstId[k] = V.ConstStringId then Exit(FConstAddr[k]);
  Result := EMPTY_STR;      // unreachable: ScanForPrint saw every operand
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

{ ---------------- the heap ----------------

  A BUMP allocator and nothing more: one global cursor, round up to 4, grow the
  memory when the cursor passes it. It never frees, and that is a stated v1
  limit rather than an oversight - a loop that builds strings consumes memory
  until the module hits the 4 GB ceiling. The alternative (a free list, or
  reference counting on the string handles) is a real piece of runtime, and it
  is worth writing only once there is a program that needs it. ⛔ What must NOT
  happen meanwhile is emitting something that runs and lies: running out of
  memory here is a trap, which is loud. }

procedure TWasmBackend.EmitHeapHelpers;
var
  B: TWasmBuf;
begin
  B := TWasmBuf.Create;
  try
    // alloc(n: i32) -> i32.  locals: 1 = the block, 2 = pages wanted
    B.GlobalGet(FHeapTop);
    B.LocalTee(1);
    B.LocalGet(0); B.Op(wopI32Add);
    B.I32Const(3); B.Op(wopI32Add);
    B.I32Const(-4); B.Op(wopI32And);
    B.GlobalSet(FHeapTop);

    B.GlobalGet(FHeapTop);
    B.I32Const(65535); B.Op(wopI32Add);
    B.I32Const(65536); B.Op(wopI32DivU);
    B.LocalTee(2);
    B.Op(wopMemorySize); B.U8(0);
    B.Op(wopI32GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(2);
      B.Op(wopMemorySize); B.U8(0);
      B.Op(wopI32Sub);
      B.Op(wopMemoryGrow); B.U8(0);
      B.Op(wopDrop);
    B.EndOp;

    B.LocalGet(1);
    FModule.AddFunction(FModule.TypeIndex([wvtI32], [wvtI32]), [wvtI32, wvtI32], B);
  finally
    B.Free;
  end;
end;

{ ---------------- strings ----------------

  A string VALUE is [i32 len][len bytes] in linear memory, and a string REGISTER
  holds its address. Three consequences, and the whole design is in them:

  1. ⭐ Handle 0 is the empty string for free. Linear memory starts zeroed and a
     fresh WASM local is 0, so a BASIC string variable that was never assigned
     reads as length 0 without a line of code. The four bytes at address 0 are
     reserved for that and never written.
  2. Strings are IMMUTABLE here: every operation allocates its result and
     ssaCopyString copies the handle. That is sound only because nothing mutates
     one in place - and two opcodes do (ssaStrAppendMapped grows its Dest,
     ssaStrMidAssign overwrites inside its buffer). They stay REFUSED, and the
     refusal names the reason. ⛔ Covering them by aliasing would produce a
     module that runs and prints the wrong string, which is the one outcome this
     backend may not have.
  3. Every rule below is the interpreter's, read out of SedaiBytecodeVM rather
     than reasoned about: LEFT's negative length, RIGHT's clamp, MID's two
     dialect-dependent rules, ASC of an empty string. They are what the
     differential compares. }

procedure TWasmBackend.EmitStringHelpers;
var
  B: TWasmBuf;
  TNewStr, TCat, TSub, TCmp, TAsc, TChr, TRight, TMid, TPrint: LongWord;
begin
  TNewStr := FModule.TypeIndex([wvtI32], [wvtI32]);
  TCat    := FModule.TypeIndex([wvtI32, wvtI32], [wvtI32]);
  TSub    := FModule.TypeIndex([wvtI32, wvtI64, wvtI64], [wvtI32]);
  TCmp    := FModule.TypeIndex([wvtI32, wvtI32], [wvtI32]);
  TAsc    := FModule.TypeIndex([wvtI32], [wvtI64]);
  TChr    := FModule.TypeIndex([wvtI64], [wvtI32]);
  TRight  := FModule.TypeIndex([wvtI32, wvtI64], [wvtI32]);
  TMid    := FModule.TypeIndex([wvtI32, wvtI64, wvtI64], [wvtI32]);
  TPrint  := FModule.TypeIndex([wvtI32], []);

  { strNew(len: i32) -> i32: an uninitialised header of that length, or the
    canonical empty string when the length is zero. }
  B := TWasmBuf.Create;
  try
    B.LocalGet(0); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(EMPTY_STR); B.LocalSet(1);
    B.Op(wopElse);
      B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add);
      B.Call(FAllocFunc);
      B.LocalTee(1);
      B.LocalGet(0);
      B.OpMem(wopI32Store, 2, 0);
    B.EndOp;
    B.LocalGet(1);
    FModule.AddFunction(TNewStr, [wvtI32], B);
  finally
    B.Free;
  end;

  { strCat(a, b) -> a + b. A zero length copy is legal and cannot trap, so the
    two copies need no guard. }
  B := TWasmBuf.Create;
  try
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(3);
    B.LocalGet(1); B.OpMem(wopI32Load, 2, 0); B.LocalSet(4);
    B.LocalGet(3); B.LocalGet(4); B.Op(wopI32Add);
    B.Call(FStrNewFunc); B.LocalSet(2);

    B.LocalGet(2); B.I32Const(4); B.Op(wopI32Add);
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add);
    B.LocalGet(3);
    B.MemoryCopy;

    B.LocalGet(2); B.I32Const(4); B.Op(wopI32Add); B.LocalGet(3); B.Op(wopI32Add);
    B.LocalGet(1); B.I32Const(4); B.Op(wopI32Add);
    B.LocalGet(4);
    B.MemoryCopy;

    B.LocalGet(2);
    FModule.AddFunction(TCat, [wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { strSub(s, start, cnt) -> the substring, with AssignSubstr's clamping:
    a count of zero or less is empty, a start below 1 is 1, and a count past the
    end is cut to what is left.
    ⚠️ The order differs from the Pascal on purpose. AssignSubstr asks
    "Start + Cnt - 1 > Length(S)", which on 64-bit registers holding absurd
    values can WRAP; asking "is start past the end" first makes every later
    subtraction bounded. For every value a program can actually produce the two
    agree - and for the ones it cannot, this one does not wrap into a false
    answer. }
  B := TWasmBuf.Create;
  try
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32U); B.LocalSet(3);

    B.LocalGet(1); B.I64Const(1); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(1); B.LocalSet(1);
    B.EndOp;

    B.LocalGet(2); B.I64Const(0); B.Op(wopI64LeS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(EMPTY_STR); B.Op(wopReturn);
    B.EndOp;

    B.LocalGet(1); B.LocalGet(3); B.Op(wopI64GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(EMPTY_STR); B.Op(wopReturn);
    B.EndOp;

    // avail = len - start + 1, which is now in 1..len
    B.LocalGet(3); B.LocalGet(1); B.Op(wopI64Sub); B.I64Const(1); B.Op(wopI64Add);
    B.LocalSet(6);
    B.LocalGet(2); B.LocalGet(6); B.Op(wopI64GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(6); B.LocalSet(2);
    B.EndOp;

    B.LocalGet(2); B.Op(wopI32WrapI64); B.LocalSet(4);
    B.LocalGet(4); B.Call(FStrNewFunc); B.LocalSet(5);
    B.LocalGet(5); B.I32Const(4); B.Op(wopI32Add);
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add);
      B.LocalGet(1); B.Op(wopI32WrapI64); B.Op(wopI32Add);
      B.I32Const(1); B.Op(wopI32Sub);
    B.LocalGet(4);
    B.MemoryCopy;
    B.LocalGet(5);
    FModule.AddFunction(TSub, [wvtI64, wvtI32, wvtI32, wvtI64], B);
  finally
    B.Free;
  end;

  { strCmp(a, b) -> -1 / 0 / 1. FPC compares AnsiStrings byte by byte as
    UNSIGNED characters and settles a tie by length, and the four BASIC string
    comparisons are built on that. }
  B := TWasmBuf.Create;
  try
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(2);
    B.LocalGet(1); B.OpMem(wopI32Load, 2, 0); B.LocalSet(3);
    B.LocalGet(2); B.LocalSet(4);
    B.LocalGet(3); B.LocalGet(4); B.Op(wopI32LtU);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(3); B.LocalSet(4);
    B.EndOp;
    B.I32Const(0); B.LocalSet(5);

    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(5); B.LocalGet(4); B.Op(wopI32GeU); B.BrIf(1);
        B.LocalGet(0); B.LocalGet(5); B.Op(wopI32Add);
          B.OpMem(wopI32Load8U, 0, 4); B.LocalSet(6);
        B.LocalGet(1); B.LocalGet(5); B.Op(wopI32Add);
          B.OpMem(wopI32Load8U, 0, 4); B.LocalSet(7);
        B.LocalGet(6); B.LocalGet(7); B.Op(wopI32Ne);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(6); B.LocalGet(7); B.Op(wopI32LtU);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.I32Const(-1); B.Op(wopReturn);
          B.Op(wopElse);
            B.I32Const(1); B.Op(wopReturn);
          B.EndOp;
        B.EndOp;
        B.LocalGet(5); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(5);
        B.Br(0);
      B.EndOp;
    B.EndOp;

    B.LocalGet(2); B.LocalGet(3); B.Op(wopI32Eq);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(2); B.LocalGet(3); B.Op(wopI32LtU);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(-1); B.Op(wopReturn);
    B.EndOp;
    B.I32Const(1);
    FModule.AddFunction(TCmp, [wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { strAsc(s) -> the first byte, and 0 for an empty string (bcStrAsc). }
  B := TWasmBuf.Create;
  try
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(0); B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(0); B.OpMem(wopI32Load8U, 0, 4); B.Op(wopI64ExtendI32U);
    FModule.AddFunction(TAsc, [], B);
  finally
    B.Free;
  end;

  { strChr(code) -> the one-character string; the code is taken AND $FF, as
    AssignChar's caller does. }
  B := TWasmBuf.Create;
  try
    B.I32Const(1); B.Call(FStrNewFunc); B.LocalTee(1);
    B.LocalGet(0); B.Op(wopI32WrapI64); B.I32Const(255); B.Op(wopI32And);
    B.OpMem(wopI32Store8, 0, 4);
    B.LocalGet(1);
    FModule.AddFunction(TChr, [wvtI32], B);
  finally
    B.Free;
  end;

  { strRight(s, n): a negative n is 0, an n past the end is the whole string,
    and the start follows from what is left (bcStrRight). }
  B := TWasmBuf.Create;
  try
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32U); B.LocalSet(2);
    B.LocalGet(1); B.I64Const(0); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(0); B.LocalSet(1);
    B.EndOp;
    B.LocalGet(1); B.LocalGet(2); B.Op(wopI64GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(2); B.LocalSet(1);
    B.EndOp;
    B.LocalGet(0);
    B.LocalGet(2); B.LocalGet(1); B.Op(wopI64Sub); B.I64Const(1); B.Op(wopI64Add);
    B.LocalGet(1);
    B.Call(FStrSubFunc);
    FModule.AddFunction(TRight, [wvtI64], B);
  finally
    B.Free;
  end;

  { strMid(s, start, cnt) - the one helper whose RULES DIFFER BY DIALECT, and
    the difference is not cosmetic (bcStrMid):
      FreeBASIC - a start below 1 yields an EMPTY string, and a NEGATIVE count
                  means "all the rest";
      Commodore - a start below 1 clamps to 1, and a negative count is 0.
    Both were found by programs that got the wrong answer, so both are baked in
    here at emit time rather than decided at run time. }
  B := TWasmBuf.Create;
  try
    if FModern then
    begin
      B.LocalGet(1); B.I64Const(1); B.Op(wopI64LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(EMPTY_STR); B.Op(wopReturn);
      B.EndOp;
      B.LocalGet(2); B.I64Const(0); B.Op(wopI64LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32U);
        B.LocalGet(1); B.Op(wopI64Sub); B.I64Const(1); B.Op(wopI64Add);
        B.LocalSet(2);
        B.LocalGet(2); B.I64Const(0); B.Op(wopI64LtS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I64Const(0); B.LocalSet(2);
        B.EndOp;
      B.EndOp;
    end
    else
    begin
      B.LocalGet(1); B.I64Const(1); B.Op(wopI64LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I64Const(1); B.LocalSet(1);
      B.EndOp;
      B.LocalGet(2); B.I64Const(0); B.Op(wopI64LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I64Const(0); B.LocalSet(2);
      B.EndOp;
    end;
    B.LocalGet(0); B.LocalGet(1); B.LocalGet(2);
    B.Call(FStrSubFunc);
    FModule.AddFunction(TMid, [], B);
  finally
    B.Free;
  end;

  { printStr(s): the bytes straight into the sink. No formatting - there is
    none to do for a string, which is exactly why PRINT of a string is cheap
    where PRINT of a number was a whole formatter. }
  if FUsesPrint then
  begin
    B := TWasmBuf.Create;
    try
      B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add);
      B.LocalGet(0); B.OpMem(wopI32Load, 2, 0);
      B.Call(FWriteFunc);
      FModule.AddFunction(TPrint, [], B);
    finally
      B.Free;
    end;
  end;
end;

{ ---------------- arrays ----------------

  Elements live in one block from the same bump allocator, at a STRIDE OF 8 for
  every bank - i64, f64 and a string handle all get eight bytes. The waste on the
  string bank is four bytes an element and it buys one index computation instead
  of three.

  ⭐ Nothing zeroes a fresh array, and that is not an omission: the bump
  allocator never reuses, linear memory starts zeroed, and memory.grow hands out
  zeroed pages - so a block that has just been allocated IS zero, which is what
  bcArrayDim writes into it explicitly. ⛔ The day the allocator learns to free,
  this stops being true and DIM has to zero.

  ⚠️ THE BOUNDS RULE IS DIALECT-DEPENDENT and it is not a detail:
    FreeBASIC - no check. A read out of bounds yields the DEFAULT and a write is
                DROPPED. (Real fbc would touch adjacent heap; the interpreter
                chose memory safety, and this follows the interpreter.)
    Commodore  - ?BAD SUBSCRIPT, an error that stops the program.
  Here the CLASSIC arm traps, which is the loud equivalent of the interpreter
  raising - the same choice ssaModFloat made for division by zero. }

procedure TWasmBackend.EmitArrayHelpers;
var
  B: TWasmBuf;
  RT: TSSARegisterType;
  TLoad, TStore, TBound: LongWord;

  procedure BoundsTest;
  // leaves i32 "0 <= idx < total" on the stack; locals 0 = desc, 1 = idx
  begin
    B.LocalGet(1); B.I64Const(0); B.Op(wopI64GeS);
    B.LocalGet(1);
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 4); B.Op(wopI64ExtendI32S);
    B.Op(wopI64LtS);
    B.Op(wopI32And);
  end;

  procedure ElemAddr;
  // leaves base + idx*8; locals 0 = desc, 1 = idx
  begin
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0);
    B.LocalGet(1); B.Op(wopI32WrapI64); B.I32Const(8); B.Op(wopI32Mul);
    B.Op(wopI32Add);
  end;

begin
  for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
  begin
    TLoad := FModule.TypeIndex([wvtI32, wvtI64], [BankType[RT]]);
    B := TWasmBuf.Create;
    try
      BoundsTest;
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        ElemAddr;
        case RT of
          srtFloat:  B.OpMem(wopF64Load, 3, 0);
          srtString: B.OpMem(wopI32Load, 2, 0);
        else
          B.OpMem(wopI64Load, 3, 0);
        end;
        B.Op(wopReturn);
      B.EndOp;
      if FModern then
        case RT of                       // FreeBASIC: the element type's default
          srtFloat:  B.F64Const(0);
          srtString: B.I32Const(EMPTY_STR);
        else
          B.I64Const(0);
        end
      else
        B.Op(wopUnreachable);            // Commodore: ?BAD SUBSCRIPT
      FArrLoad[RT] := FModule.AddFunction(TLoad, [], B);
    finally
      B.Free;
    end;

    TStore := FModule.TypeIndex([wvtI32, wvtI64, BankType[RT]], []);
    B := TWasmBuf.Create;
    try
      BoundsTest;
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        ElemAddr;
        B.LocalGet(2);
        case RT of
          srtFloat:  B.OpMem(wopF64Store, 3, 0);
          srtString: B.OpMem(wopI32Store, 2, 0);
        else
          B.OpMem(wopI64Store, 3, 0);
        end;
      B.Op(wopElse);
        if not FModern then B.Op(wopUnreachable);   // FreeBASIC drops it silently
      B.EndOp;
      FArrStore[RT] := FModule.AddFunction(TStore, [], B);
    finally
      B.Free;
    end;
  end;

  TBound := FModule.TypeIndex([wvtI32, wvtI64], [wvtI64]);

  { LBOUND(arr, d). ⭐ d BELOW ZERO is not an error: it is FreeBASIC's "how many
    dimensions" query, written LBOUND(arr, 0), and the answer is always 1. }
  B := TWasmBuf.Create;
  try
    B.LocalGet(1); B.I64Const(0); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(1); B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(0);
    B.LocalGet(1); B.Op(wopI32WrapI64); B.I32Const(8); B.Op(wopI32Mul);
    B.Op(wopI32Add);
    B.OpMem(wopI32Load, 2, 16);
    B.Op(wopI64ExtendI32S);
    FArrLBoundFunc := FModule.AddFunction(TBound, [], B);
  finally
    B.Free;
  end;

  { UBOUND(arr, d) = lb + size - 1, and the same query at d < 0 answers with the
    number of ALLOCATED dimensions - 0 for a dynamic array not dimensioned yet,
    which is also why UBOUND of one answers -1: lb 0 plus size 0 minus one. }
  B := TWasmBuf.Create;
  try
    B.LocalGet(1); B.I64Const(0); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(0); B.OpMem(wopI32Load, 2, 8); B.Op(wopI64ExtendI32S);
      B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(0);
    B.LocalGet(1); B.Op(wopI32WrapI64); B.I32Const(8); B.Op(wopI32Mul);
    B.Op(wopI32Add);
    B.LocalTee(2);
    B.OpMem(wopI32Load, 2, 16);
    B.LocalGet(2);
    B.OpMem(wopI32Load, 2, 20);
    B.Op(wopI32Add);
    B.I32Const(1); B.Op(wopI32Sub);
    B.Op(wopI64ExtendI32S);
    FArrUBoundFunc := FModule.AddFunction(TBound, [wvtI32], B);
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
  r, i, j, k, b, s, f, N, First, Last, Idx: Integer;
  Blk, Succ: TSSABasicBlock;
  Instr: TSSAInstruction;
  Extras: TSSAValueArray;
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
      Extras := ExtraOperands(Instr);
      for k := 0 to High(Extras) do MarkUse(Extras[k], i);
      { ⛔ ssaArrayStore's Dest is the VALUE BEING STORED - a read, not a
        definition (the VM writes IntData[idx] := IntRegs[Instr.Dest]). Killing
        it here would say "this register is written before it is read", so a
        value that really does cross into a procedure would be judged not
        live-in, never promoted to a global, and read as whatever that region's
        own local happened to hold. Silent, and only in programs with both an
        array store and a shared register. }
      if Instr.OpCode = ssaArrayStore then
        MarkUse(Instr.Dest, i)
      else
      begin
        Idx := FlatId(Instr.Dest);
        if Idx >= 0 then Kill[i][Idx] := True;
      end;
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
  i, j, k, r: Integer;
  RT: TSSARegisterType;
  Blk: TSSABasicBlock;
  Instr: TSSAInstruction;
  Extras: TSSAValueArray;
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
      Extras := ExtraOperands(Instr);
      for k := 0 to High(Extras) do NoteRegister(Extras[k], r);
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
      Extras := ExtraOperands(Instr);
      for k := 0 to High(Extras) do
        if FlatId(Extras[k]) >= 0 then FRegionUses[r][FlatId(Extras[k])] := True;
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
  Slot, d, Bytes, NStr, StrBase: Integer;
  Enc: Int64;
  Ofs: LongWord;
  RT: TSSARegisterType;
  Info: TSSAArrayInfo;
  Desc: LongWord;
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

    { ---- strings ------------------------------------------------------ }

    ssaLoadConstString:
      begin
        if Instr.Src1.Kind <> svkConstString then
          Exit(Fail('ssaLoadConstString without a string constant'));
        B.I32Const(LongInt(ConstAddrOf(Instr.Src1)));
        StoreReg(B, Instr.Dest);
      end;

    ssaPrintString:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FPrintStrFunc);
      end;
    ssaPrintStringLn:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FPrintStrFunc);
        B.Call(FPrintNlFunc);
      end;

    ssaStrLen:
      begin
        LoadReg(B, Instr.Src1);
        B.OpMem(wopI32Load, 2, 0);
        B.Op(wopI64ExtendI32U);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrConcat:
      begin
        // The three-operand form is the fused "a + b + c"; it has no arm here
        // yet, and a wrong answer is not on offer.
        if Instr.Src3.Kind <> svkNone then
          Exit(Fail('a three-operand ssaStrConcat is not covered yet'));
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        B.Call(FStrCatFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrLeft:
      begin
        // LEFT is MID from 1: a negative length falls out as empty through
        // strSub's own "count <= 0" rule, exactly as bcStrLeft does.
        LoadReg(B, Instr.Src1);
        B.I64Const(1);
        LoadReg(B, Instr.Src2);
        B.Call(FStrSubFunc);
        StoreReg(B, Instr.Dest);
      end;
    ssaStrRight:
      begin
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        B.Call(FStrRightFunc);
        StoreReg(B, Instr.Dest);
      end;
    ssaStrMid:
      begin
        if Instr.Src3.Kind <> svkRegister then
          Exit(Fail('ssaStrMid without a length register'));
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        LoadReg(B, Instr.Src3);
        B.Call(FStrMidFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrAsc:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FStrAscFunc);
        StoreReg(B, Instr.Dest);
      end;
    { ASC(MID$(s, start, len)) fused by the SSA into one instruction - and the
      idiom is unavoidable, since "read character i" is written that way in
      every BASIC program that walks a string.
      ⭐ It is emitted as strMid followed by strAsc rather than as a fourth copy
      of MID's dialect rules. The interpreter keeps a hand-written arm here and
      its own comment warns that the two must not drift apart; composing them
      makes drifting impossible. What that costs is the substring the fusion
      exists to avoid - a real optimisation, and one that can only be made
      later, on top of something known to be right. }
    ssaStrAscMid:
      begin
        if Instr.Src3.Kind <> svkRegister then
          Exit(Fail('ssaStrAscMid without a length register'));
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        LoadReg(B, Instr.Src3);
        B.Call(FStrMidFunc);
        B.Call(FStrAscFunc);
        StoreReg(B, Instr.Dest);
      end;
    ssaStrChr:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FStrChrFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString:
      begin
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        B.Call(FStrCmpFunc);
        case Instr.OpCode of
          ssaCmpEqString: B.Op(wopI32Eqz);
          ssaCmpNeString: begin B.I32Const(0); B.Op(wopI32Ne); end;
          ssaCmpLtString: begin B.I32Const(0); B.Op(wopI32LtS); end;
        else
          begin B.I32Const(0); B.Op(wopI32GtS); end;
        end;
        BoolToBasic(B);
        StoreReg(B, Instr.Dest);
      end;

    { ⛔ The two in-place string opcodes. They are not "not written yet": they
      are incompatible with handles that alias, which is what ssaCopyString
      makes. Covering them wants either a copy on write or a real ownership
      model, and until then a refusal is the only honest answer. }
    ssaStrAppendMapped, ssaStrMidAssign, ssaStrConcatCharAt:
      Exit(Fail(Format('%s mutates a string in place, and in this backend a ' +
                       'string handle can be shared by several registers - ' +
                       'covering it needs copy-on-write first (line %d)',
                       [OpName(Instr.OpCode), Instr.SourceLine])));

    { ---- UDT / records -------------------------------------------------

      A record is a BYTE IMAGE with fbc's layout, not a bag of slots - that is
      what makes SizeOf, OffsetOf and a union that aliases across banks come out
      right, and it is why a field access carries an ENCODED offset rather than
      an index. The encoding is the interpreter's (RecFieldInt): the byte offset
      is Enc shr 4, and the low nibble is the width - 0 a full i64, 1..6 the
      signed/unsigned 8/16/32 forms, 7 a SINGLE.

      Strings cannot live in that image (a handle is not the value), so they sit
      in their own area after it, and the header says where:
        +0 typeId · +4 the byte offset of the string area · +8 the image }

    ssaRecordNew:
      begin
        if (Instr.Src1.Kind <> svkConstInt) or (Instr.Src3.Kind <> svkConstInt) then
          Exit(Fail('ssaRecordNew without its compile-time sizes'));
        Bytes := Integer(Instr.Src1.ConstInt);
        NStr := Integer(Instr.Src3.ConstInt and $FFFF);
        StrBase := 8 + ((Bytes + 7) div 8) * 8;
        B.I32Const(StrBase + 4 * NStr);
        B.Call(FAllocFunc);
        B.LocalTee(FRecTmp);
        B.I32Const(Integer((Instr.Src3.ConstInt shr 32) and $FFFF));
        B.OpMem(wopI32Store, 2, 0);                 // typeId
        B.LocalGet(FRecTmp);
        B.I32Const(StrBase);
        B.OpMem(wopI32Store, 2, 4);                 // where the strings start
        // the handle lives in an INT register, so it travels as an i64
        B.LocalGet(FRecTmp);
        B.Op(wopI64ExtendI32U);
        StoreReg(B, Instr.Dest);
        { ⭐ Nothing zeroes the image, for the same reason DIM does not zero an
          array: bump-allocated memory has never been written, and linear memory
          and every grown page start at zero. A string slot of 0 is the empty
          string, which is what an unassigned one has to be. }
      end;

    ssaRecordTypeId:
      begin
        LoadReg(B, Instr.Src1);
        B.OpMem(wopI32Load, 2, 0);
        B.Op(wopI64ExtendI32S);
        StoreReg(B, Instr.Dest);
      end;

    { ⛔ DELETE is a NO-OP here and that is a stated consequence of the bump
      allocator, not an oversight: nothing is ever freed. It differs from the
      interpreter only for a program that uses a handle AFTER deleting it, which
      is undefined there too - and it errs toward keeping memory alive rather
      than reading something that has been handed to someone else. The same goes
      for the block-scoped reclamation marks. }
    ssaRecordFree, ssaRecMarkPush, ssaRecMarkPop: ;

    ssaRecordLoadInt, ssaRecordLoadFloat, ssaRecordStoreInt, ssaRecordStoreFloat:
      begin
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('a record field access without a constant field encoding'));
        Enc := Instr.Src3.ConstInt;
        LoadReg(B, Instr.Src1);                     // the handle
        B.Op(wopI32WrapI64);
        if OpIn(Instr.OpCode, [ssaRecordStoreInt, ssaRecordStoreFloat]) then
          LoadReg(B, Instr.Src2);                   // the value
        Ofs := LongWord(8 + (Enc shr 4));
        case Instr.OpCode of
          ssaRecordLoadInt:
            case Enc and $F of
              1: B.OpMem(wopI64Load8S, 0, Ofs);
              2: B.OpMem(wopI64Load8U, 0, Ofs);
              3: B.OpMem(wopI64Load16S, 0, Ofs);
              4: B.OpMem(wopI64Load16U, 0, Ofs);
              5: B.OpMem(wopI64Load32S, 0, Ofs);
              6: B.OpMem(wopI64Load32U, 0, Ofs);
            else
              B.OpMem(wopI64Load, 0, Ofs);
            end;
          ssaRecordStoreInt:
            case Enc and $F of
              1, 2: B.OpMem(wopI64Store8, 0, Ofs);
              3, 4: B.OpMem(wopI64Store16, 0, Ofs);
              5, 6: B.OpMem(wopI64Store32, 0, Ofs);
            else
              B.OpMem(wopI64Store, 0, Ofs);
            end;
          ssaRecordLoadFloat:
            if (Enc and $F) = 7 then
            begin
              B.OpMem(wopF32Load, 0, Ofs);          // a SINGLE really is 4 bytes
              B.Op(wopF64PromoteF32);
            end
            else
              B.OpMem(wopF64Load, 0, Ofs);
        else
          if (Enc and $F) = 7 then
          begin
            B.Op(wopF32DemoteF64);
            B.OpMem(wopF32Store, 0, Ofs);
          end
          else
            B.OpMem(wopF64Store, 0, Ofs);
        end;
        if OpIn(Instr.OpCode, [ssaRecordLoadInt, ssaRecordLoadFloat]) then
          StoreReg(B, Instr.Dest);
      end;

    ssaRecordLoadString, ssaRecordStoreString:
      begin
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('a record string field without a constant slot index'));
        // addr = handle + strBase + 4*slot, and strBase is read from the header
        // because the image's size is not known where the field is touched
        LoadReg(B, Instr.Src1);
        B.Op(wopI32WrapI64);
        B.LocalTee(FRecTmp);
        B.LocalGet(FRecTmp);
        B.OpMem(wopI32Load, 2, 4);
        B.Op(wopI32Add);
        if Instr.OpCode = ssaRecordStoreString then
        begin
          LoadReg(B, Instr.Src2);
          B.OpMem(wopI32Store, 2, LongWord(4 * Instr.Src3.ConstInt));
        end
        else
        begin
          B.OpMem(wopI32Load, 2, LongWord(4 * Instr.Src3.ConstInt));
          StoreReg(B, Instr.Dest);
        end;
      end;

    { ---- arrays ------------------------------------------------------- }

    ssaArrayDim:
      begin
        if Instr.Src1.Kind <> svkArrayRef then
          Exit(Fail('ssaArrayDim without an array reference'));
        if (Instr.Src1.ArrayIndex < 0) or (Instr.Src1.ArrayIndex >= FProg.GetArrayCount) then
          Exit(Fail('ssaArrayDim names an array that was never declared'));
        Info := FProg.GetArray(Instr.Src1.ArrayIndex);
        Desc := FArrDescOf[Instr.Src1.ArrayIndex];

        B.I32Const(1); B.LocalSet(FArrTmp);
        for d := 0 to Info.DimCount - 1 do
        begin
          // the lower bound: a RUNTIME register wins over the constant, which is
          // how "Dim a(LBound(m) To UBound(m))" gets its bounds from another array
          B.I32Const(LongInt(Desc + LongWord(16 + 8 * d)));
          if (d <= High(Info.LowerBoundRegisters)) and (Info.LowerBoundRegisters[d] >= 0) then
          begin
            LoadReg(B, MakeSSARegister(srtInt, Info.LowerBoundRegisters[d]));
            B.Op(wopI32WrapI64);
          end
          else if d <= High(Info.LowerBounds) then
            B.I32Const(Info.LowerBounds[d])
          else
            B.I32Const(0);
          B.OpMem(wopI32Store, 2, 0);

          // the size: a constant dimension, or ub - lb + 1 from its register
          B.I32Const(LongInt(Desc + LongWord(20 + 8 * d)));
          if (d <= High(Info.Dimensions)) and (Info.Dimensions[d] <> 0) then
            B.I32Const(Info.Dimensions[d])
          else
          begin
            if (d > High(Info.DimRegisters)) or (Info.DimRegisters[d] < 0) then
              Exit(Fail(Format('array "%s" has a variable dimension %d with no register',
                               [Info.Name, d])));
            LoadReg(B, MakeSSARegister(Info.DimRegTypes[d], Info.DimRegisters[d]));
            case Info.DimRegTypes[d] of
              srtInt:   B.Op(wopI32WrapI64);
              srtFloat: B.TruncSat(wopfcI32TruncSatF64S);   // the VM truncates
            else
              Exit(Fail(Format('array "%s" takes dimension %d from a string register',
                               [Info.Name, d])));
            end;
            B.I32Const(LongInt(Desc + LongWord(16 + 8 * d)));
            B.OpMem(wopI32Load, 2, 0);
            B.Op(wopI32Sub);
            B.I32Const(1); B.Op(wopI32Add);
          end;
          B.OpMem(wopI32Store, 2, 0);

          B.LocalGet(FArrTmp);
          B.I32Const(LongInt(Desc + LongWord(20 + 8 * d)));
          B.OpMem(wopI32Load, 2, 0);
          B.Op(wopI32Mul);
          B.LocalSet(FArrTmp);
        end;

        // An upper bound below the lower one gives a NEGATIVE count. Left alone
        // it would hand the bump allocator a negative size and walk the cursor
        // BACKWARDS over memory already handed out - so it is clamped, and the
        // array comes out empty, which is what a zero TotalSize means anyway.
        B.LocalGet(FArrTmp); B.I32Const(0); B.Op(wopI32LtS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(0); B.LocalSet(FArrTmp);
        B.EndOp;

        B.I32Const(LongInt(Desc + 4)); B.LocalGet(FArrTmp); B.OpMem(wopI32Store, 2, 0);
        B.I32Const(LongInt(Desc + 8)); B.I32Const(Info.DimCount); B.OpMem(wopI32Store, 2, 0);
        B.I32Const(LongInt(Desc));
        B.LocalGet(FArrTmp); B.I32Const(8); B.Op(wopI32Mul);
        B.Call(FAllocFunc);
        B.OpMem(wopI32Store, 2, 0);
      end;

    ssaArrayLoad:
      begin
        if Instr.Src1.Kind <> svkArrayRef then
          Exit(Fail('ssaArrayLoad without an array reference'));
        B.I32Const(LongInt(FArrDescOf[Instr.Src1.ArrayIndex]));
        LoadReg(B, Instr.Src2);
        B.Call(FArrLoad[Instr.Dest.RegType]);
        StoreReg(B, Instr.Dest);
      end;

    { ⚠️ Dest is the VALUE here, not a destination - the array and the index are
      in Src1 and Src2. It reads the way an assignment is written, not the way
      the other opcodes are. }
    ssaArrayStore:
      begin
        if Instr.Src1.Kind <> svkArrayRef then
          Exit(Fail('ssaArrayStore without an array reference'));
        B.I32Const(LongInt(FArrDescOf[Instr.Src1.ArrayIndex]));
        LoadReg(B, Instr.Src2);
        LoadReg(B, Instr.Dest);
        B.Call(FArrStore[Instr.Dest.RegType]);
      end;

    ssaArrayLBound, ssaArrayUBound:
      begin
        if Instr.Src1.Kind <> svkArrayRef then
          Exit(Fail('an array bound query without an array reference'));
        B.I32Const(LongInt(FArrDescOf[Instr.Src1.ArrayIndex]));
        LoadReg(B, Instr.Src2);
        if Instr.OpCode = ssaArrayLBound then B.Call(FArrLBoundFunc)
                                          else B.Call(FArrUBoundFunc);
        StoreReg(B, Instr.Dest);
      end;

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

    { ---- linear memory and SCREENPTR ---------------------------------- }

    ssaGfxScreenRes:
      begin
        if (Instr.Src3.Kind = svkConstInt) and (Instr.Src3.ConstInt > 1) then
          Exit(Fail('SCREENRES with more than one page is not modelled yet'));
        LoadReg(B, Instr.Src1); B.Op(wopI32WrapI64); B.GlobalSet(FScrW);
        LoadReg(B, Instr.Src2); B.Op(wopI32WrapI64); B.GlobalSet(FScrH);

        // The framebuffer comes out of the same bump allocator as everything
        // else - which is what lets a program hold strings AND draw. A second
        // SCREENRES allocates a second buffer and abandons the first: the
        // allocator never frees, and the interpreter reallocates there too.
        B.GlobalGet(FScrW); B.GlobalGet(FScrH); B.Op(wopI32Mul);
        B.I32Const(4); B.Op(wopI32Mul);
        B.Call(FAllocFunc);
        B.GlobalSet(FFbBase);

        // and fill it the way the interpreter does - NOT with zero
        B.GlobalGet(FFbBase); B.LocalSet(FGfxP);
        B.GlobalGet(FScrW); B.GlobalGet(FScrH); B.Op(wopI32Mul); B.LocalSet(FGfxN);
        B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
          B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(FGfxN); B.Op(wopI32Eqz); B.BrIf(1);
            B.LocalGet(FGfxP); B.I32Const(FB_CLEAR); B.OpMem(wopI32Store, 2, 0);
            B.LocalGet(FGfxP); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(FGfxP);
            B.LocalGet(FGfxN); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(FGfxN);
            B.Br(0);
          B.EndOp;
        B.EndOp;
      end;

    ssaGfxScreenPtr:
      begin
        // Offset 0 of the framebuffer region - but 0 when there is no screen,
        // because FreeBASIC answers 0 rather than a pointer that fails later.
        B.GlobalGet(FScrW);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I64Const(RAWPTR_TAG or RAWPTR_REGION_FB);
          StoreReg(B, Instr.Dest);
        B.Op(wopElse);
          B.I64Const(0);
          StoreReg(B, Instr.Dest);
        B.EndOp;
      end;

    ssaGfxScreenInfo:
      begin
        // 0=width 1=height 2=depth(32) 3=bytes per pixel(4) 4=pitch(w*4), else 0
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('__SCRINFO with a non-constant selector'));
        case Instr.Src3.ConstInt of
          0: begin B.GlobalGet(FScrW); B.Op(wopI64ExtendI32S); end;
          1: begin B.GlobalGet(FScrH); B.Op(wopI64ExtendI32S); end;
          2: B.I64Const(32);
          3: B.I64Const(4);
          4: begin
               B.GlobalGet(FScrW); B.I32Const(4); B.Op(wopI32Mul);
               B.Op(wopI64ExtendI32S);
             end;
        else
          B.I64Const(0);
        end;
        StoreReg(B, Instr.Dest);
      end;

    ssaRawLoadInt:
      begin
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('a raw load without a constant element type code'));
        LoadReg(B, Instr.Src1);
        EmitRawAddr(B);
        // every width SIGN-extends, exactly as RawLoadInt does - a ULong Ptr
        // deref comes back negative in the interpreter too, and the backend
        // reproduces the interpreter rather than correcting it
        case Instr.Src3.ConstInt of
          RTC_I8:  B.OpMem(wopI64Load8S, 0, 0);
          RTC_I16: B.OpMem(wopI64Load16S, 1, 0);
          RTC_I32: B.OpMem(wopI64Load32S, 2, 0);
        else
          B.OpMem(wopI64Load, 3, 0);
        end;
        StoreReg(B, Instr.Dest);
      end;

    ssaRawStoreInt:
      begin
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('a raw store without a constant element type code'));
        LoadReg(B, Instr.Src1);
        EmitRawAddr(B);
        LoadReg(B, Instr.Src2);
        case Instr.Src3.ConstInt of
          RTC_I8:  B.OpMem(wopI64Store8, 0, 0);
          RTC_I16: B.OpMem(wopI64Store16, 1, 0);
          RTC_I32: B.OpMem(wopI64Store32, 2, 0);
        else
          B.OpMem(wopI64Store, 3, 0);
        end;
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

    { Round a Double to what a SINGLE can hold and keep it in the float bank -
      which is exactly a demote followed by a promote. It is not a conversion to
      another type: the value stays a Double that happens to have lost the bits
      a Single cannot carry, and that lost precision is observable. }
    ssaNarrowSingle:
      begin
        LoadReg(B, Instr.Src1);
        B.Op(wopF32DemoteF64);
        B.Op(wopF64PromoteF32);
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
  FRawTmp := LongWord(P + 4);
  FGfxP := LongWord(P + 5);
  FGfxN := LongWord(P + 6);
  FArrTmp := LongWord(P + 7);
  FRecTmp := LongWord(P + 8);
  SetLength(Locals, 9);
  Locals[0] := wvtI32;                       // dispatch state
  Locals[1] := wvtI64; Locals[2] := wvtF64; Locals[3] := wvtI32;
  Locals[4] := wvtI64;                       // raw pointer being decoded
  Locals[5] := wvtI32; Locals[6] := wvtI32;  // ScreenRes fill cursor + counter
  Locals[7] := wvtI32;                       // DIM's running element product
  Locals[8] := wvtI32;                       // a record handle being addressed
  // one local per transfer slot this region mentions
  k := P + 9;
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
  RT: TSSARegisterType;
  Next: LongWord;
  Init: TWasmBuf;
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

  if FUsesPrint or FUsesHeap then
  begin
    // Enough pages to hold the literals; the allocator grows it from there. The
    // memory is exported so a differential can read the framebuffer out without
    // the program having to print it.
    FModule.DefineMemory((FHeapBase + 65535) div 65536, 0);
    FModule.DataSegment(CONST_SPACE, PByte(PAnsiChar(' '#10)), 2);
    if Length(FConstBytes) > 0 then
      FModule.DataSegment(STR_CONST_BASE, PByte(PAnsiChar(FConstBytes)),
                          Length(FConstBytes));
    FModule.ExportMemory('memory');
  end;
  Init := TWasmBuf.Create;
  try
    if FUsesHeap then
    begin
      Init.I32Const(LongInt(FHeapBase));
      FHeapTop := FModule.DefineGlobal(wvtI32, True, Init);
      Init.Clear;
    end;
    if FUsesGfx then
    begin
      Init.I32Const(0); FScrW := FModule.DefineGlobal(wvtI32, True, Init);
      Init.Clear;
      Init.I32Const(0); FScrH := FModule.DefineGlobal(wvtI32, True, Init);
      Init.Clear;
      Init.I32Const(0); FFbBase := FModule.DefineGlobal(wvtI32, True, Init);
    end;
  finally
    Init.Free;
  end;
  if FUsesGfx then
  begin
    FModule.ExportGlobal('screen_w', FScrW);
    FModule.ExportGlobal('screen_h', FScrH);
    // ⭐ Where the framebuffer IS, rather than a constant the page has to know:
    // it is bump-allocated now, so its address depends on what else the program
    // holds. A viewer that hardcodes a base would read the wrong bytes the
    // first time a program has both strings and graphics.
    FModule.ExportGlobal('screen_ptr', FFbBase);
  end;

  // Functions have to be numbered before any of them is emitted, because a call
  // names its callee by index and a procedure may be called before it is built.
  for r := 0 to FRegionCount - 1 do
    FFuncIdx[r] := FImportCount + LongWord(r);
  // ⛔ The order here must match the order the Emit*Helpers add them in, and
  // nothing checks it: getting it wrong calls the wrong function with the right
  // types, which validates.
  Next := FImportCount + LongWord(FRegionCount);
  if FUsesPrint then
  begin
    FPrintIntFunc := Next;
    FPrintUIntFunc := Next + 1;
    FPrintNlFunc := Next + 2;
    Inc(Next, 3);
  end;
  if FUsesHeap then
  begin
    FAllocFunc := Next;
    Inc(Next);
  end;
  if FUsesStr then
  begin
    FStrNewFunc   := Next;
    FStrCatFunc   := Next + 1;
    FStrSubFunc   := Next + 2;
    FStrCmpFunc   := Next + 3;
    FStrAscFunc   := Next + 4;
    FStrChrFunc   := Next + 5;
    FStrRightFunc := Next + 6;
    FStrMidFunc   := Next + 7;
    FPrintStrFunc := Next + 8;
    Inc(Next, 9);
  end;
  if FUsesArr then
  begin
    // load and store for each of the three banks, in bank order, then the two
    // bound queries - the same order EmitArrayHelpers adds them in
    for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
    begin
      FArrLoad[RT] := Next; FArrStore[RT] := Next + 1;
      Inc(Next, 2);
    end;
    FArrLBoundFunc := Next; FArrUBoundFunc := Next + 1;
    Inc(Next, 2);
  end;

  for r := 0 to FRegionCount - 1 do
    if not EmitRegion(r) then Exit(False);
  if FUsesPrint then EmitPrintHelpers;
  if FUsesHeap then EmitHeapHelpers;
  if FUsesStr then EmitStringHelpers;
  if FUsesArr then EmitArrayHelpers;

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
