{*
 * SedaiBasic - A BASIC interpreter with bytecode VM
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * This program is dual-licensed:
 *
 * 1) For open source use: GNU General Public License version 3 (GPL-3.0-only)
 *    You may redistribute and/or modify it under the terms of the GNU GPL v3
 *    as published by the Free Software Foundation.
 *    See <https://www.gnu.org/licenses/gpl-3.0.html>
 *
 * 2) For commercial/proprietary use: A separate commercial license is required.
 *    Contact: maurizio.cammalleri@gmail.com for licensing inquiries.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}
{ ============================================================================
  SedaiRangeAnalysis - B4: bounds-check elimination via static range analysis.

  Proves, per array access, that the linear index always lies in
  [0, TotalSize) and marks the ssaArrayLoad/ssaArrayStore instruction
  BoundsSafe. The flag is a pure OPTIMIZATION HINT:
    - the interpreter keeps checking unconditionally (it never reads the
      flag), so interpreted semantics are bit-identical with or without
      this pass and the interpreter stays the correctness oracle;
    - the bytecode compiler copies the flag into the instruction's
      Immediate field (always 0 for these opcodes otherwise), where the
      loop JIT reads it; the AOT backend reads the SSA field directly.
      Both elide the "cmp idx,count + branch" guard on flagged accesses.

  WHAT IS PROVEN (all conditions required):

  Array side - the extent is a compile-time constant that cannot change:
    - constant sizes only: Dimensions[d] > 0, or a dimension register (the
      generator materializes even a CONSTANT upper bound into a register:
      "Dim a(0 To N-1)" with N a Const) that evaluates to one constant;
      no runtime lower bounds;
    - exactly ONE ssaArrayDim in the whole program targets the array;
    - no other extent-changing or aliasing op touches it: any op outside
      a small benign whitelist that carries an svkArrayRef to the array
      excludes it (default-deny, so future ops are conservative by
      construction). ssaArrayBind names the CALLER's array as a ConstInt
      in Src3, not as an svkArrayRef - special-cased.
    - the single ssaArrayDim DOMINATES the access (dominator tree; call
      edges exist in this CFG, so a module-level DIM executed before any
      call dominates procedure bodies).

  Index side - interval analysis on the SSA value of the linear index:
    - constants, CopyInt, AddInt/SubInt/MulInt compose intervals
      (saturating: anything beyond +/-2^40 degrades to unknown);
    - wherever a literal is REQUIRED rather than merely composed (the FOR
      step, a loop-invariant limit), it is resolved through up to 3 CopyInt
      hops as well: with constants value-numbered, only the FIRST occurrence
      of a literal is an ssaLoadConstInt and every later one is a copy of it;
    - Const-backing forwarding: a Const lives in a size-1 backing array
      written by exactly one entry-block store - a load from it yields
      the stored value's range (strict rules, see BackingLoadRange);
    - FOR-loop induction variables, in BOTH register disciplines:
      (a) VERSIONED (proc-local scalars in MODERN): the canonical header
          PHI - two sources, init from outside the loop, latch value of
          the form "phi +/- const";
      (b) UNVERSIONED (module-level scalars, CLASSIC): a register with
          exactly ONE def inside the loop, of the form "R +/- const"
          sitting in a latch whose only successor is the header; the
          init range is the UNION of every def of R outside the loop
          (a superset of whatever can reach the header - sound).
      Either way the header must end in a conditional branch on a compare
      of the counter against a loop-invariant limit, with the true edge
      inside the loop and the false edge outside. For USES in loop blocks
      other than the header (guarded by the compare):
        step > 0:  [init.lo, limit.hi]   (CmpLe; CmpLt gives hi-1)
        step < 0:  [limit.lo, init.hi]   (CmpGe; CmpGt gives lo+1)
      In the unversioned discipline a use in the increment's own block is
      ordered by instruction index (before the increment = the guarded
      value, after = guarded + step).

  SOUNDNESS GUARDS (learned from prior optimizer bugs, see memories):
    - a program containing error-flow ops (ON ERROR / RESUME / TRAP)
      disables the analysis outright: RESUME <label> can re-enter a loop
      body without passing the header guard and those edges are not in
      the CFG;
    - a loop containing ANY call op (ssaCall/ssaCallSub/
      ssaCallSubIndirect/ssaReturn/ssaReturnSub) is rejected: the CFG
      has call edges but NO return edges, so flow through a call is not
      modelled (the strength-reduction GOSUB phantom-loop class);
    - non-reducible loops are rejected: every loop block except the
      header must have all predecessors inside the loop (a GOTO into a
      loop body would bypass the guard);
    - values are traced through the def map: single-static-def values
      recurse into their def; multi-def values are only handled by the
      unversioned-IV rule above, everything else is unknown. Any
      instruction Dest that is an int register counts as a def EXCEPT
      ssaArrayStore, whose Dest is provably a pure read (the stored
      value); other read-through-Dest opcodes stay over-counted, which
      only pushes values toward "unknown" - the conservative direction.

  Runs AFTER all transforming SSA passes (post-DCE) and BEFORE PHI
  elimination (it needs the PHIs). It changes no instruction stream -
  only the BoundsSafe flag.
  ============================================================================ }
unit SedaiRangeAnalysis;

{$mode objfpc}{$H+}
{$I OptimizationFlags.inc}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, SedaiSSATypes, SedaiDominators;

type
  TRange = record
    Known: Boolean;
    Lo, Hi: Int64;
  end;
  TDefRec = record
    Instr: TSSAInstruction;
    Block: TSSABasicBlock;
    InstrIndex: Integer;      // position inside Block (same-block ordering)
    Count: Integer;           // total defs of this key (meaningful on the head)
    Next: Integer;            // chain to the next def of the same key (-1 = end)
  end;
  TLoopRec = record
    Header: TSSABasicBlock;
    Blocks: TFPList;          // TSSABasicBlock members (includes Header)
    Sound: Boolean;           // reducible and free of call ops
  end;
  TArrFact = record
    Eligible: Boolean;
    TotalSize: Int64;         // product of constant extents (0 when the extent is symbolic)
    // SYMBOLIC EXTENT (1-D only). "Dim a(1 To n)" with n a runtime value is the commonest array
    // declaration in BASIC and had no proof at all: TotalSize is unknown, so every access kept its
    // check. It does not need to be known. When the loop runs over the array's OWN extent - the
    // guard's limit is the SAME single-def value as the dimension register - the extent cancels out
    // of the inequality and what is left is constant arithmetic. See ProveSymbolic.
    SymExtent: Boolean;
    SymUb: TSSAValue;         // the dimension register: size = SymUb - SymLb + 1
    SymLb: Int64;
    // ARRAY PARAMETERS. A procedure's array parameter is a PLACEHOLDER slot that a call site aliases
    // to the caller's array (ssaArrayBind, the caller's id in Src3). Neither side could be proven:
    // the placeholder has no extent of its own, and the caller's array was disqualified outright for
    // being passed anywhere. On real programs that is where most unproven accesses live - 83% of them
    // on the Rosetta corpus - and it is pure bounds check: the two hot loops, one over a parameter
    // and one over a module array, are instruction-for-instruction identical apart from the "safe"
    // mark, and the parameter one runs 37% slower on --aot.
    // So the placeholder borrows the extent of what is bound to it, when every bind site agrees.
    // Declared lower bound of dimension 0, when it is a compile-time constant. Needed because the
    // subscript of an array PARAMETER lowers to "index - LBOUND(arr, 0)": the callee cannot know the
    // caller's lower bound, so the generator reads it at runtime and the index becomes opaque. Once
    // the placeholder knows what it is bound to, that read has a known value.
    LbKnown: Int64;
    LbIsKnown: Boolean;
    Sound: Boolean;           // passed every screen EXCEPT the passed-to-a-procedure one
    PassedToProc: Boolean;    // named as an argument by some ssaArrayBind (caller side)
    SizeKnown: Int64;         // constant extent, computed whether or not the array is Eligible
    BindTarget: Integer;      // caller array bound to this placeholder: -1 none, -2 conflicting
    BindCount: Integer;
    BindOpaque: Boolean;      // bound from a runtime handle (ssaArrayBindInd): extent unknowable
    DimBlock: TSSABasicBlock; // block of the single ssaArrayDim
    DimIndex: Integer;        // its index inside DimBlock
    // Single-store tracking (const-backing forwarding): a size-1 array written
    // by exactly ONE ssaArrayStore acts as a named constant (Const N lives in
    // such a backing). StoreCount saturates at 2.
    StoreCount: Integer;
    StoreInstr: TSSAInstruction;
    StoreBlock: TSSABasicBlock;
    StoreIndex: Integer;      // position inside StoreBlock
  end;

  // A scalar parameter that every call site supplies with the SAME literal. "Sub Foo(a(), n)" walked
  // with "For i = 1 To n" is how BASIC scans an array, and the extent being known buys nothing while
  // the LIMIT is an unknown parameter. Arguments ride the transfer bank: the caller stores into a
  // slot right before ssaCallSub, the callee loads it in its entry block.
  TCallParam = record
    Entry: TSSABasicBlock;    // the callee's entry block
    Slot: Integer;
    Val: Int64;
  end;

  TRangeAnalysis = class
  private
    FProgram: TSSAProgram;
    FDomTree: TDominatorTree;
    FDefs: TStringList;         // 'idx:ver' -> head index into FDefRecs (int bank only)
    FDefRecs: array of TDefRec;
    FLoops: array of TLoopRec;
    FArrFacts: array of TArrFact;
    FCallParams: array of TCallParam;
    FHasErrFlow: Boolean;       // ON ERROR / RESUME / TRAP present -> analysis disabled
    {$IFDEF DEBUG_RANGE}
    FStepWhy: string;           // why TraceStep gave up (diagnostics only)
    {$ENDIF}
    function DefKey(const V: TSSAValue): string; inline;
    function FindDefIdx(const V: TSSAValue): Integer;                 // head index, -1 = no def
    function FindDef(const V: TSSAValue; out D: TDefRec): Boolean;    // True only if SINGLE def
    function SameReg(const A, B: TSSAValue): Boolean; inline;
    procedure BuildDefMap;
    procedure BuildArrayFacts;
    procedure BuildLoops;
    procedure BuildCallArgs;
    function EntryParamConst(Blk: TSSABasicBlock; Slot: Integer; out V: Int64): Boolean;
    function XferSlotRange(Blk: TSSABasicBlock; Idx, Slot, Depth: Integer): TRange;
    function LoopOfHeader(H: TSSABasicBlock): Integer;
    function MkRange(ALo, AHi: Int64): TRange;
    function Unknown: TRange;
    function RangeAdd(const A, B: TRange): TRange;
    function RangeSub(const A, B: TRange): TRange;
    function RangeMul(const A, B: TRange): TRange;
    function RangeUnion(const A, B: TRange): TRange;
    function ConstOf(const X: TSSAValue; out CV: Int64): Boolean;
    function EvalRange(const V: TSSAValue; UseBlock: TSSABasicBlock;
                       UseIndex, Depth: Integer): TRange;
    function DefValueRange(Instr: TSSAInstruction; Blk: TSSABasicBlock;
                           InstrIdx, Depth: Integer): TRange;
    function BackingLoadRange(LoadInstr: TSSAInstruction; LoadBlock: TSSABasicBlock;
                              LoadIndex, Depth: Integer): TRange;
    function FindGuard(H: TSSABasicBlock; li: Integer; const CounterVal: TSSAValue;
                       out Cmp: TSSAInstruction): Boolean;
    function GuardedRange(li: Integer; H: TSSABasicBlock; Cmp: TSSAInstruction;
                          Step: Int64; const InitR: TRange; Depth: Integer): TRange;
    function EvalForIV(Phi: TSSAInstruction; H, UseBlock: TSSABasicBlock;
                       UseIndex, Depth: Integer): TRange;
    function EvalDerivedIV(H, UseBlock: TSSABasicBlock; li: Integer; UseIndex: Integer;
                           Step: Int64; const IR: TRange; Depth: Integer): TRange;
    function LoopStepOf(const V: TSSAValue; li: Integer; out Step: Int64): Boolean;
    function TryUnversionedIV(const V: TSSAValue; UseBlock: TSSABasicBlock;
                              UseIndex, Depth: Integer): TRange;
    function TraceStep(const LatchVal, PhiDest: TSSAValue; out Step: Int64): Boolean;
    function SameValueThroughCopies(const A, B: TSSAValue): Boolean;
    function ProveSymbolic(const V: TSSAValue; UseBlock: TSSABasicBlock;
                           UseIndex, a: Integer): Boolean;
    function SelfPhi(Phi: TSSAInstruction): Boolean;
    function StepOfDef(Ins: TSSAInstruction; const V: TSSAValue; out Step: Int64): Boolean;
  public
    constructor Create(AProgram: TSSAProgram);
    destructor Destroy; override;
    function Run: Integer;      // number of accesses proven safe
  end;

implementation

{$IFDEF DEBUG_RANGE}
uses SedaiDebug;
{$ENDIF}

const
  // Saturation bound for interval arithmetic. Any bound whose absolute value
  // exceeds this degrades to unknown; keeps every add/sub/mul far away from
  // Int64 overflow while covering any realistic array extent.
  RANGE_MAX = Int64(1) shl 40;
  MAX_DEPTH = 12;

var
  // Gate for the symbolic-extent proof (see TArrFact.SymExtent), read once: -1 unknown,
  // 0 = B4SYM=0, constant extents only (the historical rule), 1 = symbolic extents too.
  GSymExtent: Integer = -1;

function SymbolicExtentEnabled: Boolean;
begin
  if GSymExtent < 0 then
    if GetEnvironmentVariable('B4SYM') = '0' then GSymExtent := 0 else GSymExtent := 1;
  Result := GSymExtent = 1;
end;

var
  // Gate for giving a procedure's array PARAMETER the extent of what is bound to it (see the
  // placeholder screen in BuildArrayFacts), read once: -1 unknown, 0 = B4BIND=0, no placeholder is
  // ever eligible (the historical rule), 1 = borrow the extent when every bind site agrees.
  GBindExtent: Integer = -1;

function PlaceholderExtentEnabled: Boolean;
begin
  if GBindExtent < 0 then
    if GetEnvironmentVariable('B4BIND') = '0' then GBindExtent := 0 else GBindExtent := 1;
  Result := GBindExtent = 1;
end;

var
  // Gate for reading a value out of a transfer slot - a procedure ARGUMENT (see XferSlotRange and
  // BuildCallArgs), read once: -1 unknown, 0 = B4ARG=0, arguments stay opaque (the historical rule),
  // 1 = follow them.
  GArgRange: Integer = -1;

function ArgumentRangeEnabled: Boolean;
begin
  if GArgRange < 0 then
    if GetEnvironmentVariable('B4ARG') = '0' then GArgRange := 0 else GArgRange := 1;
  Result := GArgRange = 1;
end;

constructor TRangeAnalysis.Create(AProgram: TSSAProgram);
begin
  inherited Create;
  FProgram := AProgram;
  FDefs := TStringList.Create;
  FDefs.Sorted := True;
end;

destructor TRangeAnalysis.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FLoops) do
    FLoops[i].Blocks.Free;
  FDefs.Free;
  inherited Destroy;
end;

function TRangeAnalysis.DefKey(const V: TSSAValue): string;
begin
  Result := IntToStr(V.RegIndex) + ':' + IntToStr(V.Version);
end;

function TRangeAnalysis.SameReg(const A, B: TSSAValue): Boolean;
begin
  Result := (A.Kind = svkRegister) and (B.Kind = svkRegister) and
            (A.RegType = B.RegType) and (A.RegIndex = B.RegIndex) and
            (A.Version = B.Version);
end;

procedure TRangeAnalysis.BuildDefMap;
// One whole-program scan. Every instruction whose Dest is an INT register is
// counted as a def of (RegIndex, Version), EXCEPT ssaArrayStore, whose Dest is
// provably a pure READ (the stored value) - counting it would hide the real
// single def of every value that is ever stored into an array. Other opcodes
// that read through Dest are left over-counted on purpose. Also detects
// error-flow ops (the global kill-switch).
var
  b, i, di, head, p: Integer;
  Blk: TSSABasicBlock;
  Instr: TSSAInstruction;
  Key: string;
begin
  SetLength(FDefRecs, 0);
  FHasErrFlow := False;
  for b := 0 to FProgram.Blocks.Count - 1 do
  begin
    Blk := FProgram.Blocks[b];
    for i := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := Blk.Instructions[i];
      if OpIn(Instr.OpCode, [ssaOnError, ssaResume, ssaResumeNext,
                             ssaResumeLabel, ssaTrap]) then
        FHasErrFlow := True;
      if (Instr.Dest.Kind = svkRegister) and (Instr.Dest.RegType = srtInt) and
         (Instr.OpCode <> ssaArrayStore) then
      begin
        Key := DefKey(Instr.Dest);
        SetLength(FDefRecs, Length(FDefRecs) + 1);
        FDefRecs[High(FDefRecs)].Instr := Instr;
        FDefRecs[High(FDefRecs)].Block := Blk;
        FDefRecs[High(FDefRecs)].InstrIndex := i;
        FDefRecs[High(FDefRecs)].Count := 1;
        FDefRecs[High(FDefRecs)].Next := -1;
        di := FDefs.IndexOf(Key);
        if di < 0 then
          FDefs.AddObject(Key, TObject(PtrInt(High(FDefRecs))))
        else
        begin
          // Append to the chain and bump the head's count.
          head := PtrInt(FDefs.Objects[di]);
          Inc(FDefRecs[head].Count);
          p := head;
          while FDefRecs[p].Next >= 0 do p := FDefRecs[p].Next;
          FDefRecs[p].Next := High(FDefRecs);
        end;
      end;
    end;
  end;
end;

function TRangeAnalysis.FindDefIdx(const V: TSSAValue): Integer;
var
  di: Integer;
begin
  Result := -1;
  if (V.Kind <> svkRegister) or (V.RegType <> srtInt) then Exit;
  di := FDefs.IndexOf(DefKey(V));
  if di < 0 then Exit;
  Result := PtrInt(FDefs.Objects[di]);
end;

function TRangeAnalysis.FindDef(const V: TSSAValue; out D: TDefRec): Boolean;
var
  h: Integer;
begin
  Result := False;
  h := FindDefIdx(V);
  if h < 0 then Exit;
  D := FDefRecs[h];
  Result := D.Count = 1;
end;

procedure TRangeAnalysis.BuildArrayFacts;
const
  // Ops that may carry an svkArrayRef WITHOUT invalidating the constant-extent
  // proof. Anything else touching the array excludes it (default-deny).
  BENIGN: array[0..8] of TSSAOpCode = (
    ssaArrayLoad, ssaArrayStore, ssaArrayDim, ssaArrayLBound, ssaArrayUBound,
    ssaArrayIdxPush, ssaArrayIdxResolve,
    // The bind pair names the PLACEHOLDER in Src1. It does not reshape anything - it aliases, and
    // the alias is exactly what the placeholder's extent is inferred FROM (see BindTarget below), so
    // it must not disqualify it. ssaArrayBindInd stays out on purpose: it binds a runtime handle.
    ssaArrayBind, ssaArrayUnbind);
var
  a, b, bt, i, d, DimSeen: Integer;
  Info: TSSAArrayInfo;
  Blk: TSSABasicBlock;
  Instr: TSSAInstruction;
  Total, SizeD, Lb: Int64;
  HasLbRegs: Boolean;
  UbVal: TSSAValue;
  UbDef: TDefRec;
  UbRange: TRange;

  procedure Touch(ArrId: Integer; Op: TSSAOpCode);
  begin
    if (ArrId < 0) or (ArrId > High(FArrFacts)) then Exit;
    if not OpIn(Op, BENIGN) then
      FArrFacts[ArrId].Sound := False;
  end;

  procedure TouchOperand(const V: TSSAValue; Op: TSSAOpCode);
  begin
    if V.Kind = svkArrayRef then Touch(V.ArrayIndex, Op);
  end;

begin
  SetLength(FArrFacts, FProgram.GetArrayCount);
  for a := 0 to High(FArrFacts) do
  begin
    FArrFacts[a].Eligible := True;    // tentative; the screens below clear it
    FArrFacts[a].LbKnown := 0;
    FArrFacts[a].LbIsKnown := False;
    FArrFacts[a].Sound := True;       // ditto, minus the passed-to-a-procedure screen
    FArrFacts[a].PassedToProc := False;
    FArrFacts[a].SizeKnown := 0;
    FArrFacts[a].BindTarget := -1;
    FArrFacts[a].BindCount := 0;
    FArrFacts[a].BindOpaque := False;
    FArrFacts[a].TotalSize := 0;
    FArrFacts[a].SymExtent := False;
    FArrFacts[a].SymLb := 0;
    FArrFacts[a].DimBlock := nil;
    FArrFacts[a].StoreCount := 0;
    FArrFacts[a].StoreInstr := nil;
    FArrFacts[a].StoreBlock := nil;
  end;
  // Program screen: single DIM, only benign ops, no BIND (the caller-side id
  // rides in Src3.ConstInt of ssaArrayBind, not in an svkArrayRef). Also
  // tracks the single-store fact for const-backing forwarding.
  for b := 0 to FProgram.Blocks.Count - 1 do
  begin
    Blk := FProgram.Blocks[b];
    for i := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := Blk.Instructions[i];
      TouchOperand(Instr.Dest, Instr.OpCode);
      TouchOperand(Instr.Src1, Instr.OpCode);
      TouchOperand(Instr.Src2, Instr.OpCode);
      TouchOperand(Instr.Src3, Instr.OpCode);
      // A bind records BOTH sides: the caller's array is passed to a procedure (which today ends its
      // own eligibility, unchanged), and the callee's PLACEHOLDER learns what it is aliased to.
      if (Instr.OpCode = ssaArrayBind) and (Instr.Src3.Kind = svkConstInt) and
         (Instr.Src3.ConstInt >= 0) and (Instr.Src3.ConstInt <= High(FArrFacts)) then
      begin
        FArrFacts[Instr.Src3.ConstInt].PassedToProc := True;
        if Instr.Src1.Kind = svkArrayRef then
        begin
          a := Instr.Src1.ArrayIndex;
          if (a >= 0) and (a <= High(FArrFacts)) then
          begin
            Inc(FArrFacts[a].BindCount);
            if FArrFacts[a].BindCount = 1 then
              FArrFacts[a].BindTarget := Instr.Src3.ConstInt
            else if FArrFacts[a].BindTarget <> Instr.Src3.ConstInt then
              FArrFacts[a].BindTarget := -2;   // bound to different arrays: no single extent
          end;
        end;
      end;
      // Bound from a RUNTIME handle (a UDT array member): nothing can be known about the extent.
      if (Instr.OpCode = ssaArrayBindInd) and (Instr.Src1.Kind = svkArrayRef) then
      begin
        a := Instr.Src1.ArrayIndex;
        if (a >= 0) and (a <= High(FArrFacts)) then FArrFacts[a].BindOpaque := True;
      end;
      if (Instr.OpCode = ssaArrayDim) and (Instr.Src1.Kind = svkArrayRef) then
      begin
        a := Instr.Src1.ArrayIndex;
        if (a >= 0) and (a <= High(FArrFacts)) then
        begin
          if FArrFacts[a].DimBlock <> nil then
            FArrFacts[a].Sound := False   // second DIM: extent no longer single-valued
          else
          begin
            FArrFacts[a].DimBlock := Blk;
            FArrFacts[a].DimIndex := i;
          end;
        end;
      end;
      if (Instr.OpCode = ssaArrayStore) and (Instr.Src1.Kind = svkArrayRef) then
      begin
        a := Instr.Src1.ArrayIndex;
        if (a >= 0) and (a <= High(FArrFacts)) and (FArrFacts[a].StoreCount < 2) then
        begin
          Inc(FArrFacts[a].StoreCount);
          FArrFacts[a].StoreInstr := Instr;
          FArrFacts[a].StoreBlock := Blk;
          FArrFacts[a].StoreIndex := i;
        end;
      end;
    end;
  end;
  // An array never DIM'd is never proven (its descriptor may be empty) - except a placeholder, whose
  // extent comes from the array bound to it and which is resolved after the size screen below.
  // NOTE: having a DIM is NOT part of Sound. Sound means "only benign ops touch it, and it is not
  // dimensioned twice" - a placeholder satisfies that and has no DIM at all, which is the whole
  // point. The DIM requirement belongs to the arrays that carry their own extent.
  for a := 0 to High(FArrFacts) do
    // The caller-side rule, unchanged in effect: an array passed to a procedure is not proven for
    // its OWN accesses. Its extent facts now survive, because the placeholder needs them.
    FArrFacts[a].Eligible := FArrFacts[a].Sound and (FArrFacts[a].DimBlock <> nil) and
                             not FArrFacts[a].PassedToProc;
  // Size screen: constant extents only, no runtime lower bounds. A dimension is
  // static if Dimensions[d] > 0 (compile-time size) OR its dim register - the
  // generator materializes even a CONSTANT upper bound into a register ("Dim
  // a(0 To N-1)" with N a Const) - evaluates to a single constant; the runtime
  // computes size = ub - lb + 1. EvalRange runs AFTER the program screen so the
  // const-backing forwarding (Const N lives in a size-1 backing array) can rely
  // on the facts collected above; one level of indirection resolves fully.
  DimSeen := 0;
  for a := 0 to High(FArrFacts) do
  begin
    // Sound, not Eligible: an array passed to a procedure still needs its extent computed, because
    // the placeholder aliased to it borrows exactly that. A DIM is required here (this screen reads
    // the declared extent); a placeholder has none and is handled after this loop.
    if not (FArrFacts[a].Sound and (FArrFacts[a].DimBlock <> nil)) then Continue;
    Info := FProgram.GetArray(a);
    {$IFDEF DEBUG_RANGE}
    if DebugRange then
      WriteLn('[Range] arr=', a, ' "', Info.Name, '" dims=', Info.DimCount,
              ' dimregs=', Length(Info.DimRegisters),
              ' lbregs=', Length(Info.LowerBoundRegisters),
              ' stores=', FArrFacts[a].StoreCount);
    {$ENDIF}
    FArrFacts[a].Eligible := False;
    if Info.DimCount <= 0 then Continue;
    if Length(Info.Dimensions) < Info.DimCount then Continue;
    HasLbRegs := False;
    for d := 0 to High(Info.LowerBoundRegisters) do
      if Info.LowerBoundRegisters[d] >= 0 then HasLbRegs := True;
    if HasLbRegs then Continue;
    // No runtime lower bounds past this point, so dimension 0's is declared and constant. An ABSENT
    // entry means the default 0 - "Dim d(0 To N-1)" records no lower bound at all, and reading that
    // as "unknown" silently cost every zero-based array its placeholder proof. Same default the
    // extent computation below uses.
    if Info.DimCount >= 1 then
    begin
      if 0 <= High(Info.LowerBounds) then FArrFacts[a].LbKnown := Info.LowerBounds[0]
      else FArrFacts[a].LbKnown := 0;
      FArrFacts[a].LbIsKnown := True;
    end;
    Total := 1;
    for d := 0 to Info.DimCount - 1 do
    begin
      SizeD := 0;
      if Info.Dimensions[d] > 0 then
        SizeD := Info.Dimensions[d]
      else if (d < Length(Info.DimRegisters)) and (Info.DimRegisters[d] >= 0) and
              (d < Length(Info.DimRegTypes)) and (Info.DimRegTypes[d] = srtInt) then
      begin
        UbVal := MakeSSARegister(srtInt, Info.DimRegisters[d]);   // Version 0: descriptor regs are excluded from versioning
        if FindDef(UbVal, UbDef) then
        begin
          UbRange := EvalRange(UbVal, UbDef.Block, UbDef.InstrIndex, 0);
          if UbRange.Known and (UbRange.Lo = UbRange.Hi) then
          begin
            Lb := 0;
            if d <= High(Info.LowerBounds) then Lb := Info.LowerBounds[d];
            SizeD := UbRange.Lo - Lb + 1;
          end;
        end;
      end;
      if (SizeD <= 0) or (SizeD > High(Integer)) then begin Total := 0; Break; end;
      Total := Total * SizeD;
      if Total > High(Integer) then begin Total := 0; Break; end;
    end;
    if Total <= 0 then
    begin
      // No compile-time size. If the array is 1-D and its extent is ONE single-def int register,
      // the size is unknown but NAMED, and an access indexed by a loop over that same name still
      // proves (ProveSymbolic). This is "Dim a(1 To n)" - the commonest declaration in BASIC, and
      // until now the one with no proof at all. B4SYM=0 restores the constants-only rule.
      if SymbolicExtentEnabled and (Info.DimCount = 1) and (not HasLbRegs) and
         (Length(Info.DimRegisters) > 0) and (Info.DimRegisters[0] >= 0) and
         (Length(Info.DimRegTypes) > 0) and (Info.DimRegTypes[0] = srtInt) then
      begin
        UbVal := MakeSSARegister(srtInt, Info.DimRegisters[0]);
        if FindDef(UbVal, UbDef) then
        begin
          FArrFacts[a].Eligible := not FArrFacts[a].PassedToProc;
          FArrFacts[a].SymExtent := True;
          FArrFacts[a].SymUb := UbVal;
          if 0 <= High(Info.LowerBounds) then FArrFacts[a].SymLb := Info.LowerBounds[0];
          FArrFacts[a].TotalSize := 0;
          Inc(DimSeen);
          {$IFDEF DEBUG_RANGE}
          if DebugRange then
            WriteLn('[Range] arr=', a, ' "', Info.Name, '" SYMBOLIC extent reg',
                    Info.DimRegisters[0], ' lb=', FArrFacts[a].SymLb);
          {$ENDIF}
        end;
      end;
      Continue;
    end;
    FArrFacts[a].SizeKnown := Total;                          // kept even when not Eligible
    FArrFacts[a].Eligible := not FArrFacts[a].PassedToProc;
    FArrFacts[a].TotalSize := Total;
    Inc(DimSeen);
  end;
  // Placeholder screen: a procedure's array parameter borrows the extent of what is bound to it.
  // Requirements, each one a soundness screen:
  //   * at least one ssaArrayBind, and every one of them names the SAME caller array (BindTarget
  //     goes to -2 the moment two disagree) - otherwise there is no single extent to speak of;
  //   * never bound from a runtime handle (ssaArrayBindInd);
  //   * the placeholder itself passed the ordinary screens - the bind pair is benign FOR IT by
  //     definition, since it aliases rather than reshapes;
  //   * the target has a COMPILE-TIME extent and passed its own screens.
  // The dominance anchor is the TARGET's DIM, not the bind: the extent is established there, the
  // access touches that same memory, and a module-level DIM dominates procedure bodies through the
  // call edges this CFG has. A SYMBOLIC extent is deliberately not forwarded - the register naming
  // it is not in scope inside the callee.
  // B4BIND=0 restores the old behaviour (no placeholder is ever eligible) for a one-binary A/B.
  if PlaceholderExtentEnabled then
    for a := 0 to High(FArrFacts) do
    begin
      if FArrFacts[a].Eligible or FArrFacts[a].BindOpaque then Continue;
      if (FArrFacts[a].BindCount = 0) or (FArrFacts[a].BindTarget < 0) then Continue;
      if not FArrFacts[a].Sound then Continue;
      bt := FArrFacts[a].BindTarget;
      if (bt < 0) or (bt > High(FArrFacts)) then Continue;
      if not FArrFacts[bt].Sound then Continue;
      if FArrFacts[bt].SizeKnown <= 0 then Continue;
      if FArrFacts[bt].DimBlock = nil then Continue;
      if not FArrFacts[bt].LbIsKnown then Continue;   // the callee reads LBOUND: it must be knowable
      FArrFacts[a].Eligible := True;
      FArrFacts[a].TotalSize := FArrFacts[bt].SizeKnown;
      FArrFacts[a].LbKnown := FArrFacts[bt].LbKnown;
      FArrFacts[a].LbIsKnown := True;
      FArrFacts[a].DimBlock := FArrFacts[bt].DimBlock;
      FArrFacts[a].DimIndex := FArrFacts[bt].DimIndex;
      Inc(DimSeen);
      {$IFDEF DEBUG_RANGE}
      if DebugRange then
        WriteLn('[Range] arr=', a, ' "', FProgram.GetArray(a).Name, '" PLACEHOLDER bound to arr=', bt,
                ' size=', FArrFacts[a].TotalSize, ' (', FArrFacts[a].BindCount, ' bind sites)');
      {$ENDIF}
    end;
  {$IFDEF DEBUG_RANGE}
  if DebugRange then
    WriteLn('[Range] eligible arrays: ', DimSeen, '/', Length(FArrFacts));
  {$ENDIF}
end;

function TRangeAnalysis.EntryParamConst(Blk: TSSABasicBlock; Slot: Integer; out V: Int64): Boolean;
var
  k: Integer;
begin
  Result := False;
  V := 0;
  for k := 0 to High(FCallParams) do
    if (FCallParams[k].Entry = Blk) and (FCallParams[k].Slot = Slot) then
    begin
      V := FCallParams[k].Val;
      Exit(True);
    end;
end;

function TRangeAnalysis.XferSlotRange(Blk: TSSABasicBlock; Idx, Slot, Depth: Integer): TRange;
// What does this transfer slot hold at this point? Walk BACKWARD to the store that put it there.
//
// This is not an interprocedural question at all, and that is the point: the SSA inliner takes the
// callee's body into the caller, but the argument still round-trips through the transfer bank -
// XferStore in the caller, XferLoad in what used to be the prologue. So "Sub Foo(a(), n)" called
// with a constant keeps an UNKNOWN loop limit even after inlining, purely because the value passes
// through a slot the analysis could not read.
//
// Sound because the walk stops at anything that could have written the slot behind our back:
//   * any CALL (a callee stages its own arguments through these same slots);
//   * a store to the same slot - that one IS the value;
//   * a block with anything other than exactly one predecessor, so the path walked is the only path
//     that reaches the load, and the last store on it is the one that executed.
// Capped at 4 blocks, which also stops a unique-predecessor chain that loops.
var
  Cur: TSSABasicBlock;
  i, hops: Integer;
  Ins: TSSAInstruction;
begin
  Result := Unknown;
  if Depth > MAX_DEPTH then Exit;
  Cur := Blk;
  i := Idx - 1;
  for hops := 0 to 4 do
  begin
    while i >= 0 do
    begin
      Ins := Cur.Instructions[i];
      if OpIn(Ins.OpCode, [ssaCall, ssaCallSub, ssaCallSubIndirect]) then Exit;
      if (Ins.OpCode = ssaXferStoreInt) and (Ins.Src3.Kind = svkConstInt) and
         (Ins.Src3.ConstInt = Slot) then
        Exit(EvalRange(Ins.Src1, Cur, i, Depth + 1));
      Dec(i);
    end;
    if Cur.Predecessors.Count <> 1 then Exit;
    Cur := TSSABasicBlock(Cur.Predecessors[0]);
    i := Cur.Instructions.Count - 1;
  end;
end;

procedure TRangeAnalysis.BuildCallArgs;
// Interprocedural, and deliberately the narrowest useful shape: a scalar INT parameter that EVERY
// call site supplies with the same literal. That is enough for "Sub Foo(a(), n)" called as
// "Foo(a(), N)", which is how BASIC walks an array and which no amount of extent knowledge could
// prove while the loop's limit stayed unknown.
//
// SOUNDNESS SCREENS, all required:
//   * the whole analysis is off if the program can call through a POINTER (ssaCallSubIndirect) or
//     takes a procedure's address (ssaLoadProcAddr): either can reach a procedure with arguments
//     this scan never sees;
//   * EVERY predecessor of the entry block must be a block that ends in a call to it - a procedure
//     reachable any other way (fall-through, GOTO) receives whatever the slot happened to hold;
//   * the staging store must sit in the SAME block as the call (blocks are basic, so the last store
//     to a slot before the call is the one that reaches it). A call site whose store is elsewhere
//     drops the whole slot, not just itself;
//   * the value must be a literal at every site, and the same one. A recursive call passing "n-1"
//     fails this and takes the parameter with it - which is correct: the value is not invariant.
const
  MAX_BACK = 64;      // argument staging sits immediately before the call
var
  b, i, j, k, p, nEntries: Integer;
  Blk, Entry, PredB: TSSABasicBlock;
  Ins: TSSAInstruction;
  Entries: TFPList;
  Slots: array of Integer;
  Vals: array of Int64;
  Seen: array of Boolean;
  First: Boolean;
  C: Int64;
  Ok: Boolean;

  function LiveBlock(B2: TSSABasicBlock): Boolean;
  // Is this block still part of the program? FindBlock answers from a label map that can outlive the
  // block itself - an inlined procedure's original body is removed from Blocks, and walking the
  // predecessors of a stale one is how this pass first crashed ("List index out of bounds").
  var m: Integer;
  begin
    Result := False;
    if not Assigned(B2) then Exit;
    for m := 0 to FProgram.Blocks.Count - 1 do
      if FProgram.Blocks[m] = B2 then Exit(True);
  end;

  function CallTargetOf(B2: TSSABasicBlock; out Idx: Integer): TSSABasicBlock;
  // The ssaCallSub that ends this block, and the entry block it names.
  var m: Integer;
  begin
    Result := nil; Idx := -1;
    if not Assigned(B2) then Exit;
    for m := B2.Instructions.Count - 1 downto 0 do
      if B2.Instructions[m].OpCode = ssaCallSub then
      begin
        if B2.Instructions[m].Dest.Kind <> svkLabel then Exit;
        Result := FProgram.FindBlock(B2.Instructions[m].Dest.LabelName);
        if not LiveBlock(Result) then Result := nil;
        Idx := m;
        Exit;
      end;
  end;

  procedure ScanSite(CallBlk: TSSABasicBlock; CallIdx: Integer);
  // Collect slot -> literal for one call site, then intersect with what previous sites gave.
  var
    m, s, q, lo: Integer;
    Ins2: TSSAInstruction;
    LocalSlot: array of Integer;
    LocalVal: array of Int64;
    found: Boolean;
  begin
    SetLength(LocalSlot, 0); SetLength(LocalVal, 0);
    lo := CallIdx - MAX_BACK; if lo < 0 then lo := 0;
    for m := CallIdx - 1 downto lo do
    begin
      Ins2 := CallBlk.Instructions[m];
      if Ins2.OpCode <> ssaXferStoreInt then Continue;
      if Ins2.Src3.Kind <> svkConstInt then Continue;
      s := Ins2.Src3.ConstInt;
      found := False;
      for q := 0 to High(LocalSlot) do
        if LocalSlot[q] = s then begin found := True; Break; end;
      if found then Continue;              // a later store already won this slot
      if not ConstOf(Ins2.Src1, C) then Continue;
      SetLength(LocalSlot, Length(LocalSlot) + 1);
      SetLength(LocalVal, Length(LocalVal) + 1);
      LocalSlot[High(LocalSlot)] := s;
      LocalVal[High(LocalVal)] := C;
    end;
    if First then
    begin
      SetLength(Slots, Length(LocalSlot)); SetLength(Vals, Length(LocalVal));
      for q := 0 to High(LocalSlot) do begin Slots[q] := LocalSlot[q]; Vals[q] := LocalVal[q]; end;
      SetLength(Seen, Length(Slots));
      First := False;
      Exit;
    end;
    // Intersect: a slot survives only if this site gives it the same literal.
    for q := 0 to High(Slots) do
    begin
      if Slots[q] < 0 then Continue;
      found := False;
      for s := 0 to High(LocalSlot) do
        if (LocalSlot[s] = Slots[q]) and (LocalVal[s] = Vals[q]) then begin found := True; Break; end;
      if not found then Slots[q] := -1;
    end;
  end;

begin
  SetLength(FCallParams, 0);
  if not ArgumentRangeEnabled then Exit;
  // Screen 1: any call through a pointer, or any procedure whose address is taken, and we stop.
  for b := 0 to FProgram.Blocks.Count - 1 do
  begin
    Blk := FProgram.Blocks[b];
    for i := 0 to Blk.Instructions.Count - 1 do
      if OpIn(Blk.Instructions[i].OpCode, [ssaCallSubIndirect, ssaLoadProcAddr]) then Exit;
  end;
  Entries := TFPList.Create;
  try
    // Distinct call targets.
    for b := 0 to FProgram.Blocks.Count - 1 do
    begin
      Entry := CallTargetOf(FProgram.Blocks[b], i);
      if Assigned(Entry) and (Entries.IndexOf(Entry) < 0) then Entries.Add(Entry);
    end;
    nEntries := Entries.Count;
    for k := 0 to nEntries - 1 do
    begin
      Entry := TSSABasicBlock(Entries[k]);
      // Screen 2: every way into this block must be a call to it.
      Ok := Entry.Predecessors.Count > 0;
      for p := 0 to Entry.Predecessors.Count - 1 do
      begin
        PredB := TSSABasicBlock(Entry.Predecessors[p]);
        // A predecessor can itself be stale for the same reason - check before touching it.
        if not LiveBlock(PredB) then begin Ok := False; Break; end;
        if CallTargetOf(PredB, i) <> Entry then begin Ok := False; Break; end;
      end;
      if not Ok then Continue;
      First := True;
      SetLength(Slots, 0); SetLength(Vals, 0);
      for p := 0 to Entry.Predecessors.Count - 1 do
      begin
        PredB := TSSABasicBlock(Entry.Predecessors[p]);
        if CallTargetOf(PredB, i) <> Entry then Continue;
        ScanSite(PredB, i);
      end;
      for j := 0 to High(Slots) do
        if Slots[j] >= 0 then
        begin
          SetLength(FCallParams, Length(FCallParams) + 1);
          FCallParams[High(FCallParams)].Entry := Entry;
          FCallParams[High(FCallParams)].Slot := Slots[j];
          FCallParams[High(FCallParams)].Val := Vals[j];
          {$IFDEF DEBUG_RANGE}
          if DebugRange then
            WriteLn('[Range] param @', Entry.LabelName, ' slot ', Slots[j], ' = ', Vals[j],
                    ' at all ', Entry.Predecessors.Count, ' call sites');
          {$ENDIF}
        end;
    end;
  finally
    Entries.Free;
  end;
end;

procedure TRangeAnalysis.BuildLoops;
// Natural loops from back edges (B -> H where H dominates B), merged per
// header. Sound = reducible (no side entries) and free of call/return ops:
// the CFG models a call with a CALL edge and no RETURN edge, so any loop
// reasoning that crosses a call is built on a graph that does not describe
// the program (the strength-reduction GOSUB lesson).
var
  b, s, li, wi, i, k: Integer;
  Blk, Succ, Cur, Pred: TSSABasicBlock;
  Work: TFPList;
  Instr: TSSAInstruction;
begin
  SetLength(FLoops, 0);
  for b := 0 to FProgram.Blocks.Count - 1 do
  begin
    Blk := FProgram.Blocks[b];
    for s := 0 to Blk.Successors.Count - 1 do
    begin
      Succ := TSSABasicBlock(Blk.Successors[s]);
      if not FDomTree.IsDom(Succ, Blk) then Continue;   // not a back edge
      li := LoopOfHeader(Succ);
      if li < 0 then
      begin
        SetLength(FLoops, Length(FLoops) + 1);
        li := High(FLoops);
        FLoops[li].Header := Succ;
        FLoops[li].Blocks := TFPList.Create;
        FLoops[li].Blocks.Add(Succ);
        FLoops[li].Sound := True;
      end;
      // Grow the member set: everything reaching the back-edge source
      // backwards without passing the header.
      Work := TFPList.Create;
      try
        if FLoops[li].Blocks.IndexOf(Blk) < 0 then
        begin
          FLoops[li].Blocks.Add(Blk);
          Work.Add(Blk);
        end;
        wi := 0;
        while wi < Work.Count do
        begin
          Cur := TSSABasicBlock(Work[wi]);
          Inc(wi);
          for i := 0 to Cur.Predecessors.Count - 1 do
          begin
            Pred := TSSABasicBlock(Cur.Predecessors[i]);
            if FLoops[li].Blocks.IndexOf(Pred) < 0 then
            begin
              FLoops[li].Blocks.Add(Pred);
              Work.Add(Pred);
            end;
          end;
        end;
      finally
        Work.Free;
      end;
    end;
  end;
  // Soundness screen per loop.
  for li := 0 to High(FLoops) do
  begin
    for i := 0 to FLoops[li].Blocks.Count - 1 do
    begin
      Cur := TSSABasicBlock(FLoops[li].Blocks[i]);
      // Reducibility: only the header may have predecessors outside the loop.
      if Cur <> FLoops[li].Header then
        for k := 0 to Cur.Predecessors.Count - 1 do
          if FLoops[li].Blocks.IndexOf(Cur.Predecessors[k]) < 0 then
            FLoops[li].Sound := False;
      // No flow through calls.
      for k := 0 to Cur.Instructions.Count - 1 do
      begin
        Instr := Cur.Instructions[k];
        if OpIn(Instr.OpCode, [ssaCall, ssaCallSub, ssaCallSubIndirect,
                               ssaReturn, ssaReturnSub]) then
          FLoops[li].Sound := False;
      end;
    end;
  end;
end;

function TRangeAnalysis.LoopOfHeader(H: TSSABasicBlock): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FLoops) do
    if FLoops[i].Header = H then Exit(i);
end;

function TRangeAnalysis.MkRange(ALo, AHi: Int64): TRange;
begin
  Result.Known := (ALo <= AHi) and (ALo >= -RANGE_MAX) and (AHi <= RANGE_MAX);
  Result.Lo := ALo;
  Result.Hi := AHi;
end;

function TRangeAnalysis.Unknown: TRange;
begin
  Result.Known := False;
  Result.Lo := 0;
  Result.Hi := 0;
end;

function TRangeAnalysis.RangeAdd(const A, B: TRange): TRange;
begin
  if A.Known and B.Known then
    Result := MkRange(A.Lo + B.Lo, A.Hi + B.Hi)   // bounds <= 2^41, no overflow
  else
    Result := Unknown;
end;

function TRangeAnalysis.RangeSub(const A, B: TRange): TRange;
begin
  if A.Known and B.Known then
    Result := MkRange(A.Lo - B.Hi, A.Hi - B.Lo)
  else
    Result := Unknown;
end;

function TRangeAnalysis.RangeMul(const A, B: TRange): TRange;
var
  c: array[0..3] of Int64;
  Lo, Hi: Int64;
  i: Integer;
begin
  Result := Unknown;
  if not (A.Known and B.Known) then Exit;
  // Corner products can reach 2^80: screen magnitudes first so the Int64
  // multiplications below cannot overflow (|a|,|b| <= 2^20 -> product <= 2^40).
  if (Abs(A.Lo) > (Int64(1) shl 20)) or (Abs(A.Hi) > (Int64(1) shl 20)) or
     (Abs(B.Lo) > (Int64(1) shl 20)) or (Abs(B.Hi) > (Int64(1) shl 20)) then Exit;
  c[0] := A.Lo * B.Lo; c[1] := A.Lo * B.Hi;
  c[2] := A.Hi * B.Lo; c[3] := A.Hi * B.Hi;
  Lo := c[0]; Hi := c[0];
  for i := 1 to 3 do
  begin
    if c[i] < Lo then Lo := c[i];
    if c[i] > Hi then Hi := c[i];
  end;
  Result := MkRange(Lo, Hi);
end;

function TRangeAnalysis.RangeUnion(const A, B: TRange): TRange;
begin
  if A.Known and B.Known then
  begin
    Result.Known := True;
    if A.Lo < B.Lo then Result.Lo := A.Lo else Result.Lo := B.Lo;
    if A.Hi > B.Hi then Result.Hi := A.Hi else Result.Hi := B.Hi;
  end
  else
    Result := Unknown;
end;

function TRangeAnalysis.ConstOf(const X: TSSAValue; out CV: Int64): Boolean;
// A literal operand, or a single-def register that holds one - through up to 3
// CopyInt hops, exactly like TraceStep/StepOfDef do for the counter itself.
// The hops are NOT cosmetic: once GVN value-numbers constants, the second and
// later occurrences of a literal stop being their own ssaLoadConstInt and
// become a copy of the canonical one, so a FOR step written "i + 1" reaches
// here as "i + (copy of the program's canonical 1)". Without the hops the
// induction variable is rejected and every array access it indexes keeps its
// bounds check - which cost n-body's native path 52%.
// Sound by construction: every hop goes through FindDef, which answers only for
// SINGLE-def registers, so the copied value cannot have been reassigned.
var
  DK: TDefRec;
  V: TSSAValue;
  Hops: Integer;
begin
  Result := False;
  CV := 0;
  if X.Kind = svkConstInt then begin CV := X.ConstInt; Exit(True); end;
  V := X;
  for Hops := 0 to 3 do
  begin
    if not FindDef(V, DK) then Exit;
    case DK.Instr.OpCode of
      ssaLoadConstInt:
        begin
          if DK.Instr.Src1.Kind <> svkConstInt then Exit;
          CV := DK.Instr.Src1.ConstInt;
          Exit(True);
        end;
      ssaCopyInt:
        begin
          if DK.Instr.Src1.Kind = svkConstInt then
          begin
            CV := DK.Instr.Src1.ConstInt;
            Exit(True);
          end;
          if DK.Instr.Src1.Kind <> svkRegister then Exit;
          V := DK.Instr.Src1;
        end;
    else
      Exit;
    end;
  end;
end;

function TRangeAnalysis.TraceStep(const LatchVal, PhiDest: TSSAValue; out Step: Int64): Boolean;
// The loop-carried phi source must be "phi +/- const", possibly through a few
// CopyInt hops. Any other shape (a second def of the counter in the body, a
// runtime step) rejects the IV.
var
  D: TDefRec;
  V: TSSAValue;
  Hops: Integer;
  C: Int64;
  {$IFDEF DEBUG_RANGE}
  D2: TDefRec;
  DI: Integer;
  {$ENDIF}
begin
  Result := False;
  Step := 0;
  V := LatchVal;
  Hops := 0;
  {$IFDEF DEBUG_RANGE}
  FStepWhy := 'hops exhausted';
  {$ENDIF}
  while Hops <= 3 do
  begin
    if not FindDef(V, D) then
    begin
      {$IFDEF DEBUG_RANGE}
      FStepWhy := 'no single def of latch reg' + IntToStr(V.RegIndex) + ':' + IntToStr(V.Version);
      {$ENDIF}
      Exit;
    end;
    case D.Instr.OpCode of
      ssaCopyInt:
        begin
          V := D.Instr.Src1;
          Inc(Hops);
        end;
      ssaAddInt:
        begin
          if SameReg(D.Instr.Src1, PhiDest) and ConstOf(D.Instr.Src2, C) then
          begin Step := C; Exit(True); end;
          if SameReg(D.Instr.Src2, PhiDest) and ConstOf(D.Instr.Src1, C) then
          begin Step := C; Exit(True); end;
          {$IFDEF DEBUG_RANGE}
          if SameReg(D.Instr.Src1, PhiDest) then
          begin
            if D.Instr.Src2.Kind <> svkRegister then
              FStepWhy := 'add: src2 kind ' + IntToStr(Ord(D.Instr.Src2.Kind))
            else if not FindDef(D.Instr.Src2, D2) then
            begin
              DI := FindDefIdx(D.Instr.Src2);
              if DI < 0 then
                FStepWhy := 'add: src2 reg' + IntToStr(D.Instr.Src2.RegIndex) + ':' +
                            IntToStr(D.Instr.Src2.Version) + ' has NO def'
              else
                FStepWhy := 'add: src2 reg' + IntToStr(D.Instr.Src2.RegIndex) + ':' +
                            IntToStr(D.Instr.Src2.Version) + ' defs=' +
                            IntToStr(FDefRecs[DI].Count);
            end
            else
              FStepWhy := 'add: src2 def is ' + SSAOpCodeToString(D2.Instr.OpCode);
          end
          else if SameReg(D.Instr.Src2, PhiDest) then
            FStepWhy := 'add: src1 not const'
          else
            FStepWhy := 'add: neither operand is the phi';
          {$ENDIF}
          Exit;
        end;
      ssaSubInt:
        begin
          if SameReg(D.Instr.Src1, PhiDest) and ConstOf(D.Instr.Src2, C) then
          begin Step := -C; Exit(True); end;
          {$IFDEF DEBUG_RANGE}
          FStepWhy := 'sub: not phi-const';
          {$ENDIF}
          Exit;
        end;
    else
      {$IFDEF DEBUG_RANGE}
      FStepWhy := 'latch def is ' + SSAOpCodeToString(D.Instr.OpCode);
      {$ENDIF}
      Exit;
    end;
  end;
end;

function TRangeAnalysis.SelfPhi(Phi: TSSAInstruction): Boolean;
// True for the degenerate phi of an UNVERSIONED variable: dest and every
// source are the same (reg, version). PHI elimination lowers it to
// self-copies, i.e. no-ops - it merges nothing.
var
  i: Integer;
begin
  Result := False;
  if Phi.Dest.Kind <> svkRegister then Exit;
  for i := 0 to High(Phi.PhiSources) do
    if not SameReg(Phi.PhiSources[i].Value, Phi.Dest) then Exit;
  Result := True;
end;

function TRangeAnalysis.StepOfDef(Ins: TSSAInstruction; const V: TSSAValue;
                                  out Step: Int64): Boolean;
// Is this def "V := V +/- const"? Follows up to 3 CopyInt hops through
// single-def temps (the FOR increment is "tmp := V + step; V := Copy tmp").
var
  D: TDefRec;
  Hops: Integer;
  C: Int64;
begin
  Result := False;
  Step := 0;
  Hops := 0;
  while Hops <= 3 do
  begin
    case Ins.OpCode of
      ssaCopyInt:
        begin
          if not FindDef(Ins.Src1, D) then Exit;
          Ins := D.Instr;
          Inc(Hops);
        end;
      ssaAddInt:
        begin
          if SameReg(Ins.Src1, V) and ConstOf(Ins.Src2, C) then
          begin Step := C; Exit(True); end;
          if SameReg(Ins.Src2, V) and ConstOf(Ins.Src1, C) then
          begin Step := C; Exit(True); end;
          Exit;
        end;
      ssaSubInt:
        begin
          if SameReg(Ins.Src1, V) and ConstOf(Ins.Src2, C) then
          begin Step := -C; Exit(True); end;
          Exit;
        end;
    else
      Exit;
    end;
  end;
end;

function TRangeAnalysis.FindGuard(H: TSSABasicBlock; li: Integer;
                                  const CounterVal: TSSAValue;
                                  out Cmp: TSSAInstruction): Boolean;
// The header must end in a conditional branch whose condition is a compare of
// the counter (Src1) against something, with the TRUE edge inside the loop and
// the FALSE edge leaving it. Returns the compare instruction.
var
  k, i: Integer;
  Jump: TSSAInstruction;
  CmpDef: TDefRec;
  TrueBlk, FalseBlk, Other: TSSABasicBlock;
begin
  Result := False;
  Cmp := nil;
  Jump := nil;
  for k := H.Instructions.Count - 1 downto 0 do
    if OpIn(H.Instructions[k].OpCode, [ssaJumpIfZero, ssaJumpIfNotZero]) then
    begin
      Jump := H.Instructions[k];
      Break;
    end;
  if Jump = nil then Exit;
  if not FindDef(Jump.Src1, CmpDef) then Exit;
  if CmpDef.Block <> H then Exit;
  if not SameReg(CmpDef.Instr.Src1, CounterVal) then Exit;
  if Jump.Dest.Kind <> svkLabel then Exit;
  if H.Successors.Count <> 2 then Exit;
  Other := nil;
  FalseBlk := nil;
  for i := 0 to 1 do
    if TSSABasicBlock(H.Successors[i]).LabelName = Jump.Dest.LabelName then
      FalseBlk := TSSABasicBlock(H.Successors[i])
    else
      Other := TSSABasicBlock(H.Successors[i]);
  if Jump.OpCode = ssaJumpIfZero then
    TrueBlk := Other                    // taken when the compare is FALSE
  else
  begin
    TrueBlk := FalseBlk;                // ssaJumpIfNotZero: taken when TRUE
    FalseBlk := Other;
  end;
  if (TrueBlk = nil) or (FalseBlk = nil) then Exit;
  if FLoops[li].Blocks.IndexOf(TrueBlk) < 0 then Exit;    // body must be in-loop
  if FLoops[li].Blocks.IndexOf(FalseBlk) >= 0 then Exit;  // exit must leave it
  Cmp := CmpDef.Instr;
  Result := True;
end;

function TRangeAnalysis.GuardedRange(li: Integer; H: TSSABasicBlock;
                                     Cmp: TSSAInstruction; Step: Int64;
                                     const InitR: TRange; Depth: Integer): TRange;
// Combine the guard compare with the init range, honoring step direction.
// The limit must be loop-invariant: a constant, or a single-def register whose
// def sits outside the loop.
var
  LimitVal: TSSAValue;
  LimDef: TDefRec;
  ER: TRange;
  Lo, Hi: Int64;
begin
  Result := Unknown;
  if not InitR.Known then Exit;
  LimitVal := Cmp.Src2;
  if LimitVal.Kind = svkRegister then
  begin
    if not FindDef(LimitVal, LimDef) then Exit;
    if FLoops[li].Blocks.IndexOf(LimDef.Block) >= 0 then Exit;
    ER := EvalRange(LimitVal, H, -1, Depth + 1);
  end
  else if LimitVal.Kind = svkConstInt then
    ER := MkRange(LimitVal.ConstInt, LimitVal.ConstInt)
  else
    Exit;
  if not ER.Known then Exit;
  if Step > 0 then
  begin
    case Cmp.OpCode of
      ssaCmpLeInt: Hi := ER.Hi;
      ssaCmpLtInt: Hi := ER.Hi - 1;
    else
      Exit;
    end;
    Lo := InitR.Lo;
  end
  else
  begin
    case Cmp.OpCode of
      ssaCmpGeInt: Lo := ER.Lo;
      ssaCmpGtInt: Lo := ER.Lo + 1;
    else
      Exit;
    end;
    Hi := InitR.Hi;
  end;
  Result := MkRange(Lo, Hi);
end;

function TRangeAnalysis.LoopStepOf(const V: TSSAValue; li: Integer; out Step: Int64): Boolean;
// The constant this register advances by, once per iteration of loop li. True only when the loop
// holds EXACTLY ONE def of it and that def is "V := V +/- const" - two defs mean two step values on
// two paths, and a bound built on either of them would be a guess.
var
  di: Integer;
  S: Int64;
  Found: Boolean;
begin
  Result := False; Step := 0; Found := False;
  di := FindDefIdx(V);
  while di >= 0 do
  begin
    if FLoops[li].Blocks.IndexOf(FDefRecs[di].Block) >= 0 then
    begin
      // degenerate self-phis are merge markers, not defs (same rule as TryUnversionedIV)
      if not ((FDefRecs[di].Instr.OpCode = ssaPhi) and SelfPhi(FDefRecs[di].Instr)) then
      begin
        if Found then Exit;                                   // a second def: refuse
        if not StepOfDef(FDefRecs[di].Instr, V, S) then Exit;
        Step := S; Found := True;
      end;
    end;
    di := FDefRecs[di].Next;
  end;
  Result := Found and (Step <> 0);
end;

function TRangeAnalysis.EvalDerivedIV(H, UseBlock: TSSABasicBlock; li: Integer;
                                      UseIndex: Integer; Step: Int64;
                                      const IR: TRange; Depth: Integer): TRange;
// The proof described at the call site, built on ONE identity and nothing else:
//
//   p advances once per iteration by Step, beside the guarded counter g which advances by GStep.
//   Over the whole loop g travels GR.Hi - GR.Lo, so p travels (GR.Hi - GR.Lo) * Step / GStep,
//   and p stays within its own initial range extended by exactly that much.
//
// Using the counter's TRAVEL rather than its absolute value is what keeps this short and sound: it
// needs no relation between p's origin and g's, only that they step together - so the guarded
// counter's range, which the analysis already proves, is the only external fact required.
//
// ⛔ Refused, deliberately, in every case the identity does not cover:
//   - either step not positive (a countdown, or a step whose sign differs from the other's);
//   - the counter's travel or the arithmetic below not exactly divisible / representable;
//   - more than one def of the counter in the loop (LoopStepOf).
var
  k: Integer;
  Jump, CmpI: TSSAInstruction;
  CmpDef: TDefRec;
  GStep, Travel, Reach: Int64;
  GR: TRange;

  {$IFDEF DEBUG_RANGE}
  procedure DWhy(const Msg: string);
  begin
    if DebugRange then WriteLn('[Range]   derived-IV reject @', H.LabelName, ': ', Msg);
  end;
  {$ELSE}
  procedure DWhy(const Msg: string); begin end;
  {$ENDIF}
begin
  Result := Unknown;
  if Step <= 0 then begin DWhy('our step is not positive'); Exit; end;
  // The loop's guard, whatever counter it is on.
  Jump := nil;
  for k := H.Instructions.Count - 1 downto 0 do
    if OpIn(H.Instructions[k].OpCode, [ssaJumpIfZero, ssaJumpIfNotZero]) then
    begin Jump := H.Instructions[k]; Break; end;
  if Jump = nil then begin DWhy('header has no conditional jump'); Exit; end;
  if not FindDef(Jump.Src1, CmpDef) then begin DWhy('no def for the condition'); Exit; end;
  if CmpDef.Block <> H then begin DWhy('condition not computed in the header'); Exit; end;
  CmpI := CmpDef.Instr;
  if CmpI.Src1.Kind <> svkRegister then begin DWhy('guard counter is not a register'); Exit; end;
  if not LoopStepOf(CmpI.Src1, li, GStep) then
  begin DWhy('guard counter has no single constant step in this loop'); Exit; end;
  if GStep <= 0 then begin DWhy('guard counter step is not positive'); Exit; end;
  // Its proven range AT THIS USE - the same question the analysis answers for the counter itself,
  // asked again here rather than re-derived, so the two can never disagree.
  GR := EvalRange(CmpI.Src1, UseBlock, UseIndex, Depth + 1);
  if not GR.Known then begin DWhy('guard counter range unknown at the use'); Exit; end;
  Travel := GR.Hi - GR.Lo;
  if Travel < 0 then begin DWhy('guard counter range inverted'); Exit; end;
  if (Travel mod GStep) <> 0 then begin DWhy('counter travel not a whole number of steps'); Exit; end;
  if (Travel div GStep) > (RANGE_MAX div Step) then begin DWhy('reach overflows'); Exit; end;
  Reach := (Travel div GStep) * Step;
  if (IR.Hi > RANGE_MAX - Reach) then begin DWhy('reach saturates'); Exit; end;
  Result := MkRange(IR.Lo, IR.Hi + Reach);
  {$IFDEF DEBUG_RANGE}
  if DebugRange then
    WriteLn('[Range]   derived IV @', H.LabelName, ': counter=[', GR.Lo, ',', GR.Hi,
            '] step=', GStep, ' ours step=', Step, ' init=[', IR.Lo, ',', IR.Hi,
            '] -> [', Result.Lo, ',', Result.Hi, ']');
  {$ENDIF}
end;

function TRangeAnalysis.EvalForIV(Phi: TSSAInstruction; H, UseBlock: TSSABasicBlock;
                                  UseIndex, Depth: Integer): TRange;
// VERSIONED induction variable: the canonical loop-header PHI.
var
  li, i: Integer;
  InitVal, LatchVal: TSSAValue;
  InitBlock: TSSABasicBlock;
  NIn, NOut: Integer;
  Step: Int64;
  Cmp: TSSAInstruction;
  IR: TRange;

  {$IFDEF DEBUG_RANGE}
  procedure Why(const Msg: string);
  begin
    if DebugRange then
      WriteLn('[Range]   IV reject @', H.LabelName, ': ', Msg);
  end;
  {$ELSE}
  procedure Why(const Msg: string); begin end;
  {$ENDIF}

begin
  Result := Unknown;
  li := LoopOfHeader(H);
  if li < 0 then begin Why('no loop for header'); Exit; end;
  if not FLoops[li].Sound then begin Why('loop not sound'); Exit; end;
  // Guarded use only: a loop block other than the header is reached, in every
  // iteration, through the header's true edge - the compare held for the
  // CURRENT phi value.
  if (UseBlock = H) or (FLoops[li].Blocks.IndexOf(UseBlock) < 0) then
  begin Why('use not guarded (' + UseBlock.LabelName + ')'); Exit; end;
  if Length(Phi.PhiSources) <> 2 then
  begin Why('phi sources <> 2 (' + IntToStr(Length(Phi.PhiSources)) + ')'); Exit; end;
  // Split sources: exactly one from inside the loop (latch), one from outside.
  NIn := 0; NOut := 0;
  InitVal := Phi.Src1;  // placate the compiler; overwritten below
  LatchVal := Phi.Src1;
  InitBlock := nil;
  for i := 0 to 1 do
  begin
    if Phi.PhiSources[i].FromBlock = nil then Exit;
    if FLoops[li].Blocks.IndexOf(Phi.PhiSources[i].FromBlock) >= 0 then
    begin
      LatchVal := Phi.PhiSources[i].Value;
      Inc(NIn);
    end
    else
    begin
      InitVal := Phi.PhiSources[i].Value;
      InitBlock := Phi.PhiSources[i].FromBlock;
      Inc(NOut);
    end;
  end;
  if (NIn <> 1) or (NOut <> 1) then begin Why('phi sources not 1-in/1-out'); Exit; end;
  if not TraceStep(LatchVal, Phi.Dest, Step) then
  begin
    {$IFDEF DEBUG_RANGE}
    Why('latch not phi+const [' + FStepWhy + ']');
    {$ELSE}
    Why('latch not phi+const');
    {$ENDIF}
    Exit;
  end;
  if Step = 0 then begin Why('step 0'); Exit; end;
  IR := EvalRange(InitVal, InitBlock, -1, Depth + 1);
  if not IR.Known then begin Why('init range unknown'); Exit; end;
  if FindGuard(H, li, Phi.Dest, Cmp) then
  begin
    Result := GuardedRange(li, H, Cmp, Step, IR, Depth);
    {$IFDEF DEBUG_RANGE}
    if not Result.Known then Why('guard/limit combination failed');
    {$ENDIF}
    Exit;
  end;
  // ⭐ DERIVED induction variable: the loop's guard is on a DIFFERENT counter, and this one runs
  // beside it. `Dim p = base : For j = 0 To n-1 : a(p) : p += 1 : Next` is ordinary BASIC - it is
  // what a hand strength-reduced loop looks like - and until now every access through `p` kept its
  // bounds guard, because FindGuard only ever asked about the phi in front of it.
  //
  // 📊 What that cost, measured: matmul written with running indices instead of `i*NM+j` runs 846 ms
  // against the recomputing form's 598 - forty-one percent slower, entirely on three guards the
  // analysis could not remove. The shape is not exotic; it is the one an induction-variable
  // strength reduction would produce, so this also has to exist before that pass can pay.
  //
  // The proof is one identity: while both counters advance together, p = p0 + (g - g0). So with the
  // guarded counter's range GR and its own initial range GIR,
  //     p in [ IR.Lo + (GR.Lo - GIR.Hi) , IR.Hi + (GR.Hi - GIR.Lo) ]
  // which is exact interval arithmetic on that identity, not an approximation of it.
  //
  // ⛔ "Advance together" is the whole safety condition, and it is four things, all required:
  //   - both phis live in the SAME header, so they are evaluated on the same edge;
  //   - both are entered from the SAME predecessor, so p0 and g0 are established together;
  //   - both take their loop value from the SAME latch block, so no path updates one without the
  //     other;
  //   - THE STEPS ARE EQUAL. A different step is a different trip relation, and rather than reason
  //     about ratios this refuses - `p += 2` beside `j += 1` keeps its guard.
  Result := EvalDerivedIV(H, UseBlock, li, UseIndex, Step, IR, Depth);
  {$IFDEF DEBUG_RANGE}
  if not Result.Known then Why('no guard on phi, and no derived-IV partner');
  {$ENDIF}
end;

function TRangeAnalysis.SameValueThroughCopies(const A, B: TSSAValue): Boolean;
// Do these two operands denote the SAME value? Registers are followed through up to 3 CopyInt hops
// on either side, each taken only via FindDef (single-def), so "same name" really does mean "same
// value". The generator copies a parameter into a fresh temp before comparing against it, which is
// why the hops are needed at all: "Dim p(1 To n)" and "For i = 1 To n" reach here as two different
// register names for the one n.
  function Root(V: TSSAValue): TSSAValue;
  var DD: TDefRec; k: Integer;
  begin
    for k := 0 to 3 do
    begin
      if V.Kind <> svkRegister then Break;
      if not FindDef(V, DD) then Break;
      if DD.Instr.OpCode <> ssaCopyInt then Break;
      if DD.Instr.Src1.Kind = svkNone then Break;
      V := DD.Instr.Src1;
    end;
    Result := V;
  end;
var
  X, Y: TSSAValue;
begin
  X := Root(A);
  Y := Root(B);
  if (X.Kind = svkConstInt) and (Y.Kind = svkConstInt) then Exit(X.ConstInt = Y.ConstInt);
  Result := SameReg(X, Y);
end;

function TRangeAnalysis.ProveSymbolic(const V: TSSAValue; UseBlock: TSSABasicBlock;
                                      UseIndex, a: Integer): Boolean;
// SYMBOLIC EXTENT: prove an access in bounds when the array's size is NOT known at compile time.
// "Dim a(1 To n)" is the commonest array declaration in BASIC and had no proof at all. It does not
// need one: when the loop runs over the array's OWN extent, the unknown cancels.
//
//   the guard gives     counter <= U        (CmpLe)   or   counter <= U-1  (CmpLt)
//   the index is        counter + Off       (Off collected by peeling constants off the subscript,
//                                            e.g. the "- LBOUND" the generator emits)
//   the array holds     0 .. U - SymLb
//   so the test is      Off + (0|-1) <= -SymLb        -- U is GONE.
//
// What must still be checked numerically is the LOW end (the counter's init) and the direction.
// Requirements, each one a soundness screen and not a convenience:
//   * the index resolves to a loop-header PHI through copies and CONSTANT offsets only;
//   * the use is guarded - in the loop, not the header - so the compare held for this value;
//   * the step is positive (the decreasing case would need the other end of the guard);
//   * the guard's limit is the SAME value as the dimension register, and its def is OUTSIDE the
//     loop (as GuardedRange demands) so the extent cannot move under the counter;
//   * the array is 1-D: with more dimensions the linear index is a product and nothing cancels.
var
  Cur, InitVal, LatchVal: TSSAValue;
  Off, C, Step, MaxOff: Int64;
  hops, li, k, NIn, NOut: Integer;
  D, LimDef: TDefRec;
  H, InitBlock: TSSABasicBlock;
  PhiI: TSSAInstruction;
  Cmp: TSSAInstruction;
  IR: TRange;
  Peeling: Boolean;
begin
  Result := False;
  if (a < 0) or (a > High(FArrFacts)) or not FArrFacts[a].SymExtent then Exit;
  // 1. Peel copies and constant offsets: index = base + Off.
  Cur := V;
  Off := 0;
  Peeling := True;
  hops := 0;
  while Peeling and (hops <= 4) do
  begin
    Inc(hops);
    if Cur.Kind <> svkRegister then Exit;
    if not FindDef(Cur, D) then Exit;
    case D.Instr.OpCode of
      ssaCopyInt: Cur := D.Instr.Src1;
      ssaAddInt:
        if ConstOf(D.Instr.Src2, C) then begin Off := Off + C; Cur := D.Instr.Src1; end
        else if ConstOf(D.Instr.Src1, C) then begin Off := Off + C; Cur := D.Instr.Src2; end
        else Exit;
      ssaSubInt:
        if ConstOf(D.Instr.Src2, C) then begin Off := Off - C; Cur := D.Instr.Src1; end
        else Exit;
      ssaPhi: Peeling := False;
    else
      Exit;
    end;
  end;
  if Peeling then Exit;                       // never reached a phi within the hop budget
  PhiI := D.Instr;
  H := D.Block;
  // 2. The phi must be a loop header's, in a sound loop, and the use must be guarded by it.
  li := LoopOfHeader(H);
  if li < 0 then Exit;
  if not FLoops[li].Sound then Exit;
  if (UseBlock = H) or (FLoops[li].Blocks.IndexOf(UseBlock) < 0) then Exit;
  if Length(PhiI.PhiSources) <> 2 then Exit;
  NIn := 0; NOut := 0;
  InitVal := PhiI.Src1; LatchVal := PhiI.Src1; InitBlock := nil;
  for k := 0 to 1 do
  begin
    if PhiI.PhiSources[k].FromBlock = nil then Exit;
    if FLoops[li].Blocks.IndexOf(PhiI.PhiSources[k].FromBlock) >= 0 then
    begin LatchVal := PhiI.PhiSources[k].Value; Inc(NIn); end
    else
    begin InitVal := PhiI.PhiSources[k].Value; InitBlock := PhiI.PhiSources[k].FromBlock; Inc(NOut); end;
  end;
  if (NIn <> 1) or (NOut <> 1) then Exit;
  if not TraceStep(LatchVal, PhiI.Dest, Step) then Exit;
  if Step <= 0 then Exit;
  if not FindGuard(H, li, PhiI.Dest, Cmp) then Exit;
  // 3. The guard's limit IS the array's upper bound, and it is loop-invariant.
  if Cmp.Src2.Kind = svkRegister then
  begin
    if not FindDef(Cmp.Src2, LimDef) then Exit;
    if FLoops[li].Blocks.IndexOf(LimDef.Block) >= 0 then Exit;
  end;
  if not SameValueThroughCopies(Cmp.Src2, FArrFacts[a].SymUb) then Exit;
  // 4. The high end, with the unknown cancelled out.
  case Cmp.OpCode of
    ssaCmpLeInt: MaxOff := Off;
    ssaCmpLtInt: MaxOff := Off - 1;
  else
    Exit;
  end;
  if MaxOff > -FArrFacts[a].SymLb then Exit;
  // 5. The low end is ordinary arithmetic: with a positive step the counter never goes below init.
  IR := EvalRange(InitVal, InitBlock, -1, 0);
  if not IR.Known then Exit;
  if IR.Lo + Off < 0 then Exit;
  Result := True;
end;

function TRangeAnalysis.TryUnversionedIV(const V: TSSAValue; UseBlock: TSSABasicBlock;
                                         UseIndex, Depth: Integer): TRange;
// UNVERSIONED induction variable (module-level scalars, CLASSIC): register V
// has several static defs. For a loop containing the use:
//   - exactly ONE def of V inside the loop, of the form "V +/- const", in a
//     latch block whose only successor is the header (so no use elsewhere in
//     the loop can observe the post-increment value);
//   - the header guards V against a loop-invariant limit (FindGuard);
//   - init range = the LAST def of V inside the header's UNIQUE out-of-loop
//     predecessor. Blocks are basic (single entry, straight line), so every
//     entry into the loop executes that whole block, and the last def in it
//     is exactly the value the header sees on entry - no other def of V can
//     intervene. FOR lowering emits the init in precisely that block.
//   - a use in the increment's own block is ordered by instruction index:
//     before the increment it sees the guarded value, after it the guarded
//     value + step. An unknown position (-1) in that block is rejected.
var
  h, li, di, p, IncIdx, NIn, InitIdx: Integer;
  Step: Int64;
  Inc_: TDefRec;
  Cmp: TSSAInstruction;
  Derived: Boolean;
  IR, R: TRange;
  Ins: TSSAInstruction;
  H_, PreH, Pb: TSSABasicBlock;

  {$IFDEF DEBUG_RANGE}
  procedure UWhy(const Msg: string);
  begin
    if DebugRange then
      WriteLn('[Range]   UIV reject reg', V.RegIndex, ' loop@', H_.LabelName, ': ', Msg);
  end;
  {$ELSE}
  procedure UWhy(const Msg: string); begin end;
  {$ENDIF}

begin
  Result := Unknown;
  if Depth > MAX_DEPTH then Exit;
  h := FindDefIdx(V);
  if h < 0 then Exit;
  for li := 0 to High(FLoops) do
  begin
    H_ := FLoops[li].Header;
    if H_ = UseBlock then Continue;
    if FLoops[li].Blocks.IndexOf(UseBlock) < 0 then Continue;
    if not FLoops[li].Sound then begin UWhy('loop not sound'); Continue; end;
    // Exactly one def of V inside this loop, and it must be the increment.
    // Unversioned semi-pruned SSA also carries DEGENERATE self-phis for such
    // variables (dest and every source are the same reg+version); PHI
    // elimination lowers them to self-copies, i.e. no-ops - skip them. A phi
    // merging anything else counts as a def (conservative).
    NIn := 0;
    IncIdx := -1;
    di := h;
    while di >= 0 do
    begin
      if FLoops[li].Blocks.IndexOf(FDefRecs[di].Block) >= 0 then
      begin
        if (FDefRecs[di].Instr.OpCode = ssaPhi) and SelfPhi(FDefRecs[di].Instr) then
          // degenerate merge marker: ignore
        else
        begin
          System.Inc(NIn);
          IncIdx := di;
        end;
      end;
      di := FDefRecs[di].Next;
    end;
    if NIn <> 1 then
    begin
      {$IFDEF DEBUG_RANGE}
      if DebugRange then
      begin
        UWhy('in-loop defs=' + IntToStr(NIn));
        di := h;
        while di >= 0 do
        begin
          if FLoops[li].Blocks.IndexOf(FDefRecs[di].Block) >= 0 then
            WriteLn('[Range]     in-loop def: ', SSAOpCodeToString(FDefRecs[di].Instr.OpCode),
                    ' @', FDefRecs[di].Block.LabelName, ' line ', FDefRecs[di].Instr.SourceLine);
          di := FDefRecs[di].Next;
        end;
      end;
      {$ENDIF}
      Continue;
    end;
    Inc_ := FDefRecs[IncIdx];
    Ins := Inc_.Instr;
    if not StepOfDef(Ins, V, Step) then begin UWhy('in-loop def not V+const'); Continue; end;
    if Step = 0 then begin UWhy('step 0'); Continue; end;
    // The increment's block must fall straight back to the header: its ONLY
    // successor is the header, so no other loop block runs after it within an
    // iteration and the post-increment value is observable only in that block.
    if Inc_.Block.Successors.Count <> 1 then begin UWhy('latch succs<>1'); Continue; end;
    if TSSABasicBlock(Inc_.Block.Successors[0]) <> H_ then begin UWhy('latch succ<>header'); Continue; end;
    // ⭐ The guard may be on ANOTHER counter, with this register running beside it - the same
    // derived-IV case EvalForIV handles for versioned phis, and it has to be handled here too or
    // the whole class of MODULE-LEVEL running indices stays unprovable. That class is not a corner:
    // `Dim p` at the top of a program and `p += 1` in a loop is ordinary BASIC, and an unversioned
    // register is exactly what it lowers to.
    // ⛔ It is deferred to after the preheader/init work below, because the derived proof needs this
    // register's own initial range too - so the flag is remembered and acted on once IR is known.
    Derived := not FindGuard(H_, li, V, Cmp);
    // Unique out-of-loop predecessor of the header = the preheader.
    PreH := nil;
    for p := 0 to H_.Predecessors.Count - 1 do
    begin
      Pb := TSSABasicBlock(H_.Predecessors[p]);
      if FLoops[li].Blocks.IndexOf(Pb) < 0 then
      begin
        if PreH <> nil then begin PreH := nil; Break; end;   // more than one entry
        PreH := Pb;
      end;
    end;
    if PreH = nil then begin UWhy('no unique preheader'); Continue; end;
    // Init = the LAST def of V on the single-predecessor chain ending at the
    // preheader (LICM inserts an empty preheader between the FOR's init block
    // and the header; blocks are basic, so walking a unique-predecessor chain
    // backwards visits exactly the code every loop entry just executed).
    InitIdx := -1;
    for p := 0 to 4 do
    begin
      di := h;
      while di >= 0 do
      begin
        if FDefRecs[di].Block = PreH then
          if (InitIdx < 0) or (FDefRecs[di].InstrIndex > FDefRecs[InitIdx].InstrIndex) then
            InitIdx := di;
        di := FDefRecs[di].Next;
      end;
      if InitIdx >= 0 then Break;
      if PreH.Predecessors.Count <> 1 then Break;
      PreH := TSSABasicBlock(PreH.Predecessors[0]);
    end;
    if InitIdx < 0 then begin UWhy('no init on preheader chain'); Continue; end;
    IR := DefValueRange(FDefRecs[InitIdx].Instr, FDefRecs[InitIdx].Block,
                        FDefRecs[InitIdx].InstrIndex, Depth + 1);
    if not IR.Known then begin UWhy('init range unknown'); Continue; end;
    if Derived then
    begin
      R := EvalDerivedIV(H_, UseBlock, li, UseIndex, Step, IR, Depth);
      if R.Known then Exit(R);
      UWhy('no guard, and no derived-IV partner');
      Continue;
    end;
    R := GuardedRange(li, H_, Cmp, Step, IR, Depth);
    if not R.Known then begin UWhy('guard/limit combination failed'); Continue; end;
    // Order the use against the increment when they share a block.
    if UseBlock = Inc_.Block then
    begin
      if UseIndex < 0 then Continue;
      if UseIndex > Inc_.InstrIndex then
        R := RangeAdd(R, MkRange(Step, Step));
      if not R.Known then Continue;
    end;
    Exit(R);
  end;
end;

function TRangeAnalysis.BackingLoadRange(LoadInstr: TSSAInstruction;
                                         LoadBlock: TSSABasicBlock;
                                         LoadIndex, Depth: Integer): TRange;
// Const-backing forwarding: a Const (and every module-level named constant)
// lives in a size-1 backing array written by exactly ONE store; a load from
// it yields the stored value's range. Soundness requires that the value can
// NEVER be re-zeroed between the store and the load, so the rule is strict:
// DIM and store must both sit in the ENTRY block (which has no predecessors,
// hence executes exactly once, before everything), DIM before store, store
// before a load in the same block; entry dominates every other block.
var
  sa: Integer;
  Entry: TSSABasicBlock;
  IdxR: TRange;
begin
  Result := Unknown;
  if LoadInstr.Src1.Kind <> svkArrayRef then Exit;
  sa := LoadInstr.Src1.ArrayIndex;
  if (sa < 0) or (sa > High(FArrFacts)) then Exit;
  if FProgram.Blocks.Count = 0 then Exit;
  Entry := FProgram.Blocks[0];
  if Entry.Predecessors.Count > 0 then Exit;
  with FArrFacts[sa] do
  begin
    if not Eligible then Exit;
    if (TotalSize <> 1) or (StoreCount <> 1) or (StoreInstr = nil) then Exit;
    if (DimBlock <> Entry) or (StoreBlock <> Entry) then Exit;
    if DimIndex >= StoreIndex then Exit;
    if (LoadBlock = Entry) and (LoadIndex >= 0) and (LoadIndex <= StoreIndex) then Exit;
    if (LoadBlock = Entry) and (LoadIndex < 0) then Exit;
    // The single store must hit cell 0 and this load must read cell 0.
    IdxR := EvalRange(StoreInstr.Src2, StoreBlock, StoreIndex, Depth + 1);
    if not (IdxR.Known and (IdxR.Lo = 0) and (IdxR.Hi = 0)) then Exit;
    IdxR := EvalRange(LoadInstr.Src2, LoadBlock, LoadIndex, Depth + 1);
    if not (IdxR.Known and (IdxR.Lo = 0) and (IdxR.Hi = 0)) then Exit;
    Result := EvalRange(StoreInstr.Dest, StoreBlock, StoreIndex, Depth + 1);
  end;
end;

function TRangeAnalysis.DefValueRange(Instr: TSSAInstruction; Blk: TSSABasicBlock;
                                      InstrIdx, Depth: Integer): TRange;
// Range of the value a specific DEF assigns (used for multi-def registers,
// where EvalRange cannot pick a def itself). Operand positions are the def's.
var
  DimR: TRange;
  Ub, PV: Int64;
begin
  Result := Unknown;
  if Depth > MAX_DEPTH then Exit;
  case Instr.OpCode of
    ssaLoadConstInt:
      if Instr.Src1.Kind = svkConstInt then
        Result := MkRange(Instr.Src1.ConstInt, Instr.Src1.ConstInt);
    ssaCopyInt:
      Result := EvalRange(Instr.Src1, Blk, InstrIdx, Depth + 1);
    ssaAddInt:
      Result := RangeAdd(EvalRange(Instr.Src1, Blk, InstrIdx, Depth + 1),
                         EvalRange(Instr.Src2, Blk, InstrIdx, Depth + 1));
    ssaSubInt:
      Result := RangeSub(EvalRange(Instr.Src1, Blk, InstrIdx, Depth + 1),
                         EvalRange(Instr.Src2, Blk, InstrIdx, Depth + 1));
    ssaMulInt:
      Result := RangeMul(EvalRange(Instr.Src1, Blk, InstrIdx, Depth + 1),
                         EvalRange(Instr.Src2, Blk, InstrIdx, Depth + 1));
    ssaArrayLoad:
      Result := BackingLoadRange(Instr, Blk, InstrIdx, Depth);
    // A scalar parameter every call site supplies with the same literal (see BuildCallArgs). Only in
    // the callee's ENTRY block: that is where the prologue reads its arguments, and it is the only
    // point at which the slot is known to still hold what the call staged.
    ssaXferLoadInt:
      if ArgumentRangeEnabled and (Instr.Src3.Kind = svkConstInt) then
      begin
        // The call that survived as a call: every site staged the same literal.
        if EntryParamConst(Blk, Instr.Src3.ConstInt, PV) then
          Result := MkRange(PV, PV)
        else
          // The call the inliner already absorbed: the store is right there in the stream.
          Result := XferSlotRange(Blk, InstrIdx, Instr.Src3.ConstInt, Depth);
      end;
    // LBOUND(arr, 0) on an array whose declared lower bound is a compile-time constant. This is not
    // an optional refinement: the subscript of an array PARAMETER lowers to "index - LBOUND(arr, 0)"
    // because the callee cannot know the caller's lower bound, so without this every index reached
    // through a parameter is UNKNOWN no matter how well the extent is understood.
    ssaArrayLBound:
      if (Instr.Src1.Kind = svkArrayRef) and (Instr.Src1.ArrayIndex >= 0) and
         (Instr.Src1.ArrayIndex <= High(FArrFacts)) and
         FArrFacts[Instr.Src1.ArrayIndex].LbIsKnown and
         FArrFacts[Instr.Src1.ArrayIndex].Eligible then
      begin
        // Dimension 0 only: that is the one whose lower bound the fact records.
        DimR := EvalRange(Instr.Src2, Blk, InstrIdx, Depth + 1);
        if DimR.Known and (DimR.Lo = 0) and (DimR.Hi = 0) then
          Result := MkRange(FArrFacts[Instr.Src1.ArrayIndex].LbKnown,
                            FArrFacts[Instr.Src1.ArrayIndex].LbKnown);
      end;
    // UBOUND(arr, 0) = Lb + Size - 1 for a 1-D array of known extent. "For i = LBound(a) To
    // UBound(a)" is the idiomatic way to walk an array parameter, and it is the shape that becomes
    // provable once the placeholder knows what it is bound to.
    ssaArrayUBound:
      if (Instr.Src1.Kind = svkArrayRef) and (Instr.Src1.ArrayIndex >= 0) and
         (Instr.Src1.ArrayIndex <= High(FArrFacts)) and
         FArrFacts[Instr.Src1.ArrayIndex].LbIsKnown and
         FArrFacts[Instr.Src1.ArrayIndex].Eligible and
         (FArrFacts[Instr.Src1.ArrayIndex].TotalSize > 0) and
         (FProgram.GetArray(Instr.Src1.ArrayIndex).DimCount = 1) then
      begin
        DimR := EvalRange(Instr.Src2, Blk, InstrIdx, Depth + 1);
        if DimR.Known and (DimR.Lo = 0) and (DimR.Hi = 0) then
        begin
          Ub := FArrFacts[Instr.Src1.ArrayIndex].LbKnown +
                FArrFacts[Instr.Src1.ArrayIndex].TotalSize - 1;
          Result := MkRange(Ub, Ub);
        end;
      end;
  end;
end;

function TRangeAnalysis.EvalRange(const V: TSSAValue; UseBlock: TSSABasicBlock;
                                  UseIndex, Depth: Integer): TRange;
var
  D: TDefRec;
  h: Integer;
begin
  Result := Unknown;
  if Depth > MAX_DEPTH then Exit;
  if V.Kind = svkConstInt then Exit(MkRange(V.ConstInt, V.ConstInt));
  if (V.Kind <> svkRegister) or (V.RegType <> srtInt) then Exit;
  h := FindDefIdx(V);
  if h < 0 then Exit;
  if FDefRecs[h].Count <> 1 then
    Exit(TryUnversionedIV(V, UseBlock, UseIndex, Depth));
  D := FDefRecs[h];
  case D.Instr.OpCode of
    ssaPhi:
      Result := EvalForIV(D.Instr, D.Block, UseBlock, UseIndex, Depth);
  else
    Result := DefValueRange(D.Instr, D.Block, D.InstrIndex, Depth);
  end;
end;

function TRangeAnalysis.Run: Integer;
var
  b, i: Integer;
  Blk: TSSABasicBlock;
  Instr: TSSAInstruction;
  a: Integer;
  R: TRange;
  DomOK: Boolean;
begin
  Result := 0;
  FDomTree := TDominatorTree(FProgram.GetDomTree);
  if FDomTree = nil then Exit;
  BuildDefMap;
  if FHasErrFlow then
  begin
    // ON ERROR / RESUME / TRAP: RESUME <label> can re-enter a loop body without
    // passing the header guard and those edges are not in the CFG. No proof
    // survives that, so the whole analysis stands down.
    {$IFDEF DEBUG_RANGE}
    if DebugRange then
      WriteLn('[Range] error-flow ops present: analysis disabled');
    {$ENDIF}
    Exit;
  end;
  // Before the array facts: their extents can come from a Const backing whose value is staged like
  // any other argument, and the loop screens below read parameter ranges too.
  BuildCallArgs;
  BuildArrayFacts;
  BuildLoops;
  for b := 0 to FProgram.Blocks.Count - 1 do
  begin
    Blk := FProgram.Blocks[b];
    for i := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := Blk.Instructions[i];
      if not OpIn(Instr.OpCode, [ssaArrayLoad, ssaArrayStore]) then Continue;
      if Instr.Src1.Kind <> svkArrayRef then Continue;
      a := Instr.Src1.ArrayIndex;
      if (a < 0) or (a > High(FArrFacts)) or not FArrFacts[a].Eligible then Continue;
      // The single DIM must dominate the access.
      if FArrFacts[a].DimBlock = Blk then
        DomOK := FArrFacts[a].DimIndex < i
      else
        DomOK := FDomTree.IsDom(FArrFacts[a].DimBlock, Blk);
      if not DomOK then Continue;
      // Symbolic extents are proven on their own path: TotalSize is 0 there, so the numeric test
      // below can never fire for them and the two are mutually exclusive by construction.
      if FArrFacts[a].SymExtent then
      begin
        if ProveSymbolic(Instr.Src2, Blk, i, a) then
        begin
          Instr.BoundsSafe := True;
          Inc(Result);
          {$IFDEF DEBUG_RANGE}
          if DebugRange then
            WriteLn('[Range] SAFE ', SSAOpCodeToString(Instr.OpCode), ' arr=', a,
                    ' SYMBOLIC (the loop runs over the array''s own extent) @', Blk.LabelName);
          {$ENDIF}
        end
        else
        begin
          {$IFDEF DEBUG_RANGE}
          if DebugRange then
            WriteLn('[Range] unsafe ', SSAOpCodeToString(Instr.OpCode), ' arr=', a,
                    ' symbolic proof failed @', Blk.LabelName);
          {$ENDIF}
        end;
        Continue;
      end;
      R := EvalRange(Instr.Src2, Blk, i, 0);
      if R.Known and (R.Lo >= 0) and (R.Hi < FArrFacts[a].TotalSize) then
      begin
        Instr.BoundsSafe := True;
        Inc(Result);
        {$IFDEF DEBUG_RANGE}
        if DebugRange then
          WriteLn('[Range] SAFE ', SSAOpCodeToString(Instr.OpCode),
                  ' arr=', a, ' idx=[', R.Lo, ',', R.Hi, '] < ',
                  FArrFacts[a].TotalSize, ' @', Blk.LabelName);
        {$ENDIF}
      end
      {$IFDEF DEBUG_RANGE}
      else if DebugRange then
      begin
        if R.Known then
          WriteLn('[Range] unsafe ', SSAOpCodeToString(Instr.OpCode),
                  ' arr=', a, ' idx=[', R.Lo, ',', R.Hi, '] vs ',
                  FArrFacts[a].TotalSize, ' @', Blk.LabelName)
        else
          WriteLn('[Range] unsafe ', SSAOpCodeToString(Instr.OpCode),
                  ' arr=', a, ' idx=UNKNOWN @', Blk.LabelName);
      end
      {$ENDIF}
      ;
    end;
  end;
  {$IFDEF DEBUG_RANGE}
  if DebugRange then
    WriteLn('[Range] accesses proven safe: ', Result);
  {$ENDIF}
end;

end.
