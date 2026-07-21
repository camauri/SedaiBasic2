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
    TotalSize: Int64;         // product of constant extents
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

  TRangeAnalysis = class
  private
    FProgram: TSSAProgram;
    FDomTree: TDominatorTree;
    FDefs: TStringList;         // 'idx:ver' -> head index into FDefRecs (int bank only)
    FDefRecs: array of TDefRec;
    FLoops: array of TLoopRec;
    FArrFacts: array of TArrFact;
    FHasErrFlow: Boolean;       // ON ERROR / RESUME / TRAP present -> analysis disabled
    function DefKey(const V: TSSAValue): string; inline;
    function FindDefIdx(const V: TSSAValue): Integer;                 // head index, -1 = no def
    function FindDef(const V: TSSAValue; out D: TDefRec): Boolean;    // True only if SINGLE def
    function SameReg(const A, B: TSSAValue): Boolean; inline;
    procedure BuildDefMap;
    procedure BuildArrayFacts;
    procedure BuildLoops;
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
                       Depth: Integer): TRange;
    function TryUnversionedIV(const V: TSSAValue; UseBlock: TSSABasicBlock;
                              UseIndex, Depth: Integer): TRange;
    function TraceStep(const LatchVal, PhiDest: TSSAValue; out Step: Int64): Boolean;
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
  BENIGN: array[0..6] of TSSAOpCode = (
    ssaArrayLoad, ssaArrayStore, ssaArrayDim, ssaArrayLBound, ssaArrayUBound,
    ssaArrayIdxPush, ssaArrayIdxResolve);
var
  a, b, i, d, DimSeen: Integer;
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
      FArrFacts[ArrId].Eligible := False;
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
    FArrFacts[a].TotalSize := 0;
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
      if (Instr.OpCode = ssaArrayBind) and (Instr.Src3.Kind = svkConstInt) and
         (Instr.Src3.ConstInt >= 0) and (Instr.Src3.ConstInt <= High(FArrFacts)) then
        FArrFacts[Instr.Src3.ConstInt].Eligible := False;
      if (Instr.OpCode = ssaArrayDim) and (Instr.Src1.Kind = svkArrayRef) then
      begin
        a := Instr.Src1.ArrayIndex;
        if (a >= 0) and (a <= High(FArrFacts)) then
        begin
          if FArrFacts[a].DimBlock <> nil then
            FArrFacts[a].Eligible := False   // second DIM: extent no longer single-valued
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
  // An array never DIM'd is never proven (its descriptor may be empty).
  for a := 0 to High(FArrFacts) do
    if FArrFacts[a].DimBlock = nil then
      FArrFacts[a].Eligible := False;
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
    if not FArrFacts[a].Eligible then Continue;
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
    if Total <= 0 then Continue;
    FArrFacts[a].Eligible := True;
    FArrFacts[a].TotalSize := Total;
    Inc(DimSeen);
  end;
  {$IFDEF DEBUG_RANGE}
  if DebugRange then
    WriteLn('[Range] eligible arrays: ', DimSeen, '/', Length(FArrFacts));
  {$ENDIF}
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
var
  DK: TDefRec;
begin
  Result := False;
  CV := 0;
  if X.Kind = svkConstInt then begin CV := X.ConstInt; Exit(True); end;
  if FindDef(X, DK) and (DK.Instr.OpCode = ssaLoadConstInt) and
     (DK.Instr.Src1.Kind = svkConstInt) then
  begin
    CV := DK.Instr.Src1.ConstInt;
    Result := True;
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
begin
  Result := False;
  Step := 0;
  V := LatchVal;
  Hops := 0;
  while Hops <= 3 do
  begin
    if not FindDef(V, D) then Exit;
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
          Exit;
        end;
      ssaSubInt:
        begin
          if SameReg(D.Instr.Src1, PhiDest) and ConstOf(D.Instr.Src2, C) then
          begin Step := -C; Exit(True); end;
          Exit;
        end;
    else
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

function TRangeAnalysis.EvalForIV(Phi: TSSAInstruction; H, UseBlock: TSSABasicBlock;
                                  Depth: Integer): TRange;
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
  if not TraceStep(LatchVal, Phi.Dest, Step) then begin Why('latch not phi+const'); Exit; end;
  if Step = 0 then begin Why('step 0'); Exit; end;
  if not FindGuard(H, li, Phi.Dest, Cmp) then begin Why('no guard on phi'); Exit; end;
  IR := EvalRange(InitVal, InitBlock, -1, Depth + 1);
  if not IR.Known then begin Why('init range unknown'); Exit; end;
  Result := GuardedRange(li, H, Cmp, Step, IR, Depth);
  {$IFDEF DEBUG_RANGE}
  if not Result.Known then Why('guard/limit combination failed');
  {$ENDIF}
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
    if not FindGuard(H_, li, V, Cmp) then begin UWhy('no guard'); Continue; end;
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
      Result := EvalForIV(D.Instr, D.Block, UseBlock, Depth);
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
