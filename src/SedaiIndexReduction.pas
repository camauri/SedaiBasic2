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
  Unit: SedaiIndexReduction (induction-variable strength reduction on ARRAY INDICES)

  An index computed as "loop-invariant + counter" is recomputed from scratch on
  every iteration. It is an induction variable in its own right, so it can be
  computed once before the loop and advanced by the counter's step instead:

      For j = 0 To n-1                      p = base          ' once
        a(base + j) = ...                   For j = 0 To n-1
      Next j                                  a(p) = ...
                                              p += 1
                                            Next j

  Two instructions per index per iteration become one, and the emitted code loses
  the register copy that the three-address form needs (`mov r14,r15 / add r14,r9`
  becomes `add r14,step`). matmul's inner loop has three such indices.

  ⛔⛔ WHY THIS PASS COULD NOT EXIST BEFORE 16 Aug 2026, AND WHY IT IS SAFE NOW.
  The transformed shape used to be SLOWER, and by a lot. Written out by hand,
  matmul with running indices ran 846 ms against the recomputing form's 598 -
  41% worse - because the range analysis proves "i*NM+j" in bounds (it knows i
  and j) and a running index was opaque to it, so every access got its bounds
  guard back. Three guards cost far more than six address instructions save.
  ⇒ This pass is only profitable because SedaiRangeAnalysis.EvalDerivedIV now
  proves exactly the shape it produces: a register that advances beside the loop
  counter inherits the counter's proven range. THE TWO ARE ONE MECHANISM. If the
  derived-IV proof is ever narrowed, this pass turns into a pessimisation, and
  the guard against that is bug_derived_iv_bounds.bas.

  ⭐ Hence the recognised shape is deliberately the shape that proof accepts, not
  the widest one that would be legal: one canonical counted loop, one latch, one
  constant step, the index dead after the loop. A wider recogniser here would
  emit indices the analysis cannot prove, and every one of those is a net loss.

  Where it runs: after LICM (which is what creates the preheader this needs) and
  before DCE and the range analysis.

  ⛔⛔⛔ OFF BY DEFAULT — IT MISCOMPILES. INDEXRED=1 turns it on; do not change
  that default until the sentinel below is green.

  What is wrong is CounterInit: it does not reliably find the value the counter
  holds when the loop is entered. job/tests/bas/bug_index_reduction.bas is four
  loops whose counter does NOT start at zero, and every one of them comes out
  wrong with the pass on:

      A  base + j, j from 1, nested        340   should be 360
      B  j from 2 step 3                    63   should be 81
      C  j from -1                         138   should be 126
      D  one counter reused, starts 7 / 0   30   should be 72

  A is exactly one step low per iteration (the sum for j in 0..4 instead of
  1..5), so the initial constant came back 0 where it should have been 1. The
  most likely reason, not yet proven: this unit compares registers by RegIndex
  and IGNORES THE SSA VERSION, so a walk backwards from the preheader can match
  a different loop's counter that happens to share the index - which is also the
  shape case D exercises hardest, and case D is the worst wrong.

  ⭐ WHAT A CORRECT FIX HAS TO CHOOSE BETWEEN, both known to work in isolation:
   - find the entry value soundly (respect the version, and prove no def of the
     counter lies between the one found and the header); or
   - do not look for it at all: MOVE the original "Rd := Inv + Iv" instruction
     into the preheader, which computes the first index correctly by
     construction. That is exact, and it costs the bounds proof - the range
     analysis cannot evaluate a counter outside its own loop, so the index comes
     out with an unknown initial range and every access gets its guard back.
     Measured before this pass existed: that is worth +41%, i.e. it turns the
     whole thing into a pessimisation. So this route needs the range analysis
     extended too.

  ⚠️ AND DO NOT TRUST THE FIRST BENCHMARK RUN OF IT. An earlier, wider version of
  this pass (it transformed any "invariant + counter", not only array indices)
  measured matmul_l1 at -27.9% - from a binary that was miscompiling. The
  narrowing to array indices removed the SYMPTOM that run_regress could see
  (three OPTDIFFs) and left the defect underneath, which is why the sentinel
  above exists and why it must be believed over any timing table.
  ============================================================================ }

unit SedaiIndexReduction;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, SedaiSSATypes;

type
  TIndexReduction = class
  private
    FProgram: TSSAProgram;
    FCount: Integer;
    function Enabled: Boolean;
    function LoopStepOfIn(Body: TSSABasicBlock; const V: TSSAValue;
                          out StepVal: TSSAValue; out DefIdx: Integer): Boolean;
    function DefinedIn(Blk: TSSABasicBlock; const V: TSSAValue): Boolean;
    function UsedOutside(Body: TSSABasicBlock; const V: TSSAValue): Boolean;
    function UsesInBlock(Blk: TSSABasicBlock; const V: TSSAValue): Integer;
    function AllUsesAreIndices(Blk: TSSABasicBlock; const V: TSSAValue): Boolean;
    function CounterInit(PreH: TSSABasicBlock; const Iv: TSSAValue; out K: Int64): Boolean;
    procedure ReduceLoop(H, Body, PreH: TSSABasicBlock);
  public
    constructor Create(AProgram: TSSAProgram);
    function Run: Integer;
  end;

implementation

{$IFDEF DEBUG_STRENGTH}
uses SedaiDebug;
{$ENDIF}

var
  GEnabled: Integer = -1;

constructor TIndexReduction.Create(AProgram: TSSAProgram);
begin
  inherited Create;
  FProgram := AProgram;
  FCount := 0;
end;

function TIndexReduction.Enabled: Boolean;
begin
  if GEnabled < 0 then
  begin
    if GetEnvironmentVariable('INDEXRED') = '1' then GEnabled := 1 else GEnabled := 0;
  end;
  Result := GEnabled = 1;
end;

function TIndexReduction.DefinedIn(Blk: TSSABasicBlock; const V: TSSAValue): Boolean;
var i: Integer; Ins: TSSAInstruction;
begin
  Result := False;
  if V.Kind <> svkRegister then Exit;
  for i := 0 to Blk.Instructions.Count - 1 do
  begin
    Ins := Blk.Instructions[i];
    if (Ins.Dest.Kind = svkRegister) and (Ins.Dest.RegType = V.RegType) and
       (Ins.Dest.RegIndex = V.RegIndex) then Exit(True);
  end;
end;

function TIndexReduction.UsesInBlock(Blk: TSSABasicBlock; const V: TSSAValue): Integer;
  function Is_(const A: TSSAValue): Boolean;
  begin
    Result := (A.Kind = svkRegister) and (A.RegType = V.RegType) and (A.RegIndex = V.RegIndex);
  end;
var i, n: Integer; Ins: TSSAInstruction;
begin
  n := 0;
  for i := 0 to Blk.Instructions.Count - 1 do
  begin
    Ins := Blk.Instructions[i];
    if Is_(Ins.Src1) then Inc(n);
    if Is_(Ins.Src2) then Inc(n);
    if Is_(Ins.Src3) then Inc(n);
  end;
  Result := n;
end;

function TIndexReduction.AllUsesAreIndices(Blk: TSSABasicBlock; const V: TSSAValue): Boolean;
// Every mention of V in the block is the INDEX operand of an array access (Src2 of ssaArrayLoad /
// ssaArrayStore). One mention anywhere else and the answer is no - a value that is also arithmetic,
// or printed, or stored, is not what this pass is for.
  function Is_(const A: TSSAValue): Boolean;
  begin
    Result := (A.Kind = svkRegister) and (A.RegType = V.RegType) and (A.RegIndex = V.RegIndex);
  end;
var i: Integer; Ins: TSSAInstruction; n: Integer;
begin
  Result := False; n := 0;
  for i := 0 to Blk.Instructions.Count - 1 do
  begin
    Ins := Blk.Instructions[i];
    if Is_(Ins.Dest) and (Ins.OpCode <> ssaArrayStore) then
    begin
      if i <> 0 then ;                                  // the defining add itself; checked by caller
    end;
    if (Ins.OpCode = ssaArrayLoad) or (Ins.OpCode = ssaArrayStore) then
    begin
      if Is_(Ins.Src2) then Inc(n);
      if Is_(Ins.Src1) or Is_(Ins.Src3) then Exit;      // as the array id or the stored value: no
      if (Ins.OpCode = ssaArrayStore) and Is_(Ins.Dest) then Exit;
    end
    else
    begin
      // any other instruction may only MENTION it as the destination of its own defining add
      if Is_(Ins.Src1) or Is_(Ins.Src2) or Is_(Ins.Src3) then Exit;
    end;
  end;
  Result := n > 0;
end;

function TIndexReduction.UsedOutside(Body: TSSABasicBlock; const V: TSSAValue): Boolean;
// Any mention of V - read OR written - anywhere but the loop body. Deliberately blunt: the index
// must be entirely local to this loop, because the pass leaves it holding one step MORE than the
// original did once the loop exits, and anything that could observe that is a reason to decline.
  function Is_(const A: TSSAValue): Boolean;
  begin
    Result := (A.Kind = svkRegister) and (A.RegType = V.RegType) and (A.RegIndex = V.RegIndex);
  end;
var b, i, k: Integer; Blk: TSSABasicBlock; Ins: TSSAInstruction;
begin
  Result := True;
  for b := 0 to FProgram.Blocks.Count - 1 do
  begin
    Blk := FProgram.Blocks[b];
    if Blk = Body then Continue;
    for i := 0 to Blk.Instructions.Count - 1 do
    begin
      Ins := Blk.Instructions[i];
      if Is_(Ins.Dest) or Is_(Ins.Src1) or Is_(Ins.Src2) or Is_(Ins.Src3) then Exit;
      for k := 0 to High(Ins.PhiSources) do
        if Is_(Ins.PhiSources[k].Value) then Exit;
    end;
  end;
  Result := False;
end;

function TIndexReduction.LoopStepOfIn(Body: TSSABasicBlock; const V: TSSAValue;
                                      out StepVal: TSSAValue; out DefIdx: Integer): Boolean;
// V is advanced exactly once in Body, by "V := V + <something>", and that something is not written
// in the body (so it is the same amount every iteration). Returns the addend, and in DefIdx the
// index of the ADD - which is what candidate indices must come before.
//
// ⛔ The FOR increment is lowered as a PAIR, "tmp := V + step; V := Copy tmp", so a recogniser that
// only accepts "V := V + step" finds a CopyInt and gives up - which is exactly what this did, and
// it rejected every loop in the program. SedaiRangeAnalysis.TraceStep already documents the same
// shape and hops through it; this is the same hop, kept to ONE level because that is the only form
// the lowering emits and a deeper walk would be untested generality.
var i, n, TmpIdx: Integer; Ins, Def2: TSSAInstruction; Tmp: TSSAValue;

  function IsV(const A: TSSAValue): Boolean;
  begin
    Result := (A.Kind = svkRegister) and (A.RegType = V.RegType) and (A.RegIndex = V.RegIndex);
  end;

begin
  Result := False; DefIdx := -1;
  StepVal := MakeSSAValue(svkNone);
  n := 0;
  for i := 0 to Body.Instructions.Count - 1 do
  begin
    Ins := Body.Instructions[i];
    if IsV(Ins.Dest) then
    begin
      Inc(n);
      if n > 1 then Exit;                                    // two updates: no single step
      if Ins.OpCode = ssaAddInt then
      begin
        if not IsV(Ins.Src1) then Exit;                      // must be V := V + x
        StepVal := Ins.Src2;
        DefIdx := i;
      end
      else if Ins.OpCode = ssaCopyInt then
      begin
        // V := Copy tmp, with tmp := V + step earlier in the same block and used nowhere else
        Tmp := Ins.Src1;
        if Tmp.Kind <> svkRegister then Exit;
        TmpIdx := -1;
        for TmpIdx := 0 to i - 1 do
        begin
          Def2 := Body.Instructions[TmpIdx];
          if (Def2.Dest.Kind = svkRegister) and (Def2.Dest.RegType = Tmp.RegType) and
             (Def2.Dest.RegIndex = Tmp.RegIndex) then Break;
          Def2 := nil;
        end;
        if Def2 = nil then Exit;
        if Def2.OpCode <> ssaAddInt then Exit;
        if not IsV(Def2.Src1) then Exit;
        StepVal := Def2.Src2;
        DefIdx := TmpIdx;                                    // the ADD, not the copy
      end
      else Exit;
    end;
  end;
  if n <> 1 then Exit;
  if (StepVal.Kind = svkRegister) and DefinedIn(Body, StepVal) then Exit;  // step not invariant
  Result := True;
end;

function TIndexReduction.CounterInit(PreH: TSSABasicBlock; const Iv: TSSAValue;
                                     out K: Int64): Boolean;
// The constant the counter holds when the loop is ENTERED, found by walking back from the preheader
// through unique predecessors - the same walk SedaiRangeAnalysis uses to find an IV's initial value,
// and for the same reason: blocks are basic, so every entry into the loop executes that whole chain
// and the last def on it is what the header sees.
//
// ⛔ WHY A CONSTANT AND NOT THE COUNTER ITSELF. The obvious initialiser is "P := invariant + counter"
// written into the preheader, and it is even correct - the counter does hold its initial value
// there. But the range analysis cannot EVALUATE the counter outside its own loop (it is an
// induction variable of a loop the preheader is not inside), so the new index came out with an
// unknown initial range and every access got its bounds guard back. Which is the failure this whole
// pass exists to avoid, arriving through the front door.
var
  lvl, i: Integer;
  Blk: TSSABasicBlock;
  Ins, Found: TSSAInstruction;
begin
  Result := False; K := 0;
  Blk := PreH;
  for lvl := 0 to 4 do
  begin
    Found := nil;
    for i := 0 to Blk.Instructions.Count - 1 do
    begin
      Ins := Blk.Instructions[i];
      if (Ins.Dest.Kind = svkRegister) and (Ins.Dest.RegType = Iv.RegType) and
         (Ins.Dest.RegIndex = Iv.RegIndex) then Found := Ins;      // keep the LAST one
    end;
    if Found <> nil then
    begin
      if Found.OpCode <> ssaLoadConstInt then Exit;                // not a constant start: decline
      if Found.Src1.Kind <> svkConstInt then Exit;
      K := Found.Src1.ConstInt;
      Exit(True);
    end;
    if Blk.Predecessors.Count <> 1 then Exit;
    Blk := TSSABasicBlock(Blk.Predecessors[0]);
  end;
end;

procedure TIndexReduction.ReduceLoop(H, Body, PreH: TSSABasicBlock);
var
  i, k, q, IvDefIdx, TermIdx: Integer;
  KInit: Int64;
  Cmp, Jump, Ins, Init, Bump: TSSAInstruction;
  Iv, IvStep, Inv, Rd, P: TSSAValue;
  Cand: TSSAInstruction;

  procedure RWhy(const Msg: string);
  begin
    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then WriteLn('[IndexRed] reject @', H.LabelName, ': ', Msg);
    {$ENDIF}
  end;

  function SameR(const A, B: TSSAValue): Boolean;
  begin
    Result := (A.Kind = svkRegister) and (B.Kind = svkRegister) and
              (A.RegType = B.RegType) and (A.RegIndex = B.RegIndex);
  end;

  procedure Retarget(const From, Onto: TSSAValue);
  var q: Integer; I2: TSSAInstruction;
  begin
    for q := 0 to Body.Instructions.Count - 1 do
    begin
      I2 := Body.Instructions[q];
      if SameR(I2.Src1, From) then I2.Src1 := Onto;
      if SameR(I2.Src2, From) then I2.Src2 := Onto;
      if SameR(I2.Src3, From) then I2.Src3 := Onto;
    end;
  end;

begin
  // --- the header must be a counted guard on some integer register -------------------------------
  Jump := nil; Cmp := nil;
  for i := H.Instructions.Count - 1 downto 0 do
    if (H.Instructions[i].OpCode = ssaJumpIfZero) or
       (H.Instructions[i].OpCode = ssaJumpIfNotZero) then begin Jump := H.Instructions[i]; Break; end;
  if Jump = nil then begin RWhy('header has no conditional jump'); Exit; end;
  for i := 0 to H.Instructions.Count - 1 do
    if (H.Instructions[i].Dest.Kind = svkRegister) and SameR(H.Instructions[i].Dest, Jump.Src1) then
      Cmp := H.Instructions[i];
  if Cmp = nil then begin RWhy('no def for the condition'); Exit; end;
  if not (Cmp.OpCode in [ssaCmpLeInt, ssaCmpLtInt]) then begin RWhy('condition is not a counted test'); Exit; end;
  Iv := Cmp.Src1;
  if Iv.Kind <> svkRegister then begin RWhy('counter not a register'); Exit; end;
  if Iv.RegType <> srtInt then begin RWhy('counter not integer'); Exit; end;
  // the counter advances once per iteration by an invariant amount
  if not LoopStepOfIn(Body, Iv, IvStep, IvDefIdx) then begin RWhy('counter has no single invariant step in the body'); Exit; end;
  // the counter's value on entry, as a constant - see CounterInit for why it must be one
  if not CounterInit(PreH, Iv, KInit) then begin RWhy('counter start is not a constant'); Exit; end;

  // where the body ends: the terminator (the back edge) stays last
  TermIdx := Body.Instructions.Count - 1;
  if TermIdx < 0 then begin RWhy('empty body'); Exit; end;
  if Body.Instructions[TermIdx].OpCode <> ssaJump then begin RWhy('body does not end in a jump'); Exit; end;

  // --- candidates: rD := invariant + counter, computed BEFORE the counter advances ---------------
  i := 0;
  while i < Body.Instructions.Count do
  begin
    Cand := Body.Instructions[i];
    if (Cand.OpCode <> ssaAddInt) or (i >= IvDefIdx) then begin Inc(i); Continue; end;
    // "invariant + counter" in either operand order
    if SameR(Cand.Src2, Iv) then Inv := Cand.Src1
    else if SameR(Cand.Src1, Iv) then Inv := Cand.Src2
    else begin Inc(i); Continue; end;
    if Inv.Kind <> svkRegister then begin Inc(i); Continue; end;
    if SameR(Inv, Iv) then begin Inc(i); Continue; end;         // counter + counter is not an index
    if DefinedIn(Body, Inv) then begin Inc(i); Continue; end;   // addend must be loop-invariant
    Rd := Cand.Dest;
    if (Rd.Kind <> svkRegister) or (Rd.RegType <> srtInt) then begin Inc(i); Continue; end;
    if SameR(Rd, Iv) or SameR(Rd, Inv) then begin Inc(i); Continue; end;
    // exactly one def of rD in the body (this one), and nothing outside the body mentions it
    k := 0;
    for q := 0 to Body.Instructions.Count - 1 do
      if SameR(Body.Instructions[q].Dest, Rd) then Inc(k);
    if k <> 1 then begin Inc(i); Continue; end;
    if UsedOutside(Body, Rd) then begin Inc(i); Continue; end;
    if UsesInBlock(Body, Rd) = 0 then begin Inc(i); Continue; end;  // dead already; leave it to DCE
    // ⛔ AND IT MUST ACTUALLY BE AN ARRAY INDEX - every use of it, not merely one. The first version
    // of this pass transformed any "invariant + counter", which is algebraically the same thing and
    // is NOT the same risk: it rewrote `i * 10 + k` in a printed expression and got the initial
    // value wrong, and run_regress caught it as OPTDIFF on three programs. Narrowing to the
    // instruction's own domain removes that entire class at a stroke, and the pass is called index
    // reduction for a reason.
    if not AllUsesAreIndices(Body, Rd) then begin RWhy('not used only as an array index'); Inc(i); Continue; end;

    // --- rewrite ---------------------------------------------------------------------------------
    P := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    // once, in the preheader: P := invariant + counter-on-entry. The preheader runs exactly once per
    // loop entry and the counter still holds its initial value there, so this is the first index.
    if KInit = 0 then
    begin
      Init := TSSAInstruction.Create(ssaCopyInt);
      Init.Dest := P; Init.Src1 := Inv;
    end
    else
    begin
      Init := TSSAInstruction.Create(ssaAddInt);
      Init.Dest := P; Init.Src1 := Inv; Init.Src2 := MakeSSAConstInt(KInit);
    end;
    Init.SourceLine := Cand.SourceLine;
    Init.Comment := 'index reduction: initial ' + IntToStr(Rd.RegIndex);
    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
    begin
      WriteLn('[IndexRed] preheader ', PreH.LabelName, ' contents:');
      for k := 0 to PreH.Instructions.Count - 1 do
        WriteLn('[IndexRed]   ', PreH.Instructions[k].ToString);
    end;
    {$ENDIF}
    if (PreH.Instructions.Count > 0) and
       (PreH.Instructions[PreH.Instructions.Count - 1].OpCode in [ssaJump, ssaJumpIfZero, ssaJumpIfNotZero]) then
      PreH.Instructions.Insert(PreH.Instructions.Count - 1, Init)
    else
      PreH.Instructions.Add(Init);
    // the uses move onto P, then the recomputation goes away
    Retarget(Rd, P);
    Body.Instructions.Delete(i);
    // and P advances by the counter's own step, at the very end of the body - AFTER every use of it
    Bump := TSSAInstruction.Create(ssaAddInt);
    Bump.Dest := P; Bump.Src1 := P; Bump.Src2 := IvStep;
    Bump.SourceLine := Cand.SourceLine;
    Bump.Comment := 'index reduction: advance';
    Body.Instructions.Insert(Body.Instructions.Count - 1, Bump);
    Inc(FCount);
    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
      WriteLn('[IndexRed] ', H.LabelName, ': r', Rd.RegIndex, ' = r', Inv.RegIndex,
              ' + r', Iv.RegIndex, '  ->  running r', P.RegIndex);
    {$ENDIF}
    // the body shrank by one and grew by one; IvDefIdx moved, so re-derive it and rescan from here
    if not LoopStepOfIn(Body, Iv, IvStep, IvDefIdx) then begin RWhy('counter has no single invariant step in the body'); Exit; end;
  end;
end;

function TIndexReduction.Run: Integer;
var
  b, p: Integer;
  H, Body, PreH, Pb: TSSABasicBlock;
  Term: TSSAInstruction;
begin
  Result := 0;
  if not Enabled then Exit;
  // The canonical counted loop, and nothing else: a header H, a single body block whose only
  // successor is H, and exactly one entry into H from outside. Anything less regular is left alone -
  // the point is not to catch every loop, it is to emit only indices the range analysis can prove.
  {$IFDEF DEBUG_STRENGTH}
  if DebugStrength then
    for b := 0 to FProgram.Blocks.Count - 1 do
      WriteLn('[IndexRed] block ', FProgram.Blocks[b].LabelName,
              ' preds=', FProgram.Blocks[b].Predecessors.Count,
              ' succs=', FProgram.Blocks[b].Successors.Count);
  {$ENDIF}
  for b := 0 to FProgram.Blocks.Count - 1 do
  begin
    H := FProgram.Blocks[b];
    if H.Predecessors.Count <> 2 then Continue;
    Body := nil; PreH := nil;
    for p := 0 to 1 do
    begin
      Pb := TSSABasicBlock(H.Predecessors[p]);
      // ⛔ The LATCH, not merely "a predecessor that jumps here". The PREHEADER also has exactly one
      // successor and also ends in a jump to the header - LICM builds it that way - so testing only
      // those two properties matches both predecessors, and the "two latches" guard below then
      // rejected every loop in the program. What separates them is where they are ENTERED from: the
      // latch is reached from the header, the preheader from outside the loop.
      if (Pb.Successors.Count = 1) and (TSSABasicBlock(Pb.Successors[0]) = H) and
         (Pb.Predecessors.Count = 1) and (TSSABasicBlock(Pb.Predecessors[0]) = H) then
      begin
        if Pb.Instructions.Count = 0 then Continue;
        Term := Pb.Instructions[Pb.Instructions.Count - 1];
        if Term.OpCode = ssaJump then
        begin
          if Body <> nil then begin Body := nil; Break; end;   // two latches: not canonical
          Body := Pb;
        end;
      end;
    end;
    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
      WriteLn('[IndexRed] header ', H.LabelName, ': body=',
              BoolToStr(Body <> nil, 'yes', 'no'));
    {$ENDIF}
    if Body = nil then Continue;
    for p := 0 to 1 do
    begin
      Pb := TSSABasicBlock(H.Predecessors[p]);
      if Pb <> Body then PreH := Pb;
    end;
    if (PreH = nil) or (PreH = Body) then Continue;
    // the body must be reached only from the header (so it runs once per guarded iteration)
    if Body.Predecessors.Count <> 1 then Continue;
    if TSSABasicBlock(Body.Predecessors[0]) <> H then Continue;
    ReduceLoop(H, Body, PreH);
  end;
  Result := FCount;
end;

end.
