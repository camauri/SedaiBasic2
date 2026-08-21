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
  Unit: SedaiLoopUnroll (Loop Unrolling Optimization)

  Purpose: Duplicate loop bodies to reduce loop overhead and enable more
           instruction-level parallelism.

  Algorithm (CORRECT implementation):
    1. Detect natural loops using back-edge analysis
    2. Identify candidate loops for unrolling:
       - Simple structure (single back-edge, single latch block)
       - Loop body not too large (< MAX_BODY_SIZE instructions)
    3. Find the induction variable (IV) and its increment instruction
    4. For 2x unrolling:
       a. Clone the loop body instructions
       b. Insert IV increment between original and cloned body
       c. Update cloned instructions to use the incremented IV
    5. The loop still runs with original step - we just do 2 iterations per cycle

  Key insight: We DON'T change the loop step. Instead, we:
    - Execute body with IV
    - Increment IV
    - Execute body again with IV+step
    - Increment IV (this is the original increment, now at IV+2*step)
    - Loop back

  ⛔ THE LINE BELOW USED TO READ "Any trip count (no epilogue needed for simple 2x unroll with
  same step)" AND THAT WAS FALSE. Re-testing the condition once per PAIR runs the body one extra
  time whenever the trip count is ODD. It went unnoticed for as long as it did because the
  induction-variable matcher was broken and the pass transformed nothing; the day the matcher was
  repaired (21 Aug 2026) four corpus programs broke at once. See TripCountIsProvablyEven, which is
  where the pass now refuses, and why.

  This correctly handles:
    - Array accesses like FLAGS(I) - each copy accesses different element
    - EVEN trip counts only. An odd one needs an epilogue this pass does not build.

  Phase: Advanced Optimization (after LICM, before DCE)
  Author: Sedai Project - Performance Optimization
  Date: 2025-11-29 (Rewritten for correctness)
  ============================================================================ }

unit SedaiLoopUnroll;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I DebugFlags.inc}
{$I OptimizationFlags.inc}

interface

uses
  Classes, SysUtils, Contnrs, SedaiSSATypes, SedaiDominators;

type
  { Forward declarations }
  TUnrollableLoop = class;

  { TInductionVarInfo - Information about the loop's induction variable }
  TInductionVarInfo = record
    Found: Boolean;              // True if we identified the IV
    PhiInstr: TSSAInstruction;   // PHI node in header (if SSA form)
    IncrInstr: TSSAInstruction;  // The increment instruction (IV = IV + step)
    IncrInstrIndex: Integer;     // Index of increment instruction in its block
    IncrBlock: TSSABasicBlock;   // Block containing the increment
    StepValue: Int64;            // The step value (usually 1)
    StepIsConst: Boolean;        // True if step is a constant
    IVRegIndex: Integer;         // Register index of IV
    IVVersion: Integer;          // SSA version of IV (before increment)
    IVRegType: TSSARegisterType; // Type of IV (int or float)
  end;

  { TLoopUnroller - Loop unrolling optimizer }
  TLoopUnroller = class
  private
    FProgram: TSSAProgram;
    FUnrollFactor: Integer;      // How many times to duplicate (default: 2)
    FMaxBodySize: Integer;       // Max instructions in loop body to unroll
    FUnrolledCount: Integer;     // Number of loops unrolled
    // O(1) lookup using BlockIndex instead of string hash
    FDominatorMap: array of TSSABasicBlock;  // FDominatorMap[blockIndex] = idom block
    FBlockCount: Integer;

    { Build dominator map from program's dominator tree }
    procedure BuildDominatorMap;

    { Check if edge (From -> Target) is a back-edge }
    function IsBackEdge(From, Target: TSSABasicBlock): Boolean;
    function IsCallEdge(From, Target: TSSABasicBlock): Boolean;   // recursion, not a loop

    { Resolve a step operand that the SSA has materialised into a register }
    function ResolveConstStep(const Val: TSSAValue; out Step: Int64): Boolean;

    { Find candidate loops for unrolling }
    function FindUnrollableLoops: TObjectList;

    { Analyze a single loop for unrolling potential }
    function AnalyzeLoop(Header: TSSABasicBlock; BackEdgeSource: TSSABasicBlock): TUnrollableLoop;

    { Check if loop has simple structure suitable for unrolling }
    function HasSimpleStructure(Loop: TUnrollableLoop): Boolean;

    { Get loop body blocks }
    procedure GetLoopBlocks(Loop: TUnrollableLoop);

    { Count total instructions in loop body }
    function CountLoopInstructions(Loop: TUnrollableLoop): Integer;

    { Find the induction variable and its increment }
    function FindInductionVariable(Loop: TUnrollableLoop): TInductionVarInfo;

    { Check if instruction uses the induction variable }
    function UsesInductionVar(Instr: TSSAInstruction; const IVInfo: TInductionVarInfo): Boolean;

    { Clone an instruction, optionally offsetting IV references }
    function CloneInstructionWithIVOffset(Instr: TSSAInstruction;
      const IVInfo: TInductionVarInfo; IVOffsetReg: Integer): TSSAInstruction;

    { Apply unrolling transformation to a loop }
    function UnrollLoop(Loop: TUnrollableLoop): Boolean;

  public
    constructor Create(Prog: TSSAProgram);
    destructor Destroy; override;

    { Run loop unrolling - returns number of loops unrolled }
    function Run: Integer;

    property UnrollFactor: Integer read FUnrollFactor write FUnrollFactor;
    property MaxBodySize: Integer read FMaxBodySize write FMaxBodySize;
  end;

  { TUnrollableLoop - Information about a loop candidate for unrolling }
  TUnrollableLoop = class
  public
    Header: TSSABasicBlock;           // Loop header block
    Latch: TSSABasicBlock;            // Block with back-edge to header (latch)
    BodyBlocks: TFPList;              // All blocks in loop body
    ExitBlock: TSSABasicBlock;        // Block exited to after loop
    BodyInstrCount: Integer;          // Total instructions in body
    RegisterOnly: Boolean;            // Body never touches memory - see BodyIsRegisterOnly
    IVInfo: TInductionVarInfo;        // Induction variable information

    constructor Create(AHeader, ALatch: TSSABasicBlock);
    destructor Destroy; override;
    function ContainsBlock(Block: TSSABasicBlock): Boolean;
  end;

function RunLoopUnrolling(Prog: TSSAProgram): Integer;

implementation


uses SedaiDebug;

var
  // ⛔ UNROLL_DIAG=1: perche' questo passo non srotola MAI. L'audit del 20 ago 2026 ha misurato che
  // spegnerlo non cambia un byte su 162 programmi, ma i motivi di scarto erano dietro
  // {$IFDEF DEBUG_SSA} e il PRIMO cancello - HasSimpleStructure - scartava in SILENZIO.
  UD_Seen, UD_Struct, UD_Size, UD_NoIV, UD_Step, UD_Done: Integer;
  // Body size of every candidate rejected for want of an induction variable. The saving from a
  // 2x unroll is FIXED - one loop-condition test and one back-edge jump per pair of iterations -
  // so the body size is what turns that fixed saving into a percentage, and it is the only
  // number that decides whether repairing the matcher is worth anything. Measured 21 Aug 2026:
  // a body of one operation gains 20%, a body of an array-indexed float multiply-accumulate
  // gains 2%. The probes are hand-written BASIC in the internal test tree, not published.
  UD_NoIVSizes: array of Integer;
  UD_RegOnly: Integer;                  // candidates whose body never touches memory
  UD_NotProfitable: Integer;            // and how many the profitability gate turned away
  UD_NoEpilogue: Integer;               // and how many are correct-and-profitable but need an epilogue
  UD_Culprits: array of TSSAOpCode;     // and, for the rest, WHAT disqualified each one
  UD_CulpritN: array of Integer;


{ TUnrollableLoop }

constructor TUnrollableLoop.Create(AHeader, ALatch: TSSABasicBlock);
begin
  inherited Create;
  Header := AHeader;
  Latch := ALatch;
  BodyBlocks := TFPList.Create;
  ExitBlock := nil;
  BodyInstrCount := 0;
  RegisterOnly := False;
  FillChar(IVInfo, SizeOf(IVInfo), 0);
  BodyBlocks.Add(Pointer(Header));
end;

destructor TUnrollableLoop.Destroy;
begin
  BodyBlocks.Free;
  inherited;
end;

function TUnrollableLoop.ContainsBlock(Block: TSSABasicBlock): Boolean;
begin
  Result := BodyBlocks.IndexOf(Pointer(Block)) >= 0;
end;

{ TLoopUnroller }

constructor TLoopUnroller.Create(Prog: TSSAProgram);
begin
  inherited Create;
  FProgram := Prog;
  FUnrollFactor := 2;       // Conservative: 2x unrolling
  FMaxBodySize := 30;       // Allow slightly larger loops
  FUnrolledCount := 0;
  FBlockCount := FProgram.Blocks.Count;
  SetLength(FDominatorMap, FBlockCount);
end;

destructor TLoopUnroller.Destroy;
begin
  SetLength(FDominatorMap, 0);
  inherited;
end;

procedure TLoopUnroller.BuildDominatorMap;
var
  DomTree: TDominatorTree;
  i: Integer;
  Block, IdomBlock: TSSABasicBlock;
begin
  // Initialize all entries to nil
  FillChar(FDominatorMap[0], FBlockCount * SizeOf(TSSABasicBlock), 0);

  if not Assigned(FProgram.GetDomTree) then
    Exit;

  DomTree := TDominatorTree(FProgram.GetDomTree);

  for i := 0 to FBlockCount - 1 do
  begin
    Block := FProgram.Blocks[i];
    // Use BlockIndex for O(1) lookup instead of string hash
    if (Block.BlockIndex >= 0) and (Block.BlockIndex < FBlockCount) then
    begin
      try
        IdomBlock := DomTree.GetIdom(Block);
        FDominatorMap[Block.BlockIndex] := IdomBlock;  // nil is OK
      except
        FDominatorMap[Block.BlockIndex] := nil;
      end;
    end;
  end;
end;

function TLoopUnroller.IsCallEdge(From, Target: TSSABasicBlock): Boolean;
// True when From reaches Target by CALLING it. Such an edge is recursion, not a loop: the "body"
// it closes is a whole activation of the procedure, and treating it as a loop lets a loop pass
// reason about values that belong to DIFFERENT activations as if they were successive iterations
// of one. (LICM did exactly that and hoisted a per-activation value into a preheader the recursive
// calls then entered below - see the frame-relocation work.)
var
  i: Integer;
  Instr: TSSAInstruction;
begin
  Result := False;
  if (From = nil) or (Target = nil) then Exit;
  for i := 0 to From.Instructions.Count - 1 do
  begin
    Instr := TSSAInstruction(From.Instructions[i]);
    if (Instr.OpCode = ssaCallSub) or (Instr.OpCode = ssaCall) then
      if Instr.Dest.LabelName = Target.LabelName then Exit(True);
  end;
end;

function TLoopUnroller.IsBackEdge(From, Target: TSSABasicBlock): Boolean;
var
  Current: TSSABasicBlock;
  LoopCount: Integer;
begin
  // Back-edge: From dominates Target (Target is ancestor in dominator tree)
  // Walk up the dominator tree from From to see if we reach Target
  Result := False;
  Current := From;
  LoopCount := 0;

  while (Current <> nil) and (LoopCount < 1000) do
  begin
    Inc(LoopCount);
    if Current = Target then
    begin
      Result := True;
      Exit;
    end;
    // O(1) lookup using BlockIndex instead of string hash
    if (Current.BlockIndex < 0) or (Current.BlockIndex >= FBlockCount) then
      Break;
    Current := FDominatorMap[Current.BlockIndex];
  end;
end;

function TLoopUnroller.FindUnrollableLoops: TObjectList;
var
  i, j: Integer;
  Block, Succ: TSSABasicBlock;
  Loop: TUnrollableLoop;
begin
  Result := TObjectList.Create(True);

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];

    for j := 0 to Block.Successors.Count - 1 do
    begin
      Succ := TSSABasicBlock(Block.Successors[j]);

      if IsBackEdge(Block, Succ) and (not IsCallEdge(Block, Succ)) then
      begin
        Loop := AnalyzeLoop(Succ, Block);
        if Loop <> nil then
          Result.Add(Loop);
      end;
    end;
  end;
end;

function TripCountIsProvablyEven(Loop: TUnrollableLoop): Boolean;
// ⛔⛔ THE CORRECTNESS GUARD, AND THE REASON THIS PASS TRANSFORMS NOTHING TODAY.
//
// This pass unrolls by copying the body, incrementing the induction variable between the copies,
// and re-testing the loop condition ONCE per pair. The unit header above used to claim that needs
// no epilogue "for any trip count". It is not true: a loop meant to run an ODD number of times runs
// one iteration too many.
//
// Measured 21 Aug 2026, the day the induction-variable matcher was repaired and the pass fired for
// the first time (70 loops over the 162-program corpus). Four programs broke immediately:
//     mandelbrot-modern.bas, mandelbrot-modern-1t.bas .... picture differs from --no-opt
//     fannkuch-redux-modern.bas, k-nucleotide-modern.bas . do not terminate
// mandelbrot's innermost loop is "For h = 1 To 7" - seven trips, body in registers only - which is
// exactly the shape. Note that a profitability gate keyed on memory traffic does NOT hide this:
// that loop is one of the register-only ones such a gate would let through.
//
// The fix is an EPILOGUE - run (trip count mod 2) iterations first, then the pairs - which needs a
// trip count this pass never computes. That is writing the pass, not repairing it, so until someone
// does, the answer here is False and the pass reports how many loops are waiting on it rather than
// silently doing nothing. Everything upstream of this point - the matcher, the body classifier, the
// diagnostic - is live and measured, which is the whole point of refusing HERE and not at the
// {$IFNDEF} in the caller.
begin
  Result := False;
end;

function BodyIsRegisterOnly(Loop: TUnrollableLoop; out Culprit: TSSAOpCode): Boolean;
// Does this loop body do its work entirely in registers, or does it touch memory?
//
// ⭐ WHY THIS IS THE QUESTION. A 2x unroll saves a FIXED amount - one loop-condition test and one
// back-edge jump per pair of iterations. Measured 21 Aug 2026 with the bodies hand-written in the
// exact form the pass emits, at the SAME body size of 10 instructions:
//     registers only ......... -15.1%   (size12_rolled.bas vs size12_unrolled.bas)
//     one array read ......... -3.1%    (arr10_rolled.bas  vs arr10_unrolled.bas)
// The body SIZE is not what separates them - both are 10 instructions. What separates them is that
// the second one waits on memory, and the saved dispatch hides in that wait. So the value of
// repairing the induction-variable matcher is the fraction of candidates that are register-only.
//
// ⛔ THE LIST BELOW IS A CONSENSUS LIST, AND THAT IS DELIBERATE. Naming the opcodes that DO touch
// memory would be a list by omission: forget one and the answer is silently wrong. Naming the ones
// that do not means a forgotten opcode only makes a loop look worse than it is - a missed
// opportunity, never a wrong claim. And the caller reports every opcode that landed here unnamed,
// so the omission cannot ship unnoticed.
var
  bi, ii: Integer;
  Blk: TSSABasicBlock;
  Op: TSSAOpCode;
begin
  Result := True;
  Culprit := ssaPhi;
  for bi := 0 to Loop.BodyBlocks.Count - 1 do
  begin
    Blk := TSSABasicBlock(Loop.BodyBlocks[bi]);
    for ii := 0 to Blk.Instructions.Count - 1 do
    begin
      Op := TSSAInstruction(Blk.Instructions[ii]).OpCode;
      case Op of
        ssaPhi, ssaLoadConstInt, ssaLoadConstFloat, ssaCopyInt, ssaCopyFloat,
        ssaAddInt, ssaSubInt, ssaMulInt, ssaDivInt, ssaModInt, ssaNegInt,
        ssaDivUInt, ssaModUInt,
        ssaAddFloat, ssaSubFloat, ssaMulFloat, ssaDivFloat, ssaModFloat,
        ssaPowFloat, ssaNegFloat,
        ssaIntToFloat, ssaFloatToInt,
        ssaCmpEqInt, ssaCmpNeInt, ssaCmpLtInt, ssaCmpGtInt, ssaCmpLeInt, ssaCmpGeInt,
        ssaCmpLtUInt, ssaCmpGtUInt, ssaCmpLeUInt, ssaCmpGeUInt,
        ssaCmpEqFloat, ssaCmpNeFloat, ssaCmpLtFloat, ssaCmpGtFloat,
        ssaCmpLeFloat, ssaCmpGeFloat,
        ssaBitwiseAnd, ssaBitwiseOr, ssaBitwiseXor, ssaBitwiseNot,
        ssaShl, ssaShr, ssaShrUInt,
        ssaBitClz, ssaBitCtz, ssaBitPopcnt, ssaBitRotl, ssaBitRotr,
        ssaFloatRound, ssaNarrowInt, ssaNarrowSingle,
        ssaMathSin, ssaMathCos, ssaMathTan, ssaMathAtn, ssaMathLog, ssaMathExp,
        ssaMathSqr, ssaMathAbs, ssaMathSgn, ssaMathInt,
        ssaMathLog10, ssaMathLog2, ssaMathLogN,
        ssaMathAcos, ssaMathAsin, ssaMathAtan2, ssaMathFix, ssaMathFrac,
        ssaMathSinh, ssaMathCosh, ssaMathTanh,
        ssaMathAsinh, ssaMathAcosh, ssaMathAtanh,
        ssaMathCeil, ssaMathRound, ssaMathMin, ssaMathMax, ssaMathCopySign,
        ssaSingleBits, ssaBitsToSingle,
        ssaNop,   // left behind by DCE and the peephole; it is not work and not memory
        ssaLabel, ssaJump, ssaJumpIfZero, ssaJumpIfNotZero:
          ; // register work: no memory traffic for the saved dispatch to hide behind
      else
        Culprit := Op;
        Exit(False);
      end;
    end;
  end;
end;

procedure RecordCulprit(Op: TSSAOpCode);
var i: Integer;
begin
  for i := 0 to High(UD_Culprits) do
    if UD_Culprits[i] = Op then begin Inc(UD_CulpritN[i]); Exit; end;
  SetLength(UD_Culprits, Length(UD_Culprits) + 1);
  SetLength(UD_CulpritN, Length(UD_Culprits));
  UD_Culprits[High(UD_Culprits)] := Op;
  UD_CulpritN[High(UD_CulpritN)] := 1;
end;

procedure RecordBodySize(N: Integer);
begin
  SetLength(UD_NoIVSizes, Length(UD_NoIVSizes) + 1);
  UD_NoIVSizes[High(UD_NoIVSizes)] := N;
end;

function TLoopUnroller.AnalyzeLoop(Header: TSSABasicBlock;
  BackEdgeSource: TSSABasicBlock): TUnrollableLoop;
var
  Loop: TUnrollableLoop;
  DiagCulprit: TSSAOpCode;
begin
  Result := nil;

  Loop := TUnrollableLoop.Create(Header, BackEdgeSource);
  try
    GetLoopBlocks(Loop);

    Inc(UD_Seen);
    // Size FIRST, and for every candidate, because the size is the diagnostic. A candidate rejected
    // for its structure still tells us how big the loops in this corpus are, and the earlier version
    // measured only the ones that got past that gate - which is a sample selected by the very thing
    // being studied.
    Loop.BodyInstrCount := CountLoopInstructions(Loop);
    RecordBodySize(Loop.BodyInstrCount);
    Loop.RegisterOnly := BodyIsRegisterOnly(Loop, DiagCulprit);
    if Loop.RegisterOnly then Inc(UD_RegOnly) else RecordCulprit(DiagCulprit);

    if not HasSimpleStructure(Loop) then
    begin
      Inc(UD_Struct);
      Loop.Free;
      Exit;
    end;

    if Loop.BodyInstrCount > FMaxBodySize then
    begin
      Inc(UD_Size);
      {$IFDEF DEBUG_SSA}
      WriteLn('[UNROLL] Skipping loop at ', Header.LabelName,
        ': body too large (', Loop.BodyInstrCount, ' > ', FMaxBodySize, ')');
      {$ENDIF}
      Loop.Free;
      Exit;
    end;

    // PROFITABILITY. A 2x unroll saves a FIXED amount - one loop-condition test and one back-edge
    // jump per pair of iterations - and doubles the body. Whether that fixed saving is worth
    // anything depends on what the body DOES, not on how big it is. Measured 21 Aug 2026 on bodies
    // hand-written in the exact form this pass emits, at the SAME size of 10 instructions:
    //     registers only ....... -15.1%
    //     one array read ....... -3.1%
    // A body that waits on memory hides the dispatch this pass removes. Over the 162-program corpus
    // only 56 of 724 candidates (7.7%) are register-only - so the gate also confines the code growth
    // to that 7.7%, which is what makes the pass affordable on the MCU target, where the binding
    // constraint is SIZE and a longer loop that does not run faster is a regression, not a tie.
    // UNROLL_ALL=1 opens the gate, for measuring what it costs to keep it shut.
    if (not Loop.RegisterOnly) and (GetEnvironmentVariable('UNROLL_ALL') <> '1') then
    begin
      Inc(UD_NotProfitable);
      Loop.Free;
      Exit;
    end;

    // Find induction variable - REQUIRED for correct unrolling
    Loop.IVInfo := FindInductionVariable(Loop);
    if not Loop.IVInfo.Found then
    begin
      Inc(UD_NoIV);
      {$IFDEF DEBUG_SSA}
      WriteLn('[UNROLL] Skipping loop at ', Header.LabelName,
        ': could not identify induction variable');
      {$ENDIF}
      Loop.Free;
      Exit;
    end;

    // CORRECTNESS, and it comes last so the diagnostic can price everything upstream of it.
    if not TripCountIsProvablyEven(Loop) then
    begin
      Inc(UD_NoEpilogue);
      Loop.Free;
      Exit;
    end;

    // Only unroll loops with constant integer step
    if not Loop.IVInfo.StepIsConst then
    begin
      Inc(UD_Step);
      {$IFDEF DEBUG_SSA}
      WriteLn('[UNROLL] Skipping loop at ', Header.LabelName,
        ': non-constant step');
      {$ENDIF}
      Loop.Free;
      Exit;
    end;

    Result := Loop;
  except
    Loop.Free;
    raise;
  end;
end;

procedure TLoopUnroller.GetLoopBlocks(Loop: TUnrollableLoop);
var
  WorkList: TFPList;
  Block, Pred: TSSABasicBlock;
  i: Integer;
begin
  WorkList := TFPList.Create;
  try
    if Loop.Latch <> Loop.Header then
    begin
      Loop.BodyBlocks.Add(Pointer(Loop.Latch));
      WorkList.Add(Pointer(Loop.Latch));
    end;

    while WorkList.Count > 0 do
    begin
      Block := TSSABasicBlock(WorkList[WorkList.Count - 1]);
      WorkList.Delete(WorkList.Count - 1);

      for i := 0 to Block.Predecessors.Count - 1 do
      begin
        Pred := TSSABasicBlock(Block.Predecessors[i]);
        if not Loop.ContainsBlock(Pred) then
        begin
          Loop.BodyBlocks.Add(Pointer(Pred));
          WorkList.Add(Pointer(Pred));
        end;
      end;
    end;

    // Find exit block
    for i := 0 to Loop.Header.Successors.Count - 1 do
    begin
      Block := TSSABasicBlock(Loop.Header.Successors[i]);
      if not Loop.ContainsBlock(Block) then
      begin
        Loop.ExitBlock := Block;
        Break;
      end;
    end;

  finally
    WorkList.Free;
  end;
end;

function TLoopUnroller.HasSimpleStructure(Loop: TUnrollableLoop): Boolean;
var
  i, BackEdgeCount: Integer;
  Block: TSSABasicBlock;
begin
  Result := False;

  // Must have single back-edge
  BackEdgeCount := 0;
  for i := 0 to Loop.Header.Predecessors.Count - 1 do
  begin
    Block := TSSABasicBlock(Loop.Header.Predecessors[i]);
    if Loop.ContainsBlock(Block) then
      Inc(BackEdgeCount);
  end;

  if BackEdgeCount <> 1 then
  begin
    {$IFDEF DEBUG_SSA}
    WriteLn('[UNROLL] Skipping loop at ', Loop.Header.LabelName,
      ': multiple back-edges (', BackEdgeCount, ')');
    {$ENDIF}
    Exit;
  end;

  // Must have an exit
  if Loop.ExitBlock = nil then
  begin
    {$IFDEF DEBUG_SSA}
    WriteLn('[UNROLL] Skipping loop at ', Loop.Header.LabelName, ': no exit block');
    {$ENDIF}
    Exit;
  end;

  // For simplicity, require single-block loop body (header = latch)
  // This covers FOR loops which are the most common case
  if Loop.BodyBlocks.Count > 2 then
  begin
    {$IFDEF DEBUG_SSA}
    WriteLn('[UNROLL] Skipping loop at ', Loop.Header.LabelName,
      ': multi-block body (', Loop.BodyBlocks.Count, ' blocks) - not yet supported');
    {$ENDIF}
    Exit;
  end;

  Result := True;
end;

function TLoopUnroller.CountLoopInstructions(Loop: TUnrollableLoop): Integer;
var
  i, j: Integer;
  Block: TSSABasicBlock;
begin
  Result := 0;
  for i := 0 to Loop.BodyBlocks.Count - 1 do
  begin
    Block := TSSABasicBlock(Loop.BodyBlocks[i]);
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      case Block.Instructions[j].OpCode of
        ssaLabel, ssaJump, ssaJumpIfZero, ssaJumpIfNotZero, ssaPhi:
          ; // Don't count control flow and PHI
      else
        Inc(Result);
      end;
    end;
  end;
end;

function TLoopUnroller.ResolveConstStep(const Val: TSSAValue; out Step: Int64): Boolean;
// Read the induction step, which may arrive either as an immediate or - and this is the normal
// case now - as a REGISTER that SSA generation loaded the constant into:
//     LoadConstInt R116, 1
//     AddInt       R116, R116, R70
// Resolving one level is what SedaiAlgebraic.IsZero already does for zero.
//
// ⛔ AND IT IS NOT ENOUGH TO FIND *A* LoadConstInt THAT WRITES THE REGISTER. A constant loaded
// before the loop does not still hold inside it when the register is reassigned per iteration:
// BuildConstantMap in SedaiAlgebraic carries the scar - folding a preheader "i = 1" against the
// body's "i = i + 1" turned the step into 2 and made m304_shared_for_counter loop forever, which is
// why that map is confined to a single block. The unroller cannot confine itself that way, because
// the load and the use are in different blocks by construction.
//
// So the test here is stronger and needs no block reasoning at all: the register must have EXACTLY
// ONE definition in the whole program, and that definition must be a LoadConstInt. One definition
// means there is no second value it could hold on any path, back edge included. Under CLASSIC
// global-by-name semantics every version is 0 and a user variable is written many times, so the
// count naturally exceeds one and the answer is a conservative False - which is the right answer
// there for exactly the reason the scar records.
var
  bi, ii, Defs: Integer;
  Blk: TSSABasicBlock;
  Instr, TheDef: TSSAInstruction;
begin
  Result := False;
  Step := 0;

  if Val.Kind = svkConstInt then
  begin
    Step := Val.ConstInt;
    Exit(True);
  end;
  if Val.Kind <> svkRegister then Exit;

  Defs := 0;
  TheDef := nil;
  for bi := 0 to FProgram.Blocks.Count - 1 do
  begin
    Blk := FProgram.Blocks[bi];
    for ii := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := Blk.Instructions[ii];
      if (Instr.Dest.Kind = svkRegister) and
         (Instr.Dest.RegType = Val.RegType) and
         (Instr.Dest.RegIndex = Val.RegIndex) and
         (Instr.Dest.Version = Val.Version) then
      begin
        Inc(Defs);
        if Defs > 1 then Exit;      // more than one value: not a constant step
        TheDef := Instr;
      end;
    end;
  end;

  if (Defs = 1) and (TheDef.OpCode = ssaLoadConstInt) and
     (TheDef.Src1.Kind = svkConstInt) then
  begin
    Step := TheDef.Src1.ConstInt;
    Result := True;
  end;
end;

function TLoopUnroller.FindInductionVariable(Loop: TUnrollableLoop): TInductionVarInfo;
var
  i, j: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  StepConst: Int64;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Found := False;

  // Strategy: Look for the pattern IV = IV + const in the latch block
  // This is the increment that happens at the end of each iteration
  //
  // ⛔⛔ QUI STA IL MOTIVO PER CUI QUESTO PASSO NON SROTOLA MAI (diagnosi 21 ago 2026).
  // La condizione sotto pretende `Instr.Src2.Kind = svkConstInt`, cioe' il passo come OPERANDO
  // COSTANTE IMMEDIATO. Ma la generazione SSA MATERIALIZZA le costanti in un REGISTRO prima di
  // usarle - la forma che emettiamo e':
  //     LoadConstInt R116, 1
  //     AddInt       R116, R116, R70
  // quindi Src2 e' un REGISTRO che contiene 1, non un svkConstInt, e il confronto fallisce.
  //
  // 📊 Misurato con UNROLL_DIAG=1 (la strumentazione qui sotto): su quattro programmi pieni di
  // cicli, 26 candidati su 35 vengono scartati proprio qui.
  //     mandelbrot     8 esaminati -> 7 senza indice
  //     spectral-norm 18 esaminati -> 15 senza indice
  //     sieve          4 esaminati -> 2 senza indice
  //     n-body         5 esaminati -> 2 senza indice (3 scartati per dimensione)
  // Nessuno scartato per "passo non costante": non ci si arriva nemmeno.
  //
  // ⭐ LA CORREZIONE, per chi la fa: risolvere UN livello, accettando un registro la cui
  // definizione e' un ssaLoadConstInt - esattamente cio' che SedaiAlgebraic.IsZero fa gia' per
  // riconoscere lo zero attraverso un registro. Il valore del passo si legge da li'.
  // ⛔ NON e' una modifica piccola nei suoi EFFETTI: accende una trasformazione DORMIENTE su
  // centinaia di cicli del corpus. Va fatta con la rete differenziale completa (opt vs --no-opt),
  // AOT e JIT, e misurata - uno srotolamento che allunga il codice senza far guadagnare tempo e'
  // un peggioramento, non un pareggio.
  //
  // E' la stessa forma di CONST_PROP, che cerca ssaStoreVar/ssaLoadVar che l'SSA non emette piu':
  // un matcher scritto per una rappresentazione che l'IR ha smesso di produrre.

  Block := Loop.Latch;
  for j := 0 to Block.Instructions.Count - 1 do
  begin
    Instr := Block.Instructions[j];

    // Look for AddInt or AddFloat with constant step - as an immediate, or through the register
    // the SSA materialised it into, which is the form it actually emits. See ResolveConstStep.
    if OpIn(Instr.OpCode, [ssaAddInt, ssaAddFloat]) and
       ResolveConstStep(Instr.Src2, StepConst) then
    begin
      // Found a potential IV increment
      Result.Found := True;
      Result.IncrInstr := Instr;
      Result.IncrInstrIndex := j;
      Result.IncrBlock := Block;
      Result.StepValue := StepConst;
      Result.StepIsConst := True;
      Result.IVRegIndex := Instr.Src1.RegIndex;
      Result.IVVersion := Instr.Src1.Version;
      Result.IVRegType := Instr.Src1.RegType;

      {$IFDEF DEBUG_SSA}
      WriteLn('[UNROLL] Found IV: R', Result.IVRegIndex, '_', Result.IVVersion,
        ' with step ', Result.StepValue);
      {$ENDIF}
      Exit;
    end;

    // Also check for AddFloat with float constant
    if (Instr.OpCode = ssaAddFloat) and
       (Instr.Src2.Kind = svkConstFloat) then
    begin
      Result.Found := True;
      Result.IncrInstr := Instr;
      Result.IncrInstrIndex := j;
      Result.IncrBlock := Block;
      Result.StepValue := Round(Instr.Src2.ConstFloat);
      Result.StepIsConst := Abs(Instr.Src2.ConstFloat - Result.StepValue) < 0.0001;
      Result.IVRegIndex := Instr.Src1.RegIndex;
      Result.IVVersion := Instr.Src1.Version;
      Result.IVRegType := Instr.Src1.RegType;

      if Result.StepIsConst then
      begin
        {$IFDEF DEBUG_SSA}
        WriteLn('[UNROLL] Found float IV: R', Result.IVRegIndex, '_', Result.IVVersion,
          ' with step ', Result.StepValue);
        {$ENDIF}
        Exit;
      end;
    end;
  end;

  // Also look in header for PHI-based IV (for more complex SSA patterns)
  for j := 0 to Loop.Header.Instructions.Count - 1 do
  begin
    Instr := Loop.Header.Instructions[j];
    if Instr.OpCode = ssaPhi then
    begin
      Result.PhiInstr := Instr;
      // Continue looking for the increment
    end;
  end;
end;

function TLoopUnroller.UsesInductionVar(Instr: TSSAInstruction;
  const IVInfo: TInductionVarInfo): Boolean;

  function CheckValue(const Val: TSSAValue): Boolean;
  begin
    Result := (Val.Kind = svkRegister) and
              (Val.RegIndex = IVInfo.IVRegIndex);
  end;

begin
  Result := CheckValue(Instr.Src1) or
            CheckValue(Instr.Src2) or
            CheckValue(Instr.Src3);
end;

function TLoopUnroller.CloneInstructionWithIVOffset(Instr: TSSAInstruction;
  const IVInfo: TInductionVarInfo; IVOffsetReg: Integer): TSSAInstruction;

  function RemapValue(const Val: TSSAValue): TSSAValue;
  begin
    Result := Val;
    // Replace IV register with the offset version
    if (Val.Kind = svkRegister) and (Val.RegIndex = IVInfo.IVRegIndex) then
      Result.RegIndex := IVOffsetReg;
  end;

begin
  Result := Instr.Clone;
  // Remap source operands that reference IV
  Result.Src1 := RemapValue(Result.Src1);
  Result.Src2 := RemapValue(Result.Src2);
  Result.Src3 := RemapValue(Result.Src3);
  // Dest gets a new register (allocated by caller)
end;

function TLoopUnroller.UnrollLoop(Loop: TUnrollableLoop): Boolean;
var
  i, j, k: Integer;
  Block: TSSABasicBlock;
  Instr, ClonedInstr, IVIncrInstr: TSSAInstruction;
  OriginalInstrs: TList;
  IncrIdx: Integer;
  IVOffsetReg, NewDestReg: Integer;
  IVInfo: TInductionVarInfo;
  InsertPos: Integer;
begin
  Result := False;
  IVInfo := Loop.IVInfo;

  {$IFDEF DEBUG_SSA}
  WriteLn('[UNROLL] Unrolling loop at ', Loop.Header.LabelName,
    ' (', Loop.BodyInstrCount, ' instructions, factor=', FUnrollFactor, ')');
  {$ENDIF}

  // For 2x unrolling with proper IV handling:
  // Original: [body using IV] [IV = IV + step] [loop back]
  // Unrolled: [body using IV] [IV' = IV + step] [body using IV'] [IV = IV' + step] [loop back]
  //
  // This way:
  // - First copy of body uses original IV
  // - We compute IV + step into a temp register
  // - Second copy of body uses IV + step
  // - Original increment now adds step to get IV + 2*step for next iteration

  Block := Loop.Latch;  // For single-block loops, this is where all the action is
  IncrIdx := IVInfo.IncrInstrIndex;

  // Allocate register for IV + step (intermediate value)
  IVOffsetReg := FProgram.AllocRegister(IVInfo.IVRegType);

  // Collect original instructions (before increment) that we need to clone
  OriginalInstrs := TList.Create;
  try
    for j := 0 to IncrIdx - 1 do
    begin
      Instr := Block.Instructions[j];
      // Skip PHI, labels, and control flow
      case Instr.OpCode of
        ssaPhi, ssaLabel, ssaJump, ssaJumpIfZero, ssaJumpIfNotZero:
          Continue;
      end;
      OriginalInstrs.Add(Pointer(Instr));
    end;

    if OriginalInstrs.Count = 0 then
    begin
      {$IFDEF DEBUG_SSA}
      WriteLn('[UNROLL] No instructions to unroll before IV increment');
      {$ENDIF}
      Exit;
    end;

    // Insert position: right after the original IV increment
    InsertPos := IncrIdx + 1;

    // Step 1: Create IV' = IV + step instruction (compute next iteration's IV)
    IVIncrInstr := TSSAInstruction.Create(IVInfo.IncrInstr.OpCode);
    IVIncrInstr.Dest := MakeSSARegister(IVInfo.IVRegType, IVOffsetReg);
    IVIncrInstr.Src1 := MakeSSARegister(IVInfo.IVRegType, IVInfo.IVRegIndex);
    IVIncrInstr.Src1.Version := IVInfo.IVVersion;
    if IVInfo.IVRegType = srtFloat then
      IVIncrInstr.Src2 := MakeSSAConstFloat(IVInfo.StepValue)
    else
      IVIncrInstr.Src2 := MakeSSAConstInt(IVInfo.StepValue);
    IVIncrInstr.Comment := 'unroll: IV'' = IV + step';

    // Insert the intermediate IV computation before the cloned body
    Block.Instructions.Insert(InsertPos, IVIncrInstr);
    Inc(InsertPos);

    // Step 2: Clone each instruction, replacing IV references with IV'
    for j := 0 to OriginalInstrs.Count - 1 do
    begin
      Instr := TSSAInstruction(OriginalInstrs[j]);

      // Clone with IV offset
      ClonedInstr := CloneInstructionWithIVOffset(Instr, IVInfo, IVOffsetReg);

      // Allocate new destination register for cloned instruction
      if ClonedInstr.Dest.Kind = svkRegister then
      begin
        NewDestReg := FProgram.AllocRegister(ClonedInstr.Dest.RegType);
        ClonedInstr.Dest.RegIndex := NewDestReg;
      end;

      ClonedInstr.Comment := Instr.Comment + ' [unroll copy]';

      Block.Instructions.Insert(InsertPos, ClonedInstr);
      Inc(InsertPos);
    end;

    // Step 3: Update the original increment to use IV' instead of IV
    // So now it computes: IV_next = IV' + step = (IV + step) + step = IV + 2*step
    IVInfo.IncrInstr.Src1.RegIndex := IVOffsetReg;

    {$IFDEF DEBUG_SSA}
    WriteLn('[UNROLL] Successfully unrolled loop: added ', OriginalInstrs.Count + 1,
      ' instructions');
    {$ENDIF}

    Result := True;
    Inc(FUnrolledCount);

  finally
    OriginalInstrs.Free;
  end;
end;

procedure ReportNoIVSizes;
// Print the body sizes of the candidates that died at the induction-variable gate, because that
// gate is the one the matcher fails at and the sizes are what price its repair. Sorted, so the
// median is read off directly rather than computed.
var
  i, j, t: Integer;
  Tiny: Integer;
  Line: string;
begin
  if Length(UD_NoIVSizes) = 0 then Exit;
  for i := 0 to High(UD_NoIVSizes) - 1 do             // insertion sort: the array is a few dozen
    for j := i + 1 to High(UD_NoIVSizes) do
      if UD_NoIVSizes[j] < UD_NoIVSizes[i] then
      begin
        t := UD_NoIVSizes[i]; UD_NoIVSizes[i] := UD_NoIVSizes[j]; UD_NoIVSizes[j] := t;
      end;
  Tiny := 0;
  Line := '';
  for i := 0 to High(UD_NoIVSizes) do
  begin
    if UD_NoIVSizes[i] <= 4 then Inc(Tiny);
    if Line <> '' then Line := Line + ' ';
    Line := Line + IntToStr(UD_NoIVSizes[i]);
  end;
  WriteLn(ErrOutput, '[UNROLL] corpi dei candidati (istruzioni, ordinati): ', Line);
  WriteLn(ErrOutput, '[UNROLL]   SOLO-REGISTRI=', UD_RegOnly, '/', Length(UD_NoIVSizes));
  for i := 0 to High(UD_Culprits) do
    WriteLn(ErrOutput, '[UNROLL]   tocca-memoria: ', SSAOpCodeToString(UD_Culprits[i]),
            ' x', UD_CulpritN[i]);
  WriteLn(ErrOutput, '[UNROLL]   mediana=', UD_NoIVSizes[Length(UD_NoIVSizes) div 2],
          '  minimo=', UD_NoIVSizes[0], '  massimo=', UD_NoIVSizes[High(UD_NoIVSizes)],
          '  corpi<=4 istruzioni=', Tiny, '/', Length(UD_NoIVSizes));
end;

function TLoopUnroller.Run: Integer;
var
  Loops: TObjectList;
  i: Integer;
  Loop: TUnrollableLoop;
begin
  FUnrolledCount := 0;
  UD_Seen := 0; UD_Struct := 0; UD_Size := 0; UD_NoIV := 0; UD_Step := 0; UD_Done := 0;
  SetLength(UD_NoIVSizes, 0); UD_RegOnly := 0; UD_NotProfitable := 0; UD_NoEpilogue := 0;
  SetLength(UD_Culprits, 0); SetLength(UD_CulpritN, 0);

  BuildDominatorMap;

  Loops := FindUnrollableLoops;
  try
    {$IFDEF DEBUG_SSA}
    WriteLn('[UNROLL] Found ', Loops.Count, ' candidate loops');
    {$ENDIF}

    for i := 0 to Loops.Count - 1 do
    begin
      Loop := TUnrollableLoop(Loops[i]);
      try
        UnrollLoop(Loop);
      except
        on E: Exception do
        begin
          {$IFDEF DEBUG_SSA}
          WriteLn('[UNROLL] Failed to unroll loop at ', Loop.Header.LabelName,
            ': ', E.Message);
          {$ENDIF}
        end;
      end;
    end;

    Result := FUnrolledCount;

    {$IFDEF DEBUG_SSA}
    if FUnrolledCount > 0 then
      WriteLn('[UNROLL] Unrolled ', FUnrolledCount, ' loops');
    {$ENDIF}

  finally
    Loops.Free;
  end;
  if GetEnvironmentVariable('UNROLL_DIAG') = '1' then
  begin
    WriteLn(ErrOutput, '[UNROLL] esaminati=', UD_Seen,
            '  scartati: struttura=', UD_Struct, ' dimensione=', UD_Size,
            ' senza-indice=', UD_NoIV, ' passo-non-costante=', UD_Step,
            ' non-profittevole=', UD_NotProfitable,
            ' senza-epilogo=', UD_NoEpilogue,
            '  SROTOLATI=', FUnrolledCount);
    ReportNoIVSizes;
  end;
end;

function RunLoopUnrolling(Prog: TSSAProgram): Integer;
var
  Unroller: TLoopUnroller;
begin
  Unroller := TLoopUnroller.Create(Prog);
  try
    Result := Unroller.Run;
  finally
    Unroller.Free;
  end;
end;

end.
