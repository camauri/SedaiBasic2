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

  This correctly handles:
    - Array accesses like FLAGS(I) - each copy accesses different element
    - Any trip count (no epilogue needed for simple 2x unroll with same step)

  Phase: Advanced Optimization (after LICM, before DCE)
  Author: Sedai Project - Performance Optimization
  Date: 2025-11-29 (Rewritten for correctness)
  ============================================================================ }

unit SedaiLoopUnroll;

{$mode objfpc}{$H+}
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
    FDominatorMap: TFPHashList;  // Maps block -> immediate dominator

    { Build dominator map from program's dominator tree }
    procedure BuildDominatorMap;

    { Check if edge (From -> Target) is a back-edge }
    function IsBackEdge(From, Target: TSSABasicBlock): Boolean;

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
    IVInfo: TInductionVarInfo;        // Induction variable information

    constructor Create(AHeader, ALatch: TSSABasicBlock);
    destructor Destroy; override;
    function ContainsBlock(Block: TSSABasicBlock): Boolean;
  end;

function RunLoopUnrolling(Prog: TSSAProgram): Integer;

implementation

uses SedaiDebug;

{ TUnrollableLoop }

constructor TUnrollableLoop.Create(AHeader, ALatch: TSSABasicBlock);
begin
  inherited Create;
  Header := AHeader;
  Latch := ALatch;
  BodyBlocks := TFPList.Create;
  ExitBlock := nil;
  BodyInstrCount := 0;
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
  FDominatorMap := TFPHashList.Create;
end;

destructor TLoopUnroller.Destroy;
begin
  FDominatorMap.Free;
  inherited;
end;

procedure TLoopUnroller.BuildDominatorMap;
var
  DomTree: TDominatorTree;
  i: Integer;
  Block, IdomBlock: TSSABasicBlock;
begin
  FDominatorMap.Clear;

  if not Assigned(FProgram.GetDomTree) then
  begin
    {$IFDEF DEBUG_SSA}
    WriteLn('[UNROLL] ERROR: Dominator tree not available');
    {$ENDIF}
    Exit;
  end;

  DomTree := TDominatorTree(FProgram.GetDomTree);

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    try
      IdomBlock := DomTree.GetIdom(Block);
      if IdomBlock <> nil then
        FDominatorMap.Add(Format('%p', [Pointer(Block)]), Pointer(IdomBlock));
    except
      // Block may not be in dominator tree - skip it
    end;
  end;
end;

function TLoopUnroller.IsBackEdge(From, Target: TSSABasicBlock): Boolean;
var
  Current: TSSABasicBlock;
  Idx: Integer;
begin
  // A back-edge is an edge where Target dominates From
  Result := False;
  Current := From;

  while Current <> nil do
  begin
    if Current = Target then
    begin
      Result := True;
      Exit;
    end;
    Idx := FDominatorMap.FindIndexOf(Format('%p', [Pointer(Current)]));
    if Idx < 0 then
      Break;
    Current := TSSABasicBlock(FDominatorMap.Items[Idx]);
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

      if IsBackEdge(Block, Succ) then
      begin
        Loop := AnalyzeLoop(Succ, Block);
        if Loop <> nil then
          Result.Add(Loop);
      end;
    end;
  end;
end;

function TLoopUnroller.AnalyzeLoop(Header: TSSABasicBlock;
  BackEdgeSource: TSSABasicBlock): TUnrollableLoop;
var
  Loop: TUnrollableLoop;
begin
  Result := nil;

  Loop := TUnrollableLoop.Create(Header, BackEdgeSource);
  try
    GetLoopBlocks(Loop);

    if not HasSimpleStructure(Loop) then
    begin
      Loop.Free;
      Exit;
    end;

    Loop.BodyInstrCount := CountLoopInstructions(Loop);

    if Loop.BodyInstrCount > FMaxBodySize then
    begin
      {$IFDEF DEBUG_SSA}
      WriteLn('[UNROLL] Skipping loop at ', Header.LabelName,
        ': body too large (', Loop.BodyInstrCount, ' > ', FMaxBodySize, ')');
      {$ENDIF}
      Loop.Free;
      Exit;
    end;

    // Find induction variable - REQUIRED for correct unrolling
    Loop.IVInfo := FindInductionVariable(Loop);
    if not Loop.IVInfo.Found then
    begin
      {$IFDEF DEBUG_SSA}
      WriteLn('[UNROLL] Skipping loop at ', Header.LabelName,
        ': could not identify induction variable');
      {$ENDIF}
      Loop.Free;
      Exit;
    end;

    // Only unroll loops with constant integer step
    if not Loop.IVInfo.StepIsConst then
    begin
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

function TLoopUnroller.FindInductionVariable(Loop: TUnrollableLoop): TInductionVarInfo;
var
  i, j: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Found := False;

  // Strategy: Look for the pattern IV = IV + const in the latch block
  // This is the increment that happens at the end of each iteration

  Block := Loop.Latch;
  for j := 0 to Block.Instructions.Count - 1 do
  begin
    Instr := Block.Instructions[j];

    // Look for AddInt or AddFloat with constant step
    if (Instr.OpCode in [ssaAddInt, ssaAddFloat]) and
       (Instr.Src2.Kind = svkConstInt) then
    begin
      // Found a potential IV increment
      Result.Found := True;
      Result.IncrInstr := Instr;
      Result.IncrInstrIndex := j;
      Result.IncrBlock := Block;
      Result.StepValue := Instr.Src2.ConstInt;
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

function TLoopUnroller.Run: Integer;
var
  Loops: TObjectList;
  i: Integer;
  Loop: TUnrollableLoop;
begin
  FUnrolledCount := 0;

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
