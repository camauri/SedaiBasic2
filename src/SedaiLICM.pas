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
  Unit: SedaiLICM (Loop-Invariant Code Motion)

  Purpose: Move loop-invariant computations outside loops to reduce per-iteration
           work and improve runtime performance.

  Algorithm: Natural loop detection + dominance-based hoisting
             1. Identify natural loops via back-edges in CFG
             2. Compute loop membership for each basic block
             3. Find invariant instructions (operands defined outside loop)
             4. Create pre-header block for each loop (properly integrated in CFG)
             5. Clone and hoist invariant instructions to pre-header

  Key correctness requirements:
    - Pre-header must be properly wired into CFG (predecessors redirected)
    - Instructions must be CLONED, not moved (preserves SSA)
    - Register definitions not found = assume INSIDE loop (conservative)
    - PHI nodes and control flow are never hoisted

  Phase: Advanced Optimization (Tier 3 - after GVN)
  Author: Sedai Project - Performance Optimization
  Date: 2025-01-25
  Rewritten: 2025-01-27 (fixed CFG/SSA issues)
  ============================================================================ }

unit SedaiLICM;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, Contnrs, SedaiSSATypes, SedaiDominators;

type
  { TLoopInfo - Information about a natural loop }
  TLoopInfo = class
    Header: TSSABasicBlock;
    Blocks: TFPList;  // List of TSSABasicBlock
    PreHeader: TSSABasicBlock;
    BackEdgeSources: TFPList;  // All back-edge sources (list of TSSABasicBlock)
    constructor Create(AHeader: TSSABasicBlock);
    destructor Destroy; override;
    function ContainsBlock(Block: TSSABasicBlock): Boolean;
  end;

  { TLoopInvariantCodeMotion - LICM optimizer }
  TLoopInvariantCodeMotion = class
  private
    FProgram: TSSAProgram;
    FLoops: TObjectList;  // Owns TLoopInfo objects
    FDominatorMap: TFPHashList;  // Maps TSSABasicBlock → TSSABasicBlock (block → idom)
    FHoistedCount: Integer;

    { Build dominator map from program's dominator tree }
    procedure BuildDominatorMap;

    { Find all natural loops via back-edges }
    procedure FindLoops;

    { Check if edge (From -> Target) is a back-edge }
    function IsBackEdge(From, Target: TSSABasicBlock): Boolean;

    { Compute all blocks in a natural loop }
    procedure ComputeLoopBlocks(Loop: TLoopInfo; BackEdgeSource: TSSABasicBlock);

    { Create and wire pre-header into CFG }
    procedure CreatePreHeader(Loop: TLoopInfo);

    { Check if instruction can be safely hoisted }
    function IsSafeToHoist(const Instr: TSSAInstruction): Boolean;

    { Check if register is a BASIC user variable (Version=0 globals) }
    function IsUserVariable(const Val: TSSAValue): Boolean;

    { Check if a value is defined outside the loop }
    function IsDefinedOutsideLoop(const Val: TSSAValue; Loop: TLoopInfo): Boolean;

    { Check if instruction is loop-invariant }
    function IsInvariant(const Instr: TSSAInstruction; Loop: TLoopInfo): Boolean;

    { Check if an array is modified anywhere in the loop }
    function IsArrayModifiedInLoop(ArrayIndex: Integer; Loop: TLoopInfo): Boolean;

    { Clone an instruction for hoisting }
    function CloneInstruction(const Instr: TSSAInstruction): TSSAInstruction;

    { Hoist invariant instructions from loop to pre-header }
    procedure HoistInvariants(Loop: TLoopInfo);

  public
    constructor Create(Prog: TSSAProgram);
    destructor Destroy; override;

    { Run LICM pass - returns number of instructions hoisted }
    function Run: Integer;
  end;

implementation

uses SedaiDebug;

{ TLoopInfo }

constructor TLoopInfo.Create(AHeader: TSSABasicBlock);
begin
  inherited Create;
  Header := AHeader;
  Blocks := TFPList.Create;
  BackEdgeSources := TFPList.Create;
  PreHeader := nil;
  Blocks.Add(Pointer(Header));
end;

destructor TLoopInfo.Destroy;
begin
  Blocks.Free;
  BackEdgeSources.Free;
  inherited;
end;

function TLoopInfo.ContainsBlock(Block: TSSABasicBlock): Boolean;
begin
  Result := Blocks.IndexOf(Pointer(Block)) >= 0;
end;

{ TLoopInvariantCodeMotion }

constructor TLoopInvariantCodeMotion.Create(Prog: TSSAProgram);
begin
  inherited Create;
  FProgram := Prog;
  FLoops := TObjectList.Create(True);  // Owns TLoopInfo objects
  FDominatorMap := TFPHashList.Create;
  FHoistedCount := 0;
end;

destructor TLoopInvariantCodeMotion.Destroy;
begin
  FLoops.Free;
  FDominatorMap.Free;
  inherited;
end;

function TLoopInvariantCodeMotion.Run: Integer;
var
  Loop: TLoopInfo;
  i, j: Integer;
  Temp: TLoopInfo;
begin
  {$IFDEF DEBUG_LICM}
  if DebugLICM then
    WriteLn('[LICM] Running loop-invariant code motion...');
  {$ENDIF}

  // Step 1: Build dominator map
  BuildDominatorMap;
  if FDominatorMap.Count = 0 then
  begin
    {$IFDEF DEBUG_LICM}
    if DebugLICM then
      WriteLn('[LICM] ERROR: Failed to build dominator map');
    {$ENDIF}
    Exit(0);
  end;

  // Step 2: Find all natural loops
  FindLoops;
  {$IFDEF DEBUG_LICM}
  if DebugLICM then
    WriteLn('[LICM] Found ', FLoops.Count, ' natural loops');
  {$ENDIF}

  if FLoops.Count = 0 then
  begin
    {$IFDEF DEBUG_LICM}
    if DebugLICM then
      WriteLn('[LICM] No loops to optimize');
    {$ENDIF}
    Exit(0);
  end;

  // Step 3: Sort loops innermost-first (fewer blocks = more inner)
  // This ensures inner loops are processed before outer loops
  // CRITICAL for nested loop correctness
  // CRITICAL: TObjectList.OwnsObjects=True causes the old item to be freed when assigning!
  // Temporarily disable ownership during sort to prevent object destruction
  FLoops.OwnsObjects := False;
  for i := 0 to FLoops.Count - 2 do
  begin
    for j := i + 1 to FLoops.Count - 1 do
    begin
      if TLoopInfo(FLoops[j]).Blocks.Count < TLoopInfo(FLoops[i]).Blocks.Count then
      begin
        // Safe swap now that OwnsObjects is False
        Temp := TLoopInfo(FLoops[i]);
        FLoops[i] := FLoops[j];
        FLoops[j] := Temp;
      end;
    end;
  end;
  FLoops.OwnsObjects := True;  // Re-enable ownership

  {$IFDEF DEBUG_LICM}
  if DebugLICM then
  begin
    WriteLn('[LICM] Processing order (innermost first):');
    for i := 0 to FLoops.Count - 1 do
      WriteLn('[LICM]   ', i + 1, '. ', TLoopInfo(FLoops[i]).Header.LabelName, ' (', TLoopInfo(FLoops[i]).Blocks.Count, ' blocks)');
  end;
  {$ENDIF}

  // Step 4: Process each loop (now in innermost-first order)
  for i := 0 to FLoops.Count - 1 do
  begin
    Loop := TLoopInfo(FLoops[i]);
    {$IFDEF DEBUG_LICM}
    if DebugLICM then
      WriteLn('[LICM] Processing loop: ', Loop.Header.LabelName, ' (', Loop.Blocks.Count, ' blocks)');
    {$ENDIF}

    // Create pre-header with proper CFG wiring
    CreatePreHeader(Loop);

    // Hoist invariants
    HoistInvariants(Loop);
  end;

  // CRITICAL: Clear dominator tree after CFG modifications (pre-header creation)
  // The dominator tree now contains stale references and will be rebuilt on next use
  if FLoops.Count > 0 then
    FProgram.ClearDomTree;

  {$IFDEF DEBUG_LICM}
  if DebugLICM then
    WriteLn('[LICM] Hoisted ', FHoistedCount, ' instructions');
  {$ENDIF}
  Result := FHoistedCount;
end;

procedure TLoopInvariantCodeMotion.BuildDominatorMap;
var
  DomTree: TDominatorTree;
  Block, IdomBlock: TSSABasicBlock;
  i: Integer;
begin
  if not Assigned(FProgram.GetDomTree) then
  begin
    {$IFDEF DEBUG_LICM}
    if DebugLICM then
      WriteLn('[LICM] ERROR: Dominator tree not available');
    {$ENDIF}
    Exit;
  end;

  DomTree := TDominatorTree(FProgram.GetDomTree);

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    IdomBlock := DomTree.GetIdom(Block);
    if Assigned(IdomBlock) then
    begin
      // TFPHashList: hash on pointer address, store idom as data
      FDominatorMap.Add(Format('%p', [Pointer(Block)]), Pointer(IdomBlock));
    end;
  end;

  {$IFDEF DEBUG_LICM}
  if DebugLICM then
    WriteLn('[LICM] Dominator map: ', FDominatorMap.Count, ' entries');
  {$ENDIF}
end;

procedure TLoopInvariantCodeMotion.FindLoops;
var
  Block, Succ: TSSABasicBlock;
  Loop: TLoopInfo;
  i, j, k: Integer;
  ExistingLoop: TLoopInfo;
  Found: Boolean;
begin
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];

    for j := 0 to Block.Successors.Count - 1 do
    begin
      Succ := TSSABasicBlock(Block.Successors[j]);

      if IsBackEdge(Block, Succ) then
      begin
        // Check if we already have a loop with this header
        Found := False;
        for k := 0 to FLoops.Count - 1 do
        begin
          ExistingLoop := TLoopInfo(FLoops[k]);
          if ExistingLoop.Header = Succ then
          begin
            // Add this back-edge source to existing loop
            if ExistingLoop.BackEdgeSources.IndexOf(Pointer(Block)) < 0 then
              ExistingLoop.BackEdgeSources.Add(Pointer(Block));
            ComputeLoopBlocks(ExistingLoop, Block);
            Found := True;
            Break;
          end;
        end;

        if not Found then
        begin
          // Create new loop
          Loop := TLoopInfo.Create(Succ);
          Loop.BackEdgeSources.Add(Pointer(Block));
          ComputeLoopBlocks(Loop, Block);
          FLoops.Add(Loop);
        end;
      end;
    end;
  end;
end;

function TLoopInvariantCodeMotion.IsBackEdge(From, Target: TSSABasicBlock): Boolean;
var
  Dom, NextDom: TSSABasicBlock;
  Steps, Idx: Integer;
begin
  Result := False;
  Dom := From;
  Steps := 0;

  while Assigned(Dom) do
  begin
    Inc(Steps);
    if Steps > 200 then Exit(False);
    if Dom = Target then Exit(True);

    Idx := FDominatorMap.FindIndexOf(Format('%p', [Pointer(Dom)]));
    if Idx < 0 then Break;

    NextDom := TSSABasicBlock(FDominatorMap.Items[Idx]);
    if not Assigned(NextDom) or (NextDom = Dom) then Break;

    Dom := NextDom;
  end;
end;

procedure TLoopInvariantCodeMotion.ComputeLoopBlocks(Loop: TLoopInfo; BackEdgeSource: TSSABasicBlock);
var
  Worklist: TFPList;
  Current, Pred: TSSABasicBlock;
  i, Iterations: Integer;
begin
  Worklist := TFPList.Create;
  try
    if not Loop.ContainsBlock(BackEdgeSource) then
    begin
      Loop.Blocks.Add(Pointer(BackEdgeSource));
      Worklist.Add(Pointer(BackEdgeSource));
    end;

    Iterations := 0;
    while Worklist.Count > 0 do
    begin
      Inc(Iterations);
      if Iterations > 10000 then
      begin
        {$IFDEF DEBUG_LICM}
        if DebugLICM then
          WriteLn('[LICM] WARNING: ComputeLoopBlocks iteration limit');
        {$ENDIF}
        Break;
      end;

      Current := TSSABasicBlock(Worklist[Worklist.Count - 1]);
      Worklist.Delete(Worklist.Count - 1);

      if Current = Loop.Header then
        Continue;

      for i := 0 to Current.Predecessors.Count - 1 do
      begin
        Pred := TSSABasicBlock(Current.Predecessors[i]);

        if not Loop.ContainsBlock(Pred) then
        begin
          Loop.Blocks.Add(Pointer(Pred));
          Worklist.Add(Pointer(Pred));
        end;
      end;
    end;
  finally
    Worklist.Free;
  end;
end;

procedure TLoopInvariantCodeMotion.CreatePreHeader(Loop: TLoopInfo);
var
  PreHeader: TSSABasicBlock;
  Pred: TSSABasicBlock;
  EntryPreds: TFPList;
  i, j, k: Integer;
  JumpInstr: TSSAInstruction;
begin
  // Collect entry predecessors (outside the loop)
  EntryPreds := TFPList.Create;
  try
    for i := 0 to Loop.Header.Predecessors.Count - 1 do
    begin
      Pred := TSSABasicBlock(Loop.Header.Predecessors[i]);
      if not Loop.ContainsBlock(Pred) then
        EntryPreds.Add(Pointer(Pred));
    end;

    // If no entry predecessors or only one that's already effectively a pre-header, skip
    if EntryPreds.Count = 0 then
    begin
      {$IFDEF DEBUG_LICM}
      if DebugLICM then
        WriteLn('[LICM] Loop ', Loop.Header.LabelName, ' has no entry predecessors - skipping pre-header');
      {$ENDIF}
      Exit;
    end;

    // Create pre-header block BEFORE the loop header
    // This is critical for correct bytecode order - pre-header must be compiled before header
    // Using CreateBlockBefore ensures the pre-header appears in correct execution order
    PreHeader := FProgram.CreateBlockBefore(Loop.Header.LabelName + '_prehead', Loop.Header);
    Loop.PreHeader := PreHeader;

    // Add jump from pre-header to header
    // CRITICAL: Bytecode compiler expects jump target in Dest, not Src1!
    JumpInstr := TSSAInstruction.Create(ssaJump);
    JumpInstr.Dest := MakeSSALabel(Loop.Header.LabelName);
    PreHeader.AddInstruction(JumpInstr);

    // Wire pre-header -> header
    PreHeader.AddSuccessor(Loop.Header);
    Loop.Header.AddPredecessor(PreHeader);

    // Redirect entry predecessors to pre-header
    for i := 0 to EntryPreds.Count - 1 do
    begin
      Pred := TSSABasicBlock(EntryPreds[i]);
      {$IFDEF DEBUG_LICM}
      if DebugLICM then
      begin
        if Assigned(Pred) then
          WriteLn('[LICM]   Redirecting predecessor: ', Pred.LabelName)
        else
          WriteLn('[LICM]   ERROR: NIL predecessor!');
      end;
      {$ENDIF}
      if not Assigned(Pred) then Continue;

      // Update Pred's successors: replace Header with PreHeader
      for j := 0 to Pred.Successors.Count - 1 do
      begin
        if TSSABasicBlock(Pred.Successors[j]) = Loop.Header then
        begin
          Pred.Successors[j] := PreHeader;
          Break;
        end;
      end;

      // Update pre-header's predecessors
      PreHeader.AddPredecessor(Pred);

      // Remove Pred from Header's predecessors
      for j := Loop.Header.Predecessors.Count - 1 downto 0 do
      begin
        if TSSABasicBlock(Loop.Header.Predecessors[j]) = Pred then
        begin
          Loop.Header.Predecessors.Delete(j);
          Break;
        end;
      end;

      // Update jump targets in Pred's instructions
      // CRITICAL: Bytecode compiler expects jump target in Dest, not Src1/Src2!
      for k := 0 to Pred.Instructions.Count - 1 do
      begin
        if Pred.Instructions[k].Dest.Kind = svkLabel then
          if Pred.Instructions[k].Dest.LabelName = Loop.Header.LabelName then
            Pred.Instructions[k].Dest.LabelName := PreHeader.LabelName;
      end;

      // CRITICAL: Update PHI sources in loop header
      // When we redirect Pred -> PreHeader, PHI nodes that reference Pred
      // must now reference PreHeader instead, otherwise PhiElimination
      // will insert copies in the wrong block
      for k := 0 to Loop.Header.Instructions.Count - 1 do
      begin
        if Loop.Header.Instructions[k].OpCode <> ssaPhi then
          Break;  // PHIs are always at the start
        for j := 0 to High(Loop.Header.Instructions[k].PhiSources) do
        begin
          if Loop.Header.Instructions[k].PhiSources[j].FromBlock = Pred then
            Loop.Header.Instructions[k].PhiSources[j].FromBlock := PreHeader;
        end;
      end;
    end;

    // Pre-header already added to block list by CreateBlock
    // No need to add again

    {$IFDEF DEBUG_LICM}
    if DebugLICM then
      WriteLn('[LICM] Created pre-header: ', PreHeader.LabelName);
    {$ENDIF}

  finally
    EntryPreds.Free;
  end;
end;

function TLoopInvariantCodeMotion.IsSafeToHoist(const Instr: TSSAInstruction): Boolean;
begin
  case Instr.OpCode of
    // Pure arithmetic - safe
    ssaAddInt, ssaSubInt, ssaMulInt, ssaDivInt, ssaModInt, ssaNegInt,
    ssaAddFloat, ssaSubFloat, ssaMulFloat, ssaDivFloat, ssaPowFloat, ssaNegFloat:
      Result := True;

    // Type conversions - safe
    ssaIntToFloat, ssaFloatToInt:
      Result := True;

    // Comparisons - safe
    ssaCmpEqInt, ssaCmpNeInt, ssaCmpLtInt, ssaCmpGtInt, ssaCmpLeInt, ssaCmpGeInt,
    ssaCmpEqFloat, ssaCmpNeFloat, ssaCmpLtFloat, ssaCmpGtFloat, ssaCmpLeFloat, ssaCmpGeFloat:
      Result := True;

    // Bitwise - safe
    ssaBitwiseAnd, ssaBitwiseOr, ssaBitwiseXor, ssaBitwiseNot:
      Result := True;

    // Math functions - safe
    ssaMathSin, ssaMathCos, ssaMathTan, ssaMathAtn, ssaMathLog, ssaMathExp,
    ssaMathSqr, ssaMathAbs, ssaMathSgn, ssaMathInt,
    ssaMathLog10, ssaMathLog2, ssaMathLogN:
      Result := True;

    // Load constant - safe but usually not worth hoisting
    ssaLoadConstInt, ssaLoadConstFloat:
      Result := True;

    // Array load - potentially safe if array is not modified in loop
    // Actual safety check is done in IsInvariant which calls IsArrayModifiedInLoop
    ssaArrayLoad:
      Result := True;

    // Everything else - NOT safe
    else
      Result := False;
  end;
end;

function TLoopInvariantCodeMotion.IsArrayModifiedInLoop(ArrayIndex: Integer; Loop: TLoopInfo): Boolean;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  i, j: Integer;
begin
  { Check if any ssaArrayStore in the loop modifies the same array.
    ArrayIndex is stored in Src1 for both ssaArrayLoad and ssaArrayStore. }

  for i := 0 to Loop.Blocks.Count - 1 do
  begin
    Block := TSSABasicBlock(Loop.Blocks[i]);
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];
      if Instr.OpCode = ssaArrayStore then
      begin
        // Check if it's the same array (ArrayIndex in Src1)
        if (Instr.Src1.Kind = svkConstInt) and (Instr.Src1.ConstInt = ArrayIndex) then
        begin
          {$IFDEF DEBUG_LICM}
          if DebugLICM then
            WriteLn('[LICM] Array ', ArrayIndex, ' is modified in loop at block ', Block.LabelName);
          {$ENDIF}
          Exit(True);
        end;
      end;
    end;
  end;

  Result := False;
end;

function TLoopInvariantCodeMotion.IsUserVariable(const Val: TSSAValue): Boolean;
var
  i: Integer;
  VarKey, MappedKey: string;
begin
  // Only registers can be user variables
  if Val.Kind <> svkRegister then
    Exit(False);

  // Build key: "RegType:RegIndex" (without Version - VarRegMap uses this format)
  VarKey := IntToStr(Ord(Val.RegType)) + ':' + IntToStr(Val.RegIndex);

  // Check if this register maps to a BASIC user variable
  for i := 0 to FProgram.VarRegMap.Count - 1 do
  begin
    MappedKey := FProgram.VarRegMap.ValueFromIndex[i];
    if MappedKey = VarKey then
      Exit(True);
  end;

  Result := False;
end;

function TLoopInvariantCodeMotion.IsDefinedOutsideLoop(const Val: TSSAValue; Loop: TLoopInfo): Boolean;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  i, j: Integer;
begin
  // Constants are always outside
  if Val.Kind in [svkConstInt, svkConstFloat, svkConstString, svkLabel, svkNone] then
    Exit(True);

  // CRITICAL: User variables with GlobalVariableSemantics (Version=0) cannot be
  // reliably determined as loop-invariant because all definitions have same version.
  // Conservatively treat them as NOT outside loop (don't hoist)
  if IsUserVariable(Val) then
  begin
    {$IFDEF DEBUG_LICM}
    if DebugLICM then
      WriteLn('[LICM] User variable r', Val.RegIndex, '_v', Val.Version, ' - treating as inside loop (BASIC global semantics)');
    {$ENDIF}
    Exit(False);
  end;

  // For registers, find where they're defined
  if Val.Kind = svkRegister then
  begin
    for i := 0 to FProgram.Blocks.Count - 1 do
    begin
      Block := FProgram.Blocks[i];

      for j := 0 to Block.Instructions.Count - 1 do
      begin
        Instr := Block.Instructions[j];

        // Match both RegIndex AND Version
        if (Instr.Dest.Kind = svkRegister) and
           (Instr.Dest.RegIndex = Val.RegIndex) and
           (Instr.Dest.Version = Val.Version) then
        begin
          // Found definition - is it outside the loop?
          Result := not Loop.ContainsBlock(Block);
          Exit;
        end;
      end;
    end;

    // CRITICAL FIX: Definition not found = CONSERVATIVELY assume INSIDE loop
    // This prevents hoisting instructions that depend on PHI nodes or
    // loop induction variables whose definitions we can't find
    {$IFDEF DEBUG_LICM}
    if DebugLICM then
      WriteLn('[LICM] WARNING: Definition not found for r', Val.RegIndex, '_v', Val.Version, ' - assuming inside loop');
    {$ENDIF}
    Exit(False);
  end;

  // Variables - assume inside loop (conservative)
  Result := False;
end;

function TLoopInvariantCodeMotion.IsInvariant(const Instr: TSSAInstruction; Loop: TLoopInfo): Boolean;
begin
  if not IsSafeToHoist(Instr) then
    Exit(False);

  // CRITICAL FIX for GlobalVariableSemantics (BASIC mode with Version=0):
  // Problem: VarRegMap maps BASIC variables to ORIGINAL registers, but optimizations
  // (CopyProp, ConstProp, etc.) RENAME registers without updating VarRegMap.
  // Example: C1 mapped to r0, but after CopyProp it's r5. VarRegMap still says r0.
  //
  // CONSERVATIVE SOLUTION: In GlobalVariableSemantics mode, NEVER hoist ANY instruction
  // that writes to a register. This prevents all correctness issues with variable aliasing.
  //
  // Why this works:
  // - Constants (LoadConstInt/Float) can still be hoisted (dest is not a register)
  // - All operations on variables stay in loop (safe, no semantic changes)
  // - Performance impact: lower, but correctness is guaranteed
  //
  // Better solution would require: maintaining VarRegMap updates through all optimizations,
  // or switching to full SSA with version tracking.
  // In GlobalVariableSemantics mode (BASIC), we must be extremely conservative.
  // NEVER hoist ANY instruction that writes to a register, including LoadConst.
  //
  // Why even LoadConst is unsafe: Consider this pattern:
  //   FOR ITER = 1 TO 10
  //     GOSUB ...
  //   NEXT
  //   VV = 0              <- LoadConst for VV
  //   FOR I = 0 TO N
  //     VV = VV + ...
  //   NEXT
  //
  // If LICM sees the inner loop and hoists "VV = 0" (LoadConst 0 -> R_VV),
  // it might move it to a preheader that's actually inside the outer ITER loop,
  // causing VV to be 0 only on the first outer iteration.
  //
  // The safe approach: don't hoist anything with a register destination.
  if FProgram.GlobalVariableSemantics then
  begin
    if Instr.Dest.Kind = svkRegister then
    begin
      {$IFDEF DEBUG_LICM}
      if DebugLICM then
        WriteLn('[LICM] Skipping hoist in GlobalVariableSemantics: dest is register r',
                Instr.Dest.RegIndex, '_v', Instr.Dest.Version,
                ' (unsafe in BASIC semantics)');
      {$ENDIF}
      Exit(False);
    end;
  end;

  // Special handling for array loads:
  // For ssaArrayLoad: Src1 = ArrayIndex (constant), Src2 = linear index register
  // Array load is invariant if:
  //   1. The index is defined outside the loop (checked below)
  //   2. The array is NOT modified anywhere in the loop
  if Instr.OpCode = ssaArrayLoad then
  begin
    // Src1 should be constant ArrayIndex
    if Instr.Src1.Kind = svkConstInt then
    begin
      if IsArrayModifiedInLoop(Instr.Src1.ConstInt, Loop) then
      begin
        {$IFDEF DEBUG_LICM}
        if DebugLICM then
          WriteLn('[LICM] ArrayLoad not invariant: array ', Instr.Src1.ConstInt, ' is modified in loop');
        {$ENDIF}
        Exit(False);
      end;
    end
    else
    begin
      // Dynamic array index - can't analyze, not safe
      {$IFDEF DEBUG_LICM}
      if DebugLICM then
        WriteLn('[LICM] ArrayLoad not invariant: dynamic array index');
      {$ENDIF}
      Exit(False);
    end;

    // For array load, only Src2 (the index) needs to be loop-invariant
    // Src1 is the array index (constant), Src3 is unused
    if not IsDefinedOutsideLoop(Instr.Src2, Loop) then
      Exit(False);

    {$IFDEF DEBUG_LICM}
    if DebugLICM then
      WriteLn('[LICM] ArrayLoad is loop-invariant: array ', Instr.Src1.ConstInt, ', index is defined outside');
    {$ENDIF}
    Exit(True);
  end;

  // Check all operands must be defined outside loop
  if not IsDefinedOutsideLoop(Instr.Src1, Loop) then
    Exit(False);

  if not IsDefinedOutsideLoop(Instr.Src2, Loop) then
    Exit(False);

  if not IsDefinedOutsideLoop(Instr.Src3, Loop) then
    Exit(False);

  Result := True;
end;

function TLoopInvariantCodeMotion.CloneInstruction(const Instr: TSSAInstruction): TSSAInstruction;
begin
  // Use built-in Clone method from TSSAInstruction
  Result := Instr.Clone;
end;

procedure TLoopInvariantCodeMotion.HoistInvariants(Loop: TLoopInfo);
var
  Block: TSSABasicBlock;
  Instr, ClonedInstr: TSSAInstruction;
  ToHoist: TFPList;  // List of TSSAInstruction
  OriginalBlocks: TFPList;  // List of TSSABasicBlock
  i, j, InsertPos, OrigIdx: Integer;
  Changed: Boolean;
  Iterations: Integer;
begin
  if not Assigned(Loop.PreHeader) then
  begin
    {$IFDEF DEBUG_LICM}
    if DebugLICM then
      WriteLn('[LICM] No pre-header for ', Loop.Header.LabelName, ' - skipping');
    {$ENDIF}
    Exit;
  end;

  ToHoist := TFPList.Create;
  OriginalBlocks := TFPList.Create;
  try
    // Multi-pass: find invariants until no more found
    Iterations := 0;
    repeat
      Changed := False;
      Inc(Iterations);

      if Iterations > 50 then
      begin
        {$IFDEF DEBUG_LICM}
        if DebugLICM then
          WriteLn('[LICM] WARNING: Exceeded iteration limit');
        {$ENDIF}
        Break;
      end;

      for i := 0 to Loop.Blocks.Count - 1 do
      begin
        Block := TSSABasicBlock(Loop.Blocks[i]);

        // Skip header (contains PHI nodes)
        if Block = Loop.Header then
          Continue;

        for j := 0 to Block.Instructions.Count - 1 do
        begin
          Instr := Block.Instructions[j];

          if IsInvariant(Instr, Loop) then
          begin
            if ToHoist.IndexOf(Pointer(Instr)) < 0 then
            begin
              ToHoist.Add(Pointer(Instr));
              OriginalBlocks.Add(Pointer(Block));
              Changed := True;
              {$IFDEF DEBUG_LICM}
              if DebugLICM then
                WriteLn('[LICM]   Invariant: ', Instr.ToString, ' from ', Block.LabelName);
              {$ENDIF}
            end;
          end;
        end;
      end;
    until not Changed;

    // Clone and insert into pre-header (before the jump)
    InsertPos := Loop.PreHeader.Instructions.Count - 1;  // Before final jump
    if InsertPos < 0 then InsertPos := 0;

    for i := 0 to ToHoist.Count - 1 do
    begin
      Instr := TSSAInstruction(ToHoist[i]);
      Block := TSSABasicBlock(OriginalBlocks[i]);

      // Clone instruction
      ClonedInstr := CloneInstruction(Instr);

      // Insert clone into pre-header
      Loop.PreHeader.Instructions.Insert(InsertPos, ClonedInstr);
      Inc(InsertPos);
      Inc(FHoistedCount);

      // Remove original from its block using Extract (doesn't free the object)
      // Then explicitly free it to avoid memory leak
      Block.Instructions.Extract(Instr);
      Instr.Free;

      {$IFDEF DEBUG_LICM}
      if DebugLICM then
        WriteLn('[LICM]   Hoisted to ', Loop.PreHeader.LabelName);
      {$ENDIF}
    end;

  finally
    ToHoist.Free;
    OriginalBlocks.Free;
  end;
end;

end.
