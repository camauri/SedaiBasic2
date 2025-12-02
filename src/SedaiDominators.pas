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
  Unit: SedaiDominators

  Purpose: Dominator-tree construction for SSA basic blocks using the
           Cooper-Harvey-Kennedy algorithm.

  Algorithm: Iterative dataflow analysis with O(E*V) time complexity.
             Suitable for any language's control-flow graph (BASIC, C, Rust, etc.)

  Scalability: Tested on CFGs with 10,000+ blocks. Iteration limit is
               adaptive based on graph characteristics.

  Phase: 3 Tier 2 - Language-agnostic SSA infrastructure
  Author: Sedai Project - Phase 3 Implementation
  Date: 2025-11-13
  ============================================================================ }

unit SedaiDominators;

{$mode objfpc}{$H+}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, StrUtils, Generics.Collections, SedaiSSATypes;

type
  { Procedural type for block visitor (can be nested procedure or method) }
  TBlockVisitor = procedure(Block: TSSABasicBlock);

  { TDomNode - Node in the dominator tree }
  TDomNode = record
    Block: TSSABasicBlock;              // The basic block this node represents
    Idom: TSSABasicBlock;               // Immediate dominator (nil for entry)
    Children: specialize TList<TSSABasicBlock>; // Blocks immediately dominated by this one
    Preorder: Integer;                  // Preorder DFS number (for dominance queries)
    Postorder: Integer;                 // Postorder DFS number (for dominance queries)
    Visited: Boolean;                   // Debug flag for traversal validation
  end;

  { TCFGValidationError - Error types for CFG validation }
  TCFGValidationError = (
    cfgErrNone,
    cfgErrNoBlocks,
    cfgErrNilEntry,
    cfgErrMultipleEntries,
    cfgErrUnreachableBlock,
    cfgErrPredSuccMismatch,
    cfgErrNilEdge,
    cfgErrDuplicateEdge,
    cfgErrNoTerminator,
    cfgErrSelfLoopEntry
  );

  { TCFGValidationResult - Result of CFG validation }
  TCFGValidationResult = record
    Valid: Boolean;
    ErrorType: TCFGValidationError;
    ErrorBlock: TSSABasicBlock;
    ErrorMessage: string;
    UnreachableCount: Integer;
    TotalBlocks: Integer;
    ReachableBlocks: Integer;
  end;

  { TDominatorTree - Manages dominator relationships for an SSA program }
  TDominatorTree = class
  private
    FNodes: specialize TDictionary<TSSABasicBlock, TDomNode>;
    FEntry: TSSABasicBlock;
    FPreorderList: specialize TList<TSSABasicBlock>;
    FMaxIterations: Integer;

    { Initial DFS to assign preorder numbers and collect blocks }
    procedure InitialDFS(Block: TSSABasicBlock; var PreNum: Integer);

    { Compute immediate dominators using iterative dataflow }
    procedure ComputeIdoms;

    { Find common dominator of two blocks (core of Cooper-Harvey-Kennedy) }
    function Intersect(b1, b2: TSSABasicBlock): TSSABasicBlock;

    { Build children lists from idom relationships }
    procedure BuildChildrenLists;

    { Compute postorder numbers via DFS }
    procedure ComputePostorder(Block: TSSABasicBlock; var PostNum: Integer);

    { Get or create node for a block }
    function GetNode(Block: TSSABasicBlock): TDomNode;
    procedure SetNode(Block: TSSABasicBlock; const Node: TDomNode);

  public
    constructor Create;
    destructor Destroy; override;

    { Build dominator tree for the given SSA program }
    procedure Build(const Prog: TSSAProgram);

    { Configure iteration limit manually (for extreme cases or testing) }
    procedure SetMaxIterations(Limit: Integer);

    { Query: Get immediate dominator of a block (nil if entry) }
    function GetIdom(Block: TSSABasicBlock): TSSABasicBlock; inline;

    { Query: Does 'Dom' dominate 'Sub'? }
    function IsDom(Dom, Sub: TSSABasicBlock): Boolean; inline;

    { Traverse tree in preorder, calling Visitor for each block }
    procedure TraversePreorder(Visitor: TBlockVisitor);

    { Get number of blocks in preorder list }
    function GetPreorderCount: Integer; inline;

    { Get block at index i in preorder list }
    function GetPreorderBlock(Index: Integer): TSSABasicBlock; inline;

    { Debug: Dump tree structure to a string }
    function DumpTree: string;

    { Debug: Export to Graphviz DOT format }
    function ExportDOT: string;

    { Get children of a block in dominator tree }
    function GetChildren(Block: TSSABasicBlock): TFPList;

    { Validate CFG before building dominator tree }
    class function ValidateCFG(const Prog: TSSAProgram): TCFGValidationResult;

    { Dump block details for debugging }
    class function DumpBlock(Block: TSSABasicBlock): string;

    { Clear all internal structures (safe cleanup before SSA modifications) }
    procedure Clear;
  end;

implementation

{$IF DEFINED(DEBUG_DOMTREE) OR DEFINED(DEBUG_CLEANUP)}
uses SedaiDebug;
{$ENDIF}

{ TDominatorTree }

constructor TDominatorTree.Create;
begin
  FNodes := specialize TDictionary<TSSABasicBlock, TDomNode>.Create;
  FPreorderList := specialize TList<TSSABasicBlock>.Create;
  FEntry := nil;
  FMaxIterations := 0;
end;

destructor TDominatorTree.Destroy;
var
  Node: TDomNode;
  NodeCount: Integer;
begin
  {$IFDEF DEBUG_CLEANUP}
  if DebugCleanup then
  begin
    WriteLn('[DominatorTree.Destroy] Starting...');
    WriteLn('[DominatorTree.Destroy] FNodes.Count = ', FNodes.Count);
  end;
  {$ENDIF}

  // Free all children lists
  NodeCount := 0;
  for Node in FNodes.Values do
  begin
    Inc(NodeCount);
    {$IFDEF DEBUG_CLEANUP}
    if DebugCleanup then
    begin
      if Assigned(Node.Block) then
        WriteLn('[DominatorTree.Destroy] Node ', NodeCount, ': ', Node.Block.LabelName)
      else
        WriteLn('[DominatorTree.Destroy] Node ', NodeCount, ': NIL BLOCK!');
    end;
    {$ENDIF}
    if Assigned(Node.Children) then
    begin
      {$IFDEF DEBUG_CLEANUP}
      if DebugCleanup then
        WriteLn('[DominatorTree.Destroy]   Freeing Children list');
      {$ENDIF}
      Node.Children.Free;
    end;
  end;

  {$IFDEF DEBUG_CLEANUP}
  if DebugCleanup then
    WriteLn('[DominatorTree.Destroy] Freeing FNodes...');
  {$ENDIF}
  FNodes.Free;

  {$IFDEF DEBUG_CLEANUP}
  if DebugCleanup then
    WriteLn('[DominatorTree.Destroy] Freeing FPreorderList...');
  {$ENDIF}
  FPreorderList.Free;

  {$IFDEF DEBUG_CLEANUP}
  if DebugCleanup then
  begin
    WriteLn('[DominatorTree.Destroy] Calling inherited...');
    Flush(Output);
  end;
  {$ENDIF}
  inherited;
  {$IFDEF DEBUG_CLEANUP}
  // Note: Safe to write after inherited - object not deallocated until after destructor returns
  if DebugCleanup then
  begin
    WriteLn('[DominatorTree.Destroy] Complete');
    Flush(Output);
  end;
  {$ENDIF}
end;

procedure TDominatorTree.Clear;
var
  Node: TDomNode;
begin
  // Free all Children lists
  for Node in FNodes.Values do
  begin
    if Assigned(Node.Children) then
      Node.Children.Free;
  end;
  // Clear the dictionary and preorder list
  FNodes.Clear;
  FPreorderList.Clear;
  FEntry := nil;
end;

function TDominatorTree.GetNode(Block: TSSABasicBlock): TDomNode;
begin
  if not FNodes.TryGetValue(Block, Result) then
  begin
    // Create new node
    Result.Block := Block;
    Result.Idom := nil;
    Result.Children := specialize TList<TSSABasicBlock>.Create;
    Result.Preorder := -1;
    Result.Postorder := -1;
    Result.Visited := False;
    FNodes.Add(Block, Result);
  end;
end;

procedure TDominatorTree.SetNode(Block: TSSABasicBlock; const Node: TDomNode);
begin
  if FNodes.ContainsKey(Block) then
    FNodes[Block] := Node
  else
    FNodes.Add(Block, Node);
end;

procedure TDominatorTree.InitialDFS(Block: TSSABasicBlock; var PreNum: Integer);
var
  Node: TDomNode;
  Succ: TSSABasicBlock;
  i: Integer;
begin
  Node := GetNode(Block);

  // Already visited?
  if Node.Preorder >= 0 then
    Exit;

  // Assign preorder number
  Node.Preorder := PreNum;
  Inc(PreNum);
  SetNode(Block, Node);

  FPreorderList.Add(Block);

  // Visit successors
  if Assigned(Block.Successors) then
    for i := 0 to Block.Successors.Count - 1 do
    begin
      Succ := TSSABasicBlock(Block.Successors[i]);
      InitialDFS(Succ, PreNum);
    end;
end;

function TDominatorTree.Intersect(b1, b2: TSSABasicBlock): TSSABasicBlock;
var
  Node1, Node2: TDomNode;
  PrevB1, PrevB2: TSSABasicBlock;
  OuterSteps, InnerSteps: Integer;
  MaxSteps: Integer;
begin
  // Cooper-Harvey-Kennedy intersect algorithm
  // Climbs idom chains until common dominator found

  MaxSteps := FPreorderList.Count * 2;  // Should never need more than this
  OuterSteps := 0;

  while b1 <> b2 do
  begin
    Inc(OuterSteps);
    if OuterSteps > MaxSteps then
    begin
      {$IFDEF DEBUG_DOMTREE}
      if DebugDomTree then
      begin
        WriteLn('[Intersect] ERROR: Outer loop exceeded ', MaxSteps, ' steps');
        WriteLn('  b1 = ', b1.LabelName, ' (pre=', FNodes[b1].Preorder, ')');
        WriteLn('  b2 = ', b2.LabelName, ' (pre=', FNodes[b2].Preorder, ')');
      end;
      {$ENDIF}
      raise Exception.CreateFmt('Intersect: Infinite loop detected (b1=%s, b2=%s)',
        [b1.LabelName, b2.LabelName]);
    end;

    Node1 := FNodes[b1];
    Node2 := FNodes[b2];

    // Climb up the dominator tree (b1 side)
    InnerSteps := 0;
    while Node1.Preorder > Node2.Preorder do
    begin
      Inc(InnerSteps);
      if InnerSteps > MaxSteps then
      begin
        {$IFDEF DEBUG_DOMTREE}
        if DebugDomTree then
        begin
          WriteLn('[Intersect] ERROR: Inner loop (b1) exceeded ', MaxSteps, ' steps');
          WriteLn('  b1 = ', b1.LabelName, ' (pre=', Node1.Preorder, ')');
          WriteLn('  b2 = ', b2.LabelName, ' (pre=', Node2.Preorder, ')');
        end;
        {$ENDIF}
        raise Exception.Create('Intersect: Infinite loop in b1 chain');
      end;

      PrevB1 := b1;
      b1 := Node1.Idom;

      // Safety check: detect self-loop (entry has itself as idom)
      if b1 = PrevB1 then
      begin
        Result := b1;  // b1 is the entry block
        Exit;
      end;

      if b1 = nil then
        raise Exception.CreateFmt('Intersect: b1 reached nil idom (prev=%s)', [PrevB1.LabelName]);

      Node1 := FNodes[b1];
    end;

    // Climb up the dominator tree (b2 side)
    InnerSteps := 0;
    while Node2.Preorder > Node1.Preorder do
    begin
      Inc(InnerSteps);
      if InnerSteps > MaxSteps then
      begin
        {$IFDEF DEBUG_DOMTREE}
        if DebugDomTree then
        begin
          WriteLn('[Intersect] ERROR: Inner loop (b2) exceeded ', MaxSteps, ' steps');
          WriteLn('  b1 = ', b1.LabelName, ' (pre=', Node1.Preorder, ')');
          WriteLn('  b2 = ', b2.LabelName, ' (pre=', Node2.Preorder, ')');
        end;
        {$ENDIF}
        raise Exception.Create('Intersect: Infinite loop in b2 chain');
      end;

      PrevB2 := b2;
      b2 := Node2.Idom;

      // Safety check: detect self-loop (entry has itself as idom)
      if b2 = PrevB2 then
      begin
        Result := b2;  // b2 is the entry block
        Exit;
      end;

      if b2 = nil then
        raise Exception.CreateFmt('Intersect: b2 reached nil idom (prev=%s)', [PrevB2.LabelName]);

      Node2 := FNodes[b2];
    end;
  end;

  Result := b1;
end;

procedure TDominatorTree.ComputeIdoms;
var
  Changed: Boolean;
  Iterations: Integer;
  i, j: Integer;
  Block, Pred, NewIdom: TSSABasicBlock;
  Node: TDomNode;
  ProcessedPred: Boolean;
begin
  // Initialize: Entry dominates itself (sentinel for algorithm)
  Node := GetNode(FEntry);
  Node.Idom := FEntry;
  SetNode(FEntry, Node);

  Iterations := 0;
  repeat
    Changed := False;
    Inc(Iterations);

    // Iteration limit check (prevent infinite loops)
    if Iterations > FMaxIterations then
      raise Exception.CreateFmt(
        'ComputeIdoms: Failed to converge after %d iterations (possible CFG cycle)',
        [FMaxIterations]
      );

    // Process blocks in preorder (except entry)
    for i := 1 to FPreorderList.Count - 1 do
    begin
      Block := FPreorderList[i];
      Node := GetNode(Block);

      if not Assigned(Block.Predecessors) or (Block.Predecessors.Count = 0) then
        Continue; // Unreachable block

      // Find first processed predecessor
      NewIdom := nil;
      for j := 0 to Block.Predecessors.Count - 1 do
      begin
        Pred := TSSABasicBlock(Block.Predecessors[j]);
        if FNodes[Pred].Idom <> nil then
        begin
          NewIdom := Pred;
          Break;
        end;
      end;

      if NewIdom = nil then
        Continue; // No processed predecessors yet

      // Intersect with other processed predecessors
      for j := 0 to Block.Predecessors.Count - 1 do
      begin
        Pred := TSSABasicBlock(Block.Predecessors[j]);

        // Skip unprocessed or self
        if (Pred = NewIdom) or (FNodes[Pred].Idom = nil) then
          Continue;

        NewIdom := Intersect(Pred, NewIdom);
      end;

      // Update if changed
      if Node.Idom <> NewIdom then
      begin
        Node.Idom := NewIdom;
        SetNode(Block, Node);
        Changed := True;
      end;
    end;

  until not Changed;

  {$IFDEF DEBUG_DOMTREE}
  // Diagnostic output for performance monitoring
  if DebugDomTree then
  begin
    if Iterations > FPreorderList.Count then
      WriteLn(Format('[DominatorTree] WARNING: Converged after %d iterations (%d blocks) - complex CFG detected',
        [Iterations, FPreorderList.Count]))
    else
      WriteLn(Format('[DominatorTree] Converged after %d iterations (%d blocks)',
        [Iterations, FPreorderList.Count]));
  end;
  {$ENDIF}
end;

procedure TDominatorTree.BuildChildrenLists;
var
  Block: TSSABasicBlock;
  Node, IdomNode: TDomNode;
begin
  for Block in FPreorderList do
  begin
    if Block = FEntry then
      Continue; // Entry has no idom

    Node := FNodes[Block];
    if Node.Idom = nil then
      Continue; // Unreachable block

    // Add this block to its idom's children
    IdomNode := FNodes[Node.Idom];
    if not IdomNode.Children.Contains(Block) then
      IdomNode.Children.Add(Block);
    SetNode(Node.Idom, IdomNode);
  end;
end;

procedure TDominatorTree.ComputePostorder(Block: TSSABasicBlock; var PostNum: Integer);
var
  Node: TDomNode;
  Child: TSSABasicBlock;
  i: Integer;
begin
  Node := FNodes[Block];

  // Already visited?
  if Node.Visited then
    Exit;

  Node.Visited := True;
  SetNode(Block, Node);

  // Visit children first (postorder)
  for i := 0 to Node.Children.Count - 1 do
  begin
    Child := Node.Children[i];
    ComputePostorder(Child, PostNum);
  end;

  // Assign postorder number after children
  Node := FNodes[Block]; // Re-fetch (may have been updated)
  Node.Postorder := PostNum;
  Inc(PostNum);
  SetNode(Block, Node);
end;

procedure TDominatorTree.Build(const Prog: TSSAProgram);
var
  PreNum, PostNum: Integer;
  Block: TSSABasicBlock;
  Node: TDomNode;
  ValidationResult: TCFGValidationResult;
begin
  { Build dominator tree for any language's CFG.

    Expected characteristics by language:
    - BASIC/Lua: 50-500 blocks, 2-3 iterations typical
    - C/C++: 500-5000 blocks, 3-5 iterations typical
    - C++ with templates: 5000-20000 blocks, 5-10 iterations
    - Functional (Haskell/OCaml): Variable, deep nesting possible

    Algorithm handles all cases with adaptive iteration limit. }

  { ===== STEP 0: Validate CFG before building dominator tree ===== }
  ValidationResult := ValidateCFG(Prog);
  if not ValidationResult.Valid then
  begin
    raise Exception.CreateFmt(
      'DominatorTree.Build: CFG validation failed - %s' + LineEnding +
      '  Error type: %d' + LineEnding +
      '  Block: %s',
      [ValidationResult.ErrorMessage,
       Ord(ValidationResult.ErrorType),
       IfThen(Assigned(ValidationResult.ErrorBlock),
              ValidationResult.ErrorBlock.LabelName, '<none>')]);
  end;

  // Validate single entry
  if not Assigned(Prog.Blocks) or (Prog.Blocks.Count = 0) then
    raise Exception.Create('DominatorTree.Build: SSA program has no blocks');

  FEntry := TSSABasicBlock(Prog.Blocks[0]); // Assume first block is entry

  if FEntry = nil then
    raise Exception.Create('DominatorTree.Build: Entry block is nil');

  // Set adaptive iteration limit
  // For deeply nested or complex CFGs (e.g., C++ templates, functional languages),
  // convergence may require more iterations. Use generous limit while still
  // catching infinite loops.
  FMaxIterations := 10 * Prog.Blocks.Count;
  if FMaxIterations < 100 then
    FMaxIterations := 100;  // Minimum safety threshold

  {$IFDEF DEBUG_DOMTREE}
  if DebugDomTree then
    WriteLn('[DominatorTree] Building dominator tree for ', Prog.Blocks.Count, ' blocks...');
  {$ENDIF}

  // Step 1: DFS to assign preorder numbers
  PreNum := 0;
  InitialDFS(FEntry, PreNum);

  {$IFDEF DEBUG_DOMTREE}
  if DebugDomTree then
    WriteLn(Format('[DominatorTree] DFS collected %d reachable blocks', [FPreorderList.Count]));
  {$ENDIF}

  // Step 2: Compute immediate dominators (Cooper-Harvey-Kennedy)
  ComputeIdoms;

  // Step 3: Build children lists
  BuildChildrenLists;

  // Step 4: Compute postorder numbers
  PostNum := 0;

  // Reset visited flags
  for Block in FPreorderList do
  begin
    Node := FNodes[Block];
    Node.Visited := False;
    SetNode(Block, Node);
  end;

  ComputePostorder(FEntry, PostNum);

  {$IFDEF DEBUG_DOMTREE}
  if DebugDomTree then
    WriteLn('[DominatorTree] Build complete');
  {$ENDIF}
end;

procedure TDominatorTree.SetMaxIterations(Limit: Integer);
begin
  { Allow manual override of iteration limit.
    Useful for:
    - Extremely large CFGs (e.g., whole-program optimization)
    - Testing convergence behavior
    - Languages with unusual control flow (e.g., coroutines, async/await) }

  if Limit < 10 then
    raise Exception.CreateFmt('SetMaxIterations: Limit too low (%d), minimum is 10', [Limit]);

  FMaxIterations := Limit;
  {$IFDEF DEBUG_DOMTREE}
  if DebugDomTree then
    WriteLn(Format('[DominatorTree] Manual iteration limit set to %d', [Limit]));
  {$ENDIF}
end;

function TDominatorTree.GetIdom(Block: TSSABasicBlock): TSSABasicBlock;
var
  Node: TDomNode;
begin
  if not FNodes.TryGetValue(Block, Node) then
    raise Exception.CreateFmt('GetIdom: Block %p not in dominator tree', [Pointer(Block)]);

  // Entry block has itself as idom internally (for algorithm), but we return nil
  if Block = FEntry then
    Result := nil
  else
    Result := Node.Idom;
end;

function TDominatorTree.IsDom(Dom, Sub: TSSABasicBlock): Boolean;
var
  DomNode, SubNode: TDomNode;
begin
  // A dominates B iff A.Preorder <= B.Preorder AND A.Postorder >= B.Postorder
  // (A's subtree contains B in the dominator tree)

  if Dom = Sub then
    Exit(True); // Block dominates itself

  if not FNodes.TryGetValue(Dom, DomNode) then
    raise Exception.CreateFmt('IsDom: Dom block %p not in tree', [Pointer(Dom)]);

  if not FNodes.TryGetValue(Sub, SubNode) then
    raise Exception.CreateFmt('IsDom: Sub block %p not in tree', [Pointer(Sub)]);

  Result := (DomNode.Preorder <= SubNode.Preorder) and
            (DomNode.Postorder >= SubNode.Postorder);
end;

procedure TDominatorTree.TraversePreorder(Visitor: TBlockVisitor);
var
  Block: TSSABasicBlock;
begin
  // Preorder list already computed during Build
  for Block in FPreorderList do
    Visitor(Block);
end;

function TDominatorTree.GetPreorderCount: Integer;
begin
  Result := FPreorderList.Count;
end;

function TDominatorTree.GetPreorderBlock(Index: Integer): TSSABasicBlock;
begin
  if (Index < 0) or (Index >= FPreorderList.Count) then
    raise Exception.CreateFmt('GetPreorderBlock: Index %d out of bounds (0..%d)',
      [Index, FPreorderList.Count - 1]);

  Result := FPreorderList[Index];
end;

function TDominatorTree.DumpTree: string;
var
  Block: TSSABasicBlock;
  Node: TDomNode;
  i: Integer;
begin
  Result := '=== Dominator Tree ===' + LineEnding;

  for Block in FPreorderList do
  begin
    Node := FNodes[Block];

    Result := Result + Format('Block %p: Idom=%p, Pre=%d, Post=%d, Children=[',
      [Pointer(Block), Pointer(Node.Idom), Node.Preorder, Node.Postorder]);

    for i := 0 to Node.Children.Count - 1 do
    begin
      if i > 0 then Result := Result + ', ';
      Result := Result + Format('%p', [Pointer(Node.Children[i])]);
    end;

    Result := Result + ']' + LineEnding;
  end;
end;

function TDominatorTree.ExportDOT: string;
var
  Block: TSSABasicBlock;
  Node: TDomNode;
  i: Integer;
begin
  // Graphviz DOT format for visualization
  Result := 'digraph DominatorTree {' + LineEnding;
  Result := Result + '  rankdir=TB;' + LineEnding;
  Result := Result + '  node [shape=box];' + LineEnding;

  for Block in FPreorderList do
  begin
    Node := FNodes[Block];

    // Node definition
    Result := Result + Format('  "B%p" [label="Block %p\nPre=%d Post=%d"];' + LineEnding,
      [Pointer(Block), Pointer(Block), Node.Preorder, Node.Postorder]);

    // Idom edge
    if Assigned(Node.Idom) and (Node.Idom <> Block) then
      Result := Result + Format('  "B%p" -> "B%p" [color=red, label="idom"];' + LineEnding,
        [Pointer(Node.Idom), Pointer(Block)]);
  end;

  Result := Result + '}' + LineEnding;
end;

function TDominatorTree.GetChildren(Block: TSSABasicBlock): TFPList;
var
  Node: TDomNode;
  i: Integer;
begin
  { Convert generic TList to TFPList for FPC compatibility }
  Result := TFPList.Create;
  if FNodes.TryGetValue(Block, Node) and Assigned(Node.Children) then
  begin
    for i := 0 to Node.Children.Count - 1 do
      Result.Add(Node.Children[i]);
  end;
end;

class function TDominatorTree.DumpBlock(Block: TSSABasicBlock): string;
var
  i: Integer;
  Instr: TSSAInstruction;
begin
  if not Assigned(Block) then
  begin
    Result := '  <nil block>';
    Exit;
  end;

  Result := Format('  Block: %s (%p)' + LineEnding, [Block.LabelName, Pointer(Block)]);
  Result := Result + Format('    Predecessors (%d): ', [Block.Predecessors.Count]);

  for i := 0 to Block.Predecessors.Count - 1 do
  begin
    if i > 0 then Result := Result + ', ';
    if Block.Predecessors[i] = nil then
      Result := Result + '<nil>'
    else
      Result := Result + TSSABasicBlock(Block.Predecessors[i]).LabelName;
  end;
  Result := Result + LineEnding;

  Result := Result + Format('    Successors (%d): ', [Block.Successors.Count]);
  for i := 0 to Block.Successors.Count - 1 do
  begin
    if i > 0 then Result := Result + ', ';
    if Block.Successors[i] = nil then
      Result := Result + '<nil>'
    else
      Result := Result + TSSABasicBlock(Block.Successors[i]).LabelName;
  end;
  Result := Result + LineEnding;

  Result := Result + Format('    Instructions (%d):', [Block.Instructions.Count]) + LineEnding;
  for i := 0 to Block.Instructions.Count - 1 do
  begin
    Instr := Block.Instructions[i];
    Result := Result + Format('      %3d: %s', [i, Instr.ToString]) + LineEnding;
  end;
end;

class function TDominatorTree.ValidateCFG(const Prog: TSSAProgram): TCFGValidationResult;
var
  Entry: TSSABasicBlock;
  Block, Succ, Pred: TSSABasicBlock;
  Reachable: TFPList;    // List of reachable blocks
  Worklist: TFPList;     // BFS worklist
  SeenSuccs: TFPList;    // Temp list for duplicate check
  i, j, k: Integer;
  LastInstr: TSSAInstruction;
  Found: Boolean;

  procedure SetError(ErrType: TCFGValidationError; ErrBlock: TSSABasicBlock; const Msg: string);
  begin
    Result.Valid := False;
    Result.ErrorType := ErrType;
    Result.ErrorBlock := ErrBlock;
    Result.ErrorMessage := Msg;
  end;

begin
  { Initialize result }
  Result.Valid := True;
  Result.ErrorType := cfgErrNone;
  Result.ErrorBlock := nil;
  Result.ErrorMessage := '';
  Result.UnreachableCount := 0;
  Result.TotalBlocks := 0;
  Result.ReachableBlocks := 0;

  {$IFDEF DEBUG_DOMTREE}
  if DebugDomTree then
    WriteLn('[ValidateCFG] Starting CFG validation...');
  {$ENDIF}

  { ===== CHECK 1: Basic structure ===== }
  if not Assigned(Prog.Blocks) or (Prog.Blocks.Count = 0) then
  begin
    SetError(cfgErrNoBlocks, nil, 'CFG has no blocks');
    {$IFDEF DEBUG_DOMTREE}
    if DebugDomTree then
      WriteLn('[ValidateCFG] FAILED: ', Result.ErrorMessage);
    {$ENDIF}
    Exit;
  end;

  Result.TotalBlocks := Prog.Blocks.Count;
  Entry := Prog.Blocks[0];

  if Entry = nil then
  begin
    SetError(cfgErrNilEntry, nil, 'Entry block (Blocks[0]) is nil');
    {$IFDEF DEBUG_DOMTREE}
    if DebugDomTree then
      WriteLn('[ValidateCFG] FAILED: ', Result.ErrorMessage);
    {$ENDIF}
    Exit;
  end;

  {$IFDEF DEBUG_DOMTREE}
  if DebugDomTree then
    WriteLn('[ValidateCFG] Entry block: ', Entry.LabelName);
  {$ENDIF}

  { ===== CHECK 2: Single entry (no other blocks with zero predecessors) ===== }
  for i := 1 to Prog.Blocks.Count - 1 do
  begin
    Block := Prog.Blocks[i];
    if Assigned(Block) and (Block.Predecessors.Count = 0) then
    begin
      SetError(cfgErrMultipleEntries, Block,
        Format('Multiple entry points: Block "%s" has no predecessors (only entry should have none)',
          [Block.LabelName]));
      {$IFDEF DEBUG_DOMTREE}
      if DebugDomTree then
      begin
        WriteLn('[ValidateCFG] FAILED: ', Result.ErrorMessage);
        WriteLn(DumpBlock(Block));
      end;
      {$ENDIF}
      Exit;
    end;
  end;

  { ===== CHECK 3: Entry has no self-loop as only successor ===== }
  if (Entry.Successors.Count = 1) and (TSSABasicBlock(Entry.Successors[0]) = Entry) and
     (Entry.Predecessors.Count = 0) then
  begin
    {$IFDEF DEBUG_DOMTREE}
    if DebugDomTree then
      WriteLn('[ValidateCFG] WARNING: Entry block has self-loop with no exit path');
    {$ENDIF}
  end;

  { ===== CHECK 4: No nil edges ===== }
  for i := 0 to Prog.Blocks.Count - 1 do
  begin
    Block := Prog.Blocks[i];
    if not Assigned(Block) then Continue;

    for j := 0 to Block.Successors.Count - 1 do
    begin
      if Block.Successors[j] = nil then
      begin
        SetError(cfgErrNilEdge, Block,
          Format('Block "%s" has nil successor at index %d', [Block.LabelName, j]));
        {$IFDEF DEBUG_DOMTREE}
        if DebugDomTree then
        begin
          WriteLn('[ValidateCFG] FAILED: ', Result.ErrorMessage);
          WriteLn(DumpBlock(Block));
        end;
        {$ENDIF}
        Exit;
      end;
    end;

    for j := 0 to Block.Predecessors.Count - 1 do
    begin
      if Block.Predecessors[j] = nil then
      begin
        SetError(cfgErrNilEdge, Block,
          Format('Block "%s" has nil predecessor at index %d', [Block.LabelName, j]));
        {$IFDEF DEBUG_DOMTREE}
        if DebugDomTree then
        begin
          WriteLn('[ValidateCFG] FAILED: ', Result.ErrorMessage);
          WriteLn(DumpBlock(Block));
        end;
        {$ENDIF}
        Exit;
      end;
    end;
  end;

  { ===== CHECK 5: No duplicate edges ===== }
  SeenSuccs := TFPList.Create;
  try
    for i := 0 to Prog.Blocks.Count - 1 do
    begin
      Block := Prog.Blocks[i];
      if not Assigned(Block) then Continue;

      SeenSuccs.Clear;
      for j := 0 to Block.Successors.Count - 1 do
      begin
        Succ := TSSABasicBlock(Block.Successors[j]);
        if SeenSuccs.IndexOf(Pointer(Succ)) >= 0 then
        begin
          SetError(cfgErrDuplicateEdge, Block,
            Format('Block "%s" has duplicate successor edge to "%s"',
              [Block.LabelName, Succ.LabelName]));
          {$IFDEF DEBUG_DOMTREE}
          if DebugDomTree then
          begin
            WriteLn('[ValidateCFG] FAILED: ', Result.ErrorMessage);
            WriteLn(DumpBlock(Block));
          end;
          {$ENDIF}
          Exit;
        end;
        SeenSuccs.Add(Pointer(Succ));
      end;
    end;
  finally
    SeenSuccs.Free;
  end;

  { ===== CHECK 6: Pred/Succ consistency ===== }
  for i := 0 to Prog.Blocks.Count - 1 do
  begin
    Block := Prog.Blocks[i];
    if not Assigned(Block) then Continue;

    { For each successor B of A, A must be in B.Predecessors }
    for j := 0 to Block.Successors.Count - 1 do
    begin
      Succ := TSSABasicBlock(Block.Successors[j]);

      Found := False;
      for k := 0 to Succ.Predecessors.Count - 1 do
      begin
        if TSSABasicBlock(Succ.Predecessors[k]) = Block then
        begin
          Found := True;
          Break;
        end;
      end;

      if not Found then
      begin
        SetError(cfgErrPredSuccMismatch, Block,
          Format('Edge "%s" -> "%s" exists in successors but "%s" not in "%s".predecessors',
            [Block.LabelName, Succ.LabelName, Block.LabelName, Succ.LabelName]));
        {$IFDEF DEBUG_DOMTREE}
        if DebugDomTree then
        begin
          WriteLn('[ValidateCFG] FAILED: ', Result.ErrorMessage);
          WriteLn('Source block:');
          WriteLn(DumpBlock(Block));
          WriteLn('Target block:');
          WriteLn(DumpBlock(Succ));
        end;
        {$ENDIF}
        Exit;
      end;
    end;

    { For each predecessor P of A, A must be in P.Successors }
    for j := 0 to Block.Predecessors.Count - 1 do
    begin
      Pred := TSSABasicBlock(Block.Predecessors[j]);

      Found := False;
      for k := 0 to Pred.Successors.Count - 1 do
      begin
        if TSSABasicBlock(Pred.Successors[k]) = Block then
        begin
          Found := True;
          Break;
        end;
      end;

      if not Found then
      begin
        SetError(cfgErrPredSuccMismatch, Block,
          Format('Block "%s" lists "%s" as predecessor but "%s" does not have "%s" as successor',
            [Block.LabelName, Pred.LabelName, Pred.LabelName, Block.LabelName]));
        {$IFDEF DEBUG_DOMTREE}
        if DebugDomTree then
        begin
          WriteLn('[ValidateCFG] FAILED: ', Result.ErrorMessage);
          WriteLn('Current block:');
          WriteLn(DumpBlock(Block));
          WriteLn('Predecessor block:');
          WriteLn(DumpBlock(Pred));
        end;
        {$ENDIF}
        Exit;
      end;
    end;
  end;

  { ===== CHECK 7: Each block has a terminator ===== }
  for i := 0 to Prog.Blocks.Count - 1 do
  begin
    Block := Prog.Blocks[i];
    if not Assigned(Block) then Continue;

    if Block.Instructions.Count = 0 then
    begin
      { Empty block - only valid if it has exactly one successor (fallthrough) }
      if Block.Successors.Count <> 1 then
      begin
        SetError(cfgErrNoTerminator, Block,
          Format('Block "%s" is empty but has %d successors (expected 1 for fallthrough)',
            [Block.LabelName, Block.Successors.Count]));
        {$IFDEF DEBUG_DOMTREE}
        if DebugDomTree then
        begin
          WriteLn('[ValidateCFG] FAILED: ', Result.ErrorMessage);
          WriteLn(DumpBlock(Block));
        end;
        {$ENDIF}
        Exit;
      end;
      Continue;
    end;

    LastInstr := Block.Instructions[Block.Instructions.Count - 1];

    { Valid terminators }
    { Note: ssaCall (GOSUB) has 2 successors: call target + return point }
    if not (LastInstr.OpCode in [ssaJump, ssaJumpIfZero, ssaJumpIfNotZero,
                                  ssaReturn, ssaEnd, ssaStop, ssaCall]) then
    begin
      { Block doesn't end with terminator - must have exactly 1 successor (fallthrough) }
      if Block.Successors.Count > 1 then
      begin
        SetError(cfgErrNoTerminator, Block,
          Format('Block "%s" ends with %s but has %d successors (needs terminator for >1 succ)',
            [Block.LabelName, LastInstr.ToString, Block.Successors.Count]));
        {$IFDEF DEBUG_DOMTREE}
        if DebugDomTree then
        begin
          WriteLn('[ValidateCFG] FAILED: ', Result.ErrorMessage);
          WriteLn(DumpBlock(Block));
        end;
        {$ENDIF}
        Exit;
      end;
    end;
  end;

  { ===== CHECK 8: Reachability from entry ===== }
  Reachable := TFPList.Create;
  Worklist := TFPList.Create;
  try
    { BFS from entry }
    Worklist.Add(Pointer(Entry));
    Reachable.Add(Pointer(Entry));

    while Worklist.Count > 0 do
    begin
      Block := TSSABasicBlock(Worklist[0]);
      Worklist.Delete(0);

      for j := 0 to Block.Successors.Count - 1 do
      begin
        Succ := TSSABasicBlock(Block.Successors[j]);
        if Reachable.IndexOf(Pointer(Succ)) < 0 then
        begin
          Reachable.Add(Pointer(Succ));
          Worklist.Add(Pointer(Succ));
        end;
      end;
    end;

    Result.ReachableBlocks := Reachable.Count;
    Result.UnreachableCount := Prog.Blocks.Count - Reachable.Count;

    { Report unreachable blocks (warning, not error - they'll be skipped by DFS) }
    {$IFDEF DEBUG_DOMTREE}
    if DebugDomTree and (Result.UnreachableCount > 0) then
    begin
      WriteLn(Format('[ValidateCFG] WARNING: %d unreachable blocks detected (will be ignored by dominator analysis):',
        [Result.UnreachableCount]));

      for i := 0 to Prog.Blocks.Count - 1 do
      begin
        Block := Prog.Blocks[i];
        if Assigned(Block) and (Reachable.IndexOf(Pointer(Block)) < 0) then
        begin
          WriteLn(Format('  - "%s" (preds=%d, succs=%d)',
            [Block.LabelName, Block.Predecessors.Count, Block.Successors.Count]));
        end;
      end;
    end;
    {$ENDIF}

  finally
    Worklist.Free;
    Reachable.Free;
  end;

  { ===== ALL CHECKS PASSED ===== }
  {$IFDEF DEBUG_DOMTREE}
  if DebugDomTree then
    WriteLn(Format('[ValidateCFG] PASSED: %d blocks, %d reachable, %d unreachable',
      [Result.TotalBlocks, Result.ReachableBlocks, Result.UnreachableCount]));
  {$ENDIF}
end;

end.
