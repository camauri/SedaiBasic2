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
  Unit: SedaiDBE (Dead Block Elimination)

  Purpose: Remove unreachable basic blocks from CFG before dominator tree
           construction and SSA-based optimizations.

  Algorithm: Reachability analysis via depth-first search from entry block
             1. Mark all blocks reachable from entry via DFS
             2. Remove all unmarked (unreachable) blocks
             3. Clean up predecessor/successor lists

  Why this is needed:
    - Dominator tree requires exactly ONE entry point (only entry has no preds)
    - Unreachable blocks violate this invariant
    - Such blocks arise from unconditional jumps (GOTO) that skip code
    - Example: LINE 830: GOTO 850 makes LINE 840 unreachable

  What is eliminated:
    - Blocks with no predecessors (except entry block)
    - Dead code after unconditional jumps
    - Code in unreachable subroutines

  What is preserved:
    - Entry block (always reachable by definition)
    - All blocks reachable via control flow from entry
    - All reachable code paths

  Complexity: O(N + E) where N = blocks, E = CFG edges (standard DFS)

  Phase: PRE-optimization (before dominator tree construction)
  Author: Sedai Project - Optimization Pipeline
  Date: 2025-01-27
  ============================================================================ }

unit SedaiDBE;

{$mode objfpc}{$H+}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, Generics.Collections, SedaiSSATypes;

type
  { TDeadBlockElimination - Unreachable block removal via reachability analysis }
  TDeadBlockElimination = class
  private
    FProgram: TSSAProgram;
    FReachable: array of Boolean;  // [BlockIdx] = IsReachable
    FRemovedCount: Integer;

    { Mark all blocks reachable from entry via DFS }
    procedure MarkReachableBlocks;

    { DFS helper - recursively mark block and successors as reachable }
    procedure DFS(BlockIdx: Integer);

    { Remove all unmarked (unreachable) blocks }
    procedure RemoveUnreachableBlocks;

  public
    constructor Create(Prog: TSSAProgram);
    destructor Destroy; override;

    { Run DBE pass - returns number of blocks removed }
    function Run: Integer;
  end;

implementation

{$IFDEF DEBUG_DBE}
uses SedaiDebug;
{$ENDIF}

{ TDeadBlockElimination }

constructor TDeadBlockElimination.Create(Prog: TSSAProgram);
begin
  inherited Create;
  FProgram := Prog;
  FRemovedCount := 0;
end;

destructor TDeadBlockElimination.Destroy;
begin
  inherited;
end;

function TDeadBlockElimination.Run: Integer;
var
  i: Integer;
begin
  {$IFDEF DEBUG_DBE}
  if DebugDBE then
    WriteLn('[DBE] Running dead block elimination...');
  {$ENDIF}

  // Step 1: Initialize reachability map
  SetLength(FReachable, FProgram.Blocks.Count);
  for i := 0 to High(FReachable) do
    FReachable[i] := False;

  // Step 2: Mark all blocks reachable from entry
  MarkReachableBlocks;

  // Step 3: Remove unreachable blocks
  RemoveUnreachableBlocks;

  {$IFDEF DEBUG_DBE}
  if DebugDBE then
    WriteLn('[DBE] Removed ', FRemovedCount, ' unreachable blocks');
  {$ENDIF}
  Result := FRemovedCount;
end;

procedure TDeadBlockElimination.MarkReachableBlocks;
var
  EntryIdx: Integer;
  Block: TSSABasicBlock;
  i: Integer;
begin
  { Find entry block and start DFS from there.
    Entry block is typically the first block, but we search by name to be sure. }

  EntryIdx := -1;
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    if (Block.LabelName = '_entry') or (i = 0) then
    begin
      EntryIdx := i;
      Break;
    end;
  end;

  if EntryIdx < 0 then
  begin
    {$IFDEF DEBUG_DBE}
    if DebugDBE then
      WriteLn('[DBE] ERROR: Entry block not found!');
    {$ENDIF}
    Exit;
  end;

  {$IFDEF DEBUG_DBE}
  if DebugDBE then
    WriteLn('[DBE] Starting reachability DFS from entry block (index ', EntryIdx, ')');
  {$ENDIF}
  DFS(EntryIdx);
end;

procedure TDeadBlockElimination.DFS(BlockIdx: Integer);
var
  Block: TSSABasicBlock;
  Succ: TSSABasicBlock;
  SuccIdx: Integer;
  i: Integer;
begin
  { Depth-first search to mark reachable blocks.

    INVARIANT: A block is reachable iff there exists a path from entry to it.

    Algorithm:
    1. Mark current block as reachable
    2. Recursively visit all unmarked successors }

  if (BlockIdx < 0) or (BlockIdx >= FProgram.Blocks.Count) then
    Exit;

  // Already visited?
  if FReachable[BlockIdx] then
    Exit;

  // Mark as reachable
  FReachable[BlockIdx] := True;

  Block := FProgram.Blocks[BlockIdx];

  // Visit all successors
  for i := 0 to Block.Successors.Count - 1 do
  begin
    Succ := TSSABasicBlock(Block.Successors[i]);

    // Find successor index in program block list
    SuccIdx := FProgram.Blocks.IndexOf(Succ);

    if SuccIdx >= 0 then
      DFS(SuccIdx);
  end;
end;

procedure TDeadBlockElimination.RemoveUnreachableBlocks;
var
  Block: TSSABasicBlock;
  i, j: Integer;
  UnreachableCount: Integer;
  PredBlock, SuccBlock: TSSABasicBlock;
begin
  { Remove all blocks not marked as reachable.

    CRITICAL: Must also clean up predecessor/successor lists in remaining blocks
    to avoid dangling pointers. }

  // Count unreachable blocks first
  UnreachableCount := 0;
  for i := 0 to FProgram.Blocks.Count - 1 do
    if not FReachable[i] then
      Inc(UnreachableCount);

  if UnreachableCount = 0 then
  begin
    {$IFDEF DEBUG_DBE}
    if DebugDBE then
      WriteLn('[DBE] All blocks are reachable - nothing to remove');
    {$ENDIF}
    Exit;
  end;

  {$IFDEF DEBUG_DBE}
  if DebugDBE then
    WriteLn('[DBE] Found ', UnreachableCount, ' unreachable blocks');
  {$ENDIF}

  // Pass 1: Clean up predecessor/successor lists
  // Remove references to unreachable blocks from reachable blocks
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    if not FReachable[i] then
      Continue;

    Block := FProgram.Blocks[i];

    // Clean up successors list
    j := 0;
    while j < Block.Successors.Count do
    begin
      SuccBlock := TSSABasicBlock(Block.Successors[j]);
      if FProgram.Blocks.IndexOf(SuccBlock) >= 0 then
      begin
        if not FReachable[FProgram.Blocks.IndexOf(SuccBlock)] then
        begin
          {$IFDEF DEBUG_DBE}
          if DebugDBE then
            WriteLn('[DBE]   Removing successor reference from reachable block ', Block.LabelName);
          {$ENDIF}
          Block.Successors.Delete(j);
          Continue;  // Don't increment j - we just deleted
        end;
      end;
      Inc(j);
    end;

    // Clean up predecessors list
    j := 0;
    while j < Block.Predecessors.Count do
    begin
      PredBlock := TSSABasicBlock(Block.Predecessors[j]);
      if FProgram.Blocks.IndexOf(PredBlock) >= 0 then
      begin
        if not FReachable[FProgram.Blocks.IndexOf(PredBlock)] then
        begin
          {$IFDEF DEBUG_DBE}
          if DebugDBE then
            WriteLn('[DBE]   Removing predecessor reference from reachable block ', Block.LabelName);
          {$ENDIF}
          Block.Predecessors.Delete(j);
          Continue;  // Don't increment j - we just deleted
        end;
      end;
      Inc(j);
    end;
  end;

  // Pass 2: Remove unreachable blocks from program
  // CRITICAL: Remove from end to start to avoid index shifts
  for i := FProgram.Blocks.Count - 1 downto 0 do
  begin
    if not FReachable[i] then
    begin
      Block := FProgram.Blocks[i];
      {$IFDEF DEBUG_DBE}
      if DebugDBE then
        WriteLn('[DBE] Removing unreachable block: ', Block.LabelName,
                ' (', Block.Instructions.Count, ' instructions)');
      {$ENDIF}

      // Extract removes without freeing, then we free manually
      FProgram.Blocks.Extract(Block).Free;
      Inc(FRemovedCount);
    end;
  end;
end;

end.
