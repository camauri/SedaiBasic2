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

  Algorithm: Smart reachability analysis that preserves referenced blocks
             1. Collect all line numbers referenced by dynamic jumps
                (GOTO, GOSUB, TRAP, RESUME, RESTORE, ON...GOTO, ON...GOSUB)
             2. Mark all blocks reachable from entry via DFS
             3. Also mark blocks whose line numbers are in the reference set
             4. Remove only blocks that are both unreachable AND unreferenced

  Why this is needed:
    - Dominator tree requires exactly ONE entry point (only entry has no preds)
    - Unreachable blocks violate this invariant
    - BUT: Some "unreachable" blocks may be targets of dynamic jumps
    - Example: TRAP 100 makes LINE 100 reachable even if not in static CFG

  What is preserved:
    - Entry block (always reachable by definition)
    - All blocks reachable via control flow from entry
    - All blocks whose line numbers are referenced by any instruction
    - REM lines that might be referenced by GOTO/GOSUB/TRAP/RESUME/RESTORE

  What is eliminated:
    - Blocks that are BOTH unreachable AND unreferenced
    - Dead code that cannot be reached by any means

  Complexity: O(N * M) where N = blocks, M = instructions (reference collection)
              + O(N + E) for DFS reachability (E = CFG edges)

  Phase: PRE-optimization (before dominator tree construction)
  Author: Sedai Project - Optimization Pipeline
  Date: 2025-01-27
  ============================================================================ }

unit SedaiDBE;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, Generics.Collections, SedaiSSATypes;

type
  { TDeadBlockElimination - Unreachable block removal via smart reachability analysis }
  TDeadBlockElimination = class
  private
    FProgram: TSSAProgram;
    FReachable: array of Boolean;  // [BlockIdx] = IsReachable
    FReferencedLines: specialize TDictionary<Integer, Boolean>;  // Line numbers referenced by instructions
    FRemovedCount: Integer;

    { Try to resolve a register value to its constant definition }
    function TryResolveConstant(Block: TSSABasicBlock; InstrIdx: Integer;
      const Val: TSSAValue; out ConstVal: Integer): Boolean;

    { Collect all line numbers referenced by dynamic jump instructions }
    procedure CollectReferencedLines;

    { Mark all blocks reachable from entry via DFS }
    procedure MarkReachableBlocks;

    { DFS helper - recursively mark block and successors as reachable }
    procedure DFS(BlockIdx: Integer);

    { Check if a block's line number is in the referenced set }
    function IsBlockReferenced(Block: TSSABasicBlock): Boolean;

    { Remove all unmarked AND unreferenced blocks }
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
  FReferencedLines := specialize TDictionary<Integer, Boolean>.Create;
end;

destructor TDeadBlockElimination.Destroy;
begin
  FReferencedLines.Free;
  inherited;
end;

{ Try to resolve a register value to a constant integer.
  Returns True if the register is defined by LoadConstInt in the same block,
  or if it's a direct constant. }
function TDeadBlockElimination.TryResolveConstant(Block: TSSABasicBlock;
  InstrIdx: Integer; const Val: TSSAValue; out ConstVal: Integer): Boolean;
var
  k: Integer;
  DefInstr: TSSAInstruction;
  RegIndex: Integer;
begin
  Result := False;
  ConstVal := 0;

  // Direct constant
  if Val.Kind = svkConstInt then
  begin
    ConstVal := Val.ConstInt;
    Result := True;
    Exit;
  end;

  // If it's a register, look for the definition in this block (before this instruction)
  if Val.Kind = svkRegister then
  begin
    RegIndex := Val.RegIndex;
    // Search backwards from InstrIdx-1 for the definition
    for k := InstrIdx - 1 downto 0 do
    begin
      DefInstr := Block.Instructions[k];
      if (DefInstr.OpCode = ssaLoadConstInt) and
         (DefInstr.Dest.Kind = svkRegister) and
         (DefInstr.Dest.RegIndex = RegIndex) then
      begin
        // Found it - the constant is in Src1
        if DefInstr.Src1.Kind = svkConstInt then
        begin
          ConstVal := DefInstr.Src1.ConstInt;
          Result := True;
        end;
        Exit;
      end;
    end;
  end;
end;

procedure TDeadBlockElimination.CollectReferencedLines;
var
  i, j: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  LineNum: Integer;
begin
  { Scan all instructions to collect line numbers that are referenced.
    This includes:
    - TRAP <line>       (ssaTrap with line number)
    - RESUME <line>     (ssaResume with line number)
    - RESTORE <line>    (ssaDataRestore with line number)

    The line number may be a direct constant OR a register that was
    loaded from a constant (via LoadConstInt) in the same block.

    Note: GOTO and GOSUB use block labels which are already in CFG edges. }

  FReferencedLines.Clear;

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      case Instr.OpCode of
        // TRAP <line> - error handler target
        ssaTrap:
          begin
            if TryResolveConstant(Block, j, Instr.Src1, LineNum) then
            begin
              if LineNum > 0 then
              begin
                FReferencedLines.AddOrSetValue(LineNum, True);
                {$IFDEF DEBUG_DBE}
                if DebugDBE then
                  WriteLn('[DBE] TRAP references line ', LineNum);
                {$ENDIF}
              end;
            end;
          end;

        // RESUME <line> - resume execution at specific line
        ssaResume:
          begin
            if TryResolveConstant(Block, j, Instr.Src1, LineNum) then
            begin
              if LineNum > 0 then
              begin
                FReferencedLines.AddOrSetValue(LineNum, True);
                {$IFDEF DEBUG_DBE}
                if DebugDBE then
                  WriteLn('[DBE] RESUME references line ', LineNum);
                {$ENDIF}
              end;
            end;
          end;

        // RESTORE <line> - DATA pointer reset
        ssaDataRestore:
          begin
            if TryResolveConstant(Block, j, Instr.Src1, LineNum) then
            begin
              if LineNum > 0 then
              begin
                FReferencedLines.AddOrSetValue(LineNum, True);
                {$IFDEF DEBUG_DBE}
                if DebugDBE then
                  WriteLn('[DBE] RESTORE references line ', LineNum);
                {$ENDIF}
              end;
            end;
          end;
      end;
    end;
  end;

  {$IFDEF DEBUG_DBE}
  if DebugDBE then
    WriteLn('[DBE] Collected ', FReferencedLines.Count, ' referenced line numbers');
  {$ENDIF}
end;

function TDeadBlockElimination.IsBlockReferenced(Block: TSSABasicBlock): Boolean;
var
  LineNum: Integer;
  LabelName: string;
begin
  Result := False;

  // Extract line number from block label (e.g., "LINE_100" -> 100)
  LabelName := Block.LabelName;
  if LabelName.StartsWith('LINE_') then
  begin
    if TryStrToInt(Copy(LabelName, 6, Length(LabelName) - 5), LineNum) then
    begin
      Result := FReferencedLines.ContainsKey(LineNum);
      {$IFDEF DEBUG_DBE}
      if DebugDBE and Result then
        WriteLn('[DBE] Block ', LabelName, ' is referenced (line ', LineNum, ')');
      {$ENDIF}
    end;
  end;

  // Also check the first instruction's SourceLine
  if (not Result) and (Block.Instructions.Count > 0) then
  begin
    LineNum := Block.Instructions[0].SourceLine;
    if LineNum > 0 then
    begin
      Result := FReferencedLines.ContainsKey(LineNum);
      {$IFDEF DEBUG_DBE}
      if DebugDBE and Result then
        WriteLn('[DBE] Block ', LabelName, ' is referenced via SourceLine ', LineNum);
      {$ENDIF}
    end;
  end;
end;

function TDeadBlockElimination.Run: Integer;
var
  i: Integer;
begin
  {$IFDEF DEBUG_DBE}
  if DebugDBE then
    WriteLn('[DBE] Running dead block elimination...');
  {$ENDIF}

  // Step 1: Collect all referenced line numbers
  CollectReferencedLines;

  // Step 2: Initialize reachability map
  SetLength(FReachable, FProgram.Blocks.Count);
  for i := 0 to High(FReachable) do
    FReachable[i] := False;

  // Step 3: Mark all blocks reachable from entry via DFS
  MarkReachableBlocks;

  // Step 4: Remove blocks that are BOTH unreachable AND unreferenced
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
  ShouldRemove: Boolean;
begin
  { Remove blocks that are BOTH unreachable AND unreferenced.

    IMPORTANT: A block is kept if:
    - It is reachable via CFG edges (normal control flow)
    - OR its line number is referenced by TRAP/RESUME/RESTORE/etc.

    CRITICAL: Must also clean up predecessor/successor lists in remaining blocks
    to avoid dangling pointers. }

  // Count blocks to be removed (unreachable AND unreferenced)
  UnreachableCount := 0;
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    if not FReachable[i] then
    begin
      Block := FProgram.Blocks[i];
      if not IsBlockReferenced(Block) then
        Inc(UnreachableCount);
    end;
  end;

  if UnreachableCount = 0 then
  begin
    {$IFDEF DEBUG_DBE}
    if DebugDBE then
      WriteLn('[DBE] All blocks are either reachable or referenced - nothing to remove');
    {$ENDIF}
    Exit;
  end;

  {$IFDEF DEBUG_DBE}
  if DebugDBE then
    WriteLn('[DBE] Found ', UnreachableCount, ' blocks to remove (unreachable AND unreferenced)');
  {$ENDIF}

  // Build removal set: blocks that are unreachable AND unreferenced
  // We'll use FReachable array, setting it to True for blocks we want to KEEP
  //
  // IMPORTANT: When we keep a referenced block, we must also keep all blocks
  // that follow it sequentially until we hit a terminator (GOTO, RESUME, END, etc.)
  // This ensures error handlers are complete.
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    if not FReachable[i] then
    begin
      Block := FProgram.Blocks[i];
      if IsBlockReferenced(Block) then
      begin
        // This block is referenced - mark it and all following blocks as "keep"
        // until we hit a block that has explicit control flow (not fallthrough)
        j := i;
        while j < FProgram.Blocks.Count do
        begin
          FReachable[j] := True;
          Block := FProgram.Blocks[j];
          {$IFDEF DEBUG_DBE}
          if DebugDBE then
            WriteLn('[DBE] Keeping block in referenced chain: ', Block.LabelName);
          {$ENDIF}

          // Stop if this block has a terminator (non-fallthrough ending)
          // Check last instruction for control flow
          if Block.Instructions.Count > 0 then
          begin
            if Block.Instructions[Block.Instructions.Count - 1].OpCode in
               [ssaJump, ssaReturn, ssaEnd, ssaStop, ssaResume, ssaResumeNext] then
              Break;
          end;

          // Continue to next block (fallthrough)
          Inc(j);
        end;
      end;
    end;
  end;

  // Pass 1: Clean up predecessor/successor lists
  // Remove references to blocks we're about to delete
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
            WriteLn('[DBE]   Removing successor reference from block ', Block.LabelName);
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
            WriteLn('[DBE]   Removing predecessor reference from block ', Block.LabelName);
          {$ENDIF}
          Block.Predecessors.Delete(j);
          Continue;  // Don't increment j - we just deleted
        end;
      end;
      Inc(j);
    end;
  end;

  // Pass 2: Fix blocks that we're keeping but have no successors
  // These blocks need fallthrough edges to the next kept block
  // (unless they end with a terminator instruction)
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    if not FReachable[i] then
      Continue;  // Will be removed

    Block := FProgram.Blocks[i];

    // Skip if block already has successors
    if Block.Successors.Count > 0 then
      Continue;

    // Skip if block ends with a terminator (no fallthrough needed)
    if Block.Instructions.Count > 0 then
    begin
      if Block.Instructions[Block.Instructions.Count - 1].OpCode in
         [ssaJump, ssaReturn, ssaEnd, ssaStop, ssaResume, ssaResumeNext] then
        Continue;
    end;

    // Find next block that we're keeping and add fallthrough
    for j := i + 1 to FProgram.Blocks.Count - 1 do
    begin
      if FReachable[j] then
      begin
        SuccBlock := FProgram.Blocks[j];
        Block.Successors.Add(SuccBlock);
        SuccBlock.Predecessors.Add(Block);
        {$IFDEF DEBUG_DBE}
        if DebugDBE then
          WriteLn('[DBE] Added fallthrough: ', Block.LabelName, ' -> ', SuccBlock.LabelName);
        {$ENDIF}
        Break;
      end;
    end;
  end;

  // Pass 3: Remove unreachable AND unreferenced blocks from program
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
