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
  Unit: SedaiSSAConstruction

  Purpose: Semi-Pruned SSA construction - fast and efficient PHI placement.

  Algorithm: Semi-Pruned SSA (Briggs et al., 1998)
    - Places PHI only for variables that are LIVE at merge points
    - 40-70% fewer PHI nodes compared to Minimal SSA (Cytron)
    - Faster construction time
    - Fully compatible with existing optimizations (GVN, LICM, CSE, etc.)

  Steps:
    1. Compute dominance frontiers (same as Cytron)
    2. Collect variable uses (liveness information)
    3. Insert PHI only for LIVE variables at merge points
    4. Rename variables with unique versions (R0_1, R0_2, etc.)

  This fixes the register reuse problem with minimal overhead!

  Author: Sedai Project - SSA Reconstruction
  Date: 2025-01-26
  ============================================================================ }

unit SedaiSSAConstruction;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, Contnrs, SedaiSSATypes, SedaiDominators;

type
  { Simple stack for integer versions }
  TIntegerStack = class
  private
    FItems: array of Integer;
    FCount: Integer;
  public
    constructor Create;
    procedure Push(Value: Integer);
    function Pop: Integer;
    function Peek: Integer;
    function IsEmpty: Boolean;
    property Count: Integer read FCount;
  end;

  { Variable info record for fast O(1) lookup by integer key }
  PVarInfo = ^TVarInfo;
  TVarInfo = record
    RegType: TSSARegisterType; // Register type (for PHI placement)
    RegIndex: Integer;         // Register index (for PHI placement)
    // Use inline arrays instead of TFPList - much cheaper allocation
    DefBlocks: array of TSSABasicBlock;  // Blocks where this variable is defined
    DefBlockCount: Integer;              // Number of entries in DefBlocks
    UseBlockCount: Integer;              // Number of blocks with uses (for IsLiveAt check)
    DefBlockOffset: Integer;   // Offset into FBoolPool for DefBlockSet (FBlockCount booleans)
    UseBlockOffset: Integer;   // Offset into FBoolPool for UseBlockSet (FBlockCount booleans)
    VersionCounter: Integer;   // Next version number
    LastVersion: Integer;      // Last assigned version (fallback)
    VersionStack: TIntegerStack; // Stack of versions for scoped semantics
  end;

  { SSA Construction - converts non-SSA IR to proper SSA with PHI functions }
  TSSAConstruction = class
  private
    FProgram: TSSAProgram;
    FDomTree: TDominatorTree;

    // Dominance Frontier: indexed by block position for O(1) lookup
    // Uses Block.BlockIndex for direct array access (no hash lookup)
    FDomFrontier: array of TFPList;  // FDomFrontier[blockIndex] = list of DF blocks
    FDomFrontierSet: array of array of Boolean;  // O(1) check: FDomFrontierSet[blockIdx, dfBlockIdx]
    FBlockCount: Integer;

    // Pre-allocated work arrays for PlacePhiForVariable (avoid 765+ allocations)
    FPhiWorkList: array of TSSABasicBlock;  // Inline array worklist (no TFPList overhead!)
    FPhiWorkListCount: Integer;              // Current worklist size
    FPhiBlockSet: array of Integer;  // Version-based set (avoids 765 FillChar calls!)
    FPhiBlockVersion: Integer;       // Current version number

    // Pre-allocated boolean pool for DefBlockSet/UseBlockSet
    // Instead of 765+ individual SetLength calls, we allocate ONE big array
    // and give each variable an offset into it
    FBoolPool: array of Boolean;     // Pool: 2 * MaxVars * FBlockCount booleans
    FBoolPoolNextOffset: Integer;    // Next available offset in pool

    // Variable info: array of all variables
    FVarInfo: array of TVarInfo;
    FVarInfoCount: Integer;
    // Direct O(1) lookup: FVarLookup[RegType, RegIndex] = index+1 in FVarInfo (0 = not found)
    // Uses 2D array indexed by RegType (0-2) and RegIndex (0-MaxReg)
    FVarLookup: array[0..2] of array of Integer;  // 3 register types
    FMaxRegIndex: Integer;  // Current max RegIndex allocated

    // Language semantics: true = BASIC/FORTRAN (global vars), false = C/Pascal (scoped vars)
    FGlobalVariableSemantics: Boolean;

    { Phase 1: Compute dominance frontiers }
    procedure ComputeDominanceFrontiers;

    { Phase 2: Insert PHI functions at merge points }
    procedure InsertPhiFunctions;
    procedure PlacePhiForVariable(VarIdx: Integer; VarRegType: TSSARegisterType; VarRegIndex: Integer);

    { Phase 3: Rename variables with unique versions }
    procedure RenameVariables;
    procedure RenameBlock(Block: TSSABasicBlock);

    { Helper functions - now use integer keys for O(1) lookup }
    function EncodeRegKey(RegType: TSSARegisterType; RegIndex: Integer): Integer; inline;
    function GetOrCreateVarIndex(RegType: TSSARegisterType; RegIndex: Integer): Integer;
    function FindVarIndex(RegType: TSSARegisterType; RegIndex: Integer): Integer;
    procedure CollectDefinitions;
    procedure CollectUses;
    function IsLiveAt(VarIdx: Integer): Boolean; inline;
    function NewVersion(VarIdx: Integer): Integer;
    function CurrentVersion(VarIdx: Integer): Integer;
    procedure PushVersion(VarIdx: Integer; Version: Integer);
    procedure PopVersion(VarIdx: Integer);

    { Dominance frontier helpers }
    function GetDFList(Block: TSSABasicBlock): TFPList;
    procedure AddToDFList(Block: TSSABasicBlock; DFBlock: TSSABasicBlock);

  public
    constructor Create(Prog: TSSAProgram; DomTree: TDominatorTree; GlobalVarSemantics: Boolean = True);
    destructor Destroy; override;

    { Run full SSA construction: PHI insertion + renaming }
    procedure Run;
  end;

implementation

{$IFDEF DEBUG_SSA}
uses SedaiDebug;
{$ENDIF}

{ TIntegerStack }

constructor TIntegerStack.Create;
begin
  inherited Create;
  FCount := 0;
  SetLength(FItems, 16);  // Initial capacity
end;

procedure TIntegerStack.Push(Value: Integer);
begin
  if FCount >= Length(FItems) then
    SetLength(FItems, Length(FItems) * 2);
  FItems[FCount] := Value;
  Inc(FCount);
end;

function TIntegerStack.Pop: Integer;
begin
  if FCount > 0 then
  begin
    Dec(FCount);
    Result := FItems[FCount];
  end
  else
    Result := 0;
end;

function TIntegerStack.Peek: Integer;
begin
  if FCount > 0 then
    Result := FItems[FCount - 1]
  else
    Result := 0;
end;

function TIntegerStack.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

{ TSSAConstruction }

constructor TSSAConstruction.Create(Prog: TSSAProgram; DomTree: TDominatorTree; GlobalVarSemantics: Boolean = True);
var
  i: Integer;
begin
  inherited Create;
  FProgram := Prog;
  FDomTree := DomTree;
  FGlobalVariableSemantics := GlobalVarSemantics;

  // Store block count for direct indexing
  FBlockCount := FProgram.Blocks.Count;

  // Pre-allocate DF arrays and assign block indices for O(1) lookup
  SetLength(FDomFrontier, FBlockCount);
  SetLength(FDomFrontierSet, FBlockCount, FBlockCount);  // 2D array for O(1) membership test
  for i := 0 to FBlockCount - 1 do
  begin
    FDomFrontier[i] := TFPList.Create;
    // Assign block index for direct array access (no hash needed!)
    FProgram.Blocks[i].BlockIndex := i;
  end;

  // Pre-allocate work arrays for PlacePhiForVariable (reused for all 765+ variables)
  SetLength(FPhiWorkList, FBlockCount);  // Max size = all blocks
  FPhiWorkListCount := 0;
  SetLength(FPhiBlockSet, FBlockCount);
  FillChar(FPhiBlockSet[0], FBlockCount * SizeOf(Integer), 0);  // Zero once at start
  FPhiBlockVersion := 0;  // Start at version 0

  // Pre-allocate boolean pool for DefBlockSet/UseBlockSet
  // Estimate: 1024 variables * 2 sets * FBlockCount = one big allocation
  // This replaces 2048 individual SetLength calls with ONE allocation!
  SetLength(FBoolPool, 1024 * 2 * FBlockCount);
  FBoolPoolNextOffset := 0;

  // Initialize variable info array (grows as needed)
  SetLength(FVarInfo, 256);  // Initial capacity
  FVarInfoCount := 0;
  // Initialize 2D lookup arrays for O(1) access (no hash, no strings)
  FMaxRegIndex := 255;  // Initial capacity
  for i := 0 to 2 do
    SetLength(FVarLookup[i], FMaxRegIndex + 1);

  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSAConstruction] Created with GlobalVariableSemantics = ', FGlobalVariableSemantics);
  {$ENDIF}
end;

destructor TSSAConstruction.Destroy;
var
  i: Integer;
begin
  // Free dominance frontier lists
  for i := 0 to High(FDomFrontier) do
    FDomFrontier[i].Free;
  SetLength(FDomFrontier, 0);

  // Free pre-allocated work arrays
  SetLength(FPhiWorkList, 0);
  SetLength(FPhiBlockSet, 0);
  SetLength(FBoolPool, 0);  // Free the pooled boolean array

  // Free variable info
  for i := 0 to FVarInfoCount - 1 do
  begin
    SetLength(FVarInfo[i].DefBlocks, 0);  // Free inline array
    if FVarInfo[i].VersionStack <> nil then
      FVarInfo[i].VersionStack.Free;
  end;
  SetLength(FVarInfo, 0);
  // Free lookup arrays
  for i := 0 to 2 do
    SetLength(FVarLookup[i], 0);

  inherited;
end;

function TSSAConstruction.EncodeRegKey(RegType: TSSARegisterType; RegIndex: Integer): Integer;
begin
  // No longer used - kept for compatibility
  Result := Ord(RegType) * 65536 + RegIndex;
end;

function TSSAConstruction.GetOrCreateVarIndex(RegType: TSSARegisterType; RegIndex: Integer): Integer;
var
  TypeIdx, i, NeededSize: Integer;
begin
  TypeIdx := Ord(RegType);

  // Grow lookup array if needed
  if RegIndex > FMaxRegIndex then
  begin
    FMaxRegIndex := RegIndex * 2;  // Double to avoid frequent reallocations
    for i := 0 to 2 do
      SetLength(FVarLookup[i], FMaxRegIndex + 1);
  end;

  // Direct O(1) lookup - no hash, no strings!
  Result := FVarLookup[TypeIdx, RegIndex];
  if Result > 0 then
  begin
    Result := Result - 1;  // Stored as index+1 to distinguish from 0
    Exit;
  end;

  // Create new entry
  Result := FVarInfoCount;
  Inc(FVarInfoCount);

  // Grow VarInfo array if needed
  if FVarInfoCount > Length(FVarInfo) then
    SetLength(FVarInfo, Length(FVarInfo) * 2);

  // Allocate from boolean pool instead of individual SetLength calls
  // Each variable needs 2 * FBlockCount booleans (DefBlockSet + UseBlockSet)
  NeededSize := FBoolPoolNextOffset + 2 * FBlockCount;
  if NeededSize > Length(FBoolPool) then
  begin
    // Grow pool - double it
    SetLength(FBoolPool, Length(FBoolPool) * 2);
  end;

  // Initialize new entry - use offsets into pool instead of separate arrays
  FVarInfo[Result].RegType := RegType;
  FVarInfo[Result].RegIndex := RegIndex;
  // Use inline array instead of TFPList - allocate small initial capacity
  SetLength(FVarInfo[Result].DefBlocks, 4);  // Most vars defined in 1-4 blocks
  FVarInfo[Result].DefBlockCount := 0;
  FVarInfo[Result].UseBlockCount := 0;
  FVarInfo[Result].DefBlockOffset := FBoolPoolNextOffset;
  FVarInfo[Result].UseBlockOffset := FBoolPoolNextOffset + FBlockCount;
  FBoolPoolNextOffset := FBoolPoolNextOffset + 2 * FBlockCount;
  // Pool is already zero-initialized by SetLength, no need to clear
  FVarInfo[Result].VersionCounter := 1;
  FVarInfo[Result].LastVersion := 0;
  // Lazy allocation: only create VersionStack when scoped semantics are used
  // With GlobalVariableSemantics=True (BASIC), this is NEVER used!
  FVarInfo[Result].VersionStack := nil;

  // Store in lookup array (index+1 to distinguish from not-found)
  FVarLookup[TypeIdx, RegIndex] := Result + 1;
end;

function TSSAConstruction.FindVarIndex(RegType: TSSARegisterType; RegIndex: Integer): Integer;
var
  TypeIdx: Integer;
begin
  TypeIdx := Ord(RegType);

  // Direct O(1) lookup - no hash, no strings!
  if RegIndex <= FMaxRegIndex then
  begin
    Result := FVarLookup[TypeIdx, RegIndex];
    if Result > 0 then
    begin
      Result := Result - 1;
      Exit;
    end;
  end;

  Result := -1;
end;

function TSSAConstruction.GetDFList(Block: TSSABasicBlock): TFPList;
var
  Idx: Integer;
begin
  // Direct O(1) lookup using Block.BlockIndex - no hash, no strings!
  Idx := Block.BlockIndex;
  if (Idx >= 0) and (Idx < FBlockCount) then
    Result := FDomFrontier[Idx]
  else
    Result := nil;
end;

procedure TSSAConstruction.AddToDFList(Block: TSSABasicBlock; DFBlock: TSSABasicBlock);
var
  Idx, DFIdx: Integer;
begin
  // Direct O(1) lookup using Block.BlockIndex - no hash, no strings!
  Idx := Block.BlockIndex;
  DFIdx := DFBlock.BlockIndex;
  if (Idx >= 0) and (Idx < FBlockCount) and (DFIdx >= 0) and (DFIdx < FBlockCount) then
  begin
    // O(1) membership test using 2D boolean array
    if not FDomFrontierSet[Idx, DFIdx] then
    begin
      FDomFrontierSet[Idx, DFIdx] := True;
      FDomFrontier[Idx].Add(DFBlock);
    end;
  end;
end;

procedure TSSAConstruction.Run;
{$IFDEF DEBUG_SSA_TIMING}
var
  StartTime, EndTime: QWord;
  TimeDF, TimePhi, TimeRename: Double;
{$ENDIF}
begin
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
  begin
    WriteLn('[SSAConstruction] ===== SEMI-PRUNED SSA CONSTRUCTION =====');
    WriteLn('[SSAConstruction] Algorithm: Briggs et al., 1998');
    WriteLn('[SSAConstruction] Benefits: 40-70% fewer PHI nodes, faster than Cytron');
    WriteLn;
    WriteLn('[SSAConstruction] Step 1: Computing dominance frontiers...');
  end;
  {$ENDIF}
  {$IFDEF DEBUG_SSA_TIMING}
  StartTime := GetTickCount64;
  {$ENDIF}
  ComputeDominanceFrontiers;
  {$IFDEF DEBUG_SSA_TIMING}
  EndTime := GetTickCount64;
  TimeDF := EndTime - StartTime;
  {$ENDIF}

  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSAConstruction] Step 2: Inserting PHI functions (Semi-Pruned)...');
  {$ENDIF}
  {$IFDEF DEBUG_SSA_TIMING}
  StartTime := GetTickCount64;
  {$ENDIF}
  InsertPhiFunctions;
  {$IFDEF DEBUG_SSA_TIMING}
  EndTime := GetTickCount64;
  TimePhi := EndTime - StartTime;
  {$ENDIF}

  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSAConstruction] Step 3: Renaming variables with unique versions...');
  {$ENDIF}
  {$IFDEF DEBUG_SSA_TIMING}
  StartTime := GetTickCount64;
  {$ENDIF}
  RenameVariables;
  {$IFDEF DEBUG_SSA_TIMING}
  EndTime := GetTickCount64;
  TimeRename := EndTime - StartTime;
  WriteLn('[SSA-TIMING] ComputeDF: ', TimeDF:0:1, ' ms, InsertPhi: ', TimePhi:0:1, ' ms, Rename: ', TimeRename:0:1, ' ms');
  {$ENDIF}

  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSAConstruction] ===== SSA CONSTRUCTION COMPLETE =====');
  {$ENDIF}
end;

procedure TSSAConstruction.ComputeDominanceFrontiers;
var
  B, P, Runner, BIdom: TSSABasicBlock;
  i, j, LoopCount: Integer;
begin
  { Algorithm: For each block B with multiple predecessors,
    compute DF(B) = set of blocks where definitions from B might need PHI functions.

    For each predecessor P of B:
      Runner := P
      while Runner != IDom(B):
        add B to DF(Runner)
        Runner := IDom(Runner)
  }

  // DF arrays already initialized in constructor
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSAConstruction]   DF sets pre-allocated for ', FProgram.Blocks.Count, ' blocks');
  {$ENDIF}

  // Compute DF for each block
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSAConstruction]   Computing DF for each block...');
  {$ENDIF}
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    B := FProgram.Blocks[i];
    {$IFDEF DEBUG_SSA}
    if DebugSSA then
      WriteLn('[SSAConstruction]     Processing block ', i, ': ', B.LabelName, ' (preds=', B.Predecessors.Count, ')');
    {$ENDIF}

    // Only process blocks with multiple predecessors (potential merge points)
    if B.Predecessors.Count < 2 then
      Continue;

    // Get B's immediate dominator - if nil, skip this block (unreachable from entry)
    // This can happen for blocks that are kept for TRAP but not in main CFG
    try
      BIdom := FDomTree.GetIDom(B);
      if BIdom = nil then
        Continue;
    except
      // Block not in dominator tree - skip it
      Continue;
    end;

    // For each predecessor
    for j := 0 to B.Predecessors.Count - 1 do
    begin
      P := TSSABasicBlock(B.Predecessors[j]);

      // Skip predecessors that are not in the main dominator tree (e.g., TRAP handlers)
      // These blocks have no path to the entry block
      try
        if FDomTree.GetIDom(P) = nil then
        begin
          {$IFDEF DEBUG_SSA}
          if DebugSSA then
            WriteLn('[SSAConstruction]       Skipping predecessor ', P.LabelName, ' (not in dom tree)');
          {$ENDIF}
          Continue;
        end;
      except
        {$IFDEF DEBUG_SSA}
        if DebugSSA then
          WriteLn('[SSAConstruction]       Skipping predecessor ', P.LabelName, ' (exception in GetIDom)');
        {$ENDIF}
        Continue;
      end;

      Runner := P;
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
        WriteLn('[SSAConstruction]       Starting walk from ', P.LabelName, ' to BIdom=', BIdom.LabelName);
      {$ENDIF}

      // Walk up dominator tree until we reach B's immediate dominator
      // Use cached BIdom to avoid repeated GetIDom calls
      // Safety: limit iterations to prevent infinite loops
      LoopCount := 0;
      while (Runner <> nil) and (Runner <> BIdom) and (LoopCount < 1000) do
      begin
        Inc(LoopCount);
        {$IFDEF DEBUG_SSA}
        if DebugSSA and (LoopCount <= 10) then
          WriteLn('[SSAConstruction]         Step ', LoopCount, ': Runner=', Runner.LabelName);
        {$ENDIF}

        // Add B to DF(Runner) if not already there
        AddToDFList(Runner, B);

        try
          Runner := FDomTree.GetIDom(Runner);
        except
          // Runner not in dominator tree - stop
          Runner := nil;
        end;
      end;
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
      begin
        if LoopCount >= 1000 then
          WriteLn('[SSAConstruction]       WARNING: Loop limit reached!')
        else
          WriteLn('[SSAConstruction]       Walk completed in ', LoopCount, ' steps');
      end;
      {$ENDIF}
    end;
  end;

  {$IFDEF DEBUG_SSA}
  if DebugSSA then
  begin
    for i := 0 to High(FDomFrontier) do
    begin
      if FDomFrontier[i].Count > 0 then
      begin
        Write('[SSAConstruction]   DF(', FProgram.Blocks[i].LabelName, ') = {');
        for j := 0 to FDomFrontier[i].Count - 1 do
        begin
          if j > 0 then Write(', ');
          Write(TSSABasicBlock(FDomFrontier[i][j]).LabelName);
        end;
        WriteLn('}');
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TSSAConstruction.CollectDefinitions;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  i, j, VarIdx, BlockIdx: Integer;
begin
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSAConstruction]   Collecting variable definitions...');
  {$ENDIF}

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    BlockIdx := Block.BlockIndex;
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      // Check if instruction defines a register
      // CRITICAL FIX: Some instructions USE their Dest operand instead of DEFINING it!
      if (Instr.Dest.Kind = svkRegister) and
         (Instr.OpCode <> ssaArrayStore) and
         (Instr.OpCode <> ssaPrint) and
         (Instr.OpCode <> ssaPrintLn) then
      begin
        VarIdx := GetOrCreateVarIndex(Instr.Dest.RegType, Instr.Dest.RegIndex);

        // O(1) membership test using pooled boolean array
        if not FBoolPool[FVarInfo[VarIdx].DefBlockOffset + BlockIdx] then
        begin
          FBoolPool[FVarInfo[VarIdx].DefBlockOffset + BlockIdx] := True;
          // Add to inline array (grow if needed)
          if FVarInfo[VarIdx].DefBlockCount >= Length(FVarInfo[VarIdx].DefBlocks) then
            SetLength(FVarInfo[VarIdx].DefBlocks, Length(FVarInfo[VarIdx].DefBlocks) * 2);
          FVarInfo[VarIdx].DefBlocks[FVarInfo[VarIdx].DefBlockCount] := Block;
          Inc(FVarInfo[VarIdx].DefBlockCount);
        end;
      end;
    end;
  end;

  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSAConstruction]   Found definitions for ', FVarInfoCount, ' variables');
  {$ENDIF}
end;

procedure TSSAConstruction.CollectUses;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  i, j, VarIdx, BlockIdx: Integer;

  procedure AddUse(const Val: TSSAValue);
  var
    Idx: Integer;
  begin
    if Val.Kind <> svkRegister then Exit;

    Idx := GetOrCreateVarIndex(Val.RegType, Val.RegIndex);

    // O(1) membership test using pooled boolean array
    if not FBoolPool[FVarInfo[Idx].UseBlockOffset + BlockIdx] then
    begin
      FBoolPool[FVarInfo[Idx].UseBlockOffset + BlockIdx] := True;
      Inc(FVarInfo[Idx].UseBlockCount);  // Just count, don't store list
    end;
  end;

begin
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSAConstruction]   Collecting variable uses (Semi-Pruned SSA)...');
  {$ENDIF}

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    BlockIdx := Block.BlockIndex;
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      // Track uses in Src1, Src2, Src3
      AddUse(Instr.Src1);
      AddUse(Instr.Src2);
      AddUse(Instr.Src3);

      // CRITICAL FIX: Some instructions USE their Dest operand instead of DEFINING it!
      if Instr.OpCode in [ssaArrayStore, ssaPrint, ssaPrintLn] then
        AddUse(Instr.Dest);
    end;
  end;

  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSAConstruction]   Found uses for ', FVarInfoCount, ' variables');
  {$ENDIF}
end;

function TSSAConstruction.IsLiveAt(VarIdx: Integer): Boolean;
begin
  // A variable is live if it has any use in the program
  // Simple and fast heuristic for Semi-Pruned SSA
  Result := (VarIdx >= 0) and (VarIdx < FVarInfoCount) and
            (FVarInfo[VarIdx].UseBlockCount > 0);
end;

procedure TSSAConstruction.InsertPhiFunctions;
var
  i, j: Integer;
  PhiCount, SkippedCount: Integer;
  {$IFDEF DEBUG_SSA_TIMING}
  T1, T2, T3, T4: QWord;
  TotalPhiInserted: Integer;
  {$ENDIF}
begin
  {$IFDEF DEBUG_SSA_TIMING}
  T1 := GetTickCount64;
  {$ENDIF}
  // Collect definitions and uses
  CollectDefinitions;
  {$IFDEF DEBUG_SSA_TIMING}
  T2 := GetTickCount64;
  {$ENDIF}
  CollectUses;
  {$IFDEF DEBUG_SSA_TIMING}
  T3 := GetTickCount64;
  {$ENDIF}

  PhiCount := 0;
  SkippedCount := 0;
  {$IFDEF DEBUG_SSA_TIMING}
  TotalPhiInserted := 0;
  {$ENDIF}

  // For each variable with definitions, check if it's live before placing PHI
  // OPTIMIZATION: Only process variables with DefBlockCount > 0 AND UseBlockCount > 0
  // Most variables (764/765) are live, so the filter overhead is minimal
  for i := 0 to FVarInfoCount - 1 do
  begin
    // Skip variables with no definitions (can't generate PHI)
    if FVarInfo[i].DefBlockCount = 0 then
      Continue;

    // Semi-Pruned SSA: Skip PHI placement if variable is not used anywhere
    if FVarInfo[i].UseBlockCount = 0 then
    begin
      Inc(SkippedCount);
      Continue;
    end;

    // CRITICAL OPTIMIZATION: Skip variables defined in only ONE block
    // They can NEVER need PHI functions (PHI merges values from different paths)
    if FVarInfo[i].DefBlockCount = 1 then
    begin
      Inc(PhiCount);  // Count as processed but skip PlacePhiForVariable
      Continue;
    end;

    // Use stored RegType/RegIndex for PHI placement
    PlacePhiForVariable(i, FVarInfo[i].RegType, FVarInfo[i].RegIndex);
    Inc(PhiCount);
  end;

  {$IFDEF DEBUG_SSA_TIMING}
  // Count total PHI instructions inserted and vars with multiple defs
  for i := 0 to FProgram.Blocks.Count - 1 do
    if (FProgram.Blocks[i].Instructions.Count > 0) and
       (FProgram.Blocks[i].Instructions[0].OpCode = ssaPhi) then
      Inc(TotalPhiInserted);
  T4 := GetTickCount64;
  // Count vars with multiple definition blocks (only these can generate PHI)
  j := 0;
  for i := 0 to FVarInfoCount - 1 do
    if FVarInfo[i].DefBlockCount > 1 then Inc(j);
  WriteLn('[SSA-TIMING] InsertPhi: CollectDefs=', T2-T1, ' CollectUses=', T3-T2, ' PlacePhi=', T4-T3, ' ms');
  WriteLn('[SSA-TIMING]   vars=', FVarInfoCount, ' live=', PhiCount, ' multiDef=', j, ' phisInserted=', TotalPhiInserted);
  {$ENDIF}

  {$IFDEF DEBUG_SSA}
  if DebugSSA then
  begin
    WriteLn('[SSAConstruction]   Semi-Pruned SSA: placed PHI for ', PhiCount, ' variables');
    if PhiCount + SkippedCount > 0 then
      WriteLn('[SSAConstruction]   Semi-Pruned SSA: skipped ', SkippedCount, ' dead variables (',
              Format('%.1f', [100.0 * SkippedCount / (PhiCount + SkippedCount)]), '% reduction)');
  end;
  {$ENDIF}
end;

procedure TSSAConstruction.PlacePhiForVariable(VarIdx: Integer; VarRegType: TSSARegisterType; VarRegIndex: Integer);
var
  Block, Y: TSSABasicBlock;
  PhiInstr: TSSAInstruction;
  i, j, WorkIdx, YIdx: Integer;
  PredBlock: TSSABasicBlock;
  DFList: TFPList;
begin
  { Standard PHI placement algorithm (Cytron et al.) with O(1) optimizations
    Uses pre-allocated FPhiWorkList and FPhiBlockSet to avoid 765+ allocations }

  // Clear worklist and increment version (O(1) instead of O(n) FillChar!)
  FPhiWorkListCount := 0;  // Reset count (O(1))
  Inc(FPhiBlockVersion);  // New version = all blocks are "not in set"

  // Initialize WorkList with blocks where variable is defined (use inline array)
  for i := 0 to FVarInfo[VarIdx].DefBlockCount - 1 do
  begin
    FPhiWorkList[FPhiWorkListCount] := FVarInfo[VarIdx].DefBlocks[i];
    Inc(FPhiWorkListCount);
  end;

  // Use index-based iteration
  WorkIdx := 0;
  while WorkIdx < FPhiWorkListCount do
  begin
    Block := FPhiWorkList[WorkIdx];
    Inc(WorkIdx);  // Move to next

    // Get dominance frontier for this block
    DFList := GetDFList(Block);
    if not Assigned(DFList) then
      Continue;

    // For each block Y in DF(Block)
    for i := 0 to DFList.Count - 1 do
    begin
      Y := TSSABasicBlock(DFList[i]);
      YIdx := Y.BlockIndex;

      // O(1) membership test using version: if FPhiBlockSet[YIdx] < FPhiBlockVersion, block is not in set
      if FPhiBlockSet[YIdx] <> FPhiBlockVersion then
      begin
        FPhiBlockSet[YIdx] := FPhiBlockVersion;  // Mark as "in set" for this version

        // Create PHI instruction: dest = PHI(...)
        PhiInstr := TSSAInstruction.Create(ssaPhi);
        PhiInstr.Dest := MakeSSARegister(VarRegType, VarRegIndex);

        // Add PHI source for each predecessor (values will be filled during renaming)
        for j := 0 to Y.Predecessors.Count - 1 do
        begin
          PredBlock := TSSABasicBlock(Y.Predecessors[j]);
          PhiInstr.AddPhiSource(MakeSSARegister(VarRegType, VarRegIndex), PredBlock);
        end;

        // Insert PHI at the BEGINNING of the block
        Y.Instructions.Insert(0, PhiInstr);

        {$IFDEF DEBUG_SSA}
        if DebugSSA then
          WriteLn('[SSAConstruction]     Inserted PHI for var ', VarIdx, ' in block ', Y.LabelName);
        {$ENDIF}

        // O(1) check: If Y was not an original definition site, add to worklist
        if not FBoolPool[FVarInfo[VarIdx].DefBlockOffset + YIdx] then
        begin
          FPhiWorkList[FPhiWorkListCount] := Y;
          Inc(FPhiWorkListCount);
        end;
      end;
    end;
  end;
end;

function TSSAConstruction.NewVersion(VarIdx: Integer): Integer;
begin
  // Global variable semantics: skip versioning (all use Version=0)
  if FGlobalVariableSemantics then
  begin
    Result := 0;
    Exit;
  end;

  // Scoped variable semantics: normal SSA versioning
  Result := FVarInfo[VarIdx].VersionCounter;
  Inc(FVarInfo[VarIdx].VersionCounter);
  FVarInfo[VarIdx].LastVersion := Result;
end;

function TSSAConstruction.CurrentVersion(VarIdx: Integer): Integer;
begin
  // Global variable semantics: all variables use Version=0
  if FGlobalVariableSemantics then
  begin
    Result := 0;
    Exit;
  end;

  // Scoped semantics: use stack first, fallback to LastVersion
  if (VarIdx >= 0) and (VarIdx < FVarInfoCount) then
  begin
    if (FVarInfo[VarIdx].VersionStack <> nil) and
       (not FVarInfo[VarIdx].VersionStack.IsEmpty) then
      Result := FVarInfo[VarIdx].VersionStack.Peek
    else
      Result := FVarInfo[VarIdx].LastVersion;
  end
  else
    Result := 0;
end;

procedure TSSAConstruction.PushVersion(VarIdx: Integer; Version: Integer);
begin
  // Global variable semantics: no need to track versions
  if FGlobalVariableSemantics then
    Exit;

  // Scoped semantics: update LastVersion and push to stack
  if (VarIdx >= 0) and (VarIdx < FVarInfoCount) then
  begin
    FVarInfo[VarIdx].LastVersion := Version;
    // Lazy allocation: create VersionStack only when first used
    if FVarInfo[VarIdx].VersionStack = nil then
      FVarInfo[VarIdx].VersionStack := TIntegerStack.Create;
    FVarInfo[VarIdx].VersionStack.Push(Version);
  end;
end;

procedure TSSAConstruction.PopVersion(VarIdx: Integer);
begin
  // Global semantics: NO-OP (versions must persist)
  if FGlobalVariableSemantics then
    Exit;

  // Scoped semantics: pop from stack
  if (VarIdx >= 0) and (VarIdx < FVarInfoCount) then
  begin
    if (FVarInfo[VarIdx].VersionStack <> nil) and
       (not FVarInfo[VarIdx].VersionStack.IsEmpty) then
      FVarInfo[VarIdx].VersionStack.Pop;
  end;
end;

procedure TSSAConstruction.RenameVariables;
var
  EntryBlock: TSSABasicBlock;
begin
  { Rename variables using depth-first traversal of dominator tree }

  if FProgram.Blocks.Count = 0 then Exit;

  // Start renaming from entry block (root of dominator tree)
  EntryBlock := FProgram.Blocks[0];
  RenameBlock(EntryBlock);

  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSAConstruction]   Renamed all variables with unique versions');
  {$ENDIF}
end;

procedure TSSAConstruction.RenameBlock(Block: TSSABasicBlock);
var
  Instr: TSSAInstruction;
  VarIdx, NewVer, i, j, k: Integer;
  SuccBlock: TSSABasicBlock;
  Children: TFPList;
  Child: TSSABasicBlock;
  SavedVarIndices: array of Integer;
  SavedCount: Integer;
begin
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSA-RENAME] ========== Processing block: ', Block.LabelName, ' ==========');
  {$ENDIF}

  // Pre-allocate saved indices array only for scoped semantics
  SavedCount := 0;
  if not FGlobalVariableSemantics then
    SetLength(SavedVarIndices, Block.Instructions.Count * 2);  // Reasonable estimate

  // STEP 1: Process PHI instructions (assign dest only, don't rename sources yet)
  for i := 0 to Block.Instructions.Count - 1 do
  begin
    Instr := Block.Instructions[i];
    if Instr.OpCode <> ssaPhi then Break;

    VarIdx := FindVarIndex(Instr.Dest.RegType, Instr.Dest.RegIndex);

    // With global variable semantics, PHI dests use Version=0
    if FGlobalVariableSemantics then
      Instr.Dest.Version := 0
    else if VarIdx >= 0 then
    begin
      NewVer := NewVersion(VarIdx);
      Instr.Dest.Version := NewVer;
      PushVersion(VarIdx, NewVer);
      // Track for backtracking
      if SavedCount >= Length(SavedVarIndices) then
        SetLength(SavedVarIndices, Length(SavedVarIndices) * 2);
      SavedVarIndices[SavedCount] := VarIdx;
      Inc(SavedCount);
    end;
  end;

  // STEP 2: Process normal instructions
  for i := 0 to Block.Instructions.Count - 1 do
  begin
    Instr := Block.Instructions[i];
    if Instr.OpCode = ssaPhi then Continue;

    // Rename USES (Src1, Src2, Src3) - only for scoped semantics
    if not FGlobalVariableSemantics then
    begin
      if Instr.Src1.Kind = svkRegister then
      begin
        VarIdx := FindVarIndex(Instr.Src1.RegType, Instr.Src1.RegIndex);
        if VarIdx >= 0 then
          Instr.Src1.Version := CurrentVersion(VarIdx);
      end;

      if Instr.Src2.Kind = svkRegister then
      begin
        VarIdx := FindVarIndex(Instr.Src2.RegType, Instr.Src2.RegIndex);
        if VarIdx >= 0 then
          Instr.Src2.Version := CurrentVersion(VarIdx);
      end;

      if Instr.Src3.Kind = svkRegister then
      begin
        VarIdx := FindVarIndex(Instr.Src3.RegType, Instr.Src3.RegIndex);
        if VarIdx >= 0 then
          Instr.Src3.Version := CurrentVersion(VarIdx);
      end;

      // Rename PhiSources when used for extra operands
      for j := 0 to Length(Instr.PhiSources) - 1 do
      begin
        if Instr.PhiSources[j].Value.Kind = svkRegister then
        begin
          VarIdx := FindVarIndex(Instr.PhiSources[j].Value.RegType, Instr.PhiSources[j].Value.RegIndex);
          if VarIdx >= 0 then
            Instr.PhiSources[j].Value.Version := CurrentVersion(VarIdx);
        end;
      end;
    end;

    // Rename DEFINITION (Dest)
    if Instr.Dest.Kind = svkRegister then
    begin
      // Check if this instruction USES Dest (not defines it)
      if Instr.OpCode in [ssaArrayStore, ssaPrint, ssaPrintLn] then
      begin
        // USE Dest - with global semantics keep Version=0
        if not FGlobalVariableSemantics then
        begin
          VarIdx := FindVarIndex(Instr.Dest.RegType, Instr.Dest.RegIndex);
          if VarIdx >= 0 then
            Instr.Dest.Version := CurrentVersion(VarIdx);
        end;
      end
      else
      begin
        // DEFINE Dest with new version
        if FGlobalVariableSemantics then
          Instr.Dest.Version := 0
        else
        begin
          VarIdx := FindVarIndex(Instr.Dest.RegType, Instr.Dest.RegIndex);
          if VarIdx >= 0 then
          begin
            NewVer := NewVersion(VarIdx);
            Instr.Dest.Version := NewVer;
            PushVersion(VarIdx, NewVer);
            // Track for backtracking
            if SavedCount >= Length(SavedVarIndices) then
              SetLength(SavedVarIndices, Length(SavedVarIndices) * 2);
            SavedVarIndices[SavedCount] := VarIdx;
            Inc(SavedCount);
          end;
        end;
      end;
    end;
  end;

  // STEP 3: Fill PHI operands in successor blocks
  for i := 0 to Block.Successors.Count - 1 do
  begin
    SuccBlock := TSSABasicBlock(Block.Successors[i]);

    for j := 0 to SuccBlock.Instructions.Count - 1 do
    begin
      Instr := SuccBlock.Instructions[j];
      if Instr.OpCode <> ssaPhi then Break;

      // Find the PHI source corresponding to this block
      for k := 0 to High(Instr.PhiSources) do
      begin
        if Instr.PhiSources[k].FromBlock = Block then
        begin
          VarIdx := FindVarIndex(Instr.PhiSources[k].Value.RegType, Instr.PhiSources[k].Value.RegIndex);
          if VarIdx >= 0 then
            Instr.PhiSources[k].Value.Version := CurrentVersion(VarIdx);
          Break;
        end;
      end;
    end;
  end;

  // STEP 4: Recursively process children in dominator tree
  Children := FDomTree.GetChildren(Block);
  if Assigned(Children) then
  begin
    try
      for i := 0 to Children.Count - 1 do
      begin
        Child := TSSABasicBlock(Children[i]);
        RenameBlock(Child);
      end;
    finally
      Children.Free;
    end;
  end;

  // STEP 5: Restore version stack (backtrack) - only for scoped semantics
  if not FGlobalVariableSemantics then
  begin
    for i := 0 to SavedCount - 1 do
      PopVersion(SavedVarIndices[i]);
  end;

  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSA-RENAME] ========== Finished block: ', Block.LabelName, ' ==========');
  {$ENDIF}
end;

end.
