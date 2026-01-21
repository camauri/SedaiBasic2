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
  Unit: SedaiDCE (Dead Code Elimination)

  Purpose: Remove dead/unused instructions from SSA IR to reduce bytecode size
           and improve performance.

  Algorithm: SSA-aware worklist algorithm with versioned liveness tracking
             1. Build def→location map for all versioned registers
             2. Mark all instructions with side effects as live (seed worklist)
             3. Backward propagate liveness through def-use chains
             4. Remove all unmarked (dead) instructions

  SSA Properties Exploited:
    - Single static assignment: each (RegIndex, Version) has exactly one def
    - Def-use chains are explicit and acyclic
    - No iterative dataflow needed: worklist converges in O(N)

  What is eliminated:
    - Unused computation results
    - Redundant LoadConst instructions after constant propagation
    - Dead PHI nodes whose results are never used
    - Copy operations where result is never used

  What is preserved:
    - All control flow instructions (jumps, branches, labels)
    - All memory operations (stores, array operations)
    - All I/O operations (print, input)
    - All function calls (may have side effects)
    - Instructions whose results are used by live instructions

  Complexity: O(N) time, O(N) space where N = number of instructions

  Phase: Post-optimization cleanup (after algebraic/constant propagation)
  Author: Sedai Project - Optimization Pipeline
  Date: 2025-01-27 (rewritten for SSA correctness)
  ============================================================================ }

unit SedaiDCE;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, Generics.Collections, SedaiSSATypes;

type
  { TDeadCodeElimination - SSA-aware dead code elimination }
  TDeadCodeElimination = class
  private
    FProgram: TSSAProgram;
    FLiveMap: array of array of Boolean;  // [BlockIdx][InstrIdx] = IsLive
    FRemovedCount: Integer;
    FSSACorrupted: Boolean;  // True if SSA invariants are violated

    { Def-use map: (RegIndex, Version) → (BlockIdx, InstrIdx) that defines it }
    type
      TDefLocation = record
        BlockIdx: Integer;
        InstrIdx: Integer;
      end;

      TWorkItem = record
        BlockIdx: Integer;
        InstrIdx: Integer;
      end;

    var
      FDefMap: specialize TDictionary<Int64, TDefLocation>;
      FWorklist: specialize TQueue<TWorkItem>;

    { Encode (RegIndex, Version) into single Int64 key }
    function MakeDefKey(RegIndex, Version: Integer): Int64; inline;

    { Build def→location map for all versioned registers }
    procedure BuildDefMap;

    { Check if instruction has side effects (must be preserved) }
    function HasSideEffects(const Instr: TSSAInstruction): Boolean;

    { Mark instruction as live and add its dependencies to worklist }
    procedure MarkLiveAndEnqueue(BlockIdx, InstrIdx: Integer);

    { Process worklist: backward propagate liveness through def-use chains }
    procedure ProcessWorklist;

    { Add defining instruction of a register use to worklist }
    procedure EnqueueDefiningInstruction(RegIndex, Version: Integer);

    { Remove dead instructions from all blocks }
    procedure RemoveDeadInstructions;

  public
    constructor Create(Prog: TSSAProgram);
    destructor Destroy; override;

    { Run DCE pass - returns number of instructions removed }
    function Run: Integer;
  end;

implementation

uses SedaiDebug;

{ TDeadCodeElimination }

constructor TDeadCodeElimination.Create(Prog: TSSAProgram);
begin
  inherited Create;
  FProgram := Prog;
  FDefMap := specialize TDictionary<Int64, TDefLocation>.Create;
  FWorklist := specialize TQueue<TWorkItem>.Create;
  FRemovedCount := 0;
  FSSACorrupted := False;
  // LiveMap will be initialized in Run() with current block sizes
end;

destructor TDeadCodeElimination.Destroy;
begin
  FDefMap.Free;
  FWorklist.Free;
  inherited;
end;

function TDeadCodeElimination.MakeDefKey(RegIndex, Version: Integer): Int64;
begin
  { Encode (RegIndex, Version) into single Int64 key.
    Supports up to 1M versions per register (plenty for any program).
    Key = RegIndex * 1000000 + Version }
  Result := Int64(RegIndex) * 1000000 + Int64(Version);
end;

function TDeadCodeElimination.Run: Integer;
var
  i: Integer;
begin
  {$IFDEF DEBUG_DCE}
  if DebugDCE then
    WriteLn('[DCE] Running SSA-aware dead code elimination...');
  {$ENDIF}

  // Step 0: Initialize live map with CURRENT block sizes
  // CRITICAL: Must do this here, not in constructor, because previous
  // optimizations may have changed the number of instructions per block
  SetLength(FLiveMap, FProgram.Blocks.Count);
  for i := 0 to FProgram.Blocks.Count - 1 do
    SetLength(FLiveMap[i], FProgram.Blocks[i].Instructions.Count);

  // Step 1: Build def→location map for fast register lookups
  // This also marks duplicate definitions as unconditionally live
  BuildDefMap;

  {$IFDEF DEBUG_DCE}
  if DebugDCE and FSSACorrupted then
    WriteLn('[DCE] WARNING: SSA corruption detected, but DCE will continue safely');
  {$ENDIF}

  // Step 2: Process worklist to propagate liveness backward
  ProcessWorklist;

  // Step 3: Remove dead instructions
  RemoveDeadInstructions;

  {$IFDEF DEBUG_DCE}
  if DebugDCE then
    WriteLn('[DCE] Removed ', FRemovedCount, ' dead instructions');
  {$ENDIF}
  Result := FRemovedCount;
end;

procedure TDeadCodeElimination.BuildDefMap;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  i, j: Integer;
  DefLoc: TDefLocation;
  Key: Int64;
  PrevDefLoc: TDefLocation;
begin
  { Build map: (RegIndex, Version) → (BlockIdx, InstrIdx) that defines it.
    This enables O(1) lookup when tracing backward through def-use chains.

    CRITICAL: In SSA, each (RegIndex, Version) has exactly ONE definition.

    SAFETY: When we find duplicate definitions (SSA corruption), we mark
    BOTH definitions as unconditionally live to prevent removing instructions
    that might be reachable depending on control flow. }

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      // If this instruction defines a register, record its location
      if Instr.Dest.Kind = svkRegister then
      begin
        DefLoc.BlockIdx := i;
        DefLoc.InstrIdx := j;
        Key := MakeDefKey(Instr.Dest.RegIndex, Instr.Dest.Version);

        // In SSA, this should never overwrite (each version defined once)
        if FDefMap.ContainsKey(Key) then
        begin
          {$IFDEF DEBUG_DCE}
          if DebugDCE then
          begin
            WriteLn('[DCE] WARNING: Duplicate definition of ',
                    SSARegisterTypeToString(Instr.Dest.RegType), '[',
                    Instr.Dest.RegIndex, ']_', Instr.Dest.Version);
            WriteLn('[DCE]   Marking BOTH definitions as unconditionally live');
          end;
          {$ENDIF}

          // Mark the PREVIOUS definition as live (we can't know which one is used)
          FDefMap.TryGetValue(Key, PrevDefLoc);
          MarkLiveAndEnqueue(PrevDefLoc.BlockIdx, PrevDefLoc.InstrIdx);

          // Mark the CURRENT definition as live too
          MarkLiveAndEnqueue(i, j);

          FSSACorrupted := True;
        end;

        FDefMap.AddOrSetValue(Key, DefLoc);
      end;
    end;
  end;

  {$IFDEF DEBUG_DCE}
  if DebugDCE then
    WriteLn('[DCE] Built def→location map with ', FDefMap.Count, ' versioned definitions');
  {$ENDIF}
end;

function TDeadCodeElimination.HasSideEffects(const Instr: TSSAInstruction): Boolean;
begin
  { Instructions with side effects must always be preserved.

    CRITICAL CHANGE: PHI nodes are NO LONGER treated as side effects!
    A PHI is live only if its result is used by a live instruction.
    This enables aggressive elimination of dead PHI nodes. }

  case Instr.OpCode of
    // Control flow - always live (affect program execution)
    ssaJump, ssaJumpIfZero, ssaJumpIfNotZero, ssaReturn:
      Result := True;

    // Program termination and system state - always live
    ssaEnd, ssaStop, ssaFast, ssaSlow, ssaSleep, ssaClear,
    ssaTron, ssaTroff,  // TRON/TROFF switch VM execution mode (side effect)
    ssaTrap, ssaResume, ssaResumeNext:  // Error handling (side effect on VM state)
      Result := True;

    // Labels - always live (control flow targets)
    ssaLabel:
      Result := True;

    // Memory operations - always live (may affect program state)
    ssaStoreVar, ssaArrayStore, ssaArrayDim,
    ssaPoke:  // POKE writes to memory-mapped I/O (visible side effects like color changes)
      Result := True;

    // I/O operations - always live (visible side effects)
    ssaPrint, ssaPrintLn, ssaPrintString, ssaPrintStringLn,
    ssaPrintInt, ssaPrintIntLn,
    ssaPrintComma, ssaPrintSemicolon, ssaPrintTab, ssaPrintSpc, ssaPrintNewLine,
    ssaInput, ssaInputInt, ssaInputFloat, ssaInputString,
    ssaPrintUsing, ssaPudef:
      Result := True;

    // DATA/READ/RESTORE - always live (affects DATA pool state)
    ssaDataAdd, ssaDataRead, ssaDataRestore:
      Result := True;

    // Graphics operations - always live (visible side effects on screen)
    ssaGraphicSetMode, ssaGraphicBox, ssaGraphicCircle, ssaGraphicDraw, ssaGraphicLocate,
    ssaGraphicColor, ssaSetColor, ssaGraphicWidth, ssaGraphicScale, ssaGraphicPaint, ssaGraphicWindow,
    ssaGraphicSShape, ssaGraphicGShape, ssaGraphicGList, ssaPLoad, ssaPSave, ssaScnClr:
      Result := True;

    // Sound operations - always live (audible side effects)
    ssaSoundVol, ssaSoundSound, ssaSoundEnvelope, ssaSoundTempo, ssaSoundPlay, ssaSoundFilter:
      Result := True;

    // Function calls - always live (may have side effects)
    ssaCall:
      Result := True;

    // All other instructions (including PHI!) - only live if result is used
    else
      Result := False;
  end;
end;

procedure TDeadCodeElimination.MarkLiveAndEnqueue(BlockIdx, InstrIdx: Integer);
var
  WorkItem: TWorkItem;
begin
  { Mark instruction as live and add it to worklist for processing.

    This is the only place where we mark instructions as live.
    Idempotent: if already marked, do nothing. }

  if (BlockIdx < 0) or (BlockIdx >= Length(FLiveMap)) then
    Exit;
  if (InstrIdx < 0) or (InstrIdx >= Length(FLiveMap[BlockIdx])) then
    Exit;

  // Already marked as live? Nothing to do
  if FLiveMap[BlockIdx][InstrIdx] then
    Exit;

  // Mark as live
  FLiveMap[BlockIdx][InstrIdx] := True;

  // Add to worklist for dependency processing
  WorkItem.BlockIdx := BlockIdx;
  WorkItem.InstrIdx := InstrIdx;
  FWorklist.Enqueue(WorkItem);
end;

procedure TDeadCodeElimination.EnqueueDefiningInstruction(RegIndex, Version: Integer);
var
  Key: Int64;
  DefLoc: TDefLocation;
begin
  { Find the instruction that defines (RegIndex, Version) and enqueue it.

    This is the core of backward liveness propagation:
    "If X uses R_v, then the instruction defining R_v is live" }

  Key := MakeDefKey(RegIndex, Version);

  if FDefMap.TryGetValue(Key, DefLoc) then
    MarkLiveAndEnqueue(DefLoc.BlockIdx, DefLoc.InstrIdx)
  else
  begin
    // This should never happen in well-formed SSA
    {$IFDEF DEBUG_DCE}
    if DebugDCE then
      WriteLn('[DCE] WARNING: Use of undefined register: RegIndex=', RegIndex,
              ' Version=', Version);
    {$ENDIF}
    FSSACorrupted := True;
  end;
end;

procedure TDeadCodeElimination.ProcessWorklist;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  WorkItem: TWorkItem;
  i, j, k: Integer;
  InitialWorklistSize: Integer;
begin
  { Worklist algorithm:
    1. Seed worklist with all instructions that have side effects
    2. Process worklist: for each live instruction, mark its operands' defs as live
    3. Continue until worklist is empty (fixed point)

    Complexity: O(N) where N = number of instructions
    Each instruction is enqueued at most once, processed at most once. }

  // PHASE 1: Seed worklist with all side-effect instructions
  {$IFDEF DEBUG_DCE}
  if DebugDCE then
    WriteLn('[DCE] Seeding worklist with side-effect instructions...');
  {$ENDIF}

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];
      if HasSideEffects(Instr) then
        MarkLiveAndEnqueue(i, j);
    end;
  end;

  InitialWorklistSize := FWorklist.Count;
  {$IFDEF DEBUG_DCE}
  if DebugDCE then
    WriteLn('[DCE] Worklist seeded with ', InitialWorklistSize, ' critical instructions');
  {$ENDIF}

  // PHASE 2: Process worklist - backward propagate liveness
  {$IFDEF DEBUG_DCE}
  if DebugDCE then
    WriteLn('[DCE] Propagating liveness through def-use chains...');
  {$ENDIF}

  while FWorklist.Count > 0 do
  begin
    WorkItem := FWorklist.Dequeue;

    if (WorkItem.BlockIdx < 0) or (WorkItem.BlockIdx >= FProgram.Blocks.Count) then
      Continue;

    Block := FProgram.Blocks[WorkItem.BlockIdx];

    if (WorkItem.InstrIdx < 0) or (WorkItem.InstrIdx >= Block.Instructions.Count) then
      Continue;

    Instr := Block.Instructions[WorkItem.InstrIdx];

    // Mark dependencies as live based on instruction type

    if Instr.OpCode = ssaPhi then
    begin
      { PHI special case: operands come from different predecessor blocks.
        In SSA semantics, each PHI source is "used" at the end of its
        corresponding predecessor block.

        For DCE, we simply mark all PHI source definitions as live. }

      for i := 0 to High(Instr.PhiSources) do
      begin
        if Instr.PhiSources[i].Value.Kind = svkRegister then
          EnqueueDefiningInstruction(
            Instr.PhiSources[i].Value.RegIndex,
            Instr.PhiSources[i].Value.Version
          );
      end;
    end
    else
    begin
      { Regular instruction: mark all register operands' definitions as live }

      if Instr.Src1.Kind = svkRegister then
        EnqueueDefiningInstruction(Instr.Src1.RegIndex, Instr.Src1.Version);

      if Instr.Src2.Kind = svkRegister then
        EnqueueDefiningInstruction(Instr.Src2.RegIndex, Instr.Src2.Version);

      if Instr.Src3.Kind = svkRegister then
        EnqueueDefiningInstruction(Instr.Src3.RegIndex, Instr.Src3.Version);

      { For instructions that use PhiSources for extra operands (like ssaGraphicRGBA),
        also mark those as live }
      if Length(Instr.PhiSources) > 0 then
      begin
        for i := 0 to High(Instr.PhiSources) do
        begin
          if Instr.PhiSources[i].Value.Kind = svkRegister then
            EnqueueDefiningInstruction(
              Instr.PhiSources[i].Value.RegIndex,
              Instr.PhiSources[i].Value.Version
            );
        end;
      end;
    end;
  end;

  // Count live instructions
  j := 0;
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for k := 0 to Block.Instructions.Count - 1 do
      if FLiveMap[i][k] then
        Inc(j);
  end;

  {$IFDEF DEBUG_DCE}
  if DebugDCE then
    WriteLn('[DCE] Liveness propagation complete: ', j, ' instructions marked live');
  {$ENDIF}
end;

procedure TDeadCodeElimination.RemoveDeadInstructions;
var
  Block: TSSABasicBlock;
  i, j: Integer;
  DeadCount: Integer;
begin
  { Remove all instructions not marked as live.

    NOTE: We keep the TSSAInstruction objects in memory (don't free them)
    because the TSSAInstructionList owns them. We just remove them from
    the list and the list's ownership will handle cleanup. }

  try
    for i := 0 to FProgram.Blocks.Count - 1 do
    begin
      Block := FProgram.Blocks[i];

      if not Assigned(Block) then
      begin
        {$IFDEF DEBUG_DCE}
        if DebugDCE then
          WriteLn('[DCE] ERROR: Block ', i, ' is nil!');
        {$ENDIF}
        Continue;
      end;

      if not Assigned(Block.Instructions) then
      begin
        {$IFDEF DEBUG_DCE}
        if DebugDCE then
          WriteLn('[DCE] ERROR: Block ', i, ' instructions list is nil!');
        {$ENDIF}
        Continue;
      end;

      DeadCount := 0;

      // Count dead instructions in this block
      for j := 0 to Block.Instructions.Count - 1 do
        if not FLiveMap[i][j] then
          Inc(DeadCount);

      // Only rebuild if there are dead instructions
      if DeadCount > 0 then
      begin
        {$IFDEF DEBUG_DCE}
        if DebugDCE then
          WriteLn('[DCE] Block ', i, ': removing ', DeadCount, ' dead instructions:');
        {$ENDIF}

        // Remove dead instructions from end to start (to avoid index shifts)
        // CRITICAL: Use Extract() instead of Delete() to avoid freeing the objects
        for j := Block.Instructions.Count - 1 downto 0 do
        begin
          if not FLiveMap[i][j] then
          begin
            {$IFDEF DEBUG_DCE}
            // Log what we're removing
            if DebugDCE then
            begin
              WriteLn('[DCE]   Removing instr[', j, ']: ',
                      SSAOpCodeToString(Block.Instructions[j].OpCode));
              if Block.Instructions[j].Dest.Kind = svkRegister then
                WriteLn('[DCE]     Dest: ', SSARegisterTypeToString(Block.Instructions[j].Dest.RegType),
                        '[', Block.Instructions[j].Dest.RegIndex, ']_',
                        Block.Instructions[j].Dest.Version);
            end;
            {$ENDIF}

            // Extract removes without freeing, then we free manually
            Block.Instructions.Extract(Block.Instructions[j]).Free;
            Inc(FRemovedCount);
          end;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      {$IFDEF DEBUG_DCE}
      if DebugDCE then
        WriteLn('[DCE] ERROR in RemoveDeadInstructions at block ', i, ': ', E.Message);
      {$ENDIF}
      raise;
    end;
  end;
end;

end.
