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
unit SedaiNopCompaction;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$I OptimizationFlags.inc}
{$I DebugFlags.inc}

{ NOP Compaction pass

  Removes NOP instructions from bytecode and adjusts all jump targets.
  Should run AFTER all optimization passes that generate NOPs (peephole, superinstructions)
  and BEFORE VM execution.

  Algorithm:
  1. Build a mapping from old instruction indices to new (compacted) indices
  2. Scan for NOPs and calculate new positions for non-NOP instructions
  3. Adjust all jump targets using the mapping
  4. Remove NOPs by compacting the instruction array
}

interface

uses
  Classes, SysUtils, SedaiBytecodeTypes;

type
  TNopCompactor = class
  private
    FProgram: TBytecodeProgram;
    FNopCount: Integer;
    FIndexMap: array of Integer;  // Maps old index -> new index

    { Build index mapping (old -> new) accounting for NOP removal }
    procedure BuildIndexMap;

    { Adjust all jump targets using the index map }
    procedure AdjustJumpTargets;

    { Compact the instruction array by removing NOPs }
    procedure CompactInstructions;

  public
    constructor Create(AProgram: TBytecodeProgram);
    destructor Destroy; override;

    { Run NOP compaction - returns number of NOPs removed }
    function Run: Integer;
  end;

function RunNopCompaction(AProgram: TBytecodeProgram): Integer;

implementation

{ TNopCompactor }

constructor TNopCompactor.Create(AProgram: TBytecodeProgram);
begin
  inherited Create;
  FProgram := AProgram;
  FNopCount := 0;
end;

destructor TNopCompactor.Destroy;
begin
  inherited;
end;

procedure TNopCompactor.BuildIndexMap;
var
  i, NewIndex: Integer;
  Count: Integer;
  Instr: TBytecodeInstruction;
begin
  Count := FProgram.GetInstructionCount;
  SetLength(FIndexMap, Count);

  NewIndex := 0;
  for i := 0 to Count - 1 do
  begin
    Instr := FProgram.GetInstruction(i);
    if TBytecodeOp(Instr.OpCode) = bcNop then
    begin
      // This NOP will be removed, map to -1 (though we shouldn't need to use this)
      FIndexMap[i] := -1;
      Inc(FNopCount);
    end
    else
    begin
      // This instruction will be at position NewIndex after compaction
      FIndexMap[i] := NewIndex;
      Inc(NewIndex);
    end;
  end;
end;

function IsJumpOrBranchOp(OpCode: Word): Boolean;
begin
  // Check base bytecode jump instructions
  case OpCode of
    Ord(bcJump), Ord(bcJumpIfZero), Ord(bcJumpIfNotZero), Ord(bcCall):
      Result := True;
    // Fused compare-and-branch (Int)
    bcBranchEqInt, bcBranchNeInt, bcBranchLtInt, bcBranchGtInt, bcBranchLeInt, bcBranchGeInt:
      Result := True;
    // Fused compare-and-branch (Float)
    bcBranchEqFloat, bcBranchNeFloat, bcBranchLtFloat, bcBranchGtFloat, bcBranchLeFloat, bcBranchGeFloat:
      Result := True;
    // Fused compare-zero-and-branch
    bcBranchEqZeroInt, bcBranchNeZeroInt, bcBranchEqZeroFloat, bcBranchNeZeroFloat:
      Result := True;
    // Fused loop increment-and-branch
    bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt:
      Result := True;
    // Array load and branch
    bcArrayLoadIntBranchNZ, bcArrayLoadIntBranchZ:
      Result := True;
  else
    Result := False;
  end;
end;

procedure TNopCompactor.AdjustJumpTargets;
var
  i: Integer;
  Instr: TBytecodeInstruction;
  OldTarget, NewTarget: Integer;
begin
  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    Instr := FProgram.GetInstruction(i);

    // Check if this is a jump/branch instruction
    if IsJumpOrBranchOp(Instr.OpCode) then
    begin
      // Get the current jump target from Immediate field
      OldTarget := Instr.Immediate;

      // Validate target is within range
      if (OldTarget >= 0) and (OldTarget < Length(FIndexMap)) then
      begin
        NewTarget := FIndexMap[OldTarget];

        // If target was a NOP, find the next non-NOP instruction
        if NewTarget = -1 then
        begin
          // Scan forward to find next non-NOP
          while (OldTarget < Length(FIndexMap)) and (FIndexMap[OldTarget] = -1) do
            Inc(OldTarget);

          if OldTarget < Length(FIndexMap) then
            NewTarget := FIndexMap[OldTarget]
          else
            NewTarget := FProgram.GetInstructionCount - FNopCount; // Point to end
        end;

        if NewTarget <> Instr.Immediate then
        begin
          Instr.Immediate := NewTarget;
          FProgram.SetInstruction(i, Instr);
        end;
      end;
    end;
  end;
end;

procedure TNopCompactor.CompactInstructions;
var
  i, WritePos: Integer;
  Instr: TBytecodeInstruction;
  NewInstructions: array of TBytecodeInstruction;
  Count, NewCount: Integer;
begin
  Count := FProgram.GetInstructionCount;
  NewCount := Count - FNopCount;

  if NewCount = Count then
    Exit; // Nothing to compact

  SetLength(NewInstructions, NewCount);

  WritePos := 0;
  for i := 0 to Count - 1 do
  begin
    Instr := FProgram.GetInstruction(i);
    if TBytecodeOp(Instr.OpCode) <> bcNop then
    begin
      NewInstructions[WritePos] := Instr;
      Inc(WritePos);
    end;
  end;

  // Replace program instructions with compacted array
  // Use ClearInstructionsOnly to preserve label lines (for TRAP/RESUME)
  FProgram.ClearInstructionsOnly;
  for i := 0 to NewCount - 1 do
    FProgram.AddInstruction(NewInstructions[i]);
end;

function TNopCompactor.Run: Integer;
begin
  // Step 1: Build mapping from old to new indices
  BuildIndexMap;

  if FNopCount = 0 then
  begin
    Result := 0;
    Exit;
  end;

  // Step 2: Adjust all jump targets
  AdjustJumpTargets;

  // Step 3: Adjust label lines (for TRAP/RESUME targets)
  FProgram.AdjustLabelLines(FIndexMap);

  // Step 4: Adjust source map (for RESUME NEXT and GetSourceLine)
  FProgram.AdjustSourceMap(FIndexMap);

  // Step 5: Compact the instruction array
  CompactInstructions;

  Result := FNopCount;
end;

function RunNopCompaction(AProgram: TBytecodeProgram): Integer;
var
  Compactor: TNopCompactor;
begin
  Compactor := TNopCompactor.Create(AProgram);
  try
    Result := Compactor.Run;
  finally
    Compactor.Free;
  end;
end;

end.
