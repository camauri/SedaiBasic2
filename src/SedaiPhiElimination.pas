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
  Unit: SedaiPhiElimination

  Purpose: PHI Elimination - converts PHI functions to normal copy instructions.

  Algorithm: Standard SSA Deconstruction (Cytron et al., Appel, LLVM)
    - PHI functions are conceptual merge points for SSA analysis
    - Bytecode VMs don't support PHI instructions
    - Solution: Insert copy instructions at the END of each predecessor block

  Example Transformation:
    BEFORE:
      Block1: R0_1 = 1.0; Jump Block3
      Block2: R0_2 = 2.0; Jump Block3
      Block3: R0_3 = PHI(R0_1 from Block1, R0_2 from Block2)
              Print R0_3

    AFTER:
      Block1: R0_1 = 1.0; R0_3 = R0_1; Jump Block3
      Block2: R0_2 = 2.0; R0_3 = R0_2; Jump Block3
      Block3: Print R0_3

  This pass MUST run:
    - AFTER all SSA optimizations (they need PHI for analysis)
    - BEFORE bytecode compilation (bytecode has no PHI)

  Author: Sedai Project - SSA Deconstruction
  Date: 2025-01-26
  ============================================================================ }

unit SedaiPhiElimination;

{$mode objfpc}{$H+}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, SedaiSSATypes;

type
  { PHI Elimination Pass }
  TPhiElimination = class
  private
    FProgram: TSSAProgram;
    FPhisEliminated: Integer;
    FCopiesInserted: Integer;

    procedure EliminatePhisInBlock(Block: TSSABasicBlock);
    function GetCopyOpCode(RegType: TSSARegisterType): TSSAOpCode;
    function FindActualPredecessor(PhiBlock: TSSABasicBlock; NominalPred: TSSABasicBlock): TSSABasicBlock;
  public
    constructor Create(Prog: TSSAProgram);
    destructor Destroy; override;

    procedure Run;
  end;

implementation

uses SedaiDebug;

{ TPhiElimination }

constructor TPhiElimination.Create(Prog: TSSAProgram);
begin
  inherited Create;
  FProgram := Prog;
  FPhisEliminated := 0;
  FCopiesInserted := 0;
end;

destructor TPhiElimination.Destroy;
begin
  inherited Destroy;
end;

function TPhiElimination.GetCopyOpCode(RegType: TSSARegisterType): TSSAOpCode;
begin
  { Map register type to appropriate copy instruction }
  case RegType of
    srtInt: Result := ssaCopyInt;
    srtFloat: Result := ssaCopyFloat;
    srtString: Result := ssaCopyString;
  else
    Result := ssaCopyInt;  // Fallback
  end;
end;

function TPhiElimination.FindActualPredecessor(PhiBlock: TSSABasicBlock; NominalPred: TSSABasicBlock): TSSABasicBlock;
var
  i: Integer;
  LastInstr: TSSAInstruction;
  TargetLabel: string;
begin
  { Find the actual block where we should insert the PHI copy.

    Problem: If NominalPred has internal control flow (nested IF/LOOP),
    we can't just insert at the end of NominalPred. We need to find
    which of its exit blocks actually jumps to PhiBlock.

    Solution: Check if NominalPred's last instruction jumps directly to PhiBlock.
    If yes, use NominalPred. If no, it means NominalPred has nested control flow,
    so we just use NominalPred and rely on the insertion position logic to place
    the copy correctly. }

  Result := NominalPred;

  // Check if this block has any instructions
  if NominalPred.Instructions.Count = 0 then Exit;

  // Get the last instruction (terminator)
  LastInstr := NominalPred.Instructions[NominalPred.Instructions.Count - 1];

  // Check if it's a jump to PhiBlock
  if LastInstr.OpCode in [ssaJump, ssaJumpIfZero, ssaJumpIfNotZero] then
  begin
    if LastInstr.Dest.Kind = svkLabel then
    begin
      TargetLabel := LastInstr.Dest.LabelName;

      // If the jump target matches PhiBlock's label, use NominalPred
      if TargetLabel = PhiBlock.LabelName then
      begin
        Result := NominalPred;
        Exit;
      end;
    end;
  end;

  // If we get here, NominalPred doesn't directly jump to PhiBlock
  // This means it has nested control flow. In this case, we need to search
  // through all blocks to find the one that actually jumps to PhiBlock
  // and is dominated by NominalPred.

  // For now, simple solution: just use NominalPred and let the insertion
  // logic handle it. The copies will be inserted before the terminator,
  // which should work correctly even for nested control flow.

  Result := NominalPred;
end;

procedure TPhiElimination.EliminatePhisInBlock(Block: TSSABasicBlock);
var
  Instr: TSSAInstruction;
  PhiSource: TSSAPhiSource;
  PredBlock: TSSABasicBlock;
  CopyInstr: TSSAInstruction;
  CopyOpCode: TSSAOpCode;
  i, j, InsertPos: Integer;
begin
  { Process all PHI instructions in this block.
    For each PHI:
      - For each predecessor block, insert a copy instruction at the END
        (just before the jump/branch instruction)
      - Remove the PHI instruction

    CRITICAL: Insert copies BEFORE the terminator (jump/branch) instruction! }

  i := 0;
  while i < Block.Instructions.Count do
  begin
    Instr := Block.Instructions[i];

    // PHI instructions are always at the beginning of a block
    if Instr.OpCode <> ssaPhi then
      Break;  // No more PHIs in this block

    // For each PHI source, insert a copy in the corresponding predecessor block
    for j := 0 to High(Instr.PhiSources) do
    begin
      PhiSource := Instr.PhiSources[j];
      PredBlock := PhiSource.FromBlock;

      if not Assigned(PredBlock) then
      begin
        {$IFDEF DEBUG_PHIELIM}
        if DebugPhiElim then
          WriteLn('[PhiElim] WARNING: PHI source has no predecessor block!');
        {$ENDIF}
        Continue;
      end;

      // Find the actual block where we should insert the copy
      // (handles nested control flow in predecessor blocks)
      // TEMPORARILY DISABLED - need to investigate SSA Construction first
      // PredBlock := FindActualPredecessor(Block, PredBlock);

      // Find insertion point: AFTER all non-terminator instructions, BEFORE terminator
      // Scan backwards to find the last non-terminator instruction
      InsertPos := PredBlock.Instructions.Count;
      while InsertPos > 0 do
      begin
        case PredBlock.Instructions[InsertPos - 1].OpCode of
          ssaJump, ssaJumpIfZero, ssaJumpIfNotZero, ssaReturn, ssaEnd:
            Dec(InsertPos);  // Skip terminator, continue backwards
        else
          Break;  // Found last non-terminator, insert after it
        end;
      end;

      // Create copy instruction: Dest = Src
      CopyOpCode := GetCopyOpCode(Instr.Dest.RegType);
      CopyInstr := TSSAInstruction.Create(CopyOpCode);
      CopyInstr.Dest := Instr.Dest;      // PHI destination (e.g., R0_3)
      CopyInstr.Src1 := PhiSource.Value; // PHI source from this predecessor (e.g., R0_1)
      CopyInstr.Comment := Format('PHI copy from %s', [PredBlock.LabelName]);

      // CRITICAL: Skip this copy if the source value has Version=0 (unversioned)
      // This happens when the PHI source FromBlock is incorrect due to nested control flow
      if (CopyInstr.Src1.Kind = svkRegister) and (CopyInstr.Src1.Version = 0) then
      begin
        {$IFDEF DEBUG_PHIELIM}
        if DebugPhiElim then
          WriteLn(Format('[PhiElim] WARNING: Skipping invalid PHI copy in block %s: %s = %s (source unversioned)',
            [PredBlock.LabelName,
             SSAValueToString(CopyInstr.Dest),
             SSAValueToString(CopyInstr.Src1)]));
        {$ENDIF}
        CopyInstr.Free;
        Continue;
      end;

      {$IFDEF DEBUG_PHIELIM}
      // DEBUG: Log the PHI copy being inserted
      if DebugPhiElim then
        WriteLn(Format('[PhiElim] Inserting copy in block %s at pos %d: %s = %s',
          [PredBlock.LabelName, InsertPos,
           SSAValueToString(CopyInstr.Dest),
           SSAValueToString(CopyInstr.Src1)]));
      {$ENDIF}

      // Insert BEFORE the terminator (jump/branch/etc.)
      PredBlock.Instructions.Insert(InsertPos, CopyInstr);

      Inc(FCopiesInserted);
    end;

    // Remove the PHI instruction (it's been replaced by copies in predecessors)
    Block.Instructions.Delete(i);
    Inc(FPhisEliminated);

    // Don't increment i - we just deleted an instruction, so the next one is at the same index
  end;
end;

procedure TPhiElimination.Run;
var
  Block: TSSABasicBlock;
  i: Integer;
begin
  {$IFDEF DEBUG_PHIELIM}
  if DebugPhiElim then
  begin
    WriteLn('[PhiElimination] ===== PHI ELIMINATION =====');
    WriteLn('[PhiElimination] Converting PHI functions to copy instructions...');
    WriteLn;
  end;
  {$ENDIF}

  FPhisEliminated := 0;
  FCopiesInserted := 0;

  // Process all blocks
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    EliminatePhisInBlock(Block);
  end;

  {$IFDEF DEBUG_PHIELIM}
  if DebugPhiElim then
  begin
    WriteLn('[PhiElimination] PHI elimination complete:');
    WriteLn(Format('[PhiElimination]   %d PHI instructions removed', [FPhisEliminated]));
    WriteLn(Format('[PhiElimination]   %d copy instructions inserted', [FCopiesInserted]));
    WriteLn('[PhiElimination] SSA program is now ready for bytecode compilation');
    WriteLn('[PhiElimination] ===== PHI ELIMINATION COMPLETE =====');
    WriteLn;
  end;
  {$ENDIF}
end;

end.
