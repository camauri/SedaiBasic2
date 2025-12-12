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
unit SedaiPeephole;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$I OptimizationFlags.inc}
{$I DebugFlags.inc}

{ Peephole optimization pass

  Performs local bytecode optimizations on small windows of instructions.
  Runs AFTER bytecode compilation and BEFORE superinstruction fusion.

  Optimizations performed:

  1. Redundant Copy Elimination
     Pattern: CopyInt R0, R0 (or CopyFloat/CopyString)
     Action: Replace with NOP
     Benefit: Eliminates useless self-copy instructions

  2. Jump Chain Optimization
     Pattern: Jump L1; L1: Jump L2
     Action: Jump L2 (skip intermediate jump)
     Benefit: Reduces jump chain traversal

  3. Dead Jump Elimination
     Pattern: Jump L1; L1: (next instruction)
     Action: Replace with NOP (jump to next instruction is useless)
     Benefit: Eliminates redundant jumps

  4. NOP Compaction (optional, after other passes)
     Pattern: Multiple consecutive NOPs
     Action: Keep only one (or remove all if not jump targets)
     Benefit: Reduces code size

  5. Conditional Jump Simplification
     Pattern: JumpIfZero to next instruction
     Action: Replace with NOP
     Benefit: Eliminates useless conditional jumps

  NOTE: This pass does NOT remove NOPs - that would invalidate jump targets.
  NOP removal is done separately if needed, with jump target adjustment.
}

interface

uses
  Classes, SysUtils, SedaiBytecodeTypes;

type
  TPeepholeOptimizer = class
  private
    FProgram: TBytecodeProgram;
    FOptimizedCount: Integer;
    FJumpTargets: array of Boolean;  // Track which instruction indices are jump targets

    { Build jump target map for the program }
    procedure BuildJumpTargetMap;

    { Check if an instruction index is a jump target }
    function IsJumpTarget(Index: Integer): Boolean;

    { Individual optimization passes }
    function OptimizeRedundantCopy(Index: Integer): Boolean;
    function OptimizeJumpChain(Index: Integer): Boolean;
    function OptimizeDeadJump(Index: Integer): Boolean;
    function OptimizeDeadConditionalJump(Index: Integer): Boolean;

    { Make instruction into NOP }
    procedure MakeNop(Index: Integer);

  public
    constructor Create(AProgram: TBytecodeProgram);
    destructor Destroy; override;

    { Run all peephole optimizations - returns count of optimizations applied }
    function Run: Integer;
  end;

function RunPeephole(AProgram: TBytecodeProgram): Integer;

implementation

{$IFDEF DEBUG_PEEPHOLE}
uses SedaiDebug;
{$ENDIF}

{ TPeepholeOptimizer }

constructor TPeepholeOptimizer.Create(AProgram: TBytecodeProgram);
begin
  inherited Create;
  FProgram := AProgram;
  FOptimizedCount := 0;
end;

destructor TPeepholeOptimizer.Destroy;
begin
  inherited;
end;

procedure TPeepholeOptimizer.BuildJumpTargetMap;
var
  i: Integer;
  Instr: TBytecodeInstruction;
  Target: Integer;
begin
  SetLength(FJumpTargets, FProgram.GetInstructionCount);

  // Initialize all to false
  for i := 0 to High(FJumpTargets) do
    FJumpTargets[i] := False;

  // Scan all instructions and mark jump targets
  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    Instr := FProgram.GetInstruction(i);

    case TBytecodeOp(Instr.OpCode) of
      bcJump, bcJumpIfZero, bcJumpIfNotZero, bcCall:
      begin
        Target := Instr.Immediate;
        if (Target >= 0) and (Target < FProgram.GetInstructionCount) then
          FJumpTargets[Target] := True;
      end;
    end;

    // Also check for superinstruction branch opcodes (100+)
    if Instr.OpCode >= 110 then
    begin
      // Superinstruction branches use Immediate for target
      Target := Instr.Immediate;
      if (Target >= 0) and (Target < FProgram.GetInstructionCount) then
        FJumpTargets[Target] := True;
    end;
  end;
end;

function TPeepholeOptimizer.IsJumpTarget(Index: Integer): Boolean;
begin
  if (Index >= 0) and (Index < Length(FJumpTargets)) then
    Result := FJumpTargets[Index]
  else
    Result := False;
end;

procedure TPeepholeOptimizer.MakeNop(Index: Integer);
var
  NopInstr: TBytecodeInstruction;
begin
  FillChar(NopInstr, SizeOf(NopInstr), 0);
  NopInstr.OpCode := Byte(bcNop);
  FProgram.SetInstruction(Index, NopInstr);
end;

function TPeepholeOptimizer.OptimizeRedundantCopy(Index: Integer): Boolean;
var
  Instr: TBytecodeInstruction;
begin
  Result := False;

  if Index >= FProgram.GetInstructionCount then Exit;

  Instr := FProgram.GetInstruction(Index);

  // Check for self-copy: CopyXxx Rn, Rn
  case TBytecodeOp(Instr.OpCode) of
    bcCopyInt, bcCopyFloat, bcCopyString:
    begin
      if Instr.Dest = Instr.Src1 then
      begin
        {$IFDEF DEBUG_PEEPHOLE}
        if DebugPeephole then
          WriteLn('[Peephole] Removing redundant self-copy at ', Index,
                  ': Copy R', Instr.Dest, ', R', Instr.Src1);
        {$ENDIF}
        MakeNop(Index);
        Result := True;
      end;
    end;
  end;
end;

function TPeepholeOptimizer.OptimizeJumpChain(Index: Integer): Boolean;
var
  Instr, TargetInstr: TBytecodeInstruction;
  Target, FinalTarget: Integer;
  ChainLength: Integer;
begin
  Result := False;

  if Index >= FProgram.GetInstructionCount then Exit;

  Instr := FProgram.GetInstruction(Index);

  // Only optimize unconditional jumps for now
  if TBytecodeOp(Instr.OpCode) <> bcJump then Exit;

  Target := Instr.Immediate;
  if (Target < 0) or (Target >= FProgram.GetInstructionCount) then Exit;

  TargetInstr := FProgram.GetInstruction(Target);

  // If target is also a jump, follow the chain
  if TBytecodeOp(TargetInstr.OpCode) = bcJump then
  begin
    FinalTarget := Target;
    ChainLength := 0;

    // Follow jump chain (with limit to avoid infinite loops)
    while (ChainLength < 10) and
          (FinalTarget >= 0) and
          (FinalTarget < FProgram.GetInstructionCount) do
    begin
      TargetInstr := FProgram.GetInstruction(FinalTarget);
      if TBytecodeOp(TargetInstr.OpCode) <> bcJump then
        Break;

      FinalTarget := TargetInstr.Immediate;
      Inc(ChainLength);
    end;

    // If we found a shorter path, update the jump
    if (FinalTarget <> Target) and
       (FinalTarget >= 0) and
       (FinalTarget < FProgram.GetInstructionCount) then
    begin
      {$IFDEF DEBUG_PEEPHOLE}
      if DebugPeephole then
        WriteLn('[Peephole] Shortening jump chain at ', Index,
                ': Jump ', Target, ' → Jump ', FinalTarget,
                ' (skipped ', ChainLength, ' intermediate jumps)');
      {$ENDIF}
      Instr.Immediate := FinalTarget;
      FProgram.SetInstruction(Index, Instr);
      Result := True;
    end;
  end;
end;

function TPeepholeOptimizer.OptimizeDeadJump(Index: Integer): Boolean;
var
  Instr: TBytecodeInstruction;
begin
  Result := False;

  if Index >= FProgram.GetInstructionCount - 1 then Exit;  // Need at least one more instruction

  Instr := FProgram.GetInstruction(Index);

  // Check for jump to next instruction
  if TBytecodeOp(Instr.OpCode) = bcJump then
  begin
    if Instr.Immediate = Index + 1 then
    begin
      {$IFDEF DEBUG_PEEPHOLE}
      if DebugPeephole then
        WriteLn('[Peephole] Removing dead jump to next instruction at ', Index);
      {$ENDIF}
      MakeNop(Index);
      Result := True;
    end;
  end;
end;

function TPeepholeOptimizer.OptimizeDeadConditionalJump(Index: Integer): Boolean;
var
  Instr: TBytecodeInstruction;
begin
  Result := False;

  if Index >= FProgram.GetInstructionCount - 1 then Exit;

  Instr := FProgram.GetInstruction(Index);

  // Check for conditional jump to next instruction
  case TBytecodeOp(Instr.OpCode) of
    bcJumpIfZero, bcJumpIfNotZero:
    begin
      if Instr.Immediate = Index + 1 then
      begin
        {$IFDEF DEBUG_PEEPHOLE}
        if DebugPeephole then
          WriteLn('[Peephole] Removing dead conditional jump to next instruction at ', Index);
        {$ENDIF}
        MakeNop(Index);
        Result := True;
      end;
    end;
  end;
end;

function TPeepholeOptimizer.Run: Integer;
var
  i: Integer;
  Changed: Boolean;
  Pass: Integer;
begin
  {$IFDEF DISABLE_PEEPHOLE}
  {$IFDEF DEBUG_PEEPHOLE}
  if DebugPeephole then
    WriteLn('[Peephole] SKIPPED (disabled by flag)');
  {$ENDIF}
  Result := 0;
  Exit;
  {$ENDIF}

  {$IFDEF DEBUG_PEEPHOLE}
  if DebugPeephole then
    WriteLn('[Peephole] Running peephole optimization...');
  {$ENDIF}

  FOptimizedCount := 0;
  Pass := 0;

  // Run multiple passes until no more optimizations can be made
  repeat
    Changed := False;
    Inc(Pass);

    // Rebuild jump target map at start of each pass
    BuildJumpTargetMap;

    // Scan all instructions
    for i := 0 to FProgram.GetInstructionCount - 1 do
    begin
      // Try each optimization in order
      if OptimizeRedundantCopy(i) then
      begin
        Changed := True;
        Inc(FOptimizedCount);
      end
      else if OptimizeJumpChain(i) then
      begin
        Changed := True;
        Inc(FOptimizedCount);
      end
      else if OptimizeDeadJump(i) then
      begin
        Changed := True;
        Inc(FOptimizedCount);
      end
      else if OptimizeDeadConditionalJump(i) then
      begin
        Changed := True;
        Inc(FOptimizedCount);
      end;
    end;

    {$IFDEF DEBUG_PEEPHOLE}
    if Changed and DebugPeephole then
      WriteLn('[Peephole] Pass ', Pass, ': ', FOptimizedCount, ' total optimizations');
    {$ENDIF}

  until (not Changed) or (Pass >= 5);  // Limit passes to avoid infinite loops

  {$IFDEF DEBUG_PEEPHOLE}
  if DebugPeephole then
    WriteLn('[Peephole] Completed: ', FOptimizedCount, ' optimizations in ', Pass, ' pass(es)');
  {$ENDIF}
  Result := FOptimizedCount;
end;

function RunPeephole(AProgram: TBytecodeProgram): Integer;
var
  Optimizer: TPeepholeOptimizer;
begin
  Optimizer := TPeepholeOptimizer.Create(AProgram);
  try
    Result := Optimizer.Run;
  finally
    Optimizer.Free;
  end;
end;

end.
