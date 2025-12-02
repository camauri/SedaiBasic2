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
  Unit: SedaiCopyProp (Copy Propagation)

  Purpose: Eliminate redundant copy operations by propagating the source
           through all uses of the destination.

  Algorithm: Forward dataflow analysis
             1. Build def-use chains for all registers
             2. Identify copy instructions: %dest = %src (no computation)
             3. Replace all uses of %dest with %src
             4. Mark copy instruction as dead (DCE will remove it)

  Examples:
    Before:               After:
    %r1 = LoadVar X       %r1 = LoadVar X
    %r2 = Copy %r1        %r2 = Copy %r1 (dead - will be removed by DCE)
    %r3 = Add %r2, %r4    %r3 = Add %r1, %r4

  What is eliminated:
    - ssaCopyInt/Float/String instructions
    - Redundant register-to-register moves
    - Temporary register allocations

  What is preserved:
    - First definition (source of copy)
    - All other instructions
    - SSA form integrity

  Phase: Early optimization (post-SSA, before DCE)
  Author: Sedai Project - Optimization Pipeline
  Date: 2025-01-26
  ============================================================================ }

unit SedaiCopyProp;

{$mode objfpc}{$H+}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, contnrs, SedaiSSATypes;

type
  { TCopyPropagation - Eliminate redundant copies }
  TCopyPropagation = class
  private
    FProgram: TSSAProgram;
    FReplacements: Integer;

    { Try to replace a register operand with its copy source }
    function TryReplaceCopy(const RegVal: TSSAValue; CurrBlock: TSSABasicBlock;
      CurrInstrIdx: Integer; out Replaced: TSSAValue): Boolean;

    { Propagate copies through all instructions }
    procedure PropagateCopies;

  public
    constructor Create(Prog: TSSAProgram);
    destructor Destroy; override;

    { Run copy propagation pass - returns number of replacements }
    function Run: Integer;
  end;

implementation

{$IFDEF DEBUG_COPYPROP}
uses SedaiDebug;
{$ENDIF}

{ TCopyPropagation }

constructor TCopyPropagation.Create(Prog: TSSAProgram);
begin
  inherited Create;
  FProgram := Prog;
  FReplacements := 0;
end;

destructor TCopyPropagation.Destroy;
begin
  inherited;
end;

function TCopyPropagation.Run: Integer;
begin
  {$IFDEF DEBUG_COPYPROP}
  if DebugCopyProp then
    WriteLn('[CopyProp] Running copy propagation...');
  {$ENDIF}

  // Propagate copies through instructions
  PropagateCopies;

  {$IFDEF DEBUG_COPYPROP}
  if DebugCopyProp then
    WriteLn('[CopyProp] Made ', FReplacements, ' copy propagations');
  {$ENDIF}
  Result := FReplacements;
end;

function TCopyPropagation.TryReplaceCopy(const RegVal: TSSAValue;
  CurrBlock: TSSABasicBlock; CurrInstrIdx: Integer; out Replaced: TSSAValue): Boolean;
var
  Instr: TSSAInstruction;
  j: Integer;
begin
  // Conservative approach: only look BACKWARDS in the current block
  // This ensures we only propagate copies that are defined before use
  // and avoids SSA dominance violations

  Result := False;

  if RegVal.Kind <> svkRegister then
    Exit;

  // Search backwards in current block only
  for j := CurrInstrIdx - 1 downto 0 do
  begin
    Instr := CurrBlock.Instructions[j];

    // Check if this instruction defines our register
    if (Instr.Dest.Kind = svkRegister) and
       (Instr.Dest.RegIndex = RegVal.RegIndex) and
       (Instr.Dest.Version = RegVal.Version) then
    begin
      // Found the definition - check if it's a copy
      if Instr.OpCode in [ssaCopyInt, ssaCopyFloat, ssaCopyString] then
      begin
        if Instr.Src1.Kind = svkRegister then
        begin
          Replaced := Instr.Src1;
          Exit(True);
        end;
      end;

      // Not a copy, stop searching
      Exit(False);
    end;
  end;
end;

procedure TCopyPropagation.PropagateCopies;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  NewInstr: TSSAInstruction;
  i, j: Integer;
  ReplacedVal: TSSAValue;
  Changed: Boolean;
begin
  // Simple single-pass propagation
  // For each instruction, try to replace register operands with copy sources

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];
      Changed := False;
      NewInstr := Instr.Clone;

      // Try to replace Src1
      if TryReplaceCopy(Instr.Src1, Block, j, ReplacedVal) then
      begin
        NewInstr.Src1 := ReplacedVal;
        Changed := True;
        Inc(FReplacements);
      end;

      // Try to replace Src2
      if TryReplaceCopy(Instr.Src2, Block, j, ReplacedVal) then
      begin
        NewInstr.Src2 := ReplacedVal;
        Changed := True;
        Inc(FReplacements);
      end;

      // Try to replace Src3
      if TryReplaceCopy(Instr.Src3, Block, j, ReplacedVal) then
      begin
        NewInstr.Src3 := ReplacedVal;
        Changed := True;
        Inc(FReplacements);
      end;

      // Update instruction if any source was changed
      if Changed then
        Block.Instructions[j] := NewInstr;
    end;
  end;
end;

end.
