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
  Unit: SedaiConstPropAggressive

  Purpose: SSA-aware constant propagation using version tracking.

  With Semi-Pruned SSA (versioning enabled):
  - Each register version is defined ONCE: R0_1, R0_2, R0_3, etc.
  - No register reuse across blocks - every version is unique
  - Constant tracking is GLOBAL and SAFE (no reuse issues!)

  Algorithm:
  1. Scan all LoadConst instructions, track (RegType:RegIndex:Version) → Constant
  2. Replace all uses of versioned registers with their constant values
  3. Simple, fast, correct - no complex liveness or dominance checks needed!

  Author: Sedai Project - SSA-Aware Optimization
  Date: 2025-01-26
  ============================================================================ }

unit SedaiConstPropAggressive;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$I OptimizationFlags.inc}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, SedaiSSATypes;

type
  { SSA-aware aggressive constant propagation }
  TAggressiveConstProp = class
  private
    FProgram: TSSAProgram;
    FPropagations: Integer;
    FReplacements: Integer;

    { Track versioned register constants: "RegType:RegIndex:Version" → Constant Value }
    FVersionedConstants: TStringList;  // Key = "type:idx:ver", Object = TSSAValue (as pointer)

    procedure CollectVersionedConstants;
    procedure PropagateConstants;
    function GetVersionedKey(const Val: TSSAValue): string;
    function IsConstant(const Val: TSSAValue; out ConstVal: TSSAValue): Boolean;

  public
    constructor Create(Prog: TSSAProgram);
    destructor Destroy; override;

    function Run(Level: Integer): Integer;
  end;

implementation

uses SedaiDominators
     {$IFDEF DEBUG_CONSTPROP}, SedaiDebug{$ENDIF};

{ TAggressiveConstProp }

constructor TAggressiveConstProp.Create(Prog: TSSAProgram);
begin
  inherited Create;
  FProgram := Prog;
  FPropagations := 0;
  FReplacements := 0;
  FVersionedConstants := TStringList.Create;
  FVersionedConstants.Sorted := True;
  FVersionedConstants.Duplicates := dupIgnore;
end;

destructor TAggressiveConstProp.Destroy;
var
  i: Integer;
  ConstPtr: ^TSSAValue;
begin
  // Free allocated TSSAValue pointers
  for i := 0 to FVersionedConstants.Count - 1 do
    if Assigned(FVersionedConstants.Objects[i]) then
    begin
      ConstPtr := Pointer(FVersionedConstants.Objects[i]);
      Dispose(ConstPtr);
    end;
  FVersionedConstants.Free;
  inherited;
end;

function TAggressiveConstProp.GetVersionedKey(const Val: TSSAValue): string;
begin
  if Val.Kind <> svkRegister then Exit('');
  Result := Format('%d:%d:%d', [Ord(Val.RegType), Val.RegIndex, Val.Version]);
end;

procedure TAggressiveConstProp.CollectVersionedConstants;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  i, j: Integer;
  RegKey: string;
  ConstPtr: ^TSSAValue;
begin
  { With SSA versioning, each LoadConst defines a UNIQUE register version.
    Track all LoadConst instructions globally - no reuse issues! }

  {$IFDEF DEBUG_CONSTPROP}
  if DebugConstProp then
    WriteLn('[AggressiveCP] Collecting versioned constants from LoadConst instructions...');
  {$ENDIF}

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      // Track LoadConst instructions: R_ver = Const
      if (Instr.OpCode in [ssaLoadConstInt, ssaLoadConstFloat, ssaLoadConstString]) and
         (Instr.Dest.Kind = svkRegister) and
         (Instr.Dest.Version > 0) then  // Only versioned registers (SSA form)
      begin
        RegKey := GetVersionedKey(Instr.Dest);
        if RegKey = '' then Continue;

        // Allocate TSSAValue on heap to store constant
        New(ConstPtr);
        ConstPtr^ := Instr.Src1;  // Store the constant value

        FVersionedConstants.AddObject(RegKey, TObject(ConstPtr));
      end;
    end;
  end;

  {$IFDEF DEBUG_CONSTPROP}
  if DebugConstProp then
    WriteLn('[AggressiveCP]   Found ', FVersionedConstants.Count, ' versioned constants');
  {$ENDIF}
end;

function TAggressiveConstProp.IsConstant(const Val: TSSAValue; out ConstVal: TSSAValue): Boolean;
var
  RegKey: string;
  Idx: Integer;
  ConstPtr: ^TSSAValue;
begin
  Result := False;

  if Val.Kind <> svkRegister then Exit;
  if Val.Version = 0 then Exit;  // Unversioned - skip (not SSA form)

  RegKey := GetVersionedKey(Val);
  Idx := FVersionedConstants.IndexOf(RegKey);

  if Idx >= 0 then
  begin
    ConstPtr := Pointer(FVersionedConstants.Objects[Idx]);
    if Assigned(ConstPtr) then
    begin
      ConstVal := ConstPtr^;
      Result := True;
    end;
  end;
end;

procedure TAggressiveConstProp.PropagateConstants;
var
  Block: TSSABasicBlock;
  Instr, NewInstr: TSSAInstruction;
  i, j: Integer;
  ConstVal: TSSAValue;
  Modified: Boolean;
begin
  {$IFDEF DEBUG_CONSTPROP}
  if DebugConstProp then
    WriteLn('[AggressiveCP] Propagating versioned constants...');
  {$ENDIF}

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];
      Modified := False;

      // CRITICAL: Only LoadConst instructions support immediate constants!
      // All other instructions (binary ops, print, etc.) require registers.
      // The bytecode compiler puts constants in Immediate field, but most VM
      // instructions read from registers (FIntRegs[Src1], not Immediate).
      //
      // Therefore: DO NOT PROPAGATE CONSTANTS except in LoadConst destinations!
      // This would break the VM's register-based architecture.

      // Skip LoadConst - they define constants, we don't modify them
      if Instr.OpCode in [ssaLoadConstInt, ssaLoadConstFloat, ssaLoadConstString] then
        Continue;

      // DISABLED: Constant propagation into operands is UNSAFE!
      // The VM requires all operands to be in registers.
      // Propagating immediate constants breaks binary ops, print, and most instructions.
      {
      NewInstr := Instr.Clone;

      if IsConstant(Instr.Src1, ConstVal) then
      begin
        NewInstr.Src1 := ConstVal;
        Modified := True;
        Inc(FPropagations);
      end;

      if IsConstant(Instr.Src2, ConstVal) then
      begin
        NewInstr.Src2 := ConstVal;
        Modified := True;
        Inc(FPropagations);
      end;

      if IsConstant(Instr.Src3, ConstVal) then
      begin
        NewInstr.Src3 := ConstVal;
        Modified := True;
        Inc(FPropagations);
      end;
      }

      if Modified then
      begin
        Block.Instructions[j] := NewInstr;
        // Note: TFPGObjectList automatically frees the old instruction
        Inc(FReplacements);
      end
      else
        NewInstr.Free;
    end;
  end;

  {$IFDEF DEBUG_CONSTPROP}
  if DebugConstProp then
  begin
    WriteLn('[AggressiveCP]   Propagated ', FPropagations, ' constants');
    WriteLn('[AggressiveCP]   Replaced ', FReplacements, ' instructions');
  end;
  {$ENDIF}
end;

function TAggressiveConstProp.Run(Level: Integer): Integer;
begin
  {$IFDEF DEBUG_CONSTPROP}
  if DebugConstProp then
  begin
    WriteLn('[AggressiveCP] ===== SSA-AWARE CONSTANT PROPAGATION =====');
    WriteLn('[AggressiveCP] Using Semi-Pruned SSA versioning (unique register versions)');
    WriteLn('[AggressiveCP] Level: ', Level, ' (unused - SSA makes this trivial)');
    WriteLn;
  end;
  {$ENDIF}

  FPropagations := 0;
  FReplacements := 0;

  CollectVersionedConstants;

  if FVersionedConstants.Count = 0 then
  begin
    {$IFDEF DEBUG_CONSTPROP}
    if DebugConstProp then
      WriteLn('[AggressiveCP] No versioned constants found - SSA construction may not have run');
    {$ENDIF}
    Result := 0;
    Exit;
  end;

  PropagateConstants;

  {$IFDEF DEBUG_CONSTPROP}
  if DebugConstProp then
  begin
    WriteLn('[AggressiveCP] ===== PROPAGATION COMPLETE =====');
    WriteLn;
  end;
  {$ENDIF}

  Result := FReplacements;
end;

end.
