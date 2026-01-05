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
  Unit: SedaiConstProp

  Purpose: Simple constant propagation for SSA IR - identifies BASIC variables
           that are assigned constant values once and never modified, then
           propagates those values to eliminate redundant loads and enable folding.

  Algorithm: Two-phase variable-based propagation
             Phase 1: Identify semantic constants
               - Scan all blocks for StoreVar operations
               - Count assignments per BASIC variable
               - Track last constant value assigned
               - Mark variables assigned exactly once with constant value

             Phase 2: Propagate and fold
               - Replace LoadVar(const_var) with LoadConst(value)
               - Map result registers to constant values
               - Substitute register operands with constants
               - Fold binary operations with constant operands
               - Track folded results for cascading propagation

  Examples: PI = 3.14159, SOLAR_MASS = 39.478, N% = 1000, DT = 0.01

  Limitations: Only handles variables with single assignment
               Does not handle:
               - Variables modified in loops (multiple assignments)
               - Variables modified in conditionals
               - Complex expressions (function results, array elements)

  Phase: Simple Optimization Pass (post-GVN, pre-DCE)
  Author: Sedai Project - Code Generation Improvements
  Date: 2025-01-25
  ============================================================================ }

unit SedaiConstProp;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, Generics.Collections, SedaiSSATypes;

type
  { TConstantInfo - Information about a propagatable constant }
  TConstantValueType = (cvtInt, cvtFloat, cvtString);

  TConstantInfo = record
    IsConstant: Boolean;      // True if this variable is a propagatable constant
    ValueType: TConstantValueType;  // Type of the constant value
    IntValue: Int64;          // Integer value (if ValueType = cvtInt)
    FloatValue: Double;       // Float value (if ValueType = cvtFloat)
    StringValue: string;      // String value (if ValueType = cvtString)
  end;

  { TSimpleConstProp - Simple constant propagation pass }
  TSimpleConstProp = class
  private
    FProgram: TSSAProgram;
    FConstants: specialize TDictionary<string, TConstantInfo>;  // Variable name → constant info
    FRegisterMap: specialize TDictionary<Integer, TConstantInfo>; // Register → constant value
    FPropagations: Integer;  // Count of propagated values

    { Scan entry blocks to identify single-assignment constants }
    procedure IdentifyConstants;

    { Propagate constants through instructions in all blocks }
    procedure PropagateConstants;

    { Replace register uses with constant values in an instruction }
    procedure PropagateInInstruction(Block: TSSABasicBlock; InstrIdx: Integer);

    { Try to fold a binary operation with constant operands }
    function TryFoldBinaryOp(OpCode: TSSAOpCode; const Left, Right: TConstantInfo;
                             out ResultInfo: TConstantInfo): Boolean;

  public
    constructor Create(Prog: TSSAProgram);
    destructor Destroy; override;

    { Run constant propagation pass - returns number of propagations }
    function Run: Integer;
  end;

implementation

{$IFDEF DEBUG_CONSTPROP}
uses SedaiDebug;
{$ENDIF}

{ TSimpleConstProp }

constructor TSimpleConstProp.Create(Prog: TSSAProgram);
begin
  inherited Create;
  FProgram := Prog;
  FConstants := specialize TDictionary<string, TConstantInfo>.Create;
  FRegisterMap := specialize TDictionary<Integer, TConstantInfo>.Create;
  FPropagations := 0;
end;

destructor TSimpleConstProp.Destroy;
begin
  FConstants.Free;
  FRegisterMap.Free;
  inherited;
end;

function TSimpleConstProp.Run: Integer;
begin
  {$IFDEF DEBUG_CONSTPROP}
  if DebugConstProp then
    WriteLn('[ConstProp] Running simple constant propagation...');
  {$ENDIF}

  // Step 1: Identify single-assignment constant registers
  IdentifyConstants;

  // Step 2: Propagate constants through instructions
  if FRegisterMap.Count > 0 then
  begin
    PropagateConstants;
    {$IFDEF DEBUG_CONSTPROP}
    if DebugConstProp then
      WriteLn('[ConstProp] Propagated ', FPropagations, ' constant values');
    {$ENDIF}
  end;

  Result := FPropagations;
end;

procedure TSimpleConstProp.IdentifyConstants;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  Info: TConstantInfo;
  i, j: Integer;
  VarName: string;
  VarAssignCount: specialize TDictionary<string, Integer>;
  LastVarValue: specialize TDictionary<string, TConstantInfo>;
begin
  // Identify BASIC variables that are assigned exactly once with a constant value
  // These are semantic constants like: PI = 3.14159, N% = 1000, etc.
  // NOTE: We do NOT track computed constants (e.g., SOLAR_MASS = 4 * PI * PI)
  // because that would require tracking register values, which is unsafe due to register reuse.

  VarAssignCount := specialize TDictionary<string, Integer>.Create;
  LastVarValue := specialize TDictionary<string, TConstantInfo>.Create;
  try
    // Pass 1: Count assignments to each variable and track constant values
    // We only track IMMEDIATE constant assignments, not register-based ones
    for i := 0 to FProgram.Blocks.Count - 1 do
    begin
      Block := FProgram.Blocks[i];
      for j := 0 to Block.Instructions.Count - 1 do
      begin
        Instr := Block.Instructions[j];

        // Handle StoreVar instructions
        if Instr.OpCode = ssaStoreVar then
        begin
          if Instr.Dest.Kind = svkVariable then
          begin
            VarName := Instr.Dest.VarName;

            // Count assignment
            if VarAssignCount.ContainsKey(VarName) then
              VarAssignCount[VarName] := VarAssignCount[VarName] + 1
            else
              VarAssignCount.Add(VarName, 1);

            // Track the value if it's a constant (immediate or from constant register)
            if Instr.Src1.Kind = svkConstInt then
            begin
              Info.IsConstant := True;
              Info.ValueType := cvtInt;
              Info.IntValue := Instr.Src1.ConstInt;
              Info.FloatValue := 0.0;
              Info.StringValue := '';
              LastVarValue.AddOrSetValue(VarName, Info);
            end
            else if Instr.Src1.Kind = svkConstFloat then
            begin
              Info.IsConstant := True;
              Info.ValueType := cvtFloat;
              Info.IntValue := 0;
              Info.FloatValue := Instr.Src1.ConstFloat;
              Info.StringValue := '';
              LastVarValue.AddOrSetValue(VarName, Info);
            end
            else if Instr.Src1.Kind = svkConstString then
            begin
              Info.IsConstant := True;
              Info.ValueType := cvtString;
              Info.IntValue := 0;
              Info.FloatValue := 0.0;
              Info.StringValue := Instr.Src1.ConstString;
              LastVarValue.AddOrSetValue(VarName, Info);
            end
            // REMOVED: Don't propagate from registers in RegMap!
            // RegMap is built by scanning all blocks linearly, so if a register
            // is reused (e.g., R5 = 55, then later R5 = 7.28), RegMap will contain
            // only the LAST value, leading to incorrect propagation.
            // We only propagate variables assigned with IMMEDIATE constants.
            {
            else if (Instr.Src1.Kind = svkRegister) and RegMap.TryGetValue(Instr.Src1.RegIndex, Info) then
            begin
              // Register holds a constant value - propagate it
              LastVarValue.AddOrSetValue(VarName, Info);
            end
            }
            else
            begin
              // Not a constant value - remove if present
              if LastVarValue.ContainsKey(VarName) then
                LastVarValue.Remove(VarName);
            end;
          end;
        end;
      end;
    end;

    // Pass 2: Keep only variables assigned exactly once with constant value
    for VarName in VarAssignCount.Keys do
    begin
      if (VarAssignCount[VarName] = 1) and LastVarValue.ContainsKey(VarName) then
      begin
        FConstants.Add(VarName, LastVarValue[VarName]);
      end;
    end;

    {$IFDEF DEBUG_CONSTPROP}
    if DebugConstProp then
      WriteLn(Format('[ConstProp] Found %d single-assignment constant variables', [FConstants.Count]));
    {$ENDIF}

  finally
    VarAssignCount.Free;
    LastVarValue.Free;
  end;
end;

procedure TSimpleConstProp.PropagateConstants;
var
  Block: TSSABasicBlock;
  i, j: Integer;
begin
  // Process all blocks
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];

    // Process each instruction
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      PropagateInInstruction(Block, j);
    end;
  end;
end;

procedure TSimpleConstProp.PropagateInInstruction(Block: TSSABasicBlock; InstrIdx: Integer);
var
  Instr: TSSAInstruction;
  Info, LeftInfo, RightInfo, ResultInfo: TConstantInfo;
  NewInstr: TSSAInstruction;
  HasLeft, HasRight: Boolean;
  VarName: string;
begin
  Instr := Block.Instructions[InstrIdx];

  // Step 1: Replace LoadVar with LoadConst for constant variables
  // This transforms:  %r1 = LoadVar PI  →  %r1 = LoadConstFloat 3.14159
  if Instr.OpCode = ssaLoadVar then
  begin
    if Instr.Src1.Kind = svkVariable then
    begin
      VarName := Instr.Src1.VarName;
      if FConstants.TryGetValue(VarName, Info) then
      begin
        // Replace LoadVar with LoadConst
        NewInstr := Instr.Clone;
        case Info.ValueType of
          cvtInt:
          begin
            NewInstr.OpCode := ssaLoadConstInt;
            NewInstr.Src1 := MakeSSAConstInt(Info.IntValue);
          end;
          cvtFloat:
          begin
            NewInstr.OpCode := ssaLoadConstFloat;
            NewInstr.Src1 := MakeSSAConstFloat(Info.FloatValue);
          end;
          cvtString:
          begin
            NewInstr.OpCode := ssaLoadConstString;
            NewInstr.Src1 := MakeSSAConstString(Info.StringValue);
          end;
        end;

        Block.Instructions[InstrIdx] := NewInstr;
        Instr := NewInstr;
        Inc(FPropagations);

        // Do NOT track the register in FRegisterMap here!
        // The register may be reused later with a different value,
        // and FRegisterMap is global across all blocks.
        // We only trust registers populated in IdentifyConstants phase.
      end;
    end;
  end;

  // Step 2: Substitute register operands with constant values
  // DISABLED: FRegisterMap tracking is unsafe due to register reuse
  // Registers can be reassigned, so a global map leads to incorrect propagation
  // TODO: Implement per-block or per-instruction constant tracking for registers
  {
  NewInstr := Instr.Clone;
  if (Instr.Src1.Kind = svkRegister) and FRegisterMap.TryGetValue(Instr.Src1.RegIndex, Info) then
  begin
    case Info.ValueType of
      cvtInt: NewInstr.Src1 := MakeSSAConstInt(Info.IntValue);
      cvtFloat: NewInstr.Src1 := MakeSSAConstFloat(Info.FloatValue);
      cvtString: NewInstr.Src1 := MakeSSAConstString(Info.StringValue);
    end;
    Block.Instructions[InstrIdx] := NewInstr;
    Instr := NewInstr;
    Inc(FPropagations);
  end;

  if (Instr.Src2.Kind = svkRegister) and FRegisterMap.TryGetValue(Instr.Src2.RegIndex, Info) then
  begin
    case Info.ValueType of
      cvtInt: NewInstr.Src2 := MakeSSAConstInt(Info.IntValue);
      cvtFloat: NewInstr.Src2 := MakeSSAConstFloat(Info.FloatValue);
      cvtString: NewInstr.Src2 := MakeSSAConstString(Info.StringValue);
    end;
    Block.Instructions[InstrIdx] := NewInstr;
    Instr := NewInstr;
    Inc(FPropagations);
  end;

  if (Instr.Src3.Kind = svkRegister) and FRegisterMap.TryGetValue(Instr.Src3.RegIndex, Info) then
  begin
    case Info.ValueType of
      cvtInt: NewInstr.Src3 := MakeSSAConstInt(Info.IntValue);
      cvtFloat: NewInstr.Src3 := MakeSSAConstFloat(Info.FloatValue);
      cvtString: NewInstr.Src3 := MakeSSAConstString(Info.StringValue);
    end;
    Block.Instructions[InstrIdx] := NewInstr;
    Instr := NewInstr;
    Inc(FPropagations);
  end;
  }

  // Step 3: Try to fold binary operations with constant operands
  // This transforms:  %r3 = AddFloat 3.14159, 365.24  →  %r3 = LoadConstFloat 368.38159
  if Instr.OpCode in [ssaAddInt, ssaSubInt, ssaMulInt, ssaDivInt,
                      ssaAddFloat, ssaSubFloat, ssaMulFloat, ssaDivFloat] then
  begin
    HasLeft := False;
    HasRight := False;

    // Get left operand constant info
    if Instr.Src1.Kind = svkConstInt then
    begin
      LeftInfo.IsConstant := True;
      LeftInfo.ValueType := cvtInt;
      LeftInfo.IntValue := Instr.Src1.ConstInt;
      LeftInfo.FloatValue := 0.0;
      LeftInfo.StringValue := '';
      HasLeft := True;
    end
    else if Instr.Src1.Kind = svkConstFloat then
    begin
      LeftInfo.IsConstant := True;
      LeftInfo.ValueType := cvtFloat;
      LeftInfo.IntValue := 0;
      LeftInfo.FloatValue := Instr.Src1.ConstFloat;
      LeftInfo.StringValue := '';
      HasLeft := True;
    end;

    // Get right operand constant info
    if Instr.Src2.Kind = svkConstInt then
    begin
      RightInfo.IsConstant := True;
      RightInfo.ValueType := cvtInt;
      RightInfo.IntValue := Instr.Src2.ConstInt;
      RightInfo.FloatValue := 0.0;
      RightInfo.StringValue := '';
      HasRight := True;
    end
    else if Instr.Src2.Kind = svkConstFloat then
    begin
      RightInfo.IsConstant := True;
      RightInfo.ValueType := cvtFloat;
      RightInfo.IntValue := 0;
      RightInfo.FloatValue := Instr.Src2.ConstFloat;
      RightInfo.StringValue := '';
      HasRight := True;
    end;

    // If both operands are constants, try to fold
    if HasLeft and HasRight then
    begin
      if TryFoldBinaryOp(Instr.OpCode, LeftInfo, RightInfo, ResultInfo) then
      begin
        // Replace binary op with LoadConst
        NewInstr := Instr.Clone;
        case ResultInfo.ValueType of
          cvtInt:
          begin
            NewInstr.OpCode := ssaLoadConstInt;
            NewInstr.Src1 := MakeSSAConstInt(ResultInfo.IntValue);
          end;
          cvtFloat:
          begin
            NewInstr.OpCode := ssaLoadConstFloat;
            NewInstr.Src1 := MakeSSAConstFloat(ResultInfo.FloatValue);
          end;
          cvtString:
          begin
            NewInstr.OpCode := ssaLoadConstString;
            NewInstr.Src1 := MakeSSAConstString(ResultInfo.StringValue);
          end;
        end;
        NewInstr.Src2 := MakeSSAValue(svkNone);
        NewInstr.Src3 := MakeSSAValue(svkNone);

        Block.Instructions[InstrIdx] := NewInstr;
        Inc(FPropagations);

        // Do NOT track the folded result in FRegisterMap here!
        // The register may be reused later, and FRegisterMap is global.
        // We only trust registers from the initial IdentifyConstants phase.
      end;
    end;
  end;
end;

function TSimpleConstProp.TryFoldBinaryOp(OpCode: TSSAOpCode;
  const Left, Right: TConstantInfo; out ResultInfo: TConstantInfo): Boolean;
var
  LeftFloat, RightFloat: Double;
begin
  Result := False;
  ResultInfo.IsConstant := False;

  // Integer operations
  if (Left.ValueType = cvtInt) and (Right.ValueType = cvtInt) then
  begin
    ResultInfo.ValueType := cvtInt;
    ResultInfo.IsConstant := True;
    ResultInfo.FloatValue := 0.0;
    ResultInfo.StringValue := '';

    case OpCode of
      ssaAddInt: ResultInfo.IntValue := Left.IntValue + Right.IntValue;
      ssaSubInt: ResultInfo.IntValue := Left.IntValue - Right.IntValue;
      ssaMulInt: ResultInfo.IntValue := Left.IntValue * Right.IntValue;
      ssaDivInt:
      begin
        if Right.IntValue = 0 then Exit(False);
        ResultInfo.IntValue := Left.IntValue div Right.IntValue;
      end;
      else Exit(False);
    end;

    Result := True;
  end

  // Float operations (convert ints to float if needed)
  else if (Left.ValueType in [cvtInt, cvtFloat]) and (Right.ValueType in [cvtInt, cvtFloat]) then
  begin
    ResultInfo.ValueType := cvtFloat;
    ResultInfo.IsConstant := True;
    ResultInfo.IntValue := 0;
    ResultInfo.StringValue := '';

    // Get left operand as float
    if Left.ValueType = cvtInt then
      LeftFloat := Left.IntValue
    else
      LeftFloat := Left.FloatValue;

    // Get right operand as float
    if Right.ValueType = cvtInt then
      RightFloat := Right.IntValue
    else
      RightFloat := Right.FloatValue;

    case OpCode of
      ssaAddFloat:
        ResultInfo.FloatValue := LeftFloat + RightFloat;
      ssaSubFloat:
        ResultInfo.FloatValue := LeftFloat - RightFloat;
      ssaMulFloat:
        ResultInfo.FloatValue := LeftFloat * RightFloat;
      ssaDivFloat:
      begin
        if RightFloat = 0.0 then Exit(False);
        ResultInfo.FloatValue := LeftFloat / RightFloat;
      end;
      else Exit(False);
    end;

    Result := True;
  end
  else
    Result := False;  // String operations not supported in folding yet
end;

end.
