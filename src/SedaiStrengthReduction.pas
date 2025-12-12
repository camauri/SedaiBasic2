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
  Unit: SedaiStrengthReduction (Strength Reduction)

  Purpose: Replace expensive operations with cheaper equivalents that produce
           the same result.

  Algorithm: Pattern matching on arithmetic operations
             1. Identify expensive operations (multiply, divide, power)
             2. Check if they can be replaced with cheaper alternatives
             3. Apply transformations

  Examples:
    Before:                After:
    %r1 = x * 2            %r1 = x + x
    %r2 = x * 4            %r2 = x << 2
    %r3 = x * 8            %r3 = x << 3
    %r4 = x / 2            %r4 = x >> 1
    %r5 = x / 4            %r5 = x >> 2
    %r6 = x ^ 2            %r6 = x * x

  Strength Reduction Rules:
    Multiplication by power of 2 → Left shift
      x * 2 → x << 1
      x * 4 → x << 2
      x * 8 → x << 3
      etc.

    Division by power of 2 → Right shift (for unsigned)
      x / 2 → x >> 1
      x / 4 → x >> 2
      etc.

    Multiplication by small constant → Addition
      x * 2 → x + x
      x * 3 → x + x + x (or (x << 1) + x)

    Power by small constant → Multiplication
      x ^ 2 → x * x
      x ^ 3 → x * x * x

  Note: Shift operations are only valid for integers.
        For floats, we use addition-based strength reduction only.

  Phase: Early optimization (post-algebraic, before CSE)
  Author: Sedai Project - Optimization Pipeline
  Date: 2025-01-25
  ============================================================================ }

unit SedaiStrengthReduction;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, Contnrs, SedaiSSATypes, SedaiDominators;

type
  { TInductionVariable - Basic induction variable info }
  TInductionVariable = record
    VarRegIndex: Integer;     // Register index of the IV
    VarRegType: TSSARegisterType;
    VarVersion: Integer;      // SSA version (0 for BASIC global semantics)
    InitValue: TSSAValue;     // Initial value (outside loop)
    StepValue: TSSAValue;     // Step constant (e.g., 1 for i = i + 1)
    StepIsAdd: Boolean;       // True for addition, False for subtraction
    HeaderBlock: TSSABasicBlock; // Loop header
    UpdateInstr: TSSAInstruction; // The i = i + step instruction
  end;

  { TLoopInfoSR - Loop information for strength reduction }
  TLoopInfoSR = class
    Header: TSSABasicBlock;
    Blocks: TFPList;  // List of TSSABasicBlock
    BackEdgeSources: TFPList;
    constructor Create(AHeader: TSSABasicBlock);
    destructor Destroy; override;
    function ContainsBlock(Block: TSSABasicBlock): Boolean;
  end;

  { TStrengthReduction - Replace expensive operations with cheaper ones }
  TStrengthReduction = class
  private
    FProgram: TSSAProgram;
    FReductions: Integer;
    FLoops: TObjectList;  // Owns TLoopInfoSR objects
    FDominatorMap: TFPHashList;
    FInductionVars: array of TInductionVariable;

    { Check if value is a constant integer }
    function GetConstInt(const Val: TSSAValue; out ConstVal: Int64): Boolean;

    { Check if value is a constant float }
    function GetConstFloat(const Val: TSSAValue; out ConstVal: Double): Boolean;

    { Check if integer is power of 2, return log2 if yes }
    function IsPowerOfTwo(N: Int64; out Log2: Integer): Boolean;

    { Reduce multiplication operations }
    function ReduceMultiplication(const Instr: TSSAInstruction): TSSAInstruction;

    { Reduce division operations }
    function ReduceDivision(const Instr: TSSAInstruction): TSSAInstruction;

    { Reduce power operations }
    function ReducePower(const Instr: TSSAInstruction): TSSAInstruction;

    { Process all blocks for simple reductions }
    procedure ReduceBlocks;

    { === Loop-based strength reduction === }

    { Build dominator map from program's dominator tree }
    procedure BuildDominatorMap;

    { Find all natural loops via back-edges }
    procedure FindLoops;

    { Check if edge (From -> Target) is a back-edge }
    function IsBackEdge(From, Target: TSSABasicBlock): Boolean;

    { Compute all blocks in a natural loop }
    procedure ComputeLoopBlocks(Loop: TLoopInfoSR; BackEdgeSource: TSSABasicBlock);

    { Find basic induction variables in all loops }
    procedure FindInductionVariables;

    { Check if instruction is IV update (i = i + const or i = i - const) }
    function IsIVUpdate(Instr: TSSAInstruction; Loop: TLoopInfoSR;
                        out IVRegIndex: Integer; out IVRegType: TSSARegisterType;
                        out StepValue: TSSAValue; out IsAdd: Boolean): Boolean;

    { Apply strength reduction to loop IV-dependent multiplications }
    procedure ReduceIVMultiplications;

    { Find the initial value of an IV from the preheader }
    function FindIVInitValue(const IV: TInductionVariable; PreHeader: TSSABasicBlock;
                             out InitVal: TSSAValue): Boolean;

  public
    constructor Create(Prog: TSSAProgram);
    destructor Destroy; override;

    { Run strength reduction pass }
    function Run: Integer;
  end;

implementation

{$IFDEF DEBUG_STRENGTH}
uses SedaiDebug;
{$ENDIF}

{ TLoopInfoSR }

constructor TLoopInfoSR.Create(AHeader: TSSABasicBlock);
begin
  inherited Create;
  Header := AHeader;
  Blocks := TFPList.Create;
  BackEdgeSources := TFPList.Create;
  Blocks.Add(Pointer(Header));
end;

destructor TLoopInfoSR.Destroy;
begin
  Blocks.Free;
  BackEdgeSources.Free;
  inherited;
end;

function TLoopInfoSR.ContainsBlock(Block: TSSABasicBlock): Boolean;
begin
  Result := Blocks.IndexOf(Pointer(Block)) >= 0;
end;

{ TStrengthReduction }

constructor TStrengthReduction.Create(Prog: TSSAProgram);
begin
  inherited Create;
  FProgram := Prog;
  FReductions := 0;
  FLoops := TObjectList.Create(True);  // Owns TLoopInfoSR objects
  FDominatorMap := TFPHashList.Create;
  SetLength(FInductionVars, 0);
end;

destructor TStrengthReduction.Destroy;
begin
  FLoops.Free;
  FDominatorMap.Free;
  inherited;
end;

function TStrengthReduction.Run: Integer;
begin
  {$IFDEF DEBUG_STRENGTH}
  if DebugStrength then
    WriteLn('[StrengthRed] Running strength reduction...');
  {$ENDIF}

  // Phase 1: Simple pattern-based reductions (x*2 → x+x, etc.)
  ReduceBlocks;

  // Phase 2: Loop-based induction variable strength reduction
  // Transforms: FOR I = init TO n: J = I * const -> J starts at init*const, stride = step*const
  BuildDominatorMap;
  if FDominatorMap.Count > 0 then
  begin
    FindLoops;
    if FLoops.Count > 0 then
    begin
      FindInductionVariables;
      if Length(FInductionVars) > 0 then
        ReduceIVMultiplications;
    end;
  end;

  {$IFDEF DEBUG_STRENGTH}
  if DebugStrength then
    WriteLn('[StrengthRed] Applied ', FReductions, ' reductions');
  {$ENDIF}
  Result := FReductions;
end;

function TStrengthReduction.GetConstInt(const Val: TSSAValue; out ConstVal: Int64): Boolean;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  i, j: Integer;
  SourceReg: TSSAValue;
begin
  Result := False;

  // Check if it's an inline constant
  if Val.Kind = svkConstInt then
  begin
    ConstVal := Val.ConstInt;
    Result := True;
    Exit;
  end;

  // Check if it's a register loaded with a constant
  if Val.Kind = svkRegister then
  begin
    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
      WriteLn('[StrengthRed] GetConstInt: Looking for INT[', Val.RegIndex, '] (RegType=', Ord(Val.RegType), ')');
    {$ENDIF}

    // Pass 1: Look for direct LoadConstInt
    for i := 0 to FProgram.Blocks.Count - 1 do
    begin
      Block := FProgram.Blocks[i];
      for j := 0 to Block.Instructions.Count - 1 do
      begin
        Instr := Block.Instructions[j];

        // Check if this instruction loads an int constant into our register
        if (Instr.OpCode = ssaLoadConstInt) and
           (Instr.Dest.Kind = svkRegister) and
           (Instr.Dest.RegType = Val.RegType) and
           (Instr.Dest.RegIndex = Val.RegIndex) and
           (Instr.Src1.Kind = svkConstInt) then
        begin
          ConstVal := Instr.Src1.ConstInt;
          {$IFDEF DEBUG_STRENGTH}
          if DebugStrength then
            WriteLn('[StrengthRed] GetConstInt: Found! Value=', ConstVal);
          {$ENDIF}
          Result := True;
          Exit;
        end;
      end;
    end;

    // Pass 2: Look for Copy from a constant register (one level of indirection)
    for i := 0 to FProgram.Blocks.Count - 1 do
    begin
      Block := FProgram.Blocks[i];
      for j := 0 to Block.Instructions.Count - 1 do
      begin
        Instr := Block.Instructions[j];

        // Check for Copy that writes to our register
        if (Instr.OpCode = ssaCopyInt) and
           (Instr.Dest.Kind = svkRegister) and
           (Instr.Dest.RegType = Val.RegType) and
           (Instr.Dest.RegIndex = Val.RegIndex) and
           (Instr.Src1.Kind = svkRegister) then
        begin
          // Recursively check if source is a constant
          SourceReg := Instr.Src1;
          if GetConstInt(SourceReg, ConstVal) then
          begin
            {$IFDEF DEBUG_STRENGTH}
            if DebugStrength then
              WriteLn('[StrengthRed] GetConstInt: Found via Copy! Value=', ConstVal);
            {$ENDIF}
            Result := True;
            Exit;
          end;
        end;
      end;
    end;

    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
    begin
      WriteLn('[StrengthRed] GetConstInt: NOT FOUND');
      // Dump all LoadConstInt instructions
      WriteLn('[StrengthRed]   All LoadConstInt instructions:');
      for i := 0 to FProgram.Blocks.Count - 1 do
      begin
        Block := FProgram.Blocks[i];
        for j := 0 to Block.Instructions.Count - 1 do
        begin
          Instr := Block.Instructions[j];
          if Instr.OpCode = ssaLoadConstInt then
            WriteLn('[StrengthRed]     ', Instr.ToString);
        end;
      end;
    end;
    {$ENDIF}
  end;
end;

function TStrengthReduction.GetConstFloat(const Val: TSSAValue; out ConstVal: Double): Boolean;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  i, j: Integer;
  TempInt: Int64;
begin
  Result := False;

  // Check if it's an inline constant
  if Val.Kind = svkConstFloat then
  begin
    ConstVal := Val.ConstFloat;
    Result := True;
    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
      WriteLn('[StrengthRed] GetConstFloat: Found inline float constant: ', ConstVal:0:2);
    {$ENDIF}
    Exit;
  end;

  // Check if it's an int constant that should be treated as float
  if Val.Kind = svkConstInt then
  begin
    ConstVal := Double(Val.ConstInt);
    Result := True;
    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
      WriteLn('[StrengthRed] GetConstFloat: Found inline int constant: ', ConstVal:0:2);
    {$ENDIF}
    Exit;
  end;

  // Check if it's a register loaded with a constant
  if Val.Kind = svkRegister then
  begin
    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
      WriteLn('[StrengthRed] GetConstFloat: Searching for register F', Val.RegIndex, ' in LoadConst instructions...');
    {$ENDIF}

    // Scan all blocks to find LoadConstFloat instruction for this register
    for i := 0 to FProgram.Blocks.Count - 1 do
    begin
      Block := FProgram.Blocks[i];
      for j := 0 to Block.Instructions.Count - 1 do
      begin
        Instr := Block.Instructions[j];

        // Check if this instruction loads a float constant into our register
        if (Instr.OpCode = ssaLoadConstFloat) and
           (Instr.Dest.Kind = svkRegister) and
           (Instr.Dest.RegType = Val.RegType) and
           (Instr.Dest.RegIndex = Val.RegIndex) and
           (Instr.Src1.Kind = svkConstFloat) then
        begin
          ConstVal := Instr.Src1.ConstFloat;
          Result := True;
          {$IFDEF DEBUG_STRENGTH}
          if DebugStrength then
            WriteLn('[StrengthRed] GetConstFloat: Found LoadConstFloat F', Val.RegIndex, ' = ', ConstVal:0:2);
          {$ENDIF}
          Exit;
        end;

        // Check if this instruction loads an int constant into our register
        if (Instr.OpCode = ssaLoadConstInt) and
           (Instr.Dest.Kind = svkRegister) and
           (Instr.Dest.RegType = Val.RegType) and
           (Instr.Dest.RegIndex = Val.RegIndex) and
           (Instr.Src1.Kind = svkConstInt) then
        begin
          ConstVal := Double(Instr.Src1.ConstInt);
          Result := True;
          {$IFDEF DEBUG_STRENGTH}
          if DebugStrength then
            WriteLn('[StrengthRed] GetConstFloat: Found LoadConstInt F', Val.RegIndex, ' = ', ConstVal:0:2);
          {$ENDIF}
          Exit;
        end;

        // Check if this instruction converts int register to float (IntToFloat)
        // and track back to find the int constant
        if (Instr.OpCode = ssaIntToFloat) and
           (Instr.Dest.Kind = svkRegister) and
           (Instr.Dest.RegType = Val.RegType) and
           (Instr.Dest.RegIndex = Val.RegIndex) and
           (Instr.Src1.Kind = svkRegister) then
        begin
          // Recursively check if Src1 (the int register) contains a constant
          if GetConstInt(Instr.Src1, TempInt) then
          begin
            ConstVal := Double(TempInt);
            Result := True;
            {$IFDEF DEBUG_STRENGTH}
            if DebugStrength then
              WriteLn('[StrengthRed] GetConstFloat: Found IntToFloat F', Val.RegIndex, ' from I', Instr.Src1.RegIndex, ' = ', ConstVal:0:2);
            {$ENDIF}
            Exit;
          end;
        end;
      end;
    end;

    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
    begin
      WriteLn('[StrengthRed] GetConstFloat: Register F', Val.RegIndex, ' NOT FOUND in any LoadConst');
      WriteLn('[StrengthRed]   Dumping ALL instructions that write to F', Val.RegIndex, ':');
      // Debug: show all instructions that write to this register
      for i := 0 to FProgram.Blocks.Count - 1 do
      begin
        Block := FProgram.Blocks[i];
        for j := 0 to Block.Instructions.Count - 1 do
        begin
          Instr := Block.Instructions[j];
          if (Instr.Dest.Kind = svkRegister) and
             (Instr.Dest.RegType = Val.RegType) and
             (Instr.Dest.RegIndex = Val.RegIndex) then
          begin
            WriteLn('[StrengthRed]     ', SSAOpCodeToString(Instr.OpCode), ' F', Val.RegIndex);
          end;
        end;
      end;
    end;
    {$ENDIF}
  end;
end;

function TStrengthReduction.IsPowerOfTwo(N: Int64; out Log2: Integer): Boolean;
var
  Temp: Int64;
begin
  Result := False;
  Log2 := 0;

  if N <= 0 then
    Exit;

  // Check if N has only one bit set
  if (N and (N - 1)) = 0 then
  begin
    Result := True;
    Temp := N;
    while Temp > 1 do
    begin
      Inc(Log2);
      Temp := Temp shr 1;
    end;
  end;
end;

function TStrengthReduction.ReduceMultiplication(const Instr: TSSAInstruction): TSSAInstruction;
var
  NewInstr: TSSAInstruction;
  ConstValInt: Int64;
  ConstValFloat: Double;
  ConstVal1, ConstVal2: Double;
  ConstVal1Int, ConstVal2Int: Int64;
  Log2: Integer;
  IsConst1, IsConst2: Boolean;
  IsFloat: Boolean;
begin
  Result := Instr;
  NewInstr := Instr.Clone;

  // Handle both integer and float multiplication
  if not (Instr.OpCode in [ssaMulInt, ssaMulFloat]) then
    Exit;

  IsFloat := (Instr.OpCode = ssaMulFloat);

  // Check for constant operands based on type
  if IsFloat then
  begin
    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
    begin
      WriteLn('[StrengthRed] ReduceMultiplication: MulFloat found, checking operands...');
      WriteLn('[StrengthRed]   Src1.Kind=', Ord(Instr.Src1.Kind), ', Src2.Kind=', Ord(Instr.Src2.Kind));
    end;
    {$ENDIF}

    // Float multiplication - check BOTH operands for constants
    IsConst1 := GetConstFloat(Instr.Src1, ConstVal1);
    IsConst2 := GetConstFloat(Instr.Src2, ConstVal2);

    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
    begin
      WriteLn('[StrengthRed]   IsConst1=', IsConst1, ', IsConst2=', IsConst2);
      if IsConst1 then
        WriteLn('[StrengthRed]   ConstVal1=', ConstVal1:0:2);
      if IsConst2 then
        WriteLn('[StrengthRed]   ConstVal2=', ConstVal2:0:2);
    end;
    {$ENDIF}

    // Check if either operand is the constant 2.0
    if IsConst1 and (Abs(ConstVal1 - 2.0) < 1e-10) then
    begin
      // Src1 is 2.0, Src2 is the variable: 2.0 * x → x + x
      NewInstr.OpCode := ssaAddFloat;
      NewInstr.Src1 := Instr.Src2;
      NewInstr.Src2 := Instr.Src2;
      Inc(FReductions);
      {$IFDEF DEBUG_STRENGTH}
      if DebugStrength then
        WriteLn('[StrengthRed]   Applied: 2.0 * x → x + x');
      {$ENDIF}
      Exit(NewInstr);
    end
    else if IsConst2 and (Abs(ConstVal2 - 2.0) < 1e-10) then
    begin
      // Src2 is 2.0, Src1 is the variable: x * 2.0 → x + x
      NewInstr.OpCode := ssaAddFloat;
      NewInstr.Src2 := Instr.Src1;
      Inc(FReductions);
      {$IFDEF DEBUG_STRENGTH}
      if DebugStrength then
        WriteLn('[StrengthRed]   Applied: x * 2.0 → x + x');
      {$ENDIF}
      Exit(NewInstr);
    end;

    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength and (IsConst1 or IsConst2) then
      WriteLn('[StrengthRed]   Constants found but not 2.0, skipping');
    {$ENDIF}
  end
  else
  begin
    // Integer multiplication - check BOTH operands for constants
    IsConst1 := GetConstInt(Instr.Src1, ConstVal1Int);
    IsConst2 := GetConstInt(Instr.Src2, ConstVal2Int);

    // Check if either operand is the constant 2
    if IsConst1 and (ConstVal1Int = 2) then
    begin
      // Src1 is 2, Src2 is the variable: 2 * x → x + x
      NewInstr.OpCode := ssaAddInt;
      NewInstr.Src1 := Instr.Src2;
      NewInstr.Src2 := Instr.Src2;
      Inc(FReductions);
      Exit(NewInstr);
    end
    else if IsConst2 and (ConstVal2Int = 2) then
    begin
      // Src2 is 2, Src1 is the variable: x * 2 → x + x
      NewInstr.OpCode := ssaAddInt;
      NewInstr.Src2 := Instr.Src1;
      Inc(FReductions);
      Exit(NewInstr);
    end;

    // TODO: Future optimizations
    // - x * 3 → (x << 1) + x (requires temp register allocation)
    // - x * power_of_2 → x << log2(power_of_2) (requires shift operations in SSA)
  end;
end;

function TStrengthReduction.ReduceDivision(const Instr: TSSAInstruction): TSSAInstruction;
var
  NewInstr: TSSAInstruction;
  ConstVal: Int64;
  Log2: Integer;
begin
  Result := Instr;
  NewInstr := Instr.Clone;

  // Only handle integer division
  if Instr.OpCode <> ssaDivInt then
    Exit;

  // Check if divisor is a constant
  if not GetConstInt(Instr.Src2, ConstVal) then
    Exit;

  // x / power_of_2 → x >> log2(power_of_2)
  // Note: This requires shift operations in SSA (not yet implemented)
  // Also, right shift is only correct for positive integers (arithmetic shift needed for signed)
  // Skip for now
  if IsPowerOfTwo(ConstVal, Log2) then
  begin
    // Would emit: NewInstr.OpCode := ssaShrInt; NewInstr.Src2 := MakeSSAConstInt(Log2);
    // But ssaShrInt doesn't exist yet, so skip
    Exit;
  end;
end;

function TStrengthReduction.ReducePower(const Instr: TSSAInstruction): TSSAInstruction;
var
  NewInstr: TSSAInstruction;
  ConstVal: Int64;
  ConstFloat: Double;
begin
  Result := Instr;
  NewInstr := Instr.Clone;

  // Check if this is a power operation
  if Instr.OpCode <> ssaPowFloat then
    Exit;

  // x ^ 2 → x * x (for small integer exponents only)
  if Instr.Src2.Kind = svkConstInt then
  begin
    ConstVal := Instr.Src2.ConstInt;
    if ConstVal = 2 then
    begin
      NewInstr.OpCode := ssaMulFloat;
      NewInstr.Src2 := Instr.Src1;  // x * x
      Inc(FReductions);
      Exit(NewInstr);
    end;
  end
  else if Instr.Src2.Kind = svkConstFloat then
  begin
    ConstFloat := Instr.Src2.ConstFloat;
    if Abs(ConstFloat - 2.0) < 1e-10 then
    begin
      NewInstr.OpCode := ssaMulFloat;
      NewInstr.Src2 := Instr.Src1;  // x * x
      Inc(FReductions);
      Exit(NewInstr);
    end;
  end;
end;

procedure TStrengthReduction.ReduceBlocks;
var
  Block: TSSABasicBlock;
  Instr, NewInstr: TSSAInstruction;
  i, j: Integer;
begin
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      // Try strength reduction
      case Instr.OpCode of
        ssaMulInt, ssaMulFloat:
          NewInstr := ReduceMultiplication(Instr);
        ssaDivInt, ssaDivFloat:
          NewInstr := ReduceDivision(Instr);
        ssaPowFloat:
          NewInstr := ReducePower(Instr);
        else
          Continue;
      end;

      // Apply if changed
      if NewInstr.OpCode <> Instr.OpCode then
        Block.Instructions[j] := NewInstr;
    end;
  end;
end;

{ ============================================================================
  Loop-based Induction Variable Strength Reduction
  ============================================================================ }

procedure TStrengthReduction.BuildDominatorMap;
var
  DomTree: TDominatorTree;
  Block, IdomBlock: TSSABasicBlock;
  i: Integer;
begin
  if not Assigned(FProgram.GetDomTree) then
  begin
    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
      WriteLn('[StrengthRed] Dominator tree not available');
    {$ENDIF}
    Exit;
  end;

  DomTree := TDominatorTree(FProgram.GetDomTree);

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    IdomBlock := DomTree.GetIdom(Block);
    if Assigned(IdomBlock) then
      FDominatorMap.Add(Format('%p', [Pointer(Block)]), Pointer(IdomBlock));
  end;

  {$IFDEF DEBUG_STRENGTH}
  if DebugStrength then
    WriteLn('[StrengthRed] Dominator map: ', FDominatorMap.Count, ' entries');
  {$ENDIF}
end;

function TStrengthReduction.IsBackEdge(From, Target: TSSABasicBlock): Boolean;
var
  Dom, NextDom: TSSABasicBlock;
  Steps, Idx: Integer;
begin
  Result := False;
  Dom := From;
  Steps := 0;

  while Assigned(Dom) do
  begin
    Inc(Steps);
    if Steps > 200 then Exit(False);
    if Dom = Target then Exit(True);

    Idx := FDominatorMap.FindIndexOf(Format('%p', [Pointer(Dom)]));
    if Idx < 0 then Break;

    NextDom := TSSABasicBlock(FDominatorMap.Items[Idx]);
    if not Assigned(NextDom) or (NextDom = Dom) then Break;

    Dom := NextDom;
  end;
end;

procedure TStrengthReduction.ComputeLoopBlocks(Loop: TLoopInfoSR; BackEdgeSource: TSSABasicBlock);
var
  Worklist: TFPList;
  Current, Pred: TSSABasicBlock;
  i, Iterations: Integer;
begin
  Worklist := TFPList.Create;
  try
    if not Loop.ContainsBlock(BackEdgeSource) then
    begin
      Loop.Blocks.Add(Pointer(BackEdgeSource));
      Worklist.Add(Pointer(BackEdgeSource));
    end;

    Iterations := 0;
    while Worklist.Count > 0 do
    begin
      Inc(Iterations);
      if Iterations > 10000 then Break;

      Current := TSSABasicBlock(Worklist[Worklist.Count - 1]);
      Worklist.Delete(Worklist.Count - 1);

      if Current = Loop.Header then
        Continue;

      for i := 0 to Current.Predecessors.Count - 1 do
      begin
        Pred := TSSABasicBlock(Current.Predecessors[i]);
        if not Loop.ContainsBlock(Pred) then
        begin
          Loop.Blocks.Add(Pointer(Pred));
          Worklist.Add(Pointer(Pred));
        end;
      end;
    end;
  finally
    Worklist.Free;
  end;
end;

procedure TStrengthReduction.FindLoops;
var
  Block, Succ: TSSABasicBlock;
  Loop: TLoopInfoSR;
  i, j, k: Integer;
  ExistingLoop: TLoopInfoSR;
  Found: Boolean;
begin
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];

    for j := 0 to Block.Successors.Count - 1 do
    begin
      Succ := TSSABasicBlock(Block.Successors[j]);

      if IsBackEdge(Block, Succ) then
      begin
        // Check if we already have a loop with this header
        Found := False;
        for k := 0 to FLoops.Count - 1 do
        begin
          ExistingLoop := TLoopInfoSR(FLoops[k]);
          if ExistingLoop.Header = Succ then
          begin
            if ExistingLoop.BackEdgeSources.IndexOf(Pointer(Block)) < 0 then
              ExistingLoop.BackEdgeSources.Add(Pointer(Block));
            ComputeLoopBlocks(ExistingLoop, Block);
            Found := True;
            Break;
          end;
        end;

        if not Found then
        begin
          Loop := TLoopInfoSR.Create(Succ);
          Loop.BackEdgeSources.Add(Pointer(Block));
          ComputeLoopBlocks(Loop, Block);
          FLoops.Add(Loop);
        end;
      end;
    end;
  end;
end;

function TStrengthReduction.IsIVUpdate(Instr: TSSAInstruction; Loop: TLoopInfoSR;
                                       out IVRegIndex: Integer; out IVRegType: TSSARegisterType;
                                       out StepValue: TSSAValue; out IsAdd: Boolean): Boolean;
var
  DestReg: TSSAValue;
  TempInt: Int64;
  TempFloat: Double;
begin
  Result := False;

  // Check for i = i + const or i = i - const (integer or float)
  if not (Instr.OpCode in [ssaAddInt, ssaSubInt, ssaAddFloat, ssaSubFloat]) then
    Exit;

  // Destination must be a register
  if Instr.Dest.Kind <> svkRegister then
    Exit;

  DestReg := Instr.Dest;

  // Check if Src1 is the same register as Dest (i = i + step)
  if (Instr.Src1.Kind = svkRegister) and
     (Instr.Src1.RegIndex = DestReg.RegIndex) and
     (Instr.Src1.RegType = DestReg.RegType) then
  begin
    // Src2 must be a constant (inline or in a register via LoadConst)
    if Instr.Src2.Kind in [svkConstInt, svkConstFloat] then
    begin
      IVRegIndex := DestReg.RegIndex;
      IVRegType := DestReg.RegType;
      StepValue := Instr.Src2;
      IsAdd := Instr.OpCode in [ssaAddInt, ssaAddFloat];
      Result := True;
      Exit;
    end
    else if (Instr.Src2.Kind = svkRegister) then
    begin
      // Check if the register contains a constant
      if (Instr.OpCode in [ssaAddInt, ssaSubInt]) and GetConstInt(Instr.Src2, TempInt) then
      begin
        IVRegIndex := DestReg.RegIndex;
        IVRegType := DestReg.RegType;
        StepValue := MakeSSAConstInt(TempInt);
        IsAdd := Instr.OpCode = ssaAddInt;
        Result := True;
        Exit;
      end
      else if (Instr.OpCode in [ssaAddFloat, ssaSubFloat]) and GetConstFloat(Instr.Src2, TempFloat) then
      begin
        IVRegIndex := DestReg.RegIndex;
        IVRegType := DestReg.RegType;
        StepValue := MakeSSAConstFloat(TempFloat);
        IsAdd := Instr.OpCode = ssaAddFloat;
        Result := True;
        Exit;
      end;
    end;
  end;

  // Check if Src2 is the same register as Dest (i = step + i) - only for addition
  if (Instr.OpCode in [ssaAddInt, ssaAddFloat]) and
     (Instr.Src2.Kind = svkRegister) and
     (Instr.Src2.RegIndex = DestReg.RegIndex) and
     (Instr.Src2.RegType = DestReg.RegType) then
  begin
    // Src1 must be a constant (inline or in a register via LoadConst)
    if Instr.Src1.Kind in [svkConstInt, svkConstFloat] then
    begin
      IVRegIndex := DestReg.RegIndex;
      IVRegType := DestReg.RegType;
      StepValue := Instr.Src1;
      IsAdd := True;
      Result := True;
      Exit;
    end
    else if (Instr.Src1.Kind = svkRegister) then
    begin
      // Check if the register contains a constant
      if (Instr.OpCode = ssaAddInt) and GetConstInt(Instr.Src1, TempInt) then
      begin
        IVRegIndex := DestReg.RegIndex;
        IVRegType := DestReg.RegType;
        StepValue := MakeSSAConstInt(TempInt);
        IsAdd := True;
        Result := True;
        Exit;
      end
      else if (Instr.OpCode = ssaAddFloat) and GetConstFloat(Instr.Src1, TempFloat) then
      begin
        IVRegIndex := DestReg.RegIndex;
        IVRegType := DestReg.RegType;
        StepValue := MakeSSAConstFloat(TempFloat);
        IsAdd := True;
        Result := True;
        Exit;
      end;
    end;
  end;
end;

procedure TStrengthReduction.FindInductionVariables;
var
  Loop: TLoopInfoSR;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  IV: TInductionVariable;
  i, j, k: Integer;
  IVRegIndex: Integer;
  IVRegType: TSSARegisterType;
  StepValue: TSSAValue;
  IsAdd: Boolean;
  {$IFDEF DEBUG_STRENGTH}
  TempInt: Int64;
  TempFloat: Double;
  {$ENDIF}
begin
  SetLength(FInductionVars, 0);

  for i := 0 to FLoops.Count - 1 do
  begin
    Loop := TLoopInfoSR(FLoops[i]);

    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
      WriteLn('[StrengthRed] Scanning loop ', Loop.Header.LabelName, ' (', Loop.Blocks.Count, ' blocks)');
    {$ENDIF}

    // Scan all blocks in the loop for IV updates
    for j := 0 to Loop.Blocks.Count - 1 do
    begin
      Block := TSSABasicBlock(Loop.Blocks[j]);

      {$IFDEF DEBUG_STRENGTH}
      if DebugStrength then
        WriteLn('[StrengthRed]   Block ', Block.LabelName, ' has ', Block.Instructions.Count, ' instructions');
      {$ENDIF}

      for k := 0 to Block.Instructions.Count - 1 do
      begin
        Instr := Block.Instructions[k];

        {$IFDEF DEBUG_STRENGTH}
        if DebugStrength and (Instr.OpCode in [ssaAddInt, ssaSubInt, ssaAddFloat, ssaSubFloat]) then
        begin
          WriteLn('[StrengthRed]     Add/Sub instr: ', Instr.ToString);
          // Debug: check if we can resolve the step constant
          if (Instr.Src1.Kind = svkRegister) and
             (Instr.Src1.RegIndex = Instr.Dest.RegIndex) and
             (Instr.Src1.RegType = Instr.Dest.RegType) then
          begin
            Write('[StrengthRed]       -> Potential IV, Src2=');
            if Instr.Src2.Kind = svkConstInt then
              WriteLn('const ', Instr.Src2.ConstInt)
            else if Instr.Src2.Kind = svkConstFloat then
              WriteLn('const ', Instr.Src2.ConstFloat:0:2)
            else if Instr.Src2.Kind = svkRegister then
            begin
              Write('reg INT[', Instr.Src2.RegIndex, '], ');
              if GetConstInt(Instr.Src2, TempInt) then
                WriteLn('resolved to ', TempInt)
              else if GetConstFloat(Instr.Src2, TempFloat) then
                WriteLn('resolved to ', TempFloat:0:2)
              else
                WriteLn('NOT constant');
            end
            else
              WriteLn('other');
          end;
        end;
        {$ENDIF}

        if IsIVUpdate(Instr, Loop, IVRegIndex, IVRegType, StepValue, IsAdd) then
        begin
          // Found an induction variable update
          IV.VarRegIndex := IVRegIndex;
          IV.VarRegType := IVRegType;
          IV.VarVersion := Instr.Dest.Version;
          IV.StepValue := StepValue;
          IV.StepIsAdd := IsAdd;
          IV.HeaderBlock := Loop.Header;
          IV.UpdateInstr := Instr;
          IV.InitValue.Kind := svkNone;  // We'll find this later if needed

          SetLength(FInductionVars, Length(FInductionVars) + 1);
          FInductionVars[High(FInductionVars)] := IV;

          {$IFDEF DEBUG_STRENGTH}
          if DebugStrength then
            WriteLn('[StrengthRed] Found IV: reg ', IVRegIndex, ' in loop ', Loop.Header.LabelName,
                    ' step=', SSAValueToString(StepValue), ' add=', IsAdd);
          {$ENDIF}
        end;
      end;
    end;
  end;
end;

function TStrengthReduction.FindIVInitValue(const IV: TInductionVariable;
  PreHeader: TSSABasicBlock; out InitVal: TSSAValue): Boolean;
var
  Instr: TSSAInstruction;
  i, j: Integer;
  Block: TSSABasicBlock;
  TempInt: Int64;
  TempFloat: Double;
begin
  Result := False;
  InitVal.Kind := svkNone;

  // Strategy: Look for assignments to the IV register in the preheader
  // or blocks that dominate the preheader (going backwards)

  // First, check the preheader itself
  if Assigned(PreHeader) then
  begin
    for i := PreHeader.Instructions.Count - 1 downto 0 do
    begin
      Instr := PreHeader.Instructions[i];

      // Look for LoadConst or Copy that writes to the IV register
      if (Instr.Dest.Kind = svkRegister) and
         (Instr.Dest.RegIndex = IV.VarRegIndex) and
         (Instr.Dest.RegType = IV.VarRegType) then
      begin
        // Found assignment to IV
        case Instr.OpCode of
          ssaLoadConstInt:
            if Instr.Src1.Kind = svkConstInt then
            begin
              InitVal := Instr.Src1;
              {$IFDEF DEBUG_STRENGTH}
              if DebugStrength then
                WriteLn('[StrengthRed] Found IV init in preheader: ', InitVal.ConstInt);
              {$ENDIF}
              Result := True;
              Exit;
            end;
          ssaLoadConstFloat:
            if Instr.Src1.Kind = svkConstFloat then
            begin
              InitVal := Instr.Src1;
              {$IFDEF DEBUG_STRENGTH}
              if DebugStrength then
                WriteLn('[StrengthRed] Found IV init in preheader: ', InitVal.ConstFloat:0:2);
              {$ENDIF}
              Result := True;
              Exit;
            end;
          ssaCopyInt:
            // Try to resolve the copied value
            if GetConstInt(Instr.Src1, TempInt) then
            begin
              InitVal := MakeSSAConstInt(TempInt);
              {$IFDEF DEBUG_STRENGTH}
              if DebugStrength then
                WriteLn('[StrengthRed] Found IV init via Copy: ', TempInt);
              {$ENDIF}
              Result := True;
              Exit;
            end;
          ssaCopyFloat:
            if GetConstFloat(Instr.Src1, TempFloat) then
            begin
              InitVal := MakeSSAConstFloat(TempFloat);
              {$IFDEF DEBUG_STRENGTH}
              if DebugStrength then
                WriteLn('[StrengthRed] Found IV init via Copy: ', TempFloat:0:2);
              {$ENDIF}
              Result := True;
              Exit;
            end;
          ssaIntToFloat:
            // IntToFloat means we need to find the int source
            if GetConstInt(Instr.Src1, TempInt) then
            begin
              InitVal := MakeSSAConstFloat(Double(TempInt));
              {$IFDEF DEBUG_STRENGTH}
              if DebugStrength then
                WriteLn('[StrengthRed] Found IV init via IntToFloat: ', TempInt);
              {$ENDIF}
              Result := True;
              Exit;
            end;
        end;
      end;
    end;
  end;

  // Also check all blocks before the loop (scan entire program for now)
  // This is needed for FOR loops where initialization might be in a separate block
  for j := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[j];
    // Skip blocks that are part of any loop containing the header
    // (we want blocks BEFORE the loop)
    if Block = IV.HeaderBlock then
      Continue;

    for i := Block.Instructions.Count - 1 downto 0 do
    begin
      Instr := Block.Instructions[i];

      // Look for StoreRegToVar followed by LoadVarToReg for the IV
      // or direct LoadConst to the IV register
      if (Instr.Dest.Kind = svkRegister) and
         (Instr.Dest.RegIndex = IV.VarRegIndex) and
         (Instr.Dest.RegType = IV.VarRegType) then
      begin
        case Instr.OpCode of
          ssaLoadConstInt:
            if Instr.Src1.Kind = svkConstInt then
            begin
              InitVal := Instr.Src1;
              {$IFDEF DEBUG_STRENGTH}
              if DebugStrength then
                WriteLn('[StrengthRed] Found IV init in block ', Block.LabelName, ': ', InitVal.ConstInt);
              {$ENDIF}
              Result := True;
              Exit;
            end;
          ssaLoadConstFloat:
            if Instr.Src1.Kind = svkConstFloat then
            begin
              InitVal := Instr.Src1;
              {$IFDEF DEBUG_STRENGTH}
              if DebugStrength then
                WriteLn('[StrengthRed] Found IV init in block ', Block.LabelName, ': ', InitVal.ConstFloat:0:2);
              {$ENDIF}
              Result := True;
              Exit;
            end;
        end;
      end;
    end;
  end;

  {$IFDEF DEBUG_STRENGTH}
  if DebugStrength then
    WriteLn('[StrengthRed] Could not find IV init value for reg ', IV.VarRegIndex);
  {$ENDIF}
end;

procedure TStrengthReduction.ReduceIVMultiplications;
var
  Loop: TLoopInfoSR;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  IV: TInductionVariable;
  i, j, k, m: Integer;
  MulConstInt: Int64;
  MulConstFloat: Double;
  FoundIV: Boolean;
  IVSrc1: Boolean;  // True if IV is in Src1, False if in Src2
  AccumReg: Integer;
  AccumRegType: TSSARegisterType;
  InitInstr, UpdateInstr: TSSAInstruction;
  PreHeader: TSSABasicBlock;
  InsertPos: Integer;
  IVStepInt: Int64;
  IVStepFloat: Double;
  StrideInt: Int64;
  StrideFloat: Double;
  IVInitValue: TSSAValue;
  AccumInitInt: Int64;
  AccumInitFloat: Double;

  // Initialize variables to avoid warnings
  procedure InitVars;
  begin
    MulConstInt := 0;
    MulConstFloat := 0.0;
    IVStepInt := 0;
    IVStepFloat := 0.0;
    StrideInt := 0;
    StrideFloat := 0.0;
    AccumInitInt := 0;
    AccumInitFloat := 0.0;
    IVSrc1 := False;
    FillChar(IV, SizeOf(IV), 0);
    IVInitValue.Kind := svkNone;
  end;

  { Helper to check if a value uses a specific IV }
  function UsesIV(const Val: TSSAValue; const IV: TInductionVariable): Boolean;
  begin
    Result := (Val.Kind = svkRegister) and
              (Val.RegIndex = IV.VarRegIndex) and
              (Val.RegType = IV.VarRegType);
  end;

  { Find pre-header block (predecessor of header that's not in the loop) }
  function FindPreHeader(Loop: TLoopInfoSR): TSSABasicBlock;
  var
    PredIdx: Integer;
    Pred: TSSABasicBlock;
  begin
    Result := nil;
    for PredIdx := 0 to Loop.Header.Predecessors.Count - 1 do
    begin
      Pred := TSSABasicBlock(Loop.Header.Predecessors[PredIdx]);
      if not Loop.ContainsBlock(Pred) then
      begin
        // Check if this looks like a pre-header (single successor = loop header)
        if (Pred.Successors.Count = 1) and
           (TSSABasicBlock(Pred.Successors[0]) = Loop.Header) then
        begin
          Result := Pred;
          Exit;
        end;
      end;
    end;
    // If no dedicated pre-header found, just use first non-loop predecessor
    for PredIdx := 0 to Loop.Header.Predecessors.Count - 1 do
    begin
      Pred := TSSABasicBlock(Loop.Header.Predecessors[PredIdx]);
      if not Loop.ContainsBlock(Pred) then
      begin
        Result := Pred;
        Exit;
      end;
    end;
  end;

  { Find instruction position after IV update in block }
  function FindPositionAfterIVUpdate(IVUpdate: TSSAInstruction; Block: TSSABasicBlock): Integer;
  var
    InstrIdx: Integer;
  begin
    for InstrIdx := 0 to Block.Instructions.Count - 1 do
    begin
      if Block.Instructions[InstrIdx] = IVUpdate then
      begin
        Result := InstrIdx + 1;
        Exit;
      end;
    end;
    Result := Block.Instructions.Count;
  end;

begin
  InitVars;  // Initialize all variables to avoid uninitialized warnings

  { Strategy for IV multiplication strength reduction:

    Original:
      FOR I = init TO N
        J = I * STRIDE
        ...
      NEXT I

    Transformed:
      ACCUM = init * STRIDE
      FOR I = init TO N
        J = ACCUM
        ...
        ACCUM = ACCUM + (step * STRIDE)  (after IV update)
      NEXT I

    The stride for the accumulator is: IV_STEP * MUL_CONST
    For example: if I goes 2,3,4,5 (init=2, step=1) and we compute I*4,
    then ACCUM goes 8,12,16,20 (init=8, stride=4)
  }

  for i := 0 to FLoops.Count - 1 do
  begin
    Loop := TLoopInfoSR(FLoops[i]);
    PreHeader := FindPreHeader(Loop);

    // Scan all blocks in the loop for IV * constant patterns
    for j := 0 to Loop.Blocks.Count - 1 do
    begin
      Block := TSSABasicBlock(Loop.Blocks[j]);

      k := 0;
      while k < Block.Instructions.Count do
      begin
        Instr := Block.Instructions[k];

        // Look for multiplication operations
        if not (Instr.OpCode in [ssaMulInt, ssaMulFloat]) then
        begin
          Inc(k);
          Continue;
        end;

        // Check if either operand is an induction variable
        FoundIV := False;
        IVSrc1 := False;

        for m := 0 to High(FInductionVars) do
        begin
          IV := FInductionVars[m];
          if IV.HeaderBlock <> Loop.Header then
            Continue;

          // Check if Src1 or Src2 is the IV
          // IMPORTANT: Skip IV * IV patterns (e.g., i% * i%) - we can only optimize IV * const
          if UsesIV(Instr.Src1, IV) and not UsesIV(Instr.Src2, IV) then
          begin
            IVSrc1 := True;
            // IV * something - check if "something" is constant
            if Instr.OpCode = ssaMulInt then
            begin
              if GetConstInt(Instr.Src2, MulConstInt) then
                FoundIV := True;
            end
            else // ssaMulFloat
            begin
              if GetConstFloat(Instr.Src2, MulConstFloat) then
                FoundIV := True;
            end;
          end
          else if UsesIV(Instr.Src2, IV) and not UsesIV(Instr.Src1, IV) then
          begin
            IVSrc1 := False;
            // something * IV - check if "something" is constant
            if Instr.OpCode = ssaMulInt then
            begin
              if GetConstInt(Instr.Src1, MulConstInt) then
                FoundIV := True;
            end
            else // ssaMulFloat
            begin
              if GetConstFloat(Instr.Src1, MulConstFloat) then
                FoundIV := True;
            end;
          end;

          if FoundIV then
            Break;
        end;

        if not FoundIV then
        begin
          Inc(k);
          Continue;
        end;

        // Skip transformation if no pre-header available
        if PreHeader = nil then
        begin
          {$IFDEF DEBUG_STRENGTH}
          if DebugStrength then
            WriteLn('[StrengthRed] No pre-header for loop ', Loop.Header.LabelName, ' - skipping IV SR');
          {$ENDIF}
          Inc(k);
          Continue;
        end;

        // Get IV step value
        if IV.StepValue.Kind = svkConstInt then
          IVStepInt := IV.StepValue.ConstInt
        else if IV.StepValue.Kind = svkConstFloat then
          IVStepFloat := IV.StepValue.ConstFloat
        else
        begin
          Inc(k);
          Continue;  // Step must be constant
        end;

        // Calculate accumulator stride = IV_step * mul_const
        if Instr.OpCode = ssaMulInt then
        begin
          AccumRegType := srtInt;
          StrideInt := IVStepInt * MulConstInt;
          if not IV.StepIsAdd then StrideInt := -StrideInt;
        end
        else
        begin
          AccumRegType := srtFloat;
          if IV.StepValue.Kind = svkConstInt then
            StrideFloat := IVStepInt * MulConstFloat
          else
            StrideFloat := IVStepFloat * MulConstFloat;
          if not IV.StepIsAdd then StrideFloat := -StrideFloat;
        end;

        // Find the IV initial value to correctly initialize the accumulator
        // AccumInit = IVInit * MulConst
        if not FindIVInitValue(IV, PreHeader, IVInitValue) then
        begin
          {$IFDEF DEBUG_STRENGTH}
          if DebugStrength then
            WriteLn('[StrengthRed] Cannot find IV init value - skipping transformation');
          {$ENDIF}
          Inc(k);
          Continue;  // Skip this transformation if we can't find init value
        end;

        // Calculate accumulator initial value = IVInit * MulConst
        if Instr.OpCode = ssaMulInt then
        begin
          if IVInitValue.Kind = svkConstInt then
            AccumInitInt := IVInitValue.ConstInt * MulConstInt
          else
          begin
            Inc(k);
            Continue;  // Need integer IV for integer multiplication
          end;
        end
        else
        begin
          if IVInitValue.Kind = svkConstFloat then
            AccumInitFloat := IVInitValue.ConstFloat * MulConstFloat
          else if IVInitValue.Kind = svkConstInt then
            AccumInitFloat := IVInitValue.ConstInt * MulConstFloat
          else
          begin
            Inc(k);
            Continue;
          end;
        end;

        // Allocate new register for accumulator
        AccumReg := FProgram.AllocRegister(AccumRegType);

        {$IFDEF DEBUG_STRENGTH}
        if DebugStrength then
        begin
          if AccumRegType = srtInt then
            WriteLn('[StrengthRed] Transforming IV*', MulConstInt, ' -> accum (init=', AccumInitInt,
                    ', stride=', StrideInt, ', reg=', AccumReg, ')')
          else
            WriteLn('[StrengthRed] Transforming IV*', MulConstFloat:0:2, ' -> accum (init=', AccumInitFloat:0:2,
                    ', stride=', StrideFloat:0:2, ', reg=', AccumReg, ')');
        end;
        {$ENDIF}

        // 1. Insert initialization in pre-header: ACCUM = IVInit * MulConst
        //    Insert before the final jump instruction
        InsertPos := PreHeader.Instructions.Count - 1;
        if InsertPos < 0 then InsertPos := 0;

        if AccumRegType = srtInt then
        begin
          InitInstr := TSSAInstruction.Create(ssaLoadConstInt);
          InitInstr.Dest := MakeSSARegister(srtInt, AccumReg);
          InitInstr.Src1 := MakeSSAConstInt(AccumInitInt);
        end
        else
        begin
          InitInstr := TSSAInstruction.Create(ssaLoadConstFloat);
          InitInstr.Dest := MakeSSARegister(srtFloat, AccumReg);
          InitInstr.Src1 := MakeSSAConstFloat(AccumInitFloat);
        end;
        InitInstr.Comment := 'SR: accum init';
        PreHeader.Instructions.Insert(InsertPos, InitInstr);

        // 2. Replace multiplication with copy from accumulator
        //    Change: J = I * STRIDE  -->  J = ACCUM (as Copy instruction)
        if AccumRegType = srtInt then
          Instr.OpCode := ssaCopyInt
        else
          Instr.OpCode := ssaCopyFloat;
        Instr.Src1 := MakeSSARegister(AccumRegType, AccumReg);
        Instr.Src2.Kind := svkNone;
        Instr.Comment := 'SR: from accum';

        // 3. Insert accumulator update after IV update: ACCUM = ACCUM + STRIDE
        //    We need to find the back-edge block(s) where IV is updated
        //    and insert the accumulator update there
        for m := 0 to Loop.BackEdgeSources.Count - 1 do
        begin
          Block := TSSABasicBlock(Loop.BackEdgeSources[m]);

          // Find position after IV update in this block
          InsertPos := FindPositionAfterIVUpdate(IV.UpdateInstr, Block);

          // If IV update not in this block, insert at end before jump
          if InsertPos = Block.Instructions.Count then
          begin
            InsertPos := Block.Instructions.Count - 1;
            if InsertPos < 0 then InsertPos := 0;
          end;

          if AccumRegType = srtInt then
          begin
            UpdateInstr := TSSAInstruction.Create(ssaAddInt);
            UpdateInstr.Dest := MakeSSARegister(srtInt, AccumReg);
            UpdateInstr.Src1 := MakeSSARegister(srtInt, AccumReg);
            UpdateInstr.Src2 := MakeSSAConstInt(StrideInt);
          end
          else
          begin
            UpdateInstr := TSSAInstruction.Create(ssaAddFloat);
            UpdateInstr.Dest := MakeSSARegister(srtFloat, AccumReg);
            UpdateInstr.Src1 := MakeSSARegister(srtFloat, AccumReg);
            UpdateInstr.Src2 := MakeSSAConstFloat(StrideFloat);
          end;
          UpdateInstr.Comment := 'SR: accum update';
          Block.Instructions.Insert(InsertPos, UpdateInstr);
        end;

        Inc(FReductions);
        Inc(k);  // Move to next instruction
      end;
    end;
  end;
end;

end.
