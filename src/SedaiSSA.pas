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
unit SedaiSSA;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

// Include shared optimization flags - modified to force recompilation
{$I OptimizationFlags.inc}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, StrUtils, Variants, Math, Generics.Collections,
  SedaiLexerTypes, SedaiLexerToken, SedaiParserTypes, SedaiAST,
  SedaiSSATypes;

type
  { Loop info for FOR/NEXT implementation }
  TLoopInfo = record
    VarName: string;
    VarReg: TSSAValue;
    EndValue: TSSAValue;
    StepValue: TSSAValue;
    StepIsNegative: Boolean;  // True if STEP is known to be negative at compile time
    NeedRuntimeCheck: Boolean;  // True if STEP direction determined at runtime
    CondLabel: string;          // Primary condition label (or LE label if runtime check)
    CondLabelGE: string;        // GE label if NeedRuntimeCheck, empty otherwise
    BodyLabel: string;
    EndLabel: string;
  end;

  { SSA Generator - converts AST to SSA IR }
  TSSAGenerator = class
  private
    FProgram: TSSAProgram;
    FCurrentBlock: TSSABasicBlock;
    FLabelCounter: Integer;
    FCurrentLineNumber: Integer;  // Current BASIC line number being processed
    FLastResult: TSSAValue;    // Last expression result
    FVarMap: TStringList;      // Maps variable name to register index (stored as object)
    FLoopStack: array of TLoopInfo;  // Stack for nested loops
    FConstFloatRegs: specialize TDictionary<Integer, Double>;   // Maps float register to constant value
    FConstIntRegs: specialize TDictionary<Integer, Integer>;    // Maps int register to constant value

    function GenerateUniqueLabel(const Prefix: string): string;
    function GetVariableType(const VarName: string): TSSARegisterType;
    function GetOrAllocateVariable(const VarName: string): TSSAValue;
    procedure PreAllocateVariables(Node: TASTNode);  // Pre-scan AST to allocate all variable registers
    procedure ProcessStatement(Node: TASTNode);
    procedure FixForwardReferences;  // PHASE 3 TIER 3: Fix forward GOTO/GOSUB references
    procedure ProcessExpression(Node: TASTNode; out Result: TSSAValue); overload;
    procedure ProcessExpression(Node: TASTNode; out Result: TSSAValue; const DestHint: TSSAValue); overload;
    procedure ProcessAssignment(Node: TASTNode);
    procedure ProcessArrayStore(Node: TASTNode);
    procedure ProcessPrint(Node: TASTNode);
    procedure ProcessInput(Node: TASTNode);
    procedure ProcessDim(Node: TASTNode);
    procedure ProcessForLoop(Node: TASTNode);
    procedure ProcessNext(Node: TASTNode);
    procedure ProcessIfStatement(Node: TASTNode);
    procedure ProcessGoto(Node: TASTNode);
    procedure ProcessGosub(Node: TASTNode);
    procedure ProcessBox(Node: TASTNode);
    procedure ProcessCircle(Node: TASTNode);
    procedure ProcessDraw(Node: TASTNode);
    procedure ProcessLocate(Node: TASTNode);
    procedure ProcessGraphics(Node: TASTNode);
    procedure ProcessScnClr(Node: TASTNode);
    procedure EmitInstruction(OpCode: TSSAOpCode; Dest, Src1, Src2, Src3: TSSAValue);

    // Constant register tracking helpers
    procedure RecordConstantRegister(const Val: TSSAValue);
    function TryGetConstantFloat(RegIndex: Integer; out ConstValue: Double): Boolean;
    function TryGetConstantInt(RegIndex: Integer; out ConstValue: Integer): Boolean;
    procedure InvalidateRegister(RegIndex: Integer; RegType: TSSARegisterType);

    // Type conversion helpers
    function EnsureIntRegister(const Val: TSSAValue): TSSAValue;
  public
    constructor Create;
    destructor Destroy; override;
    function Generate(AST: TASTNode): TSSAProgram;
    property Program_: TSSAProgram read FProgram;
  end;

implementation

{$IFDEF DEBUG_SSA}
uses SedaiDebug;
{$ENDIF}

constructor TSSAGenerator.Create;
begin
  inherited Create;
  FProgram := nil;
  FCurrentBlock := nil;
  FLabelCounter := 0;
  FCurrentLineNumber := 0;
  FLastResult := MakeSSAValue(svkNone);
  FVarMap := TStringList.Create;
  FVarMap.Sorted := True;
  FVarMap.Duplicates := dupIgnore;
  SetLength(FLoopStack, 0);
  FConstFloatRegs := specialize TDictionary<Integer, Double>.Create;
  FConstIntRegs := specialize TDictionary<Integer, Integer>.Create;
end;

destructor TSSAGenerator.Destroy;
begin
  FVarMap.Free;
  FConstFloatRegs.Free;
  FConstIntRegs.Free;
  inherited Destroy;
end;

function TSSAGenerator.GenerateUniqueLabel(const Prefix: string): string;
begin
  Inc(FLabelCounter);
  Result := Format('%s_%d', [Prefix, FLabelCounter]);
end;

function TSSAGenerator.GetVariableType(const VarName: string): TSSARegisterType;
var
  LastChar: Char;
begin
  if Length(VarName) = 0 then Exit(srtFloat);
  LastChar := VarName[Length(VarName)];
  if LastChar = '$' then
    Result := srtString
  else if LastChar = '%' then
    Result := srtInt
  else
    Result := srtFloat;  // Default BASIC type
end;

function TSSAGenerator.GetOrAllocateVariable(const VarName: string): TSSAValue;
var
  Idx: Integer;
  RegType: TSSARegisterType;
  RegIndex: Integer;
  PackedInfo: PtrInt;
begin
  Idx := FVarMap.IndexOf(VarName);
  if Idx >= 0 then
  begin
    // Variable already allocated
    // Packing: RegType in bits 16-23, RegIndex in bits 0-15 (supports 0-65535)
    PackedInfo := PtrInt(FVarMap.Objects[Idx]);
    RegType := TSSARegisterType(PackedInfo shr 16);
    RegIndex := PackedInfo and $FFFF;  // 16-bit mask for 65536 registers
    Result := MakeSSARegister(RegType, RegIndex);
  end
  else
  begin
    // Allocate new register for this variable
    RegType := GetVariableType(VarName);
    RegIndex := FProgram.AllocRegister(RegType);
    // Pack RegType in upper bits, RegIndex in lower 16 bits
    PackedInfo := (Ord(RegType) shl 16) or RegIndex;
    FVarMap.AddObject(VarName, TObject(PackedInfo));
    FProgram.AddVariable(VarName);
    // Map variable to register in TSSAProgram for optimization passes
    FProgram.MapVariableToRegister(VarName, RegType, RegIndex);
    Result := MakeSSARegister(RegType, RegIndex);
  end;
end;

{ PreAllocateVariables - Scan AST to find all BASIC variables and allocate registers
  This separates variable registers from temporary expression registers, preventing conflicts }
procedure TSSAGenerator.PreAllocateVariables(Node: TASTNode);
var
  i: Integer;
  VarName: string;
begin
  if Node = nil then Exit;

  case Node.NodeType of
    antAssignment:
    begin
      // Left side is the variable being assigned
      if (Node.ChildCount > 0) and (Node.GetChild(0).NodeType = antIdentifier) then
      begin
        VarName := VarToStr(Node.GetChild(0).Value);
        GetOrAllocateVariable(VarName);  // Pre-allocate register for this variable
      end;
    end;

    antIdentifier:
    begin
      // This is a variable reference - allocate if not already done
      VarName := VarToStr(Node.Value);
      GetOrAllocateVariable(VarName);
    end;

    antForLoop:
    begin
      // FOR loop variable
      if (Node.ChildCount > 0) and (Node.GetChild(0).NodeType = antIdentifier) then
      begin
        VarName := VarToStr(Node.GetChild(0).Value);
        GetOrAllocateVariable(VarName);
      end;
    end;

    antInput:
    begin
      // INPUT variable
      if (Node.ChildCount > 0) and (Node.GetChild(0).NodeType = antIdentifier) then
      begin
        VarName := VarToStr(Node.GetChild(0).Value);
        GetOrAllocateVariable(VarName);
      end;
    end;
  end;

  // Recursively scan all children
  for i := 0 to Node.ChildCount - 1 do
    PreAllocateVariables(Node.GetChild(i));
end;

procedure TSSAGenerator.EmitInstruction(OpCode: TSSAOpCode; Dest, Src1, Src2, Src3: TSSAValue);
var
  Instr: TSSAInstruction;
begin
  // CRITICAL: After GOTO/RETURN, FCurrentBlock can be nil (block is terminated)
  // Don't emit instructions after block termination
  if not Assigned(FCurrentBlock) then
    Exit;

  Instr := TSSAInstruction.Create(OpCode);
  Instr.Dest := Dest;
  Instr.Src1 := Src1;
  Instr.Src2 := Src2;
  Instr.Src3 := Src3;
  Instr.SourceLine := FCurrentLineNumber;  // Track BASIC line number for error reporting
  FCurrentBlock.AddInstruction(Instr);
end;

// Overload without hint - creates empty hint internally
procedure TSSAGenerator.ProcessExpression(Node: TASTNode; out Result: TSSAValue);
var
  EmptyHint: TSSAValue;
begin
  EmptyHint.Kind := svkNone;
  EmptyHint.RegType := srtFloat;
  EmptyHint.RegIndex := 0;
  ProcessExpression(Node, Result, EmptyHint);
end;

// Main implementation with destination hint
procedure TSSAGenerator.ProcessExpression(Node: TASTNode; out Result: TSSAValue; const DestHint: TSSAValue);
var
  Left, Right: TSSAValue;
  DestReg: Integer;
  OpCode: TSSAOpCode;
  FuncName: string;
  ArgValue, ArgReg: TSSAValue;
  TempReg, IntReg: Integer;
  IntRegVal, TempVal: TSSAValue;
  ArgListNode: TASTNode;
  TempFloat: Double;
  TempInt: Integer;
  // Array access variables
  ArrName: string;
  ArrayIdx: Integer;
  ArrInfo: TSSAArrayInfo;
  IndicesNode: TASTNode;
  Indices: array of TSSAValue;
  i, j: Integer;
  ArrayRef: TSSAValue;
  UseHint: Boolean;
  // Linear index calculation for N-D arrays
  LinearIndex, StrideVal, MulResult, AddResult: TSSAValue;
  Stride: Int64;
  // RGBA function handling
  RVal, GVal, BVal, AVal: TSSAValue;
  RGBAResult: Int64;
  RReg, GReg, BReg, AReg: TSSAValue;
begin
  if Node = nil then
  begin
    Result := MakeSSAValue(svkNone);
    Exit;
  end;

  case Node.NodeType of
    antLiteral:
    begin
      if VarIsNumeric(Node.Value) then
      begin
        if Frac(Double(Node.Value)) = 0 then
          Result := MakeSSAConstInt(Int64(Node.Value))
        else
          Result := MakeSSAConstFloat(Double(Node.Value));
      end
      else
        Result := MakeSSAConstString(VarToStr(Node.Value));
    end;

    antIdentifier:
    begin
      // Return the register assigned to this variable
      Result := GetOrAllocateVariable(VarToStr(Node.Value));
    end;

    antParentheses:
    begin
      // Parentheses just contain an expression - evaluate it
      if Node.ChildCount > 0 then
        ProcessExpression(Node.GetChild(0), Result)
      else
        Result := MakeSSAValue(svkNone);
    end;

    antUnaryOp:
    begin
      // Handle unary operators (currently just negation)
      if Node.ChildCount > 0 then
      begin
        ProcessExpression(Node.GetChild(0), Left);

        // ALWAYS fold negation of constants (this is basic code generation, not optimization)
        // Generating "LoadConst + Neg + Copy" for "-1.16" is wasteful even in unoptimized code
        if Left.Kind = svkConstFloat then
        begin
          // Negate float constant at compile time
          Result := MakeSSAConstFloat(-Left.ConstFloat);
        end
        else if Left.Kind = svkConstInt then
        begin
          // Negate int constant at compile time
          Result := MakeSSAConstInt(-Left.ConstInt);
        end
        else
        begin
          // When constant folding disabled, must materialize constants first
          if Left.Kind = svkConstFloat then
          begin
            TempReg := FProgram.AllocRegister(srtFloat);
            TempVal := MakeSSARegister(srtFloat, TempReg);
            EmitInstruction(ssaLoadConstFloat, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            Left := TempVal;
          end
          else if Left.Kind = svkConstInt then
          begin
            TempReg := FProgram.AllocRegister(srtInt);
            TempVal := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaLoadConstInt, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            Left := TempVal;
          end;

          // Now apply negation to register
          if Left.RegType = srtFloat then
          begin
            DestReg := FProgram.AllocRegister(srtFloat);
            Result := MakeSSARegister(srtFloat, DestReg);
            EmitInstruction(ssaNegFloat, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else
          begin
            // Int register
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            EmitInstruction(ssaNegInt, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end;
        end;
      end
      else
        Result := MakeSSAValue(svkNone);
    end;

    antBinaryOp:
    begin
      ProcessExpression(Node.GetChild(0), Left);
      ProcessExpression(Node.GetChild(1), Right);

      // PHASE 3 TIER 3: Unwrap register-held constants for constant folding
      // If Left is a register holding a constant, treat it as a constant
      if (Left.Kind = svkRegister) and (Left.RegType = srtFloat) then
      begin
        if TryGetConstantFloat(Left.RegIndex, TempFloat) then
          Left := MakeSSAConstFloat(TempFloat);
      end
      else if (Left.Kind = svkRegister) and (Left.RegType = srtInt) then
      begin
        if TryGetConstantInt(Left.RegIndex, TempInt) then
          Left := MakeSSAConstInt(TempInt);
      end;

      // Same for Right operand
      if (Right.Kind = svkRegister) and (Right.RegType = srtFloat) then
      begin
        if TryGetConstantFloat(Right.RegIndex, TempFloat) then
          Right := MakeSSAConstFloat(TempFloat);
      end
      else if (Right.Kind = svkRegister) and (Right.RegType = srtInt) then
      begin
        if TryGetConstantInt(Right.RegIndex, TempInt) then
          Right := MakeSSAConstInt(TempInt);
      end;

      // ALWAYS perform basic algebraic simplification (even with optimizations disabled)
      // This is not an "optimization" but correct code generation - we shouldn't generate
      // instructions for operations that can be resolved at compile time

      // Case 1: Both operands are constants - fold completely
      if (Left.Kind = svkConstFloat) and (Right.Kind = svkConstFloat) then
      begin
        case Node.Token.TokenType of
          ttOpAdd: Result := MakeSSAConstFloat(Left.ConstFloat + Right.ConstFloat);
          ttOpSub: Result := MakeSSAConstFloat(Left.ConstFloat - Right.ConstFloat);
          ttOpMul: Result := MakeSSAConstFloat(Left.ConstFloat * Right.ConstFloat);
          ttOpDiv:
            if Right.ConstFloat <> 0.0 then
              Result := MakeSSAConstFloat(Left.ConstFloat / Right.ConstFloat)
            else
              Result := MakeSSAValue(svkNone);  // Let runtime handle div by zero
        else
          Result := MakeSSAValue(svkNone);
        end;

        if Result.Kind <> svkNone then
          Exit;  // Done - constant folded!
      end
      else if (Left.Kind = svkConstInt) and (Right.Kind = svkConstInt) then
      begin
        case Node.Token.TokenType of
          ttOpAdd: Result := MakeSSAConstInt(Left.ConstInt + Right.ConstInt);
          ttOpSub: Result := MakeSSAConstInt(Left.ConstInt - Right.ConstInt);
          ttOpMul: Result := MakeSSAConstInt(Left.ConstInt * Right.ConstInt);
          ttOpDiv:
            if Right.ConstInt <> 0 then
              Result := MakeSSAConstInt(Left.ConstInt div Right.ConstInt)
            else
              Result := MakeSSAValue(svkNone);
        else
          Result := MakeSSAValue(svkNone);
        end;

        if Result.Kind <> svkNone then
          Exit;  // Done - constant folded!
      end;

      // Case 2: One operand is constant with algebraic identity
      // Mul by 0 = 0, Mul by 1 = other, Add 0 = other, etc.
      if (Left.Kind = svkConstFloat) or (Right.Kind = svkConstFloat) then
      begin
        case Node.Token.TokenType of
          ttOpMul:
          begin
            // 0 * x = 0, x * 0 = 0
            if ((Left.Kind = svkConstFloat) and (Left.ConstFloat = 0.0)) or
               ((Right.Kind = svkConstFloat) and (Right.ConstFloat = 0.0)) then
            begin
              Result := MakeSSAConstFloat(0.0);
              Exit;
            end;
            // 1 * x = x, x * 1 = x
            if (Left.Kind = svkConstFloat) and (Left.ConstFloat = 1.0) then
            begin
              Result := Right;
              Exit;
            end;
            if (Right.Kind = svkConstFloat) and (Right.ConstFloat = 1.0) then
            begin
              Result := Left;
              Exit;
            end;
          end;
          ttOpAdd:
          begin
            // 0 + x = x, x + 0 = x
            if (Left.Kind = svkConstFloat) and (Left.ConstFloat = 0.0) then
            begin
              Result := Right;
              Exit;
            end;
            if (Right.Kind = svkConstFloat) and (Right.ConstFloat = 0.0) then
            begin
              Result := Left;
              Exit;
            end;
          end;
          ttOpSub:
          begin
            // x - 0 = x
            if (Right.Kind = svkConstFloat) and (Right.ConstFloat = 0.0) then
            begin
              Result := Left;
              Exit;
            end;
          end;
        end;
      end;

      // Materialize constants into registers
      if Left.Kind in [svkConstInt, svkConstFloat, svkConstString] then
      begin
        if Left.Kind = svkConstInt then
        begin
          TempReg := FProgram.AllocRegister(srtInt);
          TempVal := MakeSSARegister(srtInt, TempReg);
          EmitInstruction(ssaLoadConstInt, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          FConstIntRegs.AddOrSetValue(TempReg, Left.ConstInt);  // Track constant value
          Left := TempVal;
        end
        else if Left.Kind = svkConstFloat then
        begin
          TempReg := FProgram.AllocRegister(srtFloat);
          TempVal := MakeSSARegister(srtFloat, TempReg);
          EmitInstruction(ssaLoadConstFloat, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          FConstFloatRegs.AddOrSetValue(TempReg, Left.ConstFloat);  // Track constant value
          Left := TempVal;
        end
        else if Left.Kind = svkConstString then
        begin
          TempReg := FProgram.AllocRegister(srtString);
          TempVal := MakeSSARegister(srtString, TempReg);
          EmitInstruction(ssaLoadConstString, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          Left := TempVal;
        end;
      end;

      // Type coercion: convert int to float when mixing types FIRST
      // Check if we need type conversion and fold constants during conversion
      if (Left.RegType = srtFloat) and (Right.Kind = svkConstInt) then
      begin
        // Constant folding: int constant will become float, skip LoadConstInt
        Right := MakeSSAConstFloat(Double(Right.ConstInt));
      end
      else if (Left.Kind = svkConstInt) and (Right.RegType = srtFloat) then
      begin
        // Constant folding: int constant will become float, skip LoadConstInt
        Left := MakeSSAConstFloat(Double(Left.ConstInt));
      end;

      // Load constants into registers (after type coercion check)
      if Right.Kind in [svkConstInt, svkConstFloat, svkConstString] then
      begin
        if Right.Kind = svkConstInt then
        begin
          TempReg := FProgram.AllocRegister(srtInt);
          TempVal := MakeSSARegister(srtInt, TempReg);
          EmitInstruction(ssaLoadConstInt, TempVal, Right, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          FConstIntRegs.AddOrSetValue(TempReg, Right.ConstInt);  // Track constant value
          Right := TempVal;
        end
        else if Right.Kind = svkConstFloat then
        begin
          TempReg := FProgram.AllocRegister(srtFloat);
          TempVal := MakeSSARegister(srtFloat, TempReg);
          EmitInstruction(ssaLoadConstFloat, TempVal, Right, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          FConstFloatRegs.AddOrSetValue(TempReg, Right.ConstFloat);  // Track constant value
          Right := TempVal;
        end
        else if Right.Kind = svkConstString then
        begin
          TempReg := FProgram.AllocRegister(srtString);
          TempVal := MakeSSARegister(srtString, TempReg);
          EmitInstruction(ssaLoadConstString, TempVal, Right, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          Right := TempVal;
        end;
      end;

      // Handle remaining type conversions (non-constant cases)
      if (Left.RegType = srtFloat) and (Right.RegType = srtInt) then
      begin
        TempReg := FProgram.AllocRegister(srtFloat);
        TempVal := MakeSSARegister(srtFloat, TempReg);
        EmitInstruction(ssaIntToFloat, TempVal, Right, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        Right := TempVal;
      end
      else if (Left.RegType = srtInt) and (Right.RegType = srtFloat) then
      begin
        TempReg := FProgram.AllocRegister(srtFloat);
        TempVal := MakeSSARegister(srtFloat, TempReg);
        EmitInstruction(ssaIntToFloat, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        Left := TempVal;
      end;

      // Check if this is a comparison operator (result is always Int boolean)
      case Node.Token.TokenType of
        ttOpEq, ttOpNeq, ttOpLt, ttOpGt, ttOpLe, ttOpGe:
        begin
          // Comparisons always return integer 0 or 1
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);

          // Determine comparison type based on operand types
          if (Left.RegType = srtString) or (Right.RegType = srtString) then
          begin
            // String comparison
            case Node.Token.TokenType of
              ttOpEq: OpCode := ssaCmpEqString;
              ttOpNeq: OpCode := ssaCmpNeString;
              ttOpLt: OpCode := ssaCmpLtString;
              ttOpGt: OpCode := ssaCmpGtString;
            else
              OpCode := ssaCmpEqString;
            end;
          end
          else if (Left.RegType = srtFloat) or (Right.RegType = srtFloat) then
          begin
            // Float comparison
            case Node.Token.TokenType of
              ttOpEq: OpCode := ssaCmpEqFloat;
              ttOpNeq: OpCode := ssaCmpNeFloat;
              ttOpLt: OpCode := ssaCmpLtFloat;
              ttOpGt: OpCode := ssaCmpGtFloat;
              ttOpLe: OpCode := ssaCmpLeFloat;
              ttOpGe: OpCode := ssaCmpGeFloat;
            else
              OpCode := ssaCmpEqFloat;
            end;
          end
          else
          begin
            // Integer comparison
            case Node.Token.TokenType of
              ttOpEq: OpCode := ssaCmpEqInt;
              ttOpNeq: OpCode := ssaCmpNeInt;
              ttOpLt: OpCode := ssaCmpLtInt;
              ttOpGt: OpCode := ssaCmpGtInt;
              ttOpLe: OpCode := ssaCmpLeInt;
              ttOpGe: OpCode := ssaCmpGeInt;
            else
              OpCode := ssaCmpEqInt;
            end;
          end;

          EmitInstruction(OpCode, Result, Left, Right, MakeSSAValue(svkNone));
        end;
      else
        // Arithmetic operators
        // SPECIAL CASE: Division always returns float (even for int/int)
        if Node.Token.TokenType = ttOpDiv then
        begin
          // Convert both operands to float if needed
          if Left.RegType = srtInt then
          begin
            TempReg := FProgram.AllocRegister(srtFloat);
            TempVal := MakeSSARegister(srtFloat, TempReg);
            EmitInstruction(ssaIntToFloat, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            Left := TempVal;
          end;
          if Right.RegType = srtInt then
          begin
            TempReg := FProgram.AllocRegister(srtFloat);
            TempVal := MakeSSARegister(srtFloat, TempReg);
            EmitInstruction(ssaIntToFloat, TempVal, Right, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            Right := TempVal;
          end;

          // Result is always float for division
          DestReg := FProgram.AllocRegister(srtFloat);
          Result := MakeSSARegister(srtFloat, DestReg);
          OpCode := ssaDivFloat;
        end
        // Determine result type and allocate register (use hint if compatible)
        else if (Left.RegType = srtFloat) or (Right.RegType = srtFloat) then
        begin
          // Check if we can use the destination hint
          UseHint := (DestHint.Kind = svkRegister) and (DestHint.RegType = srtFloat);
          if UseHint then
          begin
            Result := DestHint;  // Use the suggested destination
          end
          else
          begin
            DestReg := FProgram.AllocRegister(srtFloat);
            Result := MakeSSARegister(srtFloat, DestReg);
          end;
          case Node.Token.TokenType of
            ttOpAdd: OpCode := ssaAddFloat;
            ttOpSub: OpCode := ssaSubFloat;
            ttOpMul: OpCode := ssaMulFloat;
            ttOpPow: OpCode := ssaPowFloat;
          else
            OpCode := ssaAddFloat;
          end;
        end
        else
        begin
          // Check if we can use the destination hint
          UseHint := (DestHint.Kind = svkRegister) and (DestHint.RegType = srtInt);
          if UseHint then
          begin
            Result := DestHint;  // Use the suggested destination
          end
          else
          begin
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
          end;
          case Node.Token.TokenType of
            ttOpAdd: OpCode := ssaAddInt;
            ttOpSub: OpCode := ssaSubInt;
            ttOpMul: OpCode := ssaMulInt;
            ttOpMod: OpCode := ssaModInt;
          else
            OpCode := ssaAddInt;
          end;
        end;

        EmitInstruction(OpCode, Result, Left, Right, MakeSSAValue(svkNone));
      end;
    end;

    antFunctionCall:
    begin
      // Handle built-in functions
      if Node.ChildCount > 0 then
      begin
        FuncName := UpperCase(VarToStr(Node.Value));
        ArgListNode := Node.GetChild(0);

        // Handle RGBA function specially (4 integer args -> 1 integer result)
        if FuncName = 'RGBA' then
        begin
          // RGBA(r, g, b, a) returns 32-bit integer color value
          // Result = (a << 24) | (r << 16) | (g << 8) | b
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 4) then
          begin
            // Get all 4 arguments as integers
            ProcessExpression(ArgListNode.GetChild(0), RVal);  // R
            ProcessExpression(ArgListNode.GetChild(1), GVal);  // G
            ProcessExpression(ArgListNode.GetChild(2), BVal);  // B
            ProcessExpression(ArgListNode.GetChild(3), AVal);  // A

            // Check if all arguments are constants - if so, compute at compile time
            if (RVal.Kind = svkConstInt) and (GVal.Kind = svkConstInt) and
               (BVal.Kind = svkConstInt) and (AVal.Kind = svkConstInt) then
            begin
              // Constant folding: compute RGBA at compile time
              RGBAResult := ((AVal.ConstInt and $FF) shl 24) or
                            ((RVal.ConstInt and $FF) shl 16) or
                            ((GVal.ConstInt and $FF) shl 8) or
                            (BVal.ConstInt and $FF);

              // Return as integer constant (will be converted to float by caller if needed)
              Result := MakeSSAConstInt(RGBAResult);
            end
            else if (RVal.Kind = svkConstFloat) and (GVal.Kind = svkConstFloat) and
                    (BVal.Kind = svkConstFloat) and (AVal.Kind = svkConstFloat) then
            begin
              // Constant folding for float constants: convert to int and compute
              RGBAResult := ((Trunc(AVal.ConstFloat) and $FF) shl 24) or
                            ((Trunc(RVal.ConstFloat) and $FF) shl 16) or
                            ((Trunc(GVal.ConstFloat) and $FF) shl 8) or
                            (Trunc(BVal.ConstFloat) and $FF);

              // Return as integer constant
              Result := MakeSSAConstInt(RGBAResult);
            end
            else
            begin
              // Non-constant arguments - use RGBA instruction with PhiSources
              // This path may have issues with optimization passes not updating PhiSources
              RReg := EnsureIntRegister(RVal);
              GReg := EnsureIntRegister(GVal);
              BReg := EnsureIntRegister(BVal);
              AReg := EnsureIntRegister(AVal);

              // Allocate result register (integer)
              DestReg := FProgram.AllocRegister(srtInt);
              Result := MakeSSARegister(srtInt, DestReg);

              // Emit RGBA instruction: Dest=result, Src1=R, Src2=G, Src3=B, PhiSources[0]=A
              EmitInstruction(ssaGraphicRGBA, Result, RReg, GReg, BReg);
              FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1].AddPhiSource(AReg, nil);
            end;
          end
          else
            raise Exception.Create('RGBA requires 4 arguments: RGBA(r, g, b, a)');
        end
        else if FuncName = 'RDOT' then
        begin
          // RDOT(n) returns pixel cursor info: 0=x, 1=y, 2=color at PC
          // Returns integer value
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);

            // Convert argument to int register if needed
            if ArgValue.Kind = svkConstInt then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if ArgValue.Kind = svkConstFloat then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(Trunc(ArgValue.ConstFloat)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if (ArgValue.Kind = svkRegister) and (ArgValue.RegType = srtFloat) then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaFloatToInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else
              ArgReg := ArgValue;

            // Allocate result register (integer)
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);

            // Emit ssaGraphicRdot: Dest=result, Src1=which (0/1/2)
            EmitInstruction(ssaGraphicRdot, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else
            raise Exception.Create('RDOT requires 1 argument: RDOT(n) where n=0,1,2');
        end
        else if FuncName = 'RGR' then
        begin
          // RGR(n) returns graphics mode info
          // n=0: current graphics mode (0-11)
          // Future: n=1..n for other info
          // For now, RGR(0) or RGR() returns current mode

          // Check if we have an argument (optional, defaults to 0)
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
            // Convert argument to int register if needed
            if ArgValue.Kind = svkConstInt then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if ArgValue.Kind = svkConstFloat then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(Trunc(ArgValue.ConstFloat)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if (ArgValue.Kind = svkRegister) and (ArgValue.RegType = srtFloat) then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaFloatToInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else
              ArgReg := ArgValue;
          end
          else
          begin
            // No argument - default to 0 (current mode)
            TempReg := FProgram.AllocRegister(srtInt);
            ArgReg := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end;

          // Allocate result register (integer)
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);

          // Emit ssaGraphicGetMode: Dest=result, Src1=which
          EmitInstruction(ssaGraphicGetMode, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else
        begin
          // Standard math functions (single argument, float result)
          if (ArgListNode <> nil) then
          begin
            // If it's an argument list node, get first child
            if (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount > 0) then
              ProcessExpression(ArgListNode.GetChild(0), ArgValue)
            else
              // Otherwise, the node itself is the argument
              ProcessExpression(ArgListNode, ArgValue);
          end
          else
          begin
            // No arguments
            Result := MakeSSAValue(svkNone);
            Exit;
          end;

          // Convert argument to float register if needed
          if ArgValue.Kind = svkConstFloat then
          begin
            TempReg := FProgram.AllocRegister(srtFloat);
            ArgReg := MakeSSARegister(srtFloat, TempReg);
            EmitInstruction(ssaLoadConstFloat, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else if ArgValue.Kind = svkConstInt then
          begin
            TempReg := FProgram.AllocRegister(srtFloat);
            ArgReg := MakeSSARegister(srtFloat, TempReg);
            IntReg := FProgram.AllocRegister(srtInt);
            IntRegVal := MakeSSARegister(srtInt, IntReg);
            EmitInstruction(ssaLoadConstInt, IntRegVal, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            EmitInstruction(ssaIntToFloat, ArgReg, IntRegVal, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else if ArgValue.RegType = srtInt then
          begin
            TempReg := FProgram.AllocRegister(srtFloat);
            ArgReg := MakeSSARegister(srtFloat, TempReg);
            EmitInstruction(ssaIntToFloat, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else
            ArgReg := ArgValue;

          // Allocate result register (float)
          DestReg := FProgram.AllocRegister(srtFloat);
          Result := MakeSSARegister(srtFloat, DestReg);

          // Emit appropriate math function
          if FuncName = 'ABS' then
            OpCode := ssaMathAbs
          else if FuncName = 'SGN' then
            OpCode := ssaMathSgn
          else if FuncName = 'INT' then
            OpCode := ssaMathInt
          else if FuncName = 'SQR' then
            OpCode := ssaMathSqr
          else if FuncName = 'SIN' then
            OpCode := ssaMathSin
          else if FuncName = 'COS' then
            OpCode := ssaMathCos
          else if FuncName = 'TAN' then
            OpCode := ssaMathTan
          else if FuncName = 'EXP' then
            OpCode := ssaMathExp
          else if FuncName = 'LOG' then
            OpCode := ssaMathLog
          else if FuncName = 'RND' then
            OpCode := ssaMathRnd
          else
            OpCode := ssaNop;

          EmitInstruction(OpCode, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end;
      end
      else
        Result := MakeSSAValue(svkNone);
    end;

    antGraphicsFunction:
    begin
      // Handle graphics functions: RGBA, RDOT, RGR
      // Same structure as antFunctionCall
      if Node.ChildCount > 0 then
      begin
        FuncName := UpperCase(VarToStr(Node.Value));
        ArgListNode := Node.GetChild(0);

        // Handle RGBA function specially (4 integer args -> 1 integer result)
        if FuncName = 'RGBA' then
        begin
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 4) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), RVal);
            ProcessExpression(ArgListNode.GetChild(1), GVal);
            ProcessExpression(ArgListNode.GetChild(2), BVal);
            ProcessExpression(ArgListNode.GetChild(3), AVal);

            if (RVal.Kind = svkConstInt) and (GVal.Kind = svkConstInt) and
               (BVal.Kind = svkConstInt) and (AVal.Kind = svkConstInt) then
            begin
              RGBAResult := ((AVal.ConstInt and $FF) shl 24) or
                            ((RVal.ConstInt and $FF) shl 16) or
                            ((GVal.ConstInt and $FF) shl 8) or
                            (BVal.ConstInt and $FF);
              Result := MakeSSAConstInt(RGBAResult);
            end
            else if (RVal.Kind = svkConstFloat) and (GVal.Kind = svkConstFloat) and
                    (BVal.Kind = svkConstFloat) and (AVal.Kind = svkConstFloat) then
            begin
              RGBAResult := ((Trunc(AVal.ConstFloat) and $FF) shl 24) or
                            ((Trunc(RVal.ConstFloat) and $FF) shl 16) or
                            ((Trunc(GVal.ConstFloat) and $FF) shl 8) or
                            (Trunc(BVal.ConstFloat) and $FF);
              Result := MakeSSAConstInt(RGBAResult);
            end
            else
            begin
              RReg := EnsureIntRegister(RVal);
              GReg := EnsureIntRegister(GVal);
              BReg := EnsureIntRegister(BVal);
              AReg := EnsureIntRegister(AVal);
              DestReg := FProgram.AllocRegister(srtInt);
              Result := MakeSSARegister(srtInt, DestReg);
              EmitInstruction(ssaGraphicRGBA, Result, RReg, GReg, BReg);
              FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1].AddPhiSource(AReg, nil);
            end;
          end
          else
            raise Exception.Create('RGBA requires 4 arguments: RGBA(r, g, b, a)');
        end
        else if FuncName = 'RDOT' then
        begin
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
            if ArgValue.Kind = svkConstInt then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if ArgValue.Kind = svkConstFloat then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(Trunc(ArgValue.ConstFloat)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if (ArgValue.Kind = svkRegister) and (ArgValue.RegType = srtFloat) then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaFloatToInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else
              ArgReg := ArgValue;
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            EmitInstruction(ssaGraphicRdot, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else
            raise Exception.Create('RDOT requires 1 argument: RDOT(n) where n=0,1,2');
        end
        else if FuncName = 'RGR' then
        begin
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
            if ArgValue.Kind = svkConstInt then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if ArgValue.Kind = svkConstFloat then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(Trunc(ArgValue.ConstFloat)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if (ArgValue.Kind = svkRegister) and (ArgValue.RegType = srtFloat) then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaFloatToInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else
              ArgReg := ArgValue;
          end
          else
          begin
            TempReg := FProgram.AllocRegister(srtInt);
            ArgReg := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end;
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          WriteLn('>>> SSA RGR: Emitting ssaGraphicGetMode, DestReg=', DestReg, ' ArgReg=', ArgReg.RegIndex);
          EmitInstruction(ssaGraphicGetMode, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else
          Result := MakeSSAValue(svkNone);
      end
      else
        Result := MakeSSAValue(svkNone);
    end;

    antArrayAccess:
    begin
      // Array element access: A(5) or M(I,J)
      // Structure: antArrayAccess
      //   antIdentifier: array name
      //   antExpressionList: indices

      if Node.ChildCount < 2 then
        begin
          Result := MakeSSAValue(svkNone);
          Exit;
        end;

        // First child is array name
        if Node.GetChild(0).NodeType <> antIdentifier then
        begin
          Result := MakeSSAValue(svkNone);
          Exit;
        end;

        ArrName := VarToStr(Node.GetChild(0).Value);
        ArrayIdx := FProgram.FindArray(ArrName);
        if ArrayIdx < 0 then
          raise Exception.CreateFmt('Array not declared: %s', [ArrName]);

        ArrInfo := FProgram.GetArray(ArrayIdx);
        IndicesNode := Node.GetChild(1);  // antExpressionList

        // Evaluate each index expression
        SetLength(Indices, IndicesNode.ChildCount);
        for i := 0 to IndicesNode.ChildCount - 1 do
        begin
          ProcessExpression(IndicesNode.GetChild(i), Indices[i]);

          // Materialize constants into int registers and convert float registers to int
          if Indices[i].Kind = svkConstInt then
          begin
            TempReg := FProgram.AllocRegister(srtInt);
            TempVal := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaLoadConstInt, TempVal, Indices[i], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            Indices[i] := TempVal;
          end
          else if Indices[i].Kind = svkConstFloat then
          begin
            // Convert float constant to int
            TempReg := FProgram.AllocRegister(srtInt);
            TempVal := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaLoadConstInt, TempVal, MakeSSAConstInt(Trunc(Indices[i].ConstFloat)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            Indices[i] := TempVal;
          end
          else if (Indices[i].Kind = svkRegister) and (Indices[i].RegType = srtFloat) then
          begin
            // Convert float register to int (FOR loop counters are float)
            TempReg := FProgram.AllocRegister(srtInt);
            TempVal := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaFloatToInt, TempVal, Indices[i], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            Indices[i] := TempVal;
          end;
        end;

        // Calculate linear index at compile-time using row-major order formula:
        // LinearIndex = i0 * (d1*d2*...*dn) + i1 * (d2*...*dn) + ... + i(n-1)
        // This eliminates the need for consecutive registers at runtime
        if Length(Indices) = 1 then
        begin
          // 1D array: linear index = first index
          LinearIndex := Indices[0];
        end
        else
        begin
          // N-D array: compute linear index
          // Start with index 0 (will accumulate the sum)
          TempReg := FProgram.AllocRegister(srtInt);
          LinearIndex := MakeSSARegister(srtInt, TempReg);
          EmitInstruction(ssaLoadConstInt, LinearIndex, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

          for i := 0 to High(Indices) do
          begin
            // Calculate stride for dimension i: product of dimensions i+1 to n-1
            // NOTE: ArrInfo.Dimensions[j] already contains the actual size (N+1 for DIM A(N))
            Stride := 1;
            for j := i + 1 to High(ArrInfo.Dimensions) do
              Stride := Stride * ArrInfo.Dimensions[j];

            if Stride = 1 then
            begin
              // Last dimension: just add the index
              TempReg := FProgram.AllocRegister(srtInt);
              AddResult := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaAddInt, AddResult, LinearIndex, Indices[i], MakeSSAValue(svkNone));
              LinearIndex := AddResult;
            end
            else
            begin
              // Multiply index by stride, then add to accumulator
              TempReg := FProgram.AllocRegister(srtInt);
              StrideVal := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, StrideVal, MakeSSAConstInt(Stride), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

              TempReg := FProgram.AllocRegister(srtInt);
              MulResult := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaMulInt, MulResult, Indices[i], StrideVal, MakeSSAValue(svkNone));

              TempReg := FProgram.AllocRegister(srtInt);
              AddResult := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaAddInt, AddResult, LinearIndex, MulResult, MakeSSAValue(svkNone));
              LinearIndex := AddResult;
            end;
          end;
        end;

        // Allocate result register with array element type
        DestReg := FProgram.AllocRegister(ArrInfo.ElementType);
        Result := MakeSSARegister(ArrInfo.ElementType, DestReg);
        ArrayRef := MakeSSAArrayRef(ArrayIdx, ArrInfo.ElementType);

        // Emit ssaArrayLoad instruction with pre-computed linear index
        // Dest, ArrayRef, LinearIndex, None
        EmitInstruction(ssaArrayLoad, Result, ArrayRef, LinearIndex, MakeSSAValue(svkNone));
    end;

  else
    Result := MakeSSAValue(svkNone);
  end;
end;

procedure TSSAGenerator.ProcessAssignment(Node: TASTNode);
var
  VarNode, ExprNode: TASTNode;
  VarName: string;
  ExprValue, VarReg: TSSAValue;
  CopyOp: TSSAOpCode;
begin
  if Node.ChildCount < 2 then Exit;

  VarNode := Node.GetChild(0);
  ExprNode := Node.GetChild(1);

  // Check if target is array access (array store)
  if VarNode.NodeType = antArrayAccess then
  begin
    ProcessArrayStore(Node);
    Exit;
  end;

  if VarNode.NodeType <> antIdentifier then Exit;
  VarName := VarToStr(VarNode.Value);

  // Get or allocate register for this variable
  VarReg := GetOrAllocateVariable(VarName);

  // Evaluate expression with destination hint to avoid unnecessary copies
  ProcessExpression(ExprNode, ExprValue, VarReg);

  // If expression result is a constant, load it directly to variable register
  if ExprValue.Kind in [svkConstInt, svkConstFloat, svkConstString] then
  begin
    // Check if type conversion is needed
    if (ExprValue.Kind = svkConstInt) and (VarReg.RegType = srtFloat) then
    begin
      // Convert int constant to float constant at compile time and load directly
      // This eliminates LoadConstInt + IntToFloat pattern
      ExprValue := MakeSSAConstFloat(Double(ExprValue.ConstInt));
      EmitInstruction(ssaLoadConstFloat, VarReg, ExprValue,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
    begin
      // Direct load
      case ExprValue.Kind of
        svkConstInt:
          EmitInstruction(ssaLoadConstInt, VarReg, ExprValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        svkConstFloat:
          EmitInstruction(ssaLoadConstFloat, VarReg, ExprValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        svkConstString:
          EmitInstruction(ssaLoadConstString, VarReg, ExprValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end;
  end
  else if ExprValue.RegIndex <> VarReg.RegIndex then
  begin
    // Expression result is in a different register than the variable
    // Need to copy or convert
    if (ExprValue.RegType = srtInt) and (VarReg.RegType = srtFloat) then
    begin
      // Convert INT register to FLOAT
      EmitInstruction(ssaIntToFloat, VarReg, ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if (ExprValue.RegType = srtFloat) and (VarReg.RegType = srtInt) then
    begin
      // Convert FLOAT register to INT
      EmitInstruction(ssaFloatToInt, VarReg, ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
    begin
      // Same type - direct copy (only if registers are different!)
      case VarReg.RegType of
        srtInt: CopyOp := ssaCopyInt;
        srtFloat: CopyOp := ssaCopyFloat;
        srtString: CopyOp := ssaCopyString;
      end;
      EmitInstruction(CopyOp, VarReg, ExprValue,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end;
  // else: ExprValue is already in VarReg thanks to hint - no copy needed!
end;

procedure TSSAGenerator.ProcessPrint(Node: TASTNode);
var
  i: Integer;
  ExprValue, RegValue: TSSAValue;
  DestReg: Integer;
  Child: TASTNode;
  SeparatorChar: string;
  EndsWithSeparator: Boolean;
begin
  // Handle empty PRINT statement (just newline)
  if Node.ChildCount = 0 then
  begin
    EmitInstruction(ssaPrintNewLine, MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    Exit;
  end;

  // Check if PRINT ends with a separator (;  or ,)
  // This determines if we emit a final newline or not
  EndsWithSeparator := (Node.GetChild(Node.ChildCount - 1).NodeType = antSeparator);

  // Process each child
  for i := 0 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);

    // Handle separator nodes - emit appropriate separator instruction
    if Child.NodeType = antSeparator then
    begin
      SeparatorChar := VarToStr(Child.Value);
      if SeparatorChar = ',' then
        EmitInstruction(ssaPrintComma, MakeSSAValue(svkNone),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else if SeparatorChar = ';' then
        EmitInstruction(ssaPrintSemicolon, MakeSSAValue(svkNone),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      Continue;
    end;

    ProcessExpression(Child, ExprValue);

    // If expression is a constant, load it into a register first
    if ExprValue.Kind in [svkConstInt, svkConstFloat, svkConstString] then
    begin
      // Allocate register based on constant type
      DestReg := FProgram.AllocRegister(ExprValue.RegType);
      RegValue := MakeSSARegister(ExprValue.RegType, DestReg);

      // Emit load constant instruction
      case ExprValue.Kind of
        svkConstInt:
        begin
          EmitInstruction(ssaLoadConstInt, RegValue, ExprValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          ExprValue := RegValue;
        end;
        svkConstFloat:
        begin
          EmitInstruction(ssaLoadConstFloat, RegValue, ExprValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          ExprValue := RegValue;
        end;
        svkConstString:
        begin
          EmitInstruction(ssaLoadConstString, RegValue, ExprValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          ExprValue := RegValue;
        end;
      end;
    end;

    // Emit appropriate print instruction based on type
    case ExprValue.RegType of
      srtInt:
        EmitInstruction(ssaPrintInt, MakeSSAValue(svkNone), ExprValue,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      srtFloat:
        EmitInstruction(ssaPrint, MakeSSAValue(svkNone), ExprValue,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      srtString:
        EmitInstruction(ssaPrintString, MakeSSAValue(svkNone), ExprValue,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end;

  // Emit newline ONLY if PRINT does NOT end with a separator
  // PRINT A     -> newline
  // PRINT A;    -> no newline
  // PRINT A,    -> no newline
  // PRINT A; B  -> newline (separator is between, not at end)
  if not EndsWithSeparator then
    EmitInstruction(ssaPrintNewLine, MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessInput(Node: TASTNode);
var
  i: Integer;
  Child: TASTNode;
  PromptStr: string;
  PromptReg, VarReg: TSSAValue;
  PromptRegIdx: Integer;
  VarName: string;
begin
  // INPUT has structure: optional prompt string, separator, variable name
  // Examples:
  // INPUT "Enter value: "; N    -> prompt + variable
  // INPUT N                      -> no prompt, just variable

  PromptStr := '';
  VarName := '';

  // Scan children to find prompt (if any) and variable name
  for i := 0 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);

    case Child.NodeType of
      antLiteral:
      begin
        // This is the prompt string
        PromptStr := VarToStr(Child.Value);
      end;
      antIdentifier:
      begin
        // This is the variable to store input
        VarName := VarToStr(Child.Value);
      end;
      antSeparator:
        // Skip separators
        Continue;
    end;
  end;

  // If we don't have a variable name, nothing to do
  if VarName = '' then Exit;

  // Get or allocate register for the target variable
  VarReg := GetOrAllocateVariable(VarName);

  // If there's a prompt, emit instruction to print it first
  if PromptStr <> '' then
  begin
    PromptRegIdx := FProgram.AllocRegister(srtString);
    PromptReg := MakeSSARegister(srtString, PromptRegIdx);
    EmitInstruction(ssaLoadConstString, PromptReg, MakeSSAConstString(PromptStr),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    EmitInstruction(ssaPrintString, MakeSSAValue(svkNone), PromptReg,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit INPUT instruction based on variable type
  // Dest = variable register where input will be stored
  // No source operands needed - INPUT reads from stdin
  case VarReg.RegType of
    srtInt:
      EmitInstruction(ssaInputInt, VarReg, MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    srtFloat:
      EmitInstruction(ssaInputFloat, VarReg, MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    srtString:
      EmitInstruction(ssaInputString, VarReg, MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;
end;

procedure TSSAGenerator.ProcessArrayStore(Node: TASTNode);
var
  TargetNode, ExprNode: TASTNode;
  ArrName: string;
  ArrayIdx: Integer;
  ArrInfo: TSSAArrayInfo;
  IndicesNode: TASTNode;
  Indices: array of TSSAValue;
  i, j, TempReg: Integer;
  ArrayRef, ExprValue, TempVal, LinearIndex, StrideVal, MulResult, AddResult: TSSAValue;
  FloatVal: Double;
  IntVal: Int64;
  Stride: Int64;
begin
  // Array store: A(5) = 10
  // Node structure: antAssignment
  //   antArrayAccess
  //     antIdentifier: array name
  //     antExpressionList: indices
  //   expression (value to store)

  if Node.ChildCount < 2 then Exit;

  TargetNode := Node.GetChild(0);  // antArrayAccess
  ExprNode := Node.GetChild(1);     // Value expression

  if TargetNode.NodeType <> antArrayAccess then Exit;
  if TargetNode.ChildCount < 2 then Exit;

  // Get array name
  if TargetNode.GetChild(0).NodeType <> antIdentifier then Exit;
  ArrName := VarToStr(TargetNode.GetChild(0).Value);

  // Find array in program
  ArrayIdx := FProgram.FindArray(ArrName);
  if ArrayIdx < 0 then
    raise Exception.CreateFmt('Array not declared: %s', [ArrName]);

  ArrInfo := FProgram.GetArray(ArrayIdx);
  IndicesNode := TargetNode.GetChild(1);  // antExpressionList

  // Evaluate each index expression
  SetLength(Indices, IndicesNode.ChildCount);
  for i := 0 to IndicesNode.ChildCount - 1 do
  begin
    ProcessExpression(IndicesNode.GetChild(i), Indices[i]);

    // Materialize constants into int registers and convert float registers to int
    if Indices[i].Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, TempVal, Indices[i], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      Indices[i] := TempVal;
    end
    else if Indices[i].Kind = svkConstFloat then
    begin
      // Convert float constant to int
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, TempVal, MakeSSAConstInt(Trunc(Indices[i].ConstFloat)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      Indices[i] := TempVal;
    end
    else if (Indices[i].Kind = svkRegister) and (Indices[i].RegType = srtFloat) then
    begin
      // Convert float register to int (FOR loop counters are float)
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaFloatToInt, TempVal, Indices[i], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      Indices[i] := TempVal;
    end;
  end;

  // Calculate linear index at compile-time using row-major order formula:
  // LinearIndex = i0 * (d1*d2*...*dn) + i1 * (d2*...*dn) + ... + i(n-1)
  // This eliminates the need for consecutive registers at runtime
  if Length(Indices) = 1 then
  begin
    // 1D array: linear index = first index
    LinearIndex := Indices[0];
  end
  else
  begin
    // N-D array: compute linear index
    // Start with index 0 (will accumulate the sum)
    TempReg := FProgram.AllocRegister(srtInt);
    LinearIndex := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, LinearIndex, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    for i := 0 to High(Indices) do
    begin
      // Calculate stride for dimension i: product of dimensions i+1 to n-1
      // NOTE: ArrInfo.Dimensions[j] already contains the actual size (N+1 for DIM A(N))
      Stride := 1;
      for j := i + 1 to High(ArrInfo.Dimensions) do
        Stride := Stride * ArrInfo.Dimensions[j];

      if Stride = 1 then
      begin
        // Last dimension: just add the index
        TempReg := FProgram.AllocRegister(srtInt);
        AddResult := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaAddInt, AddResult, LinearIndex, Indices[i], MakeSSAValue(svkNone));
        LinearIndex := AddResult;
      end
      else
      begin
        // Multiply index by stride, then add to accumulator
        TempReg := FProgram.AllocRegister(srtInt);
        StrideVal := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaLoadConstInt, StrideVal, MakeSSAConstInt(Stride), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

        TempReg := FProgram.AllocRegister(srtInt);
        MulResult := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaMulInt, MulResult, Indices[i], StrideVal, MakeSSAValue(svkNone));

        TempReg := FProgram.AllocRegister(srtInt);
        AddResult := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaAddInt, AddResult, LinearIndex, MulResult, MakeSSAValue(svkNone));
        LinearIndex := AddResult;
      end;
    end;
  end;

  // Evaluate value expression
  ProcessExpression(ExprNode, ExprValue);

  {$IFNDEF DISABLE_CONSTANT_FOLDING}
  // OPTIMIZATION: Constant folding for type conversions
  // Convert constants at compile-time instead of emitting runtime instructions
  if ExprValue.Kind = svkConstInt then
  begin
    // Float array + Int constant → convert at compile-time
    if ArrInfo.ElementType = srtFloat then
    begin
      FloatVal := Double(ExprValue.ConstInt);
      ExprValue := MakeSSAValue(svkConstFloat);
      ExprValue.ConstFloat := FloatVal;
    end;
  end
  else if ExprValue.Kind = svkConstFloat then
  begin
    // Int array + Float constant → convert at compile-time
    if ArrInfo.ElementType = srtInt then
    begin
      IntVal := Trunc(ExprValue.ConstFloat);
      ExprValue := MakeSSAValue(svkConstInt);
      ExprValue.ConstInt := IntVal;
    end;
  end;
  {$ENDIF}

  // Materialize constant value into register
  if ExprValue.Kind = svkConstInt then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    TempVal := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, TempVal, ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    ExprValue := TempVal;
  end
  else if ExprValue.Kind = svkConstFloat then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    TempVal := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, TempVal, ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    ExprValue := TempVal;
  end
  else if ExprValue.Kind = svkConstString then
  begin
    TempReg := FProgram.AllocRegister(srtString);
    TempVal := MakeSSARegister(srtString, TempReg);
    EmitInstruction(ssaLoadConstString, TempVal, ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    ExprValue := TempVal;
  end;

  // Type conversion: value must match array element type
  if ExprValue.Kind = svkRegister then
  begin
    // Float array + Int value → convert Int to Float
    if (ArrInfo.ElementType = srtFloat) and (ExprValue.RegType = srtInt) then
    begin
      TempReg := FProgram.AllocRegister(srtFloat);
      TempVal := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaIntToFloat, TempVal, ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      ExprValue := TempVal;
    end
    // Int array + Float value → convert Float to Int
    else if (ArrInfo.ElementType = srtInt) and (ExprValue.RegType = srtFloat) then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaFloatToInt, TempVal, ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      ExprValue := TempVal;
    end;
  end;

  ArrayRef := MakeSSAArrayRef(ArrayIdx, ArrInfo.ElementType);

  // Emit ssaArrayStore instruction with pre-computed linear index
  // Dest = value to store
  // Src1 = ArrayRef
  // Src2 = LinearIndex (single pre-computed index)
  // Src3 = None (no longer needed - linear index handles all dimensions)
  EmitInstruction(ssaArrayStore, ExprValue, ArrayRef, LinearIndex, MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessDim(Node: TASTNode);
var
  ArrName: string;
  ElementType: TSSARegisterType;
  DimsNode, DimExpr, ArrayDeclNode: TASTNode;
  Dimensions: array of Integer;
  DimCount, i, j, ArrayIdx: Integer;
  DimValue: TSSAValue;
  DimValues: array of TSSAValue;  // Store dimension values (registers or constants)
  DimRegs: array of Integer;      // Register indices for variable dimensions
  DimRegTypes: array of TSSARegisterType;  // Register types for variable dimensions
  ArrayRef: TSSAValue;
  TotalElements: Int64;
  HasVariableDims: Boolean;
const
  MAX_ARRAY_ELEMENTS = 125000000;  // 125M elements max (~1GB for 500x500x500 matrix)
begin
  // DIM can declare multiple arrays:
  // DIM A(10)                    -> single array
  // DIM A1(10), A2(10, 10)       -> multiple arrays
  // DIM C(5, 5, 5, 5, 5)         -> multidimensional (up to 13 dimensions like C128)

  if Node.ChildCount < 1 then Exit;

  // Parser generates AST structure:
  // antDim -> antArrayDecl(s)
  // Each antArrayDecl -> antIdentifier, antDimensions

  // Process all array declarations (support for multiple arrays in one DIM)
  for j := 0 to Node.ChildCount - 1 do
  begin
    ArrayDeclNode := Node.GetChild(j);

    // Only process antArrayDecl nodes (modern AST format)
    if ArrayDeclNode.NodeType <> antArrayDecl then
      Continue;

    // antArrayDecl contains identifier and dimensions
    if ArrayDeclNode.ChildCount < 2 then Continue;
    if ArrayDeclNode.GetChild(0).NodeType <> antIdentifier then Continue;

    ArrName := VarToStr(ArrayDeclNode.GetChild(0).Value);
    DimsNode := ArrayDeclNode.GetChild(1);

    // Determine element type from array name suffix
    ElementType := GetVariableType(ArrName);

    // Validate dimensions node
    if DimsNode.NodeType <> antDimensions then
      raise Exception.CreateFmt('Invalid array dimensions for: %s', [ArrName]);

    // Extract dimension sizes
    DimCount := DimsNode.ChildCount;

    // Safety check: must have at least one dimension
    if DimCount < 1 then
      raise Exception.CreateFmt('Array must have at least 1 dimension: %s', [ArrName]);

    SetLength(Dimensions, DimCount);
    SetLength(DimValues, DimCount);
    TotalElements := 1;

    for i := 0 to DimCount - 1 do
    begin
      DimExpr := DimsNode.GetChild(i);
      ProcessExpression(DimExpr, DimValue);

      // Store dimension value for VM (needed for variable dimensions)
      DimValues[i] := DimValue;

      // BASIC semantics: DIM A(N) allocates N+1 elements [0..N]
      // Dimensions can be constants or variables - evaluated once at DIM execution
      if DimValue.Kind = svkConstInt then
      begin
        Dimensions[i] := Integer(DimValue.ConstInt) + 1;

        // Safety check for constant dimensions
        if Dimensions[i] <= 0 then
          raise Exception.CreateFmt('Array dimension must be positive: %s[%d] = %d', [ArrName, i, Dimensions[i]-1]);

        // Calculate total elements (with overflow check)
        TotalElements := TotalElements * Dimensions[i];
        if TotalElements > MAX_ARRAY_ELEMENTS then
          raise Exception.CreateFmt('Array %s too large: %d elements (max %d)', [ArrName, TotalElements, MAX_ARRAY_ELEMENTS]);
      end
      else if DimValue.Kind = svkConstFloat then
      begin
        Dimensions[i] := Integer(Trunc(DimValue.ConstFloat)) + 1;

        if Dimensions[i] <= 0 then
          raise Exception.CreateFmt('Array dimension must be positive: %s[%d] = %d', [ArrName, i, Dimensions[i]-1]);

        TotalElements := TotalElements * Dimensions[i];
        if TotalElements > MAX_ARRAY_ELEMENTS then
          raise Exception.CreateFmt('Array %s too large: %d elements (max %d)', [ArrName, TotalElements, MAX_ARRAY_ELEMENTS]);
      end
      else if DimValue.Kind = svkRegister then
      begin
        // Variable dimension: DIM A(S%) - VM will read value from register
        // Use 0 as placeholder - actual size determined at runtime
        Dimensions[i] := 0;
        // Cannot validate size at compile time
        TotalElements := -1;
      end
      else
        raise Exception.CreateFmt('Invalid array dimension expression: %s', [ArrName]);
    end;

    // Declare array in SSA program
    ArrayIdx := FProgram.DeclareArray(ArrName, ElementType, Dimensions);

    // Store dimension registers (for any variable dimensions, even if mixed with constants)
    SetLength(DimRegs, DimCount);
    SetLength(DimRegTypes, DimCount);
    HasVariableDims := False;
    for i := 0 to DimCount - 1 do
    begin
      if DimValues[i].Kind = svkRegister then
      begin
        DimRegs[i] := DimValues[i].RegIndex;
        DimRegTypes[i] := DimValues[i].RegType;
        HasVariableDims := True;
      end
      else
      begin
        DimRegs[i] := -1;  // Not a register (constant)
        DimRegTypes[i] := srtInt;
      end;
    end;

    // Only store registers if at least one dimension is variable
    if HasVariableDims then
      FProgram.SetArrayDimRegisters(ArrayIdx, DimRegs, DimRegTypes);

    // Emit ssaArrayDim instruction
    // Dest = not used
    // Src1 = array reference (contains metadata index)
    // VM will use Src1.ArrayIndex to look up metadata and allocate
    ArrayRef := MakeSSAArrayRef(ArrayIdx, ElementType);
    EmitInstruction(ssaArrayDim, MakeSSAValue(svkNone), ArrayRef,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;
end;

procedure TSSAGenerator.ProcessForLoop(Node: TASTNode);
var
  VarName: string;
  StartValue, EndValue, StepValue: TSSAValue;
  VarReg: TSSAValue;
  CondLabel, BodyLabel, EndLabel: string;
  CmpReg: Integer;
  LoopInfo: TLoopInfo;
  TempReg: Integer;
  TempVal: TSSAValue;
  PrevBlock, CondBlock, BodyBlock, GEBlock: TSSABasicBlock;  // PHASE 3 TIER 3: CFG construction
  StepNode: TASTNode;
  StepIsNegative: Boolean;
  StepSignReg: Integer;  // Register to hold STEP sign check result
  UseLELabel, UseGELabel, CheckResultLabel: string;  // Labels for runtime step direction
  NeedRuntimeCheck: Boolean;  // True if STEP direction must be determined at runtime
begin
  if Node.ChildCount < 3 then Exit;
  if Node.GetChild(0).NodeType <> antIdentifier then Exit;

  VarName := VarToStr(Node.GetChild(0).Value);
  VarReg := GetOrAllocateVariable(VarName);

  // Evaluate start, end, and step values
  ProcessExpression(Node.GetChild(1), StartValue);
  ProcessExpression(Node.GetChild(2), EndValue);

  // Check if STEP is negative by examining AST node
  StepIsNegative := False;
  NeedRuntimeCheck := False;
  if Node.ChildCount > 3 then
  begin
    StepNode := Node.GetChild(3);
    // Check if STEP is a unary minus (negative constant or expression)
    if StepNode.NodeType = antUnaryOp then
      StepIsNegative := True;
    ProcessExpression(StepNode, StepValue);

    // Also check if the evaluated STEP value is a negative constant
    // This handles cases where STEP is a variable containing a negative value
    if not StepIsNegative then
    begin
      if (StepValue.Kind = svkConstInt) and (StepValue.ConstInt < 0) then
        StepIsNegative := True
      else if (StepValue.Kind = svkConstFloat) and (StepValue.ConstFloat < 0) then
        StepIsNegative := True
      else if StepValue.Kind = svkRegister then
        // STEP is a runtime variable - we need runtime check to determine direction
        NeedRuntimeCheck := True;
    end;
  end
  else
    StepValue := MakeSSAConstInt(1);  // Default step = 1

  // Initialize loop variable with start value (do this first!)
  // Convert to match VarReg type to avoid register collision
  if StartValue.Kind = svkConstInt then
  begin
    if VarReg.RegType = srtFloat then
      EmitInstruction(ssaLoadConstFloat, VarReg, MakeSSAConstFloat(StartValue.ConstInt), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaLoadConstInt, VarReg, StartValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if StartValue.Kind = svkConstFloat then
    EmitInstruction(ssaLoadConstFloat, VarReg, StartValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
  else if StartValue.Kind = svkRegister then
  begin
    // StartValue is in a register - must convert type to match VarReg if needed
    if (StartValue.RegType = srtInt) and (VarReg.RegType = srtFloat) then
    begin
      // Convert int StartValue to float to match VarReg
      TempReg := FProgram.AllocRegister(srtFloat);
      TempVal := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaIntToFloat, TempVal, StartValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EmitInstruction(ssaCopyFloat, VarReg, TempVal, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if (StartValue.RegType = srtFloat) and (VarReg.RegType = srtInt) then
    begin
      // Convert float StartValue to int to match VarReg
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaFloatToInt, TempVal, StartValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EmitInstruction(ssaCopyInt, VarReg, TempVal, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
    begin
      // Same type, just copy
      if VarReg.RegType = srtInt then
        EmitInstruction(ssaCopyInt, VarReg, StartValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else
        EmitInstruction(ssaCopyFloat, VarReg, StartValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end;

  // Materialize constants into registers for EndValue and StepValue (after var init!)
  // Convert to same type as VarReg for proper comparison
  if EndValue.Kind = svkConstInt then
  begin
    if VarReg.RegType = srtFloat then
    begin
      // Convert int constant to float
      TempReg := FProgram.AllocRegister(srtFloat);
      TempVal := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaLoadConstFloat, TempVal, MakeSSAConstFloat(EndValue.ConstInt),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EndValue := TempVal;
    end
    else
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, TempVal, EndValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EndValue := TempVal;
    end;
  end
  else if EndValue.Kind = svkConstFloat then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    TempVal := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, TempVal, EndValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    EndValue := TempVal;
  end
  else if EndValue.Kind = svkRegister then
  begin
    // EndValue is already in a register (e.g., result of sqr(n))
    // MUST copy to a dedicated register to prevent overwriting during loop
    // AND convert type to match VarReg if needed
    if (EndValue.RegType = srtInt) and (VarReg.RegType = srtFloat) then
    begin
      // Convert int EndValue to float to match VarReg
      TempReg := FProgram.AllocRegister(srtFloat);
      TempVal := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaIntToFloat, TempVal, EndValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EndValue := TempVal;
    end
    else if (EndValue.RegType = srtFloat) and (VarReg.RegType = srtInt) then
    begin
      // Convert float EndValue to int to match VarReg
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaFloatToInt, TempVal, EndValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EndValue := TempVal;
    end
    else
    begin
      // Same type, just copy
      TempReg := FProgram.AllocRegister(EndValue.RegType);
      TempVal := MakeSSARegister(EndValue.RegType, TempReg);
      if EndValue.RegType = srtInt then
        EmitInstruction(ssaCopyInt, TempVal, EndValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else
        EmitInstruction(ssaCopyFloat, TempVal, EndValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EndValue := TempVal;
    end;
  end;

  if StepValue.Kind = svkConstInt then
  begin
    if VarReg.RegType = srtFloat then
    begin
      // Convert int constant to float
      TempReg := FProgram.AllocRegister(srtFloat);
      TempVal := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaLoadConstFloat, TempVal, MakeSSAConstFloat(StepValue.ConstInt),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      StepValue := TempVal;
    end
    else
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, TempVal, StepValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      StepValue := TempVal;
    end;
  end
  else if StepValue.Kind = svkConstFloat then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    TempVal := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, TempVal, StepValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    StepValue := TempVal;
  end
  else if StepValue.Kind = svkRegister then
  begin
    // StepValue is already in a register
    // MUST copy to a dedicated register to prevent overwriting during loop
    // AND convert type to match VarReg if needed
    if (StepValue.RegType = srtInt) and (VarReg.RegType = srtFloat) then
    begin
      // Convert int StepValue to float to match VarReg
      TempReg := FProgram.AllocRegister(srtFloat);
      TempVal := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaIntToFloat, TempVal, StepValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      StepValue := TempVal;
    end
    else if (StepValue.RegType = srtFloat) and (VarReg.RegType = srtInt) then
    begin
      // Convert float StepValue to int to match VarReg
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaFloatToInt, TempVal, StepValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      StepValue := TempVal;
    end
    else
    begin
      // Same type, just copy
      TempReg := FProgram.AllocRegister(StepValue.RegType);
      TempVal := MakeSSARegister(StepValue.RegType, TempReg);
      if StepValue.RegType = srtInt then
        EmitInstruction(ssaCopyInt, TempVal, StepValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else
        EmitInstruction(ssaCopyFloat, TempVal, StepValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      StepValue := TempVal;
    end;
  end;

  // Generate labels
  BodyLabel := GenerateUniqueLabel('for_body');
  EndLabel := GenerateUniqueLabel('for_end');

  // PHASE 3 TIER 3: Save current block before creating new ones
  PrevBlock := FCurrentBlock;

  if NeedRuntimeCheck then
  begin
    // STEP sign unknown at compile-time - generate runtime check and two separate paths

    // Check STEP < 0
    // Materialize zero constant into a register first
    TempReg := FProgram.AllocRegister(VarReg.RegType);
    TempVal := MakeSSARegister(VarReg.RegType, TempReg);
    if VarReg.RegType = srtInt then
      EmitInstruction(ssaLoadConstInt, TempVal, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaLoadConstFloat, TempVal, MakeSSAConstFloat(0.0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    StepSignReg := FProgram.AllocRegister(VarReg.RegType);
    if VarReg.RegType = srtInt then
      EmitInstruction(ssaCmpLtInt, MakeSSARegister(VarReg.RegType, StepSignReg), StepValue, TempVal, MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaCmpLtFloat, MakeSSARegister(VarReg.RegType, StepSignReg), StepValue, TempVal, MakeSSAValue(svkNone));

    // Generate two condition labels
    UseLELabel := GenerateUniqueLabel('for_cond_le');
    UseGELabel := GenerateUniqueLabel('for_cond_ge');

    // Jump to GE label if STEP < 0
    EmitInstruction(ssaJumpIfNotZero, MakeSSALabel(UseGELabel), MakeSSARegister(VarReg.RegType, StepSignReg),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // ===== LE Condition Block (STEP >= 0) =====
    FCurrentBlock := FProgram.CreateBlock(UseLELabel);
    CondBlock := FCurrentBlock;
    CondLabel := UseLELabel;

    // PHASE 3 TIER 3: Connect edge PrevBlock → LE CondBlock
    if Assigned(PrevBlock) then
    begin
      PrevBlock.AddSuccessor(CondBlock);
      CondBlock.AddPredecessor(PrevBlock);
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
        WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', CondBlock.LabelName);
      {$ENDIF}
    end;

    CmpReg := FProgram.AllocRegister(VarReg.RegType);
    if VarReg.RegType = srtInt then
      EmitInstruction(ssaCmpLeInt, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaCmpLeFloat, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone));
    EmitInstruction(ssaJumpIfZero, MakeSSALabel(EndLabel), MakeSSARegister(VarReg.RegType, CmpReg),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    EmitInstruction(ssaJump, MakeSSALabel(BodyLabel), MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // ===== GE Condition Block (STEP < 0) =====
    FCurrentBlock := FProgram.CreateBlock(UseGELabel);
    GEBlock := FCurrentBlock;

    // PHASE 3 TIER 3: Connect edge PrevBlock → GE CondBlock
    if Assigned(PrevBlock) then
    begin
      PrevBlock.AddSuccessor(GEBlock);
      GEBlock.AddPredecessor(PrevBlock);
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
        WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', GEBlock.LabelName);
      {$ENDIF}
    end;

    CmpReg := FProgram.AllocRegister(VarReg.RegType);
    if VarReg.RegType = srtInt then
      EmitInstruction(ssaCmpGeInt, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaCmpGeFloat, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone));
    EmitInstruction(ssaJumpIfZero, MakeSSALabel(EndLabel), MakeSSARegister(VarReg.RegType, CmpReg),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    // Fallthrough to body
  end
  else
  begin
    // STEP direction known at compile-time - use single condition label
    CondLabel := GenerateUniqueLabel('for_cond');

    // Jump to condition check
    EmitInstruction(ssaJump, MakeSSALabel(CondLabel), MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // Condition check block
    FCurrentBlock := FProgram.CreateBlock(CondLabel);
    CondBlock := FCurrentBlock;

    // PHASE 3 TIER 3: Connect edge PrevBlock → CondBlock
    if Assigned(PrevBlock) then
    begin
      PrevBlock.AddSuccessor(CondBlock);
      CondBlock.AddPredecessor(PrevBlock);
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
        WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', CondBlock.LabelName);
      {$ENDIF}
    end;

    // Compare loop variable with end value
    CmpReg := FProgram.AllocRegister(VarReg.RegType);

    if StepIsNegative then
    begin
      // Negative step: use >= comparison
      if VarReg.RegType = srtInt then
        EmitInstruction(ssaCmpGeInt, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone))
      else
        EmitInstruction(ssaCmpGeFloat, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone));
    end
    else
    begin
      // Positive or zero step: use <= comparison
      if VarReg.RegType = srtInt then
        EmitInstruction(ssaCmpLeInt, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone))
      else
        EmitInstruction(ssaCmpLeFloat, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone));
    end;

    // Jump to body if condition is true (!=0), otherwise jump to end
    EmitInstruction(ssaJumpIfZero, MakeSSALabel(EndLabel), MakeSSARegister(VarReg.RegType, CmpReg),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Body block - subsequent statements will be added here until NEXT
  FCurrentBlock := FProgram.CreateBlock(BodyLabel);
  BodyBlock := FCurrentBlock;

  // PHASE 3 TIER 3: Connect edge CondBlock → BodyBlock (true branch)
  CondBlock.AddSuccessor(BodyBlock);
  BodyBlock.AddPredecessor(CondBlock);
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSA] Edge: ', CondBlock.LabelName, ' → ', BodyBlock.LabelName, ' (true)');
  {$ENDIF}

  // Push loop info onto stack
  LoopInfo.VarName := VarName;
  LoopInfo.VarReg := VarReg;
  LoopInfo.EndValue := EndValue;
  LoopInfo.StepValue := StepValue;
  LoopInfo.StepIsNegative := StepIsNegative;
  LoopInfo.NeedRuntimeCheck := NeedRuntimeCheck;
  LoopInfo.CondLabel := CondLabel;
  if NeedRuntimeCheck then
    LoopInfo.CondLabelGE := UseGELabel
  else
    LoopInfo.CondLabelGE := '';
  LoopInfo.BodyLabel := BodyLabel;
  LoopInfo.EndLabel := EndLabel;
  SetLength(FLoopStack, Length(FLoopStack) + 1);
  FLoopStack[High(FLoopStack)] := LoopInfo;
end;

procedure TSSAGenerator.ProcessNext(Node: TASTNode);
var
  LoopInfo: TLoopInfo;
  NewVarReg: Integer;
  TempReg: Integer;
  TempVal: TSSAValue;
  BodyBlock, CondBlock, EndBlock: TSSABasicBlock;  // PHASE 3 TIER 3: CFG construction
begin
  // Pop loop info from stack
  if Length(FLoopStack) = 0 then Exit;  // Error: NEXT without FOR
  LoopInfo := FLoopStack[High(FLoopStack)];
  SetLength(FLoopStack, Length(FLoopStack) - 1);

  // Increment loop variable by step
  if LoopInfo.VarReg.RegType = srtInt then
  begin
    NewVarReg := FProgram.AllocRegister(srtInt);
    EmitInstruction(ssaAddInt, MakeSSARegister(srtInt, NewVarReg), LoopInfo.VarReg, LoopInfo.StepValue, MakeSSAValue(svkNone));
    EmitInstruction(ssaCopyInt, LoopInfo.VarReg, MakeSSARegister(srtInt, NewVarReg), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
  begin
    NewVarReg := FProgram.AllocRegister(srtFloat);
    EmitInstruction(ssaAddFloat, MakeSSARegister(srtFloat, NewVarReg), LoopInfo.VarReg, LoopInfo.StepValue, MakeSSAValue(svkNone));
    EmitInstruction(ssaCopyFloat, LoopInfo.VarReg, MakeSSARegister(srtFloat, NewVarReg), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Jump back to condition check
  if LoopInfo.NeedRuntimeCheck then
  begin
    // Runtime check: need to test STEP sign again and jump to appropriate condition label
    // Materialize zero constant into a register first
    TempReg := FProgram.AllocRegister(LoopInfo.VarReg.RegType);
    TempVal := MakeSSARegister(LoopInfo.VarReg.RegType, TempReg);
    if LoopInfo.VarReg.RegType = srtInt then
      EmitInstruction(ssaLoadConstInt, TempVal, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaLoadConstFloat, TempVal, MakeSSAConstFloat(0.0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    NewVarReg := FProgram.AllocRegister(LoopInfo.VarReg.RegType);
    if LoopInfo.VarReg.RegType = srtInt then
      EmitInstruction(ssaCmpLtInt, MakeSSARegister(LoopInfo.VarReg.RegType, NewVarReg), LoopInfo.StepValue, TempVal, MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaCmpLtFloat, MakeSSARegister(LoopInfo.VarReg.RegType, NewVarReg), LoopInfo.StepValue, TempVal, MakeSSAValue(svkNone));

    // If STEP < 0, jump to GE label, else jump to LE label
    EmitInstruction(ssaJumpIfNotZero, MakeSSALabel(LoopInfo.CondLabelGE), MakeSSARegister(LoopInfo.VarReg.RegType, NewVarReg),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    EmitInstruction(ssaJump, MakeSSALabel(LoopInfo.CondLabel), MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
  begin
    // Standard case: jump to single condition label
    EmitInstruction(ssaJump, MakeSSALabel(LoopInfo.CondLabel), MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // PHASE 3 TIER 3: Add back-edge BodyBlock → CondBlock
  BodyBlock := FCurrentBlock;
  CondBlock := FProgram.FindBlock(LoopInfo.CondLabel);
  if Assigned(CondBlock) then
  begin
    BodyBlock.AddSuccessor(CondBlock);
    CondBlock.AddPredecessor(BodyBlock);
    {$IFDEF DEBUG_SSA}
    if DebugSSA then
      WriteLn('[SSA] Edge: ', BodyBlock.LabelName, ' → ', CondBlock.LabelName, ' (BACK-EDGE)');
    {$ENDIF}
  end;

  // Create end block for code after the loop
  FCurrentBlock := FProgram.CreateBlock(LoopInfo.EndLabel);
  EndBlock := FCurrentBlock;

  // PHASE 3 TIER 3: Add false branch CondBlock → EndBlock
  if Assigned(CondBlock) then
  begin
    CondBlock.AddSuccessor(EndBlock);
    EndBlock.AddPredecessor(CondBlock);
    {$IFDEF DEBUG_SSA}
    if DebugSSA then
      WriteLn('[SSA] Edge: ', CondBlock.LabelName, ' → ', EndBlock.LabelName, ' (false)');
    {$ENDIF}
  end;
end;

procedure TSSAGenerator.ProcessIfStatement(Node: TASTNode);
var
  CondValue: TSSAValue;
  ThenLabel, ElseLabel, EndLabel: string;
  i: Integer;
  Child: TASTNode;
  HasElse: Boolean;
  PrevBlock, ThenBlock, ElseBlock, EndBlock: TSSABasicBlock;  // PHASE 3 TIER 3: CFG construction
begin
  if Node.ChildCount < 1 then Exit;

  // Evaluate condition (should return Int 0 or 1)
  ProcessExpression(Node.GetChild(0), CondValue);

  // Generate unique labels
  ThenLabel := GenerateUniqueLabel('then');
  ElseLabel := GenerateUniqueLabel('else');
  EndLabel := GenerateUniqueLabel('endif');

  // Check if there's an ELSE clause
  HasElse := False;
  for i := 1 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);
    if Assigned(Child) and (Child.NodeType = antElse) then
    begin
      HasElse := True;
      Break;
    end;
  end;

  // PHASE 3 TIER 3: Save current block before creating new ones
  PrevBlock := FCurrentBlock;

  // Jump if condition is zero (false) to else or end
  if HasElse then
    EmitInstruction(ssaJumpIfZero, MakeSSALabel(ElseLabel), CondValue,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone))
  else
    EmitInstruction(ssaJumpIfZero, MakeSSALabel(EndLabel), CondValue,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));

  // THEN block
  FCurrentBlock := FProgram.CreateBlock(ThenLabel);
  ThenBlock := FCurrentBlock;

  // PHASE 3 TIER 3: Connect edge PrevBlock → ThenBlock (true branch)
  if Assigned(PrevBlock) then
  begin
    PrevBlock.AddSuccessor(ThenBlock);
    ThenBlock.AddPredecessor(PrevBlock);
    {$IFDEF DEBUG_SSA}
    if DebugSSA then
      WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', ThenBlock.LabelName, ' (true)');
    {$ENDIF}
  end;

  for i := 1 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);
    if Assigned(Child) and (Child.NodeType = antThen) then
    begin
      ProcessStatement(Child);
      Break;
    end;
  end;

  // FIX: Save the actual last block of THEN branch (might have changed due to nested control flow)
  ThenBlock := FCurrentBlock;

  // Jump to end after THEN
  EmitInstruction(ssaJump, MakeSSALabel(EndLabel), MakeSSAValue(svkNone),
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));

  // ELSE block (if present)
  if HasElse then
  begin
    FCurrentBlock := FProgram.CreateBlock(ElseLabel);
    ElseBlock := FCurrentBlock;

    // PHASE 3 TIER 3: Connect edge PrevBlock → ElseBlock (false branch)
    if Assigned(PrevBlock) then
    begin
      PrevBlock.AddSuccessor(ElseBlock);
      ElseBlock.AddPredecessor(PrevBlock);
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
        WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', ElseBlock.LabelName, ' (false)');
      {$ENDIF}
    end;

    for i := 1 to Node.ChildCount - 1 do
    begin
      Child := Node.GetChild(i);
      if Assigned(Child) and (Child.NodeType = antElse) then
      begin
        ProcessStatement(Child);
        Break;
      end;
    end;

    // FIX: Save the actual last block of ELSE branch (might have changed due to nested control flow)
    ElseBlock := FCurrentBlock;
  end;

  // End block
  FCurrentBlock := FProgram.CreateBlock(EndLabel);
  EndBlock := FCurrentBlock;

  // PHASE 3 TIER 3: Connect edges to EndBlock
  // ThenBlock → EndBlock (unconditional jump after THEN)
  if Assigned(ThenBlock) then
  begin
    ThenBlock.AddSuccessor(EndBlock);
    EndBlock.AddPredecessor(ThenBlock);
    {$IFDEF DEBUG_SSA}
    if DebugSSA then
      WriteLn('[SSA] Edge: ', ThenBlock.LabelName, ' → ', EndBlock.LabelName);
    {$ENDIF}
  end;

  if HasElse then
  begin
    // ElseBlock → EndBlock (fall-through after ELSE)
    if Assigned(ElseBlock) then
    begin
      ElseBlock.AddSuccessor(EndBlock);
      EndBlock.AddPredecessor(ElseBlock);
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
        WriteLn('[SSA] Edge: ', ElseBlock.LabelName, ' → ', EndBlock.LabelName);
      {$ENDIF}
    end;
  end
  else
  begin
    // PrevBlock → EndBlock (false branch when no ELSE)
    if Assigned(PrevBlock) then
    begin
      PrevBlock.AddSuccessor(EndBlock);
      EndBlock.AddPredecessor(PrevBlock);
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
        WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', EndBlock.LabelName, ' (false, no ELSE)');
      {$ENDIF}
    end;
  end;
end;

procedure TSSAGenerator.ProcessGoto(Node: TASTNode);
var
  LabelNode: TASTNode;
  LabelName: string;
  SourceBlock, TargetBlock: TSSABasicBlock;  // PHASE 3 TIER 3: CFG construction
begin
  if Node.ChildCount = 0 then Exit;
  LabelNode := Node.GetChild(0);
  LabelName := 'LINE_' + VarToStr(LabelNode.Value);

  // PHASE 3 TIER 3: Save current block before jump
  SourceBlock := FCurrentBlock;

  EmitInstruction(ssaJump, MakeSSALabel(LabelName), MakeSSAValue(svkNone),
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));

  // PHASE 3 TIER 3: Connect edge SourceBlock → TargetBlock
  // Note: TargetBlock might not exist yet (forward GOTO), will be connected later
  TargetBlock := FProgram.FindBlock(LabelName);
  if Assigned(TargetBlock) and Assigned(SourceBlock) then
  begin
    SourceBlock.AddSuccessor(TargetBlock);
    TargetBlock.AddPredecessor(SourceBlock);
    {$IFDEF DEBUG_SSA}
    if DebugSSA then
      WriteLn('[SSA] Edge: ', SourceBlock.LabelName, ' → ', TargetBlock.LabelName, ' (GOTO)');
    {$ENDIF}
  end;

  // PHASE 3 TIER 3: GOTO terminates the current block - no fall-through
  FCurrentBlock := nil;
end;

procedure TSSAGenerator.ProcessGosub(Node: TASTNode);
var
  LabelNode: TASTNode;
  LabelName: string;
  SourceBlock, TargetBlock: TSSABasicBlock;  // PHASE 3 TIER 3: CFG construction
begin
  if Node.ChildCount = 0 then Exit;
  LabelNode := Node.GetChild(0);
  LabelName := 'LINE_' + VarToStr(LabelNode.Value);

  // PHASE 3 TIER 3: Save current block before call
  SourceBlock := FCurrentBlock;

  EmitInstruction(ssaCall, MakeSSALabel(LabelName), MakeSSAValue(svkNone),
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));

  // PHASE 3 TIER 3: Connect edge SourceBlock → TargetBlock (subroutine entry)
  // Note: TargetBlock might not exist yet (forward GOSUB), will be connected later
  TargetBlock := FProgram.FindBlock(LabelName);
  if Assigned(TargetBlock) and Assigned(SourceBlock) then
  begin
    SourceBlock.AddSuccessor(TargetBlock);
    TargetBlock.AddPredecessor(SourceBlock);
    {$IFDEF DEBUG_SSA}
    if DebugSSA then
      WriteLn('[SSA] Edge: ', SourceBlock.LabelName, ' → ', TargetBlock.LabelName, ' (GOSUB)');
    {$ENDIF}
  end;

  // NOTE: Unlike GOTO, GOSUB does NOT set FCurrentBlock := nil
  // The fall-through edge LINE_750 → LINE_760 represents where the program counter
  // continues after RETURN. Variables flow via the RETURN edge (LINE_9030 → LINE_760).
end;

{ ProcessGraphics - Handle GRAPHIC command for setting graphics mode
  Syntax: GRAPHIC mode [, clear [, param3]]
  Parameters:
    0: mode   - graphics mode (0-7)
    1: clear  - clear screen (0 or 1), optional, default 0
    2: param3 - additional parameter for mode 7 (SDL2 mode index), optional
}
procedure TSSAGenerator.ProcessGraphics(Node: TASTNode);
var
  i, TempReg: Integer;
  ParamValues: array[0..2] of TSSAValue;
  ParamRegs: array[0..2] of TSSAValue;
  ParamCount: Integer;
begin
  // Initialize all parameters to None
  for i := 0 to 2 do
  begin
    ParamValues[i] := MakeSSAValue(svkNone);
    ParamRegs[i] := MakeSSAValue(svkNone);
  end;

  // Get parameter count
  ParamCount := Node.ChildCount;
  if ParamCount < 1 then
  begin
    WriteLn(StdErr, 'GRAPHIC: requires at least 1 parameter (mode)');
    Exit;
  end;

  // Evaluate each parameter expression
  for i := 0 to Min(ParamCount - 1, 2) do
  begin
    ProcessExpression(Node.GetChild(i), ParamValues[i]);

    // Materialize constants into registers
    if ParamValues[i].Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ParamRegs[i], ParamValues[i],
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ParamValues[i].Kind = svkConstFloat then
    begin
      // Convert float to int for mode parameter
      TempReg := FProgram.AllocRegister(srtInt);
      ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ParamRegs[i], MakeSSAConstInt(Trunc(ParamValues[i].ConstFloat)),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ParamValues[i].Kind = svkRegister then
      ParamRegs[i] := ParamValues[i]
    else
      ParamRegs[i] := MakeSSAValue(svkNone);
  end;

  // Set defaults for optional parameters
  // clear defaults to 0 (preserve buffer content)
  // First-time initialization is handled in SetGraphicMode when buffer doesn't exist yet
  if (ParamCount < 2) or (ParamRegs[1].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    ParamRegs[1] := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, ParamRegs[1], MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // param3 defaults to 0
  if (ParamCount < 3) or (ParamRegs[2].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    ParamRegs[2] := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, ParamRegs[2], MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit ssaGraphicSetMode instruction
  // Dest=None (no result), Src1=mode, Src2=clear, Src3=param3
  EmitInstruction(ssaGraphicSetMode, MakeSSAValue(svkNone),
                 ParamRegs[0], ParamRegs[1], ParamRegs[2]);
end;

{ ProcessScnClr - Handle SCNCLR command (Screen Clear)
  Syntax: SCNCLR [mode]
  If mode is specified (0-11), clears that mode's buffer
  If mode is omitted, clears the current mode's buffer

  Implemented as: GRAPHIC current_mode, 1  (or GRAPHIC specified_mode, 1)
  where 1 means "clear buffer"
}
procedure TSSAGenerator.ProcessScnClr(Node: TASTNode);
var
  ModeVal, ModeReg, ClearReg, Param3Reg: TSSAValue;
  TempReg, DestReg: Integer;
begin
  if FCurrentBlock = nil then Exit;

  // Check if mode is specified
  if Node.ChildCount > 0 then
  begin
    // Mode specified - use it
    ProcessExpression(Node.GetChild(0), ModeVal);

    // Materialize to int register
    if ModeVal.Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      ModeReg := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ModeReg, ModeVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ModeVal.Kind = svkConstFloat then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      ModeReg := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ModeReg, MakeSSAConstInt(Trunc(ModeVal.ConstFloat)),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if (ModeVal.Kind = svkRegister) and (ModeVal.RegType = srtFloat) then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      ModeReg := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaFloatToInt, ModeReg, ModeVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
      ModeReg := ModeVal;
  end
  else
  begin
    // No mode specified - use RGR(0) to get current mode
    // First emit ssaGraphicGetMode to get current mode
    TempReg := FProgram.AllocRegister(srtInt);
    DestReg := FProgram.AllocRegister(srtInt);
    ModeReg := MakeSSARegister(srtInt, DestReg);

    // Load 0 as argument for RGR
    EmitInstruction(ssaLoadConstInt, MakeSSARegister(srtInt, TempReg),
                   MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // Get current mode
    EmitInstruction(ssaGraphicGetMode, ModeReg, MakeSSARegister(srtInt, TempReg),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Clear flag = 1 (always clear)
  TempReg := FProgram.AllocRegister(srtInt);
  ClearReg := MakeSSARegister(srtInt, TempReg);
  EmitInstruction(ssaLoadConstInt, ClearReg, MakeSSAConstInt(1),
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));

  // Param3 = 0 (no special parameter)
  TempReg := FProgram.AllocRegister(srtInt);
  Param3Reg := MakeSSARegister(srtInt, TempReg);
  EmitInstruction(ssaLoadConstInt, Param3Reg, MakeSSAConstInt(0),
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));

  // Emit ssaGraphicSetMode: mode, 1 (clear), 0 (param3)
  EmitInstruction(ssaGraphicSetMode, MakeSSAValue(svkNone),
                 ModeReg, ClearReg, Param3Reg);
end;

{ ProcessBox - Handle BOX command for drawing rectangles
  Syntax: BOX color, x1, y1, x2, y2 [, angle [, filled [, fill_color]]]
  Parameters:
    0: color     - border color (32-bit RGBA or palette index)
    1: x1        - first corner X
    2: y1        - first corner Y
    3: x2        - second corner X
    4: y2        - second corner Y
    5: angle     - rotation angle in degrees (optional, default 0)
    6: filled    - 1=filled, 0=outline only (optional, default 0)
    7: fill_color - fill color (optional, default = border color)
}
procedure TSSAGenerator.ProcessBox(Node: TASTNode);
var
  i, TempReg: Integer;
  ParamValues: array[0..7] of TSSAValue;
  ParamRegs: array[0..7] of TSSAValue;
  ParamCount: Integer;
  Instr: TSSAInstruction;
begin
  // Initialize all parameters to None
  for i := 0 to 7 do
  begin
    ParamValues[i] := MakeSSAValue(svkNone);
    ParamRegs[i] := MakeSSAValue(svkNone);
  end;

  // Get parameter count
  ParamCount := Node.ChildCount;
  if ParamCount < 5 then
  begin
    // BOX requires at least 5 parameters: color, x1, y1, x2, y2
    WriteLn(StdErr, 'BOX: requires at least 5 parameters (color, x1, y1, x2, y2)');
    Exit;
  end;

  // Evaluate each parameter expression
  for i := 0 to Min(ParamCount - 1, 7) do
  begin
    ProcessExpression(Node.GetChild(i), ParamValues[i]);

    // Materialize constants into registers
    if ParamValues[i].Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
      WriteLn('>>> BOX SSA: param[', i, '] ConstInt=', ParamValues[i].ConstInt,
              ' -> allocated SSA reg ', TempReg,
              ' ParamRegs[', i, '].RegIndex=', ParamRegs[i].RegIndex);
      EmitInstruction(ssaLoadConstInt, ParamRegs[i], ParamValues[i],
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ParamValues[i].Kind = svkConstFloat then
    begin
      TempReg := FProgram.AllocRegister(srtFloat);
      ParamRegs[i] := MakeSSARegister(srtFloat, TempReg);
      WriteLn('>>> BOX SSA: param[', i, '] ConstFloat -> allocated SSA reg ', TempReg);
      EmitInstruction(ssaLoadConstFloat, ParamRegs[i], ParamValues[i],
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ParamValues[i].Kind = svkRegister then
    begin
      ParamRegs[i] := ParamValues[i];
      WriteLn('>>> BOX SSA: param[', i, '] already register ', ParamValues[i].RegIndex);
    end
    else
      ParamRegs[i] := MakeSSAValue(svkNone);
  end;

  // Set defaults for optional parameters
  // angle defaults to 0
  if (ParamCount < 6) or (ParamRegs[5].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    ParamRegs[5] := MakeSSARegister(srtFloat, TempReg);
    WriteLn('>>> BOX SSA: param[5] angle default -> allocated SSA float reg ', TempReg);
    EmitInstruction(ssaLoadConstFloat, ParamRegs[5], MakeSSAConstFloat(0.0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // filled defaults to 0 (outline only)
  if (ParamCount < 7) or (ParamRegs[6].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    ParamRegs[6] := MakeSSARegister(srtInt, TempReg);
    WriteLn('>>> BOX SSA: param[6] filled default -> allocated SSA int reg ', TempReg);
    EmitInstruction(ssaLoadConstInt, ParamRegs[6], MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // fill_color defaults to border color (ParamRegs[0])
  if (ParamCount < 8) or (ParamRegs[7].Kind = svkNone) then
    ParamRegs[7] := ParamRegs[0];

  // Emit ssaGraphicBox instruction
  // We use Dest=None (no result), Src1=color, Src2=x1, Src3=y1
  // Additional parameters stored in PhiSources: x2, y2, angle, filled, fill_color
  EmitInstruction(ssaGraphicBox, MakeSSAValue(svkNone),
                 ParamRegs[0], ParamRegs[1], ParamRegs[2]);

  // Add remaining parameters as PhiSources
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  WriteLn('>>> BOX SSA PhiSources: x2=', ParamRegs[3].RegIndex, ' y2=', ParamRegs[4].RegIndex,
          ' angle=', ParamRegs[5].RegIndex, ' filled=', ParamRegs[6].RegIndex,
          ' fill_color=', ParamRegs[7].RegIndex);
  Instr.AddPhiSource(ParamRegs[3], nil);  // x2
  Instr.AddPhiSource(ParamRegs[4], nil);  // y2
  Instr.AddPhiSource(ParamRegs[5], nil);  // angle
  Instr.AddPhiSource(ParamRegs[6], nil);  // filled
  Instr.AddPhiSource(ParamRegs[7], nil);  // fill_color
end;

{ ProcessCircle - Handle CIRCLE command for drawing circles, ellipses, arcs, polygons
  Syntax: CIRCLE [color], x, y, xr [, yr [, sa [, ea [, angle [, inc]]]]]
  Parameters:
    0: color  - color source (0=bg, 1=fg, 2=mc1, 3=mc2)
    1: x      - center X coordinate
    2: y      - center Y coordinate
    3: xr     - X radius (scaled)
    4: yr     - Y radius (optional, default = xr for circle)
    5: sa     - starting arc angle in degrees (optional, default 0)
    6: ea     - ending arc angle in degrees (optional, default 360)
    7: angle  - rotation angle in degrees (optional, default 0)
    8: inc    - degrees between segments (optional, default 2)
}
procedure TSSAGenerator.ProcessCircle(Node: TASTNode);
var
  i, TempReg: Integer;
  ParamValues: array[0..8] of TSSAValue;
  ParamRegs: array[0..8] of TSSAValue;
  ParamCount: Integer;
  Instr: TSSAInstruction;
begin
  // Initialize all parameters to None
  for i := 0 to 8 do
  begin
    ParamValues[i] := MakeSSAValue(svkNone);
    ParamRegs[i] := MakeSSAValue(svkNone);
  end;

  // Get parameter count
  ParamCount := Node.ChildCount;
  if ParamCount < 4 then
  begin
    // CIRCLE requires at least 4 parameters: color, x, y, xr
    WriteLn(StdErr, 'CIRCLE: requires at least 4 parameters (color, x, y, xr)');
    Exit;
  end;

  // Evaluate each parameter expression
  for i := 0 to Min(ParamCount - 1, 8) do
  begin
    ProcessExpression(Node.GetChild(i), ParamValues[i]);

    // Materialize constants into registers
    if ParamValues[i].Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ParamRegs[i], ParamValues[i],
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ParamValues[i].Kind = svkConstFloat then
    begin
      TempReg := FProgram.AllocRegister(srtFloat);
      ParamRegs[i] := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaLoadConstFloat, ParamRegs[i], ParamValues[i],
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ParamValues[i].Kind = svkRegister then
      ParamRegs[i] := ParamValues[i]
    else
      ParamRegs[i] := MakeSSAValue(svkNone);
  end;

  // Set defaults for optional parameters
  // yr defaults to xr (make a circle)
  if (ParamCount < 5) or (ParamRegs[4].Kind = svkNone) then
    ParamRegs[4] := ParamRegs[3];  // yr = xr

  // sa (start angle) defaults to 0
  if (ParamCount < 6) or (ParamRegs[5].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    ParamRegs[5] := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, ParamRegs[5], MakeSSAConstFloat(0.0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // ea (end angle) defaults to 360
  if (ParamCount < 7) or (ParamRegs[6].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    ParamRegs[6] := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, ParamRegs[6], MakeSSAConstFloat(360.0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // angle (rotation) defaults to 0
  if (ParamCount < 8) or (ParamRegs[7].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    ParamRegs[7] := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, ParamRegs[7], MakeSSAConstFloat(0.0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // inc (increment) defaults to 2
  if (ParamCount < 9) or (ParamRegs[8].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    ParamRegs[8] := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, ParamRegs[8], MakeSSAConstFloat(2.0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit ssaGraphicCircle instruction
  // We use Dest=None (no result), Src1=color, Src2=x, Src3=y
  // Additional parameters stored in PhiSources: xr, yr, sa, ea, angle, inc
  EmitInstruction(ssaGraphicCircle, MakeSSAValue(svkNone),
                 ParamRegs[0], ParamRegs[1], ParamRegs[2]);

  // Add remaining parameters as PhiSources
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  Instr.AddPhiSource(ParamRegs[3], nil);  // xr
  Instr.AddPhiSource(ParamRegs[4], nil);  // yr
  Instr.AddPhiSource(ParamRegs[5], nil);  // sa
  Instr.AddPhiSource(ParamRegs[6], nil);  // ea
  Instr.AddPhiSource(ParamRegs[7], nil);  // angle
  Instr.AddPhiSource(ParamRegs[8], nil);  // inc
end;

{ ProcessDraw - Handle DRAW command for drawing dots and lines
  Syntax: DRAW [color], x1, y1 [TO x2, y2] [TO x3, y3] ...
  Parameters:
    0: color - color source (0=bg, 1=fg, 2=mc1, 3=mc2), can be nil/omitted
    1: x1    - first point X coordinate
    2: y1    - first point Y coordinate
    3+: x2,y2, x3,y3... - additional points for TO segments

  Each ssaGraphicDraw instruction draws from current PC to (x,y)
  First point positions PC without drawing (unless only 1 point = draw dot)
  TO segments draw lines from current PC to next point
}
procedure TSSAGenerator.ProcessDraw(Node: TASTNode);
var
  i, TempReg, ParamCount, PointCount: Integer;
  ColorVal, XVal, YVal: TSSAValue;
  ColorReg, XReg, YReg: TSSAValue;
  Instr: TSSAInstruction;
  HasColor: Boolean;
begin
  ParamCount := Node.ChildCount;

  // Minimum: color (or nil), x1, y1 = at least 2 non-nil params
  // Parse: [color], x1, y1 [TO x2, y2]...
  // If first param is nil (comma without value), color is omitted

  if ParamCount < 2 then
  begin
    WriteLn(StdErr, 'DRAW: requires at least x, y coordinates');
    Exit;
  end;

  // Check if color is provided (first child could be nil for omitted color)
  HasColor := (ParamCount >= 3) and Assigned(Node.GetChild(0));

  // Process color if provided
  if HasColor then
  begin
    ProcessExpression(Node.GetChild(0), ColorVal);
    if ColorVal.Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      ColorReg := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ColorReg, ColorVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ColorVal.Kind = svkRegister then
      ColorReg := ColorVal
    else
    begin
      // Default color = 1 (foreground)
      TempReg := FProgram.AllocRegister(srtInt);
      ColorReg := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ColorReg, MakeSSAConstInt(1),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    i := 1;  // Start processing points from index 1
  end
  else
  begin
    // No color provided - use default (1 = foreground)
    TempReg := FProgram.AllocRegister(srtInt);
    ColorReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, ColorReg, MakeSSAConstInt(1),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    // Skip nil first child if color was omitted with comma
    if (ParamCount > 0) and not Assigned(Node.GetChild(0)) then
      i := 1
    else
      i := 0;
  end;

  // Process coordinate pairs
  // Each pair: (x, y)
  // First pair: move PC (mode=0), subsequent pairs: draw line (mode=1)
  PointCount := 0;
  while i < ParamCount - 1 do
  begin
    // Process X coordinate
    if not Assigned(Node.GetChild(i)) then
    begin
      Inc(i);
      Continue;
    end;
    ProcessExpression(Node.GetChild(i), XVal);
    if XVal.Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      XReg := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, XReg, XVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if XVal.Kind = svkConstFloat then
    begin
      TempReg := FProgram.AllocRegister(srtFloat);
      XReg := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaLoadConstFloat, XReg, XVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if XVal.Kind = svkRegister then
      XReg := XVal
    else
      XReg := MakeSSAValue(svkNone);
    Inc(i);

    // Process Y coordinate
    if i >= ParamCount then Break;
    if not Assigned(Node.GetChild(i)) then
    begin
      Inc(i);
      Continue;
    end;
    ProcessExpression(Node.GetChild(i), YVal);
    if YVal.Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      YReg := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, YReg, YVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if YVal.Kind = svkConstFloat then
    begin
      TempReg := FProgram.AllocRegister(srtFloat);
      YReg := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaLoadConstFloat, YReg, YVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if YVal.Kind = svkRegister then
      YReg := YVal
    else
      YReg := MakeSSAValue(svkNone);
    Inc(i);

    // Emit ssaGraphicDraw instruction
    // Dest=None, Src1=color, Src2=x, Src3=y
    // PhiSource[0] = mode: 0=move only (first point), 1=draw line (TO points)
    EmitInstruction(ssaGraphicDraw, MakeSSAValue(svkNone),
                   ColorReg, XReg, YReg);
    Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];

    // Mode: 0 for first point (move PC), 1 for subsequent (draw line)
    // If only one point total, mode=2 means draw dot
    if PointCount = 0 then
    begin
      // Check if this is the only point
      if i >= ParamCount then
        Instr.AddPhiSource(MakeSSAConstInt(2), nil)  // Single point = draw dot
      else
        Instr.AddPhiSource(MakeSSAConstInt(0), nil); // First point = move PC
    end
    else
      Instr.AddPhiSource(MakeSSAConstInt(1), nil);  // TO point = draw line

    Inc(PointCount);
  end;
end;

{ ProcessLocate - Handle LOCATE command for positioning pixel cursor
  Syntax: LOCATE x, y
  Parameters:
    0: x - pixel cursor X coordinate
    1: y - pixel cursor Y coordinate
}
procedure TSSAGenerator.ProcessLocate(Node: TASTNode);
var
  TempReg: Integer;
  XVal, YVal: TSSAValue;
  XReg, YReg: TSSAValue;
begin
  if Node.ChildCount < 2 then
  begin
    WriteLn(StdErr, 'LOCATE: requires x, y coordinates');
    Exit;
  end;

  // Process X coordinate
  ProcessExpression(Node.GetChild(0), XVal);
  if XVal.Kind = svkConstInt then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    XReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, XReg, XVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if XVal.Kind = svkConstFloat then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    XReg := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, XReg, XVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if XVal.Kind = svkRegister then
    XReg := XVal
  else
    XReg := MakeSSAValue(svkNone);

  // Process Y coordinate
  ProcessExpression(Node.GetChild(1), YVal);
  if YVal.Kind = svkConstInt then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    YReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, YReg, YVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if YVal.Kind = svkConstFloat then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    YReg := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, YReg, YVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if YVal.Kind = svkRegister then
    YReg := YVal
  else
    YReg := MakeSSAValue(svkNone);

  // Emit ssaGraphicLocate instruction
  // Dest=None, Src1=x, Src2=y, Src3=None
  EmitInstruction(ssaGraphicLocate, MakeSSAValue(svkNone),
                 XReg, YReg, MakeSSAValue(svkNone));
end;

{ PHASE 3 TIER 3: Fix forward GOTO/GOSUB references and RETURN edges }
procedure TSSAGenerator.FixForwardReferences;
var
  i, j, k, SuccIdx: Integer;
  Block, ReturnBlock, GosubBlock, ReturnTarget: TSSABasicBlock;
  Instr: TSSAInstruction;
  TargetBlock: TSSABasicBlock;
  TargetLabel: string;
begin
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSA] Fixing forward GOTO/GOSUB references...');
  {$ENDIF}

  // Pass 1: Fix forward GOTO/GOSUB edges
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];

    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      // Check for GOTO or GOSUB with label operand
      if (Instr.OpCode in [ssaJump, ssaCall]) and (Instr.Dest.Kind = svkLabel) then
      begin
        TargetLabel := Instr.Dest.LabelName;
        TargetBlock := FProgram.FindBlock(TargetLabel);

        if Assigned(TargetBlock) then
        begin
          // Check if edge already exists
          if Block.Successors.IndexOf(TargetBlock) = -1 then
          begin
            // Add missing edge
            Block.AddSuccessor(TargetBlock);
            TargetBlock.AddPredecessor(Block);
            {$IFDEF DEBUG_SSA}
            if DebugSSA then
            begin
              if Instr.OpCode = ssaJump then
                WriteLn('[SSA] Edge: ', Block.LabelName, ' → ', TargetLabel, ' (GOTO forward ref)')
              else
                WriteLn('[SSA] Edge: ', Block.LabelName, ' → ', TargetLabel, ' (GOSUB forward ref)');
            end;
            {$ENDIF}
          end;
        end
        {$IFDEF DEBUG_SSA}
        else if DebugSSA then
          WriteLn('[SSA] WARNING: Target block not found: ', TargetLabel);
        {$ENDIF}
        ;
      end;
    end;
  end;

  // Pass 2: Fix RETURN edges
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSA] Fixing RETURN edges...');
  {$ENDIF}
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];

    // Check if this block contains RETURN
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      if Block.Instructions[j].OpCode = ssaReturn then
      begin
        ReturnBlock := Block;

        // Find all GOSUB blocks and connect RETURN to their successors
        for k := 0 to FProgram.Blocks.Count - 1 do
        begin
          GosubBlock := FProgram.Blocks[k];

          // Check if this block contains GOSUB (ssaCall)
          if (GosubBlock.Instructions.Count > 0) and
             (GosubBlock.Instructions[GosubBlock.Instructions.Count-1].OpCode = ssaCall) then
          begin
            // The return target is the fall-through successor of the GOSUB block
            if GosubBlock.Successors.Count > 0 then
            begin
              // Find the fall-through successor (not the CALL target)
              for SuccIdx := 0 to GosubBlock.Successors.Count - 1 do
              begin
                ReturnTarget := TSSABasicBlock(GosubBlock.Successors[SuccIdx]);

                // Skip the CALL target - return point is the other successor
                if ReturnTarget.LabelName <> GosubBlock.Instructions[GosubBlock.Instructions.Count-1].Dest.LabelName then
                begin
                  // Add edge from RETURN block to return point
                  if ReturnBlock.Successors.IndexOf(ReturnTarget) = -1 then
                  begin
                    ReturnBlock.AddSuccessor(ReturnTarget);
                    ReturnTarget.AddPredecessor(ReturnBlock);
                    {$IFDEF DEBUG_SSA}
                    if DebugSSA then
                      WriteLn('[SSA] Edge: ', ReturnBlock.LabelName, ' → ', ReturnTarget.LabelName, ' (RETURN)');
                    {$ENDIF}
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSSAGenerator.ProcessStatement(Node: TASTNode);
var
  LineNum: Integer;
  LabelName: string;
  i: Integer;
  PrevBlock, NewBlock: TSSABasicBlock;  // PHASE 3 TIER 3: CFG construction
  SecondsVal: TSSAValue;  // For SLEEP command
begin
  if Node = nil then Exit;

  // Handle line numbers
  if (Node.ChildCount > 0) and (Node.GetChild(0).NodeType = antLineNumber) then
  begin
    LineNum := Integer(Node.GetChild(0).Value);
    LabelName := 'LINE_' + IntToStr(LineNum);

    // PHASE 3 TIER 3: Save previous block before creating new one
    PrevBlock := FCurrentBlock;

    // Create new block for this line
    FCurrentBlock := FProgram.CreateBlock(LabelName);
    NewBlock := FCurrentBlock;

    // PHASE 3 TIER 3: Add fall-through edge PrevBlock → NewBlock
    if Assigned(PrevBlock) and Assigned(NewBlock) and (PrevBlock <> NewBlock) then
    begin
      PrevBlock.AddSuccessor(NewBlock);
      NewBlock.AddPredecessor(PrevBlock);
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
        WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', NewBlock.LabelName, ' (fall-through)');
      {$ENDIF}
    end;
  end;

  case Node.NodeType of
    antLineNumber:
    begin
      // When we encounter a line number node, create a new block for it
      LineNum := Integer(Node.Value);
      LabelName := 'LINE_' + IntToStr(LineNum);

      // Track current line number for error reporting
      FCurrentLineNumber := LineNum;

      // PHASE 3 TIER 3: Save previous block before creating new one
      PrevBlock := FCurrentBlock;

      FCurrentBlock := FProgram.CreateBlock(LabelName);
      NewBlock := FCurrentBlock;

      // PHASE 3 TIER 3: Add fall-through edge PrevBlock → NewBlock
      if Assigned(PrevBlock) and Assigned(NewBlock) and (PrevBlock <> NewBlock) then
      begin
        PrevBlock.AddSuccessor(NewBlock);
        NewBlock.AddPredecessor(PrevBlock);
        {$IFDEF DEBUG_SSA}
        if DebugSSA then
          WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', NewBlock.LabelName, ' (fall-through)');
        {$ENDIF}
      end;
    end;
    antAssignment: ProcessAssignment(Node);
    antConst:
    begin
      // CONST is just an assignment with a constant value
      // Process the child assignment node
      if Node.ChildCount > 0 then
        ProcessAssignment(Node.GetChild(0));
    end;
    antPrint: ProcessPrint(Node);
    antInput: ProcessInput(Node);
    antDim: ProcessDim(Node);
    antForLoop: ProcessForLoop(Node);
    antNext: ProcessNext(Node);
    antIf: ProcessIfStatement(Node);
    antGoto: ProcessGoto(Node);
    antGosub: ProcessGosub(Node);
    antBox: ProcessBox(Node);
    antCircle: ProcessCircle(Node);
    antDraw: ProcessDraw(Node);
    antLocate: ProcessLocate(Node);
    antGraphics: ProcessGraphics(Node);
    antScnClr: ProcessScnClr(Node);
    antReturn:
    begin
      EmitInstruction(ssaReturn, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      // PHASE 3 TIER 3: RETURN terminates the current block - no fall-through
      FCurrentBlock := nil;
    end;
    antEnd:
    begin
      EmitInstruction(ssaEnd, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      // PHASE 3 TIER 3: END terminates the current block - no fall-through
      FCurrentBlock := nil;
    end;
    antFast:
    begin
      EmitInstruction(ssaFast, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    antSlow:
    begin
      EmitInstruction(ssaSlow, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    antSleep:
    begin
      // SLEEP n - delay for n seconds
      // Child[0] = expression for seconds
      if Node.ChildCount > 0 then
      begin
        ProcessExpression(Node.GetChild(0), SecondsVal);
        EmitInstruction(ssaSleep, MakeSSAValue(svkNone), SecondsVal,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
      begin
        // SLEEP without parameter - default to 1 second
        EmitInstruction(ssaSleep, MakeSSAValue(svkNone), MakeSSAConstInt(1),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end;
    antProgram, antStatement, antThen, antElse:
    begin
      // Process children
      for i := 0 to Node.ChildCount - 1 do
        ProcessStatement(Node.GetChild(i));
    end;
  else
    // Unhandled node type - still process children in case they contain statements
    // This ensures that nested statements are not silently dropped
    begin
      WriteLn(StdErr, '[SSA] WARNING: Unhandled node type ', Ord(Node.NodeType), ' - processing children');
      for i := 0 to Node.ChildCount - 1 do
        ProcessStatement(Node.GetChild(i));
    end;
  end;
end;

function TSSAGenerator.Generate(AST: TASTNode): TSSAProgram;
var
  LastBlock: TSSABasicBlock;
  LastInstr: TSSAInstruction;
begin
  FProgram := TSSAProgram.Create;
  FLabelCounter := 0;

  // PRE-ALLOCATE ALL VARIABLE REGISTERS FIRST
  // This prevents conflicts between variable registers and temporary expression registers
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSA] Pre-allocating variable registers...');
  {$ENDIF}
  PreAllocateVariables(AST);
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSA] Variable registers pre-allocated: ', FVarMap.Count, ' variables');
  {$ENDIF}

  // Create entry block
  FCurrentBlock := FProgram.CreateBlock('_entry');

  // Process AST
  ProcessStatement(AST);

  // PHASE 3 TIER 3: Fix forward GOTO/GOSUB references
  FixForwardReferences;

  // Add END if not present - check the LAST block in the program, not current block
  if FProgram.Blocks.Count > 0 then
  begin
    LastBlock := FProgram.Blocks[FProgram.Blocks.Count - 1];
    if (LastBlock.Instructions.Count = 0) or
       (LastBlock.Instructions[LastBlock.Instructions.Count-1].OpCode <> ssaEnd) then
    begin
      // Switch to last block to add END there
      FCurrentBlock := LastBlock;
      EmitInstruction(ssaEnd, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end;

  Result := FProgram;
end;

{ Helper methods for constant register tracking }

procedure TSSAGenerator.RecordConstantRegister(const Val: TSSAValue);
begin
  if Val.Kind <> svkRegister then
    Exit;

  // This method is called after emitting LoadConstFloat/LoadConstInt
  // The register now holds a known constant value
  case Val.RegType of
    srtFloat:
      begin
        // We can't directly get the const value here, so this will be called
        // from the LoadConst emission site with the constant value
      end;
    srtInt:
      begin
        // Same as above
      end;
  end;
end;

function TSSAGenerator.TryGetConstantFloat(RegIndex: Integer; out ConstValue: Double): Boolean;
begin
  Result := FConstFloatRegs.TryGetValue(RegIndex, ConstValue);
end;

function TSSAGenerator.TryGetConstantInt(RegIndex: Integer; out ConstValue: Integer): Boolean;
begin
  Result := FConstIntRegs.TryGetValue(RegIndex, ConstValue);
end;

procedure TSSAGenerator.InvalidateRegister(RegIndex: Integer; RegType: TSSARegisterType);
begin
  // Remove register from constant tracking when it gets reassigned
  case RegType of
    srtFloat: FConstFloatRegs.Remove(RegIndex);
    srtInt: FConstIntRegs.Remove(RegIndex);
  end;
end;

function TSSAGenerator.EnsureIntRegister(const Val: TSSAValue): TSSAValue;
var
  TempReg: Integer;
  FloatReg: Integer;
  FloatRegVal: TSSAValue;
begin
  // Ensure value is in an integer register
  case Val.Kind of
    svkConstInt:
    begin
      // Load constant into integer register
      TempReg := FProgram.AllocRegister(srtInt);
      Result := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, Result, Val, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    svkConstFloat:
    begin
      // Convert float constant to integer: load float, then convert
      FloatReg := FProgram.AllocRegister(srtFloat);
      FloatRegVal := MakeSSARegister(srtFloat, FloatReg);
      EmitInstruction(ssaLoadConstFloat, FloatRegVal, Val, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      TempReg := FProgram.AllocRegister(srtInt);
      Result := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaFloatToInt, Result, FloatRegVal, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    svkRegister:
    begin
      if Val.RegType = srtInt then
        // Already an int register
        Result := Val
      else if Val.RegType = srtFloat then
      begin
        // Convert float register to int
        TempReg := FProgram.AllocRegister(srtInt);
        Result := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaFloatToInt, Result, Val, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
        // String or other - return as-is (will likely cause error later)
        Result := Val;
    end;
  else
    // For other kinds, return as-is
    Result := Val;
  end;
end;

end.
