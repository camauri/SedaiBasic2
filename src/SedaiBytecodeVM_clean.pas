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
unit SedaiBytecodeVM;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Variants, Math,
  SedaiBytecodeTypes, SedaiExecutorContext, SedaiExecutorTypes,
  SedaiOutputInterface;

const
  MAX_REGISTERS = 32;  // Number of registers per type (Int, Float, String)

type
  { Variable type tracking for register-based architecture }
  TVMVarType = (
    vmvtInteger,   // Int64
    vmvtFloat,     // Double
    vmvtString     // String
  );

  { Forward declaration for opcode handler }
  TSedaiBytecodeVM = class;
  
  { Opcode handler procedure type - THREADED CODE optimization }
  TOpcodeHandler = procedure(VM: TSedaiBytecodeVM; const Inst: TInstruction);

  { TSedaiBytecodeVM - Register-based virtual machine with threaded code interpreter
    Pure register architecture with typed registers (Int64, Double, String) }
  TSedaiBytecodeVM = class
  private
    // Execution state
    FProgram: TBytecodeProgram;
    FPC: Integer;                    // Program Counter

    // Call stack for GOSUB/RETURN (and function calls)
    FCallStack: array[0..255] of Integer;   // Return addresses
    FCallSP: Integer;                        // Call stack pointer

    FHalted: Boolean;
    FBreakRequested: Boolean;

    // REGISTER-BASED ARCHITECTURE: Pure typed registers (NO STACK, NO VARIANT!)
    FIntRegisters: array[0..31] of Int64;    // R0-R31: Integer registers
    FFloatRegisters: array[0..31] of Double; // F0-F31: Float registers
    FStrRegisters: array[0..31] of string;   // S0-S31: String registers
    FCompareFlags: Integer;                   // Comparison result flags

    // Variable metadata for register-based operations
    FVarTypes: array of TVMVarType;          // Variable types (deduced from name suffix)
    FVarToRegMap: array of Integer;          // Variable index -> Register index (or -1)

    // Runtime context (shared with interpreter)
    FContext: TExecutorContext;
    FOutput: IOutputDevice;
    FInput: IInputDevice;

    // Statistics
    FInstructionsExecuted: Int64;
    FStartTime: TDateTime;
    FEndTime: TDateTime;

    // Random number generation counter
    FRndCounter: Cardinal;  // Increments with each RND call to ensure unique seeds

    // Profiling (optional, can be disabled for production)
    FOpcodeCounts: array[TOpCode] of Int64;
    FProfilingEnabled: Boolean;
    
    // THREADED CODE: Direct dispatch table for opcodes
    class var FOpcodeHandlers: array[TOpCode] of TOpcodeHandler;
    class var FHandlersInitialized: Boolean;
    class procedure InitializeHandlers;

    // Instruction execution (DEPRECATED - replaced by threaded code)
    procedure ExecuteInstruction(const Inst: TInstruction);
    procedure ExecuteAdd;
    procedure ExecuteSub;
    procedure ExecuteMul;
    procedure ExecuteDiv;
    procedure ExecuteMod;
    procedure ExecutePow;
    procedure ExecuteNeg;
    
    // OPTIMIZED: Integer-only operations
    procedure ExecuteAddInt;
    procedure ExecuteSubInt;
    procedure ExecuteMulInt;
    procedure ExecuteDivInt;
    procedure ExecuteModInt;
    procedure ExecuteNegInt;
    procedure ExecuteIncVar;
    procedure ExecuteDecVar;
    
    // PHASE 2: Float-only operations
    procedure ExecuteAddFloat;
    procedure ExecuteSubFloat;
    procedure ExecuteMulFloat;
    procedure ExecuteDivFloat;
    procedure ExecuteNegFloat;
    procedure ExecutePowFloat;

    procedure ExecuteEq;
    procedure ExecuteNeq;
    procedure ExecuteLt;
    procedure ExecuteGt;
    procedure ExecuteLe;
    procedure ExecuteGe;
    
    // OPTIMIZED: Integer-only comparisons
    procedure ExecuteEqInt;
    procedure ExecuteNeqInt;
    procedure ExecuteLtInt;
    procedure ExecuteGtInt;
    procedure ExecuteLeInt;
    procedure ExecuteGeInt;
    
    // PHASE 2: Float-only comparisons
    procedure ExecuteEqFloat;
    procedure ExecuteNeqFloat;
    procedure ExecuteLtFloat;
    procedure ExecuteGtFloat;
    procedure ExecuteLeFloat;
    procedure ExecuteGeFloat;

    procedure ExecuteAnd;
    procedure ExecuteOr;
    procedure ExecuteNot;

    procedure ExecutePrint;
    procedure ExecutePrintNewline;
    procedure ExecuteInput(VarIdx: Integer);

    procedure ExecuteCallBuiltin(ArgCount: Integer);

    procedure ExecuteDimArray(VarIdx, DimCount: Integer);
    procedure ExecuteLoadArray(VarIdx, IndexCount: Integer);
    procedure ExecuteStoreArray(VarIdx, IndexCount: Integer);
    procedure ExecuteLoadArray1DFast(VarIdx: Integer);   // OPTIMIZED
    procedure ExecuteStoreArray1DFast(VarIdx: Integer);  // OPTIMIZED
    
    // PHASE 4: Typed array operations
    procedure ExecuteLoadArray1DInt(VarIdx: Integer);    // Integer-specialized
    procedure ExecuteStoreArray1DInt(VarIdx: Integer);   // Integer-specialized

    // Error handling
    procedure RaiseRuntimeError(const Msg: string);
    procedure CheckStackUnderflow(Required: Integer);
    procedure CheckStackOverflow;

  public
    constructor Create(AContext: TExecutorContext; AOutput: IOutputDevice; AInput: IInputDevice = nil);
    destructor Destroy; override;

    // Execution control
    procedure Execute(AProgram: TBytecodeProgram);
    procedure Reset;
    procedure Break;

    // Statistics
    function GetExecutionTime: Double;  // In milliseconds
    function GetInstructionsPerSecond: Double;
    
    // Profiling
    procedure EnableProfiling;
    procedure DisableProfiling;
    function GetOpcodeCount(OpCode: TOpCode): Int64;
    function GetTopOpcodes(TopN: Integer): string;

    property InstructionsExecuted: Int64 read FInstructionsExecuted;
    property PC: Integer read FPC;
    property Halted: Boolean read FHalted;
    property ProfilingEnabled: Boolean read FProfilingEnabled write FProfilingEnabled;
  end;

implementation

uses
  DateUtils, TypInfo;

{ TSedaiBytecodeVM }

constructor TSedaiBytecodeVM.Create(AContext: TExecutorContext; AOutput: IOutputDevice; AInput: IInputDevice = nil);
var
  I: Integer;
begin
  inherited Create;
  FContext := AContext;
  FOutput := AOutput;
  FInput := AInput;
  FProgram := nil;
  FPC := 0;
  FCallSP := 0;
  FHalted := False;
  FBreakRequested := False;
  FInstructionsExecuted := 0;
  FRndCounter := 0;
  FProfilingEnabled := False;

  // Initialize 32 typed registers (register-based architecture)
  for I := 0 to 31 do
  begin
    FIntRegisters[I] := 0;
    FFloatRegisters[I] := 0.0;
    FStrRegisters[I] := '';
  end;

  // Initialize random number generator
  Randomize;
end;

destructor TSedaiBytecodeVM.Destroy;
begin
  inherited Destroy;
end;

// === MAIN EXECUTION LOOP - THREADED CODE ===

procedure TSedaiBytecodeVM.Execute(AProgram: TBytecodeProgram);
var
  Inst: TInstruction;
  MaxPC: Integer;
  Handler: TOpcodeHandler;
  I: Integer;
begin
  if not Assigned(AProgram) then
    RaiseRuntimeError('No program loaded');

  // Initialize handlers on first use
  if not FHandlersInitialized then
    InitializeHandlers;

  // Initialize
  FProgram := AProgram;
  FPC := FProgram.EntryPoint;
  FCallSP := 0;
  FHalted := False;
  FBreakRequested := False;
  FInstructionsExecuted := 0;
  FStartTime := Now;

  // Note: Don't reset FProfilingEnabled here - it's set by EnableProfiling/DisableProfiling

  // PHASE 4F: Set array dimensioning mode from program
  FContext.ArrayIndexMode := FProgram.ArrayIndexMode;

  // PHASE 3: Allocate typed indexed storage for variables and arrays
  // This enables ZERO Variant overhead (direct Int64/Double/String access)
  // Variables and arrays share the same namespace in BASIC
  FContext.AllocateIndexedStorage(FProgram.Variables, FProgram.Variables.Count);

  // Initialize variable type tracking and register mapping
  SetLength(FVarTypes, FProgram.Variables.Count);
  SetLength(FVarToRegMap, FProgram.Variables.Count);
  if Length(FVarTypes) > 0 then
  begin
    for I := 0 to Length(FVarTypes) - 1 do
    begin
      // Deduce type from BASIC variable name suffix
      case FProgram.Variables[I][Length(FProgram.Variables[I])] of
        '$': FVarTypes[I] := vmvtString;   // String variable
        '%': FVarTypes[I] := vmvtInteger;  // Integer variable
      else
        FVarTypes[I] := vmvtFloat;         // Float variable (default)
      end;
      // No register mapped initially
      FVarToRegMap[I] := -1;
    end;
  end;

  MaxPC := FProgram.CodeSize;

  // THREADED CODE DISPATCH LOOP - Direct function call, no case statement!
  // PERFORMANCE: Split into two loops to avoid profiling check overhead
  // ULTRA-PERFORMANCE: Removed Handler variable indirection for maximum speed
  if FProfilingEnabled then
  begin
    // Profiling enabled - track opcode counts
    while (FPC < MaxPC) and not FHalted and not FBreakRequested do
    begin
      Inst := FProgram.Code[FPC];
      Inc(FPC);
      Inc(FInstructionsExecuted);
      Inc(FOpcodeCounts[Inst.OpCode]);

      // Direct dispatch - no intermediate variable
      FOpcodeHandlers[Inst.OpCode](Self, Inst);
    end;
  end
  else
  begin
    // Profiling disabled - ULTRA MAXIMUM SPEED
    while (FPC < MaxPC) and not FHalted and not FBreakRequested do
    begin
      Inst := FProgram.Code[FPC];
      Inc(FPC);
      Inc(FInstructionsExecuted);

      // Direct dispatch - no intermediate variable for maximum speed
      FOpcodeHandlers[Inst.OpCode](Self, Inst);
    end;
  end;

  FEndTime := Now;
end;

// DEPRECATED: ExecuteInstruction removed - VM now uses threaded code (FOpcodeHandlers)

// === ARITHMETIC OPERATIONS ===

procedure TSedaiBytecodeVM.ExecuteAdd;
begin
  // REGISTER-BASED: F0 := F1 + F0 (Float addition)
  FFloatRegisters[0] := FFloatRegisters[1] + FFloatRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecuteSub;
begin
  // REGISTER-BASED: F0 := F1 - F0 (Float subtraction)
  FFloatRegisters[0] := FFloatRegisters[1] - FFloatRegisters[0];
end;

// === ARITHMETIC OPERATIONS ===

procedure TSedaiBytecodeVM.ExecuteAdd;
begin
  // REGISTER-BASED: F0 := F1 + F0 (Float addition)
  FFloatRegisters[0] := FFloatRegisters[1] + FFloatRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecuteSub;
begin
  // REGISTER-BASED: F0 := F1 - F0 (Float subtraction)
  FFloatRegisters[0] := FFloatRegisters[1] - FFloatRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecuteMul;
begin
  // REGISTER-BASED: F0 := F1 * F0 (Float multiplication)
  FFloatRegisters[0] := FFloatRegisters[1] * FFloatRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecuteDiv;
begin
  // REGISTER-BASED: F0 := F1 / F0 (Float division)
  if FFloatRegisters[0] = 0.0 then
    RaiseRuntimeError('Division by zero');
  FFloatRegisters[0] := FFloatRegisters[1] / FFloatRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecuteMod;
begin
  // REGISTER-BASED: R0 := R1 mod R0 (Integer modulo)
  if FIntRegisters[0] = 0 then
    RaiseRuntimeError('Modulo by zero');
  FIntRegisters[0] := FIntRegisters[1] mod FIntRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecutePow;
begin
  // REGISTER-BASED: F0 := F1 ^ F0 (Float power)
  FFloatRegisters[0] := Power(FFloatRegisters[1], FFloatRegisters[0]);
end;

procedure TSedaiBytecodeVM.ExecuteNeg;
begin
  // REGISTER-BASED: F0 := -F0 (Float negation)
  FFloatRegisters[0] := -FFloatRegisters[0];
end;

// === OPTIMIZED INTEGER-ONLY OPERATIONS ===

procedure TSedaiBytecodeVM.ExecuteAddInt;
begin
  // REGISTER-BASED: R0 := R1 + R0 (Integer addition)
  FIntRegisters[0] := FIntRegisters[1] + FIntRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecuteSubInt;
begin
  // REGISTER-BASED: R0 := R1 - R0 (Integer subtraction)
  FIntRegisters[0] := FIntRegisters[1] - FIntRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecuteMulInt;
begin
  // REGISTER-BASED: R0 := R1 * R0 (Integer multiplication)
  FIntRegisters[0] := FIntRegisters[1] * FIntRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecuteDivInt;
begin
  // REGISTER-BASED: R0 := R1 div R0 (Integer division)
  if FIntRegisters[0] = 0 then
    RaiseRuntimeError('Division by zero');
  FIntRegisters[0] := FIntRegisters[1] div FIntRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecuteModInt;
begin
  // REGISTER-BASED: R0 := R1 mod R0 (Integer modulo)
  if FIntRegisters[0] = 0 then
    RaiseRuntimeError('Modulo by zero');
  FIntRegisters[0] := FIntRegisters[1] mod FIntRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecuteNegInt;
begin
  // REGISTER-BASED: R0 := -R0 (Integer negation)
  FIntRegisters[0] := -FIntRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecuteIncVar;
var
  VarIdx: Integer;
  Value: Int64;
  Inst: TInstruction;
  RegIdx: Integer;
begin
  // REGISTER-BASED: Increment variable and sync with register if mapped (NO VARIANT!)
  Inst := FProgram.Code[FPC];
  VarIdx := Inst.VarIndex;

  // Increment in memory
  Value := FContext.GetVariableByIndexInt(VarIdx);
  Value := Value + 1;
  FContext.SetVariableByIndexInt(VarIdx, Value);

  // If variable mapped to a register, update register copy as well
  if (VarIdx >= 0) and (VarIdx < Length(FVarToRegMap)) then
  begin
    RegIdx := FVarToRegMap[VarIdx];
    if RegIdx >= 0 then
      FIntRegisters[RegIdx] := Value;
  end;
end;

procedure TSedaiBytecodeVM.ExecuteDecVar;
var
  VarIdx: Integer;
  Value: Int64;
  Inst: TInstruction;
  RegIdx: Integer;
begin
  // REGISTER-BASED: Decrement variable and sync with register if mapped (NO VARIANT!)
  Inst := FProgram.Code[FPC];
  VarIdx := Inst.VarIndex;

  // Decrement in memory
  Value := FContext.GetVariableByIndexInt(VarIdx);
  Value := Value - 1;
  FContext.SetVariableByIndexInt(VarIdx, Value);

  // If variable mapped to a register, update register copy as well
  if (VarIdx >= 0) and (VarIdx < Length(FVarToRegMap)) then
  begin
    RegIdx := FVarToRegMap[VarIdx];
    if RegIdx >= 0 then
      FIntRegisters[RegIdx] := Value;
  end;
end;

// === FLOAT-ONLY OPERATIONS ===

procedure TSedaiBytecodeVM.ExecuteAddFloat;
begin
  // REGISTER-BASED: F0 := F1 + F0 (Float addition, same as ExecuteAdd)
  FFloatRegisters[0] := FFloatRegisters[1] + FFloatRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecuteSubFloat;
begin
  // REGISTER-BASED: F0 := F1 - F0 (Float subtraction)
  FFloatRegisters[0] := FFloatRegisters[1] - FFloatRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecuteMulFloat;
begin
  // REGISTER-BASED: F0 := F1 * F0 (Float multiplication)
  FFloatRegisters[0] := FFloatRegisters[1] * FFloatRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecuteDivFloat;
begin
  // REGISTER-BASED: F0 := F1 / F0 (Float division)
  if FFloatRegisters[0] = 0.0 then
    RaiseRuntimeError('Division by zero');
  FFloatRegisters[0] := FFloatRegisters[1] / FFloatRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecuteNegFloat;
begin
  // REGISTER-BASED: F0 := -F0 (Float negation)
  FFloatRegisters[0] := -FFloatRegisters[0];
end;

procedure TSedaiBytecodeVM.ExecutePowFloat;
begin
  // REGISTER-BASED: F0 := F1 ^ F0 (Float power)
  FFloatRegisters[0] := Power(FFloatRegisters[1], FFloatRegisters[0]);
end;

// === COMPARISON OPERATIONS ===

procedure TSedaiBytecodeVM.ExecuteEq;
var
  Right, Left: Variant;
begin
  CheckStackUnderflow(2);
  Right := Pop;
  Left := Pop;
  PushVariant(Integer(Ord(Left = Right)));
end;

procedure TSedaiBytecodeVM.ExecuteNeq;
var
  Right, Left: Variant;
begin
  CheckStackUnderflow(2);
  Right := Pop;
  Left := Pop;
  PushVariant(Integer(Ord(Left <> Right)));
end;

procedure TSedaiBytecodeVM.ExecuteLt;
var
  Right, Left: Variant;
begin
  CheckStackUnderflow(2);
  Right := Pop;
  Left := Pop;
  PushVariant(Integer(Ord(Double(Left) < Double(Right))));
end;

procedure TSedaiBytecodeVM.ExecuteGt;
var
  Right, Left: Variant;
begin
  CheckStackUnderflow(2);
  Right := Pop;
  Left := Pop;
  PushVariant(Integer(Ord(Double(Left) > Double(Right))));
end;

procedure TSedaiBytecodeVM.ExecuteLe;
var
  Right, Left: Variant;
begin
  CheckStackUnderflow(2);
  Right := Pop;
  Left := Pop;
  PushVariant(Integer(Ord(Double(Left) <= Double(Right))));
end;

procedure TSedaiBytecodeVM.ExecuteGe;
var
  Right, Left: Variant;
begin
  CheckStackUnderflow(2);
  Right := Pop;
  Left := Pop;
  PushVariant(Integer(Ord(Double(Left) >= Double(Right))));
end;

// === OPTIMIZED INTEGER-ONLY COMPARISONS ===

procedure TSedaiBytecodeVM.ExecuteEqInt;
var
  Right, Left: Integer;
begin
  CheckStackUnderflow(2);
  Right := PopInt;
  Left := PopInt;
  PushVariant(Integer(Ord(Left = Right)));
end;

procedure TSedaiBytecodeVM.ExecuteNeqInt;
var
  Right, Left: Integer;
begin
  CheckStackUnderflow(2);
  Right := PopInt;
  Left := PopInt;
  PushVariant(Integer(Ord(Left <> Right)));
end;

procedure TSedaiBytecodeVM.ExecuteLtInt;
var
  Right, Left: Integer;
begin
  CheckStackUnderflow(2);
  Right := PopInt;
  Left := PopInt;
  PushVariant(Integer(Ord(Left < Right)));
end;

procedure TSedaiBytecodeVM.ExecuteGtInt;
var
  Right, Left: Integer;
begin
  CheckStackUnderflow(2);
  Right := PopInt;
  Left := PopInt;
  PushVariant(Integer(Ord(Left > Right)));
end;

procedure TSedaiBytecodeVM.ExecuteLeInt;
var
  Right, Left: Integer;
begin
  CheckStackUnderflow(2);
  Right := PopInt;
  Left := PopInt;
  PushVariant(Integer(Ord(Left <= Right)));
end;

procedure TSedaiBytecodeVM.ExecuteGeInt;
var
  Right, Left: Integer;
begin
  CheckStackUnderflow(2);
  Right := PopInt;
  Left := PopInt;
  PushVariant(Integer(Ord(Left >= Right)));
end;

// === PHASE 2: FLOAT COMPARISONS ===

procedure TSedaiBytecodeVM.ExecuteEqFloat;
var
  Right, Left: Double;
begin
  CheckStackUnderflow(2);
  Right := PopFloat;
  Left := PopFloat;
  PushVariant(Integer(Ord(Left = Right)));
end;

procedure TSedaiBytecodeVM.ExecuteNeqFloat;
var
  Right, Left: Double;
begin
  CheckStackUnderflow(2);
  Right := PopFloat;
  Left := PopFloat;
  PushVariant(Integer(Ord(Left <> Right)));
end;

procedure TSedaiBytecodeVM.ExecuteLtFloat;
var
  Right, Left: Double;
begin
  CheckStackUnderflow(2);
  Right := PopFloat;
  Left := PopFloat;
  PushVariant(Integer(Ord(Left < Right)));
end;

procedure TSedaiBytecodeVM.ExecuteGtFloat;
var
  Right, Left: Double;
begin
  CheckStackUnderflow(2);
  Right := PopFloat;
  Left := PopFloat;
  PushVariant(Integer(Ord(Left > Right)));
end;

procedure TSedaiBytecodeVM.ExecuteLeFloat;
var
  Right, Left: Double;
begin
  CheckStackUnderflow(2);
  Right := PopFloat;
  Left := PopFloat;
  PushVariant(Integer(Ord(Left <= Right)));
end;

procedure TSedaiBytecodeVM.ExecuteGeFloat;
var
  Right, Left: Double;
begin
  CheckStackUnderflow(2);
  Right := PopFloat;
  Left := PopFloat;
  PushVariant(Integer(Ord(Left >= Right)));
end;

// === LOGICAL OPERATIONS ===

procedure TSedaiBytecodeVM.ExecuteAnd;
var
  Right, Left: Variant;
begin
  CheckStackUnderflow(2);
  Right := Pop;
  Left := Pop;
  PushVariant(Integer(Ord((Double(Left) <> 0.0) and (Double(Right) <> 0.0))));
end;

procedure TSedaiBytecodeVM.ExecuteOr;
var
  Right, Left: Variant;
begin
  CheckStackUnderflow(2);
  Right := Pop;
  Left := Pop;
  PushVariant(Integer(Ord((Double(Left) <> 0.0) or (Double(Right) <> 0.0))));
end;

procedure TSedaiBytecodeVM.ExecuteNot;
var
  Value: Variant;
begin
  CheckStackUnderflow(1);
  Value := Pop;
  PushVariant(Integer(Ord(Double(Value) = 0.0)));
end;

// === I/O OPERATIONS ===

procedure TSedaiBytecodeVM.ExecutePrint;
var
  Text: string;
begin
  CheckStackUnderflow(1);
  // PHASE 3: Direct conversion from typed stack (no Variant!)
  Text := PopStr;

  if Assigned(FOutput) then
    FOutput.Print(Text);
end;

procedure TSedaiBytecodeVM.ExecutePrintNewline;
begin
  if Assigned(FOutput) then
    FOutput.NewLine;
end;

procedure TSedaiBytecodeVM.ExecuteInput(VarIdx: Integer);
var
  VarName: string;
  Value: string;
  NumValue: Double;
begin
  VarName := FProgram.Variables[VarIdx];

  // Unified INPUT logic: same behavior as SedaiBasic2Tester (sedaiexecutor.pas:732-821)
  // Determine variable type from suffix and read appropriate input
  if (Length(VarName) > 0) then
  begin
    case VarName[Length(VarName)] of
      '$':  // String variable - any input allowed
        begin
          if Assigned(FInput) then
            Value := FInput.ReadLine('? ', False, False, True)
          else
            Value := FContext.ReadInput('? ');
          FContext.SetVariable(VarName, Value);
        end;

      '%':  // Integer variable - numeric only, no decimals
        begin
          if Assigned(FInput) then
            Value := FInput.ReadLine('? ', False, True, False)  // NumericOnly=True, AllowDecimal=False
          else
            Value := FContext.ReadInput('? ');

          if TryStrToFloat(Value, NumValue) then
            FContext.SetVariable(VarName, Trunc(NumValue))  // Truncate to integer
          else
            FContext.SetVariable(VarName, 0);
        end;

      else  // Float variable: '!', '#' or no suffix - numeric with decimals
        begin
          if Assigned(FInput) then
            Value := FInput.ReadLine('? ', False, True, True)  // NumericOnly=True, AllowDecimal=True
          else
            Value := FContext.ReadInput('? ');

          if TryStrToFloat(Value, NumValue) then
            FContext.SetVariable(VarName, NumValue)
          else
            FContext.SetVariable(VarName, 0.0);
        end;
    end;
  end;

  // Check for CTRL+END stop request after input completes
  if Assigned(FInput) and FInput.ShouldStop then
  begin
    FBreakRequested := True;
    FInput.ClearStopRequest;
  end;
end;

// === BUILT-IN FUNCTIONS ===

procedure TSedaiBytecodeVM.ExecuteCallBuiltin(ArgCount: Integer);
var
  FuncName: string;
  Args: array of Variant;
  Result: Variant;
  i: Integer;
  V, V2: Double;  // PHASE 4F: Fast-path variables
begin
  // Get function name from next instruction (opNop with string index)
  if FPC < FProgram.CodeSize then
  begin
    FuncName := FProgram.Strings[FProgram.Code[FPC].IntArg];
    Inc(FPC);
  end
  else
    RaiseRuntimeError('Missing function name');

  // PHASE 4F: Fast-path for common math functions (avoid context lookup overhead)
  // This eliminates UpperCase, IndexOf, and indirect call overhead
  if ArgCount = 1 then
  begin
    // Single-argument math functions
    if (FuncName = 'SQR') or (FuncName = 'sqr') then
    begin
      V := Pop;
      PushVariant(Sqrt(V));
      Exit;
    end
    else if (FuncName = 'ABS') or (FuncName = 'abs') then
    begin
      V := Pop;
      PushVariant(Abs(V));
      Exit;
    end
    else if (FuncName = 'SIN') or (FuncName = 'sin') then
    begin
      V := Pop;
      PushVariant(Sin(V));
      Exit;
    end
    else if (FuncName = 'COS') or (FuncName = 'cos') then
    begin
      V := Pop;
      PushVariant(Cos(V));
      Exit;
    end
    else if (FuncName = 'TAN') or (FuncName = 'tan') then
    begin
      V := Pop;
      PushVariant(Tan(V));
      Exit;
    end
    else if (FuncName = 'EXP') or (FuncName = 'exp') then
    begin
      V := Pop;
      PushVariant(Exp(V));
      Exit;
    end
    else if (FuncName = 'LOG') or (FuncName = 'log') then
    begin
      V := Pop;
      PushVariant(Ln(V));
      Exit;
    end
    else if (FuncName = 'INT') or (FuncName = 'int') then
    begin
      V := Pop;
      PushVariant(Int(V));
      Exit;
    end
    else if (FuncName = 'RND') or (FuncName = 'rnd') then
    begin
      V := Pop;  // x parameter
      // RND(x): returns float [0.0, 1.0)
      // If x <> 0: set RandSeed using x + counter to ensure uniqueness
      // If x = 0: continue sequence (don't modify RandSeed)
      if V <> 0 then
      begin
        Inc(FRndCounter);
        if FRndCounter > High(Cardinal) - 1 then
          FRndCounter := 0;
        RandSeed := Trunc(Abs(V)) + Integer(FRndCounter);
      end;
      PushVariant(Random);  // Random without args returns float [0.0, 1.0)
      Exit;
    end;
  end
  else if ArgCount = 2 then
  begin
    // Two-argument RND: RND(x, y)
    if (FuncName = 'RND') or (FuncName = 'rnd') then
    begin
      // RND(x, y): returns Int64 in range [x, y)
      // Uses counter-based seed for each call
      V := Pop;   // y (max)
      Result := Pop; // x (min)

      // Update RandSeed using counter to ensure different values each call
      Inc(FRndCounter);
      if FRndCounter > High(Cardinal) - 1 then
        FRndCounter := 0;
      RandSeed := Integer(FRndCounter);

      // If x > y, use Int64.MaxValue as limit
      if Result > V then
        V := High(Int64);

      // RandomRange returns value in [min, max) - exactly what we need
      PushVariant(RandomRange(Trunc(Result), Trunc(V)));
      Exit;
    end;
  end
  else if ArgCount = 3 then
  begin
    // Three-argument RND: RND(x, y, s)
    if (FuncName = 'RND') or (FuncName = 'rnd') then
    begin
      // RND(x, y, s): returns Int64 in range [x, y)
      // If s <> 0: use s + counter as seed, otherwise just use counter
      // Pop in reverse order: s, y, x
      V2 := Pop;  // s (seed modifier)
      V := Pop;   // y (max)
      Result := Pop; // x (min)

      // Update RandSeed using counter + optional seed modifier
      Inc(FRndCounter);
      if FRndCounter > High(Cardinal) - 1 then
        FRndCounter := 0;
      if V2 <> 0 then
        RandSeed := Trunc(Abs(V2)) + Integer(FRndCounter)
      else
        RandSeed := Integer(FRndCounter);

      // If x > y, use Int64.MaxValue as limit
      if Result > V then
        V := High(Int64);

      // Return random integer in range [x, y)
      PushVariant(RandomRange(Trunc(Result), Trunc(V)));
      Exit;
    end;
  end;

  // Fallback to full context lookup for other functions
  SetLength(Args, ArgCount);
  for i := ArgCount - 1 downto 0 do
    Args[i] := Pop;

  try
    Result := FContext.CallFunction(FuncName, Args);
    PushVariant(Result);
  except
    on E: Exception do
      RaiseRuntimeError('Function ' + FuncName + ': ' + E.Message);
  end;
end;

// === CONTROL ===

procedure TSedaiBytecodeVM.Reset;
var
  I: Integer;
begin
  FPC := 0;
  FCallSP := 0;
  FHalted := False;
  FBreakRequested := False;
  FInstructionsExecuted := 0;

  // Clear all registers
  for I := 0 to 31 do
  begin
    FIntRegisters[I] := 0;
    FFloatRegisters[I] := 0.0;
    FStrRegisters[I] := '';
  end;
end;

procedure TSedaiBytecodeVM.Break;
begin
  FBreakRequested := True;
end;

// === ERROR HANDLING ===

procedure TSedaiBytecodeVM.RaiseRuntimeError(const Msg: string);
var
  SourceLine: Integer;
begin
  SourceLine := FProgram.LineNumbers.FindSourceLine(FPC);
  if SourceLine > 0 then
    raise Exception.CreateFmt('Runtime error at line %d: %s', [SourceLine, Msg])
  else
    raise Exception.CreateFmt('Runtime error: %s', [Msg]);
end;

// === STATISTICS ===

function TSedaiBytecodeVM.GetExecutionTime: Double;
begin
  if FEndTime > 0 then
    Result := MilliSecondsBetween(FEndTime, FStartTime)
  else
    Result := MilliSecondsBetween(Now, FStartTime);
end;

function TSedaiBytecodeVM.GetInstructionsPerSecond: Double;
var
  ElapsedSec: Double;
begin
  ElapsedSec := GetExecutionTime / 1000.0;
  if ElapsedSec > 0 then
    Result := FInstructionsExecuted / ElapsedSec
  else
    Result := 0;
end;

// === ARRAY OPERATIONS ===

procedure TSedaiBytecodeVM.ExecuteDimArray(VarIdx, DimCount: Integer);
var
  VarName: string;
  Dimensions: TArrayDimensions;
  i: Integer;
begin
  CheckStackUnderflow(DimCount);
  
  // Pop dimensions from stack (already in correct order)
  SetLength(Dimensions, DimCount);
  for i := DimCount - 1 downto 0 do
    Dimensions[i] := PopInt;
  
  // Get variable name
  if (VarIdx < 0) or (VarIdx >= FProgram.Variables.Count) then
    RaiseRuntimeError(Format('Invalid variable index: %d', [VarIdx]));
  
  VarName := FProgram.Variables[VarIdx];
  
  // Allocate array in context
  FContext.DimensionArray(VarName, Dimensions);
end;

procedure TSedaiBytecodeVM.ExecuteLoadArray(VarIdx, IndexCount: Integer);
var
  VarName: string;
  Indices: TArrayDimensions;
  i: Integer;
  Value: Variant;
begin
  CheckStackUnderflow(IndexCount);
  
  // Pop indices from stack
  SetLength(Indices, IndexCount);
  for i := IndexCount - 1 downto 0 do
    Indices[i] := PopInt;
  
  // Get variable name
  if (VarIdx < 0) or (VarIdx >= FProgram.Variables.Count) then
    RaiseRuntimeError(Format('Invalid variable index: %d', [VarIdx]));
  
  VarName := FProgram.Variables[VarIdx];
  
  // Load array element
  if IndexCount = 1 then
    Value := FContext.GetArrayElement(VarName, Indices[0])
  else
    Value := FContext.GetArrayElement(VarName, Indices);
  PushVariant(Value);
end;

procedure TSedaiBytecodeVM.ExecuteStoreArray(VarIdx, IndexCount: Integer);
var
  VarName: string;
  Indices: TArrayDimensions;
  i: Integer;
  Value: Variant;
begin
  CheckStackUnderflow(IndexCount + 1);
  
  // Pop value first (pushed last)
  Value := Pop;
  
  // Pop indices
  SetLength(Indices, IndexCount);
  for i := IndexCount - 1 downto 0 do
    Indices[i] := PopInt;
  
  // Get variable name
  if (VarIdx < 0) or (VarIdx >= FProgram.Variables.Count) then
    RaiseRuntimeError(Format('Invalid variable index: %d', [VarIdx]));
  
  VarName := FProgram.Variables[VarIdx];
  
  // Store array element
  if IndexCount = 1 then
    FContext.SetArrayElement(VarName, Indices[0], Value)
  else
    FContext.SetArrayElement(VarName, Indices, Value);
end;

procedure TSedaiBytecodeVM.ExecuteLoadArray1DFast(VarIdx: Integer);
var
  VarName: string;
  Index: Integer;
  Value: Variant;
begin
  // OPTIMIZED: 1D array load with inline access
  // Stack: [..., index] -> [..., value]
  
  CheckStackUnderflow(1);
  Index := PopInt;
  
  // Inline variable name lookup (no validation in hot path for speed)
  VarName := FProgram.Variables[VarIdx];
  
  // Direct single-index array access
  Value := FContext.GetArrayElement(VarName, Index);
  PushVariant(Value);
end;

procedure TSedaiBytecodeVM.ExecuteStoreArray1DFast(VarIdx: Integer);
var
  VarName: string;
  Index: Integer;
  Value: Variant;
begin
  // OPTIMIZED: 1D array store with inline access
  // Stack: [..., index, value] -> [...]
  
  CheckStackUnderflow(2);
  Value := Pop;   // Value pushed last
  Index := PopInt;
  
  // Inline variable name lookup
  VarName := FProgram.Variables[VarIdx];
  
  // Direct single-index array access
  FContext.SetArrayElement(VarName, Index, Value);
end;

// === PHASE 4: TYPED ARRAY OPERATIONS ===

procedure TSedaiBytecodeVM.ExecuteLoadArray1DInt(VarIdx: Integer);
var
  Index: Integer;
  Value: Variant;
begin
  // PHASE 4: Type-specialized 1D array load for Integer
  // Stack: [..., index] -> [..., intValue]
  
  CheckStackUnderflow(1);
  Index := PopInt;
  
  // Use direct indexed access (faster)
  Value := FContext.GetArrayElementByIndex(VarIdx, Index);
  
  // Push as Integer (no Variant overhead on stack)
  PushVariant(Int64(Value));
end;

procedure TSedaiBytecodeVM.ExecuteStoreArray1DInt(VarIdx: Integer);
var
  Index: Integer;
  Value: Integer;
begin
  // PHASE 4: Type-specialized 1D array store for Integer
  // Stack: [..., index, intValue] -> [...]
  
  CheckStackUnderflow(2);
  Value := PopInt;  // Already integer
  Index := PopInt;
  
  // Use direct indexed access (faster)
  FContext.SetArrayElementByIndex(VarIdx, Index, Value);
end;

// === PROFILING METHODS ===

function OpCodeToString(Op: TOpCode): string;
begin
  WriteStr(Result, Op);
end;

procedure TSedaiBytecodeVM.EnableProfiling;
var
  Op: TOpCode;
begin
  FProfilingEnabled := True;
  // Reset counters
  for Op := Low(TOpCode) to High(TOpCode) do
    FOpcodeCounts[Op] := 0;
end;

procedure TSedaiBytecodeVM.DisableProfiling;
begin
  FProfilingEnabled := False;
end;

function TSedaiBytecodeVM.GetOpcodeCount(OpCode: TOpCode): Int64;
begin
  Result := FOpcodeCounts[OpCode];
end;

function TSedaiBytecodeVM.GetTopOpcodes(TopN: Integer): string;
type
  TOpcodeStats = record
    OpCode: TOpCode;
    Count: Int64;
    Percentage: Double;
  end;
var
  Stats: array of TOpcodeStats;
  i, j: Integer;
  Op: TOpCode;
  Total: Int64;
  Temp: TOpcodeStats;
begin
  // Calculate total
  Total := 0;
  for Op := Low(TOpCode) to High(TOpCode) do
    Total := Total + FOpcodeCounts[Op];
    
  if Total = 0 then
  begin
    Result := 'No profiling data available';
    Exit;
  end;
  
  // Collect stats
  SetLength(Stats, Ord(High(TOpCode)) + 1);
  for Op := Low(TOpCode) to High(TOpCode) do
  begin
    Stats[Ord(Op)].OpCode := Op;
    Stats[Ord(Op)].Count := FOpcodeCounts[Op];
    Stats[Ord(Op)].Percentage := (FOpcodeCounts[Op] / Total) * 100;
  end;
  
  // Bubble sort by count (descending)
  for i := 0 to High(Stats) - 1 do
    for j := i + 1 to High(Stats) do
      if Stats[j].Count > Stats[i].Count then
      begin
        Temp := Stats[i];
        Stats[i] := Stats[j];
        Stats[j] := Temp;
      end;
  
  // Build result string
  Result := Format('Top %d opcodes (total: %d instructions):', [TopN, Total]) + sLineBreak;
  for i := 0 to Min(TopN - 1, High(Stats)) do
  begin
    if Stats[i].Count > 0 then
      Result := Result + Format('  %-20s: %10d  (%.2f%%)',  [
        OpCodeToString(Stats[i].OpCode),
        Stats[i].Count,
        Stats[i].Percentage
      ]) + sLineBreak;
  end;
end;

// === THREADED CODE HANDLERS ===
// These procedures are called directly via function pointers,
// eliminating case statement overhead and branch misprediction

{$REGION 'Opcode Handlers - Stack Operations'}

procedure HandlePushConst(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  ConstValue: Variant;
begin
  // REGISTER-BASED: Load constant to R0 (temp register for stack emulation)
  ConstValue := VM.FProgram.Constants[Inst.ConstIndex];
  if VarIsNumeric(ConstValue) then
  begin
    if VarType(ConstValue) in [varByte, varSmallInt, varInteger, varInt64, varWord, varLongWord, varQWord] then
      VM.FIntRegisters[0] := Int64(ConstValue)
    else
      VM.FFloatRegisters[0] := Double(ConstValue);
  end
  else if VarIsStr(ConstValue) then
    VM.FStrRegisters[0] := VarToStr(ConstValue)
  else
    VM.FIntRegisters[0] := 0;
end;

procedure HandlePush0(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // REGISTER-BASED: Load immediate 0 to R0
  VM.FIntRegisters[0] := 0;
end;

procedure HandlePush1(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // REGISTER-BASED: Load immediate 1 to R0
  VM.FIntRegisters[0] := 1;
end;

procedure HandlePushM1(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // REGISTER-BASED: Load immediate -1 to R0
  VM.FIntRegisters[0] := -1;
end;

procedure HandlePop(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // REGISTER-BASED: No-op (was stack discard operation)
  // In register architecture, this becomes a NOP or register deallocation hint
end;

procedure HandleDup(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // REGISTER-BASED: R1 := R0 (duplicate top of virtual stack)
  VM.FIntRegisters[1] := VM.FIntRegisters[0];
  VM.FFloatRegisters[1] := VM.FFloatRegisters[0];
  VM.FStrRegisters[1] := VM.FStrRegisters[0];
end;

procedure HandleRotTwo(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  TempInt: Int64;
  TempFloat: Double;
  TempStr: string;
begin
  // REGISTER-BASED: Swap R0 and R1
  TempInt := VM.FIntRegisters[0];
  TempFloat := VM.FFloatRegisters[0];
  TempStr := VM.FStrRegisters[0];

  VM.FIntRegisters[0] := VM.FIntRegisters[1];
  VM.FFloatRegisters[0] := VM.FFloatRegisters[1];
  VM.FStrRegisters[0] := VM.FStrRegisters[1];

  VM.FIntRegisters[1] := TempInt;
  VM.FFloatRegisters[1] := TempFloat;
  VM.FStrRegisters[1] := TempStr;
end;

{$ENDREGION}

{$REGION 'Opcode Handlers - Variables'}

procedure HandleLoadVar(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // REGISTER-BASED: Load variable to R0/F0/S0 based on type (NO VARIANT!)
  case VM.FVarTypes[Inst.VarIndex] of
    vmvtInteger: VM.FIntRegisters[0] := VM.FContext.GetVariableByIndexInt(Inst.VarIndex);
    vmvtFloat:   VM.FFloatRegisters[0] := VM.FContext.GetVariableByIndexFloat(Inst.VarIndex);
    vmvtString:  VM.FStrRegisters[0] := VM.FContext.GetVariableByIndexStr(Inst.VarIndex);
  end;
end;

procedure HandleStoreVar(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // REGISTER-BASED: Store from R0/F0/S0 to variable based on type (NO VARIANT!)
  case VM.FVarTypes[Inst.VarIndex] of
    vmvtInteger: VM.FContext.SetVariableByIndexInt(Inst.VarIndex, VM.FIntRegisters[0]);
    vmvtFloat:   VM.FContext.SetVariableByIndexFloat(Inst.VarIndex, VM.FFloatRegisters[0]);
    vmvtString:  VM.FContext.SetVariableByIndexStr(Inst.VarIndex, VM.FStrRegisters[0]);
  end;
end;

{ REGISTER-BASED: Typed variable operations (ZERO VARIANT!) }

procedure HandleLoadVarInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // Load Int64 variable to R0 (pure register operation)
  VM.FIntRegisters[0] := VM.FContext.GetVariableByIndexInt(Inst.VarIndex);
end;

procedure HandleStoreVarInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // Store R0 to Int64 variable (pure register operation)
  VM.FContext.SetVariableByIndexInt(Inst.VarIndex, VM.FIntRegisters[0]);
end;

procedure HandleLoadVarFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // Load Double variable to F0 (pure register operation)
  VM.FFloatRegisters[0] := VM.FContext.GetVariableByIndexFloat(Inst.VarIndex);
end;

procedure HandleStoreVarFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // Store F0 to Double variable (pure register operation)
  VM.FContext.SetVariableByIndexFloat(Inst.VarIndex, VM.FFloatRegisters[0]);
end;

procedure HandleLoadVarString(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // Load String variable to S0 (pure register operation)
  VM.FStrRegisters[0] := VM.FContext.GetVariableByIndexStr(Inst.VarIndex);
end;

procedure HandleStoreVarString(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // Store S0 to String variable (pure register operation)
  VM.FContext.SetVariableByIndexStr(Inst.VarIndex, VM.FStrRegisters[0]);
end;

procedure HandleIncVarInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  Value: Int64;
begin
  // Increment Int64 variable by 1 (NO VARIANT!)
  Value := VM.FContext.GetVariableByIndexInt(Inst.VarIndex);
  VM.FContext.SetVariableByIndexInt(Inst.VarIndex, Value + 1);
end;

procedure HandleDecVarInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  Value: Int64;
begin
  // Decrement Int64 variable by 1 (NO VARIANT!)
  Value := VM.FContext.GetVariableByIndexInt(Inst.VarIndex);
  VM.FContext.SetVariableByIndexInt(Inst.VarIndex, Value - 1);
end;

procedure HandleIncVarFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  Value: Double;
begin
  // Increment Double variable by 1.0 (NO VARIANT!)
  Value := VM.FContext.GetVariableByIndexFloat(Inst.VarIndex);
  VM.FContext.SetVariableByIndexFloat(Inst.VarIndex, Value + 1.0);
end;

procedure HandleDecVarFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  Value: Double;
begin
  // Decrement Double variable by 1.0 (NO VARIANT!)
  Value := VM.FContext.GetVariableByIndexFloat(Inst.VarIndex);
  VM.FContext.SetVariableByIndexFloat(Inst.VarIndex, Value - 1.0);
end;

{$ENDREGION}

{$REGION 'Opcode Handlers - Arithmetic'}

procedure HandleAdd(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteAdd;
end;

procedure HandleSub(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteSub;
end;

procedure HandleMul(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteMul;
end;

procedure HandleDiv(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteDiv;
end;

procedure HandleMod(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteMod;
end;

procedure HandlePow(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecutePow;
end;

procedure HandleNeg(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteNeg;
end;

procedure HandleAddInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteAddInt;
end;

procedure HandleSubInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteSubInt;
end;

procedure HandleMulInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteMulInt;
end;

procedure HandleDivInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteDivInt;
end;

procedure HandleModInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteModInt;
end;

procedure HandleNegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteNegInt;
end;

{$ENDREGION}

{$REGION 'Opcode Handlers - Float Arithmetic (PHASE 2)'}

procedure HandleAddFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteAddFloat;
end;

procedure HandleSubFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteSubFloat;
end;

procedure HandleMulFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteMulFloat;
end;

procedure HandleDivFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteDivFloat;
end;

procedure HandleNegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteNegFloat;
end;

procedure HandlePowFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecutePowFloat;
end;

{$ENDREGION}

{$REGION 'Opcode Handlers - Comparisons'}

procedure HandleEq(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteEq;
end;

procedure HandleNeq(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteNeq;
end;

procedure HandleLt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteLt;
end;

procedure HandleGt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteGt;
end;

procedure HandleLe(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteLe;
end;

procedure HandleGe(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteGe;
end;

procedure HandleEqInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteEqInt;
end;

procedure HandleNeqInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteNeqInt;
end;

procedure HandleLtInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteLtInt;
end;

procedure HandleGtInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteGtInt;
end;

procedure HandleLeInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteLeInt;
end;

procedure HandleGeInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteGeInt;
end;

{$ENDREGION}

{$REGION 'Opcode Handlers - Float Comparisons (PHASE 2)'}

procedure HandleEqFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteEqFloat;
end;

procedure HandleNeqFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteNeqFloat;
end;

procedure HandleLtFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteLtFloat;
end;

procedure HandleGtFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteGtFloat;
end;

procedure HandleLeFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteLeFloat;
end;

procedure HandleGeFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteGeFloat;
end;

{$ENDREGION}

{$REGION 'Opcode Handlers - Logic'}

procedure HandleAnd(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteAnd;
end;

procedure HandleOr(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteOr;
end;

procedure HandleNot(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteNot;
end;

{$ENDREGION}

{$REGION 'Opcode Handlers - Control Flow'}

procedure HandleJump(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  NewPC: Integer;
begin
  NewPC := Inst.Offset;
  if (NewPC >= 0) and (NewPC < VM.FProgram.CodeSize) then
    VM.FPC := NewPC
  else
    VM.RaiseRuntimeError(Format('Invalid jump target: %d', [NewPC]));
end;

procedure HandleJumpIfFalse(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  NewPC: Integer;
begin
  if not VM.PopBool then
  begin
    NewPC := Inst.Offset;
    if (NewPC >= 0) and (NewPC < VM.FProgram.CodeSize) then
      VM.FPC := NewPC
    else
      VM.RaiseRuntimeError(Format('Invalid jump target: %d', [NewPC]));
  end;
end;

procedure HandleJumpIfTrue(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  NewPC: Integer;
begin
  if VM.PopBool then
  begin
    NewPC := Inst.Offset;
    if (NewPC >= 0) and (NewPC < VM.FProgram.CodeSize) then
      VM.FPC := NewPC
    else
      VM.RaiseRuntimeError(Format('Invalid jump target: %d', [NewPC]));
  end;
end;

procedure HandlePopJumpIfFalse(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  NewPC: Integer;
begin
  if not VM.PopBool then
  begin
    NewPC := Inst.Offset;
    if (NewPC >= 0) and (NewPC < VM.FProgram.CodeSize) then
      VM.FPC := NewPC
    else
      VM.RaiseRuntimeError(Format('Invalid jump target: %d', [NewPC]));
  end;
end;

procedure HandlePopJumpIfTrue(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  NewPC: Integer;
begin
  if VM.PopBool then
  begin
    NewPC := Inst.Offset;
    if (NewPC >= 0) and (NewPC < VM.FProgram.CodeSize) then
      VM.FPC := NewPC
    else
      VM.RaiseRuntimeError(Format('Invalid jump target: %d', [NewPC]));
  end;
end;

procedure HandleCall(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  NewPC: Integer;
begin
  // Push return address (next instruction) onto call stack
  if VM.FCallSP >= Length(VM.FCallStack) then
    VM.RaiseRuntimeError('Call stack overflow');
  VM.FCallStack[VM.FCallSP] := VM.FPC;
  Inc(VM.FCallSP);
  // Jump to target (Inst.Offset contains the jump target)
  NewPC := Inst.Offset;
  if (NewPC >= 0) and (NewPC < VM.FProgram.CodeSize) then
    VM.FPC := NewPC
  else
    VM.RaiseRuntimeError(Format('Invalid call target: %d', [NewPC]));
end;

procedure HandleReturn(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // Pop return address from call stack
  if VM.FCallSP > 0 then
  begin
    Dec(VM.FCallSP);
    VM.FPC := VM.FCallStack[VM.FCallSP];
  end
  else
    VM.FHalted := True;  // No more return addresses, halt program
end;

{$ENDREGION}

{$REGION 'Opcode Handlers - I/O'}

procedure HandlePrint(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecutePrint;
end;

procedure HandlePrintNewline(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecutePrintNewline;
end;

procedure HandleInput(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteInput(Inst.VarIndex);
end;

{$ENDREGION}

{$REGION 'Opcode Handlers - Functions & Arrays'}

procedure HandleCallBuiltin(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteCallBuiltin(Inst.IntArg);
end;

procedure HandleDimArray(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  Dimensions: TArrayDimensions;
  i, DimCount, ArrayIndex: Integer;
begin
  // TIER 2 OPTIMIZATION: Direct indexed array dimension
  ArrayIndex := Inst.TwoArgs.Arg1;
  DimCount := Inst.TwoArgs.Arg2;

  VM.CheckStackUnderflow(DimCount);

  SetLength(Dimensions, DimCount);
  for i := DimCount - 1 downto 0 do
    Dimensions[i] := VM.PopInt;

  VM.FContext.DimensionArrayByIndex(ArrayIndex, Dimensions);
end;

procedure HandleDimArrayTyped(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  Dimensions: TArrayDimensions;
  i, DimCount, ArrayIndex, StorageType: Integer;
begin
  // TYPED ARRAYS: Direct indexed array dimension with native storage type
  ArrayIndex := Inst.DimArrayTyped.ArrayIdx;
  DimCount := Inst.DimArrayTyped.DimCount;
  StorageType := Inst.DimArrayTyped.StorageType;

  VM.CheckStackUnderflow(DimCount);

  SetLength(Dimensions, DimCount);
  for i := DimCount - 1 downto 0 do
    Dimensions[i] := VM.PopInt;

  // Call typed dimension method with storage type
  VM.FContext.DimensionArrayByIndex(ArrayIndex, Dimensions,
    SedaiExecutorContext.TArrayStorageType(StorageType));
end;

procedure HandleLoadArray(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteLoadArray(Inst.TwoArgs.Arg1, Inst.TwoArgs.Arg2);
end;

procedure HandleStoreArray(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteStoreArray(Inst.TwoArgs.Arg1, Inst.TwoArgs.Arg2);
end;

procedure HandleLoadArray1D(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  Index: Integer;
begin
  // TIER 2 OPTIMIZATION: Direct indexed array access
  VM.CheckStackUnderflow(1);
  Index := VM.PopInt;
  VM.PushVariant(VM.FContext.GetArrayElementByIndex(Inst.IntArg, Index));
end;

procedure HandleStoreArray1D(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  Index: Integer;
  Value: Variant;
begin
  // TIER 2 OPTIMIZATION: Direct indexed array access
  VM.CheckStackUnderflow(2);
  Value := VM.Pop;
  Index := VM.PopInt;
  VM.FContext.SetArrayElementByIndex(Inst.IntArg, Index, Value);
end;

// DAY 5: Register-based array access handlers
procedure HandleLoadArray1DReg(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  ArrayVar, RegIdx: Integer;
  Index: Variant;
  IntIndex: Int64;
begin
  // Load array[Reg[idx]] - no LoadReg needed!
  // TwoArgs: Arg1 = array variable, Arg2 = register index
  ArrayVar := Inst.TwoArgs.Arg1;
  RegIdx := Inst.TwoArgs.Arg2;
  
  if (RegIdx >= 0) and (RegIdx < MAX_REGISTERS) then
  begin
    // DAY 6: Read from typed register if applicable
    if (RegIdx >= 0) and (RegIdx < 16) then
      IntIndex := VM.FIntRegisters[RegIdx]  // Typed Int64 register! (already integer)
    else if (RegIdx >= 16) and (RegIdx < 32) then
      IntIndex := Trunc(VM.FFloatRegisters[RegIdx])  // Typed Double register! (convert to integer)
    else
      IntIndex := VarAsType(VM.FRegisters[RegIdx], varInt64);  // Legacy Variant register
      
    VM.PushVariant(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  end
  else
    raise Exception.CreateFmt('Invalid register index: %d', [RegIdx]);
end;

procedure HandleStoreArray1DReg(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  ArrayVar, RegIdx: Integer;
  IntIndex: Int64;
  Value: Variant;
begin
  // Store TOS to array[Reg[idx]] - no LoadReg needed!
  // TwoArgs: Arg1 = array variable, Arg2 = register index
  ArrayVar := Inst.TwoArgs.Arg1;
  RegIdx := Inst.TwoArgs.Arg2;
  
  if (RegIdx >= 0) and (RegIdx < MAX_REGISTERS) then
  begin
    VM.CheckStackUnderflow(1);
    Value := VM.Pop;
    
    // DAY 6: Read from typed register if applicable
    if (RegIdx >= 0) and (RegIdx < 16) then
      IntIndex := VM.FIntRegisters[RegIdx]  // Typed Int64 register! (already integer)
    else if (RegIdx >= 16) and (RegIdx < 32) then
      IntIndex := Trunc(VM.FFloatRegisters[RegIdx])  // Typed Double register! (convert to integer)
    else
      IntIndex := VarAsType(VM.FRegisters[RegIdx], varInt64);  // Legacy Variant register
      
    VM.FContext.SetArrayElementByIndex(ArrayVar, IntIndex, Value);
  end
  else
    raise Exception.CreateFmt('Invalid register index: %d', [RegIdx]);
end;

// PHASE 4: Typed array handlers
procedure HandleLoadArray1DInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteLoadArray1DInt(Inst.IntArg);
end;

procedure HandleStoreArray1DInt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.ExecuteStoreArray1DInt(Inst.IntArg);
end;

// === DAY 6 PHASE 3: FUSED ARRAY COMPARE+JUMP ===
// These handlers combine: LoadArray + Compare + Jump into ONE instruction!
// Pattern: IF FLAGS(I%) = 0 THEN GOTO ... (4 instructions → 1 instruction!)

procedure HandleJumpIfArrayEqImmInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, RegIdx, ImmValue: Integer;
  IntIndex: Int64;
  ArrayValueInt: Integer;
begin
  // Extract parameters from ArrayRegImmOffset encoding
  ArrayVar := Inst.ArrayRegImmOffset.ArrayVar;
  RegIdx := Inst.ArrayRegImmOffset.RegIdx;
  ImmValue := Inst.ArrayRegImmOffset.ImmValue;
  
  // Get array index from register (typed Int64 register)
  if (RegIdx >= 0) and (RegIdx < 16) then
    IntIndex := VM.FIntRegisters[RegIdx]
  else if (RegIdx >= 16) and (RegIdx < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[RegIdx - 16])  // BUG FIX: FloatRegisters are 0-based!
  else
    IntIndex := VarAsType(VM.FRegisters[RegIdx], varInt64);
  
  // Load array element DIRECTLY as Integer (NO Variant overhead!)
  ArrayValueInt := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  // Compare and jump if equal
  if ArrayValueInt = ImmValue then
    VM.FPC := Inst.ArrayRegImmOffset.JumpOffset;
  // else: PC already incremented by main loop, do nothing
end;

procedure HandleJumpIfArrayNeImmInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, RegIdx, ImmValue: Integer;
  IntIndex: Int64;
  ArrayValueInt: Integer;
begin
  // Extract parameters from ArrayRegImmOffset encoding
  ArrayVar := Inst.ArrayRegImmOffset.ArrayVar;
  RegIdx := Inst.ArrayRegImmOffset.RegIdx;
  ImmValue := Inst.ArrayRegImmOffset.ImmValue;
  
  // Get array index from register (typed Int64 register)
  if (RegIdx >= 0) and (RegIdx < 16) then
    IntIndex := VM.FIntRegisters[RegIdx]
  else if (RegIdx >= 16) and (RegIdx < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[RegIdx - 16])  // BUG FIX: FloatRegisters are 0-based!
  else
    IntIndex := VarAsType(VM.FRegisters[RegIdx], varInt64);
  
  // Load array element DIRECTLY as Integer (NO Variant overhead!)
  ArrayValueInt := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  // Compare and jump if NOT equal
  if ArrayValueInt <> ImmValue then
    VM.FPC := Inst.ArrayRegImmOffset.JumpOffset;
  // else: PC already incremented by main loop, do nothing
end;

// === PHASE 4A: EXTENDED FUSED ARRAY COMPARE+JUMP ===

procedure HandleJumpIfArrayLtImmInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, RegIdx, ImmValue: Integer;
  IntIndex: Int64;
  ArrayValueInt: Integer;
begin
  ArrayVar := Inst.ArrayRegImmOffset.ArrayVar;
  RegIdx := Inst.ArrayRegImmOffset.RegIdx;
  ImmValue := Inst.ArrayRegImmOffset.ImmValue;
  
  if (RegIdx >= 0) and (RegIdx < 16) then
    IntIndex := VM.FIntRegisters[RegIdx]
  else if (RegIdx >= 16) and (RegIdx < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[RegIdx - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[RegIdx], varInt64);
  
  ArrayValueInt := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if ArrayValueInt < ImmValue then
    VM.FPC := Inst.ArrayRegImmOffset.JumpOffset;
end;

procedure HandleJumpIfArrayLeImmInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, RegIdx, ImmValue: Integer;
  IntIndex: Int64;
  ArrayValueInt: Integer;
begin
  ArrayVar := Inst.ArrayRegImmOffset.ArrayVar;
  RegIdx := Inst.ArrayRegImmOffset.RegIdx;
  ImmValue := Inst.ArrayRegImmOffset.ImmValue;
  
  if (RegIdx >= 0) and (RegIdx < 16) then
    IntIndex := VM.FIntRegisters[RegIdx]
  else if (RegIdx >= 16) and (RegIdx < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[RegIdx - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[RegIdx], varInt64);
  
  ArrayValueInt := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if ArrayValueInt <= ImmValue then
    VM.FPC := Inst.ArrayRegImmOffset.JumpOffset;
end;

procedure HandleJumpIfArrayGtImmInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, RegIdx, ImmValue: Integer;
  IntIndex: Int64;
  ArrayValueInt: Integer;
begin
  ArrayVar := Inst.ArrayRegImmOffset.ArrayVar;
  RegIdx := Inst.ArrayRegImmOffset.RegIdx;
  ImmValue := Inst.ArrayRegImmOffset.ImmValue;
  
  if (RegIdx >= 0) and (RegIdx < 16) then
    IntIndex := VM.FIntRegisters[RegIdx]
  else if (RegIdx >= 16) and (RegIdx < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[RegIdx - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[RegIdx], varInt64);
  
  ArrayValueInt := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if ArrayValueInt > ImmValue then
    VM.FPC := Inst.ArrayRegImmOffset.JumpOffset;
end;

procedure HandleJumpIfArrayGeImmInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, RegIdx, ImmValue: Integer;
  IntIndex: Int64;
  ArrayValueInt: Integer;
begin
  ArrayVar := Inst.ArrayRegImmOffset.ArrayVar;
  RegIdx := Inst.ArrayRegImmOffset.RegIdx;
  ImmValue := Inst.ArrayRegImmOffset.ImmValue;
  
  if (RegIdx >= 0) and (RegIdx < 16) then
    IntIndex := VM.FIntRegisters[RegIdx]
  else if (RegIdx >= 16) and (RegIdx < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[RegIdx - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[RegIdx], varInt64);
  
  ArrayValueInt := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if ArrayValueInt >= ImmValue then
    VM.FPC := Inst.ArrayRegImmOffset.JumpOffset;
end;

// === PHASE 4A: EXTENDED FUSED ARRAY COMPARE+JUMP (FLOAT) ===

procedure HandleJumpIfArrayEqImmFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, RegIdx: Integer;
  ImmValueFloat: Double;
  IntIndex: Int64;
  ArrayValueFloat: Double;
begin
  ArrayVar := Inst.ArrayRegImmOffset.ArrayVar;
  RegIdx := Inst.ArrayRegImmOffset.RegIdx;
  ImmValueFloat := Double(Inst.ArrayRegImmOffset.ImmValue);  // Cast Int to Double
  
  if (RegIdx >= 0) and (RegIdx < 16) then
    IntIndex := VM.FIntRegisters[RegIdx]
  else if (RegIdx >= 16) and (RegIdx < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[RegIdx - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[RegIdx], varInt64);
  
  ArrayValueFloat := Double(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if ArrayValueFloat = ImmValueFloat then
    VM.FPC := Inst.ArrayRegImmOffset.JumpOffset;
end;

procedure HandleJumpIfArrayNeImmFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, RegIdx: Integer;
  ImmValueFloat: Double;
  IntIndex: Int64;
  ArrayValueFloat: Double;
begin
  ArrayVar := Inst.ArrayRegImmOffset.ArrayVar;
  RegIdx := Inst.ArrayRegImmOffset.RegIdx;
  ImmValueFloat := Double(Inst.ArrayRegImmOffset.ImmValue);

  if (RegIdx >= 0) and (RegIdx < 16) then
    IntIndex := VM.FIntRegisters[RegIdx]
  else if (RegIdx >= 16) and (RegIdx < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[RegIdx - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[RegIdx], varInt64);

  ArrayValueFloat := Double(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));

  if ArrayValueFloat <> ImmValueFloat then
    VM.FPC := Inst.ArrayRegImmOffset.JumpOffset;
end;

procedure HandleJumpIfArrayLtImmFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, RegIdx: Integer;
  ImmValueFloat: Double;
  IntIndex: Int64;
  ArrayValueFloat: Double;
begin
  ArrayVar := Inst.ArrayRegImmOffset.ArrayVar;
  RegIdx := Inst.ArrayRegImmOffset.RegIdx;
  ImmValueFloat := Double(Inst.ArrayRegImmOffset.ImmValue);
  
  if (RegIdx >= 0) and (RegIdx < 16) then
    IntIndex := VM.FIntRegisters[RegIdx]
  else if (RegIdx >= 16) and (RegIdx < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[RegIdx - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[RegIdx], varInt64);
  
  ArrayValueFloat := Double(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if ArrayValueFloat < ImmValueFloat then
    VM.FPC := Inst.ArrayRegImmOffset.JumpOffset;
end;

procedure HandleJumpIfArrayLeImmFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, RegIdx: Integer;
  ImmValueFloat: Double;
  IntIndex: Int64;
  ArrayValueFloat: Double;
begin
  ArrayVar := Inst.ArrayRegImmOffset.ArrayVar;
  RegIdx := Inst.ArrayRegImmOffset.RegIdx;
  ImmValueFloat := Double(Inst.ArrayRegImmOffset.ImmValue);
  
  if (RegIdx >= 0) and (RegIdx < 16) then
    IntIndex := VM.FIntRegisters[RegIdx]
  else if (RegIdx >= 16) and (RegIdx < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[RegIdx - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[RegIdx], varInt64);
  
  ArrayValueFloat := Double(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if ArrayValueFloat <= ImmValueFloat then
    VM.FPC := Inst.ArrayRegImmOffset.JumpOffset;
end;

procedure HandleJumpIfArrayGtImmFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, RegIdx: Integer;
  ImmValueFloat: Double;
  IntIndex: Int64;
  ArrayValueFloat: Double;
begin
  ArrayVar := Inst.ArrayRegImmOffset.ArrayVar;
  RegIdx := Inst.ArrayRegImmOffset.RegIdx;
  ImmValueFloat := Double(Inst.ArrayRegImmOffset.ImmValue);
  
  if (RegIdx >= 0) and (RegIdx < 16) then
    IntIndex := VM.FIntRegisters[RegIdx]
  else if (RegIdx >= 16) and (RegIdx < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[RegIdx - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[RegIdx], varInt64);
  
  ArrayValueFloat := Double(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if ArrayValueFloat > ImmValueFloat then
    VM.FPC := Inst.ArrayRegImmOffset.JumpOffset;
end;

procedure HandleJumpIfArrayGeImmFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, RegIdx: Integer;
  ImmValueFloat: Double;
  IntIndex: Int64;
  ArrayValueFloat: Double;
begin
  ArrayVar := Inst.ArrayRegImmOffset.ArrayVar;
  RegIdx := Inst.ArrayRegImmOffset.RegIdx;
  ImmValueFloat := Double(Inst.ArrayRegImmOffset.ImmValue);
  
  if (RegIdx >= 0) and (RegIdx < 16) then
    IntIndex := VM.FIntRegisters[RegIdx]
  else if (RegIdx >= 16) and (RegIdx < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[RegIdx - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[RegIdx], varInt64);
  
  ArrayValueFloat := Double(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if ArrayValueFloat >= ImmValueFloat then
    VM.FPC := Inst.ArrayRegImmOffset.JumpOffset;
end;

// === PHASE 4B: ARRAY-ARRAY COMPARISON FUSION (INTEGER) ===

procedure HandleJumpIfArrayEqArrayInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar1, RegIdx1, ArrayVar2, RegIdx2: Integer;
  IntIndex1, IntIndex2: Int64;
  ArrayValue1, ArrayValue2: Integer;
begin
  ArrayVar1 := Inst.ArrayRegArrayRegOffset.ArrayVar1;
  RegIdx1 := Inst.ArrayRegArrayRegOffset.RegIdx1;
  ArrayVar2 := Inst.ArrayRegArrayRegOffset.ArrayVar2;
  RegIdx2 := Inst.ArrayRegArrayRegOffset.RegIdx2;
  
  if (RegIdx1 >= 0) and (RegIdx1 < 16) then
    IntIndex1 := VM.FIntRegisters[RegIdx1]
  else if (RegIdx1 >= 16) and (RegIdx1 < 32) then
    IntIndex1 := Trunc(VM.FFloatRegisters[RegIdx1 - 16])
  else
    IntIndex1 := VarAsType(VM.FRegisters[RegIdx1], varInt64);
  
  if (RegIdx2 >= 0) and (RegIdx2 < 16) then
    IntIndex2 := VM.FIntRegisters[RegIdx2]
  else if (RegIdx2 >= 16) and (RegIdx2 < 32) then
    IntIndex2 := Trunc(VM.FFloatRegisters[RegIdx2 - 16])
  else
    IntIndex2 := VarAsType(VM.FRegisters[RegIdx2], varInt64);
  
  ArrayValue1 := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar1, IntIndex1));
  ArrayValue2 := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar2, IntIndex2));
  
  if ArrayValue1 = ArrayValue2 then
    VM.FPC := Inst.ArrayRegArrayRegOffset.JumpOffset;
end;

procedure HandleJumpIfArrayNeArrayInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar1, RegIdx1, ArrayVar2, RegIdx2: Integer;
  IntIndex1, IntIndex2: Int64;
  ArrayValue1, ArrayValue2: Integer;
begin
  ArrayVar1 := Inst.ArrayRegArrayRegOffset.ArrayVar1;
  RegIdx1 := Inst.ArrayRegArrayRegOffset.RegIdx1;
  ArrayVar2 := Inst.ArrayRegArrayRegOffset.ArrayVar2;
  RegIdx2 := Inst.ArrayRegArrayRegOffset.RegIdx2;
  
  if (RegIdx1 >= 0) and (RegIdx1 < 16) then
    IntIndex1 := VM.FIntRegisters[RegIdx1]
  else if (RegIdx1 >= 16) and (RegIdx1 < 32) then
    IntIndex1 := Trunc(VM.FFloatRegisters[RegIdx1 - 16])
  else
    IntIndex1 := VarAsType(VM.FRegisters[RegIdx1], varInt64);
  
  if (RegIdx2 >= 0) and (RegIdx2 < 16) then
    IntIndex2 := VM.FIntRegisters[RegIdx2]
  else if (RegIdx2 >= 16) and (RegIdx2 < 32) then
    IntIndex2 := Trunc(VM.FFloatRegisters[RegIdx2 - 16])
  else
    IntIndex2 := VarAsType(VM.FRegisters[RegIdx2], varInt64);
  
  ArrayValue1 := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar1, IntIndex1));
  ArrayValue2 := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar2, IntIndex2));
  
  if ArrayValue1 <> ArrayValue2 then
    VM.FPC := Inst.ArrayRegArrayRegOffset.JumpOffset;
end;

procedure HandleJumpIfArrayLtArrayInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar1, RegIdx1, ArrayVar2, RegIdx2: Integer;
  IntIndex1, IntIndex2: Int64;
  ArrayValue1, ArrayValue2: Integer;
begin
  ArrayVar1 := Inst.ArrayRegArrayRegOffset.ArrayVar1;
  RegIdx1 := Inst.ArrayRegArrayRegOffset.RegIdx1;
  ArrayVar2 := Inst.ArrayRegArrayRegOffset.ArrayVar2;
  RegIdx2 := Inst.ArrayRegArrayRegOffset.RegIdx2;
  
  if (RegIdx1 >= 0) and (RegIdx1 < 16) then
    IntIndex1 := VM.FIntRegisters[RegIdx1]
  else if (RegIdx1 >= 16) and (RegIdx1 < 32) then
    IntIndex1 := Trunc(VM.FFloatRegisters[RegIdx1 - 16])
  else
    IntIndex1 := VarAsType(VM.FRegisters[RegIdx1], varInt64);
  
  if (RegIdx2 >= 0) and (RegIdx2 < 16) then
    IntIndex2 := VM.FIntRegisters[RegIdx2]
  else if (RegIdx2 >= 16) and (RegIdx2 < 32) then
    IntIndex2 := Trunc(VM.FFloatRegisters[RegIdx2 - 16])
  else
    IntIndex2 := VarAsType(VM.FRegisters[RegIdx2], varInt64);
  
  ArrayValue1 := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar1, IntIndex1));
  ArrayValue2 := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar2, IntIndex2));
  
  if ArrayValue1 < ArrayValue2 then
    VM.FPC := Inst.ArrayRegArrayRegOffset.JumpOffset;
end;

procedure HandleJumpIfArrayLeArrayInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar1, RegIdx1, ArrayVar2, RegIdx2: Integer;
  IntIndex1, IntIndex2: Int64;
  ArrayValue1, ArrayValue2: Integer;
begin
  ArrayVar1 := Inst.ArrayRegArrayRegOffset.ArrayVar1;
  RegIdx1 := Inst.ArrayRegArrayRegOffset.RegIdx1;
  ArrayVar2 := Inst.ArrayRegArrayRegOffset.ArrayVar2;
  RegIdx2 := Inst.ArrayRegArrayRegOffset.RegIdx2;
  
  if (RegIdx1 >= 0) and (RegIdx1 < 16) then
    IntIndex1 := VM.FIntRegisters[RegIdx1]
  else if (RegIdx1 >= 16) and (RegIdx1 < 32) then
    IntIndex1 := Trunc(VM.FFloatRegisters[RegIdx1 - 16])
  else
    IntIndex1 := VarAsType(VM.FRegisters[RegIdx1], varInt64);
  
  if (RegIdx2 >= 0) and (RegIdx2 < 16) then
    IntIndex2 := VM.FIntRegisters[RegIdx2]
  else if (RegIdx2 >= 16) and (RegIdx2 < 32) then
    IntIndex2 := Trunc(VM.FFloatRegisters[RegIdx2 - 16])
  else
    IntIndex2 := VarAsType(VM.FRegisters[RegIdx2], varInt64);
  
  ArrayValue1 := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar1, IntIndex1));
  ArrayValue2 := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar2, IntIndex2));
  
  if ArrayValue1 <= ArrayValue2 then
    VM.FPC := Inst.ArrayRegArrayRegOffset.JumpOffset;
end;

procedure HandleJumpIfArrayGtArrayInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar1, RegIdx1, ArrayVar2, RegIdx2: Integer;
  IntIndex1, IntIndex2: Int64;
  ArrayValue1, ArrayValue2: Integer;
begin
  ArrayVar1 := Inst.ArrayRegArrayRegOffset.ArrayVar1;
  RegIdx1 := Inst.ArrayRegArrayRegOffset.RegIdx1;
  ArrayVar2 := Inst.ArrayRegArrayRegOffset.ArrayVar2;
  RegIdx2 := Inst.ArrayRegArrayRegOffset.RegIdx2;
  
  if (RegIdx1 >= 0) and (RegIdx1 < 16) then
    IntIndex1 := VM.FIntRegisters[RegIdx1]
  else if (RegIdx1 >= 16) and (RegIdx1 < 32) then
    IntIndex1 := Trunc(VM.FFloatRegisters[RegIdx1 - 16])
  else
    IntIndex1 := VarAsType(VM.FRegisters[RegIdx1], varInt64);
  
  if (RegIdx2 >= 0) and (RegIdx2 < 16) then
    IntIndex2 := VM.FIntRegisters[RegIdx2]
  else if (RegIdx2 >= 16) and (RegIdx2 < 32) then
    IntIndex2 := Trunc(VM.FFloatRegisters[RegIdx2 - 16])
  else
    IntIndex2 := VarAsType(VM.FRegisters[RegIdx2], varInt64);
  
  ArrayValue1 := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar1, IntIndex1));
  ArrayValue2 := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar2, IntIndex2));
  
  if ArrayValue1 > ArrayValue2 then
    VM.FPC := Inst.ArrayRegArrayRegOffset.JumpOffset;
end;

procedure HandleJumpIfArrayGeArrayInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar1, RegIdx1, ArrayVar2, RegIdx2: Integer;
  IntIndex1, IntIndex2: Int64;
  ArrayValue1, ArrayValue2: Integer;
begin
  ArrayVar1 := Inst.ArrayRegArrayRegOffset.ArrayVar1;
  RegIdx1 := Inst.ArrayRegArrayRegOffset.RegIdx1;
  ArrayVar2 := Inst.ArrayRegArrayRegOffset.ArrayVar2;
  RegIdx2 := Inst.ArrayRegArrayRegOffset.RegIdx2;
  
  if (RegIdx1 >= 0) and (RegIdx1 < 16) then
    IntIndex1 := VM.FIntRegisters[RegIdx1]
  else if (RegIdx1 >= 16) and (RegIdx1 < 32) then
    IntIndex1 := Trunc(VM.FFloatRegisters[RegIdx1 - 16])
  else
    IntIndex1 := VarAsType(VM.FRegisters[RegIdx1], varInt64);
  
  if (RegIdx2 >= 0) and (RegIdx2 < 16) then
    IntIndex2 := VM.FIntRegisters[RegIdx2]
  else if (RegIdx2 >= 16) and (RegIdx2 < 32) then
    IntIndex2 := Trunc(VM.FFloatRegisters[RegIdx2 - 16])
  else
    IntIndex2 := VarAsType(VM.FRegisters[RegIdx2], varInt64);
  
  ArrayValue1 := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar1, IntIndex1));
  ArrayValue2 := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar2, IntIndex2));
  
  if ArrayValue1 >= ArrayValue2 then
    VM.FPC := Inst.ArrayRegArrayRegOffset.JumpOffset;
end;

// === PHASE 4B: ARRAY-ARRAY COMPARISON FUSION (FLOAT) ===

procedure HandleJumpIfArrayEqArrayFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar1, RegIdx1, ArrayVar2, RegIdx2: Integer;
  IntIndex1, IntIndex2: Int64;
  ArrayValue1, ArrayValue2: Double;
begin
  ArrayVar1 := Inst.ArrayRegArrayRegOffset.ArrayVar1;
  RegIdx1 := Inst.ArrayRegArrayRegOffset.RegIdx1;
  ArrayVar2 := Inst.ArrayRegArrayRegOffset.ArrayVar2;
  RegIdx2 := Inst.ArrayRegArrayRegOffset.RegIdx2;
  
  if (RegIdx1 >= 0) and (RegIdx1 < 16) then
    IntIndex1 := VM.FIntRegisters[RegIdx1]
  else if (RegIdx1 >= 16) and (RegIdx1 < 32) then
    IntIndex1 := Trunc(VM.FFloatRegisters[RegIdx1 - 16])
  else
    IntIndex1 := VarAsType(VM.FRegisters[RegIdx1], varInt64);
  
  if (RegIdx2 >= 0) and (RegIdx2 < 16) then
    IntIndex2 := VM.FIntRegisters[RegIdx2]
  else if (RegIdx2 >= 16) and (RegIdx2 < 32) then
    IntIndex2 := Trunc(VM.FFloatRegisters[RegIdx2 - 16])
  else
    IntIndex2 := VarAsType(VM.FRegisters[RegIdx2], varInt64);
  
  ArrayValue1 := Double(VM.FContext.GetArrayElementByIndex(ArrayVar1, IntIndex1));
  ArrayValue2 := Double(VM.FContext.GetArrayElementByIndex(ArrayVar2, IntIndex2));
  
  if ArrayValue1 = ArrayValue2 then
    VM.FPC := Inst.ArrayRegArrayRegOffset.JumpOffset;
end;

procedure HandleJumpIfArrayNeArrayFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar1, RegIdx1, ArrayVar2, RegIdx2: Integer;
  IntIndex1, IntIndex2: Int64;
  ArrayValue1, ArrayValue2: Double;
begin
  ArrayVar1 := Inst.ArrayRegArrayRegOffset.ArrayVar1;
  RegIdx1 := Inst.ArrayRegArrayRegOffset.RegIdx1;
  ArrayVar2 := Inst.ArrayRegArrayRegOffset.ArrayVar2;
  RegIdx2 := Inst.ArrayRegArrayRegOffset.RegIdx2;
  
  if (RegIdx1 >= 0) and (RegIdx1 < 16) then
    IntIndex1 := VM.FIntRegisters[RegIdx1]
  else if (RegIdx1 >= 16) and (RegIdx1 < 32) then
    IntIndex1 := Trunc(VM.FFloatRegisters[RegIdx1 - 16])
  else
    IntIndex1 := VarAsType(VM.FRegisters[RegIdx1], varInt64);
  
  if (RegIdx2 >= 0) and (RegIdx2 < 16) then
    IntIndex2 := VM.FIntRegisters[RegIdx2]
  else if (RegIdx2 >= 16) and (RegIdx2 < 32) then
    IntIndex2 := Trunc(VM.FFloatRegisters[RegIdx2 - 16])
  else
    IntIndex2 := VarAsType(VM.FRegisters[RegIdx2], varInt64);
  
  ArrayValue1 := Double(VM.FContext.GetArrayElementByIndex(ArrayVar1, IntIndex1));
  ArrayValue2 := Double(VM.FContext.GetArrayElementByIndex(ArrayVar2, IntIndex2));
  
  if ArrayValue1 <> ArrayValue2 then
    VM.FPC := Inst.ArrayRegArrayRegOffset.JumpOffset;
end;

procedure HandleJumpIfArrayLtArrayFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar1, RegIdx1, ArrayVar2, RegIdx2: Integer;
  IntIndex1, IntIndex2: Int64;
  ArrayValue1, ArrayValue2: Double;
begin
  ArrayVar1 := Inst.ArrayRegArrayRegOffset.ArrayVar1;
  RegIdx1 := Inst.ArrayRegArrayRegOffset.RegIdx1;
  ArrayVar2 := Inst.ArrayRegArrayRegOffset.ArrayVar2;
  RegIdx2 := Inst.ArrayRegArrayRegOffset.RegIdx2;
  
  if (RegIdx1 >= 0) and (RegIdx1 < 16) then
    IntIndex1 := VM.FIntRegisters[RegIdx1]
  else if (RegIdx1 >= 16) and (RegIdx1 < 32) then
    IntIndex1 := Trunc(VM.FFloatRegisters[RegIdx1 - 16])
  else
    IntIndex1 := VarAsType(VM.FRegisters[RegIdx1], varInt64);
  
  if (RegIdx2 >= 0) and (RegIdx2 < 16) then
    IntIndex2 := VM.FIntRegisters[RegIdx2]
  else if (RegIdx2 >= 16) and (RegIdx2 < 32) then
    IntIndex2 := Trunc(VM.FFloatRegisters[RegIdx2 - 16])
  else
    IntIndex2 := VarAsType(VM.FRegisters[RegIdx2], varInt64);
  
  ArrayValue1 := Double(VM.FContext.GetArrayElementByIndex(ArrayVar1, IntIndex1));
  ArrayValue2 := Double(VM.FContext.GetArrayElementByIndex(ArrayVar2, IntIndex2));
  
  if ArrayValue1 < ArrayValue2 then
    VM.FPC := Inst.ArrayRegArrayRegOffset.JumpOffset;
end;

procedure HandleJumpIfArrayLeArrayFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar1, RegIdx1, ArrayVar2, RegIdx2: Integer;
  IntIndex1, IntIndex2: Int64;
  ArrayValue1, ArrayValue2: Double;
begin
  ArrayVar1 := Inst.ArrayRegArrayRegOffset.ArrayVar1;
  RegIdx1 := Inst.ArrayRegArrayRegOffset.RegIdx1;
  ArrayVar2 := Inst.ArrayRegArrayRegOffset.ArrayVar2;
  RegIdx2 := Inst.ArrayRegArrayRegOffset.RegIdx2;
  
  if (RegIdx1 >= 0) and (RegIdx1 < 16) then
    IntIndex1 := VM.FIntRegisters[RegIdx1]
  else if (RegIdx1 >= 16) and (RegIdx1 < 32) then
    IntIndex1 := Trunc(VM.FFloatRegisters[RegIdx1 - 16])
  else
    IntIndex1 := VarAsType(VM.FRegisters[RegIdx1], varInt64);
  
  if (RegIdx2 >= 0) and (RegIdx2 < 16) then
    IntIndex2 := VM.FIntRegisters[RegIdx2]
  else if (RegIdx2 >= 16) and (RegIdx2 < 32) then
    IntIndex2 := Trunc(VM.FFloatRegisters[RegIdx2 - 16])
  else
    IntIndex2 := VarAsType(VM.FRegisters[RegIdx2], varInt64);
  
  ArrayValue1 := Double(VM.FContext.GetArrayElementByIndex(ArrayVar1, IntIndex1));
  ArrayValue2 := Double(VM.FContext.GetArrayElementByIndex(ArrayVar2, IntIndex2));
  
  if ArrayValue1 <= ArrayValue2 then
    VM.FPC := Inst.ArrayRegArrayRegOffset.JumpOffset;
end;

procedure HandleJumpIfArrayGtArrayFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar1, RegIdx1, ArrayVar2, RegIdx2: Integer;
  IntIndex1, IntIndex2: Int64;
  ArrayValue1, ArrayValue2: Double;
begin
  ArrayVar1 := Inst.ArrayRegArrayRegOffset.ArrayVar1;
  RegIdx1 := Inst.ArrayRegArrayRegOffset.RegIdx1;
  ArrayVar2 := Inst.ArrayRegArrayRegOffset.ArrayVar2;
  RegIdx2 := Inst.ArrayRegArrayRegOffset.RegIdx2;
  
  if (RegIdx1 >= 0) and (RegIdx1 < 16) then
    IntIndex1 := VM.FIntRegisters[RegIdx1]
  else if (RegIdx1 >= 16) and (RegIdx1 < 32) then
    IntIndex1 := Trunc(VM.FFloatRegisters[RegIdx1 - 16])
  else
    IntIndex1 := VarAsType(VM.FRegisters[RegIdx1], varInt64);
  
  if (RegIdx2 >= 0) and (RegIdx2 < 16) then
    IntIndex2 := VM.FIntRegisters[RegIdx2]
  else if (RegIdx2 >= 16) and (RegIdx2 < 32) then
    IntIndex2 := Trunc(VM.FFloatRegisters[RegIdx2 - 16])
  else
    IntIndex2 := VarAsType(VM.FRegisters[RegIdx2], varInt64);
  
  ArrayValue1 := Double(VM.FContext.GetArrayElementByIndex(ArrayVar1, IntIndex1));
  ArrayValue2 := Double(VM.FContext.GetArrayElementByIndex(ArrayVar2, IntIndex2));
  
  if ArrayValue1 > ArrayValue2 then
    VM.FPC := Inst.ArrayRegArrayRegOffset.JumpOffset;
end;

procedure HandleJumpIfArrayGeArrayFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar1, RegIdx1, ArrayVar2, RegIdx2: Integer;
  IntIndex1, IntIndex2: Int64;
  ArrayValue1, ArrayValue2: Double;
begin
  ArrayVar1 := Inst.ArrayRegArrayRegOffset.ArrayVar1;
  RegIdx1 := Inst.ArrayRegArrayRegOffset.RegIdx1;
  ArrayVar2 := Inst.ArrayRegArrayRegOffset.ArrayVar2;
  RegIdx2 := Inst.ArrayRegArrayRegOffset.RegIdx2;
  
  if (RegIdx1 >= 0) and (RegIdx1 < 16) then
    IntIndex1 := VM.FIntRegisters[RegIdx1]
  else if (RegIdx1 >= 16) and (RegIdx1 < 32) then
    IntIndex1 := Trunc(VM.FFloatRegisters[RegIdx1 - 16])
  else
    IntIndex1 := VarAsType(VM.FRegisters[RegIdx1], varInt64);
  
  if (RegIdx2 >= 0) and (RegIdx2 < 16) then
    IntIndex2 := VM.FIntRegisters[RegIdx2]
  else if (RegIdx2 >= 16) and (RegIdx2 < 32) then
    IntIndex2 := Trunc(VM.FFloatRegisters[RegIdx2 - 16])
  else
    IntIndex2 := VarAsType(VM.FRegisters[RegIdx2], varInt64);
  
  ArrayValue1 := Double(VM.FContext.GetArrayElementByIndex(ArrayVar1, IntIndex1));
  ArrayValue2 := Double(VM.FContext.GetArrayElementByIndex(ArrayVar2, IntIndex2));
  
  if ArrayValue1 >= ArrayValue2 then
    VM.FPC := Inst.ArrayRegArrayRegOffset.JumpOffset;
end;

// === PHASE 4C: ARITHMETIC FUSION (Array load + arithmetic) ===

procedure HandleLoadArrayAddImmInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, ImmValue, DstReg: Integer;
  IntIndex: Int64;
  ArrayValueInt: Integer;
begin
  // IntReg[dst] = IntArray[IntReg[idx]] + imm (FUSED operation - NO stack!)
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  ImmValue := Inst.ArrayRegImmReg.ImmValue;
  DstReg := Inst.ArrayRegImmReg.DstReg;
  
  // Get array index from register
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  // Load array element DIRECTLY as Integer
  ArrayValueInt := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  // Perform arithmetic and store to destination register (FUSED!)
  if (DstReg >= 0) and (DstReg < 16) then
    VM.FIntRegisters[DstReg] := ArrayValueInt + ImmValue
  else
    VM.FRegisters[DstReg] := ArrayValueInt + ImmValue;
end;

procedure HandleLoadArraySubImmInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, ImmValue, DstReg: Integer;
  IntIndex: Int64;
  ArrayValueInt: Integer;
begin
  // IntReg[dst] = IntArray[IntReg[idx]] - imm (FUSED operation - NO stack!)
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  ImmValue := Inst.ArrayRegImmReg.ImmValue;
  DstReg := Inst.ArrayRegImmReg.DstReg;
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  ArrayValueInt := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if (DstReg >= 0) and (DstReg < 16) then
    VM.FIntRegisters[DstReg] := ArrayValueInt - ImmValue
  else
    VM.FRegisters[DstReg] := ArrayValueInt - ImmValue;
end;

procedure HandleLoadArrayMulImmInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, ImmValue, DstReg: Integer;
  IntIndex: Int64;
  ArrayValueInt: Integer;
begin
  // IntReg[dst] = IntArray[IntReg[idx]] * imm (FUSED operation - NO stack!)
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  ImmValue := Inst.ArrayRegImmReg.ImmValue;
  DstReg := Inst.ArrayRegImmReg.DstReg;
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  ArrayValueInt := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if (DstReg >= 0) and (DstReg < 16) then
    VM.FIntRegisters[DstReg] := ArrayValueInt * ImmValue
  else
    VM.FRegisters[DstReg] := ArrayValueInt * ImmValue;
end;

procedure HandleStoreArrayAddImmInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, SrcReg, ImmValue: Integer;
  IntIndex: Int64;
  RegValue: Int64;
begin
  // IntArray[IntReg[idx]] = IntReg[src] + imm (FUSED operation - NO stack!)
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  SrcReg := Inst.ArrayRegImmReg.DstReg;  // Note: reusing DstReg field for SrcReg
  ImmValue := Inst.ArrayRegImmReg.ImmValue;
  
  // Get array index from register
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  // Get source register value
  if (SrcReg >= 0) and (SrcReg < 16) then
    RegValue := VM.FIntRegisters[SrcReg]
  else
    RegValue := VarAsType(VM.FRegisters[SrcReg], varInt64);
  
  // Store (RegValue + Imm) to array (FUSED!)
  VM.FContext.SetArrayElementByIndex(ArrayVar, IntIndex, RegValue + ImmValue);
end;

procedure HandleStoreArraySubImmInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, SrcReg, ImmValue: Integer;
  IntIndex: Int64;
  RegValue: Int64;
begin
  // IntArray[IntReg[idx]] = IntReg[src] - imm (FUSED operation - NO stack!)
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  SrcReg := Inst.ArrayRegImmReg.DstReg;
  ImmValue := Inst.ArrayRegImmReg.ImmValue;
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  if (SrcReg >= 0) and (SrcReg < 16) then
    RegValue := VM.FIntRegisters[SrcReg]
  else
    RegValue := VarAsType(VM.FRegisters[SrcReg], varInt64);
  
  VM.FContext.SetArrayElementByIndex(ArrayVar, IntIndex, RegValue - ImmValue);
end;

procedure HandleStoreArrayMulImmInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, SrcReg, ImmValue: Integer;
  IntIndex: Int64;
  RegValue: Int64;
begin
  // IntArray[IntReg[idx]] = IntReg[src] * imm (FUSED operation - NO stack!)
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  SrcReg := Inst.ArrayRegImmReg.DstReg;
  ImmValue := Inst.ArrayRegImmReg.ImmValue;
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  if (SrcReg >= 0) and (SrcReg < 16) then
    RegValue := VM.FIntRegisters[SrcReg]
  else
    RegValue := VarAsType(VM.FRegisters[SrcReg], varInt64);
  
  VM.FContext.SetArrayElementByIndex(ArrayVar, IntIndex, RegValue * ImmValue);
end;

// === PHASE 4C: ARITHMETIC FUSION (FLOAT) ===

procedure HandleLoadArrayAddImmFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, DstReg: Integer;
  ImmValueFloat: Double;
  IntIndex: Int64;
  ArrayValueFloat: Double;
begin
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  ImmValueFloat := Double(Inst.ArrayRegImmReg.ImmValue);
  DstReg := Inst.ArrayRegImmReg.DstReg;
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  ArrayValueFloat := Double(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if (DstReg >= 16) and (DstReg < 32) then
    VM.FFloatRegisters[DstReg - 16] := ArrayValueFloat + ImmValueFloat
  else
    VM.FRegisters[DstReg] := ArrayValueFloat + ImmValueFloat;
end;

procedure HandleLoadArraySubImmFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, DstReg: Integer;
  ImmValueFloat: Double;
  IntIndex: Int64;
  ArrayValueFloat: Double;
begin
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  ImmValueFloat := Double(Inst.ArrayRegImmReg.ImmValue);
  DstReg := Inst.ArrayRegImmReg.DstReg;
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  ArrayValueFloat := Double(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if (DstReg >= 16) and (DstReg < 32) then
    VM.FFloatRegisters[DstReg - 16] := ArrayValueFloat - ImmValueFloat
  else
    VM.FRegisters[DstReg] := ArrayValueFloat - ImmValueFloat;
end;

procedure HandleLoadArrayMulImmFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, DstReg: Integer;
  ImmValueFloat: Double;
  IntIndex: Int64;
  ArrayValueFloat: Double;
begin
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  ImmValueFloat := Double(Inst.ArrayRegImmReg.ImmValue);
  DstReg := Inst.ArrayRegImmReg.DstReg;
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  ArrayValueFloat := Double(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if (DstReg >= 16) and (DstReg < 32) then
    VM.FFloatRegisters[DstReg - 16] := ArrayValueFloat * ImmValueFloat
  else
    VM.FRegisters[DstReg] := ArrayValueFloat * ImmValueFloat;
end;

procedure HandleStoreArrayAddImmFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, SrcReg: Integer;
  ImmValueFloat: Double;
  IntIndex: Int64;
  RegValue: Double;
begin
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  SrcReg := Inst.ArrayRegImmReg.DstReg;
  ImmValueFloat := Double(Inst.ArrayRegImmReg.ImmValue);
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  if (SrcReg >= 16) and (SrcReg < 32) then
    RegValue := VM.FFloatRegisters[SrcReg - 16]
  else
    RegValue := VarAsType(VM.FRegisters[SrcReg], varDouble);
  
  VM.FContext.SetArrayElementByIndex(ArrayVar, IntIndex, RegValue + ImmValueFloat);
end;

procedure HandleStoreArraySubImmFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, SrcReg: Integer;
  ImmValueFloat: Double;
  IntIndex: Int64;
  RegValue: Double;
begin
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  SrcReg := Inst.ArrayRegImmReg.DstReg;
  ImmValueFloat := Double(Inst.ArrayRegImmReg.ImmValue);
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  if (SrcReg >= 16) and (SrcReg < 32) then
    RegValue := VM.FFloatRegisters[SrcReg - 16]
  else
    RegValue := VarAsType(VM.FRegisters[SrcReg], varDouble);
  
  VM.FContext.SetArrayElementByIndex(ArrayVar, IntIndex, RegValue - ImmValueFloat);
end;

procedure HandleStoreArrayMulImmFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, SrcReg: Integer;
  ImmValueFloat: Double;
  IntIndex: Int64;
  RegValue: Double;
begin
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  SrcReg := Inst.ArrayRegImmReg.DstReg;
  ImmValueFloat := Double(Inst.ArrayRegImmReg.ImmValue);
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  if (SrcReg >= 16) and (SrcReg < 32) then
    RegValue := VM.FFloatRegisters[SrcReg - 16]
  else
    RegValue := VarAsType(VM.FRegisters[SrcReg], varDouble);
  
  VM.FContext.SetArrayElementByIndex(ArrayVar, IntIndex, RegValue * ImmValueFloat);
end;

// === PHASE 4D: LOAD-USE FUSION (INTEGER) ===
// Direct load-use: Reg[dst] = Array[Reg[idx]] + imm (NO stack operations!)

procedure HandleLoadArrayAddImmToRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, DstReg, ImmValue: Integer;
  IntIndex: Int64;
  ArrayValueInt: Integer;
begin
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  ImmValue := Inst.ArrayRegImmReg.ImmValue;
  DstReg := Inst.ArrayRegImmReg.DstReg;
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  ArrayValueInt := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if (DstReg >= 0) and (DstReg < 16) then
    VM.FIntRegisters[DstReg] := ArrayValueInt + ImmValue
  else
    VM.FRegisters[DstReg] := ArrayValueInt + ImmValue;
end;

procedure HandleLoadArraySubImmToRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, DstReg, ImmValue: Integer;
  IntIndex: Int64;
  ArrayValueInt: Integer;
begin
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  ImmValue := Inst.ArrayRegImmReg.ImmValue;
  DstReg := Inst.ArrayRegImmReg.DstReg;
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  ArrayValueInt := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if (DstReg >= 0) and (DstReg < 16) then
    VM.FIntRegisters[DstReg] := ArrayValueInt - ImmValue
  else
    VM.FRegisters[DstReg] := ArrayValueInt - ImmValue;
end;

procedure HandleLoadArrayMulImmToRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, DstReg, ImmValue: Integer;
  IntIndex: Int64;
  ArrayValueInt: Integer;
begin
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  ImmValue := Inst.ArrayRegImmReg.ImmValue;
  DstReg := Inst.ArrayRegImmReg.DstReg;
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  ArrayValueInt := Integer(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if (DstReg >= 0) and (DstReg < 16) then
    VM.FIntRegisters[DstReg] := ArrayValueInt * ImmValue
  else
    VM.FRegisters[DstReg] := ArrayValueInt * ImmValue;
end;

// === PHASE 4D: LOAD-USE FUSION (FLOAT) ===
// Direct load-use: FloatReg[dst] = FloatArray[Reg[idx]] + imm (NO stack operations!)

procedure HandleLoadArrayAddImmToRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, DstReg: Integer;
  ImmValueFloat: Double;
  IntIndex: Int64;
  ArrayValueFloat: Double;
begin
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  ImmValueFloat := Double(Inst.ArrayRegImmReg.ImmValue);
  DstReg := Inst.ArrayRegImmReg.DstReg;
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  ArrayValueFloat := Double(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if (DstReg >= 16) and (DstReg < 32) then
    VM.FFloatRegisters[DstReg - 16] := ArrayValueFloat + ImmValueFloat
  else
    VM.FRegisters[DstReg] := ArrayValueFloat + ImmValueFloat;
end;

procedure HandleLoadArraySubImmToRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, DstReg: Integer;
  ImmValueFloat: Double;
  IntIndex: Int64;
  ArrayValueFloat: Double;
begin
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  ImmValueFloat := Double(Inst.ArrayRegImmReg.ImmValue);
  DstReg := Inst.ArrayRegImmReg.DstReg;
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  ArrayValueFloat := Double(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if (DstReg >= 16) and (DstReg < 32) then
    VM.FFloatRegisters[DstReg - 16] := ArrayValueFloat - ImmValueFloat
  else
    VM.FRegisters[DstReg] := ArrayValueFloat - ImmValueFloat;
end;

procedure HandleLoadArrayMulImmToRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, DstReg: Integer;
  ImmValueFloat: Double;
  IntIndex: Int64;
  ArrayValueFloat: Double;
begin
  ArrayVar := Inst.ArrayRegImmReg.ArrayVar;
  IdxReg := Inst.ArrayRegImmReg.IdxReg;
  ImmValueFloat := Double(Inst.ArrayRegImmReg.ImmValue);
  DstReg := Inst.ArrayRegImmReg.DstReg;
  
  if (IdxReg >= 0) and (IdxReg < 16) then
    IntIndex := VM.FIntRegisters[IdxReg]
  else if (IdxReg >= 16) and (IdxReg < 32) then
    IntIndex := Trunc(VM.FFloatRegisters[IdxReg - 16])
  else
    IntIndex := VarAsType(VM.FRegisters[IdxReg], varInt64);
  
  ArrayValueFloat := Double(VM.FContext.GetArrayElementByIndex(ArrayVar, IntIndex));
  
  if (DstReg >= 16) and (DstReg < 32) then
    VM.FFloatRegisters[DstReg - 16] := ArrayValueFloat * ImmValueFloat
  else
    VM.FRegisters[DstReg] := ArrayValueFloat * ImmValueFloat;
end;

{$ENDREGION}

{$REGION 'Opcode Handlers - Phase 4E: String & Division Fusion'}

// === PHASE 4E: STRING OPERATIONS FUSION ===

procedure HandleStrConcatReg(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  DstStrReg: Byte;
  SrcVar1, SrcVar2: SmallInt;
  Str1, Str2: string;
begin
  // StrReg[dst] = StrVar[src1] + StrVar[src2] (NO STACK!)
  DstStrReg := Inst.StrConcat.DstStrReg;
  SrcVar1 := Inst.StrConcat.SrcVar1;
  SrcVar2 := Inst.StrConcat.SrcVar2;
  
  // Get source strings from variables
  Str1 := VarToStr(VM.FContext.GetVariableByIndex(SrcVar1));
  Str2 := VarToStr(VM.FContext.GetVariableByIndex(SrcVar2));
  
  // Fused concatenation - store directly to string register
  if (DstStrReg >= 0) and (DstStrReg < 16) then
    VM.FStrRegisters[DstStrReg] := Str1 + Str2
  else
    raise Exception.Create('Invalid string register index: ' + IntToStr(DstStrReg));
end;

procedure HandleStrCmpRegEq(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  DstIntReg: Byte;
  SrcVar1, SrcVar2: SmallInt;
  Str1, Str2: string;
begin
  // IntReg[dst] = (StrVar[src1] == StrVar[src2]) ? 1 : 0
  DstIntReg := Inst.StrCmp.DstIntReg;
  SrcVar1 := Inst.StrCmp.SrcVar1;
  SrcVar2 := Inst.StrCmp.SrcVar2;
  
  Str1 := VarToStr(VM.FContext.GetVariableByIndex(SrcVar1));
  Str2 := VarToStr(VM.FContext.GetVariableByIndex(SrcVar2));
  
  if (DstIntReg >= 0) and (DstIntReg < 16) then
    VM.FIntRegisters[DstIntReg] := Ord(Str1 = Str2)  // 1 if equal, 0 if not
  else
    raise Exception.Create('Invalid integer register index: ' + IntToStr(DstIntReg));
end;

procedure HandleStrCmpRegNe(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  DstIntReg: Byte;
  SrcVar1, SrcVar2: SmallInt;
  Str1, Str2: string;
begin
  // IntReg[dst] = (StrVar[src1] != StrVar[src2]) ? 1 : 0
  DstIntReg := Inst.StrCmp.DstIntReg;
  SrcVar1 := Inst.StrCmp.SrcVar1;
  SrcVar2 := Inst.StrCmp.SrcVar2;
  
  Str1 := VarToStr(VM.FContext.GetVariableByIndex(SrcVar1));
  Str2 := VarToStr(VM.FContext.GetVariableByIndex(SrcVar2));
  
  if (DstIntReg >= 0) and (DstIntReg < 16) then
    VM.FIntRegisters[DstIntReg] := Ord(Str1 <> Str2)  // 1 if not equal, 0 if equal
  else
    raise Exception.Create('Invalid integer register index: ' + IntToStr(DstIntReg));
end;

procedure HandleStrCmpRegLt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  DstIntReg: Byte;
  SrcVar1, SrcVar2: SmallInt;
  Str1, Str2: string;
begin
  // IntReg[dst] = (StrVar[src1] < StrVar[src2]) ? 1 : 0 (lexicographic)
  DstIntReg := Inst.StrCmp.DstIntReg;
  SrcVar1 := Inst.StrCmp.SrcVar1;
  SrcVar2 := Inst.StrCmp.SrcVar2;
  
  Str1 := VarToStr(VM.FContext.GetVariableByIndex(SrcVar1));
  Str2 := VarToStr(VM.FContext.GetVariableByIndex(SrcVar2));
  
  if (DstIntReg >= 0) and (DstIntReg < 16) then
    VM.FIntRegisters[DstIntReg] := Ord(Str1 < Str2)  // 1 if less, 0 otherwise
  else
    raise Exception.Create('Invalid integer register index: ' + IntToStr(DstIntReg));
end;

procedure HandleStrCmpRegGt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  DstIntReg: Byte;
  SrcVar1, SrcVar2: SmallInt;
  Str1, Str2: string;
begin
  // IntReg[dst] = (StrVar[src1] > StrVar[src2]) ? 1 : 0 (lexicographic)
  DstIntReg := Inst.StrCmp.DstIntReg;
  SrcVar1 := Inst.StrCmp.SrcVar1;
  SrcVar2 := Inst.StrCmp.SrcVar2;
  
  Str1 := VarToStr(VM.FContext.GetVariableByIndex(SrcVar1));
  Str2 := VarToStr(VM.FContext.GetVariableByIndex(SrcVar2));
  
  if (DstIntReg >= 0) and (DstIntReg < 16) then
    VM.FIntRegisters[DstIntReg] := Ord(Str1 > Str2)  // 1 if greater, 0 otherwise
  else
    raise Exception.Create('Invalid integer register index: ' + IntToStr(DstIntReg));
end;

// === PHASE 4E: DIVISION/MODULO FUSION ===

procedure HandleDivModFusionInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  QuotReg, ModReg: Byte;
  DividendVar, DivisorVar: SmallInt;
  Dividend, Divisor: Int64;
begin
  // IntReg[quot] = dividend / divisor; IntReg[mod] = dividend % divisor
  // FUSED operation - single CPU DIV produces both results!
  QuotReg := Inst.DivMod.QuotReg;
  ModReg := Inst.DivMod.ModReg;
  DividendVar := Inst.DivMod.DividendVar;
  DivisorVar := Inst.DivMod.DivisorVar;
  
  // PHASE 3: Get operands as integers using typed methods
  Dividend := VM.FContext.GetVariableByIndexInt(DividendVar);
  Divisor := VM.FContext.GetVariableByIndexInt(DivisorVar);
  
  // Zero-division guard
  if Divisor = 0 then
  begin
    // Set both results to 0 on division by zero (BASIC convention)
    if (QuotReg >= 0) and (QuotReg < 16) then
      VM.FIntRegisters[QuotReg] := 0;
    if (ModReg >= 0) and (ModReg < 16) then
      VM.FIntRegisters[ModReg] := 0;
    Exit;
  end;
  
  // Fused division and modulo (single CPU instruction on x86!)
  if (QuotReg >= 0) and (QuotReg < 16) then
    VM.FIntRegisters[QuotReg] := Dividend div Divisor
  else
    raise Exception.Create('Invalid quotient register index: ' + IntToStr(QuotReg));
  
  if (ModReg >= 0) and (ModReg < 16) then
    VM.FIntRegisters[ModReg] := Dividend mod Divisor
  else
    raise Exception.Create('Invalid modulo register index: ' + IntToStr(ModReg));
end;

{$ENDREGION}

{$REGION 'Opcode Handlers - Combined Operations (Quick Wins)'}

procedure HandleIncVar(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  Value: Variant;
  VarIdx: Integer;
  RegIdx: Integer;
begin
  // TIER 2 OPTIMIZATION: i = i + 1 - Direct indexed access, ZERO string ops!
  VarIdx := Inst.VarIndex;
  Value := VM.FContext.GetVariableByIndex(VarIdx);
  Value := Value + 1;
  VM.FContext.SetVariableByIndex(VarIdx, Value);

  // If variable mapped to a register, update typed register copies as well
  if (VarIdx >= 0) and (VarIdx < Length(VM.FVarToRegMap)) then
  begin
    RegIdx := VM.FVarToRegMap[VarIdx];
    if RegIdx >= 0 then
    begin
      if (RegIdx >= 0) and (RegIdx < 16) then
      begin
        VM.FIntRegisters[RegIdx] := VarAsType(Value, varInt64);
        VM.FRegisters[RegIdx] := VM.FIntRegisters[RegIdx];
      end
      else if (RegIdx >= 16) and (RegIdx < 32) then
      begin
        VM.FFloatRegisters[RegIdx - 16] := VarAsType(Value, varDouble);
        VM.FRegisters[RegIdx] := VM.FFloatRegisters[RegIdx - 16];
      end
      else
        VM.FRegisters[RegIdx] := Value;
    end;
  end;
end;

procedure HandleDecVar(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  Value: Variant;
  VarIdx: Integer;
  RegIdx: Integer;
begin
  // TIER 2 OPTIMIZATION: i = i - 1 - Direct indexed access, ZERO string ops!
  VarIdx := Inst.VarIndex;
  Value := VM.FContext.GetVariableByIndex(VarIdx);
  Value := Value - 1;
  VM.FContext.SetVariableByIndex(VarIdx, Value);

  // If variable mapped to a register, update typed register copies as well
  if (VarIdx >= 0) and (VarIdx < Length(VM.FVarToRegMap)) then
  begin
    RegIdx := VM.FVarToRegMap[VarIdx];
    if RegIdx >= 0 then
    begin
      if (RegIdx >= 0) and (RegIdx < 16) then
      begin
        VM.FIntRegisters[RegIdx] := VarAsType(Value, varInt64);
        VM.FRegisters[RegIdx] := VM.FIntRegisters[RegIdx];
      end
      else if (RegIdx >= 16) and (RegIdx < 32) then
      begin
        VM.FFloatRegisters[RegIdx - 16] := VarAsType(Value, varDouble);
        VM.FRegisters[RegIdx] := VM.FFloatRegisters[RegIdx - 16];
      end
      else
        VM.FRegisters[RegIdx] := Value;
    end;
  end;
end;

procedure HandleAddVarConst(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  Value, ConstValue: Variant;
  VarIndex, ConstIndex: Integer;
begin
  // TIER 2 OPTIMIZATION: i = i + N - Direct indexed access
  VarIndex := Inst.TwoArgs.Arg1;
  ConstIndex := Inst.TwoArgs.Arg2;
  Value := VM.FContext.GetVariableByIndex(VarIndex);
  ConstValue := VM.FProgram.Constants[ConstIndex];
  VM.FContext.SetVariableByIndex(VarIndex, Value + ConstValue);
end;

procedure HandleMulVarConst(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  Value, ConstValue: Variant;
  VarIndex, ConstIndex: Integer;
begin
  // TIER 2 OPTIMIZATION: i = i * N - Direct indexed access
  VarIndex := Inst.TwoArgs.Arg1;
  ConstIndex := Inst.TwoArgs.Arg2;
  Value := VM.FContext.GetVariableByIndex(VarIndex);
  ConstValue := VM.FProgram.Constants[ConstIndex];
  VM.FContext.SetVariableByIndex(VarIndex, Value * ConstValue);
end;

{$ENDREGION}

{$REGION 'Opcode Handlers - Register Operations (Phase B)'}

procedure HandleLoadReg(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  RegIdx: Integer;
begin
  // Push register value to stack
  RegIdx := Inst.IntArg;

  // DAY 6: Load from typed register if applicable
  if (RegIdx >= 0) and (RegIdx < 16) then
    VM.PushVariant(VM.FIntRegisters[RegIdx])  // Typed Int64 register!
  else if (RegIdx >= 16) and (RegIdx < 32) then
    VM.PushVariant(VM.FFloatRegisters[RegIdx - 16])  // Typed Double register!
  else
    VM.PushVariant(VM.FRegisters[RegIdx]);  // Legacy Variant register
end;

procedure HandleStoreReg(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  RegIdx: Integer;
  Val: Variant;
begin
  // Pop stack value to register
  RegIdx := Inst.IntArg;
  Val := VM.Pop;
  
  // DAY 6: Store to typed register if applicable (CRITICAL FIX for SIEVE.BAS)
  if (RegIdx >= 0) and (RegIdx < 16) then
  begin
    VM.FIntRegisters[RegIdx] := Val;  // Typed Int64 register! (array[0..15])
    VM.FRegisters[RegIdx] := Val;     // Also store to legacy for compatibility
  end
  else if (RegIdx >= 16) and (RegIdx < 32) then
  begin
    VM.FFloatRegisters[RegIdx] := Val;  // Typed Double register! (array[16..31])
    VM.FRegisters[RegIdx] := Val;       // Also store to legacy for compatibility
  end
  else
    VM.FRegisters[RegIdx] := Val;  // Legacy Variant register only (>= 32)
end;

procedure HandleLoadVarToReg(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  RegIndex, VarIndex: Integer;
begin
  // PHASE 3: Load variable directly to register using TYPED methods (NO Variant!)
  RegIndex := Inst.TwoArgs.Arg1;
  VarIndex := Inst.TwoArgs.Arg2;
  // Load into typed registers using typed methods
  if (RegIndex >= 0) and (RegIndex < 16) then
  begin
    VM.FIntRegisters[RegIndex] := VM.FContext.GetVariableByIndexInt(VarIndex);
    VM.FRegisters[RegIndex] := VM.FIntRegisters[RegIndex];
  end
  else if (RegIndex >= 16) and (RegIndex < 32) then
  begin
    VM.FFloatRegisters[RegIndex - 16] := VM.FContext.GetVariableByIndexFloat(VarIndex);
    VM.FRegisters[RegIndex] := VM.FFloatRegisters[RegIndex - 16];
  end
  else
    VM.FRegisters[RegIndex] := VM.FContext.GetVariableByIndex(VarIndex);

  // Record mapping VarIndex -> RegIndex so VM ops can keep them in sync
  if (VarIndex >= 0) and (VarIndex < Length(VM.FVarToRegMap)) then
    VM.FVarToRegMap[VarIndex] := RegIndex;
end;

procedure HandleStoreRegToVar(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  VarIndex, RegIndex: Integer;
begin
  // PHASE 3: Store register directly to variable using TYPED methods (NO Variant!)
  VarIndex := Inst.TwoArgs.Arg1;
  RegIndex := Inst.TwoArgs.Arg2;
  // Write back from typed registers using typed methods
  if (RegIndex >= 0) and (RegIndex < 16) then
    VM.FContext.SetVariableByIndexInt(VarIndex, VM.FIntRegisters[RegIndex])
  else if (RegIndex >= 16) and (RegIndex < 32) then
    VM.FContext.SetVariableByIndexFloat(VarIndex, VM.FFloatRegisters[RegIndex - 16])
  else
    VM.FContext.SetVariableByIndex(VarIndex, VM.FRegisters[RegIndex]);

  // Update mapping VarIndex -> RegIndex
  if (VarIndex >= 0) and (VarIndex < Length(VM.FVarToRegMap)) then
    VM.FVarToRegMap[VarIndex] := RegIndex;
end;

procedure HandleAddRegImm(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  RegIdx, ImmValue: Integer;
begin
  // Reg[dst] = Reg[dst] + immediate
  RegIdx := Inst.TwoArgs.Arg1;
  ImmValue := Inst.TwoArgs.Arg2;

  // FIX: Use typed registers for R0-R31
  if (RegIdx >= 0) and (RegIdx < 16) then
    // Int64 typed register (R0-R15)
    VM.FIntRegisters[RegIdx] := VM.FIntRegisters[RegIdx] + ImmValue
  else if (RegIdx >= 16) and (RegIdx < 32) then
    // Double typed register (R16-R31)
    VM.FFloatRegisters[RegIdx - 16] := VM.FFloatRegisters[RegIdx - 16] + ImmValue
  else
    // Variant register (fallback)
    VM.FRegisters[RegIdx] := VM.FRegisters[RegIdx] + ImmValue;
end;

procedure HandleIncReg(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  RegIdx: Integer;
begin
  // Reg[dst]++ (CRITICAL FIX: update both typed and legacy registers)
  RegIdx := Inst.IntArg;
  
  if (RegIdx >= 0) and (RegIdx < 16) then
  begin
    VM.FIntRegisters[RegIdx] := VM.FIntRegisters[RegIdx] + 1;
    VM.FRegisters[RegIdx] := VM.FIntRegisters[RegIdx];  // Keep in sync
  end
  else if (RegIdx >= 16) and (RegIdx < 32) then
  begin
    VM.FFloatRegisters[RegIdx] := VM.FFloatRegisters[RegIdx] + 1.0;
    VM.FRegisters[RegIdx] := VM.FFloatRegisters[RegIdx];  // Keep in sync
  end
  else
    VM.FRegisters[RegIdx] := VM.FRegisters[RegIdx] + 1;
end;

procedure HandleDecReg(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  RegIdx: Integer;
begin
  // Reg[dst]-- (CRITICAL FIX: update both typed and legacy registers)
  RegIdx := Inst.IntArg;
  
  if (RegIdx >= 0) and (RegIdx < 16) then
  begin
    VM.FIntRegisters[RegIdx] := VM.FIntRegisters[RegIdx] - 1;
    VM.FRegisters[RegIdx] := VM.FIntRegisters[RegIdx];  // Keep in sync
  end
  else if (RegIdx >= 16) and (RegIdx < 32) then
  begin
    VM.FFloatRegisters[RegIdx] := VM.FFloatRegisters[RegIdx] - 1.0;
    VM.FRegisters[RegIdx] := VM.FFloatRegisters[RegIdx];  // Keep in sync
  end
  else
    VM.FRegisters[RegIdx] := VM.FRegisters[RegIdx] - 1;
end;

procedure HandleAddReg(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  DstReg, SrcReg: Integer;
begin
  // Reg[dst] = Reg[dst] + Reg[src]
  DstReg := Inst.TwoArgs.Arg1;
  SrcReg := Inst.TwoArgs.Arg2;
  VM.FRegisters[DstReg] := VM.FRegisters[DstReg] + VM.FRegisters[SrcReg];
end;

procedure HandleSubReg(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  DstReg, SrcReg: Integer;
begin
  // Reg[dst] = Reg[dst] - Reg[src]
  DstReg := Inst.TwoArgs.Arg1;
  SrcReg := Inst.TwoArgs.Arg2;
  VM.FRegisters[DstReg] := VM.FRegisters[DstReg] - VM.FRegisters[SrcReg];
end;

procedure HandleMulReg(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  DstReg, SrcReg: Integer;
begin
  // Reg[dst] = Reg[dst] * Reg[src]
  DstReg := Inst.TwoArgs.Arg1;
  SrcReg := Inst.TwoArgs.Arg2;
  VM.FRegisters[DstReg] := VM.FRegisters[DstReg] * VM.FRegisters[SrcReg];
end;

procedure HandleDivReg(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  DstReg, SrcReg: Integer;
begin
  // Reg[dst] = Reg[dst] / Reg[src]
  DstReg := Inst.TwoArgs.Arg1;
  SrcReg := Inst.TwoArgs.Arg2;
  VM.FRegisters[DstReg] := VM.FRegisters[DstReg] / VM.FRegisters[SrcReg];
end;

procedure HandleCmpRegImm(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  RegValue, ImmValue: Variant;
begin
  // Compare Reg with immediate, set flags
  RegValue := VM.FRegisters[Inst.TwoArgs.Arg1];
  ImmValue := Inst.TwoArgs.Arg2;
  
  if RegValue < ImmValue then
    VM.FCompareFlags := -1
  else if RegValue > ImmValue then
    VM.FCompareFlags := 1
  else
    VM.FCompareFlags := 0;
end;

procedure HandleJumpIfRegLe(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  RegValue: Variant;
  CompareValue: Integer;
  JumpOffset: Integer;
begin
  // Jump if Reg <= value
  // TwoArgs: Arg1 = regIndex, Arg2 = compareValue
  // Offset field = jump target
  RegValue := VM.FRegisters[Inst.TwoArgs.Arg1];
  CompareValue := Inst.TwoArgs.Arg2;
  
  if Integer(RegValue) <= CompareValue then
  begin
    JumpOffset := Inst.Offset;
    if (JumpOffset >= 0) and (JumpOffset < VM.FProgram.CodeSize) then
      VM.FPC := JumpOffset
    else
      VM.RaiseRuntimeError(Format('Invalid jump target: %d', [JumpOffset]));
  end;
end;

// ============================================================================
// PHASE C: TYPED REGISTER HANDLERS (NO VARIANT OVERHEAD!)
// ============================================================================

{$PUSH}
{$INLINE ON}

procedure HandleLoadRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Push IntReg[idx] to typed stack (NO VARIANT!)
  VM.PushInt(VM.FIntRegisters[Inst.IntArg]);
end;

procedure HandleStoreRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Pop typed stack value to IntReg[idx] (NO VARIANT!)
  VM.FIntRegisters[Inst.IntArg] := VM.PopInt;
end;

procedure HandleLoadRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  RegIdx: Integer;
begin
  // Push FloatReg[idx] to typed stack (NO VARIANT!) (RegIdx is 16-31, map to FloatRegisters[0-15])
  RegIdx := Inst.IntArg;
  if RegIdx >= 16 then
    RegIdx := RegIdx - 16;
  VM.PushFloat(VM.FFloatRegisters[RegIdx]);
end;

procedure HandleStoreRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  RegIdx: Integer;
begin
  // Pop typed stack value to FloatReg[idx] (NO VARIANT!) (RegIdx is 16-31, map to FloatRegisters[0-15])
  RegIdx := Inst.IntArg;
  if RegIdx >= 16 then
    RegIdx := RegIdx - 16;
  VM.FFloatRegisters[RegIdx] := VM.PopFloat;
end;

{ PHASE 1 OPTIMIZATION: Load immediate values directly to registers }
procedure HandleLoadImmRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // PHASE 4F Step 2.1: Load immediate Int32 to IntReg[RegIdx]
  // CRITICAL FIX: Use LoadImmReg (not LoadImmRegFloat!)
  // Format: LoadImmReg (RegIdx:Byte + ImmValue:Integer = 32-bit)
  // NO STACK OPERATIONS!
  VM.FIntRegisters[Inst.LoadImmReg.RegIdx] := Int64(Inst.LoadImmReg.ImmValue);
end;

procedure HandleLoadImmRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  RegIdx: Integer;
begin
  // PHASE 4F Step 2.1: Load immediate Double to FloatReg[RegIdx]
  // Format: LoadImmRegFloat variant (RegIdx:Byte + ImmValue:Double)
  // NO STACK OPERATIONS!
  RegIdx := Inst.LoadImmRegFloat.RegIdx;
  if RegIdx >= 16 then
    RegIdx := RegIdx - 16;
  VM.FFloatRegisters[RegIdx] := Inst.LoadImmRegFloat.ImmValue;
end;

procedure HandleMoveRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // IntReg[dst] = IntReg[src]
  VM.FIntRegisters[Inst.TwoArgs.Arg1] := VM.FIntRegisters[Inst.TwoArgs.Arg2];
end;

procedure HandleMoveRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  DstIdx, SrcIdx: Integer;
begin
  // FloatReg[dst] = FloatReg[src] (Map R16-R31 to FloatRegisters[0-15])
  DstIdx := Inst.TwoArgs.Arg1;
  SrcIdx := Inst.TwoArgs.Arg2;
  if DstIdx >= 16 then DstIdx := DstIdx - 16;
  if SrcIdx >= 16 then SrcIdx := SrcIdx - 16;
  VM.FFloatRegisters[DstIdx] := VM.FFloatRegisters[SrcIdx];
end;

procedure HandleAddRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // IntReg[dst] = IntReg[src1] + IntReg[src2]
  // ThreeArgs: Arg1=dst, Arg2=src1, Arg3=src2
  VM.FIntRegisters[Inst.ThreeArgs.Arg1] := 
    VM.FIntRegisters[Inst.ThreeArgs.Arg2] + VM.FIntRegisters[Inst.ThreeArgs.Arg3];
end;

procedure HandleSubRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // IntReg[dst] = IntReg[src1] - IntReg[src2]
  VM.FIntRegisters[Inst.ThreeArgs.Arg1] := 
    VM.FIntRegisters[Inst.ThreeArgs.Arg2] - VM.FIntRegisters[Inst.ThreeArgs.Arg3];
end;

procedure HandleMulRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // IntReg[dst] = IntReg[src1] * IntReg[src2]
  VM.FIntRegisters[Inst.ThreeArgs.Arg1] := 
    VM.FIntRegisters[Inst.ThreeArgs.Arg2] * VM.FIntRegisters[Inst.ThreeArgs.Arg3];
end;

procedure HandleDivRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // IntReg[dst] = IntReg[src1] div IntReg[src2]
  VM.FIntRegisters[Inst.ThreeArgs.Arg1] := 
    VM.FIntRegisters[Inst.ThreeArgs.Arg2] div VM.FIntRegisters[Inst.ThreeArgs.Arg3];
end;

procedure HandleIncRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // IntReg[dst]++
  Inc(VM.FIntRegisters[Inst.IntArg]);
end;

procedure HandleDecRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // IntReg[dst]--
  Dec(VM.FIntRegisters[Inst.IntArg]);
end;

procedure HandleAddRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  DstIdx, Src1Idx, Src2Idx: Integer;
begin
  // FloatReg[dst] = FloatReg[src1] + FloatReg[src2] (Map R16-R31 to 0-15)
  DstIdx := Inst.ThreeArgs.Arg1;
  Src1Idx := Inst.ThreeArgs.Arg2;
  Src2Idx := Inst.ThreeArgs.Arg3;
  if DstIdx >= 16 then DstIdx := DstIdx - 16;
  if Src1Idx >= 16 then Src1Idx := Src1Idx - 16;
  if Src2Idx >= 16 then Src2Idx := Src2Idx - 16;
  VM.FFloatRegisters[DstIdx] := VM.FFloatRegisters[Src1Idx] + VM.FFloatRegisters[Src2Idx];
end;

procedure HandleSubRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  DstIdx, Src1Idx, Src2Idx: Integer;
begin
  // FloatReg[dst] = FloatReg[src1] - FloatReg[src2] (Map R16-R31 to 0-15)
  DstIdx := Inst.ThreeArgs.Arg1;
  Src1Idx := Inst.ThreeArgs.Arg2;
  Src2Idx := Inst.ThreeArgs.Arg3;
  if DstIdx >= 16 then DstIdx := DstIdx - 16;
  if Src1Idx >= 16 then Src1Idx := Src1Idx - 16;
  if Src2Idx >= 16 then Src2Idx := Src2Idx - 16;
  VM.FFloatRegisters[DstIdx] := VM.FFloatRegisters[Src1Idx] - VM.FFloatRegisters[Src2Idx];
end;

procedure HandleMulRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  DstIdx, Src1Idx, Src2Idx: Integer;
begin
  // FloatReg[dst] = FloatReg[src1] * FloatReg[src2] (Map R16-R31 to 0-15)
  DstIdx := Inst.ThreeArgs.Arg1;
  Src1Idx := Inst.ThreeArgs.Arg2;
  Src2Idx := Inst.ThreeArgs.Arg3;
  if DstIdx >= 16 then DstIdx := DstIdx - 16;
  if Src1Idx >= 16 then Src1Idx := Src1Idx - 16;
  if Src2Idx >= 16 then Src2Idx := Src2Idx - 16;
  VM.FFloatRegisters[DstIdx] := VM.FFloatRegisters[Src1Idx] * VM.FFloatRegisters[Src2Idx];
end;

procedure HandleDivRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  DstIdx, Src1Idx, Src2Idx: Integer;
begin
  // FloatReg[dst] = FloatReg[src1] / FloatReg[src2] (Map R16-R31 to 0-15)
  DstIdx := Inst.ThreeArgs.Arg1;
  Src1Idx := Inst.ThreeArgs.Arg2;
  Src2Idx := Inst.ThreeArgs.Arg3;
  if DstIdx >= 16 then DstIdx := DstIdx - 16;
  if Src1Idx >= 16 then Src1Idx := Src1Idx - 16;
  if Src2Idx >= 16 then Src2Idx := Src2Idx - 16;
  VM.FFloatRegisters[DstIdx] := VM.FFloatRegisters[Src1Idx] / VM.FFloatRegisters[Src2Idx];
end;

procedure HandleIncRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  RegIdx: Integer;
begin
  // FloatReg[dst]++ (RegIdx is 16-31, map to 0-15)
  RegIdx := Inst.IntArg;
  if RegIdx >= 16 then
    RegIdx := RegIdx - 16;  // Map R16-R31 to FloatRegisters[0-15]
  VM.FFloatRegisters[RegIdx] := VM.FFloatRegisters[RegIdx] + 1.0;
end;

procedure HandleDecRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  RegIdx: Integer;
begin
  // FloatReg[dst]-- (RegIdx is 16-31, map to 0-15)
  RegIdx := Inst.IntArg;
  if RegIdx >= 16 then
    RegIdx := RegIdx - 16;
  VM.FFloatRegisters[RegIdx] := VM.FFloatRegisters[RegIdx] - 1.0;
end;

procedure HandleCmpRegImmInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  RegValue: Int64;
  ImmValue: Int64;
begin
  // Compare IntReg with immediate (typed, no Variant!)
  // Push result (for <= comparison with PopJumpIfFalse)
  RegValue := VM.FIntRegisters[Inst.TwoArgs.Arg1];
  ImmValue := Inst.TwoArgs.Arg2;
  
  // Push TRUE if RegValue <= ImmValue, else FALSE
  VM.PushVariant(RegValue <= ImmValue);
  
  // Also set flags for backward compatibility
  if RegValue < ImmValue then
    VM.FCompareFlags := -1
  else if RegValue > ImmValue then
    VM.FCompareFlags := 1
  else
    VM.FCompareFlags := 0;
end;

procedure HandleCmpRegImmFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  RegValue: Double;
  ImmValue: Double;
  RegIdx: Integer;
begin
  // Compare FloatReg with immediate (typed, no Variant!)
  // Push result (for <= comparison with PopJumpIfFalse)
  RegIdx := Inst.TwoArgs.Arg1;
  if RegIdx >= 16 then
    RegIdx := RegIdx - 16;  // Map R16-R31 to FloatRegisters[0-15]
    
  RegValue := VM.FFloatRegisters[RegIdx];
  ImmValue := Inst.TwoArgs.Arg2;
  
  // Push TRUE if RegValue <= ImmValue, else FALSE
  VM.PushVariant(RegValue <= ImmValue);
  
  // Also set flags for backward compatibility
  if RegValue < ImmValue then
    VM.FCompareFlags := -1
  else if RegValue > ImmValue then
    VM.FCompareFlags := 1
  else
    VM.FCompareFlags := 0;
end;

// === STEP 3: FUSED COMPARISON+JUMP INSTRUCTIONS ===

procedure HandleJumpIfRegLeIntReg(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // FUSED: Compare IntReg[idx1] <= IntReg[idx2] and jump if FALSE (like PopJumpIfFalse)
  // TwoArgsAndOffset: Arg1 = regIdx1, Arg2 = regIdx2, JumpOffset = target
  // NO STACK OPERATIONS! Direct register comparison!
  // Jump to BreakLabel when loop counter > end value
  // NOTE: FPC already incremented by main loop, so we only set it on jump
  if VM.FIntRegisters[Inst.TwoArgsAndOffset.Arg1] > VM.FIntRegisters[Inst.TwoArgsAndOffset.Arg2] then
    VM.FPC := Inst.TwoArgsAndOffset.JumpOffset;
  // else: do nothing, FPC already incremented by main loop
end;

procedure HandleJumpIfRegLeFloatReg(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // FUSED: Compare FloatReg[idx1] <= FloatReg[idx2] and jump if FALSE (like PopJumpIfFalse)
  // TwoArgsAndOffset: Arg1 = regIdx1, Arg2 = regIdx2, JumpOffset = target
  // NO STACK OPERATIONS! Direct register comparison!
  // Jump to BreakLabel when loop counter > end value
  // Map R16-R31 to FloatRegisters[0-15]
  // NOTE: FPC already incremented by main loop, so we only set it on jump
  if VM.FFloatRegisters[Inst.TwoArgsAndOffset.Arg1 - 16] > VM.FFloatRegisters[Inst.TwoArgsAndOffset.Arg2 - 16] then
    VM.FPC := Inst.TwoArgsAndOffset.JumpOffset;
  // else: do nothing, FPC already incremented by main loop
end;

{ PHASE 4F: Direct conditional jump opcodes - jump when condition is TRUE }
procedure HandleJumpIfRegGtIntReg(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Jump if IntReg[idx1] > IntReg[idx2] (condition TRUE → jump)
  // Direct register comparison, NO STACK!
  // NOTE: FPC already incremented by main loop, so we only set it on jump
  if VM.FIntRegisters[Inst.TwoArgsAndOffset.Arg1] > VM.FIntRegisters[Inst.TwoArgsAndOffset.Arg2] then
    VM.FPC := Inst.TwoArgsAndOffset.JumpOffset;
  // else: do nothing, FPC already incremented by main loop
end;

procedure HandleJumpIfRegGtFloatReg(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Jump if FloatReg[idx1] > FloatReg[idx2] (condition TRUE → jump)
  // Direct register comparison, NO STACK!
  // Map R16-R31 to FloatRegisters[0-15]
  // NOTE: FPC already incremented by main loop, so we only set it on jump
  if VM.FFloatRegisters[Inst.TwoArgsAndOffset.Arg1 - 16] > VM.FFloatRegisters[Inst.TwoArgsAndOffset.Arg2 - 16] then
    VM.FPC := Inst.TwoArgsAndOffset.JumpOffset;
  // else: do nothing, FPC already incremented by main loop
end;

{ PHASE 4F Step 2.3: Equality/inequality comparison+jump opcodes for IF statements }
procedure HandleJumpIfRegEqIntReg(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Jump if IntReg[idx1] == IntReg[idx2] (condition TRUE → jump)
  // Direct register comparison, NO STACK!
  // NOTE: FPC already incremented by main loop, so we only set it on jump
  if VM.FIntRegisters[Inst.TwoArgsAndOffset.Arg1] = VM.FIntRegisters[Inst.TwoArgsAndOffset.Arg2] then
    VM.FPC := Inst.TwoArgsAndOffset.JumpOffset;
  // else: do nothing, FPC already incremented by main loop
end;

procedure HandleJumpIfRegEqFloatReg(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Jump if FloatReg[idx1] == FloatReg[idx2] (condition TRUE → jump)
  // Direct register comparison, NO STACK!
  // Map R16-R31 to FloatRegisters[0-15]
  // NOTE: FPC already incremented by main loop, so we only set it on jump
  if VM.FFloatRegisters[Inst.TwoArgsAndOffset.Arg1 - 16] = VM.FFloatRegisters[Inst.TwoArgsAndOffset.Arg2 - 16] then
    VM.FPC := Inst.TwoArgsAndOffset.JumpOffset;
  // else: do nothing, FPC already incremented by main loop
end;

procedure HandleJumpIfRegNeIntReg(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Jump if IntReg[idx1] != IntReg[idx2] (condition TRUE → jump)
  // Direct register comparison, NO STACK!
  // NOTE: FPC already incremented by main loop, so we only set it on jump
  if VM.FIntRegisters[Inst.TwoArgsAndOffset.Arg1] <> VM.FIntRegisters[Inst.TwoArgsAndOffset.Arg2] then
    VM.FPC := Inst.TwoArgsAndOffset.JumpOffset;
  // else: do nothing, FPC already incremented by main loop
end;

procedure HandleJumpIfRegNeFloatReg(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Jump if FloatReg[idx1] != FloatReg[idx2] (condition TRUE → jump)
  // Direct register comparison, NO STACK!
  // Map R16-R31 to FloatRegisters[0-15]
  // NOTE: FPC already incremented by main loop, so we only set it on jump
  if VM.FFloatRegisters[Inst.TwoArgsAndOffset.Arg1 - 16] <> VM.FFloatRegisters[Inst.TwoArgsAndOffset.Arg2 - 16] then
    VM.FPC := Inst.TwoArgsAndOffset.JumpOffset;
  // else: do nothing, FPC already incremented by main loop
end;

{ PHASE 4F Step 2.5: Register-based single-argument math functions }
procedure HandleCallMath1FloatReg(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  SrcValue, Result: Double;
begin
  // FloatReg[dst] = MathFunc(FloatReg[src])
  // CallMath1Reg: FuncID, SrcReg, DstReg
  // Map R16-R31 to FloatRegisters[0-15]
  SrcValue := VM.FFloatRegisters[Inst.CallMath1Reg.SrcReg - 16];

  // Execute math function based on FuncID
  case TMathFuncID(Inst.CallMath1Reg.FuncID) of
    mfSQR: Result := Sqrt(SrcValue);
    mfABS: Result := Abs(SrcValue);
    mfSIN: Result := Sin(SrcValue);
    mfCOS: Result := Cos(SrcValue);
    mfTAN: Result := Tan(SrcValue);
    mfATN: Result := ArcTan(SrcValue);
    mfEXP: Result := Exp(SrcValue);
    mfLOG: Result := Ln(SrcValue);
    mfSGN:
      if SrcValue > 0 then
        Result := 1
      else if SrcValue < 0 then
        Result := -1
      else
        Result := 0;
    mfINT: Result := Int(SrcValue);  // Floor
    mfFIX: Result := Trunc(SrcValue); // Truncate toward zero
  else
    Result := 0;  // Unknown function ID
  end;

  // Store result in destination Float register
  VM.FFloatRegisters[Inst.CallMath1Reg.DstReg - 16] := Result;
end;

{ PHASE 4F Step 3: Register type conversions - NO STACK! }
procedure HandleConvertFloatToIntReg(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // IntReg[dst] = Trunc(FloatReg[src])
  // TwoArgs: Arg1=dstReg (0-15), Arg2=srcReg (16-31)
  // Map R16-R31 to FloatRegisters[0-15]
  VM.FIntRegisters[Inst.TwoArgs.Arg1] := Trunc(VM.FFloatRegisters[Inst.TwoArgs.Arg2 - 16]);
end;

procedure HandleConvertIntToFloatReg(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // FloatReg[dst] = Double(IntReg[src])
  // TwoArgs: Arg1=dstReg (16-31), Arg2=srcReg (0-15)
  // Map R16-R31 to FloatRegisters[0-15]
  VM.FFloatRegisters[Inst.TwoArgs.Arg1 - 16] := Double(VM.FIntRegisters[Inst.TwoArgs.Arg2]);
end;

procedure HandleLoadArray1DRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, DstReg: Integer;
  Index: Int64;
begin
  // IntReg[dst] = Array[IntReg[idx]]
  // ThreeArgs: Arg1=arrayVar, Arg2=idxReg, Arg3=dstReg
  ArrayVar := Inst.ThreeArgs.Arg1;
  IdxReg := Inst.ThreeArgs.Arg2;
  DstReg := Inst.ThreeArgs.Arg3;
  
  Index := VM.FIntRegisters[IdxReg];
  // TYPED ARRAYS: Use direct Int64 access (no Variant conversion!)
  VM.FIntRegisters[DstReg] := VM.FContext.GetArrayElementByIndexInt64(ArrayVar, Index);
end;

procedure HandleStoreArray1DRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, SrcReg: Integer;
  Index: Int64;
begin
  // Array[IntReg[idx]] = IntReg[src]
  // ThreeArgs: Arg1=arrayVar, Arg2=idxReg, Arg3=srcReg
  ArrayVar := Inst.ThreeArgs.Arg1;
  IdxReg := Inst.ThreeArgs.Arg2;
  SrcReg := Inst.ThreeArgs.Arg3;
  
  Index := VM.FIntRegisters[IdxReg];
  // TYPED ARRAYS: Use direct Int64 access (no Variant conversion!)
  VM.FContext.SetArrayElementByIndexInt64(ArrayVar, Index, VM.FIntRegisters[SrcReg]);
end;

procedure HandleLoadArray1DRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, DstReg: Integer;
  Index: Int64;
begin
  // FloatReg[dst] = Array[IntReg[idx]]
  ArrayVar := Inst.ThreeArgs.Arg1;
  IdxReg := Inst.ThreeArgs.Arg2;
  DstReg := Inst.ThreeArgs.Arg3;

  Index := VM.FIntRegisters[IdxReg];
  // Map R16-R31 to FFloatRegisters[0-15]
  VM.FFloatRegisters[DstReg - 16] := Double(VM.FContext.GetArrayElementByIndex(ArrayVar, Index));
end;

procedure HandleStoreArray1DRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, SrcReg: Integer;
  Index: Int64;
begin
  // Array[IntReg[idx]] = FloatReg[src]
  ArrayVar := Inst.ThreeArgs.Arg1;
  IdxReg := Inst.ThreeArgs.Arg2;
  SrcReg := Inst.ThreeArgs.Arg3;

  Index := VM.FIntRegisters[IdxReg];
  // Map R16-R31 to FFloatRegisters[0-15]
  VM.FContext.SetArrayElementByIndex(ArrayVar, Index, VM.FFloatRegisters[SrcReg - 16]);
end;

{ FASE 2: Immediate array store - Array[Reg] = immediate }
procedure HandleStoreArray1DRegImmInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, ImmValue: Integer;
  Index: Int64;
begin
  // Array[IntReg[idx]] = immediate_int (no register needed!)
  // ThreeArgs: Arg1=ArrayVar, Arg2=IdxReg, Arg3=ImmValue
  ArrayVar := Inst.ThreeArgs.Arg1;
  IdxReg := Inst.ThreeArgs.Arg2;
  ImmValue := Inst.ThreeArgs.Arg3;

  Index := VM.FIntRegisters[IdxReg];
  VM.FContext.SetArrayElementByIndex(ArrayVar, Index, Int64(ImmValue));
end;

procedure HandleStoreArray1DRegImmFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
var
  ArrayVar, IdxReg, ImmValue: Integer;
  Index: Int64;
begin
  // Array[IntReg[idx]] = immediate_float (converted to Double)
  // ThreeArgs: Arg1=ArrayVar, Arg2=IdxReg, Arg3=ImmValue
  ArrayVar := Inst.ThreeArgs.Arg1;
  IdxReg := Inst.ThreeArgs.Arg2;
  ImmValue := Inst.ThreeArgs.Arg3;

  Index := VM.FIntRegisters[IdxReg];
  VM.FContext.SetArrayElementByIndex(ArrayVar, Index, Double(ImmValue));
end;

{$POP}

{$ENDREGION}

{$REGION 'Opcode Handlers - Debug'}

procedure HandleLineNumber(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // In production builds, this can be a no-op
  {$IFDEF DEBUG}
  // Could track current line for error messages
  {$ENDIF}
end;

procedure HandleNop(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // No operation
end;

{$ENDREGION}

// === PHASE 2: REGISTER-TO-REGISTER COMPARISON HANDLERS ===

procedure HandleLeRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Push (IntReg[arg1] <= IntReg[arg2])
  if VM.FIntRegisters[Inst.TwoArgs.Arg1] <= VM.FIntRegisters[Inst.TwoArgs.Arg2] then
    VM.PushVariant(1)
  else
    VM.PushVariant(0);
end;

procedure HandleLeRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Push (FloatReg[arg1] <= FloatReg[arg2])
  if VM.FFloatRegisters[Inst.TwoArgs.Arg1 - 16] <= VM.FFloatRegisters[Inst.TwoArgs.Arg2 - 16] then
    VM.PushVariant(1)
  else
    VM.PushVariant(0);
end;

procedure HandleLtRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Push (IntReg[arg1] < IntReg[arg2])
  if VM.FIntRegisters[Inst.TwoArgs.Arg1] < VM.FIntRegisters[Inst.TwoArgs.Arg2] then
    VM.PushVariant(1)
  else
    VM.PushVariant(0);
end;

procedure HandleLtRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Push (FloatReg[arg1] < FloatReg[arg2])
  if VM.FFloatRegisters[Inst.TwoArgs.Arg1 - 16] < VM.FFloatRegisters[Inst.TwoArgs.Arg2 - 16] then
    VM.PushVariant(1)
  else
    VM.PushVariant(0);
end;

procedure HandleGeRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Push (IntReg[arg1] >= IntReg[arg2])
  if VM.FIntRegisters[Inst.TwoArgs.Arg1] >= VM.FIntRegisters[Inst.TwoArgs.Arg2] then
    VM.PushVariant(1)
  else
    VM.PushVariant(0);
end;

procedure HandleGeRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Push (FloatReg[arg1] >= FloatReg[arg2])
  if VM.FFloatRegisters[Inst.TwoArgs.Arg1 - 16] >= VM.FFloatRegisters[Inst.TwoArgs.Arg2 - 16] then
    VM.PushVariant(1)
  else
    VM.PushVariant(0);
end;

procedure HandleGtRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Push (IntReg[arg1] > IntReg[arg2])
  if VM.FIntRegisters[Inst.TwoArgs.Arg1] > VM.FIntRegisters[Inst.TwoArgs.Arg2] then
    VM.PushVariant(1)
  else
    VM.PushVariant(0);
end;

procedure HandleGtRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Push (FloatReg[arg1] > FloatReg[arg2])
  if VM.FFloatRegisters[Inst.TwoArgs.Arg1 - 16] > VM.FFloatRegisters[Inst.TwoArgs.Arg2 - 16] then
    VM.PushVariant(1)
  else
    VM.PushVariant(0);
end;

procedure HandleEqRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Push (IntReg[arg1] == IntReg[arg2])
  if VM.FIntRegisters[Inst.TwoArgs.Arg1] = VM.FIntRegisters[Inst.TwoArgs.Arg2] then
    VM.PushVariant(1)
  else
    VM.PushVariant(0);
end;

procedure HandleEqRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Push (FloatReg[arg1] == FloatReg[arg2])
  if VM.FFloatRegisters[Inst.TwoArgs.Arg1 - 16] = VM.FFloatRegisters[Inst.TwoArgs.Arg2 - 16] then
    VM.PushVariant(1)
  else
    VM.PushVariant(0);
end;

procedure HandleNeqRegInt(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Push (IntReg[arg1] != IntReg[arg2])
  if VM.FIntRegisters[Inst.TwoArgs.Arg1] <> VM.FIntRegisters[Inst.TwoArgs.Arg2] then
    VM.PushVariant(1)
  else
    VM.PushVariant(0);
end;

procedure HandleNeqRegFloat(VM: TSedaiBytecodeVM; const Inst: TInstruction); inline;
begin
  // Push (FloatReg[arg1] != FloatReg[arg2])
  if VM.FFloatRegisters[Inst.TwoArgs.Arg1 - 16] <> VM.FFloatRegisters[Inst.TwoArgs.Arg2 - 16] then
    VM.PushVariant(1)
  else
    VM.PushVariant(0);
end;

procedure HandleUnimplemented(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.RaiseRuntimeError(Format('Unimplemented opcode: %d', [Ord(Inst.OpCode)]));
end;

procedure HandleRotThree(VM: TSedaiBytecodeVM; const Inst: TInstruction);
var
  Top, Middle, Bottom: Variant;
begin
  VM.CheckStackUnderflow(3);
  Top := VM.Pop;
  Middle := VM.Pop;
  Bottom := VM.Pop;
  VM.PushVariant(Middle);
  VM.PushVariant(Top);
  VM.PushVariant(Bottom);
end;

procedure HandleHalt(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  VM.FHalted := True;
end;

procedure HandleCheckBreak(VM: TSedaiBytecodeVM; const Inst: TInstruction);
begin
  // Could check for CTRL+C here
end;

// === HANDLER INITIALIZATION ===

class procedure TSedaiBytecodeVM.InitializeHandlers;
var
  Op: TOpCode;
begin
  if FHandlersInitialized then
    Exit;

  // Initialize all handlers to unimplemented by default (safety)
  for Op := Low(TOpCode) to High(TOpCode) do
    FOpcodeHandlers[Op] := @HandleUnimplemented;

  // Stack operations
  FOpcodeHandlers[opPushConst] := @HandlePushConst;
  FOpcodeHandlers[opPush0] := @HandlePush0;
  FOpcodeHandlers[opPush1] := @HandlePush1;
  FOpcodeHandlers[opPushM1] := @HandlePushM1;
  FOpcodeHandlers[opPop] := @HandlePop;
  FOpcodeHandlers[opDup] := @HandleDup;
  FOpcodeHandlers[opRotTwo] := @HandleRotTwo;
  FOpcodeHandlers[opRotThree] := @HandleRotThree;

  // Variables
  FOpcodeHandlers[opLoadVar] := @HandleLoadVar;
  FOpcodeHandlers[opStoreVar] := @HandleStoreVar;

  // FASE 2: Typed variable operations (NO VARIANT!)
  FOpcodeHandlers[opLoadVarInt] := @HandleLoadVarInt;
  FOpcodeHandlers[opStoreVarInt] := @HandleStoreVarInt;
  FOpcodeHandlers[opLoadVarFloat] := @HandleLoadVarFloat;
  FOpcodeHandlers[opStoreVarFloat] := @HandleStoreVarFloat;
  FOpcodeHandlers[opLoadVarString] := @HandleLoadVarString;
  FOpcodeHandlers[opStoreVarString] := @HandleStoreVarString;
  FOpcodeHandlers[opIncVarInt] := @HandleIncVarInt;
  FOpcodeHandlers[opDecVarInt] := @HandleDecVarInt;
  FOpcodeHandlers[opIncVarFloat] := @HandleIncVarFloat;
  FOpcodeHandlers[opDecVarFloat] := @HandleDecVarFloat;

  // Arithmetic - Generic
  FOpcodeHandlers[opAdd] := @HandleAdd;
  FOpcodeHandlers[opSub] := @HandleSub;
  FOpcodeHandlers[opMul] := @HandleMul;
  FOpcodeHandlers[opDiv] := @HandleDiv;
  FOpcodeHandlers[opMod] := @HandleMod;
  FOpcodeHandlers[opPow] := @HandlePow;
  FOpcodeHandlers[opNeg] := @HandleNeg;

  // Arithmetic - Integer optimized
  FOpcodeHandlers[opAddInt] := @HandleAddInt;
  FOpcodeHandlers[opSubInt] := @HandleSubInt;
  FOpcodeHandlers[opMulInt] := @HandleMulInt;
  FOpcodeHandlers[opDivInt] := @HandleDivInt;
  FOpcodeHandlers[opModInt] := @HandleModInt;
  FOpcodeHandlers[opNegInt] := @HandleNegInt;

  // Arithmetic - Float optimized (PHASE 2)
  FOpcodeHandlers[opAddFloat] := @HandleAddFloat;
  FOpcodeHandlers[opSubFloat] := @HandleSubFloat;
  FOpcodeHandlers[opMulFloat] := @HandleMulFloat;
  FOpcodeHandlers[opDivFloat] := @HandleDivFloat;
  FOpcodeHandlers[opNegFloat] := @HandleNegFloat;
  FOpcodeHandlers[opPowFloat] := @HandlePowFloat;

  // Comparisons - Generic
  FOpcodeHandlers[opEq] := @HandleEq;
  FOpcodeHandlers[opNeq] := @HandleNeq;
  FOpcodeHandlers[opLt] := @HandleLt;
  FOpcodeHandlers[opGt] := @HandleGt;
  FOpcodeHandlers[opLe] := @HandleLe;
  FOpcodeHandlers[opGe] := @HandleGe;

  // Comparisons - Integer optimized
  FOpcodeHandlers[opEqInt] := @HandleEqInt;
  FOpcodeHandlers[opNeqInt] := @HandleNeqInt;
  FOpcodeHandlers[opLtInt] := @HandleLtInt;
  FOpcodeHandlers[opGtInt] := @HandleGtInt;
  FOpcodeHandlers[opLeInt] := @HandleLeInt;
  FOpcodeHandlers[opGeInt] := @HandleGeInt;

  // Comparisons - Float optimized (PHASE 2)
  FOpcodeHandlers[opEqFloat] := @HandleEqFloat;
  FOpcodeHandlers[opNeqFloat] := @HandleNeqFloat;
  FOpcodeHandlers[opLtFloat] := @HandleLtFloat;
  FOpcodeHandlers[opGtFloat] := @HandleGtFloat;
  FOpcodeHandlers[opLeFloat] := @HandleLeFloat;
  FOpcodeHandlers[opGeFloat] := @HandleGeFloat;

  // Logic
  FOpcodeHandlers[opAnd] := @HandleAnd;
  FOpcodeHandlers[opOr] := @HandleOr;
  FOpcodeHandlers[opNot] := @HandleNot;

  // Control flow
  FOpcodeHandlers[opJump] := @HandleJump;
  FOpcodeHandlers[opJumpIfFalse] := @HandleJumpIfFalse;
  FOpcodeHandlers[opJumpIfTrue] := @HandleJumpIfTrue;
  FOpcodeHandlers[opPopJumpIfFalse] := @HandlePopJumpIfFalse;
  FOpcodeHandlers[opPopJumpIfTrue] := @HandlePopJumpIfTrue;
  FOpcodeHandlers[opCall] := @HandleCall;
  FOpcodeHandlers[opReturn] := @HandleReturn;

  // I/O
  FOpcodeHandlers[opPrint] := @HandlePrint;
  FOpcodeHandlers[opPrintNewline] := @HandlePrintNewline;
  FOpcodeHandlers[opInput] := @HandleInput;

  // Functions & Arrays
  FOpcodeHandlers[opCallBuiltin] := @HandleCallBuiltin;
  FOpcodeHandlers[opDimArray] := @HandleDimArray;
  FOpcodeHandlers[opDimArrayTyped] := @HandleDimArrayTyped;  // TYPED ARRAYS
  FOpcodeHandlers[opLoadArray] := @HandleLoadArray;
  FOpcodeHandlers[opStoreArray] := @HandleStoreArray;
  FOpcodeHandlers[opLoadArray1D] := @HandleLoadArray1D;
  FOpcodeHandlers[opStoreArray1D] := @HandleStoreArray1D;
  FOpcodeHandlers[opLoadArray1DReg] := @HandleLoadArray1DReg;
  FOpcodeHandlers[opStoreArray1DReg] := @HandleStoreArray1DReg;
  
  // PHASE 4: Typed array handlers
  FOpcodeHandlers[opLoadArray1DInt] := @HandleLoadArray1DInt;
  FOpcodeHandlers[opStoreArray1DInt] := @HandleStoreArray1DInt;
  
  // DAY 6 PHASE 3: Fused array compare+jump
  FOpcodeHandlers[opJumpIfArrayEqImmInt] := @HandleJumpIfArrayEqImmInt;
  FOpcodeHandlers[opJumpIfArrayNeImmInt] := @HandleJumpIfArrayNeImmInt;
  
  // DAY 6 PHASE 4A: Extended fused array compare+jump (INTEGER)
  FOpcodeHandlers[opJumpIfArrayLtImmInt] := @HandleJumpIfArrayLtImmInt;
  FOpcodeHandlers[opJumpIfArrayLeImmInt] := @HandleJumpIfArrayLeImmInt;
  FOpcodeHandlers[opJumpIfArrayGtImmInt] := @HandleJumpIfArrayGtImmInt;
  FOpcodeHandlers[opJumpIfArrayGeImmInt] := @HandleJumpIfArrayGeImmInt;
  
  // DAY 6 PHASE 4A: Extended fused array compare+jump (FLOAT)
  FOpcodeHandlers[opJumpIfArrayEqImmFloat] := @HandleJumpIfArrayEqImmFloat;
  FOpcodeHandlers[opJumpIfArrayNeImmFloat] := @HandleJumpIfArrayNeImmFloat;
  FOpcodeHandlers[opJumpIfArrayLtImmFloat] := @HandleJumpIfArrayLtImmFloat;
  FOpcodeHandlers[opJumpIfArrayLeImmFloat] := @HandleJumpIfArrayLeImmFloat;
  FOpcodeHandlers[opJumpIfArrayGtImmFloat] := @HandleJumpIfArrayGtImmFloat;
  FOpcodeHandlers[opJumpIfArrayGeImmFloat] := @HandleJumpIfArrayGeImmFloat;
  
  // DAY 6 PHASE 4B: Array-array comparison fusion (INTEGER)
  FOpcodeHandlers[opJumpIfArrayEqArrayInt] := @HandleJumpIfArrayEqArrayInt;
  FOpcodeHandlers[opJumpIfArrayNeArrayInt] := @HandleJumpIfArrayNeArrayInt;
  FOpcodeHandlers[opJumpIfArrayLtArrayInt] := @HandleJumpIfArrayLtArrayInt;
  FOpcodeHandlers[opJumpIfArrayLeArrayInt] := @HandleJumpIfArrayLeArrayInt;
  FOpcodeHandlers[opJumpIfArrayGtArrayInt] := @HandleJumpIfArrayGtArrayInt;
  FOpcodeHandlers[opJumpIfArrayGeArrayInt] := @HandleJumpIfArrayGeArrayInt;
  
  // DAY 6 PHASE 4B: Array-array comparison fusion (FLOAT)
  FOpcodeHandlers[opJumpIfArrayEqArrayFloat] := @HandleJumpIfArrayEqArrayFloat;
  FOpcodeHandlers[opJumpIfArrayNeArrayFloat] := @HandleJumpIfArrayNeArrayFloat;
  FOpcodeHandlers[opJumpIfArrayLtArrayFloat] := @HandleJumpIfArrayLtArrayFloat;
  FOpcodeHandlers[opJumpIfArrayLeArrayFloat] := @HandleJumpIfArrayLeArrayFloat;
  FOpcodeHandlers[opJumpIfArrayGtArrayFloat] := @HandleJumpIfArrayGtArrayFloat;
  FOpcodeHandlers[opJumpIfArrayGeArrayFloat] := @HandleJumpIfArrayGeArrayFloat;
  
  // DAY 6 PHASE 4C: Arithmetic fusion (Array load + arithmetic) - INTEGER
  FOpcodeHandlers[opLoadArrayAddImmInt] := @HandleLoadArrayAddImmInt;
  FOpcodeHandlers[opLoadArraySubImmInt] := @HandleLoadArraySubImmInt;
  FOpcodeHandlers[opLoadArrayMulImmInt] := @HandleLoadArrayMulImmInt;
  FOpcodeHandlers[opStoreArrayAddImmInt] := @HandleStoreArrayAddImmInt;
  FOpcodeHandlers[opStoreArraySubImmInt] := @HandleStoreArraySubImmInt;
  FOpcodeHandlers[opStoreArrayMulImmInt] := @HandleStoreArrayMulImmInt;

  // DAY 6 PHASE 4C: Arithmetic fusion (Array load + arithmetic) - FLOAT
  FOpcodeHandlers[opLoadArrayAddImmFloat] := @HandleLoadArrayAddImmFloat;
  FOpcodeHandlers[opLoadArraySubImmFloat] := @HandleLoadArraySubImmFloat;
  FOpcodeHandlers[opLoadArrayMulImmFloat] := @HandleLoadArrayMulImmFloat;
  FOpcodeHandlers[opStoreArrayAddImmFloat] := @HandleStoreArrayAddImmFloat;
  FOpcodeHandlers[opStoreArraySubImmFloat] := @HandleStoreArraySubImmFloat;
  FOpcodeHandlers[opStoreArrayMulImmFloat] := @HandleStoreArrayMulImmFloat;

  // DAY 6 PHASE 4D: Load-Use fusion (Direct to register) - INTEGER
  FOpcodeHandlers[opLoadArrayAddImmToRegInt] := @HandleLoadArrayAddImmToRegInt;
  FOpcodeHandlers[opLoadArraySubImmToRegInt] := @HandleLoadArraySubImmToRegInt;
  FOpcodeHandlers[opLoadArrayMulImmToRegInt] := @HandleLoadArrayMulImmToRegInt;

  // DAY 6 PHASE 4D: Load-Use fusion (Direct to register) - FLOAT
  FOpcodeHandlers[opLoadArrayAddImmToRegFloat] := @HandleLoadArrayAddImmToRegFloat;
  FOpcodeHandlers[opLoadArraySubImmToRegFloat] := @HandleLoadArraySubImmToRegFloat;
  FOpcodeHandlers[opLoadArrayMulImmToRegFloat] := @HandleLoadArrayMulImmToRegFloat;

  // Combined operations (Quick Wins)
  FOpcodeHandlers[opIncVar] := @HandleIncVar;
  FOpcodeHandlers[opDecVar] := @HandleDecVar;
  FOpcodeHandlers[opAddVarConst] := @HandleAddVarConst;
  FOpcodeHandlers[opMulVarConst] := @HandleMulVarConst;
  
  // Register operations (Phase B)
  FOpcodeHandlers[opLoadReg] := @HandleLoadReg;
  FOpcodeHandlers[opStoreReg] := @HandleStoreReg;
  FOpcodeHandlers[opLoadVarToReg] := @HandleLoadVarToReg;
  FOpcodeHandlers[opStoreRegToVar] := @HandleStoreRegToVar;
  FOpcodeHandlers[opAddRegImm] := @HandleAddRegImm;
  FOpcodeHandlers[opIncReg] := @HandleIncReg;
  FOpcodeHandlers[opDecReg] := @HandleDecReg;
  FOpcodeHandlers[opAddReg] := @HandleAddReg;
  FOpcodeHandlers[opSubReg] := @HandleSubReg;
  FOpcodeHandlers[opMulReg] := @HandleMulReg;
  FOpcodeHandlers[opDivReg] := @HandleDivReg;
  FOpcodeHandlers[opCmpRegImm] := @HandleCmpRegImm;
  FOpcodeHandlers[opJumpIfRegLe] := @HandleJumpIfRegLe;
  
  // PHASE C: Typed register handlers
  FOpcodeHandlers[opLoadRegInt] := @HandleLoadRegInt;
  FOpcodeHandlers[opStoreRegInt] := @HandleStoreRegInt;
  FOpcodeHandlers[opLoadRegFloat] := @HandleLoadRegFloat;
  FOpcodeHandlers[opStoreRegFloat] := @HandleStoreRegFloat;

  // PHASE 1 OPTIMIZATION: Immediate load to registers
  FOpcodeHandlers[opLoadImmRegInt] := @HandleLoadImmRegInt;
  FOpcodeHandlers[opLoadImmRegFloat] := @HandleLoadImmRegFloat;
  FOpcodeHandlers[opMoveRegInt] := @HandleMoveRegInt;
  FOpcodeHandlers[opMoveRegFloat] := @HandleMoveRegFloat;
  FOpcodeHandlers[opAddRegInt] := @HandleAddRegInt;
  FOpcodeHandlers[opSubRegInt] := @HandleSubRegInt;
  FOpcodeHandlers[opMulRegInt] := @HandleMulRegInt;
  FOpcodeHandlers[opDivRegInt] := @HandleDivRegInt;
  FOpcodeHandlers[opIncRegInt] := @HandleIncRegInt;
  FOpcodeHandlers[opDecRegInt] := @HandleDecRegInt;
  FOpcodeHandlers[opAddRegFloat] := @HandleAddRegFloat;
  FOpcodeHandlers[opSubRegFloat] := @HandleSubRegFloat;
  FOpcodeHandlers[opMulRegFloat] := @HandleMulRegFloat;
  FOpcodeHandlers[opDivRegFloat] := @HandleDivRegFloat;
  FOpcodeHandlers[opIncRegFloat] := @HandleIncRegFloat;
  FOpcodeHandlers[opDecRegFloat] := @HandleDecRegFloat;
  FOpcodeHandlers[opCmpRegImmInt] := @HandleCmpRegImmInt;
  FOpcodeHandlers[opCmpRegImmFloat] := @HandleCmpRegImmFloat;
  FOpcodeHandlers[opLoadArray1DRegInt] := @HandleLoadArray1DRegInt;
  FOpcodeHandlers[opStoreArray1DRegInt] := @HandleStoreArray1DRegInt;
  FOpcodeHandlers[opLoadArray1DRegFloat] := @HandleLoadArray1DRegFloat;
  FOpcodeHandlers[opStoreArray1DRegFloat] := @HandleStoreArray1DRegFloat;

  // FASE 2: Immediate array store
  FOpcodeHandlers[opStoreArray1DRegImmInt] := @HandleStoreArray1DRegImmInt;
  FOpcodeHandlers[opStoreArray1DRegImmFloat] := @HandleStoreArray1DRegImmFloat;

  // STEP 3: Fused comparison+jump instructions
  FOpcodeHandlers[opJumpIfRegLeIntReg] := @HandleJumpIfRegLeIntReg;
  FOpcodeHandlers[opJumpIfRegLeFloatReg] := @HandleJumpIfRegLeFloatReg;
  FOpcodeHandlers[opJumpIfRegGtIntReg] := @HandleJumpIfRegGtIntReg;     // PHASE 4F
  FOpcodeHandlers[opJumpIfRegGtFloatReg] := @HandleJumpIfRegGtFloatReg; // PHASE 4F
  FOpcodeHandlers[opJumpIfRegEqIntReg] := @HandleJumpIfRegEqIntReg;     // PHASE 4F Step 2.3
  FOpcodeHandlers[opJumpIfRegEqFloatReg] := @HandleJumpIfRegEqFloatReg; // PHASE 4F Step 2.3
  FOpcodeHandlers[opJumpIfRegNeIntReg] := @HandleJumpIfRegNeIntReg;     // PHASE 4F Step 2.3
  FOpcodeHandlers[opJumpIfRegNeFloatReg] := @HandleJumpIfRegNeFloatReg; // PHASE 4F Step 2.3

  // PHASE 4F Step 2.5: Register-based math functions
  FOpcodeHandlers[opCallMath1FloatReg] := @HandleCallMath1FloatReg;

  // PHASE 4F Step 3: Register type conversions
  FOpcodeHandlers[opConvertFloatToIntReg] := @HandleConvertFloatToIntReg;
  FOpcodeHandlers[opConvertIntToFloatReg] := @HandleConvertIntToFloatReg;

  // PHASE 4E: String operations fusion
  FOpcodeHandlers[opStrConcatReg] := @HandleStrConcatReg;
  FOpcodeHandlers[opStrCmpRegEq] := @HandleStrCmpRegEq;
  FOpcodeHandlers[opStrCmpRegNe] := @HandleStrCmpRegNe;
  FOpcodeHandlers[opStrCmpRegLt] := @HandleStrCmpRegLt;
  FOpcodeHandlers[opStrCmpRegGt] := @HandleStrCmpRegGt;

  // PHASE 4E: Division/modulo fusion
  FOpcodeHandlers[opDivModFusionInt] := @HandleDivModFusionInt;

  // PHASE 2: Register-to-register comparisons
  FOpcodeHandlers[opLeRegInt] := @HandleLeRegInt;
  FOpcodeHandlers[opLeRegFloat] := @HandleLeRegFloat;
  FOpcodeHandlers[opLtRegInt] := @HandleLtRegInt;
  FOpcodeHandlers[opLtRegFloat] := @HandleLtRegFloat;
  FOpcodeHandlers[opGeRegInt] := @HandleGeRegInt;
  FOpcodeHandlers[opGeRegFloat] := @HandleGeRegFloat;
  FOpcodeHandlers[opGtRegInt] := @HandleGtRegInt;
  FOpcodeHandlers[opGtRegFloat] := @HandleGtRegFloat;
  FOpcodeHandlers[opEqRegInt] := @HandleEqRegInt;
  FOpcodeHandlers[opEqRegFloat] := @HandleEqRegFloat;
  FOpcodeHandlers[opNeqRegInt] := @HandleNeqRegInt;
  FOpcodeHandlers[opNeqRegFloat] := @HandleNeqRegFloat;

  // Debug & Control
  FOpcodeHandlers[opLineNumber] := @HandleLineNumber;
  FOpcodeHandlers[opNop] := @HandleNop;
  FOpcodeHandlers[opHalt] := @HandleHalt;
  FOpcodeHandlers[opCheckBreak] := @HandleCheckBreak;

  FHandlersInitialized := True;
end;

end.
