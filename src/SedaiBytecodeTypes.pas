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
unit SedaiBytecodeTypes;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, fgl, SedaiSSATypes;

type
  { Bytecode Opcode - maps directly to VM instructions }
  TBytecodeOp = (
    // Load constants
    bcLoadConstInt, bcLoadConstFloat, bcLoadConstString,
    // Copy between registers
    bcCopyInt, bcCopyFloat, bcCopyString,
    // Variable operations
    bcLoadVar, bcStoreVar,
    // Integer arithmetic
    bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt, bcNegInt,
    // Float arithmetic
    bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat, bcPowFloat, bcNegFloat,
    // Type conversions
    bcIntToFloat, bcFloatToInt, bcIntToString, bcFloatToString,
    bcStringToInt, bcStringToFloat,
    // Comparisons
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpGtInt, bcCmpLeInt, bcCmpGeInt,
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpGtFloat, bcCmpLeFloat, bcCmpGeFloat,
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString,
    // Logical operations
    bcLogicalAnd, bcLogicalOr, bcLogicalNot,
    // String operations
    bcStrConcat, bcStrLen, bcStrLeft, bcStrRight, bcStrMid,
    // Math functions
    bcMathSin, bcMathCos, bcMathTan, bcMathAtn, bcMathLog, bcMathExp,
    bcMathSqr, bcMathAbs, bcMathSgn, bcMathInt, bcMathRnd,
    // Control flow
    bcJump, bcJumpIfZero, bcJumpIfNotZero, bcCall, bcReturn,
    // Array operations (generic - deprecated, use typed versions)
    bcArrayLoad, bcArrayStore, bcArrayDim,
    // Typed array operations (for register compaction and performance)
    bcArrayLoadInt, bcArrayLoadFloat, bcArrayLoadString,
    bcArrayStoreInt, bcArrayStoreFloat, bcArrayStoreString,
    // I/O
    bcPrint, bcPrintLn, bcPrintString, bcPrintStringLn,
    bcInput, bcInputInt, bcInputFloat, bcInputString,
    // System
    bcEnd, bcStop, bcNop
  );

  { Bytecode instruction encoding }
  TBytecodeInstruction = packed record
    OpCode: Byte;           // Opcode (TBytecodeOp)
    Dest: Word;             // Destination register index (2 bytes, 0-65535)
    Src1: Word;             // Source 1 register index (2 bytes, 0-65535)
    Src2: Word;             // Source 2 register index (2 bytes, 0-65535)
    Immediate: Int64;       // Immediate value (for constants, jump offsets, etc)
    SourceLine: Integer;    // BASIC source line number (0 if unknown)
  end;

  { Variable info for runtime }
  TVariableInfo = record
    Name: string;
    RegType: Byte;          // 0=Int, 1=Float, 2=String
    IsArray: Boolean;
    ArraySize: Integer;
  end;

  { Variable Table - compatibility with executor context }
  TVariableTable = class
  private
    FNames: TStringList;
    function GetCount: Integer;
    function GetName(Index: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    function AllocateVariable(const Name: string): Integer;
    function FindVariable(const Name: string): Integer;
    function GetOrAllocate(const Name: string): Integer;
    procedure Clear;
    property Count: Integer read GetCount;
    property Names[Index: Integer]: string read GetName; default;
  end;

  { Bytecode program - compiled bytecode ready for VM }
  TBytecodeProgram = class
  private
    FInstructions: array of TBytecodeInstruction;
    FVariables: array of TVariableInfo;
    FArrays: array of TSSAArrayInfo;
    FStringConstants: TStringList;
    FEntryPoint: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddInstruction(const Instr: TBytecodeInstruction);
    procedure SetInstruction(Index: Integer; const Instr: TBytecodeInstruction);
    procedure ClearInstructions;
    procedure AddVariable(const VarInfo: TVariableInfo);
    procedure AddArrayInfo(const ArrInfo: TSSAArrayInfo);
    procedure SetArray(Index: Integer; const ArrInfo: TSSAArrayInfo);
    function AddStringConstant(const Str: string): Integer;
    function GetInstruction(Index: Integer): TBytecodeInstruction;
    function GetInstructionCount: Integer;
    function GetVariable(Index: Integer): TVariableInfo;
    function GetVariableCount: Integer;
    function GetArray(Index: Integer): TSSAArrayInfo;
    function GetArrayCount: Integer;
    function GetInstructionsPtr: Pointer;  // Direct access for fast VM loop
    property StringConstants: TStringList read FStringConstants;
    property EntryPoint: Integer read FEntryPoint write FEntryPoint;
  end;

function BytecodeOpToString(Op: TBytecodeOp): string;
function OpcodeToString(OpCode: Byte): string;  // Handles both regular and superinstructions
function MakeBytecodeInstruction(OpCode: TBytecodeOp; Dest, Src1, Src2: Word; Immediate: Int64): TBytecodeInstruction;

implementation

uses TypInfo;

{ TVariableTable }

constructor TVariableTable.Create;
begin
  inherited Create;
  FNames := TStringList.Create;
  FNames.CaseSensitive := False;
  FNames.Duplicates := dupIgnore;
end;

destructor TVariableTable.Destroy;
begin
  FNames.Free;
  inherited Destroy;
end;

function TVariableTable.GetCount: Integer;
begin
  Result := FNames.Count;
end;

function TVariableTable.GetName(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FNames.Count) then
    Result := FNames[Index]
  else
    Result := '';
end;

function TVariableTable.AllocateVariable(const Name: string): Integer;
begin
  Result := FNames.IndexOf(UpperCase(Name));
  if Result < 0 then
    Result := FNames.Add(UpperCase(Name));
end;

function TVariableTable.FindVariable(const Name: string): Integer;
begin
  Result := FNames.IndexOf(UpperCase(Name));
end;

function TVariableTable.GetOrAllocate(const Name: string): Integer;
begin
  Result := AllocateVariable(Name);
end;

procedure TVariableTable.Clear;
begin
  FNames.Clear;
end;

{ TBytecodeProgram }

constructor TBytecodeProgram.Create;
begin
  inherited Create;
  SetLength(FInstructions, 0);
  SetLength(FVariables, 0);
  FStringConstants := TStringList.Create;
  FEntryPoint := 0;
end;

destructor TBytecodeProgram.Destroy;
begin
  FStringConstants.Free;
  inherited Destroy;
end;

procedure TBytecodeProgram.AddInstruction(const Instr: TBytecodeInstruction);
var
  Len: Integer;
begin
  Len := Length(FInstructions);
  SetLength(FInstructions, Len + 1);
  FInstructions[Len] := Instr;
end;

procedure TBytecodeProgram.SetInstruction(Index: Integer; const Instr: TBytecodeInstruction);
begin
  if (Index >= 0) and (Index < Length(FInstructions)) then
    FInstructions[Index] := Instr;
end;

procedure TBytecodeProgram.ClearInstructions;
begin
  SetLength(FInstructions, 0);
end;

procedure TBytecodeProgram.AddVariable(const VarInfo: TVariableInfo);
var
  Len: Integer;
begin
  Len := Length(FVariables);
  SetLength(FVariables, Len + 1);
  FVariables[Len] := VarInfo;
end;

function TBytecodeProgram.AddStringConstant(const Str: string): Integer;
begin
  Result := FStringConstants.IndexOf(Str);
  if Result = -1 then
    Result := FStringConstants.Add(Str);
end;

function TBytecodeProgram.GetInstruction(Index: Integer): TBytecodeInstruction;
begin
  if (Index >= 0) and (Index < Length(FInstructions)) then
    Result := FInstructions[Index]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TBytecodeProgram.GetInstructionCount: Integer;
begin
  Result := Length(FInstructions);
end;

function TBytecodeProgram.GetVariable(Index: Integer): TVariableInfo;
begin
  if (Index >= 0) and (Index < Length(FVariables)) then
    Result := FVariables[Index]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TBytecodeProgram.GetVariableCount: Integer;
begin
  Result := Length(FVariables);
end;

procedure TBytecodeProgram.AddArrayInfo(const ArrInfo: TSSAArrayInfo);
var
  Idx: Integer;
begin
  Idx := Length(FArrays);
  SetLength(FArrays, Idx + 1);
  FArrays[Idx] := ArrInfo;
end;

function TBytecodeProgram.GetArray(Index: Integer): TSSAArrayInfo;
begin
  if (Index >= 0) and (Index < Length(FArrays)) then
    Result := FArrays[Index]
  else
    raise Exception.CreateFmt('Array index out of bounds: %d', [Index]);
end;

procedure TBytecodeProgram.SetArray(Index: Integer; const ArrInfo: TSSAArrayInfo);
begin
  if (Index >= 0) and (Index < Length(FArrays)) then
    FArrays[Index] := ArrInfo;
end;

function TBytecodeProgram.GetArrayCount: Integer;
begin
  Result := Length(FArrays);
end;

function TBytecodeProgram.GetInstructionsPtr: Pointer;
begin
  if Length(FInstructions) > 0 then
    Result := @FInstructions[0]
  else
    Result := nil;
end;

function BytecodeOpToString(Op: TBytecodeOp): string;
begin
  Result := GetEnumName(TypeInfo(TBytecodeOp), Ord(Op));
  if Copy(Result, 1, 2) = 'bc' then
    Result := Copy(Result, 3, Length(Result) - 2);
end;

function OpcodeToString(OpCode: Byte): string;
begin
  // Handle superinstructions (opcodes >= 100)
  if OpCode >= 100 then
  begin
    case OpCode of
      100: Result := 'BranchEqInt';
      101: Result := 'BranchNeInt';
      102: Result := 'BranchLtInt';
      103: Result := 'BranchGtInt';
      104: Result := 'BranchLeInt';
      105: Result := 'BranchGeInt';
      110: Result := 'BranchEqFloat';
      111: Result := 'BranchNeFloat';
      112: Result := 'BranchLtFloat';
      113: Result := 'BranchGtFloat';
      114: Result := 'BranchLeFloat';
      115: Result := 'BranchGeFloat';
      120: Result := 'AddIntTo';
      121: Result := 'SubIntTo';
      122: Result := 'MulIntTo';
      130: Result := 'AddFloatTo';
      131: Result := 'SubFloatTo';
      132: Result := 'MulFloatTo';
      133: Result := 'DivFloatTo';
      140: Result := 'AddIntConst';
      141: Result := 'SubIntConst';
      142: Result := 'MulIntConst';
      150: Result := 'AddFloatConst';
      151: Result := 'SubFloatConst';
      152: Result := 'MulFloatConst';
      153: Result := 'DivFloatConst';
      160: Result := 'BranchEqZeroInt';
      161: Result := 'BranchNeZeroInt';
      170: Result := 'BranchEqZeroFlt';
      171: Result := 'BranchNeZeroFlt';
      180: Result := 'ArrayStoreIntConst';
      181: Result := 'ArrayStoreFltConst';
      182: Result := 'ArrayStoreStrConst';
      190: Result := 'AddIntToBranchLe';
      191: Result := 'AddIntToBranchLt';
      192: Result := 'SubIntToBranchGe';
      193: Result := 'SubIntToBranchGt';
    else
      Result := Format('SuperInstr_%d', [OpCode]);
    end;
  end
  else
    Result := BytecodeOpToString(TBytecodeOp(OpCode));
end;

function MakeBytecodeInstruction(OpCode: TBytecodeOp; Dest, Src1, Src2: Word; Immediate: Int64): TBytecodeInstruction;
begin
  // Initialize entire record to zero to avoid garbage in padding bytes
  FillChar(Result, SizeOf(Result), 0);
  Result.OpCode := Byte(OpCode);
  Result.Dest := Dest;
  Result.Src1 := Src1;
  Result.Src2 := Src2;
  Result.Immediate := Immediate;
  Result.SourceLine := 0;  // Will be set by bytecode compiler from SSA instruction
end;

end.
