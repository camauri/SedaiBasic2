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
    // I/O - Print values
    bcPrint, bcPrintLn, bcPrintString, bcPrintStringLn,
    bcPrintInt, bcPrintIntLn,
    // I/O - Print separators and formatting
    bcPrintComma,      // Apply comma separator (TAB zone)
    bcPrintSemicolon,  // Apply semicolon separator (concatenate)
    bcPrintTab,        // TAB(n) - move to column n (Immediate = column)
    bcPrintSpc,        // SPC(n) - print n spaces (Immediate = count)
    bcPrintNewLine,    // Force newline
    // I/O - Input
    bcInput, bcInputInt, bcInputFloat, bcInputString,
    // Graphics - MUST stay under opcode 100 (superinstructions start at 100)
    bcGraphicRGBA,     // Create 32-bit RGBA color: Dest = RGBA(Src1, Src2, Src3, Immediate=alpha)
    bcGraphicSetMode,  // Set graphics mode: GRAPHIC Src1(mode), Src2(clear), Immediate(param3)
    bcGraphicBox,      // Draw box
    bcGraphicCircle,   // Draw circle/ellipse/arc
    bcGraphicDraw,     // Draw dot or line
    bcGraphicLocate,   // Set pixel cursor position
    bcGraphicRdot,     // Get pixel cursor position or color
    bcGraphicGetMode,  // RGR - Get current graphics mode (0-11)
    // System commands - keep at end but MUST be under opcode 100 (superinstructions start at 100)
    bcEnd, bcStop, bcFast, bcSlow, bcSleep,
    // NOP must be last regular opcode (before superinstructions at 100+)
    bcNop
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
const
  SUPERINSTR_BASE = 110;  // Superinstructions start at 110 (regular opcodes 0-109)
begin
  // Handle superinstructions (opcodes >= SUPERINSTR_BASE)
  if OpCode >= SUPERINSTR_BASE then
  begin
    case OpCode of
      110: Result := 'BranchEqInt';
      111: Result := 'BranchNeInt';
      112: Result := 'BranchLtInt';
      113: Result := 'BranchGtInt';
      114: Result := 'BranchLeInt';
      115: Result := 'BranchGeInt';
      120: Result := 'BranchEqFloat';
      121: Result := 'BranchNeFloat';
      122: Result := 'BranchLtFloat';
      123: Result := 'BranchGtFloat';
      124: Result := 'BranchLeFloat';
      125: Result := 'BranchGeFloat';
      130: Result := 'AddIntTo';
      131: Result := 'SubIntTo';
      132: Result := 'MulIntTo';
      140: Result := 'AddFloatTo';
      141: Result := 'SubFloatTo';
      142: Result := 'MulFloatTo';
      143: Result := 'DivFloatTo';
      150: Result := 'AddIntConst';
      151: Result := 'SubIntConst';
      152: Result := 'MulIntConst';
      160: Result := 'AddFloatConst';
      161: Result := 'SubFloatConst';
      162: Result := 'MulFloatConst';
      163: Result := 'DivFloatConst';
      170: Result := 'BranchEqZeroInt';
      171: Result := 'BranchNeZeroInt';
      180: Result := 'BranchEqZeroFlt';
      181: Result := 'BranchNeZeroFlt';
      190: Result := 'ArrayStoreIntConst';
      191: Result := 'ArrayStoreFltConst';
      192: Result := 'ArrayStoreStrConst';
      200: Result := 'AddIntToBranchLe';
      201: Result := 'AddIntToBranchLt';
      202: Result := 'SubIntToBranchGe';
      203: Result := 'SubIntToBranchGt';
      // FMA and advanced float patterns
      210: Result := 'MulAddFloat';
      211: Result := 'MulSubFloat';
      212: Result := 'MulAddToFloat';
      213: Result := 'MulSubToFloat';
      220: Result := 'ArrayLoadAddFloat';
      221: Result := 'ArrayLoadSubFloat';
      222: Result := 'ArrayLoadDivAddFloat';
      230: Result := 'SquareSumFloat';
      231: Result := 'AddSquareFloat';
      240: Result := 'MulMulFloat';
      241: Result := 'AddSqrtFloat';
      250: Result := 'ArrayLoadIntBranchNZ';
      251: Result := 'ArrayLoadIntBranchZ';
      // Legacy superinstructions (mapped for compatibility)
      166: Result := 'ArrayReverseRange';
      167: Result := 'ArrayShiftLeft';
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
