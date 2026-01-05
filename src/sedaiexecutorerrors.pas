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
unit SedaiExecutorErrors;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils;

type
  // === BASE EXECUTOR EXCEPTION ===
  TExecutorException = class(Exception)
  private
    FLineNumber: Integer;
    FStatementText: string;
    FErrorCode: Integer;

  public
    constructor Create(const Msg: string); overload;
    constructor Create(const Msg: string; LineNumber: Integer); overload;
    constructor Create(const Msg: string; LineNumber: Integer; const StatementText: string); overload;
    constructor CreateWithCode(const Msg: string; ErrorCode: Integer); overload;
    constructor CreateWithCode(const Msg: string; ErrorCode: Integer; LineNumber: Integer); overload;

    function GetFullMessage: string;
    function GetErrorDescription: string;

    property LineNumber: Integer read FLineNumber write FLineNumber;
    property StatementText: string read FStatementText write FStatementText;
    property ErrorCode: Integer read FErrorCode write FErrorCode;
  end;

  // === RUNTIME EXCEPTIONS ===
  TExecutorRuntimeException = class(TExecutorException);
  TExecutorSyntaxException = class(TExecutorException);
  TExecutorTypeException = class(TExecutorException);
  TExecutorMathException = class(TExecutorException);
  TExecutorStringException = class(TExecutorException);
  TExecutorArrayException = class(TExecutorException);
  TExecutorIOException = class(TExecutorException);
  TExecutorMemoryException = class(TExecutorException);

  // === FLOW CONTROL EXCEPTIONS ===
  TExecutorFlowException = class(TExecutorException);
  TExecutorBreakException = class(TExecutorFlowException);
  TExecutorStopException = class(TExecutorFlowException);
  TExecutorEndException = class(TExecutorFlowException);
  TExecutorReturnException = class(TExecutorFlowException);
  TExecutorGotoException = class(TExecutorFlowException);

  // === SPECIFIC ERROR TYPES ===
  TExecutorDivisionByZeroException = class(TExecutorMathException)
  public
    constructor Create;
  end;

  TExecutorArrayIndexException = class(TExecutorArrayException)
  private
    FIndex: Integer;
    FMaxIndex: Integer;
  public
    constructor Create(Index, MaxIndex: Integer);
    property Index: Integer read FIndex;
    property MaxIndex: Integer read FMaxIndex;
  end;

  TExecutorUndefinedVariableException = class(TExecutorRuntimeException)
  private
    FVariableName: string;
  public
    constructor Create(const VariableName: string);
    property VariableName: string read FVariableName;
  end;

  TExecutorUndefinedFunctionException = class(TExecutorRuntimeException)
  private
    FFunctionName: string;
  public
    constructor Create(const FunctionName: string);
    property FunctionName: string read FFunctionName;
  end;

  TExecutorInvalidArgumentException = class(TExecutorRuntimeException)
  private
    FFunctionName: string;
    FExpectedArgs: Integer;
    FActualArgs: Integer;
  public
    constructor Create(const FunctionName: string; ExpectedArgs, ActualArgs: Integer);
    property FunctionName: string read FFunctionName;
    property ExpectedArgs: Integer read FExpectedArgs;
    property ActualArgs: Integer read FActualArgs;
  end;

  TExecutorOutOfDataException = class(TExecutorRuntimeException)
  public
    constructor Create;
  end;

  TExecutorReturnWithoutGosubException = class(TExecutorFlowException)
  public
    constructor Create;
  end;

  TExecutorLineNotFoundException = class(TExecutorFlowException)
  private
    FTargetLine: Integer;
  public
    constructor Create(TargetLine: Integer);
    property TargetLine: Integer read FTargetLine;
  end;

  // === ERROR CODES ===
  // C128 BASIC 7.0 compatible error codes (1-41)
  // Extended codes (100+) for SedaiBasic-specific errors
const
  ERR_NONE = 0;

  // C128 BASIC 7.0 Error Codes (1-41)
  ERR_TOO_MANY_FILES       = 1;   // Too many files open (limit 10)
  ERR_FILE_OPEN            = 2;   // File already open
  ERR_FILE_NOT_OPEN        = 3;   // File not open
  ERR_FILE_NOT_FOUND       = 4;   // File not found
  ERR_DEVICE_NOT_PRESENT   = 5;   // Device not present/available
  ERR_NOT_INPUT_FILE       = 6;   // Not an input file
  ERR_NOT_OUTPUT_FILE      = 7;   // Not an output file
  ERR_MISSING_FILE_NAME    = 8;   // Missing file name
  ERR_ILLEGAL_DEVICE_NUM   = 9;   // Illegal device number
  ERR_NEXT_WITHOUT_FOR     = 10;  // NEXT without FOR
  ERR_SYNTAX               = 11;  // Syntax error
  ERR_RETURN_WITHOUT_GOSUB = 12;  // RETURN without GOSUB
  ERR_OUT_OF_DATA          = 13;  // Out of DATA
  ERR_ILLEGAL_QUANTITY     = 14;  // Illegal quantity (argument out of range)
  ERR_OVERFLOW             = 15;  // Overflow (result > 1.7E+38)
  ERR_OUT_OF_MEMORY        = 16;  // Out of memory
  ERR_UNDEFINED_STATEMENT  = 17;  // Undefined statement (line not found)
  ERR_BAD_SUBSCRIPT        = 18;  // Bad subscript (array index out of range)
  ERR_REDIM_ARRAY          = 19;  // Redimensioned array
  ERR_DIVISION_BY_ZERO     = 20;  // Division by zero
  ERR_ILLEGAL_DIRECT       = 21;  // Illegal direct mode command
  ERR_TYPE_MISMATCH        = 22;  // Type mismatch
  ERR_STRING_TOO_LONG      = 23;  // String too long (>255 chars)
  ERR_FILE_DATA            = 24;  // Bad file data
  ERR_FORMULA_TOO_COMPLEX  = 25;  // Formula too complex
  ERR_CANT_CONTINUE        = 26;  // Can't continue
  ERR_UNDEFINED_FUNCTION   = 27;  // Undefined function
  ERR_VERIFY               = 28;  // Verify error
  ERR_LOAD                 = 29;  // Load error
  ERR_BREAK                = 30;  // Break (STOP key pressed)
  ERR_CANT_RESUME          = 31;  // Can't resume (no TRAP active)
  ERR_LOOP_NOT_FOUND       = 32;  // Loop not found (DO without LOOP)
  ERR_LOOP_WITHOUT_DO      = 33;  // Loop without DO
  ERR_DIRECT_MODE_ONLY     = 34;  // Direct mode only command
  ERR_NO_GRAPHICS_AREA     = 35;  // No graphics area (GRAPHIC not executed)
  ERR_BAD_DISK             = 36;  // Bad disk
  ERR_BEND_NOT_FOUND       = 37;  // BEND not found (BEGIN without BEND)
  ERR_LINE_TOO_LARGE       = 38;  // Line number too large (>63999)
  ERR_UNRESOLVED_REF       = 39;  // Unresolved reference in RENUMBER
  ERR_UNIMPLEMENTED        = 40;  // Unimplemented command
  ERR_FILE_READ            = 41;  // File read error

  // SedaiBasic Extended Error Codes (100+)
  ERR_INTERNAL             = 100; // Internal error
  ERR_STACK_OVERFLOW       = 101; // Stack overflow
  ERR_UNDERFLOW            = 102; // Numeric underflow
  ERR_DOMAIN               = 103; // Domain error (math function)
  ERR_RANGE                = 104; // Range error
  ERR_UNDEFINED_VARIABLE   = 105; // Undefined variable
  ERR_ARRAY_NOT_DIM        = 106; // Array not dimensioned
  ERR_WRONG_DIMENSIONS     = 107; // Wrong number of array dimensions
  ERR_INVALID_STRING_INDEX = 108; // Invalid string index
  ERR_STRING_CONVERSION    = 109; // String conversion error
  ERR_FILE_ACCESS          = 110; // File access error
  ERR_INPUT_PAST_END       = 111; // Input past end of file
  ERR_WRONG_NUM_ARGS       = 112; // Wrong number of arguments
  ERR_INVALID_ARGUMENT     = 113; // Invalid argument

  // Aliases for backward compatibility
  ERR_UNDEFINED_LINE = ERR_UNDEFINED_STATEMENT;
  ERR_ARRAY_INDEX_OUT_OF_BOUNDS = ERR_BAD_SUBSCRIPT;
  ERR_REDIMENSIONED_ARRAY = ERR_REDIM_ARRAY;
  ERR_ILLEGAL_FUNCTION_CALL = ERR_ILLEGAL_QUANTITY;
  ERR_DEVICE_NOT_AVAILABLE = ERR_DEVICE_NOT_PRESENT;
  ERR_WRONG_NUMBER_OF_ARGS = ERR_WRONG_NUM_ARGS;
  ERR_FOR_WITHOUT_NEXT = ERR_LOOP_NOT_FOUND;
  ERR_ARRAY_NOT_DIMENSIONED = ERR_ARRAY_NOT_DIM;
  ERR_WRONG_NUMBER_OF_DIMENSIONS = ERR_WRONG_DIMENSIONS;

// === UTILITY FUNCTIONS ===
function GetErrorCodeDescription(ErrorCode: Integer): string;
function CreateExecutorError(ErrorCode: Integer; const Message: string; LineNumber: Integer = 0): TExecutorException;

implementation

// === ERROR DESCRIPTIONS ===
// Returns C128 BASIC 7.0 compatible error messages
function GetErrorCodeDescription(ErrorCode: Integer): string;
begin
  case ErrorCode of
    ERR_NONE: Result := '';

    // C128 BASIC 7.0 Error Messages (1-41)
    ERR_TOO_MANY_FILES:       Result := 'TOO MANY FILES';
    ERR_FILE_OPEN:            Result := 'FILE OPEN';
    ERR_FILE_NOT_OPEN:        Result := 'FILE NOT OPEN';
    ERR_FILE_NOT_FOUND:       Result := 'FILE NOT FOUND';
    ERR_DEVICE_NOT_PRESENT:   Result := 'DEVICE NOT PRESENT';
    ERR_NOT_INPUT_FILE:       Result := 'NOT INPUT FILE';
    ERR_NOT_OUTPUT_FILE:      Result := 'NOT OUTPUT FILE';
    ERR_MISSING_FILE_NAME:    Result := 'MISSING FILE NAME';
    ERR_ILLEGAL_DEVICE_NUM:   Result := 'ILLEGAL DEVICE NUMBER';
    ERR_NEXT_WITHOUT_FOR:     Result := 'NEXT WITHOUT FOR';
    ERR_SYNTAX:               Result := 'SYNTAX';
    ERR_RETURN_WITHOUT_GOSUB: Result := 'RETURN WITHOUT GOSUB';
    ERR_OUT_OF_DATA:          Result := 'OUT OF DATA';
    ERR_ILLEGAL_QUANTITY:     Result := 'ILLEGAL QUANTITY';
    ERR_OVERFLOW:             Result := 'OVERFLOW';
    ERR_OUT_OF_MEMORY:        Result := 'OUT OF MEMORY';
    ERR_UNDEFINED_STATEMENT:  Result := 'UNDEF''D STATEMENT';
    ERR_BAD_SUBSCRIPT:        Result := 'BAD SUBSCRIPT';
    ERR_REDIM_ARRAY:          Result := 'REDIM''D ARRAY';
    ERR_DIVISION_BY_ZERO:     Result := 'DIVISION BY ZERO';
    ERR_ILLEGAL_DIRECT:       Result := 'ILLEGAL DIRECT';
    ERR_TYPE_MISMATCH:        Result := 'TYPE MISMATCH';
    ERR_STRING_TOO_LONG:      Result := 'STRING TOO LONG';
    ERR_FILE_DATA:            Result := 'FILE DATA';
    ERR_FORMULA_TOO_COMPLEX:  Result := 'FORMULA TOO COMPLEX';
    ERR_CANT_CONTINUE:        Result := 'CAN''T CONTINUE';
    ERR_UNDEFINED_FUNCTION:   Result := 'UNDEF''D FUNCTION';
    ERR_VERIFY:               Result := 'VERIFY';
    ERR_LOAD:                 Result := 'LOAD';
    ERR_BREAK:                Result := 'BREAK';
    ERR_CANT_RESUME:          Result := 'CAN''T RESUME';
    ERR_LOOP_NOT_FOUND:       Result := 'LOOP NOT FOUND';
    ERR_LOOP_WITHOUT_DO:      Result := 'LOOP WITHOUT DO';
    ERR_DIRECT_MODE_ONLY:     Result := 'DIRECT MODE ONLY';
    ERR_NO_GRAPHICS_AREA:     Result := 'NO GRAPHICS AREA';
    ERR_BAD_DISK:             Result := 'BAD DISK';
    ERR_BEND_NOT_FOUND:       Result := 'BEND NOT FOUND';
    ERR_LINE_TOO_LARGE:       Result := 'LINE NUMBER TOO LARGE';
    ERR_UNRESOLVED_REF:       Result := 'UNRESOLVED REFERENCE';
    ERR_UNIMPLEMENTED:        Result := 'UNIMPLEMENTED COMMAND';
    ERR_FILE_READ:            Result := 'FILE READ';

    // SedaiBasic Extended Error Codes (100+)
    ERR_INTERNAL:             Result := 'INTERNAL ERROR';
    ERR_STACK_OVERFLOW:       Result := 'STACK OVERFLOW';
    ERR_UNDERFLOW:            Result := 'UNDERFLOW';
    ERR_DOMAIN:               Result := 'DOMAIN ERROR';
    ERR_RANGE:                Result := 'RANGE ERROR';
    ERR_UNDEFINED_VARIABLE:   Result := 'UNDEFINED VARIABLE';
    ERR_ARRAY_NOT_DIM:        Result := 'ARRAY NOT DIMENSIONED';
    ERR_WRONG_DIMENSIONS:     Result := 'WRONG NUMBER OF DIMENSIONS';
    ERR_INVALID_STRING_INDEX: Result := 'INVALID STRING INDEX';
    ERR_STRING_CONVERSION:    Result := 'STRING CONVERSION ERROR';
    ERR_FILE_ACCESS:          Result := 'FILE ACCESS ERROR';
    ERR_INPUT_PAST_END:       Result := 'INPUT PAST END';
    ERR_WRONG_NUM_ARGS:       Result := 'WRONG NUMBER OF ARGUMENTS';
    ERR_INVALID_ARGUMENT:     Result := 'INVALID ARGUMENT';

    else
      Result := Format('ERROR %d', [ErrorCode]);
  end;
end;

function CreateExecutorError(ErrorCode: Integer; const Message: string; LineNumber: Integer): TExecutorException;
begin
  case ErrorCode of
    ERR_DIVISION_BY_ZERO:
      Result := TExecutorDivisionByZeroException.Create;
    ERR_UNDEFINED_VARIABLE:
      Result := TExecutorUndefinedVariableException.Create(Message);
    ERR_UNDEFINED_FUNCTION:
      Result := TExecutorUndefinedFunctionException.Create(Message);
    ERR_OUT_OF_DATA:
      Result := TExecutorOutOfDataException.Create;
    ERR_RETURN_WITHOUT_GOSUB:
      Result := TExecutorReturnWithoutGosubException.Create;
    ERR_ARRAY_INDEX_OUT_OF_BOUNDS:
      Result := TExecutorArrayIndexException.Create(0, 0); // Would need more params in real usage
    ERR_TYPE_MISMATCH:
      Result := TExecutorTypeException.CreateWithCode(Message, ErrorCode, LineNumber);
    ERR_SYNTAX:
      Result := TExecutorSyntaxException.CreateWithCode(Message, ErrorCode, LineNumber);
    ERR_OVERFLOW, ERR_UNDERFLOW, ERR_DOMAIN, ERR_RANGE:
      Result := TExecutorMathException.CreateWithCode(Message, ErrorCode, LineNumber);
    ERR_STRING_TOO_LONG, ERR_INVALID_STRING_INDEX, ERR_STRING_CONVERSION:
      Result := TExecutorStringException.CreateWithCode(Message, ErrorCode, LineNumber);
    ERR_FILE_NOT_FOUND, ERR_FILE_ACCESS, ERR_DEVICE_NOT_AVAILABLE, ERR_INPUT_PAST_END:
      Result := TExecutorIOException.CreateWithCode(Message, ErrorCode, LineNumber);
    ERR_OUT_OF_MEMORY, ERR_STACK_OVERFLOW:
      Result := TExecutorMemoryException.CreateWithCode(Message, ErrorCode, LineNumber);
    else
      Result := TExecutorRuntimeException.CreateWithCode(Message, ErrorCode, LineNumber);
  end;
end;

// === EXCEPTION IMPLEMENTATIONS ===

{ TExecutorException }

constructor TExecutorException.Create(const Msg: string);
begin
  inherited Create(Msg);
  FLineNumber := 0;
  FStatementText := '';
  FErrorCode := ERR_NONE;
end;

constructor TExecutorException.Create(const Msg: string; LineNumber: Integer);
begin
  Create(Msg);
  FLineNumber := LineNumber;
end;

constructor TExecutorException.Create(const Msg: string; LineNumber: Integer; const StatementText: string);
begin
  Create(Msg, LineNumber);
  FStatementText := StatementText;
end;

constructor TExecutorException.CreateWithCode(const Msg: string; ErrorCode: Integer);
begin
  Create(Msg);
  FErrorCode := ErrorCode;
end;

constructor TExecutorException.CreateWithCode(const Msg: string; ErrorCode: Integer; LineNumber: Integer);
begin
  Create(Msg, LineNumber);
  FErrorCode := ErrorCode;
end;

function TExecutorException.GetFullMessage: string;
begin
  Result := Message;

  if FLineNumber > 0 then
    Result := Format('Error at line %d: %s', [FLineNumber, Result]);

  if FStatementText <> '' then
    Result := Result + Format(' in "%s"', [FStatementText]);

  if FErrorCode <> ERR_NONE then
    Result := Result + Format(' (Error %d)', [FErrorCode]);
end;

function TExecutorException.GetErrorDescription: string;
begin
  if FErrorCode <> ERR_NONE then
    Result := GetErrorCodeDescription(FErrorCode)  // ← Nome diverso
  else
    Result := Message;
end;

{ TExecutorDivisionByZeroException }

constructor TExecutorDivisionByZeroException.Create;
begin
  inherited CreateWithCode('Division by zero', ERR_DIVISION_BY_ZERO);
end;

{ TExecutorArrayIndexException }

constructor TExecutorArrayIndexException.Create(Index, MaxIndex: Integer);
begin
  inherited CreateWithCode(Format('Array index %d out of bounds (0..%d)', [Index, MaxIndex]), ERR_ARRAY_INDEX_OUT_OF_BOUNDS);
  FIndex := Index;
  FMaxIndex := MaxIndex;
end;

{ TExecutorUndefinedVariableException }

constructor TExecutorUndefinedVariableException.Create(const VariableName: string);
begin
  inherited CreateWithCode(Format('Undefined variable: %s', [VariableName]), ERR_UNDEFINED_VARIABLE);
  FVariableName := VariableName;
end;

{ TExecutorUndefinedFunctionException }

constructor TExecutorUndefinedFunctionException.Create(const FunctionName: string);
begin
  inherited CreateWithCode(Format('Undefined function: %s', [FunctionName]), ERR_UNDEFINED_FUNCTION);
  FFunctionName := FunctionName;
end;

{ TExecutorInvalidArgumentException }

constructor TExecutorInvalidArgumentException.Create(const FunctionName: string; ExpectedArgs, ActualArgs: Integer);
begin
  inherited CreateWithCode(Format('Function %s expects %d arguments, got %d', [FunctionName, ExpectedArgs, ActualArgs]), ERR_WRONG_NUMBER_OF_ARGS);
  FFunctionName := FunctionName;
  FExpectedArgs := ExpectedArgs;
  FActualArgs := ActualArgs;
end;

{ TExecutorOutOfDataException }

constructor TExecutorOutOfDataException.Create;
begin
  inherited CreateWithCode('Out of DATA', ERR_OUT_OF_DATA);
end;

{ TExecutorReturnWithoutGosubException }

constructor TExecutorReturnWithoutGosubException.Create;
begin
  inherited CreateWithCode('RETURN without GOSUB', ERR_RETURN_WITHOUT_GOSUB);
end;

{ TExecutorLineNotFoundException }

constructor TExecutorLineNotFoundException.Create(TargetLine: Integer);
begin
  inherited CreateWithCode(Format('Line number %d not found', [TargetLine]), ERR_UNDEFINED_LINE);
  FTargetLine := TargetLine;
end;

end.
