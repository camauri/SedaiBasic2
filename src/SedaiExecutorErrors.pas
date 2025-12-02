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
const
  // General errors
  ERR_NONE = 0;
  ERR_SYNTAX = 1;
  ERR_TYPE_MISMATCH = 2;
  ERR_OUT_OF_MEMORY = 3;
  ERR_STACK_OVERFLOW = 4;
  ERR_INTERNAL = 5;

  // Math errors
  ERR_DIVISION_BY_ZERO = 10;
  ERR_OVERFLOW = 11;
  ERR_UNDERFLOW = 12;
  ERR_DOMAIN = 13;
  ERR_RANGE = 14;

  // Variable and array errors
  ERR_UNDEFINED_VARIABLE = 20;
  ERR_UNDEFINED_FUNCTION = 21;
  ERR_ARRAY_NOT_DIMENSIONED = 22;
  ERR_ARRAY_INDEX_OUT_OF_BOUNDS = 23;
  ERR_REDIMENSIONED_ARRAY = 24;
  ERR_WRONG_NUMBER_OF_DIMENSIONS = 25;

  // String errors
  ERR_STRING_TOO_LONG = 30;
  ERR_INVALID_STRING_INDEX = 31;
  ERR_STRING_CONVERSION = 32;

  // I/O errors
  ERR_FILE_NOT_FOUND = 40;
  ERR_FILE_ACCESS = 41;
  ERR_DEVICE_NOT_AVAILABLE = 42;
  ERR_OUT_OF_DATA = 43;
  ERR_INPUT_PAST_END = 44;

  // Flow control errors
  ERR_RETURN_WITHOUT_GOSUB = 50;
  ERR_NEXT_WITHOUT_FOR = 51;
  ERR_FOR_WITHOUT_NEXT = 52;
  ERR_UNDEFINED_LINE = 53;
  ERR_ILLEGAL_DIRECT = 54;

  // Function errors
  ERR_ILLEGAL_FUNCTION_CALL = 60;
  ERR_WRONG_NUMBER_OF_ARGS = 61;
  ERR_INVALID_ARGUMENT = 62;

// === UTILITY FUNCTIONS ===
function GetErrorCodeDescription(ErrorCode: Integer): string;
function CreateExecutorError(ErrorCode: Integer; const Message: string; LineNumber: Integer = 0): TExecutorException;

implementation

// === ERROR DESCRIPTIONS ===
function GetErrorCodeDescription(ErrorCode: Integer): string;
begin
  case ErrorCode of
    ERR_NONE: Result := 'No error';
    ERR_SYNTAX: Result := 'Syntax error';
    ERR_TYPE_MISMATCH: Result := 'Type mismatch';
    ERR_OUT_OF_MEMORY: Result := 'Out of memory';
    ERR_STACK_OVERFLOW: Result := 'Stack overflow';
    ERR_INTERNAL: Result := 'Internal error';

    ERR_DIVISION_BY_ZERO: Result := 'Division by zero';
    ERR_OVERFLOW: Result := 'Overflow';
    ERR_UNDERFLOW: Result := 'Underflow';
    ERR_DOMAIN: Result := 'Domain error';
    ERR_RANGE: Result := 'Range error';

    ERR_UNDEFINED_VARIABLE: Result := 'Undefined variable';
    ERR_UNDEFINED_FUNCTION: Result := 'Undefined function';
    ERR_ARRAY_NOT_DIMENSIONED: Result := 'Array not dimensioned';
    ERR_ARRAY_INDEX_OUT_OF_BOUNDS: Result := 'Array index out of bounds';
    ERR_REDIMENSIONED_ARRAY: Result := 'Array already dimensioned';
    ERR_WRONG_NUMBER_OF_DIMENSIONS: Result := 'Wrong number of array dimensions';

    ERR_STRING_TOO_LONG: Result := 'String too long';
    ERR_INVALID_STRING_INDEX: Result := 'Invalid string index';
    ERR_STRING_CONVERSION: Result := 'String conversion error';

    ERR_FILE_NOT_FOUND: Result := 'File not found';
    ERR_FILE_ACCESS: Result := 'File access error';
    ERR_DEVICE_NOT_AVAILABLE: Result := 'Device not available';
    ERR_OUT_OF_DATA: Result := 'Out of DATA';
    ERR_INPUT_PAST_END: Result := 'Input past end';

    ERR_RETURN_WITHOUT_GOSUB: Result := 'RETURN without GOSUB';
    ERR_NEXT_WITHOUT_FOR: Result := 'NEXT without FOR';
    ERR_FOR_WITHOUT_NEXT: Result := 'FOR without NEXT';
    ERR_UNDEFINED_LINE: Result := 'Undefined line number';
    ERR_ILLEGAL_DIRECT: Result := 'Illegal direct command';

    ERR_ILLEGAL_FUNCTION_CALL: Result := 'Illegal function call';
    ERR_WRONG_NUMBER_OF_ARGS: Result := 'Wrong number of arguments';
    ERR_INVALID_ARGUMENT: Result := 'Invalid argument';

    else
      Result := Format('Unknown error (%d)', [ErrorCode]);
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
