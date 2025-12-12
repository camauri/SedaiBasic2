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
unit SedaiExceptionHelpers;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  SysUtils, SedaiExecutorErrors;

// ===== HELPER FUNCTIONS FOR UNIFORM EXCEPTION HANDLING =====

// Type errors
procedure RaiseTypeMismatch(const Context: string = ''); inline;
procedure RaiseTypeMismatchInFunction(const FuncName: string); inline;

// Syntax errors
procedure RaiseSyntaxError(LineNumber: Integer = 0); inline;
procedure RaiseSyntaxErrorWithMessage(const Message: string; LineNumber: Integer = 0); inline;

// Array errors
procedure RaiseArrayDimensionError(Expected, Got: Integer); inline;
procedure RaiseArrayIndexOutOfBounds(Index, MaxIndex: Integer); inline;
procedure RaiseArrayNotDimensioned(const ArrayName: string); inline;

// Variable errors
procedure RaiseUndefinedVariable(const VarName: string); inline;
procedure RaiseUndefinedFunction(const FuncName: string); inline;

// Math errors
procedure RaiseDivisionByZero; inline;
procedure RaiseMathError(const Operation: string); inline;

// I/O errors
procedure RaiseFileNotFound(const FileName: string); inline;
procedure RaiseFileError(const FileName, Operation: string); inline;

// General errors
procedure RaiseRuntimeError(const Message: string; ErrorCode: Integer = 0); inline;
procedure RaiseInvalidOperation(const Operation: string); inline;

implementation

// ===== TYPE ERRORS =====

procedure RaiseTypeMismatch(const Context: string);
begin
  if Context <> '' then
    raise TExecutorTypeException.CreateWithCode(
      '?TYPE MISMATCH ERROR in ' + Context,
      ERR_TYPE_MISMATCH
    )
  else
    raise TExecutorTypeException.CreateWithCode(
      '?TYPE MISMATCH ERROR',
      ERR_TYPE_MISMATCH
    );
end;

procedure RaiseTypeMismatchInFunction(const FuncName: string);
begin
  raise TExecutorTypeException.CreateWithCode(
    Format('?TYPE MISMATCH ERROR in function %s', [FuncName]),
    ERR_TYPE_MISMATCH
  );
end;

// ===== SYNTAX ERRORS =====

procedure RaiseSyntaxError(LineNumber: Integer);
begin
  if LineNumber > 0 then
    raise TExecutorSyntaxException.CreateWithCode(
      Format('?SYNTAX ERROR IN %d', [LineNumber]),
      ERR_SYNTAX, LineNumber
    )
  else
    raise TExecutorSyntaxException.CreateWithCode(
      '?SYNTAX ERROR',
      ERR_SYNTAX
    );
end;

procedure RaiseSyntaxErrorWithMessage(const Message: string; LineNumber: Integer);
begin
  if LineNumber > 0 then
    raise TExecutorSyntaxException.CreateWithCode(
      Format('?SYNTAX ERROR: %s IN %d', [Message, LineNumber]),
      ERR_SYNTAX, LineNumber
    )
  else
    raise TExecutorSyntaxException.CreateWithCode(
      '?SYNTAX ERROR: ' + Message,
      ERR_SYNTAX
    );
end;

// ===== ARRAY ERRORS =====

procedure RaiseArrayDimensionError(Expected, Got: Integer);
begin
  raise TExecutorArrayException.CreateWithCode(
    Format('Array dimension mismatch: expected %d, got %d', [Expected, Got]),
    ERR_WRONG_NUMBER_OF_DIMENSIONS
  );
end;

procedure RaiseArrayIndexOutOfBounds(Index, MaxIndex: Integer);
begin
  raise TExecutorArrayIndexException.CreateWithCode(
    Format('Array index %d out of bounds (0..%d)', [Index, MaxIndex]),
    ERR_ARRAY_INDEX_OUT_OF_BOUNDS
  );
end;

procedure RaiseArrayNotDimensioned(const ArrayName: string);
begin
  raise TExecutorArrayException.CreateWithCode(
    Format('Array %s not dimensioned', [ArrayName]),
    ERR_ARRAY_NOT_DIMENSIONED
  );
end;

// ===== VARIABLE ERRORS =====

procedure RaiseUndefinedVariable(const VarName: string);
begin
  raise TExecutorUndefinedVariableException.CreateWithCode(
    Format('Undefined variable: %s', [VarName]),
    ERR_UNDEFINED_VARIABLE
  );
end;

procedure RaiseUndefinedFunction(const FuncName: string);
begin
  raise TExecutorUndefinedFunctionException.CreateWithCode(
    Format('Undefined function: %s', [FuncName]),
    ERR_UNDEFINED_FUNCTION
  );
end;

// ===== MATH ERRORS =====

procedure RaiseDivisionByZero;
begin
  raise TExecutorDivisionByZeroException.CreateWithCode(
    '?DIVISION BY ZERO ERROR',
    ERR_DIVISION_BY_ZERO
  );
end;

procedure RaiseMathError(const Operation: string);
begin
  raise TExecutorMathException.CreateWithCode(
    Format('Math error in operation: %s', [Operation]),
    ERR_ILLEGAL_FUNCTION_CALL
  );
end;

// ===== I/O ERRORS =====

procedure RaiseFileNotFound(const FileName: string);
begin
  raise TExecutorIOException.CreateWithCode(
    Format('?FILE NOT FOUND: %s', [FileName]),
    ERR_FILE_NOT_FOUND
  );
end;

procedure RaiseFileError(const FileName, Operation: string);
begin
  raise TExecutorIOException.CreateWithCode(
    Format('File error during %s: %s', [Operation, FileName]),
    ERR_FILE_ACCESS
  );
end;

// ===== GENERAL ERRORS =====

procedure RaiseRuntimeError(const Message: string; ErrorCode: Integer);
begin
  if ErrorCode <> 0 then
    raise TExecutorRuntimeException.CreateWithCode(Message, ErrorCode)
  else
    raise TExecutorRuntimeException.Create(Message);
end;

procedure RaiseInvalidOperation(const Operation: string);
begin
  raise TExecutorRuntimeException.CreateWithCode(
    Format('Invalid operation: %s', [Operation]),
    ERR_ILLEGAL_FUNCTION_CALL
  );
end;

end.
