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
unit SedaiParserErrors;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, fgl, contnrs,
  SedaiLexerTypes, SedaiLexerToken;

type
  { TParserError - Parse error information }
  TParserError = class
  private
    FMessage: string;
    FToken: TLexerToken;
    FLine: Integer;
    FColumn: Integer;
    FPosition: Integer;
    FErrorType: string;
    FSeverity: Integer; // 0=Info, 1=Warning, 2=Error, 3=Fatal

  public
    constructor Create(const AMessage: string; AToken: TLexerToken); overload;
    constructor Create(const AMessage: string; ALine, AColumn, APosition: Integer); overload;
    constructor CreateWithSeverity(const AMessage: string; AToken: TLexerToken; ASeverity: Integer); overload;

    function ToString: string; override;
    function GetLocationString: string;
    function GetSeverityString: string;
    function IsWarning: Boolean;
    function IsError: Boolean;
    function IsFatal: Boolean;

    property Message: string read FMessage write FMessage;
    property Token: TLexerToken read FToken write FToken;
    property Line: Integer read FLine write FLine;
    property Column: Integer read FColumn write FColumn;
    property Position: Integer read FPosition write FPosition;
    property ErrorType: string read FErrorType write FErrorType;
    property Severity: Integer read FSeverity write FSeverity;
  end;

  { TParserErrorList - Collection of parser errors }
  TParserErrorList = class(TFPObjectList)
  private
    FMaxErrors: Integer;
    FWarningCount: Integer;
    FErrorCount: Integer;
    FFatalCount: Integer;

    procedure UpdateCounts;

  public
    constructor Create(AMaxErrors: Integer = 100);

    function AddError(Error: TParserError): Integer; overload;
    function AddError(const Message: string; Token: TLexerToken): Integer; overload;
    function AddError(const Message: string; Line, Column, Position: Integer): Integer; overload;
    function AddWarning(const Message: string; Token: TLexerToken): Integer;
    function AddFatal(const Message: string; Token: TLexerToken): Integer;

    function GetError(Index: Integer): TParserError;
    function GetErrorMessages: TStringList;
    function GetWarnings: TParserErrorList;
    function GetErrors: TParserErrorList;
    function GetFatals: TParserErrorList;

    function HasErrors: Boolean;
    function HasWarnings: Boolean;
    function HasFatals: Boolean;
    function CanContinue: Boolean; // True if no fatal errors

    procedure SortByPosition;
    procedure FilterBySeverity(MinSeverity: Integer);
    function GetReport: string;

    property MaxErrors: Integer read FMaxErrors write FMaxErrors;
    property WarningCount: Integer read FWarningCount;
    property ErrorCount: Integer read FErrorCount;
    property FatalCount: Integer read FFatalCount;
    property Errors[Index: Integer]: TParserError read GetError; default;
  end;

  // === EXCEPTION CLASSES ===
  EParserException = class(Exception);
  EParserSyntaxException = class(EParserException);
  EParserInternalException = class(EParserException);
  EParserFatalException = class(EParserException);

// === UTILITY FUNCTIONS ===
function CreateSyntaxError(const Message: string; Token: TLexerToken): TParserError;
function CreateSemanticError(const Message: string; Token: TLexerToken): TParserError;
function CreateWarning(const Message: string; Token: TLexerToken): TParserError;
function CompareErrorsByPosition(Item1, Item2: Pointer): Integer;

implementation

uses
  Math;

const
  SEVERITY_INFO = 0;
  SEVERITY_WARNING = 1;
  SEVERITY_ERROR = 2;
  SEVERITY_FATAL = 3;

{ TParserError }

constructor TParserError.Create(const AMessage: string; AToken: TLexerToken);
begin
  inherited Create;
  FMessage := AMessage;
  FToken := AToken;
  if Assigned(AToken) then
  begin
    FLine := AToken.Line;
    FColumn := AToken.Column;
    FPosition := AToken.Position;
  end
  else
  begin
    FLine := 0;
    FColumn := 0;
    FPosition := 0;
  end;
  FErrorType := 'Syntax Error';
  FSeverity := SEVERITY_ERROR;
end;

constructor TParserError.Create(const AMessage: string; ALine, AColumn, APosition: Integer);
begin
  inherited Create;
  FMessage := AMessage;
  FToken := nil;
  FLine := ALine;
  FColumn := AColumn;
  FPosition := APosition;
  FErrorType := 'Syntax Error';
  FSeverity := SEVERITY_ERROR;
end;

constructor TParserError.CreateWithSeverity(const AMessage: string; AToken: TLexerToken; ASeverity: Integer);
begin
  Create(AMessage, AToken);
  FSeverity := ASeverity;

  case ASeverity of
    SEVERITY_INFO: FErrorType := 'Info';
    SEVERITY_WARNING: FErrorType := 'Warning';
    SEVERITY_ERROR: FErrorType := 'Error';
    SEVERITY_FATAL: FErrorType := 'Fatal Error';
  end;
end;

function TParserError.ToString: string;
begin
  Result := Format('%s at %s: %s', [FErrorType, GetLocationString, FMessage]);
end;

function TParserError.GetLocationString: string;
begin
  if Assigned(FToken) and (FToken.SourceFile <> '') then
    Result := Format('%s:%d:%d', [FToken.SourceFile, FLine, FColumn])
  else
    Result := Format('%d:%d', [FLine, FColumn]);
end;

function TParserError.GetSeverityString: string;
begin
  case FSeverity of
    SEVERITY_INFO: Result := 'INFO';
    SEVERITY_WARNING: Result := 'WARNING';
    SEVERITY_ERROR: Result := 'ERROR';
    SEVERITY_FATAL: Result := 'FATAL';
    else
      Result := 'UNKNOWN';
  end;
end;

function TParserError.IsWarning: Boolean;
begin
  Result := FSeverity = SEVERITY_WARNING;
end;

function TParserError.IsError: Boolean;
begin
  Result := FSeverity = SEVERITY_ERROR;
end;

function TParserError.IsFatal: Boolean;
begin
  Result := FSeverity = SEVERITY_FATAL;
end;

{ TParserErrorList }

constructor TParserErrorList.Create(AMaxErrors: Integer);
begin
  inherited Create(True); // Own objects
  FMaxErrors := AMaxErrors;
  FWarningCount := 0;
  FErrorCount := 0;
  FFatalCount := 0;
end;

procedure TParserErrorList.UpdateCounts;
var
  i: Integer;
  Error: TParserError;
begin
  FWarningCount := 0;
  FErrorCount := 0;
  FFatalCount := 0;

  for i := 0 to Count - 1 do
  begin
    Error := GetError(i);
    case Error.Severity of
      SEVERITY_WARNING: Inc(FWarningCount);
      SEVERITY_ERROR: Inc(FErrorCount);
      SEVERITY_FATAL: Inc(FFatalCount);
    end;
  end;
end;

function TParserErrorList.AddError(Error: TParserError): Integer;
begin
  if Count >= FMaxErrors then
  begin
    // Don't add more errors if we've reached the limit
    Error.Free;
    Result := -1;
    Exit;
  end;

  Result := Add(Error);
  UpdateCounts;
end;

function TParserErrorList.AddError(const Message: string; Token: TLexerToken): Integer;
var
  Error: TParserError;
begin
  Error := TParserError.Create(Message, Token);
  Result := AddError(Error);
end;

function TParserErrorList.AddError(const Message: string; Line, Column, Position: Integer): Integer;
var
  Error: TParserError;
begin
  Error := TParserError.Create(Message, Line, Column, Position);
  Result := AddError(Error);
end;

function TParserErrorList.AddWarning(const Message: string; Token: TLexerToken): Integer;
var
  Error: TParserError;
begin
  Error := TParserError.CreateWithSeverity(Message, Token, SEVERITY_WARNING);
  Result := AddError(Error);
end;

function TParserErrorList.AddFatal(const Message: string; Token: TLexerToken): Integer;
var
  Error: TParserError;
begin
  Error := TParserError.CreateWithSeverity(Message, Token, SEVERITY_FATAL);
  Result := AddError(Error);
end;

function TParserErrorList.GetError(Index: Integer): TParserError;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TParserError(Items[Index])
  else
    Result := nil;
end;

function TParserErrorList.GetErrorMessages: TStringList;
var
  i: Integer;
  Error: TParserError;
begin
  Result := TStringList.Create;
  try
    for i := 0 to Count - 1 do
    begin
      Error := GetError(i);
      if Assigned(Error) then
        Result.Add(Error.ToString);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TParserErrorList.GetWarnings: TParserErrorList;
var
  i: Integer;
  Error: TParserError;
begin
  Result := TParserErrorList.Create(Count);
  try
    for i := 0 to Count - 1 do
    begin
      Error := GetError(i);
      if Assigned(Error) and Error.IsWarning then
      begin
        // Create a new TParserError instead of cloning
        Result.AddError(TParserError.CreateWithSeverity(Error.Message, Error.Token, Error.Severity));
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TParserErrorList.GetErrors: TParserErrorList;
var
  i: Integer;
  Error: TParserError;
begin
  Result := TParserErrorList.Create(Count);
  try
    for i := 0 to Count - 1 do
    begin
      Error := GetError(i);
      if Assigned(Error) and Error.IsError then
      begin
        // Create a new TParserError instead of cloning
        Result.AddError(TParserError.CreateWithSeverity(Error.Message, Error.Token, Error.Severity));
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TParserErrorList.GetFatals: TParserErrorList;
var
  i: Integer;
  Error: TParserError;
begin
  Result := TParserErrorList.Create(Count);
  try
    for i := 0 to Count - 1 do
    begin
      Error := GetError(i);
      if Assigned(Error) and Error.IsFatal then
      begin
        // Create a new TParserError instead of cloning
        Result.AddError(TParserError.CreateWithSeverity(Error.Message, Error.Token, Error.Severity));
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TParserErrorList.HasErrors: Boolean;
begin
  Result := FErrorCount > 0;
end;

function TParserErrorList.HasWarnings: Boolean;
begin
  Result := FWarningCount > 0;
end;

function TParserErrorList.HasFatals: Boolean;
begin
  Result := FFatalCount > 0;
end;

function TParserErrorList.CanContinue: Boolean;
begin
  Result := FFatalCount = 0;
end;

procedure TParserErrorList.SortByPosition;
begin
  Sort(@CompareErrorsByPosition);
end;

procedure TParserErrorList.FilterBySeverity(MinSeverity: Integer);
var
  i: Integer;
  Error: TParserError;
begin
  for i := Count - 1 downto 0 do
  begin
    Error := GetError(i);
    if Assigned(Error) and (Error.Severity < MinSeverity) then
      Delete(i);
  end;
  UpdateCounts;
end;

function TParserErrorList.GetReport: string;
var
  Report: TStringList;
  i: Integer;
  Error: TParserError;
begin
  Report := TStringList.Create;
  try
    Report.Add('=== PARSER ERROR REPORT ===');
    Report.Add(Format('Total: %d errors (%d warnings, %d errors, %d fatal)',
      [Count, FWarningCount, FErrorCount, FFatalCount]));
    Report.Add('');

    if Count = 0 then
    begin
      Report.Add('No errors found.');
    end
    else
    begin
      // Sort by position first
      SortByPosition;

      for i := 0 to Count - 1 do
      begin
        Error := GetError(i);
        if Assigned(Error) then
          Report.Add(Format('[%s] %s', [Error.GetSeverityString, Error.ToString]));
      end;
    end;

    Report.Add('=== END REPORT ===');
    Result := Report.Text;

  finally
    Report.Free;
  end;
end;

// === UTILITY FUNCTIONS ===

function CreateSyntaxError(const Message: string; Token: TLexerToken): TParserError;
begin
  Result := TParserError.Create(Message, Token);
  Result.ErrorType := 'Syntax Error';
  Result.Severity := SEVERITY_ERROR;
end;

function CreateSemanticError(const Message: string; Token: TLexerToken): TParserError;
begin
  Result := TParserError.Create(Message, Token);
  Result.ErrorType := 'Semantic Error';
  Result.Severity := SEVERITY_ERROR;
end;

function CreateWarning(const Message: string; Token: TLexerToken): TParserError;
begin
  Result := TParserError.CreateWithSeverity(Message, Token, SEVERITY_WARNING);
  Result.ErrorType := 'Warning';
end;

function CompareErrorsByPosition(Item1, Item2: Pointer): Integer;
var
  Error1, Error2: TParserError;
begin
  Error1 := TParserError(Item1);
  Error2 := TParserError(Item2);

  Result := Error1.Position - Error2.Position;
  if Result = 0 then
    Result := Error1.Line - Error2.Line;
  if Result = 0 then
    Result := Error1.Column - Error2.Column;
end;

end.

