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
unit SedaiCommandRouter;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, SedaiCommandTypes, SedaiBasicKeywords;

type
  TCommandRouter = class
  private
    FSystemCommands: TStringList;
      
    function IsNumericLineStart(const Input: string): Boolean;
    function ExtractLineNumber(const Input: string; out LineNum: Integer; out Statement: string): Boolean;

    // List of recognized system commands
    function IsSystemCommand(const Statement: string): Boolean; overload;

  public
    constructor Create;
    destructor Destroy; override;

    function ParseInput(const Input: string): TInputParseResult;
    function IsValidBasicLine(const Input: string): Boolean;
  end;

implementation

uses
  StrUtils;

{ TCommandRouter }

constructor TCommandRouter.Create;
begin
  inherited Create;
  FSystemCommands := TStringList.Create;
  FSystemCommands.CaseSensitive := False;
  // Program execution
  FSystemCommands.Add(kRUN);
  // Program listing
  FSystemCommands.Add(kLIST);
  FSystemCommands.Add(kGLIST);
  // Program memory management
  FSystemCommands.Add(kNEW);
  // Program file I/O
  FSystemCommands.Add(kLOAD);
  FSystemCommands.Add(kDLOAD);
  FSystemCommands.Add(kSAVE);
  FSystemCommands.Add(kDSAVE);
  // Directory listing
  FSystemCommands.Add(kCATALOG);
  FSystemCommands.Add(kDIR);
  FSystemCommands.Add(kDIRECTORY);
  // File operations
  FSystemCommands.Add(kCOPY);
  FSystemCommands.Add(kSCRATCH);
  FSystemCommands.Add(kRENAME);
  // File verification
  FSystemCommands.Add(kVERIFY);
  FSystemCommands.Add(kDVERIFY);
  // Bytecode operations
  FSystemCommands.Add(kBLOAD);
  FSystemCommands.Add(kBSAVE);
  FSystemCommands.Add(kBOOT);
end;

destructor TCommandRouter.Destroy;
begin
  FSystemCommands.Free;
  inherited Destroy;
end;

function TCommandRouter.ParseInput(const Input: string): TInputParseResult;
var
  TrimmedInput: string;
  LineNum: Integer;
  Statement: string;
  PipePos: Integer;
begin
  // Initialize result
  Result.CommandType := ctImmediate;
  Result.LineNumber := 0;
  Result.Statement := '';
  Result.IsValid := False;
  Result.ErrorMessage := '';
  Result.PipeModifier := pmNone;

  TrimmedInput := Trim(Input);

  // Empty input
  if TrimmedInput = '' then
  begin
    Result.ErrorMessage := 'Empty input';
    Exit;
  end;

  // Check for pipe to MORE (command | MORE)
  PipePos := Pos('|', TrimmedInput);
  if PipePos > 0 then
  begin
    // Extract command after pipe
    Statement := UpperCase(Trim(Copy(TrimmedInput, PipePos + 1, MaxInt)));

    // Pipe must be followed by MORE command
    if Statement = 'MORE' then
      Result.PipeModifier := pmMore
    else if Statement = '' then
    begin
      Result.ErrorMessage := '?SYNTAX ERROR - | requires command (e.g. | MORE)';
      Exit;
    end
    else
    begin
      Result.ErrorMessage := '?UNKNOWN PIPE COMMAND: ' + Statement;
      Exit;
    end;

    TrimmedInput := Trim(Copy(TrimmedInput, 1, PipePos - 1));

    // Handle empty command before pipe
    if TrimmedInput = '' then
    begin
      Result.ErrorMessage := '?SYNTAX ERROR - empty command before |';
      Exit;
    end;
  end;

  // Check if starts with line number
  if IsNumericLineStart(TrimmedInput) then
  begin
    if ExtractLineNumber(TrimmedInput, LineNum, Statement) then
    begin
      Result.CommandType := ctProgramLine;
      Result.LineNumber := LineNum;
      Result.Statement := Statement;
      Result.IsValid := True;
    end
    else
    begin
      Result.ErrorMessage := 'Invalid line number format';
      Exit;
    end;
  end
  else
  begin
    // Check if it's a system command
    if IsSystemCommand(TrimmedInput) then
    begin
      Result.CommandType := ctSystemCommand;
      Result.Statement := TrimmedInput;
    end
    else
    begin
      // Otherwise, it's an immediate command
      Result.CommandType := ctImmediate;
      Result.Statement := TrimmedInput;
    end;
    Result.IsValid := True;
  end;
end;

function TCommandRouter.IsNumericLineStart(const Input: string): Boolean;
var
  i: Integer;
  FirstWord: string;
  SpacePos: Integer;
begin
  Result := False;

  SpacePos := Pos(' ', Input);
  if SpacePos > 0 then
    FirstWord := Copy(Input, 1, SpacePos - 1)
  else
    FirstWord := Input;

  // Check if first word is purely numeric
  if Length(FirstWord) = 0 then
    Exit;

  for i := 1 to Length(FirstWord) do
  begin
    if not (FirstWord[i] in ['0'..'9']) then
      Exit;
  end;

  Result := True;
end;

function TCommandRouter.ExtractLineNumber(const Input: string; out LineNum: Integer; out Statement: string): Boolean;
var
  SpacePos: Integer;
  LineNumStr: string;
begin
  Result := False;
  LineNum := 0;
  Statement := '';

  SpacePos := Pos(' ', Input);

  if SpacePos > 0 then
  begin
    LineNumStr := Copy(Input, 1, SpacePos - 1);
    Statement := Trim(Copy(Input, SpacePos + 1, Length(Input)));
  end
  else
  begin
    LineNumStr := Input;
    Statement := '';
  end;

  // Convert line number
  if TryStrToInt(LineNumStr, LineNum) then
  begin
    if (LineNum >= 0) and (LineNum <= 65535) then
      Result := True;
  end;
end;

function TCommandRouter.IsSystemCommand(const Statement: string): Boolean;
var
  UpperStatement: string;
  FirstWord: string;
  SpacePos: Integer;
begin
  UpperStatement := UpperCase(Statement);
  SpacePos := Pos(' ', UpperStatement);
  if SpacePos > 0 then FirstWord := Copy(UpperStatement, 1, SpacePos - 1) else FirstWord := UpperStatement;
  Result := FSystemCommands.IndexOf(FirstWord) >= 0;
end;

function TCommandRouter.IsValidBasicLine(const Input: string): Boolean;
var
  ParseResult: TInputParseResult;
begin
  ParseResult := ParseInput(Input);
  Result := ParseResult.IsValid;
end;

end.
