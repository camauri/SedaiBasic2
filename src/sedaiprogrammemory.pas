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
unit SedaiProgramMemory;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Contnrs, TypInfo,
  SedaiLexerFSM, SedaiLexerToken, SedaiTokenList, SedaiLexerTypes,
  SedaiPackratParser, SedaiParserErrors, SedaiParserResults, SedaiAST;

type
  TProgramLine = class
  public
    LineNumber: Integer;
    SourceText: string;
    constructor Create(ALineNumber: Integer; const ASourceText: string);
  end;

  { TProgramMemory }

  TProgramMemory = class
  private
    FLines: TFPObjectList;

    // Dedicated parsing components for programs
    FProgramLexer: TLexerFSM;
    FProgramParser: TPackratParser;

    // AST caching with invalidation
    FCachedAST: TASTNode;
    FIsDirty: Boolean;

    function FindLineIndex(LineNumber: Integer): Integer;
    function MapLexerLineToBasicLine(LexerLineIndex: Integer): Integer;
    procedure SortLines;
    procedure MarkDirty;
    function RebuildAST: TASTNode;

  public
    constructor Create;
    destructor Destroy; override;

    procedure StoreLine(LineNumber: Integer; const Statement: string);
    procedure DeleteLine(LineNumber: Integer);
    function GetLineText(LineNumber: Integer): string;
    function HasLine(LineNumber: Integer): Boolean;
    function GetLineCount: Integer;

    // Main AST access with caching
    function GetOrBuildAST: TASTNode;
    function BuildProgramText: string;
    procedure Clear;

    // Listing support - return data instead of printing
    function GetAllLines: TStringList;
    function GetLinesInRange(StartLine, EndLine: Integer): TStringList;
    function GetProgramListing: string;

    procedure GetLineNumbers(List: TList);
    function GetFirstLineNumber: Integer;
    function GetLastLineNumber: Integer;
  end;

implementation

uses
  StrUtils;

{ TProgramLine }

constructor TProgramLine.Create(ALineNumber: Integer; const ASourceText: string);
begin
  inherited Create;
  LineNumber := ALineNumber;
  SourceText := ASourceText;
end;

{ TProgramMemory }

constructor TProgramMemory.Create;
begin
  inherited Create;
  FLines := TFPObjectList.Create(True); // Own objects

  // Create dedicated parsing components
  FProgramLexer := TLexerFSM.Create;
  FProgramLexer.SetHasLineNumbers(True);
  FProgramLexer.SetRequireSpacesBetweenTokens(True);
  FProgramLexer.SetCaseSensitive(False);

  FProgramParser := TPackratParser.Create;

  // Initialize cache
  FCachedAST := nil;
  FIsDirty := True;
end;

destructor TProgramMemory.Destroy;
begin
  if Assigned(FCachedAST) then
    FCachedAST.Free;

  FProgramParser.Free;
  FProgramLexer.Free;
  FLines.Free;
  inherited Destroy;
end;

procedure TProgramMemory.MarkDirty;
begin
  FIsDirty := True;

  // Free old cached AST
  if Assigned(FCachedAST) then
  begin
    FCachedAST.Free;
    FCachedAST := nil;
  end;
end;

function TProgramMemory.FindLineIndex(LineNumber: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FLines.Count - 1 do
  begin
    if TProgramLine(FLines[i]).LineNumber = LineNumber then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TProgramMemory.SortLines;
var
  i, j: Integer;
begin
  // Simple bubble sort using Exchange to avoid TFPObjectList auto-freeing objects
  for i := 0 to FLines.Count - 2 do
  begin
    for j := i + 1 to FLines.Count - 1 do
    begin
      if TProgramLine(FLines[i]).LineNumber > TProgramLine(FLines[j]).LineNumber then
      begin
        FLines.Exchange(i, j);
      end;
    end;
  end;
end;

procedure TProgramMemory.StoreLine(LineNumber: Integer; const Statement: string);
var
  Index: Integer;
  NewLine: TProgramLine;
begin
  Index := FindLineIndex(LineNumber);

  if Trim(Statement) = '' then
  begin
    // Empty statement = delete line
    if Index >= 0 then
    begin
      FLines.Delete(Index);
      MarkDirty;
    end;
  end
  else
  begin
    NewLine := TProgramLine.Create(LineNumber, Statement);

    if Index >= 0 then
    begin
      // Replace existing line
      FLines[Index] := NewLine;
    end
    else
    begin
      // Add new line
      FLines.Add(NewLine);
      SortLines;
    end;

    MarkDirty;
  end;
end;

procedure TProgramMemory.DeleteLine(LineNumber: Integer);
var
  Index: Integer;
begin
  Index := FindLineIndex(LineNumber);
  if Index >= 0 then
  begin
    FLines.Delete(Index);
    MarkDirty;
  end;
end;

function TProgramMemory.GetLineText(LineNumber: Integer): string;
var
  Index: Integer;
begin
  Index := FindLineIndex(LineNumber);
  if Index >= 0 then
    Result := TProgramLine(FLines[Index]).SourceText
  else
    Result := '';
end;

function TProgramMemory.HasLine(LineNumber: Integer): Boolean;
begin
  Result := FindLineIndex(LineNumber) >= 0;
end;

function TProgramMemory.GetLineCount: Integer;
begin
  Result := FLines.Count;
end;

function TProgramMemory.GetOrBuildAST: TASTNode;
begin
  // Return cached AST if available and valid
  if (not FIsDirty) and Assigned(FCachedAST) then
  begin
    Result := FCachedAST;
    Exit;
  end;

  // Need to rebuild
  Result := RebuildAST;
end;

function TProgramMemory.RebuildAST: TASTNode;
var
  ProgramText: string;
  TokenList: TTokenList;
  ParseResult: TParsingResult;
  ErrorLineIndex, ErrorLine, i: Integer;
  ErrorMessage: string;
begin
  Result := nil;

  if FLines.Count = 0 then
  begin
    FIsDirty := False;
    Exit;
  end;

  ProgramText := BuildProgramText;

  try
    FProgramLexer.Source := ProgramText;
    FProgramLexer.PreScanOptions;
    TokenList := FProgramLexer.ScanAllTokensFast;

    if not Assigned(TokenList) or (TokenList.Count = 0) then
    begin
      FIsDirty := True;
      raise Exception.Create('?SYNTAX ERROR');
    end;

    ParseResult := FProgramParser.Parse(TokenList);

    if not Assigned(ParseResult) then
    begin
      FIsDirty := True;
      raise Exception.Create('?SYNTAX ERROR');
    end;

    if ParseResult.Success and Assigned(ParseResult.AST) then
    begin
      // *** SUCCESS - Copy and cache the AST ***
      FCachedAST := ParseResult.AST;
      ParseResult.AST := nil; // Transfer ownership
      Result := FCachedAST;
      FIsDirty := False;
      ParseResult.Free;
    end
    else
    begin
      // *** PARSING ERROR - Build BASIC-style message ***
      if (ParseResult.Errors.Count > 0) and
         Assigned(ParseResult.Errors[0]) and
         Assigned(TParserError(ParseResult.Errors[0]).Token) then
      begin
        ErrorLineIndex := TParserError(ParseResult.Errors[0]).Token.Line;
        ErrorLine := MapLexerLineToBasicLine(ErrorLineIndex);
        if ErrorLine > 0 then
          ErrorMessage := Format('?SYNTAX ERROR IN %d', [ErrorLine])
        else
          ErrorMessage := '?SYNTAX ERROR';
      end
      else
        ErrorMessage := '?SYNTAX ERROR';

      ParseResult.Free;
      FIsDirty := True;
      raise Exception.Create(ErrorMessage);
    end;

  except
    on E: Exception do
    begin
      FIsDirty := True;
      // If the error is already in BASIC format, re-raise it
      if Pos('?', E.Message) = 1 then
        raise
      else
        raise Exception.Create('?SYNTAX ERROR - ' + E.Message);
    end;
  end;
end;

function TProgramMemory.BuildProgramText: string;
var
  i: Integer;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    for i := 0 to FLines.Count - 1 do
    begin
      Lines.Add(IntToStr(TProgramLine(FLines[i]).LineNumber) + ' ' + TProgramLine(FLines[i]).SourceText);
    end;
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TProgramMemory.Clear;
begin
  FLines.Clear;
  MarkDirty;
end;

function TProgramMemory.GetAllLines: TStringList;
var
  i: Integer;
  LineNum: Integer;
  LineText: string;
begin
  Result := TStringList.Create;

  for i := 0 to FLines.Count - 1 do
  begin
    LineNum := TProgramLine(FLines[i]).LineNumber;
    LineText := TProgramLine(FLines[i]).SourceText;
    Result.Add(IntToStr(LineNum) + ' ' + LineText);
  end;
end;

function TProgramMemory.GetLinesInRange(StartLine, EndLine: Integer): TStringList;
var
  i: Integer;
  LineNum: Integer;
  LineText: string;
begin
  Result := TStringList.Create;

  for i := 0 to FLines.Count - 1 do
  begin
    LineNum := TProgramLine(FLines[i]).LineNumber;
    if (LineNum >= StartLine) and (LineNum <= EndLine) then
    begin
      LineText := TProgramLine(FLines[i]).SourceText;
      Result.Add(IntToStr(LineNum) + ' ' + LineText);
    end;
  end;
end;

function TProgramMemory.GetProgramListing: string;
var
  AllLines: TStringList;
begin
  AllLines := GetAllLines;
  try
    Result := AllLines.Text;
  finally
    AllLines.Free;
  end;
end;

procedure TProgramMemory.GetLineNumbers(List: TList);
var
  i: Integer;
begin
  List.Clear;
  for i := 0 to FLines.Count - 1 do
    List.Add(Pointer(TProgramLine(FLines[i]).LineNumber));
end;

function TProgramMemory.GetFirstLineNumber: Integer;
begin
  if FLines.Count > 0 then
    Result := TProgramLine(FLines[0]).LineNumber
  else
    Result := -1;
end;

function TProgramMemory.GetLastLineNumber: Integer;
begin
  if FLines.Count > 0 then
    Result := TProgramLine(FLines[FLines.Count - 1]).LineNumber
  else
    Result := -1;
end;

function TProgramMemory.MapLexerLineToBasicLine(LexerLineIndex: Integer): Integer;
begin
  // Lexer counts from 1, array from 0
  if (LexerLineIndex >= 1) and (LexerLineIndex <= FLines.Count) then
    Result := TProgramLine(FLines[LexerLineIndex - 1]).LineNumber
  else
    Result := 0; // Invalid index
end;

end.
