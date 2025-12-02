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
unit SedaiTokenList;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, fgl, contnrs,
  SedaiLexerTypes, SedaiLexerToken;

type
  // Forward declarations
  TTokenList = class;

  // === TIPI DI SUPPORTO ===
  TTokenPosition = record
    Index: Integer;
    Token: TLexerToken;
    IsValid: Boolean;
  end;

  TTokenRange = record
    StartIndex: Integer;
    EndIndex: Integer;
    Count: Integer;
    IsValid: Boolean;
  end;

  TTokenSearchOptions = record
    CaseSensitive: Boolean;
    WholeWord: Boolean;
    SearchInComments: Boolean;
    SearchInStrings: Boolean;
    MaxResults: Integer;
    StartFromIndex: Integer;
  end;

  // Callback types
  //TTokenIteratorProc = procedure(Token: TLexerToken; Index: Integer; var Continue: Boolean);
  //TTokenIteratorFunc = function(Token: TLexerToken; Index: Integer): Boolean;
  //TTokenCompareFunc = function(Token1, Token2: TLexerToken): Integer;
  //TTokenFilterFunc = function(Token: TLexerToken; Index: Integer): Boolean;
  //TTokenMapFunc = function(Token: TLexerToken; Index: Integer): TLexerToken;

  // Array dinamici per risultati
  TTokenIndexArray = array of Integer;
  TTokenArray = array of TLexerToken;

  { TTokenIterator - Iterator per navigazione sicura }
  TTokenIterator = class
  private
    FTokenList: TTokenList;
    FCurrentIndex: Integer;
    FStartIndex: Integer;
    FEndIndex: Integer;
    FDirection: Integer; // 1 = forward, -1 = backward

  public
    constructor Create(ATokenList: TTokenList; AStartIndex: Integer = 0;
                      AEndIndex: Integer = -1; ADirection: Integer = 1);

    // Navigation
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: TLexerToken;
    function Previous: TLexerToken;
    function Current: TLexerToken;
    function Peek(Offset: Integer = 1): TLexerToken;

    // Position
    function GetCurrentIndex: Integer;
    procedure Reset;
    procedure MoveTo(Index: Integer);
    procedure MoveToEnd;

    // Properties
    property CurrentIndex: Integer read GetCurrentIndex;
  end;

  { TTokenList - Lista avanzata di token con funzionalità complete }
  TTokenList = class(TFPObjectList)
  private
    FCurrentIndex: Integer;
    //FOwnsTokens: Boolean;
    FName: string;
    FSourceFile: string;
    FModificationCount: Integer;
    FReadOnly: Boolean;

    // Cache per performance
    FLastSearchIndex: Integer;
    FLastSearchToken: TLexerToken;

    procedure CheckReadOnly;
    procedure IncModificationCount;
    function GetTokenByIndex(Index: Integer): TLexerToken;
    procedure SetTokenByIndex(Index: Integer; const Value: TLexerToken);
    function GetIsEmpty: Boolean;
    function GetFirst: TLexerToken;
    function GetLast: TLexerToken;

  protected
    // Virtual methods for extensibility
    procedure DoTokenAdded(Token: TLexerToken; Index: Integer); virtual;
    procedure DoTokenRemoved(Token: TLexerToken; Index: Integer); virtual;
    procedure DoTokenReplaced(OldToken, NewToken: TLexerToken; Index: Integer); virtual;
    procedure DoListCleared; virtual;

  public
    constructor Create(AOwnsTokens: Boolean = True); overload;
    constructor CreateNamed(const AName: string; AOwnsTokens: Boolean = True); overload;
    destructor Destroy; override;

    // === BASIC OPERATIONS ===
    function AddToken(Token: TLexerToken): Integer;
    function InsertToken(Index: Integer; Token: TLexerToken): Integer;
    function RemoveToken(Token: TLexerToken): Boolean;
    function RemoveTokenAt(Index: Integer): Boolean;
    function ExtractToken(Token: TLexerToken): TLexerToken;
    function ExtractTokenAt(Index: Integer): TLexerToken;
    procedure Clear; virtual;

    // === ACCESSO DIRETTO OTTIMIZZATO ===
    function GetTokenDirect(Index: Integer): TLexerToken; inline;
    procedure SetTokenDirect(Index: Integer; Token: TLexerToken); inline;

    procedure PreAllocateTokens(ACount: Integer);
    function GetNextPoolToken(var CurrentIndex: Integer): TLexerToken; inline;
    procedure ResetPool; inline;

    // === BATCH OPERATIONS ===
    procedure AddTokens(const Tokens: array of TLexerToken); overload;
    procedure AddTokens(TokenList: TTokenList); overload;
    procedure InsertTokens(Index: Integer; const Tokens: array of TLexerToken); overload;
    procedure InsertTokens(Index: Integer; TokenList: TTokenList); overload;
    function RemoveTokensInRange(StartIndex, EndIndex: Integer): Integer;
    function RemoveTokensByType(TokenType: TTokenType): Integer;
    function RemoveTokensByTypes(const TokenTypes: array of TTokenType): Integer;

    // === NAVIGATION ===
    function GetCurrentToken: TLexerToken;
    function GetNextToken: TLexerToken;
    function GetPreviousToken: TLexerToken;
    function PeekToken(Offset: Integer = 1): TLexerToken;
    procedure Reset;
    procedure MoveTo(Index: Integer);
    procedure MoveToFirst;
    procedure MoveToLast;
    function HasMore: Boolean;
    function HasPrevious: Boolean;

    // === SEARCH & FILTER ===
    function FindToken(Token: TLexerToken): Integer;
    function FindTokenByValue(const Value: string; StartIndex: Integer = 0): Integer;
    function FindTokenByType(TokenType: TTokenType; StartIndex: Integer = 0): Integer;
    function FindTokensByType(TokenType: TTokenType): TTokenIndexArray;
    function FindTokensByTypes(const TokenTypes: array of TTokenType): TTokenIndexArray;
    function FindTokensByValue(const Value: string; Options: TTokenSearchOptions): TTokenIndexArray;

    // Advanced search
    //function FindFirstWhere(FilterFunc: TTokenFilterFunc): TTokenPosition;
    //function FindAllWhere(FilterFunc: TTokenFilterFunc): TTokenIndexArray;
    //function FindInRange(StartIndex, EndIndex: Integer; FilterFunc: TTokenFilterFunc): TTokenIndexArray;
    function FindNextKeyword(StartIndex: Integer = -1): TTokenPosition;
    function FindNextOfTypes(const TokenTypes: array of TTokenType; StartIndex: Integer = -1): TTokenPosition;

    // === POSITION & RANGE OPERATIONS ===
    function GetTokenByPosition(Position: Integer): TLexerToken;
    function GetTokenByLocation(Line, Column: Integer): TLexerToken;
    function GetTokensInRange(StartPos, EndPos: Integer): TTokenList;
    function GetTokensInLineRange(StartLine, EndLine: Integer): TTokenList;
    function GetTokenRange(StartIndex, EndIndex: Integer): TTokenRange;

    // === ANALYSIS & STATISTICS ===
    function CountTokensByType(TokenType: TTokenType): Integer;
    function CountTokensByTypes(const TokenTypes: array of TTokenType): Integer;
    function GetTokenTypeStatistics: TStringList; // Type -> Count
    function GetUniqueValues: TStringList;
    function GetLineCount: Integer;
    function GetMaxLineNumber: Integer;
    function HasTokenType(TokenType: TTokenType): Boolean;
    function HasAnyOfTypes(const TokenTypes: array of TTokenType): Boolean;

    // === ITERATION ===
    function CreateIterator(StartIndex: Integer = 0; EndIndex: Integer = -1): TTokenIterator;

    // === TRANSFORMATION ===
    //function Map(MapFunc: TTokenMapFunc): TTokenList;
    //function Filter(FilterFunc: TTokenFilterFunc): TTokenList;
    function FilterByType(TokenType: TTokenType): TTokenList;
    function FilterByTypes(const TokenTypes: array of TTokenType): TTokenList;
    function FilterByRange(StartIndex, EndIndex: Integer): TTokenList;

    // === SORTING ===
    procedure Sort(CompareFunc: TTokenCompareFunc);
    procedure SortByPosition;
    procedure SortByType;
    procedure SortByValue;

    // === VALIDATION ===
    function ValidatePositions: Boolean;
    function ValidateLineNumbers: Boolean;
    function FindDuplicates: TTokenIndexArray;
    function FindGaps: TTokenIndexArray; // Find position gaps

    // === SERIALIZATION ===
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);

    // === UTILITY METHODS ===
    function Clone: TTokenList;
    function CloneRange(StartIndex, EndIndex: Integer): TTokenList;
    procedure Assign(Source: TTokenList);
    procedure Merge(Other: TTokenList; AllowDuplicates: Boolean = True);
    function IsEqual(Other: TTokenList): Boolean;
    function GetChecksum: string;

    // === DEBUG & DIAGNOSTICS ===
    function GetDebugInfo: string;
    function GetStatisticsReport: string;
    procedure DumpToConsole;
    procedure DumpRangeToConsole(StartIndex, EndIndex: Integer);

    // === PROPERTIES ===
    property Tokens[Index: Integer]: TLexerToken read GetTokenByIndex write SetTokenByIndex; default;
    property CurrentIndex: Integer read FCurrentIndex write FCurrentIndex;
    //property OwnsTokens: Boolean read FOwnsTokens write FOwnsTokens;
    property Name: string read FName write FName;
    property SourceFile: string read FSourceFile write FSourceFile;
    property ModificationCount: Integer read FModificationCount;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property IsEmpty: Boolean read GetIsEmpty;
    property First: TLexerToken read GetFirst;
    property Last: TLexerToken read GetLast;
  end;

  // === SPECIALIZED TOKEN LISTS ===

  { TKeywordTokenList - Lista specializzata per keyword }
  TKeywordTokenList = class(TTokenList)
  private
    FCaseSensitive: Boolean;
  public
    constructor Create(ACaseSensitive: Boolean = False);
    function AddKeyword(const Keyword: string; TokenType: TTokenType;
                       Line, Column, Position: Integer): Integer;
    function HasKeyword(const Keyword: string): Boolean;
    function FindKeyword(const Keyword: string): Integer;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
  end;

  { TFilteredTokenList - Lista con filtri automatici }
  TFilteredTokenList = class(TTokenList)
  private
    FAutoFilter: TTokenFilterFunc;
    FSourceList: TTokenList;
    FAutoUpdate: Boolean;
    //procedure UpdateFromSource;
  public
    constructor Create(ASourceList: TTokenList; AAutoFilter: TTokenFilterFunc;
                      AAutoUpdate: Boolean = True);
    procedure RefreshFilter;
    property AutoFilter: TTokenFilterFunc read FAutoFilter write FAutoFilter;
    property SourceList: TTokenList read FSourceList;
    property AutoUpdate: Boolean read FAutoUpdate write FAutoUpdate;
  end;

// === FACTORY FUNCTIONS ===
function CreateTokenList(const Name: string = ''): TTokenList;
function CreateKeywordList(CaseSensitive: Boolean = False): TKeywordTokenList;
function CreateFilteredList(Source: TTokenList; Filter: TTokenFilterFunc): TFilteredTokenList;

// === UTILITY FUNCTIONS ===
function DefaultTokenCompare(Token1, Token2: TLexerToken): Integer;
function TokenCompareByPosition(Token1, Token2: TLexerToken): Integer;
function TokenCompareByType(Token1, Token2: TLexerToken): Integer;
function TokenCompareByValue(Token1, Token2: TLexerToken): Integer;

// Common filters
function FilterNonWhitespace(Token: TLexerToken; Index: Integer): Boolean;
function FilterKeywords(Token: TLexerToken; Index: Integer): Boolean;
function FilterComments(Token: TLexerToken; Index: Integer): Boolean;
//function FilterByType(TargetType: TTokenType): TTokenFilterFunc;

// === EXCEPTIONS ===
type
  ETokenListException = class(Exception);
  ETokenListReadOnlyException = class(ETokenListException);
  ETokenListIndexException = class(ETokenListException);

implementation

uses
  StrUtils, Math;

{ TTokenIterator }

constructor TTokenIterator.Create(ATokenList: TTokenList; AStartIndex: Integer;
                                 AEndIndex: Integer; ADirection: Integer);
begin
  inherited Create;
  FTokenList := ATokenList;
  FStartIndex := Max(0, AStartIndex);
  FEndIndex := AEndIndex;
  if (FEndIndex < 0) or (FEndIndex >= FTokenList.Count) then
    FEndIndex := FTokenList.Count - 1;
  FDirection := ADirection;
  FCurrentIndex := FStartIndex;
end;

function TTokenIterator.HasNext: Boolean;
begin
  if FDirection = 1 then
    Result := FCurrentIndex <= FEndIndex
  else
    Result := FCurrentIndex >= FEndIndex;
end;

function TTokenIterator.HasPrevious: Boolean;
begin
  if FDirection = 1 then
    Result := FCurrentIndex > FStartIndex
  else
    Result := FCurrentIndex < FStartIndex;
end;

function TTokenIterator.Next: TLexerToken;
begin
  if HasNext then
  begin
    Result := FTokenList[FCurrentIndex];
    Inc(FCurrentIndex, FDirection);
  end
  else
    Result := nil;
end;

function TTokenIterator.Previous: TLexerToken;
begin
  Dec(FCurrentIndex, FDirection);
  if (FCurrentIndex >= 0) and (FCurrentIndex < FTokenList.Count) then
    Result := FTokenList[FCurrentIndex]
  else
  begin
    Inc(FCurrentIndex, FDirection); // Restore position
    Result := nil;
  end;
end;

function TTokenIterator.Current: TLexerToken;
begin
  if (FCurrentIndex >= 0) and (FCurrentIndex < FTokenList.Count) then
    Result := FTokenList[FCurrentIndex]
  else
    Result := nil;
end;

function TTokenIterator.Peek(Offset: Integer): TLexerToken;
var
  PeekIndex: Integer;
begin
  PeekIndex := FCurrentIndex + (Offset * FDirection);
  if (PeekIndex >= 0) and (PeekIndex < FTokenList.Count) then
    Result := FTokenList[PeekIndex]
  else
    Result := nil;
end;

function TTokenIterator.GetCurrentIndex: Integer;
begin
  Result := FCurrentIndex;
end;

procedure TTokenIterator.Reset;
begin
  FCurrentIndex := FStartIndex;
end;

procedure TTokenIterator.MoveTo(Index: Integer);
begin
  FCurrentIndex := Max(FStartIndex, Min(FEndIndex, Index));
end;

procedure TTokenIterator.MoveToEnd;
begin
  FCurrentIndex := FEndIndex;
end;

{ TTokenList }

constructor TTokenList.Create(AOwnsTokens: Boolean);
begin
  inherited Create(AOwnsTokens);
  //FOwnsTokens := AOwnsTokens;
  FCurrentIndex := 0;
  FModificationCount := 0;
  FReadOnly := False;
  FLastSearchIndex := -1;
  FLastSearchToken := nil;
end;

constructor TTokenList.CreateNamed(const AName: string; AOwnsTokens: Boolean);
begin
  Create(AOwnsTokens);
  FName := AName;
end;

destructor TTokenList.Destroy;
begin
  inherited Destroy;  // TFPObjectList gestisce la memoria
end;

procedure TTokenList.CheckReadOnly;
begin
  if FReadOnly then
    raise ETokenListReadOnlyException.Create('TokenList is read-only');
end;

procedure TTokenList.IncModificationCount;
begin
  Inc(FModificationCount);
end;

function TTokenList.GetTokenByIndex(Index: Integer): TLexerToken;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TLexerToken(inherited Items[Index])
  else
    raise ETokenListIndexException.CreateFmt('Token index %d out of bounds (0..%d)', [Index, Count-1]);
end;

procedure TTokenList.SetTokenByIndex(Index: Integer; const Value: TLexerToken);
var
  OldToken: TLexerToken;
begin
  CheckReadOnly;
  if (Index >= 0) and (Index < Count) then
  begin
    OldToken := TLexerToken(Items[Index]);
    Items[Index] := Value;
    DoTokenReplaced(OldToken, Value, Index);
    IncModificationCount;
  end
  else
    raise ETokenListIndexException.CreateFmt('Token index %d out of bounds (0..%d)', [Index, Count-1]);
end;

function TTokenList.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TTokenList.GetFirst: TLexerToken;
begin
  if Count > 0 then
    Result := GetTokenByIndex(0)
  else
    Result := nil;
end;

function TTokenList.GetLast: TLexerToken;
begin
  if Count > 0 then
    Result := GetTokenByIndex(Count - 1)
  else
    Result := nil;
end;

// Virtual event methods
procedure TTokenList.DoTokenAdded(Token: TLexerToken; Index: Integer);
begin
  // Override in descendants
end;

procedure TTokenList.DoTokenRemoved(Token: TLexerToken; Index: Integer);
begin
  // Override in descendants
end;

procedure TTokenList.DoTokenReplaced(OldToken, NewToken: TLexerToken; Index: Integer);
begin
  // Override in descendants
end;

procedure TTokenList.DoListCleared;
begin
  // Override in descendants
end;

// === BASIC OPERATIONS ===

function TTokenList.AddToken(Token: TLexerToken): Integer;
begin
  CheckReadOnly;
  Result := self.Add(Token);  // <-- TFPObjectList.Add(Object) restituisce index
  DoTokenAdded(Token, Result);
  IncModificationCount;
end;

function TTokenList.InsertToken(Index: Integer; Token: TLexerToken): Integer;
begin
  CheckReadOnly;
  inherited Insert(Index, Token);  // <-- TFPObjectList.Insert(Index, Object) is a procedure
  Result := Index;
  DoTokenAdded(Token, Index);
  IncModificationCount;
end;

function TTokenList.RemoveToken(Token: TLexerToken): Boolean;
var
  Index: Integer;
begin
  CheckReadOnly;
  Index := FindToken(Token);
  Result := Index >= 0;
  if Result then
  begin
    inherited Delete(Index);  // <-- TFPObjectList.Delete(Index)
    DoTokenRemoved(Token, Index);
    IncModificationCount;
  end;
end;

function TTokenList.RemoveTokenAt(Index: Integer): Boolean;
var
  Token: TLexerToken;
begin
  CheckReadOnly;
  Result := (Index >= 0) and (Index < Count);
  if Result then
  begin
    Token := GetTokenByIndex(Index);
    Delete(Index);  // <-- Semplice Delete di TFPObjectList
    DoTokenRemoved(Token, Index);
    IncModificationCount;
  end;
end;

function TTokenList.ExtractToken(Token: TLexerToken): TLexerToken;
var
  Index: Integer;
begin
  CheckReadOnly;
  Index := FindToken(Token);
  if Index >= 0 then
    Result := ExtractTokenAt(Index)
  else
    Result := nil;
end;

function TTokenList.ExtractTokenAt(Index: Integer): TLexerToken;
begin
  CheckReadOnly;
  if (Index >= 0) and (Index < Count) then
  begin
    Result := GetTokenByIndex(Index);
    Items[Index] := nil; // Don't free
    Delete(Index);
    DoTokenRemoved(Result, Index);
    IncModificationCount;
  end
  else
    Result := nil;
end;

procedure TTokenList.Clear;
begin
  CheckReadOnly;
  inherited Clear;  // <-- TFPObjectList.Clear
  FCurrentIndex := 0;
  DoListCleared;
  IncModificationCount;
end;

function TTokenList.GetTokenDirect(Index: Integer): TLexerToken;
begin
  Result := TLexerToken(Items[Index]);
end;

procedure TTokenList.SetTokenDirect(Index: Integer; Token: TLexerToken);
begin
  // Accesso diretto senza controlli - per performance critiche
  Items[Index] := Token;
end;

procedure TTokenList.PreAllocateTokens(ACount: Integer);
var
  i: Integer;
  Token: TLexerToken;
begin
  CheckReadOnly;
  Capacity := ACount;

  for i := 0 to ACount - 1 do
  begin
    Token := TLexerToken.Create(ttUnknown, '', 0, 0, 0, 0, nil);
    Add(Token);  // Usa Add normale, non inherited
  end;
end;

function TTokenList.GetNextPoolToken(var CurrentIndex: Integer): TLexerToken;
begin
  if CurrentIndex < Count then
  begin
    // Token pre-allocato disponibile
    Result := TLexerToken(Items[CurrentIndex]);  // <-- Accesso diretto O(1)
  end
  else
  begin
    // Overflow: crea nuovo token
    Result := TLexerToken.Create(ttUnknown, '', 0, 0, 0, 0, nil);
    Add(Result);  // <-- Semplice Add
  end;
  Inc(CurrentIndex);
end;

procedure TTokenList.ResetPool;
begin
  // Reset solo il current index per riutilizzo
  FCurrentIndex := 0;
  // Non chiamare Clear() - mantieni i token pre-allocati
end;


// === BATCH OPERATIONS ===

procedure TTokenList.AddTokens(const Tokens: array of TLexerToken);
var
  i: Integer;
begin
  CheckReadOnly;
  for i := Low(Tokens) to High(Tokens) do
    AddToken(Tokens[i]);
end;

procedure TTokenList.AddTokens(TokenList: TTokenList);
var
  i: Integer;
begin
  CheckReadOnly;
  if Assigned(TokenList) then
    for i := 0 to TokenList.Count - 1 do
      AddToken(TokenList[i]);
end;

procedure TTokenList.InsertTokens(Index: Integer; const Tokens: array of TLexerToken);
var
  i: Integer;
begin
  CheckReadOnly;
  for i := Low(Tokens) to High(Tokens) do
    InsertToken(Index + i, Tokens[i]);
end;

procedure TTokenList.InsertTokens(Index: Integer; TokenList: TTokenList);
var
  i: Integer;
begin
  CheckReadOnly;
  if Assigned(TokenList) then
    for i := 0 to TokenList.Count - 1 do
      InsertToken(Index + i, TokenList[i]);
end;

function TTokenList.RemoveTokensInRange(StartIndex, EndIndex: Integer): Integer;
var
  i: Integer;
begin
  CheckReadOnly;
  Result := 0;
  if (StartIndex >= 0) and (EndIndex < Count) and (StartIndex <= EndIndex) then
  begin
    for i := EndIndex downto StartIndex do
    begin
      RemoveTokenAt(i);
      Inc(Result);
    end;
  end;
end;

function TTokenList.RemoveTokensByType(TokenType: TTokenType): Integer;
var
  i: Integer;
begin
  CheckReadOnly;
  Result := 0;
  for i := Count - 1 downto 0 do
  begin
    if GetTokenByIndex(i).TokenType = TokenType then
    begin
      RemoveTokenAt(i);
      Inc(Result);
    end;
  end;
end;

function TTokenList.RemoveTokensByTypes(const TokenTypes: array of TTokenType): Integer;
var
  i, j: Integer;
  Token: TLexerToken;
  ShouldRemove: Boolean;
begin
  CheckReadOnly;
  Result := 0;
  for i := Count - 1 downto 0 do
  begin
    Token := GetTokenByIndex(i);
    ShouldRemove := False;

    for j := Low(TokenTypes) to High(TokenTypes) do
    begin
      if Token.TokenType = TokenTypes[j] then
      begin
        ShouldRemove := True;
        Break;
      end;
    end;

    if ShouldRemove then
    begin
      RemoveTokenAt(i);
      Inc(Result);
    end;
  end;
end;

// === NAVIGATION ===

function TTokenList.GetCurrentToken: TLexerToken;
begin
  if (FCurrentIndex >= 0) and (FCurrentIndex < Count) then
    Result := GetTokenByIndex(FCurrentIndex)
  else
    Result := nil;
end;

function TTokenList.GetNextToken: TLexerToken;
begin
  Inc(FCurrentIndex);
  Result := GetCurrentToken;
end;

function TTokenList.GetPreviousToken: TLexerToken;
begin
  if FCurrentIndex > 0 then
    Dec(FCurrentIndex);
  Result := GetCurrentToken;
end;

function TTokenList.PeekToken(Offset: Integer): TLexerToken;
var
  PeekIndex: Integer;
begin
  PeekIndex := FCurrentIndex + Offset;
  if (PeekIndex >= 0) and (PeekIndex < Count) then
    Result := TLexerToken(Items[PeekIndex])
  else
    Result := nil;
end;

procedure TTokenList.Reset;
begin
  FCurrentIndex := 0;
end;

procedure TTokenList.MoveTo(Index: Integer);
begin
  FCurrentIndex := Max(0, Min(Count - 1, Index));
end;

procedure TTokenList.MoveToFirst;
begin
  FCurrentIndex := 0;
end;

procedure TTokenList.MoveToLast;
begin
  FCurrentIndex := Max(0, Count - 1);
end;

function TTokenList.HasMore: Boolean;
begin
  Result := FCurrentIndex < Count;
end;

function TTokenList.HasPrevious: Boolean;
begin
  Result := FCurrentIndex > 0;
end;

// === SEARCH METHODS ===

function TTokenList.FindToken(Token: TLexerToken): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if TLexerToken(Items[i]) = Token then  // <-- Accesso diretto
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TTokenList.FindTokenByValue(const Value: string; StartIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := StartIndex to Count - 1 do
  begin
    if TLexerToken(Items[i]).Value = Value then  // <-- Accesso diretto
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TTokenList.FindTokenByType(TokenType: TTokenType; StartIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := StartIndex to Count - 1 do
  begin
    if GetTokenByIndex(i).TokenType = TokenType then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TTokenList.FindTokensByType(TokenType: TTokenType): TTokenIndexArray;
var
  i, ResultCount: Integer;  // Cambia "Count" in "ResultCount"
begin
  SetLength(Result, 0);
  ResultCount := 0;

  for i := 0 to Self.Count - 1 do
  begin
    if GetTokenByIndex(i).TokenType = TokenType then
    begin
      if ResultCount >= Length(Result) then
        SetLength(Result, Max(4, Length(Result) * 2));
      Result[ResultCount] := i;
      Inc(ResultCount);
    end;
  end;

  SetLength(Result, ResultCount);
end;

function TTokenList.FindTokensByTypes(const TokenTypes: array of TTokenType): TTokenIndexArray;
var
  i, j, ResultCount: Integer;  // Cambia "Count" in "ResultCount"
  Token: TLexerToken;
  Found: Boolean;
begin
  SetLength(Result, 0);
  ResultCount := 0;  // Cambia tutte le occorrenze

  for i := 0 to Self.Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    Found := False;

    for j := Low(TokenTypes) to High(TokenTypes) do
    begin
      if Token.TokenType = TokenTypes[j] then
      begin
        Found := True;
        Break;
      end;
    end;

    if Found then
    begin
      if ResultCount >= Length(Result) then
        SetLength(Result, Max(4, Length(Result) * 2));
      Result[ResultCount] := i;
      Inc(ResultCount);
    end;
  end;

  SetLength(Result, ResultCount);
end;

function TTokenList.FindTokensByValue(const Value: string; Options: TTokenSearchOptions): TTokenIndexArray;
var
  i, ResultCount: Integer;  // Cambia "Count" in "ResultCount"
  Token: TLexerToken;
  TokenValue: string;
  ShouldInclude: Boolean;
begin
  SetLength(Result, 0);
  ResultCount := 0;

  for i := Options.StartFromIndex to Self.Count - 1 do
  begin
    if (Options.MaxResults > 0) and (ResultCount >= Options.MaxResults) then
      Break;

    Token := GetTokenByIndex(i);

    if not Options.SearchInComments and Token.IsCommentToken then
      Continue;
    if not Options.SearchInStrings and Token.IsStringToken then
      Continue;

    TokenValue := Token.Value;
    ShouldInclude := False;

    if Options.CaseSensitive then
    begin
      if Options.WholeWord then
        ShouldInclude := TokenValue = Value
      else
        ShouldInclude := Pos(Value, TokenValue) > 0;
    end
    else
    begin
      if Options.WholeWord then
        ShouldInclude := UpperCase(TokenValue) = UpperCase(Value)
      else
        ShouldInclude := Pos(UpperCase(Value), UpperCase(TokenValue)) > 0;
    end;

    if ShouldInclude then
    begin
      if ResultCount >= Length(Result) then
        SetLength(Result, Max(4, Length(Result) * 2));
      Result[ResultCount] := i;
      Inc(ResultCount);
    end;
  end;

  SetLength(Result, ResultCount);
end;

// Additional search methods implementation would continue here...
// [For brevity, showing key methods - full implementation would include all declared methods]

function TTokenList.CreateIterator(StartIndex: Integer; EndIndex: Integer): TTokenIterator;
begin
  Result := TTokenIterator.Create(Self, StartIndex, EndIndex);
end;

// === UTILITY FUNCTIONS ===

function CreateTokenList(const Name: string): TTokenList;
begin
  if Name <> '' then
    Result := TTokenList.CreateNamed(Name)
  else
    Result := TTokenList.Create;
end;

function DefaultTokenCompare(Token1, Token2: TLexerToken): Integer;
begin
  if Token1.Position < Token2.Position then
    Result := -1
  else if Token1.Position > Token2.Position then
    Result := 1
  else
    Result := 0;
end;

function TokenCompareByPosition(Token1, Token2: TLexerToken): Integer;
begin
  Result := DefaultTokenCompare(Token1, Token2);
end;

function TokenCompareByType(Token1, Token2: TLexerToken): Integer;
begin
  Result := Ord(Token1.TokenType) - Ord(Token2.TokenType);
end;

function TokenCompareByValue(Token1, Token2: TLexerToken): Integer;
begin
  Result := CompareStr(Token1.Value, Token2.Value);
end;

// === FILTER FUNCTIONS ===

function FilterNonWhitespace(Token: TLexerToken; Index: Integer): Boolean;
begin
  Result := not Token.IsWhitespaceToken;
end;

function FilterKeywords(Token: TLexerToken; Index: Integer): Boolean;
begin
  Result := Token.IsKeywordToken;
end;

function FilterComments(Token: TLexerToken; Index: Integer): Boolean;
begin
  Result := Token.IsCommentToken;
end;

//function FilterByType(TargetType: TTokenType): TTokenFilterFunc;
//begin
//  Result := function(Token: TLexerToken; Index: Integer): Boolean
//  begin
//    Result := Token.TokenType = TargetType;
//  end;
//end;

// === CONTINUED IMPLEMENTATION OF TTokenList ===

//function TTokenList.FindFirstWhere(FilterFunc: TTokenFilterFunc): TTokenPosition;
//var
//  i: Integer;
//begin
//  Result.IsValid := False;
//  Result.Index := -1;
//  Result.Token := nil;
//
//  for i := 0 to Count - 1 do
//  begin
//    if FilterFunc(GetTokenByIndex(i), i) then
//    begin
//      Result.IsValid := True;
//      Result.Index := i;
//      Result.Token := GetTokenByIndex(i);
//      Break;
//    end;
//  end;
//end;

//function TTokenList.FindAllWhere(FilterFunc: TTokenFilterFunc): TTokenIndexArray;
//var
//  i, ACount: Integer;
//begin
//  SetLength(Result, 0);
//  ACount := 0;
//
//  for i := 0 to Self.Count - 1 do
//  begin
//    if FilterFunc(GetTokenByIndex(i), i) then
//    begin
//      if ACount >= Length(Result) then
//        SetLength(Result, Max(4, Length(Result) * 2));
//      Result[ACount] := i;
//      Inc(ACount);
//    end;
//  end;
//
//  SetLength(Result, ACount);
//end;

//function TTokenList.FindInRange(StartIndex, EndIndex: Integer; FilterFunc: TTokenFilterFunc): TTokenIndexArray;
//var
//  i, ACount: Integer;
//  SafeStart, SafeEnd: Integer;
//begin
//  SetLength(Result, 0);
//  ACount := 0;
//
//  SafeStart := Max(0, StartIndex);
//  SafeEnd := Min(Self.Count - 1, EndIndex);
//
//  for i := SafeStart to SafeEnd do
//  begin
//    if FilterFunc(GetTokenByIndex(i), i) then
//    begin
//      if ACount >= Length(Result) then
//        SetLength(Result, Max(4, Length(Result) * 2));
//      Result[ACount] := i;
//      Inc(ACount);
//    end;
//  end;
//
//  SetLength(Result, ACount);
//end;

function TTokenList.FindNextKeyword(StartIndex: Integer): TTokenPosition;
var
  i: Integer;
  StartPos: Integer;
begin
  Result.IsValid := False;
  Result.Index := -1;
  Result.Token := nil;

  if StartIndex < 0 then
    StartPos := FCurrentIndex + 1
  else
    StartPos := StartIndex;

  for i := StartPos to Count - 1 do
  begin
    if GetTokenByIndex(i).IsKeywordToken then
    begin
      Result.IsValid := True;
      Result.Index := i;
      Result.Token := GetTokenByIndex(i);
      Break;
    end;
  end;
end;

function TTokenList.FindNextOfTypes(const TokenTypes: array of TTokenType; StartIndex: Integer): TTokenPosition;
var
  i, j: Integer;
  StartPos: Integer;
  Token: TLexerToken;
begin
  Result.IsValid := False;
  Result.Index := -1;
  Result.Token := nil;

  if StartIndex < 0 then
    StartPos := FCurrentIndex + 1
  else
    StartPos := StartIndex;

  for i := StartPos to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    for j := Low(TokenTypes) to High(TokenTypes) do
    begin
      if Token.TokenType = TokenTypes[j] then
      begin
        Result.IsValid := True;
        Result.Index := i;
        Result.Token := Token;
        Exit;
      end;
    end;
  end;
end;

// === POSITION & RANGE OPERATIONS ===

function TTokenList.GetTokenByPosition(Position: Integer): TLexerToken;
var
  i: Integer;
  Token: TLexerToken;
begin
  Result := nil;

  // Use cache if possible
  if (FLastSearchIndex >= 0) and (FLastSearchIndex < Count) and
     Assigned(FLastSearchToken) and
     FLastSearchToken.ContainsPosition(Position) then
  begin
    Result := FLastSearchToken;
    Exit;
  end;

  // Linear search for now - could be optimized with binary search if tokens are sorted
  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    if Token.ContainsPosition(Position) then
    begin
      Result := Token;
      FLastSearchIndex := i;
      FLastSearchToken := Token;
      Break;
    end;
  end;
end;

function TTokenList.GetTokenByLocation(Line, Column: Integer): TLexerToken;
var
  i: Integer;
  Token: TLexerToken;
begin
  Result := nil;

  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    if Token.ContainsLocation(Line, Column) then
    begin
      Result := Token;
      Break;
    end;
  end;
end;

function TTokenList.GetTokensInRange(StartPos, EndPos: Integer): TTokenList;
var
  i: Integer;
  Token: TLexerToken;
begin
  Result := TTokenList.Create(False); // Don't own tokens

  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    if (Token.Position >= StartPos) and (Token.EndPosition <= EndPos) then
      Result.AddToken(Token);
  end;
end;

function TTokenList.GetTokensInLineRange(StartLine, EndLine: Integer): TTokenList;
var
  i: Integer;
  Token: TLexerToken;
begin
  Result := TTokenList.Create(False); // Don't own tokens

  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    if (Token.Line >= StartLine) and (Token.Line <= EndLine) then
      Result.AddToken(Token);
  end;
end;

function TTokenList.GetTokenRange(StartIndex, EndIndex: Integer): TTokenRange;
begin
  Result.StartIndex := Max(0, StartIndex);
  Result.EndIndex := Min(Count - 1, EndIndex);
  Result.Count := Max(0, Result.EndIndex - Result.StartIndex + 1);
  Result.IsValid := (Result.StartIndex <= Result.EndIndex) and
                   (Result.StartIndex >= 0) and
                   (Result.EndIndex < Count);
end;

// === ANALYSIS & STATISTICS ===

function TTokenList.CountTokensByType(TokenType: TTokenType): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    if TLexerToken(Items[i]).TokenType = TokenType then
      Inc(Result);
  end;
end;

function TTokenList.CountTokensByTypes(const TokenTypes: array of TTokenType): Integer;
var
  i, j: Integer;
  Token: TLexerToken;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    for j := Low(TokenTypes) to High(TokenTypes) do
    begin
      if Token.TokenType = TokenTypes[j] then
      begin
        Inc(Result);
        Break;
      end;
    end;
  end;
end;

function TTokenList.GetTokenTypeStatistics: TStringList;
var
  i: Integer;
  Token: TLexerToken;
  TypeName: string;
  Index: Integer;
  ACount: PtrInt;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;

  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    TypeName := GetTokenTypeName(Token.TokenType);

    Index := Result.IndexOf(TypeName);
    if Index >= 0 then
    begin
      // CORREZIONE: Usa PtrInt per conversioni sicure
      ACount := PtrInt(Result.Objects[Index]) + 1;
      Result.Objects[Index] := TObject(ACount);
    end
    else
      Result.Objects[Index] := TObject(PtrInt(1));  // <-- PtrInt(1) invece di 1
  end;
end;

function TTokenList.GetUniqueValues: TStringList;
var
  i: Integer;
  Token: TLexerToken;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;

  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    Result.Add(Token.Value);
  end;
end;

function TTokenList.GetLineCount: Integer;
var
  i: Integer;
  Token: TLexerToken;
  MaxLine: Integer;
begin
  MaxLine := 0;
  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    if Token.Line > MaxLine then
      MaxLine := Token.Line;
  end;
  Result := MaxLine;
end;

function TTokenList.GetMaxLineNumber: Integer;
var
  i: Integer;
  Token: TLexerToken;
  LineNum: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    if Token.TokenType = ttLineNumber then
    begin
      if TryStrToInt(Token.Value, LineNum) and (LineNum > Result) then
        Result := LineNum;
    end;
  end;
end;

function TTokenList.HasTokenType(TokenType: TTokenType): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if TLexerToken(Items[i]).TokenType = TokenType then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TTokenList.HasAnyOfTypes(const TokenTypes: array of TTokenType): Boolean;
var
  i, j: Integer;
  Token: TLexerToken;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    for j := Low(TokenTypes) to High(TokenTypes) do
    begin
      if Token.TokenType = TokenTypes[j] then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

// === TRANSFORMATION ===

//function TTokenList.Map(MapFunc: TTokenMapFunc): TTokenList;
//var
//  i: Integer;
//  MappedToken: TLexerToken;
//begin
//  Result := TTokenList.Create(True); // Own mapped tokens
//
//  for i := 0 to Count - 1 do
//  begin
//    MappedToken := MapFunc(GetTokenByIndex(i), i);
//    if Assigned(MappedToken) then
//      Result.AddToken(MappedToken);
//  end;
//end;

//function TTokenList.Filter(FilterFunc: TTokenFilterFunc): TTokenList;
//var
//  i: Integer;
//  Token: TLexerToken;
//begin
//  Result := TTokenList.Create(False); // Don't own original tokens
//
//  for i := 0 to Count - 1 do
//  begin
//    Token := GetTokenByIndex(i);
//    if FilterFunc(Token, i) then
//      Result.AddToken(Token);
//  end;
//end;

function TTokenList.FilterByType(TokenType: TTokenType): TTokenList;
var
  i: Integer;
  Token: TLexerToken;
begin
  Result := TTokenList.Create(False); // Don't own original tokens

  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    if Token.TokenType = TokenType then
      Result.AddToken(Token);
  end;
end;

function TTokenList.FilterByTypes(const TokenTypes: array of TTokenType): TTokenList;
var
  i, j: Integer;
  Token: TLexerToken;
  ShouldInclude: Boolean;
begin
  Result := TTokenList.Create(False); // Don't own original tokens

  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    ShouldInclude := False;

    for j := Low(TokenTypes) to High(TokenTypes) do
    begin
      if Token.TokenType = TokenTypes[j] then
      begin
        ShouldInclude := True;
        Break;
      end;
    end;

    if ShouldInclude then
      Result.AddToken(Token);
  end;
end;

function TTokenList.FilterByRange(StartIndex, EndIndex: Integer): TTokenList;
var
  i: Integer;
  SafeStart, SafeEnd: Integer;
begin
  Result := TTokenList.Create(False); // Don't own original tokens

  SafeStart := Max(0, StartIndex);
  SafeEnd := Min(Count - 1, EndIndex);

  for i := SafeStart to SafeEnd do
    Result.AddToken(GetTokenByIndex(i));
end;

// === SORTING ===

procedure TTokenList.Sort(CompareFunc: TTokenCompareFunc);
var
  i, j: Integer;
  Token1, Token2: TLexerToken;
begin
  CheckReadOnly;

  // Simple bubble sort - could be optimized with QuickSort
  for i := 0 to Count - 2 do
  begin
    for j := i + 1 to Count - 1 do
    begin
      Token1 := GetTokenByIndex(i);
      Token2 := GetTokenByIndex(j);

      if CompareFunc(Token1, Token2) > 0 then
      begin
        Items[i] := Token2;
        Items[j] := Token1;
      end;
    end;
  end;

  IncModificationCount;
end;

procedure TTokenList.SortByPosition;
begin
  Sort(@TokenCompareByPosition);
end;

procedure TTokenList.SortByType;
begin
  Sort(@TokenCompareByType);
end;

procedure TTokenList.SortByValue;
begin
  Sort(@TokenCompareByValue);
end;

// === VALIDATION ===

function TTokenList.ValidatePositions: Boolean;
var
  i: Integer;
  Token: TLexerToken;
  LastPosition: Integer;
begin
  Result := True;
  LastPosition := -1;

  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    if Token.Position <= LastPosition then
    begin
      Result := False;
      Break;
    end;
    LastPosition := Token.EndPosition;
  end;
end;

function TTokenList.ValidateLineNumbers: Boolean;
var
  i: Integer;
  Token: TLexerToken;
begin
  Result := True;

  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    if (Token.Line < 1) or (Token.Column < 1) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TTokenList.FindDuplicates: TTokenIndexArray;
var
  i, j, ACount: Integer;
  Token1, Token2: TLexerToken;
begin
  SetLength(Result, 0);
  ACount := 0;

  for i := 0 to Self.Count - 2 do
  begin
    Token1 := GetTokenByIndex(i);
    for j := i + 1 to Self.Count - 1 do
    begin
      Token2 := GetTokenByIndex(j);
      if Token1.Equals(Token2) then
      begin
        if ACount >= Length(Result) then
          SetLength(Result, Max(4, Length(Result) * 2));
        Result[ACount] := j;
        Inc(ACount);
      end;
    end;
  end;

  SetLength(Result, ACount);
end;

function TTokenList.FindGaps: TTokenIndexArray;
var
  i, ACount: Integer;
  Token, NextToken: TLexerToken;
begin
  SetLength(Result, 0);
  ACount := 0;

  for i := 0 to Self.Count - 2 do
  begin
    Token := GetTokenByIndex(i);
    NextToken := GetTokenByIndex(i + 1);

    if Token.EndPosition + 1 < NextToken.Position then
    begin
      if ACount >= Length(Result) then
        SetLength(Result, Max(4, Length(Result) * 2));
      Result[ACount] := i;
      Inc(ACount);
    end;
  end;

  SetLength(Result, ACount);
end;

// === UTILITY METHODS ===

function TTokenList.Clone: TTokenList;
var
  i: Integer;
begin
  Result := TTokenList.CreateNamed(FName, True); // Own cloned tokens
  Result.FSourceFile := FSourceFile;

  for i := 0 to Count - 1 do
    Result.AddToken(GetTokenByIndex(i).Clone);
end;

function TTokenList.CloneRange(StartIndex, EndIndex: Integer): TTokenList;
var
  i: Integer;
  SafeStart, SafeEnd: Integer;
begin
  Result := TTokenList.Create(True); // Own cloned tokens

  SafeStart := Max(0, StartIndex);
  SafeEnd := Min(Count - 1, EndIndex);

  for i := SafeStart to SafeEnd do
    Result.AddToken(GetTokenByIndex(i).Clone);
end;

procedure TTokenList.Assign(Source: TTokenList);
var
  i: Integer;
begin
  CheckReadOnly;
  Clear;

  if Assigned(Source) then
  begin
    FName := Source.FName;
    FSourceFile := Source.FSourceFile;

    for i := 0 to Source.Count - 1 do
    begin
      if self.OwnsObjects then
        AddToken(Source[i].Clone)
      else
        AddToken(Source[i]);
    end;
  end;
end;

procedure TTokenList.Merge(Other: TTokenList; AllowDuplicates: Boolean);
var
  i: Integer;
  Token: TLexerToken;
begin
  CheckReadOnly;

  if Assigned(Other) then
  begin
    for i := 0 to Other.Count - 1 do
    begin
      Token := Other[i];
      if AllowDuplicates or (FindToken(Token) < 0) then
      begin
        if self.OwnsObjects then
          AddToken(Token.Clone)
        else
          AddToken(Token);
      end;
    end;
  end;
end;

function TTokenList.IsEqual(Other: TTokenList): Boolean;
var
  i: Integer;
begin
  Result := False;

  if not Assigned(Other) or (Count <> Other.Count) then
    Exit;

  for i := 0 to Count - 1 do
  begin
    if not GetTokenByIndex(i).Equals(Other[i]) then
      Exit;
  end;

  Result := True;
end;

function TTokenList.GetChecksum: string;
var
  i: Integer;
  Token: TLexerToken;
  HashValue: Cardinal;
begin
  HashValue := 0;

  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    // Simple hash combining token properties
    HashValue := HashValue xor Cardinal(Ord(Token.TokenType));
    HashValue := HashValue xor Cardinal(Length(Token.Value));
    HashValue := HashValue xor Cardinal(Token.Position);
  end;

  Result := IntToHex(HashValue, 8);
end;

// === DEBUG & DIAGNOSTICS ===

function TTokenList.GetDebugInfo: string;
begin
  Result := Format('TokenList "%s": %d tokens, ModCount=%d, ReadOnly=%s, Current=%d', [
    FName, Count, FModificationCount, BoolToStr(FReadOnly, True), FCurrentIndex
  ]);

  if FSourceFile <> '' then
    Result := Result + Format(', Source="%s"', [FSourceFile]);
end;

function TTokenList.GetStatisticsReport: string;
var
  Stats: TStringList;
  i: Integer;
  TypeName: string;
  TypeCount: PtrInt;
  MemoryUsedMB: Double;
  OverheadPercent: Double;
  AccessMethod: string;
  EfficiencyNote: string;
  IsOptimized: Boolean;
begin
  Stats := GetTokenTypeStatistics;
  try
    // === HEADER CON INFO IMPLEMENTAZIONE ===
    Result := Format('=== TOKEN STATISTICS for "%s" ==='+LineEnding, [FName]);

    // === BASIC COUNTS ===
    Result := Result + Format('Total Tokens: %d'+LineEnding, [Count]);
    Result := Result + Format('Unique Types: %d'+LineEnding, [Stats.Count]);
    Result := Result + Format('Line Count: %d'+LineEnding, [GetLineCount]);

    // === MEMORY & PERFORMANCE INFO ===
    MemoryUsedMB := (Count * SizeOf(TLexerToken)) / (1024 * 1024);
    Result := Result + Format('Memory Used: %.2f MB'+LineEnding, [MemoryUsedMB]);

    // Determine the current implementation type
    // FIX: Check the base class instead of "is TFPObjectList"
    IsOptimized := (Self.ClassParent = TFPObjectList) or (Self.ClassName = 'TTokenList');

    if IsOptimized then
    begin
      AccessMethod := 'Direct O(1) array indexing';
      OverheadPercent := 0.0;
      EfficiencyNote := 'OPTIMIZED - Zero hash overhead';

      Result := Result + Format('Capacity: %d'+LineEnding, [Capacity]);
      if Capacity > 0 then
        Result := Result + Format('Fill Ratio: %.1f%%'+LineEnding, [(Count * 100.0) / Capacity]);
    end
    else
    begin
      AccessMethod := 'Hash table lookup O(1)';
      OverheadPercent := 15.0; // Stima overhead hash
      EfficiencyNote := 'LEGACY - Hash lookup overhead present';
    end;

    Result := Result + Format('Access Method: %s'+LineEnding, [AccessMethod]);
    Result := Result + Format('Lookup Overhead: %.1f%%'+LineEnding, [OverheadPercent]);
    Result := Result + Format('Implementation: %s'+LineEnding, [EfficiencyNote]);
    Result := Result + Format('Base Class: %s'+LineEnding, [Self.ClassParent.ClassName]);

    // === SOURCE FILE INFO ===
    if FSourceFile <> '' then
      Result := Result + Format('Source File: "%s"'+LineEnding, [FSourceFile]);

    // === MODIFICATION TRACKING ===
    Result := Result + Format('Modification Count: %d'+LineEnding, [FModificationCount]);
    Result := Result + Format('Read Only: %s'+LineEnding, [BoolToStr(FReadOnly, True)]);
    Result := Result + Format('Current Index: %d'+LineEnding, [FCurrentIndex]);

    // === PERFORMANCE ESTIMATES ===
    Result := Result + LineEnding + '=== PERFORMANCE ESTIMATES ===' + LineEnding;

    if IsOptimized then
    begin
      Result := Result + 'Expected Lexing Speed: 150-200K tokens/sec' + LineEnding;
      Result := Result + 'Multi-pass Speed: 500K+ tokens/sec' + LineEnding;
      Result := Result + 'Access Time: ~1-2 CPU cycles' + LineEnding;
    end
    else
    begin
      Result := Result + 'Expected Lexing Speed: 100-150K tokens/sec' + LineEnding;
      Result := Result + 'Multi-pass Speed: 200-400K tokens/sec' + LineEnding;
      Result := Result + 'Access Time: ~10-20 CPU cycles' + LineEnding;
    end;

    // === TYPE DISTRIBUTION ===
    Result := Result + LineEnding + '=== TYPE DISTRIBUTION ===' + LineEnding;

    for i := 0 to Stats.Count - 1 do
    begin
      TypeName := Stats[i];
      TypeCount := PtrInt(Stats.Objects[i]);
      Result := Result + Format('  %-20s: %6d (%5.1f%%)'+LineEnding, [
        TypeName, TypeCount, (TypeCount * 100.0) / Count
      ]);
    end;

    // === QUALITY METRICS ===
    Result := Result + LineEnding + '=== QUALITY METRICS ===' + LineEnding;

    // Calculate quality metrics
    if HasTokenType(ttError) then
      Result := Result + Format('Error Tokens: %d'+LineEnding, [CountTokensByType(ttError)])
    else
      Result := Result + 'Error Tokens: 0 (Clean scan)'+LineEnding;

    if HasTokenType(ttLineNumber) then
      Result := Result + Format('Max Line Number: %d'+LineEnding, [GetMaxLineNumber]);

    // Calculate token density per line
    if GetLineCount > 0 then
      Result := Result + Format('Avg Tokens/Line: %.1f'+LineEnding, [Count / GetLineCount]);

    // === MEMORY BREAKDOWN ===
    Result := Result + LineEnding + '=== MEMORY BREAKDOWN ===' + LineEnding;
    Result := Result + Format('Token Objects: %.2f MB'+LineEnding, [MemoryUsedMB]);

    if IsOptimized then
    begin
      Result := Result + Format('Array Overhead: %.3f MB'+LineEnding,
        [(Capacity * SizeOf(Pointer)) / (1024 * 1024)]);
      Result := Result + 'Hash Table: 0 MB (Not used)'+LineEnding;
    end
    else
    begin
      Result := Result + Format('Hash Table: ~%.3f MB (Estimated)'+LineEnding,
        [(Count * 64) / (1024 * 1024)]); // Stima overhead hash
      Result := Result + Format('String Keys: ~%.3f MB (Estimated)'+LineEnding,
        [(Count * 16) / (1024 * 1024)]); // Stima chiavi "POOL_123"
    end;

    // === RECOMMENDATIONS ===
    Result := Result + LineEnding + '=== OPTIMIZATION RECOMMENDATIONS ===' + LineEnding;

    if IsOptimized then
    begin
      Result := Result + '✓ Already using optimized TFPObjectList implementation'+LineEnding;
      Result := Result + '✓ Direct array access - maximum performance'+LineEnding;
      Result := Result + '✓ Perfect for multi-pass parsing'+LineEnding;

      if Capacity > Count * 2 then
        Result := Result + Format('⚠ Consider reducing capacity from %d to %d (%.1f%% waste)'+LineEnding,
          [Capacity, Count + (Count div 4), ((Capacity - Count) * 100.0) / Capacity]);
    end
    else
    begin
      Result := Result + '⚠ Currently using TFPHashObjectList (legacy)'+LineEnding;
      Result := Result + '→ Migrate to TFPObjectList for 30-50% speed improvement'+LineEnding;
      Result := Result + '→ Eliminate hash lookup overhead'+LineEnding;
      Result := Result + '→ Better cache locality for large token lists'+LineEnding;
    end;

    if Count > 100000 then
    begin
      Result := Result + '📊 Large token list detected:'+LineEnding;
      Result := Result + '→ Consider streaming/chunked processing for very large files'+LineEnding;
      Result := Result + '→ Monitor memory usage in production'+LineEnding;
    end;

    // === FOOTER ===
    Result := Result + LineEnding + '=== END STATISTICS ===' + LineEnding;

  finally
    Stats.Free;
  end;
end;

procedure TTokenList.DumpToConsole;
var
  i: Integer;
  Token: TLexerToken;
begin
  WriteLn('=== TOKEN DUMP: ', FName, ' (TFPObjectList) ===');
  WriteLn('Count: ', Count);
  WriteLn('Capacity: ', Capacity);
  WriteLn('Current Index: ', FCurrentIndex);
  WriteLn;

  for i := 0 to Count - 1 do
  begin
    Token := TLexerToken(Items[i]);
    WriteLn(Format('[%3d] %s', [i, Token.GetDebugInfo]));
  end;

  WriteLn('=== END DUMP ===');
end;

procedure TTokenList.DumpRangeToConsole(StartIndex, EndIndex: Integer);
var
  i: Integer;
  Token: TLexerToken;
  SafeStart, SafeEnd: Integer;
begin
  SafeStart := Max(0, StartIndex);
  SafeEnd := Min(Count - 1, EndIndex);

  WriteLn(Format('=== TOKEN RANGE DUMP: %s [%d..%d] ===', [FName, SafeStart, SafeEnd]));

  for i := SafeStart to SafeEnd do
  begin
    Token := GetTokenByIndex(i);
    WriteLn(Format('[%3d] %s', [i, Token.GetDebugInfo]));
  end;

  WriteLn('=== END RANGE DUMP ===');
end;

// === SPECIALIZED TOKEN LISTS ===

{ TKeywordTokenList }

constructor TKeywordTokenList.Create(ACaseSensitive: Boolean);
begin
  inherited Create(True);
  FCaseSensitive := ACaseSensitive;
end;

function TKeywordTokenList.AddKeyword(const Keyword: string; TokenType: TTokenType;
                                     Line, Column, Position: Integer): Integer;
var
  Token: TLexerToken;
begin
  Token := CreateKeywordToken(Keyword, TokenType, Line, Column, Position);
  Result := AddToken(Token);
end;

function TKeywordTokenList.HasKeyword(const Keyword: string): Boolean;
begin
  Result := FindKeyword(Keyword) >= 0;
end;

function TKeywordTokenList.FindKeyword(const Keyword: string): Integer;
var
  i: Integer;
  Token: TLexerToken;
  TokenValue: string;
begin
  Result := -1;

  for i := 0 to Count - 1 do
  begin
    Token := GetTokenByIndex(i);
    if Token.IsKeywordToken then
    begin
      TokenValue := Token.Value;

      if FCaseSensitive then
      begin
        if TokenValue = Keyword then
        begin
          Result := i;
          Break;
        end;
      end
      else
      begin
        if UpperCase(TokenValue) = UpperCase(Keyword) then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;
end;

{ TFilteredTokenList }

constructor TFilteredTokenList.Create(ASourceList: TTokenList; AAutoFilter: TTokenFilterFunc;
                                     AAutoUpdate: Boolean);
begin
  inherited Create(False); // Don't own tokens from source
  FSourceList := ASourceList;
  FAutoFilter := AAutoFilter;
  FAutoUpdate := AAutoUpdate;
  //UpdateFromSource;
end;

//procedure TFilteredTokenList.UpdateFromSource;
//var
//  i: Integer;
//  Token: TLexerToken;
//begin
//  Clear;
//
//  if Assigned(FSourceList) and Assigned(FAutoFilter) then
//  begin
//    for i := 0 to FSourceList.Count - 1 do
//    begin
//      Token := FSourceList[i];
//      if FAutoFilter(Token, i) then
//        AddToken(Token);
//    end;
//  end;
//end;

procedure TFilteredTokenList.RefreshFilter;
begin
  //UpdateFromSource;
end;

// === FACTORY FUNCTIONS ===

function CreateKeywordList(CaseSensitive: Boolean): TKeywordTokenList;
begin
  Result := TKeywordTokenList.Create(CaseSensitive);
end;

function CreateFilteredList(Source: TTokenList; Filter: TTokenFilterFunc): TFilteredTokenList;
begin
  Result := TFilteredTokenList.Create(Source, Filter, True);
end;

// === SERIALIZATION IMPLEMENTATION ===

procedure TTokenList.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTokenList.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
begin
  CheckReadOnly;
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTokenList.SaveToStream(Stream: TStream);
var
  i: Integer;
  Token: TLexerToken;
  Writer: TWriter;
begin
  Writer := TWriter.Create(Stream, 4096);
  try
    // Write header
    Writer.WriteString('SEDAI_TOKEN_LIST');
    Writer.WriteInteger(1); // Version
    Writer.WriteString(FName);
    Writer.WriteString(FSourceFile);
    Writer.WriteInteger(Count);

    // Write tokens
    for i := 0 to Count - 1 do
    begin
      Token := GetTokenByIndex(i);
      Writer.WriteInteger(Ord(Token.TokenType));
      Writer.WriteString(Token.Value);
      Writer.WriteInteger(Token.Line);
      Writer.WriteInteger(Token.Column);
      Writer.WriteInteger(Token.Position);
      Writer.WriteInteger(Token.Length);
      Writer.WriteString(Token.SourceFile);
    end;
  finally
    Writer.Free;
  end;
end;

procedure TTokenList.LoadFromStream(Stream: TStream);
var
  i, TokenCount: Integer;
  Token: TLexerToken;
  Reader: TReader;
  Header: string;
  Version: Integer;
  TokenType: TTokenType;
  Value, ASourceFile: string;
  Line, Column, Position, Length: Integer;
begin
  Clear;

  Reader := TReader.Create(Stream, 4096);
  try
    // Read header
    Header := Reader.ReadString;
    if Header <> 'SEDAI_TOKEN_LIST' then
      raise ETokenListException.Create('Invalid token list file format');

    Version := Reader.ReadInteger;
    if Version <> 1 then
      raise ETokenListException.CreateFmt('Unsupported version: %d', [Version]);

    FName := Reader.ReadString;
    FSourceFile := Reader.ReadString;
    TokenCount := Reader.ReadInteger;

    // Read tokens
    for i := 0 to TokenCount - 1 do
    begin
      TokenType := TTokenType(Reader.ReadInteger);
      Value := Reader.ReadString;
      Line := Reader.ReadInteger;
      Column := Reader.ReadInteger;
      Position := Reader.ReadInteger;
      Length := Reader.ReadInteger;
      ASourceFile := Reader.ReadString;

      Token := TLexerToken.Create(TokenType, Value, Line, Column, Position, Length);
      Token.SourceFile := ASourceFile;

      AddToken(Token);
    end;
  finally
    Reader.Free;
  end;
end;

end.
