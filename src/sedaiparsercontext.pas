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
unit SedaiParserContext;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils,
  SedaiLexerTypes, SedaiLexerToken, SedaiTokenList,
  SedaiParserTypes, SedaiParserErrors, SedaiParserResults;

type
  { TParserContext - Streamlined parser state and context management }
  TParserContext = class
  private
    FTokenList: TTokenList;
    FCurrentIndex: Integer;
    FErrors: TParserErrorList;
    FCache: TPackratCache;
    FParseRules: array[TTokenType] of TParseRule;
    FInPanicMode: Boolean;
    FDebugMode: Boolean;
    FRecursionDepth: Integer;
    FMaxRecursionLimit: Integer;

    // Performance tracking (minimal)
    FNodesCreated: Integer;

    // BASIC line number tracking
    FCurrentBasicLineNumber: Integer;
    FCurrentBasicSourceLine: string;

  public
    constructor Create(ATokenList: TTokenList);
    destructor Destroy; override;

    // === TOKEN NAVIGATION (hot path - inline where possible) ===
    function GetCurrentToken: TLexerToken; inline;
    function GetPreviousToken: TLexerToken; inline;
    function PeekToken(Offset: Integer = 1): TLexerToken; inline;
    function PeekNext: TLexerToken; inline;
    function Advance: TLexerToken; inline;
    function IsAtEnd: Boolean; inline;
    function Match(TokenType: TTokenType): Boolean; inline; overload;
    function Match(const TokenTypes: array of TTokenType): Boolean; overload;
    function Consume(TokenType: TTokenType; const ErrorMessage: string): TLexerToken;
    function Check(TokenType: TTokenType): Boolean; inline;
    function CheckAny(const TokenTypes: array of TTokenType): Boolean;

    // === POSITION MANAGEMENT ===
    procedure SavePosition(out SavedIndex: Integer); inline;
    procedure RestorePosition(SavedIndex: Integer); inline;

    // === RECURSION CONTROL ===
    function EnterRecursion: Boolean; inline;
    procedure ExitRecursion; inline;

    // === ERROR HANDLING (essential only) ===
    procedure AddError(const Message: string; Token: TLexerToken); overload;
    procedure AddError(const Message: string; Line, Column, Position: Integer); overload;
    procedure AddWarning(const Message: string; Token: TLexerToken);
    function HasErrors: Boolean; inline;
    function CanContinue: Boolean; inline;
    procedure Synchronize; // Error recovery
    procedure EnterPanicMode; inline;
    procedure ExitPanicMode; inline;

    // === PARSE RULES MANAGEMENT (hot path) ===
    procedure SetParseRule(TokenType: TTokenType; const Rule: TParseRule); inline;
    function GetParseRule(TokenType: TTokenType): TParseRule; inline;
    function HasParseRule(TokenType: TTokenType): Boolean; inline;

    // === PACKRAT CACHE (hot path) ===
    function GetCacheEntry(const RuleName: string; TokenIndex: Integer): TPackratCacheEntry; inline;
    procedure SetCacheEntry(const RuleName: string; TokenIndex: Integer; Entry: TPackratCacheEntry); inline;
    procedure ClearCache;

    // === PERFORMANCE TRACKING (minimal) ===
    procedure IncrementNodesCreated; inline;

    // === CONFIGURATION ===
    procedure SetMaxRecursionLimit(Limit: Integer);

    // === ESSENTIAL VALIDATION ===
    function IsValid: Boolean; inline;

    // === MINIMAL DEBUG ===
    function GetDebugInfo: string;

    // === BASIC LINE NUMBER TRACKING ===
    procedure SetCurrentBasicLine(LineNumber: Integer; const SourceLine: string);

    // === PROPERTIES ===
    property TokenList: TTokenList read FTokenList;
    property CurrentIndex: Integer read FCurrentIndex write FCurrentIndex;
    property CurrentToken: TLexerToken read GetCurrentToken;
    property PreviousToken: TLexerToken read GetPreviousToken;
    property Errors: TParserErrorList read FErrors;
    property Cache: TPackratCache read FCache;
    property InPanicMode: Boolean read FInPanicMode;
    property DebugMode: Boolean read FDebugMode write FDebugMode;
    property RecursionDepth: Integer read FRecursionDepth;
    property MaxRecursionLimit: Integer read FMaxRecursionLimit write FMaxRecursionLimit;
    property NodesCreated: Integer read FNodesCreated;
    property CurrentBasicLineNumber: Integer read FCurrentBasicLineNumber;
    property CurrentBasicSourceLine: string read FCurrentBasicSourceLine;
  end;

  { TParserPositionGuard - Helper for automatic position restore

    Usage pattern - automatic restore on failure:

      var Guard: TParserPositionGuard;
      begin
        Guard.Init(Context);
        try
          // Try parsing something
          if not TrySomeParse(Context) then
            Exit(nil);  // Position automatically restored in finally

          // Success - commit the position change
          Guard.Commit;
          Result := SomeSuccessValue;
        finally
          Guard.Done;
        end;
      end;

    This replaces the manual pattern and ensures position is always restored
    unless explicitly committed.
  }
  TParserPositionGuard = object
    FContext: TParserContext;
    FSavedIndex: Integer;
    FCommitted: Boolean;
    procedure Init(Context: TParserContext); inline;
    procedure Commit; inline;
    procedure Done; inline;
  end;

implementation

uses
  Math, TypInfo;

{ TParserContext }

constructor TParserContext.Create(ATokenList: TTokenList);
var
  TokenType: TTokenType;
begin
  inherited Create;
  FTokenList := ATokenList;
  FCurrentIndex := 0;
  FErrors := TParserErrorList.Create;
  FCache := TPackratCache.Create;
  FInPanicMode := False;
  FDebugMode := False;
  FRecursionDepth := 0;
  FMaxRecursionLimit := 1000; // Default limit
  FNodesCreated := 0;
  FCurrentBasicLineNumber := 0;
  FCurrentBasicSourceLine := '';

  // Initialize parse rules array
  for TokenType := Low(TTokenType) to High(TTokenType) do
  begin
    FParseRules[TokenType].Prefix := nil;
    FParseRules[TokenType].Infix := nil;
    FParseRules[TokenType].Precedence := precNone;
  end;
end;

destructor TParserContext.Destroy;
begin
  FCache.Free;
  FErrors.Free;
  inherited Destroy;
end;

// === TOKEN NAVIGATION ===

function TParserContext.GetCurrentToken: TLexerToken;
begin
  if (FCurrentIndex >= 0) and (FCurrentIndex < FTokenList.Count) then
    Result := FTokenList.GetTokenDirect(FCurrentIndex)
  else
    Result := nil;
end;

function TParserContext.GetPreviousToken: TLexerToken;
begin
  if (FCurrentIndex > 0) and (FCurrentIndex - 1 < FTokenList.Count) then
    Result := FTokenList.GetTokenDirect(FCurrentIndex - 1)
  else
    Result := nil;
end;

function TParserContext.PeekToken(Offset: Integer): TLexerToken;
var
  Index: Integer;
begin
  Index := FCurrentIndex + Offset;
  if (Index >= 0) and (Index < FTokenList.Count) then
    Result := FTokenList.GetTokenDirect(Index)
  else
    Result := nil;
end;

function TParserContext.PeekNext: TLexerToken;
begin
  Result := PeekToken(1);
end;

function TParserContext.Advance: TLexerToken;
begin
  Result := GetCurrentToken;
  if not IsAtEnd then
    Inc(FCurrentIndex);
end;

function TParserContext.IsAtEnd: Boolean;
begin
  if not Assigned(FTokenList) then
    Result := True
  else
  begin
    Result := (FCurrentIndex >= FTokenList.Count) or
              (Assigned(GetCurrentToken) and (GetCurrentToken.TokenType = ttEndOfFile));
  end;
end;

function TParserContext.Match(TokenType: TTokenType): Boolean;
begin
  Result := Check(TokenType);
  if Result then
    Advance;
end;

function TParserContext.Match(const TokenTypes: array of TTokenType): Boolean;
var
  i: Integer;
  ACurrentToken: TLexerToken;
begin
  Result := False;
  ACurrentToken := GetCurrentToken;

  if Assigned(CurrentToken) then
  begin
    for i := Low(TokenTypes) to High(TokenTypes) do
    begin
      if ACurrentToken.TokenType = TokenTypes[i] then
      begin
        Result := True;
        Advance;
        Break;
      end;
    end;
  end;
end;

function TParserContext.Consume(TokenType: TTokenType; const ErrorMessage: string): TLexerToken;
begin
  if Check(TokenType) then
    Result := Advance
  else
  begin
    Result := GetCurrentToken;
    AddError(ErrorMessage, Result);
    EnterPanicMode;
  end;
end;

function TParserContext.Check(TokenType: TTokenType): Boolean;
var
  ACurrentToken: TLexerToken;
begin
  if IsAtEnd then
    Result := False
  else
  begin
    ACurrentToken := GetCurrentToken;
    Result := Assigned(ACurrentToken) and (ACurrentToken.TokenType = TokenType);
  end;
end;

function TParserContext.CheckAny(const TokenTypes: array of TTokenType): Boolean;
var
  i: Integer;
  ACurrentToken: TLexerToken;
begin
  Result := False;
  if IsAtEnd then
    Exit;

  ACurrentToken := GetCurrentToken;
  if not Assigned(ACurrentToken) then
    Exit;

  for i := Low(TokenTypes) to High(TokenTypes) do
  begin
    if ACurrentToken.TokenType = TokenTypes[i] then
    begin
      Result := True;
      Break;
    end;
  end;
end;

// === POSITION MANAGEMENT ===

procedure TParserContext.SavePosition(out SavedIndex: Integer);
begin
  SavedIndex := FCurrentIndex;
end;

procedure TParserContext.RestorePosition(SavedIndex: Integer);
begin
  if (SavedIndex >= 0) and (SavedIndex <= FTokenList.Count) then
    FCurrentIndex := SavedIndex;
end;

// === RECURSION CONTROL ===

function TParserContext.EnterRecursion: Boolean;
begin
  Inc(FRecursionDepth);
  Result := FRecursionDepth <= FMaxRecursionLimit;

  if not Result then
  begin
    AddError('Maximum recursion depth exceeded', GetCurrentToken);
    Dec(FRecursionDepth); // Don't count failed recursion
  end;
end;

procedure TParserContext.ExitRecursion;
begin
  if FRecursionDepth > 0 then
    Dec(FRecursionDepth);
end;

// === ERROR HANDLING ===

procedure TParserContext.AddError(const Message: string; Token: TLexerToken);
var
  ErrorIndex: Integer;
begin
  ErrorIndex := FErrors.AddError(Message, Token);
  // Set BASIC line number if tracked
  if (ErrorIndex >= 0) and (FCurrentBasicLineNumber > 0) then
  begin
    FErrors[ErrorIndex].BasicLineNumber := FCurrentBasicLineNumber;
    FErrors[ErrorIndex].SourceLine := FCurrentBasicSourceLine;
  end;
  {$IFDEF DEBUG}
  if FDebugMode then
    WriteLn('PARSER ERROR: ', Message);
  {$ENDIF}
end;

procedure TParserContext.AddError(const Message: string; Line, Column, Position: Integer);
var
  ErrorIndex: Integer;
begin
  ErrorIndex := FErrors.AddError(Message, Line, Column, Position);
  // Set BASIC line number if tracked
  if (ErrorIndex >= 0) and (FCurrentBasicLineNumber > 0) then
  begin
    FErrors[ErrorIndex].BasicLineNumber := FCurrentBasicLineNumber;
    FErrors[ErrorIndex].SourceLine := FCurrentBasicSourceLine;
  end;
  {$IFDEF DEBUG}
  if FDebugMode then
    WriteLn('PARSER ERROR: ', Message);
  {$ENDIF}
end;

procedure TParserContext.AddWarning(const Message: string; Token: TLexerToken);
begin
  FErrors.AddWarning(Message, Token);
  {$IFDEF DEBUG}
  if FDebugMode then
    WriteLn('PARSER WARNING: ', Message);
  {$ENDIF}
end;

function TParserContext.HasErrors: Boolean;
begin
  Result := FErrors.HasErrors;
end;

function TParserContext.CanContinue: Boolean;
begin
  Result := FErrors.CanContinue;
end;

procedure TParserContext.Synchronize;
var
  ACurrentToken: TLexerToken;
begin
  FInPanicMode := False;

  while not IsAtEnd do
  begin
    ACurrentToken := GetCurrentToken;

    // Stop at statement boundaries (essential recovery points only)
    if Assigned(ACurrentToken) then
    begin
      case ACurrentToken.TokenType of
        ttEndOfLine,           // End of line
        ttLineNumber,          // New line number
        ttConditionalIf,       // IF
        ttLoopBlockStart,      // FOR, WHILE, etc.
        ttOutputCommand,       // PRINT
        ttInputCommand,        // INPUT
        ttJumpKeyword:         // GOTO, GOSUB
          Exit;
      end;
    end;

    Advance;
  end;
end;

procedure TParserContext.EnterPanicMode;
begin
  FInPanicMode := True;
end;

procedure TParserContext.ExitPanicMode;
begin
  FInPanicMode := False;
end;

// === PARSE RULES MANAGEMENT ===

procedure TParserContext.SetParseRule(TokenType: TTokenType; const Rule: TParseRule);
begin
  FParseRules[TokenType] := Rule;
end;

function TParserContext.GetParseRule(TokenType: TTokenType): TParseRule;
begin
  Result := FParseRules[TokenType];
end;

function TParserContext.HasParseRule(TokenType: TTokenType): Boolean;
begin
  Result := Assigned(FParseRules[TokenType].Prefix) or Assigned(FParseRules[TokenType].Infix);
end;

// === PACKRAT CACHE ===

function TParserContext.GetCacheEntry(const RuleName: string; TokenIndex: Integer): TPackratCacheEntry;
begin
  Result := FCache.GetEntry(RuleName, TokenIndex);
end;

procedure TParserContext.SetCacheEntry(const RuleName: string; TokenIndex: Integer; Entry: TPackratCacheEntry);
begin
  FCache.SetEntry(RuleName, TokenIndex, Entry);
end;

procedure TParserContext.ClearCache;
begin
  FCache.ClearCache;
end;

// === PERFORMANCE TRACKING ===

procedure TParserContext.IncrementNodesCreated;
begin
  Inc(FNodesCreated);
end;

// === CONFIGURATION ===

procedure TParserContext.SetMaxRecursionLimit(Limit: Integer);
begin
  FMaxRecursionLimit := Max(10, Limit); // Minimum reasonable limit
end;

// === ESSENTIAL VALIDATION ===

function TParserContext.IsValid: Boolean;
begin
  Result := Assigned(FTokenList) and
            (FCurrentIndex >= 0) and
            (FRecursionDepth >= 0) and
            (FRecursionDepth <= FMaxRecursionLimit);
end;

// === MINIMAL DEBUG ===

function TParserContext.GetDebugInfo: string;
var
  Token: TLexerToken;
begin
  Token := GetCurrentToken;
  if Assigned(Token) then
    Result := Format('Context: Index=%d, Token=%s, Line=%d, Column=%d, Recursion=%d, Errors=%d, BasicLine=%d',
      [FCurrentIndex, Token.Value, Token.Line, Token.Column, FRecursionDepth, FErrors.Count, FCurrentBasicLineNumber])
  else
    Result := Format('Context: Index=%d, Token=<EOF>, Recursion=%d, Errors=%d, BasicLine=%d',
      [FCurrentIndex, FRecursionDepth, FErrors.Count, FCurrentBasicLineNumber]);
end;

// === BASIC LINE NUMBER TRACKING ===

procedure TParserContext.SetCurrentBasicLine(LineNumber: Integer; const SourceLine: string);
begin
  FCurrentBasicLineNumber := LineNumber;
  FCurrentBasicSourceLine := SourceLine;
end;

{ TParserPositionGuard }

procedure TParserPositionGuard.Init(Context: TParserContext);
begin
  FContext := Context;
  FCommitted := False;
  if Assigned(Context) then
    Context.SavePosition(FSavedIndex)
  else
    FSavedIndex := -1;
end;

procedure TParserPositionGuard.Commit;
begin
  FCommitted := True;
end;

procedure TParserPositionGuard.Done;
begin
  if not FCommitted and Assigned(FContext) and (FSavedIndex >= 0) then
    FContext.RestorePosition(FSavedIndex);
  FContext := nil;
end;

end.
