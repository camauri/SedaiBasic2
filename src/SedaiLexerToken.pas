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
unit SedaiLexerToken;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, TypInfo,
  SedaiLexerTypes;

type
  // Re-declare callback type here to avoid circular dependency
  TTokenValueExtractor = function(Context: Pointer; RecIndex: Integer): string;

type
  // Forward declaration for dependency injection
  TKeywordInfo = class;

  { TLexerToken - Main class to represent a token }
  TLexerToken = class
  private
    FTokenType: TTokenType;
    FValue: string;
    FLine: Integer;
    FColumn: Integer;
    FPosition: Integer;
    FLength: Integer;
    FKeywordInfo: TKeywordInfo; // Optional keyword reference
    FSourceFile: string;        // Source file name (optional)

    // Lazy evaluation flags for performance
    FDisplayStringCached: string;
    FDisplayStringValid: Boolean;

    // LAZY VALUE EXTRACTION from source buffer
    FValueExtracted: Boolean;     // True if FValue has been extracted
    FExtractorContext: Pointer;   // Context for extractor function
    FExtractorFunc: TTokenValueExtractor;  // Function to extract value
    FTokenRecIndex: Integer;      // Index in token record pool

    FHasConstraints: Boolean;
    function GetValue: string;

  protected
    procedure InvalidateCache; virtual;
    function BuildDisplayString: string; virtual;

  public
    constructor Create(ATokenType: TTokenType; const AValue: string;
                      ALine, AColumn, APosition, ALength: Integer;
                      AKeywordInfo: TKeywordInfo = nil); overload;

    constructor Create(ATokenType: TTokenType; const AValue: string;
                      ALine, AColumn, APosition: Integer;
                      AKeywordInfo: TKeywordInfo = nil); overload;

    constructor CreateSimple(ATokenType: TTokenType; const AValue: string); overload;

    destructor Destroy; override;

    // === TYPE IDENTIFICATION METHODS ===
    function IsKeywordToken: Boolean; virtual;
    function IsNumericToken: Boolean; virtual;
    function IsStringToken: Boolean; virtual;
    function IsCommentToken: Boolean; virtual;
    function IsOperatorToken: Boolean; virtual;
    function IsDelimiterToken: Boolean; virtual;
    function IsWhitespaceToken: Boolean; virtual;
    function IsControlFlowToken: Boolean; virtual;
    function IsIdentifierToken: Boolean; virtual;
    function IsErrorToken: Boolean; virtual;

    // === VALUE CONVERSION METHODS ===
    function AsInteger: Integer; virtual;
    function AsFloat: Double; virtual;
    function AsBoolean: Boolean; virtual;
    function AsString: string; virtual;

    // Safe conversions with defaults
    function AsIntegerDef(const DefaultValue: Integer = 0): Integer; virtual;
    function AsFloatDef(const DefaultValue: Double = 0.0): Double; virtual;
    function AsBooleanDef(const DefaultValue: Boolean = False): Boolean; virtual;

    // === VALIDATION METHODS ===
    function IsValidNumeric: Boolean; virtual;
    function IsValidIdentifier: Boolean; virtual;
    function IsValidString: Boolean; virtual;
    function IsValidLineNumber: Boolean; virtual;

    // === COMPARISON METHODS ===
    function Equals(Other: TLexerToken): Boolean; virtual;
    function SameType(Other: TLexerToken): Boolean; virtual;
    function SameValue(Other: TLexerToken): Boolean; virtual;
    function SamePosition(Other: TLexerToken): Boolean; virtual;

    // === UTILITY METHODS ===
    function GetDisplayString: string; virtual;
    function GetDebugInfo: string; virtual;
    function GetLocationString: string; virtual;
    function Clone: TLexerToken; virtual;
    function CloneWithNewValue(const NewValue: string): TLexerToken; virtual;
    function CloneWithNewType(NewType: TTokenType): TLexerToken; virtual;

    // === FORMATTING METHODS ===
    function ToString: string; override;
    function ToShortString: string; virtual;
    function ToDebugString: string; virtual;

    // === POSITION AND NAVIGATION ===
    function GetEndPosition: Integer; virtual;
    function GetEndColumn: Integer; virtual;
    function ContainsPosition(Pos: Integer): Boolean; virtual;
    function ContainsLocation(ALine, AColumn: Integer): Boolean; virtual;

    // === PROPERTIES ===
    property TokenType: TTokenType read FTokenType write FTokenType;
    property Value: string read GetValue write FValue;
    property Line: Integer read FLine write FLine;
    property Column: Integer read FColumn write FColumn;
    property Position: Integer read FPosition write FPosition;
    property Length: Integer read FLength write FLength;
    property KeywordInfo: TKeywordInfo read FKeywordInfo write FKeywordInfo;
    property SourceFile: string read FSourceFile write FSourceFile;

    // INTERNAL: Lazy extraction setup (used by lexer)
    procedure SetupLazyExtraction(ExtractorCtx: Pointer; ExtractorFn: TTokenValueExtractor; RecIdx: Integer);

    // Read-only computed properties
    property EndPosition: Integer read GetEndPosition;
    property EndColumn: Integer read GetEndColumn;
    property DisplayString: string read GetDisplayString;
    property DebugInfo: string read GetDebugInfo;
    property LocationString: string read GetLocationString;

    property HasConstraints: Boolean read FHasConstraints write FHasConstraints;
  end;

  { TKeywordInfo - Keyword information class }
  TKeywordInfo = class
  private
    FKeyword: string;
    FTokenType: TTokenType;
    FKeywordLength: Integer;
    FDescription: string;
    FCategory: string;
    FIsDeprecated: Boolean;
    FMinLanguageVersion: string;

  public
    constructor Create(const AKeyword: string; ATokenType: TTokenType;
                      const ADescription: string = ''); overload;

    constructor CreateWithCategory(const AKeyword: string; ATokenType: TTokenType;
                                  const ACategory, ADescription: string); overload;

    // === VIRTUAL METHODS FOR EXTENSIBILITY ===
    function GetDisplayName: string; virtual;
    function GetFullDescription: string; virtual;
    function IsCommentKeyword: Boolean; virtual;
    function IsControlFlowKeyword: Boolean; virtual;
    function RequiresSpecialHandling: Boolean; virtual;
    function IsValidInContext(const Context: string): Boolean; virtual;

    // === COMPARISON METHODS ===
    function Equals(Other: TKeywordInfo): Boolean; virtual;
    function MatchesText(const Text: string; CaseSensitive: Boolean = False): Boolean; virtual;

    // === UTILITY METHODS ===
    function ToString: string; override;
    function Clone: TKeywordInfo; virtual;

    // === PROPERTIES ===
    property Keyword: string read FKeyword write FKeyword;
    property TokenType: TTokenType read FTokenType write FTokenType;
    property KeywordLength: Integer read FKeywordLength;
    property Description: string read FDescription write FDescription;
    property Category: string read FCategory write FCategory;
    property IsDeprecated: Boolean read FIsDeprecated write FIsDeprecated;
    property MinLanguageVersion: string read FMinLanguageVersion write FMinLanguageVersion;
  end;

  // === SUPPORT TYPES ===
  TTokenArray = array of TLexerToken;
  TTokenCompareFunc = function(Token1, Token2: TLexerToken): Integer;
  TTokenFilterFunc = function(Token: TLexerToken): Boolean;

  // === SPECIALIZED EXCEPTIONS ===
  ETokenException = class(Exception);
  ETokenConversionException = class(ETokenException);
  ETokenValidationException = class(ETokenException);
  ETokenPositionException = class(ETokenException);

// === GLOBAL UTILITY FUNCTIONS ===

// Factory functions for quick token creation
function CreateKeywordToken(const Keyword: string; TokenType: TTokenType;
                           Line, Column, Position: Integer): TLexerToken;
function CreateNumberToken(const Number: string; Line, Column, Position: Integer): TLexerToken;
function CreateStringToken(const Text: string; Line, Column, Position: Integer): TLexerToken;
function CreateIdentifierToken(const Identifier: string; Line, Column, Position: Integer): TLexerToken;
function CreateOperatorToken(const Op: string; Line, Column, Position: Integer): TLexerToken;

// Validation functions
function IsValidBasicIdentifier(const Identifier: string): Boolean;
function IsValidBasicNumber(const Number: string): Boolean;
function IsValidBasicString(const Text: string): Boolean;

// Safe conversion functions
function TryTokenToInteger(Token: TLexerToken; out Value: Integer): Boolean;
function TryTokenToFloat(Token: TLexerToken; out Value: Double): Boolean;
function TryTokenToBoolean(Token: TLexerToken; out Value: Boolean): Boolean;

implementation

{ TLexerToken }

constructor TLexerToken.Create(ATokenType: TTokenType; const AValue: string;
                              ALine, AColumn, APosition, ALength: Integer;
                              AKeywordInfo: TKeywordInfo);
begin
  inherited Create;
  FTokenType := ATokenType;
  FValue := AValue;
  FLine := ALine;
  FColumn := AColumn;
  FPosition := APosition;
  FLength := ALength;
  FKeywordInfo := AKeywordInfo;
  FSourceFile := '';
  FDisplayStringValid := False;
  FValueExtracted := (AValue <> '');  // If value provided, mark as extracted
  FExtractorContext := nil;
  FExtractorFunc := nil;
  FTokenRecIndex := -1;
end;

constructor TLexerToken.Create(ATokenType: TTokenType; const AValue: string;
                              ALine, AColumn, APosition: Integer;
                              AKeywordInfo: TKeywordInfo);
begin
  Create(ATokenType, AValue, ALine, AColumn, APosition, System.Length(AValue), AKeywordInfo);
end;

constructor TLexerToken.CreateSimple(ATokenType: TTokenType; const AValue: string);
begin
  Create(ATokenType, AValue, 1, 1, 1, System.Length(AValue), nil);
end;

destructor TLexerToken.Destroy;
begin
  // Don't free FKeywordInfo as it's managed by the registry
  inherited Destroy;
end;

procedure TLexerToken.SetupLazyExtraction(ExtractorCtx: Pointer; ExtractorFn: TTokenValueExtractor; RecIdx: Integer);
begin
  FValueExtracted := False;
  FExtractorContext := ExtractorCtx;
  FExtractorFunc := ExtractorFn;
  FTokenRecIndex := RecIdx;
end;

procedure TLexerToken.InvalidateCache;
begin
  FDisplayStringValid := False;
end;

function TLexerToken.BuildDisplayString: string;
begin
  Result := Format('%s: "%s" [%d:%d]', [
    GetTokenTypeName(FTokenType),
    FValue,
    FLine,
    FColumn
  ]);
end;

// === TYPE IDENTIFICATION METHODS ===

function TLexerToken.IsKeywordToken: Boolean;
begin
  Result := Assigned(FKeywordInfo) or IsKeywordTokenType(FTokenType);
end;

function TLexerToken.IsNumericToken: Boolean;
begin
  Result := IsNumericTokenType(FTokenType);
end;

function TLexerToken.IsStringToken: Boolean;
begin
  Result := FTokenType = ttStringLiteral;
end;

function TLexerToken.IsCommentToken: Boolean;
begin
  Result := IsCommentTokenType(FTokenType);
end;

function TLexerToken.IsOperatorToken: Boolean;
begin
  Result := IsOperatorTokenType(FTokenType);
end;

function TLexerToken.IsDelimiterToken: Boolean;
begin
  Result := IsDelimiterTokenType(FTokenType);
end;

function TLexerToken.IsWhitespaceToken: Boolean;
begin
  Result := IsWhitespaceTokenType(FTokenType);
end;

function TLexerToken.IsControlFlowToken: Boolean;
begin
  Result := IsControlFlowTokenType(FTokenType);
end;

function TLexerToken.IsIdentifierToken: Boolean;
begin
  Result := FTokenType = ttIdentifier;
end;

function TLexerToken.IsErrorToken: Boolean;
begin
  Result := FTokenType in [ttError, ttUnknown];
end;

// === VALUE CONVERSION METHODS ===

function TLexerToken.AsInteger: Integer;
begin
  if not TryStrToInt(FValue, Result) then
    raise ETokenConversionException.CreateFmt(
      'Cannot convert token value "%s" to integer at %s',
      [FValue, GetLocationString]);
end;

function TLexerToken.AsFloat: Double;
begin
  if not TryStrToFloat(FValue, Result) then
    raise ETokenConversionException.CreateFmt(
      'Cannot convert token value "%s" to float at %s',
      [FValue, GetLocationString]);
end;

function TLexerToken.AsBoolean: Boolean;
begin
  if not TryTokenToBoolean(Self, Result) then
    raise ETokenConversionException.CreateFmt(
      'Cannot convert token value "%s" to boolean at %s',
      [FValue, GetLocationString]);
end;

function TLexerToken.AsString: string;
begin
  Result := FValue;

  // If string literal, remove quotes
  if IsStringToken and (System.Length(Result) >= 2) then
  begin
    if (Result[1] = '"') and (Result[System.Length(Result)] = '"') then
      Result := Copy(Result, 2, System.Length(Result) - 2);
  end;
end;

function TLexerToken.AsIntegerDef(const DefaultValue: Integer): Integer;
begin
  if not TryStrToInt(FValue, Result) then
    Result := DefaultValue;
end;

function TLexerToken.AsFloatDef(const DefaultValue: Double): Double;
begin
  if not TryStrToFloat(FValue, Result) then
    Result := DefaultValue;
end;

function TLexerToken.AsBooleanDef(const DefaultValue: Boolean): Boolean;
begin
  if not TryTokenToBoolean(Self, Result) then
    Result := DefaultValue;
end;

function TLexerToken.GetValue: string;
begin
  // LAZY EXTRACTION: Extract from source buffer if not yet done
  if not FValueExtracted then
  begin
    if Assigned(FExtractorFunc) then
    begin
      // Call extractor function to get value from source buffer
      FValue := FExtractorFunc(FExtractorContext, FTokenRecIndex);
      FValueExtracted := True;
    end;
  end;

  if FValue <> '' then
  begin
    Result := FValue;
    Exit;
  end;

  // Lazy evaluation for static tokens
  case TokenType of
    ttOpAdd: Result := '+';
    ttOpSub: Result := '-';
    ttOpMul: Result := '*';
    ttOpDiv: Result := '/';
    ttOpEq: Result := '=';
    // etc...
    else
      Result := FValue;
  end;

  FValue := Result; // Cache for future calls
end;

// === VALIDATION METHODS ===

function TLexerToken.IsValidNumeric: Boolean;
var
  Dummy: Double;
begin
  Result := TryStrToFloat(FValue, Dummy);
end;

function TLexerToken.IsValidIdentifier: Boolean;
begin
  Result := IsValidBasicIdentifier(FValue);
end;

function TLexerToken.IsValidString: Boolean;
begin
  Result := IsValidBasicString(FValue);
end;

function TLexerToken.IsValidLineNumber: Boolean;
var
  Num: Integer;
begin
  Result := TryStrToInt(FValue, Num) and (Num > 0) and (Num <= 65535);
end;

// === COMPARISON METHODS ===

function TLexerToken.Equals(Other: TLexerToken): Boolean;
begin
  Result := Assigned(Other) and
            (FTokenType = Other.FTokenType) and
            (FValue = Other.FValue) and
            (FLine = Other.FLine) and
            (FColumn = Other.FColumn) and
            (FPosition = Other.FPosition);
end;

function TLexerToken.SameType(Other: TLexerToken): Boolean;
begin
  Result := Assigned(Other) and (FTokenType = Other.FTokenType);
end;

function TLexerToken.SameValue(Other: TLexerToken): Boolean;
begin
  Result := Assigned(Other) and (FValue = Other.FValue);
end;

function TLexerToken.SamePosition(Other: TLexerToken): Boolean;
begin
  Result := Assigned(Other) and
            (FLine = Other.FLine) and
            (FColumn = Other.FColumn) and
            (FPosition = Other.FPosition);
end;

// === UTILITY METHODS ===

function TLexerToken.GetDisplayString: string;
begin
  if not FDisplayStringValid then
  begin
    FDisplayStringCached := BuildDisplayString;
    FDisplayStringValid := True;
  end;
  Result := FDisplayStringCached;
end;

function TLexerToken.GetDebugInfo: string;
begin
  Result := Format('Token[%s] = "%s" at %s (pos=%d, len=%d)', [
    GetTokenTypeName(FTokenType),
    FValue,
    GetLocationString,
    FPosition,
    FLength
  ]);

  if Assigned(FKeywordInfo) then
    Result := Result + Format(' [Keyword: %s]', [FKeywordInfo.Keyword]);
end;

function TLexerToken.GetLocationString: string;
begin
  if FSourceFile <> '' then
    Result := Format('%s:%d:%d', [FSourceFile, FLine, FColumn])
  else
    Result := Format('%d:%d', [FLine, FColumn]);
end;

function TLexerToken.Clone: TLexerToken;
begin
  Result := TLexerToken.Create(FTokenType, FValue, FLine, FColumn,
                              FPosition, FLength, FKeywordInfo);
  Result.FSourceFile := FSourceFile;
end;

function TLexerToken.CloneWithNewValue(const NewValue: string): TLexerToken;
begin
  Result := Clone;
  Result.FValue := NewValue;
  Result.FLength := System.Length(NewValue);
  Result.InvalidateCache;
end;

function TLexerToken.CloneWithNewType(NewType: TTokenType): TLexerToken;
begin
  Result := Clone;
  Result.FTokenType := NewType;
  Result.InvalidateCache;
end;

// === FORMATTING METHODS ===

function TLexerToken.ToString: string;
begin
  Result := GetDisplayString;
end;

function TLexerToken.ToShortString: string;
begin
  Result := Format('%s:"%s"', [GetTokenTypeName(FTokenType), FValue]);
end;

function TLexerToken.ToDebugString: string;
begin
  Result := GetDebugInfo;
end;

// === POSITION AND NAVIGATION ===

function TLexerToken.GetEndPosition: Integer;
begin
  Result := FPosition + FLength - 1;
end;

function TLexerToken.GetEndColumn: Integer;
begin
  Result := FColumn + FLength - 1;
end;

function TLexerToken.ContainsPosition(Pos: Integer): Boolean;
begin
  Result := (Pos >= FPosition) and (Pos < FPosition + FLength);
end;

function TLexerToken.ContainsLocation(ALine, AColumn: Integer): Boolean;
begin
  Result := (ALine = FLine) and
            (AColumn >= FColumn) and
            (AColumn < FColumn + FLength);
end;

{ TKeywordInfo }

constructor TKeywordInfo.Create(const AKeyword: string; ATokenType: TTokenType;
                               const ADescription: string);
begin
  inherited Create;
  FKeyword := UpperCase(AKeyword);
  FTokenType := ATokenType;
  FKeywordLength := System.Length(AKeyword);
  FDescription := ADescription;
  FCategory := GetTokenCategory(ATokenType);
  FIsDeprecated := False;
  FMinLanguageVersion := '';
end;

constructor TKeywordInfo.CreateWithCategory(const AKeyword: string; ATokenType: TTokenType;
                                           const ACategory, ADescription: string);
begin
  Create(AKeyword, ATokenType, ADescription);
  FCategory := ACategory;
end;

function TKeywordInfo.GetDisplayName: string;
begin
  Result := FKeyword;
  if FIsDeprecated then
    Result := Result + ' (deprecated)';
end;

function TKeywordInfo.GetFullDescription: string;
begin
  Result := FDescription;
  if Result = '' then
    Result := Format('%s keyword', [FCategory]);

  if FIsDeprecated then
    Result := Result + ' [DEPRECATED]';

  if FMinLanguageVersion <> '' then
    Result := Result + Format(' (requires v%s+)', [FMinLanguageVersion]);
end;

function TKeywordInfo.IsCommentKeyword: Boolean;
begin
  Result := FTokenType = ttCommentRemark;
end;

function TKeywordInfo.IsControlFlowKeyword: Boolean;
begin
  Result := IsControlFlowTokenType(FTokenType);
end;

function TKeywordInfo.RequiresSpecialHandling: Boolean;
begin
  Result := IsCommentKeyword or IsControlFlowKeyword;
end;

function TKeywordInfo.IsValidInContext(const Context: string): Boolean;
begin
  // Base implementation - can be overridden in subclasses
  Result := not FIsDeprecated;
end;

function TKeywordInfo.Equals(Other: TKeywordInfo): Boolean;
begin
  Result := Assigned(Other) and
            (FKeyword = Other.FKeyword) and
            (FTokenType = Other.FTokenType);
end;

function TKeywordInfo.MatchesText(const Text: string; CaseSensitive: Boolean): Boolean;
begin
  if CaseSensitive then
    Result := FKeyword = Text
  else
    Result := FKeyword = UpperCase(Text);
end;

function TKeywordInfo.ToString: string;
begin
  Result := Format('%s (%s)', [FKeyword, GetTokenTypeName(FTokenType)]);
end;

function TKeywordInfo.Clone: TKeywordInfo;
begin
  Result := TKeywordInfo.Create(FKeyword, FTokenType, FDescription);
  Result.FCategory := FCategory;
  Result.FIsDeprecated := FIsDeprecated;
  Result.FMinLanguageVersion := FMinLanguageVersion;
end;

// === GLOBAL UTILITY FUNCTIONS ===

function CreateKeywordToken(const Keyword: string; TokenType: TTokenType;
                           Line, Column, Position: Integer): TLexerToken;
begin
  Result := TLexerToken.Create(TokenType, Keyword, Line, Column, Position);
end;

function CreateNumberToken(const Number: string; Line, Column, Position: Integer): TLexerToken;
begin
  Result := TLexerToken.Create(ttNumber, Number, Line, Column, Position);
end;

function CreateStringToken(const Text: string; Line, Column, Position: Integer): TLexerToken;
begin
  Result := TLexerToken.Create(ttStringLiteral, Text, Line, Column, Position);
end;

function CreateIdentifierToken(const Identifier: string; Line, Column, Position: Integer): TLexerToken;
begin
  Result := TLexerToken.Create(ttIdentifier, Identifier, Line, Column, Position);
end;

function CreateOperatorToken(const Op: string; Line, Column, Position: Integer): TLexerToken;
begin
  Result := TLexerToken.Create(ttOperator, Op, Line, Column, Position);
end;

function IsValidBasicIdentifier(const Identifier: string): Boolean;
var
  i: Integer;
  Ch: Char;
begin
  Result := False;

  if Identifier = '' then Exit;

  // First character must be letter or underscore
  Ch := Identifier[1];
  if not (Ch in ['A'..'Z', 'a'..'z', '_']) then Exit;

  // Other characters: letters, digits, underscore
  for i := 2 to System.Length(Identifier) do
  begin
    Ch := Identifier[i];
    if not (Ch in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then Exit;
  end;

  Result := True;
end;

function IsValidBasicNumber(const Number: string): Boolean;
var
  Dummy: Double;
begin
  Result := TryStrToFloat(Number, Dummy);
end;

function IsValidBasicString(const Text: string): Boolean;
begin
  // Basic validation for BASIC strings
  Result := (System.Length(Text) >= 2) and
            (Text[1] = '"') and
            (Text[System.Length(Text)] = '"');
end;

function TryTokenToInteger(Token: TLexerToken; out Value: Integer): Boolean;
begin
  Result := Assigned(Token) and TryStrToInt(Token.Value, Value);
end;

function TryTokenToFloat(Token: TLexerToken; out Value: Double): Boolean;
begin
  Result := Assigned(Token) and TryStrToFloat(Token.Value, Value);
end;

function TryTokenToBoolean(Token: TLexerToken; out Value: Boolean): Boolean;
var
  UpperValue: string;
begin
  Result := False;
  if not Assigned(Token) then Exit;

  UpperValue := UpperCase(Token.Value);

  if (UpperValue = 'TRUE') or (UpperValue = '1') or (UpperValue = 'YES') or (UpperValue = 'ON') then
  begin
    Value := True;
    Result := True;
  end
  else if (UpperValue = 'FALSE') or (UpperValue = '0') or (UpperValue = 'NO') or (UpperValue = 'OFF') then
  begin
    Value := False;
    Result := True;
  end;
end;


end.
