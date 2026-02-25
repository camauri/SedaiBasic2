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
unit SedaiLexerFSM;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, TypInfo, Math,
  SedaiLexerTypes, SedaiLexerToken, SedaiTokenList,
  SedaiKeywordTypes, SedaiKeywordRegistry, SedaiKeywordUtils,
  SedaiBasicKeywords, SedaiMemoryUtils, SedaiDateTimeUtils;

const
  MAX_TOKEN_LENGTH = 255;

type
  { TLexerFSM - Main Finite State Machine Lexer }
  TLexerFSM = class
  private
    // === POOL CONFIGURATION ===
    FTokensPerCharRatio: Double;
    FSafetyFactor: Double;
    FMinPoolSize: Integer;
    FMaxPoolSize: Integer;

    // === POOL STATISTICS ===
    FTokensAllocated: Int64;
    FTokensReused: Int64;
    FOverflowCount: Integer;

    // === CORE COMPONENTS ===
    //FTokenPool: TGenericTokenPool;
    FTokenList: TTokenList;
    FKeywordRegistry: TKeywordRegistry;
    FTransitionTable: TTransitionTable;
    FCharConfig: TCharacterConfig;
    FLexerOptions: TLexerOptions;

    // === ZERO-ALLOCATION TOKEN POOL ===
    FTokenRecPool: array of TTokenRec;  // Lightweight record pool
    FTokenRecCount: Integer;             // Current count
    FTokenRecCapacity: Integer;          // Pool capacity

    // === STATE MANAGEMENT ===
    FCurrentTokenIndex: Integer;
    FLastUsedTokenIndex: Integer;
    FEstimatedTokenCount: Integer;
    FSource: string;
    FSourceFile: string;
    FCurrentPosition: Integer;
    FCurrentLine: Integer;
    FCurrentColumn: Integer;
    FCurrentState: TLexerState;
    FTokenStart: Integer;
    FTokenStartLine: Integer;
    FTokenStartColumn: Integer;
    FTokenText: string;
    FAtLineStart: Boolean;
    FInComment: Boolean;

    // === STRING CACHE OPTIMIZATION ===
    FLastTokenString: string;
    FLastTokenLength: Integer;
    // === PERFORMANCE OPTIMIZATION ===
    FSourceLength: Integer;
    FCharClassCache: array[Char] of TCharClass;
    FCacheValid: Boolean;
    // === STATISTICS ===
    FTokensProcessed: Integer;
    FErrorsFound: Integer;
    {$IFDEF DEBUG}
    FProcessingTime: Double;
    {$ENDIF}
    FFSMTokenCount: Integer;

    {$IFDEF DEBUG}
    FDebugMode: Boolean;
    FVerboseLogging: Boolean;
    {$ENDIF}

    // === STRING BUFFER OPTIMIZATION ===
    FTokenBuffer: array[0..255] of Char;
    FTokenLength: Integer;

    function GetTokenAt(Index: Integer): TLexerToken;
    function HandleTokenOverflow: TLexerToken;
    function ParseNumberDigits: Boolean;
    procedure ResetTokenPool;
    procedure TokenBufferReset; inline;
    procedure TokenBufferAdd(C: Char); inline;
    function TokenBufferAsString: string; inline;
    procedure TokenBufferSetString(const S: string); inline;

    // === INTERNAL METHODS ===
    function GetCurrentChar: Char; inline;
    function PeekChar(Offset: Integer = 1): Char; inline;
    procedure AdvanceChar;
    function CreateToken(TokenType: TTokenType; KeywordInfo: TKeywordInfo = nil): TLexerToken; inline;
    function AllocTokenRec: Integer; inline;  // Returns index to new TTokenRec
    function GetTokenValue(const Rec: TTokenRec): string; inline;  // Lazy extract from source
    function ExtractTokenValue(RecIndex: Integer): string;  // PUBLIC: Extract value for token
    procedure ResetToken;
    procedure BuildTransitionTable;
    procedure InitializeCharacterConfig;
    procedure InitializeLexerOptions;
    procedure BuildCharClassCache;
    {$IFDEF DEBUG}
    procedure LogDebug(const Msg: string);
    procedure LogVerbose(const Msg: string);
    {$ENDIF}

    // === CHARACTER CLASSIFICATION ===
    function ClassifyChar(Ch: Char): TCharClass; inline;
    function IsLetter(Ch: Char): Boolean; virtual;
    function IsDigit(Ch: Char): Boolean; virtual;
    function IsOperator(Ch: Char): Boolean; virtual;
    function IsDelimiter(Ch: Char): Boolean; virtual;
    function IsSeparator(Ch: Char): Boolean; virtual;
    function IsWhitespace(Ch: Char): Boolean; virtual;
    function IsNewline(Ch: Char): Boolean; virtual;
    function IsQuote(Ch: Char): Boolean; virtual;
    function IsSpecialChar(Ch: Char): Boolean; virtual;
    function IsDot(Ch: Char): Boolean; virtual;

    // === TOKEN PROCESSING ===
    function ProcessIdentifier: TLexerToken;
    function ProcessNumber: TLexerToken;
    function ProcessString: TLexerToken;
    function ProcessComment: TLexerToken;
    function ProcessLineComment: TLexerToken;
    function ProcessNewline: TLexerToken;
    function ProcessEndOfFile: TLexerToken;

    // === KEYWORD PROCESSING ===
    function ResolveKeyword(const Text: string): TKeywordMatchResult;
    function HandleSpecialKeywords(Token: TLexerToken): TLexerToken;

    // === VALIDATION ===
    function IsLineNumber(const Value: string): Boolean;

    // === ERROR HANDLING ===
    procedure HandleLexicalError(const ErrorMsg: string);
    function CreateErrorToken(const ErrorMsg: string): TLexerToken;

    // === GETTERS/SETTERS ===
    function GetCurrentPosition: Integer;
    function GetCurrentLine: Integer;
    function GetCurrentColumn: Integer;
    function GetSource: string;
    procedure SetSource(const ASource: string);
    function GetTokenCount: Integer;
    function GetHasErrors: Boolean;
    {$IFDEF DEBUG}
    function GetProcessingSpeed: Double;
    {$ENDIF}

    procedure PreAllocateTokens(EstimatedCount: Integer);
    function GetNextAvailableToken: TLexerToken;

  protected
    // === VIRTUAL METHODS ===
    procedure DoError(const ErrorMsg: string; Position: Integer); virtual;

    // === CONFIGURATION METHODS ===
    procedure ConfigureForClassicBasic; virtual;
    procedure ConfigureForModernBasic; virtual;

  public
    constructor Create; overload;
    constructor Create(const ASourceFile: string); overload;
    destructor Destroy; override;

    // === MAIN LEXING METHODS ===
    function NextToken: TLexerToken;
    function NextTokenFSM: TLexerToken;
    function ScanAllTokens: TTokenList;
    function ScanAllTokensFast: TTokenList;

    // === POSITION & NAVIGATION ===
    procedure Reset;
    procedure SeekToPosition(Position: Integer);
    procedure SeekToLine(LineNumber: Integer);

    // === CONFIGURATION ===
    procedure SetLanguageMode(const Mode: string);

    // === CHARACTER SET CONFIGURATION ===
    procedure SetLetterChars(const Chars: TCharSet);
    procedure SetDigitChars(const Chars: TCharSet);
    procedure SetOperatorChars(const Chars: TCharSet);
    procedure SetDelimiterChars(const Chars: TCharSet);
    procedure SetSeparatorChars(const Chars: TCharSet);
    procedure SetWhitespaceChars(const Chars: TCharSet);
    procedure SetNewlineChars(const Chars: TCharSet);
    procedure SetQuoteChars(const Chars: TCharSet);
    procedure SetSpecialChars(const Chars: TCharSet);

    // === LEXER OPTIONS ===
    procedure SetHasLineNumbers(Value: Boolean);
    procedure SetRequireSpacesBetweenTokens(Value: Boolean);
    procedure SetCaseSensitive(Value: Boolean);

    // === DEBUGGING ===
    {$IFDEF DEBUG}
    procedure EnableDebugMode(ADebugMode: Boolean = True);
    procedure DisableDebugMode;
    procedure DumpState;
    function GetDebugInfo: string;
    procedure PrintFastPathStats;
    {$ENDIF}

    // === PROPERTIES ===
    property Source: string read GetSource write SetSource;
    property SourceFile: string read FSourceFile write FSourceFile;
    property CurrentPosition: Integer read GetCurrentPosition;
    property CurrentLine: Integer read GetCurrentLine;
    property CurrentColumn: Integer read GetCurrentColumn;
    property CurrentState: TLexerState read FCurrentState;
    property TokenList: TTokenList read FTokenList;
    property KeywordRegistry: TKeywordRegistry read FKeywordRegistry;
    property TokenCount: Integer read GetTokenCount;
    property HasErrors: Boolean read GetHasErrors;
    {$IFDEF DEBUG}
    property DebugMode: Boolean read FDebugMode;
    {$ENDIF}

    // === TOKEN POOL CACHE ===
    property TokensPerCharRatio: Double read FTokensPerCharRatio write FTokensPerCharRatio;
    property SafetyFactor: Double read FSafetyFactor write FSafetyFactor;
    property MinPoolSize: Integer read FMinPoolSize write FMinPoolSize;
    property MaxPoolSize: Integer read FMaxPoolSize write FMaxPoolSize;
    property OverflowCount: Integer read FOverflowCount;
    procedure SetLanguageProfile(const Language: string);
    procedure SetCustomTokenDensity(Ratio: Double; Safety: Double = 2.0);

    // === STATISTICS ===
    function GetPoolStatistics: string;
    procedure PrintPoolStatistics;
  end;

  // === GLOBAL SETTINGS ===
  procedure SetGlobalDebugMode(Enabled: Boolean);
  function GetGlobalLexerStatistics: string;


implementation

uses
  DateUtils, StrUtils;

// Forward declaration for lazy extraction callback
function StaticExtractTokenValue(Context: Pointer; RecIndex: Integer): string; forward;

var
  GlobalDebugMode: Boolean = False;
  GlobalLexerCount: Integer = 0;
  GlobalTokensProcessed: Int64 = 0;


{ TLexerFSM }

constructor TLexerFSM.Create;
begin
  inherited Create;

  // Force BASIC-compatible number format
  DefaultFormatSettings.DecimalSeparator := '.';
  DefaultFormatSettings.ThousandSeparator := ',';

  Inc(GlobalLexerCount);

  // Initialize core components
  FTokenList := TTokenList.CreateNamed('MainTokens', True);
  FKeywordRegistry := TKeywordRegistry.Create(False, False);

  // === INITIALIZE POOL CONFIGURATION ===
  FTokensPerCharRatio := 0.15;  // Default: 15% characters = tokens
  FSafetyFactor := 2.0;         // Default: 2x safety margin
  FMinPoolSize := 100;          // Minimum 100 tokens
  FMaxPoolSize := CalculateMaxTokensFromMemory(SizeOf(TLexerToken) + 64);

  // === INITIALIZE STATISTICS ===
  FTokensAllocated := 0;
  FTokensReused := 0;
  FOverflowCount := 0;
  FCurrentTokenIndex := 0;
  FLastUsedTokenIndex := -1;

  // Initialize state
  FCurrentState := lsInitial;
  FCurrentPosition := 1;
  FCurrentLine := 1;
  FCurrentColumn := 1;
  FAtLineStart := True;
  FInComment := False;
  FTokenStart := 1;
  FTokenStartLine := 1;
  FTokenStartColumn := 1;
  FTokenText := '';

  // Initialize token record pool
  FTokenRecCount := 0;
  FTokenRecCapacity := 0;

  // Initialize configuration
  InitializeCharacterConfig;
  InitializeLexerOptions;
  BuildTransitionTable;

  // Initialize performance tracking
  FTokensProcessed := 0;
  FFSMTokenCount := 0;
  FErrorsFound := 0;
  {$IFDEF DEBUG}
  FProcessingTime := 0.0;
  FDebugMode := GlobalDebugMode;
  FVerboseLogging := False;
  {$ENDIF}
  FCacheValid := False;

  {$IFDEF DEBUG}
  LogDebug('TLexerFSM: Created successfully');
  {$ENDIF}
end;

constructor TLexerFSM.Create(const ASourceFile: string);
begin
  Create;
  FSourceFile := ASourceFile;

  if FileExists(ASourceFile) then
  begin
    with TStringList.Create do
    try
      LoadFromFile(ASourceFile);
      SetSource(Text);
      {$IFDEF DEBUG}
      LogDebug(Format('Loaded source: %s (%d chars)', [ASourceFile, Length(FSource)]));
      {$ENDIF}
    finally
      Free;
    end;
  end;
end;

destructor TLexerFSM.Destroy;
begin
  {$IFDEF DEBUG}
  LogDebug('TLexerFSM: Destroying');
  {$ENDIF}

  // === PRINT DEBUG STATISTICS ===
  {$IFDEF DEBUG}
  if FDebugMode then
    PrintPoolStatistics;
  {$ENDIF}

  Dec(GlobalLexerCount);
  Inc(GlobalTokensProcessed, FTokensProcessed);

  if Assigned(FTokenList) then
  begin
    FreeAndNil(FTokenList);
  end;

  if Assigned(FKeywordRegistry) then
  begin
    FreeAndNil(FKeywordRegistry);
  end;

  inherited Destroy;
end;

procedure TLexerFSM.InitializeCharacterConfig;
begin
  FCharConfig.Letters := ['A'..'Z', 'a'..'z'];
  FCharConfig.Digits := ['0'..'9'];
  FCharConfig.Operators := ['+', '-', '*', '/', '=', '<', '>', '^', '&', '|', '!', '~'];
  FCharConfig.Delimiters := ['(', ')', '[', ']', '{', '}'];
  FCharConfig.Separators := [',', ';', ':'];
  FCharConfig.Whitespace := [' ', #9];
  FCharConfig.Newlines := [#10, #13];
  FCharConfig.Quotes := ['"', ''''];
  FCharConfig.SpecialChars := ['_', '$', '%', '#', '@'];
  FCharConfig.DotChars := ['.'];

  BuildCharClassCache;
  {$IFDEF DEBUG}
  LogDebug('Character configuration initialized');
  {$ENDIF}
end;

procedure TLexerFSM.InitializeLexerOptions;
begin
  FLexerOptions.HasLineNumbers := True;
  FLexerOptions.LineNumbersRequired := False;
  FLexerOptions.MaxLineNumber := 65535;
  FLexerOptions.RequireSpacesBetweenTokens := False;
  FLexerOptions.CaseSensitive := False;
  FLexerOptions.AllowUnicodeIdentifiers := False;

  {$IFDEF DEBUG}
  LogVerbose(Format('Options: LineNumbers=%s, RequireSpaces=%s, CaseSensitive=%s',
    [BoolToStr(FLexerOptions.HasLineNumbers, True),
     BoolToStr(FLexerOptions.RequireSpacesBetweenTokens, True),
     BoolToStr(FLexerOptions.CaseSensitive, True)]));
  {$ENDIF}
end;

procedure TLexerFSM.BuildCharClassCache;
var
  Ch: Char;
begin
  for Ch := Low(Char) to High(Char) do
  begin
    if Ch in FCharConfig.Letters then
      FCharClassCache[Ch] := ccLetter
    else if Ch in FCharConfig.Digits then
      FCharClassCache[Ch] := ccDigit
    else if Ch in FCharConfig.Operators then
      FCharClassCache[Ch] := ccOperator
    else if Ch in FCharConfig.Delimiters then
      FCharClassCache[Ch] := ccDelimiter
    else if Ch in FCharConfig.Separators then
      FCharClassCache[Ch] := ccSeparator
    else if Ch in FCharConfig.Whitespace then
      FCharClassCache[Ch] := ccWhitespace
    else if Ch in FCharConfig.Newlines then
      FCharClassCache[Ch] := ccNewline
    else if Ch in FCharConfig.Quotes then
      FCharClassCache[Ch] := ccQuote
    else if Ch in FCharConfig.SpecialChars then
      FCharClassCache[Ch] := ccUnderscore
    else if Ch in FCharConfig.DotChars then
      FCharClassCache[Ch] := ccDot
    else
      FCharClassCache[Ch] := ccOther;
  end;

  FCacheValid := True;
  {$IFDEF DEBUG}
  LogDebug('Character class cache built');
  {$ENDIF}
end;

procedure TLexerFSM.BuildTransitionTable;
var
  s: TLexerState;
  c: TCharClass;
begin
  // Initialize all transitions to error
  for s := Low(TLexerState) to High(TLexerState) do
    for c := Low(TCharClass) to High(TCharClass) do
    begin
      FTransitionTable[s, c].NextState := lsError;
      FTransitionTable[s, c].Action := laError;
      FTransitionTable[s, c].TokenType := ttError;
    end;

  // ===== INITIAL STATE =====
  with FTransitionTable[lsInitial, ccLetter] do
  begin
    NextState := lsInIdentifier;
    Action := laStartToken;
    TokenType := ttIdentifier;
  end;

  with FTransitionTable[lsInitial, ccDigit] do
  begin
    NextState := lsInNumber;
    Action := laStartToken;
    TokenType := ttNumber;
  end;

  with FTransitionTable[lsInitial, ccUnderscore] do
  begin
    NextState := lsInIdentifier;
    Action := laStartToken;
    TokenType := ttIdentifier;
  end;

  with FTransitionTable[lsInitial, ccQuote] do
  begin
    NextState := lsInString;
    Action := laStartToken;
    TokenType := ttStringLiteral;
  end;

  with FTransitionTable[lsInitial, ccOperator] do
  begin
    NextState := lsInOperator;
    Action := laStartToken;
    TokenType := ttOperator;
  end;

  with FTransitionTable[lsInitial, ccSeparator] do
  begin
    NextState := lsAccept;
    Action := laStartToken;
    TokenType := ttSeparator;  // <-- Now will be processed by ProcessSeparator
  end;

  // === NEW STATE: lsInOperator ===
  // For '<' followed by '=' or '>'
  with FTransitionTable[lsInOperator, ccOperator] do
  begin
    NextState := lsAccept;
    Action := laAppendChar;  // <-- Append the second character
    TokenType := ttOperator;
  end;

  // For all other characters after an operator
  with FTransitionTable[lsInOperator, ccLetter] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;  // <-- Don't consume, go back
    TokenType := ttOperator;
  end;

  with FTransitionTable[lsInOperator, ccWhitespace] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;  // Don't consume the space
    TokenType := ttOperator;
  end;

  with FTransitionTable[lsInOperator, ccDigit] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttOperator;
  end;

  with FTransitionTable[lsInOperator, ccDelimiter] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttOperator;
  end;

  with FTransitionTable[lsInOperator, ccSeparator] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttOperator;
  end;

  with FTransitionTable[lsInOperator, ccNewline] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttOperator;
  end;

  with FTransitionTable[lsInOperator, ccDot] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttOperator;
  end;

  with FTransitionTable[lsInOperator, ccUnderscore] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttOperator;
  end;

  with FTransitionTable[lsInOperator, ccQuote] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttOperator;
  end;

  with FTransitionTable[lsInOperator, ccOther] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttOperator;
  end;

  with FTransitionTable[lsInitial, ccDelimiter] do
  begin
    NextState := lsAccept;
    Action := laStartToken;
    TokenType := ttDelimiter;
  end;

  with FTransitionTable[lsInitial, ccSeparator] do
  begin
    NextState := lsAccept;
    Action := laStartToken;
    TokenType := ttSeparator;
  end;

  //with FTransitionTable[lsInitial, ccWhitespace] do
  //begin
  //  NextState := lsInitial;
  //  Action := laSkipChar;
  //  TokenType := ttWhitespace;
  //end;

  with FTransitionTable[lsInitial, ccNewline] do
  begin
    NextState := lsAccept;
    Action := laStartToken;
    TokenType := ttEndOfLine;
  end;

  with FTransitionTable[lsInitial, ccDot] do
  begin
    NextState := lsAccept;
    Action := laStartToken;
    TokenType := ttOperator;
  end;

  with FTransitionTable[lsInitial, ccOther] do
  begin
    NextState := lsInitial;
    Action := laSkipChar;
    TokenType := ttUnknown;
  end;

  // ===== IN IDENTIFIER =====
  with FTransitionTable[lsInIdentifier, ccLetter] do
  begin
    NextState := lsInIdentifier;
    Action := laAppendChar;
    TokenType := ttIdentifier;
  end;

  with FTransitionTable[lsInIdentifier, ccDigit] do
  begin
    NextState := lsInIdentifier;
    Action := laAppendChar;
    TokenType := ttIdentifier;
  end;

  with FTransitionTable[lsInIdentifier, ccUnderscore] do
  begin
    NextState := lsInIdentifier;
    Action := laAppendChar;
    TokenType := ttIdentifier;
  end;

  // All other characters = end identifier (LOOKAHEAD)
  with FTransitionTable[lsInIdentifier, ccOperator] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttIdentifier;
  end;

  with FTransitionTable[lsInIdentifier, ccDelimiter] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttIdentifier;
  end;

  with FTransitionTable[lsInIdentifier, ccSeparator] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttIdentifier;
  end;

  with FTransitionTable[lsInIdentifier, ccWhitespace] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttIdentifier;
  end;

  with FTransitionTable[lsInIdentifier, ccNewline] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttIdentifier;
  end;

  with FTransitionTable[lsInIdentifier, ccQuote] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttIdentifier;
  end;

  with FTransitionTable[lsInIdentifier, ccDot] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttIdentifier;
  end;

  with FTransitionTable[lsInIdentifier, ccOther] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttIdentifier;
  end;

  // ===== IN NUMBER =====
  with FTransitionTable[lsInNumber, ccDigit] do
  begin
    NextState := lsInNumber;
    Action := laAppendChar;
    TokenType := ttNumber;
  end;

  with FTransitionTable[lsInNumber, ccDot] do
  begin
    NextState := lsInNumberDecimal;
    Action := laAppendChar;
    TokenType := ttNumber;
  end;

  // All others = end number
  with FTransitionTable[lsInNumber, ccLetter] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttNumber;
  end;

  with FTransitionTable[lsInNumber, ccOperator] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttNumber;
  end;

  with FTransitionTable[lsInNumber, ccDelimiter] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttNumber;
  end;

  with FTransitionTable[lsInNumber, ccSeparator] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttNumber;
  end;

  with FTransitionTable[lsInNumber, ccWhitespace] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttNumber;
  end;

  with FTransitionTable[lsInNumber, ccNewline] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttNumber;
  end;

  // Special chars (#, $, %, @) after number = end number (e.g. MOVSPR 3, 45#2)
  with FTransitionTable[lsInNumber, ccUnderscore] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttNumber;
  end;

  // ===== IN DECIMAL NUMBER =====
  with FTransitionTable[lsInNumberDecimal, ccDigit] do
  begin
    NextState := lsInNumberDecimal;
    Action := laAppendChar;
    TokenType := ttNumber;
  end;

  // All others = end decimal number
  with FTransitionTable[lsInNumberDecimal, ccLetter] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttNumber;
  end;

  with FTransitionTable[lsInNumberDecimal, ccOperator] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttNumber;
  end;

  with FTransitionTable[lsInNumberDecimal, ccDelimiter] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttNumber;
  end;

  with FTransitionTable[lsInNumberDecimal, ccSeparator] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttNumber;
  end;

  with FTransitionTable[lsInNumberDecimal, ccWhitespace] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttNumber;
  end;

  with FTransitionTable[lsInNumberDecimal, ccNewline] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttNumber;
  end;

  // Special chars (#, $, %, @) after decimal number = end number
  with FTransitionTable[lsInNumberDecimal, ccUnderscore] do
  begin
    NextState := lsAccept;
    Action := laLookAhead;
    TokenType := ttNumber;
  end;

  // ===== IN STRING =====
  with FTransitionTable[lsInString, ccQuote] do
  begin
    NextState := lsAccept;
    Action := laAppendChar;
    TokenType := ttStringLiteral;
  end;

  // All other characters continue the string
  with FTransitionTable[lsInString, ccLetter] do
  begin
    NextState := lsInString;
    Action := laAppendChar;
    TokenType := ttStringLiteral;
  end;

  with FTransitionTable[lsInString, ccDigit] do
  begin
    NextState := lsInString;
    Action := laAppendChar;
    TokenType := ttStringLiteral;
  end;

  with FTransitionTable[lsInString, ccOperator] do
  begin
    NextState := lsInString;
    Action := laAppendChar;
    TokenType := ttStringLiteral;
  end;

  with FTransitionTable[lsInString, ccDelimiter] do
  begin
    NextState := lsInString;
    Action := laAppendChar;
    TokenType := ttStringLiteral;
  end;

  with FTransitionTable[lsInString, ccSeparator] do
  begin
    NextState := lsInString;
    Action := laAppendChar;
    TokenType := ttStringLiteral;
  end;

  with FTransitionTable[lsInString, ccWhitespace] do
  begin
    NextState := lsInString;
    Action := laAppendChar;
    TokenType := ttStringLiteral;
  end;

  with FTransitionTable[lsInString, ccNewline] do
  begin
    NextState := lsInString;
    Action := laAppendChar;
    TokenType := ttStringLiteral;
  end;

  with FTransitionTable[lsInString, ccDot] do
  begin
    NextState := lsInString;
    Action := laAppendChar;
    TokenType := ttStringLiteral;
  end;

  with FTransitionTable[lsInString, ccUnderscore] do
  begin
    NextState := lsInString;
    Action := laAppendChar;
    TokenType := ttStringLiteral;
  end;

  with FTransitionTable[lsInString, ccOther] do
  begin
    NextState := lsInString;
    Action := laAppendChar;
    TokenType := ttStringLiteral;
  end;

  // ===== IN LINE COMMENT =====
  with FTransitionTable[lsInLineComment, ccLetter] do
  begin
    NextState := lsInLineComment;
    Action := laAppendChar;
    TokenType := ttCommentText;
  end;

  with FTransitionTable[lsInLineComment, ccDigit] do
  begin
    NextState := lsInLineComment;
    Action := laAppendChar;
    TokenType := ttCommentText;
  end;

  with FTransitionTable[lsInLineComment, ccOperator] do
  begin
    NextState := lsInLineComment;
    Action := laAppendChar;
    TokenType := ttCommentText;
  end;

  with FTransitionTable[lsInLineComment, ccDelimiter] do
  begin
    NextState := lsInLineComment;
    Action := laAppendChar;
    TokenType := ttCommentText;
  end;

  with FTransitionTable[lsInLineComment, ccSeparator] do
  begin
    NextState := lsInLineComment;
    Action := laAppendChar;
    TokenType := ttCommentText;
  end;

  with FTransitionTable[lsInLineComment, ccWhitespace] do
  begin
    NextState := lsInLineComment;
    Action := laAppendChar;
    TokenType := ttCommentText;
  end;

  with FTransitionTable[lsInLineComment, ccUnderscore] do
  begin
    NextState := lsInLineComment;
    Action := laAppendChar;
    TokenType := ttCommentText;
  end;

  with FTransitionTable[lsInLineComment, ccQuote] do
  begin
    NextState := lsInLineComment;
    Action := laAppendChar;
    TokenType := ttCommentText;
  end;

  with FTransitionTable[lsInLineComment, ccDot] do
  begin
    NextState := lsInLineComment;
    Action := laAppendChar;
    TokenType := ttCommentText;
  end;

  with FTransitionTable[lsInLineComment, ccOther] do
  begin
    NextState := lsInLineComment;
    Action := laAppendChar;
    TokenType := ttCommentText;
  end;

  // EOL ends comment and returns to initial state
  with FTransitionTable[lsInLineComment, ccNewline] do
  begin
    NextState := lsInitial;
    Action := laLookAhead;
    TokenType := ttCommentText;
  end;

  {$IFDEF DEBUG}
  LogDebug('Transition table built successfully');
  {$ENDIF}
end;


// === TOKEN BUFFER METHODS ===

procedure TLexerFSM.TokenBufferReset;
begin
  FTokenLength := 0;
  FLastTokenLength := -1; // Invalida cache
end;

procedure TLexerFSM.TokenBufferAdd(C: Char);
begin
  if FTokenLength >= MAX_TOKEN_LENGTH then
    raise Exception.CreateFmt('Token too long at line %d (max %d chars) - possible unterminated string',
      [FCurrentLine, MAX_TOKEN_LENGTH]);

  FTokenBuffer[FTokenLength] := C;
  Inc(FTokenLength);
end;

function TLexerFSM.TokenBufferAsString: string;
begin
  Result := '';
  // Cache dell'ultima conversione
  if FLastTokenLength = FTokenLength then
  begin
    Result := FLastTokenString;
    Exit;
  end;

  // Convert only if necessary
  SetLength(Result, FTokenLength);
  if FTokenLength > 0 then
    Move(FTokenBuffer[0], Result[1], FTokenLength);

  // Cache result
  FLastTokenString := Result;
  FLastTokenLength := FTokenLength;
end;

procedure TLexerFSM.TokenBufferSetString(const S: string);
var
  Len: Integer;
begin
  Len := Length(S);
  if Len > High(FTokenBuffer) then
    Len := High(FTokenBuffer);

  FTokenLength := Len;
  if Len > 0 then
    Move(S[1], FTokenBuffer[0], Len);
end;

// === CORE CHARACTER AND POSITION METHODS ===

function TLexerFSM.GetCurrentChar: Char;
begin
  if FCurrentPosition <= FSourceLength then
    Result := FSource[FCurrentPosition]
  else
    Result := #0;
end;

function TLexerFSM.PeekChar(Offset: Integer): Char;
var
  Pos: Integer;
begin
  Pos := FCurrentPosition + Offset;
  if Pos <= FSourceLength then
    Result := FSource[Pos]
  else
    Result := #0;
end;

procedure TLexerFSM.AdvanceChar;
begin
  if FCurrentPosition <= FSourceLength then
  begin
    if FSource[FCurrentPosition] = #13 then
    begin
      Inc(FCurrentPosition);
      Inc(FCurrentLine);
      FCurrentColumn := 1;
      // Skip LF if it follows CR
      if (FCurrentPosition <= FSourceLength) and (FSource[FCurrentPosition] = #10) then
        Inc(FCurrentPosition);
    end
    else if FSource[FCurrentPosition] = #10 then
    begin
      Inc(FCurrentPosition);
      Inc(FCurrentLine);
      FCurrentColumn := 1;
    end
    else
    begin
      Inc(FCurrentPosition);
      Inc(FCurrentColumn);
    end;
  end;
end;

function TLexerFSM.ClassifyChar(Ch: Char): TCharClass;
begin
  if FCacheValid then
    Result := FCharClassCache[Ch]
  else
  begin
    // Fallback classification
    if IsLetter(Ch) then
      Result := ccLetter
    else if IsDigit(Ch) then
      Result := ccDigit
    else if IsOperator(Ch) then
      Result := ccOperator
    else if IsDelimiter(Ch) then
      Result := ccDelimiter
    else if IsSeparator(Ch) then
      Result := ccSeparator
    else if IsWhitespace(Ch) then
      Result := ccWhitespace
    else if IsNewline(Ch) then
      Result := ccNewline
    else if IsQuote(Ch) then
      Result := ccQuote
    else if IsSpecialChar(Ch) then
      Result := ccUnderscore
    else if IsDot(Ch) then
      Result := ccDot
    else
      Result := ccOther;
  end;
end;

// === CHARACTER CLASSIFICATION METHODS ===

function TLexerFSM.IsLetter(Ch: Char): Boolean;
begin
  Result := Ch in FCharConfig.Letters;
end;

function TLexerFSM.IsDigit(Ch: Char): Boolean;
begin
  Result := Ch in FCharConfig.Digits;
end;

function TLexerFSM.IsOperator(Ch: Char): Boolean;
begin
  Result := Ch in FCharConfig.Operators;
end;

function TLexerFSM.IsDelimiter(Ch: Char): Boolean;
begin
  Result := Ch in FCharConfig.Delimiters;
end;

function TLexerFSM.IsSeparator(Ch: Char): Boolean;
begin
  Result := Ch in FCharConfig.Separators;
end;

function TLexerFSM.IsWhitespace(Ch: Char): Boolean;
begin
  Result := Ch in FCharConfig.Whitespace;
end;

function TLexerFSM.IsNewline(Ch: Char): Boolean;
begin
  Result := Ch in FCharConfig.Newlines;
end;

function TLexerFSM.IsQuote(Ch: Char): Boolean;
begin
  Result := Ch in FCharConfig.Quotes;
end;

function TLexerFSM.IsSpecialChar(Ch: Char): Boolean;
begin
  Result := Ch in FCharConfig.SpecialChars;
end;

function TLexerFSM.IsDot(Ch: Char): Boolean;
begin
  Result := Ch in FCharConfig.DotChars;
end;

// === TOKEN CREATION AND PROCESSING ===

function TLexerFSM.CreateToken(TokenType: TTokenType; KeywordInfo: TKeywordInfo = nil): TLexerToken;
var
  ATokenLine: Integer;
  RecIdx: Integer;
  Rec: PTokenRec;
begin
  // === ALLOCATE TOKEN RECORD (LIGHTWEIGHT, NO HEAP) ===
  RecIdx := AllocTokenRec;
  Rec := @FTokenRecPool[RecIdx];

  // === SETUP TOKEN RECORD ===
  if TokenType = ttEndOfFile then
    ATokenLine := FCurrentLine
  else
    ATokenLine := FTokenStartLine;

  Rec^.TokenType := TokenType;
  Rec^.StartPos := FTokenStart;
  Rec^.Length := FTokenLength;
  Rec^.Line := ATokenLine;
  Rec^.Column := FTokenStartColumn;
  Rec^.KeywordInfo := KeywordInfo;

  // === POOL ACCESS FOR WRAPPER OBJECT ===
  if FCurrentTokenIndex < FTokenList.Count then
  begin
    Result := TLexerToken(FTokenList.Items[FCurrentTokenIndex]);
    Inc(FTokensReused);
  end
  else
    Result := HandleTokenOverflow;

  FLastUsedTokenIndex := FCurrentTokenIndex;
  Inc(FCurrentTokenIndex);

  // === TRUE LAZY SETUP: DON'T extract value yet! ===
  Result.TokenType := TokenType;
  Result.KeywordInfo := KeywordInfo;
  Result.Line := ATokenLine;
  Result.Column := FTokenStartColumn;
  Result.Position := FTokenStart;
  Result.Length := FTokenLength;
  Result.SourceFile := FSourceFile;

  // Setup lazy extraction
  Result.SetupLazyExtraction(Self, @StaticExtractTokenValue, RecIdx);

  Result := HandleSpecialKeywords(Result);

  Inc(FTokensProcessed);

  if TokenType = ttEndOfLine then
    FAtLineStart := True
  else
    FAtLineStart := False;
end;

function TLexerFSM.HandleTokenOverflow: TLexerToken;
begin
  // Overflow: create new token and add it
  Result := TLexerToken.Create(ttUnknown, '', 0, 0, 0, 0, nil);
  FTokenList.AddToken(Result);  // <-- Use TTokenList method
  Inc(FTokensAllocated);
  Inc(FOverflowCount);

  //FLastUsedTokenIndex := FCurrentTokenIndex;
  //Inc(FCurrentTokenIndex);

  {$IFDEF DEBUG}
  LogDebug(Format('Pool overflow: created token at index %d', [FLastUsedTokenIndex]));
  {$ENDIF}
end;

procedure TLexerFSM.ResetToken;
begin
  TokenBufferReset;
  FTokenStart := FCurrentPosition;
  FTokenStartLine := 1;
  FTokenStartColumn := 1;
  FTokenStartLine := FCurrentLine;
  FTokenStartColumn := FCurrentColumn;
end;

function TLexerFSM.ProcessIdentifier: TLexerToken;
var
  Match: TKeywordMatchResult;
  TokenText: string;
begin
  Result := nil;

  // OPTIMIZATION: Single-char identifiers are NEVER keywords in BASIC
  if FTokenLength = 1 then
  begin
    Result := CreateToken(ttIdentifier);
    Exit;
  end;

  TokenText := TokenBufferAsString;

  // Full keyword lookup
  Match := ResolveKeyword(TokenText);

  if Match.Found then
    Result := CreateToken(Match.KeywordInfo.TokenType, Match.KeywordInfo)
  else
    Result := CreateToken(ttIdentifier);
end;

function TLexerFSM.ProcessNumber: TLexerToken;
var
 TokenText: string;
begin
 Result := nil;
 TokenText := TokenBufferAsString;

 // Check if it's a line number at start of line
 // (no need for DefaultFormatSettings overhead here!)
 if FAtLineStart and IsLineNumber(TokenText) then
   Result := CreateToken(ttLineNumber)
 else
   Result := CreateToken(ttNumber);
end;

function TLexerFSM.ProcessString: TLexerToken;
var
  S: string;
begin
  Result := CreateToken(ttStringLiteral);
  // Remove surrounding quotes from string value
  S := Result.Value;
  if (Length(S) >= 2) and (S[1] = '"') and (S[Length(S)] = '"') then
    Result.Value := Copy(S, 2, Length(S) - 2);
end;

function TLexerFSM.ProcessComment: TLexerToken;
begin
  Result := CreateToken(ttCommentText);
end;

function TLexerFSM.ProcessLineComment: TLexerToken;
begin
  // Remove leading space from comment text
  if (System.Length(FTokenText) > 0) and (FTokenText[1] = ' ') then
  begin
    FTokenText := Copy(FTokenText, 2, System.Length(FTokenText) - 1);
  end;

  Result := CreateToken(ttCommentText);
  FCurrentState := lsInitial; // Return to initial state
end;

function TLexerFSM.ProcessNewline: TLexerToken;
begin
  FAtLineStart := True;
  Result := CreateToken(ttEndOfLine);
end;

function TLexerFSM.ProcessEndOfFile: TLexerToken;
begin
  Result := CreateToken(ttEndOfFile);
end;

// === TOKEN RECORD POOL MANAGEMENT ===

function TLexerFSM.AllocTokenRec: Integer;
begin
  // Check if we need to grow the pool
  if FTokenRecCount >= FTokenRecCapacity then
  begin
    // Grow by 50% or at least 100 tokens
    if FTokenRecCapacity = 0 then
      FTokenRecCapacity := 100
    else
      FTokenRecCapacity := FTokenRecCapacity + (FTokenRecCapacity div 2);

    SetLength(FTokenRecPool, FTokenRecCapacity);
  end;

  Result := FTokenRecCount;
  Inc(FTokenRecCount);
end;

function TLexerFSM.GetTokenValue(const Rec: TTokenRec): string;
begin
  // ZERO-COPY: Extract substring directly from source buffer
  SetLength(Result, Rec.Length);
  if Rec.Length > 0 then
    Move(FSource[Rec.StartPos], Result[1], Rec.Length);
end;

function TLexerFSM.ExtractTokenValue(RecIndex: Integer): string;
begin
  // PUBLIC: Extract value from token record by index
  if (RecIndex >= 0) and (RecIndex < FTokenRecCount) then
    Result := GetTokenValue(FTokenRecPool[RecIndex])
  else
    Result := '';
end;

// Static callback function for lazy token value extraction
function StaticExtractTokenValue(Context: Pointer; RecIndex: Integer): string;
var
  Lexer: TLexerFSM;
begin
  Lexer := TLexerFSM(Context);
  Result := Lexer.ExtractTokenValue(RecIndex);
end;

// === KEYWORD PROCESSING ===

function TLexerFSM.ResolveKeyword(const Text: string): TKeywordMatchResult;
var
  Options: TKeywordSearchOptions;
begin
  // Configure search options based on lexer settings
  Options := CreateDefaultSearchOptions;
  Options.CaseSensitive := FLexerOptions.CaseSensitive;
  Options.AllowPartialMatch := not FLexerOptions.RequireSpacesBetweenTokens;
  Options.MaxResults := 1; // We only want the best match

  Result := FKeywordRegistry.FindKeyword(Text, Options);

  {$IFDEF DEBUG}
  LogVerbose(Format('Keyword lookup: "%s" -> Found=%s', [Text, BoolToStr(Result.Found, True)]));
  {$ENDIF}
end;

function TLexerFSM.HandleSpecialKeywords(Token: TLexerToken): TLexerToken;
begin
  Result := Token;

  // Handle comment keywords that change state
  if Assigned(Token.KeywordInfo) and Token.KeywordInfo.IsCommentKeyword then
  begin
    FCurrentState := lsInLineComment;
    FInComment := True;
    {$IFDEF DEBUG}
    LogDebug('Entering line comment state after REM');
    {$ENDIF}
  end;
end;

// === HELPER METHOD ===
function TLexerFSM.ParseNumberDigits: Boolean;
var
  CurrentChar: Char;
begin
  Result := False;
  CurrentChar := GetCurrentChar;

  // Digits
  while (CurrentChar >= '0') and (CurrentChar <= '9') do
  begin
    TokenBufferAdd(CurrentChar);
    AdvanceChar;
    CurrentChar := GetCurrentChar;
    Result := True;
  end;

  // Scientific notation
  if (CurrentChar = 'E') or (CurrentChar = 'e') then
  begin
    TokenBufferAdd(CurrentChar);
    AdvanceChar;
    CurrentChar := GetCurrentChar;

    // Optional sign
    if (CurrentChar = '+') or (CurrentChar = '-') then
    begin
      TokenBufferAdd(CurrentChar);
      AdvanceChar;
      CurrentChar := GetCurrentChar;
    end;

    // Exponent digits
    while (CurrentChar >= '0') and (CurrentChar <= '9') do
    begin
      TokenBufferAdd(CurrentChar);
      AdvanceChar;
      CurrentChar := GetCurrentChar;
    end;
  end;
end;

// === MAIN LEXING METHOD ===

function TLexerFSM.NextToken: TLexerToken;
var
  CurrentChar: Char;
  {$IFDEF DEBUG}
  StartTime: TDateTime;
  {$ENDIF}
begin
  {$IFDEF DEBUG}
  if FDebugMode then
    StartTime := Now;
  {$ENDIF}

  // Reset for new token if not in comment
  if FCurrentState <> lsInLineComment then
  begin
    ResetToken;
    FCurrentState := lsInitial;
  end
  else
  begin
    ResetToken;
  end;

  // === MEGA FAST PATH - BYPASS FSM FOR 95% OF CASES ===
  if FCurrentState = lsInitial then
  begin
    CurrentChar := GetCurrentChar;

    // Very fast EOF check
    if (FCurrentPosition > FSourceLength) or (CurrentChar = #0) then
    begin
      Result := ProcessEndOfFile;
      {$IFDEF DEBUG}
      if FDebugMode then
        FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
      {$ENDIF}
      Exit;
    end;

    // === WHITESPACE - VERY FAST SKIP ===
    while CurrentChar in [' ', #9] do
    begin
      AdvanceChar;
      CurrentChar := GetCurrentChar;
      if (FCurrentPosition > FSourceLength) or (CurrentChar = #0) then
      begin
        Result := ProcessEndOfFile;
        {$IFDEF DEBUG}
        if FDebugMode then
          FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
        {$ENDIF}
        Exit;
      end;
    end;

    // === DIRECT CHARACTER DISPATCH - LUA STYLE ===
    case CurrentChar of
      // === NEWLINES ===
      #10, #13:
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          // Handle CRLF
          if (CurrentChar = #13) and (GetCurrentChar = #10) then
          begin
            TokenBufferAdd(GetCurrentChar);
            AdvanceChar;
          end;
          Result := ProcessNewline;
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      // === SINGLE CHAR OPERATORS - ZERO PROCESSING ===
      '+':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttOpAdd);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      '-':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttOpSub);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      '*':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttOpMul);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      '/':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttOpDiv);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      '^':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttOpPow);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      // === MULTI-CHAR OPERATORS ===
      '=':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          if GetCurrentChar = '=' then
          begin
            TokenBufferAdd(GetCurrentChar);
            AdvanceChar;
            Result := CreateToken(ttOpEq);
          end
          else
            Result := CreateToken(ttOpEq); // Single =
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      '<':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          case GetCurrentChar of
            '=':
              begin
                TokenBufferAdd(GetCurrentChar);
                AdvanceChar;
                Result := CreateToken(ttOpLe);
              end;
            '>':
              begin
                TokenBufferAdd(GetCurrentChar);
                AdvanceChar;
                Result := CreateToken(ttOpNeq);
              end;
            else
              Result := CreateToken(ttOpLt);
          end;
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      '>':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          if GetCurrentChar = '=' then
          begin
            TokenBufferAdd(GetCurrentChar);
            AdvanceChar;
            Result := CreateToken(ttOpGe);
          end
          else
            Result := CreateToken(ttOpGt);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      // === DELIMITERS - ZERO PROCESSING ===
      '(':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttDelimParOpen);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      ')':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttDelimParClose);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      '[':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttDelimBrackOpen);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      ']':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttDelimBrackClose);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      '{':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttDelimBraceOpen);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      '}':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttDelimBraceClose);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      // === SEPARATORS - ZERO PROCESSING ===
      ',':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttSeparParam);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      ';':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttSeparOutput);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      ':':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttSeparStmt);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      //// === DOT OPERATOR ===
      //'.':
      //  begin
      //    ResetToken;
      //    TokenBufferAdd(CurrentChar);
      //    AdvanceChar;
      //    Result := CreateToken(ttOperator);
      //    {$IFDEF DEBUG}
      //    if FDebugMode then
      //      FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
      //    {$ENDIF}
      //    Exit;
      //  end;

      // === IDENTIFIERS E KEYWORDS ===
      'A'..'Z', 'a'..'z', '_':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;

          // Fast identifier loop - NO FSM
          CurrentChar := GetCurrentChar;
          while ((CurrentChar >= 'A') and (CurrentChar <= 'Z')) or
                ((CurrentChar >= 'a') and (CurrentChar <= 'z')) or
                ((CurrentChar >= '0') and (CurrentChar <= '9')) or
                (CurrentChar = '_') or
                (CurrentChar = '$') or   // String
                (CurrentChar = '%') or   // Integer (16 bit)
                (CurrentChar = '#') or   // Double precision (64 bit)
                (CurrentChar = '!') do   // Single precision (32 bit)
          begin
            TokenBufferAdd(CurrentChar);
            AdvanceChar;
            CurrentChar := GetCurrentChar;
          end;

          Result := ProcessIdentifier;
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      // === NUMBERS ===
      '0'..'9':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;

          ParseNumberDigits;  // ← Prima parte

          // Punto decimale
          if GetCurrentChar = '.' then
          begin
            TokenBufferAdd(GetCurrentChar);
            AdvanceChar;
            ParseNumberDigits;  // ← Parte decimale + eventuale notazione scientifica
          end;

          Result := ProcessNumber;
          Exit;
        end;

      // === DOT ===
      '.':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;

          if ParseNumberDigits then  // ← If finds digits, it's a number
            Result := ProcessNumber
          else
            Result := CreateToken(ttOperator);  // ← Otherwise it's an operator
          Exit;
        end;

      // === STRING LITERALS - FAST PARSING ===
      '"':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar); // Opening quote
          AdvanceChar;

          // Fast string loop
          CurrentChar := GetCurrentChar;
          while (CurrentChar <> #0) and (CurrentChar <> '"') do
          begin
            if CurrentChar = '\' then
            begin
              TokenBufferAdd(CurrentChar);
              AdvanceChar;
              if GetCurrentChar <> #0 then
              begin
                TokenBufferAdd(GetCurrentChar);
                AdvanceChar;
              end;
            end
            else
            begin
              TokenBufferAdd(CurrentChar);
              AdvanceChar;
            end;
            CurrentChar := GetCurrentChar;
          end;

          if CurrentChar = '"' then
          begin
            TokenBufferAdd(CurrentChar); // Closing quote
            AdvanceChar;
          end;

          Result := ProcessString;
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      '''':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar); // Opening quote
          AdvanceChar;

          // Fast string loop
          CurrentChar := GetCurrentChar;
          while (CurrentChar <> #0) and (CurrentChar <> '''') do
          begin
            if CurrentChar = '\' then
            begin
              TokenBufferAdd(CurrentChar);
              AdvanceChar;
              if GetCurrentChar <> #0 then
              begin
                TokenBufferAdd(GetCurrentChar);
                AdvanceChar;
              end;
            end
            else
            begin
              TokenBufferAdd(CurrentChar);
              AdvanceChar;
            end;
            CurrentChar := GetCurrentChar;
          end;

          if CurrentChar = '''' then
          begin
            TokenBufferAdd(CurrentChar); // Closing quote
            AdvanceChar;
          end;

          Result := ProcessString;
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      // === HASH - FILE HANDLE PREFIX / MOVSPR SEPARATOR ===
      '#':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttFileHandlePrefix);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      // === OTHER CHARACTERS - USE FSM ===
      else
        begin
          // Only for very rare cases - multiline comments, exotic characters, errors
          {$IFDEF DEBUG}
          LogVerbose('Using FSM for rare character: ' + CurrentChar);
          {$ENDIF}
          Result := NextTokenFSM;
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;
    end;
  end
  else
  begin
    // Non-initial states (comments, errors) - use FSM
    Result := NextTokenFSM;
    {$IFDEF DEBUG}
    if FDebugMode then
      FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
    {$ENDIF}
  end;
end;

// ===== COMPLETE NextTokenFSM METHOD WITH BUFFER =====
function TLexerFSM.NextTokenFSM: TLexerToken;
var
  CurrentChar: Char;
  CharClass: TCharClass;
  Transition: TTransition;
begin
  Inc(FFSMTokenCount);

  // Main FSM loop - handles all complex cases
  while True do
  begin
    CurrentChar := GetCurrentChar;

    // Handle end of file
    if (FCurrentPosition > FSourceLength) or (CurrentChar = #0) then
    begin
      if FTokenLength > 0 then  // <-- Replaces FTokenText <> ''
      begin
        // Process remaining token based on current state
        case FCurrentState of
          lsInIdentifier: Result := ProcessIdentifier;
          lsInNumber, lsInNumberDecimal: Result := ProcessNumber;
          lsInString: Result := ProcessString;
          lsInLineComment: Result := ProcessLineComment;
          else
            Result := CreateErrorToken('Unexpected end of file');
        end;
      end
      else
        Result := ProcessEndOfFile;
      Exit;
    end;

    // Get character class and transition
    CharClass := ClassifyChar(CurrentChar);
    Transition := FTransitionTable[FCurrentState, CharClass];

    {$IFDEF DEBUG}
    LogVerbose(Format('FSM: State=%s Char=[%s](%d) Class=%s -> State=%s Action=%s',
      [GetEnumName(TypeInfo(TLexerState), Ord(FCurrentState)),
       CurrentChar, Ord(CurrentChar),
       GetEnumName(TypeInfo(TCharClass), Ord(CharClass)),
       GetEnumName(TypeInfo(TLexerState), Ord(Transition.NextState)),
       GetEnumName(TypeInfo(TLexerAction), Ord(Transition.Action))]));
    {$ENDIF}

    // Apply transition action
    case Transition.Action of
      laStartToken:
        begin
          ResetToken;  // <-- Now uses TokenBufferReset
          TokenBufferAdd(CurrentChar);  // <-- Replaces FTokenText := CurrentChar;
          FCurrentState := Transition.NextState;
          AdvanceChar;
        end;

      laAppendChar:
        begin
          TokenBufferAdd(CurrentChar);  // <-- Replaces FTokenText := FTokenText + CurrentChar;
          FCurrentState := Transition.NextState;
          AdvanceChar;
        end;

      laAcceptToken:
        begin
          TokenBufferAdd(CurrentChar);  // <-- Replaces FTokenText := FTokenText + CurrentChar;
          AdvanceChar;

          // Process token based on type
          case Transition.TokenType of
            ttIdentifier: Result := ProcessIdentifier;
            ttNumber: Result := ProcessNumber;
            ttStringLiteral: Result := ProcessString;
            ttEndOfLine: Result := ProcessNewline;
            ttCommentText: Result := ProcessComment;
            else
              Result := CreateToken(Transition.TokenType);
          end;
          Exit;
        end;

      laLookAhead:
        begin
          // Don't consume character
          case Transition.TokenType of
            ttIdentifier: Result := ProcessIdentifier;
            ttNumber: Result := ProcessNumber;
            ttStringLiteral: Result := ProcessString;
            ttCommentText: Result := ProcessLineComment;
            else
              Result := CreateToken(Transition.TokenType);
          end;
          Exit;
        end;

      laSkipChar:
        begin
          AdvanceChar;
          FCurrentState := Transition.NextState;
        end;

      laError:
        begin
          HandleLexicalError(Format('Unexpected character "%s" in state %s',
            [CurrentChar, GetEnumName(TypeInfo(TLexerState), Ord(FCurrentState))]));
          TokenBufferAdd(CurrentChar);  // <-- Usa buffer invece di FTokenText := CurrentChar;
          AdvanceChar;
          Result := CreateErrorToken('Lexical error');
          Exit;
        end;
    end;

    // Check for accept state
    if FCurrentState = lsAccept then
    begin
      case Transition.TokenType of
        ttIdentifier: Result := ProcessIdentifier;
        ttNumber: Result := ProcessNumber;
        else
          Result := CreateToken(Transition.TokenType);
      end;
      Exit;
    end;
  end;
end;

// === PUBLIC LEXING METHODS ===

function TLexerFSM.ScanAllTokens: TTokenList;
var
  Token: TLexerToken;
  {$IFDEF DEBUG}
  StartTime: TDateTime;
  {$ENDIF}

begin
  {$IFDEF DEBUG}
  StartTime := Now;
  {$ENDIF}

  FTokenList.Clear;

  repeat
    Token := NextToken;
    if not Token.IsWhitespaceToken then // Skip whitespace unless needed
      FTokenList.AddToken(Token);
  until Token.TokenType = ttEndOfFile;

  {$IFDEF DEBUG}
  FProcessingTime := MilliSecondsBetween(Now, StartTime);
  {$ENDIF}

  Result := FTokenList;

  {$IFDEF DEBUG}
  LogDebug(Format('Scanned %d tokens in %.2f ms', [FTokenList.Count, FProcessingTime]));
  {$ENDIF}
end;

function TLexerFSM.ScanAllTokensFast: TTokenList;
{$IFDEF DEBUG}
var
  TokensPerSecond: Double;
  ATimer: THiResTimer;
  ElapsedMicros: Double;
{$ENDIF}
var
  Token: TLexerToken;
  SafetyCounter: Integer;
  MaxTokens: Integer;
begin
  {$IFDEF DEBUG}
  ATimer.Start;
  {$ENDIF}

  // === RESET COMPLETO ===
  ResetTokenPool;
  Reset;

  {$IFDEF DEBUG}
  LogDebug(Format('Starting fast scan with %d pre-allocated tokens', [FTokenList.Count]));
  {$ENDIF}

  // Safety mechanism: prevent infinite loops
  SafetyCounter := 0;
  MaxTokens := 1000000;  // Maximum 1 million tokens (safety limit)

  repeat
    Token := NextToken;
    Inc(SafetyCounter);

    if SafetyCounter > MaxTokens then
    begin
      raise Exception.CreateFmt('Lexer safety limit exceeded: %d tokens generated. Possible infinite loop!', [MaxTokens]);
    end;
  until Token.TokenType = ttEndOfFile;

  Result := FTokenList;

  // === CALCULATE SPEED ===
  {$IFDEF DEBUG}
  ElapsedMicros := ATimer.ElapsedMicroseconds;
  if ElapsedMicros > 0 then
    TokensPerSecond := (FLastUsedTokenIndex + 1) / (ElapsedMicros / 1000000.0)
  else
    TokensPerSecond := 0;

  LogDebug(Format('Fast scanned %d tokens in %.2f ms (%.0f tokens/sec)',
    [FLastUsedTokenIndex + 1, ElapsedMicros / 1000.0, TokensPerSecond]));
  {$ENDIF}


end;

// === POSITION & NAVIGATION ===

procedure TLexerFSM.Reset;
begin
  FCurrentPosition := 1;
  FCurrentLine := 1;
  FCurrentColumn := 1;
  FCurrentState := lsInitial;
  FAtLineStart := True;
  FInComment := False;
  ResetToken;

  // === RESET COUNTERS AND STATISTICS ===
  FTokensProcessed := 0;
  FFSMTokenCount := 0;
  FErrorsFound := 0;
  {$IFDEF DEBUG}
  FProcessingTime := 0.0;
  {$ENDIF}

  // === RESET TOKEN POOL ===
  FCurrentTokenIndex := 0;
  FLastUsedTokenIndex := -1;

  // === RESET STRING CACHE ===
  FLastTokenString := '';
  FLastTokenLength := -1;

  {$IFDEF DEBUG}
  LogDebug('Lexer reset to beginning');
  {$ENDIF}

end;

procedure TLexerFSM.SeekToPosition(Position: Integer);
var
  i: Integer;
  Line, Column: Integer;
begin
  if (Position < 1) or (Position > FSourceLength) then
    raise Exception.CreateFmt('Position %d out of range (1..%d)', [Position, FSourceLength]);

  // Reset and scan to position
  FCurrentPosition := 1;
  Line := 1;
  Column := 1;

  i := 1;
  while i < Position do
  begin
    if FSource[i] = #13 then
    begin
      Inc(Line);
      Column := 1;
      // Skip LF if follows CR
      if (i < FSourceLength) and (FSource[i + 1] = #10) then
        Inc(i); // Now we can increment i
    end
    else if FSource[i] = #10 then
    begin
      Inc(Line);
      Column := 1;
    end
    else
      Inc(Column);

    Inc(i);
  end;

  FCurrentPosition := Position;
  FCurrentLine := Line;
  FCurrentColumn := Column;
  FCurrentState := lsInitial;

  {$IFDEF DEBUG}
  LogDebug(Format('Seeked to position %d (line %d, column %d)', [Position, Line, Column]));
  {$ENDIF}

end;

procedure TLexerFSM.SeekToLine(LineNumber: Integer);
var
  i: Integer;
  TempCurrentLine: Integer;
begin
  if LineNumber < 1 then
    raise Exception.CreateFmt('Line number %d out of range (must be >= 1)', [LineNumber]);

  Reset;
  TempCurrentLine := 1;

  if LineNumber = 1 then
    Exit;

  i := 1;
  while i <= FSourceLength do
  begin
    if FSource[i] = #13 then
    begin
      Inc(TempCurrentLine);
      if TempCurrentLine = LineNumber then
      begin
        FCurrentPosition := i + 1;
        // Skip LF if follows CR
        if (i < FSourceLength) and (FSource[i + 1] = #10) then
          Inc(FCurrentPosition);
        FCurrentLine := LineNumber;
        FCurrentColumn := 1;

        {$IFDEF DEBUG}
        LogDebug(Format('Seeked to line %d at position %d', [LineNumber, FCurrentPosition]));
        {$ENDIF}

        Exit;
      end;
      // Skip LF if follows CR
      if (i < FSourceLength) and (FSource[i + 1] = #10) then
        Inc(i); // Now we can increment i because we use while
    end
    else if FSource[i] = #10 then
    begin
      Inc(TempCurrentLine);
      if TempCurrentLine = LineNumber then
      begin
        FCurrentPosition := i + 1;
        FCurrentLine := LineNumber;
        FCurrentColumn := 1;

        {$IFDEF DEBUG}
        LogDebug(Format('Seeked to line %d at position %d', [LineNumber, FCurrentPosition]));
        {$ENDIF}

        Exit;
      end;
    end;

    Inc(i);
  end;

  // If we get here, line number was beyond end of file
  raise Exception.CreateFmt('Line number %d not found (file has %d lines)', [LineNumber, TempCurrentLine]);
end;

// === CONFIGURATION ===

procedure TLexerFSM.SetLanguageMode(const Mode: string);
begin
  case UpperCase(Mode) of
    'BASIC', 'CLASSIC_BASIC', 'COMMODORE': ConfigureForClassicBasic;
    'MODERN_BASIC', 'QBASIC', 'FREEBASIC': ConfigureForModernBasic;
    else
      raise Exception.CreateFmt('Unsupported language mode: %s', [Mode]);
  end;

  // Rebuild structures after configuration change
  BuildCharClassCache;
  BuildTransitionTable;

  {$IFDEF DEBUG}
  LogDebug(Format('Language mode set to: %s', [Mode]));
  {$ENDIF}

end;

procedure TLexerFSM.ConfigureForClassicBasic;
begin
  FLexerOptions.HasLineNumbers := True;
  FLexerOptions.LineNumbersRequired := False;
  FLexerOptions.MaxLineNumber := 65535;
  FLexerOptions.RequireSpacesBetweenTokens := False;
  FLexerOptions.CaseSensitive := False;
  FLexerOptions.AllowUnicodeIdentifiers := False;

  {$IFDEF DEBUG}
  LogDebug('Configured for Classic BASIC');
  {$ENDIF}

end;

procedure TLexerFSM.ConfigureForModernBasic;
begin
  FLexerOptions.HasLineNumbers := False;
  FLexerOptions.LineNumbersRequired := False;
  FLexerOptions.MaxLineNumber := 0;
  FLexerOptions.RequireSpacesBetweenTokens := True;
  FLexerOptions.CaseSensitive := False;
  FLexerOptions.AllowUnicodeIdentifiers := True;

  {$IFDEF DEBUG}
  LogDebug('Configured for Modern BASIC');
  {$ENDIF}

end;

// === VALIDATION METHODS ===

function TLexerFSM.IsLineNumber(const Value: string): Boolean;
var
  NumValue: Integer;
begin
  Result := False;

  if not FLexerOptions.HasLineNumbers then
    Exit;

  if not TryStrToInt(Value, NumValue) then
    Exit;

  if (NumValue < 1) or (NumValue > FLexerOptions.MaxLineNumber) then
    Exit;

  Result := True;
end;

// === ERROR HANDLING ===

procedure TLexerFSM.HandleLexicalError(const ErrorMsg: string);
begin
  Inc(FErrorsFound);

  {$IFDEF DEBUG}
  LogDebug(Format('Lexical error at %d:%d - %s', [FCurrentLine, FCurrentColumn, ErrorMsg]));
  {$ENDIF}

  DoError(ErrorMsg, FCurrentPosition);
end;

function TLexerFSM.CreateErrorToken(const ErrorMsg: string): TLexerToken;
begin
  Result := CreateToken(ttError);
  Result.Value := ErrorMsg;
end;

// === CHARACTER SET CONFIGURATION ===

procedure TLexerFSM.SetLetterChars(const Chars: TCharSet);
begin
  FCharConfig.Letters := Chars;
  BuildCharClassCache;

  {$IFDEF DEBUG}
  LogDebug('Letter characters updated');
  {$ENDIF}

end;

procedure TLexerFSM.SetDigitChars(const Chars: TCharSet);
begin
  FCharConfig.Digits := Chars;
  BuildCharClassCache;

  {$IFDEF DEBUG}
  LogDebug('Digit characters updated');
  {$ENDIF}

end;

procedure TLexerFSM.SetOperatorChars(const Chars: TCharSet);
begin
  FCharConfig.Operators := Chars;
  BuildCharClassCache;

  {$IFDEF DEBUG}
  LogDebug('Operator characters updated');
  {$ENDIF}

end;

procedure TLexerFSM.SetDelimiterChars(const Chars: TCharSet);
begin
  FCharConfig.Delimiters := Chars;
  BuildCharClassCache;

  {$IFDEF DEBUG}
  LogDebug('Delimiter characters updated');
  {$ENDIF}

end;

procedure TLexerFSM.SetSeparatorChars(const Chars: TCharSet);
begin
  FCharConfig.Separators := Chars;
  BuildCharClassCache;

  {$IFDEF DEBUG}
  LogDebug('Separator characters updated');
  {$ENDIF}

end;

procedure TLexerFSM.SetWhitespaceChars(const Chars: TCharSet);
begin
  FCharConfig.Whitespace := Chars;
  BuildCharClassCache;

  {$IFDEF DEBUG}
  LogDebug('Whitespace characters updated');
  {$ENDIF}

end;

procedure TLexerFSM.SetNewlineChars(const Chars: TCharSet);
begin
  FCharConfig.Newlines := Chars;
  BuildCharClassCache;

  {$IFDEF DEBUG}
  LogDebug('Newline characters updated');
  {$ENDIF}

end;

procedure TLexerFSM.SetQuoteChars(const Chars: TCharSet);
begin
  FCharConfig.Quotes := Chars;
  BuildCharClassCache;

  {$IFDEF DEBUG}
  LogDebug('Quote characters updated');
  {$ENDIF}

end;

procedure TLexerFSM.SetSpecialChars(const Chars: TCharSet);
begin
  FCharConfig.SpecialChars := Chars;
  BuildCharClassCache;

  {$IFDEF DEBUG}
  LogDebug('Special characters updated');
  {$ENDIF}

end;

// === LEXER OPTIONS ===

procedure TLexerFSM.SetHasLineNumbers(Value: Boolean);
begin
  FLexerOptions.HasLineNumbers := Value;

  {$IFDEF DEBUG}
  LogDebug(Format('HasLineNumbers set to %s', [BoolToStr(Value, True)]));
  {$ENDIF}

end;

procedure TLexerFSM.SetRequireSpacesBetweenTokens(Value: Boolean);
begin
  FLexerOptions.RequireSpacesBetweenTokens := Value;

  {$IFDEF DEBUG}
  LogDebug(Format('RequireSpacesBetweenTokens set to %s', [BoolToStr(Value, True)]));
  {$ENDIF}

end;

procedure TLexerFSM.SetCaseSensitive(Value: Boolean);
begin
  FLexerOptions.CaseSensitive := Value;
  FKeywordRegistry.CaseSensitive := Value;

  {$IFDEF DEBUG}
  LogDebug(Format('CaseSensitive set to %s', [BoolToStr(Value, True)]));
  {$ENDIF}

end;


// === DEBUGGING & DIAGNOSTICS ===

{$IFDEF DEBUG}
procedure TLexerFSM.EnableDebugMode(ADebugMode: Boolean);
begin
  FDebugMode := ADebugMode;
  FVerboseLogging := ADebugMode;
  LogDebug(Format('Debug and verbose mode = %s', [BoolToStr(ADebugMode, True)]));
end;
{$ENDIF}

{$IFDEF DEBUG}
procedure TLexerFSM.DisableDebugMode;
begin
  LogDebug('Debug mode disabled');
  FDebugMode := False;
  FVerboseLogging := False;
end;


procedure TLexerFSM.DumpState;
begin
  WriteLn('=== LEXER STATE DUMP ===');
  WriteLn('Position: ', FCurrentPosition);
  WriteLn('Line: ', FCurrentLine);
  WriteLn('Column: ', FCurrentColumn);
  WriteLn('State: ', GetEnumName(TypeInfo(TLexerState), Ord(FCurrentState)));
  WriteLn('At Line Start: ', FAtLineStart);
  WriteLn('In Comment: ', FInComment);
  WriteLn('Token Start: ', FTokenStart);
  WriteLn('Token Text: "', FTokenText, '"');
  WriteLn('Current Char: "', GetCurrentChar, '" (', Ord(GetCurrentChar), ')');
  WriteLn('Source Length: ', FSourceLength);
  WriteLn('Tokens Processed: ', FTokensProcessed);
  WriteLn('Errors Found: ', FErrorsFound);
  WriteLn('=== END STATE DUMP ===');
end;

function TLexerFSM.GetDebugInfo: string;
begin
  Result := Format('TLexerFSM[%s]: Pos=%d Line=%d:%d State=%s Tokens=%d Errors=%d',
    [FSourceFile, FCurrentPosition, FCurrentLine, FCurrentColumn,
     GetEnumName(TypeInfo(TLexerState), Ord(FCurrentState)),
     FTokensProcessed, FErrorsFound]);
end;

procedure TLexerFSM.PrintFastPathStats;
var
  FastPathTokens, TotalTokens: Integer;
  Coverage: Double;
begin
  // Add counters in fast path to measure coverage
  TotalTokens := FTokensProcessed;
  FastPathTokens := TotalTokens - FFSMTokenCount; // Assume FFSMTokens counter for FSM

  if TotalTokens > 0 then
  begin
    Coverage := (FastPathTokens / TotalTokens) * 100.0;
    WriteLn(Format('Fast Path Coverage: %.1f%% (%d/%d tokens)',
      [Coverage, FastPathTokens, TotalTokens]));
  end;
end;
{$ENDIF}

procedure TLexerFSM.SetLanguageProfile(const Language: string);
begin
  case UpperCase(Language) of
    'BASIC', 'COMMODORE':
      begin
        FTokensPerCharRatio := 0.15;
        FSafetyFactor := 2.0;
      end;
    'PASCAL', 'FREEPASCAL':
      begin
        FTokensPerCharRatio := 0.12;
        FSafetyFactor := 1.8;
      end;
    'C', 'CPP':
      begin
        FTokensPerCharRatio := 0.18;
        FSafetyFactor := 2.2;
      end;
    'PYTHON':
      begin
        FTokensPerCharRatio := 0.14;
        FSafetyFactor := 1.9;
      end;
    'JAVASCRIPT':
      begin
        FTokensPerCharRatio := 0.20;
        FSafetyFactor := 2.5;
      end;
    else
      begin
        // Generic default
        FTokensPerCharRatio := 0.15;
        FSafetyFactor := 2.0;
      end;
  end;

  {$IFDEF DEBUG}
  LogDebug(Format('Language profile "%s": ratio=%.3f, safety=%.1f',
    [Language, FTokensPerCharRatio, FSafetyFactor]));
  {$ENDIF}

end;

procedure TLexerFSM.SetCustomTokenDensity(Ratio: Double; Safety: Double);
begin
  FTokensPerCharRatio := Ratio;
  FSafetyFactor := Safety;

  {$IFDEF DEBUG}
  LogDebug(Format('Custom token density: ratio=%.3f, safety=%.1f', [Ratio, Safety]));
  {$ENDIF}

end;

function TLexerFSM.GetPoolStatistics: string;
var
  EfficiencyPercent: Double;
  MemoryMB: Double;
  UsedTokens: Integer;
begin
  UsedTokens := FLastUsedTokenIndex + 1;

  if FTokensAllocated > 0 then
    EfficiencyPercent := (FTokensReused / FTokensAllocated) * 100.0
  else
    EfficiencyPercent := 0.0;

  MemoryMB := (FTokenList.Count * SizeOf(TLexerToken)) / (1024 * 1024);

  Result := Format('TFPObjectList Pool: %d/%d tokens, %.1f%% eff, %d overflows, %.2f MB',
    [UsedTokens, FTokenList.Count, EfficiencyPercent, FOverflowCount, MemoryMB]);
end;

procedure TLexerFSM.PrintPoolStatistics;
var
  EfficiencyPercent: Double;
  MemoryUsedMB: Double;
  UsedTokens: Integer;
begin
  UsedTokens := FLastUsedTokenIndex + 1;

  if FTokensAllocated > 0 then
  begin
    EfficiencyPercent := (FTokensReused / FTokensAllocated) * 100.0;
    MemoryUsedMB := (FTokenList.Count * SizeOf(TLexerToken)) / (1024 * 1024);

    WriteLn('TFPObjectList Pool Statistics:');
    WriteLn(Format('  Efficiency: %.1f%% (%d reused / %d allocated)',
      [EfficiencyPercent, FTokensReused, FTokensAllocated]));
    WriteLn(Format('  Overflows: %d', [FOverflowCount]));
    WriteLn(Format('  Memory: %.2f MB (Capacity: %d)', [MemoryUsedMB, FTokenList.Capacity]));
    WriteLn(Format('  Used tokens: %d / %d', [UsedTokens, FTokenList.Count]));
    WriteLn('  Array access overhead: ZERO (direct indexing)');
    WriteLn('  Implementation: TFPObjectList with O(1) access');
  end;
end;

{$IFDEF DEBUG}
procedure TLexerFSM.LogDebug(const Msg: string);
begin
  if not FDebugMode then
    Exit;

  WriteLn('DEBUG [', ClassName, ']: ', Msg);
end;
{$ENDIF}

{$IFDEF DEBUG}
procedure TLexerFSM.LogVerbose(const Msg: string);
begin
  // Avoid expensive calls if logging disabled
  if not FVerboseLogging then
    Exit;

  // Use StringBuilder for complex messages if necessary
  WriteLn('VERBOSE [', ClassName, ']: ', Msg);
end;
{$ENDIF}


// === GETTERS/SETTERS ===

function TLexerFSM.GetCurrentPosition: Integer;
begin
  Result := FCurrentPosition;
end;

function TLexerFSM.GetCurrentLine: Integer;
begin
  Result := FCurrentLine;
end;

function TLexerFSM.GetCurrentColumn: Integer;
begin
  Result := FCurrentColumn;
end;

function TLexerFSM.GetSource: string;
begin
  Result := FSource;
end;

procedure TLexerFSM.SetSource(const ASource: string);
begin
  FSource := ASource;
  FSourceLength := Length(ASource);
  FCurrentPosition := 1;
  FCurrentLine := 1;
  FCurrentColumn := 1;
  FTokensProcessed := 0;

  Reset;

  // === CALCULATE POOL SIZE WITH CONFIGURATION ===
  if FSourceLength <= 0 then
    FEstimatedTokenCount := FMinPoolSize
  else
  begin
    FEstimatedTokenCount := Round(FSourceLength * FTokensPerCharRatio);
    FEstimatedTokenCount := Round(FEstimatedTokenCount * FSafetyFactor);
  end;

  // Apply limits
  FEstimatedTokenCount := Max(FEstimatedTokenCount, FMinPoolSize);
  FEstimatedTokenCount := Min(FEstimatedTokenCount, FMaxPoolSize);

  // === PRE-ALLOCATE TOKENS ===
  PreAllocateTokens(FEstimatedTokenCount);

  // === PRINT POOL INFO ===
  {$IFDEF DEBUG}
  LogDebug('Token Pool Created:');
  LogDebug(Format('  Source: %d chars', [FSourceLength]));
  LogDebug(Format('  Ratio: %.3f tokens/char', [FTokensPerCharRatio]));
  LogDebug(Format('  Safety: %.1fx', [FSafetyFactor]));
  LogDebug(Format('  Pool: %d tokens (%.1f MB)',
    [FEstimatedTokenCount, (FEstimatedTokenCount * SizeOf(TLexerToken)) / (1024*1024)]));
  LogDebug(Format('  Limits: %d min, %d max', [FMinPoolSize, FMaxPoolSize]));

  LogDebug(Format('Source set: %d chars, pool configured with %d tokens',
    [FSourceLength, FEstimatedTokenCount]));
  {$ENDIF}

end;

function TLexerFSM.GetTokenCount: Integer;
begin
  Result := FLastUsedTokenIndex + 1;  // <-- Number of tokens actually used
end;

function TLexerFSM.GetTokenAt(Index: Integer): TLexerToken;
begin
  if (Index >= 0) and (Index <= FLastUsedTokenIndex) then
    Result := FTokenList.GetTokenDirect(Index)  // <-- Use TTokenList method
  else
    raise Exception.CreateFmt('Token index %d out of range (0..%d)',
      [Index, FLastUsedTokenIndex]);
end;

function TLexerFSM.GetHasErrors: Boolean;
begin
  Result := FErrorsFound > 0;
end;

{$IFDEF DEBUG}
function TLexerFSM.GetProcessingSpeed: Double;
begin
  if FProcessingTime > 0 then
    Result := FTokensProcessed / (FProcessingTime / 1000.0) // Tokens per second
  else
    Result := 0.0;
end;
{$ENDIF}


procedure TLexerFSM.PreAllocateTokens(EstimatedCount: Integer);
var
  i: Integer;
  Token: TLexerToken;
begin
  // Clean existing list
  FTokenList.Clear;

  // Reset counters
  FCurrentTokenIndex := 0;
  FLastUsedTokenIndex := -1;
  FTokensAllocated := 0;
  FTokensReused := 0;
  FOverflowCount := 0;

  // === USE TTokenList METHOD ===
  FTokenList.PreAllocateTokens(EstimatedCount);
  FTokensAllocated := EstimatedCount;

  {$IFDEF DEBUG}
  LogDebug(Format('Pre-allocated %d tokens using TTokenList.PreAllocateTokens, Count = %d',
    [EstimatedCount, FTokenList.Count]));
  {$ENDIF}

end;

function TLexerFSM.GetNextAvailableToken: TLexerToken;
begin
  if FCurrentTokenIndex < FTokenList.Count then
  begin
    Result := TLexerToken(FTokenList.Items[FCurrentTokenIndex]);
    Inc(FTokensReused);
  end
  else
  begin
    // POOL EXHAUSTED - CREATE NEW TOKEN
    Result := TLexerToken.Create(ttUnknown, '', 0, 0, 0, 0, nil);
    FTokenList.AddToken(Result);
    Inc(FTokensAllocated);
    Inc(FOverflowCount);
  end;

  FLastUsedTokenIndex := FCurrentTokenIndex;
  Inc(FCurrentTokenIndex);
end;

// === VIRTUAL EVENT METHODS ===

procedure TLexerFSM.DoError(const ErrorMsg: string; Position: Integer);
begin
  // Override in descendants for custom error handling
end;

procedure TLexerFSM.ResetTokenPool;
begin
  // Reset only indices, keep pre-allocated tokens
  FTokenList.ResetPool;  // <-- Use TTokenList method
  FCurrentTokenIndex := 0;
  FLastUsedTokenIndex := -1;

  {$IFDEF DEBUG}
  LogDebug('Token pool reset - reusing pre-allocated tokens');
  {$ENDIF}

end;

// === GLOBAL SETTINGS ===

procedure SetGlobalDebugMode(Enabled: Boolean);
begin
  GlobalDebugMode := Enabled;
end;

function GetGlobalLexerStatistics: string;
begin
  Result := Format('Global Lexer Statistics: %d active lexers, %d total tokens processed',
    [GlobalLexerCount, GlobalTokensProcessed]);
end;

end.

