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
  INITIAL_TOKEN_CAPACITY = 256;   // token buffer starts here and doubles on demand (no hard token cap)

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
    FTokenBuffer: array of Char;   // grows on demand (INITIAL_TOKEN_CAPACITY, then doubling)
    FTokenLength: Integer;

    // Set while lexing a number whose float suffix says SINGLE ("1.5f", "1.5!"); ProcessNumber stamps it
    // onto the token it builds and clears it. The suffix is scanned before the token object exists, so the
    // mark cannot be written straight to the token.
    FPendingSingleSuffix: Boolean;

    function GetTokenAt(Index: Integer): TLexerToken;
    function HandleTokenOverflow: TLexerToken;
    function ParseNumberDigits: Boolean;
    procedure ConsumeIntLiteralSuffix;   // drop a trailing FB integer literal suffix (U/L/LL/UL/ULL/...)
    procedure ConsumeFloatLiteralSuffix; // drop a trailing FB float literal suffix (f/F/d/D/!/#)
    procedure ResetTokenPool;
    procedure TokenBufferReset; inline;
    procedure TokenBufferAdd(C: Char); inline;
    function TokenBufferAsString: string; inline;
    procedure TokenBufferSetString(const S: string); inline;

    // === INTERNAL METHODS ===
    function GetCurrentChar: Char; inline;
    function PeekChar(Offset: Integer = 1): Char; inline;
    function IsLineContinuationHere: Boolean;   // current char '_' is a FreeBASIC line-continuation marker
    procedure ConsumeLineContinuation;          // consume '_' + rest-of-line + newline (no token emitted)
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
    function LexAmpBaseLiteral: TLexerToken;   // FreeBASIC &H/&O/&B hex/oct/bin integer literal
    function LexPrefixedString(DoEscape: Boolean): TLexerToken; // FreeBASIC !"..." / $"..." string literal
    function ProcessEscapes(const Raw: string): string;         // expand FreeBASIC escape sequences
    function ProcessString: TLexerToken;
    function ProcessComment: TLexerToken;
    function ProcessLineComment: TLexerToken;
    procedure SkipBlockComment;   // consume a FreeBASIC /' ... '/ block comment (nestable)
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
    procedure PreScanOptions;

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
  FCharConfig.Operators := ['+', '-', '*', '/', '\', '=', '<', '>', '^', '&', '|', '!', '~'];
  FCharConfig.Delimiters := ['(', ')', '[', ']', '{', '}'];
  FCharConfig.Separators := [',', ';', ':'];
  FCharConfig.Whitespace := [' ', #9];
  // cVirtualEOL ends a line without being one: AdvanceChar only counts #13/#10, so a #macro expansion
  // reads as several lines yet stays on the physical line it was invoked from.
  FCharConfig.Newlines := [#10, #13, cVirtualEOL];
  FCharConfig.Quotes := ['"'];   // only " delimits strings (BASIC v7 / FreeBASIC); ' is a comment
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
    TokenType := ttOpDot;   // member-access operator '.'
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
  // Grow on demand rather than capping token length: a long string literal (or any long token) must
  // lex without a hard limit, as it does in FreeBASIC. The buffer starts at INITIAL_TOKEN_CAPACITY and
  // doubles; almost every token is short, so a grow is rare and the fast index-write path is unchanged.
  if FTokenLength >= Length(FTokenBuffer) then
  begin
    if Length(FTokenBuffer) = 0 then
      SetLength(FTokenBuffer, INITIAL_TOKEN_CAPACITY)
    else
      SetLength(FTokenBuffer, Length(FTokenBuffer) * 2);
  end;
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
  if Len > Length(FTokenBuffer) then
    SetLength(FTokenBuffer, Len);        // grow to fit instead of truncating

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

function TLexerFSM.IsLineContinuationHere: Boolean;
// Precondition: GetCurrentChar = '_'. Returns True when this '_' is a FreeBASIC
// line-continuation marker, i.e. a STANDALONE '_' whose remainder of the physical line is
// only whitespace (optionally a ' comment) before the newline/EOF. This distinguishes it
// from '_' as an identifier character: '_foo' has a letter after the '_' (not a line end),
// and 'a_' is consumed by the identifier scanner before this point is ever reached.
var
  Off: Integer;
  Ch: Char;
begin
  Off := 1;
  Ch := PeekChar(Off);
  while (Ch = ' ') or (Ch = #9) do
  begin
    Inc(Off);
    Ch := PeekChar(Off);
  end;
  Result := (Ch = #10) or (Ch = #13) or (Ch = #0) or (Ch = '''');
end;

procedure TLexerFSM.ConsumeLineContinuation;
// Precondition: IsLineContinuationHere = True. Consumes the '_', any trailing whitespace, an
// optional trailing ' comment, and the terminating CR/LF/CRLF, so the logical line continues
// seamlessly on the next physical line. No token is emitted. AdvanceChar keeps FCurrentLine
// accurate (it treats CRLF as a single step), so token line numbers stay correct.
var
  Ch: Char;
begin
  AdvanceChar;  // consume '_'
  Ch := GetCurrentChar;
  while (Ch = ' ') or (Ch = #9) do begin AdvanceChar; Ch := GetCurrentChar; end;
  if Ch = '''' then
    while (Ch <> #0) and (Ch <> #10) and (Ch <> #13) do begin AdvanceChar; Ch := GetCurrentChar; end;
  if (Ch = #13) or (Ch = #10) then
    AdvanceChar;  // AdvanceChar consumes CRLF as one step
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
  Result.BasePrefixed := False;   // pooled object: clear it, or the previous token's "&H.." mark carries over
  Result.SingleSuffixed := False; // ditto for the "1.5f" mark (ProcessNumber sets it from FPendingSingleSuffix)

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
  OrigLen, Rewind: Integer;
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
  begin
    // Dialect-pluggable filter: a keyword tagged for the OTHER dialect is not a keyword here — emit a
    // plain identifier so the name stays usable (e.g. FB-only CONTINUE/LSET/ENUM as a variable in
    // CLASSIC). HasLineNumbers=True => CLASSIC; False => MODERN.
    if Assigned(Match.KeywordInfo) and
       (((Match.KeywordInfo.Dialect = kdModernOnly) and FLexerOptions.HasLineNumbers) or
        ((Match.KeywordInfo.Dialect = kdClassicOnly) and not FLexerOptions.HasLineNumbers)) then
    begin
      Result := CreateToken(ttIdentifier);
      Exit;
    end;

    // Spaceless mode: rewind cursor if keyword is prefix of longer token
    if (Match.MatchType = mtPrefix) and (not FLexerOptions.RequireSpacesBetweenTokens) then
    begin
      OrigLen := FTokenLength;
      Rewind := OrigLen - Match.MatchedLength;
      FTokenLength := Match.MatchedLength;
      FCurrentPosition := FCurrentPosition - Rewind;
      FCurrentColumn := FCurrentColumn - Rewind;
      FLastTokenLength := -1; // Invalidate cache
    end;
    Result := CreateToken(Match.KeywordInfo.TokenType, Match.KeywordInfo);
  end
  else
    Result := CreateToken(ttIdentifier);
end;

function TLexerFSM.ProcessNumber: TLexerToken;
var
 TokenText: string;
 i: Integer;
 HasDExp: Boolean;
begin
 Result := nil;
 TokenText := TokenBufferAsString;

 // Check if it's a line number at start of line
 // (no need for DefaultFormatSettings overhead here!)
 if FAtLineStart and IsLineNumber(TokenText) then
   Result := CreateToken(ttLineNumber)
 else
 begin
   Result := CreateToken(ttNumber);
   // FreeBASIC writes a double-precision exponent with 'D' ("743.1D-13"), meaning exactly what 'E' means.
   // The numeric conversion only understands 'E', so give the token an explicit normalized value -- its
   // text is otherwise lifted straight from the source, and a 'D' there made the literal unparsable
   // (before that, it was dropped entirely and "743.1D-13" silently became the subtraction 743.1-13).
   HasDExp := False;
   for i := 1 to Length(TokenText) do
     if (TokenText[i] = 'D') or (TokenText[i] = 'd') then begin HasDExp := True; Break; end;
   if HasDExp then
   begin
     for i := 1 to Length(TokenText) do
       if (TokenText[i] = 'D') or (TokenText[i] = 'd') then TokenText[i] := 'E';
     Result.SetExtractedValue(TokenText);
   end;
   Result.SingleSuffixed := FPendingSingleSuffix;
 end;
 // Always cleared, on BOTH branches and whichever call site got here: ProcessNumber is reached from FSM
 // paths that never scan a suffix, and a stale mark would make the NEXT literal a Single.
 FPendingSingleSuffix := False;
end;

function TLexerFSM.LexAmpBaseLiteral: TLexerToken;
// FreeBASIC &H../&O../&B.. integer literal. On entry the '&' has been consumed and added to the token
// buffer, and the current char is the base letter (H/O/B). Returns nil (consuming nothing more) when
// the '&' is NOT a base-literal prefix, so the caller falls back to "&="/concat. The base digits are
// folded into an Int64 and the token's value is set to the decimal string, so the parser handles it as
// an ordinary integer (values beyond Int64 wrap — an accepted v1 limit, like other big literals).
var
  Base, DigVal: Integer;
  Ch: Char;
  Val: Int64;

  function IsBaseDigit(C: Char; B: Integer): Boolean;
  begin
    case B of
      16: Result := C in ['0'..'9', 'A'..'F', 'a'..'f'];
      8:  Result := C in ['0'..'7'];
      2:  Result := C in ['0'..'1'];
    else  Result := False;
    end;
  end;

begin
  Result := nil;
  Ch := GetCurrentChar;   // base letter candidate
  case Ch of
    'H', 'h': Base := 16;
    'O', 'o': Base := 8;
    'B', 'b': Base := 2;
  else        Exit;       // not a base prefix
  end;
  if not IsBaseDigit(PeekChar(1), Base) then Exit;   // need at least one valid digit -> else it's '&'

  TokenBufferAdd(Ch); AdvanceChar;   // consume the base letter
  Val := 0;
  while IsBaseDigit(GetCurrentChar, Base) do
  begin
    Ch := GetCurrentChar;
    case Ch of
      '0'..'9': DigVal := Ord(Ch) - Ord('0');
      'A'..'F': DigVal := Ord(Ch) - Ord('A') + 10;
      'a'..'f': DigVal := Ord(Ch) - Ord('a') + 10;
    else        DigVal := 0;
    end;
    Val := Val * Base + DigVal;
    TokenBufferAdd(Ch); AdvanceChar;
  end;
  ConsumeIntLiteralSuffix;   // FreeBASIC typed integer literal: &hFFul, &b1010ULL, ... (dropped)
  Result := CreateToken(ttNumber);
  Result.SetExtractedValue(IntToStr(Val));   // logical value is decimal, not the "&H.." source text
  // ...which is why the token has to remember that it WAS base-prefixed: with the source text gone it is
  // indistinguishable from a plain decimal literal, and FreeBASIC's type ladder treats the two differently
  // (a decimal 4294967295 is an unsigned ULong; &HFFFFFFFF is not).
  Result.BasePrefixed := True;
end;

function TLexerFSM.ProcessEscapes(const Raw: string): string;
// Expand the FreeBASIC escape sequences accepted inside an escaped string literal (!"...").
// Recognised: \a \b \f \n \l \r \t \v \\ \" \' ; \DDD decimal (1-3 digits); \&hNN hex; \&oNNN octal;
// \&bNNNNNNNN binary; \uNNNN unicode codepoint (1-4 hex digits, UTF-8 encoded). A produced NUL
// (\0 / \&h00 / ...) is the string terminator: only the bytes before it are kept (FB semantics).
var
  i, n: Integer;
  c: Char;
  Val, Cnt, d, Base: Integer;

  function HexDigit(ch: Char): Integer;
  begin
    case ch of
      '0'..'9': Result := Ord(ch) - Ord('0');
      'A'..'F': Result := Ord(ch) - Ord('A') + 10;
      'a'..'f': Result := Ord(ch) - Ord('a') + 10;
    else        Result := -1;
    end;
  end;

  procedure AppendCP(CP: Integer);
  // UTF-8 encode a codepoint into Result (our string storage is UTF-8, like WSTRING).
  begin
    if CP < $80 then
      Result := Result + Chr(CP)
    else if CP < $800 then
    begin
      Result := Result + Chr($C0 or (CP shr 6));
      Result := Result + Chr($80 or (CP and $3F));
    end
    else if CP < $10000 then
    begin
      Result := Result + Chr($E0 or (CP shr 12));
      Result := Result + Chr($80 or ((CP shr 6) and $3F));
      Result := Result + Chr($80 or (CP and $3F));
    end
    else
    begin
      Result := Result + Chr($F0 or (CP shr 18));
      Result := Result + Chr($80 or ((CP shr 12) and $3F));
      Result := Result + Chr($80 or ((CP shr 6) and $3F));
      Result := Result + Chr($80 or (CP and $3F));
    end;
  end;

  // Emit a numeric escape result. Returns False when the value is NUL (truncate the string).
  function EmitByte(Val: Integer): Boolean;
  begin
    if Val = 0 then
      Result := False
    else
    begin
      AppendCP(Val);
      Result := True;
    end;
  end;

begin
  Result := '';
  i := 1;
  n := Length(Raw);
  while i <= n do
  begin
    c := Raw[i];
    if (c <> '\') or (i = n) then
    begin
      Result := Result + c;
      Inc(i);
      Continue;
    end;
    // c = '\', and there is at least one more char
    Inc(i);
    c := Raw[i];
    case c of
      'a': begin Result := Result + Chr(7);  Inc(i); end;
      'b': begin Result := Result + Chr(8);  Inc(i); end;
      'f': begin Result := Result + Chr(12); Inc(i); end;
      'n', 'l': begin Result := Result + Chr(10); Inc(i); end;
      'r': begin Result := Result + Chr(13); Inc(i); end;
      't': begin Result := Result + Chr(9);  Inc(i); end;
      'v': begin Result := Result + Chr(11); Inc(i); end;
      '\': begin Result := Result + '\';  Inc(i); end;
      '"': begin Result := Result + '"';  Inc(i); end;
      '''': begin Result := Result + ''''; Inc(i); end;
      '0'..'9':
        begin
          // decimal, up to 3 digits
          Val := 0; Cnt := 0;
          while (i <= n) and (Cnt < 3) and (Raw[i] >= '0') and (Raw[i] <= '9') do
          begin
            Val := Val * 10 + (Ord(Raw[i]) - Ord('0'));
            Inc(i); Inc(Cnt);
          end;
          if not EmitByte(Val and $FF) then Exit;
        end;
      'u', 'U':
        begin
          // unicode codepoint, up to 4 hex digits
          Inc(i);
          Val := 0; Cnt := 0;
          while (i <= n) and (Cnt < 4) and (HexDigit(Raw[i]) >= 0) do
          begin
            Val := Val * 16 + HexDigit(Raw[i]);
            Inc(i); Inc(Cnt);
          end;
          if not EmitByte(Val) then Exit;
        end;
      '&':
        begin
          // \&hNN hex, \&oNNN octal, \&bNNNNNNNN binary
          Inc(i);
          if i > n then begin Result := Result + '&'; Continue; end;
          case Raw[i] of
            'h', 'H': Base := 16;
            'o', 'O': Base := 8;
            'b', 'B': Base := 2;
          else        Base := 0;
          end;
          if Base = 0 then
          begin
            Result := Result + '&';   // not a valid base prefix: keep literally
            Continue;
          end;
          Inc(i);
          Val := 0;
          while i <= n do
          begin
            if Base = 16 then
            begin
              d := HexDigit(Raw[i]);
              if d < 0 then Break;
            end
            else
            begin
              d := Ord(Raw[i]) - Ord('0');
              if (d < 0) or (d >= Base) then Break;
            end;
            Val := Val * Base + d;
            Inc(i);
          end;
          if not EmitByte(Val and $FF) then Exit;
        end;
    else
      // Unknown escape: keep the character literally (lenient; FB would raise a compile error).
      Result := Result + c;
      Inc(i);
    end;
  end;
end;

function TLexerFSM.LexPrefixedString(DoEscape: Boolean): TLexerToken;
// On entry the current char is the prefix ('!' or '$') and PeekChar(1) is the opening '"'.
// Consumes the prefix, the quotes, and the body. For the escaped form (!), a '\' protects the
// following char so an escaped quote (\") does not terminate the literal; escape expansion is then
// applied. For the non-escaped form ($), '\' has no special meaning and a '"' always terminates.
var
  Ch: Char;
  Raw: string;
begin
  ResetToken;
  TokenBufferAdd(GetCurrentChar);   // the prefix, for source position/length bookkeeping
  AdvanceChar;                      // consume prefix
  AdvanceChar;                      // consume opening quote
  Raw := '';
  Ch := GetCurrentChar;
  while (Ch <> #0) and (Ch <> '"') do
  begin
    if DoEscape and (Ch = '\') then
    begin
      Raw := Raw + Ch;              // keep the backslash for ProcessEscapes
      AdvanceChar;
      Ch := GetCurrentChar;
      if Ch = #0 then Break;
      Raw := Raw + Ch;              // keep the escaped char verbatim (even if it is '"')
      AdvanceChar;
    end
    else
    begin
      Raw := Raw + Ch;
      AdvanceChar;
    end;
    Ch := GetCurrentChar;
  end;
  if Ch = '"' then AdvanceChar;     // consume closing quote
  if DoEscape then Raw := ProcessEscapes(Raw);
  Result := CreateToken(ttStringLiteral);
  Result.SetExtractedValue(Raw);
end;

function TLexerFSM.ProcessString: TLexerToken;
var
  S: string;
begin
  Result := CreateToken(ttStringLiteral);
  // Remove surrounding quotes, then collapse each doubled "" to a single literal quote (standard BASIC
  // string escaping; the scanner kept both quotes of a "" pair in the raw token text).
  S := Result.Value;
  if (Length(S) >= 2) and (S[1] = '"') and (S[Length(S)] = '"') then
  begin
    S := Copy(S, 2, Length(S) - 2);
    if Pos('""', S) > 0 then
      S := StringReplace(S, '""', '"', [rfReplaceAll]);
    Result.Value := S;
  end;
end;

function TLexerFSM.ProcessComment: TLexerToken;
begin
  Result := CreateToken(ttCommentText);
end;

procedure TLexerFSM.SkipBlockComment;
// Precondition: GetCurrentChar = '/' and PeekChar(1) = ''''  (start of a FreeBASIC /' block comment).
// Consumes the whole comment up to and including the matching '/ (or EOF), honoring nesting.
var
  Depth: Integer;
begin
  AdvanceChar;               // '/'
  AdvanceChar;               // opening quote
  Depth := 1;
  while (Depth > 0) and (GetCurrentChar <> #0) do
  begin
    if (GetCurrentChar = '/') and (PeekChar(1) = '''') then
    begin
      AdvanceChar; AdvanceChar; Inc(Depth);   // nested /'
    end
    else if (GetCurrentChar = '''') and (PeekChar(1) = '/') then
    begin
      AdvanceChar; AdvanceChar; Dec(Depth);    // closing '/
    end
    else
      AdvanceChar;
  end;
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
  if Options.AllowPartialMatch then
    Options.MaxResults := 0  // No limit - need all matches for longest selection
  else
    Options.MaxResults := 1;

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
  i, SignLen: Integer;
  HasMantissaDigit: Boolean;
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

  // Scientific notation — only when a mantissa digit has already been lexed (either by the loop above or
  // by the caller, e.g. the leading digit of "1e10" or the "1" of "1.e10"). Without this guard the '.'
  // lexer path — which calls this before deciding number-vs-member-dot — would treat the leading 'e' of a
  // member name like "g.edges" as an exponent marker and consume it, corrupting the identifier to "dges".
  HasMantissaDigit := False;
  for i := 0 to FTokenLength - 1 do
    if (FTokenBuffer[i] >= '0') and (FTokenBuffer[i] <= '9') then begin HasMantissaDigit := True; Break; end;
  // FreeBASIC spells the exponent with 'E' or with 'D' ("743.1D-13"), the latter forcing DOUBLE precision;
  // both mean the same magnitude, so normalize 'D' to 'E' -- the numeric conversion only understands 'E',
  // and a dropped 'D' turned "743.1D-13" into the SUBTRACTION 743.1-13 = 730.1, silently.
  // The marker may also stand alone with no exponent digits ("123e", "-123.0d"): that is just a
  // double-precision literal. Only commit to the marker (and to a sign) when a digit really follows, so a
  // bare 'e'/'d' is dropped rather than left in the buffer as an unparsable "123e".
  if ((CurrentChar = 'E') or (CurrentChar = 'e') or (CurrentChar = 'D') or (CurrentChar = 'd')) and
     HasMantissaDigit then
  begin
    SignLen := 0;
    if (PeekChar(1) = '+') or (PeekChar(1) = '-') then SignLen := 1;
    if (PeekChar(1 + SignLen) >= '0') and (PeekChar(1 + SignLen) <= '9') then
    begin
      // Keep the marker VERBATIM in the buffer: a token's text is extracted lazily from the SOURCE by
      // (start, length), so the buffer only fixes the length -- rewriting a character in it would not
      // change the token's value. ProcessNumber does the D->E normalization on the value instead.
      TokenBufferAdd(CurrentChar);
      AdvanceChar;                         // consume the marker
      CurrentChar := GetCurrentChar;

      if (CurrentChar = '+') or (CurrentChar = '-') then
      begin
        TokenBufferAdd(CurrentChar);
        AdvanceChar;
        CurrentChar := GetCurrentChar;
      end;

      while (CurrentChar >= '0') and (CurrentChar <= '9') do
      begin
        TokenBufferAdd(CurrentChar);
        AdvanceChar;
        CurrentChar := GetCurrentChar;
      end;
    end
    else
    begin
      // Bare marker: consume it (it belongs to the literal) but add nothing -- the value is the mantissa.
      AdvanceChar;
      CurrentChar := GetCurrentChar;
    end;
  end;
end;

procedure TLexerFSM.ConsumeIntLiteralSuffix;
// FreeBASIC integer literal TYPE suffix: an optional 'U' (unsigned) then an optional size letter
// (LL / L / S / B / I), case-insensitive — e.g. 100L, 5UL, 18446744073709551615ULL, &hFFul. The suffix
// is consumed and DROPPED (not added to the token text), so the literal's value is the number itself.
// (We do not model per-width overflow; a big unsigned value is already handled as two's-complement.)
// A number can never be validly glued to an identifier without an operator, so treating a trailing
// U/L run as a suffix is unambiguous.
var C: Char;
begin
  C := GetCurrentChar;
  if (C = 'U') or (C = 'u') then
  begin
    AdvanceChar; C := GetCurrentChar;
    if (C = 'L') or (C = 'l') then
    begin
      AdvanceChar;
      if (GetCurrentChar = 'L') or (GetCurrentChar = 'l') then AdvanceChar;   // ULL
    end
    else if C in ['S', 's', 'B', 'b', 'I', 'i'] then
      AdvanceChar;                                                            // US / UB / UI
  end
  else if (C = 'L') or (C = 'l') then
  begin
    AdvanceChar;
    if (GetCurrentChar = 'L') or (GetCurrentChar = 'l') then AdvanceChar;     // LL
  end
  else if C = '%' then
    AdvanceChar;   // "64%" (Integer). Unambiguous after a number: BASIC's modulo is MOD, never '%'.
  // NOT '&' (the Long suffix, "64&"): it collides with the concatenation operator and the &H/&O/&B
  // literal prefix, so a bare trailing '&' stays a concat. Rare enough to leave alone.
end;

procedure TLexerFSM.ConsumeFloatLiteralSuffix;
// FreeBASIC float literal TYPE suffix: 'f'/'F' (Single), 'd'/'D' (Double), '!' (Single), '#' (Double).
// The suffix is consumed and dropped from the token text -- the literal's value is the number either way,
// and the bank is float regardless -- but a SINGLE one is REMEMBERED in FPendingSingleSuffix: it decides
// the literal's TYPE, and so whether it prints 7 significant digits and keeps an expression single.
// ProcessNumber, which builds the token, is what stamps it (the token does not exist yet here).
// A number can never be validly glued to an identifier, so a trailing f/d is an unambiguous suffix. For
// f/d/F/D we require the following char to be a non-alnum so a VB-style exponent (1.5D10) isn't mistaken
// for a suffix.
var C: Char;
begin
  C := GetCurrentChar;
  if (C = 'f') or (C = 'F') or (C = 'd') or (C = 'D') then
  begin
    if not (PeekChar(1) in ['0'..'9', 'A'..'Z', 'a'..'z', '_']) then
    begin
      FPendingSingleSuffix := (C = 'f') or (C = 'F');
      AdvanceChar;
    end;
  end
  else if (C = '!') or (C = '#') then
  begin
    FPendingSingleSuffix := (C = '!');
    AdvanceChar;
  end;
end;

// === MAIN LEXING METHOD ===

function TLexerFSM.NextToken: TLexerToken;
var
  CurrentChar: Char;
  HashStem: string;   // identifier text before a '#' (to disambiguate PRINT#/GET# vs DOPEN#1 vs A#)
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

    // === WHITESPACE + LINE CONTINUATION - VERY FAST SKIP ===
    // A FreeBASIC '_' line-continuation marker joins the current line to the next physical
    // line. It is consumed here (transparently, like whitespace) together with the newline,
    // so the two lines lex as one logical line. Identifiers are unaffected: a continuation
    // '_' is a standalone token followed only by whitespace/comment before the newline, while
    // '_foo'/'a_' are handled by the identifier scanner (see IsLineContinuationHere).
    while True do
    begin
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

      if (CurrentChar = '_') and IsLineContinuationHere then
      begin
        ConsumeLineContinuation;
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
        Continue;  // keep skipping whitespace on the continued line
      end;

      Break;
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

      // === '?' IS AN ALIAS FOR PRINT (Commodore BASIC and FreeBASIC) ===
      // Emit a PRINT token (ttOutputCommand, value 'PRINT') so the statement parser dispatches to
      // ParsePrintStatement. '?' inside a string literal never reaches here (the '"' handler consumes
      // the whole string first).
      '?':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttOutputCommand);
          Result.SetExtractedValue(kPRINT);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      // === SINGLE CHAR OPERATORS - ZERO PROCESSING ===
      // Arithmetic operators. FreeBASIC compound assignment ("op="): if '=' follows, emit a single
      // ttCompoundAssign token whose value is the operator symbol (the trailing '=' is consumed but not
      // kept), which the parser desugars to "lhs = lhs op rhs".
      '+':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          if GetCurrentChar = '=' then begin AdvanceChar; Result := CreateToken(ttCompoundAssign); end
          else Result := CreateToken(ttOpAdd);
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
          if GetCurrentChar = '=' then begin AdvanceChar; Result := CreateToken(ttCompoundAssign); end
          // FreeBASIC pointer-to-member "->": lexes as the member-access operator (our UDT pointers
          // carry the record handle directly, so p->field is the same as p.field).
          else if GetCurrentChar = '>' then begin AdvanceChar; Result := CreateToken(ttOpDot); end
          else Result := CreateToken(ttOpSub);
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
          if GetCurrentChar = '=' then begin AdvanceChar; Result := CreateToken(ttCompoundAssign); end
          else Result := CreateToken(ttOpMul);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      '/':
        begin
          // FreeBASIC block comment "/' ... '/" (nestable). Transparent like whitespace — it can appear
          // inline (code before/after on the same line), so skip it and return the next real token rather
          // than emitting a comment/EOL token.
          if PeekChar(1) = '''' then
          begin
            SkipBlockComment;
            Result := NextToken;
            {$IFDEF DEBUG}
            if FDebugMode then
              FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
            {$ENDIF}
            Exit;
          end;
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          if GetCurrentChar = '=' then begin AdvanceChar; Result := CreateToken(ttCompoundAssign); end
          else Result := CreateToken(ttOpDiv);
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      '\':
        begin
          // FreeBASIC integer division; compound "\=" emits a ttCompoundAssign (value '\').
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          if GetCurrentChar = '=' then begin AdvanceChar; Result := CreateToken(ttCompoundAssign); end
          else Result := CreateToken(ttOpIntDiv);
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
          if GetCurrentChar = '=' then begin AdvanceChar; Result := CreateToken(ttCompoundAssign); end
          else Result := CreateToken(ttOpPow);
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

      // === AT OPERATOR (proc-address prefix: @subname) ===
      '&':
        begin
          // FreeBASIC string concatenation. Compound "&=" emits a ttCompoundAssign (value '&').
          // "&H../&O../&B.." are hex/oct/bin integer literals (handled by LexAmpBaseLiteral).
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := LexAmpBaseLiteral;   // &H/&O/&B integer literal, or nil if not a base prefix
          if Result = nil then
          begin
            if GetCurrentChar = '=' then begin AdvanceChar; Result := CreateToken(ttCompoundAssign); end
            else Result := CreateToken(ttOpConcat);
          end;
          {$IFDEF DEBUG}
          if FDebugMode then
            FProcessingTime := FProcessingTime + MilliSecondsBetween(Now, StartTime);
          {$ENDIF}
          Exit;
        end;

      '@':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar);
          AdvanceChar;
          Result := CreateToken(ttOpAt);
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

          // Fast identifier loop - NO FSM. '#' is handled separately below: it is ambiguous between the
          // double-precision suffix (A#), the PRINT#/INPUT#/GET# keywords, and a Commodore file-handle
          // prefix (DOPEN#1), so gluing it unconditionally (and swallowing the following digits) is wrong.
          CurrentChar := GetCurrentChar;
          while ((CurrentChar >= 'A') and (CurrentChar <= 'Z')) or
                ((CurrentChar >= 'a') and (CurrentChar <= 'z')) or
                ((CurrentChar >= '0') and (CurrentChar <= '9')) or
                (CurrentChar = '_') or
                (CurrentChar = '$') or   // String
                (CurrentChar = '%') or   // Integer (16 bit)
                (CurrentChar = '!') do   // Single precision (32 bit)
          begin
            TokenBufferAdd(CurrentChar);
            AdvanceChar;
            CurrentChar := GetCurrentChar;
          end;

          // '#' disambiguation (Commodore-friendly):
          //   PRINT# / INPUT# / GET#      -> '#' is part of the keyword: glue it, then stop.
          //   <name>#<digit>  (DOPEN#1)   -> '#' begins a file-handle number: stop BEFORE '#' so it lexes
          //                                  as a separate file-handle-prefix token (like the spaced form).
          //   <name>#<other>  (A#)        -> '#' is the double-precision type suffix: glue it, then stop.
          if CurrentChar = '#' then
          begin
            HashStem := UpperCase(TokenBufferAsString);
            if (HashStem = 'PRINT') or (HashStem = 'INPUT') or (HashStem = 'GET') or
               not ((PeekChar(1) >= '0') and (PeekChar(1) <= '9')) then
            begin
              TokenBufferAdd('#');
              AdvanceChar;
            end;
            // else: '#' + digit and not a #-keyword -> leave '#' for the file-handle-prefix token.
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

          ConsumeIntLiteralSuffix;   // FreeBASIC typed integer literal: 100L, 5UL, ...ULL (dropped)
          ConsumeFloatLiteralSuffix; // FreeBASIC typed float literal: 1.0f, 2.5d, 3! , 4# (dropped)
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
          begin
            ConsumeFloatLiteralSuffix;   // FreeBASIC ".5f" / ".25d" typed float literal (dropped)
            Result := ProcessNumber;
          end
          else
            Result := CreateToken(ttOpDot);  // ← Otherwise it's the member-access operator '.'
          Exit;
        end;

      // === STRING LITERALS - FAST PARSING ===
      '"':
        begin
          ResetToken;
          TokenBufferAdd(CurrentChar); // Opening quote
          AdvanceChar;

          // Fast string loop. A plain "..." literal is NON-escaped (FreeBASIC default): '\' is an
          // ordinary character. Escape processing happens only for the prefixed !"..." form
          // (LexPrefixedString). The one in-string escape here is the standard BASIC doubled quote: a ""
          // inside the string is a single literal '"' and does NOT terminate it (e.g. "say ""hi"""). The
          // buffer is kept byte-for-byte with the source (the token value is extracted from the source by
          // StartPos/Length), so both quotes of a "" are buffered; ProcessString un-doubles them.
          CurrentChar := GetCurrentChar;
          while CurrentChar <> #0 do
          begin
            if CurrentChar = '"' then
            begin
              if PeekChar(1) = '"' then
              begin
                TokenBufferAdd(CurrentChar); AdvanceChar;      // first quote of the "" pair
                TokenBufferAdd(GetCurrentChar); AdvanceChar;   // second quote
                CurrentChar := GetCurrentChar;
                Continue;
              end;
              Break;   // a lone '"' terminates the string
            end;
            TokenBufferAdd(CurrentChar);
            AdvanceChar;
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
          // Apostrophe = line comment (FreeBASIC/QBasic style). NOT a string delimiter — in
          // both BASIC v7 (only ") and FreeBASIC, ' is never a string quote (that is Free
          // Pascal's convention). Skip to end of line; the comment token is discarded by the
          // parser like REM. (Text is not buffered, so a long comment can't overflow the token.)
          ResetToken;
          AdvanceChar;                 // consume the apostrophe
          CurrentChar := GetCurrentChar;
          while (CurrentChar <> #0) and (CurrentChar <> #10) and (CurrentChar <> #13) do
          begin
            AdvanceChar;
            CurrentChar := GetCurrentChar;
          end;

          // Emit end-of-line: the comment runs to the line end, so to the parser the line
          // simply ends here (the real newline that follows yields a second, harmless EOL that
          // statement/program loops skip). This keeps comments fully transparent everywhere
          // (after a statement's arguments, inside bodies, etc.).
          Result := ProcessNewline;
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

      // === FreeBASIC ESCAPED / NON-ESCAPED STRING LITERAL PREFIXES ===
      // !"..."  -> string literal WITH escape-sequence processing (\n \t \\ \" \&hNN \uNNNN ...).
      // $"..."  -> string literal explicitly WITHOUT escape processing; since plain "..." is already
      //            non-escaped (our default), the '$' is simply consumed and the body taken verbatim.
      // A '!' or '$' NOT immediately followed by '"' is not a string prefix: fall through to the FSM
      // (preserves '!' for FSM error handling and '$' as a lone unexpected char, unchanged behaviour).
      '!', '$':
        begin
          if PeekChar(1) = '"' then
            Result := LexPrefixedString(CurrentChar = '!')
          else
            Result := NextTokenFSM;
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

procedure TLexerFSM.PreScanOptions;
var
  i, Len: Integer;
  Line: string;
  P: Integer;
  UpperLine: string;
begin
  // Pre-scan source for OPTION directives that affect lexing
  // Must be called AFTER setting Source and BEFORE ScanAllTokensFast
  if System.Length(FSource) = 0 then Exit;

  i := 1;
  Len := System.Length(FSource);
  while i <= Len do
  begin
    // Extract line
    P := i;
    while (i <= Len) and (FSource[i] <> #10) and (FSource[i] <> #13) do
      Inc(i);
    Line := Trim(Copy(FSource, P, i - P));
    // Skip line endings
    if (i <= Len) and (FSource[i] = #13) then Inc(i);
    if (i <= Len) and (FSource[i] = #10) then Inc(i);

    // Skip optional line number
    P := 1;
    while (P <= System.Length(Line)) and (Line[P] >= '0') and (Line[P] <= '9') do
      Inc(P);
    while (P <= System.Length(Line)) and (Line[P] = ' ') do
      Inc(P);

    // Check if line starts with OPTION
    UpperLine := UpperCase(Copy(Line, P, System.Length(Line) - P + 1));
    if Pos(kOPTION, UpperLine) = 1 then
    begin
      if Pos(kOPTION + ' ' + kOPTION_SPACELESS, UpperLine) = 1 then
        FLexerOptions.RequireSpacesBetweenTokens := False
      else if Pos(kOPTION + ' ' + kOPTION_STRICT, UpperLine) = 1 then
        FLexerOptions.RequireSpacesBetweenTokens := True;
    end;
  end;
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

