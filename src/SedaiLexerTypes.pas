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
unit SedaiLexerTypes;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils;

type

  TMatchType = (mtExact, mtPrefix, mtPartial, mtNone);

  // ===== LEXER FSM STATES =====
  TLexerState = (
    lsInitial,
    lsInNumber,
    lsInNumberDecimal,
    lsInString,
    lsInIdentifier,
    lsInOperator,
    lsInComment,
    lsInLineComment,
    lsError,
    lsAccept
  );

  // ===== FSM ACTIONS =====
  TLexerAction = (
    laNone,           // No action
    laStartToken,     // Start a new token
    laAppendChar,     // Append character to current token
    laAcceptToken,    // Accept current token
    laSkipChar,       // Skip current character
    laError,          // Lexical error
    laLookAhead       // Look ahead without consuming
  );

  // ===== TOKEN TYPES =====

  TTokenType = (
    // === IDENTIFIERS AND LITERALS ===
    ttKeyword,               // BASIC keyword (PRINT, IF, FOR)
    ttLabel,                 // Label for jump statements like GOTO or GOSUB
    ttIdentifier,            // Variable name
    ttNumber,                // Number (integer or float)
    ttInteger,               // Integer number (TODO)
    ttFloat,                 // Float number (TODO)
    ttStringLiteral,         // String
    ttLineNumber,            // BASIC line number

    // === WHITESPACE AND SYMBOLS ===
    ttWhitespace,         // Simple white space (TODO)
    ttTabulation,         // Simple tabulation (`CHR(9)`) (TODO)
    ttEndOfLine,          // End of line (EOL = '\n', '\r', '\r\n')
    ttEndOfFile,          // End of file (EOF)

    // === MATHEMATICAL OPERATORS ===
    ttOpAdd,                 // +
    ttOpSub,                 // -
    ttOpMul,                 // *
    ttOpDiv,                 // /
    ttOpPow,                 // ^
    ttOpMod,                 // MOD

    // === COMPARISON OPERATORS ===
    ttOpEq,                  // =
    ttOpNeq,                 // <> or !=
    ttOpLt,                  // <
    ttOpGt,                  // >
    ttOpLe,                  // <=
    ttOpGe,                  // >=

    // === BITWISE OPERATORS ===
    ttBitwiseOperator,       // Bitwise operators (AND, OR, XOR, NOT)
    ttBitwiseAND,
    ttBitwiseOR,
    ttBitwiseNOT,
    ttBitwiseXOR,
    ttBitwiseNAND,
    ttBitwiseNOR,
    ttBitwiseXNOR,

    // === DELIMITERS ===
    ttDelimParOpen,           // (
    ttDelimParClose,          // )
    ttDelimBrackOpen,         // [
    ttDelimBrackClose,        // ]
    ttDelimBraceOpen,         // {
    ttDelimBraceClose,        // }

    // === SEPARATORS ===
    ttSeparParam,            // , (comma)
    ttSeparOutput,           // ; (semicolon)
    ttSeparStmt,             // : (colon)

    // === FLOW CONTROL - CONDITIONAL ===
    //ttConditionalKeyword,     // IF, CASE, SWITCH, WHEN, etc.
    ttConditionalElse,        // ELSE, OTHERWISE, DEFAULT
    ttConditionalEnd,         // ENDIF, ENDCASE, ENDSWITCH
    ttConditionalIf,          // IF
    ttConditionalThen,        // THEN, DO (in conditional context)

    // === FLOW CONTROL - JUMP RELATED STATEMENTS ===
    ttJumpGoto,                // GOTO
    ttJumpGosub,               // GOSUB
    ttJumpKeyword,            // Generic jump call
    ttJumpLabel,              // Labels for jumps, line numbers as target
    ttJumpReturn,             // RETURN, EXIT, BREAK, CONTINUE
    ttJumpConditional,        // ON...GOTO, ON...GOSUB

    // === FLOW CONTROL - PROGRAM EXECUTION ===
    ttProgramClock,           // FAST, SLOW
    ttProgramCont,            // CONT
    ttProgramEnd,             // END
    ttProgramRun,             // RUN
    ttProgramSleep,           // SLEEP
    ttProgramFrame,           // FRAME
    ttProgramStop,            // STOP
    ttProgramWait,            // WAIT

    // === FLOW CONTROL - LOOP CONSTRUCTS ===
    ttLoopBlockEnd,           // NEXT, ENDWHILE, ENDLOOP (end)
    ttLoopBlockStart,         // FOR, DO, REPEAT, LOOP (start)
    ttLoopControl,            // TO, STEP, DOWNTO, IN, WHILE, UNTIL

    // === CODE BLOCK CONSTRUCTS ===
    ttBlockBegin,             // BEGIN, START, BLOCK
    ttBlockEnd,               // END, FINISH, ENDBLOCK

    // === PROCEDURES HANDLING ===
    ttProcedureCall,          // FN
    ttProcedureDefine,        // DEF (FN)
    ttProcedureEnd,           // ENDPROCEDURE, ENDFUNCTION
    ttProcedureStart,         // PROCEDURE, FUNCTION, DEF, SUB

    // === DATA HANDLING ===
    ttDataAssignment,         // LET, SET, :=
    ttDataClear,              // CLR
    ttConstant,               // CONST
    ttDataConstant,           // ENUM, DATA
    ttDataDeclaration,        // DIM, DECLARE, VAR
    ttDataRead,               // READ
    ttDataType,               // TYPE, STRUCT, RECORD

    // === STANDARD INPUT/OUTPUT ===
    ttInputCommand,           // INPUT, GET, KEY etc.
    ttOutputCommand,          // PRINT, etc.
    ttOutputFunction,         // CHAR

    // === FILE INPUT/OUTPUT ===
    ttFileInputCommand,       // GET#, INPUT#
    ttFileInputFunction,      // not used
    ttFileOutputCommand,      // PRINT#
    ttFileOutputFunction,     // not used

    // === INPUT/OUTPUT HANDLING===
    ttIOCommand,              // CMD

    // === DOS COMMANDS ===
    ttFileOperation,          // OPEN, CLOSE, SAVE, LOAD
    ttFileManagement,         // DELETE, RENAME, COPY, CATALOG

    // === STRING FUNCTIONS ===
    ttStringCommand,          // NOT USED
    ttStringFunction,         // LEN, MID$, LEFT$, ASC, CHR$, etc.

    // === MEMORY HANDLING (COMMANDS AND FUNCTIONS) ===
    ttMemoryCommand,          // BANK, FETCH, POKE, etc.
    ttMemoryFunction,         // FRE, PEEK, POINTER, etc.

    // === GRAPHICS HANDLING (COMMANDS AND FUNCTION) ===
    ttGraphicsCommand,        // BOX, CIRCLE, COLOR, DRAW, etc.
    ttGraphicsFunction,       // POS, RCLR, RDOT, RWINDOW, etc.

    // === SPRITES HANDLING (COMMANDS AND FUNCTIONS) ===
    ttSpriteCommand,          // COLLISION, SPRITE, MOVSPR, etc.
    ttSpriteFunction,         // BUMP, RSPCOLOR, RSPPOS, etc.

    // === SOUND HANDLING (COMMANDS AND FUNCTIONS) ===
    ttSoundCommand,           // ENVELOPE, FILTER, PLAY, SOUND, etc.
    ttSoundFunction,          // not used

    // === WEB HANDLING (COMMANDS AND FUNCTIONS) - WEB_MODE only ===
    ttWebCommand,             // SETHEADER, STATUS
    ttWebFunction,            // GET$, POST$, HTML$, URL$, HEADER$, GETRAW$, POSTRAW$
    ttWebVariable,            // METHOD$, PATH$, QUERY$

    // === MATH FUNCTIONS ===
    ttMathFunction,           // SIN, COS, TAN, etc.

    // === SPECIAL VARIABLES ===
    ttSpecialVariable,

    // === ERROR HANDLING (COMMANDS AND FUNCTIONS) ===
    ttErrorHandlingCommand,   // TRAP, RESUME, CONT
    ttErrorHandlingFunction,  // ERR$

    // === DEBUG ===
    ttDebugCommand,
    ttDebugTracingMode,

    // === MACHINE LANGUAGE HANDLING ===
    ttMonitor,                // MONITOR
    ttSysCommand,             // SYS
    ttUsrFunction,            // USR

    // === PROGRAM EDITING COMMANDS ===
    ttProgramEditing,         // PROGRAM EDITING

    // === PROGRAM EDITING COMMANDS ===
    ttCommentEnd,             // Comment end (*/, })
    ttCommentRemark,          // Single line comment (REM, //)
    ttCommentStart,           // Comment start keyword (/*, {)
    ttCommentText,            // Comment text

    // === SPECIAL INPUT DEVICES ===
    ttInputFunction,          // JOY, PEN, POT

    // === SYSTEM HANDLING ===
    ttKeyDefine,              // KEY

    // === ENVIRONMENT SETUP DIRECTIVES ===
    ttDirective,              // EXPNOTATION, maybe other directives (#DEFINE, #INCLUDE)

    // === LESSICAL ERRORS ===
    ttError,                  // Wrong token (maybe will never be used) (TODO)
    ttUnknown,                // Unknown token (maybe will never be used) (TODO)


    // === LEGACY (MUST BE CHECKED AND REMOVE OR RECLASSIFIED) ===
    ttSystemFunction,         // PEEK, POKE, SYS, FRE, etc.
    ttMultimediaCommand,      // PLAY, SOUND, SPRITE, MUSIC
    ttPrefixSpecial,          // #, $, &, % (file handles, special numbers, etc.)
    ttFileHandlePrefix,       // File handle prefix in Commodore BASIC command (PRINT#, DOPEN#, GET#, etc.)
    ttSystemCommand,          // NEW, RUN, STOP, LIST, etc.
    ttFormattingKeyword,      // PRINT, PRINT USING, PUDEF
    ttSymbol,                 // Symbol (=, <, >=) - to be gradually removed
    ttOperator,               // Math operators (+, -, *, /) - to be gradually removed
    ttSeparator,              // Separators - to be gradually removed
    ttDelimiter               // Block delimiters - to be gradually removed
  );

  // Token type set for fast classifications
  TTokenTypeSet = set of TTokenType;

  // ===== CHARACTER CLASSIFICATION =====
  TCharClass = (
    ccLetter,         // A-Z, a-z
    ccDigit,          // 0-9
    ccUnderscore,     // _
    ccQuote,          // "
    ccOperator,       // +, -, *, /, =, <, >, etc.
    ccDelimiter,      // (, ), [, ], {, }
    ccSeparator,      // ,, ;, :
    ccWhitespace,     // space, tab
    ccNewline,        // CR, LF
    ccDot,            // .
    ccOther           // Everything else
  );

  // ===== FSM TRANSITION STRUCTURE =====
  TTransition = record
    NextState: TLexerState;
    Action: TLexerAction;
    TokenType: TTokenType;
  end;

  // ===== FSM TRANSITION TABLE =====
  // [Current state, Character class] -> Transition
  TTransitionTable = array[TLexerState, TCharClass] of TTransition;

  TFastTransitionCache = record
    // Most common states
    InitialLetter: TTransition;
    InitialDigit: TTransition;
    InitialOperator: TTransition;
    InIdentifierLetter: TTransition;
    InIdentifierDigit: TTransition;
    InNumberDigit: TTransition;
    // Cache validity flag
    Valid: Boolean;
  end;

  // ===== LANGUAGE-SPECIFIC PARSING OPTIONS =====
  TLexerOptions = record
    HasLineNumbers: Boolean;        // Language supports line numbers
    LineNumbersRequired: Boolean;   // Line numbers are required
    MaxLineNumber: Integer;         // Maximum line number (e.g. 65535 for BASIC)
    RequireSpacesBetweenTokens: Boolean;  // If False, allows LETA, FORI, REMTEST, etc.
    TruncateVariableNames: Boolean;    // Truncate to 2 characters (Commodore style)
    MaxVariableNameLength: Integer;    // Maximum length (default 2 for Commodore)
    CaseSensitive: Boolean;         // Case sensitivity for keywords/identifiers
    AllowUnicodeIdentifiers: Boolean; // Unicode support in identifiers
  end;

  // ===== CHARACTER SET CONFIGURATION =====
  TCharSet = set of Char;

  // Character configuration for the lexer
  // ===== LIGHTWEIGHT TOKEN RECORD (ZERO ALLOCATION) =====
  TTokenRec = packed record
    TokenType: TTokenType;
    StartPos: Integer;    // Offset in source buffer
    Length: Word;         // Token length (max 65535 chars)
    Line: Word;           // Line number (max 65535)
    Column: Word;         // Column number (max 65535)
    KeywordInfo: Pointer; // Nil if not keyword, otherwise pointer to TKeywordInfo
  end;
  PTokenRec = ^TTokenRec;

  TCharacterConfig = record
    Letters: TCharSet;
    Digits: TCharSet;
    Operators: TCharSet;
    Delimiters: TCharSet;
    Separators: TCharSet;
    Whitespace: TCharSet;
    Newlines: TCharSet;
    Quotes: TCharSet;
    SpecialChars: TCharSet;
    DotChars: TCharSet;
  end;

  // ===== CONSTANTS FOR TOKEN CLASSIFICATIONS =====
const
  // Tokens representing keywords
  KEYWORD_TOKENS: TTokenTypeSet = [
    ttKeyword, ttLoopBlockStart, ttLoopBlockEnd,
    ttBlockBegin, ttBlockEnd, ttProcedureStart, ttProcedureEnd,
    ttJumpKeyword, ttDataDeclaration, ttDataAssignment, ttCommentRemark,
    ttBitwiseOperator
  ];

  // Numeric tokens
  NUMERIC_TOKENS: TTokenTypeSet = [
    ttNumber, ttInteger, ttFloat, ttLineNumber
  ];

  // Comment tokens
  COMMENT_TOKENS: TTokenTypeSet = [
    ttCommentStart, ttCommentEnd, ttCommentRemark,
    ttCommentText
  ];

  // Flow control tokens
  CONTROL_FLOW_TOKENS: TTokenTypeSet = [
    ttConditionalThen, ttConditionalElse,
    ttConditionalEnd, ttLoopBlockStart, ttLoopBlockEnd, ttLoopControl,
    ttJumpKeyword, ttJumpReturn, ttJumpConditional
  ];

  // Operator tokens
  OPERATOR_TOKENS: TTokenTypeSet = [
    ttOpAdd, ttOpSub, ttOpMul, ttOpDiv, ttOpPow, ttOpMod,
    ttOpEq, ttOpNeq, ttOpLt, ttOpGt, ttOpLe, ttOpGe,
    ttBitwiseOperator, ttOperator
  ];

  // Delimiter tokens
  DELIMITER_TOKENS: TTokenTypeSet = [
    ttDelimParOpen, ttDelimParClose, ttDelimBrackOpen, ttDelimBrackClose,
    ttDelimBraceOpen, ttDelimBraceClose, ttDelimiter
  ];

  // Separator tokens
  SEPARATOR_TOKENS: TTokenTypeSet = [
    ttSeparParam, ttSeparOutput, ttSeparStmt, ttSeparator
  ];

  // Whitespace and control tokens
  WHITESPACE_TOKENS: TTokenTypeSet = [
    ttWhitespace, ttTabulation, ttEndOfLine, ttEndOfFile
  ];

  // Error tokens
  ERROR_TOKENS: TTokenTypeSet = [
    ttError, ttUnknown
  ];

// ===== UTILITY FUNCTIONS FOR TYPES =====

// Check if a token type is a keyword
function IsKeywordTokenType(TokenType: TTokenType): Boolean; inline;

// Check if a token type is numeric
function IsNumericTokenType(TokenType: TTokenType): Boolean; inline;

// Check if a token type is a comment
function IsCommentTokenType(TokenType: TTokenType): Boolean; inline;

// Check if a token type is an operator
function IsOperatorTokenType(TokenType: TTokenType): Boolean; inline;

// Check if a token type is a delimiter
function IsDelimiterTokenType(TokenType: TTokenType): Boolean; inline;

// Check if a token type is whitespace
function IsWhitespaceTokenType(TokenType: TTokenType): Boolean; inline;

// Get human-readable name of token type
function GetTokenTypeName(TokenType: TTokenType): string;

// Get token category
function GetTokenCategory(TokenType: TTokenType): string;

// Check if a token is flow control
function IsControlFlowTokenType(TokenType: TTokenType): Boolean; inline;

implementation

uses
  TypInfo;

// ===== UTILITY FUNCTIONS IMPLEMENTATION =====

function IsKeywordTokenType(TokenType: TTokenType): Boolean;
begin
  Result := TokenType in KEYWORD_TOKENS;
end;

function IsNumericTokenType(TokenType: TTokenType): Boolean;
begin
  Result := TokenType in NUMERIC_TOKENS;
end;

function IsCommentTokenType(TokenType: TTokenType): Boolean;
begin
  Result := TokenType in COMMENT_TOKENS;
end;

function IsOperatorTokenType(TokenType: TTokenType): Boolean;
begin
  Result := TokenType in OPERATOR_TOKENS;
end;

function IsDelimiterTokenType(TokenType: TTokenType): Boolean;
begin
  Result := TokenType in DELIMITER_TOKENS;
end;

function IsWhitespaceTokenType(TokenType: TTokenType): Boolean;
begin
  Result := TokenType in WHITESPACE_TOKENS;
end;

function IsControlFlowTokenType(TokenType: TTokenType): Boolean;
begin
  Result := TokenType in CONTROL_FLOW_TOKENS;
end;

function GetTokenTypeName(TokenType: TTokenType): string;
begin
  Result := GetEnumName(TypeInfo(TTokenType), Ord(TokenType));
  // Remove 'tt' prefix if present
  if Copy(Result, 1, 2) = 'tt' then
    Result := Copy(Result, 3, Length(Result) - 2);
end;

function GetTokenCategory(TokenType: TTokenType): string;
begin
  if IsKeywordTokenType(TokenType) then
    Result := 'Keyword'
  else if IsNumericTokenType(TokenType) then
    Result := 'Numeric'
  else if IsCommentTokenType(TokenType) then
    Result := 'Comment'
  else if IsOperatorTokenType(TokenType) then
    Result := 'Operator'
  else if IsDelimiterTokenType(TokenType) then
    Result := 'Delimiter'
  else if IsWhitespaceTokenType(TokenType) then
    Result := 'Whitespace'
  else if IsControlFlowTokenType(TokenType) then
    Result := 'Control Flow'
  else
    case TokenType of
      ttIdentifier: Result := 'Identifier';
      ttStringLiteral: Result := 'String';
      ttLabel: Result := 'Label';
      ttDirective: Result := 'Directive';
      ttError, ttUnknown: Result := 'Error';
      else
        Result := 'Other';
    end;
end;

end.
