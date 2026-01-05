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
unit SedaiParserTypes;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils,
  SedaiLexerTypes, SedaiLexerToken, SedaiExecutorTypes;

type
  // === AST NODE TYPES ===
  TASTNodeType = (
    // === LITERALS ===
    antLiteral,           // Literal values (numbers, strings)
    antIdentifier,        // Variable names, labels
    antLineNumber,        // BASIC line numbers
    antSpecialVariable,   // Special variables (TI, TI$, DT$, etc.)

    // === FUNCTIONS ===

    antMathFunction,
    antStringFunction,
    antMemoryFunction,
    antGraphicsFunction,
    antSpriteFunction,
    antInputFunction,
    antUsrFunction,
    antUserFunction,      // User-defined function call (FN)

    // === EXPRESSIONS ===
    antBinaryOp,          // Binary operations (+, -, *, /, =, <, >, etc.)
    antUnaryOp,           // Unary operations (-, NOT, etc.)
    antFunctionCall,      // Function calls (SIN(X), LEN(A$), etc.)
    antParentheses,       // Parenthesized expressions

    // === STATEMENTS ===
    antSeparator,         // Statement param/arguments separator
    antProgram,           // Root program node
    antStatement,         // Generic statement
    antAssignment,        // LET A = B, A = B
    antPrint,             // PRINT statement
    antInput,             // INPUT statement
    antGoto,              // GOTO statement
    antGosub,             // GOSUB statement
    antReturn,            // RETURN statement
    antEnd,               // END statement
    antStop,              // STOP statement
    antFast,              // FAST statement (C128 2MHz mode)
    antSlow,              // SLOW statement (C128 1MHz mode)
    antSleep,             // SLEEP statement (delay n seconds)
    antKey,               // KEY statement (define/list function keys)
    antTron,              // TRON statement (trace on - enable debug mode)
    antTroff,             // TROFF statement (trace off - disable debug mode)
    antTrap,              // TRAP statement (error handler: TRAP linenum)
    antResume,            // RESUME statement (continue after error)
    antResumeNext,        // RESUME NEXT statement (continue after error at next statement)
    antRem,               // REM statement

    // === CONTROL FLOW ===
    antIf,                // IF statement
    antThen,              // THEN statement
    antElse,              // ELSE statement
    antForLoop,           // FOR...TO...STEP...NEXT loop
    antNext,              // NEXT statement
    antWhileLoop,         // WHILE...WEND loop (if supported)
    antDoLoop,            // DO...LOOP constructs

    // === DECLARATIONS ===
    antDim,               // DIM statement
    antDef,               // DEF FN statement

    // === ARRAY ===
    antArrayAccess,      // Array element access: A(5)
    antArrayDecl,        // DIM array declaration: A(10)
    antDimensions,       // List dimensions: (5,3,2)

    // === DATA ===
    antData,              // DATA statement
    antConst,             // CONST statement
    antRead,              // READ statement
    antRestore,           // RESTORE statement
    antClear,             // CLR statement (clear variables)

    // === I/O ===
    antOpen,              // OPEN statement
    antClose,             // CLOSE statement
    antDopen,             // DOPEN statement (disk file open)
    antDclose,            // DCLOSE statement (disk file close)
    antGet,               // GET statement (non-blocking character input)
    antGetkey,            // GETKEY statement (blocking character input)
    antGetFile,           // GET# statement (get char from file)
    antInputFile,         // INPUT# statement (input from file)
    antPrintFile,         // PRINT# statement (print to file)
    antCmd,               // CMD statement (redirect output to file/device)
    antPut,               // PUT statement (if supported)
    antPrintUsing,        // PRINT USING statement (formatted output)
    antPudef,             // PUDEF statement (redefine PRINT USING symbols)
    antChar,              // CHAR statement (display character at position)

    // === SYSTEM ===
    antPoke,              // POKE statement
    antSys,               // SYS statement
    antNew,               // NEW command
    antRun,               // RUN command
    antList,              // LIST command
    antSave,              // SAVE command
    antLoad,              // LOAD command
    antVerify,            // VERIFY command
    antBload,             // BLOAD command (load bytecode)
    antBsave,             // BSAVE command (save bytecode)
    antBoot,              // BOOT command (load and run bytecode)
    antCatalog,           // CATALOG/DIR command
    antDelete,            // DELETE command (delete program lines)
    antRenumber,          // RENUMBER command (renumber program lines)
    antCopy,              // COPY/CP command (copy file)
    antScratch,           // SCRATCH command (delete file)
    antRenameFile,        // RENAME command (rename file)
    antConcat,            // CONCAT command (concatenate files)
    antMkdir,             // MKDIR/MD command (create directory)
    antChdir,             // CHDIR/CD command (change directory)
    antMove,              // MOVE/MV command (move file)
    antGraphics,          // GRAPHIC command
    antScnClr,            // SCNCLR command (clear screen)
    antBox,               // BOX command
    antCircle,            // CIRCLE command
    antDraw,              // DRAW command
    antLocate,            // LOCATE command
    antColor,             // COLOR command (set screen area colors)
    antWidth,             // WIDTH command (set line width)
    antScale,             // SCALE command (coordinate scaling)
    antPaint,             // PAINT command (flood fill)
    antWindow,            // WINDOW command (define text window)
    antSShape,            // SSHAPE command (save bitmap to string)
    antGShape,            // GSHAPE command (load string to bitmap)
    antGList,             // GLIST command (list SDL2 video modes)

    // === SOUND ===
    antVol,               // VOL command (set volume)
    antSound,             // SOUND command (sound effects)
    antEnvelope,          // ENVELOPE command (define instrument)
    antTempo,             // TEMPO command (set playback speed)
    antPlay,              // PLAY command (play music string)
    antFilter,            // FILTER command (SID filter parameters)

    // === SPRITE ===
    antSprite,            // SPRITE command (set sprite properties)
    antMovspr,            // MOVSPR command (move/position sprite)
    antSprcolor,          // SPRCOLOR command (set global multicolors)
    antSprsav,            // SPRSAV command (save/load sprite data)
    antCollision,         // COLLISION command (set collision handler)
    // Note: SPRDEF (sprite editor) will be added later
    // Sprite functions (BUMP, RSPCOLOR, RSPPOS, RSPRITE) are handled
    // via antSpriteFunction with function name in Value

    // === SPECIAL CONSTRUCTS ===
    antBlock,             // Block of statements
    antExpressionList,    // List of expressions (for PRINT, etc.)
    antParameterList,     // Function parameter list
    antArgumentList,      // Function argument list

    // === ERROR HANDLING ===
    antError,             // Error node
    antUnknown            // Unknown construct
  );

  // === PRECEDENCE LEVELS FOR PRATT PARSER ===
  TPrecedence = (
    precNone,           // No precedence
    precAssignment,     // = (assignment)
    precOr,             // OR
    precAnd,            // AND
    precEquality,       // = <> (comparison)
    precComparison,     // < > <= >=
    precTerm,           // + -
    precFactor,         // * /
    precUnary,          // - NOT
    precPower,          // ^
    precCall,           // Function calls, array access
    precPrimary         // Literals, identifiers, parentheses
  );

  // === PARSING FUNCTIONS (completamente generiche) ===
  // Usiamo solo Pointer e TObject per evitare dipendenze circolari
  TPrefixParseFn = function(Parser: Pointer; Token: TLexerToken): TObject;
  TInfixParseFn = function(Parser: Pointer; Left: TObject; Token: TLexerToken): TObject;

  // === PARSER RULE ===
  TParseRule = record
    Prefix: TPrefixParseFn;
    Infix: TInfixParseFn;
    Precedence: TPrecedence;
  end;

  // === PARSER OPTIONS ===
  TParserOptions = record
    IncludeComments: Boolean;        // Include REM statements in AST (default: False)
    IncludeLineNumbers: Boolean;     // Include line number nodes in AST (default: True)
    OptimizeAST: Boolean;           // Apply AST optimizations (default: True)
    StrictMode: Boolean;            // Strict syntax checking (default: True)
    ArrayIndexMode: TArrayIndexMode;
  end;


// === UTILITY FUNCTIONS ===

// Parse Rule Helpers
function MakeParseRule(Prefix: TPrefixParseFn; Infix: TInfixParseFn; Precedence: TPrecedence): TParseRule;
function MakePrefixRule(Prefix: TPrefixParseFn; Precedence: TPrecedence = precNone): TParseRule;
function MakeInfixRule(Infix: TInfixParseFn; Precedence: TPrecedence): TParseRule;

// Precedence Utilities
function GetTokenPrecedence(TokenType: TTokenType): TPrecedence;
function PrecedenceToString(Precedence: TPrecedence): string;

// AST Utilities
function NodeTypeToString(NodeType: TASTNodeType): string;
function IsExpressionNode(NodeType: TASTNodeType): Boolean;
function IsStatementNode(NodeType: TASTNodeType): Boolean;
function IsLiteralNode(NodeType: TASTNodeType): Boolean;

implementation

uses
  TypInfo;

// === UTILITY FUNCTIONS ===

function MakeParseRule(Prefix: TPrefixParseFn; Infix: TInfixParseFn; Precedence: TPrecedence): TParseRule;
begin
  Result.Prefix := Prefix;
  Result.Infix := Infix;
  Result.Precedence := Precedence;
end;

function MakePrefixRule(Prefix: TPrefixParseFn; Precedence: TPrecedence): TParseRule;
begin
  Result := MakeParseRule(Prefix, nil, Precedence);
end;

function MakeInfixRule(Infix: TInfixParseFn; Precedence: TPrecedence): TParseRule;
begin
  Result := MakeParseRule(nil, Infix, Precedence);
end;

function GetTokenPrecedence(TokenType: TTokenType): TPrecedence;
begin
  case TokenType of
    // Assignment
    ttDataAssignment: Result := precAssignment;

    // Bitwise OR
    ttBitwiseOR: Result := precOr;

    // Bitwise AND
    ttBitwiseAND: Result := precAnd;

    // Equality
    ttOpEq, ttOpNeq: Result := precEquality;

    // Comparison
    ttOpLt, ttOpGt, ttOpLe, ttOpGe: Result := precComparison;

    // Addition/Subtraction
    ttOpAdd, ttOpSub: Result := precTerm;

    // Multiplication/Division
    ttOpMul, ttOpDiv, ttOpMod: Result := precFactor;

    // Unary
    ttBitwiseNOT: Result := precUnary;

    // Power
    ttOpPow: Result := precPower;

    // Function calls, array access
    ttDelimParOpen, ttDelimBrackOpen: Result := precCall;

    else
      Result := precNone;
  end;
end;

function PrecedenceToString(Precedence: TPrecedence): string;
begin
  case Precedence of
    precNone: Result := 'None';
    precAssignment: Result := 'Assignment';
    precOr: Result := 'Or';
    precAnd: Result := 'And';
    precEquality: Result := 'Equality';
    precComparison: Result := 'Comparison';
    precTerm: Result := 'Term';
    precFactor: Result := 'Factor';
    precUnary: Result := 'Unary';
    precPower: Result := 'Power';
    precCall: Result := 'Call';
    precPrimary: Result := 'Primary';
    else
      Result := 'Unknown';
  end;
end;

function NodeTypeToString(NodeType: TASTNodeType): string;
begin
  Result := GetEnumName(TypeInfo(TASTNodeType), Ord(NodeType));
  // Remove 'ant' prefix
  if Copy(Result, 1, 3) = 'ant' then
    Result := Copy(Result, 4, Length(Result) - 3)
  else Result := 'Unknown';
end;

function IsExpressionNode(NodeType: TASTNodeType): Boolean;
begin
  Result := NodeType in [
    antLiteral, antIdentifier, antBinaryOp, antUnaryOp,
    antFunctionCall, antArrayAccess, antParentheses,
    antExpressionList, antParameterList, antArgumentList
  ];
end;

function IsStatementNode(NodeType: TASTNodeType): Boolean;
begin
  Result := NodeType in [
    antStatement, antAssignment, antPrint, antInput, antGoto,
    antGosub, antReturn, antEnd, antStop, antRem, antIf,
    antThen, antElse, antForLoop, antWhileLoop, antDoLoop, antDim,
    antDef, antData, antConst, antRead, antRestore, antClear,
    antOpen, antClose, antDopen, antDclose,
    antGet, antPut, antPoke, antSys, antNew, antRun, antList,
    antSave, antLoad, antVerify, antBload, antBsave, antBoot, antCatalog, antBlock,
    // Sprite commands
    antSprite, antMovspr, antSprcolor, antSprsav, antCollision
  ];
end;

function IsLiteralNode(NodeType: TASTNodeType): Boolean;
begin
  Result := NodeType in [antLiteral, antLineNumber];
end;

end.
