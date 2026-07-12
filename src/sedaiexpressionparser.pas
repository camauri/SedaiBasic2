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
unit SedaiExpressionParser;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Variants,
  SedaiLexerTypes, SedaiLexerToken, SedaiTokenList,
  SedaiParserTypes, SedaiAST, SedaiParserContext, SedaiPackratCore,
  SedaiBasicKeywords;

type
  { TExpressionParser - Streamlined Pratt-style expression parser }
  TExpressionParser = class(TPackratCore)
  private
    procedure InitializePrattRules;
    function GetCurrentPrecedence: TPrecedence; inline;
    {$IFDEF DEBUG}
    procedure LogDebug(const Msg: string); inline;
    procedure LogVerbose(const Msg: string); inline;
    {$ENDIF}

  public
    // Active dialect, mirrored from TPackratParser by ApplyDialectProfile. The expression grammar is
    // almost dialect-neutral; the exceptions are spellings that exist only in FreeBASIC (MODERN) and
    // must NOT be silently accepted while compiling Commodore BASIC v7 (CLASSIC).
    ModernMode: Boolean;
    // Current WITH-block object (M3.2): a leading '.field' resolves against this (cloned).
    // nil when not inside a WITH; set/restored by the statement parser's ParseWith.
    WithObject: TASTNode;

    constructor Create;
    procedure SetContext(AContext: TParserContext);

    // === MAIN EXPRESSION PARSING (hot path) ===
    function ParseExpression(Precedence: TPrecedence = precNone): TASTNode;
    function ParsePrimaryExpression: TASTNode;
    function ParseExpressionList(Delimiter: TTokenType = ttSeparParam): TASTNode;

    // === PREFIX PARSING FUNCTIONS ===
    function ParseNumber(Token: TLexerToken): TASTNode;
    function ParseString(Token: TLexerToken): TASTNode;
    function ParseIdentifier(Token: TLexerToken): TASTNode;
    function ParseLineNumber(Token: TLexerToken): TASTNode;
    function ParseUnaryPlus(Token: TLexerToken): TASTNode;
    function ParseUnaryMinus(Token: TLexerToken): TASTNode;
    function ParseLogicalNot(Token: TLexerToken): TASTNode;
    function ParseParentheses(Token: TLexerToken): TASTNode;
    function ParseFunctionCall(Token: TLexerToken): TASTNode;
    function ParseMathFunction(Token: TLexerToken): TASTNode;
    function ParseStringFunction(Token: TLexerToken): TASTNode;
    function ParseMemoryFunction(Token: TLexerToken): TASTNode;
    function ParseGraphicsFunction(Token: TLexerToken): TASTNode;
    function ParseGraphicsCommandForm(Token: TLexerToken): TASTNode;  // FreeBASIC SCREEN(row, col [, flag])
    function ParseSpriteFunction(Token: TLexerToken): TASTNode;
    function ParseInputFunction(Token: TLexerToken): TASTNode;
    function ParseInputFunctionForm(Token: TLexerToken): TASTNode;   // FreeBASIC INPUT(n [, [#]f])
    function ParseUsrFunction(Token: TLexerToken): TASTNode;
    function ParseUserFunction(Token: TLexerToken): TASTNode;
    function ParseProcAddress(Token: TLexerToken): TASTNode;   // @subname → antProcAddress (M5.2)
    function ParseNew(Token: TLexerToken): TASTNode;           // NEW T [(args)] → antNew (FreeBASIC)
    function ParseTypeConstructor(Token: TLexerToken): TASTNode; // type<T>(args) → anonymous UDT temporary
    function ParseCast(Token: TLexerToken): TASTNode;          // CAST/CPTR(type, expr) → antCast
    function ParseSizeOfPtrType(Token: TLexerToken): TASTNode; // SIZEOF(<type> PTR) → SIZEOF("T PTR"); nil if not a pointer type
    function ParseDeref(Token: TLexerToken): TASTNode;         // *ptr → antDeref (FreeBASIC pointers)
    function ParseThreadCreate(Token: TLexerToken): TASTNode;  // THREADCREATE(@sub, param) (M5.2)
    function ParseThreadSelf(Token: TLexerToken): TASTNode;    // THREADSELF() → antThreadSelf (M5.5)
    function ParseThreadCall(Token: TLexerToken): TASTNode;    // THREADCALL sub(arg) → antThreadCreate (M5.5)
    function ParseMutexCreate(Token: TLexerToken): TASTNode;   // MUTEXCREATE() → antMutexCreate (M5.4)
    function ParseCondCreate(Token: TLexerToken): TASTNode;    // CONDCREATE() → antCondCreate (M5.4)
    function ParseSpecialVariable(Token: TLexerToken): TASTNode;
    {$IFDEF WEB_MODE}
    function ParseWebFunction(Token: TLexerToken): TASTNode;
    function ParseWebVariable(Token: TLexerToken): TASTNode;
    {$ENDIF}

    // === INFIX PARSING FUNCTIONS ===
    function ParseBinaryOperator(Left: TASTNode; Token: TLexerToken): TASTNode;
    function ParseAssignment(Left: TASTNode; Token: TLexerToken): TASTNode;
    function ParseArrayAccess(Left: TASTNode; Token: TLexerToken): TASTNode;
    function ParseMemberAccess(Left: TASTNode; Token: TLexerToken): TASTNode; // record.field
    function ParseWithMember(Token: TLexerToken): TASTNode; // leading '.field' inside WITH
    function ParsePower(Left: TASTNode; Token: TLexerToken): TASTNode; // Right-associative

    // === UTILITY PARSING ===
    function ParseArgumentList: TASTNode;
    function ParseParameterList: TASTNode;

    // === VALIDATION ===
    function ValidateExpression(Node: TASTNode): Boolean;
  end;

// === STATIC PARSING FUNCTIONS (for function pointers) ===
function StaticParseNumber(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseString(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseIdentifier(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseLineNumber(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseUnaryPlus(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseUnaryMinus(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseLogicalNot(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseParentheses(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseFunctionCall(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseMathFunction(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseStringFunction(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseMemoryFunction(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseGraphicsFunction(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseGraphicsCommandForm(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseSpriteFunction(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseInputFunctionForm(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseInputFunction(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseUsrFunction(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseUserFunction(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseProcAddress(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseNew(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseTypeConstructor(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseDeref(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseThreadCreate(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseThreadSelf(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseThreadCall(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseMutexCreate(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseCondCreate(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseSpecialVariable(Parser: Pointer; Token: TLexerToken): TObject;
{$IFDEF WEB_MODE}
function StaticParseWebFunction(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseWebVariable(Parser: Pointer; Token: TLexerToken): TObject;
{$ENDIF}

function StaticParseBinaryOperator(Parser: Pointer; Left: TObject; Token: TLexerToken): TObject;
function StaticParseAssignment(Parser: Pointer; Left: TObject; Token: TLexerToken): TObject;
function StaticParseArrayAccess(Parser: Pointer; Left: TObject; Token: TLexerToken): TObject;
function StaticParseMemberAccess(Parser: Pointer; Left: TObject; Token: TLexerToken): TObject;
function StaticParseWithMember(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParsePower(Parser: Pointer; Left: TObject; Token: TLexerToken): TObject;

function StaticParseCommaDebug(Parser: Pointer; Token: TLexerToken): TObject;

// === FACTORY FUNCTIONS ===
function CreateExpressionParser: TExpressionParser;

implementation

uses
  Math;

// === STATIC FUNCTIONS IMPLEMENTATION ===

function StaticParseNumber(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseNumber(Token);
end;

function StaticParseString(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseString(Token);
end;

function StaticParseIdentifier(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseIdentifier(Token);
end;

function StaticParseBase(Parser: Pointer; Token: TLexerToken): TObject;
// FreeBASIC BASE.field / BASE.method() inside a derived method: `base` names the base subobject, which in
// our inheritance model shares the same instance fields as `this`, so lower it to the THIS identifier (the
// member-access infix rule then resolves `base.field` as `this.field`). The BASEREF attribute marks it so
// a `base.method()` call is dispatched non-virtually against the parent type (a super call), rather than
// virtually against the instance's runtime type. (The BASE(args) constructor-call form is a statement.)
begin
  Result := TASTNode.CreateWithValue(antIdentifier, 'THIS', Token);
  TASTNode(Result).Attributes.Values['BASEREF'] := '1';
end;

function StaticParseLineNumber(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseLineNumber(Token);
end;

function StaticParseUnaryPlus(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseUnaryPlus(Token);
end;

function StaticParseUnaryMinus(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseUnaryMinus(Token);
end;

function StaticParseLogicalNot(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseLogicalNot(Token);
end;

function StaticParseParentheses(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseParentheses(Token);
end;

function StaticParseFunctionCall(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseFunctionCall(Token);
end;

function StaticParseMathFunction(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseMathFunction(Token);
end;

function StaticParseStringFunction(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseStringFunction(Token);
end;

function StaticParseMemoryFunction(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseMemoryFunction(Token);
end;

function StaticParseGraphicsFunction(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseGraphicsFunction(Token);
end;

function StaticParseGraphicsCommandForm(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseGraphicsCommandForm(Token);
end;

function StaticParseSpriteFunction(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseSpriteFunction(Token);
end;

function StaticParseInputFunctionForm(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseInputFunctionForm(Token);
end;

function StaticParseInputFunction(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseInputFunction(Token);
end;

function StaticParseUsrFunction(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseUsrFunction(Token);
end;

function StaticParseUserFunction(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseUserFunction(Token);
end;

function StaticParseProcAddress(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseProcAddress(Token);
end;

function StaticParseTypeConstructor(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseTypeConstructor(Token);
end;

function StaticParseNew(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseNew(Token);
end;

function StaticParseDeref(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseDeref(Token);
end;

function StaticParseThreadCreate(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseThreadCreate(Token);
end;

function StaticParseMutexCreate(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseMutexCreate(Token);
end;

function StaticParseCondCreate(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseCondCreate(Token);
end;

function StaticParseThreadSelf(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseThreadSelf(Token);
end;

function StaticParseThreadCall(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseThreadCall(Token);
end;

function StaticParseSpecialVariable(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseSpecialVariable(Token);
end;

{$IFDEF WEB_MODE}
function StaticParseWebFunction(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseWebFunction(Token);
end;

function StaticParseWebVariable(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseWebVariable(Token);
end;
{$ENDIF}

function StaticParseBinaryOperator(Parser: Pointer; Left: TObject; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseBinaryOperator(TASTNode(Left), Token);
end;

function StaticParseAssignment(Parser: Pointer; Left: TObject; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseAssignment(TASTNode(Left), Token);
end;

function StaticParseArrayAccess(Parser: Pointer; Left: TObject; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseArrayAccess(TASTNode(Left), Token);
end;

function StaticParseMemberAccess(Parser: Pointer; Left: TObject; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseMemberAccess(TASTNode(Left), Token);
end;

function StaticParseWithMember(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseWithMember(Token);
end;

function StaticParsePower(Parser: Pointer; Left: TObject; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParsePower(TASTNode(Left), Token);
end;

function StaticParseCommaDebug(Parser: Pointer; Token: TLexerToken): TObject;
begin
  //WriteLn('>>> ERROR DEBUG: ParseExpression was called on comma token!');
  //WriteLn('>>> ERROR DEBUG: Token position: Line=', Token.Line, ', Column=', Token.Column);
  //WriteLn('>>> ERROR DEBUG: Previous token was: "', TExpressionParser(Parser).Context.PreviousToken.Value, '"');
  //WriteLn('>>> ERROR DEBUG: This means a statement parser did not consume its parameters correctly');

  // Try to identify which statement is the culprit by looking at previous tokens
  //WriteLn('>>> ERROR DEBUG: Call stack context - this should help identify the problem statement');

  Result := nil; // Return nil to indicate error
end;


{ TExpressionParser }

constructor TExpressionParser.Create;
begin
  inherited Create;
  {$IFDEF DEBUG}
  LogDebug('TExpressionParser created successfully');
  {$ENDIF}
end;

procedure TExpressionParser.SetContext(AContext: TParserContext);
begin
  inherited SetContext(AContext);
  if HasValidContext then
    InitializePrattRules;
end;

procedure TExpressionParser.InitializePrattRules;
var
  Rule: TParseRule;
begin
  if not HasValidContext then
  begin
    {$IFDEF DEBUG}
    LogDebug('Cannot initialize Pratt rules without valid context');
    {$ENDIF}
    Exit;
  end;

  {$IFDEF DEBUG}
  LogDebug('Initializing Pratt expression parsing rules...');
  {$ENDIF}

  // *** TEMPORARY DEBUG RULE for comma ***
  Context.SetParseRule(ttSeparParam, MakePrefixRule(@StaticParseCommaDebug, precNone));
  Context.SetParseRule(ttSeparOutput, MakePrefixRule(@StaticParseCommaDebug, precNone));

  // === PREFIX RULES ===
  Context.SetParseRule(ttNumber, MakePrefixRule(@StaticParseNumber, precPrimary));
  Context.SetParseRule(ttInteger, MakePrefixRule(@StaticParseNumber, precPrimary));
  Context.SetParseRule(ttFloat, MakePrefixRule(@StaticParseNumber, precPrimary));
  Context.SetParseRule(ttStringLiteral, MakePrefixRule(@StaticParseString, precPrimary));
  Context.SetParseRule(ttIdentifier, MakePrefixRule(@StaticParseIdentifier, precPrimary));
  Context.SetParseRule(ttBaseCall, MakePrefixRule(@StaticParseBase, precPrimary));   // base.field -> this.field
  Context.SetParseRule(ttLineNumber, MakePrefixRule(@StaticParseLineNumber, precPrimary));

  // *** FIX: Parentesi tonde per ENTRAMBI parentheses e array access ***
  Context.SetParseRule(ttDelimParOpen, MakePrefixRule(@StaticParseParentheses, precCall));

  // === FUNCTIONS AS PREFIX EXPRESSIONS ===
  Context.SetParseRule(ttMathFunction, MakePrefixRule(@StaticParseMathFunction, precCall));
  Context.SetParseRule(ttStringFunction, MakePrefixRule(@StaticParseStringFunction, precCall));
  Context.SetParseRule(ttMemoryFunction, MakePrefixRule(@StaticParseMemoryFunction, precCall));
  Context.SetParseRule(ttGraphicsFunction, MakePrefixRule(@StaticParseGraphicsFunction, precCall));
  // SCREEN is a graphics STATEMENT keyword ("Screen 12"), but FreeBASIC also exposes it as a function
  // reading back a console cell ("Screen(row, col)"). Only that one command has a function form, so the
  // rule accepts SCREEN and rejects every other graphics command still standing in expression position.
  Context.SetParseRule(ttGraphicsCommand, MakePrefixRule(@StaticParseGraphicsCommandForm, precCall));
  Context.SetParseRule(ttSpriteFunction, MakePrefixRule(@StaticParseSpriteFunction, precCall));
  Context.SetParseRule(ttInputFunction, MakePrefixRule(@StaticParseInputFunction, precCall));
  // FreeBASIC INPUT(n [, [#]filenum]) — the FUNCTION form, which reads n characters and returns them.
  // INPUT lexes as ttInputCommand because its usual role is the INPUT statement; that is parsed by the
  // statement parser and never reaches here, so a prefix rule in expression position is unambiguous.
  // The parselet refuses the form in CLASSIC: Commodore BASIC v7 has no INPUT() function, and accepting
  // it there would let a v7 program parse into an expression the SSA never lowers -- silently yielding
  // nothing instead of the ?SYNTAX ERROR the dialect owes the programmer.
  Context.SetParseRule(ttInputCommand, MakePrefixRule(@StaticParseInputFunctionForm, precCall));
  Context.SetParseRule(ttUsrFunction, MakePrefixRule(@StaticParseUsrFunction, precCall));
  Context.SetParseRule(ttProcedureStart, MakePrefixRule(@StaticParseUserFunction, precCall));
  // M5.2 threading: @subname (proc address) and THREADCREATE(@sub, param) as value expressions.
  Context.SetParseRule(ttOpAt, MakePrefixRule(@StaticParseProcAddress, precUnary));
  // FreeBASIC heap allocation: "NEW T"/"NEW T(args)" as a value expression (a "T PTR"). NEW lexes as
  // ttProgramEditing (its classic role is the program-erase command, parsed at statement level), so a
  // prefix rule here only fires when NEW appears where an expression is expected (e.g. "p = NEW T").
  Context.SetParseRule(ttProgramEditing, MakePrefixRule(@StaticParseNew, precUnary));
  // FreeBASIC "type<T>(args)" anonymous temporary as a value expression. TYPE lexes as ttTypeDecl (its
  // statement role opens a record declaration); a prefix rule here only fires in expression position.
  Context.SetParseRule(ttTypeDecl, MakePrefixRule(@StaticParseTypeConstructor, precCall));
  Context.SetParseRule(ttThreadCreate, MakePrefixRule(@StaticParseThreadCreate, precCall));
  // M5.5: THREADSELF() value + THREADCALL sub(arg) (sugar that lowers to THREADCREATE).
  Context.SetParseRule(ttThreadSelf, MakePrefixRule(@StaticParseThreadSelf, precCall));
  Context.SetParseRule(ttThreadCall, MakePrefixRule(@StaticParseThreadCall, precCall));
  // M5.4 mutexes: MUTEXCREATE() is a value expression returning an int handle.
  Context.SetParseRule(ttMutexCreate, MakePrefixRule(@StaticParseMutexCreate, precCall));
  Context.SetParseRule(ttCondCreate, MakePrefixRule(@StaticParseCondCreate, precCall));
  Context.SetParseRule(ttSpecialVariable, MakePrefixRule(@StaticParseSpecialVariable, precPrimary));

  {$IFDEF WEB_MODE}
  // Web functions and variables
  Context.SetParseRule(ttWebFunction, MakePrefixRule(@StaticParseWebFunction, precCall));
  Context.SetParseRule(ttWebVariable, MakePrefixRule(@StaticParseWebVariable, precPrimary));
  {$ENDIF}

  // Unary operators (PREFIX only)
  Context.SetParseRule(ttOpAdd, MakePrefixRule(@StaticParseUnaryPlus, precUnary));
  Context.SetParseRule(ttOpSub, MakePrefixRule(@StaticParseUnaryMinus, precUnary));
  Context.SetParseRule(ttBitwiseNOT, MakePrefixRule(@StaticParseLogicalNot, precUnary));

  // === INFIX RULES ===

  // Per ttOpAdd: aggiungi INFIX preservando PREFIX
  Rule := Context.GetParseRule(ttOpAdd);
  Rule.Infix := @StaticParseBinaryOperator;
  Rule.Precedence := precTerm;
  Context.SetParseRule(ttOpAdd, Rule);

  // Per ttOpSub: aggiungi INFIX preservando PREFIX
  Rule := Context.GetParseRule(ttOpSub);
  Rule.Infix := @StaticParseBinaryOperator;
  Rule.Precedence := precTerm;
  Context.SetParseRule(ttOpSub, Rule);

  // Altri operatori aritmetici (solo INFIX)
  Context.SetParseRule(ttOpMul, MakeInfixRule(@StaticParseBinaryOperator, precFactor));
  // FreeBASIC pointer dereference: '*' is ALSO a unary prefix (*ptr). Add the prefix function while
  // preserving the infix multiply rule above (the Pratt loop uses prefix at operand position only).
  Rule := Context.GetParseRule(ttOpMul);
  Rule.Prefix := @StaticParseDeref;
  Context.SetParseRule(ttOpMul, Rule);
  Context.SetParseRule(ttOpDiv, MakeInfixRule(@StaticParseBinaryOperator, precFactor));
  Context.SetParseRule(ttOpMod, MakeInfixRule(@StaticParseBinaryOperator, precFactor));
  Context.SetParseRule(ttOpIntDiv, MakeInfixRule(@StaticParseBinaryOperator, precFactor));  // \ integer division
  // Bit shifts (FreeBASIC): tighter than +/-, looser than * / \ MOD. Per the FB precedence
  // table, `a Shl n + 1` parses as `(a Shl n) + 1`, so SHL/SHR bind tighter than binary +/-.
  Context.SetParseRule(ttOpShl, MakeInfixRule(@StaticParseBinaryOperator, precShift));
  Context.SetParseRule(ttOpShr, MakeInfixRule(@StaticParseBinaryOperator, precShift));
  // String concatenation (FreeBASIC): looser than +/-, tighter than comparisons.
  Context.SetParseRule(ttOpConcat, MakeInfixRule(@StaticParseBinaryOperator, precConcat));
  Context.SetParseRule(ttOpPow, MakeInfixRule(@StaticParsePower, precPower));

  // Comparison operators
  Context.SetParseRule(ttOpEq, MakeInfixRule(@StaticParseBinaryOperator, precEquality));
  Context.SetParseRule(ttOpNeq, MakeInfixRule(@StaticParseBinaryOperator, precEquality));
  Context.SetParseRule(ttOpLt, MakeInfixRule(@StaticParseBinaryOperator, precComparison));
  Context.SetParseRule(ttOpGt, MakeInfixRule(@StaticParseBinaryOperator, precComparison));
  Context.SetParseRule(ttOpLe, MakeInfixRule(@StaticParseBinaryOperator, precComparison));
  Context.SetParseRule(ttOpGe, MakeInfixRule(@StaticParseBinaryOperator, precComparison));

  // Bitwise operators
  Context.SetParseRule(ttBitwiseAND, MakeInfixRule(@StaticParseBinaryOperator, precAnd));
  Context.SetParseRule(ttBitwiseOR, MakeInfixRule(@StaticParseBinaryOperator, precOr));
  Context.SetParseRule(ttBitwiseXOR, MakeInfixRule(@StaticParseBinaryOperator, precOr));
  // FreeBASIC EQV / IMP: looser than OR/XOR; IMP is the loosest binary operator.
  Context.SetParseRule(ttOpEqv, MakeInfixRule(@StaticParseBinaryOperator, precEqv));
  Context.SetParseRule(ttOpImp, MakeInfixRule(@StaticParseBinaryOperator, precImp));
  // FreeBASIC ANDALSO / ORELSE short-circuit operators: the loosest of all (ORELSE looser than
  // ANDALSO). They parse as ordinary binary-op nodes; the SSA lowers them to short-circuit IFs.
  Context.SetParseRule(ttOpAndAlso, MakeInfixRule(@StaticParseBinaryOperator, precAndAlso));
  Context.SetParseRule(ttOpOrElse, MakeInfixRule(@StaticParseBinaryOperator, precOrElse));
  // FreeBASIC IS (runtime type check): relational-level; RHS is a type name (an identifier).
  Context.SetParseRule(ttOpIs, MakeInfixRule(@StaticParseBinaryOperator, precEquality));

  // Assignment
  Context.SetParseRule(ttDataAssignment, MakeInfixRule(@StaticParseAssignment, precAssignment));

  // *** FIX PRINCIPALE: Array access con parentesi TONDE ***
  Rule := Context.GetParseRule(ttDelimParOpen);
  Rule.Infix := @StaticParseArrayAccess;  // Aggiungi INFIX per array access
  Rule.Precedence := precCall;            // Stessa precedenza di function call
  Context.SetParseRule(ttDelimParOpen, Rule);

  // Array access con parentesi quadre (se supportato)
  Context.SetParseRule(ttDelimBrackOpen, MakeInfixRule(@StaticParseArrayAccess, precCall));

  // Member access (record.field) — high precedence postfix (infix), like call/index. The
  // prefix slot handles a leading '.field' inside a WITH block.
  Rule := MakeInfixRule(@StaticParseMemberAccess, precCall);
  Rule.Prefix := @StaticParseWithMember;
  Context.SetParseRule(ttOpDot, Rule);

  {$IFDEF DEBUG}
  LogDebug('Pratt expression rules initialized successfully');
  {$ENDIF}
end;

// === PRATT PARSING CORE ===

function TExpressionParser.GetCurrentPrecedence: TPrecedence;
var
  Token: TLexerToken;
  Rule: TParseRule;
begin
  if not HasValidContext then
  begin
    Result := precNone;
    Exit;
  end;

  Token := Context.CurrentToken;
  if not Assigned(Token) or Context.IsAtEnd then
    Result := precNone
  else
  begin
    Rule := Context.GetParseRule(Token.TokenType);
    Result := Rule.Precedence;
  end;
end;

{$IFDEF DEBUG}
procedure TExpressionParser.LogDebug(const Msg: string);
begin
  if HasValidContext and Context.DebugMode then
    WriteLn('DEBUG [ExpressionParser]: ', Msg);
end;
{$ENDIF}


{$IFDEF DEBUG}
procedure TExpressionParser.LogVerbose(const Msg: string);
begin
  if HasValidContext and Context.DebugMode then
    WriteLn('VERBOSE [ExpressionParser]: ', Msg);
end;
{$ENDIF}


// === MAIN EXPRESSION PARSING ===

function TExpressionParser.ParseExpression(Precedence: TPrecedence): TASTNode;
var
  Token: TLexerToken;
  Rule: TParseRule;
  Left: TASTNode;
  InfixRule: TParseRule;
begin
  Result := nil;

  if not HasValidContext then
  begin
    {$IFDEF DEBUG}
    LogDebug('ParseExpression called without valid context');
    {$ENDIF}
    Exit;
  end;

  Token := Context.Advance;
  if not Assigned(Token) then
  begin
    //WriteLn('>>> ERROR ParseExpression: Token is NIL');
    HandleError('Unexpected end of input', nil);
    Exit;
  end;

  //WriteLn('>>> DEBUG ParseExpression: token="', Token.Value, '" type=', Ord(Token.TokenType));

  {$IFDEF DEBUG}
  LogVerbose(Format('ParseExpression: token=%s, precedence=%s',
    [Token.Value, PrecedenceToString(Precedence)]));
  {$ENDIF}

  // Get prefix rule
  Rule := Context.GetParseRule(Token.TokenType);
  //WriteLn('>>> DEBUG ParseExpression: Rule.Prefix assigned = ', Assigned(Rule.Prefix));
  if not Assigned(Rule.Prefix) then
  begin
    //WriteLn('>>> ERROR ParseExpression: No prefix rule for token "', Token.Value, '"');
    HandleError(Format('Unexpected token "%s"', [Token.Value]), Token);
    Exit;
  end;

  // Parse prefix
  //WriteLn('>>> DEBUG ParseExpression: Calling prefix rule...');
  Left := TASTNode(Rule.Prefix(Self, Token));
  //WriteLn('>>> DEBUG ParseExpression: Prefix returned: ', Assigned(Left));

  if not Assigned(Left) then
  begin
    //WriteLn('>>> ERROR ParseExpression: Prefix parsing failed');
    Exit;
  end;

  // Parse infix while precedence allows
  while (Precedence <= GetCurrentPrecedence) and not Context.IsAtEnd do
  begin
    Token := Context.CurrentToken; // ← NON advance ancora!
    InfixRule := Context.GetParseRule(Token.TokenType);

    if not Assigned(InfixRule.Infix) then
      Break; // ← Esci senza consumare il token

    // Solo ora consuma il token
    Context.Advance;

    // Parse infix
    Left := TASTNode(InfixRule.Infix(Self, TObject(Left), Token));
    if not Assigned(Left) then
      Exit;
  end;

  Result := Left;
  //WriteLn('>>> DEBUG ParseExpression: SUCCESS, returning node');
end;

function TExpressionParser.ParsePrimaryExpression: TASTNode;
begin
  Result := ParseExpression(precPrimary);
end;

function TExpressionParser.ParseExpressionList(Delimiter: TTokenType = ttSeparParam): TASTNode;
var
  Expr: TASTNode;
  WasAny: Boolean;
begin
  //WriteLn('>>> DEBUG ExpressionParser.ParseExpressionList: ENTRY');
  //WriteLn('>>> DEBUG ExpressionParser.ParseExpressionList: Current token = "', Context.CurrentToken.Value, '"');

  Result := TASTNode.Create(antExpressionList);

  if not HasValidContext then
  begin
    //WriteLn('>>> ERROR: No valid context!');
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse first expression if present
  if not Context.IsAtEnd and not Context.CheckAny([ttEndOfLine, ttDelimParClose, ttDelimBrackClose]) then
  begin
    repeat
      //WriteLn('>>> DEBUG ExpressionParser.ParseExpressionList: About to parse expression, token = "', Context.CurrentToken.Value, '"');

      // *** MAIN FIX: Check if we are on a delimiter ***
      if Context.Check(Delimiter) then
      begin
        //WriteLn('>>> DEBUG ExpressionParser.ParseExpressionList: Found delimiter without expression, skipping');
        // Add empty expression for consecutive delimiters
        Result.AddChild(TASTNode.Create(antLiteral));
        Context.Advance; // Consume the delimiter

        // If after the delimiter we're at the end, exit
        if Context.CheckAny([ttEndOfLine, ttDelimParClose, ttDelimBrackClose, ttEndOfFile]) then
          Break;

        Continue; // Continue looking for the next expression
      end;

      // FreeBASIC "Any" modifier (char-set form of TRIM/LTRIM/RTRIM and INSTRREV): "Any <expr>"
      // marks this argument so the lowering trims/searches any character in the set rather than the
      // whole substring. Only treated as a modifier when "Any" is immediately followed by another
      // expression (otherwise it is a plain identifier argument named ANY). The attribute is inert for
      // calls that don't consult it.
      // FreeBASIC lets a file number carry an optional '#' sigil wherever one is expected as an
      // argument: WINPUT(n, #f), EOF(#f), LOF(#f), SEEK(#f). The sigil adds nothing -- the argument is
      // the same integer either way -- so consume it and parse the number that follows. A '#' can only
      // begin an argument in this position: a trailing type suffix ("a#") is glued onto the identifier
      // by the lexer, and preprocessor directives never reach the expression parser.
      if Context.Check(ttFileHandlePrefix) then
        Context.Advance;

      WasAny := False;
      if Context.Check(ttIdentifier) and (UpperCase(Context.CurrentToken.Value) = 'ANY')
         and Assigned(Context.PeekNext)
         and not (Context.PeekNext.TokenType in
                  [ttDelimParClose, ttDelimBrackClose, ttSeparParam, ttEndOfLine, ttEndOfFile]) then
      begin
        Context.Advance;   // consume "Any"
        WasAny := True;
      end;

      Expr := ParseExpression(precNone);
      //WriteLn('>>> DEBUG ExpressionParser.ParseExpressionList: ParseExpression returned: ', Assigned(Expr));

      if Assigned(Expr) then
      begin
        if WasAny then
          Expr.Attributes.Values['ANYSET'] := '1';
        Result.AddChild(Expr)
      end
      else
        Break;

      // *** FIX: Check delimiter AFTER parsing the expression ***
      if Context.Check(Delimiter) then
      begin
        Context.Advance; // Consume the delimiter
        //WriteLn('>>> DEBUG ExpressionParser.ParseExpressionList: Consumed delimiter, continuing...');

        // If after the delimiter we're at the end, exit
        if Context.CheckAny([ttEndOfLine, ttDelimParClose, ttDelimBrackClose, ttEndOfFile]) then
          Break;
      end
      else
      begin
        //WriteLn('>>> DEBUG ExpressionParser.ParseExpressionList: No delimiter, ending list');
        Break; // No delimiter, end the list
      end;

    until False; // Infinite loop controlled by break
  end;

  //WriteLn('>>> DEBUG ExpressionParser.ParseExpressionList: DONE, token now = "', Context.CurrentToken.Value, '"');
  DoNodeCreated(Result);
end;

// === PREFIX PARSING FUNCTIONS ===

function TExpressionParser.ParseNumber(Token: TLexerToken): TASTNode;
var
  Value: Variant;
  Int64Value: Int64;
  QWordValue: QWord;
  ValCode: Integer;
  FloatValue: Double;
  FormatSettings: TFormatSettings;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseNumber: %s', [Token.Value]));
  {$ENDIF}

  // Setup format settings for parsing
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';

  // Try integer first (full 64-bit range, so e.g. 5000000000 stays an integer rather than
  // overflowing into a float). A literal written with a '.'/exponent fails this and stays a
  // float Variant - the SSA literal handler keys the int/float bank off that Variant type, so
  // 1.0 remains a float (and 1.0/3.0 is float division), not collapsed to integer 1.
  if TryStrToInt64(Token.Value, Int64Value) then
    Value := Int64Value
  else
  begin
    // A decimal integer literal in (Int64.Max, QWord.Max] (e.g. an ULongInt constant like
    // 18446744073709551615) overflows Int64 but fits an unsigned 64-bit value. Store its exact
    // two's-complement bits reinterpreted as Int64 (Variant stays varInt64, so the whole
    // int-bank pipeline is unaffected) instead of degrading to a lossy Double. IsUnsigned64Expr
    // in the SSA later keys the unsigned compare/div/mod/print forms off the literal's text.
    // Val rejects any '.'/exponent or an out-of-range magnitude via a non-zero error code, so a
    // genuine float or a value above QWord.Max still falls through to the float path below.
    Val(Token.Value, QWordValue, ValCode);
    if ValCode = 0 then
      Value := Int64(QWordValue)
    else if TryStrToFloat(Token.Value, FloatValue, FormatSettings) then
      Value := FloatValue
    else
    begin
      HandleError(Format('Invalid number format: "%s"', [Token.Value]), Token);
      Result := nil;
      Exit;
    end;
  end;

  Result := CreateLiteralNode(Value, Token);
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseString(Token: TLexerToken): TASTNode;
var
  Value: string;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseString: %s', [Token.Value]));
  {$ENDIF}

  // The token value is already the final string content (the lexer's ProcessString strips the
  // surrounding quotes and collapses doubled "" to a single "). Do NOT strip quotes again here: a
  // legitimate content that itself begins and ends with a quote (e.g. """x""" -> "x") would be corrupted.
  Value := Token.Value;

  Result := CreateLiteralNode(Value, Token);
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseIdentifier(Token: TLexerToken): TASTNode;
var
  IdentName: string;
  FnName: string;
  Args: TASTNode;
  NameNode: TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseIdentifier: %s', [Token.Value]));
  {$ENDIF}

  if not HasValidContext then
  begin
    Result := nil;
    Exit;
  end;

  IdentName := UpperCase(Token.Value);

  // FreeBASIC CAST(type, expr) / CPTR(type, expr): the first argument is a TYPE (which the expression
  // parser cannot parse as an expression), so handle it specially. Lowers to antCast(value = type
  // string, child0 = value expr). In the raw-pointer model a pointer cast is a value passthrough (the
  // byte offset is unchanged); a scalar type converts the value.
  if ((IdentName = 'CAST') or (IdentName = 'CPTR')) and Context.Check(ttDelimParOpen) then
  begin
    Result := ParseCast(Token);
    Exit;
  end;

  // SIZEOF(<type> PTR): the argument is a POINTER TYPE, not an expression -- "Integer Ptr" parses as two
  // juxtaposed identifiers and died on "Expected ')'". Fold it into the single "INTEGER PTR" identifier the
  // rest of the pipeline already uses for pointer types (a DIM records exactly that spelling), so SIZEOF
  // resolves it to the pointer size. Only when a PTR actually follows: SIZEOF(x) over a variable, and
  // SIZEOF(T) over a plain type, keep their existing paths.
  if (IdentName = 'SIZEOF') and Context.Check(ttDelimParOpen) and
     Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttIdentifier) then
  begin
    Result := ParseSizeOfPtrType(Token);
    if Assigned(Result) then Exit;
  end;

  // Check if this is a user-defined function call (FNxxx followed by parenthesis)
  // This handles the case where FNSQ(5) is tokenized as a single identifier
  if (Length(IdentName) > 2) and (Copy(IdentName, 1, 2) = 'FN') and
     Context.Check(ttDelimParOpen) then
  begin
    // Extract function name (part after FN)
    FnName := Copy(IdentName, 3, Length(IdentName) - 2);

    // Create user function node
    Result := TASTNode.CreateWithValue(antUserFunction, IdentName, Token);

    // Add function name as first child
    NameNode := TASTNode.CreateWithValue(antIdentifier, FnName, Token);
    Result.AddChild(NameNode);

    // Consume opening parenthesis
    Context.Advance;

    // Parse argument (single expression for DEF FN functions)
    if not Context.Check(ttDelimParClose) then
    begin
      Args := ParseExpression;
      if Assigned(Args) then
        Result.AddChild(Args);
    end;

    // Expect closing parenthesis
    if not Context.Match(ttDelimParClose) then
    begin
      HandleError('Expected ")" after function argument', Context.CurrentToken);
      Result.Free;
      Result := nil;
      Exit;
    end;

    DoNodeCreated(Result);
    Exit;
  end;

  // Regular identifier - let infix parsing handle ( if present
  Result := CreateIdentifierNode(IdentName, Token);
  DoNodeCreated(Result);
end;


function TExpressionParser.ParseLineNumber(Token: TLexerToken): TASTNode;
var
  LineNum: Integer;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseLineNumber: %s', [Token.Value]));
  {$ENDIF}

  if not TryStrToInt(Token.Value, LineNum) then
  begin
    HandleError(Format('Invalid line number: "%s"', [Token.Value]), Token);
    Result := nil;
    Exit;
  end;

  Result := TASTNode.CreateWithValue(antLineNumber, LineNum, Token);
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseUnaryPlus(Token: TLexerToken): TASTNode;
var
  Operand: TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose('ParseUnaryPlus');
  {$ENDIF}

  Operand := ParseExpression(precUnary);
  if not Assigned(Operand) then
  begin
    Result := nil;
    Exit;
  end;

  Result := CreateUnaryOpNode(ttOpAdd, Operand, Token);
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseUnaryMinus(Token: TLexerToken): TASTNode;
var
  Operand: TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose('ParseUnaryMinus');
  {$ENDIF}

  Operand := ParseExpression(precUnary);
  if not Assigned(Operand) then
  begin
    Result := nil;
    Exit;
  end;

  Result := CreateUnaryOpNode(ttOpSub, Operand, Token);
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseLogicalNot(Token: TLexerToken): TASTNode;
var
  Operand: TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose('ParseLogicalNot');
  {$ENDIF}

  Operand := ParseExpression(precUnary);
  if not Assigned(Operand) then
  begin
    Result := nil;
    Exit;
  end;

  Result := CreateUnaryOpNode(ttBitwiseNOT, Operand, Token);
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseParentheses(Token: TLexerToken): TASTNode;
var
  Expr: TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose('ParseParentheses');
  {$ENDIF}

  if not HasValidContext then
  begin
    Result := nil;
    Exit;
  end;

  Expr := ParseExpression(precNone);
  if not Assigned(Expr) then
  begin
    Result := nil;
    Exit;
  end;

  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" after expression', Context.CurrentToken);
    Expr.Free;
    Result := nil;
    Exit;
  end;

  Result := TASTNode.Create(antParentheses, Token);
  Result.AddChild(Expr);
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseFunctionCall(Token: TLexerToken): TASTNode;
var
  Args: TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseFunctionCall: %s', [Token.Value]));
  {$ENDIF}

  if not HasValidContext then
  begin
    Result := nil;
    Exit;
  end;

  Result := CreateFunctionCallNode(Token.Value, nil, Token);

  // Consume opening parenthesis
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected "(" after function name', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse arguments if any
  if not Context.Check(ttDelimParClose) then
  begin
    Args := ParseArgumentList;
    if Assigned(Args) then
      Result.AddChild(Args);
  end;

  // Consume closing parenthesis
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" after function arguments', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  DoNodeCreated(Result);
end;

function TExpressionParser.ParseMathFunction(Token: TLexerToken): TASTNode;
var
  Args: TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseMathFunction: %s', [Token.Value]));
  {$ENDIF}

  if not HasValidContext then
  begin
    Result := nil;
    Exit;
  end;

  Result := TASTNode.CreateWithValue(antFunctionCall, Token.Value, Token);

  // FreeBASIC FREEFILE/NOW/TIMER take no argument: accept bare (FREEFILE), as well as FREEFILE(). Attach
  // an empty argument list so the SSA function dispatch (which only runs for ChildCount>0) still sees it.
  if ((UpperCase(Token.Value) = 'FREEFILE') or (UpperCase(Token.Value) = kNOW) or
      (UpperCase(Token.Value) = kTIMER)) and not Context.Check(ttDelimParOpen) then
  begin
    Result.AddChild(TASTNode.Create(antArgumentList, Token));
    Exit;
  end;

  // FreeBASIC bare RND (no parentheses) is equivalent to RND(1): synthesize the argument so it
  // reuses the existing RND(arg) lowering with no SSA changes.
  if (UpperCase(Token.Value) = kRND) and not Context.Check(ttDelimParOpen) then
  begin
    Args := TASTNode.Create(antArgumentList, Token);
    Args.AddChild(TASTNode.CreateWithValue(antLiteral, 1, Token));
    Result.AddChild(Args);
    Exit;
  end;

  // Consume opening parenthesis
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected "(" after math function', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse arguments if any
  if not Context.Check(ttDelimParClose) then
  begin
    Args := ParseArgumentList;
    if Assigned(Args) then
      Result.AddChild(Args);
  end;

  // Consume closing parenthesis
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" after function arguments', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  DoNodeCreated(Result);
end;

function TExpressionParser.ParseStringFunction(Token: TLexerToken): TASTNode;
var
  Args: TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseStringFunction: %s', [Token.Value]));
  {$ENDIF}

  if not HasValidContext then
  begin
    Result := nil;
    Exit;
  end;

  Result := TASTNode.CreateWithValue(antFunctionCall, Token.Value, Token);

  // FreeBASIC DATE/TIME take no argument: accept bare (DATE), as well as DATE(). Attach an empty
  // argument list so the SSA function dispatch (which only runs for ChildCount>0) still sees it.
  if ((UpperCase(Token.Value) = kDATEFN) or (UpperCase(Token.Value) = kTIMEFN)) and
     not Context.Check(ttDelimParOpen) then
  begin
    Result.AddChild(TASTNode.Create(antArgumentList, Token));
    Exit;
  end;

  // Consume opening parenthesis
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected "(" after string function', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse arguments if any
  if not Context.Check(ttDelimParClose) then
  begin
    Args := ParseArgumentList;
    if Assigned(Args) then
      Result.AddChild(Args);
  end;

  // Consume closing parenthesis
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" after function arguments', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  DoNodeCreated(Result);
end;

function TExpressionParser.ParseMemoryFunction(Token: TLexerToken): TASTNode;
var
  Args: TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseMemoryFunction: %s', [Token.Value]));
  {$ENDIF}

  if not HasValidContext then
  begin
    Result := nil;
    Exit;
  end;

  Result := TASTNode.CreateWithValue(antFunctionCall, Token.Value, Token);

  // Consume opening parenthesis
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected "(" after memory function', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse arguments if any
  if not Context.Check(ttDelimParClose) then
  begin
    Args := ParseArgumentList;
    if Assigned(Args) then
      Result.AddChild(Args);
  end;

  // Consume closing parenthesis
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" after function arguments', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  DoNodeCreated(Result);
end;

function TExpressionParser.ParseGraphicsFunction(Token: TLexerToken): TASTNode;
var
  Args: TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseGraphicsFunction: %s', [Token.Value]));
  {$ENDIF}

  if not HasValidContext then
  begin
    Result := nil;
    Exit;
  end;

  Result := TASTNode.CreateWithValue(antGraphicsFunction, Token.Value, Token);

  // Consume opening parenthesis
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected "(" after graphics function', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse arguments if any
  if not Context.Check(ttDelimParClose) then
  begin
    Args := ParseArgumentList;
    if Assigned(Args) then
      Result.AddChild(Args);
  end;

  // Consume closing parenthesis
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" after function arguments', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  DoNodeCreated(Result);
end;

function TExpressionParser.ParseGraphicsCommandForm(Token: TLexerToken): TASTNode;
// A graphics command found where an expression was expected. Exactly one of them has a function form:
// FreeBASIC's SCREEN(row, column [, colorflag]), which reads a character or colour attribute back out
// of the console. MODERN only -- Commodore BASIC v7 has no SCREEN at all, so in CLASSIC this stays the
// syntax error it has always been, and the dialects keep their own semantics.
begin
  if (UpperCase(Token.Value) <> kSCREENGFX) or not ModernMode or not Context.Check(ttDelimParOpen) then
  begin
    HandleError(Format('Unexpected token "%s"', [Token.Value]), Token);
    Result := nil;
    Exit;
  end;
  Result := ParseGraphicsFunction(Token);   // antGraphicsFunction "SCREEN" + argument list
end;

function TExpressionParser.ParseSpriteFunction(Token: TLexerToken): TASTNode;
var
  Args: TASTNode;
  FuncName: string;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseSpriteFunction: %s', [Token.Value]));
  {$ENDIF}

  if not HasValidContext then
  begin
    Result := nil;
    Exit;
  end;

  FuncName := UpperCase(Token.Value);
  Result := TASTNode.CreateWithValue(antSpriteFunction, FuncName, Token);

  // Set specific function type attribute for SSA generator
  // BUMP(n) - returns collision bitmask for type n (1=sprite-sprite, 2=sprite-display)
  // RSPCOLOR(n) - returns multicolor n (1=MC1, 2=MC2)
  // RSPPOS(sprite, attr) - returns position/speed (0=X, 1=Y, 2=speed)
  // RSPRITE(sprite, attr) - returns sprite attribute (0=enabled, 1=color, etc.)
  if FuncName = kBUMP then
    Result.Attributes.Values['sprite_func'] := 'BUMP'
  else if FuncName = kRSPCOLOR then
    Result.Attributes.Values['sprite_func'] := 'RSPCOLOR'
  else if FuncName = kRSPPOS then
    Result.Attributes.Values['sprite_func'] := 'RSPPOS'
  else if FuncName = kRSPRITE then
    Result.Attributes.Values['sprite_func'] := 'RSPRITE'
  else
    Result.Attributes.Values['sprite_func'] := FuncName;

  // Consume opening parenthesis
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected "(" after sprite function', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse arguments if any
  if not Context.Check(ttDelimParClose) then
  begin
    Args := ParseArgumentList;
    if Assigned(Args) then
      Result.AddChild(Args);
  end;

  // Consume closing parenthesis
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" after function arguments', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  DoNodeCreated(Result);
end;

function TExpressionParser.ParseInputFunctionForm(Token: TLexerToken): TASTNode;
// FreeBASIC INPUT(n [, [#]filenum]). MODERN only: in CLASSIC, INPUT is a statement and nothing else, so
// an INPUT in expression position is a syntax error there -- exactly as Commodore BASIC v7 reports it.
begin
  if not ModernMode then
  begin
    HandleError('INPUT is a statement in Commodore BASIC v7; INPUT(n) as a function is FreeBASIC only', Token);
    Result := nil;
    Exit;
  end;
  Result := ParseStringFunction(Token);   // antFunctionCall "INPUT" + argument list
end;

function TExpressionParser.ParseInputFunction(Token: TLexerToken): TASTNode;
var
  Args: TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseInputFunction: %s', [Token.Value]));
  {$ENDIF}

  if not HasValidContext then
  begin
    Result := nil;
    Exit;
  end;

  Result := TASTNode.CreateWithValue(antInputFunction, Token.Value, Token);

  // Consume opening parenthesis
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected "(" after input function', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse arguments if any
  if not Context.Check(ttDelimParClose) then
  begin
    Args := ParseArgumentList;
    if Assigned(Args) then
      Result.AddChild(Args);
  end;

  // Consume closing parenthesis
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" after function arguments', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  DoNodeCreated(Result);
end;

function TExpressionParser.ParseUsrFunction(Token: TLexerToken): TASTNode;
var
  Args: TASTNode;
begin
  Result := TASTNode.CreateWithValue(antUsrFunction, Token.Value, Token);

  // Consume opening parenthesis
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected "(" after USR', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse address argument
  if not Context.Check(ttDelimParClose) then
  begin
    Args := ParseArgumentList;
    if Assigned(Args) then
      Result.AddChild(Args);
  end;

  // Consume closing parenthesis
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" after USR arguments', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  DoNodeCreated(Result);
end;

function TExpressionParser.ParseUserFunction(Token: TLexerToken): TASTNode;
var
  FnName: string;
  Args: TASTNode;
  NameNode: TASTNode;
begin
  // FN is already consumed, now we expect the function name (identifier)
  // Format: FNname(arg)  or  FN name(arg)  depending on tokenizer

  if not Context.Check(ttIdentifier) then
  begin
    HandleError('Expected function name after FN', Context.CurrentToken);
    Result := nil;
    Exit;
  end;

  FnName := Context.CurrentToken.Value;
  Result := TASTNode.CreateWithValue(antUserFunction, 'FN' + FnName, Token);

  // Add function name as first child
  NameNode := TASTNode.CreateWithValue(antIdentifier, FnName, Context.CurrentToken);
  Result.AddChild(NameNode);
  Context.Advance; // Consume function name

  // Expect opening parenthesis
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected "(" after function name', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse argument (single expression for DEF FN functions)
  if not Context.Check(ttDelimParClose) then
  begin
    Args := ParseExpression;
    if Assigned(Args) then
      Result.AddChild(Args);
  end;

  // Expect closing parenthesis
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" after function argument', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  DoNodeCreated(Result);
end;

function TExpressionParser.ParseProcAddress(Token: TLexerToken): TASTNode;
var
  Operand: TASTNode;
begin
  // '@' is already consumed. Parse the operand as a full postfix expression (precCall stops before
  // binary operators), so all of these are handled: @sub / @x (identifier), @arr(i) (array access),
  // @obj.field and @arr(i).field and @a.b.c (member access). Lower to antProcAddress; SSA dispatches
  // on the operand's node kind:
  //   - antIdentifier  → Value = name, no child (data-var address or @sub entry PC)
  //   - antArrayAccess → child0 = the array access (element address)
  //   - antMemberAccess→ child0 = the member access (record-field pointer)
  // @Sin / @Cos / ...: the address of a math builtin taken as a function pointer. The name is a keyword
  // token (ttMathFunction), not an identifier, and has no "(args)" here, so create the antProcAddress
  // directly (the SSA encodes a BUILTIN_FP_TAG sentinel; an indirect call computes the op). The crt/math.bi
  // "_" aliases (sin_, cos_, ...) are ordinary identifiers and take the normal path below.
  if Context.Check(ttMathFunction) then
  begin
    Result := TASTNode.CreateWithValue(antProcAddress, UpperCase(VarToStr(Context.CurrentToken.Value)), Token);
    Context.Advance;
    DoNodeCreated(Result);
    Exit;
  end;
  if not Context.Check(ttIdentifier) then
  begin
    HandleError('Expected a name after "@"', Context.CurrentToken);
    Result := nil;
    Exit;
  end;
  Operand := ParseExpression(precCall);
  if not Assigned(Operand) then
  begin
    HandleError('Expected a name after "@"', Context.CurrentToken);
    Result := nil;
    Exit;
  end;
  if Operand.NodeType = antIdentifier then
  begin
    // @x / @sub: keep the historical shape (Value = name, no child) so threading and scalar/proc
    // address paths are unaffected.
    Result := TASTNode.CreateWithValue(antProcAddress, UpperCase(VarToStr(Operand.Value)), Token);
    Operand.Free;
  end
  else
  begin
    // @arr(i) / @obj.field: keep the operand subtree as child0.
    Result := TASTNode.Create(antProcAddress, Token);
    Result.AddChild(Operand);
  end;
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseNew(Token: TLexerToken): TASTNode;
// FreeBASIC "NEW T" / "NEW T(args)". The NEW token is already consumed (prefix rule). Read the type
// name, then an optional parenthesised constructor argument list. Lowers to antNew(value = type name,
// child0 = antArgumentList of ctor args when present). SSA allocates a heap record and runs its ctor.
var
  ArgList: TASTNode;
begin
  if not Context.Check(ttIdentifier) then
  begin
    HandleError('Expected a TYPE name after NEW', Context.CurrentToken);
    Result := nil;
    Exit;
  end;
  Result := TASTNode.CreateWithValue(antNew, UpperCase(Context.CurrentToken.Value), Token);
  Context.Advance;  // consume the type name
  if Context.Check(ttDelimParOpen) then
  begin
    Context.Advance;  // consume '('
    if Context.Check(ttDelimParClose) then
      ArgList := TASTNode.Create(antArgumentList, Token)   // NEW T() — empty arg list
    else
      ArgList := ParseArgumentList;                        // comma-separated ctor args → antArgumentList
    if not Context.Match(ttDelimParClose) then
    begin
      HandleError('Expected ")" after NEW constructor arguments', Context.CurrentToken);
      if Assigned(ArgList) then ArgList.Free;
      Result.Free; Result := nil; Exit;
    end;
    if Assigned(ArgList) then Result.AddChild(ArgList);
  end;
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseTypeConstructor(Token: TLexerToken): TASTNode;
// FreeBASIC "type<T>(args)": an anonymous temporary of UDT T initialised with args (via T's constructor,
// or field-by-field when T has none). The TYPE token is already consumed. Lowers to the same shape the
// SSA's UDT-temporary hook recognises for "T(args)": antArrayAccess(child0 = antIdentifier(T),
// child1 = antExpressionList(args)). The angle brackets are the comparison tokens ttOpLt / ttOpGt.
var
  TypeName: TLexerToken;
  NameNode, Args: TASTNode;
begin
  if not Context.Check(ttOpLt) then
  begin
    // FreeBASIC shorthand "Type(args)" without an explicit <T>: the UDT is inferred from the enclosing
    // context (a DIM/assignment target or a FUNCTION result type). Emit the same temporary shape with an
    // empty, INFERTYPE-marked type name; the DIM/return lowering fills in the concrete type name.
    if Context.Check(ttDelimParOpen) then
    begin
      NameNode := TASTNode.CreateWithValue(antIdentifier, '', Token);
      Context.Advance;                                  // '('
      if Context.Check(ttDelimParClose) then
        Args := TASTNode.Create(antExpressionList, Token)
      else
        Args := ParseExpressionList(ttSeparParam);
      if not Context.Match(ttDelimParClose) then
      begin
        HandleError('Expected ")" to close a Type(...) expression', Context.CurrentToken);
        NameNode.Free; if Assigned(Args) then Args.Free; Exit(nil);
      end;
      Result := TASTNode.Create(antArrayAccess, Token);
      Result.Attributes.Values['INFERTYPE'] := '1';     // type deduced by the DIM/assignment/return lowering
      Result.AddChild(NameNode);
      Result.AddChild(Args);
      DoNodeCreated(Result);
      Exit;
    end;
    HandleError('Expected "<" or "(" after TYPE in a type constructor expression', Context.CurrentToken);
    Exit(nil);
  end;
  Context.Advance;   // consume '<'
  if not Context.Check(ttIdentifier) then
  begin
    HandleError('Expected a TYPE name in a type<T>(...) expression', Context.CurrentToken);
    Exit(nil);
  end;
  TypeName := Context.CurrentToken;
  NameNode := TASTNode.CreateWithValue(antIdentifier, TypeName.Value, TypeName);
  Context.Advance;   // consume the type name
  if not Context.Match(ttOpGt) then
  begin
    HandleError('Expected ">" after the type name in a type<T>(...) expression', Context.CurrentToken);
    NameNode.Free; Exit(nil);
  end;
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected "(" after type<T> in a type<T>(...) expression', Context.CurrentToken);
    NameNode.Free; Exit(nil);
  end;
  if Context.Check(ttDelimParClose) then
    Args := TASTNode.Create(antExpressionList, Token)   // type<T>() — no field values
  else
    Args := ParseExpressionList(ttSeparParam);          // comma-separated field/ctor args
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" to close a type<T>(...) expression', Context.CurrentToken);
    NameNode.Free; if Assigned(Args) then Args.Free; Exit(nil);
  end;
  Result := TASTNode.Create(antArrayAccess, Token);
  Result.AddChild(NameNode);
  Result.AddChild(Args);
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseSizeOfPtrType(Token: TLexerToken): TASTNode;
// "SIZEOF(<type> PTR [PTR...])" -> antFunctionCall SIZEOF whose single argument is the type spelled as ONE
// identifier ("INTEGER PTR"), the same spelling a DIM produces. A speculative parse: it only commits when
// a PTR really follows the type name, and otherwise rewinds and returns nil so SIZEOF(var) / SIZEOF(T) --
// which are ordinary expressions -- take their existing path untouched.
var
  Saved: Integer;
  TypeStr: string;
  Args, NameNode: TASTNode;
begin
  Result := nil;
  Context.SavePosition(Saved);
  Context.Advance;                                   // consume '('
  if not Context.Check(ttIdentifier) then begin Context.RestorePosition(Saved); Exit; end;
  TypeStr := UpperCase(VarToStr(Context.CurrentToken.Value));
  Context.Advance;                                   // consume the type name
  if not (Context.Check(ttIdentifier) and (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'PTR')) then
  begin
    Context.RestorePosition(Saved);                  // not a pointer type -- leave it to the normal path
    Exit;
  end;
  while Context.Check(ttIdentifier) and (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'PTR') do
  begin
    TypeStr := TypeStr + ' PTR';                     // "T PTR PTR" folds too
    Context.Advance;
  end;
  if not Context.Match(ttDelimParClose) then
  begin
    Context.RestorePosition(Saved);
    Exit;
  end;
  // Same node shape SIZEOF(T) already produces: antArrayAccess(SIZEOF, antExpressionList(<type>)) -- the
  // SSA folds it there. Only the type identifier differs ("INTEGER PTR" instead of "INTEGER").
  Args := TASTNode.Create(antExpressionList, Token);
  Args.AddChild(TASTNode.CreateWithValue(antIdentifier, TypeStr, Token));
  NameNode := TASTNode.CreateWithValue(antIdentifier, 'SIZEOF', Token);
  Result := TASTNode.Create(antArrayAccess, Token);
  Result.AddChild(NameNode);
  Result.AddChild(Args);
end;

function TExpressionParser.ParseCast(Token: TLexerToken): TASTNode;
// CAST/CPTR(type, expr). The leading identifier is already consumed; the current token is '('. Read the
// type (identifiers joined with spaces, dots kept tight — e.g. "INTEGER PTR", "FORMS.POINT"), then the
// value expression. Lowers to antCast(value = upper-case type string, child0 = value expr).
var
  TypeStr: string;
  ValExpr: TASTNode;
begin
  Context.Advance;   // consume '('
  TypeStr := '';
  while (not Context.IsAtEnd) and (not Context.Check(ttSeparParam)) and (not Context.Check(ttDelimParClose)) do
  begin
    if Context.Check(ttOpDot) then TypeStr := TypeStr + '.'
    else
    begin
      if (TypeStr <> '') and (TypeStr[Length(TypeStr)] <> '.') then TypeStr := TypeStr + ' ';
      TypeStr := TypeStr + UpperCase(Context.CurrentToken.Value);
    end;
    Context.Advance;
  end;
  if not Context.Match(ttSeparParam) then
  begin
    HandleError('Expected "," after the type in CAST/CPTR', Context.CurrentToken);
    Exit(nil);
  end;
  ValExpr := ParseExpression(precNone);
  if not Assigned(ValExpr) then
  begin
    HandleError('Expected a value expression in CAST/CPTR', Context.CurrentToken);
    Exit(nil);
  end;
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" to close CAST/CPTR', Context.CurrentToken);
    ValExpr.Free; Exit(nil);
  end;
  Result := TASTNode.CreateWithValue(antCast, Trim(TypeStr), Token);
  Result.AddChild(ValExpr);
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseDeref(Token: TLexerToken): TASTNode;
// '*' as a prefix operator (FreeBASIC pointer dereference). '*' is already consumed; parse the
// pointer operand at unary precedence so "*p" binds the pointer and "*p + 1" is "(*p) + 1".
var
  Operand: TASTNode;
begin
  Operand := ParseExpression(precUnary);
  if not Assigned(Operand) then
  begin
    HandleError('Expected a pointer expression after "*"', Token);
    Result := nil;
    Exit;
  end;
  Result := TASTNode.Create(antDeref, Token);
  Result.AddChild(Operand);
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseThreadCreate(Token: TLexerToken): TASTNode;
var
  ProcExpr, ArgList, ParamExpr: TASTNode;
begin
  // THREADCREATE(@sub [, param]) — spawn a worker thread; evaluates to an int handle. FB passes a single
  // (Any Ptr) parameter. child0 = proc-address (@sub), child1 = antArgumentList (the param, or literal 0
  // if omitted). The arg list lets SSA stage the parameter through StageCallArgs like a normal call.
  Result := TASTNode.CreateWithValue(antThreadCreate, kTHREADCREATE, Token);
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected "(" after THREADCREATE', Context.CurrentToken);
    Result.Free; Result := nil; Exit;
  end;
  ProcExpr := ParseExpression;
  if not Assigned(ProcExpr) then
  begin
    HandleError('Expected @sub as the first THREADCREATE argument', Context.CurrentToken);
    Result.Free; Result := nil; Exit;
  end;
  Result.AddChild(ProcExpr);
  ArgList := TASTNode.Create(antArgumentList, Token);
  Result.AddChild(ArgList);
  if Context.Match(ttSeparParam) then
  begin
    ParamExpr := ParseExpression;
    if not Assigned(ParamExpr) then
    begin
      HandleError('Expected a parameter expression after "," in THREADCREATE', Context.CurrentToken);
      Result.Free; Result := nil; Exit;
    end;
    ArgList.AddChild(ParamExpr);
  end
  else
    // Param omitted: default to integer 0 (FB allows the optional parameter to be absent).
    ArgList.AddChild(TASTNode.CreateWithValue(antLiteral, '0', Token));
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" after THREADCREATE arguments', Context.CurrentToken);
    Result.Free; Result := nil; Exit;
  end;
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseMutexCreate(Token: TLexerToken): TASTNode;
begin
  // MUTEXCREATE [()] — create a mutex; evaluates to an int handle. No arguments.
  Result := TASTNode.CreateWithValue(antMutexCreate, kMUTEXCREATE, Token);
  if Context.Check(ttDelimParOpen) then
  begin
    Context.Advance;                              // optional (
    if Context.Check(ttDelimParClose) then Context.Advance;  // )
  end;
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseThreadSelf(Token: TLexerToken): TASTNode;
begin
  // THREADSELF [()] — the current thread's handle (0 on the main thread). No arguments.
  Result := TASTNode.CreateWithValue(antThreadSelf, kTHREADSELF, Token);
  if Context.Check(ttDelimParOpen) then
  begin
    Context.Advance;                              // optional (
    if Context.Check(ttDelimParClose) then Context.Advance;  // )
  end;
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseThreadCall(Token: TLexerToken): TASTNode;
var
  ProcNode, ArgList, ArgExpr: TASTNode;
begin
  // THREADCALL subname [(arg [, ...])] — sugar for THREADCREATE that passes the SUB's actual arguments
  // (int/float/string, any arity). Lowers to an antThreadCreate node: child0 = @subname, child1 =
  // antArgumentList, so SSA stages the args through StageCallArgs exactly like a normal call.
  if not Context.Check(ttIdentifier) then
  begin
    HandleError('Expected a SUB name after THREADCALL', Context.CurrentToken);
    Result := nil;
    Exit;
  end;
  ProcNode := TASTNode.CreateWithValue(antProcAddress, UpperCase(Context.CurrentToken.Value), Token);
  Context.Advance;                                // consume the sub name
  Result := TASTNode.CreateWithValue(antThreadCreate, kTHREADCREATE, Token);
  Result.AddChild(ProcNode);
  ArgList := TASTNode.Create(antArgumentList, Token);
  Result.AddChild(ArgList);
  if Context.Match(ttDelimParOpen) then
  begin
    if not Context.Check(ttDelimParClose) then
    begin
      repeat
        ArgExpr := ParseExpression;
        if not Assigned(ArgExpr) then Break;
        ArgList.AddChild(ArgExpr);
      until not Context.Match(ttSeparParam);
    end;
    if Context.Check(ttDelimParClose) then Context.Advance;  // )
  end;
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseCondCreate(Token: TLexerToken): TASTNode;
begin
  // CONDCREATE [()] — create a condition variable; evaluates to an int handle. No arguments.
  Result := TASTNode.CreateWithValue(antCondCreate, kCONDCREATE, Token);
  if Context.Check(ttDelimParOpen) then
  begin
    Context.Advance;                              // optional (
    if Context.Check(ttDelimParClose) then Context.Advance;  // )
  end;
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseSpecialVariable(Token: TLexerToken): TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseSpecialVariable: %s', [Token.Value]));
  {$ENDIF}

  // Special variables are simple value-returning expressions (no arguments)
  Result := TASTNode.CreateWithValue(antSpecialVariable, UpperCase(Token.Value), Token);
  DoNodeCreated(Result);
end;

// === INFIX PARSING FUNCTIONS ===

function TExpressionParser.ParseBinaryOperator(Left: TASTNode; Token: TLexerToken): TASTNode;
var
  Right: TASTNode;
  Rule: TParseRule;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseBinaryOperator: %s', [Token.Value]));
  {$ENDIF}

  if not HasValidContext then
  begin
    Result := nil;
    Exit;
  end;

  Rule := Context.GetParseRule(Token.TokenType);
  Right := ParseExpression(TPrecedence(Ord(Rule.Precedence) + 1)); // Left-associative

  if not Assigned(Right) then
  begin
    Result := nil;
    Exit;
  end;

  Result := CreateBinaryOpNode(Token.TokenType, Left, Right, Token);
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseAssignment(Left: TASTNode; Token: TLexerToken): TASTNode;
var
  Right: TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose('ParseAssignment');
  {$ENDIF}

  // Assignment is right-associative
  Right := ParseExpression(precAssignment);

  if not Assigned(Right) then
  begin
    Result := nil;
    Exit;
  end;

  Result := TASTNode.Create(antAssignment, Token);
  Result.AddChild(Left);
  Result.AddChild(Right);
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseArrayAccess(Left: TASTNode; Token: TLexerToken): TASTNode;
var
  Indices: TASTNode;
  FuncName: string;
begin
  {$IFDEF DEBUG}
  LogVerbose('ParseArrayAccess');
  {$ENDIF}

  if not HasValidContext then
  begin
    Result := nil;
    Exit;
  end;

  // *** CRITICO: Distingui tra array access e function call ***
  if Assigned(Left) and (Left.NodeType = antIdentifier) then
  begin
    FuncName := UpperCase(VarToStr(Left.Value));

    // *** DEBUG TEMPORANEO ***
    {$IFDEF DEBUG}
    WriteLn('DEBUG: ParseArrayAccess called for: ', FuncName);
    {$ENDIF}

    // If you have access to the executor context, check if it's an array
    // NOTE: This requires a modification to pass the executor context
    // For now, assume all IDENTIFIER( is a potential array access
  end;

  // Parse indices/arguments
  if not Context.Check(ttDelimParClose) then
  begin
    Indices := ParseExpressionList(ttSeparParam);
    if not Assigned(Indices) then
    begin
      Result := nil;
      Exit;
    end;
  end
  else
  begin
    // Empty parentheses - create empty expression list
    Indices := TASTNode.Create(antExpressionList);
  end;

  // Expect closing parenthesis (tonde o quadre)
  if Token.TokenType = ttDelimParOpen then
  begin
    if not Context.Match(ttDelimParClose) then
    begin
      HandleError('Expected ")" after array indices', Context.CurrentToken);
      Indices.Free;
      Result := nil;
      Exit;
    end;
  end
  else if Token.TokenType = ttDelimBrackOpen then
  begin
    if not Context.Match(ttDelimBrackClose) then
    begin
      HandleError('Expected "]" after array indices', Context.CurrentToken);
      Indices.Free;
      Result := nil;
      Exit;
    end;
  end;

  // *** DECISION: ALWAYS create antArrayAccess - the executor will decide ***
  Result := TASTNode.Create(antArrayAccess, Token);
  Result.AddChild(Left);
  Result.AddChild(Indices);
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseMemberAccess(Left: TASTNode; Token: TLexerToken): TASTNode;
// record.field — the '.' operator has already been consumed by the Pratt loop. Read the
// field name and build antMemberAccess(value=fieldName, child0=object expr). Chaining
// (a.b.c) works because Left may itself be an antMemberAccess.
var
  FieldName: string;
begin
  if not HasValidContext then Exit(nil);
  // A field name follows '.' unambiguously, so accept any alphabetic token here — including
  // reserved words (LEN, TYPE, NAME, ...) that would otherwise lex as keywords.
  FieldName := UpperCase(VarToStr(Context.CurrentToken.Value));
  if (FieldName = '') or not (FieldName[1] in ['A'..'Z', '_']) then
  begin
    HandleError('Expected field name after "."', Context.CurrentToken);
    Result := Left;
    Exit;
  end;
  Context.Advance;   // consume field name
  Result := TASTNode.CreateWithValue(antMemberAccess, FieldName, Token);
  Result.AddChild(Left);
  DoNodeCreated(Result);
end;

function TExpressionParser.ParseWithMember(Token: TLexerToken): TASTNode;
// Leading '.field' inside a WITH block (M3.2): resolves to <with-object>.field. The '.' has
// already been consumed (prefix rule). Builds antMemberAccess(field, clone of the WITH object).
var
  FieldName: string;
begin
  if (WithObject = nil) or not HasValidContext then
  begin
    HandleError('"." with no object (outside a WITH block)', Token);
    Result := nil;
    Exit;
  end;
  FieldName := UpperCase(VarToStr(Context.CurrentToken.Value));
  if (FieldName = '') or not (FieldName[1] in ['A'..'Z', '_']) then
  begin
    HandleError('Expected field name after "."', Context.CurrentToken);
    Result := nil;
    Exit;
  end;
  Context.Advance;   // consume field name
  Result := TASTNode.CreateWithValue(antMemberAccess, FieldName, Token);
  Result.AddChild(WithObject.Clone);
  DoNodeCreated(Result);
end;

function TExpressionParser.ParsePower(Left: TASTNode; Token: TLexerToken): TASTNode;
var
  Right: TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose('ParsePower');
  {$ENDIF}

  // Power is right-associative, so we use the same precedence
  Right := ParseExpression(precPower);

  if not Assigned(Right) then
  begin
    Result := nil;
    Exit;
  end;

  Result := CreateBinaryOpNode(ttOpPow, Left, Right, Token);
  DoNodeCreated(Result);
end;

{$IFDEF WEB_MODE}
// === WEB PARSING FUNCTIONS ===

function TExpressionParser.ParseWebFunction(Token: TLexerToken): TASTNode;
var
  Args: TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseWebFunction: %s', [Token.Value]));
  {$ENDIF}

  if not HasValidContext then
  begin
    Result := nil;
    Exit;
  end;

  // Web functions: GET$, POST$, GETRAW$, POSTRAW$, HTML$, URL$, HEADER$
  Result := TASTNode.CreateWithValue(antWebFunction, UpperCase(Token.Value), Token);

  // Consume opening parenthesis
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected "(" after web function', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse arguments if any
  if not Context.Check(ttDelimParClose) then
  begin
    Args := ParseArgumentList;
    if Assigned(Args) then
      Result.AddChild(Args);
  end;

  // Consume closing parenthesis
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" after web function arguments', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  DoNodeCreated(Result);
end;

function TExpressionParser.ParseWebVariable(Token: TLexerToken): TASTNode;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseWebVariable: %s', [Token.Value]));
  {$ENDIF}

  // Web variables: METHOD$, PATH$, QUERY$ - no arguments, just value
  Result := TASTNode.CreateWithValue(antWebVariable, UpperCase(Token.Value), Token);
  DoNodeCreated(Result);
end;
{$ENDIF}

// === UTILITY PARSING ===

function TExpressionParser.ParseArgumentList: TASTNode;
begin
  Result := ParseExpressionList(ttSeparParam);
  if Assigned(Result) then
    Result.NodeType := antArgumentList;
end;

function TExpressionParser.ParseParameterList: TASTNode;
begin
  Result := ParseExpressionList(ttSeparParam);
  if Assigned(Result) then
    Result.NodeType := antParameterList;
end;

// === VALIDATION ===

function TExpressionParser.ValidateExpression(Node: TASTNode): Boolean;
begin
  Result := False;

  if not Assigned(Node) then
    Exit;

  // Basic validation - can be extended
  case Node.NodeType of
    antLiteral, antIdentifier, antLineNumber:
      Result := True;

    antBinaryOp:
      Result := (Node.ChildCount = 2) and
                ValidateExpression(Node.Child[0]) and
                ValidateExpression(Node.Child[1]);

    antUnaryOp:
      Result := (Node.ChildCount = 1) and
                ValidateExpression(Node.Child[0]);

    antFunctionCall,
    antMathFunction,
    antStringFunction,
    antMemoryFunction,
    antGraphicsFunction,
    antSpriteFunction,
    antInputFunction:
      Result := True; // More complex validation can be added

    antArrayAccess:
      Result := (Node.ChildCount = 2) and
                ValidateExpression(Node.Child[0]) and
                ValidateExpression(Node.Child[1]);

    antParentheses:
      Result := (Node.ChildCount = 1) and
                ValidateExpression(Node.Child[0]);

    else
      Result := True; // Allow other node types
  end;
end;

// === FACTORY FUNCTIONS ===

function CreateExpressionParser: TExpressionParser;
begin
  Result := TExpressionParser.Create;
end;

end.
