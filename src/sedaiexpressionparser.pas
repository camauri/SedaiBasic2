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
    function ParseSpriteFunction(Token: TLexerToken): TASTNode;
    function ParseInputFunction(Token: TLexerToken): TASTNode;
    function ParseUsrFunction(Token: TLexerToken): TASTNode;
    function ParseUserFunction(Token: TLexerToken): TASTNode;
    function ParseSpecialVariable(Token: TLexerToken): TASTNode;
    {$IFDEF WEB_MODE}
    function ParseWebFunction(Token: TLexerToken): TASTNode;
    function ParseWebVariable(Token: TLexerToken): TASTNode;
    {$ENDIF}

    // === INFIX PARSING FUNCTIONS ===
    function ParseBinaryOperator(Left: TASTNode; Token: TLexerToken): TASTNode;
    function ParseAssignment(Left: TASTNode; Token: TLexerToken): TASTNode;
    function ParseArrayAccess(Left: TASTNode; Token: TLexerToken): TASTNode;
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
function StaticParseSpriteFunction(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseInputFunction(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseUsrFunction(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseUserFunction(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseSpecialVariable(Parser: Pointer; Token: TLexerToken): TObject;
{$IFDEF WEB_MODE}
function StaticParseWebFunction(Parser: Pointer; Token: TLexerToken): TObject;
function StaticParseWebVariable(Parser: Pointer; Token: TLexerToken): TObject;
{$ENDIF}

function StaticParseBinaryOperator(Parser: Pointer; Left: TObject; Token: TLexerToken): TObject;
function StaticParseAssignment(Parser: Pointer; Left: TObject; Token: TLexerToken): TObject;
function StaticParseArrayAccess(Parser: Pointer; Left: TObject; Token: TLexerToken): TObject;
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

function StaticParseSpriteFunction(Parser: Pointer; Token: TLexerToken): TObject;
begin
  Result := TExpressionParser(Parser).ParseSpriteFunction(Token);
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
  Context.SetParseRule(ttLineNumber, MakePrefixRule(@StaticParseLineNumber, precPrimary));

  // *** FIX: Parentesi tonde per ENTRAMBI parentheses e array access ***
  Context.SetParseRule(ttDelimParOpen, MakePrefixRule(@StaticParseParentheses, precCall));

  // === FUNCTIONS AS PREFIX EXPRESSIONS ===
  Context.SetParseRule(ttMathFunction, MakePrefixRule(@StaticParseMathFunction, precCall));
  Context.SetParseRule(ttStringFunction, MakePrefixRule(@StaticParseStringFunction, precCall));
  Context.SetParseRule(ttMemoryFunction, MakePrefixRule(@StaticParseMemoryFunction, precCall));
  Context.SetParseRule(ttGraphicsFunction, MakePrefixRule(@StaticParseGraphicsFunction, precCall));
  Context.SetParseRule(ttSpriteFunction, MakePrefixRule(@StaticParseSpriteFunction, precCall));
  Context.SetParseRule(ttInputFunction, MakePrefixRule(@StaticParseInputFunction, precCall));
  Context.SetParseRule(ttUsrFunction, MakePrefixRule(@StaticParseUsrFunction, precCall));
  Context.SetParseRule(ttProcedureStart, MakePrefixRule(@StaticParseUserFunction, precCall));
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
  Context.SetParseRule(ttOpDiv, MakeInfixRule(@StaticParseBinaryOperator, precFactor));
  Context.SetParseRule(ttOpMod, MakeInfixRule(@StaticParseBinaryOperator, precFactor));
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

  // Assignment
  Context.SetParseRule(ttDataAssignment, MakeInfixRule(@StaticParseAssignment, precAssignment));

  // *** FIX PRINCIPALE: Array access con parentesi TONDE ***
  Rule := Context.GetParseRule(ttDelimParOpen);
  Rule.Infix := @StaticParseArrayAccess;  // Aggiungi INFIX per array access
  Rule.Precedence := precCall;            // Stessa precedenza di function call
  Context.SetParseRule(ttDelimParOpen, Rule);

  // Array access con parentesi quadre (se supportato)
  Context.SetParseRule(ttDelimBrackOpen, MakeInfixRule(@StaticParseArrayAccess, precCall));

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

      Expr := ParseExpression(precNone);
      //WriteLn('>>> DEBUG ExpressionParser.ParseExpressionList: ParseExpression returned: ', Assigned(Expr));

      if Assigned(Expr) then
        Result.AddChild(Expr)
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
  IntValue: Integer;
  FloatValue: Double;
  FormatSettings: TFormatSettings;
begin
  {$IFDEF DEBUG}
  LogVerbose(Format('ParseNumber: %s', [Token.Value]));
  {$ENDIF}

  // Setup format settings for parsing
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';

  // Try integer first
  if TryStrToInt(Token.Value, IntValue) then
    Value := IntValue
  else if TryStrToFloat(Token.Value, FloatValue, FormatSettings) then
    Value := FloatValue
  else
  begin
    HandleError(Format('Invalid number format: "%s"', [Token.Value]), Token);
    Result := nil;
    Exit;
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

  Value := Token.Value;

  // Remove quotes if present
  if (Length(Value) >= 2) and (Value[1] = '"') and (Value[Length(Value)] = '"') then
    Value := Copy(Value, 2, Length(Value) - 2);

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
