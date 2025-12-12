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
unit SedaiPackratParser;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, DateUtils,
  SedaiLexerTypes, SedaiLexerToken, SedaiTokenList, SedaiParserTypes,
  SedaiAST, SedaiParserContext, SedaiParserResults, SedaiParserErrors,
  SedaiPackratCore, SedaiExpressionParser, SedaiParserValidation,
  SedaiExecutorTypes, SedaiBasicKeywords;

type
  { TPackratParser - Main BASIC Parser with Packrat memoization }
  TPackratParser = class(TPackratCore)
  private
    FExpressionParser: TExpressionParser;
    FStartTime: TDateTime;
    FOptions: TParserOptions;
    FValidationStacks: TParserValidationStacks;

    // options & configuration
    function DefaultParserOptions: TParserOptions;
    function GetOptions: TParserOptions;
    function ParseArrayAccess: TASTNode;
    function ParseArrayDeclaration: TASTNode;

    // Helper methods for block parsing
    function ParseBlockUntil(EndTokens: array of TTokenType): TASTNode;
    function FindMatchingNext: Integer;
    function FindMatchingEnd(StartToken: TTokenType): Integer;
    function ParseDimensionList: TASTNode;
    procedure SetOptions(AValue: TParserOptions);

  protected
    procedure DoParsingStarted; virtual;
    procedure DoParsingFinished(Result: TParsingResult); virtual;

  public
    constructor Create;
    destructor Destroy; override;

    property Options: TParserOptions read GetOptions write SetOptions;

    // === CONTEXT MANAGEMENT OVERRIDE ===
    procedure SetContext(AContext: TParserContext); override;

    // === MAIN PARSING INTERFACE ===
    function Parse(TokenList: TTokenList): TParsingResult;
    function ParseExpression(TokenList: TTokenList): TParsingResult; overload;

    // === CORE PARSING METHODS ===
    function ParseProgram: TASTNode;
    function ParseStatement: TASTNode;

    function ParseAssignmentStatement: TASTNode;

    // === STATEMENT PARSING (Packrat memoized) ===
    function ParsePrintStatement: TASTNode;
    function ParseInputStatement: TASTNode;
    function ParseIOStatement: TASTNode;
    function ParseLetStatement: TASTNode;
    function ParseIfStatement: TASTNode;
    function ParseThenStatement: TASTNode;
    function ParseElseStatement: TASTNode;
    function ParseForStatement: TASTNode;
    function ParseDoStatement: TASTNode;
    function ParseGotoStatement: TASTNode;
    function ParseGosubStatement: TASTNode;
    function ParseReturnStatement: TASTNode;
    function ParseEndStatement: TASTNode;
    function ParseFastStatement: TASTNode;
    function ParseSlowStatement: TASTNode;
    function ParseRemStatement: TASTNode;
    function ParseDimStatement: TASTNode;
    function ParseDefStatement: TASTNode;
    function ParseFnStatement: TASTNode;
    function ParseConstStatement: TASTNode;
    function ParseDataStatement: TASTNode;
    function ParseReadStatement: TASTNode;
    function ParseRestoreStatement: TASTNode;
    function ParseClearStatement: TASTNode;
    function ParseStopStatement: TASTNode;
    function ParseContStatement: TASTNode;
    function ParseRunStatement: TASTNode;
    function ParseClockStatement: TASTNode;
    function ParseSleepStatement: TASTNode;
    function ParseWaitStatement: TASTNode;
    function ParseProgramEditingStatement: TASTNode;
    function ParseLoopStatement: TASTNode;
    function ParseLoopEndStatement: TASTNode;
    function ParseLoopControlStatement: TASTNode;
    function ParseJumpStatement: TASTNode;
    function ParseConditionalJumpStatement: TASTNode;
    function ParseWhileStatement: TASTNode;
    function ParseOnStatement: TASTNode;
    function ParseBlockStatement: TASTNode;
    function ParseBlockEndStatement: TASTNode;
    function ParseMemoryStatement: TASTNode;
    function ParseGraphicsStatement: TASTNode;
    function ParseSpriteStatement: TASTNode;
    function ParseSoundStatement: TASTNode;
    function ParseFileOperationStatement: TASTNode;
    function ParseFileManagementStatement: TASTNode;
    function ParseFileInputStatement: TASTNode;
    function ParseFileOutputStatement: TASTNode;
    function ParseErrorHandlingStatement: TASTNode;
    function ParseDebugStatement: TASTNode;
    function ParseTracingStatement: TASTNode;
    function ParseMonitorStatement: TASTNode;
    function ParseSysStatement: TASTNode;
    function ParseUsrStatement: TASTNode;
    function ParseKeyStatement: TASTNode;
    function ParseDirectiveStatement: TASTNode;

    // === EXPRESSION DELEGATION ===
    function ParseExpression: TASTNode; inline;
    function ParseExpressionList(Delimiter: TTokenType = ttSeparParam): TASTNode; inline;
    function ParseArgumentList: TASTNode; inline;

    function ParseExpressionStatement: TASTNode;

    // === UTILITY PARSING ===
    function ParseStatementList: TASTNode;

    // === VALIDATION ===
    function ValidateProgram: Boolean;
    property ValidationStacks: TParserValidationStacks read FValidationStacks;

    // === PROPERTIES ===
    property ExpressionParser: TExpressionParser read FExpressionParser;
  end;

// === FACTORY FUNCTIONS ===
function CreatePackratParser: TPackratParser;

implementation

uses
  Math, StrUtils, TypInfo;

{ TPackratParser }

constructor TPackratParser.Create;
begin
  inherited Create;
  FExpressionParser := TExpressionParser.Create;
  FOptions := DefaultParserOptions;

  // === TIER 1: Enable adaptive memoization for BASIC (linear programs) ===
  // Most BASIC programs are linear with simple control flow
  // Only complex expressions and nested structures need memoization
  MemoizationMode := mmAdaptive;
  MemoizationThreshold := 3;  // Cache after 3 recursion levels
end;

destructor TPackratParser.Destroy;
begin
  if Assigned(FValidationStacks) then
    FValidationStacks.Free;

  if Assigned(FExpressionParser) then
    FExpressionParser.Free;

  inherited Destroy;
end;

procedure TPackratParser.SetContext(AContext: TParserContext);
begin
  inherited SetContext(AContext);

  if Assigned(FValidationStacks) then
    FValidationStacks.Free;
  FValidationStacks := TParserValidationStacks.Create(AContext);

  if Assigned(FExpressionParser) then
    FExpressionParser.SetContext(AContext);
end;

// === MAIN PARSING METHODS ===

function TPackratParser.Parse(TokenList: TTokenList): TParsingResult;
var
  i: Integer;
begin
  FStartTime := Now;
  Result := TParsingResult.Create;

  try
    // Initialize context
    SetContext(TParserContext.Create(TokenList));
    {$IFDEF DEBUG}
    Context.DebugMode := DebugMode;
    {$ENDIF}

    DoParsingStarted;

    // Parse the program
    Result.AST := Memoize('Program', @ParseProgram);

    // *** VALIDATE ALL CONSTRUCTS CLOSED ***
    if Assigned(Result.AST) then
      Result.Success := ValidateProgram and not Context.HasErrors
    else
      Result.Success := False;

    Result.TokensConsumed := Context.CurrentIndex;
    Result.ParsingTime := MilliSecondsBetween(Now, FStartTime);

    // Copy errors to result
    if Context.HasErrors then
    begin
      for i := 0 to Context.Errors.Count - 1 do
        Result.AddError(TParserError(Context.Errors[i]));
    end;

    DoParsingFinished(Result);

  except
    on E: Exception do
    begin
      Result.Success := False;
      if Assigned(Context.CurrentToken) then
        Result.AddError(TParserError.Create('Internal parser error: ' + E.Message, Context.CurrentToken))
      else
        Result.AddError(TParserError.Create('Internal parser error at unknown position: ' + E.Message, nil));
    end;
  end;
end;

function TPackratParser.ParseExpression(TokenList: TTokenList): TParsingResult;
var
  i: Integer;
begin
  FStartTime := Now;
  Result := TParsingResult.Create;

  try
    // Initialize context
    SetContext(TParserContext.Create(TokenList));
    {$IFDEF DEBUG}
    Context.DebugMode := DebugMode;
    {$ENDIF}

    // Parse single expression using expression parser
    Result.AST := FExpressionParser.ParseExpression();
    Result.Success := Assigned(Result.AST) and not Context.HasErrors;
    Result.TokensConsumed := Context.CurrentIndex;
    Result.ParsingTime := MilliSecondsBetween(Now, FStartTime);

    // Copy errors
    if Context.HasErrors then
    begin
      for i := 0 to Context.Errors.Count - 1 do
        Result.AddError(TParserError(Context.Errors[i]));
    end;

  except
    on E: Exception do
    begin
      Result.Success := False;
      Result.AddError(TParserError.Create('Expression parser error: ' + E.Message, Context.CurrentToken));
    end;
  end;
end;

// === CORE PARSING METHODS ===

function TPackratParser.ParseProgram: TASTNode;
var
 Token: TLexerToken;
 LineNumber: TASTNode;
 Statement: TASTNode;
 LineNum: Integer;
begin
 Result := TASTNode.Create(antProgram);

 while not Context.IsAtEnd do
 begin
   Token := Context.CurrentToken;

   // Skip end-of-line tokens, MA prima fai Pop degli IF completati
   if Context.Match(ttEndOfLine) then
   begin
     // *** Pop IF completati a fine riga (solo se non hanno blocchi attivi) ***
     if FValidationStacks.HasActiveIf and FValidationStacks.CanPopIfAtEOL then
     begin
       FValidationStacks.PopIf;
     end;
     Continue;
   end;

   // Stop at end of file
   if Context.Check(ttEndOfFile) then
     Break;

   // Parse line number if present
   if Context.Check(ttLineNumber) then
   begin
     try
       LineNum := StrToInt(Token.Value);
     except
       LineNum := 0; // Fallback per valori non validi
     end;
     LineNumber := TASTNode.CreateWithValue(antLineNumber, Token.Value, Token);

     Result.AddChild(LineNumber);
     Context.Advance; // Consume line number
   end;

   // Parse ALL statements on this line (separated by :)
   while not Context.CheckAny([ttEndOfLine, ttEndOfFile]) do
   begin
     Token := Context.CurrentToken;

     // *** IMPORTANTE: Skip statement separators (:) PRIMA di chiamare ParseStatement ***
     if Context.Check(ttSeparStmt) then
     begin
       Context.Advance; // Consume ":"
       Continue; // Continue to next statement on same line
     end;

     Statement := Memoize('Statement', @ParseStatement);

     if Assigned(Statement) then
     begin
       Result.AddChild(Statement);
     end
     else
     begin
       // Se ParseStatement restituisce nil (es. ha skippato un separatore),
       // continua senza errore
       if Context.CurrentToken.Value = Token.Value then
       begin
         Context.Advance; // Evita loop infinito
       end;
     end;
   end;
 end;

 DoNodeCreated(Result);
end;

function TPackratParser.ParseStatement: TASTNode;
var
  Token: TLexerToken;
  ErrorToken: TLexerToken;
  SavedIndex: integer;
begin
  Result := nil;

  if not HasValidContext or Context.IsAtEnd then
    Exit;

  Token := Context.CurrentToken;
  if not Assigned(Token) then
    Exit;

  //WriteLn('DEBUG: Parsing token "', Token.Value, '" type=', Ord(Token.TokenType), ' at line=', Token.Line);

  //if Token.TokenType = ttIdentifier then
  //begin
  //  WriteLn('DEBUG: Found identifier: ', Token.Value);
  //  if Assigned(Context.PeekNext) then
  //    WriteLn('DEBUG: Next token: ', Context.PeekNext.Value, ' type: ', Ord(Context.PeekNext.TokenType));
  //end;

 // Skip statement separators (:)
 if Token.TokenType = ttSeparStmt then
 begin
   Context.Advance; // Consume ":"
   Result := nil;   // Return nil = no statement created
   Exit;
 end;

 // Skip other separators without creating nodes
 if Token.TokenType = ttSeparParam then
 begin
   Context.Advance; // Consume ","
   Result := nil;   // Return nil = no statement created
   Exit;
 end;

 // Route to appropriate statement parser based on keyword
 case Token.TokenType of
    // === I/O COMMANDS ===
    ttOutputCommand: Result := Memoize('PrintStatement', @ParsePrintStatement);
    ttInputCommand: Result := Memoize('InputStatement', @ParseInputStatement);
    ttIOCommand: Result := Memoize('IOStatement', @ParseIOStatement);

    // === DATA HANDLING ===
    ttDataAssignment: Result := Memoize('LetStatement', @ParseLetStatement);
    ttDataDeclaration: Result := Memoize('DimStatement', @ParseDimStatement);
    ttConstant: Result := Memoize('ConstStatement', @ParseConstStatement);
    ttDataConstant: Result := Memoize('DataStatement', @ParseDataStatement);
    ttDataRead: Result := Memoize('ReadStatement', @ParseReadStatement);
    ttDataClear: Result := Memoize('ClearStatement', @ParseClearStatement);

    // === FLOW CONTROL ===
    ttConditionalIf: Result := Memoize('IfStatement', @ParseIfStatement);
    ttConditionalThen: Result := Memoize('ThenStatement', @ParseThenStatement);
    ttConditionalElse: Result := Memoize('ElseStatement', @ParseElseStatement);
    ttLoopBlockStart: Result := Memoize('LoopStatement', @ParseLoopStatement);
    ttLoopBlockEnd: Result := Memoize('LoopEndStatement', @ParseLoopEndStatement);
    ttLoopControl: Result := Memoize('LoopControlStatement', @ParseLoopControlStatement);
    ttJumpGoto: Result := Memoize('GotoStatement', @ParseGotoStatement);
    ttJumpGosub: Result := Memoize('GosubStatement', @ParseGosubStatement);
    ttJumpKeyword:
    begin
      //WriteLn('>>> DEBUG: Found ttJumpKeyword="', Token.Value, '"');
      if UpperCase(Token.Value) = 'GOTO' then
        Result := Memoize('GotoStatement', @ParseGotoStatement)
      else
        Result := Memoize('JumpStatement', @ParseJumpStatement);
      //WriteLn('>>> DEBUG: Jump statement result=', Assigned(Result));
    end;
    ttJumpReturn: Result := Memoize('ReturnStatement', @ParseReturnStatement);
    ttJumpConditional: Result := Memoize('ConditionalJumpStatement', @ParseConditionalJumpStatement);

    // === PROGRAM CONTROL ===
    ttProgramEnd:
      begin
        //WriteLn('>>> DEBUG: Found ttProgramEnd, calling ParseEndStatement');
        Result := Memoize('EndStatement', @ParseEndStatement);
        //WriteLn('>>> DEBUG: ParseEndStatement result=', Assigned(Result));
      end;
    ttProgramStop: Result := Memoize('StopStatement', @ParseStopStatement);
    ttProgramRun: Result := Memoize('RunStatement', @ParseRunStatement);
    ttProgramCont: Result := Memoize('ContStatement', @ParseContStatement);
    ttProgramClock: Result := Memoize('ClockStatement', @ParseClockStatement);
    ttProgramSleep: Result := Memoize('SleepStatement', @ParseSleepStatement);
    ttProgramWait: Result := Memoize('WaitStatement', @ParseWaitStatement);
    ttProgramEditing: Result := Memoize('ProgramEditingStatement', @ParseProgramEditingStatement);

    // === BLOCK CONSTRUCTS ===
    ttBlockBegin: Result := Memoize('BlockStatement', @ParseBlockStatement);
    ttBlockEnd: Result := Memoize('BlockEndStatement', @ParseBlockEndStatement);

    // === COMMENTS ===
    ttCommentRemark: Result := Memoize('RemStatement', @ParseRemStatement);

    // === PROCEDURES ===
    ttProcedureDefine: Result := Memoize('DefStatement', @ParseDefStatement);
    ttProcedureStart: Result := Memoize('FnStatement', @ParseFnStatement);

    // === MEMORY COMMANDS ===
    ttMemoryCommand: Result := Memoize('MemoryStatement', @ParseMemoryStatement);

    // === GRAPHICS COMMANDS ===
    ttGraphicsCommand: Result := Memoize('GraphicsStatement', @ParseGraphicsStatement);

    // === SPRITE COMMANDS ===
    ttSpriteCommand: Result := Memoize('SpriteStatement', @ParseSpriteStatement);
    ttSpriteFunction: Result := Memoize('ExpressionStatement', @ParseExpressionStatement);

    // === SOUND COMMANDS ===
    ttSoundCommand: Result := Memoize('SoundStatement', @ParseSoundStatement);

    // === FILE OPERATIONS ===
    ttFileOperation: Result := Memoize('FileOperationStatement', @ParseFileOperationStatement);
    ttFileManagement: Result := Memoize('FileManagementStatement', @ParseFileManagementStatement);
    ttFileInputCommand: Result := Memoize('FileInputStatement', @ParseFileInputStatement);
    ttFileOutputCommand: Result := Memoize('FileOutputStatement', @ParseFileOutputStatement);

    // === ERROR HANDLING ===
    ttErrorHandlingCommand: Result := Memoize('ErrorHandlingStatement', @ParseErrorHandlingStatement);

    // === DEBUG ===
    ttDebugCommand: Result := Memoize('DebugStatement', @ParseDebugStatement);
    ttDebugTracingMode: Result := Memoize('TracingStatement', @ParseTracingStatement);

    // === MACHINE LANGUAGE ===
    ttMonitor: Result := Memoize('MonitorStatement', @ParseMonitorStatement);
    ttSysCommand: Result := Memoize('SysStatement', @ParseSysStatement);
    ttUsrFunction: Result := Memoize('UsrStatement', @ParseUsrStatement);

    // === SYSTEM HANDLING ===
    ttKeyDefine: Result := Memoize('KeyStatement', @ParseKeyStatement);

    // === DIRECTIVES ===
    ttDirective: Result := Memoize('DirectiveStatement', @ParseDirectiveStatement);

    // === INPUT FUNCTIONS ===
    ttInputFunction: Result := Memoize('ExpressionStatement', @ParseExpressionStatement);

    // === LITERALS AND PRIMITIVES ===
    ttStringLiteral: Result := Memoize('ExpressionStatement', @ParseExpressionStatement);
    ttNumber,
    ttInteger,
    ttFloat: Result := Memoize('ExpressionStatement', @ParseExpressionStatement);

    // === DELIMITERS (when used as standalone expressions) ===
    ttDelimParOpen,
    ttDelimParClose: Result := Memoize('ExpressionStatement', @ParseExpressionStatement);

    // === LOGICAL OPERATORS ===
    ttLogicalAND,
    ttLogicalOR,
    ttLogicalXOR,
    ttLogicalNOT: Result := Memoize('ExpressionStatement', @ParseExpressionStatement);

    // === IDENTIFIERS ===
    ttIdentifier:
      begin
        // Prova SEMPRE assignment prima per identifier
        SavedIndex := Context.CurrentIndex;

        Result := Memoize('AssignmentStatement', @ParseAssignmentStatement);

        if not Assigned(Result) then
        begin
          // Assignment fallito, riprova come expression
          Context.CurrentIndex := SavedIndex;
          Result := Memoize('ExpressionStatement', @ParseExpressionStatement);
        end;
      end;

    else
    begin
      ErrorToken := Context.CurrentToken;  // ← errore sul TOKEN CORRENTE, non quello catturato all'inizio
      //WriteLn('DEBUG: ERROR on token "', ErrorToken.Value, '" at line=', ErrorToken.Line);
      HandleError(Format('Unexpected token in statement: "%s"', [ErrorToken.Value]), ErrorToken);
      Result := nil;
    end;
  end;
end;

function TPackratParser.ParseAssignmentStatement: TASTNode;
var
 LeftSide, Expression: TASTNode;
 Token: TLexerToken;
 SavedToken: TLexerToken;
begin
 Token := Context.CurrentToken;

  // Parse left side - can be A or A(i)
  if Context.Check(ttIdentifier) and Assigned(Context.PeekNext) and
    (Context.PeekNext.TokenType = ttDelimParOpen) then
  begin
    // It's array access A(i) - use expression parser
    LeftSide := FExpressionParser.ParseExpression(precCall);
  end
  else if Context.Check(ttIdentifier) then
  begin
    // *** SAVE THE TOKEN BEFORE ADVANCING ***
    SavedToken := Context.CurrentToken;
    LeftSide := TASTNode.CreateWithValue(antIdentifier, UpperCase(Token.Value), Token);
    Context.Advance; // Consume identifier
    //WriteLn('DEBUG: Consumed identifier, next token: "', Context.CurrentToken.Value, '"');
  end
  else
  begin
    HandleError('Expected variable name in assignment', Context.CurrentToken);  // ← Token corrente
    Result := nil;
    Exit;
  end;

  // Expect =
  //WriteLn('DEBUG: Looking for =, current token: "', Context.CurrentToken.Value, '" type=', Ord(Context.CurrentToken.TokenType));
  if not Context.Match(ttOpEq) then
  begin
    //WriteLn('DEBUG: NO = found! Generating error!');
    // *** USA IL TOKEN SALVATO, NON IL CORRENTE ***
    HandleError('Expected "=" in assignment', SavedToken);
    if Assigned(LeftSide) then
      LeftSide.Free;
    Result := nil;
    Exit;
  end;

  // Parse right-hand expression
  Expression := FExpressionParser.ParseExpression;
  if not Assigned(Expression) then
  begin
    if Assigned(LeftSide) then
      LeftSide.Free;
    Result := nil;
    Exit;
  end;

  Result := TASTNode.Create(antAssignment);
  Result.AddChild(LeftSide);
  Result.AddChild(Expression);
  DoNodeCreated(Result);
end;

// === STATEMENT PARSING METHODS ===

function TPackratParser.ParsePrintStatement: TASTNode;
var
  Token: TLexerToken;
  Expr: TASTNode;
  SeparatorNode: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antPrint, Token);
  Context.Advance; // Consume PRINT

  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) do
  begin
    // Parse expression
    Expr := ParseExpression;
    if Assigned(Expr) then
      Result.AddChild(Expr)
    else
      Break;

    // Check for PRINT separators (comma or semicolon)
    if Context.CheckAny([ttSeparParam, ttSeparOutput]) then
    begin
      // Create separator node with actual separator value
      SeparatorNode := TASTNode.CreateWithValue(antSeparator, Context.CurrentToken.Value, Context.CurrentToken);
      Result.AddChild(SeparatorNode);
      Context.Advance; // Consume separator

      // If separator is at end of line, exit
      if Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) then
        Break;
    end
    else
      Break;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseInputStatement: TASTNode;
var
  Token: TLexerToken;
  Expr: TASTNode;
  SeparatorNode: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antInput, Token);
  Context.Advance; // Consume INPUT

  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) do
  begin
    // Parse expression (prompt string or variable)
    Expr := ParseExpression;
    if Assigned(Expr) then
      Result.AddChild(Expr)
    else
      Break;

    // Check for INPUT separators (comma or semicolon)
    if Context.CheckAny([ttSeparParam, ttSeparOutput]) then
    begin
      // Create separator node with actual separator value
      SeparatorNode := TASTNode.CreateWithValue(antSeparator, Context.CurrentToken.Value, Context.CurrentToken);
      Result.AddChild(SeparatorNode);
      Context.Advance; // Consume separator

      // If separator is at end of line, exit
      if Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) then
        Break;
    end
    else
      Break;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseLetStatement: TASTNode;
var
  Assignment: TASTNode;
  SavedToken: TLexerToken;
begin
  SavedToken := Context.CurrentToken;
  Context.Advance; // Consume LET (if present)

  // Parse assignment expression
  Assignment := ParseAssignmentStatement;
  if Assigned(Assignment) and (Assignment.NodeType = antAssignment) then
    Result := Assignment
  else
  begin
    HandleError('Expected assignment after LET', SavedToken);
    Result := nil;
  end;
end;

function TPackratParser.ParseIfStatement: TASTNode;
var
  Token: TLexerToken;
  Condition: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antIf, Token);
  Context.Advance; // Consume IF

  // Parse condition
  Condition := ParseExpression;
  if not Assigned(Condition) then
  begin
    HandleError('Expected condition after IF', Token);
    Result.Free;
    Result := nil;
    Exit;
  end;
  Result.AddChild(Condition);

  // *** PUSH ONTO IF STACK ***
  FValidationStacks.PushIf(Result, Context.CurrentIndex);

  DoNodeCreated(Result);
end;

function TPackratParser.ParseThenStatement: TASTNode;
var
 Token: TLexerToken;
 Statement: TASTNode;
 HasBeginBlock: Boolean;
 CurrentIf: TIfStackEntry;
 ThenNode: TASTNode;
 GotoNode: TASTNode;
begin
 Token := Context.CurrentToken;
 // *** VALIDATE THEN ***
 if not FValidationStacks.ValidateThen then
 begin
   Result := nil;
   Exit;
 end;
 ThenNode := TASTNode.Create(antThen, Token);
 Context.Advance; // Consume THEN

 // *** FIX CRITICO: Aggiungi THEN come figlio dell'IF corrente ***
 CurrentIf := FValidationStacks.GetCurrentIf;
 if Assigned(CurrentIf.IfNode) then
 begin
   CurrentIf.IfNode.AddChild(ThenNode);
 end;

 // *** ASSOCIATE WITH CURRENT IF ***
 FValidationStacks.SetThenForCurrentIf(ThenNode);

 // *** FIX: THEN <numero> → THEN GOTO <numero> ***
 if Context.Check(ttNumber) or Context.Check(ttLineNumber) then
 begin
   // THEN seguito da numero = GOTO implicito
   GotoNode := TASTNode.Create(antGoto);
   Statement := ParseExpression; // Il numero di riga
   if Assigned(Statement) then
     GotoNode.AddChild(Statement);
   ThenNode.AddChild(GotoNode);

   DoNodeCreated(ThenNode);
   Result := nil;
   Exit;
 end;

 // *** Check if the first statement is BEGIN ***
 HasBeginBlock := Context.Check(ttBlockBegin);
 if HasBeginBlock then
   FValidationStacks.SetThenBlockForCurrentIf;

 // *** Parse THEN statements until : ELSE or EOL ***
 while not Context.CheckAny([ttEndOfLine, ttEndOfFile]) do
 begin
   // *** CHECK FOR : ELSE SEQUENCE ***
   if Context.Check(ttSeparStmt) then
   begin
     // Look ahead for ELSE after :
     if Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttConditionalElse) then
     begin
       // Found : ELSE - STOP parsing THEN statements
       Break;
     end
     else
     begin
       // Just a : separator between statements in THEN - SKIP and continue
       Context.Advance; // Consume :
       Continue;
     end;
   end;

   // Parse statement and add to THEN
   Statement := ParseStatement;
   if Assigned(Statement) then
   begin
     ThenNode.AddChild(Statement);
   end
   else
     Break;
 end;

 DoNodeCreated(ThenNode);
 // *** FIX: DON'T return the node to avoid duplication ***
 Result := nil;
end;

function TPackratParser.ParseElseStatement: TASTNode;
var
  Token: TLexerToken;
  Statement: TASTNode;
  HasBeginBlock: Boolean;
  CurrentIf: TIfStackEntry;
  ElseNode: TASTNode;
begin
  Token := Context.CurrentToken;
  // *** VALIDATE ELSE ***
  if not FValidationStacks.ValidateElse then
  begin
    Result := nil;
    Exit;
  end;
  ElseNode := TASTNode.Create(antElse, Token);
  Context.Advance; // Consume ELSE

  // *** FIX CRITICO: Aggiungi ELSE come figlio dell'IF corrente ***
  CurrentIf := FValidationStacks.GetCurrentIf;
  if Assigned(CurrentIf.IfNode) then
  begin
    CurrentIf.IfNode.AddChild(ElseNode);
    //WriteLn('DEBUG: ELSE added as child to IF - IF now has ', CurrentIf.IfNode.ChildCount, ' children');
  end;

  // *** Check if the first statement is BEGIN ***
  HasBeginBlock := Context.Check(ttBlockBegin);
  if HasBeginBlock then
    FValidationStacks.SetElseBlockForCurrentIf;

  // *** Parse ELSE statements until EOL ***
  while not Context.CheckAny([ttEndOfLine, ttEndOfFile]) do
  begin
    if Context.Check(ttSeparStmt) then
    begin
      Context.Advance; // Consume : and continue
      Continue;
    end;

    // Parse statement and add to ELSE
    Statement := ParseStatement;
    if Assigned(Statement) then
    begin
      ElseNode.AddChild(Statement);
      //WriteLn('DEBUG: Added statement to ELSE: ', NodeTypeToString(Statement.NodeType));
    end
    else
      Break;
  end;

  //WriteLn('DEBUG: ELSE completed with ', ElseNode.ChildCount, ' child statements');
  DoNodeCreated(ElseNode);

  // *** FIX: DON'T return the node to avoid duplication ***
  Result := nil;
end;

function TPackratParser.ParseForStatement: TASTNode;
var
  Variable, StartExpr, EndExpr, StepExpr: TASTNode;
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antForLoop, Token);
  Context.Advance; // Consume FOR

  // *** PUSH ONTO LOOP STACK ***
  FValidationStacks.PushLoop(ttLoopBlockStart, Result, 'FOR', 'NEXT', Context.CurrentIndex);

  // Parse: variable = start TO end [STEP step]
  if not Context.Check(ttIdentifier) then
  begin
    HandleError('Expected variable name after FOR', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  Variable := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
  Context.Advance;
  Result.AddChild(Variable);

  if not Context.Match(ttOpEq) and not Context.Match(ttDataAssignment) then
  begin
    HandleError('Expected "=" after FOR variable', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  StartExpr := ParseExpression;
  if not Assigned(StartExpr) then
  begin
    HandleError('Expected start value in FOR statement', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;
  Result.AddChild(StartExpr);

  if not (Context.Check(ttLoopControl) and (UpperCase(Context.CurrentToken.Value) = 'TO')) then
  begin
    HandleError('Expected "TO" in FOR statement', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;
  Context.Advance; // Consume TO

  EndExpr := ParseExpression;
  if not Assigned(EndExpr) then
  begin
    HandleError('Expected end value in FOR statement', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;
  Result.AddChild(EndExpr);

  // Optional STEP
  if Context.Check(ttLoopControl) and (UpperCase(Context.CurrentToken.Value) = 'STEP') then
  begin
    Context.Advance; // Consume STEP
    StepExpr := ParseExpression;
    if Assigned(StepExpr) then
      Result.AddChild(StepExpr);
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseDoStatement: TASTNode;
var
  Body: TASTNode;
  EndIndex: Integer;
  Token: TLexerToken;
  Condition: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antDoLoop, Token);
  Context.Advance; // Consume DO

  // Find matching LOOP
  EndIndex := FindMatchingEnd(ttLoopBlockStart);
  if EndIndex = -1 then
  begin
    HandleError('DO statement without matching LOOP', Token);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse body until LOOP
  Body := ParseBlockUntil([ttLoopBlockEnd]);
  if Assigned(Body) then
    Result.AddChild(Body);

  // Consume LOOP
  if Context.Match(ttLoopBlockEnd) and (UpperCase(Context.PreviousToken.Value) = 'LOOP') then
  begin
    // Optional condition after LOOP (WHILE or UNTIL)
    if Context.Match(ttLoopControl) then
    begin
      Condition := ParseExpression;
      if Assigned(Condition) then
        Result.AddChild(Condition);
    end;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseGotoStatement: TASTNode;
var
  Target: TASTNode;
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antGoto, Token);
  Context.Advance; // Consume GOTO

  Target := ParseExpression;
  if Assigned(Target) then
    Result.AddChild(Target);

  DoNodeCreated(Result);
end;

function TPackratParser.ParseGosubStatement: TASTNode;
var
  Target: TASTNode;
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antGosub, Token);
  Context.Advance; // Consume GOSUB

  // *** PUSH ONTO GOSUB STACK ***
  //if not Assigned(FValidationStacks) and Assigned(Context) then
  //  FValidationStacks := TParserValidationStacks.Create(Context);
  //if Assigned(FValidationStacks) then
  //  FValidationStacks.PushGosub(Result, Context.CurrentIndex);

  Target := ParseExpression;
  if Assigned(Target) then
    Result.AddChild(Target);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseReturnStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;

  //if not Assigned(FValidationStacks) and Assigned(Context) then
  //  FValidationStacks := TParserValidationStacks.Create(Context);
  //
  //// *** VALIDATE RETURN ***
  //if not FValidationStacks.ValidateReturn then
  //begin
  //  Result := nil;
  //  Exit;
  //end;

  Result := TASTNode.Create(antReturn, Token);
  Context.Advance; // Consume RETURN
  DoNodeCreated(Result);
end;

function TPackratParser.ParseEndStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antEnd, Token);
  Context.Advance; // Consume END
  DoNodeCreated(Result);
end;

function TPackratParser.ParseFastStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antFast, Token);
  Context.Advance; // Consume FAST
  DoNodeCreated(Result);
end;

function TPackratParser.ParseSlowStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antSlow, Token);
  Context.Advance; // Consume SLOW
  DoNodeCreated(Result);
end;

function TPackratParser.ParseRemStatement: TASTNode;
var
  Comment: string;
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Context.Advance; // Consume REM

  // If comments are disabled, ignore everything and return nil
  if not FOptions.IncludeComments then
  begin
    // Consume everything until end of line but don't create AST nodes
    while not Context.CheckAny([ttEndOfLine, ttEndOfFile]) do
      Context.Advance;
    Result := nil;
    Exit;
  end;

  // If comments are enabled, create the node as before
  Result := TASTNode.Create(antRem, Token);

  // Consume rest of line as comment
  Comment := '';
  while not Context.CheckAny([ttEndOfLine, ttEndOfFile]) do
  begin
    Comment := Comment + Context.CurrentToken.Value + ' ';
    Context.Advance;
  end;

  Result.Value := Trim(Comment);
  DoNodeCreated(Result);
end;

// === ADDITIONAL STATEMENT IMPLEMENTATIONS ===

function TPackratParser.ParseIOStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume IO command
  DoNodeCreated(Result);
end;

function TPackratParser.ParseContStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume CONT
  DoNodeCreated(Result);
end;

function TPackratParser.ParseClockStatement: TASTNode;
var
  Token: TLexerToken;
  NodeType: TASTNodeType;
begin
  Token := Context.CurrentToken;
  // Distinguish FAST from SLOW by checking token value
  if SameText(Token.Value, kFAST) then
    NodeType := antFast
  else
    NodeType := antSlow;
  Result := TASTNode.Create(NodeType, Token);
  Context.Advance; // Consume FAST/SLOW
  DoNodeCreated(Result);
end;

function TPackratParser.ParseSleepStatement: TASTNode;
var
  Token: TLexerToken;
  Param: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antSleep, Token);
  Context.Advance; // Consume SLEEP

  // Parse required parameter (seconds to sleep)
  if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) then
  begin
    Param := ParseExpression;
    if Assigned(Param) then
      Result.AddChild(Param);
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseWaitStatement: TASTNode;
var
  Token: TLexerToken;
  Condition: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume WAIT

  // Parse condition
  if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) then
  begin
    Condition := ParseExpression;
    if Assigned(Condition) then
      Result.AddChild(Condition);
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseFnStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume FN
  DoNodeCreated(Result);
end;

function TPackratParser.ParseLoopEndStatement: TASTNode;
var
  Token: TLexerToken;
  EndKeyword: string;
begin
  Token := Context.CurrentToken;
  EndKeyword := UpperCase(Token.Value);

  // *** VALIDATE LOOP END ***
  if not FValidationStacks.ValidateLoopEnd(EndKeyword) then
  begin
    Result := nil;
    Exit;
  end;

  Result := TASTNode.Create(antNext, Token);
  Context.Advance; // Consume NEXT/LOOP/WEND

  // Optional variable after NEXT
  if (EndKeyword = 'NEXT') and Context.Check(ttIdentifier) then
    Context.Advance; // Consume variable name

  DoNodeCreated(Result);
end;

function TPackratParser.ParseLoopControlStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume TO/STEP/WHILE/UNTIL
  DoNodeCreated(Result);
end;

function TPackratParser.ParseConditionalJumpStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume ON
  DoNodeCreated(Result);
end;

function TPackratParser.ParseBlockStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antBlock, Token);
  Context.Advance; // Consume BEGIN

  // *** PUSH ONTO BLOCK STACK ***
  FValidationStacks.PushBlock(ttBlockBegin, Result, 'END', Context.CurrentIndex);

  DoNodeCreated(Result);
end;

function TPackratParser.ParseBlockEndStatement: TASTNode;
var
  Token: TLexerToken;
  EndKeyword: string;
begin
  Token := Context.CurrentToken;
  EndKeyword := UpperCase(Token.Value);

  // *** VALIDATE BLOCK END ***
  if not FValidationStacks.ValidateBlockEnd(EndKeyword) then
  begin
    Result := nil;
    Exit;
  end;

  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume END/BEND

  // *** Check if this BEND closes an IF/THEN/ELSE block ***
  if FValidationStacks.HasActiveIf and FValidationStacks.CanPopIfAtEOL then
  begin
    FValidationStacks.PopIf;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseMemoryStatement: TASTNode;
var
  Token: TLexerToken;
  Params: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume memory command

  // Parse parameters as single expression list
  if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) then
  begin
    Params := ParseExpressionList(ttSeparParam);
    if Assigned(Params) then
      Result.AddChild(Params);
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseGraphicsStatement: TASTNode;
var
  Token: TLexerToken;
  Param: TASTNode;
  ParamCount, MaxParams: Integer;
  CmdName: string;
begin
  Token := Context.CurrentToken;
  CmdName := UpperCase(Token.Value);

  // Select appropriate node type based on command
  if CmdName = 'GRAPHIC' then
    Result := TASTNode.Create(antGraphics, Token)
  else if CmdName = 'SCNCLR' then
    Result := TASTNode.Create(antScnClr, Token)
  else if CmdName = 'BOX' then
    Result := TASTNode.Create(antBox, Token)
  else if CmdName = 'CIRCLE' then
    Result := TASTNode.Create(antCircle, Token)
  else if CmdName = 'DRAW' then
    Result := TASTNode.Create(antDraw, Token)
  else if CmdName = 'LOCATE' then
    Result := TASTNode.Create(antLocate, Token)
  else
    Result := TASTNode.Create(antStatement, Token);

  Context.Advance;
  ParamCount := 0;

  // CIRCLE has up to 9 parameters, BOX has up to 8, LOCATE has 2, SCNCLR has 1, other graphics commands have up to 5
  if CmdName = 'CIRCLE' then
    MaxParams := 9
  else if CmdName = 'BOX' then
    MaxParams := 8
  else if CmdName = 'LOCATE' then
    MaxParams := 2
  else if CmdName = 'DRAW' then
    MaxParams := 100  // DRAW can have many TO segments
  else if CmdName = 'SCNCLR' then
    MaxParams := 1  // SCNCLR [mode] - optional mode 0-11
  else
    MaxParams := 5;

  // Special handling for DRAW: parse color, x1, y1 [TO x2, y2] ...
  // Each segment (including TO keyword) is stored as children
  if CmdName = 'DRAW' then
  begin
    // Parse optional color (can be omitted but comma must remain)
    if Context.Check(ttSeparParam) then
    begin
      // Color omitted, add nil placeholder
      Result.AddChild(nil);
      Context.Advance;
    end
    else if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) then
    begin
      Param := ParseExpression;
      if Assigned(Param) then
        Result.AddChild(Param);
      if Context.Check(ttSeparParam) then
        Context.Advance;
    end;

    // Parse x1, y1
    while ParamCount < 2 do
    begin
      if Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) then
        Break;
      Param := ParseExpression;
      if Assigned(Param) then
      begin
        Result.AddChild(Param);
        Inc(ParamCount);
      end
      else
        Break;
      if Context.Check(ttSeparParam) then
        Context.Advance;
    end;

    // Parse TO x2, y2 segments (can have multiple)
    while Context.Check(ttLoopControl) and (UpperCase(Context.CurrentToken.Value) = 'TO') do
    begin
      Context.Advance; // consume TO
      // Parse x, y coordinates
      Param := ParseExpression;
      if Assigned(Param) then
        Result.AddChild(Param);
      if Context.Check(ttSeparParam) then
        Context.Advance;
      Param := ParseExpression;
      if Assigned(Param) then
        Result.AddChild(Param);
      if Context.Check(ttSeparParam) then
        Context.Advance;
    end;
  end
  else
  begin
    // Standard parsing for other graphics commands
    while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) and (ParamCount < MaxParams) do
    begin
      if Context.Check(ttSeparParam) then
      begin
        Context.Advance;
        Continue;
      end;
      Param := ParseExpression;
      if Assigned(Param) then
      begin
        Result.AddChild(Param);
        Inc(ParamCount);
      end
      else
        Break;
      if Context.Check(ttSeparParam) then
        Context.Advance
      else if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) then
        Break;
    end;
  end;
  DoNodeCreated(Result);
end;

function TPackratParser.ParseSpriteStatement: TASTNode;
var
  Token: TLexerToken;
  Params: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume sprite command

  // Parse parameters
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) do
  begin
    Params := ParseExpression;
    if Assigned(Params) then
      Result.AddChild(Params)
    else
      Break;

    // Handle comma separator
    if Context.Check(ttSeparParam) then
      Context.Advance
    else
      Break;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseSoundStatement: TASTNode;
var
  Token: TLexerToken;
  Params: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume sound command

  // Parse parameters
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) do
  begin
    Params := ParseExpression;
    if Assigned(Params) then
      Result.AddChild(Params)
    else
      Break;

    // Handle comma separator
    if Context.Check(ttSeparParam) then
      Context.Advance
    else
      Break;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseFileOperationStatement: TASTNode;
var
  Token: TLexerToken;
  Param: TASTNode;
begin
  Token := Context.CurrentToken;

  // *** FIX: Riconosci LOAD specificamente ***
  if UpperCase(Token.Value) = 'LOAD' then
    Result := TASTNode.Create(antLoad, Token)
  else
    Result := TASTNode.Create(antStatement, Token); // Altri comandi file

  Context.Advance; // Consume file operation command

  // Parse ALL parameters until end of statement
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) do
  begin
    // Skip commas
    if Context.Check(ttSeparParam) then
    begin
      Context.Advance;
      Continue;
    end;

    Param := ParseExpression;
    if Assigned(Param) then
      Result.AddChild(Param)
    else
      Break;

    // Handle comma separator
    if Context.Check(ttSeparParam) then
      Context.Advance
    else if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) then
      Break;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseFileManagementStatement: TASTNode;
var
  Token: TLexerToken;
  Params: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume file management command

  // Parse parameters
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) do
  begin
    Params := ParseExpression;
    if Assigned(Params) then
      Result.AddChild(Params)
    else
      Break;

    // Handle comma separator
    if Context.Check(ttSeparParam) then
      Context.Advance
    else
      Break;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseFileInputStatement: TASTNode;
var
  Token: TLexerToken;
  Params: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume file input command

  // Parse parameters
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) do
  begin
    Params := ParseExpression;
    if Assigned(Params) then
      Result.AddChild(Params)
    else
      Break;

    // Handle comma separator
    if Context.Check(ttSeparParam) then
      Context.Advance
    else
      Break;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseFileOutputStatement: TASTNode;
var
  Token: TLexerToken;
  Params: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume file output command

  // Parse parameters
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) do
  begin
    Params := ParseExpression;
    if Assigned(Params) then
      Result.AddChild(Params)
    else
      Break;

    // Handle comma separator
    if Context.Check(ttSeparParam) then
      Context.Advance
    else
      Break;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseErrorHandlingStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume error handling command
  DoNodeCreated(Result);
end;

function TPackratParser.ParseDebugStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume debug command
  DoNodeCreated(Result);
end;

function TPackratParser.ParseTracingStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume TRON/TROFF
  DoNodeCreated(Result);
end;

function TPackratParser.ParseMonitorStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume MONITOR
  DoNodeCreated(Result);
end;

function TPackratParser.ParseSysStatement: TASTNode;
var
  Token: TLexerToken;
  Address: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume SYS

  // Parse address parameter
  if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) then
  begin
    Address := ParseExpression;
    if Assigned(Address) then
      Result.AddChild(Address);
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseUsrStatement: TASTNode;
var
  Token: TLexerToken;
  Address: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antUsrFunction, Token);
  Context.Advance; // Consume USR

  // Expect opening parenthesis
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected "(" after USR', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse address parameter
  Address := ParseExpression;
  if Assigned(Address) then
    Result.AddChild(Address)
  else
  begin
    HandleError('Expected address parameter for USR', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Expect closing parenthesis
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" after USR address', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseKeyStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume KEY
  DoNodeCreated(Result);
end;

function TPackratParser.ParseDirectiveStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume directive
  DoNodeCreated(Result);
end;

function TPackratParser.ParseExpressionStatement: TASTNode;
begin
  // Parse as expression, could be assignment or function call
  Result := ParseExpression;
end;

// === STUB IMPLEMENTATIONS ===

function TPackratParser.ParseArrayDeclaration: TASTNode;
var
  VarName: TASTNode;
  Dimensions: TASTNode;
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;

  // Parse variable name
  if not Context.Check(ttIdentifier) then
  begin
    HandleError('Expected variable name in array declaration', Token);
    Result := nil;
    Exit;
  end;

  VarName := TASTNode.CreateWithValue(antIdentifier, UpperCase(Token.Value), Token);
  Context.Advance;

  // Expect opening parenthesis
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected "(" after array name', Context.CurrentToken);
    VarName.Free;
    Result := nil;
    Exit;
  end;

  // Parse dimensions
  Dimensions := ParseDimensionList;
  if not Assigned(Dimensions) then
  begin
    HandleError('Expected dimension list', Context.CurrentToken);
    VarName.Free;
    Result := nil;
    Exit;
  end;

  // Expect closing parenthesis
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" after dimension list', Context.CurrentToken);
    VarName.Free;
    Dimensions.Free;
    Result := nil;
    Exit;
  end;

  // Create array declaration node
  Result := TASTNode.Create(antArrayDecl, Token);
  Result.AddChild(VarName);
  Result.AddChild(Dimensions);

  DoNodeCreated(Result);
end;

function TPackratParser.ParseDimensionList: TASTNode;
var
  Dimension: TASTNode;
begin
  Result := TASTNode.Create(antDimensions);

  repeat
    Dimension := ParseExpression;
    if Assigned(Dimension) then
      Result.AddChild(Dimension)
    else
      Break;

    // Check for comma separator
    if Context.Check(ttSeparParam) then
      Context.Advance // Consume comma
    else
      Break; // No more dimensions

  until Context.CheckAny([ttDelimParClose, ttEndOfLine, ttEndOfFile]);

  DoNodeCreated(Result);
end;

function TPackratParser.ParseArrayAccess: TASTNode;
var
  ArrayName: TASTNode;
  Indices: TASTNode;
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;

  // Parse array name
  if not Context.Check(ttIdentifier) then
  begin
    Result := nil;
    Exit;
  end;

  ArrayName := TASTNode.CreateWithValue(antIdentifier, Token.Value, Token);
  Context.Advance;

  // Expect opening parenthesis
  if not Context.Match(ttDelimParOpen) then
  begin
    // Not an array access, just return the identifier
    Result := ArrayName;
    Exit;
  end;

  // Parse index expressions
  Indices := ParseExpressionList(ttSeparParam);
  if not Assigned(Indices) then
  begin
    HandleError('Expected index expression', Context.CurrentToken);
    ArrayName.Free;
    Result := nil;
    Exit;
  end;

  // Expect closing parenthesis
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ")" after array indices', Context.CurrentToken);
    ArrayName.Free;
    Indices.Free;
    Result := nil;
    Exit;
  end;

  // Create array access node
  Result := TASTNode.Create(antArrayAccess, Token);
  Result.AddChild(ArrayName);
  Result.AddChild(Indices);

  DoNodeCreated(Result);
end;

function TPackratParser.ParseDimStatement: TASTNode;
var
  Token: TLexerToken;
  ArrayDecl: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antDim, Token);
  Context.Advance; // Consume DIM

  // Parse array declarations separated by commas
  repeat
    ArrayDecl := ParseArrayDeclaration;
    if Assigned(ArrayDecl) then
      Result.AddChild(ArrayDecl)
    else
    begin
      HandleError('Expected array declaration after DIM', Context.CurrentToken);
      Break;
    end;

    // Check for comma separator
    if Context.Check(ttSeparParam) then
      Context.Advance // Consume comma
    else
      Break; // No more array declarations

  until Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]);

  DoNodeCreated(Result);
end;

function TPackratParser.ParseDefStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antDef, Token);
  Context.Advance; // Consume DEF
  DoNodeCreated(Result);
end;

function TPackratParser.ParseConstStatement: TASTNode;
var
  Token: TLexerToken;
  Assignment: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antConst, Token);
  Context.Advance; // Consume CONST

  // Parse assignment expression (name = value)
  Assignment := ParseAssignmentStatement;
  if Assigned(Assignment) and (Assignment.NodeType = antAssignment) then
    Result.AddChild(Assignment)
  else
  begin
    HandleError('Expected assignment after CONST', Token);
    Result.Free;
    Result := nil;
    Exit;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseDataStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antData, Token);
  Context.Advance; // Consume DATA
  DoNodeCreated(Result);
end;

function TPackratParser.ParseReadStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antRead, Token);
  Context.Advance; // Consume READ
  DoNodeCreated(Result);
end;

function TPackratParser.ParseRestoreStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antRestore, Token);
  Context.Advance; // Consume RESTORE
  DoNodeCreated(Result);
end;

function TPackratParser.ParseClearStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume CLR
  DoNodeCreated(Result);
end;

function TPackratParser.ParseStopStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume STOP
  DoNodeCreated(Result);
end;

function TPackratParser.ParseRunStatement: TASTNode;
var
  Token: TLexerToken;
  LineNumber: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antRun, Token);
  Context.Advance; // Consume RUN

  // Optional line number parameter
  if Context.Check(ttLineNumber) or Context.Check(ttInteger) then
  begin
    LineNumber := ParseExpression;
    if Assigned(LineNumber) then
      Result.AddChild(LineNumber);
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseProgramEditingStatement: TASTNode;
var
  Token: TLexerToken;
  Param: TASTNode;
begin
  Token := Context.CurrentToken;

  // *** FIX: Riconosci LIST specificamente ***
  if UpperCase(Token.Value) = 'LIST' then
    Result := TASTNode.Create(antList, Token)
  else
    Result := TASTNode.Create(antStatement, Token);

  Context.Advance; // Consume command

  // Parse ALL parameters until end of statement
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) do
  begin
    // Skip commas between parameters
    if Context.Check(ttSeparParam) then
    begin
      Context.Advance;
      Continue;
    end;

    Param := ParseExpression;
    if Assigned(Param) then
      Result.AddChild(Param)
    else
      Break;

    // Handle comma separator
    if Context.Check(ttSeparParam) then
      Context.Advance
    else if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) then
      Break;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseLoopStatement: TASTNode;
var
 Token: TLexerToken;
begin
 Token := Context.CurrentToken;

 // Determine loop type based on keyword
 if UpperCase(Token.Value) = 'FOR' then
   Result := ParseForStatement
 else if UpperCase(Token.Value) = 'DO' then
   Result := ParseDoStatement
 else if UpperCase(Token.Value) = 'WHILE' then
   Result := ParseWhileStatement
 else
 begin
   HandleError(Format('Unknown loop type: "%s"', [Context.CurrentToken.Value]), Context.CurrentToken);
   Result := nil;
 end;
end;

function TPackratParser.ParseJumpStatement: TASTNode;
var
 Token: TLexerToken;
begin
 Token := Context.CurrentToken;

 // Determine jump type based on keyword
 if UpperCase(Token.Value) = 'GOTO' then
   Result := ParseGotoStatement
 else if UpperCase(Token.Value) = 'GOSUB' then
   Result := ParseGosubStatement
 else if UpperCase(Token.Value) = 'ON' then
   Result := ParseOnStatement
 else
 begin
   HandleError(Format('Unknown jump type: "%s"', [Context.CurrentToken.Value]), Context.CurrentToken);
   Result := nil;
 end;
end;

function TPackratParser.ParseWhileStatement: TASTNode;
var
  Condition, Body: TASTNode;
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antWhileLoop, Token);
  Context.Advance; // Consume WHILE

  // Parse condition
  Condition := ParseExpression;
  if not Assigned(Condition) then
  begin
    Result.Free;
    Result := nil;
    Exit;
  end;
  Result.AddChild(Condition);

  // Parse body until matching WEND or END WHILE
  Body := ParseBlockUntil([ttLoopBlockEnd]);
  if Assigned(Body) then
    Result.AddChild(Body);

  DoNodeCreated(Result);
end;

function TPackratParser.ParseOnStatement: TASTNode;
var
  Expression, Target: TASTNode;
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume ON

  // Parse expression
  Expression := ParseExpression;
  if Assigned(Expression) then
    Result.AddChild(Expression);

  // Expect GOTO or GOSUB
  if Context.Match(ttJumpKeyword) then
  begin
    // Parse target list
    Target := ParseExpressionList(ttSeparParam);
    if Assigned(Target) then
      Result.AddChild(Target);
  end
  else
  begin
    HandleError('Expected GOTO or GOSUB after ON expression', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  DoNodeCreated(Result);
end;

// === HELPER METHODS FOR BLOCK PARSING ===

function TPackratParser.DefaultParserOptions: TParserOptions;
begin
  Result.IncludeComments := False;        // REM ignorati per performance
  Result.IncludeLineNumbers := True;      // Line numbers utili per debug
  Result.OptimizeAST := True;             // Ottimizzazioni attive
  Result.StrictMode := True;              // Parsing rigoroso
  Result.ArrayIndexMode := aimMaxIndex;   // Commodore BASIC style by default
end;

function TPackratParser.GetOptions: TParserOptions;
begin
  Result := FOptions;
end;

procedure TPackratParser.SetOptions(AValue: TParserOptions);
begin
  FOptions := AValue;
end;

function TPackratParser.ParseBlockUntil(EndTokens: array of TTokenType): TASTNode;
var
  Statement: TASTNode;
  Token: TLexerToken;
  i: Integer;
  Found: Boolean;
begin
  Result := TASTNode.Create(antBlock);

  while not Context.IsAtEnd do
  begin
    Token := Context.CurrentToken;

    // Check if we hit an end token
    Found := False;
    for i := Low(EndTokens) to High(EndTokens) do
    begin
      if Token.TokenType = EndTokens[i] then
      begin
        Found := True;
        Break;
      end;
    end;

    if Found then
      Break;

    // Skip line numbers and EOL - they remain at root level
    if Context.Match(ttLineNumber) or Context.Match(ttEndOfLine) then
      Continue;

    // Parse statement within block
    Statement := ParseStatement;
    if Assigned(Statement) then
      Result.AddChild(Statement)
    else
      Break;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.FindMatchingNext: Integer;
var
  i, NestedLevel: Integer;
  Token: TLexerToken;
begin
  Result := -1;
  NestedLevel := 0;

  for i := Context.CurrentIndex to Context.TokenList.Count - 1 do
  begin
    Token := TLexerToken(Context.TokenList[i]);
    if not Assigned(Token) or (Token.TokenType = ttEndOfFile) then
      Break;

    if (Token.TokenType = ttLoopBlockStart) and (UpperCase(Token.Value) = 'FOR') then
      Inc(NestedLevel)
    else if (Token.TokenType = ttLoopBlockEnd) and (UpperCase(Token.Value) = 'NEXT') then
    begin
      if NestedLevel = 0 then
      begin
        Result := i;
        Break;
      end
      else
        Dec(NestedLevel);
    end;
  end;
end;

function TPackratParser.FindMatchingEnd(StartToken: TTokenType): Integer;
var
  i, NestedLevel: Integer;
  Token: TLexerToken;
  StartKeyword, EndKeyword: string;
begin
  Result := -1;
  NestedLevel := 0;

  // Determine what end token we're looking for
  case StartToken of
    ttLoopBlockStart: EndKeyword := 'LOOP';
    ttBlockBegin: EndKeyword := 'END';
    else Exit;
  end;

  StartKeyword := Context.CurrentToken.Value;

  for i := Context.CurrentIndex to Context.TokenList.Count - 1 do
  begin
    Token := TLexerToken(Context.TokenList[i]);
    if not Assigned(Token) or (Token.TokenType = ttEndOfFile) then
      Break;

    if (Token.TokenType = StartToken) and (UpperCase(Token.Value) = UpperCase(StartKeyword)) then
      Inc(NestedLevel)
    else if (UpperCase(Token.Value) = EndKeyword) then
    begin
      if NestedLevel = 0 then
      begin
        Result := i;
        Break;
      end
      else
        Dec(NestedLevel);
    end;
  end;
end;

// === EXPRESSION DELEGATION ===

function TPackratParser.ParseExpression: TASTNode;
begin
  if Assigned(FExpressionParser) then
    Result := FExpressionParser.ParseExpression()
  else
    Result := nil;
end;

function TPackratParser.ParseExpressionList(Delimiter: TTokenType): TASTNode;
begin
  if Assigned(FExpressionParser) then
    Result := FExpressionParser.ParseExpressionList(Delimiter)
  else
    Result := nil;
end;

function TPackratParser.ParseArgumentList: TASTNode;
begin
  if Assigned(FExpressionParser) then
    Result := FExpressionParser.ParseArgumentList()
  else
    Result := nil;
end;

// === UTILITY PARSING ===

function TPackratParser.ParseStatementList: TASTNode;
var
 Statement: TASTNode;
begin
 Result := TASTNode.Create(antBlock);

 repeat
   Statement := ParseStatement;
   if Assigned(Statement) then
     Result.AddChild(Statement)
   else
     Break;

 until not Context.Match(ttSeparStmt);

 DoNodeCreated(Result);
end;

function TPackratParser.ValidateProgram: Boolean;
begin
  Result := FValidationStacks.ValidateAllClosed;
end;

// === VIRTUAL EVENT METHODS ===

procedure TPackratParser.DoParsingStarted;
begin
  // Override in subclasses
end;

procedure TPackratParser.DoParsingFinished(Result: TParsingResult);
begin
  // Override in subclasses
end;

// === FACTORY FUNCTIONS ===

function CreatePackratParser: TPackratParser;
begin
  Result := TPackratParser.Create;
end;

end.
