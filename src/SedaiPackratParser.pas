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
  Classes, SysUtils, DateUtils, Variants,
  SedaiLexerTypes, SedaiLexerToken, SedaiTokenList, SedaiParserTypes,
  SedaiAST, SedaiParserContext, SedaiParserResults, SedaiParserErrors,
  SedaiPackratCore, SedaiExpressionParser, SedaiParserValidation,
  SedaiExecutorTypes, SedaiBasicKeywords;

type
  // Dialect selection for the parser. pdAuto (default) detects the dialect from the token
  // stream at each Parse (line numbers => CLASSIC, none => MODERN). pdModern/pdClassic force
  // it explicitly and survive subsequent Parse calls — set these from NEW MODERN / NEW CLASSIC,
  // OPTION MODE MODERN / OPTION MODE CLASSIC; use SetDialect(pdAuto) after LOAD to re-detect.
  TParserDialect = (pdAuto, pdModern, pdClassic);

  // Dialect-pluggable statement handler: a per-dialect parser for a statement keyed by its leading
  // token type. Registered into the parser by the active dialect profile; consulted by ParseStatement
  // BEFORE the built-in dispatch. A handler may return nil to decline (then the built-in case runs).
  TStatementParseFunc = function: TASTNode of object;

  // A dialect profile bundles the parsing differences between CLASSIC (Commodore BASIC v7) and
  // MODERN (FreeBASIC-style). It is data: feature toggles read by ApplyDialectProfile, which (re)installs
  // the matching statement handlers (mechanism 3) and — when dialects diverge there — expression
  // parse-rules (mechanism 2). Keyword availability (mechanism 1) is handled upstream by the lexer's
  // per-keyword dialect tags. Add a toggle here when a new construct's parsing differs by dialect.
  TDialectProfile = record
    Modern: Boolean;           // MODERN (FreeBASIC) vs CLASSIC (v7)
    SwapIsStatement: Boolean;  // SWAP exchanges two lvalues (MODERN) vs C128 RAM-bank command (CLASSIC)
    MidIsStatement: Boolean;   // bare "MID(dst,start[,len]) = src" in-place overwrite (MODERN)
  end;

  { TPackratParser - Main BASIC Parser with Packrat memoization }
  TPackratParser = class(TPackratCore)
  private
    FExpressionParser: TExpressionParser;
    FStartTime: TDateTime;
    FOptions: TParserOptions;
    FValidationStacks: TParserValidationStacks;
    // Dialect: True when parsing in the FreeBASIC / MODERN dialect. Refreshed at each Parse:
    // from FDialectOverride when forced (pdModern/pdClassic), else auto-detected from the token
    // stream (no line numbers => MODERN); mirrors the SSA's SourceHasLineNumbers gate.
    FModernMode: Boolean;
    FDialectOverride: TParserDialect;  // pdAuto = detect per-Parse; pdModern/pdClassic = forced
    // Dialect-pluggable: per-token statement handlers installed by the active dialect profile.
    // Consulted by ParseStatement before the built-in case; nil entry = no override.
    FStmtHandlers: array[TTokenType] of TStatementParseFunc;
    FProfile: TDialectProfile;  // active dialect profile (derived from FModernMode)

    // Dialect profile application + the per-dialect statement handlers it installs.
    procedure ApplyDialectProfile;
    function MemSwapStatementHandler: TASTNode;   // MODERN: SWAP a,b ; declines for other mem commands
    function IdentMidStatementHandler: TASTNode;  // MODERN: MID(dst,..)=src ; declines for other idents
    function ProgEditModernHandler: TASTNode;     // MODERN: DELETE p ; declines for other prog-edit cmds
    function ParseDeleteStatement: TASTNode;      // MODERN: DELETE p → antDelete(child0=ptr expr)

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

    // Helper for error reporting
    function BuildSourceLine(AContext: TParserContext): string;

    // Helper: peek ahead for ELSE on the next line (for multi-line BEGIN/BEND support)
    function PeekForElseOnNextLine: Boolean;
    // Helper: pop completed IFs at EOL, with ELSE lookahead for block IFs
    procedure PopCompletedIfsAtEOL;

  protected
    procedure DoParsingStarted; virtual;
    procedure DoParsingFinished(Result: TParsingResult); virtual;

  public
    constructor Create;
    destructor Destroy; override;

    property Options: TParserOptions read GetOptions write SetOptions;

    // Dialect of the program being parsed: MODERN (FreeBASIC, no line numbers) vs
    // CLASSIC (BASIC v7, line-numbered). Used to disambiguate keywords that exist in
    // both dialects with different meaning (e.g. SWAP, MID).
    property ModernMode: Boolean read FModernMode;
    // Force the dialect (NEW MODERN/CLASSIC, OPTION MODE) or pdAuto to re-detect (after LOAD).
    // Takes effect on the next Parse; pdAuto also refreshes FModernMode immediately if a
    // token stream is already bound.
    procedure SetDialect(ADialect: TParserDialect);

    // === DIALECT-PLUGGABLE STATEMENT HANDLERS (mechanism 3) ===
    // Install/clear per-token statement parsers for the active dialect. A handler returning nil
    // (without committing) declines and the built-in dispatch runs instead.
    procedure RegisterStatementHandler(TokenType: TTokenType; Handler: TStatementParseFunc);
    procedure ClearStatementHandlers;

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
    function ParseGetStatement: TASTNode;
    function ParseGetkeyStatement: TASTNode;
    function ParsePudefStatement: TASTNode;
    function ParseCharStatement: TASTNode;
    function ParseIOStatement: TASTNode;
    function ParseLetStatement: TASTNode;
    function ParseIfStatement: TASTNode;
    function ParseThenStatement: TASTNode;
    function ParseElseStatement: TASTNode;
    // Collect statements of a block-IF THEN/ELSE body (multi-line, FreeBASIC/QB
    // style) into Parent, stopping at ELSE / ELSEIF / ENDIF / end-of-file.
    procedure ParseBlockIfBody(Parent: TASTNode);
    // Parse the ELSEIF*/ELSE? tail of a block IF into IfNode (ELSEIF lowers to a
    // nested IF inside an ELSE). Leaves the closing ENDIF for the caller.
    procedure ParseBlockElseChain(IfNode: TASTNode);
    // Block-IF terminator: ENDIF (one word) or END IF (two words, QuickBASIC).
    function AtBlockIfTerminator: Boolean;
    procedure ConsumeBlockIfTerminator;
    // SELECT CASE (FreeBASIC/QB), desugared to a nested IF/ELSEIF/ELSE chain whose
    // conditions clone the selector expression.
    function ParseSelectCase: TASTNode;
    function ParseCaseCondition(Selector: TASTNode): TASTNode;
    procedure ParseCaseBody(Parent: TASTNode);
    function AtEndSelect: Boolean;
    procedure ConsumeEndSelect;
    // SUB / FUNCTION declaration (FreeBASIC/QB). Body up to END SUB / END FUNCTION.
    function ParseProcedureDecl: TASTNode;
    procedure ParseProcedureBody(Parent: TASTNode);
    function AtEndProcedure: Boolean;
    // CALL name [ ( args ) ] : statement-level SUB invocation.
    function ParseCallStatement: TASTNode;
    // BASE [ ( args ) ] : explicit base-constructor call inside a child CONSTRUCTOR (M4.4f).
    function ParseBaseStatement: TASTNode;
    // THREADWAIT handle : join a worker thread by handle (M5.2 threading).
    function ParseThreadWaitStatement: TASTNode;
    // THREADDETACH handle : detach a worker thread by handle (M5.5).
    function ParseThreadDetachStatement: TASTNode;
    // MUTEXLOCK/MUTEXUNLOCK/MUTEXDESTROY handle : mutex ops (M5.4); node type keyed on the token.
    function ParseMutexOpStatement: TASTNode;
    // CONDWAIT cond, mutex : wait on a condition variable (M5.4).
    function ParseCondWaitStatement: TASTNode;
    // CONDSIGNAL/CONDBROADCAST/CONDDESTROY cond : single-handle cond ops (M5.4); node keyed on the token.
    function ParseCondOpStatement: TASTNode;
    // SHARED used as a standalone statement (not as the DIM SHARED modifier): not a -lang fb feature;
    // report a clean error pointing to DIM SHARED at module level, then recover.
    function ParseSharedError: TASTNode;
    // TYPE name / field AS type / ... / END TYPE : user-defined type (record/UDT).
    function ParseTypeDecl: TASTNode;
    function AtEndType: Boolean;
    procedure ConsumeEndType;
    // WITH obj / ... / END WITH : a leading '.field' resolves against obj. Parse-time desugar.
    function ParseWith: TASTNode;
    function AtEndWith: Boolean;
    // NAMESPACE name / ... / END NAMESPACE (FreeBASIC): group member decls under a name.
    function ParseNamespaceDecl: TASTNode;
    function AtEndNamespace: Boolean;
    // Read a dotted name "ident(.ident)*" (e.g. a namespace-qualified type "Forms.Point"); returns
    // the joined UPPER-cased name and consumes all segments. The first token must already be checked.
    function ParseDottedName: string;
    procedure ConsumeEndProcedure;
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
    function ParseEraseStatement: TASTNode;
    function ParseRedimStatement: TASTNode;
    function ParseSwapStatement: TASTNode;
    function ParseLRSetStatement(NodeType: TASTNodeType): TASTNode;
    function ParseMidStatement: TASTNode;
    function ParseEnumStatement: TASTNode;
    function ParseDefTypeStatement: TASTNode;
    function ParseDefStatement: TASTNode;
    function ParseFnStatement: TASTNode;
    function ParseConstStatement: TASTNode;
    function ParseDataStatement: TASTNode;
    function ParseReadStatement: TASTNode;
    function ParseRestoreStatement: TASTNode;
    function ParseClearStatement: TASTNode;
    function ParseStopStatement: TASTNode;
    function ParseKeyStatement: TASTNode;
    function ParseContStatement: TASTNode;
    function ParseRunStatement: TASTNode;
    function ParseClockStatement: TASTNode;
    function ParseSleepStatement: TASTNode;
    function ParseFrameStatement: TASTNode;
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
    function ParseLineInputStatement: TASTNode;  // FreeBASIC LINE INPUT #n, var (whole line)
    function ParseWriteFileStatement: TASTNode;   // FreeBASIC WRITE #n, exprlist (quoted CSV)
    function ParseSeekStatement: TASTNode;         // FreeBASIC SEEK #n, pos (set position)
    function ParseErrorHandlingStatement: TASTNode;
    function ParseDebugStatement: TASTNode;
    function ParseTracingStatement: TASTNode;
    function ParseMonitorStatement: TASTNode;
    function ParseSysStatement: TASTNode;
    function ParseUsrStatement: TASTNode;
    function ParseDirectiveStatement: TASTNode;
    {$IFDEF WEB_MODE}
    function ParseWebStatement: TASTNode;
    {$ENDIF}

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
  FDialectOverride := pdAuto;  // detect dialect from each program's tokens unless forced
  ClearStatementHandlers;      // no per-dialect statement overrides until a profile installs them

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

procedure TPackratParser.SetDialect(ADialect: TParserDialect);
begin
  FDialectOverride := ADialect;
  // Reflect the new dialect immediately so a query before the next Parse is correct.
  case ADialect of
    pdModern:  FModernMode := True;
    pdClassic: FModernMode := False;
  else
    if Assigned(Context) and Assigned(Context.TokenList) then
      FModernMode := not Context.TokenList.HasTokenType(ttLineNumber);
  end;
  ApplyDialectProfile;   // keep the installed handlers in sync with the forced/redetected dialect
end;

procedure TPackratParser.RegisterStatementHandler(TokenType: TTokenType; Handler: TStatementParseFunc);
begin
  FStmtHandlers[TokenType] := Handler;
end;

procedure TPackratParser.ClearStatementHandlers;
var
  tt: TTokenType;
begin
  for tt := Low(TTokenType) to High(TTokenType) do
    FStmtHandlers[tt] := nil;
end;

procedure TPackratParser.ApplyDialectProfile;
// Build the active profile from the resolved dialect (FModernMode) and (re)install the dialect's
// statement handlers. Idempotent: clears first, so it is safe to call again on a dialect switch
// (NEW MODERN/CLASSIC, OPTION MODE, LOAD re-detect). Mechanism-2 expression parse-rules would be
// (re)registered here too once a construct's expression syntax diverges by dialect (none today).
begin
  FProfile.Modern := FModernMode;
  FProfile.SwapIsStatement := FModernMode;
  FProfile.MidIsStatement := FModernMode;

  ClearStatementHandlers;
  if FProfile.SwapIsStatement then
    RegisterStatementHandler(ttMemoryCommand, @MemSwapStatementHandler);
  if FProfile.MidIsStatement then
    RegisterStatementHandler(ttIdentifier, @IdentMidStatementHandler);
  if FProfile.Modern then
    RegisterStatementHandler(ttProgramEditing, @ProgEditModernHandler);
end;

function TPackratParser.ProgEditModernHandler: TASTNode;
// MODERN override for ttProgramEditing: "DELETE p" frees a NEW'd object (FreeBASIC). Any other
// program-editing command (NEW/LIST/RENUMBER...) declines (nil) so the classic statement parser runs.
// (Bare "NEW T" is an expression, handled by the expression parser, not here.)
begin
  Result := nil;
  if UpperCase(Context.CurrentToken.Value) = 'DELETE' then
    Result := ParseDeleteStatement;
end;

function TPackratParser.ParseDeleteStatement: TASTNode;
// "DELETE p" — run the destructor of the pointee and free it. child0 = the pointer expression.
var
  PtrExpr: TASTNode;
begin
  Context.Advance;   // consume DELETE
  PtrExpr := FExpressionParser.ParseExpression(precCall);
  if not Assigned(PtrExpr) then
  begin
    HandleError('Expected a pointer after DELETE', Context.CurrentToken);
    Exit(nil);
  end;
  Result := TASTNode.Create(antDelete, Context.CurrentToken);
  Result.AddChild(PtrExpr);
end;

function TPackratParser.MemSwapStatementHandler: TASTNode;
// MODERN override for ttMemoryCommand: SWAP exchanges two lvalues. Any other memory command declines
// (returns nil) so the built-in ParseMemoryStatement handles it (POKE/BANK/...).
begin
  if UpperCase(Context.CurrentToken.Value) = 'SWAP' then
    Result := ParseSwapStatement
  else
    Result := nil;
end;

function TPackratParser.IdentMidStatementHandler: TASTNode;
// MODERN override for ttIdentifier: the in-place "MID(dst,start[,len]) = src" statement. Declines
// (nil) for any other identifier, and for MID(...) without a trailing '=' (ParseMidStatement returns
// nil), so the normal identifier path (label / assignment / call / expression) runs instead.
begin
  Result := nil;
  if (UpperCase(Context.CurrentToken.Value) = 'MID') and
     Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttDelimParOpen) then
    Result := ParseMidStatement;
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

    // Resolve the dialect for this parse: honor an explicit override (NEW MODERN/CLASSIC,
    // OPTION MODE), otherwise auto-detect — a program with no line-number tokens is MODERN.
    case FDialectOverride of
      pdModern:  FModernMode := True;
      pdClassic: FModernMode := False;
    else
      FModernMode := Assigned(TokenList) and not TokenList.HasTokenType(ttLineNumber);
    end;
    ApplyDialectProfile;   // install the dialect's statement handlers for this parse

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
     // Pop completed IFs (with ELSE lookahead for multi-line BEGIN/BEND blocks)
     PopCompletedIfsAtEOL;
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

     // Track BASIC line number for error reporting
     Context.SetCurrentBasicLine(LineNum, BuildSourceLine(Context));

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

 // Dialect-pluggable (mechanism 3): a per-dialect statement handler for this token type takes
 // priority over the built-in dispatch. It may decline by returning nil without committing, in
 // which case we restore the cursor and fall through to the case below.
 if Assigned(FStmtHandlers[Token.TokenType]) then
 begin
   SavedIndex := Context.CurrentIndex;
   Result := FStmtHandlers[Token.TokenType]();
   if Assigned(Result) then Exit;
   Context.CurrentIndex := SavedIndex;
 end;

 // Route to appropriate statement parser based on keyword
 case Token.TokenType of
    // === I/O COMMANDS ===
    ttOutputCommand:
    begin
      // Dispatch based on specific keyword
      case UpperCase(Token.Value) of
        kPRINT: Result := Memoize('PrintStatement', @ParsePrintStatement);
        kCHAR: Result := Memoize('CharStatement', @ParseCharStatement);
        kPUDEF: Result := Memoize('PudefStatement', @ParsePudefStatement);
        kUSING: Result := Memoize('PrintStatement', @ParsePrintStatement); // USING alone - handle in PRINT
        kWINDOW: Result := Memoize('GraphicsStatement', @ParseGraphicsStatement);
      else
        Result := Memoize('PrintStatement', @ParsePrintStatement);
      end;
    end;
    ttInputCommand:
    begin
      // Dispatch based on specific keyword
      case UpperCase(Token.Value) of
        kINPUT: Result := Memoize('InputStatement', @ParseInputStatement);
        kGET: Result := Memoize('GetStatement', @ParseGetStatement);
        kGETKEY: Result := Memoize('GetkeyStatement', @ParseGetkeyStatement);
      else
        Result := Memoize('InputStatement', @ParseInputStatement);
      end;
    end;
    ttIOCommand: Result := Memoize('IOStatement', @ParseIOStatement);

    // === DATA HANDLING ===
    ttDataAssignment: Result := Memoize('LetStatement', @ParseLetStatement);
    ttDataDeclaration: Result := Memoize('DimStatement', @ParseDimStatement);
    ttArrayErase: Result := Memoize('EraseStatement', @ParseEraseStatement);
    ttLSet: Result := ParseLRSetStatement(antLSet);
    ttRSet: Result := ParseLRSetStatement(antRSet);
    ttArrayRedim: Result := Memoize('RedimStatement', @ParseRedimStatement);
    ttEnum: Result := Memoize('EnumStatement', @ParseEnumStatement);
    ttDefType: Result := Memoize('DefTypeStatement', @ParseDefTypeStatement);
    ttConstant: Result := Memoize('ConstStatement', @ParseConstStatement);
    ttDataConstant: Result := Memoize('DataStatement', @ParseDataStatement);
    ttDataRead: Result := Memoize('ReadStatement', @ParseReadStatement);
    ttDataClear: Result := Memoize('ClearStatement', @ParseClearStatement);

    // === FLOW CONTROL ===
    ttConditionalIf: Result := Memoize('IfStatement', @ParseIfStatement);
    ttConditionalThen: Result := Memoize('ThenStatement', @ParseThenStatement);
    ttConditionalElse: Result := Memoize('ElseStatement', @ParseElseStatement);
    ttSelectCase: Result := Memoize('SelectCase', @ParseSelectCase);
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
    ttProgramFrame: Result := Memoize('FrameStatement', @ParseFrameStatement);
    ttProgramWait: Result := Memoize('WaitStatement', @ParseWaitStatement);
    ttProgramEditing: Result := Memoize('ProgramEditingStatement', @ParseProgramEditingStatement);

    // === BLOCK CONSTRUCTS ===
    ttBlockBegin: Result := Memoize('BlockStatement', @ParseBlockStatement);
    ttBlockEnd: Result := Memoize('BlockEndStatement', @ParseBlockEndStatement);

    // === COMMENTS ===
    ttCommentRemark: Result := Memoize('RemStatement', @ParseRemStatement);

    // === PROCEDURES ===
    ttProcedureDefine: Result := Memoize('DefStatement', @ParseDefStatement);
    ttProcedureStart:
      if (UpperCase(Token.Value) = kSUB) or (UpperCase(Token.Value) = kFUNCTION) or
         (UpperCase(Token.Value) = kCONSTRUCTOR) or (UpperCase(Token.Value) = kDESTRUCTOR) or
         (UpperCase(Token.Value) = kPROPERTY) or (UpperCase(Token.Value) = kOPERATOR) then
        Result := Memoize('ProcedureDecl', @ParseProcedureDecl)
      else
        Result := Memoize('FnStatement', @ParseFnStatement);
    ttCallSub: Result := Memoize('CallStatement', @ParseCallStatement);
    ttBaseCall: Result := Memoize('BaseStatement', @ParseBaseStatement);
    ttThreadWait: Result := Memoize('ThreadWaitStatement', @ParseThreadWaitStatement);
    ttThreadDetach: Result := Memoize('ThreadDetachStatement', @ParseThreadDetachStatement);
    ttMutexLock, ttMutexUnlock, ttMutexDestroy:
      Result := Memoize('MutexOpStatement', @ParseMutexOpStatement);
    ttCondWait: Result := Memoize('CondWaitStatement', @ParseCondWaitStatement);
    ttCondSignal, ttCondBroadcast, ttCondDestroy:
      Result := Memoize('CondOpStatement', @ParseCondOpStatement);
    ttSharedDecl: Result := ParseSharedError;   // SHARED is only the DIM SHARED modifier, not a statement
    ttTypeDecl: Result := Memoize('TypeDecl', @ParseTypeDecl);
    ttWithBlock: Result := ParseWith;
    ttNamespaceBlock: Result := Memoize('NamespaceDecl', @ParseNamespaceDecl);

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

    // === BITWISE OPERATORS ===
    ttBitwiseAND,
    ttBitwiseOR,
    ttBitwiseXOR,
    ttBitwiseNOT: Result := Memoize('ExpressionStatement', @ParseExpressionStatement);

    // === IDENTIFIERS ===
    ttIdentifier:
      begin
        // Named label "name:" (FreeBASIC/QB): an identifier immediately followed by
        // ':' defines a GOTO/GOSUB target. (Assignments are "name = ...", calls are
        // "name(...)"/"name arg", so the ':' is unambiguous here.)
        if Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttSeparStmt) then
        begin
          Result := TASTNode.CreateWithValue(antLabel, Token.Value, Token);
          Context.Advance;   // consume the identifier
          Context.Advance;   // consume ':'
          DoNodeCreated(Result);
        end
        // FreeBASIC "LINE INPUT #n, var": LINE is not a registered keyword here (it is a bare
        // identifier), so detect the two-word form. Unambiguous — no statement has `line input`
        // meaning anything else.
        else if (UpperCase(Token.Value) = 'LINE') and Assigned(Context.PeekNext) and
                (UpperCase(Context.PeekNext.Value) = 'INPUT') then
          Result := ParseLineInputStatement
        // FreeBASIC "WRITE #n, ...": comma-separated, quoted-string CSV output (WRITE is a bare
        // identifier here; the `#` after it disambiguates from an assignment to a var named `write`).
        else if (UpperCase(Token.Value) = 'WRITE') and Assigned(Context.PeekNext) and
                ((Context.PeekNext.TokenType = ttFileHandlePrefix) or (Context.PeekNext.Value = '#')) then
          Result := ParseWriteFileStatement
        // FreeBASIC "SEEK #n, pos" statement (SEEK is also the SEEK(n) function — the `#` selects the
        // statement form). SEEK is a bare identifier here.
        else if (UpperCase(Token.Value) = 'SEEK') and Assigned(Context.PeekNext) and
                ((Context.PeekNext.TokenType = ttFileHandlePrefix) or (Context.PeekNext.Value = '#')) then
          Result := ParseSeekStatement
        // Note: the FreeBASIC in-place "MID(dst,start[,len]) = src" statement (MODERN) is intercepted
        // earlier by the dialect profile's IdentMidStatementHandler (mechanism 3), so it does not need
        // a branch here; in CLASSIC bare MID is a plain identifier handled by the default path below.
        else
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
      end;

    // === WITH leading-dot member (".field = ..." inside a WITH block, M3.2) ===
    ttOpDot:
      begin
        SavedIndex := Context.CurrentIndex;
        Result := Memoize('AssignmentStatement', @ParseAssignmentStatement);
        if not Assigned(Result) then
        begin
          Context.CurrentIndex := SavedIndex;
          Result := Memoize('ExpressionStatement', @ParseExpressionStatement);
        end;
      end;

    // === FreeBASIC pointer-deref assignment ("*p = expr") ===
    ttOpMul:
      begin
        SavedIndex := Context.CurrentIndex;
        Result := Memoize('AssignmentStatement', @ParseAssignmentStatement);
        if not Assigned(Result) then
        begin
          Context.CurrentIndex := SavedIndex;
          Result := Memoize('ExpressionStatement', @ParseExpressionStatement);
        end;
      end;

    // === SPECIAL VARIABLES (TI$, etc.) ===
    ttSpecialVariable:
      begin
        // Special variables like TI$ can be assigned (TI$="120000")
        SavedIndex := Context.CurrentIndex;

        Result := Memoize('AssignmentStatement', @ParseAssignmentStatement);

        if not Assigned(Result) then
        begin
          // Assignment fallito, riprova come expression (e.g., PRINT TI$)
          Context.CurrentIndex := SavedIndex;
          Result := Memoize('ExpressionStatement', @ParseExpressionStatement);
        end;
      end;

    {$IFDEF WEB_MODE}
    // === WEB COMMANDS ===
    ttWebCommand: Result := Memoize('WebStatement', @ParseWebStatement);
    {$ENDIF}

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
 LhsIsExpr: Boolean;   // LHS built by the expression parser (member/array): may be a call stmt
 OpSym: string;        // compound-assignment operator symbol ('+','-','*','/','^')
 OpType: TTokenType;   // its arithmetic binary-op token type
begin
 Token := Context.CurrentToken;
 SavedToken := Token;   // default (member/array LHS branches don't set it; avoids nil on error)
 LhsIsExpr := False;

  // Parse left side - can be A or A(i) or special variable (TI$)
  if Context.Check(ttOpDot) then
  begin
    // Leading '.field = ...' inside a WITH block (M3.2): the expression parser's prefix rule
    // resolves it against the current WITH object.
    LeftSide := FExpressionParser.ParseExpression(precCall);
    LhsIsExpr := True;
  end
  else if Context.Check(ttOpMul) then
  begin
    // FreeBASIC pointer-deref assignment "*p = expr": the expression parser's '*' prefix rule builds
    // the antDeref target (it stops before '=', lower precedence).
    LeftSide := FExpressionParser.ParseExpression(precCall);
    LhsIsExpr := True;
  end
  else if Context.Check(ttIdentifier) and Assigned(Context.PeekNext) and
    ((Context.PeekNext.TokenType = ttDelimParOpen) or
     (Context.PeekNext.TokenType = ttDelimBrackOpen) or
     (Context.PeekNext.TokenType = ttOpDot)) then
  begin
    // Array access A(i), pointer index p[i], or member access rec.field - use the expression parser to
    // build the full target (antArrayAccess / antMemberAccess); it stops before '=' (lower prec).
    LeftSide := FExpressionParser.ParseExpression(precCall);
    LhsIsExpr := True;
  end
  else if Context.Check(ttIdentifier) then
  begin
    // *** SAVE THE TOKEN BEFORE ADVANCING ***
    SavedToken := Context.CurrentToken;
    LeftSide := TASTNode.CreateWithValue(antIdentifier, UpperCase(Token.Value), Token);
    Context.Advance; // Consume identifier
    //WriteLn('DEBUG: Consumed identifier, next token: "', Context.CurrentToken.Value, '"');
  end
  else if Context.Check(ttSpecialVariable) then
  begin
    // Special variable like TI$ - can be assigned
    SavedToken := Context.CurrentToken;
    LeftSide := TASTNode.CreateWithValue(antSpecialVariable, UpperCase(Token.Value), Token);
    Context.Advance; // Consume special variable
  end
  else
  begin
    HandleError('Expected variable name in assignment', Context.CurrentToken);  // ← Token corrente
    Result := nil;
    Exit;
  end;

  // FreeBASIC compound assignment "lhs op= rhs" desugars to "lhs = lhs op rhs". The lexer emits a single
  // ttCompoundAssign token whose value is the operator symbol; clone the LHS as the left operand.
  if Context.Check(ttCompoundAssign) then
  begin
    OpSym := Context.CurrentToken.Value;
    Context.Advance;                                 // consume the "op=" token
    Expression := FExpressionParser.ParseExpression;
    if not Assigned(Expression) then
    begin
      if Assigned(LeftSide) then LeftSide.Free;
      Result := nil;
      Exit;
    end;
    case OpSym of
      '-': OpType := ttOpSub;
      '*': OpType := ttOpMul;
      '/': OpType := ttOpDiv;
      '\': OpType := ttOpIntDiv;   // FreeBASIC integer-division compound "\="
      '&': OpType := ttOpConcat;   // FreeBASIC string-concat compound "&="
      '^': OpType := ttOpPow;
    else
      OpType := ttOpAdd;
    end;
    Expression := CreateBinaryOpNode(OpType, LeftSide.Clone, Expression,
                                     TLexerToken.CreateSimple(OpType, OpSym));
    Result := TASTNode.Create(antAssignment);
    Result.AddChild(LeftSide);
    Result.AddChild(Expression);
    DoNodeCreated(Result);
    Exit;
  end;

  // Expect =
  //WriteLn('DEBUG: Looking for =, current token: "', Context.CurrentToken.Value, '" type=', Ord(Context.CurrentToken.TokenType));
  if not Context.Match(ttOpEq) then
  begin
    // No '=': if the LHS came from the expression parser (member/array access) it is not an
    // assignment but a call/expression statement (e.g. obj.method(args)) — return nil quietly
    // so the caller falls back to an expression statement, without recording a syntax error.
    if LhsIsExpr then
    begin
      if Assigned(LeftSide) then LeftSide.Free;
      Result := nil;
      Exit;
    end;
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
  Expr, FormatNode: TASTNode;
  SeparatorNode: TASTNode;
  IsUsingFormat: Boolean;
begin
  Token := Context.CurrentToken;
  IsUsingFormat := False;

  // Check if this is PRINT USING
  if UpperCase(Token.Value) = kUSING then
  begin
    // Standalone USING - create PRINT USING node
    Result := TASTNode.Create(antPrintUsing, Token);
    Context.Advance; // Consume USING
    IsUsingFormat := True;
  end
  else
  begin
    Result := TASTNode.Create(antPrint, Token);
    Context.Advance; // Consume PRINT

    // FreeBASIC file output: "PRINT #n, exprlist" -> antPrintFile (handle = child 0). The shared
    // print-list loop below appends the expressions/separators (same shape as PRINT#).
    if Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#') then
    begin
      Result.Free;
      Result := TASTNode.Create(antPrintFile, Token);
      Context.Advance;  // consume '#'
      if Context.Check(ttNumber) or Context.Check(ttInteger) then
      begin
        Result.AddChild(TASTNode.CreateWithValue(antLiteral, StrToInt(Context.CurrentToken.Value), Context.CurrentToken));
        Context.Advance;
      end
      else if Context.Check(ttIdentifier) then
      begin
        Result.AddChild(TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken));
        Context.Advance;
      end
      else
        HandleError('Expected file number after PRINT #', Token);
      if Context.CheckAny([ttSeparParam, ttSeparOutput]) then
        Context.Advance;   // the comma/semicolon after the file number
    end
    else
    // Check for USING keyword after PRINT
    if Context.Check(ttOutputCommand) and (UpperCase(Context.CurrentToken.Value) = kUSING) then
    begin
      Result.Free;
      Result := TASTNode.Create(antPrintUsing, Token);
      Context.Advance; // Consume USING
      IsUsingFormat := True;
    end;
  end;

  // For PRINT USING, first parameter is format string
  if IsUsingFormat then
  begin
    // Parse format string
    FormatNode := ParseExpression;
    if Assigned(FormatNode) then
      Result.AddChild(FormatNode)
    else
    begin
      HandleError('Expected format string after USING', Token);
      Exit;
    end;

    // Expect semicolon separator before values
    if Context.Check(ttSeparOutput) then
      Context.Advance  // Consume semicolon
    else if Context.Check(ttSeparParam) then
      Context.Advance; // Also accept comma
  end;

  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
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
      if Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
        Break;
    end
    else
    begin
      // Commodore BASIC implicit semicolon: PRINT "text"expr = PRINT "text";expr
      // If not at end of statement, insert implicit semicolon and continue
      if Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
        Break;
      SeparatorNode := TASTNode.CreateWithValue(antSeparator, ';', Context.CurrentToken);
      Result.AddChild(SeparatorNode);
    end;
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

  // FreeBASIC file input: "INPUT #n, varlist" -> antInputFile (handle = child 0, then variables).
  if Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#') then
  begin
    Result.Free;
    Result := TASTNode.Create(antInputFile, Token);
    Context.Advance;  // consume '#'
    if Context.Check(ttNumber) or Context.Check(ttInteger) then
    begin
      Result.AddChild(TASTNode.CreateWithValue(antLiteral, StrToInt(Context.CurrentToken.Value), Context.CurrentToken));
      Context.Advance;
    end
    else if Context.Check(ttIdentifier) then
    begin
      Result.AddChild(TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken));
      Context.Advance;
    end
    else
      HandleError('Expected file number after INPUT #', Token);
    if Context.CheckAny([ttSeparParam, ttSeparOutput]) then Context.Advance;  // comma after handle
    while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
    begin
      Expr := ParseExpression;     // a destination variable (identifier or array element)
      if Assigned(Expr) then Result.AddChild(Expr) else Break;
      if Context.Check(ttSeparParam) then Context.Advance else Break;
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
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
      if Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
        Break;
    end
    else
      Break;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseGetStatement: TASTNode;
var
  Token: TLexerToken;
  VarNode: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antGet, Token);
  Context.Advance; // Consume GET

  // GET requires a single string variable
  // Format: GET A$
  if Context.Check(ttIdentifier) then
  begin
    VarNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
    Result.AddChild(VarNode);
    Context.Advance;
  end
  else
    HandleError('Expected variable after GET', Token);

  DoNodeCreated(Result);
end;

function TPackratParser.ParseGetkeyStatement: TASTNode;
var
  Token: TLexerToken;
  VarNode: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antGetkey, Token);
  Context.Advance; // Consume GETKEY

  // GETKEY requires a single string variable
  // Format: GETKEY A$
  if Context.Check(ttIdentifier) then
  begin
    VarNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
    Result.AddChild(VarNode);
    Context.Advance;
  end
  else
    HandleError('Expected variable after GETKEY', Token);

  DoNodeCreated(Result);
end;

function TPackratParser.ParsePudefStatement: TASTNode;
var
  Token: TLexerToken;
  FormatNode: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antPudef, Token);
  Context.Advance; // Consume PUDEF

  // PUDEF requires a string with 4 character positions
  // Format: PUDEF " ,.$" where positions are: filler, comma, decimal, dollar
  if Context.Check(ttStringLiteral) then
  begin
    FormatNode := TASTNode.CreateWithValue(antLiteral, Context.CurrentToken.Value, Context.CurrentToken);
    Result.AddChild(FormatNode);
    Context.Advance;
  end
  else
    HandleError('Expected format string after PUDEF', Token);

  DoNodeCreated(Result);
end;

function TPackratParser.ParseCharStatement: TASTNode;
var
  Token: TLexerToken;
  Expr: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antChar, Token);
  Context.Advance; // Consume CHAR

  // CHAR mode, col, row, "text" [,reverse]
  // Parse comma-separated parameters
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
  begin
    Expr := ParseExpression;
    if Assigned(Expr) then
      Result.AddChild(Expr)
    else
      Break;

    if Context.Check(ttSeparParam) then
      Context.Advance
    else
      Break;
  end;

  if Result.ChildCount < 4 then
    HandleError('CHAR requires at least 4 parameters: mode, col, row, text', Token);

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

 // *** FreeBASIC/QuickBASIC block IF: "IF cond THEN" with nothing after THEN (end of
 //     line) opens a multi-line block, closed by ELSE / ENDIF. Parsed self-contained
 //     here (so nesting just recurses); the IF is popped when ENDIF is consumed. ***
 if Context.CheckAny([ttEndOfLine]) then
 begin
   ParseBlockIfBody(ThenNode);               // THEN body, up to ELSE/ELSEIF/ENDIF/EOF
   ParseBlockElseChain(CurrentIf.IfNode);    // ELSEIF* / ELSE? tail
   ConsumeBlockIfTerminator;                 // consume ENDIF or END IF
   if FValidationStacks.HasActiveIf then
     FValidationStacks.PopIf;                // the block IF is now closed
   DoNodeCreated(ThenNode);
   Result := nil;
   Exit;
 end;

 // *** Check if the first statement is BEGIN ***
 HasBeginBlock := Context.Check(ttBlockBegin);
 if HasBeginBlock then
   FValidationStacks.SetThenBlockForCurrentIf;

 // *** Parse THEN statements until ELSE / : ELSE or EOL ***
 while not Context.CheckAny([ttEndOfLine, ttEndOfFile]) do
 begin
   // *** ELSE ends the THEN clause. Accept it both directly ("THEN x ELSE y")
   //     and after a separator ("THEN x : ELSE y") — BASIC v7 wrote the colon,
   //     but we tolerate either form. ***
   if Context.Check(ttConditionalElse) then
     Break;

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

procedure TPackratParser.ParseBlockIfBody(Parent: TASTNode);
var
  Statement: TASTNode;
  PrevIdx: Integer;
begin
  // Collect statements across lines into Parent until ELSE / ENDIF (ttBlockEnd) /
  // end-of-file. A nested block IF is parsed by ParseStatement -> ParseThenStatement,
  // which consumes its own ENDIF, so only this body's own ELSE/ENDIF stops us here.
  while not Context.Check(ttEndOfFile) do
  begin
    if Context.Match(ttEndOfLine) then Continue;       // skip line breaks
    if Context.Check(ttSeparStmt) then
    begin
      Context.Advance;                                  // skip ':' separators
      Continue;
    end;
    if Context.Check(ttConditionalElse) or AtBlockIfTerminator then
      Break;                                            // ELSE / ENDIF / END IF ends this body
    PrevIdx := Context.CurrentIndex;
    Statement := ParseStatement;
    // NB: many statement handlers (THEN/ELSE) return nil on SUCCESS (they attach to
    // their parent). So nil is not "stop" — only stop if no progress was made.
    if Assigned(Statement) then
      Parent.AddChild(Statement)
    else if Context.CurrentIndex = PrevIdx then
      Break;                                            // no node and no progress
  end;
end;

procedure TPackratParser.ParseBlockElseChain(IfNode: TASTNode);
var
  Tok: TLexerToken;
  ElseNode, NestedIf, NestedThen, Cond: TASTNode;
begin
  // Current token is ELSE / ELSEIF (both ttConditionalElse) or ENDIF/EOF (nothing
  // to do). ELSEIF lowers to:  ELSE { IF cond THEN <body> <further chain> }, so a
  // single trailing ENDIF closes the whole chain (consumed by the caller).
  if not Context.Check(ttConditionalElse) then Exit;
  Tok := Context.CurrentToken;

  if UpperCase(Tok.Value) = kELSEIF then
  begin
    Context.Advance;                                   // consume ELSEIF
    ElseNode := TASTNode.Create(antElse, Tok);
    if Assigned(IfNode) then IfNode.AddChild(ElseNode);
    NestedIf := TASTNode.Create(antIf, Tok);
    ElseNode.AddChild(NestedIf);
    Cond := ParseExpression;                           // the ELSEIF condition
    if Assigned(Cond) then NestedIf.AddChild(Cond);
    if Context.Check(ttConditionalThen) then
      Context.Advance;                                 // consume THEN
    NestedThen := TASTNode.Create(antThen, Tok);
    NestedIf.AddChild(NestedThen);
    ParseBlockIfBody(NestedThen);                      // body up to ELSE/ELSEIF/ENDIF
    ParseBlockElseChain(NestedIf);                     // recurse for further ELSEIF/ELSE
    DoNodeCreated(NestedIf);
    DoNodeCreated(ElseNode);
  end
  else
  begin
    Context.Advance;                                   // consume plain ELSE
    ElseNode := TASTNode.Create(antElse, Tok);
    if Assigned(IfNode) then IfNode.AddChild(ElseNode);
    ParseBlockIfBody(ElseNode);                        // ELSE body up to ENDIF
    DoNodeCreated(ElseNode);
  end;
end;

function TPackratParser.AtBlockIfTerminator: Boolean;
begin
  // ENDIF (one word) or the QuickBASIC two-word "END IF" (END immediately followed
  // by IF). A bare END (end-of-program) is NOT a terminator.
  Result := Context.Check(ttBlockEnd) or
    (Context.Check(ttProgramEnd) and Assigned(Context.PeekNext) and
     (Context.PeekNext.TokenType = ttConditionalIf));
end;

procedure TPackratParser.ConsumeBlockIfTerminator;
begin
  if Context.Check(ttBlockEnd) then
    Context.Advance                                    // ENDIF
  else if Context.Check(ttProgramEnd) and Assigned(Context.PeekNext) and
          (Context.PeekNext.TokenType = ttConditionalIf) then
  begin
    Context.Advance;                                   // END
    Context.Advance;                                   // IF
  end;
end;

function TPackratParser.AtEndSelect: Boolean;
begin
  // END SELECT (two words). END is ttProgramEnd, SELECT is ttSelectCase.
  Result := Context.Check(ttProgramEnd) and Assigned(Context.PeekNext) and
            (Context.PeekNext.TokenType = ttSelectCase);
end;

procedure TPackratParser.ConsumeEndSelect;
begin
  if AtEndSelect then
  begin
    Context.Advance;   // END
    Context.Advance;   // SELECT
  end;
end;

function TPackratParser.AtEndProcedure: Boolean;
begin
  // END SUB / END FUNCTION (END is ttProgramEnd, SUB/FUNCTION is ttProcedureStart).
  Result := Context.Check(ttProgramEnd) and Assigned(Context.PeekNext) and
            (Context.PeekNext.TokenType = ttProcedureStart);
end;

procedure TPackratParser.ConsumeEndProcedure;
begin
  if AtEndProcedure then
  begin
    Context.Advance;   // END
    Context.Advance;   // SUB / FUNCTION
  end;
end;

procedure TPackratParser.ParseProcedureBody(Parent: TASTNode);
var
  Statement: TASTNode;
  PrevIdx: Integer;
begin
  while not Context.Check(ttEndOfFile) do
  begin
    if Context.Match(ttEndOfLine) then Continue;
    if Context.Check(ttSeparStmt) then begin Context.Advance; Continue; end;
    if AtEndProcedure then Break;
    PrevIdx := Context.CurrentIndex;
    Statement := ParseStatement;
    if Assigned(Statement) then
      Parent.AddChild(Statement)
    else if Context.CurrentIndex = PrevIdx then
      Break;
  end;
end;

function TPackratParser.ParseProcedureDecl: TASTNode;
var
  Token, NameTok, RetTok: TLexerToken;
  Kind, MethodType, QualName, ParamMode, OpSym, OpOwnerType: string;
  NameNode, ParamList, ParamNode, ThisNode, DefExpr: TASTNode;
begin
  // SUB|FUNCTION name [ ( params ) ] [AS type] <body> END SUB|FUNCTION
  // Method form (M4.1): SUB|FUNCTION Type.method(...) — qualified name "TYPE.METHOD" with an
  // implicit first parameter THIS AS Type (the instance handle).
  Token := Context.CurrentToken;
  Kind := UpperCase(Token.Value);                 // 'SUB' or 'FUNCTION'
  Context.Advance;                                // consume SUB / FUNCTION
  Result := TASTNode.CreateWithValue(antProcedureDecl, Kind, Token);

  MethodType := '';
  OpSym := '';
  if Kind = kOPERATOR then
  begin
    // OPERATOR <sym> (a AS T, b AS T) AS R — capture the operator symbol token; the owning type is
    // derived from the first parameter after the list is parsed (label "<T>.OPERATOR<sym>"). No
    // implicit THIS: it is a 2-argument global function resolved by operand type at the binary op.
    OpSym := Context.CurrentToken.Value;
    Context.Advance;                                // consume the operator symbol
    NameNode := TASTNode.CreateWithValue(antIdentifier, 'OPERATOR' + OpSym, Token);  // placeholder
    Result.AddChild(NameNode);
  end
  else
  begin
  // The procedure/owner name must be a plain identifier. A reserved word here (e.g. a graphics
  // keyword such as CIRCLE/BOX/LINE used as a type name) is malformed: report a clean error and
  // skip the body up to its END, so the parser terminates instead of derailing on a misaligned
  // token stream. (The method name *after* the dot may be a reserved word — handled below.)
  if not Context.Check(ttIdentifier) then
  begin
    HandleError(Format('Expected a name after %s, but found the reserved word "%s"',
                       [Kind, Context.CurrentToken.Value]), Context.CurrentToken);
    while not Context.Check(ttEndOfFile) do
    begin
      if AtEndProcedure then begin ConsumeEndProcedure; Break; end;
      Context.Advance;
    end;
    Exit;
  end;
  if Context.Check(ttIdentifier) then
  begin
    NameTok := Context.CurrentToken;
    QualName := UpperCase(NameTok.Value);
    Context.Advance;
    if (Kind = kCONSTRUCTOR) or (Kind = kDESTRUCTOR) then
    begin
      // CONSTRUCTOR/DESTRUCTOR Type(...) — the identifier is the owner type; the method is
      // "Type.CONSTRUCTOR" / "Type.DESTRUCTOR" with an implicit THIS AS Type. Auto-called at
      // instance allocation (M4.4) / scope exit (V5).
      MethodType := QualName;
      QualName := MethodType + '.' + Kind;
    end
    else if Context.Check(ttOpDot) then
    begin
      // Type.method — a method of an existing TYPE. The method name may be a reserved word
      // (e.g. SCALE, LEN), so accept any alphabetic token here.
      Context.Advance;                            // consume '.'
      if Context.Check(ttIdentifier) or
         ((Length(Context.CurrentToken.Value) > 0) and
          (UpCase(Context.CurrentToken.Value[1]) in ['A'..'Z', '_'])) then
      begin
        MethodType := QualName;
        QualName := MethodType + '.' + UpperCase(Context.CurrentToken.Value);
        Context.Advance;                          // method name
      end;
    end;
    NameNode := TASTNode.CreateWithValue(antIdentifier, QualName, NameTok);
    Result.AddChild(NameNode);
  end;
  end;   // end of non-OPERATOR name parsing

  ParamList := TASTNode.Create(antParameterList, Token);
  // Implicit THIS parameter for methods: THIS AS <Type> (record handle), first in the list.
  if MethodType <> '' then
  begin
    ThisNode := TASTNode.CreateWithValue(antIdentifier, 'THIS', Token);
    ThisNode.AddChild(TASTNode.CreateWithValue(antIdentifier, MethodType, Token));
    ParamList.AddChild(ThisNode);
  end;
  if Context.Check(ttDelimParOpen) then
  begin
    Context.Advance;                              // (
    while (not Context.Check(ttDelimParClose)) and (not Context.Check(ttEndOfFile)) and
          (not Context.Check(ttEndOfLine)) do
    begin
      // Optional passing convention (V4): BYVAL (copy) or BYREF (alias, the default) before the
      // parameter name. Recorded on the param node as the 'BYVAL' attribute for the SSA prologue.
      ParamMode := '';
      if Context.Check(ttParamMode) then
      begin
        ParamMode := UpperCase(Context.CurrentToken.Value);
        Context.Advance;
      end;
      if Context.Check(ttIdentifier) then
      begin
        ParamNode := TASTNode.CreateWithValue(antIdentifier,
                       UpperCase(Context.CurrentToken.Value), Context.CurrentToken);
        if ParamMode = kBYVAL then ParamNode.Attributes.Values['BYVAL'] := '1';
        // An explicit BYREF on a scalar parameter requests write-back (the callee's mutations are
        // copied back into the caller's variable argument). Recorded for the SSA call lowering; BYREF
        // is also the implicit default, but only an explicit BYREF opts a scalar into write-back.
        if ParamMode = kBYREF then ParamNode.Attributes.Values['BYREF'] := '1';
        Context.Advance;
        // Optional "AS typename" (M3.1): attach the type as a child antIdentifier so the
        // SSA pre-scan can type the parameter (record handle / explicit builtin bank).
        if Context.Check(ttAsType) then
        begin
          Context.Advance;                        // AS
          if Context.Check(ttIdentifier) then
          begin
            RetTok := Context.CurrentToken;
            ParamNode.AddChild(TASTNode.CreateWithValue(antIdentifier,
                         ParseDottedName, RetTok));  // dotted: namespace-qualified param type
          end;
        end;
        // Optional default value "= expr" (M7): a call that omits this trailing argument has the
        // default staged in its place. Marked with 'HASDEFAULT'; the default expression is the
        // parameter node's last child (after the optional type child).
        if Context.Check(ttOpEq) then
        begin
          Context.Advance;                        // =
          DefExpr := FExpressionParser.ParseExpression;
          if Assigned(DefExpr) then
          begin
            ParamNode.Attributes.Values['HASDEFAULT'] := '1';
            ParamNode.AddChild(DefExpr);          // last child = default-value expression
          end;
        end;
        ParamList.AddChild(ParamNode);
      end
      else
        Context.Advance;                          // skip unexpected token (defensive)
      if Context.Check(ttSeparParam) then
        Context.Advance;                          // ,
    end;
    if Context.Check(ttDelimParClose) then
      Context.Advance;                            // )
  end;

  // PROPERTY (FreeBASIC OOP) desugars to a method: a getter "PROPERTY T.p() AS RT" becomes
  // FUNCTION T.p (read via obj.p), a setter "PROPERTY T.p(v AS VT)" becomes SUB T.p.SET (write via
  // obj.p = v). Decided by explicit param count (THIS excluded). After this, the FUNCTION/SUB
  // machinery (return type, body, END) applies unchanged; END PROPERTY is accepted generically.
  if (Kind = kPROPERTY) and Assigned(NameNode) then
  begin
    if (ParamList.ChildCount - 1) >= 1 then       // >=1 explicit param (THIS at index 0) => setter
    begin
      Kind := kSUB;
      Result.Value := kSUB;
      NameNode.Value := QualName + '.SET';
    end
    else
    begin
      Kind := kFUNCTION;                           // getter returns the property value
      Result.Value := kFUNCTION;
    end;
  end;

  // OPERATOR: now that the parameters are parsed, take the owning type from the first parameter's
  // AS-type and form the label "<T>.OPERATOR<sym>", then treat it as a normal FUNCTION. The binary-op
  // lowering resolves it by the left operand's type.
  if (Kind = kOPERATOR) and Assigned(NameNode) and (ParamList.ChildCount >= 1) and
     (ParamList.GetChild(0).ChildCount >= 1) then
  begin
    OpOwnerType := UpperCase(VarToStr(ParamList.GetChild(0).GetChild(0).Value));
    NameNode.Value := OpOwnerType + '.OPERATOR' + OpSym;
    Kind := kFUNCTION;
    Result.Value := kFUNCTION;
  end;

  // FreeBASIC BYREF function result: "FUNCTION name(...) BYREF AS rettype" returns a reference (the
  // SSA lowers it to return an address; the caller reads/writes through it). Mark and consume BYREF.
  if (Kind = kFUNCTION) and Context.Check(ttParamMode) and
     (UpperCase(Context.CurrentToken.Value) = kBYREF) and Assigned(NameNode) then
  begin
    Result.Attributes.Values['BYREFRET'] := '1';
    Context.Advance;                                // consume BYREF
  end;

  // FUNCTION return type: "FUNCTION name(...) AS rettype" (M3.2). Attach the type as a child
  // of the name node so the pre-scan can type the function name (UDT handle / builtin bank).
  if (Kind = kFUNCTION) and Context.Check(ttAsType) and Assigned(NameNode) then
  begin
    Context.Advance;                              // AS
    if Context.Check(ttIdentifier) then
    begin
      RetTok := Context.CurrentToken;
      NameNode.AddChild(TASTNode.CreateWithValue(antIdentifier,
                   ParseDottedName, RetTok));       // dotted: namespace-qualified return type
    end;
  end;

  // Arity-based constructor overloading (M4.4d): encode the explicit-parameter count in the label
  // (THIS excluded) so multiple CONSTRUCTORs of the same type get distinct procedure labels, e.g.
  // "TYPE.CONSTRUCTOR#0", "TYPE.CONSTRUCTOR#2". The call site resolves by argument count.
  if (Kind = kCONSTRUCTOR) and Assigned(NameNode) then
    NameNode.Value := QualName + '#' + IntToStr(ParamList.ChildCount - 1);

  Result.AddChild(ParamList);

  ParseProcedureBody(Result);                     // statements up to END SUB/FUNCTION
  ConsumeEndProcedure;
  DoNodeCreated(Result);
end;

function TPackratParser.ParseCallStatement: TASTNode;
var
  Token, NameTok: TLexerToken;
  ArgList, ArgExpr: TASTNode;
  HasParens: Boolean;
begin
  // CALL name [ ( arg [, arg ...] ) ]  — QB/FB statement-level SUB invocation.
  // Also accepts unparenthesised args ("CALL name a, b"). The arguments are kept in
  // an antArgumentList child; SSA lowering wires them through the transfer registers.
  Token := Context.CurrentToken;
  Context.Advance;                                // consume CALL
  Result := nil;
  if not Context.Check(ttIdentifier) then
    Exit;                                         // malformed CALL: nothing to call
  NameTok := Context.CurrentToken;
  Result := TASTNode.CreateWithValue(antProcedureCall, UpperCase(NameTok.Value), NameTok);
  Context.Advance;                                // consume name

  ArgList := TASTNode.Create(antArgumentList, Token);
  Result.AddChild(ArgList);

  HasParens := Context.Check(ttDelimParOpen);
  if HasParens then Context.Advance;              // (
  // Parse a comma-separated argument list (empty is fine).
  if not (Context.Check(ttEndOfFile) or Context.Check(ttEndOfLine) or
          Context.Check(ttSeparStmt) or Context.Check(ttDelimParClose)) then
  begin
    repeat
      ArgExpr := FExpressionParser.ParseExpression;
      if not Assigned(ArgExpr) then Break;
      ArgList.AddChild(ArgExpr);
      if Context.Check(ttSeparParam) then
        Context.Advance                           // ,
      else
        Break;
    until False;
  end;
  if HasParens and Context.Check(ttDelimParClose) then
    Context.Advance;                              // )
  DoNodeCreated(Result);
end;

function TPackratParser.ParseBaseStatement: TASTNode;
var
  Token: TLexerToken;
  ArgList, ArgExpr: TASTNode;
  HasParens: Boolean;
begin
  // BASE [ ( args ) ] — explicit base-constructor call inside a child CONSTRUCTOR body. Lowers to an
  // antProcedureCall named "BASE"; SSA routes it to the owner type's parent constructor (by arity) on
  // THIS, and suppresses the automatic default-base chaining for this ctor.
  Token := Context.CurrentToken;
  Context.Advance;                                // consume BASE
  Result := TASTNode.CreateWithValue(antProcedureCall, 'BASE', Token);
  ArgList := TASTNode.Create(antArgumentList, Token);
  Result.AddChild(ArgList);

  HasParens := Context.Check(ttDelimParOpen);
  if HasParens then Context.Advance;              // (
  if not (Context.Check(ttEndOfFile) or Context.Check(ttEndOfLine) or
          Context.Check(ttSeparStmt) or Context.Check(ttDelimParClose)) then
  begin
    repeat
      ArgExpr := FExpressionParser.ParseExpression;
      if not Assigned(ArgExpr) then Break;
      ArgList.AddChild(ArgExpr);
      if Context.Check(ttSeparParam) then
        Context.Advance                           // ,
      else
        Break;
    until False;
  end;
  if HasParens and Context.Check(ttDelimParClose) then
    Context.Advance;                              // )
  DoNodeCreated(Result);
end;

function TPackratParser.ParseThreadWaitStatement: TASTNode;
var
  Token: TLexerToken;
  HandleExpr: TASTNode;
  HasParens: Boolean;
begin
  // THREADWAIT handle  (or THREADWAIT(handle)) — join a worker thread. Lowers to antThreadWait
  // with the handle expression as child0; SSA emits ssaThreadWait(handle).
  Token := Context.CurrentToken;
  Context.Advance;                                // consume THREADWAIT
  Result := nil;
  HasParens := Context.Check(ttDelimParOpen);
  if HasParens then Context.Advance;              // (
  HandleExpr := FExpressionParser.ParseExpression;
  if not Assigned(HandleExpr) then Exit;          // malformed THREADWAIT
  if HasParens and Context.Check(ttDelimParClose) then
    Context.Advance;                              // )
  Result := TASTNode.CreateWithValue(antThreadWait, kTHREADWAIT, Token);
  Result.AddChild(HandleExpr);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseThreadDetachStatement: TASTNode;
var
  Token: TLexerToken;
  HandleExpr: TASTNode;
  HasParens: Boolean;
begin
  // THREADDETACH handle  (or THREADDETACH(handle)) — detach a worker. child0 = handle expr.
  Token := Context.CurrentToken;
  Context.Advance;                                // consume THREADDETACH
  Result := nil;
  HasParens := Context.Check(ttDelimParOpen);
  if HasParens then Context.Advance;              // (
  HandleExpr := FExpressionParser.ParseExpression;
  if not Assigned(HandleExpr) then Exit;
  if HasParens and Context.Check(ttDelimParClose) then
    Context.Advance;                              // )
  Result := TASTNode.CreateWithValue(antThreadDetach, kTHREADDETACH, Token);
  Result.AddChild(HandleExpr);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseMutexOpStatement: TASTNode;
var
  Token: TLexerToken;
  HandleExpr: TASTNode;
  HasParens: Boolean;
  NodeType: TASTNodeType;
  Name: string;
begin
  // MUTEXLOCK / MUTEXUNLOCK / MUTEXDESTROY handle  (parens optional). Lowers to the matching
  // antMutex* node with the handle expression as child0.
  Token := Context.CurrentToken;
  case Token.TokenType of
    ttMutexUnlock:  begin NodeType := antMutexUnlock;  Name := kMUTEXUNLOCK;  end;
    ttMutexDestroy: begin NodeType := antMutexDestroy; Name := kMUTEXDESTROY; end;
  else
    begin NodeType := antMutexLock; Name := kMUTEXLOCK; end;
  end;
  Context.Advance;                                // consume the MUTEX* keyword
  Result := nil;
  HasParens := Context.Check(ttDelimParOpen);
  if HasParens then Context.Advance;              // (
  HandleExpr := FExpressionParser.ParseExpression;
  if not Assigned(HandleExpr) then Exit;          // malformed
  if HasParens and Context.Check(ttDelimParClose) then
    Context.Advance;                              // )
  Result := TASTNode.CreateWithValue(NodeType, Name, Token);
  Result.AddChild(HandleExpr);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseCondWaitStatement: TASTNode;
var
  Token: TLexerToken;
  CondExpr, MutexExpr: TASTNode;
  HasParens: Boolean;
begin
  // CONDWAIT cond, mutex  (or CONDWAIT(cond, mutex)) — child0 = cond handle, child1 = mutex handle.
  Token := Context.CurrentToken;
  Context.Advance;                                // consume CONDWAIT
  Result := nil;
  HasParens := Context.Check(ttDelimParOpen);
  if HasParens then Context.Advance;              // (
  CondExpr := FExpressionParser.ParseExpression;
  if not Assigned(CondExpr) then Exit;
  if not Context.Check(ttSeparParam) then Exit;   // expect ,
  Context.Advance;                                // ,
  MutexExpr := FExpressionParser.ParseExpression;
  if not Assigned(MutexExpr) then Exit;
  if HasParens and Context.Check(ttDelimParClose) then
    Context.Advance;                              // )
  Result := TASTNode.CreateWithValue(antCondWait, kCONDWAIT, Token);
  Result.AddChild(CondExpr);
  Result.AddChild(MutexExpr);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseCondOpStatement: TASTNode;
var
  Token: TLexerToken;
  HandleExpr: TASTNode;
  HasParens: Boolean;
  NodeType: TASTNodeType;
  Name: string;
begin
  // CONDSIGNAL / CONDBROADCAST / CONDDESTROY cond  (parens optional). child0 = cond handle.
  Token := Context.CurrentToken;
  case Token.TokenType of
    ttCondBroadcast: begin NodeType := antCondBroadcast; Name := kCONDBROADCAST; end;
    ttCondDestroy:   begin NodeType := antCondDestroy;   Name := kCONDDESTROY;   end;
  else
    begin NodeType := antCondSignal; Name := kCONDSIGNAL; end;
  end;
  Context.Advance;                                // consume the COND* keyword
  Result := nil;
  HasParens := Context.Check(ttDelimParOpen);
  if HasParens then Context.Advance;              // (
  HandleExpr := FExpressionParser.ParseExpression;
  if not Assigned(HandleExpr) then Exit;
  if HasParens and Context.Check(ttDelimParClose) then
    Context.Advance;                              // )
  Result := TASTNode.CreateWithValue(NodeType, Name, Token);
  Result.AddChild(HandleExpr);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseSharedError: TASTNode;
var
  Tok: TLexerToken;
begin
  // `SHARED x` as a standalone statement (typically attempted inside a SUB/FUNCTION) is not a -lang fb
  // feature — the FreeBASIC manual: "The Shared statement inside scope blocks ... is not supported. Use
  // Dim|Redim|Common|Static Shared in the main program instead." Report a clean error and skip the
  // identifier list so parsing can recover.
  Tok := Context.CurrentToken;
  HandleError('SHARED is only valid as the DIM SHARED modifier at module level, not as a statement '
            + '(declare the variable with DIM SHARED outside the SUB/FUNCTION)', Tok);
  Context.Advance;                                // consume SHARED
  while Context.Check(ttIdentifier) do
  begin
    Context.Advance;                              // name
    if Context.Check(ttSeparParam) then Context.Advance else Break;   // optional comma
  end;
  Result := nil;
end;

function TPackratParser.AtEndType: Boolean;
begin
  // END TYPE  (END is ttProgramEnd, TYPE is ttTypeDecl)
  Result := Context.Check(ttProgramEnd) and Assigned(Context.PeekNext) and
            (Context.PeekNext.TokenType = ttTypeDecl);
end;

procedure TPackratParser.ConsumeEndType;
begin
  if AtEndType then
  begin
    Context.Advance;   // END
    Context.Advance;   // TYPE
  end;
end;

function TPackratParser.AtEndWith: Boolean;
begin
  // END WITH  (END is ttProgramEnd, WITH is ttWithBlock)
  Result := Context.Check(ttProgramEnd) and Assigned(Context.PeekNext) and
            (Context.PeekNext.TokenType = ttWithBlock);
end;

function TPackratParser.ParseWith: TASTNode;
// WITH obj <newline> ... <newline> END WITH. Parse-time desugar: while parsing the body, the
// expression parser substitutes the (cloned) object for any leading '.field'. The body is
// returned as an antBlock — WITH itself emits nothing.
var
  Token: TLexerToken;
  ObjExpr, Stmt, PrevWith: TASTNode;
  PrevIdx: Integer;
begin
  Token := Context.CurrentToken;
  Context.Advance;                                  // consume WITH
  ObjExpr := FExpressionParser.ParseExpression;
  Result := TASTNode.Create(antBlock, Token);
  if not Assigned(ObjExpr) then
  begin
    if AtEndWith then begin Context.Advance; Context.Advance; end;
    Exit;
  end;

  PrevWith := FExpressionParser.WithObject;         // support nested WITH
  FExpressionParser.WithObject := ObjExpr;
  while not Context.Check(ttEndOfFile) do
  begin
    if Context.Match(ttEndOfLine) then Continue;
    if Context.Check(ttSeparStmt) then begin Context.Advance; Continue; end;
    if AtEndWith then Break;
    PrevIdx := Context.CurrentIndex;
    Stmt := ParseStatement;
    if Assigned(Stmt) then
      Result.AddChild(Stmt)
    else if Context.CurrentIndex = PrevIdx then
      Break;
  end;
  FExpressionParser.WithObject := PrevWith;         // restore outer WITH (or nil)

  if AtEndWith then
  begin
    Context.Advance;   // END
    Context.Advance;   // WITH
  end;
  DoNodeCreated(Result);
end;

function TPackratParser.AtEndNamespace: Boolean;
begin
  // END NAMESPACE  (END is ttProgramEnd, NAMESPACE is ttNamespaceBlock)
  Result := Context.Check(ttProgramEnd) and Assigned(Context.PeekNext) and
            (Context.PeekNext.TokenType = ttNamespaceBlock);
end;

function TPackratParser.ParseDottedName: string;
// Read "ident(.ident)*" — used for namespace-qualified type names ("Forms.Point"). The cursor
// must be on the first identifier. Segments after a '.' may be reserved words (member names).
begin
  Result := UpperCase(VarToStr(Context.CurrentToken.Value));
  Context.Advance;                                   // first segment
  while Context.Check(ttOpDot) and Assigned(Context.PeekNext) and
        (Length(VarToStr(Context.PeekNext.Value)) > 0) and
        (UpCase(VarToStr(Context.PeekNext.Value)[1]) in ['A'..'Z', '_']) do
  begin
    Context.Advance;                                 // '.'
    Result := Result + '.' + UpperCase(VarToStr(Context.CurrentToken.Value));
    Context.Advance;                                 // segment
  end;
end;

function TPackratParser.ParseNamespaceDecl: TASTNode;
// NAMESPACE name <newline> member-statements <newline> END NAMESPACE (FreeBASIC). The body holds
// ordinary declarations (TYPE/SUB/FUNCTION/CONST/DIM); a later AST pass (SedaiNamespace) mangles
// their names to "name.member" and hoists them to module level. The name may itself be dotted
// (nested specifier, e.g. NAMESPACE Outer.Inner).
var
  Token: TLexerToken;
  Stmt: TASTNode;
  NsName: string;
  PrevIdx: Integer;
begin
  Token := Context.CurrentToken;
  Context.Advance;                                   // consume NAMESPACE
  Result := nil;
  if not Context.Check(ttIdentifier) then
  begin
    HandleError('Expected a namespace name after NAMESPACE', Context.CurrentToken);
    Exit;
  end;
  NsName := ParseDottedName;                          // dotted nested specifier allowed
  Result := TASTNode.CreateWithValue(antNamespace, NsName, Token);

  while not Context.Check(ttEndOfFile) do
  begin
    if Context.Match(ttEndOfLine) then Continue;
    if Context.Check(ttSeparStmt) then begin Context.Advance; Continue; end;
    if AtEndNamespace then Break;
    PrevIdx := Context.CurrentIndex;
    Stmt := ParseStatement;
    if Assigned(Stmt) then
      Result.AddChild(Stmt)
    else if Context.CurrentIndex = PrevIdx then
      Context.Advance;                               // no progress: skip a token (defensive)
  end;

  if AtEndNamespace then
  begin
    Context.Advance;   // END
    Context.Advance;   // NAMESPACE
  end;
  DoNodeCreated(Result);
end;

function TPackratParser.ParseTypeDecl: TASTNode;
var
  Token, NameTok, FieldTok: TLexerToken;
  FieldNode, TypeNode: TASTNode;
  PrevIdx: Integer;
  FieldTypeName: string;
begin
  // TYPE name <newline> field AS type <newline> ... END TYPE
  // Each field node is antIdentifier(fieldName) with one child antIdentifier(typeName).
  // An empty type name child means "infer from the field's name suffix" (SSA side).
  Token := Context.CurrentToken;
  Context.Advance;                                  // consume TYPE
  Result := nil;
  // The type name must be a plain identifier; a reserved word (e.g. the graphics keyword
  // CIRCLE/BOX/LINE) is not a valid type name — report it cleanly instead of silently bailing
  // and leaving the stream misaligned (which can derail later parsing).
  if not Context.Check(ttIdentifier) then
  begin
    HandleError(Format('"%s" is a reserved word and cannot be used as a type name',
                       [Context.CurrentToken.Value]), Context.CurrentToken);
    Exit;
  end;
  NameTok := Context.CurrentToken;
  Result := TASTNode.CreateWithValue(antTypeDecl, UpperCase(NameTok.Value), NameTok);
  Context.Advance;                                  // consume type name

  // Optional single inheritance: TYPE Child EXTENDS Parent (M4.2). Stored as an attribute.
  if Context.Check(ttExtends) then
  begin
    Context.Advance;                                // consume EXTENDS
    if Context.Check(ttIdentifier) then
    begin
      Result.Attributes.Values['EXTENDS'] := UpperCase(Context.CurrentToken.Value);
      Context.Advance;                              // parent type name
    end;
  end;

  while not Context.Check(ttEndOfFile) do
  begin
    if Context.Match(ttEndOfLine) then Continue;
    if Context.Check(ttSeparStmt) then begin Context.Advance; Continue; end;
    if AtEndType then Break;
    PrevIdx := Context.CurrentIndex;
    // A field name may be an identifier or a reserved word (e.g. LEN, TYPE, NAME): accept any
    // alphabetic token as the field name here.
    if Context.Check(ttIdentifier) or
       ((Length(Context.CurrentToken.Value) > 0) and
        (UpCase(Context.CurrentToken.Value[1]) in ['A'..'Z', '_'])) then
    begin
      FieldTok := Context.CurrentToken;
      Context.Advance;                              // field name
      FieldTypeName := '';                          // empty => infer by suffix
      if Context.Check(ttAsType) then
      begin
        Context.Advance;                            // AS
        if Context.Check(ttIdentifier) or
           ((Length(Context.CurrentToken.Value) > 0) and
            (UpCase(Context.CurrentToken.Value[1]) in ['A'..'Z', '_'])) then
        begin
          FieldTypeName := ParseDottedName;         // dotted: namespace-qualified field type
          // FreeBASIC pointer field "<type> PTR": stored as an int handle. Capturing the suffix keeps
          // a self-referential field (e.g. "NXT AS NODE PTR" in a linked list) from being treated as a
          // nested record (which would recurse forever when allocating the type).
          while Context.Check(ttIdentifier) and (UpCase(Context.CurrentToken.Value) = 'PTR') do
          begin
            FieldTypeName := FieldTypeName + ' PTR';
            Context.Advance;                        // consume PTR
          end;
          // FreeBASIC fixed-length string field: "AS STRING * n" / "AS WSTRING * n". Consume the
          // "* <length>" so the field parses; the capacity is advisory in v1 (variable-length storage).
          if Context.Check(ttOpMul) then
          begin
            Context.Advance;                        // '*'
            FExpressionParser.ParseExpression(precCall).Free;   // length operand (discarded)
          end;
        end;
      end;
      FieldNode := TASTNode.CreateWithValue(antIdentifier, UpperCase(FieldTok.Value), FieldTok);
      TypeNode := TASTNode.CreateWithValue(antIdentifier, FieldTypeName, FieldTok);
      FieldNode.AddChild(TypeNode);
      Result.AddChild(FieldNode);
    end
    else
      Context.Advance;                              // skip unexpected token (defensive)
    if Context.CurrentIndex = PrevIdx then Break;   // no progress guard
  end;
  ConsumeEndType;
  DoNodeCreated(Result);
end;

// Build the condition for a CASE clause: "(sel = v1) OR (sel = v2) OR ...", where
// each 'sel' is a fresh clone of the SELECT selector (so it isn't shared in the AST).
function TPackratParser.ParseCaseCondition(Selector: TASTNode): TASTNode;
var
  ValueExpr, Cmp: TASTNode;
begin
  Result := nil;
  repeat
    ValueExpr := FExpressionParser.ParseExpression;
    if not Assigned(ValueExpr) then Break;
    // NB: SSA lowering reads the operator from Node.Token.TokenType, so the binary
    // op needs a real token of that type (not nil).
    Cmp := CreateBinaryOpNode(ttOpEq, Selector.Clone, ValueExpr,
                              TLexerToken.CreateSimple(ttOpEq, '='));     // sel = value
    if Result = nil then
      Result := Cmp
    else
      Result := CreateBinaryOpNode(ttBitwiseOR, Result, Cmp,
                                   TLexerToken.CreateSimple(ttBitwiseOR, 'OR'));  // OR chain
    if Context.Check(ttSeparParam) then
      Context.Advance      // consume ',' and parse the next value
    else
      Break;
  until False;
end;

// Collect the statements of a CASE body into Parent, until the next CASE, END
// SELECT or end-of-file. (Like ParseBlockIfBody: a nil statement is not a stop.)
procedure TPackratParser.ParseCaseBody(Parent: TASTNode);
var
  Statement: TASTNode;
  PrevIdx: Integer;
begin
  while not Context.Check(ttEndOfFile) do
  begin
    if Context.Match(ttEndOfLine) then Continue;
    if Context.Check(ttSeparStmt) then begin Context.Advance; Continue; end;
    if Context.Check(ttCaseClause) or AtEndSelect then Break;
    PrevIdx := Context.CurrentIndex;
    Statement := ParseStatement;
    if Assigned(Statement) then
      Parent.AddChild(Statement)
    else if Context.CurrentIndex = PrevIdx then
      Break;
  end;
end;

function TPackratParser.ParseSelectCase: TASTNode;
var
  Token: TLexerToken;
  Selector, RootIf, PrevIf, CurIf, ThenNode, ElseNode, Cond, BlockNode: TASTNode;
  IsFirst: Boolean;
begin
  // SELECT CASE <selector> / CASE <values> ... / [CASE ELSE ...] / END SELECT
  // Desugared to a nested IF/ELSEIF/ELSE chain (conditions clone the selector).
  Token := Context.CurrentToken;
  Context.Advance;                                  // consume SELECT
  if Context.Check(ttCaseClause) then Context.Advance;   // consume CASE
  Selector := ParseExpression;

  RootIf := nil; PrevIf := nil; IsFirst := True;
  while (not Context.Check(ttEndOfFile)) and (not AtEndSelect) do
  begin
    if Context.Match(ttEndOfLine) then Continue;
    if Context.Check(ttSeparStmt) then begin Context.Advance; Continue; end;
    if not Context.Check(ttCaseClause) then Break;   // unexpected token
    Context.Advance;                                 // consume CASE

    if Context.Check(ttConditionalElse) then
    begin
      // CASE ELSE — the default branch; closes the chain.
      Context.Advance;                               // consume ELSE
      if Assigned(PrevIf) then
      begin
        ElseNode := TASTNode.Create(antElse, Token);
        ParseCaseBody(ElseNode);
        PrevIf.AddChild(ElseNode);
      end
      else
      begin
        // SELECT with only CASE ELSE: the body always runs (wrap in a block).
        BlockNode := TASTNode.Create(antBlock, Token);
        ParseCaseBody(BlockNode);
        RootIf := BlockNode;
      end;
      Break;
    end
    else
    begin
      // CASE value [, value ...]
      Cond := ParseCaseCondition(Selector);
      ThenNode := TASTNode.Create(antThen, Token);
      ParseCaseBody(ThenNode);
      CurIf := TASTNode.Create(antIf, Token);
      CurIf.AddChild(Cond);
      CurIf.AddChild(ThenNode);
      if IsFirst then
      begin
        RootIf := CurIf;
        IsFirst := False;
      end
      else
      begin
        ElseNode := TASTNode.Create(antElse, Token);
        ElseNode.AddChild(CurIf);                    // ELSEIF = nested IF in ELSE
        PrevIf.AddChild(ElseNode);
      end;
      PrevIf := CurIf;
    end;
  end;

  ConsumeEndSelect;
  if Assigned(Selector) then Selector.Free;          // only clones were used
  Result := RootIf;                                  // nil if there were no clauses
  if Assigned(Result) then DoNodeCreated(Result);
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
  Token, CondToken: TLexerToken;
  Condition: TASTNode;
  ConditionType: string;
  ConditionPosition: string;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antDoLoop, Token);
  Context.Advance; // Consume DO

  // NOTE: DO/LOOP is handled internally by ParseDoStatement using FindMatchingEnd
  // and ParseBlockUntil, so we don't use the validation stack for this.
  // The LOOP is consumed directly by this function.

  ConditionType := '';
  ConditionPosition := '';
  Condition := nil;

  // Check for condition immediately after DO (DO UNTIL expr / DO WHILE expr)
  if Context.Check(ttLoopControl) then
  begin
    CondToken := Context.CurrentToken;
    ConditionType := UpperCase(CondToken.Value);
    ConditionPosition := 'TOP';
    Context.Advance; // Consume WHILE or UNTIL

    // Parse the condition expression
    Condition := ParseExpression;
  end;

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
    // Check for condition after LOOP (LOOP UNTIL expr / LOOP WHILE expr)
    // Only if we don't already have a top condition
    if (ConditionType = '') and Context.Check(ttLoopControl) then
    begin
      CondToken := Context.CurrentToken;
      ConditionType := UpperCase(CondToken.Value);
      ConditionPosition := 'BOTTOM';
      Context.Advance; // Consume WHILE or UNTIL

      Condition := ParseExpression;
    end;
  end;

  // Add condition as second child (if present)
  if Assigned(Condition) then
    Result.AddChild(Condition);

  // Store condition metadata in attributes
  Result.Attributes.Values['ConditionType'] := ConditionType;
  Result.Attributes.Values['ConditionPosition'] := ConditionPosition;

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
  Token, KindTok: TLexerToken;
  IsExit, IsContinue: Boolean;
  Kw: string;
  ExprNode: TASTNode;
  Levels: Integer;
begin
  Token := Context.CurrentToken;
  Kw := UpperCase(Token.Value);
  IsExit := Kw = kEXIT;
  IsContinue := Kw = kCONTINUE;
  Result := TASTNode.Create(antReturn, Token);
  Context.Advance; // consume EXIT / CONTINUE / RETURN

  if IsExit or IsContinue then
  begin
    // EXIT [SUB|FUNCTION|FOR|DO|WHILE|LOOP] / CONTINUE [FOR|DO|WHILE|LOOP]. Capture the kind
    // word (if any) in the node value so SSA can route EXIT SUB/FUNCTION to a frame return vs a
    // loop exit. FreeBASIC multi-level form repeats the same loop kind comma-separated
    // ("Exit For, For" / "Continue Do, Do") to target the N-th enclosing loop of that kind: count
    // the repetitions into the LEVELS attribute (default 1 = innermost).
    if not (Context.Check(ttEndOfFile) or Context.Check(ttEndOfLine) or Context.Check(ttSeparStmt)) then
    begin
      KindTok := Context.CurrentToken;
      Result.Value := Kw + ' ' + UpperCase(KindTok.Value);
      Context.Advance;   // consume the kind keyword (SUB/FUNCTION/FOR/...)
      Levels := 1;
      // Additional ", <same-kind>" entries increase the target depth (loops only).
      while Context.Check(ttSeparParam) and Assigned(Context.PeekNext) and
            (UpperCase(Context.PeekNext.Value) = UpperCase(KindTok.Value)) do
      begin
        Context.Advance;   // comma
        Context.Advance;   // repeated kind word
        Inc(Levels);
      end;
      if Levels > 1 then
        Result.Attributes.Values[ATTR_LOOP_LEVELS] := IntToStr(Levels);
    end
    else
      Result.Value := Kw;
  end
  else
  begin
    // RETURN [expr]: a bare RETURN ends a GOSUB / procedure; RETURN expr (FreeBASIC) also
    // delivers a FUNCTION result. Parse a trailing expression if present on this line.
    Result.Value := kRETURN;
    if not (Context.Check(ttEndOfFile) or Context.Check(ttEndOfLine) or Context.Check(ttSeparStmt)) then
    begin
      ExprNode := FExpressionParser.ParseExpression;
      if Assigned(ExprNode) then
        Result.AddChild(ExprNode);
    end;
  end;
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
  HandleNode, Expr: TASTNode;
  CmdName: string;
begin
  Token := Context.CurrentToken;
  CmdName := UpperCase(Token.Value);

  // Handle CMD command specifically
  if CmdName = kCMD then
  begin
    Result := TASTNode.Create(antCmd, Token);
    Context.Advance; // Consume CMD

    // CMD file [, write list]
    // Parse file handle: #number or expression
    if Context.Check(ttFileHandlePrefix) then
      Context.Advance;  // Consume #

    // Parse handle (number or expression)
    if Context.Check(ttNumber) or Context.Check(ttInteger) then
    begin
      HandleNode := TASTNode.CreateWithValue(antLiteral, StrToInt(Context.CurrentToken.Value), Context.CurrentToken);
      Result.AddChild(HandleNode);
      Context.Advance;
    end
    else if Context.Check(ttIdentifier) then
    begin
      HandleNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
      Result.AddChild(HandleNode);
      Context.Advance;
    end
    else
    begin
      // Parse as expression
      HandleNode := ParseExpression;
      if Assigned(HandleNode) then
        Result.AddChild(HandleNode)
      else
      begin
        HandleError('Expected file handle after CMD', Token);
        Exit;
      end;
    end;

    // Optional comma and write list
    if Context.Check(ttSeparParam) then
    begin
      Context.Advance; // Consume comma

      // Parse expressions to write (print list)
      while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
      begin
        Expr := ParseExpression;
        if Assigned(Expr) then
          Result.AddChild(Expr)
        else
          Break;

        // Check for separator
        if Context.Check(ttSeparParam) or Context.Check(ttSeparOutput) then
          Context.Advance
        else
          Break;
      end;
    end;

    DoNodeCreated(Result);
    Exit;
  end;

  // Generic IO command handling
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
  if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
  begin
    Param := ParseExpression;
    if Assigned(Param) then
      Result.AddChild(Param);
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseFrameStatement: TASTNode;
var
  Token: TLexerToken;
  Param: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antFrame, Token);
  Context.Advance; // Consume FRAME

  // Parse optional FPS parameter (default 60 if omitted)
  if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
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
  if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
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
  Expression, TargetList, TargetNode: TASTNode;
  Token, JumpToken: TLexerToken;
  IsGosub: Boolean;
begin
  Token := Context.CurrentToken;
  Context.Advance; // Consume ON

  // Parse expression (the selector value)
  Expression := ParseExpression;
  if not Assigned(Expression) then
  begin
    HandleError('Expected expression after ON', Token);
    Result := nil;
    Exit;
  end;

  // Expect GOTO or GOSUB
  if not Context.CheckAny([ttJumpGoto, ttJumpGosub]) then
  begin
    HandleError('Expected GOTO or GOSUB after ON expression', Context.CurrentToken);
    Expression.Free;
    Result := nil;
    Exit;
  end;

  JumpToken := Context.CurrentToken;
  IsGosub := (JumpToken.TokenType = ttJumpGosub);
  Context.Advance; // Consume GOTO/GOSUB

  // Create appropriate node type
  if IsGosub then
    Result := TASTNode.Create(antOnGosub, Token)
  else
    Result := TASTNode.Create(antOnGoto, Token);

  Result.AddChild(Expression);

  // Parse list of target line numbers
  TargetList := TASTNode.Create(antExpressionList, JumpToken);

  // Parse first target (required)
  TargetNode := ParseExpression;
  if Assigned(TargetNode) then
    TargetList.AddChild(TargetNode)
  else
  begin
    HandleError('Expected line number after GOTO/GOSUB', Context.CurrentToken);
    TargetList.Free;
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse additional targets separated by comma
  while Context.Match(ttSeparParam) do
  begin
    TargetNode := ParseExpression;
    if Assigned(TargetNode) then
      TargetList.AddChild(TargetNode)
    else
      Break;
  end;

  Result.AddChild(TargetList);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseBlockStatement: TASTNode;
var
  Token: TLexerToken;
  Statement: TASTNode;
  LineNum: Integer;
  LineNumNode: TASTNode;
  EndKeyword: string;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antBlock, Token);
  Context.Advance; // Consume BEGIN

  // *** PUSH ONTO BLOCK STACK ***
  FValidationStacks.PushBlock(ttBlockBegin, Result, 'BEND', Context.CurrentIndex);

  // *** Parse all statements between BEGIN and BEND ***
  while not Context.Check(ttEndOfFile) do
  begin
    // Skip end-of-line tokens
    if Context.Match(ttEndOfLine) then
    begin
      // Inside a block, only pop simple single-line IFs (not those with
      // active BEGIN blocks).  Do NOT call PopCompletedIfsAtEOL here — its
      // force-clear logic would prematurely pop the outer IF that owns
      // this block, before we even reach BEND.
      while FValidationStacks.HasActiveIf and FValidationStacks.CanPopIfAtEOL do
        FValidationStacks.PopIf;
      Continue;
    end;

    // Skip statement separators (:)
    if Context.Check(ttSeparStmt) then
    begin
      Context.Advance;
      Continue;
    end;

    // Check for BEND - end of block
    if Context.Check(ttBlockEnd) then
    begin
      EndKeyword := UpperCase(Context.CurrentToken.Value);

      // Validate and pop block stack
      FValidationStacks.ValidateBlockEnd(EndKeyword);
      Context.Advance; // Consume BEND

      // For ELSE blocks: clear the flag so the IF can be popped.
      // For THEN blocks: keep HasThenBlock set so the IF stays on the
      // stack, allowing ELSE on a subsequent line. PopCompletedIfsAtEOL
      // will peek for ELSE and pop when appropriate.
      if FValidationStacks.HasActiveIf then
      begin
        if FValidationStacks.GetCurrentIf.HasElse then
          FValidationStacks.ClearElseBlockForCurrentIf;
        // Don't clear HasThenBlock — IF stays alive for possible ELSE
      end;

      Break;
    end;

    // Handle line numbers inside the block
    if Context.Check(ttLineNumber) then
    begin
      Token := Context.CurrentToken;
      try
        LineNum := StrToInt(Token.Value);
      except
        LineNum := 0;
      end;
      LineNumNode := TASTNode.CreateWithValue(antLineNumber, Token.Value, Token);

      // Track BASIC line number for error reporting
      Context.SetCurrentBasicLine(LineNum, BuildSourceLine(Context));

      Result.AddChild(LineNumNode);
      Context.Advance; // Consume line number
      Continue;
    end;

    // Parse statement and add to block
    Statement := ParseStatement;
    if Assigned(Statement) then
      Result.AddChild(Statement)
    else
      Break;
  end;

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

  // *** BEND closes the BEGIN block, so clear the block flags for IF ***
  if FValidationStacks.HasActiveIf then
  begin
    // Clear the block flags since BEGIN/BEND block is now closed
    FValidationStacks.ClearThenBlockForCurrentIf;
    FValidationStacks.ClearElseBlockForCurrentIf;

    // Now check if IF can be closed
    if FValidationStacks.CanPopIfAtEOL then
      FValidationStacks.PopIf;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseMemoryStatement: TASTNode;
var
  Token: TLexerToken;
  CmdName: string;
  Param1, Param2: TASTNode;
begin
  Token := Context.CurrentToken;
  CmdName := UpperCase(Token.Value);

  // SWAP is dialect-dependent: here (reached only in CLASSIC, or in MODERN for a non-SWAP command)
  // it is the C128 RAM-bank memory command. In MODERN, SWAP a,b is intercepted earlier by the dialect
  // profile's MemSwapStatementHandler (mechanism 3), so it never reaches this v7 path.

  // Select appropriate node type based on command
  if CmdName = 'POKE' then
  begin
    Result := TASTNode.Create(antPoke, Token);
    Context.Advance; // Consume POKE

    // POKE address, value - parse two parameters
    Param1 := ParseExpression;
    if Assigned(Param1) then
      Result.AddChild(Param1);

    if Context.Check(ttSeparParam) then
    begin
      Context.Advance; // Consume comma
      Param2 := ParseExpression;
      if Assigned(Param2) then
        Result.AddChild(Param2);
    end;
  end
  else
  begin
    // Other memory commands (BANK, FETCH, etc.) - use generic handling
    Result := TASTNode.Create(antStatement, Token);
    Context.Advance; // Consume memory command

    // Parse parameters as single expression list
    if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
    begin
      Param1 := ParseExpressionList(ttSeparParam);
      if Assigned(Param1) then
        Result.AddChild(Param1);
    end;
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
  else if CmdName = 'COLOR' then
    Result := TASTNode.Create(antColor, Token)
  else if CmdName = 'SETCOLOR' then
    Result := TASTNode.Create(antSetColor, Token)
  else if CmdName = 'WIDTH' then
    Result := TASTNode.Create(antWidth, Token)
  else if CmdName = 'SCALE' then
    Result := TASTNode.Create(antScale, Token)
  else if CmdName = 'PAINT' then
    Result := TASTNode.Create(antPaint, Token)
  else if CmdName = 'WINDOW' then
    Result := TASTNode.Create(antWindow, Token)
  else if CmdName = 'SSHAPE' then
    Result := TASTNode.Create(antSShape, Token)
  else if CmdName = 'GSHAPE' then
    Result := TASTNode.Create(antGShape, Token)
  else if CmdName = 'GLIST' then
    Result := TASTNode.Create(antGList, Token)
  else if CmdName = 'PLOAD' then
    Result := TASTNode.Create(antPLoad, Token)
  else if CmdName = 'PSAVE' then
    Result := TASTNode.Create(antPSave, Token)
  else if CmdName = 'PRST' then
    Result := TASTNode.Create(antPRst, Token)
  else
    Result := TASTNode.Create(antStatement, Token);

  Context.Advance;
  ParamCount := 0;

  // Set max parameters based on command
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
  else if CmdName = 'COLOR' then
    MaxParams := 2  // COLOR source, color
  else if CmdName = 'SETCOLOR' then
    MaxParams := 5  // SETCOLOR index, R, G, B [, A]
  else if CmdName = 'WIDTH' then
    MaxParams := 1  // WIDTH n
  else if CmdName = 'SCALE' then
    MaxParams := 3  // SCALE n [,xmax, ymax]
  else if CmdName = 'PAINT' then
    MaxParams := 4  // PAINT [source], x, y [,mode]
  else if CmdName = 'WINDOW' then
    MaxParams := 5  // WINDOW col1, row1, col2, row2 [,clear]
  else if CmdName = 'SSHAPE' then
    MaxParams := 5  // SSHAPE A$, x1, y1 [,x2, y2]
  else if CmdName = 'GSHAPE' then
    MaxParams := 4  // GSHAPE A$, x, y [,mode]
  else if CmdName = 'GLIST' then
    MaxParams := 0  // GLIST (no parameters)
  else if CmdName = 'PLOAD' then
    MaxParams := 1  // PLOAD "filename"
  else if CmdName = 'PSAVE' then
    MaxParams := 1  // PSAVE "filename"
  else if CmdName = 'PRST' then
    MaxParams := 0  // PRST (no parameters)
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
    else if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
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
      if Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
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
  // SETCOLOR uses standard parameter parsing (index, R, G, B [, A])
  else
  begin
    // Standard parsing for other graphics commands
    // Handle optional parameters: empty params (,,) add nil placeholder
    while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) and (ParamCount < MaxParams) do
    begin
      // Check for empty parameter (comma without expression)
      if Context.Check(ttSeparParam) then
      begin
        // Add nil placeholder for omitted parameter
        Result.AddChild(nil);
        Inc(ParamCount);
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
      else if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
        Break;
    end;
  end;
  DoNodeCreated(Result);
end;

function TPackratParser.ParseSpriteStatement: TASTNode;
var
  Token: TLexerToken;
  Param: TASTNode;
  CmdName: string;
  ParamCount, MaxParams: Integer;
  MovsprMode: Integer;  // 0=abs, 1=rel, 2=polar, 3=auto
begin
  Token := Context.CurrentToken;
  CmdName := UpperCase(Token.Value);
  MovsprMode := 0;

  // Select appropriate node type based on command
  if CmdName = kSPRITE then
    Result := TASTNode.Create(antSprite, Token)
  else if CmdName = kMOVSPR then
    Result := TASTNode.Create(antMovspr, Token)
  else if CmdName = kSPRCOLOR then
    Result := TASTNode.Create(antSprcolor, Token)
  else if CmdName = kSPRSAV then
    Result := TASTNode.Create(antSprsav, Token)
  else if CmdName = kCOLLISION then
    Result := TASTNode.Create(antCollision, Token)
  else if CmdName = kSPRDEF then
    Result := TASTNode.Create(antSprdef, Token)  // SPRDEF: interactive sprite editor
  else if CmdName = kSPRSAVE then
    Result := TASTNode.Create(antSprsave, Token)  // SPRSAVE "file"
  else if CmdName = kSPRLOAD then
    Result := TASTNode.Create(antSprload, Token)  // SPRLOAD "file"
  else if CmdName = kSPRSIZE then
    Result := TASTNode.Create(antSprsize, Token)  // SPRSIZE n, w, h
  else if CmdName = kSPRFORM then
    Result := TASTNode.Create(antSprform, Token)  // SPRFORM n, format
  else
    Result := TASTNode.Create(antStatement, Token);

  Context.Advance;
  ParamCount := 0;

  // Set max parameters based on command
  // SPRITE: 7 (n, enabled, color, priority, scalex, scaley, mode)
  // MOVSPR: 3 (n, x/dist/angle, y/angle/speed) - determined by delimiter
  // SPRCOLOR: 2 (mc1, mc2)
  // SPRSAV: 2 (source, dest)
  // COLLISION: 2 (type, line)
  if CmdName = kSPRITE then
    MaxParams := 7
  else if CmdName = kMOVSPR then
    MaxParams := 3
  else if CmdName = kSPRCOLOR then
    MaxParams := 2
  else if CmdName = kSPRSAV then
    MaxParams := 2
  else if CmdName = kCOLLISION then
    MaxParams := 2
  else if CmdName = kSPRDEF then
    MaxParams := 1   // SPRDEF [n]: optional sprite number
  else if CmdName = kSPRSAVE then
    MaxParams := 1   // SPRSAVE "filename"
  else if CmdName = kSPRLOAD then
    MaxParams := 2   // SPRLOAD "filename" [, usefilecolors]
  else if CmdName = kSPRSIZE then
    MaxParams := 3   // SPRSIZE n, width, height
  else if CmdName = kSPRFORM then
    MaxParams := 2   // SPRFORM n, format
  else
    MaxParams := 10;

  // Parse parameters
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) and (ParamCount < MaxParams) do
  begin
    // Handle comma separator
    if Context.Check(ttSeparParam) then
    begin
      Context.Advance;
      Continue;
    end;

    // MOVSPR special handling: detect +/- for relative mode before parsing expression
    if (CmdName = kMOVSPR) and (ParamCount = 1) then
    begin
      // Check for + or - prefix indicating relative movement
      if Context.Check(ttOpAdd) then
      begin
        MovsprMode := 1;  // Relative mode
        // Store mode marker in node
        Result.Attributes.Values['movspr_mode'] := '1';
      end
      else if Context.Check(ttOpSub) then
      begin
        MovsprMode := 1;  // Relative mode (negative will be in expression)
        Result.Attributes.Values['movspr_mode'] := '1';
      end;
    end;

    Param := ParseExpression;
    if Assigned(Param) then
    begin
      Result.AddChild(Param);
      Inc(ParamCount);
    end
    else
      Break;

    // MOVSPR special handling: detect ; for polar mode or # for auto mode
    if (CmdName = kMOVSPR) and (ParamCount = 2) then
    begin
      if Context.Check(ttSeparOutput) then  // ; semicolon
      begin
        MovsprMode := 2;  // Polar mode: distance;angle
        Result.Attributes.Values['movspr_mode'] := '2';
        Context.Advance;
        Continue;
      end
      else if Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#') then
      begin
        MovsprMode := 3;  // Auto mode: angle#speed
        Result.Attributes.Values['movspr_mode'] := '3';
        Context.Advance;
        Continue;
      end;
    end;

    if Context.Check(ttSeparParam) then
      Context.Advance
    else if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttSeparOutput, ttFileHandlePrefix]) then
      Break;
  end;

  // Set default mode for MOVSPR if not explicitly set
  if (CmdName = kMOVSPR) and (Result.Attributes.Values['movspr_mode'] = '') then
    Result.Attributes.Values['movspr_mode'] := '0';  // Absolute mode

  DoNodeCreated(Result);
end;

function TPackratParser.ParseSoundStatement: TASTNode;
var
  Token: TLexerToken;
  Param: TASTNode;
  ParamCount, MaxParams: Integer;
  CmdName: string;
begin
  Token := Context.CurrentToken;
  CmdName := UpperCase(Token.Value);

  // Select appropriate node type based on command
  if CmdName = 'VOL' then
    Result := TASTNode.Create(antVol, Token)
  else if CmdName = 'SOUND' then
    Result := TASTNode.Create(antSound, Token)
  else if CmdName = 'ENVELOPE' then
    Result := TASTNode.Create(antEnvelope, Token)
  else if CmdName = 'TEMPO' then
    Result := TASTNode.Create(antTempo, Token)
  else if CmdName = 'PLAY' then
    Result := TASTNode.Create(antPlay, Token)
  else if CmdName = 'FILTER' then
    Result := TASTNode.Create(antFilter, Token)
  else
    Result := TASTNode.Create(antStatement, Token);

  Context.Advance;
  ParamCount := 0;

  // Set max parameters based on command
  // VOL: 1 (volume 0-15)
  // TEMPO: 1 (tempo 0-255)
  // SOUND: 8 (vc, freq, dur, dir, min, sv, wf, pw)
  // ENVELOPE: 7 (e, a, d, s, r, wf, pw)
  // FILTER: 5 (cf, lp, bp, hp, res)
  // PLAY: 1 (string with control characters)
  if CmdName = 'VOL' then
    MaxParams := 1
  else if CmdName = 'TEMPO' then
    MaxParams := 1
  else if CmdName = 'SOUND' then
    MaxParams := 8
  else if CmdName = 'ENVELOPE' then
    MaxParams := 7
  else if CmdName = 'FILTER' then
    MaxParams := 5
  else if CmdName = 'PLAY' then
    MaxParams := 1  // Single string argument
  else
    MaxParams := 10;

  // Parse parameters
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) and (ParamCount < MaxParams) do
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
    else if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
      Break;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseFileOperationStatement: TASTNode;
var
  Token: TLexerToken;
  Param, HandleNode: TASTNode;
  CmdName, ModeStr, MW: string;
begin
  Token := Context.CurrentToken;
  CmdName := UpperCase(Token.Value);

  // Recognize file operation commands
  case CmdName of
    'LOAD', 'DLOAD': Result := TASTNode.Create(antLoad, Token);
    'SAVE', 'DSAVE': Result := TASTNode.Create(antSave, Token);
    'VERIFY', 'DVERIFY': Result := TASTNode.Create(antVerify, Token);
    'BLOAD': Result := TASTNode.Create(antBload, Token);
    'BSAVE': Result := TASTNode.Create(antBsave, Token);
    'BOOT': Result := TASTNode.Create(antBoot, Token);
    // Disk file I/O with handle
    'DOPEN', 'OPEN': Result := TASTNode.Create(antDopen, Token);
    'DCLOSE', 'CLOSE': Result := TASTNode.Create(antDclose, Token);
    'APPEND': Result := TASTNode.Create(antAppend, Token);
    'DCLEAR': Result := TASTNode.Create(antDclear, Token);
    'RECORD': Result := TASTNode.Create(antRecord, Token);
  else
    Result := TASTNode.Create(antStatement, Token); // Other file commands
  end;

  Context.Advance; // Consume file operation command

  // FreeBASIC OPEN: OPEN "filename" FOR {INPUT|OUTPUT|APPEND|BINARY|RANDOM} AS [#]n [LEN = reclen].
  // Detected when OPEN is NOT immediately followed by a '#handle' (that is the legacy C64/C128 form).
  // Built as the same antDopen node (child0=handle, child1=filename, child2=mode$) the legacy form uses.
  if ((CmdName = 'DOPEN') or (CmdName = 'OPEN')) and
     not (Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#')) then
  begin
    Param := ParseExpression;     // filename
    if not Assigned(Param) then begin HandleError('Expected filename after OPEN', Token); Exit; end;
    ModeStr := 'R';
    if UpperCase(Context.CurrentToken.Value) = 'FOR' then
    begin
      Context.Advance;            // FOR
      MW := UpperCase(Context.CurrentToken.Value);
      if MW = 'INPUT' then ModeStr := 'R'
      else if MW = 'OUTPUT' then ModeStr := 'W'
      else if MW = 'APPEND' then ModeStr := 'A'
      else if MW = 'BINARY' then ModeStr := 'B'
      else if MW = 'RANDOM' then ModeStr := 'B'
      else HandleError('Expected INPUT/OUTPUT/APPEND/BINARY/RANDOM after FOR', Token);
      Context.Advance;            // mode word
    end;
    if (UpperCase(Context.CurrentToken.Value) = 'AS') or Context.Check(ttAsType) then
      Context.Advance;            // AS
    if Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#') then
      Context.Advance;            // optional '#'
    if Context.Check(ttNumber) or Context.Check(ttInteger) then
    begin
      HandleNode := TASTNode.CreateWithValue(antLiteral, StrToInt(Context.CurrentToken.Value), Context.CurrentToken);
      Context.Advance;
    end
    else if Context.Check(ttIdentifier) then
    begin
      HandleNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
      Context.Advance;
    end
    else begin HandleError('Expected file number after AS', Token); Exit; end;
    if UpperCase(Context.CurrentToken.Value) = 'LEN' then   // optional "LEN = reclen" (RANDOM): ignored v1
    begin
      Context.Advance;
      if Context.Check(ttOpEq) then Context.Advance;
      ParseExpression.Free;
    end;
    Result.AddChild(HandleNode);                            // child 0 = handle
    Result.AddChild(Param);                                 // child 1 = filename
    Result.AddChild(TASTNode.CreateWithValue(antLiteral, ModeStr, Token));  // child 2 = mode$
    DoNodeCreated(Result);
    Exit;
  end;

  // Special handling for DOPEN/OPEN and DCLOSE/CLOSE
  if (CmdName = 'DOPEN') or (CmdName = 'OPEN') or (CmdName = 'DCLOSE') or (CmdName = 'CLOSE') then
  begin
    // Parse file handle: #number or #identifier
    // Syntax: DOPEN #1, "filename" [, mode$]
    //         DOPEN #MYFILE, "filename" [, mode$]
    //         DCLOSE #1
    //         DCLOSE #MYFILE

    // Expect # prefix
    if Context.Check(ttFileHandlePrefix) then
      Context.Advance  // Consume #
    else if Context.CurrentToken.Value = '#' then
      Context.Advance; // Handle # as separate token if needed

    // Parse handle (number or identifier)
    if Context.Check(ttNumber) or Context.Check(ttInteger) then
    begin
      // Numeric handle: #1, #2, etc. - convert string to integer for proper SSA handling
      HandleNode := TASTNode.CreateWithValue(antLiteral, StrToInt(Context.CurrentToken.Value), Context.CurrentToken);
      Result.AddChild(HandleNode);
      Context.Advance;
    end
    else if Context.Check(ttIdentifier) then
    begin
      // Named handle: #MYFILE, #DATA, etc.
      HandleNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
      Result.AddChild(HandleNode);
      Context.Advance;
    end
    else
    begin
      HandleError('Expected file handle after #', Token);
      Exit;
    end;

    // For DOPEN/OPEN, parse filename and optional mode
    if (CmdName = 'DOPEN') or (CmdName = 'OPEN') then
    begin
      // Expect comma separator
      if Context.Check(ttSeparParam) then
        Context.Advance;

      // Parse filename (required)
      Param := ParseExpression;
      if Assigned(Param) then
        Result.AddChild(Param)
      else
      begin
        HandleError('Expected filename after handle', Token);
        Exit;
      end;

      // Parse optional mode parameter
      if Context.Check(ttSeparParam) then
      begin
        Context.Advance;
        Param := ParseExpression;
        if Assigned(Param) then
          Result.AddChild(Param);
      end;
    end;
    // DCLOSE/CLOSE only needs the handle, already parsed

    DoNodeCreated(Result);
    Exit;
  end;

  // Special handling for APPEND (append data to file)
  // Syntax: APPEND #handle, expression
  if CmdName = 'APPEND' then
  begin
    // Expect # prefix
    if Context.Check(ttFileHandlePrefix) then
      Context.Advance
    else if Context.CurrentToken.Value = '#' then
      Context.Advance;

    // Parse handle
    if Context.Check(ttNumber) or Context.Check(ttInteger) then
    begin
      HandleNode := TASTNode.CreateWithValue(antLiteral, StrToInt(Context.CurrentToken.Value), Context.CurrentToken);
      Result.AddChild(HandleNode);
      Context.Advance;
    end
    else if Context.Check(ttIdentifier) then
    begin
      HandleNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
      Result.AddChild(HandleNode);
      Context.Advance;
    end
    else
    begin
      HandleError('Expected file handle after #', Token);
      Exit;
    end;

    // Parse comma and data expression
    if Context.Check(ttSeparParam) then
      Context.Advance;

    Param := ParseExpression;
    if Assigned(Param) then
      Result.AddChild(Param);

    DoNodeCreated(Result);
    Exit;
  end;

  // Special handling for DCLEAR (close all channels)
  // Syntax: DCLEAR
  if CmdName = 'DCLEAR' then
  begin
    // No parameters needed
    DoNodeCreated(Result);
    Exit;
  end;

  // Special handling for RECORD (seek file position)
  // Syntax: RECORD #handle, position
  if CmdName = 'RECORD' then
  begin
    // Expect # prefix
    if Context.Check(ttFileHandlePrefix) then
      Context.Advance
    else if Context.CurrentToken.Value = '#' then
      Context.Advance;

    // Parse handle
    if Context.Check(ttNumber) or Context.Check(ttInteger) then
    begin
      HandleNode := TASTNode.CreateWithValue(antLiteral, StrToInt(Context.CurrentToken.Value), Context.CurrentToken);
      Result.AddChild(HandleNode);
      Context.Advance;
    end
    else if Context.Check(ttIdentifier) then
    begin
      HandleNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
      Result.AddChild(HandleNode);
      Context.Advance;
    end
    else
    begin
      HandleError('Expected file handle after #', Token);
      Exit;
    end;

    // Parse comma and position expression
    if Context.Check(ttSeparParam) then
      Context.Advance;

    Param := ParseExpression;
    if Assigned(Param) then
      Result.AddChild(Param)
    else
    begin
      HandleError('Expected position after handle', Token);
      Exit;
    end;

    DoNodeCreated(Result);
    Exit;
  end;

  // Parse ALL parameters until end of statement (for other file commands)
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
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
    else if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
      Break;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseFileManagementStatement: TASTNode;
var
  Token: TLexerToken;
  Params: TASTNode;
  CmdName: string;
begin
  Token := Context.CurrentToken;
  CmdName := UpperCase(Token.Value);

  // Recognize file management commands
  case CmdName of
    'CATALOG', 'DIR', 'DIRECTORY': Result := TASTNode.Create(antCatalog, Token);
    'COPY', 'CP': Result := TASTNode.Create(antCopy, Token);
    'SCRATCH': Result := TASTNode.Create(antScratch, Token);
    'RENAME': Result := TASTNode.Create(antRenameFile, Token);
    'CONCAT': Result := TASTNode.Create(antConcat, Token);
    'MKDIR', 'MD': Result := TASTNode.Create(antMkdir, Token);
    'CHDIR', 'CD': Result := TASTNode.Create(antChdir, Token);
    'MOVE', 'MV': Result := TASTNode.Create(antMove, Token);
  else
    Result := TASTNode.Create(antStatement, Token);
  end;

  Context.Advance; // Consume file management command

  // Parse parameters
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
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
  HandleNode, VarNode: TASTNode;
  CmdName: string;
begin
  Token := Context.CurrentToken;
  CmdName := UpperCase(Token.Value);

  // Handle GET# and INPUT# commands
  if CmdName = kGETN then
  begin
    // GET# file, variable
    // Syntax: GET#1, A$ or GET# 1, A$
    Result := TASTNode.Create(antGetFile, Token);
    Context.Advance; // Consume GET#

    // Parse file handle
    if Context.Check(ttFileHandlePrefix) then
      Context.Advance;  // Consume # if present (shouldn't be after GET# but handle it)

    if Context.Check(ttNumber) or Context.Check(ttInteger) then
    begin
      HandleNode := TASTNode.CreateWithValue(antLiteral, StrToInt(Context.CurrentToken.Value), Context.CurrentToken);
      Result.AddChild(HandleNode);
      Context.Advance;
    end
    else if Context.Check(ttIdentifier) then
    begin
      HandleNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
      Result.AddChild(HandleNode);
      Context.Advance;
    end
    else
    begin
      HandleError('Expected file handle after GET#', Token);
      Exit;
    end;

    // Expect comma
    if Context.Check(ttSeparParam) then
      Context.Advance;

    // Parse variable (single variable for GET#)
    if Context.Check(ttIdentifier) then
    begin
      VarNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
      Result.AddChild(VarNode);
      Context.Advance;
    end
    else
    begin
      HandleError('Expected variable after GET# handle', Token);
      Exit;
    end;

    DoNodeCreated(Result);
    Exit;
  end
  else if CmdName = kINPUTN then
  begin
    // INPUT# file, variable [, variable ...]
    // Syntax: INPUT#1, A$, B, C
    Result := TASTNode.Create(antInputFile, Token);
    Context.Advance; // Consume INPUT#

    // Parse file handle
    if Context.Check(ttFileHandlePrefix) then
      Context.Advance;

    if Context.Check(ttNumber) or Context.Check(ttInteger) then
    begin
      HandleNode := TASTNode.CreateWithValue(antLiteral, StrToInt(Context.CurrentToken.Value), Context.CurrentToken);
      Result.AddChild(HandleNode);
      Context.Advance;
    end
    else if Context.Check(ttIdentifier) then
    begin
      HandleNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
      Result.AddChild(HandleNode);
      Context.Advance;
    end
    else
    begin
      HandleError('Expected file handle after INPUT#', Token);
      Exit;
    end;

    // Expect comma
    if Context.Check(ttSeparParam) then
      Context.Advance;

    // Parse variable list
    while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
    begin
      if Context.Check(ttIdentifier) then
      begin
        VarNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
        Result.AddChild(VarNode);
        Context.Advance;
      end
      else
        Break;

      // Handle comma separator
      if Context.Check(ttSeparParam) then
        Context.Advance
      else
        Break;
    end;

    if Result.ChildCount < 2 then
      HandleError('Expected at least one variable after INPUT# handle', Token);

    DoNodeCreated(Result);
    Exit;
  end;

  // Generic file input command handling (fallback)
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance;
  DoNodeCreated(Result);
end;

function TPackratParser.ParseLineInputStatement: TASTNode;
// FreeBASIC "LINE INPUT #n, var" — read a whole line from a file (commas are NOT field separators).
// Cursor is at the LINE identifier. Builds an antInputFile node tagged LINEINPUT.
var
  P: TASTNode;
  Tok: TLexerToken;
begin
  Tok := Context.CurrentToken;
  Context.Advance;  // LINE
  Context.Advance;  // INPUT
  if not (Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#')) then
  begin
    HandleError('LINE INPUT from the console is not yet supported (use LINE INPUT #n, var)', Tok);
    Result := TASTNode.Create(antStatement, Tok);
    Exit;
  end;
  Result := TASTNode.Create(antInputFile, Tok);
  Result.Attributes.Values['LINEINPUT'] := '1';
  Context.Advance;  // '#'
  if Context.Check(ttNumber) or Context.Check(ttInteger) then
  begin
    Result.AddChild(TASTNode.CreateWithValue(antLiteral, StrToInt(Context.CurrentToken.Value), Context.CurrentToken));
    Context.Advance;
  end
  else if Context.Check(ttIdentifier) then
  begin
    Result.AddChild(TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken));
    Context.Advance;
  end
  else
    HandleError('Expected file number after LINE INPUT #', Tok);
  if Context.CheckAny([ttSeparParam, ttSeparOutput]) then Context.Advance;  // comma after the handle
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
  begin
    P := ParseExpression;     // destination string variable
    if Assigned(P) then Result.AddChild(P) else Break;
    if Context.Check(ttSeparParam) then Context.Advance else Break;
  end;
  DoNodeCreated(Result);
end;

function TPackratParser.ParseWriteFileStatement: TASTNode;
// FreeBASIC "WRITE #n, v1, v2, ..." — CSV output (strings quoted). Cursor is at the WRITE identifier.
// Built as an antPrintFile node tagged WRITE; SSA (EmitWriteFileValues) does the formatting.
var
  P: TASTNode;
  Tok: TLexerToken;
begin
  Tok := Context.CurrentToken;
  Context.Advance;  // WRITE
  Result := TASTNode.Create(antPrintFile, Tok);
  Result.Attributes.Values['WRITE'] := '1';
  if Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#') then
    Context.Advance;  // '#'
  if Context.Check(ttNumber) or Context.Check(ttInteger) then
  begin
    Result.AddChild(TASTNode.CreateWithValue(antLiteral, StrToInt(Context.CurrentToken.Value), Context.CurrentToken));
    Context.Advance;
  end
  else if Context.Check(ttIdentifier) then
  begin
    Result.AddChild(TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken));
    Context.Advance;
  end
  else
    HandleError('Expected file number after WRITE #', Tok);
  if Context.CheckAny([ttSeparParam, ttSeparOutput]) then Context.Advance;  // comma after the handle
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
  begin
    P := ParseExpression;
    if Assigned(P) then Result.AddChild(P) else Break;
    if Context.Check(ttSeparParam) then Context.Advance else Break;
  end;
  DoNodeCreated(Result);
end;

function TPackratParser.ParseSeekStatement: TASTNode;
// FreeBASIC "SEEK #n, pos" — set the 1-based file position. Cursor is at the SEEK identifier. Reuses
// an antPrintFile node tagged SEEK (child0=handle, child1=position); SSA emits ssaSeekSet.
var
  P: TASTNode;
  Tok: TLexerToken;
begin
  Tok := Context.CurrentToken;
  Context.Advance;  // SEEK
  Result := TASTNode.Create(antPrintFile, Tok);
  Result.Attributes.Values['SEEK'] := '1';
  if Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#') then
    Context.Advance;  // '#'
  if Context.Check(ttNumber) or Context.Check(ttInteger) then
  begin
    Result.AddChild(TASTNode.CreateWithValue(antLiteral, StrToInt(Context.CurrentToken.Value), Context.CurrentToken));
    Context.Advance;
  end
  else if Context.Check(ttIdentifier) then
  begin
    Result.AddChild(TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken));
    Context.Advance;
  end
  else
    HandleError('Expected file number after SEEK #', Tok);
  if Context.CheckAny([ttSeparParam, ttSeparOutput]) then Context.Advance;  // comma
  P := ParseExpression;   // position (1-based)
  if Assigned(P) then Result.AddChild(P);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseFileOutputStatement: TASTNode;
var
  Token: TLexerToken;
  HandleNode, Expr: TASTNode;
  SeparatorNode: TASTNode;
  CmdName: string;
begin
  Token := Context.CurrentToken;
  CmdName := UpperCase(Token.Value);

  // Handle PRINT# command
  if CmdName = kPRINTN then
  begin
    // PRINT# file [, print list]
    // Syntax: PRINT#1, "Hello"; A$
    // Note: PRINT# alone (no data) can be used to close CMD redirection
    Result := TASTNode.Create(antPrintFile, Token);
    Context.Advance; // Consume PRINT#

    // Parse file handle
    if Context.Check(ttFileHandlePrefix) then
      Context.Advance;

    if Context.Check(ttNumber) or Context.Check(ttInteger) then
    begin
      HandleNode := TASTNode.CreateWithValue(antLiteral, StrToInt(Context.CurrentToken.Value), Context.CurrentToken);
      Result.AddChild(HandleNode);
      Context.Advance;
    end
    else if Context.Check(ttIdentifier) then
    begin
      HandleNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
      Result.AddChild(HandleNode);
      Context.Advance;
    end
    else
    begin
      HandleError('Expected file handle after PRINT#', Token);
      Exit;
    end;

    // Optional comma and print list
    // PRINT# without additional parameters is valid (used to reset CMD)
    if Context.Check(ttSeparParam) or Context.Check(ttSeparOutput) then
    begin
      Context.Advance; // Consume separator

      // Parse expressions (like PRINT statement)
      while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
      begin
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
          if Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
            Break;
        end
        else
          Break;
      end;
    end;

    DoNodeCreated(Result);
    Exit;
  end;

  // Generic file output command handling (fallback)
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance;
  DoNodeCreated(Result);
end;

function TPackratParser.ParseErrorHandlingStatement: TASTNode;
var
  Token: TLexerToken;
  Command: string;
  LineNumNode: TASTNode;
begin
  Token := Context.CurrentToken;
  Command := UpperCase(Token.Value);

  if Command = 'TRAP' then
  begin
    // TRAP linenum - set error handler line
    Result := TASTNode.Create(antTrap, Token);
    Context.Advance; // Consume TRAP

    // Parse the target line number expression
    LineNumNode := ParseExpression;
    if Assigned(LineNumNode) then
      Result.AddChild(LineNumNode);
  end
  else if Command = 'RESUME' then
  begin
    Context.Advance; // Consume RESUME

    // Check for NEXT keyword
    if Context.Check(ttLoopBlockEnd) and (UpperCase(Context.CurrentToken.Value) = 'NEXT') then
    begin
      Result := TASTNode.Create(antResumeNext, Token);
      Context.Advance; // Consume NEXT
    end
    // Check for line number (RESUME <line>)
    else if Context.Check(ttNumber) then
    begin
      Result := TASTNode.Create(antResume, Token);
      LineNumNode := ParseExpression;
      if Assigned(LineNumNode) then
        Result.AddChild(LineNumNode);
    end
    else
    begin
      // Plain RESUME - resume at error line
      Result := TASTNode.Create(antResume, Token);
    end;
  end
  else
  begin
    // Unknown error handling command - create generic statement
    Result := TASTNode.Create(antStatement, Token);
    Context.Advance;
  end;

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
  NodeType: TASTNodeType;
begin
  Token := Context.CurrentToken;
  // Determine if TRON or TROFF based on token value
  if UpperCase(Token.Value) = 'TRON' then
    NodeType := antTron
  else
    NodeType := antTroff;
  Result := TASTNode.Create(NodeType, Token);
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
  if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
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
  KeyNumExpr, KeyTextExpr: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antKey, Token);
  Context.Advance; // Consume KEY

  // KEY without arguments lists all key definitions
  // KEY n, "text" defines function key n
  if not Context.IsAtEnd and not Context.Check(ttEndOfLine) and not Context.Check(ttSeparStmt) then
  begin
    // Parse key number
    KeyNumExpr := ParseExpression;
    if Assigned(KeyNumExpr) then
      Result.AddChild(KeyNumExpr);

    // Expect comma and text
    if Context.Check(ttSeparParam) then
    begin
      Context.Advance; // Consume comma
      KeyTextExpr := ParseExpression;
      if Assigned(KeyTextExpr) then
        Result.AddChild(KeyTextExpr);
    end;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseDirectiveStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume directive keyword

  // OPTION directive: consume option name argument
  if Assigned(Token.KeywordInfo) and (Token.KeywordInfo.Keyword = kOPTION) then
  begin
    if Assigned(Context.CurrentToken) and (Context.CurrentToken.TokenType = ttIdentifier) then
      Context.Advance;
  end;

  DoNodeCreated(Result);
end;

{$IFDEF WEB_MODE}
function TPackratParser.ParseWebStatement: TASTNode;
var
  Token: TLexerToken;
  NameExpr, ValueExpr, StatusExpr: TASTNode;
  KeywordUpper: string;
begin
  Token := Context.CurrentToken;
  KeywordUpper := UpperCase(Token.Value);

  // SETHEADER name, value
  if KeywordUpper = kSETHEADER then
  begin
    Result := TASTNode.Create(antWebCommand, Token);
    Result.Value := kSETHEADER;
    Context.Advance; // Consume SETHEADER

    // Parse header name (string expression)
    NameExpr := ParseExpression;
    if not Assigned(NameExpr) then
    begin
      HandleError('Expected header name after SETHEADER', Context.CurrentToken);
      Result.Free;
      Result := nil;
      Exit;
    end;
    Result.AddChild(NameExpr);

    // Expect comma
    if not Context.Match(ttSeparParam) then
    begin
      HandleError('Expected "," after header name', Context.CurrentToken);
      Result.Free;
      Result := nil;
      Exit;
    end;

    // Parse header value (string expression)
    ValueExpr := ParseExpression;
    if not Assigned(ValueExpr) then
    begin
      HandleError('Expected header value after ","', Context.CurrentToken);
      Result.Free;
      Result := nil;
      Exit;
    end;
    Result.AddChild(ValueExpr);

    DoNodeCreated(Result);
    Exit;
  end;

  // STATUS code
  if KeywordUpper = kSTATUS then
  begin
    Result := TASTNode.Create(antWebCommand, Token);
    Result.Value := kSTATUS;
    Context.Advance; // Consume STATUS

    // Parse status code (numeric expression)
    StatusExpr := ParseExpression;
    if not Assigned(StatusExpr) then
    begin
      HandleError('Expected status code after STATUS', Context.CurrentToken);
      Result.Free;
      Result := nil;
      Exit;
    end;
    Result.AddChild(StatusExpr);

    DoNodeCreated(Result);
    Exit;
  end;

  // Unknown web command
  HandleError(Format('Unknown web command: %s', [Token.Value]), Token);
  Result := nil;
end;
{$ENDIF}

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
  Token, TypeTok: TLexerToken;
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

  // Optional "AS typename" (M3.1): array of UDT (or explicitly-typed array). Attached as a
  // 3rd child antIdentifier(typename); SSA treats a UDT element type as an int handle array.
  if Context.Check(ttAsType) then
  begin
    Context.Advance;                                // AS
    if Context.Check(ttIdentifier) then
    begin
      TypeTok := Context.CurrentToken;
      Result.AddChild(TASTNode.CreateWithValue(antIdentifier,
                   ParseDottedName, TypeTok));        // dotted: namespace-qualified element type
      // FreeBASIC fixed-length string array: "AS STRING * n" / "AS WSTRING * n" (advisory in v1).
      if Context.Check(ttOpMul) then
      begin
        Context.Advance;                              // '*'
        FExpressionParser.ParseExpression(precCall).Free;   // length operand (discarded)
      end;
    end;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseDimensionList: TASTNode;
var
  Dimension, UpperExpr, RangeNode: TASTNode;
begin
  Result := TASTNode.Create(antDimensions);

  repeat
    Dimension := ParseExpression;
    if not Assigned(Dimension) then Break;
    // FreeBASIC explicit bound "lb TO ub": the first expression is the lower bound. Wrap both in an
    // antDimRange (child0=lb, child1=ub). A bare expression stays the upper bound (lower bound = 0).
    if Context.Check(ttLoopControl) and (UpperCase(Context.CurrentToken.Value) = kTO) then
    begin
      Context.Advance;                              // consume TO
      UpperExpr := ParseExpression;
      if not Assigned(UpperExpr) then
      begin
        HandleError('Expected an upper bound after TO in array dimension', Context.CurrentToken);
        Dimension.Free;
        Break;
      end;
      RangeNode := TASTNode.Create(antDimRange, Context.CurrentToken);
      RangeNode.AddChild(Dimension);
      RangeNode.AddChild(UpperExpr);
      Result.AddChild(RangeNode);
    end
    else
      Result.AddChild(Dimension);

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
  Token, NameTok, TypeTok: TLexerToken;
  ArrayDecl, VarNameNode, TypeNode, CtorArgs, ArgExpr, InitExpr: TASTNode;
  IsShared: Boolean;
  DimTypeName: string;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antDim, Token);
  Context.Advance; // Consume DIM
  // M6: "DIM SHARED ..." — the declared variables are module globals visible (read/write) inside
  // SUB/FUNCTION bodies. Marked on each decl with the 'SHARED' attribute for the SSA pre-scan.
  IsShared := Context.Check(ttSharedDecl);
  if IsShared then Context.Advance;   // consume SHARED

  // Parse declarations separated by commas. Each is either:
  //   name AS typename   -> typed scalar (UDT record or explicit builtin type)
  //   name ( dims )      -> array (classic)
  repeat
    if Context.Check(ttIdentifier) and Assigned(Context.PeekNext) and
       (Context.PeekNext.TokenType = ttAsType) then
    begin
      // "name AS typename"
      NameTok := Context.CurrentToken;
      Context.Advance;                       // name
      Context.Advance;                       // AS
      if not Context.Check(ttIdentifier) then
      begin
        HandleError('Expected type name after AS', Context.CurrentToken);
        Break;
      end;
      TypeTok := Context.CurrentToken;
      DimTypeName := ParseDottedName;          // dotted: namespace-qualified type ("Forms.Point")
      // FreeBASIC pointer type: "<type> PTR" (one or more PTR). A pointer is stored as an int handle
      // (the address); the suffix is kept on the type name so the SSA records the pointee bank.
      while Context.Check(ttIdentifier) and (UpperCase(Context.CurrentToken.Value) = 'PTR') do
      begin
        DimTypeName := DimTypeName + ' PTR';
        Context.Advance;                       // consume PTR
      end;
      ArrayDecl := TASTNode.Create(antArrayDecl, NameTok);
      VarNameNode := TASTNode.CreateWithValue(antIdentifier, UpperCase(NameTok.Value), NameTok);
      TypeNode := TASTNode.CreateWithValue(antIdentifier, DimTypeName, TypeTok);
      ArrayDecl.AddChild(VarNameNode);
      ArrayDecl.AddChild(TypeNode);          // child[1] is antIdentifier (type) => typed scalar
      // FreeBASIC fixed-length string: "AS STRING * n" / "AS WSTRING * n" / "AS ZSTRING * n". The
      // declared capacity is parsed and recorded (attribute 'FIXEDLEN', advisory in v1 — storage is
      // variable-length). Consume "* <length-expr>" so the declaration parses cleanly.
      if Context.Check(ttOpMul) then
      begin
        Context.Advance;                     // consume '*'
        InitExpr := FExpressionParser.ParseExpression(precCall);   // length operand (no binary ops)
        if Assigned(InitExpr) then
        begin
          ArrayDecl.Attributes.Values['FIXEDLEN'] := '1';
          InitExpr.Free;                     // advisory: capacity not stored as a child in v1
        end;
      end;
      // Optional initializer after the type. Two cases:
      //   = T(args)   (M4.4c) constructor call on the declared type — consume "= T" and let the
      //               shared arg-parsing block below attach the antArgumentList as child[2].
      //   = expr      (M4.4e) general initializer — parse the expression and attach it as child[2];
      //               SSA emits an assignment (scalar store / UDT value-copy) after construction.
      if Context.Check(ttOpEq) then
      begin
        Context.Advance;                     // =
        if Context.Check(ttIdentifier) and
           (UpperCase(Context.CurrentToken.Value) = DimTypeName) then
          Context.Advance                    // RHS == declared type: ctor form (block below reads '(')
        else
        begin
          InitExpr := FExpressionParser.ParseExpression;    // general initializer expression
          if Assigned(InitExpr) then
            ArrayDecl.AddChild(InitExpr);                   // child[2] = initializer (not antArgumentList)
        end;
      end;
      // Optional parameterised construction (M4.4b): attach the constructor argument list as
      // child[2]; SSA stages these and calls T's matching CONSTRUCTOR.
      if Context.Check(ttDelimParOpen) then
      begin
        Context.Advance;                     // (
        CtorArgs := TASTNode.Create(antArgumentList, Context.CurrentToken);
        if not Context.Check(ttDelimParClose) then
          repeat
            ArgExpr := FExpressionParser.ParseExpression;
            if not Assigned(ArgExpr) then Break;
            CtorArgs.AddChild(ArgExpr);
            if Context.Check(ttSeparParam) then Context.Advance else Break;
          until Context.CheckAny([ttDelimParClose, ttEndOfLine, ttEndOfFile]);
        if Context.Check(ttDelimParClose) then Context.Advance;   // )
        ArrayDecl.AddChild(CtorArgs);        // child[2] = antArgumentList (ctor args)
      end;
      if IsShared then ArrayDecl.Attributes.Values['SHARED'] := '1';   // M6: module-global scalar
      DoNodeCreated(ArrayDecl);
      Result.AddChild(ArrayDecl);
    end
    else
    begin
      ArrayDecl := ParseArrayDeclaration;
      if Assigned(ArrayDecl) then
      begin
        if IsShared then ArrayDecl.Attributes.Values['SHARED'] := '1';
        Result.AddChild(ArrayDecl);
      end
      else
      begin
        HandleError('Expected array declaration after DIM', Context.CurrentToken);
        Break;
      end;
    end;

    // Check for comma separator
    if Context.Check(ttSeparParam) then
      Context.Advance // Consume comma
    else
      Break; // No more declarations

  until Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]);

  DoNodeCreated(Result);
end;

function TPackratParser.ParseEraseStatement: TASTNode;
// ERASE arr [, arr ...] (FreeBASIC, B1.4) - reset each named array's elements to default.
var
  Token, NameTok: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antErase, Token);
  Context.Advance; // Consume ERASE
  repeat
    if not Context.Check(ttIdentifier) then
    begin
      HandleError('Expected array name after ERASE', Context.CurrentToken);
      Break;
    end;
    NameTok := Context.CurrentToken;
    Result.AddChild(TASTNode.CreateWithValue(antIdentifier, UpperCase(NameTok.Value), NameTok));
    Context.Advance;                     // array name
    if Context.Check(ttSeparParam) then
      Context.Advance                    // comma -> another array
    else
      Break;
  until Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseRedimStatement: TASTNode;
// REDIM [PRESERVE] arr(dims) [, arr(dims) ...] (FreeBASIC, B1.4) - re-dimension arrays.
var
  Token: TLexerToken;
  ArrayDecl: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antRedim, Token);
  Context.Advance; // Consume REDIM
  // Optional PRESERVE modifier (not a reserved keyword -> arrives as an identifier).
  if Context.Check(ttIdentifier) and (UpperCase(Context.CurrentToken.Value) = 'PRESERVE') then
  begin
    Result.Attributes.Values['PRESERVE'] := '1';
    Context.Advance;
  end;
  repeat
    ArrayDecl := ParseArrayDeclaration;  // name(dims) [AS type]
    if Assigned(ArrayDecl) then
      Result.AddChild(ArrayDecl)
    else
    begin
      HandleError('Expected array declaration after REDIM', Context.CurrentToken);
      Break;
    end;
    if Context.Check(ttSeparParam) then
      Context.Advance                    // comma -> another array
    else
      Break;
  until Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseSwapStatement: TASTNode;
// SWAP a, b (FreeBASIC) - exchange the values of two lvalues. Each operand is a
// full lvalue expression (scalar, array element, UDT member); the SSA stage snapshots
// one value into a temp and reuses ProcessAssignment for the cross-store.
var
  Token: TLexerToken;
  Left, Right: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antSwap, Token);
  Context.Advance; // Consume SWAP
  Left := ParseExpression;
  if not Assigned(Left) then
  begin
    HandleError('Expected variable after SWAP', Context.CurrentToken);
    DoNodeCreated(Result);
    Exit;
  end;
  Result.AddChild(Left);
  if not Context.Check(ttSeparParam) then
  begin
    HandleError('Expected comma between SWAP operands', Context.CurrentToken);
    DoNodeCreated(Result);
    Exit;
  end;
  Context.Advance; // comma
  Right := ParseExpression;
  if not Assigned(Right) then
  begin
    HandleError('Expected second variable in SWAP', Context.CurrentToken);
    DoNodeCreated(Result);
    Exit;
  end;
  Result.AddChild(Right);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseLRSetStatement(NodeType: TASTNodeType): TASTNode;
// LSET/RSET dst (= | ,) src  - justify src into dst's string buffer (dst's length is preserved).
// Both the QBasic ("dst = src") and FreeBASIC ("dst, src") separators are accepted.
// AST children: child0 = dst lvalue, child1 = src expression.
var
  Token: TLexerToken;
  Dst, Src: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(NodeType, Token);
  Context.Advance; // consume LSET / RSET
  // Parse the destination as an lvalue (identifier / array element / member). Use precCall so the
  // expression parser stops before "=" (which would otherwise be read as an equality operator in the
  // QBasic "dst = src" form).
  Dst := FExpressionParser.ParseExpression(precCall);
  if not Assigned(Dst) then
  begin
    HandleError('Expected destination variable after LSET/RSET', Context.CurrentToken);
    DoNodeCreated(Result);
    Exit;
  end;
  Result.AddChild(Dst);
  if Context.Check(ttOpEq) or Context.Check(ttSeparParam) then
    Context.Advance                      // "=" (QBasic) or "," (FreeBASIC)
  else
  begin
    HandleError('Expected "=" or "," after LSET/RSET destination', Context.CurrentToken);
    DoNodeCreated(Result);
    Exit;
  end;
  Src := ParseExpression;
  if not Assigned(Src) then
  begin
    HandleError('Expected source string in LSET/RSET', Context.CurrentToken);
    DoNodeCreated(Result);
    Exit;
  end;
  Result.AddChild(Src);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseMidStatement: TASTNode;
// MID(target, start [, len]) = source  (FreeBASIC, MODERN): overwrite a substring of target
// in place. Returns nil (no error) if the trailing "=" is absent, so the caller can fall back
// to an expression/assignment. AST children: target, start, [len,] source.
var
  Token: TLexerToken;
  TargetNode, StartNode, LenNode, SourceNode: TASTNode;
begin
  Token := Context.CurrentToken;
  Context.Advance;                       // consume MID
  if not Context.Match(ttDelimParOpen) then Exit(nil);
  TargetNode := FExpressionParser.ParseExpression;
  if not Assigned(TargetNode) then Exit(nil);
  if not Context.Match(ttSeparParam) then begin TargetNode.Free; Exit(nil); end;
  StartNode := FExpressionParser.ParseExpression;
  if not Assigned(StartNode) then begin TargetNode.Free; Exit(nil); end;
  LenNode := nil;
  if Context.Match(ttSeparParam) then    // optional length
  begin
    LenNode := FExpressionParser.ParseExpression;
    if not Assigned(LenNode) then begin TargetNode.Free; StartNode.Free; Exit(nil); end;
  end;
  if not Context.Match(ttDelimParClose) then
  begin
    TargetNode.Free; StartNode.Free;
    if Assigned(LenNode) then LenNode.Free;
    Exit(nil);
  end;
  // The "=" decides this is the MID statement (not a MID(...) expression).
  if not Context.Match(ttOpEq) then
  begin
    TargetNode.Free; StartNode.Free;
    if Assigned(LenNode) then LenNode.Free;
    Exit(nil);
  end;
  SourceNode := FExpressionParser.ParseExpression;
  if not Assigned(SourceNode) then
  begin
    TargetNode.Free; StartNode.Free;
    if Assigned(LenNode) then LenNode.Free;
    Exit(nil);
  end;
  Result := TASTNode.Create(antMidStatement, Token);
  Result.AddChild(TargetNode);
  Result.AddChild(StartNode);
  if Assigned(LenNode) then Result.AddChild(LenNode);
  Result.AddChild(SourceNode);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseEnumStatement: TASTNode;
// ENUM [typename] / member [= expr] / ... / END ENUM  (FreeBASIC named integer constants).
// Desugared to a sequence of assignments (like CONST): a member with no "= expr" takes the
// previous member + 1 (the first defaults to 0). Children of the antEnum node are antAssignment.
var
  Token: TLexerToken;
  MemberName, PrevMember: string;
  ValueNode, AsnNode: TASTNode;
  IsFirst: Boolean;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antEnum, Token);
  Context.Advance;                              // consume ENUM
  // Optional enum type name on the header line (unused). Skip whatever token it is — it may be a
  // reserved word (e.g. a type/colour keyword), so don't require ttIdentifier.
  if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttSeparParam, ttEndOfFile]) then
    Context.Advance;
  while Context.CheckAny([ttEndOfLine, ttSeparStmt, ttSeparParam]) do Context.Advance;

  PrevMember := '';
  IsFirst := True;
  while not Context.Check(ttEndOfFile) do
  begin
    if Context.Check(ttProgramEnd) then        // END [ENUM] terminator
    begin
      Context.Advance;
      if Context.Check(ttEnum) then Context.Advance;
      Break;
    end;
    if not Context.Check(ttIdentifier) then
    begin
      if Context.CheckAny([ttEndOfLine, ttSeparStmt, ttSeparParam]) then
      begin Context.Advance; Continue; end;
      HandleError('Expected enum member name', Context.CurrentToken);
      Break;
    end;
    MemberName := UpperCase(Context.CurrentToken.Value);
    Context.Advance;
    if Context.Match(ttOpEq) then
      ValueNode := FExpressionParser.ParseExpression
    else if IsFirst then
      ValueNode := CreateLiteralNode(0, Token)
    else
      ValueNode := CreateBinaryOpNode(ttOpAdd,
        TASTNode.CreateWithValue(antIdentifier, PrevMember, Token),
        CreateLiteralNode(1, Token),
        TLexerToken.CreateSimple(ttOpAdd, '+'));
    if not Assigned(ValueNode) then ValueNode := CreateLiteralNode(0, Token);
    AsnNode := TASTNode.Create(antAssignment, Token);
    AsnNode.AddChild(TASTNode.CreateWithValue(antIdentifier, MemberName, Token));
    AsnNode.AddChild(ValueNode);
    Result.AddChild(AsnNode);
    PrevMember := MemberName;
    IsFirst := False;
    while Context.CheckAny([ttEndOfLine, ttSeparStmt, ttSeparParam]) do Context.Advance;
  end;
  DoNodeCreated(Result);
end;

function TPackratParser.ParseDefTypeStatement: TASTNode;
// DEFINT/DEFLNG/DEFSNG/DEFDBL/DEFSTR... letter-ranges  (FreeBASIC): set the default type of
// variables whose name starts with one of the given initials (when they have no suffix / explicit
// type). Stored as Value=bank (0=int,1=float,2=string) + attribute LETTERS = the covered initials.
var
  Token: TLexerToken;
  Bank: Integer;
  Letters: string;
  c1, c2, c: Char;
  KwU: string;
begin
  Token := Context.CurrentToken;
  KwU := UpperCase(Token.Value);
  if KwU = 'DEFSTR' then Bank := 2
  else if (KwU = 'DEFSNG') or (KwU = 'DEFDBL') then Bank := 1
  else Bank := 0;   // DEFINT/DEFLNG/DEFBYTE/DEFSHORT/DEFLNGINT -> int bank
  Result := TASTNode.CreateWithValue(antDefType, Bank, Token);
  Context.Advance;  // consume DEFxxx
  Letters := '';
  while Context.Check(ttIdentifier) and (Length(Context.CurrentToken.Value) > 0) do
  begin
    c1 := UpCase(Context.CurrentToken.Value[1]);
    Context.Advance;
    c2 := c1;
    if Context.Check(ttOpSub) then           // a range "A-Z"
    begin
      Context.Advance;
      if Context.Check(ttIdentifier) and (Length(Context.CurrentToken.Value) > 0) then
      begin
        c2 := UpCase(Context.CurrentToken.Value[1]);
        Context.Advance;
      end;
    end;
    if c1 <= c2 then
      for c := c1 to c2 do
        if (c >= 'A') and (c <= 'Z') then Letters := Letters + c;
    if Context.Check(ttSeparParam) then Context.Advance else Break;
  end;
  Result.Attributes.Values['LETTERS'] := Letters;
  DoNodeCreated(Result);
end;

function TPackratParser.ParseDefStatement: TASTNode;
var
  Token, FnToken: TLexerToken;
  FnName: string;
  ParamNode, ExprNode, NameNode, ParamListNode: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antDef, Token);
  Context.Advance; // Consume DEF

  // Format: DEF FNname(param) = expression
  // Two syntaxes supported:
  // 1. DEF FN NAME(X) = ... (FN as separate keyword)
  // 2. DEF FNNAME(X) = ...  (FNNAME as single identifier)

  if Context.Check(ttProcedureStart) then
  begin
    // Syntax 1: FN is a separate keyword
    FnToken := Context.CurrentToken;
    Context.Advance; // Consume FN

    if not Context.Check(ttIdentifier) then
    begin
      HandleError('Expected function name after DEF FN', FnToken);
      Result.Free;
      Result := nil;
      Exit;
    end;

    FnName := Context.CurrentToken.Value;
  end
  else if Context.Check(ttIdentifier) then
  begin
    // Syntax 2: FNNAME as single identifier (e.g., FNSQ, FNDB)
    FnName := Context.CurrentToken.Value;

    // Validate that it starts with FN
    if (Length(FnName) < 3) or (UpperCase(Copy(FnName, 1, 2)) <> 'FN') then
    begin
      HandleError('Expected FN or FNname after DEF', Token);
      Result.Free;
      Result := nil;
      Exit;
    end;

    // Extract the actual function name part (after FN)
    FnName := Copy(FnName, 3, Length(FnName) - 2);
  end
  else
  begin
    HandleError('Expected FN or FNname after DEF', Token);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Create function name node
  NameNode := TASTNode.CreateWithValue(antIdentifier, FnName, Context.CurrentToken);
  Result.AddChild(NameNode);
  Context.Advance; // Consume function name (or FNNAME identifier)

  // Expect opening parenthesis for parameter
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected ( after function name', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse parameter (single variable name)
  ParamListNode := TASTNode.Create(antDimensions, nil); // Reuse dimensions for param list
  if Context.Check(ttIdentifier) then
  begin
    ParamNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
    ParamListNode.AddChild(ParamNode);
    Context.Advance; // Consume parameter name
  end;
  Result.AddChild(ParamListNode);

  // Expect closing parenthesis
  if not Context.Match(ttDelimParClose) then
  begin
    HandleError('Expected ) after parameter', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Expect = sign
  if not Context.Match(ttOpEq) then
  begin
    HandleError('Expected = after parameter list', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse the function body expression
  ExprNode := ParseExpression;
  if Assigned(ExprNode) then
    Result.AddChild(ExprNode)
  else
  begin
    HandleError('Expected expression after =', Context.CurrentToken);
    Result.Free;
    Result := nil;
    Exit;
  end;

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
  DataItem: TASTNode;
begin
  Token := Context.CurrentToken;

  // Check if this is actually RESTORE (both DATA and RESTORE use ttDataConstant)
  if SameText(Token.Value, 'RESTORE') then
  begin
    Result := ParseRestoreStatement;
    Exit;
  end;

  Result := TASTNode.Create(antData, Token);
  Context.Advance; // Consume DATA

  // Parse comma-separated list of data items (literals only - no expressions)
  // Format: DATA 5,12,1,34,18 or DATA "hello","world" or DATA COMMODORE,128
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
  begin
    // Parse data item - can be number, string, or unquoted identifier (treated as string)
    if Context.Check(ttNumber) or Context.Check(ttInteger) or Context.Check(ttFloat) then
    begin
      DataItem := TASTNode.CreateWithValue(antLiteral, Context.CurrentToken.Value, Context.CurrentToken);
      Result.AddChild(DataItem);
      Context.Advance;
    end
    else if Context.Check(ttStringLiteral) then
    begin
      DataItem := TASTNode.CreateWithValue(antLiteral, Context.CurrentToken.Value, Context.CurrentToken);
      Result.AddChild(DataItem);
      Context.Advance;
    end
    else if Context.Check(ttIdentifier) then
    begin
      // Unquoted identifier in DATA is treated as string literal
      DataItem := TASTNode.CreateWithValue(antLiteral, Context.CurrentToken.Value, Context.CurrentToken);
      Result.AddChild(DataItem);
      Context.Advance;
    end
    else if Context.Check(ttOpSub) then
    begin
      // Handle negative numbers: -5, -12.5
      Context.Advance; // Consume -
      if Context.Check(ttNumber) or Context.Check(ttInteger) or Context.Check(ttFloat) then
      begin
        DataItem := TASTNode.CreateWithValue(antLiteral, -StrToFloat(Context.CurrentToken.Value), Context.CurrentToken);
        Result.AddChild(DataItem);
        Context.Advance;
      end;
    end
    else
      Break;

    // Check for comma separator
    if Context.Check(ttSeparParam) then
      Context.Advance // Consume comma and continue
    else
      Break; // No more items
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseReadStatement: TASTNode;
var
  Token: TLexerToken;
  VarNode: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antRead, Token);
  Context.Advance; // Consume READ

  // Parse comma-separated list of variables
  // Format: READ X or READ A$,B,C or READ A(I),B$
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
  begin
    if Context.Check(ttIdentifier) then
    begin
      // Check if it's an array access
      if Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttDelimParOpen) then
      begin
        VarNode := ParseArrayAccess;
      end
      else
      begin
        // Simple variable
        VarNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
        Context.Advance;
      end;
      Result.AddChild(VarNode);
    end
    else
      Break;

    // Check for comma separator
    if Context.Check(ttSeparParam) then
      Context.Advance // Consume comma and continue
    else
      Break;
  end;

  if Result.ChildCount = 0 then
    HandleError('Expected at least one variable in READ statement', Token);

  DoNodeCreated(Result);
end;

function TPackratParser.ParseRestoreStatement: TASTNode;
var
  Token: TLexerToken;
  LineNode: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antRestore, Token);
  Context.Advance; // Consume RESTORE

  // Optional line number: RESTORE or RESTORE 100
  if Context.Check(ttNumber) or Context.Check(ttInteger) then
  begin
    LineNode := TASTNode.CreateWithValue(antLiteral, Context.CurrentToken.Value, Context.CurrentToken);
    Result.AddChild(LineNode);
    Context.Advance;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseClearStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antClear, Token);
  Context.Advance; // Consume CLR
  DoNodeCreated(Result);
end;

function TPackratParser.ParseStopStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStop, Token);
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
  CmdName: string;
begin
  Token := Context.CurrentToken;
  CmdName := UpperCase(Token.Value);

  // Recognize specific program editing commands
  case CmdName of
    'LIST': Result := TASTNode.Create(antList, Token);
    'NEW': Result := TASTNode.Create(antNew, Token);
    'DELETE': Result := TASTNode.Create(antDelete, Token);
    'RENUMBER': Result := TASTNode.Create(antRenumber, Token);
  else
    Result := TASTNode.Create(antStatement, Token);
  end;

  Context.Advance; // Consume command

  // Special handling for DELETE: parse line range (e.g., 10-50, -100, 100-)
  if CmdName = 'DELETE' then
  begin
    // Check for leading minus (DELETE -100 means delete up to line 100)
    if Context.Check(ttOpSub) then
    begin
      Context.Advance; // Consume '-'
      // Create a node with value 0 for start (meaning "from beginning")
      Param := TASTNode.Create(antLiteral, Context.CurrentToken);
      Param.Value := 0;
      Result.AddChild(Param);
      // Parse end line number
      if Context.Check(ttNumber) then
      begin
        Param := TASTNode.Create(antLiteral, Context.CurrentToken);
        Param.Value := StrToIntDef(Context.CurrentToken.Value, 0);
        Result.AddChild(Param);
        Context.Advance;
      end;
    end
    else if Context.Check(ttNumber) then
    begin
      // Parse start line number
      Param := TASTNode.Create(antLiteral, Context.CurrentToken);
      Param.Value := StrToIntDef(Context.CurrentToken.Value, 0);
      Result.AddChild(Param);
      Context.Advance;
      // Check for range separator '-'
      if Context.Check(ttOpSub) then
      begin
        Context.Advance; // Consume '-'
        // Check if there's an end number or just trailing '-' (DELETE 100-)
        if Context.Check(ttNumber) then
        begin
          Param := TASTNode.Create(antLiteral, Context.CurrentToken);
          Param.Value := StrToIntDef(Context.CurrentToken.Value, 0);
          Result.AddChild(Param);
          Context.Advance;
        end
        else
        begin
          // DELETE 100- means delete from 100 to end, use -1 as marker
          Param := TASTNode.Create(antLiteral, Context.CurrentToken);
          Param.Value := -1;
          Result.AddChild(Param);
        end;
      end;
      // If no '-', it's a single line delete (only start child exists)
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  // Parse ALL parameters until end of statement (for LIST, RENUMBER, etc.)
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
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
    else if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
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

function TPackratParser.BuildSourceLine(AContext: TParserContext): string;
var
  StartIndex, i: Integer;
  Token: TLexerToken;
begin
  // Build source line from tokens until end of line
  Result := '';
  StartIndex := AContext.CurrentIndex;

  // Scan tokens until EOL
  i := StartIndex;
  while i < AContext.TokenList.Count do
  begin
    Token := AContext.TokenList.GetTokenDirect(i);
    if not Assigned(Token) or (Token.TokenType in [ttEndOfLine, ttEndOfFile]) then
      Break;

    if Result <> '' then
      Result := Result + ' ';
    Result := Result + Token.Value;
    Inc(i);
  end;
end;

function TPackratParser.PeekForElseOnNextLine: Boolean;
var
  SavedIndex: Integer;
begin
  // Peek ahead past EOLs and line numbers to see if ELSE follows.
  // Used after THEN BEGIN...BEND to allow ELSE on a different BASIC line.
  Result := False;
  Context.SavePosition(SavedIndex);
  try
    // Skip past any EOL tokens (blank lines)
    while Context.Check(ttEndOfLine) do
      Context.Advance;
    // Skip line number if present
    if Context.Check(ttLineNumber) then
      Context.Advance;
    // Skip statement separators
    while Context.Check(ttSeparStmt) do
      Context.Advance;
    // Check if we're at ELSE
    Result := Context.Check(ttConditionalElse);
  finally
    Context.RestorePosition(SavedIndex);
  end;
end;

procedure TPackratParser.PopCompletedIfsAtEOL;
var
  CurrentIfEntry: TIfStackEntry;
begin
  // Pop completed IFs at end of line.
  // For IFs with completed THEN blocks (BEGIN...BEND), peek ahead for ELSE
  // on the next line before popping.
  while FValidationStacks.HasActiveIf do
  begin
    if FValidationStacks.CanPopIfAtEOL then
      FValidationStacks.PopIf
    else
    begin
      // Can't pop — check if it's a completed THEN block waiting for ELSE
      CurrentIfEntry := FValidationStacks.GetCurrentIf;
      if CurrentIfEntry.HasThenBlock and not CurrentIfEntry.HasElse then
      begin
        // THEN had a BEGIN block. Peek for ELSE on the next line.
        if PeekForElseOnNextLine then
          Break  // ELSE is coming — keep IF on stack
        else
        begin
          // No ELSE coming — clear block flag and pop
          FValidationStacks.ClearThenBlockForCurrentIf;
          if FValidationStacks.CanPopIfAtEOL then
            FValidationStacks.PopIf
          else
            Break;
        end;
      end
      else
        Break; // Can't pop for other reasons (active block, etc.)
    end;
  end;
end;

function TPackratParser.ParseBlockUntil(EndTokens: array of TTokenType): TASTNode;
var
  Statement: TASTNode;
  Token: TLexerToken;
  i: Integer;
  Found: Boolean;
  StartIndex: Integer;
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
    begin
      // After skipping line number, re-check for end tokens
      Token := Context.CurrentToken;
      if Assigned(Token) then
      begin
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
      end;
      Continue;
    end;

    // Parse statement within block
    // Remember position to detect if parsing made progress
    StartIndex := Context.CurrentIndex;
    Statement := ParseStatement;
    if Assigned(Statement) then
      Result.AddChild(Statement)
    else
    begin
      // ParseStatement returned nil - this is OK if it consumed tokens
      // (e.g., THEN/ELSE which add themselves to parent IF)
      // Only break if no progress was made (stuck on same token)
      if Context.CurrentIndex = StartIndex then
        Break;
      // Otherwise, continue - the statement was handled internally
    end;
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
