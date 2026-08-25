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
  SedaiExecutorTypes, SedaiBasicKeywords,
  SedaiExecutorErrors;  // runtime error codes for the CLASSIC orphan LOOP/WEND/UNTIL raises

type
  // Dialect selection for the parser. pdAuto (default) detects the dialect from the token
  // stream at each Parse (line numbers => CLASSIC, none => MODERN). pdModern/pdClassic force
  // it explicitly and survive subsequent Parse calls — set these from NEW MODERN / NEW CLASSIC,
  // OPTION MODE MODERN / OPTION MODE CLASSIC; use SetDialect(pdAuto) after LOAD to re-detect.
  TParserDialect = (pdAuto, pdModern, pdClassic);

  // Constant per-dimension element counts of an array declaration (for zero-padding a jagged
  // multi-dimensional aggregate initializer). Empty when some bound is not a compile-time constant.
  TDimSizeArray = array of Integer;

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
    FOptionBase: Integer;              // "OPTION BASE n": default lower bound for a bare-upper-bound array DIM (0 or 1)
    { "OPTION DIGITS n": significant digits for a float in PRINT. 0 = the
      directive was not used, so the dialect default stands. Unlike OPTION BASE
      this one has no effect at PARSE time - it is read out by the caller and
      handed to the runtime, because it changes how a value is SHOWN, not how
      the program is built. }
    FOptionDigits: Integer;
    // Constant capacity of the last "As String * n" TYPE-field type parsed (0 = none). Set by
    // ParseRecordFieldType, consumed when that field's node is built.
    FLastFieldFixedLen: Integer;
    FInitLevelSizes: array of Integer; // array initializer: item count per brace-nesting level (for "..." ellipsis dims)
    FDialectOverride: TParserDialect;  // pdAuto = detect per-Parse; pdModern/pdClassic = forced
    // Number of ParseDoStatement body parses currently on the call stack. A bare LOOP that
    // reaches ParseLoopEndStatement while this is > 0 sits inside a nested construct of an
    // open DO (single-line IF branch): in CLASSIC it is that loop's back-edge, not an error.
    FDoParseDepth: Integer;
    // Names declared with CONST (UPPER). A constant is not an lvalue: assigning to one is an error
    // in FreeBASIC ("error 119: Cannot modify a constant") and used to be silently accepted here,
    // because a module-level CONST lowers to a DIM and was therefore an ordinary variable.
    FConstNames: TStringList;
    // ...and what each one's inferred TYPE NAME is, so a CONST defined from another CONST can be
    // typed by its VALUE instead of falling back to the numeric default. See InferConstTypeName.
    FConstTypes: TStringList;
    // ⭐ The VALUE of a CONST that folds to an integer, keyed by name. It exists for the FIXED-LENGTH
    // CAPACITY of a string declaration, which FreeBASIC routinely writes as a CONST ("f As ZString *
    // MAXLEN") or an expression over one ("* TOTLEN+1"): TryConstIntExpr knew literals and arithmetic
    // and NOT names, so such a declaration recorded "capacity present but unknown" and every question
    // about the SIZE of the field fell back to the 24-byte string descriptor - SizeOf answered 24
    // where fbc answers 4, and the whole type's layout with it.
    FConstIntValues: TStringList;
    FConstFoldVal: Int64;   // scratch for the fold above (a field, so every CONST site can use it)
    // True while ParseConstStatement is parsing the declaration itself. One of its three forms reads
    // the "name = value" part with ParseAssignmentStatement, so without this the rejection below
    // fires on the DECLARATION of a constant whose name was already declared in another scope
    // (two procedures may each have their own "Const localc = ...").
    FInConstDecl: Boolean;
    // Dialect-pluggable: per-token statement handlers installed by the active dialect profile.
    // Consulted by ParseStatement before the built-in case; nil entry = no override.
    FStmtHandlers: array[TTokenType] of TStatementParseFunc;
    FProfile: TDialectProfile;  // active dialect profile (derived from FModernMode)
    // Overloading: procedure label (UPPER) -> the FIRST declaration seen with that label, while it is
    // still un-renamed. A second declaration of the same name means an overload set, and both get a
    // parameter-bank signature appended to their labels (see ParseProcedureDecl).
    FProcSeen: TStringList;
    // OOP: methods a TYPE body declared STATIC ("Declare Static Sub f(...)"), as "TYPE.METHOD" keys.
    // A static member procedure has NO implicit THIS, so its out-of-line definition ("Sub T.f(...)")
    // must not be given one -- otherwise every call passes its arguments one position too far right,
    // and taking its address (@T.f) yields a procedure whose arity does not match the call.
    // FreeBASIC requires the declaration to precede the definition, so a single forward pass suffices.
    FTypeStaticMethods: TStringList;
    // OOP: the DEFAULT ARGUMENTS a TYPE body's "Declare ..." line gave a method, keyed "TYPE.METHOD".
    // FreeBASIC states them on the DECLARATION, never on the out-of-line definition — so a definition
    // read on its own looks like it has none, and "Dim v As T" then found no constructor callable with
    // zero arguments and left the object unconstructed. Each entry's object is an antArgumentList with
    // one child per parameter: the default expression, or a NODEF placeholder.
    FTypeMethodDefaults: TStringList;
    procedure ApplyDeclaredDefaults(const QualName: string; ParamList: TASTNode; SkipThis: Boolean);
    procedure ClearTypeMethodDefaults;

    function ProcSigFromParams(ParamList: TASTNode; SkipThis: Boolean;
                               WithTypeNames: Boolean = False;
                               PtrKinds: Boolean = False): string;   // CONSTRUCTORS only: see the body
    procedure RegisterOverloadLabel(DeclNode, NameNode, ParamList: TASTNode; IsMethod: Boolean);

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
    function FoldFileHandlePostfix(BaseNode: TASTNode): TASTNode;
    function ParseFileHandleIdent: TASTNode;
    function ParseFileNumberOperand: TASTNode;

    // Helper methods for block parsing
    function ParseBlockUntil(EndTokens: array of TTokenType): TASTNode;
    function ParseLoopBody: TASTNode;   // DO/WHILE body, skipping nested flat FOR...NEXT
    function FindMatchingNext: Integer;
    function FindMatchingEnd(StartToken: TTokenType): Integer;
    function ParseDimensionList: TASTNode;
    procedure ParseInTypeMethodDecl(TypeNode: TASTNode; const CurAccess: string = ''; ForceAbstract: Boolean = False);   // one "Declare ..." line inside a TYPE body
    function NoDefaultPlaceholder(Tok: TLexerToken): TASTNode;
    procedure SkipTypeQualifiers;
    function SkipTypeQualifiersConst: Boolean;   // ...and report whether CONST was one of them
    function AtPointerSuffix: Boolean;   // FB: the current token is "PTR" (or its synonym "POINTER")
    function TryConstIntExpr(N: TASTNode; out V: Int64): Boolean;   // fold a constant integer expression
    function TryConstDataExpr(N: TASTNode; out V: Variant): Boolean; // fold a MODERN DATA item (num/float/string)
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
    procedure SyncExpressionDialect;   // mirror FModernMode onto the expression parser
    procedure RegisterStatementHandler(TokenType: TTokenType; Handler: TStatementParseFunc);
    procedure ClearStatementHandlers;

    // === CONTEXT MANAGEMENT OVERRIDE ===
    procedure SetContext(AContext: TParserContext); override;

    // === MAIN PARSING INTERFACE ===
    function Parse(TokenList: TTokenList): TParsingResult;
    function ParseExpression(TokenList: TTokenList): TParsingResult; overload;

    // === CORE PARSING METHODS ===
    function ParseProgram: TASTNode;
    procedure DedupNumberedLines(ProgramNode: TASTNode);
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
    function ParenGroupIsFollowedByAs(Offset: Integer): Boolean;  // "( ... ) As" starting at Offset?
    function ParseBareCallStatement: TASTNode;
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
    // UNION name / field AS type / ... / END UNION : record with overlapping same-bank fields.
    function ParseUnionDecl: TASTNode;
    // RANDOMIZE [seed] : seed the RNG (the optional seed expression becomes child0).
    function ParseRandomizeStatement: TASTNode;
    // Shared body for TYPE / UNION (IsUnion tags the node so SSA overlaps same-bank fields).
    function ParseRecordDecl(IsUnion: Boolean; IsInterface: Boolean = False): TASTNode;
    function ParseInterfaceDecl: TASTNode;   // MODERN: Interface ... End Interface
    function ParseRecordFieldType: string;
    // Parse a "{ ... }" array initializer group, appending leaf expressions to InitList (row-major).
    // DimSizes (element counts per dimension, empty if not all constant) lets a short nested row/plane be
    // zero-padded to its stride so a jagged multi-dim initializer stays row-aligned. Level = brace depth.
    procedure ParseArrayInitBraceGroup(InitList: TASTNode; const DimSizes: array of Integer; Level: Integer);
    function ConstDimSizes(DimsNode: TASTNode): TDimSizeArray;
    // Optional "= { ... }" / "=> { ... }" array initializer on an already-built antArrayDecl.
    function TryParseAggregateTuple(const DimTypeName: string): TASTNode;
    procedure ParseOptionalArrayInit(Decl, Dimensions: TASTNode; const Tok: TLexerToken);
    function AtEndType: Boolean;
    procedure ConsumeEndType;
    // WITH obj / ... / END WITH : a leading '.field' resolves against obj. Parse-time desugar.
    function ParseWith: TASTNode;
    function AtEndWith: Boolean;
    // NAMESPACE name / ... / END NAMESPACE (FreeBASIC): group member decls under a name.
    function ParseNamespaceDecl: TASTNode;
    function AtEndNamespace: Boolean;
    // SCOPE ... END SCOPE (FreeBASIC): anonymous lexical block scope. Parsed into an antBlock node
    // (same as BEGIN/BEND), so the SSA's MODERN block-scope machinery handles DIM shadowing + destructors.
    function ParseScopeBlock: TASTNode;
    function AtEndScope: Boolean;
    // Read a dotted name "ident(.ident)*" (e.g. a namespace-qualified type "Forms.Point"); returns
    // the joined UPPER-cased name and consumes all segments. The first token must already be checked.
    function ParseDottedName: string;
    // FreeBASIC function-pointer type "FUNCTION(params) AS ret" / "SUB(params)" after AS. If the
    // current token is FUNCTION/SUB, consume the whole type and mark Node with FUNCPTR / FPPARAMS /
    // FPRET attributes (the variable holds a procedure entry PC, int-banked). Returns True if matched.
    function TryParseProcPtrType(Node: TASTNode): Boolean;
    procedure ConsumeEndProcedure;
    function ParseForStatement: TASTNode;
    function ParseDoStatement: TASTNode;
    function ParseGotoStatement: TASTNode;
    function ParseGosubStatement: TASTNode;
    function ParseFunctionResultAssign: TASTNode;
    function ParseReturnStatement: TASTNode;
    function ParseEndStatement: TASTNode;
    function ParseFastStatement: TASTNode;
    function ParseSlowStatement: TASTNode;
    function ParseRemStatement: TASTNode;
    function ParseDimStatement: TASTNode;
    // VAR x = expr (FreeBASIC): declare a variable with type inferred from the initializer (SSA side).
    function ParseVarStatement: TASTNode;
    // STATIC x AS t [= expr] (FreeBASIC): a local with persistent storage across calls.
    function ParseStaticStatement: TASTNode;
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
    function ParseSetClockStatement: TASTNode;
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
    function ParseFBPokeStatement(Token: TLexerToken): TASTNode;  // FB POKE [type,] ptr, value → *CPtr(T Ptr, ptr) = value; nil if not the FB form
    function ParseGraphicsStatement: TASTNode;
    function ParseSpriteStatement: TASTNode;
    function ParseSoundStatement: TASTNode;
    function ParseFileOperationStatement: TASTNode;
    function ParseFileManagementStatement: TASTNode;
    function ParseFileInputStatement: TASTNode;
    function ParseFileOutputStatement: TASTNode;
    function ParseLineInputStatement: TASTNode;  // FreeBASIC LINE INPUT #n, var (whole line)
    function ParseGfxLineStatement: TASTNode;     // FreeBASIC LINE (x1,y1)-(x2,y2),color[,B|BF]
    function ParseGfxPutStatement: TASTNode;       // FreeBASIC PUT (x,y), src [, mode]
    function ParseWriteFileStatement: TASTNode;   // FreeBASIC WRITE #n, exprlist (quoted CSV)
    function ParseWriteConsole: TASTNode;          // FreeBASIC WRITE exprlist (quoted CSV to screen)
    function ParseSeekStatement: TASTNode;         // FreeBASIC SEEK #n, pos (set position)
    function ParseNameStatement: TASTNode;         // FreeBASIC NAME old AS new (rename)
    function PeekNameHasAs: Boolean;               // lookahead: NAME ... AS ... on this statement
    function LooksLikeImageTarget: Boolean;        // lookahead: "cmd img, (x,y)..." FB image draw-target form
    function ParseRaiseErrorStatement: TASTNode;   // FreeBASIC ERROR <n> (raise runtime error)
    function ParseBinaryFileTail(IsGet: Boolean; const Tok: TLexerToken): TASTNode;  // GET/PUT #n,[pos],var
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


// The MODERN extensions that FreeBASIC does NOT reserve: a program may use any of them as the name
// of its own procedure, and then that name is the program's, not ours.
function IsShadowableExtensionName(const NameU: string): Boolean;
begin
  Result := (NameU = 'MIN') or (NameU = 'MAX') or (NameU = 'CEIL') or (NameU = 'ROUND') or
            (NameU = 'COPYSIGN') or (NameU = 'SINGLEBITS') or (NameU = 'BITSTOSINGLE');
end;

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

  FProcSeen := TStringList.Create;
  FProcSeen.CaseSensitive := False;
  FConstNames := TStringList.Create;
  FConstNames.CaseSensitive := False;
  FConstTypes := TStringList.Create;
  FConstTypes.CaseSensitive := False;
  FConstIntValues := TStringList.Create;
  FConstIntValues.CaseSensitive := False;
  FTypeStaticMethods := TStringList.Create;
  FTypeStaticMethods.CaseSensitive := False;
  FTypeMethodDefaults := TStringList.Create;
  FTypeMethodDefaults.CaseSensitive := False;
end;

destructor TPackratParser.Destroy;
begin
  if Assigned(FValidationStacks) then
    FValidationStacks.Free;

  if Assigned(FExpressionParser) then
    FExpressionParser.Free;

  FProcSeen.Free;
  FConstNames.Free;
  FConstTypes.Free;
  FConstIntValues.Free;
  FTypeStaticMethods.Free;
  ClearTypeMethodDefaults;
  FTypeMethodDefaults.Free;

  inherited Destroy;
end;

function IsBuiltinTypeName(const N: string): Boolean; forward;

function TPackratParser.ProcSigFromParams(ParamList: TASTNode; SkipThis: Boolean;
                                          WithTypeNames: Boolean; PtrKinds: Boolean): string;
// One bank character per explicit parameter -- 'S' string, 'F' float, 'I' everything else (integers,
// pointers and UDT handles, which are int handles). This is the scheme that tells "g(As Long)" from
// "g(As Single)" apart, and the one a call site can reproduce from its arguments' banks.
//
// It is done in the PARSER, not in the SSA collector, because the pre-scans that record a procedure's
// return type run before the collector and must already see the final label.
//
// WithTypeNames adds a TAIL of UDT type names -- "I:S", "I:T" -- because the bank alphabet alone cannot
// tell two UDTs apart: every UDT is an int HANDLE, so "Sub test(v As S)" and "Sub test(v As T)" both
// signed "~I", collided on one label, and the second was SILENTLY DISCARDED (every call went to the
// first, which then read the wrong record's fields -- an access violation in udt/temp-type3).
//
// The tail is appended ONLY when some parameter is a UDT taken BY VALUE, so a program without such an
// overload keeps byte-identical labels. A POINTER parameter ("T PTR") stays a plain 'I' with a '-' in
// the tail on purpose: a call site can see the BANK of a pointer argument but cannot reconstruct its
// pointee type name, and a tail it can never match would push pointer overloads onto the arity
// fallback -- which would happily pick a different overload of the same arity.
var
  i, First: Integer;
  p: TASTNode;
  T, Nm, Banks, Names, Consts, Widths: string;
  C: Char;
  AnyUDT, AnyConst, AnyWidth: Boolean;

  // The DECLARED WIDTH of a parameter, as one character. ⛔ The codes are TypeNameWidthCode's, and they
  // have to be, because the call site reproduces this tail from OperandWidthCode - which reads the very
  // same registry. Two spellings of the same fact would be two facts.
  //   1 Byte  2 UByte  3 Short  4 UShort  5 Long  6 ULong  7 Single  8 UInteger/ULongInt
  //   9 Int32 A UInt32 B Boolean          '-' = full 64-bit (Integer/LongInt/Double) or not known
  function WidthCharOf(const TN: string): Char;
  begin
    if (TN = 'BYTE') then Result := '1'
    else if (TN = 'UBYTE') then Result := '2'
    else if (TN = 'SHORT') then Result := '3'
    else if (TN = 'USHORT') then Result := '4'
    else if (TN = 'LONG') then Result := '5'
    else if (TN = 'ULONG') then Result := '6'
    else if (TN = 'SINGLE') then Result := '7'
    else if (TN = 'UINTEGER') or (TN = 'ULONGINT') then Result := '8'
    else if (TN = 'INT32') then Result := '9'
    else if (TN = 'UINT32') then Result := 'A'
    else if (TN = 'BOOLEAN') then Result := 'B'
    // ⭐ 'Z' / 'W': a ZSTRING PTR and a WSTRING PTR parameter. Both sign the bank 'I' - every pointer
    // does - so a type declaring one CONSTRUCTOR for each shared ONE label and the second was silently
    // discarded; fbc's own udt-zstring reference implementation declares exactly that pair, and the
    // argument then arrived as 0. ⛔ CONSTRUCTORS ONLY (PtrKinds), and that is the ORACLE's rule, not a
    // shortcut: fbc REFUSES two SUBs that differ only this way ("error 4: Duplicated definition") while
    // accepting the two constructors. Handing SUBs the same distinction would invent an overload set
    // fbc does not have.
    else if PtrKinds and (TN = 'ZSTRING PTR') then Result := 'Z'
    else if PtrKinds and (TN = 'WSTRING PTR') then Result := 'W'
    else Result := '-';
  end;

begin
  Result := '';
  if ParamList = nil then Exit;
  Banks := '';
  Names := '';
  Consts := '';
  Widths := '';
  AnyUDT := False;
  AnyConst := False;
  AnyWidth := False;
  if SkipThis then First := 1 else First := 0;   // a method's implicit THIS sits at index 0
  for i := First to ParamList.ChildCount - 1 do
  begin
    p := ParamList.GetChild(i);
    T := '';
    // The type child is at index 0 when present; an untyped "param = default" has only the default
    // expression as its child, so it carries no type.
    if (p.ChildCount >= 1) and (p.GetChild(0).NodeType = antIdentifier) and
       not ((p.Attributes.Values['HASDEFAULT'] = '1') and (p.ChildCount = 1)) then
      T := UpperCase(VarToStr(p.GetChild(0).Value));
    if (T = 'STRING') or (T = 'ZSTRING') or (T = 'WSTRING') then
      C := 'S'
    else if (T = 'SINGLE') or (T = 'DOUBLE') then
      C := 'F'
    else if T <> '' then
      C := 'I'
    else
    begin
      // Untyped parameter: fall back to the name's type suffix, as the rest of the pipeline does.
      Nm := VarToStr(p.Value);
      if Nm = '' then C := 'I'
      else if Nm[Length(Nm)] = '$' then C := 'S'
      else if (Nm[Length(Nm)] = '!') or (Nm[Length(Nm)] = '#') then C := 'F'
      else C := 'I';
    end;
    Banks := Banks + C;
    Widths := Widths + WidthCharOf(T);
    if Widths[Length(Widths)] <> '-' then AnyWidth := True;
    if p.Attributes.Values['CONSTP'] = '1' then
    begin
      Consts := Consts + 'C';
      AnyConst := True;
    end
    else
      Consts := Consts + '-';
    if Names <> '' then Names := Names + ',';
    // A by-value UDT: not builtin, not a pointer, and named. Anything else contributes a placeholder --
    // the tail is POSITIONAL, so a "-" must hold the slot.
    // ⭐ ...and a POINTER type names itself here too, with its full spelling ("INTEGER PTR",
    // "BYTE PTR PTR"). Every pointer signs the bank 'I', so a set overloaded on pointee type - which is
    // what fbc's own overload/pointers declares, 22 of them - collided on ONE label and every call went
    // to the first. The comment that used to stand here said a call site cannot reconstruct a pointee
    // type; it can, for the shape that matters: a declared pointer VARIABLE or PARAMETER, which is what
    // such a call passes. When it cannot, it writes '-' and the resolver treats that as "any" (see
    // ResolveCallLabel), so nothing that resolved before stops resolving.
    if (T <> '') and ((not IsBuiltinTypeName(T)) or (Pos(' PTR', T) > 0)) then
    begin
      Names := Names + T;
      AnyUDT := True;
    end
    else
      Names := Names + '-';
  end;
  Result := Banks;
  if WithTypeNames and AnyUDT then
    Result := Result + ':' + Names;
  // A CONST tail, appended only when some parameter carries the qualifier - so a program without a
  // const/non-const overload pair keeps byte-identical labels, exactly as the UDT tail does. It is
  // POSITIONAL ('C' or '-' per parameter) because the call site has to reproduce it from its arguments.
  if AnyConst then
    Result := Result + '!' + Consts;
  // ⭐ A WIDTH tail, appended only when some parameter has a width of its own - so a set of overloads
  // that differ by BANK alone keeps byte-identical labels, exactly as the two tails above do. It is what
  // tells "g(As Long)" from "g(As Integer)": both sign the bank 'I', so before this the two collided on
  // one label and the FIRST declaration won every call. POSITIONAL, like the others.
  if AnyWidth then
    Result := Result + '%' + Widths;
end;

procedure TPackratParser.RegisterOverloadLabel(DeclNode, NameNode, ParamList: TASTNode; IsMethod: Boolean);
// See the call site in ParseProcedureDecl. Only DEFINITIONS reach here -- a DECLARE (module level or in a
// TYPE body) is skipped without producing a node -- so a repeated label really is an overload set, never a
// prototype paired with its definition.
var
  Base: string;
  Idx: Integer;
  FirstDecl, FirstName, FirstParams: TASTNode;
begin
  if (NameNode = nil) or (ParamList = nil) then Exit;
  Base := UpperCase(VarToStr(NameNode.Value));
  // Every OPERATOR carries its own discriminator already and must be left alone: the symbol form has
  // "@<arity>" (above), and the named form -- CAST / LET -- is told apart by its RETURN BANK, a suffix the
  // SSA collector appends ("T.OPERATORCAST$" / "%"). Two casts of one type share a label HERE, at parse
  // time, and have no parameters at all, so treating them as an overload set would give both the same
  // empty signature and break the return-bank scheme. A constructor likewise carries "#<arity>".
  if (Base = '') or (Pos('.OPERATOR', Base) > 0) or
     (Pos('#', Base) > 0) or (Pos('@', Base) > 0) or (Pos('~', Base) > 0) then Exit;

  Idx := FProcSeen.IndexOf(Base);
  if Idx < 0 then
  begin
    FProcSeen.AddObject(Base, DeclNode);       // first one: keep the bare label
    Exit;
  end;

  // Second (or later) declaration of this name: give it a signature...
  NameNode.Value := Base + '~' + ProcSigFromParams(ParamList, IsMethod, True);

  // ...and, the first time only, retroactively give one to the declaration already parsed. Its object is
  // cleared afterwards so a third overload does not rename it twice.
  FirstDecl := TASTNode(FProcSeen.Objects[Idx]);
  if FirstDecl <> nil then
  begin
    if (FirstDecl.ChildCount >= 2) and (FirstDecl.GetChild(0).NodeType = antIdentifier) and
       (FirstDecl.GetChild(1).NodeType = antParameterList) then
    begin
      FirstName := FirstDecl.GetChild(0);
      FirstParams := FirstDecl.GetChild(1);
      // A method's THIS sits at index 0 of its parameter list; the first declaration is a method exactly
      // when this one is (they share a qualified "TYPE.NAME" label).
      FirstName.Value := Base + '~' + ProcSigFromParams(FirstParams, IsMethod, True);
    end;
    FProcSeen.Objects[Idx] := nil;
  end;
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

procedure TPackratParser.SyncExpressionDialect;
begin
  // The expression parser needs the dialect too: a couple of spellings (INPUT(n)) exist only in MODERN
  // and must be rejected, not silently parsed, when compiling Commodore BASIC v7.
  if Assigned(FExpressionParser) then
    FExpressionParser.ModernMode := FModernMode;
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
// (NEW MODERN/CLASSIC, OPTION MODE, LOAD re-detect). Expression parse-rules are installed once, so a
// rule whose validity depends on the dialect reads TExpressionParser.ModernMode instead; that flag is
// mirrored here (SyncExpressionDialect). Today the only such rule is FreeBASIC's INPUT(n) function
// form, which must not be accepted while compiling Commodore BASIC v7.
begin
  SyncExpressionDialect;
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
var
  ArrayForm: Boolean;
begin
  Context.Advance;   // consume DELETE
  // "Delete[] p" frees what "New T[n]" allocated. The brackets are empty - they only say "this was an
  // ARRAY allocation" - so consume them and remember which form this is.
  ArrayForm := False;
  if Context.Check(ttDelimBrackOpen) then
  begin
    Context.Advance;
    if Context.Check(ttDelimBrackClose) then Context.Advance;
    ArrayForm := True;
  end;
  PtrExpr := FExpressionParser.ParseExpression(precCall);
  if not Assigned(PtrExpr) then
  begin
    HandleError('Expected a pointer after DELETE', Context.CurrentToken);
    Exit(nil);
  end;
  Result := TASTNode.Create(antDelete, Context.CurrentToken);
  if ArrayForm then Result.Attributes.Values['NEWARRAY'] := '1';
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
  FProcSeen.Clear;   // overload detection is per-program (the parser instance is reused)
  FConstNames.Clear; // ...and so is the set of CONST names (the parser instance is reused)
  FConstTypes.Clear;
  FTypeStaticMethods.Clear;  // ...and the static-member map (per-program, parser instance is reused)
  ClearTypeMethodDefaults;   // ...and the declared default arguments

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
    FOptionBase := 0;      // reset per parse; set by an "OPTION BASE 1" directive as it is encountered
    FOptionDigits := 0;    // 0 = not specified: the dialect default (16 / 7) stands

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
    // "OPTION DIGITS n" travels out on the RESULT: the parser is freed as soon
    // as parsing ends, and this one configures the runtime rather than the tree.
    Result.OptionDigits := FOptionDigits;

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
  FProcSeen.Clear;   // overload detection is per-program (the parser instance is reused)
  FConstNames.Clear; // ...and so is the set of CONST names (the parser instance is reused)
  FConstTypes.Clear;
  FTypeStaticMethods.Clear;  // ...and the static-member map (per-program, parser instance is reused)
  ClearTypeMethodDefaults;   // ...and the declared default arguments

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

 // CLASSIC replace semantics: a repeated line number REPLACES the earlier line, as on a real
 // C128 (typing the listing: last definition wins, at the number's slot). Without this both
 // versions were lowered and the duplicated LINE_<n> label crashed the dominator pass.
 DedupNumberedLines(Result);

 DoNodeCreated(Result);
end;

procedure TPackratParser.DedupNumberedLines(ProgramNode: TASTNode);
// A "line group" is an antLineNumber child plus the statements that follow it, up to the next
// antLineNumber (children before the first line number - the MODERN case - form an untouched
// unnumbered group, so this pass is inert on programs without line numbers). When the same
// number appears more than once, the LAST group's content wins and lands at the FIRST group's
// position (the C128 program store is keyed by line number, so for an otherwise-ordered
// listing that slot is where the number belongs); every other group with that number is
// dropped and freed. Line numbers nested inside BEGIN/BEND blocks are children of the block
// statement, not of the program node, and are deliberately out of scope here.
var
  GroupNum, GroupStart, GroupCnt: array of Integer;
  GCount, i, g, w, n: Integer;
  HasDup: Boolean;
  Node: TASTNode;
  Keep, Drop: TFPList;
  Emitted: array of Boolean;
begin
  // Collect the groups.
  GCount := 0;
  SetLength(GroupNum, 8); SetLength(GroupStart, 8); SetLength(GroupCnt, 8);
  for i := 0 to ProgramNode.ChildCount - 1 do
  begin
    Node := ProgramNode.GetChild(i);
    if (Node.NodeType = antLineNumber) or (GCount = 0) then
    begin
      if GCount >= Length(GroupNum) then
      begin
        SetLength(GroupNum, GCount * 2); SetLength(GroupStart, GCount * 2);
        SetLength(GroupCnt, GCount * 2);
      end;
      if Node.NodeType = antLineNumber then
        GroupNum[GCount] := StrToIntDef(VarToStr(Node.Value), 0)
      else
        GroupNum[GCount] := -1;   // unnumbered prefix (MODERN / mixed leading statements)
      GroupStart[GCount] := i;
      GroupCnt[GCount] := 0;
      Inc(GCount);
    end;
    Inc(GroupCnt[GCount - 1]);
  end;

  // Any duplicate number? (Corpus programs are small; the quadratic scan only runs on the
  // group table, not the statements.)
  HasDup := False;
  for g := 1 to GCount - 1 do
    if GroupNum[g] >= 0 then
      for w := 0 to g - 1 do
        if GroupNum[w] = GroupNum[g] then begin HasDup := True; Break; end;
  if not HasDup then Exit;

  Keep := TFPList.Create;
  Drop := TFPList.Create;
  try
    SetLength(Emitted, GCount);
    for g := 0 to GCount - 1 do Emitted[g] := False;
    for g := 0 to GCount - 1 do
    begin
      if Emitted[g] then System.Continue;
      if GroupNum[g] < 0 then
        w := g
      else
      begin
        // Winner = the last group with this number; mark every occurrence as handled.
        w := g;
        for n := g + 1 to GCount - 1 do
          if GroupNum[n] = GroupNum[g] then begin w := n; Emitted[n] := True; end;
      end;
      Emitted[g] := True;
      for i := GroupStart[w] to GroupStart[w] + GroupCnt[w] - 1 do
        Keep.Add(ProgramNode.GetChild(i));
    end;

    // Everything not kept gets freed.
    for i := 0 to ProgramNode.ChildCount - 1 do
    begin
      Node := ProgramNode.GetChild(i);
      if Keep.IndexOf(Node) < 0 then Drop.Add(Node);
    end;

    // Rebuild the child list without freeing the survivors, then free the dropped subtrees.
    ProgramNode.Children.OwnsObjects := False;
    ProgramNode.Children.Clear;
    for i := 0 to Keep.Count - 1 do
      ProgramNode.Children.Add(TASTNode(Keep[i]));
    ProgramNode.Children.OwnsObjects := True;
    for i := 0 to Drop.Count - 1 do
      TASTNode(Drop[i]).Free;
  finally
    Keep.Free;
    Drop.Free;
  end;
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

  // ⭐ A shadowable MODERN extension at the START of a statement is a NAME, not our intrinsic:
  // "min(A, B) = 0" assigns through a ByRef result, and "myproc arg" calls the user's procedure.
  // The declaration site already accepts these names (see IsShadowableExtensionName); a statement
  // that begins with one has to as well, or the program parses its own function and then cannot
  // call it. Nothing is lost: the extensions are FUNCTIONS, so none of them ever begins a statement.
  if FModernMode and (Token.TokenType <> ttIdentifier) and
     IsShadowableExtensionName(UpperCase(Token.Value)) then
    Token.TokenType := ttIdentifier;

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

 // FreeBASIC linkage prefix: PRIVATE / PUBLIC before SUB/FUNCTION/TYPE/UNION/CLASS/CONST/DECLARE.
 // SedaiBasic does not enforce module linkage, so consume the modifier and dispatch the following
 // declaration as usual (PRIVATE/PUBLIC are not registered keywords, so they arrive as identifiers).
 if (Token.TokenType = ttIdentifier) and
    ((UpperCase(Token.Value) = 'PRIVATE') or (UpperCase(Token.Value) = 'PUBLIC')) and
    Assigned(Context.PeekNext) and
    ((Context.PeekNext.TokenType in [ttProcedureStart, ttTypeDecl, ttUnionDecl, ttConstant]) or
     ((Context.PeekNext.TokenType = ttIdentifier) and (UpperCase(Context.PeekNext.Value) = 'DECLARE'))) then
 begin
   Context.Advance;                 // consume PRIVATE / PUBLIC
   Token := Context.CurrentToken;   // re-dispatch on the actual declaration keyword below
 end;

 // FreeBASIC forward declaration "DECLARE SUB|FUNCTION ...": our parser resolves calls via a pre-pass
 // over the real definitions, so a forward declaration is a no-op — skip the rest of the line. (Without
 // this, module-level DECLARE falls through to identifier/assignment parsing and hangs.)
 // FreeBASIC OOP decorators on a DEFINITION: "Virtual Destructor T()", "Abstract Function ...",
 // "Override Sub ...". Inside a TYPE body they sit after DECLARE, which is skipped wholesale; at module
 // level they lead the definition itself, and an unknown leading identifier was read as an assignment
 // target ("Expected = in assignment"). Consume the decorator and let the procedure parse - the
 // dispatch it asks for is already what a method call does.
 if (Token.TokenType = ttIdentifier) and
    ((UpperCase(Token.Value) = 'VIRTUAL') or (UpperCase(Token.Value) = 'ABSTRACT') or
     (UpperCase(Token.Value) = 'OVERRIDE')) and
    Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttProcedureStart) then
 begin
   Context.Advance;                    // the decorator
   { ⚠️ Le PARENTESI non sono stile: dentro una funzione il suo stesso nome NUDO è la variabile di
     RISULTATO, non una chiamata, e l'assegnazione diventa `Result := Result` con il risultato non
     inizializzato. È il difetto trovato il 13 ago 2026 in TLexerFSM.NextToken, dove costava un
     EAccessViolation su ogni programma con un commento /' '/ (e su Windows un token DUPLICATO in
     silenzio). Qui non sono riuscito a costruire un ingresso che PROVI di raggiungere questa riga -
     a module-level `Virtual Sub` appears to be handled earlier - so the fix is placed
     perché è un NO-OP se il nome nudo era già una chiamata, e una cura se non lo era. }
   Result := ParseStatement();         // ...and the SUB/FUNCTION/DESTRUCTOR that follows
   Exit;
 end;

 if (Token.TokenType = ttIdentifier) and (UpperCase(Token.Value) = 'DECLARE') then
 begin
   while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) do Context.Advance;
   Result := nil;
   Exit;
 end;

 // FreeBASIC EXTERN / IMPORT — external linkage, N/A for a single-module bytecode VM (no native
 // linking). Accepted and skipped so files using them still parse. `EXTERN "lang"` opens a block
 // closed by END EXTERN; otherwise it is a single-line declaration. (Without this, module-level
 // EXTERN would fall through to identifier/assignment parsing and hang, like DECLARE did.)
 if (Token.TokenType = ttIdentifier) and
    ((UpperCase(Token.Value) = 'EXTERN') or (UpperCase(Token.Value) = 'IMPORT')) then
 begin
   if (UpperCase(Token.Value) = 'EXTERN') and Assigned(Context.PeekNext) and
      (Context.PeekNext.TokenType = ttStringLiteral) then
   begin
     // `EXTERN "lang"` ... `END EXTERN` is a LINKAGE wrapper, not a container: the declarations inside it
     // are ordinary declarations of this module and fbc compiles them. Skipping to END EXTERN threw them
     // away, so a SUB declared in such a block did not exist and every call to it failed ("Array not
     // declared"). Consume the header and the terminator only, and let the body parse where it stands.
     Context.Advance;                     // EXTERN
     if Context.Check(ttStringLiteral) then Context.Advance;   // the "C" / "Windows" linkage name
     Result := nil;
     Exit;
   end
   else if (UpperCase(Token.Value) = 'EXTERN') and Assigned(Context.PeekNext) and
           (Context.PeekNext.TokenType = ttIdentifier) and
           (UpperCase(Context.PeekNext.Value) = 'EXTERN') then
   begin
     Context.Advance; Context.Advance;    // (defensive: a stray "EXTERN EXTERN")
     Result := nil;
     Exit;
   end
   else
     while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) do Context.Advance;
   Result := nil;
   Exit;
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
    ttProgramSetClock: Result := Memoize('SetClockStatement', @ParseSetClockStatement);
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
      // "FUNCTION = expr" inside a FUNCTION body is FreeBASIC's canonical way to set the result (the
      // named form "fname = expr" is the other). Only "FUNCTION" followed by "=" is the result
      // assignment -- anything else starting with FUNCTION is a declaration.
      // "OPERATOR = expr" inside an operator body sets its RESULT, exactly as "FUNCTION = expr" does in a
      // function - it is how a Cast or an arithmetic operator hands its value back, and it is what the
      // manual writes. Told apart from a declaration the same way: a following '=' can only be the
      // assignment (a declaration continues with a symbol or "<Type>.").
      // "PROPERTY = expr" is the same statement inside a property GETTER: the manual's static-member
      // example ends its getter with "Property = This.ID", which parsed as a declaration and stopped the
      // whole file at "Expected a name after PROPERTY".
      // ⛔ ...unless it is the DECLARATION of the equality operator: "Operator = ( ByRef lhs As T,
      // ByRef rhs As U ) As R" begins with the same two tokens and is not an assignment at all. It was
      // taken for one, and the parameter list was then read as a parenthesised EXPRESSION, which died
      // on "Unexpected token ByRef" - the whole of proguide/object-class stopped there.
      // The two are told apart by what CLOSES the parentheses: a declaration continues "... ) As <type>",
      // a result assignment does not (and "Operator = (a + b)" is a perfectly ordinary one).
      if ((UpperCase(Token.Value) = kFUNCTION) or (UpperCase(Token.Value) = kOPERATOR) or
          (UpperCase(Token.Value) = kPROPERTY)) and
         Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttOpEq) and
         not ((UpperCase(Token.Value) = kOPERATOR) and ParenGroupIsFollowedByAs(2)) then
        Result := Memoize('FunctionResultAssign', @ParseFunctionResultAssign)
      else if (UpperCase(Token.Value) = kSUB) or (UpperCase(Token.Value) = kFUNCTION) or
         (UpperCase(Token.Value) = kCONSTRUCTOR) or (UpperCase(Token.Value) = kDESTRUCTOR) or
         (UpperCase(Token.Value) = kPROPERTY) or (UpperCase(Token.Value) = kOPERATOR) then
        Result := Memoize('ProcedureDecl', @ParseProcedureDecl)
      else
        Result := Memoize('FnStatement', @ParseFnStatement);
    ttCallSub: Result := Memoize('CallStatement', @ParseCallStatement);
    ttBaseCall:
      // "base.field = expr" (member assignment) or "base.method()" (super call) vs "BASE(args)"
      // (base-constructor call). ParseAssignmentStatement returns nil for a member call (no '='), so
      // fall back to an expression statement which emits the call.
      if Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttOpDot) then
      begin
        Context.SavePosition(SavedIndex);
        Result := ParseAssignmentStatement;
        if not Assigned(Result) then
        begin
          Context.RestorePosition(SavedIndex);   // assignment consumed the LHS; rewind before the call form
          Result := ParseExpressionStatement;
        end;
      end
      else
        Result := Memoize('BaseStatement', @ParseBaseStatement);
    ttThreadWait: Result := Memoize('ThreadWaitStatement', @ParseThreadWaitStatement);
    ttThreadDetach: Result := Memoize('ThreadDetachStatement', @ParseThreadDetachStatement);
    ttMutexLock, ttMutexUnlock, ttMutexDestroy:
      Result := Memoize('MutexOpStatement', @ParseMutexOpStatement);
    ttCondWait: Result := Memoize('CondWaitStatement', @ParseCondWaitStatement);
    ttCondSignal, ttCondBroadcast, ttCondDestroy:
      Result := Memoize('CondOpStatement', @ParseCondOpStatement);
    ttSharedDecl: Result := ParseSharedError;   // SHARED is only the DIM SHARED modifier, not a statement
    // ⛔ TYPE at statement start is not always a DECLARATION: "type<UDT>( ).method( )" is an anonymous
    // temporary a method is called on, and fbc's own suite writes it that way. The declaration is always
    // "Type <name>" or "Type As <t> <name>"; only the type-CONSTRUCTOR spellings put "<" or "(" there,
    // which is a one-token look-ahead and cannot take a declaration away from ParseTypeDecl.
    ttTypeDecl:
      if Assigned(Context.PeekNext) and
         (Context.PeekNext.TokenType in [ttOpLt, ttDelimParOpen]) then
      begin
        // Assignment FIRST, exactly as the '(' case does: "type<UDT>( 0 ).i = 1" writes a member of the
        // temporary and is legal, and only the assignment shape carries the check that refuses writing
        // to the temporary ITSELF. Read as a plain expression it would be a discarded comparison, which
        // is how "type<UDT>( 1 ) = x" - an ERROR in fbc - came out silently accepted.
        SavedIndex := Context.CurrentIndex;
        Result := Memoize('AssignmentStatement', @ParseAssignmentStatement);
        if not Assigned(Result) then
        begin
          Context.CurrentIndex := SavedIndex;
          Result := Memoize('ExpressionStatement', @ParseExpressionStatement);
        end;
      end
      else
        Result := Memoize('TypeDecl', @ParseTypeDecl);
    ttUnionDecl: Result := Memoize('UnionDecl', @ParseUnionDecl);
    ttRandomize: Result := Memoize('RandomizeStatement', @ParseRandomizeStatement);
    ttWithBlock: Result := ParseWith;
    ttNamespaceBlock: Result := Memoize('NamespaceDecl', @ParseNamespaceDecl);
    ttScopeBlock: Result := Memoize('ScopeBlock', @ParseScopeBlock);

    // === MEMORY COMMANDS ===
    ttMemoryCommand: Result := Memoize('MemoryStatement', @ParseMemoryStatement);

    // === GRAPHICS COMMANDS ===
    ttGraphicsCommand: Result := Memoize('GraphicsStatement', @ParseGraphicsStatement);
    // A graphics function at statement start is an expression statement (e.g. GETMOUSE(x,y) called for its
    // by-reference side effects, discarding the status). Mirrors ttSpriteFunction / ttInputFunction.
    ttGraphicsFunction: Result := Memoize('ExpressionStatement', @ParseExpressionStatement);

    // ⭐ A BUILT-IN FUNCTION CALLED FOR NOTHING IS STILL A STATEMENT. FreeBASIC lets any function's
    // result be discarded - "CUInt( f( 1, 2, 3 ) )" is written exactly to run f and throw the number
    // away, and "Hex( i )" appears in fbc's own ignore-result test. Only the graphics/sprite/input
    // families were routed here; the math and string ones fell to the generic dispatcher and came out
    // as "Unexpected token in statement: "cuint"".
    ttMathFunction, ttStringFunction, ttMemoryFunction, ttSystemFunction,
    ttErrorHandlingFunction, ttOutputFunction:
      Result := Memoize('ExpressionStatement', @ParseExpressionStatement);

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
    // A statement may also START with '(' and be an assignment: "(*p).field = expr" is the only way to
    // write a field store through a dereference, since "*p.field" parses as "*(p.field)". Try the
    // assignment first and fall back to a plain expression statement, exactly as ttOpMul ("*p = expr").
    ttDelimParOpen:
      begin
        SavedIndex := Context.CurrentIndex;
        Result := Memoize('AssignmentStatement', @ParseAssignmentStatement);
        if not Assigned(Result) then
        begin
          Context.CurrentIndex := SavedIndex;
          Result := Memoize('ExpressionStatement', @ParseExpressionStatement);
        end;
      end;
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
        // ⭐ MODERN "Interface name ... End Interface". INTERFACE is not a reserved word (making it
        // one would break every program that uses it as a variable), so it is matched by spelling
        // and only in the shape that cannot mean anything else: the word followed by a NAME.
        if (UpperCase(Token.Value) = 'INTERFACE') and Assigned(Context.PeekNext) and
           (Length(VarToStr(Context.PeekNext.Value)) > 0) and
           (UpCase(VarToStr(Context.PeekNext.Value)[1]) in ['A'..'Z', '_']) then
          Result := ParseInterfaceDecl
        else if Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttSeparStmt) then
        begin
          Result := TASTNode.CreateWithValue(antLabel, Token.Value, Token);
          Context.Advance;   // consume the identifier
          Context.Advance;   // consume ':'
          DoNodeCreated(Result);
        end
        // FreeBASIC "LINE INPUT #n, var": LINE is not a registered keyword here (it is a bare
        // identifier), so detect the two-word form. Unambiguous — no statement has `line input`
        // meaning anything else.
        else if (UpperCase(Token.Value) = kLINE) and Assigned(Context.PeekNext) and
                ((UpperCase(Context.PeekNext.Value) = kINPUT) or
                 (UpperCase(Context.PeekNext.Value) = kINPUTN)) then   // 'INPUT' or combined 'INPUT#'
          Result := ParseLineInputStatement
        // FreeBASIC graphics "LINE (x1,y1)-(x2,y2),color[,B|BF]": LINE is a bare identifier here; the
        // parenthesis after it selects the graphics statement (vs LINE INPUT, vs an assignment to `line`).
        // A leading '-' also selects it ("LINE -(x2,y2)" omits the start), as does a leading STEP
        // ("LINE STEP(x1,y1)-...") or the image-target form ("LINE img,(x1,y1)-(x2,y2)").
        else if (UpperCase(Token.Value) = kLINE) and Assigned(Context.PeekNext) and
                ((Context.PeekNext.TokenType in [ttDelimParOpen, ttOpSub]) or
                 (UpperCase(Context.PeekNext.Value) = kSTEP) or LooksLikeImageTarget) then
          Result := ParseGfxLineStatement
        // FreeBASIC "WRITE #n, ...": comma-separated, quoted-string CSV output (WRITE is a bare
        // identifier here; the `#` after it disambiguates from an assignment to a var named `write`).
        else if (UpperCase(Token.Value) = kWRITE) and Assigned(Context.PeekNext) and
                ((Context.PeekNext.TokenType = ttFileHandlePrefix) or (Context.PeekNext.Value = '#')) then
          Result := ParseWriteFileStatement
        // FreeBASIC console "WRITE v1, v2, ...": quoted-CSV to the screen. WRITE is a bare identifier, so
        // only treat it as the statement when a value follows (not "write = ...", an assignment, nor a
        // bare "write" used as a variable).
        //
        // ...except that an ARGUMENT-LESS "Write" IS the statement, and prints an empty line (the manual:
        // "If no expression list is given, Write outputs a carriage return"). We swallowed it, losing the
        // blank line. Safe to take in MODERN: fbc reserves the word, so "write" cannot be a variable there
        // ("Dim write As Integer" -> error 4, Duplicated definition). CLASSIC has no console WRITE at all,
        // so a bare "write" there stays whatever it was.
        else if FModernMode and (UpperCase(Token.Value) = kWRITE) and Assigned(Context.PeekNext) and
                (Context.PeekNext.TokenType in [ttEndOfLine, ttSeparStmt, ttEndOfFile]) then
          Result := ParseWriteConsole
        else if (UpperCase(Token.Value) = kWRITE) and Assigned(Context.PeekNext) and
                (Context.PeekNext.TokenType <> ttOpEq) and (Context.PeekNext.Value <> '=') and
                not (Context.PeekNext.TokenType in [ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
          Result := ParseWriteConsole
        // FreeBASIC "SEEK #n, pos" statement (SEEK is also the SEEK(n) function — the `#` selects the
        // statement form). SEEK is a bare identifier here.
        else if (UpperCase(Token.Value) = kSEEK) and Assigned(Context.PeekNext) and
                ((Context.PeekNext.TokenType = ttFileHandlePrefix) or (Context.PeekNext.Value = '#')) then
          Result := ParseSeekStatement
        // The '#' is OPTIONAL in FreeBASIC: "Seek f, 100" is the same statement. In statement position
        // a bare file number (identifier or literal) can only be the statement form — the FUNCTION form
        // is always parenthesised, "Seek(f)", and an assignment to a variable named seek starts with '='.
        else if FModernMode and (UpperCase(Token.Value) = kSEEK) and Assigned(Context.PeekNext) and
                (Context.PeekNext.TokenType in [ttIdentifier, ttNumber, ttInteger]) then
          Result := ParseSeekStatement
        // FreeBASIC graphics "PUT (x,y), src [, mode]" — PUT is a bare identifier; the leading '('
        // (vs '#') selects the graphics blit form.
        else if (UpperCase(Token.Value) = kPUT) and Assigned(Context.PeekNext) and
                (Context.PeekNext.TokenType = ttDelimParOpen) then
          Result := ParseGfxPutStatement
        // FreeBASIC binary "PUT #n, [pos], var" — PUT is a bare identifier; the `#` selects it.
        else if (UpperCase(Token.Value) = kPUT) and Assigned(Context.PeekNext) and
                ((Context.PeekNext.TokenType = ttFileHandlePrefix) or (Context.PeekNext.Value = '#')) then
        begin
          Context.Advance;   // consume PUT
          Result := ParseBinaryFileTail(False, Token);
        end
        // FreeBASIC/QB "NAME old AS new" (rename). NAME is a bare identifier (not reserved, so it can
        // still be a variable/field); the trailing AS before end-of-statement disambiguates from an
        // assignment "name = ..." (no bare AS) and from "name" used as a value.
        else if (UpperCase(Token.Value) = kNAME) and PeekNameHasAs then
          Result := ParseNameStatement
        // FreeBASIC/QB "ERROR <n>" — raise a user runtime error. ERROR is a bare identifier (not
        // reserved); an argument (not '=' / '.' / '(' / '[' / end-of-statement) selects the statement
        // form and keeps "error" usable as a variable.
        else if (UpperCase(Token.Value) = kERROR) and Assigned(Context.PeekNext) and
                not (Context.PeekNext.TokenType in [ttOpEq, ttOpDot, ttDelimParOpen, ttDelimBrackOpen,
                                                    ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
          Result := ParseRaiseErrorStatement
        // Note: the FreeBASIC in-place "MID(dst,start[,len]) = src" statement (MODERN) is intercepted
        // earlier by the dialect profile's IdentMidStatementHandler (mechanism 3), so it does not need
        // a branch here; in CLASSIC bare MID is a plain identifier handled by the default path below.
        // FreeBASIC/QB bare SUB call with unparenthesised arguments: "SubName arg1[, arg2...]". The name
        // is a bare identifier immediately followed by a value token (the first argument), so it cannot
        // be an assignment ("name = ..."), a compound assignment ("name += ..."), or a member / array /
        // call ("name." / "name(" / "name["). A bare "name" with nothing after it is left to the default
        // path (an expression/assignment), so the name stays usable as a variable.
        else if Assigned(Context.PeekNext) and
                (Context.PeekNext.TokenType in [ttNumber, ttInteger, ttFloat, ttStringLiteral, ttIdentifier,
                                                ttOpSub, ttOpAdd,
                                                // A first argument may itself start with a builtin-function
                                                // keyword, e.g. "Split RTrim(s,sep), ...", "f Len(x), y".
                                                ttStringFunction, ttMathFunction, ttMemoryFunction,
                                                ttSystemFunction, ttInputFunction, ttUsrFunction,
                                                ttErrorHandlingFunction, ttOutputFunction,
                                                ttGraphicsFunction, ttSpriteFunction, ttSoundFunction,
                                                // ...or with its PASSING MODE written on it:
                                                // "test_const ByVal 1234" is FreeBASIC's per-argument
                                                // override, and it can only be a bare call - an
                                                // assignment never has BYVAL where its '=' belongs.
                                                ttParamMode]) then
          // A value token (or a leading +/- sign of a signed numeric argument, e.g. "bitwise -15, 3")
          // right after the name makes this a bare SUB call, never an assignment. Compound assignment
          // ("name += ..." / "name -= ...") uses a single ttCompoundAssign token, so it is unaffected.
          Result := Memoize('BareCallStatement', @ParseBareCallStatement)
        // FreeBASIC/QB bare parameterless SUB call: a lone "SubName" as a whole statement (nothing after
        // the name before the end of the statement). Parsed as an argument-less call; the SSA no-ops it
        // if the name is not a known procedure, so a stray bare identifier does not misfire as a call.
        else if Assigned(Context.PeekNext) and
                (Context.PeekNext.TokenType in [ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
          Result := Memoize('BareCallStatement', @ParseBareCallStatement)
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

    // === REFERENCE RESEATING: "@ref = expr" ===
    // FreeBASIC lets a reference be POINTED SOMEWHERE ELSE by assigning to its address: "@ru = New UDT"
    // is the manual's own way of reusing one reference over successive objects. Nothing else can begin
    // a statement with '@', so this costs no other shape; without it the line was met by the fallback
    // below and reported as "Unexpected token in statement: @".
    ttOpAt:
      begin
        SavedIndex := Context.CurrentIndex;
        Result := Memoize('AssignmentStatement', @ParseAssignmentStatement);
        if not Assigned(Result) then
        begin
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
 LeftSide, Expression, BareArgs, BareArg: TASTNode;
 Token: TLexerToken;
 SavedToken: TLexerToken;
 LhsIsExpr: Boolean;   // LHS built by the expression parser (member/array): may be a call stmt
 OpSym: string;        // compound-assignment operator symbol ('+','-','*','/','^')
 OpType: TTokenType;   // its arithmetic binary-op token type
begin
 Token := Context.CurrentToken;
 SavedToken := Token;   // default (member/array LHS branches don't set it; avoids nil on error)
 LhsIsExpr := False;

  // FreeBASIC CLEAR dst, value, count : the memset, in its STATEMENT spelling. The parenthesised call
  // form already worked - the whole lowering is in the SSA's CLEAR intercept - but CLEAR is not a
  // keyword here (it is resolved by name), so "Clear *scrbuf, 0, scrsize" arrived as an identifier and
  // died in the assignment grammar on the missing '='. Three examples hang on it: gfx/cls-memset,
  // array/clear and proguide/dynamicmemory.
  // Synthesised into exactly the node the call form builds, so there is one lowering, not two.
  if FModernMode and Context.Check(ttIdentifier) and (UpperCase(VarToStr(Token.Value)) = kCLEAR) and
     Assigned(Context.PeekNext) and
     (Context.PeekNext.TokenType <> ttOpEq) and (Context.PeekNext.TokenType <> ttDelimParOpen) and
     (Context.PeekNext.TokenType <> ttEndOfLine) and (Context.PeekNext.TokenType <> ttSeparStmt) then
  begin
    Context.Advance;                                   // CLEAR
    // The shape a "name(args)" call has - antArrayAccess(identifier, argument-list) - NOT antFunctionCall,
    // because that is the shape the SSA's raw-memory intercept already matches. One lowering, not two.
    Result := TASTNode.Create(antArrayAccess, Token);
    Result.AddChild(TASTNode.CreateWithValue(antIdentifier, kCLEAR, Token));
    Result.AddChild(ParseArgumentList);
    DoNodeCreated(Result);
    Exit;
  end;

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
  else if Context.Check(ttDelimParOpen) then
  begin
    // "(*p).field = expr", and any other target that opens with a parenthesis. The expression parser
    // builds antMemberAccess over the parenthesised deref and stops before '=' (lower precedence).
    LeftSide := FExpressionParser.ParseExpression(precCall);
    LhsIsExpr := True;
  end
  else if Context.Check(ttTypeDecl) and Assigned(Context.PeekNext) and
    (Context.PeekNext.TokenType in [ttOpLt, ttDelimParOpen]) then
  begin
    // "type<UDT>( 0 ).i = 1": a MEMBER of an anonymous temporary is a legal target (fbc's sf.net #801
    // says so, and says the temporary itself is not). The statement dispatch sends every "Type <" /
    // "Type (" here first; with no '=' this returns nil QUIETLY and the caller reads it as the
    // expression statement it is ("type<UDT>( ).method( )").
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
  else if Context.Check(ttBaseCall) then
  begin
    // FreeBASIC "base.field = expr" inside a derived method: the expression parser's BASE prefix rule
    // lowers `base` to THIS, so this builds an antMemberAccess target on the (inherited) base field.
    LeftSide := FExpressionParser.ParseExpression(precCall);
    LhsIsExpr := True;
  end
  else if Context.Check(ttIdentifier) then
  begin
    // *** SAVE THE TOKEN BEFORE ADVANCING ***
    SavedToken := Context.CurrentToken;
    // A CONST is not an lvalue. fbc rejects this outright (error 119); we used to accept it, because
    // a module CONST lowered to a plain DIM and so really was a writable variable.
    if FModernMode and (not FInConstDecl) and (FConstNames.IndexOf(UpperCase(Token.Value)) >= 0) then
    begin
      HandleError('Cannot modify a constant: ' + UpperCase(Token.Value), Token);
      Result := nil;
      Exit;
    end;
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
  else if Context.Check(ttOpAt) then
  begin
    // "@ref = expr": RESEATING a reference - pointing it at another object. The left side is the
    // reference's own storage, so it parses as the ordinary "@name" address node and the SSA writes
    // the pointer value into it. Only a reference variable can be on the left, and the SSA says so.
    LeftSide := FExpressionParser.ParseExpression(precUnary);
    LhsIsExpr := True;
    if not Assigned(LeftSide) then
    begin
      HandleError('Expected a reference after "@" in assignment', Context.CurrentToken);
      Result := nil;
      Exit;
    end;
  end
  else
  begin
    HandleError('Expected variable name in assignment', Context.CurrentToken);  // ← Token corrente
    Result := nil;
    Exit;
  end;

  // FreeBASIC keyword-operator compound assignment: "lhs MOD= rhs", "lhs AND/OR/XOR= rhs",
  // "lhs SHL/SHR= rhs", "lhs EQV/IMP= rhs". Unlike the symbolic forms (+= &= ...), the keyword stops
  // at the '=', so the lexer yields the operator token (ttOpMod/ttBitwiseAND/...) followed by a
  // separate ttOpEq. Detect by lookahead and desugar to "lhs = lhs op rhs".
  if Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttOpEq) and
     ((Context.CurrentToken.TokenType = ttOpMod) or
      (Context.CurrentToken.TokenType = ttOpShl) or
      (Context.CurrentToken.TokenType = ttOpShr) or
      (Context.CurrentToken.TokenType = ttBitwiseAND) or
      (Context.CurrentToken.TokenType = ttBitwiseOR) or
      (Context.CurrentToken.TokenType = ttBitwiseXOR) or
      (Context.CurrentToken.TokenType = ttOpEqv) or
      // ...and the SHORT-CIRCUIT pair. "i OrElse= 1" and "i AndAlso= 0" are the same spelling with the
      // same desugaring, and they were the only two keyword operators missing from this list - the
      // statement fell through to "Expected "=" in assignment", which names the very token that IS there.
      (Context.CurrentToken.TokenType = ttOpAndAlso) or
      (Context.CurrentToken.TokenType = ttOpOrElse) or
      (Context.CurrentToken.TokenType = ttOpImp)) then
  begin
    OpType := Context.CurrentToken.TokenType;
    OpSym := Context.CurrentToken.Value;
    Context.Advance;                                 // consume the operator keyword
    Context.Advance;                                 // consume the trailing "="
    Expression := FExpressionParser.ParseExpression;
    if not Assigned(Expression) then
    begin
      if Assigned(LeftSide) then LeftSide.Free;
      Result := nil;
      Exit;
    end;
    Expression := CreateBinaryOpNode(OpType, LeftSide.Clone, Expression,
                                     TLexerToken.CreateSimple(OpType, OpSym));
    Result := TASTNode.Create(antAssignment, SavedToken);
    Result.AddChild(LeftSide);
    Result.AddChild(Expression);
    // ...and the mark the SYMBOLIC branch below stamps, for the same reason: a UDT may overload the
    // SELF-operator ("Operator T.Mod= (rhs)"), which mutates in place and is not "x = x Mod rhs" -
    // there may be no binary Mod for the type at all. Without it the desugared form was lowered
    // against the record HANDLE and the statement did nothing visible: "x Mod= 5" left x unchanged
    // while "x += 3" ran the operator, one keyword apart. One rule, two spellings, one place each.
    Result.Attributes.Values['COMPOUNDOP'] := UpperCase(OpSym);
    DoNodeCreated(Result);
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
    Result := TASTNode.Create(antAssignment, SavedToken);
    Result.AddChild(LeftSide);
    Result.AddChild(Expression);
    // Remember that this assignment CAME FROM a compound "op=". A UDT may overload the self-operator
    // itself ("Operator Vector2D.*= (rhs As Single)"), which mutates in place and is NOT the same as
    // "c = c * 3" -- there may be no binary "*" at all, and the desugared form then multiplied the record
    // HANDLE. The SSA prefers the self-operator when the target is a UDT that has one.
    Result.Attributes.Values['COMPOUNDOP'] := OpSym;
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
      // ...unless it is a bare method CALL carrying arguments without parentheses -- "obj.Add 17.5", the
      // statement form FreeBASIC allows for any SUB. Left as an expression statement, the member access
      // called the method with NO arguments (which then read whatever was last staged) and the arguments
      // themselves were orphaned, one "Unhandled node type" warning each.
      if (LeftSide <> nil) and (LeftSide.NodeType = antMemberAccess) and
         (not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile])) then
      begin
        Result := TASTNode.Create(antArrayAccess, SavedToken);   // the shape a "name(args)" call has
        Result.AddChild(LeftSide);
        Result.AddChild(ParseArgumentList);
        DoNodeCreated(Result);
        Exit;
      end;
      // ...and the same statement with its FIRST argument in parentheses: "proc1 (3), 4". The expression
      // parser reads "proc1 (3)" as a complete call - the two spellings are identical up to that point -
      // and the arguments after the comma were ORPHANED: the call ran with one argument and the rest
      // became statements of their own ("Unhandled node type"), so "proc1 (3), 4" printed "3 0".
      // The group already parsed IS the first argument; the remaining ones join the same list.
      if (LeftSide <> nil) and (LeftSide.NodeType = antArrayAccess) and Context.Check(ttSeparParam) and
         (LeftSide.ChildCount >= 2) and
         (LeftSide.GetChild(1).NodeType in [antExpressionList, antArgumentList]) then
      begin
        BareArgs := LeftSide.GetChild(1);
        while Context.Check(ttSeparParam) do
        begin
          Context.Advance;                                     // ','
          BareArg := FExpressionParser.ParseExpression;
          if not Assigned(BareArg) then Break;
          BareArgs.AddChild(BareArg);
        end;
        Result := LeftSide;
        DoNodeCreated(Result);
        Exit;
      end;
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

  Result := TASTNode.Create(antAssignment, SavedToken);
  Result.AddChild(LeftSide);
  Result.AddChild(Expression);
  DoNodeCreated(Result);
end;

// === STATEMENT PARSING METHODS ===

function TPackratParser.ParsePrintStatement: TASTNode;
var
  Token: TLexerToken;
  Expr, FormatNode, UsingMarker, HandleNode: TASTNode;
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
      HandleNode := ParseFileNumberOperand;
      if Assigned(HandleNode) then
        Result.AddChild(HandleNode)
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
    // A separator with no preceding expression: FreeBASIC "Print ; x" / "Print , x" (a leading ';'/','
    // controls the start position/print zone) or consecutive separators. Emit the separator node and
    // continue, instead of letting the expression parse below fail on the ';' / ','.
    if Context.CheckAny([ttSeparParam, ttSeparOutput]) then
    begin
      SeparatorNode := TASTNode.CreateWithValue(antSeparator, Context.CurrentToken.Value, Context.CurrentToken);
      Result.AddChild(SeparatorNode);
      Context.Advance;                 // consume the leading/standalone separator
      Continue;
    end;
    // FreeBASIC mid-list "USING fmt;": a USING clause that appears after a leading separator or an item
    // (e.g. "Print , Using ""#.##""; x") switches the format applied to the value items that follow.
    // Carried as an antPrintUsing marker child (format at child 0); the SSA sets it as the current format.
    // The plain "Print Using ..." head form (whole statement) is handled above and is left untouched.
    if (not IsUsingFormat) and Context.Check(ttOutputCommand) and
       (UpperCase(VarToStr(Context.CurrentToken.Value)) = kUSING) then
    begin
      Context.Advance;                 // consume USING
      FormatNode := ParseExpression;   // format string
      UsingMarker := TASTNode.Create(antPrintUsing, Token);
      if Assigned(FormatNode) then UsingMarker.AddChild(FormatNode);
      Result.AddChild(UsingMarker);
      if Context.CheckAny([ttSeparParam, ttSeparOutput]) then Context.Advance;   // ; or , before values
      Continue;
    end;
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
  SeparatorNode, HandleNode: TASTNode;
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
    HandleNode := ParseFileNumberOperand;
    if Assigned(HandleNode) then
      Result.AddChild(HandleNode)
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

  // FreeBASIC graphics "GET (x1,y1)-(x2,y2), dst" — capture a screen rectangle into an image surface.
  // The leading '(' disambiguates from "GET A$" and "GET #n,...".
  if Context.Check(ttDelimParOpen) then
  begin
    Result.Free;
    Result := TASTNode.Create(antGfxGet, Token);
    Context.Advance;                                            // '('
    Result.AddChild(ParseExpression);                           // x1
    if Context.Check(ttSeparParam) then Context.Advance;        // ','
    Result.AddChild(ParseExpression);                           // y1
    if Context.Check(ttDelimParClose) then Context.Advance;     // ')'
    if Context.Check(ttOpSub) then Context.Advance;             // '-'
    if Context.Check(ttDelimParOpen) then Context.Advance;      // '('
    Result.AddChild(ParseExpression);                           // x2
    if Context.Check(ttSeparParam) then Context.Advance;        // ','
    Result.AddChild(ParseExpression);                           // y2
    if Context.Check(ttDelimParClose) then Context.Advance;     // ')'
    if Context.Check(ttSeparParam) then Context.Advance;        // ','
    Result.AddChild(ParseExpression);                           // dst image handle
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC binary "GET #n, [pos], var" — read sizeof(var) bytes into a scalar.
  if Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#') then
  begin
    Result.Free;
    Result := ParseBinaryFileTail(True, Token);
    Exit;
  end;

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
  Assignment, Targets, Tgt: TASTNode;
  SavedToken: TLexerToken;
begin
  SavedToken := Context.CurrentToken;
  Context.Advance; // Consume LET (if present)

  // FreeBASIC "Let(a, b, ...) = udt": DESTRUCTURING. The parenthesised list is a list of DESTINATIONS,
  // not an expression, so it cannot go through the ordinary assignment parse (which would read "(a, b)"
  // as one expression and stop at the comma). antLetList: child0 = the target list, child1 = the source.
  if Context.Check(ttDelimParOpen) and FModernMode then
  begin
    Context.Advance;                                     // (
    Targets := TASTNode.Create(antExpressionList, SavedToken);
    repeat
      Tgt := FExpressionParser.ParseExpression;
      if not Assigned(Tgt) then Break;
      Targets.AddChild(Tgt);
      if Context.Check(ttSeparParam) then Context.Advance else Break;
    until Context.CheckAny([ttDelimParClose, ttEndOfLine, ttEndOfFile]);
    if not Context.Match(ttDelimParClose) then
    begin
      HandleError('Expected ")" after the LET target list', Context.CurrentToken);
      Targets.Free; Exit(nil);
    end;
    if not Context.Match(ttOpEq) then
    begin
      HandleError('Expected "=" after the LET target list', Context.CurrentToken);
      Targets.Free; Exit(nil);
    end;
    Assignment := FExpressionParser.ParseExpression;
    if not Assigned(Assignment) then
    begin
      HandleError('Expected a source value after "="', Context.CurrentToken);
      Targets.Free; Exit(nil);
    end;
    Result := TASTNode.Create(antLetList, SavedToken);
    Result.AddChild(Targets);
    Result.AddChild(Assignment);
    Exit;
  end;

  // Parse assignment expression. NOT wrapped in FInConstDecl: "Let K = 9" on a constant must be
  // rejected exactly like the bare form.
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

 // *** FreeBASIC/QuickBASIC block IF: "IF cond THEN" with nothing after THEN opens a
 //     multi-line block, closed by ELSE / ENDIF. Parsed self-contained here (so nesting
 //     just recurses); the IF is popped when ENDIF is consumed. "Nothing after THEN" is
 //     either end-of-line OR a ':' statement separator — the latter is how a block IF
 //     written on one line ("IF c THEN : ... : END IF") appears, and how a multi-line
 //     #macro body reads once its lines are joined with ':'. ***
 if Context.CheckAny([ttEndOfLine]) or Context.Check(ttSeparStmt) then
 begin
   if Context.Check(ttSeparStmt) then Context.Advance;  // consume the ':' that opened the block
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

   // FreeBASIC compact block IF written on one line: "IF cond THEN stmt : END IF". The single-line THEN
   // body may be terminated by an END IF (the author closed a one-line block IF). Consume it and pop the
   // IF, so the trailing END IF is not left as a stray token (which failed to parse).
   if Context.Check(ttBlockEnd) or
      (Context.Check(ttProgramEnd) and Assigned(Context.PeekNext) and
       (Context.PeekNext.TokenType = ttConditionalIf)) then
   begin
     ConsumeBlockIfTerminator;
     if FValidationStacks.HasActiveIf then FValidationStacks.PopIf;
     Break;
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
  PrevIdx, StartDepth: Integer;
begin
  // Collect statements across lines into Parent until ELSE / ENDIF (ttBlockEnd) /
  // end-of-file. A nested block IF is parsed by ParseStatement -> ParseThenStatement,
  // which consumes its own ENDIF, so only this body's own ELSE/ENDIF stops us here.
  // StartDepth = the IF-stack depth on entry (the block IF that owns this body is already on it). A nested
  // single-line "IF ... THEN ... ELSE ..." pushes above StartDepth and leaves its ELSE for the statement
  // dispatcher; that ELSE must NOT terminate this body — only an ELSE at StartDepth is ours.
  StartDepth := FValidationStacks.IfStackDepth;
  while not Context.Check(ttEndOfFile) do
  begin
    if Context.Match(ttEndOfLine) then
    begin
      // Close any nested single-line IFs that ended on this line, but never the block IF that owns this
      // body (guard with IfStackDepth > StartDepth) — it is closed by its own ENDIF.
      while FValidationStacks.HasActiveIf and (FValidationStacks.IfStackDepth > StartDepth) and
            FValidationStacks.CanPopIfAtEOL do
        FValidationStacks.PopIf;
      Continue;                                         // skip line breaks
    end;
    if Context.Check(ttSeparStmt) then
    begin
      Context.Advance;                                  // skip ':' separators
      Continue;
    end;
    // An ELSE terminates this body only if it is at our own IF depth. A deeper ELSE belongs to a nested
    // single-line IF still open on the stack: fall through and let ParseStatement dispatch it to ParseElse.
    if Context.Check(ttConditionalElse) and (FValidationStacks.IfStackDepth <= StartDepth) then
      Break;
    if AtBlockIfTerminator then
      Break;                                            // ENDIF / END IF ends this body
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
  Kind, MethodType, QualName, ParamMode, OpSym, OpOwnerType, DecoU, RetTypeName, ParamTypeName, ParamNameU: string;
  ProcPtrRet: TASTNode;
  OpSymbolForm: Boolean;   // "OPERATOR <sym>(...)" (arity goes in the label) vs "OPERATOR T.CAST/LET"
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
  OpSymbolForm := False;
  if Kind = kOPERATOR then
  begin
    // Two OPERATOR forms:
    //   Symbol form "OPERATOR <sym> (a AS T, b AS T) AS R" — a global binary op; the owning type is
    //     derived from the first parameter after the list is parsed (label "<T>.OPERATOR<sym>"). No
    //     implicit THIS: it is a 2-argument function resolved by operand type at the binary op.
    //   Method form "OPERATOR <Type>.<name>() AS R" — a FreeBASIC conversion/assignment operator such as
    //     "Operator T.Cast() As String": behaves like a method (implicit THIS AS Type). Set MethodType +
    //     OpSym; the THIS injection and the OPERATOR post-processing below form the "<Type>.OPERATOR<name>"
    //     label (owner read from THIS's type) exactly like the symbol form. Distinguished by "ident '.'".
    if Context.Check(ttIdentifier) and Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttOpDot) then
    begin
      MethodType := UpperCase(VarToStr(Context.CurrentToken.Value));
      Context.Advance;                              // <Type>
      Context.Advance;                              // '.'
      OpSym := UpperCase(VarToStr(Context.CurrentToken.Value));   // operator name (CAST, LET, *=, ...)
      // A SELF-operator ("Operator T.*= (rhs)") arrives as the lexer's compound-assign token, whose value
      // is the bare symbol -- "*", not "*=". Spell the "=" back in, or the operator would be labelled
      // exactly like the binary "*" and the two could not be told apart.
      if Context.Check(ttCompoundAssign) then OpSym := OpSym + '=';
      Context.Advance;                              // operator name
      // ...and the KEYWORD self-operators, "Operator T.Mod= (rhs)" and its family. A keyword operator
      // stops at the '=', so the lexer yields the operator token and a SEPARATE ttOpEq - the same
      // shape the assignment grammar already has to undo for "lhs Mod= rhs". Without spelling the '='
      // back in here the '=' was left where a parameter list was expected and derailed the statement.
      if ((OpSym = kMOD) or (OpSym = 'SHL') or (OpSym = 'SHR') or (OpSym = 'AND') or (OpSym = 'OR') or
          (OpSym = 'XOR') or (OpSym = 'EQV') or (OpSym = 'IMP')) and Context.Check(ttOpEq) then
      begin
        OpSym := OpSym + '=';
        Context.Advance;                            // '='
      end;
      // The INDEX operator "Operator T.[] (i) ByRef As E" is written with two delimiter tokens, not one
      // name: '[' was taken as the whole operator and the ']' left behind derailed the statement. It is a
      // method with an implicit THIS, like CAST and LET, so it only needs its name spelled whole.
      if (OpSym = '[') and Context.Check(ttDelimBrackClose) then
      begin
        OpSym := '[]';
        Context.Advance;                            // ']'
      end;
      // The ARRAY forms of the allocation operators, "Operator T.New[] (...)" and "Operator
      // T.Delete[] (...)": the name is a word FOLLOWED by the two bracket tokens, so the '[' was left
      // where a parameter list was expected and derailed the statement. The brackets are part of the
      // NAME and must stay in it - "New" and "New[]" are two different operators, and one label for
      // both would let the second definition overwrite the first.
      if ((OpSym = kNEW) or (OpSym = kDELETE)) and Context.Check(ttDelimBrackOpen) and
         Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttDelimBrackClose) then
      begin
        OpSym := OpSym + '[]';
        Context.Advance;                            // '['
        Context.Advance;                            // ']'
      end;
    end
    else
    begin
      OpSym := Context.CurrentToken.Value;
      if Context.Check(ttCompoundAssign) then OpSym := OpSym + '=';   // as above, for the global form
      OpSymbolForm := True;                         // "OPERATOR <sym> (...)": arity-overloadable
      Context.Advance;                              // consume the operator symbol
    end;
    NameNode := TASTNode.CreateWithValue(antIdentifier, 'OPERATOR' + OpSym, Token);  // placeholder
    Result.AddChild(NameNode);
  end
  else
  begin
  // The procedure/owner name must be a plain identifier. A reserved word here (e.g. a graphics
  // keyword such as CIRCLE/BOX/LINE used as a type name) is malformed: report a clean error and
  // skip the body up to its END, so the parser terminates instead of derailing on a misaligned
  // token stream. (The method name *after* the dot may be a reserved word — handled below.)
  // ⭐ A MODERN EXTENSION IS NOT A FreeBASIC KEYWORD, so it must not reserve the name. MIN, MAX,
  // CEIL, ROUND, COPYSIGN, SINGLEBITS and BITSTOSINGLE are ours - the IEEE operations WASM has an
  // instruction for - and fbc accepts every one of them as a procedure name (checked against fbc,
  // 24 Aug 2026). Reserving them made "Function min(...)" a syntax error, which is a real FreeBASIC
  // program failing to compile on a name FreeBASIC leaves free.
  //
  // ⭐ Nothing else is needed: SedaiSSA consults FProcedureNames BEFORE the intrinsic chain, so once
  // the declaration is accepted the user's procedure already wins at every call site. The extension
  // stays available to every program that does not declare one.
  if FModernMode and (not Context.Check(ttIdentifier)) and
     IsShadowableExtensionName(UpperCase(Context.CurrentToken.Value)) then
    Context.CurrentToken.TokenType := ttIdentifier;

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
    // ⭐ A NESTED UDT OWNS METHODS TOO, and then the owner has a dotted name of its own:
    // "Sub T.U.proc(...)" is the method proc of the Union U declared inside Type T. One dot was
    // consumed and the next one had no object, so the whole declaration was a syntax error. The
    // owner is everything before the LAST name, which is what a nested type is called.
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
        while Context.Check(ttOpDot) do
        begin
          Context.Advance;                        // '.' of a deeper qualification
          if not (Context.Check(ttIdentifier) or
                  ((Length(Context.CurrentToken.Value) > 0) and
                   (UpCase(Context.CurrentToken.Value[1]) in ['A'..'Z', '_']))) then Break;
          MethodType := QualName;                 // the owner grows; the last name stays the method
          QualName := MethodType + '.' + UpperCase(Context.CurrentToken.Value);
          Context.Advance;
        end;
      end;
    end;
    NameNode := TASTNode.CreateWithValue(antIdentifier, QualName, NameTok);
    Result.AddChild(NameNode);
  end;
  end;   // end of non-OPERATOR name parsing

  // FreeBASIC procedure decorators between the name and the parameter list: calling conventions
  // (CDECL/STDCALL/PASCAL/FASTCALL/THISCALL), OVERLOAD, ALIAS "name", LIB "name". SedaiBasic has a
  // single internal calling convention and no external (C) linking, so these are accepted and
  // ignored — they exist so that real FreeBASIC declarations parse. (Any other identifier ends the
  // loop, so a one-line body starting with an identifier is unaffected.)
  while Context.Check(ttIdentifier) do
  begin
    DecoU := UpperCase(Context.CurrentToken.Value);
    if (DecoU = 'CDECL') or (DecoU = 'STDCALL') or (DecoU = 'PASCAL') or
       (DecoU = 'FASTCALL') or (DecoU = 'THISCALL') or (DecoU = 'OVERLOAD') then
      Context.Advance
    else if (DecoU = kALIAS) or (DecoU = kLIB) then
    begin
      Context.Advance;                            // ALIAS / LIB
      if Context.Check(ttStringLiteral) then Context.Advance;   // "name" (discarded)
    end
    else
      Break;
  end;

  ParamList := TASTNode.Create(antParameterList, Token);
  // Implicit THIS parameter for methods: THIS AS <Type> (record handle), first in the list.
  // ...except for a STATIC member procedure, which is called WITHOUT an instance: the TYPE body declared
  // it "Declare Static Sub f(...)", so giving its definition a THIS would shift every argument one slot
  // to the right and give @Type.f an arity no call site matches.
  if (MethodType <> '') and (FTypeStaticMethods.IndexOf(QualName) < 0) then
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
      // QuickBASIC-style "OPTIONAL" keyword before a parameter. FreeBASIC has no such keyword — an
      // optional parameter is expressed by giving a default directly ("name AS T = expr"), which is
      // handled below. Skip a leading OPTIONAL so it is not mistaken for a bare (untyped) parameter
      // named "OPTIONAL", which would shift every following argument by one slot. Only skip when it is
      // followed by another name or a BYVAL/BYREF qualifier, so a parameter literally named "optional"
      // ("optional AS T") is preserved.
      if Context.Check(ttIdentifier) and (UpperCase(Context.CurrentToken.Value) = 'OPTIONAL') and
         Assigned(Context.PeekNext) and
         ((Context.PeekNext.TokenType = ttIdentifier) or (Context.PeekNext.TokenType = ttParamMode)) then
        Context.Advance;                            // consume OPTIONAL keyword

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
        // FreeBASIC array parameter: "name() AS type" (empty parens; arrays are always passed ByRef,
        // with unspecified bounds). Consume the "()" and mark the parameter as an array. Without this
        // the '(' is never consumed and the parameter loop spins forever.
        if Context.Check(ttDelimParOpen) then
        begin
          Context.Advance;                        // (
          while not Context.CheckAny([ttDelimParClose, ttEndOfLine, ttEndOfFile, ttSeparStmt]) do
            Context.Advance;                      // skip anything inside (usually empty)
          if Context.Check(ttDelimParClose) then Context.Advance;   // )
          ParamNode.Attributes.Values['ARRAY'] := '1';
        end;
        // Optional "AS typename" (M3.1): attach the type as a child antIdentifier so the
        // SSA pre-scan can type the parameter (record handle / explicit builtin bank).
        ParamTypeName := '';
        if Context.Check(ttAsType) then
        begin
          Context.Advance;                        // AS
          if SkipTypeQualifiersConst then         // FB: "As Const <type>" - part of the SIGNATURE
            ParamNode.Attributes.Values['CONSTP'] := '1';
          // FreeBASIC "AS CONST <type>": a read-only (immutable) parameter. Immutability is not enforced
          // here, so consume and ignore the CONST qualifier and take the type that follows — otherwise
          // CONST (a keyword, not an identifier) is skipped and the parameter is left untyped (mis-banked
          // to numeric, so a "Const String" arg reads as 0).
          if Context.Check(ttConstant) then Context.Advance;   // optional CONST qualifier
          // FreeBASIC function-pointer parameter "f AS FUNCTION(...) AS ret": the parameter is an int
          // (a procedure entry PC); the signature is recorded on the node, no UDT type child attached.
          if TryParseProcPtrType(ParamNode) then
            ParamTypeName := ''
          else if Context.Check(ttIdentifier) then
          begin
            RetTok := Context.CurrentToken;
            ParamTypeName := UpperCase(ParseDottedName);
            // FreeBASIC pointer parameter "<type> PTR" (one or more PTR): keep the PTR suffix on the type
            // name (the pointee bank is recorded from it) and — crucially — CONSUME the PTR token(s). Left
            // unconsumed, a following parameter list ("..., x As Integer") mis-parses: the stray "PTR" is
            // taken as the next parameter, so every parameter after a pointer one is mis-slotted (its
            // transfer slot no longer matches the caller's staging). Applies to array-of-pointer params
            // ("a() As T PTR") too.
            while AtPointerSuffix do
            begin
              ParamTypeName := ParamTypeName + ' PTR';
              Context.Advance;                      // consume PTR
            end;
            ParamNode.AddChild(TASTNode.CreateWithValue(antIdentifier,
                         ParamTypeName, RetTok));  // dotted: namespace-qualified param type
          end;
        end;
        // FreeBASIC -lang fb default (MODERN): String / ZString / WString parameters are passed BYREF by
        // default (the callee's mutations propagate back to the caller's argument), unless BYVAL/BYREF
        // was given explicitly. Numeric scalars stay BYVAL, matching FB. A bare "name$" (no AS type) is a
        // string too. CLASSIC keeps its own convention (untouched).
        if FModernMode and (ParamMode = '') then
        begin
          ParamNameU := UpperCase(VarToStr(ParamNode.Value));
          if (ParamTypeName = 'STRING') or (ParamTypeName = 'ZSTRING') or (ParamTypeName = 'WSTRING') or
             ((ParamTypeName = '') and (Length(ParamNameU) > 0) and (ParamNameU[Length(ParamNameU)] = '$')) then
            ParamNode.Attributes.Values['BYREF'] := '1';
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
      // FreeBASIC variadic tail "...": the declaration accepts any number of further arguments. It used
      // to fall into the defensive skip below, so the dots vanished and the procedure looked ordinary -
      // the surplus arguments at every call site were then dropped in silence.
      else if Context.Check(ttOpDot) then
      begin
        while Context.Check(ttOpDot) do Context.Advance;        // "..."
        Result.Attributes.Values['VARIADIC'] := '1';
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
    // ⛔ THE RESULT TYPE TELLS THEM APART, NOT THE PARAMETERS. This used to read "any explicit
    // parameter means setter", which is right for the common pair and wrong for an INDEXED property:
    //
    //   Property NumBit( ByVal Index As Integer ) As Integer           '' getter, and it HAS a param
    //   Property NumBit( ByVal Index As Integer, ByVal Value As Byte ) '' setter
    //
    // Read as a setter, the getter became a SUB and its "As Integer" then had nowhere to go:
    // "Unexpected token in statement: As". FreeBASIC's own rule is the presence of a result type.
    if Context.Check(ttAsType) then
    begin
      Kind := kFUNCTION;                           // getter returns the property value
      Result.Value := kFUNCTION;
    end
    else
    begin
      Kind := kSUB;
      Result.Value := kSUB;
      NameNode.Value := QualName + '.SET';
    end;
  end;

  // OPERATOR: now that the parameters are parsed, take the owning type from the first parameter's
  // AS-type and form the label "<T>.OPERATOR<sym>", then treat it as a normal FUNCTION. The binary-op
  // lowering resolves it by the left operand's type.
  //
  // The symbol form also carries its ARITY in the label ("@1"/"@2"), exactly as a CONSTRUCTOR carries
  // its parameter count. A type routinely overloads one symbol both ways -- "Operator -(c As T)" to
  // negate and "Operator -(a As T, b As T)" to subtract -- and with a single shared label only the
  // first survived registration: the binary one was dropped, so "x - y" called the unary declaration
  // with two arguments and quietly evaluated to "-x" (Rosetta "Arithmetic/Complex"). It is done HERE,
  // in the parser, because the pre-scans that record a procedure's return type run before the SSA
  // collector and must already see the final label. The named form (CAST/LET) keeps its own scheme:
  // it takes no explicit parameters, so arity cannot tell two of them apart.
  if (Kind = kOPERATOR) and Assigned(NameNode) and (ParamList.ChildCount >= 1) and
     (ParamList.GetChild(0).ChildCount >= 1) then
  begin
    OpOwnerType := UpperCase(VarToStr(ParamList.GetChild(0).GetChild(0).Value));
    NameNode.Value := OpOwnerType + '.OPERATOR' + OpSym;
    if OpSymbolForm then
      NameNode.Value := VarToStr(NameNode.Value) + '@' + IntToStr(ParamList.ChildCount)
    // The ITERATION operators are the named form's exception: FreeBASIC defines each of FOR/STEP/NEXT
    // twice, once with the step variable and once without ("implicit step"), and a type routinely
    // declares BOTH - examples/manual/udt/step-char-iterator does. One shared label would drop the
    // second declaration in silence, exactly as it did for the symbol operators before they carried
    // their arity. Counted WITHOUT the implicit THIS, so it reads as the source writes it.
    else if (OpSym = kFOR) or (OpSym = kSTEP) or (OpSym = kNEXT) then
      NameNode.Value := VarToStr(NameNode.Value) + '@' + IntToStr(ParamList.ChildCount - 1);
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
    SkipTypeQualifiers;                     // FB: "As Const <type>"
    // "Function f(...) As Sub()" / "As Function(...) As R": the return is a PROCEDURE POINTER, which is
    // not an identifier - so this reader passed it by and the SUB keyword was met where a name was
    // expected. A PARAMETER of that type has always been read (TryParseProcPtrType); the RETURN had not,
    // the same rule in one path and not its sibling. What comes back is an entry address: int-banked.
    if Context.Check(ttProcedureStart) then
    begin
      RetTok := Context.CurrentToken;
      ProcPtrRet := TASTNode.Create(antArrayDecl, RetTok);
      try
        TryParseProcPtrType(ProcPtrRet);
      finally
        ProcPtrRet.Free;
      end;
      NameNode.AddChild(TASTNode.CreateWithValue(antIdentifier, 'INTEGER', RetTok));
    end
    else if Context.Check(ttIdentifier) then
    begin
      RetTok := Context.CurrentToken;
      RetTypeName := ParseDottedName;               // dotted: namespace-qualified return type
      // FreeBASIC pointer return type: "<type> PTR" (e.g. "FUNCTION f() AS Tree PTR"). Consume the PTR
      // suffix and keep it on the type name so the pre-scan records a pointer (int-handle) return.
      while AtPointerSuffix do
      begin
        RetTypeName := RetTypeName + ' PTR';
        Context.Advance;                            // consume PTR
      end;
      NameNode.AddChild(TASTNode.CreateWithValue(antIdentifier, RetTypeName, RetTok));
    end;
  end;

  // FreeBASIC EXPORT: "SUB s (...) EXPORT" / "FUNCTION f (...) AS T EXPORT" asks the compiler to put the
  // symbol in a shared library's export table. It follows the parameter list (and, for a FUNCTION, the
  // return type), which is why it is consumed here rather than in the decorator loop above. SedaiBasic
  // produces bytecode, not a DLL, so there is no export table: accept and ignore, so that real FreeBASIC
  // sources parse. EXPORT is not a reserved word, so a variable may still be called "export".
  if Context.Check(ttIdentifier) and (UpperCase(VarToStr(Context.CurrentToken.Value)) = kEXPORT) then
    Context.Advance;

  // Arity-based constructor overloading (M4.4d): encode the explicit-parameter count in the label
  // (THIS excluded) so multiple CONSTRUCTORs of the same type get distinct procedure labels, e.g.
  // "TYPE.CONSTRUCTOR#0", "TYPE.CONSTRUCTOR#2". The call site resolves by argument count.
  // The signature, not merely the arity: "Constructor sample(a As Integer)" and "(a As Single)" have the
  // same count and must still get distinct labels. It is spelled out HERE, in the parser, for the same
  // reason the operator and overload labels are: the pre-scans that record a procedure's parameter banks
  // and return type run before the SSA collector, and if the label changes underneath them they key their
  // entries under a name nobody will look up again. That is what left a "As Single" constructor parameter
  // printing 16 digits and a "As String" one reading 0.
  // ...and the UDT type TAIL for the same reason a SUB/FUNCTION overload needs one: every UDT is an int
  // HANDLE, so "Constructor(v As S)" and "Constructor(v As T)" both signed "#I" and the second was
  // silently discarded. The call site (EmitConstructorCall) rebuilds the tail from its ARGUMENT nodes.
  // FreeBASIC states a method's DEFAULT ARGUMENTS on its in-TYPE declaration, never here. Replay them
  // onto the definition before anything reads the parameter list: the label signature is unaffected
  // (defaults do not change a parameter's bank), but every "callable with N arguments" question asked
  // downstream — starting with whether a constructor can run with none — is decided by this list.
  if MethodType <> '' then
    ApplyDeclaredDefaults(QualName, ParamList, FTypeStaticMethods.IndexOf(QualName) < 0);

  if (Kind = kCONSTRUCTOR) and Assigned(NameNode) then
    NameNode.Value := QualName + '#' + ProcSigFromParams(ParamList, True, True, True);   // True: skip the implicit THIS

  Result.AddChild(ParamList);

  // FreeBASIC OVERLOAD: two SUB/FUNCTIONs (or two methods of one type) may share a name and differ only
  // in their parameter types -- "Function g(As Long)" and "Function g(As Single)", the accumulator-factory
  // idiom. They produced ONE label, and the SSA's registration keeps the first it sees, so the second was
  // dropped outright: every call went to the first, and "x.g(2.3)" silently truncated 2.3 to a Long.
  //
  // On the SECOND declaration of a name, append a parameter-bank signature to BOTH (the first is still
  // reachable here, un-renamed) -- "BAR.G~I" / "BAR.G~F". A name declared only once keeps its bare label,
  // so nothing about a non-overloaded program changes. Constructors ("#sig") and operators ("@arity")
  // already carry their own discriminator and are left alone.
  RegisterOverloadLabel(Result, NameNode, ParamList, MethodType <> '');

  // FreeBASIC module-level constructor/destructor: "Sub name [()] Constructor [priority]" runs before
  // module-level code; "Destructor [priority]" runs after it (at program end). Only on a plain SUB (not a
  // Type.method — MethodType = ''); the optional integer priority (101..65535) orders multiple ctors/dtors
  // (lower = earlier for ctors). Marked on the node for the SSA to collect and call at program start/end.
  if (Context.CurrentToken <> nil) and (MethodType = '') and
     ((UpperCase(VarToStr(Context.CurrentToken.Value)) = kCONSTRUCTOR) or
      (UpperCase(VarToStr(Context.CurrentToken.Value)) = kDESTRUCTOR)) then
  begin
    if UpperCase(VarToStr(Context.CurrentToken.Value)) = kCONSTRUCTOR then
      Result.Attributes.Values['MODCTOR'] := '1'
    else
      Result.Attributes.Values['MODDTOR'] := '1';
    Context.Advance;                                // consume Constructor / Destructor
    // optional integer priority immediately after the keyword
    if (Context.CurrentToken <> nil) and (Length(VarToStr(Context.CurrentToken.Value)) > 0) and
       (VarToStr(Context.CurrentToken.Value)[1] in ['0'..'9']) then
    begin
      Result.Attributes.Values['MODPRIORITY'] := VarToStr(Context.CurrentToken.Value);
      Context.Advance;
    end;
  end;

  // FreeBASIC trailing procedure modifiers after the signature: "[Static] [Export]". STATIC makes ALL
  // local variables in the body persistent between calls (marked ALLSTATIC; the STATIC-locals lowering
  // treats each scalar local as static). EXPORT is accepted and ignored. Placed here (after the return
  // type) so it is on the signature line, before the body — distinct from a body-level "Static name AS T".
  while (Context.CurrentToken <> nil) and
        ((UpperCase(VarToStr(Context.CurrentToken.Value)) = 'STATIC') or
         (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'EXPORT')) do
  begin
    if UpperCase(VarToStr(Context.CurrentToken.Value)) = 'STATIC' then
      Result.Attributes.Values['ALLSTATIC'] := '1';
    Context.Advance;
  end;

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
      // An OMITTED argument, which is how FreeBASIC's own manual writes the default:
      //     Clear array(0), , 100 * SizeOf(Integer)
      // The parenthesised list (ParseExpressionList) has always stood an empty antLiteral in that
      // position; this loop instead handed the comma itself to ParseExpression, whose prefix rule for a
      // separator is a debug stub - so the argument list came out short and shifted, the byte count was
      // read from the wrong place, and the program printed "[SSA] WARNING: Unhandled node type" on the
      // way past. Same spelling, same node: the two paths must agree on what "nothing" is.
      if Context.Check(ttSeparParam) then
      begin
        ArgList.AddChild(TASTNode.Create(antLiteral));
        Context.Advance;                          // ,
        if Context.Check(ttEndOfFile) or Context.Check(ttEndOfLine) or Context.Check(ttSeparStmt) then Break;
        Continue;
      end;
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

function TPackratParser.ParenGroupIsFollowedByAs(Offset: Integer): Boolean;
// Is the token at Offset an opening parenthesis whose MATCHING close is followed by "As"? That is the
// shape of a parameter list with a return type - "( ByRef lhs As T, ByRef rhs As U ) As R" - and it is
// what tells the declaration of the equality operator from "Operator = <expr>", which begins with the
// same two tokens. Bounded: it stops at the end of the statement.
var
  i, Depth: Integer;
  T: TLexerToken;
begin
  Result := False;
  T := Context.PeekToken(Offset);
  if not Assigned(T) or (T.TokenType <> ttDelimParOpen) then Exit;
  Depth := 0;
  i := Offset;
  while True do
  begin
    T := Context.PeekToken(i);
    if not Assigned(T) then Exit;
    if T.TokenType in [ttEndOfFile] then Exit;
    if T.TokenType = ttDelimParOpen then Inc(Depth)
    else if T.TokenType = ttDelimParClose then
    begin
      Dec(Depth);
      if Depth = 0 then
      begin
        T := Context.PeekToken(i + 1);
        Result := Assigned(T) and (T.TokenType = ttAsType);
        Exit;
      end;
    end;
    Inc(i);
  end;
end;

function TPackratParser.ParseBareCallStatement: TASTNode;
// FreeBASIC/QB statement-level SUB call without CALL or parentheses: "SubName arg1, arg2, ...".
// The name is already the current token; arguments are an unparenthesised comma-separated list. Builds
// the same antProcedureCall (child = antArgumentList) that CALL produces, so SSA lowering is shared.
var
  Token, NameTok: TLexerToken;
  ArgList, ArgExpr: TASTNode;
begin
  NameTok := Context.CurrentToken;
  Token := NameTok;
  Result := TASTNode.CreateWithValue(antProcedureCall, UpperCase(NameTok.Value), NameTok);
  Context.Advance;                                // consume the SUB name
  ArgList := TASTNode.Create(antArgumentList, Token);
  Result.AddChild(ArgList);
  if not (Context.Check(ttEndOfFile) or Context.Check(ttEndOfLine) or Context.Check(ttSeparStmt)) then
  begin
    repeat
      // An OMITTED argument - see the same handling in ParseCallStatement, and why the two spellings
      // have to agree on what "nothing" is.
      if Context.Check(ttSeparParam) then
      begin
        ArgList.AddChild(TASTNode.Create(antLiteral));
        Context.Advance;                          // ,
        if Context.Check(ttEndOfFile) or Context.Check(ttEndOfLine) or Context.Check(ttSeparStmt) then Break;
        Continue;
      end;
      ArgExpr := FExpressionParser.ParseExpression;
      if not Assigned(ArgExpr) then Break;
      ArgList.AddChild(ArgExpr);
      if Context.Check(ttSeparParam) then
        Context.Advance                           // ,
      else
        Break;
    until False;
  end;
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
  // END TYPE / END UNION  (END is ttProgramEnd, TYPE is ttTypeDecl, UNION is ttUnionDecl)
  // ...and END INTERFACE, which is MODERN's own: INTERFACE is not a reserved word, so it arrives as
  // a plain identifier and has to be matched by spelling.
  Result := Context.Check(ttProgramEnd) and Assigned(Context.PeekNext) and
            ((Context.PeekNext.TokenType = ttTypeDecl) or
             (Context.PeekNext.TokenType = ttUnionDecl) or
             (UpperCase(VarToStr(Context.PeekNext.Value)) = 'INTERFACE'));
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
var
  BaseU: string;
begin
  // FreeBASIC "UNSIGNED <basetype>" modifier: map to the unsigned variant type name. A bare
  // "UNSIGNED" (no integer base type following) means UNSIGNED INTEGER. UNSIGNED is not a reserved
  // keyword (it tokenizes as an identifier), so handle it here at the central type-name reader.
  if UpperCase(VarToStr(Context.CurrentToken.Value)) = 'UNSIGNED' then
  begin
    Context.Advance;                                 // consume UNSIGNED
    BaseU := UpperCase(VarToStr(Context.CurrentToken.Value));
    if (BaseU = 'INTEGER') or (BaseU = 'BYTE') or (BaseU = 'SHORT') or
       (BaseU = 'LONG') or (BaseU = 'LONGINT') then
    begin
      Context.Advance;                               // consume the base type
      case BaseU of
        'BYTE':    Result := 'UBYTE';
        'SHORT':   Result := 'USHORT';
        'LONG':    Result := 'ULONG';
        'LONGINT': Result := 'ULONGINT';
      else
        Result := 'UINTEGER';
      end;
    end
    else if (BaseU = 'UINTEGER') or (BaseU = 'UBYTE') or (BaseU = 'USHORT') or
            (BaseU = 'ULONG') or (BaseU = 'ULONGINT') then
    begin
      Context.Advance;                               // already unsigned: keep it
      Result := BaseU;
    end
    else
      Result := 'UINTEGER';                          // bare UNSIGNED = UNSIGNED INTEGER
    Exit;
  end;

  Result := UpperCase(VarToStr(Context.CurrentToken.Value));
  Context.Advance;                                   // first segment
  // FreeBASIC EXPLICIT-WIDTH integer: "Integer<8>" / "UInteger<16>" name the same types BYTE..LONGINT
  // by their bit count. Read here, at the central type-name reader, for the same reason UNSIGNED is:
  // every declaration form asks this one question. Left unread, the '<' looked like a comparison and
  // "Dim As Integer<8> b" failed as "Expected variable name".
  if ((Result = 'INTEGER') or (Result = 'UINTEGER')) and Context.Check(ttOpLt) and
     Assigned(Context.PeekNext) and (Context.PeekNext.TokenType in [ttNumber, ttInteger]) then
  begin
    Context.Advance;                                 // '<'
    BaseU := VarToStr(Context.CurrentToken.Value);
    Context.Advance;                                 // the bit count
    if Context.Check(ttOpGt) then Context.Advance;   // '>'
    if Result = 'INTEGER' then
      case StrToIntDef(BaseU, 0) of
         8: Result := 'BYTE';
        16: Result := 'SHORT';
        32: Result := 'LONG';
        64: Result := 'LONGINT';
      end
    else
      case StrToIntDef(BaseU, 0) of
         8: Result := 'UBYTE';
        16: Result := 'USHORT';
        32: Result := 'ULONG';
        64: Result := 'ULONGINT';
      end;
    Exit;
  end;
  while Context.Check(ttOpDot) and Assigned(Context.PeekNext) and
        (Length(VarToStr(Context.PeekNext.Value)) > 0) and
        (UpCase(VarToStr(Context.PeekNext.Value)[1]) in ['A'..'Z', '_']) do
  begin
    Context.Advance;                                 // '.'
    Result := Result + '.' + UpperCase(VarToStr(Context.CurrentToken.Value));
    Context.Advance;                                 // segment
  end;
end;

function TPackratParser.TryParseProcPtrType(Node: TASTNode): Boolean;
// FreeBASIC function-pointer type after AS: "FUNCTION(params) AS ret" or "SUB(params)". The cursor is
// on the FUNCTION/SUB keyword. Records the signature on Node (FUNCPTR='1'; FPPARAMS = comma list of the
// parameter type names; FPRET = return type name, '' for SUB). The variable itself is int-banked (it
// holds a procedure entry PC), so no type child is attached. Returns False (consuming nothing) if the
// current token is not FUNCTION/SUB.
var
  IsFunc: Boolean;
  KindU, PT, ParamTypes: string;
begin
  Result := False;
  if not Context.Check(ttProcedureStart) then Exit;
  KindU := UpperCase(VarToStr(Context.CurrentToken.Value));
  if (KindU <> kFUNCTION) and (KindU <> kSUB) then Exit;
  IsFunc := (KindU = kFUNCTION);
  Context.Advance;                                   // consume FUNCTION / SUB
  // A CALLING CONVENTION may stand between the keyword and the parameter list here exactly as it may in
  // a declaration ("Dim f As Sub CDecl ()"). The declaration path has skipped these all along; the TYPE
  // did not, so the convention was read as the parameter list's opening name and the whole declaration
  // fell apart. One internal convention here, so they are accepted and ignored - as in the other path.
  while Context.Check(ttIdentifier) and
        ((UpperCase(VarToStr(Context.CurrentToken.Value)) = 'CDECL') or
         (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'STDCALL') or
         (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'PASCAL') or
         (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'FASTCALL') or
         (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'THISCALL')) do
    Context.Advance;
  ParamTypes := '';
  if Context.Check(ttDelimParOpen) then
  begin
    Context.Advance;                                 // (
    while not Context.CheckAny([ttDelimParClose, ttEndOfLine, ttEndOfFile]) do
    begin
      if Context.Check(ttParamMode) then Context.Advance;  // optional BYVAL/BYREF
      // Optional parameter name before AS (FB allows both "as integer" and "x as integer").
      if Context.Check(ttIdentifier) and Assigned(Context.PeekNext) and
         (UpperCase(VarToStr(Context.PeekNext.Value)) = kAS) then
        Context.Advance;                             // skip the parameter name
      PT := '';
      if Context.Check(ttAsType) then
      begin
        Context.Advance;                             // AS
        SkipTypeQualifiers;                     // FB: "As Const <type>"
        if Context.Check(ttIdentifier) then PT := UpperCase(ParseDottedName);
        // Keep the "PTR" suffix on the parameter type (a "T PTR" param is an int address, not a T value).
        // Dropping it recorded a "Cat Ptr" parameter as "Cat", so the indirect call staged the argument
        // with UDT (by-value/handle) semantics instead of passing the pointer, corrupting the callee's arg.
        while AtPointerSuffix do
        begin PT := PT + ' PTR'; Context.Advance; end;
      end;
      if PT <> '' then
      begin
        if ParamTypes <> '' then ParamTypes := ParamTypes + ',';
        ParamTypes := ParamTypes + PT;
      end;
      if Context.Check(ttSeparParam) then Context.Advance;   // ,
    end;
    if Context.Check(ttDelimParClose) then Context.Advance;  // )
  end;
  Node.Attributes.Values['FUNCPTR'] := '1';
  Node.Attributes.Values['FPPARAMS'] := ParamTypes;
  Node.Attributes.Values['FPRET'] := '';
  // "Function(...) ByRef As R": the RETURN may be a reference, and the word stands between the parameter
  // list and AS. Unread, the '(' of the signature was parsed and then BYREF met where a variable name was
  // expected. What the pointer HOLDS is the same entry address either way, so the modifier is recorded and
  // the return type read as usual - a call through it dereferences by the callee's own protocol.
  if IsFunc and Context.Check(ttParamMode) and
     (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'BYREF') then
  begin
    Node.Attributes.Values['FPRETBYREF'] := '1';
    Context.Advance;                                 // BYREF
  end;
  if IsFunc and Context.Check(ttAsType) then
  begin
    Context.Advance;                                 // AS
    SkipTypeQualifiers;                     // FB: "As Const <type>"
    if Context.Check(ttIdentifier) then Node.Attributes.Values['FPRET'] := UpperCase(ParseDottedName);
    // Keep the "PTR" suffix on the return type too (a "T PTR" return is an int address).
    while AtPointerSuffix do
    begin Node.Attributes.Values['FPRET'] := Node.Attributes.Values['FPRET'] + ' PTR'; Context.Advance; end;
  end;
  Result := True;
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

function TPackratParser.AtEndScope: Boolean;
begin
  // END SCOPE  (END is ttProgramEnd, SCOPE is ttScopeBlock)
  Result := Context.Check(ttProgramEnd) and Assigned(Context.PeekNext) and
            (Context.PeekNext.TokenType = ttScopeBlock);
end;

function TPackratParser.ParseScopeBlock: TASTNode;
// SCOPE <newline> statements <newline> END SCOPE (FreeBASIC). Produced as an antBlock node so the SSA
// gives it the same MODERN block-scope treatment as BEGIN/BEND (DIM shadowing + destructors at exit).
var
  Token: TLexerToken;
  Stmt: TASTNode;
  PrevIdx: Integer;
begin
  Token := Context.CurrentToken;
  Context.Advance;                                   // consume SCOPE
  Result := TASTNode.Create(antBlock, Token);

  while not Context.Check(ttEndOfFile) do
  begin
    if Context.Match(ttEndOfLine) then Continue;
    if Context.Check(ttSeparStmt) then begin Context.Advance; Continue; end;
    if AtEndScope then Break;
    PrevIdx := Context.CurrentIndex;
    Stmt := ParseStatement;
    if Assigned(Stmt) then
      Result.AddChild(Stmt)
    else if Context.CurrentIndex = PrevIdx then
      Context.Advance;                               // no progress: skip a token (defensive)
  end;

  if AtEndScope then
  begin
    Context.Advance;   // END
    Context.Advance;   // SCOPE
  end;
  DoNodeCreated(Result);
end;

function TPackratParser.ParseTypeDecl: TASTNode;
begin
  Result := ParseRecordDecl(False);
end;

function TPackratParser.ParseUnionDecl: TASTNode;
begin
  Result := ParseRecordDecl(True);
end;

function TPackratParser.ParseInterfaceDecl: TASTNode;
// ⭐ MODERN ONLY - FreeBASIC has no INTERFACE at all (it reserves IMPLEMENTS and never implemented
// the other half). Declared as a divergence rather than smuggled in: a source using it does not
// compile under fbc, and that is the point of having it.
//
// It is sugar, not a new kind of entity: an interface IS a TYPE whose every method is implicitly
// ABSTRACT (no body here) and therefore VIRTUAL. That is what lets it reuse everything built today -
// the abstract instantiation check refuses a type that leaves one unimplemented, and the type-id
// dispatcher routes a call made through the interface to the implementor. It carries no fields.
begin
  // ParseRecordDecl consumes the leading keyword itself (TYPE / UNION), so INTERFACE is left in place
  // for it - consuming it here ate the interface's NAME instead.
  Result := ParseRecordDecl(False, True);
  if Assigned(Result) then Result.Attributes.Values['ISINTERFACE'] := '1';
end;

function TPackratParser.ParseRecordFieldType: string;
var
  FixedCapVal: Int64;   // folded "* n" capacity
  FixedLenExpr: TASTNode;
// Parse an in-TYPE field type after AS: a (dotted) type name, an optional "PTR" suffix (stored as an
// int handle), and an optional fixed-length "* n" (advisory in v1). Returns '' if no type token follows.
begin
  Result := '';
  if Context.Check(ttIdentifier) or
     ((Length(Context.CurrentToken.Value) > 0) and
      (UpCase(Context.CurrentToken.Value[1]) in ['A'..'Z', '_'])) then
  begin
    Result := ParseDottedName;                    // dotted: namespace-qualified field type
    // FreeBASIC pointer field "<type> PTR": stored as an int handle. Capturing the suffix keeps a
    // self-referential field (e.g. "NXT AS NODE PTR") from being treated as a nested record.
    while AtPointerSuffix do
    begin
      Result := Result + ' PTR';
      Context.Advance;                            // consume PTR
    end;
    // FreeBASIC fixed-length string field "AS STRING * n". The storage stays variable-length
    // (advisory), but a CONSTANT capacity is remembered: the field's byte layout on file depends
    // on it (fbc writes n+1 bytes, the declared characters plus the NUL terminator).
    if Context.Check(ttOpMul) then
    begin
      Context.Advance;                            // '*'
      FixedLenExpr := FExpressionParser.ParseExpression(precTerm);   { '* n': an EXPRESSION - see ParseStaticFixedLen }
      if Assigned(FixedLenExpr) then
      begin
        if TryConstIntExpr(FixedLenExpr, FixedCapVal) then FLastFieldFixedLen := FixedCapVal;
        FixedLenExpr.Free;
      end;
    end;
  end;
end;

procedure TPackratParser.ParseInTypeMethodDecl(TypeNode: TASTNode; const CurAccess: string = ''; ForceAbstract: Boolean = False);
// One "Declare [Virtual|Abstract|Static|Const] Sub|Function|Property|Operator|Constructor|Destructor
// name(...) [As ret]" line inside a TYPE body, with DECLARE already consumed. Nothing is emitted: the
// method is defined out of line. Only the two decorators that change how the DEFINITION reads are
// recorded, then the rest of the line is skipped exactly as before.
//   ABSTRACT<NAME> on the antTypeDecl -> the type declares NAME with no body of its own.
//   FTypeStaticMethods "TYPE.NAME"    -> NAME is a static member (no implicit THIS).
var
  DecoU, MethName, Key: string;
  IsAbstract, IsStatic, IsVirtual, IsOverride, IsFinal: Boolean;
  Depth, ParamIdx: Integer;
  Defs, DefExpr: TASTNode;
begin
  IsAbstract := ForceAbstract;   // MODERN: every method of an INTERFACE is abstract by construction
  IsStatic := False;
  IsVirtual := False;
  IsOverride := False;
  IsFinal := False;
  // Decorators sit between DECLARE and the SUB/FUNCTION/... keyword and arrive as plain identifiers.
  while Context.Check(ttIdentifier) do
  begin
    DecoU := UpperCase(VarToStr(Context.CurrentToken.Value));
    if DecoU = 'ABSTRACT' then IsAbstract := True
    else if DecoU = 'STATIC' then IsStatic := True
    // ⭐ VIRTUAL was read and thrown away, and that is what made every method virtual by default -
    // a SILENT divergence from FreeBASIC, where a method without it is not overridable and a
    // redeclaration in a child SHADOWS instead. Measured against fbc 1.10.1: through a base-typed
    // pointer fbc runs Root.F and we ran Child.F, on the same source.
    else if DecoU = 'VIRTUAL' then IsVirtual := True
    // OVERRIDE and FINAL are MODERN extensions - fbc has neither. Recorded here, checked in the SSA.
    else if DecoU = 'OVERRIDE' then IsOverride := True
    else if DecoU = 'FINAL' then IsFinal := True
    else if DecoU <> 'CONST' then Break;
    Context.Advance;
  end;
  MethName := '';
  if Context.Check(ttProcedureStart) then
  begin
    MethName := UpperCase(VarToStr(Context.CurrentToken.Value));
    Context.Advance;                                  // SUB / FUNCTION / PROPERTY / ...
    // CONSTRUCTOR and DESTRUCTOR ARE the method name; everything else names one next. A method name
    // may be a reserved word (LEN, TYPE, NAME...), so accept any alphabetic token — but not '(', which
    // is where a constructor's parameter list starts.
    if (MethName <> kCONSTRUCTOR) and (MethName <> kDESTRUCTOR) then
    begin
      if Context.Check(ttIdentifier) or
         ((not Context.Check(ttDelimParOpen)) and (Length(VarToStr(Context.CurrentToken.Value)) > 0) and
          (UpCase(VarToStr(Context.CurrentToken.Value)[1]) in ['A'..'Z', '_'])) then
      begin
        // ⭐ An OPERATOR keeps the word in its name. The definition side labels it "TYPE.OPERATORCAST"
        // (and "TYPE.OPERATOR[]", "TYPE.OPERATORLET", ...), so recording the bare "CAST" here filed
        // every decorator under a key nothing looks up: "Declare Virtual Operator Cast() As String"
        // stored VIRTUALCAST while MethodIsVirtual asked for VIRTUALOPERATORCAST$, answered no, and the
        // call resolved on the STATIC type. A Child overriding a virtual Cast printed the Parent's
        // answer through a Parent pointer, while a virtual Sub next to it dispatched correctly - the
        // tell that the defect was in the NAME and not in the dispatcher.
        if MethName = kOPERATOR then
          MethName := MethName + UpperCase(VarToStr(Context.CurrentToken.Value))
        else
          MethName := UpperCase(VarToStr(Context.CurrentToken.Value));
        Context.Advance;
      end
      else
        MethName := '';       // OPERATOR <symbol>: not a name we track here
    end;
  end;
  if MethName <> '' then
  begin
    if IsAbstract and Assigned(TypeNode) then
      TypeNode.Attributes.Values['ABSTRACT' + MethName] := '1';
    // ABSTRACT implies VIRTUAL: the only implementations an abstract method can ever have are the
    // overrides, so a call on it must dispatch. fbc requires the word on the base declaration only;
    // an override may repeat it or not, and either way the method stays virtual from there down.
    if (IsVirtual or IsAbstract) and Assigned(TypeNode) then
      TypeNode.Attributes.Values['VIRTUAL' + MethName] := '1';
    if IsOverride and Assigned(TypeNode) then
      TypeNode.Attributes.Values['OVERRIDE' + MethName] := '1';
    if IsFinal and Assigned(TypeNode) then
      TypeNode.Attributes.Values['FINAL' + MethName] := '1';
    if Assigned(TypeNode) and (CurAccess <> '') and (CurAccess <> 'PUBLIC') then
      TypeNode.Attributes.Values['ACCESS' + MethName] := CurAccess;
    if IsStatic and Assigned(TypeNode) then
      FTypeStaticMethods.Add(UpperCase(VarToStr(TypeNode.Value)) + '.' + MethName);
  end;
  // Walk what is left of the declaration, collecting the parameters' DEFAULT values on the way — they
  // are stated here and nowhere else, and the definition needs them. Parenthesis depth is tracked so a
  // ',' or '=' inside a nested expression is not read as a parameter boundary, and so a ':' inside a
  // default expression is not mistaken for the statement separator.
  Depth := 0;
  ParamIdx := 0;
  Defs := nil;
  while not Context.Check(ttEndOfFile) do
  begin
    if Context.Check(ttDelimParOpen) then Inc(Depth)
    else if Context.Check(ttDelimParClose) then Dec(Depth)
    else if (Depth <= 0) and (Context.CheckAny([ttEndOfLine, ttSeparStmt]) or AtEndType) then Break
    else if (Depth = 1) and Context.Check(ttSeparParam) then Inc(ParamIdx)
    else if (Depth = 1) and Context.Check(ttOpEq) and (MethName <> '') then
    begin
      Context.Advance;                              // '='
      DefExpr := FExpressionParser.ParseExpression;
      if Assigned(DefExpr) then
      begin
        if Defs = nil then Defs := TASTNode.Create(antArgumentList, Context.CurrentToken);
        while Defs.ChildCount < ParamIdx do         // parameters before this one have no default
          Defs.AddChild(NoDefaultPlaceholder(Context.CurrentToken));
        if Defs.ChildCount = ParamIdx then Defs.AddChild(DefExpr) else DefExpr.Free;
      end;
      Continue;                                     // ParseExpression already consumed the value
    end;
    Context.Advance;
  end;
  if Assigned(Defs) then
  begin
    Key := UpperCase(VarToStr(TypeNode.Value)) + '.' + MethName;
    if FTypeMethodDefaults.IndexOf(Key) >= 0 then Defs.Free   // overload: first declaration wins (v1)
    else FTypeMethodDefaults.AddObject(Key, Defs);
  end;
end;

function TPackratParser.NoDefaultPlaceholder(Tok: TLexerToken): TASTNode;
// Filler for a parameter position that carries no default, so the recorded list stays index-aligned
// with the parameter list it will be replayed onto.
begin
  Result := TASTNode.CreateWithValue(antIdentifier, '', Tok);
  Result.Attributes.Values['NODEF'] := '1';
end;

procedure TPackratParser.ClearTypeMethodDefaults;
var
  i: Integer;
begin
  for i := 0 to FTypeMethodDefaults.Count - 1 do
    if Assigned(FTypeMethodDefaults.Objects[i]) then
      TASTNode(FTypeMethodDefaults.Objects[i]).Free;
  FTypeMethodDefaults.Clear;
end;

procedure TPackratParser.ApplyDeclaredDefaults(const QualName: string; ParamList: TASTNode;
  SkipThis: Boolean);
// Replay onto a method's DEFINITION the default arguments its in-TYPE declaration gave it. FreeBASIC
// allows them only on the declaration, so without this the definition — the only thing the rest of the
// pipeline sees — reports no defaults at all: "Constructor T(k As Integer = 5)" then had no form
// callable with zero arguments, and "Dim v As T" silently skipped construction altogether.
// A default already written on the definition (which fbc rejects, but we are lenient) wins.
var
  Defs, P, D: TASTNode;
  Idx, i, First: Integer;
begin
  Idx := FTypeMethodDefaults.IndexOf(QualName);
  if Idx < 0 then Exit;
  Defs := TASTNode(FTypeMethodDefaults.Objects[Idx]);
  if (Defs = nil) or (ParamList = nil) then Exit;
  if SkipThis then First := 1 else First := 0;
  for i := 0 to Defs.ChildCount - 1 do
  begin
    if First + i >= ParamList.ChildCount then Break;
    D := Defs.GetChild(i);
    if D.Attributes.Values['NODEF'] = '1' then Continue;
    P := ParamList.GetChild(First + i);
    if P.Attributes.Values['HASDEFAULT'] = '1' then Continue;
    P.AddChild(D.Clone);                            // last child = default-value expression
    P.Attributes.Values['HASDEFAULT'] := '1';
  end;
end;

function TPackratParser.ParseRecordDecl(IsUnion: Boolean; IsInterface: Boolean = False): TASTNode;
var
  Token, NameTok, FieldTok: TLexerToken;
  FieldNode, TypeNode, ArrDimNode, FieldDefault, FpTmp, NestedEnum, NestedRec: TASTNode;
  PrevIdx, NestedUnionDepth, UnionGrpSeq, UnionGrpCur, BitWidth: Integer;
  NestedStructDepth, StructGrpCur: Integer;
  FieldTypeName, TokU, AliasType, FpParams, FpRet: string;
  AliasNode: TASTNode;   // "Type a As Integer, b As Double": the extra aliases of a comma list
  IsStaticField, LeadingType, FpIsFP: Boolean;
  CurAccess: string;   // the Public:/Private:/Protected: section currently in force
  ImplList: string;   // MODERN: the IMPLEMENTS list, recorded on the type node
begin
  CurAccess := '';
  NestedUnionDepth := 0;
  UnionGrpSeq := 0; UnionGrpCur := 0;
  NestedStructDepth := 0; StructGrpCur := 0;
  // TYPE/UNION name <newline> field AS type <newline> ... END TYPE/END UNION
  // Each field node is antIdentifier(fieldName) with one child antIdentifier(typeName).
  // An empty type name child means "infer from the field's name suffix" (SSA side).
  // A UNION is the same record shape but flagged so SSA overlaps fields of the same bank.
  Token := Context.CurrentToken;
  Context.Advance;                                  // consume TYPE / UNION
  Result := nil;
  // The type name must be a plain identifier; a reserved word (e.g. the graphics keyword
  // CIRCLE/BOX/LINE) is not a valid type name — report it cleanly instead of silently bailing
  // and leaving the stream misaligned (which can derail later parsing).
  // ...but FreeBASIC also writes the alias with the type FIRST and the NAME last:
  //   "Type As Function(ByVal As Integer) As Integer function_alias"
  // Same declaration as "Type function_alias As Function(...) As Integer", read the other way round, so
  // parse the type here and pick the name up afterwards.
  if (not IsUnion) and Context.Check(ttAsType) then
  begin
    Context.Advance;                                // consume AS
    Result := TASTNode.CreateWithValue(antTypeDecl, '', Token);
    // "Type As Const u Ptr t": the CONST qualifier belongs to the aliased type and is not part of its
    // NAME - left in place it was read as the type itself ("Undefined procedure: PTR"). Skipped here
    // as it is on a DIM, and for the same reason: this VM does not enforce const, but it must read
    // the declaration the same way fbc does.
    SkipTypeQualifiers;
    if Context.Check(ttProcedureStart) and TryParseProcPtrType(Result) then
      AliasType := 'INTEGER'
    else
    begin
      AliasType := ParseDottedName;
      while AtPointerSuffix do
      begin
        AliasType := AliasType + ' PTR';
        Context.Advance;
      end;
    end;
    if not Context.Check(ttIdentifier) then
    begin
      HandleError('Expected the alias name after the type', Context.CurrentToken);
      Result.Free; Result := nil; Exit;
    end;
    Result.Value := UpperCase(Context.CurrentToken.Value);
    Context.Advance;
    Result.Attributes.Values['ALIAS'] := UpperCase(AliasType);
    // "Type As Integer a, b": ONE type, several names. FreeBASIC's own test suite writes it, and the
    // list simply ended the declaration here - the ',' was left where a statement was expected.
    // The extra aliases ride as CHILD antTypeDecl nodes marked ALIASLIST, which CollectUDTNames
    // descends into; they are declarations in their own right, not members of anything.
    while Context.Check(ttSeparParam) do
    begin
      Context.Advance;                              // ','
      if not Context.Check(ttIdentifier) then Break;
      AliasNode := TASTNode.CreateWithValue(antTypeDecl, UpperCase(Context.CurrentToken.Value),
                                            Context.CurrentToken);
      Context.Advance;
      AliasNode.Attributes.Values['ALIAS'] := UpperCase(AliasType);
      AliasNode.Attributes.Values['ALIASLIST'] := '1';
      Result.AddChild(AliasNode);
    end;
    DoNodeCreated(Result);
    Exit;
  end;
  if not Context.Check(ttIdentifier) then
  begin
    HandleError(Format('"%s" is a reserved word and cannot be used as a type name',
                       [Context.CurrentToken.Value]), Context.CurrentToken);
    Exit;
  end;
  NameTok := Context.CurrentToken;
  Result := TASTNode.CreateWithValue(antTypeDecl, UpperCase(NameTok.Value), NameTok);
  if IsUnion then Result.Attributes.Values['UNION'] := '1';
  Context.Advance;                                  // consume type name

  // FreeBASIC type alias: "TYPE newname AS underlyingtype" — a one-line synonym with no field block
  // and no END TYPE. Distinguished from a record by an AS immediately after the type name. Recorded
  // as an ALIAS attribute on the antTypeDecl; SSA resolves it via CanonicalType (UNION cannot alias).
  if (not IsUnion) and Context.Check(ttAsType) then
  begin
    Context.Advance;                                // consume AS
    // FreeBASIC named function-pointer type: "TYPE X As Function(params) As R" / "TYPE X As Sub(params)".
    // Record the signature (FUNCPTR/FPPARAMS/FPRET) on the antTypeDecl and alias the storage to INTEGER
    // (a procedure entry PC). A var/param declared "As X" becomes an int-banked function pointer with
    // this signature (the SSA copies it into the per-proc FFuncPtrSigs so "f(args)" is an indirect call).
    if Context.Check(ttProcedureStart) and TryParseProcPtrType(Result) then
    begin
      Result.Attributes.Values['ALIAS'] := 'INTEGER';
      DoNodeCreated(Result);
      Exit;
    end;
    AliasType := '';
    SkipTypeQualifiers;                             // "Type t As Const Integer Ptr" - see the note above
    if Context.Check(ttIdentifier) or
       ((Length(Context.CurrentToken.Value) > 0) and
        (UpCase(Context.CurrentToken.Value[1]) in ['A'..'Z', '_'])) then
    begin
      AliasType := ParseDottedName;
      while AtPointerSuffix do
      begin
        AliasType := AliasType + ' PTR';
        Context.Advance;                            // consume PTR
      end;
      // "AS STRING * n" fixed-length form: consume and ignore the length (advisory, like fields).
      if Context.Check(ttOpMul) then
      begin
        Context.Advance;                            // '*'
        FExpressionParser.ParseExpression(precCall).Free;
      end;
    end;
    Result.Attributes.Values['ALIAS'] := UpperCase(AliasType);
    // "Type t As Integer, u As Double": several aliases on one line, each with its own type. Same
    // shape as the leading-AS list above and the same carrier (a child marked ALIASLIST).
    while Context.Check(ttSeparParam) do
    begin
      Context.Advance;                              // ','
      if not Context.Check(ttIdentifier) then Break;
      AliasNode := TASTNode.CreateWithValue(antTypeDecl, UpperCase(Context.CurrentToken.Value),
                                            Context.CurrentToken);
      Context.Advance;                              // the alias name
      if not Context.Check(ttAsType) then begin AliasNode.Free; Break; end;
      Context.Advance;                              // AS
      AliasType := ParseDottedName;
      while AtPointerSuffix do
      begin
        AliasType := AliasType + ' PTR';
        Context.Advance;
      end;
      AliasNode.Attributes.Values['ALIAS'] := UpperCase(AliasType);
      AliasNode.Attributes.Values['ALIASLIST'] := '1';
      Result.AddChild(AliasNode);
    end;
    DoNodeCreated(Result);
    Exit;
  end;

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

  // FreeBASIC field alignment header: "TYPE name [EXTENDS base] FIELD = n". Our record STORAGE is
  // slot-based and unaffected, but the value is recorded: the C byte layout the binary GET/PUT of a
  // whole instance writes is packed to it ("Field = 1" = no padding at all). FIELD is not a reserved
  // word; require the following '=' so a member named "field" (in the body) is unaffected.
  if Context.Check(ttIdentifier) and (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'FIELD') and
     Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttOpEq) then
  begin
    Context.Advance;                                // consume FIELD
    Context.Advance;                                // consume '='
    FieldDefault := FExpressionParser.ParseExpression(precCall);
    if Assigned(FieldDefault) then
    begin
      if FieldDefault.NodeType = antLiteral then
        Result.Attributes.Values['FIELDALIGN'] := VarToStr(FieldDefault.Value);
      FieldDefault.Free;
      FieldDefault := nil;
    end;
  end;

  // ⭐ MODERN: "TYPE name [EXTENDS base] IMPLEMENTS iface[, iface...]" - a CHECKED contract.
  //
  // FreeBASIC reserves IMPLEMENTS and does not implement it (the manual's page is a stub), so the
  // clause used to be accepted and thrown away: the type behaved as an ordinary UDT. MODERN gives it
  // a meaning, and DECLARES the divergence rather than hiding it - a type that names an interface
  // must provide every method of it, and it IS-A that interface for dispatch and for the IS operator.
  // A source written this way does not compile under fbc, and that is a stated choice, not an
  // accident. (An fbc source that uses IMPLEMENTS still works here: fbc's own semantics is "no
  // constraint", and every constraint we add is one such a source already satisfies vacuously,
  // because fbc has no interfaces to declare in the first place.)
  //
  // The names are recorded on the type node as IMPLEMENTS = 'I1,I2'; the SSA resolves them.
  if Context.Check(ttIdentifier) and (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'IMPLEMENTS') then
  begin
    Context.Advance;                                // consume IMPLEMENTS
    ImplList := '';
    repeat
      // consume one (possibly dotted) interface name
      if Context.Check(ttIdentifier) or
         ((Length(VarToStr(Context.CurrentToken.Value)) > 0) and
          (UpCase(VarToStr(Context.CurrentToken.Value)[1]) in ['A'..'Z', '_'])) then
      begin
        if ImplList <> '' then ImplList := ImplList + ',';
        ImplList := ImplList + UpperCase(VarToStr(Context.CurrentToken.Value));
        Context.Advance;
      end
      else
        Break;
      while Context.Check(ttOpDot) and Assigned(Context.PeekNext) do
      begin
        Context.Advance;                            // '.'
        Context.Advance;                            // dotted segment
      end;
      if Context.Check(ttSeparParam) then Context.Advance   // ',' -> another interface
      else Break;
    until False;
    if ImplList <> '' then Result.Attributes.Values['IMPLEMENTS'] := ImplList;
  end;

  while not Context.Check(ttEndOfFile) do
  begin
    if Context.Match(ttEndOfLine) then Continue;
    if Context.Check(ttSeparStmt) then begin Context.Advance; Continue; end;
    // FreeBASIC anonymous nested UNION inside a TYPE: "union ... end union". v1 FLATTENS its members as
    // ordinary (non-overlapping) fields of the parent — enough for code that reads back whichever member it
    // last wrote (true storage overlap / type-punning is not modelled). Track nesting so a nested "end
    // union" closes the union (not the whole type); the members parse as normal fields in between.
    // A NESTED "Union ... End Union" inside a TYPE. Its tokens used to be skipped and nothing else:
    // the members came out FLAT, so they were laid out one after another instead of overlapping -
    // OffsetOf(MyType, i) answered 16 where fbc says 8, SizeOf 24 where fbc says 16, and writing one
    // member did not change the other. Each block now gets an id that every field inside carries.
    if Context.Check(ttUnionDecl) then
    begin
      // ⛔ A NAMED nested block declares a TYPE OF ITS OWN ("Union U ... End Union" then "m As U"),
      // and this model flattens the members into the enclosing type instead. Accepting the name
      // silently made "U" a FIELD and the program then computed wrong values rather than failing:
      // udt/union4 printed 1 1 1 where FreeBASIC prints 1 2 513. A wrong answer in silence is worse
      // than a refusal, so it is refused until nested types are real.
      // ⭐ A NAMED nested block DECLARES A TYPE OF ITS OWN ("Union U ... End Union", then "m As U"),
      // and it is parsed as exactly that: ParseRecordDecl consumes the keyword itself, so it can be
      // called right here and hands back a complete antTypeDecl. It is hung on the enclosing type the
      // way a nested ENUM already is - the SSA registers it as a type and NOT as a field of the parent.
      // ⛔ Until this existed the name was refused, because accepting it and FLATTENING the members
      // silently computed wrong values: udt/union4 printed 1 1 1 where FreeBASIC prints 1 2 513.
      if Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttIdentifier) then
      begin
        NestedRec := ParseRecordDecl(True);
        if Assigned(NestedRec) then
        begin
          NestedRec.Attributes.Values['NESTEDTYPE'] := '1';
          NestedRec.Attributes.Values['OUTERTYPE'] := UpperCase(VarToStr(Result.Value));
          Result.AddChild(NestedRec);
        end;
        Continue;
      end;
      Inc(NestedUnionDepth);
      if NestedUnionDepth = 1 then begin Inc(UnionGrpSeq); UnionGrpCur := UnionGrpSeq; end;
      Context.Advance; Continue;
    end;
    if (NestedUnionDepth > 0) and Context.Check(ttProgramEnd) and Assigned(Context.PeekNext) and
       (Context.PeekNext.TokenType = ttUnionDecl) then
    begin
      Dec(NestedUnionDepth);
      if NestedUnionDepth = 0 then UnionGrpCur := 0;
      Context.Advance; Context.Advance;               // consume END UNION
      Continue;
    end;
    // ...AND ITS MIRROR: an ANONYMOUS "Type ... End Type" block. Inside a UNION it is what makes the
    // members SEQUENTIAL while the union overlaps the groups - "Union: ul As ULong / Type: ub0..ub3"
    // is the whole point of udt/union.bas. The parser used to see the bare TYPE, demand a type name
    // and fail outright ("is a reserved word and cannot be used as a type name"), so three manual
    // examples never even parsed. Only the ANONYMOUS form is a block: "Type Name" is still a nested
    // type declaration and is left to whoever handled it before.
    if Context.Check(ttTypeDecl) and Assigned(Context.PeekNext) and
       (Context.PeekNext.TokenType = ttEndOfLine) then
    begin
      Inc(NestedStructDepth);
      if NestedStructDepth = 1 then begin Inc(UnionGrpSeq); StructGrpCur := UnionGrpSeq; end;
      Context.Advance; Continue;
    end;
    // ...and the NAMED nested TYPE, the mirror of the named nested UNION above: "Type Child ... End
    // Type" inside a Type declares Child, it does not add fields to the parent. (Only when a NAME
    // follows - the anonymous form, handled just above, is a layout block.)
    if Context.Check(ttTypeDecl) and (NestedStructDepth = 0) and (NestedUnionDepth = 0) and
       Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttIdentifier) then
    begin
      NestedRec := ParseRecordDecl(False);
      if Assigned(NestedRec) then
      begin
        NestedRec.Attributes.Values['NESTEDTYPE'] := '1';
        NestedRec.Attributes.Values['OUTERTYPE'] := UpperCase(VarToStr(Result.Value));
        Result.AddChild(NestedRec);
      end;
      Continue;
    end;
    if (NestedStructDepth > 0) and Context.Check(ttProgramEnd) and Assigned(Context.PeekNext) and
       (Context.PeekNext.TokenType = ttTypeDecl) then
    begin
      Dec(NestedStructDepth);
      if NestedStructDepth = 0 then StructGrpCur := 0;
      Context.Advance; Context.Advance;               // consume END TYPE
      Continue;
    end;
    if (NestedUnionDepth = 0) and AtEndType then Break;
    // FreeBASIC nested ENUM inside a TYPE: "Type T : Enum e : a : b : End Enum : ... : End Type". Its
    // members are ordinary module-wide constants (reachable bare, or as "T.e.member"), not fields — so
    // parse it as the statement it is and hang it on the type node. Left to the field grammar below, the
    // ENUM keyword was read as a FIELD NAME and every member came out worth ZERO, which made a
    // "Select Case" over them pick the first arm every time.
    if Context.Check(ttEnum) then
    begin
      NestedEnum := ParseEnumStatement;
      if Assigned(NestedEnum) then Result.AddChild(NestedEnum);
      Continue;
    end;
    PrevIdx := Context.CurrentIndex;
    TokU := UpperCase(VarToStr(Context.CurrentToken.Value));
    // FreeBASIC access specifiers inside a TYPE: "Public:" / "Private:" / "Protected:".
    // ⭐ They used to be recognised and SKIPPED - "access is not enforced (v1)" - which meant a field
    // written under Private: could be read from anywhere. fbc rejects that with "error 202: Illegal
    // member access"; we printed the value. The label now sets the level for everything that follows
    // it in the body, and each member is stamped with it (ACCESS<NAME> on the type node).
    if ((TokU = 'PUBLIC') or (TokU = 'PRIVATE') or (TokU = 'PROTECTED')) and
       Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttSeparStmt) then
    begin
      CurAccess := TokU;
      Context.Advance;                              // specifier
      Context.Advance;                              // ':'
      Continue;
    end;
    // FreeBASIC in-TYPE method declaration: "Declare [Virtual|Abstract|Static] Sub|Function ...". Methods
    // are defined out-of-line (SUB Type.method), so the declaration itself emits nothing — but two of its
    // decorators change how the DEFINITION must be read, and skipping the line wholesale threw them away:
    //   ABSTRACT — the method has NO body on this type. Nothing declares it anywhere the SSA can see, so
    //     a call on a base-typed handle resolved to no label at all and was dropped in silence (the PRINT
    //     around it then showed a stale register). Recorded on the antTypeDecl so virtual dispatch can be
    //     built from the OVERRIDES alone.
    //   STATIC — the method has no implicit THIS; its definition must not be given one.
    // Everything else on the line (parameters, calling convention, OVERRIDE, ALIAS) is still skipped.
    if TokU = 'DECLARE' then
    begin
      Context.Advance;                              // consume DECLARE
      ParseInTypeMethodDecl(Result, CurAccess, IsInterface);
      Continue;
    end;
    // FreeBASIC lets CONSTRUCTOR / DESTRUCTOR / OPERATOR / PROPERTY be introduced in the TYPE body
    // WITHOUT a leading DECLARE (e.g. "Destructor()", "Constructor(x As Integer)", "Operator cast() As
    // String"). Like DECLARE'd methods they are defined out-of-line (SUB Type.method), so skip the whole
    // declaration line here — otherwise the field grammar below would take the keyword as a field name
    // and choke on its "()" parameter list (which ParseDimensionList reads as empty array dimensions).
    if (TokU = kCONSTRUCTOR) or (TokU = kDESTRUCTOR) or (TokU = kOPERATOR) or (TokU = kPROPERTY) then
    begin
      while (not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile])) and (not AtEndType) do
        Context.Advance;
      Continue;
    end;
    // FreeBASIC static member variable: "Static field AS type" — one storage shared by all instances.
    // Consume the STATIC prefix and mark the field; the SSA backs it with a module-global, not a slot.
    // ...in BOTH spellings: "Static countID As Integer" (name first) and "Static As Integer countID"
    // (type first, the form the manual's own example uses). Only the first was recognised, so the second
    // declared a field literally named STATIC and left countID typeless.
    IsStaticField := False;
    if (TokU = 'STATIC') and Assigned(Context.PeekNext) and
       ((Context.PeekNext.TokenType = ttIdentifier) or (Context.PeekNext.TokenType = ttAsType) or
        ((Length(VarToStr(Context.PeekNext.Value)) > 0) and
         (UpCase(VarToStr(Context.PeekNext.Value)[1]) in ['A'..'Z', '_']))) then
    begin
      Context.Advance;                              // consume STATIC
      IsStaticField := True;
      TokU := UpperCase(VarToStr(Context.CurrentToken.Value));   // re-read: a DIM may follow STATIC
    end;
    // FreeBASIC allows an in-TYPE field to be introduced with a leading DIM ("Dim As Double m(Any,Any)").
    // Consume it — the field grammar below handles both "As type name(dims)" and "name(dims) As type".
    if TokU = kDIM then Context.Advance;
    FieldTypeName := '';                            // empty => infer by suffix
    FLastFieldFixedLen := 0;                        // "As String * n" capacity of THIS field (0 = none)
    LeadingType := False;
    FpIsFP := False; FpParams := ''; FpRet := '';   // funcptr field ("fn As Function(...) As R")
    ArrDimNode := nil;
    // "As-first" form: "As <type> name(dims)" (the common FB form). Read the type before the name.
    if Context.Check(ttAsType) then
    begin
      Context.Advance;                              // AS
      SkipTypeQualifiers;                     // FB: "As Const <type>"
      FieldTypeName := ParseRecordFieldType;
      LeadingType := True;
    end;
    // A field name may be an identifier or a reserved word (e.g. LEN, TYPE, NAME): accept any
    // alphabetic token as the field name here.
    if Context.Check(ttIdentifier) or
       ((Length(Context.CurrentToken.Value) > 0) and
        (UpCase(Context.CurrentToken.Value[1]) in ['A'..'Z', '_'])) then
    begin
      FieldTok := Context.CurrentToken;
      Context.Advance;                              // field name
      // FreeBASIC BIT FIELD: "name : <bits> As <type>". The ':' is the statement separator token
      // everywhere else, which is why this has to be recognised HERE, right after the member's name and
      // before anything treats it as the end of a statement - a member declared that way otherwise fell
      // apart into two fragments and the type came out with the wrong members entirely.
      BitWidth := 0;
      if Context.Check(ttSeparStmt) and Assigned(Context.PeekNext) and
         (Context.PeekNext.TokenType in [ttNumber, ttInteger]) then
      begin
        Context.Advance;                            // ':'
        BitWidth := StrToIntDef(Context.CurrentToken.Value, 0);
        Context.Advance;                            // the bit count
      end;
      // Array member "name(dims)" — the dimension list may appear before the AS (name-first) or after
      // the name (As-first). Only the dimension COUNT is kept; REDIM (or the declared bounds) sizes it.
      if Context.Check(ttDelimParOpen) then
      begin
        Context.Advance;                            // '('
        ArrDimNode := ParseDimensionList;
        if Context.Check(ttDelimParClose) then Context.Advance;   // ')'
      end;
      // "name(dims) As type" — trailing type when it was not given up front.
      if (not LeadingType) and Context.Check(ttAsType) then
      begin
        Context.Advance;                            // AS
        SkipTypeQualifiers;                     // FB: "As Const <type>"
        // FreeBASIC funcptr field "fn As Function(params) As R" / "As Sub(params)": record the signature
        // (int-banked entry PC) instead of a type name; "obj.fn(args)" is lowered as an indirect call.
        if Context.Check(ttProcedureStart) then
        begin
          FpTmp := TASTNode.CreateWithValue(antIdentifier, '', FieldTok);
          if TryParseProcPtrType(FpTmp) then
          begin
            FpIsFP := True;
            FpParams := FpTmp.Attributes.Values['FPPARAMS'];
            FpRet := FpTmp.Attributes.Values['FPRET'];
            FieldTypeName := 'INTEGER';             // the field slot holds the procedure entry PC
          end;
          FpTmp.Free;
        end
        else
          FieldTypeName := ParseRecordFieldType;
        if (ArrDimNode = nil) and Context.Check(ttDelimParOpen) then
        begin
          Context.Advance;
          ArrDimNode := ParseDimensionList;
          if Context.Check(ttDelimParClose) then Context.Advance;
        end;
      end;
      FieldNode := TASTNode.CreateWithValue(antIdentifier, UpperCase(FieldTok.Value), FieldTok);
      TypeNode := TASTNode.CreateWithValue(antIdentifier, FieldTypeName, FieldTok);
      FieldNode.AddChild(TypeNode);
      if FpIsFP then
      begin
        FieldNode.Attributes.Values['FUNCPTR'] := '1';
        FieldNode.Attributes.Values['FPPARAMS'] := FpParams;
        FieldNode.Attributes.Values['FPRET'] := FpRet;
      end;
      if IsStaticField then FieldNode.Attributes.Values['STATIC'] := '1';
      if BitWidth > 0 then FieldNode.Attributes.Values['BITWIDTH'] := IntToStr(BitWidth);
      // "As String * n": the declared capacity. Storage stays variable-length (advisory), but the
      // BINARY layout needs it — fbc gives such a field n+1 bytes on file (the NUL terminator).
      if FLastFieldFixedLen > 0 then
        FieldNode.Attributes.Values['FIXEDLEN'] := IntToStr(FLastFieldFixedLen);
      if Assigned(ArrDimNode) then
      begin
        FieldNode.Attributes.Values['ARRAYFIELD'] := '1';
        FieldNode.Attributes.Values['ARRAYDIMS'] := IntToStr(ArrDimNode.ChildCount);
        // Keep the dimension list (the SSA auto-sizes a fixed-bound member at construction; an "Any"
        // member has no concrete bound and is left for an explicit REDIM).
        FieldNode.AddChild(ArrDimNode);
      end
      // FreeBASIC field default value: "field AS T = expr". Attach the expression as the last child and
      // mark HASDEFAULT so the SSA applies it on every instantiation (array members take no default).
      else if Context.Check(ttOpEq) then
      begin
        Context.Advance;                            // '='
        // ⛔ ...and the AGGREGATE TUPLE, "d As A = (4, 5, 6)", which sets the member UDT's fields in
        // declaration order. DIM and STATIC both read it through TryParseAggregateTuple; a FIELD did not,
        // so the parentheses were parsed as an expression and the declaration failed on the first comma.
        // The third caller of the one grammar, for the same reason the second exists.
        FieldDefault := TryParseAggregateTuple(FieldTypeName);
        if not Assigned(FieldDefault) then FieldDefault := FExpressionParser.ParseExpression;
        if Assigned(FieldDefault) then
        begin
          FieldNode.AddChild(FieldDefault);
          FieldNode.Attributes.Values['HASDEFAULT'] := '1';
        end;
      end;
      if UnionGrpCur > 0 then FieldNode.Attributes.Values['UNIONGRP'] := IntToStr(UnionGrpCur);
      if StructGrpCur > 0 then FieldNode.Attributes.Values['STRUCTGRP'] := IntToStr(StructGrpCur);
      Result.AddChild(FieldNode);
      if (CurAccess <> '') and (CurAccess <> 'PUBLIC') then
        Result.Attributes.Values['ACCESS' + UpperCase(VarToStr(FieldNode.Value))] := CurAccess;
      // FreeBASIC "As <type> a, b, c": the leading-AS type is shared by every comma-separated name
      // (e.g. "As String name, value" -> both String). Only the As-first form shares this way; a
      // name-first field carries its own trailing "As type", so its comma is handled by re-parsing.
      if LeadingType then
        while Context.Check(ttSeparParam) do
        begin
          Context.Advance;                          // ','
          if not (Context.Check(ttIdentifier) or
                  ((Length(Context.CurrentToken.Value) > 0) and
                   (UpCase(Context.CurrentToken.Value[1]) in ['A'..'Z', '_']))) then Break;
          FieldTok := Context.CurrentToken;
          Context.Advance;                          // additional field name
          ArrDimNode := nil;
          if Context.Check(ttDelimParOpen) then     // "As type a, b(dims)": array member in the list
          begin
            Context.Advance;
            ArrDimNode := ParseDimensionList;
            if Context.Check(ttDelimParClose) then Context.Advance;
          end;
          FieldNode := TASTNode.CreateWithValue(antIdentifier, UpperCase(FieldTok.Value), FieldTok);
          FieldNode.AddChild(TASTNode.CreateWithValue(antIdentifier, FieldTypeName, FieldTok));
          if IsStaticField then FieldNode.Attributes.Values['STATIC'] := '1';
          if Assigned(ArrDimNode) then
          begin
            FieldNode.Attributes.Values['ARRAYFIELD'] := '1';
            FieldNode.Attributes.Values['ARRAYDIMS'] := IntToStr(ArrDimNode.ChildCount);
            FieldNode.AddChild(ArrDimNode);         // keep dims for construction-time auto-sizing
          end
          else if Context.Check(ttOpEq) then        // "As T a, b = expr": per-name default value
          begin
            Context.Advance;                        // '='
            FieldDefault := FExpressionParser.ParseExpression;
            if Assigned(FieldDefault) then
            begin
              FieldNode.AddChild(FieldDefault);
              FieldNode.Attributes.Values['HASDEFAULT'] := '1';
            end;
          end;
          if UnionGrpCur > 0 then FieldNode.Attributes.Values['UNIONGRP'] := IntToStr(UnionGrpCur);
      if StructGrpCur > 0 then FieldNode.Attributes.Values['STRUCTGRP'] := IntToStr(StructGrpCur);
          Result.AddChild(FieldNode);
        end;
    end
    else
    begin
      if Assigned(ArrDimNode) then ArrDimNode.Free;
      Context.Advance;                              // skip unexpected token (defensive)
    end;
    if Context.CurrentIndex = PrevIdx then Break;   // no progress guard
  end;
  ConsumeEndType;
  DoNodeCreated(Result);
end;

function TPackratParser.ParseRandomizeStatement: TASTNode;
// RANDOMIZE [seed] : seed the RNG. The optional seed expression is child0; with no seed the
// generator is seeded from the system timer. A trailing ", algorithm" argument (FreeBASIC) is
// accepted and ignored (we have a single RNG).
var
  Token: TLexerToken;
  SeedExpr: TASTNode;
begin
  Token := Context.CurrentToken;
  Context.Advance;                                  // consume RANDOMIZE
  Result := TASTNode.CreateWithValue(antRandomize, kRANDOMIZE, Token);
  // A seed expression may follow on the same statement; stop at end-of-line/statement separator.
  if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) then
  begin
    SeedExpr := FExpressionParser.ParseExpression;
    if Assigned(SeedExpr) then Result.AddChild(SeedExpr);
    // Optional ", algorithm" — parse and discard (single RNG, no algorithm selection).
    if Context.Check(ttSeparParam) then
    begin
      Context.Advance;                              // ','
      if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) then
        FExpressionParser.ParseExpression.Free;     // algorithm operand (discarded)
    end;
  end;
  DoNodeCreated(Result);
end;

// Build the condition for a CASE clause: "(sel = v1) OR (sel = v2) OR ...", where
// each 'sel' is a fresh clone of the SELECT selector (so it isn't shared in the AST).
function TPackratParser.ParseCaseCondition(Selector: TASTNode): TASTNode;
var
  ValueExpr, HighExpr, Cmp, GeNode, LeNode: TASTNode;
  RelopType: TTokenType;
  RelopSym: string;
begin
  Result := nil;
  repeat
    // FreeBASIC/QB "CASE IS <relop> value" (e.g. CASE IS < x): matches when "selector <relop> value".
    // 'IS' here is the comparison form of CASE (distinct from the RTTI "obj IS Type" operator).
    if Context.Check(ttOpIs) then
    begin
      Context.Advance;                                 // consume IS
      RelopType := ttOpEq; RelopSym := '=';            // bare "CASE IS value" defaults to equality
      case Context.CurrentToken.TokenType of
        ttOpLt:  begin RelopType := ttOpLt;  RelopSym := '<';  Context.Advance; end;
        ttOpGt:  begin RelopType := ttOpGt;  RelopSym := '>';  Context.Advance; end;
        ttOpLe:  begin RelopType := ttOpLe;  RelopSym := '<='; Context.Advance; end;
        ttOpGe:  begin RelopType := ttOpGe;  RelopSym := '>='; Context.Advance; end;
        ttOpNeq: begin RelopType := ttOpNeq; RelopSym := '<>'; Context.Advance; end;
        ttOpEq:  begin RelopType := ttOpEq;  RelopSym := '=';  Context.Advance; end;
      end;
      ValueExpr := FExpressionParser.ParseExpression;
      if not Assigned(ValueExpr) then Break;
      Cmp := CreateBinaryOpNode(RelopType, Selector.Clone, ValueExpr,
                                TLexerToken.CreateSimple(RelopType, RelopSym));
      if Result = nil then Result := Cmp
      else Result := CreateBinaryOpNode(ttBitwiseOR, Result, Cmp,
                                        TLexerToken.CreateSimple(ttBitwiseOR, 'OR'));
      if Context.Check(ttSeparParam) then begin Context.Advance; Continue; end
      else Break;
    end;
    ValueExpr := FExpressionParser.ParseExpression;
    if not Assigned(ValueExpr) then Break;
    // NB: SSA lowering reads the operator from Node.Token.TokenType, so the binary
    // op needs a real token of that type (not nil).
    // FreeBASIC "CASE lo TO hi" range: (sel >= lo) AND (sel <= hi). Comparison results are -1/0, so a
    // bitwise AND combines them correctly (as the OR chain below does for a value list). Without this,
    // the value parses as "CASE lo" and the leftover "TO hi" derails the case body.
    if Context.Check(ttLoopControl) and (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'TO') then
    begin
      Context.Advance;                                 // consume TO
      HighExpr := FExpressionParser.ParseExpression;   // hi
      GeNode := CreateBinaryOpNode(ttOpGe, Selector.Clone, ValueExpr,
                                   TLexerToken.CreateSimple(ttOpGe, '>='));   // sel >= lo
      LeNode := CreateBinaryOpNode(ttOpLe, Selector.Clone, HighExpr,
                                   TLexerToken.CreateSimple(ttOpLe, '<='));   // sel <= hi
      Cmp := CreateBinaryOpNode(ttBitwiseAND, GeNode, LeNode,
                                TLexerToken.CreateSimple(ttBitwiseAND, 'AND'));
    end
    else
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
  // FreeBASIC "SELECT CASE AS CONST <sel>": a jump-table optimisation hint (the case values must be
  // constants). Semantically identical to a plain SELECT CASE here, so consume and ignore "AS CONST".
  if Context.Check(ttAsType) then
  begin
    Context.Advance;                                // AS
    SkipTypeQualifiers;                     // FB: "As Const <type>"
    if UpperCase(VarToStr(Context.CurrentToken.Value)) = 'CONST' then Context.Advance;   // CONST
  end;
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
  ForVarType: string;
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

  // FreeBASIC typed loop variable: "FOR i AS <type> = start TO end". The counter is a block-local of the
  // given type; consume the "AS <type>" clause (PTR-aware) and record it as an advisory attribute. The
  // loop otherwise runs on the type inferred from the bounds (integer counters, the common case).
  if Context.Check(ttAsType) then
  begin
    Context.Advance;                              // AS
    SkipTypeQualifiers;                     // FB: "As Const <type>"
    if Context.Check(ttIdentifier) then
    begin
      ForVarType := ParseDottedName;
      while AtPointerSuffix do
      begin
        ForVarType := ForVarType + ' PTR';
        Context.Advance;
      end;
      Result.Attributes.Values['VARTYPE'] := ForVarType;
    end;
  end;

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

  // Parse body until LOOP (nesting-aware: a nested flat FOR...NEXT is consumed, not mistaken
  // for the terminator). NOTE: the body is NOT bounded by EndIndex - that scan counts the
  // kind words of "Exit Do, Do" as loop openers, so its index is only reliable as a
  // "no LOOP anywhere" check. If a single-line IF branch consumes the closing LOOP (CLASSIC
  // "DO : ... : IF c THEN ... : LOOP"), the body absorbs the rest of the program and the
  // loop is marked open-ended below - flow stays linear because the DO emits no back-jump.
  Inc(FDoParseDepth);
  Body := ParseLoopBody;
  Dec(FDoParseDepth);
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
  end
  else
    // The matching LOOP was consumed INSIDE the body — i.e. in a single-line IF branch, where
    // it lowered to CONTINUE DO (the loop's only back-edge, exactly a C128's LOOP statement).
    // The DO gets NO automatic back-jump: when the branch does not take, execution falls
    // through past the end of the body, like a C128 whose LOOP statement never executes.
    Result.Attributes.Values['OpenEnded'] := '1';

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

  // CBM BASIC v7 accepts the two-word form "GO TO": the "GO" keyword (kGO_TO)
  // is followed by a separate "TO" token which must be consumed here before the
  // line-number target, otherwise ParseExpression chokes on "TO".
  if (UpperCase(Token.Value) = kGO_TO) and Assigned(Context.CurrentToken) and
     (UpperCase(Context.CurrentToken.Value) = kTO) then
    Context.Advance; // Consume TO

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

function TPackratParser.ParseFunctionResultAssign: TASTNode;
// FreeBASIC "FUNCTION = expr": set the current FUNCTION's result and CARRY ON (unlike RETURN, which
// also exits). Lowered as an ordinary assignment whose target is the reserved word FUNCTION -- the SSA
// already routes "fname = expr" to the result slot and treats this name the same way. The parser has no
// enclosing-procedure context of its own, so resolving which function this belongs to is left to the SSA.
var
  Token: TLexerToken;
  ExprNode, NameNode: TASTNode;
begin
  Token := Context.CurrentToken;
  Context.Advance;                 // consume FUNCTION
  Context.Advance;                 // consume '='

  ExprNode := ParseExpression;
  if not Assigned(ExprNode) then
  begin
    HandleError('Expected an expression after "FUNCTION ="', Context.CurrentToken);
    Result := nil;
    Exit;
  end;

  NameNode := TASTNode.CreateWithValue(antIdentifier, kFUNCTION, Token);
  Result := TASTNode.Create(antAssignment, Token);
  Result.AddChild(NameNode);
  Result.AddChild(ExprNode);
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
  ExitArg: TASTNode;
  ExitCodeVal: Int64;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antEnd, Token);
  Context.Advance; // Consume END / SYSTEM
  // "END EXTERN" closes a linkage block whose body is parsed where it stands (see the EXTERN handler):
  // there is nothing to end, so consume the word and emit nothing. Without this the bare END halted the
  // program at the closing line of the block.
  if Context.Check(ttIdentifier) and (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'EXTERN') then
  begin
    Context.Advance;
    Result.Free;
    Result := nil;
    Exit;
  end;
  // FreeBASIC END and SYSTEM both carry an optional exit code ("End 1", "System 0"): the value the
  // PROCESS answers with. It used to be parsed and DISCARDED - the note here said "we have no process
  // exit-code channel", and that was true of the whole program: sb answered 0 whatever happened. There
  // is one now (TBytecodeVM.ProgramExitCode), so the value is kept.
  // SYSTEM (FB-only) accepts any expression. For END, only consume a NUMERIC argument, and only in MODERN:
  // this cannot mis-eat a block-ender's keyword ("End Sub") should one ever reach here, and CLASSIC v7 END
  // is always standalone.
  // ⚠️ Only a CONSTANT is honoured, and that is declared: the code rides in the opcode's IMMEDIATE, so
  // a computed one has nowhere to go without a register operand on an opcode that has none. "End n"
  // with a variable halts exactly as before and answers 0.
  ExitArg := nil;
  if (UpperCase(Token.Value) = kSYSTEM) and
     (not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse])) then
    ExitArg := ParseExpression
  else if FModernMode and Context.CheckAny([ttNumber, ttInteger, ttFloat, ttOpSub, ttDelimParOpen]) then
    ExitArg := ParseExpression;
  if Assigned(ExitArg) then
  begin
    if TryConstIntExpr(ExitArg, ExitCodeVal) then
      Result.Attributes.Values['EXITCODE'] := IntToStr(ExitCodeVal and 255);
    ExitArg.Free;
  end;
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
      Context.Advance;
      HandleNode := FoldFileHandlePostfix(HandleNode);
      Result.AddChild(HandleNode);
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

  // Parse optional parameter (milliseconds to sleep)
  if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
  begin
    Param := ParseExpression;
    if Assigned(Param) then
      Result.AddChild(Param);
    // FreeBASIC "Sleep milliseconds, wakeup": the optional second argument (1 = do not wake on a
    // keypress) is consumed and ignored — the headless VM has no interactive wake anyway. Without this it
    // was left as a stray ", literal" statement (a benign but noisy "Unhandled node type" warning).
    if Context.Match(ttSeparParam) then
      ParseExpression.Free;   // wakeup flag: discard
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseSetClockStatement: TASTNode;
// SETDATE str / SETTIME str: set the VM-internal current date/time. The node Value keeps the keyword
// ("SETDATE"/"SETTIME") so SSA can pick the selector; the single string expression is child 0.
var
  Token: TLexerToken;
  Param: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.CreateWithValue(antSetClock, UpperCase(Token.Value), Token);
  Context.Advance; // Consume SETDATE/SETTIME
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
// Commodore WAIT addr, mask [, xor]: pause until (PEEK(addr) XOR xor) AND mask <> 0.
// ⛔⛔ WAIT IS NOT IMPLEMENTED: the arguments are parsed and DISCARDED and an empty statement is
// emitted. It waits for nothing and returns at once. It is built on INP, which answers a constant, so
// it could not do more than INP does even if the wait loop were written.
// 🟡 OPEN DECISION (23 Aug 2026): implement the family somehow, or withdraw the keywords so a program
// cannot silently use something inert. BASIC.md marks INP/OUT/WAIT ✗ and carries the argument.
var
  Token: TLexerToken;
  Args: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);   // empty -> no code emitted
  Context.Advance; // Consume WAIT
  if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
  begin
    Args := ParseExpressionList(ttSeparParam);       // addr, mask [, xor] — consumed and discarded
    if Assigned(Args) then Args.Free;
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
  NextCount: Integer;   // "Next j, i": how many loops this one closes
  Token: TLexerToken;
  EndKeyword: string;
begin
  Token := Context.CurrentToken;
  EndKeyword := UpperCase(Token.Value);

  // CLASSIC branch-LOOP: ParseDoStatement consumes its structural closer directly, so a LOOP
  // that arrives HERE while a DO body parse is on the stack sits inside a nested construct —
  // in practice a single-line IF branch ("DO : ... : IF c THEN ... : LOOP"). On a C128 the
  // LOOP statement is simply "jump back to the matching DO", so lower it as CONTINUE DO
  // (whose target for an unconditioned DO is the body top, and for DO WHILE/UNTIL the
  // condition re-test). A conditioned LOOP UNTIL/WHILE in branch position is rejected loudly.
  if (EndKeyword = 'LOOP') and (FDoParseDepth > 0) and (not FModernMode) then
  begin
    Context.Advance; // Consume LOOP
    if Context.Check(ttLoopControl) then
    begin
      HandleError('LOOP UNTIL/WHILE inside a conditional branch is not supported', Token);
      Result := nil;
      Exit;
    end;
    Result := TASTNode.Create(antReturn, Token);
    Result.Value := kCONTINUE + ' ' + kDO;
    DoNodeCreated(Result);
    Exit;
  end;

  // CLASSIC runtime rejection, like a real C128 (which has no compile phase at all): a LOOP
  // with no DO anywhere raises ?LOOP WITHOUT DO and an orphan WEND raises ?SYNTAX - both at
  // RUNTIME, if and when the statement executes, and both trappable. The statement lowers to
  // the error raise itself (antErrorStmt with a literal code). MODERN keeps the strict
  // compile-time rejection below, like fbc.
  if (not FModernMode) and
     (((EndKeyword = 'LOOP') and (FDoParseDepth = 0)) or
      ((EndKeyword = kWEND) and (not FValidationStacks.HasActiveLoop))) then
  begin
    Context.Advance; // Consume LOOP/WEND
    Result := TASTNode.Create(antErrorStmt, Token);
    if EndKeyword = 'LOOP' then
      Result.AddChild(TASTNode.CreateWithValue(antLiteral, ERR_LOOP_WITHOUT_DO, Token))
    else
      Result.AddChild(TASTNode.CreateWithValue(antLiteral, ERR_SYNTAX, Token));
    // A trailing UNTIL/WHILE condition on an orphan LOOP is consumed and discarded (the raise
    // aborts the statement before any condition could matter, exactly like the C128).
    if Context.Check(ttLoopControl) then
    begin
      Context.Advance;
      ParseExpression.Free;
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  // CLASSIC: an unmatched NEXT is NOT a compile error on a C128 - it raises ?NEXT WITHOUT FOR
  // at RUNTIME, if and when executed (and it is trappable). Emit the orphan antNext node; the
  // SSA lowers it to the error raise. MODERN keeps the strict compile-time rejection (fbc does).
  if (EndKeyword = 'NEXT') and (not FModernMode) and (not FValidationStacks.HasActiveLoop) then
  begin
    Result := TASTNode.Create(antNext, Token);
    Context.Advance; // Consume NEXT
    if Context.Check(ttIdentifier) then
      Context.Advance; // Consume variable name
    DoNodeCreated(Result);
    Exit;
  end;

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
  begin
    Context.Advance; // Consume variable name
    // "Next j, i" closes SEVERAL loops at once, innermost first - one NEXT written for each. Only the
    // first was consumed, so the comma and the outer counter were left to be parsed as a statement, and
    // the outer FOR never found its NEXT at all. The extra closings ride on the node; the SSA repeats
    // the loop-closing for each, and each one validates its own loop end here.
    NextCount := 1;
    while Context.Check(ttSeparParam) do
    begin
      Context.Advance;                            // ','
      if not Context.Check(ttIdentifier) then Break;
      Context.Advance;                            // the outer counter's name
      if not FValidationStacks.ValidateLoopEnd(EndKeyword) then Break;
      Inc(NextCount);
    end;
    if NextCount > 1 then Result.Attributes.Values['NEXTCOUNT'] := IntToStr(NextCount);
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseLoopControlStatement: TASTNode;
var
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;

  // A WHILE at statement start opens a WHILE...WEND loop (the DO WHILE / LOOP WHILE
  // modifier forms are consumed inside ParseDoStatement and never reach here).
  if UpperCase(Token.Value) = kWHILE then
  begin
    Result := ParseWhileStatement;
    Exit;
  end;

  // A bare TO/STEP/UNTIL at statement start is a stray loop-control keyword. CLASSIC lowers
  // it to a runtime ?SYNTAX ERROR raise - a C128 accepts the line at entry and errors when it
  // executes (trappable). MODERN keeps the old no-op tolerance.
  if not FModernMode then
  begin
    Result := TASTNode.Create(antErrorStmt, Token);
    Result.AddChild(TASTNode.CreateWithValue(antLiteral, ERR_SYNTAX, Token));
    Context.Advance; // Consume TO/STEP/UNTIL
    DoNodeCreated(Result);
    Exit;
  end;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume TO/STEP/UNTIL
  DoNodeCreated(Result);
end;

function TPackratParser.ParseConditionalJumpStatement: TASTNode;
var
  Expression, TargetList, TargetNode: TASTNode;
  Token, JumpToken: TLexerToken;
  IsGosub, IsLocal: Boolean;
begin
  Token := Context.CurrentToken;
  Context.Advance; // Consume ON

  // FreeBASIC/QB error handling: ON [LOCAL] ERROR GOTO <label|0>.
  // LOCAL is accepted and treated as a global handler in v1 (no per-procedure scoping).
  // ERROR/LOCAL are matched by token value (not reserved keywords).
  IsLocal := False;
  if UpperCase(Context.CurrentToken.Value) = 'LOCAL' then
  begin
    IsLocal := True;
    Context.Advance; // consume LOCAL
  end;
  if UpperCase(Context.CurrentToken.Value) = kERROR then
  begin
    Context.Advance; // consume ERROR
    Result := TASTNode.Create(antOnError, Token);
    if IsLocal then
      Result.Value := 'LOCAL';
    // Expect GOTO (matched by value to be robust to token classification)
    if UpperCase(Context.CurrentToken.Value) = kGOTO then
    begin
      Context.Advance; // consume GOTO
      TargetNode := ParseExpression;  // label identifier, or line number (0 disables)
      if Assigned(TargetNode) then
        Result.AddChild(TargetNode);
    end
    else
    begin
      HandleError('Expected GOTO after ON ERROR', Context.CurrentToken);
      Result.Free;
      Result := nil;
      Exit;
    end;
    DoNodeCreated(Result);
    Exit;
  end;

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

  // Two-word "GO TO" form: consume the trailing TO after the GO keyword.
  if (UpperCase(JumpToken.Value) = kGO_TO) and Assigned(Context.CurrentToken) and
     (UpperCase(Context.CurrentToken.Value) = kTO) then
    Context.Advance; // Consume TO

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

function TPackratParser.ParseFBPokeStatement(Token: TLexerToken): TASTNode;
// FreeBASIC "POKE [datatype,] pointer, value" as an assignment through a typed dereference:
// antAssignment( antDeref(antCast("<T> PTR", <ptr>)), <value> ). The current token is POKE.
//
// Speculative: it rewinds and returns nil if the shape does not hold, so a MODERN program that
// somehow reaches here with something else keeps the old path rather than failing to parse.
var
  Saved: Integer;
  TypeStr: string;
  PtrExpr, ValExpr, CastNode, DerefNode: TASTNode;
begin
  Result := nil;
  Context.SavePosition(Saved);
  Context.Advance;                                    // consume POKE
  TypeStr := '';
  if Context.Check(ttIdentifier) and IsBuiltinTypeName(VarToStr(Context.CurrentToken.Value)) and
     (UpperCase(VarToStr(Context.CurrentToken.Value)) <> 'STRING') then
  begin
    TypeStr := UpperCase(VarToStr(Context.CurrentToken.Value));
    Context.Advance;
    if Context.Check(ttSeparParam) then
      Context.Advance                                 // consume ',' after the datatype
    else
    begin
      Context.RestorePosition(Saved);                 // a variable that happens to be named like a type
      Exit;
    end;
  end;
  if TypeStr = '' then TypeStr := 'UBYTE';
  PtrExpr := ParseExpression;
  if not Assigned(PtrExpr) then begin Context.RestorePosition(Saved); Exit; end;
  if not Context.Check(ttSeparParam) then
  begin
    PtrExpr.Free; Context.RestorePosition(Saved); Exit;
  end;
  Context.Advance;                                    // consume ',' before the value
  ValExpr := ParseExpression;
  if not Assigned(ValExpr) then
  begin
    PtrExpr.Free; Context.RestorePosition(Saved); Exit;
  end;
  CastNode := TASTNode.CreateWithValue(antCast, TypeStr + ' PTR', Token);
  CastNode.AddChild(PtrExpr);
  DerefNode := TASTNode.Create(antDeref, Token);
  DerefNode.AddChild(CastNode);
  Result := TASTNode.Create(antAssignment, Token);
  Result.AddChild(DerefNode);
  Result.AddChild(ValExpr);
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
    // FreeBASIC "POKE [datatype,] pointer, value" writes REAL memory at an address; the Commodore
    // "POKE address, value" writes the emulated memory map. Same name, two dialects, and they stay
    // separate -- only MODERN takes the branch below, and CLASSIC keeps antPoke untouched. The FB form
    // is exactly a typed pointer store, so it desugars to "*CPtr(<T> Ptr, ptr) = value" and needs no
    // node, no SSA op and no VM arm of its own. Untyped means UBYTE, fbc's default.
    if FModernMode then
    begin
      Result := ParseFBPokeStatement(Token);
      if Assigned(Result) then Exit;
    end;
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
    // Other memory commands: BANK (RAM bank select), FETCH/STASH (host<->expansion-RAM DMA), RREG (6502
    // register read after SYS). None of this hardware exists in a portable VM, so they are accept-and-ignore
    // no-ops: the arguments are parsed and discarded (an empty statement emits no code, and no 'unhandled
    // node type' warning). RREG's target variables are left at their default 0.
    Result := TASTNode.Create(antStatement, Token);
    Context.Advance; // Consume memory command
    if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
    begin
      Param1 := ParseExpressionList(ttSeparParam);
      if Assigned(Param1) then
        Param1.Free;   // consumed and discarded (unemulated hardware -> no-op)
    end;
  end;

  DoNodeCreated(Result);
end;

function TPackratParser.ParseGraphicsStatement: TASTNode;
var
  Token: TLexerToken;
  Param: TASTNode;
  ParamCount, MaxParams: Integer;
  CircleArgIdx: Integer;
  TargetNode: TASTNode;
  CmdName: string;
begin
  Token := Context.CurrentToken;
  CmdName := UpperCase(Token.Value);
  TargetNode := nil;

  // Select appropriate node type based on command
  if CmdName = 'GRAPHIC' then
    Result := TASTNode.Create(antGraphics, Token)
  else if (CmdName = 'SCNCLR') or (CmdName = 'CLS') then
    Result := TASTNode.Create(antScnClr, Token)
  else if CmdName = 'BOX' then
    Result := TASTNode.Create(antBox, Token)
  else if CmdName = 'CIRCLE' then
  begin
    // FreeBASIC CIRCLE (x,y),r[,color] vs C128 CIRCLE source,x,y,... — disambiguated by the parenthesis
    // (or a leading STEP "CIRCLE STEP(x,y),r", or the image-target form "CIRCLE img,(x,y),r").
    if (Assigned(Context.PeekNext) and
        ((Context.PeekNext.TokenType = ttDelimParOpen) or (UpperCase(Context.PeekNext.Value) = kSTEP))) or
       LooksLikeImageTarget then
      Result := TASTNode.Create(antGfxCircle, Token)
    else
      Result := TASTNode.Create(antCircle, Token);
  end
  else if CmdName = 'DRAW' then
  begin
    // "DRAW STRING ..." is a different statement from DRAW, not a variant of it: FreeBASIC's text blit
    // versus Commodore's coordinate/TO form. Decided on the very next word, before either grammar runs.
    // MODERN only - v7 has no DRAW STRING, and a CLASSIC program is entitled to a variable called
    // STRING no more than a MODERN one is, but the dialect gate keeps the two decisions apart.
    if FModernMode and Assigned(Context.PeekNext) and
       (UpperCase(VarToStr(Context.PeekNext.Value)) = 'STRING') then
    begin
      Result := TASTNode.Create(antGfxDrawString, Token);
      // Consume DRAW here; the shared Advance below this if-chain then consumes STRING, so both words
      // are gone by the time the argument grammar runs. Two words, two advances - one of them borrowed.
      Context.Advance;
    end
    else
      Result := TASTNode.Create(antDraw, Token);
  end
  else if CmdName = 'LOCATE' then
    Result := TASTNode.Create(antLocate, Token)
  else if CmdName = 'COLOR' then
  begin
    // FreeBASIC COLOR [fg][,bg] (draw colours) vs C128 COLOR source,color — disambiguated by dialect.
    if FModernMode then
      Result := TASTNode.Create(antGfxColor, Token)
    else
      Result := TASTNode.Create(antColor, Token);
  end
  else if CmdName = 'SETCOLOR' then
    Result := TASTNode.Create(antSetColor, Token)
  else if CmdName = 'WIDTH' then
    Result := TASTNode.Create(antWidth, Token)
  else if CmdName = 'SCALE' then
    Result := TASTNode.Create(antScale, Token)
  else if CmdName = 'PAINT' then
  begin
    // FreeBASIC PAINT (x,y),color vs C128 PAINT source,x,y,mode — disambiguated by the parenthesis
    // (or the image-target form "PAINT img,(x,y),color").
    if (Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttDelimParOpen)) or
       LooksLikeImageTarget then
      Result := TASTNode.Create(antGfxPaint, Token)
    else
      Result := TASTNode.Create(antPaint, Token);
  end
  else if CmdName = 'WINDOW' then
  begin
    // FreeBASIC graphics WINDOW [SCREEN] (x1,y1)-(x2,y2) / bare WINDOW (disable) vs C128 text WINDOW
    // col1,row1,col2,row2 — disambiguated by a leading '(' / SCREEN keyword, or a bare WINDOW (no args:
    // C128 WINDOW always has arguments, so a bare WINDOW is the FB "disable" form).
    if (not Assigned(Context.PeekNext)) or
       (Context.PeekNext.TokenType = ttDelimParOpen) or (UpperCase(Context.PeekNext.Value) = 'SCREEN') or
       (Context.PeekNext.TokenType in [ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
      Result := TASTNode.Create(antGfxWindow, Token)
    else
      Result := TASTNode.Create(antWindow, Token);
  end
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
  else if CmdName = 'SCREENRES' then
    Result := TASTNode.Create(antScreenRes, Token)
  else if CmdName = 'PSET' then
    Result := TASTNode.Create(antGfxPset, Token)
  else if CmdName = 'PRESET' then
    Result := TASTNode.Create(antGfxPreset, Token)
  else if CmdName = 'PALETTE' then
    Result := TASTNode.Create(antPalette, Token)
  else if CmdName = 'IMAGEDESTROY' then
    Result := TASTNode.Create(antImageDestroy, Token)
  else if CmdName = 'IMAGECONVERTROW' then
    Result := TASTNode.Create(antImageConvertRow, Token)
  else if CmdName = 'IMAGEINFO' then
    Result := TASTNode.Create(antImageInfo, Token)
  else if CmdName = 'VIEW' then
  begin
    // FB graphics VIEW [SCREEN] [(x1,y1)-(x2,y2)] vs "VIEW PRINT" (the text print area / scroll region).
    if Assigned(Context.PeekNext) and (UpperCase(Context.PeekNext.Value) = 'PRINT') then
      Result := TASTNode.Create(antViewPrint, Token)
    else
      Result := TASTNode.Create(antGfxView, Token);
  end
  else if CmdName = 'SCREEN' then
    Result := TASTNode.Create(antGfxScreen, Token)
  else if CmdName = 'SCREENINFO' then
    Result := TASTNode.Create(antScreenInfo, Token)
  else if (CmdName = 'SCREENLOCK') or (CmdName = 'SCREENUNLOCK') or
          (CmdName = 'SCREENSYNC') or (CmdName = 'WINDOWTITLE') then
  begin
    // ⛔ SCREENLOCK and SCREENUNLOCK are NOT nops any more (22 Aug 2026): they are the frame
    // boundary, and the windowed presenter needs it. The node type stays antGfxNop so the argument
    // skipping below is shared; which of the four it is rides on the attribute. SCREENSYNC and
    // WINDOWTITLE really are accept-and-ignore.
    Result := TASTNode.Create(antGfxNop, Token);
    Result.Attributes.Values['NOP'] := CmdName;
  end
  else if CmdName = 'SCREENSET' then
  begin
    Result := TASTNode.Create(antScreenSet, Token);
    Result.Attributes.Values['OP'] := 'SET';
  end
  else if CmdName = 'FLIP' then
  begin
    Result := TASTNode.Create(antScreenSet, Token);
    Result.Attributes.Values['OP'] := 'FLIP';
  end
  else if CmdName = 'PCOPY' then
  begin
    Result := TASTNode.Create(antPCopy, Token);
    Result.Attributes.Values['OP'] := 'PCOPY';
  end
  else if CmdName = 'SCREENCOPY' then
  begin
    Result := TASTNode.Create(antPCopy, Token);
    Result.Attributes.Values['OP'] := 'SCREENCOPY';
  end
  else if CmdName = 'SETMOUSE' then
    Result := TASTNode.Create(antGfxSetmouse, Token)
  else
    Result := TASTNode.Create(antStatement, Token);

  Context.Advance;
  ParamCount := 0;

  // "Cls()", "ScreenLock()", "ScreenUnlock()" - FreeBASIC lets a no-argument statement be written with
  // empty parentheses, and gfx/screenlock.bas uses all three that way. Consumed here, once, for every
  // graphics statement rather than in each branch below.
  // Requires the ')' to follow the '(' IMMEDIATELY, so "PSET (x,y)" - which also opens with a
  // parenthesis - is untouched.
  if Context.Check(ttDelimParOpen) and Assigned(Context.PeekNext) and
     (Context.PeekNext.TokenType = ttDelimParClose) then
  begin
    Context.Advance;                                             // '('
    Context.Advance;                                             // ')'
  end;

  // FreeBASIC PSET/PRESET (x, y) [, color] and PAINT (x, y) [, color]: the coordinate pair is
  // parenthesised, so parse it explicitly rather than via the generic comma-separated parameter loop below.
  if (CmdName = 'PSET') or (CmdName = 'PRESET') or (Result.NodeType = antGfxPaint) then
  begin
    // FreeBASIC image draw target: "PSET img, (x,y)" — an image handle before the coordinate.
    if (not Context.Check(ttDelimParOpen)) and (UpperCase(Context.CurrentToken.Value) <> kSTEP) then
    begin
      TargetNode := ParseExpression;                              // image handle
      if Context.Check(ttSeparParam) then Context.Advance;        // ','
    end;
    // FreeBASIC STEP: the coordinate is relative to the current graphics point.
    if UpperCase(Context.CurrentToken.Value) = kSTEP then
    begin
      Result.Attributes.Values['STEP'] := '1';
      Context.Advance;                                            // STEP
    end;
    if Context.Check(ttDelimParOpen) then Context.Advance;        // '('
    Result.AddChild(ParseExpression);                             // x
    if Context.Check(ttSeparParam) then Context.Advance;          // ','
    Result.AddChild(ParseExpression);                             // y
    if Context.Check(ttDelimParClose) then Context.Advance;       // ')'
    if Context.Check(ttSeparParam) then
    begin
      Context.Advance;                                            // ','
      Result.AddChild(ParseExpression);                           // color
      // FreeBASIC PAINT (x,y), color, border : an optional border colour selects the boundary-fill form.
      if (Result.NodeType = antGfxPaint) and Context.Check(ttSeparParam) then
      begin
        Context.Advance;                                          // ','
        Result.AddChild(ParseExpression);                         // border colour
        Result.Attributes.Values['HASBORDER'] := '1';
      end;
    end;
    if Assigned(TargetNode) then   // image draw target appended last (TARGETIDX = its child index)
    begin
      Result.Attributes.Values['TARGETIDX'] := IntToStr(Result.ChildCount);
      Result.AddChild(TargetNode);
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC DRAW STRING [img,] [STEP] (x, y), text [, colour] : text drawn INTO the surface with the
  // built-in 8x8 font. Nothing about it is the Commodore DRAW, which is a coordinate/TO form, so it is
  // taken here before that grammar sees the line.
  // ⚠️ Until now the word STRING after DRAW was simply not looked for: "Draw String img,(x,y),s" was a
  // syntax error and "Draw String (x,y),s,c" was ACCEPTED AND DREW NOTHING - the framebuffer checksum
  // was identical before and after. That silence is what produced an empty logo mask during the demo
  // work, and what kept a tick in BASIC.md the statement had not earned.
  if (Result.NodeType = antGfxDrawString) then
  begin
    // Image draw target: "Draw String img, (x,y), s". Same convention as PSET/CIRCLE - appended last,
    // its index in TARGETIDX - so it rides on the existing SetTarget pair with nothing new to lower.
    if (not Context.Check(ttDelimParOpen)) and (UpperCase(Context.CurrentToken.Value) <> kSTEP) then
    begin
      TargetNode := ParseExpression;                              // image handle
      if Context.Check(ttSeparParam) then Context.Advance;        // ','
    end;
    if UpperCase(Context.CurrentToken.Value) = kSTEP then
    begin
      Result.Attributes.Values['STEP'] := '1';
      Context.Advance;                                            // STEP
    end;
    if Context.Check(ttDelimParOpen) then Context.Advance;        // '('
    Result.AddChild(ParseExpression);                             // x
    if Context.Check(ttSeparParam) then Context.Advance;          // ','
    Result.AddChild(ParseExpression);                             // y
    if Context.Check(ttDelimParClose) then Context.Advance;       // ')'
    if Context.Check(ttSeparParam) then Context.Advance;          // ','
    Result.AddChild(ParseExpression);                             // the text
    if Context.Check(ttSeparParam) then
    begin
      Context.Advance;                                            // ','
      // ⚠️ The colour may be LEFT OUT and a font given instead - "Draw String (x,y), s, , myFont" is
      // the manual's own custom-font example. ParseExpression returns NIL on a bare comma, and this
      // line went in without that guard: the same omitted-argument defect that had just been cleared
      // out of the other eight graphics statements, written afresh into the ninth. An omitted colour
      // means the current foreground, which is what the -1 default in ProcessGfxDrawString asks for -
      // so the child is simply not added.
      if not Context.CheckAny([ttSeparParam, ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
        Result.AddChild(ParseExpression);                         // colour
    end;
    // A trailing font argument ("...,, font") is accepted and ignored: we have ONE built-in font, and
    // declining the line would be worse than drawing it in the only face we have.
    while Context.Check(ttSeparParam) do
    begin
      Context.Advance;
      if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
      begin
        Param := ParseExpression;
        if Assigned(Param) then Param.Free;
      end;
    end;
    if Assigned(TargetNode) then
    begin
      Result.Attributes.Values['TARGETIDX'] := IntToStr(Result.ChildCount);
      Result.AddChild(TargetNode);
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC CIRCLE (x, y), r [, color]: parenthesised centre then radius (and optional colour).
  if Result.NodeType = antGfxCircle then
  begin
    // FreeBASIC image draw target: "CIRCLE img, (x,y), r".
    if (not Context.Check(ttDelimParOpen)) and (UpperCase(Context.CurrentToken.Value) <> kSTEP) then
    begin
      TargetNode := ParseExpression;                              // image handle
      if Context.Check(ttSeparParam) then Context.Advance;        // ','
    end;
    // FreeBASIC STEP: the centre is relative to the current graphics point.
    if UpperCase(Context.CurrentToken.Value) = kSTEP then
    begin
      Result.Attributes.Values['STEP'] := '1';
      Context.Advance;                                            // STEP
    end;
    if Context.Check(ttDelimParOpen) then Context.Advance;        // '('
    Result.AddChild(ParseExpression);                             // x
    if Context.Check(ttSeparParam) then Context.Advance;          // ','
    Result.AddChild(ParseExpression);                             // y
    if Context.Check(ttDelimParClose) then Context.Advance;       // ')'
    if Context.Check(ttSeparParam) then Context.Advance;          // ','
    Result.AddChild(ParseExpression);                             // radius
    // Optional trailing parameters, in FreeBASIC order: color, start-angle, end-angle, aspect, and a
    // fill flag (F). Any may be omitted with an empty comma ("CIRCLE (x,y),r,,,,,F"). They are captured
    // into fixed child slots (3=colour, 4=start, 5=end, 6=aspect) with HAS* presence attributes, plus 0/1
    // placeholders for omitted ones, so ProcessGfxCircle sees a stable layout. start/end are angles in
    // radians; aspect is the y/x radius ratio. The fill flag (F) is captured but deferred (no filled-ellipse
    // primitive). ProcessGfxCircle draws a plain circle when no arc/aspect is present.
    for CircleArgIdx := 0 to 3 do   // 0=colour, 1=start, 2=end, 3=aspect
    begin
      if not Context.Check(ttSeparParam) then Break;
      Context.Advance;                                            // ','
      if Context.CheckAny([ttSeparParam, ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
      begin
        // omitted (empty comma): placeholder (aspect default = 1, others = 0), no HAS* flag
        if CircleArgIdx = 3 then Result.AddChild(TASTNode.CreateWithValue(antLiteral, '1', Token))
        else Result.AddChild(TASTNode.CreateWithValue(antLiteral, '0', Token));
      end
      else
      begin
        // Distinguish a bare "F" fill flag (last, unquoted identifier) from a value expression.
        if (Context.Check(ttIdentifier) and (UpperCase(Context.CurrentToken.Value) = 'F')) and
           (not Assigned(Context.PeekNext) or
            (Context.PeekNext.TokenType in [ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse])) then
        begin
          Result.Attributes.Values['FILL'] := '1';
          Context.Advance;                                        // F
          // pad the remaining slots with placeholders
          while Result.ChildCount < 7 do
            if Result.ChildCount = 6 then Result.AddChild(TASTNode.CreateWithValue(antLiteral, '1', Token))
            else Result.AddChild(TASTNode.CreateWithValue(antLiteral, '0', Token));
          Break;
        end;
        Result.AddChild(ParseExpression);                         // present value
        case CircleArgIdx of
          0: Result.Attributes.Values['HASCOLOR']  := '1';
          1: Result.Attributes.Values['HASSTART']  := '1';
          2: Result.Attributes.Values['HASEND']    := '1';
          3: Result.Attributes.Values['HASASPECT'] := '1';
        end;
      end;
    end;
    // A trailing ",F" after aspect (fill flag) — capture and ignore the value.
    if Context.Check(ttSeparParam) then
    begin
      Context.Advance;
      if Context.Check(ttIdentifier) and (UpperCase(Context.CurrentToken.Value) = 'F') then
      begin
        Result.Attributes.Values['FILL'] := '1';
        Context.Advance;
      end;
    end;
    // Ensure the fixed 7-child layout (x,y,r,colour,start,end,aspect) even when trailing args were omitted.
    while Result.ChildCount < 7 do
      if Result.ChildCount = 6 then Result.AddChild(TASTNode.CreateWithValue(antLiteral, '1', Token))
      else Result.AddChild(TASTNode.CreateWithValue(antLiteral, '0', Token));
    if Assigned(TargetNode) then   // image draw target appended after the fixed layout (child 7)
    begin
      Result.Attributes.Values['TARGETIDX'] := IntToStr(Result.ChildCount);
      Result.AddChild(TargetNode);
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC PALETTE — forms (PALETTE USING deferred):
  //   PALETTE                       -> reset the palette to the mode default              (OP=RESET)
  //   PALETTE index, r, g, b        -> set entry index to (r,g,b), components 0-255       (OP=SET)
  //   PALETTE index, &hBBGGRR       -> set entry from a packed BGR value, components 0-63 (OP=SETPACKED)
  //   PALETTE GET index, r, g, b    -> read entry index into the r, g, b variables        (OP=GET)
  if Result.NodeType = antPalette then
  begin
    if Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
      Result.Attributes.Values['OP'] := 'RESET'
    else
    begin
      if UpperCase(Context.CurrentToken.Value) = 'GET' then
      begin
        Result.Attributes.Values['OP'] := 'GET';
        Context.Advance;                                          // GET
      end
      else
        Result.Attributes.Values['OP'] := 'SET';
      Result.AddChild(ParseExpression);                           // index
      if Context.Check(ttSeparParam) then Context.Advance;        // ','
      Result.AddChild(ParseExpression);                           // r / packed BGR colour (or r-variable for GET)
      if Context.Check(ttSeparParam) then
      begin
        Context.Advance;                                          // ','
        Result.AddChild(ParseExpression);                         // g
        if Context.Check(ttSeparParam) then Context.Advance;      // ','
        Result.AddChild(ParseExpression);                         // b
      end
      else if Result.Attributes.Values['OP'] = 'SET' then
        Result.Attributes.Values['OP'] := 'SETPACKED';            // 2-arg form: PALETTE index, &hBBGGRR
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC COLOR [fg] [, bg] — set the current draw foreground/background colour. Either may be
  // omitted ("COLOR fg", "COLOR fg, bg", "COLOR , bg"). HASFG/HASBG attributes record which are present.
  if Result.NodeType = antGfxColor then
  begin
    if not Context.CheckAny([ttSeparParam, ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
    begin
      Result.AddChild(ParseExpression);                           // fg
      Result.Attributes.Values['HASFG'] := '1';
    end;
    if Context.Check(ttSeparParam) then
    begin
      Context.Advance;                                            // ','
      if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
      begin
        Result.AddChild(ParseExpression);                         // bg
        Result.Attributes.Values['HASBG'] := '1';
      end;
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC SETMOUSE [x] [, y] [, visibility] [, clip] — set the mouse position/visibility. Each field is
  // optional (-1 = "no change"); an omitted slot is an empty antLiteral placeholder. Parens are optional
  // (both `SetMouse 320,240` and `SetMouse(320,240)` are accepted). Children: x, y, visibility, clip.
  if Result.NodeType = antGfxSetmouse then
  begin
    if Context.Check(ttDelimParOpen) then Context.Advance;       // optional '('
    while not Context.CheckAny([ttDelimParClose, ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
    begin
      if Context.Check(ttSeparParam) then
        Result.AddChild(TASTNode.CreateWithValue(antLiteral, -1, Token))  // empty slot -> "no change"
      else
        Result.AddChild(ParseExpression);
      if Context.Check(ttSeparParam) then Context.Advance         // ','
      else Break;
    end;
    if Context.Check(ttDelimParClose) then Context.Advance;      // optional ')'
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC IMAGECONVERTROW src, src_bpp, dst, dst_bpp, width [, isrgb] -- a SUB, so it is a comma
  // list of expressions (optionally parenthesised, as any FB SUB call may be).
  if Result.NodeType = antImageConvertRow then
  begin
    if Context.Check(ttDelimParOpen) then Context.Advance;
    repeat
      Result.AddChild(ParseExpression);
      if not Context.Check(ttSeparParam) then Break;
      Context.Advance;
    until Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]);
    if Context.Check(ttDelimParClose) then Context.Advance;
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC IMAGEDESTROY handle
  if Result.NodeType = antImageDestroy then
  begin
    Result.AddChild(ParseExpression);                            // handle
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC IMAGEINFO handle, w, h  (writes width/height into the w and h variables)
  if Result.NodeType = antImageInfo then
  begin
    Result.AddChild(ParseExpression);                            // handle
    if Context.Check(ttSeparParam) then Context.Advance;          // ','
    Result.AddChild(ParseExpression);                            // w variable
    if Context.Check(ttSeparParam) then Context.Advance;          // ','
    Result.AddChild(ParseExpression);                            // h variable
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC SCREENINFO w, h [, depth, bpp, pitch, rate] — writes the screen's info into the variables.
  if Result.NodeType = antScreenInfo then
  begin
    // FreeBASIC accepts the PARENTHESISED spelling too - gfx/cls-memset.bas writes
    // "ScreenInfo( , scrhei, , , scrpitch )", omitted slots and all. Only the bare form was parsed, so
    // that file died on the '(' before reaching anything it was meant to test.
    if Context.Check(ttDelimParOpen) then Context.Advance;       // '('
    // Any destination may be left out - "ScreenInfo w, h, depth,,,,driver_name" is the manual's own
    // example - and the POSITION still counts, because it is what selects the field. A skipped slot used
    // to reach the AST as a NIL child (ParseExpression returns nil on a bare comma) and every pre-pass
    // that walks the tree was one step from an access violation; SSA generation took it.
    // Unlike the graphics statements, the placeholder cannot be a value-carrying 0 - these arguments are
    // DESTINATIONS and there is nothing to write into a 0. It is a literal all the same, because a
    // literal can never BE a destination: that makes "is this slot omitted?" an exact test downstream,
    // with no extra marker to keep in step.
    if Context.CheckAny([ttSeparParam, ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse,
                         ttDelimParClose]) then
      Result.AddChild(TASTNode.CreateWithValue(antLiteral, 0, Token))
    else
      Result.AddChild(ParseExpression);                          // w variable
    while Context.Check(ttSeparParam) do
    begin
      Context.Advance;                                          // ','
      if Context.CheckAny([ttSeparParam, ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse,
                           ttDelimParClose]) then
        Result.AddChild(TASTNode.CreateWithValue(antLiteral, 0, Token))
      else
        Result.AddChild(ParseExpression);                       // next variable
    end;
    if Context.Check(ttDelimParClose) then Context.Advance;     // ')' of the parenthesised spelling
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC SCREENLOCK / SCREENUNLOCK / SCREENSYNC / WINDOWTITLE — accept-and-ignore (sync/caption
  // primitives with no effect on the buffered/headless backend). Consume any arguments, emit nothing.
  if Result.NodeType = antGfxNop then
  begin
    while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
      Context.Advance;
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC WINDOW [SCREEN] [(x1,y1)-(x2,y2)] — set/clear the logical coordinate system. SCREEN (no
  // y-flip) is recorded in the SCREEN attribute; no bounds = disable. Children: x1, y1, x2, y2.
  if Result.NodeType = antGfxWindow then
  begin
    if UpperCase(Context.CurrentToken.Value) = 'SCREEN' then
    begin
      Result.Attributes.Values['SCREEN'] := '1';
      Context.Advance;                                         // SCREEN
    end;
    if Context.Check(ttDelimParOpen) then
    begin
      Context.Advance;                                         // '('
      Result.AddChild(ParseExpression);                        // x1
      if Context.Check(ttSeparParam) then Context.Advance;     // ','
      Result.AddChild(ParseExpression);                        // y1
      if Context.Check(ttDelimParClose) then Context.Advance;  // ')'
      if Context.Check(ttOpSub) then Context.Advance;          // '-'
      if Context.Check(ttDelimParOpen) then Context.Advance;   // '('
      Result.AddChild(ParseExpression);                        // x2
      if Context.Check(ttSeparParam) then Context.Advance;     // ','
      Result.AddChild(ParseExpression);                        // y2
      if Context.Check(ttDelimParClose) then Context.Advance;  // ')'
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC SCREEN mode [, depth [, num_pages [, ...]]] — numbered graphics mode. Children: mode,
  // [depth], [num_pages]; depth is ignored, num_pages drives page allocation.
  if Result.NodeType = antGfxScreen then
  begin
    // The FIRST argument may be omitted too ("Screen , 0, 1" - fblite's own console page-flip idiom,
    // gfx/pcopy_cons.bas). The loop below already substitutes a 0 for a later empty slot; the first one
    // went straight to ParseExpression, which returned NIL, and ProcessGfxScreen took an access
    // violation on it. The same 0 is not just crash-free but CORRECT here: the VM already reads mode 0
    // as "no graphics mode change" (it leaves WinW/WinH at 0 and skips SetupGfxScreen), which is exactly
    // what FreeBASIC means by an omitted mode - keep the current one.
    if Context.Check(ttSeparParam) then
      Result.AddChild(TASTNode.CreateWithValue(antLiteral, 0, Token))       // mode omitted
    else
      Result.AddChild(ParseExpression);                        // mode
    while Context.Check(ttSeparParam) do
    begin
      Context.Advance;                                         // ','
      if not Context.CheckAny([ttSeparParam, ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
        Result.AddChild(ParseExpression)
      else
        Result.AddChild(TASTNode.CreateWithValue(antLiteral, 0, Token));   // empty arg placeholder
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC VIEW [SCREEN] [(x1,y1)-(x2,y2)] — set/clear the viewport. SCREEN = absolute coords (no
  // offset); no bounds = reset to full screen. Children: x1, y1, x2, y2. (Optional fill/border deferred.)
  if Result.NodeType = antGfxView then
  begin
    if UpperCase(Context.CurrentToken.Value) = 'SCREEN' then
    begin
      Result.Attributes.Values['SCREEN'] := '1';
      Context.Advance;                                         // SCREEN
    end;
    if Context.Check(ttDelimParOpen) then
    begin
      Context.Advance;                                         // '('
      Result.AddChild(ParseExpression);                        // x1
      if Context.Check(ttSeparParam) then Context.Advance;     // ','
      Result.AddChild(ParseExpression);                        // y1
      if Context.Check(ttDelimParClose) then Context.Advance;  // ')'
      if Context.Check(ttOpSub) then Context.Advance;          // '-'
      if Context.Check(ttDelimParOpen) then Context.Advance;   // '('
      Result.AddChild(ParseExpression);                        // x2
      if Context.Check(ttSeparParam) then Context.Advance;     // ','
      Result.AddChild(ParseExpression);                        // y2
      if Context.Check(ttDelimParClose) then Context.Advance;  // ')'
      // optional ,fill[,border] — consumed and ignored (deferred)
      while Context.Check(ttSeparParam) do
      begin
        Context.Advance;
        if not Context.CheckAny([ttSeparParam, ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
          ParseExpression;
      end;
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  // VIEW PRINT [firstrow TO lastrow]: the text print area. With no bounds the whole screen is used.
  // The leading VIEW was consumed by the caller; PRINT is still on the token stream.
  if Result.NodeType = antViewPrint then
  begin
    if UpperCase(Context.CurrentToken.Value) = 'PRINT' then
      Context.Advance;                                          // PRINT
    if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
    begin
      Result.AddChild(ParseExpression);                         // firstrow
      if Context.Check(ttLoopControl) and (UpperCase(VarToStr(Context.CurrentToken.Value)) = kTO) then
      begin
        Context.Advance;                                        // TO
        Result.AddChild(ParseExpression);                       // lastrow
      end;
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC page-flipping primitives: SCREENSET/FLIP (antScreenSet) and PCOPY/SCREENCOPY (antPCopy).
  // All take 0-2 optional comma-separated page expressions; the SSA maps them to Src1/Src2 + flags.
  if (Result.NodeType = antScreenSet) or (Result.NodeType = antPCopy) then
  begin
    if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
    begin
      Result.AddChild(ParseExpression);                          // first page
      if Context.Check(ttSeparParam) then
      begin
        Context.Advance;                                         // ','
        Result.AddChild(ParseExpression);                        // second page
      end;
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  // Set max parameters based on command
  if CmdName = 'CIRCLE' then
    MaxParams := 9
  else if CmdName = 'BOX' then
    MaxParams := 8
  else if CmdName = 'LOCATE' then
    // THREE, not two. FreeBASIC's form is "Locate [row][, [col][, [cursor]]]" and the third argument
    // was never parsed: the parser stopped after two and left ", 1" in the token stream, where it became
    // a STATEMENT of its own - an orphan the SSA walker met as "Unhandled node type 0" and warned about
    // once per LOCATE. Pre-existing (reproduces on the archived 6a14e23 binary with "Locate 3, 20, 1");
    // ProcessLocate now evaluates the flag and documents why there is nothing to set headless.
    MaxParams := 3
  else if CmdName = 'DRAW' then
    MaxParams := 100  // DRAW can have many TO segments
  else if (CmdName = 'SCNCLR') or (CmdName = 'CLS') then
    MaxParams := 1  // SCNCLR [mode] / CLS - optional mode 0-11
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
  else if CmdName = 'SCREENRES' then
    MaxParams := 4  // SCREENRES w, h [, depth [, num_pages]]
  else
    MaxParams := 5;

  // Special handling for DRAW: parse color, x1, y1 [TO x2, y2] ...
  // Each segment (including TO keyword) is stored as children
  if CmdName = 'DRAW' then
  begin
    // Parse optional color (can be omitted but comma must remain).
    // A literal 0, NOT a nil placeholder - see the note at the general omitted-parameter site below.
    if Context.Check(ttSeparParam) then
    begin
      Result.AddChild(TASTNode.CreateWithValue(antLiteral, 0, Context.CurrentToken));
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
      // An OMITTED parameter ("Circle , x, y, r", "Locate , 20", "GShape a$, , y") keeps its position
      // so the ones after it still line up. That placeholder used to be a NIL CHILD, and not one of the
      // handlers downstream expected it: every single one called ProcessExpression on it and took an
      // ACCESS VIOLATION that killed the whole SSA generation. Eight of the eleven graphics statements
      // died on their own documented syntax - Circle, SetColor, Paint, Window, SShape, GShape, Draw and
      // Locate - and the one that surfaced it, threads/threadcall.bas, has nothing to do with threads:
      // it writes "Locate , 20" to line up a column.
      //
      // A literal 0 instead. Nothing downstream ever DISTINGUISHED nil from 0 - it could not, it crashed
      // first - so this cannot change a program that works today, and 0 is already what each of these
      // commands means by an omitted argument: source page 0, colour 0, "keep the current coordinate"
      // for LOCATE (the VM resolves that one). One fix where the placeholder is BORN, rather than the
      // same guard repeated at thirty-odd call sites that would each have to remember it.
      if Context.Check(ttSeparParam) then
      begin
        Result.AddChild(TASTNode.CreateWithValue(antLiteral, 0, Context.CurrentToken));
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
  else if CmdName = 'BEEP' then
    Result := TASTNode.Create(antBeep, Token)
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
  else if CmdName = 'BEEP' then
    MaxParams := 0  // No arguments (console bell)
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
  Param, HandleNode, LenExpr, EncExpr: TASTNode;
  CmdName, ModeStr, MW, EncMark: string;
  C64Name, C64Rest, C64Base: string;   // C64 OPEN lf,dev,sa,"name[,type][,mode]" decoding
  C64Dev, C64Sa, C64FileName: TASTNode;
  C64CommaPos: Integer;
  AccessRead: Boolean;
  ClosedParen: Boolean;   // "Close(fileNum)": the FreeBASIC parenthesised handle
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
    'DCLEAR', 'RESET': Result := TASTNode.Create(antDclear, Token);  // RESET (FreeBASIC) unbinds all file numbers = DCLEAR
    'RECORD': Result := TASTNode.Create(antRecord, Token);
    'FILESETEOF': Result := TASTNode.Create(antFileSetEof, Token);  // FreeBASIC: truncate/extend to current position
  else
    Result := TASTNode.Create(antStatement, Token); // Other file commands
  end;

  Context.Advance; // Consume file operation command

  // FreeBASIC FILEFLUSH [[#]filenum [, systembuffers]]: flush buffered output. Our file streams are
  // unbuffered (writes go straight to the OS), so there is nothing to flush — accept and discard the
  // optional arguments, emitting no code (Result is the empty antStatement from the case above).
  if CmdName = 'FILEFLUSH' then
  begin
    if Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#') then
      Context.Advance;
    if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
    begin
      Param := ParseExpressionList(ttSeparParam);   // filenum [, systembuffers] — consumed and discarded
      if Assigned(Param) then Param.Free;
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC LOCK / UNLOCK #filenum [, record [TO record]] : file record locking. A single-process VM has
  // no lock contention, so these are no-ops — consume the '#' prefix and the record range(s) and discard
  // them, emitting no code (Result is the empty antStatement from the case above).
  if (CmdName = 'LOCK') or (CmdName = 'UNLOCK') then
  begin
    if Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#') then
      Context.Advance;
    while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
    begin
      // consume an expression, a comma, or a TO (record range) token
      if Context.Check(ttSeparParam) or (Context.Check(ttLoopControl) and (UpperCase(Context.CurrentToken.Value) = kTO)) then
        Context.Advance
      else
      begin
        Param := ParseExpression;
        if Assigned(Param) then Param.Free else Break;
      end;
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC FILESETEOF [#]filenum : truncate/extend the open file to the current 1-based position.
  // The file number is a bare expression (number or variable), optionally prefixed with '#'.
  if CmdName = 'FILESETEOF' then
  begin
    if Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#') then
      Context.Advance;
    Param := ParseExpression;
    if Assigned(Param) then
      Result.AddChild(Param)
    else
    begin
      HandleError('Expected file number after FILESETEOF', Token);
      Exit;
    end;
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC OPEN: OPEN "filename" FOR {INPUT|OUTPUT|APPEND|BINARY|RANDOM} AS [#]n [LEN = reclen].
  // Detected when OPEN is NOT immediately followed by a '#handle' (that is the legacy C64/C128 form).
  // Built as the same antDopen node (child0=handle, child1=filename, child2=mode$) the legacy form uses.
  if ((CmdName = 'DOPEN') or (CmdName = 'OPEN')) and
     not (Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#')) then
  begin
    // FreeBASIC DEVICE names stand where a filename would: "Open Cons For Input As #1" is the standard
    // way to read stdin, and CLBG's reverse-complement / k-nucleotide / regex-redux are all built on it.
    // The device is a bare WORD, not a string, so left to the expression parser it becomes an undeclared
    // variable - an empty filename, and an open that silently reads nothing. Turned into a literal the
    // runtime recognises instead.
    // ⚠️ Matched on the WORD, not on ttIdentifier. "ERR" is also the keyword for the last error code
    // (kERR), so it never arrived here as an identifier and this branch silently skipped it: the bare
    // Err went to the expression parser, evaluated to the error code 0, and became the FILENAME. The
    // program did not fail - it wrote its diagnostics into a file called "0" in the working directory,
    // while BASIC.md ticked "OPEN ERR" as implemented. A word followed by FOR can only be the device
    // form here; the error-code function is never followed by FOR.
    // ⚠️ ...and the FOR clause is OPTIONAL: fbc takes "Open Err As #1" as readily as
    // "Open Err For Output As #1". Requiring FOR sent the bare form back to the expression parser,
    // where ERR is the error-code function - so it evaluated to 0 and became the FILENAME again, the
    // very defect the note above describes, just through the other door. A device word can only be a
    // device when the next token is FOR or AS.
    if ((UpperCase(VarToStr(Context.CurrentToken.Value)) = 'CONS') or
        (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'SCRN') or
        (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'ERR')) and
       Assigned(Context.PeekNext) and
       ((UpperCase(VarToStr(Context.PeekNext.Value)) = kFOR) or
        (UpperCase(VarToStr(Context.PeekNext.Value)) = kAS)) then
    begin
      Param := TASTNode.CreateWithValue(antLiteral,
                 UpperCase(VarToStr(Context.CurrentToken.Value)) + ':', Context.CurrentToken);
      Context.Advance;            // the device name
    end
    else
      Param := ParseExpression;   // FB filename OR C64 logical file number
    if not Assigned(Param) then begin HandleError('Expected filename after OPEN', Token); Exit; end;

    // Commodore OPEN lf, dev [, sa [, "name[,type][,mode]"]] : the first arg is the logical file number
    // and a COMMA follows (the FreeBASIC form uses FOR/AS, never a comma here). Map it to the same
    // antDopen node (handle, filename, mode$). Device/secondary-address are parsed but not emulated (v1);
    // the read/write/append mode is taken from the filename's trailing ,W/,R/,A (a leading drive "N:" is
    // stripped). With no filename (e.g. OPEN 1,8,15 command channel) the open is a harmless no-op.
    if (CmdName = 'OPEN') and Context.Check(ttSeparParam) then
    begin
      HandleNode := Param;                       // logical file number = handle
      Context.Advance;                           // ','
      C64Dev := ParseExpression; if Assigned(C64Dev) then C64Dev.Free;   // device (8=disk...) - v1 ignore
      C64FileName := nil;
      if Context.Check(ttSeparParam) then
      begin
        Context.Advance;                         // ',' before secondary address (or, rarely, the name)
        if not Context.CheckAny([ttSeparParam, ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
        begin
          C64Sa := ParseExpression; if Assigned(C64Sa) then C64Sa.Free;  // secondary address - v1 ignore
        end;
        if Context.Check(ttSeparParam) then
        begin
          Context.Advance;                       // ',' before filename
          C64FileName := ParseExpression;        // "name[,type][,mode]"
        end;
      end;
      ModeStr := 'R';
      if Assigned(C64FileName) and (C64FileName.NodeType = antLiteral) and VarIsStr(C64FileName.Value) then
      begin
        C64Name := VarToStr(C64FileName.Value);
        C64CommaPos := Pos(',', C64Name);
        if C64CommaPos > 0 then
        begin
          C64Base := Copy(C64Name, 1, C64CommaPos - 1);
          C64Rest := UpperCase(Copy(C64Name, C64CommaPos + 1, MaxInt));   // "S,W" / "W" / "S,R" ...
          if (C64Rest = 'W') or (Pos(',W', C64Rest) > 0) then ModeStr := 'W'
          else if (C64Rest = 'A') or (Pos(',A', C64Rest) > 0) then ModeStr := 'A'
          else if (C64Rest = 'R') or (Pos(',R', C64Rest) > 0) then ModeStr := 'R';
          C64Name := C64Base;
        end;
        // Strip a leading Commodore drive prefix "N:" (e.g. "0:file" -> "file").
        if (Length(C64Name) >= 2) and (C64Name[1] in ['0'..'9']) and (C64Name[2] = ':') then
          C64Name := Copy(C64Name, 3, MaxInt);
        C64FileName.Value := C64Name;
      end;
      Result.AddChild(HandleNode);                                       // child 0 = handle
      if Assigned(C64FileName) then
        Result.AddChild(C64FileName)                                     // child 1 = filename
      else
        Result.AddChild(TASTNode.CreateWithValue(antLiteral, '', Token));  // no name -> empty (no-op open)
      Result.AddChild(TASTNode.CreateWithValue(antLiteral, ModeStr, Token));  // child 2 = mode$
      DoNodeCreated(Result);
      Exit;
    end;

    ModeStr := 'R';
    if UpperCase(Context.CurrentToken.Value) = kFOR then
    begin
      Context.Advance;            // FOR
      MW := UpperCase(Context.CurrentToken.Value);
      if MW = kINPUT then ModeStr := 'R'
      else if MW = kOUTPUT then ModeStr := 'W'
      else if MW = kAPPEND then ModeStr := 'A'
      else if MW = kBINARY then ModeStr := 'B'
      // RANDOM: a record-oriented file. The mode the runtime wants is "L<reclen>" (the same relative-file
      // mode the CLASSIC DOPEN uses), but "Len = <expr>" below may be any expression -- SizeOf(rec) in
      // FreeBASIC's own example -- so the length is left as a CHILD and the mode string is completed in
      // the SSA. A bare "L" (no Len clause) means FreeBASIC's default record length of 128.
      else if MW = kRANDOM then ModeStr := 'L'
      else HandleError('Expected INPUT/OUTPUT/APPEND/BINARY/RANDOM after FOR', Token);
      Context.Advance;            // mode word
    end;
    // Optional "ENCODING <expr>" clause (FreeBASIC text encoding). The width travels on the MODE
    // string as a trailing "~<bits>" - the same way "ACCESS READ" travels as '<' - so nothing between
    // here and the file layer had to learn a new parameter. "ascii" needs no marker: our strings are
    // already UTF-8 bytes and that is what the file gets. The mapping is EncodingModeMarker, shared
    // with the function form, which used to drop the clause entirely.
    // ⛔ The marker is appended LAST, after ACCESS/LOCK - not here. It used to be appended on the spot,
    // and "Encoding "utf16" Access Read" then built "R~16<": the '<' landed INSIDE the number and the
    // handle read raw bytes. The same file without ACCESS decoded correctly, which is how a clause
    // that changes nothing on its own changed the meaning of the one before it.
    EncMark := '';
    EncExpr := nil;
    if UpperCase(Context.CurrentToken.Value) = kENCODING then
    begin
      Context.Advance;            // ENCODING
      if Context.Check(ttStringLiteral) then
      begin
        EncMark := EncodingModeMarker(VarToStr(Context.CurrentToken.Value));
        Context.Advance;   // "ascii" / "utf8" / "utf16" / ...
      end
      else
      begin
        // ⭐ The name need not be a LITERAL: fbc's own tests write "encoding encod" and
        // "encoding files(i).encoding". Then the marker cannot be baked into the mode string here -
        // it is appended at run time (SSA), and the file layer reads the NAME after the '~'.
        EncExpr := ParseExpression;
      end;
    end;
    // Optional "ACCESS {READ | WRITE | READ WRITE}" clause (FreeBASIC). Only READ-alone changes anything
    // we model: it makes the open READ-ONLY, so a MISSING file is an error where a plain "For Binary"
    // would create it. It is carried on the mode string as a trailing '<'. WRITE and READ WRITE keep the
    // mode's own behaviour (the VM enforces no share rights).
    if UpperCase(Context.CurrentToken.Value) = kACCESS then
    begin
      Context.Advance;            // ACCESS
      AccessRead := False;
      if UpperCase(Context.CurrentToken.Value) = kREAD then
      begin AccessRead := True; Context.Advance; end;
      if UpperCase(Context.CurrentToken.Value) = kWRITE then
      begin AccessRead := False; Context.Advance; end;
      // Not on RANDOM: there the mode is 'L' and the record length is appended to it in the SSA, so a
      // marker in between would be read as part of the number.
      if AccessRead and (ModeStr <> 'L') then ModeStr := ModeStr + '<';
    end;
    // The encoding marker goes on LAST, so '~' is always a suffix of the mode string (see above).
    // Not on RANDOM, whose 'L' takes the record length appended in the SSA.
    if (EncMark <> '') and (ModeStr <> 'L') then ModeStr := ModeStr + EncMark;
    // Optional lock_type clause (FreeBASIC): "SHARED" or "LOCK {READ|WRITE|READ WRITE}" — accepted and
    // ignored (single-process VM, no file locking).
    if UpperCase(Context.CurrentToken.Value) = kSHARED then
      Context.Advance             // SHARED
    else if UpperCase(Context.CurrentToken.Value) = kLOCK then
    begin
      Context.Advance;            // LOCK
      if UpperCase(Context.CurrentToken.Value) = kREAD then Context.Advance;
      if UpperCase(Context.CurrentToken.Value) = kWRITE then Context.Advance;
    end;
    if (UpperCase(Context.CurrentToken.Value) = kAS) or Context.Check(ttAsType) then
      Context.Advance;            // AS
      SkipTypeQualifiers;                     // FB: "As Const <type>"
    if Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#') then
      Context.Advance;            // optional '#'
    HandleNode := ParseFileNumberOperand;
    if not Assigned(HandleNode) then
    begin HandleError('Expected file number after AS', Token); Exit; end;
    LenExpr := nil;
    if UpperCase(Context.CurrentToken.Value) = kLEN then    // optional "LEN = reclen" (RANDOM)
    begin
      Context.Advance;
      if Context.Check(ttOpEq) then Context.Advance;
      LenExpr := ParseExpression;
    end;
    Result.AddChild(HandleNode);                            // child 0 = handle
    Result.AddChild(Param);                                 // child 1 = filename
    Result.AddChild(TASTNode.CreateWithValue(antLiteral, ModeStr, Token));  // child 2 = mode$
    // child 3 = record length expression (RANDOM only; the SSA appends it to the "L" mode).
    if LenExpr <> nil then
    begin
      if ModeStr = 'L' then Result.AddChild(LenExpr) else LenExpr.Free;
    end;
    // A run-time ENCODING name rides as an extra child, at whatever index it lands on: the attribute
    // says WHICH, so nothing has to count the optional ones.
    if EncExpr <> nil then
    begin
      if ModeStr = 'L' then EncExpr.Free
      else
      begin
        Result.Attributes.Values['ENCEXPR'] := IntToStr(Result.ChildCount);
        Result.AddChild(EncExpr);
      end;
    end;
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

    // FreeBASIC "Close" with NO file number closes EVERY open file — the same thing RESET/DCLEAR
    // does, so it becomes that node. CLASSIC is untouched: Commodore BASIC always wants a number,
    // and a bare CLOSE there stays the syntax error it has always been.
    if FModernMode and ((CmdName = 'CLOSE') or (CmdName = 'DCLOSE')) and
       Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
    begin
      Result.Free;
      Result := TASTNode.Create(antDclear, Token);
      DoNodeCreated(Result);
      Exit;
    end;

    // ⭐ FreeBASIC also writes the handle in PARENTHESES - "Close(fileNum)" - which reads like a call
    // and is not one. Accepted only in MODERN: Commodore BASIC has no such form, and letting CLASSIC
    // take it would make "CLOSE (1)" mean something it never meant.
    ClosedParen := False;
    if FModernMode and Context.Check(ttDelimParOpen) then
    begin
      ClosedParen := True;
      Context.Advance;
    end;

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
      Context.Advance;
      HandleNode := FoldFileHandlePostfix(HandleNode);
      Result.AddChild(HandleNode);
    end
    // The PARENTHESISED handle: "Close #(1)" is the same spelling as "Print #(1)" - see
    // ParseFileNumberOperand, which is where the three shapes live for the statements that share it.
    else if Context.Check(ttDelimParOpen) then
      Result.AddChild(FoldFileHandlePostfix(FExpressionParser.ParseExpression(precCall)))
    else
    begin
      HandleError('Expected file handle after #', Token);
      Exit;
    end;

    if ClosedParen then
    begin
      if Context.Check(ttDelimParClose) then
        Context.Advance
      else
      begin
        HandleError('Expected ")" after the file handle', Token);
        Exit;
      end;
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

      // Parse optional mode. Commodore writes the mode as a bare letter (,W ,R ,A ,B); the FreeBASIC and
      // quoted forms use a string expression. A bare single mode letter must be taken literally — otherwise
      // ParseExpression reads it as a (usually empty) variable and DOPEN silently falls back to read mode.
      if Context.Check(ttSeparParam) then
      begin
        Context.Advance;
        if Context.Check(ttIdentifier) and (Length(Context.CurrentToken.Value) = 1) and
           (UpCase(Context.CurrentToken.Value[1]) in ['R', 'W', 'A', 'B', 'L']) then
        begin
          ModeStr := UpperCase(Context.CurrentToken.Value);
          Context.Advance;
          // Relative file "DOPEN#lf,"name",L,reclen": fold the record length into the mode string ("L10").
          if (ModeStr = 'L') and Context.Check(ttSeparParam) then
          begin
            Context.Advance;                      // ',' before the record length
            if Context.Check(ttNumber) or Context.Check(ttInteger) then
            begin
              ModeStr := 'L' + Context.CurrentToken.Value;
              Context.Advance;
            end;
          end;
          Result.AddChild(TASTNode.CreateWithValue(antLiteral, ModeStr, Token));
        end
        else
        begin
          Param := ParseExpression;
          if Assigned(Param) then
            Result.AddChild(Param);
        end;
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
      Context.Advance;
      HandleNode := FoldFileHandlePostfix(HandleNode);
      Result.AddChild(HandleNode);
    end
    // The PARENTHESISED handle: "Close #(1)" is the same spelling as "Print #(1)" - see
    // ParseFileNumberOperand, which is where the three shapes live for the statements that share it.
    else if Context.Check(ttDelimParOpen) then
      Result.AddChild(FoldFileHandlePostfix(FExpressionParser.ParseExpression(precCall)))
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

  // Special handling for DCLEAR / RESET (close all channels)
  // Syntax: DCLEAR  (FreeBASIC RESET is the same: unbind all file numbers)
  if (CmdName = 'DCLEAR') or (CmdName = 'RESET') then
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
      Context.Advance;
      HandleNode := FoldFileHandlePostfix(HandleNode);
      Result.AddChild(HandleNode);
    end
    // The PARENTHESISED handle: "Close #(1)" is the same spelling as "Print #(1)" - see
    // ParseFileNumberOperand, which is where the three shapes live for the statements that share it.
    else if Context.Check(ttDelimParOpen) then
      Result.AddChild(FoldFileHandlePostfix(FExpressionParser.ParseExpression(precCall)))
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
    'SETENVIRON': Result := TASTNode.Create(antSetenviron, Token);
    'SHELL': Result := TASTNode.Create(antShell, Token);
    'OUT': Result := TASTNode.Create(antOut, Token);
    'SCREENCONTROL': Result := TASTNode.Create(antOut, Token);   // no-op: evaluate and discard the arguments
    'MOVE', 'MV': Result := TASTNode.Create(antMove, Token);
    // FreeBASIC/QB filesystem mutation: KILL deletes a file (= SCRATCH), FILECOPY copies (= COPY),
    // RMDIR removes a directory (new).
    'KILL': Result := TASTNode.Create(antScratch, Token);
    // FILECOPY stamps its name as the node value: the shared antCopy lowering reads it to pick
    // the overwrite default (FreeBASIC FILECOPY always overwrites; v7 COPY does not).
    'FILECOPY': Result := TASTNode.CreateWithValue(antCopy, CmdName, Token);
    'RMDIR', 'RD': Result := TASTNode.Create(antRmdir, Token);
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
      Context.Advance;
      HandleNode := FoldFileHandlePostfix(HandleNode);
      Result.AddChild(HandleNode);
    end
    // The PARENTHESISED handle: "Close #(1)" is the same spelling as "Print #(1)" - see
    // ParseFileNumberOperand, which is where the three shapes live for the statements that share it.
    else if Context.Check(ttDelimParOpen) then
      Result.AddChild(FoldFileHandlePostfix(FExpressionParser.ParseExpression(precCall)))
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
      Context.Advance;
      HandleNode := FoldFileHandlePostfix(HandleNode);
      Result.AddChild(HandleNode);
    end
    // The PARENTHESISED handle: "Close #(1)" is the same spelling as "Print #(1)" - see
    // ParseFileNumberOperand, which is where the three shapes live for the statements that share it.
    else if Context.Check(ttDelimParOpen) then
      Result.AddChild(FoldFileHandlePostfix(FExpressionParser.ParseExpression(precCall)))
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
  CombinedHash: Boolean;   // the second word was the combined 'INPUT#' token (spaceless LINE INPUT#1)
begin
  Tok := Context.CurrentToken;
  Context.Advance;  // LINE
  CombinedHash := (UpperCase(Context.CurrentToken.Value) = kINPUTN);   // 'INPUT#' already carries the '#'
  Context.Advance;  // INPUT or INPUT#
  if (not CombinedHash) and
     not (Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#')) then
  begin
    // Console "LINE INPUT [;] [prompt ;|,] var" — read a whole line into a string variable. Reuses the
    // console string-input path (antInput); v1 shows INPUT's "? " prompt.
    Result := TASTNode.Create(antInput, Tok);
    if Context.Check(ttSeparOutput) then Context.Advance;   // optional leading ';'
    // Optional prompt string followed by ';' or ',' then the variable.
    if Context.Check(ttStringLiteral) and Assigned(Context.PeekNext) and
       ((Context.PeekNext.TokenType = ttSeparOutput) or (Context.PeekNext.TokenType = ttSeparParam)) then
    begin
      Result.AddChild(TASTNode.CreateWithValue(antLiteral, Context.CurrentToken.Value, Context.CurrentToken));
      Context.Advance;   // prompt
      Result.AddChild(TASTNode.CreateWithValue(antSeparator, Context.CurrentToken.Value, Context.CurrentToken));
      Context.Advance;   // ';' or ','
    end;
    P := ParseExpression;   // destination string variable
    if Assigned(P) then Result.AddChild(P);
    DoNodeCreated(Result);
    Exit;
  end;
  Result := TASTNode.Create(antInputFile, Tok);
  Result.Attributes.Values['LINEINPUT'] := '1';
  if not CombinedHash then Context.Advance;  // consume the separate '#' (combined 'INPUT#' already has it)
  P := ParseFileNumberOperand;
  if Assigned(P) then
    Result.AddChild(P)
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

function TPackratParser.ParseGfxLineStatement: TASTNode;
// FreeBASIC graphics "LINE (x1,y1)-(x2,y2)[,color][,B|BF]". Cursor is at the LINE identifier.
// Children: x1, y1, x2, y2 [, color]. The box flag is stored in the SHAPE attribute ('' = line,
// 'B' = box outline, 'BF' = filled box). The leading start-coordinate / STEP / line-style forms are
// deferred (v1 requires the full two-point form). "LINE INPUT" is intercepted before this is reached.
var
  Tok: TLexerToken;
  FlagStr: string;
  IsFlagToken: Boolean;
  TargetNode: TASTNode;
begin
  Tok := Context.CurrentToken;
  Result := TASTNode.Create(antGfxLine, Tok);
  Context.Advance;                                          // LINE
  TargetNode := nil;
  // FreeBASIC image draw target: "LINE img, (x1,y1)-(x2,y2)".
  if (not Context.Check(ttDelimParOpen)) and (not Context.Check(ttOpSub)) and
     (UpperCase(Context.CurrentToken.Value) <> kSTEP) then
  begin
    TargetNode := ParseExpression;                          // image handle
    if Context.Check(ttSeparParam) then Context.Advance;    // ','
  end;
  // FreeBASIC "LINE -(x2,y2)": the start point is omitted, so the line runs from the current graphics
  // point (the last point plotted by LINE/PSET/DRAW). Placeholder x1/y1 = 0; the VM substitutes the last
  // point when the NOSTART flag is set.
  if Context.Check(ttOpSub) then
  begin
    Result.Attributes.Values['NOSTART'] := '1';
    Result.AddChild(TASTNode.CreateWithValue(antLiteral, '0', Tok));   // x1 placeholder
    Result.AddChild(TASTNode.CreateWithValue(antLiteral, '0', Tok));   // y1 placeholder
  end
  else
  begin
    // FreeBASIC STEP: the start point is relative to the current graphics point.
    if UpperCase(Context.CurrentToken.Value) = kSTEP then
    begin
      Result.Attributes.Values['STEP1'] := '1';
      Context.Advance;                                        // STEP
    end;
    if Context.Check(ttDelimParOpen) then Context.Advance;    // '('
    Result.AddChild(ParseExpression);                         // x1
    if Context.Check(ttSeparParam) then Context.Advance;      // ','
    Result.AddChild(ParseExpression);                         // y1
    if Context.Check(ttDelimParClose) then Context.Advance;   // ')'
  end;
  if Context.Check(ttOpSub) then Context.Advance;           // '-'
  // FreeBASIC STEP on the end point: relative to the FIRST point (x1,y1), not the current point.
  if UpperCase(Context.CurrentToken.Value) = kSTEP then
  begin
    Result.Attributes.Values['STEP2'] := '1';
    Context.Advance;                                          // STEP
  end;
  if Context.Check(ttDelimParOpen) then Context.Advance;    // '('
  Result.AddChild(ParseExpression);                         // x2
  if Context.Check(ttSeparParam) then Context.Advance;      // ','
  Result.AddChild(ParseExpression);                         // y2
  if Context.Check(ttDelimParClose) then Context.Advance;   // ')'

  // Optional trailing fields: [,color] [,B|BF] [,style]. FB puts colour first; a lone ",B"/",BF" (colour
  // omitted) is also accepted for convenience. `style` is a 16-bit line-style bitmask (dashed line).
  if Context.Check(ttSeparParam) then
  begin
    Context.Advance;                                        // first ','
    IsFlagToken := Context.Check(ttIdentifier) and
      ((UpperCase(Context.CurrentToken.Value) = 'B') or (UpperCase(Context.CurrentToken.Value) = 'BF'));
    if IsFlagToken then
    begin
      Result.Attributes.Values['SHAPE'] := UpperCase(Context.CurrentToken.Value);
      Context.Advance;
    end
    else if not Context.CheckAny([ttSeparParam, ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
    begin
      Result.AddChild(ParseExpression);                     // color
      Result.Attributes.Values['HASCOLOR'] := '1';
    end;
    // second field: B|BF (only when the first field was the colour, not the lone flag)
    if (not IsFlagToken) and Context.Check(ttSeparParam) then
    begin
      Context.Advance;                                      // second ','
      if Context.Check(ttIdentifier) and
         ((UpperCase(Context.CurrentToken.Value) = 'B') or (UpperCase(Context.CurrentToken.Value) = 'BF')) then
      begin
        Result.Attributes.Values['SHAPE'] := UpperCase(Context.CurrentToken.Value);
        Context.Advance;
      end;
    end;
    // third field: style (a 16-bit bitmask). STYLEIDX records the child index of the style expression.
    if Context.Check(ttSeparParam) then
    begin
      Context.Advance;                                      // ',' before style
      if not Context.CheckAny([ttSeparParam, ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
      begin
        Result.Attributes.Values['STYLEIDX'] := IntToStr(Result.ChildCount);
        Result.AddChild(ParseExpression);                   // style mask
      end;
    end;
  end;
  if Assigned(TargetNode) then   // image draw target appended last (TARGETIDX = its child index)
  begin
    Result.Attributes.Values['TARGETIDX'] := IntToStr(Result.ChildCount);
    Result.AddChild(TargetNode);
  end;
  DoNodeCreated(Result);
end;

function TPackratParser.ParseGfxPutStatement: TASTNode;
// FreeBASIC graphics "PUT (x,y), src [, mode]" — blit an image surface onto the screen at (x,y).
// Cursor is at the PUT identifier. mode is an optional name (PSET/PRESET/TRANS/ALPHA/ADD/AND/OR/XOR);
// stored as the MODE attribute (blit-mode ordinal). The binary "PUT #n,..." form is handled elsewhere.
var
  Tok: TLexerToken;
  ModeStr: string;
  ModeOrd: Integer;
begin
  Tok := Context.CurrentToken;
  Result := TASTNode.Create(antGfxPut, Tok);
  Context.Advance;                                            // PUT
  if Context.Check(ttDelimParOpen) then Context.Advance;      // '('
  Result.AddChild(ParseExpression);                           // x
  if Context.Check(ttSeparParam) then Context.Advance;        // ','
  Result.AddChild(ParseExpression);                           // y
  if Context.Check(ttDelimParClose) then Context.Advance;     // ')'
  if Context.Check(ttSeparParam) then Context.Advance;        // ','
  Result.AddChild(ParseExpression);                           // src image handle
  // ⛔ THE DEFAULT METHOD IS XOR, NOT PSET. It is the one line of the PUT page a reader skims past
  // ("the default method is XOR") and it is measurable: "Put (x,y), img" over a grey background gives
  // the XOR of the two here and in fbc, where this used to copy the source. A program that omits the
  // method - which the manual's own examples do - drew something else entirely.
  ModeOrd := 5;                                               // default: XOR (FreeBASIC's)
  if Context.Check(ttSeparParam) then
  begin
    Context.Advance;                                          // ','
    ModeStr := UpperCase(Context.CurrentToken.Value);
    if ModeStr = 'PSET' then ModeOrd := 0
    // PRESET is the 1's complement of the source, NOT a synonym for PSET: it was folded into PSET
    // here, so "Put ..., PReset" copied the image unnegated.
    else if ModeStr = 'PRESET' then ModeOrd := 8
    else if ModeStr = 'TRANS' then ModeOrd := 1
    else if ModeStr = 'ALPHA' then ModeOrd := 2
    else if ModeStr = 'AND' then ModeOrd := 3
    else if ModeStr = 'OR' then ModeOrd := 4
    else if ModeStr = 'XOR' then ModeOrd := 5
    else if ModeStr = 'ADD' then ModeOrd := 6
    else if ModeStr = 'CUSTOM' then ModeOrd := 7
    else ModeOrd := 0;                                        // unknown -> PSET fallback
    Context.Advance;                                          // mode keyword
    // CUSTOM takes a user FUNCTION and an optional parameter pointer:
    //   Put (x,y), src, Custom, fn [, param]
    // The function is called once per pixel with (source_pixel, destination_pixel, parameter) and its
    // return value is what gets drawn. Both are kept as ordinary expression children so the SSA can
    // build the per-pixel loop out of them; without parsing them here the blit had no function to call
    // and the mode silently degraded to PSET.
    if ModeOrd = 7 then
    begin
      if Context.Check(ttSeparParam) then Context.Advance;    // ','
      Result.AddChild(ParseExpression);                       // child 3: function pointer
      if Context.Check(ttSeparParam) then
      begin
        Context.Advance;                                      // ','
        Result.AddChild(ParseExpression);                     // child 4: parameter (optional)
      end;
    end
    // ALPHA and ADD take a 0..255 blend value: "Put (x,y), img, Alpha, 128". It was never parsed, so
    // the value was left dangling as an unattached node (the SSA logged "Unhandled node type 0") and
    // the blit ran without it. ⭐ For ALPHA the ABSENCE of a value is not a default - it selects a
    // different formula, the image's own per-pixel alpha - so it is recorded as HASVALUE rather than
    // filled in with 255 here.
    else if ((ModeOrd = 2) or (ModeOrd = 6)) and Context.Check(ttSeparParam) then
    begin
      Context.Advance;                                        // ','
      Result.AddChild(ParseExpression);                       // child 3: the blend value
      Result.Attributes.Values['HASVALUE'] := '1';
    end;
  end;
  Result.Attributes.Values['MODE'] := IntToStr(ModeOrd);
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
  P := ParseFileNumberOperand;
  if Assigned(P) then
    Result.AddChild(P)
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

function TPackratParser.ParseBinaryFileTail(IsGet: Boolean; const Tok: TLexerToken): TASTNode;
// FreeBASIC binary GET/PUT tail: "#n [, [pos] , var]". Cursor is at '#'. Builds antGetFile tagged BIN
// (GET) or antPrintFile tagged PUTBIN (PUT): child0=handle, child1=var, optional child2=pos (HASPOS).
var
  H, V, P: TASTNode;
begin
  if IsGet then
  begin
    Result := TASTNode.Create(antGetFile, Tok);
    Result.Attributes.Values['BIN'] := '1';
  end
  else
  begin
    Result := TASTNode.Create(antPrintFile, Tok);
    Result.Attributes.Values['PUTBIN'] := '1';
  end;
  if Context.Check(ttFileHandlePrefix) or (Context.CurrentToken.Value = '#') then
    Context.Advance;  // '#'
  H := ParseFileNumberOperand;
  if not Assigned(H) then
  begin
    HandleError('Expected file number after GET/PUT #', Tok);
    H := TASTNode.CreateWithValue(antLiteral, 1, Tok);
  end;
  Result.AddChild(H);   // child 0 = handle
  P := nil;
  if Context.Check(ttSeparParam) then
  begin
    Context.Advance;  // comma after the handle
    if not Context.Check(ttSeparParam) then
      P := ParseExpression;   // optional position (empty in "GET #1, , var")
    if Context.Check(ttSeparParam) then Context.Advance;  // comma before the variable
  end;
  V := ParseExpression;   // destination (GET) / source value (PUT)
  if Assigned(V) then Result.AddChild(V);   // child 1 = var
  if Assigned(P) then
  begin
    Result.Attributes.Values['HASPOS'] := '1';
    Result.AddChild(P);   // child 2 = position
  end;
  // Optional COUNT ("Get #f, , *p, 5"): consumed and attached, so it can never leak into
  // statement position as a stray literal ("[SSA] WARNING: Unhandled node type 0"). The
  // counted-transfer SEMANTICS (N elements into raw memory / whole arrays) is the pending
  // Random-Access family work; until then the count is evaluated and its value unused.
  if Context.Check(ttSeparParam) then
  begin
    Context.Advance;
    P := ParseExpression;
    if Assigned(P) then
    begin
      Result.Attributes.Values['HASCOUNT'] := '1';
      Result.AddChild(P);   // last child = count
    end;
  end;
  DoNodeCreated(Result);
end;

function TPackratParser.ParseWriteConsole: TASTNode;
// FreeBASIC "WRITE v1, v2, ..." — quoted-CSV output to the screen. Built as an antPrint node tagged
// WRITECSV with a placeholder child 0 (so the shared CSV emitter, which skips child 0, sees values at 1+).
var
  P: TASTNode;
  Tok: TLexerToken;
begin
  Tok := Context.CurrentToken;
  Context.Advance;  // WRITE
  Result := TASTNode.Create(antPrint, Tok);
  Result.Attributes.Values['WRITECSV'] := '1';
  Result.AddChild(TASTNode.CreateWithValue(antLiteral, 0, Tok));   // child 0 placeholder
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
  P := ParseFileNumberOperand;
  if Assigned(P) then
    Result.AddChild(P)
  else
    HandleError('Expected file number after SEEK #', Tok);
  if Context.CheckAny([ttSeparParam, ttSeparOutput]) then Context.Advance;  // comma
  P := ParseExpression;   // position (1-based)
  if Assigned(P) then Result.AddChild(P);
  DoNodeCreated(Result);
end;

function TPackratParser.LooksLikeImageTarget: Boolean;
// At a graphics command token (PSET/LINE/CIRCLE/PAINT): does the FreeBASIC "cmd img, (x,y)..." image
// draw-target form follow? Heuristic for a single-token target (image handle): the next token is not '('
// or STEP, the one after is ',', and the one after that is '(' — which cleanly separates the target form
// from the C128 forms ("CIRCLE src,x,y") and the plain "(x,y)" / "STEP(x,y)" screen forms. A multi-token
// target expression in this position is not recognised (rare; image handles are simple variables).
var
  T1, T2, T3: TLexerToken;
begin
  T1 := Context.PeekToken(1);
  T2 := Context.PeekToken(2);
  T3 := Context.PeekToken(3);
  Result := Assigned(T1) and Assigned(T2) and Assigned(T3) and
            (T1.TokenType <> ttDelimParOpen) and (UpperCase(T1.Value) <> kSTEP) and
            (T2.TokenType = ttSeparParam) and (T3.TokenType = ttDelimParOpen);
end;

function TPackratParser.PeekNameHasAs: Boolean;
// True when the statement starting at NAME is the FreeBASIC "NAME old AS new" rename form: an AS
// (ttAsType) appears before end-of-statement, and NAME is not immediately followed by '=' / '.' /
// '(' / '[' (which would make it an assignment, member, array or call to a variable named NAME).
var
  SavedIndex: Integer;
begin
  Result := False;
  Context.SavePosition(SavedIndex);
  try
    Context.Advance;  // skip NAME
    if Context.CheckAny([ttOpEq, ttOpDot, ttDelimParOpen, ttDelimBrackOpen]) then
      Exit;
    while not Context.CheckAny([ttEndOfLine, ttEndOfFile, ttSeparStmt]) do
    begin
      if Context.Check(ttAsType) then
      begin
        Result := True;
        Exit;
      end;
      Context.Advance;
    end;
  finally
    Context.RestorePosition(SavedIndex);
  end;
end;

function TPackratParser.ParseNameStatement: TASTNode;
// FreeBASIC/QB "NAME old AS new" -> antRenameFile (child0 = old path, child1 = new path),
// reusing the existing RENAME lowering (ssaRenameFile).
var
  Tok: TLexerToken;
  OldExpr, NewExpr: TASTNode;
begin
  Tok := Context.CurrentToken;
  Context.Advance;  // consume NAME
  Result := TASTNode.Create(antRenameFile, Tok);

  OldExpr := ParseExpression;   // old path (stops before AS)
  if Assigned(OldExpr) then Result.AddChild(OldExpr);

  if Context.Check(ttAsType) then
    Context.Advance   // consume AS
  else
    HandleError('Expected AS in NAME statement', Context.CurrentToken);

  NewExpr := ParseExpression;   // new path
  if Assigned(NewExpr) then Result.AddChild(NewExpr);

  DoNodeCreated(Result);
end;

function TPackratParser.ParseRaiseErrorStatement: TASTNode;
// FreeBASIC/QB "ERROR <n>" -> antErrorStmt (child0 = error number expression).
var
  Tok: TLexerToken;
  NumExpr: TASTNode;
begin
  Tok := Context.CurrentToken;
  Context.Advance;  // consume ERROR
  Result := TASTNode.Create(antErrorStmt, Tok);
  NumExpr := ParseExpression;
  if Assigned(NumExpr) then Result.AddChild(NumExpr);
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
      Context.Advance;
      HandleNode := FoldFileHandlePostfix(HandleNode);
      Result.AddChild(HandleNode);
    end
    // The PARENTHESISED handle: "Close #(1)" is the same spelling as "Print #(1)" - see
    // ParseFileNumberOperand, which is where the three shapes live for the statements that share it.
    else if Context.Check(ttDelimParOpen) then
      Result.AddChild(FoldFileHandlePostfix(FExpressionParser.ParseExpression(precCall)))
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
    // Check for line number (RESUME <line>) — RESUME 0 means resume at the faulting statement
    else if Context.Check(ttNumber) then
    begin
      Result := TASTNode.Create(antResume, Token);
      LineNumNode := ParseExpression;
      if Assigned(LineNumNode) then
        Result.AddChild(LineNumNode);
    end
    // FreeBASIC: RESUME <label> — resume at a named label (MODERN, no line numbers)
    else if Context.Check(ttIdentifier) then
    begin
      Result := TASTNode.Create(antResume, Token);
      LineNumNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
      Context.Advance; // consume label
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
  Token, T: TLexerToken;
  CmdName, ExprText: string;
  HasParen: Boolean;
  StartIdx, EndIdx, i, PrevLine, PrevEnd: Integer;
  Cond: TASTNode;
begin
  Token := Context.CurrentToken;
  CmdName := UpperCase(Token.Value);

  // FreeBASIC ASSERT(expr) / ASSERTWARN(expr): if expr is false, print a diagnostic (and, for ASSERT,
  // halt). The expression's source text is captured (by joining its tokens) for the message, mirroring
  // the FB `#expression` stringize. Unlike FB, our build always generates the check (no -g gate).
  if (CmdName = kASSERT) or (CmdName = kASSERTWARN) then
  begin
    Context.Advance; // consume ASSERT / ASSERTWARN
    HasParen := (Context.CurrentToken.Value = '(');
    if HasParen then Context.Advance;
    StartIdx := Context.CurrentIndex;
    Cond := ParseExpression;
    EndIdx := Context.CurrentIndex;
    // Reconstruct the expression's SOURCE spacing from the token columns: fbc's diagnostic
    // prints the text as written ("a=1", not "a = 1"), and the assert examples are compared
    // byte for byte against it. Tokens on a later line (continuations) join with one space.
    ExprText := '';
    PrevLine := -1;
    PrevEnd := -1;
    for i := StartIdx to EndIdx - 1 do
    begin
      T := Context.TokenList.GetTokenDirect(i);
      if not Assigned(T) then Continue;
      if ExprText <> '' then
      begin
        if (T.Line = PrevLine) and (T.Column > PrevEnd) then
          ExprText := ExprText + StringOfChar(' ', T.Column - PrevEnd)
        else if T.Line <> PrevLine then
          ExprText := ExprText + ' ';
      end;
      ExprText := ExprText + T.Value;
      PrevLine := T.Line;
      PrevEnd := T.Column + Length(T.Value);
    end;
    if HasParen and (Context.CurrentToken.Value = ')') then Context.Advance;
    if CmdName = kASSERT then
      Result := TASTNode.CreateWithValue(antAssert, ExprText, Token)
    else
      Result := TASTNode.CreateWithValue(antAssertWarn, ExprText, Token);
    if Assigned(Cond) then Result.AddChild(Cond);
    DoNodeCreated(Result);
    Exit;
  end;

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

  // Parse and DISCARD the address: sb cannot execute 6502 machine language, so SYS is a
  // deliberate no-op. The address must not stay attached as a child - antStatement is a
  // transparent wrapper whose children are processed as STATEMENTS, so a kept address
  // expression leaked into statement position ("[SSA] WARNING: Unhandled node type 0").
  if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) then
  begin
    Address := ParseExpression;
    Address.Free;
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
    // OPTION BASE n: set the default lower bound (0 or 1) for arrays declared with a bare upper bound.
    if Assigned(Context.CurrentToken) and
       ((Context.CurrentToken.TokenType = ttIdentifier) or Assigned(Context.CurrentToken.KeywordInfo)) and
       (UpperCase(Context.CurrentToken.Value) = 'BASE') then
    begin
      Context.Advance;                                 // consume BASE
      if Assigned(Context.CurrentToken) and (Context.CurrentToken.TokenType = ttNumber) then
      begin
        if Trim(Context.CurrentToken.Value) = '1' then FOptionBase := 1 else FOptionBase := 0;
        Context.Advance;                               // consume the base value
      end;
    end
    // OPTION DIGITS n: how many significant digits PRINT shows for a float.
    // ⭐ The COUNT is a display choice; the ROUNDING is not - the digits come
    // from the exact binary value and are correctly rounded at every setting
    // (IEEE 754-2019 sec.5.12.2), so raising this shows more of the same number
    // rather than a differently-rounded one. 17 makes every distinct double
    // print distinctly; beyond that the extra digits are the true ones, because
    // a double's exact expansion terminates. See job/docs/PIANO_FLOAT_PRINT.md.
    else if Assigned(Context.CurrentToken) and
            ((Context.CurrentToken.TokenType = ttIdentifier) or Assigned(Context.CurrentToken.KeywordInfo)) and
            (UpperCase(Context.CurrentToken.Value) = 'DIGITS') then
    begin
      Context.Advance;                                 // consume DIGITS
      if Assigned(Context.CurrentToken) and (Context.CurrentToken.TokenType = ttNumber) then
      begin
        FOptionDigits := StrToIntDef(Trim(Context.CurrentToken.Value), 0);
        Context.Advance;                               // consume the digit count
      end
      // ⭐ "OPTION DIGITS EXACT" (or ALL): every digit the value HAS.
      // It is not "a very large number" - a double's decimal expansion is
      // FINITE. The value is M x 2^E, so for E >= 0 it is an integer and for
      // E < 0 it is M x 5^(-E) / 10^(-E), which terminates after exactly -E
      // fractional digits. The widest any double gets is 751 significant digits
      // (the smallest subnormal, 2^-1074), and the console behavior caps at 767,
      // so asking for "all" cannot truncate anything: there is nothing past the
      // end of a terminating expansion. MaxInt here says "as many as exist" and
      // the cap turns it into that number, rather than making every program
      // that wants the exact value hardcode 767.
      else if Assigned(Context.CurrentToken) and
              ((Context.CurrentToken.TokenType = ttIdentifier) or Assigned(Context.CurrentToken.KeywordInfo)) and
              ((UpperCase(Context.CurrentToken.Value) = 'EXACT') or
               (UpperCase(Context.CurrentToken.Value) = 'ALL')) then
      begin
        FOptionDigits := MaxInt;
        Context.Advance;
      end;
    end
    // Every other OPTION is a compiler switch we accept and do not act on: DYNAMIC / STATIC (default
    // array storage - we allow REDIM either way), GOSUB (enables GOSUB in fblite, which we always
    // support), BYVAL / BYREF (the fblite default passing convention), EXPLICIT, PRIVATE, ESCAPE.
    // The option NAME is usually a reserved word, not an identifier - "Option Static" is STATIC, "Option
    // GoSub" is GOSUB - so testing for an identifier left the keyword in the stream to be parsed as a
    // statement of its own, and the program died on it.
    else if Assigned(Context.CurrentToken) and
            ((Context.CurrentToken.TokenType = ttIdentifier) or Assigned(Context.CurrentToken.KeywordInfo)) then
    begin
      // OPTION NOKEYWORD <word> takes a second one: the keyword being removed from the symbol table.
      // Consumed so the statement parses; the word itself stays reserved (un-reserving it would have to
      // reach back into the LEXER, which has already classified every later occurrence).
      if UpperCase(Context.CurrentToken.Value) = 'NOKEYWORD' then
      begin
        Context.Advance;
        if Assigned(Context.CurrentToken) and
           ((Context.CurrentToken.TokenType = ttIdentifier) or Assigned(Context.CurrentToken.KeywordInfo)) then
          Context.Advance;
      end
      else
        Context.Advance;
    end;
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

function JoinIntCsv(const A: array of Integer): string;
// Comma-join an integer array ("4,4"), for stashing per-level initializer sizes on an AST attribute.
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(A) do
  begin
    if i > 0 then Result := Result + ',';
    Result := Result + IntToStr(A[i]);
  end;
end;

function TPackratParser.ParseArrayDeclaration: TASTNode;
var
  ElemTypeName: string;
  VarName: TASTNode;
  Dimensions: TASTNode;
  Token, TypeTok: TLexerToken;
  MemberNode: TASTNode;
begin
  Token := Context.CurrentToken;

  // "REDIM (<array expression>)(dims)" - the target in PARENTHESES. FreeBASIC's own manual prescribes
  // this spelling, and says why: "Redim u(0).array(0 To 9)" is ambiguous (fbc reads it as redimming u
  // and reports "Duplicated definition"), so the array expression is wrapped to say where it ends.
  // The name-then-dots walk below cannot express it - the object is an array ELEMENT, not a plain name -
  // so the whole declaration failed with "Expected variable name in array declaration". Reading and
  // writing "u(0).array(i)" already worked; only the REDIM target had no route to it.
  if Context.Check(ttDelimParOpen) then
  begin
    Context.Advance;                                  // (
    VarName := FExpressionParser.ParseExpression;
    if not Assigned(VarName) then
    begin
      HandleError('Expected an array expression after "(" in REDIM', Context.CurrentToken);
      Result := nil; Exit;
    end;
    if not Context.Match(ttDelimParClose) then
    begin
      HandleError('Expected ")" after the array expression in REDIM', Context.CurrentToken);
      VarName.Free; Result := nil; Exit;
    end;
  end
  // ⭐ "Redim .field( ... )" INSIDE A WITH BLOCK: the leading dot names a member of the WITH object,
  // exactly as it does in an assignment or a read. Only a NAME was accepted here, so the dot ended the
  // statement and the declaration failed with "Expected variable name in array declaration" - a message
  // about the very thing the WITH block is there to leave out. The expression parser's own leading-dot
  // rule builds the member access, which is the same node the "obj.field" walk below produces.
  else if Context.Check(ttOpDot) then
  begin
    // ⛔ At precPRIMARY, not precCall: the dimensions that follow are REDIM's own "( 0 To 2 )" and must
    // stay for the dimension parser. Read at call precedence they were swallowed as an array INDEX, and
    // the "To" inside then failed as "Expected ")" after array indices".
    VarName := FExpressionParser.ParseExpression(precPrimary);
    if not Assigned(VarName) then
    begin
      HandleError('Expected a member name after "." in REDIM', Context.CurrentToken);
      Result := nil; Exit;
    end;
  end
  else
  begin
  // Parse variable name
  // ⭐ ...and a MODERN extension FreeBASIC does not reserve may be a VARIABLE'S name too. The same
  // door the PROCEDURE and CONST declarations have; without it "Dim Round As Integer" refused.
  if FModernMode and (not Context.Check(ttIdentifier)) and
     IsShadowableExtensionName(UpperCase(VarToStr(Context.CurrentToken.Value))) then
    Context.CurrentToken.TokenType := ttIdentifier;
  if not Context.Check(ttIdentifier) then
  begin
    HandleError('Expected variable name in array declaration', Token);
    Result := nil;
    Exit;
  end;

  VarName := TASTNode.CreateWithValue(antIdentifier, UpperCase(Token.Value), Token);
  Context.Advance;

  // Member array target "obj.field(...)" (REDIM of a UDT array member, e.g. "Redim this.m(x-1,y-1)"):
  // fold the "." chain into an antMemberAccess so the SSA lowering resolves the field's array handle.
  while Context.Check(ttOpDot) do
  begin
    Context.Advance;                                  // '.'
    if not (Context.Check(ttIdentifier) or
            ((Length(Context.CurrentToken.Value) > 0) and
             (UpCase(Context.CurrentToken.Value[1]) in ['A'..'Z', '_']))) then
    begin
      HandleError('Expected field name after "." in REDIM target', Context.CurrentToken);
      VarName.Free; Result := nil; Exit;
    end;
    MemberNode := TASTNode.CreateWithValue(antMemberAccess, UpperCase(Context.CurrentToken.Value),
                                           Context.CurrentToken);
    MemberNode.AddChild(VarName);
    VarName := MemberNode;
    Context.Advance;                                  // field name
  end;
  end;   // end of the unparenthesised "name[.field...]" target

  // Expect opening parenthesis
  if not Context.Match(ttDelimParOpen) then
  begin
    HandleError('Expected "(" after array name', Context.CurrentToken);
    VarName.Free;
    Result := nil;
    Exit;
  end;

  // FreeBASIC variable-length array with an empty subscript list: "DIM x()" declares a resizable array
  // that starts empty (UBOUND = -1) and is sized later with REDIM. Accept the empty "()" here as a
  // dimension list with zero children (marked VARLEN); ProcessDim allocates a 0-element array.
  if Context.Check(ttDelimParClose) then
  begin
    Context.Advance;                                  // )
    Dimensions := TASTNode.Create(antDimensions);
    Result := TASTNode.Create(antArrayDecl, Token);
    Result.AddChild(VarName);
    Result.AddChild(Dimensions);
    Result.Attributes.Values['VARLEN'] := '1';
  end
  else
  begin
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
  end;

  // Optional "AS typename" (M3.1): array of UDT (or explicitly-typed array). Attached as a
  // 3rd child antIdentifier(typename); SSA treats a UDT element type as an int handle array.
  if Context.Check(ttAsType) then
  begin
    Context.Advance;                                // AS
    SkipTypeQualifiers;                     // FB: "As Const <type>"
    if Context.Check(ttIdentifier) then
    begin
      TypeTok := Context.CurrentToken;
      ElemTypeName := ParseDottedName;                // dotted: namespace-qualified element type
      // ⛔ THE POINTER SUFFIX. "Dim a(0 To 3) As ZString Ptr" is an array of POINTERS, and this was the
      // one declaration shape with no "PTR" loop: the element type came out "ZSTRING", so the elements
      // were allocated in the STRING bank and the address stored into one was lost - "*a(i)" then read
      // a null. Worse, the leftover "Ptr" was parsed as a STATEMENT of its own (a bare call to a
      // procedure named PTR), so nothing complained. Every other declaration form has had this loop for
      // a long time; this one is where a table of C strings is declared.
      while AtPointerSuffix do
      begin
        ElemTypeName := ElemTypeName + ' PTR';
        Context.Advance;                              // consume PTR
      end;
      Result.AddChild(TASTNode.CreateWithValue(antIdentifier, ElemTypeName, TypeTok));
      // FreeBASIC fixed-length string array: "AS STRING * n" / "AS WSTRING * n" (advisory in v1).
      if Context.Check(ttOpMul) then
      begin
        Context.Advance;                              // '*'
        FExpressionParser.ParseExpression(precTerm).Free;   // length operand (discarded); an EXPRESSION
      end;
    end;
  end;

  ParseOptionalArrayInit(Result, Dimensions, Token);

  DoNodeCreated(Result);
end;

function TPackratParser.TryParseAggregateTuple(const DimTypeName: string): TASTNode;
// FreeBASIC aggregate init "= (a, b, c)": a parenthesised comma-tuple that sets a UDT's fields in
// declaration order. Answers the antArgumentList (TUPLEINIT), or NIL with the stream left exactly where
// it was - so the caller can fall through to an ordinary expression, which is what "= (x + y) \ 2" is.
//
// ⛔ EXTRACTED because DIM had it and STATIC did not: "Static As T v = (a, b)" is the same declaration
// with the other modifier, and it parsed the parentheses as an expression and failed. One grammar in
// one place is the only way the two spellings cannot drift apart again.
// The current token must be the '('.
var
  SavedIdx, TupleDepth: Integer;
  IsTuple, HadComma: Boolean;
  CtorArgs, ArgExpr: TASTNode;
begin
  Result := nil;
  if not Context.Check(ttDelimParOpen) then Exit;
  SavedIdx := Context.CurrentIndex;
  Context.Advance;           // step past '(' for the scan
  TupleDepth := 1; IsTuple := False; HadComma := False;
  while (TupleDepth > 0) and (not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile])) do
  begin
    if Context.Check(ttDelimParOpen) then Inc(TupleDepth)
    else if Context.Check(ttDelimParClose) then
    begin
      Dec(TupleDepth);
      // ...and a SINGLE-element group is an initializer list too when the declared type is a UDT
      // and the parentheses span the WHOLE initializer: "Dim As UDT1 u = (1)" sets the first
      // field. Requiring a comma made that one a parenthesised EXPRESSION, and a scalar stored
      // into a record variable left the record's handle showing (the manual's control/iif4).
      // The whole-initializer test keeps "= (x + y) \ 2" an expression, comma or not.
      // ⛔ ...and a POINTER type is never an aggregate: it has no fields to fill, so parentheses around
      // its initializer are plain grouping. "Dim As ZString Ptr r = (StrPtr(s) + 1)" was read as a
      // one-field tuple and the pointer arithmetic was stored as if it were a field value - the program
      // then died dereferencing a null. IsBuiltinTypeName only matches BARE names, so "ZSTRING PTR"
      // answered False and looked like a UDT.
      if (TupleDepth = 0) and (not IsBuiltinTypeName(DimTypeName)) and
         (Pos(' PTR', UpperCase(DimTypeName)) = 0) then
      begin
        Context.Advance;
        if Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttSeparParam]) then IsTuple := True;
        Break;
      end;
    end
    else if Context.Check(ttSeparParam) and (TupleDepth = 1) then
      begin IsTuple := True; HadComma := True; Break; end;
    Context.Advance;
  end;
  Context.CurrentIndex := SavedIdx;  // rewind to the '('
  if IsTuple then
  begin
    Context.Advance;         // (
    CtorArgs := TASTNode.Create(antArgumentList, Context.CurrentToken);
    CtorArgs.Attributes.Values['TUPLEINIT'] := '1';   // UDT aggregate field init
    if not HadComma then
      // A SINGLE-element group is ambiguous: "= (1)" is a field list, but "= (""A.x"")" on a type
      // with a matching CONSTRUCTOR is a construction. Mark it so the SSA resolves a constructor
      // first and only aggregates when none matches.
      CtorArgs.Attributes.Values['TUPLE1'] := '1';
    repeat
      // ⭐ A TUPLE ELEMENT MAY BE A BRACE LIST, and it initialises an ARRAY MEMBER:
      //     type foo_2 : bar(0 to 1) as integer : end type
      //     static as foo_2 chkref2 = ( { 1234, -5678 } )
      // is FreeBASIC's own spelling (its test suite writes it), and the braces reached an
      // expression parser that has no rule for '{' - the whole declaration failed to parse.
      // Parsed by the same brace reader the array initializer uses, marked so the SSA knows
      // this element is a LIST for a member array and not a value for a scalar field.
      if Context.Check(ttDelimBraceOpen) then
      begin
        ArgExpr := TASTNode.Create(antArgumentList, Context.CurrentToken);
        ArgExpr.Attributes.Values['BRACEINIT'] := '1';
        SetLength(FInitLevelSizes, 0);
        ParseArrayInitBraceGroup(ArgExpr, ConstDimSizes(nil), 0);   // no shape: plain row-major
      end
      else
        ArgExpr := FExpressionParser.ParseExpression;
      if not Assigned(ArgExpr) then Break;
      CtorArgs.AddChild(ArgExpr);
      if Context.Check(ttSeparParam) then Context.Advance else Break;
    until Context.CheckAny([ttDelimParClose, ttEndOfLine, ttEndOfFile]);
    if Context.Check(ttDelimParClose) then Context.Advance;   // )
    Result := CtorArgs;
  end;
end;

procedure TPackratParser.ParseOptionalArrayInit(Decl, Dimensions: TASTNode; const Tok: TLexerToken);
// FreeBASIC array initializer: "DIM arr(dims) AS type = { v0, v1, ... }" or "=> { ... }". Both '=' and
// '=>' are valid initializer signs (FB manual: plain '=' is the common form, '=>' avoids the declaration
// resembling an expression); "=>" is lexed as '=' then '>'. Parse the brace value list into an
// antArgumentList child (marked ARRAYINIT); the SSA stores each value into the corresponding element
// after allocating the array. Shared by DIM/REDIM-style array declarations and STATIC arrays.
var
  InitList: TASTNode;
begin
  // "= Any" / "=> Any": FreeBASIC's way of saying DO NOT INITIALISE this array. It is an initializer
  // sign like the others, and it was not accepted here at all - so the '=' was left standing and the
  // whole declaration failed to parse ("Unexpected token in statement"). The scalar form has always
  // gone through (it lands on the ordinary initializer path, where the bare name ANY reads as an
  // undeclared identifier); only the array spelling had no route.
  // ⚠️ The storage still comes out ZEROED here, where fbc hands back whatever was on the stack. That
  // is a defined state instead of an undefined one, and it is declared in BASIC.md.
  if Context.Check(ttOpEq) then
  begin
    if Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttIdentifier) and
       (UpperCase(VarToStr(Context.PeekNext.Value)) = 'ANY') then
    begin
      Context.Advance;                              // =
      Context.Advance;                              // Any
      Decl.Attributes.Values['ANYINIT'] := '1';
      Exit;
    end;
    // the "=> Any" spelling of the same thing ("=>" is lexed as '=' then '>')
    if Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttOpGt) and
       Assigned(Context.PeekToken(2)) and (Context.PeekToken(2).TokenType = ttIdentifier) and
       (UpperCase(VarToStr(Context.PeekToken(2).Value)) = 'ANY') then
    begin
      Context.Advance; Context.Advance; Context.Advance;   // = > Any
      Decl.Attributes.Values['ANYINIT'] := '1';
      Exit;
    end;
  end;
  if not (Context.Check(ttOpEq) and Assigned(Context.PeekNext) and
          ((Context.PeekNext.TokenType = ttOpGt) or (Context.PeekNext.TokenType = ttDelimBraceOpen))) then
    Exit;
  Context.Advance;                                  // =
  if Context.Check(ttOpGt) then Context.Advance;    // optional '>' (=> form)
  if not Context.Check(ttDelimBraceOpen) then Exit;
  InitList := TASTNode.Create(antArgumentList, Tok);
  // flattens any nested {..} row-major, zero-padding short rows when the dimensions are all constant
  SetLength(FInitLevelSizes, 0);
  ParseArrayInitBraceGroup(InitList, ConstDimSizes(Dimensions), 0);
  Decl.Attributes.Values['ARRAYINIT'] := '1';
  // Per-level item counts, so a "n TO ..." ellipsis dimension deduces its size from the matching
  // brace-nesting level (level 0 = dim 0, level 1 = dim 1, ...) rather than the flat element total.
  InitList.Attributes.Values['LEVELSIZES'] := JoinIntCsv(FInitLevelSizes);
  Decl.AddChild(InitList);                          // initializer values (antArgumentList)
end;

function TPackratParser.ConstDimSizes(DimsNode: TASTNode): TDimSizeArray;
// Return the constant element count of each dimension (ub-lb+1). Return an empty array if any bound is
// not a compile-time integer literal or the dimension is an ellipsis/variable-length placeholder — the
// caller then falls back to plain row-major flattening with no padding.
var
  i, lb, ub: Integer;
  Dim: TASTNode;
  function LitInt(N: TASTNode; out V: Integer): Boolean;
  begin
    Result := (N <> nil) and (N.NodeType = antLiteral) and TryStrToInt(Trim(VarToStr(N.Value)), V);
  end;
begin
  SetLength(Result, 0);
  if DimsNode = nil then Exit;
  SetLength(Result, DimsNode.ChildCount);
  for i := 0 to DimsNode.ChildCount - 1 do
  begin
    Dim := DimsNode.GetChild(i);
    if Dim.NodeType = antDimRange then
    begin
      if (Dim.Attributes.Values['ELLIPSIS'] = '1') or
         (not LitInt(Dim.GetChild(0), lb)) or (not LitInt(Dim.GetChild(1), ub)) then
      begin SetLength(Result, 0); Exit; end;
    end
    else if LitInt(Dim, ub) then
      lb := 0
    else
    begin SetLength(Result, 0); Exit; end;
    if ub < lb then begin SetLength(Result, 0); Exit; end;
    Result[i] := ub - lb + 1;
  end;
end;

procedure TPackratParser.ParseArrayInitBraceGroup(InitList: TASTNode; const DimSizes: array of Integer; Level: Integer);
// Parse a "{ ... }" array-initializer group, appending each LEAF expression to InitList in textual
// (row-major) order. A nested "{ ... }" — a multi-dimensional initializer such as "= {{1,2},{3,4}}"
// (FreeBASIC) — is flattened recursively: our arrays store row-major, so nested braces collapse to a
// flat element sequence that ProcessDim then fills element-by-element. Arbitrary nesting depth works.
// When DimSizes is known, a short NESTED group (a row/plane, Level>=1) is zero-padded to its stride
// (product of the inner dimension sizes) so following rows stay aligned — FB zero-fills short rows.
// A parenthesised comma-tuple element "(a, b, c)" is a UDT aggregate for an array-of-UDT element (e.g.
// "Dim it(1 To 2) As T = {(""x"", 9), (""y"", 3)}"); it becomes an antArgumentList tagged TUPLEINIT and
// ProcessDim aggregate-initialises the element from it. A single "(expr)" stays a normal expression.
var
  ValExpr, TupleNode, PadNode: TASTNode;
  SavedIdx, TupleDepth, StartCount, Stride, d, ItemCount: Integer;
  IsTuple: Boolean;
begin
  if not Context.Check(ttDelimBraceOpen) then Exit;
  StartCount := InitList.ChildCount;
  Context.Advance;                                    // {
  // Count the DIRECT items of this group (nested groups OR leaves), recording the largest count seen at
  // this nesting level. For an ellipsis dimension "n TO ...", the level's item count is the deduced size:
  // level 0 = outer group's items (dim 0), level 1 = a row's items (dim 1), etc. Uniform (rectangular)
  // initializers give the same count at every group of a level; a jagged one keeps the widest.
  ItemCount := 0;
  if not Context.Check(ttDelimBraceClose) then
    repeat
      Inc(ItemCount);
      if Context.Check(ttDelimBraceOpen) then
        ParseArrayInitBraceGroup(InitList, DimSizes, Level + 1)  // nested row/plane -> flatten in place
      else if Context.Check(ttDelimParOpen) then
      begin
        // Look ahead for a TOP-LEVEL comma inside the parentheses -> a UDT aggregate tuple.
        SavedIdx := Context.CurrentIndex;
        Context.Advance;                              // step past '(' for the scan
        TupleDepth := 1; IsTuple := False;
        while (TupleDepth > 0) and (not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile])) do
        begin
          if Context.Check(ttDelimParOpen) then Inc(TupleDepth)
          else if Context.Check(ttDelimParClose) then Dec(TupleDepth)
          else if Context.Check(ttSeparParam) and (TupleDepth = 1) then begin IsTuple := True; Break; end;
          Context.Advance;
        end;
        Context.CurrentIndex := SavedIdx;             // rewind to '('
        if IsTuple then
        begin
          Context.Advance;                            // (
          TupleNode := TASTNode.Create(antArgumentList, Context.CurrentToken);
          TupleNode.Attributes.Values['TUPLEINIT'] := '1';
          repeat
            ValExpr := FExpressionParser.ParseExpression;
            if not Assigned(ValExpr) then Break;
            TupleNode.AddChild(ValExpr);
            if Context.Check(ttSeparParam) then Context.Advance else Break;
          until Context.CheckAny([ttDelimParClose, ttEndOfLine, ttEndOfFile]);
          if Context.Check(ttDelimParClose) then Context.Advance;   // )
          InitList.AddChild(TupleNode);
        end
        else
        begin
          ValExpr := FExpressionParser.ParseExpression;   // "(expr)" — an ordinary parenthesised value
          if not Assigned(ValExpr) then Break;
          InitList.AddChild(ValExpr);
        end;
      end
      else
      begin
        ValExpr := FExpressionParser.ParseExpression;
        if not Assigned(ValExpr) then Break;
        InitList.AddChild(ValExpr);
      end;
      if Context.Check(ttSeparParam) then Context.Advance else Break;
    until Context.CheckAny([ttDelimBraceClose, ttEndOfLine, ttEndOfFile, ttSeparStmt]);
  if Context.Check(ttDelimBraceClose) then Context.Advance;   // }

  // Record this level's item count (widest group wins) for ellipsis-dimension size deduction.
  if Level > High(FInitLevelSizes) then
    SetLength(FInitLevelSizes, Level + 1);
  if ItemCount > FInitLevelSizes[Level] then
    FInitLevelSizes[Level] := ItemCount;

  // Zero-pad a short NESTED group (a row/plane) to its stride so a jagged multi-dim initializer stays
  // row-aligned: FB fills each nested brace into one slot of its dimension and zero-fills the remainder.
  // Only applied to inner groups (Level>=1) with known constant dimensions; the outer group's trailing
  // shortfall is left to the array's default zero-init (avoids materialising a huge tail for big arrays).
  if (Level >= 1) and (Level <= High(DimSizes)) then
  begin
    Stride := 1;
    for d := Level to High(DimSizes) do Stride := Stride * DimSizes[d];
    if (Stride > 0) and (Stride <= 65536) then
      while InitList.ChildCount - StartCount < Stride do
      begin
        PadNode := TASTNode.CreateWithValue(antLiteral, '0', Context.CurrentToken);
        PadNode.Attributes.Values['ARRPAD'] := '1';   // SSA stores the element type's zero (0 / 0.0 / "")
        InitList.AddChild(PadNode);
      end;
  end;
end;

procedure TPackratParser.SkipTypeQualifiers;
// FreeBASIC lets a type be qualified read-only: "Dim y As Const Integer = 2", "ByRef x As Const
// Integer", "Function f() ByRef As Const ZString". The qualifier binds to the TYPE, so it appears
// exactly where a type name is expected - and every one of those sites checked for an identifier and
// gave up on the keyword.
//
// ACCEPTED, NOT ENFORCED. The declaration parses and the program runs; assigning to such a variable
// is not yet rejected the way fbc rejects it. That is a deliberate first step, not an oversight: the
// examples demonstrate USING const data, and const-correctness is a separate piece of work with its
// own diagnostics.
begin
  SkipTypeQualifiersConst;
end;

function TPackratParser.SkipTypeQualifiersConst: Boolean;
// SkipTypeQualifiers, reporting whether a CONST was among what it skipped. The qualifier is still not
// ENFORCED, but it is no longer invisible: FreeBASIC OVERLOADS on it - "foo(ByRef n As Integer)" and
// "foo(ByRef n As Const Integer)" are two procedures, and the argument's own constness picks between
// them. Dropping the word silently made the two collide on one label and the second was discarded.
begin
  Result := False;
  while Assigned(Context.CurrentToken) and (Context.CurrentToken.TokenType = ttConstant) and
        (UpperCase(Context.CurrentToken.Value) = 'CONST') do
  begin
    Result := True;
    Context.Advance;
  end;
end;

function TPackratParser.TryConstDataExpr(N: TASTNode; out V: Variant): Boolean;
// Fold a DATA item that FreeBASIC writes as an EXPRESSION. Its manual's own example for DATA is
//   Data 3, 234, 435/4, 23+433, 87643, "Good" + "Bye!"
// and the page says the items are "expressions that are evaluated at compile time" - we accepted only
// literals, so the whole statement was a syntax error at the '/' and the example never ran.
//
// Wider than TryConstIntExpr on purpose, and kept separate from it: this one has to reach the FLOAT
// quotient of 435/4 (108.75, not 108) and the STRING concatenation of "Good" + "Bye!", neither of which
// a capacity expression may produce. It hands back the Variant the DATA pool already stores - string,
// ordinal or float - so nothing downstream changes.
//
// MODERN only, at the call site: in Commodore BASIC a DATA item is RAW TEXT up to the comma. "435/4"
// there is the string "435/4" (and reads as 435 into a numeric variable, VAL-style); folding it to
// 108.75 would be bending v7 to FreeBASIC. See the dialect note in ParseDataStatement.
var
  A, B: Variant;
  Op: string;
  D: Double;
  I: Int64;
  FS: TFormatSettings;   // '.' is the source's decimal point, whatever the machine's locale says

  function IsStr(const X: Variant): Boolean;
  begin
    Result := VarIsStr(X);
  end;

  // Numeric result narrowing: an exact integral value goes back as an ordinal so the DATA pool stores
  // it as an integer (23+433 must be 456, not 456.0), anything else stays a float.
  // ⚠️ The 32-bit bound is not cosmetic: ProcessData stores an ordinal item with
  // MakeSSAConstInt(Integer(...)), which TRUNCATES. Before this folder existed a big literal reached the
  // pool as its TEXT and survived; handing it back as an ordinal would silently mangle it. Outside the
  // Int32 range it goes back as a Double, which holds every integer up to 2^53 exactly.
  function Num(const X: Double): Variant;
  begin
    if (Frac(X) = 0) and (Abs(X) <= 2147483647) then Result := Int64(Round(X)) else Result := X;
  end;

begin
  Result := False;
  V := 0;
  if N = nil then Exit;
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  case N.NodeType of
    antLiteral:
      begin
        if Assigned(N.Token) and (N.Token.TokenType = ttStringLiteral) then
          begin V := VarToStr(N.Value); Exit(True); end;
        if VarIsStr(N.Value) then
        begin
          // A numeric token whose value is still its TEXT (the lexer is lazy about it): read it as a
          // number here, or "3" would fold as a string and "3+1" would concatenate to "31".
          // Same Int32 bound as Num(), and for the same reason.
          if TryStrToInt64(VarToStr(N.Value), I) then
          begin
            if (I >= -2147483648) and (I <= 2147483647) then V := I else V := Double(I);
            Exit(True);
          end;
          if TryStrToFloat(VarToStr(N.Value), D, FS) then begin V := D; Exit(True); end;
          V := VarToStr(N.Value); Exit(True);
        end;
        V := N.Value; Result := True;
      end;
    antParentheses:
      if N.ChildCount >= 1 then Result := TryConstDataExpr(N.GetChild(0), V);
    antUnaryOp:
      if (N.ChildCount >= 1) and Assigned(N.Token) and (N.Token.TokenType = ttOpSub) then
      begin
        Result := TryConstDataExpr(N.GetChild(0), A);
        if Result and not IsStr(A) then V := Num(-Double(A)) else Result := False;
      end;
    antBinaryOp:
      if (N.ChildCount >= 2) and Assigned(N.Token) then
      begin
        if not TryConstDataExpr(N.GetChild(0), A) then Exit;
        if not TryConstDataExpr(N.GetChild(1), B) then Exit;
        Op := VarToStr(N.Value);
        // '&' always concatenates; '+' concatenates when either side is a string.
        if (N.Token.TokenType = ttOpConcat) or (Op = '&') or
           ((N.Token.TokenType = ttOpAdd) and (IsStr(A) or IsStr(B))) then
          begin V := VarToStr(A) + VarToStr(B); Exit(True); end;
        if IsStr(A) or IsStr(B) then Exit;      // arithmetic on a string is not a constant DATA item
        case N.Token.TokenType of
          ttOpAdd:    begin V := Num(Double(A) + Double(B)); Result := True; end;
          ttOpSub:    begin V := Num(Double(A) - Double(B)); Result := True; end;
          ttOpMul:    begin V := Num(Double(A) * Double(B)); Result := True; end;
          // '/' is FreeBASIC's FLOATING division even between two integers: 435/4 is 108.75. That is
          // the whole reason this folder cannot be TryConstIntExpr with a wider return type.
          ttOpDiv:    if Double(B) <> 0 then begin V := Num(Double(A) / Double(B)); Result := True; end;
          ttOpIntDiv: if Double(B) <> 0 then begin V := Num(Trunc(Double(A) / Double(B))); Result := True; end;
        else
          if Op = '+' then begin V := Num(Double(A) + Double(B)); Result := True; end
          else if Op = '-' then begin V := Num(Double(A) - Double(B)); Result := True; end
          else if Op = '*' then begin V := Num(Double(A) * Double(B)); Result := True; end
          else if (Op = '/') and (Double(B) <> 0) then begin V := Num(Double(A) / Double(B)); Result := True; end;
        end;
      end;
  end;
end;

function TPackratParser.TryConstIntExpr(N: TASTNode; out V: Int64): Boolean;
// Fold a constant integer expression made of literals, parentheses and + - * \ over them.
//
// It exists for the fixed-length capacity of a string declaration, which the manual routinely writes as
// an expression: "Dim As ZString*(10+1) z", "As String*(6-1) sig". Only a bare literal was recognised,
// so those declarations recorded "capacity present but unknown" and every question about the SIZE of
// such a variable fell back to the width of the handle. The truncation on assignment was equally blind.
var
  A, B: Int64;
  Op: string;
begin
  Result := False;
  V := 0;
  if N = nil then Exit;
  case N.NodeType of
    antLiteral:
      begin
        if VarIsOrdinal(N.Value) then begin V := N.Value; Exit(True); end;
        Result := TryStrToInt64(VarToStr(N.Value), V);
      end;
    antIdentifier:
      // ⭐ A CONST NAME. FreeBASIC writes a capacity as one all the time ("f As ZString * MAXLEN"),
      // and only literals were folded here - so the capacity was recorded as "present but unknown"
      // and SizeOf of the field answered the string DESCRIPTOR's width instead. Resolved from the
      // values recorded as each CONST was parsed, so a name used before its declaration still
      // declines, exactly as it does in fbc.
      Result := TryStrToInt64(FConstIntValues.Values[UpperCase(VarToStr(N.Value))], V);
    antParentheses:
      if N.ChildCount >= 1 then Result := TryConstIntExpr(N.GetChild(0), V);
    antUnaryOp:
      if (N.ChildCount >= 1) and Assigned(N.Token) and (N.Token.TokenType = ttOpSub) then
      begin
        Result := TryConstIntExpr(N.GetChild(0), A);
        if Result then V := -A;
      end;
    antBinaryOp:
      if (N.ChildCount >= 2) and Assigned(N.Token) then
      begin
        if not TryConstIntExpr(N.GetChild(0), A) then Exit;
        if not TryConstIntExpr(N.GetChild(1), B) then Exit;
        Op := VarToStr(N.Value);
        case N.Token.TokenType of
          ttOpAdd: begin V := A + B; Result := True; end;
          ttOpSub: begin V := A - B; Result := True; end;
          ttOpMul: begin V := A * B; Result := True; end;
          ttOpIntDiv: if B <> 0 then begin V := A div B; Result := True; end;
        else
          if Op = '+' then begin V := A + B; Result := True; end
          else if Op = '-' then begin V := A - B; Result := True; end
          else if Op = '*' then begin V := A * B; Result := True; end;
        end;
      end;
  end;
end;

function TPackratParser.AtPointerSuffix: Boolean;
// True when the current token is FreeBASIC's pointer-type suffix. fbc spells it either "PTR" or the
// synonym "POINTER" - "Dim p As ZString Pointer" is the manual's own wording - and both are reserved
// there, so in a TYPE position the word can only be the suffix.
//
// MODERN only. In CLASSIC, POINTER(v) is the Commodore spelling of VARPTR and has to stay a bare name
// (SedaiSSA intercepts it as address-of), which is why the synonym is gated on the dialect and "PTR"
// is not: v7 has no pointer types at all.
var
  W: string;
begin
  Result := False;
  // "<type> CONST PTR" - a CONSTANT POINTER, as opposed to the "AS CONST <type>" that SkipTypeQualifiers
  // already eats (a pointer to constant data). The qualifier sits BETWEEN the type and the suffix, so no
  // amount of skipping before the type reaches it: "Dim p As Integer Const Ptr" parsed as a plain
  // INTEGER, the PTR was lost with it, and "*p" then dereferenced a variable that was never a pointer.
  // Consumed HERE, in the predicate, because every caller advances past the PTR immediately after - and
  // there are fifteen of them.
  if Assigned(Context.CurrentToken) and (Context.CurrentToken.TokenType = ttConstant) and
     (UpperCase(Context.CurrentToken.Value) = 'CONST') and Assigned(Context.PeekNext) and
     (Context.PeekNext.TokenType = ttIdentifier) then
  begin
    W := UpperCase(VarToStr(Context.PeekNext.Value));
    if (W = kPTR) or (FModernMode and (W = kPOINTER)) then
    begin
      Context.Advance;                   // consume CONST; the caller consumes the PTR that follows
      Exit(True);
    end;
  end;
  if not Context.Check(ttIdentifier) then Exit;
  W := UpperCase(VarToStr(Context.CurrentToken.Value));
  Result := (W = kPTR) or (FModernMode and (W = kPOINTER));
end;

function TPackratParser.ParseDimensionList: TASTNode;
var
  Dimension, UpperExpr, RangeNode: TASTNode;
begin
  Result := TASTNode.Create(antDimensions);

  // ⛔ AN EMPTY DIMENSION LIST IS STILL A DIMENSION LIST: "a()" declares a DYNAMIC array, and the
  // caller has already consumed the '('. This went straight into ParseExpression, which has nothing to
  // read at a ')', so "Static a() As Integer" as a FIELD of a Type died on 'Unexpected token ")"' -
  // while the identical "Dim a() As Integer" at module level was accepted, because THAT path spells
  // the empty case out for itself. One more rule that one path had and its sibling did not; answered
  // here, once, where every caller asks.
  if Context.Check(ttDelimParClose) then Exit;

  repeat
    // FreeBASIC bare ellipsis dimension "(...)": no lower bound (defaults to 0), the upper bound is deduced
    // from the initializer element count (ProcessDim). "..." lexes as consecutive '.' tokens.
    if Context.Check(ttOpDot) then
    begin
      while Context.Check(ttOpDot) do Context.Advance;   // consume the "..." dots
      RangeNode := TASTNode.Create(antDimRange, Context.CurrentToken);
      RangeNode.AddChild(TASTNode.CreateWithValue(antLiteral, '0', Context.CurrentToken));   // lower bound 0
      RangeNode.AddChild(TASTNode.CreateWithValue(antLiteral, '0', Context.CurrentToken));   // placeholder ub
      RangeNode.Attributes.Values['ELLIPSIS'] := '1';
      Result.AddChild(RangeNode);
      if Context.Check(ttSeparParam) then begin Context.Advance; Continue; end else Break;
    end;
    Dimension := ParseExpression;
    if not Assigned(Dimension) then Break;
    // FreeBASIC explicit bound "lb TO ub": the first expression is the lower bound. Wrap both in an
    // antDimRange (child0=lb, child1=ub). A bare expression stays the upper bound (lower bound = 0).
    if Context.Check(ttLoopControl) and (UpperCase(Context.CurrentToken.Value) = kTO) then
    begin
      Context.Advance;                              // consume TO
      // FreeBASIC ellipsis upper bound "lb TO ...": the upper bound is deduced from the number of elements
      // in the initializer (handled in ProcessDim). "..." lexes as consecutive '.' tokens. Represent it as
      // an antDimRange marked ELLIPSIS with a placeholder upper bound of 0.
      if Context.Check(ttOpDot) then
      begin
        while Context.Check(ttOpDot) do Context.Advance;   // consume the "..." dots
        RangeNode := TASTNode.Create(antDimRange, Context.CurrentToken);
        RangeNode.AddChild(Dimension);
        RangeNode.AddChild(TASTNode.CreateWithValue(antLiteral, '0', Context.CurrentToken));
        RangeNode.Attributes.Values['ELLIPSIS'] := '1';
        Result.AddChild(RangeNode);
      end
      else
      begin
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
      end;
    end
    else if FOptionBase <> 0 then
    begin
      // OPTION BASE 1 in effect: a bare upper bound "a(n)" means lower bound 1 (a(1..n)), so wrap it in an
      // antDimRange(lb = OPTION BASE, ub = the expression). An explicit "lb TO ub" above is unaffected.
      RangeNode := TASTNode.Create(antDimRange, Context.CurrentToken);
      RangeNode.AddChild(TASTNode.CreateWithValue(antLiteral, IntToStr(FOptionBase), Context.CurrentToken));
      RangeNode.AddChild(Dimension);
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

function TPackratParser.FoldFileHandlePostfix(BaseNode: TASTNode): TASTNode;
// A file number can be a UDT member (e.g. "#bf.bw") — the handle identifier has
// already been consumed; fold any trailing ".field" chain into antMemberAccess nodes
// so the SSA evaluates the member's integer value (ProcessDopen's expression fallback).
var
  MemberNode: TASTNode;
begin
  Result := BaseNode;
  while Context.Check(ttOpDot) do
  begin
    Context.Advance;                               // '.'
    if not Context.Check(ttIdentifier) then
    begin
      HandleError('Expected field name after "." in file number', Context.CurrentToken);
      Break;
    end;
    MemberNode := TASTNode.CreateWithValue(antMemberAccess, UpperCase(Context.CurrentToken.Value),
                                           Context.CurrentToken);
    MemberNode.AddChild(Result);
    Result := MemberNode;
    Context.Advance;                               // field name
  end;
end;

function TPackratParser.ParseFileHandleIdent: TASTNode;
// Current token is a file-number identifier: build antIdentifier, advance, and fold any
// trailing ".field" chain (e.g. "#bf.bw"). Used at inline file-handle call sites.
begin
  Result := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
  Context.Advance;
  Result := FoldFileHandlePostfix(Result);
end;

function TPackratParser.ParseFileNumberOperand: TASTNode;
// ⛔ ONE place decides what a file number may LOOK like; the '#' has already been consumed. Three
// spellings: a literal, an identifier with its trailing ".field" chain, and a PARENTHESISED
// expression. "Print #(1), x" is FreeBASIC's own spelling - its suite writes it that way inside a
// macro, where the parentheses protect the argument - and it failed at SEVEN statements at once
// because each of them re-listed the two spellings it happened to know.
// Returns nil when the current token begins none of the three; the caller reports its own message.
begin
  Result := nil;
  if Context.Check(ttNumber) or Context.Check(ttInteger) then
  begin
    Result := TASTNode.CreateWithValue(antLiteral, StrToInt(Context.CurrentToken.Value), Context.CurrentToken);
    Context.Advance;
  end
  else if Context.Check(ttIdentifier) then
    Result := ParseFileHandleIdent
  else if Context.Check(ttDelimParOpen) then
    Result := FoldFileHandlePostfix(FExpressionParser.ParseExpression(precCall));
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

function IsBuiltinTypeName(const N: string): Boolean;
// True for the built-in scalar type names. Used to keep a "DIM v AS T = T(args)" from being read as a
// constructor call when T is a builtin that is ALSO a function — notably STRING (STRING(count, ch)):
// there "= String(...)" is the STRING function, not a ctor. Constructors only ever apply to UDTs.
var
  T: string;
begin
  T := UpperCase(N);
  Result := (T = 'INTEGER') or (T = 'LONG') or (T = 'SHORT') or (T = 'BYTE') or
            (T = 'UBYTE') or (T = 'USHORT') or (T = 'UINTEGER') or (T = 'ULONG') or
            (T = 'LONGINT') or (T = 'ULONGINT') or (T = 'BOOLEAN') or
            (T = 'SINGLE') or (T = 'DOUBLE') or
            (T = 'STRING') or (T = 'ZSTRING') or (T = 'WSTRING');
end;

function TPackratParser.ParseDimStatement: TASTNode;
var
  NameIsConst: Boolean;   // "As Const <type>" on this declaration
  FixedCapVal: Int64;   // folded "* n" capacity
  Token, NameTok, TypeTok, SharedTypeTok: TLexerToken;
  ArrayDecl, VarNameNode, TypeNode, CtorArgs, ArgExpr, InitExpr, AddrNode, FuncPtrSigNode, LeadingTypeOfExpr: TASTNode;
  SharedFpNode: TASTNode;   // leading-AS "Dim As Sub(...) g": the shared funcptr signature
  MemberAccess, StaticDef: TASTNode;   // "Dim As T Type.member = init": static member definition
  IsShared, IsByref, LeadingAS, IsTuple, HadComma: Boolean;
  DimTypeName, SharedTypeName, SharedFixedLen: string;
  SavedIdx, TupleDepth: Integer;
begin
  Token := Context.CurrentToken;
  SharedFpNode := nil;
  // VAR / STATIC share the ttDataDeclaration token with DIM; route to their own parsers.
  if UpperCase(VarToStr(Token.Value)) = kVAR then Exit(ParseVarStatement);
  if UpperCase(VarToStr(Token.Value)) = kSTATIC then
  begin
    // FreeBASIC static member method definition: "Static Sub|Function Type.method(...)". The STATIC
    // keyword marks the method as callable without an instance (through the type name — the call site
    // supplies a dummy THIS, see TryStaticMethodCall). The body is an ordinary "Type.method" with an
    // implicit THIS, so consume STATIC here and let ParseProcedureDecl parse the rest as usual. Any
    // other "STATIC ..." is a persistent local variable declaration.
    if Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttProcedureStart) and
       ((UpperCase(VarToStr(Context.PeekNext.Value)) = kSUB) or
        (UpperCase(VarToStr(Context.PeekNext.Value)) = kFUNCTION)) then
    begin
      Context.Advance;                                 // consume STATIC
      Exit(ParseProcedureDecl);
    end;
    Exit(ParseStaticStatement);
  end;
  Result := TASTNode.Create(antDim, Token);
  Context.Advance; // Consume DIM
  LeadingTypeOfExpr := nil;   // set when the leading-AS type is "TypeOf(expr)" (inferred in the SSA pre-pass)
  // M6: "DIM SHARED ..." — the declared variables are module globals visible (read/write) inside
  // SUB/FUNCTION bodies. Marked on each decl with the 'SHARED' attribute for the SSA pre-scan.
  IsShared := Context.Check(ttSharedDecl);
  if IsShared then Context.Advance;   // consume SHARED
  // FreeBASIC COMMON [SHARED] var: a module-shared variable. In our single-module model this is
  // exactly DIM SHARED, so force the SHARED flag (an explicit SHARED, if present, was consumed above).
  if UpperCase(VarToStr(Token.Value)) = kCOMMON then IsShared := True;
  // FreeBASIC reference variable: "DIM BYREF r AS T = target" — r is an alias for target (shared
  // storage). Detected here as a statement-level modifier; handled in the typed-scalar branch below.
  IsByref := Context.Check(ttParamMode) and (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'BYREF');
  if IsByref then Context.Advance;    // consume BYREF

  // FreeBASIC "leading-AS" form: "DIM [SHARED] AS <type> name1[, name2, ...] [= init]" — the type comes
  // first and is shared by every name in the list (e.g. "DIM AS STRING ch = MID(s,1,1)"). Parse the
  // shared type once here; each declaration in the loop below is then just "name [= init]".
  NameIsConst := False;
  LeadingAS := Context.Check(ttAsType);
  if LeadingAS then
  begin
    Context.Advance;                          // AS
    // "As Const <type>": not enforced, but part of the variable's identity for OVERLOAD RESOLUTION -
    // a const argument selects the const overload. Remembered now, not just skipped.
    NameIsConst := SkipTypeQualifiersConst;
    // FreeBASIC "DIM AS TypeOf(expr) name": the type is inferred from an expression. Capture the
    // expression; each declared name gets it as child[1] with TYPEOF='1' and the concrete type is
    // resolved in the SSA pre-pass (like VAR's INFER, but with no initializer).
    if (UpperCase(Context.CurrentToken.Value) = 'TYPEOF') and Assigned(Context.PeekNext) and
       (Context.PeekNext.TokenType = ttDelimParOpen) then
    begin
      SharedTypeTok := Context.CurrentToken;
      Context.Advance;                        // TYPEOF
      Context.Advance;                        // '('
      LeadingTypeOfExpr := FExpressionParser.ParseExpression;
      if Context.Check(ttDelimParClose) then Context.Advance;   // ')'
      SharedTypeName := 'INTEGER';            // placeholder; replaced by the inferred type in SSA
      SharedFixedLen := '';
    end
    else
    begin
    // "Dim As Sub(...) g" / "Dim As Function(...) As R f": the leading-AS spelling of a function-pointer
    // variable. Only the trailing form ("Dim f As Function(...)") parsed the signature here, so the
    // leading one stopped at a type name that is a KEYWORD.
    if Context.Check(ttProcedureStart) then
    begin
      SharedTypeTok := Context.CurrentToken;
      SharedFpNode := TASTNode.Create(antArrayDecl, SharedTypeTok);
      if TryParseProcPtrType(SharedFpNode) then
      begin
        SharedTypeName := 'INTEGER';          // a procedure entry PC, like the named funcptr TYPE alias
        SharedFixedLen := '';
      end
      else
      begin
        SharedFpNode.Free; SharedFpNode := nil;
        HandleError('Expected type name after AS', Context.CurrentToken);
        DoNodeCreated(Result);
        Exit;
      end;
    end
    else
    begin
    if not Context.Check(ttIdentifier) then
    begin
      HandleError('Expected type name after AS', Context.CurrentToken);
      DoNodeCreated(Result);
      Exit;
    end;
    SharedTypeTok := Context.CurrentToken;
    SharedTypeName := ParseDottedName;
    while AtPointerSuffix do
    begin
      SharedTypeName := SharedTypeName + ' PTR';
      Context.Advance;                        // consume PTR
    end;
    // FreeBASIC fixed-length string in leading-AS form: "DIM AS STRING * n name[, ...]". The capacity
    // follows the shared type and applies to every name in the list. Parse it once (advisory in v1 —
    // storage stays variable-length) and stamp FIXEDLEN on each declaration below.
    SharedFixedLen := '';
    if Context.Check(ttOpMul) then
    begin
      Context.Advance;                        // '*'
      InitExpr := FExpressionParser.ParseExpression(precTerm);   // length operand: an EXPRESSION
      if Assigned(InitExpr) then
      begin
        if TryConstIntExpr(InitExpr, FixedCapVal) then SharedFixedLen := IntToStr(FixedCapVal)
        else SharedFixedLen := '-1';          // present but non-constant -> advisory
        InitExpr.Free;
      end;
    end;
    end;   // end of the non-funcptr leading-AS type parse
    end;   // end of the non-TypeOf leading-AS type parse
  end;

  // Parse declarations separated by commas. Each is either:
  //   name AS typename   -> typed scalar (UDT record or explicit builtin type)
  //   AS typename name   -> leading-AS typed scalar (shared type parsed above)
  //   name ( dims )      -> array (classic)
  repeat
    // "Dim ByRef a As T = x, ByRef b As T = y": BYREF may be repeated before EACH name, and fbc's own
    // suite writes the list that way. Only the LEADING one was read, so the second died as "Expected
    // variable name in array declaration". The modifier is list-wide here either way, so a repeat is
    // consumed rather than tracked per name.
    if Context.Check(ttParamMode) and (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'BYREF') then
    begin
      IsByref := True;
      Context.Advance;
    end;
    // Leading-AS array declaration: "DIM [SHARED] AS type name(dims)". Route to ParseArrayDeclaration
    // (which handles the dimension list, including "lo TO hi" ranges and negative lower bounds) and
    // inject the shared type when no explicit "AS type" follows the array.
    if LeadingAS and Context.Check(ttIdentifier) and Assigned(Context.PeekNext) and
       (Context.PeekNext.TokenType = ttDelimParOpen) then
    begin
      ArrayDecl := ParseArrayDeclaration;
      if not Assigned(ArrayDecl) then Break;
      // Inject the shared type unless the array already carries an explicit element-type child. The type
      // child (when present) is the antIdentifier at index 2; an ARRAYINIT initializer is an antArgumentList
      // that also lands at/after index 2, so a bare ChildCount check would wrongly skip type injection for
      // "DIM AS String a(n) = { ... }". Insert at index 2 so it precedes any initializer list.
      if (ArrayDecl.ChildCount < 3) or (ArrayDecl.GetChild(2).NodeType <> antIdentifier) then
        ArrayDecl.InsertChild(2, TASTNode.CreateWithValue(antIdentifier, SharedTypeName, SharedTypeTok));
      if SharedFixedLen <> '' then ArrayDecl.Attributes.Values['FIXEDLEN'] := SharedFixedLen;  // AS STRING * n arr()
      if IsShared then ArrayDecl.Attributes.Values['SHARED'] := '1';
      Result.AddChild(ArrayDecl);
      if Context.Check(ttSeparParam) then begin Context.Advance; Continue; end;
      Break;
    end;

    // A scalar declaration: leading-AS ("AS type name"), name-first typed ("name AS type"), or a bare
    // suffix-typed scalar ("Dim x" / "Dim s$" / "Dim n%" — no AS, no dims). An identifier NOT followed by
    // '(' is a scalar (an array would have '('); the type is the AS-clause, else inferred from the suffix.
    if LeadingAS or (Context.Check(ttIdentifier) and
       (not (Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttDelimParOpen)))) then
    begin
      FuncPtrSigNode := nil;   // set only by the "name AS FUNCTION(...)" branch; nil elsewhere (leading-AS)
      if LeadingAS then
      begin
        // Leading-AS: the declaration is a bare name using the shared type parsed above.
        if not Context.Check(ttIdentifier) then
        begin
          HandleError('Expected variable name after AS type', Context.CurrentToken);
          Break;
        end;
        NameTok := Context.CurrentToken;
        Context.Advance;                       // name
        // FreeBASIC STATIC MEMBER DEFINITION: "Dim As Integer UDT.countID = 0" at module level gives the
        // type's static member its one storage. The name is DOTTED, which the scalar grammar below cannot
        // read — it handed the '.' to the expression parser and the whole file failed to parse. Our static
        // members are already backed by a shared global declared with the TYPE, so the definition is worth
        // exactly its initializer: an assignment to that member, or nothing at all.
        if Context.Check(ttOpDot) and (Result.ChildCount = 0) then
        begin
          Context.Advance;                     // '.'
          if Context.Check(ttIdentifier) or
             ((Length(VarToStr(Context.CurrentToken.Value)) > 0) and
              (UpCase(VarToStr(Context.CurrentToken.Value)[1]) in ['A'..'Z', '_'])) then
          begin
            MemberAccess := TASTNode.CreateWithValue(antMemberAccess,
                              UpperCase(VarToStr(Context.CurrentToken.Value)), Context.CurrentToken);
            MemberAccess.AddChild(TASTNode.CreateWithValue(antIdentifier,
                              UpperCase(VarToStr(NameTok.Value)), NameTok));
            Context.Advance;                   // field name
            if Context.Check(ttOpEq) then
            begin
              Context.Advance;                 // '='
              InitExpr := FExpressionParser.ParseExpression;
              if Assigned(InitExpr) then
              begin
                StaticDef := TASTNode.Create(antAssignment, NameTok);
                StaticDef.AddChild(MemberAccess);
                StaticDef.AddChild(InitExpr);
                Result.Free;
                Result := StaticDef;
                DoNodeCreated(Result);
                Exit;
              end;
            end;
            MemberAccess.Free;                 // no initializer: the declaration alone emits nothing
            Result.Free;
            Result := nil;
            Exit;
          end;
        end;
        DimTypeName := SharedTypeName;
        TypeTok := SharedTypeTok;
      end
      // FreeBASIC STATIC MEMBER DEFINITION, trailing-AS spelling: "Dim T.x As Integer". The leading-AS
      // one ("Dim As Integer T.x") was already read below; this is the SAME declaration written the
      // other way round, and it is the way FreeBASIC's own test suite writes it - so every program that
      // gives a static member its storage failed to parse on the definition line, while the member
      // itself worked. Our static members are backed by a shared global declared with the TYPE, so the
      // definition is worth exactly its initializer: an assignment to the member, or nothing at all.
      // ⚠️ The owner may be a NESTED type ("Dim T.U.x As Integer"), so the dotted run is read whole.
      else if (Result.ChildCount = 0) and Context.Check(ttIdentifier) and
              Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttOpDot) then
      begin
        NameTok := Context.CurrentToken;
        Context.Advance;                       // owner name
        MemberAccess := TASTNode.CreateWithValue(antIdentifier,
                          UpperCase(VarToStr(NameTok.Value)), NameTok);
        while Context.Check(ttOpDot) and Assigned(Context.PeekNext) and
              (Length(VarToStr(Context.PeekNext.Value)) > 0) and
              (UpCase(VarToStr(Context.PeekNext.Value)[1]) in ['A'..'Z', '_']) do
        begin
          Context.Advance;                     // '.'
          StaticDef := TASTNode.CreateWithValue(antMemberAccess,
                         UpperCase(VarToStr(Context.CurrentToken.Value)), Context.CurrentToken);
          StaticDef.AddChild(MemberAccess);
          MemberAccess := StaticDef;
          Context.Advance;                     // member name
        end;
        // "As <type>" (and any "* n" / PTR tail) belongs to the member's own declaration inside the
        // TYPE, not here: skip it to the end of the statement, then keep the initializer if there is one.
        while (not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile])) and
              (not Context.Check(ttOpEq)) do
          Context.Advance;
        if Context.Check(ttOpEq) then
        begin
          Context.Advance;                     // '='
          InitExpr := FExpressionParser.ParseExpression;
          if Assigned(InitExpr) then
          begin
            StaticDef := TASTNode.Create(antAssignment, NameTok);
            StaticDef.AddChild(MemberAccess);
            StaticDef.AddChild(InitExpr);
            Result.Free;
            Result := StaticDef;
            DoNodeCreated(Result);
            Exit;
          end;
        end;
        MemberAccess.Free;                     // no initializer: the declaration alone emits nothing
        Result.Free;
        Result := nil;
        Exit;
      end
      else if not (Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttAsType)) then
      begin
        // Bare suffix-typed scalar "Dim x" / "Dim s$": no AS clause; the type is inferred from the name
        // suffix ($ -> string, % -> integer, ...) by the SSA pre-scan (empty DimTypeName).
        NameTok := Context.CurrentToken;
        Context.Advance;                       // name
        DimTypeName := '';
        TypeTok := NameTok;
      end
      else
      begin
        // "name AS typename"
        NameTok := Context.CurrentToken;
        Context.Advance;                       // name
        Context.Advance;                       // AS
        NameIsConst := SkipTypeQualifiersConst;   // see the leading-AS note above
        // FreeBASIC function-pointer variable "DIM fp AS FUNCTION(...) AS ret": int-banked (holds an
        // entry PC); the signature is captured on a scratch node and copied onto the decl below.
        FuncPtrSigNode := nil;
        if Context.Check(ttProcedureStart) then
        begin
          FuncPtrSigNode := TASTNode.Create(antArrayDecl, NameTok);
          if TryParseProcPtrType(FuncPtrSigNode) then
          begin
            DimTypeName := 'INTEGER';
            TypeTok := NameTok;
          end
          else
          begin
            FuncPtrSigNode.Free; FuncPtrSigNode := nil;
          end;
        end;
        if not Assigned(FuncPtrSigNode) then
        begin
          if not Context.Check(ttIdentifier) then
          begin
            HandleError('Expected type name after AS', Context.CurrentToken);
            Break;
          end;
          TypeTok := Context.CurrentToken;
          DimTypeName := ParseDottedName;          // dotted: namespace-qualified type ("Forms.Point")
          // FreeBASIC pointer type: "<type> PTR" (one or more PTR). A pointer is stored as an int handle
          // (the address); the suffix is kept on the type name so the SSA records the pointee bank.
          while AtPointerSuffix do
          begin
            DimTypeName := DimTypeName + ' PTR';
            Context.Advance;                       // consume PTR
          end;
        end;
      end;
      ArrayDecl := TASTNode.Create(antArrayDecl, NameTok);
      if NameIsConst then ArrayDecl.Attributes.Values['CONSTV'] := '1';
      VarNameNode := TASTNode.CreateWithValue(antIdentifier, UpperCase(NameTok.Value), NameTok);
      ArrayDecl.AddChild(VarNameNode);
      if LeadingAS and Assigned(LeadingTypeOfExpr) then
      begin
        // "DIM AS TypeOf(expr) name": child[1] is the expression; the SSA pre-pass infers its type.
        ArrayDecl.AddChild(LeadingTypeOfExpr.Clone);
        ArrayDecl.Attributes.Values['TYPEOF'] := '1';
      end
      else
      begin
        TypeNode := TASTNode.CreateWithValue(antIdentifier, DimTypeName, TypeTok);
        ArrayDecl.AddChild(TypeNode);        // child[1] is antIdentifier (type) => typed scalar
        // ⛔ ...and the TRAILING spelling of TypeOf is one of these. "Dim b As TypeOf(a)" reads its type
        // as the ordinary name "TYPEOF" with the operand landing in the argument list beside it, so the
        // SSA pre-pass that resolves TypeOf never fired and the variable was declared with an UNKNOWN
        // type: a String answered the float default and printed 0. Only the leading-AS spelling
        // ("Dim As TypeOf(a) b") was ever marked, which is why the feature looked present.
        if UpperCase(DimTypeName) = 'TYPEOF' then
          ArrayDecl.Attributes.Values['TYPEOF'] := '1';
      end;
      // Leading-AS fixed-length string capacity ("DIM AS STRING * n name") applies to each name.
      if LeadingAS and (SharedFixedLen <> '') then ArrayDecl.Attributes.Values['FIXEDLEN'] := SharedFixedLen;
      // Transfer a captured function-pointer signature (see above) onto the declaration node. The
      // leading-AS spelling shares ONE signature across every name in the list, so it is copied rather
      // than consumed (SharedFpNode is freed once, after the loop).
      if Assigned(FuncPtrSigNode) then
      begin
        ArrayDecl.Attributes.Values['FUNCPTR'] := '1';
        ArrayDecl.Attributes.Values['FPPARAMS'] := FuncPtrSigNode.Attributes.Values['FPPARAMS'];
        ArrayDecl.Attributes.Values['FPRET'] := FuncPtrSigNode.Attributes.Values['FPRET'];
        // ...and WHETHER that return is a reference. Carried beside FPRET at BOTH copy sites, or
        // the trailing spelling honoured "ByRef As R" and the leading-AS one handed back the address.
        ArrayDecl.Attributes.Values['FPRETBYREF'] := FuncPtrSigNode.Attributes.Values['FPRETBYREF'];
        FuncPtrSigNode.Free; FuncPtrSigNode := nil;
      end
      else if LeadingAS and Assigned(SharedFpNode) then
      begin
        ArrayDecl.Attributes.Values['FUNCPTR'] := '1';
        ArrayDecl.Attributes.Values['FPPARAMS'] := SharedFpNode.Attributes.Values['FPPARAMS'];
        ArrayDecl.Attributes.Values['FPRET'] := SharedFpNode.Attributes.Values['FPRET'];
        // ...and WHETHER that return is a reference. Carried beside FPRET at BOTH copy sites, or
        // the trailing spelling honoured "ByRef As R" and the leading-AS one handed back the address.
        ArrayDecl.Attributes.Values['FPRETBYREF'] := SharedFpNode.Attributes.Values['FPRETBYREF'];
      end;
      // FreeBASIC reference variable: "DIM BYREF r AS T = target". Require "= target" and store @target
      // as child[2] (an antProcAddress), so the SSA backs the target's stable address and binds r to it;
      // r then auto-dereferences on every read/write. Skips the normal fixed-len / init / ctor handling.
      if IsByref then
      begin
        ArrayDecl.Attributes.Values['BYREF'] := '1';
        if Context.Check(ttOpEq) then
        begin
          Context.Advance;                   // =
          InitExpr := FExpressionParser.ParseExpression;   // the referand (an lvalue)
          if Assigned(InitExpr) then
          begin
            if InitExpr.NodeType = antIdentifier then
            begin
              // @scalar: historical shape (Value = name, no child).
              AddrNode := TASTNode.CreateWithValue(antProcAddress, UpperCase(VarToStr(InitExpr.Value)), NameTok);
              InitExpr.Free;
            end
            else
            begin
              // @arr(i) / @obj.field: keep the operand subtree as child0.
              AddrNode := TASTNode.Create(antProcAddress, NameTok);
              AddrNode.AddChild(InitExpr);
            end;
            ArrayDecl.AddChild(AddrNode);     // child[2] = @target
          end;
        end
        else
          HandleError('DIM BYREF requires an initializer: DIM BYREF name AS type = target', Context.CurrentToken);
        if IsShared then ArrayDecl.Attributes.Values['SHARED'] := '1';
        DoNodeCreated(ArrayDecl);
        Result.AddChild(ArrayDecl);
        if Context.Check(ttSeparParam) then begin Context.Advance; Continue; end;
        Break;
      end;
      // FreeBASIC fixed-length string: "AS STRING * n" / "AS WSTRING * n" / "AS ZSTRING * n". The
      // declared capacity is parsed and recorded (attribute 'FIXEDLEN', advisory in v1 — storage is
      // variable-length). Consume "* <length-expr>" so the declaration parses cleanly.
      if Context.Check(ttOpMul) then
      begin
        Context.Advance;                     // consume '*'
        InitExpr := FExpressionParser.ParseExpression(precTerm);   // length operand: an EXPRESSION
        if Assigned(InitExpr) then
        begin
          // Record the capacity. A constant literal becomes the number (the SSA truncates assignments
          // to it); a non-constant capacity stays advisory ('1' = present but unknown).
          if TryConstIntExpr(InitExpr, FixedCapVal) then
            ArrayDecl.Attributes.Values['FIXEDLEN'] := IntToStr(FixedCapVal)
          else
            ArrayDecl.Attributes.Values['FIXEDLEN'] := '-1';   // present but non-constant -> advisory
          InitExpr.Free;
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
           (UpperCase(Context.CurrentToken.Value) = DimTypeName) and
           (not IsBuiltinTypeName(DimTypeName)) then
          Context.Advance                    // RHS == declared UDT: ctor form (block below reads '(')
        else if Context.Check(ttDelimParOpen) then
        begin
          // FreeBASIC aggregate init "Dim As T v = (a, b, c)": a parenthesised comma-tuple sets the UDT's
          // fields in declaration order. Distinguish it from an expression that merely STARTS with '(' —
          // e.g. "= (x + y) \ 2" — by looking ahead for a TOP-LEVEL comma inside the leading parentheses;
          // only then is it a tuple. Without one, fall through to the normal (full) expression parse.
          // ⭐ ONE reader for the aggregate tuple, shared with STATIC. It used to live inline here and
          // nowhere else, so "Static As T v = (a, b)" - the same declaration with the other modifier -
          // parsed the parentheses as an EXPRESSION and failed. See TryParseAggregateTuple.
          CtorArgs := TryParseAggregateTuple(DimTypeName);
          if Assigned(CtorArgs) then
            ArrayDecl.AddChild(CtorArgs)                      // child[2] = tuple
          else
          begin
            InitExpr := FExpressionParser.ParseExpression;    // full expression, e.g. "(x + y) \ 2"
            if Assigned(InitExpr) then ArrayDecl.AddChild(InitExpr);
          end;
        end
        else
        begin
          InitExpr := FExpressionParser.ParseExpression;    // general initializer expression
          if Assigned(InitExpr) then
            ArrayDecl.AddChild(InitExpr);                   // child[2] = initializer (not antArgumentList)
        end;
      end;
      // FreeBASIC shorthand "Dim v As T = Type(args)" (no <T>): the type constructor was parsed with an
      // inferred (empty) type name; fill it from the declared type so the SSA builds a T temporary.
      if (ArrayDecl.ChildCount >= 3) and (ArrayDecl.GetChild(2).Attributes.Values['INFERTYPE'] = '1') and
         (ArrayDecl.GetChild(2).ChildCount >= 1) then
      begin
        ArrayDecl.GetChild(2).GetChild(0).Value := DimTypeName;
        ArrayDecl.GetChild(2).Attributes.Values['INFERTYPE'] := '';
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

  if Assigned(LeadingTypeOfExpr) then LeadingTypeOfExpr.Free;   // clones are on each decl; free the original
  if Assigned(SharedFpNode) then SharedFpNode.Free;             // one signature shared by every name above
  DoNodeCreated(Result);
end;

function TPackratParser.ParseVarStatement: TASTNode;
// VAR name = expr [, name = expr ...] (FreeBASIC): declare variables whose type is inferred from the
// initializer expression. Produced as an antDim whose antArrayDecl children carry child[0]=name and
// child[1]=the initializer expression, marked INFER='1'. The SSA evaluates the initializer, infers its
// bank, declares the (lexically scoped) variable in that bank, and stores the value.
var
  Token, NameTok: TLexerToken;
  Decl, InitExpr, AddrNode: TASTNode;
  VarIsByref, VarIsShared, DeclIsByref: Boolean;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antDim, Token);
  Context.Advance;                                   // consume VAR
  // "Var ByRef r = target": the reference spelling, which the manual writes beside "Dim ByRef As T r".
  // It was not accepted at all - the word BYREF was met where a name was expected and the statement
  // failed with "Expected a variable name after VAR". The initializer is wrapped in "@" here, exactly
  // as DIM BYREF wraps it, so the two spellings reach the SSA in one shape; the TYPE is what VAR
  // leaves to be inferred, and the pre-pass reads it out of the referand.
  // "Var Shared v = e": the module-global spelling, which DIM and STATIC both accept and this did not -
  // "Var Shared" failed as "Expected a variable name after VAR", the word SHARED being where a name was
  // expected. Either order is FreeBASIC's ("Var Shared ByRef r = t"), so both modifiers are read in a
  // loop rather than in a fixed sequence.
  VarIsByref := False;
  VarIsShared := False;
  while Context.Check(ttSharedDecl) or
        (Context.Check(ttParamMode) and (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'BYREF')) do
  begin
    if Context.Check(ttSharedDecl) then VarIsShared := True else VarIsByref := True;
    Context.Advance;
  end;
  repeat
    // ...and BYREF may also stand before EACH name: "Var Shared ByRef a = x, ByRef b = y". The leading
    // modifier applies to the whole list, a per-name one only to its own declaration - so the flag is
    // read here as well, into a copy, and the list-wide value survives for the names that omit it.
    DeclIsByref := VarIsByref;
    if Context.Check(ttParamMode) and (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'BYREF') then
    begin
      DeclIsByref := True;
      Context.Advance;
    end;
    if not Context.Check(ttIdentifier) then
    begin
      HandleError('Expected a variable name after VAR', Context.CurrentToken);
      while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) do
        Context.Advance;                             // recover: skip the bad statement (avoid a re-parse loop)
      Break;
    end;
    NameTok := Context.CurrentToken;
    Context.Advance;                                 // name
    if not Context.Check(ttOpEq) then
    begin
      HandleError('VAR requires an initializer: VAR name = expression', Context.CurrentToken);
      Break;
    end;
    Context.Advance;                                 // =
    InitExpr := FExpressionParser.ParseExpression;
    if not Assigned(InitExpr) then Break;
    Decl := TASTNode.Create(antArrayDecl, NameTok);
    Decl.AddChild(TASTNode.CreateWithValue(antIdentifier, UpperCase(NameTok.Value), NameTok));
    if DeclIsByref then
    begin
      // Wrap the referand in "@", the shape DIM BYREF produces (bare name = Value with no child).
      if InitExpr.NodeType = antIdentifier then
      begin
        AddrNode := TASTNode.CreateWithValue(antProcAddress, UpperCase(VarToStr(InitExpr.Value)), NameTok);
        InitExpr.Free;
      end
      else
      begin
        AddrNode := TASTNode.Create(antProcAddress, NameTok);
        AddrNode.AddChild(InitExpr);
      end;
      Decl.AddChild(AddrNode);
      Decl.Attributes.Values['BYREF'] := '1';
    end
    else
      Decl.AddChild(InitExpr);                       // child[1] = initializer (NOT a type / dimensions)
    Decl.Attributes.Values['INFER'] := '1';
    if VarIsShared then Decl.Attributes.Values['SHARED'] := '1';   // module-global, as DIM SHARED marks it
    DoNodeCreated(Decl);
    Result.AddChild(Decl);
    if Context.Check(ttSeparParam) then
      Context.Advance                                // comma -> another inferred declaration
    else
      Break;
  until Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseStaticStatement: TASTNode;
// STATIC name AS type [= expr] [, ...] (FreeBASIC): declare locals with persistent storage that keeps
// its value across calls. Produced as an antDim with antArrayDecl children shaped like a DIM typed
// scalar (child[0]=name, child[1]=type, optional child[2]=initializer), each marked STATIC='1' so the
// SSA backs it with persistent storage and runs the initializer only once.
var
  Token, NameTok, TypeTok: TLexerToken;
  DeclNode, NameNode, TypeNd, Init, Dims: TASTNode;
  StaticTypeName, StaticFixedLen, StaticDottedName: string;
  StaticAddrNd: TASTNode;
  IsShared: Boolean;   // "Static Shared ...": both modifiers on one declaration
  IsByrefStatic: Boolean;  // ...and "Static Shared ByRef As T r = target"

  // "name(dims)" on a STATIC declaration: a procedure-local array with persistent storage. Returns the
  // antDimensions node (nil when the name is not followed by '('), leaving the caller to attach it as
  // child[1] — the DIM array shape (name, dimensions, type, [initializer]) that ProcessDim expects.
  function ParseStaticDims: TASTNode;
  begin
    Result := nil;
    if not Context.Check(ttDelimParOpen) then Exit;
    Context.Advance;                                 // (
    if Context.Check(ttDelimParClose) then
    begin
      Context.Advance;                               // ) -> "STATIC a()": empty, REDIM-sized later
      Result := TASTNode.Create(antDimensions);
      Exit;
    end;
    Result := ParseDimensionList;
    if not Assigned(Result) then
    begin
      HandleError('Expected dimension list', Context.CurrentToken);
      Exit;
    end;
    if not Context.Match(ttDelimParClose) then
    begin
      HandleError('Expected ")" after dimension list', Context.CurrentToken);
      FreeAndNil(Result);
    end;
  end;

  // "* n" after the type: a FIXED-LENGTH string capacity ("Static As ZString * 32 z"). Answers the
  // capacity as text for the FIXEDLEN attribute, or '' when there is none, exactly as the DIM parser
  // does it. ⛔ STATIC had no such step in EITHER of its two spellings, so the '*' ended the type,
  // the name after it was never read as a declaration, and "* 32 z" was parsed as a separate
  // statement - an antDeref of 32 followed by a bare call to Z. The declaration silently disappeared
  // and "@z" then named nothing.
  function ParseStaticFixedLen: string;
  var CapExpr: TASTNode; CapVal: Int64;
  begin
    Result := '';
    if not Context.Check(ttOpMul) then Exit;
    Context.Advance;                                 // '*'
    // ⛔ THE CAPACITY IS AN EXPRESSION, not one term. "Dim z As ZString * TOTLEN+1" is FreeBASIC's own
    // spelling (its test suite writes it), and reading a single term stopped at the '+': the rest of the
    // line was then parsed as a separate statement and the declaration silently lost its capacity.
    // precTerm takes '+' and '-' and everything tighter, and stops before a comma or a following NAME -
    // which is what keeps the leading-AS form ("Dim As String * 3 s") reading the name as the name.
    CapExpr := FExpressionParser.ParseExpression(precTerm);   // the capacity: an EXPRESSION
    if not Assigned(CapExpr) then Exit;
    if TryConstIntExpr(CapExpr, CapVal) then Result := IntToStr(CapVal)
    else Result := '-1';                             // present but non-constant -> advisory
    CapExpr.Free;
  end;

begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antDim, Token);
  Context.Advance;                                   // consume STATIC
  // ⭐ "STATIC SHARED ...": at module level FreeBASIC lets the two modifiers stand together, and it is
  // how a module variable is written when the source also wants to say "this storage persists". Both
  // words then mean the same thing here - a module variable already persists - so the SHARED is
  // consumed and the declaration goes on exactly as "STATIC ..." does. Left unread it was not an
  // identifier, and the whole statement died with "Expected a variable name after STATIC".
  IsShared := Context.Check(ttSharedDecl);
  if IsShared then Context.Advance;                  // consume SHARED
  // ...and BYREF may stand between them and the type: "Static Shared ByRef As Integer r = target" is a
  // module-level REFERENCE that persists, and DIM has accepted the same spelling all along. Left unread
  // it was not an identifier either, so the statement died with "Expected a variable name after STATIC".
  IsByrefStatic := Context.Check(ttParamMode) and (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'BYREF');
  if IsByrefStatic then Context.Advance;             // consume BYREF
  // FreeBASIC AS-first form: "STATIC AS type name1 [= init] [, name2 ...]" — the shared type precedes the
  // names (like "DIM AS type name"). Distinct from the "STATIC name AS type" form handled below.
  if Context.Check(ttAsType) then
  begin
    Context.Advance;                                 // AS
    SkipTypeQualifiers;                     // FB: "As Const <type>"
    if not Context.Check(ttIdentifier) then
    begin
      HandleError('Expected type name after AS', Context.CurrentToken);
      DoNodeCreated(Result); Exit;
    end;
    TypeTok := Context.CurrentToken;
    StaticTypeName := ParseDottedName;
    while AtPointerSuffix do
    begin StaticTypeName := StaticTypeName + ' PTR'; Context.Advance; end;
    StaticFixedLen := ParseStaticFixedLen;   // "Static As String * n a, b": one capacity, every name
    repeat
      if not Context.Check(ttIdentifier) then Break;
      NameTok := Context.CurrentToken;
      Context.Advance;                               // name
      DeclNode := TASTNode.Create(antArrayDecl, NameTok);
      DeclNode.AddChild(TASTNode.CreateWithValue(antIdentifier, UpperCase(NameTok.Value), NameTok));
      Dims := ParseStaticDims;                       // "STATIC AS type a(dims)": array with static storage
      if Assigned(Dims) then
      begin
        DeclNode.AddChild(Dims);                     // child[1] = dimensions (array shape)
        DeclNode.AddChild(TASTNode.CreateWithValue(antIdentifier, StaticTypeName, TypeTok));
        if Dims.ChildCount = 0 then DeclNode.Attributes.Values['VARLEN'] := '1';
        ParseOptionalArrayInit(DeclNode, Dims, NameTok);
      end
      else
      begin
        DeclNode.AddChild(TASTNode.CreateWithValue(antIdentifier, StaticTypeName, TypeTok));
        if Context.Check(ttOpEq) then
        begin
          Context.Advance;                           // =
          // ⭐ ...including the AGGREGATE TUPLE, "Static As T v = (a, b)" and "= ( { 1, 2 } )". The
          // grammar lived inline in DIM and nowhere else, so the very same declaration written with
          // STATIC parsed the parentheses as an expression and failed. One reader, both spellings.
          Init := TryParseAggregateTuple(StaticTypeName);
          if not Assigned(Init) then Init := FExpressionParser.ParseExpression;
          // A BYREF declaration binds to the referand's ADDRESS, so the initializer is wrapped in "@" -
          // the same shape DIM BYREF builds, so both spellings reach the SSA as one thing.
          if IsByrefStatic and Assigned(Init) then
          begin
            if Init.NodeType = antIdentifier then
            begin
              StaticAddrNd := TASTNode.CreateWithValue(antProcAddress, UpperCase(VarToStr(Init.Value)), NameTok);
              Init.Free;
            end
            else
            begin
              StaticAddrNd := TASTNode.Create(antProcAddress, NameTok);
              StaticAddrNd.AddChild(Init);
            end;
            Init := StaticAddrNd;
          end;
          if Assigned(Init) then DeclNode.AddChild(Init);
        end;
      end;
      if IsByrefStatic then DeclNode.Attributes.Values['BYREF'] := '1';
      DeclNode.Attributes.Values['STATIC'] := '1';
      if IsShared then DeclNode.Attributes.Values['SHARED'] := '1';
      if StaticFixedLen <> '' then DeclNode.Attributes.Values['FIXEDLEN'] := StaticFixedLen;
      DoNodeCreated(DeclNode);
      Result.AddChild(DeclNode);
      if Context.Check(ttSeparParam) then Context.Advance else Break;
    until Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]);
    DoNodeCreated(Result);
    Exit;
  end;
  repeat
    if not Context.Check(ttIdentifier) then
    begin
      HandleError('Expected a variable name after STATIC', Context.CurrentToken);
      while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile]) do
        Context.Advance;                             // recover: skip the bad statement (avoid a re-parse loop)
      Break;
    end;
    NameTok := Context.CurrentToken;
    StaticDottedName := UpperCase(VarToStr(NameTok.Value));
    Context.Advance;                                 // name
    // ⭐ "Static Shared UDT.g As Integer": the DEFINITION, outside the type, of a member declared
    // "Static g As Integer" inside it. The two halves must name the SAME storage, and a static member is
    // backed by a 1-element array called "TYPE.FIELD" (CollectStaticMembers) - so the dotted spelling is
    // folded into that one name here. Read as a single identifier the '.' was left behind and the
    // statement died as "STATIC requires a type".
    while Context.Check(ttOpDot) and Assigned(Context.PeekNext) and
          (Length(VarToStr(Context.PeekNext.Value)) > 0) and
          (UpCase(VarToStr(Context.PeekNext.Value)[1]) in ['A'..'Z', '_']) do
    begin
      Context.Advance;                               // '.'
      StaticDottedName := StaticDottedName + '.' + UpperCase(VarToStr(Context.CurrentToken.Value));
      Context.Advance;                               // segment
    end;
    Dims := ParseStaticDims;                         // "STATIC a(dims) AS type": array with static storage
    if not Context.Check(ttAsType) then
    begin
      HandleError('STATIC requires a type: STATIC name AS type', Context.CurrentToken);
      FreeAndNil(Dims);
      Break;
    end;
    Context.Advance;                                 // AS
    SkipTypeQualifiers;                     // FB: "As Const <type>"
    if not Context.Check(ttIdentifier) then
    begin
      HandleError('Expected type name after AS', Context.CurrentToken);
      Break;
    end;
    TypeTok := Context.CurrentToken;
    StaticTypeName := ParseDottedName;               // dotted: namespace-qualified type
    while AtPointerSuffix do
    begin
      StaticTypeName := StaticTypeName + ' PTR';
      Context.Advance;                               // consume PTR
    end;
    StaticFixedLen := ParseStaticFixedLen;           // "Static z As String * n"
    DeclNode := TASTNode.Create(antArrayDecl, NameTok);
    NameNode := TASTNode.CreateWithValue(antIdentifier, StaticDottedName, NameTok);
    TypeNd := TASTNode.CreateWithValue(antIdentifier, StaticTypeName, TypeTok);
    DeclNode.AddChild(NameNode);
    if Assigned(Dims) then
    begin
      DeclNode.AddChild(Dims);                       // child[1] = dimensions (array shape)
      DeclNode.AddChild(TypeNd);                     // child[2] = element type
      if Dims.ChildCount = 0 then DeclNode.Attributes.Values['VARLEN'] := '1';
      ParseOptionalArrayInit(DeclNode, Dims, NameTok);
    end
    else
    begin
      DeclNode.AddChild(TypeNd);                     // child[1] = type (typed scalar)
      if Context.Check(ttOpEq) then
      begin
        Context.Advance;                             // =
        Init := FExpressionParser.ParseExpression;
        // ⛔ The BYREF wrapping belongs to BOTH spellings. "Static ByRef As T r = x" was taught it and
        // "Static ByRef r As T = x" was not, so the name-first form declared an ordinary variable and
        // read 0 - the same rule in one path and not its sibling, one edit later in the same routine.
        if IsByrefStatic and Assigned(Init) then
        begin
          if Init.NodeType = antIdentifier then
          begin
            StaticAddrNd := TASTNode.CreateWithValue(antProcAddress, UpperCase(VarToStr(Init.Value)), NameTok);
            Init.Free;
          end
          else
          begin
            StaticAddrNd := TASTNode.Create(antProcAddress, NameTok);
            StaticAddrNd.AddChild(Init);
          end;
          Init := StaticAddrNd;
        end;
        if Assigned(Init) then
          DeclNode.AddChild(Init);                   // child[2] = once-only initializer expression
      end;
    end;
    if IsByrefStatic then DeclNode.Attributes.Values['BYREF'] := '1';
    DeclNode.Attributes.Values['STATIC'] := '1';
    if IsShared then DeclNode.Attributes.Values['SHARED'] := '1';
    if StaticFixedLen <> '' then DeclNode.Attributes.Values['FIXEDLEN'] := StaticFixedLen;
    DoNodeCreated(DeclNode);
    Result.AddChild(DeclNode);
    if Context.Check(ttSeparParam) then
      Context.Advance                                // comma -> another static declaration
    else
      Break;
  until Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseEraseStatement: TASTNode;
// ERASE arr [, arr ...] (FreeBASIC, B1.4) - reset each named array's elements to default.
var
  Token, NameTok: TLexerToken;
  EraseTarget: TASTNode;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antErase, Token);
  Context.Advance; // Consume ERASE
  repeat
    // ⭐ "Erase .field" INSIDE A WITH BLOCK, the same leading dot REDIM reads a few pages above and an
    // assignment reads everywhere: it names a member of the WITH object. Only a bare NAME was accepted,
    // so the dot ended the statement - a third statement missing the rule its siblings have.
    // ⛔ ...and the QUALIFIED spelling, "Erase obj.arr", which is the same member with its object written
    // out. Fixing only the leading dot would have left the two halves of one rule in different states -
    // "Erase e.meep" read "e" as the array and died as "ERASE: array not declared: E".
    if Context.Check(ttOpDot) or
       (Context.Check(ttIdentifier) and Assigned(Context.PeekNext) and
        (Context.PeekNext.TokenType = ttOpDot)) then
    begin
      // ⛔ At precCALL, not precPRIMARY: the ".field" postfix is what has to be read, and at primary the
      // expression parser stopped at the bare name and left the dot behind - the statement then held the
      // OBJECT where the array belonged ("ERASE: array not declared: E"). REDIM reads its own target at
      // primary for the opposite reason: there the "(...)" that follows is its dimension list.
      EraseTarget := FExpressionParser.ParseExpression(precCall);
      if not Assigned(EraseTarget) then
      begin
        HandleError('Expected a member name after "." in ERASE', Context.CurrentToken);
        Break;
      end;
      Result.AddChild(EraseTarget);
      if Context.Check(ttSeparParam) then begin Context.Advance; Continue; end;
      Break;
    end;
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
// Also the leading-AS form "REDIM [PRESERVE] AS type arr(dims)[, ...]" (the type precedes the names and
// applies to each); REDIM on an undeclared array acts as a declaration (ProcessRedim synthesizes a DIM),
// so the shared type must reach it — injected as the array-decl's type child, like leading-AS DIM.
var
  Token, RedimTypeTok: TLexerToken;
  ArrayDecl: TASTNode;
  RedimLeadingAS, RedimShared: Boolean;
  RedimTypeName: string;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antRedim, Token);
  Context.Advance; // Consume REDIM
  // Optional PRESERVE and SHARED modifiers (in either order). SHARED (module-global, like DIM SHARED)
  // makes a REDIM-as-declaration a shared array; PRESERVE keeps the overlapping elements on resize.
  RedimShared := False;
  while True do
  begin
    if Context.Check(ttIdentifier) and (UpperCase(Context.CurrentToken.Value) = 'PRESERVE') then
    begin
      Result.Attributes.Values['PRESERVE'] := '1';
      Context.Advance;
    end
    else if Context.Check(ttSharedDecl) then
    begin
      RedimShared := True;
      Context.Advance;
    end
    else
      Break;
  end;
  // Leading-AS shared type: "REDIM AS type name(dims)".
  RedimLeadingAS := Context.Check(ttAsType);
  RedimTypeName := '';
  RedimTypeTok := Token;
  if RedimLeadingAS then
  begin
    Context.Advance;                          // AS
    SkipTypeQualifiers;                     // FB: "As Const <type>"
    if Context.Check(ttIdentifier) then
    begin
      RedimTypeTok := Context.CurrentToken;
      RedimTypeName := UpperCase(ParseDottedName);
      while AtPointerSuffix do
      begin RedimTypeName := RedimTypeName + ' PTR'; Context.Advance; end;
    end;
  end;
  repeat
    ArrayDecl := ParseArrayDeclaration;  // name(dims) [AS type]
    if Assigned(ArrayDecl) then
    begin
      // Inject the leading-AS shared type unless the array already carries an explicit element type.
      if RedimLeadingAS and (RedimTypeName <> '') and
         ((ArrayDecl.ChildCount < 3) or (ArrayDecl.GetChild(2).NodeType <> antIdentifier)) then
        ArrayDecl.InsertChild(2, TASTNode.CreateWithValue(antIdentifier, RedimTypeName, RedimTypeTok));
      if RedimShared then ArrayDecl.Attributes.Values['SHARED'] := '1';   // REDIM SHARED -> module-global
      Result.AddChild(ArrayDecl);
    end
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
  // Optional enum type name, then an optional "AS <underlying-type>" (FreeBASIC "Enum [name] [As Integer]").
  // The underlying type is advisory here — enum members are plain integer constants regardless of the
  // declared width. The NAME is not: FreeBASIC lets a member be reached through it ("MyEnum.option1"), so
  // keep it on the node. It may be a reserved word (a type/colour keyword), so don't require ttIdentifier;
  // exclude AS so a nameless "Enum As Integer" is not mistaken for a name.
  if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttSeparParam, ttEndOfFile, ttAsType]) then
  begin
    Result.Value := UpperCase(VarToStr(Context.CurrentToken.Value));   // enum type name
    Context.Advance;
    // FreeBASIC "Enum <name> Explicit": the members are reachable ONLY through the enum's name. Left
    // unconsumed, the word was read as the FIRST MEMBER and the real members stayed plain globals - so
    // an explicit enum's B shadowed the B of an ordinary one declared beside it.
    if Context.Check(ttIdentifier) and
       (UpperCase(VarToStr(Context.CurrentToken.Value)) = 'EXPLICIT') then
    begin
      Result.Attributes.Values['EXPLICIT'] := '1';
      Context.Advance;
    end;
  end;
  if Context.Check(ttAsType) then
  begin
    Context.Advance;                            // AS
    SkipTypeQualifiers;                     // FB: "As Const <type>"
    if not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttSeparParam, ttEndOfFile]) then
      Context.Advance;                          // underlying integer type name
  end;
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
  // The SPECIFIC type matters beyond the bank: Len/SizeOf of a def-typed variable report the
  // declared width (DefLng x -> Len(x) = 4, not 8). Carried as an attribute for the SSA.
  if KwU = 'DEFLNG' then Result.Attributes.Values['TYPENAME'] := 'LONG'
  else if KwU = 'DEFSNG' then Result.Attributes.Values['TYPENAME'] := 'SINGLE'
  else if KwU = 'DEFDBL' then Result.Attributes.Values['TYPENAME'] := 'DOUBLE'
  else if KwU = 'DEFBYTE' then Result.Attributes.Values['TYPENAME'] := 'BYTE'
  else if KwU = 'DEFSHORT' then Result.Attributes.Values['TYPENAME'] := 'SHORT'
  else if KwU = 'DEFUBYTE' then Result.Attributes.Values['TYPENAME'] := 'UBYTE'
  else if KwU = 'DEFUSHORT' then Result.Attributes.Values['TYPENAME'] := 'USHORT'
  else if KwU = 'DEFLNGINT' then Result.Attributes.Values['TYPENAME'] := 'LONGINT'
  else if KwU = 'DEFUINT' then Result.Attributes.Values['TYPENAME'] := 'UINTEGER'
  else if KwU = 'DEFSTR' then Result.Attributes.Values['TYPENAME'] := 'STRING'
  else Result.Attributes.Values['TYPENAME'] := 'INTEGER';   // DEFINT
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
  Token, NameTok, TypeTok: TLexerToken;
  Assignment, ValueNode, ArrayDecl, ProcPtrScratch: TASTNode;
  TypeName: string;

  // Best-effort bank of an untyped CONST initializer that is NOT a plain literal — a string-returning
  // intrinsic (Chr, Left, Str, ...), a '$'-suffixed name/call, or a concatenation of such. Without this a
  // "Const lr = Chr(188)" defaults to DOUBLE: a float register holding a string, which later flows through
  // a cross-bank CopyFloat (and a CopyFloat over a string PHI at an IIF merge) — a latent mismatch the
  // optimizer's register reassignment can turn into a wrong-register read.
  function ValueIsString(N: TASTNode): Boolean;
  var Nm: string;
  begin
    Result := False;
    if N = nil then Exit;
    case N.NodeType of
      antLiteral: Result := VarIsStr(N.Value);
      antIdentifier:
        begin
          Nm := UpperCase(VarToStr(N.Value));
          Result := (Nm <> '') and (Nm[Length(Nm)] = '$');
        end;
      antArrayAccess, antFunctionCall:
        begin
          // ⛔ THE TWO NODE SHAPES KEEP THE NAME IN DIFFERENT PLACES: antArrayAccess in child 0,
          // antFunctionCall in its own Value. Only child 0 was read, so every intrinsic that parses as
          // a FUNCTION CALL failed the test - "Const w = WChr(65,66,67)" was typed DOUBLE and the string
          // went into a float register, which is why it came back one character long. The list already
          // named WCHR; the name simply never reached it.
          Nm := UpperCase(VarToStr(N.Value));
          if (Nm = '') and (N.ChildCount >= 1) and (N.GetChild(0).NodeType = antIdentifier) then
            Nm := UpperCase(VarToStr(N.GetChild(0).Value));
          Result := ((Nm <> '') and (Nm[Length(Nm)] = '$')) or
                    (Nm = 'CHR') or (Nm = 'MID') or (Nm = 'LEFT') or (Nm = 'RIGHT') or
                    (Nm = 'STRING') or (Nm = 'SPACE') or (Nm = 'STR') or (Nm = 'HEX') or
                    (Nm = 'OCT') or (Nm = 'BIN') or (Nm = 'TRIM') or (Nm = 'LTRIM') or
                    (Nm = 'RTRIM') or (Nm = 'UCASE') or (Nm = 'LCASE') or (Nm = 'WCHR') or
                    (Nm = 'WSTR') or (Nm = 'WSPACE') or (Nm = 'CSTR') or (Nm = 'FORMAT');
        end;
      antBinaryOp:
        Result := (N.ChildCount >= 2) and (ValueIsString(N.GetChild(0)) or ValueIsString(N.GetChild(1)));
    end;
  end;

  // Bank of an untyped CONST from its initializer: string literal / string-valued expression -> STRING;
  // numeric literal with fraction or exponent -> DOUBLE; other numeric literal -> LONGINT; unknown -> DOUBLE.
  function InferConstTypeName(V: TASTNode): string;
  // The type a CONST takes from its VALUE. ⛔ It used to answer DOUBLE for anything that was not a
  // bare literal, so "Const TMASK = TSIZE - 1" was a Double even though both sides are integers -
  // and every use of it dragged the surrounding expression into floating point. In k-nucleotide's
  // open-addressing probe, the innermost loop of the program, "p = (p + 1) And TMASK" came out as
  // AddInt, IntToFloat, FloatToInt, BitwiseAnd. fbc types that CONST as an integer, and so do we now.
  var
    L, R: string;
  begin
    Result := 'DOUBLE';
    if V = nil then Exit;
    if V.NodeType = antLiteral then
    begin
      if VarIsStr(V.Value) then
        Result := 'STRING'
      else if (Pos('.', VarToStr(V.Value)) > 0) or
              (Pos('E', UpperCase(VarToStr(V.Value))) > 0) then
        Result := 'DOUBLE'
      else
        Result := 'LONGINT';
      Exit;
    end;
    if ValueIsString(V) then Exit('STRING');
    // A CONST already declared above this one answers with the type IT was given.
    if V.NodeType = antIdentifier then
    begin
      R := FConstTypes.Values[UpperCase(VarToStr(V.Value))];
      if R <> '' then Result := R;
      Exit;
    end;
    if (V.NodeType = antUnaryOp) and (V.ChildCount >= 1) then
      Exit(InferConstTypeName(V.GetChild(0)));
    if (V.NodeType = antBinaryOp) and (V.ChildCount >= 2) and (V.Token <> nil) then
    begin
      // ⛔ "/" is FreeBASIC's FLOATING division whatever its operands are: "Const HALF = 1 / 2" is
      // 0.5, not 0. Only the operators whose result is an integer when both sides are keep LONGINT.
      case V.Token.TokenType of
        ttOpAdd, ttOpSub, ttOpMul, ttOpIntDiv, ttOpMod,
        ttBitwiseAND, ttBitwiseOR, ttBitwiseXOR, ttOpShl, ttOpShr:
          begin
            L := InferConstTypeName(V.GetChild(0));
            R := InferConstTypeName(V.GetChild(1));
            if (L = 'STRING') or (R = 'STRING') then Result := 'STRING'
            else if (L = 'LONGINT') and (R = 'LONGINT') then Result := 'LONGINT'
            else Result := 'DOUBLE';
          end;
      end;
    end;
  end;

  // Comma-separated continuation of a CONST list: ", name [As type] = value, ...". Shared by all three
  // CONST spellings: the leading-AS form passes the list-wide type (a per-item AS is not part of that
  // syntax), the name-first and bare forms allow a per-item AS and fall back to per-value bank inference
  // when it is absent (fbc-verified: the type of one item does NOT carry over to the next). Without this
  // the typed forms lowered only the FIRST constant and everything after the comma was re-parsed as a
  // plain assignment (same defect the bare form had, m392).
  procedure ParseConstListTail(DimNode: TASTNode; const ListTypeName: string; AllowItemType: Boolean);
  var
    ItemName, ItemTypeTok: TLexerToken;
    ItemType: string;
    ItemValue, Decl: TASTNode;
  begin
    while Context.Check(ttSeparParam) do
    begin
      Context.Advance;                                // ','
      if not Context.Check(ttIdentifier) then
      begin
        HandleError('Expected constant name after "," in CONST list', Context.CurrentToken);
        Exit;
      end;
      ItemName := Context.CurrentToken;
      Context.Advance;                                // name
      ItemType := '';
      ItemTypeTok := ItemName;
      if AllowItemType and Context.Check(ttAsType) then
      begin
        Context.Advance;                              // AS
        SkipTypeQualifiers;                     // FB: "As Const <type>"
        ItemTypeTok := Context.CurrentToken;
        ItemType := 'INTEGER';
        if Context.Check(ttIdentifier) then
        begin
          ItemType := UpperCase(ParseDottedName);     // element type
          // Optional pointer suffix: the "PTR" keyword (repeated for multi-level "T Ptr Ptr") or the "*" form.
          while AtPointerSuffix or
                Context.Check(ttOpMul) do
          begin Context.Advance; ItemType := ItemType + ' PTR'; end;
        end;
      end;
      if not Context.Match(ttOpEq) then
      begin
        HandleError('Expected "=" after CONST name', Context.CurrentToken);
        Exit;
      end;
      ItemValue := ParseExpression;
      if not Assigned(ItemValue) then
      begin
        HandleError('Expected value after CONST =', Context.CurrentToken);
        Exit;
      end;
      if ItemType = '' then
      begin
        if ListTypeName <> '' then
          ItemType := ListTypeName
        else
          ItemType := InferConstTypeName(ItemValue);
      end;
      Decl := TASTNode.Create(antArrayDecl, ItemName);
      Decl.AddChild(TASTNode.CreateWithValue(antIdentifier, UpperCase(ItemName.Value), ItemName));
      Decl.AddChild(TASTNode.CreateWithValue(antIdentifier, ItemType, ItemTypeTok));
      Decl.AddChild(ItemValue);
      if FModernMode then Decl.Attributes.Values['SHARED'] := '1';
      DimNode.AddChild(Decl);
    end;
  end;

begin
  // FreeBASIC CONST METHOD DEFINITION: "Const Sub|Function|Operator|Property T.m(...)". The qualifier
  // sits in FRONT of the procedure keyword, so the statement opens with CONST and is not a constant
  // declaration at all - it was read as one and died on "Expected variable name in assignment". Same
  // move STATIC already makes for "Static Sub T.m()": consume the qualifier and let the procedure
  // grammar read the rest. CONST on a method is a promise about THIS, and this VM does not enforce it
  // (as it does not enforce "As Const" on a variable), so nothing else is needed for the definition to
  // mean what it means.
  if Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttProcedureStart) then
  begin
    Context.Advance;                                   // consume CONST
    Exit(ParseProcedureDecl);
  end;
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antConst, Token);
  Context.Advance; // Consume CONST
  // ⭐ A MODERN extension FreeBASIC does NOT reserve may be a CONSTANT'S NAME. The rule already
  // existed for a PROCEDURE name (IsShadowableExtensionName at ParseProcedureDecl) and for a
  // statement that begins with one; a CONST had no such door, so "Const MAX = 8" - which fbc
  // compiles - failed the whole file to parse. ⚠️ ABS/FIX/SGN stay reserved, as they are in fbc.
  if FModernMode and (not Context.Check(ttIdentifier)) and
     IsShadowableExtensionName(UpperCase(VarToStr(Context.CurrentToken.Value))) then
    Context.CurrentToken.TokenType := ttIdentifier;

  // FreeBASIC leading-AS typed constant: "CONST AS type name = value" (e.g. "Const As UInteger
  // children = 100"). The alternate spelling of the name-first form below; same lowering to a typed
  // scalar DIM so it reuses the full typing + initialization path.
  if Context.Check(ttAsType) then
  begin
    Context.Advance;                                  // AS
    SkipTypeQualifiers;                     // FB: "As Const <type>"
    TypeTok := Context.CurrentToken;
    TypeName := 'INTEGER';
    if Context.Check(ttIdentifier) then
    begin
      TypeName := UpperCase(ParseDottedName);         // element type
      // Optional pointer suffix: the "PTR" keyword (repeated for multi-level "T Ptr Ptr") or the "*" form.
      while AtPointerSuffix or
            Context.Check(ttOpMul) do
      begin Context.Advance; TypeName := TypeName + ' PTR'; end;
    end;
    if not Context.Check(ttIdentifier) then
    begin
      HandleError('Expected constant name after CONST AS type', Context.CurrentToken);
      Result.Free; Result := nil; Exit;
    end;
    NameTok := Context.CurrentToken;
    Context.Advance;                                  // name
    if not Context.Match(ttOpEq) then
    begin
      HandleError('Expected "=" after CONST name', Context.CurrentToken);
      Result.Free; Result := nil; Exit;
    end;
    ValueNode := ParseExpression;
    if not Assigned(ValueNode) then
    begin
      HandleError('Expected value after CONST =', Context.CurrentToken);
      Result.Free; Result := nil; Exit;
    end;
    Result.Free;                                      // discard the antConst; emit a typed DIM instead
    ArrayDecl := TASTNode.Create(antArrayDecl, NameTok);
    ArrayDecl.AddChild(TASTNode.CreateWithValue(antIdentifier, UpperCase(NameTok.Value), NameTok));
    ArrayDecl.AddChild(TASTNode.CreateWithValue(antIdentifier, TypeName, TypeTok));
    ArrayDecl.AddChild(ValueNode);
    if FModernMode then ArrayDecl.Attributes.Values['SHARED'] := '1';     // FB: a module-level CONST is globally visible
    ArrayDecl.Attributes.Values['CONSTDECL'] := '1';  // a CONST, not a variable: the SSA folds it to an immediate
    if FConstNames.IndexOf(UpperCase(VarToStr(ArrayDecl.GetChild(0).Value))) < 0 then
      FConstNames.Add(UpperCase(VarToStr(ArrayDecl.GetChild(0).Value)));
    FConstTypes.Values[UpperCase(VarToStr(ArrayDecl.GetChild(0).Value))] :=
      UpperCase(VarToStr(ArrayDecl.GetChild(1).Value));
    if (ArrayDecl.ChildCount >= 3) and TryConstIntExpr(ArrayDecl.GetChild(2), FConstFoldVal) then
      FConstIntValues.Values[UpperCase(VarToStr(ArrayDecl.GetChild(0).Value))] := IntToStr(FConstFoldVal);
    Result := TASTNode.Create(antDim, Token);
    Result.AddChild(ArrayDecl);
    ParseConstListTail(Result, TypeName, False);      // "Const As T a = 1, b = 2, ...": T applies to the whole list
    DoNodeCreated(Result);
    Exit;
  end;

  // FreeBASIC typed constant: "CONST name AS type = value". Rewrite it into a typed scalar DIM
  // (antDim -> antArrayDecl(name, typeIdent, valueExpr)) so it reuses the full typing + initialization
  // path -- a plain assignment to a suffixless name would default to a numeric bank and drop a string
  // value. (CONST is treated as an initialized typed variable; immutability is not enforced here.)
  if Context.Check(ttIdentifier) and Assigned(Context.PeekNext) and (Context.PeekNext.TokenType = ttAsType) then
  begin
    NameTok := Context.CurrentToken;
    Context.Advance;                                  // name
    Context.Advance;                                  // AS
    SkipTypeQualifiers;                     // FB: "As Const <type>"
    TypeTok := Context.CurrentToken;
    TypeName := 'INTEGER';
    // "Const p As Sub() = 0" / "As Function(...) As R": the type is a PROCEDURE POINTER, which is not an
    // identifier at all - so the reader below skipped it, the type stayed INTEGER and the '(' was met
    // where '=' was expected. DIM has read this signature all along; a constant holds only the pointer
    // VALUE (int-banked here), so the signature is consumed and its shape recorded on a scratch node.
    if Context.Check(ttProcedureStart) then
    begin
      ProcPtrScratch := TASTNode.Create(antArrayDecl, TypeTok);
      try
        TryParseProcPtrType(ProcPtrScratch);
      finally
        ProcPtrScratch.Free;
      end;
    end
    else if Context.Check(ttIdentifier) then
    begin
      TypeName := UpperCase(ParseDottedName);         // element type
      // Optional pointer suffix: the "PTR" keyword (repeated for multi-level "T Ptr Ptr") or the "*" form.
      while AtPointerSuffix or
            Context.Check(ttOpMul) do
      begin Context.Advance; TypeName := TypeName + ' PTR'; end;
    end;
    if not Context.Match(ttOpEq) then
    begin
      HandleError('Expected "=" after CONST type', Context.CurrentToken);
      Result.Free; Result := nil; Exit;
    end;
    ValueNode := ParseExpression;
    if not Assigned(ValueNode) then
    begin
      HandleError('Expected value after CONST =', Context.CurrentToken);
      Result.Free; Result := nil; Exit;
    end;
    Result.Free;                                      // discard the antConst; emit a typed DIM instead
    ArrayDecl := TASTNode.Create(antArrayDecl, NameTok);
    ArrayDecl.AddChild(TASTNode.CreateWithValue(antIdentifier, UpperCase(NameTok.Value), NameTok));
    ArrayDecl.AddChild(TASTNode.CreateWithValue(antIdentifier, TypeName, TypeTok));
    ArrayDecl.AddChild(ValueNode);
    if FModernMode then ArrayDecl.Attributes.Values['SHARED'] := '1';     // FB: a module-level CONST is globally visible
    ArrayDecl.Attributes.Values['CONSTDECL'] := '1';  // a CONST, not a variable: the SSA folds it to an immediate
    if FConstNames.IndexOf(UpperCase(VarToStr(ArrayDecl.GetChild(0).Value))) < 0 then
      FConstNames.Add(UpperCase(VarToStr(ArrayDecl.GetChild(0).Value)));
    FConstTypes.Values[UpperCase(VarToStr(ArrayDecl.GetChild(0).Value))] :=
      UpperCase(VarToStr(ArrayDecl.GetChild(1).Value));
    if (ArrayDecl.ChildCount >= 3) and TryConstIntExpr(ArrayDecl.GetChild(2), FConstFoldVal) then
      FConstIntValues.Values[UpperCase(VarToStr(ArrayDecl.GetChild(0).Value))] := IntToStr(FConstFoldVal);
    Result := TASTNode.Create(antDim, Token);
    Result.AddChild(ArrayDecl);
    ParseConstListTail(Result, '', True);             // "Const a As T = 1, b As U = 2, ...": per-item type or inference
    DoNodeCreated(Result);
    Exit;
  end;

  // Bare "CONST name = value" (untyped). Lower it to a SHARED typed scalar DIM — like the typed forms
  // above — so the constant is visible module-wide, INCLUDING inside SUB/FUNCTION bodies (FreeBASIC
  // module-level constants are global; without SHARED the synthesized module DIM would be invisible to
  // procedures, so e.g. "Const pi = 3.14159" used inside a function read as 0). The bank is inferred from
  // the value: a string literal -> STRING; a numeric literal with a fractional part or exponent -> DOUBLE;
  // any other numeric literal -> LONGINT; a non-literal value expression -> DOUBLE.
  FInConstDecl := True;
  try
    Assignment := ParseAssignmentStatement;
  finally
    FInConstDecl := False;
  end;
  if not (Assigned(Assignment) and (Assignment.NodeType = antAssignment) and (Assignment.ChildCount >= 2)) then
  begin
    HandleError('Expected assignment after CONST', Token);
    if Assigned(Assignment) then Assignment.Free;
    Result.Free;
    Result := nil;
    Exit;
  end;
  ValueNode := Assignment.GetChild(1).Clone;          // the constant value (assignment freed below)
  TypeName := InferConstTypeName(ValueNode);
  NameTok := Assignment.GetChild(0).Token;
  Result.Free;                                        // discard the antConst; emit a typed SHARED DIM
  ArrayDecl := TASTNode.Create(antArrayDecl, NameTok);
  ArrayDecl.AddChild(TASTNode.CreateWithValue(antIdentifier, UpperCase(VarToStr(Assignment.GetChild(0).Value)), NameTok));
  ArrayDecl.AddChild(TASTNode.CreateWithValue(antIdentifier, TypeName, NameTok));
  ArrayDecl.AddChild(ValueNode);
  // Only MODERN (FreeBASIC) needs a module-level CONST to be a SHARED global for procedure visibility.
  // CLASSIC (line-numbered Commodore BASIC) has no separate procedure scope and its execution model does
  // not use the shared-scalar backing array; marking it SHARED there breaks the const (m: stress.bas).
  if FModernMode then ArrayDecl.Attributes.Values['SHARED'] := '1';
  ArrayDecl.Attributes.Values['CONSTDECL'] := '1';    // a CONST, not a variable: the SSA folds it to an immediate
  if FConstNames.IndexOf(UpperCase(VarToStr(ArrayDecl.GetChild(0).Value))) < 0 then
    FConstNames.Add(UpperCase(VarToStr(ArrayDecl.GetChild(0).Value)));
  FConstTypes.Values[UpperCase(VarToStr(ArrayDecl.GetChild(0).Value))] :=
    UpperCase(VarToStr(ArrayDecl.GetChild(1).Value));
  if (ArrayDecl.ChildCount >= 3) and TryConstIntExpr(ArrayDecl.GetChild(2), FConstFoldVal) then
    FConstIntValues.Values[UpperCase(VarToStr(ArrayDecl.GetChild(0).Value))] := IntToStr(FConstFoldVal);
  Assignment.Free;
  Result := TASTNode.Create(antDim, Token);
  Result.AddChild(ArrayDecl);
  // Multi-constant list: "CONST a = v1, b = v2, ...". Each further comma-separated constant gets the SAME
  // lowering (typed scalar DIM, per-value bank inference, SHARED in MODERN). Previously everything after
  // the first constant was left in the token stream and re-parsed as a PLAIN assignment — an untyped,
  // non-shared module variable: not constant, invisible inside procedures, and bank-inferred from the
  // NAME (float default) instead of the value, so "Const hb = Chr(205), vb = Chr(186)" left hb/vb as
  // floats silently printing 0 (m392). A later item may carry its own "As type" (form 1 of the syntax).
  ParseConstListTail(Result, '', True);
  DoNodeCreated(Result);
end;

function TPackratParser.ParseDataStatement: TASTNode;
var
  Token: TLexerToken;
  DataItem, ExprNode: TASTNode;
  SavedIdx: Integer;
  FoldedVal: Variant;
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

  // Parse comma-separated list of data items.
  // CLASSIC: literals only - "DATA 5,12,1,34,18", "DATA "hello","world"", "DATA COMMODORE,128", where an
  // unquoted word is a string. That is the v7 rule and it stays: in Commodore BASIC a DATA item is the raw
  // text up to the comma, so "435/4" IS the three-character-plus string, not a quotient.
  // MODERN: FreeBASIC's DATA takes "expressions that are evaluated at compile time" (its own manual page),
  // which its own example uses - "Data 3, 234, 435/4, 23+433, 87643, "Good" + "Bye!"". Try to fold one
  // first; only if that fails does the literal grammar below run, so every form that worked still works.
  while not Context.CheckAny([ttEndOfLine, ttSeparStmt, ttEndOfFile, ttConditionalElse]) do
  begin
    if FModernMode then
    begin
      SavedIdx := Context.CurrentIndex;
      ExprNode := ParseExpression;
      if Assigned(ExprNode) and TryConstDataExpr(ExprNode, FoldedVal) then
      begin
        DataItem := TASTNode.CreateWithValue(antLiteral, FoldedVal, Token);
        Result.AddChild(DataItem);
        ExprNode.Free;
        if Context.Check(ttSeparParam) then begin Context.Advance; Continue; end
        else Break;
      end;
      // Not a foldable expression (a bare word, say): put the stream back exactly where it was and let
      // the literal grammar have it. Rewinding is what makes this addition unable to LOSE an item.
      if Assigned(ExprNode) then ExprNode.Free;
      Context.CurrentIndex := SavedIdx;
    end;
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
    // ⭐ ...and an EXPLICIT PLUS. "Data 0, +0.0" is FreeBASIC's own way of writing the positive zero
    // beside the negative one, and its test suite does exactly that (tests/boolean/boolean_data). Only
    // the MINUS was read here, so the '+' ended the DATA statement and everything after it on the line
    // was lost - the item count then no longer matched the READs.
    else if Context.Check(ttOpAdd) then
    begin
      Context.Advance;                                 // consume '+'
      if Context.Check(ttNumber) or Context.Check(ttInteger) or Context.Check(ttFloat) then
      begin
        DataItem := TASTNode.CreateWithValue(antLiteral, Context.CurrentToken.Value, Context.CurrentToken);
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
  VarNode, MemberNode: TASTNode;
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
      // Fold a ".field" chain so a READ target can be a UDT member: "READ obj.x", "READ a(i).y".
      // Each "." wraps the current target as the object child of an antMemberAccess (like the REDIM
      // target chain in ParseArrayDeclaration); the SSA reads a DATA item into a temp and stores it.
      while Context.Check(ttOpDot) do
      begin
        Context.Advance;                              // '.'
        if not Context.Check(ttIdentifier) then
        begin
          HandleError('Expected field name after "." in READ target', Context.CurrentToken);
          Break;
        end;
        MemberNode := TASTNode.CreateWithValue(antMemberAccess, UpperCase(Context.CurrentToken.Value),
                                               Context.CurrentToken);
        MemberNode.AddChild(VarNode);
        VarNode := MemberNode;
        Context.Advance;                              // field name
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

  // Optional target: "RESTORE 100" (v7 line number) or "RESTORE label" (FreeBASIC). Both name the DATA
  // item to start reading from again; the SSA resolves either to a POOL INDEX.
  // The label form was not parsed at all: the word after RESTORE was left behind as a statement of its
  // own - a bare identifier, which is silently ignored - and the RESTORE reset to the first item. So
  // "Restore second" read block ONE and said nothing. Kept as an antIdentifier child so ProcessRestore
  // can tell the two forms apart without re-reading the token.
  if Context.Check(ttNumber) or Context.Check(ttInteger) then
  begin
    LineNode := TASTNode.CreateWithValue(antLiteral, Context.CurrentToken.Value, Context.CurrentToken);
    Result.AddChild(LineNode);
    Context.Advance;
  end
  else if Context.Check(ttIdentifier) then
  begin
    LineNode := TASTNode.CreateWithValue(antIdentifier, Context.CurrentToken.Value, Context.CurrentToken);
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
// WHILE <cond> ... WEND. Desugared at parse time into the same antDoLoop node that
// "DO WHILE <cond> ... LOOP" produces (top-tested WHILE condition), so it reuses the
// fully-supported DO/LOOP lowering in both the SSA compiler and the tree executor
// (the antWhileLoop node is only handled by the tree executor, not the SSA pipeline).
var
  Condition, Body: TASTNode;
  Token: TLexerToken;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antDoLoop, Token);
  Context.Advance; // Consume WHILE

  // Parse condition (top-tested, like DO WHILE)
  Condition := ParseExpression;
  if not Assigned(Condition) then
  begin
    Result.Free;
    Result := nil;
    Exit;
  end;

  // Parse body until matching WEND (registered as ttLoopBlockEnd, like LOOP/NEXT).
  // ParseLoopBody is nesting-aware: nested flat FOR...NEXT loops are consumed rather
  // than mistaken for our terminator.
  Body := ParseLoopBody;
  if Assigned(Body) then
    Result.AddChild(Body);

  // Consume the closing WEND. ParseWhileStatement manages it directly (like DO/LOOP),
  // so no validation-stack push/pop is involved.
  if Context.Check(ttLoopBlockEnd) and (UpperCase(Context.CurrentToken.Value) = kWEND) then
    Context.Advance;

  // antDoLoop layout: child 0 = body, child 1 = condition; metadata in attributes.
  Result.AddChild(Condition);
  Result.Attributes.Values['ConditionType'] := 'WHILE';
  Result.Attributes.Values['ConditionPosition'] := 'TOP';

  DoNodeCreated(Result);
end;

function TPackratParser.ParseOnStatement: TASTNode;
var
  Expression, Target: TASTNode;
  Token: TLexerToken;
  IsLocal: Boolean;
begin
  Token := Context.CurrentToken;
  Result := TASTNode.Create(antStatement, Token);
  Context.Advance; // Consume ON

  // FreeBASIC/QB error handling: ON [LOCAL] ERROR GOTO <label|0>.
  // LOCAL is accepted and treated as a global handler in v1 (no per-procedure scoping).
  // Detected by token value (ERROR/LOCAL are not reserved keywords).
  IsLocal := False;
  if UpperCase(Context.CurrentToken.Value) = 'LOCAL' then
  begin
    IsLocal := True;
    Context.Advance; // consume LOCAL
  end;
  if UpperCase(Context.CurrentToken.Value) = kERROR then
  begin
    Context.Advance; // consume ERROR
    Result.Free;
    Result := TASTNode.Create(antOnError, Token);
    if IsLocal then
      Result.Value := 'LOCAL';
    // Expect GOTO (matched by value to be robust to token classification)
    if UpperCase(Context.CurrentToken.Value) = kGOTO then
    begin
      Context.Advance; // consume GOTO
      Target := ParseExpression;  // label identifier, or line number (0 disables)
      if Assigned(Target) then
        Result.AddChild(Target);
    end
    else
    begin
      HandleError('Expected GOTO after ON ERROR', Context.CurrentToken);
      Result.Free;
      Result := nil;
      Exit;
    end;
    DoNodeCreated(Result);
    Exit;
  end;

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

function TPackratParser.ParseLoopBody: TASTNode;
// Parses the body of a DO/LOOP or WHILE/WEND, stopping at the matching LOOP/WEND.
//
// DO and WHILE build a nested body (this block), but FOR uses a FLAT representation:
// ParseForStatement emits just the antForLoop header (pushing a validation entry) and the
// matching NEXT is a sibling statement consumed later by ParseLoopEndStatement. A generic
// ParseBlockUntil([ttLoopBlockEnd]) would therefore stop at a nested FOR's NEXT, treating it
// as the loop terminator (the historical "FOR without matching NEXT (found LOOP)" failure).
//
// This helper tracks the number of still-open nested flat FOR loops: while one is open, a NEXT
// is NOT our terminator — it is parsed as a normal statement (which closes the FOR). Nested
// DO/WHILE self-consume their own LOOP/WEND, so they never leave a pending end here.
var
  Statement: TASTNode;
  Token: TLexerToken;
  StartIndex, PendingFor: Integer;
  IsEnd: Boolean;
begin
  Result := TASTNode.Create(antBlock);
  PendingFor := 0;

  while not Context.IsAtEnd do
  begin
    Token := Context.CurrentToken;

    // A line number inside the body becomes a body child (SSA opens its LINE_<n> block there),
    // so GOTO/GOSUB targets inside a multi-line DO body resolve - and when an open-ended DO
    // absorbs the rest of a CLASSIC program, every later line keeps its label. (They used to
    // be silently dropped here.)
    if Context.Check(ttLineNumber) then
    begin
      Result.AddChild(TASTNode.CreateWithValue(antLineNumber, Token.Value, Token));
      Context.SetCurrentBasicLine(StrToIntDef(Token.Value, 0), BuildSourceLine(Context));
      Context.Advance;
      Continue;
    end;
    if Context.Match(ttEndOfLine) then
    begin
      // Same duty as ParseProgram's EOL handling: a completed single-line IF closes at end of
      // line (with ELSE lookahead for multi-line blocks). Without this, an IF inside the body
      // stayed open across the swallowed EOL and died as "IF statement never closed".
      PopCompletedIfsAtEOL;
      Continue;
    end;

    if Token.TokenType = ttLoopBlockEnd then
    begin
      // A NEXT closing a still-open nested FOR is part of the body, not our terminator.
      if (UpperCase(Token.Value) = kNEXT) and (PendingFor > 0) then
      begin
        Dec(PendingFor);
        Statement := ParseStatement;        // consumes NEXT, pops the FOR validation entry
        if Assigned(Statement) then
          Result.AddChild(Statement);
        Continue;
      end;
      // CLASSIC: a NEXT with no open FOR is a body STATEMENT (it lowers to the runtime
      // ?NEXT WITHOUT FOR raise), never this loop's terminator - treating it as one made
      // ParseDoStatement swallow the token silently. MODERN keeps the strict behavior.
      if (UpperCase(Token.Value) = kNEXT) and (not FModernMode) then
      begin
        Statement := ParseStatement;        // orphan antNext (raise when executed)
        if Assigned(Statement) then
          Result.AddChild(Statement);
        Continue;
      end;
      // Otherwise (LOOP/WEND, or MODERN NEXT with no open FOR) this is our terminator.
      Break;
    end;

    StartIndex := Context.CurrentIndex;
    Statement := ParseStatement;
    if Assigned(Statement) then
    begin
      Result.AddChild(Statement);
      // A flat FOR header leaves a NEXT to be matched later in this body.
      IsEnd := Statement.NodeType = antForLoop;
      if IsEnd then
        Inc(PendingFor);
    end
    else if Context.CurrentIndex = StartIndex then
      Break;  // no progress — avoid an infinite loop
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
