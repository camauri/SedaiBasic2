unit SedaiPreprocessor;
// FreeBASIC-style source preprocessor (v1). A pure text->text pass run BEFORE lexing.
// Supports:
//   #define NAME [value]      object-like or function-like (NAME(params) body) macro
//   #macro NAME[(params)] ... #endmacro   multi-line macro (body lines joined with ':')
//   In a function-like macro body: #param stringizes an argument; a ## b pastes tokens together.
//   #undef NAME
//   #ifdef NAME / #ifndef NAME / #else / #endif   conditional compilation
//   #elseifdef NAME / #elseifndef NAME            else-if on a defined/undefined symbol
//   #if <expr> / #elif <expr> / #elseif <expr>    conditional compilation on a constant integer expression
//       (literals, defined(NAME), macro values, comparisons, AND/OR/NOT, parentheses)
//   #include "file"           splice another file (relative to the including file's directory)
//   #error msg                stop compilation with a diagnostic (message is macro-expanded)
//   #assert <expr>            stop compilation if the constant integer expression is false
// Directive lines in the top-level file are blanked (kept as empty lines) so error line numbers in
// that file are preserved; included files are appended after preprocessing (their line numbers shift).
// Object-like macro names are substituted as whole words outside string literals.

{$mode objfpc}{$H+}
{$codepage UTF8}   // CP_UTF8 literals, like every other unit. A string that crosses a codepage
                   // boundary is converted and a comparison stops being a memcmp - see SedaiFileIO.

interface

uses SysUtils, Math, SedaiConsoleBehavior;

type
  // Raised by #error / a failed #assert. Callers catch it to report a clean compile-time
  // diagnostic and abort the build (there is no meaningful program to run).
  EPreprocessorError = class(Exception);

{ ⭐ Set by sbc when --target wasm is given, so a program can ASK which machine
  it is being compiled for. That question has to be answerable at COMPILE time,
  not at run time: the backend refuses an uncovered opcode because it is PRESENT,
  not because it executes, so a run-time "If" around a file-writing branch does
  not keep that branch out of the module. A #if does. }
var
  GTargetIsWasm: Boolean = False;

type
  { One "#line <n> ["file"]" directive: from the PHYSICAL source line it stands on, positions are
    reported as if that line were ReportedLine of ModuleName. FreeBASIC's own use for it is a code
    GENERATOR: the emitted file carries the positions of the file it was generated FROM, so a
    diagnostic points at what the author wrote. It affects REPORTING only - never what the program
    computes - which is why ignoring it, as this preprocessor used to, was silent rather than loud. }
  TPPLineDirective = record
    FromPhysical: Integer;    // the physical line the directive itself occupies (1-based)
    ReportedLine: Integer;    // ...which is reported as this
    ModuleName: string;       // '' = keep the module name it had
  end;

var
  { Filled by PreprocessSource, in physical-line order; empty for a source with no #line. Read by
    whoever reports a POSITION (the uncaught-error abort message). A global for the same reason
    GUniqueIdStacks is one: the preprocessor hands back text, and this is about that text. }
  GPPLineDirectives: array of TPPLineDirective;

{ The line number to REPORT for a physical source line, and the module to report it in. Answers the
  physical line and the unchanged module when no #line covers it. }
procedure PPMapLine(Physical: Integer; out Reported: Integer; var Module: string);

function PreprocessSource(const Src, BaseDir: string; const FileName: string = ''): string;
function DetectQBLang(const Src: string): Boolean;
// ⭐ Did the source ask for a dialect that is NOT -lang fb? fbc honours '#lang "qb"' / '#lang "fblite"'
// / '#lang "deprecated"' (and the '$lang: spelling) from INSIDE the file, and several rules - default
// types, type suffixes, the DEFxxx letter rule - are legal there and refused in -lang fb. A check that
// enforces the -lang fb rule has to know which language the file asked for, or it refuses a program
// fbc compiles. Set by PreprocessSource, which is the ONE funnel every front end passes a source
// through before lexing; read through SourceDeclaresNonFbDialect.
function DetectNonFbLang(const Src: string): Boolean;
function SourceDeclaresNonFbDialect: Boolean;

implementation

uses Classes, SedaiLexerTypes;   // cVirtualEOL: the separator a multi-line #macro body is joined with

function DetectQBLang(const Src: string): Boolean;
// Does this source select the QB dialect, '#lang "qb"' or the '$lang: "qb" metacommand?
//
// Line by line, and anchored at the START of the line: a DIRECTIVE is one, and searching the whole text
// for the substring found it inside comments and in the middle of ordinary lines. A file whose header
// comment merely MENTIONS '$lang: "qb" was compiled in the qb dialect - which changes PRINT spacing for
// every number in it, and says nothing about itself while doing so.
//
// Detected on the RAW text, before preprocessing: that pass strips both directive forms.
var
  L: TStringList;
  i: Integer;
  T: string;
begin
  Result := False;
  L := TStringList.Create;
  try
    L.Text := Src;
    for i := 0 to L.Count - 1 do
    begin
      T := UpperCase(TrimLeft(L[i]));
      if (Length(T) > 0) and (T[1] = '''') then T := TrimLeft(Copy(T, 2, MaxInt));  // '$lang metacommand
      T := StringReplace(T, ' ', '', [rfReplaceAll]);
      if (Copy(T, 1, 11) = '#LANG"QB"') or (Copy(T, 1, 12) = '$LANG:"QB"') or
         (Copy(T, 1, 9) = '#LANG"QB"') or (Copy(T, 1, 10) = '$LANG:"QB"') then
        Exit(True);
    end;
  finally
    L.Free;
  end;
end;

var
  GDeclaredNonFbDialect: Boolean = False;   // last source handed to PreprocessSource asked for qb/fblite/deprecated

function DetectNonFbLang(const Src: string): Boolean;
// The same line-anchored scan DetectQBLang does, and anchored for the same reason: a header comment
// that MENTIONS the directive is not the directive. Three languages, because fbc's own message names
// three - "only valid in -lang deprecated or fblite or qb".
var
  L: TStringList;
  i: Integer;
  T: string;
begin
  Result := False;
  L := TStringList.Create;
  try
    L.Text := Src;
    for i := 0 to L.Count - 1 do
    begin
      T := UpperCase(TrimLeft(L[i]));
      if (Length(T) > 0) and (T[1] = '''') then T := TrimLeft(Copy(T, 2, MaxInt));   // '$lang metacommand
      T := StringReplace(T, ' ', '', [rfReplaceAll]);
      if (Copy(T, 1, 5) = '#LANG') or (Copy(T, 1, 6) = '$LANG:') then
        if (Pos('"QB"', T) > 0) or (Pos('"FBLITE"', T) > 0) or (Pos('"DEPRECATED"', T) > 0) then
          Exit(True);
    end;
  finally
    L.Free;
  end;
end;

function SourceDeclaresNonFbDialect: Boolean;
begin
  Result := GDeclaredNonFbDialect;
end;

function IsIdentChar(C: Char): Boolean; inline;
begin
  Result := (C in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
end;

function StripDirectiveComment(const S: string): string;
// Remove a trailing "'" line comment from a preprocessor-directive body (e.g. a #define value),
// honoring double-quoted string literals so a "'" inside a string is kept. FreeBASIC treats "'" as a
// comment start in a #define body just like in code, so "#define MAX 100 ' note" defines MAX as "100"
// (without stripping, the comment leaks into the macro body and breaks every expansion site).
var
  i: Integer;
  InStr: Boolean;
begin
  InStr := False;
  i := 1;
  while i <= Length(S) do
  begin
    if S[i] = '"' then
      InStr := not InStr
    // ⛔ ...AND A BLOCK COMMENT IS NOT A LINE COMMENT. "/' c '/" opens with a '/' followed by the very
    // character this scanner treats as "the rest of the line is a comment", so a directive carrying one
    // was truncated at the '/' - and since this is what decides whether a directive CONTINUES on the
    // next line, "# macro create_macro /' c '/ _" stopped continuing and the parameter list that
    // followed was left standing as code. The same line without the comment worked, which is the
    // difference that named it.
    else if (not InStr) and (S[i] = '/') and (i < Length(S)) and (S[i + 1] = '''') then
    begin
      Inc(i, 2);                                   // past the opening "/'"
      while (i < Length(S)) and not ((S[i] = '''') and (S[i + 1] = '/')) do Inc(i);
      Inc(i, 2);                                   // past the closing "'/"
      Continue;
    end
    else if (S[i] = '''') and not InStr then
      Break;
    Inc(i);
  end;
  Result := TrimRight(Copy(S, 1, i - 1));
end;

// Replace whole-word object-like macro occurrences using Defs (Names hold UPPER macro names), skipping
// the contents of "..." string literals. A match must be a full identifier (word boundaries).
// Split a function-like macro argument string into top-level arguments (commas inside nested parens or
// string literals do not separate). Returns the count; Args holds the trimmed argument texts.
procedure SplitMacroArgs(const S: string; out Args: array of string; out Count: Integer;
                         Starts: PInteger = nil);
// Starts (optional) receives the 1-based offset in S where each argument's RAW text begins. A variadic
// macro parameter needs that: "#rest" must stringize what was WRITTEN, commas, gaps and all, and the
// trimmed pieces cannot be glued back into it - "a, , b" and "a,,b" would come out the same.
var
  i, depth: Integer;
  cur: string;
  InStr: Boolean;
  ArgStart: Integer;

  procedure NoteStart;
  begin
    if (Starts <> nil) and (Count <= High(Args)) then Starts[Count] := ArgStart;
  end;

begin
  Count := 0; cur := ''; depth := 0; InStr := False; ArgStart := 1;
  for i := 1 to Length(S) do
  begin
    if InStr then
    begin
      cur := cur + S[i];
      if S[i] = '"' then InStr := False;
    end
    else if S[i] = '"' then begin InStr := True; cur := cur + S[i]; end
    else if S[i] = '(' then begin Inc(depth); cur := cur + S[i]; end
    else if S[i] = ')' then begin Dec(depth); cur := cur + S[i]; end
    else if (S[i] = ',') and (depth = 0) then
    begin
      if Count <= High(Args) then Args[Count] := Trim(cur);
      NoteStart;
      Inc(Count); cur := ''; ArgStart := i + 1;
    end
    else cur := cur + S[i];
  end;
  if (Trim(cur) <> '') or (Count > 0) then
  begin
    if Count <= High(Args) then Args[Count] := Trim(cur);
    NoteStart;
    Inc(Count);
  end;
end;

function SubstituteMacros(const Line: string; Defs, FnDefs: TStringList; Depth: Integer): string; forward;

// Expand a function-like macro body by replacing each whole-identifier parameter with its argument.
// ParamsBody is "p1,p2,..."#1"body"; ArgsStr is the raw argument text between the parentheses.
// Defs/FnDefs/Depth are needed ONLY by the stringize operator - see the comment at its site.
function ExpandFnBody(const ParamsBody, ArgsStr: string;
                      Defs, FnDefs: TStringList; Depth: Integer): string;
var
  sep, i, j, k, pi: Integer;
  ParamList, Body, Word: string;
  Params: array of string;
  Args: array[0..63] of string;
  Starts: array[0..63] of Integer;
  PCount, ACount, VarIdx: Integer;
  InStr: Boolean;
  InCmt: Boolean;    // inside a ' comment: copy verbatim until the line end
  JoinStr: Boolean;   // the right side of a "##" that joins two STRING LITERALS: drop its opening quote

  function ParamIndex(const W: string): Integer;
  var n: Integer;
  begin
    Result := -1;
    for n := 0 to PCount - 1 do
      if Params[n] = W then begin Result := n; Exit; end;
  end;

  function Stringize(const S: string): string;
  // Turn an argument into a BASIC string literal: trim, and double any embedded quote.
  var t: string; n: Integer;
  begin
    t := Trim(S);
    Result := '"';
    for n := 1 to Length(t) do
      if t[n] = '"' then Result := Result + '""' else Result := Result + t[n];
    Result := Result + '"';
  end;

begin
  sep := Pos(#1, ParamsBody);
  ParamList := Copy(ParamsBody, 1, sep - 1);
  // The optional-paren mark is not a parameter: strip it here too, so the ORDINARY parenthesised call
  // of such a macro reads the same list the paren-less one does.
  if (ParamList <> '') and (ParamList[1] = '?') then ParamList := Copy(ParamList, 2, MaxInt);
  Body := Copy(ParamsBody, sep + 1, MaxInt);
  // parameter names
  SetLength(Params, 0); PCount := 0;
  i := 1;
  while i <= Length(ParamList) do
  begin
    j := i;
    while (j <= Length(ParamList)) and (ParamList[j] <> ',') do Inc(j);
    SetLength(Params, PCount + 1); Params[PCount] := Trim(Copy(ParamList, i, j - i)); Inc(PCount);
    i := j + 1;
  end;
  // FreeBASIC variadic macro parameter, "#macro m(a, rest...)": the last parameter's name carries the
  // ellipsis, and it stands for EVERY remaining argument, written exactly as they were written. Without
  // this the name never matched a parameter, so "#rest" stayed a literal "#rest" in the body and the
  // expansion was nonsense - and the no-argument call "m(5)" left it behind to be read as a value.
  VarIdx := -1;
  for k := 0 to PCount - 1 do
    if (Length(Params[k]) > 3) and (Copy(Params[k], Length(Params[k]) - 2, 3) = '...') then
    begin
      Params[k] := TrimRight(Copy(Params[k], 1, Length(Params[k]) - 3));
      VarIdx := k;
    end;
  SplitMacroArgs(ArgsStr, Args, ACount, @Starts[0]);
  // The variadic parameter takes the RAW remainder of the argument text (empty when nothing was passed).
  if VarIdx >= 0 then
  begin
    if (VarIdx < ACount) and (VarIdx <= High(Args)) then
      Args[VarIdx] := Trim(Copy(ArgsStr, Starts[VarIdx], MaxInt))
    else if VarIdx <= High(Args) then
    begin
      Args[VarIdx] := '';
      if ACount <= VarIdx then ACount := VarIdx + 1;   // so the parameter resolves to the empty text
    end;
  end;
  // ⛔⛔ THE ARGUMENTS ARE EXPANDED ONCE, HERE, BEFORE THEY ENTER THE BODY - and on this point
  // FreeBASIC does NOT follow C. In C an operand of "##" is frozen; here it is not, and the
  // manual's own example proves it (defines/fbjoin2.bas):
  //     #define join( a, b )      a##b
  //     #define makename2( x )    join( PREFIX, join( x, SUFFIX ) )
  //     makename2(text)  ->  ptext_T           '' not "PREFIXjoin( text, SUFFIX )"
  // i.e. BOTH arguments of join are expanded (PREFIX->p, join(text,SUFFIX)->text_T) and only
  // then pasted.
  // ⚠️ And that does not contradict the line above it, "#define makename1(x) PREFIX##x##SUFFIX",
  // which gives PREFIXtextSUFFIX: there PREFIX and SUFFIX are not ARGUMENTS, they are text of the
  // BODY. The body is not pre-expanded, and pasting turns it into a new identifier that the
  // rescan does not reopen. Arguments and body elements follow two different rules, and keeping
  // them apart is what this block exists to do.
  // ⛔ Expanding AFTER the paste is not enough and not the same thing: "PREFIX" and "join" pasted
  // together are the single identifier "PREFIXjoin", and by then there is no call left to expand.
  // The information has already been destroyed.
  for k := 0 to ACount - 1 do
    if Pos('#', Args[k]) = 0 then          // an argument carrying '#' is already preprocessor text
      Args[k] := SubstituteMacros(Args[k], Defs, FnDefs, Depth + 1);
  // Replace each whole-identifier parameter with its argument, handling the FreeBASIC preprocessor
  // operators: "#param" stringizes the argument; "a ## b" pastes the surrounding tokens together.
  Result := ''; i := 1; InStr := False; JoinStr := False; InCmt := False;
  while i <= Length(Body) do
  begin
    // ⛔ A ' COMMENT IS TEXT, and it ends at the line end. The body's lines are joined with
    // cVirtualEOL, and neither fact was modelled: the quotes inside a comment toggled the string
    // state, so the manual's fbquote2 - whose comment reads (otherwise the result would be
    // "arg1""arg2" => "arg1"arg2") - left an ODD number of them and every line AFTER it was treated
    // as being inside a string. Nothing was substituted there, __FB_QUOTE__ reached the parser
    // unexpanded, and the error pointed at a column 300 characters into a line the author never wrote.
    if Body[i] = cVirtualEOL then
    begin
      InCmt := False; InStr := False;      // a line end closes both, whatever they were
      Result := Result + Body[i]; Inc(i); Continue;
    end;
    if InCmt then begin Result := Result + Body[i]; Inc(i); Continue; end;
    if InStr then begin Result := Result + Body[i]; if Body[i] = '"' then InStr := False; Inc(i); Continue; end;
    if Body[i] = '''' then begin InCmt := True; Result := Result + Body[i]; Inc(i); Continue; end;
    if Body[i] = '"' then
    begin
      InStr := True;
      if JoinStr then JoinStr := False   // right side of a paste: its opening quote is dropped
      else Result := Result + Body[i];
      Inc(i); Continue;
    end;
    // Token paste "##": drop trailing whitespace already emitted and skip whitespace after ##.
    if (Body[i] = '#') and (i < Length(Body)) and (Body[i + 1] = '#') then
    begin
      while (Length(Result) > 0) and (Result[Length(Result)] in [' ', #9]) do
        Delete(Result, Length(Result), 1);
      Inc(i, 2);
      while (i <= Length(Body)) and (Body[i] in [' ', #9]) do Inc(i);
      // Pasting two STRING LITERALS joins their CONTENTS, not their text. Textual pasting produced
      // ""a""b"", which BASIC reads as ONE literal holding a"b - the doubled quote is an escaped
      // quote - so the manual's own "#arg1###arg2" (stringize, paste, stringize) printed Free"BASIC
      // where fbc prints FreeBASIC. Only taken when BOTH sides really are literals: the left one is
      // already emitted, and the right is either a literal or a stringize about to produce one.
      JoinStr := (Length(Result) > 0) and (Result[Length(Result)] = '"') and (i <= Length(Body)) and
                 ((Body[i] = '"') or (Body[i] = '#'));
      if JoinStr then Delete(Result, Length(Result), 1);
      Continue;
    end;
    // Stringize "#param": emit the matching argument as a quoted string literal.
    if Body[i] = '#' then
    begin
      j := i + 1;
      while (j <= Length(Body)) and (Body[j] in [' ', #9]) do Inc(j);
      if (j <= Length(Body)) and (Body[j] in ['A'..'Z', 'a'..'z', '_']) then
      begin
        k := j;
        while (k <= Length(Body)) and IsIdentChar(Body[k]) do Inc(k);
        Word := Copy(Body, j, k - j);
        pi := ParamIndex(Word);
        // ⛔ A PARAMETER THE CALLER LEFT OUT IS AN EMPTY ARGUMENT, not a missing one. "m()" on a
        // one-parameter macro passes ONE argument and it is empty - FreeBASIC's own test suite writes
        // exactly that ("#define m( s ) "a" + #s + "b"" then "m()"), and stringizing it must give "".
        // We required the index to be INSIDE the argument list, so the whole "#s" fell through to the
        // lone-'#' rule and a bare '#' reached the lexer.
        if (pi >= 0) and (pi >= ACount) then
        begin
          Word := '""';
          if JoinStr then begin Delete(Word, 1, 1); JoinStr := False; end;
          Result := Result + Word;
          i := k; Continue;
        end;
        if (pi >= 0) and (pi < ACount) then
        begin
          // ⛔ THE ARGUMENT IS EXPANDED BEFORE IT IS STRINGIZED, and here too FreeBASIC does NOT
          // follow C. In C "#x" freezes the text the caller wrote; in FreeBASIC the manual shows
          //     #macro dump(arg)
          //       #print #arg
          //     #endmacro
          //     dump( makename1(text) )      '' prints PREFIXtextSUFFIX, not "makename1(text)"
          // i.e. the argument is EXPANDED and then turned into text. Taking the C rule gave the
          // raw text, and it was invisible: no error, only the wrong output.
          // ⚠️ Args[] is ALREADY expanded by the block just above, so this only has to quote it.
          Word := Stringize(Args[pi]);
          // ...and when this literal is the RIGHT side of a paste, its opening quote is dropped: the
          // left one's closing quote is already gone, so the two contents become one literal.
          if JoinStr then begin Delete(Word, 1, 1); JoinStr := False; end;
          Result := Result + Word;
          i := k; Continue;
        end;
      end;
      Result := Result + '#'; Inc(i); Continue;   // a lone '#' that is not a stringize
    end;
    // ⛔⛔ A NUMERIC LITERAL IS ONE TOKEN, AND ITS EXPONENT IS NOT AN IDENTIFIER. The scan below starts
    // an identifier at any letter, and after "1.1920929" the next character is an 'e' - so a macro
    // whose parameter is named "e" had every float literal in its own body corrupted:
    //     #macro m( a, e, u ) : Print 1.1920929e-7 : #endmacro
    //     m( 1, 2, 3 )        ->  Print 1.19209292-7   ->  -5.80790708
    // Not a parse error - a WRONG NUMBER, printed. It is fbcunit's own CU_ASSERT_SINGLE_APPROX and
    // CU_ASSERT_DOUBLE_APPROX, whose parameters are (a, e, u) and whose bodies carry 1.1920929e-7 and
    // 2.220446049250313e-16, so every test that used them read a nonsense tolerance - and fbc's
    // udt-zstring/conversion and udt-wstring/conversion could not parse at all.
    // ⚠️ Only a WELL-FORMED exponent is swallowed (e/E/d/D, an optional sign, then digits): "1e" with
    // no digits after it is not one, and stays whatever it was. The type suffix ('!', '#') is left to
    // the character path exactly as before.
    if (Body[i] in ['0'..'9']) or
       ((Body[i] = '.') and (i < Length(Body)) and (Body[i + 1] in ['0'..'9'])) then
    begin
      j := i;
      while (j <= Length(Body)) and (Body[j] in ['0'..'9', '.']) do Inc(j);
      if (j <= Length(Body)) and (Body[j] in ['e', 'E', 'd', 'D']) then
      begin
        k := j + 1;
        if (k <= Length(Body)) and (Body[k] in ['+', '-']) then Inc(k);
        if (k <= Length(Body)) and (Body[k] in ['0'..'9']) then
        begin
          while (k <= Length(Body)) and (Body[k] in ['0'..'9']) do Inc(k);
          j := k;
        end;
      end;
      Result := Result + Copy(Body, i, j - i);
      i := j;
      Continue;
    end;
    if Body[i] in ['A'..'Z', 'a'..'z', '_'] then
    begin
      j := i;
      while (j <= Length(Body)) and IsIdentChar(Body[j]) do Inc(j);
      Word := Copy(Body, i, j - i);
      pi := ParamIndex(Word);
      if (pi >= 0) and (pi < ACount) then Result := Result + Args[pi]
      // ...and an omitted parameter substitutes to NOTHING, for the same reason: it was passed, empty.
      else if pi >= 0 then Result := Result + ''
      else Result := Result + Word;
      i := j;
    end
    else begin Result := Result + Body[i]; Inc(i); end;
  end;
end;

var
  PPFloatFmt: TFormatSettings;   // '.' as the decimal separator, whatever the locale says

function IsPPMathFn(const N: string): Boolean;
// The maths functions __FB_EVAL__ folds. fbc folds its whole constant-expression set; this is the
// portion a preprocessor can answer with no symbol table, and the manual's own example needs Atn.
begin
  Result := (N = 'ATN') or (N = 'SIN') or (N = 'COS') or (N = 'TAN') or (N = 'ASIN') or
            (N = 'ACOS') or (N = 'EXP') or (N = 'LOG') or (N = 'SQR') or (N = 'ABS') or
            (N = 'INT') or (N = 'FIX') or (N = 'SGN');
end;

function ApplyPPMathFn(const N: string; A: Double): Double;
begin
  if N = 'ATN' then Result := ArcTan(A)
  else if N = 'SIN' then Result := Sin(A)
  else if N = 'COS' then Result := Cos(A)
  else if N = 'TAN' then Result := Tan(A)
  else if N = 'ASIN' then Result := ArcSin(A)
  else if N = 'ACOS' then Result := ArcCos(A)
  else if N = 'EXP' then Result := Exp(A)
  else if N = 'LOG' then begin if A > 0 then Result := Ln(A) else Result := 0; end
  else if N = 'SQR' then begin if A >= 0 then Result := Sqrt(A) else Result := 0; end
  else if N = 'ABS' then Result := Abs(A)
  else if N = 'INT' then Result := Floor(A)
  else if N = 'FIX' then Result := Trunc(A)
  else if N = 'SGN' then begin if A > 0 then Result := 1 else if A < 0 then Result := -1 else Result := 0; end
  else Result := A;
end;

function EvalPPExprFloat(const RawExpr: string; Defs: TStringList; out V: Double): Boolean;
// __FB_EVAL__ over a constant FLOAT expression, intrinsics included: "4 * Atn(1)" is 3.141592653589793
// and not the text "4 * Atn(1)". The integer evaluator above is tried FIRST and answers whenever the
// expression is integral, so this one exists for exactly the cases it cannot take: a division that does
// not divide, a literal with a point, a call to one of the maths functions.
//
// ⛔ Deliberately a SECOND evaluator rather than a widening of the first. The integer one is what
// __FB_ARG_EXTRACT__ asks for an INDEX, and what "#if" asks for a condition; giving those a Double and
// rounding back would turn an exact answer into a rounded one for no gain. They are separate questions
// and they get separate answers.
var
  Toks: TStringList;
  TPos: Integer;

  procedure Tokenize(const S: string; Depth: Integer);
  var p, q: Integer; id, two: string;
  begin
    p := 1;
    while p <= Length(S) do
    begin
      if S[p] in [' ', #9] then begin Inc(p); Continue; end;
      if p < Length(S) then
      begin
        two := Copy(S, p, 2);
        if (two = '<=') or (two = '>=') or (two = '<>') then begin Toks.Add(two); Inc(p, 2); Continue; end;
      end;
      if S[p] in ['=', '<', '>', '(', ')', ',', '+', '-', '*', '/', '\', '^'] then
      begin Toks.Add(S[p]); Inc(p); Continue; end;
      if (S[p] in ['0'..'9']) or ((S[p] = '.') and (p < Length(S)) and (S[p + 1] in ['0'..'9'])) then
      begin
        q := p;
        while (q <= Length(S)) and (S[q] in ['0'..'9', '.']) do Inc(q);
        // an exponent, and its sign
        if (q <= Length(S)) and (UpCase(S[q]) in ['E', 'D']) then
        begin
          Inc(q);
          if (q <= Length(S)) and (S[q] in ['+', '-']) then Inc(q);
          while (q <= Length(S)) and (S[q] in ['0'..'9']) do Inc(q);
        end;
        Toks.Add(Copy(S, p, q - p)); p := q; Continue;
      end;
      if IsIdentChar(S[p]) then
      begin
        q := p;
        while (q <= Length(S)) and IsIdentChar(S[q]) do Inc(q);
        id := UpperCase(Copy(S, p, q - p)); p := q;
        if (id = 'MOD') or (id = 'SHL') or (id = 'SHR') or IsPPMathFn(id) then Toks.Add(id)
        else if Defs.IndexOfName(id) >= 0 then
        begin
          if Depth < 32 then Tokenize(Trim(Defs.Values[id]), Depth + 1) else Toks.Add('0');
        end
        else
          Toks.Add('?');                 // an unknown identifier: this is not a constant expression
        Continue;
      end;
      Toks.Add('?'); Inc(p);             // anything else: not foldable
    end;
  end;

  function Peek: string;
  begin if TPos < Toks.Count then Result := Toks[TPos] else Result := ''; end;

  function ParseSum: Double; forward;

  function ParseUnary: Double;
  var t: string; a: Double;
  begin
    Result := 0;
    t := Peek;
    if t = '(' then begin Inc(TPos); Result := ParseSum; if Peek = ')' then Inc(TPos); end
    else if t = '-' then begin Inc(TPos); Result := -ParseUnary(); end
    else if t = '+' then begin Inc(TPos); Result := ParseUnary(); end
    else if IsPPMathFn(t) then
    begin
      Inc(TPos);
      if Peek = '(' then Inc(TPos);
      a := ParseSum;
      if Peek = ')' then Inc(TPos);
      Result := ApplyPPMathFn(t, a);
    end
    else if (t <> '') and (t[1] in ['0'..'9', '.']) then
    begin Result := StrToFloatDef(t, 0, PPFloatFmt); Inc(TPos); end
    else
      Inc(TPos);                          // '?' and friends: consumed, and the caller has already failed
  end;

  function ParsePow: Double;
  var r: Double;
  begin
    Result := ParseUnary;
    while Peek = '^' do begin Inc(TPos); r := ParseUnary; Result := Power(Result, r); end;
  end;

  function ParseProd: Double;
  var op: string; r: Double;
  begin
    Result := ParsePow;
    while (Peek = '*') or (Peek = '/') or (Peek = '\') or (Peek = 'MOD') do
    begin
      op := Peek; Inc(TPos); r := ParsePow;
      if op = '*' then Result := Result * r
      else if r = 0 then Result := 0
      else if op = '/' then Result := Result / r
      else if op = '\' then Result := Trunc(Result) div Trunc(r)
      else Result := Trunc(Result) mod Trunc(r);
    end;
  end;

  function ParseSum: Double;
  var op: string; r: Double;
  begin
    Result := ParseProd;
    while (Peek = '+') or (Peek = '-') do
    begin op := Peek; Inc(TPos); r := ParseProd; if op = '+' then Result := Result + r else Result := Result - r; end;
  end;

var
  i: Integer;
begin
  Result := False;
  V := 0;
  Toks := TStringList.Create;
  try
    Tokenize(RawExpr, 0);
    if Toks.Count = 0 then Exit;
    for i := 0 to Toks.Count - 1 do
      if Toks[i] = '?' then Exit;         // something in there is not a constant: leave the text alone
    TPos := 0;
    V := ParseSum;
    Result := TPos >= Toks.Count;         // every token consumed, or it was not an expression
  finally
    Toks.Free;
  end;
end;

function PPDirectiveContinues(const S: string): Boolean;
// Does this preprocessor-directive line end with FreeBASIC's '_' LINE CONTINUATION?
// True only when the underscore stands alone at the end: an identifier may end in '_' too
// ("#define MAX_" defines MAX_, it does not continue), so the character before it must not be
// one an identifier could hold.
var
  T: string;
begin
  T := TrimRight(StripDirectiveComment(S));
  Result := (T <> '') and (T[Length(T)] = '_') and
            ((Length(T) = 1) or not IsIdentChar(T[Length(T) - 1]));
end;

function LineContinuationCut(const S: string): Integer;
// The position of FreeBASIC's '_' LINE CONTINUATION in an ordinary source line, or 0 when the line
// does not continue.
// ⛔ THE ANSWER HAS TO BE A POSITION, NOT A YES/NO: fbc drops everything that follows the '_', so
// "check( 1, _ )" continues on the next line and that ')' is not part of the program at all. A
// predicate that looked at the LAST character - which is what PPDirectiveContinues does, correctly,
// for a directive - answers False for exactly that shape, and fbc's own wstring tests are written
// with it.
// A '_' inside a string literal, or after a "'" comment, is text. An identifier may hold '_' at
// either end ("MAX_", "__FB_ARG__"), so the token has to stand alone on both sides.
var
  i: Integer;
  InStr: Boolean;
begin
  Result := 0;
  InStr := False;
  for i := 1 to Length(S) do
  begin
    if S[i] = '"' then InStr := not InStr
    else if InStr then Continue
    else if S[i] = '''' then Break                     // a line comment: the rest is not code
    else if (S[i] = '_') and
            ((i = 1) or not IsIdentChar(S[i - 1])) and
            ((i = Length(S)) or not IsIdentChar(S[i + 1])) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function PPConstStrFold(const S: string; Defs: TStringList; out Res: string): Boolean;
// A constant STRING expression: a chain of string literals and string-valued macros joined by '+' or
// '&'. Answers the folded text as a LITERAL (quotes included), so it can be substituted where the
// expression stood. fbc's __FB_EVAL__ folds these too, and the manual's own defines/fbeval2 builds a
// "#define ..." line out of them - without the fold the directive was assembled at the wrong time and
// carried the source text of its parts.
var
  i, j: Integer;
  Part, W: string;
  WantOp: Boolean;
begin
  Result := False;
  Res := '';
  WantOp := False;
  i := 1;
  while i <= Length(S) do
  begin
    if S[i] in [' ', #9] then begin Inc(i); Continue; end;
    if WantOp then
    begin
      if (S[i] <> '+') and (S[i] <> '&') then Exit;   // not a pure concatenation
      WantOp := False; Inc(i); Continue;
    end;
    if (S[i] = '"') or (((S[i] = '$') or (S[i] = '!')) and (i < Length(S)) and (S[i + 1] = '"')) then
    begin
      j := i; if S[j] <> '"' then Inc(j);
      Inc(j);                                          // past the opening quote
      Part := '';
      while j <= Length(S) do
      begin
        if S[j] = '"' then
        begin
          if (j < Length(S)) and (S[j + 1] = '"') then begin Part := Part + '"'; Inc(j, 2); Continue; end;
          Inc(j); Break;
        end;
        Part := Part + S[j]; Inc(j);
      end;
      Res := Res + Part; i := j; WantOp := True; Continue;
    end;
    if S[i] in ['A'..'Z', 'a'..'z', '_'] then
    begin
      j := i;
      while (j <= Length(S)) and IsIdentChar(S[j]) do Inc(j);
      W := UpperCase(Copy(S, i, j - i));
      if Defs.IndexOfName(W) < 0 then Exit;            // an unknown name is not a constant
      Part := Trim(Defs.Values[W]);
      if not PPConstStrFold(Part, Defs, Part) then Exit;
      Res := Res + Part; i := j; WantOp := True; Continue;
    end;
    Exit;                                              // anything else: not a string expression
  end;
  if not WantOp then Exit;                             // nothing was consumed, or it ended on a '+'
  Res := '"' + StringReplace(Res, '"', '""', [rfReplaceAll]) + '"';
  Result := True;
end;

function IsPPFloatExpr(const S: string): Boolean;
// Does this constant expression have to be evaluated as a FLOAT? True when it holds a decimal point, a
// '/' (fbc's '/' is float division - "10 / 4" is 2.5, and '\\' is the integer one), or a maths function.
var
  i, j: Integer;
  W: string;
begin
  Result := False;
  i := 1;
  while i <= Length(S) do
  begin
    if S[i] = '/' then Exit(True);
    if (S[i] = '.') and (i < Length(S)) and (S[i + 1] in ['0'..'9']) then Exit(True);
    if S[i] in ['A'..'Z', 'a'..'z', '_'] then
    begin
      j := i;
      while (j <= Length(S)) and IsIdentChar(S[j]) do Inc(j);
      W := UpperCase(Copy(S, i, j - i));
      if IsPPMathFn(W) then Exit(True);
      i := j;
      Continue;
    end;
    Inc(i);
  end;
end;

function PPConstIntStr(const Expr: string; Defs: TStringList): string; forward;
function QuerySymbol(What: Integer; const Sym: string; Defs: TStringList): string; forward;

var
  // __FB_UNIQUEID_* state: one stack of generated identifiers per stack NAME. These names are a
  // namespace of their own (the manual is explicit about it), so they must not share the #define
  // table. Reset at the start of every PreprocessSource: the ids are per COMPILATION, and a REPL that
  // preprocesses twice would otherwise keep counting up from the previous program.
  GUniqueIdStacks: TStringList = nil;
  GUniqueIdSerial: Integer = 0;

function TokenPos(const Hay, Needle: string): Integer;
// Position of Needle in Hay as a WHOLE TOKEN (delimited by non-identifier characters), or 0. A plain
// Pos() would find "verso" inside "versus" and split the argument at the wrong place -- and silently,
// since the result is still a well-formed piece of text.
var
  p: Integer;
begin
  Result := 0;
  if (Needle = '') or (Hay = '') then Exit;
  p := 1;
  repeat
    // Search from p onwards without StrUtils: Pos on the tail, then map the offset back.
    if p > Length(Hay) then Exit;
    Result := Pos(Needle, Copy(Hay, p, MaxInt));
    if Result = 0 then Exit;
    p := p + Result - 1;
    Result := 0;
    if ((p = 1) or not IsIdentChar(Hay[p - 1])) and
       ((p + Length(Needle) > Length(Hay)) or not IsIdentChar(Hay[p + Length(Needle)])) then
      Exit(p);
    Inc(p);
  until False;
end;

function TryPPBuiltin(const NameU, ArgsStr: string; Defs, FnDefs: TStringList;
                      out Value: string): Boolean;
// FreeBASIC's built-in FUNCTION-LIKE preprocessor macros. They are part of the preprocessor, not of the
// language, so they cannot be expressed as ordinary #defines - each needs the argument LIST itself:
//
//   __FB_JOIN__(a, b)              paste, the "##" operator in call form
//   __FB_ARG_COUNT__(args...)      how many top-level arguments were passed
//   __FB_ARG_EXTRACT__(n, args...) the n-th of them, zero-based
//   __FB_EVAL__(expr)              evaluate a constant integer expression NOW, so its VALUE (not its
//                                  text) can be used as another macro's argument
//   __FB_IIF__(c, a, b)            pick a branch at compile time
//   __FB_UNIQUEID__ / _PUSH_ / _POP_   a generated identifier, and a stack of them
//
// Without them a program using one compiled the macro's own name into the output, which is why they
// showed up as DIFFs rather than as errors.
var
  Args: array[0..63] of string;
  N, Idx: Integer;
  Cond: string;
  CondVal: Int64;
begin
  Result := True;
  Value := '';
  if NameU = '__FB_JOIN__' then
  begin
    SplitMacroArgs(ArgsStr, Args, N);
    if N >= 2 then Value := Trim(Args[0]) + Trim(Args[1])
    else if N = 1 then Value := Trim(Args[0]);
    Exit;
  end;
  if NameU = '__FB_ARG_COUNT__' then
  begin
    SplitMacroArgs(ArgsStr, Args, N);
    if Trim(ArgsStr) = '' then N := 0;
    Value := IntToStr(N);
    Exit;
  end;
  if NameU = '__FB_ARG_EXTRACT__' then
  begin
    SplitMacroArgs(ArgsStr, Args, N);
    if N >= 1 then
    begin
      Idx := StrToIntDef(Trim(PPConstIntStr(Args[0], Defs)), -1);
      if (Idx >= 0) and (Idx + 1 < N) and (Idx + 1 <= High(Args)) then Value := Trim(Args[Idx + 1]);
    end;
    Exit;
  end;
  if NameU = '__FB_EVAL__' then
  begin
    Value := PPConstIntStr(ArgsStr, Defs);
    Exit;
  end;
  if NameU = '__FB_QUOTE__' then
  begin
    // The argument as a STRING LITERAL: "#define X __FB_QUOTE__( Print "hello" )" makes X printable
    // text, and __FB_UNQUOTE__ turns it back into code. Any embedded quote is doubled, as BASIC wants.
    Value := '"' + StringReplace(Trim(ArgsStr), '"', '""', [rfReplaceAll]) + '"';
    Exit;
  end;
  if NameU = '__FB_UNQUOTE__' then
  begin
    // ...and back: strip one layer of quoting, so the text becomes code again.
    Value := Trim(ArgsStr);
    if (Length(Value) >= 2) and (Value[1] = '"') and (Value[Length(Value)] = '"') then
      Value := StringReplace(Copy(Value, 2, Length(Value) - 2), '""', '"', [rfReplaceAll]);
    Exit;
  end;
  if NameU = '__FB_IIF__' then
  begin
    SplitMacroArgs(ArgsStr, Args, N);
    if N >= 3 then
    begin
      Cond := PPConstIntStr(Args[0], Defs);
      // ⛔ THE CONDITION MUST FOLD. __FB_IIF__ picks a branch at COMPILE TIME, so fbc requires a
      // constant expression and refuses anything else. PPConstIntStr hands back the expression
      // UNCHANGED when it cannot fold it, and StrToInt64Def then read that text as 0 - which silently
      // chose the ELSE branch. operator/procptr4 is exactly that case: its condition is
      // "ProcPtr(p, Virtual ...) >= 0", a constant for fbc and not for us, and the else branch took
      // the ordinary address of the procedure and printed 4611686018427387904 where fbc runs the
      // override. Naming it costs one test and turns a wrong answer into a refusal.
      if not TryStrToInt64(Trim(Cond), CondVal) then
        raise Exception.CreateFmt(
          '__FB_IIF__ needs a CONSTANT condition - it chooses a branch while compiling - and "%s" ' +
          'does not fold to one here. Use IIf(...) for a value decided at run time.', [Trim(Args[0])]);
      if CondVal <> 0 then Value := Trim(Args[1]) else Value := Trim(Args[2]);
    end;
    Exit;
  end;
  // __FB_ARG_LEFTOF__(arg, sep [, ret]) / __FB_ARG_RIGHTOF__: split ONE argument around a separator
  // token and return the side asked for. The manual requires the separator to be SPACED in the
  // argument's text ("1 versus 2"), which is what makes a purely textual split well-defined -- the
  // separator is a whole token, never a substring of one, so "verso" never matches inside "versus".
  // When the separator is absent the result is the optional third argument, or nothing.
  if (NameU = '__FB_ARG_LEFTOF__') or (NameU = '__FB_ARG_RIGHTOF__') then
  begin
    SplitMacroArgs(ArgsStr, Args, N);
    if N < 2 then Exit;
    if N >= 3 then Value := Trim(Args[2]) else Value := '';
    Idx := TokenPos(Args[0], Trim(Args[1]));
    if Idx <= 0 then Exit;                     // separator not found: the default answer stands
    if NameU = '__FB_ARG_LEFTOF__' then
      Value := Trim(Copy(Args[0], 1, Idx - 1))
    else
      Value := Trim(Copy(Args[0], Idx + Length(Trim(Args[1])), MaxInt));
    Exit;
  end;
  // __FB_UNIQUEID_PUSH__(stk) / __FB_UNIQUEID__(stk) / __FB_UNIQUEID_POP__(stk): a compile-time stack
  // of generated identifiers, one stack per name. PUSH mints a fresh one, the bare macro reads the top
  // WITHOUT changing the stack, POP drops it. The names live in their own namespace (they are not
  // #defines), so they are kept apart from Defs; an empty or never-filled stack reads as nothing.
  // fbc mints them as "Lt_xxxx" and the manual says so, so the same shape is used here: a program may
  // legitimately print one.
  // __FB_QUERY_SYMBOL__(what, sym): ask what fbc's symbol table would say about sym. See QuerySymbol.
  if NameU = '__FB_QUERY_SYMBOL__' then
  begin
    SplitMacroArgs(ArgsStr, Args, N);
    if N >= 2 then
      // The query selector may itself be a macro/constant expression ("fbc.FB_QUERY_SYMBOL.symbclass"
      // resolves through the emulated header), so fold it before switching on it. Only the low byte is
      // the query; the high byte is a lookup FILTER we do not model.
      Value := QuerySymbol(StrToIntDef(Trim(PPConstIntStr(Args[0], Defs)), -1) and $FF,
                           Args[1], Defs);
    Exit;
  end;
  if (NameU = '__FB_UNIQUEID_PUSH__') or (NameU = '__FB_UNIQUEID__') or (NameU = '__FB_UNIQUEID_POP__') then
  begin
    Cond := UpperCase(Trim(ArgsStr));          // the stack name
    if Cond = '' then Exit;
    Idx := GUniqueIdStacks.IndexOf(Cond);
    if NameU = '__FB_UNIQUEID_PUSH__' then
    begin
      if Idx < 0 then Idx := GUniqueIdStacks.AddObject(Cond, TStringList.Create);
      Inc(GUniqueIdSerial);
      TStringList(GUniqueIdStacks.Objects[Idx]).Add(Format('Lt_%.4d', [GUniqueIdSerial]));
      Value := '';                             // PUSH is a statement, it expands to nothing
      Exit;
    end;
    if Idx < 0 then Exit;                      // never filled: empty text
    with TStringList(GUniqueIdStacks.Objects[Idx]) do
    begin
      if Count = 0 then Exit;
      if NameU = '__FB_UNIQUEID__' then
        Value := Strings[Count - 1]            // top, stack unchanged
      else
        Delete(Count - 1);                     // POP expands to nothing
    end;
    Exit;
  end;
  Result := False;
end;

function LastSegmentIsDirective(const S: string): Boolean;
// Does the LAST cVirtualEOL-separated segment of S begin (after blanks) with '#'? Used to decide
// whether a macro expansion must be closed off before the rest of the invocation line follows it.
var
  i: Integer;
  Seg: string;
begin
  i := Length(S);
  while (i > 0) and (S[i] <> cVirtualEOL) do Dec(i);
  Seg := TrimLeft(Copy(S, i + 1, MaxInt));
  Result := (Seg <> '') and (Seg[1] = '#');
end;

function SkipDirectiveSegment(const Line: string; From: Integer; var Acc: string): Integer;
// If the segment starting at From is a PREPROCESSOR DIRECTIVE (its first non-blank character is '#'),
// append it verbatim to Acc and answer the index just past it; otherwise answer From unchanged. The
// segment ends at cVirtualEOL, which is how a #macro body's lines are joined.
var
  j: Integer;
begin
  Result := From;
  j := From;
  while (j <= Length(Line)) and (Line[j] in [' ', #9]) do Inc(j);
  if (j > Length(Line)) or (Line[j] <> '#') then Exit;
  j := From;
  while (j <= Length(Line)) and (Line[j] <> cVirtualEOL) do begin Acc := Acc + Line[j]; Inc(j); end;
  Result := j;
end;

function SubstituteMacros(const Line: string; Defs, FnDefs: TStringList; Depth: Integer): string;
var
  i, j, k, idx, ParenDepth: Integer;
  Word, ArgsStr, BuiltinVal: string;
  InStr: Boolean;
  InArgStr: Boolean;   // inside a "..." while scanning a macro invocation's arguments
  InCmt: Boolean;   // inside a ' comment: copy verbatim to the end of the line
begin
  Result := '';
  i := 1;
  InStr := False;
  InCmt := False;
  // A DIRECTIVE segment is copied verbatim and left to the directive handlers, which run AFTER this
  // pass and each substitute their own text. Resolving it here inverts the order: a "#define" written
  // inside a macro body had not run yet when a later "#print __FB_ARG_EXTRACT__( that_define, args )"
  // in the SAME body was resolved, so the index read as undefined and the extraction came out empty -
  // while the identical line with a literal index worked, which is what made it look like an
  // ARG_EXTRACT bug rather than an ordering one.
  k := SkipDirectiveSegment(Line, i, Result);
  if k > i then i := k;
  while i <= Length(Line) do
  begin
    // A ' COMMENT IS TEXT, and it ends at the line end - cVirtualEOL included, because a #macro body
    // arrives here as ONE line with its lines joined by that marker. The SAME omission as in
    // ExpandFnBody, in a second place: the quotes inside the manual's own comment
    // ( "arg1""arg2" => "arg1"arg2" ) are an ODD number, so everything after them read as a string
    // and __FB_QUOTE__ was never expanded - it reached the parser as an undeclared array.
    if Line[i] = cVirtualEOL then
    begin
      InCmt := False; InStr := False;
      Result := Result + Line[i]; Inc(i);
      k := SkipDirectiveSegment(Line, i, Result);   // the NEXT segment may be a directive too
      if k > i then i := k;
      Continue;
    end;
    if InCmt then begin Result := Result + Line[i]; Inc(i); Continue; end;
    if InStr then
    begin
      Result := Result + Line[i];
      if Line[i] = '"' then InStr := False;
      Inc(i);
      Continue;
    end;
    if Line[i] = '''' then begin InCmt := True; Result := Result + Line[i]; Inc(i); Continue; end;
    if Line[i] = '"' then
    begin
      InStr := True; Result := Result + Line[i]; Inc(i); Continue;
    end;
    // Identifier start (letter or underscore; a leading digit means a number, not a macro).
    if (Line[i] in ['A'..'Z', 'a'..'z', '_']) then
    begin
      j := i;
      while (j <= Length(Line)) and IsIdentChar(Line[j]) do Inc(j);
      Word := Copy(Line, i, j - i);
      // A DOTTED key is tried before the bare one: "FB_DATACLASS.FB_DATACLASS_INTEGER" is one name, an
      // ENUM MEMBER reached through its enum, and substituting only the half after the dot would leave
      // "FB_DATACLASS.0". Nothing a program can #define carries a dot, so the only keys of this shape
      // are the ones the EMULATED HEADERS register - which is exactly where the qualified spelling
      // comes from. A member access is untouched: "x.field" is not a key.
      if (j <= Length(Line)) and (Line[j] = '.') then
      begin
        k := j + 1;
        while (k <= Length(Line)) and IsIdentChar(Line[k]) do Inc(k);
        if (k > j + 1) and (Defs.IndexOfName(UpperCase(Copy(Line, i, k - i))) >= 0) then
        begin
          Result := Result + Trim(Defs.Values[UpperCase(Copy(Line, i, k - i))]);
          i := k;
          Continue;
        end;
      end;
      // A BUILT-IN function-like macro is tried first: it is the preprocessor's own, and a program may
      // not shadow it with a #define.
      // ⛔ ...and a SPACE between the name and its parenthesis is ordinary FreeBASIC here too. The
      // USER-macro path three dozen lines below skips the blanks, and the #if evaluator has an
      // NextNonBlankIsOpenParen of its own; only this one demanded the '(' be glued to the name, so
      // "__FB_QUOTE__ ( abc )" reached the parser as an undefined array called __FB_QUOTE__.
      k := j;
      while (k <= Length(Line)) and (Line[k] in [' ', #9]) do Inc(k);
      if (k <= Length(Line)) and (Line[k] = '(') and (Copy(UpperCase(Word), 1, 5) = '__FB_') then
      begin
        j := k;
        // ⛔ AND A PARENTHESIS INSIDE A STRING LITERAL IS NOT A PARENTHESIS. This counted '(' and ')'
        // with no in-string flag, so an argument like "2,(3" closed the list early and the macro was
        // expanded with the arguments cut short - 'Unexpected token ")"'. SplitMacroArgs and
        // GatherBalancedParens, downstream, DO carry the flag and are correct; they simply never
        // receive the whole argument text. One more rule that lives in one path and not in the one
        // ahead of it.
        ParenDepth := 0; ArgsStr := ''; InArgStr := False;
        k := j + 1;
        while k <= Length(Line) do
        begin
          if (Line[k] = '"') then InArgStr := not InArgStr
          else if InArgStr then                     // text, not structure
          else if (Line[k] = '(') then Inc(ParenDepth)
          else if (Line[k] = ')') then
          begin
            if ParenDepth = 0 then Break;
            Dec(ParenDepth);
          end;
          ArgsStr := ArgsStr + Line[k];
          Inc(k);
        end;
        if TryPPBuiltin(UpperCase(Word), SubstituteMacros(ArgsStr, Defs, FnDefs, Depth + 1), Defs, FnDefs, BuiltinVal) then
        begin
          if (k <= Length(Line)) and (Line[k] = ')') then Inc(k);
          Result := Result + SubstituteMacros(BuiltinVal, Defs, FnDefs, Depth + 1);
          i := k;
          Continue;
        end;
      end;
      // Function-like macro: NAME immediately followed by '(' — expand with its arguments.
      idx := FnDefs.IndexOfName(UpperCase(Word));
      // A space between the macro name and its arguments is ordinary FreeBASIC - the manual writes
      // "concat (12,34)" - and demanding the parenthesis immediately after the name left the invocation
      // unexpanded, so the macro's own name reached the parser.
      k := j;
      while (k <= Length(Line)) and (Line[k] in [' ', #9]) do Inc(k);
      // "#macro name ? (params)" may also be invoked WITHOUT the parentheses, and then the arguments
      // run to the end of the LINE. The mark is the '?' the parameter list carries; it is stripped
      // before the body is expanded, so ExpandFnBody sees the ordinary list.
      if (idx >= 0) and (Length(FnDefs.ValueFromIndex[idx]) > 0) and (FnDefs.ValueFromIndex[idx][1] = '?') and
         ((k > Length(Line)) or (Line[k] <> '(')) then
      begin
        j := k;
        ArgsStr := '';
        // A macro body is joined with cVirtualEOL, so "the end of the line" is that marker, not the end
        // of the buffer: a paren-less invocation INSIDE a macro body must not swallow the lines after it.
        while (j <= Length(Line)) and (Line[j] <> cVirtualEOL) and (Line[j] <> ':') do
        begin ArgsStr := ArgsStr + Line[j]; Inc(j); end;
        Result := Result + SubstituteMacros(ExpandFnBody(Copy(FnDefs.ValueFromIndex[idx], 2, MaxInt),
                                                         ArgsStr, Defs, FnDefs, Depth), Defs, FnDefs, Depth + 1);
        i := j;
        Continue;
      end;
      if (idx >= 0) and (k <= Length(Line)) and (Line[k] = '(') then
      begin
        j := k;
        // ⛔ AND A PARENTHESIS INSIDE A STRING LITERAL IS NOT A PARENTHESIS. This counted '(' and ')'
        // with no in-string flag, so an argument like "2,(3" closed the list early and the macro was
        // expanded with the arguments cut short - 'Unexpected token ")"'. SplitMacroArgs and
        // GatherBalancedParens, downstream, DO carry the flag and are correct; they simply never
        // receive the whole argument text. One more rule that lives in one path and not in the one
        // ahead of it.
        ParenDepth := 0; ArgsStr := ''; InArgStr := False;
        Inc(j);   // skip '('
        while j <= Length(Line) do
        begin
          if (Line[j] = '"') then InArgStr := not InArgStr
          else if InArgStr then                     // text, not structure
          else if (Line[j] = '(') then Inc(ParenDepth)
          else if (Line[j] = ')') then
          begin
            if ParenDepth = 0 then Break;
            Dec(ParenDepth);
          end;
          ArgsStr := ArgsStr + Line[j];
          Inc(j);
        end;
        if (j <= Length(Line)) and (Line[j] = ')') then Inc(j);   // skip ')'
        // Expand the body (param substitution), then re-run object-like substitution on the result.
        BuiltinVal := SubstituteMacros(ExpandFnBody(FnDefs.ValueFromIndex[idx], ArgsStr, Defs, FnDefs, Depth), Defs, FnDefs, Depth + 1);
        // A macro body ends where the body ends: when its LAST segment is a DIRECTIVE, close it so
        // whatever follows the invocation on its own line - a trailing ' comment, most often - does
        // not become part of it. Without this the comment after "print_last( ... )" was appended to
        // the body's final "#print" and echoed as part of the compile-time message.
        if LastSegmentIsDirective(BuiltinVal) then BuiltinVal := BuiltinVal + cVirtualEOL;
        Result := Result + BuiltinVal;
        i := j;
        Continue;
      end;
      idx := Defs.IndexOfName(UpperCase(Word));
      if idx >= 0 then
        // An object-like macro's VALUE is itself macro text: "#define X __FB_QUOTE__( Print "hi" )"
        // means nothing until the built-in inside it runs. Appending the value raw left it unexpanded,
        // so the macro's own body reached the parser. Re-scanned, with a depth cap so a macro that
        // names itself stops instead of spinning.
        if Depth < 32 then
          Result := Result + SubstituteMacros(Defs.ValueFromIndex[idx], Defs, FnDefs, Depth + 1)
        else
          Result := Result + Defs.ValueFromIndex[idx]
      else
        Result := Result + Word;
      i := j;
    end
    else
    begin
      Result := Result + Line[i];
      Inc(i);
    end;
  end;
end;

function PPPrintMessage(const S: string): string;
// The text a "#print" line echoes. MEASURED against fbc 1.10.1, six shapes:
//   "#print A   B"        -> "A B"     the TOKENS, one space between them, not the verbatim text
//   "#print C  "          -> "C "      trailing whitespace collapses to ONE space
//   "#print D    '' tail" -> "D "      a comment ends the message, and leaves that one space behind
//   "#print E"            -> "E"       nothing after the last token, nothing added
//   "#print ""x  y"""     -> "x  y"    inside a string literal the spacing is the author's
//
// ⛔ THIS FILE USED TO SAY THE MESSAGE WAS THE REST OF THE LINE VERBATIM, "trailing blanks included",
// and cited "#print Release mode " ending in a space. That example agrees with BOTH readings, which is
// why it settled nothing: what separates them is a run of several spaces, or a comment. A guard written
// with ordinary trailing comments on its own #print lines is what finally showed the difference.
var
  i: Integer;
  Pending: Boolean;   // a whitespace run has been seen and not yet emitted
begin
  Result := '';
  Pending := False;
  i := 1;
  while i <= Length(S) do
  begin
    if S[i] in [' ', #9] then begin Pending := True; Inc(i); Continue; end;
    if S[i] = '''' then                       // a comment ends the message, pending space and all
    begin
      if Pending then Result := Result + ' ';
      Exit;
    end;
    if Pending then begin Result := Result + ' '; Pending := False; end;
    if S[i] = '"' then                        // a string literal keeps the author's own spacing
    begin
      Result := Result + S[i]; Inc(i);
      while i <= Length(S) do
      begin
        Result := Result + S[i];
        if S[i] = '"' then
        begin
          if (i < Length(S)) and (S[i + 1] = '"') then begin Result := Result + '"'; Inc(i, 2); Continue; end;
          Inc(i); Break;
        end;
        Inc(i);
      end;
      Continue;
    end;
    Result := Result + S[i];
    Inc(i);
  end;
  if Pending then Result := Result + ' ';
end;

function UnquotePPMessage(const S: string): string;
// A "#print" message that is EXACTLY one string literal prints as its content: fbc shows
//   #print "quoted"      -> quoted
//   #print #arg          -> whatever arg expanded to, unquoted
// and the second is the whole point of stringizing an argument to look at it. Anything else - a bare
// word, a literal with something after it, an unterminated quote - is echoed verbatim, because then
// the quotes are part of what the author wrote rather than a wrapper the expansion put on.
// Escaped quotes inside are left alone: fbc does not process escapes here either.
//
// ⚠️ The SURROUNDING whitespace is kept: only the two quote characters go. Trimming it away as well
// cost the trailing space fbc leaves where a comment followed the literal - the message is built by
// PPPrintMessage, which has already decided what whitespace belongs there.
var
  T: string;
  L, R: Integer;
begin
  Result := S;
  T := Trim(S);
  if (Length(T) >= 2) and (T[1] = '"') and (T[Length(T)] = '"') and
     (Pos('"', Copy(T, 2, Length(T) - 2)) = 0) then
  begin
    L := Pos('"', S);
    R := Length(S);
    while (R > 0) and (S[R] <> '"') do Dec(R);
    Result := Copy(S, 1, L - 1) + Copy(S, L + 1, R - L - 1) + Copy(S, R + 1, MaxInt);
  end;
end;

// Parse a "#name rest" directive line: returns the lowercase directive name and the trimmed rest.
procedure SplitDirective(const Line: string; out Name, Rest: string);
var
  s: string;
  p: Integer;
begin
  s := Trim(Line);
  Delete(s, 1, 1);                       // drop leading '#'
  s := TrimLeft(s);
  p := 1;
  while (p <= Length(s)) and IsIdentChar(s[p]) do Inc(p);
  Name := LowerCase(Copy(s, 1, p - 1));
  Rest := Trim(Copy(s, p, MaxInt));
end;

function PPMacroHeaderComplete(const S: string): Boolean;
// True when S is a "#macro" directive whose parameter list is already finished: either it has a
// balanced "( ... )", or it has no '(' at all (a parameterless macro). Only then does a trailing '_'
// stop meaning "the header goes on" - see the note at the join.
var
  i, Depth: Integer;
  DName, DRest: string;
  SawOpen: Boolean;
begin
  Result := False;
  SplitDirective(TrimRight(S), DName, DRest);
  if DName <> 'macro' then Exit;
  Depth := 0; SawOpen := False;
  for i := 1 to Length(DRest) do
    if DRest[i] = '(' then begin Inc(Depth); SawOpen := True; end
    else if DRest[i] = ')' then Dec(Depth);
  Result := (not SawOpen) or (SawOpen and (Depth <= 0));
end;

var
  // The full source text of the module being preprocessed, for SourceDeclaresSymbol below.
  // Set by PreprocessSource before Expand; the preprocessor is single-threaded by design.
  GPPSourceForDefined: string = '';
  // The line of GPPSourceForDefined the current question is being asked FROM: defined() is answered
  // from the symbol table as built so far, not from the whole file. -1 = no limit (every caller that
  // has no position). Only the MODULE's own Expand pass moves it; an #include or a macro re-expansion
  // inherits the outer position, because that is where the question really stands.
  GPPDefinedLimit: Integer = -1;
  // ⭐ ...and the better answer to the same question: the module AS EXPANDED SO FAR. A macro that
  // DECLARES ("#macro f(id) : id as integer : check_Y( TRIVIAL.id ) : #endmacro" is fbc's own) puts
  // nothing on the source line the scan can see, so a scan of the SOURCE cannot know the field
  // exists. The output being built is the expansion, in order, up to exactly here - which is what
  // fbc's symbol table is - so when it is available it is what gets scanned, and the source with a
  // line limit is the fallback for a caller that has none.
  GPPOutput: TStringList = nil;
  // ⛔ "#pragma reserve NAME" makes NAME a SYMBOL and NOT A MACRO. Putting it in Defs was tried and is
  // wrong: the name is then SUBSTITUTED in ordinary code, and fbc's own pp/pragma-reserve-4 goes on to
  // write "dim symbol as integer" - which became "dim 0 as integer". Reserving is only observable
  // through defined(), so it lives in its own set, which nothing substitutes from.
  GPPReserved: TStringList = nil;
  // ⭐ ...AND A WORD THE LANGUAGE RESERVES IS DEFINED TOO. fbc answers "#if defined( constructor )"
  // TRUE, and its own pp/defined-udt asserts it for constructor / destructor / let / cast - the same
  // names it asserts FALSE for as members of a type. Those two sets are asked the same way and stored
  // apart: a language keyword must not land in GPPReserved, whose entries are "#pragma reserve"
  // reservations carrying a SCOPE DEPTH, where a second reservation of the same name at the same level
  // is an error (m674). This one is the fixed inventory in FbReservedWords.inc, and it is the ORACLE's
  // answer, asked in one compile over an 11 000-word candidate universe - not a list reasoned out.
  GPPKeywords: TStringList = nil;

procedure SeedFbKeywords;
// Fill GPPKeywords once with the inventory in FbReservedWords.inc. Sorted, so the lookups above are a
// binary search and not a walk of 366 strings per "#ifdef".
{$I FbReservedWords.inc}
var
  i: Integer;
begin
  if GPPKeywords <> nil then Exit;
  GPPKeywords := TStringList.Create;
  GPPKeywords.CaseSensitive := False;
  GPPKeywords.Duplicates := dupIgnore;
  for i := Low(FB_RESERVED_WORDS) to High(FB_RESERVED_WORDS) do
    GPPKeywords.Add(FB_RESERVED_WORDS[i]);
  GPPKeywords.Sorted := True;
end;

function DeclaredNameOfLine(const U: string; out Kind, TypeName: string): string;
// Read ONE upper-cased source line as a declaration and return the NAME it declares ('' if it declares
// nothing). Kind is the declaring keyword; TypeName the "As <type>" it gives, when there is one.
//
// The name has to be located POSITIONALLY, not by searching the line: "Dim x As T" mentions both x and
// T, and a search for T would find that line and call T a variable. That is exactly the case the
// manual's own example tests ("isUDT(T)" true, "isVariable(T)" false).
var
  p, q: Integer;
  W: string;

  procedure SkipSpace;
  begin
    while (p <= Length(U)) and (U[p] = ' ') do Inc(p);
  end;

  function NextWord: string;
  begin
    SkipSpace;
    q := p;
    while (p <= Length(U)) and IsIdentChar(U[p]) do Inc(p);
    Result := Copy(U, q, p - q);
  end;

begin
  Result := ''; Kind := ''; TypeName := '';
  p := 1;
  W := NextWord;
  if W = 'DECLARE' then W := NextWord;                 // "Declare Sub f(...)": the kind follows
  if (W = 'PRIVATE') or (W = 'PUBLIC') then W := NextWord;
  Kind := W;
  if (W = 'TYPE') or (W = 'UNION') or (W = 'ENUM') or (W = 'CLASS') then
  begin
    Result := NextWord;                                // "Type T", "Enum e"
    Exit;
  end;
  if (W = 'SUB') or (W = 'FUNCTION') or (W = 'PROPERTY') then
  begin
    Result := NextWord;
    Exit;
  end;
  if (W = 'CONST') or (W = 'DIM') or (W = 'STATIC') or (W = 'VAR') or (W = 'COMMON') or
     (W = 'REDIM') then
  begin
    SkipSpace;
    if Copy(U, p, 7) = ' SHARED' then Inc(p, 7);
    W := NextWord;
    if W = 'SHARED' then W := NextWord;
    if W = 'AS' then
    begin
      // Type-first form "Dim As T name": read the type, then the name.
      TypeName := NextWord;
      while (p <= Length(U)) and (Copy(U, p, 4) = ' PTR') do begin TypeName := TypeName + ' PTR'; Inc(p, 4); end;
      Result := NextWord;
    end
    else
    begin
      // Name-first form "Dim name As T" (the type is optional).
      Result := W;
      SkipSpace;
      if NextWord = 'AS' then TypeName := NextWord;
    end;
    Exit;
  end;
  Kind := '';
end;

function QuerySymbol(What: Integer; const Sym: string; Defs: TStringList): string;
// __FB_QUERY_SYMBOL__(what, sym): what fbc answers from its symbol table, answered here from the
// SOURCE. Our preprocessor runs on text, before any symbol table exists, so the classification is a
// declaration-shaped scan - the same footing SourceDeclaresSymbol already stands on for Defined().
// Encodings follow inc/fbc-int/symbol.bi: 0 symbclass, 1 datatype, 2 dataclass, 3/4 typename, 6 exists.
const
  SC_VAR = 1; SC_CONST = 2; SC_PROC = 3; SC_DEFINE = 5; SC_ENUM = 9; SC_STRUCT = 10;
  DC_INTEGER = 0; DC_FPOINT = 1; DC_STRING = 2; DC_UDT = 3; DC_PROC = 4; DC_UNKNOWN = 5;
  DT_VOID = 0; DT_BOOLEAN = 1; DT_BYTE = 2; DT_UBYTE = 3; DT_SHORT = 5; DT_USHORT = 6;
  DT_INTEGER = 8; DT_UINT = 9; DT_ENUM = 10; DT_LONG = 11; DT_ULONG = 12; DT_LONGINT = 13;
  DT_ULONGINT = 14; DT_SINGLE = 15; DT_DOUBLE = 16; DT_STRING = 17; DT_STRUCT = 20;
  DT_FUNCTION = 22; DT_POINTER = 24;
var
  L: TStringList;
  i, SymClass, DataClass, DataType: Integer;
  SymU, Nm, Kind, TypeName, FoundType: string;

  procedure ClassifyType(const T: string);
  begin
    FoundType := T;
    if T = '' then begin DataClass := DC_UNKNOWN; DataType := DT_VOID; Exit; end;
    if Pos(' PTR', T) > 0 then begin DataClass := DC_INTEGER; DataType := DT_POINTER; Exit; end;
    DataClass := DC_INTEGER;
    if      T = 'BOOLEAN'   then DataType := DT_BOOLEAN
    else if T = 'BYTE'      then DataType := DT_BYTE
    else if T = 'UBYTE'     then DataType := DT_UBYTE
    else if T = 'SHORT'     then DataType := DT_SHORT
    else if T = 'USHORT'    then DataType := DT_USHORT
    else if T = 'INTEGER'   then DataType := DT_INTEGER
    else if T = 'UINTEGER'  then DataType := DT_UINT
    else if T = 'LONG'      then DataType := DT_LONG
    else if T = 'ULONG'     then DataType := DT_ULONG
    else if T = 'LONGINT'   then DataType := DT_LONGINT
    else if T = 'ULONGINT'  then DataType := DT_ULONGINT
    else if T = 'SINGLE'    then begin DataType := DT_SINGLE; DataClass := DC_FPOINT; end
    else if T = 'DOUBLE'    then begin DataType := DT_DOUBLE; DataClass := DC_FPOINT; end
    else if (T = 'STRING') or (T = 'ZSTRING') or (T = 'WSTRING') then
                                 begin DataType := DT_STRING; DataClass := DC_STRING; end
    else begin DataType := DT_STRUCT; DataClass := DC_UDT; end;   // a declared TYPE name
  end;

begin
  Result := '0';
  SymU := UpperCase(Trim(Sym));
  if SymU = '' then Exit;
  // A QUALIFIED name - "T.l", "t1.l" - is a FIELD, and what is asked of it is the field's own class.
  // The scan below matches a declaration by its NAME, and a field is declared inside its type under
  // the bare one, so ask under that. (It cannot tell two types' fields of the same name apart; the
  // scan is declaration-shaped, not a symbol table, and says so at the top of this routine.)
  if Pos('.', SymU) > 0 then
    while Pos('.', SymU) > 0 do SymU := Copy(SymU, Pos('.', SymU) + 1, MaxInt);
  SymClass := 0; DataClass := DC_UNKNOWN; DataType := DT_VOID; FoundType := '';

  // A #define is a symbol too, and it is the one kind we hold outright rather than infer.
  if Defs.IndexOfName(SymU) >= 0 then
  begin
    SymClass := SC_DEFINE;
    DataClass := DC_INTEGER; DataType := DT_INTEGER;
  end;

  if SymClass = 0 then
  begin
    L := TStringList.Create;
    try
      L.Text := GPPSourceForDefined;
      for i := 0 to L.Count - 1 do
      begin
        Nm := DeclaredNameOfLine(UpperCase(TrimLeft(L[i])), Kind, TypeName);
        if Nm <> SymU then Continue;
        if (Kind = 'TYPE') or (Kind = 'UNION') or (Kind = 'CLASS') then
        begin
          SymClass := SC_STRUCT; DataClass := DC_UDT; DataType := DT_STRUCT; FoundType := SymU;
        end
        else if Kind = 'ENUM' then
        begin
          SymClass := SC_ENUM; DataClass := DC_INTEGER; DataType := DT_ENUM; FoundType := SymU;
        end
        else if (Kind = 'SUB') or (Kind = 'FUNCTION') or (Kind = 'PROPERTY') then
        begin
          SymClass := SC_PROC; DataClass := DC_PROC; DataType := DT_FUNCTION;
        end
        else if Kind = 'CONST' then
        begin
          SymClass := SC_CONST; ClassifyType(TypeName);
        end
        else
        begin
          SymClass := SC_VAR; ClassifyType(TypeName);
        end;
        Break;
      end;
    finally
      L.Free;
    end;
  end;

  case What of
    0: Result := IntToStr(SymClass);                       // symbclass
    1: Result := IntToStr(DataType);                       // datatype
    2: Result := IntToStr(DataClass);                      // dataclass
    3, 4: Result := FoundType;                             // typename / typenameid
    6: if SymClass <> 0 then Result := '-1' else Result := '0';   // exists
  end;
end;

function SourceConstValue(const Nm: string; out V: Int64): Boolean;
// The VALUE of a module-level "Const <Nm> = <integer literal>" written in the source, for a #if / #assert
// that names it. fbc's preprocessor can read a Const because its symbol table is being built as it goes;
// this one runs on TEXT, so it asks the same declaration-shaped scan Defined() already stands on.
//
// ⛔ WHY IT IS NEEDED AT ALL: an identifier the evaluator does not know becomes 0, so "#assert N = 5"
// on a Const N answered "0 = 5" and refused the program. fbc's pp/macro-no-params is exactly that, and
// it is a COMPILE_ONLY_OK test. ⚠️ The 0 default is kept for everything else - "#if undeclaredid1 =
// undeclaredid2" depends on it - so only a name a Const really declares changes answer.
var
  L: TStringList;
  i, p, q: Integer;
  U, W, Rest: string;
begin
  Result := False;
  V := 0;
  if Nm = '' then Exit;
  L := TStringList.Create;
  try
    L.Text := GPPSourceForDefined;
    for i := 0 to L.Count - 1 do
    begin
      U := UpperCase(Trim(L[i]));
      p := 1;
      while (p <= Length(U)) and IsIdentChar(U[p]) do Inc(p);
      W := Copy(U, 1, p - 1);
      if W <> 'CONST' then Continue;
      Rest := Trim(Copy(U, p, MaxInt));
      // "Const AS <type> name = v" names the type first; step over it.
      if Copy(Rest, 1, 3) = 'AS ' then
      begin
        Rest := Trim(Copy(Rest, 4, MaxInt));
        q := 1;
        while (q <= Length(Rest)) and IsIdentChar(Rest[q]) do Inc(q);
        Rest := Trim(Copy(Rest, q, MaxInt));
      end;
      q := 1;
      while (q <= Length(Rest)) and IsIdentChar(Rest[q]) do Inc(q);
      if Trim(Copy(Rest, 1, q - 1)) <> Nm then Continue;
      Rest := Trim(Copy(Rest, q, MaxInt));
      if (Rest = '') or (Rest[1] <> '=') then Continue;
      Rest := Trim(Copy(Rest, 2, MaxInt));
      // Only a plain integer literal: anything else is an expression this stage cannot fold, and
      // answering it wrongly would be worse than leaving the old 0.
      if TryStrToInt64(Rest, V) then Exit(True);
    end;
  finally
    L.Free;
  end;
end;

function PPNameIsDefined(const Nm: string; Defs, FnDefs: TStringList): Boolean; forward;

function SourceDeclaresSymbol(const Nm: string): Boolean;
// fbc's Defined() answers TRUE for COMPILER-level symbols too, not only #defines: a Const, a
// Dim/Redim/Static variable, a Sub/Function name (fbc-verified: examples/manual/prepro/defined
// expects a Const and a Dim to count). This preprocessor runs on TEXT before any symbol table
// exists, so the question is answered by a declaration-shaped scan: a line whose first word is
// a declaring keyword and that contains Nm as a whole word. A name inside a same-line comment
// or string can false-positive - accepted for a #if convenience predicate.
//
// ⛔⛔ AND IT IS A QUESTION ABOUT A POSITION, not about the file. fbc answers from the symbol table
// as built SO FAR, so "defined( T )" is FALSE above "Type T" and TRUE below it - and its own
// pp/defined-udt asserts BOTH, for the same name, in the same file. Scanning every line made the
// first one TRUE and the file refused itself with "#error". GPPDefinedLimit is the line the question
// is being asked FROM; -1 (the default) means the whole file, which is what every caller that has no
// position gets.
//
// ⛔ A MEMBER OF A TYPE IS NOT A MODULE SYMBOL - and it IS a symbol from inside the type's own body.
// fbc answers FALSE for a field / static field / method / property asked at module level, and TRUE
// for the same name asked between "Type T" and "End Type" once it has been declared. So a member hit
// is kept only while the body that made it is still OPEN at the asking position; a body that closed
// before it takes its members with it.
//
// A QUALIFIED name is a third question again: "T.datafield" is TRUE only when T is a type declared so
// far AND datafield has been declared inside its body so far. The member half accepts an OPERATOR
// name too ("T.+=", "T.new[]"), which is why it is matched as text rather than as an identifier.
//
// ⚠️ An ENUM body is NOT skipped: its members ARE module-level constants in FreeBASIC.
// A "Type mine As LongInt" alias opens no block and must not start one.
var
  L: TStringList;
  i, p, q, TypeDepth, EnumDepth, LastLine: Integer;
  U, W, Rest, Qual, Member, Want: string;
  MemberHit, QualIsType, QualMemberHit, InQualBody: Boolean;

  function WholeWordAt(const Hay, Needle: string; At: Integer): Boolean;
  // A match that is not glued to an identifier character - and not to a '.' either, because a
  // QUALIFIED name does not declare the bare one: "Sub T.proc()" is the out-of-line definition of a
  // member, written at module level, and fbc still answers FALSE for "defined( proc )".
  begin
    Result := ((At = 1) or (not IsIdentChar(Hay[At - 1]) and (Hay[At - 1] <> '.'))) and
              ((At + Length(Needle) > Length(Hay)) or
               (not IsIdentChar(Hay[At + Length(Needle)]) and (Hay[At + Length(Needle)] <> '.')));
  end;

  function LineMentions(const Hay, Needle: string): Boolean;
  var k: Integer;
  begin
    Result := False;
    k := Pos(Needle, Hay);
    while k > 0 do
    begin
      if WholeWordAt(Hay, Needle, k) then Exit(True);
      k := Pos(Needle, Hay, k + 1);
    end;
  end;

  function TokenAt(const Hay: string; var At: Integer): string;
  // The next word of a member declaration, stopping at whitespace or '(' - so an OPERATOR name comes
  // out whole ("+=", "[]", "NEW[]", "DELETE[]"), which no identifier scan would have reached.
  var b: Integer;
  begin
    while (At <= Length(Hay)) and (Hay[At] in [' ', #9]) do Inc(At);
    b := At;
    while (At <= Length(Hay)) and not (Hay[At] in [' ', #9, '(']) do Inc(At);
    Result := Copy(Hay, b, At - b);
  end;

  function MemberNameOfLine(const Hay: string): string;
  // The member ONE line inside a type body declares, or ''. It has to be located POSITIONALLY: a
  // member hit taken from any line that MENTIONS the name made "check_N( datafield )" - the very
  // question - declare its own subject, and every check inside the body answered TRUE before a single
  // field had been written.
  var
    At: Integer;
    W1, W2: string;
  begin
    Result := '';
    At := 1;
    W1 := TokenAt(Hay, At);
    if W1 = 'DECLARE' then
    begin
      W2 := TokenAt(Hay, At);                       // SUB / FUNCTION / PROPERTY / CTOR / DTOR / OPERATOR
      if (W2 = 'CONSTRUCTOR') or (W2 = 'DESTRUCTOR') then Exit(W2);
      Exit(TokenAt(Hay, At));                       // the name, or the operator's own token
    end;
    if (W1 = 'CONSTRUCTOR') or (W1 = 'DESTRUCTOR') then Exit(W1);
    if (W1 = 'STATIC') or (W1 = 'DIM') or (W1 = 'CONST') or (W1 = 'AS') then Exit(TokenAt(Hay, At));
    // "datafield as byte": the name first, its type after. Without the AS this is not a declaration
    // at all - it is a macro invocation, a comment or a nested block header.
    if (W1 <> '') and (Pos(' AS ', ' ' + Hay + ' ') > 0) then Exit(W1);
  end;

begin
  Result := False;
  if Nm = '' then Exit;
  // Split a qualified name once: "T.DATAFIELD" -> Qual="T", Member="DATAFIELD". An operator member
  // ("T.+=") leaves Member holding the operator text, matched as a substring below.
  p := Pos('.', Nm);
  if p > 0 then
  begin
    Qual := Copy(Nm, 1, p - 1);
    Member := Copy(Nm, p + 1, MaxInt);
    if (Qual = '') or (Member = '') then Exit;
  end
  else
  begin
    Qual := '';
    Member := '';
  end;
  L := TStringList.Create;
  try
    // The expansion so far when there is one, the source truncated at the asking line otherwise.
    if (GPPOutput <> nil) and (GPPOutput.Count > 0) then
    begin
      L.Assign(GPPOutput);
      LastLine := L.Count - 1;
    end
    else
    begin
      L.Text := GPPSourceForDefined;
      LastLine := L.Count - 1;
      if (GPPDefinedLimit >= 0) and (GPPDefinedLimit < LastLine) then LastLine := GPPDefinedLimit;
    end;
    TypeDepth := 0;
    EnumDepth := 0;
    MemberHit := False;
    QualIsType := False;
    QualMemberHit := False;
    InQualBody := False;
    if Qual = '' then Want := Nm else Want := Qual;
    for i := 0 to LastLine do
    begin
      U := UpperCase(TrimLeft(L[i]));
      p := 1;
      while (p <= Length(U)) and IsIdentChar(U[p]) do Inc(p);
      W := Copy(U, 1, p - 1);
      if W = 'END' then
      begin
        Rest := Trim(Copy(U, p, MaxInt));
        if (Copy(Rest, 1, 4) = 'TYPE') or (Copy(Rest, 1, 5) = 'UNION') or
           (Copy(Rest, 1, 5) = 'CLASS') then
          if TypeDepth > 0 then
          begin
            Dec(TypeDepth);
            if TypeDepth = 0 then
            begin
              MemberHit := False;      // the body closed: its members are not module symbols
              InQualBody := False;
            end;
          end;
      end;
      if TypeDepth > 0 then
      begin
        // Inside a type body: these are MEMBERS. They answer only from in here, and only for a body
        // that is still open where the question is asked - which is what the reset above enforces.
        Rest := MemberNameOfLine(U);
        if (Qual = '') and (Rest = Nm) then MemberHit := True;
        if InQualBody and (Rest = Member) then QualMemberHit := True;
        Continue;
      end;
      // ⭐ ...BUT AN ENUM BODY DECLARES MODULE CONSTANTS, one per line, and the member's own name is
      // the FIRST word - so the declaring-keyword rule below cannot see it and defined() answered
      // FALSE for every enum member. fbc answers TRUE. Verified against the oracle.
      if EnumDepth > 0 then
      begin
        if (W <> '') and (W <> 'END') and (W = Want) and (Qual = '') then Exit(True);
        if W = 'END' then
        begin
          Rest := Trim(Copy(U, p, MaxInt));
          if Copy(Rest, 1, 4) = 'ENUM' then Dec(EnumDepth);
        end;
        Continue;
      end;
      if (W = 'ENUM') and (Pos(' AS ', ' ' + Trim(Copy(U, p, MaxInt)) + ' ') = 0) then
      begin
        // the enum's own NAME is a symbol; its members are read on the following lines
        q := Pos(Want, U);
        while q > 0 do
        begin
          if ((q = 1) or not IsIdentChar(U[q - 1])) and
             ((q + Length(Want) > Length(U)) or not IsIdentChar(U[q + Length(Want)])) then
            if Qual = '' then Exit(True);
          q := Pos(Want, U, q + 1);
        end;
        Inc(EnumDepth);
        Continue;
      end;
      if (W = 'CONST') or (W = 'DIM') or (W = 'REDIM') or (W = 'STATIC') or (W = 'VAR') or
         (W = 'SUB') or (W = 'FUNCTION') or (W = 'DECLARE') or (W = 'TYPE') or
         (W = 'ENUM') or (W = 'COMMON') then
        if (Qual = '') and LineMentions(U, Nm) then Exit(True);
      // ...and only AFTER the line has been read: "Type T" declares T itself, which IS a symbol, and
      // fbc's own test asks for it from inside the body ("check_Y( T )").
      if (W = 'TYPE') or (W = 'UNION') or (W = 'CLASS') then
      begin
        Rest := Trim(Copy(U, p, MaxInt));
        // "Type mine As LongInt" is an ALIAS - one line, no block. Anything else opens one.
        if Pos(' AS ', ' ' + Rest + ' ') = 0 then
        begin
          Inc(TypeDepth);
          if (Qual <> '') and (TypeDepth = 1) and LineMentions(U, Qual) then
          begin
            QualIsType := True;
            InQualBody := True;
          end;
        end;
      end;
    end;
    if Qual <> '' then
      Result := QualIsType and QualMemberHit
    else
      Result := MemberHit and (TypeDepth > 0);
  finally
    L.Free;
  end;
end;

function PPNameIsDefined(const Nm: string; Defs, FnDefs: TStringList): Boolean;
// Is this name DEFINED, in the sense fbc's Defined() and #ifdef both mean? Four storages, one
// question: an object-like macro (Defs), a function-like macro (FnDefs - "#macro m(a)" and
// "#define f(a)" live only there, and asking Defs alone answered 0 for a macro written three lines
// above), a "#pragma reserve" reservation that is still in scope (GPPReserved; a 'q' in the value
// marks one that has been un-reserved), a word the LANGUAGE reserves (GPPKeywords), or a declaration
// the source itself makes above this point (SourceDeclaresSymbol).
begin
  Result := ((Defs <> nil) and (Defs.IndexOfName(Nm) >= 0)) or
            ((FnDefs <> nil) and (FnDefs.IndexOfName(Nm) >= 0)) or
            ((GPPReserved <> nil) and (GPPReserved.IndexOfName(Nm) >= 0) and
             (Pos('q', GPPReserved.Values[Nm]) = 0)) or
            ((GPPKeywords <> nil) and (GPPKeywords.IndexOf(Nm) >= 0)) or
            SourceDeclaresSymbol(Nm);
end;

// Evaluate a #if / #elif constant integer expression. Supports: decimal and &H/&O/&B literals;
// defined(NAME) / defined NAME; bare macro names (-> their integer value, or 0 if undefined or
// non-numeric); parentheses; unary "-"/"+" and NOT/"!"; "*" "/" "\" MOD; "+" "-"; comparisons
// "=" "==" "<>" "!=" "<" "<=" ">" ">="; AND/"&&"; OR/"||". Nonzero result => take the branch. On any
// problem it returns False (safe default: branch not taken).
function EvalPPExprInt(const RawExpr: string; Defs: TStringList; out V: Int64;
  FnDefs: TStringList = nil): Boolean; forward;

function EvalPPExpr(const RawExpr: string; Defs: TStringList;
  FnDefs: TStringList = nil): Boolean;
// #if / #elseif: the expression as a CONDITION.
var
  V: Int64;
begin
  Result := EvalPPExprInt(RawExpr, Defs, V, FnDefs) and (V <> 0);
end;

const
  cPPStrTok = #2;   // leading byte marking a tokenized STRING LITERAL; no source character can be this

function DottedTail(const S: string; P: Integer): string;
// The identifier that follows the '.' at P, or ''. Used to try a DOTTED macro key before the bare one.
var q: Integer;
begin
  Result := '';
  if (P > Length(S)) or (S[P] <> '.') then Exit;
  q := P + 1;
  while (q <= Length(S)) and IsIdentChar(S[q]) do Inc(q);
  if q > P + 1 then Result := UpperCase(Copy(S, P + 1, q - P - 1));
end;

function NextNonBlankIsOpenParen(const S: string; P: Integer): Boolean;
// Is the next non-blank character at or after P an opening parenthesis? Tells a function-like macro
// INVOCATION from a bare mention of its name.
begin
  while (P <= Length(S)) and (S[P] in [' ', #9]) do Inc(P);
  Result := (P <= Length(S)) and (S[P] = '(');
end;

function GatherBalancedParens(const S: string; var P: Integer): string;
// The "( ... )" starting at P, parentheses balanced, quotes respected. P is left past the closing one.
var
  Depth: Integer;
  InStr: Boolean;
begin
  Result := '';
  if (P > Length(S)) or (S[P] <> '(') then Exit;
  Depth := 0; InStr := False;
  while P <= Length(S) do
  begin
    Result := Result + S[P];
    if S[P] = '"' then InStr := not InStr
    else if not InStr then
    begin
      if S[P] = '(' then Inc(Depth)
      else if S[P] = ')' then
      begin
        Dec(Depth);
        if Depth = 0 then begin Inc(P); Exit; end;
      end;
    end;
    Inc(P);
  end;
end;

function EvalPPExprInt(const RawExpr: string; Defs: TStringList; out V: Int64;
  FnDefs: TStringList = nil): Boolean;
// ...and as a VALUE, which is what "__FB_EVAL__(expr)" needs: it substitutes the RESULT of a constant
// integer expression, so another macro can take it as an argument. False when there is nothing to
// evaluate. The two entry points share one parser - the condition is simply "the value is non-zero".
var
  Toks: TStringList;
  TPos: Integer;

  function NumOf(const S: string): Int64;
  begin
    if (Length(S) >= 2) and (S[1] = '&') then
      case UpCase(S[2]) of
        'H': Result := StrToInt64Def('$' + Copy(S, 3, MaxInt), 0);
        'O': Result := StrToInt64Def('&' + Copy(S, 3, MaxInt), 0);   // FPC octal prefix '&'
        'B': Result := StrToInt64Def('%' + Copy(S, 3, MaxInt), 0);   // FPC binary prefix '%'
      else Result := 0;
      end
    else Result := StrToInt64Def(S, 0);
  end;

  // Tokenize, substituting defined()/macros into numeric tokens as we go. A macro's value is
  // re-tokenized (depth-guarded) rather than added as one token, so values like "-1" (-> '-' '1'),
  // "&HFF", or "1 + 2" parse correctly and nested macros expand.
  procedure Tokenize(const S: string; Depth: Integer);
  var p, q: Integer; id, two: string; nm: string; ConstV: Int64;
  begin
    p := 1;
    while p <= Length(S) do
    begin
      if S[p] in [' ', #9] then begin Inc(p); Continue; end;
      // multi-char operators
      if p < Length(S) then
      begin
        two := Copy(S, p, 2);
        if (two = '==') or (two = '<>') or (two = '!=') or (two = '<=') or (two = '>=') or
           (two = '&&') or (two = '||') then
        begin Toks.Add(two); Inc(p, 2); Continue; end;
      end;
      // A STRING LITERAL, kept as one token behind a marker byte that no source text can produce.
      // It used to be skipped character by character, so comparing a stringized macro argument
      // against an empty literal compared NOTHING against nothing: the quotes vanished, the empty
      // case left a lone '=' (false), and the non-empty case left "0 =" whose missing right operand
      // read as 0 - so the condition came out true exactly when it should have been false. An
      // optional '$'/'!' prefix is FreeBASIC's escaped/non-escaped literal marker, and stringizing
      // produces the '$' form.
      if (S[p] = '"') or (((S[p] = '$') or (S[p] = '!')) and (p < Length(S)) and (S[p + 1] = '"')) then
      begin
        q := p; if S[q] <> '"' then Inc(q);
        Inc(q);                                     // past the opening quote
        id := '';
        while q <= Length(S) do
        begin
          if S[q] = '"' then
          begin
            if (q < Length(S)) and (S[q + 1] = '"') then begin id := id + '"'; Inc(q, 2); Continue; end;
            Inc(q); Break;                          // closing quote
          end;
          id := id + S[q]; Inc(q);
        end;
        Toks.Add(cPPStrTok + id); p := q; Continue;
      end;
      if S[p] in ['=', '<', '>', '(', ')', '+', '-', '*', '/', '\', '!'] then
      begin Toks.Add(S[p]); Inc(p); Continue; end;
      // number (decimal or &H/&O/&B)
      if (S[p] in ['0'..'9']) or ((S[p] = '&') and (p < Length(S))) then
      begin
        q := p; if S[q] = '&' then Inc(q, 2);
        while (q <= Length(S)) and (UpCase(S[q]) in ['0'..'9', 'A'..'F']) do Inc(q);
        Toks.Add(Copy(S, p, q - p)); p := q; Continue;
      end;
      // identifier / keyword
      if IsIdentChar(S[p]) then
      begin
        q := p;
        while (q <= Length(S)) and IsIdentChar(S[q]) do Inc(q);
        id := UpperCase(Copy(S, p, q - p)); p := q;
        if id = 'DEFINED' then
        begin
          // defined(NAME) or defined NAME -> 1/0
          while (p <= Length(S)) and (S[p] in [' ', #9]) do Inc(p);
          if (p <= Length(S)) and (S[p] = '(') then Inc(p);
          while (p <= Length(S)) and (S[p] in [' ', #9]) do Inc(p);
          // ⭐ A LEADING '.' (or '..') asks for the GLOBAL scope, and for defined() it names the same
          // symbol: "defined( ..symbol )" is fbc's own spelling in pp/pragma-reserve-4. The dots are
          // not identifier characters, so the name came out EMPTY and every such test answered "not
          // defined" - and the file refused itself with #error.
          while (p <= Length(S)) and (S[p] = '.') do Inc(p);
          q := p;
          while (q <= Length(S)) and IsIdentChar(S[q]) do Inc(q);
          nm := UpperCase(Copy(S, p, q - p)); p := q;
          // ⛔ A QUALIFIED name is one name, and it was read as the BARE one with a tail left over.
          // "defined( T.datafield )" answered whatever "defined( T )" answered - so every one of the
          // fourteen "check_N( T.something )" in fbc's pp/defined-udt came out TRUE inside the type's
          // own body, where T certainly is defined. The member half may be an OPERATOR name
          // ("T.+=", "T.[]", "T.new[]"), which no identifier scan would ever reach: everything up to
          // the closing parenthesis or the end belongs to the name.
          if (p <= Length(S)) and (S[p] = '.') then
          begin
            Inc(p);                                  // the '.'
            q := p;
            while (q <= Length(S)) and (S[q] <> ')') and (S[q] <> ' ') and (S[q] <> #9) do Inc(q);
            nm := nm + '.' + UpperCase(Trim(Copy(S, p, q - p)));
            p := q;
          end;
          while (p <= Length(S)) and (S[p] in [' ', #9, ')']) do Inc(p);
          // ⛔ A FUNCTION-LIKE MACRO IS DEFINED TOO. "#macro m(a)" and "#define f(a) ..." live in
          // FnDefs, not Defs, and only Defs was consulted - so "defined(m)" answered 0 for a macro
          // that had just been written three lines above. The two tables are one QUESTION with two
          // storages; every place that asks "is this name a macro?" has to ask both.
          if PPNameIsDefined(nm, Defs, FnDefs) then
            Toks.Add('1')
          else
            Toks.Add('0');
        end
        else if id = 'TYPEOF' then
          // "#if TypeOf(a) = TypeOf(b)" asks a question only the compiler's symbol table can answer,
          // and this preprocessor runs on text, before any declaration has been seen. Falling through
          // to the undefined-identifier rule below would silently make every such condition FALSE --
          // including the ones that should be true. Say so instead. (The statement form,
          // "Dim As TypeOf(expr) name", is handled by the parser and works.)
          raise EPreprocessorError.Create(
            'TypeOf() in a #if condition is not supported: the preprocessor has no type information')
        else if (id = 'AND') or (id = 'OR') or (id = 'NOT') or (id = 'MOD') then
          Toks.Add(id)
        // A FUNCTION-LIKE macro INVOCATION: "__FB_MIN_VERSION__(0, 18, 2)". Only object-like macros
        // were substituted here, so a call like that read as an undefined identifier (0) followed by a
        // parenthesised list - and the condition came out FALSE whatever the arguments. fbc defines
        // __FB_MIN_VERSION__ as an ordinary function-like macro, and a module guarding itself with
        // "#if Not __FB_MIN_VERSION__(0,18,2)" therefore refused to compile against a version that
        // satisfies it. The call text is expanded through the ordinary substitution and re-tokenized,
        // exactly as an object-like macro's value is.
        else if (FnDefs <> nil) and (FnDefs.IndexOfName(id) >= 0) and
                (NextNonBlankIsOpenParen(S, p)) then
        begin
          q := p;
          while (q <= Length(S)) and (S[q] in [' ', #9]) do Inc(q);
          nm := id + GatherBalancedParens(S, q);   // q lands past the closing ')'
          p := q;
          if Depth < 32 then Tokenize(SubstituteMacros(nm, Defs, FnDefs, 0), Depth + 1)
          else Toks.Add('0');
        end
        // A DOTTED key ("FB_DATACLASS.FB_DATACLASS_UDT") is one name here too - the same rule the
        // line substituter follows, and a condition must not answer differently from the code below it.
        else if (p <= Length(S)) and (S[p] = '.') and
                (Defs.IndexOfName(id + '.' + DottedTail(S, p)) >= 0) then
        begin
          nm := id + '.' + DottedTail(S, p);
          p := p + 1 + Length(DottedTail(S, p));
          if Depth < 32 then Tokenize(Trim(Defs.Values[nm]), Depth + 1)
          else Toks.Add('0');
        end
        else if Defs.IndexOfName(id) >= 0 then
        begin
          // Re-tokenize the macro's value so multi-token values (-1, &HFF, 1+2) and nested
          // macros work; bail to 0 past a sane nesting depth (cycle guard).
          if Depth < 32 then Tokenize(Trim(Defs.Values[id]), Depth + 1)
          else Toks.Add('0');
        end
        // ⭐ ...unless the SOURCE declares it as a Const with an integer value. See SourceConstValue.
        else if SourceConstValue(id, ConstV) then
          Toks.Add(IntToStr(ConstV))
        else
          Toks.Add('0');                       // undefined identifier -> 0
        Continue;
      end;
      Inc(p);   // skip anything else
    end;
  end;

  function Peek: string;
  begin if TPos < Toks.Count then Result := Toks[TPos] else Result := ''; end;

  function IsNum(const S: string): Boolean;
  begin Result := (S <> '') and ((S[1] in ['0'..'9']) or (S[1] = '&')); end;

  function ParseOr: Int64; forward;

  function ParsePrimary: Int64;
  var t: string;
  begin
    t := Peek;
    // NB: the recursive self-calls MUST use parentheses — in {$mode objfpc} a bare `ParsePrimary`
    // refers to this function's Result variable (TP/Delphi compatibility), not a recursive call, so
    // `not`/unary `-`/`+` would read an uninitialised Result instead of their operand.
    if t = '(' then begin Inc(TPos); Result := ParseOr; if Peek = ')' then Inc(TPos); end
    else if (t = 'NOT') or (t = '!') then begin Inc(TPos); if ParsePrimary() <> 0 then Result := 0 else Result := 1; end
    else if t = '-' then begin Inc(TPos); Result := -ParsePrimary(); end
    else if t = '+' then begin Inc(TPos); Result := ParsePrimary(); end
    else if IsNum(t) then begin Result := NumOf(t); Inc(TPos); end
    else begin Inc(TPos); Result := 0; end;
  end;

  function ParseMul: Int64;
  var op: string; r: Int64;
  begin
    Result := ParsePrimary;
    while (Peek = '*') or (Peek = '/') or (Peek = '\') or (Peek = 'MOD') do
    begin
      op := Peek; Inc(TPos); r := ParsePrimary;
      if op = '*' then Result := Result * r
      else if r = 0 then Result := 0
      else if op = 'MOD' then Result := Result mod r
      else Result := Result div r;
    end;
  end;

  function ParseAdd: Int64;
  var op: string; r: Int64;
  begin
    Result := ParseMul;
    while (Peek = '+') or (Peek = '-') do
    begin op := Peek; Inc(TPos); r := ParseMul; if op = '+' then Result := Result + r else Result := Result - r; end;
  end;

  function IsStrTok(const T: string): Boolean;
  begin Result := (T <> '') and (T[1] = cPPStrTok); end;

  function StrTokText(const T: string): string;
  begin Result := Copy(T, 2, MaxInt); end;

  function ParseCmp: Int64;
  var op, ls, rs: string; l, r: Int64; b: Boolean;
  begin
    // A comparison whose LEFT side is a string literal is decided on the TEXT. Tested before ParseAdd
    // is asked for a number, because a string has no number to give. FreeBASIC's own idiom for "was
    // this variadic argument passed?" compares the stringized argument against an EMPTY literal, and
    // it can only work this way.
    if IsStrTok(Peek) then
    begin
      ls := StrTokText(Peek); Inc(TPos);
      Result := 0;
      while (Peek='=') or (Peek='==') or (Peek='<>') or (Peek='!=') or (Peek='<') or (Peek='<=') or (Peek='>') or (Peek='>=') do
      begin
        op := Peek; Inc(TPos);
        if IsStrTok(Peek) then begin rs := StrTokText(Peek); Inc(TPos); end
        else begin rs := ''; Inc(TPos); end;
        if (op='=') or (op='==') then b := ls = rs
        else if (op='<>') or (op='!=') then b := ls <> rs
        else if op='<' then b := ls < rs
        else if op='<=' then b := ls <= rs
        else if op='>' then b := ls > rs
        else b := ls >= rs;
        if b then Result := 1 else Result := 0;
        ls := rs;
      end;
      Exit;
    end;
    Result := ParseAdd;
    while (Peek='=') or (Peek='==') or (Peek='<>') or (Peek='!=') or (Peek='<') or (Peek='<=') or (Peek='>') or (Peek='>=') do
    begin
      op := Peek; Inc(TPos);
      // ⛔ A NUMBER ON THE LEFT AND A STRING ON THE RIGHT IS NOT EQUAL - and the case matters because
      // an UNDEFINED identifier tokenizes as 0. "#if __FB_BACKEND__ = "gas"" with the macro missing
      // therefore read as 0 = 0 and came out TRUE, so we compiled the branch fbc does not: the string
      // rule lived in the LEFT-hand path only, and ParsePrimary's discard arm answered 0 for the
      // literal on the right. Measured against fbc 1.10.1: "UNKNOWN = "gas"" is FALSE, "UNKNOWN <>
      // "gas"" is TRUE, and "0 = "0"" is a type-mismatch ERROR it refuses outright - the one case we
      // are deliberately more permissive about, since a #if must still answer something.
      if IsStrTok(Peek) then
      begin
        Inc(TPos);
        if (op='<>') or (op='!=') then Result := 1 else Result := 0;
        Continue;
      end;
      l := Result; r := ParseAdd;
      if (op='=') or (op='==') then b := l = r
      else if (op='<>') or (op='!=') then b := l <> r
      else if op='<' then b := l < r
      else if op='<=' then b := l <= r
      else if op='>' then b := l > r
      else b := l >= r;
      if b then Result := 1 else Result := 0;
    end;
  end;

  function ParseAnd: Int64;
  var r: Int64;
  begin
    Result := ParseCmp;
    while (Peek = 'AND') or (Peek = '&&') do
    begin Inc(TPos); r := ParseCmp; if (Result <> 0) and (r <> 0) then Result := 1 else Result := 0; end;
  end;

  function ParseOr: Int64;
  var r: Int64;
  begin
    Result := ParseAnd;
    while (Peek = 'OR') or (Peek = '||') do
    begin Inc(TPos); r := ParseAnd; if (Result <> 0) or (r <> 0) then Result := 1 else Result := 0; end;
  end;

begin
  Result := False;
  V := 0;
  Toks := TStringList.Create;
  try
    Tokenize(RawExpr, 0);
    if Toks.Count = 0 then Exit;
    TPos := 0;
    V := ParseOr;
    Result := True;
  finally
    Toks.Free;
  end;
end;

function PPConstIntStr(const Expr: string; Defs: TStringList): string;
// The value of a constant expression as text, or the expression unchanged when it is not one. This is
// what __FB_EVAL__ substitutes, and fbc folds integers, floats WITH the maths intrinsics, and strings -
// all three are here now. (This comment used to end "we fold integers, and say so by not pretending";
// the honesty was right and the limit is gone.)
//
// "Not a constant expression" has to be tested up front for the NUMERIC forms: the tokenizer resolves an
// unknown identifier to 0, so asking it to evaluate "4 * Atn(1)" before Atn was known would answer 0 - a
// wrong VALUE where the honest answer is "I do not fold this".
var
  V: Int64;
  F: Double;
  i, j: Integer;
  W: string;
begin
  Result := Trim(Expr);
  // ⛔ THE STRING FOLD COMES FIRST, before the identifier scan below - that scan walks the WHOLE text,
  // quoted parts included, so the letters inside "a" + "b" looked like unknown macros and refused the
  // expression before anything could fold it.
  if (Pos('"', Expr) > 0) and PPConstStrFold(Expr, Defs, W) then Exit(W);
  i := 1;
  while i <= Length(Expr) do
    if Expr[i] in ['A'..'Z', 'a'..'z', '_'] then
    begin
      j := i;
      while (j <= Length(Expr)) and IsIdentChar(Expr[j]) do Inc(j);
      W := UpperCase(Copy(Expr, i, j - i));
      if (W <> 'MOD') and (W <> 'AND') and (W <> 'OR') and (W <> 'NOT') and
         (W <> 'SHL') and (W <> 'SHR') and (not IsPPMathFn(W)) and (Defs.IndexOfName(W) < 0) then Exit;
      i := j;
    end
    else
      Inc(i);
  // ⛔ WHICH EVALUATOR ANSWERS IS DECIDED BY THE TEXT, not by trying one and falling through. The
  // integer one always ANSWERS - "10 / 4" is 2 to it and "4 * Atn(1)" is 0, Atn being an unknown name -
  // so asking it first meant it always won and the float one was never reached. A '.' , a '/' or a
  // maths function makes the expression a FLOAT expression; anything else stays integer, which keeps a
  // big literal exact (a Double cannot hold every Int64) and keeps __FB_ARG_EXTRACT__'s index whole.
  if IsPPFloatExpr(Expr) then
  begin
    if EvalPPExprFloat(Expr, Defs, F) then Result := FormatDoubleFB(F, 16);
    Exit;
  end;
  if EvalPPExprInt(Expr, Defs, V) then Result := IntToStr(V);
end;

procedure RegisterEmulatedHeader(const FileName: string; Defs, FnDefs: TStringList);
// A FreeBASIC header we do not ship, but whose CONTENT we implement anyway.
//
// An #include of a file that is not there is dropped in silence, which is the right thing for headers
// that only declare things we provide natively - but not for one that #defines CONSTANTS. A program
// that includes "dir.bi" and then passes fbDirectory to DIR would be passing an undefined name, i.e.
// zero: a different search that runs perfectly happily and lists the wrong entries.
//
// So the few headers that are pure constants are emulated here, and ONLY when the include actually
// asks for them - a program that never includes dir.bi keeps fbDirectory as an ordinary name it may
// declare itself, exactly as under fbc. Keys are UPPER: the macro lookup upper-cases the word.
var
  Base: string;
begin
  Base := LowerCase(ExtractFileName(FileName));
  // fbc-int/symbol.bi exposes __FB_QUERY_SYMBOL__ through convenience macros. The real header wraps
  // everything in "namespace FBC" and reaches the selectors as "fbc.FB_QUERY_SYMBOL.symbclass"; what a
  // program actually WRITES are the isXXX macros, so those are what is emulated - with the selector
  // folded in as a literal. Values follow the header's own enums (symbclass=0, dataclass=2, datatype=1).
  if (Base = 'symbol.bi') and (Pos('fbc-int', LowerCase(FileName)) > 0) then
  begin
    // The isXXX convenience macros, as function-like macros ("params"#1"body"), exactly as a #define
    // of the same shape would have registered them.
    FnDefs.Values['ISVARIABLE']        := 'sym'#1'(__FB_QUERY_SYMBOL__(0, sym) = 1)';
    FnDefs.Values['ISCONST']           := 'sym'#1'(__FB_QUERY_SYMBOL__(0, sym) = 2)';
    FnDefs.Values['ISPROCEDURE']       := 'sym'#1'(__FB_QUERY_SYMBOL__(0, sym) = 3)';
    FnDefs.Values['ISNAMESPACE']       := 'sym'#1'(__FB_QUERY_SYMBOL__(0, sym) = 8)';
    FnDefs.Values['ISENUM']            := 'sym'#1'(__FB_QUERY_SYMBOL__(0, sym) = 9)';
    FnDefs.Values['ISUDT']             := 'sym'#1'(__FB_QUERY_SYMBOL__(0, sym) = 10)';
    FnDefs.Values['ISDATACLASSINTEGER']:= 'sym'#1'(__FB_QUERY_SYMBOL__(2, sym) = 0)';
    FnDefs.Values['ISDATACLASSFLOAT']  := 'sym'#1'(__FB_QUERY_SYMBOL__(2, sym) = 1)';
    FnDefs.Values['ISDATACLASSSTRING'] := 'sym'#1'(__FB_QUERY_SYMBOL__(2, sym) = 2)';
    FnDefs.Values['ISDATACLASSUDT']    := 'sym'#1'(__FB_QUERY_SYMBOL__(2, sym) = 3)';
    FnDefs.Values['ISDATACLASSPROC']   := 'sym'#1'(__FB_QUERY_SYMBOL__(2, sym) = 4)';
    FnDefs.Values['ISTYPEINTEGER']     := 'sym'#1'(__FB_QUERY_SYMBOL__(1, sym) = 8)';
    FnDefs.Values['ISTYPEDOUBLE']      := 'sym'#1'(__FB_QUERY_SYMBOL__(1, sym) = 16)';
    FnDefs.Values['ISTYPESINGLE']      := 'sym'#1'(__FB_QUERY_SYMBOL__(1, sym) = 15)';
    FnDefs.Values['ISTYPESTRING']      := 'sym'#1'(__FB_QUERY_SYMBOL__(1, sym) = 17)';
    FnDefs.Values['ISSYMBOL']          := 'sym'#1'(__FB_QUERY_SYMBOL__(6, sym))';
    Defs.Values['FB_SYMBCLASS_VAR']       := '1';
    Defs.Values['FB_SYMBCLASS_CONST']     := '2';
    Defs.Values['FB_SYMBCLASS_PROC']      := '3';
    Defs.Values['FB_SYMBCLASS_NAMESPACE'] := '8';
    Defs.Values['FB_SYMBCLASS_ENUM']      := '9';
    Defs.Values['FB_SYMBCLASS_STRUCT']    := '10';
    Defs.Values['FB_DATACLASS_INTEGER']   := '0';
    Defs.Values['FB_DATACLASS_FPOINT']    := '1';
    Defs.Values['FB_DATACLASS_FLOAT']     := '1';
    Defs.Values['FB_DATACLASS_STRING']    := '2';
    Defs.Values['FB_DATACLASS_UDT']       := '3';
    Defs.Values['FB_DATACLASS_PROC']      := '4';
    // ...and the ENUM-QUALIFIED spelling, which is what the header really declares and what the
    // manual's own fbquerysymbol2 writes: "FB_DATACLASS.FB_DATACLASS_INTEGER". In the real header these
    // are members of "Enum FB_DATACLASS" inside "Namespace FBC"; emulated as macros they need the
    // qualified name as a key of its own, or the substitution would leave "FB_DATACLASS.0".
    Defs.Values['FB_DATACLASS.FB_DATACLASS_INTEGER'] := '0';
    Defs.Values['FB_DATACLASS.FB_DATACLASS_FPOINT']  := '1';
    Defs.Values['FB_DATACLASS.FB_DATACLASS_FLOAT']   := '1';
    Defs.Values['FB_DATACLASS.FB_DATACLASS_STRING']  := '2';
    Defs.Values['FB_DATACLASS.FB_DATACLASS_UDT']     := '3';
    Defs.Values['FB_DATACLASS.FB_DATACLASS_PROC']    := '4';
    Defs.Values['FB_SYMBCLASS.FB_SYMBCLASS_VAR']       := '1';
    Defs.Values['FB_SYMBCLASS.FB_SYMBCLASS_CONST']     := '2';
    Defs.Values['FB_SYMBCLASS.FB_SYMBCLASS_PROC']      := '3';
    Defs.Values['FB_SYMBCLASS.FB_SYMBCLASS_NAMESPACE'] := '8';
    Defs.Values['FB_SYMBCLASS.FB_SYMBCLASS_ENUM']      := '9';
    Defs.Values['FB_SYMBCLASS.FB_SYMBCLASS_STRUCT']    := '10';
    // The SELECTOR enum, which is what a program actually writes: "__FB_QUERY_SYMBOL__(
    // FB_QUERY_SYMBOL.dataclass, sym )". Unregistered it folded to 0, so every query asked for
    // symbclass whatever the source said - the manual's fbquerysymbol2 printed "integer" for a Double,
    // a String and a UDT alike. Values are the header's own (symbclass 0, datatype 1, dataclass 2).
    Defs.Values['FB_QUERY_SYMBOL.SYMBCLASS']  := '0';
    Defs.Values['FB_QUERY_SYMBOL.DATATYPE']   := '1';
    Defs.Values['FB_QUERY_SYMBOL.DATACLASS']  := '2';
    Defs.Values['FB_QUERY_SYMBOL.TYPENAME']   := '3';
    Defs.Values['FB_QUERY_SYMBOL.TYPENAMEID'] := '4';
    Defs.Values['FB_QUERY_SYMBOL.EXISTS']     := '6';
  end;
  if Base = 'dir.bi' then
  begin
    Defs.Values['FBREADONLY']  := '&h01';
    Defs.Values['FBHIDDEN']    := '&h02';
    Defs.Values['FBSYSTEM']    := '&h04';
    Defs.Values['FBDIRECTORY'] := '&h10';
    Defs.Values['FBARCHIVE']   := '&h20';
    Defs.Values['FBNORMAL']    := '(&h01 or &h20)';
  end;
end;

procedure RegisterIntrinsicDefines(Defs, FnDefs: TStringList);
// Pre-populate the macro table with FreeBASIC compiler intrinsic defines, so FB programs that use
// conditional compilation (#if __FB_WIN32__ / #ifdef __FB_64BIT__ / #if __FB_VER_MAJOR__ >= 1) take
// the right branch instead of failing. SedaiBasic claims FreeBASIC 1.10.x compatibility. Values use
// the FB boolean convention (-1 = TRUE) where the macro is a flag. Platform/arch macros are defined
// ONLY for the host the VM was built for, matching FreeBASIC (e.g. __FB_LINUX__ exists only on Linux),
// so `#ifdef` of a foreign platform is correctly false. String-valued macros keep their quotes so they
// substitute as string literals in ordinary code.
begin
  // --- Version (claim FreeBASIC 1.10.x) ---
  Defs.Values['__FB_VERSION__']   := '"1.10.1"';
  Defs.Values['__FB_VER_MAJOR__'] := '1';
  Defs.Values['__FB_VER_MINOR__'] := '10';
  Defs.Values['__FB_VER_PATCH__'] := '1';
  // __FB_MIN_VERSION__(major, minor, patch): true when the compiler is at least that version. fbc
  // defines it in its own prelude as this very expression, so it is written out the same way rather
  // than folded to a constant - a program may pass any triple. Measured against fbc 1.10.1:
  // (0,18,2) -> -1, (2,0,0) -> 0.
  FnDefs.Values['__FB_MIN_VERSION__'] :=
    'major,minor,patchlevel'#1 +
    '(__FB_VER_MAJOR__ > (major) or (__FB_VER_MAJOR__ = (major) and ' +
    '(__FB_VER_MINOR__ > (minor) or (__FB_VER_MINOR__ = (minor) and ' +
    '__FB_VER_PATCH__ >= (patchlevel)))))';
  Defs.Values['__FB_SIGNATURE__'] := '"SedaiBasic (FreeBASIC-compatible)"';
  // --- Language / compile mode, mapped to SedaiBasic's actual state ---
  Defs.Values['__FB_LANG__']    := '"fb"';
  // OPTION EXPLICIT is IMPLIED by -lang fb: every variable must be declared, and fbc answers -1 here
  // (measured). MODERN is that dialect, so the answer is the same - a module guarding itself with
  // "#if __FB_OPTION_EXPLICIT__ = 0 : #error ..." must compile, not refuse.
  Defs.Values['__FB_OPTION_EXPLICIT__'] := '-1';
  Defs.Values['__FB_MT__']      := '-1';   // multithreading runtime is available
  Defs.Values['__FB_OUT_EXE__'] := '-1';   // programs are run (executable-like target)
  // fbc defines this while compiling the module that holds the program's entry point. There is exactly
  // one module here - sb compiles and runs a single source - so it is always the main one.
  Defs.Values['__FB_MAIN__']    := '-1';
  // The optimisation level the SOURCE asked for. fbc's default is 0 (no -O on its command line), and
  // a #cmdline carrying -O raises it; a program reads it back to compile differently. It reports the
  // REQUEST, not our pipeline, which has no -O ladder to report.
  Defs.Values['__FB_OPTIMIZE__'] := '0';
  // --- WHICH COMPILER THE PROGRAM THINKS IT IS TALKING TO. ⛔ These six were MISSING, and a missing
  // one is not neutral: a program guarded by "#if __FB_BACKEND__ = "gas"" was compiled by us on the
  // GAS side, where fbc itself takes the gcc side - so we then died inside a body the oracle never
  // builds (functions/va_*, typedef/backpatch: four "defects" that were four wrong branches).
  // Values measured from fbc 1.10.1 on linux-x86_64 with its own defaults, which is the configuration
  // we claim to be; getting them WRONG would be worse than leaving them out, so each is the oracle's
  // answer and not a preference of ours.
  Defs.Values['__FB_BACKEND__']   := '"gcc"';      // -gen gcc is fbc's default here
  Defs.Values['__FB_GCC__']       := '-1';         // ...and its flag form
  Defs.Values['__FB_FPMODE__']    := '"precise"';  // -fpmode precise
  Defs.Values['__FB_ASM__']       := '"intel"';    // the dialect an Asm block would be written in
  Defs.Values['__FB_ERR__']       := '0';          // -e/-exx error-checking level: none
  Defs.Values['__FB_VECTORIZE__'] := '0';          // -vec 0
  // ⛔ ...AND ELEVEN MORE THAT WERE FILED "N/A - no meaning for a bytecode VM" AND HAVE AN EXACT ONE.
  // Every one of these is a compile-mode flag with a definite value in the configuration we claim to be,
  // and the paragraph above already says why a missing one is not neutral: an "#if __FB_OPTION_BYVAL__"
  // took the branch fbc does not take, in SILENCE. Being on the N/A shelf is what kept them from being
  // asked - see the note at the head of BASIC.md. Each value is fbc 1.10.1's own answer on
  // linux-x86_64 with its defaults, read out of the compiler and not chosen here.
  Defs.Values['__FB_OPTION_BYVAL__']   := '0';   // -lang fb passes by value unless BYREF is written
  Defs.Values['__FB_OPTION_DYNAMIC__'] := '0';   // arrays are static unless declared otherwise
  Defs.Values['__FB_OPTION_ESCAPE__']  := '0';   // "\n" is not an escape unless the literal says !"..."
  Defs.Values['__FB_OPTION_GOSUB__']   := '0';   // GOSUB is off in -lang fb
  Defs.Values['__FB_OPTION_PRIVATE__'] := '0';   // module procedures are public by default
  Defs.Values['__FB_OUT_DLL__']        := '0';   // the three targets we are NOT: only __FB_OUT_EXE__ is -1
  Defs.Values['__FB_OUT_LIB__']        := '0';
  Defs.Values['__FB_OUT_OBJ__']        := '0';
  Defs.Values['__FB_GUI__']            := '0';   // console programs, as fbc's default is
  // fbc defines this to 0 in a normal build (it is not gated on -g, which only RAISES it), so
  // "#ifdef __FB_DEBUG__" is TRUE there and was false here. Found by na_audit.sh --all, which is what
  // that tool is for. Measured, not assumed.
  Defs.Values['__FB_DEBUG__']          := '0';
  Defs.Values['__FB_FPU__']            := '"x87"';  // fbc 1.10.1 linux-x86_64 answers this; same
                                                    // policy as __FB_BACKEND__ - the oracle's word,
                                                    // not a preference of ours
  // ⛔ NOT __FB_PROFILE__ / __FB_OPTION_PROFILE__: fbc leaves those UNDEFINED unless -profile is given,
  // and "#ifdef __FB_PROFILE__" is how a program asks. Defining them to 0 answers the wrong question -
  // a define that EXISTS where fbc's does not is exactly as wrong as one that is missing, and this pair
  // was caught by checking each value against the oracle rather than by reasoning about it.
  // ⚠️ NOT the build-identity strings (__FB_BUILD_DATE__, __FB_BUILD_DATE_ISO__, __FB_BUILD_SHA1__).
  // Those describe the compiler's OWN build, and mirroring fbc's would be a statement about us that is
  // false. A program that reads them wants to know which binary it is talking to; the honest answer is
  // to leave them undefined rather than to answer somebody else's.
  { Which machine the program is being compiled FOR. SedaiBasic's own, with no
    FreeBASIC counterpart - fbc has no WebAssembly target - so it does not
    pretend to be an __FB_ macro. }
  if GTargetIsWasm then
    Defs.Values['__SB_WASM__'] := '-1'
  else
    Defs.Values['__SB_WASM__'] := '0';
  // __FB_ARGC__ / __FB_ARGV__ are the parameters of fbc's implicit main, so their VALUE is only known
  // when the program runs - a preprocessor constant cannot carry it. They expand instead to the
  // expression that fetches it, through two index selectors of COMMAND$ that no user spelling can
  // reach (see TBytecodeVM.CommandLine). ARGV yields the raw address of a vector of pointers, which is
  // what "ZString Ptr Ptr" holds, so "*argv[i]" reads the i-th argument exactly as the manual shows.
  Defs.Values['__FB_ARGC__']    := 'CInt(COMMAND$(-2))';
  Defs.Values['__FB_ARGV__']    := 'CPtr(ZString Ptr Ptr, __FB_ARGVPTR__)';
  {$IFDEF DEBUG}
  Defs.Values['__FB_DEBUG__']   := '-1';
  {$ENDIF}
  // --- Platform (host-only, like FreeBASIC) ---
  {$IFDEF WINDOWS}
  Defs.Values['__FB_WIN32__'] := '-1';
  Defs.Values['__FB_PCOS__']  := '-1';
  {$ENDIF}
  {$IFDEF LINUX}
  Defs.Values['__FB_LINUX__'] := '-1';
  Defs.Values['__FB_UNIX__']  := '-1';
  {$ENDIF}
  {$IFDEF DARWIN}
  Defs.Values['__FB_DARWIN__'] := '-1';
  Defs.Values['__FB_UNIX__']   := '-1';
  {$ENDIF}
  {$IFDEF FREEBSD}
  Defs.Values['__FB_FREEBSD__'] := '-1';
  Defs.Values['__FB_UNIX__']    := '-1';
  {$ENDIF}
  {$IFDEF NETBSD}
  Defs.Values['__FB_NETBSD__'] := '-1';
  Defs.Values['__FB_UNIX__']   := '-1';
  {$ENDIF}
  {$IFDEF OPENBSD}
  Defs.Values['__FB_OPENBSD__'] := '-1';
  Defs.Values['__FB_UNIX__']    := '-1';
  {$ENDIF}
  // --- Architecture (host-only) ---
  {$IFDEF CPU64}
  Defs.Values['__FB_64BIT__'] := '-1';
  {$ENDIF}
  {$IF DEFINED(CPUX86_64) OR DEFINED(CPUI386)}
  Defs.Values['__FB_X86__'] := '-1';
  {$ENDIF}
  {$IF DEFINED(CPUAARCH64) OR DEFINED(CPUARM)}
  Defs.Values['__FB_ARM__'] := '-1';
  {$ENDIF}
  {$IFDEF CPUPOWERPC}
  Defs.Values['__FB_PPC__'] := '-1';
  {$ENDIF}
  {$IFDEF ENDIAN_BIG}
  Defs.Values['__FB_BIGENDIAN__'] := '-1';
  {$ENDIF}
end;

function PPFirstBreak(const S: string): Integer;
// How many characters of S run up to the first blank: the "<n>" of "#line <n> \"file\"".
begin
  Result := 0;
  while (Result < Length(S)) and not (S[Result + 1] in [' ', #9]) do Inc(Result);
end;

procedure PPMapLine(Physical: Integer; out Reported: Integer; var Module: string);
// The LAST #line at or before this physical line wins, and the count runs on from there: a directive
// standing on physical line P and saying N makes P report as N, P+1 as N+1, and so on.
var
  i, k: Integer;
begin
  Reported := Physical;
  k := -1;
  for i := 0 to High(GPPLineDirectives) do
    if GPPLineDirectives[i].FromPhysical <= Physical then k := i else Break;
  if k < 0 then Exit;
  Reported := GPPLineDirectives[k].ReportedLine + (Physical - GPPLineDirectives[k].FromPhysical);
  if GPPLineDirectives[k].ModuleName <> '' then Module := GPPLineDirectives[k].ModuleName;
end;

function PreprocessSource(const Src, BaseDir: string; const FileName: string = ''): string;
var
  Defs: TStringList;     // Names = UPPER object-like macro names, Values = macro bodies
  FnDefs: TStringList;   // Names = UPPER function-like macro names, Values = "params"#1"body"
  Output: TStringList;
  // Conditional stack: Active[k] = currently emitting at nesting level k (already factors parents);
  // Taken[k] = a branch has been taken at this level (for #else).
  Active, Taken: array of Boolean;
  NowDT: TDateTime;      // captured once for __DATE__/__DATE_ISO__/__TIME__
  PathStr: string;       // module directory for __PATH__
  FileStr: string;       // top-level source path, in the platform's own spelling
  EscapeOn: Boolean;     // OPTION ESCAPE seen: plain "..." strings become escaped from here on
  IncOnce: TStringList;  // full paths ALREADY SPLICED, by any form of #include. ⛔ A PLAIN "#include"
                         // registers the file too: fbc's own pp/inc_once1 includes a header twice
                         // plainly and then asks for it "once", and the once is SKIPPED - "once" means
                         // "if this file has not been included yet", not "if no earlier ONCE took it".
  PragmaOnce: TStringList;  // full paths of files that asked for it themselves, with "#pragma once".
                         // Those are skipped by EVERY later include, plain or once.
  ExpandedLine: string;  // a source line after macro substitution
  FReprocessDepth: Integer;   // guard against a macro whose expansion expands to itself
  UidK: Integer;         // scratch: clearing the __FB_UNIQUEID_* stacks at entry

  function Emitting: Boolean;
  begin
    Result := (Length(Active) = 0) or Active[High(Active)];
  end;

  // Forward: ReprocessExpansion feeds a macro expansion back through Expand, which is declared below.
  procedure Expand(const Text, Dir: string; const SrcPath: string = ''); forward;

  function ExpandedLineHasDirective(const S: string): Boolean;
  // Does this expanded line hold a preprocessor directive in one of its cVirtualEOL segments? Only a
  // MACRO expansion can produce one (a directive written directly in the source was handled above), so
  // the test is cheap and fires almost never.
  var
    i: Integer;
    Seg: string;
  begin
    Result := False;
    if Pos('#', S) = 0 then Exit;
    Seg := '';
    for i := 1 to Length(S) do
      if S[i] = cVirtualEOL then
      begin
        if (Trim(Seg) <> '') and (TrimLeft(Seg)[1] = '#') then Exit(True);
        Seg := '';
      end
      else
        Seg := Seg + S[i];
    Result := (Trim(Seg) <> '') and (TrimLeft(Seg)[1] = '#');
  end;

  function ReprocessExpansion(const S, ADir: string): string;
  // Run a macro expansion that contains directives back through Expand, then rejoin whatever CODE
  // survived into the single physical line the invocation occupies. Directives leave blanks behind
  // (that is how they preserve line numbers), so the join drops them.
  var
    Txt: string;
    i, Base: Integer;
  begin
    if FReprocessDepth > 32 then Exit(S);   // a macro that expands to itself: stop rather than spin
    Inc(FReprocessDepth);
    try
      Txt := StringReplace(S, cVirtualEOL, sLineBreak, [rfReplaceAll]);
      Base := Output.Count;
      Expand(Txt, ADir);
      Result := '';
      for i := Base to Output.Count - 1 do
        if Trim(Output[i]) <> '' then
        begin
          if Result <> '' then Result := Result + cVirtualEOL;
          Result := Result + Output[i];
        end;
      while Output.Count > Base do Output.Delete(Output.Count - 1);
    finally
      Dec(FReprocessDepth);
    end;
  end;

  // OPTION ESCAPE (fblite/qb): from this statement on, ESCAPE SEQUENCES ARE PROCESSED in plain
  // double-quoted strings ("\\" prints one backslash - fbc-verified). Escaping lives in the
  // LEXER's !"..." handling and tokens are cut before the parser could flip any mode, so the
  // preprocessor - which runs first and is line-based - rewrites every plain opening quote to
  // the !"..." form instead. $"..." (raw) and already-!"..." strings are left alone, and the
  // scan stops at a ' comment. Inside a rewritten string, \x escapes and doubled "" are
  // skipped so the closing quote is found exactly where the lexer will find it.
  function ApplyEscapeRewrite(const S: string): string;
  var
    i: Integer;
    InStr: Boolean;
  begin
    Result := '';
    InStr := False;
    i := 1;
    while i <= Length(S) do
    begin
      if not InStr then
      begin
        if S[i] = '''' then begin Result := Result + Copy(S, i, MaxInt); Exit; end;  // comment tail
        if S[i] = '"' then
        begin
          if (Length(Result) = 0) or
             ((Result[Length(Result)] <> '!') and (Result[Length(Result)] <> '$')) then
            Result := Result + '!';
          InStr := True;
        end;
        Result := Result + S[i];
        Inc(i);
      end
      else
      begin
        if (S[i] = '\') and (i < Length(S)) then
        begin
          Result := Result + S[i] + S[i + 1]; Inc(i, 2); Continue;
        end;
        if (S[i] = '"') and (i < Length(S)) and (S[i + 1] = '"') then
        begin
          Result := Result + '""'; Inc(i, 2); Continue;
        end;
        if S[i] = '"' then InStr := False;
        Result := Result + S[i];
        Inc(i);
      end;
    end;
  end;

  // True if the line's statement text is OPTION ESCAPE (leading whitespace tolerated).
  function IsOptionEscapeLine(const Trimmed: string): Boolean;
  var
    U: string;
  begin
    U := UpperCase(Trimmed);
    Result := (Copy(U, 1, 6) = 'OPTION') and (Pos('ESCAPE', U) > 0) and (Pos('"', U) = 0);
  end;

  procedure Expand(const Text, Dir: string; const SrcPath: string = '');
  // SrcPath = the full path of the file THIS text came from ('' for the top-level source and for a
  // macro expansion fed back through here). It exists for one reason: "#pragma once" is a statement a
  // file makes ABOUT ITSELF, so the directive has to know which file it is standing in.
  var
    Lines: TStringList;
    li, p, q: Integer;
    Canon: string;   // the include path, canonicalised - the identity every "once" question asks about
    Raw, Trimmed, DName, DRest, MacroName, MacroVal, FileName, FullPath: string;
    Params, MacroBody, BodyTrim, EName, ERest, LineFile: string;
    LineNum: Integer;
    IsFn: Boolean;
    OptParen: Boolean;   // "#macro name ? (params)": the parentheses are optional at the call site
    ParentEmit, Cond: Boolean;
    MappedLine: Integer;
    MappedModule: string;
    IncText: TStringList;
    IncludeOnce: Boolean;   // "#include Once": splice this path at most one time
    BlockCmt, PrevBlockCmt: Integer;   // depth of the /' ... '/ block comment we are inside
    ScopeDepth: Integer;    // block nesting, for the level a #pragma reserve was made at
    DirWord: string;        // the leading word of the line, for that same counter
    SavedStackTop: Integer;
    ContJoin, CutPos: Integer;   // '_'-continued physical lines folded into this logical one
    IsModuleText: Boolean;       // this pass is over the MODULE's own text: it carries the position
  // The block-scope openers "#pragma reserve" counts levels with. Nothing else reads them.
  function BlockCloser(const S: string): Boolean;
  var W: string;
  begin
    W := Trim(S);
    if Pos(' ', W) > 0 then W := Copy(W, 1, Pos(' ', W) - 1);
    Result := (W = 'SCOPE') or (W = 'SUB') or (W = 'FUNCTION') or (W = 'PROPERTY') or
              (W = 'CONSTRUCTOR') or (W = 'DESTRUCTOR') or (W = 'OPERATOR') or (W = 'NAMESPACE');
  end;

  function BlockOpener(const S: string): Boolean;
  var W: string;
  begin
    W := Trim(S);
    if Pos(' ', W) > 0 then W := Copy(W, 1, Pos(' ', W) - 1);
    if Pos('(', W) > 0 then W := Copy(W, 1, Pos('(', W) - 1);
    Result := BlockCloser(W);
  end;

  // ⛔ A DIRECTIVE INSIDE A /' ... '/ BLOCK COMMENT IS NOT A DIRECTIVE. This preprocessor reads the
  // file LINE BY LINE and knew nothing about block comments, so an "#error" written inside one FIRED
  // and refused the program - fbc's own comments/multiline is built on exactly that, four times over.
  // Block comments NEST, and a line comment neutralises an opener that follows it on the same line
  // ("' /'" opens nothing - that file tests it too).
  procedure ScanBlockComment(const S: string; var Depth: Integer);
  var
    k: Integer;
    InStr: Boolean;
  begin
    InStr := False;
    k := 1;
    while k <= Length(S) do
    begin
      if Depth = 0 then
      begin
        if S[k] = '"' then InStr := not InStr
        else if not InStr then
        begin
          if (k < Length(S)) and (S[k] = '/') and (S[k + 1] = '''') then
          begin Inc(Depth); Inc(k, 2); Continue; end;
          if S[k] = '''' then Exit;                       // a line comment: nothing after it opens one
        end;
      end
      else
      begin
        if (k < Length(S)) and (S[k] = '/') and (S[k + 1] = '''') then
        begin Inc(Depth); Inc(k, 2); Continue; end;
        if (k < Length(S)) and (S[k] = '''') and (S[k + 1] = '/') then
        begin Dec(Depth); Inc(k, 2); Continue; end;
      end;
      Inc(k);
    end;
  end;

  begin
    SavedStackTop := High(Active);   // remember depth so includes can't leak unbalanced conditionals
    BlockCmt := 0;
    ScopeDepth := 0;
    Lines := TStringList.Create;
    try
      Lines.Text := Text;
      // Only the MODULE's own text carries a POSITION for defined(): an #include and a macro
      // re-expansion are re-entered here with their own text, whose line numbers say nothing about
      // where the question stands, so they leave the outer position alone. See GPPDefinedLimit.
      IsModuleText := (Text = GPPSourceForDefined);
      li := 0;
      while li < Lines.Count do
      begin
        if IsModuleText then GPPDefinedLimit := li;
        Raw := Lines[li];
        Trimmed := TrimLeft(Raw);
        // The depth is updated for THIS line first, so a line that OPENS a comment is still read as
        // code up to the "/'", and every line while we are inside is passed through untouched - the
        // LEXER knows block comments and will drop them; what must not happen is a DIRECTIVE firing.
        PrevBlockCmt := BlockCmt;
        ScanBlockComment(Raw, BlockCmt);
        // SCOPE nesting, for #pragma reserve: only that directive reads it, and only to tell a repeat
        // at the SAME level from one made INSIDE a nested scope - which fbc accepts.
        // BLOCK-SCOPE nesting, for "#pragma reserve" alone: it is the only directive that reads it,
        // and only to tell a repeat at the SAME level (which fbc refuses) from one made INSIDE a
        // nested block (which it accepts - its own pragma-reserve-3 re-reserves the same name in a
        // SCOPE and again inside a SUB). ⛔ A procedure body is a nested block too: counting only
        // SCOPE left the reserve inside "sub proc()" at level 0 and refused a legal program.
        // A one-line "Sub s() : ... : End Sub" opens and closes on the same line, so the closing form
        // is looked for on the line before deciding.
        DirWord := UpperCase(Copy(Trimmed, 1, 20));
        if (Copy(DirWord, 1, 4) = 'END ') then
        begin
          if BlockCloser(Copy(DirWord, 5, MaxInt)) and (ScopeDepth > 0) then Dec(ScopeDepth);
        end
        else if BlockOpener(DirWord) and (Pos(' : END ', ' ' + UpperCase(Trimmed) + ' ') = 0) then
          Inc(ScopeDepth);
        if PrevBlockCmt > 0 then
        begin
          if Emitting then Output.Add(Raw) else Output.Add('');
          Inc(li);
          Continue;
        end;
        // __LINE__ expands to the current source line number (1-based). Updated every line so it is
        // correct wherever it appears; __FILE__ is set once (top-level file) in the begin block below.
        // __LINE__ is the same question a diagnostic asks, so it follows #line too.
        PPMapLine(li + 1, MappedLine, MappedModule);
        Defs.Values['__LINE__'] := IntToStr(MappedLine);
        // QuickBASIC-style metacommand '$INCLUDE: 'file' (a leading apostrophe makes it a comment to
        // the lexer; intercept it here and splice the file, like #include).
        if (Length(Trimmed) >= 9) and (UpperCase(Copy(Trimmed, 1, 9)) = '''$INCLUDE') and Emitting then
        begin
          q := Pos('''', Copy(Trimmed, 2, MaxInt));   // first quote after the leading apostrophe
          if q > 0 then
          begin
            FileName := Copy(Trimmed, q + 2, MaxInt);   // text after that quote
            p := Pos('''', FileName);
            if p > 0 then FileName := Copy(FileName, 1, p - 1);
            FullPath := FileName;
            if not FileExists(FullPath) then FullPath := IncludeTrailingPathDelimiter(Dir) + FileName;
            if FileExists(FullPath) then
            begin
              IncText := TStringList.Create;
              try
                IncText.LoadFromFile(FullPath);
                Expand(IncText.Text, ExtractFilePath(ExpandFileName(FullPath)));
              finally
                IncText.Free;
              end;
            end
            else
              RegisterEmulatedHeader(FileName, Defs, FnDefs);
          end;
          Output.Add('');   // the metacommand line itself produces no output
          Inc(li);
          Continue;
        end;
        // FreeBASIC/QuickBASIC advisory metacommands '$DYNAMIC / '$STATIC / '$LANG: accepted and
        // ignored. '$DYNAMIC/'$STATIC pick the default array storage (we allow REDIM regardless);
        // '$LANG mirrors the #lang directive (dialect is auto-detected). They emit nothing.
        if (Length(Trimmed) >= 2) and (Trimmed[1] = '''') and (Trimmed[2] = '$') and Emitting and
           ((UpperCase(Copy(Trimmed, 3, 7)) = 'DYNAMIC') or
            (UpperCase(Copy(Trimmed, 3, 6)) = 'STATIC') or
            (UpperCase(Copy(Trimmed, 3, 4)) = 'LANG')) then
        begin
          Output.Add('');
          Inc(li);
          Continue;
        end;
        // A DIRECTIVE CONTINUED ON THE NEXT LINE. FreeBASIC's '_' continuation works inside a
        // preprocessor directive too, and the manual's own #define is written that way:
        //     #define printval(bar) _
        //         Print #bar; " ="; bar
        // Without the join the directive defined an EMPTY macro and its body was left standing in the
        // source as ordinary code - so prepro/define went on to execute "Print #bar; ..." and died on
        // "PRINT# error 64 writing to file: 0", a complaint about a file handle for a line nobody
        // wrote. Every line swallowed here leaves a blank behind, exactly as #macro does, so the line
        // numbers the rest of the pipeline reports stay the source's own.
        // ⛔ ...BUT A #macro WHOSE PARAMETER LIST IS ALREADY CLOSED DOES NOT CONTINUE. The '_' there
        // continues the HEADER while the header is still being written - that is how a long parameter
        // list is split - and fbc stops honouring it once the ')' has been seen: "#macro M(x) _"
        // followed by two body lines runs BOTH of them. Joined generically, the first body line was
        // swallowed into the header and lost, so the macro ran one statement short and said nothing.
        // ⛔ ...AND A "#macro" LINE CONTINUES LIKE ANY OTHER, WHATEVER ITS PARAMETER LIST IS DOING.
        // This used to stop the join once the list was closed - or once it was clear there was none -
        // because joining swallowed the first BODY line. That was the wrong half to fix: fbc's join is
        // at TOKEN level, so what follows the parameter list on the joined line simply IS the first
        // body line, and the #macro handler below now takes it as one. With the join stopped instead,
        // a header whose parameter list is written on the NEXT line was never seen at all -
        // "#macro gen _" / "( _" / "a, _" / ... - and fbc's own suite writes nine files that way
        // (structs/udt-ops-*, udt-comp-ops-*, udt-*string/conversion): the macro came out OBJECT-like,
        // its call "gen( 1, 2 )" was read as an expression, and the whole FILE died on
        // 'Expected ")" after expression'. All three shapes now go through one rule.
        while (Length(Trimmed) > 0) and (Trimmed[1] = '#') and PPDirectiveContinues(Trimmed) and
              (li + 1 < Lines.Count) do
        begin
          Trimmed := TrimRight(StripDirectiveComment(Trimmed));
          Trimmed := Copy(Trimmed, 1, Length(Trimmed) - 1) + ' ' + Trim(Lines[li + 1]);
          Inc(li);
          Output.Add('');
        end;
        if (Length(Trimmed) > 0) and (Trimmed[1] = '#') then
        begin
          SplitDirective(Trimmed, DName, DRest);
          if DName = 'ifdef' then
          begin
            ParentEmit := Emitting;
            // ...and a FUNCTION-LIKE macro is defined too (FnDefs); see the note on defined() above.
            // ⛔ ...AND THE RESERVED SET, which "#if defined(X)" already consults. "#ifdef X" and
            // "defined(X)" are ONE question in fbc, and its pp/defined-udt asks both of the same names
            // expecting the same answer; here they were two, and __FUNCTION__ - defined but never
            // substitutable - answered yes to one and no to the other.
            // ⚠️ Only the reserved set is added. SourceDeclaresSymbol (a Const, a Dim, a Sub) is the
            // OTHER half of that same asymmetry and is NOT closed here: every #ifdef of such a name
            // would flip at once, which is a measurement of its own. Written up in DIVERGENZE.
            // ⭐ ONE QUESTION, ONE PREDICATE. "#ifdef X" and "#if defined( X )" are the same
            // question in fbc, and its pp/defined-udt asks BOTH of every name it checks, expecting
            // the same answer. Here they were two: only #if consulted the source's own declarations,
            // so a Const, a Dim, a Sub - and a member of the enclosing type - answered yes to one and
            // no to the other. PPNameIsDefined is now the single place either of them asks.
            Cond := ParentEmit and PPNameIsDefined(UpperCase(Trim(DRest)), Defs, FnDefs);
            SetLength(Active, Length(Active) + 1); Active[High(Active)] := Cond;
            SetLength(Taken, Length(Taken) + 1);   Taken[High(Taken)] := Cond;
          end
          else if DName = 'ifndef' then
          begin
            ParentEmit := Emitting;
            Cond := ParentEmit and not PPNameIsDefined(UpperCase(Trim(DRest)), Defs, FnDefs);
            SetLength(Active, Length(Active) + 1); Active[High(Active)] := Cond;
            SetLength(Taken, Length(Taken) + 1);   Taken[High(Taken)] := Cond;
          end
          else if DName = 'if' then
          begin
            ParentEmit := Emitting;
            Cond := ParentEmit and EvalPPExpr(DRest, Defs, FnDefs);
            SetLength(Active, Length(Active) + 1); Active[High(Active)] := Cond;
            SetLength(Taken, Length(Taken) + 1);   Taken[High(Taken)] := Cond;
          end
          else if (DName = 'elif') or (DName = 'elseif') or
                  (DName = 'elseifdef') or (DName = 'elseifndef') then
          begin
            // #elif <expr> / #elseif <expr> / #elseifdef NAME / #elseifndef NAME — an else-if branch.
            if Length(Active) > 0 then
            begin
              ParentEmit := (Length(Active) = 1) or Active[High(Active) - 1];
              if Taken[High(Taken)] then
                Active[High(Active)] := False                  // an earlier branch already won
              else
              begin
                if DName = 'elseifdef' then
                  Cond := ParentEmit and (Defs.IndexOfName(UpperCase(Trim(DRest))) >= 0)
                else if DName = 'elseifndef' then
                  Cond := ParentEmit and (Defs.IndexOfName(UpperCase(Trim(DRest))) < 0)
                else
                  Cond := ParentEmit and EvalPPExpr(DRest, Defs, FnDefs);
                Active[High(Active)] := Cond;
                if Cond then Taken[High(Taken)] := True;
              end;
            end;
          end
          else if DName = 'else' then
          begin
            if Length(Active) > 0 then
            begin
              // Parent emit = the level below; re-derive from this level's stored info.
              ParentEmit := (Length(Active) = 1) or Active[High(Active) - 1];
              Active[High(Active)] := ParentEmit and (not Taken[High(Taken)]);
              if Active[High(Active)] then Taken[High(Taken)] := True;
            end;
          end
          else if DName = 'endif' then
          begin
            if Length(Active) > 0 then
            begin
              SetLength(Active, Length(Active) - 1);
              SetLength(Taken, Length(Taken) - 1);
            end;
          end
          else if (DName = 'line') and Emitting then
          begin
            // "#line <n> ["file"]": from here on, report positions as if this line were <n> of
            // <file>. It changes NOTHING the program computes - only what a diagnostic says - which
            // is why ignoring it was silent: prepro/line aborted with our own line and our own file
            // name and looked like an ordinary answer.
            // ⚠️ Recorded here and consulted where a position is REPORTED. __LINE__ follows it too,
            // being the same question asked from inside the program.
            LineNum := StrToIntDef(Trim(Copy(Trim(DRest), 1, PPFirstBreak(Trim(DRest)))), -1);
            LineFile := '';
            p := Pos('"', DRest);
            if p > 0 then
            begin
              LineFile := Copy(DRest, p + 1, MaxInt);
              q := Pos('"', LineFile);
              if q > 0 then LineFile := Copy(LineFile, 1, q - 1) else LineFile := '';
            end;
            if LineNum >= 0 then
            begin
              SetLength(GPPLineDirectives, Length(GPPLineDirectives) + 1);
              GPPLineDirectives[High(GPPLineDirectives)].FromPhysical := li + 1;
              GPPLineDirectives[High(GPPLineDirectives)].ReportedLine := LineNum;
              GPPLineDirectives[High(GPPLineDirectives)].ModuleName := LineFile;
            end;
          end
          else if (DName = 'define') and Emitting then
          begin
            p := 1;
            while (p <= Length(DRest)) and IsIdentChar(DRest[p]) do Inc(p);
            MacroName := UpperCase(Copy(DRest, 1, p - 1));
            if (p <= Length(DRest)) and (DRest[p] = '(') then
            begin
              // Function-like macro "NAME(params) body": store as "params"#1"body" in FnDefs.
              q := p + 1;
              while (q <= Length(DRest)) and (DRest[q] <> ')') do Inc(q);
              MacroVal := Trim(Copy(DRest, p + 1, q - p - 1)) + #1 + Trim(StripDirectiveComment(Copy(DRest, q + 1, MaxInt)));
              if MacroName <> '' then FnDefs.Values[MacroName] := MacroVal;
            end
            else
            begin
              MacroVal := Trim(StripDirectiveComment(Copy(DRest, p, MaxInt)));
              // __FB_EVAL__ means "evaluate HERE", so a #define carrying one stores the RESULT, not the
              // text. That is the whole reason the manual's print_last macro works: the index is
              // "__FB_EVAL__( __FB_ARG_COUNT__( args ) - 1 )", and args exists only while the macro is
              // being expanded - stored raw, it was re-expanded later where args means nothing.
              // Narrow on purpose: an ordinary #define still stores its text, as #define must.
              if Pos('__FB_EVAL__', UpperCase(MacroVal)) > 0 then
                MacroVal := Trim(SubstituteMacros(MacroVal, Defs, FnDefs, 0));
              if MacroName <> '' then Defs.Values[MacroName] := MacroVal;
            end;
          end
          else if (DName = 'macro') and Emitting then
          begin
            // Multi-line macro: "#macro NAME[(params)]" ... body lines ... "#endmacro".
            // The body lines are joined with cVirtualEOL so one invocation expands to the whole
            // sequence AS SEPARATE LINES (what FreeBASIC does) while still occupying the single
            // physical line it was invoked from. Joining with ':' instead would be wrong: BASIC puts
            // every ':'-separated statement after "IF c THEN" into the THEN branch, so a body holding
            // an inline "IF ... THEN Return" would swallow the rest of the macro whenever the
            // condition was false. With params it becomes a function-like macro (FnDefs), otherwise
            // an object-like one (Defs). Body lines are consumed here and replaced by blanks to
            // preserve source line numbers.
            p := 1;
            while (p <= Length(DRest)) and IsIdentChar(DRest[p]) do Inc(p);
            MacroName := UpperCase(Copy(DRest, 1, p - 1));
            // "#macro m ( arg1, arg2 )": FreeBASIC allows space before the parameter list, and the
            // manual writes it that way. Testing the very next character made such a macro OBJECT-like,
            // so an invocation expanded to the raw body and its arguments leaked out as code.
            while (p <= Length(DRest)) and (DRest[p] in [' ', #9]) do Inc(p);
            // ⛔ ...AND A BLOCK COMMENT MAY SIT BETWEEN THE NAME AND THE PARAMETER LIST.
            // "#macro mac /' c '/ ( a, b )" is FreeBASIC, and its own pp tests are written that way.
            // Only SPACES were skipped, so the '(' was not where the reader looked and the macro came
            // out OBJECT-like: its body expanded with the parameters unsubstituted and the arguments
            // leaked out as code, which the SSA then met as "Array not declared: B" - a diagnostic
            // naming a macro PARAMETER, several stages away from the comment that caused it.
            while (p + 1 <= Length(DRest)) and (DRest[p] = '/') and (DRest[p + 1] = '''') do
            begin
              Inc(p, 2);                                  // past the opening "/'"
              while (p + 1 <= Length(DRest)) and
                    not ((DRest[p] = '''') and (DRest[p + 1] = '/')) do Inc(p);
              Inc(p, 2);                                  // past the closing "'/"
              while (p <= Length(DRest)) and (DRest[p] in [' ', #9]) do Inc(p);
            end;
            // "#macro name ? ( params )": the '?' makes the PARENTHESES OPTIONAL at the call site, so
            // "repeat 3" invokes it with 3 and the arguments run to the end of the line. Without this
            // the '?' was not a '(' and the macro came out OBJECT-like: its body expanded with the
            // parameters unsubstituted and its arguments leaked out as code, which the SSA then met as
            // a statement it had no node for.
            OptParen := (p <= Length(DRest)) and (DRest[p] = '?');
            if OptParen then
            begin
              Inc(p);
              while (p <= Length(DRest)) and (DRest[p] in [' ', #9]) do Inc(p);
            end;
            IsFn := (p <= Length(DRest)) and (DRest[p] = '(');
            Params := '';
            if IsFn then
            begin
              q := p + 1;
              while (q <= Length(DRest)) and (DRest[q] <> ')') do Inc(q);
              Params := Trim(Copy(DRest, p + 1, q - p - 1));
            end;
            // ⭐ WHAT IS LEFT ON THE HEADER LINE IS THE FIRST BODY LINE. fbc joins '_'-continued lines
            // at token level, so "#macro m( x ) _" followed by a statement puts that statement on the
            // macro's own line - and it belongs to the BODY, not to the header. Same for a
            // parameterless "#macro m _" followed by one. Nothing is left over for a header written on
            // one line, which is every ordinary macro.
            if IsFn then MacroBody := Trim(StripDirectiveComment(Copy(DRest, q + 1, MaxInt)))
            else MacroBody := Trim(StripDirectiveComment(Copy(DRest, p, MaxInt)));
            Inc(li);
            while li < Lines.Count do
            begin
              BodyTrim := TrimLeft(Lines[li]);
              if (Length(BodyTrim) > 0) and (BodyTrim[1] = '#') then
              begin
                SplitDirective(BodyTrim, EName, ERest);
                if EName = 'endmacro' then begin Output.Add(''); Break; end;
              end;
              // ⛔ A COMMENT IN A #macro BODY IS STILL A COMMENT. The #define path strips one from the
              // value it stores (StripDirectiveComment, twice, a hundred lines up); the #macro path
              // never did, so the comment text was carried into the body and re-emitted at every
              // expansion site - and a multi-line macro whose body is a single comment, expanded
              // inside a one-line "If ... Then", left that comment where a statement had to be:
              // "Parsing failed". ⭐ The discriminator is exact: a body that is genuinely EMPTY works,
              // and the same file with one comment in it does not.
              BodyTrim := Trim(StripDirectiveComment(Lines[li]));
              if BodyTrim <> '' then
              begin
                if MacroBody <> '' then MacroBody := MacroBody + cVirtualEOL;
                MacroBody := MacroBody + BodyTrim;
              end;
              Output.Add('');   // blank placeholder preserves line numbers
              Inc(li);
            end;
            if MacroName <> '' then
            begin
              // The optional-paren mark travels with the PARAMETER LIST, as a leading '?': it has to
              // reach the call site, and the parameter list is the only thing stored per macro. A
              // parameter can never begin with '?', so nothing else can be read as the mark.
              if IsFn and OptParen then Params := '?' + Params;
              if IsFn then FnDefs.Values[MacroName] := Params + #1 + MacroBody
              else Defs.Values[MacroName] := MacroBody;
            end;
          end
          else if (DName = 'undef') and Emitting then
          begin
            // ⛔ ...from BOTH tables. "#undef m" of a function-like macro left it in FnDefs, so the
            // name went on expanding after the program had explicitly retired it.
            p := Defs.IndexOfName(UpperCase(Trim(DRest)));
            if p >= 0 then Defs.Delete(p);
            p := FnDefs.IndexOfName(UpperCase(Trim(DRest)));
            if p >= 0 then FnDefs.Delete(p);
          end
          else if (DName = 'include') and Emitting then
          begin
            // The name is what stands between the QUOTES, not the whole rest of the line: a trailing
            // "'" comment is ordinary on an include ('#include "dir.bi" ' provides the constants' is
            // the manual's own wording) and used to become part of the path, so the file was never
            // found and the include silently did nothing.
            FileName := Trim(DRest);
            // "#include Once "file"": the modifier asks for the file to be spliced at most once. Every
            // one of these used to keep ONCE as part of the path, so the file was never found and the
            // include did nothing at all - in silence, which for a header of CONSTANTS means every one
            // of them reads as zero.
            IncludeOnce := (Length(FileName) >= 4) and (UpperCase(Copy(FileName, 1, 4)) = 'ONCE') and
                           ((Length(FileName) = 4) or (FileName[5] in [' ', #9, '"']));
            if IncludeOnce then FileName := Trim(Copy(FileName, 5, MaxInt));
            if (Length(FileName) >= 2) and (FileName[1] = '"') then
            begin
              p := Pos('"', Copy(FileName, 2, MaxInt));
              if p > 0 then FileName := Copy(FileName, 2, p - 1)
              else FileName := Copy(FileName, 2, Length(FileName) - 1);
            end
            else
              FileName := Trim(StripDirectiveComment(FileName));   // unquoted form
            FullPath := FileName;
            if not FileExists(FullPath) then FullPath := IncludeTrailingPathDelimiter(Dir) + FileName;
            if FileExists(FullPath) then
            begin
              // ⛔ THE IDENTITY IS THE CANONICAL PATH, NOT THE SPELLING. fbc's own pp/inc_once1 reaches
              // one header as "inc1.bi" and as "../pp/inc1.bi" and treats the two as the SAME file.
              Canon := UpperCase(ExpandFileName(FullPath));
              if (PragmaOnce.IndexOf(Canon) >= 0) or
                 (IncludeOnce and (IncOnce.IndexOf(Canon) >= 0)) then
                IncText := nil                                  // already spliced, or it asked to be once
              else
              begin
                // ⛔ EVERY splice registers the file, not only a "once" one: what "once" asks is "has
                // this file been included yet", and a plain #include is exactly that. Registering only
                // the ONCE form made "#include" twice followed by "#include once" splice a THIRD time.
                if IncOnce.IndexOf(Canon) < 0 then IncOnce.Add(Canon);
                IncText := TStringList.Create;
                try
                  IncText.LoadFromFile(FullPath);
                  Expand(IncText.Text, ExtractFilePath(ExpandFileName(FullPath)), FullPath);
                finally
                  IncText.Free;
                end;
              end;
            end
            else
              RegisterEmulatedHeader(FileName, Defs, FnDefs);
          end
          else if (DName = 'pragma') and Emitting then
          begin
            // #pragma once — the FILE says "include me at most once", whichever form asks for it.
            // ⛔ It was not handled at all, and being dropped in silence is what made it look handled:
            // the first include of such a header was right, and every later one spliced it again
            // (fbc's own pp/inc_once2 counts 1 where we counted 3).
            // Every other pragma (reserve, push/pop) stays ignored, exactly as before.
            MacroName := UpperCase(Trim(StripDirectiveComment(DRest)));
            if (MacroName = 'ONCE') and (SrcPath <> '') then
            begin
              Canon := UpperCase(ExpandFileName(SrcPath));
              if PragmaOnce.IndexOf(Canon) < 0 then PragmaOnce.Add(Canon);
            end
            // ⭐ "#pragma reserve NAME" makes NAME a SYMBOL: fbc reserves the identifier and defined()
            // answers TRUE for it from that line on - its own pp/pragma-reserve-* files refuse
            // themselves with #error otherwise. Reserving is ALL it does here: there is no symbol table
            // at this stage to keep the name out of, and the only observable half of the feature is
            // exactly the one defined() asks for.
            else if Copy(MacroName, 1, 7) = 'RESERVE' then
            begin
              // "#pragma reserve [(qual,...)] NAME".
              // ⛔⛔ A SECOND RESERVATION OF THE SAME NAME AT THE SAME LEVEL IS AN ERROR, WHATEVER THE
              // LIST SAYS. The first attempt here compared the attribute lists as SETS and allowed an
              // identical repeat - an invention: the ORACLE refuses "#pragma reserve N" twice at module
              // level with "error 4: Duplicated definition", and refuses "(extern,asm)" followed by
              // "(asm,extern)" just the same. It was found by blessing the guard, which is the only
              // reason the rule is not still shipped for the wrong reason.
              // ⭐ What test 3 of fbc's own family really shows is a NESTING rule: reserving the name
              // again inside a SCOPE is accepted. So the reservation carries the scope DEPTH it was
              // made at, and only a repeat at that depth or shallower is refused.
              MacroVal := Trim(Copy(MacroName, 8, MaxInt));
              DirWord := '';
              // ⛔ AND ONLY AN UNQUALIFIED RESERVATION MAKES THE NAME defined(). A qualified one
              // reserves it in ANOTHER namespace - the assembler's, the linker's - so the program can
              // still declare it (fbc's pragma-reserve-12/13) and defined() answers FALSE for it. Said
              // by the oracle while blessing this guard: the qualified pair printed nothing where we
              // printed "defined". The mark rides in the stored value, so the duplicate check below
              // still sees every reservation whatever its list.
              if (MacroVal <> '') and (MacroVal[1] = '(') then
              begin
                q := Pos(')', MacroVal);
                if q > 0 then
                begin
                  MacroVal := Trim(Copy(MacroVal, q + 1, MaxInt));
                  DirWord := 'q';
                end;
              end;
              if (MacroVal <> '') and (GPPReserved <> nil) then
              begin
                q := GPPReserved.IndexOfName(MacroVal);
                if q >= 0 then
                begin
                  if StrToIntDef(StringReplace(GPPReserved.ValueFromIndex[q], 'q', '',
                                 [rfReplaceAll]), 0) >= ScopeDepth then
                    raise EPreprocessorError.Create('Duplicated definition, ' + MacroVal +
                      ' is already reserved at this level');
                end
                else
                  GPPReserved.Values[MacroVal] := DirWord + IntToStr(ScopeDepth);
              end;
            end;
          end
          else if (DName = 'print') and Emitting then
            // #print msg — emit a compile-time diagnostic (macro-expanded) to stderr. What exactly gets
            // echoed is PPPrintMessage's business: the TOKENS with one space between them, a comment
            // ending the line. (This used to be done here, as "the rest of the line verbatim".)
            // ...but when what is left after expansion is a single STRING LITERAL, fbc prints its
            // CONTENT, not the quotes. That is what makes "#print #arg" - the standard way to see what
            // a macro argument expanded to - readable: stringizing adds the quotes, and #print takes
            // them back off. We echoed them, so every such line differed from fbc by two characters.
            WriteLn(StdErr, UnquotePPMessage(PPPrintMessage(SubstituteMacros(TrimRight(DRest) +
                            Copy(Raw, Length(TrimRight(Raw)) + 1, MaxInt), Defs, FnDefs, 0))))
          else if (DName = 'cmdline') and Emitting then
          begin
            // #cmdline "opts" - fbc appends the quoted text to its own command line. Almost none of
            // fbc's switches has a counterpart here (they name a linker, an object format, a target),
            // and pretending otherwise would be worse than ignoring them. What DOES have an observable
            // meaning is the optimisation LEVEL, because a program can read it back through
            // __FB_OPTIMIZE__ and compile differently on it. So the level is honoured and the rest of
            // the line is deliberately ignored.
            // ⛔ Honoured means REPORTED, not applied: our pipeline has no -O ladder, and the level
            // here says what the SOURCE asked for - which is exactly what fbc's macro says too.
            p := Pos('-O', DRest);
            if p > 0 then
            begin
              q := p + 2;
              while (q <= Length(DRest)) and (DRest[q] in [' ', #9]) do Inc(q);
              MacroName := '';
              while (q <= Length(DRest)) and (DRest[q] in ['0'..'9']) do
              begin MacroName := MacroName + DRest[q]; Inc(q); end;
              if MacroName <> '' then Defs.Values['__FB_OPTIMIZE__'] := MacroName;
            end;
            // ⭐ ...AND "-d NAME" / "-d NAME=VALUE" DEFINES A SYMBOL, observable on the very next line:
            // fbc's own pp/cmdline asks for a symbol on the #cmdline line and then refuses itself with
            // #error unless defined() sees it. The comment above says the switches "name a linker, an
            // object format, a target" - true of the rest, and -d is the one that is not: it is the
            // command-line spelling of #define, and it is our own front end's -d too.
            p := 1;
            while p > 0 do
            begin
              p := Pos('-d', DRest, p);
              if p = 0 then Break;
              if ((p = 1) or (DRest[p - 1] in [' ', #9, '"'])) and
                 (p + 2 <= Length(DRest)) and (DRest[p + 2] in [' ', #9]) then
              begin
                q := p + 3;
                while (q <= Length(DRest)) and (DRest[q] in [' ', #9]) do Inc(q);
                MacroName := '';
                while (q <= Length(DRest)) and IsIdentChar(DRest[q]) do
                begin MacroName := MacroName + UpCase(DRest[q]); Inc(q); end;
                if MacroName <> '' then
                begin
                  MacroVal := '';
                  if (q <= Length(DRest)) and (DRest[q] = '=') then
                  begin
                    Inc(q);
                    while (q <= Length(DRest)) and not (DRest[q] in [' ', #9, '"']) do
                    begin MacroVal := MacroVal + DRest[q]; Inc(q); end;
                  end;
                  if MacroVal = '' then MacroVal := '-1';       // "-d NAME" alone: fbc gives it -1
                  Defs.Values[MacroName] := MacroVal;
                end;
              end;
              Inc(p, 2);
            end;
          end
          else if (DName = 'error') and Emitting then
            // #error msg — abort compilation with a macro-expanded diagnostic.
            raise EPreprocessorError.Create(Trim(SubstituteMacros(DRest, Defs, FnDefs, 0)))
          else if (DName = 'assert') and Emitting then
          begin
            // #assert <expr> — abort compilation if the constant integer expression is false.
            // ⚠️ DECLARED DIVERGENCE, bounded on purpose. "#assert TypeOf(a) = TypeOf(b)" asks a
            // question the preprocessor cannot answer - it has no type information - and until now the
            // whole FILE was refused for it. But an #assert is a CHECK, not a choice: skipping one
            // means "we did not verify this", which costs a diagnostic we do not emit anyway, while
            // refusing costs the entire program. 14 tests of the fbc suite die on nothing else.
            // ⛔ NOT the same for "#if TypeOf(...)": there the answer SELECTS A BRANCH, so guessing
            // would compile different code. That one still refuses, and 6 tests still wait on real
            // type information in the preprocessor.
            if Pos('TYPEOF', UpperCase(DRest)) > 0 then
            begin
              // unevaluable here: left unchecked, deliberately
            end
            else if not EvalPPExpr(DRest, Defs, FnDefs) then
              raise EPreprocessorError.Create('assertion failed: ' + Trim(DRest));
          end;
          // All directive lines are dropped from the output; emit a blank to keep line numbers.
          Output.Add('');
        end
        else if Emitting then
        begin
          // ⛔ '_' CONTINUATION IS NOT A DIRECTIVE-ONLY RULE. The join further up is gated on
          // Trimmed[1] = '#', and that looked sufficient because the LEXER folds a continued line of
          // ordinary code on its own - a continued SUB call works. But SubstituteMacros takes ONE
          // PHYSICAL LINE, so a macro whose ARGUMENT LIST is split across lines was expanded with the
          // arguments TRUNCATED: "chk( 1, _" / "2 )" died on 'Unexpected token ")"' while the very
          // same continuation in a plain call was fine. That difference is what named it, and nine
          // tests of fbc's own suite are written this way.
          // Folded HERE, before substitution, so the macro sees the whole argument list; every line
          // swallowed leaves a blank behind, exactly as the directive join does, so the line numbers
          // the rest of the pipeline reports stay the source's own.
          ContJoin := 0;
          CutPos := LineContinuationCut(Raw);
          while (CutPos > 0) and (li + ContJoin + 1 < Lines.Count) do
          begin
            Raw := Copy(Raw, 1, CutPos - 1) + ' ' + TrimLeft(Lines[li + ContJoin + 1]);
            Inc(ContJoin);
            CutPos := LineContinuationCut(Raw);
          end;
          if IsOptionEscapeLine(Trimmed) then EscapeOn := True;   // takes effect from THIS line on
          if EscapeOn then
            ExpandedLine := ApplyEscapeRewrite(SubstituteMacros(Raw, Defs, FnDefs, 0))
          else
            ExpandedLine := SubstituteMacros(Raw, Defs, FnDefs, 0);
          // A #macro body may hold DIRECTIVES of its own - "#print", "#if", "#define" - and FreeBASIC
          // runs them when the macro is INVOKED. Our expansion produced the body as ordinary text, so a
          // '#' arrived at the parser and the whole program died on "Unexpected token #". The body
          // expands to several lines joined with cVirtualEOL, so a directive is recognisable as a
          // SEGMENT that starts with '#': when one is there, the expansion goes back through this same
          // loop (conditionals, macro table and all) and only the surviving CODE comes out.
          if ExpandedLineHasDirective(ExpandedLine) then
            Output.Add(ReprocessExpansion(ExpandedLine, Dir))
          else
            Output.Add(ExpandedLine);
          while ContJoin > 0 do                       // one blank per swallowed line: keep numbering
          begin
            Output.Add('');
            Inc(li);
            Dec(ContJoin);
          end;
        end
        else
          Output.Add('');   // excluded line — blank placeholder preserves line numbers
        Inc(li);
      end;
      // Drop any conditionals left open by this (included) text, so it can't affect the caller.
      while High(Active) > SavedStackTop do
      begin
        SetLength(Active, Length(Active) - 1);
        SetLength(Taken, Length(Taken) - 1);
      end;
    finally
      Lines.Free;
    end;
  end;

begin
  // ⭐ WHICH LANGUAGE DID THIS FILE ASK FOR? Recorded here and nowhere else: this routine is the one
  // funnel every front end (sb, sbc, the web server, the runner, immediate mode) passes a source
  // through before lexing, so a rule that must only apply to -lang fb can read the answer without
  // seven callers each having to remember to pass it. Set BEFORE the fast path below, so a file with
  // no directives at all clears a previous file's answer instead of inheriting it.
  GDeclaredNonFbDialect := DetectNonFbLang(Src);
  // Fast path: no preprocessor directive and no intrinsic-define usage -> return unchanged (zero
  // overhead for normal code). '#' covers all directives; '__' covers bare __FB_*__ intrinsic
  // macros; '$ covers the QuickBASIC '$INCLUDE metacommand; 'scape'/'SCAPE' covers OPTION
  // ESCAPE, whose string rewrite lives here (a false hit merely runs the preprocessor).
  if (Pos('#', Src) = 0) and (Pos('__', Src) = 0) and (Pos('''$', Src) = 0) and
     (Pos('scape', Src) = 0) and (Pos('SCAPE', Src) = 0) then
    Exit(Src);

  Defs := TStringList.Create;

  // __FB_UNIQUEID_* stacks are per COMPILATION: start each one empty, and restart the counter, so the
  // same source always yields the same identifiers (a REPL preprocessing twice would otherwise drift).
  if GUniqueIdStacks = nil then GUniqueIdStacks := TStringList.Create;
  for UidK := 0 to GUniqueIdStacks.Count - 1 do
    TStringList(GUniqueIdStacks.Objects[UidK]).Free;
  GUniqueIdStacks.Clear;
  GUniqueIdSerial := 0;

  SetLength(GPPLineDirectives, 0);   // per COMPILATION, like the unique-id stacks above
  IncOnce := TStringList.Create;
  PragmaOnce := TStringList.Create;
  FReprocessDepth := 0;
  FnDefs := TStringList.Create;
  Output := TStringList.Create;
  try
    RegisterIntrinsicDefines(Defs, FnDefs);   // FreeBASIC compiler intrinsic defines (__FB_*__)
    // __FILE__ expands to the top-level source file name (string literal); empty if unknown.
    // In the PLATFORM's spelling: the name arrives here however the caller wrote it, and on Windows a
    // program that prints __FILE__ got forward slashes where fbc gives backslashes. The path is the
    // same path; only the separator was ours rather than the system's.
    FileStr := FileName;
    if FileStr <> '' then FileStr := ExpandFileName(FileStr);
    Defs.Values['__FILE__'] := '"' + FileStr + '"';
    // __FILE_NQ__: same file name WITHOUT the surrounding quotes (FreeBASIC "no quotes" form).
    Defs.Values['__FILE_NQ__'] := FileStr;
    // Compilation date/time intrinsics (string literals). SedaiBasic compiles-then-runs in one
    // process, so "compilation time" is captured here, once, when preprocessing starts.
    NowDT := Now;
    Defs.Values['__DATE__']     := '"' + FormatDateTime('mm"-"dd"-"yyyy', NowDT) + '"';  // mm-dd-yyyy
    Defs.Values['__DATE_ISO__'] := '"' + FormatDateTime('yyyy"-"mm"-"dd', NowDT) + '"';  // yyyy-mm-dd
    Defs.Values['__TIME__']     := '"' + FormatDateTime('hh":"nn":"ss', NowDT) + '"';    // hh:mm:ss
    // __PATH__ expands to the absolute path of the module directory (no trailing separator).
    PathStr := BaseDir;
    if PathStr = '' then PathStr := GetCurrentDir;
    PathStr := ExcludeTrailingPathDelimiter(ExpandFileName(PathStr));
    Defs.Values['__PATH__'] := '"' + PathStr + '"';
    SetLength(Active, 0);
    SetLength(Taken, 0);
    EscapeOn := False;
    GPPSourceForDefined := Src;   // lets defined() see Const/Dim/proc declarations, like fbc
    GPPOutput := Output;          // ...positionally: the expansion so far IS the symbol table so far
  if GPPReserved = nil then GPPReserved := TStringList.Create;
  GPPReserved.Clear;            // per PROGRAM: a reservation must not survive into the next one
  GPPReserved.CaseSensitive := False;
  // ⭐ __FUNCTION__ / __FUNCTION_NQ__ are DEFINED but must NOT be substituted here: their value is the
  // name of the ENCLOSING PROCEDURE, which only the parser knows, and it already substitutes them.
  // They belong in the reserved set for exactly that reason - "#ifndef __FUNCTION__ : #error" is in
  // fbc's own pp/intrinsic, a program it accepts, and we refused it.
  GPPReserved.Values['__FUNCTION__'] := '';
  GPPReserved.Values['__FUNCTION_NQ__'] := '';
  SeedFbKeywords;
    Expand(Src, BaseDir);
    Result := Output.Text;
  finally
    Defs.Free;
    IncOnce.Free;
    PragmaOnce.Free;
    FnDefs.Free;
    GPPOutput := nil;             // it is about to be freed: nothing may scan it afterwards
    Output.Free;
  end;
end;


initialization
  // A constant expression's decimal point is a '.', whatever the machine's locale calls it: the SOURCE
  // says '.', and StrToFloat would otherwise read "1.5" as 15 on a comma-decimal locale.
  PPFloatFmt := DefaultFormatSettings;
  PPFloatFmt.DecimalSeparator := '.';

end.
