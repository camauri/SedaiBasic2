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

interface

uses SysUtils;

type
  // Raised by #error / a failed #assert. Callers catch it to report a clean compile-time
  // diagnostic and abort the build (there is no meaningful program to run).
  EPreprocessorError = class(Exception);

function PreprocessSource(const Src, BaseDir: string; const FileName: string = ''): string;

implementation

uses Classes, SedaiLexerTypes;   // cVirtualEOL: the separator a multi-line #macro body is joined with

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
procedure SplitMacroArgs(const S: string; out Args: array of string; out Count: Integer);
var
  i, depth: Integer;
  cur: string;
  InStr: Boolean;
begin
  Count := 0; cur := ''; depth := 0; InStr := False;
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
      Inc(Count); cur := '';
    end
    else cur := cur + S[i];
  end;
  if (Trim(cur) <> '') or (Count > 0) then
  begin
    if Count <= High(Args) then Args[Count] := Trim(cur);
    Inc(Count);
  end;
end;

// Expand a function-like macro body by replacing each whole-identifier parameter with its argument.
// ParamsBody is "p1,p2,..."#1"body"; ArgsStr is the raw argument text between the parentheses.
function ExpandFnBody(const ParamsBody, ArgsStr: string): string;
var
  sep, i, j, k, pi: Integer;
  ParamList, Body, Word: string;
  Params: array of string;
  Args: array[0..63] of string;
  PCount, ACount: Integer;
  InStr: Boolean;

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
  SplitMacroArgs(ArgsStr, Args, ACount);
  // Replace each whole-identifier parameter with its argument, handling the FreeBASIC preprocessor
  // operators: "#param" stringizes the argument; "a ## b" pastes the surrounding tokens together.
  Result := ''; i := 1; InStr := False;
  while i <= Length(Body) do
  begin
    if InStr then begin Result := Result + Body[i]; if Body[i] = '"' then InStr := False; Inc(i); Continue; end;
    if Body[i] = '"' then begin InStr := True; Result := Result + Body[i]; Inc(i); Continue; end;
    // Token paste "##": drop trailing whitespace already emitted and skip whitespace after ##.
    if (Body[i] = '#') and (i < Length(Body)) and (Body[i + 1] = '#') then
    begin
      while (Length(Result) > 0) and (Result[Length(Result)] in [' ', #9]) do
        Delete(Result, Length(Result), 1);
      Inc(i, 2);
      while (i <= Length(Body)) and (Body[i] in [' ', #9]) do Inc(i);
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
        if (pi >= 0) and (pi < ACount) then
        begin Result := Result + Stringize(Args[pi]); i := k; Continue; end;
      end;
      Result := Result + '#'; Inc(i); Continue;   // a lone '#' that is not a stringize
    end;
    if Body[i] in ['A'..'Z', 'a'..'z', '_'] then
    begin
      j := i;
      while (j <= Length(Body)) and IsIdentChar(Body[j]) do Inc(j);
      Word := Copy(Body, i, j - i);
      pi := ParamIndex(Word);
      if (pi >= 0) and (pi < ACount) then Result := Result + Args[pi]
      else Result := Result + Word;
      i := j;
    end
    else begin Result := Result + Body[i]; Inc(i); end;
  end;
end;

function SubstituteMacros(const Line: string; Defs, FnDefs: TStringList): string;
var
  i, j, idx, depth: Integer;
  Word, ArgsStr: string;
  InStr: Boolean;
begin
  Result := '';
  i := 1;
  InStr := False;
  while i <= Length(Line) do
  begin
    if InStr then
    begin
      Result := Result + Line[i];
      if Line[i] = '"' then InStr := False;
      Inc(i);
      Continue;
    end;
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
      // Function-like macro: NAME immediately followed by '(' — expand with its arguments.
      idx := FnDefs.IndexOfName(UpperCase(Word));
      if (idx >= 0) and (j <= Length(Line)) and (Line[j] = '(') then
      begin
        depth := 0; ArgsStr := '';
        Inc(j);   // skip '('
        while j <= Length(Line) do
        begin
          if (Line[j] = '(') then Inc(depth)
          else if (Line[j] = ')') then
          begin
            if depth = 0 then Break;
            Dec(depth);
          end;
          ArgsStr := ArgsStr + Line[j];
          Inc(j);
        end;
        if (j <= Length(Line)) and (Line[j] = ')') then Inc(j);   // skip ')'
        // Expand the body (param substitution), then re-run object-like substitution on the result.
        Result := Result + SubstituteMacros(ExpandFnBody(FnDefs.ValueFromIndex[idx], ArgsStr), Defs, FnDefs);
        i := j;
        Continue;
      end;
      idx := Defs.IndexOfName(UpperCase(Word));
      if idx >= 0 then
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

var
  // The full source text of the module being preprocessed, for SourceDeclaresSymbol below.
  // Set by PreprocessSource before Expand; the preprocessor is single-threaded by design.
  GPPSourceForDefined: string = '';

function SourceDeclaresSymbol(const Nm: string): Boolean;
// fbc's Defined() answers TRUE for COMPILER-level symbols too, not only #defines: a Const, a
// Dim/Redim/Static variable, a Sub/Function name (fbc-verified: examples/manual/prepro/defined
// expects a Const and a Dim to count). This preprocessor runs on TEXT before any symbol table
// exists, so the question is answered by a declaration-shaped scan: a line whose first word is
// a declaring keyword and that contains Nm as a whole word. A name inside a same-line comment
// or string can false-positive - accepted for a #if convenience predicate.
var
  L: TStringList;
  i, p, q: Integer;
  U, W: string;
begin
  Result := False;
  if Nm = '' then Exit;
  L := TStringList.Create;
  try
    L.Text := GPPSourceForDefined;
    for i := 0 to L.Count - 1 do
    begin
      U := UpperCase(TrimLeft(L[i]));
      p := 1;
      while (p <= Length(U)) and IsIdentChar(U[p]) do Inc(p);
      W := Copy(U, 1, p - 1);
      if (W = 'CONST') or (W = 'DIM') or (W = 'REDIM') or (W = 'STATIC') or (W = 'VAR') or
         (W = 'SUB') or (W = 'FUNCTION') or (W = 'DECLARE') or (W = 'TYPE') or
         (W = 'ENUM') or (W = 'COMMON') then
      begin
        q := Pos(Nm, U);
        while q > 0 do
        begin
          if ((q = 1) or not IsIdentChar(U[q - 1])) and
             ((q + Length(Nm) > Length(U)) or not IsIdentChar(U[q + Length(Nm)])) then
            Exit(True);
          q := Pos(Nm, U, q + 1);
        end;
      end;
    end;
  finally
    L.Free;
  end;
end;

// Evaluate a #if / #elif constant integer expression. Supports: decimal and &H/&O/&B literals;
// defined(NAME) / defined NAME; bare macro names (-> their integer value, or 0 if undefined or
// non-numeric); parentheses; unary "-"/"+" and NOT/"!"; "*" "/" "\" MOD; "+" "-"; comparisons
// "=" "==" "<>" "!=" "<" "<=" ">" ">="; AND/"&&"; OR/"||". Nonzero result => take the branch. On any
// problem it returns False (safe default: branch not taken).
function EvalPPExpr(const RawExpr: string; Defs: TStringList): Boolean;
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
  var p, q: Integer; id, two: string; nm: string;
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
          q := p;
          while (q <= Length(S)) and IsIdentChar(S[q]) do Inc(q);
          nm := UpperCase(Copy(S, p, q - p)); p := q;
          while (p <= Length(S)) and (S[p] in [' ', #9, ')']) do Inc(p);
          if (Defs.IndexOfName(nm) >= 0) or SourceDeclaresSymbol(nm) then
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
        else if Defs.IndexOfName(id) >= 0 then
        begin
          // Re-tokenize the macro's value so multi-token values (-1, &HFF, 1+2) and nested
          // macros work; bail to 0 past a sane nesting depth (cycle guard).
          if Depth < 32 then Tokenize(Trim(Defs.Values[id]), Depth + 1)
          else Toks.Add('0');
        end
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

  function ParseCmp: Int64;
  var op: string; l, r: Int64; b: Boolean;
  begin
    Result := ParseAdd;
    while (Peek='=') or (Peek='==') or (Peek='<>') or (Peek='!=') or (Peek='<') or (Peek='<=') or (Peek='>') or (Peek='>=') do
    begin
      op := Peek; Inc(TPos); l := Result; r := ParseAdd;
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
  Toks := TStringList.Create;
  try
    Tokenize(RawExpr, 0);
    if Toks.Count = 0 then Exit;
    TPos := 0;
    Result := ParseOr <> 0;
  finally
    Toks.Free;
  end;
end;

procedure RegisterIntrinsicDefines(Defs: TStringList);
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
  Defs.Values['__FB_SIGNATURE__'] := '"SedaiBasic (FreeBASIC-compatible)"';
  // --- Language / compile mode, mapped to SedaiBasic's actual state ---
  Defs.Values['__FB_LANG__']    := '"fb"';
  Defs.Values['__FB_MT__']      := '-1';   // multithreading runtime is available
  Defs.Values['__FB_OUT_EXE__'] := '-1';   // programs are run (executable-like target)
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
  EscapeOn: Boolean;     // OPTION ESCAPE seen: plain "..." strings become escaped from here on

  function Emitting: Boolean;
  begin
    Result := (Length(Active) = 0) or Active[High(Active)];
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

  procedure Expand(const Text, Dir: string);
  var
    Lines: TStringList;
    li, p, q: Integer;
    Raw, Trimmed, DName, DRest, MacroName, MacroVal, FileName, FullPath: string;
    Params, MacroBody, BodyTrim, EName, ERest: string;
    IsFn: Boolean;
    ParentEmit, Cond: Boolean;
    IncText: TStringList;
    SavedStackTop: Integer;
  begin
    SavedStackTop := High(Active);   // remember depth so includes can't leak unbalanced conditionals
    Lines := TStringList.Create;
    try
      Lines.Text := Text;
      li := 0;
      while li < Lines.Count do
      begin
        Raw := Lines[li];
        Trimmed := TrimLeft(Raw);
        // __LINE__ expands to the current source line number (1-based). Updated every line so it is
        // correct wherever it appears; __FILE__ is set once (top-level file) in the begin block below.
        Defs.Values['__LINE__'] := IntToStr(li + 1);
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
            end;
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
        if (Length(Trimmed) > 0) and (Trimmed[1] = '#') then
        begin
          SplitDirective(Trimmed, DName, DRest);
          if DName = 'ifdef' then
          begin
            ParentEmit := Emitting;
            Cond := ParentEmit and (Defs.IndexOfName(UpperCase(Trim(DRest))) >= 0);
            SetLength(Active, Length(Active) + 1); Active[High(Active)] := Cond;
            SetLength(Taken, Length(Taken) + 1);   Taken[High(Taken)] := Cond;
          end
          else if DName = 'ifndef' then
          begin
            ParentEmit := Emitting;
            Cond := ParentEmit and (Defs.IndexOfName(UpperCase(Trim(DRest))) < 0);
            SetLength(Active, Length(Active) + 1); Active[High(Active)] := Cond;
            SetLength(Taken, Length(Taken) + 1);   Taken[High(Taken)] := Cond;
          end
          else if DName = 'if' then
          begin
            ParentEmit := Emitting;
            Cond := ParentEmit and EvalPPExpr(DRest, Defs);
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
                  Cond := ParentEmit and EvalPPExpr(DRest, Defs);
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
            IsFn := (p <= Length(DRest)) and (DRest[p] = '(');
            Params := '';
            if IsFn then
            begin
              q := p + 1;
              while (q <= Length(DRest)) and (DRest[q] <> ')') do Inc(q);
              Params := Trim(Copy(DRest, p + 1, q - p - 1));
            end;
            MacroBody := '';
            Inc(li);
            while li < Lines.Count do
            begin
              BodyTrim := TrimLeft(Lines[li]);
              if (Length(BodyTrim) > 0) and (BodyTrim[1] = '#') then
              begin
                SplitDirective(BodyTrim, EName, ERest);
                if EName = 'endmacro' then begin Output.Add(''); Break; end;
              end;
              if Trim(Lines[li]) <> '' then
              begin
                if MacroBody <> '' then MacroBody := MacroBody + cVirtualEOL;
                MacroBody := MacroBody + Trim(Lines[li]);
              end;
              Output.Add('');   // blank placeholder preserves line numbers
              Inc(li);
            end;
            if MacroName <> '' then
            begin
              if IsFn then FnDefs.Values[MacroName] := Params + #1 + MacroBody
              else Defs.Values[MacroName] := MacroBody;
            end;
          end
          else if (DName = 'undef') and Emitting then
          begin
            p := Defs.IndexOfName(UpperCase(Trim(DRest)));
            if p >= 0 then Defs.Delete(p);
          end
          else if (DName = 'include') and Emitting then
          begin
            FileName := Trim(DRest);
            if (Length(FileName) >= 2) and (FileName[1] = '"') then
              FileName := Copy(FileName, 2, Length(FileName) - 2);
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
            end;
          end
          else if (DName = 'print') and Emitting then
            // #print msg — emit a compile-time diagnostic (macro-expanded) to stderr.
            WriteLn(StdErr, SubstituteMacros(DRest, Defs, FnDefs))
          else if (DName = 'error') and Emitting then
            // #error msg — abort compilation with a macro-expanded diagnostic.
            raise EPreprocessorError.Create(Trim(SubstituteMacros(DRest, Defs, FnDefs)))
          else if (DName = 'assert') and Emitting then
          begin
            // #assert <expr> — abort compilation if the constant integer expression is false.
            if not EvalPPExpr(DRest, Defs) then
              raise EPreprocessorError.Create('assertion failed: ' + Trim(DRest));
          end;
          // All directive lines are dropped from the output; emit a blank to keep line numbers.
          Output.Add('');
        end
        else if Emitting then
        begin
          if IsOptionEscapeLine(Trimmed) then EscapeOn := True;   // takes effect from THIS line on
          if EscapeOn then
            Output.Add(ApplyEscapeRewrite(SubstituteMacros(Raw, Defs, FnDefs)))
          else
            Output.Add(SubstituteMacros(Raw, Defs, FnDefs));
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
  // Fast path: no preprocessor directive and no intrinsic-define usage -> return unchanged (zero
  // overhead for normal code). '#' covers all directives; '__' covers bare __FB_*__ intrinsic
  // macros; '$ covers the QuickBASIC '$INCLUDE metacommand; 'scape'/'SCAPE' covers OPTION
  // ESCAPE, whose string rewrite lives here (a false hit merely runs the preprocessor).
  if (Pos('#', Src) = 0) and (Pos('__', Src) = 0) and (Pos('''$', Src) = 0) and
     (Pos('scape', Src) = 0) and (Pos('SCAPE', Src) = 0) then
    Exit(Src);

  Defs := TStringList.Create;
  FnDefs := TStringList.Create;
  Output := TStringList.Create;
  try
    RegisterIntrinsicDefines(Defs);   // FreeBASIC compiler intrinsic defines (__FB_*__)
    // __FILE__ expands to the top-level source file name (string literal); empty if unknown.
    Defs.Values['__FILE__'] := '"' + FileName + '"';
    // __FILE_NQ__: same file name WITHOUT the surrounding quotes (FreeBASIC "no quotes" form).
    Defs.Values['__FILE_NQ__'] := FileName;
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
    Expand(Src, BaseDir);
    Result := Output.Text;
  finally
    Defs.Free;
    FnDefs.Free;
    Output.Free;
  end;
end;

end.
