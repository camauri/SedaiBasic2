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
function DetectQBLang(const Src: string): Boolean;

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

// Expand a function-like macro body by replacing each whole-identifier parameter with its argument.
// ParamsBody is "p1,p2,..."#1"body"; ArgsStr is the raw argument text between the parentheses.
function ExpandFnBody(const ParamsBody, ArgsStr: string): string;
var
  sep, i, j, k, pi: Integer;
  ParamList, Body, Word: string;
  Params: array of string;
  Args: array[0..63] of string;
  Starts: array[0..63] of Integer;
  PCount, ACount, VarIdx: Integer;
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
      if StrToInt64Def(Trim(Cond), 0) <> 0 then Value := Trim(Args[1]) else Value := Trim(Args[2]);
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

function SubstituteMacros(const Line: string; Defs, FnDefs: TStringList; Depth: Integer): string;
var
  i, j, k, idx, ParenDepth: Integer;
  Word, ArgsStr, BuiltinVal: string;
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
      // A BUILT-IN function-like macro is tried first: it is the preprocessor's own, and a program may
      // not shadow it with a #define.
      if (j <= Length(Line)) and (Line[j] = '(') and (Copy(UpperCase(Word), 1, 5) = '__FB_') then
      begin
        ParenDepth := 0; ArgsStr := '';
        k := j + 1;
        while k <= Length(Line) do
        begin
          if (Line[k] = '(') then Inc(ParenDepth)
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
      if (idx >= 0) and (k <= Length(Line)) and (Line[k] = '(') then
      begin
        j := k;
        ParenDepth := 0; ArgsStr := '';
        Inc(j);   // skip '('
        while j <= Length(Line) do
        begin
          if (Line[j] = '(') then Inc(ParenDepth)
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
        Result := Result + SubstituteMacros(ExpandFnBody(FnDefs.ValueFromIndex[idx], ArgsStr), Defs, FnDefs, Depth + 1);
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

function UnquotePPMessage(const S: string): string;
// A "#print" message that is EXACTLY one string literal prints as its content: fbc shows
//   #print "quoted"      -> quoted
//   #print #arg          -> whatever arg expanded to, unquoted
// and the second is the whole point of stringizing an argument to look at it. Anything else - a bare
// word, a literal with something after it, an unterminated quote - is echoed verbatim, because then
// the quotes are part of what the author wrote rather than a wrapper the expansion put on.
// Escaped quotes inside are left alone: fbc does not process escapes here either.
var
  T: string;
begin
  Result := S;
  T := Trim(S);
  if (Length(T) >= 2) and (T[1] = '"') and (T[Length(T)] = '"') and
     (Pos('"', Copy(T, 2, Length(T) - 2)) = 0) then
    Result := Copy(T, 2, Length(T) - 2);
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
function EvalPPExprInt(const RawExpr: string; Defs: TStringList; out V: Int64): Boolean; forward;

function EvalPPExpr(const RawExpr: string; Defs: TStringList): Boolean;
// #if / #elseif: the expression as a CONDITION.
var
  V: Int64;
begin
  Result := EvalPPExprInt(RawExpr, Defs, V) and (V <> 0);
end;

function EvalPPExprInt(const RawExpr: string; Defs: TStringList; out V: Int64): Boolean;
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
// The value of a constant INTEGER expression as text, or the expression unchanged when it is not one.
//
// "Not one" has to be tested up front: the expression tokenizer resolves an unknown identifier to 0, so
// asking it to evaluate "4 * Atn(1)" would answer 0 - a wrong VALUE where the honest answer is "I do not
// fold this". So an identifier that is not a known macro means: leave the text alone. (fbc's own
// __FB_EVAL__ does fold floats and intrinsics; we fold integers, and say so by not pretending.)
var
  V: Int64;
  i, j: Integer;
  W: string;
begin
  Result := Trim(Expr);
  i := 1;
  while i <= Length(Expr) do
    if Expr[i] in ['A'..'Z', 'a'..'z', '_'] then
    begin
      j := i;
      while (j <= Length(Expr)) and IsIdentChar(Expr[j]) do Inc(j);
      W := UpperCase(Copy(Expr, i, j - i));
      if (W <> 'MOD') and (W <> 'AND') and (W <> 'OR') and (W <> 'NOT') and
         (W <> 'SHL') and (W <> 'SHR') and (Defs.IndexOfName(W) < 0) then Exit;
      i := j;
    end
    else
      Inc(i);
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
  // fbc defines this while compiling the module that holds the program's entry point. There is exactly
  // one module here - sb compiles and runs a single source - so it is always the main one.
  Defs.Values['__FB_MAIN__']    := '-1';
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
  IncOnce: TStringList;  // full paths already spliced by an "#include Once" (that is what ONCE means)
  ExpandedLine: string;  // a source line after macro substitution
  FReprocessDepth: Integer;   // guard against a macro whose expansion expands to itself
  UidK: Integer;         // scratch: clearing the __FB_UNIQUEID_* stacks at entry

  function Emitting: Boolean;
  begin
    Result := (Length(Active) = 0) or Active[High(Active)];
  end;

  // Forward: ReprocessExpansion feeds a macro expansion back through Expand, which is declared below.
  procedure Expand(const Text, Dir: string); forward;

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

  procedure Expand(const Text, Dir: string);
  var
    Lines: TStringList;
    li, p, q: Integer;
    Raw, Trimmed, DName, DRest, MacroName, MacroVal, FileName, FullPath: string;
    Params, MacroBody, BodyTrim, EName, ERest: string;
    IsFn: Boolean;
    ParentEmit, Cond: Boolean;
    IncText: TStringList;
    IncludeOnce: Boolean;   // "#include Once": splice this path at most one time
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
            // "#macro m ( arg1, arg2 )": FreeBASIC allows space before the parameter list, and the
            // manual writes it that way. Testing the very next character made such a macro OBJECT-like,
            // so an invocation expanded to the raw body and its arguments leaked out as code.
            while (p <= Length(DRest)) and (DRest[p] in [' ', #9]) do Inc(p);
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
              if IncludeOnce and (IncOnce.IndexOf(UpperCase(ExpandFileName(FullPath))) >= 0) then
                IncText := nil                                  // already spliced: ONCE means once
              else
              begin
                if IncludeOnce then IncOnce.Add(UpperCase(ExpandFileName(FullPath)));
                IncText := TStringList.Create;
                try
                  IncText.LoadFromFile(FullPath);
                  Expand(IncText.Text, ExtractFilePath(ExpandFileName(FullPath)));
                finally
                  IncText.Free;
                end;
              end;
            end
            else
              RegisterEmulatedHeader(FileName, Defs, FnDefs);
          end
          else if (DName = 'print') and Emitting then
            // #print msg — emit a compile-time diagnostic (macro-expanded) to stderr. The message is
            // the rest of the line VERBATIM, trailing blanks included: fbc echoes exactly what was
            // written, and "#print Release mode " really does end in a space.
            // ...but when what is left after expansion is a single STRING LITERAL, fbc prints its
            // CONTENT, not the quotes. That is what makes "#print #arg" - the standard way to see what
            // a macro argument expanded to - readable: stringizing adds the quotes, and #print takes
            // them back off. We echoed them, so every such line differed from fbc by two characters.
            WriteLn(StdErr, UnquotePPMessage(SubstituteMacros(TrimRight(DRest) +
                            Copy(Raw, Length(TrimRight(Raw)) + 1, MaxInt), Defs, FnDefs, 0)))
          else if (DName = 'error') and Emitting then
            // #error msg — abort compilation with a macro-expanded diagnostic.
            raise EPreprocessorError.Create(Trim(SubstituteMacros(DRest, Defs, FnDefs, 0)))
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

  // __FB_UNIQUEID_* stacks are per COMPILATION: start each one empty, and restart the counter, so the
  // same source always yields the same identifiers (a REPL preprocessing twice would otherwise drift).
  if GUniqueIdStacks = nil then GUniqueIdStacks := TStringList.Create;
  for UidK := 0 to GUniqueIdStacks.Count - 1 do
    TStringList(GUniqueIdStacks.Objects[UidK]).Free;
  GUniqueIdStacks.Clear;
  GUniqueIdSerial := 0;

  IncOnce := TStringList.Create;
  FReprocessDepth := 0;
  FnDefs := TStringList.Create;
  Output := TStringList.Create;
  try
    RegisterIntrinsicDefines(Defs);   // FreeBASIC compiler intrinsic defines (__FB_*__)
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
    Expand(Src, BaseDir);
    Result := Output.Text;
  finally
    Defs.Free;
    IncOnce.Free;
    FnDefs.Free;
    Output.Free;
  end;
end;

end.
