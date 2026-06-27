unit SedaiPreprocessor;
// FreeBASIC-style source preprocessor (v1). A pure text->text pass run BEFORE lexing.
// Supports:
//   #define NAME [value]      object-like macro (function-like macros are not yet supported)
//   #undef NAME
//   #ifdef NAME / #ifndef NAME / #else / #endif   conditional compilation
//   #if <expr> / #elif <expr>   conditional compilation on a constant integer expression
//       (literals, defined(NAME), macro values, comparisons, AND/OR/NOT, parentheses)
//   #include "file"           splice another file (relative to the including file's directory)
// Directive lines in the top-level file are blanked (kept as empty lines) so error line numbers in
// that file are preserved; included files are appended after preprocessing (their line numbers shift).
// Object-like macro names are substituted as whole words outside string literals.

{$mode objfpc}{$H+}

interface

function PreprocessSource(const Src, BaseDir: string): string;

implementation

uses Classes, SysUtils;

function IsIdentChar(C: Char): Boolean; inline;
begin
  Result := (C in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
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
  sep, i, j, pi: Integer;
  ParamList, Body, Word: string;
  Params: array of string;
  Args: array[0..63] of string;
  PCount, ACount: Integer;
  InStr: Boolean;
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
  // replace each whole-identifier parameter occurrence in the body with the matching argument
  Result := ''; i := 1; InStr := False;
  while i <= Length(Body) do
  begin
    if InStr then begin Result := Result + Body[i]; if Body[i] = '"' then InStr := False; Inc(i); Continue; end;
    if Body[i] = '"' then begin InStr := True; Result := Result + Body[i]; Inc(i); Continue; end;
    if Body[i] in ['A'..'Z', 'a'..'z', '_'] then
    begin
      j := i;
      while (j <= Length(Body)) and IsIdentChar(Body[j]) do Inc(j);
      Word := Copy(Body, i, j - i);
      pi := -1;
      for sep := 0 to PCount - 1 do
        if Params[sep] = Word then begin pi := sep; Break; end;
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

  // Tokenize, substituting defined()/macros into numeric tokens as we go.
  procedure Tokenize(const S: string);
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
          if Defs.IndexOfName(nm) >= 0 then Toks.Add('1') else Toks.Add('0');
        end
        else if (id = 'AND') or (id = 'OR') or (id = 'NOT') or (id = 'MOD') then
          Toks.Add(id)
        else if Defs.IndexOfName(id) >= 0 then
          Toks.Add(Trim(Defs.Values[id]))     // macro value (numeric expected)
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
    if t = '(' then begin Inc(TPos); Result := ParseOr; if Peek = ')' then Inc(TPos); end
    else if (t = 'NOT') or (t = '!') then begin Inc(TPos); if ParsePrimary <> 0 then Result := 0 else Result := 1; end
    else if t = '-' then begin Inc(TPos); Result := -ParsePrimary; end
    else if t = '+' then begin Inc(TPos); Result := ParsePrimary; end
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
    Tokenize(RawExpr);
    if Toks.Count = 0 then Exit;
    TPos := 0;
    Result := ParseOr <> 0;
  finally
    Toks.Free;
  end;
end;

function PreprocessSource(const Src, BaseDir: string): string;
var
  Defs: TStringList;     // Names = UPPER object-like macro names, Values = macro bodies
  FnDefs: TStringList;   // Names = UPPER function-like macro names, Values = "params"#1"body"
  Output: TStringList;
  // Conditional stack: Active[k] = currently emitting at nesting level k (already factors parents);
  // Taken[k] = a branch has been taken at this level (for #else).
  Active, Taken: array of Boolean;

  function Emitting: Boolean;
  begin
    Result := (Length(Active) = 0) or Active[High(Active)];
  end;

  procedure Expand(const Text, Dir: string);
  var
    Lines: TStringList;
    li, p, q: Integer;
    Raw, Trimmed, DName, DRest, MacroName, MacroVal, FileName, FullPath: string;
    ParentEmit, Cond: Boolean;
    IncText: TStringList;
    SavedStackTop: Integer;
  begin
    SavedStackTop := High(Active);   // remember depth so includes can't leak unbalanced conditionals
    Lines := TStringList.Create;
    try
      Lines.Text := Text;
      for li := 0 to Lines.Count - 1 do
      begin
        Raw := Lines[li];
        Trimmed := TrimLeft(Raw);
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
          else if DName = 'elif' then
          begin
            if Length(Active) > 0 then
            begin
              ParentEmit := (Length(Active) = 1) or Active[High(Active) - 1];
              if Taken[High(Taken)] then
                Active[High(Active)] := False                  // an earlier branch already won
              else
              begin
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
              MacroVal := Trim(Copy(DRest, p + 1, q - p - 1)) + #1 + Trim(Copy(DRest, q + 1, MaxInt));
              if MacroName <> '' then FnDefs.Values[MacroName] := MacroVal;
            end
            else
            begin
              MacroVal := Trim(Copy(DRest, p, MaxInt));
              if MacroName <> '' then Defs.Values[MacroName] := MacroVal;
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
          end;
          // All directive lines are dropped from the output; emit a blank to keep line numbers.
          Output.Add('');
        end
        else if Emitting then
          Output.Add(SubstituteMacros(Raw, Defs, FnDefs))
        else
          Output.Add('');   // excluded line — blank placeholder preserves line numbers
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
  // Fast path: no preprocessor directive at all -> return unchanged (zero overhead for normal code).
  if Pos('#', Src) = 0 then
    Exit(Src);

  Defs := TStringList.Create;
  FnDefs := TStringList.Create;
  Output := TStringList.Create;
  try
    SetLength(Active, 0);
    SetLength(Taken, 0);
    Expand(Src, BaseDir);
    Result := Output.Text;
  finally
    Defs.Free;
    FnDefs.Free;
    Output.Free;
  end;
end;

end.
