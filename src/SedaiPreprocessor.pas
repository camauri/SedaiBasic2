unit SedaiPreprocessor;
// FreeBASIC-style source preprocessor (v1). A pure text->text pass run BEFORE lexing.
// Supports:
//   #define NAME [value]      object-like macro (function-like macros are not yet supported)
//   #undef NAME
//   #ifdef NAME / #ifndef NAME / #else / #endif   conditional compilation
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
