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
function SubstituteMacros(const Line: string; Defs: TStringList): string;
var
  i, j, idx: Integer;
  Word: string;
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
  Defs: TStringList;     // Names = UPPER macro names, Values = macro bodies
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
    li, p: Integer;
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
            MacroVal := Trim(Copy(DRest, p, MaxInt));
            if MacroName <> '' then Defs.Values[MacroName] := MacroVal;
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
          Output.Add(SubstituteMacros(Raw, Defs))
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
  Output := TStringList.Create;
  try
    SetLength(Active, 0);
    SetLength(Taken, 0);
    Expand(Src, BaseDir);
    Result := Output.Text;
  finally
    Defs.Free;
    Output.Free;
  end;
end;

end.
