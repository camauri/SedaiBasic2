unit SedaiConfig;

{$mode objfpc}{$H+}
{$codepage UTF8}

// ⭐ THE CONFIGURATION FILE, and the reason it exists rather than a constant somewhere.
//
// ⛔ NO PATH IS EVER HARD-CODED (owner, 8 Sep 2026). A compiler needs to know where things are - the
// headers a program includes, the directories a library is looked for in - and every way of answering
// that question except one is a lie waiting to happen: a constant is right on the machine it was
// written on, an installer's guess is right until someone moves the tree. So the answers come from,
// in this order, most specific first:
//
//   1. the command line            -i <path>          the run says it
//   2. the environment             SEDAI_INCLUDE      the shell says it
//   3. sedai.conf beside the SOURCE                   the project says it
//   4. sedai.conf in the user's config directory      the user says it
//   5. sedai.conf beside the EXECUTABLE               the installation says it
//
// ⚠️ Note what is NOT a hard-coded path: the executable's own location and the PATH are DISCOVERED at
// run time, so using them is finding out where we are, not assuming it.
//
// The format is deliberately the smallest thing that works: "key = value", one per line, '#' or "'"
// starts a comment, and a key may REPEAT - repeats build a list, in file order, which is what a search
// path is. No sections, no types, no escaping: a configuration language is a language, and this file
// should never become one.

interface

uses
  Classes, SysUtils;

{ Load the configuration, once. ASourceDir is the directory of the program being compiled, and may be
  empty; it contributes the most specific file. Calling it again with a different directory re-reads. }
procedure LoadConfig(const ASourceDir: string);

{ Every value given for AKey, in resolution order (most specific first). Never nil; may be empty.
  The list belongs to this unit - read it, do not free it. }
function ConfigList(const AKey: string): TStringList;

{ The first value for AKey, or ADefault when the key was never given. }
function ConfigValue(const AKey: string; const ADefault: string = ''): string;

{ Where the files actually read came from, for a diagnostic that can be believed. }
function ConfigSources: string;

implementation

var
  GConf: TStringList = nil;        // "KEY=value", in resolution order, duplicates kept
  GSources: TStringList = nil;
  GLoadedFor: string = #1;         // the ASourceDir the current load was made for (#1 = never loaded)
  GScratch: TStringList = nil;     // returned by ConfigList; rebuilt per call

function UserConfigDir: string;
// Where a user's own settings live, asked of the SYSTEM rather than assumed.
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA');
  if Result <> '' then Result := IncludeTrailingPathDelimiter(Result) + 'Sedai';
  {$ELSE}
  Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if Result = '' then
  begin
    Result := GetEnvironmentVariable('HOME');
    if Result <> '' then Result := IncludeTrailingPathDelimiter(Result) + '.config';
  end;
  if Result <> '' then Result := IncludeTrailingPathDelimiter(Result) + 'sedai';
  {$ENDIF}
end;

procedure ReadOneFile(const APath: string);
// ⛔ A missing file is not an error and not a warning: only the LAST of the five is likely to exist on
// any given machine, and complaining about the other four would make every run noisy.
var
  L: TStringList;
  i, p: Integer;
  Line, Key, Val: string;
begin
  if (APath = '') or not FileExists(APath) then Exit;
  L := TStringList.Create;
  try
    try
      L.LoadFromFile(APath);
    except
      Exit;                                  // unreadable is the same as absent
    end;
    for i := 0 to L.Count - 1 do
    begin
      Line := Trim(L[i]);
      if Line = '' then Continue;
      if (Line[1] = '#') or (Line[1] = '''') then Continue;
      p := Pos('=', Line);
      if p <= 1 then Continue;
      Key := UpperCase(Trim(Copy(Line, 1, p - 1)));
      Val := Trim(Copy(Line, p + 1, MaxInt));
      // A trailing comment is ordinary in a config file, and a value is never a quoted string here.
      p := Pos(' #', Val); if p > 0 then Val := TrimRight(Copy(Val, 1, p - 1));
      if (Length(Val) >= 2) and (Val[1] = '"') and (Val[Length(Val)] = '"') then
        Val := Copy(Val, 2, Length(Val) - 2);
      if (Key <> '') and (Val <> '') then GConf.Add(Key + '=' + Val);
    end;
    GSources.Add(APath);
  finally
    L.Free;
  end;
end;

procedure LoadConfig(const ASourceDir: string);
var
  EnvVal: string;
  Parts: TStringList;
  i: Integer;
begin
  if GLoadedFor = ASourceDir then Exit;
  GLoadedFor := ASourceDir;
  if GConf = nil then GConf := TStringList.Create else GConf.Clear;
  if GSources = nil then GSources := TStringList.Create else GSources.Clear;

  // 2. the environment. SEDAI_INCLUDE holds a path LIST, in the platform's own separator - the same
  //    shape PATH has, so it is the shape a shell user already knows.
  EnvVal := GetEnvironmentVariable('SEDAI_INCLUDE');
  if EnvVal <> '' then
  begin
    Parts := TStringList.Create;
    try
      Parts.Delimiter := PathSeparator;
      Parts.StrictDelimiter := True;
      Parts.DelimitedText := EnvVal;
      for i := 0 to Parts.Count - 1 do
        if Trim(Parts[i]) <> '' then GConf.Add('INCLUDE=' + Trim(Parts[i]));
      GSources.Add('$SEDAI_INCLUDE');
    finally
      Parts.Free;
    end;
  end;
  EnvVal := GetEnvironmentVariable('SEDAI_FBC');
  if EnvVal <> '' then begin GConf.Add('FBC=' + EnvVal); GSources.Add('$SEDAI_FBC'); end;

  // 3, 4, 5 - most specific first, because ConfigValue answers the FIRST match.
  if ASourceDir <> '' then
    ReadOneFile(IncludeTrailingPathDelimiter(ASourceDir) + 'sedai.conf');
  if UserConfigDir <> '' then
    ReadOneFile(IncludeTrailingPathDelimiter(UserConfigDir) + 'sedai.conf');
  ReadOneFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'sedai.conf');
end;

function ConfigList(const AKey: string): TStringList;
var
  i: Integer;
  K: string;
begin
  if GScratch = nil then GScratch := TStringList.Create else GScratch.Clear;
  Result := GScratch;
  if GConf = nil then Exit;
  K := UpperCase(AKey) + '=';
  for i := 0 to GConf.Count - 1 do
    if Copy(GConf[i], 1, Length(K)) = K then
      Result.Add(Copy(GConf[i], Length(K) + 1, MaxInt));
end;

function ConfigValue(const AKey: string; const ADefault: string = ''): string;
var
  L: TStringList;
begin
  L := ConfigList(AKey);
  if L.Count > 0 then Result := L[0] else Result := ADefault;
end;

function ConfigSources: string;
begin
  if (GSources = nil) or (GSources.Count = 0) then Exit('(none)');
  Result := StringReplace(TrimRight(GSources.Text), sLineBreak, ', ', [rfReplaceAll]);
end;

finalization
  FreeAndNil(GConf);
  FreeAndNil(GSources);
  FreeAndNil(GScratch);
end.
