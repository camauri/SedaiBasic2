unit SedaiFFI;

{$mode objfpc}{$H+}
{$codepage UTF8}

// ⭐ OPENING A LIBRARY AND FINDING A SYMBOL - and, since 8 Sep 2026, nothing else.
//
// ⛔ THIS UNIT USED TO LOAD libffi. The owner's decision that day removed that dependency on BOTH
// targets: «non voglio dipendenze da libffi su Windows! Anzi, mi chiedo se ne avevamo bisogno anche su
// Linux!» The calling convention is now ours and lives in SedaiAbi - see the note there on why the
// cost turned out lower than the old comment here argued, and on which half of the work is the hard
// one. What is left in this unit is the part that was never libffi's: asking the LOADER for a library
// and for a symbol, which FPC's dynlibs already does portably.
//
// ⚠️ Keeping the file rather than deleting it is deliberate: the two spellings a foreign declaration
// can use for a library name are a fact about the LOADER, not about the ABI, and they had already been
// measured against real systems here.

interface

uses
  Classes, SysUtils, dynlibs, SedaiAbi;

// True once a foreign call is possible at all - which now means "this architecture has a calling
// convention written for it", not "a shared library was found".
function FFIAvailable: Boolean;
function FFIUnavailableReason: string;

{ ⭐ WHERE A LIBRARY IS LOOKED FOR, beyond whatever the system loader already searches. Same five
  sources as the include path and for the same reason (owner, 8 Sep 2026: no hard-coded paths):
  "-p <path>" on the command line · "libpath = ..." in sedai.conf · and the program's own "#libpath".
  ⚠️ This is what makes #libpath mean something at last: the loader's own search path is fixed before
  the process starts, so a directory named at run time can only be honoured by trying it BY HAND. }
procedure AddLibrarySearchPath(const APath: string);
function LibrarySearchPathCount: Integer;

// Opening a user library and finding a symbol: the two things the language surface needs.
function FFILoadLibrary(const AName: string): TLibHandle;
function FFISymbol(ALib: TLibHandle; const AName: string): Pointer;
// ...and the symbols the PROCESS already has - the host executable and everything it is linked
// against. ⛔ Not the same as FFILoadLibrary(''): that asks the loader for a file called "", which
// fails. On Windows this answers nil, because a DLL's exports live in that DLL and there is no
// process-wide symbol namespace to search.
function FFISelfSymbol(const AName: string): Pointer;

implementation

uses
  SedaiConfig;        // where things are: sedai.conf, the environment, the command line

var
  GLibPaths: TStringList = nil;
  GLibPathsFromConfig: Boolean = False;

procedure AddLibrarySearchPath(const APath: string);
begin
  if Trim(APath) = '' then Exit;
  if GLibPaths = nil then
  begin
    GLibPaths := TStringList.Create;
    GLibPaths.CaseSensitive := True;
  end;
  if GLibPaths.IndexOf(IncludeTrailingPathDelimiter(APath)) < 0 then
    GLibPaths.Add(IncludeTrailingPathDelimiter(APath));
end;

function LibrarySearchPathCount: Integer;
begin
  if GLibPaths = nil then Result := 0 else Result := GLibPaths.Count;
end;

procedure PullLibPathsFromConfig;
// Once. The command line has already added its own by the time anything is loaded, and it stays in
// front: ConfigList's entries are APPENDED, so the run outranks the file.
var
  L: TStringList;
  i: Integer;
begin
  if GLibPathsFromConfig then Exit;
  GLibPathsFromConfig := True;
  L := ConfigList('LIBPATH');
  for i := 0 to L.Count - 1 do AddLibrarySearchPath(L[i]);
end;

function FFIAvailable: Boolean;
begin
  Result := AbiAvailable;
end;

function FFIUnavailableReason: string;
begin
  Result := AbiUnavailableReason;
end;

function TryOpen(const ASpelling: string; const APaths: TStringList): TLibHandle;
// One spelling, asked of the loader first (no path: it searches where IT searches - LD_LIBRARY_PATH,
// ld.so.cache, the standard directories) and then of every directory we were told about.
var
  j: Integer;
begin
  Result := LoadLibrary(ASpelling);
  if Result <> NilHandle then Exit;
  if APaths = nil then Exit;
  for j := 0 to APaths.Count - 1 do
  begin
    Result := LoadLibrary(APaths[j] + ASpelling);
    if Result <> NilHandle then Exit;
  end;
end;

function FFILoadLibrary(const AName: string): TLibHandle;
// ⛔⛔ A LIBRARY IS NOT INSTALLED UNDER THE NAME A PROGRAM WRITES. "#inclib "zip"" is what a LINKER
// reads, and a linker resolves it through libzip.so - the DEVELOPMENT symlink, which is in the -dev
// package and is absent on a machine that merely RUNS things. What is actually there is the SONAME:
// libzip.so.5. So the decorated spellings are not enough, and this is the shape that made a program
// with libzip installed report "the symbol was not found".
//
// ⭐ THE VERSIONED SPELLINGS ARE ASKED OF THE LOADER BY NAME, NOT LOOKED FOR IN DIRECTORIES WE NAME.
// "libzip.so.5" with no path sends the loader to ld.so.cache and the standard directories - wherever
// this system keeps them - so no directory is written into this file (owner: no hard-coded paths).
// Counted DOWNWARDS, so the highest version present wins, which is what a linker would have picked.
// ⚠️ The scan only runs when the plain spellings failed, so a library that is properly installed
// costs nothing; a missing one costs a few dozen failed dlopen calls, once.
//
// ...and where a directory IS known (a "-p" or a "libpath ="), the versioned file is found by LOOKING,
// which is exact and needs no guess about the number.
const
  MAX_SONAME_VERSION = 40;   // libffi is at 8, libstdc++ at 6; 40 is slack, and it is only a loop bound
var
  Cands: array[0..3] of string;
  i, v, Best, Dot: Integer;
  Base, Cand, BestName: string;
  Paths: TStringList;
  SR: TSearchRec;
begin
  Result := NilHandle;
  if AName = '' then Exit;
  PullLibPathsFromConfig;
  Paths := GLibPaths;

  // 1. The spellings a program may write, and the decorations a linker would have added.
  Cands[0] := AName;
  {$IFDEF WINDOWS}
  Cands[1] := AName + '.dll';        Cands[2] := 'lib' + AName + '.dll';  Cands[3] := AName;
  {$ELSE}
  Cands[1] := 'lib' + AName + '.so'; Cands[2] := AName + '.so';           Cands[3] := 'lib' + AName;
  {$ENDIF}
  for i := 0 to 3 do
  begin
    Result := TryOpen(Cands[i], Paths);
    if Result <> NilHandle then Exit;
  end;
  // A name the program already wrote WITH a version ("libzip.so.5") is done: it either opened above or
  // it is not there, and appending more numbers to it would be nonsense.
  if Pos('.so.', AName) > 0 then Exit;
  {$IFDEF WINDOWS}
  if Pos('.dll', LowerCase(AName)) > 0 then Exit;
  {$ENDIF}

  // 2. The SONAME. On Windows the same idea wears a different spelling: libffi-8.dll, libpng16-16.dll -
  // and DirectX's own, with an underscore: `#inclib "d3dx9"` is what the FreeBASIC headers ask for,
  // and the import library of that name (libd3dx9.dll.a) points at d3dx9_43.dll. There is no import
  // library here to read, so the numbered name is searched like any other SONAME, newest first.
  {$IFDEF WINDOWS}
  Base := 'lib' + AName + '-';
  for v := MAX_SONAME_VERSION downto 0 do
  begin
    Result := TryOpen(Base + IntToStr(v) + '.dll', Paths);
    if Result <> NilHandle then Exit;
    Result := TryOpen(AName + '-' + IntToStr(v) + '.dll', Paths);
    if Result <> NilHandle then Exit;
    Result := TryOpen(AName + '_' + IntToStr(v) + '.dll', Paths);
    if Result <> NilHandle then Exit;
  end;
  {$ELSE}
  Base := 'lib' + AName + '.so.';
  for v := MAX_SONAME_VERSION downto 0 do
  begin
    Result := TryOpen(Base + IntToStr(v), Paths);
    if Result <> NilHandle then Exit;
  end;
  {$ENDIF}

  // 3. ...and in a directory we were GIVEN, look instead of guessing: the full name may carry a minor
  // ("libzip.so.5.5") that no counted scan would reach.
  if Paths = nil then Exit;
  for i := 0 to Paths.Count - 1 do
  begin
    Best := -1; BestName := '';
    {$IFDEF WINDOWS}
    if FindFirst(Paths[i] + '*' + AName + '*.dll', faAnyFile, SR) = 0 then
    {$ELSE}
    if FindFirst(Paths[i] + 'lib' + AName + '.so.*', faAnyFile, SR) = 0 then
    {$ENDIF}
    begin
      repeat
        // Rank by the FIRST version number, so .so.5.5 beats .so.4.9 and .so.5 ties with .so.5.5 -
        // and among ties the longer (more specific) name is the real file rather than a symlink.
        Cand := SR.Name;
        Dot := Pos('.so.', Cand);
        v := 0;
        if Dot > 0 then v := StrToIntDef(Copy(Cand, Dot + 4, Pos('.', Cand + '.', Dot + 4) - Dot - 4), 0);
        if (v > Best) or ((v = Best) and (Length(Cand) > Length(BestName))) then
        begin Best := v; BestName := Cand; end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
    if BestName <> '' then
    begin
      Result := LoadLibrary(Paths[i] + BestName);
      if Result <> NilHandle then Exit;
    end;
  end;
end;

function FFISymbol(ALib: TLibHandle; const AName: string): Pointer;
begin
  if ALib = NilHandle then Exit(nil);
  Result := GetProcedureAddress(ALib, AName);
end;

function FFISelfSymbol(const AName: string): Pointer;
begin
  // ⭐ NO NEW EXTERNAL FOR THIS on Unix: GetProcedureAddress IS dlsym, and glibc's RTLD_DEFAULT -
  // "look in everything already loaded" - is the NIL handle.
  {$IFDEF WINDOWS}
  Result := nil;
  {$ELSE}
  Result := GetProcedureAddress(NilHandle, AName);
  {$ENDIF}
end;

end.
