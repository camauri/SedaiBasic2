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

function FFILoadLibrary(const AName: string): TLibHandle;
// The spellings a program may write: "zip", "libzip.so.5", "libzip.so". #inclib names the FIRST, so
// the decorated forms are tried around it - which is what a linker would have done.
var
  Cands: array[0..3] of string;
  i, j: Integer;
begin
  Result := NilHandle;
  if AName = '' then Exit;
  Cands[0] := AName;
  {$IFDEF WINDOWS}
  Cands[1] := AName + '.dll';        Cands[2] := 'lib' + AName + '.dll';  Cands[3] := AName;
  {$ELSE}
  Cands[1] := 'lib' + AName + '.so'; Cands[2] := AName + '.so';           Cands[3] := 'lib' + AName;
  {$ENDIF}
  // First as the loader would see it: an absolute path, or a name it can resolve on its own.
  for i := 0 to 3 do
  begin
    Result := LoadLibrary(Cands[i]);
    if Result <> NilHandle then Exit;
  end;
  // ...then every directory we were TOLD about, each spelling tried inside it.
  PullLibPathsFromConfig;
  if GLibPaths <> nil then
    for j := 0 to GLibPaths.Count - 1 do
      for i := 0 to 3 do
      begin
        Result := LoadLibrary(GLibPaths[j] + Cands[i]);
        if Result <> NilHandle then Exit;
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
