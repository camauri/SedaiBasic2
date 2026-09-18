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
  {$IFNDEF WINDOWS}dl,{$ENDIF}   // dlopen with RTLD_GLOBAL, for the members of a linker script (DIVERGENZE 407)
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

function FFILoadLibraryDepth(const AName: string; ADepth: Integer): TLibHandle; forward;

{$IFNDEF WINDOWS}
function OpenLinkerScript(const AErr: string; ADepth: Integer): TLibHandle;
// ⭐⭐ A "lib<name>.so" THAT IS A LINKER SCRIPT, NOT A LIBRARY (DIVERGENZE 407). On Debian libcurses.so is
// a link to libncurses.so, and that file is TEXT: "INPUT(libncurses.so.6 -ltinfo)". ld reads it, so fbc
// links; dlopen refuses it, and the name "curses" has no SONAME of its own to fall back on - so
// "#inclib "curses"" opened NOTHING, every function of ncurses.bi was "not found" and every Extern of it
// read address 0. libc.so and libm.so are scripts too ("GROUP ( /lib/.../libm.so.6 AS_NEEDED ( ... ) )").
// ⭐ NO DIRECTORY IS WRITTEN HERE (owner: no hard-coded paths): the loader already FOUND the file and says
// where in its refusal - "/lib/x86_64-linux-gnu/libcurses.so: file too short" / "...: invalid ELF header".
// The script is read at that path and its members are opened the way ld would take them: "-lX" as the
// library X, a name through the loader's own search, and an archive (".a") skipped - there is nothing to
// load from one at run time. Every member is opened RTLD_GLOBAL, so a symbol that lives in a SIBLING (not
// a dependency of the first) is still found by the process-wide lookup. The first member that opens is
// the answer.
var
  P, Txt, Tok, LastWord, Dir: string;
  SL: TStringList;
  k, i, Level: Integer;
  InList: Boolean;
  H: TLibHandle;
  Toks: TStringList;
  procedure Flush;
  begin
    if Tok <> '' then Toks.Add(Tok);
    Tok := '';
  end;
begin
  Result := NilHandle;
  if ADepth > 4 then Exit;
  k := Pos(': ', AErr);
  if k <= 1 then Exit;
  P := Copy(AErr, 1, k - 1);
  if (P = '') or (P[1] <> '/') or (not FileExists(P)) then Exit;
  SL := TStringList.Create;
  Toks := TStringList.Create;
  try
    try
      SL.LoadFromFile(P);
    except
      Exit;
    end;
    Txt := SL.Text;
    if (Length(Txt) >= 4) and (Copy(Txt, 1, 4) = #127'ELF') then Exit;
    // comments out
    repeat
      k := Pos('/*', Txt);
      if k = 0 then Break;
      i := Pos('*/', Copy(Txt, k + 2, MaxInt));
      if i = 0 then Txt := Copy(Txt, 1, k - 1)
      else Delete(Txt, k, i + 3);
    until False;
    // the arguments of every INPUT( ... ) and GROUP( ... ), nested AS_NEEDED( ... ) included
    // Outside a list only the LAST WORD before a "(" matters: "OUTPUT_FORMAT(elf64-x86-64) GROUP ( ... )".
    Tok := ''; LastWord := ''; Level := 0; InList := False;
    i := 1;
    while i <= Length(Txt) do
    begin
      case Txt[i] of
        '(':
          begin
            if not InList then
            begin
              if Tok <> '' then LastWord := Tok;
              if SameText(LastWord, 'INPUT') or SameText(LastWord, 'GROUP') then
              begin
                InList := True; Level := 1;
              end;
              Tok := ''; LastWord := '';
            end
            else
            begin
              Flush; Inc(Level);          // AS_NEEDED ( ... ): its members count as members
            end;
          end;
        ')':
          begin
            if InList then
            begin
              Flush;
              Dec(Level);
              if Level = 0 then InList := False;
            end;
            Tok := ''; LastWord := '';
          end;
        ' ', #9, #10, #13, ',':
          if InList then Flush
          else if Tok <> '' then begin LastWord := Tok; Tok := ''; end;
      else
        Tok := Tok + Txt[i];
      end;
      Inc(i);
    end;
    Dir := ExtractFilePath(P);
    for i := 0 to Toks.Count - 1 do
    begin
      Tok := Toks[i];
      if (Tok = '') or SameText(Tok, 'AS_NEEDED') then Continue;
      if LowerCase(ExtractFileExt(Tok)) = '.a' then Continue;
      if Copy(Tok, 1, 2) = '-l' then
        H := FFILoadLibraryDepth(Copy(Tok, 3, MaxInt), ADepth + 1)
      else
      begin
        H := TLibHandle(dlopen(PChar(Tok), RTLD_LAZY or RTLD_GLOBAL));
        if (H = NilHandle) and (Tok[1] <> '/') then
          H := TLibHandle(dlopen(PChar(Dir + Tok), RTLD_LAZY or RTLD_GLOBAL));
      end;
      if H = NilHandle then Continue;
      if Result = NilHandle then Result := H;
    end;
  finally
    Toks.Free;
    SL.Free;
  end;
end;
{$ENDIF}

function OpenOne(const AFile: string): TLibHandle;
// ⭐ DIVERGENZE 555 - A LIBRARY IS OPENED INTO THE GLOBAL SCOPE (RTLD_GLOBAL), as an fbc executable has every library it
// links. The symbols of a library loaded earlier then answer the undefined references of one loaded later, in the
// order the program opened them - exactly the link-order interposition an fbc program gets. With FPC's LoadLibrary
// (RTLD_LAZY, local) libGLU resolved glMultMatrixd to libGL's dispatcher even with libOSMesa already loaded, so
// gluPerspective changed nothing: the GL deck, where fbc's executable (linked -lOSMesa before -lGL) did.
begin
  {$IFDEF UNIX}
  Result := TLibHandle(dlopen(PChar(AFile), RTLD_LAZY or RTLD_GLOBAL));
  {$ELSE}
  Result := LoadLibrary(AFile);
  {$ENDIF}
end;

function TryOpen(const ASpelling: string; const APaths: TStringList; ADepth: Integer = 0): TLibHandle;
// One spelling, asked of the loader first (no path: it searches where IT searches - LD_LIBRARY_PATH,
// ld.so.cache, the standard directories) and then of every directory we were told about.
// ...and when the loader found the file and refused it, it may be a LINKER SCRIPT (DIVERGENZE 407).
var
  j: Integer;
begin
  Result := OpenOne(ASpelling);
  if Result <> NilHandle then Exit;
  {$IFNDEF WINDOWS}
  Result := OpenLinkerScript(GetLoadErrorStr, ADepth);
  if Result <> NilHandle then Exit;
  {$ENDIF}
  if APaths = nil then Exit;
  for j := 0 to APaths.Count - 1 do
  begin
    Result := OpenOne(APaths[j] + ASpelling);
    if Result <> NilHandle then Exit;
    {$IFNDEF WINDOWS}
    Result := OpenLinkerScript(GetLoadErrorStr, ADepth);
    if Result <> NilHandle then Exit;
    {$ENDIF}
  end;
end;

function FFILoadLibraryDepth(const AName: string; ADepth: Integer): TLibHandle;
// ADepth counts linker scripts followed into one another ("-ltinfo" inside INPUT(...)): a bound, not a feature.
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
    Result := TryOpen(Cands[i], Paths, ADepth);
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
      Result := OpenOne(Paths[i] + BestName);
      if Result <> NilHandle then Exit;
    end;
  end;
end;

function FFILoadLibrary(const AName: string): TLibHandle;
begin
  Result := FFILoadLibraryDepth(AName, 0);
end;

{$IF DEFINED(LINUX) AND DEFINED(CPU64)}
{ ⭐ A SYMBOL A LIBRARY EXPORTS UNDER A NON-DEFAULT VERSION (DIVERGENZE 475).

  `dlsym` answers, by design, only the DEFAULT version of a name, and glibc exports a handful of
  names with no default at all: `nm -D libc.so.6` prints `pthread_atfork@GLIBC_2.2.5` with ONE `@`,
  and `dlsym(libc, "pthread_atfork")` is nil although the function is right there. `dlvsym` finds it
  - but only if the VERSION STRING is known, and nothing outside the library says what it is.

  So it is read out of the library the loader has already mapped: `dlinfo` hands back the `link_map`,
  whose `l_ld` is the dynamic section, and the symbol, string, version-index and version-definition
  tables are all named in it. The symbol is looked up by walking the hash chains, its version index
  is `versym[i] and $7FFF`, and the matching `Verdef`'s first `Verdaux` names the version.

  ⛔ TWO THINGS THIS PAID FOR IN THE PROTOTYPE, both of them a segfault:
  (a) not every `d_un` is RELOCATED. `DT_SYMTAB` and `DT_STRTAB` came back as run-time addresses and
      `DT_VERDEF` as the link-time one (0x25FF8), so a table pointer below the load bias needs the
      bias added. There is no flag for it: the value being small IS the test.
  (b) the walk must stay inside the dynamic symbol table, which is why it follows the hash chains
      rather than guessing a count.

  🕳️ What this does NOT reach: a name that is in no shared object at all. `atexit` is one -
  it lives only in `libc_nonshared.a`, which fbc links and this VM cannot - and it stays out. }
function VersionedSymbol(ALib: TLibHandle; const AName: string): Pointer;
type
  TElf64Dyn = record d_tag: Int64; d_val: QWord; end;
  PElf64Dyn = ^TElf64Dyn;
  TElf64Sym = record
    st_name: LongWord; st_info, st_other: Byte; st_shndx: Word; st_value, st_size: QWord;
  end;
  PElf64Sym = ^TElf64Sym;
  TElf64Verdef = record
    vd_version, vd_flags, vd_ndx, vd_cnt: Word; vd_hash, vd_aux, vd_next: LongWord;
  end;
  PElf64Verdef = ^TElf64Verdef;
  TElf64Verdaux = record vda_name, vda_next: LongWord; end;
  PElf64Verdaux = ^TElf64Verdaux;
  PWordArr = ^TWordArr;      TWordArr = array[0..0] of Word;
  PDWordArr = ^TDWordArr;    TDWordArr = array[0..0] of LongWord;
const
  DT_HASH = 4; DT_STRTAB = 5; DT_SYMTAB = 6;
  DT_GNU_HASH = QWord($6FFFFEF5); DT_VERSYM = QWord($6FFFFFF0); DT_VERDEF = QWord($6FFFFFFC);
var
  LM: plink_map;
  Dyn: PElf64Dyn;
  Bias: PtrUInt;
  SymTab: PElf64Sym;
  StrTab: PAnsiChar;
  VerSym: PWordArr;
  VerDef: PElf64Verdef;
  GnuHash, SysVHash: PDWordArr;
  NBuck, SymOff, BloomSz, b, i, NSym: LongWord;
  Buckets, Chain: PDWordArr;
  Found: LongInt;
  VIdx: Word;
  VD: PElf64Verdef;
  VA: PElf64Verdaux;
  Ver: string;

  function Fix(P: Pointer): Pointer; inline;
  // A table pointer the loader left at its link-time address needs the load bias; one it already
  // relocated is at or above it. Both spellings occur in the SAME dynamic section.
  begin
    if PtrUInt(P) < Bias then Result := Pointer(PtrUInt(P) + Bias) else Result := P;
  end;

begin
  Result := nil;
  LM := nil;
  if dlinfo(Pointer(ALib), RTLD_DI_LINKMAP, @LM) <> 0 then Exit;
  if LM = nil then Exit;
  Bias := PtrUInt(LM^.l_addr);
  SymTab := nil; StrTab := nil; VerSym := nil; VerDef := nil; GnuHash := nil; SysVHash := nil;
  Dyn := PElf64Dyn(LM^.l_ld);
  if Dyn = nil then Exit;
  while Dyn^.d_tag <> 0 do
  begin
    case QWord(Dyn^.d_tag) of
      DT_SYMTAB:   SymTab   := PElf64Sym(Pointer(PtrUInt(Dyn^.d_val)));
      DT_STRTAB:   StrTab   := PAnsiChar(Pointer(PtrUInt(Dyn^.d_val)));
      DT_VERSYM:   VerSym   := PWordArr(Pointer(PtrUInt(Dyn^.d_val)));
      DT_VERDEF:   VerDef   := PElf64Verdef(Pointer(PtrUInt(Dyn^.d_val)));
      DT_GNU_HASH: GnuHash  := PDWordArr(Pointer(PtrUInt(Dyn^.d_val)));
      DT_HASH:     SysVHash := PDWordArr(Pointer(PtrUInt(Dyn^.d_val)));
    end;
    Inc(Dyn);
  end;
  if (SymTab = nil) or (StrTab = nil) or (VerSym = nil) or (VerDef = nil) then Exit;
  SymTab := Fix(SymTab); StrTab := Fix(StrTab); VerSym := Fix(VerSym); VerDef := Fix(VerDef);
  if GnuHash <> nil then GnuHash := Fix(GnuHash);
  if SysVHash <> nil then SysVHash := Fix(SysVHash);

  Found := -1;
  if GnuHash <> nil then
  begin
    NBuck := GnuHash^[0]; SymOff := GnuHash^[1]; BloomSz := GnuHash^[2];
    Buckets := PDWordArr(PAnsiChar(GnuHash) + 16 + BloomSz * SizeOf(QWord));
    Chain := PDWordArr(PAnsiChar(Buckets) + NBuck * SizeOf(LongWord));
    b := 0;
    while (b < NBuck) and (Found < 0) do
    begin
      i := Buckets^[b];
      if i >= SymOff then
        repeat
          if StrComp(StrTab + SymTab[i].st_name, PAnsiChar(AName)) = 0 then begin Found := i; Break; end;
          if (Chain^[i - SymOff] and 1) <> 0 then Break;
          Inc(i);
        until False;
      Inc(b);
    end;
  end
  else if SysVHash <> nil then
  begin
    NSym := SysVHash^[1];                     // nchain IS the dynamic symbol count
    for i := 0 to NSym - 1 do
      if StrComp(StrTab + SymTab[i].st_name, PAnsiChar(AName)) = 0 then begin Found := i; Break; end;
  end;
  if Found < 0 then Exit;

  VIdx := VerSym^[Found] and $7FFF;
  VD := VerDef;
  Ver := '';
  repeat
    if VD^.vd_ndx = VIdx then
    begin
      VA := PElf64Verdaux(PAnsiChar(VD) + VD^.vd_aux);
      Ver := StrTab + VA^.vda_name;
      Break;
    end;
    if VD^.vd_next = 0 then Break;
    VD := PElf64Verdef(PAnsiChar(VD) + VD^.vd_next);
  until False;
  if Ver = '' then Exit;
  Result := dlvsym(Pointer(ALib), PAnsiChar(AName), PAnsiChar(Ver));
end;
{$ENDIF}

function FFISymbol(ALib: TLibHandle; const AName: string): Pointer;
begin
  if ALib = NilHandle then Exit(nil);
  Result := GetProcedureAddress(ALib, AName);
{$IF DEFINED(LINUX) AND DEFINED(CPU64)}
  // ⛔ dlsym ANSWERS ONLY THE DEFAULT VERSION OF A NAME, and a few libc names have none
  // (DIVERGENZE 475). The versioned lookup runs only where the plain one already failed, so it can
  // add a symbol and never change one.
  if Result = nil then Result := VersionedSymbol(ALib, AName);
{$ENDIF}
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
