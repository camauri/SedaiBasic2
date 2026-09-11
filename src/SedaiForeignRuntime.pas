unit SedaiForeignRuntime;

{$mode objfpc}{$H+}
{$codepage UTF8}

// ⭐ THE RUNTIME HALF OF THE FFI: the table a program's bcForeignCall instructions index into, and the
// marshalling that turns transfer slots into C arguments.
//
// ⛔ THIS IS THE ONLY UNIT OF THE VM SIDE THAT NAMES THE PROVIDER. SedaiBytecodeVM names THIS, and this
// names SedaiFFI - so the count of core units that reach a provider stays at one, and the day the core
// is separated from its providers (owner's decision, 8 Sep 2026) there is one edge to cut, not one per
// call site. ⭐ Since the owner's decision of 8 Sep 2026 there is no external dependency at all: the
// calling convention is ours (SedaiAbi), and an architecture with no trampoline written for it gets a
// named diagnostic at the first foreign call rather than a build that will not run.
//
// A binding is prepared ONCE per table entry and reused: opening the library and preparing the call
// interface cost far more than the call, and a C binding is called in loops. DIVERGENZE 183.

interface

uses
  Classes, SysUtils, dynlibs, SedaiSSATypes, SedaiForeignDecl, SedaiAbi, SedaiFFI;

type
  EForeignCallError = class(Exception);

  { ⭐ HOW A POINTER ARGUMENT BECOMES A MACHINE ADDRESS. The VM owns that answer - only it knows its own
    regions - so it hands one of its methods in rather than this unit learning the pointer encoding.
    The resolver answers nil for a null pointer. }
  TForeignPtrResolver = function(ACtx: TObject; Tagged: Int64): Pointer of object;

  { ⭐⭐ ...AND HOW ONE COMES BACK. Half of the C string library returns a pointer INTO THE BUFFER IT
    WAS GIVEN - strchr, strstr, strrchr, memchr, strpbrk, strtok - so the address that comes back is
    usually an address the VM already owns, only expressed in machine terms. Answering it as a machine
    address (FGNPTR_TAG) is correct but lossy: the program can no longer dereference it through the
    VM's own funnels, and subtracting it from "@buf" - which is how C code finds an INDEX - mixes two
    pointer domains and gives a number with a tag in it.
    ⇒ This asks the VM, for a pointer ARGUMENT, how big the region is and whether it is addressed in
    BYTES; with that, a returned interior pointer is translated back into the VM domain and everything
    downstream keeps working: the dereference goes through the ordinary bounds-checked path, and the
    subtraction answers what fbc answers.
    ⛔ BYTE-ADDRESSED ONLY, and the flag is the whole point. In an Integer array a VM pointer's offset
    counts ELEMENTS, so adding a byte delta to it would name a different element - silently. Answer
    False there and the caller keeps the tag, which is merely lossy instead of wrong. }
  TForeignPtrRegion = function(ACtx: TObject; Tagged: Int64;
                               out AAvail: PtrUInt; out AElemW: Integer): Boolean of object;

  { ⭐⭐ E LA DIREZIONE OPPOSTA: una procedura BASIC che C deve poter CHIAMARE (DIVERGENZE 218).
    `qsort` non riceve dati, riceve un INDIRIZZO su cui salta - e l'indirizzo di una procedura BASIC
    non e' codice macchina, e' un PC di bytecode. Chi sa costruire il ponte e' la VM (solo lei sa
    rientrare nel proprio interprete), quindi passa un proprio metodo, esattamente come per i puntatori.
    Riceve il PC d'ingresso e la firma nella forma "FNPTR:<ret>:<arg,...>", e risponde l'indirizzo
    MACCHINA da consegnare al C. }
  TForeignClosureMaker = function(ACtx: TObject; AEntryPC: Int64;
                                  const ASig: string): Pointer of object;

  { ⭐ Un indirizzo MACCHINA riportato al dominio della VM, o 0 se non e' memoria nostra. Serve per i
    PARAMETRI D'USCITA che portano un puntatore: `strtod(@s, @fine)` non RESTITUISCE il puntatore di
    fine conversione, lo SCRIVE in `fine`, e li' nessuno lo marca (DIVERGENZE 219). ⛔ Non e'
    un'euristica: e' la DICHIARAZIONE a dirlo - quel parametro e' "byte ptr ptr", cioe' punta a un
    puntatore - quindi si guarda solo dove il tipo dice che c'e' un puntatore da guardare. }
  TForeignPtrHome = function(ACtx: TObject; A: PtrUInt): Int64 of object;

  { ⭐ MEMORY C HANDS BACK, AND WHEN IT TAKES IT AWAY (DIVERGENZE 239). The VM dereferences a machine
    address only inside a region recorded here: a pointer a call RETURNED, one C wrote into an
    out-parameter, one a callback received. ALen is the extent when the call itself says it (an
    allocator's size argument), 0 when nothing does. AAdd = False releases the region: C's own free. }
  TForeignRegionNote = procedure(ACtx: TObject; ABase, ALen: PtrUInt; AAdd, AWide: Boolean) of object;
  { AWide: the block is a WSTRING Windows handed back - UTF-16 units that the program counts as cells. }

  { ⭐ THE ADDRESS OF A BASIC RECORD (DIVERGENZE 245). A record keeps a live C image of its numeric
    fields, so C can be handed exactly the bytes it expects - but "@rec" is the record's HANDLE, a small
    integer, and nothing at run time tells it from a MAKEINTRESOURCE number. The call site knows (the
    parameter is written "REC:<type>"), and this answers the machine address of the image and how many
    bytes follow it; nil when the value names no record. A record-FIELD pointer (RECPTR_TAG, negative)
    is answered too, at its field's offset. }
  TForeignRecResolver = function(ACtx: TObject; Value: Int64; out ALen: PtrUInt): Pointer of object;

  { ⭐ THE 8-BYTE CELLS OF A NARROW VALUE (DIVERGENZE 247). A scalar whose address is taken, and an array
    of SINGLE, keep one Int64 / Double per element whatever the declared width - so "sscanf("%d", @n)"
    had C write four bytes into an eight-byte cell, and a "float*" read half a double. This answers the
    run of cells from the pointed element to the end of its storage, and which bank they are in; nil when
    the pointer does not name UNPACKED cells (a packed narrow array, raw memory, C's own memory, a record),
    which are already laid out the way C reads them. }
  TForeignCellResolver = function(ACtx: TObject; Value: Int64; out ACells: PtrUInt;
                                  out AIsFloat: Boolean): Pointer of object;

  TForeignBinding = record
    Decl: TForeignDecl;
    ArgKinds: array of TForeignKind;
    RetKind: TForeignKind;
    Fn: Pointer;
    Prepared: Boolean;
    RetRef: TAbiType;
    ArgRefs: array of TAbiType;
  end;

  { One per running program. Built from the bytecode program's declaration table, in the SAME order:
    the Immediate of a bcForeignCall is an index into it. }
  TForeignTable = class
  private
    FEntries: array of TForeignBinding;
    FLibs: TStringList;          // "#inclib" names, in the order the program gave them
    FOpened: TStringList;        // name -> handle, so a library is opened once
    FResolvePtr: TForeignPtrResolver;
    FPtrRegion: TForeignPtrRegion;      // optional: without it nothing is translated back
    FMakeClosure: TForeignClosureMaker; // optional: senza, un callback resta un PC che C non sa chiamare
    FPtrHome: TForeignPtrHome;          // optional: senza, un parametro d'uscita resta un indirizzo nudo
    FNoteRegion: TForeignRegionNote;    // optional: without it no C memory is ever readable
    FRecBytes: TForeignRecResolver;     // optional: without it a record's address cannot reach C
    FCellRun: TForeignCellResolver;     // optional: without it a narrow value's cells reach C at 8 bytes
    // ⛔⛔ ONE LOCK, AND IT COVERS PREPARATION ONLY. Two threads can reach the same foreign call for the
    // FIRST time at the same moment - a web request handler is exactly that shape - and preparation
    // opens libraries, resolves symbols and builds type descriptors, all of it writing shared state
    // (FOpened is a TStringList, which is not thread-safe at all). The CALL itself is deliberately
    // outside the lock: AbiCall keeps its whole frame in locals, so it is re-entrant, and holding a
    // lock across a call that leaves the process would serialise every database query in the program.
    FPrepLock: TRTLCriticalSection;
    function OpenLib(const AName: string): TLibHandle;
    function ResolveSymbol(var B: TForeignBinding): Pointer;
    procedure Prepare(var B: TForeignBinding);
    procedure PrepareLocked(var B: TForeignBinding);   // the body of Prepare, with FPrepLock held
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddDecl(const ALine: string);
    procedure AddLib(const AName: string);
    function Count: Integer;
    { Call entry Idx. The arguments are read from the transfer banks with the SAME per-bank slot
      numbering the SSA staged them under: the n-th integer parameter is XferInt[n], the n-th float
      parameter is XferFloat[n]. }
    procedure Invoke(Idx: Integer; ACtx: TObject;
                     const XferInt: array of Int64; const XferFloat: array of Double;
                     NArgs: Integer; out ResInt: Int64; out ResFloat: Double);
    property ResolvePtr: TForeignPtrResolver read FResolvePtr write FResolvePtr;
    property PtrRegion: TForeignPtrRegion read FPtrRegion write FPtrRegion;
    property MakeClosure: TForeignClosureMaker read FMakeClosure write FMakeClosure;
    property PtrHome: TForeignPtrHome read FPtrHome write FPtrHome;
    property NoteRegion: TForeignRegionNote read FNoteRegion write FNoteRegion;
    property RecBytes: TForeignRecResolver read FRecBytes write FRecBytes;
    property CellRun: TForeignCellResolver read FCellRun write FCellRun;
  end;

{ La mappa dai nostri tipi a quelli della ABI. ⛔ Esportata perche' chi costruisce una CHIUSURA ha
  bisogno della stessa mappa, e due copie di questa tabella sarebbero due letture dello stesso fatto
  che possono divergere. Risponde nil per un tipo che questo percorso non sa passare. }
function KindToRef(K: TForeignKind): TAbiType;

{ How many bytes are readable from machine address A to the end of the memory MAPPING that holds it; 0
  when A is not in readable memory. It is the extent a block of unknown size can be given (DIVERGENZE
  239): not the block's own size - C does not say it - but the line past which a read would fault. }
function ForeignMappedExtent(A: PtrUInt): PtrUInt;

implementation

{ ⭐ THE CALLS THAT SAY HOW BIG THE MEMORY THEY RETURN IS, and the ones that take it back (DIVERGENZE
  239). Argument indexes, -1 = none. OldP is the block a reallocator releases; FlagsA holds the flags
  of LocalAlloc/GlobalAlloc, whose MOVEABLE bit ($0002) makes the result a HANDLE and not memory.
  ⚠️ A call not listed here still hands back readable memory - of unknown extent, checked for
  provenance only. The list buys the BOUNDS, not the access. }
type
  TFgnAllocRule = record
    Sym: string;
    SizeA, SizeB, OldP, FlagsA: Integer;
  end;
  TFgnFreeRule = record
    Sym: string;
    PtrA: Integer;
  end;

const
  FGN_ALLOC_RULES: array[0..12] of TFgnAllocRule = (
    (Sym: 'malloc';           SizeA: 0; SizeB: -1; OldP: -1; FlagsA: -1),
    (Sym: 'calloc';           SizeA: 0; SizeB:  1; OldP: -1; FlagsA: -1),
    (Sym: 'realloc';          SizeA: 1; SizeB: -1; OldP:  0; FlagsA: -1),
    (Sym: 'aligned_alloc';    SizeA: 1; SizeB: -1; OldP: -1; FlagsA: -1),
    (Sym: 'CoTaskMemAlloc';   SizeA: 0; SizeB: -1; OldP: -1; FlagsA: -1),
    (Sym: 'CoTaskMemRealloc'; SizeA: 1; SizeB: -1; OldP:  0; FlagsA: -1),
    (Sym: 'HeapAlloc';        SizeA: 2; SizeB: -1; OldP: -1; FlagsA: -1),
    (Sym: 'HeapReAlloc';      SizeA: 3; SizeB: -1; OldP:  2; FlagsA: -1),
    (Sym: 'LocalAlloc';       SizeA: 1; SizeB: -1; OldP: -1; FlagsA:  0),
    (Sym: 'LocalReAlloc';     SizeA: 1; SizeB: -1; OldP:  0; FlagsA:  2),
    (Sym: 'GlobalAlloc';      SizeA: 1; SizeB: -1; OldP: -1; FlagsA:  0),
    (Sym: 'GlobalReAlloc';    SizeA: 1; SizeB: -1; OldP:  0; FlagsA:  2),
    (Sym: 'VirtualAlloc';     SizeA: 1; SizeB: -1; OldP: -1; FlagsA: -1));
  FGN_FREE_RULES: array[0..5] of TFgnFreeRule = (
    (Sym: 'free';          PtrA: 0),
    (Sym: 'CoTaskMemFree'; PtrA: 0),
    (Sym: 'HeapFree';      PtrA: 2),
    (Sym: 'LocalFree';     PtrA: 0),
    (Sym: 'GlobalFree';    PtrA: 0),
    (Sym: 'VirtualFree';   PtrA: 0));

function FgnArg(Vals: PPointer; NArgs, K: Integer): PtrUInt;
// The marshalled value of argument K - the buffer is zero-filled, so a narrower one reads right.
begin
  if (K < 0) or (K >= NArgs) or (Vals[K] = nil) then Exit(0);
  Result := PPtrUInt(Vals[K])^;
end;

function FgnAllocLen(const Sym: string; Vals: PPointer; NArgs: Integer): PtrUInt;
// The extent of the block this call returned, when the call says it; 0 otherwise.
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(FGN_ALLOC_RULES) do
    if Sym = FGN_ALLOC_RULES[i].Sym then
    begin
      with FGN_ALLOC_RULES[i] do
      begin
        if (FlagsA >= 0) and ((FgnArg(Vals, NArgs, FlagsA) and 2) <> 0) then Exit(0);   // a HANDLE
        Result := FgnArg(Vals, NArgs, SizeA);
        if SizeB >= 0 then Result := Result * FgnArg(Vals, NArgs, SizeB);
      end;
      Exit;
    end;
end;

procedure FgnNoteReleases(ACtx: TObject; const Sym: string; Vals: PPointer; NArgs: Integer;
  Note: TForeignRegionNote);
// A block C's own free (or a reallocator) has taken back stops being readable.
var
  i: Integer;
  P: PtrUInt;
begin
  for i := 0 to High(FGN_FREE_RULES) do
    if Sym = FGN_FREE_RULES[i].Sym then
    begin
      P := FgnArg(Vals, NArgs, FGN_FREE_RULES[i].PtrA);
      if P <> 0 then Note(ACtx, P, 0, False, False);
      Exit;
    end;
  for i := 0 to High(FGN_ALLOC_RULES) do
    if (Sym = FGN_ALLOC_RULES[i].Sym) and (FGN_ALLOC_RULES[i].OldP >= 0) then
    begin
      P := FgnArg(Vals, NArgs, FGN_ALLOC_RULES[i].OldP);
      if P <> 0 then Note(ACtx, P, 0, False, False);
      Exit;
    end;
end;

{$IFDEF WINDOWS}
{$PUSH}{$PACKRECORDS C}
type
  TFgnMemInfo = record                     // MEMORY_BASIC_INFORMATION, 48 bytes on win64
    BaseAddress, AllocationBase: Pointer;
    AllocationProtect: LongWord;
    PartitionId: Word;
    RegionSize: PtrUInt;
    State, Protect, MemType, Pad: LongWord;
  end;
{$POP}

function FgnVirtualQuery(lpAddress: Pointer; var lpBuffer: TFgnMemInfo; dwLength: PtrUInt): PtrUInt;
  stdcall; external 'kernel32' name 'VirtualQuery';

function ForeignMappedExtent(A: PtrUInt): PtrUInt;
var
  M: TFgnMemInfo;
  Hi: PtrUInt;
begin
  Result := 0;
  FillChar(M, SizeOf(M), 0);
  if FgnVirtualQuery(Pointer(A), M, SizeOf(M)) = 0 then Exit;
  // MEM_COMMIT, and neither PAGE_NOACCESS ($01) nor PAGE_GUARD ($100).
  if (M.State <> $1000) or (M.Protect = 0) or ((M.Protect and $101) <> 0) then Exit;
  Hi := PtrUInt(M.BaseAddress) + M.RegionSize;
  if Hi > A then Result := Hi - A;
end;
{$ELSE}
var
  GMapLo, GMapHi: array of PtrUInt;        // readable mappings of this process, contiguous ones merged
  GMapCount: Integer;
  GMapLock: TRTLCriticalSection;

procedure FgnLoadMaps;
// /proc/self/maps, reread only on a miss: a process maps new memory rarely, and a block C hands back
// almost always lies in a mapping already seen (the heap, a library's data).
var
  F: THandle;
  Buf: array[0..65535] of Char;
  Tmp, S, Line: string;
  n, p, q: Integer;
  Lo, Hi: QWord;
begin
  GMapCount := 0;
  S := '';
  F := FileOpen('/proc/self/maps', fmOpenRead);
  if F = THandle(-1) then Exit;
  try
    repeat
      n := FileRead(F, Buf, SizeOf(Buf));
      if n > 0 then
      begin
        SetString(Tmp, PChar(@Buf[0]), n);
        S := S + Tmp;
      end;
    until n <= 0;
  finally
    FileClose(F);
  end;
  p := 1;
  while p <= Length(S) do
  begin
    q := p;
    while (q <= Length(S)) and (S[q] <> #10) do Inc(q);
    Line := Copy(S, p, q - p);
    p := q + 1;
    // "lo-hi perms ..." in hex; only READABLE mappings count.
    q := Pos('-', Line);
    if (q < 2) or (Pos(' ', Line) < q) then Continue;
    if not TryStrToQWord('$' + Copy(Line, 1, q - 1), Lo) then Continue;
    Line := Copy(Line, q + 1, MaxInt);
    q := Pos(' ', Line);
    if (q < 2) or not TryStrToQWord('$' + Copy(Line, 1, q - 1), Hi) then Continue;
    if (q + 1 > Length(Line)) or (Line[q + 1] <> 'r') then Continue;
    if (GMapCount > 0) and (GMapHi[GMapCount - 1] = PtrUInt(Lo)) then
      GMapHi[GMapCount - 1] := PtrUInt(Hi)
    else
    begin
      if GMapCount = Length(GMapLo) then
      begin
        SetLength(GMapLo, 2 * GMapCount + 64);
        SetLength(GMapHi, Length(GMapLo));
      end;
      GMapLo[GMapCount] := PtrUInt(Lo);
      GMapHi[GMapCount] := PtrUInt(Hi);
      Inc(GMapCount);
    end;
  end;
end;

function ForeignMappedExtent(A: PtrUInt): PtrUInt;

  function Find: PtrUInt;
  var
    lo, hi, mid: Integer;
  begin
    Result := 0;
    lo := 0; hi := GMapCount - 1;
    while lo <= hi do
    begin
      mid := (lo + hi) shr 1;
      if A < GMapLo[mid] then hi := mid - 1
      else if A >= GMapHi[mid] then lo := mid + 1
      else Exit(GMapHi[mid] - A);
    end;
  end;

begin
  EnterCriticalSection(GMapLock);
  try
    Result := Find;
    if Result = 0 then
    begin
      FgnLoadMaps;
      Result := Find;
    end;
  finally
    LeaveCriticalSection(GMapLock);
  end;
end;
{$ENDIF}

constructor TForeignTable.Create;
begin
  inherited Create;
  FLibs := TStringList.Create;
  FOpened := TStringList.Create;
  FOpened.CaseSensitive := True;
  InitCriticalSection(FPrepLock);
end;

destructor TForeignTable.Destroy;
var
  i, j: Integer;
begin
  for i := 0 to High(FEntries) do
  begin
    FEntries[i].RetRef.Free;
    for j := 0 to High(FEntries[i].ArgRefs) do FEntries[i].ArgRefs[j].Free;
  end;
  // ⚠️ The libraries are NOT unloaded. A foreign function may have registered an atexit handler, left
  // a pointer into its own data with the program, or be shared with something else in the process;
  // closing it at program end buys nothing and can crash on the way out.
  FOpened.Free;
  FLibs.Free;
  DoneCriticalSection(FPrepLock);
  inherited Destroy;
end;

procedure TForeignTable.AddDecl(const ALine: string);
var
  n, i: Integer;
begin
  n := Length(FEntries);
  SetLength(FEntries, n + 1);
  FillChar(FEntries[n], SizeOf(FEntries[n]), 0);
  // ⛔ An unparseable line still takes a SLOT. The index is positional: skipping one would shift every
  // later entry, so a call would reach the wrong function - which is the failure this table exists to
  // make impossible.
  if not ParseForeignDecl(ALine, FEntries[n].Decl) then Exit;
  FEntries[n].RetKind := ForeignKindOf(FEntries[n].Decl.RetTypeName);
  SetLength(FEntries[n].ArgKinds, Length(FEntries[n].Decl.ParamTypeNames));
  for i := 0 to High(FEntries[n].Decl.ParamTypeNames) do
    FEntries[n].ArgKinds[i] := ForeignKindOf(FEntries[n].Decl.ParamTypeNames[i]);
end;

procedure TForeignTable.AddLib(const AName: string);
begin
  if (AName <> '') and (FLibs.IndexOf(AName) < 0) then FLibs.Add(AName);
end;

function TForeignTable.Count: Integer;
begin
  Result := Length(FEntries);
end;

function TForeignTable.OpenLib(const AName: string): TLibHandle;
var
  k: Integer;
  H: TLibHandle;
begin
  k := FOpened.IndexOf(AName);
  if k >= 0 then Exit(TLibHandle(PtrUInt(FOpened.Objects[k])));
  H := FFILoadLibrary(AName);
  FOpened.AddObject(AName, TObject(PtrUInt(H)));
  Result := H;
end;

function TForeignTable.ResolveSymbol(var B: TForeignBinding): Pointer;
// Where to look, in order: the library the DECLARATION named, then each "#inclib", then the process
// itself. The last one is what makes a symbol from the host executable or from an already-loaded
// library reachable without naming a file.
var
  i: Integer;
  H: TLibHandle;
  Tried: string;
begin
  Result := nil;
  Tried := '';
  if B.Decl.LibName <> '' then
  begin
    H := OpenLib(B.Decl.LibName);
    if H <> NilHandle then Result := FFISymbol(H, B.Decl.Symbol);
    if Result <> nil then Exit;
    Tried := B.Decl.LibName;
  end;
  for i := 0 to FLibs.Count - 1 do
  begin
    H := OpenLib(FLibs[i]);
    if H <> NilHandle then Result := FFISymbol(H, B.Decl.Symbol);
    if Result <> nil then Exit;
    if Tried <> '' then Tried := Tried + ', ';
    Tried := Tried + FLibs[i];
  end;
  {$IFDEF WINDOWS}
  // ⛔ ON WINDOWS THE C RUNTIME IS A DLL NOBODY NAMES. fbc LINKS msvcrt (and kernel32) into every
  // program, so FreeBASIC's crt/*.bi headers declare qsort, printf, strlen... with no library at all;
  // here nothing is linked, and the process-self lookup below has no RTLD_DEFAULT on Windows - it
  // answers nil. Every CRT function was "not found" under win64 (qsort in the win64 deck). The libraries
  // fbc puts on every link line are tried before giving up, in the same order.
  for i := 0 to 1 do
  begin
    if i = 0 then H := OpenLib('msvcrt') else H := OpenLib('kernel32');
    if H <> NilHandle then Result := FFISymbol(H, B.Decl.Symbol);
    if Result <> nil then Exit;
  end;
  {$ENDIF}
  Result := FFISelfSymbol(B.Decl.Symbol);   // the process's own symbols, and everything it loaded
  if Result <> nil then Exit;
  if Tried = '' then
    raise EForeignCallError.CreateFmt('%s: the symbol "%s" was not found, and no library was named ' +
      '(a "Lib" on the declaration or a "#inclib" says where to look)', [B.Decl.Name, B.Decl.Symbol])
  else
    raise EForeignCallError.CreateFmt('%s: the symbol "%s" was not found in %s',
      [B.Decl.Name, B.Decl.Symbol, Tried]);
end;

function KindToRef(K: TForeignKind): TAbiType;
// ⛔ fkLongDouble and fkUnknown answer nil ON PURPOSE, and the caller raises with the name of the type:
// a kind this path cannot pass must never be quietly turned into something it can.
begin
  case K of
    fkVoid:    Result := TAbiType.CreatePrimitive(akVoid);
    fkS8:      Result := TAbiType.CreatePrimitive(akS8);
    fkU8:      Result := TAbiType.CreatePrimitive(akU8);
    fkS16:     Result := TAbiType.CreatePrimitive(akS16);
    fkU16:     Result := TAbiType.CreatePrimitive(akU16);
    fkS32:     Result := TAbiType.CreatePrimitive(akS32);
    fkU32:     Result := TAbiType.CreatePrimitive(akU32);
    fkS64:     Result := TAbiType.CreatePrimitive(akS64);
    fkU64:     Result := TAbiType.CreatePrimitive(akU64);
    fkFloat:   Result := TAbiType.CreatePrimitive(akFloat);
    fkDouble:  Result := TAbiType.CreatePrimitive(akDouble);
    fkPointer: Result := TAbiType.CreatePrimitive(akPointer);
  else
    Result := nil;
  end;
end;

procedure TForeignTable.Prepare(var B: TForeignBinding);
var
  i: Integer;
begin
  if B.Prepared then Exit;                 // the common case, and it needs no lock: Prepared is set LAST
  EnterCriticalSection(FPrepLock);
  try
    if B.Prepared then Exit;               // ...and re-asked inside, because another thread may have won
    PrepareLocked(B);
  finally
    LeaveCriticalSection(FPrepLock);
  end;
end;

procedure TForeignTable.PrepareLocked(var B: TForeignBinding);
var
  i: Integer;
begin
  if not FFIAvailable then
    raise EForeignCallError.CreateFmt('%s cannot be called: %s', [B.Decl.Name, FFIUnavailableReason]);
  if B.Decl.Name = '' then
    raise EForeignCallError.Create('a foreign call names an entry this program does not carry ' +
      '(a .basc written before the foreign table existed?)');
  B.Fn := ResolveSymbol(B);
  B.RetRef := KindToRef(B.RetKind);
  if B.RetRef = nil then
    raise EForeignCallError.CreateFmt('%s returns %s',
      [B.Decl.Name, ForeignKindRefusalReason(B.RetKind, B.Decl.RetTypeName)]);
  SetLength(B.ArgRefs, Length(B.ArgKinds));
  for i := 0 to High(B.ArgKinds) do
  begin
    B.ArgRefs[i] := KindToRef(B.ArgKinds[i]);
    if B.ArgRefs[i] = nil then
      raise EForeignCallError.CreateFmt('%s: parameter %d - %s',
        [B.Decl.Name, i + 1, ForeignKindRefusalReason(B.ArgKinds[i], B.Decl.ParamTypeNames[i])]);
  end;
  B.Prepared := True;
end;

procedure TForeignTable.Invoke(Idx: Integer; ACtx: TObject; const XferInt: array of Int64;
  const XferFloat: array of Double; NArgs: Integer; out ResInt: Int64; out ResFloat: Double);
// ⛔ ONE BUFFER PER ARGUMENT, AT ITS OWN WIDTH, and a pointer to each: that is the convention the whole
// call path speaks, and it is why the buffers are locals that outlive the call rather than expressions. A "Long" parameter
// given the address of an Int64 would have the callee read four bytes of an eight-byte value - right on
// a little-endian machine for small numbers, and wrong the moment the value does not fit, which is the
// silent kind of wrong.
var
  B: ^TForeignBinding;
  i, SlotI, SlotF: Integer;
  Buf: array[0..63] of array[0..7] of Byte;   // storage for up to 64 arguments
  Vals: array[0..63] of Pointer;
  RetBuf: array[0..15] of Byte;
  // ⭐ Le regioni della VM che questa chiamata ha passato: base MACCHINA, quanti byte, e il puntatore
  // del DOMINIO VM da cui vengono. Servono solo per tradurre all'indietro un puntatore restituito.
  RegBase: array[0..63] of PtrUInt;
  RegLen: array[0..63] of PtrUInt;
  RegVM: array[0..63] of Int64;
  RegW: array[0..63] of Integer;
  OutLoc: array[0..63] of Pointer;     // dove un parametro "T PTR PTR" tiene il suo puntatore
  NOut: Integer;
  // ⭐ The records this call handed over (DIVERGENZE 245): the image's machine address and the VM value
  // it came from. A returned pointer EQUAL to one comes home as that value - D3DXVec3Normalize answers
  // its output argument, and "... = @v" must hold.
  RecBase: array[0..63] of PtrUInt;
  RecVM: array[0..63] of Int64;
  NRec, r: Integer;
  // ⭐ The NARROW values this call handed over (DIVERGENZE 247): each one travels as a copy at C's width,
  // made from the program's 8-byte cells and written back into them after the call.
  NTmp: array[0..63] of array of Byte;   // the copy C sees
  NCell: array[0..63] of Pointer;        // the program's first cell (PInt64 or PDouble)
  NCnt: array[0..63] of PtrUInt;         // how many cells were copied
  NCode: array[0..63] of Integer;        // the width code (1..7)
  NVM: array[0..63] of Int64;            // the VM pointer, to map a returned pointer back
  NN, nwid: Integer;
  nk: PtrUInt;
  NIsF: Boolean;
  NCode1: Integer;
  NReg: Integer;
  RetAddr: PtrUInt;
  Avail: PtrUInt;
  ElemW: Integer;
  P: Pointer;
  {$IFDEF WINDOWS}
  // ⭐ THE PORTABLE WSTRING AT THE WINDOWS BOUNDARY (DIVERGENZE 234, 237). Inside the VM a WSTRING is one
  // 4-byte cell per character on every system; a Windows "...W" function wants UTF-16. Each such argument
  // travels as a UTF-16 COPY - a surrogate pair for a character above U+FFFF - and the copy is decoded
  // back into the program's cells after the call, pairs recombined into one character.
  WTmp: array[0..63] of array of Word;   // the UTF-16 copy of each converted argument
  WCells: array[0..63] of PLongWord;     // the program's own cells it came from
  WN: array[0..63] of PtrUInt;           // how many cells the program's region holds
  WVM: array[0..63] of Int64;            // the VM-domain pointer, to map a returned pointer back
  WElemW: array[0..63] of Integer;       // that region's element width
  NW, j: Integer;
  u, k, UEnd: PtrUInt;
  c: LongWord;
  IsWide: Boolean;
  {$ENDIF}

  {$IFDEF WINDOWS}
  // One level of "WSTRING PTR", as the declaration pass spells LPWSTR / LPCWSTR after resolving the alias.
  function IsWideStrParam(const T: string): Boolean;
  var U: string;
  begin
    U := UpperCase(Trim(T));
    Result := (Pos('WSTRING PTR', U) > 0) and (Pos(' PTR PTR', U) = 0);
  end;
  {$ENDIF}

begin
  ResInt := 0; ResFloat := 0;
  {$IFDEF WINDOWS}
  NW := 0;
  {$ENDIF}
  if (Idx < 0) or (Idx > High(FEntries)) then
    raise EForeignCallError.CreateFmt('foreign call index %d is outside this program''s table of %d',
                                      [Idx, Length(FEntries)]);
  B := @FEntries[Idx];
  if NArgs > Length(B^.ArgKinds) then NArgs := Length(B^.ArgKinds);
  if NArgs > 64 then
    raise EForeignCallError.CreateFmt('%s: %d arguments is more than this call path carries',
                                      [B^.Decl.Name, NArgs]);
  Prepare(B^);

  // ⭐ A DATA SYMBOL, NOT A FUNCTION (DIVERGENZE 253): "extern ffi_type_pointer as ffi_type" names a
  // global variable of the library. The symbol was resolved exactly as a function's is (dlsym through
  // the declared library, the #inclib ones, the process); what the program needs is that ADDRESS - it
  // binds a reference to it - so nothing is called. It is C's memory, tagged and readable inside its
  // mapping like any other address C hands over (239 a).
  if UpperCase(Copy(B^.Decl.RetTypeName, 1, 5)) = 'DATA:' then
  begin
    ResInt := Int64(PtrUInt(B^.Fn));
    if ResInt <> 0 then
    begin
      if Assigned(FNoteRegion) then FNoteRegion(ACtx, PtrUInt(B^.Fn), 0, True, False);
      ResInt := ResInt or FGNPTR_TAG;
    end;
    Exit;
  end;

  SlotI := 0; SlotF := 0; NReg := 0; NOut := 0; NRec := 0; NN := 0;
  FillChar(Buf, SizeOf(Buf), 0);
  for i := 0 to NArgs - 1 do
  begin
    Vals[i] := @Buf[i][0];
    case B^.ArgKinds[i] of
      fkFloat:  begin PSingle(Vals[i])^ := XferFloat[SlotF]; Inc(SlotF); end;
      fkDouble: begin PDouble(Vals[i])^ := XferFloat[SlotF]; Inc(SlotF); end;
      fkS8, fkU8:   begin PByte(Vals[i])^ := Byte(XferInt[SlotI]); Inc(SlotI); end;
      fkS16, fkU16: begin PWord(Vals[i])^ := Word(XferInt[SlotI]); Inc(SlotI); end;
      fkS32, fkU32: begin PLongWord(Vals[i])^ := LongWord(XferInt[SlotI]); Inc(SlotI); end;
      fkPointer:
        begin
          // ⛔ A POINTER ARGUMENT IS NOT AN INTEGER. What the program holds is a VM-domain value - an
          // offset into the byte heap, a packed array pointer, or a machine address a previous foreign
          // call returned - and C wants the third of those. Handing the raw Int64 over passes an OFFSET
          // as an ADDRESS, which is the access violation this cost to find.
          // ⛔ The CONTEXT travels with the call: an array pointer resolves against the executing
          // thread's arrays, so a worker must not be resolved against the main context's.
          // ⭐ UN CALLBACK non e' un puntatore a dati: il valore nel banco intero e' il PC d'ingresso
          // di una procedura BASIC, e cio' che C vuole e' un indirizzo su cui saltare. La voce di
          // questo sito di chiamata lo dice scrivendo il parametro come "FNPTR:..." (DIVERGENZE 218).
          // ⭐ A NARROW VALUE IN 8-BYTE CELLS (DIVERGENZE 247): the call site wrote "W<k>:" because the
          // program declared a LONG / SHORT / SINGLE there, and C reads and writes that width. The cells
          // are copied at C's width, C gets the copy, and the copy goes back after the call. Only when the
          // pointer really names unpacked cells OF THE MATCHING BANK: anything else is already C's layout
          // and keeps the ordinary path below.
          NCode1 := 0;
          if i <= High(B^.Decl.ParamTypeNames) then NCode1 := ForeignNarrowCode(B^.Decl.ParamTypeNames[i]);
          if (NCode1 > 0) and Assigned(FCellRun) and (NN <= High(NTmp)) then
          begin
            P := FCellRun(ACtx, XferInt[SlotI], nk, NIsF);
            if (P <> nil) and (nk > 0) and (NIsF = (NCode1 = 7)) then
            begin
              nwid := ForeignNarrowBytes(NCode1);
              SetLength(NTmp[NN], nk * PtrUInt(nwid) + 8);    // a spare word: C may read one past the end
              FillChar(NTmp[NN][0], Length(NTmp[NN]), 0);
              case NCode1 of
                1, 2: for r := 0 to Integer(nk) - 1 do NTmp[NN][r] := Byte(PInt64(P)[r]);
                3, 4: for r := 0 to Integer(nk) - 1 do PWord(@NTmp[NN][0])[r] := Word(PInt64(P)[r]);
                5, 6: for r := 0 to Integer(nk) - 1 do PLongWord(@NTmp[NN][0])[r] := LongWord(PInt64(P)[r]);
                7:    for r := 0 to Integer(nk) - 1 do PSingle(@NTmp[NN][0])[r] := PDouble(P)[r];
                // ⭐ POINTER cells (DIVERGENZE 253): "Dim args(0 To 0) As ffi_type Ptr = {@ffi_type_pointer}"
                // holds a C address WITH its FGNPTR tag, and C reads the cell as a machine pointer. Each
                // cell is translated exactly as a pointer ARGUMENT is (FResolvePtr), so no cell reaches C
                // that the same value passed on its own would not.
                8:    if Assigned(FResolvePtr) then
                        for r := 0 to Integer(nk) - 1 do
                          PInt64(@NTmp[NN][0])[r] := Int64(PtrUInt(FResolvePtr(ACtx, PInt64(P)[r])));
              end;
              NCell[NN] := P; NCnt[NN] := nk; NCode[NN] := NCode1; NVM[NN] := XferInt[SlotI];
              PPointer(Vals[i])^ := @NTmp[NN][0];
              Inc(NN);
              Inc(SlotI);
              Continue;
            end;
          end;
          if (i <= High(B^.Decl.ParamTypeNames)) and Assigned(FMakeClosure) and
             (UpperCase(Copy(B^.Decl.ParamTypeNames[i], 1, 6)) = 'FNPTR:') then
          // ⛔⛔ NO Inc(SlotI) HERE: the one at the bottom of this arm counts every pointer, callbacks
          // included. A second one made every argument AFTER a callback read the next slot over -
          // invisible to qsort, whose callback is the last argument, and gdi32 LineDDA handed its
          // callback an lParam of 0 (guard m907j, qsort_r).
            PPointer(Vals[i])^ := FMakeClosure(ACtx, XferInt[SlotI], B^.Decl.ParamTypeNames[i])
          // ⭐ The address of a BASIC RECORD (DIVERGENZE 245): the call site marked it, because its value
          // is a record HANDLE that no run-time test can tell from a number. C gets the record's C image.
          // ⛔ A value that names no record is refused aloud: passing the handle on as an address is the
          // access violation this entry was opened for.
          else if Assigned(FRecBytes) and (XferInt[SlotI] <> 0) and
                  ((XferInt[SlotI] and FGNPTR_TAG) = 0) and (i <= High(B^.Decl.ParamTypeNames)) and
                  (UpperCase(Copy(B^.Decl.ParamTypeNames[i], 1, 4)) = 'REC:') then
          begin
            P := FRecBytes(ACtx, XferInt[SlotI], Avail);
            if P = nil then
              raise EForeignCallError.CreateFmt('%s: argument %d is the address of a record that does not exist',
                                                [B^.Decl.Name, i + 1]);
            PPointer(Vals[i])^ := P;
            if NRec <= High(RecBase) then
            begin
              RecBase[NRec] := PtrUInt(P); RecVM[NRec] := XferInt[SlotI]; Inc(NRec);
            end;
          end
          else if Assigned(FResolvePtr) then
          begin
            P := FResolvePtr(ACtx, XferInt[SlotI]);
            PPointer(Vals[i])^ := P;
            // ...and a record-FIELD pointer ("@v.y", negative) resolved to its field: remembered the same way.
            if (P <> nil) and (XferInt[SlotI] < 0) and (NRec <= High(RecBase)) then
            begin
              RecBase[NRec] := PtrUInt(P); RecVM[NRec] := XferInt[SlotI]; Inc(NRec);
            end;
            {$IFDEF WINDOWS}
            // ⭐ A WSTRING PTR argument over the program's own memory becomes a UTF-16 copy. The region
            // says how many cells there are: the copy has room for every one of them - two units for a
            // character above U+FFFF - so a buffer the callee fills to its declared size fits.
            IsWide := False;
            if (P <> nil) and (NW <= 63) and (i <= High(B^.Decl.ParamTypeNames)) and
               IsWideStrParam(B^.Decl.ParamTypeNames[i]) and Assigned(FPtrRegion) and
               ((XferInt[SlotI] and FGNPTR_TAG) = 0) and FPtrRegion(ACtx, XferInt[SlotI], Avail, ElemW) and
               (Avail >= 4) then
            begin
              WCells[NW] := PLongWord(P);
              WN[NW] := Avail div 4;
              WVM[NW] := XferInt[SlotI];
              WElemW[NW] := ElemW;
              SetLength(WTmp[NW], 2 * WN[NW] + 1);
              u := 0;
              for k := 0 to WN[NW] - 1 do
              begin
                c := WCells[NW][k];
                if (c >= $10000) and (c <= $10FFFF) then
                begin
                  WTmp[NW][u] := Word($D800 + ((c - $10000) shr 10));
                  WTmp[NW][u + 1] := Word($DC00 + ((c - $10000) and $3FF));
                  Inc(u, 2);
                end
                else
                begin
                  if c > $10FFFF then c := $FFFD;
                  WTmp[NW][u] := Word(c);
                  Inc(u);
                end;
              end;
              WTmp[NW][u] := 0;                  // the terminator a C wide string ends with
              SetLength(WTmp[NW], u + 1);
              PPointer(Vals[i])^ := @WTmp[NW][0];
              Inc(NW);
              IsWide := True;
            end;
            if IsWide then
            begin
              Inc(SlotI);
              Continue;                           // converted: not an ordinary region, not an out-pointer
            end;
            {$ENDIF}
            // ⭐ Si ricorda la regione SOLO se e' della VM e indirizzata a byte: e' la condizione che
            // rende sana la traduzione all'indietro del risultato (vedi TForeignPtrRegion). Un
            // argomento che era gia' un indirizzo macchina non si registra - un risultato che cade li'
            // dentro deve restare un indirizzo macchina.
            // ⭐ "T PTR PTR": la dichiarazione dice che LI' DENTRO c'e' un puntatore, quindi dopo la
            // chiamata quel valore va riportato a casa (DIVERGENZE 219). Solo dove il tipo lo dice.
            if (P <> nil) and (NOut <= High(OutLoc)) and (i <= High(B^.Decl.ParamTypeNames)) and
               (Pos(' PTR PTR', UpperCase(B^.Decl.ParamTypeNames[i])) > 0) then
            begin
              OutLoc[NOut] := P; Inc(NOut);
            end;
            if (P <> nil) and (NReg <= High(RegBase)) and Assigned(FPtrRegion) and
               ((XferInt[SlotI] and FGNPTR_TAG) = 0) and FPtrRegion(ACtx, XferInt[SlotI], Avail, ElemW) then
            begin
              RegBase[NReg] := PtrUInt(P);
              RegLen[NReg] := Avail;
              RegVM[NReg] := XferInt[SlotI];
              RegW[NReg] := ElemW;
              Inc(NReg);
            end;
          end
          else PInt64(Vals[i])^ := XferInt[SlotI];
          Inc(SlotI);
        end;
    else
      // 64-bit integers.
      begin PInt64(Vals[i])^ := XferInt[SlotI]; Inc(SlotI); end;
    end;
  end;

  FillChar(RetBuf, SizeOf(RetBuf), 0);
  AbiCall(B^.Fn, B^.RetRef, Slice(B^.ArgRefs, NArgs), Slice(Vals, NArgs), @RetBuf[0]);
  if Assigned(FNoteRegion) then FgnNoteReleases(ACtx, B^.Decl.Symbol, @Vals[0], NArgs, FNoteRegion);

  // ...and each narrow copy goes back into the program's cells at the program's width, SIGN-EXTENDED
  // where the declared type is signed: -7 written by "%d" is -7 again, not 4294967289 (DIVERGENZE 247).
  // Cells C did not touch round-trip unchanged: a narrow scalar is stored already narrowed, and a SINGLE
  // already rounded to single precision.
  for nwid := 0 to NN - 1 do
    case NCode[nwid] of
      1: for r := 0 to Integer(NCnt[nwid]) - 1 do PInt64(NCell[nwid])[r] := ShortInt(NTmp[nwid][r]);
      2: for r := 0 to Integer(NCnt[nwid]) - 1 do PInt64(NCell[nwid])[r] := NTmp[nwid][r];
      3: for r := 0 to Integer(NCnt[nwid]) - 1 do PInt64(NCell[nwid])[r] := SmallInt(PWord(@NTmp[nwid][0])[r]);
      4: for r := 0 to Integer(NCnt[nwid]) - 1 do PInt64(NCell[nwid])[r] := PWord(@NTmp[nwid][0])[r];
      5: for r := 0 to Integer(NCnt[nwid]) - 1 do PInt64(NCell[nwid])[r] := LongInt(PLongWord(@NTmp[nwid][0])[r]);
      6: for r := 0 to Integer(NCnt[nwid]) - 1 do PInt64(NCell[nwid])[r] := PLongWord(@NTmp[nwid][0])[r];
      7: for r := 0 to Integer(NCnt[nwid]) - 1 do PDouble(NCell[nwid])[r] := PSingle(@NTmp[nwid][0])[r];
      // ...a pointer cell C left alone keeps the program's own value (its tag, its domain); one C
      // CHANGED now holds a machine address, and becomes one the program can see as such.
      8: if Assigned(FResolvePtr) then
           for r := 0 to Integer(NCnt[nwid]) - 1 do
             if PInt64(@NTmp[nwid][0])[r] <> Int64(PtrUInt(FResolvePtr(ACtx, PInt64(NCell[nwid])[r]))) then
             begin
               if PInt64(@NTmp[nwid][0])[r] = 0 then
                 PInt64(NCell[nwid])[r] := 0
               else
               begin
                 if Assigned(FNoteRegion) then
                   FNoteRegion(ACtx, PtrUInt(PInt64(@NTmp[nwid][0])[r]), 0, True, False);
                 PInt64(NCell[nwid])[r] := PInt64(@NTmp[nwid][0])[r] or FGNPTR_TAG;
               end;
             end;
    end;

  {$IFDEF WINDOWS}
  // ...and each UTF-16 copy goes back into the program's cells: a surrogate pair becomes ONE character,
  // which is the whole difference from fbc's WSTRING on Windows (that one keeps the two units).
  for j := 0 to NW - 1 do
  begin
    u := 0; k := 0;
    UEnd := PtrUInt(Length(WTmp[j]));
    while (k < WN[j]) and (u < UEnd) do
    begin
      c := WTmp[j][u];
      if (c >= $D800) and (c <= $DBFF) and (u + 1 < UEnd) and
         (WTmp[j][u + 1] >= $DC00) and (WTmp[j][u + 1] <= $DFFF) then
      begin
        c := $10000 + ((c - $D800) shl 10) + (LongWord(WTmp[j][u + 1]) - $DC00);
        Inc(u, 2);
      end
      else
        Inc(u);
      WCells[j][k] := c;
      Inc(k);
    end;
  end;
  {$ENDIF}

  // ⛔ I PARAMETRI D'USCITA CHE PORTANO UN PUNTATORE. `strtod(@s, @fine)` non restituisce il puntatore
  // di fine conversione: lo SCRIVE in `fine`. Quello e' un indirizzo macchina, e il programma BASIC
  // che poi fa "*fine" lo legge coi propri opcode - senza questa riconversione moriva (voce 219).
  // ⚠️ Se non e' memoria nostra resta un indirizzo macchina, marcato: lossy, non sbagliato.
  if Assigned(FPtrHome) then
    for i := 0 to NOut - 1 do
      if OutLoc[i] <> nil then
      begin
        RetAddr := PPtrUInt(OutLoc[i])^;
        if RetAddr <> 0 then
        begin
          Avail := 0;
          ResInt := FPtrHome(ACtx, RetAddr);
          if ResInt <> 0 then PInt64(OutLoc[i])^ := ResInt
          else
          begin
            PInt64(OutLoc[i])^ := Int64(RetAddr) or FGNPTR_TAG;
            if Assigned(FNoteRegion) then FNoteRegion(ACtx, RetAddr, 0, True, False);   // DIVERGENZE 239
          end;
        end;
      end;

  // ⛔ A RETURN NARROWER THAN A REGISTER IS READ AT ITS OWN WIDTH AND SIGN. libffi widens an integer
  // return to at least a full word, but the BYTES above the declared width are unspecified padding -
  // reading the whole Int64 makes a "Long" returning -1 answer 4294967295 on one build and -1 on the
  // next, which is the worst kind of difference to chase.
  case B^.RetKind of
    fkVoid:   ;
    fkFloat:  ResFloat := PSingle(@RetBuf[0])^;
    fkDouble: ResFloat := PDouble(@RetBuf[0])^;
    fkS8:     ResInt := PShortInt(@RetBuf[0])^;
    fkU8:     ResInt := PByte(@RetBuf[0])^;
    fkS16:    ResInt := PSmallInt(@RetBuf[0])^;
    fkU16:    ResInt := PWord(@RetBuf[0])^;
    fkS32:    ResInt := PLongInt(@RetBuf[0])^;
    fkU32:    ResInt := PLongWord(@RetBuf[0])^;
    fkPointer:
      begin
        ResInt := PInt64(@RetBuf[0])^;
        // ⭐⭐ UN PUNTATORE DENTRO UN BUFFER CHE ABBIAMO PASSATO NOI TORNA NEL DOMINIO VM. Meta'
        // della libreria C risponde cosi' - strchr, strstr, strrchr, memchr, strpbrk - e rendere
        // quell'indirizzo un puntatore della VM chiude TRE cose in una: `*p` passa dagli imbuti
        // ordinari (quindi CON i controlli di limite), `cast(integer,p) - cast(integer,@buf)` da'
        // l'indice che da' `fbc`, e ripassarlo a un'altra funzione C continua a funzionare.
        // ⛔ Il confronto e' <= sulla FINE della regione: `strchr(s, 0)` risponde il terminatore, e
        // un puntatore alla fine di un buffer e' un puntatore legittimo in C.
        RetAddr := PtrUInt(ResInt);
        if ResInt <> 0 then
        begin
          // ⭐ The address of a record this call handed over comes home as the value it left as - the
          // handle, or the field pointer (DIVERGENZE 245). Only an EXACT match: a record handle has no
          // offset to add, so an address inside the image keeps the machine path below.
          for r := 0 to NRec - 1 do
            if RetAddr = RecBase[r] then
            begin
              ResInt := RecVM[r];
              Exit;
            end;
          // ...and a pointer INTO a narrow copy names an element of the program's own storage: the VM
          // pointer counts ELEMENTS, so the byte delta is divided by C's width (DIVERGENZE 247).
          for r := 0 to NN - 1 do
          begin
            nwid := ForeignNarrowBytes(NCode[r]);
            if (nwid > 0) and (Length(NTmp[r]) > 0) and (RetAddr >= PtrUInt(@NTmp[r][0])) and
               (RetAddr - PtrUInt(@NTmp[r][0]) <= NCnt[r] * PtrUInt(nwid)) and
               (((RetAddr - PtrUInt(@NTmp[r][0])) mod PtrUInt(nwid)) = 0) then
            begin
              ResInt := NVM[r] + Int64((RetAddr - PtrUInt(@NTmp[r][0])) div PtrUInt(nwid));
              Exit;
            end;
          end;
          {$IFDEF WINDOWS}
          // ⭐ A pointer INTO a UTF-16 copy (lstrcpyW answers its destination) names a cell of the
          // program's own WSTRING: count the characters in front of that unit, pairs as one.
          for j := 0 to NW - 1 do
            if (Length(WTmp[j]) > 0) and (RetAddr >= PtrUInt(@WTmp[j][0])) and
               (RetAddr - PtrUInt(@WTmp[j][0]) < PtrUInt(Length(WTmp[j])) * 2) and
               (((RetAddr - PtrUInt(@WTmp[j][0])) mod 2) = 0) and (WElemW[j] > 0) then
            begin
              UEnd := (RetAddr - PtrUInt(@WTmp[j][0])) div 2;
              u := 0; k := 0;
              while u < UEnd do
              begin
                if (WTmp[j][u] >= $D800) and (WTmp[j][u] <= $DBFF) and (u + 1 < UEnd) then Inc(u, 2)
                else Inc(u);
                Inc(k);
              end;
              if ((k * 4) mod PtrUInt(WElemW[j])) = 0 then
              begin
                ResInt := WVM[j] + Int64((k * 4) div PtrUInt(WElemW[j]));
                Exit;
              end;
            end;
          {$ENDIF}
          for i := 0 to NReg - 1 do
            if (RetAddr >= RegBase[i]) and (RetAddr - RegBase[i] <= RegLen[i]) and
               (RegW[i] > 0) and (((RetAddr - RegBase[i]) mod PtrUInt(RegW[i])) = 0) then
            begin
              // ⛔ Diviso per la larghezza d'ELEMENTO: l'offset di un puntatore VM conta gli elementi,
              // non i byte. Una divisione non esatta vuol dire «a meta' di un elemento», e li' non c'e'
              // nessun puntatore VM da rispondere: si tiene l'indirizzo macchina.
              ResInt := RegVM[i] + Int64((RetAddr - RegBase[i]) div PtrUInt(RegW[i]));
              Exit;
            end;
          // Non e' memoria nostra (malloc, una stringa statica dentro la libreria): resta un indirizzo
          // MACCHINA, marcato perche' la chiamata dopo lo riconosca. Un NULL resta 0: "If p = 0" e' il
          // modo in cui ogni binding lo prova. Vedi FGNPTR_TAG.
          // ⭐ ...and readable, inside the region this call handed back (DIVERGENZE 239).
          if Assigned(FNoteRegion) then
            FNoteRegion(ACtx, RetAddr, FgnAllocLen(B^.Decl.Symbol, @Vals[0], NArgs), True,
                        {$IFDEF WINDOWS}IsWideStrParam(B^.Decl.RetTypeName){$ELSE}False{$ENDIF});
          ResInt := ResInt or FGNPTR_TAG;
        end;
      end;
  else
    ResInt := PInt64(@RetBuf[0])^;
  end;
end;

{$IFNDEF WINDOWS}
initialization
  InitCriticalSection(GMapLock);
finalization
  DoneCriticalSection(GMapLock);
{$ENDIF}
end.
