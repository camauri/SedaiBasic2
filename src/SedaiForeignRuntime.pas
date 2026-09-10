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
  end;

{ La mappa dai nostri tipi a quelli della ABI. ⛔ Esportata perche' chi costruisce una CHIUSURA ha
  bisogno della stessa mappa, e due copie di questa tabella sarebbero due letture dello stesso fatto
  che possono divergere. Risponde nil per un tipo che questo percorso non sa passare. }
function KindToRef(K: TForeignKind): TAbiType;

implementation

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

  SlotI := 0; SlotF := 0; NReg := 0; NOut := 0;
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
          if (i <= High(B^.Decl.ParamTypeNames)) and Assigned(FMakeClosure) and
             (UpperCase(Copy(B^.Decl.ParamTypeNames[i], 1, 6)) = 'FNPTR:') then
          begin
            PPointer(Vals[i])^ := FMakeClosure(ACtx, XferInt[SlotI], B^.Decl.ParamTypeNames[i]);
            Inc(SlotI);
          end
          else if Assigned(FResolvePtr) then
          begin
            P := FResolvePtr(ACtx, XferInt[SlotI]);
            PPointer(Vals[i])^ := P;
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
          else PInt64(OutLoc[i])^ := Int64(RetAddr) or FGNPTR_TAG;
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
          ResInt := ResInt or FGNPTR_TAG;
        end;
      end;
  else
    ResInt := PInt64(@RetBuf[0])^;
  end;
end;

end.
