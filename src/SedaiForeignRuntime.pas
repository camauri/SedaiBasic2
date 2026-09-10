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
                               out AAvail: PtrUInt): Boolean of object;

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
  end;

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
  NReg: Integer;
  RetAddr: PtrUInt;
  Avail: PtrUInt;
  P: Pointer;
begin
  ResInt := 0; ResFloat := 0;
  if (Idx < 0) or (Idx > High(FEntries)) then
    raise EForeignCallError.CreateFmt('foreign call index %d is outside this program''s table of %d',
                                      [Idx, Length(FEntries)]);
  B := @FEntries[Idx];
  if NArgs > Length(B^.ArgKinds) then NArgs := Length(B^.ArgKinds);
  if NArgs > 64 then
    raise EForeignCallError.CreateFmt('%s: %d arguments is more than this call path carries',
                                      [B^.Decl.Name, NArgs]);
  Prepare(B^);

  SlotI := 0; SlotF := 0; NReg := 0;
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
          if Assigned(FResolvePtr) then
          begin
            P := FResolvePtr(ACtx, XferInt[SlotI]);
            PPointer(Vals[i])^ := P;
            // ⭐ Si ricorda la regione SOLO se e' della VM e indirizzata a byte: e' la condizione che
            // rende sana la traduzione all'indietro del risultato (vedi TForeignPtrRegion). Un
            // argomento che era gia' un indirizzo macchina non si registra - un risultato che cade li'
            // dentro deve restare un indirizzo macchina.
            if (P <> nil) and (NReg <= High(RegBase)) and Assigned(FPtrRegion) and
               ((XferInt[SlotI] and FGNPTR_TAG) = 0) and FPtrRegion(ACtx, XferInt[SlotI], Avail) then
            begin
              RegBase[NReg] := PtrUInt(P);
              RegLen[NReg] := Avail;
              RegVM[NReg] := XferInt[SlotI];
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
          for i := 0 to NReg - 1 do
            if (RetAddr >= RegBase[i]) and (RetAddr - RegBase[i] <= RegLen[i]) then
            begin
              ResInt := RegVM[i] + Int64(RetAddr - RegBase[i]);
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
