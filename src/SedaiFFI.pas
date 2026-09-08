unit SedaiFFI;

{$mode objfpc}{$H+}
{$codepage UTF8}
{$packrecords c}

// ⭐ THE FOREIGN FUNCTION INTERFACE - the provider that lets a program call a C library.
//
// ⛔ WHY IT IS A PROVIDER AND NOT PART OF THE CORE (owner, 8 Sep 2026). "No dependencies for the
// FUNDAMENTAL functions" is a rule about the CORE, not about the product: users must stay free to use
// what they want. And an FFI is not a fundamental function of the language - it is the BOUNDARY. It
// does not do BASIC; it hands the user over to the code they chose. So it may depend on libffi, and
// the core must never name it: this unit is loaded LAZILY, exactly as the SDL2 provider is, and a
// machine without libffi gets a named diagnostic instead of a broken build.
//
// ⛔ WHY libffi RATHER THAN OUR OWN CLASSIFIER. Passing a struct BY VALUE in the SysV AMD64 ABI means
// classifying every field into INTEGER/SSE/MEMORY, merging the classes eightbyte by eightbyte, and
// deciding registers against stack. That is not our knowledge to hold: it is the system's ABI, it
// changes per architecture, and getting it wrong is SILENT - a struct passed badly does not raise, it
// returns wrong numbers. Writing it by hand would put a system ABI inside the core, which is exactly
// what the direction forbids. libffi is the reference implementation of that one problem.
//
// What this unit deliberately does NOT do: it does not decide WHICH library or symbol - that is the
// caller's - and it does not own the memory of the arguments. It marshals and calls.

interface

uses
  SysUtils, dynlibs;

const
  // ffi_type.type codes, from ffi.h. Only the ones a BASIC declaration can name.
  FFI_TYPE_VOID       = 0;
  FFI_TYPE_INT        = 1;
  FFI_TYPE_FLOAT      = 2;
  FFI_TYPE_DOUBLE     = 3;
  FFI_TYPE_UINT8      = 5;
  FFI_TYPE_SINT8      = 6;
  FFI_TYPE_UINT16     = 7;
  FFI_TYPE_SINT16     = 8;
  FFI_TYPE_UINT32     = 9;
  FFI_TYPE_SINT32     = 10;
  FFI_TYPE_UINT64     = 11;
  FFI_TYPE_SINT64     = 12;
  FFI_TYPE_STRUCT     = 13;
  FFI_TYPE_POINTER    = 14;

  FFI_OK              = 0;
  // ⛔ FFI_DEFAULT_ABI is a per-architecture enum, NOT a constant we may guess for every target: on
  // x86-64 SysV it is FFI_UNIX64 = 2, on win64 FFI_WIN64 = 1. Wrong here means the arguments go to the
  // wrong places and nothing complains.
  {$IFDEF WINDOWS}
  FFI_DEFAULT_ABI     = 1;    // FFI_WIN64
  {$ELSE}
  FFI_DEFAULT_ABI     = 2;    // FFI_UNIX64
  {$ENDIF}

type
  // The image of libffi's ffi_type. {$packrecords c} makes the layout the C one; the trailing
  // "elements" is what carries a STRUCT's field list, NULL-terminated.
  PFFIType = ^TFFIType;
  PPFFIType = ^PFFIType;
  TFFIType = record
    size: PtrUInt;
    alignment: Word;
    ftype: Word;
    elements: PPFFIType;
  end;

  // ffi_cif is opaque and its size is a build detail, so it is never declared field by field here:
  // a generous block is handed to ffi_prep_cif, which writes only inside its own struct.
  TFFICif = array[0..255] of Byte;
  PFFICif = ^TFFICif;

  TFFIClosureFun = procedure(cif: PFFICif; ret: Pointer; args: PPointer; user: Pointer); cdecl;

  { A declared foreign type: either one of libffi's primitives, or a STRUCT built from field types. }
  TFFITypeRef = class
  private
    FOwned: PFFIType;                 // non-nil only for a struct we built
    FElems: array of PFFIType;        // the NULL-terminated element array a struct points at
    FRef: PFFIType;
  public
    constructor CreatePrimitive(ARef: PFFIType);
    constructor CreateStruct(const Fields: array of TFFITypeRef);
    destructor Destroy; override;
    property Ref: PFFIType read FRef;
  end;

  { One prepared call: a signature bound to a symbol. Prepare once, call many. }
  TFFICall = class
  private
    FCif: TFFICif;
    FArgTypes: array of PFFIType;
    FFn: Pointer;
    FReady: Boolean;
    FRetType: PFFIType;
  public
    constructor Create(AFn: Pointer; ARet: TFFITypeRef; const AArgs: array of TFFITypeRef);
    // AValues holds one POINTER PER ARGUMENT, each to the argument's storage - libffi's convention,
    // and the reason a caller keeps its own buffers alive across the call.
    procedure Call(const AValues: array of Pointer; ARetBuf: Pointer);
    property Ready: Boolean read FReady;
  end;

  { A BASIC procedure handed to C as a function pointer. The trampoline libffi builds is executable
    memory, so it is allocated and freed by libffi and never by us. }
  TFFIClosure = class
  private
    FCif: TFFICif;
    FArgTypes: array of PFFIType;
    FWritable: Pointer;      // ffi_closure_alloc's block
    FCode: Pointer;          // the address C calls
    FReady: Boolean;
  public
    constructor Create(ARet: TFFITypeRef; const AArgs: array of TFFITypeRef;
                       AHandler: TFFIClosureFun; AUser: Pointer);
    destructor Destroy; override;
    property Code: Pointer read FCode;      // pass THIS to the C function
    property Ready: Boolean read FReady;
  end;

// The provider's own gate: True once libffi answered. Never raises - a machine without it simply has
// no FFI, and the caller says so by name.
function FFIAvailable: Boolean;
function FFIUnavailableReason: string;

// The primitive types, valid only once FFIAvailable is True.
function FFITypeVoid: TFFITypeRef;
function FFITypeSInt8: TFFITypeRef;
function FFITypeUInt8: TFFITypeRef;
function FFITypeSInt16: TFFITypeRef;
function FFITypeUInt16: TFFITypeRef;
function FFITypeSInt32: TFFITypeRef;
function FFITypeUInt32: TFFITypeRef;
function FFITypeSInt64: TFFITypeRef;
function FFITypeUInt64: TFFITypeRef;
function FFITypeFloat: TFFITypeRef;
function FFITypeDouble: TFFITypeRef;
function FFITypePointer: TFFITypeRef;

// Opening a user library and finding a symbol: the two things the language surface needs.
function FFILoadLibrary(const AName: string): TLibHandle;
function FFISymbol(ALib: TLibHandle; const AName: string): Pointer;

implementation

var
  GFFI: TLibHandle = NilHandle;
  GReason: string = 'the FFI has not been asked for yet';
  GTried: Boolean = False;

  ffi_prep_cif: function(cif: PFFICif; abi: LongInt; nargs: LongWord;
                         rtype: PFFIType; atypes: PPFFIType): LongInt; cdecl = nil;
  ffi_call_: procedure(cif: PFFICif; fn: Pointer; rvalue: Pointer; avalue: PPointer); cdecl = nil;
  ffi_closure_alloc: function(size: PtrUInt; code: PPointer): Pointer; cdecl = nil;
  ffi_prep_closure_loc: function(closure: Pointer; cif: PFFICif; fun: TFFIClosureFun;
                                 user: Pointer; codeloc: Pointer): LongInt; cdecl = nil;
  ffi_closure_free: procedure(closure: Pointer); cdecl = nil;

  // libffi exports its primitives as DATA symbols, not functions.
  pffi_void, pffi_s8, pffi_u8, pffi_s16, pffi_u16, pffi_s32, pffi_u32,
  pffi_s64, pffi_u64, pffi_flt, pffi_dbl, pffi_ptr: PFFIType;

  GRefs: array of TFFITypeRef;    // the primitive wrappers, freed at finalization

function LoadFFI: Boolean;
const
  Candidates: array[0..3] of string = ('libffi.so.8', 'libffi.so.7', 'libffi.so.6', 'libffi.so');
var
  i: Integer;
begin
  Result := GFFI <> NilHandle;
  if Result or GTried then Exit(GFFI <> NilHandle);
  GTried := True;
  for i := Low(Candidates) to High(Candidates) do
  begin
    {$IFDEF WINDOWS}
    GFFI := LoadLibrary('libffi-8.dll');
    {$ELSE}
    GFFI := LoadLibrary(Candidates[i]);
    {$ENDIF}
    if GFFI <> NilHandle then Break;
  end;
  if GFFI = NilHandle then
  begin
    GReason := 'libffi is not installed (tried libffi.so.8, .7, .6 and libffi.so)';
    Exit(False);
  end;
  Pointer(ffi_prep_cif)        := GetProcedureAddress(GFFI, 'ffi_prep_cif');
  Pointer(ffi_call_)           := GetProcedureAddress(GFFI, 'ffi_call');
  Pointer(ffi_closure_alloc)   := GetProcedureAddress(GFFI, 'ffi_closure_alloc');
  Pointer(ffi_prep_closure_loc):= GetProcedureAddress(GFFI, 'ffi_prep_closure_loc');
  Pointer(ffi_closure_free)    := GetProcedureAddress(GFFI, 'ffi_closure_free');
  pffi_void := GetProcedureAddress(GFFI, 'ffi_type_void');
  pffi_s8   := GetProcedureAddress(GFFI, 'ffi_type_sint8');
  pffi_u8   := GetProcedureAddress(GFFI, 'ffi_type_uint8');
  pffi_s16  := GetProcedureAddress(GFFI, 'ffi_type_sint16');
  pffi_u16  := GetProcedureAddress(GFFI, 'ffi_type_uint16');
  pffi_s32  := GetProcedureAddress(GFFI, 'ffi_type_sint32');
  pffi_u32  := GetProcedureAddress(GFFI, 'ffi_type_uint32');
  pffi_s64  := GetProcedureAddress(GFFI, 'ffi_type_sint64');
  pffi_u64  := GetProcedureAddress(GFFI, 'ffi_type_uint64');
  pffi_flt  := GetProcedureAddress(GFFI, 'ffi_type_float');
  pffi_dbl  := GetProcedureAddress(GFFI, 'ffi_type_double');
  pffi_ptr  := GetProcedureAddress(GFFI, 'ffi_type_pointer');
  // ⛔ A partial load is worse than none: a nil ffi_call would be a jump to zero at the first call,
  // far from here. Everything or nothing, and the reason names what was missing.
  if (ffi_prep_cif = nil) or (ffi_call_ = nil) or (pffi_ptr = nil) or (pffi_s64 = nil) then
  begin
    UnloadLibrary(GFFI); GFFI := NilHandle;
    GReason := 'libffi was found but does not export ffi_prep_cif / ffi_call / its primitive types';
    Exit(False);
  end;
  GReason := '';
  Result := True;
end;

function FFIAvailable: Boolean;
begin
  Result := LoadFFI;
end;

function FFIUnavailableReason: string;
begin
  if LoadFFI then Result := '' else Result := GReason;
end;

{ TFFITypeRef }

constructor TFFITypeRef.CreatePrimitive(ARef: PFFIType);
begin
  inherited Create;
  FOwned := nil;
  FRef := ARef;
end;

constructor TFFITypeRef.CreateStruct(const Fields: array of TFFITypeRef);
// A struct BY VALUE: libffi is handed the field types and computes size, alignment and the SysV
// classification itself. size and alignment are left ZERO on purpose - ffi_prep_cif fills them, and
// filling them here is the classic way to get a silently wrong layout.
var
  i: Integer;
begin
  inherited Create;
  New(FOwned);
  FillChar(FOwned^, SizeOf(TFFIType), 0);
  SetLength(FElems, Length(Fields) + 1);
  for i := 0 to High(Fields) do FElems[i] := Fields[i].Ref;
  FElems[High(FElems)] := nil;                    // NULL terminator, as libffi expects
  FOwned^.size := 0;
  FOwned^.alignment := 0;
  FOwned^.ftype := FFI_TYPE_STRUCT;
  FOwned^.elements := @FElems[0];
  FRef := FOwned;
end;

destructor TFFITypeRef.Destroy;
begin
  if FOwned <> nil then Dispose(FOwned);
  inherited Destroy;
end;

{ TFFICall }

constructor TFFICall.Create(AFn: Pointer; ARet: TFFITypeRef; const AArgs: array of TFFITypeRef);
var
  i: Integer;
  P: PPFFIType;
begin
  inherited Create;
  FReady := False;
  FFn := AFn;
  if (not LoadFFI) or (AFn = nil) or (ARet = nil) then Exit;
  FRetType := ARet.Ref;
  SetLength(FArgTypes, Length(AArgs));
  for i := 0 to High(AArgs) do FArgTypes[i] := AArgs[i].Ref;
  FillChar(FCif, SizeOf(FCif), 0);
  if Length(FArgTypes) > 0 then P := @FArgTypes[0] else P := nil;
  FReady := ffi_prep_cif(@FCif, FFI_DEFAULT_ABI, Length(FArgTypes), FRetType, P) = FFI_OK;
end;

procedure TFFICall.Call(const AValues: array of Pointer; ARetBuf: Pointer);
var
  P: PPointer;
  Tmp: array[0..15] of Byte;
begin
  if not FReady then Exit;
  if Length(AValues) > 0 then P := @AValues[0] else P := nil;
  // ⛔ libffi always writes at least one machine word into the return buffer, even for a type
  // narrower than that: a caller who hands it a 4-byte buffer gets its neighbours overwritten. A
  // scratch of a full eightbyte pair covers every primitive, and a struct return is the caller's.
  if ARetBuf <> nil then ffi_call_(@FCif, FFn, ARetBuf, P)
  else ffi_call_(@FCif, FFn, @Tmp[0], P);
end;

{ TFFIClosure }

constructor TFFIClosure.Create(ARet: TFFITypeRef; const AArgs: array of TFFITypeRef;
                               AHandler: TFFIClosureFun; AUser: Pointer);
var
  i: Integer;
  P: PPFFIType;
begin
  inherited Create;
  FReady := False;
  FWritable := nil;
  FCode := nil;
  if (not LoadFFI) or (ffi_closure_alloc = nil) or (ffi_prep_closure_loc = nil) then Exit;
  SetLength(FArgTypes, Length(AArgs));
  for i := 0 to High(AArgs) do FArgTypes[i] := AArgs[i].Ref;
  FillChar(FCif, SizeOf(FCif), 0);
  if Length(FArgTypes) > 0 then P := @FArgTypes[0] else P := nil;
  if ffi_prep_cif(@FCif, FFI_DEFAULT_ABI, Length(FArgTypes), ARet.Ref, P) <> FFI_OK then Exit;
  // ⛔ The trampoline is EXECUTABLE memory, and on a W^X system the writable address and the
  // executable one are DIFFERENT: ffi_closure_alloc returns the first and writes the second into
  // codeloc. C must be given the second; freeing takes the first.
  FWritable := ffi_closure_alloc(4096, @FCode);
  if FWritable = nil then Exit;
  FReady := ffi_prep_closure_loc(FWritable, @FCif, AHandler, AUser, FCode) = FFI_OK;
end;

destructor TFFIClosure.Destroy;
begin
  if (FWritable <> nil) and (ffi_closure_free <> nil) then ffi_closure_free(FWritable);
  inherited Destroy;
end;

function MakeRef(P: PFFIType): TFFITypeRef;
begin
  Result := TFFITypeRef.CreatePrimitive(P);
  SetLength(GRefs, Length(GRefs) + 1);
  GRefs[High(GRefs)] := Result;
end;

var
  GPrim: array[0..11] of TFFITypeRef;

function Prim(Idx: Integer; P: PFFIType): TFFITypeRef;
begin
  if not LoadFFI then Exit(nil);
  if GPrim[Idx] = nil then GPrim[Idx] := MakeRef(P);
  Result := GPrim[Idx];
end;

function FFITypeVoid: TFFITypeRef;    begin Result := Prim(0,  pffi_void); end;
function FFITypeSInt8: TFFITypeRef;   begin Result := Prim(1,  pffi_s8);   end;
function FFITypeUInt8: TFFITypeRef;   begin Result := Prim(2,  pffi_u8);   end;
function FFITypeSInt16: TFFITypeRef;  begin Result := Prim(3,  pffi_s16);  end;
function FFITypeUInt16: TFFITypeRef;  begin Result := Prim(4,  pffi_u16);  end;
function FFITypeSInt32: TFFITypeRef;  begin Result := Prim(5,  pffi_s32);  end;
function FFITypeUInt32: TFFITypeRef;  begin Result := Prim(6,  pffi_u32);  end;
function FFITypeSInt64: TFFITypeRef;  begin Result := Prim(7,  pffi_s64);  end;
function FFITypeUInt64: TFFITypeRef;  begin Result := Prim(8,  pffi_u64);  end;
function FFITypeFloat: TFFITypeRef;   begin Result := Prim(9,  pffi_flt);  end;
function FFITypeDouble: TFFITypeRef;  begin Result := Prim(10, pffi_dbl);  end;
function FFITypePointer: TFFITypeRef; begin Result := Prim(11, pffi_ptr);  end;

function FFILoadLibrary(const AName: string): TLibHandle;
// The spellings a program may write: "zip", "libzip.so.5", "libzip.so". #inclib names the FIRST, so
// the decorated forms are tried around it - which is what a linker would have done.
var
  Cands: array[0..3] of string;
  i: Integer;
begin
  Result := NilHandle;
  if AName = '' then Exit;
  Cands[0] := AName;
  {$IFDEF WINDOWS}
  Cands[1] := AName + '.dll';       Cands[2] := 'lib' + AName + '.dll';  Cands[3] := AName;
  {$ELSE}
  Cands[1] := 'lib' + AName + '.so'; Cands[2] := AName + '.so';          Cands[3] := 'lib' + AName;
  {$ENDIF}
  for i := 0 to 3 do
  begin
    Result := LoadLibrary(Cands[i]);
    if Result <> NilHandle then Exit;
  end;
end;

function FFISymbol(ALib: TLibHandle; const AName: string): Pointer;
begin
  if ALib = NilHandle then Exit(nil);
  Result := GetProcedureAddress(ALib, AName);
end;

var
  i: Integer;

initialization
  for i := 0 to High(GPrim) do GPrim[i] := nil;

finalization
  for i := 0 to High(GRefs) do GRefs[i].Free;
  SetLength(GRefs, 0);
end.
