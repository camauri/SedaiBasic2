unit SedaiWasmEmitter;

{ ============================================================================
  SedaiWasmEmitter - WebAssembly binary-format emission primitives.

  The analogue of SedaiX86Emitter for the WASM backend (job/docs/PIANO_WASM.md,
  step 1). Purely mechanical and self-contained: it knows the binary encoding of
  a module and nothing about SSA, so it can be exercised on its own against a
  real validator (node's WebAssembly.validate) before any lowering exists.

  Two types:

    TWasmBuf     a growable byte buffer with the encodings the format needs:
                 LEB128 (unsigned and signed), raw little-endian floats,
                 length-prefixed names, and instruction helpers that pair an
                 opcode with its immediates.

    TWasmModule  assembles the eleven sections in their mandatory order and
                 hands back a finished module. It owns its section buffers;
                 bodies passed to AddFunction are COPIED, so the caller keeps
                 ownership of the TWasmBuf it built them in.

  Index spaces: imports come first. An imported function is func 0, 1, ... and
  the first defined function follows them. Getting that wrong renumbers every
  call in the module silently, so AddFunction refuses to run once it would be
  numbered ahead of an import - see the ImportsClosed guard.

  Written against opcode NAMES, never numbers (PIANO_WASM.md sec.4).
  ============================================================================ }

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

{ ---- value types ---------------------------------------------------------- }

type
  TWasmValType = (wvtI32, wvtI64, wvtF32, wvtF64);
  TWasmValTypeArray = array of TWasmValType;

const
  // Binary encodings of the value types (negative s7 in the spec, stored as bytes).
  WASM_TYPE_I32 = $7F;
  WASM_TYPE_I64 = $7E;
  WASM_TYPE_F32 = $7D;
  WASM_TYPE_F64 = $7C;
  WASM_TYPE_FUNCREF = $70;
  WASM_TYPE_FUNC    = $60;   // the tag that opens a functype
  // Block signature: "no result". A block that yields a value carries the
  // value type byte instead.
  WASM_BLOCKTYPE_EMPTY = $40;

  // External kinds, used by both the import and the export section.
  WASM_KIND_FUNC   = $00;
  WASM_KIND_TABLE  = $01;
  WASM_KIND_MEMORY = $02;
  WASM_KIND_GLOBAL = $03;

  WASM_PAGE_SIZE = 65536;

{ ---- opcodes -------------------------------------------------------------- }

const
  // control
  wopUnreachable   = $00;  wopNop           = $01;  wopBlock         = $02;
  wopLoop          = $03;  wopIf            = $04;  wopElse          = $05;
  wopEnd           = $0B;  wopBr            = $0C;  wopBrIf          = $0D;
  wopBrTable       = $0E;  wopReturn        = $0F;  wopCall          = $10;
  wopCallIndirect  = $11;
  // parametric
  wopDrop          = $1A;  wopSelect        = $1B;
  // variables
  wopLocalGet      = $20;  wopLocalSet      = $21;  wopLocalTee      = $22;
  wopGlobalGet     = $23;  wopGlobalSet     = $24;
  // memory
  wopI32Load       = $28;  wopI64Load       = $29;  wopF32Load       = $2A;
  wopF64Load       = $2B;  wopI32Load8S     = $2C;  wopI32Load8U     = $2D;
  wopI32Load16S    = $2E;  wopI32Load16U    = $2F;  wopI64Load8S     = $30;
  wopI64Load8U     = $31;  wopI64Load16S    = $32;  wopI64Load16U    = $33;
  wopI64Load32S    = $34;  wopI64Load32U    = $35;  wopI32Store      = $36;
  wopI64Store      = $37;  wopF32Store      = $38;  wopF64Store      = $39;
  wopI32Store8     = $3A;  wopI32Store16    = $3B;  wopI64Store8     = $3C;
  wopI64Store16    = $3D;  wopI64Store32    = $3E;  wopMemorySize    = $3F;
  wopMemoryGrow    = $40;
  // constants
  wopI32Const      = $41;  wopI64Const      = $42;  wopF32Const      = $43;
  wopF64Const      = $44;
  // i32 comparison
  wopI32Eqz        = $45;  wopI32Eq         = $46;  wopI32Ne         = $47;
  wopI32LtS        = $48;  wopI32LtU        = $49;  wopI32GtS        = $4A;
  wopI32GtU        = $4B;  wopI32LeS        = $4C;  wopI32LeU        = $4D;
  wopI32GeS        = $4E;  wopI32GeU        = $4F;
  // i64 comparison
  wopI64Eqz        = $50;  wopI64Eq         = $51;  wopI64Ne         = $52;
  wopI64LtS        = $53;  wopI64LtU        = $54;  wopI64GtS        = $55;
  wopI64GtU        = $56;  wopI64LeS        = $57;  wopI64LeU        = $58;
  wopI64GeS        = $59;  wopI64GeU        = $5A;
  // float comparison
  wopF32Eq         = $5B;  wopF32Ne         = $5C;  wopF32Lt         = $5D;
  wopF32Gt         = $5E;  wopF32Le         = $5F;  wopF32Ge         = $60;
  wopF64Eq         = $61;  wopF64Ne         = $62;  wopF64Lt         = $63;
  wopF64Gt         = $64;  wopF64Le         = $65;  wopF64Ge         = $66;
  // i32 arithmetic
  wopI32Clz        = $67;  wopI32Ctz        = $68;  wopI32Popcnt     = $69;
  wopI32Add        = $6A;  wopI32Sub        = $6B;  wopI32Mul        = $6C;
  wopI32DivS       = $6D;  wopI32DivU       = $6E;  wopI32RemS       = $6F;
  wopI32RemU       = $70;  wopI32And        = $71;  wopI32Or         = $72;
  wopI32Xor        = $73;  wopI32Shl        = $74;  wopI32ShrS       = $75;
  wopI32ShrU       = $76;  wopI32Rotl       = $77;  wopI32Rotr       = $78;
  // i64 arithmetic
  wopI64Clz        = $79;  wopI64Ctz        = $7A;  wopI64Popcnt     = $7B;
  wopI64Add        = $7C;  wopI64Sub        = $7D;  wopI64Mul        = $7E;
  wopI64DivS       = $7F;  wopI64DivU       = $80;  wopI64RemS       = $81;
  wopI64RemU       = $82;  wopI64And        = $83;  wopI64Or         = $84;
  wopI64Xor        = $85;  wopI64Shl        = $86;  wopI64ShrS       = $87;
  wopI64ShrU       = $88;  wopI64Rotl       = $89;  wopI64Rotr       = $8A;
  // f32 arithmetic
  wopF32Abs        = $8B;  wopF32Neg        = $8C;  wopF32Ceil       = $8D;
  wopF32Floor      = $8E;  wopF32Trunc      = $8F;  wopF32Nearest    = $90;
  wopF32Sqrt       = $91;  wopF32Add        = $92;  wopF32Sub        = $93;
  wopF32Mul        = $94;  wopF32Div        = $95;  wopF32Min        = $96;
  wopF32Max        = $97;  wopF32Copysign   = $98;
  // f64 arithmetic
  wopF64Abs        = $99;  wopF64Neg        = $9A;  wopF64Ceil       = $9B;
  wopF64Floor      = $9C;  wopF64Trunc      = $9D;  wopF64Nearest    = $9E;
  wopF64Sqrt       = $9F;  wopF64Add        = $A0;  wopF64Sub        = $A1;
  wopF64Mul        = $A2;  wopF64Div        = $A3;  wopF64Min        = $A4;
  wopF64Max        = $A5;  wopF64Copysign   = $A6;
  // conversions
  wopI32WrapI64    = $A7;  wopI32TruncF32S  = $A8;  wopI32TruncF32U  = $A9;
  wopI32TruncF64S  = $AA;  wopI32TruncF64U  = $AB;  wopI64ExtendI32S = $AC;
  wopI64ExtendI32U = $AD;  wopI64TruncF32S  = $AE;  wopI64TruncF32U  = $AF;
  wopI64TruncF64S  = $B0;  wopI64TruncF64U  = $B1;  wopF32ConvertI32S= $B2;
  wopF32ConvertI32U= $B3;  wopF32ConvertI64S= $B4;  wopF32ConvertI64U= $B5;
  wopF32DemoteF64  = $B6;  wopF64ConvertI32S= $B7;  wopF64ConvertI32U= $B8;
  wopF64ConvertI64S= $B9;  wopF64ConvertI64U= $BA;  wopF64PromoteF32 = $BB;
  wopI32ReinterpretF32 = $BC; wopI64ReinterpretF64 = $BD;
  wopF32ReinterpretI32 = $BE; wopF64ReinterpretI64 = $BF;
  // sign extension (post-MVP, universally available)
  wopI32Extend8S   = $C0;  wopI32Extend16S  = $C1;  wopI64Extend8S   = $C2;
  wopI64Extend16S  = $C3;  wopI64Extend32S  = $C4;

  { Saturating float->int truncation: prefix byte, then a u32 sub-opcode.
    The plain wopI32TruncF64S TRAPS on NaN and on out-of-range, which would turn
    a BASIC expression that merely produces a silly number into a dead module -
    these are the forms the lowering will want. }
  wopPrefixFC      = $FC;
  wopfcI32TruncSatF32S = 0;  wopfcI32TruncSatF32U = 1;
  wopfcI32TruncSatF64S = 2;  wopfcI32TruncSatF64U = 3;
  wopfcI64TruncSatF32S = 4;  wopfcI64TruncSatF32U = 5;
  wopfcI64TruncSatF64S = 6;  wopfcI64TruncSatF64U = 7;
  { Bulk memory, same prefix. memory.copy and memory.fill each carry their
    memory index (or two of them) as trailing bytes.
    ⚠️ They do NOT need the data-count section: that one is required only when a
    DATA SEGMENT INDEX appears in the code (memory.init / data.drop), which
    nothing here emits. }
  wopfcMemoryCopy      = 10; wopfcMemoryFill      = 11;

{ ---- byte buffer ---------------------------------------------------------- }

type
  EWasmEmit = class(Exception);

  TWasmBuf = class
  private
    FBuf: array of Byte;
    FLen: Integer;
    procedure Ensure(N: Integer);
  public
    procedure Clear;
    // raw bytes
    procedure U8(B: Byte);
    procedure Raw(const B: array of Byte);
    procedure RawPtr(P: PByte; Count: Integer);
    procedure Append(const Other: TWasmBuf);
    // LEB128
    procedure U32(V: LongWord);
    procedure U64(V: QWord);
    procedure S32(V: LongInt);
    procedure S64(V: Int64);
    // IEEE-754, raw little-endian (NOT LEB-encoded)
    procedure F32(V: Single);
    procedure F64(V: Double);
    // aggregates
    procedure Name(const S: AnsiString);
    procedure ValType(T: TWasmValType);
    procedure Limits(Min, Max: LongWord; HasMax: Boolean);

    { instruction helpers - an opcode plus the immediates it takes }
    procedure Op(Opcode: Byte);
    procedure OpU32(Opcode: Byte; Imm: LongWord);
    procedure OpMem(Opcode: Byte; Align, Offset: LongWord);
    procedure I32Const(V: LongInt);
    procedure I64Const(V: Int64);
    procedure F32Const(V: Single);
    procedure F64Const(V: Double);
    procedure LocalGet(Idx: LongWord);
    procedure LocalSet(Idx: LongWord);
    procedure LocalTee(Idx: LongWord);
    procedure GlobalGet(Idx: LongWord);
    procedure GlobalSet(Idx: LongWord);
    procedure Call(Idx: LongWord);
    procedure CallIndirect(TypeIdx: LongWord);
    procedure BlockStart(Opcode: Byte; ResultType: Byte);   // wopBlock/wopLoop/wopIf
    procedure Br(Depth: LongWord);
    procedure BrIf(Depth: LongWord);
    procedure BrTable(const Targets: array of LongWord; Default: LongWord);
    procedure TruncSat(SubOpcode: LongWord);
    procedure MemoryCopy;                 // dest, src, len already on the stack
    procedure MemoryFill;                 // dest, byte, len already on the stack
    procedure EndOp;

    function Bytes: PByte;
    function AsString: AnsiString;
    property Len: Integer read FLen;
  end;

{ ---- module --------------------------------------------------------------- }

type
  { A section that is a vector: the item bytes plus how many items there are.
    The count has to be written before the items and its LEB width is not known
    until the end, hence the two-buffer shape. }
  TWasmVec = class
  private
    FBuf: TWasmBuf;
    FCount: LongWord;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Item;                       // call after appending one item's bytes
    procedure EmitInto(Dest: TWasmBuf; SectionId: Byte);
    property Buf: TWasmBuf read FBuf;
    property Count: LongWord read FCount;
  end;

  TWasmModule = class
  private
    FTypes: TWasmVec;
    FImports: TWasmVec;
    FFuncs: TWasmVec;        // type index per defined function
    FTables: TWasmVec;
    FMemories: TWasmVec;
    FGlobals: TWasmVec;
    FExports: TWasmVec;
    FElems: TWasmVec;
    FCode: TWasmVec;
    FData: TWasmVec;
    FTypeKeys: array of AnsiString;
    FImportedFuncs, FImportedGlobals, FImportedMemories: LongWord;
    FImportsClosed: Boolean;
    FHasStart: Boolean;
    FStartIdx: LongWord;
    procedure CloseImports;
    procedure ConstOffset(Dest: TWasmBuf; Offset: LongWord);
  public
    constructor Create;
    destructor Destroy; override;

    { types - find-or-add, so callers can ask for a signature without tracking
      indices themselves }
    function TypeIndex(const Params, Results: array of TWasmValType): LongWord;

    { imports - ALL of them must be declared before the first definition of the
      same kind, because imports own the low indices }
    function ImportFunc(const AModule, AField: AnsiString; TypeIdx: LongWord): LongWord;
    function ImportGlobal(const AModule, AField: AnsiString; T: TWasmValType;
                          Mutable: Boolean): LongWord;
    procedure ImportMemory(const AModule, AField: AnsiString; MinPages, MaxPages: LongWord);

    { definitions }
    procedure DefineMemory(MinPages, MaxPages: LongWord);
    procedure DefineTable(MinElems, MaxElems: LongWord);
    function DefineGlobal(T: TWasmValType; Mutable: Boolean;
                          const Init: TWasmBuf): LongWord;
    { Body must be a complete expression WITHOUT its trailing wopEnd - AddFunction
      appends it. Body is copied. }
    function AddFunction(TypeIdx: LongWord; const Locals: array of TWasmValType;
                         const Body: TWasmBuf): LongWord;
    procedure ElemFuncs(Offset: LongWord; const Funcs: array of LongWord);
    procedure DataSegment(Offset: LongWord; P: PByte; Count: Integer);

    { exports }
    procedure ExportFunc(const AName: AnsiString; Idx: LongWord);
    procedure ExportMemory(const AName: AnsiString; Idx: LongWord = 0);
    procedure ExportGlobal(const AName: AnsiString; Idx: LongWord);
    procedure ExportTable(const AName: AnsiString; Idx: LongWord = 0);
    procedure SetStart(Idx: LongWord);

    { output }
    procedure Finish(Dest: TWasmBuf);
    procedure SaveToFile(const Path: string);

    property ImportedFuncCount: LongWord read FImportedFuncs;
  end;

implementation

{ ============================ TWasmBuf ============================ }

procedure TWasmBuf.Ensure(N: Integer);
begin
  if FLen + N > Length(FBuf) then SetLength(FBuf, (FLen + N) * 2 + 64);
end;

procedure TWasmBuf.Clear;
begin
  FLen := 0;
end;

procedure TWasmBuf.U8(B: Byte);
begin
  Ensure(1); FBuf[FLen] := B; Inc(FLen);
end;

procedure TWasmBuf.Raw(const B: array of Byte);
var i: Integer;
begin
  for i := 0 to High(B) do U8(B[i]);
end;

procedure TWasmBuf.RawPtr(P: PByte; Count: Integer);
begin
  if Count <= 0 then Exit;
  Ensure(Count);
  Move(P^, FBuf[FLen], Count);
  Inc(FLen, Count);
end;

procedure TWasmBuf.Append(const Other: TWasmBuf);
begin
  if (Other <> nil) and (Other.Len > 0) then RawPtr(Other.Bytes, Other.Len);
end;

procedure TWasmBuf.U32(V: LongWord);
begin
  U64(V);
end;

procedure TWasmBuf.U64(V: QWord);
var B: Byte;
begin
  repeat
    B := Byte(V and $7F);
    V := V shr 7;
    if V <> 0 then B := B or $80;
    U8(B);
  until V = 0;
end;

procedure TWasmBuf.S32(V: LongInt);
begin
  // Sign-extending to 64 bits and encoding gives the same minimal byte string
  // an s32 encoder would: the terminating group is chosen by the sign bit, not
  // by the declared width.
  S64(V);
end;

procedure TWasmBuf.S64(V: Int64);
const
  SignFill7 = QWord($FE00000000000000);   // the seven bits a shift right vacates
var
  U: QWord;
  B: Byte;
  Negative, Done: Boolean;
begin
  U := QWord(V);
  repeat
    B := Byte(U and $7F);
    Negative := (U and QWord($8000000000000000)) <> 0;
    // Explicit arithmetic shift: 'shr' on a signed Int64 is a LOGICAL shift in
    // FPC, which would encode every negative number wrong.
    U := U shr 7;
    if Negative then U := U or SignFill7;
    Done := ((U = 0) and ((B and $40) = 0)) or
            ((U = QWord($FFFFFFFFFFFFFFFF)) and ((B and $40) <> 0));
    if not Done then B := B or $80;
    U8(B);
  until Done;
end;

procedure TWasmBuf.F32(V: Single);
var
  U: LongWord;
begin
  Move(V, U, 4);
  U8(Byte(U and $FF)); U8(Byte((U shr 8) and $FF));
  U8(Byte((U shr 16) and $FF)); U8(Byte((U shr 24) and $FF));
end;

procedure TWasmBuf.F64(V: Double);
var
  U: QWord;
  i: Integer;
begin
  Move(V, U, 8);
  for i := 0 to 7 do U8(Byte((U shr (i * 8)) and $FF));
end;

procedure TWasmBuf.Name(const S: AnsiString);
begin
  U32(LongWord(Length(S)));
  if Length(S) > 0 then RawPtr(PByte(@S[1]), Length(S));
end;

procedure TWasmBuf.ValType(T: TWasmValType);
begin
  case T of
    wvtI32: U8(WASM_TYPE_I32);
    wvtI64: U8(WASM_TYPE_I64);
    wvtF32: U8(WASM_TYPE_F32);
    wvtF64: U8(WASM_TYPE_F64);
  end;
end;

procedure TWasmBuf.Limits(Min, Max: LongWord; HasMax: Boolean);
begin
  if HasMax then begin U8($01); U32(Min); U32(Max); end
  else begin U8($00); U32(Min); end;
end;

procedure TWasmBuf.Op(Opcode: Byte);
begin U8(Opcode); end;

procedure TWasmBuf.OpU32(Opcode: Byte; Imm: LongWord);
begin U8(Opcode); U32(Imm); end;

procedure TWasmBuf.OpMem(Opcode: Byte; Align, Offset: LongWord);
begin U8(Opcode); U32(Align); U32(Offset); end;

procedure TWasmBuf.I32Const(V: LongInt);
begin U8(wopI32Const); S32(V); end;

procedure TWasmBuf.I64Const(V: Int64);
begin U8(wopI64Const); S64(V); end;

procedure TWasmBuf.F32Const(V: Single);
begin U8(wopF32Const); F32(V); end;

procedure TWasmBuf.F64Const(V: Double);
begin U8(wopF64Const); F64(V); end;

procedure TWasmBuf.LocalGet(Idx: LongWord);  begin OpU32(wopLocalGet, Idx); end;
procedure TWasmBuf.LocalSet(Idx: LongWord);  begin OpU32(wopLocalSet, Idx); end;
procedure TWasmBuf.LocalTee(Idx: LongWord);  begin OpU32(wopLocalTee, Idx); end;
procedure TWasmBuf.GlobalGet(Idx: LongWord); begin OpU32(wopGlobalGet, Idx); end;
procedure TWasmBuf.GlobalSet(Idx: LongWord); begin OpU32(wopGlobalSet, Idx); end;
procedure TWasmBuf.Call(Idx: LongWord);      begin OpU32(wopCall, Idx); end;

procedure TWasmBuf.CallIndirect(TypeIdx: LongWord);
begin
  U8(wopCallIndirect); U32(TypeIdx); U8(0);   // table 0
end;

procedure TWasmBuf.BlockStart(Opcode: Byte; ResultType: Byte);
begin U8(Opcode); U8(ResultType); end;

procedure TWasmBuf.Br(Depth: LongWord);   begin OpU32(wopBr, Depth); end;
procedure TWasmBuf.BrIf(Depth: LongWord); begin OpU32(wopBrIf, Depth); end;

procedure TWasmBuf.BrTable(const Targets: array of LongWord; Default: LongWord);
var i: Integer;
begin
  U8(wopBrTable);
  U32(LongWord(Length(Targets)));
  for i := 0 to High(Targets) do U32(Targets[i]);
  U32(Default);
end;

procedure TWasmBuf.TruncSat(SubOpcode: LongWord);
begin U8(wopPrefixFC); U32(SubOpcode); end;

procedure TWasmBuf.MemoryCopy;
// The two trailing zeroes are the destination and source memory indices; with a
// single memory both are 0.
begin U8(wopPrefixFC); U32(wopfcMemoryCopy); U8(0); U8(0); end;

procedure TWasmBuf.MemoryFill;
begin U8(wopPrefixFC); U32(wopfcMemoryFill); U8(0); end;

procedure TWasmBuf.EndOp;
begin U8(wopEnd); end;

function TWasmBuf.Bytes: PByte;
begin
  if FLen > 0 then Result := @FBuf[0] else Result := nil;
end;

function TWasmBuf.AsString: AnsiString;
begin
  SetLength(Result, FLen);
  if FLen > 0 then Move(FBuf[0], Result[1], FLen);
end;

{ ============================ TWasmVec ============================ }

constructor TWasmVec.Create;
begin
  inherited Create;
  FBuf := TWasmBuf.Create;
  FCount := 0;
end;

destructor TWasmVec.Destroy;
begin
  FBuf.Free;
  inherited Destroy;
end;

procedure TWasmVec.Item;
begin
  Inc(FCount);
end;

procedure TWasmVec.EmitInto(Dest: TWasmBuf; SectionId: Byte);
var
  Header: TWasmBuf;
begin
  if FCount = 0 then Exit;          // an empty section is omitted, not written empty
  Header := TWasmBuf.Create;
  try
    Header.U32(FCount);
    Dest.U8(SectionId);
    Dest.U32(LongWord(Header.Len + FBuf.Len));
    Dest.Append(Header);
    Dest.Append(FBuf);
  finally
    Header.Free;
  end;
end;

{ ============================ TWasmModule ============================ }

constructor TWasmModule.Create;
begin
  inherited Create;
  FTypes := TWasmVec.Create;
  FImports := TWasmVec.Create;
  FFuncs := TWasmVec.Create;
  FTables := TWasmVec.Create;
  FMemories := TWasmVec.Create;
  FGlobals := TWasmVec.Create;
  FExports := TWasmVec.Create;
  FElems := TWasmVec.Create;
  FCode := TWasmVec.Create;
  FData := TWasmVec.Create;
end;

destructor TWasmModule.Destroy;
begin
  FTypes.Free; FImports.Free; FFuncs.Free; FTables.Free; FMemories.Free;
  FGlobals.Free; FExports.Free; FElems.Free; FCode.Free; FData.Free;
  inherited Destroy;
end;

function TWasmModule.TypeIndex(const Params, Results: array of TWasmValType): LongWord;
var
  Sig: TWasmBuf;
  Key: AnsiString;
  i: Integer;
begin
  Sig := TWasmBuf.Create;
  try
    Sig.U8(WASM_TYPE_FUNC);
    Sig.U32(LongWord(Length(Params)));
    for i := 0 to High(Params) do Sig.ValType(Params[i]);
    Sig.U32(LongWord(Length(Results)));
    for i := 0 to High(Results) do Sig.ValType(Results[i]);
    Key := Sig.AsString;
    for i := 0 to High(FTypeKeys) do
      if FTypeKeys[i] = Key then Exit(LongWord(i));
    SetLength(FTypeKeys, Length(FTypeKeys) + 1);
    FTypeKeys[High(FTypeKeys)] := Key;
    FTypes.Buf.Append(Sig);
    FTypes.Item;
    Result := FTypes.Count - 1;
  finally
    Sig.Free;
  end;
end;

procedure TWasmModule.CloseImports;
begin
  FImportsClosed := True;
end;

function TWasmModule.ImportFunc(const AModule, AField: AnsiString;
  TypeIdx: LongWord): LongWord;
begin
  if FImportsClosed then
    raise EWasmEmit.Create('ImportFunc after a definition: imports own the low ' +
      'indices, so this would renumber every call already emitted');
  FImports.Buf.Name(AModule);
  FImports.Buf.Name(AField);
  FImports.Buf.U8(WASM_KIND_FUNC);
  FImports.Buf.U32(TypeIdx);
  FImports.Item;
  Result := FImportedFuncs;
  Inc(FImportedFuncs);
end;

function TWasmModule.ImportGlobal(const AModule, AField: AnsiString;
  T: TWasmValType; Mutable: Boolean): LongWord;
begin
  if FImportsClosed then
    raise EWasmEmit.Create('ImportGlobal after a definition: imports own the low indices');
  FImports.Buf.Name(AModule);
  FImports.Buf.Name(AField);
  FImports.Buf.U8(WASM_KIND_GLOBAL);
  FImports.Buf.ValType(T);
  FImports.Buf.U8(Ord(Mutable));
  FImports.Item;
  Result := FImportedGlobals;
  Inc(FImportedGlobals);
end;

procedure TWasmModule.ImportMemory(const AModule, AField: AnsiString;
  MinPages, MaxPages: LongWord);
begin
  if FImportsClosed then
    raise EWasmEmit.Create('ImportMemory after a definition: imports own the low indices');
  if FImportedMemories + FMemories.Count >= 1 then
    raise EWasmEmit.Create('a module may have only one memory');
  FImports.Buf.Name(AModule);
  FImports.Buf.Name(AField);
  FImports.Buf.U8(WASM_KIND_MEMORY);
  FImports.Buf.Limits(MinPages, MaxPages, MaxPages > 0);
  FImports.Item;
  Inc(FImportedMemories);
end;

procedure TWasmModule.DefineMemory(MinPages, MaxPages: LongWord);
begin
  CloseImports;
  if FImportedMemories + FMemories.Count >= 1 then
    raise EWasmEmit.Create('a module may have only one memory');
  FMemories.Buf.Limits(MinPages, MaxPages, MaxPages > 0);
  FMemories.Item;
end;

procedure TWasmModule.DefineTable(MinElems, MaxElems: LongWord);
begin
  CloseImports;
  FTables.Buf.U8(WASM_TYPE_FUNCREF);
  FTables.Buf.Limits(MinElems, MaxElems, MaxElems > 0);
  FTables.Item;
end;

function TWasmModule.DefineGlobal(T: TWasmValType; Mutable: Boolean;
  const Init: TWasmBuf): LongWord;
begin
  CloseImports;
  FGlobals.Buf.ValType(T);
  FGlobals.Buf.U8(Ord(Mutable));
  FGlobals.Buf.Append(Init);
  FGlobals.Buf.U8(wopEnd);
  FGlobals.Item;
  Result := FImportedGlobals + FGlobals.Count - 1;
end;

function TWasmModule.AddFunction(TypeIdx: LongWord;
  const Locals: array of TWasmValType; const Body: TWasmBuf): LongWord;
var
  Entry, LocalDecl: TWasmBuf;
  i, RunStart, Runs: Integer;
begin
  CloseImports;
  FFuncs.Buf.U32(TypeIdx);
  FFuncs.Item;

  LocalDecl := TWasmBuf.Create;
  Entry := TWasmBuf.Create;
  try
    // locals are declared as runs of (count, type); consecutive equal types merge
    Runs := 0;
    i := 0;
    while i <= High(Locals) do
    begin
      RunStart := i;
      while (i < High(Locals)) and (Locals[i + 1] = Locals[RunStart]) do Inc(i);
      LocalDecl.U32(LongWord(i - RunStart + 1));
      LocalDecl.ValType(Locals[RunStart]);
      Inc(Runs);
      Inc(i);
    end;

    Entry.U32(LongWord(Runs));
    Entry.Append(LocalDecl);
    Entry.Append(Body);
    Entry.U8(wopEnd);

    FCode.Buf.U32(LongWord(Entry.Len));
    FCode.Buf.Append(Entry);
    FCode.Item;
  finally
    Entry.Free;
    LocalDecl.Free;
  end;

  Result := FImportedFuncs + FFuncs.Count - 1;
end;

procedure TWasmModule.ConstOffset(Dest: TWasmBuf; Offset: LongWord);
begin
  Dest.I32Const(LongInt(Offset));
  Dest.U8(wopEnd);
end;

procedure TWasmModule.ElemFuncs(Offset: LongWord; const Funcs: array of LongWord);
var i: Integer;
begin
  CloseImports;
  FElems.Buf.U32(0);                     // table 0, active segment
  ConstOffset(FElems.Buf, Offset);
  FElems.Buf.U32(LongWord(Length(Funcs)));
  for i := 0 to High(Funcs) do FElems.Buf.U32(Funcs[i]);
  FElems.Item;
end;

procedure TWasmModule.DataSegment(Offset: LongWord; P: PByte; Count: Integer);
begin
  CloseImports;
  FData.Buf.U32(0);                      // memory 0, active segment
  ConstOffset(FData.Buf, Offset);
  FData.Buf.U32(LongWord(Count));
  FData.Buf.RawPtr(P, Count);
  FData.Item;
end;

procedure TWasmModule.ExportFunc(const AName: AnsiString; Idx: LongWord);
begin
  FExports.Buf.Name(AName);
  FExports.Buf.U8(WASM_KIND_FUNC);
  FExports.Buf.U32(Idx);
  FExports.Item;
end;

procedure TWasmModule.ExportMemory(const AName: AnsiString; Idx: LongWord);
begin
  FExports.Buf.Name(AName);
  FExports.Buf.U8(WASM_KIND_MEMORY);
  FExports.Buf.U32(Idx);
  FExports.Item;
end;

procedure TWasmModule.ExportGlobal(const AName: AnsiString; Idx: LongWord);
begin
  FExports.Buf.Name(AName);
  FExports.Buf.U8(WASM_KIND_GLOBAL);
  FExports.Buf.U32(Idx);
  FExports.Item;
end;

procedure TWasmModule.ExportTable(const AName: AnsiString; Idx: LongWord);
begin
  FExports.Buf.Name(AName);
  FExports.Buf.U8(WASM_KIND_TABLE);
  FExports.Buf.U32(Idx);
  FExports.Item;
end;

procedure TWasmModule.SetStart(Idx: LongWord);
begin
  FHasStart := True;
  FStartIdx := Idx;
end;

procedure TWasmModule.Finish(Dest: TWasmBuf);
var
  Payload: TWasmBuf;
begin
  Dest.Clear;
  Dest.Raw([$00, $61, $73, $6D]);        // "\0asm"
  Dest.Raw([$01, $00, $00, $00]);        // version 1

  // The order is mandatory, not conventional: a decoder rejects a section that
  // appears after one with a higher id.
  FTypes.EmitInto(Dest, 1);
  FImports.EmitInto(Dest, 2);
  FFuncs.EmitInto(Dest, 3);
  FTables.EmitInto(Dest, 4);
  FMemories.EmitInto(Dest, 5);
  FGlobals.EmitInto(Dest, 6);
  FExports.EmitInto(Dest, 7);
  if FHasStart then
  begin
    // Not a vector section: one index, so build the payload first and let its
    // length be the size.
    Payload := TWasmBuf.Create;
    try
      Payload.U32(FStartIdx);
      Dest.U8(8);
      Dest.U32(LongWord(Payload.Len));
      Dest.Append(Payload);
    finally
      Payload.Free;
    end;
  end;
  FElems.EmitInto(Dest, 9);
  FCode.EmitInto(Dest, 10);
  FData.EmitInto(Dest, 11);
end;

procedure TWasmModule.SaveToFile(const Path: string);
var
  Buf: TWasmBuf;
  F: TFileStream;
begin
  Buf := TWasmBuf.Create;
  try
    Finish(Buf);
    F := TFileStream.Create(Path, fmCreate);
    try
      if Buf.Len > 0 then F.WriteBuffer(Buf.Bytes^, Buf.Len);
    finally
      F.Free;
    end;
  finally
    Buf.Free;
  end;
end;

end.
