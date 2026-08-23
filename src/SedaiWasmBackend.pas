unit SedaiWasmBackend;

{ ============================================================================
  SedaiWasmBackend - SSA -> WebAssembly module.

  Step 3 of job/docs/PIANO_WASM.md, following the mapping measured in sec.5-bis
  of that plan. It runs where the bytecode compiler runs: after PHI elimination
  and register allocation, on the same TSSAProgram.

  The four decisions it implements, all of them measured rather than assumed:

  1. Banks map to types. srtInt -> i64 (our integers ARE Int64; i32 would
     change the semantics), srtFloat -> f64, srtString -> i32, which is a
     HANDLE and not the string - the string runtime is class C and lives in
     linear memory.
  2. Registers become LOCALS, not slots in linear memory. Measured: the largest
     program in the corpus needs 530 locals in total, which an engine carries
     without noticing. In linear memory every access would be a load and a
     store that the browser's JIT cannot keep in a machine register - the
     interpreter again, minus the dispatch.
  3. One WASM function per BASIC procedure, so recursion runs on the ENGINE's
     call stack. Natively that is the frame protocol, the standing performance
     lead; here it costs nothing. This is only practical because the calling
     convention already exists: the caller stores into slot k of bank b
     (ssaXferStore*), the callee loads from the same slot (ssaXferLoad*), and
     the two agree because both ask ParamBankAndSlot over the same parameter
     list. The convention IS a signature - it is read, not invented.
  4. A register used by more than one region becomes a WASM GLOBAL, and the
     count is reported rather than buried.

  And the rule that shapes every refusal: there is no deopt. In the browser
  there is no interpreter to fall back into, so an opcode this backend does not
  cover must make the compilation FAIL WITH A MESSAGE, never emit something
  that runs and lies.
  ============================================================================ }

{$mode objfpc}{$H+}
{$codepage UTF8}   // CP_UTF8 literals, like every other unit. A string that crosses a codepage
                   // boundary is converted and a comparison stops being a memcmp - see SedaiFileIO.

interface

uses
  SysUtils, Classes, TypInfo, SedaiSSATypes, SedaiWasmEmitter, SedaiWasmControl,
  SedaiExecutorErrors;   // ERR$ builds its table by asking the interpreter's own function

type
  { Where a register lives in the emitted function. }
  TWasmRegLoc = record
    IsGlobal: Boolean;
    Index: LongWord;
  end;

  TSSAValueArray = array of TSSAValue;

  TWasmBackend = class
  private
    FProg: TSSAProgram;
    FModern: Boolean;
    FModule: TWasmModule;
    FError: string;
    FGlobalCount: Integer;

    // --- partition ------------------------------------------------------
    FRegionOf: array of Integer;            // block index -> region
    FRegionFirst, FRegionLast: array of Integer;
    FRegionName: array of string;
    FRegionCount: Integer;

    // --- per (bank, register index), addressed by a flat id -------------
    FMaxReg: array[TSSARegisterType] of Integer;
    FUseRegion: array[TSSARegisterType] of array of Integer;  // 0 none, r+1 one, -1 many
    FIsGlobal: array of Boolean;            // flat id -> lives in a WASM global
    { ⭐ Flat ids whose value is ALWAYS a 32-bit integer, so their local is an i32 rather than an i64.
      A register qualifies when EVERY definition of it is a 32-bit narrowing of the same signedness -
      which is precisely what INT32 / UINT32 arithmetic emits, and what a store to a LONG / ULONG
      emits. Anything else (a phi, a call result, a mixed signedness) is left alone.
      ⛔ Correctness does not depend on the classification: LoadReg widens an i32 local back to i64 and
      StoreReg wraps, so every consumer still sees exactly what it saw before. The classification only
      decides where the wrap/extend PAIR can be avoided - which is the whole saving, and why a wrong
      answer here costs bytes and never meaning. }
    FIs32: array of Boolean;                // flat id -> its local is an i32
    FIs32Signed: array of Boolean;          // ...and how it widens back (extend_s vs extend_u)
    FGlobalIdx: array of LongWord;
    FRegionUses: array of array of Boolean; // region -> flat id
    FLocalIdx: array of array of LongWord;  // region -> flat id -> local index
    FCurRegion: Integer;

    // --- per region -----------------------------------------------------
    FParamCount: array of array[TSSARegisterType] of Integer;
    FResultBank: array of Integer;          // -1 none, else Ord(TSSARegisterType)
    FTypeIdx: array of LongWord;
    FFuncIdx: array of LongWord;
    FCalls: array of array of Integer;      // region -> regions it calls
    FRecursive: array of Boolean;
    FUsesGlobal: array of Boolean;

    // --- transfer slots -------------------------------------------------
    { The transfer bank is modelled as one local per (bank, slot), not as
      staging that only exists at a call site. SUB inlining maps a callee's
      registers into its caller and leaves the Store/Load pair BEHIND, inside a
      single region - so a load of slot 0 is not always "read parameter 0", and
      treating it that way read the dispatch state instead. As slot locals both
      spellings mean the same thing, and a real call just copies its parameters
      into them on entry. }
    FSlotCount: array of array[TSSARegisterType] of Integer;
    { Argument slots a region writes BACK for its caller - BYREF copy-out. They
      leave as extra RESULTS (multi-value), which is recursion-safe by
      construction: a shared area in linear memory would be clobbered by the
      callee's own nested calls, exactly what the VM avoids by saving the
      transfer bank per frame. }
    FOutSlot: array of array[TSSARegisterType] of array of Boolean;
    { ⭐ THE HIGH SLOTS ARE NOT ARGUMENTS AND NOT PER FUNCTION. Slots from
      SHARED_SLOT_BASE up carry module-global SHARED scalars, the reserved
      END-in-procedure destructor handles (growing down from 253) and the
      caller-allocated handle of a FUNCTION returning a UDT by value (254). All
      three name ONE storage location for the whole program - which is exactly
      what the VM's transfer bank IS: FCtx.XferInt/Float/Str are sized once, to
      256 (SedaiBytecodeVM.pas), and a call never saves or restores them. So a
      WASM GLOBAL is the FAITHFUL mapping and not an approximation, one per
      (bank, slot); a slot LOCAL would give every procedure its own copy of a
      module global and compute the wrong thing in silence.
      ⚠️ The LOW slots stay locals, and that is the optimisation rather than the
      rule: an argument is written by the caller immediately before the call and
      read by the callee immediately after, so nothing can observe the
      difference - byref copy-out included, which leaves as extra results. }
    FXferGlobal: array[TSSARegisterType] of array of LongWord;
    FXferIsGlobal: array[TSSARegisterType] of array of Boolean;

    // --- emission state -------------------------------------------------
    FStateLocal: LongWord;
    FResultTmp: array[TSSARegisterType] of LongWord;
    FSlotBase: array[TSSARegisterType] of LongWord;
    FRawTmp: LongWord;            // i64: a raw pointer being decoded
    FFltTmp: LongWord;            // f64 scratch (TIMER's Frac, and whoever needs one next)
    FF32Tmp: LongWord;            // f32 scratch: a binary32 value that must not be promoted
    FGfxP, FGfxN: LongWord;       // i32: the ScreenRes fill cursor and counter

    { ⛔ END INSIDE A PROCEDURE ENDS THE PROGRAM, NOT THE FUNCTION. In WASM a
      return only unwinds one frame, so the plain return this used to emit let
      the caller carry on: m12_endinproc printed its destructors correctly and
      then went on to print the "NEVER" that proves the halt was ignored. There
      is no deopt here, so the choice is between refusing the shape and modelling
      it - and it is modelled with a flag global that every call site tests,
      which unwinds the whole chain one frame at a time.
      ⚠️ Emitted ONLY when a procedure actually halts (FUsesHalt), so a program
      whose END sits in main pays nothing - which is nearly all of them. }
    FUsesHalt: Boolean;
    FHaltFlag: LongWord;

    { --- array parameters: BIND / BIND APPLY / UNBIND ---------------------
      Passing an array aliases the callee's placeholder descriptor to the
      caller's array for the duration of the call, and restores it after. The
      interpreter keeps a LIFO save-stack for it; here the saved descriptor and
      the snapshot live in WASM LOCALS instead.
      ⭐ That is not a shortcut, it is the recursion answer: locals are
      per-activation, so a recursive SUB taking an array (merge sort passing
      a() and b() the other way round) gets a fresh save for free, with no
      stack to size, no ceiling to trap on, and nothing to allocate.
      The pairing is resolved at EMIT time - bind and unbind are emitted in the
      order the SSA laid them down - so this stack holds compile-time
      bookkeeping, not runtime state. }
    FBindStack: array of record
      ParamIdx, ArgIdx: Integer;
      SavedLocal, SnapLocal: LongWord;   // first of Words consecutive i32 locals
      Words: Integer;
    end;
    FBindTop: Integer;
    FBindSeq: Integer;                   // which bind of this region is next
    FBindLocal: array of LongWord;       // per bind, in emission order: saved base
    FBindWords: array of Integer;

    { --- the multi-dimensional REDIM ---------------------------------------
      "ReDim a(l0 TO u0, l1 TO u1)" lowers to one PUSH per bound and a commit,
      and the interpreter accumulates the VALUES in a pending list.
      ⛔ Reading the pushed REGISTERS back at the commit does not work, and the
      failure is silent: a bound register is dead after its push, so the
      allocator is free to give the next bound the same one. The value has to be
      captured WHERE IT IS PUSHED.
      ⭐ So each push gets its own LOCAL, pre-counted like the array binds - and
      being per-activation it also makes a REDIM inside a recursive procedure
      safe, which a shared pending area would not be. }
    FRedimLocal: array of LongWord;      // per push, in emission order
    FRedimSeq: Integer;                  // which push of this region is next
    FRedimPend: array of record          // pushes seen and not yet committed
      Local: LongWord;
      IsLb: Boolean;
    end;
    { --- the runtime multi-dimensional INDEX --------------------------------
      "a(i, j)" on an array whose shape is only known at run time pushes each
      (already lower-bound-adjusted) index and resolves them against the array's
      CURRENT dimensions. Same pairing and the same reason as the REDIM bounds:
      captured at the push, into a local of its own. }
    FIdxLocal: array of LongWord;        // per index push, in emission order
    FIdxScratch: LongWord;               // i64: the running stride
    FIdxSeq: Integer;
    FIdxPend: array of LongWord;
    { --- an array that is a MEMBER of a UDT ---------------------------------
      ⭐ The reduction that makes this family small: in this backend a
      descriptor is ALREADY an i32 address that every array helper takes as a
      parameter. A member array is therefore the same code with the descriptor
      coming from a REGISTER instead of a compile-time constant - the
      descriptor lives on the heap, one per record instance, and the record's
      field holds its address.
      ⚠️ Natively that field holds an INDEX into the VM's array table. Same
      trade as a record-field pointer: the number differs, and nothing but
      printing it could tell. }
    FDescTmp: LongWord;                  // i32: the descriptor being reshaped
    FHasDescTmp: Boolean;

    // --- graphics -------------------------------------------------------
    FUsesGfx: Boolean;
    FScrW, FScrH: LongWord;       // WASM globals holding the screen geometry
    FFbBase: LongWord;            // WASM global: where SCREENRES put the framebuffer

    // --- strings and the heap -------------------------------------------
    FUsesStr: Boolean;            // the program has string values
    FUsesHeap: Boolean;           // ... or graphics: either way it allocates
    FHeapTop: LongWord;           // WASM global: the bump pointer
    FHeapBase: LongWord;          // where the bump starts, after the literals
    FConstId: array of Integer;   // pool ids of the literals, in layout order
    FConstAddr: array of LongWord;
    FConstBytes: AnsiString;      // the data segment, already laid out
    FAllocFunc, FStrNewFunc, FStrCatFunc, FStrSubFunc, FStrCmpFunc,
    FStrAscFunc, FStrChrFunc, FStrRightFunc, FStrMidFunc,
    FPrintStrFunc, FStrFromIntFunc: LongWord;
    FStrFillFunc, FStrCaseFunc, FStrFindFunc, FStrTrimFunc: LongWord;
    FPuDigFunc, FPuFmtFunc: LongWord;   // PRINT USING: digits, then the field
    { VAL. ⛔ TWO flags, not one: VALINT needs only the integer scanner, while
      VAL needs the decimal-to-double conversion - and that one leans on fltMul,
      so it also has to force FUsesFlt. A program that only calls VALINT must
      not emit a function that calls a helper its module never got. }
    FUsesVal: Boolean;                  // VAL(s) -> Double
    FUsesValInt: Boolean;               // VALINT/VALLNG/VALUINT(s) -> Integer
    FStrValIntFunc, FValBitFunc, FStrValFunc: LongWord;
    FUsesBase: Boolean;                 // HEX$ / OCT / BIN
    FStrBaseFunc: LongWord;             // one helper, the base is a parameter
    FUsesTrimSet: Boolean;              // TRIM(s, set) and its two sides
    FStrTrimSetFunc: LongWord;
    { MKI$/MKL$/MKD$/MKS$ and CVI/CVL/CVD/CVS. ⭐ TWO helpers for eight
      spellings, because all eight are "move these bytes" in one direction or
      the other and a float only differs by a reinterpretation at the edge. }
    FUsesPack: Boolean;
    FStrPackFunc, FStrUnpackFunc: LongWord;
    { The WSTRING family. ⭐ A wide string here is UTF-8 bytes in the ORDINARY
      string bank - the decision is [[wstring-encoding-decision]] - so none of
      this needs a second storage, only a second way of COUNTING. }
    FUsesWide: Boolean;
    FWLenFunc, FWSubFunc, FWEncFunc, FWB2CPFunc, FWRepFunc: LongWord;
    { DATESERIAL / DATEPART / DATEADD / DATEDIFF and the field accessors. }
    FUsesDate: Boolean;
    FUsesDateName: Boolean;             // MONTHNAME / WEEKDAYNAME: a data segment, no helper
    FDtSplitFunc, FDtCivilFunc, FDtDaysFunc: LongWord;
    FDtIvalFunc, FDtIncMoFunc, FDtDoyFunc, FDtWoyFunc, FDtYMFunc: LongWord;
    FUsesDateStr: Boolean;              // DATE$ / TIME$: the rendering, on top of the calendar
    FDtStrFunc: LongWord;
    { RND / RANDOMIZE. ⭐ The interpreter's generator is FPC's, which is
      MT19937 seeded with init_genrand and read as genrand_int32 / 2^32 -
      IDENTIFIED by measurement, not by reading the RTL: seed 42 gives
      1608637542 and 0.37454011430963874, both to the last digit. Porting it is
      what makes a SEEDED sequence reproduce between sb and the module; a host
      import could not, whatever else it offered. }
    FUsesRnd: Boolean;
    FMtBaseG: LongWord;                 // i32 global: where the 624-word state lives
    FMtSeedFunc, FMtNextFunc: LongWord;
    FUsesErrName: Boolean;              // ERR$(n): the message table, as a data segment
    FUsesBool: Boolean;                 // PRINT of a BOOLEAN: two words in a data segment
    FUsesDateParse: Boolean;            // DATEVALUE / TIMEVALUE / ISDATE
    FDtFindFunc, FDtFieldsFunc, FDtDatePartFunc, FDtTimePartFunc, FDtParseFunc: LongWord;
    FUsesGfxPrim: Boolean;              // the program DRAWS (LINE / PSET / POINT)
    FGfxPsetFunc, FGfxLineFunc: LongWord;
    FPenX, FPenY: LongWord;             // globals: the current graphics point
    FUsesPU: Boolean;              // the program has a PRINT USING

    // --- arrays ---------------------------------------------------------
    FUsesArr: Boolean;
    FUsesRec: Boolean;              // the program builds UDT records
    FUsesRecArr: Boolean;           // ... one per element of an ARRAY of them
    FArrDescOf: array of LongWord;   // array index -> its descriptor's address
    FArrTmp: LongWord;               // i32 scratch: the running element product
    FRecTmp: LongWord;               // i32 scratch: a record handle being addressed
    FArrLoad, FArrStore: array[TSSARegisterType] of LongWord;
    FArrLBoundFunc, FArrUBoundFunc: LongWord;
    FRecNewArrFunc: LongWord;        // fill an array of UDT with fresh records

    { --- FreeBASIC POINTERS -----------------------------------------------
      A pointer VALUE is the interpreter's, bit for bit: the high 32 bits hold
      (backingArrayId + 1) so 0 stays NULL, the low 32 an element offset, and a
      record-field pointer sets bit 63 instead. Reproducing the encoding rather
      than inventing a linear address is what keeps "p + 1" advancing by one
      ELEMENT on both sides, and what makes a pointer survive a REDIM: the
      deref reads the descriptor at run time, so it follows the array to its new
      block exactly as the interpreter does.
      ⇒ the one thing the encoding needs that the interpreter gets for free is
      a way to turn an array id into a descriptor at RUN TIME - the id is a
      compile-time number everywhere else, so nothing else ever needed it. That
      is FArrTab: one i32 per declared array, emitted as data. }
    FUsesPtr: Boolean;
    FArrTabAddr: LongWord;
    FArrTabBytes: AnsiString;
    FRefLoad, FRefStore: array[TSSARegisterType] of LongWord;

    { --- FUNCTION POINTERS ------------------------------------------------
      ⛔ THE TABLE WAS NEVER THE HARD PART - THE SIGNATURE IS. Natively an
      indirect call jumps to an entry PC and the arguments travel through the
      transfer bank, so no two procedures need to agree on anything. WASM checks
      the type at every call_indirect, so the callee's signature has to be known
      where the CALLER stands - and there the target is a value.
      ⇒ Every procedure whose address is taken is given ONE signature: the union
      of what the indirect call sites stage and of what those procedures already
      take. They then share a type index, and a call through any of them
      matches. Where that union cannot be built - two of them returning
      different banks - the program is REFUSED, because the alternative is a
      module that traps on a call that was perfectly well defined natively.
      ⭐ A function pointer's VALUE is the region index (its slot in the table),
      not an entry PC: the two are equally opaque to a program, and this one is
      the only one WASM can call. }
    FIndirect: Boolean;
    FAddrTaken: array of Boolean;
    FIndParam: array[TSSARegisterType] of Integer;
    FIndTypeIdx: LongWord;
    FUsesPow: Boolean;
    FPowFunc: LongWord;
    FIndResUsed: array[TSSARegisterType] of Boolean;
    FIndResGlobal: array[TSSARegisterType] of LongWord;
    FThunkIdx: array of LongWord;         // region -> its table entry

    // --- float printing -------------------------------------------------
    FUsesFlt: Boolean;
    FFltDigits: Integer;             // "OPTION DIGITS n"; 16 unless the source said otherwise
    FQBLang: Boolean;                // the source declared -lang qb
    FFltMulFunc, FFltDecFunc, FFltPrintFunc: LongWord;
    FFltOutFunc, FFltStrFunc: LongWord;   // where the rendered text goes, and STR$
    FUsesStrStr: Boolean;                 // the program calls Str() on a float

    FBankBase: array[TSSARegisterType] of Integer;
    FFlatCount: Integer;
    FUpExposed: array of array of Boolean;   // region -> flat register id

    function Fail(const Msg: string): Boolean;
    function BankIs(const V: TSSAValue; Want: TSSARegisterType;
      const Who: string): Boolean;
    procedure Classify32BitRegisters;   // which int registers can live in an i32 local
    function FlatId(const V: TSSAValue): Integer;
    procedure ComputeUpExposed;
    function BlockOfLabel(const AName: string): Integer;
    function BuildPartition: Boolean;
    procedure NoteRegister(const V: TSSAValue; Region: Integer);
    function ClassifyRegisters: Boolean;
    function BuildSignatures: Boolean;
    function DetectRecursion: Boolean;
    function IsSharedSlot(ASlot: Integer): Boolean;
    procedure NoteXferGlobal(Bank: TSSARegisterType; ASlot: Integer);
    procedure ScanForHalt;
    procedure EmitReturnValues(B: TWasmBuf; R: Integer);
  private
    FUsesPrint: Boolean;
    FUsesClock: Boolean;          // NOW / TIMER: an import, there is no time in WASM
    FUsesTrig: Boolean;           // SIN / COS: an import, WASM has no transcendentals
    FImportCount: LongWord;
    FWriteFunc, FPrintIntFunc, FPrintUIntFunc, FPrintNlFunc: LongWord;
    { PRINT's comma tabs to a column, so the module has to know which column it
      is on - the one piece of screen state a byte sink does not give you.
      ⚠️ Paid for ONLY by a program that has a comma: without one, every write
      still goes straight to the import and no counter exists. }
    FUsesCol: Boolean;
    FColG: LongWord;              // i32 global: the cursor column
    FEmitFunc: LongWord;          // write, and advance the column
    FNowFunc: LongWord;
    { The transcendentals, all imported for the same reason: WASM has none of
      them. They are the host's, and the host's are not FPC's - one ulp apart on
      some arguments, measured. }
    FTrigFunc: array[0..11] of LongWord;
    FXTrigFunc: array[0..3] of LongWord;   // tanh/asinh/acosh/atanh, imported only if used
    FAtan2Func: LongWord;                  // atan2: TWO arguments, so its own import
    FUsesAtan2: Boolean;
    FUsesXTrig: array[0..3] of Boolean;
    procedure ScanForPrint;
    function WriteTarget: LongWord;
    function ConstAddrOf(const V: TSSAValue): LongWord;
    function ExtraOperands(Instr: TSSAInstruction): TSSAValueArray;
    procedure EmitArrayHelpers;
    procedure EmitRecordHelpers;
    procedure EmitRefHelpers;
    procedure EmitThunks;
    procedure EmitPowHelper;
    procedure EmitRedimShape(B: TWasmBuf; Preserve: Boolean; NLower, NUpper: Integer);
    procedure LoadMemberDesc(B: TWasmBuf; const Handle: TSSAValue; Enc: Integer;
                             Allocate: Boolean);
    procedure EmitFloatHelpers;
    procedure EmitFloatPrint;
    procedure PushOutSlots(B: TWasmBuf; R: Integer);
    procedure PopOutSlots(B: TWasmBuf; Callee: Integer);
    procedure EmitRawAddr(B: TWasmBuf);
    procedure EmitPrintHelpers;
    procedure EmitHeapHelpers;
    procedure EmitGfxHelpers;
    procedure EmitStringHelpers;
    procedure EmitValHelpers;
    procedure EmitBaseHelper;
    procedure EmitTrimSetHelper;
    procedure EmitPackHelpers;
    procedure EmitWideHelpers;
    procedure EmitDateHelpers;
    function BuildDateNameBlob: AnsiString;
    function BuildErrNameBlob: AnsiString;
    procedure EmitTwoDigits(B: TWasmBuf; Base, Val: LongWord; Off: LongWord);
    procedure EmitRndHelpers;
    procedure EmitDateParseHelpers;
    procedure EmitFlushField(B: TWasmBuf);
    procedure EmitTrimRange(B: TWasmBuf; Base, Lo, Hi, Tmp: LongWord);
    procedure EmitDatePartSwitch(B: TWasmBuf);
    procedure EmitDateAddSwitch(B: TWasmBuf; Instr: TSSAInstruction);
    procedure EmitDateDiffSwitch(B: TWasmBuf; Instr: TSSAInstruction);
    procedure EmitDtDays(B: TWasmBuf);
    procedure EmitDtWeekday(B: TWasmBuf);
    function EmitRegion(R: Integer): Boolean;
    function FloatRegUses(First, N, RegIdx: Integer): Integer;
    function IntRegUses(First, N, RegIdx: Integer): Integer;
    function EmitInstr(B: TWasmBuf; Instr: TSSAInstruction; R: Integer): Boolean;
    function RegionNeedsF32Tmp(R: Integer): Boolean;   // is the f32 scratch worth declaring here?
    procedure EmitTruncF32(B: TWasmBuf; Instr: TSSAInstruction);  // trunc an f32 already on the stack
    function RegIs32(const V: TSSAValue): Boolean;   // does this operand live in an i32 local?
    procedure LoadReg(B: TWasmBuf; const V: TSSAValue);
    procedure LoadRegI32(B: TWasmBuf; const V: TSSAValue);   // ...as an i32, free when it lives in one
    procedure StoreRegI32(B: TWasmBuf; const V: TSSAValue; Signed: Boolean);
    function LoadInt32(B: TWasmBuf; const V: TSSAValue): Boolean;
    procedure StoreReg(B: TWasmBuf; const V: TSSAValue);
    procedure BoolToBasic(B: TWasmBuf);
    function OpName(Op: TSSAOpCode): string;
  public
    constructor Create(AProgram: TSSAProgram; AModern: Boolean);
    destructor Destroy; override;
    function Compile: Boolean;
    procedure SaveToFile(const Path: string);
    property ErrorMessage: string read FError;
    { How many registers had to be promoted to globals because more than one
      region touches them. Reported, not buried: if it is ever large the
      mapping needs revisiting, not tolerating. }
    property GlobalCount: Integer read FGlobalCount;
    property RegionCount: Integer read FRegionCount;
    property Module: TWasmModule read FModule;
    { "OPTION DIGITS n" from the source. The backend cannot see the directive
      itself - it works on the SSA, which is past it - so the host hands it over.
      ⛔ Without this the module would print a different number of digits than
      the interpreter for the very same program, and the differential would be
      right to call it a defect. }
    property FloatDigits: Integer read FFltDigits write FFltDigits;
    { The source declared -lang qb ('$lang: "qb"'). It changes PRINT spacing and
      the backend cannot see the directive - the SSA is past it - so the host
      hands it over, exactly like the digit count. ⭐ In QB an INTEGER gets a
      trailing space and a FLOAT does not, which is why this cannot be folded
      into "not Modern": Commodore gives BOTH the trailing space. }
    property QBLang: Boolean read FQBLang write FQBLang;
  end;

const
  WASM_XFER_RESULT_SLOT = 255;   // mirrors SedaiSSA.XFER_RESULT_SLOT
  { Mirrors SedaiSSA.SHARED_SLOT_BASE: at and above this the transfer bank stops
    carrying arguments and starts carrying module-global SHARED scalars, the
    by-value UDT result handle (254) and the result (255). }
  WASM_SHARED_SLOT_BASE = 128;

  { The transcendentals the host provides, in the order they are imported. }
  TRIG_NAME: array[0..11] of AnsiString =
    ('sin', 'cos', 'tan', 'atn', 'exp', 'log', 'log10', 'log2',
     'asin', 'acos', 'sinh', 'cosh');
  { ⛔ The twelve above are imported TOGETHER the moment a program uses any of them, so adding a name
    to that array would make every existing module ask its host for a function it never asked for
    before - including the PUBLISHED demo page, whose imports are fixed. These are imported one at a
    time, only when used, which is why they are a separate table. }
  XTRIG_NAME: array[0..3] of AnsiString = ('tanh', 'asinh', 'acosh', 'atanh');

implementation

const
  BankType: array[TSSARegisterType] of TWasmValType = (wvtI64, wvtF64, wvtI32);
  { The same three banks as a BLOCK type, for an if/else that has to leave a
    value of the bank's width on the stack. }
  BlockTypeOf: array[TSSARegisterType] of Byte =
    (WASM_TYPE_I64, WASM_TYPE_F64, WASM_TYPE_I32);

  { ---- the linear memory map ----------------------------------------------

    0 .. 3      the EMPTY STRING, and it costs nothing. A string handle is the
                address of a [i32 len][bytes] header, linear memory starts
                zeroed, and a fresh WASM local is 0 - so handle 0 reads as
                length 0, which is exactly what an unassigned BASIC string is.
                ⛔ Nothing may ever be written there.
    4 .. 63     the PRINT digit scratch, built BACKWARDS from SCRATCH_END.
    64, 65      a literal ' ' and a literal LF.
    1024 ..     the string LITERALS, one [i32 len][bytes] header each, laid out
                at compile time into one data segment.
    FHeapBase   where the bump allocator starts - the first 4-aligned address
                after the literals. }
  EMPTY_STR    = 0;
  SCRATCH_END  = 64;      // one past the last byte of the digit scratch
  CONST_SPACE  = 64;      // a literal ' '
  CONST_NL     = 65;      // a literal LF
  { The specials, laid out right after ' ' and LF. FreeBASIC hands a NaN or an
    infinity to the platform's C library, so its own spelling differs by
    platform; these are MSVCRT's, which is what the native side prints on the
    machine this is compared against. }
  CONST_QNAN   = 66;      // '1.#QNAN'
  CONST_IND    = 73;      // '-1.#IND'
  CONST_INF    = 80;      // '1.#INF'
  CONST_NINF   = 86;      // '-1.#INF'
  { The float formatter's workspace. None of it is allocated: the sizes are
    bounded by the type, not by the program. }
  FLT_LEN      = 96;      // i32: how many digits FLT_DEC holds
  FLT_FRAC     = 100;     // i32: how many of them are after the point
  { ⭐ STR$ AND PRINT ARE THE SAME RENDERING, and these two cells are the whole
    difference. fltPrint always leaves its text at FLT_OUT; FLT_CAP says whether
    to hand it to the host or leave it there, and FLT_OLEN says how long it is.
    ⛔ Two memory cells rather than two parameters for the reason VAL_DECW gives:
    fltPrint's body is written against locals 2..11, and appending a parameter
    renumbers every one of them. }
  FLT_CAP      = 104;     // i32: 1 = do not write, the caller wants the text
  FLT_OLEN     = 108;     // i32: how long the text at FLT_OUT is
  FLT_DIG      = 128;     // the kept, ROUNDED digits, most significant first
  FLT_OUT      = 1024;    // the rendered text
  FLT_DEC      = 2048;    // the EXACT digits, least significant first
  { PRINT USING's workspace. PU_DIG holds the value's digits MOST significant
    first, already rounded to the field's decimals; PU_OUT the rendered field.
    ⭐ Both are bounded by the type rather than by the program - so, like the
    float formatter's workspace above, none of this is allocated.
    ⛔ It sits ABOVE 4096 and not below: FLT_DEC starts at 2048 and an exact
    expansion runs to over a thousand digits (a subnormal has 1074 fractional
    ones), so anything placed at 3072 would be overwritten by the very digits it
    was about to read. }
  PU_NINT      = 4096;    // i32: how many of the digits are integer digits
  PU_NDEC      = 4100;    // i32: and how many are fractional
  PU_DIG       = 4224;    // the rounded digits, most significant first
  PU_OUT       = 6144;    // the rendered field
  { VAL()'s workspace - the DECIMAL-TO-BINARY direction, which reuses FLT_DEC as
    its digit buffer and fltMul as the only thing that writes to it, so the two
    directions share one idiom and one scratch area. Nothing here is allocated
    either: the sizes are bounded by what a double can hold, not by the program.
    ⚠️ VAL_SIG is capped at 800 digits and VAL_MAXDIG at 1500 for the reasons
    spelled out in ExactStrToDouble (SedaiConsoleBehavior) - both are the SAME
    numbers as the native side, because the two have to agree bit for bit. }
  VAL_SIG      = 8192;    // significant digits, MOST significant first (800)
  VAL_LIMB     = 9216;    // the integer part in 30-bit limbs, low first (200)
  VAL_NLIMB    = 10240;   // i32: how many limbs VAL_LIMB holds
  { ⚠️ The DECIMAL saturation width valInt was asked for, 32 or 0. It travels in
    memory rather than as a second parameter for one reason only: valInt's body
    was written against locals 1..9, and appending a parameter renumbers every
    one of them. Both callers are in this file, ten lines apart, and each stores
    it immediately before the call. }
  VAL_DECW     = 10244;   // i32: valInt's decimal saturation width
  { HEX$/OCT/BIN's digit scratch, built BACKWARDS from BASE_SCR_END exactly as
    the decimal one is. ⛔ It cannot BE the decimal one: that scratch is the 60
    bytes at 4..63, and a 64-bit value in base 2 is 64 digits - four more than
    it holds, and the four would land on the EMPTY STRING at address 0, which
    nothing may ever write to. The widest case is what sizes a scratch, not the
    common one. }
  BASE_SCR_END = 10432;
  BASE_SCR     = BASE_SCR_END - 64;
  { The date/time split. ⭐ ONE rounded millisecond count is the whole of it -
    see EmitDateHelpers - so these five cells are everything the family needs. }
  DT_DAYS      = 10448;   // i32: whole days since 1899-12-30
  DT_MS        = 10452;   // i32: milliseconds into the day, always non-negative
  DT_Y         = 10456;
  DT_MO        = 10460;
  DT_D         = 10464;
  { MONTHNAME / WEEKDAYNAME. ⭐ The nineteen names are laid down as ORDINARY
    STRING HEADERS in their own data segment, preceded by a table of the
    nineteen handle addresses - so naming one is a single i32.load of a
    compile-time address and no helper function at all. Weekdays first (Sunday
    is 1), then the months.
    ⛔ They are the ENGLISH names and there is no locale path: the interpreter
    has one (LocaleDayName) but nothing ever turns it on - SetDateLocaleMode has
    no caller - and a sandbox has no locale to read anyway. }
  { DATEVALUE / TIMEVALUE / ISDATE. ⛔ ONLY the DETERMINISTIC forms - "yyyy-mm-dd",
    "yyyy/mm/dd", "hh:mm[:ss]", either of them alone, the two separated by a
    space, with an optional AM/PM. The interpreter falls back to the HOST's date
    parser for anything else, and a sandbox has no host to ask; that divergence
    is the one the project already declared when it made the deterministic path
    the default (a parse that changes with the machine's regional settings is not
    diffable and its baselines are not reproducible). }
  { A BOOLEAN prints as the word, so the module carries the two words. They sit
    in the gap under the calendar's cells and cost nine bytes of data segment
    only when a program actually prints a boolean. }
  BOOL_TRUE    = 10432;   // 'true'
  BOOL_FALSE   = 10436;   // 'false'
  DT_BAD       = -999999; // the "this field was not a number" sentinel, as SplitInts uses
  DT_PARSE     = 10472;   // f64: the parsed serial
  DT_F0        = 10480;   // the three integer fields a split leaves behind
  DT_F1        = 10484;
  DT_F2        = 10488;
  DT_PD        = 10496;   // f64: the date half, once it has parsed
  DT_PT        = 10504;   // f64: and the time half
  { ERR$(n). The messages go in as ordinary string headers with a table of
    already-resolved handles in front, exactly as the month names do - and the
    TABLE IS BUILT BY CALLING THE INTERPRETER'S OWN GetFBErrorCodeDescription -
    the FreeBASIC one, this target being MODERN-only, and a DIFFERENT table from
    the Commodore GetErrorCodeDescription, sharing only numbers - so
    the two cannot drift apart by a word. Codes with no message of their own
    hold 0 here and are rendered at run time as "user error n". }
  { ⭐ The codes are TWO ranges, not one - 0..41 and
    100..113 added by this project - so the table is 56 compact slots instead of
    114, which is what makes it fit at all. Slot 56 holds the word the fallback
    needs. }
  ERR_LO_MAX   = 41;
  ERR_HI_LO    = 100;
  ERR_HI_MAX   = 113;
  ERR_SLOTS    = (ERR_LO_MAX + 1) + (ERR_HI_MAX - ERR_HI_LO + 1) + 1;
  ERR_NAMES    = 10864;   // the handles, then the strings
  DT_NAMES     = 10560;   // the table of 19 handles, then the strings
  DT_NAME_COUNT = 19;
  VAL_DIGCAP   = 800;
  VAL_MAXDIG   = 1500;    // ⛔ must stay under PU_NINT - FLT_DEC = 2048
  VAL_MAXLIMB  = 200;
  STR_CONST_BASE = 12288; // the first string literal

  { The two REGIONS a tagged raw pointer can name (SedaiSSATypes): region 0 is
    the byte heap Allocate returns, region 1 the framebuffer SCREENPTR returns.
    A pointer carries a byte OFFSET, never an address, so each region has a base
    and the offset is added to it. Region 0's base is FHeapBase; region 1's is a
    GLOBAL, because the framebuffer is now bump-allocated by SCREENRES like
    everything else. ⭐ That is what keeps strings and graphics from colliding:
    a fixed FB_BASE right above the heap capped the heap at one page, and a
    fixed base above a growing heap cannot exist when the framebuffer's size is
    only known at run time. One allocator, one arena, no partition to get wrong. }
  { The framebuffer's initial contents are not zero: a fresh ScreenRes fills
    every pixel with opaque black, and linear memory starts zeroed, so the fill
    has to be emitted or the very first byte of every comparison would differ.
    ⚠️ MEASURED TWICE. The first reading said $FF000000, because the probe
    PRINTED before reading it back - and in graphics mode PRINT renders into the
    framebuffer, so the probe was reading its own output. Any measurement of the
    initial buffer has to happen before the first character is printed.
    ⛔ It then read 255 for a year, and that was the interpreter's own defect
    faithfully copied: the native ResizeScreen cleared to $000000FF, the same
    black in the WRONG byte order for a buffer the presenter uploads as ARGB.
    fbc answers &hFF000000, and so do we now - on both backends. This constant
    is not independent of SedaiGraphicsBackend.ResizeScreen; they are one
    decision written twice. }
  FB_CLEAR     = $FF000000;    // opaque black, ARGB - matches fbc and the interpreter

  { PRINT's comma zone, and the line it wraps on. Both are the FreeBASIC values
    the interpreter sets for MODERN (SedaiBytecodeVM: "A comma indicates
    printing should take place at the next 14 column boundary" - the FB manual's
    Print page - on an 80-column console). ⛔ The Commodore pair is 10 and 40,
    and it is not reachable here: the target is MODERN-only. }
  COMMA_TAB    = 14;
  SCREEN_COLS  = 80;

  { How many dimensions a UDT member array's descriptor is sized for. A declared
    array's descriptor is sized from its own DimCount because that is written in
    the program; a member's is allocated at the first REDIM, and nothing there
    bounds what a LATER one will ask for - so it is allocated at a maximum, and
    a REDIM past it is refused by name rather than writing past the block. }
  WASM_MEMBER_MAX_DIMS = 8;

{ ---------------- PRINT ----------------

  The host import is a BYTE SINK - write(ptr, len) - and nothing else. The
  formatting is ours, emitted here, because BASIC's number spacing is a dialect
  rule (TConsoleBehavior.FormatInt): a leading space stands in for the sign when
  the value is non-negative, and Commodore adds a trailing space where FreeBASIC
  does not. Handing an i64 to JS and letting it call String(n) would produce
  output that differs from the native run in exactly the places this project
  measures byte for byte - the plan rules that out, and it is the whole reason
  the sink is this narrow. }

procedure TWasmBackend.EmitRawAddr(B: TWasmBuf);
{ Takes a tagged raw pointer (i64) off the stack and leaves the linear address
  (i32) it names. The pointer carries a byte OFFSET in its low 61 bits and a
  region selector in bit 61, so the address is offset + the region's base -
  chosen with a select rather than a branch. }
begin
  B.LocalTee(FRawTmp);
  B.I64Const(RAWPTR_OFS_MASK);
  B.Op(wopI64And);
  B.Op(wopI32WrapI64);
  B.GlobalGet(FFbBase);
  B.I32Const(LongInt(FHeapBase));
  B.LocalGet(FRawTmp);
  B.I64Const(RAWPTR_REGION_FB);
  B.Op(wopI64And);
  B.Op(wopI64Eqz);
  B.Op(wopI32Eqz);          // 1 when the region bit is set -> the framebuffer
  B.Op(wopSelect);
  B.Op(wopI32Add);
end;

procedure TWasmBackend.ScanForPrint;
{ One pass that answers everything the module SHAPE depends on: does the program
  print, does it draw, does it hold strings - and which literals it holds, which
  have to be laid out before the first byte of code is emitted because their
  addresses are immediates.
  ⚠️ The order matters more than it looks: imports own the low function indices,
  so what the module imports has to be known before anything is defined. }
var
  i, j, k: Integer;
  Blk: TSSABasicBlock;
  Ins: TSSAInstruction;
  Addr: LongWord;
  S: AnsiString;

  procedure NoteConst(const V: TSSAValue);
  var
    m: Integer;
  begin
    if V.Kind <> svkConstString then Exit;
    for m := 0 to High(FConstId) do
      if FConstId[m] = V.ConstStringId then Exit;   // the pool already dedups
    SetLength(FConstId, Length(FConstId) + 1);
    FConstId[High(FConstId)] := V.ConstStringId;
  end;

begin
  FUsesPrint := False;
  FUsesCol := False;
  FUsesPU := False;
  FUsesGfxPrim := False;
  FUsesClock := False;
  FUsesTrig := False;
  for i := 0 to High(FUsesXTrig) do FUsesXTrig[i] := False;
  FUsesAtan2 := False;
  FUsesPow := False;
  FUsesGfx := False;
  FUsesStr := False;
  FUsesArr := False;
  FUsesRecArr := False;
  FUsesPtr := False;
  FUsesStrStr := False;
  FIndirect := False;
  SetLength(FAddrTaken, FRegionCount);
  for i := 0 to FRegionCount - 1 do FAddrTaken[i] := False;
  FUsesFlt := False;
  FUsesVal := False;
  FUsesValInt := False;
  FUsesBase := False;
  FUsesTrimSet := False;
  FUsesPack := False;
  FUsesWide := False;
  FUsesDate := False;
  FUsesDateName := False;
  FUsesDateStr := False;
  FUsesDateParse := False;
  FUsesBool := False;
  FUsesErrName := False;
  FUsesRnd := False;
  FUsesRec := False;
  SetLength(FConstId, 0);
  for i := 0 to FProg.Blocks.Count - 1 do
  begin
    Blk := FProg.Blocks[i];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Ins := TSSAInstruction(Blk.Instructions[j]);
      case Ins.OpCode of
        { PRINT's comma TABS, so it is the one print opcode that needs to know
          which column the cursor is on. Everything else is a byte sink. }
        ssaPrintComma:
          begin FUsesPrint := True; FUsesCol := True; end;
        ssaPrintInt, ssaPrintIntLn, ssaPrintNewLine, ssaPrintUInt:
          FUsesPrint := True;
        ssaPrintString, ssaPrintStringLn:
          begin FUsesPrint := True; FUsesStr := True; end;
        ssaPrint, ssaPrintLn:
          begin FUsesPrint := True; FUsesFlt := True; end;
        ssaGfxScreenRes, ssaGfxScreenPtr, ssaGfxScreenInfo,
        ssaRawLoadInt, ssaRawStoreInt, ssaRawLoadFloat, ssaRawStoreFloat,
        ssaRawClear, ssaRawMemCopy, ssaRawMemMove:
          FUsesGfx := True;
        ssaGfxLine, ssaGfxPset, ssaGfxPoint:
          begin FUsesGfx := True; FUsesGfxPrim := True; end;
        { ⚠️ Gfx here means "raw memory exists", not "the program draws": the
          raw pointer decoder needs the framebuffer base to select a region, and
          FUsesHeap is derived from this below - which is what actually gives
          the module its memory and its allocator. }
        ssaRawAlloc, ssaRawRealloc:
          FUsesGfx := True;
        ssaLoadConstString, ssaStrConcat, ssaStrLen, ssaStrLeft, ssaStrRight,
        ssaStrMid, ssaStrAsc, ssaStrAscMid, ssaStrChr, ssaIntToString,
        ssaStrSpace, ssaStrString, ssaStrUCase, ssaStrLCase, ssaStrInstr,
        ssaStrLTrim, ssaStrRTrim, ssaStrTrim,
        ssaCommand,
        ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString,
        ssaStrMidAssign:
          FUsesStr := True;
        ssaArrayDim, ssaArrayLoad, ssaArrayStore, ssaArrayLBound, ssaArrayUBound,
        ssaArrayBind, ssaArrayBindApply, ssaArrayUnbind, ssaArrayRedim,
        ssaArrayRedimPush, ssaArrayRedimN,
        ssaArrayIdxPush, ssaArrayIdxResolve,
        ssaArrayLoadIndInt, ssaArrayLoadIndFloat,
        ssaArrayStoreIndInt, ssaArrayStoreIndFloat,
        ssaArrayLBoundInd, ssaArrayUBoundInd, ssaArrayIdxResolveInd:
          FUsesArr := True;
        ssaArrayLoadIndString, ssaArrayStoreIndString:
          begin FUsesArr := True; FUsesStr := True; end;
        { A member array's descriptor lives on the heap, one per record, so this
          needs the record heap as much as the array machinery. }
        ssaMemberArrayRedim:
          begin FUsesArr := True; FUsesRec := True; end;
        ssaRecordNewArrayInd:
          begin FUsesArr := True; FUsesRec := True; FUsesRecArr := True; end;
        ssaPrintUsing, ssaPrintUsingInt:
          begin FUsesPU := True; FUsesStr := True; FUsesPrint := True;
                FUsesFlt := True; end;   // puDigits leans on the float digits
        { STR$ of a float is the PRINT rendering minus the spacing, so it needs
          the float formatter as much as a PRINT does - and a string to put the
          answer in. It does NOT need the print import: fltOut knows. }
        ssaStrStr, ssaFloatToString:
          begin FUsesStrStr := True; FUsesStr := True; FUsesFlt := True; end;
        ssaStrVal:
          begin FUsesVal := True; FUsesStr := True;
                FUsesFlt := True; end;   // valFlt leans on the float digit buffer
        ssaStrValInt:
          begin FUsesValInt := True; FUsesStr := True; end;
        { The three spellings of one operation: the base is the only thing that
          differs, so they share a helper and one of them makes it exist. }
        ssaStrHex, ssaStrOct, ssaStrBin:
          begin FUsesBase := True; FUsesStr := True; end;
        ssaStrTrimSet:
          begin FUsesTrimSet := True; FUsesStr := True; end;
        ssaStrMkInt, ssaStrMkFloat, ssaStrCvInt, ssaStrCvFloat:
          begin FUsesPack := True; FUsesStr := True; end;
        { ⚠️ The two WSTRING searches lean on strFind, so they need the ordinary
          string helpers as well as the wide ones - and FUsesStr already brings
          strFind in, since it is unconditional in that block. }
        ssaStrLenW, ssaStrLeftW, ssaStrRightW, ssaStrMidW,
        ssaStrInstrW, ssaStrInstrRevW, ssaStrWChr, ssaStrWStringN:
          begin FUsesWide := True; FUsesStr := True; end;
        { ⚠️ DATEPART, DATEADD and DATEDIFF name their interval with a STRING, so
          the calendar family needs the string bank even where no program text
          says so. }
        ssaDateDecode, ssaDateSerial, ssaTimeSerial:
          FUsesDate := True;
        ssaDatePart, ssaDateAdd, ssaDateDiff:
          begin FUsesDate := True; FUsesStr := True; end;
        { ⚠️ NOT FUsesDate: naming a month needs no calendar arithmetic at all,
          only the table - so a program that just calls MONTHNAME does not carry
          seven helpers it never enters. }
        ssaDateName:
          begin FUsesDateName := True; FUsesStr := True; end;
        { DATE$/TIME$ read the clock AND decode it, so they need the import, the
          calendar and a string to put the answer in. }
        ssaDateStr:
          begin FUsesDateStr := True; FUsesDate := True;
                FUsesClock := True; FUsesStr := True; end;
        ssaPrintBool:
          begin FUsesBool := True; FUsesPrint := True; end;
        ssaStrErr:
          begin FUsesErrName := True; FUsesStr := True; end;
        { SADD hands back a RAW pointer, so it needs the raw-heap machinery as
          well as the string bank. }
        ssaStrSAdd:
          begin FUsesStr := True; FUsesGfx := True; FUsesHeap := True; end;
        ssaMathRnd:
          begin FUsesRnd := True; FUsesHeap := True; end;
        { ⚠️ RANDOMIZE without a seed is time-based, so it needs the clock the
          same way TIMER does - and it is nondeterministic on both sides, which
          is exactly why the guard seeds explicitly. }
        ssaRandomize:
          begin
            FUsesRnd := True; FUsesHeap := True;
            if not ((Ins.Src3.Kind = svkConstInt) and (Ins.Src3.ConstInt <> 0)) then
              FUsesClock := True;
          end;
        ssaArrayErase:
          FUsesArr := True;
        ssaDateValue, ssaIsDate:
          begin FUsesDateParse := True; FUsesDate := True; FUsesStr := True; end;
        ssaDateNow:
          FUsesClock := True;
        { ⚠️ FORCES the transcendentals: Power's last branch is exp(y * ln x), and a function
          that calls a helper the module does not have is not a runtime surprise - it is a
          module that does not load. }
        ssaPowFloat:
          begin FUsesPow := True; FUsesTrig := True; end;
        ssaMathSin, ssaMathCos, ssaMathTan, ssaMathAtn, ssaMathExp,
        ssaMathLog, ssaMathLog10, ssaMathLog2, ssaMathAsin, ssaMathAcos,
        ssaMathSinh, ssaMathCosh:
          FUsesTrig := True;
        ssaMathAtan2: FUsesAtan2 := True;
        ssaMathTanh:  FUsesXTrig[0] := True;
        ssaMathAsinh: FUsesXTrig[1] := True;
        ssaMathAcosh: FUsesXTrig[2] := True;
        ssaMathAtanh: FUsesXTrig[3] := True;
        ssaRecordNew:
          FUsesRec := True;
        { An ARRAY of UDT is an int-handle array whose elements are filled with
          fresh records, so it needs both the record heap AND the array
          descriptor - the helper reads the element count and the data base out
          of it. FUsesArr is set here rather than relied on: the DIM that would
          set it is a different opcode, and a helper that reads a descriptor
          that was never laid out reads whatever sits at that address. }
        ssaRecordNewArray:
          begin FUsesRec := True; FUsesRecArr := True; FUsesArr := True; end;
        ssaRecordLoadString, ssaRecordStoreString:
          begin FUsesRec := True; FUsesStr := True; end;
        { ⚠️ FUsesArr is forced along with the pointers, and not because a
          pointer program is bound to have an array: it is because the DEREF
          reads an array descriptor, and a descriptor that was never laid out is
          an address in the middle of something else. A program whose only
          pointers are "@obj.field" never reaches that arm - but nothing in the
          module can prove it, so the descriptors exist either way. }
        ssaRefLoadInt, ssaRefLoadFloat,
        ssaRefStoreInt, ssaRefStoreFloat:
          begin FUsesPtr := True; FUsesArr := True; end;
        ssaRefLoadString, ssaRefStoreString:
          begin FUsesPtr := True; FUsesArr := True; FUsesStr := True; end;
        ssaRefAddrField:
          begin FUsesPtr := True; FUsesArr := True; FUsesRec := True; end;
        { A procedure whose address is taken goes into the function table, and
          both halves of the pair raise the flag: a program can take an address
          it never calls, and it can call through a pointer that arrived in a
          UDT field or an array element without an ssaLoadProcAddr in sight. }
        ssaLoadProcAddr:
          begin
            FIndirect := True;
            if Ins.Src1.Kind = svkLabel then
            begin
              k := BlockOfLabel(Ins.Src1.LabelName);
              if (k >= 0) and (k < Length(FRegionOf)) then
                FAddrTaken[FRegionOf[k]] := True;
            end;
          end;
        ssaCallSubIndirect:
          FIndirect := True;
      end;
      NoteConst(Ins.Src1); NoteConst(Ins.Src2); NoteConst(Ins.Src3);
    end;
  end;
  FUsesHeap := FUsesStr or FUsesGfx or FUsesArr or FUsesRec;

  // Lay the literals out: one [i32 len][bytes] header each, 4-aligned so a
  // handle is always aligned the way an i32.load wants it.
  FConstBytes := '';
  SetLength(FConstAddr, Length(FConstId));
  Addr := STR_CONST_BASE;
  for k := 0 to High(FConstId) do
  begin
    FConstAddr[k] := Addr;
    S := AnsiString(SSAPoolGet(FConstId[k]));
    SetLength(FConstBytes, Length(FConstBytes) + 4 + Length(S));
    PLongWord(@FConstBytes[Length(FConstBytes) - 3 - Length(S)])^ := LongWord(Length(S));
    if Length(S) > 0 then
      Move(S[1], FConstBytes[Length(FConstBytes) - Length(S) + 1], Length(S));
    Inc(Addr, LongWord(4 + Length(S)));
    while (Addr and 3) <> 0 do
    begin
      FConstBytes := FConstBytes + #0;
      Inc(Addr);
    end;
  end;

  { The ARRAY DESCRIPTORS come next, one per declared array, and their addresses
    are CONSTANTS - the array index is a compile-time number in every operand
    that names one, so nothing has to be looked up at run time.
      +0 base   the element block (0 before DIM)
      +4 total  element count, which is also the bounds check
      +8 dims   how many dimensions are allocated (0 before DIM)
      +16.. (lb, size) per dimension
    ⭐ Descriptors, not one global per array, because LBOUND/UBOUND take the
    dimension in a REGISTER: the helper has to index the bounds at run time, and
    a global cannot be indexed. }
  SetLength(FArrDescOf, FProg.GetArrayCount);
  if FUsesArr then
    for k := 0 to FProg.GetArrayCount - 1 do
    begin
      FArrDescOf[k] := Addr;
      Inc(Addr, LongWord(16 + 8 * FProg.GetArray(k).DimCount));
      while (Addr and 3) <> 0 do Inc(Addr);
    end;

  { And after them the ARRAY ID TABLE, which exists only for pointers. Every
    other operand names an array by a compile-time index, so its descriptor is a
    constant; a pointer carries the id as a VALUE, and dereferencing it has to
    reach the descriptor at run time. The descriptors are not evenly spaced -
    their size follows the dimension count - so this cannot be an arithmetic
    step, it has to be a table. }
  FArrTabBytes := '';
  if FUsesPtr then
  begin
    FArrTabAddr := Addr;
    SetLength(FArrTabBytes, 4 * FProg.GetArrayCount);
    for k := 0 to FProg.GetArrayCount - 1 do
      PLongWord(@FArrTabBytes[1 + 4 * k])^ := FArrDescOf[k];
    Inc(Addr, LongWord(4 * FProg.GetArrayCount));
  end;
  FHeapBase := Addr;
end;

function TWasmBackend.ExtraOperands(Instr: TSSAInstruction): TSSAValueArray;
{ The operands an instruction READS without naming them in Src1..Src3.

  ⛔ ssaArrayDim is the whole reason this exists. Its bounds do not travel in its
  operand slots: they live in the program's ARRAY METADATA as bare register
  NUMBERS, and the instruction carries them only as PhiSources so that DCE will
  not delete the code that computes them. Every walk over operands is therefore
  blind to them - and the failure is silent, because the array still allocates,
  just with whatever happened to be in a local that was never given a value.
  So there is one place that knows about them, and the three walks ask it. }
var
  Info: TSSAArrayInfo;
  d: Integer;

  procedure Add(RT: TSSARegisterType; Idx: Integer);
  begin
    if Idx < 0 then Exit;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := MakeSSARegister(RT, Idx);
  end;

begin
  SetLength(Result, 0);
  { ⭐ The GRAPHICS statements use PhiSources as plain extra operand slots -
    LINE puts y2, the colour and the shape flag there because Src1..Src3 are
    already taken by x1, y1 and x2. They are real reads, so every walk has to
    see them or the registers holding them look dead. }
  if OpIn(Instr.OpCode, [ssaGfxLine, ssaGfxPset, ssaGfxPoint, ssaGraphicRGBA]) then
  begin
    for d := 0 to High(Instr.PhiSources) do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Instr.PhiSources[d].Value;
    end;
    Exit;
  end;
  if Instr.OpCode <> ssaArrayDim then Exit;
  if Instr.Src1.Kind <> svkArrayRef then Exit;
  if (Instr.Src1.ArrayIndex < 0) or (Instr.Src1.ArrayIndex >= FProg.GetArrayCount) then Exit;
  Info := FProg.GetArray(Instr.Src1.ArrayIndex);
  for d := 0 to Info.DimCount - 1 do
  begin
    if (d <= High(Info.Dimensions)) and (Info.Dimensions[d] = 0) and
       (d <= High(Info.DimRegisters)) then
      Add(Info.DimRegTypes[d], Info.DimRegisters[d]);
    if d <= High(Info.LowerBoundRegisters) then
      Add(srtInt, Info.LowerBoundRegisters[d]);
  end;
end;

function TWasmBackend.ConstAddrOf(const V: TSSAValue): LongWord;
var
  k: Integer;
begin
  for k := 0 to High(FConstId) do
    if FConstId[k] = V.ConstStringId then Exit(FConstAddr[k]);
  Result := EMPTY_STR;      // unreachable: ScanForPrint saw every operand
end;

procedure TWasmBackend.EmitPrintHelpers;
{ printInt(v: i64): format v the way TConsoleBehavior.FormatInt does, then hand
  the bytes to the sink.

    p := SCRATCH_END
    neg := v < 0
    u := neg ? 0 - v : v          (unsigned, so Low(Int64) works: its negation
                                   wraps to the right magnitude)
    if u = 0 then *--p := '0'
    else while u <> 0 do *--p := '0' + u mod 10; u := u div 10
    *--p := neg ? '-' : ' '
    write(p, SCRATCH_END - p)
    [CLASSIC only] write(CONST_SPACE, 1) }
var
  B: TWasmBuf;
  TVoidI64, TVoid: LongWord;
begin
  TVoidI64 := FModule.TypeIndex([wvtI64], []);
  TVoid := FModule.TypeIndex([], []);

  B := TWasmBuf.Create;
  try
    // locals: 1 = p (i32), 2 = u (i64), 3 = neg (i32)
    B.I32Const(SCRATCH_END); B.LocalSet(1);
    B.LocalGet(0); B.I64Const(0); B.Op(wopI64LtS); B.LocalSet(3);

    B.LocalGet(3);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(0); B.LocalGet(0); B.Op(wopI64Sub); B.LocalSet(2);
    B.Op(wopElse);
      B.LocalGet(0); B.LocalSet(2);
    B.EndOp;

    B.LocalGet(2); B.Op(wopI64Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
      B.LocalGet(1); B.I32Const(Ord('0')); B.OpMem(wopI32Store8, 0, 0);
    B.Op(wopElse);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(2); B.Op(wopI64Eqz); B.BrIf(1);
          B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
          B.LocalGet(1);
          B.LocalGet(2); B.I64Const(10); B.Op(wopI64RemU); B.Op(wopI32WrapI64);
          B.I32Const(Ord('0')); B.Op(wopI32Add);
          B.OpMem(wopI32Store8, 0, 0);
          B.LocalGet(2); B.I64Const(10); B.Op(wopI64DivU); B.LocalSet(2);
          B.Br(0);
        B.EndOp;
      B.EndOp;
    B.EndOp;

    // the one prefix character: '-' when negative, otherwise the space that
    // stands in for the sign
    B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
    B.LocalGet(3);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(1); B.I32Const(Ord('-')); B.OpMem(wopI32Store8, 0, 0);
    B.Op(wopElse);
      B.LocalGet(1); B.I32Const(Ord(' ')); B.OpMem(wopI32Store8, 0, 0);
    B.EndOp;

    B.LocalGet(1);
    B.I32Const(SCRATCH_END); B.LocalGet(1); B.Op(wopI32Sub);
    B.Call(WriteTarget);

    if (not FModern) or FQBLang then
    begin
      { Commodore and -lang qb put a space AFTER an integer; FreeBASIC does not.
        ⚠️ QB is not "Commodore" though - it gives the trailing space ONLY to an
        integer, while a Single or a Double keeps just the sign pad. That is why
        this reads (not Modern) OR QBLang rather than folding the two, and why
        the float printer below does NOT get the same test. }
      B.I32Const(CONST_SPACE); B.I32Const(1); B.Call(WriteTarget);
    end;

    FModule.AddFunction(TVoidI64, [wvtI32, wvtI64, wvtI32], B);
  finally
    B.Free;
  end;

  { printUInt(v: i64): the SAME digits, but the affixes are NOT the same. The FB
    manual's Print page, under "Differences from QB", says unsigned numbers are
    printed without a space before them - so FreeBASIC gives an unsigned neither
    the sign padding nor a trailing space, while Commodore gives it both.
    TConsoleBehavior.FormatUInt is the spec; getting this wrong would print
    something that looks right and is off by a space. }
  B := TWasmBuf.Create;
  try
    B.I32Const(SCRATCH_END); B.LocalSet(1);
    B.LocalGet(0); B.LocalSet(2);

    B.LocalGet(2); B.Op(wopI64Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
      B.LocalGet(1); B.I32Const(Ord('0')); B.OpMem(wopI32Store8, 0, 0);
    B.Op(wopElse);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(2); B.Op(wopI64Eqz); B.BrIf(1);
          B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
          B.LocalGet(1);
          B.LocalGet(2); B.I64Const(10); B.Op(wopI64RemU); B.Op(wopI32WrapI64);
          B.I32Const(Ord('0')); B.Op(wopI32Add);
          B.OpMem(wopI32Store8, 0, 0);
          B.LocalGet(2); B.I64Const(10); B.Op(wopI64DivU); B.LocalSet(2);
          B.Br(0);
        B.EndOp;
      B.EndOp;
    B.EndOp;

    if not FModern then
    begin
      // Commodore keeps the leading space an unsigned would get as a positive
      // number, and the trailing one. FreeBASIC gives it neither.
      B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
      B.LocalGet(1); B.I32Const(Ord(' ')); B.OpMem(wopI32Store8, 0, 0);
    end;

    B.LocalGet(1);
    B.I32Const(SCRATCH_END); B.LocalGet(1); B.Op(wopI32Sub);
    B.Call(WriteTarget);
    if not FModern then
    begin
      B.I32Const(CONST_SPACE); B.I32Const(1); B.Call(WriteTarget);
    end;

    FModule.AddFunction(TVoidI64, [wvtI32, wvtI64, wvtI32], B);
  finally
    B.Free;
  end;

  { printNl. ⛔ It writes DIRECTLY and then zeroes the column rather than going
    through emit: a newline does not advance the cursor by one character, it
    starts the line over, and that is the one place where "bytes written" and
    "columns moved" part company. }
  B := TWasmBuf.Create;
  try
    B.I32Const(CONST_NL); B.I32Const(1); B.Call(FWriteFunc);
    if FUsesCol then
    begin
      B.I32Const(0); B.GlobalSet(FColG);
    end;
    FModule.AddFunction(TVoid, [], B);
  finally
    B.Free;
  end;

  { emit(ptr, len): the byte sink, plus the column. ⭐ The rule is the
    interpreter's AdvancePrintCol and nothing more - add the characters, wrap at
    the line width - and the wrap is not cosmetic: the counter that never wrapped
    made a comma compute a zone from a column no screen has and break the record
    in half. An embedded newline inside a printed STRING does NOT reset it here,
    for the same reason it does not there: only a print NEWLINE does. }
  if FUsesCol then
  begin
    B := TWasmBuf.Create;
    try
      B.LocalGet(0); B.LocalGet(1); B.Call(FWriteFunc);
      B.GlobalGet(FColG); B.LocalGet(1); B.Op(wopI32Add);
      B.I32Const(SCREEN_COLS); B.Op(wopI32RemS);
      B.GlobalSet(FColG);
      FModule.AddFunction(FModule.TypeIndex([wvtI32, wvtI32], []), [], B);
    finally
      B.Free;
    end;
  end;
end;

function TWasmBackend.WriteTarget: LongWord;
{ Where a print helper sends its bytes: straight to the host, or through the
  column counter. ⭐ A program with no comma in a PRINT never pays for the
  counter - and there is no third answer, so the choice is made here once
  instead of at every call site. }
begin
  if FUsesCol then Result := FEmitFunc else Result := FWriteFunc;
end;

{ ---------------- the heap ----------------

  A BUMP allocator and nothing more: one global cursor, round up to 4, grow the
  memory when the cursor passes it. It never frees, and that is a stated v1
  limit rather than an oversight - a loop that builds strings consumes memory
  until the module hits the 4 GB ceiling. The alternative (a free list, or
  reference counting on the string handles) is a real piece of runtime, and it
  is worth writing only once there is a program that needs it. ⛔ What must NOT
  happen meanwhile is emitting something that runs and lies: running out of
  memory here is a trap, which is loud. }

procedure TWasmBackend.EmitHeapHelpers;
var
  B: TWasmBuf;
begin
  B := TWasmBuf.Create;
  try
    // alloc(n: i32) -> i32.  locals: 1 = the block, 2 = pages wanted
    B.GlobalGet(FHeapTop);
    B.LocalTee(1);
    B.LocalGet(0); B.Op(wopI32Add);
    B.I32Const(3); B.Op(wopI32Add);
    B.I32Const(-4); B.Op(wopI32And);
    B.GlobalSet(FHeapTop);

    B.GlobalGet(FHeapTop);
    B.I32Const(65535); B.Op(wopI32Add);
    B.I32Const(65536); B.Op(wopI32DivU);
    B.LocalTee(2);
    B.Op(wopMemorySize); B.U8(0);
    B.Op(wopI32GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(2);
      B.Op(wopMemorySize); B.U8(0);
      B.Op(wopI32Sub);
      B.Op(wopMemoryGrow); B.U8(0);
      B.Op(wopDrop);
    B.EndOp;

    B.LocalGet(1);
    FModule.AddFunction(FModule.TypeIndex([wvtI32], [wvtI32]), [wvtI32, wvtI32], B);
  finally
    B.Free;
  end;
end;

{ ---------------- strings ----------------

  A string VALUE is [i32 len][len bytes] in linear memory, and a string REGISTER
  holds its address. Three consequences, and the whole design is in them:

  1. ⭐ Handle 0 is the empty string for free. Linear memory starts zeroed and a
     fresh WASM local is 0, so a BASIC string variable that was never assigned
     reads as length 0 without a line of code. The four bytes at address 0 are
     reserved for that and never written.
  2. Strings are IMMUTABLE here: every operation allocates its result and
     ssaCopyString copies the handle. That is sound only because nothing mutates
     one in place - and two opcodes do (ssaStrAppendMapped grows its Dest,
     ssaStrMidAssign overwrites inside its buffer). They stay REFUSED, and the
     refusal names the reason. ⛔ Covering them by aliasing would produce a
     module that runs and prints the wrong string, which is the one outcome this
     backend may not have.
  3. Every rule below is the interpreter's, read out of SedaiBytecodeVM rather
     than reasoned about: LEFT's negative length, RIGHT's clamp, MID's two
     dialect-dependent rules, ASC of an empty string. They are what the
     differential compares. }

procedure TWasmBackend.EmitGfxHelpers;
var
  B: TWasmBuf;
  TPset, TLine: LongWord;
begin
  TPset := FModule.TypeIndex([wvtI32, wvtI32, wvtI32], []);
  TLine := FModule.TypeIndex([wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32], []);

  { gfxPset(x, y, colour): one pixel, CLIPPED.
    ⚠️ The colour goes into the framebuffer exactly as given - measured, not
    assumed: "Line ..., &HFF3366CC" leaves FF3366CC in the word. There is no
    palette lookup on this path and no channel swapping.
    ⛔ Clipping is not politeness: linear memory has no guard page, so a pixel
    off the right edge would silently land on the next ROW, and one off the
    bottom would corrupt whatever the allocator handed out after the screen. }
    B := TWasmBuf.Create;
    try
      B.LocalGet(0); B.I32Const(0); B.Op(wopI32GeS);
      B.LocalGet(1); B.I32Const(0); B.Op(wopI32GeS); B.Op(wopI32And);
      B.LocalGet(0); B.GlobalGet(FScrW); B.Op(wopI32LtS); B.Op(wopI32And);
      B.LocalGet(1); B.GlobalGet(FScrH); B.Op(wopI32LtS); B.Op(wopI32And);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.GlobalGet(FFbBase);
        B.LocalGet(1); B.GlobalGet(FScrW); B.Op(wopI32Mul);
        B.LocalGet(0); B.Op(wopI32Add);
        B.I32Const(4); B.Op(wopI32Mul);
        B.Op(wopI32Add);
        B.LocalGet(2);
        B.OpMem(wopI32Store, 2, 0);
      B.EndOp;
      FModule.AddFunction(TPset, [], B);
    finally
      B.Free;
    end;

    { gfxLine(x1, y1, x2, y2, colour, flag): flag 0 = line, 1 = box outline,
      2 = filled box. Bresenham, in the integer-only form the interpreter's
      primitive uses - the same algorithm, so the same pixels.
      ⭐ It draws THROUGH gfxPset, so clipping is decided in exactly one place;
      a line running off-screen is then clipped per pixel rather than being
      rejected whole, which is what the native side does. }
    B := TWasmBuf.Create;
    try
      // locals: 6=dx 7=dy 8=sx 9=sy 10=err 11=e2 12=i
      B.LocalGet(5); B.I32Const(2); B.Op(wopI32Eq);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        // filled box: scan rows between the two corners, in either order
        B.LocalGet(1); B.LocalSet(6);
        B.LocalGet(3); B.LocalSet(7);
        B.LocalGet(6); B.LocalGet(7); B.Op(wopI32GtS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(6); B.LocalSet(8);
          B.LocalGet(7); B.LocalSet(6);
          B.LocalGet(8); B.LocalSet(7);
        B.EndOp;
        B.LocalGet(0); B.LocalSet(8);
        B.LocalGet(2); B.LocalSet(9);
        B.LocalGet(8); B.LocalGet(9); B.Op(wopI32GtS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(8); B.LocalSet(10);
          B.LocalGet(9); B.LocalSet(8);
          B.LocalGet(10); B.LocalSet(9);
        B.EndOp;
        B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
          B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(6); B.LocalGet(7); B.Op(wopI32GtS); B.BrIf(1);
            B.LocalGet(8); B.LocalSet(12);
            B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
              B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
                B.LocalGet(12); B.LocalGet(9); B.Op(wopI32GtS); B.BrIf(1);
                B.LocalGet(12); B.LocalGet(6); B.LocalGet(4); B.Call(FGfxPsetFunc);
                B.LocalGet(12); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(12);
                B.Br(0);
              B.EndOp;
            B.EndOp;
            B.LocalGet(6); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(6);
            B.Br(0);
          B.EndOp;
        B.EndOp;
        B.Op(wopReturn);
      B.EndOp;

      B.LocalGet(5); B.I32Const(1); B.Op(wopI32Eq);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        // box outline: four edges, drawn as four straight lines
        B.LocalGet(0); B.LocalGet(1); B.LocalGet(2); B.LocalGet(1);
          B.LocalGet(4); B.I32Const(0); B.Call(FGfxLineFunc);
        B.LocalGet(0); B.LocalGet(3); B.LocalGet(2); B.LocalGet(3);
          B.LocalGet(4); B.I32Const(0); B.Call(FGfxLineFunc);
        B.LocalGet(0); B.LocalGet(1); B.LocalGet(0); B.LocalGet(3);
          B.LocalGet(4); B.I32Const(0); B.Call(FGfxLineFunc);
        B.LocalGet(2); B.LocalGet(1); B.LocalGet(2); B.LocalGet(3);
          B.LocalGet(4); B.I32Const(0); B.Call(FGfxLineFunc);
        B.Op(wopReturn);
      B.EndOp;

      // Bresenham
      B.LocalGet(2); B.LocalGet(0); B.Op(wopI32Sub); B.LocalSet(6);
      B.LocalGet(6); B.I32Const(0); B.Op(wopI32LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(0); B.LocalGet(6); B.Op(wopI32Sub); B.LocalSet(6);
        B.I32Const(-1); B.LocalSet(8);
      B.Op(wopElse);
        B.I32Const(1); B.LocalSet(8);
      B.EndOp;
      B.LocalGet(3); B.LocalGet(1); B.Op(wopI32Sub); B.LocalSet(7);
      B.LocalGet(7); B.I32Const(0); B.Op(wopI32LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(0); B.LocalGet(7); B.Op(wopI32Sub); B.LocalSet(7);
        B.I32Const(-1); B.LocalSet(9);
      B.Op(wopElse);
        B.I32Const(1); B.LocalSet(9);
      B.EndOp;
      B.I32Const(0); B.LocalGet(7); B.Op(wopI32Sub); B.LocalSet(7);   // dy is negative
      B.LocalGet(6); B.LocalGet(7); B.Op(wopI32Add); B.LocalSet(10);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(0); B.LocalGet(1); B.LocalGet(4); B.Call(FGfxPsetFunc);
          B.LocalGet(0); B.LocalGet(2); B.Op(wopI32Eq);
          B.LocalGet(1); B.LocalGet(3); B.Op(wopI32Eq); B.Op(wopI32And); B.BrIf(1);
          B.LocalGet(10); B.I32Const(2); B.Op(wopI32Mul); B.LocalSet(11);
          B.LocalGet(11); B.LocalGet(7); B.Op(wopI32GeS);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(10); B.LocalGet(7); B.Op(wopI32Add); B.LocalSet(10);
            B.LocalGet(0); B.LocalGet(8); B.Op(wopI32Add); B.LocalSet(0);
          B.EndOp;
          B.LocalGet(11); B.LocalGet(6); B.Op(wopI32LeS);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(10); B.LocalGet(6); B.Op(wopI32Add); B.LocalSet(10);
            B.LocalGet(1); B.LocalGet(9); B.Op(wopI32Add); B.LocalSet(1);
          B.EndOp;
          B.Br(0);
        B.EndOp;
      B.EndOp;
      FModule.AddFunction(TLine, [wvtI32, wvtI32, wvtI32, wvtI32, wvtI32,
                                  wvtI32, wvtI32], B);
    finally
      B.Free;
    end;
end;

procedure TWasmBackend.EmitStringHelpers;
var
  B: TWasmBuf;
  TNewStr, TCat, TSub, TCmp, TAsc, TChr, TRight, TMid, TPrint,
  TFromInt, TFill, TCase, TInstr, TPuDig, TPuFmt: LongWord;
begin
  TNewStr := FModule.TypeIndex([wvtI32], [wvtI32]);
  TCat    := FModule.TypeIndex([wvtI32, wvtI32], [wvtI32]);
  TSub    := FModule.TypeIndex([wvtI32, wvtI64, wvtI64], [wvtI32]);
  TCmp    := FModule.TypeIndex([wvtI32, wvtI32], [wvtI32]);
  TAsc    := FModule.TypeIndex([wvtI32], [wvtI64]);
  TChr    := FModule.TypeIndex([wvtI64], [wvtI32]);
  TRight  := FModule.TypeIndex([wvtI32, wvtI64], [wvtI32]);
  TMid    := FModule.TypeIndex([wvtI32, wvtI64, wvtI64], [wvtI32]);
  TPrint  := FModule.TypeIndex([wvtI32], []);
  TFromInt := FModule.TypeIndex([wvtI64], [wvtI32]);
  TFill   := FModule.TypeIndex([wvtI64, wvtI64], [wvtI32]);
  TCase   := FModule.TypeIndex([wvtI32, wvtI32], [wvtI32]);
  TInstr  := FModule.TypeIndex([wvtI32, wvtI32, wvtI64, wvtI32], [wvtI64]);
  TPuDig  := FModule.TypeIndex([wvtF64, wvtI32, wvtI64, wvtI32], []);
  TPuFmt  := FModule.TypeIndex([wvtI32, wvtF64, wvtI32, wvtI64], [wvtI32]);


  { strNew(len: i32) -> i32: an uninitialised header of that length, or the
    canonical empty string when the length is zero. }
  B := TWasmBuf.Create;
  try
    B.LocalGet(0); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(EMPTY_STR); B.LocalSet(1);
    B.Op(wopElse);
      B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add);
      B.Call(FAllocFunc);
      B.LocalTee(1);
      B.LocalGet(0);
      B.OpMem(wopI32Store, 2, 0);
    B.EndOp;
    B.LocalGet(1);
    FModule.AddFunction(TNewStr, [wvtI32], B);
  finally
    B.Free;
  end;

  { strCat(a, b) -> a + b. A zero length copy is legal and cannot trap, so the
    two copies need no guard. }
  B := TWasmBuf.Create;
  try
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(3);
    B.LocalGet(1); B.OpMem(wopI32Load, 2, 0); B.LocalSet(4);
    B.LocalGet(3); B.LocalGet(4); B.Op(wopI32Add);
    B.Call(FStrNewFunc); B.LocalSet(2);

    B.LocalGet(2); B.I32Const(4); B.Op(wopI32Add);
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add);
    B.LocalGet(3);
    B.MemoryCopy;

    B.LocalGet(2); B.I32Const(4); B.Op(wopI32Add); B.LocalGet(3); B.Op(wopI32Add);
    B.LocalGet(1); B.I32Const(4); B.Op(wopI32Add);
    B.LocalGet(4);
    B.MemoryCopy;

    B.LocalGet(2);
    FModule.AddFunction(TCat, [wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { strSub(s, start, cnt) -> the substring, with AssignSubstr's clamping:
    a count of zero or less is empty, a start below 1 is 1, and a count past the
    end is cut to what is left.
    ⚠️ The order differs from the Pascal on purpose. AssignSubstr asks
    "Start + Cnt - 1 > Length(S)", which on 64-bit registers holding absurd
    values can WRAP; asking "is start past the end" first makes every later
    subtraction bounded. For every value a program can actually produce the two
    agree - and for the ones it cannot, this one does not wrap into a false
    answer. }
  B := TWasmBuf.Create;
  try
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32U); B.LocalSet(3);

    B.LocalGet(1); B.I64Const(1); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(1); B.LocalSet(1);
    B.EndOp;

    B.LocalGet(2); B.I64Const(0); B.Op(wopI64LeS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(EMPTY_STR); B.Op(wopReturn);
    B.EndOp;

    B.LocalGet(1); B.LocalGet(3); B.Op(wopI64GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(EMPTY_STR); B.Op(wopReturn);
    B.EndOp;

    // avail = len - start + 1, which is now in 1..len
    B.LocalGet(3); B.LocalGet(1); B.Op(wopI64Sub); B.I64Const(1); B.Op(wopI64Add);
    B.LocalSet(6);
    B.LocalGet(2); B.LocalGet(6); B.Op(wopI64GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(6); B.LocalSet(2);
    B.EndOp;

    B.LocalGet(2); B.Op(wopI32WrapI64); B.LocalSet(4);
    B.LocalGet(4); B.Call(FStrNewFunc); B.LocalSet(5);
    B.LocalGet(5); B.I32Const(4); B.Op(wopI32Add);
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add);
      B.LocalGet(1); B.Op(wopI32WrapI64); B.Op(wopI32Add);
      B.I32Const(1); B.Op(wopI32Sub);
    B.LocalGet(4);
    B.MemoryCopy;
    B.LocalGet(5);
    FModule.AddFunction(TSub, [wvtI64, wvtI32, wvtI32, wvtI64], B);
  finally
    B.Free;
  end;

  { strCmp(a, b) -> -1 / 0 / 1. FPC compares AnsiStrings byte by byte as
    UNSIGNED characters and settles a tie by length, and the four BASIC string
    comparisons are built on that. }
  B := TWasmBuf.Create;
  try
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(2);
    B.LocalGet(1); B.OpMem(wopI32Load, 2, 0); B.LocalSet(3);
    B.LocalGet(2); B.LocalSet(4);
    B.LocalGet(3); B.LocalGet(4); B.Op(wopI32LtU);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(3); B.LocalSet(4);
    B.EndOp;
    B.I32Const(0); B.LocalSet(5);

    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(5); B.LocalGet(4); B.Op(wopI32GeU); B.BrIf(1);
        B.LocalGet(0); B.LocalGet(5); B.Op(wopI32Add);
          B.OpMem(wopI32Load8U, 0, 4); B.LocalSet(6);
        B.LocalGet(1); B.LocalGet(5); B.Op(wopI32Add);
          B.OpMem(wopI32Load8U, 0, 4); B.LocalSet(7);
        B.LocalGet(6); B.LocalGet(7); B.Op(wopI32Ne);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(6); B.LocalGet(7); B.Op(wopI32LtU);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.I32Const(-1); B.Op(wopReturn);
          B.Op(wopElse);
            B.I32Const(1); B.Op(wopReturn);
          B.EndOp;
        B.EndOp;
        B.LocalGet(5); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(5);
        B.Br(0);
      B.EndOp;
    B.EndOp;

    B.LocalGet(2); B.LocalGet(3); B.Op(wopI32Eq);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(2); B.LocalGet(3); B.Op(wopI32LtU);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(-1); B.Op(wopReturn);
    B.EndOp;
    B.I32Const(1);
    FModule.AddFunction(TCmp, [wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { strAsc(s) -> the first byte, and 0 for an empty string (bcStrAsc). }
  B := TWasmBuf.Create;
  try
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(0); B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(0); B.OpMem(wopI32Load8U, 0, 4); B.Op(wopI64ExtendI32U);
    FModule.AddFunction(TAsc, [], B);
  finally
    B.Free;
  end;

  { strChr(code) -> the one-character string; the code is taken AND $FF, as
    AssignChar's caller does. }
  B := TWasmBuf.Create;
  try
    B.I32Const(1); B.Call(FStrNewFunc); B.LocalTee(1);
    B.LocalGet(0); B.Op(wopI32WrapI64); B.I32Const(255); B.Op(wopI32And);
    B.OpMem(wopI32Store8, 0, 4);
    B.LocalGet(1);
    FModule.AddFunction(TChr, [wvtI32], B);
  finally
    B.Free;
  end;

  { strRight(s, n): a negative n is 0, an n past the end is the whole string,
    and the start follows from what is left (bcStrRight). }
  B := TWasmBuf.Create;
  try
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32U); B.LocalSet(2);
    B.LocalGet(1); B.I64Const(0); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(0); B.LocalSet(1);
    B.EndOp;
    B.LocalGet(1); B.LocalGet(2); B.Op(wopI64GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(2); B.LocalSet(1);
    B.EndOp;
    B.LocalGet(0);
    B.LocalGet(2); B.LocalGet(1); B.Op(wopI64Sub); B.I64Const(1); B.Op(wopI64Add);
    B.LocalGet(1);
    B.Call(FStrSubFunc);
    FModule.AddFunction(TRight, [wvtI64], B);
  finally
    B.Free;
  end;

  { strMid(s, start, cnt) - the one helper whose RULES DIFFER BY DIALECT, and
    the difference is not cosmetic (bcStrMid):
      FreeBASIC - a start below 1 yields an EMPTY string, and a NEGATIVE count
                  means "all the rest";
      Commodore - a start below 1 clamps to 1, and a negative count is 0.
    Both were found by programs that got the wrong answer, so both are baked in
    here at emit time rather than decided at run time. }
  B := TWasmBuf.Create;
  try
    if FModern then
    begin
      B.LocalGet(1); B.I64Const(1); B.Op(wopI64LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(EMPTY_STR); B.Op(wopReturn);
      B.EndOp;
      B.LocalGet(2); B.I64Const(0); B.Op(wopI64LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32U);
        B.LocalGet(1); B.Op(wopI64Sub); B.I64Const(1); B.Op(wopI64Add);
        B.LocalSet(2);
        B.LocalGet(2); B.I64Const(0); B.Op(wopI64LtS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I64Const(0); B.LocalSet(2);
        B.EndOp;
      B.EndOp;
    end
    else
    begin
      B.LocalGet(1); B.I64Const(1); B.Op(wopI64LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I64Const(1); B.LocalSet(1);
      B.EndOp;
      B.LocalGet(2); B.I64Const(0); B.Op(wopI64LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I64Const(0); B.LocalSet(2);
      B.EndOp;
    end;
    B.LocalGet(0); B.LocalGet(1); B.LocalGet(2);
    B.Call(FStrSubFunc);
    FModule.AddFunction(TMid, [], B);
  finally
    B.Free;
  end;

  { strFromInt(v: i64) -> a fresh string holding v in decimal.
    ⚠️ NOT the same text PRINT produces, and the difference is the whole point:
    PRINT pads the sign column with a space, Str does NOT ("-7" is two
    characters, "42" is two). The digits are generated exactly as printInt does -
    backwards into the scratch, unsigned so that Low(Int64) negates to the right
    magnitude - and then copied into a string instead of the sink. }
  B := TWasmBuf.Create;
  try
    // locals: 1 = p (i32), 2 = u (i64), 3 = neg (i32), 4 = len (i32), 5 = h (i32)
    B.I32Const(SCRATCH_END); B.LocalSet(1);
    B.LocalGet(0); B.I64Const(0); B.Op(wopI64LtS); B.LocalSet(3);

    B.LocalGet(3);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(0); B.LocalGet(0); B.Op(wopI64Sub); B.LocalSet(2);
    B.Op(wopElse);
      B.LocalGet(0); B.LocalSet(2);
    B.EndOp;

    B.LocalGet(2); B.Op(wopI64Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
      B.LocalGet(1); B.I32Const(Ord('0')); B.OpMem(wopI32Store8, 0, 0);
    B.Op(wopElse);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(2); B.Op(wopI64Eqz); B.BrIf(1);
          B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
          B.LocalGet(1);
          B.LocalGet(2); B.I64Const(10); B.Op(wopI64RemU); B.Op(wopI32WrapI64);
          B.I32Const(Ord('0')); B.Op(wopI32Add);
          B.OpMem(wopI32Store8, 0, 0);
          B.LocalGet(2); B.I64Const(10); B.Op(wopI64DivU); B.LocalSet(2);
          B.Br(0);
        B.EndOp;
      B.EndOp;
    B.EndOp;

    // the minus sign, and ONLY when negative - no space pad
    B.LocalGet(3);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
      B.LocalGet(1); B.I32Const(Ord('-')); B.OpMem(wopI32Store8, 0, 0);
    B.EndOp;

    B.I32Const(SCRATCH_END); B.LocalGet(1); B.Op(wopI32Sub); B.LocalSet(4);
    B.LocalGet(4); B.Call(FStrNewFunc); B.LocalSet(5);
    B.LocalGet(5); B.I32Const(4); B.Op(wopI32Add);
    B.LocalGet(1);
    B.LocalGet(4);
    B.MemoryCopy;
    B.LocalGet(5);
    FModule.AddFunction(TFromInt, [wvtI32, wvtI64, wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { strFill(n: i64, ch: i64) -> a string of n copies of the character.
    SPACE$(n) is this with ch = 32, STRING$(n, c) is this with ch = c AND $FF -
    the same helper, because they are the same operation and the interpreter
    implements both with StringOfChar. A negative count is 0, not an error. }
  B := TWasmBuf.Create;
  try
    // locals: 2 = count/cursor (i32), 3 = handle (i32), 4 = end (i32)
    B.LocalGet(0); B.I64Const(0); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(0); B.LocalSet(0);
    B.EndOp;
    B.LocalGet(0); B.Op(wopI32WrapI64); B.LocalTee(2);
    B.Call(FStrNewFunc); B.LocalSet(3);
    B.LocalGet(3); B.I32Const(4); B.Op(wopI32Add); B.LocalTee(4);
    B.LocalGet(2); B.Op(wopI32Add); B.LocalSet(2);          // 2 = one past the end
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(4); B.LocalGet(2); B.Op(wopI32GeU); B.BrIf(1);
        B.LocalGet(4);
        B.LocalGet(1); B.Op(wopI32WrapI64); B.I32Const(255); B.Op(wopI32And);
        B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(4); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(4);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    B.LocalGet(3);
    FModule.AddFunction(TFill, [wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { strCase(s, up) -> a new string with the ASCII letters folded.
    ⚠️ ASCII ONLY, and deliberately so: this mirrors FPC's UpperCase/LowerCase,
    which the interpreter calls and which are themselves ASCII-only. Folding
    Latin-1 or UTF-8 here would make the module DISAGREE with sb, and agreeing
    with sb is the whole contract. }
  B := TWasmBuf.Create;
  try
    // locals: 2 = src cursor, 3 = handle, 4 = dst cursor, 5 = end, 6 = byte
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalTee(5);
    B.Call(FStrNewFunc); B.LocalSet(3);
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(2);
    B.LocalGet(3); B.I32Const(4); B.Op(wopI32Add); B.LocalTee(4);
    B.LocalGet(5); B.Op(wopI32Add); B.LocalSet(5);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(4); B.LocalGet(5); B.Op(wopI32GeU); B.BrIf(1);
        B.LocalGet(2); B.OpMem(wopI32Load8U, 0, 0); B.LocalSet(6);
        B.LocalGet(4);
        B.LocalGet(1);
        B.BlockStart(wopIf, WASM_TYPE_I32);
          // 'a'..'z' -> upper
          B.LocalGet(6); B.I32Const(Ord('a')); B.Op(wopI32GeU);
          B.LocalGet(6); B.I32Const(Ord('z')); B.Op(wopI32LeU);
          B.Op(wopI32And);
          B.BlockStart(wopIf, WASM_TYPE_I32);
            B.LocalGet(6); B.I32Const(32); B.Op(wopI32Sub);
          B.Op(wopElse);
            B.LocalGet(6);
          B.EndOp;
        B.Op(wopElse);
          // 'A'..'Z' -> lower
          B.LocalGet(6); B.I32Const(Ord('A')); B.Op(wopI32GeU);
          B.LocalGet(6); B.I32Const(Ord('Z')); B.Op(wopI32LeU);
          B.Op(wopI32And);
          B.BlockStart(wopIf, WASM_TYPE_I32);
            B.LocalGet(6); B.I32Const(32); B.Op(wopI32Add);
          B.Op(wopElse);
            B.LocalGet(6);
          B.EndOp;
        B.EndOp;
        B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(2); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(2);
        B.LocalGet(4); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(4);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    B.LocalGet(3);
    FModule.AddFunction(TCase, [wvtI32, wvtI32, wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { strTrim(s, mode) -> a new string with the padding removed. mode bit 0 trims the LEFT, bit 1 the
    RIGHT, so one helper serves LTRIM, RTRIM and TRIM - the same arrangement UCASE and LCASE share.
    ⚠️ It removes every byte <= 32, not just the space, because that is what FPC's Trim/TrimLeft/
    TrimRight do and the interpreter calls those. Trimming only #32 would make the module disagree
    with sb on a string holding a tab or a newline, which is exactly the kind of quiet difference this
    backend exists to avoid. }
  B := TWasmBuf.Create;
  try
    // locals: 2 = len, 3 = first, 4 = last (exclusive), 5 = base, 6 = handle, 7 = dst
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(2);
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(5);
    B.I32Const(0); B.LocalSet(3);
    B.LocalGet(2); B.LocalSet(4);
    // left: advance while the byte is padding and there is any left
    B.LocalGet(1); B.I32Const(1); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(3); B.LocalGet(4); B.Op(wopI32GeU); B.BrIf(1);
          B.LocalGet(5); B.LocalGet(3); B.Op(wopI32Add);
          B.OpMem(wopI32Load8U, 0, 0); B.I32Const(32); B.Op(wopI32GtU); B.BrIf(1);
          B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(3);
          B.Br(0);
        B.EndOp;
      B.EndOp;
    B.EndOp;
    // right: retreat while the byte before the end is padding
    B.LocalGet(1); B.I32Const(2); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(4); B.LocalGet(3); B.Op(wopI32LeU); B.BrIf(1);
          B.LocalGet(5); B.LocalGet(4); B.Op(wopI32Add); B.I32Const(1); B.Op(wopI32Sub);
          B.OpMem(wopI32Load8U, 0, 0); B.I32Const(32); B.Op(wopI32GtU); B.BrIf(1);
          B.LocalGet(4); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(4);
          B.Br(0);
        B.EndOp;
      B.EndOp;
    B.EndOp;
    // copy what is left
    B.LocalGet(4); B.LocalGet(3); B.Op(wopI32Sub); B.LocalTee(2);
    B.Call(FStrNewFunc); B.LocalTee(6);
    B.I32Const(4); B.Op(wopI32Add); B.LocalSet(7);
    B.LocalGet(5); B.LocalGet(3); B.Op(wopI32Add); B.LocalSet(5);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(2); B.Op(wopI32Eqz); B.BrIf(1);
        B.LocalGet(7); B.LocalGet(5); B.OpMem(wopI32Load8U, 0, 0); B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(7); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(7);
        B.LocalGet(5); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(5);
        B.LocalGet(2); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(2);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    B.LocalGet(6);
    FModule.AddFunction(TCase, [wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { strFind(hay, pat, start, mode) -> 1-based position, 0 if absent.
    mode bit 0 scans BACKWARDS, bit 1 makes pat a CHARACTER SET rather than a
    substring - so the four combinations are INSTR, INSTRREV, INSTR(...Any) and
    INSTRREV(...Any), which is every search BASIC has.

    ⭐ ONE function for the four because they differ in two independent bits and
    nothing else. Four bodies would be four places for the empty-pattern rule
    and the 1-based answer to drift apart, and the interpreter has already shown
    what that costs elsewhere.

    Mirrors bcStrInstr and its three siblings: a start below 1 clamps to 1, and
    the result is an absolute position in the haystack.
    ⭐ The EMPTY pattern is not an edge case to shrug at: Pascal's Pos returns 0
    for it, so an empty needle finds nothing - and a loop written around
    "Instr(...) > 0" would spin forever if this answered 1. The set forms agree
    (an empty set matches nothing), which is what fbc returns.
    ⚠️ START is honoured by the FORWARD forms only, because the interpreter's
    INSTRREV ignores its own - matching sb is the contract, not matching fbc. }
  B := TWasmBuf.Create;
  try
    // params 0=hay 1=pat 2=start 3=mode; locals 4=hlen 5=plen 6=eff 7=i 8=j
    //                                            9=hbase 10=pbase 11=step
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(4);
    B.LocalGet(1); B.OpMem(wopI32Load, 2, 0); B.LocalSet(5);
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(9);
    B.LocalGet(1); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(10);
    // an empty pattern never matches, whichever way it is read
    B.LocalGet(5); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(0); B.Op(wopReturn);
    B.EndOp;
    // a set matches ONE character; a substring matches its own length
    B.LocalGet(3); B.I32Const(2); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_TYPE_I32);
      B.I32Const(1);
    B.Op(wopElse);
      B.LocalGet(5);
    B.EndOp;
    B.LocalSet(6);

    B.LocalGet(3); B.I32Const(1); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      // backwards: the last position where the pattern still fits, walking down
      B.LocalGet(4); B.LocalGet(6); B.Op(wopI32Sub); B.LocalSet(7);
      B.I32Const(-1); B.LocalSet(11);
    B.Op(wopElse);
      B.LocalGet(2); B.I64Const(1); B.Op(wopI64LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I64Const(1); B.LocalSet(2);
      B.EndOp;
      B.LocalGet(2); B.Op(wopI32WrapI64); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(7);
      B.I32Const(1); B.LocalSet(11);
    B.EndOp;

    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);         // A: not found
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);        // L: for each position i
        // off either end - one test serves both directions
        B.LocalGet(7); B.I32Const(0); B.Op(wopI32LtS); B.BrIf(1);          // -> A
        B.LocalGet(7); B.LocalGet(6); B.Op(wopI32Add);
        B.LocalGet(4); B.Op(wopI32GtS); B.BrIf(1);                         // -> A
        B.I32Const(0); B.LocalSet(8);
        B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);     // M: no match at this i
          B.LocalGet(3); B.I32Const(2); B.Op(wopI32And);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            { THE SET FORM: one character against every member. The quantifier is
              the opposite of the substring form's - EXISTS a j that matches,
              where the other needs ALL of them - which is why the two are
              written out rather than folded into one loop with a flag. }
            B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);  // K
              B.LocalGet(8); B.LocalGet(5); B.Op(wopI32GeS); B.BrIf(1);    // -> M
              B.LocalGet(9); B.LocalGet(7); B.Op(wopI32Add);
              B.OpMem(wopI32Load8U, 0, 0);
              B.LocalGet(10); B.LocalGet(8); B.Op(wopI32Add);
              B.OpMem(wopI32Load8U, 0, 0);
              B.Op(wopI32Eq);
              B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
                B.LocalGet(7); B.I32Const(1); B.Op(wopI32Add); B.Op(wopI64ExtendI32S);
                B.Op(wopReturn);
              B.EndOp;
              B.LocalGet(8); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(8);
              B.Br(0);                                    // -> K
            B.EndOp;                                      // K
          B.Op(wopElse);
            B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);  // K: compare byte by byte
              // every byte matched: the answer is this i, 1-based
              B.LocalGet(8); B.LocalGet(5); B.Op(wopI32GeS);
              B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
                B.LocalGet(7); B.I32Const(1); B.Op(wopI32Add); B.Op(wopI64ExtendI32S);
                B.Op(wopReturn);
              B.EndOp;
              B.LocalGet(9); B.LocalGet(7); B.Op(wopI32Add); B.LocalGet(8); B.Op(wopI32Add);
              B.OpMem(wopI32Load8U, 0, 0);
              B.LocalGet(10); B.LocalGet(8); B.Op(wopI32Add);
              B.OpMem(wopI32Load8U, 0, 0);
              B.Op(wopI32Ne); B.BrIf(1);                  // -> M, i.e. try the next i
              B.LocalGet(8); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(8);
              B.Br(0);                                    // -> K
            B.EndOp;                                      // K
          B.EndOp;
        B.EndOp;                                          // M
        { ⛔ The mismatch exit MUST land here, past the inner loop, so that i is
          advanced before the next attempt. Branching straight back to L instead
          would retry the same i for ever - and a hang is the one failure a
          differential net cannot report, because it never gets to compare. }
        B.LocalGet(7); B.LocalGet(11); B.Op(wopI32Add); B.LocalSet(7);
        B.Br(0);                                          // -> L
      B.EndOp;                                            // L
    B.EndOp;                                              // A
    B.I64Const(0);
    FModule.AddFunction(TInstr, [wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32,
                                 wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { puDigits(v: f64, isInt: i32, iv: i64, decDigits: i32): the value's digits,
    MOST significant first, into PU_DIG - already rounded to decDigits places -
    with PU_NINT and PU_NDEC saying how they split.

    ⭐ It reuses fltDec, which produces the EXACT decimal expansion of a double
    (the value is M x 2^E, so its digits are an integer's), and that is what
    makes this tractable: rounding to a fixed number of decimals becomes
    arithmetic on DIGITS, with no floating point anywhere and nothing to drift.
    ⚠️ Half-AWAY-from-zero, which is what FPC's Format('%.*f') does and NOT what
    PRINT does - measured: 0.125 -> 0.13, 0.25 -> 0.3, 2.5 -> 3. Half-to-even
    would answer 0.12, 0.2 and 2 and be wrong three times out of three. }
  if FUsesPU then
  begin
  B := TWasmBuf.Create;
  try
    // locals: 4 = len, 5 = frac, 6 = i, 7 = n, 8 = carry, 9 = keep, 10 = t (i64)
    B.LocalGet(1);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      { the exact integer path: its digits are its own, and a fractional field
        is padded with zeros - a LongInt past 2^53 must print every digit, not
        the digits of the double nearest to it }
      B.LocalGet(2); B.I64Const(0); B.Op(wopI64LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I64Const(0); B.LocalGet(2); B.Op(wopI64Sub); B.LocalSet(2);
      B.EndOp;
      B.I32Const(0); B.LocalSet(6);
      B.LocalGet(2); B.Op(wopI64Eqz);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(FLT_DEC); B.I32Const(0); B.OpMem(wopI32Store8, 0, 0);
        B.I32Const(1); B.LocalSet(6);
      B.Op(wopElse);
        B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
          B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(2); B.Op(wopI64Eqz); B.BrIf(1);
            B.I32Const(FLT_DEC); B.LocalGet(6); B.Op(wopI32Add);
            B.LocalGet(2); B.I64Const(10); B.Op(wopI64RemU); B.Op(wopI32WrapI64);
            B.OpMem(wopI32Store8, 0, 0);
            B.LocalGet(2); B.I64Const(10); B.Op(wopI64DivU); B.LocalSet(2);
            B.LocalGet(6); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(6);
            B.Br(0);
          B.EndOp;
        B.EndOp;
      B.EndOp;
      B.LocalGet(6); B.LocalSet(4);
      B.I32Const(0); B.LocalSet(5);
    B.Op(wopElse);
      B.LocalGet(0); B.Call(FFltDecFunc);
      B.I32Const(FLT_LEN); B.OpMem(wopI32Load, 2, 0); B.LocalSet(4);
      B.I32Const(FLT_FRAC); B.OpMem(wopI32Load, 2, 0); B.LocalSet(5);
    B.EndOp;

    { Round to decDigits fractional places. FLT_DEC is least-significant-first,
      so the digits to drop are the LOW ones: keep = len - (frac - want). }
    B.LocalGet(5); B.LocalGet(3); B.Op(wopI32Sub); B.LocalSet(7);   // to drop
    B.LocalGet(7); B.I32Const(0); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.LocalSet(7);
    B.EndOp;
    B.I32Const(0); B.LocalSet(8);
    B.LocalGet(7); B.I32Const(0); B.Op(wopI32GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      // the first dropped digit decides, and 5 rounds AWAY from zero
      B.LocalGet(7); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(6);
      B.LocalGet(6); B.LocalGet(4); B.Op(wopI32LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(FLT_DEC); B.LocalGet(6); B.Op(wopI32Add);
        B.OpMem(wopI32Load8U, 0, 0);
        B.I32Const(5); B.Op(wopI32GeS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(1); B.LocalSet(8);
        B.EndOp;
      B.EndOp;
    B.EndOp;
    B.LocalGet(4); B.LocalGet(7); B.Op(wopI32Sub); B.LocalSet(9);   // kept
    B.LocalGet(9); B.I32Const(0); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.LocalSet(9);
    B.EndOp;

    { Copy the kept digits out, MOST significant first, applying the carry as we
      go. ⚠️ The carry can run off the top (999.95 -> 1000.0), and then there is
      one more digit than was kept - which the integer count has to see. }
    B.I32Const(0); B.LocalSet(6);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(6); B.LocalGet(9); B.Op(wopI32GeS); B.BrIf(1);
        B.I32Const(FLT_DEC); B.LocalGet(7); B.Op(wopI32Add);
          B.LocalGet(6); B.Op(wopI32Add);
        B.OpMem(wopI32Load8U, 0, 0);
        B.LocalGet(8); B.Op(wopI32Add); B.LocalSet(10);
        B.LocalGet(10); B.I32Const(10); B.Op(wopI32GeS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(10); B.I32Const(10); B.Op(wopI32Sub); B.LocalSet(10);
          B.I32Const(1); B.LocalSet(8);
        B.Op(wopElse);
          B.I32Const(0); B.LocalSet(8);
        B.EndOp;
        B.I32Const(FLT_DEC + 1024); B.LocalGet(6); B.Op(wopI32Add);
        B.LocalGet(10); B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(6); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(6);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    B.LocalGet(8);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(FLT_DEC + 1024); B.LocalGet(9); B.Op(wopI32Add);
      B.I32Const(1); B.OpMem(wopI32Store8, 0, 0);
      B.LocalGet(9); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(9);
    B.EndOp;

    // reverse into PU_DIG, most significant first
    B.I32Const(0); B.LocalSet(6);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(6); B.LocalGet(9); B.Op(wopI32GeS); B.BrIf(1);
        B.I32Const(PU_DIG); B.LocalGet(6); B.Op(wopI32Add);
        B.I32Const(FLT_DEC + 1024); B.LocalGet(9); B.Op(wopI32Add);
          B.LocalGet(6); B.Op(wopI32Sub); B.I32Const(1); B.Op(wopI32Sub);
        B.OpMem(wopI32Load8U, 0, 0);
        B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(6); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(6);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    { The split. A value below 1 has NO integer digits of its own, so the
      integer part is a written zero and the fraction is padded on the left -
      0.001 with three decimals is "0" and "001", not one digit and two. }
    B.LocalGet(9); B.LocalGet(3); B.Op(wopI32Sub); B.LocalSet(6);   // int digits
    B.LocalGet(6); B.I32Const(0); B.Op(wopI32GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(PU_NINT); B.LocalGet(6); B.OpMem(wopI32Store, 2, 0);
    B.Op(wopElse);
      // shift right to make room for the leading zero and any padding zeros
      B.I32Const(0); B.LocalGet(6); B.Op(wopI32Sub); B.I32Const(1); B.Op(wopI32Add);
      B.LocalSet(7);
      B.I32Const(PU_DIG); B.LocalGet(7); B.Op(wopI32Add);
      B.I32Const(PU_DIG);
      B.LocalGet(9);
      B.MemoryCopy;
      B.I32Const(PU_DIG); B.I32Const(0); B.LocalGet(7); B.MemoryFill;
      B.LocalGet(9); B.LocalGet(7); B.Op(wopI32Add); B.LocalSet(9);
      B.I32Const(PU_NINT); B.I32Const(1); B.OpMem(wopI32Store, 2, 0);
    B.EndOp;
    B.I32Const(PU_NDEC); B.LocalGet(3); B.OpMem(wopI32Store, 2, 0);
    FModule.AddFunction(TPuDig, [wvtI32, wvtI32, wvtI32, wvtI32,
                                 wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;
  end;

  { puFmt(fmt, v, isInt, iv) -> the rendered field as a string.
    The rules are FreeBASIC's and every one of them was measured against fbc
    natively before being written here (198 format/value pairs), so this is a
    transcription of a known semantics rather than a second guess at it.

    ⭐ The field is built from a cursor in the MIDDLE: digits grow forwards, and
    the things that attach in front - the floating '$', the sign, the padding,
    the overflow marker, the fixed '$' - grow backwards. Prefixing then costs
    nothing, where composing left to right would mean shifting the whole field
    every time something new turned out to belong in front of it. }
  if FUsesPU then
  begin
  B := TWasmBuf.Create;
  try
    { locals: 4=p 5=flen 6=c 7=intDig 8=decDig 9=caret 10=commas 11=flags
              12=neg 13=h 14=t 15=i 16=nint 17=ndec 18=width 19=sh 20=ex
              21=tmp 22=digits 23=mant(f64) }
    B.I32Const(0); B.LocalSet(7);  B.I32Const(0); B.LocalSet(8);
    B.I32Const(0); B.LocalSet(9);  B.I32Const(0); B.LocalSet(10);
    B.I32Const(0); B.LocalSet(11);
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(5);
    B.I32Const(0); B.LocalSet(4);

    // ---- parse the picture ----
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(4); B.LocalGet(5); B.Op(wopI32GeS); B.BrIf(1);
        B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add); B.LocalGet(4); B.Op(wopI32Add);
        B.OpMem(wopI32Load8U, 0, 0); B.LocalSet(6);

        B.LocalGet(6); B.I32Const(Ord('#')); B.Op(wopI32Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(7); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(7);
        B.EndOp;
        B.LocalGet(6); B.I32Const(Ord(',')); B.Op(wopI32Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(10); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(10);
          B.LocalGet(11); B.I32Const(32); B.Op(wopI32Or); B.LocalSet(11);
        B.EndOp;
        // '$' - two of them is the FLOATING dollar and eats both characters
        B.LocalGet(6); B.I32Const(Ord('$')); B.Op(wopI32Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(4); B.I32Const(1); B.Op(wopI32Add); B.LocalGet(5); B.Op(wopI32LtS);
          B.LocalGet(0); B.I32Const(5); B.Op(wopI32Add); B.LocalGet(4); B.Op(wopI32Add);
          B.OpMem(wopI32Load8U, 0, 0); B.I32Const(Ord('$')); B.Op(wopI32Eq);
          B.Op(wopI32And);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(11); B.I32Const(16); B.Op(wopI32Or); B.LocalSet(11);
            B.LocalGet(4); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(4);
          B.Op(wopElse);
            B.LocalGet(11); B.I32Const(8); B.Op(wopI32Or); B.LocalSet(11);
          B.EndOp;
        B.EndOp;
        // a '+' in FIRST position leads, anywhere else it trails
        B.LocalGet(6); B.I32Const(Ord('+')); B.Op(wopI32Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(4); B.Op(wopI32Eqz);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(11); B.I32Const(1); B.Op(wopI32Or); B.LocalSet(11);
          B.Op(wopElse);
            B.LocalGet(11); B.I32Const(2); B.Op(wopI32Or); B.LocalSet(11);
          B.EndOp;
        B.EndOp;
        { ⛔ PARKED BACKEND, AND THE RULE HERE IS INCOMPLETE - read this before
          porting anything. A '-' at i > 0 sets the trailing-sign flag, which is
          RIGHT. A '-' at i = 0 sets nothing, so it simply VANISHES, which is
          wrong: in FreeBASIC a leading '-' is ORDINARY TEXT and must print.
          ⛔⛔ Do NOT port the version this file's note used to point at. On 12 Aug
          2026 the native side made a leading '-' a SIGN POSITION and that was
          REFUTED by fbc 1.10.1 the next day:
            Using "-##.#";  1.5 -> "- 1.5"    the '-' is text
            Using "x#.#";  -1.5 -> "x%-1.5"   'x' is text too - '-' is not special
          The native fix is not a flag at all: IsNumFieldStart refuses to OPEN a
          field on '-', so the character falls to the literal-text arm of the
          field splitter. That is the shape to port - and it needs the splitter,
          which is the real work here, not another bit in local 11.
          See SedaiSSA.pas EmitUsingFields and job/tests/bas/print_using_lead_sign.bas
          (44 lines, every one measured against fbc). }
        B.LocalGet(6); B.I32Const(Ord('-')); B.Op(wopI32Eq);
        B.LocalGet(4); B.I32Const(0); B.Op(wopI32GtS); B.Op(wopI32And);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(11); B.I32Const(4); B.Op(wopI32Or); B.LocalSet(11);
        B.EndOp;
        // '.' then a run of '#'
        B.LocalGet(6); B.I32Const(Ord('.')); B.Op(wopI32Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(11); B.I32Const(64); B.Op(wopI32Or); B.LocalSet(11);
          B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
            B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
              B.LocalGet(4); B.I32Const(1); B.Op(wopI32Add); B.LocalGet(5); B.Op(wopI32GeS);
              B.BrIf(1);
              B.LocalGet(0); B.I32Const(5); B.Op(wopI32Add); B.LocalGet(4); B.Op(wopI32Add);
              B.OpMem(wopI32Load8U, 0, 0); B.I32Const(Ord('#')); B.Op(wopI32Ne); B.BrIf(1);
              B.LocalGet(8); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(8);
              B.LocalGet(4); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(4);
              B.Br(0);
            B.EndOp;
          B.EndOp;
        B.EndOp;
        { '^' - FIVE is the ceiling, and past it a caret is literal text.
          ⛔ Only skip what was CONSUMED: once the cap is reached the inner loop
          takes nothing, and moving the cursor back would spin for ever. That
          exact hang was written and removed on the native side. }
        B.LocalGet(6); B.I32Const(Ord('^')); B.Op(wopI32Eq);
        B.LocalGet(9); B.I32Const(5); B.Op(wopI32LtS); B.Op(wopI32And);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(9); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(9);
        B.EndOp;

        B.LocalGet(4); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(4);
        B.Br(0);
      B.EndOp;
    B.EndOp;

    // ---- the sign of the value ----
    B.I32Const(0); B.LocalSet(12);
    B.LocalGet(2);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(3); B.I64Const(0); B.Op(wopI64LtS); B.LocalSet(12);
    B.Op(wopElse);
      B.LocalGet(1); B.F64Const(0); B.Op(wopF64Lt); B.LocalSet(12);
    B.EndOp;

    { ---- exponential ----
      ⭐ The mantissa carries one FEWER significant integer digit than the field
      has '#', because the first position belongs to the sign - so "#.##^^^^"
      and "##.##^^^^" print the same number as 0.12E+04 and 1.23E+03.
      ⚠️ With no decimal point nothing is held back and at least one digit must
      remain: "#^^^^" prints 5E+00. }
    B.LocalGet(9); B.I32Const(4); B.Op(wopI32GeS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(7); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(19);
      B.LocalGet(8); B.Op(wopI32Eqz);
      B.LocalGet(11); B.I32Const(64); B.Op(wopI32And); B.Op(wopI32Eqz); B.Op(wopI32And);
      B.LocalGet(19); B.I32Const(1); B.Op(wopI32LtS); B.Op(wopI32And);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(1); B.LocalSet(19);
      B.EndOp;
      B.LocalGet(19); B.I32Const(0); B.Op(wopI32LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(0); B.LocalSet(19);
      B.EndOp;
      // |v| as an f64, and the exponent found by shifting it into its window
      B.LocalGet(2);
      B.BlockStart(wopIf, WASM_TYPE_F64);
        B.LocalGet(3); B.Op(wopF64ConvertI64S);
      B.Op(wopElse);
        B.LocalGet(1);
      B.EndOp;
      B.Op(wopF64Abs); B.LocalSet(23);
      B.I32Const(0); B.LocalSet(20);
      B.LocalGet(23); B.F64Const(0); B.Op(wopF64Ne);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        // hi = 10^sh, lo = 10^(sh-1), both built by multiplication
        B.F64Const(1); B.LocalSet(1);
        B.I32Const(0); B.LocalSet(15);
        B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
          B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(15); B.LocalGet(19); B.Op(wopI32GeS); B.BrIf(1);
            B.LocalGet(1); B.F64Const(10); B.Op(wopF64Mul); B.LocalSet(1);
            B.LocalGet(15); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(15);
            B.Br(0);
          B.EndOp;
        B.EndOp;
        B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
          B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(23); B.LocalGet(1); B.Op(wopF64Lt); B.BrIf(1);
            B.LocalGet(23); B.F64Const(10); B.Op(wopF64Div); B.LocalSet(23);
            B.LocalGet(20); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(20);
            B.Br(0);
          B.EndOp;
        B.EndOp;
        B.LocalGet(1); B.F64Const(10); B.Op(wopF64Div); B.LocalSet(1);
        B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
          B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(23); B.LocalGet(1); B.Op(wopF64Ge); B.BrIf(1);
            B.LocalGet(23); B.F64Const(10); B.Op(wopF64Mul); B.LocalSet(23);
            B.LocalGet(20); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(20);
            B.Br(0);
          B.EndOp;
        B.EndOp;
      B.EndOp;
      B.LocalGet(23); B.I32Const(0); B.I64Const(0); B.LocalGet(8);
      B.Call(FPuDigFunc);
      { rounding can push the mantissa back out of its window: 9.99 asked for
        two decimals becomes 10.00, and with sh = 0 it becomes 1.00 where a
        leading zero was required }
      B.I32Const(PU_NINT); B.OpMem(wopI32Load, 2, 0); B.LocalSet(16);
      B.LocalGet(19); B.Op(wopI32Eqz);
      B.BlockStart(wopIf, WASM_TYPE_I32);
        B.I32Const(PU_DIG); B.OpMem(wopI32Load8U, 0, 0); B.I32Const(0); B.Op(wopI32Ne);
      B.Op(wopElse);
        B.LocalGet(16); B.LocalGet(19); B.Op(wopI32GtS);
      B.EndOp;
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(23); B.F64Const(10); B.Op(wopF64Div); B.LocalSet(23);
        B.LocalGet(20); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(20);
        B.LocalGet(23); B.I32Const(0); B.I64Const(0); B.LocalGet(8);
        B.Call(FPuDigFunc);
      B.EndOp;
      B.I32Const(PU_NINT); B.OpMem(wopI32Load, 2, 0); B.LocalSet(16);
      B.I32Const(PU_NDEC); B.OpMem(wopI32Load, 2, 0); B.LocalSet(17);

      B.I32Const(PU_OUT + 128); B.LocalSet(13);
      B.LocalGet(13); B.LocalSet(14);
      { ⚠️ Pad the mantissa so its integer digits FILL the field's positions:
        "###.#^^^^" on zero is "  0.0E+00", where one space is the sign's and
        the other is the integer position the single '0' does not use. }
      B.LocalGet(16); B.LocalSet(15);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(15); B.LocalGet(19); B.Op(wopI32GeS); B.BrIf(1);
          B.LocalGet(14); B.I32Const(Ord(' ')); B.OpMem(wopI32Store8, 0, 0);
          B.LocalGet(14); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(14);
          B.LocalGet(15); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(15);
          B.Br(0);
        B.EndOp;
      B.EndOp;
      // mantissa digits, then the point, then the decimals
      B.I32Const(0); B.LocalSet(15);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(15); B.LocalGet(16); B.Op(wopI32GeS); B.BrIf(1);
          { with sh = 0 the single integer digit is a written zero, and a MINUS
            replaces it: "-.45E+01", not "-0.45E+01" }
          B.LocalGet(19); B.Op(wopI32Eqz); B.LocalGet(12); B.Op(wopI32And);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.Op(wopElse);
            B.LocalGet(14);
            B.I32Const(PU_DIG); B.LocalGet(15); B.Op(wopI32Add);
            B.OpMem(wopI32Load8U, 0, 0); B.I32Const(Ord('0')); B.Op(wopI32Add);
            B.OpMem(wopI32Store8, 0, 0);
            B.LocalGet(14); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(14);
          B.EndOp;
          B.LocalGet(15); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(15);
          B.Br(0);
        B.EndOp;
      B.EndOp;
      B.LocalGet(17); B.I32Const(0); B.Op(wopI32GtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(14); B.I32Const(Ord('.')); B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(14); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(14);
        B.I32Const(0); B.LocalSet(15);
        B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
          B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(15); B.LocalGet(17); B.Op(wopI32GeS); B.BrIf(1);
            B.LocalGet(14);
            B.I32Const(PU_DIG); B.LocalGet(16); B.Op(wopI32Add);
              B.LocalGet(15); B.Op(wopI32Add);
            B.OpMem(wopI32Load8U, 0, 0); B.I32Const(Ord('0')); B.Op(wopI32Add);
            B.OpMem(wopI32Store8, 0, 0);
            B.LocalGet(14); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(14);
            B.LocalGet(15); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(15);
            B.Br(0);
          B.EndOp;
        B.EndOp;
      B.EndOp;
      // E, its sign, and (caret - 2) digits of exponent
      B.LocalGet(14); B.I32Const(Ord('E')); B.OpMem(wopI32Store8, 0, 0);
      B.LocalGet(14); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(14);
      B.LocalGet(14);
      B.LocalGet(20); B.I32Const(0); B.Op(wopI32LtS);
      B.BlockStart(wopIf, WASM_TYPE_I32);
        B.I32Const(Ord('-'));
      B.Op(wopElse);
        B.I32Const(Ord('+'));
      B.EndOp;
      B.OpMem(wopI32Store8, 0, 0);
      B.LocalGet(14); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(14);
      B.LocalGet(20); B.I32Const(0); B.Op(wopI32LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(0); B.LocalGet(20); B.Op(wopI32Sub); B.LocalSet(20);
      B.EndOp;
      B.LocalGet(9); B.I32Const(2); B.Op(wopI32Sub); B.LocalSet(21);
      B.LocalGet(14); B.LocalGet(21); B.Op(wopI32Add); B.LocalSet(15);
      B.LocalGet(15); B.LocalSet(18);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(15); B.LocalGet(14); B.Op(wopI32LeS); B.BrIf(1);
          B.LocalGet(15); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(15);
          B.LocalGet(15);
          B.LocalGet(20); B.I32Const(10); B.Op(wopI32RemU);
            B.I32Const(Ord('0')); B.Op(wopI32Add);
          B.OpMem(wopI32Store8, 0, 0);
          B.LocalGet(20); B.I32Const(10); B.Op(wopI32DivU); B.LocalSet(20);
          B.Br(0);
        B.EndOp;
      B.EndOp;
      B.LocalGet(18); B.LocalSet(14);
      { The sign in front: a space when a position was held back and the value is
        positive. ⚠️ EXCEPT with sh = 0, where the held-back position is already
        occupied by the written zero of "0.45" - there a positive value gets
        NOTHING, and a negative one gets the minus that REPLACES that zero. }
      B.LocalGet(19); B.LocalGet(7); B.Op(wopI32LtS);
      B.LocalGet(19); B.I32Const(0); B.Op(wopI32GtS);
      B.LocalGet(12); B.Op(wopI32Or); B.Op(wopI32And);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(13); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(13);
        B.LocalGet(13);
        B.LocalGet(12);
        B.BlockStart(wopIf, WASM_TYPE_I32);
          B.I32Const(Ord('-'));
        B.Op(wopElse);
          B.I32Const(Ord(' '));
        B.EndOp;
        B.OpMem(wopI32Store8, 0, 0);
      B.Op(wopElse);
        // every position is a digit: a negative simply does not fit
        B.LocalGet(12);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(13); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(13);
          B.LocalGet(13); B.I32Const(Ord('-')); B.OpMem(wopI32Store8, 0, 0);
          B.LocalGet(13); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(13);
          B.LocalGet(13); B.I32Const(Ord('%')); B.OpMem(wopI32Store8, 0, 0);
        B.EndOp;
      B.EndOp;
      // hand back the field
      B.LocalGet(14); B.LocalGet(13); B.Op(wopI32Sub); B.LocalTee(21);
      B.Call(FStrNewFunc); B.LocalTee(22);
      B.I32Const(4); B.Op(wopI32Add);
      B.LocalGet(13); B.LocalGet(21);
      B.MemoryCopy;
      B.LocalGet(22); B.Op(wopReturn);
    B.EndOp;

    // ---- the plain numeric field ----
    B.LocalGet(1); B.LocalGet(2); B.LocalGet(3); B.LocalGet(8);
    B.Call(FPuDigFunc);
    B.I32Const(PU_NINT); B.OpMem(wopI32Load, 2, 0); B.LocalSet(16);
    B.I32Const(PU_NDEC); B.OpMem(wopI32Load, 2, 0); B.LocalSet(17);

    B.I32Const(PU_OUT + 128); B.LocalSet(13);
    B.LocalGet(13); B.LocalSet(14);
    // integer digits, with a comma every three counting from the right
    B.I32Const(0); B.LocalSet(15);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(15); B.LocalGet(16); B.Op(wopI32GeS); B.BrIf(1);
        { ⛔ i32.and is BITWISE, so a flag has to be normalised to 0/1 before it
          is combined with a comparison: "1 and 32" is ZERO, and the condition
          was never true - the commas simply never appeared. The other flag
          tests here survive only because an Eqz or an Or happens to sit between
          them and the combination. }
        B.LocalGet(15); B.I32Const(0); B.Op(wopI32GtS);
        B.LocalGet(11); B.I32Const(32); B.Op(wopI32And);
          B.I32Const(0); B.Op(wopI32Ne); B.Op(wopI32And);
        B.LocalGet(16); B.LocalGet(15); B.Op(wopI32Sub);
          B.I32Const(3); B.Op(wopI32RemU); B.Op(wopI32Eqz); B.Op(wopI32And);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(14); B.I32Const(Ord(',')); B.OpMem(wopI32Store8, 0, 0);
          B.LocalGet(14); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(14);
        B.EndOp;
        B.LocalGet(14);
        B.I32Const(PU_DIG); B.LocalGet(15); B.Op(wopI32Add);
        B.OpMem(wopI32Load8U, 0, 0); B.I32Const(Ord('0')); B.Op(wopI32Add);
        B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(14); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(14);
        B.LocalGet(15); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(15);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    { the point prints even with no '#' after it - "#." on 0.5 gives "1." - so
      it follows the PICTURE, not the presence of decimals }
    B.LocalGet(17); B.I32Const(0); B.Op(wopI32GtS);
    B.LocalGet(11); B.I32Const(64); B.Op(wopI32And); B.Op(wopI32Or);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(14); B.I32Const(Ord('.')); B.OpMem(wopI32Store8, 0, 0);
      B.LocalGet(14); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(14);
    B.EndOp;
    B.I32Const(0); B.LocalSet(15);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(15); B.LocalGet(17); B.Op(wopI32GeS); B.BrIf(1);
        B.LocalGet(14);
        B.I32Const(PU_DIG); B.LocalGet(16); B.Op(wopI32Add);
          B.LocalGet(15); B.Op(wopI32Add);
        B.OpMem(wopI32Load8U, 0, 0); B.I32Const(Ord('0')); B.Op(wopI32Add);
        B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(14); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(14);
        B.LocalGet(15); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(15);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    // the floating '$' hugs the first digit
    B.LocalGet(11); B.I32Const(16); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(13); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(13);
      B.LocalGet(13); B.I32Const(Ord('$')); B.OpMem(wopI32Store8, 0, 0);
    B.EndOp;
    // a leading sign always prints; otherwise a minus prints unless it trails
    B.LocalGet(11); B.I32Const(1); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(13); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(13);
      B.LocalGet(13);
      B.LocalGet(12);
      B.BlockStart(wopIf, WASM_TYPE_I32);
        B.I32Const(Ord('-'));
      B.Op(wopElse);
        B.I32Const(Ord('+'));
      B.EndOp;
      B.OpMem(wopI32Store8, 0, 0);
    B.Op(wopElse);
      B.LocalGet(12);
      B.LocalGet(11); B.I32Const(6); B.Op(wopI32And); B.Op(wopI32Eqz); B.Op(wopI32And);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(13); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(13);
        B.LocalGet(13); B.I32Const(Ord('-')); B.OpMem(wopI32Store8, 0, 0);
      B.EndOp;
    B.EndOp;
    { The overflow test is a CAPACITY test on the integer positions, where the
      digits, the '$' of "$$" and a FRONT sign all compete. A trailing sign does
      not: it has its own position at the end. }
    B.LocalGet(7); B.LocalSet(18);
    B.LocalGet(11); B.I32Const(16); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(18); B.I32Const(2); B.Op(wopI32Add); B.LocalSet(18);
    B.EndOp;
    B.LocalGet(11); B.I32Const(1); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(18); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(18);
    B.EndOp;
    B.LocalGet(16); B.LocalSet(21);
    B.LocalGet(11); B.I32Const(16); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(21); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(21);
    B.EndOp;
    B.LocalGet(11); B.I32Const(1); B.Op(wopI32And);
    B.LocalGet(12); B.LocalGet(11); B.I32Const(6); B.Op(wopI32And);
      B.Op(wopI32Eqz); B.Op(wopI32And);
    B.Op(wopI32Or);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(21); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(21);
    B.EndOp;
    B.LocalGet(21); B.LocalGet(18); B.Op(wopI32GtS); B.LocalSet(21);   // overflow?

    // a trailing sign, and a trailing '-' that prints a SPACE when positive
    B.LocalGet(11); B.I32Const(2); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(14);
      B.LocalGet(12);
      B.BlockStart(wopIf, WASM_TYPE_I32);
        B.I32Const(Ord('-'));
      B.Op(wopElse);
        B.I32Const(Ord('+'));
      B.EndOp;
      B.OpMem(wopI32Store8, 0, 0);
      B.LocalGet(14); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(14);
    B.Op(wopElse);
      B.LocalGet(11); B.I32Const(4); B.Op(wopI32And);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(14);
        B.LocalGet(12);
        B.BlockStart(wopIf, WASM_TYPE_I32);
          B.I32Const(Ord('-'));
        B.Op(wopElse);
          B.I32Const(Ord(' '));
        B.EndOp;
        B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(14); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(14);
      B.EndOp;
    B.EndOp;

    // pad on the left to the field width
    B.LocalGet(7); B.LocalSet(18);
    B.LocalGet(17); B.I32Const(0); B.Op(wopI32GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(18); B.LocalGet(17); B.Op(wopI32Add); B.I32Const(1); B.Op(wopI32Add);
      B.LocalSet(18);
    B.Op(wopElse);
      B.LocalGet(11); B.I32Const(64); B.Op(wopI32And);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(18); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(18);
      B.EndOp;
    B.EndOp;
    B.LocalGet(11); B.I32Const(16); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(18); B.I32Const(2); B.Op(wopI32Add); B.LocalSet(18);
    B.EndOp;
    B.LocalGet(11); B.I32Const(1); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(18); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(18);
    B.EndOp;
    B.LocalGet(11); B.I32Const(6); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(18); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(18);
    B.EndOp;
    B.LocalGet(18); B.LocalGet(10); B.Op(wopI32Add); B.LocalSet(18);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(14); B.LocalGet(13); B.Op(wopI32Sub);
        B.LocalGet(18); B.Op(wopI32GeS); B.BrIf(1);
        B.LocalGet(13); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(13);
        B.LocalGet(13); B.I32Const(Ord(' ')); B.OpMem(wopI32Store8, 0, 0);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    // the marker, then the fixed '$' - which goes AHEAD of it ("$%1234.50")
    B.LocalGet(21);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(13); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(13);
      B.LocalGet(13); B.I32Const(Ord('%')); B.OpMem(wopI32Store8, 0, 0);
    B.EndOp;
    B.LocalGet(11); B.I32Const(8); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(13); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(13);
      B.LocalGet(13); B.I32Const(Ord('$')); B.OpMem(wopI32Store8, 0, 0);
    B.EndOp;

    B.LocalGet(14); B.LocalGet(13); B.Op(wopI32Sub); B.LocalTee(21);
    B.Call(FStrNewFunc); B.LocalTee(22);
    B.I32Const(4); B.Op(wopI32Add);
    B.LocalGet(13); B.LocalGet(21);
    B.MemoryCopy;
    B.LocalGet(22);
    FModule.AddFunction(TPuFmt, [wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32,
                                 wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32,
                                 wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32,
                                 wvtI32, wvtF64], B);
  finally
    B.Free;
  end;
  end;

  { printStr(s): the bytes straight into the sink. No formatting - there is
    none to do for a string, which is exactly why PRINT of a string is cheap
    where PRINT of a number was a whole formatter. }
  if FUsesPrint then
  begin
    B := TWasmBuf.Create;
    try
      B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add);
      B.LocalGet(0); B.OpMem(wopI32Load, 2, 0);
      B.Call(WriteTarget);
      FModule.AddFunction(TPrint, [], B);
    finally
      B.Free;
    end;
  end;

  EmitValHelpers;
  EmitBaseHelper;
  EmitTrimSetHelper;
  EmitPackHelpers;
  EmitWideHelpers;
end;

{ The WSTRING family, as five functions that all say the same thing in different
  words: a CODEPOINT is any byte that is not a UTF-8 continuation byte.

  ⭐ A wide string is stored as UTF-8 in the ordinary string bank, which is the
  project's decision and not an accident of this backend - so LEN, LEFT$, MID$
  and the rest need no second storage, only a second way of COUNTING. Everything
  below is that one test, `(b and $C0) <> $80`, applied five ways.
  ⭐⭐ And the two searches need nothing new at all: INSTR of a wide string is
  the ORDINARY byte search followed by a byte-to-codepoint conversion, because a
  UTF-8 substring can only match at a codepoint boundary. INSTRREV is the same
  with strFind's direction bit set - which is why the interpreter can get away
  with scanning bytes there too. }
procedure TWasmBackend.EmitWideHelpers;
var
  B: TWasmBuf;
  TLen, TSub, TEnc, TB2CP, TRep: LongWord;

  { The continuation test, on the byte already loaded. Leaves 1 when the byte
    STARTS a codepoint. }
  procedure EmitIsLead;
  begin
    B.I32Const($C0); B.Op(wopI32And);
    B.I32Const($80); B.Op(wopI32Ne);
  end;

begin
  if not FUsesWide then Exit;
  TLen  := FModule.TypeIndex([wvtI32], [wvtI64]);
  TSub  := FModule.TypeIndex([wvtI32, wvtI64, wvtI64], [wvtI32]);
  TEnc  := FModule.TypeIndex([wvtI64], [wvtI32]);
  TB2CP := FModule.TypeIndex([wvtI32, wvtI64], [wvtI64]);
  TRep  := FModule.TypeIndex([wvtI64, wvtI64], [wvtI32]);

  { wLen(s) -> how many codepoints the bytes hold. }
  B := TWasmBuf.Create;
  try
    // param 0 = s; locals 1 = len, 2 = base, 3 = i, 4 = n
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(1);
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(2);
    B.I32Const(0); B.LocalSet(3);
    B.I32Const(0); B.LocalSet(4);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(3); B.LocalGet(1); B.Op(wopI32GeS); B.BrIf(1);
        B.LocalGet(4);
        B.LocalGet(2); B.LocalGet(3); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0);
        EmitIsLead;
        B.Op(wopI32Add); B.LocalSet(4);
        B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(3);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    B.LocalGet(4); B.Op(wopI64ExtendI32S);
    FModule.AddFunction(TLen, [wvtI32, wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { wSub(s, cpStart, cpCount) -> the substring covering cpCount codepoints from
    the 1-based codepoint cpStart, clamped at both ends. Mirrors Utf8SubCP. }
  B := TWasmBuf.Create;
  try
    // params 0=s 1=cpStart 2=cpCount; locals 3=n 4=base 5=i 6=cp
    //         7=bStart 8=bEnd 9=cs 10=cc 11=handle
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(3);
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(4);
    { ⛔ BOTH ARGUMENTS ARE CLAMPED BEFORE THEY NARROW, and not for tidiness: a
      count of 2^32+5 wrapped straight to i32 would come out as 5 and cut a
      string that should have been copied whole. No string can hold more
      codepoints than it has bytes, so n is a clamp that changes no answer. }
    B.LocalGet(1); B.I64Const(1); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_TYPE_I32);
      B.I32Const(1);
    B.Op(wopElse);
      B.LocalGet(1); B.LocalGet(3); B.Op(wopI64ExtendI32S); B.Op(wopI64GtS);
      B.BlockStart(wopIf, WASM_TYPE_I32);
        B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add);
      B.Op(wopElse);
        B.LocalGet(1); B.Op(wopI32WrapI64);
      B.EndOp;
    B.EndOp;
    B.LocalSet(9);
    B.LocalGet(2); B.I64Const(0); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_TYPE_I32);
      B.I32Const(0);
    B.Op(wopElse);
      B.LocalGet(2); B.LocalGet(3); B.Op(wopI64ExtendI32S); B.Op(wopI64GtS);
      B.BlockStart(wopIf, WASM_TYPE_I32);
        B.LocalGet(3);
      B.Op(wopElse);
        B.LocalGet(2); B.Op(wopI32WrapI64);
      B.EndOp;
    B.EndOp;
    B.LocalSet(10);

    B.LocalGet(3); B.LocalSet(7);          // bStart: past the end = empty
    B.LocalGet(3); B.LocalSet(8);          // bEnd:   past the end = to the end
    B.I32Const(0); B.LocalSet(6);
    B.I32Const(0); B.LocalSet(5);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(5); B.LocalGet(3); B.Op(wopI32GeS); B.BrIf(1);
        B.LocalGet(4); B.LocalGet(5); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0);
        EmitIsLead;
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(6); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(6);
          B.LocalGet(6); B.LocalGet(9); B.Op(wopI32Eq);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(5); B.LocalSet(7);
          B.EndOp;
          B.LocalGet(6); B.LocalGet(9); B.LocalGet(10); B.Op(wopI32Add); B.Op(wopI32Eq);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(5); B.LocalSet(8);
            B.Br(3);
          B.EndOp;
        B.EndOp;
        B.LocalGet(5); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(5);
        B.Br(0);
      B.EndOp;
    B.EndOp;

    B.LocalGet(7); B.LocalGet(3); B.Op(wopI32GeS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.Call(FStrNewFunc); B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(8); B.LocalGet(7); B.Op(wopI32Sub); B.LocalTee(5);
    B.Call(FStrNewFunc); B.LocalTee(11);
    B.I32Const(4); B.Op(wopI32Add);
    B.LocalGet(4); B.LocalGet(7); B.Op(wopI32Add);
    B.LocalGet(5);
    B.MemoryCopy;
    B.LocalGet(11);
    FModule.AddFunction(TSub, [wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32,
                               wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { wEnc(cp) -> the UTF-8 bytes of one codepoint. Out of range becomes U+FFFD,
    which is what Utf8EncodeCP does.
    ⚠️ Surrogates are NOT rejected, deliberately: the interpreter encodes them,
    and agreeing with sb is the contract. }
  B := TWasmBuf.Create;
  try
    // param 0 = cp; locals 1 = c, 2 = handle, 3 = p
    B.LocalGet(0); B.I64Const(0); B.Op(wopI64LtS);
    B.LocalGet(0); B.I64Const($10FFFF); B.Op(wopI64GtS);
    B.Op(wopI32Or);
    B.BlockStart(wopIf, WASM_TYPE_I32);
      B.I32Const($FFFD);
    B.Op(wopElse);
      B.LocalGet(0); B.Op(wopI32WrapI64);
    B.EndOp;
    B.LocalSet(1);

    B.LocalGet(1); B.I32Const($80); B.Op(wopI32LtU);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(1); B.Call(FStrNewFunc); B.LocalTee(2);
      B.I32Const(4); B.Op(wopI32Add);
      B.LocalGet(1); B.OpMem(wopI32Store8, 0, 0);
    B.Op(wopElse);
      B.LocalGet(1); B.I32Const($800); B.Op(wopI32LtU);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(2); B.Call(FStrNewFunc); B.LocalTee(2);
        B.I32Const(4); B.Op(wopI32Add); B.LocalSet(3);
        B.LocalGet(3);
        B.LocalGet(1); B.I32Const(6); B.Op(wopI32ShrU); B.I32Const($C0); B.Op(wopI32Or);
        B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(3);
        B.LocalGet(1); B.I32Const($3F); B.Op(wopI32And); B.I32Const($80); B.Op(wopI32Or);
        B.OpMem(wopI32Store8, 0, 1);
      B.Op(wopElse);
        B.LocalGet(1); B.I32Const($10000); B.Op(wopI32LtU);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(3); B.Call(FStrNewFunc); B.LocalTee(2);
          B.I32Const(4); B.Op(wopI32Add); B.LocalSet(3);
          B.LocalGet(3);
          B.LocalGet(1); B.I32Const(12); B.Op(wopI32ShrU); B.I32Const($E0); B.Op(wopI32Or);
          B.OpMem(wopI32Store8, 0, 0);
          B.LocalGet(3);
          B.LocalGet(1); B.I32Const(6); B.Op(wopI32ShrU);
          B.I32Const($3F); B.Op(wopI32And); B.I32Const($80); B.Op(wopI32Or);
          B.OpMem(wopI32Store8, 0, 1);
          B.LocalGet(3);
          B.LocalGet(1); B.I32Const($3F); B.Op(wopI32And); B.I32Const($80); B.Op(wopI32Or);
          B.OpMem(wopI32Store8, 0, 2);
        B.Op(wopElse);
          B.I32Const(4); B.Call(FStrNewFunc); B.LocalTee(2);
          B.I32Const(4); B.Op(wopI32Add); B.LocalSet(3);
          B.LocalGet(3);
          B.LocalGet(1); B.I32Const(18); B.Op(wopI32ShrU); B.I32Const($F0); B.Op(wopI32Or);
          B.OpMem(wopI32Store8, 0, 0);
          B.LocalGet(3);
          B.LocalGet(1); B.I32Const(12); B.Op(wopI32ShrU);
          B.I32Const($3F); B.Op(wopI32And); B.I32Const($80); B.Op(wopI32Or);
          B.OpMem(wopI32Store8, 0, 1);
          B.LocalGet(3);
          B.LocalGet(1); B.I32Const(6); B.Op(wopI32ShrU);
          B.I32Const($3F); B.Op(wopI32And); B.I32Const($80); B.Op(wopI32Or);
          B.OpMem(wopI32Store8, 0, 2);
          B.LocalGet(3);
          B.LocalGet(1); B.I32Const($3F); B.Op(wopI32And); B.I32Const($80); B.Op(wopI32Or);
          B.OpMem(wopI32Store8, 0, 3);
        B.EndOp;
      B.EndOp;
    B.EndOp;
    B.LocalGet(2);
    FModule.AddFunction(TEnc, [wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { wB2CP(s, bytePos) -> the 1-based CODEPOINT position of a 1-based BYTE
    position. 0 stays 0, which is how "not found" travels through it. }
  B := TWasmBuf.Create;
  try
    // params 0 = s, 1 = bytePos; locals 2 = len, 3 = base, 4 = i, 5 = n
    B.LocalGet(1); B.I64Const(0); B.Op(wopI64LeS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(0); B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(2);
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(3);
    B.I32Const(0); B.LocalSet(4);
    B.I32Const(0); B.LocalSet(5);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        // stop at the byte position asked for, or at the end - whichever first
        B.LocalGet(4); B.Op(wopI64ExtendI32S); B.LocalGet(1); B.Op(wopI64GeS); B.BrIf(1);
        B.LocalGet(4); B.LocalGet(2); B.Op(wopI32GeS); B.BrIf(1);
        B.LocalGet(5);
        B.LocalGet(3); B.LocalGet(4); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0);
        EmitIsLead;
        B.Op(wopI32Add); B.LocalSet(5);
        B.LocalGet(4); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(4);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    B.LocalGet(5); B.Op(wopI64ExtendI32S);
    FModule.AddFunction(TB2CP, [wvtI32, wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { wRep(count, cp) -> count copies of one codepoint's bytes. WSTRING(n, cp). }
  B := TWasmBuf.Create;
  try
    // params 0 = count, 1 = cp; locals 2 = one, 3 = L, 4 = cnt, 5 = handle, 6 = dst
    B.LocalGet(1); B.Call(FWEncFunc); B.LocalTee(2);
    B.OpMem(wopI32Load, 2, 0); B.LocalSet(3);
    B.LocalGet(0); B.I64Const(0); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_TYPE_I32);
      B.I32Const(0);
    B.Op(wopElse);
      B.LocalGet(0); B.Op(wopI32WrapI64);
    B.EndOp;
    B.LocalSet(4);
    B.LocalGet(4); B.LocalGet(3); B.Op(wopI32Mul);
    B.Call(FStrNewFunc); B.LocalTee(5);
    B.I32Const(4); B.Op(wopI32Add); B.LocalSet(6);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(4); B.Op(wopI32Eqz); B.BrIf(1);
        B.LocalGet(6);
        B.LocalGet(2); B.I32Const(4); B.Op(wopI32Add);
        B.LocalGet(3);
        B.MemoryCopy;
        B.LocalGet(6); B.LocalGet(3); B.Op(wopI32Add); B.LocalSet(6);
        B.LocalGet(4); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(4);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    B.LocalGet(5);
    FModule.AddFunction(TRep, [wvtI32, wvtI32, wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;
end;

{ MKI$/MKL$/MKSHORT$/MKLONGINT$/MKS$/MKD$ and their eight CV* inverses, as TWO
  functions: strPack(bits, width) writes the low `width` bytes of an integer into
  a fresh string, strUnpack(s, width) reads them back SIGN-EXTENDED.

  ⭐ A FLOAT NEEDS NOTHING OF ITS OWN. MKD$ is strPack of the double's bits and
  MKS$ is strPack of the single's, so the only float-specific instruction is the
  reinterpretation at the call site - which is what these eight spellings have
  always been underneath, and writing four bodies would be four places for the
  byte order to be got wrong.
  ⚠️ LITTLE-ENDIAN, which is what the interpreter writes and what WASM's own
  loads and stores use, so the two agree by construction rather than by testing.
  ⛔ A string SHORTER than the width reads as 0, not as an error and not as a
  partial value - that is what FreeBASIC returns and what bcStrCvInt does. }
procedure TWasmBackend.EmitPackHelpers;
var
  B: TWasmBuf;
  TPack, TUnpack: LongWord;
begin
  if not FUsesPack then Exit;
  TPack   := FModule.TypeIndex([wvtI64, wvtI32], [wvtI32]);
  TUnpack := FModule.TypeIndex([wvtI32, wvtI32], [wvtI64]);

  B := TWasmBuf.Create;
  try
    // params 0 = bits, 1 = width; locals 2 = handle, 3 = base, 4 = i
    B.LocalGet(1); B.Call(FStrNewFunc); B.LocalTee(2);
    B.I32Const(4); B.Op(wopI32Add); B.LocalSet(3);
    B.I32Const(0); B.LocalSet(4);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(4); B.LocalGet(1); B.Op(wopI32GeS); B.BrIf(1);
        B.LocalGet(3); B.LocalGet(4); B.Op(wopI32Add);
        { ⛔ A byte at a time rather than one i64.store, because the width is 2,
          4 or 8 and only known at run time here. Eight iterations at the very
          most - and branching on the width would be three stores AND the branch. }
        B.LocalGet(0);
        B.LocalGet(4); B.Op(wopI64ExtendI32S); B.I64Const(8); B.Op(wopI64Mul);
        B.Op(wopI64ShrU); B.Op(wopI32WrapI64);
        B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(4); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(4);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    B.LocalGet(2);
    FModule.AddFunction(TPack, [wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  B := TWasmBuf.Create;
  try
    // params 0 = s, 1 = width; locals 2 = base, 3 = i, 4 = acc, 5 = shift
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalGet(1); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(0); B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(2);
    B.I64Const(0); B.LocalSet(4);
    // from the TOP byte down, so each one shifts what came before it up
    B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(3);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(3); B.I32Const(0); B.Op(wopI32LtS); B.BrIf(1);
        B.LocalGet(4); B.I64Const(8); B.Op(wopI64Shl);
        B.LocalGet(2); B.LocalGet(3); B.Op(wopI32Add);
        B.OpMem(wopI32Load8U, 0, 0); B.Op(wopI64ExtendI32U);
        B.Op(wopI64Or); B.LocalSet(4);
        B.LocalGet(3); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(3);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    { Sign-extend from the top bit of the width-byte value. ⛔ Skipped at width 8:
      the shift amount would be 0, which is harmless, but i64.shl by 64 is not -
      WASM takes the count modulo 64, so it would be a shift by nothing and the
      sign would come out of the wrong bit. }
    B.LocalGet(1); B.I32Const(8); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(64);
      B.LocalGet(1); B.Op(wopI64ExtendI32S); B.I64Const(8); B.Op(wopI64Mul);
      B.Op(wopI64Sub); B.LocalSet(5);
      B.LocalGet(4); B.LocalGet(5); B.Op(wopI64Shl);
      B.LocalGet(5); B.Op(wopI64ShrS); B.LocalSet(4);
    B.EndOp;
    B.LocalGet(4);
    FModule.AddFunction(TUnpack, [wvtI32, wvtI32, wvtI64, wvtI64], B);
  finally
    B.Free;
  end;
end;

{ strTrimSet(s, pat, mode) -> a new string with the ends stripped.
  mode low two bits are the side (0 = both, 1 = left, 2 = right) and bit 2 says
  whether pat is a CHARACTER SET (FreeBASIC's "Any" form) or a whole substring
  to remove REPEATEDLY - which is bcStrTrimSet's immediate, unchanged.

  ⛔ NOT the same predicate as strTrim, and that is why it is a second helper:
  strTrim removes every byte <= 32 whatever it is, this one removes exactly what
  it was handed. Folding them would mean passing "the whitespace set" as a
  string, and then TRIM(s) and TRIM(s, " ") would stop being different - they
  are, on a tab.
  ⚠️ An EMPTY pat removes nothing. The interpreter guards on Len > 0 and returns
  the string untouched; here the two loops simply never advance, which comes to
  the same and needs no special case. }
procedure TWasmBackend.EmitTrimSetHelper;
var
  B: TWasmBuf;
  T: LongWord;

  { The "does this end still start (or finish) with the pattern" test, emitted
    twice with the ends swapped. AtEnd picks which one.

    ⛔ THE BRANCH DEPTHS ARE THE WHOLE DIFFICULTY, and an `if` COUNTS as a label:
    inside the innermost guard the stack reads 0 = that guard, 1 = the scan loop,
    2 = the set/substring `if`, 3 = MATCHED, 4 = the trim loop, 5 = DONE. Every
    exit below is written against that list. Both scans leave only by branching -
    match goes to MATCHED and failure to DONE - so nothing falls out of them, and
    a depth that is one off does not fail to validate, it trims the wrong thing. }
  procedure EmitStrip(AtEnd: Boolean);
  begin
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);          // DONE
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);         // the trim loop
        // no room left for another removal
        B.LocalGet(5); B.LocalGet(4); B.Op(wopI32Sub);
        B.LocalGet(7); B.Op(wopI32LtS); B.BrIf(1);         // -> DONE
        B.I32Const(0); B.LocalSet(8);
        B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);      // MATCHED
          B.LocalGet(2); B.I32Const(4); B.Op(wopI32And);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            { THE SET FORM: the single character at this end against every
              member. EXISTS one -> MATCHED; ran out -> DONE. }
            B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
              B.LocalGet(8); B.LocalGet(6); B.Op(wopI32GeS);
              B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
                B.Br(5);                                   // -> DONE
              B.EndOp;
              B.LocalGet(3);
              if AtEnd then
              begin
                B.LocalGet(5); B.Op(wopI32Add); B.I32Const(1); B.Op(wopI32Sub);
              end
              else
              begin
                B.LocalGet(4); B.Op(wopI32Add);
              end;
              B.OpMem(wopI32Load8U, 0, 0);
              B.LocalGet(9); B.LocalGet(8); B.Op(wopI32Add);
              B.OpMem(wopI32Load8U, 0, 0);
              B.Op(wopI32Eq);
              B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
                B.Br(3);                                   // -> MATCHED
              B.EndOp;
              B.LocalGet(8); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(8);
              B.Br(0);
            B.EndOp;
          B.Op(wopElse);
            { THE SUBSTRING FORM: the opposite quantifier - ALL the bytes have to
              be there, so running out of them is the SUCCESS and a difference is
              the failure. The two targets swap places, and that is the only
              thing that changes. }
            B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
              B.LocalGet(8); B.LocalGet(6); B.Op(wopI32GeS);
              B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
                B.Br(3);                                   // -> MATCHED
              B.EndOp;
              B.LocalGet(3);
              if AtEnd then
              begin
                B.LocalGet(5); B.Op(wopI32Add); B.LocalGet(6); B.Op(wopI32Sub);
              end
              else
              begin
                B.LocalGet(4); B.Op(wopI32Add);
              end;
              B.LocalGet(8); B.Op(wopI32Add);
              B.OpMem(wopI32Load8U, 0, 0);
              B.LocalGet(9); B.LocalGet(8); B.Op(wopI32Add);
              B.OpMem(wopI32Load8U, 0, 0);
              B.Op(wopI32Ne);
              B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
                B.Br(5);                                   // -> DONE
              B.EndOp;
              B.LocalGet(8); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(8);
              B.Br(0);
            B.EndOp;
          B.EndOp;
        B.EndOp;                                           // MATCHED
        // matched: move this end past what was removed and go round again
        if AtEnd then
        begin
          B.LocalGet(5); B.LocalGet(7); B.Op(wopI32Sub); B.LocalSet(5);
        end
        else
        begin
          B.LocalGet(4); B.LocalGet(7); B.Op(wopI32Add); B.LocalSet(4);
        end;
        B.Br(0);
      B.EndOp;
    B.EndOp;
  end;

begin
  if not FUsesTrimSet then Exit;
  T := FModule.TypeIndex([wvtI32, wvtI32, wvtI32], [wvtI32]);
  B := TWasmBuf.Create;
  try
    // params 0=s 1=pat 2=mode; locals 3=sbase 4=first 5=last 6=plen
    //         7=step 8=j 9=pbase 10=len 11=handle
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(3);
    B.LocalGet(1); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(9);
    B.I32Const(0); B.LocalSet(4);
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(5);
    B.LocalGet(1); B.OpMem(wopI32Load, 2, 0); B.LocalSet(6);
    { How much one removal takes off: a single character in the set form, the
      whole pattern otherwise. ⛔ It is NOT the same as the number of bytes
      compared when the set form is in play - there the whole set is scanned to
      remove one byte - which is why these are two locals and not one. }
    B.LocalGet(2); B.I32Const(4); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_TYPE_I32);
      B.I32Const(1);
    B.Op(wopElse);
      B.LocalGet(6);
    B.EndOp;
    B.LocalSet(7);

    B.LocalGet(6); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(0); B.Op(wopReturn);       // an empty pattern removes nothing
    B.EndOp;

    B.LocalGet(2); B.I32Const(3); B.Op(wopI32And); B.I32Const(2); B.Op(wopI32Ne);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      EmitStrip(False);
    B.EndOp;
    B.LocalGet(2); B.I32Const(3); B.Op(wopI32And); B.I32Const(1); B.Op(wopI32Ne);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      EmitStrip(True);
    B.EndOp;

    B.LocalGet(5); B.LocalGet(4); B.Op(wopI32Sub); B.LocalTee(10);
    B.Call(FStrNewFunc); B.LocalTee(11);
    B.I32Const(4); B.Op(wopI32Add);
    B.LocalGet(3); B.LocalGet(4); B.Op(wopI32Add);
    B.LocalGet(10);
    B.MemoryCopy;
    B.LocalGet(11);
    FModule.AddFunction(T, [wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32,
                            wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;
end;

{ strBase(v: i64, digits: i64, base: i64) -> a fresh string holding v in that
  base, uppercase, with the optional width applied.

  ⭐ ONE helper for HEX$, OCT and BIN because the interpreter is one function
  too - IntToBaseStr(v, 16 | 8 | 2) followed by FitBaseDigits - and three copies
  of it here would be three chances to disagree with sb about, say, what BIN of a
  negative number is.

  Two rules that are easy to get wrong and are both the interpreter's:
  ⛔ The value is read UNSIGNED. IntToBaseStr assigns the Int64 to a QWord, so
  HEX$(-1) is "FFFFFFFFFFFFFFFF" and not "-1"; i64.div_u/rem_u are the same
  reinterpretation, which is why no sign is handled anywhere below.
  ⚠️ A width SHORTER than the digits KEEPS THE RIGHTMOST ones - FitBaseDigits
  cuts from the left. It is not an error and not a clamp: BIN(255, 4) is "1111". }
procedure TWasmBackend.EmitBaseHelper;
var
  B: TWasmBuf;
  T: LongWord;
begin
  if not FUsesBase then Exit;
  T := FModule.TypeIndex([wvtI64, wvtI64, wvtI64], [wvtI32]);
  B := TWasmBuf.Create;
  try
    // locals: 3 = p, 4 = u, 5 = natural len, 6 = final len, 7 = handle,
    //         8 = digit then dst, 9 = how many digits actually survive
    B.I32Const(BASE_SCR_END); B.LocalSet(3);
    B.LocalGet(0); B.LocalSet(4);

    B.LocalGet(4); B.Op(wopI64Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      // zero is one digit, not none - the empty string is what a bare loop gives
      B.LocalGet(3); B.I32Const(1); B.Op(wopI32Sub); B.LocalTee(3);
      B.I32Const(Ord('0')); B.OpMem(wopI32Store8, 0, 0);
    B.Op(wopElse);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(4); B.Op(wopI64Eqz); B.BrIf(1);
          B.LocalGet(3); B.I32Const(1); B.Op(wopI32Sub); B.LocalTee(3);
          B.LocalGet(4); B.LocalGet(2); B.Op(wopI64RemU); B.Op(wopI32WrapI64);
          B.LocalTee(8);
          B.I32Const(10); B.Op(wopI32LtU);
          B.BlockStart(wopIf, WASM_TYPE_I32);
            B.LocalGet(8); B.I32Const(Ord('0')); B.Op(wopI32Add);
          B.Op(wopElse);
            B.LocalGet(8); B.I32Const(Ord('A') - 10); B.Op(wopI32Add);
          B.EndOp;
          B.OpMem(wopI32Store8, 0, 0);
          B.LocalGet(4); B.LocalGet(2); B.Op(wopI64DivU); B.LocalSet(4);
          B.Br(0);
        B.EndOp;
      B.EndOp;
    B.EndOp;

    B.I32Const(BASE_SCR_END); B.LocalGet(3); B.Op(wopI32Sub); B.LocalSet(5);

    { The asked-for width, or the natural one. ⛔ The clamp is not cosmetic:
      wrapping an i64 straight to i32 would turn a width of 2^32 + 5 into 5 and
      answer a SHORT string with no complaint. Above 2^31 the allocation cannot
      succeed anyway, so the module traps in strNew - which is the honest end. }
    B.LocalGet(1); B.I64Const(0); B.Op(wopI64LeS);
    B.BlockStart(wopIf, WASM_TYPE_I32);
      B.LocalGet(5);
    B.Op(wopElse);
      B.LocalGet(1); B.I64Const($7FFFFFFF); B.Op(wopI64GtS);
      B.BlockStart(wopIf, WASM_TYPE_I32);
        B.I32Const($7FFFFFFF);
      B.Op(wopElse);
        B.LocalGet(1); B.Op(wopI32WrapI64);
      B.EndOp;
    B.EndOp;
    B.LocalSet(6);

    B.LocalGet(6); B.Call(FStrNewFunc); B.LocalTee(7);
    B.I32Const(4); B.Op(wopI32Add); B.LocalSet(8);

    // how many of the digits fit: the rest are cut from the LEFT
    B.LocalGet(5); B.LocalGet(6); B.Op(wopI32LtU);
    B.BlockStart(wopIf, WASM_TYPE_I32);
      B.LocalGet(5);
    B.Op(wopElse);
      B.LocalGet(6);
    B.EndOp;
    B.LocalSet(9);

    // the zero padding, then the digits after it - one bulk op each
    B.LocalGet(8);
    B.I32Const(Ord('0'));
    B.LocalGet(6); B.LocalGet(9); B.Op(wopI32Sub);
    B.MemoryFill;

    B.LocalGet(8); B.LocalGet(6); B.Op(wopI32Add); B.LocalGet(9); B.Op(wopI32Sub);
    B.LocalGet(3); B.LocalGet(5); B.Op(wopI32Add); B.LocalGet(9); B.Op(wopI32Sub);
    B.LocalGet(9);
    B.MemoryCopy;

    B.LocalGet(7);
    FModule.AddFunction(T, [wvtI32, wvtI64, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;
end;

{ The calendar. A TDateTime here is FPC's: days since 1899-12-30, with the time
  of day in the fraction - measured, not assumed (serial 0 decodes as 1899-12-30
  and 25569 as 1970-01-01), and it is a plain proleptic Gregorian count with no
  Lotus 1900 leap-day quirk (serial 60 is the 28th of February, not the 29th).

  ⭐⭐⭐ THE SPLIT IS ONE ROUNDED MILLISECOND COUNT, and finding that is what made
  this tractable. FPC's DateTimeToTimeStamp does not decode the date and the time
  separately: it multiplies the WHOLE value by the milliseconds in a day, rounds
  ONCE away from zero, and then divides and remainders. Every edge falls out of
  that single expression instead of needing a rule of its own -
  46245.99999999999 rolls to the NEXT DAY at midnight (which decoding the two
  halves independently would never produce), and a negative serial takes its date
  from a truncating division and its time from the magnitude. Reproducing the
  arithmetic reproduces the edges; reproducing the edges one at a time would have
  been a list with no end.
  ⚠️ And IncMonth does NOT use that split for the time: it keeps Abs(Frac()) of
  the original. The two therefore disagree at the far end of a day, ON PURPOSE -
  it is what sb does. }
procedure TWasmBackend.EmitDateHelpers;
const
  MS_DAY = 86400000;
  { The interval names of DATEPART/DATEADD/DATEDIFF, in code order.
    ⛔ Anything else is code 4, a whole DAY - not an error. Measured:
    DatePart("zz", s) answers the day of the month. }
  IVAL_NAME: array[0..9] of AnsiString =
    ('yyyy', 'q', 'm', 'y', 'd', 'w', 'ww', 'h', 'n', 's');
var
  B: TWasmBuf;
  TSplit, TCivil, TDays, TIval, TIncMo, TDoy, TWoy: LongWord;
  k, c: Integer;
begin
  TSplit := FModule.TypeIndex([wvtF64], []);
  TCivil := FModule.TypeIndex([wvtI64], []);
  TDays  := FModule.TypeIndex([wvtI64, wvtI64, wvtI64], [wvtI64]);
  TIval  := FModule.TypeIndex([wvtI32], [wvtI32]);
  TIncMo := FModule.TypeIndex([wvtF64, wvtI64], [wvtF64]);
  TDoy   := FModule.TypeIndex([wvtI64], [wvtI64]);
  TWoy   := FModule.TypeIndex([wvtI64], [wvtI64]);

  { dtSplit(serial): DT_DAYS and DT_MS, by the one expression above. }
  B := TWasmBuf.Create;
  try
    // param 0 = serial; locals 1 = ms (i64)
    B.LocalGet(0); B.F64Const(MS_DAY); B.Op(wopF64Mul);
    B.LocalTee(0);
    B.F64Const(0); B.Op(wopF64Lt);
    B.BlockStart(wopIf, WASM_TYPE_F64);
      B.LocalGet(0); B.F64Const(0.5); B.Op(wopF64Sub);
    B.Op(wopElse);
      B.LocalGet(0); B.F64Const(0.5); B.Op(wopF64Add);
    B.EndOp;
    { ⛔ trunc_sat and not trunc: a serial far outside any calendar would TRAP
      the module where the interpreter merely answers nonsense. Saturating keeps
      a bad date a bad date instead of killing the program. }
    B.TruncSat(wopfcI64TruncSatF64S);
    B.LocalSet(1);
    B.I32Const(DT_DAYS);
    B.LocalGet(1); B.I64Const(MS_DAY); B.Op(wopI64DivS); B.Op(wopI32WrapI64);
    B.OpMem(wopI32Store, 2, 0);
    B.I32Const(DT_MS);
    // the magnitude first: Pascal's mod takes the sign of the dividend
    B.LocalGet(1); B.I64Const(0); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_TYPE_I64);
      B.I64Const(0); B.LocalGet(1); B.Op(wopI64Sub);
    B.Op(wopElse);
      B.LocalGet(1);
    B.EndOp;
    B.I64Const(MS_DAY); B.Op(wopI64RemS); B.Op(wopI32WrapI64);
    B.OpMem(wopI32Store, 2, 0);
    FModule.AddFunction(TSplit, [wvtI64], B);
  finally
    B.Free;
  end;

  { dtCivil(days): DT_Y, DT_MO, DT_D. The civil-from-days algorithm, with the
    era shifted so that March starts the year - which is what removes every
    special case for February and the leap day. }
  B := TWasmBuf.Create;
  try
    // param 0 = days; locals 1 = z, 2 = era, 3 = doe, 4 = yoe, 5 = doy, 6 = mp, 7 = y
    B.LocalGet(0); B.I64Const(25569); B.Op(wopI64Sub);   // -> days since 1970-01-01
    B.I64Const(719468); B.Op(wopI64Add);                 // -> days since 0000-03-01
    B.LocalSet(1);
    // era = (z >= 0 ? z : z - 146096) / 146097   -- a floor division
    B.LocalGet(1); B.I64Const(0); B.Op(wopI64GeS);
    B.BlockStart(wopIf, WASM_TYPE_I64);
      B.LocalGet(1);
    B.Op(wopElse);
      B.LocalGet(1); B.I64Const(146096); B.Op(wopI64Sub);
    B.EndOp;
    B.I64Const(146097); B.Op(wopI64DivS); B.LocalSet(2);
    B.LocalGet(1); B.LocalGet(2); B.I64Const(146097); B.Op(wopI64Mul); B.Op(wopI64Sub);
    B.LocalSet(3);                                       // doe in [0, 146096]
    B.LocalGet(3);
    B.LocalGet(3); B.I64Const(1460); B.Op(wopI64DivS); B.Op(wopI64Sub);
    B.LocalGet(3); B.I64Const(36524); B.Op(wopI64DivS); B.Op(wopI64Add);
    B.LocalGet(3); B.I64Const(146096); B.Op(wopI64DivS); B.Op(wopI64Sub);
    B.I64Const(365); B.Op(wopI64DivS); B.LocalSet(4);    // yoe in [0, 399]
    B.LocalGet(3);
    B.LocalGet(4); B.I64Const(365); B.Op(wopI64Mul);
    B.LocalGet(4); B.I64Const(4); B.Op(wopI64DivS); B.Op(wopI64Add);
    B.LocalGet(4); B.I64Const(100); B.Op(wopI64DivS); B.Op(wopI64Sub);
    B.Op(wopI64Sub); B.LocalSet(5);                      // doy in [0, 365]
    B.LocalGet(5); B.I64Const(5); B.Op(wopI64Mul); B.I64Const(2); B.Op(wopI64Add);
    B.I64Const(153); B.Op(wopI64DivS); B.LocalSet(6);    // mp in [0, 11]
    B.LocalGet(4); B.LocalGet(2); B.I64Const(400); B.Op(wopI64Mul); B.Op(wopI64Add);
    B.LocalSet(7);
    // d = doy - (153*mp + 2)/5 + 1 ; m = mp < 10 ? mp + 3 : mp - 9 ; y += m <= 2
    B.I32Const(DT_D);
    B.LocalGet(5);
    B.LocalGet(6); B.I64Const(153); B.Op(wopI64Mul); B.I64Const(2); B.Op(wopI64Add);
    B.I64Const(5); B.Op(wopI64DivS); B.Op(wopI64Sub);
    B.I64Const(1); B.Op(wopI64Add); B.Op(wopI32WrapI64);
    B.OpMem(wopI32Store, 2, 0);
    B.LocalGet(6); B.I64Const(10); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_TYPE_I64);
      B.LocalGet(6); B.I64Const(3); B.Op(wopI64Add);
    B.Op(wopElse);
      B.LocalGet(6); B.I64Const(9); B.Op(wopI64Sub);
    B.EndOp;
    B.LocalSet(6);
    B.I32Const(DT_MO); B.LocalGet(6); B.Op(wopI32WrapI64); B.OpMem(wopI32Store, 2, 0);
    B.I32Const(DT_Y);
    B.LocalGet(7);
    B.LocalGet(6); B.I64Const(2); B.Op(wopI64LeS); B.Op(wopI64ExtendI32U);
    B.Op(wopI64Add); B.Op(wopI32WrapI64);
    B.OpMem(wopI32Store, 2, 0);
    FModule.AddFunction(TCivil, [wvtI64, wvtI64, wvtI64, wvtI64, wvtI64, wvtI64, wvtI64], B);
  finally
    B.Free;
  end;

  { dtDays(y, m, d) -> the serial day. The exact inverse of dtCivil. }
  B := TWasmBuf.Create;
  try
    // params 0=y 1=m 2=d; locals 3 = yy, 4 = era, 5 = yoe, 6 = doy, 7 = doe
    B.LocalGet(0);
    B.LocalGet(1); B.I64Const(2); B.Op(wopI64LeS); B.Op(wopI64ExtendI32U);
    B.Op(wopI64Sub); B.LocalSet(3);
    B.LocalGet(3); B.I64Const(0); B.Op(wopI64GeS);
    B.BlockStart(wopIf, WASM_TYPE_I64);
      B.LocalGet(3);
    B.Op(wopElse);
      B.LocalGet(3); B.I64Const(399); B.Op(wopI64Sub);
    B.EndOp;
    B.I64Const(400); B.Op(wopI64DivS); B.LocalSet(4);
    B.LocalGet(3); B.LocalGet(4); B.I64Const(400); B.Op(wopI64Mul); B.Op(wopI64Sub);
    B.LocalSet(5);
    // doy = (153*(m + (m > 2 ? -3 : 9)) + 2)/5 + d - 1
    B.LocalGet(1);
    B.LocalGet(1); B.I64Const(2); B.Op(wopI64GtS);
    B.BlockStart(wopIf, WASM_TYPE_I64);
      B.I64Const(-3);
    B.Op(wopElse);
      B.I64Const(9);
    B.EndOp;
    B.Op(wopI64Add);
    B.I64Const(153); B.Op(wopI64Mul); B.I64Const(2); B.Op(wopI64Add);
    B.I64Const(5); B.Op(wopI64DivS);
    B.LocalGet(2); B.Op(wopI64Add); B.I64Const(1); B.Op(wopI64Sub);
    B.LocalSet(6);
    B.LocalGet(5); B.I64Const(365); B.Op(wopI64Mul);
    B.LocalGet(5); B.I64Const(4); B.Op(wopI64DivS); B.Op(wopI64Add);
    B.LocalGet(5); B.I64Const(100); B.Op(wopI64DivS); B.Op(wopI64Sub);
    B.LocalGet(6); B.Op(wopI64Add); B.LocalSet(7);
    B.LocalGet(4); B.I64Const(146097); B.Op(wopI64Mul);
    B.LocalGet(7); B.Op(wopI64Add);
    B.I64Const(719468); B.Op(wopI64Sub);
    B.I64Const(25569); B.Op(wopI64Add);
    FModule.AddFunction(TDays, [wvtI64, wvtI64, wvtI64, wvtI64, wvtI64], B);
  finally
    B.Free;
  end;

  { dtIval(s) -> the interval code. Trimmed and case-folded, exactly as
    IntervalCode does, and anything unrecognised is a DAY. }
  B := TWasmBuf.Create;
  try
    // param 0 = s; locals 1 = len, 2 = base, 3 = i
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(1);
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(2);
    { The names are 1, 2 or 4 characters and there are ten of them, so the whole
      table is a spelled-out comparison rather than a loop over literals: it is
      shorter in bytes than a data segment plus the loop that would read it. }
    for k := 0 to High(IVAL_NAME) do
    begin
      { ⛔ One BLOCK per name, and it has to be a block rather than the `if` this
        was first written as: a branch out of an `if` at depth 1 leaves the
        FUNCTION, which here means returning with nothing on the stack. The
        block gives the "try the next name" exit somewhere to land. }
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(1); B.I32Const(Length(IVAL_NAME[k])); B.Op(wopI32Ne);
        B.BrIf(0);
        for c := 1 to Length(IVAL_NAME[k]) do
        begin
          B.LocalGet(2); B.OpMem(wopI32Load8U, 0, LongWord(c - 1));
          B.I32Const($DF); B.Op(wopI32And);            // fold to upper case
          B.I32Const(Ord(UpCase(IVAL_NAME[k][c]))); B.Op(wopI32Ne);
          B.BrIf(0);                                   // a byte differs: next name
        end;
        B.I32Const(k); B.Op(wopReturn);
      B.EndOp;
    end;
    B.I32Const(4);
    FModule.AddFunction(TIval, [wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { dtIncMonth(serial, n) -> the serial n months later, with the day CLAMPED to
    the length of the month it lands in (31 January plus one month is 28
    February, measured). ⚠️ The time comes back as Abs(Frac(serial)) and not
    from dtSplit's rounded milliseconds - see the note at the top. }
  B := TWasmBuf.Create;
  try
    // params 0 = serial, 1 = n; locals 2 = y, 3 = m, 4 = d, 5 = dim, 6 = tm
    B.LocalGet(0); B.Call(FDtSplitFunc);
    B.I32Const(DT_DAYS); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S);
    B.Call(FDtCivilFunc);
    B.I32Const(DT_Y);  B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S); B.LocalSet(2);
    B.I32Const(DT_MO); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S); B.LocalSet(3);
    B.I32Const(DT_D);  B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S); B.LocalSet(4);
    { Months counted from zero make the carry a division instead of a loop, and
      a FLOOR division so that going backwards over January works. }
    B.LocalGet(2); B.I64Const(12); B.Op(wopI64Mul);
    B.LocalGet(3); B.Op(wopI64Add); B.I64Const(1); B.Op(wopI64Sub);
    B.LocalGet(1); B.Op(wopI64Add); B.LocalSet(6);
    B.LocalGet(6); B.I64Const(0); B.Op(wopI64GeS);
    B.BlockStart(wopIf, WASM_TYPE_I64);
      B.LocalGet(6);
    B.Op(wopElse);
      B.LocalGet(6); B.I64Const(11); B.Op(wopI64Sub);
    B.EndOp;
    B.I64Const(12); B.Op(wopI64DivS); B.LocalSet(2);
    B.LocalGet(6); B.LocalGet(2); B.I64Const(12); B.Op(wopI64Mul); B.Op(wopI64Sub);
    B.I64Const(1); B.Op(wopI64Add); B.LocalSet(3);
    { The length of the target month, as the difference between the first of it
      and the first of the next - no table, and the leap rule comes for free
      from dtDays. }
    B.LocalGet(2);
    B.LocalGet(3); B.I64Const(12); B.Op(wopI64Eq); B.Op(wopI64ExtendI32U);
    B.Op(wopI64Add);
    B.LocalGet(3); B.I64Const(12); B.Op(wopI64Eq);
    B.BlockStart(wopIf, WASM_TYPE_I64);
      B.I64Const(1);
    B.Op(wopElse);
      B.LocalGet(3); B.I64Const(1); B.Op(wopI64Add);
    B.EndOp;
    B.I64Const(1); B.Call(FDtDaysFunc);
    B.LocalGet(2); B.LocalGet(3); B.I64Const(1); B.Call(FDtDaysFunc);
    B.Op(wopI64Sub); B.LocalSet(5);
    B.LocalGet(4); B.LocalGet(5); B.Op(wopI64GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(5); B.LocalSet(4);
    B.EndOp;
    B.LocalGet(2); B.LocalGet(3); B.LocalGet(4); B.Call(FDtDaysFunc);
    B.Op(wopF64ConvertI64S);
    // + Abs(Frac(serial)), and BELOW zero the fraction is subtracted
    B.LocalGet(0); B.LocalGet(0); B.Op(wopF64Trunc); B.Op(wopF64Sub);
    B.Op(wopF64Abs);
    B.Op(wopF64Add);
    FModule.AddFunction(TIncMo, [wvtI64, wvtI64, wvtI64, wvtI64, wvtI64], B);
  finally
    B.Free;
  end;

  { dtDayOfYear(days) -> 1..366. }
  B := TWasmBuf.Create;
  try
    // param 0 = days; local 1 = jan1
    B.LocalGet(0); B.Call(FDtCivilFunc);
    B.I32Const(DT_Y); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S);
    B.I64Const(1); B.I64Const(1); B.Call(FDtDaysFunc);
    B.LocalSet(1);
    B.LocalGet(0); B.LocalGet(1); B.Op(wopI64Sub); B.I64Const(1); B.Op(wopI64Add);
    FModule.AddFunction(TDoy, [wvtI64], B);
  finally
    B.Free;
  end;

  { dtWeekOfYear(days) -> the VB week number, which is what fbc's DATEPART("ww")
    answers: week 1 is the week CONTAINING 1 January, and weeks start on SUNDAY.
    ⛔ NOT the ISO 8601 week (Monday-based, week 1 holds the first Thursday), which
    is what this emitted until 23 Aug 2026 and what FPC's WeekOfTheYear returns. The
    two agree most of the time - measured against fbc over 48 dates, 7 differed and
    every one was a SUNDAY, the day the VB week turns over and the ISO one does not -
    so the disagreement with the interpreter survived a full corpus run and only the
    WASM differential's calendar.bas showed it.
    ⭐ ONE arithmetic identity, the same one the interpreter uses:
        week = ((dayOfYear - 1) + (weekday(1 Jan) - 1)) div 7 + 1
    and both halves are differences of SERIALS here, so no calendar decode is needed
    beyond finding 1 January:
        dayOfYear - 1   = days - jan1
        weekday(x) - 1  = (x + 6) mod 7   (serial 0 is a Saturday, so 0 -> 6)
    Both terms are non-negative, so the floor division is a plain I64DivS. }
  B := TWasmBuf.Create;
  try
    // param 0 = days; locals 1 = jan1 serial, 2 = weekday(jan1) - 1, 3 = unused (kept
    // so the local list matches the other date helpers)
    B.LocalGet(0); B.Call(FDtCivilFunc);
    B.I32Const(DT_Y); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S);
    B.I64Const(1); B.I64Const(1); B.Call(FDtDaysFunc);
    B.LocalSet(1);
    B.LocalGet(1); B.I64Const(6); B.Op(wopI64Add);
    B.I64Const(7); B.Op(wopI64RemS);
    B.LocalTee(2); B.I64Const(0); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(2); B.I64Const(7); B.Op(wopI64Add); B.LocalSet(2);
    B.EndOp;
    B.LocalGet(0); B.LocalGet(1); B.Op(wopI64Sub);
    B.LocalGet(2); B.Op(wopI64Add);
    B.I64Const(7); B.Op(wopI64DivS);
    B.I64Const(1); B.Op(wopI64Add);
    FModule.AddFunction(TWoy, [wvtI64, wvtI64, wvtI64], B);
  finally
    B.Free;
  end;

  { dtYM(serial) -> year * 12 + (month - 1), the two calendar fields DATEDIFF
    needs in ONE value. ⭐ It exists so that the year, quarter and month
    differences can be plain arithmetic on a single number instead of two
    decodes fighting over the same memory cells - DT_Y and DT_MO belong to
    whoever decoded LAST, and DATEDIFF decodes twice. }
  B := TWasmBuf.Create;
  try
    // param 0 = serial
    B.LocalGet(0); B.Call(FDtSplitFunc);
    B.I32Const(DT_DAYS); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S);
    B.Call(FDtCivilFunc);
    B.I32Const(DT_Y); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S);
    B.I64Const(12); B.Op(wopI64Mul);
    B.I32Const(DT_MO); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S);
    B.Op(wopI64Add); B.I64Const(1); B.Op(wopI64Sub);
    FModule.AddFunction(FModule.TypeIndex([wvtF64], [wvtI64]), [], B);
  finally
    B.Free;
  end;

  { dtStr(kind) -> DATE$ ("mm-dd-yyyy") when kind is 0, TIME$ ("hh:mm:ss") when
    it is 1. DT_DAYS and DT_MS are already set by the caller's dtSplit.
    ⭐ Both fields are FIXED WIDTH and zero-padded, which is what FormatDateTime
    gives for mm/dd/hh/nn/ss - so there is no number formatter here at all, only
    digits written into a string of known length. }
  if FUsesDateStr then
  begin
    B := TWasmBuf.Create;
    try
      // param 0 = kind; locals 1 = handle, 2 = base, 3 = v
      B.LocalGet(0); B.I32Const(1); B.Op(wopI32Eq);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(8); B.Call(FStrNewFunc); B.LocalTee(1);
        B.I32Const(4); B.Op(wopI32Add); B.LocalSet(2);
        B.I32Const(DT_MS); B.OpMem(wopI32Load, 2, 0);
        B.I32Const(3600000); B.Op(wopI32DivS); B.LocalSet(3);
        EmitTwoDigits(B, 2, 3, 0);
        B.LocalGet(2); B.I32Const(Ord(':')); B.OpMem(wopI32Store8, 0, 2);
        B.I32Const(DT_MS); B.OpMem(wopI32Load, 2, 0);
        B.I32Const(60000); B.Op(wopI32DivS); B.I32Const(60); B.Op(wopI32RemS); B.LocalSet(3);
        EmitTwoDigits(B, 2, 3, 3);
        B.LocalGet(2); B.I32Const(Ord(':')); B.OpMem(wopI32Store8, 0, 5);
        B.I32Const(DT_MS); B.OpMem(wopI32Load, 2, 0);
        B.I32Const(1000); B.Op(wopI32DivS); B.I32Const(60); B.Op(wopI32RemS); B.LocalSet(3);
        EmitTwoDigits(B, 2, 3, 6);
      B.Op(wopElse);
        B.I32Const(DT_DAYS); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S);
        B.Call(FDtCivilFunc);
        B.I32Const(10); B.Call(FStrNewFunc); B.LocalTee(1);
        B.I32Const(4); B.Op(wopI32Add); B.LocalSet(2);
        B.I32Const(DT_MO); B.OpMem(wopI32Load, 2, 0); B.LocalSet(3);
        EmitTwoDigits(B, 2, 3, 0);
        B.LocalGet(2); B.I32Const(Ord('-')); B.OpMem(wopI32Store8, 0, 2);
        B.I32Const(DT_D); B.OpMem(wopI32Load, 2, 0); B.LocalSet(3);
        EmitTwoDigits(B, 2, 3, 3);
        B.LocalGet(2); B.I32Const(Ord('-')); B.OpMem(wopI32Store8, 0, 5);
        // the year, four digits: the top two, then the bottom two
        B.I32Const(DT_Y); B.OpMem(wopI32Load, 2, 0);
        B.I32Const(100); B.Op(wopI32DivS); B.LocalSet(3);
        EmitTwoDigits(B, 2, 3, 6);
        B.I32Const(DT_Y); B.OpMem(wopI32Load, 2, 0);
        B.I32Const(100); B.Op(wopI32RemS); B.LocalSet(3);
        EmitTwoDigits(B, 2, 3, 8);
      B.EndOp;
      B.LocalGet(1);
      FModule.AddFunction(FModule.TypeIndex([wvtI32], [wvtI32]),
                          [wvtI32, wvtI32, wvtI32], B);
    finally
      B.Free;
    end;
  end;
end;

{ RND and RANDOMIZE: MT19937, transcribed.

  ⭐ WHICH generator was MEASURED rather than read out of the RTL: seeding 42 and
  asking for numbers gives 1608637542 and 0.37454011430963874, which is
  init_genrand(42) followed by genrand_int32 divided by 2^32, to the last digit.
  ⚠️ Dividing by 2^32 is the one division by a literal that is safe, and for a
  reason worth stating after the day this file has had: the reciprocal of a power
  of two is EXACT, so the rewrite an optimiser might apply cannot move a bit.

  ⛔ Only a SEEDED sequence reproduces. Unseeded, sb takes its seed from the
  clock, so the two sides were never going to agree - which is why the guard
  calls RANDOMIZE with a number, and why a host import was never an option: it
  could not reproduce a seeded sequence at all. }
procedure TWasmBackend.EmitRndHelpers;
const
  MT_N = 624;
  MT_IDX = MT_N * 4;          // the read cursor sits just past the state
var
  B: TWasmBuf;
begin
  if not FUsesRnd then Exit;

  { mtSeed(s): allocate on first use, then init_genrand. }
  B := TWasmBuf.Create;
  try
    // param 0 = seed; locals 1 = base, 2 = i, 3 = prev
    B.GlobalGet(FMtBaseG); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(MT_IDX + 4); B.Call(FAllocFunc); B.GlobalSet(FMtBaseG);
    B.EndOp;
    B.GlobalGet(FMtBaseG); B.LocalSet(1);
    B.LocalGet(1); B.LocalGet(0); B.Op(wopI32WrapI64); B.LocalTee(3);
    B.OpMem(wopI32Store, 2, 0);
    B.I32Const(1); B.LocalSet(2);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(2); B.I32Const(MT_N); B.Op(wopI32GeU); B.BrIf(1);
        // mt[i] = 1812433253 * (mt[i-1] xor (mt[i-1] >>> 30)) + i
        B.LocalGet(1); B.LocalGet(2); B.I32Const(4); B.Op(wopI32Mul); B.Op(wopI32Add);
        B.LocalGet(3);
        B.LocalGet(3); B.I32Const(30); B.Op(wopI32ShrU); B.Op(wopI32Xor);
        B.I32Const(1812433253); B.Op(wopI32Mul);
        B.LocalGet(2); B.Op(wopI32Add);
        B.LocalTee(3);
        B.OpMem(wopI32Store, 2, 0);
        B.LocalGet(2); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(2);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    // the cursor past the end, so the first read twists
    B.LocalGet(1); B.I32Const(MT_N); B.OpMem(wopI32Store, 2, MT_IDX);
    FModule.AddFunction(FModule.TypeIndex([wvtI64], []),
                        [wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { mtNext() -> the next 32 bits, zero-extended. }
  B := TWasmBuf.Create;
  try
    // locals 0 = base, 1 = i, 2 = y, 3 = k
    B.GlobalGet(FMtBaseG); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      { ⛔ Unseeded, the module seeds ITSELF with a fixed number rather than
        asking anything: a module that answers the same on every run is the
        honest behaviour for a sandbox, and it cannot match sb's clock-derived
        seed whatever it does. RANDOMIZE overrides it. }
      B.I64Const(1); B.Call(FMtSeedFunc);
    B.EndOp;
    B.GlobalGet(FMtBaseG); B.LocalSet(0);
    B.LocalGet(0); B.OpMem(wopI32Load, 2, MT_IDX); B.LocalTee(1);
    B.I32Const(MT_N); B.Op(wopI32GeU);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      // the twist, in one pass with the wrap done by a modulo of the index
      B.I32Const(0); B.LocalSet(3);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(3); B.I32Const(MT_N); B.Op(wopI32GeU); B.BrIf(1);
          // y = (mt[k] and $80000000) + (mt[(k+1) mod N] and $7FFFFFFF)
          B.LocalGet(0); B.LocalGet(3); B.I32Const(4); B.Op(wopI32Mul); B.Op(wopI32Add);
          B.OpMem(wopI32Load, 2, 0);
          B.I32Const(Integer($80000000)); B.Op(wopI32And);
          B.LocalGet(0);
          B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add);
          B.I32Const(MT_N); B.Op(wopI32RemU);
          B.I32Const(4); B.Op(wopI32Mul); B.Op(wopI32Add);
          B.OpMem(wopI32Load, 2, 0);
          B.I32Const($7FFFFFFF); B.Op(wopI32And);
          B.Op(wopI32Add); B.LocalSet(2);
          // mt[k] = mt[(k+397) mod N] xor (y >>> 1) xor (y and 1 ? 2567483615 : 0)
          B.LocalGet(0); B.LocalGet(3); B.I32Const(4); B.Op(wopI32Mul); B.Op(wopI32Add);
          B.LocalGet(0);
          B.LocalGet(3); B.I32Const(397); B.Op(wopI32Add);
          B.I32Const(MT_N); B.Op(wopI32RemU);
          B.I32Const(4); B.Op(wopI32Mul); B.Op(wopI32Add);
          B.OpMem(wopI32Load, 2, 0);
          B.LocalGet(2); B.I32Const(1); B.Op(wopI32ShrU); B.Op(wopI32Xor);
          B.LocalGet(2); B.I32Const(1); B.Op(wopI32And);
          B.BlockStart(wopIf, WASM_TYPE_I32);
            B.I32Const(Integer(2567483615));
          B.Op(wopElse);
            B.I32Const(0);
          B.EndOp;
          B.Op(wopI32Xor);
          B.OpMem(wopI32Store, 2, 0);
          B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(3);
          B.Br(0);
        B.EndOp;
      B.EndOp;
      B.I32Const(0); B.LocalSet(1);
    B.EndOp;
    B.LocalGet(0); B.LocalGet(1); B.I32Const(4); B.Op(wopI32Mul); B.Op(wopI32Add);
    B.OpMem(wopI32Load, 2, 0); B.LocalSet(2);
    B.LocalGet(0); B.LocalGet(1); B.I32Const(1); B.Op(wopI32Add);
    B.OpMem(wopI32Store, 2, MT_IDX);
    // the tempering
    B.LocalGet(2); B.LocalGet(2); B.I32Const(11); B.Op(wopI32ShrU); B.Op(wopI32Xor);
    B.LocalTee(2);
    B.LocalGet(2); B.I32Const(7); B.Op(wopI32Shl);
    B.I32Const(Integer(2636928640)); B.Op(wopI32And); B.Op(wopI32Xor);
    B.LocalTee(2);
    B.LocalGet(2); B.I32Const(15); B.Op(wopI32Shl);
    B.I32Const(Integer(4022730752)); B.Op(wopI32And); B.Op(wopI32Xor);
    B.LocalTee(2);
    B.LocalGet(2); B.I32Const(18); B.Op(wopI32ShrU); B.Op(wopI32Xor);
    B.Op(wopI64ExtendI32U);
    FModule.AddFunction(FModule.TypeIndex([], [wvtI64]),
                        [wvtI32, wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;
end;

{ The DATEVALUE / TIMEVALUE / ISDATE parser, as five small functions rather than
  one long body - each has its own locals, which is what keeps the branch depths
  readable.

  ⛔ THE DETERMINISTIC FORMS ONLY, and that is a declared boundary and not an
  omission: the interpreter hands anything it does not recognise to the HOST's
  date parser, whose answer depends on the machine's regional settings, and a
  sandbox has no host to ask. This project already chose the deterministic path
  as its default for exactly that reason - a result that changes with the
  machine is not diffable and its baselines are not reproducible - so the module
  simply has nothing else to fall into. Accepted: "yyyy-mm-dd", "yyyy/mm/dd",
  "hh:mm[:ss]", either alone, the two separated by a space, with an optional
  trailing AM or PM. Anything else answers 0 / false. }
procedure TWasmBackend.EmitDateParseHelpers;
var
  B: TWasmBuf;
begin
  if not FUsesDateParse then Exit;

  { dtFind(base, from, upto, ch) -> the first index holding ch, or -1. }
  B := TWasmBuf.Create;
  try
    // params 0=base 1=from 2=upto 3=ch; local 4=i
    B.LocalGet(1); B.LocalSet(4);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(4); B.LocalGet(2); B.Op(wopI32GeS); B.BrIf(1);
        B.LocalGet(0); B.LocalGet(4); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0);
        B.LocalGet(3); B.Op(wopI32Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(4); B.Op(wopReturn);
        B.EndOp;
        B.LocalGet(4); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(4);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    B.I32Const(-1);
    FModule.AddFunction(FModule.TypeIndex([wvtI32, wvtI32, wvtI32, wvtI32], [wvtI32]),
                        [wvtI32], B);
  finally
    B.Free;
  end;

  { dtFields(base, len, sep) -> the field count (2 or 3), or -1, with the values
    at DT_F0..DT_F2. Mirrors SplitInts: spaces around a field are ignored, a
    THIRD field that is not a number becomes 0 rather than failing, but a bad
    first or second field fails the whole split - which is the asymmetry that
    lets "12:30" parse while "12:ab" does not. }
  B := TWasmBuf.Create;
  try
    // params 0=base 1=len 2=sep; locals 3=i 4=n 5=acc 6=neg 7=any 8=bad 9=c 10=v
    B.I32Const(DT_F0); B.I32Const(0); B.OpMem(wopI32Store, 2, 0);
    B.I32Const(DT_F1); B.I32Const(0); B.OpMem(wopI32Store, 2, 0);
    B.I32Const(DT_F2); B.I32Const(0); B.OpMem(wopI32Store, 2, 0);
    B.I32Const(0); B.LocalSet(3);
    B.I32Const(0); B.LocalSet(4);
    B.I32Const(0); B.LocalSet(5);
    B.I32Const(0); B.LocalSet(6);
    B.I32Const(0); B.LocalSet(7);
    B.I32Const(0); B.LocalSet(8);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(3); B.LocalGet(1); B.Op(wopI32GeS); B.BrIf(1);
        B.LocalGet(0); B.LocalGet(3); B.Op(wopI32Add);
        B.OpMem(wopI32Load8U, 0, 0); B.LocalSet(9);
        B.LocalGet(9); B.LocalGet(2); B.Op(wopI32Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          EmitFlushField(B);
          B.LocalGet(4); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(4);
          B.LocalGet(4); B.I32Const(2); B.Op(wopI32GtS);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.I32Const(-1); B.Op(wopReturn);              // more than three fields
          B.EndOp;
          B.I32Const(0); B.LocalSet(5);
          B.I32Const(0); B.LocalSet(6);
          B.I32Const(0); B.LocalSet(7);
          B.I32Const(0); B.LocalSet(8);
        B.Op(wopElse);
          B.LocalGet(9); B.I32Const(Ord('0')); B.Op(wopI32GeU);
          B.LocalGet(9); B.I32Const(Ord('9')); B.Op(wopI32LeU);
          B.Op(wopI32And);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(5); B.I32Const(10); B.Op(wopI32Mul);
            B.LocalGet(9); B.I32Const(Ord('0')); B.Op(wopI32Sub); B.Op(wopI32Add);
            B.LocalSet(5);
            B.I32Const(1); B.LocalSet(7);
          B.Op(wopElse);
            B.LocalGet(9); B.I32Const(Ord('-')); B.Op(wopI32Eq);
            B.LocalGet(7); B.Op(wopI32Eqz); B.Op(wopI32And);
            B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
              B.I32Const(1); B.LocalSet(6);
            B.Op(wopElse);
              B.LocalGet(9); B.I32Const(32); B.Op(wopI32Ne);
              B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
                B.I32Const(1); B.LocalSet(8);
              B.EndOp;
            B.EndOp;
          B.EndOp;
        B.EndOp;
        B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(3);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    EmitFlushField(B);
    // one field alone is never a date or a time
    B.LocalGet(4); B.I32Const(1); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(-1); B.Op(wopReturn);
    B.EndOp;
    B.I32Const(DT_F0); B.OpMem(wopI32Load, 2, 0); B.I32Const(DT_BAD); B.Op(wopI32Eq);
    B.I32Const(DT_F1); B.OpMem(wopI32Load, 2, 0); B.I32Const(DT_BAD); B.Op(wopI32Eq);
    B.Op(wopI32Or);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(-1); B.Op(wopReturn);
    B.EndOp;
    // a third field that is not a number is zero, exactly as StrToIntDef(…, 0) gives it
    B.I32Const(DT_F2); B.OpMem(wopI32Load, 2, 0); B.I32Const(DT_BAD); B.Op(wopI32Eq);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(DT_F2); B.I32Const(0); B.OpMem(wopI32Store, 2, 0);
    B.EndOp;
    B.LocalGet(4); B.I32Const(1); B.Op(wopI32Add);
    FModule.AddFunction(FModule.TypeIndex([wvtI32, wvtI32, wvtI32], [wvtI32]),
                        [wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { dtDatePart(base, len) -> 1 and DT_PD when the range is a date. }
  B := TWasmBuf.Create;
  try
    // params 0=base 1=len; locals 2=sep 3=y 4=mo 5=d 6=dim
    B.LocalGet(0); B.I32Const(0); B.LocalGet(1); B.I32Const(Ord('-'));
    B.Call(FDtFindFunc);
    B.I32Const(0); B.Op(wopI32GeS);
    B.BlockStart(wopIf, WASM_TYPE_I32);
      B.I32Const(Ord('-'));
    B.Op(wopElse);
      B.LocalGet(0); B.I32Const(0); B.LocalGet(1); B.I32Const(Ord('/'));
      B.Call(FDtFindFunc);
      B.I32Const(0); B.Op(wopI32GeS);
      B.BlockStart(wopIf, WASM_TYPE_I32);
        B.I32Const(Ord('/'));
      B.Op(wopElse);
        B.I32Const(0);                                    // no separator: not a date we know
      B.EndOp;
    B.EndOp;
    B.LocalTee(2); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(0); B.LocalGet(1); B.LocalGet(2); B.Call(FDtFieldsFunc);
    B.I32Const(3); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.Op(wopReturn);                     // "yyyy-mm" is not a date
    B.EndOp;
    B.I32Const(DT_F0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(3);
    B.I32Const(DT_F1); B.OpMem(wopI32Load, 2, 0); B.LocalSet(4);
    B.I32Const(DT_F2); B.OpMem(wopI32Load, 2, 0); B.LocalSet(5);
    { The same ranges TryEncodeDate enforces, the day against the length of ITS
      OWN month - which comes from the calendar (the 1st of the next month minus
      the 1st of this one), so February and the leap rule need no special case. }
    B.LocalGet(3); B.I32Const(1); B.Op(wopI32LtS);
    B.LocalGet(3); B.I32Const(9999); B.Op(wopI32GtS); B.Op(wopI32Or);
    B.LocalGet(4); B.I32Const(1); B.Op(wopI32LtS); B.Op(wopI32Or);
    B.LocalGet(4); B.I32Const(12); B.Op(wopI32GtS); B.Op(wopI32Or);
    B.LocalGet(5); B.I32Const(1); B.Op(wopI32LtS); B.Op(wopI32Or);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(3);
    B.LocalGet(4); B.I32Const(12); B.Op(wopI32Eq); B.Op(wopI32Add); B.Op(wopI64ExtendI32S);
    B.LocalGet(4); B.I32Const(12); B.Op(wopI32Eq);
    B.BlockStart(wopIf, WASM_TYPE_I64);
      B.I64Const(1);
    B.Op(wopElse);
      B.LocalGet(4); B.I32Const(1); B.Op(wopI32Add); B.Op(wopI64ExtendI32S);
    B.EndOp;
    B.I64Const(1); B.Call(FDtDaysFunc);
    B.LocalGet(3); B.Op(wopI64ExtendI32S);
    B.LocalGet(4); B.Op(wopI64ExtendI32S);
    B.I64Const(1); B.Call(FDtDaysFunc);
    B.Op(wopI64Sub); B.Op(wopI32WrapI64); B.LocalSet(6);
    B.LocalGet(5); B.LocalGet(6); B.Op(wopI32GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.Op(wopReturn);
    B.EndOp;
    B.I32Const(DT_PD);
    B.LocalGet(3); B.Op(wopI64ExtendI32S);
    B.LocalGet(4); B.Op(wopI64ExtendI32S);
    B.LocalGet(5); B.Op(wopI64ExtendI32S);
    B.Call(FDtDaysFunc); B.Op(wopF64ConvertI64S);
    B.OpMem(wopF64Store, 3, 0);
    B.I32Const(1);
    FModule.AddFunction(FModule.TypeIndex([wvtI32, wvtI32], [wvtI32]),
                        [wvtI32, wvtI32, wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { dtTimePart(base, len) -> 1 and DT_PT when the range is a time. }
  B := TWasmBuf.Create;
  try
    // params 0=base 1=len; locals 2=h 3=m 4=s
    B.LocalGet(0); B.I32Const(0); B.LocalGet(1); B.I32Const(Ord(':'));
    B.Call(FDtFindFunc);
    B.I32Const(0); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(0); B.LocalGet(1); B.I32Const(Ord(':')); B.Call(FDtFieldsFunc);
    B.I32Const(2); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.Op(wopReturn);
    B.EndOp;
    B.I32Const(DT_F0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(2);
    B.I32Const(DT_F1); B.OpMem(wopI32Load, 2, 0); B.LocalSet(3);
    B.I32Const(DT_F2); B.OpMem(wopI32Load, 2, 0); B.LocalSet(4);
    B.LocalGet(2); B.I32Const(0); B.Op(wopI32LtS);
    B.LocalGet(2); B.I32Const(23); B.Op(wopI32GtS); B.Op(wopI32Or);
    B.LocalGet(3); B.I32Const(0); B.Op(wopI32LtS); B.Op(wopI32Or);
    B.LocalGet(3); B.I32Const(59); B.Op(wopI32GtS); B.Op(wopI32Or);
    B.LocalGet(4); B.I32Const(0); B.Op(wopI32LtS); B.Op(wopI32Or);
    B.LocalGet(4); B.I32Const(59); B.Op(wopI32GtS); B.Op(wopI32Or);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.Op(wopReturn);
    B.EndOp;
    { The seconds count is an INTEGER before it becomes a fraction, for the
      reason bcTimeSerial had to learn twice: one conversion, one division. }
    B.I32Const(DT_PT);
    B.LocalGet(2); B.I32Const(3600); B.Op(wopI32Mul);
    B.LocalGet(3); B.I32Const(60); B.Op(wopI32Mul); B.Op(wopI32Add);
    B.LocalGet(4); B.Op(wopI32Add);
    B.Op(wopF64ConvertI32S);
    B.F64Const(86400.0); B.Op(wopF64Div);
    B.OpMem(wopF64Store, 3, 0);
    B.I32Const(1);
    FModule.AddFunction(FModule.TypeIndex([wvtI32, wvtI32], [wvtI32]),
                        [wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  { dtParse(s) -> 1 and DT_PARSE. }
  B := TWasmBuf.Create;
  try
    // param 0=s; locals 1=base 2=b 3=e 4=ampm 5=sp 6=dEnd 7=tStart
    //            8=haveD 9=haveT 10=c
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(1);
    B.I32Const(0); B.LocalSet(2);
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(3);
    B.I32Const(0); B.LocalSet(4);
    B.I32Const(0); B.LocalSet(8);
    B.I32Const(0); B.LocalSet(9);
    B.I32Const(DT_PARSE); B.F64Const(0); B.OpMem(wopF64Store, 3, 0);
    B.I32Const(DT_PD); B.F64Const(0); B.OpMem(wopF64Store, 3, 0);
    B.I32Const(DT_PT); B.F64Const(0); B.OpMem(wopF64Store, 3, 0);
    EmitTrimRange(B, 1, 2, 3, 10);
    B.LocalGet(3); B.LocalGet(2); B.Op(wopI32LeS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.Op(wopReturn);
    B.EndOp;

    { ⛔ THE 12-HOUR MARKER COMES OFF FIRST, before anything else looks at the
      string. The interpreter learned that the hard way: "07:12:28 AM" otherwise
      splits on the SPACE, and "07:12:28" goes to the date parser, which fails
      and takes the whole value down with it. }
    B.LocalGet(3); B.LocalGet(2); B.Op(wopI32Sub); B.I32Const(2); B.Op(wopI32GeS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(1); B.LocalGet(3); B.Op(wopI32Add); B.I32Const(1); B.Op(wopI32Sub);
      B.OpMem(wopI32Load8U, 0, 0); B.I32Const($DF); B.Op(wopI32And);
      B.I32Const(Ord('M')); B.Op(wopI32Eq);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(1); B.LocalGet(3); B.Op(wopI32Add); B.I32Const(2); B.Op(wopI32Sub);
        B.OpMem(wopI32Load8U, 0, 0); B.I32Const($DF); B.Op(wopI32And); B.LocalSet(10);
        B.LocalGet(10); B.I32Const(Ord('P')); B.Op(wopI32Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(2); B.LocalSet(4);
        B.Op(wopElse);
          B.LocalGet(10); B.I32Const(Ord('A')); B.Op(wopI32Eq);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.I32Const(1); B.LocalSet(4);
          B.EndOp;
        B.EndOp;
        B.LocalGet(4);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(3); B.I32Const(2); B.Op(wopI32Sub); B.LocalSet(3);
          EmitTrimRange(B, 1, 2, 3, 10);
        B.EndOp;
      B.EndOp;
    B.EndOp;
    B.LocalGet(3); B.LocalGet(2); B.Op(wopI32LeS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.Op(wopReturn);
    B.EndOp;

    // a space splits date from time; failing that, a colon means it is all time
    B.LocalGet(1); B.LocalGet(2); B.LocalGet(3); B.I32Const(Ord(' '));
    B.Call(FDtFindFunc); B.LocalTee(5);
    B.I32Const(0); B.Op(wopI32GeS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(5); B.LocalSet(6);
      B.LocalGet(5); B.LocalSet(7);
    B.Op(wopElse);
      B.LocalGet(1); B.LocalGet(2); B.LocalGet(3); B.I32Const(Ord(':'));
      B.Call(FDtFindFunc);
      B.I32Const(0); B.Op(wopI32GeS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(2); B.LocalSet(6);                     // no date part
        B.LocalGet(2); B.LocalSet(7);
      B.Op(wopElse);
        B.LocalGet(3); B.LocalSet(6);                     // no time part
        B.LocalGet(3); B.LocalSet(7);
      B.EndOp;
    B.EndOp;

    B.LocalGet(6); B.LocalGet(2); B.Op(wopI32GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(1); B.LocalGet(2); B.Op(wopI32Add);
      B.LocalGet(6); B.LocalGet(2); B.Op(wopI32Sub);
      B.Call(FDtDatePartFunc); B.LocalTee(8);
      B.Op(wopI32Eqz);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(0); B.Op(wopReturn);
      B.EndOp;
    B.EndOp;

    EmitTrimRange(B, 1, 7, 3, 10);
    B.LocalGet(7); B.LocalGet(3); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(1); B.LocalGet(7); B.Op(wopI32Add);
      B.LocalGet(3); B.LocalGet(7); B.Op(wopI32Sub);
      B.Call(FDtTimePartFunc); B.LocalTee(9);
      B.Op(wopI32Eqz);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(0); B.Op(wopReturn);
      B.EndOp;
    B.EndOp;

    B.LocalGet(8); B.LocalGet(9); B.Op(wopI32Or); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.Op(wopReturn);
    B.EndOp;

    { Half a day is exactly noon, so "before noon" is the whole test: 12:30 PM
      stays 12:30 and 12:30 AM becomes 00:30, which is the rule VB and
      FreeBASIC follow. }
    B.LocalGet(9);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(4); B.I32Const(2); B.Op(wopI32Eq);
      B.I32Const(DT_PT); B.OpMem(wopF64Load, 3, 0); B.F64Const(0.5); B.Op(wopF64Lt);
      B.Op(wopI32And);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(DT_PT);
        B.I32Const(DT_PT); B.OpMem(wopF64Load, 3, 0); B.F64Const(0.5); B.Op(wopF64Add);
        B.OpMem(wopF64Store, 3, 0);
      B.Op(wopElse);
        B.LocalGet(4); B.I32Const(1); B.Op(wopI32Eq);
        B.I32Const(DT_PT); B.OpMem(wopF64Load, 3, 0); B.F64Const(0.5); B.Op(wopF64Ge);
        B.Op(wopI32And);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(DT_PT);
          B.I32Const(DT_PT); B.OpMem(wopF64Load, 3, 0); B.F64Const(0.5); B.Op(wopF64Sub);
          B.OpMem(wopF64Store, 3, 0);
        B.EndOp;
      B.EndOp;
    B.EndOp;

    B.I32Const(DT_PARSE);
    B.I32Const(DT_PD); B.OpMem(wopF64Load, 3, 0);
    B.I32Const(DT_PT); B.OpMem(wopF64Load, 3, 0);
    B.Op(wopF64Add);
    B.OpMem(wopF64Store, 3, 0);
    B.I32Const(1);
    FModule.AddFunction(FModule.TypeIndex([wvtI32], [wvtI32]),
                        [wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32,
                         wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;
end;

{ dtFields' "close the field I was accumulating" step, emitted at each separator
  and once at the end. A field that never held a digit, or held something that is
  neither a digit nor a sign nor padding, stores the SENTINEL - which the caller
  turns into a failure for the first two fields and into a zero for the third. }
procedure TWasmBackend.EmitFlushField(B: TWasmBuf);
begin
  B.I32Const(DT_F0);
  B.LocalGet(4); B.I32Const(4); B.Op(wopI32Mul); B.Op(wopI32Add);
  B.LocalGet(8); B.LocalGet(7); B.Op(wopI32Eqz); B.Op(wopI32Or);
  B.BlockStart(wopIf, WASM_TYPE_I32);
    B.I32Const(DT_BAD);
  B.Op(wopElse);
    B.LocalGet(6);
    B.BlockStart(wopIf, WASM_TYPE_I32);
      B.I32Const(0); B.LocalGet(5); B.Op(wopI32Sub);
    B.Op(wopElse);
      B.LocalGet(5);
    B.EndOp;
  B.EndOp;
  B.OpMem(wopI32Store, 2, 0);
end;

{ Move Lo forward and Hi back over padding - every byte at or below 32, which is
  what Trim removes. Tmp is a scratch local the caller owns. }
procedure TWasmBackend.EmitTrimRange(B: TWasmBuf; Base, Lo, Hi, Tmp: LongWord);
begin
  B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
    B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(Lo); B.LocalGet(Hi); B.Op(wopI32GeS); B.BrIf(1);
      B.LocalGet(Base); B.LocalGet(Lo); B.Op(wopI32Add);
      B.OpMem(wopI32Load8U, 0, 0); B.I32Const(32); B.Op(wopI32GtU); B.BrIf(1);
      B.LocalGet(Lo); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(Lo);
      B.Br(0);
    B.EndOp;
  B.EndOp;
  B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
    B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(Hi); B.LocalGet(Lo); B.Op(wopI32LeS); B.BrIf(1);
      B.LocalGet(Base); B.LocalGet(Hi); B.Op(wopI32Add); B.I32Const(1); B.Op(wopI32Sub);
      B.OpMem(wopI32Load8U, 0, 0); B.I32Const(32); B.Op(wopI32GtU); B.BrIf(1);
      B.LocalGet(Hi); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(Hi);
      B.Br(0);
    B.EndOp;
  B.EndOp;
  if Tmp = 0 then ;   // the parameter exists so callers can pass their scratch
end;

{ Two zero-padded decimal digits of local Val, written at Base + Off. }
procedure TWasmBackend.EmitTwoDigits(B: TWasmBuf; Base, Val: LongWord; Off: LongWord);
begin
  B.LocalGet(Base);
  B.LocalGet(Val); B.I32Const(10); B.Op(wopI32DivS);
  B.I32Const(Ord('0')); B.Op(wopI32Add);
  B.OpMem(wopI32Store8, 0, Off);
  B.LocalGet(Base);
  B.LocalGet(Val); B.I32Const(10); B.Op(wopI32RemS);
  B.I32Const(Ord('0')); B.Op(wopI32Add);
  B.OpMem(wopI32Store8, 0, Off + 1);
end;

{ The MONTHNAME / WEEKDAYNAME data segment: nineteen i32 handle addresses, then
  the nineteen strings as ordinary [len][bytes] headers, each 4-aligned.
  ⭐ Resolving the handles HERE rather than at run time is what makes the opcode
  a single i32.load with no helper: the addresses are known the moment the base
  of the block is, so the table can hold the finished handles. }
function TWasmBackend.BuildDateNameBlob: AnsiString;
const
  DT_NAME: array[0..DT_NAME_COUNT - 1] of AnsiString =
    ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday',
     'January', 'February', 'March', 'April', 'May', 'June',
     'July', 'August', 'September', 'October', 'November', 'December');
var
  k, Head: Integer;
  Addr: LongWord;
  Body: AnsiString;
begin
  Head := DT_NAME_COUNT * 4;
  SetLength(Result, Head);
  Body := '';
  Addr := DT_NAMES + LongWord(Head);
  for k := 0 to DT_NAME_COUNT - 1 do
  begin
    PLongWord(@Result[1 + k * 4])^ := Addr;
    SetLength(Body, Length(Body) + 4 + Length(DT_NAME[k]));
    PLongWord(@Body[Length(Body) - 3 - Length(DT_NAME[k])])^ := LongWord(Length(DT_NAME[k]));
    Move(DT_NAME[k][1], Body[Length(Body) - Length(DT_NAME[k]) + 1], Length(DT_NAME[k]));
    Inc(Addr, LongWord(4 + Length(DT_NAME[k])));
    while (Addr and 3) <> 0 do
    begin
      Body := Body + #0;
      Inc(Addr);
    end;
  end;
  Result := Result + Body;
end;

{ ERR$(n)'s message table: the handles first, then the strings, laid out exactly
  as the month names are.

  ⭐⭐ THE MESSAGES ARE NOT TRANSCRIBED - the table is built by CALLING the
  interpreter's own GetFBErrorCodeDescription - the FreeBASIC table, this target
  being MODERN-only - so the two cannot drift apart by a
  word, an apostrophe or a code. A copied table would be right on the day it was
  written and wrong the first time somebody adds an error.
  ⛔ It sits in a FIXED region under the string literals, and the fit is tight -
  so it is CHECKED. Silently running past STR_CONST_BASE would overwrite the
  program's own literals, which is a defect nothing downstream could explain. }
function TWasmBackend.BuildErrNameBlob: AnsiString;
var
  k, Slot, Head: Integer;
  Addr: LongWord;
  Body, M: AnsiString;

  procedure Put(ASlot: Integer; const S: AnsiString);
  begin
    PLongWord(@Result[1 + ASlot * 4])^ := Addr;
    SetLength(Body, Length(Body) + 4 + Length(S));
    PLongWord(@Body[Length(Body) - 3 - Length(S)])^ := LongWord(Length(S));
    if Length(S) > 0 then
      Move(S[1], Body[Length(Body) - Length(S) + 1], Length(S));
    Inc(Addr, LongWord(4 + Length(S)));
    while (Addr and 3) <> 0 do
    begin
      Body := Body + #0;
      Inc(Addr);
    end;
  end;

begin
  Head := ERR_SLOTS * 4;
  SetLength(Result, Head);
  FillChar(Result[1], Head, 0);
  Body := '';
  Addr := ERR_NAMES + LongWord(Head);
  { ⛔ THE FREEBASIC TABLE, because this target is MODERN-only. The numbers had been separated by
    dialect since the beginning and the WORDS had not: ERR$(5) answered DEVICE NOT PRESENT (Commodore
    5) for a program whose Err() reports FreeBASIC codes, where 5 is "Illegal resume". Our own
    extended codes (100..113) keep their own messages inside GetFBErrorCodeDescription - FreeBASIC
    defines nothing there, so there is nothing to collide with. }
  { ⭐ A code whose message IS the fallback gets no slot at all: the handle stays 0, which is exactly
    what the emitted lookup already tests to build "user error <n>" at run time. FreeBASIC defines only
    0..17, so codes 18..41 are all fallback here where the Commodore table had a word for each - and
    storing those 24 strings is what pushed the blob past STR_CONST_BASE the moment the table became
    FreeBASIC's. ⛔ Code 0 is NOT one of them: its message is genuinely the empty string. }
  for k := 0 to ERR_LO_MAX do
  begin
    M := AnsiString(SedaiExecutorErrors.GetFBErrorCodeDescription(k));
    if M <> AnsiString('user error ' + IntToStr(k)) then Put(k, M);
  end;
  for k := ERR_HI_LO to ERR_HI_MAX do
  begin
    Slot := (ERR_LO_MAX + 1) + (k - ERR_HI_LO);
    M := AnsiString(SedaiExecutorErrors.GetFBErrorCodeDescription(k));
    if M <> AnsiString('user error ' + IntToStr(k)) then Put(Slot, M);
  end;
  { The last slot is the word the fallback prefixes to a code with no message of
    its own - "user error 42" - which is what the interpreter's else branch formats. }
  Put(ERR_SLOTS - 1, 'user error ');
  Result := Result + Body;
  if LongWord(Length(Result)) + ERR_NAMES > STR_CONST_BASE then
    raise Exception.CreateFmt('the ERR$ message table no longer fits below the string ' +
      'literals: %d bytes at %d runs past %d', [Length(Result), ERR_NAMES, STR_CONST_BASE]);
end;

{ DT_DAYS as an i64, which every arm below starts from. }
procedure TWasmBackend.EmitDtDays(B: TWasmBuf);
begin
  B.I32Const(DT_DAYS); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S);
end;

{ The weekday of DT_DAYS, 1 = Sunday .. 7 = Saturday. Serial 0 is a Saturday
  (measured), so serial + 6 is a Sunday-based count - and the remainder is taken
  twice because Pascal's mod, and WASM's, keep the sign of the dividend and a
  serial before 1899 is negative. }
procedure TWasmBackend.EmitDtWeekday(B: TWasmBuf);
begin
  EmitDtDays(B);
  B.I64Const(6); B.Op(wopI64Add);
  B.I64Const(7); B.Op(wopI64RemS);
  B.I64Const(7); B.Op(wopI64Add);
  B.I64Const(7); B.Op(wopI64RemS);
  B.I64Const(1); B.Op(wopI64Add);
end;

{ DATEPART. The interval code is on the stack; DT_DAYS and DT_MS are already set.
  ⛔ The chain is built by a LOOP rather than written out: ten arms spelled by
  hand is ten chances to put an arm under the wrong code, and the codes are the
  same list dtIval answers with. }
procedure TWasmBackend.EmitDatePartSwitch(B: TWasmBuf);
var
  k: Integer;

  procedure Field(Cell: LongWord);
  begin
    EmitDtDays(B);
    B.Call(FDtCivilFunc);
    B.I32Const(LongInt(Cell)); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S);
  end;

  procedure MsPart(Div1, Mod1: Int64);
  begin
    B.I32Const(DT_MS); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S);
    B.I64Const(Div1); B.Op(wopI64DivS);
    if Mod1 > 0 then begin B.I64Const(Mod1); B.Op(wopI64RemS); end;
  end;

begin
  B.LocalSet(FArrTmp);
  for k := 0 to 9 do
  begin
    B.LocalGet(FArrTmp); B.I32Const(k); B.Op(wopI32Eq);
    B.BlockStart(wopIf, WASM_TYPE_I64);
      case k of
        0: Field(DT_Y);
        1: begin                                  // the quarter
             Field(DT_MO);
             B.I64Const(1); B.Op(wopI64Sub);
             B.I64Const(3); B.Op(wopI64DivS);
             B.I64Const(1); B.Op(wopI64Add);
           end;
        2: Field(DT_MO);
        3: begin EmitDtDays(B); B.Call(FDtDoyFunc); end;
        4: Field(DT_D);
        5: EmitDtWeekday(B);
        6: begin EmitDtDays(B); B.Call(FDtWoyFunc); end;
        7: MsPart(3600000, 0);
        8: MsPart(60000, 60);
      else MsPart(1000, 60);
      end;
    B.Op(wopElse);
  end;
  B.I64Const(0);
  for k := 0 to 9 do B.EndOp;
end;

{ DATEADD. The interval code is on the stack; Src2 is the count and Src3 the
  serial. ⚠️ Codes 3, 4 and 5 - day of year, day, weekday - all add whole DAYS,
  which is why they fall through to the default rather than having arms. }
procedure TWasmBackend.EmitDateAddSwitch(B: TWasmBuf; Instr: TSSAInstruction);
var
  k: Integer;

  procedure Months(Factor: Int64);
  begin
    LoadReg(B, Instr.Src3);
    LoadReg(B, Instr.Src2);
    if Factor <> 1 then begin B.I64Const(Factor); B.Op(wopI64Mul); end;
    B.Call(FDtIncMoFunc);
  end;

  procedure Fraction(Per: Double);
  begin
    LoadReg(B, Instr.Src3);
    LoadReg(B, Instr.Src2); B.Op(wopF64ConvertI64S);
    B.F64Const(Per); B.Op(wopF64Div);
    B.Op(wopF64Add);
  end;

begin
  B.LocalSet(FArrTmp);
  for k := 0 to 9 do
  begin
    if k in [3, 4, 5] then Continue;      // whole days: the default answers
    B.LocalGet(FArrTmp); B.I32Const(k); B.Op(wopI32Eq);
    B.BlockStart(wopIf, WASM_TYPE_F64);
      case k of
        0: Months(12);
        1: Months(3);
        2: Months(1);
        6: begin                          // whole weeks
             LoadReg(B, Instr.Src3);
             LoadReg(B, Instr.Src2); B.I64Const(7); B.Op(wopI64Mul);
             B.Op(wopF64ConvertI64S); B.Op(wopF64Add);
           end;
        7: Fraction(24.0);
        8: Fraction(1440.0);
      else Fraction(86400.0);
      end;
    B.Op(wopElse);
  end;
  LoadReg(B, Instr.Src3);
  LoadReg(B, Instr.Src2); B.Op(wopF64ConvertI64S); B.Op(wopF64Add);
  for k := 0 to 6 do B.EndOp;             // seven arms were emitted
end;

{ DATEDIFF. Src2 is the earlier serial and Src3 the later one.
  ⚠️ The hour/minute/second differences ROUND, and FPC's Round is
  half-to-EVEN - which is exactly what f64.nearest does, so the two agree
  without a correction. Round-half-away would differ on every exact half. }
procedure TWasmBackend.EmitDateDiffSwitch(B: TWasmBuf; Instr: TSSAInstruction);
var
  k: Integer;

  procedure WholeDays(Which: TSSAValue);
  begin
    LoadReg(B, Which); B.Op(wopF64Trunc); B.TruncSat(wopfcI64TruncSatF64S);
  end;

  procedure Scaled(Per: Double);
  begin
    LoadReg(B, Instr.Src3);
    LoadReg(B, Instr.Src2); B.Op(wopF64Sub);
    B.F64Const(Per); B.Op(wopF64Mul);
    B.Op(wopF64Nearest); B.TruncSat(wopfcI64TruncSatF64S);
  end;

  { The quarter index of a packed year*12 + month-1. }
  procedure QuarterOf(Which: TSSAValue);
  begin
    LoadReg(B, Which); B.Call(FDtYMFunc); B.LocalTee(FRawTmp);
    B.I64Const(12); B.Op(wopI64DivS); B.I64Const(4); B.Op(wopI64Mul);
    B.LocalGet(FRawTmp); B.I64Const(12); B.Op(wopI64RemS);
    B.I64Const(3); B.Op(wopI64DivS);
    B.Op(wopI64Add);
  end;

begin
  B.LocalSet(FArrTmp);
  for k := 0 to 9 do
  begin
    if k in [3, 4, 5] then Continue;      // whole days: the default answers
    B.LocalGet(FArrTmp); B.I32Const(k); B.Op(wopI32Eq);
    B.BlockStart(wopIf, WASM_TYPE_I64);
      case k of
        0: begin                          // whole years
             LoadReg(B, Instr.Src3); B.Call(FDtYMFunc);
             B.I64Const(12); B.Op(wopI64DivS);
             LoadReg(B, Instr.Src2); B.Call(FDtYMFunc);
             B.I64Const(12); B.Op(wopI64DivS);
             B.Op(wopI64Sub);
           end;
        1: begin QuarterOf(Instr.Src3); QuarterOf(Instr.Src2); B.Op(wopI64Sub); end;
        2: begin                          // whole months: the packed difference IS it
             LoadReg(B, Instr.Src3); B.Call(FDtYMFunc);
             LoadReg(B, Instr.Src2); B.Call(FDtYMFunc);
             B.Op(wopI64Sub);
           end;
        6: begin                          // whole weeks
             WholeDays(Instr.Src3); WholeDays(Instr.Src2); B.Op(wopI64Sub);
             B.I64Const(7); B.Op(wopI64DivS);
           end;
        7: Scaled(24.0);
        8: Scaled(1440.0);
      else Scaled(86400.0);
      end;
    B.Op(wopElse);
  end;
  WholeDays(Instr.Src3); WholeDays(Instr.Src2); B.Op(wopI64Sub);
  for k := 0 to 6 do B.EndOp;
end;

procedure TWasmBackend.EmitValHelpers;
{ VAL and the VALINT family - the DECIMAL-TO-BINARY direction.

  ⛔ These are emitted from inside EmitStringHelpers, last, and numbered last in
  the string block. Nothing checks that the two orders agree.

  The three functions mirror the interpreter one for one:
    valInt   <- ParseLeadingInt64      (SedaiBytecodeVM)
    valBit   \
    valFlt   /- ParseLeadingFloat + ExactStrToDouble (SedaiConsoleBehavior)
  and the second pair is the reason the interpreter was corrected first. VAL used
  to end in FPC's Val(), which cannot exist here, and which was wrong twice
  anyway - it gave up on any numeric string past 255 characters and it rounded
  through the 80-bit Extended before rounding again into the Double. Rewriting it
  as exact integer work on a digit buffer made THIS port mechanical, and makes
  the two sides agree by construction rather than by testing. Same shape as the
  float PRINT direction, and for the same reason.

  ⭐ The digit buffer is FLT_DEC and the only thing that writes to it is fltMul,
  the float printer's own helper - so the two directions share one scratch area
  and one multiply. They never overlap: VAL has produced its double before
  anything can print it. }
var
  B: TWasmBuf;
  TValInt, TValBit, TValFlt: LongWord;

  { The ±0 that four different dead ends have to answer, sign included. }
  procedure ReturnZero(ANeg: LongWord);
  begin
    B.LocalGet(ANeg);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(Int64($8000000000000000)); B.Op(wopF64ReinterpretI64); B.Op(wopReturn);
    B.EndOp;
    B.F64Const(0); B.Op(wopReturn);
  end;

begin
  if not (FUsesVal or FUsesValInt) then Exit;

  TValInt := FModule.TypeIndex([wvtI32], [wvtI64]);
  TValBit := FModule.TypeIndex([wvtI32], [wvtI32]);
  TValFlt := FModule.TypeIndex([wvtI32], [wvtF64]);

  { valInt(s) -> i64: the leading integer, FreeBASIC VALINT/VALLNG/VALUINT.
    ⛔ A "&H"/"&O"/"&B" prefix is honoured only when NOTHING precedes it - fbc
    does not accept a sign before one, so VALINT("-&HFF") is 0 and not -255.
    That is measured behaviour, not a reading of the manual. }
  B := TWasmBuf.Create;
  try
    // 1=len 2=p 3=i 4=neg 5=signed 6=c 7=base 8=d (i32), 9=res (i64)
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(1);
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(2);
    B.I32Const(0); B.LocalSet(3);

    // skip leading spaces
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(3); B.LocalGet(1); B.Op(wopI32GeS); B.BrIf(1);
        B.LocalGet(2); B.LocalGet(3); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0);
          B.I32Const(32); B.Op(wopI32Ne); B.BrIf(1);
        B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(3);
        B.Br(0);
      B.EndOp;
    B.EndOp;

    B.I32Const(0); B.LocalSet(4);
    B.I32Const(0); B.LocalSet(5);
    B.LocalGet(3); B.LocalGet(1); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(2); B.LocalGet(3); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0); B.LocalSet(6);
      B.LocalGet(6); B.I32Const(43); B.Op(wopI32Eq);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(1); B.LocalSet(5);
        B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(3);
      B.Op(wopElse);
        B.LocalGet(6); B.I32Const(45); B.Op(wopI32Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(1); B.LocalSet(4);
          B.I32Const(1); B.LocalSet(5);
          B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(3);
        B.EndOp;
      B.EndOp;
    B.EndOp;

    // the base prefix, and only with nothing in front of it
    B.I32Const(0); B.LocalSet(7);
    B.LocalGet(5); B.Op(wopI32Eqz);
    B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add); B.LocalGet(1); B.Op(wopI32LtS);
    B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(2); B.LocalGet(3); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0);
        B.I32Const(38); B.Op(wopI32Eq);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(2); B.LocalGet(3); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 1); B.LocalSet(6);
        B.LocalGet(6); B.I32Const(97); B.Op(wopI32GeS);
        B.LocalGet(6); B.I32Const(122); B.Op(wopI32LeS); B.Op(wopI32And);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(6); B.I32Const(32); B.Op(wopI32Sub); B.LocalSet(6);
        B.EndOp;
        B.LocalGet(6); B.I32Const(72); B.Op(wopI32Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(16); B.LocalSet(7);
        B.EndOp;
        B.LocalGet(6); B.I32Const(79); B.Op(wopI32Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(8); B.LocalSet(7);
        B.EndOp;
        B.LocalGet(6); B.I32Const(66); B.Op(wopI32Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(2); B.LocalSet(7);
        B.EndOp;
      B.EndOp;
    B.EndOp;

    B.I64Const(0); B.LocalSet(9);
    B.LocalGet(7); B.I32Const(0); B.Op(wopI32GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(3); B.I32Const(2); B.Op(wopI32Add); B.LocalSet(3);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(3); B.LocalGet(1); B.Op(wopI32GeS); B.BrIf(1);
          B.LocalGet(2); B.LocalGet(3); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0); B.LocalSet(6);
          B.LocalGet(6); B.I32Const(97); B.Op(wopI32GeS);
          B.LocalGet(6); B.I32Const(122); B.Op(wopI32LeS); B.Op(wopI32And);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(6); B.I32Const(32); B.Op(wopI32Sub); B.LocalSet(6);
          B.EndOp;
          B.I32Const(-1); B.LocalSet(8);
          B.LocalGet(6); B.I32Const(48); B.Op(wopI32GeS);
          B.LocalGet(6); B.I32Const(57); B.Op(wopI32LeS); B.Op(wopI32And);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(6); B.I32Const(48); B.Op(wopI32Sub); B.LocalSet(8);
          B.Op(wopElse);
            B.LocalGet(6); B.I32Const(65); B.Op(wopI32GeS);
            B.LocalGet(6); B.I32Const(70); B.Op(wopI32LeS); B.Op(wopI32And);
            B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
              B.LocalGet(6); B.I32Const(55); B.Op(wopI32Sub); B.LocalSet(8);
            B.EndOp;
          B.EndOp;
          B.LocalGet(8); B.I32Const(0); B.Op(wopI32LtS); B.BrIf(1);
          B.LocalGet(8); B.LocalGet(7); B.Op(wopI32GeS); B.BrIf(1);
          B.LocalGet(9); B.LocalGet(7); B.Op(wopI64ExtendI32U); B.Op(wopI64Mul);
            B.LocalGet(8); B.Op(wopI64ExtendI32U); B.Op(wopI64Add); B.LocalSet(9);
          B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(3);
          B.Br(0);
        B.EndOp;
      B.EndOp;
    B.Op(wopElse);
      { The DECIMAL magnitude SATURATES - it does not wrap - at 2^64-1, and at
        2^32-1 when the caller asked for a 32-bit spelling. ⛔ The base-prefix
        branch above deliberately keeps wrapping: fbc scans &H/&O/&B itself and
        reads a decimal through the C library, and the two disagree. We
        reproduce the coherent half. Same rule as ParseLeadingInt64. }
      B.I32Const(0); B.LocalSet(10);              // overflow flag
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(3); B.LocalGet(1); B.Op(wopI32GeS); B.BrIf(1);
          B.LocalGet(2); B.LocalGet(3); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0); B.LocalSet(6);
          B.LocalGet(6); B.I32Const(48); B.Op(wopI32LtS); B.BrIf(1);
          B.LocalGet(6); B.I32Const(57); B.Op(wopI32GtS); B.BrIf(1);
          // would res*10 + d pass 2^64-1?  res >u (2^64-1 - d) / 10
          B.LocalGet(9);
          B.I64Const(-1);
            B.LocalGet(6); B.I32Const(48); B.Op(wopI32Sub); B.Op(wopI64ExtendI32U);
            B.Op(wopI64Sub);
          B.I64Const(10); B.Op(wopI64DivU);
          B.Op(wopI64GtU);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.I32Const(1); B.LocalSet(10);
          B.Op(wopElse);
            B.LocalGet(9); B.I64Const(10); B.Op(wopI64Mul);
              B.LocalGet(6); B.I32Const(48); B.Op(wopI32Sub); B.Op(wopI64ExtendI32U);
              B.Op(wopI64Add); B.LocalSet(9);
          B.EndOp;
          B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(3);
          B.Br(0);
        B.EndOp;
      B.EndOp;
      B.LocalGet(10);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I64Const(-1); B.LocalSet(9);
      B.EndOp;
      // the 32-bit spellings saturate at THEIR maximum
      B.I32Const(VAL_DECW); B.OpMem(wopI32Load, 2, 0); B.I32Const(32); B.Op(wopI32Eq);
      B.LocalGet(9); B.I64Const($FFFFFFFF); B.Op(wopI64GtU);
      B.Op(wopI32And);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I64Const($FFFFFFFF); B.LocalSet(9);
      B.EndOp;
    B.EndOp;

    B.LocalGet(4);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(0); B.LocalGet(9); B.Op(wopI64Sub); B.LocalSet(9);
    B.EndOp;
    B.LocalGet(9);
    // 1..8 i32, 9 the accumulator (i64), 10 the decimal overflow flag (i32)
    FModule.AddFunction(TValInt, [wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32,
                                  wvtI32, wvtI32, wvtI64, wvtI32], B);
  finally
    B.Free;
  end;

  if not FUsesVal then Exit;

  { valBit(pos) -> the bit at that position of the integer part, 0 past either
    end. The limbs are 30 bits because that is what one pass of the division
    produces, not because of any register width. }
  B := TWasmBuf.Create;
  try
    B.LocalGet(0); B.I32Const(0); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(0); B.I32Const(30); B.Op(wopI32DivS); B.LocalTee(1);
    B.I32Const(VAL_NLIMB); B.OpMem(wopI32Load, 2, 0); B.Op(wopI32GeS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.Op(wopReturn);
    B.EndOp;
    B.I32Const(VAL_LIMB); B.LocalGet(1); B.I32Const(4); B.Op(wopI32Mul); B.Op(wopI32Add);
      B.OpMem(wopI32Load, 2, 0);
    B.LocalGet(0); B.I32Const(30); B.Op(wopI32RemS); B.Op(wopI32ShrU);
    B.I32Const(1); B.Op(wopI32And);
    FModule.AddFunction(TValBit, [wvtI32], B);
  finally
    B.Free;
  end;

  { valFlt(s) -> f64: the whole of VAL. The scan is ParseLeadingFloat's, the
    conversion is ExactStrToDouble's, and both are transcribed rather than
    reinvented - see those two for why each rule is the way it is. }
  B := TWasmBuf.Create;
  try
    // i32 1=len 2=p 3=i 4=neg 5=c 6=sig 7=exp10 8=sawDot 9=sawDigit 10=sticky
    //     11=d 12=q 13=esign 14=ev 15=j 16=sh 17=nlimb 18=L 19=E 20=prec
    //     21=drop 22=b 23=lo 24=hi 25=off 26=t 27=rest
    // i64 28=T 29=rem 30=mant 31=bits 32=mul
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0); B.LocalSet(1);
    B.LocalGet(0); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(2);
    B.I32Const(0); B.LocalSet(3);

    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(3); B.LocalGet(1); B.Op(wopI32GeS); B.BrIf(1);
        B.LocalGet(2); B.LocalGet(3); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0);
          B.I32Const(32); B.Op(wopI32Ne); B.BrIf(1);
        B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(3);
        B.Br(0);
      B.EndOp;
    B.EndOp;

    { A base prefix is an INTEGER, and it is looked for before the sign scan on
      purpose: fbc does not accept a sign in front of one. valInt skips the same
      leading spaces, so handing it the whole string reaches the same place. }
    B.LocalGet(3); B.LocalGet(1); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(2); B.LocalGet(3); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0);
        B.I32Const(38); B.Op(wopI32Eq);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(VAL_DECW); B.I32Const(0); B.OpMem(wopI32Store, 2, 0);
        B.LocalGet(0); B.Call(FStrValIntFunc); B.Op(wopF64ConvertI64S); B.Op(wopReturn);
      B.EndOp;
    B.EndOp;

    B.I32Const(0); B.LocalSet(4);
    B.LocalGet(3); B.LocalGet(1); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(2); B.LocalGet(3); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0); B.LocalSet(5);
      B.LocalGet(5); B.I32Const(43); B.Op(wopI32Eq);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(3);
      B.Op(wopElse);
        B.LocalGet(5); B.I32Const(45); B.Op(wopI32Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(1); B.LocalSet(4);
          B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(3);
        B.EndOp;
      B.EndOp;
    B.EndOp;

    { The digits, into VAL_SIG most significant first. A digit before the point
      is part of the integer; one after it moves the exponent down; a leading
      zero after the point moves it down without being stored. }
    B.I32Const(0); B.LocalSet(6);
    B.I32Const(0); B.LocalSet(7);
    B.I32Const(0); B.LocalSet(8);
    B.I32Const(0); B.LocalSet(9);
    B.I32Const(0); B.LocalSet(10);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(3); B.LocalGet(1); B.Op(wopI32GeS); B.BrIf(1);
        B.LocalGet(2); B.LocalGet(3); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0); B.LocalSet(5);
        B.LocalGet(5); B.I32Const(48); B.Op(wopI32GeS);
        B.LocalGet(5); B.I32Const(57); B.Op(wopI32LeS); B.Op(wopI32And); B.LocalSet(26);
        B.LocalGet(26); B.Op(wopI32Eqz);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          // not a digit: only an unseen '.' keeps the scan going
          B.LocalGet(5); B.I32Const(46); B.Op(wopI32Eq);
          B.LocalGet(8); B.Op(wopI32Eqz); B.Op(wopI32And);
          B.Op(wopI32Eqz); B.BrIf(2);
          B.I32Const(1); B.LocalSet(8);
        B.Op(wopElse);
          B.I32Const(1); B.LocalSet(9);
          B.LocalGet(5); B.I32Const(48); B.Op(wopI32Sub); B.LocalSet(11);
          B.LocalGet(6); B.Op(wopI32Eqz);
          B.LocalGet(11); B.Op(wopI32Eqz); B.Op(wopI32And);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(8);
            B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
              B.LocalGet(7); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(7);
            B.EndOp;
          B.Op(wopElse);
            B.LocalGet(6); B.I32Const(VAL_DIGCAP); B.Op(wopI32LtS);
            B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
              B.I32Const(VAL_SIG); B.LocalGet(6); B.Op(wopI32Add);
                B.LocalGet(11); B.OpMem(wopI32Store8, 0, 0);
              B.LocalGet(6); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(6);
              B.LocalGet(8);
              B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
                B.LocalGet(7); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(7);
              B.EndOp;
            B.Op(wopElse);
              B.LocalGet(11);
              B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
                B.I32Const(1); B.LocalSet(10);
              B.EndOp;
              B.LocalGet(8); B.Op(wopI32Eqz);
              B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
                B.LocalGet(7); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(7);
              B.EndOp;
            B.EndOp;
          B.EndOp;
        B.EndOp;
        B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(3);
        B.Br(0);
      B.EndOp;
    B.EndOp;

    { No digit at all. ⚠️ NOT plain zero: fbc applies a minus it has already
      consumed, so "-x" reads as NEGATIVE zero while a lone "-" does not. }
    B.LocalGet(9); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(4);
      B.LocalGet(3); B.LocalGet(1); B.Op(wopI32LtS); B.Op(wopI32And);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I64Const(Int64($8000000000000000)); B.Op(wopF64ReinterpretI64); B.Op(wopReturn);
      B.EndOp;
      B.F64Const(0); B.Op(wopReturn);
    B.EndOp;

    { The exponent, 'E' or FreeBASIC's 'D', and consumed only when a digit
      follows it - "1e" is one, not an error. }
    B.LocalGet(3); B.LocalGet(1); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(2); B.LocalGet(3); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0); B.LocalSet(5);
      B.LocalGet(5); B.I32Const(97); B.Op(wopI32GeS);
      B.LocalGet(5); B.I32Const(122); B.Op(wopI32LeS); B.Op(wopI32And);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(5); B.I32Const(32); B.Op(wopI32Sub); B.LocalSet(5);
      B.EndOp;
      B.LocalGet(5); B.I32Const(69); B.Op(wopI32Eq);
      B.LocalGet(5); B.I32Const(68); B.Op(wopI32Eq); B.Op(wopI32Or);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(3); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(12);
        B.I32Const(1); B.LocalSet(13);
        B.LocalGet(12); B.LocalGet(1); B.Op(wopI32LtS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(2); B.LocalGet(12); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0); B.LocalSet(26);
          B.LocalGet(26); B.I32Const(43); B.Op(wopI32Eq);
          B.LocalGet(26); B.I32Const(45); B.Op(wopI32Eq); B.Op(wopI32Or);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(26); B.I32Const(45); B.Op(wopI32Eq);
            B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
              B.I32Const(-1); B.LocalSet(13);
            B.EndOp;
            B.LocalGet(12); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(12);
          B.EndOp;
        B.EndOp;
        B.I32Const(0); B.LocalSet(26);
        B.LocalGet(12); B.LocalGet(1); B.Op(wopI32LtS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(2); B.LocalGet(12); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0); B.LocalSet(5);
          B.LocalGet(5); B.I32Const(48); B.Op(wopI32GeS);
          B.LocalGet(5); B.I32Const(57); B.Op(wopI32LeS); B.Op(wopI32And); B.LocalSet(26);
        B.EndOp;
        B.LocalGet(26);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(0); B.LocalSet(14);
          B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
            B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
              B.LocalGet(12); B.LocalGet(1); B.Op(wopI32GeS); B.BrIf(1);
              B.LocalGet(2); B.LocalGet(12); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0); B.LocalSet(5);
              B.LocalGet(5); B.I32Const(48); B.Op(wopI32LtS); B.BrIf(1);
              B.LocalGet(5); B.I32Const(57); B.Op(wopI32GtS); B.BrIf(1);
              // clamped, not wrapped: a wild exponent still has to answer
              // infinity or zero, and must not overflow on the way there
              B.LocalGet(14); B.I32Const(1000000); B.Op(wopI32LtS);
              B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
                B.LocalGet(14); B.I32Const(10); B.Op(wopI32Mul);
                  B.LocalGet(5); B.Op(wopI32Add); B.I32Const(48); B.Op(wopI32Sub); B.LocalSet(14);
              B.EndOp;
              B.LocalGet(12); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(12);
              B.Br(0);
            B.EndOp;
          B.EndOp;
          B.LocalGet(7); B.LocalGet(13); B.LocalGet(14); B.Op(wopI32Mul);
            B.Op(wopI32Add); B.LocalSet(7);
          B.LocalGet(12); B.LocalSet(3);
        B.EndOp;
      B.EndOp;
    B.EndOp;

    B.LocalGet(6); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      ReturnZero(4);
    B.EndOp;

    { Two guards that BOUND THE BUFFER and decide nothing: the ordinary path
      below still has to answer infinity for 1e309 and zero for 1e-324. }
    B.LocalGet(6); B.I32Const(1); B.Op(wopI32Sub); B.LocalGet(7); B.Op(wopI32Add); B.LocalSet(26);
    B.LocalGet(26); B.I32Const(330); B.Op(wopI32GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(Int64($7FF0000000000000)); B.LocalSet(31);
      B.LocalGet(4);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(31); B.I64Const(Int64($8000000000000000)); B.Op(wopI64Or); B.LocalSet(31);
      B.EndOp;
      B.LocalGet(31); B.Op(wopF64ReinterpretI64); B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(26); B.I32Const(-400); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      ReturnZero(4);
    B.EndOp;

    // the digits into FLT_DEC, least significant first
    B.I32Const(0); B.LocalSet(26);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(26); B.LocalGet(6); B.Op(wopI32GeS); B.BrIf(1);
        B.I32Const(FLT_DEC); B.LocalGet(26); B.Op(wopI32Add);
        B.I32Const(VAL_SIG); B.LocalGet(6); B.Op(wopI32Add); B.I32Const(1); B.Op(wopI32Sub);
          B.LocalGet(26); B.Op(wopI32Sub); B.OpMem(wopI32Load8U, 0, 0);
        B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(26); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(26);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    B.I32Const(FLT_LEN); B.LocalGet(6); B.OpMem(wopI32Store, 2, 0);

    { A positive exponent folds into the integer; a negative one becomes j, the
      number of digits that sit BELOW the point - and dividing a decimal number
      by a power of ten is reading the digits above it, not a division. }
    B.LocalGet(7); B.I32Const(0); B.Op(wopI32GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(7); B.LocalSet(26);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(26); B.I32Const(0); B.Op(wopI32LeS); B.BrIf(1);
          B.LocalGet(26); B.LocalSet(22);
          B.LocalGet(22); B.I32Const(17); B.Op(wopI32GtS);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.I32Const(17); B.LocalSet(22);
          B.EndOp;
          B.I64Const(1); B.LocalSet(32);
          B.LocalGet(22); B.LocalSet(25);
          B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
            B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
              B.LocalGet(25); B.I32Const(0); B.Op(wopI32LeS); B.BrIf(1);
              B.LocalGet(32); B.I64Const(10); B.Op(wopI64Mul); B.LocalSet(32);
              B.LocalGet(25); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(25);
              B.Br(0);
            B.EndOp;
          B.EndOp;
          B.LocalGet(32); B.Call(FFltMulFunc);
          B.LocalGet(26); B.LocalGet(22); B.Op(wopI32Sub); B.LocalSet(26);
          B.Br(0);
        B.EndOp;
      B.EndOp;
      B.I32Const(0); B.LocalSet(15);
    B.Op(wopElse);
      B.I32Const(0); B.LocalGet(7); B.Op(wopI32Sub); B.LocalSet(15);
    B.EndOp;

    { Grow until 21 digits sit above the point: 10^20 is past 2^66, so the
      integer part is guaranteed to carry more bits than the rounding needs. }
    B.I32Const(0); B.LocalSet(16);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(FLT_LEN); B.OpMem(wopI32Load, 2, 0); B.LocalGet(15); B.Op(wopI32Sub);
          B.I32Const(21); B.Op(wopI32GeS); B.BrIf(1);
        B.I32Const(FLT_LEN); B.OpMem(wopI32Load, 2, 0);
          B.I32Const(VAL_MAXDIG - 12); B.Op(wopI32GeS); B.BrIf(1);
        B.I64Const(Int64(1) shl 30); B.Call(FFltMulFunc);
        B.LocalGet(16); B.I32Const(30); B.Op(wopI32Add); B.LocalSet(16);
        B.Br(0);
      B.EndOp;
    B.EndOp;

    // everything below the point is one bit of information
    B.LocalGet(10); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.LocalSet(26);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(26); B.LocalGet(15); B.Op(wopI32GeS); B.BrIf(1);
          B.I32Const(FLT_DEC); B.LocalGet(26); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.I32Const(1); B.LocalSet(10);
            B.Br(2);
          B.EndOp;
          B.LocalGet(26); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(26);
          B.Br(0);
        B.EndOp;
      B.EndOp;
    B.EndOp;

    { The integer part to binary, the only way a decimal number gets there
      without a big divide: divide by 2^30 and keep the remainders, which ARE
      the limbs. Every partial remainder stays below the divisor, so each
      quotient digit stays below ten. }
    B.I32Const(0); B.LocalSet(17);
    B.LocalGet(15); B.LocalSet(23);
    B.I32Const(FLT_LEN); B.OpMem(wopI32Load, 2, 0); B.LocalSet(24);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(24); B.LocalGet(23); B.Op(wopI32LeS); B.BrIf(1);
        B.LocalGet(17); B.I32Const(VAL_MAXLIMB); B.Op(wopI32GeS); B.BrIf(1);
        B.I64Const(0); B.LocalSet(29);
        B.LocalGet(24); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(26);
        B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
          B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(26); B.LocalGet(23); B.Op(wopI32LtS); B.BrIf(1);
            B.LocalGet(29); B.I64Const(10); B.Op(wopI64Mul);
              B.I32Const(FLT_DEC); B.LocalGet(26); B.Op(wopI32Add); B.OpMem(wopI32Load8U, 0, 0);
              B.Op(wopI64ExtendI32U); B.Op(wopI64Add); B.LocalSet(28);
            B.I32Const(FLT_DEC); B.LocalGet(26); B.Op(wopI32Add);
              B.LocalGet(28); B.I64Const(30); B.Op(wopI64ShrU); B.Op(wopI32WrapI64);
              B.OpMem(wopI32Store8, 0, 0);
            B.LocalGet(28); B.I64Const((Int64(1) shl 30) - 1); B.Op(wopI64And); B.LocalSet(29);
            B.LocalGet(26); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(26);
            B.Br(0);
          B.EndOp;
        B.EndOp;
        B.I32Const(VAL_LIMB); B.LocalGet(17); B.I32Const(4); B.Op(wopI32Mul); B.Op(wopI32Add);
          B.LocalGet(29); B.Op(wopI32WrapI64); B.OpMem(wopI32Store, 2, 0);
        B.LocalGet(17); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(17);
        B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
          B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(24); B.LocalGet(23); B.Op(wopI32LeS); B.BrIf(1);
            B.I32Const(FLT_DEC); B.LocalGet(24); B.I32Const(1); B.Op(wopI32Sub); B.Op(wopI32Add);
              B.OpMem(wopI32Load8U, 0, 0); B.BrIf(1);
            B.LocalGet(24); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(24);
            B.Br(0);
          B.EndOp;
        B.EndOp;
        B.Br(0);
      B.EndOp;
    B.EndOp;
    B.I32Const(VAL_NLIMB); B.LocalGet(17); B.OpMem(wopI32Store, 2, 0);

    B.LocalGet(17); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      ReturnZero(4);
    B.EndOp;

    // the bit length of the integer part
    B.LocalGet(17); B.I32Const(1); B.Op(wopI32Sub); B.I32Const(30); B.Op(wopI32Mul); B.LocalSet(18);
    B.I32Const(29); B.LocalSet(22);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(22); B.I32Const(0); B.Op(wopI32LtS); B.BrIf(1);
        B.I32Const(VAL_LIMB); B.LocalGet(17); B.I32Const(1); B.Op(wopI32Sub);
          B.I32Const(4); B.Op(wopI32Mul); B.Op(wopI32Add); B.OpMem(wopI32Load, 2, 0);
          B.LocalGet(22); B.Op(wopI32ShrU); B.I32Const(1); B.Op(wopI32And); B.BrIf(1);
        B.LocalGet(22); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(22);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    B.LocalGet(18); B.LocalGet(22); B.Op(wopI32Add); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(18);
    B.LocalGet(18); B.I32Const(1); B.Op(wopI32Sub); B.LocalGet(16); B.Op(wopI32Sub); B.LocalSet(19);

    B.I32Const(53); B.LocalSet(20);
    B.LocalGet(19); B.I32Const(-1022); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(53); B.LocalGet(19); B.Op(wopI32Add); B.I32Const(1022); B.Op(wopI32Add);
        B.LocalSet(20);
    B.EndOp;
    B.LocalGet(20); B.I32Const(0); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      ReturnZero(4);
    B.EndOp;
    B.LocalGet(18); B.LocalGet(20); B.Op(wopI32Sub); B.LocalSet(21);

    B.I64Const(0); B.LocalSet(30);
    B.LocalGet(20); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(22);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(22); B.I32Const(0); B.Op(wopI32LtS); B.BrIf(1);
        B.LocalGet(30); B.I64Const(1); B.Op(wopI64Shl);
          B.LocalGet(21); B.LocalGet(22); B.Op(wopI32Add); B.Call(FValBitFunc);
          B.Op(wopI64ExtendI32U); B.Op(wopI64Or); B.LocalSet(30);
        B.LocalGet(22); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(22);
        B.Br(0);
      B.EndOp;
    B.EndOp;

    { Round half to EVEN, and the tie test looks at EVERY bit below - which is
      the whole reason this exists instead of a chain of multiplies. }
    B.LocalGet(10); B.LocalSet(27);
    B.LocalGet(27); B.Op(wopI32Eqz);
    B.LocalGet(21); B.I32Const(2); B.Op(wopI32GeS); B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(21); B.I32Const(1); B.Op(wopI32Sub); B.I32Const(30); B.Op(wopI32DivS);
        B.LocalSet(12);
      B.I32Const(0); B.LocalSet(26);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(26); B.LocalGet(12); B.Op(wopI32GeS); B.BrIf(1);
          B.I32Const(VAL_LIMB); B.LocalGet(26); B.I32Const(4); B.Op(wopI32Mul); B.Op(wopI32Add);
            B.OpMem(wopI32Load, 2, 0);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.I32Const(1); B.LocalSet(27);
            B.Br(2);
          B.EndOp;
          B.LocalGet(26); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(26);
          B.Br(0);
        B.EndOp;
      B.EndOp;
      B.LocalGet(27); B.Op(wopI32Eqz);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(21); B.I32Const(1); B.Op(wopI32Sub); B.I32Const(30); B.Op(wopI32RemS);
          B.LocalSet(25);
        B.LocalGet(25); B.I32Const(0); B.Op(wopI32GtS);
        B.LocalGet(12); B.LocalGet(17); B.Op(wopI32LtS); B.Op(wopI32And);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(VAL_LIMB); B.LocalGet(12); B.I32Const(4); B.Op(wopI32Mul); B.Op(wopI32Add);
            B.OpMem(wopI32Load, 2, 0);
          B.I32Const(1); B.LocalGet(25); B.Op(wopI32Shl); B.I32Const(1); B.Op(wopI32Sub);
          B.Op(wopI32And);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.I32Const(1); B.LocalSet(27);
          B.EndOp;
        B.EndOp;
      B.EndOp;
    B.EndOp;

    B.LocalGet(21); B.I32Const(0); B.Op(wopI32GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(21); B.I32Const(1); B.Op(wopI32Sub); B.Call(FValBitFunc);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(27);
        B.LocalGet(30); B.I64Const(1); B.Op(wopI64And); B.I64Const(0); B.Op(wopI64Ne);
        B.Op(wopI32Or);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(30); B.I64Const(1); B.Op(wopI64Add); B.LocalSet(30);
        B.EndOp;
      B.EndOp;
    B.EndOp;

    B.LocalGet(19); B.I32Const(-1022); B.Op(wopI32GeS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      // the carry can push the mantissa into the next binade
      B.LocalGet(30); B.I64Const(Int64(1) shl 53); B.Op(wopI64Eq);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I64Const(Int64(1) shl 52); B.LocalSet(30);
        B.LocalGet(19); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(19);
      B.EndOp;
      B.LocalGet(19); B.I32Const(1023); B.Op(wopI32GtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I64Const(Int64($7FF0000000000000)); B.LocalSet(31);
      B.Op(wopElse);
        B.LocalGet(19); B.I32Const(1023); B.Op(wopI32Add); B.Op(wopI64ExtendI32S);
          B.I64Const(52); B.Op(wopI64Shl);
        B.LocalGet(30); B.I64Const(Int64($000FFFFFFFFFFFFF)); B.Op(wopI64And);
        B.Op(wopI64Or); B.LocalSet(31);
      B.EndOp;
    B.Op(wopElse);
      { Subnormal: the value is Mant x 2^-1074 by construction, which IS the bit
        pattern - and when the rounding carried it up to 2^52 the same formula
        gives the smallest NORMAL number, which is exactly right. }
      B.LocalGet(30); B.LocalSet(31);
    B.EndOp;

    B.LocalGet(4);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(31); B.I64Const(Int64($8000000000000000)); B.Op(wopI64Or); B.LocalSet(31);
    B.EndOp;
    B.LocalGet(31); B.Op(wopF64ReinterpretI64);
    FModule.AddFunction(TValFlt, [wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32,
                                  wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32,
                                  wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32,
                                  wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32,
                                  wvtI32, wvtI32, wvtI32,
                                  wvtI64, wvtI64, wvtI64, wvtI64, wvtI64], B);
  finally
    B.Free;
  end;
end;

{ ---------------- arrays ----------------

  Elements live in one block from the same bump allocator, at a STRIDE OF 8 for
  every bank - i64, f64 and a string handle all get eight bytes. The waste on the
  string bank is four bytes an element and it buys one index computation instead
  of three.

  ⭐ Nothing zeroes a fresh array, and that is not an omission: the bump
  allocator never reuses, linear memory starts zeroed, and memory.grow hands out
  zeroed pages - so a block that has just been allocated IS zero, which is what
  bcArrayDim writes into it explicitly. ⛔ The day the allocator learns to free,
  this stops being true and DIM has to zero.

  ⚠️ THE BOUNDS RULE IS DIALECT-DEPENDENT and it is not a detail:
    FreeBASIC - no check. A read out of bounds yields the DEFAULT and a write is
                DROPPED. (Real fbc would touch adjacent heap; the interpreter
                chose memory safety, and this follows the interpreter.)
    Commodore  - ?BAD SUBSCRIPT, an error that stops the program.
  Here the CLASSIC arm traps, which is the loud equivalent of the interpreter
  raising - the same choice ssaModFloat made for division by zero. }

procedure TWasmBackend.EmitArrayHelpers;
var
  B: TWasmBuf;
  RT: TSSARegisterType;
  TLoad, TStore, TBound: LongWord;

  procedure BoundsTest;
  // leaves i32 "0 <= idx < total" on the stack; locals 0 = desc, 1 = idx
  begin
    B.LocalGet(1); B.I64Const(0); B.Op(wopI64GeS);
    B.LocalGet(1);
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 4); B.Op(wopI64ExtendI32S);
    B.Op(wopI64LtS);
    B.Op(wopI32And);
  end;

  procedure ElemAddr;
  // leaves base + idx*8; locals 0 = desc, 1 = idx
  begin
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 0);
    B.LocalGet(1); B.Op(wopI32WrapI64); B.I32Const(8); B.Op(wopI32Mul);
    B.Op(wopI32Add);
  end;

begin
  for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
  begin
    TLoad := FModule.TypeIndex([wvtI32, wvtI64], [BankType[RT]]);
    B := TWasmBuf.Create;
    try
      BoundsTest;
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        ElemAddr;
        case RT of
          srtFloat:  B.OpMem(wopF64Load, 3, 0);
          srtString: B.OpMem(wopI32Load, 2, 0);
        else
          B.OpMem(wopI64Load, 3, 0);
        end;
        B.Op(wopReturn);
      B.EndOp;
      if FModern then
        case RT of                       // FreeBASIC: the element type's default
          srtFloat:  B.F64Const(0);
          srtString: B.I32Const(EMPTY_STR);
        else
          B.I64Const(0);
        end
      else
        B.Op(wopUnreachable);            // Commodore: ?BAD SUBSCRIPT
      FArrLoad[RT] := FModule.AddFunction(TLoad, [], B);
    finally
      B.Free;
    end;

    TStore := FModule.TypeIndex([wvtI32, wvtI64, BankType[RT]], []);
    B := TWasmBuf.Create;
    try
      BoundsTest;
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        ElemAddr;
        B.LocalGet(2);
        case RT of
          srtFloat:  B.OpMem(wopF64Store, 3, 0);
          srtString: B.OpMem(wopI32Store, 2, 0);
        else
          B.OpMem(wopI64Store, 3, 0);
        end;
      B.Op(wopElse);
        if not FModern then B.Op(wopUnreachable);   // FreeBASIC drops it silently
      B.EndOp;
      FArrStore[RT] := FModule.AddFunction(TStore, [], B);
    finally
      B.Free;
    end;
  end;

  TBound := FModule.TypeIndex([wvtI32, wvtI64], [wvtI64]);

  { LBOUND(arr, d). ⭐ d BELOW ZERO is not an error: it is FreeBASIC's "how many
    dimensions" query, written LBOUND(arr, 0), and the answer is always 1. }
  B := TWasmBuf.Create;
  try
    B.LocalGet(1); B.I64Const(0); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I64Const(1); B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(0);
    B.LocalGet(1); B.Op(wopI32WrapI64); B.I32Const(8); B.Op(wopI32Mul);
    B.Op(wopI32Add);
    B.OpMem(wopI32Load, 2, 16);
    B.Op(wopI64ExtendI32S);
    FArrLBoundFunc := FModule.AddFunction(TBound, [], B);
  finally
    B.Free;
  end;

  { UBOUND(arr, d) = lb + size - 1, and the same query at d < 0 answers with the
    number of ALLOCATED dimensions - 0 for a dynamic array not dimensioned yet,
    which is also why UBOUND of one answers -1: lb 0 plus size 0 minus one. }
  B := TWasmBuf.Create;
  try
    B.LocalGet(1); B.I64Const(0); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(0); B.OpMem(wopI32Load, 2, 8); B.Op(wopI64ExtendI32S);
      B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(0);
    B.LocalGet(1); B.Op(wopI32WrapI64); B.I32Const(8); B.Op(wopI32Mul);
    B.Op(wopI32Add);
    B.LocalTee(2);
    B.OpMem(wopI32Load, 2, 16);
    B.LocalGet(2);
    B.OpMem(wopI32Load, 2, 20);
    B.Op(wopI32Add);
    B.I32Const(1); B.Op(wopI32Sub);
    B.Op(wopI64ExtendI32S);
    FArrUBoundFunc := FModule.AddFunction(TBound, [wvtI32], B);
  finally
    B.Free;
  end;
end;

{ ---------------- an ARRAY of UDT ----------------

  "Dim p(1 To 8) As Point" is an array of HANDLES, and every element gets its own
  record eagerly at DIM time - which is what makes "p(i).x = 3" work without the
  array ever having to know it holds records. The interpreter does exactly this
  (RecordNewArrayInit), and the reason it is a loop rather than one big block is
  that the elements are independent objects: a later REDIM PRESERVE has to be
  able to keep the ones that already exist.

  ⭐ THAT IS WHY THE TEST IS "handle = 0" AND NOT "the array is fresh". After a
  plain DIM every slot is zero, so all of them are filled; after a REDIM the old
  slots still hold their records and only the grown ones are zero. Filling
  unconditionally would leak the old records AND lose their contents, which is
  the whole difference between REDIM and REDIM PRESERVE.
  ⛔ It relies on a record address never being 0, which holds because the bump
  allocator starts above the literals - the same reason handle 0 can mean "the
  empty string" for the string bank. }

procedure TWasmBackend.EmitRecordHelpers;
var
  B: TWasmBuf;
  T: LongWord;
begin
  // (desc, allocSize, strBase, typeId) -> (); locals 4=i, 5=n, 6=addr, 7=rec
  T := FModule.TypeIndex([wvtI32, wvtI32, wvtI32, wvtI32], []);
  B := TWasmBuf.Create;
  try
    B.LocalGet(0); B.OpMem(wopI32Load, 2, 4); B.LocalSet(5);   // the element count
    B.I32Const(0); B.LocalSet(4);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(4); B.LocalGet(5); B.Op(wopI32GeS); B.BrIf(1);
        { The data base is re-read every iteration on purpose: alloc can grow the
          memory, and while growing never MOVES a block, reading it once would be
          a fact about the allocator rather than about the descriptor. }
        B.LocalGet(0); B.OpMem(wopI32Load, 2, 0);
        B.LocalGet(4); B.I32Const(8); B.Op(wopI32Mul);
        B.Op(wopI32Add);
        B.LocalTee(6);
        B.OpMem(wopI64Load, 3, 0);
        B.Op(wopI64Eqz);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(1); B.Call(FAllocFunc); B.LocalSet(7);
          B.LocalGet(7); B.LocalGet(3); B.OpMem(wopI32Store, 2, 0);   // typeId
          B.LocalGet(7); B.LocalGet(2); B.OpMem(wopI32Store, 2, 4);   // string area
          B.LocalGet(6); B.LocalGet(7); B.Op(wopI64ExtendI32U);
          B.OpMem(wopI64Store, 3, 0);
        B.EndOp;
        B.LocalGet(4); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(4);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    FRecNewArrFunc := FModule.AddFunction(T, [wvtI32, wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;
end;

{ ---------------- "^" on floats ----------------

  ⛔ NOT exp(y * ln x). That is only the LAST arm of what the interpreter runs,
  and taking it for the whole thing would make "x ^ 2" differ from the native
  answer in the last bits for no reason at all. The native side is FPC's
  Math.Power, and this is its STRUCTURE, transcribed:

    exponent = 0                         -> 1
    base = 0 and exponent > 0            -> 0
    |exponent| <= maxint, no fraction    -> intpower, binary exponentiation
    otherwise                            -> exp(exponent * ln base)

  ⭐ The third arm is the one that matters and the one that comes out BIT
  IDENTICAL: it is nothing but f64 multiplications, and IEEE multiplication is
  the same everywhere. It also covers what "^" is actually written for - a
  square, a cube, an inverse square - so the common case does not depend on
  anybody's library.
  ⚠️ intpower inverts the BASE and not the result when the exponent is negative,
  and the order is not cosmetic: 1/b then squaring rounds differently from
  squaring then 1/result. Transcribed in FPC's order.
  ⚠️ The last arm inherits the transcendentals' caveat this backend already
  carries: exp and ln are the HOST's, and the host's are not FPC's - measured a
  ulp apart on some arguments. A non-integer exponent is therefore as faithful
  as SIN and COS are, and no more. }

procedure TWasmBackend.EmitPowHelper;
var
  B: TWasmBuf;
begin
  B := TWasmBuf.Create;
  try
    // params 0 = base, 1 = exponent; locals 2 = i (i32), 3 = res, 4 = b
    B.LocalGet(1); B.F64Const(0); B.Op(wopF64Eq);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.F64Const(1); B.Op(wopReturn);
    B.EndOp;
    B.LocalGet(0); B.F64Const(0); B.Op(wopF64Eq);
    B.LocalGet(1); B.F64Const(0); B.Op(wopF64Gt);
    B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.F64Const(0); B.Op(wopReturn);
    B.EndOp;

    // an INTEGER exponent that fits a LongInt: binary exponentiation
    B.LocalGet(1); B.Op(wopF64Abs); B.F64Const(2147483647); B.Op(wopF64Le);
    B.LocalGet(1); B.LocalGet(1); B.Op(wopF64Trunc); B.Op(wopF64Sub);
      B.F64Const(0); B.Op(wopF64Eq);
    B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(1); B.TruncSat(wopfcI32TruncSatF64S); B.LocalSet(2);
      B.LocalGet(0); B.LocalSet(4);
      B.LocalGet(2); B.I32Const(0); B.Op(wopI32LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.F64Const(1); B.LocalGet(4); B.Op(wopF64Div); B.LocalSet(4);
        B.I32Const(0); B.LocalGet(2); B.Op(wopI32Sub); B.LocalSet(2);   // i := abs(e)
      B.EndOp;
      B.F64Const(1); B.LocalSet(3);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(2); B.I32Const(0); B.Op(wopI32LeS); B.BrIf(1);
          B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
            B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
              B.LocalGet(2); B.I32Const(1); B.Op(wopI32And); B.BrIf(1);
              B.LocalGet(2); B.I32Const(1); B.Op(wopI32ShrS); B.LocalSet(2);
              B.LocalGet(4); B.LocalGet(4); B.Op(wopF64Mul); B.LocalSet(4);
              B.Br(0);
            B.EndOp;
          B.EndOp;
          B.LocalGet(2); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(2);
          B.LocalGet(3); B.LocalGet(4); B.Op(wopF64Mul); B.LocalSet(3);
          B.Br(0);
        B.EndOp;
      B.EndOp;
      B.LocalGet(3); B.Op(wopReturn);
    B.EndOp;

    B.LocalGet(1);
    B.LocalGet(0); B.Call(FTrigFunc[5]);        // ln
    B.Op(wopF64Mul);
    B.Call(FTrigFunc[4]);                       // exp
    FModule.AddFunction(FModule.TypeIndex([wvtF64, wvtF64], [wvtF64]),
                        [wvtI32, wvtF64, wvtF64], B);
  finally
    B.Free;
  end;
end;

{ ---------------- the function table ----------------

  One thunk per address-taken procedure, all of them sharing the type a
  call_indirect names. A thunk takes the UNION of the parameters, hands its
  callee the ones that callee declares - slots are positional per bank, so the
  first n of each bank are exactly its own - and moves the result into the
  global for its bank.

  ⭐ Why a thunk instead of putting the procedures in the table directly: their
  signatures genuinely differ, and a real program proves it rather than a worry
  about one (m217_funcptr composes Integer, Double and String pointers in the
  same file). The uniform half is the parameters, which pad; the result is what
  cannot, so it leaves the type and travels the way the interpreter already
  carries it - through the transfer bank, which is global storage. }

procedure TWasmBackend.EmitThunks;
var
  B: TWasmBuf;
  r, n: Integer;
  RT: TSSARegisterType;
  Base: LongWord;
begin
  for r := 1 to FRegionCount - 1 do
  begin
    if not FAddrTaken[r] then Continue;
    B := TWasmBuf.Create;
    try
      Base := 0;
      for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
      begin
        for n := 0 to FParamCount[r][RT] - 1 do
          B.LocalGet(Base + LongWord(n));
        Inc(Base, LongWord(FIndParam[RT]));
      end;
      B.Call(FFuncIdx[r]);
      // a SUB carries nothing back, and its caller reads nothing
      if FResultBank[r] >= 0 then
        B.GlobalSet(FIndResGlobal[TSSARegisterType(FResultBank[r])]);
      { ⛔ The index is the one RESERVED in Compile, not the one AddFunction
        hands back: the table's elem segment is written before a single function
        body exists, so the two have to be agreed in advance. They are the same
        number as long as the reservation and this loop walk the regions in the
        same order - which is the rule the whole helper numbering follows. }
      FModule.AddFunction(FIndTypeIdx, [], B);
    finally
      B.Free;
    end;
  end;
end;

{ The shape half of a REDIM, against the descriptor in FDescTmp. ⭐ ONE body for
  both spellings: "ReDim a(...)" knows its descriptor at compile time and
  "ReDim obj.f(...)" only at run time, and that is the ONLY difference - writing
  it twice would be two implementations of one rule, which is how a plain array
  and a member array come to disagree about PRESERVE six months from now.
  The bounds come from FRedimPend: the lowers first (all of them or none), then
  the uppers, which is the order SedaiSSA lays them down. }
procedure TWasmBackend.EmitRedimShape(B: TWasmBuf; Preserve: Boolean;
  NLower, NUpper: Integer);
var
  k: Integer;
begin
  B.I32Const(1); B.LocalSet(FArrTmp);
  for k := 0 to NUpper - 1 do
  begin
    // the lower bound: the pushed one, or the dimension's current one
    if NLower > 0 then
      B.LocalGet(FRedimPend[k].Local)
    else
    begin
      B.LocalGet(FDescTmp); B.OpMem(wopI32Load, 2, LongWord(16 + 8 * k));
    end;
    B.LocalSet(FRecTmp);
    B.LocalGet(FDescTmp); B.LocalGet(FRecTmp);
    B.OpMem(wopI32Store, 2, LongWord(16 + 8 * k));
    // the size: ub - lb + 1, clamped at zero exactly as RedimArrayN clamps it
    B.LocalGet(FRedimPend[NLower + k].Local);
    B.LocalGet(FRecTmp); B.Op(wopI32Sub);
    B.I32Const(1); B.Op(wopI32Add);
    B.LocalTee(FGfxN);
    B.I32Const(0); B.Op(wopI32LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(0); B.LocalSet(FGfxN);
    B.EndOp;
    B.LocalGet(FDescTmp); B.LocalGet(FGfxN);
    B.OpMem(wopI32Store, 2, LongWord(20 + 8 * k));
    B.LocalGet(FArrTmp); B.LocalGet(FGfxN); B.Op(wopI32Mul);
    B.LocalSet(FArrTmp);
  end;

  B.LocalGet(FArrTmp); B.I32Const(8); B.Op(wopI32Mul);
  B.Call(FAllocFunc); B.LocalSet(FGfxP);
  if Preserve then
  begin
    { PRESERVE keeps the flat element order up to the SMALLER of the two sizes -
      SetLength's own rule, which is what the interpreter leans on. ⚠️ The OLD
      total is still in the descriptor: nothing above has touched +4, and it has
      to be read before the store below. }
    B.LocalGet(FGfxP);
    B.LocalGet(FDescTmp); B.OpMem(wopI32Load, 2, 0);
    B.LocalGet(FDescTmp); B.OpMem(wopI32Load, 2, 4);
    B.LocalTee(FGfxN);
    B.LocalGet(FArrTmp);
    B.Op(wopI32GtS);
    B.BlockStart(wopIf, WASM_TYPE_I32);
      B.LocalGet(FArrTmp);
    B.Op(wopElse);
      B.LocalGet(FGfxN);
    B.EndOp;
    B.I32Const(8); B.Op(wopI32Mul);
    B.MemoryCopy;
  end;
  B.LocalGet(FDescTmp); B.LocalGet(FGfxP);   B.OpMem(wopI32Store, 2, 0);
  B.LocalGet(FDescTmp); B.LocalGet(FArrTmp); B.OpMem(wopI32Store, 2, 4);
  B.LocalGet(FDescTmp); B.I32Const(NUpper);  B.OpMem(wopI32Store, 2, 8);
end;

procedure TWasmBackend.LoadMemberDesc(B: TWasmBuf; const Handle: TSSAValue;
  Enc: Integer; Allocate: Boolean);
{ FDescTmp := the descriptor of the array member at field encoding Enc of the
  record whose handle is in Handle. With Allocate, a member that has none yet
  gets one - lazily, exactly like the interpreter's FArrays entry, and only at
  the REDIM that first sizes it.
  ⚠️ The descriptor is allocated at its MAXIMUM size rather than at the size the
  first REDIM needs: a later REDIM with more dimensions would otherwise write
  past it, and unlike a declared array there is nothing at compile time that
  bounds the count for the whole program. }
begin
  LoadReg(B, Handle);
  B.Op(wopI32WrapI64);
  B.LocalTee(FGfxP);
  B.OpMem(wopI64Load, 0, LongWord(8 + (Enc shr 4)));
  B.Op(wopI32WrapI64);
  B.LocalSet(FDescTmp);
  if Allocate then
  begin
    B.LocalGet(FDescTmp); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(16 + 8 * WASM_MEMBER_MAX_DIMS);
      B.Call(FAllocFunc);
      B.LocalSet(FDescTmp);
      B.LocalGet(FGfxP);
      B.LocalGet(FDescTmp); B.Op(wopI64ExtendI32U);
      B.OpMem(wopI64Store, 0, LongWord(8 + (Enc shr 4)));
    B.EndOp;
  end;
end;

{ ---------------- FreeBASIC pointers ----------------

  Two kinds of pointer share these six helpers, told apart by bit 63 exactly as
  the interpreter tells them apart:

    a MANAGED pointer names an element of a backing array - (arrayId+1) in the
    high 32 bits, the element offset in the low 32. A scalar whose address is
    taken gets a one-element backing array, so "@x" and "@a(i)" are the same
    shape and "p + 1" is a plain integer add on both sides;

    a RECORD-FIELD pointer (@obj.field) sets bit 63, and packs the record in
    bits 24..55 with the field's encoding in the low 24. Natively those bits
    hold a record INDEX; here they hold the record's linear ADDRESS, which is
    what a handle is in this backend - the only place the two encodings differ,
    and it is invisible to a program that does not print a pointer.

  ⭐ The deref reads the DESCRIPTOR, not a remembered address, which is what
  makes a pointer survive a REDIM: the array moves and the pointer still names
  the same element. Getting that for free is the reason the interpreter's
  encoding was reproduced rather than replaced by a linear address.

  ⚠️ A bad pointer TRAPS here where the interpreter raises "Null or invalid
  pointer dereference". Both are loud, the diagnostics differ - the same
  arrangement as division by zero and an out-of-range subscript in CLASSIC. }

procedure TWasmBackend.EmitRefHelpers;
var
  B: TWasmBuf;
  RT: TSSARegisterType;
  T: LongWord;

  procedure Trap;
  begin
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.Op(wopUnreachable);
    B.EndOp;
  end;

  procedure ArrayPath(PtrL, AddrL: LongWord);
  { The managed arm: AddrL := the element's byte address. AddrL doubles as the
    scratch for the id and the descriptor on the way there. }
  begin
    // AddrL := arrayId + 1, which is 0 for NULL
    B.LocalGet(PtrL); B.I64Const(POINTER_ARRAY_SHIFT); B.Op(wopI64ShrU);
    B.Op(wopI32WrapI64); B.LocalTee(AddrL);
    B.Op(wopI32Eqz); Trap;
    B.LocalGet(AddrL); B.I32Const(FProg.GetArrayCount); B.Op(wopI32GtU); Trap;
    // the descriptor, from the table: base - 4 + 4*(id+1)
    B.LocalGet(AddrL); B.I32Const(4); B.Op(wopI32Mul);
    B.I32Const(LongInt(FArrTabAddr) - 4); B.Op(wopI32Add);
    B.OpMem(wopI32Load, 2, 0);
    B.LocalSet(AddrL);
    // the offset against the element count, unsigned so a negative one is out too
    B.LocalGet(PtrL); B.Op(wopI32WrapI64);
    B.LocalGet(AddrL); B.OpMem(wopI32Load, 2, 4);
    B.Op(wopI32GeU); Trap;
    B.LocalGet(AddrL); B.OpMem(wopI32Load, 2, 0);
    B.LocalGet(PtrL); B.Op(wopI32WrapI64); B.I32Const(8); B.Op(wopI32Mul);
    B.Op(wopI32Add);
    B.LocalSet(AddrL);
  end;

  procedure Decode(PtrL, AddrL, EncL: LongWord; Str: Boolean);
  { AddrL := where the value lives, EncL := the field's width code (0 for the
    managed arm, which is always a full eight bytes). For the STRING bank the
    record arm addresses the string AREA the header points at, because a handle
    cannot live in the byte image - the same split as ssaRecordLoadString. }
  begin
    B.LocalGet(PtrL); B.I64Const(0); B.Op(wopI64LtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(PtrL); B.Op(wopI32WrapI64);
      B.I32Const(LongInt(RECPTR_SLOT_MASK)); B.Op(wopI32And);
      B.LocalSet(EncL);
      B.LocalGet(PtrL); B.I64Const(RECPTR_SLOT_BITS); B.Op(wopI64ShrU);
      B.Op(wopI32WrapI64);
      if Str then
      begin
        B.LocalTee(AddrL);
        B.LocalGet(AddrL); B.OpMem(wopI32Load, 2, 4);   // where the strings start
        B.Op(wopI32Add);
        B.LocalGet(EncL); B.I32Const(4); B.Op(wopI32Mul); B.Op(wopI32Add);
        B.LocalSet(AddrL);
        B.I32Const(0); B.LocalSet(EncL);
      end
      else
      begin
        B.I32Const(8); B.Op(wopI32Add);                 // past the header
        B.LocalGet(EncL); B.I32Const(4); B.Op(wopI32ShrU); B.Op(wopI32Add);
        B.LocalSet(AddrL);
        B.LocalGet(EncL); B.I32Const($F); B.Op(wopI32And); B.LocalSet(EncL);
      end;
    B.Op(wopElse);
      ArrayPath(PtrL, AddrL);
      B.I32Const(0); B.LocalSet(EncL);
    B.EndOp;
  end;

  procedure WidthCase(AddrL, EncL: LongWord; Code: Integer; MemOp: Byte;
                      ValL: Integer);
  { One arm of the width switch: "if enc = Code then <the narrow access>". A
    chain rather than a br_table because the widths are six and the shape stays
    readable - and because nothing outside SedaiWasmControl computes a branch
    depth by hand. }
  begin
    B.LocalGet(EncL); B.I32Const(Code); B.Op(wopI32Eq);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(AddrL);
      if ValL >= 0 then B.LocalGet(LongWord(ValL));
      B.OpMem(MemOp, 0, 0);
      B.Op(wopReturn);
    B.EndOp;
  end;

begin
  for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
  begin
    { ---- load: (ptr) -> value.  locals 1 = addr, 2 = enc ---- }
    T := FModule.TypeIndex([wvtI64], [BankType[RT]]);
    B := TWasmBuf.Create;
    try
      Decode(0, 1, 2, RT = srtString);
      case RT of
        srtString:
          B.LocalGet(1);
        srtFloat:
          begin
            // width 7 is a SINGLE, and it really is four bytes
            B.LocalGet(2); B.I32Const(7); B.Op(wopI32Eq);
            B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
              B.LocalGet(1); B.OpMem(wopF32Load, 0, 0);
              B.Op(wopF64PromoteF32); B.Op(wopReturn);
            B.EndOp;
            B.LocalGet(1);
          end;
      else
        begin
          WidthCase(1, 2, 1, wopI64Load8S,  -1);
          WidthCase(1, 2, 2, wopI64Load8U,  -1);
          WidthCase(1, 2, 3, wopI64Load16S, -1);
          WidthCase(1, 2, 4, wopI64Load16U, -1);
          WidthCase(1, 2, 5, wopI64Load32S, -1);
          WidthCase(1, 2, 6, wopI64Load32U, -1);
          B.LocalGet(1);
        end;
      end;
      case RT of
        srtFloat:  B.OpMem(wopF64Load, 0, 0);
        srtString: B.OpMem(wopI32Load, 0, 0);
      else
        B.OpMem(wopI64Load, 0, 0);
      end;
      FRefLoad[RT] := FModule.AddFunction(T, [wvtI32, wvtI32], B);
    finally
      B.Free;
    end;

    { ---- store: (ptr, value) -> ().  locals 2 = addr, 3 = enc ---- }
    T := FModule.TypeIndex([wvtI64, BankType[RT]], []);
    B := TWasmBuf.Create;
    try
      Decode(0, 2, 3, RT = srtString);
      case RT of
        srtString:
          begin
            B.LocalGet(2); B.LocalGet(1); B.OpMem(wopI32Store, 0, 0);
          end;
        srtFloat:
          begin
            B.LocalGet(3); B.I32Const(7); B.Op(wopI32Eq);
            B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
              B.LocalGet(2); B.LocalGet(1); B.Op(wopF32DemoteF64);
              B.OpMem(wopF32Store, 0, 0);
              B.Op(wopReturn);
            B.EndOp;
            B.LocalGet(2); B.LocalGet(1); B.OpMem(wopF64Store, 0, 0);
          end;
      else
        begin
          WidthCase(2, 3, 1, wopI64Store8,  1);
          WidthCase(2, 3, 2, wopI64Store8,  1);
          WidthCase(2, 3, 3, wopI64Store16, 1);
          WidthCase(2, 3, 4, wopI64Store16, 1);
          WidthCase(2, 3, 5, wopI64Store32, 1);
          WidthCase(2, 3, 6, wopI64Store32, 1);
          B.LocalGet(2); B.LocalGet(1); B.OpMem(wopI64Store, 0, 0);
        end;
      end;
      FRefStore[RT] := FModule.AddFunction(T, [wvtI32, wvtI32], B);
    finally
      B.Free;
    end;
  end;
end;

{ ---------------- PRINT of a float ----------------

  The same algorithm the interpreter runs, and it ports because of what that
  algorithm is: the digits of a double come from its EXACT value, and the exact
  value is an integer built by repeated multiplication. A double is M x 2^E, so
  for E >= 0 it is the integer M x 2^E, and for E < 0 it is M x 5^(-E) / 10^(-E)
  - either way, no division, no floating point, nothing that needs a wider type
  than an i64.

  ⛔ THAT IS THE ONLY REASON THIS EXISTS AT ALL. Reproducing what the native side
  USED to print would have meant reproducing FPC's str_real: 543 lines that
  generate digits in FLOATING POINT and reach 17 of them only by using 80-bit
  Extended. WebAssembly has no 80-bit type, so that was not an expensive road,
  it was a closed one. Fixing the interpreter to round correctly (IEEE 754-2019
  sec.5.12.2) is what made a port possible - native and WebAssembly now agree by
  construction rather than by luck. See job/docs/PIANO_FLOAT_PRINT.md. }

procedure TWasmBackend.EmitFloatHelpers;
var
  B: TWasmBuf;
  TMul, TDec, TPrint: LongWord;

  procedure LoadLen;
  begin B.I32Const(FLT_LEN); B.OpMem(wopI32Load, 2, 0); end;

  procedure StoreLenFrom(Local: LongWord);
  begin B.I32Const(FLT_LEN); B.LocalGet(Local); B.OpMem(wopI32Store, 2, 0); end;

begin
  { fltMul(f: i64): multiply the digit buffer by f, in place, growing it.
    ⭐ f is applied in CHUNKS (5^13, 2^30) rather than one factor at a time: a
    digit is at most 9, so 9*5^13 plus the carry still fits an i64, and 1074
    passes over the buffer collapse into 83. }
  TMul := FModule.TypeIndex([wvtI64], []);
  B := TWasmBuf.Create;
  try
    // locals: 1 = i, 2 = len (i32); 3 = carry, 4 = t (i64)
    LoadLen; B.LocalSet(2);
    B.I64Const(0); B.LocalSet(3);
    B.I32Const(0); B.LocalSet(1);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(1); B.LocalGet(2); B.Op(wopI32GeS); B.BrIf(1);
        B.I32Const(FLT_DEC); B.LocalGet(1); B.Op(wopI32Add);
        B.OpMem(wopI32Load8U, 0, 0); B.Op(wopI64ExtendI32U);
        B.LocalGet(0); B.Op(wopI64Mul);
        B.LocalGet(3); B.Op(wopI64Add);
        B.LocalSet(4);
        B.I32Const(FLT_DEC); B.LocalGet(1); B.Op(wopI32Add);
        B.LocalGet(4); B.I64Const(10); B.Op(wopI64RemU); B.Op(wopI32WrapI64);
        B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(4); B.I64Const(10); B.Op(wopI64DivU); B.LocalSet(3);
        B.LocalGet(1); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(1);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    // the carry becomes new high digits
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(3); B.Op(wopI64Eqz); B.BrIf(1);
        B.I32Const(FLT_DEC); B.LocalGet(2); B.Op(wopI32Add);
        B.LocalGet(3); B.I64Const(10); B.Op(wopI64RemU); B.Op(wopI32WrapI64);
        B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(3); B.I64Const(10); B.Op(wopI64DivU); B.LocalSet(3);
        B.LocalGet(2); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(2);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    StoreLenFrom(2);
    FFltMulFunc := FModule.AddFunction(TMul, [wvtI32, wvtI32, wvtI64, wvtI64], B);
  finally
    B.Free;
  end;

  { fltDec(v: f64): the EXACT decimal digits of |v| into FLT_DEC, least
    significant first, with FLT_LEN and FLT_FRAC set. }
  TDec := FModule.TypeIndex([wvtF64], []);
  B := TWasmBuf.Create;
  try
    // locals: 1 = bits, 2 = M, 5 = mul (i64); 3 = E, 4 = k, 6 = i, 7 = c (i32)
    B.LocalGet(0); B.Op(wopI64ReinterpretF64); B.LocalSet(1);
    B.LocalGet(1); B.I64Const($000FFFFFFFFFFFFF); B.Op(wopI64And); B.LocalSet(2);
    B.LocalGet(1); B.I64Const(52); B.Op(wopI64ShrU);
      B.I64Const($7FF); B.Op(wopI64And); B.Op(wopI32WrapI64); B.LocalSet(3);
    // a subnormal has no implicit leading bit, and its exponent is the fixed floor
    B.LocalGet(3); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(-1074); B.LocalSet(3);
    B.Op(wopElse);
      B.LocalGet(2); B.I64Const(Int64(1) shl 52); B.Op(wopI64Or); B.LocalSet(2);
      B.LocalGet(3); B.I32Const(1075); B.Op(wopI32Sub); B.LocalSet(3);
    B.EndOp;

    B.I32Const(FLT_LEN); B.I32Const(0); B.OpMem(wopI32Store, 2, 0);
    B.I32Const(0); B.LocalSet(6);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(2); B.Op(wopI64Eqz); B.BrIf(1);
        B.I32Const(FLT_DEC); B.LocalGet(6); B.Op(wopI32Add);
        B.LocalGet(2); B.I64Const(10); B.Op(wopI64RemU); B.Op(wopI32WrapI64);
        B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(2); B.I64Const(10); B.Op(wopI64DivU); B.LocalSet(2);
        B.LocalGet(6); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(6);
        B.Br(0);
      B.EndOp;
    B.EndOp;
    StoreLenFrom(6);

    B.LocalGet(3); B.I32Const(0); B.Op(wopI32GeS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      // E >= 0: the value is the integer M * 2^E, so double it E times
      B.I32Const(FLT_FRAC); B.I32Const(0); B.OpMem(wopI32Store, 2, 0);
      B.LocalGet(3); B.LocalSet(6);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(6); B.I32Const(0); B.Op(wopI32LeS); B.BrIf(1);
          B.LocalGet(6); B.LocalSet(4);
          B.LocalGet(4); B.I32Const(30); B.Op(wopI32GtS);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.I32Const(30); B.LocalSet(4);
          B.EndOp;
          B.I64Const(1); B.LocalGet(4); B.Op(wopI64ExtendI32U); B.Op(wopI64Shl);
          B.Call(FFltMulFunc);
          B.LocalGet(6); B.LocalGet(4); B.Op(wopI32Sub); B.LocalSet(6);
          B.Br(0);
        B.EndOp;
      B.EndOp;
    B.Op(wopElse);
      // E < 0: the value is M * 5^(-E) / 10^(-E) - so the digits are those of
      // M * 5^(-E), and the point sits -E places from the right
      B.I32Const(FLT_FRAC); B.I32Const(0); B.LocalGet(3); B.Op(wopI32Sub);
        B.OpMem(wopI32Store, 2, 0);
      B.I32Const(0); B.LocalGet(3); B.Op(wopI32Sub); B.LocalSet(6);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(6); B.I32Const(0); B.Op(wopI32LeS); B.BrIf(1);
          B.LocalGet(6); B.LocalSet(4);
          B.LocalGet(4); B.I32Const(13); B.Op(wopI32GtS);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.I32Const(13); B.LocalSet(4);
          B.EndOp;
          B.I64Const(1); B.LocalSet(5);
          B.LocalGet(4); B.LocalSet(7);
          B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
            B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
              B.LocalGet(7); B.I32Const(0); B.Op(wopI32LeS); B.BrIf(1);
              B.LocalGet(5); B.I64Const(5); B.Op(wopI64Mul); B.LocalSet(5);
              B.LocalGet(7); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(7);
              B.Br(0);
            B.EndOp;
          B.EndOp;
          B.LocalGet(5); B.Call(FFltMulFunc);
          B.LocalGet(6); B.LocalGet(4); B.Op(wopI32Sub); B.LocalSet(6);
          B.Br(0);
        B.EndOp;
      B.EndOp;
    B.EndOp;
    FFltDecFunc := FModule.AddFunction(TDec,
      [wvtI64, wvtI64, wvtI32, wvtI32, wvtI64, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;

  EmitFloatPrint;

  { fltOut(len): where the rendered text goes. It always records the length, and
    it writes only when the caller did not ask to keep it.
    ⛔ The write arm is emitted only when the program PRINTS: a program that
    calls Str() on a float and never prints has no "write" import at all, and a
    call to a function the module does not have is not a runtime surprise - it
    is a module that fails to load. }
  B := TWasmBuf.Create;
  try
    B.I32Const(FLT_OLEN); B.LocalGet(0); B.OpMem(wopI32Store, 2, 0);
    if FUsesPrint then
    begin
      B.I32Const(FLT_CAP); B.OpMem(wopI32Load, 2, 0); B.Op(wopI32Eqz);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(FLT_OUT); B.LocalGet(0); B.Call(WriteTarget);
      B.EndOp;
    end;
    FFltOutFunc := FModule.AddFunction(FModule.TypeIndex([wvtI32], []), [], B);
  finally
    B.Free;
  end;

  { fltStr() -> handle: the text fltPrint just left at FLT_OUT, with the spaces
    PRINT puts around a number taken off, copied into a string.
    ⭐ Str is not a second formatter - it is this one minus the spacing, which is
    exactly what the interpreter does (Trim of FormatNumber). Writing it any
    other way would give a program two renderings of the same double that agree
    until they do not.
    ⚠️ MODERN only, and the target is MODERN only: Commodore's STR$ keeps the
    leading sign-space and drops just the trailing one. }
  if FUsesStrStr then
  begin
    B := TWasmBuf.Create;
    try
      // 0 = p, 1 = e, 2 = h, 3 = n
      B.I32Const(FLT_OUT); B.LocalSet(0);
      B.I32Const(FLT_OUT); B.I32Const(FLT_OLEN); B.OpMem(wopI32Load, 2, 0);
      B.Op(wopI32Add); B.LocalSet(1);
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(0); B.LocalGet(1); B.Op(wopI32GeU); B.BrIf(1);
          B.LocalGet(0); B.OpMem(wopI32Load8U, 0, 0);
          B.I32Const(Ord(' ')); B.Op(wopI32Ne); B.BrIf(1);
          B.LocalGet(0); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(0);
          B.Br(0);
        B.EndOp;
      B.EndOp;
      B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
        B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(1); B.LocalGet(0); B.Op(wopI32LeU); B.BrIf(1);
          B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub);
          B.OpMem(wopI32Load8U, 0, 0);
          B.I32Const(Ord(' ')); B.Op(wopI32Ne); B.BrIf(1);
          B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(1);
          B.Br(0);
        B.EndOp;
      B.EndOp;
      B.LocalGet(1); B.LocalGet(0); B.Op(wopI32Sub); B.LocalTee(3);
      B.Call(FStrNewFunc); B.LocalTee(2);
      B.I32Const(4); B.Op(wopI32Add);
      B.LocalGet(0); B.LocalGet(3);
      B.MemoryCopy;
      B.LocalGet(2);
      FFltStrFunc := FModule.AddFunction(FModule.TypeIndex([], [wvtI32]),
                                         [wvtI32, wvtI32, wvtI32, wvtI32], B);
    finally
      B.Free;
    end;
  end;
end;


procedure TWasmBackend.EmitFloatPrint;
{ fltPrint(v: f64, sig: i32): TConsoleBehavior.FormatNumber for FreeBASIC, with
  the digit count as an ARGUMENT so a Single (7), a Double (16) and any
  "OPTION DIGITS n" all share one function.

  Everything below mirrors the interpreter step for step, because that is what
  the differential compares: a leading space stands in for the sign, nothing
  trails, the specials are MSVCRT's spellings, negative zero prints "-0", and
  the fixed/exponential choice is %g's - exponential when the decimal exponent
  is below -4 or at least the digit count. }
var
  B: TWasmBuf;
  TPrint: LongWord;

  procedure Emit(Ch: Integer);        // *p++ = Ch
  begin
    B.LocalGet(8); B.I32Const(Ch); B.OpMem(wopI32Store8, 0, 0);
    B.LocalGet(8); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(8);
  end;

  procedure EmitDigitAt(IdxLocal: LongWord);   // *p++ = '0' + FLT_DIG[idx]
  begin
    B.LocalGet(8);
    B.I32Const(FLT_DIG); B.LocalGet(IdxLocal); B.Op(wopI32Add);
    B.OpMem(wopI32Load8U, 0, 0); B.I32Const(Ord('0')); B.Op(wopI32Add);
    B.OpMem(wopI32Store8, 0, 0);
    B.LocalGet(8); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(8);
  end;

  procedure LastNonZeroFrom(LowLocal: Integer);
  { local 9 := the highest index above `low` whose digit is non-zero, or `low`
    itself. It is what strips the trailing zeros the padding leaves behind. }
  begin
    B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(9);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(9);
        if LowLocal < 0 then B.I32Const(0) else B.LocalGet(LongWord(LowLocal));
        B.Op(wopI32LeS); B.BrIf(1);
        B.I32Const(FLT_DIG); B.LocalGet(9); B.Op(wopI32Add);
        B.OpMem(wopI32Load8U, 0, 0); B.BrIf(1);
        B.LocalGet(9); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(9);
        B.Br(0);
      B.EndOp;
    B.EndOp;
  end;

  procedure EmitRangeToK;
  { for i := local 6 to local 9: emit digit i. }
  begin
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(6); B.LocalGet(9); B.Op(wopI32GtS); B.BrIf(1);
        EmitDigitAt(6);
        B.LocalGet(6); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(6);
        B.Br(0);
      B.EndOp;
    B.EndOp;
  end;

begin
  TPrint := FModule.TypeIndex([wvtF64, wvtI32], []);
  B := TWasmBuf.Create;
  try
    // 2 = bits(i64); 3 = neg, 4 = len, 5 = ex, 6 = i, 7 = j, 8 = p, 9 = k,
    // 10 = roundup, 11 = d  (all i32)
    B.LocalGet(0); B.Op(wopI64ReinterpretF64); B.LocalSet(2);
    B.LocalGet(2); B.I64Const(0); B.Op(wopI64LtS); B.LocalSet(3);
    B.I32Const(FLT_OUT); B.LocalSet(8);

    { NaN and infinity never reach the digit machinery - the interpreter cannot
      let them either, because Frac and FloatToStr trap on them. FreeBASIC hands
      them to the platform's C library, so both sides print MSVCRT's spelling
      rather than one of their own. }
    B.LocalGet(2); B.I64Const(52); B.Op(wopI64ShrU);
      B.I64Const($7FF); B.Op(wopI64And); B.I64Const($7FF); B.Op(wopI64Eq);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(3); B.Op(wopI32Eqz);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        Emit(Ord(' '));
      B.EndOp;
      B.LocalGet(2); B.I64Const($000FFFFFFFFFFFFF); B.Op(wopI64And); B.Op(wopI64Eqz);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(3);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(CONST_NINF); B.LocalSet(9); B.I32Const(7); B.LocalSet(11);
        B.Op(wopElse);
          B.I32Const(CONST_INF); B.LocalSet(9); B.I32Const(6); B.LocalSet(11);
        B.EndOp;
      B.Op(wopElse);
        { The sign bit tells the two NaNs apart: SET is the "indefinite" an
          invalid operation makes (0/0, Sqr of a negative), CLEAR the quiet NaN
          the C library returns. }
        B.LocalGet(3);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(CONST_IND); B.LocalSet(9); B.I32Const(7); B.LocalSet(11);
        B.Op(wopElse);
          B.I32Const(CONST_QNAN); B.LocalSet(9); B.I32Const(7); B.LocalSet(11);
        B.EndOp;
      B.EndOp;
      B.LocalGet(8); B.LocalGet(9); B.LocalGet(11); B.MemoryCopy;
      B.LocalGet(8); B.LocalGet(11); B.Op(wopI32Add); B.LocalSet(8);
      B.LocalGet(8); B.I32Const(FLT_OUT); B.Op(wopI32Sub);
      B.Call(FFltOutFunc);
      B.Op(wopReturn);
    B.EndOp;

    { Zero. -0.0 compares EQUAL to zero, so its sign can only come from the bit -
      and FreeBASIC does print "-0". }
    B.LocalGet(0); B.F64Const(0); B.Op(wopF64Eq);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(Ord('-')); B.I32Const(Ord(' ')); B.LocalGet(3); B.Op(wopSelect);
      B.LocalSet(11);
      B.LocalGet(8); B.LocalGet(11); B.OpMem(wopI32Store8, 0, 0);
      B.LocalGet(8); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(8);
      Emit(Ord('0'));
      B.LocalGet(8); B.I32Const(FLT_OUT); B.Op(wopI32Sub);
      B.Call(FFltOutFunc);
      B.Op(wopReturn);
    B.EndOp;

    // the exact digits, and the decimal exponent of the leading one
    B.LocalGet(0); B.Call(FFltDecFunc);
    B.I32Const(FLT_LEN); B.OpMem(wopI32Load, 2, 0); B.LocalSet(4);
    B.LocalGet(4); B.I32Const(1); B.Op(wopI32Sub);
      B.I32Const(FLT_FRAC); B.OpMem(wopI32Load, 2, 0); B.Op(wopI32Sub); B.LocalSet(5);

    // the top `sig` digits, most significant first, zero-padded when the exact
    // expansion is shorter than asked for - which is not padding but the truth:
    // a double's expansion terminates
    B.I32Const(0); B.LocalSet(6);
    B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
      B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(6); B.LocalGet(1); B.Op(wopI32GeS); B.BrIf(1);
        B.LocalGet(6); B.LocalGet(4); B.Op(wopI32LtS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(FLT_DEC); B.LocalGet(4); B.Op(wopI32Add);
          B.I32Const(1); B.Op(wopI32Sub);
          B.LocalGet(6); B.Op(wopI32Sub);
          B.OpMem(wopI32Load8U, 0, 0); B.LocalSet(11);
        B.Op(wopElse);
          B.I32Const(0); B.LocalSet(11);
        B.EndOp;
        B.I32Const(FLT_DIG); B.LocalGet(6); B.Op(wopI32Add);
        B.LocalGet(11); B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(6); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(6);
        B.Br(0);
      B.EndOp;
    B.EndOp;

    { ROUND, ONCE, half to even - and the decision looks at EVERY dropped digit,
      not just the first. Looking only at the first is what a double rounding
      does, and it disagrees with the correct answer on 4.75% of doubles. }
    B.LocalGet(4); B.LocalGet(1); B.Op(wopI32GtS);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.I32Const(FLT_DEC); B.LocalGet(4); B.Op(wopI32Add);
      B.LocalGet(1); B.Op(wopI32Sub); B.I32Const(1); B.Op(wopI32Sub);
      B.OpMem(wopI32Load8U, 0, 0); B.LocalSet(11);
      B.I32Const(0); B.LocalSet(10);
      B.LocalGet(11); B.I32Const(5); B.Op(wopI32GtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(1); B.LocalSet(10);
      B.Op(wopElse);
        B.LocalGet(11); B.I32Const(5); B.Op(wopI32Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(0); B.LocalSet(7);
          B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
            B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
              B.LocalGet(7);
              B.LocalGet(4); B.LocalGet(1); B.Op(wopI32Sub);
                B.I32Const(1); B.Op(wopI32Sub);
              B.Op(wopI32GeS); B.BrIf(1);
              B.I32Const(FLT_DEC); B.LocalGet(7); B.Op(wopI32Add);
              B.OpMem(wopI32Load8U, 0, 0);
              B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
                B.I32Const(1); B.LocalSet(10);
                B.Br(2);
              B.EndOp;
              B.LocalGet(7); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(7);
              B.Br(0);
            B.EndOp;
          B.EndOp;
          // an EXACT tie, and only here: round to even
          B.LocalGet(10); B.Op(wopI32Eqz);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.I32Const(FLT_DIG); B.LocalGet(1); B.Op(wopI32Add);
            B.I32Const(1); B.Op(wopI32Sub);
            B.OpMem(wopI32Load8U, 0, 0); B.I32Const(1); B.Op(wopI32And);
            B.LocalSet(10);
          B.EndOp;
        B.EndOp;
      B.EndOp;
      B.LocalGet(10);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(1); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(6);
        B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
          B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(6); B.I32Const(0); B.Op(wopI32LtS); B.BrIf(1);
            B.I32Const(FLT_DIG); B.LocalGet(6); B.Op(wopI32Add);
            B.OpMem(wopI32Load8U, 0, 0); B.LocalSet(11);
            B.LocalGet(11); B.I32Const(9); B.Op(wopI32LtS);
            B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
              B.I32Const(FLT_DIG); B.LocalGet(6); B.Op(wopI32Add);
              B.LocalGet(11); B.I32Const(1); B.Op(wopI32Add);
              B.OpMem(wopI32Store8, 0, 0);
              B.Br(2);
            B.EndOp;
            B.I32Const(FLT_DIG); B.LocalGet(6); B.Op(wopI32Add);
            B.I32Const(0); B.OpMem(wopI32Store8, 0, 0);
            B.LocalGet(6); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(6);
            B.Br(0);
          B.EndOp;
        B.EndOp;
        // the carry ran off the front: 99..9 became 10..0, one decade up
        B.LocalGet(6); B.I32Const(0); B.Op(wopI32LtS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(FLT_DIG); B.I32Const(1); B.OpMem(wopI32Store8, 0, 0);
          B.LocalGet(5); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(5);
        B.EndOp;
      B.EndOp;
    B.EndOp;

    // the sign: a leading space stands in for it when the value is not negative
    B.LocalGet(3); B.Op(wopI32Eqz);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      Emit(Ord(' '));
    B.Op(wopElse);
      Emit(Ord('-'));
    B.EndOp;

    B.LocalGet(5); B.I32Const(-4); B.Op(wopI32GeS);
    B.LocalGet(5); B.LocalGet(1); B.Op(wopI32LtS);
    B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
      B.LocalGet(5); B.I32Const(0); B.Op(wopI32GeS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        // ddd.ddd - the point sits after digit ex
        B.I32Const(0); B.LocalSet(6);
        B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
          B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(6); B.LocalGet(5); B.Op(wopI32GtS); B.BrIf(1);
            EmitDigitAt(6);
            B.LocalGet(6); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(6);
            B.Br(0);
          B.EndOp;
        B.EndOp;
        LastNonZeroFrom(5);
        B.LocalGet(9); B.LocalGet(5); B.Op(wopI32GtS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          Emit(Ord('.'));
          B.LocalGet(5); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(6);
          EmitRangeToK;
        B.EndOp;
      B.Op(wopElse);
        // 0.000ddd - ex is negative, so -ex-1 zeros come first
        Emit(Ord('0'));
        Emit(Ord('.'));
        B.I32Const(0); B.LocalSet(6);
        B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
          B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(6);
            B.I32Const(0); B.LocalGet(5); B.Op(wopI32Sub);
              B.I32Const(1); B.Op(wopI32Sub);
            B.Op(wopI32GeS); B.BrIf(1);
            Emit(Ord('0'));
            B.LocalGet(6); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(6);
            B.Br(0);
          B.EndOp;
        B.EndOp;
        LastNonZeroFrom(-1);
        B.I32Const(0); B.LocalSet(6);
        EmitRangeToK;
      B.EndOp;
    B.Op(wopElse);
      // d.dddde+xx, the exponent signed and AT LEAST TWO digits wide (three when it needs three)
      LastNonZeroFrom(-1);
      B.LocalGet(8);
      B.I32Const(FLT_DIG); B.OpMem(wopI32Load8U, 0, 0);
        B.I32Const(Ord('0')); B.Op(wopI32Add);
      B.OpMem(wopI32Store8, 0, 0);
      B.LocalGet(8); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(8);
      B.LocalGet(9); B.I32Const(0); B.Op(wopI32GtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        Emit(Ord('.'));
        B.I32Const(1); B.LocalSet(6);
        EmitRangeToK;
      B.EndOp;
      Emit(Ord('e'));
      B.I32Const(Ord('+')); B.I32Const(Ord('-'));
        B.LocalGet(5); B.I32Const(0); B.Op(wopI32GeS); B.Op(wopSelect);
      B.LocalSet(11);
      B.LocalGet(8); B.LocalGet(11); B.OpMem(wopI32Store8, 0, 0);
      B.LocalGet(8); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(8);
      B.LocalGet(5); B.LocalSet(7);
      B.LocalGet(7); B.I32Const(0); B.Op(wopI32LtS);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.I32Const(0); B.LocalGet(7); B.Op(wopI32Sub); B.LocalSet(7);
      B.EndOp;
      // The HUNDREDS digit only when the exponent has one: %g's rule is at least TWO digits, widening
      // to three only for 100 and up ("1e+16", "1e+300"). This used to emit three unconditionally,
      // and after the interpreter was corrected it was the last place still printing "1e+020".
      B.LocalGet(7); B.I32Const(100); B.Op(wopI32GeU);
      B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
        B.LocalGet(8);
        B.LocalGet(7); B.I32Const(100); B.Op(wopI32DivU);
          B.I32Const(10); B.Op(wopI32RemU); B.I32Const(Ord('0')); B.Op(wopI32Add);
        B.OpMem(wopI32Store8, 0, 0);
        B.LocalGet(8); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(8);
      B.EndOp;
      B.LocalGet(8);
      B.LocalGet(7); B.I32Const(10); B.Op(wopI32DivU);
        B.I32Const(10); B.Op(wopI32RemU); B.I32Const(Ord('0')); B.Op(wopI32Add);
      B.OpMem(wopI32Store8, 0, 0);
      B.LocalGet(8); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(8);
      B.LocalGet(8);
      B.LocalGet(7); B.I32Const(10); B.Op(wopI32RemU);
        B.I32Const(Ord('0')); B.Op(wopI32Add);
      B.OpMem(wopI32Store8, 0, 0);
      B.LocalGet(8); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(8);
    B.EndOp;

    B.LocalGet(8); B.I32Const(FLT_OUT); B.Op(wopI32Sub);
    B.Call(FFltOutFunc);
    FFltPrintFunc := FModule.AddFunction(TPrint,
      [wvtI64, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32, wvtI32], B);
  finally
    B.Free;
  end;
end;

constructor TWasmBackend.Create(AProgram: TSSAProgram; AModern: Boolean);
begin
  inherited Create;
  FProg := AProgram;
  FModern := AModern;
  FFltDigits := 16;                  // the dialect default; the host may override
  FQBLang := False;
  FModule := TWasmModule.Create;
end;

destructor TWasmBackend.Destroy;
begin
  FModule.Free;
  inherited Destroy;
end;

function TWasmBackend.Fail(const Msg: string): Boolean;
begin
  if FError = '' then FError := Msg;
  Result := False;
end;

function BankName(T: TSSARegisterType): string;
begin
  case T of
    srtInt:    Result := 'int';
    srtFloat:  Result := 'float';
  else         Result := 'string';
  end;
end;

function TWasmBackend.BankIs(const V: TSSAValue; Want: TSSARegisterType;
  const Who: string): Boolean;
{ Does this operand live in the bank its opcode reads it from?

  ⛔ It normally does, and where it does not the front end has produced a
  BANK COLLISION - an instruction of one bank holding a register index of
  another. The interpreter survives those: it reads bank-typed arrays, so a
  string index into the float bank quietly answers whatever that float register
  held, usually zero. ⚠️ A WASM module cannot: the helper's parameter is f64 and
  the value on the stack is an i32, so the module DOES NOT VALIDATE - which is
  the one outcome this backend is not allowed to produce.

  Found by writing the VAL guardian: "Dim As Double d = ""3.14""" compiles to
  LoadConstString R0 followed by Print R0, and Print there is the FLOAT print.
  ⭐ fbc REJECTS that line outright (error 24, "Invalid data types"), so the
  interpreter accepting it and answering 0 is a fidelity defect in its own
  right - recorded, not fixed here, because changing an assignment rule has a
  blast radius this block did not measure. Refusing is what the backend owes
  either way. }
begin
  if (V.Kind = svkRegister) and (V.RegType <> Want) then
    Exit(Fail(Format('%s reads a %s register where the opcode wants a %s one - '
      + 'the front end put an operand in the wrong bank', [Who,
      BankName(V.RegType), BankName(Want)])));
  Result := True;
end;

function TWasmBackend.OpName(Op: TSSAOpCode): string;
begin
  Result := GetEnumName(TypeInfo(TSSAOpCode), Ord(Op));
end;

function TWasmBackend.BlockOfLabel(const AName: string): Integer;
var
  i: Integer;
begin
  for i := 0 to FProg.Blocks.Count - 1 do
    if SameText(FProg.Blocks[i].LabelName, AName) then Exit(i);
  Result := -1;
end;

{ ---------------- 1. partition into procedure regions ---------------- }

function TWasmBackend.BuildPartition: Boolean;
// Procedure bodies are contiguous block regions past the module's END, reachable
// only through ssaCallSub (LowerDeferredProcedures). So the entries partition
// the block list, and region 0 is the module itself.
var
  i, j, k, B, N: Integer;
  Blk: TSSABasicBlock;
  Instr: TSSAInstruction;
  IsEntry: array of Boolean;
  Target: Integer;
begin
  N := FProg.Blocks.Count;
  if N = 0 then Exit(Fail('the program has no basic blocks'));
  SetLength(IsEntry, N);
  for i := 0 to N - 1 do IsEntry[i] := False;

  for i := 0 to N - 1 do
  begin
    Blk := FProg.Blocks[i];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := TSSAInstruction(Blk.Instructions[j]);
      if (Instr.OpCode = ssaCallSub) and (Instr.Dest.Kind = svkLabel) then
      begin
        B := BlockOfLabel(Instr.Dest.LabelName);
        if B < 0 then
          Exit(Fail(Format('call to "%s", which is not a block in this program',
                           [Instr.Dest.LabelName])));
        IsEntry[B] := True;
      end
      { ⛔ A procedure whose address is TAKEN is an entry too, and forgetting it
        is not a missing feature but a miscompilation: a SUB that is only ever
        called through a pointer has no ssaCallSub naming it, so without this it
        would be folded into whatever region its blocks happen to follow - and
        the table would then hold a function that is the wrong half of somebody
        else's body. }
      else if (Instr.OpCode = ssaLoadProcAddr) and (Instr.Src1.Kind = svkLabel) then
      begin
        B := BlockOfLabel(Instr.Src1.LabelName);
        if B < 0 then
          Exit(Fail(Format('the address of "%s" is taken, but it is not a block in this program',
                           [Instr.Src1.LabelName])));
        IsEntry[B] := True;
      end;
    end;
  end;
  IsEntry[0] := True;                       // region 0 is the module

  SetLength(FRegionOf, N);
  FRegionCount := 0;
  for i := 0 to N - 1 do
  begin
    if IsEntry[i] then
    begin
      Inc(FRegionCount);
      SetLength(FRegionFirst, FRegionCount);
      SetLength(FRegionLast, FRegionCount);
      SetLength(FRegionName, FRegionCount);
      FRegionFirst[FRegionCount - 1] := i;
      FRegionName[FRegionCount - 1] := FProg.Blocks[i].LabelName;
    end;
    FRegionOf[i] := FRegionCount - 1;
    FRegionLast[FRegionCount - 1] := i;
  end;
  if FRegionName[0] = '' then FRegionName[0] := 'main';

  // A jump that leaves its region would break the dispatch, which only knows
  // the blocks of one function. Refuse rather than emit a wrong branch.
  for i := 0 to N - 1 do
  begin
    Blk := FProg.Blocks[i];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := TSSAInstruction(Blk.Instructions[j]);
      if (Instr.Dest.Kind = svkLabel) and
         (Instr.OpCode in [ssaJump, ssaJumpIfZero, ssaJumpIfNotZero]) then
      begin
        Target := BlockOfLabel(Instr.Dest.LabelName);
        if Target < 0 then
          Exit(Fail(Format('jump to unknown label "%s"', [Instr.Dest.LabelName])));
        if FRegionOf[Target] <> FRegionOf[i] then
          Exit(Fail(Format('a jump crosses a procedure boundary: block "%s" (%s) -> "%s" (%s)',
                    [Blk.LabelName, FRegionName[FRegionOf[i]],
                     Instr.Dest.LabelName, FRegionName[FRegionOf[Target]]])));
      end;
    end;
  end;

  SetLength(FParamCount, FRegionCount);
  SetLength(FSlotCount, FRegionCount);
  SetLength(FOutSlot, FRegionCount);
  SetLength(FResultBank, FRegionCount);
  SetLength(FTypeIdx, FRegionCount);
  SetLength(FFuncIdx, FRegionCount);
  SetLength(FCalls, FRegionCount);
  SetLength(FRecursive, FRegionCount);
  SetLength(FUsesGlobal, FRegionCount);
  for i := 0 to FRegionCount - 1 do
  begin
    for k := 0 to Ord(High(TSSARegisterType)) do
    begin
      FParamCount[i][TSSARegisterType(k)] := 0;
      FSlotCount[i][TSSARegisterType(k)] := 0;
    end;
    FResultBank[i] := -1;
    FRecursive[i] := False;
    FUsesGlobal[i] := False;
  end;
  Result := True;
end;

{ ---------------- 2. registers: local, or global if shared ---------------- }

procedure TWasmBackend.NoteRegister(const V: TSSAValue; Region: Integer);
var
  RT: TSSARegisterType;
begin
  if V.Kind <> svkRegister then Exit;
  RT := V.RegType;
  if V.RegIndex > FMaxReg[RT] then
  begin
    FMaxReg[RT] := V.RegIndex;
    SetLength(FUseRegion[RT], FMaxReg[RT] + 1);
  end;
  while Length(FUseRegion[RT]) <= V.RegIndex do
    SetLength(FUseRegion[RT], Length(FUseRegion[RT]) + 1);
  if FUseRegion[RT][V.RegIndex] = 0 then
    FUseRegion[RT][V.RegIndex] := Region + 1        // 0 = untouched, so bias by one
  else if FUseRegion[RT][V.RegIndex] <> Region + 1 then
    FUseRegion[RT][V.RegIndex] := -1;               // more than one region
end;

procedure TWasmBackend.Classify32BitRegisters;
{ Which integer registers hold ONLY 32-bit values, and with which signedness.
  A register qualifies when every definition of it is ssaNarrowInt with width 5 or 6, all the same -
  exactly what INT32 / UINT32 arithmetic emits for its operands and results, and what a store to a
  LONG / ULONG emits. One definition of any other kind disqualifies it.
  ⛔ Globals are excluded: a global is how two regions share a value, and narrowing the storage they
  share is a change of meaning rather than of representation. }
var
  b, j, f, W: Integer;
  Blk: TSSABasicBlock;
  Ins: TSSAInstruction;
  Seen: array of Boolean;      // has any definition at all
  Bad: array of Boolean;       // has a definition that is not a 32-bit narrowing
  Sign: array of Integer;      // 0 = none yet, 5 or 6 = the width seen so far, -1 = mixed
  NativeUse, OtherUse: array of Integer;

  { Can this consumer take the operand as an i32 without widening it first? The list is exactly the
    lowerings that have a 32-bit form - keep the two in step or the count lies. }
  procedure CountUse(Ins: TSSAInstruction; const V: TSSAValue);
  var u: Integer;
  begin
    u := FlatId(V);
    if (u < 0) or (V.RegType <> srtInt) then Exit;
    if OpIn(Ins.OpCode, [ssaNarrowInt, ssaIntToFloat, ssaAddInt, ssaSubInt, ssaMulInt,
                         ssaBitwiseAnd, ssaBitwiseOr, ssaBitwiseXor, ssaNegInt,
                         ssaRecordStoreInt]) then
      Inc(NativeUse[u])
    else
      Inc(OtherUse[u]);
  end;

begin
  SetLength(FIs32, FFlatCount);
  SetLength(FIs32Signed, FFlatCount);
  SetLength(Seen, FFlatCount);
  SetLength(Bad, FFlatCount);
  SetLength(Sign, FFlatCount);
  for b := 0 to FProg.Blocks.Count - 1 do
  begin
    Blk := FProg.Blocks[b];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Ins := TSSAInstruction(Blk.Instructions[j]);
      f := FlatId(Ins.Dest);
      if (f < 0) or (Ins.Dest.RegType <> srtInt) then Continue;
      Seen[f] := True;
      if (Ins.OpCode = ssaNarrowInt) and (Ins.Src3.Kind = svkConstInt) then
      begin
        W := Integer(Ins.Src3.ConstInt);
        if (W = 5) or (W = 6) then
        begin
          if Sign[f] = 0 then Sign[f] := W
          else if Sign[f] <> W then Sign[f] := -1;
          Continue;
        end;
      end;
      Bad[f] := True;
    end;
  end;
  { ⛔ Eligible is not the same as WORTH IT. Every use by an instruction with no 32-bit form pays a
    widening, and a register narrowed only because it is stored into a LONG - which has nothing to do
    with 32-bit arithmetic - is read almost entirely by such instructions. Left unchecked that made a
    64-bit hash loop SIX BYTES LONGER while the 32-bit one gained two: eligibility said yes where the
    byte count said no. So the uses are counted, and the classification is taken only when at least
    half of them can consume an i32 directly. }
  SetLength(NativeUse, FFlatCount);
  SetLength(OtherUse, FFlatCount);
  for b := 0 to FProg.Blocks.Count - 1 do
  begin
    Blk := FProg.Blocks[b];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Ins := TSSAInstruction(Blk.Instructions[j]);
      CountUse(Ins, Ins.Src1);
      CountUse(Ins, Ins.Src2);
      CountUse(Ins, Ins.Src3);
    end;
  end;
  for f := 0 to FFlatCount - 1 do
  begin
    FIs32[f] := Seen[f] and (not Bad[f]) and (not FIsGlobal[f]) and (Sign[f] > 0)
                and (NativeUse[f] >= OtherUse[f]);
    FIs32Signed[f] := Sign[f] = 5;
  end;
end;

function TWasmBackend.FlatId(const V: TSSAValue): Integer;
begin
  if V.Kind <> svkRegister then Exit(-1);
  Result := FBankBase[V.RegType] + V.RegIndex;
end;

procedure TWasmBackend.ComputeUpExposed;
{ Which registers carry a value INTO a region, i.e. are read on some path before
  being written there. That is the question that separates "two procedures share
  a value" from "the allocator reused a number for two unrelated temporaries" -
  and getting it wrong the conservative way refuses perfectly good programs,
  which is what asking only "does more than one region mention it" did.

  Standard live-in, restricted to the region: Gen = read before written in the
  block, Kill = written in the block, LiveIn = Gen + (LiveOut - Kill). Successors
  outside the region are dropped, which is sound here because jumps never cross a
  region (BuildPartition refuses that) - the only cross-region edge is a call,
  and a call's effect on the callee's registers is what the classification is
  deciding, not something to propagate through. }
var
  r, i, j, k, b, s, f, N, First, Last, Idx: Integer;
  Blk, Succ: TSSABasicBlock;
  Instr: TSSAInstruction;
  Extras: TSSAValueArray;
  Gen, Kill, LiveIn, LiveOut: array of array of Boolean;
  Changed: Boolean;

  procedure MarkUse(const V: TSSAValue; Blk: Integer);
  var fid: Integer;
  begin
    fid := FlatId(V);
    if (fid >= 0) and (not Kill[Blk][fid]) then Gen[Blk][fid] := True;
  end;

begin
  N := FProg.Blocks.Count;
  SetLength(Gen, N); SetLength(Kill, N); SetLength(LiveIn, N); SetLength(LiveOut, N);
  for i := 0 to N - 1 do
  begin
    SetLength(Gen[i], FFlatCount);   SetLength(Kill[i], FFlatCount);
    SetLength(LiveIn[i], FFlatCount); SetLength(LiveOut[i], FFlatCount);
    for j := 0 to FFlatCount - 1 do
    begin
      Gen[i][j] := False; Kill[i][j] := False;
      LiveIn[i][j] := False; LiveOut[i][j] := False;
    end;
  end;

  for i := 0 to N - 1 do
  begin
    Blk := FProg.Blocks[i];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := TSSAInstruction(Blk.Instructions[j]);
      MarkUse(Instr.Src1, i);
      MarkUse(Instr.Src2, i);
      MarkUse(Instr.Src3, i);
      Extras := ExtraOperands(Instr);
      for k := 0 to High(Extras) do MarkUse(Extras[k], i);
      { ⛔ ssaArrayStore's Dest is the VALUE BEING STORED - a read, not a
        definition (the VM writes IntData[idx] := IntRegs[Instr.Dest]). Killing
        it here would say "this register is written before it is read", so a
        value that really does cross into a procedure would be judged not
        live-in, never promoted to a global, and read as whatever that region's
        own local happened to hold. Silent, and only in programs with both an
        array store and a shared register. }
      if Instr.OpCode = ssaArrayStore then
        MarkUse(Instr.Dest, i)
      else
      begin
        Idx := FlatId(Instr.Dest);
        if Idx >= 0 then Kill[i][Idx] := True;
      end;
    end;
  end;

  repeat
    Changed := False;
    for i := N - 1 downto 0 do
    begin
      Blk := FProg.Blocks[i];
      for s := 0 to Blk.Successors.Count - 1 do
      begin
        Succ := TSSABasicBlock(Blk.Successors[s]);
        b := -1;
        for j := 0 to N - 1 do
          if FProg.Blocks[j] = Succ then begin b := j; Break; end;
        if (b < 0) or (FRegionOf[b] <> FRegionOf[i]) then Continue;
        for f := 0 to FFlatCount - 1 do
          if LiveIn[b][f] and not LiveOut[i][f] then
          begin
            LiveOut[i][f] := True;
            Changed := True;
          end;
      end;
      for f := 0 to FFlatCount - 1 do
        if (Gen[i][f] or (LiveOut[i][f] and not Kill[i][f])) and not LiveIn[i][f] then
        begin
          LiveIn[i][f] := True;
          Changed := True;
        end;
    end;
  until not Changed;

  SetLength(FUpExposed, FRegionCount);
  for r := 0 to FRegionCount - 1 do
  begin
    SetLength(FUpExposed[r], FFlatCount);
    First := FRegionFirst[r];
    Last := FRegionLast[r];
    for f := 0 to FFlatCount - 1 do FUpExposed[r][f] := LiveIn[First][f];
    // A block inside the region that no in-region edge reaches (a procedure's
    // epilogue reached only by a return path, say) would otherwise hide its own
    // live-in, so fold every block whose in-region predecessors are none.
    for i := First to Last do
      if i <> First then
      begin
        Blk := FProg.Blocks[i];
        b := 0;
        for s := 0 to Blk.Predecessors.Count - 1 do
        begin
          Succ := TSSABasicBlock(Blk.Predecessors[s]);
          for j := First to Last do
            if FProg.Blocks[j] = Succ then begin Inc(b); Break; end;
        end;
        if b = 0 then
          for f := 0 to FFlatCount - 1 do
            if LiveIn[i][f] then FUpExposed[r][f] := True;
      end;
  end;
end;

function TWasmBackend.ClassifyRegisters: Boolean;
var
  i, j, k, r: Integer;
  RT: TSSARegisterType;
  Blk: TSSABasicBlock;
  Instr: TSSAInstruction;
  Extras: TSSAValueArray;
  Init: TWasmBuf;
  Carried: Boolean;
begin
  for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
  begin
    FMaxReg[RT] := -1;
    SetLength(FUseRegion[RT], 0);
  end;

  for i := 0 to FProg.Blocks.Count - 1 do
  begin
    Blk := FProg.Blocks[i];
    r := FRegionOf[i];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := TSSAInstruction(Blk.Instructions[j]);
      NoteRegister(Instr.Dest, r);
      NoteRegister(Instr.Src1, r);
      NoteRegister(Instr.Src2, r);
      NoteRegister(Instr.Src3, r);
      Extras := ExtraOperands(Instr);
      for k := 0 to High(Extras) do NoteRegister(Extras[k], r);
    end;
  end;

  // Flat register ids, so liveness and the per-region maps can use plain arrays.
  FFlatCount := 0;
  for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
  begin
    FBankBase[RT] := FFlatCount;
    Inc(FFlatCount, FMaxReg[RT] + 1);
  end;
  ComputeUpExposed;

  SetLength(FRegionUses, FRegionCount);
  for r := 0 to FRegionCount - 1 do
  begin
    SetLength(FRegionUses[r], FFlatCount);
    for i := 0 to FFlatCount - 1 do FRegionUses[r][i] := False;
  end;
  for i := 0 to FProg.Blocks.Count - 1 do
  begin
    Blk := FProg.Blocks[i];
    r := FRegionOf[i];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := TSSAInstruction(Blk.Instructions[j]);
      if FlatId(Instr.Dest) >= 0 then FRegionUses[r][FlatId(Instr.Dest)] := True;
      if FlatId(Instr.Src1) >= 0 then FRegionUses[r][FlatId(Instr.Src1)] := True;
      if FlatId(Instr.Src2) >= 0 then FRegionUses[r][FlatId(Instr.Src2)] := True;
      if FlatId(Instr.Src3) >= 0 then FRegionUses[r][FlatId(Instr.Src3)] := True;
      Extras := ExtraOperands(Instr);
      for k := 0 to High(Extras) do
        if FlatId(Extras[k]) >= 0 then FRegionUses[r][FlatId(Extras[k])] := True;
    end;
  end;

  // A register mentioned by two regions is NOT necessarily a value that crosses:
  // the allocator reuses a number for unrelated values with disjoint live ranges,
  // and in that case each region can keep its own local. What forces a global is
  // a register that carries a value IN - read on some path before being written.
  // Asking the coarse question instead refused programs that are perfectly fine,
  // starting with any recursive procedure.
  FGlobalCount := 0;
  SetLength(FIsGlobal, FFlatCount);
  SetLength(FGlobalIdx, FFlatCount);
  for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
    for i := 0 to FMaxReg[RT] do
    begin
      j := FBankBase[RT] + i;
      FIsGlobal[j] := False;
      FGlobalIdx[j] := 0;
      Carried := False;
      if FUseRegion[RT][i] = -1 then
        for r := 0 to FRegionCount - 1 do
          if FRegionUses[r][j] and FUpExposed[r][j] then Carried := True;
      if Carried then
      begin
        Init := TWasmBuf.Create;
        try
          case RT of
            srtFloat:  Init.F64Const(0);
            srtString: Init.I32Const(0);
          else
            Init.I64Const(0);
          end;
          FIsGlobal[j] := True;
          FGlobalIdx[j] := FModule.DefineGlobal(BankType[RT], True, Init);
        finally
          Init.Free;
        end;
        Inc(FGlobalCount);
      end;
    end;

  for r := 0 to FRegionCount - 1 do
    for j := 0 to FFlatCount - 1 do
      if FRegionUses[r][j] and FIsGlobal[j] then FUsesGlobal[r] := True;
  Result := True;
end;

function TWasmBackend.IsSharedSlot(ASlot: Integer): Boolean;
{ Every slot at or above the base EXCEPT the result. 255 is a different animal:
  it is produced and consumed across ONE call boundary, so a per-region local
  plus the function's WASM result already models it, and turning it into a
  global would serialise what multi-value returns keep separate. }
begin
  Result := (ASlot >= WASM_SHARED_SLOT_BASE) and (ASlot <> WASM_XFER_RESULT_SLOT);
end;

procedure TWasmBackend.NoteXferGlobal(Bank: TSSARegisterType; ASlot: Integer);
{ One mutable WASM global per (bank, slot), defined the first time the slot is
  seen. Zero is the right initial value in all three banks: the VM's transfer
  bank starts cleared, an unwritten float reads 0.0, and string handle 0 IS the
  empty string (memory is zeroed, so it reads as length 0). }
var
  n: Integer;
  Init: TWasmBuf;
begin
  n := Length(FXferIsGlobal[Bank]);
  if n <= ASlot then
  begin
    SetLength(FXferIsGlobal[Bank], ASlot + 1);
    SetLength(FXferGlobal[Bank], ASlot + 1);
    while n <= ASlot do
    begin
      FXferIsGlobal[Bank][n] := False;
      FXferGlobal[Bank][n] := 0;
      Inc(n);
    end;
  end;
  if FXferIsGlobal[Bank][ASlot] then Exit;
  Init := TWasmBuf.Create;
  try
    case Bank of
      srtFloat:  Init.F64Const(0);
      srtString: Init.I32Const(0);
    else
      Init.I64Const(0);
    end;
    FXferGlobal[Bank][ASlot] := FModule.DefineGlobal(BankType[Bank], True, Init);
    FXferIsGlobal[Bank][ASlot] := True;
  finally
    Init.Free;
  end;
end;

procedure TWasmBackend.ScanForHalt;
{ Does any region OTHER than main halt the program? Main's END is already a
  return that ends the run, so the flag is worth nothing there. }
var
  i, j: Integer;
  Blk: TSSABasicBlock;
begin
  FUsesHalt := False;
  for i := 0 to FProg.Blocks.Count - 1 do
  begin
    if FRegionOf[i] = 0 then Continue;
    Blk := FProg.Blocks[i];
    for j := 0 to Blk.Instructions.Count - 1 do
      if OpIn(TSSAInstruction(Blk.Instructions[j]).OpCode, [ssaEnd, ssaStop]) then
      begin
        FUsesHalt := True;
        Exit;
      end;
  end;
end;

procedure TWasmBackend.EmitReturnValues(B: TWasmBuf; R: Integer);
{ What this region has to leave on the stack to return: its own value first,
  then the byref slots it copies back. }
begin
  if FResultBank[R] >= 0 then
    B.LocalGet(FResultTmp[TSSARegisterType(FResultBank[R])]);
  PushOutSlots(B, R);
end;

{ ---------------- 3. signatures, read off the transfer slots ---------------- }

function TWasmBackend.BuildSignatures: Boolean;
var
  i, j, r, Slot, Target, TR: Integer;
  RT: TSSARegisterType;
  Blk: TSSABasicBlock;
  Instr: TSSAInstruction;
  Params: TWasmValTypeArray;
  Res: TWasmValTypeArray;
  n, p: Integer;
  TargetInd: Boolean;
  IndParams, IndRes: TWasmValTypeArray;

  procedure Widen(Region: Integer; Bank: TSSARegisterType; ASlot: Integer);
  begin
    if ASlot = WASM_XFER_RESULT_SLOT then Exit;
    if ASlot + 1 > FParamCount[Region][Bank] then FParamCount[Region][Bank] := ASlot + 1;
  end;

  procedure NoteSlot(Region: Integer; Bank: TSSARegisterType; ASlot: Integer);
  begin
    if ASlot = WASM_XFER_RESULT_SLOT then Exit;
    if ASlot + 1 > FSlotCount[Region][Bank] then FSlotCount[Region][Bank] := ASlot + 1;
  end;

  function SlotOf(Instr: TSSAInstruction; out ASlot: Integer): Boolean;
  begin
    Result := Instr.Src3.Kind = svkConstInt;
    if Result then ASlot := Integer(Instr.Src3.ConstInt) else ASlot := -1;
  end;

  function XferBank(Op: TSSAOpCode; out Bank: TSSARegisterType): Boolean;
  begin
    Result := True;
    case Op of
      ssaXferStoreInt, ssaXferLoadInt:       Bank := srtInt;
      ssaXferStoreFloat, ssaXferLoadFloat:   Bank := srtFloat;
      ssaXferStoreString, ssaXferLoadString: Bank := srtString;
    else
      Bank := srtInt; Result := False;
    end;
  end;

begin
  for RT := Low(TSSARegisterType) to High(TSSARegisterType) do FIndParam[RT] := 0;
  // The callee's loads and the caller's stores both name (bank, slot), so the
  // signature is the union of the two - a parameter the body never reads still
  // has to be in the type, or the call site would not match.
  for i := 0 to FProg.Blocks.Count - 1 do
  begin
    Blk := FProg.Blocks[i];
    r := FRegionOf[i];
    Target := -1;
    TargetInd := False;
    for j := Blk.Instructions.Count - 1 downto 0 do
    begin
      Instr := TSSAInstruction(Blk.Instructions[j]);
      if (Instr.OpCode = ssaCallSub) and (Instr.Dest.Kind = svkLabel) then
      begin
        Target := FRegionOf[BlockOfLabel(Instr.Dest.LabelName)];
        TargetInd := False;
        // record the call-graph edge while we are here
        n := Length(FCalls[r]);
        SetLength(FCalls[r], n + 1);
        FCalls[r][n] := Target;
      end
      else if Instr.OpCode = ssaCallSubIndirect then
      begin
        { ⛔ Target MUST be cleared here. Walking backwards, a store sees the
          call BELOW it - and without this an argument staged for an indirect
          call would have been credited to whatever direct call happened to
          follow, widening a signature that has nothing to do with it. The
          slots go to the shared indirect signature instead. }
        Target := -1;
        TargetInd := True;
      end
      else if XferBank(Instr.OpCode, RT) then
      begin
        if not SlotOf(Instr, Slot) then
          Exit(Fail(Format('a transfer slot is not a constant in block "%s"', [Blk.LabelName])));
        { ⛔ NOT EVERY TRANSFER SLOT IS A PARAMETER, and reading them all as one
          is what made a destructor come out with 254 of them. The bank is
          partitioned by CONVENTION (SedaiSSA): 0..N are real arguments, 128 and
          up (SHARED_SLOT_BASE) are module-global SHARED scalars, 254 carries the
          caller-allocated handle for a FUNCTION returning a UDT by value, and
          255 is the result.
          A high slot mapped as "parameter number 253" produced a signature with
          253 phantom arguments and a module the engine REFUSED TO LOAD - which
          is the one outcome this backend exists to prevent.
          ⚠️ And the honest fix is not to renumber them: a SHARED slot must
          SURVIVE a call, so it belongs in a WASM global, while a slot local is
          per function - every procedure would get its own copy of a module
          global and the program would quietly compute the wrong thing.
          ⇒ That is what NoteXferGlobal builds, and it takes those slots OUT of
          the arity question entirely: a high slot is neither a parameter of the
          region that mentions it nor a local of it. }
        if IsSharedSlot(Slot) then
        begin
          NoteXferGlobal(RT, Slot);
          Continue;
        end;
        NoteSlot(r, RT, Slot);      // this region needs a local for that slot
        if Instr.OpCode in [ssaXferLoadInt, ssaXferLoadFloat, ssaXferLoadString] then
        begin
          { ⛔ The RESULT load is NOT handled here, and that was a real defect:
            this loop runs BACKWARDS so that a staged store finds the call that
            FOLLOWS it - but a result load sits AFTER its call, so going
            backwards it is seen BEFORE, and "Target" is some later call or
            none. A virtual-dispatch thunk came out declared as returning
            NOTHING, its caller stored nothing, and the program printed 0 for an
            area of 50. The forward pass below owns this question. }
          // A non-result LOAD says nothing about this region's arity. SUB
          // inlining leaves the callee's loads behind in its caller, so reading
          // them as "my parameters" gave Hypot(Double, Double) an integer first
          // parameter - the one Keep took. Only the CALL SITES know the arity.
        end
        else
        begin
          if Slot = WASM_XFER_RESULT_SLOT then
            FResultBank[r] := Ord(RT)              // the callee writes its result
          else if TargetInd then
          begin
            if Slot + 1 > FIndParam[RT] then FIndParam[RT] := Slot + 1;
          end
          else if Target >= 0 then
            Widen(Target, RT, Slot);               // staged for the call below
          { A non-result store with no call after it IN THIS BLOCK. Two very
            different things wear that shape, and the backend cannot yet tell
            them apart:
              - staging for a call that sits in ANOTHER block (PROC_GCD stages
                its own recursive call this way, and it compiles correctly);
              - BYREF COPY-OUT, the callee writing a parameter back for its
                caller - which this backend DROPS, because one WASM function per
                procedure means the slot locals are per function and the caller
                never sees the write.
            ⛔ MEASURED: refusing every such store to close the second case also
            refuses the first, and print_calls.bas went from 38 identical lines
            to a refusal. Block-local shape is not the question - whether a call
            is REACHABLE from the store is - which is the same mistake as
            guard-scope-was-one-block-not-dominance.
            ⇒ Left as it was, and the two byref programs stay on the known-diff
            list rather than being bought back with a false refusal. }
        end;
      end;
    end;
  end;

  { A SECOND pass, FORWARDS, for the one question the backward pass cannot
    answer: which function a RESULT load belongs to. A result slot is loaded
    after the call that produced it, so the owner is the last call SEEN, not the
    next one - the opposite direction from argument staging, which is why the
    two cannot share a walk. }
  Target := -1;
  TR := -1;
  for i := 0 to FProg.Blocks.Count - 1 do
  begin
    Blk := FProg.Blocks[i];
    { ⚠️ The window is WIDER than a block. A call and the load that reads its
      result - or its byref writeback - are routinely in DIFFERENT blocks,
      because the SSA splits at the call; resetting per block saw neither, and
      the byref detection below found nothing at all until this changed. It is
      also bounded by the REGION, which is as far as a call's effects reach.
      ⛔ But the region alone is TOO WIDE, and that was a real defect: SUB
      inlining flattens a leaf callee into its caller and leaves the callee's OWN
      transfer traffic behind, so a load belonging to inlined code was credited
      to whatever call happened to come before it. A method returning Integer
      came out declared as returning a STRING - with a phantom byref result on
      top - because a string-returning leaf function was inlined ten instructions
      later. The closing bracket is below: a STORE ends the window. }
    if FRegionOf[i] <> TR then
    begin
      TR := FRegionOf[i];
      Target := -1;
    end;
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := TSSAInstruction(Blk.Instructions[j]);
      if (Instr.OpCode = ssaCallSub) and (Instr.Dest.Kind = svkLabel) then
      begin
        Target := FRegionOf[BlockOfLabel(Instr.Dest.LabelName)];
        TargetInd := False;
      end
      else if Instr.OpCode = ssaCallSubIndirect then
      begin
        // the result read after THIS call belongs to the shared signature, and
        // to no direct callee - the same clearing as in the backward pass
        Target := -1;
        TargetInd := True;
      end
      else if TargetInd and
              OpIn(Instr.OpCode, [ssaXferLoadInt, ssaXferLoadFloat, ssaXferLoadString]) and
              SlotOf(Instr, Slot) and (Slot = WASM_XFER_RESULT_SLOT) then
        { ⛔ SWALLOWED ON PURPOSE, and the branch has to exist even though it
          does nothing: the result of an INDIRECT call belongs to no region, and
          without this arm it would fall through to the ones below and be
          credited to whichever direct callee came before. The bank it arrives
          in is decided per call site, by the load itself, not by a signature. }
      else if OpIn(Instr.OpCode, [ssaXferLoadInt, ssaXferLoadFloat, ssaXferLoadString]) and
              XferBank(Instr.OpCode, RT) and SlotOf(Instr, Slot) and
              (Slot <> WASM_XFER_RESULT_SLOT) and (not IsSharedSlot(Slot)) and
              (Target >= 0) then
      begin
        { ⚠️ A HIGH slot is excluded above, and it is not a detail: re-reading a
          SHARED global after a call looks exactly like byref copy-out to this
          criterion, and buying it back as an extra RESULT would have given the
          callee a phantom parameter for a variable it never received. The
          global is already visible on both sides - there is nothing to copy.
          ⭐ The CALLER re-reading an ARGUMENT slot after a call is BYREF
          copy-out, and it is the only unambiguous witness of it. A store to an
          argument slot inside a procedure cannot tell copy-out from the
          procedure staging its OWN call - that ambiguity already produced one
          wrong refusal - but a caller reading a slot back after a call is
          exactly when the write becomes observable.
          ⚠️ A false positive here is HARMLESS, which is what makes the criterion
          usable: the callee's slot local is initialised from the parameter, so
          a slot it never writes comes back unchanged and the caller stores the
          same value it sent. }
        while Length(FOutSlot[Target][RT]) <= Slot do
          SetLength(FOutSlot[Target][RT], Length(FOutSlot[Target][RT]) + 1);
        FOutSlot[Target][RT][Slot] := True;
        NoteSlot(Target, RT, Slot);        // the callee needs a local for it
        Widen(Target, RT, Slot);           // and it is a parameter, by definition
      end
      else if OpIn(Instr.OpCode, [ssaXferLoadInt, ssaXferLoadFloat, ssaXferLoadString]) and
              XferBank(Instr.OpCode, RT) and SlotOf(Instr, Slot) and
              (Slot = WASM_XFER_RESULT_SLOT) and (Target >= 0) then
        { The caller reads the result of the call it just made, so that call's
          region returns this bank. It is the CALLER that knows - a forwarding
          thunk never stores the result slot itself, so nothing inside it says
          what it returns. }
        FResultBank[Target] := Ord(RT)
      else if OpIn(Instr.OpCode, [ssaXferStoreInt, ssaXferStoreFloat, ssaXferStoreString]) then
        { ⭐ A STORE CLOSES THE WINDOW, and that is the whole fix. A call's
          effects are read out BEFORE anything else is written into the transfer
          bank - the result load and any byref writeback come first, then the
          caller stages its next call. So a store means the previous call has
          been read to the end, and anything after it belongs to somebody else:
          in practice, to a leaf procedure that SUB inlining flattened in here.
          ⚠️ Deliberately ANY store, result slot included: inlined code writes
          slot 255 too, and a region that has just written its own result is not
          reading a call's either. }
      begin
        Target := -1;
        TargetInd := False;
      end;
    end;
  end;

  { ---- ONE type for everything the table can hold ---------------------------

    A call_indirect names a TYPE, so every entry the table can reach has to have
    the same one - and the first attempt, "give every address-taken procedure
    one signature", DIED ON A REAL PROGRAM: m217_funcptr composes an
    Integer->Integer, a (Double,Double)->Double and a String->String through
    pointers in the same file. There is no single WASM signature for those.

    ⭐ The way out was already in the design: the RESULT does not have to be a
    WASM result. The interpreter's transfer bank is global storage, slot 255
    included, so a callee writing its result into a GLOBAL is the faithful
    model and not a workaround. That takes the return type out of the signature
    entirely, and what is left - the parameters - CAN be unified by padding,
    because slots are positional per bank and an extra one is simply ignored.

    ⇒ Every address-taken procedure gets the union of the parameter counts, and
    a THUNK (emitted later) with the shared type: it passes on the parameters
    its own callee declares and moves the result into the bank's global. The
    caller reads the global of the bank IT expects, which is the bank the BASIC
    type system already agreed on.

    ⛔ BYREF copy-out is still refused: it leaves as extra RESULTS, so it is part
    of the type, and an indirect call site cannot know which slots to pop. }
  if FIndirect then
  begin
    for r := 0 to FRegionCount - 1 do
      if FAddrTaken[r] then
      begin
        for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
          if FParamCount[r][RT] > FIndParam[RT] then FIndParam[RT] := FParamCount[r][RT];
        for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
          for n := 0 to Length(FOutSlot[r][RT]) - 1 do
            if FOutSlot[r][RT][n] then
              Exit(Fail(Format('procedure "%s" is used as a function pointer and writes a ' +
                               'parameter back to its caller; a BYREF result cannot travel ' +
                               'through an indirect call here', [FRegionName[r]])));
      end;
    { ⛔ The union is applied to the PROCEDURES too, and it is not cosmetic: a
      procedure called ONLY through a pointer has no ssaCallSub naming it, so
      Widen never saw it and its arity would be ZERO - the thunk would pass
      nothing and the body would read its parameters as zeros, in silence. }
    for r := 1 to FRegionCount - 1 do
      if FAddrTaken[r] then
        for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
        begin
          FParamCount[r][RT] := FIndParam[RT];
          if FIndParam[RT] > FSlotCount[r][RT] then FSlotCount[r][RT] := FIndParam[RT];
        end;
  end;

  for r := 0 to FRegionCount - 1 do
  begin
    SetLength(Params, 0);
    p := 0;
    for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
    begin
      for n := 0 to FParamCount[r][RT] - 1 do
      begin
        SetLength(Params, p + 1);
        Params[p] := BankType[RT];
        Inc(p);
      end;
    end;
    if r = 0 then
    begin
      // The module entry takes nothing and returns nothing. The parameter COUNTS
      // have to be cleared too, not just the signature: EmitRegion lays the
      // locals out behind them, so leaving them set shifts every local index in
      // main by however many arguments some call happened to stage.
      SetLength(Params, 0);
      for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
        FParamCount[0][RT] := 0;
      FResultBank[0] := -1;
    end;
    { The results are the function's own value FIRST, then every argument slot it
      copies back. The order is fixed and both sides walk it the same way: the
      caller pops in reverse, so the result - pushed first, deepest - is the last
      thing it stores, which is where the single-result code already put it. }
    SetLength(Res, 0);
    if FResultBank[r] >= 0 then
    begin
      SetLength(Res, 1);
      Res[0] := BankType[TSSARegisterType(FResultBank[r])];
    end;
    if r <> 0 then
      for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
        for n := 0 to Length(FOutSlot[r][RT]) - 1 do
          if FOutSlot[r][RT][n] then
          begin
            SetLength(Res, Length(Res) + 1);
            Res[High(Res)] := BankType[RT];
          end;
    FTypeIdx[r] := FModule.TypeIndex(Params, Res);
    { WASM_DIAG=1 prints the signature the backend DERIVED for each region. The
      convention is read out of the transfer bank rather than declared, so when
      a module fails to validate on a local type this is the first thing to
      look at - and reading it settles in one line what guessing does not. }
    if GetEnvironmentVariable('WASM_DIAG') = '1' then
    begin
      WriteLn(ErrOutput, Format('WASMDIAG region %d "%s": params int=%d float=%d string=%d, result=%d',
        [r, FRegionName[r], FParamCount[r][srtInt], FParamCount[r][srtFloat],
         FParamCount[r][srtString], FResultBank[r]]));
      for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
        for n := 0 to Length(FOutSlot[r][RT]) - 1 do
          if FOutSlot[r][RT][n] then
            WriteLn(ErrOutput, Format('WASMDIAG    byref out: bank %d slot %d', [Ord(RT), n]));
    end;
  end;

  { The shared type: the union of the parameters, in the same bank order every
    signature uses, and NO result - the result travels in a global. }
  if FIndirect then
  begin
    SetLength(IndParams, 0);
    p := 0;
    for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
      for n := 0 to FIndParam[RT] - 1 do
      begin
        SetLength(IndParams, p + 1);
        IndParams[p] := BankType[RT];
        Inc(p);
      end;
    SetLength(IndRes, 0);
    FIndTypeIdx := FModule.TypeIndex(IndParams, IndRes);
    // which banks a pointer call can bring a result back in
    for r := 1 to FRegionCount - 1 do
      if FAddrTaken[r] and (FResultBank[r] >= 0) then
        FIndResUsed[TSSARegisterType(FResultBank[r])] := True;
  end;
  Result := True;
end;

function TWasmBackend.DetectRecursion: Boolean;
var
  r, i, k, Changed: Integer;
  Reach: array of array of Boolean;
begin
  SetLength(Reach, FRegionCount);
  for r := 0 to FRegionCount - 1 do
  begin
    SetLength(Reach[r], FRegionCount);
    for i := 0 to FRegionCount - 1 do Reach[r][i] := False;
    for i := 0 to High(FCalls[r]) do Reach[r][FCalls[r][i]] := True;
  end;
  repeat
    Changed := 0;
    for r := 0 to FRegionCount - 1 do
      for i := 0 to FRegionCount - 1 do
        if Reach[r][i] then
          for k := 0 to FRegionCount - 1 do
            if Reach[i][k] and not Reach[r][k] then
            begin
              Reach[r][k] := True;
              Inc(Changed);
            end;
  until Changed = 0;

  for r := 0 to FRegionCount - 1 do
    FRecursive[r] := Reach[r][r];

  // A global is one storage location for the whole program, so a recursive
  // procedure holding a value there would have its own activation clobbered by
  // the next one. That is exactly the case the VM answers with a copying frame,
  // and there is no deopt here to fall back on - refuse, and say which.
  for r := 0 to FRegionCount - 1 do
    if FRecursive[r] and FUsesGlobal[r] then
      Exit(Fail(Format('procedure "%s" is recursive AND touches a register shared with ' +
                       'another procedure, which has to become a WASM global - one storage ' +
                       'location cannot hold two activations', [FRegionName[r]])));
  Result := True;
end;

{ ---------------- 4. lowering ---------------- }

procedure TWasmBackend.EmitTruncF32(B: TWasmBuf; Instr: TSSAInstruction);
{ The implicit float -> int conversion of a value that is on the stack as an f32 and must NOT be
  promoted first. Same rule and same guard as the f64 form - MODERN rounds to nearest, out of range
  gives x86's "integer indefinite" rather than trapping - written against binary32 comparisons so the
  f32 never has to become an f64 on the way.
  ⛔ The guard is not decoration: i32/i64.trunc_f32 TRAP outside their range, and reproducing the
  interpreter means answering the indefinite instead. Every input that would trap is excluded by the
  comparison in front of it, so the trap is unreachable by construction - the same argument that
  brought the f64 forms back on merit. }
var
  Dst32: Boolean;
begin
  if FModern then B.Op(wopF32Nearest);
  B.LocalTee(FF32Tmp);
  Dst32 := RegIs32(Instr.Dest);
  if Dst32 then
  begin
    B.F32Const(-2147483649.0);
    B.Op(wopF32Gt);
    B.LocalGet(FF32Tmp);
    B.F32Const(4294967296.0);
    B.Op(wopF32Lt);
    B.Op(wopI32And);
    B.BlockStart(wopIf, WASM_TYPE_I32);
      B.LocalGet(FF32Tmp);
      B.F32Const(0.0);
      B.Op(wopF32Lt);
      B.BlockStart(wopIf, WASM_TYPE_I32);
        B.LocalGet(FF32Tmp); B.Op(wopI32TruncF32S);
      B.Op(wopElse);
        B.LocalGet(FF32Tmp); B.Op(wopI32TruncF32U);
      B.EndOp;
    B.Op(wopElse);
      B.I32Const(Integer($80000000));
    B.EndOp;
    B.LocalSet(FLocalIdx[FCurRegion][FlatId(Instr.Dest)]);
    Exit;
  end;
  B.F32Const(-9223372036854775808.0);
  B.Op(wopF32Ge);
  B.LocalGet(FF32Tmp);
  B.F32Const(9223372036854775808.0);
  B.Op(wopF32Lt);
  B.Op(wopI32And);
  B.BlockStart(wopIf, WASM_TYPE_I64);
    B.LocalGet(FF32Tmp);
    if (Instr.Src3.Kind = svkConstInt) and (Instr.Src3.ConstInt = 1) then
      B.Op(wopI64TruncF32U)          // unsigned destination, and the guard keeps it in range
    else
      B.Op(wopI64TruncF32S);
  B.Op(wopElse);
    B.I64Const(Int64($8000000000000000));
  B.EndOp;
  StoreReg(B, Instr.Dest);
end;

function PlatformRefusal(Op: TSSAOpCode): string;
{ Why WebAssembly will NEVER have this, as opposed to why we have not written it yet. The distinction
  is the whole point: a module runs in a sandbox that deliberately has no filesystem, no environment
  and no clock to set, and no threads without shared memory. Those are not gaps in this backend - they
  are the platform, and saying "not covered YET" about them promises something that will not come.
  ⛔ Nothing here is ever to be EMULATED. A virtual filesystem would be a different language that
  happens to compile, which is exactly what the goal of this backend excludes. }
begin
  case Op of
    ssaOpenFunc, ssaFileQuery, ssaFileExists, ssaFileLen, ssaFileDateTime,
    ssaChdir, ssaMkdir, ssaRmdir, ssaScratch, ssaCopyFile, ssaRenameFile, ssaCurDir:
      Result := 'WebAssembly has no filesystem: a module cannot open, inspect or change files';
    ssaEnviron, ssaExePath, ssaShell:
      Result := 'WebAssembly has no operating system underneath: no environment, no path, no shell';
    ssaSetClock:
      Result := 'a WebAssembly module cannot set the host clock';
    ssaThreadCreate, ssaThreadWait, ssaThreadDetach, ssaThreadSelf,
    ssaMutexCreate, ssaMutexLock, ssaMutexUnlock, ssaMutexDestroy,
    ssaCondCreate, ssaCondWait, ssaCondSignal, ssaCondBroadcast, ssaCondDestroy:
      Result := 'WebAssembly has no threads without shared memory, which this module does not use';
  else
    Result := '';
  end;
end;

function TWasmBackend.RegionNeedsF32Tmp(R: Integer): Boolean;
{ Does any block in this region hold the shape the trunc_f32 fusion consumes - a narrowing to binary32
  immediately converted to an integer? Asked BEFORE the locals are laid out, because a scratch local
  declared in every region and used by almost none is not free: it cost the voxel demo 14 bytes across
  its six functions, which is exactly the kind of "it is only two bytes" that this backend is not
  allowed to hand out. }
var
  b, j: Integer;
  Blk: TSSABasicBlock;
  A, C: TSSAInstruction;
begin
  Result := False;
  for b := FRegionFirst[R] to FRegionLast[R] do
  begin
    Blk := FProg.Blocks[b];
    for j := 0 to Blk.Instructions.Count - 2 do
    begin
      A := TSSAInstruction(Blk.Instructions[j]);
      if A.OpCode <> ssaNarrowSingle then Continue;
      C := TSSAInstruction(Blk.Instructions[j + 1]);
      if (C.OpCode = ssaFloatToInt) and (C.Src1.Kind = svkRegister) and
         (A.Dest.Kind = svkRegister) and (C.Src1.RegIndex = A.Dest.RegIndex) then
        Exit(True);
    end;
  end;
end;

function TWasmBackend.RegIs32(const V: TSSAValue): Boolean;
{ Does this operand live in an i32 local? Asked by the lowerings that have a 32-bit form available. }
var f: Integer;
begin
  f := FlatId(V);
  Result := (f >= 0) and (f < Length(FIs32)) and (not FIsGlobal[f]) and FIs32[f];
end;

procedure TWasmBackend.LoadReg(B: TWasmBuf; const V: TSSAValue);
{ Always leaves the bank's own type on the stack - i64 for an integer register - whatever the local
  actually is. That is what keeps every existing call site correct without being touched: a register
  living in an i32 local is widened back here, and only the paths that WANT an i32 (LoadRegI32) skip
  the widening. Correctness never depends on the classification; only the byte count does. }
var
  f: Integer;
begin
  f := FlatId(V);
  if FIsGlobal[f] then begin B.GlobalGet(FGlobalIdx[f]); Exit; end;
  B.LocalGet(FLocalIdx[FCurRegion][f]);
  if FIs32[f] then
    if FIs32Signed[f] then B.Op(wopI64ExtendI32S) else B.Op(wopI64ExtendI32U);
end;

procedure TWasmBackend.LoadRegI32(B: TWasmBuf; const V: TSSAValue);
{ The same register as an i32 on the stack: free when it already lives in one, a wrap when it does
  not. This is the half of the saving that the classification exists for. }
var
  f: Integer;
begin
  f := FlatId(V);
  if (f >= 0) and (not FIsGlobal[f]) and FIs32[f] then
  begin
    B.LocalGet(FLocalIdx[FCurRegion][f]);
    Exit;
  end;
  LoadReg(B, V);
  B.Op(wopI32WrapI64);
end;

function TWasmBackend.LoadInt32(B: TWasmBuf; const V: TSSAValue): Boolean;
{ An i32 from an operand that may be a REGISTER or an immediate CONSTANT.
  ⛔ The graphics statements are where this matters: "Line (0,0)-(3,3)" leaves
  its coordinates as constants, and LoadReg assumes a register - it read some
  unrelated local, and the module failed to VALIDATE because that local had the
  wrong type. Which is the good outcome: a local of the right type by accident
  would have drawn somewhere else in silence. }
begin
  Result := True;
  case V.Kind of
    svkConstInt:
      B.I32Const(LongInt(V.ConstInt));
    svkRegister:
      begin
        LoadReg(B, V);
        case V.RegType of
          srtInt:    B.Op(wopI32WrapI64);
          srtFloat:  B.TruncSat(wopfcI32TruncSatF64S);   // as the VM truncates
          srtString: Exit(Fail('a graphics operand came from the string bank'));
        end;
      end;
  else
    Exit(Fail('a graphics operand is neither a register nor a constant'));
  end;
end;

procedure TWasmBackend.StoreReg(B: TWasmBuf; const V: TSSAValue);
var
  f: Integer;
begin
  { The mirror of LoadReg: what arrives on the stack is the bank's type, so a register living in an
    i32 local is wrapped on the way in. StoreRegI32 is the form for a value that is ALREADY an i32. }
  f := FlatId(V);
  if FIsGlobal[f] then begin B.GlobalSet(FGlobalIdx[f]); Exit; end;
  if FIs32[f] then B.Op(wopI32WrapI64);
  B.LocalSet(FLocalIdx[FCurRegion][f]);
end;

procedure TWasmBackend.StoreRegI32(B: TWasmBuf; const V: TSSAValue; Signed: Boolean);
{ Store a value that is ALREADY an i32 on the stack. Free when the register lives in an i32 local -
  which, together with LoadRegI32, is what makes a chain of 32-bit operations cost nothing but the
  operations themselves. Otherwise it widens, and Signed says how: the caller knows, because it is the
  width the operation was narrowed to. }
var
  f: Integer;
begin
  f := FlatId(V);
  if (f >= 0) and (not FIsGlobal[f]) and FIs32[f] then
  begin
    B.LocalSet(FLocalIdx[FCurRegion][f]);
    Exit;
  end;
  if Signed then B.Op(wopI64ExtendI32S) else B.Op(wopI64ExtendI32U);
  StoreReg(B, V);
end;

procedure TWasmBackend.BoolToBasic(B: TWasmBuf);
// A WASM comparison yields i32 0/1; BASIC's TRUE is -1 (FTrueValue is built as
// -1 and nothing ever changes it). Widen, then flip the sign.
begin
  B.Op(wopI64ExtendI32S);
  B.I64Const(-1);
  B.Op(wopI64Mul);
end;

{ ---- true f32 arithmetic (the largest missing family in the module's instruction coverage) ----

  A SINGLE-typed operation reaches the backend as four instructions: narrow each operand, operate,
  narrow the result. Emitted literally that is f64 arithmetic wrapped in demote/promote pairs -
  correct, but it never emits one f32 instruction.

  So the operation and the narrowing that follows it become ONE f32 instruction, between a demote of
  each operand and a promote of the result. That equals the unfused form exactly when the operands
  are already binary32 - demoting them is then the identity - and the SSA generator guarantees
  precisely that: it converts both operands of a SINGLE-typed operation, and folds a constant one
  through Single() while it is still a constant. This is not an assumption about values; it is the
  contract the generator keeps.

  ⛔ Guarded on the operation's result having exactly ONE use. Fusing consumes it - the f64 value is
  never materialised - so a second reader would find a stale register. The generator makes it a fresh
  single-use temporary, but an optimisation is free to change that, and counting is cheaper than
  finding out from a wrong answer. }
function TWasmBackend.IntRegUses(First, N, RegIdx: Integer): Integer;
// The integer twin of FloatRegUses, and it exists for the same reason: an operation may be fused with
// the narrowing that consumes it ONLY if that narrowing is the intermediate's only reader. Otherwise
// the unfused value is still needed and skipping its store would lose it.
var i, j, k, Acc: Integer; Blk: TSSABasicBlock; Ins: TSSAInstruction;
  procedure Count(const V: TSSAValue);
  begin
    if (V.Kind = svkRegister) and (V.RegType = srtInt) and (V.RegIndex = RegIdx) then Inc(Acc);
  end;
begin
  Acc := 0;
  for i := First to First + N - 1 do
  begin
    Blk := FProg.Blocks[i];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Ins := TSSAInstruction(Blk.Instructions[j]);
      Count(Ins.Src1); Count(Ins.Src2); Count(Ins.Src3);
      for k := 0 to High(Ins.PhiSources) do Count(Ins.PhiSources[k].Value);
    end;
  end;
  Result := Acc;
end;

function TWasmBackend.FloatRegUses(First, N, RegIdx: Integer): Integer;
var i, j, k, Acc: Integer; Blk: TSSABasicBlock; Ins: TSSAInstruction;
  procedure Count(const V: TSSAValue);
  begin
    if (V.Kind = svkRegister) and (V.RegType = srtFloat) and (V.RegIndex = RegIdx) then Inc(Acc);
  end;
begin
  Acc := 0;
  for i := First to First + N - 1 do
  begin
    Blk := FProg.Blocks[i];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Ins := TSSAInstruction(Blk.Instructions[j]);
      Count(Ins.Src1); Count(Ins.Src2); Count(Ins.Src3);
      for k := 0 to High(Ins.PhiSources) do Count(Ins.PhiSources[k].Value);
    end;
  end;
  Result := Acc;
end;

// The i32 form of an integer opcode, or 0 when there is none. Reached only when the operation and the
// 32-bit narrowing that consumes it are ONE instruction - which is what INT32 / UINT32 arithmetic is.
//
// ⭐ It pays because the value lives in a 64-bit local: the unfused pair costs a load, the 64-bit op,
// a store, then a load, the narrowing and a store. Fused it is two loads with a wrap each, the i32
// op, one widening and one store - fewer instructions, and shorter still for the UNSIGNED narrowing,
// which is otherwise an i64.const plus an i64.and. MEASURED on a hash loop: 12 narrowings out of 37
// instructions, and 708 bytes against 609 for the same loop written with 64-bit arithmetic.
// ⛔ Not every integer opcode belongs here. Division, remainder and the shifts are NOT congruent
// modulo 2^32: their 32-bit answer differs from the low half of the 64-bit one, so fusing them would
// change results rather than shorten them. Only the operations whose low 32 bits do not depend on the
// high ones are listed - which is exactly what makes the fusion free.
function I32FormOf(Op: TSSAOpCode; out Un: Boolean): Byte;
begin
  Un := False;
  case Op of
    ssaAddInt:      Result := wopI32Add;
    ssaSubInt:      Result := wopI32Sub;
    ssaMulInt:      Result := wopI32Mul;
    ssaBitwiseAnd:  Result := wopI32And;
    ssaBitwiseOr:   Result := wopI32Or;
    ssaBitwiseXor:  Result := wopI32Xor;
    ssaNegInt:      begin Result := wopI32Sub; Un := True; end;   // 0 - x, emitted as such below
  else
    Result := 0;
  end;
end;

// The f32 form of a float opcode, or 0 when there is none. Binary ones read two operands, the unary
// ones (Un = True) read one.
function F32FormOf(Op: TSSAOpCode; out Un: Boolean): Byte;
begin
  Un := False;
  case Op of
    ssaAddFloat: Result := wopF32Add;
    ssaSubFloat: Result := wopF32Sub;
    ssaMulFloat: Result := wopF32Mul;
    ssaDivFloat: Result := wopF32Div;
    ssaMathSqr:  begin Result := wopF32Sqrt; Un := True; end;
    ssaMathCeil:     begin Result := wopF32Ceil;    Un := True; end;
    ssaMathRound:    begin Result := wopF32Nearest; Un := True; end;
    ssaMathMin:      Result := wopF32Min;
    ssaMathMax:      Result := wopF32Max;
    ssaMathCopySign: Result := wopF32Copysign;
    ssaNegFloat: begin Result := wopF32Neg;  Un := True; end;
    ssaMathAbs:  begin Result := wopF32Abs;  Un := True; end;
    // INT is floor and FIX is truncate-toward-zero, which is what these two f32 forms are. Rounding
    // an exact binary32 leaves it exact, so the fused and unfused forms agree here for free.
    ssaMathInt:  begin Result := wopF32Floor; Un := True; end;
    ssaMathFix:  begin Result := wopF32Trunc; Un := True; end;
  else
    Result := 0;
  end;
end;

function TWasmBackend.EmitInstr(B: TWasmBuf; Instr: TSSAInstruction; R: Integer): Boolean;

  procedure Bin(Opcode: Byte);
  begin
    LoadReg(B, Instr.Src1);
    LoadReg(B, Instr.Src2);
    B.Op(Opcode);
    StoreReg(B, Instr.Dest);
  end;

  procedure Cmp(Opcode: Byte);
  begin
    LoadReg(B, Instr.Src1);
    LoadReg(B, Instr.Src2);
    B.Op(Opcode);
    BoolToBasic(B);
    StoreReg(B, Instr.Dest);
  end;

  // A comparison whose operands are both binary32 (Src3 = 1, set by the generator). Demoting them is
  // the identity - they ARE binary32 already - so this answers exactly what the f64 form answers, and
  // a consumer that ignores the flag stays correct. That is why the flag is safe to ignore here and
  // was not safe to ignore on the int->float conversion: there, ignoring it changed the answer.
  procedure Cmp32(Opcode: Byte);
  begin
    LoadReg(B, Instr.Src1); B.Op(wopF32DemoteF64);
    LoadReg(B, Instr.Src2); B.Op(wopF32DemoteF64);
    B.Op(Opcode);
    BoolToBasic(B);
    StoreReg(B, Instr.Dest);
  end;
  function CmpIs32: Boolean;
  begin Result := (Instr.Src3.Kind = svkConstInt) and (Instr.Src3.ConstInt = 1); end;

  procedure Un(Opcode: Byte);
  begin
    LoadReg(B, Instr.Src1);
    B.Op(Opcode);
    StoreReg(B, Instr.Dest);
  end;

var
  Loaded32, Stored32: Boolean;   // a narrow field went straight to/from an i32 (see ssaRecordLoadInt)
  IsDyn: Boolean;                // ERASE: does this array get freed, or only cleared
  Slot, d, Bytes, NStr, StrBase: Integer;
  n, k: Integer;                  // the array-bind bookkeeping
  Extras: TSSAValueArray;         // operands past Src1..Src3 (graphics carries them there)
  Enc: Int64;
  Ofs: LongWord;
  RT: TSSARegisterType;
  Info: TSSAArrayInfo;
  Desc: LongWord;
begin
  Result := True;
  case Instr.OpCode of
    ssaLabel: ;                             // a label is a position, not code
    ssaNop: B.Op(wopNop);                   // and a no-op is exactly what WASM's nop is
    ssaPrintEnd: ;                          // resets C128 reverse mode; nothing to do here

    { ⛔ The three PRINT families each read ONE bank, and a front-end coercion
      gap is what puts the wrong register index there. Checked rather than
      trusted, because the interpreter forgives such a collision by reading an
      unrelated register while the module simply fails to validate - and an
      invalid module is the one thing this backend must never emit. }
    ssaPrintInt:
      begin
        if not BankIs(Instr.Src1, srtInt, 'ssaPrintInt') then Exit(False);
        LoadReg(B, Instr.Src1);
        B.Call(FPrintIntFunc);
      end;
    ssaPrintIntLn:
      begin
        if not BankIs(Instr.Src1, srtInt, 'ssaPrintIntLn') then Exit(False);
        LoadReg(B, Instr.Src1);
        B.Call(FPrintIntFunc);
        B.Call(FPrintNlFunc);
      end;
    ssaPrintNewLine:
      B.Call(FPrintNlFunc);

    { PRINT of a FLOAT. Src3 = 1 marks a SINGLE, which shows 7 significant
      digits against a Double's 16 - the count is an argument, so one function
      serves both and "OPTION DIGITS n" as well. }
    ssaPrint, ssaPrintLn:
      begin
        if not BankIs(Instr.Src1, srtFloat, 'ssaPrint') then Exit(False);
        LoadReg(B, Instr.Src1);
        { A SINGLE shows 7 digits by DEFAULT, but "OPTION DIGITS n" overrides
          both banks - which is what the interpreter does, and printing a Single
          at 7 where the interpreter shows 25 is a divergence the sweep caught. }
        if (Instr.Src3.Kind = svkConstInt) and (Instr.Src3.ConstInt = 1) and
           (FFltDigits = 16) then
          B.I32Const(7)
        else
          B.I32Const(FFltDigits);
        B.Call(FFltPrintFunc);
        if Instr.OpCode = ssaPrintLn then B.Call(FPrintNlFunc);
      end;
    ssaPrintUInt:
      begin
        if not BankIs(Instr.Src1, srtInt, 'ssaPrintUInt') then Exit(False);
        LoadReg(B, Instr.Src1);
        B.Call(FPrintUIntFunc);
      end;
    { PRINT's semicolon separator. Every dialect preset in the tree sets
      SemicolonAction to saNoSpace - all eight of them - so this emits nothing.
      Written out rather than folded in with the other no-ops, because that is a
      MEASURED fact about the presets and not an assumption about the language:
      the property is writable, and if a preset ever asks for a space this is the
      place that has to grow one. }
    ssaPrintSemicolon: ;

    { ⚠️ TAB(n) AND SPC(n) EMIT NOTHING HERE, and that is the interpreter's rule
      read rather than a shortcut: in MODERN they are cursor MOVEMENTS that only
      happen onto a VISIBLE screen, and to a redirected stream FreeBASIC writes
      nothing at all (SedaiBytecodeVM, bcPrintTab: the MODERN arm is skipped
      unless FOutputDevice.IsScreenVisible). The host here is a byte sink, so
      there is never a visible screen and the answer is always "nothing" - the
      column does not move either, exactly as in the skipped arm.
      ⛔ CLASSIC always emits the spaces, and that arm is unreachable: the target
      is MODERN-only. ⚠️ NO differential can tell this apart from a defect,
      because headless sb prints nothing either - it is right by CONSTRUCTION,
      not by measurement, and the day the module gets a real screen it is the
      first thing to revisit. }
    ssaPrintTab, ssaPrintSpc: ;

    { PRINT's comma: tab to the next zone, or start a new line when that zone
      would fall off the end. The rule is GetNextTabPosition + the caTabZone arm
      of the interpreter, with the MODERN numbers - and NOT a reading of what
      FreeBASIC "should" do: the pair (14, 80) is written down in
      SedaiBytecodeVM and was measured against sb before this was written.
      ⭐ The pad goes out as ONE write, built in the digit scratch. That area is
      only live inside printInt, and a comma is never inside one - so it is free
      space rather than a place to be careful about. }
    ssaPrintComma:
      begin
        // 0 = the next zone: ((col / 14) + 1) * 14, or 0 when it leaves the line
        B.GlobalGet(FColG); B.I32Const(COMMA_TAB); B.Op(wopI32DivS);
        B.I32Const(1); B.Op(wopI32Add);
        B.I32Const(COMMA_TAB); B.Op(wopI32Mul);
        B.LocalTee(FGfxN);
        B.I32Const(SCREEN_COLS); B.Op(wopI32GeS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.Call(FPrintNlFunc);              // ... and printNl zeroes the column
        B.Op(wopElse);
          // fill (next - col) spaces in the scratch, write them in one go
          B.I32Const(4);
          B.I32Const(Ord(' '));
          B.LocalGet(FGfxN); B.GlobalGet(FColG); B.Op(wopI32Sub);
          B.MemoryFill;
          B.I32Const(4);
          B.LocalGet(FGfxN); B.GlobalGet(FColG); B.Op(wopI32Sub);
          B.Call(FWriteFunc);
          B.LocalGet(FGfxN); B.GlobalSet(FColG);
        B.EndOp;
      end;

    { ---- strings ------------------------------------------------------ }

    { STR$(x) / Str(x) of a float. ⭐ It is the PRINT rendering with the spacing
      taken off, and it is written that way ON PURPOSE: two formatters for the
      same double would agree until the day they did not, and the interpreter
      makes the same choice (Trim of FormatNumber).
      ⚠️ The SINGLE flag rides Src3 exactly as it does on ssaPrint, and it means
      the same thing - 7 significant digits unless OPTION DIGITS said otherwise. }
    { ⭐ ssaFloatToString is HERE and not in a case of its own because it is the
      same thing: the interpreter runs the identical line for both
      (Trim of FormatNumber), one reached by writing Str(x) and the other by
      letting a float meet a string in a concatenation. Splitting them would be
      two implementations of one rule, which is how two renderings of the same
      double come to agree until they do not. }
    ssaStrStr, ssaFloatToString:
      begin
        if not BankIs(Instr.Src1, srtFloat, OpName(Instr.OpCode)) then Exit(False);
        B.I32Const(FLT_CAP); B.I32Const(1); B.OpMem(wopI32Store, 2, 0);
        LoadReg(B, Instr.Src1);
        if (Instr.Src3.Kind = svkConstInt) and (Instr.Src3.ConstInt = 1) and
           (FFltDigits = 16) then
          B.I32Const(7)
        else
          B.I32Const(FFltDigits);
        B.Call(FFltPrintFunc);
        B.I32Const(FLT_CAP); B.I32Const(0); B.OpMem(wopI32Store, 2, 0);
        B.Call(FFltStrFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaLoadConstString:
      begin
        if Instr.Src1.Kind <> svkConstString then
          Exit(Fail('ssaLoadConstString without a string constant'));
        { ⛔ And the DESTINATION has to be in the string bank. It is not always:
          "Dim As Double d = ""3.14""" lowers to a string constant loaded into a
          FLOAT register, so the i32 address lands in an f64 local and the module
          does not validate. See BankIs - fbc rejects that line outright. }
        if not BankIs(Instr.Dest, srtString, 'ssaLoadConstString') then Exit(False);
        B.I32Const(LongInt(ConstAddrOf(Instr.Src1)));
        StoreReg(B, Instr.Dest);
      end;

    ssaPrintString:
      begin
        if not BankIs(Instr.Src1, srtString, 'ssaPrintString') then Exit(False);
        LoadReg(B, Instr.Src1);
        B.Call(FPrintStrFunc);
      end;
    ssaPrintStringLn:
      begin
        if not BankIs(Instr.Src1, srtString, 'ssaPrintStringLn') then Exit(False);
        LoadReg(B, Instr.Src1);
        B.Call(FPrintStrFunc);
        B.Call(FPrintNlFunc);
      end;

    ssaStrLen:
      begin
        LoadReg(B, Instr.Src1);
        B.OpMem(wopI32Load, 2, 0);
        B.Op(wopI64ExtendI32U);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrConcat:
      begin
        // The three-operand form is the fused "a + b + c"; it has no arm here
        // yet, and a wrong answer is not on offer.
        if Instr.Src3.Kind <> svkNone then
          Exit(Fail('a three-operand ssaStrConcat is not covered yet'));
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        B.Call(FStrCatFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrLeft:
      begin
        // LEFT is MID from 1: a negative length falls out as empty through
        // strSub's own "count <= 0" rule, exactly as bcStrLeft does.
        LoadReg(B, Instr.Src1);
        B.I64Const(1);
        LoadReg(B, Instr.Src2);
        B.Call(FStrSubFunc);
        StoreReg(B, Instr.Dest);
      end;
    ssaStrRight:
      begin
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        B.Call(FStrRightFunc);
        StoreReg(B, Instr.Dest);
      end;
    ssaStrMid:
      begin
        if Instr.Src3.Kind <> svkRegister then
          Exit(Fail('ssaStrMid without a length register'));
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        LoadReg(B, Instr.Src3);
        B.Call(FStrMidFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrAsc:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FStrAscFunc);
        StoreReg(B, Instr.Dest);
      end;
    { ASC(MID$(s, start, len)) fused by the SSA into one instruction - and the
      idiom is unavoidable, since "read character i" is written that way in
      every BASIC program that walks a string.
      ⭐ It is emitted as strMid followed by strAsc rather than as a fourth copy
      of MID's dialect rules. The interpreter keeps a hand-written arm here and
      its own comment warns that the two must not drift apart; composing them
      makes drifting impossible. What that costs is the substring the fusion
      exists to avoid - a real optimisation, and one that can only be made
      later, on top of something known to be right. }
    ssaStrAscMid:
      begin
        if Instr.Src3.Kind <> svkRegister then
          Exit(Fail('ssaStrAscMid without a length register'));
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        LoadReg(B, Instr.Src3);
        B.Call(FStrMidFunc);
        B.Call(FStrAscFunc);
        StoreReg(B, Instr.Dest);
      end;
    ssaStrChr:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FStrChrFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaIntToString:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FStrFromIntFunc);
        StoreReg(B, Instr.Dest);
      end;

    { VAL and the VALINT family. ⚠️ ssaStrVal is not only VAL(): it is also
      CDBL of a string and the coercion FreeBASIC performs whenever a string
      reaches a numeric context, which is why it blocked far more programs than
      the keyword's own use would suggest. }
    ssaStrVal:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FStrValFunc);
        StoreReg(B, Instr.Dest);
      end;
    ssaStrValInt:
      begin
        { Src3 is the DECIMAL saturation width - 32 for the Long/ULong spellings -
          and it is a compile-time constant, so it is stored once right before
          the call. ⚠️ Here it is Src3 and not Immediate: this backend reads the
          SSA, where the bytecode compiler has not yet moved one into the other. }
        B.I32Const(VAL_DECW);
        if Instr.Src3.Kind = svkConstInt then B.I32Const(LongInt(Instr.Src3.ConstInt))
                                         else B.I32Const(0);
        B.OpMem(wopI32Store, 2, 0);
        LoadReg(B, Instr.Src1);
        B.Call(FStrValIntFunc);
        StoreReg(B, Instr.Dest);
      end;

    { PRINT USING: format the field, then hand the bytes to the sink. Two
      opcodes because the value's TYPE matters - an exact integer past 2^53 has
      to keep every digit instead of being rounded through a Double, which is
      why the interpreter separates them too. }
    ssaPrintUsing, ssaPrintUsingInt:
      begin
        LoadReg(B, Instr.Src1);                    // the format string
        if Instr.OpCode = ssaPrintUsingInt then
        begin
          B.F64Const(0);
          B.I32Const(1);
          LoadReg(B, Instr.Src2);
        end
        else
        begin
          LoadReg(B, Instr.Src2);
          B.I32Const(0);
          B.I64Const(0);
        end;
        B.Call(FPuFmtFunc);
        B.Call(FPrintStrFunc);
      end;

    { COMMAND$(n) - the command line, which a WASM module does not have.
      ⭐ The EMPTY STRING is the honest answer and not a stub: a module was not
      launched from a shell, so it genuinely received no arguments. A program
      that switches on an argument therefore takes its default branch, which is
      what "run it in a browser" should mean.
      📌 The day the page wants to pass something, the natural mapping is the
      query string - a decision about MEANING, to be taken then and declared,
      not smuggled in now. }
    ssaCommand:
      begin
        B.I32Const(EMPTY_STR);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrSpace:
      begin
        LoadReg(B, Instr.Src1);
        B.I64Const(Ord(' '));
        B.Call(FStrFillFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrString:
      begin
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);      // the CODE, masked to a byte by the helper
        B.Call(FStrFillFunc);
        StoreReg(B, Instr.Dest);
      end;

    { LTRIM / RTRIM / TRIM: one helper, the sides in a flag - as UCASE and LCASE share theirs. }
    ssaStrLTrim, ssaStrRTrim, ssaStrTrim:
      begin
        LoadReg(B, Instr.Src1);
        case Instr.OpCode of
          ssaStrLTrim: B.I32Const(1);
          ssaStrRTrim: B.I32Const(2);
        else           B.I32Const(3);
        end;
        B.Call(FStrTrimFunc);
        StoreReg(B, Instr.Dest);
      end;

    { HEX$ / OCT / BIN: one helper, the base as a third argument - and Src2 is
      always a real register holding the optional width (0 when it was absent),
      materialised by SedaiSSA so nothing here has to guess. }
    ssaStrHex, ssaStrOct, ssaStrBin:
      begin
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        case Instr.OpCode of
          ssaStrHex: B.I64Const(16);
          ssaStrOct: B.I64Const(8);
        else         B.I64Const(2);
        end;
        B.Call(FStrBaseFunc);
        StoreReg(B, Instr.Dest);
      end;

    { TRIM(s, set) and its two sides. The mode is a compile-time constant that
      SedaiSSA already packs the way bcStrTrimSet reads it - side in the low two
      bits, the "Any" form in bit 2 - so it travels unchanged. }
    ssaStrTrimSet:
      begin
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        if Instr.Src3.Kind = svkConstInt then
          B.I32Const(LongInt(Instr.Src3.ConstInt))
        else
          B.I32Const(0);          // both ends, the whole substring: the default
        B.Call(FStrTrimSetFunc);
        StoreReg(B, Instr.Dest);
      end;

    { MK* and CV*: the byte movement is one helper each way, and the float
      spellings are the integer ones with a reinterpretation at the edge.
      ⚠️ MKS$ DEMOTES first - one rounding, not two - which is the rule the
      whole SINGLE family follows here. }
    ssaStrMkInt:
      begin
        LoadReg(B, Instr.Src1);
        B.I32Const(LongInt(Instr.Src3.ConstInt));
        B.Call(FStrPackFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrMkFloat:
      begin
        LoadReg(B, Instr.Src1);
        if Instr.Src3.ConstInt = 4 then
        begin
          B.Op(wopF32DemoteF64);
          B.Op(wopI32ReinterpretF32);
          B.Op(wopI64ExtendI32U);
        end
        else
          B.Op(wopI64ReinterpretF64);
        B.I32Const(LongInt(Instr.Src3.ConstInt));
        B.Call(FStrPackFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrCvInt:
      begin
        LoadReg(B, Instr.Src1);
        B.I32Const(LongInt(Instr.Src3.ConstInt));
        B.Call(FStrUnpackFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrCvFloat:
      begin
        LoadReg(B, Instr.Src1);
        B.I32Const(LongInt(Instr.Src3.ConstInt));
        B.Call(FStrUnpackFunc);
        if Instr.Src3.ConstInt = 4 then
        begin
          B.Op(wopI32WrapI64);
          B.Op(wopF32ReinterpretI32);
          B.Op(wopF64PromoteF32);
        end
        else
          B.Op(wopF64ReinterpretI64);
        StoreReg(B, Instr.Dest);
      end;

    { ---- the calendar ---- }

    { YEAR / MONTH / DAY / HOUR / MINUTE / SECOND / WEEKDAY, the immediate
      choosing the field - the same immediate bcDateDecode reads. }
    ssaDateDecode:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FDtSplitFunc);
        case Instr.Src3.ConstInt of
          0, 1, 2:                     // year, month, day
            begin
              B.I32Const(DT_DAYS); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S);
              B.Call(FDtCivilFunc);
              case Instr.Src3.ConstInt of
                0: B.I32Const(DT_Y);
                1: B.I32Const(DT_MO);
              else B.I32Const(DT_D);
              end;
              B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S);
            end;
          3, 4, 5:                     // hour, minute, second
            begin
              B.I32Const(DT_MS); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S);
              case Instr.Src3.ConstInt of
                3: begin B.I64Const(3600000); B.Op(wopI64DivS); end;
                4: begin B.I64Const(60000); B.Op(wopI64DivS);
                         B.I64Const(60); B.Op(wopI64RemS); end;
              else     begin B.I64Const(1000); B.Op(wopI64DivS);
                         B.I64Const(60); B.Op(wopI64RemS); end;
              end;
            end;
          6:                           // WEEKDAY, 1 = Sunday .. 7 = Saturday
            begin
              { Serial 0 is a Saturday (measured), so serial + 6 is a Sunday-based
                count - and the remainder needs the floor form, because a serial
                before 1899 is negative and Pascal's mod is not. }
              B.I32Const(DT_DAYS); B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S);
              B.I64Const(6); B.Op(wopI64Add);
              B.I64Const(7); B.Op(wopI64RemS);
              B.I64Const(7); B.Op(wopI64Add);
              B.I64Const(7); B.Op(wopI64RemS);
              B.I64Const(1); B.Op(wopI64Add);
            end;
        else
          B.I64Const(0);
        end;
        StoreReg(B, Instr.Dest);
      end;

    { DATESERIAL(y, m, d), with the rollover the interpreter gets from IncMonth:
      the year's 1st of January, then m-1 months, then d-1 days. ⚠️ NOT
      dtDays(y, m, d) - that would reject month 13 instead of rolling it into
      the next year, and DateSerial(2026, 13, 1) is measured as 2027-01-01. }
    ssaDateSerial:
      begin
        LoadReg(B, Instr.Src1);
        B.I64Const(1); B.I64Const(1);
        B.Call(FDtDaysFunc);
        B.Op(wopF64ConvertI64S);
        LoadReg(B, Instr.Src2); B.I64Const(1); B.Op(wopI64Sub);
        B.Call(FDtIncMoFunc);
        LoadReg(B, Instr.Src3); B.I64Const(1); B.Op(wopI64Sub);
        B.Op(wopF64ConvertI64S); B.Op(wopF64Add);
        StoreReg(B, Instr.Dest);
      end;

    ssaTimeSerial:
      begin
        LoadReg(B, Instr.Src1); B.Op(wopF64ConvertI64S);
        B.F64Const(3600.0); B.Op(wopF64Mul);
        LoadReg(B, Instr.Src2); B.Op(wopF64ConvertI64S);
        B.F64Const(60.0); B.Op(wopF64Mul); B.Op(wopF64Add);
        LoadReg(B, Instr.Src3); B.Op(wopF64ConvertI64S); B.Op(wopF64Add);
        B.F64Const(86400.0); B.Op(wopF64Div);
        StoreReg(B, Instr.Dest);
      end;

    { SADD(s) -> a raw pointer to a NUL-terminated COPY of the bytes, allocated
      the way RawAlloc allocates: an eight-byte header holding the payload size,
      then the payload rounded up to eight. The pointer that comes back is the
      tagged form - region bit clear, offset relative to the heap base - so
      everything that already dereferences a raw pointer reads it without
      knowing where it came from.
      ⚠️ A COPY, and read-only in effect: writing through it does not reach the
      managed string, exactly as the interpreter documents. A managed string has
      no stable mutable buffer address to hand out.
      ⛔ And the value is only meaningful INSIDE the module. SADD exists to hand
      a ZSTRING to foreign code, and a .wasm has no foreign code to hand it to -
      it is covered because a program may compute with it, not because the
      address means anything outside. }
    ssaStrSAdd:
      begin
        LoadReg(B, Instr.Src1); B.LocalTee(FArrTmp);
        B.OpMem(wopI32Load, 2, 0); B.LocalSet(FGfxN);          // the byte length
        B.LocalGet(FGfxN); B.I32Const(8); B.Op(wopI32Add);     // len + 1, rounded up to 8
        B.I32Const(-8); B.Op(wopI32And); B.LocalSet(FRecTmp);
        B.LocalGet(FRecTmp); B.I32Const(8); B.Op(wopI32Add);
        B.Call(FAllocFunc); B.LocalTee(FGfxP);
        B.LocalGet(FRecTmp);
        B.OpMem(wopI32Store, 2, 0);                            // the size header
        B.LocalGet(FGfxP); B.I32Const(8); B.Op(wopI32Add);
        B.LocalGet(FArrTmp); B.I32Const(4); B.Op(wopI32Add);
        B.LocalGet(FGfxN);
        B.MemoryCopy;
        B.LocalGet(FGfxP); B.LocalGet(FGfxN); B.Op(wopI32Add);
        B.I32Const(0); B.OpMem(wopI32Store8, 0, 8);            // the NUL
        B.LocalGet(FGfxP); B.I32Const(8); B.Op(wopI32Add);
        B.I32Const(LongInt(FHeapBase)); B.Op(wopI32Sub);
        B.Op(wopI64ExtendI32U);
        B.I64Const(RAWPTR_TAG); B.Op(wopI64Or);
        StoreReg(B, Instr.Dest);
      end;

    { ERR$(n): one load from the handle table, and a code with no message of its
      own is rendered "ERROR n" at run time - which is what the interpreter's
      else branch formats. ⭐ The codes are two ranges, so the slot is computed
      rather than being the code itself. }
    ssaStrErr:
      begin
        LoadReg(B, Instr.Src1); B.Op(wopI32WrapI64); B.LocalTee(FArrTmp);
        B.I32Const(0); B.Op(wopI32GeS);
        B.LocalGet(FArrTmp); B.I32Const(ERR_LO_MAX); B.Op(wopI32LeS);
        B.Op(wopI32And);
        B.BlockStart(wopIf, WASM_TYPE_I32);
          B.LocalGet(FArrTmp);
        B.Op(wopElse);
          B.LocalGet(FArrTmp); B.I32Const(ERR_HI_LO); B.Op(wopI32GeS);
          B.LocalGet(FArrTmp); B.I32Const(ERR_HI_MAX); B.Op(wopI32LeS);
          B.Op(wopI32And);
          B.BlockStart(wopIf, WASM_TYPE_I32);
            B.LocalGet(FArrTmp); B.I32Const(ERR_HI_LO - (ERR_LO_MAX + 1)); B.Op(wopI32Sub);
          B.Op(wopElse);
            B.I32Const(-1);
          B.EndOp;
        B.EndOp;
        B.LocalTee(FRecTmp);
        B.I32Const(0); B.Op(wopI32GeS);
        B.BlockStart(wopIf, WASM_TYPE_I32);
          B.LocalGet(FRecTmp); B.I32Const(4); B.Op(wopI32Mul);
          B.OpMem(wopI32Load, 2, ERR_NAMES);
        B.Op(wopElse);
          B.I32Const(0);
        B.EndOp;
        B.LocalTee(FGfxP);
        B.Op(wopI32Eqz);
        B.BlockStart(wopIf, WASM_TYPE_I32);
          B.I32Const((ERR_SLOTS - 1) * 4); B.OpMem(wopI32Load, 2, ERR_NAMES);
          B.LocalGet(FArrTmp); B.Op(wopI64ExtendI32S); B.Call(FStrFromIntFunc);
          B.Call(FStrCatFunc);
        B.Op(wopElse);
          B.LocalGet(FGfxP);
        B.EndOp;
        StoreReg(B, Instr.Dest);
      end;

    { RND -> genrand_int32 / 2^32, which is what FPC's Random is. }
    ssaMathRnd:
      begin
        B.Call(FMtNextFunc);
        B.Op(wopF64ConvertI64U);
        B.F64Const(4294967296.0); B.Op(wopF64Div);
        StoreReg(B, Instr.Dest);
      end;

    { RANDOMIZE. With a seed it is init_genrand; without one the interpreter
      goes to the clock, so the module does too - and neither side is
      reproducible then, which is the point of seeding in a guard. }
    ssaRandomize:
      begin
        if (Instr.Src3.Kind = svkConstInt) and (Instr.Src3.ConstInt <> 0) then
          LoadReg(B, Instr.Src1)
        else
        begin
          B.Call(FNowFunc);
          B.F64Const(86400000.0); B.Op(wopF64Mul);
          B.TruncSat(wopfcI64TruncSatF64S);
        end;
        B.Call(FMtSeedFunc);
      end;

    { A BOOLEAN prints as the WORD, not as a number, so the two words live in a
      data segment and this is a pointer and a length straight to the sink. The
      condition is read twice - it is a register, so that is free - rather than
      parked in a local. }
    ssaPrintBool:
      begin
        LoadReg(B, Instr.Src1); B.Op(wopI64Eqz);
        B.BlockStart(wopIf, WASM_TYPE_I32);
          B.I32Const(BOOL_FALSE);
        B.Op(wopElse);
          B.I32Const(BOOL_TRUE);
        B.EndOp;
        LoadReg(B, Instr.Src1); B.Op(wopI64Eqz);
        B.BlockStart(wopIf, WASM_TYPE_I32);
          B.I32Const(5);
        B.Op(wopElse);
          B.I32Const(4);
        B.EndOp;
        B.Call(WriteTarget);
      end;

    { ERASE. ⭐ Two different operations under one name, and the immediate says
      which: a DYNAMIC array is freed - no block, no elements, no dimensions,
      which is what leaves LBound 0 and UBound -1 behind - while a STATIC one
      KEEPS its bounds and only zeroes its elements.
      ⚠️ Zero is the right empty value for all three banks at once: an integer
      reads 0, a double reads +0.0, and a string handle of 0 reads as the empty
      string because linear memory starts zeroed. That is why one memory.fill
      serves an array of any type. }
    ssaArrayErase:
      begin
        if Instr.Src1.Kind <> svkArrayRef then
          Exit(Fail('ssaArrayErase without an array reference'));
        if (Instr.Src1.ArrayIndex < 0) or (Instr.Src1.ArrayIndex >= FProg.GetArrayCount) then
          Exit(Fail('ssaArrayErase names an array that was never declared'));
        Info := FProg.GetArray(Instr.Src1.ArrayIndex);
        Desc := FArrDescOf[Instr.Src1.ArrayIndex];
        { ⚠️⚠️ AND HERE IS A MISDIAGNOSIS WORTH LEAVING WRITTEN DOWN. This read
          "ask the ARRAY, because the flag in Src3 arrives as zero" for a while,
          and the flag arrives perfectly - it was measured wrong. A dynamic ERASE
          that clears the block, the count and the dimension number but NOT the
          per-dimension bounds answers LBound 1 and UBound 4 - which is exactly
          what a STATIC erase answers too. The symptom could not tell the two
          branches apart, I picked the wrong one, and then "confirmed" it by
          changing the branch test and the bound clearing in the SAME step.
          ⇒ The bug was only ever the bounds, below. The flag is the front end's
          own answer and is used as it stands. }
        IsDyn := Instr.Src3.ConstInt <> 0;
        if IsDyn then
        begin
          B.I32Const(LongInt(Desc));      B.I32Const(0); B.OpMem(wopI32Store, 2, 0);
          B.I32Const(LongInt(Desc + 4));  B.I32Const(0); B.OpMem(wopI32Store, 2, 0);
          B.I32Const(LongInt(Desc + 8));  B.I32Const(0); B.OpMem(wopI32Store, 2, 0);
          { ⛔ AND THE PER-DIMENSION BOUNDS, which is what LBOUND and UBOUND
            actually read - clearing the block, the count and the dimension
            number leaves them untouched, so a freed array still answered its
            old 1 and 4 instead of the 0 and -1 an empty one gives. UBOUND is
            lb + size - 1, so a lower bound of 0 and a size of 0 produce the -1
            by arithmetic rather than by a special case. }
          for d := 0 to Info.DimCount - 1 do
          begin
            B.I32Const(LongInt(Desc + LongWord(16 + 8 * d)));
            B.I32Const(0); B.OpMem(wopI32Store, 2, 0);
            B.I32Const(LongInt(Desc + LongWord(20 + 8 * d)));
            B.I32Const(0); B.OpMem(wopI32Store, 2, 0);
          end;
        end
        else
        begin
          B.I32Const(LongInt(Desc)); B.OpMem(wopI32Load, 2, 0); B.LocalTee(FArrTmp);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);   // nothing to clear before DIM
            B.LocalGet(FArrTmp);
            B.I32Const(0);
            B.I32Const(LongInt(Desc + 4)); B.OpMem(wopI32Load, 2, 0);
            B.I32Const(8); B.Op(wopI32Mul);
            B.MemoryFill;
          B.EndOp;
        end;
      end;

    { DATE$ / TIME$: read the clock, split it once, render a fixed-width field. }
    ssaDateStr:
      begin
        B.Call(FNowFunc);
        B.Call(FDtSplitFunc);
        if Instr.Src3.ConstInt = 1 then B.I32Const(1) else B.I32Const(0);
        B.Call(FDtStrFunc);
        StoreReg(B, Instr.Dest);
      end;

    { MONTHNAME(n) / WEEKDAYNAME(n): one i32.load of the handle table, and an
      index outside the range answers the EMPTY string - handle 0, which reads
      as length 0 because linear memory starts zeroed. Not an error: that is
      what bcDateName returns. }
    ssaDateName:
      begin
        LoadReg(B, Instr.Src1);
        B.Op(wopI32WrapI64);
        B.LocalTee(FArrTmp);
        B.I32Const(1); B.Op(wopI32LtS);
        B.LocalGet(FArrTmp);
        if Instr.Src3.ConstInt = 1 then B.I32Const(7) else B.I32Const(12);
        B.Op(wopI32GtS);
        B.Op(wopI32Or);
        B.BlockStart(wopIf, WASM_TYPE_I32);
          B.I32Const(EMPTY_STR);
        B.Op(wopElse);
          B.LocalGet(FArrTmp); B.I32Const(1); B.Op(wopI32Sub);
          if Instr.Src3.ConstInt <> 1 then
          begin
            B.I32Const(7); B.Op(wopI32Add);      // the months follow the weekdays
          end;
          B.I32Const(4); B.Op(wopI32Mul);
          B.OpMem(wopI32Load, 2, DT_NAMES);
        B.EndOp;
        StoreReg(B, Instr.Dest);
      end;

    { DATEVALUE / TIMEVALUE (the immediate picks the half) and ISDATE. A parse
      that fails is 0 and -1/0, not an error - the interpreter's answer too. }
    ssaDateValue:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FDtParseFunc);
        B.BlockStart(wopIf, WASM_TYPE_F64);
          B.I32Const(DT_PARSE); B.OpMem(wopF64Load, 3, 0);
          if Instr.Src3.ConstInt = 1 then
          begin
            // TIMEVALUE: the fraction, as Frac gives it
            B.LocalTee(FFltTmp);
            B.LocalGet(FFltTmp); B.Op(wopF64Trunc); B.Op(wopF64Sub);
          end
          else
            B.Op(wopF64Trunc);
        B.Op(wopElse);
          B.F64Const(0);
        B.EndOp;
        StoreReg(B, Instr.Dest);
      end;

    ssaIsDate:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FDtParseFunc);
        B.BlockStart(wopIf, WASM_TYPE_I64);
          B.I64Const(-1);
        B.Op(wopElse);
          B.I64Const(0);
        B.EndOp;
        StoreReg(B, Instr.Dest);
      end;

    ssaDatePart:
      begin
        LoadReg(B, Instr.Src2);
        B.Call(FDtSplitFunc);
        LoadReg(B, Instr.Src1);
        B.Call(FDtIvalFunc);
        EmitDatePartSwitch(B);
        StoreReg(B, Instr.Dest);
      end;

    ssaDateAdd:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FDtIvalFunc);
        EmitDateAddSwitch(B, Instr);
        StoreReg(B, Instr.Dest);
      end;

    ssaDateDiff:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FDtIvalFunc);
        EmitDateDiffSwitch(B, Instr);
        StoreReg(B, Instr.Dest);
      end;

    { ---- WSTRING: the same operations counted in CODEPOINTS ---- }
    ssaStrLenW:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FWLenFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrLeftW:
      begin
        LoadReg(B, Instr.Src1);
        B.I64Const(1);
        LoadReg(B, Instr.Src2);
        B.Call(FWSubFunc);
        StoreReg(B, Instr.Dest);
      end;

    { RIGHT$ of a wide string has to COUNT before it can cut - there is no
      walking backwards through UTF-8 - so the start is total - count + 1.
      ⭐ AND THE CLAMPS ARE NOT REPEATED HERE. The interpreter clamps count to
      the total before computing the start; wSub already clamps both its own
      arguments, and the two produce the same answer on every case: a count
      above the total gives a start at or below zero, which clamps to 1 and
      returns the whole string, and a negative one gives a start past the end,
      which returns empty. Clamping twice would be a second place to disagree. }
    ssaStrRightW:
      begin
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src1); B.Call(FWLenFunc);
        LoadReg(B, Instr.Src2); B.Op(wopI64Sub);
        B.I64Const(1); B.Op(wopI64Add);
        LoadReg(B, Instr.Src2);
        B.Call(FWSubFunc);
        StoreReg(B, Instr.Dest);
      end;

    { MID$ of a wide string. ⚠️ A NEGATIVE length means "the rest", and the
      interpreter makes that rule conditional on MODERN - which is the only
      dialect this target accepts, so it applies unconditionally here. }
    ssaStrMidW:
      begin
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        LoadReg(B, Instr.Src3);
        B.I64Const(0); B.Op(wopI64LtS);
        B.BlockStart(wopIf, WASM_TYPE_I64);
          LoadReg(B, Instr.Src1); B.Call(FWLenFunc);
          LoadReg(B, Instr.Src2); B.Op(wopI64Sub);
          B.I64Const(1); B.Op(wopI64Add);
        B.Op(wopElse);
          LoadReg(B, Instr.Src3);
        B.EndOp;
        B.Call(FWSubFunc);
        StoreReg(B, Instr.Dest);
      end;

    { ⭐ The two wide searches need NO wide search: a UTF-8 substring can only
      match at a codepoint boundary, so the ordinary byte search answers, and the
      only wide step is turning its byte position into a codepoint one. That is
      why the interpreter scans bytes here too. }
    ssaStrInstrW, ssaStrInstrRevW:
      begin
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        B.I64Const(1);
        if Instr.OpCode = ssaStrInstrRevW then B.I32Const(1) else B.I32Const(0);
        B.Call(FStrFindFunc);
        B.Call(FWB2CPFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrWChr:
      begin
        LoadReg(B, Instr.Src1);
        B.Call(FWEncFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrWStringN:
      begin
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        B.Call(FWRepFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrUCase, ssaStrLCase:
      begin
        LoadReg(B, Instr.Src1);
        if Instr.OpCode = ssaStrUCase then B.I32Const(1) else B.I32Const(0);
        B.Call(FStrCaseFunc);
        StoreReg(B, Instr.Dest);
      end;

    { INSTR's start position rides in a THIRD register (Src3), not as a value:
      the two-argument form materialises a constant 1 so there is always a real
      register to read (SedaiSSA). ⚠️ The BYTECODE puts that register NUMBER in
      the immediate, which is a different thing entirely - reading the immediate
      here would search from position <register number>. }
    ssaStrInstr:
      begin
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        LoadReg(B, Instr.Src3);
        B.I32Const(0);               // forwards, a substring
        B.Call(FStrFindFunc);
        StoreReg(B, Instr.Dest);
      end;

    { The other three searches, the same helper with the two bits set. ⚠️ The
      REV forms take no start of their own - the interpreter's ignore theirs -
      so they hand over a literal 1 rather than a register. }
    ssaStrInstrAny:
      begin
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        LoadReg(B, Instr.Src3);
        B.I32Const(2);               // forwards, a character set
        B.Call(FStrFindFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrInstrRev, ssaStrInstrRevAny:
      begin
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        B.I64Const(1);
        if Instr.OpCode = ssaStrInstrRevAny then B.I32Const(3) else B.I32Const(1);
        B.Call(FStrFindFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString:
      begin
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        B.Call(FStrCmpFunc);
        case Instr.OpCode of
          ssaCmpEqString: B.Op(wopI32Eqz);
          ssaCmpNeString: begin B.I32Const(0); B.Op(wopI32Ne); end;
          ssaCmpLtString: begin B.I32Const(0); B.Op(wopI32LtS); end;
        else
          begin B.I32Const(0); B.Op(wopI32GtS); end;
        end;
        BoolToBasic(B);
        StoreReg(B, Instr.Dest);
      end;

    { ⛔ The two in-place string opcodes. They are not "not written yet": they
      are incompatible with handles that alias, which is what ssaCopyString
      makes. Covering them wants either a copy on write or a real ownership
      model, and until then a refusal is the only honest answer. }
    { The MID STATEMENT: overwrite Len(Src2) bytes of the string at Src3, in
      place, never changing its length. In place is exactly what this backend
      cannot do - a handle can be shared by several registers, and ssaCopyString
      copies the handle - so the string is COPIED first and the copy is what
      gets written. That is copy-on-write done at the write site: correct
      however the handles alias, and it needs no ownership bookkeeping at all.
      ⚠️ THE PRICE IS DECLARED, not hidden: one allocation per assignment, where
      the interpreter writes into the buffer it already has. A loop filling a
      long string one character at a time therefore allocates quadratically, and
      the allocator never frees - the v1 limit this whole subsystem already
      carries. ⭐ The way out, when it is worth it, is a SHARED bit in the string
      header: strNew makes an owned string, ssaCopyString marks it shared, and
      only a shared one has to be copied here.
      ⛔ The clamps are the interpreter's, and both matter: a start outside
      1..Len writes NOTHING (fbc leaves the string untouched there, where the
      old rebuild used to prepend and grow it), and the replacement is cut to
      the room that remains. }
    ssaPowFloat:
      begin
        if not BankIs(Instr.Src1, srtFloat, 'ssaPowFloat base') then Exit(False);
        if not BankIs(Instr.Src2, srtFloat, 'ssaPowFloat exponent') then Exit(False);
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        B.Call(FPowFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrMidAssign:
      begin
        if not BankIs(Instr.Dest, srtString, 'ssaStrMidAssign target') then Exit(False);
        if not BankIs(Instr.Src1, srtString, 'ssaStrMidAssign source') then Exit(False);
        if not BankIs(Instr.Src2, srtString, 'ssaStrMidAssign replacement') then Exit(False);
        if not FHasDescTmp then
          Exit(Fail('ssaStrMidAssign with no scratch local reserved for this region'));
        // the copy: new = strNew(len(src)), bytes moved over
        LoadReg(B, Instr.Src1); B.LocalTee(FRecTmp);
        B.OpMem(wopI32Load, 2, 0); B.LocalTee(FGfxN);
        B.Call(FStrNewFunc); B.LocalSet(FGfxP);
        B.LocalGet(FGfxP); B.I32Const(4); B.Op(wopI32Add);
        B.LocalGet(FRecTmp); B.I32Const(4); B.Op(wopI32Add);
        B.LocalGet(FGfxN);
        B.MemoryCopy;
        // the start position, 1-based
        LoadReg(B, Instr.Src3); B.Op(wopI32WrapI64); B.LocalSet(FArrTmp);
        B.LocalGet(FArrTmp); B.I32Const(1); B.Op(wopI32GeS);
        B.LocalGet(FArrTmp); B.LocalGet(FGfxN); B.Op(wopI32LeS);
        B.Op(wopI32And);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          // avail = len - pos + 1, and the replacement is cut to it
          B.LocalGet(FGfxN); B.LocalGet(FArrTmp); B.Op(wopI32Sub);
          B.I32Const(1); B.Op(wopI32Add); B.LocalSet(FGfxN);
          LoadReg(B, Instr.Src2); B.LocalTee(FRecTmp);
          B.OpMem(wopI32Load, 2, 0); B.LocalTee(FDescTmp);
          B.LocalGet(FGfxN); B.Op(wopI32GtS);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(FGfxN); B.LocalSet(FDescTmp);
          B.EndOp;
          B.LocalGet(FDescTmp); B.I32Const(0); B.Op(wopI32GtS);
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(FGfxP); B.I32Const(4); B.Op(wopI32Add);
            B.LocalGet(FArrTmp); B.Op(wopI32Add); B.I32Const(1); B.Op(wopI32Sub);
            B.LocalGet(FRecTmp); B.I32Const(4); B.Op(wopI32Add);
            B.LocalGet(FDescTmp);
            B.MemoryCopy;
          B.EndOp;
        B.EndOp;
        B.LocalGet(FGfxP);
        StoreReg(B, Instr.Dest);
      end;

    ssaStrAppendMapped, ssaStrConcatCharAt:
      Exit(Fail(Format('%s mutates a string in place, and in this backend a ' +
                       'string handle can be shared by several registers - ' +
                       'covering it needs copy-on-write first (line %d)',
                       [OpName(Instr.OpCode), Instr.SourceLine])));

    { ---- UDT / records -------------------------------------------------

      A record is a BYTE IMAGE with fbc's layout, not a bag of slots - that is
      what makes SizeOf, OffsetOf and a union that aliases across banks come out
      right, and it is why a field access carries an ENCODED offset rather than
      an index. The encoding is the interpreter's (RecFieldInt): the byte offset
      is Enc shr 4, and the low nibble is the width - 0 a full i64, 1..6 the
      signed/unsigned 8/16/32 forms, 7 a SINGLE.

      Strings cannot live in that image (a handle is not the value), so they sit
      in their own area after it, and the header says where:
        +0 typeId · +4 the byte offset of the string area · +8 the image }

    ssaRecordNew:
      begin
        if (Instr.Src1.Kind <> svkConstInt) or (Instr.Src3.Kind <> svkConstInt) then
          Exit(Fail('ssaRecordNew without its compile-time sizes'));
        Bytes := Integer(Instr.Src1.ConstInt);
        NStr := Integer(Instr.Src3.ConstInt and $FFFF);
        StrBase := 8 + ((Bytes + 7) div 8) * 8;
        B.I32Const(StrBase + 4 * NStr);
        B.Call(FAllocFunc);
        B.LocalTee(FRecTmp);
        B.I32Const(Integer((Instr.Src3.ConstInt shr 32) and $FFFF));
        B.OpMem(wopI32Store, 2, 0);                 // typeId
        B.LocalGet(FRecTmp);
        B.I32Const(StrBase);
        B.OpMem(wopI32Store, 2, 4);                 // where the strings start
        // the handle lives in an INT register, so it travels as an i64
        B.LocalGet(FRecTmp);
        B.Op(wopI64ExtendI32U);
        StoreReg(B, Instr.Dest);
        { ⭐ Nothing zeroes the image, for the same reason DIM does not zero an
          array: bump-allocated memory has never been written, and linear memory
          and every grown page start at zero. A string slot of 0 is the empty
          string, which is what an unassigned one has to be. }
      end;

    { DIM of an array of UDT. ⚠️ The packed counts arrive in ONE constant here
      (Src2) where ssaRecordNew splits them over Src1 and Src3 - the bytecode
      compiler folds this one into an immediate, and the fields are the same
      three: byte size, string count, type id. }
    ssaRecordNewArray:
      begin
        if Instr.Src1.Kind <> svkArrayRef then
          Exit(Fail('ssaRecordNewArray without an array reference'));
        if (Instr.Src1.ArrayIndex < 0) or (Instr.Src1.ArrayIndex >= FProg.GetArrayCount) then
          Exit(Fail('ssaRecordNewArray names an array that was never declared'));
        if Instr.Src2.Kind <> svkConstInt then
          Exit(Fail('ssaRecordNewArray without its compile-time sizes'));
        Bytes := Integer(Instr.Src2.ConstInt and $FFFF);
        NStr := Integer((Instr.Src2.ConstInt shr 32) and $FFFF);
        StrBase := 8 + ((Bytes + 7) div 8) * 8;
        B.I32Const(LongInt(FArrDescOf[Instr.Src1.ArrayIndex]));
        B.I32Const(StrBase + 4 * NStr);
        B.I32Const(StrBase);
        B.I32Const(Integer((Instr.Src2.ConstInt shr 48) and $FFFF));
        B.Call(FRecNewArrFunc);
      end;

    ssaRecordTypeId:
      begin
        LoadReg(B, Instr.Src1);
        B.Op(wopI32WrapI64);        // the handle travels as i64; an address is i32
        B.OpMem(wopI32Load, 2, 0);
        B.Op(wopI64ExtendI32S);
        StoreReg(B, Instr.Dest);
      end;

    { ⛔ DELETE is a NO-OP here and that is a stated consequence of the bump
      allocator, not an oversight: nothing is ever freed. It differs from the
      interpreter only for a program that uses a handle AFTER deleting it, which
      is undefined there too - and it errs toward keeping memory alive rather
      than reading something that has been handed to someone else. The same goes
      for the block-scoped reclamation marks. }
    ssaRecordFree, ssaRecMarkPush, ssaRecMarkPop: ;

    ssaRecordLoadInt, ssaRecordLoadFloat, ssaRecordStoreInt, ssaRecordStoreFloat:
      begin
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('a record field access without a constant field encoding'));
        Enc := Instr.Src3.ConstInt;
        Loaded32 := False;
        LoadReg(B, Instr.Src1);                     // the handle
        B.Op(wopI32WrapI64);
        // ⭐ The mirror of the load above: a value that LIVES in an i32 local is stored FROM an i32,
        // which saves the widening the bank would otherwise force on the way out.
        Stored32 := (Instr.OpCode = ssaRecordStoreInt) and RegIs32(Instr.Src2) and
                    ((Enc and $F) >= 1) and ((Enc and $F) <= 6);
        if Stored32 then LoadRegI32(B, Instr.Src2)
        else if OpIn(Instr.OpCode, [ssaRecordStoreInt, ssaRecordStoreFloat]) then
          LoadReg(B, Instr.Src2);                   // the value
        Ofs := LongWord(8 + (Enc shr 4));
        case Instr.OpCode of
          ssaRecordLoadInt:
            // ⭐ A field read into a register that LIVES in an i32 local is loaded AS an i32: the
            // 8- and 16-bit forms land the value where it belongs, where the i64 form would then be
            // wrapped on the way into the local. One instruction shorter, and the direct spelling.
            if RegIs32(Instr.Dest) and ((Enc and $F) >= 1) and ((Enc and $F) <= 6) then
            begin
              Loaded32 := True;
              case Enc and $F of
                1: B.OpMem(wopI32Load8S, 0, Ofs);
                2: B.OpMem(wopI32Load8U, 0, Ofs);
                3: B.OpMem(wopI32Load16S, 0, Ofs);
                4: B.OpMem(wopI32Load16U, 0, Ofs);
              else
                B.OpMem(wopI32Load, 2, Ofs);       // 5 and 6: the whole 32-bit word
              end;
            end
            else
            case Enc and $F of
              1: B.OpMem(wopI64Load8S, 0, Ofs);
              2: B.OpMem(wopI64Load8U, 0, Ofs);
              3: B.OpMem(wopI64Load16S, 0, Ofs);
              4: B.OpMem(wopI64Load16U, 0, Ofs);
              5: B.OpMem(wopI64Load32S, 0, Ofs);
              6: B.OpMem(wopI64Load32U, 0, Ofs);
            else
              B.OpMem(wopI64Load, 0, Ofs);
            end;
          ssaRecordStoreInt:
            if Stored32 then
              case Enc and $F of
                1, 2: B.OpMem(wopI32Store8, 0, Ofs);
                3, 4: B.OpMem(wopI32Store16, 0, Ofs);
              else
                B.OpMem(wopI32Store, 2, Ofs);       // 5 and 6: the whole 32-bit word
              end
            else
            case Enc and $F of
              1, 2: B.OpMem(wopI64Store8, 0, Ofs);
              3, 4: B.OpMem(wopI64Store16, 0, Ofs);
              5, 6: B.OpMem(wopI64Store32, 0, Ofs);
            else
              B.OpMem(wopI64Store, 0, Ofs);
            end;
          ssaRecordLoadFloat:
            if (Enc and $F) = 7 then
            begin
              B.OpMem(wopF32Load, 0, Ofs);          // a SINGLE really is 4 bytes
              B.Op(wopF64PromoteF32);
            end
            else
              B.OpMem(wopF64Load, 0, Ofs);
        else
          if (Enc and $F) = 7 then
          begin
            B.Op(wopF32DemoteF64);
            B.OpMem(wopF32Store, 0, Ofs);
          end
          else
            B.OpMem(wopF64Store, 0, Ofs);
        end;
        if OpIn(Instr.OpCode, [ssaRecordLoadInt, ssaRecordLoadFloat]) then
          // ⛔ The i32 path already put an i32 on the stack, so StoreReg's wrap would be applied to
          // one - a module that does not validate, not a wrong number. It goes straight to the local.
          if Loaded32 then B.LocalSet(FLocalIdx[FCurRegion][FlatId(Instr.Dest)])
          else StoreReg(B, Instr.Dest);
      end;

    ssaRecordLoadString, ssaRecordStoreString:
      begin
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('a record string field without a constant slot index'));
        // addr = handle + strBase + 4*slot, and strBase is read from the header
        // because the image's size is not known where the field is touched
        LoadReg(B, Instr.Src1);
        B.Op(wopI32WrapI64);
        B.LocalTee(FRecTmp);
        B.LocalGet(FRecTmp);
        B.OpMem(wopI32Load, 2, 4);
        B.Op(wopI32Add);
        if Instr.OpCode = ssaRecordStoreString then
        begin
          LoadReg(B, Instr.Src2);
          B.OpMem(wopI32Store, 2, LongWord(4 * Instr.Src3.ConstInt));
        end
        else
        begin
          B.OpMem(wopI32Load, 2, LongWord(4 * Instr.Src3.ConstInt));
          StoreReg(B, Instr.Dest);
        end;
      end;

    { ---- arrays ------------------------------------------------------- }

    ssaArrayDim:
      begin
        if Instr.Src1.Kind <> svkArrayRef then
          Exit(Fail('ssaArrayDim without an array reference'));
        if (Instr.Src1.ArrayIndex < 0) or (Instr.Src1.ArrayIndex >= FProg.GetArrayCount) then
          Exit(Fail('ssaArrayDim names an array that was never declared'));
        Info := FProg.GetArray(Instr.Src1.ArrayIndex);
        Desc := FArrDescOf[Instr.Src1.ArrayIndex];

        B.I32Const(1); B.LocalSet(FArrTmp);
        for d := 0 to Info.DimCount - 1 do
        begin
          // the lower bound: a RUNTIME register wins over the constant, which is
          // how "Dim a(LBound(m) To UBound(m))" gets its bounds from another array
          B.I32Const(LongInt(Desc + LongWord(16 + 8 * d)));
          if (d <= High(Info.LowerBoundRegisters)) and (Info.LowerBoundRegisters[d] >= 0) then
          begin
            LoadReg(B, MakeSSARegister(srtInt, Info.LowerBoundRegisters[d]));
            B.Op(wopI32WrapI64);
          end
          else if d <= High(Info.LowerBounds) then
            B.I32Const(Info.LowerBounds[d])
          else
            B.I32Const(0);
          B.OpMem(wopI32Store, 2, 0);

          // the size: a constant dimension, or ub - lb + 1 from its register
          B.I32Const(LongInt(Desc + LongWord(20 + 8 * d)));
          if (d <= High(Info.Dimensions)) and (Info.Dimensions[d] <> 0) then
            B.I32Const(Info.Dimensions[d])
          else
          begin
            if (d > High(Info.DimRegisters)) or (Info.DimRegisters[d] < 0) then
              Exit(Fail(Format('array "%s" has a variable dimension %d with no register',
                               [Info.Name, d])));
            LoadReg(B, MakeSSARegister(Info.DimRegTypes[d], Info.DimRegisters[d]));
            case Info.DimRegTypes[d] of
              srtInt:   B.Op(wopI32WrapI64);
              srtFloat: B.TruncSat(wopfcI32TruncSatF64S);   // the VM truncates
            else
              Exit(Fail(Format('array "%s" takes dimension %d from a string register',
                               [Info.Name, d])));
            end;
            B.I32Const(LongInt(Desc + LongWord(16 + 8 * d)));
            B.OpMem(wopI32Load, 2, 0);
            B.Op(wopI32Sub);
            B.I32Const(1); B.Op(wopI32Add);
          end;
          B.OpMem(wopI32Store, 2, 0);

          B.LocalGet(FArrTmp);
          B.I32Const(LongInt(Desc + LongWord(20 + 8 * d)));
          B.OpMem(wopI32Load, 2, 0);
          B.Op(wopI32Mul);
          B.LocalSet(FArrTmp);
        end;

        // An upper bound below the lower one gives a NEGATIVE count. Left alone
        // it would hand the bump allocator a negative size and walk the cursor
        // BACKWARDS over memory already handed out - so it is clamped, and the
        // array comes out empty, which is what a zero TotalSize means anyway.
        B.LocalGet(FArrTmp); B.I32Const(0); B.Op(wopI32LtS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(0); B.LocalSet(FArrTmp);
        B.EndOp;

        B.I32Const(LongInt(Desc + 4)); B.LocalGet(FArrTmp); B.OpMem(wopI32Store, 2, 0);
        B.I32Const(LongInt(Desc + 8)); B.I32Const(Info.DimCount); B.OpMem(wopI32Store, 2, 0);
        B.I32Const(LongInt(Desc));
        B.LocalGet(FArrTmp); B.I32Const(8); B.Op(wopI32Mul);
        B.Call(FAllocFunc);
        B.OpMem(wopI32Store, 2, 0);
      end;

    ssaArrayLoad:
      begin
        if Instr.Src1.Kind <> svkArrayRef then
          Exit(Fail('ssaArrayLoad without an array reference'));
        B.I32Const(LongInt(FArrDescOf[Instr.Src1.ArrayIndex]));
        LoadReg(B, Instr.Src2);
        B.Call(FArrLoad[Instr.Dest.RegType]);
        StoreReg(B, Instr.Dest);
      end;

    { ⚠️ Dest is the VALUE here, not a destination - the array and the index are
      in Src1 and Src2. It reads the way an assignment is written, not the way
      the other opcodes are. }
    ssaArrayStore:
      begin
        if Instr.Src1.Kind <> svkArrayRef then
          Exit(Fail('ssaArrayStore without an array reference'));
        B.I32Const(LongInt(FArrDescOf[Instr.Src1.ArrayIndex]));
        LoadReg(B, Instr.Src2);
        LoadReg(B, Instr.Dest);
        B.Call(FArrStore[Instr.Dest.RegType]);
      end;

    { ⭐ BINDING AN ARRAY PARAMETER IS A DESCRIPTOR SWAP, and nothing is copied:
      the placeholder's descriptor is made to name the caller's data, so the
      callee writes THROUGH it and the caller sees the writes - which is what
      BYREF means. The saved descriptor and the snapshot go into LOCALS, so a
      recursive procedure gets a fresh save per activation for free.
      ⛔ Two phases, and the reason is real: a batch that SWAPS arrays
      (merge sort calling proc(b(), a()) where arg and param slots coincide)
      has to read every argument BEFORE any of them is overwritten. So bind
      snapshots, and bindApply commits. }
    ssaArrayBind:
      begin
        if (Instr.Src1.Kind <> svkArrayRef) or (Instr.Src3.Kind <> svkConstInt) then
          Exit(Fail('ssaArrayBind without an array reference and a constant argument'));
        d := Instr.Src1.ArrayIndex;
        n := Integer(Instr.Src3.ConstInt);
        if (n < 0) or (n >= FProg.GetArrayCount) then
          Exit(Fail('ssaArrayBind names an argument array that was never declared'));
        { ⛔ A descriptor is 16 + 8*dim bytes, so binding across DIFFERENT
          dimension counts would copy the wrong number of words - reading past
          one descriptor and writing past the other, which corrupts the array
          that happens to sit next in memory. Refuse instead: it is decidable
          right here, both indices being constants. }
        if FProg.GetArray(d).DimCount <> FProg.GetArray(n).DimCount then
          Exit(Fail(Format('array parameter "%s" has %d dimension(s) but the argument "%s" has %d',
                           [FProg.GetArray(d).Name, FProg.GetArray(d).DimCount,
                            FProg.GetArray(n).Name, FProg.GetArray(n).DimCount])));
        if FBindSeq > High(FBindLocal) then
          Exit(Fail('more array binds emitted than the pre-pass counted'));
        SetLength(FBindStack, FBindTop + 1);
        FBindStack[FBindTop].ParamIdx := d;
        FBindStack[FBindTop].ArgIdx := n;
        FBindStack[FBindTop].Words := FBindWords[FBindSeq];
        FBindStack[FBindTop].SavedLocal := FBindLocal[FBindSeq];
        FBindStack[FBindTop].SnapLocal := FBindLocal[FBindSeq] +
                                          LongWord(FBindWords[FBindSeq]);
        for k := 0 to FBindStack[FBindTop].Words - 1 do
        begin
          B.I32Const(LongInt(FArrDescOf[d] + LongWord(4 * k)));
          B.OpMem(wopI32Load, 2, 0);
          B.LocalSet(FBindStack[FBindTop].SavedLocal + LongWord(k));
          B.I32Const(LongInt(FArrDescOf[n] + LongWord(4 * k)));
          B.OpMem(wopI32Load, 2, 0);
          B.LocalSet(FBindStack[FBindTop].SnapLocal + LongWord(k));
        end;
        Inc(FBindTop);
        Inc(FBindSeq);
      end;

    ssaArrayBindApply:
      begin
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('ssaArrayBindApply without a constant count'));
        n := Integer(Instr.Src3.ConstInt);
        if (n < 0) or (n > FBindTop) then
          Exit(Fail('ssaArrayBindApply commits more binds than are pending'));
        for d := FBindTop - n to FBindTop - 1 do
          for k := 0 to FBindStack[d].Words - 1 do
          begin
            B.I32Const(LongInt(FArrDescOf[FBindStack[d].ParamIdx] + LongWord(4 * k)));
            B.LocalGet(FBindStack[d].SnapLocal + LongWord(k));
            B.OpMem(wopI32Store, 2, 0);
          end;
      end;

    ssaArrayUnbind:
      begin
        if Instr.Src1.Kind <> svkArrayRef then
          Exit(Fail('ssaArrayUnbind without an array reference'));
        if FBindTop = 0 then
          Exit(Fail('ssaArrayUnbind with no matching bind'));
        Dec(FBindTop);
        if FBindStack[FBindTop].ParamIdx <> Instr.Src1.ArrayIndex then
          Exit(Fail('ssaArrayUnbind does not name the array the last bind took'));
        d := FBindStack[FBindTop].ParamIdx;
        n := FBindStack[FBindTop].ArgIdx;
        { ⚠️ Copy the callee's descriptor BACK only when a REDIM reallocated the
          storage - recognised by the base no longer being the one snapshotted.
          Without a resize the caller already sees every write through the
          shared data, and copying unconditionally is WRONG: in deep recursion
          the argument slot may have been rebound at an outer level, and an
          unconditional copy corrupts it. Exactly what the interpreter does.
          ✅ EXERCISED since REDIM was covered: a SUB doing "ReDim Preserve a(0
          To n)" on its array parameter reallocates, so the base no longer
          matches the snapshot and the caller sees the new array - guardian
          array_params.bas. Before that this arm was the interpreter's rule
          transcribed and untested, which is why covering REDIM was the thing
          that closed it. }
        if n <> d then
        begin
          { ⛔ THE TEST IS "THE DESCRIPTOR CHANGED", NOT "THE BASE CHANGED", and
            the difference is a real defect that got this far: alloc(0) does not
            advance the bump cursor, so an EMPTY array and the first block
            allocated after it have the SAME base. A callee doing
            "ReDim d(1 To n)" on a parameter that arrived empty therefore came
            back looking untouched, and the caller kept its empty array while
            the callee had happily filled a new one.
            ⇒ Compare every word: if the callee rewrote the descriptor at all,
            the caller has to see it. Writes to the ELEMENTS still change
            nothing here, which is right - those are already shared. }
          B.I32Const(0);
          for k := 0 to FBindStack[FBindTop].Words - 1 do
          begin
            B.I32Const(LongInt(FArrDescOf[d] + LongWord(4 * k)));
            B.OpMem(wopI32Load, 2, 0);
            B.LocalGet(FBindStack[FBindTop].SnapLocal + LongWord(k));
            B.Op(wopI32Ne);
            B.Op(wopI32Or);
          end;
          B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
            for k := 0 to FBindStack[FBindTop].Words - 1 do
            begin
              B.I32Const(LongInt(FArrDescOf[n] + LongWord(4 * k)));
              B.I32Const(LongInt(FArrDescOf[d] + LongWord(4 * k)));
              B.OpMem(wopI32Load, 2, 0);
              B.OpMem(wopI32Store, 2, 0);
            end;
          B.EndOp;
        end;
        for k := 0 to FBindStack[FBindTop].Words - 1 do
        begin
          B.I32Const(LongInt(FArrDescOf[d] + LongWord(4 * k)));
          B.LocalGet(FBindStack[FBindTop].SavedLocal + LongWord(k));
          B.OpMem(wopI32Store, 2, 0);
        end;
      end;

    { REDIM [PRESERVE] a([lb TO] ub): a NEW block, and the descriptor retargeted
      at it. ⚠️ It COLLAPSES the array to ONE dimension, which is not a
      simplification here but what RedimArray does - the interpreter rewrites
      DimCount, Dimensions[0] and LowerBounds[0] and leaves nothing else.
      ⭐ Not preserving costs NOTHING: a fresh block from the bump allocator is
      already zero, so "clear it" and "allocate it" are the same act. }
    { One bound of a REDIM, captured into its own local. ⛔ Captured HERE and
      not read back at the commit: after this instruction the bound's register
      is dead, so the allocator may hand the next bound the same one - and the
      commit would then reshape the array with one bound repeated, silently. }
    ssaArrayRedimPush:
      begin
        if not BankIs(Instr.Src1, srtInt, 'ssaArrayRedimPush') then Exit(False);
        if FRedimSeq > High(FRedimLocal) then
          Exit(Fail('more REDIM bound pushes emitted than the pre-pass counted'));
        LoadReg(B, Instr.Src1);
        B.Op(wopI32WrapI64);
        B.LocalSet(FRedimLocal[FRedimSeq]);
        SetLength(FRedimPend, Length(FRedimPend) + 1);
        FRedimPend[High(FRedimPend)].Local := FRedimLocal[FRedimSeq];
        FRedimPend[High(FRedimPend)].IsLb :=
          (Instr.Src3.Kind = svkConstInt) and (Instr.Src3.ConstInt = 1);
        Inc(FRedimSeq);
      end;

    ssaArrayRedim:
      begin
        if Instr.Src1.Kind <> svkArrayRef then
          Exit(Fail('ssaArrayRedim without an array reference'));
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('ssaArrayRedim without constant flags'));
        Desc := FArrDescOf[Instr.Src1.ArrayIndex];
        n := Integer(Instr.Src3.ConstInt);
        { ⛔ A RUNTIME lower bound arrives as a preceding push, and it WINS over
          the flags - the interpreter's rule, and the one thing that makes this
          arm reachable now that pushes are covered. Reading the flags instead
          would keep the OLD lower bound and put every element at the wrong
          index, in silence. }
        if Length(FRedimPend) > 0 then
        begin
          if (Length(FRedimPend) <> 1) or (not FRedimPend[0].IsLb) then
            Exit(Fail('a single-dimension REDIM with bounds pushed for a different shape'));
          B.LocalGet(FRedimPend[0].Local);
          SetLength(FRedimPend, 0);
        end
        // the lower bound: the explicit one, or the array's current one
        else if (n and 2) <> 0 then
          B.I32Const(n shr 8)
        else
        begin
          B.I32Const(LongInt(Desc + 16));
          B.OpMem(wopI32Load, 2, 0);
        end;
        B.LocalSet(FRecTmp);
        // the new element count, clamped at zero exactly as RedimArray clamps it
        LoadReg(B, Instr.Src2); B.Op(wopI32WrapI64);
        B.LocalGet(FRecTmp); B.Op(wopI32Sub);
        B.I32Const(1); B.Op(wopI32Add);
        B.LocalTee(FArrTmp);
        B.I32Const(0); B.Op(wopI32LtS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I32Const(0); B.LocalSet(FArrTmp);
        B.EndOp;
        B.LocalGet(FArrTmp); B.I32Const(8); B.Op(wopI32Mul);
        B.Call(FAllocFunc); B.LocalSet(FGfxP);
        if (n and 1) <> 0 then
        begin
          { PRESERVE keeps the flat element order up to the SMALLER of the two
            sizes - SetLength's own rule, which is what the interpreter leans on.
            Anything past that is already zero in the new block. }
          B.LocalGet(FGfxP);
          B.I32Const(LongInt(Desc)); B.OpMem(wopI32Load, 2, 0);
          B.I32Const(LongInt(Desc + 4)); B.OpMem(wopI32Load, 2, 0);
          B.LocalTee(FGfxN);
          B.LocalGet(FArrTmp);
          B.Op(wopI32GtS);
          B.BlockStart(wopIf, WASM_TYPE_I32);
            B.LocalGet(FArrTmp);
          B.Op(wopElse);
            B.LocalGet(FGfxN);
          B.EndOp;
          B.I32Const(8); B.Op(wopI32Mul);
          B.MemoryCopy;
        end;
        B.I32Const(LongInt(Desc));      B.LocalGet(FGfxP);  B.OpMem(wopI32Store, 2, 0);
        B.I32Const(LongInt(Desc + 4));  B.LocalGet(FArrTmp); B.OpMem(wopI32Store, 2, 0);
        B.I32Const(LongInt(Desc + 8));  B.I32Const(1);       B.OpMem(wopI32Store, 2, 0);
        B.I32Const(LongInt(Desc + 16)); B.LocalGet(FRecTmp); B.OpMem(wopI32Store, 2, 0);
        B.I32Const(LongInt(Desc + 20)); B.LocalGet(FArrTmp); B.OpMem(wopI32Store, 2, 0);
      end;

    { A procedure's address IS its slot in the function table, which is its
      region index. Natively the same value is a bytecode entry PC: both are
      opaque numbers a program can only pass around and call through, and this
      one is the only one an engine can dispatch on. }
    ssaLoadProcAddr:
      begin
        if Instr.Src1.Kind <> svkLabel then
          Exit(Fail('ssaLoadProcAddr without a procedure label'));
        n := BlockOfLabel(Instr.Src1.LabelName);
        if (n < 0) or (n >= Length(FRegionOf)) then
          Exit(Fail(Format('ssaLoadProcAddr names "%s", which is not a procedure in this module',
                           [Instr.Src1.LabelName])));
        B.I64Const(FRegionOf[n]);
        StoreReg(B, Instr.Dest);
      end;

    { ---- FreeBASIC pointers -------------------------------------------

      The ADDRESS side needs no opcode at all and that is worth saying: "@x"
      lowers to the constant (backingArrayId+1) shl 32 and "@a(i)" to that
      constant plus the index, so taking an address is ordinary integer
      arithmetic that this backend already emitted. Only the DEREF and the
      record-field pack are opcodes, which is why covering pointers is these
      seven cases and no more. }

    ssaRefLoadInt, ssaRefLoadFloat, ssaRefLoadString:
      begin
        if not BankIs(Instr.Src1, srtInt, OpName(Instr.OpCode) + ' address') then
          Exit(False);
        LoadReg(B, Instr.Src1);
        B.Call(FRefLoad[Instr.Dest.RegType]);
        StoreReg(B, Instr.Dest);
      end;

    ssaRefStoreInt, ssaRefStoreFloat, ssaRefStoreString:
      begin
        if not BankIs(Instr.Src1, srtInt, OpName(Instr.OpCode) + ' address') then
          Exit(False);
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        B.Call(FRefStore[Instr.Src2.RegType]);
      end;

    { @obj.field. ⚠️ The record's HANDLE is its linear address here, where
      natively it is an index into a record table - so this pointer's numeric
      value differs from the interpreter's while the managed one does not. It
      is the deliberate half of the encoding: nothing but a print of the
      pointer itself can tell, and the alternative was a second table. }
    ssaRefAddrField:
      begin
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('ssaRefAddrField without a constant field encoding'));
        if (Instr.Src3.ConstInt < 0) or (Instr.Src3.ConstInt > RECPTR_SLOT_MASK) then
          Exit(Fail('ssaRefAddrField with a field encoding too wide to pack'));
        LoadReg(B, Instr.Src1);
        B.I64Const(RECPTR_SLOT_BITS); B.Op(wopI64Shl);
        B.I64Const(RECPTR_TAG or Instr.Src3.ConstInt);
        B.Op(wopI64Or);
        StoreReg(B, Instr.Dest);
      end;

    { The multi-dimensional commit. The bounds arrived as pushes - all the LOWER
      ones first when every dimension was written "lb TO ub", then all the
      uppers - which is the order SedaiSSA lays them down, and the reason a mix
      of "lb TO ub" and bare "ub" pushes NO lowers at all: a partial list would
      be misaligned, so the old lower bounds are kept instead.
      ⛔ The descriptor is 16 + 8*dim bytes and its size was fixed when the
      module was laid out, so a REDIM that GROWS the dimension count would write
      past it and into the next array's descriptor. It is decidable right here -
      both counts are compile-time - so it is refused rather than discovered. }
    ssaArrayRedimN:
      begin
        if Instr.Src1.Kind <> svkArrayRef then
          Exit(Fail('ssaArrayRedimN without an array reference'));
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('ssaArrayRedimN without constant flags'));
        Desc := FArrDescOf[Instr.Src1.ArrayIndex];
        Info := FProg.GetArray(Instr.Src1.ArrayIndex);
        n := Integer(Instr.Src3.ConstInt);
        // split the pending pushes into the lowers and the uppers
        NStr := 0;                                   // how many are lower bounds
        for k := 0 to High(FRedimPend) do
          if FRedimPend[k].IsLb then Inc(NStr);
        d := Length(FRedimPend) - NStr;              // and how many are uppers
        if d = 0 then
          Exit(Fail('ssaArrayRedimN with no upper bound pushed for it'));
        if (NStr <> 0) and (NStr <> d) then
          Exit(Fail('ssaArrayRedimN got a partial list of lower bounds'));
        for k := 0 to NStr - 1 do
          if not FRedimPend[k].IsLb then
            Exit(Fail('ssaArrayRedimN got its lower bounds out of order'));
        if d > Info.DimCount then
          Exit(Fail(Format('"ReDim %s(...)" asks for %d dimensions where the array was ' +
                           'declared with %d, and the descriptor was sized for that',
                           [Info.Name, d, Info.DimCount])));
        if not FHasDescTmp then
          Exit(Fail('ssaArrayRedimN with no descriptor local reserved for this region'));
        B.I32Const(LongInt(Desc)); B.LocalSet(FDescTmp);
        EmitRedimShape(B, (n and 1) <> 0, NStr, d);
        SetLength(FRedimPend, 0);
      end;

    { REDIM of an array that is a MEMBER of a record. Same shape code, and the
      only difference is where the descriptor comes from: a member's is on the
      heap, one per record instance, allocated at the first REDIM that sizes it
      - which is exactly when the interpreter first makes its FArrays entry. }
    ssaMemberArrayRedim:
      begin
        if not BankIs(Instr.Src1, srtInt, 'ssaMemberArrayRedim handle') then Exit(False);
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('ssaMemberArrayRedim without its constant field encoding'));
        if not FHasDescTmp then
          Exit(Fail('ssaMemberArrayRedim with no descriptor local reserved for this region'));
        n := Integer(Instr.Src3.ConstInt);
        k := (n shr 8) and $FFFF;                    // the field's encoding
        if (k and $F) <> 0 then
          Exit(Fail('a UDT array member held in a narrowed field is not covered'));
        NStr := 0;
        for d := 0 to High(FRedimPend) do
          if FRedimPend[d].IsLb then Inc(NStr);
        d := Length(FRedimPend) - NStr;
        if d = 0 then
          Exit(Fail('ssaMemberArrayRedim with no upper bound pushed for it'));
        if (NStr <> 0) and (NStr <> d) then
          Exit(Fail('ssaMemberArrayRedim got a partial list of lower bounds'));
        if d > WASM_MEMBER_MAX_DIMS then
          Exit(Fail(Format('a UDT array member is redimensioned to %d dimensions; this ' +
                           'backend sizes a member descriptor for %d',
                           [d, WASM_MEMBER_MAX_DIMS])));
        LoadMemberDesc(B, Instr.Src1, k, True);
        EmitRedimShape(B, (n and 1) <> 0, NStr, d);
        SetLength(FRedimPend, 0);
      end;

    { ---- the same array operations, on a member's runtime descriptor -------

      ⭐ Every one of these is the direct opcode with the descriptor coming from
      a REGISTER, because the helpers already take it as a parameter. What they
      all have to add is the guard the direct forms cannot need: a member that
      has never been REDIMmed has handle 0, and the interpreter answers a read
      with the default and drops a store. Without the test, "total" would be
      read from address 4 - the PRINT scratch - and the bounds check would be
      decided by whatever digits were last formatted. }

    ssaArrayLoadIndInt, ssaArrayLoadIndFloat, ssaArrayLoadIndString:
      begin
        if not BankIs(Instr.Src1, srtInt, OpName(Instr.OpCode) + ' handle') then Exit(False);
        if not FHasDescTmp then
          Exit(Fail(OpName(Instr.OpCode) + ' with no descriptor local reserved'));
        LoadReg(B, Instr.Src1); B.Op(wopI32WrapI64); B.LocalTee(FDescTmp);
        B.BlockStart(wopIf, BlockTypeOf[Instr.Dest.RegType]);
          B.LocalGet(FDescTmp);
          LoadReg(B, Instr.Src2);
          B.Call(FArrLoad[Instr.Dest.RegType]);
        B.Op(wopElse);
          case Instr.Dest.RegType of
            srtFloat:  B.F64Const(0);
            srtString: B.I32Const(EMPTY_STR);
          else
            B.I64Const(0);
          end;
        B.EndOp;
        StoreReg(B, Instr.Dest);
      end;

    ssaArrayStoreIndInt, ssaArrayStoreIndFloat, ssaArrayStoreIndString:
      begin
        if not BankIs(Instr.Src1, srtInt, OpName(Instr.OpCode) + ' handle') then Exit(False);
        if not FHasDescTmp then
          Exit(Fail(OpName(Instr.OpCode) + ' with no descriptor local reserved'));
        LoadReg(B, Instr.Src1); B.Op(wopI32WrapI64); B.LocalTee(FDescTmp);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(FDescTmp);
          LoadReg(B, Instr.Src2);
          LoadReg(B, Instr.Dest);
          B.Call(FArrStore[Instr.Dest.RegType]);
        B.EndOp;
      end;

    ssaArrayLBoundInd, ssaArrayUBoundInd:
      begin
        if not BankIs(Instr.Src1, srtInt, OpName(Instr.OpCode) + ' handle') then Exit(False);
        if not FHasDescTmp then
          Exit(Fail(OpName(Instr.OpCode) + ' with no descriptor local reserved'));
        LoadReg(B, Instr.Src1); B.Op(wopI32WrapI64); B.LocalTee(FDescTmp);
        B.BlockStart(wopIf, WASM_TYPE_I64);
          B.LocalGet(FDescTmp);
          LoadReg(B, Instr.Src2);
          if Instr.OpCode = ssaArrayLBoundInd then B.Call(FArrLBoundFunc)
                                               else B.Call(FArrUBoundFunc);
        B.Op(wopElse);
          // an unallocated member: LBOUND 0 and UBOUND -1, the empty array
          if Instr.OpCode = ssaArrayLBoundInd then B.I64Const(0) else B.I64Const(-1);
        B.EndOp;
        StoreReg(B, Instr.Dest);
      end;

    ssaArrayIdxResolveInd:
      begin
        if not BankIs(Instr.Src1, srtInt, 'ssaArrayIdxResolveInd handle') then Exit(False);
        if Length(FIdxPend) = 0 then
          Exit(Fail('ssaArrayIdxResolveInd with no index pushed for it'));
        if not FHasDescTmp then
          Exit(Fail('ssaArrayIdxResolveInd with no descriptor local reserved'));
        LoadReg(B, Instr.Src1); B.Op(wopI32WrapI64); B.LocalSet(FDescTmp);
        B.I64Const(0);
        for k := 0 to High(FIdxPend) do
        begin
          B.I64Const(1); B.LocalSet(FIdxScratch);
          B.I32Const(k + 1); B.LocalSet(FArrTmp);
          B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
            B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
              B.LocalGet(FArrTmp);
              B.LocalGet(FDescTmp); B.OpMem(wopI32Load, 2, 8);
              B.Op(wopI32GeS); B.BrIf(1);
              B.LocalGet(FIdxScratch);
              B.LocalGet(FDescTmp);
              B.LocalGet(FArrTmp); B.I32Const(8); B.Op(wopI32Mul);
              B.Op(wopI32Add);
              B.OpMem(wopI32Load, 2, 20); B.Op(wopI64ExtendI32S);
              B.Op(wopI64Mul); B.LocalSet(FIdxScratch);
              B.LocalGet(FArrTmp); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(FArrTmp);
              B.Br(0);
            B.EndOp;
          B.EndOp;
          B.LocalGet(FIdxPend[k]); B.LocalGet(FIdxScratch); B.Op(wopI64Mul);
          B.Op(wopI64Add);
        end;
        StoreReg(B, Instr.Dest);
        SetLength(FIdxPend, 0);
      end;

    { An array-of-UDT that is a member: the same eager fill, with the descriptor
      read from the field instead of named by an index. }
    ssaRecordNewArrayInd:
      begin
        if not BankIs(Instr.Src1, srtInt, 'ssaRecordNewArrayInd handle') then Exit(False);
        if Instr.Src2.Kind <> svkConstInt then
          Exit(Fail('ssaRecordNewArrayInd without its compile-time sizes'));
        if not FHasDescTmp then
          Exit(Fail('ssaRecordNewArrayInd with no descriptor local reserved'));
        Bytes := Integer(Instr.Src2.ConstInt and $FFFF);
        NStr := Integer((Instr.Src2.ConstInt shr 32) and $FFFF);
        StrBase := 8 + ((Bytes + 7) div 8) * 8;
        LoadReg(B, Instr.Src1); B.Op(wopI32WrapI64); B.LocalTee(FDescTmp);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.LocalGet(FDescTmp);
          B.I32Const(StrBase + 4 * NStr);
          B.I32Const(StrBase);
          B.I32Const(Integer((Instr.Src2.ConstInt shr 48) and $FFFF));
          B.Call(FRecNewArrFunc);
        B.EndOp;
      end;

    ssaArrayIdxPush:
      begin
        if not BankIs(Instr.Src1, srtInt, 'ssaArrayIdxPush') then Exit(False);
        if FIdxSeq > High(FIdxLocal) then
          Exit(Fail('more array index pushes emitted than the pre-pass counted'));
        LoadReg(B, Instr.Src1);
        B.LocalSet(FIdxLocal[FIdxSeq]);
        SetLength(FIdxPend, Length(FIdxPend) + 1);
        FIdxPend[High(FIdxPend)] := FIdxLocal[FIdxSeq];
        Inc(FIdxSeq);
      end;

    { The row-major linear index, from the array's CURRENT dimensions - which is
      the whole reason this exists rather than a compile-time formula: after a
      REDIM the shape is not the declared one.
      ⚠️ The inner product runs to the array's RUNTIME dimension count, not to
      the number of indices pushed. That is the interpreter's loop transcribed,
      and the two differ only for a program that indexes with the wrong arity -
      where transcribing is the only way to agree with it. }
    ssaArrayIdxResolve:
      begin
        if Instr.Src1.Kind <> svkArrayRef then
          Exit(Fail('ssaArrayIdxResolve without an array reference'));
        if Length(FIdxPend) = 0 then
          Exit(Fail('ssaArrayIdxResolve with no index pushed for it'));
        Desc := FArrDescOf[Instr.Src1.ArrayIndex];
        B.I64Const(0);                                  // the accumulator, on the stack
        for k := 0 to High(FIdxPend) do
        begin
          B.I64Const(1); B.LocalSet(FIdxScratch);
          B.I32Const(k + 1); B.LocalSet(FArrTmp);       // d = i + 1
          B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
            B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
              B.LocalGet(FArrTmp);
              B.I32Const(LongInt(Desc + 8)); B.OpMem(wopI32Load, 2, 0);
              B.Op(wopI32GeS); B.BrIf(1);
              B.LocalGet(FIdxScratch);
              B.I32Const(LongInt(Desc + 20));
              B.LocalGet(FArrTmp); B.I32Const(8); B.Op(wopI32Mul);
              B.Op(wopI32Add);
              B.OpMem(wopI32Load, 2, 0); B.Op(wopI64ExtendI32S);
              B.Op(wopI64Mul); B.LocalSet(FIdxScratch);
              B.LocalGet(FArrTmp); B.I32Const(1); B.Op(wopI32Add); B.LocalSet(FArrTmp);
              B.Br(0);
            B.EndOp;
          B.EndOp;
          B.LocalGet(FIdxPend[k]); B.LocalGet(FIdxScratch); B.Op(wopI64Mul);
          B.Op(wopI64Add);
        end;
        StoreReg(B, Instr.Dest);
        SetLength(FIdxPend, 0);
      end;

    ssaArrayLBound, ssaArrayUBound:
      begin
        if Instr.Src1.Kind <> svkArrayRef then
          Exit(Fail('an array bound query without an array reference'));
        B.I32Const(LongInt(FArrDescOf[Instr.Src1.ArrayIndex]));
        LoadReg(B, Instr.Src2);
        if Instr.OpCode = ssaArrayLBound then B.Call(FArrLBoundFunc)
                                          else B.Call(FArrUBoundFunc);
        StoreReg(B, Instr.Dest);
      end;

    ssaModFloat:
      begin
        // The VM raises "Float modulo by zero"; f64.div would quietly answer NaN
        // and let the program carry on, which is the one thing the backend must
        // never do. The trap is the loud equivalent of the interpreter stopping.
        LoadReg(B, Instr.Src2);
        B.F64Const(0);
        B.Op(wopF64Eq);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.Op(wopUnreachable);
        B.EndOp;
        // x - floor(x / y) * y, exactly as the interpreter computes it
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        B.Op(wopF64Div);
        B.Op(wopF64Floor);
        LoadReg(B, Instr.Src2);
        B.Op(wopF64Mul);
        B.Op(wopF64Sub);
        StoreReg(B, Instr.Dest);
      end;

    { ---- linear memory and SCREENPTR ---------------------------------- }

    ssaGfxScreenRes:
      begin
        if (Instr.Src3.Kind = svkConstInt) and (Instr.Src3.ConstInt > 1) then
          Exit(Fail('SCREENRES with more than one page is not modelled yet'));
        LoadReg(B, Instr.Src1); B.Op(wopI32WrapI64); B.GlobalSet(FScrW);
        LoadReg(B, Instr.Src2); B.Op(wopI32WrapI64); B.GlobalSet(FScrH);

        // The framebuffer comes out of the same bump allocator as everything
        // else - which is what lets a program hold strings AND draw. A second
        // SCREENRES allocates a second buffer and abandons the first: the
        // allocator never frees, and the interpreter reallocates there too.
        B.GlobalGet(FScrW); B.GlobalGet(FScrH); B.Op(wopI32Mul);
        B.I32Const(4); B.Op(wopI32Mul);
        B.Call(FAllocFunc);
        B.GlobalSet(FFbBase);

        // and fill it the way the interpreter does - NOT with zero
        B.GlobalGet(FFbBase); B.LocalSet(FGfxP);
        B.GlobalGet(FScrW); B.GlobalGet(FScrH); B.Op(wopI32Mul); B.LocalSet(FGfxN);
        B.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);
          B.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);
            B.LocalGet(FGfxN); B.Op(wopI32Eqz); B.BrIf(1);
            B.LocalGet(FGfxP); B.I32Const(FB_CLEAR); B.OpMem(wopI32Store, 2, 0);
            B.LocalGet(FGfxP); B.I32Const(4); B.Op(wopI32Add); B.LocalSet(FGfxP);
            B.LocalGet(FGfxN); B.I32Const(1); B.Op(wopI32Sub); B.LocalSet(FGfxN);
            B.Br(0);
          B.EndOp;
        B.EndOp;
      end;

    ssaGfxScreenPtr:
      begin
        // Offset 0 of the framebuffer region - but 0 when there is no screen,
        // because FreeBASIC answers 0 rather than a pointer that fails later.
        B.GlobalGet(FScrW);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I64Const(RAWPTR_TAG or RAWPTR_REGION_FB);
          StoreReg(B, Instr.Dest);
        B.Op(wopElse);
          B.I64Const(0);
          StoreReg(B, Instr.Dest);
        B.EndOp;
      end;

    ssaGfxScreenInfo:
      begin
        // 0=width 1=height 2=depth(32) 3=bytes per pixel(4) 4=pitch(w*4), else 0
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('__SCRINFO with a non-constant selector'));
        case Instr.Src3.ConstInt of
          0: begin B.GlobalGet(FScrW); B.Op(wopI64ExtendI32S); end;
          1: begin B.GlobalGet(FScrH); B.Op(wopI64ExtendI32S); end;
          2: B.I64Const(32);
          3: B.I64Const(4);
          4: begin
               B.GlobalGet(FScrW); B.I32Const(4); B.Op(wopI32Mul);
               B.Op(wopI64ExtendI32S);
             end;
        else
          B.I64Const(0);
        end;
        StoreReg(B, Instr.Dest);
      end;

    ssaRawLoadInt:
      begin
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('a raw load without a constant element type code'));
        LoadReg(B, Instr.Src1);
        EmitRawAddr(B);
        // The type code carries the SIGNEDNESS, so the extension follows it. This comment used to say
        // every width sign-extends "exactly as RawLoadInt does - a ULong Ptr deref comes back negative
        // in the interpreter too, and the backend reproduces the interpreter rather than correcting
        // it". That was true and is no longer: the interpreter was wrong, RTC_U8/U16/U32 now exist, and
        // reproducing the interpreter means zero-extending here too.
        case Instr.Src3.ConstInt of
          RTC_I8:  B.OpMem(wopI64Load8S, 0, 0);
          RTC_I16: B.OpMem(wopI64Load16S, 1, 0);
          RTC_I32: B.OpMem(wopI64Load32S, 2, 0);
          RTC_U8:  B.OpMem(wopI64Load8U, 0, 0);
          RTC_U16: B.OpMem(wopI64Load16U, 1, 0);
          RTC_U32: B.OpMem(wopI64Load32U, 2, 0);
        else
          B.OpMem(wopI64Load, 3, 0);
        end;
        StoreReg(B, Instr.Dest);
      end;

    ssaRawStoreInt:
      begin
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('a raw store without a constant element type code'));
        LoadReg(B, Instr.Src1);
        EmitRawAddr(B);
        LoadReg(B, Instr.Src2);
        case Instr.Src3.ConstInt of
          RTC_I8, RTC_U8:   B.OpMem(wopI64Store8, 0, 0);
          RTC_I16, RTC_U16: B.OpMem(wopI64Store16, 1, 0);
          RTC_I32, RTC_U32: B.OpMem(wopI64Store32, 2, 0);
        else
          B.OpMem(wopI64Store, 3, 0);
        end;
      end;

    ssaRawLoadFloat:
      begin
        LoadReg(B, Instr.Src1);
        EmitRawAddr(B);
        if (Instr.Src3.Kind = svkConstInt) and (Instr.Src3.ConstInt = RTC_SINGLE) then
        begin
          B.OpMem(wopF32Load, 2, 0);
          B.Op(wopF64PromoteF32);      // a Single Ptr deref widens, as it does natively
        end
        else
          B.OpMem(wopF64Load, 3, 0);
        StoreReg(B, Instr.Dest);
      end;

    ssaRawStoreFloat:
      begin
        LoadReg(B, Instr.Src1);
        EmitRawAddr(B);
        LoadReg(B, Instr.Src2);
        if (Instr.Src3.Kind = svkConstInt) and (Instr.Src3.ConstInt = RTC_SINGLE) then
        begin
          B.Op(wopF32DemoteF64);
          B.OpMem(wopF32Store, 2, 0);
        end
        else
          B.OpMem(wopF64Store, 3, 0);
      end;

    { ⭐ RawAlloc is the bump allocator plus a HEADER, and the header buys two
      things that are not optional:
        - REALLOC needs the old size, and there is nowhere else to keep it;
        - the interpreter reserves offset 0 as NULL. Here the first payload
          lands at offset 8 for free, because the header sits in front of it -
          so a perfectly good pointer can never read as NULL.
      ⭐ Zeroing the payload is free and not an omission: the bump allocator
      never reuses, linear memory starts zeroed and memory.grow hands out zeroed
      pages. ⛔ The day it learns to free, this stops being true. }
    ssaRawAlloc:
      begin
        LoadReg(B, Instr.Src1);
        B.LocalTee(FRawTmp);
        B.I64Const(1); B.Op(wopI64LtS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I64Const(1); B.LocalSet(FRawTmp);      // 0 bytes means 1, as the VM does
        B.EndOp;
        // round the payload up to 8, exactly as RawAlloc does
        B.LocalGet(FRawTmp); B.I64Const(7); B.Op(wopI64Add);
        B.I64Const(-8); B.Op(wopI64And); B.LocalSet(FRawTmp);
        B.LocalGet(FRawTmp); B.Op(wopI32WrapI64); B.I32Const(8); B.Op(wopI32Add);
        B.Call(FAllocFunc); B.LocalTee(FGfxP);
        B.LocalGet(FRawTmp); B.Op(wopI32WrapI64);
        B.OpMem(wopI32Store, 2, 0);                // the size header
        B.LocalGet(FGfxP); B.I32Const(8); B.Op(wopI32Add);
        B.I32Const(LongInt(FHeapBase)); B.Op(wopI32Sub);
        B.Op(wopI64ExtendI32U);
        B.I64Const(RAWPTR_TAG); B.Op(wopI64Or);
        StoreReg(B, Instr.Dest);
      end;

    { ⚠️ FREE IS A NO-OP, and that is the v1 limit stated out loud rather than
      hidden: the allocator is a bump pointer and never reclaims. A program that
      allocates in a loop runs until the memory traps. Silently doing nothing is
      correct here in the sense that nothing breaks - what would be wrong is not
      saying so. }
    ssaRawFree: ;

    ssaRawRealloc:
      begin
        // new block, copy across the SMALLER of the two sizes, old block leaks
        LoadReg(B, Instr.Src2);
        B.LocalTee(FRawTmp);
        B.I64Const(1); B.Op(wopI64LtS);
        B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
          B.I64Const(1); B.LocalSet(FRawTmp);
        B.EndOp;
        B.LocalGet(FRawTmp); B.I64Const(7); B.Op(wopI64Add);
        B.I64Const(-8); B.Op(wopI64And); B.LocalSet(FRawTmp);
        B.LocalGet(FRawTmp); B.Op(wopI32WrapI64); B.I32Const(8); B.Op(wopI32Add);
        B.Call(FAllocFunc); B.LocalTee(FGfxP);
        B.LocalGet(FRawTmp); B.Op(wopI32WrapI64);
        B.OpMem(wopI32Store, 2, 0);
        { ⛔ BOTH sizes are read back out of their HEADERS, and that is not
          style: EmitRawAddr uses FRawTmp as its own scratch, so the new size
          cannot be kept there across the call - it held the right value, the
          decode overwrote it, and realloc copied four bytes instead of
          sixty-four. The headers cannot be clobbered by anything. }
        LoadReg(B, Instr.Src1);
        EmitRawAddr(B);
        B.LocalSet(FGfxN);                         // old payload address
        B.LocalGet(FGfxP); B.I32Const(8); B.Op(wopI32Add);
        B.LocalGet(FGfxN);
        B.LocalGet(FGfxN); B.I32Const(8); B.Op(wopI32Sub);
        B.OpMem(wopI32Load, 2, 0);                 // old size, from its header
        B.LocalTee(FArrTmp);
        B.LocalGet(FGfxP); B.OpMem(wopI32Load, 2, 0);   // new size, from its header
        B.Op(wopI32GtU);
        B.BlockStart(wopIf, WASM_TYPE_I32);
          B.LocalGet(FGfxP); B.OpMem(wopI32Load, 2, 0);
        B.Op(wopElse);
          B.LocalGet(FArrTmp);
        B.EndOp;
        B.MemoryCopy;
        B.LocalGet(FGfxP); B.I32Const(8); B.Op(wopI32Add);
        B.I32Const(LongInt(FHeapBase)); B.Op(wopI32Sub);
        B.Op(wopI64ExtendI32U);
        B.I64Const(RAWPTR_TAG); B.Op(wopI64Or);
        StoreReg(B, Instr.Dest);
      end;

    ssaRawClear:
      begin
        LoadReg(B, Instr.Src1);
        EmitRawAddr(B);
        LoadReg(B, Instr.Src2); B.Op(wopI32WrapI64);
        B.I32Const(255); B.Op(wopI32And);          // CLEAR takes a BYTE value
        LoadReg(B, Instr.Src3); B.Op(wopI32WrapI64);
        B.MemoryFill;
      end;

    { ⭐ ONE arm for both, and it is not laziness - it is what the interpreter
      does: bcRawMemCopy and bcRawMemMove both call RawMemCopy. WASM's
      memory.copy is specified to behave as if through a temporary buffer, so it
      is overlap-safe and MEMMOVE is satisfied by construction. Both hand back
      the DESTINATION, which is what FreeBASIC returns. }
    ssaRawMemCopy, ssaRawMemMove:
      begin
        LoadReg(B, Instr.Src1);
        EmitRawAddr(B);
        LoadReg(B, Instr.Src2);
        EmitRawAddr(B);
        LoadReg(B, Instr.Src3); B.Op(wopI32WrapI64);
        B.MemoryCopy;
        LoadReg(B, Instr.Src1);
        StoreReg(B, Instr.Dest);
      end;

    ssaNarrowInt:
      begin
        // Width codes are NarrowInt64's: 1=s8 2=u8 3=s16 4=u16 5=s32 6=u32,
        // anything else is the identity.
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('ssaNarrowInt without a constant width code'));
        // ⭐ A 32-bit narrowing into a register that LIVES in an i32 local is the wrap itself - and
        // when the source is already such a register it is nothing at all but a move. This is where
        // the i32 classification pays for the widening it costs at the boundaries.
        if RegIs32(Instr.Dest) then
        begin
          // Every width has a 32-bit form, and taking it keeps the value out of the 64-bit bank
          // entirely: the sign extensions are single instructions and the unsigned ones a mask, where
          // the i64 route would need a widening on the way back in.
          LoadRegI32(B, Instr.Src1);
          case Instr.Src3.ConstInt of
            1: B.Op(wopI32Extend8S);
            2: begin B.I32Const($FF); B.Op(wopI32And); end;
            3: B.Op(wopI32Extend16S);
            4: begin B.I32Const($FFFF); B.Op(wopI32And); end;
            // 5 and 6 are the identity on a value that is already 32 bits wide.
          end;
          B.LocalSet(FLocalIdx[FCurRegion][FlatId(Instr.Dest)]);
          Exit(True);
        end;
        LoadReg(B, Instr.Src1);
        case Instr.Src3.ConstInt of
          1: B.Op(wopI64Extend8S);
          2: begin B.I64Const($FF); B.Op(wopI64And); end;
          3: B.Op(wopI64Extend16S);
          4: begin B.I64Const($FFFF); B.Op(wopI64And); end;
          5: B.Op(wopI64Extend32S);
          6: begin B.I64Const($FFFFFFFF); B.Op(wopI64And); end;
        end;
        StoreReg(B, Instr.Dest);
      end;

    { Round a Double to what a SINGLE can hold and keep it in the float bank -
      which is exactly a demote followed by a promote. It is not a conversion to
      another type: the value stays a Double that happens to have lost the bits
      a Single cannot carry, and that lost precision is observable. }
    ssaNarrowSingle:
      begin
        LoadReg(B, Instr.Src1);
        B.Op(wopF32DemoteF64);
        B.Op(wopF64PromoteF32);
        StoreReg(B, Instr.Dest);
      end;

    ssaPhi:
      Exit(Fail('a PHI survived into the backend: PHI elimination must run first'));

    ssaLoadConstInt:
      begin
        { ⛔ "@Sin" and its family are not procedure addresses at all: the SSA
          folds them into a CONSTANT carrying BUILTIN_FP_TAG, and the
          interpreter's indirect call recognises the tag and computes the
          operation without jumping anywhere. There is no function to put in the
          table, so this is refused - and refused where the value is BUILT,
          which names the line, rather than at the call that would trap.
          ⚠️ Gated on the program having an indirect call: the same bit pattern
          is a perfectly ordinary integer for a program that never calls
          through it. }
        if FIndirect and (Instr.Src1.Kind = svkConstInt) and
           ((Instr.Src1.ConstInt and BUILTIN_FP_TAG) <> 0) and
           ((Instr.Src1.ConstInt and not (BUILTIN_FP_TAG or $FF)) = 0) then
          Exit(Fail(Format('the address of a math builtin ("@Sin" and its family) is a ' +
                           'tagged sentinel the interpreter interprets, not a procedure ' +
                           'this backend can put in a function table (line %d)',
                           [Instr.SourceLine])));
        if Instr.Src1.Kind = svkConstInt then B.I64Const(Instr.Src1.ConstInt)
        else Exit(Fail('ssaLoadConstInt without an integer constant'));
        if not BankIs(Instr.Dest, srtInt, 'ssaLoadConstInt') then Exit(False);
        StoreReg(B, Instr.Dest);
      end;
    ssaLoadConstFloat:
      begin
        if Instr.Src1.Kind = svkConstFloat then B.F64Const(Instr.Src1.ConstFloat)
        else if Instr.Src1.Kind = svkConstInt then B.F64Const(Instr.Src1.ConstInt)
        else Exit(Fail('ssaLoadConstFloat without a numeric constant'));
        if not BankIs(Instr.Dest, srtFloat, 'ssaLoadConstFloat') then Exit(False);
        StoreReg(B, Instr.Dest);
      end;

    { A copy names its bank in its own opcode, so BOTH ends have to be in it.
      Nothing checked that, and a mismatch is a local.set of the wrong type. }
    ssaCopyInt, ssaCopyFloat, ssaCopyString:
      begin
        case Instr.OpCode of
          ssaCopyInt:
            if not (BankIs(Instr.Dest, srtInt, 'ssaCopyInt') and
                    BankIs(Instr.Src1, srtInt, 'ssaCopyInt')) then Exit(False);
          ssaCopyFloat:
            if not (BankIs(Instr.Dest, srtFloat, 'ssaCopyFloat') and
                    BankIs(Instr.Src1, srtFloat, 'ssaCopyFloat')) then Exit(False);
        else
          if not (BankIs(Instr.Dest, srtString, 'ssaCopyString') and
                  BankIs(Instr.Src1, srtString, 'ssaCopyString')) then Exit(False);
        end;
        LoadReg(B, Instr.Src1);
        StoreReg(B, Instr.Dest);
      end;

    ssaAddInt:  Bin(wopI64Add);
    ssaSubInt:  Bin(wopI64Sub);
    ssaMulInt:  Bin(wopI64Mul);
    // i64.div_s / rem_s TRAP on a zero divisor where the VM raises a BASIC error.
    // Both stop the program loudly; the diagnostic differs and that is recorded,
    // not papered over.
    ssaDivInt:  Bin(wopI64DivS);
    ssaModInt:  Bin(wopI64RemS);
    ssaDivUInt: Bin(wopI64DivU);
    ssaModUInt: Bin(wopI64RemU);
    ssaNegInt:
      begin
        // there is no i64.neg
        B.I64Const(0);
        LoadReg(B, Instr.Src1);
        B.Op(wopI64Sub);
        StoreReg(B, Instr.Dest);
      end;

    // The IEEE extras: one instruction each, which is the whole reason they are opcodes. The f32
    // forms come from the same fusion the arithmetic uses - operation plus the narrowing that
    // consumes it - so nothing extra is needed for them here.
    ssaMathCeil:     Un(wopF64Ceil);
    ssaMathRound:    Un(wopF64Nearest);
    ssaMathMin:      Bin(wopF64Min);
    ssaMathMax:      Bin(wopF64Max);
    ssaMathCopySign: Bin(wopF64Copysign);
    { The bit-casts, and the one place a 32-bit REINTERPRET is the whole point: the value in the float
      bank is a double that a binary32 can hold, so demoting is exact and the bits it then has ARE the
      answer. The other direction reads 32 bits as a binary32 and widens. }
    ssaSingleBits:
      begin
        LoadReg(B, Instr.Src1);
        B.Op(wopF32DemoteF64);
        B.Op(wopI32ReinterpretF32);
        B.Op(wopI64ExtendI32U);
        StoreReg(B, Instr.Dest);
      end;
    ssaBitsToSingle:
      begin
        LoadReg(B, Instr.Src1);
        B.Op(wopI32WrapI64);
        B.Op(wopF32ReinterpretI32);
        B.Op(wopF64PromoteF32);
        StoreReg(B, Instr.Dest);
      end;
    ssaAddFloat: Bin(wopF64Add);
    ssaSubFloat: Bin(wopF64Sub);
    ssaMulFloat: Bin(wopF64Mul);
    ssaDivFloat: Bin(wopF64Div);
    ssaNegFloat: Un(wopF64Neg);

    ssaBitwiseAnd: Bin(wopI64And);
    ssaBitwiseOr:  Bin(wopI64Or);
    ssaBitwiseXor: Bin(wopI64Xor);
    ssaBitwiseNot:
      begin
        LoadReg(B, Instr.Src1);
        B.I64Const(-1);
        B.Op(wopI64Xor);
        StoreReg(B, Instr.Dest);
      end;
    { ⚠️ SHIFT COUNTS PAST THE WIDTH. WASM defines i64.shl/shr as taking the
      count MODULO 64, and that is also what the hardware does - so v Shr 64 is
      v there, and fbc agrees (while WARNING that the shift is out of range,
      i.e. declaring the case out of contract).
      ⛔ Our VM does NOT do that. ArithShr64/LogicalShr64 SATURATE: a count past
      63 gives the sign (-1 or 0) for an arithmetic shift and 0 for a logical
      one, deliberately, "to keep the result defined where the hardware shift
      would not be". Since there is no standard mandating either and the
      reference explicitly warns the case is out of range, OUR semantics are the
      ones to reproduce - so the backend adds the guard the hardware lacks.
      ⚠️ SHL is left as a plain i64.shl because the VM leaves it to FPC, which
      masks: measured, v Shl 64 is v on both sides and on fbc. The asymmetry is
      the interpreter's and is mirrored here rather than tidied away.
      ⚠️ A count of zero or less returns the value untouched, which is the
      helpers' first line and NOT what a masked shift would do. }
    ssaShl:     Bin(wopI64Shl);
    ssaShr:
      begin
        LoadReg(B, Instr.Src1);                            // c <= 0: unchanged
        LoadReg(B, Instr.Src1); B.I64Const(63); B.Op(wopI64ShrS);   // saturated sign
        LoadReg(B, Instr.Src1); LoadReg(B, Instr.Src2); B.Op(wopI64ShrS);
        LoadReg(B, Instr.Src2); B.I64Const(63); B.Op(wopI64GtS);
        B.Op(wopSelect);                                   // count > 63 ? sign : shifted
        LoadReg(B, Instr.Src2); B.I64Const(0); B.Op(wopI64LeS);
        B.Op(wopSelect);
        StoreReg(B, Instr.Dest);
      end;
    ssaShrUInt:
      begin
        LoadReg(B, Instr.Src1);
        B.I64Const(0);
        LoadReg(B, Instr.Src1); LoadReg(B, Instr.Src2); B.Op(wopI64ShrU);
        LoadReg(B, Instr.Src2); B.I64Const(63); B.Op(wopI64GtS);
        B.Op(wopSelect);
        LoadReg(B, Instr.Src2); B.I64Const(0); B.Op(wopI64LeS);
        B.Op(wopSelect);
        StoreReg(B, Instr.Dest);
      end;

    { ⭐ THE MODERN BIT INTRINSICS, AND THE REASON THEY EXIST. These five opcodes are the only way
      the ten WebAssembly instructions i32/i64.{clz,ctz,popcnt,rotl,rotr} become reachable: the
      emitter has always known how to encode them, and no BASIC construct named them.
      One case per operation, the width read from the constant Src3 - so no build can cover the
      64-bit form and quietly leave the 32-bit one out.

      ⚠️ Our registers are i64, so a 32-bit form wraps its operands and extends the result back.
      The extension is SIGNED for the rotates, because a 32-bit value here is what a "Dim As Long"
      holds; the counting forms return 0..32 and cannot tell the two apart.
      ⚠️ A rotate count wrapped to i32 keeps its low 5 bits, which is all that "mod 32" reads, so
      wrapping the count is not an approximation of the interpreter - it is the same number.
      ⭐ And unlike the SHIFTS just above, nothing has to be corrected here: the interpreter's
      BitRotl/BitRotr were written to WebAssembly's rule (count modulo the width), so these lower to
      one instruction instead of a guard. That was the point of choosing that rule. }
    ssaBitClz, ssaBitCtz, ssaBitPopcnt, ssaBitRotl, ssaBitRotr:
      begin
        if Instr.Src3.Kind <> svkConstInt then
          Exit(Fail('a bit intrinsic without a constant width'));
        if (Instr.Src3.ConstInt <> 32) and (Instr.Src3.ConstInt <> 64) then
          Exit(Fail('a bit intrinsic with a width that is neither 32 nor 64'));
        // ⛔ LoadReg assumes a REGISTER and reads an unrelated local when handed a constant, in
        // silence. The SSA always materializes both operands into registers here, so this guard
        // costs nothing today - it is here so that a pass which one day folds a constant into Src1
        // or Src2 stops the compilation instead of emitting a module that runs and is wrong.
        if (Instr.Src1.Kind <> svkRegister) or
           (OpIn(Instr.OpCode, [ssaBitRotl, ssaBitRotr]) and (Instr.Src2.Kind <> svkRegister)) then
          Exit(Fail('a bit intrinsic with a non-register operand'));
        LoadReg(B, Instr.Src1);
        if Instr.Src3.ConstInt = 32 then
        begin
          B.Op(wopI32WrapI64);
          if OpIn(Instr.OpCode, [ssaBitRotl, ssaBitRotr]) then
          begin
            LoadReg(B, Instr.Src2);
            B.Op(wopI32WrapI64);
          end;
          case Instr.OpCode of
            ssaBitClz:    B.Op(wopI32Clz);
            ssaBitCtz:    B.Op(wopI32Ctz);
            ssaBitPopcnt: B.Op(wopI32Popcnt);
            ssaBitRotl:   B.Op(wopI32Rotl);
            ssaBitRotr:   B.Op(wopI32Rotr);
          end;
          B.Op(wopI64ExtendI32S);
        end
        else
        begin
          if OpIn(Instr.OpCode, [ssaBitRotl, ssaBitRotr]) then LoadReg(B, Instr.Src2);
          case Instr.OpCode of
            ssaBitClz:    B.Op(wopI64Clz);
            ssaBitCtz:    B.Op(wopI64Ctz);
            ssaBitPopcnt: B.Op(wopI64Popcnt);
            ssaBitRotl:   B.Op(wopI64Rotl);
            ssaBitRotr:   B.Op(wopI64Rotr);
          end;
        end;
        StoreReg(B, Instr.Dest);
      end;

    ssaCmpEqInt: Cmp(wopI64Eq);
    ssaCmpNeInt: Cmp(wopI64Ne);
    ssaCmpLtInt: Cmp(wopI64LtS);
    ssaCmpGtInt: Cmp(wopI64GtS);
    ssaCmpLeInt: Cmp(wopI64LeS);
    ssaCmpGeInt: Cmp(wopI64GeS);
    ssaCmpLtUInt: Cmp(wopI64LtU);
    ssaCmpGtUInt: Cmp(wopI64GtU);
    ssaCmpLeUInt: Cmp(wopI64LeU);
    ssaCmpGeUInt: Cmp(wopI64GeU);
    ssaCmpEqFloat: if CmpIs32 then Cmp32(wopF32Eq) else Cmp(wopF64Eq);
    ssaCmpNeFloat: if CmpIs32 then Cmp32(wopF32Ne) else Cmp(wopF64Ne);
    ssaCmpLtFloat: if CmpIs32 then Cmp32(wopF32Lt) else Cmp(wopF64Lt);
    ssaCmpGtFloat: if CmpIs32 then Cmp32(wopF32Gt) else Cmp(wopF64Gt);
    ssaCmpLeFloat: if CmpIs32 then Cmp32(wopF32Le) else Cmp(wopF64Le);
    ssaCmpGeFloat: if CmpIs32 then Cmp32(wopF32Ge) else Cmp(wopF64Ge);

    // Src3 = 1 says the source was UNSIGNED, and WASM has the instruction for it - the one case
    // where the distinction costs nothing at all.
    ssaIntToFloat:
      // Src3 is a set of BITS: bit 0 = unsigned source, bit 1 = straight to binary32. All four
      // combinations are real, and WASM has a dedicated instruction for each of them.
      if (Instr.Src3.Kind = svkConstInt) and ((Instr.Src3.ConstInt and 2) <> 0) then
      begin
        // straight to binary32 - one rounding, one instruction - then widened back into the bank.
        // ⭐ From a register that LIVES in an i32 local, WASM has the conversion that takes the 32-bit
        // value directly: it saves the widening the bank would otherwise need, so it is shorter as
        // well as more direct.
        if RegIs32(Instr.Src1) then
        begin
          LoadRegI32(B, Instr.Src1);
          if (Instr.Src3.ConstInt and 1) <> 0 then B.Op(wopF32ConvertI32U)
          else B.Op(wopF32ConvertI32S);
        end
        else
        begin
          LoadReg(B, Instr.Src1);
          if (Instr.Src3.ConstInt and 1) <> 0 then B.Op(wopF32ConvertI64U)
          else B.Op(wopF32ConvertI64S);
        end;
        B.Op(wopF64PromoteF32);
        StoreReg(B, Instr.Dest);
      end
      else if RegIs32(Instr.Src1) then
      begin
        LoadRegI32(B, Instr.Src1);
        if (Instr.Src3.Kind = svkConstInt) and ((Instr.Src3.ConstInt and 1) <> 0) then
          B.Op(wopF64ConvertI32U)
        else B.Op(wopF64ConvertI32S);
        StoreReg(B, Instr.Dest);
      end
      else if (Instr.Src3.Kind = svkConstInt) and ((Instr.Src3.ConstInt and 1) <> 0) then
        Un(wopF64ConvertI64U)
      else Un(wopF64ConvertI64S);
    { The IMPLICIT conversion is dialect-dependent: FreeBASIC rounds half to even (which is exactly
      f64.nearest), Commodore v7 truncates.

      ⛔ And what it does with a value an Int64 cannot hold is NOT saturation, which is what this used
      to emit. The interpreter reaches FPC's Round/Trunc, which on x86-64 is cvttsd2si, and that gives
      the "integer indefinite" - Int64.MinValue - for a NaN and for anything out of range. Saturating
      answered Int64.MAX for +1e30 and 0 for a NaN, so the module and the interpreter disagreed on
      every one of those inputs. Nothing caught it because no corpus program converts a float that far
      out, which is exactly how a silent divergence survives.

      ⭐ Reproducing the interpreter needs a GUARD - is the value in range? - and once the guard is
      there the TRAPPING truncation is the right instruction, because the guard has already excluded
      every input that could trap. The trap is unreachable by construction, which is the only honest
      way to use it: the reason it was avoided was that it would kill the module, and a trap that
      cannot be reached kills nothing. NaN fails both comparisons and takes the same road. }
    ssaFloatToInt, ssaFloatRound:
      begin
        LoadReg(B, Instr.Src1);
        if FModern or (Instr.OpCode = ssaFloatRound) then B.Op(wopF64Nearest);
        B.LocalTee(FFltTmp);
        // ⭐ A destination that LIVES in an i32 local takes the 32-bit truncation, which lands the
        // value where it belongs and saves the wrap StoreReg would otherwise add. The guard is the
        // same one the 64-bit form uses and excludes every input that would trap - and here the range
        // is the 32-bit one, which is what makes trunc_f64_u usable for the unsigned destination:
        // nothing negative and nothing at or above 2^32 reaches it.
        if RegIs32(Instr.Dest) and (Instr.OpCode = ssaFloatToInt) then
        begin
          B.F64Const(-2147483649.0);
          B.Op(wopF64Gt);
          B.LocalGet(FFltTmp);
          B.F64Const(4294967296.0);
          B.Op(wopF64Lt);
          B.Op(wopI32And);
          B.BlockStart(wopIf, WASM_TYPE_I32);
            B.LocalGet(FFltTmp);
            B.F64Const(0.0);
            B.Op(wopF64Lt);
            B.BlockStart(wopIf, WASM_TYPE_I32);
              B.LocalGet(FFltTmp); B.Op(wopI32TruncF64S);   // negative: in [-2^31-1, 0)
            B.Op(wopElse);
              B.LocalGet(FFltTmp); B.Op(wopI32TruncF64U);   // non-negative: up to 2^32-1
            B.EndOp;
          B.Op(wopElse);
            B.I32Const(Integer($80000000));                 // out of range: the integer indefinite
          B.EndOp;
          B.LocalSet(FLocalIdx[FCurRegion][FlatId(Instr.Dest)]);
          Exit(True);
        end;
        if (Instr.Src3.Kind = svkConstInt) and (Instr.Src3.ConstInt = 1) then
        begin
          // Src3 = 1: the DESTINATION is unsigned 64-bit, and [2^63, 2^64) is exactly the range the
          // signed truncation cannot answer. ⛔ i64.trunc_f64_u is NOT the instruction for this: it
          // traps on a negative input, where the interpreter answers the wrapped magnitude (-5 gives
          // 18446744073709551611). What is reproduced here is the interpreter's own rule, which is
          // fbc's measured one - a biased signed truncation for the high half:
          //     d >= 2^63 : trunc_s(d - 2^63) + 2^63     (the add wraps)
          //     otherwise : trunc_s(d)
          // with the "integer indefinite" wherever the truncation cannot answer. Every trap is
          // excluded by the comparison that sits in front of it, so none of them can fire.
          B.F64Const(9223372036854775808.0);
          B.Op(wopF64Ge);
          B.BlockStart(wopIf, WASM_TYPE_I64);
            // ⭐ High half - and this is the ONE place where WASM has the exact operation: inside
            // [2^63, 2^64) i64.trunc_f64_u IS the interpreter's answer, in a single instruction.
            // It traps on a negative input and out of range, which is why it cannot be the whole
            // lowering; but here the two comparisons in front of it have already excluded every
            // input it would trap on, so the trap is unreachable by construction - the same argument
            // that brought i64.trunc_f64_s back on merit.
            // It replaces a bias-subtract, a signed truncation and a wrapping add: shorter code that
            // happens to be one more instruction of the core set, which is the right order of those
            // two facts.
            B.LocalGet(FFltTmp);
            B.F64Const(18446744073709551616.0);     // 2^64, exactly representable
            B.Op(wopF64Lt);
            B.BlockStart(wopIf, WASM_TYPE_I64);
              B.LocalGet(FFltTmp);
              B.Op(wopI64TruncF64U);
            B.Op(wopElse);
              // 2^64 and above, +inf included: the interpreter's bias arithmetic wraps to ZERO here
              // (the indefinite plus 2^63), not to the indefinite. MEASURED against fbc, which gives
              // 0 for 2^64, 1e20, 1e30 and +inf alike.
              B.I64Const(0);
            B.EndOp;
          B.Op(wopElse);
            // Low half - and a NaN reaches here too, both of its comparisons being false, which is
            // how it arrives at the indefinite without a single explicit NaN test.
            B.LocalGet(FFltTmp);
            B.F64Const(-9223372036854775808.0);
            B.Op(wopF64Ge);
            B.BlockStart(wopIf, WASM_TYPE_I64);
              B.LocalGet(FFltTmp);
              B.Op(wopI64TruncF64S);
            B.Op(wopElse);
              B.I64Const(Int64($8000000000000000));
            B.EndOp;
          B.EndOp;
        end
        else
        begin
          B.F64Const(-9223372036854775808.0);
          B.Op(wopF64Ge);
          B.LocalGet(FFltTmp);
          B.F64Const(9223372036854775808.0);
          B.Op(wopF64Lt);
          B.Op(wopI32And);
          B.BlockStart(wopIf, WASM_TYPE_I64);   // an `if` that yields an i64
            B.LocalGet(FFltTmp);
            B.Op(wopI64TruncF64S);           // in range: the trap here cannot fire
          B.Op(wopElse);
            B.I64Const(Int64($8000000000000000));   // cvttsd2si's answer for everything else
          B.EndOp;
        end;
        StoreReg(B, Instr.Dest);
      end;

    ssaMathSqr: Un(wopF64Sqrt);
    { LINE (x1,y1)-(x2,y2), colour [,B|BF]. ⚠️ x2, the colour and the flag do
      NOT travel in Src: they are PhiSources, which is how the SSA carries
      operands past the three slots - so this reads them from ExtraOperands, and
      a walk that only looked at Src1..Src3 would see a line with no colour.
      Flag: 0 = line, 1 = box outline, 2 = filled box, +4 = "no start given",
      which means the current graphics point. }
    ssaGfxLine:
      begin
        Extras := ExtraOperands(Instr);
        if Length(Extras) < 3 then
          Exit(Fail('ssaGfxLine without its y2, colour and flag operands'));
        if Extras[2].Kind <> svkConstInt then
          Exit(Fail('ssaGfxLine without a constant shape flag'));
        n := Integer(Extras[2].ConstInt);
        if (n and 4) <> 0 then
        begin
          B.GlobalGet(FPenX);
          B.GlobalGet(FPenY);
        end
        else
        begin
          if not LoadInt32(B, Instr.Src1) then Exit(False);
          if not LoadInt32(B, Instr.Src2) then Exit(False);
        end;
        if not LoadInt32(B, Instr.Src3) then Exit(False);
        if not LoadInt32(B, Extras[0]) then Exit(False);
        if not LoadInt32(B, Extras[1]) then Exit(False);
        B.I32Const(n and 3);
        B.Call(FGfxLineFunc);
        // the end point becomes the current graphics point
        if not LoadInt32(B, Instr.Src3) then Exit(False);
        B.GlobalSet(FPenX);
        if not LoadInt32(B, Extras[0]) then Exit(False);
        B.GlobalSet(FPenY);
      end;

    { RGBA(r,g,b,a) packs to A<<24 | R<<16 | G<<8 | B, which is the word the
      framebuffer holds - measured: PSet with &HFF778899 leaves FF778899 there.
      ⚠️ The alpha rides in PhiSources[0], the fourth argument having nowhere
      else to go. }
    ssaGraphicRGBA:
      begin
        Extras := ExtraOperands(Instr);
        if Length(Extras) < 1 then
          Exit(Fail('ssaGraphicRGBA without its alpha operand'));
        LoadReg(B, Extras[0]);  B.I64Const($FF); B.Op(wopI64And);
          B.I64Const(24); B.Op(wopI64Shl);
        LoadReg(B, Instr.Src1); B.I64Const($FF); B.Op(wopI64And);
          B.I64Const(16); B.Op(wopI64Shl); B.Op(wopI64Or);
        LoadReg(B, Instr.Src2); B.I64Const($FF); B.Op(wopI64And);
          B.I64Const(8); B.Op(wopI64Shl); B.Op(wopI64Or);
        LoadReg(B, Instr.Src3); B.I64Const($FF); B.Op(wopI64And); B.Op(wopI64Or);
        StoreReg(B, Instr.Dest);
      end;

    ssaGfxPset:
      begin
        Extras := ExtraOperands(Instr);
        if not LoadInt32(B, Instr.Src1) then Exit(False);
        B.LocalTee(FGfxP);
        if not LoadInt32(B, Instr.Src2) then Exit(False);
        B.LocalTee(FGfxN);
        if Length(Extras) >= 1 then
          begin if not LoadInt32(B, Extras[0]) then Exit(False); end
        else
          begin if not LoadInt32(B, Instr.Src3) then Exit(False); end;
        B.Call(FGfxPsetFunc);
        B.LocalGet(FGfxP); B.GlobalSet(FPenX);
        B.LocalGet(FGfxN); B.GlobalSet(FPenY);
      end;

    { POINT(x, y) reads a pixel back. ⚠️ Out of bounds it answers -1, which is
      what the interpreter does rather than trapping - a query is allowed to say
      "nothing there". }
    ssaGfxPoint:
      begin
        if not LoadInt32(B, Instr.Src1) then Exit(False);
        B.LocalSet(FGfxP);
        if not LoadInt32(B, Instr.Src2) then Exit(False);
        B.LocalSet(FGfxN);
        B.LocalGet(FGfxP); B.I32Const(0); B.Op(wopI32GeS);
        B.LocalGet(FGfxN); B.I32Const(0); B.Op(wopI32GeS); B.Op(wopI32And);
        B.LocalGet(FGfxP); B.GlobalGet(FScrW); B.Op(wopI32LtS); B.Op(wopI32And);
        B.LocalGet(FGfxN); B.GlobalGet(FScrH); B.Op(wopI32LtS); B.Op(wopI32And);
        B.BlockStart(wopIf, WASM_TYPE_I64);
          B.GlobalGet(FFbBase);
          B.LocalGet(FGfxN); B.GlobalGet(FScrW); B.Op(wopI32Mul);
          B.LocalGet(FGfxP); B.Op(wopI32Add);
          B.I32Const(4); B.Op(wopI32Mul);
          B.Op(wopI32Add);
          B.OpMem(wopI32Load, 2, 0);
          { ⛔ UNSIGNED, and it was signed until 9 Aug: a pixel is a 32-bit
            COLOUR, and the interpreter widens it as one (GetPixel returns a
            UInt32, which Int64() zero-extends). Sign-extending made
            POINT(1,1) on an opaque red - $FF0000FF - come back as -16776961
            where sb says 4278190335. Every colour with the alpha byte set, so
            every ordinary one, and nothing in the corpus was reading a pixel
            back until m162_rgb was measured. }
          B.Op(wopI64ExtendI32U);
        B.Op(wopElse);
          { Out of bounds is &hFFFFFFFF. It was ZERO here, faithfully copying the
            interpreter - which was itself wrong: fbc answers -1 as a ULONG for a
            point outside the current VIEW, and outside the surface as well, so
            the two idioms that use POINT as a bounds test both read as a
            legitimate black. Corrected on both sides on 23 Aug; the interpreter
            also tests the VIEW rectangle, which this backend has no viewport for
            yet, so only the surface bound is emitted. }
          B.I64Const(4294967295);
        B.EndOp;
        StoreReg(B, Instr.Dest);
      end;

    ssaMathAbs: Un(wopF64Abs);

    { ⚠️ ONE ULP AWAY FROM THE INTERPRETER, sometimes. WASM has no transcendental
      instructions, so these are the host's, and the host's are not FPC's:
      measured over 24 values, 23 agree to 17 digits and Sin(2.0) differs in the
      last bit. Where the value only reaches the output that is invisible; where
      it feeds back into a program's own geometry - a raycaster's camera angle -
      it spreads over the whole frame. Declared, not hidden. }
    { The hyperbolics the host also provides. Separate from the twelve below only in HOW they are
      imported - one at a time, when used - never in what they mean. }
    { ATAN2 takes two arguments, which is why it cannot ride the unary import table. }
    ssaMathAtan2:
      begin
        LoadReg(B, Instr.Src1);
        LoadReg(B, Instr.Src2);
        B.Call(FAtan2Func);
        StoreReg(B, Instr.Dest);
      end;

    ssaMathTanh, ssaMathAsinh, ssaMathAcosh, ssaMathAtanh:
      begin
        LoadReg(B, Instr.Src1);
        case Instr.OpCode of
          ssaMathTanh:  B.Call(FXTrigFunc[0]);
          ssaMathAsinh: B.Call(FXTrigFunc[1]);
          ssaMathAcosh: B.Call(FXTrigFunc[2]);
        else            B.Call(FXTrigFunc[3]);
        end;
        StoreReg(B, Instr.Dest);
      end;

    { SGN is three comparisons and no call: 1 above zero, -1 below, 0 otherwise - which is also what
      the interpreter answers for a NaN (neither comparison holds) and for a negative zero. }
    ssaMathSgn:
      begin
        LoadReg(B, Instr.Src1);
        B.LocalTee(FFltTmp);
        B.F64Const(0.0);
        B.Op(wopF64Gt);
        B.BlockStart(wopIf, WASM_TYPE_F64);
          B.F64Const(1.0);
        B.Op(wopElse);
          B.LocalGet(FFltTmp);
          B.F64Const(0.0);
          B.Op(wopF64Lt);
          B.BlockStart(wopIf, WASM_TYPE_F64);
            B.F64Const(-1.0);
          B.Op(wopElse);
            B.F64Const(0.0);
          B.EndOp;
        B.EndOp;
        StoreReg(B, Instr.Dest);
      end;

    { FRAC is x - Int(x), and the sign of a zero result is NOT simply x's - MEASURED against the
      interpreter, which is the oracle here:
          Frac(-0.0) = -0      but      Frac(-5.0) = +0
      ⭐ One rule produces both, and it is the one the memory of this project had already written down
      without anyone being able to spend a register on it: System.Int returns PLUS zero for -0.0. So
      Frac(-0.0) is -0.0 - (+0.0) = -0.0, while Frac(-5.0) is -5.0 - (-5.0) = +0.0.
      ⛔ f64.trunc does NOT do that - IEEE keeps the sign, so trunc(-0.0) is -0.0 - which is why the
      truncation is nudged with "+ 0.0": that turns a negative zero into a positive one and leaves
      every other value exactly as it was.
      ⚠️ An earlier version used copysign to give the result x's sign in every case. It is the obvious
      rule, it is wrong, and only Frac(-5.0) shows it. }
    ssaMathFrac:
      begin
        LoadReg(B, Instr.Src1);
        B.LocalTee(FFltTmp);
        B.LocalGet(FFltTmp);
        B.Op(wopF64Trunc);
        B.F64Const(0.0);
        B.Op(wopF64Add);          // Int(x): the truncation, with a negative zero made positive
        B.Op(wopF64Sub);          // x - Int(x)
        StoreReg(B, Instr.Dest);
      end;

    ssaMathSin, ssaMathCos, ssaMathTan, ssaMathAtn, ssaMathExp,
    ssaMathLog, ssaMathLog10, ssaMathLog2, ssaMathAsin, ssaMathAcos,
    ssaMathSinh, ssaMathCosh:
      begin
        LoadReg(B, Instr.Src1);
        case Instr.OpCode of
          ssaMathSin:   B.Call(FTrigFunc[0]);
          ssaMathCos:   B.Call(FTrigFunc[1]);
          ssaMathTan:   B.Call(FTrigFunc[2]);
          ssaMathAtn:   B.Call(FTrigFunc[3]);
          ssaMathExp:   B.Call(FTrigFunc[4]);
          ssaMathLog:   B.Call(FTrigFunc[5]);
          ssaMathLog10: B.Call(FTrigFunc[6]);
          ssaMathLog2:  B.Call(FTrigFunc[7]);
          ssaMathAsin:  B.Call(FTrigFunc[8]);
          ssaMathAcos:  B.Call(FTrigFunc[9]);
          ssaMathSinh:  B.Call(FTrigFunc[10]);
        else
          B.Call(FTrigFunc[11]);                   // cosh
        end;
        StoreReg(B, Instr.Dest);
      end;

    { NOW is the serial date the host hands back; TIMER is the seconds elapsed
      in the current day, which is its fractional part scaled - the arithmetic
      the interpreter does, kept on this side so the two cannot drift apart on
      anything except the clock reading itself. }
    ssaDateNow:
      begin
        B.Call(FNowFunc);
        if (Instr.Src3.Kind = svkConstInt) and (Instr.Src3.ConstInt = 1) then
        begin
          // TIMER: Frac(v) * 86400, and Frac is v - Trunc(v) as FPC computes it
          B.LocalTee(FFltTmp);
          B.LocalGet(FFltTmp);
          B.Op(wopF64Trunc);
          B.Op(wopF64Sub);
          B.F64Const(86400);
          B.Op(wopF64Mul);
        end;
        StoreReg(B, Instr.Dest);
      end;

    { ⚠️ LOCATE IS A NO-OP HERE, and it is a FAITHFUL one rather than a gap:
      headless sb emits not one byte for it (checked - "Locate 5,10" between two
      PRINTs leaves the output exactly "AAA\r\nBBB\r\n"). It moves a text cursor
      that stdout does not reflect, and stdout is what the oracle compares.
      ⛔ The day the module gets a real console - a canvas with a text grid -
      this stops being faithful and has to move that cursor. }
    ssaConLocate: ;
    ssaMathInt: Un(wopF64Floor);
    { ⭐ Fix(-0.0) is +0, not -0 - ASKED of fbc rather than reasoned about, and
      sb agrees with it. The interpreter loses the sign because its Fix goes
      through an integer; f64.trunc keeps it, so the sign has to be dropped
      here. Adding +0.0 is exactly that rule and nothing else: IEEE says
      (-0) + (+0) = +0, and x + 0 is x for every other value.
      ⚠️ And the rule is NARROWER than it first looks - my first attempt got it
      wrong by being too broad. FixDouble reads:
          Result := Int(X);
          if (Result = 0) and (X <> 0) and (sign bit of X) then Result := -Result
      so Fix(-0.5) IS -0 (the sign is put back deliberately) while Fix(-0.0) is
      +0, because "X <> 0" is false for a negative zero. ⇒ only a ZERO INPUT
      needs its sign dropped; f64.trunc is already right everywhere else.
      ⚠️ Int(-0.0) keeps the sign, so none of this applies to it. }
    ssaMathFix:
      begin
        B.F64Const(0);
        LoadReg(B, Instr.Src1); B.Op(wopF64Trunc);
        LoadReg(B, Instr.Src1); B.F64Const(0); B.Op(wopF64Eq);
        B.Op(wopSelect);
        StoreReg(B, Instr.Dest);
      end;

    ssaXferStoreInt, ssaXferStoreFloat, ssaXferStoreString:
      begin
        case Instr.OpCode of
          ssaXferStoreFloat:  RT := srtFloat;
          ssaXferStoreString: RT := srtString;
        else
          RT := srtInt;
        end;
        Slot := Integer(Instr.Src3.ConstInt);
        LoadReg(B, Instr.Src1);
        if Slot = WASM_XFER_RESULT_SLOT then B.LocalSet(FResultTmp[RT])
        else if IsSharedSlot(Slot) then B.GlobalSet(FXferGlobal[RT][Slot])
        else B.LocalSet(FSlotBase[RT] + LongWord(Slot));
      end;

    ssaXferLoadInt, ssaXferLoadFloat, ssaXferLoadString:
      begin
        case Instr.OpCode of
          ssaXferLoadFloat:  RT := srtFloat;
          ssaXferLoadString: RT := srtString;
        else
          RT := srtInt;
        end;
        Slot := Integer(Instr.Src3.ConstInt);
        if Slot = WASM_XFER_RESULT_SLOT then
          B.LocalGet(FResultTmp[RT])          // the callee's result, parked after the call
        else if IsSharedSlot(Slot) then
          B.GlobalGet(FXferGlobal[RT][Slot])  // module-global storage, survives every call
        else
          B.LocalGet(FSlotBase[RT] + LongWord(Slot));
        StoreReg(B, Instr.Dest);
      end;
  else
    // ⛔ Two different refusals, and they must not read the same. "yet" is a promise, and it is the
    // wrong one for something the platform does not have.
    if PlatformRefusal(Instr.OpCode) <> '' then
      Exit(Fail(Format('%s cannot be compiled to WebAssembly (line %d): %s',
                       [OpName(Instr.OpCode), Instr.SourceLine, PlatformRefusal(Instr.OpCode)])))
    else
      Exit(Fail(Format('%s is not covered by the WASM backend yet (line %d)',
                       [OpName(Instr.OpCode), Instr.SourceLine])));
  end;
end;


procedure TWasmBackend.PushOutSlots(B: TWasmBuf; R: Integer);
{ At a return: the function's own value is already on the stack, and every
  argument slot it copies back follows, in ascending (bank, slot) order. }
var
  RT: TSSARegisterType;
  n: Integer;
begin
  if R = 0 then Exit;
  for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
    for n := 0 to Length(FOutSlot[R][RT]) - 1 do
      if FOutSlot[R][RT][n] then
        B.LocalGet(FSlotBase[RT] + LongWord(n));
end;

procedure TWasmBackend.PopOutSlots(B: TWasmBuf; Callee: Integer);
{ After a call: the results sit on the stack in declaration order, so the LAST
  one is on top and they come off in reverse. Each lands in the CALLER's slot
  local, which is what makes the callee's write visible - the whole point. }
var
  RT: TSSARegisterType;
  n, bk: Integer;      // "b" would collide with the buffer: Pascal ignores case
begin
  for bk := Ord(High(TSSARegisterType)) downto 0 do
  begin
    RT := TSSARegisterType(bk);
    for n := Length(FOutSlot[Callee][RT]) - 1 downto 0 do
      if FOutSlot[Callee][RT][n] then
        B.LocalSet(FSlotBase[RT] + LongWord(n));
  end;
end;

function TWasmBackend.EmitRegion(R: Integer): Boolean;
var
  D: TWasmDispatch;
  Blk: TSSABasicBlock;
  Instr: TSSAInstruction;
  Body, B: TWasmBuf;
  i, j, k, First, Last, N, Target, P: Integer;
  RT: TSSARegisterType;
  Locals: TWasmValTypeArray;
  Terminated: Boolean;
  SkipNext, SkipTwo, F32Un, I32Un, NeedF32: Boolean;
  F32Op, I32Op: Byte;
  NextI: TSSAInstruction;
  CalleeRegion: Integer;
  FalseTarget: Integer;
  NextInstr, ThirdI, TruncI: TSSAInstruction;

  procedure PushArgs(Callee: Integer);
  // The arguments are already in this region's slot locals, put there by the
  // ssaXferStore* that staged them; WASM just wants them on the stack in
  // signature order.
  var
    Bank: TSSARegisterType;
    s: Integer;
  begin
    for Bank := Low(TSSARegisterType) to High(TSSARegisterType) do
      for s := 0 to FParamCount[Callee][Bank] - 1 do
        if s < FSlotCount[R][Bank] then
          B.LocalGet(FSlotBase[Bank] + LongWord(s))
        else
          case Bank of                       // a parameter this caller never staged
            srtFloat:  B.F64Const(0);
            srtString: B.I32Const(0);
          else
            B.I64Const(0);
          end;
  end;

  procedure PushArgsInd;
  // The same, against the SHARED signature: an indirect call has no callee to
  // ask, which is the whole reason that signature exists.
  var
    Bank: TSSARegisterType;
    s: Integer;
  begin
    for Bank := Low(TSSARegisterType) to High(TSSARegisterType) do
      for s := 0 to FIndParam[Bank] - 1 do
        if s < FSlotCount[R][Bank] then
          B.LocalGet(FSlotBase[Bank] + LongWord(s))
        else
          case Bank of
            srtFloat:  B.F64Const(0);
            srtString: B.I32Const(0);
          else
            B.I64Const(0);
          end;
  end;

begin
  First := FRegionFirst[R];
  Last := FRegionLast[R];
  N := Last - First + 1;

  // locals: parameters, then the dispatch state, the three result temporaries,
  // and finally one per register this region owns
  P := FParamCount[R][srtInt] + FParamCount[R][srtFloat] + FParamCount[R][srtString];
  FStateLocal := LongWord(P);
  FResultTmp[srtInt] := LongWord(P + 1);
  FResultTmp[srtFloat] := LongWord(P + 2);
  FResultTmp[srtString] := LongWord(P + 3);
  FRawTmp := LongWord(P + 4);
  FGfxP := LongWord(P + 5);
  FGfxN := LongWord(P + 6);
  FArrTmp := LongWord(P + 7);
  FRecTmp := LongWord(P + 8);
  FFltTmp := LongWord(P + 9);
  NeedF32 := RegionNeedsF32Tmp(R);
  FF32Tmp := LongWord(P + 10);
  if NeedF32 then SetLength(Locals, 11) else SetLength(Locals, 10);
  Locals[0] := wvtI32;                       // dispatch state
  Locals[1] := wvtI64; Locals[2] := wvtF64; Locals[3] := wvtI32;
  Locals[4] := wvtI64;                       // raw pointer being decoded
  Locals[5] := wvtI32; Locals[6] := wvtI32;  // ScreenRes fill cursor + counter
  Locals[7] := wvtI32;                       // DIM's running element product
  Locals[8] := wvtI32;                       // a record handle being addressed
  // Locals[9] is FFltTmp (f64); Locals[10] is the f32 scratch - see the trunc_f32 fusion.
  Locals[9] := wvtF64;                       // f64 scratch
  if NeedF32 then Locals[10] := wvtF32;      // f32 scratch, only where the fusion can use it
  // one local per transfer slot this region mentions
  if NeedF32 then k := P + 11 else k := P + 10;
  for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
  begin
    FSlotBase[RT] := LongWord(k);
    for i := 0 to FSlotCount[R][RT] - 1 do
    begin
      SetLength(Locals, Length(Locals) + 1);
      Locals[High(Locals)] := BankType[RT];
      Inc(k);
    end;
  end;
  { One save area and one snapshot per ssaArrayBind THIS REGION contains, sized
    from the placeholder's own dimension count, laid out in the order the binds
    are emitted. A pre-pass, because the local list has to be complete before
    the first byte of the body is written. }
  SetLength(FBindLocal, 0);
  SetLength(FBindWords, 0);
  SetLength(FRedimLocal, 0);
  SetLength(FIdxLocal, 0);
  for i := First to Last do
  begin
    Blk := FProg.Blocks[i];
    for j := 0 to Blk.Instructions.Count - 1 do
    begin
      Instr := TSSAInstruction(Blk.Instructions[j]);
      { One i32 per REDIM bound push, same pre-pass and same reason: the local
        list has to be complete before the first byte of the body. }
      if Instr.OpCode = ssaArrayRedimPush then
      begin
        SetLength(FRedimLocal, Length(FRedimLocal) + 1);
        FRedimLocal[High(FRedimLocal)] := LongWord(k);
        SetLength(Locals, Length(Locals) + 1);
        Locals[High(Locals)] := wvtI32;
        Inc(k);
        Continue;
      end;
      { And one i64 per runtime index push. i64 because an index IS one: the
        array helpers take it that way, and the row-major product is computed at
        that width so a large array cannot overflow the arithmetic that finds
        its element. }
      if Instr.OpCode = ssaArrayIdxPush then
      begin
        SetLength(FIdxLocal, Length(FIdxLocal) + 1);
        FIdxLocal[High(FIdxLocal)] := LongWord(k);
        SetLength(Locals, Length(Locals) + 1);
        Locals[High(Locals)] := wvtI64;
        Inc(k);
        Continue;
      end;
      if Instr.OpCode <> ssaArrayBind then Continue;
      if (Instr.Src1.Kind <> svkArrayRef) or (Instr.Src1.ArrayIndex < 0) or
         (Instr.Src1.ArrayIndex >= FProg.GetArrayCount) then
        Exit(Fail('ssaArrayBind without a valid array reference'));
      N := (16 + 8 * FProg.GetArray(Instr.Src1.ArrayIndex).DimCount) div 4;
      SetLength(FBindLocal, Length(FBindLocal) + 1);
      SetLength(FBindWords, Length(FBindWords) + 1);
      FBindLocal[High(FBindLocal)] := LongWord(k);
      FBindWords[High(FBindWords)] := N;
      for Target := 0 to 2 * N - 1 do          // saved, then snapshot
      begin
        SetLength(Locals, Length(Locals) + 1);
        Locals[High(Locals)] := wvtI32;
        Inc(k);
      end;
    end;
  end;
  { The row-major stride, and ONLY when this region resolves a runtime index.
    ⚠️ Allocated unconditionally at first, which silently changed every module in
    the corpus - including a published demo artifact - for a local that nearly
    nothing uses. A local costs a byte in the table and a shifted index for
    everything after it: "always" is not free. }
  if Length(FIdxLocal) > 0 then
  begin
    FIdxScratch := LongWord(k);
    SetLength(Locals, Length(Locals) + 1);
    Locals[High(Locals)] := wvtI64;
    Inc(k);
  end;
  { The descriptor being worked on: needed by a REDIM (either spelling) and by
    every member-array access. Conditional for the same reason as the one above
    - a local that "always" exists changes every module in the corpus. }
  FHasDescTmp := False;
  for i := First to Last do
  begin
    Blk := FProg.Blocks[i];
    for j := 0 to Blk.Instructions.Count - 1 do
      if OpIn(TSSAInstruction(Blk.Instructions[j]).OpCode,
              [ssaArrayRedimN, ssaMemberArrayRedim, ssaRecordNewArrayInd,
               ssaArrayLoadIndInt, ssaArrayLoadIndFloat, ssaArrayLoadIndString,
               ssaArrayStoreIndInt, ssaArrayStoreIndFloat, ssaArrayStoreIndString,
               ssaArrayLBoundInd, ssaArrayUBoundInd, ssaArrayIdxResolveInd,
               ssaStrMidAssign]) then
      begin
        FHasDescTmp := True;
        Break;
      end;
    if FHasDescTmp then Break;
  end;
  if FHasDescTmp then
  begin
    FDescTmp := LongWord(k);
    SetLength(Locals, Length(Locals) + 1);
    Locals[High(Locals)] := wvtI32;
    Inc(k);
  end;
  N := Last - First + 1;                       // restored: the pre-pass reused it

  // Every register this region touches and that is not a global gets a local
  // HERE - a register two regions use for unrelated values needs its own local
  // in each, which is the whole point of not making it a global.
  SetLength(FLocalIdx[R], FFlatCount);
  for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
    for i := 0 to FMaxReg[RT] do
    begin
      j := FBankBase[RT] + i;
      if FRegionUses[R][j] and (not FIsGlobal[j]) then
      begin
        FLocalIdx[R][j] := LongWord(k);
        SetLength(Locals, Length(Locals) + 1);
        // ⭐ A register every definition of which is a 32-bit narrowing lives in an i32 local. The
        // widening/wrapping at the boundaries is LoadReg's and StoreReg's job, so nothing else in the
        // backend has to know - and inside a chain of 32-bit operations there are no boundaries.
        if FIs32[j] then Locals[High(Locals)] := wvtI32
        else Locals[High(Locals)] := BankType[RT];
        Inc(k);
      end;
    end;
  FCurRegion := R;
  FBindTop := 0;
  FBindSeq := 0;
  SetLength(FBindStack, 0);
  FRedimSeq := 0;
  SetLength(FRedimPend, 0);
  FIdxSeq := 0;
  SetLength(FIdxPend, 0);

  D := TWasmDispatch.Create(N, FStateLocal);
  Body := TWasmBuf.Create;
  try
    // Prologue: copy the parameters into the slot locals the body reads.
    j := 0;
    for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
      for i := 0 to FParamCount[R][RT] - 1 do
      begin
        Body.LocalGet(LongWord(j));
        Body.LocalSet(FSlotBase[RT] + LongWord(i));
        Inc(j);
      end;

    for i := 0 to N - 1 do
    begin
      Blk := FProg.Blocks[First + i];
      B := D.Body(i);
      Terminated := False;
      SkipNext := False;
      SkipTwo := False;
      for j := 0 to Blk.Instructions.Count - 1 do
      begin
        if SkipNext then begin SkipNext := False; if SkipTwo then begin SkipTwo := False; SkipNext := True; end; Continue; end;
        Instr := TSSAInstruction(Blk.Instructions[j]);
        // f32: this operation and the narrowing that consumes it are ONE f32 instruction. See the
        // block above FloatRegUses for why the operands may be demoted without proving anything.
        F32Op := 0;
        I32Op := 0;
        if j + 1 < Blk.Instructions.Count then
        begin
          NextI := TSSAInstruction(Blk.Instructions[j + 1]);
          if (NextI.OpCode = ssaNarrowSingle) and
             (Instr.Dest.Kind = svkRegister) and (Instr.Dest.RegType = srtFloat) and
             (NextI.Src1.Kind = svkRegister) and (NextI.Src1.RegType = srtFloat) and
             (NextI.Src1.RegIndex = Instr.Dest.RegIndex) then
            F32Op := F32FormOf(Instr.OpCode, F32Un);
          if (F32Op <> 0) and (FloatRegUses(First, N, Instr.Dest.RegIndex) <> 1) then F32Op := 0;
        end;
        // ⭐ THREE instructions, not two, when the narrowed result is immediately converted to an
        // integer: the f32 value is truncated where it stands instead of being promoted to f64 first.
        // That promote is the only thing standing between us and i32/i64.trunc_f32, and dropping it is
        // shorter as well as more direct - the conversion of a binary32 is the same number either way,
        // because promoting is exact.
        TruncI := nil;
        if (F32Op <> 0) and (j + 2 < Blk.Instructions.Count) then
        begin
          ThirdI := TSSAInstruction(Blk.Instructions[j + 2]);
          if (ThirdI.OpCode = ssaFloatToInt) and
             (ThirdI.Src1.Kind = svkRegister) and (ThirdI.Src1.RegType = srtFloat) and
             (NextI.Dest.Kind = svkRegister) and
             (ThirdI.Src1.RegIndex = NextI.Dest.RegIndex) and
             (ThirdI.Dest.Kind = svkRegister) and
             (FloatRegUses(First, N, NextI.Dest.RegIndex) = 1) then
            TruncI := ThirdI;
        end;
        if F32Op <> 0 then
        begin
          LoadReg(B, Instr.Src1);
          B.Op(wopF32DemoteF64);
          if not F32Un then
          begin
            LoadReg(B, Instr.Src2);
            B.Op(wopF32DemoteF64);
          end;
          B.Op(F32Op);
          if TruncI <> nil then
          begin
            EmitTruncF32(B, TruncI);
            SkipNext := True; SkipTwo := True;
            Continue;
          end;
          B.Op(wopF64PromoteF32);
          StoreReg(B, NextI.Dest);
          SkipNext := True;                 // the narrowing was just emitted as part of this
          Continue;
        end;
        // The INTEGER twin, and it is the same fusion for the same reason: an INT32 / UINT32 operation
        // and the 32-bit narrowing the SSA puts after it are ONE i32 instruction. Width code 5 widens
        // back signed, 6 unsigned - which is also the only thing that tells the two types apart here.
        I32Op := 0;
        if (j + 1 < Blk.Instructions.Count) and (F32Op = 0) then
        begin
          NextI := TSSAInstruction(Blk.Instructions[j + 1]);
          if (NextI.OpCode = ssaNarrowInt) and (NextI.Src3.Kind = svkConstInt) and
             ((NextI.Src3.ConstInt = 5) or (NextI.Src3.ConstInt = 6)) and
             (Instr.Dest.Kind = svkRegister) and (Instr.Dest.RegType = srtInt) and
             (NextI.Src1.Kind = svkRegister) and (NextI.Src1.RegType = srtInt) and
             (NextI.Src1.RegIndex = Instr.Dest.RegIndex) then
            I32Op := I32FormOf(Instr.OpCode, I32Un);
          // ⛔ Only when the intermediate has exactly ONE reader - the narrowing itself. With another
          // reader the unfused 64-bit value is still wanted, and skipping its store would lose it.
          if (I32Op <> 0) and (IntRegUses(First, N, Instr.Dest.RegIndex) <> 1) then I32Op := 0;
        end;
        if I32Op <> 0 then
        begin
          if I32Un then B.I32Const(0)       // NEG is 0 - x, which is what the unary form means here
          else LoadRegI32(B, Instr.Src1);
          if I32Un then LoadRegI32(B, Instr.Src1)
          else LoadRegI32(B, Instr.Src2);
          B.Op(I32Op);
          StoreRegI32(B, NextI.Dest, NextI.Src3.ConstInt = 5);
          SkipNext := True;
          Continue;
        end;
        case Instr.OpCode of
          ssaJump:
            begin
              Target := BlockOfLabel(Instr.Dest.LabelName) - First;
              D.EmitGotoTerminal(i, Target);
              Terminated := True;
            end;
          ssaJumpIfZero, ssaJumpIfNotZero:
            begin
              Target := BlockOfLabel(Instr.Dest.LabelName) - First;
              { ⛔⛔ THE FALSE SIDE IS NOT ALWAYS THE NEXT BLOCK. It used to be
                hard-coded to i + 1, and that is a MISCOMPILATION whenever a
                block ends with a conditional jump AND an unconditional one
                after it - which is how the SSA writes "if the test fails, go
                somewhere that is not next". Taking the conditional and dropping
                the jump sent the false path to whatever block happened to be
                laid out next, and when that block was the TRUE target both
                sides went there: the test was emitted, evaluated, and had no
                effect at all.
                ⚠️ MEASURED, not imagined: "ReDim Preserve" on an array of UDT
                re-ran the per-element construction guarded by exactly this
                shape, so every element was rebuilt and its data lost - while
                the probe it branched on was read correctly. The guard was
                right, the branch went both ways.
                ⇒ An ssaJump immediately after the conditional NAMES the false
                target; only without one does the block fall through. }
              FalseTarget := -1;
              if j + 1 < Blk.Instructions.Count then
              begin
                NextInstr := TSSAInstruction(Blk.Instructions[j + 1]);
                if (NextInstr.OpCode = ssaJump) and (NextInstr.Dest.Kind = svkLabel) then
                  FalseTarget := BlockOfLabel(NextInstr.Dest.LabelName) - First;
              end;
              if FalseTarget < 0 then
              begin
                if i + 1 >= N then
                  Exit(Fail(Format('a conditional jump in block "%s" has no following block',
                                   [Blk.LabelName])));
                FalseTarget := i + 1;
              end;
              LoadReg(B, Instr.Src1);
              B.Op(wopI64Eqz);               // i32: "the value is zero"
              if Instr.OpCode = ssaJumpIfZero then
                D.EmitBranch(i, Target, FalseTarget)
              else
                D.EmitBranch(i, FalseTarget, Target);
              Terminated := True;
            end;
          ssaCallSub:
            begin
              CalleeRegion := FRegionOf[BlockOfLabel(Instr.Dest.LabelName)];
              PushArgs(CalleeRegion);
              B.Call(FFuncIdx[CalleeRegion]);
              PopOutSlots(B, CalleeRegion);
              if FResultBank[CalleeRegion] >= 0 then
                B.LocalSet(FResultTmp[TSSARegisterType(FResultBank[CalleeRegion])]);
              { The callee may have ENDed the program. Unwind this frame too -
                and the results pushed here are whatever the locals hold, which
                nobody will read: the caller above is about to do the same. }
              if FUsesHalt then
              begin
                B.GlobalGet(FHaltFlag);
                B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
                  EmitReturnValues(B, R);
                  B.Op(wopReturn);
                B.EndOp;
              end;
            end;
          { The same call, with the callee arriving as a VALUE. The arguments go
            up the same way - they are already in this region's slot locals -
            and the table index goes last, which is where call_indirect wants
            it. The type is the shared one every table entry was given.
            ⚠️ No PopOutSlots: an address-taken procedure with byref copy-out was
            refused in BuildSignatures, so there is nothing to pop and nothing
            here has to guess which slots it would have been. }
          ssaCallSubIndirect:
            begin
              if not BankIs(Instr.Src1, srtInt, 'ssaCallSubIndirect target') then
                Exit(False);
              PushArgsInd;
              LoadReg(B, Instr.Src1);
              B.Op(wopI32WrapI64);
              B.CallIndirect(FIndTypeIdx);
              { Every bank a pointer call can answer in, moved from its global
                into the result temporary the ssaXferLoad after this call will
                read. Which one that is belongs to the CALLER, and it already
                knows: the load names its own bank. Copying all of them costs at
                most three instructions and removes the question. }
              for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
                if FIndResUsed[RT] then
                begin
                  B.GlobalGet(FIndResGlobal[RT]);
                  B.LocalSet(FResultTmp[RT]);
                end;
              if FUsesHalt then
              begin
                B.GlobalGet(FHaltFlag);
                B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
                  EmitReturnValues(B, R);
                  B.Op(wopReturn);
                B.EndOp;
              end;
            end;
          ssaReturnSub:
            begin
              EmitReturnValues(B, R);
              B.Op(wopReturn);
              Terminated := True;
            end;
          ssaEnd, ssaStop:
            begin
              // In main this return IS the halt; in a procedure it is only the
              // first frame of one, and the flag carries it the rest of the way.
              if FUsesHalt and (R <> 0) then
              begin
                B.I32Const(1);
                B.GlobalSet(FHaltFlag);
              end;
              EmitReturnValues(B, R);
              B.Op(wopReturn);
              Terminated := True;
            end;
        else
          if not EmitInstr(B, Instr, R) then Exit(False);
        end;
        if Terminated then Break;
      end;
      if not Terminated then
      begin
        if i + 1 < N then D.EmitGotoTerminal(i, i + 1)
        else
        begin
          EmitReturnValues(D.Body(i), R);
          D.Body(i).Op(wopReturn);
        end;
      end;
    end;

    D.Emit(Body, 0);
    { ⛔ A bound pushed and never committed means the pairing this rests on does
      not hold for this program - a push and its commit landed in different
      regions, or control flow got between them. Refuse: the alternative is a
      REDIM built from whichever bounds happened to be pending. }
    if Length(FRedimPend) > 0 then
      Exit(Fail(Format('region "%s" pushes a REDIM bound that no commit consumes',
                       [FRegionName[R]])));
    if Length(FIdxPend) > 0 then
      Exit(Fail(Format('region "%s" pushes an array index that no resolve consumes',
                       [FRegionName[R]])));
    // The region is left only by an explicit return, so anything after it is
    // unreachable - but the validator still wants the function to type-check.
    Body.Op(wopUnreachable);
    FFuncIdx[R] := FModule.AddFunction(FTypeIdx[R], Locals, Body);
    Result := True;
  finally
    Body.Free;
    D.Free;
  end;
end;

function TWasmBackend.Compile: Boolean;
var
  r, i: Integer;
  RT: TSSARegisterType;
  Next: LongWord;
  Init: TWasmBuf;
  TabFuncs: array of LongWord;
begin
  FError := '';
  { ⛔ THE WASM TARGET IS MODERN-ONLY. A project rule, decided 7 Aug 2026, and it
    belongs HERE rather than scattered over the opcodes: the dialect is known at
    compile time, so the boundary can be one refusal that names itself instead of
    a surprise somewhere inside a formatter.
    ⭐ Some arms below still carry their Commodore branch (PRINT's trailing space,
    MID$'s clamping, the ?BAD SUBSCRIPT trap). They are correct and now
    UNREACHABLE, kept because they are the semantics, not a workaround - but
    nothing exercises them any more, so do not read them as tested.
    ⚠️ One consequence worth stating: ssaModFloat is reachable ONLY in Commodore
    (in FreeBASIC, Mod is an integer operator), so its arm is dead code from here
    on. The two CLASSIC guardians in job/tests/bas/wasm are deliberately left in
    place: they must now be REFUSED, and the day one of them compiles, this rule
    has been broken without anyone noticing. }
  if not FModern then
    Exit(Fail('the WASM target supports the FreeBASIC (MODERN) dialect only; ' +
              'this program is Commodore BASIC'));
  { Clamp the digit count to what the workspace holds - and 767 is not an
    arbitrary size: it bounds M x 5^1074, the widest exact expansion a double
    has, so nothing is lost by capping there. "OPTION DIGITS EXACT" arrives as
    MaxInt and becomes exactly this. }
  if FFltDigits < 1 then FFltDigits := 1;
  if FFltDigits > 767 then FFltDigits := 767;
  if not BuildPartition then Exit(False);

  // Imports own the low indices, so they must be declared before the first
  // DEFINITION - and ClassifyRegisters defines globals.
  ScanForPrint;
  FImportCount := 0;
  if FUsesPrint then
  begin
    FWriteFunc := FModule.ImportFunc('env', 'write',
                                     FModule.TypeIndex([wvtI32, wvtI32], []));
    Inc(FImportCount);
  end;
  { ⚠️ THE CLOCK CANNOT COME FROM INSIDE. There is no time in WebAssembly, so
    NOW/TIMER are an import or nothing. The host hands back a serial date in the
    interpreter's own convention - days since 1899-12-30, LOCAL time - so that
    the arithmetic on this side is identical to the interpreter's.
    ⛔ And a program that reads the clock CANNOT match a previous run, natively
    or here: that is a property of the program, not a defect of the backend. }
  if FUsesClock then
  begin
    FNowFunc := FModule.ImportFunc('env', 'now', FModule.TypeIndex([], [wvtF64]));
    Inc(FImportCount);
  end;
  { ⚠️ WASM HAS NO TRANSCENDENTALS. Measured 8 Aug 2026 over 24 values: FPC and
    the host's Math.sin/cos agree to 17 digits on 23 of them and differ by ONE
    ULP on Sin(2.0). ⇒ a program using them can come out one ulp away from the
    native run, which is DECLARED here rather than discovered later.
    🎯 The ideal remains our own implementation used natively TOO, so both sides
    agree by construction - the move that settled float printing - but only if
    it does not cost native performance, which has to be measured first. }
  if FUsesTrig then
  begin
    { ⛔ The ORDER here is the order TRIG_NAME lists them, and the lowering
      indexes that same table - so a name added in one place and not the other
      calls the wrong function with the right type, which validates. }
    for i := 0 to High(TRIG_NAME) do
    begin
      FTrigFunc[i] := FModule.ImportFunc('env', TRIG_NAME[i],
                                         FModule.TypeIndex([wvtF64], [wvtF64]));
      Inc(FImportCount);
    end;
  end;
  for i := 0 to High(XTRIG_NAME) do
    if FUsesXTrig[i] then
    begin
      FXTrigFunc[i] := FModule.ImportFunc('env', XTRIG_NAME[i],
                                          FModule.TypeIndex([wvtF64], [wvtF64]));
      Inc(FImportCount);
    end;
  if FUsesAtan2 then
  begin
    FAtan2Func := FModule.ImportFunc('env', 'atan2',
                                     FModule.TypeIndex([wvtF64, wvtF64], [wvtF64]));
    Inc(FImportCount);
  end;

  ScanForHalt;
  SetLength(FLocalIdx, FProg.Blocks.Count);   // sized by region below
  if not ClassifyRegisters then Exit(False);
  Classify32BitRegisters;
  SetLength(FLocalIdx, FRegionCount);
  if not BuildSignatures then Exit(False);
  if not DetectRecursion then Exit(False);

  if FUsesPrint or FUsesHeap then
  begin
    // Enough pages to hold the literals; the allocator grows it from there. The
    // memory is exported so a differential can read the framebuffer out without
    // the program having to print it.
    FModule.DefineMemory((FHeapBase + 65535) div 65536, 0);
    FModule.DataSegment(CONST_SPACE, PByte(PAnsiChar(' '#10 + '1.#QNAN' + '-1.#IND' + '1.#INF' + '-1.#INF')), 29);
    if Length(FConstBytes) > 0 then
      FModule.DataSegment(STR_CONST_BASE, PByte(PAnsiChar(FConstBytes)),
                          Length(FConstBytes));
    if Length(FArrTabBytes) > 0 then
      FModule.DataSegment(FArrTabAddr, PByte(PAnsiChar(FArrTabBytes)),
                          Length(FArrTabBytes));
    if FUsesDateName then
      FModule.DataSegment(DT_NAMES, PByte(PAnsiChar(BuildDateNameBlob)),
                          Length(BuildDateNameBlob));
    if FUsesBool then
      FModule.DataSegment(BOOL_TRUE, PByte(PAnsiChar('true' + 'false')), 9);
    if FUsesErrName then
      FModule.DataSegment(ERR_NAMES, PByte(PAnsiChar(BuildErrNameBlob)),
                          Length(BuildErrNameBlob));
    FModule.ExportMemory('memory');
  end;
  Init := TWasmBuf.Create;
  try
    if FUsesHalt then
    begin
      Init.I32Const(0);
      FHaltFlag := FModule.DefineGlobal(wvtI32, True, Init);
      Init.Clear;
    end;
    if FUsesCol then
    begin
      Init.I32Const(0);
      FColG := FModule.DefineGlobal(wvtI32, True, Init);
      Init.Clear;
    end;
    { ⭐ The generator's state is ALLOCATED, not carved out of the fixed low
      memory: 624 words is 2496 bytes and there is no gap that size below the
      literals - and moving the literals would change the address of every
      string in every module, the published demo included. A global holds the
      block's address and doubles as the "not seeded yet" flag. }
    if FUsesRnd then
    begin
      Init.I32Const(0);
      FMtBaseG := FModule.DefineGlobal(wvtI32, True, Init);
      Init.Clear;
    end;
    { One per bank a pointer call can return in. This IS the transfer bank's
      result slot, and it is a global for the same reason the shared slots are:
      the storage is the program's, not a frame's. A callee's own nested calls
      cannot clobber it, because the caller reads it immediately on return -
      before anything else can be staged. }
    for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
      if FIndResUsed[RT] then
      begin
        case RT of
          srtFloat:  Init.F64Const(0);
          srtString: Init.I32Const(0);
        else
          Init.I64Const(0);
        end;
        FIndResGlobal[RT] := FModule.DefineGlobal(BankType[RT], True, Init);
        Init.Clear;
      end;
    if FUsesHeap then
    begin
      Init.I32Const(LongInt(FHeapBase));
      FHeapTop := FModule.DefineGlobal(wvtI32, True, Init);
      Init.Clear;
    end;
    if FUsesGfx then
    begin
      Init.I32Const(0); FScrW := FModule.DefineGlobal(wvtI32, True, Init);
      Init.Clear;
      Init.I32Const(0); FScrH := FModule.DefineGlobal(wvtI32, True, Init);
      Init.Clear;
      Init.I32Const(0); FFbBase := FModule.DefineGlobal(wvtI32, True, Init);
      Init.Clear;
      { The CURRENT GRAPHICS POINT, which "LINE -(x2,y2)" reads and every draw
        updates. It is per PROGRAM, not per call, so it is a global - the same
        question that decided the transfer slots. }
      Init.I32Const(0); FPenX := FModule.DefineGlobal(wvtI32, True, Init);
      Init.Clear;
      Init.I32Const(0); FPenY := FModule.DefineGlobal(wvtI32, True, Init);
    end;
  finally
    Init.Free;
  end;
  if FUsesGfx then
  begin
    FModule.ExportGlobal('screen_w', FScrW);
    FModule.ExportGlobal('screen_h', FScrH);
    // ⭐ Where the framebuffer IS, rather than a constant the page has to know:
    // it is bump-allocated now, so its address depends on what else the program
    // holds. A viewer that hardcodes a base would read the wrong bytes the
    // first time a program has both strings and graphics.
    FModule.ExportGlobal('screen_ptr', FFbBase);
  end;

  // Functions have to be numbered before any of them is emitted, because a call
  // names its callee by index and a procedure may be called before it is built.
  for r := 0 to FRegionCount - 1 do
    FFuncIdx[r] := FImportCount + LongWord(r);
  // ⛔ The order here must match the order the Emit*Helpers add them in, and
  // nothing checks it: getting it wrong calls the wrong function with the right
  // types, which validates.
  Next := FImportCount + LongWord(FRegionCount);
  { ⛔ FIRST, because EmitThunks runs immediately after the regions and before
    every helper. Reserved rather than read back from AddFunction: the table's
    elem segment is written before any body exists. }
  SetLength(FThunkIdx, FRegionCount);
  if FIndirect then
    for r := 1 to FRegionCount - 1 do
      if FAddrTaken[r] then
      begin
        FThunkIdx[r] := Next;
        Inc(Next);
      end;
  if FUsesPow then
  begin
    FPowFunc := Next;
    Inc(Next);
  end;
  if FUsesPrint then
  begin
    FPrintIntFunc := Next;
    FPrintUIntFunc := Next + 1;
    FPrintNlFunc := Next + 2;
    Inc(Next, 3);
    if FUsesCol then           // conditional, so sequential and last in the block
    begin
      FEmitFunc := Next;
      Inc(Next);
    end;
  end;
  if FUsesHeap then
  begin
    FAllocFunc := Next;
    Inc(Next);
  end;
  if FUsesStr then
  begin
    FStrNewFunc   := Next;
    FStrCatFunc   := Next + 1;
    FStrSubFunc   := Next + 2;
    FStrCmpFunc   := Next + 3;
    FStrAscFunc   := Next + 4;
    FStrChrFunc   := Next + 5;
    FStrRightFunc := Next + 6;
    FStrMidFunc   := Next + 7;
    FStrFromIntFunc := Next + 8;
    FStrFillFunc    := Next + 9;    // SPACE$ and STRING$: one helper, the char differs
    FStrCaseFunc    := Next + 10;   // UCASE / LCASE: one helper, the direction is a flag
    { ⛔ THE ORDER HERE IS THE ORDER EmitStringHelpers ADDS THEM IN, and nothing checks it: numbering
      trim after instr while emitting it before made every call land one function off - and it was
      caught by the VALIDATOR, not by a wrong answer, only because the signatures happened to differ.
      See the note above about conditional helpers, which is the same hazard. }
    FStrTrimFunc    := Next + 11;   // LTRIM/RTRIM/TRIM: one helper, the sides in a flag
    FStrFindFunc    := Next + 12;   // INSTR / INSTRREV / both Any forms: direction and kind in a flag
    Inc(Next, 13);
    { ⛔ FROM HERE THE NUMBERING IS SEQUENTIAL, not fixed offsets, because these
      are CONDITIONAL: printStr exists only if the program prints, puDigits only
      if it uses PRINT USING. With offsets, a program that had one but not the
      other would slide every later index by one - and calling the wrong
      function with the right types VALIDATES, so nothing would catch it.
      ⚠️ The order here must match the order EmitStringHelpers adds them in. }
    if FUsesPU then
    begin
      FPuDigFunc := Next;
      FPuFmtFunc := Next + 1;
      Inc(Next, 2);
    end;
    if FUsesPrint then
    begin
      FPrintStrFunc := Next;
      Inc(Next);
    end;
    { VAL, last in the string block and sequential like the two above. valInt is
      shared: VAL reads a "&H" prefix through it exactly as VALINT does, which is
      the interpreter's arrangement too (ParseLeadingFloat calls
      ParseLeadingInt64), so the two dialective rules cannot drift apart. }
    if FUsesVal or FUsesValInt then
    begin
      FStrValIntFunc := Next;
      Inc(Next);
    end;
    if FUsesVal then
    begin
      FValBitFunc := Next;
      FStrValFunc := Next + 1;
      Inc(Next, 2);
    end;
    { HEX$/OCT/BIN, after VAL because EmitStringHelpers emits it after
      EmitValHelpers - sequential and conditional, like everything else in this
      tail, for the reason spelled out above it. }
    if FUsesBase then
    begin
      FStrBaseFunc := Next;
      Inc(Next);
    end;
    if FUsesTrimSet then
    begin
      FStrTrimSetFunc := Next;
      Inc(Next);
    end;
    if FUsesPack then
    begin
      FStrPackFunc := Next;
      FStrUnpackFunc := Next + 1;
      Inc(Next, 2);
    end;
    if FUsesWide then
    begin
      FWLenFunc  := Next;
      FWSubFunc  := Next + 1;
      FWEncFunc  := Next + 2;
      FWB2CPFunc := Next + 3;
      FWRepFunc  := Next + 4;
      Inc(Next, 5);
    end;
  end;

  if FUsesArr then
  begin
    // load and store for each of the three banks, in bank order, then the two
    // bound queries - the same order EmitArrayHelpers adds them in
    for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
    begin
      FArrLoad[RT] := Next; FArrStore[RT] := Next + 1;
      Inc(Next, 2);
    end;
    FArrLBoundFunc := Next; FArrUBoundFunc := Next + 1;
    Inc(Next, 2);
  end;
  { ⚠️ Conditional, so it is numbered sequentially and OUTSIDE the array block -
    the same rule the string block's tail follows. It sits here because
    EmitRecordHelpers runs right after EmitArrayHelpers and before the float
    ones; move one and the other has to move with it. }
  if FUsesRecArr then
  begin
    FRecNewArrFunc := Next;
    Inc(Next);
  end;
  { Load then store for each bank, in bank order - the order EmitRefHelpers adds
    them in. Conditional like the two above, and numbered sequentially for the
    same reason. }
  if FUsesPtr then
    for RT := Low(TSSARegisterType) to High(TSSARegisterType) do
    begin
      FRefLoad[RT] := Next; FRefStore[RT] := Next + 1;
      Inc(Next, 2);
    end;

  if FUsesFlt then
  begin
    FFltMulFunc := Next;
    FFltDecFunc := Next + 1;
    FFltPrintFunc := Next + 2;
    FFltOutFunc := Next + 3;      // EmitFloatHelpers adds these two after fltPrint
    Inc(Next, 4);
    if FUsesStrStr then           // conditional, so sequential and last
    begin
      FFltStrFunc := Next;
      Inc(Next);
    end;
  end;
  { ⛔⛔ LAST, because EmitGfxHelpers runs last. Numbering these before the array
    and float helpers - while emitting them after - made every call land two
    functions early: an array store went to the float helper and the module
    failed to validate on the argument types. ⭐ It failed LOUDLY only because
    the signatures happened to differ; with matching types it would have drawn
    into the wrong place in silence, which is what this whole ordering is for.
    ⚠️ These also sit OUTSIDE the string block: a program can draw without ever
    holding a string. }
  if FUsesGfxPrim then
  begin
    FGfxPsetFunc := Next;
    FGfxLineFunc := Next + 1;
    Inc(Next, 2);
  end;
  { The calendar, last and conditional - EmitDateHelpers runs last too. }
  if FUsesDate then
  begin
    FDtSplitFunc := Next;
    FDtCivilFunc := Next + 1;
    FDtDaysFunc  := Next + 2;
    FDtIvalFunc  := Next + 3;
    FDtIncMoFunc := Next + 4;
    FDtDoyFunc   := Next + 5;
    FDtWoyFunc   := Next + 6;
    FDtYMFunc    := Next + 7;
    Inc(Next, 8);
    { Conditional, so sequential and last - the same rule the string block's
      tail follows, and EmitDateHelpers adds it last too. }
    if FUsesDateStr then
    begin
      FDtStrFunc := Next;
      Inc(Next);
    end;
    if FUsesDateParse then
    begin
      FDtFindFunc      := Next;
      FDtFieldsFunc    := Next + 1;
      FDtDatePartFunc  := Next + 2;
      FDtTimePartFunc  := Next + 3;
      FDtParseFunc     := Next + 4;
      Inc(Next, 5);
    end;
  end;
  { ⛔⛔ OUTSIDE the calendar's block, and that is exactly the bug this comment
    exists for: numbered INSIDE it, a program that used RND without a single
    date got an index that was never assigned, so the call landed on whatever
    function happened to sit there. It did not validate, which was the lucky
    half - a wrong index whose signature happens to match draws into the wrong
    function in silence.
    ⚠️ RND needs no calendar, so its numbering cannot live under a calendar
    test. And the ORDER here is the order the driver emits them in. }
  if FUsesRnd then
  begin
    FMtSeedFunc := Next;
    FMtNextFunc := Next + 1;
    Inc(Next, 2);
  end;

  { The function table. ⭐ It holds EVERY region rather than only the
    address-taken ones, and the reason is a defect avoided rather than laziness:
    with a compacted table the value of a function pointer would depend on which
    OTHER procedures had their address taken, so adding a "@f" anywhere would
    renumber pointers everywhere. With the region index as the slot, a pointer
    means the same thing wherever it is built - and the cost is one funcref per
    procedure. Region 0 (main) sits at index 0 and nothing can reach it: its
    address cannot be taken. }
  if FIndirect then
  begin
    SetLength(TabFuncs, FRegionCount);
    for r := 0 to FRegionCount - 1 do
      if FAddrTaken[r] and (r <> 0) then TabFuncs[r] := FThunkIdx[r]
      else TabFuncs[r] := FFuncIdx[r];   // reachable only by a wrong index, and it TRAPS
    FModule.DefineTable(LongWord(FRegionCount), LongWord(FRegionCount));
    FModule.ElemFuncs(0, TabFuncs);
  end;

  for r := 0 to FRegionCount - 1 do
    if not EmitRegion(r) then Exit(False);
  if FIndirect then EmitThunks;
  if FUsesPow then EmitPowHelper;
  if FUsesPrint then EmitPrintHelpers;
  if FUsesHeap then EmitHeapHelpers;
  if FUsesStr then EmitStringHelpers;
  if FUsesArr then EmitArrayHelpers;
  if FUsesRecArr then EmitRecordHelpers;
  if FUsesPtr then EmitRefHelpers;
  if FUsesFlt then EmitFloatHelpers;
  if FUsesGfxPrim then EmitGfxHelpers;
  if FUsesDate then EmitDateHelpers;
  if FUsesDateParse then EmitDateParseHelpers;
  if FUsesRnd then EmitRndHelpers;

  FModule.ExportFunc('main', FFuncIdx[0]);
  for r := 1 to FRegionCount - 1 do
    FModule.ExportFunc(FRegionName[r], FFuncIdx[r]);
  Result := True;
end;

procedure TWasmBackend.SaveToFile(const Path: string);
begin
  FModule.SaveToFile(Path);
end;

end.
