unit SedaiAot;

{ ============================================================================
  SedaiAot - AOT backend on the SSA IR (plan B, job/docs/PIANO_B1_AOT_DESIGN.md).

  Stage S3 (this file, survey only): slice the flat SSA program into function
  regions and classify each against the B1 scalar op set, so we know - before
  writing any codegen - how many real functions the B1 subset covers and what
  the top bail reasons are. Codegen lands in S4.

  A "function region" is a contiguous run of basic blocks: the module body from
  block 0 ('_entry') up to the first 'PROC_' labeled block, then one region per
  'PROC_' label (the IR has no per-procedure structure; the label prefix is the
  only delimiter). Instruction ordinals are counted over EVERY instruction of
  EVERY block in program order - the exact walk TBytecodeCompiler.Compile uses
  to build the SSA->PC map, so ordinals index that map directly.

  Diagnostics: set AOT_DIAG=1 to print one line per region (NATIVE / BAIL with
  the culprit op) plus a summary; output goes to stderr so program output
  stays byte-comparable.
  ============================================================================ }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SedaiSSATypes, SedaiBytecodeTypes, SedaiX86Emitter;

type
  // Per-call context of a compiled function: base pointers of the ACTIVE execution
  // context's transfer banks. Built by the interpreter once per Run and passed on
  // every call - never baked into the code (a worker thread must see its own banks).
  TAotCtx = record
    XferInt: PInt64;     // offset 0
    XferFloat: PInt64;   // offset 8
    ArrDesc: PInt64;     // offset 16: @FJitArrDesc[0] (4x Int64/array: IntData, FloatData, Count, LBound),
                         // refreshed per call after the dirty rebuild - same table the loop JIT uses
    // C3 runtime-helper triple. Emitted code loads all three from here, never from a baked
    // constant: the VM instance is shared but the CONTEXT is per-worker, so a thread must
    // see its own. Offsets are part of the codegen contract (AOTCTX_* below).
    ExecOne: Pointer;    // offset 24: @AotExecOne (the one-instruction helper)
    VMSelf: Pointer;     // offset 32: the TBytecodeVM instance
    CtxObj: Pointer;     // offset 40: the ACTIVE TExecutionContext
    // C5 native string lowering. The string bank base and the leaf primitives the emitted
    // code calls directly instead of routing bcCmp*String etc. through AotExecOne. StrRegs is
    // stable for the run (the banks are sized once by LoadProgram, never reallocated - the same
    // reason rbx/rsi can be kept), so it is set once with the others and never refreshed.
    StrRegs: Pointer;    // offset 48: @Ctx.StringRegs[0], base of the managed-string bank
    StrCmp: Pointer;     // offset 56: @AotStrCmp (a, b, kind) -> 1/0
    StrAssign: Pointer;  // offset 64: @AotStrAssign (dstSlot, srcVal) - copy
    StrLoadConst: Pointer; // offset 72: @AotStrLoadConst (dstSlot, VMSelf, imm)
    StrConcat: Pointer;  // offset 80: @AotStrConcat (dstSlot, aVal, bVal)
    StrLen: Pointer;     // offset 88: @AotStrLen (sVal) -> length
    // B3 native call site: the Pascal primitive that replicates bcCallSub around a call to
    // the callee's compiled function (FramePush + return-PC push, the compiled call, then
    // FramePop + pop when the callee reached its bcReturnSub). Reached through the ctx like
    // every other primitive - never a baked address.
    CallSub: Pointer;    // offset 96: @AotCallSub (ctx, calleeEntryPC, bcCallSubPC) -> PC/sentinel
    // C5 residuals: byte-string substring/char/search primitives. StrMid is dialect-variant
    // (MODERN: start<1 -> '', negative length -> rest of string; CLASSIC: clamps both), so the
    // run loop installs @AotStrMidModern or @AotStrMidClassic per program - the emitted code
    // stays dialect-blind.
    StrLeft: Pointer;    // offset 104: @AotStrLeft  (dstSlot, sVal, n)
    StrRight: Pointer;   // offset 112: @AotStrRight (dstSlot, sVal, n)
    StrMid: Pointer;     // offset 120: @AotStrMid{Modern|Classic} (dstSlot, sVal, start, len)
    StrAsc: Pointer;     // offset 128: @AotStrAsc  (sVal) -> code of first byte (0 if empty)
    StrChr: Pointer;     // offset 136: @AotStrChr  (dstSlot, code)
    StrInstr: Pointer;   // offset 144: @AotStrInstr (hayVal, needleVal, start) -> 1-based pos
    // Str()/Val() leaf primitives: dialect-independent handlers (IntToStr / leading-number
    // parse), hot in string benchmarks. Float Str() stays on the helper - its handler needs
    // the console-behavior object (dialect trim + SINGLE digits).
    StrIntToStr: Pointer; // offset 152: @AotIntToString (dstSlot, v)
    StrVal: Pointer;      // offset 160: @AotStrVal (sVal) -> Double (xmm0)
    StrValInt: Pointer;   // offset 168: @AotStrValInt (sVal) -> Int64
    // A STRING array element is managed, so it is reached through a primitive rather than by
    // address: the descriptor in ArrDesc has no StringData slot (its four Int64 are IntData,
    // FloatData, Count, LBound) and widening it would change the *32 stride baked into every
    // emitted array access in BOTH backends.
    ArrLoadStr: Pointer;  // offset 176: @AotArrLoadStr  (dstSlot, VMSelf, arrIdx, idx)
    ArrStoreStr: Pointer; // offset 184: @AotArrStoreStr (VMSelf, arrIdx, srcVal, idx)
    // ASC(MID$(...)) fused. Dialect-variant like StrMid, installed per program.
    StrAscMid: Pointer;   // offset 192: @AotStrAscMid{Modern|Classic} (sVal, ignored, start, len) -> code
    // "acc + MID$(tab, k, 1)" fused. Dialect-variant for the same reason as StrMid: only the rule
    // that decides whether the one-character substring is EMPTY differs between the two.
    StrConcatCharAt: Pointer; // offset 200: @AotStrConcatCharAt{Modern|Classic} (dstSlot, accVal, tabVal, k)
    // "acc += tab[Asc(s[i])+1]" fused whole. Dialect-variant for the same reason as the two above:
    // only the rule for a start below 1 differs.
    StrAppendMapped: Pointer; // offset 208: @AotStrAppendMapped{Modern|Classic} (dstSlot, srcVal, tabVal, i)
    // C6 native RECORD family. Same shape and the same reason as the C5 string primitives: without
    // them New/Delete/RecMark ran through AotExecOne, and EmitHelperCall's flush+reload of every
    // allocated register around each one cost more than the allocation itself (measured: `--aot`
    // 358 ns per New+Delete pair against the interpreter's 194).
    RecNew: Pointer;       // offset 216: @AotRecordNew (VMSelf, CtxObj, packedCounts, imm) -> handle
    RecFree: Pointer;      // offset 224: @AotRecordFree (VMSelf, handle)
    RecMarkPush: Pointer;  // offset 232: @AotRecMarkPush (CtxObj)
    RecMarkPop: Pointer;   // offset 240: @AotRecMarkPop (CtxObj)
  end;
  PAotCtx = ^TAotCtx;

const
  // Field offsets in TAotCtx, as the emitted [r8+disp] loads use them.
  AOTCTX_XFERINT   = 0;
  AOTCTX_XFERFLOAT = 8;
  AOTCTX_ARRDESC   = 16;
  AOTCTX_EXECONE   = 24;
  AOTCTX_VMSELF    = 32;
  AOTCTX_CTXOBJ    = 40;
  AOTCTX_STRREGS   = 48;
  AOTCTX_STRCMP    = 56;
  AOTCTX_STRASSIGN = 64;
  AOTCTX_STRLOADCONST = 72;
  AOTCTX_STRCONCAT = 80;
  AOTCTX_STRLEN    = 88;
  AOTCTX_ARRLOADSTR  = 176;
  AOTCTX_ARRSTORESTR = 184;
  AOTCTX_STRASCMID   = 192;
  AOTCTX_STRCONCATCHARAT = 200;
  AOTCTX_STRAPPENDMAPPED = 208;
  AOTCTX_RECNEW      = 216;
  AOTCTX_RECFREE     = 224;
  AOTCTX_RECMARKPUSH = 232;
  AOTCTX_RECMARKPOP  = 240;
  AOTCTX_CALLSUB   = 96;
  AOTCTX_STRLEFT   = 104;
  AOTCTX_STRRIGHT  = 112;
  AOTCTX_STRMID    = 120;
  AOTCTX_STRASC    = 128;
  AOTCTX_STRCHR    = 136;
  AOTCTX_STRINSTR  = 144;
  AOTCTX_STRINTTOSTR = 152;
  AOTCTX_STRVAL    = 160;
  AOTCTX_STRVALINT = 168;

  // Helper return contract. Normally the helper returns the bytecode PC that follows the
  // instruction it ran; native code compares against the PC it expects and keeps going.
  // Anything else leaves to the interpreter at the returned PC. These two values are not
  // PCs at all and are handled specially at the AOT call site in RunTemplate:
  AOT_HELPER_EXC  = PtrInt(-1);   // the instruction raised: exception parked in Ctx.AotPendingExc,
                                  // culprit PC in Ctx.AotFaultPC -> re-raise there
  AOT_HELPER_HALT = PtrInt(-2);   // the instruction cleared Ctx.Running (CTRL+C, quit, failed
                                  // ASSERT): resume PC in Ctx.AotFaultPC -> leave the run loop
  // B3: AotCallSub's "call completed" value, consumed by the CALLER's native code (never seen
  // by AotSettle): the callee ran to its bcReturnSub and the frame is popped - continue
  // natively after the call. Any non-negative return is a deopt PC instead (the callee handed
  // the rest of the invocation to the interpreter, frame still pushed); the two negative
  // helper sentinels pass through unchanged.
  AOT_CALL_OK     = PtrInt(-3);

type
  // The one-instruction runtime helper (C3). Executes bytecode instruction PC on Ctx with the
  // interpreter's existing slow path and returns the next PC, or one of the sentinels above.
  // cdecl: emitted code follows the platform C ABI.
  //
  // AotCtx is the caller's own context record, passed back so the helper can REFRESH it: an
  // instruction like DIM/REDIM/ERASE reallocates the array descriptor table, which would leave
  // the pointer native code is holding stale (C4 - this is what made array programs read zeros
  // once ssaArrayDim started going through the helper).
  TAotExecOneFn = function(VMSelf, CtxObj: Pointer; PC: PtrInt; AotCtx: PAotCtx): PtrInt; cdecl;

  // A compiled function: same bank pointers as the loop JIT plus the AOT context in
  // the third argument register. Returns the bytecode PC where the interpreter
  // resumes (the function's bcReturnSub / bcEnd, or a deopt PC).
  TNativeFuncFn = function(IntRegs, FloatRegs: PInt64; Ctx: PAotCtx): PtrInt;

  // B3 native call-site primitive (implemented in SedaiBytecodeVM next to AotExecOne so it can
  // reach the VM's private FramePush/FramePop/FNativeFuncs). Replicates bcCallSub for a callee
  // that is itself compiled; returns AOT_CALL_OK, a deopt PC, or a helper sentinel.
  TAotCallSubFn = function(AotCtx: PAotCtx; CalleeEntryPC, BcCallSubPC: PtrInt): PtrInt; cdecl;

  TAotFuncEntry = record
    EntryPC: Integer;
    // Last bytecode PC this region covers. The loop JIT needs it: without knowing WHICH PCs the AOT
    // already owns, it compiles the same loops a second time and the combined profile pays two
    // compilations to run the AOT's code anyway -- measured as +21% on fannkuch and +8,6% on
    // binary-trees against --aot alone.
    LastPC: Integer;
    Mem: TExecMem;       // ownership passes to the caller (the VM frees it)
  end;
  TAotFuncs = array of TAotFuncEntry;

type
  TAotRegion = record
    Name: string;                    // 'MAIN' or the procedure name (PROC_ suffix)
    FirstBlock, LastBlock: Integer;  // inclusive block-index range in SSAProg.Blocks
    FirstOrdinal: Integer;           // ordinal of the region's first SSA instruction
    InstrCount: Integer;             // SSA instructions in the region
    EntryPC: Integer;                // final bytecode PC of the first emitted instruction (-1 = none)
    Eligible: Boolean;               // every op is in the B1 scalar set and all jumps stay inside
    BailReason: string;              // first offender ('' when eligible)
    // B3 survey: what this region would need for a NATIVE call site (see PIANO_B1_AOT_DESIGN §4).
    CallTargets: array of string;    // PROC_ names this region calls (static targets only)
    HasIndirectCall: Boolean;        // an indirect call: never eligible for a native call site
    EligibleNoCalls: Boolean;        // every op except ssaCallSub is in the set
    // True when the ONLY thing keeping this region out is its calls, and every target is itself
    // eligible - i.e. exactly what B3 would unlock. Filled by AotMarkB3Candidates.
    B3Candidate: Boolean;
  end;
  TAotRegions = array of TAotRegion;

// Slice into regions and classify against the B1 scalar set. Prog supplies the
// SSA->PC map (entry PCs and the cross-check that the map lines up with ProcMap).
function AotSliceAndClassify(SSAProg: TSSAProgram; Prog: TBytecodeProgram): TAotRegions;

// AOT_DIAG=1 printout: per-region verdict + summary + map cross-check warnings.
procedure AotSurvey(SSAProg: TSSAProgram; Prog: TBytecodeProgram; AllowUnsafe: Boolean);

// Diagnostics from the last region compiled (liveness, C1). Not thread-safe; reporting only.
var
  AotDiagPeakLiveInt: Integer = 0;
  AotDiagPeakLiveFloat: Integer = 0;
  // How many DISTINCT VM registers the region touches, per bank. Read next to peakLive it says
  // whether register pressure is real or an artefact of the SSA allocator: peak-live 3 floats
  // spread over dozens of distinct VM registers means the values would all fit in the xmm pool
  // if they were reused, and every access beyond the 6 pooled ones is pure memory traffic.
  AotDiagDistinctInt: Integer = 0;
  AotDiagDistinctFloat: Integer = 0;
  // TRUE peak of simultaneously-live values, measured mid-block (not just at block boundaries
  // like peakLive). This is the number that decides the linear-scan payoff: if maxLive <= pool
  // size (7 gpr / 6 xmm) a live-range allocator can keep EVERY live value in a register with
  // zero hot-loop spill, collapsing the distinct-register memory traffic to nothing.
  AotDiagMaxLiveInt: Integer = 0;
  AotDiagMaxLiveFloat: Integer = 0;
  // Loop-weighted register traffic already resident (top pool-size slots the static home
  // allocator pins) vs the spilled tail. A large spilled-tail with a SMALL maxLive is the
  // signature of the static allocator's inability to time-multiplex a machine register across
  // disjoint-lifetime values - exactly the traffic a linear-scan allocator would reclaim.
  AotDiagFloatResident: Int64 = 0;
  AotDiagFloatTotal: Int64 = 0;
  AotDiagIntResident: Int64 = 0;
  AotDiagIntTotal: Int64 = 0;
  // Simulated resident traffic under a linear-scan (live-range) allocator with the same pool
  // size - the go/no-go for building the real thing. Conservative lower bound (see the sim).
  AotDiagFloatLinScan: Int64 = 0;
  AotDiagIntLinScan: Int64 = 0;
  // Loop-weighted use of BLOCK-LOCAL float temporaries (every touch in one block, neither
  // live-in nor live-out). This is the low-risk hybrid's ceiling: these can go to a within-block
  // dynamic xmm pool with zero bank traffic and no cross-block consistency hazard.
  AotDiagFloatBlockLocal: Int64 = 0;
  AotDiagFloatBlockLocalCount: Integer = 0;
  AotDiagLivenessOK: Boolean = False;
  // AOT_DYNF=1 enables the within-block dynamic float allocator (see AotCompileRegion). -1 =
  // env not yet read. Default OFF: the static-home codegen is emitted byte-for-byte as before,
  // so the two can be A/B'd on one binary.
  GAotDynFloatState: Integer = -1;
  // AOT_LINSCAN=1 enables the interval allocator (B1b). -1 = env not yet read.
  GAotLinScanState: Integer = -1;
  // Set by the VM's StrCapacityInit once it has CONFIRMED, on this runtime, that an AnsiString's
  // length field really sits at [ptr - SizeOf(SizeInt)]. Emitted code that steps into the string
  // header (the inline Asc(Mid()) fast path) is gated on it, so a future FPC with a different
  // layout loses the optimisation instead of reading garbage as a length. Deliberately a SEPARATE
  // flag from GStrCapacity: STRCAP=0 is the A/B switch for the capacity work and must not silently
  // turn this off too.
  GAotStrHdrOK: Boolean = False;
  // ASCMIDINLINE=0 forces bcStrAscMid back to the pure helper call (the A/B on one binary).
  // -1 = env not yet read.
  GAotAscMidInlineState: Integer = -1;
  // AOT_DUMP: write the emitted machine code of every compiled region to disk, together with the
  // annotation map that says which SSA instruction each byte range came from. Purely observational
  // - with the variable unset not one byte of the codegen path changes. -1 = env not yet read,
  // GAotDumpDir empty = off. See AotDumpDir.
  GAotDumpState: Integer = -1;
  GAotDumpDir: string = '';
  GAotDumpSeq: Integer = 0;
  // Did the interval allocator actually run for the last region, and what did it place?
  AotDiagLinScanActive: Boolean = False;
  AotDiagLsPlacedInt: Integer = 0;
  AotDiagLsPlacedFloat: Integer = 0;
  AotDiagLsSpilledInt: Integer = 0;
  AotDiagLsSpilledFloat: Integer = 0;
  // The traffic the interval schedule ACTUALLY emits, weighted by the loop depth of the block it
  // lands in - which is the only version of the number that means anything, since one store in a
  // loop body outweighs fifty in the prologue. Read against the static homes, which emit neither
  // inside a loop: this is the whole account of what the allocator pays for what it saves.
  AotDiagLsLoads: Integer = 0;
  AotDiagLsStores: Integer = 0;
  AotDiagLsLoadW: Int64 = 0;
  AotDiagLsStoreW: Int64 = 0;
  // Loop-weighted operand accesses the EMITTED code sends to the banks, counted where the decision
  // is actually made (FAlloc/IAlloc answering "no home"). This is the number that compares two
  // allocators honestly - the residency figures elsewhere model a policy, this counts instructions.
  AotDiagMemAccI: Int64 = 0;
  AotDiagMemAccF: Int64 = 0;
  AotDiagCodeW: Int64 = 0;      // loop-weighted bytes of emitted code
  // Which of the two register strategies actually ran for the last region compiled, and whether
  // the AUTO arbitration was the reason. Without this the choice is only observable on a
  // stopwatch - and the two are antagonistic, so reading it wrong costs a whole campaign.
  AotDiagDynFActive: Boolean = False;
  AotDiagDynIActive: Boolean = False;
  AotDiagMergeApplied: Boolean = False;
  // C3: runtime-helper calls emitted in the last region. Also the coverage delta this stage
  // bought: a region reporting helpers>0 is one that could not compile at all before, since
  // a single op outside the native set used to bail the whole function.
  AotDiagHelperCalls: Integer = 0;
  // Which ops those helper calls execute, as "name*count" pairs - the first thing to read
  // when hunting a hot-loop helper (a cold DIM/PRINT is fine, a per-iteration op is not).
  AotDiagHelperOps: string = '';
  // B1b interval model, measured on the last region compiled. The pair to read is (webs,
  // maxOverlap): webs is how many independent VALUES the region really has where the static
  // allocator sees "distinct" register NUMBERS, and maxOverlap is how many of them are live at
  // once - the number a machine pool has to fit. maxOverlap <= pool means everything can be
  // resident; webs >> distinct means the merge really did stack values on shared numbers, which
  // is exactly what splitting takes back apart.
  AotDiagLsWebsInt: Integer = 0;
  AotDiagLsWebsFloat: Integer = 0;
  AotDiagLsRanges: Integer = 0;
  AotDiagLsMaxOverInt: Integer = 0;
  AotDiagLsMaxOverFloat: Integer = 0;
  AotDiagLsCross: Integer = 0;        // (edge, register) pairs a value crosses: the upper bound on
                                      // the resolution moves the CFG can demand
  AotDiagLsWhy: string = '';          // empty = the model was built

// Compile every eligible region to native code (B1a: static frequency register
// assignment, deopt only for trapping ops). TrueVal is the VM's TRUE (-1);
// the dialect comes from Prog.ModernMode. AllowUnsafe = MODERN and no forced
// bounds check: array OOB takes the FreeBASIC default path natively; otherwise
// array accesses guard and deopt so the interpreter raises. Diag prints
// per-region compile results.
// SkipMain: engine arbitration for the COMBINED --aot --jit mode. The loop JIT can only see
// (and inline callees into) loops that run in the interpreter's dispatch loop; an AOT-compiled
// MAIN steals the module-level hot loop from it and replaces loop inlining with a native call
// per iteration - measured 2x slower on n-body BACK THEN. Since that measurement the AOT gained
// movaps copies, in-place computation and the dynamic register allocator, and on n-body it is now
// the FASTEST profile (404 ms against the JIT's 606), so the arbitration is worth re-deciding.
// AotSkipMainDefault reads the current verdict; AOT_MAIN=1/0 overrides it, which is what makes the
// two arrangements A/B-able on one binary.
function AotCompileProgram(SSAProg: TSSAProgram; Prog: TBytecodeProgram;
                           TrueVal: Int64; AllowUnsafe, Diag, SkipMain: Boolean): TAotFuncs;

// Should the AOT leave MAIN to the JIT in combined mode? Tri-state env override AOT_MAIN
// (1 = AOT takes MAIN, 0 = AOT skips it); with the variable unset the compiled-in default wins.
function AotSkipMainDefault(CombinedMode: Boolean): Boolean;

// Hand the emitter the TExecutionContext/TRecordStorage layout so a record FIELD access can be
// lowered natively instead of routed to the helper (the JIT has done this since J13). Only offsets
// are taken, never an address: the emitted code reads the current record base from the context it
// is given, so one compiled function serves the main context and a THREADCREATE worker alike.
// Until this is called the offsets are zero and every record op keeps going through the helper,
// which is the safe default for any caller that does not supply them.
//
// Why it matters: a helper call is not just a call. AotHelperCall flushes EVERY allocated VM
// register to its bank slot before it and reloads them all after, so with a ten-register pool one
// record field access costs about twenty memory operations. job/tests/bench/intrec_fb.bas takes
// FOUR of them per iteration (three field reads and one write) in a loop body of ~25 instructions -
// which is why --aot is only 1.8x the interpreter on that program against 27x on n-body.
procedure AotSetRecordLayout(RecordsOff, RecSize, RecIntOff, RecFloatOff, SharedRecOff: Integer);

implementation

uses TypInfo;

// Record layout handed in by AotSetRecordLayout. Zero = "not supplied", which keeps every record op
// on the helper. Compilation is single-threaded (the same reason the SSA name pool is), so unit
// state is safe here.
var
  GRecordsOff: Integer = 0;
  GRecSize: Integer = 0;
  GRecIntOff: Integer = 0;
  GRecFloatOff: Integer = 0;
  GSharedRecOff: Integer = 0;
  GRecNativeState: Integer = -1;   // -1 unread, 0 off, 1 on
  GRecAllocState: Integer = -1;    // C6 New/Delete/RecMark as leaf calls: -1 unread, 0 off, 1 on
  GNoThreads: Boolean = False;     // program creates no thread: the shared region cannot grow under us
  // A STRING array element can be reached natively (through AotStrAssign) only when an
  // out-of-range index cannot RAISE: the helper would throw across a compiled frame that is not
  // registered for unwinding. That is exactly AllowUnsafe (MODERN, no forced bounds check), which
  // is also what ArrClassic is derived from - so the classifier, the prescan and the emitter all
  // read THIS, and cannot disagree about which instructions are native.
  GArrStrNative: Boolean = False;
  GDivConstState: Integer = -1;    // C7 division by a constant: -1 unread, 0 off, 1 on
  // Diagnostics: how many div/mod sites took the magic path and how many stayed on idiv, and why.
  GDivConstHit: Integer = 0;
  GDivConstMiss: Integer = 0;

procedure AotSetRecordLayout(RecordsOff, RecSize, RecIntOff, RecFloatOff, SharedRecOff: Integer);
begin
  GRecordsOff := RecordsOff;
  GRecSize := RecSize;
  GRecIntOff := RecIntOff;
  GRecFloatOff := RecFloatOff;
  GSharedRecOff := SharedRecOff;
end;

// Native record field access, gated so the two arrangements are A/B-able on ONE binary:
// AOT_RECNAT=0 forces every record op back onto the helper (the historical behaviour).
function AotRecNative: Boolean;
begin
  if GRecNativeState < 0 then
  begin
    if GetEnvironmentVariable('AOT_RECNAT') = '0' then GRecNativeState := 0
    else GRecNativeState := 1;
  end;
  Result := (GRecNativeState = 1) and (GRecSize > 0);
end;

// C7: replace a division by a CONSTANT with a multiply-high and shifts, gated for the A/B.
// AOT_DIVCONST=0 emits the historical idiv.
function AotDivConstNative: Boolean;
begin
  if GDivConstState < 0 then
  begin
    if GetEnvironmentVariable('AOT_DIVCONST') = '0' then GDivConstState := 0
    else GDivConstState := 1;
  end;
  Result := GDivConstState = 1;
end;

{ ⛔ STRUCTURAL NEGATIVE, measured 2026-08-01: the constant divisor CANNOT be recovered HERE.
  C7's first attempt was a whole-program per-register analysis ("a register whose definitions are
  all the same constant holds that constant"): sound, and useless. On pidigits it found 11 registers
  out of 401 and ZERO division sites, because after register allocation the register number no
  longer identifies a value - the one carrying 1000000000 is also written by a LoadConstInt 10, an
  ArrayLoad, two CopyInt and a SubInt elsewhere in the program. The constant only exists BEFORE
  allocation: TSSAProgram.AnnotateDivByConst stamps it on the instruction and it is read back here
  from the Immediate.
  ⭐ The lesson: an analysis that runs after the information has been erased is not conservative,
  it is BLIND - and the stopwatch does not say so. The first run measured -1%, i.e. noise, which is
  exactly how a transformation that never fires reads; what told the truth was the COUNTER below
  ("0 site(s) lowered"). }

{ Magic number for a SIGNED 64-bit division by d (|d| >= 2), Hacker's Delight figure 10-4.
  Returns M and the post-shift s such that
      q = floor_to_zero(n / d)
  is computed as  t = mulhi(M, n); [t += n | t -= n]; t >>= s; q = t + (t >>> 63).
  AddMarker/SubMarker say whether the corrective add or subtract is needed (M's sign disagreeing
  with d's is exactly that case). }
procedure AotMagicSigned(d: Int64; out M: Int64; out s: Integer; out NeedAdd, NeedSub: Boolean);
var
  p: Integer;
  ad, anc, delta, q1, r1, q2, r2, t: QWord;
  two63: QWord;
begin
  two63 := QWord(1) shl 63;
  if d < 0 then ad := QWord(-d) else ad := QWord(d);
  t := two63 + (QWord(d) shr 63);          // 2^63 + (d<0 ? 1 : 0)
  anc := t - 1 - (t mod ad);               // |nc|
  p := 63;
  q1 := two63 div anc;        r1 := two63 - q1 * anc;
  q2 := two63 div ad;         r2 := two63 - q2 * ad;
  repeat
    Inc(p);
    q1 := 2 * q1;  r1 := 2 * r1;
    if r1 >= anc then begin Inc(q1); Dec(r1, anc); end;
    q2 := 2 * q2;  r2 := 2 * r2;
    if r2 >= ad then begin Inc(q2); Dec(r2, ad); end;
    delta := ad - r2;
  until not ((q1 < delta) or ((q1 = delta) and (r1 = 0)));
  M := Int64(q2 + 1);
  if d < 0 then M := -M;
  s := p - 64;
  NeedAdd := (d > 0) and (M < 0);
  NeedSub := (d < 0) and (M > 0);
end;

// C6: record ALLOCATION (New/Delete) and the block marks as native leaf calls instead of runtime
// helper calls. Gated on its own so the two arrangements are A/B-able on ONE binary:
// AOT_RECALLOC=0 puts the whole family back on AotExecOne (the historical behaviour).
//
// ⚠️ Independent of AotRecNative: that one needs the record LAYOUT (field access reads
// Records[h].IntData[slot] by address), these primitives do not - they call the same VM routines
// the interpreter calls, so they work with or without a layout.
function AotRecAllocNative: Boolean;
begin
  if GRecAllocState < 0 then
  begin
    if GetEnvironmentVariable('AOT_RECALLOC') = '0' then GRecAllocState := 0
    else GRecAllocState := 1;
  end;
  Result := GRecAllocState = 1;
end;

// AOT_DYNF gate, read once. Tri-state: 0 = AUTO (default: enable per region only where the
// dynamic float allocator pays - see PlanDynFloat's throughput-bound test), 1 = force ON every
// region (A/B and testing), 2 = force OFF (A/B baseline; static homes, byte-identical to before).
function AotDynFloatMode: Integer;
var s: string;
begin
  if GAotDynFloatState < 0 then
  begin
    s := GetEnvironmentVariable('AOT_DYNF');
    if s = '' then GAotDynFloatState := 0
    else if s = '0' then GAotDynFloatState := 2
    else GAotDynFloatState := 1;
  end;
  Result := GAotDynFloatState;
end;

// AOT_LINSCAN gate, read once. Tri-state with the same shape as AOT_DYNF: 1 = force ON,
// 2 = force OFF (byte-identical baseline, which is what makes every measurement an A/B on ONE
// binary). The default is OFF while the nets that decide - the FB example sweep above all - have
// not been run with it on: this is a register allocator, and the lesson Copy Coalescing and the
// first REGREUSE flip both taught is that the net that decides is REAL PROGRAMS.
function FloatPoolTop: Integer;
// Highest xmm the allocators may hand out. AOT_FPOOL=<n> asks for n registers, xmm2..(n+1);
// AOT_FPOOL=6 restores the historic pool exactly, which is what makes every measurement below an
// A/B on a single binary.
//
// The default is the FULL pool, xmm2..15, since 2026-07-25. What made it worth the REX prefix on
// every float encoding in this unit was the slope downwards: with 4 registers instead of 6,
// floatpoly costs +58% and n-body +9%. The pool is the binding constraint, not the allocation
// policy - after the REGREUSE merge the peak simultaneously-live float count is 8 on n-body and 19
// on floatpoly, against the 6 registers there were to give.
//
// Interleaved A/B, best-of-5, on one binary, every run's output compared against the baseline:
//
//   bench       --aot  6 -> 14      --aot --jit  6 -> 14
//   n-body      347 -> 329  -5%     354 -> 327  -7%
//   floatpoly   241 -> 226  -6%     241 -> 227  -5%
//   intpoly     640 -> 643   0%     646 -> 645   0%
//   cvtpoly     530 -> 528   0%     538 -> 533   0%
//   arraysum    320 -> 324  +1%     327 -> 325   0%
//   sieve       644 -> 648   0%     642 -> 641   0%
//   strops      547 -> 542   0%     544 -> 546   0%
//   nbody_v7  12255 ->12280   0%    894 -> 899   0%
//
// Two clear wins where the float pressure is, nothing worse than noise anywhere else, and the
// output bit-identical in every cell. xmm6-15 are callee-saved on Win64, so each register handed
// out costs a save/restore pair in the prologue - which is why the flat rows are the ones to read:
// a region that does not need the extra registers does not pay for them, because SaveXmm is set
// only for a register the pool ACTUALLY gives to a value.
var s: string; n: Integer;
begin
  Result := 15;
  s := GetEnvironmentVariable('AOT_FPOOL');
  if s = '' then Exit;
  n := StrToIntDef(s, 14);
  if (n >= 1) and (n <= 14) then Result := 1 + n;
end;

function IntPoolCount(Avail: Integer): Integer;
// How many of the GPR pool's registers the allocators may hand out. VM int registers and the
// array-descriptor cache compete for the same set, which is why adding two pays more than the
// marginal slope suggested: it relieves both. AOT_IPOOL=7 restores the historic r9..r15 pool
// exactly, which is what makes the measurement an A/B on a single binary.
//
// The default is the full pool since 2026-07-25. Interleaved A/B, best-of-5, output compared on
// every run, both engine profiles, fbc thermometer 193 ms:
//
//   bench       --aot  7 -> 9       --aot --jit  7 -> 9
//   n-body      326 -> 310  -4%     340 -> 315  -7%
//   intpoly     642 -> 609  -5%     650 -> 615  -5%
//   sieve       647 -> 626  -3%     653 -> 631  -3%
//   arraysum    324 -> 315  -2%     331 -> 321  -3%
//   cvtpoly     537 -> 528  -1%     535 -> 534   0%
//   strops      549 -> 539  -1%     547 -> 541  -1%
//   floatpoly   224 -> 224   0%     229 -> 226  -1%
//   nbody_v7  12257 ->12233  0%     912 -> 905   0%
//
// Seven of eight improve, nothing regresses, output bit-identical in all sixteen cells - a broader
// win than the float pool's, which only moved the two float-bound programs.
var s: string; n: Integer;
begin
  Result := Avail;
  s := GetEnvironmentVariable('AOT_IPOOL');
  if s = '' then Exit;
  n := StrToIntDef(s, Avail);
  if (n >= 1) and (n <= Avail) then Result := n;
end;

function AotRsiPoolEnabled: Boolean;
// AOT_RSI=0 keeps rsi reserved even in a float-free region (the A/B baseline on one binary).
begin
  Result := GetEnvironmentVariable('AOT_RSI') <> '0';
end;

function AotLinScanMode: Integer;
var s: string;
begin
  if GAotLinScanState < 0 then
  begin
    s := GetEnvironmentVariable('AOT_LINSCAN');
    if (s = '') or (s = '0') then GAotLinScanState := 2
    else GAotLinScanState := 1;
  end;
  Result := GAotLinScanState;
end;

function AotAscMidInline: Boolean;
// Is the inline fast path for bcStrAscMid emitted? Needs BOTH the runtime-confirmed string header
// layout and the gate left at its default (ASCMIDINLINE=0 is the A/B on one binary).
begin
  if GAotAscMidInlineState < 0 then
  begin
    if GetEnvironmentVariable('ASCMIDINLINE') = '0' then GAotAscMidInlineState := 0
    else GAotAscMidInlineState := 1;
  end;
  Result := (GAotAscMidInlineState = 1) and GAotStrHdrOK;
end;

function AotDumpDir: string;
// Where AOT_DUMP writes, or '' when it is off (the default, and then nothing at all is recorded).
// AOT_DUMP=<path> picks the directory; AOT_DUMP=1 means job/aotdump. Every region compiled leaves
// two files behind: <seq>_<region>.bin, the exact bytes handed to TExecMem, and <seq>_<region>.map,
// which says what the emitter was doing at each offset - SSA ordinal, bytecode PC, op name - plus
// the register legend. Reading a disassembly without that legend is guesswork: the machine register
// numbers mean nothing until you know which VM bank slot each one is standing in for.
begin
  if GAotDumpState < 0 then
  begin
    GAotDumpDir := Trim(GetEnvironmentVariable('AOT_DUMP'));
    if (GAotDumpDir = '1') or (LowerCase(GAotDumpDir) = 'yes') then
      GAotDumpDir := 'job' + PathDelim + 'aotdump'
    else if GAotDumpDir = '0' then
      GAotDumpDir := '';
    if (GAotDumpDir <> '') and not ForceDirectories(GAotDumpDir) then
      GAotDumpDir := '';                     // undumpable path: stay silent rather than fail a run
    GAotDumpState := Ord(GAotDumpDir <> '');
  end;
  Result := GAotDumpDir;
end;

function GprName(R: Integer): string;
const
  N: array[0..15] of string = ('rax', 'rcx', 'rdx', 'rbx', 'rsp', 'rbp', 'rsi', 'rdi',
                               'r8', 'r9', 'r10', 'r11', 'r12', 'r13', 'r14', 'r15');
begin
  if (R >= 0) and (R <= 15) then Result := N[R] else Result := '?';
end;

function XmmName(R: Integer): string;
begin
  if (R >= 0) and (R <= 15) then Result := 'xmm' + IntToStr(R) else Result := '?';
end;

function AotSkipMainDefault(CombinedMode: Boolean): Boolean;
// Engine arbitration for --aot --jit. Outside combined mode there is nothing to arbitrate: the
// AOT always takes MAIN. Inside it, the historic answer was "leave MAIN to the JIT", because an
// AOT MAIN replaced the JIT's loop inlining with a native call per iteration. That verdict was
// measured before movaps, in-place computation and the dynamic allocator; with those in, the AOT
// owns MAIN faster than the JIT does, so the default is to take it. AOT_MAIN overrides either way.
var s: string;
begin
  if not CombinedMode then Exit(False);
  s := GetEnvironmentVariable('AOT_MAIN');
  if s = '1' then Result := False          // AOT takes MAIN
  else if s = '0' then Result := True      // historic split: JIT owns MAIN
  else Result := False;                    // default: AOT takes MAIN
end;

// The B1 op set: scalar int/float compute + control flow + the Xfer scalar forms
// (parameter prologue / result epilogue) + frame record marks (no-ops in a function
// that owns no records, which the classifier guarantees by excluding record ops).
function IsB1Op(Op: TSSAOpCode): Boolean;
begin
  case Op of
    ssaLoadConstInt, ssaLoadConstFloat,
    ssaCopyInt, ssaCopyFloat,
    ssaAddInt, ssaSubInt, ssaMulInt, ssaDivInt, ssaModInt, ssaNegInt,
    ssaDivUInt, ssaModUInt,
    ssaAddFloat, ssaSubFloat, ssaMulFloat, ssaDivFloat, ssaNegFloat,
    ssaIntToFloat, ssaFloatToInt, ssaFloatRound, ssaNarrowInt, ssaNarrowSingle,
    ssaCmpEqInt, ssaCmpNeInt, ssaCmpLtInt, ssaCmpGtInt, ssaCmpLeInt, ssaCmpGeInt,
    ssaCmpLtUInt, ssaCmpGtUInt, ssaCmpLeUInt, ssaCmpGeUInt,
    ssaCmpEqFloat, ssaCmpNeFloat, ssaCmpLtFloat, ssaCmpGtFloat, ssaCmpLeFloat, ssaCmpGeFloat,
    // C5: string comparisons lower to a leaf call (AotStrCmp) that produces an int - no
    // allocation, no refcount, so the string operands stay in their bank and only the int Dest
    // is register-allocated. (No Le/Ge: the parser rewrites them to Gt/Lt with swapped operands.)
    ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString,
    // C5: the string bank ops that dominate hot string loops - each a leaf call to a Pascal
    // primitive (copy/const-load/concat are managed assignments; len returns an int).
    ssaCopyString, ssaLoadConstString, ssaStrConcat, ssaStrLen,
    // C5 residuals: byte-string substring/char/search primitives (W codepoint ops excluded).
    ssaStrLeft, ssaStrRight, ssaStrMid, ssaStrAsc, ssaStrAscMid, ssaStrConcatCharAt, ssaStrAppendMapped, ssaStrChr, ssaStrInstr,
    // Str() of an int and Val(): dialect-independent leaf primitives (float Str() stays on
    // the helper - it needs the console-behavior object).
    ssaIntToString, ssaStrVal, ssaStrValInt,
    ssaBitwiseAnd, ssaBitwiseOr, ssaBitwiseXor, ssaBitwiseNot,
    ssaShl, ssaShr, ssaShrUInt,
    ssaMathSqr,
    ssaLabel, ssaNop, ssaJump, ssaJumpIfZero, ssaJumpIfNotZero,
    ssaXferLoadInt, ssaXferLoadFloat, ssaXferStoreInt, ssaXferStoreFloat,
    ssaReturnSub, ssaEnd, ssaStop,
    ssaRecMarkPush, ssaRecMarkPop,
    // Record FIELD access: Ctx.Records[handle].{Int,Float}Data[slot]. Native only when the layout
    // was supplied (AotSetRecordLayout) - AotIsNative checks that. A shared-region handle deopts.
    ssaRecordLoadInt, ssaRecordLoadFloat, ssaRecordStoreInt, ssaRecordStoreFloat,
    // C6: record ALLOCATION as a leaf call to the VM's own AllocRecord/FreeSharedRecord
    // (AotIsNative checks the gate and the operand shape).
    ssaRecordNew, ssaRecordFree,
    // B2: 1-D int/float array element access + dim-0 bound queries (string-element
    // arrays are rejected by the classifier/prescan; multi-dim access goes through
    // ssaArrayIdxPush/Resolve, which are not in the set, so those regions bail).
    ssaArrayLoad, ssaArrayStore, ssaArrayLBound, ssaArrayUBound:
      Result := True;
  else
    Result := False;
  end;
end;

{ ---- C4: which ops the runtime helper may run --------------------------------------------
  C3 routed one hand-picked family (PRINT). C4 inverts the default: anything without a native
  lowering goes to the helper, EXCEPT what is proven unable to survive the trip. The gate is
  applied to the BYTECODE opcode, not the SSA opcode, because what actually runs is
  ExecuteInstruction on one bytecode instruction - so the question "can this be executed by
  the helper" has a precise answer that does not depend on guessing the lowering.

  ⚠️ The reason a deny-list is even needed: ExecuteInstruction's group-0 `case` has NO `else`,
  so an opcode it does not handle is a SILENT NO-OP, not an error. The list below was derived
  mechanically - for every group, the opcodes declared in SedaiBytecodeTypes minus those named
  in that group's handler - and it is exactly 24 opcodes:

    * group 0 (18): threads, mutexes, condition variables (they need run-loop state the helper
      has no access to), bcLoadProcAddr, the legacy bcLoadVar/bcStoreVar, bcStringToFloat/Int;
    * group 3 (6): the TYPED array accessors, which live only in the interpreter's inline case.

  Everything else - groups 1,2,4,5,6,7,10,11 and all 58 superinstructions - is fully covered by
  ExecuteInstruction and its per-group handlers, verified the same way.

  If this list ever drifts, the failure mode is a silent wrong answer, so re-derive it (not by
  eye) whenever an opcode is added: compare the group's declarations against its handler. }
function AotHelperUnsafeOp(BcOp: Word): Boolean;
begin
  case BcOp of
    // Group 0: no handler in ExecuteInstruction -> would silently do nothing.
    // (The thread/mutex/cond family USED to be here for that reason. ExecuteInstruction now handles
    // all thirteen - the same single call each inline arm makes - so they are routable, and a region
    // that touches one no longer bails. That was the MAIN of essentially every parallel program.
    // Cheap to route, unlike a string primitive: these are OS-level operations, so the helper's
    // register flush is noise next to the operation itself.)
    bcLoadProcAddr, bcLoadVar, bcStoreVar, bcStringToFloat, bcStringToInt,
    // Group 3: typed array accessors are handled only by the interpreter's inline case,
    // not by ExecuteArrayOp.
    bcArrayLoadInt, bcArrayLoadFloat, bcArrayLoadString,
    bcArrayStoreInt, bcArrayStoreFloat, bcArrayStoreString,
    // ⛔ Call and return: the two dispatchers store the return address with DIFFERENT
    // conventions, and mixing them corrupts the call stack.
    //   RunTemplate  pushes CurPC + 1 and returns to the popped value verbatim.
    //   ExecuteInstruction pushes Ctx.PC and expects its caller to add 1 on return.
    // Both are self-consistent; neither survives being paired with the other. A call pushed
    // by the helper and returned from by the interpreter jumps back ONTO the call, which
    // re-runs the whole callee - which is exactly what this cost to find. Routing them buys
    // nothing anyway: the helper moves the PC, so native execution would deopt immediately.
    // Native call sites are B3's job, not the helper's.
    bcCall, bcReturn, bcCallSub, bcCallSubIndirect, bcReturnSub,
    // ⛔ RESUME: the ONLY three handlers in ExecuteInstruction that set Ctx.PC and then return
    // WITHOUT the usual "target - 1" convention ("Exit; // Don't increment PC"), so the
    // helper's uniform "next PC = Ctx.PC + 1" is off by one for them. Worse, RunTemplate does
    // not merely differ in convention: its RESUME NEXT resumes at the next BASIC LINE
    // (FindPCAfterLine), a different semantic altogether. Error resumption stays interpreted.
    bcResume, bcResumeNext, bcResumeLabel,
    // ⛔ TRON/TROFF switch the VM between RunFast and RunDebug, which only the run loop can do
    // (it breaks out and re-enters through the other one). ExecuteInstruction just flips the
    // flag, so via the helper tracing would silently never engage.
    bcTron, bcTroff,
    // ⛔ The multi-dimensional index sequence: bcArrayIdxPush accumulates indices into VM
    // state that a later Resolve consumes. That makes the sequence, not the instruction, the
    // unit of correctness - and a helper call can hand control back to the interpreter in the
    // middle of one (a moved PC, a sentinel), leaving a half-built index list that the
    // interpreter then adds to, so Resolve linearises the wrong subscripts. Whole regions
    // using runtime multi-dim indexing stay interpreted, as they did before C4.
    bcArrayIdxPush, bcArrayIdxResolve, bcArrayIdxResolveInd:
      Result := True;
  else
    Result := False;
  end;
end;

// Can this array op take the NATIVE path? It needs a compile-time array ref, and for element
// access an int/float element bank (string elements are managed - interpreter only). Shared by
// the classifier, the prescan and the emitter so the three cannot disagree about which
// instructions are native; when it says no, the op falls back to the runtime helper.
function AotArrayNativeOK(SSAProg: TSSAProgram; const Ins: TSSAInstruction): Boolean;
begin
  Result := False;
  if (Ins.Src1.Kind <> svkArrayRef) or (Ins.Src1.ArrayIndex < 0) or
     (Ins.Src1.ArrayIndex >= SSAProg.GetArrayCount) then Exit;
  // A string element is MANAGED: the assignment has to go through AotStrAssign for the refcount,
  // which is fine (the same primitive every native string op uses) - but only where an
  // out-of-range index cannot raise. Before this, ONE such access took the whole region down, and
  // since a "Dim Shared" scalar is array-backed, a single shared string variable was enough to
  // leave a program entirely interpreted: 7 of the CLBG regions bailed here.
  if ((Ins.OpCode = ssaArrayLoad) or (Ins.OpCode = ssaArrayStore)) and
     (SSAProg.GetArray(Ins.Src1.ArrayIndex).ElementType = srtString) and
     not (GArrStrNative or Ins.BoundsSafe) then Exit;
  // ⚠️ ONE DIMENSION ONLY. The descriptor carries the element COUNT, not per-dimension
  // extents, so the bound lowering computes UBound = LBound + Count - 1 - true for a vector,
  // nonsense for a matrix (DIM m(3,4) would answer 19 instead of 3).
  //
  // This was a latent bug in the B2 lowering, not something C4 introduced: it simply could
  // never fire while every region holding a multi-dim array bailed for some other reason.
  // Widening the compiled set is what exposed it, and the same will be true of the next ones.
  if SSAProg.GetArray(Ins.Src1.ArrayIndex).DimCount <> 1 then Exit;
  Result := True;
end;

// Shape check for the C5 native string ops: the operands the emitted code reads as registers
// must actually BE registers of the expected bank (a rare const operand falls back to the helper
// rather than failing the region at emit time). Shared by classifier/prescan/emitter so the three
// agree, exactly like AotArrayNativeOK.
function AotStringNativeOK(const Ins: TSSAInstruction): Boolean;
  function IsStr(const V: TSSAValue): Boolean;
  begin Result := (V.Kind = svkRegister) and (V.RegType = srtString); end;
  function IsInt(const V: TSSAValue): Boolean;
  begin Result := (V.Kind = svkRegister) and (V.RegType = srtInt); end;
  function IsFlt(const V: TSSAValue): Boolean;
  begin Result := (V.Kind = svkRegister) and (V.RegType = srtFloat); end;
begin
  case Ins.OpCode of
    ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString:
      Result := IsInt(Ins.Dest) and IsStr(Ins.Src1) and IsStr(Ins.Src2);
    ssaCopyString:      Result := IsStr(Ins.Dest) and IsStr(Ins.Src1);
    ssaLoadConstString: Result := IsStr(Ins.Dest);
    ssaStrConcat:       Result := IsStr(Ins.Dest) and IsStr(Ins.Src1) and IsStr(Ins.Src2);
    ssaStrLen:          Result := IsInt(Ins.Dest) and IsStr(Ins.Src1);
    // C5 residuals (byte-string ops only; the W codepoint family stays on the helper).
    ssaStrLeft, ssaStrRight:
      Result := IsStr(Ins.Dest) and IsStr(Ins.Src1) and IsInt(Ins.Src2);
    // "acc + tab[k]": string dest, two string sources, and the index in the int bank.
    ssaStrAppendMapped,   // same operand shape: string dest+2 string sources, index in the int bank
    ssaStrConcatCharAt: Result := IsStr(Ins.Dest) and IsStr(Ins.Src1) and
                                  IsStr(Ins.Src2) and IsInt(Ins.Src3);
    ssaStrAscMid:       Result := IsInt(Ins.Dest) and IsStr(Ins.Src1) and
                                  IsInt(Ins.Src2) and IsInt(Ins.Src3);
    ssaStrMid:
      Result := IsStr(Ins.Dest) and IsStr(Ins.Src1) and IsInt(Ins.Src2) and IsInt(Ins.Src3);
    ssaStrAsc:          Result := IsInt(Ins.Dest) and IsStr(Ins.Src1);
    ssaStrChr:          Result := IsStr(Ins.Dest) and IsInt(Ins.Src1);
    ssaStrInstr:
      Result := IsInt(Ins.Dest) and IsStr(Ins.Src1) and IsStr(Ins.Src2) and IsInt(Ins.Src3);
    ssaIntToString:     Result := IsStr(Ins.Dest) and IsInt(Ins.Src1);
    ssaStrVal:          Result := IsFlt(Ins.Dest) and IsStr(Ins.Src1);
    ssaStrValInt:       Result := IsInt(Ins.Dest) and IsStr(Ins.Src1);
  else
    Result := False;
  end;
end;

// Is this SSA op one the AOT lowers natively? Combines the op set with the per-op shape
// conditions, so callers get a single yes/no and the helper picks up everything else.
function AotIsNative(SSAProg: TSSAProgram; const Ins: TSSAInstruction): Boolean;
begin
  // B3: a STATIC call is a native call site (AotCallSub replicates bcCallSub in Pascal and
  // invokes the callee's compiled function, falling back to the interpreter at run time when
  // the callee is not compiled). Indirect calls (ssaCallSubIndirect) and GOSUB (ssaCall) are
  // NOT: the target is a runtime value / the return convention differs.
  if Ins.OpCode = ssaCallSub then Exit(Ins.Dest.Kind = svkLabel);
  Result := IsB1Op(Ins.OpCode);
  if not Result then Exit;
  case Ins.OpCode of
    ssaArrayLoad, ssaArrayStore, ssaArrayLBound, ssaArrayUBound:
      Result := AotArrayNativeOK(SSAProg, Ins);
    ssaRecordLoadInt, ssaRecordLoadFloat, ssaRecordStoreInt, ssaRecordStoreFloat:
      // Needs the record layout AND a constant slot: the slot is baked into the displacement.
      Result := AotRecNative and (Ins.Src3.Kind = svkConstInt);
    // C6: New needs an int register for the handle it produces (the three operands are
    // compile-time slot counts, read from the BYTECODE instruction at emit time); Delete needs
    // the handle in an int register. Any other shape falls back to the helper.
    ssaRecordNew:
      Result := AotRecAllocNative and (Ins.Dest.Kind = svkRegister) and (Ins.Dest.RegType = srtInt);
    ssaRecordFree:
      Result := AotRecAllocNative and (Ins.Src1.Kind = svkRegister) and (Ins.Src1.RegType = srtInt);
    ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString,
    ssaCopyString, ssaLoadConstString, ssaStrConcat, ssaStrLen,
    ssaStrLeft, ssaStrRight, ssaStrMid, ssaStrAsc, ssaStrAscMid, ssaStrConcatCharAt, ssaStrAppendMapped, ssaStrChr, ssaStrInstr,
    ssaIntToString, ssaStrVal, ssaStrValInt:
      Result := AotStringNativeOK(Ins);
  end;
end;

// Can SSA ordinal AOrd be handed to the helper? Three conditions, all conservative:
// it emitted bytecode at all; it emitted EXACTLY ONE instruction (the helper runs one, so a
// 1:N lowering would silently skip N-1); and that instruction is one the helper can execute.
// Ordinals that emit nothing (labels, nops) map to -1 and are skipped when looking ahead.
function AotHelperRoutable(Prog: TBytecodeProgram; AOrd: Integer): Boolean;
type
  PInstr = ^TBytecodeInstruction;
var
  apc, q, nxt: Integer;
  Instrs: PInstr;
begin
  Result := False;
  if Prog = nil then Exit;
  apc := Prog.GetSsaPc(AOrd);
  if apc < 0 then Exit;
  Instrs := PInstr(Prog.GetInstructionsPtr);
  if (Instrs = nil) or (apc >= Prog.GetInstructionCount) then Exit;
  if AotHelperUnsafeOp(Instrs[apc].OpCode) then Exit;
  for q := AOrd + 1 to AOrd + 64 do
  begin
    nxt := Prog.GetSsaPc(q);
    if nxt >= 0 then Exit(nxt = apc + 1);
  end;
end;

function OpName(Op: TSSAOpCode): string;
begin
  Result := GetEnumName(TypeInfo(TSSAOpCode), Ord(Op));
end;

function AotSliceAndClassify(SSAProg: TSSAProgram; Prog: TBytecodeProgram): TAotRegions;
var
  Regions: TAotRegions;
  NRegions: Integer;
  BlockOrdinal: array of Integer;   // block index -> ordinal of its first instruction
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  RegionLabels: TStringList;        // labels of the current region's blocks (jump containment)
  i, j, Ordinal, r, o: Integer;

  procedure StartRegion(const AName: string; ABlock, AOrdinal: Integer);
  begin
    if NRegions >= Length(Regions) then SetLength(Regions, NRegions * 2 + 8);
    with Regions[NRegions] do
    begin
      Name := AName;
      FirstBlock := ABlock;
      LastBlock := ABlock;
      FirstOrdinal := AOrdinal;
      InstrCount := 0;
      EntryPC := -1;
      Eligible := True;
      BailReason := '';
      SetLength(CallTargets, 0);
      HasIndirectCall := False;
      EligibleNoCalls := True;
      B3Candidate := False;
    end;
    Inc(NRegions);
  end;

begin
  Result := nil;
  if (SSAProg = nil) or (SSAProg.Blocks.Count = 0) then Exit;
  SetLength(Regions, 8);
  NRegions := 0;

  // Pass 0: block -> ordinal of first instruction (ordinals count every instruction
  // of every block in program order, matching the bytecode compiler's emission walk).
  SetLength(BlockOrdinal, SSAProg.Blocks.Count);
  Ordinal := 0;
  for i := 0 to SSAProg.Blocks.Count - 1 do
  begin
    BlockOrdinal[i] := Ordinal;
    Inc(Ordinal, SSAProg.Blocks[i].Instructions.Count);
  end;

  // Pass 1: slice at PROC_ labels.
  StartRegion('MAIN', 0, 0);
  for i := 0 to SSAProg.Blocks.Count - 1 do
  begin
    Block := SSAProg.Blocks[i];
    if (i > 0) and (Copy(Block.LabelName, 1, 5) = 'PROC_') then
      StartRegion(Copy(Block.LabelName, 6, MaxInt), i, BlockOrdinal[i]);
    Regions[NRegions - 1].LastBlock := i;
  end;
  SetLength(Regions, NRegions);

  // Pass 2: classify each region.
  RegionLabels := TStringList.Create;
  try
    RegionLabels.Sorted := True;
    RegionLabels.Duplicates := dupIgnore;
    for r := 0 to NRegions - 1 do
    begin
      RegionLabels.Clear;
      for i := Regions[r].FirstBlock to Regions[r].LastBlock do
        if SSAProg.Blocks[i].LabelName <> '' then
          RegionLabels.Add(SSAProg.Blocks[i].LabelName);

      o := Regions[r].FirstOrdinal;
      for i := Regions[r].FirstBlock to Regions[r].LastBlock do
      begin
        Block := SSAProg.Blocks[i];
        for j := 0 to Block.Instructions.Count - 1 do
        begin
          Instr := Block.Instructions[j];
          Inc(Regions[r].InstrCount);
          // Entry PC = first instruction of the region that emitted bytecode.
          if (Regions[r].EntryPC < 0) and (Prog <> nil) then
            Regions[r].EntryPC := Prog.GetSsaPc(o);
          // B3 survey: record the call shape regardless of eligibility, so we can report how
          // many regions a native call site (PIANO_B1_AOT_DESIGN section 4) would unlock.
          if Instr.OpCode = ssaCallSubIndirect then
            Regions[r].HasIndirectCall := True
          else if Instr.OpCode = ssaCallSub then
          begin
            if Instr.Dest.Kind = svkLabel then
            begin
              SetLength(Regions[r].CallTargets, Length(Regions[r].CallTargets) + 1);
              Regions[r].CallTargets[High(Regions[r].CallTargets)] := Instr.Dest.LabelName;
            end
            else
              Regions[r].HasIndirectCall := True;
          end
          else if not IsB1Op(Instr.OpCode) then
            Regions[r].EligibleNoCalls := False;
          if Regions[r].Eligible then
          begin
            // A jump leaving the region means the region is not a self-contained function
            // (interleaved code, computed flow): still a hard bail. The helper could in
            // principle run it and deopt at the target, but the native lowering for jumps is
            // block-relative and would have to be bypassed per-instruction - not this stage.
            if ((Instr.OpCode = ssaJump) or (Instr.OpCode = ssaJumpIfZero) or
                (Instr.OpCode = ssaJumpIfNotZero)) and
               (Instr.Dest.Kind = svkLabel) and
               (RegionLabels.IndexOf(Instr.Dest.LabelName) < 0) then
            begin
              Regions[r].Eligible := False;
              Regions[r].BailReason := 'jump-out:' + Instr.Dest.LabelName;
            end
            // C4: no native lowering is no longer a bail - it becomes a runtime-helper call,
            // provided the helper can actually run that bytecode instruction. Only what fails
            // BOTH paths still takes the whole region down.
            else if not (AotIsNative(SSAProg, Instr) or AotHelperRoutable(Prog, o)) then
            begin
              Regions[r].Eligible := False;
              Regions[r].BailReason := OpName(Instr.OpCode);
            end;
          end;
          Inc(o);
        end;
      end;
    end;
  finally
    RegionLabels.Free;
  end;

  // B3 survey: a region is a candidate if calls are the ONLY thing stopping it, it makes no
  // indirect call, and every target it calls is itself compilable (or a candidate). Fixpoint,
  // so a chain caller -> mid -> leaf is credited once mid becomes a candidate.
  for r := 0 to NRegions - 1 do
    Regions[r].B3Candidate := (not Regions[r].Eligible) and Regions[r].EligibleNoCalls and
                              (not Regions[r].HasIndirectCall) and (Length(Regions[r].CallTargets) > 0);
  repeat
    Ordinal := 0;   // reused as "changed" counter
    for r := 0 to NRegions - 1 do
    begin
      if not Regions[r].B3Candidate then Continue;
      for i := 0 to High(Regions[r].CallTargets) do
      begin
        // Target name is the PROC_ label; regions are named without the prefix.
        o := -1;
        for j := 0 to NRegions - 1 do
          if 'PROC_' + Regions[j].Name = Regions[r].CallTargets[i] then begin o := j; Break; end;
        if (o < 0) or not (Regions[o].Eligible or Regions[o].B3Candidate) then
        begin
          Regions[r].B3Candidate := False;
          Inc(Ordinal);
          Break;
        end;
      end;
    end;
  until Ordinal = 0;

  Result := Regions;
end;

procedure AotSurvey(SSAProg: TSSAProgram; Prog: TBytecodeProgram; AllowUnsafe: Boolean);
var
  Regions: TAotRegions;
  r, NElig, NB3: Integer;
  ProcAtEntry: string;
begin
  // The survey runs BEFORE the real compilation, so it has to set the same gate the compiler will;
  // otherwise AOT_DIAG reports an eligibility nobody actually gets. A diagnostic that disagrees with
  // reality is worse than none - it cost an afternoon earlier today.
  GArrStrNative := AllowUnsafe;
  Regions := AotSliceAndClassify(SSAProg, Prog);
  NElig := 0; NB3 := 0;
  for r := 0 to High(Regions) do
  begin
    with Regions[r] do
    begin
      if Eligible then
      begin
        Inc(NElig);
        WriteLn(ErrOutput, Format('[AOT] %-24s blocks=%-4d instrs=%-5d entryPC=%-6d NATIVE',
                                  [Name, LastBlock - FirstBlock + 1, InstrCount, EntryPC]));
      end
      else if B3Candidate then
      begin
        Inc(NB3);
        WriteLn(ErrOutput, Format('[AOT] %-24s blocks=%-4d instrs=%-5d entryPC=%-6d BAIL %s  <- B3-CANDIDATE (calls only)',
                                  [Name, LastBlock - FirstBlock + 1, InstrCount, EntryPC, BailReason]));
      end
      else
        WriteLn(ErrOutput, Format('[AOT] %-24s blocks=%-4d instrs=%-5d entryPC=%-6d BAIL %s',
                                  [Name, LastBlock - FirstBlock + 1, InstrCount, EntryPC, BailReason]));
      // Cross-check the S2 plumbing: the proc map must agree on who owns the entry PC.
      if (Prog <> nil) and (EntryPC >= 0) and (Name <> 'MAIN') then
      begin
        ProcAtEntry := Prog.GetProcNameAt(EntryPC);
        if ProcAtEntry <> Name then
          WriteLn(ErrOutput, Format('[AOT] WARNING: SSA->PC map says %s starts at PC %d but ProcMap owner there is "%s"',
                                    [Name, EntryPC, ProcAtEntry]));
      end;
    end;
  end;
  WriteLn(ErrOutput, Format('[AOT] survey: %d/%d regions eligible (B1+B2 set), %d more would need only B3 native calls',
                            [NElig, Length(Regions), NB3]));
end;

{ ============================ B1a code generation ============================ }

// Compile one eligible region to native x86-64. The encodings mirror the loop
// JIT's validated forms (SedaiJit); operands come from post-regalloc SSA values
// whose bank indexes are composed through the register-compaction remap, so the
// code reads/writes exactly the registers the interpreter uses. Returns nil on
// any condition the B1a subset cannot honor (the region stays interpreted).
function AotCompileRegion(SSAProg: TSSAProgram; Prog: TBytecodeProgram;
                          const Region: TAotRegion; TrueVal: Int64;
                          Modern, AllowUnsafe: Boolean; out BailWhy: string): TExecMem;
const
  // The GPR pool, in the order Allocate hands registers out. r9..r15 first (never callee-saved
  // except r12-r15, and free of ABI meaning here), then the two the emitter never needed: rdi -
  // which is ABI_ARG0 on System V, so it thrashes around calls there, and callee-saved on Win64 -
  // and rbp, which this codegen does not use as a frame pointer (everything is rsp-relative).
  // Neither can appear as a ModRM BASE here (pool registers are only ever a reg field or a
  // mod=11 rm), so rbp's disp-less encoding trap never arises.
  // ...and rsi LAST, which is only ever handed out to a region that touches no float register
  // at all: there the FloatRegs base it normally holds is dead weight. See RsiIsPool.
  IntPool: array[0..9] of Integer = (R9, R10, R11, R12, R13, R14, R15, RDI, RBP, RSI);
type
  TFix = record PatchOff, TargetBlock: Integer; end;  // TargetBlock -1 = epilogue
  // B1b, the linear-scan model. A RANGE is one contiguous run of linear positions, inside ONE
  // block, over which a VM register holds a value. A register's lifetime is a set of ranges, and
  // the gaps between them are the holes an allocator is allowed to give away - which is the whole
  // point: the REGREUSE merge deliberately gives one register number to values with disjoint
  // lifetimes, and the AOT's one-home-per-region allocation then reads that as a single long life.
  // Splitting reads it back apart.
  //
  // Ranges connected by liveness across a CFG edge (live-out of P, live-in of S) carry ONE value
  // and are unioned into a WEB. A web is the allocation unit: fully resident in a machine register
  // or fully memory-homed. Between two webs of the same register there is, by construction, a
  // point where the value is dead - so nothing flows between them and they need no move.
  TLsRange = record
    Bank: Integer;             // 0 = int, 1 = float
    Reg: Integer;              // final VM bank index (post register-compaction remap)
    Blk: Integer;              // region-relative block
    PStart, PEnd: Integer;     // inclusive linear positions
    OpensOnUse: Boolean;       // the first touch READS: the value must already be somewhere
    Wrote: Boolean;            // something WRITES the register inside this range. Not the negation
                               // of OpensOnUse: "d := d + 1" reads the incoming value, so the range
                               // runs on rather than splitting - and it is still a write, so the
                               // bank copy still goes stale and still has to be written back.
    LiveIn, LiveOut: Boolean;  // crosses the block's entry / exit edge
    Web: Integer;              // union-find parent, then the web id
    Weight: Int64;             // loop-weighted touches inside the range
  end;
  TLsWeb = record
    Bank, Reg: Integer;
    PStart, PEnd: Integer;     // span, holes included
    NRange: Integer;
    Weight: Int64;             // loop-weighted touches: what an eviction costs
    NeedsLoad: Boolean;        // opens on a use -> the value arrives through the bank
    HasDef: Boolean;           // written while resident -> the bank copy goes stale
    // Entry points the value can reach WITHOUT passing through this web: a block where it is live
    // in but some predecessor does not have it live out, or a use of a value the dataflow never
    // saw defined. On such a path the machine register holds something else entirely - which is
    // why a web with an uncovered entry anywhere except its own start is not placed at all.
    NUncov: Integer;
    UncovPos: Integer;         // position of the lowest uncovered entry (-1 = none)
    Home: Integer;             // machine register (-1 = stays memory-homed)
    StoreEarly: Boolean;       // its last position is a terminator: write back BEFORE it, or the
                               // store lands after the branch and never runs
  end;
  TLsEventList = array of array of Integer;   // linear position -> web ids
var
  E: TX86Emitter;
  ILoc, FLoc: array of Integer;         // final VM reg -> native reg (or -1)
  IUse, FUse: array of Integer;         // usage counts
  AUse: array of Integer;               // array id -> element-access count (J5c/J6f cache)
  ArrCountNeeded: array of Boolean;     // array id -> has a non-BoundsSafe access (grown with AUse):
                                        // the cached COUNT is consumed only by the non-safe compare;
                                        // a safe access reads just the base and AotArrBound reads the
                                        // descriptor directly - caching the count of an all-safe array
                                        // wastes a GPR a base could use (reload avoided per access).
  MaxIReg, MaxFReg, MaxArrId: Integer;
  IAllocd, FAllocd: array of Integer;   // allocated VM regs in pool order
  NIAlloc, NFAlloc: Integer;
  // Array descriptor cache: base/count of hot arrays held in leftover GPRs for the whole
  // invocation (stable: no DIM/REDIM/ERASE in the op set). Read-only - never flushed.
  NACache: Integer;
  ACacheId, ACacheKind, ACacheReg: array of Integer;   // kind 0 = data base, 1 = count
  SaveGpr: array[0..15] of Boolean;     // GPRs this region must preserve for ITS caller
  SaveXmm: array[6..15] of Boolean;     // xmm6-15 are callee-saved on Win64: one 8-byte frame
                                        // slot each, saved only when the pool actually hands
                                        // the register out
  BlockOff: array of Integer;           // block index -> native offset
  Fixups: array of TFix;
  NFix: Integer;
  EpiOff: Integer;
  // B3: second epilogue entry that SKIPS the register flush (fixup target -2). Used after
  // AotCallSub returns a non-OK value: the caller's registers were already flushed before the
  // call and the CALLEE has since written the banks - re-flushing would overwrite the callee's
  // results with stale caller values. Does the same stack teardown and returns rax.
  BareEpiOff: Integer;
  OK: Boolean;
  HasRecMark, HasDeopt: Boolean;
  RecMarkRoutable: Boolean;             // every recmark op can go through the helper (see below)
  HasNativeRecAlloc: Boolean;           // C6: the region allocates/releases records natively
  RecMarkNative: Boolean;               // C6: emit the marks as leaf calls (never elided)
  ArrClassic: Boolean;                  // array OOB raises (CLASSIC / --bounds-check) -> guard + deopt
  LivenessOK: Boolean;                  // C1: the liveness fixpoint converged
  PeakLiveInt, PeakLiveFloat: Integer;  // C1: peak simultaneously-live values per bank
  // C1 (B1b): the liveness RESULT, kept for the whole region instead of dying with
  // ComputeLiveness. The interval builder and the linear-scan allocator both read it, and the
  // alternative - running the fixpoint again per consumer - is the same work twice on a pass that
  // already iterates to convergence. Indexed [region-relative block][final bank index].
  UseI, DefI, InI, OutI: array of array of Boolean;
  UseF, DefF, InF, OutF: array of array of Boolean;
  LiveNB: Integer;                      // block count the arrays above are sized for; 0 means
                                        // ComputeLiveness bailed and they must not be read
  RsiIsPool: Boolean;                   // the region has no float registers, so rsi is not the
                                        // FloatRegs base here and can carry a value instead
  BlockW: array of Integer;             // region-relative block -> loop-depth weight (Prescan);
                                        // the interval builder weights its ranges with the same
                                        // numbers the static allocator ranks registers by
  // B1b interval model (see the TLsRange/TLsWeb declarations). Built once per region, consumed by
  // the linear-scan allocator; LsOK = False means it bailed and every consumer must stand down.
  LsPos0: array of Integer;             // region-relative block -> its first linear position
  LsNPos: Integer;                      // linear positions in the region (one per instruction)
  LsRanges: array of TLsRange;
  LsNRange: Integer;
  LsWebs: array of TLsWeb;
  LsNWeb: Integer;
  LsOK: Boolean;
  LsWhy: string;                        // why the model was not built (diagnostics)
  // C3 helper calls. The region stops being a leaf function as soon as one is emitted, which
  // is what forces a real frame: 16-byte alignment at the call and the callee's shadow space.
  HasHelperCall: Boolean;
  NHelperCalls: Integer;                // helper calls actually emitted (diagnostics)
  FrameSize: Integer;                   // bytes subtracted from rsp after the pushes (0 = leaf)
  SlotXmm: Integer;                     // [rsp+SlotXmm]   xmm6..15 save area, 8 bytes per
                                        // register in ascending order (-1 = none)
  SlotCtxSave: Integer;                 // [rsp+SlotCtxSave] the TAotCtx pointer (r8 is volatile)
  SlotFltSave: Integer;                 // [rsp+SlotFltSave] the FloatRegs base (rsi is volatile in SysV)
  Cur: TSSAInstruction;
  CurOrd: Integer;                      // ordinal of Cur (indexes the SSA->PC map)
  CurBlkIdx: Integer;                   // absolute block index of Cur (set by the emit loop)
  CurIsBlockLast: Boolean;              // Cur is its block's last instruction (jump elision)
  LabelIdx: TStringList;                // region-local label -> block-list index
  HelperOps: TStringList;               // diagnostics: op name of every helper call emitted

  // Dynamic within-block float allocation (AOT_DYNF). A machine xmm is time-multiplexed across
  // block-local temporaries with disjoint lifetimes, keeping the hot float traffic the static
  // home allocator leaves in memory in registers instead. Correctness rests on two invariants
  // checked when a temp is admitted: it is BLOCK-LOCAL (all touches in one block, not live-out,
  // so nothing crosses a block boundary and the epilogue/other blocks never see it) and
  // DEF-BEFORE-USE (its first touch is the defining store, so no use reads the xmm before it is
  // written - the implicit-zero-read hazard). FLoc is then updated as emission walks positions:
  // set at the def, cleared at the last touch. The four sites that hand values to the
  // interpreter/callee through the banks (helper call, native call-sub, C5 leaf call, deopt
  // exit) flush the currently-resident set first.
  DynFActive: Boolean;
  DynFHomeReg: array of Integer;        // region position -> VM float reg defined here that gets a home (-1)
  DynFHomeXmm: array of Integer;        // region position -> the xmm (2..7) assigned to it
  DynFFree: array of array of Integer;  // region position -> VM float regs whose last touch is here
  DynFCur: array[0..15] of Integer;     // xmm index -> VM float reg currently resident there (-1 free)
  DynPos: Integer;                      // running region position during emission
  // Same scheme for integers (c). The GPR pool r9..r15 is shared with the array-descriptor cache,
  // so the dynamic pool is IntPool MINUS the GPRs Allocate handed to array bases/counts; those
  // stay pinned for the whole invocation. Scratch is rax/rcx/rdx (never in the pool), the GPR
  // analogue of xmm0/xmm1 for floats.
  DynIActive: Boolean;
  DynIHomeReg: array of Integer;        // region position -> VM int reg defined here that gets a home (-1)
  DynIHomeGpr: array of Integer;        // region position -> the GPR (R9..R15) assigned to it
  DynIFree: array of array of Integer;  // region position -> VM int regs whose last touch is here
  DynICur: array[0..15] of Integer;     // GPR number -> VM int reg currently resident there (-1 free)
  // B1b: the interval allocator's schedule. Same idea as DynF*/DynI* - events indexed by linear
  // position - but a position can start SEVERAL webs at once (every value live in to a block starts
  // its range at the block's first position), so these are lists, and they carry a WEB id rather
  // than a register: the web record already says which register, which machine register, whether
  // the value arrives through the bank and whether it has to be written back.
  LsActive: Boolean;
  LsTakeAt: TLsEventList;               // position -> web ids that take their machine home here
  LsFreeAt: TLsEventList;               // position -> web ids whose life ends here
  // AOT_DUMP (observational only). DumpOn is read ONCE per region so the emit loop tests a boolean
  // and never the environment; every recording site is guarded by it at the CALL, not inside Note,
  // because the argument is a Format() that would otherwise be built on every instruction of every
  // compilation. DumpAt/DumpTxt are a parallel array of (native offset, what the emitter was about
  // to do), in emission order - which is already offset order.
  DumpOn: Boolean;
  DumpAt: array of Integer;
  DumpTxt: array of string;
  NNote: Integer;
  DumpHdr: TStringList;                 // the register legend, captured BEFORE the body: ILoc/FLoc
                                        // are mutated by the dynamic allocators as emission walks

  procedure Fail(const Why: string);
  begin
    if OK then BailWhy := Why;
    OK := False;
  end;

  // AOT_DUMP: remember that whatever is emitted from the CURRENT offset on is S. Never called
  // unless DumpOn (the guard lives at the call site - see the declaration).
  procedure DumpNote(const S: string);
  begin
    if NNote = Length(DumpAt) then
    begin
      SetLength(DumpAt, NNote * 2 + 64);
      SetLength(DumpTxt, NNote * 2 + 64);
    end;
    DumpAt[NNote] := E.Len;
    DumpTxt[NNote] := S;
    Inc(NNote);
  end;

  // Mapped bytecode PC of the current instruction; Fail when the map has none.
  function NeedPC: Integer;
  begin
    Result := Prog.GetSsaPc(CurOrd);
    if Result < 0 then Fail('no-pc:' + OpName(Cur.OpCode));
  end;

  // Final interpreter bank index of a register operand (bail on shape surprises).
  function IReg(const V: TSSAValue): Integer;
  begin
    Result := 0;
    if (V.Kind <> svkRegister) or (V.RegType <> srtInt) then
      Fail('operand:' + OpName(Cur.OpCode))
    else begin
      Result := Prog.AotRemapIntReg(V.RegIndex);
      if Result < 0 then Fail('unmapped-reg:' + OpName(Cur.OpCode));
    end;
  end;
  function FReg(const V: TSSAValue): Integer;
  begin
    Result := 0;
    if (V.Kind <> svkRegister) or (V.RegType <> srtFloat) then
      Fail('operand:' + OpName(Cur.OpCode))
    else begin
      Result := Prog.AotRemapFloatReg(V.RegIndex);
      if Result < 0 then Fail('unmapped-reg:' + OpName(Cur.OpCode));
    end;
  end;
  function CInt(const V: TSSAValue): Int64;
  begin
    Result := 0;
    if V.Kind <> svkConstInt then Fail('const-operand:' + OpName(Cur.OpCode))
    else Result := V.ConstInt;
  end;
  // C5: a string operand's slot index in the StringRegs bank (never register-allocated).
  function SReg(const V: TSSAValue): Integer;
  begin
    Result := 0;
    if (V.Kind <> svkRegister) or (V.RegType <> srtString) then
      Fail('str-operand:' + OpName(Cur.OpCode))
    else begin
      Result := Prog.AotRemapStringReg(V.RegIndex);
      if Result < 0 then Fail('unmapped-str:' + OpName(Cur.OpCode));
    end;
  end;

  { --- emission helpers (mirrors of the loop JIT's validated encoders) --- }

  procedure AddFixup(AOff, ABlock: Integer);
  begin
    if NFix >= Length(Fixups) then SetLength(Fixups, NFix * 2 + 8);
    Fixups[NFix].PatchOff := AOff;
    Fixups[NFix].TargetBlock := ABlock;
    Inc(NFix);
  end;
  procedure JmpRel(TargetBlock: Integer);
  begin
    E.Emit8($E9); AddFixup(E.Len, TargetBlock); E.Emit32(0);
  end;
  procedure JccRel(CC: Byte; TargetBlock: Integer);
  begin
    E.Emit8($0F); E.Emit8(CC); AddFixup(E.Len, TargetBlock); E.Emit32(0);
  end;
  // Exit to the interpreter at absolute bytecode PC (deopt and normal exits alike):
  // the epilogue flushes the allocated registers, so the interpreter resumes coherent.
  // AOT_DYNF: store / reload every dynamically-resident float temp to/from its bank slot. The
  // flush makes the banks coherent for anything that reads them through rsi (the interpreter on
  // a deopt, a helper handler, a callee); the reload brings the values back into their xmm to
  // continue native. Emission-time DynFCur is a faithful map of runtime residency at this
  // position (the schedule is deterministic per linear position), so the emitted stores match
  // exactly what is live in registers here. No-op unless the dynamic allocator is active.
  { --- SSE encodings that also work for xmm8-15 -----------------------------------------------
    An SSE instruction is <legacy prefix> [REX] 0F <op> <ModRM>: the REX byte goes AFTER the
    F2/F3/66 prefix and BEFORE the 0F escape, which is why these cannot be expressed by prepending
    bytes to the emitter's generic MemOp. When neither register is extended NO REX is emitted, so
    every encoding this unit produced before comes out byte for byte identical - that is what makes
    routing the float emitter through here a zero-behaviour change, and what lets the pool grow
    past xmm7 afterwards. ------------------------------------------------------------------- }
  procedure SseRR(const Op: array of Byte; RegField, RmReg: Integer);
  var i, rex: Integer;
  begin
    i := 0;
    while (i < Length(Op)) and ((Op[i] = $F2) or (Op[i] = $F3) or (Op[i] = $66)) do
    begin E.Emit8(Op[i]); Inc(i); end;
    rex := 0;
    if RegField >= 8 then rex := rex or $04;         // REX.R
    if RmReg    >= 8 then rex := rex or $01;         // REX.B
    if rex <> 0 then E.Emit8($40 or rex);
    while i < Length(Op) do begin E.Emit8(Op[i]); Inc(i); end;
    E.Emit8($C0 or ((RegField and 7) shl 3) or (RmReg and 7));
  end;

  // The REX.W forms that cross the banks (cvtsi2sd xmm, r64): F2 REX.W[R][B] 0F <op> ModRM.
  procedure SseWRex(const Op: array of Byte; XmmReg, GprReg: Integer);
  var rex: Byte;
  begin
    rex := $48;
    if XmmReg >= 8 then rex := rex or $04;
    if GprReg >= 8 then rex := rex or $01;
    E.Emit8($F2); E.Emit8(rex); E.Emit8(Op[0]); E.Emit8(Op[1]);
    E.Emit8($C0 or ((XmmReg and 7) shl 3) or (GprReg and 7));
  end;

  procedure SseMem(const Op: array of Byte; RegField, BaseReg: Integer; Disp: LongWord);
  var i, rex: Integer;
  begin
    i := 0;
    while (i < Length(Op)) and ((Op[i] = $F2) or (Op[i] = $F3) or (Op[i] = $66)) do
    begin E.Emit8(Op[i]); Inc(i); end;
    rex := 0;
    if RegField >= 8 then rex := rex or $04;
    if BaseReg  >= 8 then rex := rex or $01;
    if rex <> 0 then E.Emit8($40 or rex);
    while i < Length(Op) do begin E.Emit8(Op[i]); Inc(i); end;
    E.Emit8($80 or ((RegField and 7) shl 3) or (BaseReg and 7));
    E.Emit32(Disp);
  end;

  // The four routines below keep the DYNAMIC residency (AOT_DYNF's within-block temps and B1b's
  // interval webs, which share DynFCur/DynICur as their runtime map) in step with the banks
  // wherever a value leaves this code's hands: a runtime helper, a native call, a leaf string
  // primitive, a deopt exit.
  procedure FlushResidentF;
  var x: Integer;
  begin
    if not (DynFActive or LsActive) then Exit;
    for x := 2 to 15 do
      if DynFCur[x] >= 0 then
        SseMem([$F2, $0F, $11], x, RSI, LongWord(DynFCur[x]) * 8);   // movsd [rsi+reg*8], xmm x
  end;
  procedure ReloadResidentF;
  var x: Integer;
  begin
    if not (DynFActive or LsActive) then Exit;
    for x := 2 to 15 do
      if DynFCur[x] >= 0 then
        SseMem([$F2, $0F, $10], x, RSI, LongWord(DynFCur[x]) * 8);   // movsd xmm x, [rsi+reg*8]
  end;
  // AOT_DYNF int counterpart: store / reload the dynamically-resident int temps through the int
  // bank (rbx). Pool GPRs are r9..r15; the store/load helpers bake the right REX for extended regs.
  // mov [rbx+reg*8], g / mov g, [rbx+reg*8] emitted raw (StoreRegMem/LoadRegMem are defined below
  // this point; the pool GPRs are all >= r8 so REX.R is always set).
  procedure FlushResidentI;
  var g: Integer;
  begin
    if not (DynIActive or LsActive) then Exit;
    for g := R9 to R15 do
      if DynICur[g] >= 0 then
      begin
        E.Emit8($4C); E.Emit8($89);                                    // REX.WR, mov r/m,r
        E.Emit8($80 or ((g and 7) shl 3) or RBX); E.Emit32(LongWord(DynICur[g]) * 8);
      end;
  end;
  procedure ReloadResidentI;
  var g: Integer;
  begin
    if not (DynIActive or LsActive) then Exit;
    for g := R9 to R15 do
      if DynICur[g] >= 0 then
      begin
        E.Emit8($4C); E.Emit8($8B);                                    // REX.WR, mov r,r/m
        E.Emit8($80 or ((g and 7) shl 3) or RBX); E.Emit32(LongWord(DynICur[g]) * 8);
      end;
  end;

  procedure ExitTo(apc: Integer);
  begin
    FlushResidentF; FlushResidentI;                 // dynamic temps -> banks (epilogue won't)
    E.EmitBytes([$B8]); E.Emit32(LongWord(apc));   // mov eax, apc
    JmpRel(-1);                                     // jmp epilogue
  end;

  procedure EmitRR(const Op: array of Byte; regField, rmReg: Integer);
  var rex: Byte; k: Integer;
  begin
    rex := $48;
    if regField >= 8 then rex := rex or $04;
    if rmReg    >= 8 then rex := rex or $01;
    E.Emit8(rex);
    for k := 0 to High(Op) do E.Emit8(Op[k]);
    E.Emit8($C0 or ((regField and 7) shl 3) or (rmReg and 7));
  end;
  procedure MovRR(dst, src: Integer);
  begin EmitRR([$89], src, dst); end;
  procedure MovImm64(natreg: Integer; imm: Int64);
  var rex: Byte;
  begin
    rex := $48; if natreg >= 8 then rex := rex or $01;
    E.Emit8(rex); E.Emit8($B8 or (natreg and 7)); E.Emit64(QWord(imm));
  end;
  // lea dst, [base+disp32]. Used for &StringRegs[i] (base is always rax here, so no SIB case).
  procedure Lea(dst, base: Integer; disp: LongWord);
  var rex: Byte;
  begin
    rex := $48; if dst >= 8 then rex := rex or $04; if base >= 8 then rex := rex or $01;
    E.Emit8(rex); E.Emit8($8D);
    E.Emit8($80 or ((dst and 7) shl 3) or (base and 7)); E.Emit32(disp);
  end;
  // mov dst, [base+disp32] with a REX computed for either register being an extended reg (r8-r15).
  // MemOp bakes REX into its opcode bytes, which is fine for fixed regs but not for an ABI arg
  // register that is r8 on Win64 (needs REX.R) - so the string emitters use this instead.
  procedure MovLoad(dst, base: Integer; disp: LongWord);
  var rex: Byte;
  begin
    rex := $48; if dst >= 8 then rex := rex or $04; if base >= 8 then rex := rex or $01;
    E.Emit8(rex); E.Emit8($8B);
    E.Emit8($80 or ((dst and 7) shl 3) or (base and 7)); E.Emit32(disp);
  end;
  procedure LoadRegMem(natreg: Integer; disp: LongWord);   // mov natreg,[rbx+disp]
  var rex: Byte;
  begin
    rex := $48; if natreg >= 8 then rex := rex or $04;
    E.Emit8(rex); E.Emit8($8B);
    E.Emit8($80 or ((natreg and 7) shl 3) or RBX); E.Emit32(disp);
  end;
  procedure StoreRegMem(natreg: Integer; disp: LongWord);  // mov [rbx+disp],natreg
  var rex: Byte;
  begin
    rex := $48; if natreg >= 8 then rex := rex or $04;
    E.Emit8(rex); E.Emit8($89);
    E.Emit8($80 or ((natreg and 7) shl 3) or RBX); E.Emit32(disp);
  end;
  function IAlloc(vmreg: Integer): Integer;
  begin
    if vmreg <= MaxIReg then Result := ILoc[vmreg] else Result := -1;
    // The account that decides between two allocators: how much loop-weighted operand traffic the
    // EMITTED code sends to the bank. A register with no home HERE is a memory access at this
    // point, and the block's loop weight is what it is worth. The residency figures above model a
    // policy; this one counts instructions.
    if (Result < 0) and (CurBlkIdx >= Region.FirstBlock) and (CurBlkIdx <= Region.LastBlock) then
      AotDiagMemAccI := AotDiagMemAccI + BlockW[CurBlkIdx - Region.FirstBlock];
  end;
  // ILoadArg is only valid BEFORE the sequence clobbers anything: SpillVolatiles writes the
  // volatile-homed values to the bank but leaves ILoc pointing at the machine register, so ILoadArg
  // still reads that register - and a leaf-call sequence typically overwrites rax (bank base) and
  // rcx/rdx/r8/r9 (the ABI slots) on the way. Reading an operand after that gave the BANK BASE
  // POINTER as an integer operand: "s[i]" answered 0 because its start arrived as 0x1_0004_2380.
  //
  // After a spill the truth is: a CALLEE-SAVED home still holds the value (nothing here touches
  // those), everything else is authoritative in the bank. This variant asks that question, so the
  // caller no longer has to get an ordering right that nothing checks.
  procedure ILoadArgSpilled(argReg, vmreg: Integer);
  var n: Integer;
  begin
    n := IAlloc(vmreg);
    if (n >= 0) and GprIsCalleeSaved(n) then MovRR(argReg, n)
    else MovLoad(argReg, RBX, LongWord(vmreg) * 8);
  end;
  procedure ILoad(scr, vmreg: Integer);       // scratch (rax/rcx/rdx) := VM int reg
  var n: Integer;
  begin
    n := IAlloc(vmreg);
    if n >= 0 then MovRR(scr, n)
    else E.MemOp([$48, $8B], scr, RBX, LongWord(vmreg) * 8);
  end;
  // ILoad for an ABI ARGUMENT register: arg2/arg3 are r8/r9 on Win64, and ILoad's memory
  // path bakes REX $48 (low-register form) - loading into r8 would actually encode rax.
  // MovRR (via EmitRR) and MovLoad both compute the REX for extended targets.
  procedure ILoadArg(argReg, vmreg: Integer);
  var n: Integer;
  begin
    n := IAlloc(vmreg);
    if n >= 0 then MovRR(argReg, n)
    else MovLoad(argReg, RBX, LongWord(vmreg) * 8);
  end;
  procedure IStore(vmreg, scr: Integer);
  var n: Integer;
  begin
    n := IAlloc(vmreg);
    if n >= 0 then MovRR(n, scr)
    else E.MemOp([$48, $89], scr, RBX, LongWord(vmreg) * 8);
  end;
  procedure IOp(const MemForm: array of Byte; scr, vmreg: Integer);
  var rest: array of Byte; k, n: Integer;
  begin
    n := IAlloc(vmreg);
    if n >= 0 then
    begin
      SetLength(rest, Length(MemForm) - 1);   // drop the $48 REX; EmitRR rebuilds it
      for k := 1 to High(MemForm) do rest[k - 1] := MemForm[k];
      EmitRR(rest, scr, n);
    end
    else
    begin
      // Memory operand. MemForm[0] is REX.W; MemOp bakes it verbatim, so add REX.R here for an
      // extended scratch (r8..r15) - the in-place int path uses pool GPRs as the accumulator.
      SetLength(rest, Length(MemForm));
      for k := 0 to High(MemForm) do rest[k] := MemForm[k];
      if scr >= 8 then rest[0] := rest[0] or $04;
      E.MemOp(rest, scr, RBX, LongWord(vmreg) * 8);
    end;
  end;
  function FAlloc(vmreg: Integer): Integer;
  begin
    if vmreg <= MaxFReg then Result := FLoc[vmreg] else Result := -1;
    if (Result < 0) and (CurBlkIdx >= Region.FirstBlock) and (CurBlkIdx <= Region.LastBlock) then
      AotDiagMemAccF := AotDiagMemAccF + BlockW[CurBlkIdx - Region.FirstBlock];
  end;
  // MEASURED AND REJECTED (23 Jul 2026): caching "which VM float register xmm0 currently holds"
  // and dropping the redundant FLoad of the next float op in a chain (~37% of consecutive
  // float-op pairs on the n-body forward dest->src1) buys NOTHING - interleaved A/B on one
  // binary, best-of-9: 0.679 s with, 0.671 s without. The two reasons it cannot pay here: the
  // hottest float registers are already bound to xmm2..xmm7, so the "elided" load was a
  // register-to-register movsd the renamer executes for free; and for a memory-resident
  // register the load right after its store is served by the hardware store buffer anyway.
  // Do not re-attempt without new evidence; the float traffic that costs is elsewhere.
  procedure FLoad(Wx, vmreg: Integer);
  var n: Integer;
  begin
    n := FAlloc(vmreg);
    if n >= 0 then
    begin
      // Register-to-register copy uses movaps, not movsd. movsd xmm,xmm MERGES (low 64 only),
      // so it is a partial-register write: not move-eliminated and false-dependent on the
      // destination's upper bits. movaps copies all 128 bits, is move-eliminated on Ivy Bridge+
      // (zero uop), and the upper half is never read by any scalar-double op - so the whole
      // xmm0 round-trip a dynamic-allocated op still emits costs nothing.
      if n <> Wx then SseRR([$0F, $28], Wx, n);                          // movaps Wx, n
    end
    else SseMem([$F2, $0F, $10], Wx, RSI, LongWord(vmreg) * 8);
  end;
  procedure FOp(const SseOp: array of Byte; Wx, vmreg: Integer);
  var n: Integer;
  begin
    n := FAlloc(vmreg);
    if n >= 0 then
      SseRR([SseOp[0], SseOp[1], SseOp[2]], Wx, n)
    else
      SseMem(SseOp, Wx, RSI, LongWord(vmreg) * 8);
  end;
  procedure FStore(vmreg, Wx: Integer);
  var n: Integer;
  begin
    n := FAlloc(vmreg);
    if n >= 0 then
    begin
      if n <> Wx then SseRR([$0F, $28], n, Wx);                          // movaps n, Wx (see FLoad)
    end
    else SseMem([$F2, $0F, $11], Wx, RSI, LongWord(vmreg) * 8);
  end;

  // al holds 0/1 -> dest := TrueVal/0 (dest = current instruction's Dest int reg).
  procedure CmpBoolToDest;
  begin
    E.EmitBytes([$0F, $B6, $C0]);                       // movzx eax,al
    if TrueVal = -1 then
      E.EmitBytes([$48, $F7, $D8])                      // neg rax
    else if TrueVal <> 1 then
    begin E.EmitBytes([$48, $69, $C0]); E.Emit32(LongWord(TrueVal and $FFFFFFFF)); end;
    IStore(IReg(Cur.Dest), RAX);
  end;
  procedure IntCmp(SetCC: Byte);
  begin
    ILoad(RAX, IReg(Cur.Src1));
    IOp([$48, $3B], RAX, IReg(Cur.Src2));               // cmp rax, src2
    E.EmitBytes([$0F, SetCC, $C0]);                     // setcc al
    CmpBoolToDest;
  end;
  // Kind: 0=Lt 1=Le 2=Gt 3=Ge 4=Eq 5=Ne (ordered IEEE, NaN-correct - JIT J8 pattern).
  procedure FloatCmp(Kind: Integer);
  begin
    FLoad(XMM0, FReg(Cur.Src1));
    FLoad(XMM1, FReg(Cur.Src2));
    case Kind of
      0: begin E.EmitBytes([$66, $0F, $2E, $C8]); E.EmitBytes([$0F, $97, $C0]); end;
      1: begin E.EmitBytes([$66, $0F, $2E, $C8]); E.EmitBytes([$0F, $93, $C0]); end;
      2: begin E.EmitBytes([$66, $0F, $2E, $C1]); E.EmitBytes([$0F, $97, $C0]); end;
      3: begin E.EmitBytes([$66, $0F, $2E, $C1]); E.EmitBytes([$0F, $93, $C0]); end;
      4: begin E.EmitBytes([$66, $0F, $2E, $C1]);
               E.EmitBytes([$0F, $94, $C0]); E.EmitBytes([$0F, $9B, $C1]);
               E.EmitBytes([$20, $C8]); end;
      5: begin E.EmitBytes([$66, $0F, $2E, $C1]);
               E.EmitBytes([$0F, $95, $C0]); E.EmitBytes([$0F, $9A, $C1]);
               E.EmitBytes([$08, $C8]); end;
    end;
    CmpBoolToDest;
  end;
  // dest := src1 <op> src2. When dest has a machine home we compute IN PLACE - directly into
  // that register - instead of round-tripping through the xmm0 scratch: the accumulator form
  // dest==src1 collapses to a single `<op>sd Hd, src2`, and the general homed case drops the
  // final store. Commutative ops (add/mul) may also fold dest==src2. The only case that still
  // needs the scratch is a non-commutative op whose dest aliases src2 (loading src1 into the
  // dest home would clobber src2 before the op reads it), plus a memory-homed dest.
  procedure FloatBin(const SseOp: array of Byte; Commutative: Boolean);
  var Hd, Hs1, Hs2, d, s1, s2: Integer;
  begin
    d := FReg(Cur.Dest); s1 := FReg(Cur.Src1); s2 := FReg(Cur.Src2); if not OK then Exit;
    Hd := FAlloc(d);
    if Hd < 0 then
    begin
      FLoad(XMM0, s1); FOp(SseOp, XMM0, s2); FStore(d, XMM0);       // memory-homed dest
      Exit;
    end;
    Hs1 := FAlloc(s1); Hs2 := FAlloc(s2);
    if Hd = Hs1 then
      FOp(SseOp, Hd, s2)                                            // Hd already holds src1
    else if Commutative and (Hd = Hs2) then
      FOp(SseOp, Hd, s1)                                            // Hd holds src2, op commutes
    else if Hd <> Hs2 then
    begin
      FLoad(Hd, s1);                                               // Hd <- src1
      FOp(SseOp, Hd, s2);                                          // Hd := src1 op src2
    end
    else
    begin
      FLoad(XMM0, s1); FOp(SseOp, XMM0, s2); FStore(d, XMM0);       // non-commutative dest==src2
    end;
  end;
  // Integer analogue of FloatBin: dest := src1 <op> src2 computed in place in the dest's GPR home
  // when it has one (rax scratch round-trip only for a memory-homed dest or the non-commutative
  // dest==src2 alias). MemForm is the [$48, opcode...] memory-form encoding; IOp/ILoadArg build
  // the right REX for the extended pool GPRs.
  procedure IntBin(const MemForm: array of Byte; Commutative: Boolean);
  var Hd, Hs1, Hs2, d, s1, s2: Integer;
  begin
    d := IReg(Cur.Dest); s1 := IReg(Cur.Src1); s2 := IReg(Cur.Src2); if not OK then Exit;
    Hd := IAlloc(d);
    if Hd < 0 then
    begin
      ILoad(RAX, s1); IOp(MemForm, RAX, s2); IStore(d, RAX);        // memory-homed dest
      Exit;
    end;
    Hs1 := IAlloc(s1); Hs2 := IAlloc(s2);
    if Hd = Hs1 then
      IOp(MemForm, Hd, s2)                                          // Hd already holds src1
    else if Commutative and (Hd = Hs2) then
      IOp(MemForm, Hd, s1)                                          // Hd holds src2, op commutes
    else if Hd <> Hs2 then
    begin
      ILoadArg(Hd, s1);                                            // Hd <- src1 (extended-safe load)
      IOp(MemForm, Hd, s2);                                        // Hd := src1 op src2
    end
    else
    begin
      ILoad(RAX, s1); IOp(MemForm, RAX, s2); IStore(d, RAX);        // non-commutative dest==src2
    end;
  end;
  // dest(int) := cvt(src1 float). Op2 is the two opcode bytes after the F2/REX.W prefix
  // (2D = cvtsd2si round, 2C = cvttsd2si truncate). Writes the dest GPR directly when it has a
  // home - no xmm0 load, no store - reading src1 as a register or straight from its bank slot.
  procedure CvtFloatToInt(const Op2: array of Byte);
  var Hd, hs1, d, s1: Integer; rex: Byte;
  begin
    d := IReg(Cur.Dest); s1 := FReg(Cur.Src1); if not OK then Exit;
    Hd := IAlloc(d);
    if Hd < 0 then
    begin
      FLoad(XMM0, s1);
      E.EmitBytes([$F2, $48, Op2[0], Op2[1], $C0]);                 // cvt rax, xmm0
      IStore(d, RAX);
      Exit;
    end;
    rex := $48; if Hd >= 8 then rex := rex or $04;                  // REX.R for extended dest GPR
    hs1 := FAlloc(s1);
    if hs1 >= 8 then rex := rex or $01;                             // REX.B for extended xmm source
    E.Emit8($F2); E.Emit8(rex); E.Emit8(Op2[0]); E.Emit8(Op2[1]);
    if hs1 >= 0 then
      E.Emit8($C0 or ((Hd and 7) shl 3) or (hs1 and 7))             // cvt Hd, xmm_src
    else
    begin
      E.Emit8($80 or ((Hd and 7) shl 3) or RSI); E.Emit32(LongWord(s1) * 8);   // cvt Hd, [rsi+off]
    end;
  end;
  // C7: n \ C and n Mod C for a CONSTANT C, without idiv. The divisor's register is known to hold
  // one value, stamped on the instruction by the SSA, so the sequence is the classic multiply-high:
  //     t = mulhi(M, n);  [t += n | t -= n];  t >>= s;  q = t + (t >>> 63)
  // and the remainder comes back as n - q*C. Measured worth on pidigits: two idiv per limb are 23,5
  // of the 26,2 ns a limb costs, and the program walks 25 M limbs.
  //
  // Returns False when the constant is not one this path handles (0, ±1, or a magnitude that does
  // not fit the imm32 of the remainder's multiply): the caller then emits the historical idiv,
  // which also keeps the divide-by-zero and INT64_MIN/-1 traps exactly where they were.
  function TryDivModConst(apc: Integer; WantRemainder: Boolean): Boolean;
  var
    d, M: Int64;
    s: Integer;
    NeedAdd, NeedSub: Boolean;
  begin
    Result := False;
    if not AotDivConstNative then Exit;
    if (apc < 0) or (Prog = nil) then Exit;
    // The divisor is NOT recoverable from the register here: after allocation that number carries
    // several values. It arrives stamped on the instruction by the SSA (AnnotateDivByConst).
    d := Prog.GetInstruction(apc).Immediate;
    if d = 0 then begin Inc(GDivConstMiss); Exit; end;                            // not annotated
    if (d = 1) or (d = -1) then begin Inc(GDivConstMiss); Exit; end;              // identity / corner
    if WantRemainder and ((d > High(LongInt)) or (d < Low(LongInt))) then
      begin Inc(GDivConstMiss); Exit; end;                                        // imm32 for imul
    Inc(GDivConstHit);
    AotMagicSigned(d, M, s, NeedAdd, NeedSub);

    ILoad(RCX, IReg(Cur.Src1));                   // rcx = n (kept: the remainder needs it)
    MovImm64(RAX, M);
    E.EmitBytes([$48, $F7, $E9]);                 // imul rcx        -> rdx:rax = M * n (signed)
    MovRR(RAX, RDX);                              // rax = mulhi
    if NeedAdd then E.EmitBytes([$48, $01, $C8])  // add rax, rcx
    else if NeedSub then E.EmitBytes([$48, $29, $C8]);  // sub rax, rcx
    if s > 0 then begin E.EmitBytes([$48, $C1, $F8]); E.Emit8(Byte(s)); end;   // sar rax, s
    MovRR(RDX, RAX);
    E.EmitBytes([$48, $C1, $EA, $3F]);            // shr rdx, 63     -> the sign bit
    E.EmitBytes([$48, $01, $D0]);                 // add rax, rdx    -> rax = quotient
    if WantRemainder then
    begin
      E.EmitBytes([$48, $69, $D0]); E.Emit32(LongWord(Int64(LongInt(d))));  // imul rdx, rax, d
      E.EmitBytes([$48, $29, $D1]);               // sub rcx, rdx    -> rcx = n - q*d
      IStore(IReg(Cur.Dest), RCX);
    end
    else
      IStore(IReg(Cur.Dest), RAX);
    Result := True;
  end;

  // Signed div/mod with the interpreter's raise semantics via deopt (JIT J10 pattern).
  procedure DivModSigned(apc: Integer; WantRemainder: Boolean);
  var p1, p2: Integer;
  begin
    if TryDivModConst(apc, WantRemainder) then Exit;
    ILoad(RAX, IReg(Cur.Src1));
    ILoad(RCX, IReg(Cur.Src2));
    E.EmitBytes([$48, $85, $C9]);                 // test rcx,rcx
    E.EmitBytes([$75, $00]); p1 := E.Len - 1;     // jnz +skip
    ExitTo(apc);                                   // /0 -> interpreter raises
    E.PatchByte(p1, Byte(E.Len - (p1 + 1)));
    // INT64_MIN / -1 faults in idiv; the interpreter's FPC result needs a deopt too.
    E.EmitBytes([$48, $83, $F9, $FF]);            // cmp rcx,-1
    E.EmitBytes([$75, $00]); p1 := E.Len - 1;     // jnz +ok
    MovImm64(RDX, Int64($8000000000000000));
    E.EmitBytes([$48, $39, $D0]);                 // cmp rax,rdx
    E.EmitBytes([$75, $00]); p2 := E.Len - 1;     // jnz +ok
    ExitTo(apc);
    E.PatchByte(p2, Byte(E.Len - (p2 + 1)));
    E.PatchByte(p1, Byte(E.Len - (p1 + 1)));
    E.EmitBytes([$48, $99]);                      // cqo
    E.EmitBytes([$48, $F7, $F9]);                 // idiv rcx
    if WantRemainder then IStore(IReg(Cur.Dest), RDX)
    else IStore(IReg(Cur.Dest), RAX);
  end;
  procedure DivModUnsigned(apc: Integer; WantRemainder: Boolean);
  var p1: Integer;
  begin
    ILoad(RAX, IReg(Cur.Src1));
    ILoad(RCX, IReg(Cur.Src2));
    E.EmitBytes([$48, $85, $C9]);                 // test rcx,rcx
    E.EmitBytes([$75, $00]); p1 := E.Len - 1;     // jnz +skip
    ExitTo(apc);
    E.PatchByte(p1, Byte(E.Len - (p1 + 1)));
    E.EmitBytes([$31, $D2]);                      // xor edx,edx
    E.EmitBytes([$48, $F7, $F1]);                 // div rcx
    if WantRemainder then IStore(IReg(Cur.Dest), RDX)
    else IStore(IReg(Cur.Dest), RAX);
  end;
  // ArithShr64 / LogicalShr64 saturating semantics (NOT the hardware masked shift).
  procedure ShrSat(Arith: Boolean);
  var pKeep, pDo, pDone: Integer;
  begin
    ILoad(RAX, IReg(Cur.Src1));
    ILoad(RCX, IReg(Cur.Src2));
    E.EmitBytes([$48, $85, $C9]);                 // test rcx,rcx
    E.EmitBytes([$7E, $00]); pKeep := E.Len - 1;  // jle @done (count<=0 -> value)
    E.EmitBytes([$48, $83, $F9, $40]);            // cmp rcx,64
    E.EmitBytes([$7C, $00]); pDo := E.Len - 1;    // jl @shift
    if Arith then
      E.EmitBytes([$48, $C1, $F8, $3F])           // sar rax,63 (saturate to sign)
    else
      E.EmitBytes([$31, $C0]);                    // xor eax,eax (saturate to 0)
    E.EmitBytes([$EB, $00]); pDone := E.Len - 1;  // jmp @done
    E.PatchByte(pDo, Byte(E.Len - (pDo + 1)));
    if Arith then
      E.EmitBytes([$48, $D3, $F8])                // sar rax,cl
    else
      E.EmitBytes([$48, $D3, $E8]);               // shr rax,cl
    E.PatchByte(pDone, Byte(E.Len - (pDone + 1)));
    E.PatchByte(pKeep, Byte(E.Len - (pKeep + 1)));
    IStore(IReg(Cur.Dest), RAX);
  end;
  // Load the Xfer bank base (slot 0 = XferInt, 1 = XferFloat) from the AOT ctx (r8) into rdx.
  { --- C3: runtime-helper call ------------------------------------------------------------ }

  // [rsp+disp32]. Not MemOp: rsp is the one base whose ModRM rm field (100) means "a SIB
  // byte follows", so the operand needs an explicit SIB $24 (index=none, base=rsp).
  procedure FrameMem(const Op: array of Byte; natreg: Integer; disp: Integer);
  var rex: Byte; k: Integer;
  begin
    rex := $48; if natreg >= 8 then rex := rex or $04;
    E.Emit8(rex);
    for k := 0 to High(Op) do E.Emit8(Op[k]);
    E.Emit8($80 or ((natreg and 7) shl 3) or RSP);   // mod=10, rm=100 -> SIB
    E.Emit8($24);                                    // SIB: no index, base = rsp
    E.Emit32(LongWord(disp));
  end;
  procedure FrameStore(natreg, disp: Integer);   // mov [rsp+disp], natreg
  begin FrameMem([$89], natreg, disp); end;
  procedure FrameLoad(natreg, disp: Integer);    // mov natreg, [rsp+disp]
  begin FrameMem([$8B], natreg, disp); end;
  // movsd xmm <-> [rsp+disp]. Its own encoder because the $F2 prefix goes BEFORE any REX,
  // which FrameMem's REX-first order cannot express (and xmm0-7 need no REX here anyway).
  procedure FrameXmm(IsStore: Boolean; Wx, disp: Integer);
  begin
    E.Emit8($F2);
    if Wx >= 8 then E.Emit8($44);                    // REX.R
    E.Emit8($0F);
    if IsStore then E.Emit8($11) else E.Emit8($10);
    E.Emit8($80 or ((Wx and 7) shl 3) or RSP);
    E.Emit8($24);
    E.Emit32(LongWord(disp));
  end;

  // Load the cached array descriptor slots (data base / element count) from ctx.ArrDesc.
  // Invariant for a whole invocation, so the prologue does this once - but a helper call
  // clobbers the registers holding them, so it has to be repeatable.
  procedure ReloadArrayCache;
  var k, b: Integer; rex: Byte;
  begin
    if NACache = 0 then Exit;
    E.MemOp([$49, $8B], RDX, R8, AOTCTX_ARRDESC);   // rdx = ctx.ArrDesc
    for k := 0 to NACache - 1 do
    begin
      if ACacheKind[k] = 1 then b := 16
      else if SSAProg.GetArray(ACacheId[k]).ElementType = srtFloat then b := 8
      else b := 0;
      rex := $48; if ACacheReg[k] >= 8 then rex := rex or $04;   // REX.W (+R)
      E.Emit8(rex); E.Emit8($8B);
      E.Emit8($80 or ((ACacheReg[k] and 7) shl 3) or RDX);
      E.Emit32(LongWord(ACacheId[k]) * 32 + LongWord(b));
    end;
  end;

  // Call AotExecOne for the bytecode instruction at apc, then decide whether native
  // execution may continue.
  //
  // The flush/reload around the call is NOT the ABI spill the plan sketched, and is not
  // optional: the helper runs an interpreter handler that reads and writes the register
  // BANKS, so every value native code is holding in a machine register has to be in memory
  // before the call and re-read after it. That requirement subsumes caller-saved-register
  // preservation entirely, which is why no ABI register list appears here. (Liveness (C1)
  // can later trim the RELOAD side to values still live after the call; the flush side is
  // semantic and stays.)
  procedure EmitHelperCall(apc: Integer);
  var k: Integer;
  begin
    Inc(NHelperCalls);
    HelperOps.Add(OpName(Cur.OpCode));
    // 1. Flush every allocated VM register to its bank slot (same stores as the epilogue).
    for k := 0 to NIAlloc - 1 do
      StoreRegMem(ILoc[IAllocd[k]], LongWord(IAllocd[k]) * 8);
    for k := 0 to NFAlloc - 1 do
      SseMem([$F2, $0F, $11], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
    FlushResidentF; FlushResidentI;                             // AOT_DYNF: dynamic temps too

    // 2. Arguments, all read from the ctx record BEFORE r8 is clobbered - it is an argument
    //    register on Win64 (arg2) and volatile on both ABIs. arg3 is the ctx record itself, so
    //    the helper can refresh the array descriptor pointer we re-read in step 4.
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_EXECONE);                 // rax  = ctx.ExecOne
    MovRR(ABI_ARG3, R8);                                          // arg3 = ctx
    E.MemOp([$49, $8B], ABI_ARG0, R8, AOTCTX_VMSELF);             // arg0 = ctx.VMSelf
    E.MemOp([$49, $8B], ABI_ARG1, R8, AOTCTX_CTXOBJ);             // arg1 = ctx.CtxObj
    if ABI_ARG2 >= 8 then E.Emit8($49) else E.Emit8($48);         // arg2 = apc (sign-extended)
    E.Emit8($C7); E.Emit8($C0 or (ABI_ARG2 and 7)); E.Emit32(LongWord(apc));
    E.EmitBytes([$FF, $D0]);                                      // call rax

    // 3. Restore our base registers from the frame. rbx (IntRegs) is callee-saved on both
    //    ABIs and survives; rsi (FloatRegs) does not in System V, and r8 (ctx) never does.
    FrameLoad(R8, SlotCtxSave);
    if not RsiIsPool then FrameLoad(RSI, SlotFltSave);   // float-free region: rsi carries a VALUE

    // 4. Re-read the banks: the helper may have written any of them.
    for k := 0 to NIAlloc - 1 do
      LoadRegMem(ILoc[IAllocd[k]], LongWord(IAllocd[k]) * 8);
    for k := 0 to NFAlloc - 1 do
      SseMem([$F2, $0F, $10], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
    ReloadResidentF; ReloadResidentI;                           // AOT_DYNF: dynamic temps too
    ReloadArrayCache;

    // 5. Continue natively only if the helper landed exactly where this code expects.
    //    Anything else - a moved PC, or one of the negative sentinels - leaves to the
    //    interpreter with that value in rax, which is already the epilogue's contract.
    E.EmitBytes([$48, $3D]); E.Emit32(LongWord(apc + 1));         // cmp rax, apc+1
    JccRel($85, -1);                                              // jne epilogue
  end;

  // B3: native call site for ssaCallSub. The arguments were already staged into the xfer
  // slots by the preceding (native) XferStore ops and the result comes back through them.
  // AotCallSub replicates bcCallSub in Pascal (FramePush + return-PC push), invokes the
  // callee's COMPILED function on the same banks, and on a clean bcReturnSub performs the
  // return (FramePop + pop) and yields AOT_CALL_OK; every other outcome (callee not
  // compiled, deopt inside the callee, helper sentinel) hands the rest of the invocation
  // to the interpreter, so the call site carries a deopt's hazard (prescan sets HasDeopt).
  procedure EmitCallSubNative(apc, calleePC: Integer);
  var k: Integer;
  begin
    Inc(NHelperCalls);
    // 1. Flush ALL allocated registers to the banks: the callee runs on the same banks
    //    through rbx/rsi and clobbers them freely.
    for k := 0 to NIAlloc - 1 do
      StoreRegMem(ILoc[IAllocd[k]], LongWord(IAllocd[k]) * 8);
    for k := 0 to NFAlloc - 1 do
      SseMem([$F2, $0F, $11], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
    FlushResidentF; FlushResidentI;                           // AOT_DYNF: dynamic temps too
    // 2. Arguments. Read the primitive address from the ctx BEFORE any argument setup can
    //    clobber r8 (it is arg2 on Win64 and volatile on both ABIs) - the C5 concat lesson.
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_CALLSUB);              // rax  = ctx.CallSub
    MovRR(ABI_ARG0, R8);                                       // arg0 = ctx
    MovImm64(ABI_ARG1, calleePC);                              // arg1 = callee entry PC
    MovImm64(ABI_ARG2, apc);                                   // arg2 = bcCallSub PC (last: may clobber r8)
    E.EmitBytes([$FF, $D0]);                                   // call rax
    // 3. Restore our base registers (r8/rsi are volatile; rbx survives).
    FrameLoad(R8, SlotCtxSave);
    if not RsiIsPool then FrameLoad(RSI, SlotFltSave);   // float-free region: rsi carries a VALUE
    // 4. Continue natively only on a completed call. Anything else leaves through the BARE
    //    epilogue with rax as is: our registers were flushed before the call and the callee
    //    has since written the banks - the normal epilogue's re-flush would corrupt them.
    E.EmitBytes([$48, $3D]); E.Emit32(LongWord(AOT_CALL_OK));  // cmp rax, AOT_CALL_OK
    JccRel($85, -2);                                           // jne bare-epilogue
    // 5. Re-read the banks and the array cache: the callee may have written any register,
    //    and a DIM/REDIM inside it may have moved the descriptor table (AotCallSub
    //    refreshed ctx.ArrDesc while still on a Pascal frame).
    for k := 0 to NIAlloc - 1 do
      LoadRegMem(ILoc[IAllocd[k]], LongWord(IAllocd[k]) * 8);
    for k := 0 to NFAlloc - 1 do
      SseMem([$F2, $0F, $10], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
    ReloadResidentF; ReloadResidentI;                         // AOT_DYNF: dynamic temps too
    ReloadArrayCache;
  end;

  // C5: caller-saved spill around a native leaf call (the string primitives). Only the
  // ABI-VOLATILE allocated registers need it - callee-saved ones survive the call - and the
  // bank slot is their canonical home, so the round-trip goes through the banks, exactly like
  // the helper flush but skipping every callee-saved register. ALL volatiles are reloaded (not
  // trimmed by liveness): the epilogue flushes every allocated register unconditionally, so a
  // register left holding a clobbered value would be flushed into its bank. Trimming the reload
  // is a separate, careful change (see the helper-hot-loop note in the AOT design doc).
  procedure SpillVolatiles;
  var k: Integer;
  begin
    for k := 0 to NIAlloc - 1 do
      if not GprIsCalleeSaved(ILoc[IAllocd[k]]) then
        StoreRegMem(ILoc[IAllocd[k]], LongWord(IAllocd[k]) * 8);
    for k := 0 to NFAlloc - 1 do
      if not XmmIsCalleeSaved(FLoc[FAllocd[k]]) then
        SseMem([$F2, $0F, $11], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
    FlushResidentF; FlushResidentI;
  end;
  procedure ReloadVolatiles;
  var k: Integer;
  begin
    for k := 0 to NIAlloc - 1 do
      if not GprIsCalleeSaved(ILoc[IAllocd[k]]) then
        LoadRegMem(ILoc[IAllocd[k]], LongWord(IAllocd[k]) * 8);
    for k := 0 to NFAlloc - 1 do
      if not XmmIsCalleeSaved(FLoc[FAllocd[k]]) then
        SseMem([$F2, $0F, $10], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
  end;
  // After a native leaf call: restore the base regs the call may have clobbered (r8 ctx, rsi
  // FloatRegs), then the caller-saved allocated registers (rbx is callee-saved and survives).
  //
  // 🐛 ...AND the array cache, which is NOT covered by any of the above. ACacheReg draws straight
  // from IntPool, and IntPool's first three entries (R9/R10/R11) are CALLER-SAVED on Win64: a
  // cached array base living in one of them does not survive the call, and unlike an allocated VM
  // register it has no bank slot to be spilled to and reloaded from - only ReloadArrayCache can
  // rebuild it. EmitHelperCall and EmitCallSubNative have always called it; this epilogue did not,
  // which left every C5 string leaf call able to corrupt a cached array base in a region that has
  // both. It stayed invisible because it needs the cache to land in one of those three registers
  // AND an array access after the call; the C6 record calls, which stage the primitive in r11
  // explicitly, hit it immediately (an access violation on the second record allocation).
  // Reloading is idempotent - the values are re-read from ctx.ArrDesc - and costs nothing in a
  // region with no cache (NACache = 0 returns at once).
  procedure StrCallEpilogue;
  begin
    FrameLoad(R8, SlotCtxSave);
    if not RsiIsPool then FrameLoad(RSI, SlotFltSave);   // float-free region: rsi carries a VALUE
    ReloadVolatiles;
    ReloadResidentF; ReloadResidentI;
    ReloadArrayCache;
  end;

  // C5: bcCmp*String lowered to a leaf call to AotStrCmp. Kind 0=Eq 1=Ne 2=Lt 3=Gt. The two
  // string operands stay in the StringRegs bank; the emitted code reads their slot values and
  // passes them as pointers. Always completes natively (a comparison cannot raise) - no deopt.
  procedure EmitStrCmp(Kind: Integer);
  var s1, s2: Integer;
  begin
    s1 := SReg(Cur.Src1); s2 := SReg(Cur.Src2); if not OK then Exit;
    // Save caller-saved allocated regs BEFORE arg setup clobbers the base regs (on System V
    // arg1 IS rsi, the float bank base that SpillVolatiles reads through).
    SpillVolatiles;
    // Read both operand pointers and the primitive address out of the ctx (r8) BEFORE arg2
    // clobbers r8 on Win64; base regs are reloaded from the frame after the call.
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);         // rax = ctx.StrRegs (bank base)
    E.MemOp([$48, $8B], ABI_ARG0, RAX, LongWord(s1) * 8); // arg0 = StringRegs[s1]
    E.MemOp([$48, $8B], ABI_ARG1, RAX, LongWord(s2) * 8); // arg1 = StringRegs[s2]
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRCMP);          // rax = ctx.StrCmp (call target)
    MovImm64(ABI_ARG2, Kind);                             // arg2 = kind (may clobber r8 on Win64)
    E.EmitBytes([$FF, $D0]);                              // call rax
    StrCallEpilogue;
    CmpBoolToDest;                                        // al (0/1) -> Dest := TrueVal/0
  end;

  // C5: StringRegs[dest] := StringRegs[src] (managed copy) via AotStrAssign(&dst, srcVal).
  procedure EmitStrCopy;
  var d, s: Integer;
  begin
    d := SReg(Cur.Dest); s := SReg(Cur.Src1); if not OK then Exit;
    SpillVolatiles;
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);         // rax = bank base
    Lea(ABI_ARG0, RAX, LongWord(d) * 8);                  // arg0 = &StringRegs[dest]
    E.MemOp([$48, $8B], ABI_ARG1, RAX, LongWord(s) * 8);  // arg1 = StringRegs[src] (value)
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRASSIGN);       // rax = primitive
    E.EmitBytes([$FF, $D0]);                              // call rax
    StrCallEpilogue;
  end;

  // C5: StringRegs[dest] := StringConstants[imm] via AotStrLoadConst(&dst, VMSelf, imm). The
  // constant index lives in the BYTECODE instruction (assigned by the compiler), not the SSA.
  procedure EmitStrLoadConst(apc: Integer);
  var d: Integer; imm: Int64;
  begin
    d := SReg(Cur.Dest); if not OK then Exit;
    imm := Prog.GetInstruction(apc).Immediate;
    SpillVolatiles;
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);         // rax = bank base
    Lea(ABI_ARG0, RAX, LongWord(d) * 8);                  // arg0 = &StringRegs[dest]
    E.MemOp([$49, $8B], ABI_ARG1, R8, AOTCTX_VMSELF);     // arg1 = VMSelf (before r8 clobber)
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRLOADCONST);    // rax = primitive (before r8 clobber)
    MovImm64(ABI_ARG2, imm);                              // arg2 = imm (may clobber r8 on Win64)
    E.EmitBytes([$FF, $D0]);                              // call rax
    StrCallEpilogue;
  end;

  // C5: StringRegs[dest] := StringRegs[a] + StringRegs[b] via AotStrConcat(&dst, aVal, bVal).
  // Unlike the others, arg2 is a MEMORY load into a register that is r8 (ctx) on Win64, so the
  // ctx would be gone before the primitive address could be read from it. The primitive is
  // therefore staged in r11 (volatile everywhere, never an arg, safe as scratch between the
  // spill and reload) BEFORE arg2 clobbers r8.
  procedure EmitStrConcat;
  var d, a, b: Integer;
  begin
    d := SReg(Cur.Dest); a := SReg(Cur.Src1); b := SReg(Cur.Src2); if not OK then Exit;
    SpillVolatiles;
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);         // rax = bank base
    E.MemOp([$4D, $8B], R11, R8, AOTCTX_STRCONCAT);       // r11 = primitive (before r8 clobber)
    Lea(ABI_ARG0, RAX, LongWord(d) * 8);                  // arg0 = &StringRegs[dest]
    MovLoad(ABI_ARG1, RAX, LongWord(a) * 8);              // arg1 = StringRegs[a] (value)
    MovLoad(ABI_ARG2, RAX, LongWord(b) * 8);              // arg2 = StringRegs[b] (may clobber r8)
    E.EmitBytes([$41, $FF, $D3]);                         // call r11
    StrCallEpilogue;
  end;

  // "acc + MID$(tab, k, 1)" fused: AotStrConcatCharAt(&dst, accVal, tabVal, kValue).
  // Same r11 staging as EmitStrConcat and for the same reason: arg2 is r8 on Win64, which IS the
  // context register, so the primitive's address must be read out of the context BEFORE that
  // argument overwrites it. The index comes from the INT bank through Immediate, like bcStrMid's
  // length -- and it is loaded with the SPILLED accessor, because after SpillVolatiles the value
  // lives in the bank, not in the machine register ILoadArg would have read.
  procedure EmitStrConcatCharAt;
  var d, a, t, kreg: Integer;
  begin
    d := SReg(Cur.Dest); a := SReg(Cur.Src1); t := SReg(Cur.Src2);
    kreg := IReg(Cur.Src3); if not OK then Exit;
    SpillVolatiles;
    ILoadArgSpilled(ABI_ARG3, kreg);                      // arg3 = IntRegs[k], read from the bank
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);         // rax = string bank base
    E.MemOp([$4D, $8B], R11, R8, AOTCTX_STRCONCATCHARAT); // r11 = primitive (before r8 is clobbered)
    Lea(ABI_ARG0, RAX, LongWord(d) * 8);                  // arg0 = &StringRegs[dest]
    MovLoad(ABI_ARG1, RAX, LongWord(a) * 8);              // arg1 = StringRegs[acc] (value)
    MovLoad(ABI_ARG2, RAX, LongWord(t) * 8);              // arg2 = StringRegs[tab] (clobbers r8)
    E.EmitBytes([$41, $FF, $D3]);                         // call r11
    StrCallEpilogue;
  end;

  // "acc += tab[Asc(MID$(s, i, 1)) + 1]" fused whole: AotStrAppendMapped(&dst, srcVal, tabVal, i).
  // Argument shape and r11 staging identical to EmitStrConcatCharAt above - arg2 is r8, the context
  // register, so the primitive's address is read out of the context BEFORE that argument overwrites
  // it, and the index comes from the INT bank through Immediate with the SPILLED accessor.
  procedure EmitStrAppendMapped;
  // ⚠️ NOT "ireg" for the index: Pascal identifiers are case-insensitive, so a local named ireg
  // shadows the IReg() accessor itself and the call stops parsing ("';' expected but '(' found").
  var d, s, t, idxr: Integer;
  begin
    d := SReg(Cur.Dest); s := SReg(Cur.Src1); t := SReg(Cur.Src2);
    idxr := IReg(Cur.Src3); if not OK then Exit;
    SpillVolatiles;
    ILoadArgSpilled(ABI_ARG3, idxr);                      // arg3 = IntRegs[i], read from the bank
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);         // rax = string bank base
    E.MemOp([$4D, $8B], R11, R8, AOTCTX_STRAPPENDMAPPED); // r11 = primitive (before r8 is clobbered)
    Lea(ABI_ARG0, RAX, LongWord(d) * 8);                  // arg0 = &StringRegs[dest] (accumulator)
    MovLoad(ABI_ARG1, RAX, LongWord(s) * 8);              // arg1 = StringRegs[src] (value)
    MovLoad(ABI_ARG2, RAX, LongWord(t) * 8);              // arg2 = StringRegs[tab] (clobbers r8)
    E.EmitBytes([$41, $FF, $D3]);                         // call r11
    StrCallEpilogue;
  end;

  // C6: IntRegs[dest] := AotRecordNew(VMSelf, CtxObj, packedCounts, imm). The three slot counts
  // are NOT registers - the bytecode compiler puts them in Src1/Src2/Immediate of the bytecode
  // instruction - so they are baked as immediates here and the call takes no bank operand at all.
  // Src1/Src2 pack into one argument because Win64 has four argument registers and New needs five
  // values. Always completes natively (allocation cannot hand the invocation back) - no deopt.
  procedure EmitRecordNew(apc: Integer);
  var
    d: Integer;
    Bc: TBytecodeInstruction;
    Counts: Int64;
  begin
    d := IReg(Cur.Dest); if not OK then Exit;
    Bc := Prog.GetInstruction(apc);
    Counts := Int64(LongWord(Bc.Src1)) or (Int64(LongWord(Bc.Src2)) shl 32);
    SpillVolatiles;
    // The primitive is staged in r11 (volatile, never an argument) BEFORE arg2 clobbers r8 on
    // Win64 - r8 IS the context register, and the address lives inside it. Same lesson as C5.
    E.MemOp([$4D, $8B], R11, R8, AOTCTX_RECNEW);          // r11 = primitive
    E.MemOp([$49, $8B], ABI_ARG0, R8, AOTCTX_VMSELF);     // arg0 = VMSelf
    E.MemOp([$49, $8B], ABI_ARG1, R8, AOTCTX_CTXOBJ);     // arg1 = CtxObj (the active context)
    MovImm64(ABI_ARG2, Counts);                           // arg2 = intSlots | floatSlots<<32
    MovImm64(ABI_ARG3, Bc.Immediate);                     // arg3 = strSlots | typeId<<32 | shared<<48
    E.EmitBytes([$41, $FF, $D3]);                         // call r11
    StrCallEpilogue;
    IStore(d, RAX);                                       // rax = handle -> int Dest
  end;

  // C6: AotRecordFree(VMSelf, IntRegs[src]) - DELETE p. The handle is read with the SPILLED
  // accessor: after SpillVolatiles a caller-saved allocated register lives in the bank, not in
  // the machine register a plain load would read.
  procedure EmitRecordFree;
  var s: Integer;
  begin
    s := IReg(Cur.Src1); if not OK then Exit;
    SpillVolatiles;
    E.MemOp([$4D, $8B], R11, R8, AOTCTX_RECFREE);         // r11 = primitive (before any clobber)
    E.MemOp([$49, $8B], ABI_ARG0, R8, AOTCTX_VMSELF);     // arg0 = VMSelf
    ILoadArgSpilled(ABI_ARG1, s);                         // arg1 = the handle
    E.EmitBytes([$41, $FF, $D3]);                         // call r11
    StrCallEpilogue;
  end;

  // C6: the block record marks as leaf calls - AotRecMarkPush/Pop(CtxObj). Two context fields and
  // a counter; the point is what they do NOT do compared with the helper route (no flush of the
  // allocated registers, no interpreter dispatch, no reload, no PC comparison).
  procedure EmitRecMarkNative(IsPush: Boolean);
  var Off: Integer;
  begin
    if IsPush then Off := AOTCTX_RECMARKPUSH else Off := AOTCTX_RECMARKPOP;
    SpillVolatiles;
    E.MemOp([$4D, $8B], R11, R8, Off);                    // r11 = primitive
    E.MemOp([$49, $8B], ABI_ARG0, R8, AOTCTX_CTXOBJ);     // arg0 = CtxObj
    E.EmitBytes([$41, $FF, $D3]);                         // call r11
    StrCallEpilogue;
  end;

  // C5: IntRegs[dest] := Length(StringRegs[src]) via AotStrLen(srcVal) -> rax.
  procedure EmitStrLen;
  var s: Integer;
  begin
    s := SReg(Cur.Src1); if not OK then Exit;
    SpillVolatiles;
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);         // rax = bank base
    E.MemOp([$48, $8B], ABI_ARG0, RAX, LongWord(s) * 8);  // arg0 = StringRegs[src] (value)
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRLEN);          // rax = primitive
    E.EmitBytes([$FF, $D0]);                              // call rax
    StrCallEpilogue;
    IStore(IReg(Cur.Dest), RAX);                          // rax = length -> int Dest
  end;

  // C5 residuals: substring/char/search leaf primitives. Staging discipline: every bank
  // read (string slots via rax) happens FIRST; the primitive address is loaded into RAX
  // right after (r8 still intact); INT operands are loaded into their arg registers LAST -
  // on Win64 arg2 is r8 (clobbers the ctx, already consumed) and arg3 is r9, which sits in
  // the allocation POOL, so it must be written only after every operand read. r11 is NOT
  // used as a stage here (unlike concat): it is in the pool too and an int operand could
  // live there.
  procedure EmitStrSlice(CtxOff: Integer);   // ssaStrLeft/ssaStrRight: (dstSlot, sVal, n)
  var d, s, n: Integer;
  begin
    d := SReg(Cur.Dest); s := SReg(Cur.Src1); n := IReg(Cur.Src2); if not OK then Exit;
    SpillVolatiles;
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);         // rax = bank base
    Lea(ABI_ARG0, RAX, LongWord(d) * 8);                  // arg0 = &StringRegs[dest]
    MovLoad(ABI_ARG1, RAX, LongWord(s) * 8);              // arg1 = StringRegs[src] (value)
    E.MemOp([$49, $8B], RAX, R8, CtxOff);                 // rax = primitive (before r8 clobber)
    ILoadArg(ABI_ARG2, n);                                // arg2 = length (clobbers r8 on Win64)
    E.EmitBytes([$FF, $D0]);                              // call rax
    StrCallEpilogue;
  end;
  procedure EmitStrMid;
  var d, s, st, ln: Integer;
  begin
    d := SReg(Cur.Dest); s := SReg(Cur.Src1);
    st := IReg(Cur.Src2); ln := IReg(Cur.Src3); if not OK then Exit;
    SpillVolatiles;
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);
    Lea(ABI_ARG0, RAX, LongWord(d) * 8);
    MovLoad(ABI_ARG1, RAX, LongWord(s) * 8);
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRMID);
    ILoadArg(ABI_ARG2, st);                               // start
    ILoadArg(ABI_ARG3, ln);                               // length: r9 (pool) written LAST
    E.EmitBytes([$FF, $D0]);
    StrCallEpilogue;
  end;
  // A managed STRING array element, through the ctx primitives. Two ordering constraints, both
  // learned from the string emitters above: on Win64 ABI_ARG2 IS r8, the context register, so
  // everything read out of the context must be read BEFORE arg2 is written; and ABI_ARG3 is r9,
  // which lives in the allocation pool, so it is written LAST.
  // ⛔ ORDER IS THE WHOLE PROBLEM HERE, and getting it wrong miscompiled silently: the array
  // initializer "{ "a","c",... }" lost exactly one element, so fasta dropped every 'c'.
  // The rule that makes it safe: read the INDEX FIRST, while every pooled register still holds what
  // the allocator says it holds. ILoadArg may copy from a pooled machine register, and my earlier
  // version wrote arg1 (rdx) and arg2 (r8) BEFORE reading it - so an index living in one of those
  // was read back as the array id. Reading it first means nothing written afterwards can matter:
  // everything else comes either from the context (r8, read before it is overwritten last) or from
  // the string bank base in rax, and neither is in the allocation pool.
  procedure EmitArrLoadStr(ArrayId, IdxReg, DstSlot: Integer);
  begin
    SpillVolatiles;
    ILoadArgSpilled(ABI_ARG3, IdxReg);                       // arg3 = index (spill-safe read)
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);            // rax = string bank base
    Lea(ABI_ARG0, RAX, LongWord(DstSlot) * 8);               // arg0 = &StringRegs[dest]
    E.MemOp([$49, $8B], ABI_ARG1, R8, AOTCTX_VMSELF);        // arg1 = the TBytecodeVM
    E.MemOp([$4D, $8B], R11, R8, AOTCTX_ARRLOADSTR);         // r11 = primitive (last ctx read)
    MovImm64(ABI_ARG2, ArrayId);                             // arg2 = array id (clobbers r8/ctx LAST)
    E.EmitBytes([$41, $FF, $D3]);                            // call r11
    StrCallEpilogue;
  end;

  // Signature deliberately (VMSelf, arrIdx, srcVal, idx): the index is the LAST argument so it can
  // be the FIRST thing loaded, into r9, without anything else needing to be in place yet.
  procedure EmitArrStoreStr(ArrayId, IdxReg, SrcSlot: Integer);
  begin
    SpillVolatiles;
    ILoadArgSpilled(ABI_ARG3, IdxReg);                       // arg3 = index (spill-safe read)
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);            // rax = string bank base
    E.MemOp([$49, $8B], ABI_ARG0, R8, AOTCTX_VMSELF);        // arg0 = the TBytecodeVM
    E.MemOp([$4D, $8B], R11, R8, AOTCTX_ARRSTORESTR);        // r11 = primitive (last ctx read)
    MovImm64(ABI_ARG1, ArrayId);                             // arg1 = array id
    MovLoad(ABI_ARG2, RAX, LongWord(SrcSlot) * 8);           // arg2 = StringRegs[src] (clobbers ctx)
    E.EmitBytes([$41, $FF, $D3]);                            // call r11
    StrCallEpilogue;
  end;

  // IntRegs[dest] := Asc(Mid(StringRegs[src], start, len)) in one call, without building the
  // substring. Same argument discipline as EmitStrMid: the string operand is read from the bank
  // through rax, the primitive is fetched from the context BEFORE arg2 clobbers r8 on Win64, and
  // the pooled arg3 is written last.
  //
  // FAST PATH (AotAscMidInline). Reading ONE byte out of a string is three machine instructions -
  // load the data pointer, check the bound, movzx the byte - and paying a spill-everything helper
  // call for them was measured at 15,6 ns per character, 72% of reverse-complement's fused inner
  // loop (job/tests/bench/xform_floor.bas: empty loop 0 ms, + Asc(Mid) 78 ms on 5 M characters).
  // The inline covers the ONE shape that is dialect-blind: length exactly 1 with 1 <= start <=
  // Len(s), where MODERN and CLASSIC both answer Ord(s[start]). EVERYTHING else - a length other
  // than 1, a start outside the string, an empty (nil) buffer - falls through to the helper below,
  // which keeps all the rules and stays the single place they are written.
  //
  // rax/rcx/rdx are safe scratch here: IntPool never hands them out, and the fast path runs BEFORE
  // SpillVolatiles, while every pooled value is still where ILoad expects it. Both arms converge on
  // "result in rax", so the pool state at the join is identical either way - the slow arm spills and
  // reloads, the fast arm touches nothing pooled.
  procedure EmitStrAscMid;
  var d, s, st, ln: Integer;
      pLen, pNil, pLo, pHi, pDone: Integer;
  begin
    d := IReg(Cur.Dest); s := SReg(Cur.Src1);
    st := IReg(Cur.Src2); ln := IReg(Cur.Src3); if not OK then Exit;
    pDone := -1;
    if AotAscMidInline then
    begin
      E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);        // rax = string bank base
      E.MemOp([$48, $8B], RAX, RAX, LongWord(s) * 8);      // rax = StringRegs[src] (data pointer)
      ILoad(RCX, st);                                      // rcx = start  (pre-spill: pool intact)
      ILoad(RDX, ln);                                      // rdx = length
      E.EmitBytes([$48, $83, $FA, $01]);                   // cmp rdx, 1
      E.EmitBytes([$75, $00]); pLen := E.Len - 1;          // jne slow   (only length 1 is blind)
      E.EmitBytes([$48, $85, $C0]);                        // test rax, rax
      E.EmitBytes([$74, $00]); pNil := E.Len - 1;          // jz slow    (empty string -> 0)
      E.EmitBytes([$48, $83, $F9, $01]);                   // cmp rcx, 1
      E.EmitBytes([$7C, $00]); pLo := E.Len - 1;           // jl slow    (start < 1: dialects differ)
      E.EmitBytes([$48, $3B, $48, $F8]);                   // cmp rcx, [rax-8]   (the length field)
      E.EmitBytes([$7F, $00]); pHi := E.Len - 1;           // jg slow    (start > Len -> 0)
      E.EmitBytes([$0F, $B6, $44, $08, $FF]);              // movzx eax, byte [rax+rcx-1]
      // rel32, not a short jump: the helper sequence it skips is SpillVolatiles + call +
      // StrCallEpilogue, which is comfortably past 127 bytes once the pool is full.
      E.Emit8($E9); pDone := E.Len; E.Emit32(0);           // jmp done
      E.PatchByte(pLen, Byte(E.Len - (pLen + 1)));
      E.PatchByte(pNil, Byte(E.Len - (pNil + 1)));
      E.PatchByte(pLo,  Byte(E.Len - (pLo + 1)));
      E.PatchByte(pHi,  Byte(E.Len - (pHi + 1)));
    end;
    SpillVolatiles;
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);         // rax = string bank base
    E.MemOp([$4D, $8B], R11, R8, AOTCTX_STRASCMID);       // r11 = primitive (last ctx read)
    MovLoad(ABI_ARG0, RAX, LongWord(s) * 8);              // arg0 = StringRegs[src] (value)
    // arg1 (rdx) is left ALONE and the primitive ignores it: rdx can hold a pooled value, and
    // writing it before reading the other int operand is what returned 0 for "s[i]". The two ints
    // go to r8 (never pooled - it is the ctx register) and r9 (written LAST), exactly as EmitStrMid.
    ILoadArgSpilled(ABI_ARG1, st);                        // arg1 = start
    ILoadArgSpilled(ABI_ARG3, ln);                        // arg3 = length
    E.EmitBytes([$41, $FF, $D3]);                         // call r11
    StrCallEpilogue;
    if pDone >= 0 then E.Patch32(pDone, LongWord(E.Len - (pDone + 4)));   // @done
    IStore(d, RAX);                                       // the code comes back in rax
  end;

  procedure EmitStrAsc;    // IntRegs[dest] := code of StringRegs[src][1] (0 if empty)
  var d, s: Integer;
  begin
    d := IReg(Cur.Dest); s := SReg(Cur.Src1); if not OK then Exit;
    SpillVolatiles;
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);
    E.MemOp([$48, $8B], ABI_ARG0, RAX, LongWord(s) * 8);  // arg0 = value
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRASC);
    E.EmitBytes([$FF, $D0]);
    StrCallEpilogue;
    IStore(d, RAX);
  end;
  procedure EmitStrChr;    // StringRegs[dest] := Chr(code and $FF)
  var d, c: Integer;
  begin
    d := SReg(Cur.Dest); c := IReg(Cur.Src1); if not OK then Exit;
    SpillVolatiles;
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);
    Lea(ABI_ARG0, RAX, LongWord(d) * 8);
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRCHR);
    ILoadArg(ABI_ARG1, c);
    E.EmitBytes([$FF, $D0]);
    StrCallEpilogue;
  end;
  procedure EmitStrInstr;  // IntRegs[dest] := Instr(start, hay, needle)
  var d, hay, nee, st: Integer;
  begin
    d := IReg(Cur.Dest); hay := SReg(Cur.Src1);
    nee := SReg(Cur.Src2); st := IReg(Cur.Src3); if not OK then Exit;
    SpillVolatiles;
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);
    MovLoad(ABI_ARG0, RAX, LongWord(hay) * 8);
    MovLoad(ABI_ARG1, RAX, LongWord(nee) * 8);
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRINSTR);
    ILoadArg(ABI_ARG2, st);
    E.EmitBytes([$FF, $D0]);
    StrCallEpilogue;
    IStore(d, RAX);
  end;
  procedure EmitIntToStr;  // StringRegs[dest] := IntToStr(IntRegs[src]) - Str() of an int
  var d, v: Integer;
  begin
    d := SReg(Cur.Dest); v := IReg(Cur.Src1); if not OK then Exit;
    SpillVolatiles;
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);
    Lea(ABI_ARG0, RAX, LongWord(d) * 8);                  // arg0 = &StringRegs[dest]
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRINTTOSTR);     // rax = primitive (before r8 clobber)
    ILoadArg(ABI_ARG1, v);                                // arg1 = value (may clobber r8 on Win64)
    E.EmitBytes([$FF, $D0]);
    StrCallEpilogue;
  end;
  procedure EmitStrVal;    // FloatRegs[dest] := ParseLeadingFloat(StringRegs[src])
  var d, s: Integer;
  begin
    d := FReg(Cur.Dest); s := SReg(Cur.Src1); if not OK then Exit;
    SpillVolatiles;
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);
    E.MemOp([$48, $8B], ABI_ARG0, RAX, LongWord(s) * 8);  // arg0 = value
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRVAL);
    E.EmitBytes([$FF, $D0]);
    StrCallEpilogue;       // reloads volatile xmm2..5 from the banks; xmm0 (the result) survives
    FStore(d, XMM0);
  end;
  procedure EmitStrValInt; // IntRegs[dest] := ParseLeadingInt64(StringRegs[src])
  var d, s: Integer;
  begin
    d := IReg(Cur.Dest); s := SReg(Cur.Src1); if not OK then Exit;
    SpillVolatiles;
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRREGS);
    E.MemOp([$48, $8B], ABI_ARG0, RAX, LongWord(s) * 8);  // arg0 = value
    E.MemOp([$49, $8B], RAX, R8, AOTCTX_STRVALINT);
    E.EmitBytes([$FF, $D0]);
    StrCallEpilogue;
    IStore(d, RAX);
  end;

  procedure LoadXferBase(FloatBank: Boolean);
  begin
    if FloatBank then E.MemOp([$49, $8B], RDX, R8, 8)
    else E.MemOp([$49, $8B], RDX, R8, 0);
  end;

  { --- B2 arrays: descriptor via ctx (never a baked address), JIT-identical semantics --- }

  function CachedBase(ArrayId: Integer): Integer;
  var q: Integer;
  begin
    Result := -1;
    for q := 0 to NACache - 1 do
      if (ACacheId[q] = ArrayId) and (ACacheKind[q] = 0) then Exit(ACacheReg[q]);
  end;
  function CachedCount(ArrayId: Integer): Integer;
  var q: Integer;
  begin
    Result := -1;
    for q := 0 to NACache - 1 do
      if (ACacheId[q] = ArrayId) and (ACacheKind[q] = 1) then Exit(ACacheReg[q]);
  end;

  // SIB element access [BaseReg + rcx*8] (scale 8, index rcx). Base low-3 = 101 (rbp/r13)
  // has no mod=00 form -> mod=01 disp8=0 (the JIT's EmitSib fix; kept although the AOT
  // base is always rdx, so a future cached-base upgrade cannot re-trip it).
  procedure AotSib(BaseReg: Integer);
  var sib: Byte;
  begin
    sib := $C8 or (BaseReg and 7);
    if (BaseReg and 7) = 5 then
    begin E.Emit8($44); E.Emit8(sib); E.Emit8($00); end
    else
    begin E.Emit8($04); E.Emit8(sib); end;
  end;
  procedure AotArrData(IsFloat, IsStore: Boolean; BaseReg: Integer);
  begin
    if IsFloat then
    begin
      E.Emit8($F2);
      if BaseReg >= 8 then E.Emit8($41);
      E.Emit8($0F);
      if IsStore then E.Emit8($11) else E.Emit8($10);
      AotSib(BaseReg);                             // movsd xmm0 <-> [base+rcx*8]
    end
    else
    begin
      if BaseReg >= 8 then E.Emit8($49) else E.Emit8($48);
      if IsStore then E.Emit8($89) else E.Emit8($8B);
      AotSib(BaseReg);                             // mov rax <-> [base+rcx*8]
    end;
  end;
  // Element load/store with the interpreter's dialect bounds semantics. Sequence:
  // rcx = index; rdx = desc table (ctx); rax = Count; unsigned cmp; then either the
  // CLASSIC guard (OOB -> deopt, interpreter raises) or the MODERN default path
  // (load 0 / drop store); rdx is reused for the data base after the compare.
  // Safe = B4 range analysis proved the index in [0, TotalSize): no count load, no
  // compare, no guard - dialect is irrelevant because the check could never trip.
  procedure AotArrAccess(IsFloat, IsStore: Boolean; ArrayId, IdxReg, ValReg, apc: Integer;
                         Safe: Boolean);
  var pOOB, pDone, DataOff, cbase, ccount, baseR: Integer;
    procedure EmitBase;   // leave the data base register in baseR (cached GPR or reloaded rdx)
    begin
      if cbase >= 0 then baseR := cbase
      else
      begin
        E.MemOp([$48, $8B], RDX, RDX, LongWord(ArrayId) * 32 + LongWord(DataOff));
        baseR := RDX;
      end;
    end;
  begin
    if IsFloat then DataOff := 8 else DataOff := 0;
    cbase := CachedBase(ArrayId);
    ccount := CachedCount(ArrayId);
    ILoad(RCX, IdxReg);                                            // rcx = index
    if Safe then
    begin
      if cbase < 0 then
        E.MemOp([$49, $8B], RDX, R8, 16);                          // rdx = ctx.ArrDesc
      EmitBase;
      if IsStore then
      begin
        if IsFloat then FLoad(XMM0, ValReg) else ILoad(RAX, ValReg);
        AotArrData(IsFloat, True, baseR);
      end
      else
      begin
        AotArrData(IsFloat, False, baseR);
        if IsFloat then FStore(ValReg, XMM0) else IStore(ValReg, RAX);
      end;
      Exit;
    end;
    if (cbase < 0) or (ccount < 0) then
      E.MemOp([$49, $8B], RDX, R8, 16);                            // rdx = ctx.ArrDesc
    if ccount >= 0 then
      EmitRR([$3B], RCX, ccount)                                   // cmp rcx, cachedCount
    else
    begin
      E.MemOp([$48, $8B], RAX, RDX, LongWord(ArrayId) * 32 + 16);  // rax = Count
      E.EmitBytes([$48, $39, $C1]);                                // cmp rcx, rax
    end;
    if ArrClassic then
    begin
      E.EmitBytes([$72, $00]); pOOB := E.Len - 1;                  // jb +over (in bounds)
      ExitTo(apc);                                                 // OOB -> interpreter raises
      E.PatchByte(pOOB, Byte(E.Len - (pOOB + 1)));
      EmitBase;
      if IsStore then
      begin
        if IsFloat then FLoad(XMM0, ValReg) else ILoad(RAX, ValReg);
        AotArrData(IsFloat, True, baseR);
      end
      else
        AotArrData(IsFloat, False, baseR);
    end
    else if IsStore then
    begin
      E.EmitBytes([$73, $00]); pOOB := E.Len - 1;                  // jae skip (store dropped)
      EmitBase;
      if IsFloat then FLoad(XMM0, ValReg) else ILoad(RAX, ValReg);
      AotArrData(IsFloat, True, baseR);
      E.PatchByte(pOOB, Byte(E.Len - (pOOB + 1)));
    end
    else
    begin
      E.EmitBytes([$73, $00]); pOOB := E.Len - 1;                  // jae oob
      EmitBase;
      AotArrData(IsFloat, False, baseR);
      E.EmitBytes([$EB, $00]); pDone := E.Len - 1;                 // jmp done
      E.PatchByte(pOOB, Byte(E.Len - (pOOB + 1)));
      if IsFloat then E.EmitBytes([$0F, $57, $C0])                 // xorps xmm0,xmm0
      else E.EmitBytes([$48, $31, $C0]);                           // xor rax,rax
      E.PatchByte(pDone, Byte(E.Len - (pDone + 1)));
    end;
    if not IsStore then
    begin
      if IsFloat then FStore(ValReg, XMM0) else IStore(ValReg, RAX);
    end;
  end;
  // Record field access, ported from the JIT's J13 (SedaiJit.RecAccess) - same shape, same guard,
  // AOT deopt instead of the JIT's.
  //
  // Ctx.Records[handle].{Int,Float}Data[slot]. A SHARED_REC_FLAG handle (bit 62) belongs to the
  // locked cross-thread region and leaves to the interpreter, which takes the lock. A plain handle
  // indexes the per-thread heap: deref the ctx object's Records FIELD to the current base (never a
  // baked address - the dynamic array moves when it grows, and a worker has its own), add
  // handle*RecSize, load the field's data pointer, then load or store at [ptr + slot*8].
  //
  // No handle or slot bounds check, which matches the interpreter exactly (it indexes with range
  // checks off). The slot is a compile-time constant, so it rides in the displacement.
  //
  // HandleReg = Src1, value = Dest (load) or Src2 (store), Slot = Src3 const.
  procedure AotRecAccess(apc, HandleReg, Slot, ValReg: Integer; IsFloat, IsStore: Boolean);
  var p, pJoin: Integer;
  begin
    ILoad(RAX, HandleReg);                          // rax = handle
    E.EmitBytes([$48, $0F, $BA, $E0, 62]);          // bt rax, 62   (SHARED_REC_FLAG = 1 shl 62)
    E.EmitBytes([$73, $00]); p := E.Len - 1;        // jnc +plain   (CF=0 -> per-context heap)
    // --- shared region ---
    // EVERY record of an array of UDT lives here (AllocSharedRecord), which is exactly the shape of
    // real BASIC code, so deopting here would give up the whole point: measured, a deopt-only
    // version made intrec_fb 84% SLOWER than the helper it replaced, because every one of its four
    // per-iteration field accesses left to the interpreter.
    // Each shared record is its own heap block with a STABLE pointer; the VM's lock exists only to
    // guard the array of those pointers while it GROWS. So this is safe without the lock precisely
    // when no other thread can grow it - hence the whole-program "creates no thread" gate.
    if GNoThreads then
    begin
      E.EmitBytes([$48, $0F, $BA, $F0, 62]);        // btr rax, 62  -> shared-region index
      E.MemOp([$49, $8B], RDX, R8, AOTCTX_VMSELF);  // rdx = the TBytecodeVM instance
      E.EmitBytes([$48, $8B, $92]); E.Emit32(LongWord(GSharedRecOff));  // rdx = FSharedRecords base
      E.EmitBytes([$48, $8B, $14, $C2]);            // mov rdx, [rdx + rax*8]  -> PRecordStorage
      E.EmitBytes([$EB, $00]); pJoin := E.Len - 1;  // jmp +join (rdx already points at the record)
    end
    else
    begin
      ExitTo(apc);                                   // threads present -> interpreter takes the lock
      pJoin := -1;
    end;
    E.PatchByte(p, Byte(E.Len - (p + 1)));
    // --- per-context heap ---
    E.MemOp([$49, $8B], RDX, R8, AOTCTX_CTXOBJ);     // rdx = ctx.CtxObj (the TExecutionContext)
    E.EmitBytes([$48, $8B, $92]); E.Emit32(LongWord(GRecordsOff));  // rdx = [rdx+RecordsOff] = base
    E.EmitBytes([$48, $69, $C0]); E.Emit32(LongWord(GRecSize));     // imul rax, rax, RecSize
    E.EmitBytes([$48, $01, $C2]);                    // add rdx, rax  -> @Records[handle]
    if pJoin >= 0 then E.PatchByte(pJoin, Byte(E.Len - (pJoin + 1)));
    // --- join: rdx = PRecordStorage either way ---
    E.EmitBytes([$48, $8B, $8A]);                    // mov rcx, [rdx + fieldoff] = data pointer
    if IsFloat then E.Emit32(LongWord(GRecFloatOff)) else E.Emit32(LongWord(GRecIntOff));
    if IsStore then
    begin
      if IsFloat then
      begin
        FLoad(XMM0, ValReg);
        E.EmitBytes([$F2, $0F, $11, $81]); E.Emit32(LongWord(Slot) * 8);   // movsd [rcx+slot*8], xmm0
      end
      else
      begin
        ILoad(RAX, ValReg);
        E.EmitBytes([$48, $89, $81]); E.Emit32(LongWord(Slot) * 8);        // mov [rcx+slot*8], rax
      end;
    end
    else
    begin
      if IsFloat then
      begin
        E.EmitBytes([$F2, $0F, $10, $81]); E.Emit32(LongWord(Slot) * 8);   // movsd xmm0, [rcx+slot*8]
        FStore(ValReg, XMM0);
      end
      else
      begin
        E.EmitBytes([$48, $8B, $81]); E.Emit32(LongWord(Slot) * 8);        // mov rax, [rcx+slot*8]
        IStore(ValReg, RAX);
      end;
    end;
  end;

  // LBOUND/UBOUND with a runtime dim: only dim 0 is native (LBound at +24; UBOUND =
  // LBound + Count - 1); any other dim (rank query, per-dim bounds) deopts (JIT J10).
  procedure AotArrBound(apc, ArrayId: Integer; WantUpper: Boolean);
  var p1: Integer;
  begin
    if Cur.Src2.Kind = svkConstInt then
      MovImm64(RCX, Cur.Src2.ConstInt)
    else
    begin
      ILoad(RCX, IReg(Cur.Src2)); if not OK then Exit;
    end;
    E.EmitBytes([$48, $85, $C9]);                                  // test rcx, rcx
    E.EmitBytes([$74, $00]); p1 := E.Len - 1;                      // jz dim0
    ExitTo(apc);
    E.PatchByte(p1, Byte(E.Len - (p1 + 1)));
    E.MemOp([$49, $8B], RDX, R8, 16);                              // rdx = ctx.ArrDesc
    E.MemOp([$48, $8B], RAX, RDX, LongWord(ArrayId) * 32 + 24);    // rax = LBound
    if WantUpper then
    begin
      E.MemOp([$48, $8B], RDX, RDX, LongWord(ArrayId) * 32 + 16);  // rdx = Count
      E.EmitBytes([$48, $01, $D0]);                                // add rax, rdx
      E.EmitBytes([$48, $FF, $C8]);                                // dec rax
    end;
    IStore(IReg(Cur.Dest), RAX);
  end;
  // Array id of the current instruction (Src1 must be a compile-time array ref).
  function ArrId: Integer;
  begin
    Result := -1;
    if (Cur.Src1.Kind <> svkArrayRef) or (Cur.Src1.ArrayIndex < 0) or
       (Cur.Src1.ArrayIndex >= SSAProg.GetArrayCount) then Fail('array-shape')
    else Result := Cur.Src1.ArrayIndex;
  end;

  { --- prescan: usage counts, deopt needs, structural checks --- }
  { --- C1: liveness (PIANO_B1_AOT_DESIGN section 5.3) ---------------------------------
    Backward dataflow over the region's CFG, per bank, on FINAL register indexes:
      live_out(B) = union of live_in(S) for every successor S inside the region
      live_in(B)  = use(B) + (live_out(B) - def(B))
    Iterated to a fixpoint over blocks in reverse region order (a couple of passes for
    reducible loops). What it is FOR: knowing which values are live ACROSS a call site,
    so that when unsupported ops become runtime-helper calls we spill only those - the
    same rule on Win64 and SysV, where the volatile-register sets differ but the question
    ("what survives the call?") does not. It is also the base the linear-scan allocator,
    native calls and range analysis all need. Computed here, not yet consumed: this pass
    must not change a single emitted byte. --------------------------------------------- }
  procedure ComputeLiveness;
  var
    nb, bi, k, k2, pass, si, r2: Integer;
    Blk, Succ: TSSABasicBlock;
    Ins: TSSAInstruction;
    Changed: Boolean;
    // (The per-block use/def/in/out bitsets are fields of the region now - see the declaration
    // beside PeakLive*: B1b's interval builder reads them after this pass returns.)
    CurLiveI, CurLiveF: array of Boolean;             // mid-block live set (peak measurement)
    used: array of Boolean; kk, bestr: Integer; bestv, totF, topF, totI, topI: Int64;  // payoff probe
    blkOf: array of Integer;                          // per float slot: single touch block / -2 many

    // Record which block(s) touch a float slot (block-local ceiling measurement).
    procedure NoteFloatBlk(const V: TSSAValue; bidx: Integer);
    var q: Integer;
    begin
      if (V.Kind <> svkRegister) or (V.RegType <> srtFloat) then Exit;
      q := Prog.AotRemapFloatReg(V.RegIndex);
      if (q < 0) or (q > MaxFReg) then Exit;
      if blkOf[q] = -1 then blkOf[q] := bidx
      else if blkOf[q] <> bidx then blkOf[q] := -2;
    end;

    // Linear-scan RESIDENCY SIMULATION for one bank: how much loop-weighted register use a real
    // live-range allocator with `nregs` machine registers would keep resident, versus the static
    // top-nregs the AOT pins today. Model: interval per slot = [first touch .. last touch] in
    // region emission order (linear, so it over-approximates liveness across control flow -> more
    // pressure -> the reclaim it reports is a LOWER BOUND); greedy scan, on overflow evict the
    // active slot with the least loop-weighted use (whole-interval spill, another conservatism).
    function LinScanResident(isFloat: Boolean; maxr, nregs: Integer): Int64;
    var
      first, last, ordr, active: array of Integer;
      spilled: array of Boolean;
      nord, activeCount, pp, bb, jj, a, w, mi, ins2, rr, tmp: Integer;
      B2: TSSABasicBlock; I2: TSSAInstruction;

      function Wt(r: Integer): Integer; inline;
      begin if isFloat then Wt := FUse[r] else Wt := IUse[r]; end;

      procedure Touch(const V: TSSAValue; atPos: Integer);
      var q: Integer;
      begin
        if V.Kind <> svkRegister then Exit;
        if isFloat then begin if V.RegType <> srtFloat then Exit; q := Prog.AotRemapFloatReg(V.RegIndex); end
        else begin if V.RegType <> srtInt then Exit; q := Prog.AotRemapIntReg(V.RegIndex); end;
        if (q < 0) or (q > maxr) then Exit;
        if atPos < first[q] then first[q] := atPos;
        if atPos > last[q] then last[q] := atPos;
      end;
    begin
      LinScanResident := 0;
      if maxr < 0 then Exit;
      SetLength(first, maxr + 1); SetLength(last, maxr + 1); SetLength(spilled, maxr + 1);
      for rr := 0 to maxr do begin first[rr] := MaxInt; last[rr] := -1; spilled[rr] := False; end;
      // Assign a linear position to every instruction and record each slot's touch span.
      pp := 0;
      for bb := 0 to nb - 1 do
      begin
        B2 := SSAProg.Blocks[Region.FirstBlock + bb];
        for jj := 0 to B2.Instructions.Count - 1 do
        begin
          I2 := B2.Instructions[jj];
          Touch(I2.Src1, pp); Touch(I2.Src2, pp); Touch(I2.Src3, pp); Touch(I2.Dest, pp);
          for a := 0 to High(I2.PhiSources) do Touch(I2.PhiSources[a].Value, pp);
          Inc(pp);
        end;
      end;
      // Order the touched slots by interval start.
      nord := 0; SetLength(ordr, maxr + 1);
      for rr := 0 to maxr do if last[rr] >= 0 then begin ordr[nord] := rr; Inc(nord); end;
      for a := 1 to nord - 1 do                                  // insertion sort by first[]
      begin
        tmp := ordr[a]; w := a - 1;
        while (w >= 0) and (first[ordr[w]] > first[tmp]) do begin ordr[w + 1] := ordr[w]; Dec(w); end;
        ordr[w + 1] := tmp;
      end;
      // Greedy linear scan.
      SetLength(active, nregs + 1); activeCount := 0;
      for a := 0 to nord - 1 do
      begin
        rr := ordr[a];
        w := 0;                                                  // expire intervals that ended
        for ins2 := 0 to activeCount - 1 do
          if last[active[ins2]] >= first[rr] then begin active[w] := active[ins2]; Inc(w); end;
        activeCount := w;
        if activeCount < nregs then begin active[activeCount] := rr; Inc(activeCount); end
        else
        begin
          mi := 0;                                               // evict the least-used live slot
          for ins2 := 1 to activeCount - 1 do
            if Wt(active[ins2]) < Wt(active[mi]) then mi := ins2;
          if Wt(rr) > Wt(active[mi]) then begin spilled[active[mi]] := True; active[mi] := rr; end
          else spilled[rr] := True;
        end;
      end;
      for rr := 0 to maxr do
        if (last[rr] >= 0) and not spilled[rr] then LinScanResident := LinScanResident + Wt(rr);
    end;

    // Mark an operand read as live in the mid-block replay set (diagnostic only).
    procedure MidMarkUse(const V: TSSAValue; var LiveI, LiveF: array of Boolean);
    var rr: Integer;
    begin
      if V.Kind <> svkRegister then Exit;
      if V.RegType = srtInt then
      begin
        rr := Prog.AotRemapIntReg(V.RegIndex);
        if (rr >= 0) and (rr <= MaxIReg) then LiveI[rr] := True;
      end
      else if V.RegType = srtFloat then
      begin
        rr := Prog.AotRemapFloatReg(V.RegIndex);
        if (rr >= 0) and (rr <= MaxFReg) then LiveF[rr] := True;
      end;
    end;

    function RegionIdx(B: TSSABasicBlock): Integer;   // -1 = outside this region
    var q: Integer;
    begin
      Result := -1;
      for q := Region.FirstBlock to Region.LastBlock do
        if SSAProg.Blocks[q] = B then Exit(q - Region.FirstBlock);
    end;
    // An operand READ: a use, unless the same instruction already defined it in this block.
    procedure MarkUse(const V: TSSAValue);
    var r: Integer;
    begin
      if V.Kind <> svkRegister then Exit;
      if V.RegType = srtInt then
      begin
        r := Prog.AotRemapIntReg(V.RegIndex);
        if (r >= 0) and (r <= MaxIReg) and not DefI[bi][r] then UseI[bi][r] := True;
      end
      else if V.RegType = srtFloat then
      begin
        r := Prog.AotRemapFloatReg(V.RegIndex);
        if (r >= 0) and (r <= MaxFReg) and not DefF[bi][r] then UseF[bi][r] := True;
      end;
    end;
    procedure MarkDef(const V: TSSAValue);
    var r: Integer;
    begin
      if V.Kind <> svkRegister then Exit;
      if V.RegType = srtInt then
      begin
        r := Prog.AotRemapIntReg(V.RegIndex);
        if (r >= 0) and (r <= MaxIReg) then DefI[bi][r] := True;
      end
      else if V.RegType = srtFloat then
      begin
        r := Prog.AotRemapFloatReg(V.RegIndex);
        if (r >= 0) and (r <= MaxFReg) then DefF[bi][r] := True;
      end;
    end;
  begin
    nb := Region.LastBlock - Region.FirstBlock + 1;
    if (nb <= 0) or (MaxIReg < 0) and (MaxFReg < 0) then Exit;
    LiveNB := nb;
    SetLength(UseI, nb); SetLength(DefI, nb); SetLength(InI, nb); SetLength(OutI, nb);
    SetLength(UseF, nb); SetLength(DefF, nb); SetLength(InF, nb); SetLength(OutF, nb);
    for k := 0 to nb - 1 do
    begin
      SetLength(UseI[k], MaxIReg + 1); SetLength(DefI[k], MaxIReg + 1);
      SetLength(InI[k], MaxIReg + 1);  SetLength(OutI[k], MaxIReg + 1);
      SetLength(UseF[k], MaxFReg + 1); SetLength(DefF[k], MaxFReg + 1);
      SetLength(InF[k], MaxFReg + 1);  SetLength(OutF[k], MaxFReg + 1);
    end;

    // Local use/def per block, in program order.
    for bi := 0 to nb - 1 do
    begin
      Blk := SSAProg.Blocks[Region.FirstBlock + bi];
      for k := 0 to Blk.Instructions.Count - 1 do
      begin
        Ins := Blk.Instructions[k];
        // Reads first, then the definition: a self-referencing "d := d + 1" is a use AND a def.
        MarkUse(Ins.Src1); MarkUse(Ins.Src2); MarkUse(Ins.Src3);
        // These opcodes carry a USE in Dest, not a definition (the canonical exception list -
        // SedaiSSAConstruction: array stores and prints read Dest).
        if (Ins.OpCode = ssaArrayStore) or (Ins.OpCode = ssaPrint) or (Ins.OpCode = ssaPrintLn) or
           (Ins.OpCode = ssaXferStoreInt) or (Ins.OpCode = ssaXferStoreFloat) then
          MarkUse(Ins.Dest)
        else
          MarkDef(Ins.Dest);
      end;
    end;

    // Backward fixpoint.
    pass := 0;
    repeat
      Changed := False;
      Inc(pass);
      for bi := nb - 1 downto 0 do
      begin
        Blk := SSAProg.Blocks[Region.FirstBlock + bi];
        // out = union of successors' in. A successor outside the region is an exit and
        // contributes nothing, which is correct: the epilogue flushes every allocated
        // register to the banks before leaving.
        for si := 0 to Blk.Successors.Count - 1 do
        begin
          Succ := TSSABasicBlock(Blk.Successors[si]);
          k := RegionIdx(Succ);
          if k < 0 then System.Continue;
          for r2 := 0 to MaxIReg do
            if InI[k][r2] and not OutI[bi][r2] then begin OutI[bi][r2] := True; Changed := True; end;
          for r2 := 0 to MaxFReg do
            if InF[k][r2] and not OutF[bi][r2] then begin OutF[bi][r2] := True; Changed := True; end;
        end;
        for r2 := 0 to MaxIReg do
          if (UseI[bi][r2] or (OutI[bi][r2] and not DefI[bi][r2])) and not InI[bi][r2] then
          begin InI[bi][r2] := True; Changed := True; end;
        for r2 := 0 to MaxFReg do
          if (UseF[bi][r2] or (OutF[bi][r2] and not DefF[bi][r2])) and not InF[bi][r2] then
          begin InF[bi][r2] := True; Changed := True; end;
      end;
    until (not Changed) or (pass > nb + 2);
    LivenessOK := pass <= nb + 2;

    // Register pressure: the peak number of simultaneously live values per bank. It is the
    // number the allocator and the future helper-call spill logic both care about, and it is
    // a checkable output of the dataflow (0 would mean the pass did nothing).
    PeakLiveInt := 0; PeakLiveFloat := 0;
    for bi := 0 to nb - 1 do
    begin
      k := 0;
      for r2 := 0 to MaxIReg do if InI[bi][r2] then Inc(k);
      if k > PeakLiveInt then PeakLiveInt := k;
      k := 0;
      for r2 := 0 to MaxIReg do if OutI[bi][r2] then Inc(k);
      if k > PeakLiveInt then PeakLiveInt := k;
      k := 0;
      for r2 := 0 to MaxFReg do if InF[bi][r2] then Inc(k);
      if k > PeakLiveFloat then PeakLiveFloat := k;
      k := 0;
      for r2 := 0 to MaxFReg do if OutF[bi][r2] then Inc(k);
      if k > PeakLiveFloat then PeakLiveFloat := k;
    end;
    AotDiagPeakLiveInt := PeakLiveInt;
    AotDiagPeakLiveFloat := PeakLiveFloat;
    // TRUE mid-block peak: replay each block backward from its live-out set (per-instruction),
    // so a value born and consumed inside the block counts. This is the number a linear-scan
    // allocator has to fit; peakLive above only samples block boundaries and understates it.
    AotDiagMaxLiveInt := PeakLiveInt; AotDiagMaxLiveFloat := PeakLiveFloat;
    for bi := 0 to nb - 1 do
    begin
      Blk := SSAProg.Blocks[Region.FirstBlock + bi];
      // Seed the live set with live-out, then walk instructions in reverse: an operand read
      // makes the reg live, a def (Dest, minus the use-in-Dest exceptions) kills it above.
      SetLength(CurLiveI, MaxIReg + 1); SetLength(CurLiveF, MaxFReg + 1);
      for r2 := 0 to MaxIReg do CurLiveI[r2] := OutI[bi][r2];
      for r2 := 0 to MaxFReg do CurLiveF[r2] := OutF[bi][r2];
      for k := Blk.Instructions.Count - 1 downto 0 do
      begin
        Ins := Blk.Instructions[k];
        // Def kills liveness above this point (before adding this instruction's own reads).
        if not ((Ins.OpCode = ssaArrayStore) or (Ins.OpCode = ssaPrint) or (Ins.OpCode = ssaPrintLn) or
                (Ins.OpCode = ssaXferStoreInt) or (Ins.OpCode = ssaXferStoreFloat)) then
        begin
          if Ins.Dest.Kind = svkRegister then
          begin
            if Ins.Dest.RegType = srtInt then
            begin
              r2 := Prog.AotRemapIntReg(Ins.Dest.RegIndex);
              if (r2 >= 0) and (r2 <= MaxIReg) then CurLiveI[r2] := False;
            end
            else if Ins.Dest.RegType = srtFloat then
            begin
              r2 := Prog.AotRemapFloatReg(Ins.Dest.RegIndex);
              if (r2 >= 0) and (r2 <= MaxFReg) then CurLiveF[r2] := False;
            end;
          end;
        end;
        MidMarkUse(Ins.Src1, CurLiveI, CurLiveF);
        MidMarkUse(Ins.Src2, CurLiveI, CurLiveF);
        MidMarkUse(Ins.Src3, CurLiveI, CurLiveF);
        if (Ins.OpCode = ssaArrayStore) or (Ins.OpCode = ssaPrint) or (Ins.OpCode = ssaPrintLn) or
           (Ins.OpCode = ssaXferStoreInt) or (Ins.OpCode = ssaXferStoreFloat) then
          MidMarkUse(Ins.Dest, CurLiveI, CurLiveF);
        // Cardinality AT this program point (values live across this instruction boundary).
        k2 := 0; for r2 := 0 to MaxIReg do if CurLiveI[r2] then Inc(k2);
        if k2 > AotDiagMaxLiveInt then AotDiagMaxLiveInt := k2;
        k2 := 0; for r2 := 0 to MaxFReg do if CurLiveF[r2] then Inc(k2);
        if k2 > AotDiagMaxLiveFloat then AotDiagMaxLiveFloat := k2;
      end;
    end;
    // Distinct VM registers actually touched, per bank (IUse/FUse are the region's use counts).
    // Reported beside peakLive because the two together say whether the pool is too small or the
    // VALUES are spread too thin: 6 xmm cover a peak of 3 live floats easily -- unless they are
    // scattered over dozens of never-reused register numbers, which is memory traffic by
    // construction, and no amount of x86-side work can take it back.
    AotDiagDistinctInt := 0;
    for r2 := 0 to MaxIReg do if IUse[r2] > 0 then Inc(AotDiagDistinctInt);
    AotDiagDistinctFloat := 0;
    for r2 := 0 to MaxFReg do if FUse[r2] > 0 then Inc(AotDiagDistinctFloat);

    // Payoff probe: loop-weighted use already resident (top pool-size slots the static
    // allocator pins) vs the spilled tail. Computed always (a few thousand iterations),
    // printed only under Diag. A large tail with small maxLive => linear-scan opportunity.
    totF := 0; topF := 0; totI := 0; topI := 0;
    for r2 := 0 to MaxFReg do totF := totF + FUse[r2];
    SetLength(used, MaxFReg + 1);
    for kk := 1 to 6 do
    begin
      bestr := -1; bestv := 0;
      for r2 := 0 to MaxFReg do if (not used[r2]) and (FUse[r2] > bestv) then begin bestr := r2; bestv := FUse[r2]; end;
      if bestr < 0 then Break; used[bestr] := True; topF := topF + bestv;
    end;
    for r2 := 0 to MaxIReg do totI := totI + IUse[r2];
    SetLength(used, 0); SetLength(used, MaxIReg + 1);
    for kk := 1 to 7 do
    begin
      bestr := -1; bestv := 0;
      for r2 := 0 to MaxIReg do if (not used[r2]) and (IUse[r2] > bestv) then begin bestr := r2; bestv := IUse[r2]; end;
      if bestr < 0 then Break; used[bestr] := True; topI := topI + bestv;
    end;
    AotDiagFloatResident := topF; AotDiagFloatTotal := totF;
    AotDiagIntResident := topI; AotDiagIntTotal := totI;
    AotDiagFloatLinScan := LinScanResident(True, MaxFReg, 6);
    AotDiagIntLinScan := LinScanResident(False, MaxIReg, 7);

    // Hybrid ceiling: block-local float temporaries (single block, not live-in/out of it).
    SetLength(blkOf, MaxFReg + 1);
    for r2 := 0 to MaxFReg do blkOf[r2] := -1;   // -1 = untouched, >=0 = single block, -2 = many
    for bi := 0 to nb - 1 do
    begin
      Blk := SSAProg.Blocks[Region.FirstBlock + bi];
      for k := 0 to Blk.Instructions.Count - 1 do
      begin
        Ins := Blk.Instructions[k];
        NoteFloatBlk(Ins.Src1, bi); NoteFloatBlk(Ins.Src2, bi); NoteFloatBlk(Ins.Src3, bi);
        NoteFloatBlk(Ins.Dest, bi);
        for k2 := 0 to High(Ins.PhiSources) do NoteFloatBlk(Ins.PhiSources[k2].Value, bi);
      end;
    end;
    AotDiagFloatBlockLocal := 0; AotDiagFloatBlockLocalCount := 0;
    for r2 := 0 to MaxFReg do
      if (blkOf[r2] >= 0) and not InF[blkOf[r2]][r2] and not OutF[blkOf[r2]][r2] then
      begin
        AotDiagFloatBlockLocal := AotDiagFloatBlockLocal + FUse[r2];
        Inc(AotDiagFloatBlockLocalCount);
      end;
    AotDiagLivenessOK := LivenessOK;
  end;

  { --- B1b: build the live intervals ----------------------------------------------------------
    Turns the liveness bitsets into RANGES (see TLsRange) and unions the ranges that carry the
    same value across CFG edges into WEBS (see TLsWeb).

    Where the splitting happens: a definition of a register that is DEAD at that point starts a
    new range, and that range is never unioned with the previous one - nothing flows between them.
    A definition that also READS the register ("d := d + 1") does not split: the read is a use of
    the incoming value, so the range continues. This is the "def .. last use before the next def"
    rule, and it is what undoes the live-range lengthening the REGREUSE merge introduces: the
    merge hands one register number to several values, and this reads them back apart.

    Position numbering is the region's linear emission order - the same running index the emit
    loop keeps in DynPos - so a schedule expressed in these positions can be replayed there
    directly.

    MEASUREMENT ONLY at this stage: nothing here changes an emitted byte. ------------------- }
  procedure BuildIntervals;
  var
    nb, bi, j, p, r, k, si, bank, maxr, nreg, rid, wid, tot: Integer;
    Blk, Succ: TSSABasicBlock;
    Ins: TSSAInstruction;
    W: Int64;
    OpenR: array of Integer;                  // reg -> currently open range (-1 = none)
    ReadHere: array of Boolean;               // reg -> read by the instruction being walked
    FirstR, LastR: array of array of Integer; // [block][reg] -> first / last range in that block
    RootOf: array of Integer;                 // range -> its union-find root, resolved up front
    WebOf: array of Integer;                  // range root -> web id (-1 = not assigned yet)
    Cover: array of Integer;                  // per position: ranges covering it (difference array)
    RangeUncov: array of Boolean;             // range -> reachable without passing through its web
    CoveredIn: array of array of Boolean;     // [block][reg] live-in and every predecessor has it
    HasPred: array of Boolean;                // block -> has at least one in-region predecessor

    function BankReg(const V: TSSAValue; out rr: Integer): Boolean;
    // Is this operand a register of the bank being walked, and which final index is it?
    begin
      Result := False; rr := -1;
      if V.Kind <> svkRegister then Exit;
      if (bank = 0) and (V.RegType = srtInt) then
        rr := Prog.AotRemapIntReg(V.RegIndex)
      else if (bank = 1) and (V.RegType = srtFloat) then
        rr := Prog.AotRemapFloatReg(V.RegIndex)
      else
        Exit;
      Result := (rr >= 0) and (rr <= maxr);
    end;

    function NewRange(rr, ablk, apos: Integer; onUse, isLiveIn: Boolean): Integer;
    begin
      if LsNRange >= Length(LsRanges) then SetLength(LsRanges, LsNRange * 2 + 64);
      Result := LsNRange;
      LsRanges[Result].Bank := bank;
      LsRanges[Result].Reg := rr;
      LsRanges[Result].Blk := ablk;
      LsRanges[Result].PStart := apos;
      LsRanges[Result].PEnd := apos;
      LsRanges[Result].OpensOnUse := onUse;
      LsRanges[Result].Wrote := not onUse;
      LsRanges[Result].LiveIn := isLiveIn;
      LsRanges[Result].LiveOut := False;
      LsRanges[Result].Web := Result;         // union-find: its own root until unioned
      LsRanges[Result].Weight := 0;
      Inc(LsNRange);
      if FirstR[ablk][rr] < 0 then FirstR[ablk][rr] := Result;
      LastR[ablk][rr] := Result;
    end;

    function Find(x: Integer): Integer;
    begin
      while LsRanges[x].Web <> x do
      begin
        LsRanges[x].Web := LsRanges[LsRanges[x].Web].Web;   // path halving
        x := LsRanges[x].Web;
      end;
      Result := x;
    end;

    procedure Union(a, b: Integer);
    var ra, rb: Integer;
    begin
      if (a < 0) or (b < 0) then Exit;
      ra := Find(a); rb := Find(b);
      if ra <> rb then LsRanges[rb].Web := ra;
    end;

  begin
    LsOK := False; LsWhy := ''; LsNRange := 0; LsNWeb := 0;
    if not LivenessOK then begin LsWhy := 'liveness'; Exit; end;
    nb := LiveNB;
    if nb <= 0 then begin LsWhy := 'no-liveness'; Exit; end;

    // Linear positions, and the two shapes this model does not describe.
    SetLength(LsPos0, nb);
    LsNPos := 0;
    for bi := 0 to nb - 1 do
    begin
      Blk := SSAProg.Blocks[Region.FirstBlock + bi];
      LsPos0[bi] := LsNPos;
      // An EMPTY block would need a range with no position to carry a value across it, and every
      // consumer downstream indexes by position. Rare enough to refuse rather than special-case.
      if Blk.Instructions.Count = 0 then begin LsWhy := 'empty-block'; Exit; end;
      Inc(LsNPos, Blk.Instructions.Count);
      for j := 0 to Blk.Instructions.Count - 1 do
        // A real PHI is gone by register allocation. If one survived, its operands would be live
        // over EDGES rather than at a position, which this position-indexed model cannot say.
        // (The PhiSources ARRAY, on the other hand, is alive and well as an extra-operand vector -
        // ssaArrayDim carries the dimension registers there so DCE can see them, and the graphics
        // family its 4th operand onwards. Those are ordinary uses; see the walk below.)
        if TSSAInstruction(Blk.Instructions[j]).OpCode = ssaPhi then
        begin LsWhy := 'phi'; Exit; end;
    end;
    if LsNPos = 0 then begin LsWhy := 'no-positions'; Exit; end;

    SetLength(LsRanges, 256);
    AotDiagLsCross := 0;

    for bank := 0 to 1 do
    begin
      if bank = 0 then maxr := MaxIReg else maxr := MaxFReg;
      if maxr < 0 then System.Continue;
      nreg := maxr + 1;
      // Guard: the per-block tables are blocks x registers. A huge region is not worth gigabytes
      // for an optimisation; it stays on the static homes, exactly as today.
      if Int64(nb) * nreg > 8 * 1000 * 1000 then begin LsWhy := 'too-large'; Exit; end;

      SetLength(OpenR, nreg); SetLength(ReadHere, nreg);
      SetLength(FirstR, 0); SetLength(LastR, 0);
      SetLength(FirstR, nb); SetLength(LastR, nb);
      for bi := 0 to nb - 1 do
      begin
        SetLength(FirstR[bi], nreg); SetLength(LastR[bi], nreg);
        for r := 0 to nreg - 1 do begin FirstR[bi][r] := -1; LastR[bi][r] := -1; end;
      end;

      for bi := 0 to nb - 1 do
      begin
        Blk := SSAProg.Blocks[Region.FirstBlock + bi];
        W := BlockW[bi];
        for r := 0 to nreg - 1 do OpenR[r] := -1;
        // Live-in values are already somewhere when the block starts: open their range at the
        // block's first position, reading (a load, if the predecessors left them in the bank).
        for r := 0 to nreg - 1 do
          if ((bank = 0) and InI[bi][r]) or ((bank = 1) and InF[bi][r]) then
            OpenR[r] := NewRange(r, bi, LsPos0[bi], True, True);

        for j := 0 to Blk.Instructions.Count - 1 do
        begin
          Ins := Blk.Instructions[j];
          p := LsPos0[bi] + j;
          for r := 0 to nreg - 1 do ReadHere[r] := False;

          // Reads first, then the write - the order liveness uses, and the reason "d := d + 1"
          // counts as a use of the incoming value.
          if BankReg(Ins.Src1, r) then ReadHere[r] := True;
          if BankReg(Ins.Src2, r) then ReadHere[r] := True;
          if BankReg(Ins.Src3, r) then ReadHere[r] := True;
          // The canonical exception list (identical to ComputeLiveness): for these opcodes Dest
          // carries an INPUT, not a definition.
          if ((Ins.OpCode = ssaArrayStore) or (Ins.OpCode = ssaPrint) or (Ins.OpCode = ssaPrintLn) or
              (Ins.OpCode = ssaXferStoreInt) or (Ins.OpCode = ssaXferStoreFloat)) and
             BankReg(Ins.Dest, r) then
            ReadHere[r] := True;
          // Operands beyond the third live in the PhiSources vector (ssaArrayDim's dimension
          // registers, the graphics family's tail). ComputeLiveness does NOT count them, so a
          // register used ONLY there looks dead to the dataflow. Counting them here can therefore
          // open a range where liveness says nothing is live - which is exactly right, and safe,
          // because a range that opens on a use loads from the bank, and the bank is authoritative
          // whenever a value is not resident (every web that writes stores back when it ends).
          for k := 0 to High(Ins.PhiSources) do
            if BankReg(Ins.PhiSources[k].Value, r) then ReadHere[r] := True;

          for r := 0 to nreg - 1 do
            if ReadHere[r] then
            begin
              // A read with nothing open reads a value this analysis never saw defined (a region
              // live-in, or one of the invisible-use shapes). Opening on a use is the honest
              // description: it says the value has to come from the bank.
              if OpenR[r] < 0 then OpenR[r] := NewRange(r, bi, p, True, False);
              LsRanges[OpenR[r]].PEnd := p;
              LsRanges[OpenR[r]].Weight := LsRanges[OpenR[r]].Weight + W;
            end;

          if not ((Ins.OpCode = ssaArrayStore) or (Ins.OpCode = ssaPrint) or (Ins.OpCode = ssaPrintLn) or
                  (Ins.OpCode = ssaXferStoreInt) or (Ins.OpCode = ssaXferStoreFloat)) then
            if BankReg(Ins.Dest, r) then
            begin
              if ReadHere[r] then
              begin
                // Read-modify-write: the incoming value is this instruction's input, so the range
                // runs on. NOT a split point - but it IS a write.
                LsRanges[OpenR[r]].PEnd := p;
                LsRanges[OpenR[r]].Wrote := True;
              end
              else
              begin
                // A definition while the register is dead (or holding a value whose last use is
                // behind us): THE split point. The old range keeps the end its last use gave it.
                OpenR[r] := NewRange(r, bi, p, False, False);
                LsRanges[OpenR[r]].Weight := LsRanges[OpenR[r]].Weight + W;
              end;
            end;
        end;

        // Anything live out of the block has to survive to the block's last position.
        p := LsPos0[bi] + Blk.Instructions.Count - 1;
        for r := 0 to nreg - 1 do
          if (OpenR[r] >= 0) and (((bank = 0) and OutI[bi][r]) or ((bank = 1) and OutF[bi][r])) then
          begin
            LsRanges[OpenR[r]].PEnd := p;
            LsRanges[OpenR[r]].LiveOut := True;
          end;
      end;

      // Union across CFG edges: a value live out of P and live in to S is ONE value. The same walk
      // answers the COVERAGE question: is every path into S carrying this value, or can control
      // arrive with the register holding something else? Only a covered entry may inherit a
      // machine home; an uncovered one is where the bank's value (the implicit zero of a variable
      // never assigned on that path, among others) has to be readable.
      SetLength(CoveredIn, 0); SetLength(CoveredIn, nb);
      SetLength(HasPred, 0); SetLength(HasPred, nb);
      for bi := 0 to nb - 1 do
      begin
        SetLength(CoveredIn[bi], nreg);
        for r := 0 to nreg - 1 do CoveredIn[bi][r] := True;
      end;
      for bi := 0 to nb - 1 do
      begin
        Blk := SSAProg.Blocks[Region.FirstBlock + bi];
        for si := 0 to Blk.Successors.Count - 1 do
        begin
          Succ := TSSABasicBlock(Blk.Successors[si]);
          k := -1;
          for j := Region.FirstBlock to Region.LastBlock do
            if SSAProg.Blocks[j] = Succ then begin k := j - Region.FirstBlock; Break; end;
          if k < 0 then System.Continue;                  // leaves the region: an exit
          // An edge back into the region's ENTRY block is not a value-carrying edge. A
          // self-recursive function has one - the call site's successor is its own entry - and
          // following it would let liveness conclude that the parameter "arrives" already in a
          // machine register on entry, when the entry is reached from the CALLER, through the
          // banks. (Found the honest way: a recursive Fib returning a pointer-shaped integer.)
          if k = 0 then System.Continue;
          HasPred[k] := True;
          for r := 0 to nreg - 1 do
            if ((bank = 0) and InI[k][r]) or ((bank = 1) and InF[k][r]) then
            begin
              if ((bank = 0) and OutI[bi][r]) or ((bank = 1) and OutF[bi][r]) then
              begin
                Union(LastR[bi][r], FirstR[k][r]);
                Inc(AotDiagLsCross);
              end
              else
                CoveredIn[k][r] := False;                 // arrives here without the value
            end;
        end;
      end;

      // Mark the uncovered ranges. Two shapes: a live-in with an incomplete set of predecessors
      // (the region's entry block among them - its live-ins come from outside, through the bank),
      // and a mid-block read of something the dataflow never saw defined.
      SetLength(RangeUncov, LsNRange);
      for k := 0 to LsNRange - 1 do
        if LsRanges[k].Bank = bank then
        begin
          if LsRanges[k].LiveIn then
          begin
            // The region's entry block is always entered from OUTSIDE - that is what makes it the
            // entry - so whatever is live in there arrives through the banks, whether or not some
            // block inside the region also names it as a successor.
            if (LsRanges[k].Blk = 0) or (not HasPred[LsRanges[k].Blk]) or
               (not CoveredIn[LsRanges[k].Blk][LsRanges[k].Reg]) then
              RangeUncov[k] := True;
          end
          else if LsRanges[k].OpensOnUse then
            RangeUncov[k] := True;
        end;
    end;

    // Compact the union-find roots into dense web ids and summarise each web.
    // Resolve every range's ROOT first, in its own pass: rewriting Web from "union-find parent"
    // to "web id" while Find is still being called walks a later query into a web id it reads as
    // a parent index - and the first id that is not a valid range index is -1, which is a read
    // off the front of the array. (Found the honest way: a range check error on m271.)
    SetLength(RootOf, LsNRange);
    for k := 0 to LsNRange - 1 do RootOf[k] := Find(k);
    SetLength(WebOf, LsNRange);
    for k := 0 to LsNRange - 1 do WebOf[k] := -1;
    SetLength(LsWebs, LsNRange);
    LsNWeb := 0;
    for k := 0 to LsNRange - 1 do
    begin
      rid := RootOf[k];
      if WebOf[rid] < 0 then
      begin
        wid := LsNWeb; Inc(LsNWeb);
        WebOf[rid] := wid;
        LsWebs[wid].Bank := LsRanges[k].Bank;
        LsWebs[wid].Reg := LsRanges[k].Reg;
        LsWebs[wid].PStart := LsRanges[k].PStart;
        LsWebs[wid].PEnd := LsRanges[k].PEnd;
        LsWebs[wid].NRange := 0;
        LsWebs[wid].Weight := 0;
        LsWebs[wid].NeedsLoad := False;
        LsWebs[wid].HasDef := False;
        LsWebs[wid].NUncov := 0;
        LsWebs[wid].UncovPos := -1;
        LsWebs[wid].Home := -1;
        LsWebs[wid].StoreEarly := False;
      end;
      wid := WebOf[rid];
      Inc(LsWebs[wid].NRange);
      LsWebs[wid].Weight := LsWebs[wid].Weight + LsRanges[k].Weight;
      if LsRanges[k].PStart < LsWebs[wid].PStart then LsWebs[wid].PStart := LsRanges[k].PStart;
      if LsRanges[k].PEnd > LsWebs[wid].PEnd then LsWebs[wid].PEnd := LsRanges[k].PEnd;
      if LsRanges[k].Wrote then LsWebs[wid].HasDef := True;
      if RangeUncov[k] then
      begin
        Inc(LsWebs[wid].NUncov);
        if (LsWebs[wid].UncovPos < 0) or (LsRanges[k].PStart < LsWebs[wid].UncovPos) then
          LsWebs[wid].UncovPos := LsRanges[k].PStart;
      end;
    end;
    // Rewrite each range's Web field from "union-find parent" to "web id" - from here on it is
    // an index into LsWebs, and Find must not be called again.
    for k := 0 to LsNRange - 1 do LsRanges[k].Web := WebOf[RootOf[k]];

    // Peak overlap per bank, over the RANGES (holes excluded, which is the point): the number a
    // machine register pool actually has to fit.
    AotDiagLsMaxOverInt := 0; AotDiagLsMaxOverFloat := 0;
    for bank := 0 to 1 do
    begin
      SetLength(Cover, 0); SetLength(Cover, LsNPos + 2);
      for k := 0 to LsNRange - 1 do
        if LsRanges[k].Bank = bank then
        begin
          Inc(Cover[LsRanges[k].PStart]);
          Dec(Cover[LsRanges[k].PEnd + 1]);
        end;
      tot := 0;
      for p := 0 to LsNPos - 1 do
      begin
        Inc(tot, Cover[p]);
        if bank = 0 then
        begin if tot > AotDiagLsMaxOverInt then AotDiagLsMaxOverInt := tot; end
        else
          if tot > AotDiagLsMaxOverFloat then AotDiagLsMaxOverFloat := tot;
      end;
    end;

    AotDiagLsWebsInt := 0; AotDiagLsWebsFloat := 0;
    for k := 0 to LsNWeb - 1 do
      if LsWebs[k].Bank = 0 then Inc(AotDiagLsWebsInt) else Inc(AotDiagLsWebsFloat);
    AotDiagLsRanges := LsNRange;
    if GetEnvironmentVariable('LS_DUMP') = '1' then
      for k := 0 to LsNRange - 1 do
        WriteLn(ErrOutput, Format('[LSR] range %d web=%d bank=%d reg=%d blk=%d [%d..%d] onUse=%s wrote=%s in=%s out=%s uncov=%s haspred=%s',
          [k, LsRanges[k].Web, LsRanges[k].Bank, LsRanges[k].Reg, LsRanges[k].Blk,
           LsRanges[k].PStart, LsRanges[k].PEnd,
           BoolToStr(LsRanges[k].OpensOnUse, 'y', 'n'), BoolToStr(LsRanges[k].Wrote, 'y', 'n'),
           BoolToStr(LsRanges[k].LiveIn, 'y', 'n'), BoolToStr(LsRanges[k].LiveOut, 'y', 'n'),
           BoolToStr(RangeUncov[k], 'y', 'n'), BoolToStr(HasPred[LsRanges[k].Blk], 'y', 'n')]));
    LsOK := True;
  end;

  procedure Prescan;
  var
    b, j, o: Integer;
    Blk: TSSABasicBlock;
    Ins: TSSAInstruction;
    UseW: Integer;                      // current block's loop-depth weight (B1b-lite)

    // B1b-lite: weight use counts by loop depth, so the greedy allocator stops preferring
    // init-code registers (many STATIC occurrences, run once) over hot-loop registers (few
    // occurrences, run a million times). A backward jump to block T from block B marks the
    // contiguous interval [T..B] as a loop body - exact for the reducible, contiguously
    // laid-out loops FOR/WHILE/DO produce. Deduped per header (a CONTINUE adds a second
    // back edge to the same header, which must not double the weight): each header applies
    // one x8 over [header..furthest back-jump source], capped at x512 (3 nesting levels).
    procedure ComputeBlockWeights;
    var
      L: TStringList;
      bb, jj, d, t, w: Integer;
      HdrT, HdrEnd: array of Integer;
      NHdr: Integer;
      B2: TSSABasicBlock;
      I2: TSSAInstruction;
    begin
      SetLength(BlockW, Region.LastBlock - Region.FirstBlock + 1);
      for bb := 0 to High(BlockW) do BlockW[bb] := 1;
      NHdr := 0;
      SetLength(HdrT, 8); SetLength(HdrEnd, 8);
      L := TStringList.Create;
      try
        L.Sorted := True;
        L.Duplicates := dupIgnore;
        for bb := Region.FirstBlock to Region.LastBlock do
          if SSAProg.Blocks[bb].LabelName <> '' then
            L.AddObject(SSAProg.Blocks[bb].LabelName, TObject(PtrInt(bb)));
        for bb := Region.FirstBlock to Region.LastBlock do
        begin
          B2 := SSAProg.Blocks[bb];
          for jj := 0 to B2.Instructions.Count - 1 do
          begin
            I2 := B2.Instructions[jj];
            if (I2.OpCode <> ssaJump) and (I2.OpCode <> ssaJumpIfZero) and
               (I2.OpCode <> ssaJumpIfNotZero) then Continue;
            if I2.Dest.Kind <> svkLabel then Continue;
            d := L.IndexOf(I2.Dest.LabelName);
            if d < 0 then Continue;
            t := PtrInt(L.Objects[d]);
            if t > bb then Continue;                     // forward edge: not a loop
            for d := 0 to NHdr - 1 do
              if HdrT[d] = t then
              begin
                if bb > HdrEnd[d] then HdrEnd[d] := bb;  // same header: widen, don't re-count
                t := -1;
                Break;
              end;
            if t < 0 then Continue;
            if NHdr >= Length(HdrT) then
            begin SetLength(HdrT, NHdr * 2); SetLength(HdrEnd, NHdr * 2); end;
            HdrT[NHdr] := t; HdrEnd[NHdr] := bb; Inc(NHdr);
          end;
        end;
      finally
        L.Free;
      end;
      for d := 0 to NHdr - 1 do
        for bb := HdrT[d] to HdrEnd[d] do
        begin
          w := BlockW[bb - Region.FirstBlock];
          if w < 512 then BlockW[bb - Region.FirstBlock] := w * 8;
        end;
    end;

    procedure CountVal(const V: TSSAValue);
    var r: Integer;
    begin
      if V.Kind <> svkRegister then Exit;
      case V.RegType of
        srtInt: begin
          r := Prog.AotRemapIntReg(V.RegIndex);
          if r < 0 then begin Fail('unmapped-reg'); Exit; end;
          if r > MaxIReg then MaxIReg := r;
          if r >= Length(IUse) then SetLength(IUse, r * 2 + 16);
          Inc(IUse[r], UseW);
        end;
        srtFloat: begin
          r := Prog.AotRemapFloatReg(V.RegIndex);
          if r < 0 then begin Fail('unmapped-reg'); Exit; end;
          if r > MaxFReg then MaxFReg := r;
          if r >= Length(FUse) then SetLength(FUse, r * 2 + 16);
          Inc(FUse[r], UseW);
        end;
        else Fail('string-operand');
      end;
    end;
    // C4: this instruction will be a runtime-helper call. Operands are deliberately NOT
    // counted - the helper reads and writes them in the banks, so pinning them to a machine
    // register would only add a flush/reload, and it is what lets STRING operands through at
    // all (CountVal rejects those outright).
    procedure NoteHelperOp;
    begin
      if not AotHelperRoutable(Prog, o) then
        Fail('helper:' + OpName(Ins.OpCode))
      else
      begin
        HasHelperCall := True;
        // A helper call can hand the rest of the invocation back to the interpreter (a moved
        // PC or a sentinel), so it carries a deopt's hazard and obeys the same rules.
        HasDeopt := True;
      end;
    end;
  begin
    ComputeBlockWeights;
    UseW := 1;
    o := Region.FirstOrdinal;
    for b := Region.FirstBlock to Region.LastBlock do
    begin
      Blk := SSAProg.Blocks[b];
      UseW := BlockW[b - Region.FirstBlock];
      for j := 0 to Blk.Instructions.Count - 1 do
      begin
        Ins := Blk.Instructions[j];
        case Ins.OpCode of
          ssaLabel, ssaNop: ;
          ssaRecMarkPush, ssaRecMarkPop:
          begin
            // In a region with NO deopt hazard the marks are skipped (the whole invocation
            // is native, so both push and pop are elided together: balance holds and
            // reclamation is deferred to FramePop). With a deopt hazard they are routed
            // through the helper instead, so the mark stack stays balanced no matter where
            // the interpreter takes over - decided after the scan, when HasDeopt is final.
            // C6: with the gate on they instead become a two-instruction leaf call, which is
            // balanced by construction and costs a fraction of the helper round trip.
            HasRecMark := True;
            if not AotHelperRoutable(Prog, o) then RecMarkRoutable := False;
          end;
          // C6: record allocation as a leaf call. Like the string primitives it needs a
          // call-ready frame but always completes natively - no deopt hazard. Only the int
          // operands are counted: New's three "sources" are compile-time slot counts read from
          // the bytecode instruction at emit time, not registers.
          ssaRecordNew, ssaRecordFree:
          begin
            if not AotIsNative(SSAProg, Ins) then NoteHelperOp
            else
            begin
              HasHelperCall := True;
              HasNativeRecAlloc := True;
              if Ins.OpCode = ssaRecordNew then
              begin
                CountVal(Ins.Dest);                   // the handle
                if Prog.GetSsaPc(o) < 0 then
                  Fail('no-pc-recnew');               // needs the bytecode Src1/Src2/Immediate
              end
              else
                CountVal(Ins.Src1);                   // the handle to release
            end;
          end;
          ssaJump: ;
          ssaJumpIfZero, ssaJumpIfNotZero: CountVal(Ins.Src1);
          ssaReturnSub, ssaEnd, ssaStop:
            if Prog.GetSsaPc(o) < 0 then Fail('no-pc-exit');
          ssaDivInt, ssaModInt, ssaDivUInt, ssaModUInt:
          begin
            HasDeopt := True;
            if Prog.GetSsaPc(o) < 0 then Fail('no-pc-trap');
            CountVal(Ins.Dest); CountVal(Ins.Src1); CountVal(Ins.Src2);
          end;
          ssaDivFloat, ssaMathSqr:
          begin
            if not Modern then
            begin
              HasDeopt := True;
              if Prog.GetSsaPc(o) < 0 then Fail('no-pc-trap');
            end;
            CountVal(Ins.Dest); CountVal(Ins.Src1);
            if Ins.OpCode = ssaDivFloat then CountVal(Ins.Src2);
          end;
          ssaXferLoadInt, ssaXferLoadFloat: CountVal(Ins.Dest);
          ssaXferStoreInt, ssaXferStoreFloat: CountVal(Ins.Src1);
          ssaNarrowInt: begin CountVal(Ins.Dest); CountVal(Ins.Src1); end;
          ssaArrayLoad, ssaArrayStore:
          begin
            // A shape the native path cannot take (computed array ref, string elements) is no
            // longer fatal: it falls back to the helper like any other non-native op.
            if not AotIsNative(SSAProg, Ins) then NoteHelperOp
            else
            begin
              // B4: a proven-safe access emits no guard, so it needs no deopt PC even
              // under CLASSIC. Same condition as the emitter (they must agree).
              if ArrClassic and not Ins.BoundsSafe then
              begin
                HasDeopt := True;
                if Prog.GetSsaPc(o) < 0 then Fail('no-pc-arr');
              end;
              // A STRING element is reached through a ctx primitive, exactly like the string
              // leaf ops below: the string operand stays in the bank and must NOT be counted
              // (CountVal rejects string registers outright, which is what made the whole
              // region bail with 'string-operand' the first time this path was opened). The
              // call needs a call-ready frame but always completes natively - no deopt hazard.
              if SSAProg.GetArray(Ins.Src1.ArrayIndex).ElementType = srtString then
              begin
                HasHelperCall := True;
                CountVal(Ins.Src2);                    // the index only
              end
              else
              begin
                CountVal(Ins.Dest); CountVal(Ins.Src2);
              end;
              if Ins.Src1.ArrayIndex > MaxArrId then MaxArrId := Ins.Src1.ArrayIndex;
              if Ins.Src1.ArrayIndex >= Length(AUse) then
              begin
                SetLength(AUse, Ins.Src1.ArrayIndex * 2 + 8);
                SetLength(ArrCountNeeded, Length(AUse));
              end;
              Inc(AUse[Ins.Src1.ArrayIndex], UseW);   // loop-weighted, like CountVal
              if not Ins.BoundsSafe then
                ArrCountNeeded[Ins.Src1.ArrayIndex] := True;
            end;
          end;
          ssaArrayLBound, ssaArrayUBound:
          begin
            HasDeopt := True;                       // dim <> 0 deopts even in MODERN
            if Prog.GetSsaPc(o) < 0 then Fail('no-pc-bound');
            CountVal(Ins.Dest);
            if Ins.Src2.Kind = svkRegister then CountVal(Ins.Src2);
          end;
          ssaCallSub:
          begin
            // B3: a native call site. Needs a call-ready frame; AotCallSub can hand the rest
            // of the invocation to the interpreter (callee not compiled, deopt inside the
            // callee, exception), so it carries a deopt's hazard - same recmark rule as the
            // helpers. The bcCallSub PC is required both as the fallback resume point and to
            // read the callee entry PC out of the instruction's Immediate at emit time.
            if Ins.Dest.Kind <> svkLabel then NoteHelperOp   // indirect: not routable -> bail
            else
            begin
              HasHelperCall := True;
              HasDeopt := True;
              if Prog.GetSsaPc(o) < 0 then Fail('no-pc-callsub');
            end;
          end;
          ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString,
          ssaCopyString, ssaLoadConstString, ssaStrConcat, ssaStrLen,
          ssaStrLeft, ssaStrRight, ssaStrMid, ssaStrAsc, ssaStrAscMid, ssaStrConcatCharAt, ssaStrAppendMapped, ssaStrChr, ssaStrInstr,
          ssaIntToString, ssaStrVal, ssaStrValInt:
          begin
            // C5: a native leaf call to a string primitive. String operands stay in the bank
            // (not register-allocated, not counted - CountVal rejects them); only INT operands
            // (results, lengths, positions, char codes) are counted. The call needs a call-ready
            // frame but always completes natively, so it sets the frame flag WITHOUT a deopt
            // hazard. A non-native shape (e.g. a const operand) falls back to the helper.
            if not AotIsNative(SSAProg, Ins) then NoteHelperOp
            else
            begin
              HasHelperCall := True;
              case Ins.OpCode of
                ssaStrLen, ssaCmpEqString, ssaCmpNeString, ssaCmpLtString,
                ssaCmpGtString, ssaStrAsc:
                  CountVal(Ins.Dest);                              // the int result
                ssaStrLeft, ssaStrRight:
                  CountVal(Ins.Src2);                              // the length
                ssaStrMid:
                  begin CountVal(Ins.Src2); CountVal(Ins.Src3); end;   // start + length
                ssaStrAscMid:
                  begin CountVal(Ins.Dest); CountVal(Ins.Src2); CountVal(Ins.Src3); end; // code + start + len
                ssaStrConcatCharAt, ssaStrAppendMapped:
                  CountVal(Ins.Src3);                              // the index (dest and both sources are strings)
                ssaStrChr:
                  CountVal(Ins.Src1);                              // the char code
                ssaStrInstr:
                  begin CountVal(Ins.Dest); CountVal(Ins.Src3); end;   // result + start
                ssaIntToString:
                  CountVal(Ins.Src1);                              // the int value
                ssaStrVal, ssaStrValInt:
                  CountVal(Ins.Dest);                              // the parsed number
              end;
              if (Ins.OpCode = ssaLoadConstString) and (Prog.GetSsaPc(o) < 0) then
                Fail('no-pc-strconst');             // needs the bytecode Immediate
            end;
          end;
        else
          if AotIsNative(SSAProg, Ins) then
          begin
            CountVal(Ins.Dest);
            if Ins.Src1.Kind = svkRegister then CountVal(Ins.Src1);
            if Ins.Src2.Kind = svkRegister then CountVal(Ins.Src2);
            if Ins.Src3.Kind = svkRegister then CountVal(Ins.Src3);
          end
          else
            NoteHelperOp;
        end;
        if not OK then Exit;
        Inc(o);
      end;
    end;
    // A mid-function deopt hands the REST of the invocation to the interpreter; SKIPPED
    // RecMark pushes would then unbalance the record-mark stack. So with both present the
    // marks are NOT skipped: they run through the helper (real push/pop, order preserved),
    // which also needs a call-ready frame. B3 made this the common case (every region whose
    // loop body contains a call carries marks AND a deopt hazard - the old hard bail here
    // would have kept MAIN uncompilable).
    // C6: when the record family is native the marks become a leaf call - always emitted (never
    // elided) as soon as the region allocates records natively, so reclamation stays exactly as
    // eager as it is today. Before C6 such a region ALWAYS carried a helper (New itself was one),
    // hence always a deopt hazard, hence marks that ran: eliding them here instead would defer
    // every temporary to FramePop and change how much memory a hot allocating loop holds.
    RecMarkNative := HasRecMark and AotRecAllocNative and (HasDeopt or HasNativeRecAlloc);
    if HasRecMark and HasDeopt and not RecMarkNative then
    begin
      if not RecMarkRoutable then Fail('recmark-route');
      HasHelperCall := True;
    end;
    if RecMarkNative then HasHelperCall := True;    // a leaf call still needs a call-ready frame
    // The region's last instruction must leave natively (no fall-through off the end).
    Blk := SSAProg.Blocks[Region.LastBlock];
    if Blk.Instructions.Count = 0 then Fail('empty-last-block')
    else
    begin
      // (Explicit comparisons: TSSAOpCode has >256 values, so a set constructor is illegal.)
      Ins := Blk.Instructions[Blk.Instructions.Count - 1];
      if not ((Ins.OpCode = ssaReturnSub) or (Ins.OpCode = ssaEnd) or
              (Ins.OpCode = ssaStop) or (Ins.OpCode = ssaJump)) then
        Fail('open-region-end');
    end;
  end;

  procedure Allocate;
  var r, k, id, best, bestUse, bestKind: Integer;
      Taken, TakenAB, TakenAC: array of Boolean;
  begin
    // GPRs r9..r15: UNIFIED candidate pool by use count (the JIT's J6f model) - VM int
    // registers (kind -1) compete with array descriptor slots (kind 0 = data base,
    // kind 1 = count; base preferred over count of the same array at equal frequency).
    SetLength(Taken, MaxIReg + 1);
    SetLength(TakenAB, MaxArrId + 1);
    SetLength(TakenAC, MaxArrId + 1);
    for k := 0 to IntPoolCount(Length(IntPool) - Ord(not RsiIsPool)) - 1 do
    begin
      best := -1; bestUse := 0; bestKind := -2;
      for r := 0 to MaxIReg do
        if (not Taken[r]) and (IUse[r] > bestUse) then
        begin best := r; bestUse := IUse[r]; bestKind := -1; end;
      for id := 0 to MaxArrId do
      begin
        // For a BASE, AUse is exactly the dynamic reload count avoided by caching (one reload
        // per access, loop-weighted). A COUNT competes only when some non-safe access will
        // actually read it - an all-BoundsSafe array's count slot would be dead weight.
        if (not TakenAB[id]) and (AUse[id] > bestUse) then
        begin best := id; bestUse := AUse[id]; bestKind := 0; end;
        if (not TakenAC[id]) and (id <= High(ArrCountNeeded)) and ArrCountNeeded[id] and
           (AUse[id] > bestUse) then
        begin best := id; bestUse := AUse[id]; bestKind := 1; end;
      end;
      if bestKind = -2 then Break;
      if bestKind = -1 then
      begin
        Taken[best] := True;
        ILoc[best] := IntPool[k];
        IAllocd[NIAlloc] := best; Inc(NIAlloc);
      end
      else
      begin
        if bestKind = 0 then TakenAB[best] := True else TakenAC[best] := True;
        ACacheId[NACache] := best;
        ACacheKind[NACache] := bestKind;
        ACacheReg[NACache] := IntPool[k];
        Inc(NACache);
      end;
      // rbx and rsi are pushed by name in the prologue; SaveGpr must not push them a second time.
      if GprIsCalleeSaved(IntPool[k]) and (IntPool[k] <> RSI) and (IntPool[k] <> RBX) then
        SaveGpr[IntPool[k]] := True;
    end;
    // Floats: most-used first onto xmm2..xmm7.
    // AOT_FPOOL SHRINKS the pool (probe): the slope of "how much does one xmm less cost" is what
    // says whether paying for xmm8-15 - which every float encoding would have to grow a REX prefix
    // for - can buy anything. Never widens; values above 6 are ignored.
    SetLength(Taken, 0); SetLength(Taken, MaxFReg + 1);
    for k := 2 to FloatPoolTop do
    begin
      best := -1; bestUse := 0;
      for r := 0 to MaxFReg do
        if (not Taken[r]) and (FUse[r] > bestUse) then begin best := r; bestUse := FUse[r]; end;
      if best < 0 then Break;
      Taken[best] := True;
      FLoc[best] := k;
      FAllocd[NFAlloc] := best; Inc(NFAlloc);
      if k >= 6 then SaveXmm[k] := True;
    end;
  end;

  // AOT_DYNF: build the within-block dynamic float schedule. Runs after Allocate and OVERRIDES
  // its static float homes only if it admits at least one temp; otherwise the static homes stand
  // (a region with no block-local float temporaries must not regress). A temp is admitted when
  // it is block-local (all touches in one block) AND def-before-use (its first def precedes its
  // first use, so no read hits an unwritten xmm - the implicit-zero hazard). Each admitted temp
  // holds one xmm for [firstDef .. lastTouch]; the greedy scan never evicts a started interval
  // (on overflow the newcomer stays in the bank), so an xmm holds exactly one temp at a time.
  // AUTO-enable test: the dynamic allocator reclaims float memory traffic, which only moves the
  // clock when the loop is THROUGHPUT-bound. A hot loop carrying a float divide or sqrt is
  // LATENCY-bound (divsd/sqrtsd ~20 cycles): the reclaimed traffic hides under that latency and
  // costs nothing, so dynamic allocation is neutral-to-slightly-negative (n-body). We therefore
  // skip a region whose LOOP blocks contain ssaDivFloat/ssaMathSqr. Conservative: a cold divide
  // in a non-loop block does not disqualify, and a false negative only forgoes a win, never
  // regresses. Loop membership = any back edge (a successor at region index <= this block).
  function RegionThroughputBound: Boolean;
  // Is this region's time going into THROUGHPUT (register traffic worth recovering) or into the
  // LATENCY of a few long operations (where the traffic is free, hidden under the stalls)?
  //
  // The first cut answered "any div/sqrt inside a loop => latency-bound" and bailed. That was
  // tuned BEFORE the movaps fix and in-place computation, and it is now too coarse: one division
  // in a loop of forty other operations does not make the region latency-bound. On n-body AUTO
  // switched the allocator OFF and left 639 ms on the table where forcing it on gives 399.
  //
  // So count instead: a region is latency-bound only when the long-latency operations are a
  // sizable FRACTION of the loop's arithmetic. The threshold is deliberately generous - being
  // wrong here costs some of a speed-up, never correctness (the allocator is output-identical).
  const LATENCY_NUM = 1; LATENCY_DEN = 8;    // >= 1/8 of the loop's arithmetic is long-latency
  var bi2, si, i2, ri, sIdx, lo, nLat, nArith: Integer;
      B2, Sx: TSSABasicBlock; Ins2: TSSAInstruction;
      inLoop: array of Boolean;
    function RegIdxOf(B: TSSABasicBlock): Integer;
    var q: Integer;
    begin
      Result := -1;
      for q := Region.FirstBlock to Region.LastBlock do
        if SSAProg.Blocks[q] = B then Exit(q - Region.FirstBlock);
    end;
  begin
    ri := Region.LastBlock - Region.FirstBlock + 1;
    SetLength(inLoop, ri);
    for bi2 := 0 to ri - 1 do inLoop[bi2] := False;
    for bi2 := 0 to ri - 1 do
    begin
      B2 := SSAProg.Blocks[Region.FirstBlock + bi2];
      for si := 0 to B2.Successors.Count - 1 do
      begin
        Sx := TSSABasicBlock(B2.Successors[si]);
        sIdx := RegIdxOf(Sx);
        if (sIdx >= 0) and (sIdx <= bi2) then                 // back edge: [sIdx..bi2] is a loop
          for lo := sIdx to bi2 do inLoop[lo] := True;
      end;
    end;
    nLat := 0; nArith := 0;
    for bi2 := 0 to ri - 1 do
    begin
      if not inLoop[bi2] then System.Continue;
      B2 := SSAProg.Blocks[Region.FirstBlock + bi2];
      for i2 := 0 to B2.Instructions.Count - 1 do
      begin
        Ins2 := B2.Instructions[i2];
        case Ins2.OpCode of
          ssaDivFloat, ssaMathSqr,
          ssaDivInt, ssaModInt, ssaDivUInt, ssaModUInt:                // ~20-40 cycle latency
            begin Inc(nLat); Inc(nArith); end;
          // The work the register traffic competes with: everything the dynamic allocator can
          // keep in a machine register. Loads, copies, compares and array element access count -
          // they are exactly what pays for having a home.
          ssaAddFloat, ssaSubFloat, ssaMulFloat, ssaNegFloat, ssaPowFloat,
          ssaAddInt, ssaSubInt, ssaMulInt, ssaNegInt,
          ssaLoadConstInt, ssaLoadConstFloat, ssaCopyInt, ssaCopyFloat,
          ssaIntToFloat, ssaFloatToInt,
          ssaBitwiseAnd, ssaBitwiseOr, ssaBitwiseXor, ssaBitwiseNot,
          ssaCmpEqInt, ssaCmpNeInt, ssaCmpLtInt, ssaCmpGtInt, ssaCmpLeInt, ssaCmpGeInt,
          ssaCmpEqFloat, ssaCmpNeFloat, ssaCmpLtFloat, ssaCmpGtFloat, ssaCmpLeFloat, ssaCmpGeFloat,
          ssaArrayLoad, ssaArrayStore,
          ssaArrayLoadIndInt, ssaArrayLoadIndFloat,
          ssaArrayStoreIndInt, ssaArrayStoreIndFloat:
            Inc(nArith);
        end;
      end;
    end;
    // No loop arithmetic at all: nothing to recover either way, keep the historic answer (a region
    // with no long-latency op was throughput-bound before this change too).
    Result := (nArith = 0) or (nLat * LATENCY_DEN < nArith * LATENCY_NUM);
  end;

  procedure PlanDynFloat;
  var
    totpos, nbk, bi, kk, pp, r, a, x, xf, mode: Integer;
    Blk: TSSABasicBlock; Ins: TSSAInstruction;
    blkOf, firstDef, firstUse, lastTouch, cand: array of Integer;
    activeReg: array[2..15] of Integer;
    ncand: Integer; usedAny, isDef: Boolean;
    usedX: array[6..15] of Boolean;

    procedure Note(const V: TSSAValue; pos: Integer; asDef: Boolean);
    var q: Integer;
    begin
      if (V.Kind <> svkRegister) or (V.RegType <> srtFloat) then Exit;
      q := Prog.AotRemapFloatReg(V.RegIndex);
      if (q < 0) or (q > MaxFReg) then Exit;
      if blkOf[q] = -1 then blkOf[q] := bi else if blkOf[q] <> bi then blkOf[q] := -2;
      if pos > lastTouch[q] then lastTouch[q] := pos;
      if asDef then begin if pos < firstDef[q] then firstDef[q] := pos; end
      else begin if pos < firstUse[q] then firstUse[q] := pos; end;
    end;

  begin
    DynFActive := False;
    // The WHOLE map: the residency scans run to xmm15 now, and an entry left uninitialised there
    // is a bank index the next flush would happily store an untouched xmm into.
    for a := 0 to 15 do DynFCur[a] := -1;
    mode := AotDynFloatMode;
    if mode = 2 then Exit;                                     // forced off
    if LsActive then Exit;                                     // B1b ran: it SUBSUMES this pass -
                                                               // a block-local single-def temp is
                                                               // just a short web
    if MaxFReg < 0 then Exit;
    // AUTO arbitration against the REGREUSE merge. The two are ANTAGONISTIC, not additive: DYNF
    // admits only block-local single-def temps and holds an xmm for [firstDef..lastTouch] without
    // evicting, whereas a merged register is multi-def and long-lived - so it pins a machine
    // register for the whole block and starves the others. Measured, both on is always the WORST
    // of the four combinations (n-body 699 against 360 for the merge alone). The merge also beats
    // the static allocation everywhere it was measured, so when it ran, it wins. AOT_DYNF=1 still
    // forces DYNF on, which is what makes the A/B measurable on one binary.
    if (mode = 0) and SSAProg.RegisterMergeApplied then Exit;
    if (mode = 0) and not RegionThroughputBound then Exit;     // auto: skip latency-bound regions
    nbk := Region.LastBlock - Region.FirstBlock + 1;
    totpos := 0;
    for bi := 0 to nbk - 1 do Inc(totpos, SSAProg.Blocks[Region.FirstBlock + bi].Instructions.Count);
    if totpos = 0 then Exit;

    SetLength(blkOf, MaxFReg + 1); SetLength(firstDef, MaxFReg + 1);
    SetLength(firstUse, MaxFReg + 1); SetLength(lastTouch, MaxFReg + 1);
    for r := 0 to MaxFReg do begin blkOf[r] := -1; firstDef[r] := MaxInt; firstUse[r] := MaxInt; lastTouch[r] := -1; end;

    // Pass 1: linear positions and per-slot touch spans (def vs use distinguished).
    pp := 0;
    for bi := 0 to nbk - 1 do
    begin
      Blk := SSAProg.Blocks[Region.FirstBlock + bi];
      for kk := 0 to Blk.Instructions.Count - 1 do
      begin
        Ins := Blk.Instructions[kk];
        Note(Ins.Src1, pp, False); Note(Ins.Src2, pp, False); Note(Ins.Src3, pp, False);
        for a := 0 to High(Ins.PhiSources) do Note(Ins.PhiSources[a].Value, pp, False);
        isDef := not ((Ins.OpCode = ssaArrayStore) or (Ins.OpCode = ssaPrint) or (Ins.OpCode = ssaPrintLn) or
                      (Ins.OpCode = ssaXferStoreInt) or (Ins.OpCode = ssaXferStoreFloat));
        Note(Ins.Dest, pp, isDef);
        Inc(pp);
      end;
    end;

    SetLength(DynFHomeReg, totpos); SetLength(DynFHomeXmm, totpos); SetLength(DynFFree, totpos);
    for pp := 0 to totpos - 1 do begin DynFHomeReg[pp] := -1; SetLength(DynFFree[pp], 0); end;

    usedAny := False; FillChar(usedX, SizeOf(usedX), 0);
    // Pass 2: per-block greedy linear scan over admitted temps.
    for bi := 0 to nbk - 1 do
    begin
      Blk := SSAProg.Blocks[Region.FirstBlock + bi];
      ncand := 0; SetLength(cand, MaxFReg + 1);
      for r := 0 to MaxFReg do
        if (blkOf[r] = bi) and (firstDef[r] < MaxInt) and (firstUse[r] < MaxInt) and (firstDef[r] < firstUse[r]) then
        begin cand[ncand] := r; Inc(ncand); end;
      for a := 1 to ncand - 1 do                      // insertion sort by firstDef
      begin
        x := cand[a]; kk := a - 1;
        while (kk >= 0) and (firstDef[cand[kk]] > firstDef[x]) do begin cand[kk + 1] := cand[kk]; Dec(kk); end;
        cand[kk + 1] := x;
      end;
      for x := 2 to FloatPoolTop do activeReg[x] := -1;
      for a := 0 to ncand - 1 do
      begin
        r := cand[a];
        for x := 2 to FloatPoolTop do                 // expire intervals that ended before r's def
          if (activeReg[x] >= 0) and (lastTouch[activeReg[x]] < firstDef[r]) then activeReg[x] := -1;
        xf := -1;
        for x := 2 to FloatPoolTop do if activeReg[x] < 0 then begin xf := x; Break; end;
        if xf < 0 then System.Continue;               // pool full: r stays in the bank
        activeReg[xf] := r;
        DynFHomeReg[firstDef[r]] := r; DynFHomeXmm[firstDef[r]] := xf;
        SetLength(DynFFree[lastTouch[r]], Length(DynFFree[lastTouch[r]]) + 1);
        DynFFree[lastTouch[r]][High(DynFFree[lastTouch[r]])] := r;
        usedAny := True;
        if xf >= 6 then usedX[xf] := True;
      end;
    end;

    if not usedAny then Exit;                          // keep the static homes Allocate set
    DynFActive := True;
    for r := 0 to MaxFReg do FLoc[r] := -1;            // drop static float homes; go fully dynamic
    NFAlloc := 0;
    for x := 6 to 15 do SaveXmm[x] := usedX[x];
  end;

  // (c) Integer counterpart of PlanDynFloat. Identical scheme; the only difference is the pool:
  // r9..r15 MINUS the GPRs Allocate pinned to array descriptors (ACacheReg), which stay reserved
  // for the whole invocation. Scratch stays rax/rcx/rdx. Overrides the static int homes on
  // activation but leaves the array cache alone.
  procedure PlanDynInt;
  var
    totpos, nbk, bi, kk, pp, r, a, x, xf, mode, np: Integer;
    Blk: TSSABasicBlock; Ins: TSSAInstruction;
    blkOf, firstDef, firstUse, lastTouch, cand, poolG, activeG: array of Integer;
    ncand: Integer; usedAny, isDef, taken: Boolean;

    procedure Note(const V: TSSAValue; pos: Integer; asDef: Boolean);
    var q: Integer;
    begin
      if (V.Kind <> svkRegister) or (V.RegType <> srtInt) then Exit;
      q := Prog.AotRemapIntReg(V.RegIndex);
      if (q < 0) or (q > MaxIReg) then Exit;
      if blkOf[q] = -1 then blkOf[q] := bi else if blkOf[q] <> bi then blkOf[q] := -2;
      if pos > lastTouch[q] then lastTouch[q] := pos;
      if asDef then begin if pos < firstDef[q] then firstDef[q] := pos; end
      else begin if pos < firstUse[q] then firstUse[q] := pos; end;
    end;

  begin
    DynIActive := False;
    for a := 0 to 15 do DynICur[a] := -1;
    mode := AotDynFloatMode;
    if mode = 2 then Exit;
    if LsActive then Exit;                                     // subsumed by B1b, see PlanDynFloat
    if MaxIReg < 0 then Exit;
    if (mode = 0) and SSAProg.RegisterMergeApplied then Exit;  // see PlanDynFloat: antagonistic
    if (mode = 0) and not RegionThroughputBound then Exit;

    // Build the dynamic pool: IntPool GPRs not reserved by the array-descriptor cache.
    SetLength(poolG, Length(IntPool)); np := 0;
    for a := 0 to IntPoolCount(Length(IntPool) - Ord(not RsiIsPool)) - 1 do
    begin
      taken := False;
      for kk := 0 to NACache - 1 do if ACacheReg[kk] = IntPool[a] then begin taken := True; Break; end;
      if not taken then begin poolG[np] := IntPool[a]; Inc(np); end;
    end;
    if np = 0 then Exit;
    SetLength(poolG, np); SetLength(activeG, np);

    nbk := Region.LastBlock - Region.FirstBlock + 1;
    totpos := 0;
    for bi := 0 to nbk - 1 do Inc(totpos, SSAProg.Blocks[Region.FirstBlock + bi].Instructions.Count);
    if totpos = 0 then Exit;

    SetLength(blkOf, MaxIReg + 1); SetLength(firstDef, MaxIReg + 1);
    SetLength(firstUse, MaxIReg + 1); SetLength(lastTouch, MaxIReg + 1);
    for r := 0 to MaxIReg do begin blkOf[r] := -1; firstDef[r] := MaxInt; firstUse[r] := MaxInt; lastTouch[r] := -1; end;

    pp := 0;
    for bi := 0 to nbk - 1 do
    begin
      Blk := SSAProg.Blocks[Region.FirstBlock + bi];
      for kk := 0 to Blk.Instructions.Count - 1 do
      begin
        Ins := Blk.Instructions[kk];
        Note(Ins.Src1, pp, False); Note(Ins.Src2, pp, False); Note(Ins.Src3, pp, False);
        for a := 0 to High(Ins.PhiSources) do Note(Ins.PhiSources[a].Value, pp, False);
        isDef := not ((Ins.OpCode = ssaArrayStore) or (Ins.OpCode = ssaPrint) or (Ins.OpCode = ssaPrintLn) or
                      (Ins.OpCode = ssaXferStoreInt) or (Ins.OpCode = ssaXferStoreFloat));
        Note(Ins.Dest, pp, isDef);
        Inc(pp);
      end;
    end;

    SetLength(DynIHomeReg, totpos); SetLength(DynIHomeGpr, totpos); SetLength(DynIFree, totpos);
    for pp := 0 to totpos - 1 do begin DynIHomeReg[pp] := -1; SetLength(DynIFree[pp], 0); end;

    usedAny := False;
    for bi := 0 to nbk - 1 do
    begin
      Blk := SSAProg.Blocks[Region.FirstBlock + bi];
      ncand := 0; SetLength(cand, MaxIReg + 1);
      for r := 0 to MaxIReg do
        if (blkOf[r] = bi) and (firstDef[r] < MaxInt) and (firstUse[r] < MaxInt) and (firstDef[r] < firstUse[r]) then
        begin cand[ncand] := r; Inc(ncand); end;
      for a := 1 to ncand - 1 do
      begin
        x := cand[a]; kk := a - 1;
        while (kk >= 0) and (firstDef[cand[kk]] > firstDef[x]) do begin cand[kk + 1] := cand[kk]; Dec(kk); end;
        cand[kk + 1] := x;
      end;
      for x := 0 to np - 1 do activeG[x] := -1;
      for a := 0 to ncand - 1 do
      begin
        r := cand[a];
        for x := 0 to np - 1 do
          if (activeG[x] >= 0) and (lastTouch[activeG[x]] < firstDef[r]) then activeG[x] := -1;
        xf := -1;
        for x := 0 to np - 1 do if activeG[x] < 0 then begin xf := x; Break; end;
        if xf < 0 then System.Continue;
        activeG[xf] := r;
        DynIHomeReg[firstDef[r]] := r; DynIHomeGpr[firstDef[r]] := poolG[xf];
        SetLength(DynIFree[lastTouch[r]], Length(DynIFree[lastTouch[r]]) + 1);
        DynIFree[lastTouch[r]][High(DynIFree[lastTouch[r]])] := r;
        usedAny := True;
        if GprIsCalleeSaved(poolG[xf]) and (poolG[xf] <> RSI) then SaveGpr[poolG[xf]] := True;
      end;
    end;

    if not usedAny then Exit;
    DynIActive := True;
    for r := 0 to MaxIReg do ILoc[r] := -1;            // drop static int homes; array cache stays
    NIAlloc := 0;
  end;

  { --- B1b: the interval allocator ------------------------------------------------------------
    A linear scan over the WEBS BuildIntervals produced, instead of over register numbers. What it
    buys, and why the two mechanisms it replaces could not:

      * Allocate gives a VM register ONE home for the whole region, ranked by use count. A register
        the REGREUSE merge loaded with five different values therefore holds a machine register for
        the union of their lifetimes, and the four it starves are the reason the merge costs
        intpoly 12%.
      * PlanDynFloat/PlanDynInt only admit a temp that is BLOCK-LOCAL and SINGLE-DEF, which a
        merged register never is - so with the merge on they admit nothing, and the two are
        arbitrated against each other rather than composed.

    A web is neither: it is one value, with a real lifetime, and it can start and end anywhere. So
    the same six xmm can carry a hundred and forty-nine successive float values instead of six.

    First cut deliberately: a web is placed WHOLE, or not at all. Splitting a web when the pool
    overflows (and resolving the resulting location changes on CFG edges with moves) is the next
    step, and it is the one that needs the trampolines - as long as a value keeps ONE home for its
    whole life, its location is the same at both ends of every edge and no resolution is needed.

    The safety invariant, which is what makes this bearable at all: THE BANK IS AUTHORITATIVE
    whenever a value is not resident. A web that writes is written back when it releases its home,
    so any read the analysis never saw - and this codebase has a whole class of those, operands
    that lower to register 0, opcodes that touch registers they do not name - finds the right value
    in the bank, exactly as it does today. Residency only ever caches. ------------------------- }
  procedure PlanLinScan;
  var
    mode, i, j, k, w, x, r, np, bank, nplaced, nspill: Integer;
    poolG: array of Integer;              // int: IntPool minus the array-descriptor cache
    poolN: Integer;
    activeW: array of Integer;            // pool slot -> web id resident there (-1 = free)
    ordw: array of Integer;               // web ids, sorted by start position
    WebMap: array of Integer;             // old web id -> canonical web id after the overlap merge
    taken: Boolean;
    minSlot: Integer;
    minW: Int64;

    procedure PushAt(var Slot: TLsEventList; pos, wid: Integer);
    begin
      SetLength(Slot[pos], Length(Slot[pos]) + 1);
      Slot[pos][High(Slot[pos])] := wid;
    end;

    function CrossesCall(wid: Integer): Boolean;
    // DIAGNOSTIC PROBE (LS_NOCALL=1): refuse any web whose life spans a position that hands
    // control away - a runtime helper or a native call-sub.
    var b2, lo, q: Integer; B3: TSSABasicBlock;
    begin
      Result := False;
      for b2 := 0 to LiveNB - 1 do
      begin
        B3 := TSSABasicBlock(SSAProg.Blocks[Region.FirstBlock + b2]);
        lo := LsPos0[b2];
        for q := 0 to B3.Instructions.Count - 1 do
          if (lo + q >= LsWebs[wid].PStart) and (lo + q <= LsWebs[wid].PEnd) then
            if (TSSAInstruction(B3.Instructions[q]).OpCode = ssaCallSub) or
               (not AotIsNative(SSAProg, TSSAInstruction(B3.Instructions[q]))) then
              Exit(True);
      end;
    end;

    function BlockWeightAt(pos: Integer): Int64;
    var b2: Integer;
    begin
      Result := 1;
      for b2 := LiveNB - 1 downto 0 do
        if pos >= LsPos0[b2] then Exit(BlockW[b2]);
    end;

    function InstrAt(pos: Integer): TSSAInstruction;
    var b2, lo: Integer; B3: TSSABasicBlock;
    begin
      Result := nil;
      for b2 := 0 to LiveNB - 1 do
      begin
        B3 := TSSABasicBlock(SSAProg.Blocks[Region.FirstBlock + b2]);
        lo := LsPos0[b2];
        if (pos >= lo) and (pos < lo + B3.Instructions.Count) then
          Exit(TSSAInstruction(B3.Instructions[pos - lo]));
      end;
    end;

    function EndsOnTerminator(pos: Integer): Boolean;
    // The write-back of a web whose last position is a jump has to be emitted BEFORE it, or it
    // lands after the branch and never runs. Safe: a store does not disturb the register the
    // terminator is about to read, and a terminator never defines one.
    var Ins2: TSSAInstruction;
    begin
      // A chain, not a set: TSSAOpCode has well past the 256 elements a Pascal set holds.
      Ins2 := InstrAt(pos);
      Result := (Ins2 <> nil) and
                ((Ins2.OpCode = ssaJump) or (Ins2.OpCode = ssaJumpIfZero) or
                 (Ins2.OpCode = ssaJumpIfNotZero) or (Ins2.OpCode = ssaReturn) or
                 (Ins2.OpCode = ssaReturnSub) or (Ins2.OpCode = ssaEnd) or (Ins2.OpCode = ssaStop));
    end;

    function StartsOnHelper(wid: Integer): Boolean;
    // Does this web's first position belong to an instruction that hands the work to a runtime
    // helper (or to a native call-sub)?
    //
    // It matters because a helper does its work THROUGH THE BANKS: the emitted call flushes the
    // resident registers first and re-reads them after. The take event happens BEFORE the
    // instruction, so a web starting there would have its machine register flushed while it still
    // holds nothing - writing garbage over the bank slot the helper is about to read or write.
    // The cure is not to refuse the web but to LOAD at the take: the flush then writes back what
    // it read, and the reload afterwards picks up whatever the helper produced.
    var Ins2: TSSAInstruction;
    begin
      Ins2 := InstrAt(LsWebs[wid].PStart);
      Result := (Ins2 <> nil) and ((Ins2.OpCode = ssaCallSub) or not AotIsNative(SSAProg, Ins2));
    end;

    function WebPlaceable(wid: Integer): Boolean;
    // May this web hold a machine register for its whole life?
    //
    // Only if every path that reaches a point where the value is live has passed through the web -
    // otherwise the register holds an unrelated value where the program expects the bank's (which
    // for a BASIC variable never assigned on that path is its implicit zero, an OBSERVABLE value:
    // "Dim n : Print n" prints 0).
    //
    // One uncovered entry is allowed, and only if it is the web's own start: there a load makes
    // the register agree with the bank before anything reads it. That single case is the common
    // and valuable one - the region's entry block, whose live-ins arrive from outside through the
    // banks. Anything else stays memory-homed, exactly as it is today.
    begin
      Result := (LsWebs[wid].NUncov = 0) or
                ((LsWebs[wid].NUncov = 1) and (LsWebs[wid].UncovPos = LsWebs[wid].PStart));
      if Result and (GetEnvironmentVariable('LS_NOCALL') = '1') and CrossesCall(wid) then
        Result := False;
      if Result and (GetEnvironmentVariable('LS_ONLYW') <> '') and
         (GetEnvironmentVariable('LS_ONLYW') <> IntToStr(wid)) then
        Result := False;
      if Result then
        LsWebs[wid].NeedsLoad := (LsWebs[wid].NUncov = 1) or StartsOnHelper(wid);
    end;

  begin
    LsActive := False;
    mode := AotLinScanMode;
    if mode = 2 then Exit;                                  // forced off: byte-identical baseline
    // DIAGNOSTIC: restrict the allocator to one region, to bisect a failure to its compiland.
    if (GetEnvironmentVariable('LS_REGION') <> '') and
       (GetEnvironmentVariable('LS_REGION') <> Region.Name) then Exit;
    if not LsOK then Exit;
    if LsNWeb = 0 then Exit;
    // Sorting and the per-register merge are quadratic in the worst case; a region with this many
    // webs is not one where a register allocator decides the clock.
    if LsNWeb > 20000 then Exit;

    // (1) Two webs of the SAME VM register whose SPANS overlap must not get different homes: the
    // emitter keeps one FLoc/ILoc entry per register, so at any position a register can only be in
    // one place. It happens when one web has a hole another web's life falls into. Merge them and
    // let the pair share a home - a small loss of precision for an invariant worth having.
    SetLength(WebMap, LsNWeb);
    for i := 0 to LsNWeb - 1 do WebMap[i] := i;
    for i := 0 to LsNWeb - 1 do
    begin
      if WebMap[i] <> i then System.Continue;
      for j := i + 1 to LsNWeb - 1 do
      begin
        if WebMap[j] <> j then System.Continue;
        if (LsWebs[j].Bank <> LsWebs[i].Bank) or (LsWebs[j].Reg <> LsWebs[i].Reg) then System.Continue;
        if (LsWebs[j].PStart > LsWebs[i].PEnd) or (LsWebs[i].PStart > LsWebs[j].PEnd) then System.Continue;
        WebMap[j] := i;
        if LsWebs[j].PStart < LsWebs[i].PStart then LsWebs[i].PStart := LsWebs[j].PStart;
        if LsWebs[j].PEnd > LsWebs[i].PEnd then LsWebs[i].PEnd := LsWebs[j].PEnd;
        LsWebs[i].Weight := LsWebs[i].Weight + LsWebs[j].Weight;
        LsWebs[i].NeedsLoad := LsWebs[i].NeedsLoad or LsWebs[j].NeedsLoad;
        LsWebs[i].HasDef := LsWebs[i].HasDef or LsWebs[j].HasDef;
        LsWebs[j].Home := -2;                               // absorbed: never placed on its own
      end;
    end;
    // The merge above can widen a span across another already-absorbed one; one more sweep settles
    // the common case, and anything it misses is caught by the overlap check in the scan itself.
    for i := 0 to LsNWeb - 1 do
      if WebMap[i] <> i then
        while WebMap[WebMap[i]] <> WebMap[i] do WebMap[i] := WebMap[WebMap[i]];

    // (2) The pools. Float is xmm2..7 as always; int is IntPool minus the GPRs Allocate pinned to
    // array descriptors, which stay reserved for the whole invocation.
    SetLength(poolG, Length(IntPool)); poolN := 0;
    for i := 0 to IntPoolCount(Length(IntPool) - Ord(not RsiIsPool)) - 1 do
    begin
      taken := False;
      for j := 0 to NACache - 1 do if ACacheReg[j] = IntPool[i] then begin taken := True; Break; end;
      if not taken then begin poolG[poolN] := IntPool[i]; Inc(poolN); end;
    end;

    // (3) The scan, per bank: webs by start position, expire what ended strictly BEFORE this one
    // starts (a web releasing at position p must not hand its register to one starting at p - the
    // release is emitted after the instruction, the take before it), then first free slot, and on
    // overflow evict the lightest active if this web is heavier.
    SetLength(LsTakeAt, 0); SetLength(LsFreeAt, 0);
    SetLength(LsTakeAt, LsNPos); SetLength(LsFreeAt, LsNPos);
    nplaced := 0; nspill := 0;
    AotDiagLsPlacedInt := 0; AotDiagLsPlacedFloat := 0;
    AotDiagLsSpilledInt := 0; AotDiagLsSpilledFloat := 0;
    AotDiagLsLoads := 0; AotDiagLsStores := 0;
    AotDiagLsLoadW := 0; AotDiagLsStoreW := 0;

    for bank := 0 to 1 do
    begin
      if bank = 0 then np := poolN else np := FloatPoolTop - 1;   // xmm2..FloatPoolTop
      if np <= 0 then System.Continue;

      SetLength(ordw, 0); SetLength(ordw, LsNWeb);
      k := 0;
      for i := 0 to LsNWeb - 1 do
        if (LsWebs[i].Bank = bank) and (WebMap[i] = i) and WebPlaceable(i) then
        begin ordw[k] := i; Inc(k); end;
      if k = 0 then System.Continue;
      for i := 1 to k - 1 do                                // insertion sort by start position
      begin
        x := ordw[i]; j := i - 1;
        while (j >= 0) and (LsWebs[ordw[j]].PStart > LsWebs[x].PStart) do
        begin ordw[j + 1] := ordw[j]; Dec(j); end;
        ordw[j + 1] := x;
      end;

      SetLength(activeW, 0); SetLength(activeW, np);
      for i := 0 to np - 1 do activeW[i] := -1;

      for i := 0 to k - 1 do
      begin
        w := ordw[i];
        for x := 0 to np - 1 do
          if (activeW[x] >= 0) and (LsWebs[activeW[x]].PEnd < LsWebs[w].PStart) then activeW[x] := -1;
        r := -1;
        for x := 0 to np - 1 do if activeW[x] < 0 then begin r := x; Break; end;
        if r < 0 then
        begin
          minSlot := 0; minW := -1;
          for x := 0 to np - 1 do
            if (minW < 0) or (LsWebs[activeW[x]].Weight < minW) then
            begin minSlot := x; minW := LsWebs[activeW[x]].Weight; end;
          if LsWebs[w].Weight > minW then
          begin
            LsWebs[activeW[minSlot]].Home := -1;            // evicted: stays memory-homed
            Inc(nspill);
            Dec(nplaced);
            if bank = 0 then
            begin Inc(AotDiagLsSpilledInt); Dec(AotDiagLsPlacedInt); end
            else
            begin Inc(AotDiagLsSpilledFloat); Dec(AotDiagLsPlacedFloat); end;
            r := minSlot;
          end
          else
          begin
            LsWebs[w].Home := -1;
            Inc(nspill);
            if bank = 0 then Inc(AotDiagLsSpilledInt) else Inc(AotDiagLsSpilledFloat);
            System.Continue;
          end;
        end;
        activeW[r] := w;
        if bank = 0 then LsWebs[w].Home := poolG[r] else LsWebs[w].Home := r + 2;
        Inc(nplaced);
        if bank = 0 then Inc(AotDiagLsPlacedInt) else Inc(AotDiagLsPlacedFloat);
      end;
    end;

    if nplaced <= 0 then Exit;                              // nothing to gain: keep the static homes

    // (4) Turn the placements into position events. An evicted web has Home = -1 and contributes
    // none, so its register simply stays in the bank exactly as it does today.
    for i := 0 to LsNWeb - 1 do
      if (WebMap[i] = i) and (LsWebs[i].Home >= 0) then
      begin
        // LS_NOWB=1: UNSOUND PROBE, and the only way to price the work that would make it sound.
        // ⚠️ Being unsound is not theoretical - on intpoly it MISCOMPILES, so that benchmark's
        // "ceiling" is not a measurement at all. Check the output of every cell before reading a
        // number out of this flag; the first table taken with it did not, and reported a 15% win
        // on a program that was computing something else.
        // It drops every write-back, which is what a perfect oracle for "this value is a pure
        // temporary, dead for good after its last use" would let the allocator do. What it cannot
        // tell apart is the value that OUTLIVES the region - and that distinction cannot be
        // invented here: the AOT's liveness treats an out-of-region successor as "nothing is
        // live", so it writes everything back out of prudence. The information has to come from
        // the SSA. This flag measures what buying it would be worth.
        if GetEnvironmentVariable('LS_NOWB') = '1' then LsWebs[i].HasDef := False;
        LsWebs[i].StoreEarly := LsWebs[i].HasDef and EndsOnTerminator(LsWebs[i].PEnd);
        if GetEnvironmentVariable('LS_DUMP') = '1' then
          WriteLn(ErrOutput, Format('[LS] web %d bank=%d reg=%d [%d..%d] home=%d load=%s def=%s w=%d uncov=%d@%d nr=%d',
            [i, LsWebs[i].Bank, LsWebs[i].Reg, LsWebs[i].PStart, LsWebs[i].PEnd, LsWebs[i].Home,
             BoolToStr(LsWebs[i].NeedsLoad, 'y', 'n'), BoolToStr(LsWebs[i].HasDef, 'y', 'n'),
             LsWebs[i].Weight, LsWebs[i].NUncov, LsWebs[i].UncovPos, LsWebs[i].NRange]));
        if LsWebs[i].NeedsLoad then
        begin
          Inc(AotDiagLsLoads);
          AotDiagLsLoadW := AotDiagLsLoadW + BlockWeightAt(LsWebs[i].PStart);
        end;
        if LsWebs[i].HasDef then
        begin
          Inc(AotDiagLsStores);
          AotDiagLsStoreW := AotDiagLsStoreW + BlockWeightAt(LsWebs[i].PEnd);
        end;
        PushAt(LsTakeAt, LsWebs[i].PStart, i);
        PushAt(LsFreeAt, LsWebs[i].PEnd, i);
        if LsWebs[i].Bank = 1 then
        begin
          if LsWebs[i].Home >= 6 then SaveXmm[LsWebs[i].Home] := True;
        end
        else if GprIsCalleeSaved(LsWebs[i].Home) and (LsWebs[i].Home <> RSI) then
          SaveGpr[LsWebs[i].Home] := True;
      end;

    LsActive := True;
    // The interval schedule REPLACES the static homes: everything it does not place stays in the
    // bank, and nothing is loaded on entry or flushed on exit for a register that has no home for
    // the whole region. Same handover PlanDynFloat makes, for the same reason.
    for i := 0 to MaxFReg do FLoc[i] := -1;
    for i := 0 to MaxIReg do ILoc[i] := -1;
    NFAlloc := 0; NIAlloc := 0;
  end;

  procedure EmitInstruction;
  var d, w: Integer;
      apc: Integer;
      p1: Integer;
      bits: Int64;
      Hd, Hs, s1v: Integer;   // in-place unary/conversion: dest home, src home, src VM reg
      rexb: Byte;
  begin
    // C4: anything the native path does not cover becomes ONE runtime-helper call. Deciding
    // it here, before the case, keeps a single entry point for the fallback - so an op with a
    // native form that its operands do not fit (a string-element array, a computed array ref)
    // takes the same road as an op with no native form at all. Prescan agreed already, using
    // the same two predicates; the Fail is defence, not a path.
    if not AotIsNative(SSAProg, Cur) then
    begin
      if not AotHelperRoutable(Prog, CurOrd) then
      begin Fail('helper:' + OpName(Cur.OpCode)); Exit; end;
      apc := NeedPC; if not OK then Exit;
      EmitHelperCall(apc);
      Exit;
    end;
    case Cur.OpCode of
      ssaLabel, ssaNop: ;

      // Record-scope marks: skipped in a deopt-free region (push and pop elide together,
      // reclamation deferred to FramePop), routed through the helper when a deopt could
      // strand the interpreter against an unbalanced mark stack. Same HasDeopt the prescan
      // saw - the two must agree.
      ssaRecMarkPush, ssaRecMarkPop:
        // C6 first: a leaf call, decided by the prescan (RecMarkNative). Otherwise the historical
        // two-way choice - helper when a deopt could strand the interpreter against an unbalanced
        // mark stack, elided when the whole invocation is native.
        if RecMarkNative then
          EmitRecMarkNative(Cur.OpCode = ssaRecMarkPush)
        else if HasDeopt then
        begin
          apc := NeedPC; if not OK then Exit;
          EmitHelperCall(apc);
        end;

      ssaRecordNew:
      begin
        apc := NeedPC; if not OK then Exit;   // the slot counts live in the bytecode instruction
        EmitRecordNew(apc);
      end;
      ssaRecordFree: EmitRecordFree;


      ssaLoadConstInt:
      begin
        if Cur.Src1.Kind <> svkConstInt then begin Fail('const-shape'); Exit; end;
        d := IReg(Cur.Dest); if not OK then Exit;
        if IAlloc(d) >= 0 then MovImm64(IAlloc(d), Cur.Src1.ConstInt)
        else begin MovImm64(RAX, Cur.Src1.ConstInt); IStore(d, RAX); end;
      end;
      ssaLoadConstFloat:
      begin
        if Cur.Src1.Kind <> svkConstFloat then begin Fail('const-shape'); Exit; end;
        bits := PInt64(@Cur.Src1.ConstFloat)^;
        MovImm64(RAX, bits);
        E.EmitBytes([$66, $48, $0F, $6E, $C0]);        // movq xmm0, rax
        FStore(FReg(Cur.Dest), XMM0);
      end;

      // A copy whose ends are both machine-allocated is ONE reg-reg move (the JIT's bcCopyInt
      // fast path): the RAX/XMM0 staging costs a second move per copy, and the int one is the
      // PHI-elimination copy of every FOR counter - one extra instruction per loop iteration,
      // the byte-proven cause of the AOT-vs-JIT gap on the pure-int microbench.
      ssaCopyInt:
      begin
        d := IReg(Cur.Dest); p1 := IReg(Cur.Src1); if not OK then Exit;
        if (IAlloc(d) >= 0) and (IAlloc(p1) >= 0) then
          MovRR(IAlloc(d), IAlloc(p1))
        else
        begin ILoad(RAX, p1); IStore(d, RAX); end;
      end;
      ssaCopyFloat:
      begin
        d := FReg(Cur.Dest); p1 := FReg(Cur.Src1); if not OK then Exit;
        if (FAlloc(d) >= 0) and (FAlloc(p1) >= 0) then
        begin
          if FAlloc(d) <> FAlloc(p1) then                       // movaps xmm_d, xmm_s (move-eliminated; see FLoad)
            SseRR([$0F, $28], FAlloc(d), FAlloc(p1));
        end
        else
        begin FLoad(XMM0, p1); FStore(d, XMM0); end;
      end;

      ssaAddInt: IntBin([$48, $03], True);
      ssaSubInt: IntBin([$48, $2B], False);
      ssaMulInt: IntBin([$48, $0F, $AF], True);
      ssaNegInt:
      begin
        d := IReg(Cur.Dest); s1v := IReg(Cur.Src1); if not OK then Exit;
        Hd := IAlloc(d);
        if Hd >= 0 then
        begin
          if IAlloc(s1v) <> Hd then ILoadArg(Hd, s1v);              // Hd <- src1 (skip if already there)
          rexb := $48; if Hd >= 8 then rexb := rexb or $01;         // REX.B
          E.Emit8(rexb); E.Emit8($F7); E.Emit8($D8 or (Hd and 7));  // neg Hd (in place)
        end
        else
        begin ILoad(RAX, s1v); E.EmitBytes([$48, $F7, $D8]); IStore(d, RAX); end;
      end;
      ssaDivInt:  begin apc := NeedPC; if OK then DivModSigned(apc, False); end;
      ssaModInt:  begin apc := NeedPC; if OK then DivModSigned(apc, True); end;
      ssaDivUInt: begin apc := NeedPC; if OK then DivModUnsigned(apc, False); end;
      ssaModUInt: begin apc := NeedPC; if OK then DivModUnsigned(apc, True); end;

      ssaAddFloat: FloatBin([$F2, $0F, $58], True);
      ssaSubFloat: FloatBin([$F2, $0F, $5C], False);
      ssaMulFloat: FloatBin([$F2, $0F, $59], True);
      ssaDivFloat:
      begin
        if Modern then FloatBin([$F2, $0F, $5E], False)
        else
        begin
          // CLASSIC raises on a zero divisor: catch +-0.0 (bits shifted left of the sign
          // are 0) and deopt so the interpreter reproduces the raise. NaN goes native.
          apc := NeedPC; if not OK then Exit;
          FLoad(XMM0, FReg(Cur.Src1));
          FLoad(XMM1, FReg(Cur.Src2));
          E.EmitBytes([$66, $48, $0F, $7E, $C8]);      // movq rax, xmm1
          E.EmitBytes([$48, $D1, $E0]);                // shl rax,1 (drop sign)
          E.EmitBytes([$75, $00]); p1 := E.Len - 1;    // jnz +ok
          ExitTo(apc);
          E.PatchByte(p1, Byte(E.Len - (p1 + 1)));
          E.EmitBytes([$F2, $0F, $5E, $C1]);           // divsd xmm0, xmm1
          FStore(FReg(Cur.Dest), XMM0);
        end;
      end;
      ssaNegFloat:
      begin
        d := FReg(Cur.Dest); s1v := FReg(Cur.Src1); if not OK then Exit;
        Hd := FAlloc(d);
        MovImm64(RAX, Int64($8000000000000000));
        E.EmitBytes([$66, $48, $0F, $6E, $C8]);        // movq xmm1, rax (sign mask)
        if Hd >= 0 then
        begin
          FLoad(Hd, s1v);                              // Hd <- src1 (movaps; skipped if Hd==src1 home)
          SseRR([$66, $0F, $57], Hd, 1);                          // xorpd Hd, xmm1 (in place)
        end
        else
        begin
          FLoad(XMM0, s1v);
          E.EmitBytes([$66, $0F, $57, $C1]);           // xorpd xmm0, xmm1
          FStore(d, XMM0);
        end;
      end;
      ssaMathSqr:
      begin
        d := FReg(Cur.Dest); s1v := FReg(Cur.Src1); if not OK then Exit;
        Hd := FAlloc(d);
        if not Modern then
        begin
          // CLASSIC raises on Sqr(neg): sign bit set (incl. -0.0, where the interpreter
          // is also the safe path) -> deopt. Needs src1 in xmm0 for the check, so scratch path.
          FLoad(XMM0, s1v);
          apc := NeedPC; if not OK then Exit;
          E.EmitBytes([$66, $48, $0F, $7E, $C0]);      // movq rax, xmm0
          E.EmitBytes([$48, $85, $C0]);                // test rax, rax
          E.EmitBytes([$79, $00]); p1 := E.Len - 1;    // jns +ok
          ExitTo(apc);
          E.PatchByte(p1, Byte(E.Len - (p1 + 1)));
          E.EmitBytes([$F2, $0F, $51, $C0]);           // sqrtsd xmm0, xmm0
          FStore(d, XMM0);
        end
        else if Hd >= 0 then
          FOp([$F2, $0F, $51], Hd, s1v)                // sqrtsd Hd, <src1>  (2-operand, in place)
        else
        begin FLoad(XMM0, s1v); E.EmitBytes([$F2, $0F, $51, $C0]); FStore(d, XMM0); end;
      end;

      ssaIntToFloat:
      begin
        d := FReg(Cur.Dest); if not OK then Exit;
        Hd := FAlloc(d);
        ILoad(RAX, IReg(Cur.Src1));
        if Hd >= 0 then
          SseWRex([$0F, $2A], Hd, RAX)                           // cvtsi2sd Hd, rax (in place)
        else
        begin E.EmitBytes([$F2, $48, $0F, $2A, $C0]); FStore(d, XMM0); end;
      end;
      ssaFloatToInt:
      begin
        if Modern then CvtFloatToInt([$0F, $2D])       // cvtsd2si (round-to-even)
        else CvtFloatToInt([$0F, $2C]);                // cvttsd2si (truncate)
      end;
      ssaFloatRound: CvtFloatToInt([$0F, $2D]);        // CINT: round-to-even
      ssaNarrowSingle:
      begin
        FLoad(XMM0, FReg(Cur.Src1));
        E.EmitBytes([$F2, $0F, $5A, $C0]);             // cvtsd2ss xmm0, xmm0
        E.EmitBytes([$F3, $0F, $5A, $C0]);             // cvtss2sd xmm0, xmm0
        FStore(FReg(Cur.Dest), XMM0);
      end;
      ssaNarrowInt:
      begin
        w := CInt(Cur.Src3); if not OK then Exit;
        ILoad(RAX, IReg(Cur.Src1));
        case w of
          1: E.EmitBytes([$48, $0F, $BE, $C0]);        // movsx rax, al
          2: E.EmitBytes([$0F, $B6, $C0]);             // movzx eax, al
          3: E.EmitBytes([$48, $0F, $BF, $C0]);        // movsx rax, ax
          4: E.EmitBytes([$0F, $B7, $C0]);             // movzx eax, ax
          5: E.EmitBytes([$48, $63, $C0]);             // movsxd rax, eax
          6: E.EmitBytes([$89, $C0]);                  // mov eax, eax (zero upper)
        end;                                           // else: full width, no-op
        IStore(IReg(Cur.Dest), RAX);
      end;

      ssaBitwiseAnd: begin ILoad(RAX, IReg(Cur.Src1)); IOp([$48, $23], RAX, IReg(Cur.Src2)); IStore(IReg(Cur.Dest), RAX); end;
      ssaBitwiseOr:  begin ILoad(RAX, IReg(Cur.Src1)); IOp([$48, $0B], RAX, IReg(Cur.Src2)); IStore(IReg(Cur.Dest), RAX); end;
      ssaBitwiseXor: begin ILoad(RAX, IReg(Cur.Src1)); IOp([$48, $33], RAX, IReg(Cur.Src2)); IStore(IReg(Cur.Dest), RAX); end;
      ssaBitwiseNot: begin ILoad(RAX, IReg(Cur.Src1)); E.EmitBytes([$48, $F7, $D0]); IStore(IReg(Cur.Dest), RAX); end;
      ssaShl:
      begin
        // FPC shl on x86-64 = hardware shl (count masked mod 64): native matches exactly.
        ILoad(RAX, IReg(Cur.Src1)); ILoad(RCX, IReg(Cur.Src2));
        E.EmitBytes([$48, $D3, $E0]);                  // shl rax, cl
        IStore(IReg(Cur.Dest), RAX);
      end;
      ssaShr: ShrSat({Arith=} Modern);   // MODERN arithmetic, CLASSIC logical (both saturating)
      ssaShrUInt: ShrSat(False);

      ssaCmpEqInt: IntCmp($94); ssaCmpNeInt: IntCmp($95);
      ssaCmpLtInt: IntCmp($9C); ssaCmpGtInt: IntCmp($9F);
      ssaCmpLeInt: IntCmp($9E); ssaCmpGeInt: IntCmp($9D);
      ssaCmpLtUInt: IntCmp($92); ssaCmpGtUInt: IntCmp($97);
      ssaCmpLeUInt: IntCmp($96); ssaCmpGeUInt: IntCmp($93);
      ssaCmpLtFloat: FloatCmp(0); ssaCmpLeFloat: FloatCmp(1);
      ssaCmpGtFloat: FloatCmp(2); ssaCmpGeFloat: FloatCmp(3);
      ssaCmpEqFloat: FloatCmp(4); ssaCmpNeFloat: FloatCmp(5);

      ssaCmpEqString: EmitStrCmp(0); ssaCmpNeString: EmitStrCmp(1);
      ssaCmpLtString: EmitStrCmp(2); ssaCmpGtString: EmitStrCmp(3);

      ssaCopyString: EmitStrCopy;
      ssaLoadConstString: begin apc := NeedPC; if OK then EmitStrLoadConst(apc); end;
      ssaStrConcat: EmitStrConcat;
      ssaStrLen: EmitStrLen;
      ssaStrLeft:  EmitStrSlice(AOTCTX_STRLEFT);
      ssaStrRight: EmitStrSlice(AOTCTX_STRRIGHT);
      ssaStrMid:   EmitStrMid;
      ssaStrAscMid: EmitStrAscMid;
      ssaStrConcatCharAt: EmitStrConcatCharAt;
      ssaStrAppendMapped: EmitStrAppendMapped;
      ssaStrAsc:   EmitStrAsc;
      ssaStrChr:   EmitStrChr;
      ssaIntToString: EmitIntToStr;
      ssaStrVal:      EmitStrVal;
      ssaStrValInt:   EmitStrValInt;
      ssaStrInstr: EmitStrInstr;

      ssaJump, ssaJumpIfZero, ssaJumpIfNotZero:
      begin
        // Resolve the target through the region's OWN label->index map (built from the
        // actual list positions): TSSABasicBlock.BlockIndex is stamped at SSA construction
        // and goes stale when later passes (LICM pre-headers) insert blocks mid-list.
        if Cur.Dest.Kind <> svkLabel then begin Fail('jump-shape'); Exit; end;
        d := LabelIdx.IndexOf(Cur.Dest.LabelName);
        if d < 0 then begin Fail('jump-target'); Exit; end;
        d := PtrInt(LabelIdx.Objects[d]);
        // An unconditional jump to the NEXT block in emission order is a fall-through:
        // emitting it produced a taken "jmp +0" (byte-proven: 16 executed per n-body driver
        // step at the intermediate nest levels). Elide it - only when it is the block's last
        // instruction, so nothing after it in this block could be skipped. Every other jump
        // still resolves through the same end-of-emission fixups, so no target moves.
        if (Cur.OpCode = ssaJump) and (d = CurBlkIdx + 1) and CurIsBlockLast then
          { fall through }
        else if Cur.OpCode = ssaJump then JmpRel(d)
        else
        begin
          ILoad(RAX, IReg(Cur.Src1)); if not OK then Exit;
          E.EmitBytes([$48, $85, $C0]);                // test rax, rax
          if Cur.OpCode = ssaJumpIfZero then JccRel($84, d)
          else JccRel($85, d);
        end;
      end;

      ssaXferLoadInt:
      begin
        w := CInt(Cur.Src3); if not OK then Exit;
        LoadXferBase(False);
        E.MemOp([$48, $8B], RAX, RDX, LongWord(w) * 8);   // mov rax,[rdx+slot*8]
        IStore(IReg(Cur.Dest), RAX);
      end;
      ssaXferLoadFloat:
      begin
        w := CInt(Cur.Src3); if not OK then Exit;
        LoadXferBase(True);
        SseMem([$F2, $0F, $10], XMM0, RDX, LongWord(w) * 8);
        FStore(FReg(Cur.Dest), XMM0);
      end;
      ssaXferStoreInt:
      begin
        w := CInt(Cur.Src3); if not OK then Exit;
        LoadXferBase(False);
        ILoad(RAX, IReg(Cur.Src1));
        E.MemOp([$48, $89], RAX, RDX, LongWord(w) * 8);   // mov [rdx+slot*8],rax
      end;
      ssaXferStoreFloat:
      begin
        w := CInt(Cur.Src3); if not OK then Exit;
        LoadXferBase(True);
        FLoad(XMM0, FReg(Cur.Src1));
        SseMem([$F2, $0F, $11], XMM0, RDX, LongWord(w) * 8);
      end;

      // B3: native call site. The callee entry PC is the resolved label sitting in the
      // bcCallSub instruction's Immediate (the jump-fixup pass filled it in).
      ssaCallSub:
      begin
        if Cur.Dest.Kind <> svkLabel then Fail('callsub-shape')
        else
        begin
          apc := NeedPC; if not OK then Exit;
          d := Integer(Prog.GetInstruction(apc).Immediate);
          if (d < 0) or (d >= Prog.GetInstructionCount) then Fail('callsub-target')
          else EmitCallSubNative(apc, d);
        end;
      end;

      ssaArrayLoad:
      begin
        d := ArrId; if not OK then Exit;
        apc := -1;
        // B4: a proven-safe access needs no deopt PC even under CLASSIC (the guard is elided).
        if ArrClassic and not Cur.BoundsSafe then begin apc := NeedPC; if not OK then Exit; end;
        if SSAProg.GetArray(d).ElementType = srtString then
          EmitArrLoadStr(d, IReg(Cur.Src2), SReg(Cur.Dest))
        else if SSAProg.GetArray(d).ElementType = srtFloat then
          AotArrAccess(True, False, d, IReg(Cur.Src2), FReg(Cur.Dest), apc, Cur.BoundsSafe)
        else
          AotArrAccess(False, False, d, IReg(Cur.Src2), IReg(Cur.Dest), apc, Cur.BoundsSafe);
      end;
      ssaArrayStore:
      begin
        d := ArrId; if not OK then Exit;
        apc := -1;
        if ArrClassic and not Cur.BoundsSafe then begin apc := NeedPC; if not OK then Exit; end;
        if SSAProg.GetArray(d).ElementType = srtString then
          EmitArrStoreStr(d, IReg(Cur.Src2), SReg(Cur.Dest))
        else if SSAProg.GetArray(d).ElementType = srtFloat then
          AotArrAccess(True, True, d, IReg(Cur.Src2), FReg(Cur.Dest), apc, Cur.BoundsSafe)
        else
          AotArrAccess(False, True, d, IReg(Cur.Src2), IReg(Cur.Dest), apc, Cur.BoundsSafe);
      end;
      ssaRecordLoadInt, ssaRecordLoadFloat:
      begin
        apc := NeedPC; if not OK then Exit;          // a shared-region handle leaves here
        if Cur.OpCode = ssaRecordLoadFloat then
          AotRecAccess(apc, IReg(Cur.Src1), Cur.Src3.ConstInt, FReg(Cur.Dest), True, False)
        else
          AotRecAccess(apc, IReg(Cur.Src1), Cur.Src3.ConstInt, IReg(Cur.Dest), False, False);
      end;
      ssaRecordStoreInt, ssaRecordStoreFloat:
      begin
        apc := NeedPC; if not OK then Exit;
        if Cur.OpCode = ssaRecordStoreFloat then
          AotRecAccess(apc, IReg(Cur.Src1), Cur.Src3.ConstInt, FReg(Cur.Src2), True, True)
        else
          AotRecAccess(apc, IReg(Cur.Src1), Cur.Src3.ConstInt, IReg(Cur.Src2), False, True);
      end;
      ssaArrayLBound:
      begin
        d := ArrId; if not OK then Exit;
        apc := NeedPC; if not OK then Exit;
        AotArrBound(apc, d, False);
      end;
      ssaArrayUBound:
      begin
        d := ArrId; if not OK then Exit;
        apc := NeedPC; if not OK then Exit;
        AotArrBound(apc, d, True);
      end;

      ssaReturnSub, ssaEnd, ssaStop:
      begin
        apc := NeedPC; if not OK then Exit;
        ExitTo(apc);   // interpreter executes the bcReturnSub/bcEnd itself (FramePop etc.)
      end;
    else
      // Unreachable: IsB1Op and this case list are the same set, and everything else was
      // routed to the helper above.
      Fail('op:' + OpName(Cur.OpCode));
    end;
  end;

  // AOT_DUMP: leave the finished region on disk. Two files, because they answer two different
  // questions and only one of them needs a disassembler: the .bin is exactly what TExecMem is about
  // to copy into executable memory (feed it to `objdump -b binary -m i386:x86-64`), and the .map
  // says what the emitter was doing at each offset. job/tests/tools/aot_disasm.ps1 merges them.
  //
  // Called after the jump fixups are patched, so the branch displacements on disk are the ones that
  // will actually run - a dump taken before them shows every jump going to itself.
  procedure WriteDump;
  var
    Base, nm: string;
    i: Integer;
    L: TStringList;
    Fs: TFileStream;
  begin
    nm := '';
    for i := 1 to Length(Region.Name) do
      if Region.Name[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_'] then nm := nm + Region.Name[i]
      else nm := nm + '_';
    if nm = '' then nm := 'region';
    Base := IncludeTrailingPathDelimiter(AotDumpDir) + Format('%.3d_%s', [GAotDumpSeq, nm]);
    Inc(GAotDumpSeq);
    try
      Fs := TFileStream.Create(Base + '.bin', fmCreate);
      try
        if E.Len > 0 then Fs.WriteBuffer(E.Bytes^, E.Len);
      finally
        Fs.Free;
      end;
      L := TStringList.Create;
      try
        L.Add('# region ' + Region.Name + '  bytes=' + IntToStr(E.Len));
        L.Add('# blocks=' + IntToStr(Region.FirstBlock) + '..' + IntToStr(Region.LastBlock) +
              ' ssa=' + IntToStr(Region.InstrCount) + ' entryPC=' + IntToStr(Region.EntryPC));
        L.Add('# frame=' + IntToStr(FrameSize) + ' helperCalls=' + IntToStr(NHelperCalls) +
              ' linscan=' + BoolToStr(LsActive, True) +
              ' dynf=' + BoolToStr(DynFActive, True) +
              ' dyni=' + BoolToStr(DynIActive, True) +
              ' rsiPool=' + BoolToStr(RsiIsPool, True));
        L.Add('# epilogue=' + IntToStr(EpiOff) + ' bareEpilogue=' + IntToStr(BareEpiOff));
        L.AddStrings(DumpHdr);
        for i := 0 to NNote - 1 do
          L.Add(IntToStr(DumpAt[i]) + #9 + DumpTxt[i]);
        L.SaveToFile(Base + '.map');
      finally
        L.Free;
      end;
    except
      // A dump that cannot be written must never take the run down with it: this is a diagnostic,
      // and the compiled code is already correct and complete at this point.
      on E2: Exception do
        WriteLn(ErrOutput, '[AOT_DUMP] ', Base, ': ', E2.Message);
    end;
  end;

var
  b, j, k, w, d, TargetOff: Integer;
  Blk: TSSABasicBlock;
begin
  Result := nil;
  LabelIdx := nil;
  DumpHdr := nil;
  DumpOn := AotDumpDir <> '';           // read ONCE: the emit loop must never touch the environment
  NNote := 0;
  BailWhy := '';
  OK := True;
  ArrClassic := not AllowUnsafe;
  HasRecMark := False; HasDeopt := False; HasHelperCall := False; NHelperCalls := 0;
  RecMarkRoutable := True;
  HasNativeRecAlloc := False; RecMarkNative := False;
  MaxIReg := -1; MaxFReg := -1; MaxArrId := -1;
  SetLength(IUse, 16); SetLength(FUse, 16); SetLength(AUse, 8);
  NFix := 0; NIAlloc := 0; NFAlloc := 0;
  NACache := 0;
  SetLength(ArrCountNeeded, 0); SetLength(ArrCountNeeded, 8);
  SetLength(ACacheId, Length(IntPool));
  SetLength(ACacheKind, Length(IntPool));
  SetLength(ACacheReg, Length(IntPool));
  FillChar(SaveXmm, SizeOf(SaveXmm), 0);
  FillChar(SaveGpr, SizeOf(SaveGpr), 0);

  CurOrd := Region.FirstOrdinal;   // Prescan uses its own ordinal; keep for NeedPC in emission
  Prescan;
  if not OK then Exit;

  // C1: liveness. Computed here, consumed from C3 on (helper-call spilling) - it must not
  // change a single emitted byte today.
  AotDiagMemAccI := 0; AotDiagMemAccF := 0; AotDiagCodeW := 0;
  LivenessOK := False; PeakLiveInt := 0; PeakLiveFloat := 0;
  // rsi normally holds the FloatRegs base. A region that touches NO float register never needs
  // it, and then it is simply a tenth GPR - which is where the AOT's remaining pressure is: on
  // n-body 98.8% of the bank traffic is integer, and the integer-only benchmarks (intpoly, sieve,
  // arraysum) are exactly the ones with 20-28 values live against the pool. Nothing is
  // re-addressed: the register is only handed out where its usual job does not exist.
  // The prologue pushes rsi by name either way, so the caller's value is preserved regardless.
  RsiIsPool := (MaxFReg < 0) and AotRsiPoolEnabled;
  LiveNB := 0;
  ComputeLiveness;

  // B1b: the interval model. Built from the liveness above and Prescan's block weights; measured
  // and reported now, consumed by the linear-scan allocator.
  BuildIntervals;
  AotDiagLsWhy := LsWhy;

  SetLength(ILoc, MaxIReg + 1); for k := 0 to MaxIReg do ILoc[k] := -1;
  SetLength(FLoc, MaxFReg + 1); for k := 0 to MaxFReg do FLoc[k] := -1;
  SetLength(IAllocd, Length(IntPool)); SetLength(FAllocd, 14);
  Allocate;
  PlanLinScan;    // AOT_LINSCAN (B1b): may replace the static homes with a per-INTERVAL schedule
  PlanDynFloat;   // AOT_DYNF: may replace the static float homes with a within-block dynamic schedule
  PlanDynInt;     // AOT_DYNF (c): same for the integer GPR pool (minus the array-descriptor cache)
  AotDiagDynFActive := DynFActive;
  AotDiagDynIActive := DynIActive;
  AotDiagLinScanActive := LsActive;
  AotDiagMergeApplied := SSAProg.RegisterMergeApplied;

  HelperOps := TStringList.Create;

  // Region-local label -> block-list index (see the jump case for why not BlockIndex).
  LabelIdx := TStringList.Create;
  LabelIdx.Sorted := True;
  LabelIdx.Duplicates := dupIgnore;
  for k := Region.FirstBlock to Region.LastBlock do
    if SSAProg.Blocks[k].LabelName <> '' then
      LabelIdx.AddObject(SSAProg.Blocks[k].LabelName, TObject(PtrInt(k)));

  // AOT_DUMP register legend, captured HERE and not at write time: the dynamic allocators mutate
  // ILoc/FLoc as emission walks, so by the end they describe the last instruction, not the region.
  // rbx and rsi are the two bases the prologue installs; the pool assignments are what a
  // disassembly cannot possibly tell you on its own.
  if DumpOn then
  begin
    DumpHdr := TStringList.Create;
    DumpHdr.Add('# base rbx=IntRegs  rsi=' + BoolToStr(RsiIsPool, 'pool', 'FloatRegs') +
                '  scratch rax/rcx/rdx xmm0/xmm1');
    for k := 0 to NIAlloc - 1 do
      DumpHdr.Add('# home int r' + IntToStr(IAllocd[k]) + ' -> ' + GprName(ILoc[IAllocd[k]]) +
                  ' (uses=' + IntToStr(IUse[IAllocd[k]]) + ')');
    for k := 0 to NFAlloc - 1 do
      DumpHdr.Add('# home float f' + IntToStr(FAllocd[k]) + ' -> ' + XmmName(FLoc[FAllocd[k]]) +
                  ' (uses=' + IntToStr(FUse[FAllocd[k]]) + ')');
    for k := 0 to NACache - 1 do
      if ACacheKind[k] = 0 then
        DumpHdr.Add('# cache arr' + IntToStr(ACacheId[k]) + '.data -> ' + GprName(ACacheReg[k]))
      else
        DumpHdr.Add('# cache arr' + IntToStr(ACacheId[k]) + '.count -> ' + GprName(ACacheReg[k]));
  end;

  E := TX86Emitter.Create;
  try
    SetLength(BlockOff, SSAProg.Blocks.Count);
    for k := 0 to High(BlockOff) do BlockOff[k] := -1;
    if DumpOn then DumpNote('prologue');

    // Frame layout. A region with no helper call stays a leaf and keeps exactly the frame it
    // has always had (nothing, or 16 bytes for xmm6/7) - the validated codegen must not move
    // because of a feature it does not use. A region that DOES call needs, on top of that,
    // the callee's shadow space, a slot for each base register the ABI lets a callee clobber,
    // and enough padding that rsp is 16-byte aligned at the call.
    if HasHelperCall then FrameSize := ABI_SHADOW_SPACE else FrameSize := 0;
    SlotXmm := -1; SlotCtxSave := -1; SlotFltSave := -1;
    w := 0;
    for k := 6 to 15 do if SaveXmm[k] then Inc(w);
    if w > 0 then
    begin
      SlotXmm := FrameSize;
      Inc(FrameSize, ((w * 8) + 15) and not 15);      // keep the frame 16-byte aligned
    end;
    if HasHelperCall then
    begin
      SlotCtxSave := FrameSize; SlotFltSave := FrameSize + 8; Inc(FrameSize, 16);
      // rsp is 8 (mod 16) on entry and moves by 8 per push; pad so the `call` sees 0.
      k := 2; for b := 0 to 15 do if SaveGpr[b] then Inc(k);
      Inc(FrameSize, ((8 + 8 * k) - FrameSize) mod 16);
    end;

    // Prologue (Win64: rcx=IntRegs rdx=FloatRegs r8=AotCtx; SysV: rdi/rsi/rdx).
    E.Emit8($53);                                    // push rbx
    E.Emit8($56);                                    // push rsi
    for k := 0 to 15 do
      if SaveGpr[k] then
      begin
        if k >= 8 then E.Emit8($41);                 // REX.B for r8-r15
        E.Emit8($50 or (k and 7));                   // push k
      end;
    {$IFDEF WINDOWS}
    E.EmitBytes([$48, $89, $CB]);                    // mov rbx, rcx
    E.EmitBytes([$48, $89, $D6]);                    // mov rsi, rdx
    {$ELSE}
    E.EmitBytes([$48, $89, $FB]);                    // mov rbx, rdi
    E.EmitBytes([$49, $89, $D0]);                    // mov r8, rdx
    {$ENDIF}
    if FrameSize > 0 then
    begin
      E.EmitBytes([$48, $81, $EC]); E.Emit32(LongWord(FrameSize));  // sub rsp, FrameSize
      w := 0;
      for k := 6 to 15 do
        if SaveXmm[k] then begin FrameXmm(True, k, SlotXmm + w * 8); Inc(w); end;
      if HasHelperCall then
      begin
        FrameStore(R8, SlotCtxSave);                 // the ctx pointer: r8 is volatile everywhere
        if not RsiIsPool then
          FrameStore(RSI, SlotFltSave);              // the FloatRegs base: rsi is volatile in SysV
      end;
    end;
    // Entry loads of the allocated registers.
    for k := 0 to NIAlloc - 1 do
      LoadRegMem(ILoc[IAllocd[k]], LongWord(IAllocd[k]) * 8);
    for k := 0 to NFAlloc - 1 do
      SseMem([$F2, $0F, $10], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
    // Array descriptor cache: base/count of the hot arrays, invariant for the whole
    // invocation (no DIM/REDIM/ERASE in the op set).
    ReloadArrayCache;

    // Body: blocks in order (fall-through preserved by contiguous emission).
    CurOrd := Region.FirstOrdinal;
    DynPos := 0;
    for b := Region.FirstBlock to Region.LastBlock do
    begin
      Blk := SSAProg.Blocks[b];
      BlockOff[b] := E.Len;
      // The loop weight is the single most useful number on a block header here: it is what says
      // whether the bytes below run once or a million times.
      if DumpOn then
        DumpNote(Format('block %d %s w=%d', [b, Blk.LabelName, BlockW[b - Region.FirstBlock]]));
      for j := 0 to Blk.Instructions.Count - 1 do
      begin
        Cur := Blk.Instructions[j];
        CurBlkIdx := b;
        CurIsBlockLast := j = Blk.Instructions.Count - 1;
        // AOT_DYNF start event: the temp defined here becomes resident in its home BEFORE the
        // instruction is emitted, so its defining store writes that register.
        if DynFActive and (DynFHomeReg[DynPos] >= 0) then
        begin
          FLoc[DynFHomeReg[DynPos]] := DynFHomeXmm[DynPos];
          DynFCur[DynFHomeXmm[DynPos]] := DynFHomeReg[DynPos];
          if DumpOn then
            DumpNote(Format('~dynf f%d takes %s', [DynFHomeReg[DynPos], XmmName(DynFHomeXmm[DynPos])]));
        end;
        if DynIActive and (DynIHomeReg[DynPos] >= 0) then
        begin
          ILoc[DynIHomeReg[DynPos]] := DynIHomeGpr[DynPos];
          DynICur[DynIHomeGpr[DynPos]] := DynIHomeReg[DynPos];
          if DumpOn then
            DumpNote(Format('~dyni r%d takes %s', [DynIHomeReg[DynPos], GprName(DynIHomeGpr[DynPos])]));
        end;
        // B1b start events: every web starting here takes its machine home BEFORE the instruction,
        // so a defining store writes it. A web that opens on a USE - the region's entry live-ins,
        // and anything starting on a helper-routed op, which does its work through the banks -
        // reads the bank first, which is what makes the machine register agree with it.
        //
        if LsActive then
          for k := 0 to High(LsTakeAt[DynPos]) do
          begin
            w := LsTakeAt[DynPos][k];
            if DumpOn then
              DumpNote(Format('~web%d %s%d takes %s%s', [w,
                       BoolToStr(LsWebs[w].Bank = 1, 'f', 'r'), LsWebs[w].Reg,
                       BoolToStr(LsWebs[w].Bank = 1, XmmName(LsWebs[w].Home), GprName(LsWebs[w].Home)),
                       BoolToStr(LsWebs[w].NeedsLoad, ' (load)', '')]));
            if LsWebs[w].Bank = 1 then
            begin
              FLoc[LsWebs[w].Reg] := LsWebs[w].Home;
              DynFCur[LsWebs[w].Home] := LsWebs[w].Reg;
              if LsWebs[w].NeedsLoad then
                SseMem([$F2, $0F, $10], LsWebs[w].Home, RSI, LongWord(LsWebs[w].Reg) * 8);
            end
            else
            begin
              ILoc[LsWebs[w].Reg] := LsWebs[w].Home;
              DynICur[LsWebs[w].Home] := LsWebs[w].Reg;
              if LsWebs[w].NeedsLoad then
                LoadRegMem(LsWebs[w].Home, LongWord(LsWebs[w].Reg) * 8);
            end;
          end;
        // A web ending on a terminator writes back before the branch is emitted (StoreEarly).
        if LsActive then
          for k := 0 to High(LsFreeAt[DynPos]) do
          begin
            w := LsFreeAt[DynPos][k];
            if not LsWebs[w].StoreEarly then System.Continue;
            if DumpOn then
              DumpNote(Format('~web%d %s%d writes back early', [w,
                       BoolToStr(LsWebs[w].Bank = 1, 'f', 'r'), LsWebs[w].Reg]));
            if LsWebs[w].Bank = 1 then
              SseMem([$F2, $0F, $11], LsWebs[w].Home, RSI, LongWord(LsWebs[w].Reg) * 8)
            else
              StoreRegMem(LsWebs[w].Home, LongWord(LsWebs[w].Reg) * 8);
          end;
        if DumpOn then
          DumpNote(Format('#%d pc=%d %s', [CurOrd, Prog.GetSsaPc(CurOrd), OpName(Cur.OpCode)]));
        d := E.Len;
        EmitInstruction;
        if not OK then Exit;
        // Loop-weighted EMITTED CODE SIZE: how many bytes actually execute per iteration of the
        // hottest loop. When two allocators send the SAME operand traffic to the banks, this is
        // what is left to explain a clock difference between them.
        AotDiagCodeW := AotDiagCodeW + Int64(E.Len - d) * BlockW[b - Region.FirstBlock];
        // AOT_DYNF free events: temps whose last touch was this instruction leave their home
        // AFTER it is emitted (the last use has just read them). No bank store - they are dead.
        if DynFActive then
          for k := 0 to High(DynFFree[DynPos]) do
          begin
            w := FLoc[DynFFree[DynPos][k]];
            if (w >= 0) and (DynFCur[w] = DynFFree[DynPos][k]) then DynFCur[w] := -1;
            FLoc[DynFFree[DynPos][k]] := -1;
          end;
        if DynIActive then
          for k := 0 to High(DynIFree[DynPos]) do
          begin
            w := ILoc[DynIFree[DynPos][k]];
            if (w >= 0) and (DynICur[w] = DynIFree[DynPos][k]) then DynICur[w] := -1;
            ILoc[DynIFree[DynPos][k]] := -1;
          end;
        // B1b end events: the value's last use has just been emitted, so the machine register goes
        // back to the pool. Residency must not outlive the live range - outside it, no path
        // guarantees what the register holds, and the next flush at a helper or an exit would
        // write that into the bank.
        //
        // A web that WROTE its register writes it back here. That store is the price of this
        // allocator, and the measurement says it IS the price: skipping it (unsound) brings a
        // 12-42% regression back to parity. It buys the invariant everything else rests on - the
        // bank is authoritative for every value that is not resident - which is what lets a read
        // this analysis never saw still find the right value.
        if LsActive then
          for k := 0 to High(LsFreeAt[DynPos]) do
          begin
            w := LsFreeAt[DynPos][k];
            if DumpOn then
              DumpNote(Format('~web%d %s%d released%s', [w,
                       BoolToStr(LsWebs[w].Bank = 1, 'f', 'r'), LsWebs[w].Reg,
                       BoolToStr(LsWebs[w].HasDef and not LsWebs[w].StoreEarly, ' (store)', '')]));
            if LsWebs[w].Bank = 1 then
            begin
              if LsWebs[w].HasDef and not LsWebs[w].StoreEarly then
                SseMem([$F2, $0F, $11], LsWebs[w].Home, RSI, LongWord(LsWebs[w].Reg) * 8);
              if DynFCur[LsWebs[w].Home] = LsWebs[w].Reg then DynFCur[LsWebs[w].Home] := -1;
              FLoc[LsWebs[w].Reg] := -1;
            end
            else
            begin
              if LsWebs[w].HasDef and not LsWebs[w].StoreEarly then
                StoreRegMem(LsWebs[w].Home, LongWord(LsWebs[w].Reg) * 8);
              if DynICur[LsWebs[w].Home] = LsWebs[w].Reg then DynICur[LsWebs[w].Home] := -1;
              ILoc[LsWebs[w].Reg] := -1;
            end;
          end;
        Inc(DynPos);
        Inc(CurOrd);
      end;
    end;

    // Epilogue: rax already holds the exit PC; flush allocated regs and return.
    EpiOff := E.Len;
    if DumpOn then DumpNote('epilogue (flush + teardown)');
    for k := 0 to NIAlloc - 1 do
      StoreRegMem(ILoc[IAllocd[k]], LongWord(IAllocd[k]) * 8);
    for k := 0 to NFAlloc - 1 do
      SseMem([$F2, $0F, $11], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
    // B3 bare epilogue: same teardown, no flush (see the declaration comment).
    BareEpiOff := E.Len;
    if DumpOn then DumpNote('bare epilogue (teardown only)');
    if FrameSize > 0 then
    begin
      w := 0;
      for k := 6 to 15 do
        if SaveXmm[k] then begin FrameXmm(False, k, SlotXmm + w * 8); Inc(w); end;
      E.EmitBytes([$48, $81, $C4]); E.Emit32(LongWord(FrameSize));  // add rsp, FrameSize
    end;
    for k := 15 downto 0 do
      if SaveGpr[k] then
      begin
        if k >= 8 then E.Emit8($41);
        E.Emit8($58 or (k and 7));                   // pop k
      end;
    E.Emit8($5E);                                    // pop rsi
    E.Emit8($5B);                                    // pop rbx
    E.Emit8($C3);                                    // ret

    // Patch jump fixups (block targets, the epilogue, or the bare epilogue).
    for k := 0 to NFix - 1 do
    begin
      if Fixups[k].TargetBlock = -1 then TargetOff := EpiOff
      else if Fixups[k].TargetBlock = -2 then TargetOff := BareEpiOff
      else TargetOff := BlockOff[Fixups[k].TargetBlock];
      if TargetOff < 0 then begin Fail('fixup-target'); Exit; end;
      E.Patch32(Fixups[k].PatchOff, LongWord(TargetOff - (Fixups[k].PatchOff + 4)));
    end;

    AotDiagHelperCalls := NHelperCalls;
    AotDiagHelperOps := '';
    for k := 0 to HelperOps.Count - 1 do
      if (k = 0) or (HelperOps[k] <> HelperOps[k - 1]) then
      begin
        if AotDiagHelperOps <> '' then AotDiagHelperOps := AotDiagHelperOps + ' ';
        AotDiagHelperOps := AotDiagHelperOps + HelperOps[k];
        j := 1;
        while (k + j < HelperOps.Count) and (HelperOps[k + j] = HelperOps[k]) do Inc(j);
        if j > 1 then AotDiagHelperOps := AotDiagHelperOps + '*' + IntToStr(j);
      end;
    if DumpOn then WriteDump;
    Result := TExecMem.Create(E);
    if Result.Ptr = nil then begin FreeAndNil(Result); Fail('exec-alloc'); end;
  finally
    E.Free;
    LabelIdx.Free;
    HelperOps.Free;
    DumpHdr.Free;
    if (Result = nil) and (BailWhy = '') then BailWhy := 'unknown';
  end;
end;

function AotCompileProgram(SSAProg: TSSAProgram; Prog: TBytecodeProgram;
                           TrueVal: Int64; AllowUnsafe, Diag, SkipMain: Boolean): TAotFuncs;
var
  Regions: TAotRegions;
  r, n, o, LastMapped: Integer;
  Mem: TExecMem;
  Why: string;
begin
  Result := nil;
  n := 0;
  // Whole-program gate for the native SHARED-record path: without a second thread nothing can grow
  // the shared-pointer array while compiled code indexes it, so the VM's lock is not needed there.
  //
  // ⛔ MEASURED AND REJECTED (29 Jul 2026): lifting this gate is worth ZERO, do not retry it blind.
  // Once the VM stopped freeing the outgrown pointer array the gate's original reason was gone, so
  // the obvious next step was to drop it and let compiled code index the shared region even with
  // threads about. On binary-trees -- whose MAKETREE/CHECKTREE/FREETREE regions ARE native, so the
  // gate really did apply -- it measured -1.6% against a 1.2% null floor. Nothing.
  // The reason is that removing the per-access LOCK (see ResolveRec) already made the deopt cheap:
  // what dominates binary-trees now is AllocSharedRecord, four mallocs and a global lock per node.
  // Revisit only once allocation is fixed and the benchmark is traversal-bound again -- and measure
  // it then, because it buys cross-thread exposure in compiled code that nothing currently pays for.
  GNoThreads := True;
  if AotDumpDir <> '' then
    WriteLn(ErrOutput, '[AOT] AOT_DUMP: region dumps go to ', AotDumpDir,
            ' (disassemble with job/tests/tools/aot_disasm.ps1)');
  for r := 0 to SSAProg.Blocks.Count - 1 do
  begin
    for n := 0 to SSAProg.Blocks[r].Instructions.Count - 1 do
      if SSAProg.Blocks[r].Instructions[n].OpCode in [ssaThreadCreate, ssaCallSubIndirect] then
      begin
        GNoThreads := False;
        Break;
      end;
    if not GNoThreads then Break;
  end;
  // Must be set BEFORE AotSliceAndClassify: the classifier reads it through AotArrayNativeOK, and
  // the emitter reads the same global later. If the two saw different values a region would be
  // accepted and then fail at emit time.
  GArrStrNative := AllowUnsafe;
  n := 0;
  Regions := AotSliceAndClassify(SSAProg, Prog);
  SetLength(Result, Length(Regions));
  for r := 0 to High(Regions) do
  begin
    if not Regions[r].Eligible then Continue;
    if Regions[r].EntryPC < 0 then Continue;
    if SkipMain and (Regions[r].Name = 'MAIN') then
    begin
      if Diag then
        WriteLn(ErrOutput, '[AOT] skip MAIN (combined mode: the loop JIT owns module-level loops)');
      Continue;
    end;
    Mem := AotCompileRegion(SSAProg, Prog, Regions[r], TrueVal, Prog.ModernMode, AllowUnsafe, Why);
    if Mem <> nil then
    begin
      Result[n].EntryPC := Regions[r].EntryPC;
      // The region's extent in final bytecode: walk its ordinals backwards to the last one that maps
      // to a PC (the tail can be ordinals that emitted nothing). EntryPC alone if none does.
      Result[n].LastPC := Regions[r].EntryPC;
      for o := Regions[r].FirstOrdinal + Regions[r].InstrCount - 1 downto Regions[r].FirstOrdinal do
      begin
        LastMapped := Prog.GetSsaPc(o);
        if LastMapped >= Result[n].LastPC then
        begin
          Result[n].LastPC := LastMapped;
          Break;
        end;
      end;
      Result[n].Mem := Mem;
      Inc(n);
      if Diag then
      begin
        WriteLn(ErrOutput, Format('[AOT] compiled %-24s entryPC=%-6d liveness=%s peakLive int=%d float=%d ' +
                                  'maxLive int=%d float=%d distinct int=%d float=%d helpers=%d',
                                  [Regions[r].Name, Regions[r].EntryPC,
                                   BoolToStr(AotDiagLivenessOK, 'ok', 'NOT-CONVERGED'),
                                   AotDiagPeakLiveInt, AotDiagPeakLiveFloat,
                                   AotDiagMaxLiveInt, AotDiagMaxLiveFloat,
                                   AotDiagDistinctInt, AotDiagDistinctFloat,
                                   AotDiagHelperCalls]));
        if AotDiagHelperOps <> '' then
          WriteLn(ErrOutput, '[AOT]   helper ops: ' + AotDiagHelperOps);
        // Which register strategy this region got. "merge" and "dynf" are mutually exclusive by
        // arbitration in AUTO (they are antagonistic, never additive); "static" means the region
        // got the plain static homes, either because AUTO judged it latency-bound or because
        // nothing qualified for the dynamic pool.
        WriteLn(ErrOutput, Format('[AOT]   registers: %s (dynf float=%s int=%s, regreuse merge=%s)',
          [BoolToStr(AotDiagMergeApplied, 'merge',
             BoolToStr(AotDiagDynFActive or AotDiagDynIActive, 'dynf', 'static')),
           BoolToStr(AotDiagDynFActive, 'on', 'off'), BoolToStr(AotDiagDynIActive, 'on', 'off'),
           BoolToStr(AotDiagMergeApplied, 'on', 'off')]));
        if AotDiagFloatTotal > 0 then
          WriteLn(ErrOutput, Format('[AOT]   float traffic: static-resident=%d (%.1f%% mem) -> linscan-resident=%d (%.1f%% mem)  recovers %.1f%% of tail',
            [AotDiagFloatResident, 100.0 * (AotDiagFloatTotal - AotDiagFloatResident) / AotDiagFloatTotal,
             AotDiagFloatLinScan, 100.0 * (AotDiagFloatTotal - AotDiagFloatLinScan) / AotDiagFloatTotal,
             100.0 * (AotDiagFloatLinScan - AotDiagFloatResident) / (AotDiagFloatTotal - AotDiagFloatResident + 0.0001)]));
        if AotDiagFloatTotal > 0 then
          WriteLn(ErrOutput, Format('[AOT]   float block-local temps: %d slots, use=%d (%.1f%% of total) - the low-risk hybrid ceiling',
            [AotDiagFloatBlockLocalCount, AotDiagFloatBlockLocal,
             100.0 * AotDiagFloatBlockLocal / AotDiagFloatTotal]));
        // B1b interval model. Read (webs vs distinct) and (maxOverlap vs pool): the first says how
        // much the one-home-per-register allocation is conflating, the second whether what is
        // really live at once fits the machine pool at all.
        WriteLn(ErrOutput, Format('[AOT]   emitted bank traffic (loop-weighted): int=%d float=%d  code bytes=%d',
          [AotDiagMemAccI, AotDiagMemAccF, AotDiagCodeW]));
        // C7: how many div/mod sites got the multiply-high and how many stayed on idiv. Without this
        // the only evidence is the stopwatch, and a lowering that never fires reads exactly like one
        // that fires and does not pay.
        if (GDivConstHit > 0) or (GDivConstMiss > 0) then
          WriteLn(ErrOutput, Format('[AOT]   div by constant: %d site(s) lowered to multiply-high, %d left on idiv',
            [GDivConstHit, GDivConstMiss]));
        if AotDiagLsWhy = '' then
        begin
          WriteLn(ErrOutput, Format('[AOT]   intervals: webs int=%d float=%d (ranges=%d) maxOverlap int=%d float=%d edge-crossings=%d',
            [AotDiagLsWebsInt, AotDiagLsWebsFloat, AotDiagLsRanges,
             AotDiagLsMaxOverInt, AotDiagLsMaxOverFloat, AotDiagLsCross]));
          if AotDiagLinScanActive then
          begin
            WriteLn(ErrOutput, Format('[AOT]   linscan: ACTIVE, placed int=%d float=%d, memory-homed int=%d float=%d',
              [AotDiagLsPlacedInt, AotDiagLsPlacedFloat, AotDiagLsSpilledInt, AotDiagLsSpilledFloat]));
            WriteLn(ErrOutput, Format('[AOT]   linscan traffic: loads=%d (loop-weighted %d)  stores=%d (loop-weighted %d)',
              [AotDiagLsLoads, AotDiagLsLoadW, AotDiagLsStores, AotDiagLsStoreW]));
          end;
        end
        else
          WriteLn(ErrOutput, '[AOT]   intervals: not built (' + AotDiagLsWhy + ')');
        if AotDiagIntTotal > 0 then
          WriteLn(ErrOutput, Format('[AOT]   int traffic:   static-resident=%d (%.1f%% mem) -> linscan-resident=%d (%.1f%% mem)  recovers %.1f%% of tail',
            [AotDiagIntResident, 100.0 * (AotDiagIntTotal - AotDiagIntResident) / AotDiagIntTotal,
             AotDiagIntLinScan, 100.0 * (AotDiagIntTotal - AotDiagIntLinScan) / AotDiagIntTotal,
             100.0 * (AotDiagIntLinScan - AotDiagIntResident) / (AotDiagIntTotal - AotDiagIntResident + 0.0001)]));
      end;
    end
    else if Diag then
      WriteLn(ErrOutput, Format('[AOT] compile-bail %-20s (%s)', [Regions[r].Name, Why]));
  end;
  SetLength(Result, n);
end;

end.
