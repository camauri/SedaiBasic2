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
procedure AotSurvey(SSAProg: TSSAProgram; Prog: TBytecodeProgram);

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
  // C3: runtime-helper calls emitted in the last region. Also the coverage delta this stage
  // bought: a region reporting helpers>0 is one that could not compile at all before, since
  // a single op outside the native set used to bail the whole function.
  AotDiagHelperCalls: Integer = 0;
  // Which ops those helper calls execute, as "name*count" pairs - the first thing to read
  // when hunting a hot-loop helper (a cold DIM/PRINT is fine, a per-iteration op is not).
  AotDiagHelperOps: string = '';

// Compile every eligible region to native code (B1a: static frequency register
// assignment, deopt only for trapping ops). TrueVal is the VM's TRUE (-1);
// the dialect comes from Prog.ModernMode. AllowUnsafe = MODERN and no forced
// bounds check: array OOB takes the FreeBASIC default path natively; otherwise
// array accesses guard and deopt so the interpreter raises. Diag prints
// per-region compile results.
// SkipMain: engine arbitration for the COMBINED --aot --jit mode. The loop JIT can only see
// (and inline callees into) loops that run in the interpreter's dispatch loop; an AOT-compiled
// MAIN steals the module-level hot loop from it and replaces loop inlining with a native call
// per iteration - measured 2x slower on n-body. Until AOT does its own inlining, the split
// that wins is: JIT owns MAIN (module loops, inlining), AOT owns the procedures.
function AotCompileProgram(SSAProg: TSSAProgram; Prog: TBytecodeProgram;
                           TrueVal: Int64; AllowUnsafe, Diag, SkipMain: Boolean): TAotFuncs;

implementation

uses TypInfo;

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
    ssaStrLeft, ssaStrRight, ssaStrMid, ssaStrAsc, ssaStrChr, ssaStrInstr,
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
    bcThreadCreate, bcThreadDetach, bcThreadSelf, bcThreadWait,
    bcMutexCreate, bcMutexDestroy, bcMutexLock, bcMutexUnlock,
    bcCondCreate, bcCondDestroy, bcCondSignal, bcCondBroadcast, bcCondWait,
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
  if ((Ins.OpCode = ssaArrayLoad) or (Ins.OpCode = ssaArrayStore)) and
     (SSAProg.GetArray(Ins.Src1.ArrayIndex).ElementType = srtString) then Exit;
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
    ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString,
    ssaCopyString, ssaLoadConstString, ssaStrConcat, ssaStrLen,
    ssaStrLeft, ssaStrRight, ssaStrMid, ssaStrAsc, ssaStrChr, ssaStrInstr,
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

procedure AotSurvey(SSAProg: TSSAProgram; Prog: TBytecodeProgram);
var
  Regions: TAotRegions;
  r, NElig, NB3: Integer;
  ProcAtEntry: string;
begin
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
  IntPool: array[0..6] of Integer = (R9, R10, R11, R12, R13, R14, R15);
type
  TFix = record PatchOff, TargetBlock: Integer; end;  // TargetBlock -1 = epilogue
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
  SaveGpr: array[R12..R15] of Boolean;
  SaveX6, SaveX7: Boolean;
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
  ArrClassic: Boolean;                  // array OOB raises (CLASSIC / --bounds-check) -> guard + deopt
  LivenessOK: Boolean;                  // C1: the liveness fixpoint converged
  PeakLiveInt, PeakLiveFloat: Integer;  // C1: peak simultaneously-live values per bank
  // C3 helper calls. The region stops being a leaf function as soon as one is emitted, which
  // is what forces a real frame: 16-byte alignment at the call and the callee's shadow space.
  HasHelperCall: Boolean;
  NHelperCalls: Integer;                // helper calls actually emitted (diagnostics)
  FrameSize: Integer;                   // bytes subtracted from rsp after the pushes (0 = leaf)
  SlotXmm: Integer;                     // [rsp+SlotXmm]   xmm6/xmm7 save area (-1 = none)
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
  DynFCur: array[0..7] of Integer;      // xmm index -> VM float reg currently resident there (-1 free)
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

  procedure Fail(const Why: string);
  begin
    if OK then BailWhy := Why;
    OK := False;
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
  procedure FlushResidentF;
  var x: Integer;
  begin
    if not DynFActive then Exit;
    for x := 2 to 7 do
      if DynFCur[x] >= 0 then
        E.MemOp([$F2, $0F, $11], x, RSI, LongWord(DynFCur[x]) * 8);   // movsd [rsi+reg*8], xmm x
  end;
  procedure ReloadResidentF;
  var x: Integer;
  begin
    if not DynFActive then Exit;
    for x := 2 to 7 do
      if DynFCur[x] >= 0 then
        E.MemOp([$F2, $0F, $10], x, RSI, LongWord(DynFCur[x]) * 8);   // movsd xmm x, [rsi+reg*8]
  end;
  // AOT_DYNF int counterpart: store / reload the dynamically-resident int temps through the int
  // bank (rbx). Pool GPRs are r9..r15; the store/load helpers bake the right REX for extended regs.
  // mov [rbx+reg*8], g / mov g, [rbx+reg*8] emitted raw (StoreRegMem/LoadRegMem are defined below
  // this point; the pool GPRs are all >= r8 so REX.R is always set).
  procedure FlushResidentI;
  var g: Integer;
  begin
    if not DynIActive then Exit;
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
    if not DynIActive then Exit;
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
      E.MemOp(MemForm, scr, RBX, LongWord(vmreg) * 8);
  end;
  function FAlloc(vmreg: Integer): Integer;
  begin
    if vmreg <= MaxFReg then Result := FLoc[vmreg] else Result := -1;
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
      if n <> Wx then E.EmitBytes([$0F, $28, $C0 or (Wx shl 3) or n]);   // movaps Wx, n
    end
    else E.MemOp([$F2, $0F, $10], Wx, RSI, LongWord(vmreg) * 8);
  end;
  procedure FOp(const SseOp: array of Byte; Wx, vmreg: Integer);
  var n: Integer;
  begin
    n := FAlloc(vmreg);
    if n >= 0 then
      E.EmitBytes([SseOp[0], SseOp[1], SseOp[2], $C0 or (Wx shl 3) or n])
    else
      E.MemOp(SseOp, Wx, RSI, LongWord(vmreg) * 8);
  end;
  procedure FStore(vmreg, Wx: Integer);
  var n: Integer;
  begin
    n := FAlloc(vmreg);
    if n >= 0 then
    begin
      if n <> Wx then E.EmitBytes([$0F, $28, $C0 or (n shl 3) or Wx]);   // movaps n, Wx (see FLoad)
    end
    else E.MemOp([$F2, $0F, $11], Wx, RSI, LongWord(vmreg) * 8);
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
  procedure FloatBin(const SseOp: array of Byte);
  begin
    FLoad(XMM0, FReg(Cur.Src1));
    FOp(SseOp, XMM0, FReg(Cur.Src2));
    FStore(FReg(Cur.Dest), XMM0);
  end;
  // Signed div/mod with the interpreter's raise semantics via deopt (JIT J10 pattern).
  procedure DivModSigned(apc: Integer; WantRemainder: Boolean);
  var p1, p2: Integer;
  begin
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
    E.Emit8($F2); E.Emit8($0F);
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
      E.MemOp([$F2, $0F, $11], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
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
    FrameLoad(RSI, SlotFltSave);

    // 4. Re-read the banks: the helper may have written any of them.
    for k := 0 to NIAlloc - 1 do
      LoadRegMem(ILoc[IAllocd[k]], LongWord(IAllocd[k]) * 8);
    for k := 0 to NFAlloc - 1 do
      E.MemOp([$F2, $0F, $10], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
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
      E.MemOp([$F2, $0F, $11], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
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
    FrameLoad(RSI, SlotFltSave);
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
      E.MemOp([$F2, $0F, $10], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
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
        E.MemOp([$F2, $0F, $11], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
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
        E.MemOp([$F2, $0F, $10], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
  end;
  // After a native leaf call: restore the base regs the call may have clobbered (r8 ctx, rsi
  // FloatRegs), then the caller-saved allocated registers (rbx is callee-saved and survives).
  procedure StrCallEpilogue;
  begin
    FrameLoad(R8, SlotCtxSave);
    FrameLoad(RSI, SlotFltSave);
    ReloadVolatiles;
    ReloadResidentF; ReloadResidentI;
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
    // Per region-relative block index: bitsets as plain boolean arrays (register counts are
    // small - MAX_*_REGS is 32/32/16 before compaction, and post-compaction indexes are dense).
    UseI, DefI, InI, OutI: array of array of Boolean;
    UseF, DefF, InF, OutF: array of array of Boolean;
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

  procedure Prescan;
  var
    b, j, o: Integer;
    Blk: TSSABasicBlock;
    Ins: TSSAInstruction;
    UseW: Integer;                      // current block's loop-depth weight (B1b-lite)
    BlockW: array of Integer;           // region-relative block index -> weight

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
            HasRecMark := True;
            if not AotHelperRoutable(Prog, o) then RecMarkRoutable := False;
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
              CountVal(Ins.Dest); CountVal(Ins.Src2);
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
          ssaStrLeft, ssaStrRight, ssaStrMid, ssaStrAsc, ssaStrChr, ssaStrInstr,
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
    if HasRecMark and HasDeopt then
    begin
      if not RecMarkRoutable then Fail('recmark-route');
      HasHelperCall := True;
    end;
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
    for k := 0 to High(IntPool) do
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
      if IntPool[k] >= R12 then SaveGpr[IntPool[k]] := True;
    end;
    // Floats: most-used first onto xmm2..xmm7.
    SetLength(Taken, 0); SetLength(Taken, MaxFReg + 1);
    for k := 2 to 7 do
    begin
      best := -1; bestUse := 0;
      for r := 0 to MaxFReg do
        if (not Taken[r]) and (FUse[r] > bestUse) then begin best := r; bestUse := FUse[r]; end;
      if best < 0 then Break;
      Taken[best] := True;
      FLoc[best] := k;
      FAllocd[NFAlloc] := best; Inc(NFAlloc);
      if k = 6 then SaveX6 := True;
      if k = 7 then SaveX7 := True;
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
  var bi2, si, i2, ri, sIdx, lo: Integer;
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
    for bi2 := 0 to ri - 1 do
    begin
      if not inLoop[bi2] then System.Continue;
      B2 := SSAProg.Blocks[Region.FirstBlock + bi2];
      for i2 := 0 to B2.Instructions.Count - 1 do
      begin
        Ins2 := B2.Instructions[i2];
        case Ins2.OpCode of
          ssaDivFloat, ssaMathSqr,
          ssaDivInt, ssaModInt, ssaDivUInt, ssaModUInt: Exit(False);   // ~20-40 cycle latency
        end;
      end;
    end;
    Result := True;
  end;

  procedure PlanDynFloat;
  var
    totpos, nbk, bi, kk, pp, r, a, x, xf, mode: Integer;
    Blk: TSSABasicBlock; Ins: TSSAInstruction;
    blkOf, firstDef, firstUse, lastTouch, cand: array of Integer;
    activeReg: array[2..7] of Integer;
    ncand: Integer; usedAny, used6, used7, isDef: Boolean;

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
    for a := 0 to 7 do DynFCur[a] := -1;
    mode := AotDynFloatMode;
    if mode = 2 then Exit;                                     // forced off
    if MaxFReg < 0 then Exit;
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

    usedAny := False; used6 := False; used7 := False;
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
      for x := 2 to 7 do activeReg[x] := -1;
      for a := 0 to ncand - 1 do
      begin
        r := cand[a];
        for x := 2 to 7 do                            // expire intervals that ended before r's def
          if (activeReg[x] >= 0) and (lastTouch[activeReg[x]] < firstDef[r]) then activeReg[x] := -1;
        xf := -1;
        for x := 2 to 7 do if activeReg[x] < 0 then begin xf := x; Break; end;
        if xf < 0 then System.Continue;               // pool full: r stays in the bank
        activeReg[xf] := r;
        DynFHomeReg[firstDef[r]] := r; DynFHomeXmm[firstDef[r]] := xf;
        SetLength(DynFFree[lastTouch[r]], Length(DynFFree[lastTouch[r]]) + 1);
        DynFFree[lastTouch[r]][High(DynFFree[lastTouch[r]])] := r;
        usedAny := True;
        if xf = 6 then used6 := True; if xf = 7 then used7 := True;
      end;
    end;

    if not usedAny then Exit;                          // keep the static homes Allocate set
    DynFActive := True;
    for r := 0 to MaxFReg do FLoc[r] := -1;            // drop static float homes; go fully dynamic
    NFAlloc := 0;
    SaveX6 := used6; SaveX7 := used7;
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
    if MaxIReg < 0 then Exit;
    if (mode = 0) and not RegionThroughputBound then Exit;

    // Build the dynamic pool: IntPool GPRs not reserved by the array-descriptor cache.
    SetLength(poolG, Length(IntPool)); np := 0;
    for a := 0 to High(IntPool) do
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
        if poolG[xf] >= R12 then SaveGpr[poolG[xf]] := True;
      end;
    end;

    if not usedAny then Exit;
    DynIActive := True;
    for r := 0 to MaxIReg do ILoc[r] := -1;            // drop static int homes; array cache stays
    NIAlloc := 0;
  end;

  procedure EmitInstruction;
  var d, w: Integer;
      apc: Integer;
      p1: Integer;
      bits: Int64;
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
        if HasDeopt then
        begin
          apc := NeedPC; if not OK then Exit;
          EmitHelperCall(apc);
        end;


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
            E.EmitBytes([$0F, $28, $C0 or (FAlloc(d) shl 3) or FAlloc(p1)]);
        end
        else
        begin FLoad(XMM0, p1); FStore(d, XMM0); end;
      end;

      ssaAddInt: begin ILoad(RAX, IReg(Cur.Src1)); IOp([$48, $03], RAX, IReg(Cur.Src2)); IStore(IReg(Cur.Dest), RAX); end;
      ssaSubInt: begin ILoad(RAX, IReg(Cur.Src1)); IOp([$48, $2B], RAX, IReg(Cur.Src2)); IStore(IReg(Cur.Dest), RAX); end;
      ssaMulInt: begin ILoad(RAX, IReg(Cur.Src1)); IOp([$48, $0F, $AF], RAX, IReg(Cur.Src2)); IStore(IReg(Cur.Dest), RAX); end;
      ssaNegInt: begin ILoad(RAX, IReg(Cur.Src1)); E.EmitBytes([$48, $F7, $D8]); IStore(IReg(Cur.Dest), RAX); end;
      ssaDivInt:  begin apc := NeedPC; if OK then DivModSigned(apc, False); end;
      ssaModInt:  begin apc := NeedPC; if OK then DivModSigned(apc, True); end;
      ssaDivUInt: begin apc := NeedPC; if OK then DivModUnsigned(apc, False); end;
      ssaModUInt: begin apc := NeedPC; if OK then DivModUnsigned(apc, True); end;

      ssaAddFloat: FloatBin([$F2, $0F, $58]);
      ssaSubFloat: FloatBin([$F2, $0F, $5C]);
      ssaMulFloat: FloatBin([$F2, $0F, $59]);
      ssaDivFloat:
      begin
        if Modern then FloatBin([$F2, $0F, $5E])
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
        FLoad(XMM0, FReg(Cur.Src1));
        MovImm64(RAX, Int64($8000000000000000));
        E.EmitBytes([$66, $48, $0F, $6E, $C8]);        // movq xmm1, rax
        E.EmitBytes([$66, $0F, $57, $C1]);             // xorpd xmm0, xmm1
        FStore(FReg(Cur.Dest), XMM0);
      end;
      ssaMathSqr:
      begin
        FLoad(XMM0, FReg(Cur.Src1));
        if not Modern then
        begin
          // CLASSIC raises on Sqr(neg): sign bit set (incl. -0.0, where the interpreter
          // is also the safe path) -> deopt.
          apc := NeedPC; if not OK then Exit;
          E.EmitBytes([$66, $48, $0F, $7E, $C0]);      // movq rax, xmm0
          E.EmitBytes([$48, $85, $C0]);                // test rax, rax
          E.EmitBytes([$79, $00]); p1 := E.Len - 1;    // jns +ok
          ExitTo(apc);
          E.PatchByte(p1, Byte(E.Len - (p1 + 1)));
        end;
        E.EmitBytes([$F2, $0F, $51, $C0]);             // sqrtsd xmm0, xmm0
        FStore(FReg(Cur.Dest), XMM0);
      end;

      ssaIntToFloat:
      begin
        ILoad(RAX, IReg(Cur.Src1));
        E.EmitBytes([$F2, $48, $0F, $2A, $C0]);        // cvtsi2sd xmm0, rax
        FStore(FReg(Cur.Dest), XMM0);
      end;
      ssaFloatToInt:
      begin
        FLoad(XMM0, FReg(Cur.Src1));
        if Modern then E.EmitBytes([$F2, $48, $0F, $2D, $C0])   // cvtsd2si (round-to-even)
        else E.EmitBytes([$F2, $48, $0F, $2C, $C0]);            // cvttsd2si (truncate)
        IStore(IReg(Cur.Dest), RAX);
      end;
      ssaFloatRound:
      begin
        FLoad(XMM0, FReg(Cur.Src1));
        E.EmitBytes([$F2, $48, $0F, $2D, $C0]);        // cvtsd2si (CINT: round-to-even)
        IStore(IReg(Cur.Dest), RAX);
      end;
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
        E.MemOp([$F2, $0F, $10], XMM0, RDX, LongWord(w) * 8);
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
        E.MemOp([$F2, $0F, $11], XMM0, RDX, LongWord(w) * 8);
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
        if SSAProg.GetArray(d).ElementType = srtFloat then
          AotArrAccess(True, False, d, IReg(Cur.Src2), FReg(Cur.Dest), apc, Cur.BoundsSafe)
        else
          AotArrAccess(False, False, d, IReg(Cur.Src2), IReg(Cur.Dest), apc, Cur.BoundsSafe);
      end;
      ssaArrayStore:
      begin
        d := ArrId; if not OK then Exit;
        apc := -1;
        if ArrClassic and not Cur.BoundsSafe then begin apc := NeedPC; if not OK then Exit; end;
        if SSAProg.GetArray(d).ElementType = srtFloat then
          AotArrAccess(True, True, d, IReg(Cur.Src2), FReg(Cur.Dest), apc, Cur.BoundsSafe)
        else
          AotArrAccess(False, True, d, IReg(Cur.Src2), IReg(Cur.Dest), apc, Cur.BoundsSafe);
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

var
  b, j, k, w, TargetOff: Integer;
  Blk: TSSABasicBlock;
begin
  Result := nil;
  LabelIdx := nil;
  BailWhy := '';
  OK := True;
  ArrClassic := not AllowUnsafe;
  HasRecMark := False; HasDeopt := False; HasHelperCall := False; NHelperCalls := 0;
  RecMarkRoutable := True;
  MaxIReg := -1; MaxFReg := -1; MaxArrId := -1;
  SetLength(IUse, 16); SetLength(FUse, 16); SetLength(AUse, 8);
  NFix := 0; NIAlloc := 0; NFAlloc := 0;
  NACache := 0;
  SetLength(ArrCountNeeded, 0); SetLength(ArrCountNeeded, 8);
  SetLength(ACacheId, Length(IntPool));
  SetLength(ACacheKind, Length(IntPool));
  SetLength(ACacheReg, Length(IntPool));
  SaveX6 := False; SaveX7 := False;
  FillChar(SaveGpr, SizeOf(SaveGpr), 0);

  CurOrd := Region.FirstOrdinal;   // Prescan uses its own ordinal; keep for NeedPC in emission
  Prescan;
  if not OK then Exit;

  // C1: liveness. Computed here, consumed from C3 on (helper-call spilling) - it must not
  // change a single emitted byte today.
  LivenessOK := False; PeakLiveInt := 0; PeakLiveFloat := 0;
  ComputeLiveness;

  SetLength(ILoc, MaxIReg + 1); for k := 0 to MaxIReg do ILoc[k] := -1;
  SetLength(FLoc, MaxFReg + 1); for k := 0 to MaxFReg do FLoc[k] := -1;
  SetLength(IAllocd, Length(IntPool)); SetLength(FAllocd, 6);
  Allocate;
  PlanDynFloat;   // AOT_DYNF: may replace the static float homes with a within-block dynamic schedule
  PlanDynInt;     // AOT_DYNF (c): same for the integer GPR pool (minus the array-descriptor cache)

  HelperOps := TStringList.Create;

  // Region-local label -> block-list index (see the jump case for why not BlockIndex).
  LabelIdx := TStringList.Create;
  LabelIdx.Sorted := True;
  LabelIdx.Duplicates := dupIgnore;
  for k := Region.FirstBlock to Region.LastBlock do
    if SSAProg.Blocks[k].LabelName <> '' then
      LabelIdx.AddObject(SSAProg.Blocks[k].LabelName, TObject(PtrInt(k)));

  E := TX86Emitter.Create;
  try
    SetLength(BlockOff, SSAProg.Blocks.Count);
    for k := 0 to High(BlockOff) do BlockOff[k] := -1;

    // Frame layout. A region with no helper call stays a leaf and keeps exactly the frame it
    // has always had (nothing, or 16 bytes for xmm6/7) - the validated codegen must not move
    // because of a feature it does not use. A region that DOES call needs, on top of that,
    // the callee's shadow space, a slot for each base register the ABI lets a callee clobber,
    // and enough padding that rsp is 16-byte aligned at the call.
    if HasHelperCall then FrameSize := ABI_SHADOW_SPACE else FrameSize := 0;
    SlotXmm := -1; SlotCtxSave := -1; SlotFltSave := -1;
    if SaveX6 or SaveX7 then begin SlotXmm := FrameSize; Inc(FrameSize, 16); end;
    if HasHelperCall then
    begin
      SlotCtxSave := FrameSize; SlotFltSave := FrameSize + 8; Inc(FrameSize, 16);
      // rsp is 8 (mod 16) on entry and moves by 8 per push; pad so the `call` sees 0.
      k := 2; for b := R12 to R15 do if SaveGpr[b] then Inc(k);
      Inc(FrameSize, ((8 + 8 * k) - FrameSize) mod 16);
    end;

    // Prologue (Win64: rcx=IntRegs rdx=FloatRegs r8=AotCtx; SysV: rdi/rsi/rdx).
    E.Emit8($53);                                    // push rbx
    E.Emit8($56);                                    // push rsi
    for k := R12 to R15 do
      if SaveGpr[k] then begin E.Emit8($41); E.Emit8($54 + (k - R12)); end;
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
      if SaveX6 then FrameXmm(True, 6, SlotXmm);
      if SaveX7 then FrameXmm(True, 7, SlotXmm + 8);
      if HasHelperCall then
      begin
        FrameStore(R8, SlotCtxSave);                 // the ctx pointer: r8 is volatile everywhere
        FrameStore(RSI, SlotFltSave);                // the FloatRegs base: rsi is volatile in SysV
      end;
    end;
    // Entry loads of the allocated registers.
    for k := 0 to NIAlloc - 1 do
      LoadRegMem(ILoc[IAllocd[k]], LongWord(IAllocd[k]) * 8);
    for k := 0 to NFAlloc - 1 do
      E.MemOp([$F2, $0F, $10], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
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
        end;
        if DynIActive and (DynIHomeReg[DynPos] >= 0) then
        begin
          ILoc[DynIHomeReg[DynPos]] := DynIHomeGpr[DynPos];
          DynICur[DynIHomeGpr[DynPos]] := DynIHomeReg[DynPos];
        end;
        EmitInstruction;
        if not OK then Exit;
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
        Inc(DynPos);
        Inc(CurOrd);
      end;
    end;

    // Epilogue: rax already holds the exit PC; flush allocated regs and return.
    EpiOff := E.Len;
    for k := 0 to NIAlloc - 1 do
      StoreRegMem(ILoc[IAllocd[k]], LongWord(IAllocd[k]) * 8);
    for k := 0 to NFAlloc - 1 do
      E.MemOp([$F2, $0F, $11], FLoc[FAllocd[k]], RSI, LongWord(FAllocd[k]) * 8);
    // B3 bare epilogue: same teardown, no flush (see the declaration comment).
    BareEpiOff := E.Len;
    if FrameSize > 0 then
    begin
      if SaveX6 then FrameXmm(False, 6, SlotXmm);
      if SaveX7 then FrameXmm(False, 7, SlotXmm + 8);
      E.EmitBytes([$48, $81, $C4]); E.Emit32(LongWord(FrameSize));  // add rsp, FrameSize
    end;
    for k := R15 downto R12 do
      if SaveGpr[k] then begin E.Emit8($41); E.Emit8($5C + (k - R12)); end;
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
    Result := TExecMem.Create(E);
    if Result.Ptr = nil then begin FreeAndNil(Result); Fail('exec-alloc'); end;
  finally
    E.Free;
    LabelIdx.Free;
    HelperOps.Free;
    if (Result = nil) and (BailWhy = '') then BailWhy := 'unknown';
  end;
end;

function AotCompileProgram(SSAProg: TSSAProgram; Prog: TBytecodeProgram;
                           TrueVal: Int64; AllowUnsafe, Diag, SkipMain: Boolean): TAotFuncs;
var
  Regions: TAotRegions;
  r, n: Integer;
  Mem: TExecMem;
  Why: string;
begin
  Result := nil;
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
        if AotDiagFloatTotal > 0 then
          WriteLn(ErrOutput, Format('[AOT]   float traffic: static-resident=%d (%.1f%% mem) -> linscan-resident=%d (%.1f%% mem)  recovers %.1f%% of tail',
            [AotDiagFloatResident, 100.0 * (AotDiagFloatTotal - AotDiagFloatResident) / AotDiagFloatTotal,
             AotDiagFloatLinScan, 100.0 * (AotDiagFloatTotal - AotDiagFloatLinScan) / AotDiagFloatTotal,
             100.0 * (AotDiagFloatLinScan - AotDiagFloatResident) / (AotDiagFloatTotal - AotDiagFloatResident + 0.0001)]));
        if AotDiagFloatTotal > 0 then
          WriteLn(ErrOutput, Format('[AOT]   float block-local temps: %d slots, use=%d (%.1f%% of total) - the low-risk hybrid ceiling',
            [AotDiagFloatBlockLocalCount, AotDiagFloatBlockLocal,
             100.0 * AotDiagFloatBlockLocal / AotDiagFloatTotal]));
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
