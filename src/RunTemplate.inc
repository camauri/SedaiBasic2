{*
 * RunTemplate.inc - Unified VM execution loop template
 *
 * This file is included by both RunFast and RunDebug methods.
 * DEBUG_MODE define controls whether debug/profiling code is included.
 *
 * When DEBUG_MODE is defined:
 *   - TRON trace output (line numbers)
 *   - Profiler hooks (BeforeInstruction/AfterInstruction)
 *   - bcTron switches to RunDebug, bcTroff switches to RunFast
 *
 * When DEBUG_MODE is NOT defined:
 *   - Pure speed, no debug overhead
 *   - bcTron switches to RunDebug, bcTroff does nothing (already fast)
 *}

type
  PBytecodeInstruction = ^TBytecodeInstruction;
  PExecMemA = ^TExecMem;
var
  JitOn: Boolean;          // JIT enabled for this run (compiled loops present)
  JitLoops: PExecMemA;     // @FNativeLoops[0]: JitLoops[CurPC] is the compiled loop at that header, or nil
  // AOT (plan B): whole compiled functions keyed by entry PC. The check fires on the dispatch
  // iteration after bcCallSub has done FramePush, so the native function needs no frame logic.
  AotOn: Boolean;
  AotFuncs: PExecMemA;     // @FNativeFuncs[0]
  LocalAotCtx: TAotCtx;    // per-Run transfer-bank bases of the ACTIVE context (worker-safe)
  Instructions: PBytecodeInstruction;
  InstrCount: Integer;
  CurPC: Integer;  // Current Program Counter (local for speed)
  Instr: TBytecodeInstruction;
  Ctx: TExecutionContext;  // M5.2: active execution context, read once per Run (zero hot-path cost)
  Op: Word;
  // M2 decode-once dense dispatch: DenseOps[CurPC] is the instruction's dense linear index (built once
  // by EnsureDenseOps). The loop dispatches on it with a single flat case -- no per-instruction group
  // extraction and no separate superinstruction branch (the alternation that hurt float-heavy loops).
  DenseOp: Integer;
  DenseOps: PWord;
  // Local copies of frequently accessed fields for better register allocation
  IntRegs: PInt64;
  FloatRegs: PDouble;
  ArrayIdx, LinearIdx, ArrLowerBound: Integer;
  InputStr, PrintStr: string;
  InputVal: Double;
  i, ProdDims: Integer;
  NextTabCol, TabIdx: Integer;
  ArrInfo: TSSAArrayInfo;
  // Local vars for ArrayReverseRange and ArrayShiftLeft
  ArrIdxW: Word;
  LStartIdx, LEndIdx, LLoopIdx: Integer;
  LFirstVal: Int64;
  LElemVal: Double;  // scratch for bounds-guarded array element reads in superinstructions
  // Local copy of TrueValue for comparison operators
  TrueVal: Int64;
  // Dialect of the implicit float->int conversion, read once (MODERN rounds, CLASSIC truncates)
  ModernConv: Boolean;
  // Flag for TRAP handler jump (can't use goto from except block)
  TrapTriggered: Boolean;
  // Event polling counter
  EventPollCounter: Integer;
  {$IFDEF JIT_PROFILE}
  // J1: local copy of the JIT back-edge-profiling flag; BEBase points at FBackEdgeCount[0].
  BEProfile: Boolean;
  BEBase: PInteger;
  {$ENDIF}
  {$IFDEF DEBUG_MODE}
  CurSourceLine: Integer;  // Current source line from Source Map
  {$ENDIF}
begin
  if FProgram = nil then raise Exception.Create('No program loaded');

  Instructions := PBytecodeInstruction(FProgram.GetInstructionsPtr);
  if Instructions = nil then raise Exception.Create('Empty program');

  InstrCount := FProgram.GetInstructionCount;
  // M2: ensure the decode-once dense-opcode table matches this program, then bind it register-direct.
  EnsureDenseOps;
  if Length(FDenseOps) > 0 then DenseOps := @FDenseOps[0] else DenseOps := nil;
  // JIT (J2/J3): bind the compiled-loop table. When a loop header PC has a native loop, the interpreter
  // calls it (runs every iteration natively) and continues at the returned exit PC.
  // Compiled loops are context-relative since the AOT⊃JIT unification step: the executing
  // context is the native function's 4th argument and all per-context state (Xfer banks,
  // record heap) is read through it - no baked addresses. Workers may enter them like the
  // main thread (the old "main context only" gate for the m451 corruption is gone).
  JitOn := FJitEnabled and (Length(FNativeLoops) > 0);
  if JitOn then JitLoops := @FNativeLoops[0] else JitLoops := nil;
  {$IFDEF JIT_PROFILE}
  // J1: bind the back-edge profiler (counting is a no-op unless FJitProfile is set).
  BEProfile := FJitProfile;
  if BEProfile and (Length(FBackEdgeCount) > 0) then BEBase := @FBackEdgeCount[0] else BEBase := nil;
  {$ENDIF}
  // M5.2: bind the active execution context once per Run — a worker thread supplies its own via
  // the GActiveCtx threadvar (set by WorkerThreadEntry before calling RunFast); the main thread
  // leaves it nil and uses FCtx. A single read per Run keeps per-instruction access register-direct.
  if GActiveCtx <> nil then Ctx := GActiveCtx else Ctx := FCtx;
  // The main context starts at the program EntryPoint (StartPC = -1); a worker starts at the entry
  // PC of the SUB it runs (StartPC set in RunWorker), with a synthetic call frame already primed.
  // A pending TRON/TROFF mode switch overrides both: the other run loop handed control over
  // mid-program and execution must CONTINUE, not restart (one-shot, consumed here).
  if Ctx.ModeSwitchPC >= 0 then
  begin
    CurPC := Ctx.ModeSwitchPC;
    Ctx.ModeSwitchPC := -1;
  end
  else if Ctx.StartPC >= 0 then CurPC := Ctx.StartPC else CurPC := FProgram.EntryPoint;
  Ctx.Running := True;  // Mark VM as running
  Ctx.AotRaisePC := -1; // no AOT-raised exception pending (0 would read as "PC 0")
  // M5.3: the main thread is the render owner (only it may touch the SDL device). A worker
  // (GActiveCtx <> nil) must never steal ownership; its graphics ops defer to the owner's draw queue.
  if GActiveCtx = nil then
    FRenderOwnerThreadId := GetCurrentThreadID;

  // Get direct pointers to register arrays
  IntRegs := @Ctx.IntRegs[0];
  FloatRegs := @Ctx.FloatRegs[0];
  // AOT: bind the compiled-function table and this context's transfer-bank bases (built per
  // Run and passed per call, NEVER baked into code: a worker must see its own banks).
  AotOn := FAotEnabled and (Length(FNativeFuncs) > 0) and
           (Length(Ctx.XferInt) > 0) and (Length(Ctx.XferFloat) > 0);
  if AotOn then
  begin
    AotFuncs := @FNativeFuncs[0];
    LocalAotCtx.XferInt := @Ctx.XferInt[0];
    LocalAotCtx.XferFloat := @Ctx.XferFloat[0];
    // C3: the runtime-helper triple. Compiled code calls back into the interpreter for ops
    // with no native form, and must reach THIS run's VM/context pair - hence per-Run here
    // rather than baked into the code (a worker executes the same bytes on its own context).
    LocalAotCtx.ExecOne := @AotExecOne;
    LocalAotCtx.VMSelf := Self;
    LocalAotCtx.CtxObj := Ctx;
    // C5: native string lowering. The string bank base (stable for the run) and the leaf
    // primitives the compiled code calls directly for the hot string ops.
    if Length(Ctx.StringRegs) > 0 then LocalAotCtx.StrRegs := @Ctx.StringRegs[0]
    else LocalAotCtx.StrRegs := nil;
    LocalAotCtx.StrCmp := @AotStrCmp;
    LocalAotCtx.StrAssign := @AotStrAssign;
    LocalAotCtx.StrLoadConst := @AotStrLoadConst;
    LocalAotCtx.StrConcat := @AotStrConcat;
    LocalAotCtx.StrLen := @AotStrLen;
    LocalAotCtx.CallSub := @AotCallSub;   // B3: native call site primitive
    // C5 residuals. StrMid is dialect-variant: install the flavor once per run.
    LocalAotCtx.StrLeft := @AotStrLeft;
    LocalAotCtx.StrRight := @AotStrRight;
    if Assigned(FProgram) and FProgram.ModernMode then
      LocalAotCtx.StrMid := @AotStrMidModern
    else
      LocalAotCtx.StrMid := @AotStrMidClassic;
    LocalAotCtx.StrAsc := @AotStrAsc;
    LocalAotCtx.StrChr := @AotStrChr;
    LocalAotCtx.StrInstr := @AotStrInstr;
    LocalAotCtx.StrIntToStr := @AotIntToString;
    LocalAotCtx.StrVal := @AotStrVal;
    LocalAotCtx.StrValInt := @AotStrValInt;
  end
  else
    AotFuncs := nil;
  // Local copy of TrueValue for comparison operators
  TrueVal := FTrueValue;
  ModernConv := Assigned(FProgram) and FProgram.ModernMode;
  // Initialize event polling counter
  EventPollCounter := 0;

  {$IFDEF DEBUG_MODE}
  {$IFDEF ENABLE_PROFILER}
  // Start profiler if attached
  if Assigned(FProfiler) then
    FProfiler.Start;
  {$ENDIF}
  {$ENDIF}

  // Main execution loop - disable range checking for speed
  {$PUSH}
  {$R-}  // Range checking off
  {$Q-}  // Overflow checking off

  repeat
    TrapTriggered := False;
    try
    while Ctx.Running and (CurPC < InstrCount) do
  begin
    // JIT (J2/J3): a compiled loop whose header is here runs entirely in native code; it reads/writes
    // the same register banks, then returns the exit PC. (One predicted-not-taken branch when JIT is off.)
    if JitOn and (JitLoops[CurPC] <> nil) then
    begin
      if FArraysDirty then RebuildJitArrDesc;
      CurPC := TNativeLoopFn(TExecMem(JitLoops[CurPC]).Ptr)(IntRegs, PInt64(FloatRegs), @FJitArrDesc[0],
                                                            Pointer(Ctx));
      System.Continue;
    end;

    // AOT (plan B): a whole compiled function registered at this entry PC runs natively on the
    // same banks and returns the resume PC (its bcReturnSub/bcEnd, or a deopt PC). The array
    // descriptor pointer is refreshed per call (the rebuild may reallocate the table).
    if AotOn and (AotFuncs[CurPC] <> nil) then
    begin
      if FArraysDirty then RebuildJitArrDesc;
      if Length(FJitArrDesc) > 0 then LocalAotCtx.ArrDesc := @FJitArrDesc[0]
      else LocalAotCtx.ArrDesc := nil;
      // AotSettle resolves the C3 sentinels, out of line on purpose: this is the interpreter's
      // hot loop, and its speed is measurably sensitive to code layout - see
      // job/docs/PIANO_B1_AOT_DESIGN.md §5.7 - so it stays as small as it was.
      CurPC := AotSettle(Ctx,
        TNativeFuncFn(TExecMem(AotFuncs[CurPC]).Ptr)(IntRegs, PInt64(FloatRegs), @LocalAotCtx));
      System.Continue;
    end;

    Instr := Instructions[CurPC];
    Op := Instr.OpCode;
    DenseOp := DenseOps[CurPC];

    {$IFDEF DEBUG_MODE}
    // Debugger integration: check for breakpoints and stepping
    // SourceLine now retrieved from Source Map
    CurSourceLine := FProgram.GetSourceLine(CurPC);
    if Assigned(FDebugger) and FDebugger.IsActive and (CurSourceLine > 0) then
    begin
      // Notify debugger of line change (handles trace output internally)
      FDebugger.NotifyLineChange(CurSourceLine);

      // Check if debugger wants to pause (breakpoint or step)
      if FDebugger.IsPaused then
      begin
        // Save state and exit loop - console will handle debug commands
        Ctx.PC := CurPC;
        Ctx.Running := False;
        Break;
      end;
    end
    else if Ctx.TraceActive and (CurSourceLine > 0) and (CurSourceLine <> Ctx.LastSourceLine) then
    begin
      // Legacy TRON trace output (when no debugger attached)
      Ctx.LastSourceLine := CurSourceLine;
      if Assigned(FOutputDevice) then
        FOutputDevice.Print('[' + IntToStr(CurSourceLine) + ']');
    end;

    {$IFDEF ENABLE_PROFILER}
    // Profiler: BeforeInstruction hook
    if Assigned(FProfiler) and FProfiler.Enabled then
      FProfiler.BeforeInstruction(CurPC, Op);
    {$ENDIF}
    {$ENDIF}

    {$IFDEF ENABLE_INSTRUCTION_COUNTING}
    Inc(FInstructionsExecuted);
    {$ENDIF}

    // Periodic event polling via callback - keeps UI responsive. M5.2: only the main thread
    // (GActiveCtx = nil) polls — the callback touches SDL/input, which is not thread-safe.
    if Assigned(FEventPollCallback) and (GActiveCtx = nil) then
    begin
      Inc(EventPollCounter);
      if EventPollCounter >= FEventPollInterval then
      begin
        EventPollCounter := 0;
        if FEventPollCallback() then
        begin
          Ctx.Running := False;
          Break;
        end;
      end;
    end;

    // M2 flat dispatch: ONE case on the dense linear index (DenseOps built once by EnsureDenseOps).
    // No per-instruction group extraction and no separate superinstruction branch -- the two big case
    // statements and the alternation between them are gone; each family is a contiguous dense range.
    // The group-0 and superinstruction BODIES below are byte-identical to the previous nested cases.
    case DenseOp of
      // === Superinstructions (dense DENSE_SUPER_BASE + sub, sub = Op and $FF) ===
      DENSE_SUPER_BASE + 0 .. DENSE_SUPER_BASE + 255:
      case Op and $FF of
        // Fused compare-and-branch (Int) - sub-opcodes 0-5
        0: if IntRegs[Instr.Src1] = IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);  // bcCmpBranchEqInt
        1: if IntRegs[Instr.Src1] <> IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); // bcCmpBranchNeInt
        2: if IntRegs[Instr.Src1] < IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);  // bcCmpBranchLtInt
        3: if IntRegs[Instr.Src1] > IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);  // bcCmpBranchGtInt
        4: if IntRegs[Instr.Src1] <= IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); // bcCmpBranchLeInt
        5: if IntRegs[Instr.Src1] >= IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); // bcCmpBranchGeInt

        // Fused compare-and-branch (Float) - sub-opcodes 10-15
        10: if FloatRegs[Instr.Src1] = FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);  // bcCmpBranchEqFloat
        11: if FloatRegs[Instr.Src1] <> FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); // bcCmpBranchNeFloat
        12: if FloatRegs[Instr.Src1] < FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);  // bcCmpBranchLtFloat
        13: if FloatRegs[Instr.Src1] > FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);  // bcCmpBranchGtFloat
        14: if FloatRegs[Instr.Src1] <= FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); // bcCmpBranchLeFloat
        15: if FloatRegs[Instr.Src1] >= FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); // bcCmpBranchGeFloat

        // Fused arithmetic-to-dest (Int) - sub-opcodes 20-22
        20: begin IntRegs[Instr.Dest] := IntRegs[Instr.Dest] + IntRegs[Instr.Src1]; Inc(CurPC); end; // bcAddIntTo
        21: begin IntRegs[Instr.Dest] := IntRegs[Instr.Dest] - IntRegs[Instr.Src1]; Inc(CurPC); end; // bcSubIntTo
        22: begin IntRegs[Instr.Dest] := IntRegs[Instr.Dest] * IntRegs[Instr.Src1]; Inc(CurPC); end; // bcMulIntTo

        // Fused arithmetic-to-dest (Float) - sub-opcodes 30-33
        30: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] + FloatRegs[Instr.Src1]; Inc(CurPC); end; // bcAddFloatTo
        31: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] - FloatRegs[Instr.Src1]; Inc(CurPC); end; // bcSubFloatTo
        32: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] * FloatRegs[Instr.Src1]; Inc(CurPC); end; // bcMulFloatTo
        33: begin // bcDivFloatTo
          if Abs(FloatRegs[Instr.Src1]) < 1e-300 then
            FloatRegs[Instr.Dest] := DivZeroFloat(FloatRegs[Instr.Dest])   // MODERN: IEEE Inf/NaN; CLASSIC: error
          else
            FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] / FloatRegs[Instr.Src1];
          Inc(CurPC);
        end;

        // Fused constant arithmetic (Int) - sub-opcodes 40-42
        40: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1] + Instr.Immediate; Inc(CurPC); end; // bcAddIntConst
        41: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1] - Instr.Immediate; Inc(CurPC); end; // bcSubIntConst
        42: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1] * Instr.Immediate; Inc(CurPC); end; // bcMulIntConst

        // Fused constant arithmetic (Float) - sub-opcodes 50-53
        50: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] + Double(Pointer(@Instr.Immediate)^); Inc(CurPC); end; // bcAddFloatConst
        51: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] - Double(Pointer(@Instr.Immediate)^); Inc(CurPC); end; // bcSubFloatConst
        52: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] * Double(Pointer(@Instr.Immediate)^); Inc(CurPC); end; // bcMulFloatConst
        53: begin // bcDivFloatConst
          if Abs(Double(Pointer(@Instr.Immediate)^)) < 1e-300 then
            FloatRegs[Instr.Dest] := DivZeroFloat(FloatRegs[Instr.Src1])   // MODERN: IEEE Inf/NaN; CLASSIC: error
          else
            FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] / Double(Pointer(@Instr.Immediate)^);
          Inc(CurPC);
        end;

        // Fused compare-zero-and-branch (Int) - sub-opcodes 60-61
        60: if IntRegs[Instr.Src1] = 0 then CurPC := Instr.Immediate else Inc(CurPC);  // bcBranchEqZeroInt
        61: if IntRegs[Instr.Src1] <> 0 then CurPC := Instr.Immediate else Inc(CurPC); // bcBranchNeZeroInt

        // Fused compare-zero-and-branch (Float) - sub-opcodes 70-71
        70: if FloatRegs[Instr.Src1] = 0.0 then CurPC := Instr.Immediate else Inc(CurPC);  // bcBranchEqZeroFloat
        71: if FloatRegs[Instr.Src1] <> 0.0 then CurPC := Instr.Immediate else Inc(CurPC); // bcBranchNeZeroFloat

        // Fused array-store-constant - sub-opcodes 80-82. Bounds-guarded: OOB store dropped (MODERN), CLASSIC raises.
        80: begin if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) then FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] := Instr.Immediate; Inc(CurPC); end; // bcArrayStoreIntConst
        81: begin if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) then FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]] := Double(Pointer(@Instr.Immediate)^); Inc(CurPC); end; // bcArrayStoreFloatConst
        82: begin if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) then FArrays[Instr.Src1].StringData[IntRegs[Instr.Src2]] := FProgram.StringConstants[Instr.Immediate]; Inc(CurPC); end; // bcArrayStoreStringConst

        // Fused loop increment-and-branch (most common in hot loops) - sub-opcodes 90-93
        90: // bcAddIntToBranchLe
          begin
            Inc(IntRegs[Instr.Dest], IntRegs[Instr.Src1]);
            if IntRegs[Instr.Dest] <= IntRegs[Instr.Src2] then
              CurPC := Instr.Immediate
            else
              Inc(CurPC);
          end;
        91: // bcAddIntToBranchLt
          begin
            Inc(IntRegs[Instr.Dest], IntRegs[Instr.Src1]);
            if IntRegs[Instr.Dest] < IntRegs[Instr.Src2] then
              CurPC := Instr.Immediate
            else
              Inc(CurPC);
          end;
        92: // bcSubIntToBranchGe
          begin
            Dec(IntRegs[Instr.Dest], IntRegs[Instr.Src1]);
            if IntRegs[Instr.Dest] >= IntRegs[Instr.Src2] then
              CurPC := Instr.Immediate
            else
              Inc(CurPC);
          end;
        93: // bcSubIntToBranchGt
          begin
            Dec(IntRegs[Instr.Dest], IntRegs[Instr.Src1]);
            if IntRegs[Instr.Dest] > IntRegs[Instr.Src2] then
              CurPC := Instr.Immediate
            else
              Inc(CurPC);
          end;

        // FMA (Fused Multiply-Add) - sub-opcodes 100-103
        // Format: Dest, Src1=a, Src2=b, Immediate=c (extra register)
        100: begin // bcMulAddFloat: dest = c + a*b
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate] + FloatRegs[Instr.Src1] * FloatRegs[Instr.Src2];
          Inc(CurPC);
        end;
        101: begin // bcMulSubFloat: dest = c - a*b
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate] - FloatRegs[Instr.Src1] * FloatRegs[Instr.Src2];
          Inc(CurPC);
        end;
        102: begin // bcMulAddToFloat: dest += a*b
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] + FloatRegs[Instr.Src1] * FloatRegs[Instr.Src2];
          Inc(CurPC);
        end;
        103: begin // bcMulSubToFloat: dest -= a*b
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] - FloatRegs[Instr.Src1] * FloatRegs[Instr.Src2];
          Inc(CurPC);
        end;

        // Array Load + Arithmetic - sub-opcodes 110-112
        // Format: Dest, Src1=arr_index, Src2=idx_reg, Immediate=acc_reg
        // Bounds-guarded: an OOB read yields the element default 0.0 (MODERN); CLASSIC raises.
        110: begin // bcArrayLoadAddFloat: dest = acc + arr[idx]
          if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) then
            FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate] + FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]]
          else
            FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate];
          Inc(CurPC);
        end;
        111: begin // bcArrayLoadSubFloat: dest = acc - arr[idx]
          if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) then
            FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate] - FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]]
          else
            FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate];
          Inc(CurPC);
        end;
        112: begin // bcArrayLoadDivAddFloat: dest = acc + arr[idx] / denom
          // Extra encodes: acc_reg (low 16) + denom_reg (high 16)
          if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) then
            LElemVal := FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]]
          else
            LElemVal := 0.0;
          if Abs(FloatRegs[(Instr.Immediate shr 16) and $FFFF]) < 1e-300 then
            FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate and $FFFF] +
              DivZeroFloat(LElemVal)   // MODERN: IEEE; CLASSIC: error
          else
            FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate and $FFFF] +
              LElemVal / FloatRegs[(Instr.Immediate shr 16) and $FFFF];
          Inc(CurPC);
        end;

        // Square-Sum patterns - sub-opcodes 120-121
        120: begin // bcSquareSumFloat: dest = x*x + y*y
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] * FloatRegs[Instr.Src1] +
                                   FloatRegs[Instr.Src2] * FloatRegs[Instr.Src2];
          Inc(CurPC);
        end;
        121: begin // bcAddSquareFloat: dest = sum + x*x
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] + FloatRegs[Instr.Src2] * FloatRegs[Instr.Src2];
          Inc(CurPC);
        end;

        // Mul-Mul and Add-Sqrt - sub-opcodes 130-131
        130: begin // bcMulMulFloat: dest = a*b*c
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] * FloatRegs[Instr.Src2] * FloatRegs[Instr.Immediate];
          Inc(CurPC);
        end;
        131: begin // bcAddSqrtFloat: dest = sqrt(a+b)
          FloatRegs[Instr.Dest] := SqrtFloat(FloatRegs[Instr.Src1] + FloatRegs[Instr.Src2]);
          Inc(CurPC);
        end;

        // Array Load + Branch - sub-opcodes 140-141. Bounds-guarded: an OOB read is treated as the element
        // default 0 in MODERN (NZ does not branch, Z branches); CLASSIC raises.
        140: // bcArrayLoadIntBranchNZ: if arr[idx] <> 0 goto target
          if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) and
             (FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] <> 0) then
            CurPC := Instr.Immediate
          else
            Inc(CurPC);
        141: // bcArrayLoadIntBranchZ: if arr[idx] = 0 goto target
          if (not ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2])) or
             (FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] = 0) then
            CurPC := Instr.Immediate
          else
            Inc(CurPC);

        // Array Reverse Range - sub-opcode 156
        156: // bcArrayReverseRange: reverse arr[start..end-1] in-place
          begin
            LStartIdx := IntRegs[Instr.Src2];
            LEndIdx := IntRegs[Instr.Dest] - 1;
            ArrIdxW := Instr.Src1;
            // Bounds-guard the contiguous range once (endpoints valid => interior valid); skip if OOB (MODERN).
            if (LStartIdx < LEndIdx) and
               (not ArrayBoundsOK(ArrIdxW, LStartIdx) or not ArrayBoundsOK(ArrIdxW, LEndIdx)) then
              LStartIdx := LEndIdx;
            while LStartIdx < LEndIdx do
            begin
              Ctx.SwapTempInt := FArrays[ArrIdxW].IntData[LStartIdx];
              FArrays[ArrIdxW].IntData[LStartIdx] := FArrays[ArrIdxW].IntData[LEndIdx];
              FArrays[ArrIdxW].IntData[LEndIdx] := Ctx.SwapTempInt;
              Inc(LStartIdx);
              Dec(LEndIdx);
            end;
            Inc(CurPC);
          end;

        // Array Shift Left - sub-opcode 157
        157: // bcArrayShiftLeft: shift left and rotate first to end+1
          begin
            LStartIdx := IntRegs[Instr.Src2];
            LEndIdx := IntRegs[Instr.Dest];
            ArrIdxW := Instr.Src1;
            // Bounds-guard the touched range [start .. end+1] once; skip the whole rotate if OOB (MODERN).
            if ArrayBoundsOK(ArrIdxW, LStartIdx) and ArrayBoundsOK(ArrIdxW, LEndIdx + 1) then
            begin
              LFirstVal := FArrays[ArrIdxW].IntData[LStartIdx];
              LLoopIdx := LStartIdx;
              while LLoopIdx <= LEndIdx do
              begin
                FArrays[ArrIdxW].IntData[LLoopIdx] := FArrays[ArrIdxW].IntData[LLoopIdx + 1];
                Inc(LLoopIdx);
              end;
              FArrays[ArrIdxW].IntData[LEndIdx + 1] := LFirstVal;
            end;
            Inc(CurPC);
          end;

        // Array Swap (Int) - sub-opcode 250. Bounds-guarded: skip if either index OOB (MODERN); CLASSIC raises.
        250: // bcArraySwapInt: swap arr[idx1] and arr[idx2]
          begin
            if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) and
               ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Dest]) then
            begin
              Ctx.SwapTempInt := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]];
              FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Dest]];
              FArrays[Instr.Src1].IntData[IntRegs[Instr.Dest]] := Ctx.SwapTempInt;
            end;
            Inc(CurPC);
          end;

        // Self-increment/decrement (Int) - sub-opcodes 251-252
        251: begin Inc(IntRegs[Instr.Dest], IntRegs[Instr.Src1]); Inc(CurPC); end; // bcAddIntSelf
        252: begin Dec(IntRegs[Instr.Dest], IntRegs[Instr.Src1]); Inc(CurPC); end; // bcSubIntSelf

        // Array Load to register (Int) - sub-opcode 253. Bounds-guarded: OOB read = default 0 (MODERN); CLASSIC raises.
        253: begin
            if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) then
              IntRegs[Instr.Dest] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]]
            else
              IntRegs[Instr.Dest] := 0;
            Inc(CurPC);
          end; // bcArrayLoadIntTo

        // Array Copy Element - sub-opcode 254. Bounds-guarded: OOB store dropped, OOB source reads 0 (MODERN); CLASSIC raises.
        254: begin
            if ArrayBoundsOK(Instr.Dest, IntRegs[Instr.Src2]) then
            begin
              if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) then
                FArrays[Instr.Dest].IntData[IntRegs[Instr.Src2]] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]]
              else
                FArrays[Instr.Dest].IntData[IntRegs[Instr.Src2]] := 0;
            end;
            Inc(CurPC);
          end; // bcArrayCopyElement

        // Array Move Element - sub-opcode 255. Bounds-guarded like 254.
        255: begin
            if ArrayBoundsOK(Instr.Dest, IntRegs[Instr.Src2]) then
            begin
              if ArrayBoundsOK(Instr.Dest, IntRegs[Instr.Src1]) then
                FArrays[Instr.Dest].IntData[IntRegs[Instr.Src2]] := FArrays[Instr.Dest].IntData[IntRegs[Instr.Src1]]
              else
                FArrays[Instr.Dest].IntData[IntRegs[Instr.Src2]] := 0;
            end;
            Inc(CurPC);
          end; // bcArrayMoveElement

      else
        raise Exception.CreateFmt('Unknown superinstruction sub-opcode %d (full opcode $%.4X) at CurPC=%d', [Op and $FF, Op, CurPC]);
      end;

      // === Delegated families (each a contiguous dense range) ===
      DENSE_STRING_BASE + 0 .. DENSE_STRING_BASE + 48:
        begin Ctx.PC := CurPC; ExecuteStringOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
      DENSE_MATH_BASE + 0 .. DENSE_MATH_BASE + 35:
        begin Ctx.PC := CurPC; ExecuteMathOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
      // Group 3 (arrays). The ELEMENT-ACCESS opcodes (sub 3..8) run INLINE here, from the same source
      // text the handler uses (ArrayHotOps.inc). Everything else in group 3 is rare and still delegates.
      DENSE_ARRAY_BASE + 3 .. DENSE_ARRAY_BASE + 8:
        begin
          // The shared branches never touch the PC, so this path advances it itself.
          case Op and $FF of
            {$I ArrayHotOps.inc}
          end;
          Inc(CurPC);
        end;
      DENSE_ARRAY_BASE + 0 .. DENSE_ARRAY_BASE + 2, DENSE_ARRAY_BASE + 9 .. DENSE_ARRAY_BASE + 51:
        // Cold array ops (DIM/REDIM/ERASE/BIND/...) may resize or move an array's backing store; the
        // descriptor rebuild is triggered inside ExecuteArrayOp itself (it has two callers now - here
        // and the AOT helper - so the flag lives with the op, not the call site).
        begin Ctx.PC := CurPC; ExecuteArrayOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
      DENSE_IO_BASE + 0 .. DENSE_IO_BASE + 22:
        begin Ctx.PC := CurPC; ExecuteIOOp(Ctx, Instr); if not Ctx.Running then Break; CurPC := Ctx.PC + 1; end;
      DENSE_SPECIAL_BASE + 0 .. DENSE_SPECIAL_BASE + 16:
        begin Ctx.PC := CurPC; ExecuteSpecialVarOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
      DENSE_FILEIO_BASE + 0 .. DENSE_FILEIO_BASE + 27:
        begin Ctx.PC := CurPC; ExecuteFileIOOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
      DENSE_SPRITE_BASE + 0 .. DENSE_SPRITE_BASE + 16:
        begin Ctx.PC := CurPC; ExecuteSpriteOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
      {$IFDEF WEB_MODE}
      DENSE_WEB_BASE + 0 .. DENSE_WEB_BASE + 12:
        begin Ctx.PC := CurPC; ExecuteWebOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
      {$ENDIF}
      DENSE_GRAPHICS_BASE + 0 .. DENSE_GRAPHICS_BASE + 63:
        begin Ctx.PC := CurPC; ExecuteGraphicsOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
      DENSE_SOUND_BASE + 0 .. DENSE_SOUND_BASE + 5:
        begin Ctx.PC := CurPC; ExecuteSoundOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;

      // === Core VM operations (group 0, dense == 16-bit value) - inline for performance ===
      DENSE_CORE_BASE + 0 .. DENSE_CORE_BASE + 154:
          case Op of
        // Load constants - very common
        bcLoadConstInt: begin IntRegs[Instr.Dest] := Instr.Immediate; Inc(CurPC); end;
        bcLoadConstFloat: begin FloatRegs[Instr.Dest] := Double(Pointer(@Instr.Immediate)^); Inc(CurPC); end;
        bcLoadConstString: begin Ctx.StringRegs[Instr.Dest] := FProgram.StringConstants[Instr.Immediate]; Inc(CurPC); end;

        // Copy operations
        bcCopyInt: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1]; Inc(CurPC); end;
        bcCopyFloat: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1]; Inc(CurPC); end;
        bcCopyString: begin Ctx.StringRegs[Instr.Dest] := Ctx.StringRegs[Instr.Src1]; Inc(CurPC); end;

        // Integer arithmetic
        bcAddInt: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1] + IntRegs[Instr.Src2]; Inc(CurPC); end;
        bcSubInt: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1] - IntRegs[Instr.Src2]; Inc(CurPC); end;
        bcMulInt: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1] * IntRegs[Instr.Src2]; Inc(CurPC); end;
        bcDivInt: begin
          if IntRegs[Instr.Src2] = 0 then
            raise EDivByZero.Create('Integer division by zero');
          IntRegs[Instr.Dest] := IntRegs[Instr.Src1] div IntRegs[Instr.Src2];
          Inc(CurPC);
        end;
        bcModInt: begin
          if IntRegs[Instr.Src2] = 0 then
            raise EDivByZero.Create('Integer modulo by zero');
          IntRegs[Instr.Dest] := IntRegs[Instr.Src1] mod IntRegs[Instr.Src2];
          Inc(CurPC);
        end;
        bcDivUInt: begin   // unsigned 64-bit \ (QWord)
          if IntRegs[Instr.Src2] = 0 then
            raise EDivByZero.Create('Integer division by zero');
          IntRegs[Instr.Dest] := Int64(QWord(IntRegs[Instr.Src1]) div QWord(IntRegs[Instr.Src2]));
          Inc(CurPC);
        end;
        bcModUInt: begin   // unsigned 64-bit Mod (QWord)
          if IntRegs[Instr.Src2] = 0 then
            raise EDivByZero.Create('Integer modulo by zero');
          IntRegs[Instr.Dest] := Int64(QWord(IntRegs[Instr.Src1]) mod QWord(IntRegs[Instr.Src2]));
          Inc(CurPC);
        end;
        bcNegInt: begin IntRegs[Instr.Dest] := -IntRegs[Instr.Src1]; Inc(CurPC); end;

        // Float arithmetic - critical for spectral-norm
        bcAddFloat: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] + FloatRegs[Instr.Src2]; Inc(CurPC); end;
        bcSubFloat: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] - FloatRegs[Instr.Src2]; Inc(CurPC); end;
        bcMulFloat: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] * FloatRegs[Instr.Src2]; Inc(CurPC); end;
        bcDivFloat: begin
          if FloatRegs[Instr.Src2] = 0.0 then
            FloatRegs[Instr.Dest] := DivZeroFloat(FloatRegs[Instr.Src1])   // MODERN: IEEE Inf/NaN; CLASSIC: error
          else
            FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] / FloatRegs[Instr.Src2];
          Inc(CurPC);
        end;
        bcModFloat: begin
          if FloatRegs[Instr.Src2] = 0.0 then
            raise EZeroDivide.Create('Float modulo by zero');
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] - Floor(FloatRegs[Instr.Src1] / FloatRegs[Instr.Src2]) * FloatRegs[Instr.Src2];
          Inc(CurPC);
        end;
        bcPowFloat: begin FloatRegs[Instr.Dest] := Power(FloatRegs[Instr.Src1], FloatRegs[Instr.Src2]); Inc(CurPC); end;
        bcNegFloat: begin FloatRegs[Instr.Dest] := -FloatRegs[Instr.Src1]; Inc(CurPC); end;

        // Type conversions
        bcIntToFloat: begin FloatRegs[Instr.Dest] := IntRegs[Instr.Src1]; Inc(CurPC); end;
        // Implicit float -> int: MODERN rounds (to nearest, ties to even), CLASSIC truncates. See the
        // ExecuteInstruction copy for why.
        bcFloatToInt: begin IntRegs[Instr.Dest] := FloatToIntConv(FloatRegs[Instr.Src1], ModernConv); Inc(CurPC); end;
        // Numeric -> string (FreeBASIC Str() / "&" concat): no leading sign-space, unlike v7 STR$.
        bcIntToString: begin Ctx.StringRegs[Instr.Dest] := IntToStr(IntRegs[Instr.Src1]); Inc(CurPC); end;
        bcFloatToString:
          // FreeBASIC Str()/"&" concat: no leading sign-space AND no trailing field-space (FormatNumber
          // adds both under the Commodore preset). Trim both, matching the ExecuteInstruction path.
          // Immediate = 1 -> the value is SINGLE-typed: 7 significant digits, as PRINT gives it.
          begin
            Ctx.StringRegs[Instr.Dest] := Trim(FConsoleBehavior.FormatNumber(FloatRegs[Instr.Src1],
                                                                             Instr.Immediate = 1));
            Inc(CurPC);
          end;
        bcFloatRound: begin IntRegs[Instr.Dest] := Round(FloatRegs[Instr.Src1]); Inc(CurPC); end;  // CINT (round-to-even)
        bcNarrowInt: begin IntRegs[Instr.Dest] := NarrowInt64(IntRegs[Instr.Src1], Instr.Immediate); Inc(CurPC); end;  // B1.5
        bcNarrowSingle: begin FloatRegs[Instr.Dest] := Single(FloatRegs[Instr.Src1]); Inc(CurPC); end;  // B1.5

        // Integer comparisons (use TrueVal for TRUE, 0 for FALSE)
        bcCmpEqInt: begin if IntRegs[Instr.Src1] = IntRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpNeInt: begin if IntRegs[Instr.Src1] <> IntRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpLtInt: begin if IntRegs[Instr.Src1] < IntRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpGtInt: begin if IntRegs[Instr.Src1] > IntRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpLeInt: begin if IntRegs[Instr.Src1] <= IntRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpGeInt: begin if IntRegs[Instr.Src1] >= IntRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        // Unsigned 64-bit comparisons (reinterpret registers as QWord)
        bcCmpLtUInt: begin if QWord(IntRegs[Instr.Src1]) < QWord(IntRegs[Instr.Src2]) then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpGtUInt: begin if QWord(IntRegs[Instr.Src1]) > QWord(IntRegs[Instr.Src2]) then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpLeUInt: begin if QWord(IntRegs[Instr.Src1]) <= QWord(IntRegs[Instr.Src2]) then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpGeUInt: begin if QWord(IntRegs[Instr.Src1]) >= QWord(IntRegs[Instr.Src2]) then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;

        // Float comparisons (use TrueVal for TRUE, 0 for FALSE)
        bcCmpEqFloat: begin if FloatRegs[Instr.Src1] = FloatRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpNeFloat: begin if FloatRegs[Instr.Src1] <> FloatRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpLtFloat: begin if FloatRegs[Instr.Src1] < FloatRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpGtFloat: begin if FloatRegs[Instr.Src1] > FloatRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpLeFloat: begin if FloatRegs[Instr.Src1] <= FloatRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpGeFloat: begin if FloatRegs[Instr.Src1] >= FloatRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;

        // String comparisons (use TrueVal for TRUE, 0 for FALSE)
        bcCmpEqString: begin if Ctx.StringRegs[Instr.Src1] = Ctx.StringRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpNeString: begin if Ctx.StringRegs[Instr.Src1] <> Ctx.StringRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpLtString: begin if Ctx.StringRegs[Instr.Src1] < Ctx.StringRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpGtString: begin if Ctx.StringRegs[Instr.Src1] > Ctx.StringRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;

        // Math functions
        bcMathAbs: begin FloatRegs[Instr.Dest] := Abs(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathSgn: begin
          if FloatRegs[Instr.Src1] > 0 then FloatRegs[Instr.Dest] := 1
          else if FloatRegs[Instr.Src1] < 0 then FloatRegs[Instr.Dest] := -1
          else FloatRegs[Instr.Dest] := 0;
          Inc(CurPC);
        end;
        bcMathInt: begin FloatRegs[Instr.Dest] := Floor(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathSqr: begin FloatRegs[Instr.Dest] := SqrtFloat(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathSin: begin FloatRegs[Instr.Dest] := Sin(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathCos: begin FloatRegs[Instr.Dest] := Cos(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathTan: begin FloatRegs[Instr.Dest] := Tan(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathExp: begin FloatRegs[Instr.Dest] := Exp(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathLog: begin FloatRegs[Instr.Dest] := LnFloat(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathRnd: begin FloatRegs[Instr.Dest] := Random; Inc(CurPC); end;
        bcMathAtn: begin FloatRegs[Instr.Dest] := ArcTan(FloatRegs[Instr.Src1]); Inc(CurPC); end;

        // Control flow
        {$IFDEF JIT_PROFILE}
        bcJump:
          begin
            if BEProfile and (Instr.Immediate < CurPC) then Inc(BEBase[Instr.Immediate]);  // J1 back-edge
            CurPC := Instr.Immediate;
          end;
        bcJumpIfZero:
          if IntRegs[Instr.Src1] = 0 then
          begin
            if BEProfile and (Instr.Immediate < CurPC) then Inc(BEBase[Instr.Immediate]);  // J1 back-edge
            CurPC := Instr.Immediate;
          end
          else Inc(CurPC);
        bcJumpIfNotZero:
          if IntRegs[Instr.Src1] <> 0 then
          begin
            if BEProfile and (Instr.Immediate < CurPC) then Inc(BEBase[Instr.Immediate]);  // J1 back-edge
            CurPC := Instr.Immediate;
          end
          else Inc(CurPC);
        {$ELSE}
        bcJump: CurPC := Instr.Immediate;
        bcJumpIfZero: if IntRegs[Instr.Src1] = 0 then CurPC := Instr.Immediate else Inc(CurPC);
        bcJumpIfNotZero: if IntRegs[Instr.Src1] <> 0 then CurPC := Instr.Immediate else Inc(CurPC);
        {$ENDIF}
        bcCall:
          begin
            GrowCallStackIfNeeded(Ctx);
            Ctx.CallStack[Ctx.CallStackPtr] := CurPC + 1;
            Inc(Ctx.CallStackPtr);
            {$IFDEF DEBUG_MODE}
            if Assigned(FDebugger) then FDebugger.NotifyCall;
            {$ENDIF}
            CurPC := Instr.Immediate;
          end;
        bcReturn:
          begin
            Dec(Ctx.CallStackPtr);
            {$IFDEF DEBUG_MODE}
            if Assigned(FDebugger) then FDebugger.NotifyReturn;
            {$ENDIF}
            CurPC := Ctx.CallStack[Ctx.CallStackPtr];
          end;
        // SUB/FUNCTION call frames (M2): like bcCall/bcReturn but snapshot/restore the
        // register banks so the callee has its own locals and recursion works.
        bcCallSub:
          begin
            FramePush(Ctx, Instr.Immediate);   // narrow the snapshot to what this callee can clobber
            GrowCallStackIfNeeded(Ctx);
            Ctx.CallStack[Ctx.CallStackPtr] := CurPC + 1;
            Inc(Ctx.CallStackPtr);
            {$IFDEF DEBUG_MODE}
            if Assigned(FDebugger) then FDebugger.NotifyCall;
            {$ENDIF}
            CurPC := Instr.Immediate;
          end;
        bcCallSubIndirect:  // FreeBASIC function pointer: like bcCallSub but the target entry PC is in Src1 (int reg)
          if (IntRegs[Instr.Src1] and BUILTIN_FP_TAG) <> 0 then
          begin
            // @Sin etc.: compute the math op on the Double arg (xfer slot 0), write the Double result slot.
            Ctx.XferFloat[255] := ComputeBuiltinFP(IntRegs[Instr.Src1] and $FF, Ctx.XferFloat[0]);
            Inc(CurPC);
          end
          else
          begin
            // Indirect call: the entry PC is in the register, not the Immediate.
            FramePush(Ctx, IntRegs[Instr.Src1]);
            GrowCallStackIfNeeded(Ctx);
            Ctx.CallStack[Ctx.CallStackPtr] := CurPC + 1;
            Inc(Ctx.CallStackPtr);
            {$IFDEF DEBUG_MODE}
            if Assigned(FDebugger) then FDebugger.NotifyCall;
            {$ENDIF}
            CurPC := IntRegs[Instr.Src1];
          end;
        bcReturnSub:
          begin
            Dec(Ctx.CallStackPtr);
            {$IFDEF DEBUG_MODE}
            if Assigned(FDebugger) then FDebugger.NotifyReturn;
            {$ENDIF}
            CurPC := Ctx.CallStack[Ctx.CallStackPtr];
            FramePop(Ctx);
          end;
        // OS threading (M5.2, FreeBASIC API).
        bcLoadProcAddr:
          // @sub: the SUB's entry PC was resolved into Immediate at link time; load it into Dest.
          begin Ctx.IntRegs[Instr.Dest] := Instr.Immediate; Inc(CurPC); end;
        bcThreadCreate:
          // h = THREADCREATE/THREADCALL: spawn a worker running the SUB at the entry PC (Src1). The
          // arguments are already staged in this context's transfer slots; SpawnWorker snapshots them
          // into the worker. The returned int handle goes into Dest. Falls through.
          begin
            Ctx.IntRegs[Instr.Dest] := SpawnWorker(Ctx.IntRegs[Instr.Src1], Ctx);
            Inc(CurPC);
          end;
        bcThreadWait:
          // THREADWAIT h: join the worker named by the handle in Src1, then continue.
          begin JoinWorker(Ctx.IntRegs[Instr.Src1]); Inc(CurPC); end;
        bcThreadSelf:
          // THREADSELF(): the current thread's handle (0 on the main thread).
          begin Ctx.IntRegs[Instr.Dest] := GSelfHandle; Inc(CurPC); end;
        bcThreadDetach:
          // THREADDETACH h: detach the worker named by the handle in Src1.
          begin DetachWorker(Ctx.IntRegs[Instr.Src1]); Inc(CurPC); end;
        // Mutexes (M5.4, FreeBASIC API).
        bcMutexCreate:
          begin Ctx.IntRegs[Instr.Dest] := CreateMutex; Inc(CurPC); end;
        bcMutexLock:
          begin LockMutex(Ctx.IntRegs[Instr.Src1]); Inc(CurPC); end;
        bcMutexUnlock:
          begin UnlockMutex(Ctx.IntRegs[Instr.Src1]); Inc(CurPC); end;
        bcMutexDestroy:
          begin DestroyMutex(Ctx.IntRegs[Instr.Src1]); Inc(CurPC); end;
        // Condition variables (M5.4, FreeBASIC API).
        bcCondCreate:
          begin Ctx.IntRegs[Instr.Dest] := CreateCond; Inc(CurPC); end;
        bcCondWait:
          begin CondWaitOp(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2]); Inc(CurPC); end;
        bcCondSignal:
          begin CondSignalOp(Ctx.IntRegs[Instr.Src1]); Inc(CurPC); end;
        bcCondBroadcast:
          begin CondBroadcastOp(Ctx.IntRegs[Instr.Src1]); Inc(CurPC); end;
        bcCondDestroy:
          begin DestroyCond(Ctx.IntRegs[Instr.Src1]); Inc(CurPC); end;
        // Block-scoped record reclamation (M8): mark at a loop-body entry, reclaim at its exit.
        bcRecMarkPush:
          begin
            if Ctx.BlockRecMarkTop >= Length(Ctx.BlockRecMark) then
              SetLength(Ctx.BlockRecMark, Ctx.BlockRecMarkTop + 256);
            Ctx.BlockRecMark[Ctx.BlockRecMarkTop] := Ctx.RecordCount;
            Inc(Ctx.BlockRecMarkTop);
            Inc(CurPC);
          end;
        bcRecMarkPop:
          begin
            if Ctx.BlockRecMarkTop > 0 then
            begin
              Dec(Ctx.BlockRecMarkTop);
              if Ctx.BlockRecMark[Ctx.BlockRecMarkTop] < Ctx.RecordCount then
                Ctx.RecordCount := Ctx.BlockRecMark[Ctx.BlockRecMarkTop];
            end;
            Inc(CurPC);
          end;
        // Transfer registers (M2): move a value to/from the non-saved transfer banks.
        bcXferStoreInt:
          begin Ctx.XferInt[Instr.Immediate] := Ctx.IntRegs[Instr.Src1]; Inc(CurPC); end;
        bcXferStoreFloat:
          begin Ctx.XferFloat[Instr.Immediate] := Ctx.FloatRegs[Instr.Src1]; Inc(CurPC); end;
        bcXferStoreString:
          begin Ctx.XferStr[Instr.Immediate] := Ctx.StringRegs[Instr.Src1]; Inc(CurPC); end;
        bcXferLoadInt:
          begin Ctx.IntRegs[Instr.Dest] := Ctx.XferInt[Instr.Immediate]; Inc(CurPC); end;
        bcXferLoadFloat:
          begin Ctx.FloatRegs[Instr.Dest] := Ctx.XferFloat[Instr.Immediate]; Inc(CurPC); end;
        bcXferLoadString:
          begin Ctx.StringRegs[Instr.Dest] := Ctx.XferStr[Instr.Immediate]; Inc(CurPC); end;
        // UDT/record heap (M3)
        bcRecordNew:
          begin
            // Immediate bit 48 (M5.2c refinement): allocate in the shared region (cross-thread, e.g. a
            // SHARED UDT scalar); otherwise in this thread's per-context heap. typeId is bits 32..47.
            if (Instr.Immediate shr 48) and 1 <> 0 then
              Ctx.IntRegs[Instr.Dest] := AllocSharedRecord(Instr.Src1, Instr.Src2,
                                      Instr.Immediate and $FFFF, (Instr.Immediate shr 32) and $FFFF)
            else
              Ctx.IntRegs[Instr.Dest] := AllocRecord(Ctx, Instr.Src1, Instr.Src2,
                                      Instr.Immediate and $FFFF, (Instr.Immediate shr 32) and $FFFF);
            Inc(CurPC);
          end;
        bcRecordNewArray:
          begin
            RecordNewArrayInit(Ctx, Instr.Src1, Instr.Immediate);
            Inc(CurPC);
          end;
        bcRecordNewArrayInd:  // array-of-UDT MEMBER: FArrays id is the runtime handle in IntRegs[Src1]
          begin
            RecordNewArrayInit(Ctx, IntRegs[Instr.Src1], Instr.Immediate);
            Inc(CurPC);
          end;
        bcRecordNewBlock:  // Callocate(n, SizeOf(T)) of a UDT: n consecutive shared records; Dest = first
          begin
            IntRegs[Instr.Dest] := AllocSharedRecordBlock(IntRegs[Instr.Src1],
                                     Instr.Immediate and $FFFF, (Instr.Immediate shr 16) and $FFFF,
                                     (Instr.Immediate shr 32) and $FFFF, (Instr.Immediate shr 48) and $FFFF);
            Inc(CurPC);
          end;
        // M5.2c: ResolveRec routes the handle to its record (per-thread heap or the shared region).
        bcRecordLoadInt:
          begin Ctx.IntRegs[Instr.Dest] := ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.IntData[Instr.Immediate]; Inc(CurPC); end;
        bcRecordLoadFloat:
          begin Ctx.FloatRegs[Instr.Dest] := ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.FloatData[Instr.Immediate]; Inc(CurPC); end;
        bcRecordLoadString:
          begin Ctx.StringRegs[Instr.Dest] := ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.StringData[Instr.Immediate]; Inc(CurPC); end;
        bcRecordStoreInt:
          begin ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.IntData[Instr.Immediate] := Ctx.IntRegs[Instr.Src2]; Inc(CurPC); end;
        bcRecordStoreFloat:
          begin ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.FloatData[Instr.Immediate] := Ctx.FloatRegs[Instr.Src2]; Inc(CurPC); end;
        bcRecordStoreString:
          begin ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.StringData[Instr.Immediate] := Ctx.StringRegs[Instr.Src2]; Inc(CurPC); end;
        bcRecordTypeId:
          begin Ctx.IntRegs[Instr.Dest] := ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.TypeId; Inc(CurPC); end;
        bcRecordFree:
          begin FreeSharedRecord(Ctx.IntRegs[Instr.Src1]); Inc(CurPC); end;   // DELETE p

        // Array operations - typed versions are faster. Bounds-guarded to match the base ExecuteArrayOp
        // path and the m207 model: MODERN OOB read -> default, OOB store -> dropped (memory-safe); CLASSIC raises.
        bcArrayLoadInt:
          begin
            if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) then
              Ctx.IntRegs[Instr.Dest] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]]
            else
              Ctx.IntRegs[Instr.Dest] := 0;
            Inc(CurPC);
          end;
        bcArrayLoadFloat:
          begin
            if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) then
              Ctx.FloatRegs[Instr.Dest] := FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]]
            else
              Ctx.FloatRegs[Instr.Dest] := 0.0;
            Inc(CurPC);
          end;
        bcArrayLoadString:
          begin
            if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) then
              Ctx.StringRegs[Instr.Dest] := FArrays[Instr.Src1].StringData[IntRegs[Instr.Src2]]
            else
              Ctx.StringRegs[Instr.Dest] := '';
            Inc(CurPC);
          end;
        bcArrayStoreInt:
          begin
            if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) then
              FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] := IntRegs[Instr.Dest];
            Inc(CurPC);
          end;
        bcArrayStoreFloat:
          begin
            if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) then
              FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]] := FloatRegs[Instr.Dest];
            Inc(CurPC);
          end;
        bcArrayStoreString:
          begin
            if ArrayBoundsOK(Instr.Src1, IntRegs[Instr.Src2]) then
              FArrays[Instr.Src1].StringData[IntRegs[Instr.Src2]] := Ctx.StringRegs[Instr.Dest];
            Inc(CurPC);
          end;

        // Array dimension (slow path - only at initialization)
        bcArrayDim:
          begin
            ArrayIdx := Instr.Src1;
            ArrInfo := FProgram.GetArray(ArrayIdx);
            if ArrayIdx >= Length(FArrays) then
              SetLength(FArrays, ArrayIdx + 1);
            FArrays[ArrayIdx].ElementType := Byte(ArrInfo.ElementType);
            FArrays[ArrayIdx].DimCount := ArrInfo.DimCount;
            SetLength(FArrays[ArrayIdx].Dimensions, ArrInfo.DimCount);
            SetLength(FArrays[ArrayIdx].LowerBounds, ArrInfo.DimCount);
            for i := 0 to ArrInfo.DimCount - 1 do
            begin
              // Effective lower bound: a RUNTIME lb register (FreeBASIC "Lbound(m) To Ubound(m)")
              // overrides the compile-time constant; recorded for LBOUND/UBOUND and index adjustment.
              ArrLowerBound := 0;
              if (i < Length(ArrInfo.LowerBoundRegisters)) and (ArrInfo.LowerBoundRegisters[i] >= 0) then
                ArrLowerBound := IntRegs[ArrInfo.LowerBoundRegisters[i]]
              else if i <= High(ArrInfo.LowerBounds) then
                ArrLowerBound := ArrInfo.LowerBounds[i];
              FArrays[ArrayIdx].LowerBounds[i] := ArrLowerBound;

              if ArrInfo.Dimensions[i] = 0 then
              begin
                // Variable upper bound: size = ub - lb + 1.
                case ArrInfo.DimRegTypes[i] of
                  srtInt: FArrays[ArrayIdx].Dimensions[i] := IntRegs[ArrInfo.DimRegisters[i]] - ArrLowerBound + 1;
                  srtFloat: FArrays[ArrayIdx].Dimensions[i] := Trunc(FloatRegs[ArrInfo.DimRegisters[i]]) - ArrLowerBound + 1;
                end;
              end
              else
                FArrays[ArrayIdx].Dimensions[i] := ArrInfo.Dimensions[i];
            end;
            ProdDims := 1;
            for i := 0 to ArrInfo.DimCount - 1 do
              ProdDims := ProdDims * FArrays[ArrayIdx].Dimensions[i];
            FArrays[ArrayIdx].TotalSize := ProdDims;
            case ArrInfo.ElementType of
              srtInt: SetLength(FArrays[ArrayIdx].IntData, ProdDims);
              srtFloat: SetLength(FArrays[ArrayIdx].FloatData, ProdDims);
              srtString: SetLength(FArrays[ArrayIdx].StringData, ProdDims);
            end;
            Inc(CurPC);
          end;

        // LBOUND/UBOUND(arr[, dim]) - bound of a dimension (B1.4)
        bcArrayLBound:
          begin
            ArrayIdx := Instr.Src1;
            LinearIdx := IntRegs[Instr.Src2];  // 0-based dim index
            IntRegs[Instr.Dest] := FArrays[ArrayIdx].LowerBounds[LinearIdx];
            Inc(CurPC);
          end;
        bcArrayUBound:
          begin
            ArrayIdx := Instr.Src1;
            LinearIdx := IntRegs[Instr.Src2];  // 0-based dim index
            IntRegs[Instr.Dest] := FArrays[ArrayIdx].LowerBounds[LinearIdx]
                                   + FArrays[ArrayIdx].Dimensions[LinearIdx] - 1;
            Inc(CurPC);
          end;

        // ERASE arr / REDIM [PRESERVE] arr(ub) (B1.4)
        bcArrayErase:
          begin
            EraseArray(Instr.Src1);
            Inc(CurPC);
          end;
        bcArrayRedim:
          begin
            RedimArray(Instr.Src1, IntRegs[Instr.Src2], (Instr.Immediate and 1) <> 0);
            Inc(CurPC);
          end;

        // Generic array load/store (slower - legacy)
        bcArrayLoad:
          begin
            ArrayIdx := Instr.Src1;
            LinearIdx := IntRegs[Instr.Src2];
            case FArrays[ArrayIdx].ElementType of
              0: Ctx.IntRegs[Instr.Dest] := FArrays[ArrayIdx].IntData[LinearIdx];
              1: Ctx.FloatRegs[Instr.Dest] := FArrays[ArrayIdx].FloatData[LinearIdx];
              2: Ctx.StringRegs[Instr.Dest] := FArrays[ArrayIdx].StringData[LinearIdx];
            end;
            Inc(CurPC);
          end;
        bcArrayStore:
          begin
            ArrayIdx := Instr.Src1;
            LinearIdx := IntRegs[Instr.Src2];
            case FArrays[ArrayIdx].ElementType of
              0: FArrays[ArrayIdx].IntData[LinearIdx] := Ctx.IntRegs[Instr.Dest];
              1: FArrays[ArrayIdx].FloatData[LinearIdx] := Ctx.FloatRegs[Instr.Dest];
              2: FArrays[ArrayIdx].StringData[LinearIdx] := Ctx.StringRegs[Instr.Dest];
            end;
            Inc(CurPC);
          end;

        // NOTE: I/O operations (bcPrint*, bcInput*) are now handled by Group 4 (ExecuteIOOp)
        // NOTE: String operations (bcStr*) are now handled by Group 1 (ExecuteStringOp)

        // Bitwise operations
        bcBitwiseAnd:
          begin
            IntRegs[Instr.Dest] := IntRegs[Instr.Src1] and IntRegs[Instr.Src2];
            Inc(CurPC);
          end;
        bcBitwiseOr:
          begin
            IntRegs[Instr.Dest] := IntRegs[Instr.Src1] or IntRegs[Instr.Src2];
            Inc(CurPC);
          end;
        bcBitwiseXor:
          begin
            IntRegs[Instr.Dest] := IntRegs[Instr.Src1] xor IntRegs[Instr.Src2];
            Inc(CurPC);
          end;
        bcBitwiseNot:
          begin
            IntRegs[Instr.Dest] := not IntRegs[Instr.Src1];
            Inc(CurPC);
          end;
        bcShl:  // FreeBASIC SHL
          begin
            IntRegs[Instr.Dest] := IntRegs[Instr.Src1] shl IntRegs[Instr.Src2];
            Inc(CurPC);
          end;
        bcShr:  // MODERN: arithmetic (a signed operand keeps its sign, "-5 Shr 2" = -2). CLASSIC: logical.
          begin
            if ModernConv then
              IntRegs[Instr.Dest] := ArithShr64(IntRegs[Instr.Src1], IntRegs[Instr.Src2])
            else
              IntRegs[Instr.Dest] := LogicalShr64(IntRegs[Instr.Src1], IntRegs[Instr.Src2]);
            Inc(CurPC);
          end;
        bcShrUInt:  // SHR on an unsigned operand (UInteger/ULongInt): zero-filling
          begin
            IntRegs[Instr.Dest] := LogicalShr64(IntRegs[Instr.Src1], IntRegs[Instr.Src2]);
            Inc(CurPC);
          end;
        bcRandomize:  // RANDOMIZE: seed the RNG (Immediate=1 -> explicit seed in Src1; 0 -> time-based)
          begin
            if Instr.Immediate <> 0 then RandSeed := Cardinal(IntRegs[Instr.Src1]) else Randomize;
            Inc(CurPC);
          end;

        // NOTE: Graphics operations (bcGraphic*) are now handled by Group 10 (ExecuteGraphicsOp)
        // NOTE: Sound operations (bcSound*) are now handled by Group 11 (ExecuteSoundOp)

        // Special variables - fall back to slow path
        bcLoadTI, bcLoadTIS, bcStoreTIS, bcLoadDTS, bcFre, bcPeek, bcPoke:
          begin
            Ctx.PC := CurPC;
            ExecuteInstruction(Ctx, Instr);
            CurPC := Ctx.PC + 1;
          end;

        // TRON/TROFF - switch between debug and fast mode
        bcTron:
          begin
            Ctx.TraceActive := True;
            Ctx.LastSourceLine := 0;
            {$IFNDEF DEBUG_MODE}
            // In RunFast: switch to RunDebug for trace support
            Ctx.ModeSwitchPC := CurPC + 1;  // RunDebug continues from the next instruction
            Ctx.Running := False; // Signal to exit main loop
            Break;             // Exit loop, then call RunDebug
            {$ELSE}
            Inc(CurPC);
            {$ENDIF}
          end;
        bcTroff:
          begin
            Ctx.TraceActive := False;
            {$IFDEF DEBUG_MODE}
            // In RunDebug: switch to RunFast for speed
            Ctx.ModeSwitchPC := CurPC + 1;  // RunFast continues from the next instruction
            Ctx.Running := False; // Signal to exit main loop
            Break;             // Exit loop, then call RunFast
            {$ELSE}
            Inc(CurPC);
            {$ENDIF}
          end;

        // Termination
        bcEnd, bcStop: Break;
        bcNop: Inc(CurPC);
        bcClear:
          begin
            // CLR: Clear all variables (fall back to slow path for simplicity)
            Ctx.PC := CurPC;
            ExecuteInstruction(Ctx, Instr);
            CurPC := Ctx.PC + 1;
          end;

        // Load/Store var (legacy - should not appear in optimized code)
        bcLoadVar, bcStoreVar:
          begin
            // Fall back to slow path
            Ctx.PC := CurPC;
            ExecuteInstruction(Ctx, Instr);
            CurPC := Ctx.PC + 1;
          end;

        // DATA/READ/RESTORE - fall back to slow path
        bcDataAdd, bcDataReadInt, bcDataReadFloat, bcDataReadString, bcDataRestore:
          begin
            Ctx.PC := CurPC;
            ExecuteInstruction(Ctx, Instr);
            CurPC := Ctx.PC + 1;
          end;

        // Input/Output commands - fall back to slow path
        bcGet, bcGetkey, bcPrintUsing, bcPrintUsingInt, bcPrintUsingStage, bcPrintUsingRun, bcPudef, bcChar,
        // ASSERT may halt the program (its slow-path handler clears Ctx.Running), so check it here too.
        bcAssert:
          begin
            Ctx.PC := CurPC;
            ExecuteInstruction(Ctx, Instr);
            if not Ctx.Running then Break;  // CTRL+C, quit requested, or a failed ASSERT
            CurPC := Ctx.PC + 1;
          end;

        // System commands - fall back to slow path
        bcFast, bcSlow, bcSleep, bcKey, bcFrame:
          begin
            Ctx.PC := CurPC;
            ExecuteInstruction(Ctx, Instr);
            CurPC := Ctx.PC + 1;
          end;

        // Error handling
        bcTrap:
          begin
            // TRAP linenum - Set error handler line
            // If Immediate >= 0, use it directly (constant line number)
            // If Immediate = -1, use register R[Src1] (variable line number)
            if Instr.Immediate >= 0 then
              Ctx.TrapLine := Instr.Immediate
            else
              Ctx.TrapLine := IntRegs[Instr.Src1];
            if Ctx.TrapLine > 0 then
              Ctx.TrapPC := -1  // Will be resolved when error occurs
            else
            begin
              // TRAP 0 - Disable trap handler
              Ctx.TrapLine := 0;
              Ctx.TrapPC := -1;
            end;
            Inc(CurPC);
          end;
        bcResume:
          begin
            // RESUME [line] - Resume at error line or specified line
            if Ctx.InErrorHandler then
            begin
              // Register form: Immediate = -1 flags "line number in Src1" (register 0 is a
              // perfectly valid register - the old Src1 <> 0 test alone silently degraded
              // RESUME <line> to plain RESUME when the line landed in R0). Src1 <> 0 is kept
              // as a fallback for .basc files predating the flag.
              if (Instr.Immediate = -1) or (Instr.Src1 <> 0) then
              begin
                // RESUME <line> - jump to specified line
                CurPC := FProgram.FindPCForLine(IntRegs[Instr.Src1]);
                if CurPC < 0 then
                  CurPC := InstrCount;  // Line not found - end program
              end
              else if Ctx.ResumePC >= 0 then
              begin
                // Plain RESUME - jump back to error line
                CurPC := Ctx.ResumePC;
              end
              else
              begin
                Inc(CurPC);
                Ctx.InErrorHandler := False;
                ResetErrorStateIfModern(Ctx);
                System.Continue;
              end;
              Ctx.InErrorHandler := False;
              ResetErrorStateIfModern(Ctx);  // FreeBASIC clears Err after RESUME (MODERN only)
              // NOTE: Must use System.Continue to get loop control, NOT Self.Continue method!
              System.Continue;  // Jump to resume PC immediately
            end
            else
              Inc(CurPC);
          end;
        bcResumeNext:
          begin
            // RESUME NEXT - Resume at next BASIC line after the one that caused the error
            if Ctx.InErrorHandler and (Ctx.LastErrorLine > 0) then
            begin
              // Find the first instruction of the NEXT BASIC line after the error line
              // This is the correct semantics: RESUME NEXT skips to the next BASIC statement
              CurPC := FProgram.FindPCAfterLine(Ctx.LastErrorLine);
              if CurPC < 0 then
                CurPC := InstrCount;  // No more lines - end program
              Ctx.InErrorHandler := False;
              ResetErrorStateIfModern(Ctx);  // FreeBASIC clears Err after RESUME NEXT (MODERN only)
              // NOTE: Must use System.Continue to get loop control, NOT Self.Continue method!
              System.Continue;  // Jump to new PC immediately
            end
            else
              Inc(CurPC);
          end;
        bcOnError:
          begin
            // ON ERROR GOTO <label> - install a label-based error handler.
            // Immediate = resolved handler PC; TrapLine = -2 marks "label handler, PC pre-resolved".
            Ctx.TrapPC := Instr.Immediate;
            Ctx.TrapLine := -2;
            Inc(CurPC);
          end;
        bcResumeLabel:
          begin
            // RESUME <label> - resume at a named label (Immediate = target PC)
            if Ctx.InErrorHandler then
            begin
              CurPC := Instr.Immediate;
              Ctx.InErrorHandler := False;
              ResetErrorStateIfModern(Ctx);  // FreeBASIC clears Err after RESUME <label> (MODERN only)
              System.Continue;  // Jump to target PC immediately
            end
            else
              Inc(CurPC);
          end;

        // File and system operations - fall back to slow path
        bcLoad, bcSave, bcVerify, bcBload, bcBsave, bcBoot,
        bcRun, bcList, bcNew, bcCatalog,
        // File management commands
        bcCopyFile, bcScratch, bcRenameFile, bcConcat, bcMkdir, bcChdir, bcRmdir, bcMoveFile,
        bcSetEnviron, bcShell,
        bcRaiseError:
          begin
            Ctx.PC := CurPC;
            ExecuteInstruction(Ctx, Instr);
            CurPC := Ctx.PC + 1;
          end;
      else
        raise Exception.CreateFmt('Unknown Group 0 opcode %d at CurPC=%d', [Op, CurPC]);
      end; // end case Op of (Group 0)
    else
      // A dense index with no opcode: a superinstruction hole or an out-of-range value. Illegal.
      raise Exception.CreateFmt('Unknown/illegal opcode $%.4X (dense %d) at CurPC=%d', [Op, DenseOp, CurPC]);
    end; // end case DenseOp of

    {$IFDEF DEBUG_MODE}
    {$IFDEF ENABLE_PROFILER}
    // Profiler: AfterInstruction hook
    if Assigned(FProfiler) and FProfiler.Enabled then
    begin
      FProfiler.AfterInstruction(CurPC, Op);
      // Track superinstructions
      if Op >= bcGroupSuper then
        FProfiler.OnSuperinstruction(Op, 1);
    end;
    {$ENDIF}
    {$ENDIF}
  end;
  except
    on E: Exception do
    begin
      // Ctx.PC is not maintained instruction by instruction during fast dispatch - it holds
      // whatever the last thing that synced it left there - but it IS what the host reads to
      // report an error ("ERROR during VM execution at PC="). Point it at the instruction that
      // actually raised, so the report names the culprit rather than the last slow-path op that
      // happened to write the field. Without this the reported PC is incidental, and an AOT
      // helper call (which syncs Ctx.PC at its own instruction) reports a different one from
      // the interpreter for the very same error. An exception re-raised out of the AOT call
      // site names its own culprit, since CurPC there is only the region entry.
      if Ctx.AotRaisePC >= 0 then
      begin
        CurPC := Ctx.AotRaisePC;
        Ctx.AotRaisePC := -1;
      end;
      Ctx.PC := CurPC;
      // Check if a TRAP handler is set and we're not already in an error handler
      // (to prevent infinite recursion if exception occurs during error handling).
      // TrapLine > 0: classic line-number handler (TRAP / ON ERROR GOTO <line>).
      // TrapLine = -2: FreeBASIC label-based handler (ON ERROR GOTO <label>); TrapPC pre-resolved.
      if ((Ctx.TrapLine > 0) or (Ctx.TrapLine = -2)) and (not Ctx.InErrorHandler) then
      begin
        // Save error info for EL, ER, ERR$ variables
        Ctx.LastErrorLine := FProgram.GetSourceLine(CurPC);
        // ERFN: the procedure that was executing when the error was raised ('' at module level).
        Ctx.LastErrorProc := FProgram.GetProcNameAt(CurPC);
        Ctx.ResumePC := CurPC;  // Save PC for RESUME
        Ctx.InErrorHandler := True;

        // Map the exception to a BASIC error code. A TExecutorException already carries the
        // dialect-aware code (e.g. filesystem errors raised by RaiseFileError) — use it verbatim.
        if E is TExecutorException then
          Ctx.LastErrorCode := TExecutorException(E).ErrorCode
        // Map FPC exception types to BASIC error codes
        else if E is EDivByZero then
          Ctx.LastErrorCode := ERR_DIVISION_BY_ZERO
        else if E is EZeroDivide then
          Ctx.LastErrorCode := ERR_DIVISION_BY_ZERO
        else if E is EOverflow then
          Ctx.LastErrorCode := ERR_OVERFLOW
        else if E is EUnderflow then
          Ctx.LastErrorCode := ERR_UNDERFLOW
        else if E is EInvalidOp then
          Ctx.LastErrorCode := ERR_DOMAIN
        else if E is ERangeError then
          Ctx.LastErrorCode := ERR_BAD_SUBSCRIPT
        else
          Ctx.LastErrorCode := ERR_INTERNAL;

        Ctx.LastErrorMessage := E.Message;

        // Resolve the handler PC. Line-based: look up the line; label-based (-2): TrapPC is
        // already the resolved handler PC set by bcOnError.
        if Ctx.TrapLine > 0 then
          Ctx.TrapPC := FProgram.FindPCForLine(Ctx.TrapLine);

        if Ctx.TrapPC >= 0 then
        begin
          // Jump to error handler - set flag to re-enter loop
          CurPC := Ctx.TrapPC;
          Ctx.PC := CurPC;
          Ctx.Running := True;
          TrapTriggered := True;  // Will restart the loop
        end
        else
        begin
          // Trap line not found - re-raise
          Ctx.InErrorHandler := False;
          raise;
        end;
      end
      else
      begin
        // No TRAP set - re-raise exception
        raise;
      end;
    end;
  end;
  until not TrapTriggered;  // Restart loop if TRAP handler was triggered

  {$POP}  // Restore range/overflow checking

  Ctx.PC := CurPC;

  // Check if we need to switch execution mode (TRON/TROFF)
  if Ctx.TraceActive and (Ctx.PC < InstrCount) then
  begin
    {$IFNDEF DEBUG_MODE}
    // Was RunFast, now need RunDebug
    Ctx.Running := True;
    RunDebug;
    Exit;
    {$ENDIF}
  end
  else if (not Ctx.TraceActive) and (Ctx.PC < InstrCount) then
  begin
    {$IFDEF DEBUG_MODE}
    // Was RunDebug, now need RunFast
    Ctx.Running := True;
    RunFast;
    Exit;
    {$ENDIF}
  end;

  Ctx.Running := False;

  {$IFDEF DEBUG_MODE}
  {$IFDEF ENABLE_PROFILER}
  // Stop profiler
  if Assigned(FProfiler) then
    FProfiler.Stop;
  {$ENDIF}
  {$ENDIF}

  // Reset FAST mode when program ends (ensure screen is visible)
  if Assigned(FOutputDevice) then
    FOutputDevice.SetFastMode(False);
end;
