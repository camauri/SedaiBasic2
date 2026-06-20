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
var
  Instructions: PBytecodeInstruction;
  InstrCount: Integer;
  CurPC: Integer;  // Current Program Counter (local for speed)
  Instr: TBytecodeInstruction;
  Ctx: TExecutionContext;  // M5.2: active execution context, read once per Run (zero hot-path cost)
  Op: Word;
  Group: Word;
  // Local copies of frequently accessed fields for better register allocation
  IntRegs: PInt64;
  FloatRegs: PDouble;
  ArrayIdx, LinearIdx: Integer;
  InputStr, PrintStr: string;
  InputVal: Double;
  i, ProdDims: Integer;
  NextTabCol, TabIdx: Integer;
  ArrInfo: TSSAArrayInfo;
  // Local vars for ArrayReverseRange and ArrayShiftLeft
  ArrIdxW: Word;
  LStartIdx, LEndIdx, LLoopIdx: Integer;
  LFirstVal: Int64;
  // Local copy of TrueValue for comparison operators
  TrueVal: Int64;
  // Flag for TRAP handler jump (can't use goto from except block)
  TrapTriggered: Boolean;
  // Event polling counter
  EventPollCounter: Integer;
  {$IFDEF DEBUG_MODE}
  CurSourceLine: Integer;  // Current source line from Source Map
  {$ENDIF}
begin
  if FProgram = nil then raise Exception.Create('No program loaded');

  Instructions := PBytecodeInstruction(FProgram.GetInstructionsPtr);
  if Instructions = nil then raise Exception.Create('Empty program');

  InstrCount := FProgram.GetInstructionCount;
  // M5.2: bind the active execution context once per Run — a worker thread supplies its own via
  // the GActiveCtx threadvar (set by WorkerThreadEntry before calling RunFast); the main thread
  // leaves it nil and uses FCtx. A single read per Run keeps per-instruction access register-direct.
  if GActiveCtx <> nil then Ctx := GActiveCtx else Ctx := FCtx;
  // The main context starts at the program EntryPoint (StartPC = -1); a worker starts at the entry
  // PC of the SUB it runs (StartPC set in RunWorker), with a synthetic call frame already primed.
  if Ctx.StartPC >= 0 then CurPC := Ctx.StartPC else CurPC := FProgram.EntryPoint;
  Ctx.Running := True;  // Mark VM as running
  // M5.3: the main thread is the render owner (only it may touch the SDL device). A worker
  // (GActiveCtx <> nil) must never steal ownership; its graphics ops defer to the owner's draw queue.
  if GActiveCtx = nil then
    FRenderOwnerThreadId := GetCurrentThreadID;

  // Get direct pointers to register arrays
  IntRegs := @Ctx.IntRegs[0];
  FloatRegs := @Ctx.FloatRegs[0];
  // Local copy of TrueValue for comparison operators
  TrueVal := FTrueValue;
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
    Instr := Instructions[CurPC];
    Op := Instr.OpCode;
    Group := Op shr 8;

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

    // Two-level dispatch: first check group, then sub-opcode
    // Superinstructions are in group $C8+ (bcGroupSuper = $C800)
    if Group >= $C8 then
    begin
      // Extract sub-opcode for superinstruction dispatch
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
            raise EZeroDivide.Create('Division by zero');
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
            raise EZeroDivide.Create('Division by zero');
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] / Double(Pointer(@Instr.Immediate)^);
          Inc(CurPC);
        end;

        // Fused compare-zero-and-branch (Int) - sub-opcodes 60-61
        60: if IntRegs[Instr.Src1] = 0 then CurPC := Instr.Immediate else Inc(CurPC);  // bcBranchEqZeroInt
        61: if IntRegs[Instr.Src1] <> 0 then CurPC := Instr.Immediate else Inc(CurPC); // bcBranchNeZeroInt

        // Fused compare-zero-and-branch (Float) - sub-opcodes 70-71
        70: if FloatRegs[Instr.Src1] = 0.0 then CurPC := Instr.Immediate else Inc(CurPC);  // bcBranchEqZeroFloat
        71: if FloatRegs[Instr.Src1] <> 0.0 then CurPC := Instr.Immediate else Inc(CurPC); // bcBranchNeZeroFloat

        // Fused array-store-constant - sub-opcodes 80-82
        80: begin FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] := Instr.Immediate; Inc(CurPC); end; // bcArrayStoreIntConst
        81: begin FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]] := Double(Pointer(@Instr.Immediate)^); Inc(CurPC); end; // bcArrayStoreFloatConst
        82: begin FArrays[Instr.Src1].StringData[IntRegs[Instr.Src2]] := FProgram.StringConstants[Instr.Immediate]; Inc(CurPC); end; // bcArrayStoreStringConst

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
        110: begin // bcArrayLoadAddFloat: dest = acc + arr[idx]
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate] + FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]];
          Inc(CurPC);
        end;
        111: begin // bcArrayLoadSubFloat: dest = acc - arr[idx]
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate] - FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]];
          Inc(CurPC);
        end;
        112: begin // bcArrayLoadDivAddFloat: dest = acc + arr[idx] / denom
          // Extra encodes: acc_reg (low 16) + denom_reg (high 16)
          if Abs(FloatRegs[(Instr.Immediate shr 16) and $FFFF]) < 1e-300 then
            raise EZeroDivide.Create('Division by zero');
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate and $FFFF] +
            FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]] / FloatRegs[(Instr.Immediate shr 16) and $FFFF];
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
          FloatRegs[Instr.Dest] := Sqrt(FloatRegs[Instr.Src1] + FloatRegs[Instr.Src2]);
          Inc(CurPC);
        end;

        // Array Load + Branch - sub-opcodes 140-141
        140: // bcArrayLoadIntBranchNZ: if arr[idx] <> 0 goto target
          if FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] <> 0 then
            CurPC := Instr.Immediate
          else
            Inc(CurPC);
        141: // bcArrayLoadIntBranchZ: if arr[idx] = 0 goto target
          if FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] = 0 then
            CurPC := Instr.Immediate
          else
            Inc(CurPC);

        // Array Reverse Range - sub-opcode 156
        156: // bcArrayReverseRange: reverse arr[start..end-1] in-place
          begin
            LStartIdx := IntRegs[Instr.Src2];
            LEndIdx := IntRegs[Instr.Dest] - 1;
            ArrIdxW := Instr.Src1;
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
            LFirstVal := FArrays[ArrIdxW].IntData[LStartIdx];
            LLoopIdx := LStartIdx;
            while LLoopIdx <= LEndIdx do
            begin
              FArrays[ArrIdxW].IntData[LLoopIdx] := FArrays[ArrIdxW].IntData[LLoopIdx + 1];
              Inc(LLoopIdx);
            end;
            FArrays[ArrIdxW].IntData[LEndIdx + 1] := LFirstVal;
            Inc(CurPC);
          end;

        // Array Swap (Int) - sub-opcode 250
        250: // bcArraySwapInt: swap arr[idx1] and arr[idx2]
          begin
            Ctx.SwapTempInt := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]];
            FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Dest]];
            FArrays[Instr.Src1].IntData[IntRegs[Instr.Dest]] := Ctx.SwapTempInt;
            Inc(CurPC);
          end;

        // Self-increment/decrement (Int) - sub-opcodes 251-252
        251: begin Inc(IntRegs[Instr.Dest], IntRegs[Instr.Src1]); Inc(CurPC); end; // bcAddIntSelf
        252: begin Dec(IntRegs[Instr.Dest], IntRegs[Instr.Src1]); Inc(CurPC); end; // bcSubIntSelf

        // Array Load to register (Int) - sub-opcode 253
        253: begin IntRegs[Instr.Dest] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]]; Inc(CurPC); end; // bcArrayLoadIntTo

        // Array Copy Element - sub-opcode 254
        254: begin FArrays[Instr.Dest].IntData[IntRegs[Instr.Src2]] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]]; Inc(CurPC); end; // bcArrayCopyElement

        // Array Move Element - sub-opcode 255
        255: begin FArrays[Instr.Dest].IntData[IntRegs[Instr.Src2]] := FArrays[Instr.Dest].IntData[IntRegs[Instr.Src1]]; Inc(CurPC); end; // bcArrayMoveElement

      else
        raise Exception.CreateFmt('Unknown superinstruction sub-opcode %d (full opcode $%.4X) at CurPC=%d', [Op and $FF, Op, CurPC]);
      end;
    end
    else
    begin
      // Standard bytecode opcodes - dispatch by group
      // For non-Group 0 opcodes, delegate to specialized handlers
      case Group of
        1: begin Ctx.PC := CurPC; ExecuteStringOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
        2: begin Ctx.PC := CurPC; ExecuteMathOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
        3: begin Ctx.PC := CurPC; ExecuteArrayOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
        4: begin Ctx.PC := CurPC; ExecuteIOOp(Ctx, Instr); if not Ctx.Running then Break; CurPC := Ctx.PC + 1; end;
        5: begin Ctx.PC := CurPC; ExecuteSpecialVarOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
        6: begin Ctx.PC := CurPC; ExecuteFileIOOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
        7: begin Ctx.PC := CurPC; ExecuteSpriteOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
        {$IFDEF WEB_MODE}
        8: begin Ctx.PC := CurPC; ExecuteWebOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
        {$ENDIF}
        10: begin Ctx.PC := CurPC; ExecuteGraphicsOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
        11: begin Ctx.PC := CurPC; ExecuteSoundOp(Ctx, Instr); CurPC := Ctx.PC + 1; end;
        0: // Core VM operations - inline for performance
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
        bcNegInt: begin IntRegs[Instr.Dest] := -IntRegs[Instr.Src1]; Inc(CurPC); end;

        // Float arithmetic - critical for spectral-norm
        bcAddFloat: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] + FloatRegs[Instr.Src2]; Inc(CurPC); end;
        bcSubFloat: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] - FloatRegs[Instr.Src2]; Inc(CurPC); end;
        bcMulFloat: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] * FloatRegs[Instr.Src2]; Inc(CurPC); end;
        bcDivFloat: begin
          if FloatRegs[Instr.Src2] = 0.0 then
            raise EZeroDivide.Create('Division by zero');
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
        bcFloatToInt: begin IntRegs[Instr.Dest] := Trunc(FloatRegs[Instr.Src1]); Inc(CurPC); end;

        // Integer comparisons (use TrueVal for TRUE, 0 for FALSE)
        bcCmpEqInt: begin if IntRegs[Instr.Src1] = IntRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpNeInt: begin if IntRegs[Instr.Src1] <> IntRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpLtInt: begin if IntRegs[Instr.Src1] < IntRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpGtInt: begin if IntRegs[Instr.Src1] > IntRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpLeInt: begin if IntRegs[Instr.Src1] <= IntRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;
        bcCmpGeInt: begin if IntRegs[Instr.Src1] >= IntRegs[Instr.Src2] then IntRegs[Instr.Dest] := TrueVal else IntRegs[Instr.Dest] := 0; Inc(CurPC); end;

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
        bcMathSqr: begin FloatRegs[Instr.Dest] := Sqrt(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathSin: begin FloatRegs[Instr.Dest] := Sin(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathCos: begin FloatRegs[Instr.Dest] := Cos(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathTan: begin FloatRegs[Instr.Dest] := Tan(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathExp: begin FloatRegs[Instr.Dest] := Exp(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathLog: begin FloatRegs[Instr.Dest] := Ln(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathRnd: begin FloatRegs[Instr.Dest] := Random; Inc(CurPC); end;
        bcMathAtn: begin FloatRegs[Instr.Dest] := ArcTan(FloatRegs[Instr.Src1]); Inc(CurPC); end;

        // Control flow
        bcJump: CurPC := Instr.Immediate;
        bcJumpIfZero: if IntRegs[Instr.Src1] = 0 then CurPC := Instr.Immediate else Inc(CurPC);
        bcJumpIfNotZero: if IntRegs[Instr.Src1] <> 0 then CurPC := Instr.Immediate else Inc(CurPC);
        bcCall:
          begin
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
            FramePush(Ctx);
            Ctx.CallStack[Ctx.CallStackPtr] := CurPC + 1;
            Inc(Ctx.CallStackPtr);
            {$IFDEF DEBUG_MODE}
            if Assigned(FDebugger) then FDebugger.NotifyCall;
            {$ENDIF}
            CurPC := Instr.Immediate;
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
          // h = THREADCREATE(@sub, param): spawn a worker running the SUB at the entry PC (Src1)
          // with the int parameter (Src2); the returned int handle goes into Dest. Falls through.
          begin
            Ctx.IntRegs[Instr.Dest] := SpawnWorker(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2]);
            Inc(CurPC);
          end;
        bcThreadWait:
          // THREADWAIT h: join the worker named by the handle in Src1, then continue.
          begin JoinWorker(Ctx.IntRegs[Instr.Src1]); Inc(CurPC); end;
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
            Ctx.IntRegs[Instr.Dest] := AllocRecord(Ctx, Instr.Src1, Instr.Src2,
                                      Instr.Immediate and $FFFF, (Instr.Immediate shr 32) and $FFFF);
            Inc(CurPC);
          end;
        bcRecordNewArray:
          begin
            RecordNewArrayInit(Ctx, Instr.Src1, Instr.Immediate);
            Inc(CurPC);
          end;
        bcRecordLoadInt:
          begin Ctx.IntRegs[Instr.Dest] := Ctx.Records[Ctx.IntRegs[Instr.Src1]].IntData[Instr.Immediate]; Inc(CurPC); end;
        bcRecordLoadFloat:
          begin Ctx.FloatRegs[Instr.Dest] := Ctx.Records[Ctx.IntRegs[Instr.Src1]].FloatData[Instr.Immediate]; Inc(CurPC); end;
        bcRecordLoadString:
          begin Ctx.StringRegs[Instr.Dest] := Ctx.Records[Ctx.IntRegs[Instr.Src1]].StringData[Instr.Immediate]; Inc(CurPC); end;
        bcRecordStoreInt:
          begin Ctx.Records[Ctx.IntRegs[Instr.Src1]].IntData[Instr.Immediate] := Ctx.IntRegs[Instr.Src2]; Inc(CurPC); end;
        bcRecordStoreFloat:
          begin Ctx.Records[Ctx.IntRegs[Instr.Src1]].FloatData[Instr.Immediate] := Ctx.FloatRegs[Instr.Src2]; Inc(CurPC); end;
        bcRecordStoreString:
          begin Ctx.Records[Ctx.IntRegs[Instr.Src1]].StringData[Instr.Immediate] := Ctx.StringRegs[Instr.Src2]; Inc(CurPC); end;
        bcRecordTypeId:
          begin Ctx.IntRegs[Instr.Dest] := Ctx.Records[Ctx.IntRegs[Instr.Src1]].TypeId; Inc(CurPC); end;

        // Array operations - typed versions are faster
        bcArrayLoadInt:
          begin
            Ctx.IntRegs[Instr.Dest] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]];
            Inc(CurPC);
          end;
        bcArrayLoadFloat:
          begin
            Ctx.FloatRegs[Instr.Dest] := FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]];
            Inc(CurPC);
          end;
        bcArrayLoadString:
          begin
            Ctx.StringRegs[Instr.Dest] := FArrays[Instr.Src1].StringData[IntRegs[Instr.Src2]];
            Inc(CurPC);
          end;
        bcArrayStoreInt:
          begin
            FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] := IntRegs[Instr.Dest];
            Inc(CurPC);
          end;
        bcArrayStoreFloat:
          begin
            FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]] := FloatRegs[Instr.Dest];
            Inc(CurPC);
          end;
        bcArrayStoreString:
          begin
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
            for i := 0 to ArrInfo.DimCount - 1 do
            begin
              if ArrInfo.Dimensions[i] = 0 then
              begin
                case ArrInfo.DimRegTypes[i] of
                  srtInt: FArrays[ArrayIdx].Dimensions[i] := IntRegs[ArrInfo.DimRegisters[i]] + 1;
                  srtFloat: FArrays[ArrayIdx].Dimensions[i] := Trunc(FloatRegs[ArrInfo.DimRegisters[i]]) + 1;
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
            Ctx.PC := CurPC + 1;  // Continue from next instruction
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
            Ctx.PC := CurPC + 1;  // Continue from next instruction
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
        bcGet, bcGetkey, bcPrintUsing, bcPudef, bcChar:
          begin
            Ctx.PC := CurPC;
            ExecuteInstruction(Ctx, Instr);
            if not Ctx.Running then Break;  // CTRL+C or quit requested
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
              // Check if Src1 contains a line number (non-zero register = line number)
              if Instr.Src1 <> 0 then
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
                System.Continue;
              end;
              Ctx.InErrorHandler := False;
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
              // NOTE: Must use System.Continue to get loop control, NOT Self.Continue method!
              System.Continue;  // Jump to new PC immediately
            end
            else
              Inc(CurPC);
          end;

        // File and system operations - fall back to slow path
        bcLoad, bcSave, bcVerify, bcBload, bcBsave, bcBoot,
        bcRun, bcList, bcNew, bcCatalog,
        // File management commands
        bcCopyFile, bcScratch, bcRenameFile, bcConcat, bcMkdir, bcChdir, bcMoveFile:
          begin
            Ctx.PC := CurPC;
            ExecuteInstruction(Ctx, Instr);
            CurPC := Ctx.PC + 1;
          end;
      else
        raise Exception.CreateFmt('Unknown Group 0 opcode %d at CurPC=%d', [Op, CurPC]);
      end; // end case Op of (Group 0)
      else
        raise Exception.CreateFmt('Unknown opcode group %d (opcode $%.4X) at CurPC=%d', [Group, Op, CurPC]);
      end; // end case Group of
    end;

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
      // Check if a TRAP handler is set and we're not already in an error handler
      // (to prevent infinite recursion if exception occurs during error handling)
      if (Ctx.TrapLine > 0) and (not Ctx.InErrorHandler) then
      begin
        // Save error info for EL, ER, ERR$ variables
        Ctx.LastErrorLine := FProgram.GetSourceLine(CurPC);
        Ctx.ResumePC := CurPC;  // Save PC for RESUME
        Ctx.InErrorHandler := True;

        // Map FPC exception types to BASIC error codes
        if E is EDivByZero then
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

        // Find PC for trap line number
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
