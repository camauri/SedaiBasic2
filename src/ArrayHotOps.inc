{ ArrayHotOps.inc -- the hot element-access opcodes of group 3 (bcArrayLoad/Store x Int/Float/String).

  ONE copy of this code, included in TWO places:
    * TBytecodeVM.ExecuteArrayOp -- the general group-3 handler (which still owns every other group-3 op)
    * RunTemplate.inc            -- the hot loop, which now runs these INLINE instead of delegating

  Why. RunTemplate executes group 0 and the superinstructions inline, but hands groups 1/2/3 to a handler
  procedure: write Ctx.PC, make a non-inlined call, enter a SECOND case, read Ctx.PC back. That double
  dispatch cost roughly four times the work itself -- a plain ArrayLoadFloat measured ~100 ns against
  ~25 ns for a scalar opcode, while the FUSED forms (superinstructions, dispatched inline) came in at
  ~34-45 ns doing the very same work. Array-heavy code paid it on every element access, and that is why
  the optimizer could strip 24% of n-body's hot-loop instructions and buy only 1.8% of wall clock: it took
  away the cheap arithmetic and left the expensive accesses standing.

  These branches are self-contained -- they touch only ArrayIdx/LinearIdx, ArrayBoundsOK, FArrays and the
  register banks, and never Ctx.PC -- which is what makes them safe to run inline. Sharing the source text
  is what keeps the two dispatch paths from drifting apart, the standing trap of this VM.

  BOUNDS SEMANTICS ARE DIALECT-DEPENDENT, and they stay in ArrayBoundsOK, untouched: CLASSIC raises
  Commodore's ?BAD SUBSCRIPT, MODERN follows FreeBASIC and substitutes a default on a read / drops the
  store. Both dispatch paths call that same function, so both dialects keep exactly the behaviour they had.
  This change moves WHERE the opcode is executed, never WHAT it does. }

    3: // bcArrayLoadInt
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if ArrayBoundsOK(ArrayIdx, LinearIdx) then
        begin
          Ctx.IntRegs[Instr.Dest] := FArrays[ArrayIdx].IntData[LinearIdx];
          {$IFDEF ENABLE_PROFILER}
          if Assigned(FProfiler) and FProfiler.Enabled then
            FProfiler.OnArrayAccess(ArrayIdx, False, LinearIdx);
          {$ENDIF}
        end
        else
          Ctx.IntRegs[Instr.Dest] := 0;   // MODERN out-of-bounds read -> default (FreeBASIC)
      end;
    4: // bcArrayLoadFloat
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if ArrayBoundsOK(ArrayIdx, LinearIdx) then
        begin
          Ctx.FloatRegs[Instr.Dest] := FArrays[ArrayIdx].FloatData[LinearIdx];
          {$IFDEF ENABLE_PROFILER}
          if Assigned(FProfiler) and FProfiler.Enabled then
            FProfiler.OnArrayAccess(ArrayIdx, False, LinearIdx);
          {$ENDIF}
        end
        else
          Ctx.FloatRegs[Instr.Dest] := 0.0;   // MODERN out-of-bounds read -> default
      end;
    5: // bcArrayLoadString
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if ArrayBoundsOK(ArrayIdx, LinearIdx) then
        begin
          Ctx.StringRegs[Instr.Dest] := FArrays[ArrayIdx].StringData[LinearIdx];
          {$IFDEF ENABLE_PROFILER}
          if Assigned(FProfiler) and FProfiler.Enabled then
            FProfiler.OnArrayAccess(ArrayIdx, False, LinearIdx);
          {$ENDIF}
        end
        else
          Ctx.StringRegs[Instr.Dest] := '';   // MODERN out-of-bounds read -> default
      end;
    6: // bcArrayStoreInt
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if ArrayBoundsOK(ArrayIdx, LinearIdx) then     // MODERN out-of-bounds store is dropped (FreeBASIC)
        begin
          FArrays[ArrayIdx].IntData[LinearIdx] := Ctx.IntRegs[Instr.Dest];
          {$IFDEF ENABLE_PROFILER}
          if Assigned(FProfiler) and FProfiler.Enabled then
            FProfiler.OnArrayAccess(ArrayIdx, True, LinearIdx);
          {$ENDIF}
        end;
      end;
    7: // bcArrayStoreFloat
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if ArrayBoundsOK(ArrayIdx, LinearIdx) then
        begin
          FArrays[ArrayIdx].FloatData[LinearIdx] := Ctx.FloatRegs[Instr.Dest];
          {$IFDEF ENABLE_PROFILER}
          if Assigned(FProfiler) and FProfiler.Enabled then
            FProfiler.OnArrayAccess(ArrayIdx, True, LinearIdx);
          {$ENDIF}
        end;
      end;
    8: // bcArrayStoreString
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if ArrayBoundsOK(ArrayIdx, LinearIdx) then
        begin
          FArrays[ArrayIdx].StringData[LinearIdx] := Ctx.StringRegs[Instr.Dest];
          {$IFDEF ENABLE_PROFILER}
          if Assigned(FProfiler) and FProfiler.Enabled then
            FProfiler.OnArrayAccess(ArrayIdx, True, LinearIdx);
          {$ENDIF}
        end;
      end;
