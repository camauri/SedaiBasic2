unit SedaiJit;

{ ============================================================================
  SedaiJit - native code generation for hot loops (VM performance, milestone J2/J3).

  Compiles a hot bytecode loop [HeaderPC..EndPC] to native x86-64 that operates
  DIRECTLY on the VM register banks (IntRegs / FloatRegs), removing per-instruction
  dispatch. Integer ops use the general registers; float ops use SSE2 (addsd/mulsd/
  subsd on xmm), so a float-heavy loop runs at near-native speed.

  Calling convention of a compiled loop (Win64 / SysV both pass the two pointers in
  the first two integer arg registers; the emitter picks the right ones):
      function(IntRegs, FloatRegs: PInt64): PtrInt;   // returns the exit bytecode PC

  CONSERVATIVE BY DESIGN (bit-identical or bail): CompileLoop returns nil unless
  EVERY instruction in the range is in the supported set and every branch either
  stays in the range or is a clean exit. Anything else -> the loop is left to the
  interpreter. The supported set is grown as milestones land (arrays, sqrt, div...).

  ...except that "anything else" no longer has to mean the WHOLE loop (J14, the helper
  route). An instruction with no native form can now be RUN BY THE INTERPRETER, one
  instruction at a time, with native execution continuing after it - which is what the
  AOT has always done. The two engines never differed by philosophy, only by what they
  were handed: the AOT gets ExecOne in its context record, the JIT was given no channel
  to the interpreter at all, so its only fallback was to give up the loop. It is given
  one now (HelperFn/VMSelf below), and one unsupported instruction costs a call instead
  of the whole loop. See EmitHelperCall for the five parts and what may NOT be routed.

  Register file (Win64 non-volatile saved in the prologue): rbx = IntRegs base,
  rsi = FloatRegs base. rax/rcx scratch (integer), xmm0/xmm1 scratch (float).
  ============================================================================ }

{$mode objfpc}{$H+}

interface

uses
  SysUtils, SedaiBytecodeTypes, SedaiX86Emitter,
  // SedaiAot for the AOTCTX_* offsets ONLY: the string/record leaf primitives live in one table that
  // the VM fills once and both backends read, which is what keeps them from drifting apart. The
  // dependency is one-way - SedaiAot does not use this unit.
  SedaiAot,
  Cpu;   // AVXSupport / POPCNTSupport: the two runtime feature tests the lowerings below are gated on

type
  // Re-exported so existing clients (SedaiBytecodeVM) keep compiling against this unit;
  // the real definitions live in SedaiX86Emitter, shared with the AOT backend.
  TX86Emitter = SedaiX86Emitter.TX86Emitter;
  TExecMem = SedaiX86Emitter.TExecMem;

  // A compiled loop: call with the two register-bank base pointers + the array descriptor table
  // (4x Int64/array: IntData ptr, FloatData ptr, Count, LBound) + the EXECUTING context object.
  // Returns the exit PC. Ctx makes the code thread-agnostic: the Xfer banks and the record heap
  // are read through it at run time (field offsets baked, addresses NOT), so any context - main
  // or THREADCREATE worker - can run the same native loop on its own state.
  TNativeLoopFn = function(IntRegs, FloatRegs: PInt64; ArrDesc: Pointer; Ctx: Pointer): PtrInt;

// Compile the loop body [HeaderPC..EndPC] (inclusive) to native code. Ins points at instruction 0.
// ProgLen is the whole program's instruction count (NativeOff/InRange are indexed by absolute PC so an
// inlined callee's PCs resolve too). TrueVal is the VM's TRUE value baked into integer comparisons.
// AllowUnsafe = MODERN dialect and no forced bounds-check: only then may array access / sqrt / div be
// compiled (their MODERN edge semantics -- OOB->default, div0->IEEE, sqrt(neg)->NaN -- match the native
// SSE forms; CLASSIC would raise, so the loop bails). XferIntOff/XferFloatOff/RecordsOff are the byte
// offsets of the XferInt/XferFloat/Records dynamic-array FIELDS inside TExecutionContext: the emitted
// code loads the current data pointer from [ctx + off] at run time (the ctx object arrives as the 4th
// call argument), so the native code holds NO context-specific address and is safe for any thread's
// context - the old design baked the MAIN context's absolute addresses and corrupted worker state.
// RecSize/RecIntOff/RecFloatOff are SizeOf(TRecordStorage) and the byte offsets of its IntData/FloatData
// fields, so record field access (J13) needs no hardcoded layout. Returns a TExecMem whose Ptr is a
// TNativeLoopFn, or nil if the loop is not compilable.
// HelperFn/VMSelf are the helper route (J14): @AotExecOne and the VM instance that owns the compiled
// loop. Both are fixed for the life of the code, so they are baked as imm64 - the only per-CONTEXT
// value the route needs is the context object, and that is already in the frame slot the Xfer/record
// accessors read. Pass nil for either and the route is off: every routable opcode bails the loop
// again, which is the pre-J14 behaviour.
// PrimCtx/StringRegsOff are the string family as native LEAF calls (J15): the VM's filled TAotCtx,
// whose primitive addresses are read at COMPILE time and baked, and the byte offset of the StringRegs
// field inside TExecutionContext, read at RUN time so a worker uses its own bank. nil puts the whole
// string family back on the helper route - slower, but correct.
function CompileLoop(Ins: Pointer; HeaderPC, EndPC, ProgLen: Integer; TrueVal: Int64;
                     AllowUnsafe, Modern: Boolean; XferIntOff, XferFloatOff: Integer;
                     RecordsOff: Integer; RecSize, RecIntOff, RecFloatOff: Integer;
                     HelperFn, VMSelf: Pointer;
                     PrimCtx: Pointer; StringRegsOff: Integer): TExecMem;

// J2 self-test: emit  a+b  and call it, proving the emit->exec->call pipeline.
function JitSelfTest(out Msg: string): Boolean;

// Diagnostic (set to the last opcode/PC processed by CompileLoop): when a loop bails, these hold the
// culprit. Read by BuildJitLoops under the JIT_DIAG env var. Not thread-safe; diagnostics only.
var
  JitDiagCurOp: Word = 0;
  JitDiagCurPC: Integer = -1;

implementation

// x86-64 register numbers (RAX..R15, XMM0/1) come from SedaiX86Emitter.

{ ---------------- loop compiler ---------------- }

type
  PBcInstr = ^TBytecodeInstruction;
  TFixup = record
    PatchOff: Integer;    // byte offset of the rel32 field to patch
    TargetPC: Integer;    // bytecode PC to jump to (or -1 = epilogue)
  end;

// JIT_HELPER=0 puts every routable opcode back on "bail the whole loop", which is the A/B for the
// helper route on ONE binary (the same shape as AOT_BITOPS). Read once, cached.
var
  GJitHelperState: Integer = -1;

function HelperRouteEnabled: Boolean;
begin
  if GJitHelperState < 0 then
  begin
    if GetEnvironmentVariable('JIT_HELPER') = '0' then GJitHelperState := 0 else GJitHelperState := 1;
  end;
  Result := GJitHelperState = 1;
end;

// The opcodes the helper route may carry (J14) - a WHITELIST, and every exclusion is a reason, not a
// to-do item. Two properties make an opcode routable, and BOTH were read out of the interpreter
// rather than assumed:
//   * it must not touch the ARRAY descriptor table. ExecuteArrayOp sets FArraysDirty on every
//     operation and a REDIM inside it can move the element storage, while this code holds the
//     descriptor pointer in r8 and element bases in the GPR pool for the whole invocation, with no
//     ctx record for the helper to refresh (see EmitHelperCall). Group 3 is therefore out - and
//     ExecuteStringOp / ExecuteMathOp / ExecuteIOOp were CHECKED: none of them names FArrays.
//   * it must not move the PC. The guard after the call is correct either way, but an instruction
//     that jumps would leave the native loop EVERY time it is taken, which is slower than never
//     compiling the loop at all. The same three handlers were checked for `Ctx.PC :=`: none.
// Groups 1/2/4 (string, math, I/O) are in wholesale on that reading - which is what puts a PRINT, a
// SIN or a `^` inside a hot loop, each of them enough to cost the whole loop until now. Out for the
// same "buys nothing" reason: calls, returns and branches, which have their own arms above.
function IsRoutableOp(Op: Word): Boolean;
begin
  case Op and $FF00 of
    bcGroupString, bcGroupMath, bcGroupIO:
      Result := True;
  else
    // The core-group and superinstruction stragglers, by name: the string bank the JIT does not
    // have, and record allocation, which needs the VM's allocator.
    case Op of
      bcLoadConstString, bcCopyString, bcIntToString,
      bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString,
      bcRecordNew, bcRecordFree,
      bcStrAppendMapped, bcStrConcatCharAt:
        Result := True;
    else
      Result := False;
    end;
  end;
end;

function CompileLoop(Ins: Pointer; HeaderPC, EndPC, ProgLen: Integer; TrueVal: Int64;
                     AllowUnsafe, Modern: Boolean; XferIntOff, XferFloatOff: Integer;
                     RecordsOff: Integer; RecSize, RecIntOff, RecFloatOff: Integer;
                     HelperFn, VMSelf: Pointer;
                     PrimCtx: Pointer; StringRegsOff: Integer): TExecMem;
var
  E: TX86Emitter;
  NativeOff: array of Integer;      // absolute bytecode PC -> native offset (sized ProgLen)
  InRange: array of Boolean;        // absolute PC belongs to the compiled code (caller range + inlined callees)
  Fixups: array of TFixup;
  NFix: Integer;
  Prog: PBcInstr;
  pc: Integer;
  I: PBcInstr;
  Dd, S1, S2: LongWord;              // register byte offsets (index*8)
  EpilogueOff: Integer;
  d, target: Integer;
  // --- inlined SUB calls (J6): each inlinable bcCallSub in the caller range becomes an inline copy of the
  // callee body [EntryPC..ReturnPC], emitted all-memory (InCallee) around a native FramePush/Pop. ---
  InCallee: Boolean;                 // True while emitting a callee body: everything memory-homed, no cache
  CallPC, CallEntry, CallRet, CallSaveN: array of Integer;   // parallel arrays, one entry per call site
  NCall: Integer;
  ScratchBytes: Integer;             // stack bytes reserved for the deepest call site's bank save/restore
  // Float register allocation (J4): map a VM float reg -> a native xmm (2..7, no REX) or -1 (memory).
  // Only the volatile-plus-two set xmm2..xmm7 is used; xmm6/xmm7 are callee-saved so they are spilled to
  // the stack in the prologue when allocated. Integer VM regs are allocated to r9..r15 (see ILoc below).
  FLoc: array of Integer;
  FMaxReg, NextXmm, fi: Integer;
  SaveX6, SaveX7: Boolean;
  // Integer register allocation (J5): map a VM int reg -> a native GPR (r9..r15) or -1 (memory-homed).
  // Pool order: r9/r10/r11 (volatile) first, then r12..r15 (callee-saved, push/pop'd when used).
  ILoc: array of Integer;
  IMaxReg, NextGpr, ii, gpr: Integer;
  IntPool: array[0..6] of Integer;
  SaveGpr: array[0..15] of Boolean;
  GprUsed: array[0..15] of Boolean;  // which native GPRs are claimed (int alloc + array-base cache)
  // Array base/count caching (J5c LICM): a compiled loop's array descriptor is fixed for the whole
  // native invocation, so the base pointer and element count are loop-invariant. Cache them in the GPRs
  // left free after int allocation, removing two descriptor loads from every array access.
  CArrId: array of Integer;      // distinct array ids used in the loop
  CArrOff: array of Integer;     // descriptor base offset: 0 (int arrays) or 8 (float arrays)
  CArrUses: array of Integer;    // access count (for priority)
  CArrBase: array of Integer;    // assigned GPR holding the base pointer, or -1
  CArrCount: array of Integer;   // assigned GPR holding the element count, or -1
  NCArr, ci, cj, ct: Integer;
  // Callee-dedicated array cache (J6d Stage 2): while emitting an inlined callee, the caller's live GPRs are
  // saved to the stack scratch, freeing the whole r9..r15 pool for the callee's OWN array base/count cache
  // (CArr2*), loaded at the inline entry. This gives an array-heavy inlined SUB far more cached arrays than
  // the handful of GPRs left free after the caller's allocation. Restored at the callee's ReturnSub.
  CArr2Id, CArr2Off, CArr2Uses, CArr2Base, CArr2Count: array of Integer;
  NCArr2: Integer;
  CallerGpr: array of Integer;   // distinct native GPRs the caller uses (to preserve around every callee)
  NCallerGpr, GprSaveDisp: Integer;
  CtxDisp: Integer;                  // [rsp+CtxDisp] holds the Ctx object pointer (4th call argument)
  // Callee integer register allocation (J6f): while emitting an inlined callee, its hottest int regs get a
  // native GPR (r9..r15, shared with the callee array cache by use-count priority), so the inner-loop index
  // no longer reloads from memory on every array access. Non-allocated callee int regs stay memory-homed.
  ILoc2: array of Integer;
  ICalleeMax: Integer;
  // Sparse frame save (J6e): an inlined callee runs all-memory, so it can only corrupt a caller register
  // whose home is MEMORY (not allocated to a native reg). Save/restore ONLY those around the callee instead
  // of the whole bank -- the allocated caller regs live in r9-r15/xmm2-7, which the callee never touches.
  // For a loop whose caller regs are all allocated (e.g. n-body's main loop) this list is empty: no copy.
  SaveIntRegs, SaveFloatRegs: array of Integer;
  NSaveInt, NSaveFloat: Integer;
  // Inlined GOSUB (bcCall): a classic GOSUB shares the caller's register frame (no FramePush), so its body
  // [GEntry..GRet] is emitted inline with the all-memory model. The caller's allocated regs are spilled to
  // their home slots before the body and reloaded after, so the body reads/writes shared variables through
  // memory consistently. Deopt-prone ops that could leave a NON-terminal path (integer div/mod, LBOUND/
  // UBOUND, an out-of-region jump) bail; the only deopts left in an inlinable body are the terminal CLASSIC
  // traps (array OOB, Sqr of a negative, divide by zero) which raise and never return.
  InGosub: Boolean;
  GEntry, GRet, GCallPC: array of Integer;   // one entry per inlinable GOSUB site
  NGosub: Integer;
  // Helper route (J14): scratch slots for the two base registers a call destroys but that have no bank
  // slot of their own (r8 = the array descriptor table, rsi = the float bank base, volatile on SysV),
  // and the statically-known pad that puts rsp back on a 16-byte boundary at the call.
  UseHelper: Boolean;                // this loop routes at least one instruction (allocates the slots)
  RoutesRecords: Boolean;            // ...and at least one of them ALLOCATES a record (see bcRecMarkPush)
  ArrDescDisp, FltSaveDisp: Integer;
  HelperAdjust: Integer;             // bytes subtracted from rsp around a call (shadow space + pad)
  UseLeaf: Boolean;                  // J15: the string family goes to leaf calls, not the helper route

  procedure AddFixup(AOff, ATarget: Integer);
  begin
    if NFix >= Length(Fixups) then SetLength(Fixups, NFix * 2 + 8);
    Fixups[NFix].PatchOff := AOff;
    Fixups[NFix].TargetPC := ATarget;
    Inc(NFix);
  end;

  // Emit a rel32 jump/branch with a placeholder, register a fixup to TargetPC (or -1 for epilogue).
  procedure JmpRel(TargetPC: Integer);
  begin
    E.Emit8($E9);                    // jmp rel32
    AddFixup(E.Len, TargetPC);
    E.Emit32(0);
  end;
  procedure JccRel(CC: Byte; TargetPC: Integer);
  begin
    E.Emit8($0F); E.Emit8(CC);       // 0F 8x  (jcc rel32)
    AddFixup(E.Len, TargetPC);
    E.Emit32(0);
  end;
  // Deopt: leave the native loop and resume the interpreter at absolute PC apc (mov eax,apc; jmp epilogue).
  // The epilogue flushes the allocated registers to memory, so the interpreter re-executes apc with correct
  // state -- used to defer the rare/faulting cases (div-by-zero raise, LBOUND/UBOUND of a non-first dim) to
  // the interpreter. NOT valid inside an inlined callee (its native frame would be lost), so callers guard
  // on InCallee before emitting one.
  procedure DeoptTo(apc: Integer);
  begin
    E.EmitBytes([$B8]); E.Emit32(LongWord(apc));   // mov eax, apc
    JmpRel(-1);                                     // jmp epilogue
  end;

  // --- float register-allocation aware operand access (J4) ---
  // movsd Wx, <VM float reg vmreg>  (reg-reg if allocated to an xmm, else load from [rsi+off]).
  // In callee-inline mode every VM reg is memory-homed (the caller's FLoc must not be consulted).
  procedure FLoad(Wx, vmreg: Integer);
  begin
    if InCallee or InGosub then
      E.MemOp([$F2, $0F, $10], Wx, RSI, LongWord(vmreg) * 8)
    else if FLoc[vmreg] >= 0 then
    begin
      if FLoc[vmreg] <> Wx then
        E.EmitBytes([$0F, $28, $C0 or (Wx shl 3) or FLoc[vmreg]])         // movaps Wx, xmm_src
    end
    else
      E.MemOp([$F2, $0F, $10], Wx, RSI, LongWord(vmreg) * 8);             // movsd Wx, [rsi+off]
  end;
  // <op>sd Wx, <VM float reg vmreg>
  procedure FOp(const SseOp: array of Byte; Wx, vmreg: Integer);
  begin
    if InCallee or InGosub then
      E.MemOp(SseOp, Wx, RSI, LongWord(vmreg) * 8)
    else if FLoc[vmreg] >= 0 then
      E.EmitBytes([SseOp[0], SseOp[1], SseOp[2], $C0 or (Wx shl 3) or FLoc[vmreg]])
    else
      E.MemOp(SseOp, Wx, RSI, LongWord(vmreg) * 8);
  end;
  // store working xmm Wx -> VM float reg dest
  procedure FStore(vmreg, Wx: Integer);
  begin
    if InCallee or InGosub then
      E.MemOp([$F2, $0F, $11], Wx, RSI, LongWord(vmreg) * 8)
    else if FLoc[vmreg] >= 0 then
    begin
      if FLoc[vmreg] <> Wx then
        E.EmitBytes([$0F, $28, $C0 or (FLoc[vmreg] shl 3) or Wx])         // movaps xmm_dst, Wx
    end
    else
      E.MemOp([$F2, $0F, $11], Wx, RSI, LongWord(vmreg) * 8);             // movsd [rsi+off], Wx
  end;

  // --- integer GPR register-allocation helpers (J5) ---
  // Reg-reg instruction: REX.W (+R if regField>=8)(+B if rmReg>=8), Op..., ModRM(mod=11, regField, rmReg).
  procedure EmitRR(const Op: array of Byte; regField, rmReg: Integer);
  var rex: Byte; k: Integer;
  begin
    rex := $48;
    if regField >= 8 then rex := rex or $04;    // REX.R
    if rmReg    >= 8 then rex := rex or $01;     // REX.B
    E.Emit8(rex);
    for k := 0 to High(Op) do E.Emit8(Op[k]);
    E.Emit8($C0 or ((regField and 7) shl 3) or (rmReg and 7));
  end;
  // mov <native dst>, <native src>   (89 = mov r/m64,r64 : reg field = src, rm field = dst)
  procedure MovRR(dst, src: Integer);
  begin EmitRR([$89], src, dst); end;
  // mov <native reg>, imm64
  procedure MovImm64(natreg: Integer; imm: Int64);
  var rex: Byte;
  begin
    rex := $48; if natreg >= 8 then rex := rex or $01;   // REX.B
    E.Emit8(rex); E.Emit8($B8 or (natreg and 7)); E.Emit64(QWord(imm));
  end;
  // rdx = the current data pointer of a context dynamic-array field: loads the Ctx object from its
  // stack slot, then the field at fieldOff (a dynamic array field IS the data pointer). Used by the
  // Xfer-bank and record-heap accessors - per-context state read through the 4th call argument, so
  // the emitted code holds no context-specific address (thread-agnostic, unlike the old baked bases).
  procedure LoadCtxFieldRdx(fieldOff: Integer);
  begin
    E.EmitBytes([$48, $8B, $94, $24]); E.Emit32(LongWord(CtxDisp));   // mov rdx, [rsp+CtxDisp]
    E.EmitBytes([$48, $8B, $92]); E.Emit32(LongWord(fieldOff));       // mov rdx, [rdx+fieldOff]
  end;
  // mov <native reg>, [rbx+disp]   (entry load; REX.W + REX.R if reg>=8; rbx base, mod=10 disp32)
  procedure LoadRegMem(natreg: Integer; disp: LongWord);
  var rex: Byte;
  begin
    rex := $48; if natreg >= 8 then rex := rex or $04;   // REX.R
    E.Emit8(rex); E.Emit8($8B);
    E.Emit8($80 or ((natreg and 7) shl 3) or RBX);        // mod=10 reg=natreg rm=rbx(3)
    E.Emit32(disp);
  end;
  // mov [rbx+disp], <native reg>   (exit store)
  procedure StoreRegMem(natreg: Integer; disp: LongWord);
  var rex: Byte;
  begin
    rex := $48; if natreg >= 8 then rex := rex or $04;   // REX.R
    E.Emit8(rex); E.Emit8($89);
    E.Emit8($80 or ((natreg and 7) shl 3) or RBX);
    E.Emit32(disp);
  end;
  // Native GPR allocated to VM int reg vmreg in the CURRENT context (caller ILoc, or callee ILoc2 while
  // inlining), or -1 = memory-homed.
  function IAlloc(vmreg: Integer): Integer;
  begin
    if InGosub then
      Result := -1                         // inlined GOSUB body: everything memory-homed (shared frame)
    else if InCallee then
    begin
      if vmreg <= ICalleeMax then Result := ILoc2[vmreg] else Result := -1;
    end
    else
      Result := ILoc[vmreg];
  end;
  // Load VM int reg `vmreg` into scratch native reg `scr` (rax/rcx, always < 8).
  procedure ILoad(scr, vmreg: Integer);
  var n: Integer;
  begin
    n := IAlloc(vmreg);
    if n >= 0 then MovRR(scr, n)
    else E.MemOp([$48, $8B], scr, RBX, LongWord(vmreg) * 8);
  end;
  // Store scratch native reg `scr` (< 8) into VM int reg `vmreg`.
  procedure IStore(vmreg, scr: Integer);
  var n: Integer;
  begin
    n := IAlloc(vmreg);
    if n >= 0 then MovRR(n, scr)
    else E.MemOp([$48, $89], scr, RBX, LongWord(vmreg) * 8);
  end;
  // ALU op  scr <op> vmreg  (MemForm = full memory-form bytes incl. the $48 REX; scr is rax/rcx < 8).
  procedure IOp(const MemForm: array of Byte; scr, vmreg: Integer);
  var rest: array of Byte; k, n: Integer;
  begin
    n := IAlloc(vmreg);
    if n >= 0 then
    begin
      SetLength(rest, Length(MemForm) - 1);      // drop MemForm[0] = $48 REX (EmitRR rebuilds it)
      for k := 1 to High(MemForm) do rest[k - 1] := MemForm[k];
      EmitRR(rest, scr, n);
    end
    else
      E.MemOp(MemForm, scr, RBX, LongWord(vmreg) * 8);
  end;

  // al holds 0/1 -> IntRegs[Dest] := (al<>0) ? TrueVal : 0 (shared by the int and float comparisons).
  procedure CmpBoolToDest;
  begin
    E.EmitBytes([$0F, $B6, $C0]);           // movzx eax,al   (rax = 0/1)
    if TrueVal = -1 then
      E.EmitBytes([$48, $F7, $D8])          // neg rax        (0/-1)
    else if TrueVal <> 1 then
      begin E.EmitBytes([$48, $69, $C0]); E.Emit32(LongWord(TrueVal and $FFFFFFFF)); end;  // imul rax,rax,imm32
    IStore(I^.Dest, RAX);                   // dest := rax
  end;

  // Integer comparison Rd = (Rs1 <cc> Rs2) ? TrueVal : 0
  procedure IntCmp(SetCC: Byte);
  begin
    ILoad(RAX, I^.Src1);                    // mov rax, src1
    IOp([$48, $3B], RAX, I^.Src2);          // cmp rax, src2
    E.EmitBytes([$0F, SetCC, $C0]);         // setcc al
    CmpBoolToDest;
  end;

  // Float comparison Rd = (Rs1 <cc> Rs2) ? TrueVal : 0, with the interpreter's ORDERED IEEE semantics
  // (a NaN operand makes <,<=,>,>=,= false and <> true). ucomisd A,B sets CF=1 if A<B (or unordered),
  // ZF=1 if A==B (or unordered), PF=1 if unordered. Lt/Le/Gt/Ge reduce to seta/setae with an operand swap
  // (seta/setae are false when unordered); Eq/Ne need the parity flag to exclude/include the NaN case.
  // Kind: 0=Lt 1=Le 2=Gt 3=Ge 4=Eq 5=Ne.
  procedure FloatCmp(Kind: Integer);
  begin
    FLoad(XMM0, I^.Src1);                    // xmm0 = a
    FLoad(XMM1, I^.Src2);                    // xmm1 = b
    case Kind of
      0: begin E.EmitBytes([$66, $0F, $2E, $C8]); E.EmitBytes([$0F, $97, $C0]); end;  // a<b : ucomisd b,a ; seta  al
      1: begin E.EmitBytes([$66, $0F, $2E, $C8]); E.EmitBytes([$0F, $93, $C0]); end;  // a<=b: ucomisd b,a ; setae al
      2: begin E.EmitBytes([$66, $0F, $2E, $C1]); E.EmitBytes([$0F, $97, $C0]); end;  // a>b : ucomisd a,b ; seta  al
      3: begin E.EmitBytes([$66, $0F, $2E, $C1]); E.EmitBytes([$0F, $93, $C0]); end;  // a>=b: ucomisd a,b ; setae al
      4: begin E.EmitBytes([$66, $0F, $2E, $C1]);                                     // a=b : ucomisd a,b
               E.EmitBytes([$0F, $94, $C0]); E.EmitBytes([$0F, $9B, $C1]);            //       sete al ; setnp cl
               E.EmitBytes([$20, $C8]); end;                                          //       and al,cl (equal AND ordered)
      5: begin E.EmitBytes([$66, $0F, $2E, $C1]);                                     // a<>b: ucomisd a,b
               E.EmitBytes([$0F, $95, $C0]); E.EmitBytes([$0F, $9A, $C1]);            //       setne al ; setp cl
               E.EmitBytes([$08, $C8]); end;                                          //       or al,cl (not-equal OR unordered)
    end;
    CmpBoolToDest;
  end;

  // Float op:  Rd = Rs1 <sse> Rs2   (compute in xmm0, honouring register allocation of the operands)
  procedure FloatBin(const SseOp: array of Byte);
  begin
    FLoad(XMM0, I^.Src1);
    FOp(SseOp, XMM0, I^.Src2);
    FStore(I^.Dest, XMM0);
  end;

  // Integer DIV / MOD (signed, truncating toward zero -- matches FPC div/mod and x86 idiv). The interpreter
  // RAISES on a zero divisor, and x86 idiv faults on both /0 and the INT64_MIN/-1 overflow; guard both and
  // deopt to `apc` so the interpreter reproduces the exact behaviour (raise, or FPC's overflow result).
  // WantRemainder selects mod (rdx) vs div (rax). Divisor in rcx, dividend in rax; rdx is clobbered.
  procedure DivMod(apc: Integer; WantRemainder: Boolean);
  var p1, p2, p3: Integer;
  begin
    ILoad(RAX, I^.Src1);                          // rax = dividend
    ILoad(RCX, I^.Src2);                          // rcx = divisor
    E.EmitBytes([$48, $85, $C9]);                 // test rcx, rcx
    E.EmitBytes([$75, $00]); p1 := E.Len - 1;     // jnz over-deopt
    DeoptTo(apc);                                  // divisor == 0 -> interpreter raises
    E.PatchByte(p1, Byte(E.Len - (p1 + 1)));
    E.EmitBytes([$48, $83, $F9, $FF]);            // cmp rcx, -1
    E.EmitBytes([$75, $00]); p2 := E.Len - 1;     // jne skip the INT_MIN overflow guard
    E.EmitBytes([$48, $BA]); E.Emit64(QWord($8000000000000000));  // mov rdx, INT64_MIN
    E.EmitBytes([$48, $39, $D0]);                 // cmp rax, rdx
    E.EmitBytes([$75, $00]); p3 := E.Len - 1;     // jne over-deopt
    DeoptTo(apc);                                  // INT64_MIN / -1 -> interpreter (matches FPC exactly)
    E.PatchByte(p3, Byte(E.Len - (p3 + 1)));
    E.PatchByte(p2, Byte(E.Len - (p2 + 1)));
    E.EmitBytes([$48, $99]);                      // cqo   (sign-extend rax into rdx:rax)
    E.EmitBytes([$48, $F7, $F9]);                 // idiv rcx
    if WantRemainder then IStore(I^.Dest, RDX)    // mod -> remainder
    else IStore(I^.Dest, RAX);                    // div -> quotient
  end;

  // UNSIGNED div/mod. Shorter than the signed form above because there is no overflow case: only a
  // zero divisor, which the interpreter raises on, so it deopts. Same lowering as the AOT's.
  procedure DivModUnsigned(apc: Integer; WantRemainder: Boolean);
  var p1: Integer;
  begin
    ILoad(RAX, I^.Src1);                          // rax = dividend
    ILoad(RCX, I^.Src2);                          // rcx = divisor
    E.EmitBytes([$48, $85, $C9]);                 // test rcx, rcx
    E.EmitBytes([$75, $00]); p1 := E.Len - 1;     // jnz over-deopt
    DeoptTo(apc);                                  // divisor == 0 -> interpreter raises
    E.PatchByte(p1, Byte(E.Len - (p1 + 1)));
    E.EmitBytes([$31, $D2]);                      // xor edx, edx  (zero the high half)
    E.EmitBytes([$48, $F7, $F1]);                 // div rcx
    if WantRemainder then IStore(I^.Dest, RDX)
    else IStore(I^.Dest, RAX);
  end;

  // SHR with the interpreter's SATURATING rule, not the hardware's masked shift: a count at or past
  // the width gives the sign (arithmetic) or zero (logical), and a count <= 0 leaves the value alone.
  // ⛔ `shr rax, cl` alone would be WRONG here - x86 masks the count by 63, so "v Shr 64" would be v.
  // Transcribed from the AOT's ShrSat, which aot_validate exercises; the two must not drift.
  procedure ShrSat(Arith: Boolean);
  var pKeep, pDo, pDone: Integer;
  begin
    ILoad(RAX, I^.Src1);
    ILoad(RCX, I^.Src2);
    E.EmitBytes([$48, $85, $C9]);                 // test rcx, rcx
    E.EmitBytes([$7E, $00]); pKeep := E.Len - 1;  // jle @done   (count <= 0 -> value unchanged)
    E.EmitBytes([$48, $83, $F9, $40]);            // cmp rcx, 64
    E.EmitBytes([$7C, $00]); pDo := E.Len - 1;    // jl @shift
    if Arith then E.EmitBytes([$48, $C1, $F8, $3F])   // sar rax, 63  (saturate to the sign)
    else          E.EmitBytes([$31, $C0]);            // xor eax, eax (saturate to 0)
    E.EmitBytes([$EB, $00]); pDone := E.Len - 1;  // jmp @done
    E.PatchByte(pDo, Byte(E.Len - (pDo + 1)));
    if Arith then E.EmitBytes([$48, $D3, $F8])        // sar rax, cl
    else          E.EmitBytes([$48, $D3, $E8]);       // shr rax, cl
    E.PatchByte(pDone, Byte(E.Len - (pDone + 1)));
    E.PatchByte(pKeep, Byte(E.Len - (pKeep + 1)));
    IStore(I^.Dest, RAX);
  end;

  // The MODERN bit intrinsics, width (32 or 64) in the Immediate. Same lowering as the AOT's:
  // rol/ror need no guard at all (x86 masks the count to exactly the modulo the language defines),
  // while bsr/bsf leave their destination UNDEFINED at zero - which is the case these intrinsics
  // define - hence the cmov, and hence redoing the zero test with `test` AFTER the `sub` that
  // clobbers the flags bsr set. A 32-bit form needs no masking: a 32-bit operand-size instruction
  // reads only eax; only the rotates sign-extend, because their result is what a Long holds.
  procedure BitIntrinsic;
  var w32: Boolean;
  begin
    w32 := I^.Immediate = 32;
    ILoad(RAX, I^.Src1);
    case I^.OpCode of
      bcBitRotl, bcBitRotr:
        begin
          ILoad(RCX, I^.Src2);
          if w32 then
          begin
            if I^.OpCode = bcBitRotl then E.EmitBytes([$D3, $C0])        // rol eax, cl
            else                           E.EmitBytes([$D3, $C8]);      // ror eax, cl
            E.EmitBytes([$48, $63, $C0]);                                // movsxd rax, eax
          end
          else if I^.OpCode = bcBitRotl then E.EmitBytes([$48, $D3, $C0])   // rol rax, cl
          else                                E.EmitBytes([$48, $D3, $C8]); // ror rax, cl
        end;
      bcBitClz:
        if w32 then
        begin
          E.EmitBytes([$0F, $BD, $D0]);                                  // bsr edx, eax
          E.EmitBytes([$B9, $1F, $00, $00, $00]);                        // mov ecx, 31
          E.EmitBytes([$29, $D1]);                                       // sub ecx, edx
          E.EmitBytes([$BA, $20, $00, $00, $00]);                        // mov edx, 32
          E.EmitBytes([$85, $C0]);                                       // test eax, eax
          E.EmitBytes([$0F, $44, $CA]);                                  // cmovz ecx, edx
          E.EmitBytes([$89, $C8]);                                       // mov eax, ecx
        end
        else
        begin
          E.EmitBytes([$48, $0F, $BD, $D0]);                             // bsr rdx, rax
          E.EmitBytes([$B9, $3F, $00, $00, $00]);                        // mov ecx, 63
          E.EmitBytes([$48, $29, $D1]);                                  // sub rcx, rdx
          E.EmitBytes([$BA, $40, $00, $00, $00]);                        // mov edx, 64
          E.EmitBytes([$48, $85, $C0]);                                  // test rax, rax
          E.EmitBytes([$48, $0F, $44, $CA]);                             // cmovz rcx, rdx
          E.EmitBytes([$48, $89, $C8]);                                  // mov rax, rcx
        end;
      bcBitCtz:
        if w32 then
        begin
          E.EmitBytes([$0F, $BC, $C8]);                                  // bsf ecx, eax
          E.EmitBytes([$BA, $20, $00, $00, $00]);                        // mov edx, 32
          E.EmitBytes([$85, $C0]);                                       // test eax, eax
          E.EmitBytes([$0F, $44, $CA]);                                  // cmovz ecx, edx
          E.EmitBytes([$89, $C8]);                                       // mov eax, ecx
        end
        else
        begin
          E.EmitBytes([$48, $0F, $BC, $C8]);                             // bsf rcx, rax
          E.EmitBytes([$BA, $40, $00, $00, $00]);                        // mov edx, 64
          E.EmitBytes([$48, $85, $C0]);                                  // test rax, rax
          E.EmitBytes([$48, $0F, $44, $CA]);                             // cmovz rcx, rdx
          E.EmitBytes([$48, $89, $C8]);                                  // mov rax, rcx
        end;
      bcBitPopcnt:
        if w32 then E.EmitBytes([$F3, $0F, $B8, $C0])                    // popcnt eax, eax
        else        E.EmitBytes([$F3, $48, $0F, $B8, $C0]);              // popcnt rax, rax
    end;
    IStore(I^.Dest, RAX);
  end;

  // mov <reg>, [r8 + disp32]   (REX.W + REX.B; r8 = array descriptor base)
  // mov <reg (rax/rcx/rdx, <8)>, [r8+disp]   (REX.W + REX.B for r8 base)
  procedure R8Load(RegField: Byte; Disp: LongWord);
  begin
    E.Emit8($49);
    E.Emit8($8B);
    E.Emit8($80 or ((RegField and 7) shl 3));   // modrm mod=10 reg=RegField rm=000 (r8 low bits)
    E.Emit32(Disp);
  end;
  // mov <native reg (0..15)>, [r8+disp]   (adds REX.R for r9..r15; used to load cached bases/counts)
  procedure R8LoadR(reg: Integer; Disp: LongWord);
  var rex: Byte;
  begin
    rex := $49;                                  // REX.W + REX.B (base r8)
    if reg >= 8 then rex := rex or $04;          // REX.R
    E.Emit8(rex); E.Emit8($8B);
    E.Emit8($80 or ((reg and 7) shl 3));         // mod=10 reg=reg rm=000 (r8)
    E.Emit32(Disp);
  end;

  // LBOUND / UBOUND of a 1-D array (dim 0). Src1 = array id (constant), Src2 = the dim register. The
  // interpreter special-cases dim<0 (rank query) and reads per-dim bounds for dim>0; only dim==0 is handled
  // natively (LBOUND = descriptor LBound; UBOUND = LBound + Count - 1), anything else deopts to `apc`.
  //
  // ⛔⛔ Rank1 is NOT optional on the UPPER bound, and the guard that was here checked the wrong thing:
  // it tested WHICH DIMENSION was asked for and never HOW MANY the array has. `Count` in the descriptor
  // is the TOTAL element count, so LBound + Count - 1 is the upper bound only of a VECTOR:
  // "Dim a(5 To 7, 100 To 104) : UBound(a, 1)" answered 19 where every other engine — and fbc — says 7.
  // Right in intent, wrong in SCOPE, which is the shape this codebase has now hit four times.
  // ⭐ The AOT had already found it on its side (AotArrayNativeOK, "DimCount <> 1 then Exit") and left
  // the note that widening the compiled set would expose the same thing again. It did — the moment the
  // nondeterminism filter stopped dropping this feature's own guardian from jit_validate.
  // The rank cannot be read at run time: the descriptor is 4 Int64 (IntData, FloatData, Count, LBound)
  // and carries no rank. It arrives as BC_ARRAY_RANK1_FLAG in the Immediate, decided in the SSA.
  // ⚠️ The LOWER bound needs none of this: dimension 0's lower bound is exactly what the descriptor
  // holds, whatever the rank.
  procedure ArrBound(apc, ArrayId: Integer; WantUpper, Rank1: Boolean);
  var p1: Integer;
  begin
    if WantUpper and (not Rank1) then
    begin
      DeoptTo(apc);      // rank > 1, or not provable: only the interpreter knows the per-dim extents
      Exit;
    end;
    ILoad(RCX, I^.Src2);                          // rcx = dim
    E.EmitBytes([$48, $85, $C9]);                 // test rcx, rcx
    E.EmitBytes([$74, $00]); p1 := E.Len - 1;     // jz dim0  (dim == 0 -> native)
    DeoptTo(apc);                                  // dim != 0 (rank query / other dim) -> interpreter
    E.PatchByte(p1, Byte(E.Len - (p1 + 1)));
    R8Load(RAX, LongWord(ArrayId) * 32 + 24);     // rax = LBound (dim 0)
    if WantUpper then
    begin
      R8Load(RDX, LongWord(ArrayId) * 32 + 16);   // rdx = Count
      E.EmitBytes([$48, $01, $D0]);               // add rax, rdx
      E.EmitBytes([$48, $FF, $C8]);               // dec rax      (UBOUND = LBound + Count - 1)
    end;
    IStore(I^.Dest, RAX);
  end;

  // --- array base/count caching (J5c): index into the ACTIVE cache, or -1 if this array is not cached.
  // The active cache is the callee-dedicated CArr2 while emitting an inlined callee (J6d), else the caller
  // cache CArr. Both hold loop-invariant base/count values, so they are safe from either context. ---
  function CArrIdx(ArrayId: Integer): Integer;
  var q: Integer;
  begin
    Result := -1;
    if InCallee then
    begin
      for q := 0 to NCArr2 - 1 do
        if CArr2Id[q] = ArrayId then begin Result := q; Exit; end;
    end
    else
      for q := 0 to NCArr - 1 do
        if CArrId[q] = ArrayId then begin Result := q; Exit; end;
  end;
  function ActiveBase(ix: Integer): Integer;   // cached base GPR for active-cache index ix (-1 = not cached)
  begin if InCallee then Result := CArr2Base[ix] else Result := CArrBase[ix]; end;
  function ActiveCount(ix: Integer): Integer;  // cached count GPR for active-cache index ix (-1 = not cached)
  begin if InCallee then Result := CArr2Count[ix] else Result := CArrCount[ix]; end;
  // Emit `cmp rcx, <count>` using the cached count reg if present, else reloading Count into rdx.
  procedure ArrCountCmp(ArrayId, CountReg: Integer);
  begin
    if CountReg >= 0 then
      EmitRR([$3B], RCX, CountReg)                          // cmp rcx, CountReg
    else
    begin
      R8Load(RDX, LongWord(ArrayId) * 32 + 16);             // mov rdx, Count
      E.EmitBytes([$48, $39, $D1]);                          // cmp rcx, rdx
    end;
  end;
  // Return the register holding the array base: the cached one, or rdx after reloading from the descriptor.
  function ArrBaseReg(ArrayId, Off, CachedBase: Integer): Integer;
  begin
    if CachedBase >= 0 then Result := CachedBase
    else begin R8Load(RDX, LongWord(ArrayId) * 32 + LongWord(Off)); Result := RDX; end;
  end;
  // Emit a load/store of xmm0/rax to [BaseReg + rcx*8].  BaseReg may be rdx or any r8..r15 (REX.B).
  // A SIB base whose low 3 bits are 101 (rbp / r13) has no mod=00 encoding -- that slot means "disp32, no
  // base" -- so such a base needs mod=01 with an explicit disp8=0. EmitSib emits the right ModRM+SIB(+disp8).
  procedure EmitSib(BaseReg: Integer);
  var sib: Byte;
  begin
    sib := $C8 or (BaseReg and 7);               // scale=8 (11), index=rcx (001), base=BaseReg&7
    if (BaseReg and 7) = 5 then
    begin
      E.Emit8($44); E.Emit8(sib); E.Emit8($00);  // mod=01 (disp8) rm=100 (SIB), disp8 = 0
    end
    else
    begin
      E.Emit8($04); E.Emit8(sib);                // mod=00 rm=100 (SIB)
    end;
  end;
  procedure ArrDataAccess(IsFloat, IsStore: Boolean; BaseReg: Integer);
  begin
    if IsFloat then
    begin
      E.Emit8($F2);
      if BaseReg >= 8 then E.Emit8($41);         // REX.B
      E.Emit8($0F);
      if IsStore then E.Emit8($11) else E.Emit8($10);
      EmitSib(BaseReg);                          // movsd xmm0, [base+rcx*8]  (or store)
    end
    else
    begin
      if BaseReg >= 8 then E.Emit8($49) else E.Emit8($48);   // REX.W (+B)
      if IsStore then E.Emit8($89) else E.Emit8($8B);
      EmitSib(BaseReg);                          // mov rax, [base+rcx*8]  (or store)
    end;
  end;

  // Bounds behaviour depends on the dialect. MODERN + no forced check (Classic=False): out of bounds ->
  // default (read 0 / drop store), matching FreeBASIC. CLASSIC or --bounds-check (Classic=True): the
  // interpreter RAISES, so the JIT deopts to the array op's PC and lets the interpreter reproduce the exact
  // error. `cmp rcx,count` must already be emitted; this emits the in-bounds guard for the CLASSIC path.
  procedure EmitClassicBoundsGuard(apc: Integer);
  var p: Integer;
  begin
    E.EmitBytes([$72, $00]); p := E.Len - 1;               // jb +over  (unsigned: idx<count -> in bounds)
    DeoptTo(apc);                                           // OOB -> interpreter raises ERangeError
    E.PatchByte(p, Byte(E.Len - (p + 1)));
  end;

  // FloatRegs[Dst] := arr[idx]. ArrayId is a compile-time constant. Safe = B4 range analysis
  // proved idx in [0, TotalSize) (BC_BOUNDS_SAFE_FLAG on the instruction): no compare, no
  // guard, no deopt - just the data access. Classic is irrelevant when Safe (nothing can trip).
  procedure ArrLoadF(apc, ArrayId, IdxReg, DstReg: Integer; Classic, Safe: Boolean);
  var pOOB, pDone, ix, baseR: Integer;
  begin
    ix := CArrIdx(ArrayId);
    ILoad(RCX, IdxReg);                                     // rcx := index (reg or [rbx+idx])
    if not Safe then
      if ix >= 0 then ArrCountCmp(ArrayId, ActiveCount(ix)) else ArrCountCmp(ArrayId, -1);
    if ix >= 0 then baseR := ActiveBase(ix) else baseR := -1;
    if Safe then
    begin
      baseR := ArrBaseReg(ArrayId, 8, baseR);
      ArrDataAccess(True, False, baseR);                    // movsd xmm0, [base+rcx*8]
    end
    else if Classic then
    begin
      EmitClassicBoundsGuard(apc);
      baseR := ArrBaseReg(ArrayId, 8, baseR);
      ArrDataAccess(True, False, baseR);                    // movsd xmm0, [base+rcx*8]
    end
    else
    begin
      E.EmitBytes([$73, $00]); pOOB := E.Len - 1;           // jae oob
      baseR := ArrBaseReg(ArrayId, 8, baseR);
      ArrDataAccess(True, False, baseR);
      E.EmitBytes([$EB, $00]); pDone := E.Len - 1;          // jmp done
      E.PatchByte(pOOB, Byte(E.Len - (pOOB + 1)));
      E.EmitBytes([$0F, $57, $C0]);                          // xorps xmm0,xmm0  -> 0.0
      E.PatchByte(pDone, Byte(E.Len - (pDone + 1)));
    end;
    FStore(DstReg, XMM0);                                    // FloatRegs[Dst] := xmm0 (reg or memory)
  end;

  // arr[idx] := FloatRegs[Val].
  procedure ArrStoreF(apc, ArrayId, IdxReg, ValReg: Integer; Classic, Safe: Boolean);
  var pSkip, ix, baseR: Integer;
  begin
    ix := CArrIdx(ArrayId);
    ILoad(RCX, IdxReg);                                     // rcx := index (reg or [rbx+idx])
    if not Safe then
      if ix >= 0 then ArrCountCmp(ArrayId, ActiveCount(ix)) else ArrCountCmp(ArrayId, -1);
    if ix >= 0 then baseR := ActiveBase(ix) else baseR := -1;
    if Safe then
    begin
      baseR := ArrBaseReg(ArrayId, 8, baseR);
      FLoad(XMM0, ValReg);
      ArrDataAccess(True, True, baseR);                     // movsd [base+rcx*8], xmm0
    end
    else if Classic then
    begin
      EmitClassicBoundsGuard(apc);
      baseR := ArrBaseReg(ArrayId, 8, baseR);
      FLoad(XMM0, ValReg);
      ArrDataAccess(True, True, baseR);                     // movsd [base+rcx*8], xmm0
    end
    else
    begin
      E.EmitBytes([$73, $00]); pSkip := E.Len - 1;          // jae skip
      baseR := ArrBaseReg(ArrayId, 8, baseR);
      FLoad(XMM0, ValReg);
      ArrDataAccess(True, True, baseR);
      E.PatchByte(pSkip, Byte(E.Len - (pSkip + 1)));
    end;
  end;

  // IntRegs[Dst] := arr[idx]. Int arrays are Int64-per-element (IntData at desc+0); the interpreter stores
  // the raw register value (narrowing happens on a separate op), so this is exact.
  procedure ArrLoadI(apc, ArrayId, IdxReg, DstReg: Integer; Classic, Safe: Boolean);
  var pOOB, pDone, ix, baseR: Integer;
  begin
    ix := CArrIdx(ArrayId);
    ILoad(RCX, IdxReg);                                     // rcx := index
    if not Safe then
      if ix >= 0 then ArrCountCmp(ArrayId, ActiveCount(ix)) else ArrCountCmp(ArrayId, -1);
    if ix >= 0 then baseR := ActiveBase(ix) else baseR := -1;
    if Safe then
    begin
      baseR := ArrBaseReg(ArrayId, 0, baseR);
      ArrDataAccess(False, False, baseR);                   // mov rax, [base+rcx*8]
    end
    else if Classic then
    begin
      EmitClassicBoundsGuard(apc);
      baseR := ArrBaseReg(ArrayId, 0, baseR);
      ArrDataAccess(False, False, baseR);                   // mov rax, [base+rcx*8]
    end
    else
    begin
      E.EmitBytes([$73, $00]); pOOB := E.Len - 1;           // jae oob
      baseR := ArrBaseReg(ArrayId, 0, baseR);
      ArrDataAccess(False, False, baseR);
      E.EmitBytes([$EB, $00]); pDone := E.Len - 1;          // jmp done
      E.PatchByte(pOOB, Byte(E.Len - (pOOB + 1)));
      E.EmitBytes([$48, $31, $C0]);                          // xor rax,rax  -> 0
      E.PatchByte(pDone, Byte(E.Len - (pDone + 1)));
    end;
    IStore(DstReg, RAX);                                    // IntRegs[Dst] := rax (reg or memory)
  end;

  // arr[idx] := IntRegs[Val].
  procedure ArrStoreI(apc, ArrayId, IdxReg, ValReg: Integer; Classic, Safe: Boolean);
  var pSkip, ix, baseR: Integer;
  begin
    ix := CArrIdx(ArrayId);
    ILoad(RCX, IdxReg);                                     // rcx := index
    if not Safe then
      if ix >= 0 then ArrCountCmp(ArrayId, ActiveCount(ix)) else ArrCountCmp(ArrayId, -1);
    if ix >= 0 then baseR := ActiveBase(ix) else baseR := -1;
    if Safe then
    begin
      baseR := ArrBaseReg(ArrayId, 0, baseR);
      ILoad(RAX, ValReg);
      ArrDataAccess(False, True, baseR);                    // mov [base+rcx*8], rax
    end
    else if Classic then
    begin
      EmitClassicBoundsGuard(apc);
      baseR := ArrBaseReg(ArrayId, 0, baseR);
      ILoad(RAX, ValReg);
      ArrDataAccess(False, True, baseR);                    // mov [base+rcx*8], rax
    end
    else
    begin
      E.EmitBytes([$73, $00]); pSkip := E.Len - 1;          // jae skip
      baseR := ArrBaseReg(ArrayId, 0, baseR);
      ILoad(RAX, ValReg);
      ArrDataAccess(False, True, baseR);
      E.PatchByte(pSkip, Byte(E.Len - (pSkip + 1)));
    end;
  end;

  // Record field access (J13): Ctx.Records[handle].{Int,Float}Data[slot]. A SHARED_REC_FLAG handle (bit 62)
  // routes to the locked cross-thread region -> deopt to the interpreter. A plain handle indexes the stable
  // per-thread heap: deref @Ctx.Records to the current base, add handle*RecSize, load the field data pointer
  // (a dynamic array = a pointer) at its offset, then load/store [fieldptr + slot*8]. No handle/slot bounds
  // check, matching the interpreter (range checks off). HandleReg = Src1, ValDstReg = Dest/Src2, Slot = Imm.
  procedure RecAccess(apc, HandleReg, Slot, ValDstReg: Integer; IsFloat, IsStore: Boolean);
  var p, Ofs, W: Integer;
  begin
    // A3-i: Slot is no longer an index into a slot array. It carries the field's BYTE OFFSET in
    // bits 4..31 and its width code in bits 0..3, and the record's numeric halves are one byte
    // image - so both "bank" offsets name the same field and the access is [ptr + offset].
    // The widths are emitted here rather than deopted: a deopt is NOT a local cost - measured on
    // job/tests/bench/udt_floor.bas, sending narrow fields to the interpreter cost the arms that
    // came AFTER them too, because leaving takes the whole compiled region with it.
    W := Slot and $F;
    Ofs := Slot shr 4;
    ILoad(RAX, HandleReg);                          // rax = handle
    E.EmitBytes([$48, $0F, $BA, $E0, 62]);          // bt rax, 62  (SHARED_REC_FLAG = 1 shl 62)
    E.EmitBytes([$73, $00]); p := E.Len - 1;        // jnc +over  (CF=0 -> not shared -> fast path)
    DeoptTo(apc);                                    // shared record -> interpreter (takes the lock)
    E.PatchByte(p, Byte(E.Len - (p + 1)));
    LoadCtxFieldRdx(RecordsOff);                     // rdx = current @Ctx.Records[0] (via the ctx slot)
    E.EmitBytes([$48, $69, $C0]); E.Emit32(LongWord(RecSize));   // imul rax, rax, RecSize
    E.EmitBytes([$48, $01, $C2]);                    // add rdx, rax    -> @Records[handle]
    E.EmitBytes([$48, $8B, $8A]);                    // mov rcx, [rdx + fieldoff]  -> field data pointer
    if IsFloat then E.Emit32(LongWord(RecFloatOff)) else E.Emit32(LongWord(RecIntOff));
    if IsStore then
    begin
      if IsFloat then
      begin
        FLoad(XMM0, ValDstReg);
        if W = 7 then
        begin
          E.EmitBytes([$F2, $0F, $5A, $C0]);                               // cvtsd2ss xmm0, xmm0
          E.EmitBytes([$F3, $0F, $11, $81]); E.Emit32(LongWord(Ofs));      // movss [rcx+ofs], xmm0
        end
        else
        begin
          E.EmitBytes([$F2, $0F, $11, $81]); E.Emit32(LongWord(Ofs));      // movsd [rcx+ofs], xmm0
        end;
      end
      else
      begin
        ILoad(RAX, ValDstReg);
        case W of
          1, 2: begin E.EmitBytes([$88, $81]); E.Emit32(LongWord(Ofs)); end;        // mov [rcx+ofs], al
          3, 4: begin E.EmitBytes([$66, $89, $81]); E.Emit32(LongWord(Ofs)); end;   // mov [rcx+ofs], ax
          5, 6: begin E.EmitBytes([$89, $81]); E.Emit32(LongWord(Ofs)); end;        // mov [rcx+ofs], eax
        else    begin E.EmitBytes([$48, $89, $81]); E.Emit32(LongWord(Ofs)); end;   // mov [rcx+ofs], rax
        end;
      end;
    end
    else
    begin
      if IsFloat then
      begin
        if W = 7 then
        begin
          E.EmitBytes([$F3, $0F, $5A, $81]); E.Emit32(LongWord(Ofs));      // cvtss2sd xmm0, [rcx+ofs]
        end
        else
        begin
          E.EmitBytes([$F2, $0F, $10, $81]); E.Emit32(LongWord(Ofs));      // movsd xmm0, [rcx+ofs]
        end;
        FStore(ValDstReg, XMM0);
      end
      else
      begin
        case W of
          1: begin E.EmitBytes([$48, $0F, $BE, $81]); E.Emit32(LongWord(Ofs)); end; // movsx rax, byte
          2: begin E.EmitBytes([$48, $0F, $B6, $81]); E.Emit32(LongWord(Ofs)); end; // movzx rax, byte
          3: begin E.EmitBytes([$48, $0F, $BF, $81]); E.Emit32(LongWord(Ofs)); end; // movsx rax, word
          4: begin E.EmitBytes([$48, $0F, $B7, $81]); E.Emit32(LongWord(Ofs)); end; // movzx rax, word
          5: begin E.EmitBytes([$48, $63, $81]); E.Emit32(LongWord(Ofs)); end;      // movsxd rax, dword
          6: begin E.EmitBytes([$8B, $81]); E.Emit32(LongWord(Ofs)); end;           // mov eax, dword (zx)
        else begin E.EmitBytes([$48, $8B, $81]); E.Emit32(LongWord(Ofs)); end;      // mov rax, qword
        end;
        IStore(ValDstReg, RAX);
      end;
    end;
  end;

  // Scan the loop for FLOAT register operands. Mark=False: compute FMaxReg. Mark=True: flag each used
  // float reg as -2 in FLoc (candidate for allocation).
  procedure ScanF(Mark: Boolean);
  var q: Integer; J: PBcInstr;
    procedure T(r: Word);
    begin
      if Mark then FLoc[r] := -2
      else if r > FMaxReg then FMaxReg := r;
    end;
  begin
    for q := HeaderPC to EndPC do
    begin
      J := @Prog[q];
      case J^.OpCode of
        bcLoadConstFloat, bcIntToFloat: T(J^.Dest);
        bcCopyFloat, bcMathSqr: begin T(J^.Dest); T(J^.Src1); end;
        bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat: begin T(J^.Dest); T(J^.Src1); T(J^.Src2); end;
        bcArrayLoadFloat:  T(J^.Dest);               // Dest = loaded float
        bcArrayStoreFloat: T(J^.Dest);               // Dest = stored VALUE (float)
        bcXferStoreFloat:  T(J^.Src1);               // Src1 = value moved to the transfer slot
        bcXferLoadFloat:   T(J^.Dest);               // Dest = value moved from the transfer slot
        bcRecordLoadFloat: T(J^.Dest);               // Dest = loaded record field (float)
        bcRecordStoreFloat: T(J^.Src2);              // Src2 = stored VALUE (float); Src1 = handle (int)
        bcCmpLtFloat, bcCmpLeFloat, bcCmpGtFloat, bcCmpGeFloat, bcCmpEqFloat, bcCmpNeFloat:
          begin T(J^.Src1); T(J^.Src2); end;         // float operands (Dest is an int reg -> ScanI)
        bcFloatToInt: T(J^.Src1);                    // float input (Dest is an int reg -> ScanI)
        bcFloatRound: T(J^.Src1);                    // CINT: float input, int Dest (-> ScanI)
        bcNegFloat, bcNarrowSingle, bcMathInt, bcMathAbs, bcMathSgn, bcMathFix:
          begin T(J^.Dest); T(J^.Src1); end;
        // J15: VAL is the only leaf-called string primitive that answers in the FLOAT bank.
        bcStrVal: T(J^.Dest);
      end;
    end;
  end;

  // Scan the loop for INTEGER register operands. Mark=False: compute IMaxReg. Mark=True: flag each
  // used int reg as -2 in ILoc (allocation candidate). CAUTION: for bcArrayLoad/StoreFloat, Src1 is
  // the array id (a constant), NOT a register -- only Src2 (the index) is an int register.
  procedure ScanI(Mark: Boolean);
  var q: Integer; J: PBcInstr;
    procedure T(r: Word);
    begin
      if Mark then ILoc[r] := -2
      else if r > IMaxReg then IMaxReg := r;
    end;
  begin
    for q := HeaderPC to EndPC do
    begin
      J := @Prog[q];
      case J^.OpCode of
        bcLoadConstInt: T(J^.Dest);
        bcCopyInt: begin T(J^.Dest); T(J^.Src1); end;
        bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt:
          begin T(J^.Dest); T(J^.Src1); T(J^.Src2); end;
        bcNarrowInt: begin T(J^.Dest); T(J^.Src1); end;
        bcArrayLBound, bcArrayUBound: begin T(J^.Dest); T(J^.Src2); end;  // Dest=result, Src2=dim
        bcCmpLtInt, bcCmpLeInt, bcCmpGtInt, bcCmpGeInt, bcCmpEqInt, bcCmpNeInt:
          begin T(J^.Dest); T(J^.Src1); T(J^.Src2); end;
        bcIntToFloat: T(J^.Src1);                    // int input (Dest is float)
        bcArrayLoadFloat, bcArrayStoreFloat: T(J^.Src2);   // Src2 = index; Src1 is the array id
        bcArrayLoadInt, bcArrayStoreInt: begin T(J^.Dest); T(J^.Src2); end;  // Dest=result/value, Src2=index
        bcXferStoreInt: T(J^.Src1);                  // Src1 = value moved to the transfer slot
        bcXferLoadInt:  T(J^.Dest);                  // Dest = value moved from the transfer slot
        bcRecordLoadInt:    begin T(J^.Dest); T(J^.Src1); end;   // Dest=field value, Src1=handle
        bcRecordStoreInt:   begin T(J^.Src1); T(J^.Src2); end;   // Src1=handle, Src2=stored value
        bcRecordLoadFloat, bcRecordStoreFloat: T(J^.Src1);       // Src1=handle (int); value is a float reg
        bcCmpLtFloat, bcCmpLeFloat, bcCmpGtFloat, bcCmpGeFloat, bcCmpEqFloat, bcCmpNeFloat:
          T(J^.Dest);                                // float compare writes an int result reg
        bcFloatToInt, bcFloatRound: T(J^.Dest);      // float->int writes an int result reg
        bcJumpIfZero, bcJumpIfNotZero: T(J^.Src1);
        // ⛔ EVERY opcode the emitter lowers must appear in this scanner, and the failure mode is not
        // a missed optimisation: ILoc is sized to IMaxReg+2 from the pass below, and IAlloc indexes it
        // WITHOUT a bounds check - so a register mentioned only by a forgotten opcode reads past the
        // array and can come back as a plausible GPR number. Silent miscompile, not a bail.
        bcNegInt, bcBitwiseNot, bcBitClz, bcBitCtz, bcBitPopcnt:
          begin T(J^.Dest); T(J^.Src1); end;
        bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor,
        bcShl, bcShr, bcShrUInt, bcDivUInt, bcModUInt,
        bcBitRotl, bcBitRotr,
        bcCmpLtUInt, bcCmpLeUInt, bcCmpGtUInt, bcCmpGeUInt:
          begin T(J^.Dest); T(J^.Src1); T(J^.Src2); end;
        // J15, and this is the ⛔ above applied: the string family is lowered to leaf calls now, so
        // every INT register it reads or writes has to be declared here. A string op is not exempt
        // from the scanner just because its other operands live in a bank this JIT never allocates.
        bcStrLen, bcStrAsc, bcStrValInt: T(J^.Dest);                     // Dest = int result
        bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString: T(J^.Dest);
        bcStrLeft, bcStrRight: T(J^.Src2);                               // Src2 = the length
        bcStrChr, bcIntToString: T(J^.Src1);                             // Src1 = the int operand
        bcStrMid: begin T(J^.Src2); T(Word(J^.Immediate and $FFFF)); end;   // start + length REGISTER
        bcStrInstr: begin T(J^.Dest); T(Word(J^.Immediate and $FFFF)); end; // result + start REGISTER
        bcStrAscMid: begin T(J^.Dest); T(J^.Src2); T(Word(J^.Immediate and $FFFF)); end;
        bcStrConcatCharAt, bcStrAppendMapped: T(Word(J^.Immediate and $FFFF));  // the index REGISTER
        bcRecordNew: T(J^.Dest);            // Src1/Src2/Immediate are slot COUNTS, not registers
        bcRecordFree: T(J^.Src1);           // the handle
      end;
    end;
  end;

  // Collect the distinct arrays accessed in the loop range (with descriptor base offset + use count) so the
  // invariant base/count loads can be hoisted into registers (J5c). Src1 of an array op is the array id (a
  // constant), Src2 the index. The caller uses this over [HeaderPC..EndPC]; an inlined callee builds its own
  // dedicated cache with AllocCalleeArr over its body (J6d).
  procedure ScanArrRange(lo, hi: Integer);
    procedure ScanRange(lo2, hi2: Integer);
    var q, k, off, aid: Integer; J: PBcInstr;
    begin
      for q := lo2 to hi2 do
      begin
        J := @Prog[q];
        case J^.OpCode of
          bcArrayLoadFloat, bcArrayStoreFloat: off := 8;
          bcArrayLoadInt,   bcArrayStoreInt:   off := 0;
        else
          continue;
        end;
        aid := J^.Src1;
        k := 0;
        while (k < NCArr) and (CArrId[k] <> aid) do Inc(k);
        if k = NCArr then
        begin
          Inc(NCArr);
          SetLength(CArrId, NCArr); SetLength(CArrOff, NCArr); SetLength(CArrUses, NCArr);
          SetLength(CArrBase, NCArr); SetLength(CArrCount, NCArr);
          CArrId[k] := aid; CArrOff[k] := off; CArrUses[k] := 0;
          CArrBase[k] := -1; CArrCount[k] := -1;
        end;
        Inc(CArrUses[k]);
      end;
    end;
  begin
    NCArr := 0;
    ScanRange(lo, hi);
  end;

  // Build the inlined callee's dedicated GPR plan (J6d array cache + J6f int regalloc) over its body
  // [ep..rp]. The whole r9..r15 pool is free (the caller's GPRs are saved around the callee), so hand it to
  // the highest-use candidates -- each an array base, an array count, or a callee int register -- by use
  // count. The inner-loop index thus lands in a register instead of reloading from memory on every access.
  // Deterministic (recomputed identically at emit); a callee-saved GPR (r12..r15) claimed is marked SaveGpr.
  procedure AllocCalleeArr(ep, rp: Integer);
  var q, k, off, aid, a, b, poolN, gp: Integer; J: PBcInstr;
    IntUses: array of Integer;
    CandUses, CandKind, CandRef: array of Integer;   // kind 0=int reg, 1=array base, 2=array count
    NCand: Integer;
    procedure Swap(var x, y: Integer); var t: Integer; begin t := x; x := y; y := t; end;
    procedure IU(r: Word);                             // count a callee int-reg use
    begin if IntUses[r] >= 0 then Inc(IntUses[r]); end;
    procedure AddCand(Kind, Ref, UseN: Integer);
    begin
      Inc(NCand); SetLength(CandUses, NCand); SetLength(CandKind, NCand); SetLength(CandRef, NCand);
      CandUses[NCand-1] := UseN; CandKind[NCand-1] := Kind; CandRef[NCand-1] := Ref;
    end;
  begin
    // --- collect arrays with use counts (base+count candidates) ---
    NCArr2 := 0;
    for q := ep to rp do
    begin
      J := @Prog[q];
      case J^.OpCode of
        bcArrayLoadFloat, bcArrayStoreFloat: off := 8;
        bcArrayLoadInt,   bcArrayStoreInt:   off := 0;
      else
        continue;
      end;
      aid := J^.Src1;
      k := 0;
      while (k < NCArr2) and (CArr2Id[k] <> aid) do Inc(k);
      if k = NCArr2 then
      begin
        Inc(NCArr2);
        SetLength(CArr2Id, NCArr2); SetLength(CArr2Off, NCArr2); SetLength(CArr2Uses, NCArr2);
        SetLength(CArr2Base, NCArr2); SetLength(CArr2Count, NCArr2);
        CArr2Id[k] := aid; CArr2Off[k] := off; CArr2Uses[k] := 0;
        CArr2Base[k] := -1; CArr2Count[k] := -1;
      end;
      Inc(CArr2Uses[k]);
    end;
    // --- collect callee int registers with use counts ---
    ICalleeMax := -1;
    for q := ep to rp do
    begin
      J := @Prog[q];
      case J^.OpCode of
        bcLoadConstInt, bcFloatToInt: if J^.Dest > ICalleeMax then ICalleeMax := J^.Dest;
        bcCopyInt, bcNarrowInt: begin if J^.Dest > ICalleeMax then ICalleeMax := J^.Dest; if J^.Src1 > ICalleeMax then ICalleeMax := J^.Src1; end;
        bcAddInt, bcSubInt, bcMulInt, bcCmpLtInt, bcCmpLeInt, bcCmpGtInt, bcCmpGeInt, bcCmpEqInt, bcCmpNeInt:
          begin if J^.Dest > ICalleeMax then ICalleeMax := J^.Dest; if J^.Src1 > ICalleeMax then ICalleeMax := J^.Src1; if J^.Src2 > ICalleeMax then ICalleeMax := J^.Src2; end;
        bcIntToFloat, bcJumpIfZero, bcJumpIfNotZero: if J^.Src1 > ICalleeMax then ICalleeMax := J^.Src1;
        bcArrayLoadFloat, bcArrayStoreFloat: if J^.Src2 > ICalleeMax then ICalleeMax := J^.Src2;
        bcArrayLoadInt, bcArrayStoreInt: begin if J^.Dest > ICalleeMax then ICalleeMax := J^.Dest; if J^.Src2 > ICalleeMax then ICalleeMax := J^.Src2; end;
        bcXferStoreInt: if J^.Src1 > ICalleeMax then ICalleeMax := J^.Src1;
        bcXferLoadInt:  if J^.Dest > ICalleeMax then ICalleeMax := J^.Dest;
      end;
    end;
    SetLength(ILoc2, ICalleeMax + 2);
    SetLength(IntUses, ICalleeMax + 2);
    for a := 0 to High(ILoc2) do begin ILoc2[a] := -1; IntUses[a] := 0; end;
    for q := ep to rp do
    begin
      J := @Prog[q];
      case J^.OpCode of
        bcLoadConstInt, bcFloatToInt: IU(J^.Dest);
        bcCopyInt, bcNarrowInt: begin IU(J^.Dest); IU(J^.Src1); end;
        bcAddInt, bcSubInt, bcMulInt, bcCmpLtInt, bcCmpLeInt, bcCmpGtInt, bcCmpGeInt, bcCmpEqInt, bcCmpNeInt:
          begin IU(J^.Dest); IU(J^.Src1); IU(J^.Src2); end;
        bcIntToFloat, bcJumpIfZero, bcJumpIfNotZero: IU(J^.Src1);
        bcArrayLoadFloat, bcArrayStoreFloat: IU(J^.Src2);
        bcArrayLoadInt, bcArrayStoreInt: begin IU(J^.Dest); IU(J^.Src2); end;
        bcXferStoreInt: IU(J^.Src1);
        bcXferLoadInt:  IU(J^.Dest);
      end;
    end;
    // --- unified candidate list, sorted by use count (base added before count for tie priority) ---
    NCand := 0;
    for a := 0 to NCArr2 - 1 do begin AddCand(1, a, CArr2Uses[a]); AddCand(2, a, CArr2Uses[a]); end;
    for a := 0 to ICalleeMax do if IntUses[a] > 0 then AddCand(0, a, IntUses[a]);
    for a := 0 to NCand - 2 do
      for b := a + 1 to NCand - 1 do
        if CandUses[b] > CandUses[a] then
        begin Swap(CandUses[a], CandUses[b]); Swap(CandKind[a], CandKind[b]); Swap(CandRef[a], CandRef[b]); end;
    // --- assign r9..r15 to the top candidates ---
    poolN := 0;
    for a := 0 to NCand - 1 do
    begin
      if poolN > 6 then Break;
      gp := IntPool[poolN];
      case CandKind[a] of
        0: begin if ILoc2[CandRef[a]] < 0 then begin ILoc2[CandRef[a]] := gp; Inc(poolN); end; end;
        1: begin if CArr2Base[CandRef[a]]  < 0 then begin CArr2Base[CandRef[a]]  := gp; Inc(poolN); end; end;
        2: begin if CArr2Count[CandRef[a]] < 0 then begin CArr2Count[CandRef[a]] := gp; Inc(poolN); end; end;
      end;
    end;
    for a := 9 to 15 do
    begin
      b := 0;
      for k := 0 to NCArr2 - 1 do if (CArr2Base[k] = a) or (CArr2Count[k] = a) then b := 1;
      for k := 0 to ICalleeMax do if ILoc2[k] = a then b := 1;
      if (b = 1) and (a >= 12) then SaveGpr[a] := True;   // callee-saved GPR claimed -> prologue preserves it
    end;
  end;

  // Save / restore the caller's live GPRs (CallerGpr) around an inlined callee, to/from the stack scratch at
  // GprSaveDisp, so the callee may use the whole r9..r15 pool for its dedicated array cache.
  procedure EmitSaveCallerGpr;
  var i, N: Integer;
  begin
    for i := 0 to NCallerGpr - 1 do
    begin
      N := CallerGpr[i];                                         // mov [rsp+disp], rN
      if N >= 8 then E.Emit8($4C) else E.Emit8($48);            // REX.W (+R for r8..r15)
      E.Emit8($89); E.Emit8($84 or ((N and 7) shl 3)); E.Emit8($24);
      E.Emit32(LongWord(GprSaveDisp + i * 8));
    end;
  end;
  procedure EmitRestoreCallerGpr;
  var i, N: Integer;
  begin
    for i := 0 to NCallerGpr - 1 do
    begin
      N := CallerGpr[i];                                         // mov rN, [rsp+disp]
      if N >= 8 then E.Emit8($4C) else E.Emit8($48);
      E.Emit8($8B); E.Emit8($84 or ((N and 7) shl 3)); E.Emit8($24);
      E.Emit32(LongWord(GprSaveDisp + i * 8));
    end;
  end;

  // Sparse frame save/restore (J6e): save the listed memory-homed caller regs (raw qwords, rax scratch)
  // to the stack scratch at [rsp+Disp+i*8]. Restore is the reverse. Emits nothing when the list is empty.
  procedure EmitSaveSparse(BankBase: Byte; const Regs: array of Integer; NRegs: Integer; Disp: LongWord);
  var i: Integer;
  begin
    for i := 0 to NRegs - 1 do
    begin
      E.MemOp([$48, $8B], RAX, BankBase, LongWord(Regs[i]) * 8);     // mov rax, [bank+reg*8]
      E.EmitBytes([$48, $89, $84, $24]); E.Emit32(Disp + LongWord(i) * 8);  // mov [rsp+Disp+i*8], rax
    end;
  end;
  procedure EmitRestoreSparse(BankBase: Byte; const Regs: array of Integer; NRegs: Integer; Disp: LongWord);
  var i: Integer;
  begin
    for i := 0 to NRegs - 1 do
    begin
      E.EmitBytes([$48, $8B, $84, $24]); E.Emit32(Disp + LongWord(i) * 8);  // mov rax, [rsp+Disp+i*8]
      E.MemOp([$48, $89], RAX, BankBase, LongWord(Regs[i]) * 8);     // mov [bank+reg*8], rax
    end;
  end;

  { ---------------- J14: the helper route ----------------

    An instruction with no native form is handed to the INTERPRETER, one instruction at a time,
    and native execution resumes after it. This is EmitHelperCall from the AOT (SedaiAot.pas),
    transcribed: the JIT lacked it only because nobody had passed it a channel to the interpreter.

    What may be routed, and what may NOT (the case labels are the authority; this is the reason):
      * NOT the array family. ExecuteArrayOp sets FArraysDirty on EVERY operation, and the
        descriptor table it may rebuild is held in r8 for the whole invocation, with element base
        pointers cached in the GPR pool. The AOT survives that because AotExecOne refreshes ArrDesc
        in its ctx record; the JIT has no such record and passes nil as the 4th argument, which
        SKIPS that refresh - correct only as long as nothing routed can dirty the table.
      * NOT inside an inlined callee or GOSUB body. Their caller registers are parked on the stack
        (not in the banks), so the flush below would write the wrong values back - the same reason
        DeoptTo is refused there.
    Everything else is safe by construction, because of two things that are NOT obvious:
      * The flush/reload is COMPLETE, so the routed opcodes need no entry in ScanI/ScanF: no
        ILoad/IStore is emitted for them, so ILoc/FLoc are never indexed with a register that only
        they mention - which is the one way this JIT miscompiles in silence.
      * The PC guard at the end makes a static list of "opcodes that move the PC" unnecessary. The
        helper reports where the interpreter would go next; native code continues ONLY if that is
        exactly apc+1. Anything else - a moved PC, or one of AotExecOne's negative sentinels for a
        raised exception / a cleared Running - leaves through the epilogue with that value in rax,
        which is already the epilogue's contract (and the run loop now resolves it with AotSettle,
        exactly as it does for the AOT). }
  procedure EmitHelperCall(apc: Integer);
  var k: Integer;
  begin
    // 1. Flush every allocated VM register to its bank slot - the same stores the epilogue emits.
    //    The helper runs an interpreter handler that reads and WRITES the banks, so no value may
    //    stay in a machine register across the call. (rbx, the int bank base, is callee-saved on
    //    both ABIs and survives; r8 and rsi do not, hence the two scratch slots below.)
    for k := 0 to IMaxReg do
      if ILoc[k] >= 0 then StoreRegMem(ILoc[k], LongWord(k) * 8);
    for k := 0 to FMaxReg do
      if FLoc[k] >= 0 then E.MemOp([$F2, $0F, $11], FLoc[k], RSI, LongWord(k) * 8);
    // 2. Park the two base registers that have no bank slot, BEFORE the argument setup clobbers
    //    them (r8 is arg2 on Win64; rsi is arg1 on System V and the float bank base here).
    E.EmitBytes([$4C, $89, $84, $24]); E.Emit32(LongWord(ArrDescDisp));   // mov [rsp+ArrDescDisp], r8
    E.EmitBytes([$48, $89, $B4, $24]); E.Emit32(LongWord(FltSaveDisp));   // mov [rsp+FltSaveDisp], rsi
    // 3. Arguments: AotExecOne(VMSelf, CtxObj, apc, nil). VMSelf and the helper address are baked -
    //    both are fixed for the life of this code - while the CONTEXT is read from its frame slot,
    //    so a worker thread running this same loop hands the helper its own state. The 4th argument
    //    is nil on purpose: see the array note above.
    {$IFDEF WINDOWS}
    MovImm64(RCX, PtrInt(VMSelf));                                        // arg0 = VMSelf
    E.EmitBytes([$48, $8B, $94, $24]); E.Emit32(LongWord(CtxDisp));       // arg1 = rdx = ctx object
    MovImm64(R8, apc);                                                    // arg2 = the bytecode PC
    E.EmitBytes([$45, $31, $C9]);                                         // arg3 = xor r9d, r9d (nil)
    {$ELSE}
    MovImm64(RDI, PtrInt(VMSelf));                                        // arg0 = VMSelf
    E.EmitBytes([$48, $8B, $B4, $24]); E.Emit32(LongWord(CtxDisp));       // arg1 = rsi = ctx object
    MovImm64(RDX, apc);                                                   // arg2 = the bytecode PC
    E.EmitBytes([$31, $C9]);                                              // arg3 = xor ecx, ecx (nil)
    {$ENDIF}
    MovImm64(RAX, PtrInt(HelperFn));                                      // rax = @AotExecOne
    // 4. The first call inside JIT-generated code: Win64 wants 32 bytes of shadow space and both
    //    ABIs want rsp 16-aligned AT the call. Both are static - the whole frame displacement is
    //    known at emission - so the adjustment is a constant computed once (HelperAdjust). The
    //    scratch reads above happen BEFORE it, so their fixed offsets stay valid.
    if HelperAdjust > 0 then
    begin
      E.EmitBytes([$48, $83, $EC]); E.Emit8(Byte(HelperAdjust));          // sub rsp, HelperAdjust
    end;
    E.EmitBytes([$FF, $D0]);                                              // call rax
    if HelperAdjust > 0 then
    begin
      E.EmitBytes([$48, $83, $C4]); E.Emit8(Byte(HelperAdjust));          // add rsp, HelperAdjust
    end;
    // 5. Rebuild everything the call destroyed: the two base registers, then the banks (the helper
    //    may have written any of them), then the array base/count cache - which is NOT covered by
    //    the bank reload, because it lives in the GPR pool whose first three entries (r9/r10/r11)
    //    are caller-saved. A cached base has no bank slot; only a re-read from the descriptor
    //    rebuilds it. (Reloading is idempotent, and emits nothing in a loop with no cache.)
    E.EmitBytes([$4C, $8B, $84, $24]); E.Emit32(LongWord(ArrDescDisp));   // mov r8,  [rsp+ArrDescDisp]
    E.EmitBytes([$48, $8B, $B4, $24]); E.Emit32(LongWord(FltSaveDisp));   // mov rsi, [rsp+FltSaveDisp]
    for k := 0 to IMaxReg do
      if ILoc[k] >= 0 then LoadRegMem(ILoc[k], LongWord(k) * 8);
    for k := 0 to FMaxReg do
      if FLoc[k] >= 0 then E.MemOp([$F2, $0F, $10], FLoc[k], RSI, LongWord(k) * 8);
    for k := 0 to NCArr - 1 do
    begin
      if CArrBase[k]  >= 0 then R8LoadR(CArrBase[k],  LongWord(CArrId[k]) * 32 + LongWord(CArrOff[k]));
      if CArrCount[k] >= 0 then R8LoadR(CArrCount[k], LongWord(CArrId[k]) * 32 + 16);
    end;
    // 6. Continue natively only if the interpreter landed exactly where this code expects.
    E.EmitBytes([$48, $3D]); E.Emit32(LongWord(apc + 1));                 // cmp rax, apc+1
    JccRel($85, -1);                                                      // jne epilogue
  end;

  { ---------------- J15: the STRING family as native LEAF calls ----------------

    Why this exists, measured rather than assumed: routing a string op through EmitHelperCall above
    costs ~46 ns - a flush of EVERY allocated register, a re-entry into the interpreter's dispatch,
    and a full reload - and on a loop that is mostly strings that is a 28% LOSS against not compiling
    it at all. The AOT is 15% FASTER than the interpreter on the very same loop, doing the identical
    string work, because it reaches the primitives as leaf calls: only the ABI-VOLATILE registers are
    spilled and the interpreter's dispatch is never re-entered. It is not the strings; it is the route.
    (The AOT's own C6 comment records the same lesson from the record family: the flush around each
    allocation cost more than the allocation.)

    So the primitives are the AOT's, unchanged - the same Pascal functions, called with the same
    arguments. Two things had to be answered for the JIT rather than copied:
      * WHERE the string bank is. The AOT keeps it in its ctx record; here it is per-CONTEXT, so it is
        read at run time from the context object's StringRegs field through the frame slot the Xfer
        and record accessors already use. Nothing context-specific is baked, so a worker is safe.
      * WHERE the primitives are. They are static functions, fixed for the life of the compiled code,
        and the dialect-variant ones (MID$ and the fused pair) are already resolved by the time a loop
        is compiled - so their addresses are read out of the VM's primitive table AT COMPILE TIME and
        baked as imm64. One instruction, no indirection, and the table is the same one the AOT fills,
        which is what stops the two backends from drifting apart. }

  // mov <dst>, [<base> + disp32]   (any base including rsp/r12, any dst; REX computed)
  procedure MovLoadR(dst, base: Integer; disp: LongWord);
  var rex: Byte;
  begin
    rex := $48;
    if dst >= 8 then rex := rex or $04;                       // REX.R
    if base >= 8 then rex := rex or $01;                      // REX.B
    E.Emit8(rex); E.Emit8($8B);
    E.Emit8($80 or ((dst and 7) shl 3) or (base and 7));
    if (base and 7) = 4 then E.Emit8($24);                    // rsp/r12 addressing needs a SIB byte
    E.Emit32(disp);
  end;
  // lea <dst>, [<base> + disp32]
  procedure LeaR(dst, base: Integer; disp: LongWord);
  var rex: Byte;
  begin
    rex := $48;
    if dst >= 8 then rex := rex or $04;
    if base >= 8 then rex := rex or $01;
    E.Emit8(rex); E.Emit8($8D);
    E.Emit8($80 or ((dst and 7) shl 3) or (base and 7));
    if (base and 7) = 4 then E.Emit8($24);
    E.Emit32(disp);
  end;
  // Read VM int register vmreg into ANY native register (unlike ILoad, whose scratch must be < 8).
  // Called BEFORE the call, while every pooled register still holds what the allocator says: the
  // spill below only STORES, it does not clobber.
  procedure ILoadTo(dst, vmreg: Integer);
  var n: Integer;
  begin
    n := IAlloc(vmreg);
    if n >= 0 then MovRR(dst, n) else MovLoadR(dst, RBX, LongWord(vmreg) * 8);
  end;

  // Is this native register preserved across a call by the ABI? r12..r15 and rbx are on both; the
  // xmm answer differs - Win64 preserves xmm6/xmm7, System V preserves none.
  function GprSurvives(n: Integer): Boolean;
  begin Result := (n = RBX) or (n >= R12); end;
  function XmmSurvives(n: Integer): Boolean;
  begin
    {$IFDEF WINDOWS} Result := n >= 6; {$ELSE} Result := False; {$ENDIF}
  end;

  // Caller-saved spill around a leaf call: ONLY the volatile allocated registers, which is the whole
  // difference from EmitHelperCall's flush of everything. Their bank slot is their canonical home.
  procedure SpillVol;
  var k: Integer;
  begin
    for k := 0 to IMaxReg do
      if (ILoc[k] >= 0) and not GprSurvives(ILoc[k]) then StoreRegMem(ILoc[k], LongWord(k) * 8);
    for k := 0 to FMaxReg do
      if (FLoc[k] >= 0) and not XmmSurvives(FLoc[k]) then
        E.MemOp([$F2, $0F, $11], FLoc[k], RSI, LongWord(k) * 8);
  end;
  // Park the two base registers a call destroys and that have no bank slot: r8 (the array descriptor
  // table) and rsi (the float bank base - callee-saved on Win64, VOLATILE on System V, where it is
  // also an argument register). Must run BEFORE any argument setup.
  procedure LeafSaveBases;
  begin
    E.EmitBytes([$4C, $89, $84, $24]); E.Emit32(LongWord(ArrDescDisp));   // mov [rsp+ArrDescDisp], r8
    E.EmitBytes([$48, $89, $B4, $24]); E.Emit32(LongWord(FltSaveDisp));   // mov [rsp+FltSaveDisp], rsi
  end;
  // rax := the string bank base of the EXECUTING context. Read through the ctx slot, never baked.
  // ⚠️ Uses an rsp-relative displacement, so it must be emitted BEFORE the stack adjustment.
  procedure StrBaseRax;
  begin
    E.EmitBytes([$48, $8B, $84, $24]); E.Emit32(LongWord(CtxDisp));       // mov rax, [rsp+CtxDisp]
    E.EmitBytes([$48, $8B, $80]); E.Emit32(LongWord(StringRegsOff));      // mov rax, [rax+StringRegsOff]
  end;
  // The call itself: the target goes in rax (never an argument register on either ABI), so it is
  // staged LAST, after every argument - including the ones read through rax as the bank base.
  procedure LeafCall(prim: Pointer);
  begin
    MovImm64(RAX, PtrInt(prim));
    if HelperAdjust > 0 then
    begin
      E.EmitBytes([$48, $83, $EC]); E.Emit8(Byte(HelperAdjust));          // sub rsp, shadow+pad
    end;
    E.EmitBytes([$FF, $D0]);                                              // call rax
    if HelperAdjust > 0 then
    begin
      E.EmitBytes([$48, $83, $C4]); E.Emit8(Byte(HelperAdjust));          // add rsp, shadow+pad
    end;
  end;
  // After the call: the base registers, then the volatile allocated ones, then the array base/count
  // cache - which no bank reload covers, because it lives in the caller-saved end of the GPR pool and
  // has no bank slot at all. Reloading it is idempotent and emits nothing when there is no cache.
  procedure LeafRestore;
  var k: Integer;
  begin
    E.EmitBytes([$4C, $8B, $84, $24]); E.Emit32(LongWord(ArrDescDisp));   // mov r8,  [rsp+ArrDescDisp]
    E.EmitBytes([$48, $8B, $B4, $24]); E.Emit32(LongWord(FltSaveDisp));   // mov rsi, [rsp+FltSaveDisp]
    for k := 0 to IMaxReg do
      if (ILoc[k] >= 0) and not GprSurvives(ILoc[k]) then LoadRegMem(ILoc[k], LongWord(k) * 8);
    for k := 0 to FMaxReg do
      if (FLoc[k] >= 0) and not XmmSurvives(FLoc[k]) then
        E.MemOp([$F2, $0F, $10], FLoc[k], RSI, LongWord(k) * 8);
    for k := 0 to NCArr - 1 do
    begin
      if CArrBase[k]  >= 0 then R8LoadR(CArrBase[k],  LongWord(CArrId[k]) * 32 + LongWord(CArrOff[k]));
      if CArrCount[k] >= 0 then R8LoadR(CArrCount[k], LongWord(CArrId[k]) * 32 + 16);
    end;
  end;
  // The primitive's address, read from the VM's table at COMPILE time (see the header above).
  function Prim(CtxOff: Integer): Pointer;
  begin Result := PPointer(PtrUInt(PrimCtx) + PtrUInt(CtxOff))^; end;

  { The emitters. Argument order is the AOT's, and it is not stylistic: on Win64 arg2 IS r8 (already
    parked) and arg3 IS r9, which lives in the allocation pool - so every operand that may be read
    out of a pooled register is read BEFORE arg3 is written. }

  // dst := StringRegs[src]  /  dst := StringConstants[imm]  /  dst := IntRegs[src] as text
  procedure EmitStrOneSlot(CtxOff, dSlot, sSlot: Integer);       // (dstSlot, srcVal)
  begin
    SpillVol; LeafSaveBases;
    StrBaseRax;
    LeaR(ABI_ARG0, RAX, LongWord(dSlot) * 8);                    // arg0 = &StringRegs[dest]
    MovLoadR(ABI_ARG1, RAX, LongWord(sSlot) * 8);                // arg1 = StringRegs[src] (value)
    LeafCall(Prim(CtxOff));
    LeafRestore;
  end;
  procedure EmitStrLoadConstJ(dSlot: Integer; imm: Int64);       // (dstSlot, VMSelf, imm)
  begin
    SpillVol; LeafSaveBases;
    StrBaseRax;
    LeaR(ABI_ARG0, RAX, LongWord(dSlot) * 8);
    MovImm64(ABI_ARG1, PtrInt(VMSelf));
    MovImm64(ABI_ARG2, imm);
    LeafCall(Prim(AOTCTX_STRLOADCONST));
    LeafRestore;
  end;
  procedure EmitIntToStringJ(dSlot, vReg: Integer);              // (dstSlot, v)
  begin
    SpillVol; LeafSaveBases;
    ILoadTo(ABI_ARG1, vReg);                                     // read the pooled operand FIRST
    StrBaseRax;
    LeaR(ABI_ARG0, RAX, LongWord(dSlot) * 8);
    LeafCall(Prim(AOTCTX_STRINTTOSTR));
    LeafRestore;
  end;
  procedure EmitStrConcatJ(dSlot, aSlot, bSlot: Integer);        // (dstSlot, aVal, bVal)
  begin
    SpillVol; LeafSaveBases;
    StrBaseRax;
    LeaR(ABI_ARG0, RAX, LongWord(dSlot) * 8);
    MovLoadR(ABI_ARG1, RAX, LongWord(aSlot) * 8);
    MovLoadR(ABI_ARG2, RAX, LongWord(bSlot) * 8);
    LeafCall(Prim(AOTCTX_STRCONCAT));
    LeafRestore;
  end;
  // Dest(int) := primitive(StringRegs[src]) - LEN$ and ASC.
  procedure EmitStrToInt(CtxOff, dReg, sSlot: Integer);
  begin
    SpillVol; LeafSaveBases;
    StrBaseRax;
    MovLoadR(ABI_ARG0, RAX, LongWord(sSlot) * 8);
    LeafCall(Prim(CtxOff));
    LeafRestore;
    IStore(dReg, RAX);
  end;
  // dst := LEFT$/RIGHT$(src, n)   (dstSlot, sVal, n)
  procedure EmitStrSliceJ(CtxOff, dSlot, sSlot, nReg: Integer);
  begin
    SpillVol; LeafSaveBases;
    ILoadTo(ABI_ARG2, nReg);                                     // pooled operand before arg3/rax use
    StrBaseRax;
    LeaR(ABI_ARG0, RAX, LongWord(dSlot) * 8);
    MovLoadR(ABI_ARG1, RAX, LongWord(sSlot) * 8);
    LeafCall(Prim(CtxOff));
    LeafRestore;
  end;
  // dst := MID$(src, start, len)   (dstSlot, sVal, start, cnt)
  procedure EmitStrMidJ(dSlot, sSlot, stReg, lnReg: Integer);
  begin
    SpillVol; LeafSaveBases;
    ILoadTo(ABI_ARG2, stReg);
    ILoadTo(ABI_ARG3, lnReg);                                    // r9 on Win64: written LAST of the two
    StrBaseRax;
    LeaR(ABI_ARG0, RAX, LongWord(dSlot) * 8);
    MovLoadR(ABI_ARG1, RAX, LongWord(sSlot) * 8);
    LeafCall(Prim(AOTCTX_STRMID));
    LeafRestore;
  end;
  // Dest(int) := INSTR(start, hay, needle)   (hayVal, needleVal, start)
  procedure EmitStrInstrJ(dReg, haySlot, needleSlot, stReg: Integer);
  begin
    SpillVol; LeafSaveBases;
    ILoadTo(ABI_ARG2, stReg);
    StrBaseRax;
    MovLoadR(ABI_ARG0, RAX, LongWord(haySlot) * 8);
    MovLoadR(ABI_ARG1, RAX, LongWord(needleSlot) * 8);
    LeafCall(Prim(AOTCTX_STRINSTR));
    LeafRestore;
    IStore(dReg, RAX);
  end;
  // dst := CHR$(code)   (dstSlot, code)
  procedure EmitStrChrJ(dSlot, cReg: Integer);
  begin
    SpillVol; LeafSaveBases;
    ILoadTo(ABI_ARG1, cReg);
    StrBaseRax;
    LeaR(ABI_ARG0, RAX, LongWord(dSlot) * 8);
    LeafCall(Prim(AOTCTX_STRCHR));
    LeafRestore;
  end;
  // Dest(float) := VAL(src) - the only one of the family that answers in xmm0.
  procedure EmitStrValJ(dReg, sSlot: Integer);
  begin
    SpillVol; LeafSaveBases;
    StrBaseRax;
    MovLoadR(ABI_ARG0, RAX, LongWord(sSlot) * 8);
    LeafCall(Prim(AOTCTX_STRVAL));
    LeafRestore;
    FStore(dReg, XMM0);
  end;
  // Dest(int) := VALINT(src, decWidth) - the width is an IMMEDIATE, not a register.
  procedure EmitStrValIntJ(dReg, sSlot: Integer; width: Int64);
  begin
    SpillVol; LeafSaveBases;
    StrBaseRax;
    MovLoadR(ABI_ARG0, RAX, LongWord(sSlot) * 8);
    MovImm64(ABI_ARG1, width);
    LeafCall(Prim(AOTCTX_STRVALINT));
    LeafRestore;
    IStore(dReg, RAX);
  end;
  // Dest(int) := (a <cmp> b) ? TrueVal : 0. Kind 0=Eq 1=Ne 2=Lt 3=Gt, the AOT primitive's own coding.
  procedure EmitStrCmpJ(Kind, aSlot, bSlot: Integer);
  begin
    SpillVol; LeafSaveBases;
    StrBaseRax;
    MovLoadR(ABI_ARG0, RAX, LongWord(aSlot) * 8);
    MovLoadR(ABI_ARG1, RAX, LongWord(bSlot) * 8);
    MovImm64(ABI_ARG2, Kind);
    LeafCall(Prim(AOTCTX_STRCMP));
    LeafRestore;
    CmpBoolToDest;                                               // al (0/1) -> Dest := TrueVal/0
  end;

  // Dest(int) := ASC(MID$(src, start, len)) without building the substring.
  // ⚠️ The primitive's second and FOURTH parameters are start and length; the third is a filler that
  // exists only so the argument shape matches the family. Read from the DECLARATION, not from the
  // ctx comment, which lists them in a different order.
  //
  // FAST PATH, transcribed from the AOT's and gated on the SAME predicate (AotAscMidInline), which is
  // exported for exactly that reason: it answers "may emitted code step into a string header on this
  // runtime?", and two engines answering it separately is how one of them ends up reading garbage as
  // a length. Reading one byte out of a string is three machine instructions - load the data pointer,
  // check the bound, movzx - and paying a whole call for them was measured at 25% of this loop.
  // The inline covers the ONE shape that is dialect-blind: length exactly 1 with 1 <= start <= Len(s).
  // Everything else - any other length, a start outside the string, an empty (nil) buffer - falls
  // through to the call below, which keeps all the rules and stays the single place they are written.
  //
  // rax/rcx/rdx are safe scratch: the allocation pool is r9..r15, so it never hands them out, and the
  // fast path runs BEFORE SpillVol while every pooled value is still where ILoad expects it. Both arms
  // converge on "result in rax" with the pool untouched, so the join needs no fixing up.
  procedure EmitStrAscMidJ(dReg, sSlot, stReg, lnReg: Integer);
  var pLen, pNil, pLo, pHi, pDone: Integer;
  begin
    pDone := -1;
    if AotAscMidInline then
    begin
      StrBaseRax;                                                // rax = string bank base
      MovLoadR(RAX, RAX, LongWord(sSlot) * 8);                   // rax = StringRegs[src] (data pointer)
      ILoad(RCX, stReg);                                         // rcx = start  (pre-spill: pool intact)
      ILoad(RDX, lnReg);                                         // rdx = length
      E.EmitBytes([$48, $83, $FA, $01]);                         // cmp rdx, 1
      E.EmitBytes([$75, $00]); pLen := E.Len - 1;                // jne slow  (only length 1 is blind)
      E.EmitBytes([$48, $85, $C0]);                              // test rax, rax
      E.EmitBytes([$74, $00]); pNil := E.Len - 1;                // jz  slow  (empty string -> 0)
      E.EmitBytes([$48, $83, $F9, $01]);                         // cmp rcx, 1
      E.EmitBytes([$7C, $00]); pLo := E.Len - 1;                 // jl  slow  (start < 1: dialects differ)
      E.EmitBytes([$48, $3B, $48, $F8]);                         // cmp rcx, [rax-8]  (the length field)
      E.EmitBytes([$7F, $00]); pHi := E.Len - 1;                 // jg  slow  (start > Len -> 0)
      E.EmitBytes([$0F, $B6, $44, $08, $FF]);                    // movzx eax, byte [rax+rcx-1]
      // rel32, not a short jump: what it skips is the spill, the call and the reload, which is well
      // past 127 bytes once the pool is full.
      E.Emit8($E9); pDone := E.Len; E.Emit32(0);                 // jmp done
      E.PatchByte(pLen, Byte(E.Len - (pLen + 1)));
      E.PatchByte(pNil, Byte(E.Len - (pNil + 1)));
      E.PatchByte(pLo,  Byte(E.Len - (pLo + 1)));
      E.PatchByte(pHi,  Byte(E.Len - (pHi + 1)));
    end;
    SpillVol; LeafSaveBases;
    ILoadTo(ABI_ARG1, stReg);
    ILoadTo(ABI_ARG3, lnReg);                                    // r9 on Win64: last of the pooled reads
    StrBaseRax;
    MovLoadR(ABI_ARG0, RAX, LongWord(sSlot) * 8);
    LeafCall(Prim(AOTCTX_STRASCMID));
    LeafRestore;
    if pDone >= 0 then E.Patch32(pDone, LongWord(E.Len - (pDone + 4)));   // @done
    IStore(dReg, RAX);
  end;
  // ⛔ The two fused ACCUMULATOR forms - bcStrConcatCharAt and bcStrAppendMapped - are NOT lowered
  // here, and the reason is not effort: NOTHING IN THE PIPELINE PRODUCES THEM TODAY. The SSA fusion
  // that made ssaStrConcatCharAt is switched off because it miscompiled (five values, four operand
  // slots), and the bytecode fusion that made bcStrAppendMapped is closed by measurement - see the
  // long note in SedaiSuperinstructions.pas, where it came out FOUR TIMES slower under the AOT.
  // Writing an emitter for them would be twenty machine instructions apiece that no program can
  // reach, and unexercised codegen is worse than none: the helper route already carries them,
  // correctly, on the day the fusion comes back. (Their INT operands stay declared in ScanI, which
  // costs nothing and is the side to be wrong on.)
  // rdst := [rsp+CtxDisp] - the executing context object, which the record primitives take directly
  // (unlike the string ones, which want the bank base inside it).
  procedure CtxObjTo(dst: Integer);
  var rex: Byte;
  begin
    rex := $48; if dst >= 8 then rex := rex or $04;
    E.Emit8(rex); E.Emit8($8B);
    E.Emit8($80 or ((dst and 7) shl 3) or RSP); E.Emit8($24);
    E.Emit32(LongWord(CtxDisp));
  end;
  // C6 in the JIT: Dest(int) := AotRecordNew(VMSelf, CtxObj, intSlots|floatSlots<<32, Immediate).
  // The three slot counts are not registers - the bytecode compiler puts them in Src1/Src2/Immediate -
  // so they are baked and the call reads no bank at all.
  procedure EmitRecordNewJ(dReg: Integer; src1, src2: Word; imm: Int64);
  begin
    SpillVol; LeafSaveBases;
    MovImm64(ABI_ARG0, PtrInt(VMSelf));
    CtxObjTo(ABI_ARG1);
    MovImm64(ABI_ARG2, Int64(LongWord(src1)) or (Int64(LongWord(src2)) shl 32));
    MovImm64(ABI_ARG3, imm);
    LeafCall(Prim(AOTCTX_RECNEW));
    LeafRestore;
    IStore(dReg, RAX);
  end;
  procedure EmitRecordFreeJ(hReg: Integer);
  begin
    SpillVol; LeafSaveBases;
    ILoadTo(ABI_ARG1, hReg);                                     // the handle, read while the pool is intact
    MovImm64(ABI_ARG0, PtrInt(VMSelf));
    LeafCall(Prim(AOTCTX_RECFREE));
    LeafRestore;
  end;
  procedure EmitRecMarkJ(IsPush: Boolean);
  begin
    SpillVol; LeafSaveBases;
    CtxObjTo(ABI_ARG0);
    if IsPush then LeafCall(Prim(AOTCTX_RECMARKPUSH)) else LeafCall(Prim(AOTCTX_RECMARKPOP));
    LeafRestore;
  end;

  // Return the call-site index for a bcCallSub at absolute PC apc (populated by BuildCallSites), or -1
  // if that call was found non-inlinable (then the op stays unsupported and the loop bails).
  function FindCallSite(apc: Integer): Integer;
  var k: Integer;
  begin
    Result := -1;
    for k := 0 to NCall - 1 do
      if CallPC[k] = apc then begin Result := k; Exit; end;
  end;

  // Return the GOSUB-site index for a bcCall at absolute PC apc (populated by BuildGosubSites), or -1.
  function FindGosubSite(apc: Integer): Integer;
  var k: Integer;
  begin
    Result := -1;
    for k := 0 to NGosub - 1 do
      if GCallPC[k] = apc then begin Result := k; Exit; end;
  end;

  // Emit native code for one bytecode instruction at absolute PC apc. Returns False (bail) on any
  // unsupported opcode or a bcCallSub that could not be inlined. A bcCallSub emits an inline copy of the
  // callee body wrapped in a native FramePush/Pop; the callee is compiled all-memory (InCallee).
  function EmitOne(apc: Integer): Boolean;
  var cs, cpc, ck, pOk, pOk2: Integer;
  begin
    Result := False;
    I := @Prog[apc];
    JitDiagCurOp := I^.OpCode; JitDiagCurPC := apc;   // last op seen -> the culprit if we bail below
    Dd := LongWord(I^.Dest) * 8;
    S1 := LongWord(I^.Src1) * 8;
    S2 := LongWord(I^.Src2) * 8;
    case I^.OpCode of
      bcLoadConstInt:
        if IAlloc(I^.Dest) >= 0 then
          MovImm64(IAlloc(I^.Dest), I^.Immediate)                  // mov gpr, imm64
        else
        begin
          MovImm64(RAX, I^.Immediate);                             // mov rax, imm64
          E.MemOp([$48, $89], RAX, RBX, Dd);                       // mov [rbx+d],rax
        end;
      bcLoadConstFloat:
        begin
          E.EmitBytes([$48, $B8]); E.Emit64(QWord(I^.Immediate));  // mov rax, rawbits
          E.EmitBytes([$66, $48, $0F, $6E, $C0]);                  // movq xmm0, rax
          FStore(I^.Dest, XMM0);                                   // -> xmm reg or [rsi+d]
        end;
      bcCopyInt:
        if (IAlloc(I^.Dest) >= 0) and (IAlloc(I^.Src1) >= 0) then
          MovRR(IAlloc(I^.Dest), IAlloc(I^.Src1))   // reg-reg copy in one move
        else
        begin
          ILoad(RAX, I^.Src1);
          IStore(I^.Dest, RAX);
        end;
      bcCopyFloat:
        begin
          FLoad(XMM0, I^.Src1);
          FStore(I^.Dest, XMM0);
        end;
      bcAddInt:
        begin
          ILoad(RAX, I^.Src1);
          IOp([$48, $03], RAX, I^.Src2);            // add rax, src2
          IStore(I^.Dest, RAX);
        end;
      bcSubInt:
        begin
          ILoad(RAX, I^.Src1);
          IOp([$48, $2B], RAX, I^.Src2);            // sub rax, src2
          IStore(I^.Dest, RAX);
        end;
      bcMulInt:
        begin
          ILoad(RAX, I^.Src1);
          IOp([$48, $0F, $AF], RAX, I^.Src2);       // imul rax, src2
          IStore(I^.Dest, RAX);
        end;
      // Integer div/mod deopt to the interpreter on the faulting cases -- not valid inside an inlined
      // callee (its native frame would be lost on the deopt), so bail there.
      bcDivInt: if InCallee or InGosub then Exit else DivMod(apc, False);
      bcModInt: if InCallee or InGosub then Exit else DivMod(apc, True);
      bcNarrowInt:
        begin
          ILoad(RAX, I^.Src1);
          case I^.Immediate of
            1: E.EmitBytes([$48, $0F, $BE, $C0]);   // s8:  movsx rax, al
            2: E.EmitBytes([$0F, $B6, $C0]);        // u8:  movzx eax, al
            3: E.EmitBytes([$48, $0F, $BF, $C0]);   // s16: movsx rax, ax
            4: E.EmitBytes([$0F, $B7, $C0]);        // u16: movzx eax, ax
            5: E.EmitBytes([$48, $63, $C0]);        // s32: movsxd rax, eax
            6: E.EmitBytes([$89, $C0]);             // u32: mov eax, eax (zero-extends)
            // else: width code 0/unknown -> value unchanged
          end;
          IStore(I^.Dest, RAX);
        end;
      // Unsigned div/mod: same deopt-on-zero shape as the signed pair, so the same restriction -
      // a deopt inside an inlined callee would lose its native frame.
      bcDivUInt: if InCallee or InGosub then Exit else DivModUnsigned(apc, False);
      bcModUInt: if InCallee or InGosub then Exit else DivModUnsigned(apc, True);
      bcNegInt:
        begin
          ILoad(RAX, I^.Src1);
          E.EmitBytes([$48, $F7, $D8]);             // neg rax
          IStore(I^.Dest, RAX);
        end;
      // The bitwise family. Nothing here can trap or depend on the dialect, which is why it took
      // three lines each and had been missing anyway - one AND in a loop bailed the whole JIT.
      bcBitwiseAnd:
        begin
          ILoad(RAX, I^.Src1);
          IOp([$48, $23], RAX, I^.Src2);            // and rax, src2
          IStore(I^.Dest, RAX);
        end;
      bcBitwiseOr:
        begin
          ILoad(RAX, I^.Src1);
          IOp([$48, $0B], RAX, I^.Src2);            // or rax, src2
          IStore(I^.Dest, RAX);
        end;
      bcBitwiseXor:
        begin
          ILoad(RAX, I^.Src1);
          IOp([$48, $33], RAX, I^.Src2);            // xor rax, src2
          IStore(I^.Dest, RAX);
        end;
      bcBitwiseNot:
        begin
          ILoad(RAX, I^.Src1);
          E.EmitBytes([$48, $F7, $D0]);             // not rax
          IStore(I^.Dest, RAX);
        end;
      // ⚠️ SHL masks (that is what FPC does, and the interpreter leaves it to FPC) but SHR
      // SATURATES - the asymmetry is the interpreter's and is mirrored, not tidied away.
      bcShl:
        begin
          ILoad(RAX, I^.Src1);
          ILoad(RCX, I^.Src2);
          E.EmitBytes([$48, $D3, $E0]);             // shl rax, cl
          IStore(I^.Dest, RAX);
        end;
      bcShr:     ShrSat({Arith=} Modern);           // MODERN arithmetic, CLASSIC logical
      bcShrUInt: ShrSat(False);
      bcBitClz, bcBitCtz, bcBitRotl, bcBitRotr: BitIntrinsic;
      // popcnt is the only one of the five that is not baseline x86-64. Without the feature the loop
      // bails, which is the JIT's contract: bit-identical or nothing.
      bcBitPopcnt: if not POPCNTSupport then Exit else BitIntrinsic;
      bcArrayLBound: if InCallee or InGosub then Exit else ArrBound(apc, I^.Src1, False, True);
      bcArrayUBound: if InCallee or InGosub then Exit
                     else ArrBound(apc, I^.Src1, True, (I^.Immediate and BC_ARRAY_RANK1_FLAG) <> 0);
      bcNegFloat:
        begin
          // Flip the sign BIT, which is what unary minus on a double is - not "0 - x", which would
          // turn -0.0 into +0.0 and change what the interpreter prints.
          FLoad(XMM0, I^.Src1);
          E.EmitBytes([$48, $B8]); E.Emit64(QWord($8000000000000000));  // mov rax, sign mask
          E.EmitBytes([$66, $48, $0F, $6E, $C8]);   // movq xmm1, rax
          E.EmitBytes([$66, $0F, $57, $C1]);        // xorpd xmm0, xmm1
          FStore(I^.Dest, XMM0);
        end;
      bcNarrowSingle:
        begin
          // A demote followed by a promote: the value stays a Double that has lost the bits a Single
          // cannot carry, and that loss is observable, so it is not a no-op.
          FLoad(XMM0, I^.Src1);
          E.EmitBytes([$F2, $0F, $5A, $C0]);        // cvtsd2ss xmm0, xmm0
          E.EmitBytes([$F3, $0F, $5A, $C0]);        // cvtss2sd xmm0, xmm0
          FStore(I^.Dest, XMM0);
        end;
      // ⛔ Immediate = 1 is an unsigned-64 destination, which cvtsd2si cannot answer above 2^63. Bail,
      // as bcFloatToInt does: the interpreter's arm is the one that knows.
      bcFloatRound:
        if I^.Immediate = 1 then Exit
        else
        begin
          // CINT: FPC's Round is round-half-to-EVEN, which is cvtsd2si under the default mode.
          FLoad(XMM0, I^.Src1);
          E.EmitBytes([$F2, $48, $0F, $2D, $C0]);   // cvtsd2si rax, xmm0
          IStore(I^.Dest, RAX);
        end;
      // INT()/FLOOR is one instruction - roundsd toward -inf - on any CPU with SSE4.1. The RTL
      // exposes no SSE4.1 test, so this uses AVX as the AOT does: conservative in the safe
      // direction, since AVX implies SSE4.1 and a machine without it just keeps bailing as before.
      bcMathInt:
        if not AVXSupport then Exit
        else
        begin
          FLoad(XMM0, I^.Src1);
          E.EmitBytes([$66, $0F, $3A, $0B, $C0, $01]);   // roundsd xmm0, xmm0, 1
          FStore(I^.Dest, XMM0);
        end;
      // FIX = truncate toward zero. One instruction and nothing else, because FixDouble now gives a
      // zero result the sign of its operand (IEEE 754 §5.9) - which is what roundsd mode 3 does.
      // ⛔ FRAC is NOT here: it reads System.Int, which returns a POSITIVE zero, so Frac(-0.0) is
      // -0.0 and reproducing that needs a third live xmm - xmm2 upwards is the allocation pool.
      bcMathFix:
        if not AVXSupport then Exit
        else
        begin
          FLoad(XMM0, I^.Src1);
          E.EmitBytes([$66, $0F, $3A, $0B, $C0, $03]);   // roundsd xmm0, xmm0, 3 (toward zero)
          FStore(I^.Dest, XMM0);
        end;
      // ABS = clear the sign BIT. Measured against the interpreter, not assumed: its Abs gives
      // +QNaN for either NaN sign and +0 for -0, which is andpd and not a compare-and-negate.
      bcMathAbs:
        begin
          FLoad(XMM0, I^.Src1);
          E.EmitBytes([$48, $B8]); E.Emit64(QWord($7FFFFFFFFFFFFFFF));
          E.EmitBytes([$66, $48, $0F, $6E, $C8]);   // movq xmm1, rax
          E.EmitBytes([$66, $0F, $54, $C1]);        // andpd xmm0, xmm1
          FStore(I^.Dest, XMM0);
        end;
      // SGN = (x > 0) - (0 > x). ⚠️ Both tests use seta so an UNORDERED operand answers false twice
      // and the result is 0, which is what the interpreter's if/else-if chain gives for a NaN.
      bcMathSgn:
        begin
          FLoad(XMM0, I^.Src1);
          E.EmitBytes([$66, $0F, $57, $C9]);        // xorpd xmm1, xmm1
          E.EmitBytes([$66, $0F, $2E, $C1]);        // ucomisd xmm0, xmm1
          E.EmitBytes([$0F, $97, $C0]);             // seta al
          E.EmitBytes([$66, $0F, $2E, $C8]);        // ucomisd xmm1, xmm0
          E.EmitBytes([$0F, $97, $C1]);             // seta cl
          E.EmitBytes([$0F, $B6, $C0]);             // movzx eax, al
          E.EmitBytes([$0F, $B6, $C9]);             // movzx ecx, cl
          E.EmitBytes([$29, $C8]);                  // sub eax, ecx
          E.EmitBytes([$F2, $0F, $2A, $C0]);        // cvtsi2sd xmm0, eax
          FStore(I^.Dest, XMM0);
        end;
      bcAddFloat: FloatBin([$F2, $0F, $58]);        // addsd
      bcSubFloat: FloatBin([$F2, $0F, $5C]);        // subsd
      bcMulFloat: FloatBin([$F2, $0F, $59]);        // mulsd
      // Float divide. MODERN follows IEEE (divsd: x/0 = +/-Inf, 0/0 = NaN), so it compiles unconditionally.
      // CLASSIC raises ?DIVISION BY ZERO only on an exact-zero divisor (the interpreter tests `= 0.0`); guard
      // the divisor and deopt to the interpreter on zero. A NaN divisor is not zero -> divide (yields NaN,
      // matching the interpreter). Inside an inlined callee a deopt is unsafe (native frame lost) -> bail.
      bcDivFloat:
        if AllowUnsafe then FloatBin([$F2, $0F, $5E])          // divsd (IEEE = MODERN)
        else if InCallee then Exit
        else
        begin
          FLoad(XMM1, I^.Src2);                                // xmm1 = divisor
          E.EmitBytes([$0F, $57, $C0]);                        // xorps xmm0, xmm0   (0.0)
          E.EmitBytes([$66, $0F, $2E, $C8]);                   // ucomisd xmm1, xmm0
          E.EmitBytes([$7A, $00]); pOk := E.Len - 1;           // jp  @ok  (NaN divisor -> divide)
          E.EmitBytes([$75, $00]); pOk2 := E.Len - 1;          // jne @ok  (divisor <> 0 -> divide)
          DeoptTo(apc);                                        // divisor == 0 -> interpreter raises
          E.PatchByte(pOk, Byte(E.Len - (pOk + 1)));
          E.PatchByte(pOk2, Byte(E.Len - (pOk2 + 1)));
          FLoad(XMM0, I^.Src1);                                // xmm0 = dividend
          E.EmitBytes([$F2, $0F, $5E, $C1]);                   // divsd xmm0, xmm1
          FStore(I^.Dest, XMM0);
        end;
      // Square root. MODERN (FreeBASIC) Sqr of a negative is NaN (no trap) -> sqrtsd unconditionally. CLASSIC
      // raises ?ILLEGAL QUANTITY on a negative operand; guard operand >= 0 and deopt otherwise (a NaN operand
      // fails the ordered compare too and deopts, where the interpreter reproduces NaN). Callee -> bail.
      bcMathSqr:
        if AllowUnsafe then
        begin
          FOp([$F2, $0F, $51], XMM0, I^.Src1);          // sqrtsd xmm0, <s1>
          FStore(I^.Dest, XMM0);
        end
        else if InCallee then Exit
        else
        begin
          FLoad(XMM0, I^.Src1);                          // xmm0 = X
          E.EmitBytes([$0F, $57, $C9]);                  // xorps xmm1, xmm1   (0.0)
          E.EmitBytes([$66, $0F, $2E, $C1]);             // ucomisd xmm0, xmm1
          E.EmitBytes([$73, $00]); pOk := E.Len - 1;     // jae @ok  (X >= 0 ordered -> sqrt)
          DeoptTo(apc);                                  // X < 0 (or NaN) -> interpreter
          E.PatchByte(pOk, Byte(E.Len - (pOk + 1)));
          E.EmitBytes([$F2, $0F, $51, $C0]);             // sqrtsd xmm0, xmm0
          FStore(I^.Dest, XMM0);
        end;
      // ⛔ ANY non-zero flag bails, and the "<> 0" is doing more work than the original "= 1": Src3 is
      // a set of BITS - bit 0 an UNSIGNED source, which cvtsi2sd gets wrong, bit 1 a result that must
      // land in binary32 with ONE rounding, which this arm does not do either. Wrong here is a SILENT
      // miscompile, not a slow loop. The interpreter's arm is the one that knows.
      bcIntToFloat:
        if I^.Immediate <> 0 then Exit
        else
        begin
          if IAlloc(I^.Src1) >= 0 then
          begin
            gpr := IAlloc(I^.Src1);
            E.Emit8($F2);
            if gpr >= 8 then E.Emit8($49) else E.Emit8($48);   // REX.W (+B)
            E.EmitBytes([$0F, $2A]);
            E.Emit8($C0 or (XMM0 shl 3) or (gpr and 7));        // cvtsi2sd xmm0, gpr
          end
          else
          begin
            E.EmitBytes([$F2, $48, $0F, $2A]);                  // cvtsi2sd xmm0, [rbx+s1]
            E.Emit8($80 or (XMM0 shl 3) or RBX); E.Emit32(S1);
          end;
          FStore(I^.Dest, XMM0);
        end;
      // Implicit float->int (assignment/index/FOR bound/arg). MODERN rounds half-to-even = cvtsd2si under
      // the default MXCSR round-to-nearest mode (the same mode FPC's Round reads). CLASSIC truncates toward
      // zero = cvttsd2si (matches FPC's Trunc). Depends only on the dialect, not on bounds-checking.
      // ⛔ Immediate = 1 is an UNSIGNED-64 DESTINATION, and neither cvtsd2si nor cvttsd2si answers
      // that above 2^63 - they give the "integer indefinite" where fbc gives the value. Wrong here is
      // a SILENT miscompile, not a slow loop. Bail: the interpreter's arm is the one that knows.
      bcFloatToInt:
        if I^.Immediate = 1 then Exit
        else
        begin
          FLoad(XMM0, I^.Src1);                        // xmm0 = V
          if Modern then E.EmitBytes([$F2, $48, $0F, $2D, $C0])   // cvtsd2si rax, xmm0   (round)
          else           E.EmitBytes([$F2, $48, $0F, $2C, $C0]);  // cvttsd2si rax, xmm0  (truncate)
          IStore(I^.Dest, RAX);
        end;
      // Arrays: MODERN + no forced check -> in-place (OOB = default). CLASSIC / --bounds-check -> compile
      // with an OOB deopt to the interpreter (which raises), except inside an inlined callee where a deopt
      // is unsafe (native frame lost) -> bail. B4: an access the range analysis PROVED in-bounds
      // (BC_BOUNDS_SAFE_FLAG in Immediate) needs no compare/guard/deopt at all - and therefore
      // compiles even in the Classic-inside-inlined-callee case that otherwise bails.
      bcArrayLoadFloat:
        if (I^.Immediate and BC_BOUNDS_SAFE_FLAG) <> 0 then ArrLoadF(apc, I^.Src1, I^.Src2, I^.Dest, False, True)
        else if AllowUnsafe then ArrLoadF(apc, I^.Src1, I^.Src2, I^.Dest, False, False)
        else if InCallee then Exit else ArrLoadF(apc, I^.Src1, I^.Src2, I^.Dest, True, False);
      bcArrayStoreFloat:
        if (I^.Immediate and BC_BOUNDS_SAFE_FLAG) <> 0 then ArrStoreF(apc, I^.Src1, I^.Src2, I^.Dest, False, True)
        else if AllowUnsafe then ArrStoreF(apc, I^.Src1, I^.Src2, I^.Dest, False, False)
        else if InCallee then Exit else ArrStoreF(apc, I^.Src1, I^.Src2, I^.Dest, True, False);
      bcArrayLoadInt:
        if (I^.Immediate and BC_BOUNDS_SAFE_FLAG) <> 0 then ArrLoadI(apc, I^.Src1, I^.Src2, I^.Dest, False, True)
        else if AllowUnsafe then ArrLoadI(apc, I^.Src1, I^.Src2, I^.Dest, False, False)
        else if InCallee then Exit else ArrLoadI(apc, I^.Src1, I^.Src2, I^.Dest, True, False);
      bcArrayStoreInt:
        if (I^.Immediate and BC_BOUNDS_SAFE_FLAG) <> 0 then ArrStoreI(apc, I^.Src1, I^.Src2, I^.Dest, False, True)
        else if AllowUnsafe then ArrStoreI(apc, I^.Src1, I^.Src2, I^.Dest, False, False)
        else if InCallee then Exit else ArrStoreI(apc, I^.Src1, I^.Src2, I^.Dest, True, False);
      // Record field access: a shared-record handle deopts (locked region), which is unsafe inside an
      // inlined callee -> bail there. Slot = Immediate; handle = Src1; value/dest = Dest (load) / Src2 (store).
      bcRecordLoadInt:    if InCallee then Exit else RecAccess(apc, I^.Src1, Integer(I^.Immediate), I^.Dest, False, False);
      bcRecordLoadFloat:  if InCallee then Exit else RecAccess(apc, I^.Src1, Integer(I^.Immediate), I^.Dest, True,  False);
      bcRecordStoreInt:   if InCallee then Exit else RecAccess(apc, I^.Src1, Integer(I^.Immediate), I^.Src2, False, True);
      bcRecordStoreFloat: if InCallee then Exit else RecAccess(apc, I^.Src1, Integer(I^.Immediate), I^.Src2, True,  True);
      bcCmpLtInt: IntCmp($9C);                      // setl
      bcCmpLeInt: IntCmp($9E);                      // setle
      bcCmpGtInt: IntCmp($9F);                      // setg
      bcCmpGeInt: IntCmp($9D);                      // setge
      bcCmpEqInt: IntCmp($94);                      // sete
      bcCmpNeInt: IntCmp($95);                      // setne
      // The UNSIGNED comparisons a UInteger/ULongInt operand selects: the same shape with the
      // below/above condition codes. Eq/Ne need no unsigned form - equality does not read the sign.
      bcCmpLtUInt: IntCmp($92);                     // setb
      bcCmpLeUInt: IntCmp($96);                     // setbe
      bcCmpGtUInt: IntCmp($97);                     // seta
      bcCmpGeUInt: IntCmp($93);                     // setae
      bcCmpLtFloat: FloatCmp(0);
      bcCmpLeFloat: FloatCmp(1);
      bcCmpGtFloat: FloatCmp(2);
      bcCmpGeFloat: FloatCmp(3);
      bcCmpEqFloat: FloatCmp(4);
      bcCmpNeFloat: FloatCmp(5);
      // Transfer registers (args / result): the executing context's Xfer banks, read through the
      // ctx slot at run time (per-context - a worker uses its own banks, not the main's).
      bcXferStoreInt:
        begin
          ILoad(RAX, I^.Src1);
          LoadCtxFieldRdx(XferIntOff);
          E.MemOp([$48, $89], RAX, RDX, LongWord(I^.Immediate) * 8);       // mov [rdx+slot*8], rax
        end;
      bcXferStoreFloat:
        begin
          FLoad(XMM0, I^.Src1);
          LoadCtxFieldRdx(XferFloatOff);
          E.MemOp([$F2, $0F, $11], XMM0, RDX, LongWord(I^.Immediate) * 8);  // movsd [rdx+slot*8], xmm0
        end;
      bcXferLoadInt:
        begin
          LoadCtxFieldRdx(XferIntOff);
          E.MemOp([$48, $8B], RAX, RDX, LongWord(I^.Immediate) * 8);        // mov rax, [rdx+slot*8]
          IStore(I^.Dest, RAX);
        end;
      bcXferLoadFloat:
        begin
          LoadCtxFieldRdx(XferFloatOff);
          E.MemOp([$F2, $0F, $10], XMM0, RDX, LongWord(I^.Immediate) * 8);  // movsd xmm0, [rdx+slot*8]
          FStore(I^.Dest, XMM0);
        end;
      // Block-scoped record marks. They were a no-op here on a premise the helper route has just
      // REVOKED: "a loop we compile allocates no records, so RecordCount is invariant across the mark
      // and reclaiming to it is exact". A routed bcRecordNew allocates inside the loop, and then a
      // no-op push followed by a real pop would reclaim to a mark taken OUTSIDE the loop - so the
      // pair follows the allocation: routed when this loop allocates, free when it does not. ⛔ Both
      // read the SAME flag: a routed pop against a skipped push is the corruption, not either alone.
      bcRecMarkPush, bcRecMarkPop:
        if RoutesRecords then
        begin
          if InCallee or InGosub then Exit;
          if UseLeaf then EmitRecMarkJ(I^.OpCode = bcRecMarkPush) else EmitHelperCall(apc);
        end;
      // Inlined SUB call (J6): FramePush (native bank save) + inline callee body (all-memory) + FramePop.
      bcCallSub:
        begin
          cs := FindCallSite(apc);
          if cs < 0 then Exit;                       // not inlinable -> bail
          // Save only the caller's MEMORY-HOMED regs (J6e): the callee can corrupt those memory slots; the
          // caller's allocated regs live in native registers the callee never touches.
          EmitSaveSparse(RBX, SaveIntRegs, NSaveInt, 0);
          EmitSaveSparse(RSI, SaveFloatRegs, NSaveFloat, LongWord(NSaveInt) * 8);
          EmitSaveCallerGpr;                                    // free r9..r15 for the callee's array cache
          AllocCalleeArr(CallEntry[cs], CallRet[cs]);          // build CArr2 (same result as the pre-pass)
          for ck := 0 to NCArr2 - 1 do                         // load the callee cache from the descriptor
          begin
            if CArr2Base[ck]  >= 0 then R8LoadR(CArr2Base[ck],  LongWord(CArr2Id[ck]) * 32 + LongWord(CArr2Off[ck]));
            if CArr2Count[ck] >= 0 then R8LoadR(CArr2Count[ck], LongWord(CArr2Id[ck]) * 32 + 16);
          end;
          InCallee := True;
          for cpc := CallEntry[cs] to CallRet[cs] do
          begin
            NativeOff[cpc] := E.Len;
            if not EmitOne(cpc) then begin InCallee := False; Exit; end;
          end;
          InCallee := False;
        end;
      bcReturnSub:
        if InCallee then
        begin
          EmitRestoreCallerGpr;                                    // restore the caller's GPRs
          EmitRestoreSparse(RBX, SaveIntRegs, NSaveInt, 0);       // restore memory-homed caller int regs
          EmitRestoreSparse(RSI, SaveFloatRegs, NSaveFloat, LongWord(NSaveInt) * 8);  // ...and float
        end
        else
          Exit;                                      // a bare RETURN at loop top level is not compilable
      // Inlined GOSUB (classic): the body shares the caller frame, so no FramePush -- just spill the caller's
      // allocated regs to their home slots, emit the body all-memory, then reload them (it may have written
      // shared variables through memory). A deopt inside is only reachable on a terminal CLASSIC trap.
      bcCall:
        begin
          cs := FindGosubSite(apc);
          if cs < 0 then Exit;                       // not inlinable -> bail
          for ck := 0 to High(ILoc) do
            if ILoc[ck] >= 0 then StoreRegMem(ILoc[ck], LongWord(ck) * 8);
          for ck := 0 to High(FLoc) do
            if FLoc[ck] >= 0 then E.MemOp([$F2, $0F, $11], FLoc[ck], RSI, LongWord(ck) * 8);
          InGosub := True;
          for cpc := GEntry[cs] to GRet[cs] do
          begin
            NativeOff[cpc] := E.Len;
            if not EmitOne(cpc) then begin InGosub := False; Exit; end;
          end;
          InGosub := False;
          for ck := 0 to High(ILoc) do
            if ILoc[ck] >= 0 then LoadRegMem(ILoc[ck], LongWord(ck) * 8);
          for ck := 0 to High(FLoc) do
            if FLoc[ck] >= 0 then E.MemOp([$F2, $0F, $10], FLoc[ck], RSI, LongWord(ck) * 8);
        end;
      bcReturn:
        if not InGosub then Exit;                    // top-level RETURN not compilable; in-body = terminator
      bcJump:
        begin
          target := Integer(I^.Immediate);
          if InRange[target] then JmpRel(target)
          else
          begin
            // A deopt out of an inlined body is unsafe for BOTH kinds - its native frame would be
            // lost, and the interpreter would resume mid-callee with no frame pushed. BuildCallSites
            // already refuses such a site; this is the backstop that makes the rule local.
            if InGosub or InCallee then Exit;
            E.EmitBytes([$B8]); E.Emit32(LongWord(target));   // mov eax, target (exit PC)
            JmpRel(-1);                                        // jmp epilogue
          end;
        end;
      bcJumpIfZero, bcJumpIfNotZero:
        begin
          target := Integer(I^.Immediate);
          ILoad(RAX, I^.Src1);                      // rax := condition (reg or [rbx+s1])
          E.EmitBytes([$48, $85, $C0]);             // test rax,rax
          if InRange[target] then
          begin
            if I^.OpCode = bcJumpIfZero then JccRel($84, target)   // jz
            else JccRel($85, target);                              // jnz
          end
          else
          begin
            if InGosub or InCallee then Exit;        // a conditional deopt out of an inlined body is unsafe
            // Conditional EXIT: skip over the exit sequence when the branch is NOT taken.
            if I^.OpCode = bcJumpIfZero then E.EmitBytes([$75, $00])   // jnz short +len(exit)
            else E.EmitBytes([$74, $00]);                             // jz  short +len(exit)
            d := E.Len;                               // start of the exit sequence
            E.EmitBytes([$B8]); E.Emit32(LongWord(target));   // mov eax, target
            JmpRel(-1);                                        // jmp epilogue
            E.PatchByte(d - 1, Byte(E.Len - d));               // patch the skip displacement
          end;
        end;
      // END / STOP: a native loop cannot end the program - but it can LEAVE, and the interpreter then
      // executes the very instruction we stopped at, which is what a deopt already means. Cheaper than
      // routing it (no call at all), and it is what stops a single `IF ... THEN END` from costing the
      // whole loop. Not valid inside an inlined body, same as every other deopt.
      bcEnd, bcStop:
        if InCallee or InGosub then Exit else DeoptTo(apc);
      // ---- J15: the STRING family as native leaf calls (see the block above EmitStrOneSlot) ----
      // Each is guarded on UseLeaf and on not being inside an inlined body, exactly like the helper
      // route: a leaf call spills the caller's volatile registers to their BANK slots, and inside an
      // inlined callee those slots are not where the caller's values live.
      bcCopyString:
        if UseLeaf and not (InCallee or InGosub) then EmitStrOneSlot(AOTCTX_STRASSIGN, I^.Dest, I^.Src1)
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcLoadConstString:
        if UseLeaf and not (InCallee or InGosub) then EmitStrLoadConstJ(I^.Dest, I^.Immediate)
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcIntToString:
        if UseLeaf and not (InCallee or InGosub) then EmitIntToStringJ(I^.Dest, I^.Src1)
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcStrConcat:
        if UseLeaf and not (InCallee or InGosub) then EmitStrConcatJ(I^.Dest, I^.Src1, I^.Src2)
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcStrLen:
        if UseLeaf and not (InCallee or InGosub) then EmitStrToInt(AOTCTX_STRLEN, I^.Dest, I^.Src1)
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcStrAsc:
        if UseLeaf and not (InCallee or InGosub) then EmitStrToInt(AOTCTX_STRASC, I^.Dest, I^.Src1)
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcStrLeft:
        if UseLeaf and not (InCallee or InGosub) then EmitStrSliceJ(AOTCTX_STRLEFT, I^.Dest, I^.Src1, I^.Src2)
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcStrRight:
        if UseLeaf and not (InCallee or InGosub) then EmitStrSliceJ(AOTCTX_STRRIGHT, I^.Dest, I^.Src1, I^.Src2)
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcStrMid:
        if UseLeaf and not (InCallee or InGosub) then
          EmitStrMidJ(I^.Dest, I^.Src1, I^.Src2, Integer(I^.Immediate and $FFFF))
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcStrInstr:
        if UseLeaf and not (InCallee or InGosub) then
          EmitStrInstrJ(I^.Dest, I^.Src1, I^.Src2, Integer(I^.Immediate and $FFFF))
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcStrChr:
        if UseLeaf and not (InCallee or InGosub) then EmitStrChrJ(I^.Dest, I^.Src1)
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcStrVal:
        if UseLeaf and not (InCallee or InGosub) then EmitStrValJ(I^.Dest, I^.Src1)
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcStrValInt:
        if UseLeaf and not (InCallee or InGosub) then EmitStrValIntJ(I^.Dest, I^.Src1, I^.Immediate)
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcStrAscMid:
        if UseLeaf and not (InCallee or InGosub) then
          EmitStrAscMidJ(I^.Dest, I^.Src1, I^.Src2, Integer(I^.Immediate and $FFFF))
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcRecordNew:
        if UseLeaf and not (InCallee or InGosub) then
          EmitRecordNewJ(I^.Dest, I^.Src1, I^.Src2, I^.Immediate)
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcRecordFree:
        if UseLeaf and not (InCallee or InGosub) then EmitRecordFreeJ(I^.Src1)
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
      bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString:
        if UseLeaf and not (InCallee or InGosub) then
        begin
          case I^.OpCode of
            bcCmpEqString: EmitStrCmpJ(0, I^.Src1, I^.Src2);
            bcCmpNeString: EmitStrCmpJ(1, I^.Src1, I^.Src2);
            bcCmpLtString: EmitStrCmpJ(2, I^.Src1, I^.Src2);
          else               EmitStrCmpJ(3, I^.Src1, I^.Src2);
          end;
        end
        else if UseHelper and not (InCallee or InGosub) then EmitHelperCall(apc) else Exit;
    else
      // The helper route (J14): an instruction with no native form is run by the INTERPRETER and
      // native execution carries on after it, instead of the whole loop being given up. Only for
      // what IsRoutableOp allows, and never inside an inlined callee or GOSUB body - their caller
      // registers are parked on the stack rather than in the banks, so EmitHelperCall's flush would
      // write the wrong values back (the same reason DeoptTo is refused there).
      if UseHelper and (not InCallee) and (not InGosub) and IsRoutableOp(I^.OpCode) then
        EmitHelperCall(apc)
      else
        Exit;      // still unsupported -> bail: the whole loop stays interpreted
    end;
    Result := True;
  end;

  // Pre-scan the caller range for inlinable bcCallSub sites: locate each callee's single ReturnSub,
  // reject nested calls / string / record ops, compute the frame save size, and mark the callee PCs as
  // in-range so their internal jumps resolve as internal (not loop exits).
  procedure BuildCallSites;
  var q, r, ep, rp, md: Integer; J: PBcInstr; ok: Boolean;
  begin
    NCall := 0;
    ScratchBytes := 0;
    for q := HeaderPC to EndPC do
    begin
      if Prog[q].OpCode <> bcCallSub then Continue;
      ep := Integer(Prog[q].Immediate);
      if (ep < 0) or (ep >= ProgLen) then Continue;    // out of range -> not inlinable
      rp := -1; ok := True; md := 0; r := ep;
      while r < ProgLen do
      begin
        J := @Prog[r];
        if J^.OpCode = bcReturnSub then begin rp := r; Break; end;
        case J^.OpCode of
          bcCallSub, bcCallSubIndirect,                 // no nested calls in V1
          bcXferStoreString, bcXferLoadString,          // no string transfer
          bcRecordNew, bcRecordNewArray: begin ok := False; Break; end;   // no record allocation
        end;
        if J^.Dest > md then md := J^.Dest;
        Inc(r);
      end;
      // ⛔ Every jump in the body must STAY in the body. `rp` is the FIRST bcReturnSub, which for a
      // callee with an early `Return` is the early one - so the real body sits AFTER rp, out of
      // range, and the conditional jump that reaches it would be emitted as a loop EXIT from inside
      // an inlined callee. That is precisely what DeoptTo refuses to do (the native frame is lost):
      // the program resumed mid-callee with no frame pushed and died on an access violation.
      // Rejecting the site turns a silent miscompile into a bail, which is this compiler's contract.
      // 🐛 It was invisible until J14: every witness had a PRINT in the loop, so the loop bailed
      // before reaching the call. Covering an opcode is how a latent defect gets found.
      if ok and (rp >= 0) then
        for r := ep to rp do
        begin
          J := @Prog[r];
          if (J^.OpCode = bcJump) or (J^.OpCode = bcJumpIfZero) or (J^.OpCode = bcJumpIfNotZero) then
            if (Integer(J^.Immediate) < ep) or (Integer(J^.Immediate) > rp) then
            begin
              ok := False;
              Break;
            end;
        end;
      if (rp < 0) or (not ok) then Continue;
      Inc(NCall);
      SetLength(CallPC, NCall); SetLength(CallEntry, NCall);
      SetLength(CallRet, NCall); SetLength(CallSaveN, NCall);
      CallPC[NCall - 1] := q; CallEntry[NCall - 1] := ep; CallRet[NCall - 1] := rp;
      CallSaveN[NCall - 1] := md + 1;                  // save [0,maxDest+1) of each bank
      for r := ep to rp do InRange[r] := True;
      if (md + 1) * 16 > ScratchBytes then ScratchBytes := (md + 1) * 16;
    end;
  end;

  // Pre-scan for inlinable classic GOSUB (bcCall) sites: locate the single terminating bcReturn, reject
  // nested calls / string / record ops, and mark the body PCs in-range. A target reached from more than one
  // call site in the loop is skipped (its body would be emitted twice, breaking forward-jump fixups). Any
  // remaining unsafe op (integer div/mod, LBOUND/UBOUND, a jump out of the body) makes the emit bail.
  procedure BuildGosubSites;
  var q, r, ep, rp, cnt, k2: Integer; J: PBcInstr; ok: Boolean;
  begin
    NGosub := 0;
    for q := HeaderPC to EndPC do
    begin
      if Prog[q].OpCode <> bcCall then Continue;
      ep := Integer(Prog[q].Immediate);
      if (ep < 0) or (ep >= ProgLen) then Continue;    // out of range -> not inlinable
      if (ep >= HeaderPC) and (ep <= EndPC) then Continue;   // target inside the loop -> skip (overlap)
      cnt := 0;                                         // reject targets reached from >1 site (double-emit)
      for k2 := HeaderPC to EndPC do
        if (Prog[k2].OpCode = bcCall) and (Integer(Prog[k2].Immediate) = ep) then Inc(cnt);
      if cnt <> 1 then Continue;
      rp := -1; ok := True; r := ep;
      while r < ProgLen do
      begin
        J := @Prog[r];
        if J^.OpCode = bcReturn then begin rp := r; Break; end;
        case J^.OpCode of
          bcCall, bcCallSub, bcCallSubIndirect, bcReturnSub,   // no nested calls / SUB returns
          bcXferStoreString, bcXferLoadString,                 // no string transfer
          bcRecordNew, bcRecordNewArray: begin ok := False; Break; end;   // no record allocation
        end;
        Inc(r);
      end;
      if (rp < 0) or (not ok) then Continue;
      Inc(NGosub);
      SetLength(GCallPC, NGosub); SetLength(GEntry, NGosub); SetLength(GRet, NGosub);
      GCallPC[NGosub - 1] := q; GEntry[NGosub - 1] := ep; GRet[NGosub - 1] := rp;
      for r := ep to rp do InRange[r] := True;
    end;
  end;

begin
  Result := nil;
  Prog := PBcInstr(Ins);
  if (HeaderPC < 0) or (EndPC < HeaderPC) then Exit;

  // Quick reject: no CALL / RETURN / anything that leaves the loop frame may appear (handled by the
  // op whitelist below), but also refuse loops longer than a sane cap.
  if EndPC - HeaderPC > 4096 then Exit;

  if ProgLen <= EndPC then Exit;      // NativeOff/InRange are indexed by absolute PC
  E := TX86Emitter.Create;
  SetLength(NativeOff, ProgLen);
  SetLength(InRange, ProgLen);
  for d := 0 to ProgLen - 1 do InRange[d] := False;
  for d := HeaderPC to EndPC do InRange[d] := True;
  InCallee := False;
  InGosub := False;
  NSaveInt := 0; NSaveFloat := 0;     // J6e: sparse-save lists, filled by the allocation overflow branches
  BuildCallSites;                     // find inlinable bcCallSub, mark callee ranges, size the stack scratch
  BuildGosubSites;                    // find inlinable classic GOSUB (bcCall), mark body ranges in-range
  NFix := 0;

  // --- float register allocation (J4): assign xmm2..xmm7 to the used VM float regs, spill the rest ---
  FMaxReg := -1;
  ScanF(False);                             // compute FMaxReg
  SetLength(FLoc, FMaxReg + 2);
  for fi := 0 to High(FLoc) do FLoc[fi] := -1;
  if FMaxReg >= 0 then ScanF(True);         // mark used regs as -2
  NextXmm := 2; SaveX6 := False; SaveX7 := False;
  for fi := 0 to FMaxReg do
    if FLoc[fi] = -2 then
    begin
      if NextXmm <= 7 then
      begin
        FLoc[fi] := NextXmm;
        if NextXmm = 6 then SaveX6 := True;
        if NextXmm = 7 then SaveX7 := True;
        Inc(NextXmm);
      end
      else
      begin
        FLoc[fi] := -1;                     // overflow -> memory-homed (used but no xmm)
        Inc(NSaveFloat); SetLength(SaveFloatRegs, NSaveFloat); SaveFloatRegs[NSaveFloat - 1] := fi;
      end;
    end;

  // --- integer GPR allocation (J5): r9/r10/r11 (volatile) then r12..r15 (callee-saved) ---
  IntPool[0] := R9;  IntPool[1] := R10; IntPool[2] := R11;
  IntPool[3] := R12; IntPool[4] := R13; IntPool[5] := R14; IntPool[6] := R15;
  for gpr := 0 to 15 do SaveGpr[gpr] := False;
  IMaxReg := -1;
  ScanI(False);                             // compute IMaxReg
  SetLength(ILoc, IMaxReg + 2);
  for ii := 0 to High(ILoc) do ILoc[ii] := -1;
  if IMaxReg >= 0 then ScanI(True);         // mark used regs as -2
  NextGpr := 0;
  for ii := 0 to IMaxReg do
    if ILoc[ii] = -2 then
    begin
      if NextGpr <= 6 then
      begin
        ILoc[ii] := IntPool[NextGpr];
        if IntPool[NextGpr] >= 12 then SaveGpr[IntPool[NextGpr]] := True;  // callee-saved
        Inc(NextGpr);
      end
      else
      begin
        ILoc[ii] := -1;                     // overflow -> memory-homed (used but no GPR)
        Inc(NSaveInt); SetLength(SaveIntRegs, NSaveInt); SaveIntRegs[NSaveInt - 1] := ii;
      end;
    end;

  // --- array base/count caching (J5c LICM): hand the GPRs left free after int allocation to the
  // loop-invariant array base pointers and counts, most-used arrays first (base then count each). ---
  NCArr := 0;
  begin                                        // arrays now compile in CLASSIC too (OOB -> deopt), so cache always
    for gpr := 0 to 15 do GprUsed[gpr] := False;
    for ii := 0 to IMaxReg do
      if ILoc[ii] >= 0 then GprUsed[ILoc[ii]] := True;
    ScanArrRange(HeaderPC, EndPC);
    // selection sort the parallel arrays by use count, descending (NCArr is tiny)
    for ci := 0 to NCArr - 2 do
      for cj := ci + 1 to NCArr - 1 do
        if CArrUses[cj] > CArrUses[ci] then
        begin
          ct := CArrId[ci];    CArrId[ci]    := CArrId[cj];    CArrId[cj]    := ct;
          ct := CArrOff[ci];   CArrOff[ci]   := CArrOff[cj];   CArrOff[cj]   := ct;
          ct := CArrUses[ci];  CArrUses[ci]  := CArrUses[cj];  CArrUses[cj]  := ct;
        end;
    // assign base then count for each array in priority order, from the free r9..r15
    for ci := 0 to NCArr - 1 do
    begin
      for ct := 0 to 6 do                    // base
        if (CArrBase[ci] < 0) and (not GprUsed[IntPool[ct]]) then
        begin
          CArrBase[ci] := IntPool[ct]; GprUsed[IntPool[ct]] := True;
          if IntPool[ct] >= 12 then SaveGpr[IntPool[ct]] := True;
        end;
      for ct := 0 to 6 do                    // count
        if (CArrCount[ci] < 0) and (not GprUsed[IntPool[ct]]) then
        begin
          CArrCount[ci] := IntPool[ct]; GprUsed[IntPool[ct]] := True;
          if IntPool[ct] >= 12 then SaveGpr[IntPool[ct]] := True;
        end;
    end;
  end;

  // --- inlined-callee dedicated array cache setup (J6d Stage 2) ---
  // CallerGpr = the caller's live GPRs (its int allocation + its shared array cache) that must be preserved
  // around every inlined callee so the callee may reuse the whole r9..r15 pool. Run AllocCalleeArr per call
  // site now (before the prologue) so a callee-saved GPR its cache claims is pushed; recomputed at emit.
  NCallerGpr := 0;
  if NCall > 0 then
    for gpr := 9 to 15 do
    begin
      ct := 0;
      for ii := 0 to IMaxReg do if ILoc[ii] = gpr then ct := 1;
      for ci := 0 to NCArr - 1 do if (CArrBase[ci] = gpr) or (CArrCount[ci] = gpr) then ct := 1;
      if ct = 1 then
      begin
        Inc(NCallerGpr); SetLength(CallerGpr, NCallerGpr); CallerGpr[NCallerGpr - 1] := gpr;
      end;
    end;
  for ci := 0 to NCall - 1 do
    AllocCalleeArr(CallEntry[ci], CallRet[ci]);   // sets SaveGpr for callee-saved cache regs
  // Sparse frame-save lists (J6e) were built during allocation (the overflow branches): SaveIntRegs /
  // SaveFloatRegs hold exactly the USED caller regs that got no native register. For n-body's fully-allocated
  // main loop both are empty -> no per-call bank copy at all.
  // --- helper route (J14): does this loop need it at all? ---
  // Decided BEFORE the frame is laid out, because the route costs two more scratch slots and a
  // different rsp adjustment, and a loop that routes nothing keeps the frame it had before this
  // existed. Deliberately CONSERVATIVE: an opcode that IsRoutableOp accepts may still be lowered
  // natively by the case below (bcMathSqr is in the math group), so a loop can reserve the slots and
  // never use them. Sixteen bytes of stack is not worth a second emission pass to reclaim.
  UseHelper := False;
  RoutesRecords := False;
  // ⚠️ Gated on the SAME switch as the helper route, and that is the point: JIT_HELPER=0 has to mean
  // "this JIT does not call out at all", or the A/B stops separating anything. It measured +2% once
  // when it only disabled the route - both sides were taking the leaf calls and the knob was inert.
  UseLeaf := (PrimCtx <> nil) and (StringRegsOff > 0) and HelperRouteEnabled;
  if (HelperFn <> nil) and (VMSelf <> nil) and HelperRouteEnabled then
    for ii := HeaderPC to EndPC do
    begin
      if IsRoutableOp(Prog[ii].OpCode) then UseHelper := True;
      if (Prog[ii].OpCode = bcRecordNew) or (Prog[ii].OpCode = bcRecordFree) then RoutesRecords := True;
    end;
  // Scratch layout: [0, (NSaveInt+NSaveFloat)*8) sparse bank save, then [GprSaveDisp, +NCallerGpr*8)
  // GPR save, then the 8-byte ctx slot at CtxDisp (always present: the Xfer/record accessors read the
  // Ctx object pointer from it; rsp is stable through the body so the offset is fixed), and finally -
  // only when this loop routes - the two slots for the base registers a call destroys.
  GprSaveDisp := (NSaveInt + NSaveFloat) * 8;
  CtxDisp := GprSaveDisp + NCallerGpr * 8;
  ScratchBytes := CtxDisp + 8;
  ArrDescDisp := -1; FltSaveDisp := -1; HelperAdjust := 0;
  if UseHelper then
  begin
    ArrDescDisp := ScratchBytes;
    FltSaveDisp := ScratchBytes + 8;
    ScratchBytes := ScratchBytes + 16;
    // rsp at a call site, computed statically: the return address, the two unconditional pushes, the
    // callee-saved GPRs the allocator claimed, the optional xmm6/7 area, and the scratch. Win64 also
    // wants 32 bytes of shadow space; System V wants only the alignment.
    ci := 8 + 16;
    for gpr := 12 to 15 do if SaveGpr[gpr] then Inc(ci, 8);
    if SaveX6 or SaveX7 then Inc(ci, 16);
    Inc(ci, ScratchBytes);
    {$IFDEF WINDOWS}
    HelperAdjust := 32 + ((16 - (ci mod 16)) mod 16);
    {$ELSE}
    HelperAdjust := (16 - (ci mod 16)) mod 16;
    {$ENDIF}
  end;

  try
    // --- prologue ---  (Win64: rcx=IntRegs, rdx=FloatRegs; SysV: rdi/rsi)
    E.EmitBytes([$53]);                       // push rbx
    E.EmitBytes([$56]);                       // push rsi
    // Save the callee-saved GPRs (r12..r15) that got allocated (both ABIs: r12..r15 are non-volatile).
    if SaveGpr[R12] then E.EmitBytes([$41, $54]);   // push r12
    if SaveGpr[R13] then E.EmitBytes([$41, $55]);   // push r13
    if SaveGpr[R14] then E.EmitBytes([$41, $56]);   // push r14
    if SaveGpr[R15] then E.EmitBytes([$41, $57]);   // push r15
    {$IFDEF WINDOWS}
    E.EmitBytes([$48, $89, $CB]);             // mov rbx, rcx    (arg0 = IntRegs)
    E.EmitBytes([$48, $89, $D6]);             // mov rsi, rdx    (arg1 = FloatRegs)
    // arg2 (ArrDesc) is already in r8 on Win64.
    E.EmitBytes([$4C, $89, $CA]);             // mov rdx, r9     (arg3 = Ctx - grabbed into rdx NOW:
                                              // r9 is in the r9..r15 allocation pool and the entry
                                              // loads below would clobber it. rdx stays untouched
                                              // until the ctx slot store after the scratch reserve.)
    {$ELSE}
    E.EmitBytes([$48, $89, $FB]);             // mov rbx, rdi    (arg0 = IntRegs)
    E.EmitBytes([$48, $89, $F6]);             // mov rsi, rsi    (arg1 = FloatRegs, already in rsi)
    E.EmitBytes([$49, $89, $D0]);             // mov r8, rdx     (arg2 = ArrDesc)
    E.EmitBytes([$48, $89, $CA]);             // mov rdx, rcx    (arg3 = Ctx, same reasoning as Win64)
    {$ENDIF}

    // Save the callee-saved xmm6/xmm7 (Win64) if they were allocated, then load the allocated VM float
    // regs from memory into their native xmm.
    if SaveX6 or SaveX7 then
    begin
      E.EmitBytes([$48, $83, $EC, $10]);                          // sub rsp, 16
      if SaveX6 then E.EmitBytes([$F2, $0F, $11, $74, $24, $00]); // movsd [rsp],   xmm6
      if SaveX7 then E.EmitBytes([$F2, $0F, $11, $7C, $24, $08]); // movsd [rsp+8], xmm7
    end;
    for fi := 0 to FMaxReg do
      if FLoc[fi] >= 0 then
        E.MemOp([$F2, $0F, $10], FLoc[fi], RSI, LongWord(fi) * 8);  // movsd xmm_alloc, [rsi+fi*8]
    // Load the allocated VM int regs from memory into their native GPR.
    for ii := 0 to IMaxReg do
      if ILoc[ii] >= 0 then
        LoadRegMem(ILoc[ii], LongWord(ii) * 8);                     // mov gpr, [rbx+ii*8]
    // Load the cached array base pointers / counts from the descriptor (r8) -- loop-invariant.
    for ci := 0 to NCArr - 1 do
    begin
      if CArrBase[ci]  >= 0 then R8LoadR(CArrBase[ci],  LongWord(CArrId[ci]) * 32 + LongWord(CArrOff[ci]));
      if CArrCount[ci] >= 0 then R8LoadR(CArrCount[ci], LongWord(CArrId[ci]) * 32 + 16);
    end;

    // Reserve stack scratch for inlined SUB frame save/restore + the ctx slot (sits below the xmm6/7
    // save area; rsp is stable through the body so the scratch is at a fixed [rsp+0..ScratchBytes)
    // offset). ScratchBytes is always > 0 now (the ctx slot), so the reserve is unconditional.
    if ScratchBytes > 0 then
    begin
      E.EmitBytes([$48, $81, $EC]); E.Emit32(LongWord(ScratchBytes));   // sub rsp, ScratchBytes
    end;
    // Park the Ctx object pointer (still in rdx from the argument moves) in its slot.
    E.EmitBytes([$48, $89, $94, $24]); E.Emit32(LongWord(CtxDisp));     // mov [rsp+CtxDisp], rdx

    // --- body --- (each instruction is emitted by EmitOne; a bcCallSub emits its callee inline)
    for pc := HeaderPC to EndPC do
    begin
      NativeOff[pc] := E.Len;
      if not EmitOne(pc) then Exit;
    end;

    // Fall-through past the last body instruction is also a loop exit to EndPC+1.
    E.EmitBytes([$B8]); E.Emit32(LongWord(EndPC + 1));   // mov eax, EndPC+1
    JmpRel(-1);

    // --- epilogue --- (rax already holds the exit PC; the writebacks below do not touch rax)
    EpilogueOff := E.Len;
    // Write the allocated VM int regs back to memory so the interpreter sees their final values.
    for ii := 0 to IMaxReg do
      if ILoc[ii] >= 0 then
        StoreRegMem(ILoc[ii], LongWord(ii) * 8);                   // mov [rbx+ii*8], gpr
    // Write the allocated float regs back to memory so the interpreter sees their final values.
    for fi := 0 to FMaxReg do
      if FLoc[fi] >= 0 then
        E.MemOp([$F2, $0F, $11], FLoc[fi], RSI, LongWord(fi) * 8);  // movsd [rsi+fi*8], xmm_alloc
    // Release the inlined-call scratch (brings rsp back to the xmm6/7 save area).
    if ScratchBytes > 0 then
    begin
      E.EmitBytes([$48, $81, $C4]); E.Emit32(LongWord(ScratchBytes));   // add rsp, ScratchBytes
    end;
    // Restore callee-saved xmm and the stack.
    if SaveX6 or SaveX7 then
    begin
      if SaveX6 then E.EmitBytes([$F2, $0F, $10, $74, $24, $00]); // movsd xmm6, [rsp]
      if SaveX7 then E.EmitBytes([$F2, $0F, $10, $7C, $24, $08]); // movsd xmm7, [rsp+8]
      E.EmitBytes([$48, $83, $C4, $10]);                          // add rsp, 16
    end;
    // Restore the callee-saved GPRs (reverse of the prologue push order).
    if SaveGpr[R15] then E.EmitBytes([$41, $5F]);   // pop r15
    if SaveGpr[R14] then E.EmitBytes([$41, $5E]);   // pop r14
    if SaveGpr[R13] then E.EmitBytes([$41, $5D]);   // pop r13
    if SaveGpr[R12] then E.EmitBytes([$41, $5C]);   // pop r12
    E.EmitBytes([$5E]);          // pop rsi
    E.EmitBytes([$5B]);          // pop rbx
    E.EmitBytes([$C3]);          // ret

    // --- patch fixups ---
    for pc := 0 to NFix - 1 do
    begin
      if Fixups[pc].TargetPC = -1 then
        target := EpilogueOff
      else
        target := NativeOff[Fixups[pc].TargetPC];   // absolute PC (covers inlined-callee targets too)
      E.Patch32(Fixups[pc].PatchOff, LongWord(target - (Fixups[pc].PatchOff + 4)));
    end;

    Result := TExecMem.Create(E);
    if Result.Ptr = nil then FreeAndNil(Result);
  finally
    E.Free;
  end;
end;

{ ---------------- self-test ---------------- }

type
  TAddFn = function(A, B: Int64): Int64;

function JitSelfTest(out Msg: string): Boolean;
var E: TX86Emitter; Mem: TExecMem; Fn: TAddFn; r1, r2, r3: Int64;
begin
  Result := False; Msg := '';
  E := TX86Emitter.Create;
  try
    {$IFDEF WINDOWS}
    E.EmitBytes([$48, $89, $C8, $48, $01, $D0, $C3]);   // mov rax,rcx; add rax,rdx; ret
    {$ELSE}
    E.EmitBytes([$48, $89, $F8, $48, $01, $F0, $C3]);   // mov rax,rdi; add rax,rsi; ret
    {$ENDIF}
    Mem := TExecMem.Create(E);
    try
      if Mem.Ptr = nil then begin Msg := 'exec alloc failed'; Exit; end;
      Fn := TAddFn(Mem.Ptr);
      r1 := Fn(3, 4); r2 := Fn(-10, 100); r3 := Fn(1000000000000, 2000000000000);
      if (r1 = 7) and (r2 = 90) and (r3 = 3000000000000) then
      begin Msg := Format('native add() OK: 3+4=%d, -10+100=%d, 1e12+2e12=%d', [r1, r2, r3]); Result := True; end
      else Msg := Format('native add() WRONG: %d,%d,%d', [r1, r2, r3]);
    finally Mem.Free; end;
  finally E.Free; end;
end;

end.
