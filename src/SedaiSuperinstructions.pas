{*
 * SedaiBasic - A BASIC interpreter with bytecode VM
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * This program is dual-licensed:
 *
 * 1) For open source use: GNU General Public License version 3 (GPL-3.0-only)
 *    You may redistribute and/or modify it under the terms of the GNU GPL v3
 *    as published by the Free Software Foundation.
 *    See <https://www.gnu.org/licenses/gpl-3.0.html>
 *
 * 2) For commercial/proprietary use: A separate commercial license is required.
 *    Contact: maurizio.cammalleri@gmail.com for licensing inquiries.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}
unit SedaiSuperinstructions;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$I OptimizationFlags.inc}
{$I DebugFlags.inc}

{ Superinstruction optimization pass

  Fuses common instruction sequences into single superinstructions.
  This reduces dispatch overhead and improves cache utilization.

  Patterns recognized (from real bytecode analysis of SIEVE.BAS):

  1. Compare + JumpIfZero → Fused branch (VERY COMMON - ~15 occurrences)
     Pattern: CmpLeInt R24, R0, R1 + JumpIfZero R24, 17
     Becomes: BranchGtInt R0, R1, 17  (negated because JumpIfZero)
     Saves: 1 instruction + 1 register

  2. Arith + Copy → Fused arithmetic-to-dest (VERY COMMON - ~8 occurrences)
     Pattern: AddInt R19, R0, R22 + CopyInt R0, R19
     Becomes: AddIntTo R0, R22  (dest = dest + src)
     Saves: 1 instruction + 1 register

  3. LoadConst + Compare + JumpIfZero → Fused compare-zero-branch
     Pattern: LoadConstInt R31, 0 + CmpEqInt R29, R30, R31 + JumpIfZero R29, 30
     Becomes: BranchNeZeroInt R30, 30
     Saves: 2 instructions + 2 registers

  4. LoadConst + Arith → Fused constant arithmetic
     Pattern: LoadConstInt R17, 1 + AddInt R3, R3, R17
     Becomes: IncInt R3  (or AddIntConst R3, R3, 1)
     Saves: 1 instruction + 1 register

  Must run AFTER bytecode compilation, operates on TBytecodeProgram.
}

interface

uses
  Classes, SysUtils, SedaiBytecodeTypes, SedaiNopCompaction, SedaiOpcodeBanks, SedaiSSATypes;

{ Superinstruction opcodes are now defined in SedaiBytecodeTypes.pas }
{ All bcXxx constants are imported via the uses clause }

type
  TSuperinstructionOptimizer = class
  private
    FProgram: TBytecodeProgram;
    FFusedCount: Integer;
    FKindMask: string;
    FAotOwns: array of Boolean;
    function KindOn(Kind: Integer): Boolean;
    function KindOptIn(Kind: Integer): Boolean;
    // ⛔ True where the AOT WILL compile: fusion is refused there and allowed everywhere else.
    // Empty means "nobody told us", and with a compiler running that has to mean OFF - the same
    // conservative answer this pass gave before the mask existed.
    function MayFuseAt(Index: Integer): Boolean;

    { Try to fuse instruction at index i with following instructions }
    function TryFuseCompareAndBranch(Index: Integer): Boolean;
    function TryFuseArithAndCopy(Index: Integer): Boolean;
    function TryFuseConstantArithmetic(Index: Integer): Boolean;
    function TryFuseCompareZeroAndBranch(Index: Integer): Boolean;
    function TryFuseArrayStoreConst(Index: Integer): Boolean;
    function TryFuseLoopIncrementAndBranch(Index: Integer): Boolean;

    { NEW: FMA and advanced patterns }
    function TryFuseMulAddFloat(Index: Integer): Boolean;
    function TryFuseArrayLoadAddFloat(Index: Integer): Boolean;
    function TryFuseSquareSumFloat(Index: Integer): Boolean;
    function TryFuseMulMulFloat(Index: Integer): Boolean;
    function TryFuseAddSqrtFloat(Index: Integer): Boolean;
    function TryFuseArrayLoadBranchInt(Index: Integer): Boolean;

    { NEW: Array swap and self-increment patterns for fannkuch-redux }
    function TryFuseArraySwapInt(Index: Integer): Boolean;
    function TryFuseAddIntSelf(Index: Integer): Boolean;
    function TryFuseArrayLoadIntTo(Index: Integer): Boolean;

    { NEW: High-impact array operations for fannkuch-redux }
    function TryFuseArrayCopyElement(Index: Integer): Boolean;
    function TryFuseArrayMoveElement(Index: Integer): Boolean;
    function TryFuseArrayReverseRange(Index: Integer): Boolean;
    function TryFuseArrayShiftLeft(Index: Integer): Boolean;

    { "acc += tab[Asc(MID$(s, i, 1)) + 1]" - three instructions into one. Lives HERE and not in the
      SSA passes for a structural reason, see the function's own comment. }
    function TryFuseAppendMapped(Index: Integer): Boolean;
    function IntRegIsConstOne(Reg: Word): Boolean;
    function IntRegReadElsewhere(Reg: Word; A, B, C: Integer): Boolean;

    { Check if register is only used by the next instruction (temporary) }
    function RegReadElsewhere(Reg: Word; Bank: TRegBank; Lo, Hi: Integer): Boolean;
    function IsTemporaryResult(Index: Integer; Reg: Word; FusedDest: Integer = -1; PatternEnd: Integer = -1): Boolean;

    { Check if an instruction index is a jump target }
    function IsJumpTarget(Index: Integer): Boolean;

    { Make a NOP instruction }
    procedure MakeNop(Index: Integer);

  public
    constructor Create(AProgram: TBytecodeProgram);
    // The AOT's PC mask (see AotEligiblePCMask). Set it before Run when a compiler will run.
    procedure SetAotOwnedPCs(const Mask: array of Boolean);
    function Run: Integer;
  end;

function RunSuperinstructions(AProgram: TBytecodeProgram): Integer;

{ FuseAtLoad - the fusion pass, run by whoever is about to EXECUTE, on a program that arrived as
  bytecode.

  ⛔⛔⛔ WHY THIS EXISTS: A COMPILER MUST NOT BAKE A DECISION ONLY THE RUNNER CAN MAKE.
  Whether fusing pays depends on which ENGINE runs the bytecode - the interpreter wants it, the loop
  JIT is destroyed by it (it has arms for 3 of the 72 superinstructions, so a fused hot loop bails
  entirely). That is why RunSuperinstructions carries the GJitWillRun gate. But `sbc` does not know
  which engine will run its output, never sets that flag, and therefore always fused - and a `.basc`
  cannot be un-fused afterwards.

  📊 Measured 3 Sep 2026, fannkuch-redux-modern N=11, one binary:

        from the source, --jit          2 019 ms      JIT_DIAG: 7 loops NATIVE, 0 BAIL
        from `sbc` output, --jit       21 641 ms      JIT_DIAG: 0 loops NATIVE, 5 BAIL
        from the source, interpreted   21 796 ms
        `SUPERINSTR=0 sbc`, --jit       2 056 ms      <- the same file, unfused: cured

  ⇒ `sb prog.basc --jit` was TEN AND A HALF TIMES slower than `sb prog.bas --jit`, on the same
  program, because the compiler had already decided. The JIT compiled NOTHING at all.

  ⇒ The cure is the ORDER, not a patch: `sbc` no longer fuses, and every runner fuses here, at load,
  where the engine is known. An old `.basc` produced before this change is still fused and still
  hostile to --jit; nothing can undo that, which is exactly why the decision has moved.

  ⚠️ GAotWillRun is forced False for the call, and that is a FACT and not a convenience: the AOT
  compiles from the SSA program, a `.basc` carries no SSA, and `sb` says so where it refuses --aot
  for one. Leaving the flag set would have `--aot prog.basc` suppress fusion for a compiler that
  cannot run - an interpreted run with the optimisation switched off for nobody. }
function FuseAtLoad(AProgram: TBytecodeProgram): Integer;

implementation

{$IFDEF DEBUG_SUPERINSTR}
uses SedaiDebug;
{$ENDIF}

{ TSuperinstructionOptimizer }

constructor TSuperinstructionOptimizer.Create(AProgram: TBytecodeProgram);
begin
  inherited Create;
  FProgram := AProgram;
  FFusedCount := 0;
end;

procedure TSuperinstructionOptimizer.MakeNop(Index: Integer);
var
  NopInstr: TBytecodeInstruction;
begin
  // IMPORTANT: Initialize entire record to zero to avoid garbage in uninitialized fields
  FillChar(NopInstr, SizeOf(NopInstr), 0);
  NopInstr.OpCode := bcNop;
  // All other fields are already 0 from FillChar
  FProgram.SetInstruction(Index, NopInstr);
end;

function TSuperinstructionOptimizer.IsJumpTarget(Index: Integer): Boolean;
// Is any instruction's Immediate this index?
//
// ⛔ This used to answer with two explicit cases - six core opcodes and nineteen Group-super ones -
// and between them they left out FOURTEEN opcodes whose Immediate is a target: the STRING and the
// UNSIGNED compare-and-branch families (added later and never added here), the two
// array-load-and-branch forms, and bcOnError / bcResumeLabel. This test exists to REFUSE a fusion
// whose second half is a jump target; a target it cannot see is a jump that lands after the fused
// pair. One shared list now, in SedaiOpcodeBanks, which is also the one the NOP compactor uses to
// remap those very targets.
var
  i: Integer;
  Instr: TBytecodeInstruction;
begin
  Result := False;
  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    Instr := FProgram.GetInstruction(i);
    if OpCarriesJumpTarget(Instr.OpCode) and (Instr.Immediate = Index) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TSuperinstructionOptimizer.IntRegIsConstOne(Reg: Word): Boolean;
// Does INT register Reg hold the literal 1 wherever it is read? True only when every write to it in
// the WHOLE program is "LoadConstInt Reg, 1" - no control-flow reasoning needed, because then no
// path can give it another value.
//
// ⚠️ "Every write" is judged conservatively: a Dest numbered Reg counts as a write unless the opcode
// is known to write the STRING bank. Only the string classification is published (SedaiOpcodeBanks),
// so a float Dest with that number is treated as an int write and answers False. Missed fusion, never
// a wrong one.
var
  i, Writes: Integer;
  Instr: TBytecodeInstruction;
  Op: TBytecodeOp;
begin
  Result := False;
  Writes := 0;
  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    Instr := FProgram.GetInstruction(i);
    Op := TBytecodeOp(Instr.OpCode);
    if (Instr.Dest = Reg) and (not DestIsStringReg(Op)) then
    begin
      if (Op <> bcLoadConstInt) or (Instr.Immediate <> 1) then Exit;
      Inc(Writes);
    end;
  end;
  Result := Writes > 0;
end;

function TSuperinstructionOptimizer.IntRegReadElsewhere(Reg: Word; A, B, C: Integer): Boolean;
// Can INT register Reg be read by any instruction other than the three at A, B and C?
//
// This is what licenses REMOVING the two producers: if nothing else reads the temporary, nothing can
// observe the value we stop computing, and that holds on every path - no liveness, no basic blocks,
// no assumption about where control flow goes. The whole-program scan is O(n) per candidate and the
// candidates are rare.
//
// ⛔ Deliberately NOT built on IsTemporaryResult: that helper compares register NUMBERS with no idea
// of the bank, so a string R12 being defined makes it declare the int R12 dead - a false positive,
// which is the direction that miscompiles. It also stops at the end of the basic block, which says
// nothing about a successor that reads the value.
var
  i: Integer;
  Instr: TBytecodeInstruction;
  Op: TBytecodeOp;
begin
  Result := True;
  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    if (i = A) or (i = B) or (i = C) then Continue;
    Instr := FProgram.GetInstruction(i);
    Op := TBytecodeOp(Instr.OpCode);
    // Any field holding this number counts as a read unless it is PROVABLY a string operand.
    if (Instr.Src1 = Reg) and (not Src1IsStringReg(Op)) then Exit;
    if (Instr.Src2 = Reg) and (not Src2IsStringReg(Op)) then Exit;
    // A Dest can be read back as well as written (the accumulator opcodes do exactly that), and
    // there is no published int equivalent of DestReadIsStringReg - so any non-string Dest with this
    // number refuses. Over-strict: a later instruction that merely REDEFINES the register is
    // harmless in principle and still blocks the fusion here.
    if (Instr.Dest = Reg) and (not DestIsStringReg(Op)) then Exit;
    if ImmediateReadsIntReg(Instr, Reg) then Exit;
  end;
  Result := False;
end;

function TSuperinstructionOptimizer.TryFuseAppendMapped(Index: Integer): Boolean;
// "acc += tab[Asc(MID$(s, i, 1)) + 1]" - the whole inner loop of reverse-complement - as ONE
// instruction:
//
//   StrAscMid        T, s, i, len=1        ' T = Asc(Mid(s, i, 1))
//   AddInt           T, T, one             ' T = T + 1, the 1-based table index
//   StrConcatCharAt  acc, acc, tab, [T]    ' acc += tab[T]
//   =>
//   StrAppendMapped  acc, s, tab, [i]
//
// ⛔⛔ MEASURED NEGATIVE (2026-08-01) - this is UNREACHABLE, behind DISABLE_SUPERINSTRUCTIONS, and it
// is kept only so the result is not re-derived from scratch. It is CORRECT; it is simply worthless.
//
// The idea was sound as far as it went. In SSA the fused form has to name FIVE values - accumulator
// out, accumulator in, source string, table, index - and a TSSAInstruction has four operand slots, so
// with the source in Src1 the incoming accumulator is named by nothing, PHI elimination has nothing
// to rewrite, and the reset of the accumulator lands on a different register from the one the append
// grows. That is the miscompile that keeps the SSA version switched off. Down here, after register
// allocation, the registers are PHYSICAL: incoming and outgoing accumulator ARE the same register and
// Dest names both. Four fields, five values, no conflict - and it does produce correct output where
// the SSA version produced wrong output.
//
// What it does NOT produce is speed, and the numbers say why (module-level probe,
// job/tests/bench/apmap_ceiling.bas, 20 M characters per timed block, null floor 922 vs 922):
//   interpreter   4800 -> 4847 ms   flat. Three dispatches into one is worth NOTHING on this VM,
//                                   exactly as the 2026-07-18 strategy note says.
//   --aot          922 -> 3708 ms   FOUR TIMES SLOWER.
//
// The -39.5% that motivated this work was never dispatch: it was the native helper AotStrAppendMapped
// replacing three AOT helper calls. And the AOT compiles from SSA, not from bytecode - so a fusion
// performed here is invisible to it and merely leaves the region running interpreted.
//
// 🎯 The payoff therefore lives in the AOT, and the place to fuse is SedaiAot's emission, which walks
// SSA instructions and emits code directly: it can recognise the triple without ever building an SSA
// instruction, so no operand slot, no PHI elimination and no allocator are involved. That is the route
// to try next; this one is closed.
var
  A, B, C, Fused: TBytecodeInstruction;
  T, LenReg, OneReg: Word;
  Diag: Boolean;
begin
  Result := False;
  if Index + 2 >= FProgram.GetInstructionCount then Exit;
  A := FProgram.GetInstruction(Index);
  B := FProgram.GetInstruction(Index + 1);
  C := FProgram.GetInstruction(Index + 2);
  if (TBytecodeOp(A.OpCode) <> bcStrAscMid) or
     (TBytecodeOp(B.OpCode) <> bcAddInt) or
     (TBytecodeOp(C.OpCode) <> bcStrConcatCharAt) then Exit;

  // APPENDMAP_DIAG=1 says which guard refused a candidate. The pattern is rare and the guards are
  // deliberately conservative, so "why did it not fire" is the question this pass gets asked.
  Diag := GetEnvironmentVariable('APPENDMAP_DIAG') = '1';

  // Jumping into the middle of the sequence would reach the fused instruction with the state the
  // removed producers were supposed to have built.
  if IsJumpTarget(Index + 1) or IsJumpTarget(Index + 2) then
  begin
    if Diag then WriteLn(ErrOutput, '[APPENDMAP] ', Index, ': jump target inside the sequence');
    Exit;
  end;

  // The temporary must be produced by the Asc, incremented in place, and consumed by the append.
  T := A.Dest;
  if (B.Dest <> T) or (B.Src1 <> T) then
  begin
    if Diag then WriteLn(ErrOutput, '[APPENDMAP] ', Index, ': AddInt is not "T = T + k"');
    Exit;
  end;
  if (C.Immediate and $FFFF) <> T then
  begin
    if Diag then WriteLn(ErrOutput, '[APPENDMAP] ', Index, ': the append does not index with T');
    Exit;
  end;

  // Only the in-place accumulator shape: the fused opcode APPENDS to Dest, so a different
  // destination would silently drop whatever the accumulator already held.
  if C.Dest <> C.Src1 then
  begin
    if Diag then WriteLn(ErrOutput, '[APPENDMAP] ', Index, ': append is not in place (Dest <> Src1)');
    Exit;
  end;

  // Both the substring length and the addend must be the literal 1 - that is the only case where
  // "Asc of a one-character substring, plus one" is what the fused opcode computes.
  LenReg := A.Immediate and $FFFF;
  OneReg := B.Src2;
  if not IntRegIsConstOne(LenReg) then
  begin
    if Diag then WriteLn(ErrOutput, '[APPENDMAP] ', Index, ': length register R', LenReg, ' is not the constant 1');
    Exit;
  end;
  if not IntRegIsConstOne(OneReg) then
  begin
    if Diag then WriteLn(ErrOutput, '[APPENDMAP] ', Index, ': addend register R', OneReg, ' is not the constant 1');
    Exit;
  end;

  // Nothing outside the three may read the temporary, or removing its producers changes what that
  // reader sees.
  if IntRegReadElsewhere(T, Index, Index + 1, Index + 2) then
  begin
    if Diag then WriteLn(ErrOutput, '[APPENDMAP] ', Index, ': temporary R', T, ' may be read elsewhere');
    Exit;
  end;

  // Rewrite the append in place and drop the two producers. Src2 (the table) is already right.
  Fused := C;
  Fused.OpCode := bcStrAppendMapped;
  Fused.Src1 := A.Src1;                  // the source string the byte is read from
  Fused.Immediate := A.Src2;             // ...and the int register holding the position in it
  FProgram.SetInstruction(Index + 2, Fused);
  MakeNop(Index);
  MakeNop(Index + 1);
  Inc(FFusedCount);
  Result := True;
end;

function FieldMayName(FieldBank, Wanted: TRegBank): Boolean;
// Can a field classified as FieldBank be naming a register of bank Wanted?
//
// Yes when they agree, and yes when the field's bank is UNKNOWN - a scan looking for reads must
// assume the worst there.
//
// ⛔ IT IS TEMPTING TO READ rbUnknown AS "NOT A REGISTER", AND IT IS WRONG. The argument for it is
// good and still fails: a Src field claimed by none of the three bank lists is one REGISTER
// COMPACTION does not remap either, so trusting the lists here would be exactly as safe as
// compaction already is. Tried, and aot_validate found the hole in one program - Src2IsFloatReg
// did not list bcMathMin/bcMathMax/bcMathCopySign, whose Src2 is a float operand, so `Min(a, b)`
// stopped counting as a read of b and the fusion deleted the instruction that produced it
// (ieee_intrinsics, interpreter against AOT). The lists are complete enough for compaction because
// compaction only has to RENUMBER consistently; they are not complete enough to prove a NEGATIVE.
//
// The cost of staying conservative is real and measured: n-body's hottest loop has
// `ArrayLoadFloat R17, ARR[17], R29`, whose Src1 is an ARRAY ID and not a register at all, and that
// makes the integer R17 of the loop's compare-and-branch look read - so the head keeps its
// CmpInt+JumpIfZero and, with no BranchGtInt in the head, the TAIL cannot fuse either. Two
// instructions per iteration. Closing it needs a POSITIVE "this field is not a register index"
// answer from the funnel, not the absence of a claim.
begin
  Result := (FieldBank = Wanted) or (FieldBank = rbUnknown);
end;

function TSuperinstructionOptimizer.RegReadElsewhere(Reg: Word; Bank: TRegBank; Lo, Hi: Integer): Boolean;
// Does ANY instruction outside [Lo..Hi] read register Reg of bank Bank?
//
// This is the question a forward scan cannot answer once it meets a branch, and it is the question
// that actually licenses deleting a definition: if nothing anywhere reads the value, nothing can
// observe that we stopped computing it - on every path, across every back edge, with no liveness
// analysis and no basic blocks. A WRITE elsewhere is irrelevant; only a read is.
//
// It is the generalisation of IntRegReadElsewhere just above, which was written for one fusion and
// could only speak about int registers, because the bank predicates for the other two banks were
// still locked inside the register compactor. They are in SedaiOpcodeBanks now.
var
  i: Integer;
  Instr: TBytecodeInstruction;
  Op: TBytecodeOp;
begin
  Result := True;
  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    if (i >= Lo) and (i <= Hi) then Continue;
    Instr := FProgram.GetInstruction(i);
    Op := TBytecodeOp(Instr.OpCode);
    if (Instr.Src1 = Reg) and (not SedaiOpcodeBanks.Src1IsArrayId(Op)) and
       FieldMayName(SedaiOpcodeBanks.BankOfSrc1(Op), Bank) then Exit;
    if (Instr.Src2 = Reg) and FieldMayName(SedaiOpcodeBanks.BankOfSrc2(Op), Bank) then Exit;
    // A Dest that is READ BACK is a read like any other (ArrayStore's value, BigInt's target).
    if Instr.Dest = Reg then
      case Bank of
        rbInt:    if SedaiOpcodeBanks.DestReadIsIntReg(Op) then Exit;
        rbFloat:  if SedaiOpcodeBanks.DestReadIsFloatReg(Op) then Exit;
        rbString: if SedaiOpcodeBanks.DestReadIsStringReg(Op) then Exit;
      end;
    // ...and a superinstruction Dest is read-modify-write often enough (bcAddIntTo, bcAddIntSelf,
    // bcMulAddToFloat: "Dest = Dest op ...") that any of them holding this number refuses - but only
    // in the SAME BANK. Refusing bank-blind here undid the whole point of the scan: in n-body the
    // integer R17 of a loop's compare-and-branch was refused because a `MulSubFloat R17 = ...`
    // exists elsewhere, so the loop head kept its CmpInt+JumpIfZero, and with no BranchGtInt in the
    // head the loop TAIL could not fuse either - one missed fusion dragging a second one down, two
    // instructions per iteration in a hot loop.
    if (Instr.Dest = Reg) and (Instr.OpCode >= bcGroupSuper) and
       FieldMayName(SedaiOpcodeBanks.BankOfDest(TBytecodeOp(Instr.OpCode)), Bank) then Exit;
    if SedaiOpcodeBanks.ImmediateReadsIntReg(Instr, Reg) and (Bank = rbInt) then Exit;
    if SedaiOpcodeBanks.ImmediateReadsFloatReg(Instr, Reg) and (Bank = rbFloat) then Exit;
  end;
  Result := False;
end;

function TSuperinstructionOptimizer.IsTemporaryResult(Index: Integer; Reg: Word; FusedDest: Integer = -1; PatternEnd: Integer = -1): Boolean;
var
  i: Integer;
  Instr, DefInstr: TBytecodeInstruction;
  IsControlFlow: Boolean;
  DestBank: TRegBank;
begin
  // Check if the register defined at Index is ONLY used by Index+1
  // For superinstruction fusion, we just need to know that:
  // 1. The register is produced at Index
  // 2. The register is consumed at Index+1
  // 3. The register is not used elsewhere in the same basic block before redefinition
  //
  // We scan forward from Index+2 until we hit:
  // - Control flow (end of basic block) -> register is temporary (safe to fuse)
  // - Register reuse as source -> NOT temporary (can't fuse)
  // - Register redefinition -> temporary (safe to fuse)

  Result := True;  // Assume temporary until proven otherwise

  // ⭐ THE QUESTION IS MOOT WHEN THE FUSED INSTRUCTION WRITES THE SAME REGISTER. "MulFloat R12,R13,R12
  // + AddFloat R12,R1,R12" becomes "MulAddFloat R12 = R13*R12 + R1": R12 ends the pair holding the
  // same value either way, at the same point, so who reads it AFTERWARDS cannot tell the difference.
  // Only the INTERMEDIATE value disappears, and nothing can observe that - entry at Index+1 is
  // already excluded by the caller's IsJumpTarget test.
  // ⛔ The caller must pass this ONLY when the fused instruction really does write Reg. A fusion that
  // REMOVES the write (compare-and-branch consumes its result and stores nothing) must not: there the
  // register genuinely stops being written and every later read matters.
  if (FusedDest >= 0) and (FusedDest = Reg) then Exit;

  // The bank of Reg is the bank the DEFINING instruction writes it in. Every caller passes the Dest
  // of the instruction at Index; one that does not cannot be answered, and gets a no.
  DefInstr := FProgram.GetInstruction(Index);
  if DefInstr.Dest <> Reg then begin Result := False; Exit; end;
  DestBank := SedaiOpcodeBanks.BankOfDest(TBytecodeOp(DefInstr.OpCode));
  if DestBank = rbUnknown then begin Result := False; Exit; end;

  // ⭐ PatternEnd IS THE LAST INSTRUCTION THE FUSION REPLACES, and the whole function assumed it was
  // Index+1 - a PAIR - in two separate places. A three-instruction fusion has its first result read
  // by its own last instruction, so both the forward scan and the RegReadElsewhere exclusion below
  // counted a read that is part of the pattern being removed, and answered "not temporary" every
  // time. That is precisely what kept bcSquareSumFloat from ever being produced: it is a 3-for-1.
  // ⛔ Fixing only the scan is not enough - the exclusion range at the control-flow test is the same
  // assumption written a second time, and it is the one that actually fires.
  if PatternEnd < 0 then PatternEnd := Index + 1;
  for i := PatternEnd + 1 to FProgram.GetInstructionCount - 1 do
  begin
    Instr := FProgram.GetInstruction(i);

    // Check if this register is used as source BEFORE checking control flow
    // This catches the case where the jump itself uses the register
    //
    // ⛔⛔⛔ AND THE THIRD PLACE A REGISTER CAN BE READ: THE IMMEDIATE.
    // Src1 and Src2 are not the whole story - some opcodes carry an int register INDEX in Immediate
    // (bcStrAscMid's length, bcStrMid's length, bcStrConcatCharAt's index, and forty others).
    // Without this test the scan declared such a register dead and the fusion deleted the LoadConst
    // that produced it: m198_byrefstr printed "Hello" with its first byte corrupted, because R3 held
    // the LENGTH read by the very next StrAscMid.
    //
    // ⭐ ImmediateReadsIntReg already existed for exactly this - its own comment says "the only
    // consumer is a fusion pass asking 'is this int register read anywhere else?'" - and this pass,
    // the one it was written for, was not calling it. It deliberately over-reports, which costs a
    // missed fusion and never a miscompile.
    // A field holding this NUMBER is a read only if it names the same BANK - and rbUnknown counts
    // as a read, because "no list claims this field" is not evidence of absence. Getting this half
    // right costs real fusions: n-body's inner loop has `CmpInt R14` killed by a FLOAT R14, and a
    // bank-blind scan then sees the float R14 read two instructions later and declines both the
    // compare-and-branch and the loop-increment-and-branch fusions - two instructions per iteration
    // out of twenty-two, measured at +6% on the interpreter.
    // The Immediate is asked of both int and float - the fused multiply-add family carries its
    // accumulator there, where Src1/Src2 cannot see it - but each answer is kept to ITS OWN bank.
    // Asking both without that guard is the same bank-blindness one level down, and it cost the
    // same kind of fusion: `MulSubFloat R16 = R14 * R16 - R17` carries the FLOAT R17 in Immediate,
    // which made the INTEGER R17 of a loop's compare-and-branch look read.
    if ((Instr.Src1 = Reg) and (not SedaiOpcodeBanks.Src1IsArrayId(TBytecodeOp(Instr.OpCode))) and
        FieldMayName(SedaiOpcodeBanks.BankOfSrc1(TBytecodeOp(Instr.OpCode)), DestBank)) or
       ((Instr.Src2 = Reg) and FieldMayName(SedaiOpcodeBanks.BankOfSrc2(TBytecodeOp(Instr.OpCode)), DestBank)) or
       (SedaiOpcodeBanks.ImmediateReadsIntReg(Instr, Reg) and (DestBank = rbInt)) or
       (SedaiOpcodeBanks.ImmediateReadsFloatReg(Instr, Reg) and (DestBank = rbFloat)) then
    begin
      Result := False;
      Exit;
    end;

    // ⛔⛔⛔ AND THE FOURTH WAY TO GET THIS WRONG: A REGISTER NUMBER IS NOT A REGISTER.
    // "Dest = Reg, so Reg is redefined, so it was dead" compares NUMBERS across three separate
    // banks. In test_division_bug the constant lived in FLOAT R1 and the next block wrote STRING
    // R1 ("VX = "): the scan called that a redefinition, the fusion deleted the LoadConstFloat, and
    // the DivFloat eleven instructions later divided by a register nobody had loaded - a spurious
    // Division by zero under the interpreter, with every other engine right. IntRegReadElsewhere,
    // fifteen lines up this file, already carried the diagnosis in its comment ("that helper
    // compares register NUMBERS with no idea of the bank... a false positive, which is the
    // direction that miscompiles") - it was written down and never carried back here.
    //
    // A kill therefore needs THREE things, and anything less keeps scanning:
    //   1. the same bank (DestBank below, from the defining instruction);
    //   2. a pure WRITE - an opcode that reads Dest back (ArrayStore, BigInt) is a USE, not a kill;
    //   3. not a superinstruction, because that is where the read-modify-write forms live
    //      (bcAddIntTo, bcAddIntSelf, bcMulAddToFloat: "Dest = Dest op ..."). Declining to kill on
    //      one costs a fusion; killing on one deletes a live definition.
    if Instr.Dest = Reg then
    begin
      // A read of Reg through Dest is a use, and ends the question.
      if ((DestBank = rbInt) and SedaiOpcodeBanks.DestReadIsIntReg(TBytecodeOp(Instr.OpCode))) or
         ((DestBank = rbFloat) and SedaiOpcodeBanks.DestReadIsFloatReg(TBytecodeOp(Instr.OpCode))) or
         ((DestBank = rbString) and SedaiOpcodeBanks.DestReadIsStringReg(TBytecodeOp(Instr.OpCode))) then
      begin
        Result := False;
        Exit;
      end;
      if (Instr.OpCode < bcGroupSuper) and (SedaiOpcodeBanks.BankOfDest(TBytecodeOp(Instr.OpCode)) = DestBank) then
        Exit;  // Result remains True: same bank, pure write, really is a redefinition
      // Otherwise it is some OTHER register that happens to share this number. Keep scanning.
    end;

    // Check for control flow (end of basic block)
    // Must handle both standard opcodes and superinstructions
    IsControlFlow := False;
    if Instr.OpCode < bcGroupString then
    begin
      // Standard control flow opcodes in Group 0
      case Instr.OpCode of
        bcJump, bcJumpIfZero, bcJumpIfNotZero, bcCall, bcReturn, bcEnd, bcStop,
        bcCallSub, bcReturnSub,
        // M5.2/M5.5: spawning/joining/detaching a worker is a side-effecting barrier (the worker touches shared state).
        bcThreadCreate, bcThreadWait, bcThreadSelf, bcThreadDetach,
        // M5.4: mutex + condition-variable ops are synchronization barriers — don't fuse across them.
        bcMutexCreate, bcMutexLock, bcMutexUnlock, bcMutexDestroy,
        bcCondCreate, bcCondWait, bcCondSignal, bcCondBroadcast, bcCondDestroy:
          IsControlFlow := True;
      end;
    end
    else if Instr.OpCode >= bcGroupSuper then
    begin
      // Superinstruction branches are also control flow
      case Instr.OpCode of
        bcBranchEqInt, bcBranchNeInt, bcBranchLtInt, bcBranchGtInt, bcBranchLeInt, bcBranchGeInt,
        bcBranchEqFloat, bcBranchNeFloat, bcBranchLtFloat, bcBranchGtFloat, bcBranchLeFloat, bcBranchGeFloat,
        bcBranchEqZeroInt, bcBranchNeZeroInt,
        bcBranchEqZeroFloat, bcBranchNeZeroFloat:
          IsControlFlow := True;
      end;
    end;

    // ⛔⛔⛔ THIS USED TO SAY "Exit; Result remains True - safe within this basic block", AND IT IS
    // THE DEFECT THAT KEPT THE WHOLE SUPERINSTRUCTION FAMILY OFF SINCE JULY.
    //
    // Reaching the end of a basic block does NOT mean the register is dead. It can be live in a
    // successor, or across a back edge, or read by a block some jump enters later - none of which a
    // forward scan that stops at the first branch can see. The guard is right in INTENT and wrong in
    // SCOPE, exactly like the copy-coalescing miscompile: it asks "is anyone using it HERE?" when the
    // question is "can anyone use it ANYWHERE".
    //
    // Turning the family on with this as it was gave 16 FAIL and 16 OPTDIFF in run_regress, and the
    // bisection (SUPERMASK) named ONE culprit out of nineteen kinds: ConstantArithmetic, the only one
    // that leans on this predicate. m6_shared printed COUNTER as 11 where it is 7.
    //
    // Answering NO at a block boundary is the conservative reading, and it costs only the fusions
    // that span one - which are exactly the ones that were never safe.
    // ⛔ A BLOCK BOUNDARY IS NOT AN ANSWER - BUT "NO" IS NOT THE ONLY HONEST ONE.
    // Reaching a branch means this scan has run out of what it can see: the register may be live in
    // a successor, across a back edge, or in a block some jump enters later. Answering NO there was
    // the 18 Aug fix, and it was right against the previous answer (YES, which miscompiled). It is
    // not the best answer available: ask the WHOLE PROGRAM whether anything else reads the register,
    // which needs no liveness at all, and decline only if something does.
    //
    // This is not a theoretical improvement. n-body's inner loop ends in a back edge, so every
    // fusion in it dies at this test - the compare-and-branch and the loop-increment-and-branch
    // both, two instructions per iteration out of twenty-two. Until the bank fix above, they
    // survived by ACCIDENT: a float register sharing the int register's number ended the scan early,
    // with the wrong reason and the right result. Removing the accident without adding this made the
    // interpreter 6% slower on n-body.
    if IsControlFlow then
    begin
      Result := not RegReadElsewhere(Reg, DestBank, Index, PatternEnd);
      Exit;
    end;
  end;

  // Reached the end of the PROGRAM with no further use and no redefinition: that one really is dead.
end;

function TSuperinstructionOptimizer.TryFuseCompareAndBranch(Index: Integer): Boolean;
var
  CmpInstr, JmpInstr, FusedInstr: TBytecodeInstruction;
  CmpOp, JmpOp: TBytecodeOp;
  FusedOpCode: Word;
  NegateCondition: Boolean;
begin
  Result := False;

  if Index + 1 >= FProgram.GetInstructionCount then Exit;

  CmpInstr := FProgram.GetInstruction(Index);
  JmpInstr := FProgram.GetInstruction(Index + 1);

  // Safety: both instructions must be standard opcodes (< 100)
  if (CmpInstr.OpCode >= bcGroupSuper) or (JmpInstr.OpCode >= bcGroupSuper) then Exit;

  CmpOp := TBytecodeOp(CmpInstr.OpCode);
  JmpOp := TBytecodeOp(JmpInstr.OpCode);

  // Check for comparison followed by conditional jump
  if not (JmpOp in [bcJumpIfZero, bcJumpIfNotZero]) then Exit;

  // Check if jump uses the comparison result
  if JmpInstr.Src1 <> CmpInstr.Dest then Exit;

  // SAFETY: Don't fuse if the second instruction is a jump target
  if IsJumpTarget(Index + 1) then Exit;

  // Check if comparison result is temporary
  if not IsTemporaryResult(Index, CmpInstr.Dest) then Exit;

  // JumpIfZero: jump when FALSE → negate the condition
  // JumpIfNotZero: jump when TRUE → keep the condition
  NegateCondition := (JmpOp = bcJumpIfZero);

  // Determine the fused opcode
  case CmpOp of
    // Int comparisons
    bcCmpEqInt: if NegateCondition then FusedOpCode := bcBranchNeInt else FusedOpCode := bcBranchEqInt;
    bcCmpNeInt: if NegateCondition then FusedOpCode := bcBranchEqInt else FusedOpCode := bcBranchNeInt;
    bcCmpLtInt: if NegateCondition then FusedOpCode := bcBranchGeInt else FusedOpCode := bcBranchLtInt;
    bcCmpGtInt: if NegateCondition then FusedOpCode := bcBranchLeInt else FusedOpCode := bcBranchGtInt;
    bcCmpLeInt: if NegateCondition then FusedOpCode := bcBranchGtInt else FusedOpCode := bcBranchLeInt;
    bcCmpGeInt: if NegateCondition then FusedOpCode := bcBranchLtInt else FusedOpCode := bcBranchGeInt;

    // Float comparisons
    bcCmpEqFloat: if NegateCondition then FusedOpCode := bcBranchNeFloat else FusedOpCode := bcBranchEqFloat;
    bcCmpNeFloat: if NegateCondition then FusedOpCode := bcBranchEqFloat else FusedOpCode := bcBranchNeFloat;
    bcCmpLtFloat: if NegateCondition then FusedOpCode := bcBranchGeFloat else FusedOpCode := bcBranchLtFloat;
    bcCmpGtFloat: if NegateCondition then FusedOpCode := bcBranchLeFloat else FusedOpCode := bcBranchGtFloat;
    bcCmpLeFloat: if NegateCondition then FusedOpCode := bcBranchGtFloat else FusedOpCode := bcBranchLeFloat;
    bcCmpGeFloat: if NegateCondition then FusedOpCode := bcBranchLtFloat else FusedOpCode := bcBranchGeFloat;

    // String comparisons. ⚠️ Only FOUR of these exist as comparisons - there is no CmpLeString or
    // CmpGeString - but the negated arm needs both, which is why the branch family has six forms.
    // This asymmetry is exactly what made "add the missing opcodes" look smaller than it is.
    bcCmpEqString: if NegateCondition then FusedOpCode := bcBranchNeString else FusedOpCode := bcBranchEqString;
    bcCmpNeString: if NegateCondition then FusedOpCode := bcBranchEqString else FusedOpCode := bcBranchNeString;
    bcCmpLtString: if NegateCondition then FusedOpCode := bcBranchGeString else FusedOpCode := bcBranchLtString;
    bcCmpGtString: if NegateCondition then FusedOpCode := bcBranchLeString else FusedOpCode := bcBranchGtString;

    // Unsigned comparisons. Equality is bit-identical to the signed one and already arrives here as
    // bcCmpEqInt, so only the four ORDERING forms need a branch of their own.
    bcCmpLtUInt: if NegateCondition then FusedOpCode := bcBranchGeUInt else FusedOpCode := bcBranchLtUInt;
    bcCmpGtUInt: if NegateCondition then FusedOpCode := bcBranchLeUInt else FusedOpCode := bcBranchGtUInt;
    bcCmpLeUInt: if NegateCondition then FusedOpCode := bcBranchGtUInt else FusedOpCode := bcBranchLeUInt;
    bcCmpGeUInt: if NegateCondition then FusedOpCode := bcBranchLtUInt else FusedOpCode := bcBranchGeUInt;
  else
    Exit;  // Not a comparison we can fuse
  end;

  // Create fused instruction - initialize to zero first
  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := FusedOpCode;
  FusedInstr.Dest := 0;
  FusedInstr.Src1 := CmpInstr.Src1;
  FusedInstr.Src2 := CmpInstr.Src2;
  FusedInstr.Immediate := JmpInstr.Immediate;  // Jump target
  // SourceLine now managed via Source Map (unchanged by superinstruction fusion)

  FProgram.SetInstruction(Index, FusedInstr);
  MakeNop(Index + 1);

  {$IFDEF DEBUG_SUPERINSTR}
  // DEBUG: Log fusion details
  if DebugSuperinstr then
    WriteLn(StdErr, Format('FUSE CompareAndBranch @%d: opcode=%d Src1=%d Src2=%d Imm=%d',
      [Index, FusedOpCode, FusedInstr.Src1, FusedInstr.Src2, FusedInstr.Immediate]));
  {$ENDIF}

  Inc(FFusedCount);
  Result := True;
end;

function TSuperinstructionOptimizer.TryFuseArithAndCopy(Index: Integer): Boolean;
var
  ArithInstr, CopyInstr, FusedInstr: TBytecodeInstruction;
  ArithOp, CopyOp: TBytecodeOp;
  FusedOpCode: Word;
begin
  Result := False;

  if Index + 1 >= FProgram.GetInstructionCount then Exit;

  ArithInstr := FProgram.GetInstruction(Index);
  CopyInstr := FProgram.GetInstruction(Index + 1);

  // Safety: both instructions must be standard opcodes (< 100)
  if (ArithInstr.OpCode >= bcGroupSuper) or (CopyInstr.OpCode >= bcGroupSuper) then Exit;

  ArithOp := TBytecodeOp(ArithInstr.OpCode);
  CopyOp := TBytecodeOp(CopyInstr.OpCode);

  // Check for arithmetic followed by copy
  // Pattern: ArithXx Rtmp, Rdest, Rsrc + CopyXx Rdest, Rtmp
  // This means: Rdest = Rdest op Rsrc (the copy moves result back to original dest)

  // Check if copy source is arithmetic destination (temporary)
  if CopyInstr.Src1 <> ArithInstr.Dest then Exit;

  // Check if copy destination matches arithmetic Src1 (the accumulator pattern)
  if CopyInstr.Dest <> ArithInstr.Src1 then Exit;

  // SAFETY: Don't fuse if the second instruction is a jump target
  if IsJumpTarget(Index + 1) then Exit;

  // Check if arithmetic result is temporary
  if not IsTemporaryResult(Index, ArithInstr.Dest) then Exit;

  // Match Int arithmetic + CopyInt
  if CopyOp = bcCopyInt then
  begin
    case ArithOp of
      bcAddInt: FusedOpCode := bcAddIntTo;
      bcSubInt: FusedOpCode := bcSubIntTo;
      bcMulInt: FusedOpCode := bcMulIntTo;
    else
      Exit;
    end;
  end
  // Match Float arithmetic + CopyFloat
  else if CopyOp = bcCopyFloat then
  begin
    case ArithOp of
      bcAddFloat: FusedOpCode := bcAddFloatTo;
      bcSubFloat: FusedOpCode := bcSubFloatTo;
      bcMulFloat: FusedOpCode := bcMulFloatTo;
      bcDivFloat: FusedOpCode := bcDivFloatTo;
    else
      Exit;
    end;
  end
  else
    Exit;

  // Create fused instruction: Dest = Dest op Src1
  // (where Dest is the copy destination, Src1 is the "other" arithmetic operand)
  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := FusedOpCode;
  FusedInstr.Dest := CopyInstr.Dest;  // The accumulator register
  FusedInstr.Src1 := ArithInstr.Src2; // The operand being added/mul/etc
  FusedInstr.Src2 := 0;
  FusedInstr.Immediate := 0;
  // SourceLine now managed via Source Map

  FProgram.SetInstruction(Index, FusedInstr);
  MakeNop(Index + 1);

  {$IFDEF DEBUG_SUPERINSTR}
  // DEBUG: Log fusion details
  if DebugSuperinstr then
    WriteLn(StdErr, Format('FUSE ArithAndCopy @%d: opcode=%d Dest=%d Src1=%d',
      [Index, FusedOpCode, FusedInstr.Dest, FusedInstr.Src1]));
  {$ENDIF}

  Inc(FFusedCount);
  Result := True;
end;

function TSuperinstructionOptimizer.TryFuseConstantArithmetic(Index: Integer): Boolean;
var
  LoadInstr, ArithInstr, FusedInstr: TBytecodeInstruction;
  LoadOp, ArithOp: TBytecodeOp;
  FusedOpCode: Word;
  ConstReg: Word;
begin
  Result := False;

  if Index + 1 >= FProgram.GetInstructionCount then Exit;

  LoadInstr := FProgram.GetInstruction(Index);
  ArithInstr := FProgram.GetInstruction(Index + 1);

  // Safety: both instructions must be standard opcodes (< 100)
  if (LoadInstr.OpCode >= bcGroupSuper) or (ArithInstr.OpCode >= bcGroupSuper) then Exit;

  LoadOp := TBytecodeOp(LoadInstr.OpCode);
  ArithOp := TBytecodeOp(ArithInstr.OpCode);

  // Check for LoadConst followed by arithmetic using that constant
  if not (LoadOp in [bcLoadConstInt, bcLoadConstFloat]) then Exit;

  ConstReg := LoadInstr.Dest;

  // Check if arithmetic uses the constant as Src2
  if ArithInstr.Src2 <> ConstReg then Exit;

  // SAFETY: Don't fuse if Src1 == Src2 (e.g., x + x from strength reduction)
  // This pattern means the "constant" register is actually a variable being added to itself
  if ArithInstr.Src1 = ConstReg then Exit;

  // SAFETY: Don't fuse if the second instruction is a jump target
  if IsJumpTarget(Index + 1) then Exit;

  // Check if constant register is temporary
  // ArithInstr.Dest is what the fused instruction writes - see IsTemporaryResult.
  if not IsTemporaryResult(Index, ConstReg, ArithInstr.Dest) then Exit;

  // Match LoadConstInt + IntArith
  if LoadOp = bcLoadConstInt then
  begin
    case ArithOp of
      bcAddInt: FusedOpCode := bcAddIntConst;
      bcSubInt: FusedOpCode := bcSubIntConst;
      bcMulInt: FusedOpCode := bcMulIntConst;
    else
      Exit;
    end;
  end
  // Match LoadConstFloat + FloatArith
  else if LoadOp = bcLoadConstFloat then
  begin
    case ArithOp of
      bcAddFloat: FusedOpCode := bcAddFloatConst;
      bcSubFloat: FusedOpCode := bcSubFloatConst;
      bcMulFloat: FusedOpCode := bcMulFloatConst;
      bcDivFloat: FusedOpCode := bcDivFloatConst;
    else
      Exit;
    end;
  end
  else
    Exit;

  // Create fused instruction: Dest = Src1 op Immediate
  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := FusedOpCode;
  FusedInstr.Dest := ArithInstr.Dest;
  FusedInstr.Src1 := ArithInstr.Src1;
  FusedInstr.Src2 := 0;
  FusedInstr.Immediate := LoadInstr.Immediate;  // The constant
  // SourceLine now managed via Source Map

  FProgram.SetInstruction(Index, FusedInstr);  // Replace Load with fused
  MakeNop(Index + 1);  // Remove Arith

  {$IFDEF DEBUG_SUPERINSTR}
  // DEBUG: Log fusion details
  if DebugSuperinstr then
    WriteLn(StdErr, Format('FUSE ConstantArith @%d: opcode=%d Dest=%d Src1=%d Imm=%d',
      [Index, FusedOpCode, FusedInstr.Dest, FusedInstr.Src1, FusedInstr.Immediate]));
  {$ENDIF}

  Inc(FFusedCount);
  Result := True;
end;

function TSuperinstructionOptimizer.TryFuseCompareZeroAndBranch(Index: Integer): Boolean;
var
  LoadInstr, CmpInstr, JmpInstr, FusedInstr: TBytecodeInstruction;
  LoadOp, CmpOp, JmpOp: TBytecodeOp;
  FusedOpCode: Word;
  ConstReg, CmpResultReg: Word;
  IsFloat: Boolean;
  NegateCondition: Boolean;
begin
  Result := False;

  if Index + 2 >= FProgram.GetInstructionCount then Exit;

  LoadInstr := FProgram.GetInstruction(Index);
  CmpInstr := FProgram.GetInstruction(Index + 1);
  JmpInstr := FProgram.GetInstruction(Index + 2);

  // Safety: all instructions must be standard opcodes (< 100)
  if (LoadInstr.OpCode >= bcGroupSuper) or (CmpInstr.OpCode >= bcGroupSuper) or (JmpInstr.OpCode >= bcGroupSuper) then Exit;

  LoadOp := TBytecodeOp(LoadInstr.OpCode);
  CmpOp := TBytecodeOp(CmpInstr.OpCode);
  JmpOp := TBytecodeOp(JmpInstr.OpCode);

  // Check for LoadConst 0 + CmpEq/CmpNe + JumpIfZero/NotZero
  if not (LoadOp in [bcLoadConstInt, bcLoadConstFloat]) then Exit;
  if LoadInstr.Immediate <> 0 then Exit;  // Must be zero constant

  ConstReg := LoadInstr.Dest;
  IsFloat := (LoadOp = bcLoadConstFloat);

  // Check comparison uses the zero constant
  if CmpInstr.Src2 <> ConstReg then Exit;

  // Only handle equality comparisons with zero
  if IsFloat then
  begin
    if not (CmpOp in [bcCmpEqFloat, bcCmpNeFloat]) then Exit;
  end
  else
  begin
    if not (CmpOp in [bcCmpEqInt, bcCmpNeInt]) then Exit;
  end;

  CmpResultReg := CmpInstr.Dest;

  // Check jump uses comparison result
  if not (JmpOp in [bcJumpIfZero, bcJumpIfNotZero]) then Exit;
  if JmpInstr.Src1 <> CmpResultReg then Exit;

  // SAFETY: Don't fuse if any of the following instructions is a jump target
  if IsJumpTarget(Index + 1) then Exit;
  if IsJumpTarget(Index + 2) then Exit;

  // Check both temporary registers
  if not IsTemporaryResult(Index, ConstReg) then Exit;
  if not IsTemporaryResult(Index + 1, CmpResultReg) then Exit;

  // Determine fused opcode
  // CmpEq + JumpIfZero → branch if NOT equal to zero (BranchNeZero)
  // CmpEq + JumpIfNotZero → branch if equal to zero (BranchEqZero)
  // CmpNe + JumpIfZero → branch if NOT (not equal) = equal to zero (BranchEqZero)
  // CmpNe + JumpIfNotZero → branch if not equal to zero (BranchNeZero)

  NegateCondition := (JmpOp = bcJumpIfZero);

  if IsFloat then
  begin
    if CmpOp = bcCmpEqFloat then
    begin
      if NegateCondition then FusedOpCode := bcBranchNeZeroFloat
      else FusedOpCode := bcBranchEqZeroFloat;
    end
    else // CmpNeFloat
    begin
      if NegateCondition then FusedOpCode := bcBranchEqZeroFloat
      else FusedOpCode := bcBranchNeZeroFloat;
    end;
  end
  else
  begin
    if CmpOp = bcCmpEqInt then
    begin
      if NegateCondition then FusedOpCode := bcBranchNeZeroInt
      else FusedOpCode := bcBranchEqZeroInt;
    end
    else // CmpNeInt
    begin
      if NegateCondition then FusedOpCode := bcBranchEqZeroInt
      else FusedOpCode := bcBranchNeZeroInt;
    end;
  end;

  // Create fused instruction: if (Src1 op 0) goto target
  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := FusedOpCode;
  FusedInstr.Dest := 0;
  FusedInstr.Src1 := CmpInstr.Src1;  // The value being compared to zero
  FusedInstr.Src2 := 0;
  FusedInstr.Immediate := JmpInstr.Immediate;  // Jump target
  // SourceLine now managed via Source Map (unchanged by superinstruction fusion)

  FProgram.SetInstruction(Index, FusedInstr);
  MakeNop(Index + 1);
  MakeNop(Index + 2);

  {$IFDEF DEBUG_SUPERINSTR}
  // DEBUG: Log fusion details
  if DebugSuperinstr then
    WriteLn(StdErr, Format('FUSE CompareZeroBranch @%d: opcode=%d Src1=%d Imm=%d',
      [Index, FusedOpCode, FusedInstr.Src1, FusedInstr.Immediate]));
  {$ENDIF}

  Inc(FFusedCount, 2);  // Count as 2 fusions (eliminated 2 instructions)
  Result := True;
end;

function TSuperinstructionOptimizer.TryFuseArrayStoreConst(Index: Integer): Boolean;
var
  LoadInstr, StoreInstr, FusedInstr: TBytecodeInstruction;
  LoadOp, StoreOp: TBytecodeOp;
  FusedOpCode: Word;
  ConstReg: Word;
begin
  { Pattern: LoadConstXxx Rtmp, K
             ArrayStoreXxx ARR, idx, Rtmp
     =>      ArrayStoreXxxConst ARR, idx, K

     This is very common in SIEVE: flags(j) = 0 and flags(i) = 1 }

  Result := False;

  if Index + 1 >= FProgram.GetInstructionCount then Exit;

  LoadInstr := FProgram.GetInstruction(Index);
  StoreInstr := FProgram.GetInstruction(Index + 1);

  // Safety: both instructions must be standard opcodes (< 100)
  if (LoadInstr.OpCode >= bcGroupSuper) or (StoreInstr.OpCode >= bcGroupSuper) then Exit;

  LoadOp := TBytecodeOp(LoadInstr.OpCode);
  StoreOp := TBytecodeOp(StoreInstr.OpCode);

  // Check for LoadConst followed by ArrayStore
  if not (LoadOp in [bcLoadConstInt, bcLoadConstFloat, bcLoadConstString]) then Exit;

  // Map ArrayStore types
  case StoreOp of
    bcArrayStoreInt: ;
    bcArrayStoreFloat: ;
    bcArrayStoreString: ;
  else
    Exit;
  end;

  ConstReg := LoadInstr.Dest;

  // Check if ArrayStore uses the constant as value (Dest field in ArrayStore)
  // ArrayStore format: Src1=ArrayIndex, Src2=IndexReg, Dest=ValueReg
  if StoreInstr.Dest <> ConstReg then Exit;

  // SAFETY: Don't fuse if the second instruction is a jump target
  if IsJumpTarget(Index + 1) then Exit;

  // Check if constant register is temporary (not used elsewhere)
  if not IsTemporaryResult(Index, ConstReg) then Exit;

  // Type matching: LoadConstInt -> ArrayStoreInt, etc.
  case LoadOp of
    bcLoadConstInt:
      if StoreOp = bcArrayStoreInt then
        FusedOpCode := bcArrayStoreIntConst
      else
        Exit;  // Type mismatch

    bcLoadConstFloat:
      if StoreOp = bcArrayStoreFloat then
        FusedOpCode := bcArrayStoreFloatConst
      else
        Exit;  // Type mismatch

    bcLoadConstString:
      if StoreOp = bcArrayStoreString then
        FusedOpCode := bcArrayStoreStringConst
      else
        Exit;  // Type mismatch
  else
    Exit;
  end;

  // Create fused instruction: ArrayStoreXxxConst ARR[idx] = immediate
  // Format: Src1=ArrayIndex, Src2=IndexReg, Immediate=ConstValue
  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := FusedOpCode;
  FusedInstr.Dest := 0;  // Not used
  FusedInstr.Src1 := StoreInstr.Src1;      // Array index
  FusedInstr.Src2 := StoreInstr.Src2;      // Index register
  FusedInstr.Immediate := LoadInstr.Immediate;  // Constant value
  // SourceLine now managed via Source Map

  FProgram.SetInstruction(Index, FusedInstr);
  MakeNop(Index + 1);

  {$IFDEF DEBUG_SUPERINSTR}
  if DebugSuperinstr then
    WriteLn(StdErr, Format('FUSE ArrayStoreConst @%d: opcode=%d Arr=%d Idx=%d Imm=%d',
      [Index, FusedOpCode, FusedInstr.Src1, FusedInstr.Src2, FusedInstr.Immediate]));
  {$ENDIF}

  Inc(FFusedCount);
  Result := True;
end;

function TSuperinstructionOptimizer.TryFuseLoopIncrementAndBranch(Index: Integer): Boolean;
var
  AddInstr, JumpInstr, BranchInstr, FusedInstr: TBytecodeInstruction;
  JumpOp: TBytecodeOp;
  FusedOpCode: Word;
  LoopTarget: Integer;
  JumpIndex: Integer;
  LoopBodyIndex: Integer;
begin
  { Pattern: AddIntTo Rdest, Rstep       ; counter += step
             [NOP]*                       ; optional NOPs from previous fusions
             Jump loopTop                 ; unconditional jump back to loop header

     Where loopTop contains: BranchGtInt Rdest, Rlimit, exitTarget

     The BranchGtInt at loopTop tests: if (counter > limit) goto exit
     So the loop continues while counter <= limit.

     Fused: AddIntToBranchLe Rdest, Rstep, Rlimit, loopBody
            (dest += step; if dest <= limit goto loopBody)

     Where loopBody = loopTop + 1 (skip the branch instruction)

     This pattern occurs at the END of FOR loops where:
     - AddIntTo increments the counter
     - Jump goes back to the loop header (which has BranchGtInt) }

  Result := False;

  AddInstr := FProgram.GetInstruction(Index);

  // Check for AddIntTo (superinstruction) or AddInt (standard opcode)
  // AddIntTo: Dest += Src1 (2 operands)
  // AddInt: Dest = Src1 + Src2 (3 operands, but for loops use Dest = Dest + Step)
  if (AddInstr.OpCode <> bcAddIntTo) and (AddInstr.OpCode <> bcAddInt) then Exit;

  // For AddInt, we need Dest = Dest + Src (self-increment pattern)
  if AddInstr.OpCode = bcAddInt then
  begin
    if AddInstr.Dest <> AddInstr.Src1 then Exit;  // Must be K% = K% + step
  end;

  // Find Jump instruction, skipping any NOPs
  JumpIndex := Index + 1;
  while JumpIndex < FProgram.GetInstructionCount do
  begin
    JumpInstr := FProgram.GetInstruction(JumpIndex);
    if JumpInstr.OpCode <> Byte(bcNop) then
      Break;
    Inc(JumpIndex);
  end;

  // Need to have found a valid instruction
  if JumpIndex >= FProgram.GetInstructionCount then Exit;

  // Safety check on Jump
  if JumpInstr.OpCode >= bcGroupSuper then Exit;
  JumpOp := TBytecodeOp(JumpInstr.OpCode);

  // Check for unconditional Jump
  if JumpOp <> bcJump then Exit;

  // Get the loop header target from the Jump
  LoopTarget := JumpInstr.Immediate;

  // Validate loop target is within bounds
  if (LoopTarget < 0) or (LoopTarget >= FProgram.GetInstructionCount) then Exit;

  // Get the instruction at the loop header
  BranchInstr := FProgram.GetInstruction(LoopTarget);

  {$IFDEF DEBUG_SUPERINSTR}
  if DebugSuperinstr then
    WriteLn(StdErr, Format('TryFuseLoop @%d: AddDest=%d JumpTarget=%d BranchOp=%d (expect %d) BranchSrc1=%d',
      [Index, AddInstr.Dest, LoopTarget, BranchInstr.OpCode, bcBranchGtInt, BranchInstr.Src1]));
  {$ENDIF}

  // Check for BranchGtInt at the loop header
  if BranchInstr.OpCode <> bcBranchGtInt then Exit;

  // Check that BranchGtInt tests the same counter as AddIntTo increments
  if BranchInstr.Src1 <> AddInstr.Dest then Exit;

  // SAFETY: Don't fuse if Jump is a jump target itself
  if IsJumpTarget(JumpIndex) then Exit;

  // Find the first non-NOP instruction after BranchGtInt - that's the loop body
  LoopBodyIndex := LoopTarget + 1;
  while (LoopBodyIndex < FProgram.GetInstructionCount) and
        (FProgram.GetInstruction(LoopBodyIndex).OpCode = Byte(bcNop)) do
    Inc(LoopBodyIndex);

  // Create fused instruction: dest += step; if (dest <= limit) goto loopBody
  FusedOpCode := bcAddIntToBranchLe;

  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := FusedOpCode;
  FusedInstr.Dest := AddInstr.Dest;       // Counter register
  // For AddIntTo: Dest += Src1, so Src1 is step
  // For AddInt: Dest = Src1 + Src2 where Dest=Src1, so Src2 is step
  if AddInstr.OpCode = bcAddIntTo then
    FusedInstr.Src1 := AddInstr.Src1       // Step register (AddIntTo format)
  else
    FusedInstr.Src1 := AddInstr.Src2;      // Step register (AddInt format: K%=K%+step)
  FusedInstr.Src2 := BranchInstr.Src2;    // Limit register
  FusedInstr.Immediate := LoopBodyIndex;  // Loop body (first non-NOP after branch)
  // SourceLine now managed via Source Map

  FProgram.SetInstruction(Index, FusedInstr);
  MakeNop(JumpIndex);  // Remove Jump

  // NOTE: We do NOT remove the BranchGtInt at LoopTarget because:
  // 1. It's needed for the first iteration (entry from outside the loop)
  // 2. Other code may jump to it

  {$IFDEF DEBUG_SUPERINSTR}
  if DebugSuperinstr then
    WriteLn(StdErr, Format('FUSE LoopIncrBranch @%d: Dest=%d Step=%d Limit=%d Target=%d',
      [Index, FusedInstr.Dest, FusedInstr.Src1, FusedInstr.Src2, FusedInstr.Immediate]));
  {$ENDIF}

  Inc(FFusedCount);  // Saved 1 instruction (the Jump)
  Result := True;
end;

function TSuperinstructionOptimizer.TryFuseMulAddFloat(Index: Integer): Boolean;
var
  MulInstr, AddInstr, FusedInstr: TBytecodeInstruction;
  MulOp, AddOp: TBytecodeOp;
  FusedOpCode: Word;
  MulDest: Word;
begin
  { Pattern 1: MulFloat Rtmp, Ra, Rb + AddFloat Rdest, Rc, Rtmp => bcMulAddFloat dest = c + a*b
    Pattern 2: MulFloat Rtmp, Ra, Rb + SubFloat Rdest, Rc, Rtmp => bcMulSubFloat dest = c - a*b
    Pattern 3: MulFloat Rtmp, Ra, Rb + AddFloatTo Rdest, Rtmp => bcMulAddToFloat dest += a*b
    Pattern 4: MulFloat Rtmp, Ra, Rb + SubFloatTo Rdest, Rtmp => bcMulSubToFloat dest -= a*b }

  Result := False;

  if Index + 1 >= FProgram.GetInstructionCount then Exit;

  MulInstr := FProgram.GetInstruction(Index);
  AddInstr := FProgram.GetInstruction(Index + 1);

  // First instruction must be MulFloat
  if MulInstr.OpCode >= bcGroupSuper then Exit;
  MulOp := TBytecodeOp(MulInstr.OpCode);
  if MulOp <> bcMulFloat then Exit;

  MulDest := MulInstr.Dest;

  // SAFETY: Don't fuse if second instruction is a jump target
  if IsJumpTarget(Index + 1) then Exit;

  // Check second instruction
  if AddInstr.OpCode >= bcGroupSuper then
  begin
    // Could be a superinstruction AddFloatTo or SubFloatTo
    case AddInstr.OpCode of
      bcAddFloatTo: // Dest += Src1
        begin
          // Check if AddFloatTo uses the mul result
          if AddInstr.Src1 <> MulDest then Exit;
          // Check if mul result is temporary
          // AddInstr.Dest is what the fused instruction writes - see IsTemporaryResult.
          if not IsTemporaryResult(Index, MulDest, AddInstr.Dest) then Exit;

          FusedOpCode := bcMulAddToFloat;

          // Create: dest += a*b
          FillChar(FusedInstr, SizeOf(FusedInstr), 0);
          FusedInstr.OpCode := FusedOpCode;
          FusedInstr.Dest := AddInstr.Dest;     // Accumulator
          FusedInstr.Src1 := MulInstr.Src1;     // a
          FusedInstr.Src2 := MulInstr.Src2;     // b
          FusedInstr.Immediate := 0;
          // SourceLine now managed via Source Map

          FProgram.SetInstruction(Index, FusedInstr);
          MakeNop(Index + 1);

          {$IFDEF DEBUG_SUPERINSTR}
          if DebugSuperinstr then
            WriteLn(StdErr, Format('FUSE MulAddToFloat @%d: Dest=%d Src1=%d Src2=%d',
              [Index, FusedInstr.Dest, FusedInstr.Src1, FusedInstr.Src2]));
          {$ENDIF}

          Inc(FFusedCount);
          Result := True;
          Exit;
        end;

      bcSubFloatTo: // Dest -= Src1
        begin
          // Check if SubFloatTo uses the mul result
          if AddInstr.Src1 <> MulDest then Exit;
          // Check if mul result is temporary
          // AddInstr.Dest is what the fused instruction writes - see IsTemporaryResult.
          if not IsTemporaryResult(Index, MulDest, AddInstr.Dest) then Exit;

          FusedOpCode := bcMulSubToFloat;

          // Create: dest -= a*b
          FillChar(FusedInstr, SizeOf(FusedInstr), 0);
          FusedInstr.OpCode := FusedOpCode;
          FusedInstr.Dest := AddInstr.Dest;     // Accumulator
          FusedInstr.Src1 := MulInstr.Src1;     // a
          FusedInstr.Src2 := MulInstr.Src2;     // b
          FusedInstr.Immediate := 0;
          // SourceLine now managed via Source Map

          FProgram.SetInstruction(Index, FusedInstr);
          MakeNop(Index + 1);

          {$IFDEF DEBUG_SUPERINSTR}
          if DebugSuperinstr then
            WriteLn(StdErr, Format('FUSE MulSubToFloat @%d: Dest=%d Src1=%d Src2=%d',
              [Index, FusedInstr.Dest, FusedInstr.Src1, FusedInstr.Src2]));
          {$ENDIF}

          Inc(FFusedCount);
          Result := True;
          Exit;
        end;
    else
      Exit;
    end;
  end
  else
  begin
    // Standard opcode
    AddOp := TBytecodeOp(AddInstr.OpCode);

    case AddOp of
      bcAddFloat:
        begin
          // Check if AddFloat uses the mul result as Src2
          if AddInstr.Src2 <> MulDest then Exit;
          // Check if mul result is temporary
          // AddInstr.Dest is what the fused instruction writes - see IsTemporaryResult.
          if not IsTemporaryResult(Index, MulDest, AddInstr.Dest) then Exit;
          // SAFETY: the extra operand c (Src1, stored in the fused op's Immediate) must not be the mul
          // destination — the fused op recomputes a*b into that register, so reading c=MulDest would take
          // its STALE pre-multiply value. Happens for "(a*b) + (a*b)" (e.g. strength-reduced "2.0*(a*b)").
          if AddInstr.Src1 = MulDest then Exit;

          FusedOpCode := bcMulAddFloat;

          // Create: dest = c + a*b (where c is AddInstr.Src1)
          FillChar(FusedInstr, SizeOf(FusedInstr), 0);
          FusedInstr.OpCode := FusedOpCode;
          FusedInstr.Dest := AddInstr.Dest;      // Result
          FusedInstr.Src1 := MulInstr.Src1;      // a
          FusedInstr.Src2 := MulInstr.Src2;      // b
          FusedInstr.Immediate := AddInstr.Src1; // c (extra operand stored in Immediate)
          // SourceLine now managed via Source Map

          FProgram.SetInstruction(Index, FusedInstr);
          MakeNop(Index + 1);

          {$IFDEF DEBUG_SUPERINSTR}
          if DebugSuperinstr then
            WriteLn(StdErr, Format('FUSE MulAddFloat @%d: Dest=%d a=%d b=%d c=%d',
              [Index, FusedInstr.Dest, FusedInstr.Src1, FusedInstr.Src2, FusedInstr.Immediate]));
          {$ENDIF}

          Inc(FFusedCount);
          Result := True;
          Exit;
        end;

      bcSubFloat:
        begin
          // Check if SubFloat uses the mul result as Src2 (c - a*b)
          if AddInstr.Src2 <> MulDest then Exit;
          // Check if mul result is temporary
          // AddInstr.Dest is what the fused instruction writes - see IsTemporaryResult.
          if not IsTemporaryResult(Index, MulDest, AddInstr.Dest) then Exit;
          // SAFETY: the extra operand c (Src1) must not be the mul destination (see the bcAddFloat note).
          if AddInstr.Src1 = MulDest then Exit;

          FusedOpCode := bcMulSubFloat;

          // Create: dest = c - a*b (where c is AddInstr.Src1)
          FillChar(FusedInstr, SizeOf(FusedInstr), 0);
          FusedInstr.OpCode := FusedOpCode;
          FusedInstr.Dest := AddInstr.Dest;      // Result
          FusedInstr.Src1 := MulInstr.Src1;      // a
          FusedInstr.Src2 := MulInstr.Src2;      // b
          FusedInstr.Immediate := AddInstr.Src1; // c (extra operand stored in Immediate)
          // SourceLine now managed via Source Map

          FProgram.SetInstruction(Index, FusedInstr);
          MakeNop(Index + 1);

          {$IFDEF DEBUG_SUPERINSTR}
          if DebugSuperinstr then
            WriteLn(StdErr, Format('FUSE MulSubFloat @%d: Dest=%d a=%d b=%d c=%d',
              [Index, FusedInstr.Dest, FusedInstr.Src1, FusedInstr.Src2, FusedInstr.Immediate]));
          {$ENDIF}

          Inc(FFusedCount);
          Result := True;
          Exit;
        end;
    end;
  end;
end;

function TSuperinstructionOptimizer.TryFuseArrayLoadAddFloat(Index: Integer): Boolean;
var
  LoadInstr, AddInstr, FusedInstr: TBytecodeInstruction;
  LoadOp, AddOp: TBytecodeOp;
  FusedOpCode: Word;
  LoadDest: Word;
begin
  { Pattern: ArrayLoadFloat Rtmp, arr, idx + AddFloat Rdest, Racc, Rtmp => bcArrayLoadAddFloat
            ArrayLoadFloat Rtmp, arr, idx + SubFloat Rdest, Racc, Rtmp => bcArrayLoadSubFloat }

  Result := False;

  if Index + 1 >= FProgram.GetInstructionCount then Exit;

  LoadInstr := FProgram.GetInstruction(Index);
  AddInstr := FProgram.GetInstruction(Index + 1);

  // First instruction must be ArrayLoadFloat
  if LoadInstr.OpCode >= bcGroupSuper then Exit;
  LoadOp := TBytecodeOp(LoadInstr.OpCode);
  if LoadOp <> bcArrayLoadFloat then Exit;

  LoadDest := LoadInstr.Dest;

  // SAFETY: Don't fuse if second instruction is a jump target
  if IsJumpTarget(Index + 1) then Exit;

  // Check if load result is temporary
  if not IsTemporaryResult(Index, LoadDest) then Exit;

  // Check second instruction
  if AddInstr.OpCode >= bcGroupSuper then Exit;
  AddOp := TBytecodeOp(AddInstr.OpCode);

  case AddOp of
    bcAddFloat:
      begin
        // Check if AddFloat uses the load result as Src2
        if AddInstr.Src2 <> LoadDest then Exit;
        // SAFETY: the accumulator (Src1, which becomes the fused op's Immediate operand) must NOT be the
        // load destination. The fused op reads the accumulator and performs the array load itself, so if
        // the accumulator IS the load dest it reads the register's STALE pre-load value instead of the
        // loaded element. This happens for "x + x" where both operands are the same loaded value (e.g.
        // strength-reduced "2.0 * arr(i)" -> "arr(i) + arr(i)"): fusing gives "stale + arr[idx]" = arr[idx]
        // instead of 2*arr[idx]. Leave it unfused in that case.
        if AddInstr.Src1 = LoadDest then Exit;

        FusedOpCode := bcArrayLoadAddFloat;

        // Create: dest = acc + arr[idx]
        FillChar(FusedInstr, SizeOf(FusedInstr), 0);
        FusedInstr.OpCode := FusedOpCode;
        FusedInstr.Dest := AddInstr.Dest;       // Result
        FusedInstr.Src1 := LoadInstr.Src1;      // Array index
        FusedInstr.Src2 := LoadInstr.Src2;      // Index register
        FusedInstr.Immediate := AddInstr.Src1;  // Accumulator register
        // SourceLine now managed via Source Map

        FProgram.SetInstruction(Index, FusedInstr);
        MakeNop(Index + 1);

        {$IFDEF DEBUG_SUPERINSTR}
        if DebugSuperinstr then
          WriteLn(StdErr, Format('FUSE ArrayLoadAddFloat @%d: Dest=%d Arr=%d Idx=%d Acc=%d',
            [Index, FusedInstr.Dest, FusedInstr.Src1, FusedInstr.Src2, FusedInstr.Immediate]));
        {$ENDIF}

        Inc(FFusedCount);
        Result := True;
      end;

    bcSubFloat:
      begin
        // Check if SubFloat uses the load result as Src2 (acc - arr[idx])
        if AddInstr.Src2 <> LoadDest then Exit;
        // SAFETY: the accumulator (Src1) must not be the load destination — see the bcAddFloat note.
        if AddInstr.Src1 = LoadDest then Exit;

        FusedOpCode := bcArrayLoadSubFloat;

        // Create: dest = acc - arr[idx]
        FillChar(FusedInstr, SizeOf(FusedInstr), 0);
        FusedInstr.OpCode := FusedOpCode;
        FusedInstr.Dest := AddInstr.Dest;       // Result
        FusedInstr.Src1 := LoadInstr.Src1;      // Array index
        FusedInstr.Src2 := LoadInstr.Src2;      // Index register
        FusedInstr.Immediate := AddInstr.Src1;  // Accumulator register
        // SourceLine now managed via Source Map

        FProgram.SetInstruction(Index, FusedInstr);
        MakeNop(Index + 1);

        {$IFDEF DEBUG_SUPERINSTR}
        if DebugSuperinstr then
          WriteLn(StdErr, Format('FUSE ArrayLoadSubFloat @%d: Dest=%d Arr=%d Idx=%d Acc=%d',
            [Index, FusedInstr.Dest, FusedInstr.Src1, FusedInstr.Src2, FusedInstr.Immediate]));
        {$ENDIF}

        Inc(FFusedCount);
        Result := True;
      end;
  end;
end;

function TSuperinstructionOptimizer.TryFuseSquareSumFloat(Index: Integer): Boolean;
var
  Mul1Instr, Mul2Instr, AddInstr, FusedInstr: TBytecodeInstruction;
  Mul1Dest, Mul2Dest: Word;
begin
  { Pattern: MulFloat Rsq1, Rx, Rx + MulFloat Rsq2, Ry, Ry + AddFloat Rdest, Rsq1, Rsq2
            => bcSquareSumFloat dest = x*x + y*y

    Also: MulFloat Rsq, Rx, Rx + AddFloat Rdest, Rsum, Rsq
          => bcAddSquareFloat dest = sum + x*x }

  Result := False;

  // First check for simple AddSquare pattern (2 instructions)
  if Index + 1 >= FProgram.GetInstructionCount then Exit;

  Mul1Instr := FProgram.GetInstruction(Index);
  AddInstr := FProgram.GetInstruction(Index + 1);

  // First instruction must be MulFloat
  if Mul1Instr.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(Mul1Instr.OpCode) <> bcMulFloat then Exit;

  // Check if it's a square (Src1 == Src2)
  if Mul1Instr.Src1 <> Mul1Instr.Src2 then Exit;

  Mul1Dest := Mul1Instr.Dest;

  // SAFETY: Don't fuse if second instruction is a jump target
  if IsJumpTarget(Index + 1) then Exit;

  // ⛔ THIS CHECK USED TO LIVE HERE, BEFORE THE CASE, and it is why bcSquareSumFloat could not be
  // produced by anything. The two-instruction form needs Mul1's result dead after the PAIR; the
  // three-instruction form has it read by the AddFloat at Index+2, which is part of the pattern.
  // Asking the pair's question of the triple always answers no, so the triple never fused - and the
  // C hot loop carried an arm for an opcode nothing could emit. Each branch now asks its own.
  if AddInstr.OpCode >= bcGroupSuper then Exit;

  case TBytecodeOp(AddInstr.OpCode) of
    bcAddFloat:
      begin
        // Pattern: sq + something
        // If sq is in Src2, we have: dest = sum + sq => AddSquareFloat
        if not IsTemporaryResult(Index, Mul1Dest) then Exit;
        if AddInstr.Src2 = Mul1Dest then
        begin
          // Create: dest = sum + x*x
          FillChar(FusedInstr, SizeOf(FusedInstr), 0);
          FusedInstr.OpCode := bcAddSquareFloat;
          FusedInstr.Dest := AddInstr.Dest;      // Result
          FusedInstr.Src1 := AddInstr.Src1;      // Existing sum
          FusedInstr.Src2 := Mul1Instr.Src1;     // x (value to square)
          // SourceLine now managed via Source Map

          FProgram.SetInstruction(Index, FusedInstr);
          MakeNop(Index + 1);

          {$IFDEF DEBUG_SUPERINSTR}
          if DebugSuperinstr then
            WriteLn(StdErr, Format('FUSE AddSquareFloat @%d: Dest=%d Sum=%d X=%d',
              [Index, FusedInstr.Dest, FusedInstr.Src1, FusedInstr.Src2]));
          {$ENDIF}

          Inc(FFusedCount);
          Result := True;
          Exit;
        end;
      end;

    bcMulFloat:
      begin
        // Could be second square - check for 3-instruction pattern
        if Index + 2 >= FProgram.GetInstructionCount then Exit;

        Mul2Instr := AddInstr; // This is actually the second MulFloat
        AddInstr := FProgram.GetInstruction(Index + 2);

        // Check if second is also a square
        if Mul2Instr.Src1 <> Mul2Instr.Src2 then Exit;

        Mul2Dest := Mul2Instr.Dest;

        // SAFETY: Don't fuse if third instruction is a jump target
        if IsJumpTarget(Index + 2) then Exit;

        // Check if both results are temporary
        if not IsTemporaryResult(Index + 1, Mul2Dest, -1, Index + 2) then Exit;
        // Mul1's result IS read at Index+2, by the AddFloat that is part of the pattern: the
        // question is whether anything reads it AFTER the triple.
        if not IsTemporaryResult(Index, Mul1Dest, -1, Index + 2) then Exit;

        // Check third instruction is AddFloat that combines both squares
        if AddInstr.OpCode >= bcGroupSuper then Exit;
        if TBytecodeOp(AddInstr.OpCode) <> bcAddFloat then Exit;

        // Check if AddFloat uses both squares
        if not ((AddInstr.Src1 = Mul1Dest) and (AddInstr.Src2 = Mul2Dest)) and
           not ((AddInstr.Src1 = Mul2Dest) and (AddInstr.Src2 = Mul1Dest)) then Exit;

        // Create: dest = x*x + y*y
        FillChar(FusedInstr, SizeOf(FusedInstr), 0);
        FusedInstr.OpCode := bcSquareSumFloat;
        FusedInstr.Dest := AddInstr.Dest;       // Result
        FusedInstr.Src1 := Mul1Instr.Src1;      // x
        FusedInstr.Src2 := Mul2Instr.Src1;      // y
        // SourceLine now managed via Source Map

        FProgram.SetInstruction(Index, FusedInstr);
        MakeNop(Index + 1);
        MakeNop(Index + 2);

        {$IFDEF DEBUG_SUPERINSTR}
        if DebugSuperinstr then
          WriteLn(StdErr, Format('FUSE SquareSumFloat @%d: Dest=%d X=%d Y=%d',
            [Index, FusedInstr.Dest, FusedInstr.Src1, FusedInstr.Src2]));
        {$ENDIF}

        Inc(FFusedCount, 2);  // Saved 2 instructions
        Result := True;
      end;
  end;
end;

function TSuperinstructionOptimizer.TryFuseMulMulFloat(Index: Integer): Boolean;
var
  Mul1Instr, Mul2Instr, FusedInstr: TBytecodeInstruction;
  Mul1Dest: Word;
begin
  { Pattern: MulFloat Rtmp, Ra, Rb + MulFloat Rdest, Rtmp, Rc => bcMulMulFloat dest = a*b*c }

  Result := False;

  if Index + 1 >= FProgram.GetInstructionCount then Exit;

  Mul1Instr := FProgram.GetInstruction(Index);
  Mul2Instr := FProgram.GetInstruction(Index + 1);

  // Both must be MulFloat
  if Mul1Instr.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(Mul1Instr.OpCode) <> bcMulFloat then Exit;

  if Mul2Instr.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(Mul2Instr.OpCode) <> bcMulFloat then Exit;

  Mul1Dest := Mul1Instr.Dest;

  // SAFETY: Don't fuse if second instruction is a jump target
  if IsJumpTarget(Index + 1) then Exit;

  // Check if first result is temporary
  if not IsTemporaryResult(Index, Mul1Dest) then Exit;

  // Check if second mul uses first result as Src1
  if Mul2Instr.Src1 <> Mul1Dest then Exit;

  // Create: dest = a*b*c
  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := bcMulMulFloat;
  FusedInstr.Dest := Mul2Instr.Dest;      // Result
  FusedInstr.Src1 := Mul1Instr.Src1;      // a
  FusedInstr.Src2 := Mul1Instr.Src2;      // b
  FusedInstr.Immediate := Mul2Instr.Src2; // c
  // SourceLine now managed via Source Map

  FProgram.SetInstruction(Index, FusedInstr);
  MakeNop(Index + 1);

  {$IFDEF DEBUG_SUPERINSTR}
  if DebugSuperinstr then
    WriteLn(StdErr, Format('FUSE MulMulFloat @%d: Dest=%d a=%d b=%d c=%d',
      [Index, FusedInstr.Dest, FusedInstr.Src1, FusedInstr.Src2, FusedInstr.Immediate]));
  {$ENDIF}

  Inc(FFusedCount);
  Result := True;
end;

function TSuperinstructionOptimizer.TryFuseAddSqrtFloat(Index: Integer): Boolean;
var
  AddInstr, SqrtInstr, FusedInstr: TBytecodeInstruction;
  AddDest: Word;
begin
  { Pattern: AddFloat Rtmp, Ra, Rb + MathSqr Rdest, Rtmp => bcAddSqrtFloat dest = sqrt(a+b) }

  Result := False;

  if Index + 1 >= FProgram.GetInstructionCount then Exit;

  AddInstr := FProgram.GetInstruction(Index);
  SqrtInstr := FProgram.GetInstruction(Index + 1);

  // First must be AddFloat
  if AddInstr.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(AddInstr.OpCode) <> bcAddFloat then Exit;

  // Second must be MathSqr
  if SqrtInstr.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(SqrtInstr.OpCode) <> bcMathSqr then Exit;

  AddDest := AddInstr.Dest;

  // SAFETY: Don't fuse if second instruction is a jump target
  if IsJumpTarget(Index + 1) then Exit;

  // Check if add result is temporary
  if not IsTemporaryResult(Index, AddDest) then Exit;

  // Check if sqrt uses add result
  if SqrtInstr.Src1 <> AddDest then Exit;

  // Create: dest = sqrt(a+b)
  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := bcAddSqrtFloat;
  FusedInstr.Dest := SqrtInstr.Dest;     // Result
  FusedInstr.Src1 := AddInstr.Src1;      // a
  FusedInstr.Src2 := AddInstr.Src2;      // b
  // SourceLine now managed via Source Map

  FProgram.SetInstruction(Index, FusedInstr);
  MakeNop(Index + 1);

  {$IFDEF DEBUG_SUPERINSTR}
  if DebugSuperinstr then
    WriteLn(StdErr, Format('FUSE AddSqrtFloat @%d: Dest=%d a=%d b=%d',
      [Index, FusedInstr.Dest, FusedInstr.Src1, FusedInstr.Src2]));
  {$ENDIF}

  Inc(FFusedCount);
  Result := True;
end;

function TSuperinstructionOptimizer.TryFuseArrayLoadBranchInt(Index: Integer): Boolean;
var
  LoadInstr, BranchInstr, FusedInstr: TBytecodeInstruction;
  LoadDest: Word;
  FusedOpCode: Word;
begin
  { Pattern: ArrayLoadInt Rtmp, arr, idx + BranchNeZeroInt Rtmp, target => bcArrayLoadIntBranchNZ
            ArrayLoadInt Rtmp, arr, idx + BranchEqZeroInt Rtmp, target => bcArrayLoadIntBranchZ }

  Result := False;

  if Index + 1 >= FProgram.GetInstructionCount then Exit;

  LoadInstr := FProgram.GetInstruction(Index);
  BranchInstr := FProgram.GetInstruction(Index + 1);

  // First must be ArrayLoadInt
  if LoadInstr.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(LoadInstr.OpCode) <> bcArrayLoadInt then Exit;

  LoadDest := LoadInstr.Dest;

  // SAFETY: Don't fuse if second instruction is a jump target
  if IsJumpTarget(Index + 1) then Exit;

  // Check if load result is temporary
  if not IsTemporaryResult(Index, LoadDest) then Exit;

  // Check second instruction - must be BranchEqZeroInt or BranchNeZeroInt
  case BranchInstr.OpCode of
    bcBranchNeZeroInt:
      begin
        if BranchInstr.Src1 <> LoadDest then Exit;
        FusedOpCode := bcArrayLoadIntBranchNZ;
      end;
    bcBranchEqZeroInt:
      begin
        if BranchInstr.Src1 <> LoadDest then Exit;
        FusedOpCode := bcArrayLoadIntBranchZ;
      end;
  else
    Exit;
  end;

  // Create fused instruction
  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := FusedOpCode;
  FusedInstr.Dest := 0;                    // Not used
  FusedInstr.Src1 := LoadInstr.Src1;       // Array index
  FusedInstr.Src2 := LoadInstr.Src2;       // Index register
  FusedInstr.Immediate := BranchInstr.Immediate; // Branch target
  // SourceLine now managed via Source Map

  FProgram.SetInstruction(Index, FusedInstr);
  MakeNop(Index + 1);

  {$IFDEF DEBUG_SUPERINSTR}
  if DebugSuperinstr then
    WriteLn(StdErr, Format('FUSE ArrayLoadBranchInt @%d: Op=%d Arr=%d Idx=%d Target=%d',
      [Index, FusedOpCode, FusedInstr.Src1, FusedInstr.Src2, FusedInstr.Immediate]));
  {$ENDIF}

  Inc(FFusedCount);
  Result := True;
end;

function TSuperinstructionOptimizer.TryFuseArraySwapInt(Index: Integer): Boolean;
var
  Load1Instr, CopyInstr, Load2Instr, Store1Instr, Store2Instr, FusedInstr: TBytecodeInstruction;
  Load1Dest, CopyDest, Load2Dest: Word;
  ArrayIdx: Word;
begin
  { Pattern: ArrayLoadInt  Rtmp1, arr, idx1   ; tmp1 = arr[idx1]
             CopyInt       Rsave, Rtmp1       ; save = tmp1
             ArrayLoadInt  Rtmp2, arr, idx2   ; tmp2 = arr[idx2]
             ArrayStoreInt arr, idx1, Rtmp2   ; arr[idx1] = tmp2
             ArrayStoreInt arr, idx2, Rsave   ; arr[idx2] = save

     Becomes: ArraySwapInt arr, idx1, idx2
     Format: Src1=arr_index, Src2=idx1_reg, Dest=idx2_reg }

  Result := False;

  if Index + 4 >= FProgram.GetInstructionCount then Exit;

  Load1Instr := FProgram.GetInstruction(Index);
  CopyInstr := FProgram.GetInstruction(Index + 1);
  Load2Instr := FProgram.GetInstruction(Index + 2);
  Store1Instr := FProgram.GetInstruction(Index + 3);
  Store2Instr := FProgram.GetInstruction(Index + 4);

  // All must be standard opcodes (< 100)
  if (Load1Instr.OpCode >= bcGroupSuper) or (CopyInstr.OpCode >= bcGroupSuper) or
     (Load2Instr.OpCode >= bcGroupSuper) or (Store1Instr.OpCode >= bcGroupSuper) or
     (Store2Instr.OpCode >= bcGroupSuper) then Exit;

  // Check instruction types
  if TBytecodeOp(Load1Instr.OpCode) <> bcArrayLoadInt then Exit;
  if TBytecodeOp(CopyInstr.OpCode) <> bcCopyInt then Exit;
  if TBytecodeOp(Load2Instr.OpCode) <> bcArrayLoadInt then Exit;
  if TBytecodeOp(Store1Instr.OpCode) <> bcArrayStoreInt then Exit;
  if TBytecodeOp(Store2Instr.OpCode) <> bcArrayStoreInt then Exit;

  // Extract register destinations
  Load1Dest := Load1Instr.Dest;
  CopyDest := CopyInstr.Dest;
  Load2Dest := Load2Instr.Dest;

  // Get array index - all operations must be on the same array
  ArrayIdx := Load1Instr.Src1;
  if Load2Instr.Src1 <> ArrayIdx then Exit;
  if Store1Instr.Src1 <> ArrayIdx then Exit;
  if Store2Instr.Src1 <> ArrayIdx then Exit;

  // Verify pattern: CopyInt saves Load1 result
  if CopyInstr.Src1 <> Load1Dest then Exit;

  // Verify Store1 stores Load2 result at idx1 position
  if Store1Instr.Src2 <> Load1Instr.Src2 then Exit;  // Same index as Load1
  if Store1Instr.Dest <> Load2Dest then Exit;        // Value from Load2

  // Verify Store2 stores saved value at idx2 position
  if Store2Instr.Src2 <> Load2Instr.Src2 then Exit;  // Same index as Load2
  if Store2Instr.Dest <> CopyDest then Exit;          // Saved value

  // SAFETY: Don't fuse if any instruction is a jump target
  if IsJumpTarget(Index + 1) then Exit;
  if IsJumpTarget(Index + 2) then Exit;
  if IsJumpTarget(Index + 3) then Exit;
  if IsJumpTarget(Index + 4) then Exit;

  // Check temporaries
  if not IsTemporaryResult(Index, Load1Dest) then Exit;
  if not IsTemporaryResult(Index + 2, Load2Dest) then Exit;
  // CopyDest (save) is used in Store2, which is Index+4, so we need a different check
  // Actually CopyDest is NOT temporary - it's used later. But that's OK, we're replacing the whole pattern.

  // Create fused instruction: ArraySwapInt arr, idx1, idx2
  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := bcArraySwapInt;
  FusedInstr.Src1 := ArrayIdx;              // Array index
  FusedInstr.Src2 := Load1Instr.Src2;       // idx1 register
  FusedInstr.Dest := Load2Instr.Src2;       // idx2 register
  FusedInstr.Immediate := 0;
  // SourceLine now managed via Source Map

  FProgram.SetInstruction(Index, FusedInstr);
  MakeNop(Index + 1);
  MakeNop(Index + 2);
  MakeNop(Index + 3);
  MakeNop(Index + 4);

  {$IFDEF DEBUG_SUPERINSTR}
  if DebugSuperinstr then
    WriteLn(StdErr, Format('FUSE ArraySwapInt @%d: Arr=%d Idx1=%d Idx2=%d',
      [Index, FusedInstr.Src1, FusedInstr.Src2, FusedInstr.Dest]));
  {$ENDIF}

  Inc(FFusedCount, 4);  // Saved 4 instructions
  Result := True;
end;

function TSuperinstructionOptimizer.TryFuseAddIntSelf(Index: Integer): Boolean;
var
  ArithInstr, FusedInstr: TBytecodeInstruction;
  ArithOp: TBytecodeOp;
  FusedOpCode: Word;
begin
  { Pattern: AddInt Rdest, Rdest, Rsrc  (when dest == src1)
             SubInt Rdest, Rdest, Rsrc  (when dest == src1)

     Becomes: AddIntSelf Rdest, Rsrc   (dest += src)
              SubIntSelf Rdest, Rsrc   (dest -= src)

     This pattern is NOT caught by TryFuseArithAndCopy because there's no CopyInt after.
     Very common in fannkuch-redux: I% = I% + C1%, J% = J% - C1% }

  Result := False;

  ArithInstr := FProgram.GetInstruction(Index);

  // Must be standard opcode
  if ArithInstr.OpCode >= bcGroupSuper then Exit;

  ArithOp := TBytecodeOp(ArithInstr.OpCode);

  // Check for AddInt or SubInt where Dest == Src1 (self-modification)
  case ArithOp of
    bcAddInt:
      begin
        if ArithInstr.Dest <> ArithInstr.Src1 then Exit;
        FusedOpCode := bcAddIntSelf;
      end;
    bcSubInt:
      begin
        if ArithInstr.Dest <> ArithInstr.Src1 then Exit;
        FusedOpCode := bcSubIntSelf;
      end;
  else
    Exit;
  end;

  // Create fused instruction: dest += src  or  dest -= src
  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := FusedOpCode;
  FusedInstr.Dest := ArithInstr.Dest;      // Register to modify
  FusedInstr.Src1 := ArithInstr.Src2;      // Amount register
  FusedInstr.Src2 := 0;
  FusedInstr.Immediate := 0;
  // SourceLine now managed via Source Map

  FProgram.SetInstruction(Index, FusedInstr);

  {$IFDEF DEBUG_SUPERINSTR}
  if DebugSuperinstr then
    WriteLn(StdErr, Format('FUSE AddIntSelf @%d: Op=%d Dest=%d Src=%d',
      [Index, FusedOpCode, FusedInstr.Dest, FusedInstr.Src1]));
  {$ENDIF}

  Inc(FFusedCount);
  Result := True;
end;

function TSuperinstructionOptimizer.TryFuseArrayLoadIntTo(Index: Integer): Boolean;
var
  LoadInstr, CopyInstr, FusedInstr: TBytecodeInstruction;
  LoadDest: Word;
begin
  { Pattern: ArrayLoadInt Rtmp, arr, idx + CopyInt Rdest, Rtmp
     Becomes: ArrayLoadIntTo Rdest, arr, idx
     Format: Dest=final destination, Src1=arr_index, Src2=idx_reg }

  Result := False;

  if Index + 1 >= FProgram.GetInstructionCount then Exit;

  LoadInstr := FProgram.GetInstruction(Index);
  CopyInstr := FProgram.GetInstruction(Index + 1);

  // Both must be standard opcodes (< 100)
  if (LoadInstr.OpCode >= bcGroupSuper) or (CopyInstr.OpCode >= bcGroupSuper) then Exit;

  // Check instruction types
  if TBytecodeOp(LoadInstr.OpCode) <> bcArrayLoadInt then Exit;
  if TBytecodeOp(CopyInstr.OpCode) <> bcCopyInt then Exit;

  LoadDest := LoadInstr.Dest;

  // Check if CopyInt uses the ArrayLoadInt result
  if CopyInstr.Src1 <> LoadDest then Exit;

  // SAFETY: Don't fuse if second instruction is a jump target
  if IsJumpTarget(Index + 1) then Exit;

  // Check if load result is temporary
  if not IsTemporaryResult(Index, LoadDest) then Exit;

  // Create fused instruction: dest = arr[idx]
  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := bcArrayLoadIntTo;
  FusedInstr.Dest := CopyInstr.Dest;       // Final destination register
  FusedInstr.Src1 := LoadInstr.Src1;       // Array index
  FusedInstr.Src2 := LoadInstr.Src2;       // Index register
  FusedInstr.Immediate := 0;
  // SourceLine now managed via Source Map

  FProgram.SetInstruction(Index, FusedInstr);
  MakeNop(Index + 1);

  {$IFDEF DEBUG_SUPERINSTR}
  if DebugSuperinstr then
    WriteLn(StdErr, Format('FUSE ArrayLoadIntTo @%d: Dest=%d Arr=%d Idx=%d',
      [Index, FusedInstr.Dest, FusedInstr.Src1, FusedInstr.Src2]));
  {$ENDIF}

  Inc(FFusedCount);
  Result := True;
end;

function TSuperinstructionOptimizer.TryFuseArrayCopyElement(Index: Integer): Boolean;
var
  LoadInstr, StoreInstr, FusedInstr: TBytecodeInstruction;
  LoadDest: Word;
begin
  { Pattern: ArrayLoadInt Rtmp, ARR[a], idx=Ri + ArrayStoreInt ARR[b], idx=Ri, value=Rtmp
     where idx is the same in both instructions
     Becomes: ArrayCopyElement ARR[b], ARR[a], idx=Ri
     Format: Dest=dest_arr_index, Src1=src_arr_index, Src2=idx_reg }

  Result := False;

  if Index + 1 >= FProgram.GetInstructionCount then Exit;

  LoadInstr := FProgram.GetInstruction(Index);
  StoreInstr := FProgram.GetInstruction(Index + 1);

  // Both must be standard opcodes (< 100)
  if (LoadInstr.OpCode >= bcGroupSuper) or (StoreInstr.OpCode >= bcGroupSuper) then Exit;

  // Check instruction types
  if TBytecodeOp(LoadInstr.OpCode) <> bcArrayLoadInt then Exit;
  if TBytecodeOp(StoreInstr.OpCode) <> bcArrayStoreInt then Exit;

  LoadDest := LoadInstr.Dest;

  // Check if ArrayStoreInt uses the ArrayLoadInt result as value
  // ArrayStoreInt format: Src1=arr_index, Src2=idx_reg, Dest=value_reg
  if StoreInstr.Dest <> LoadDest then
  begin
    {$IFDEF DEBUG_SUPERINSTR}
    if DebugSuperinstr then
      WriteLn(StdErr, Format('  ArrayCopyElement @%d: value reg mismatch Store.Dest=%d Load.Dest=%d',
        [Index, StoreInstr.Dest, LoadDest]));
    {$ENDIF}
    Exit;
  end;

  // Check if both use the same index register
  if StoreInstr.Src2 <> LoadInstr.Src2 then
  begin
    {$IFDEF DEBUG_SUPERINSTR}
    if DebugSuperinstr then
      WriteLn(StdErr, Format('  ArrayCopyElement @%d: idx reg mismatch Store.Src2=%d Load.Src2=%d',
        [Index, StoreInstr.Src2, LoadInstr.Src2]));
    {$ENDIF}
    Exit;
  end;

  // SAFETY: Don't fuse if second instruction is a jump target
  if IsJumpTarget(Index + 1) then
  begin
    {$IFDEF DEBUG_SUPERINSTR}
    if DebugSuperinstr then
      WriteLn(StdErr, Format('  ArrayCopyElement @%d: blocked - instr %d is jump target',
        [Index, Index + 1]));
    {$ENDIF}
    Exit;
  end;

  // Check if load result is temporary
  // NOTE: For ArrayCopyElement, we don't need this check because the
  // pattern is simple: load + store with same value register
  // if not IsTemporaryResult(Index, LoadDest) then Exit;

  // Create fused instruction: arr_dest[idx] = arr_src[idx]
  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := bcArrayCopyElement;
  FusedInstr.Dest := StoreInstr.Src1;       // Destination array index
  FusedInstr.Src1 := LoadInstr.Src1;        // Source array index
  FusedInstr.Src2 := LoadInstr.Src2;        // Index register
  FusedInstr.Immediate := 0;
  // SourceLine now managed via Source Map

  FProgram.SetInstruction(Index, FusedInstr);
  MakeNop(Index + 1);

  {$IFDEF DEBUG_SUPERINSTR}
  if DebugSuperinstr then
    WriteLn(StdErr, Format('FUSE ArrayCopyElement @%d: DestArr=%d SrcArr=%d Idx=%d',
      [Index, FusedInstr.Dest, FusedInstr.Src1, FusedInstr.Src2]));
  {$ENDIF}

  Inc(FFusedCount);
  Result := True;
end;

function TSuperinstructionOptimizer.TryFuseArrayMoveElement(Index: Integer): Boolean;
var
  LoadInstr, StoreInstr, FusedInstr: TBytecodeInstruction;
  LoadDest: Word;
begin
  { Pattern: ArrayLoadInt Rtmp, ARR[a], idx=Ri + ArrayStoreInt ARR[a], idx=Rj, value=Rtmp
     where array is the same but indices are different
     Becomes: ArrayMoveElement ARR[a], src_idx=Ri, dest_idx=Rj
     Format: Dest=arr_index, Src1=src_idx_reg, Src2=dest_idx_reg }

  Result := False;

  if Index + 1 >= FProgram.GetInstructionCount then Exit;

  LoadInstr := FProgram.GetInstruction(Index);
  StoreInstr := FProgram.GetInstruction(Index + 1);

  // Both must be standard opcodes (< 100)
  if (LoadInstr.OpCode >= bcGroupSuper) or (StoreInstr.OpCode >= bcGroupSuper) then Exit;

  // Check instruction types
  if TBytecodeOp(LoadInstr.OpCode) <> bcArrayLoadInt then Exit;
  if TBytecodeOp(StoreInstr.OpCode) <> bcArrayStoreInt then Exit;

  LoadDest := LoadInstr.Dest;

  // Check if ArrayStoreInt uses the ArrayLoadInt result as value
  if StoreInstr.Dest <> LoadDest then Exit;

  // Check if both use the SAME array
  if StoreInstr.Src1 <> LoadInstr.Src1 then Exit;

  // Check that indices are DIFFERENT (otherwise use ArrayCopyElement)
  if StoreInstr.Src2 = LoadInstr.Src2 then Exit;

  // SAFETY: Don't fuse if second instruction is a jump target
  if IsJumpTarget(Index + 1) then Exit;

  // CRITICAL: Check if load result is temporary (only used by the store)
  // Without this check, we might fuse patterns where the loaded value
  // is used elsewhere, causing incorrect behavior
  if not IsTemporaryResult(Index, LoadDest) then Exit;

  // Create fused instruction: arr[dest_idx] = arr[src_idx]
  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := bcArrayMoveElement;
  FusedInstr.Dest := LoadInstr.Src1;        // Array index
  FusedInstr.Src1 := LoadInstr.Src2;        // Source index register
  FusedInstr.Src2 := StoreInstr.Src2;       // Destination index register
  FusedInstr.Immediate := 0;
  // SourceLine now managed via Source Map

  FProgram.SetInstruction(Index, FusedInstr);
  MakeNop(Index + 1);

  {$IFDEF DEBUG_SUPERINSTR}
  if DebugSuperinstr then
    WriteLn(StdErr, Format('FUSE ArrayMoveElement @%d: Arr=%d SrcIdx=%d DestIdx=%d',
      [Index, FusedInstr.Dest, FusedInstr.Src1, FusedInstr.Src2]));
  {$ENDIF}

  Inc(FFusedCount);
  Result := True;
end;

function TSuperinstructionOptimizer.TryFuseArrayReverseRange(Index: Integer): Boolean;
var
  CopyI, SubJ, BranchLt, Jump1, SwapInstr, AddI, SubJSelf, Jump2: TBytecodeInstruction;
  FusedInstr: TBytecodeInstruction;
  StartReg, EndReg, OneReg, ArrayIdx: Word;
  LoopTarget, ExitTarget: Integer;
begin
  { Pattern: Reverse loop (8 instructions)
     0: CopyInt     Ri, R_start    ; i = start
     1: SubInt      Rj, R_end, R1  ; j = end - 1 (or just end)
     2: BranchLtInt Ri, Rj, 4      ; while i < j goto body
     3: Jump        exit           ; exit loop
     4: ArraySwapInt ARR, idx1=Ri, idx2=Rj  ; swap(arr[i], arr[j])
     5: AddIntSelf  Ri, R1         ; i++
     6: SubIntSelf  Rj, R1         ; j--
     7: Jump        2              ; back to condition

     Becomes: ArrayReverseRange ARR, start=Ri, end=Rj
     Format: Src1=arr_index, Src2=start_idx_reg, Dest=end_idx_reg }

  Result := False;

  // Need at least 8 instructions
  if Index + 7 >= FProgram.GetInstructionCount then Exit;

  // Read all 8 instructions
  CopyI := FProgram.GetInstruction(Index);
  SubJ := FProgram.GetInstruction(Index + 1);
  BranchLt := FProgram.GetInstruction(Index + 2);
  Jump1 := FProgram.GetInstruction(Index + 3);
  SwapInstr := FProgram.GetInstruction(Index + 4);
  AddI := FProgram.GetInstruction(Index + 5);
  SubJSelf := FProgram.GetInstruction(Index + 6);
  Jump2 := FProgram.GetInstruction(Index + 7);

  // Check instruction 0: CopyInt Ri, R_start
  if CopyI.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(CopyI.OpCode) <> bcCopyInt then Exit;
  StartReg := CopyI.Dest;  // This is the 'i' register

  // Check instruction 1: SubInt Rj, R_end, R1
  if SubJ.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(SubJ.OpCode) <> bcSubInt then Exit;
  EndReg := SubJ.Dest;     // This is the 'j' register
  OneReg := SubJ.Src2;     // Should be constant 1

  // Check instruction 2: BranchLtInt Ri, Rj, body
  if BranchLt.OpCode <> bcBranchLtInt then Exit;
  if BranchLt.Src1 <> StartReg then Exit;
  if BranchLt.Src2 <> EndReg then Exit;
  LoopTarget := BranchLt.Immediate;  // Should point to SwapInstr (Index + 4)
  if LoopTarget <> Index + 4 then Exit;

  // Check instruction 3: Jump exit
  if Jump1.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(Jump1.OpCode) <> bcJump then Exit;
  ExitTarget := Jump1.Immediate;  // Where to go after loop

  // Check instruction 4: ArraySwapInt ARR, idx1=Ri, idx2=Rj
  if SwapInstr.OpCode <> bcArraySwapInt then Exit;
  if SwapInstr.Src2 <> StartReg then Exit;  // idx1 = i
  if SwapInstr.Dest <> EndReg then Exit;    // idx2 = j
  ArrayIdx := SwapInstr.Src1;               // Array index

  // Check instruction 5: AddIntSelf Ri, R1 (i++)
  if AddI.OpCode <> bcAddIntSelf then Exit;
  if AddI.Dest <> StartReg then Exit;
  if AddI.Src1 <> OneReg then Exit;

  // Check instruction 6: SubIntSelf Rj, R1 (j--)
  if SubJSelf.OpCode <> bcSubIntSelf then Exit;
  if SubJSelf.Dest <> EndReg then Exit;
  if SubJSelf.Src1 <> OneReg then Exit;

  // Check instruction 7: Jump back to BranchLt
  if Jump2.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(Jump2.OpCode) <> bcJump then Exit;
  if Jump2.Immediate <> Index + 2 then Exit;  // Jump to BranchLt

  // SAFETY: Check no jumps into the middle of this pattern
  if IsJumpTarget(Index + 1) then Exit;
  if IsJumpTarget(Index + 3) then Exit;
  if IsJumpTarget(Index + 5) then Exit;
  if IsJumpTarget(Index + 6) then Exit;
  if IsJumpTarget(Index + 7) then Exit;

  // Create fused instruction: ArrayReverseRange ARR, start, end
  // After reverse, jump to exit target
  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := bcArrayReverseRange;
  FusedInstr.Src1 := ArrayIdx;              // Array index
  FusedInstr.Src2 := CopyI.Src1;            // Start value register (original start)
  FusedInstr.Dest := SubJ.Src1;             // End value register (original end, before -1)
  FusedInstr.Immediate := ExitTarget;       // Where to jump after (encoded for clarity)
  // SourceLine now managed via Source Map

  // Replace first instruction with fused, NOP the rest
  FProgram.SetInstruction(Index, FusedInstr);
  MakeNop(Index + 1);
  MakeNop(Index + 2);
  MakeNop(Index + 3);
  MakeNop(Index + 4);
  MakeNop(Index + 5);
  MakeNop(Index + 6);
  MakeNop(Index + 7);

  {$IFDEF DEBUG_SUPERINSTR}
  if DebugSuperinstr then
    WriteLn(StdErr, Format('FUSE ArrayReverseRange @%d: Arr=%d Start=%d End=%d Exit=%d',
      [Index, ArrayIdx, FusedInstr.Src2, FusedInstr.Dest, ExitTarget]));
  {$ENDIF}

  Inc(FFusedCount, 7);  // Saved 7 instructions
  Result := True;
end;

function TSuperinstructionOptimizer.TryFuseArrayShiftLeft(Index: Integer): Boolean;
var
  LoadFirst, CopyJ, BranchLe, ExitAddEnd, ExitStore, ExitJump: TBytecodeInstruction;
  BodyAddIdx, BodyMove, BodyAddJ, BodyJump: TBytecodeInstruction;
  FusedInstr: TBytecodeInstruction;
  StartReg, JReg, OneReg, ArrayIdx, EndReg, FirstReg: Word;
  ExitTarget: Integer;
begin
  { Pattern: Rotate left loop (10 instructions)
     ArrayLoadIntTo    Rfirst, ARR, idx=Rstart    ; first = arr[start]
     CopyInt           Rj, Rstart                 ; j = start
     BranchLeInt       Rj, Rend, body             ; while j <= end goto body
     AddInt            Rtemp, Rend, R1            ; temp = end + 1
     ArrayStoreInt     ARR, idx=Rtemp, value=Rfirst  ; arr[end+1] = first
     Jump              exit                       ; exit loop
     AddInt            Ridx, Rj, R1               ; idx = j + 1
     ArrayMoveElement  ARR[Rj] = ARR[Ridx]        ; arr[j] = arr[j+1]
     AddIntSelf        Rj, R1                     ; j++
     Jump              branch                     ; loop back

     Becomes: ArrayShiftLeft ARR, start=Rstart, end=Rend
     Format: Src1=arr_index, Src2=start_idx_reg, Dest=end_idx_reg }

  Result := False;

  // Need at least 10 instructions
  if Index + 9 >= FProgram.GetInstructionCount then Exit;

  // Read all instructions
  LoadFirst := FProgram.GetInstruction(Index);
  CopyJ := FProgram.GetInstruction(Index + 1);
  BranchLe := FProgram.GetInstruction(Index + 2);
  ExitAddEnd := FProgram.GetInstruction(Index + 3);
  ExitStore := FProgram.GetInstruction(Index + 4);
  ExitJump := FProgram.GetInstruction(Index + 5);
  BodyAddIdx := FProgram.GetInstruction(Index + 6);
  BodyMove := FProgram.GetInstruction(Index + 7);
  BodyAddJ := FProgram.GetInstruction(Index + 8);
  BodyJump := FProgram.GetInstruction(Index + 9);

  // Check instruction 0: ArrayLoadIntTo
  if LoadFirst.OpCode <> bcArrayLoadIntTo then Exit;
  ArrayIdx := LoadFirst.Src1;
  StartReg := LoadFirst.Src2;
  FirstReg := LoadFirst.Dest;

  // Check instruction 1: CopyInt
  if CopyJ.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(CopyJ.OpCode) <> bcCopyInt then Exit;
  if CopyJ.Src1 <> StartReg then Exit;
  JReg := CopyJ.Dest;

  // Check instruction 2: BranchLeInt
  if BranchLe.OpCode <> bcBranchLeInt then Exit;
  if BranchLe.Src1 <> JReg then Exit;
  EndReg := BranchLe.Src2;
  if BranchLe.Immediate <> Index + 6 then Exit;

  // Check instruction 3: AddInt
  if ExitAddEnd.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(ExitAddEnd.OpCode) <> bcAddInt then Exit;
  if ExitAddEnd.Src1 <> EndReg then Exit;
  OneReg := ExitAddEnd.Src2;

  // Check instruction 4: ArrayStoreInt
  if ExitStore.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(ExitStore.OpCode) <> bcArrayStoreInt then Exit;
  if ExitStore.Src1 <> ArrayIdx then Exit;
  if ExitStore.Src2 <> ExitAddEnd.Dest then Exit;
  if ExitStore.Dest <> FirstReg then Exit;

  // Check instruction 5: Jump
  if ExitJump.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(ExitJump.OpCode) <> bcJump then Exit;
  ExitTarget := ExitJump.Immediate;

  // Check instruction 6: AddInt
  if BodyAddIdx.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(BodyAddIdx.OpCode) <> bcAddInt then Exit;
  if BodyAddIdx.Src1 <> JReg then Exit;
  if BodyAddIdx.Src2 <> OneReg then Exit;

  // Check instruction 7: ArrayMoveElement (Dest=ArrIdx, Src1=SrcIdx, Src2=DestIdx)
  if BodyMove.OpCode <> bcArrayMoveElement then Exit;
  if BodyMove.Dest <> ArrayIdx then Exit;
  if BodyMove.Src2 <> JReg then Exit;
  if BodyMove.Src1 <> BodyAddIdx.Dest then Exit;

  // Check instruction 8: AddIntSelf
  if BodyAddJ.OpCode <> bcAddIntSelf then Exit;
  if BodyAddJ.Dest <> JReg then Exit;
  if BodyAddJ.Src1 <> OneReg then Exit;

  // Check instruction 9: Jump back
  if BodyJump.OpCode >= bcGroupSuper then Exit;
  if TBytecodeOp(BodyJump.OpCode) <> bcJump then Exit;
  if BodyJump.Immediate <> Index + 2 then Exit;

  // SAFETY checks
  if IsJumpTarget(Index + 1) then Exit;
  if IsJumpTarget(Index + 3) then Exit;
  if IsJumpTarget(Index + 4) then Exit;
  if IsJumpTarget(Index + 5) then Exit;
  if IsJumpTarget(Index + 7) then Exit;
  if IsJumpTarget(Index + 8) then Exit;
  if IsJumpTarget(Index + 9) then Exit;

  // Create fused instruction
  FillChar(FusedInstr, SizeOf(FusedInstr), 0);
  FusedInstr.OpCode := bcArrayShiftLeft;
  FusedInstr.Src1 := ArrayIdx;
  FusedInstr.Src2 := StartReg;
  FusedInstr.Dest := EndReg;
  FusedInstr.Immediate := ExitTarget;
  // SourceLine now managed via Source Map

  FProgram.SetInstruction(Index, FusedInstr);
  MakeNop(Index + 1);
  MakeNop(Index + 2);
  MakeNop(Index + 3);
  MakeNop(Index + 4);
  MakeNop(Index + 5);
  MakeNop(Index + 6);
  MakeNop(Index + 7);
  MakeNop(Index + 8);
  MakeNop(Index + 9);

  Inc(FFusedCount, 9);
  Result := True;
end;





function TSuperinstructionOptimizer.KindOn(Kind: Integer): Boolean;
// SUPERMASK bisection - see Run. Empty mask = every kind on; the token `all` says the same thing
// explicitly, so that SUPERMASK=all,9 means "the shipping set PLUS the parked kind 9" - which is the
// only arrangement that A/Bs a parked kind against what actually ships. Naming kinds one by one
// cannot express it, and measuring kind 9 ALONE measures a program with no other fusion in it.
begin
  Result := (FKindMask = '') or (Pos(',all,', ',' + FKindMask + ',') > 0) or
            (Pos(',' + IntToStr(Kind) + ',', ',' + FKindMask + ',') > 0);
end;

function TSuperinstructionOptimizer.KindOptIn(Kind: Integer): Boolean;
// The gate for a kind that is OFF by default: an empty mask leaves it off, and only naming it
// explicitly turns it on. The kinds parked with a measured reason (9, 11) used to be written
// `False and TryFuseXxx`, which made the SUPERMASK=<n> the comment promised a no-op - the switch
// documented for hunting the remaining defect could not reach the code it was meant to reach.
// `all` deliberately does NOT reach these: a kind parked with a measured reason is turned on by
// name, never by a wildcard.
begin
  Result := (FKindMask <> '') and (Pos(',' + IntToStr(Kind) + ',', ',' + FKindMask + ',') > 0);
end;

procedure TSuperinstructionOptimizer.SetAotOwnedPCs(const Mask: array of Boolean);
var i: Integer;
begin
  SetLength(FAotOwns, Length(Mask));
  for i := 0 to High(Mask) do FAotOwns[i] := Mask[i];
end;

function TSuperinstructionOptimizer.MayFuseAt(Index: Integer): Boolean;
// ⛔ A fusion consumes TWO instructions, so BOTH have to be outside the AOT's regions: fusing across
// the boundary would put a superinstruction at the last PC of a declined region and NOP the first
// of a compiled one, which is precisely the stream the AOT's emitter then refuses.
begin
  if Length(FAotOwns) = 0 then Exit(True);          // no compiler will run: the gate above decided
  if (Index < 0) or (Index + 1 > High(FAotOwns)) then Exit(False);
  Result := (not FAotOwns[Index]) and (not FAotOwns[Index + 1]);
end;

function TSuperinstructionOptimizer.Run: Integer;
var
  i, Pass: Integer;
  Changed: Boolean;
begin
  FFusedCount := 0;

  {$IFDEF DISABLE_SUPERINSTRUCTIONS}
  Result := 0;
  Exit;
  {$ENDIF}
  // ⚡ SUPERINSTR: runtime A/B while the compile-time switch is being re-decided. '0' forces the pass
  // off on a binary built with it ON, so the two arrangements are one binary and one switch.
  if GetEnvironmentVariable('SUPERINSTR') = '0' then begin Result := 0; Exit; end;
  // ⛔⛔⛔ ONLY WHEN THE INTERPRETER IS THE ONLY ENGINE, AND THE TEST LIVES HERE BECAUSE THERE ARE
  // FOUR CALLERS - SedaiRunner, SedaiBasicVM.lpr, SedaiBasicCompiler.lpr and SedaiWebServer. Gating
  // one of them (the runner) left `sb` itself, which calls the second, still fusing under --jit.
  //
  // The pass rewrites BYTECODE, which is exactly what the JIT consumes; the JIT declines any hot
  // loop holding an opcode it cannot lower, and every superinstruction is one. Measured 18 Aug:
  // fannkuch --jit went from 178 ms to 4378, a factor of 24 - not slower, SWITCHED OFF. The AOT
  // loses through the regions it declines and runs interpreted. Fusion and the compilers compete for
  // the same representation, so the pass runs only when no compiler will.
  // ⚡ SUPERINSTR_AOT=1 lifts the gate FOR THE AOT ONLY, and it is the A/B on one binary for the
  // question this gate never separated: the JIT CONSUMES bytecode, so a superinstruction really can
  // switch a hot loop off (fannkuch x24, above) - but line 273 of this same file records that the
  // AOT compiles from SSA, NOT from bytecode. If that holds, fusion cannot cost the AOT a region; it
  // can only pay it back in the regions the AOT DECLINES and leaves interpreted.
  // 📊 Measured 2 Sep 2026 with AOTC_DIAG and the pair census: on a program whose MAIN bails
  // (recfield_fb, BAIL ssaLoadProcAddr) --aot executed 15 541 126 instructions against the
  // interpreter's 10 340 999 - +50%, and the whole difference is AddIntToBranchLe unfusing back into
  // AddInt/CopyInt/Jump + CmpLeInt/JumpIfZero. That made --aot 0.83x the INTERPRETER, which the
  // standing rule calls a defect.
  // ⛔⛔ THE JIT AND THE AOT ARE NOT THE SAME QUESTION, and treating them as one cost eight programs.
  // The JIT declines a hot loop holding an opcode it cannot lower, and it picks its loops at RUN
  // time, so nothing static can say where it will go: it stays a whole-program refusal (fannkuch
  // --jit went 178 -> 4378 ms with fusion on, a factor of 24 - not slower, SWITCHED OFF).
  // The AOT's regions ARE known statically, so it gets a mask instead of a veto: fusion is refused
  // inside a region the AOT will compile and allowed in one it declines - which runs interpreted,
  // and had been running UNFUSED bytecode for nothing. 📊 2 Sep 2026, 77 programs, --aot against the
  // interpreter: eight were slower COMPILED, led by render_probe 0.47x and recfield_fb 0.80x.
  // ⛔ SUPERINSTR_AOT=1 forces fusion everywhere even under the AOT - the A/B that PROVED the mask
  // is needed rather than a plain lift: lifting it outright reads apmap_ceiling 944 -> 1691 ms and
  // pairs_probe 1120 -> 2564, because the AOT's emitter then refuses regions its own survey accepts.
  if GJitWillRun then begin Result := 0; Exit; end;
  if GAotWillRun and (Length(FAotOwns) = 0) and (GetEnvironmentVariable('SUPERINSTR_AOT') <> '1') then
    begin Result := 0; Exit; end;
  if GetEnvironmentVariable('SUPERINSTR_AOT') = '1' then SetLength(FAotOwns, 0);
  // ⚡ SUPERMASK: bisect WHICH fusion is wrong. Every TryFuseXxx call is guarded by KindOn(n), and
  // SUPERMASK is a comma-separated list of the kinds to ENABLE (empty = all). The family was off for
  // a month, so it has rotted: with all of it on, run_regress gives 16 FAIL and 16 OPTDIFF. This is
  // how the broken one gets named instead of guessed.
  FKindMask := GetEnvironmentVariable('SUPERMASK');

  Pass := 0;

  // Multiple passes until no more fusions possible
  repeat
    Inc(Pass);
    Changed := False;
    i := 0;
    while i < FProgram.GetInstructionCount - 1 do
    begin
      // Skip NOPs
      if TBytecodeOp(FProgram.GetInstruction(i).OpCode) = bcNop then
      begin
        Inc(i);
        Continue;
      end;

      // ⛔ ONE test for every fusion below, on purpose: a per-pattern gate is a parallel list, and a
      // parallel list has no moment at which it is complete. Here the AOT owns this stretch and the
      // pass keeps out; everywhere else it fuses exactly as it does with no compiler at all.
      if not MayFuseAt(i) then
      begin
        Inc(i);
        Continue;
      end;

      // Try TryFuseLoopIncrementAndBranch FIRST for AddIntTo or AddInt
      // This allows fusing (AddIntTo or AddInt) + Jump into AddIntToBranchLe
      if (FProgram.GetInstruction(i).OpCode = bcAddIntTo) or
         (FProgram.GetInstruction(i).OpCode = bcAddInt) then
      begin
        if KindOn(1) and TryFuseLoopIncrementAndBranch(i) then
        begin
          Changed := True;
          Inc(i);
          Continue;  // Successfully fused, move to next instruction
        end;
        // If fusion failed, fall through to try other patterns
      end;

      // Try TryFuseArrayShiftLeft for ArrayLoadIntTo (opcode 253)
      // This pattern starts with a superinstruction, so check it before skipping
      if FProgram.GetInstruction(i).OpCode = bcArrayLoadIntTo then
      begin
        if KindOn(2) and TryFuseArrayShiftLeft(i) then
          Changed := True;
        Inc(i);
        Continue;
      end;

      // "acc += tab[Asc(MID$(s,i,1))+1]": anchored on the ASC, which is a plain string-group opcode,
      // but the third instruction of the pattern is a superinstruction - so this has to be tried
      // before the blanket skip below.
      if FProgram.GetInstruction(i).OpCode = bcStrAscMid then
      begin
        if KindOn(3) and TryFuseAppendMapped(i) then
        begin
          Changed := True;
          Inc(i);
          Continue;
        end;
      end;

      // Skip other superinstructions (can't fuse already-fused instructions)
      if FProgram.GetInstruction(i).OpCode >= bcGroupSuper then
      begin
        Inc(i);
        Continue;
      end;

      // Try fusion patterns in order of priority
      // (8-instruction patterns first, then 5, then 3, then 2)

      // NEW: Array reverse range (8 instructions) - HIGHEST IMPACT for fannkuch-redux
      if KindOn(4) and TryFuseArrayReverseRange(i) then
        Changed := True
      // NEW: Array shift left (9 instructions) - HIGH IMPACT for fannkuch-redux
      else if KindOn(2) and TryFuseArrayShiftLeft(i) then
        Changed := True
      // NEW: Array swap pattern (5 instructions) - HIGH PRIORITY for fannkuch-redux
      else if KindOn(5) and TryFuseArraySwapInt(i) then
        Changed := True
      else if KindOn(6) and TryFuseCompareZeroAndBranch(i) then
        Changed := True
      else if KindOn(7) and TryFuseCompareAndBranch(i) then
        Changed := True
      else if KindOn(8) and TryFuseArithAndCopy(i) then
        Changed := True
      // ⛔⛔ KIND 9 IS OFF, AND IT IS THE THIRD DEFECT IN IT IN ONE DAY. TryFuseConstantArithmetic
      // folds "LoadConst + arith" into arith-with-immediate and deletes the load, which needs to know
      // that nothing else reads the constant's register. Two ways of getting that wrong were found and
      // fixed on 18 Aug - "end of basic block" read as "dead", and a register carried in Immediate
      // that the scan could not see - and a third survives both: test_division_bug raises a spurious
      // Division by zero under the interpreter where every other engine is right (SUPERMASK named it
      // on the first pass). A fusion that has produced three independent miscompiles in a day does not
      // get to stay on while the fourth is looked for. SUPERMASK=9 re-enables it for the hunt.
      else if KindOptIn(9) and TryFuseConstantArithmetic(i) then
        Changed := True
      {$IFNDEF DISABLE_ARRAYSTORECONST}
      else if KindOn(10) and TryFuseArrayStoreConst(i) then
        Changed := True
      {$ENDIF}
      // ⛔ KIND 11 IS OFF, and the disassembly is why: it is a ONE-FOR-ONE substitution, not a fusion.
      // "AddInt R105, R105, R112" becomes "AddIntSelf Dest=105 Src1=112" - same instruction count,
      // nothing saved, and the only thing that changes is which dispatch arm runs. It cannot win by
      // construction, and measured alone on binary-trees it loses 28.5% - the entire regression that
      // benchmark showed. Every other kind collapses N instructions into one; this one does not.
      else if KindOptIn(11) and TryFuseAddIntSelf(i) then
        Changed := True
      else if KindOn(12) and TryFuseArrayCopyElement(i) then
        Changed := True
      else if KindOn(13) and TryFuseArrayMoveElement(i) then
        Changed := True
      else if TryFuseArrayLoadIntTo(i) then
        Changed := True
      // ⛔ KIND 16 WAS DECLARED, DEFINED AND NEVER CALLED. TryFuseSquareSumFloat existed in full,
      // with both its patterns, and no line in this driver reached it - so bcAddSquareFloat and
      // bcSquareSumFloat could not be produced by anything, while the C hot loop carried an arm for
      // each. Found 24 Aug 2026 by asking which of the 100 arms in hotdisp.c anything ever emits.
      //
      // ⭐ BEFORE KIND 14, and that is not a preference. TryFuseMulAddFloat matches the general
      // "MulFloat + AddFloat"; a square is that pattern with Src1 = Src2, so whichever runs first
      // takes it. Placed after, this one would keep on never firing.
      else if KindOn(16) and TryFuseSquareSumFloat(i) then
        Changed := True
      else if KindOn(14) and TryFuseMulAddFloat(i) then
        Changed := True
      else if KindOn(15) and TryFuseArrayLoadAddFloat(i) then
        Changed := True
      else if KindOn(17) and TryFuseMulMulFloat(i) then
        Changed := True
      else if KindOn(18) and TryFuseAddSqrtFloat(i) then
        Changed := True
      else if KindOn(19) and TryFuseArrayLoadBranchInt(i) then
        Changed := True
      ;

      Inc(i);
    end;

    // Compact NOPs after each pass to enable multi-instruction pattern matching
    if Changed then
      RunNopCompaction(FProgram);

  until (not Changed) or (Pass > 10);  // Safety limit

  Result := FFusedCount;
end;

function RunSuperinstructions(AProgram: TBytecodeProgram): Integer;
var
  Optimizer: TSuperinstructionOptimizer;
begin
  Optimizer := TSuperinstructionOptimizer.Create(AProgram);
  try
    Result := Optimizer.Run;
  finally
    Optimizer.Free;
  end;
end;

function FuseAtLoad(AProgram: TBytecodeProgram): Integer;
var
  SavedAot: Boolean;
begin
  Result := 0;
  if AProgram = nil then Exit;
  SavedAot := GAotWillRun;
  GAotWillRun := False;              // see the header: no AOT can consume a .basc
  try
    Result := RunSuperinstructions(AProgram);
    // ⚠️ AND THE COMPACTION IS NOT OPTIONAL. A fusion leaves a NOP where the second opcode was, so
    // without this the interpreter performs exactly as many dispatches as before and the pass buys
    // NOTHING. It is the same note the pipeline carries where these two run from source.
    if Result > 0 then RunNopCompaction(AProgram);
  finally
    GAotWillRun := SavedAot;
  end;
end;

end.
