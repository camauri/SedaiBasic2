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
unit SedaiPeephole;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$I OptimizationFlags.inc}
{$I DebugFlags.inc}

{ Peephole optimization pass

  Performs local bytecode optimizations on small windows of instructions.
  Runs AFTER bytecode compilation and BEFORE superinstruction fusion.

  Optimizations performed:

  1. Redundant Copy Elimination
     Pattern: CopyInt R0, R0 (or CopyFloat/CopyString)
     Action: Replace with NOP
     Benefit: Eliminates useless self-copy instructions

  2. Jump Chain Optimization
     Pattern: Jump L1; L1: Jump L2
     Action: Jump L2 (skip intermediate jump)
     Benefit: Reduces jump chain traversal

  3. Dead Jump Elimination
     Pattern: Jump L1; L1: (next instruction)
     Action: Replace with NOP (jump to next instruction is useless)
     Benefit: Eliminates redundant jumps

  4. NOP Compaction (optional, after other passes)
     Pattern: Multiple consecutive NOPs
     Action: Keep only one (or remove all if not jump targets)
     Benefit: Reduces code size

  5. Conditional Jump Simplification
     Pattern: JumpIfZero to next instruction
     Action: Replace with NOP
     Benefit: Eliminates useless conditional jumps

  NOTE: This pass does NOT remove NOPs - that would invalidate jump targets.
  NOP removal is done separately if needed, with jump target adjustment.
}

interface

uses
  Classes, SysUtils, SedaiBytecodeTypes, SedaiOpcodeBanks;

type
  TPeepholeOptimizer = class
  private
    FProgram: TBytecodeProgram;
    FOptimizedCount: Integer;
    FJumpTargets: array of Boolean;  // Track which instruction indices are jump targets
    // How many instructions READ each string register, counted once per pass. Lets the temp-copy
    // fusion below ask "is this temporary read anywhere else?" in O(1) instead of rescanning the
    // program per candidate, which would be quadratic. Only ever an OVER-count as the pass nops
    // instructions out, and an over-count just declines the rewrite - never permits a wrong one.
    FStrReadCount: array of Integer;
    FFuseStringTemps: Boolean;       // gate: STRFUSE=0 restores the temp + copy pair (A/B on one binary)

    { Build jump target map for the program }
    procedure BuildJumpTargetMap;

    { Count, per string register, how many instructions read it }
    procedure BuildStringReadCounts;

    { Check if an instruction index is a jump target }
    function IsJumpTarget(Index: Integer): Boolean;

    { Individual optimization passes }
    function OptimizeRedundantCopy(Index: Integer): Boolean;
    function OptimizeJumpChain(Index: Integer): Boolean;
    function OptimizeDeadJump(Index: Integer): Boolean;
    function OptimizeDeadConditionalJump(Index: Integer): Boolean;
    function OptimizeStringTempCopy(Index: Integer): Boolean;

    { Make instruction into NOP }
    procedure MakeNop(Index: Integer);

  public
    constructor Create(AProgram: TBytecodeProgram);
    destructor Destroy; override;

    { Run all peephole optimizations - returns count of optimizations applied }
    function Run: Integer;
  end;

function RunPeephole(AProgram: TBytecodeProgram): Integer;

implementation

{$IFDEF DEBUG_PEEPHOLE}
uses SedaiDebug;
{$ENDIF}

{ TPeepholeOptimizer }

constructor TPeepholeOptimizer.Create(AProgram: TBytecodeProgram);
begin
  inherited Create;
  FProgram := AProgram;
  FOptimizedCount := 0;
  // ⛔ OFF BY DEFAULT, and it must stay off until the fusion moves to the SSA level.
  // Rewriting BYTECODE here desynchronises it from the SSA the AOT compiles from: the AOT then
  // installs native code generated from the UNFUSED SSA over the PC ranges of the FUSED bytecode.
  // The result is a silent miscompile -- "Str(123)" came out as the empty string under --aot while
  // the interpreter printed it correctly. run_regress cannot see this (it never runs --aot); only
  // aot_validate can, and it found 8 programs.
  // STRFUSE=1 opts in, for measuring the interpreter-side win (which is large and real).
  FFuseStringTemps := GetEnvironmentVariable('STRFUSE') = '1';
end;

destructor TPeepholeOptimizer.Destroy;
begin
  inherited;
end;

procedure TPeepholeOptimizer.BuildJumpTargetMap;
var
  i: Integer;
  Instr: TBytecodeInstruction;
  Target: Integer;
begin
  SetLength(FJumpTargets, FProgram.GetInstructionCount);

  // Initialize all to false
  for i := 0 to High(FJumpTargets) do
    FJumpTargets[i] := False;

  // Scan all instructions and mark jump targets
  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    Instr := FProgram.GetInstruction(i);

    case TBytecodeOp(Instr.OpCode) of
      bcJump, bcJumpIfZero, bcJumpIfNotZero, bcCall, bcCallSub,
      bcLoadProcAddr:   // M5.2: Immediate is a SUB entry PC (a worker enters there) → a block boundary
      begin
        Target := Instr.Immediate;
        if (Target >= 0) and (Target < FProgram.GetInstructionCount) then
          FJumpTargets[Target] := True;
      end;
    end;

    // Also check for superinstruction branch opcodes
    if Instr.OpCode >= bcGroupSuper then
    begin
      // Superinstruction branches use Immediate for target
      Target := Instr.Immediate;
      if (Target >= 0) and (Target < FProgram.GetInstructionCount) then
        FJumpTargets[Target] := True;
    end;
  end;
end;

function TPeepholeOptimizer.IsJumpTarget(Index: Integer): Boolean;
begin
  if (Index >= 0) and (Index < Length(FJumpTargets)) then
    Result := FJumpTargets[Index]
  else
    Result := False;
end;

procedure TPeepholeOptimizer.MakeNop(Index: Integer);
var
  NopInstr: TBytecodeInstruction;
begin
  FillChar(NopInstr, SizeOf(NopInstr), 0);
  NopInstr.OpCode := bcNop;
  FProgram.SetInstruction(Index, NopInstr);
end;

function TPeepholeOptimizer.OptimizeRedundantCopy(Index: Integer): Boolean;
var
  Instr: TBytecodeInstruction;
begin
  Result := False;

  if Index >= FProgram.GetInstructionCount then Exit;

  Instr := FProgram.GetInstruction(Index);

  // Check for self-copy: CopyXxx Rn, Rn
  case TBytecodeOp(Instr.OpCode) of
    bcCopyInt, bcCopyFloat, bcCopyString:
    begin
      if Instr.Dest = Instr.Src1 then
      begin
        {$IFDEF DEBUG_PEEPHOLE}
        if DebugPeephole then
          WriteLn('[Peephole] Removing redundant self-copy at ', Index,
                  ': Copy R', Instr.Dest, ', R', Instr.Src1);
        {$ENDIF}
        MakeNop(Index);
        Result := True;
      end;
    end;
  end;
end;

procedure TPeepholeOptimizer.BuildStringReadCounts;
var
  i, r, hi: Integer;
  Instr: TBytecodeInstruction;
begin
  // ⚠️ Every field here can legitimately be NEGATIVE: an ABSENT operand lowers to -1 (the same
  // convention RESUME and the clear form of KEY use, see absent-operand-lowers-to-r0). Indexing with
  // one raises a range error in a debug build -- and RunPeephole is called inside "try ... except
  // end", so that would silently switch the WHOLE peephole off rather than report anything. In a
  // release build, with range checks off, it would write outside the array instead. Hence >= 0
  // everywhere, not just <= hi.
  hi := 0;
  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    Instr := FProgram.GetInstruction(i);
    if Instr.Dest > hi then hi := Instr.Dest;
    if Instr.Src1 > hi then hi := Instr.Src1;
    if Instr.Src2 > hi then hi := Instr.Src2;
    if (Instr.Immediate > hi) and (Instr.Immediate < 65536) then hi := Instr.Immediate;
  end;
  SetLength(FStrReadCount, hi + 2);
  for r := 0 to High(FStrReadCount) do FStrReadCount[r] := 0;
  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    Instr := FProgram.GetInstruction(i);
    if Src1IsStringReg(TBytecodeOp(Instr.OpCode)) and (Instr.Src1 >= 0) and (Instr.Src1 <= hi) then
      Inc(FStrReadCount[Instr.Src1]);
    if Src2IsStringReg(TBytecodeOp(Instr.OpCode)) and (Instr.Src2 >= 0) and (Instr.Src2 <= hi) then
      Inc(FStrReadCount[Instr.Src2]);
    if ImmediateIsStringReg(TBytecodeOp(Instr.OpCode)) and (Instr.Immediate >= 0) and (Instr.Immediate <= hi) then
      Inc(FStrReadCount[Instr.Immediate]);
    if DestReadIsStringReg(TBytecodeOp(Instr.OpCode)) and (Instr.Dest >= 0) and (Instr.Dest <= hi) then
      Inc(FStrReadCount[Instr.Dest]);
  end;
end;

function TPeepholeOptimizer.OptimizeStringTempCopy(Index: Integer): Boolean;
// Fuse "<string op> T, ..." + "CopyString D, T" into "<string op> D, ...".
//
// Every string primitive writes a fresh TEMPORARY which the very next instruction copies into the
// variable, so "s = s + x" compiles to
//     StrConcat   T, s, x
//     CopyString  s, T
// That costs a dispatch per operation, but the real damage is that the buffer is then SHARED between
// T and s, so its reference count is never 1 and neither the destination-buffer reuse in AssignSubstr
// nor an in-place append can ever engage. Removing the copy is what unblocks both: see
// string-runtime-is-allocation-bound. With the copy gone, "s = s + x" lowers to "StrConcat s, s, x",
// which the VM can then satisfy by appending to s rather than building a new string every time -- the
// difference between linear and quadratic.
//
// Safe only when the temporary is genuinely dead afterwards, so:
//   - the copy must not be a jump target (control must not be able to reach it without the producer);
//   - the producer must WRITE Dest as a string and not also read it back (bcArrayStoreString carries
//     its VALUE in Dest, so its Dest is not a pure output);
//   - superinstructions are excluded: they fold several effects into one opcode;
//   - and T must be read by NOTHING except this copy. That last test is why the bank classification
//     had to move to SedaiOpcodeBanks first: asking "does this instruction read string register T"
//     with a private, second copy of the opcode tables is exactly how copyprop-soundness-bug got in.
var
  Prod, Copy: TBytecodeInstruction;
  T, D, Reads: Integer;
begin
  Result := False;
  if Index + 1 >= FProgram.GetInstructionCount then Exit;
  if IsJumpTarget(Index + 1) then Exit;

  Copy := FProgram.GetInstruction(Index + 1);
  if TBytecodeOp(Copy.OpCode) <> bcCopyString then Exit;

  Prod := FProgram.GetInstruction(Index);
  if Prod.OpCode >= bcGroupSuper then Exit;
  if not DestIsStringReg(TBytecodeOp(Prod.OpCode)) then Exit;
  if DestReadIsStringReg(TBytecodeOp(Prod.OpCode)) then Exit;

  T := Prod.Dest;
  D := Copy.Dest;
  if Copy.Src1 <> T then Exit;
  if D = T then Exit;                       // self-copy: OptimizeRedundantCopy's business
  if (T < 0) or (D < 0) then Exit;          // an absent operand is -1, never a register
  if T > High(FStrReadCount) then Exit;

  // The copy reads T once. The producer may read it too ("StrConcat T, T, x"), and that stays correct
  // because after the rewrite the producer reads T and writes D. Any OTHER reader would observe T
  // keeping its old value instead of the result, so refuse.
  Reads := FStrReadCount[T] - 1;
  if ReadsStringReg(Prod, T) then Dec(Reads);
  if Reads <> 0 then Exit;

  // T now has no bytecode use left, but the SSA the AOT compiles from still names it -- and the AOT
  // translates SSA registers through the map the register COMPACTOR builds from BYTECODE. Without
  // this, the compactor drops T (-> -1) and the whole region bails with "unmapped-str", which cost
  // pidigits its native MAIN and doubled its --aot time. Measured, not guessed.
  FProgram.ReserveStringReg(T);
  Prod.Dest := D;
  FProgram.SetInstruction(Index, Prod);
  MakeNop(Index + 1);
  {$IFDEF DEBUG_PEEPHOLE}
  if DebugPeephole then
    WriteLn('[Peephole] fused string temp at ', Index, ': write R', T, ' -> R', D,
            ', dropped the copy');
  {$ENDIF}
  Result := True;
end;

function TPeepholeOptimizer.OptimizeJumpChain(Index: Integer): Boolean;
var
  Instr, TargetInstr: TBytecodeInstruction;
  Target, FinalTarget: Integer;
  ChainLength: Integer;
begin
  Result := False;

  if Index >= FProgram.GetInstructionCount then Exit;

  Instr := FProgram.GetInstruction(Index);

  // Only optimize unconditional jumps for now
  if TBytecodeOp(Instr.OpCode) <> bcJump then Exit;

  Target := Instr.Immediate;
  if (Target < 0) or (Target >= FProgram.GetInstructionCount) then Exit;

  TargetInstr := FProgram.GetInstruction(Target);

  // If target is also a jump, follow the chain
  if TBytecodeOp(TargetInstr.OpCode) = bcJump then
  begin
    FinalTarget := Target;
    ChainLength := 0;

    // Follow jump chain (with limit to avoid infinite loops)
    while (ChainLength < 10) and
          (FinalTarget >= 0) and
          (FinalTarget < FProgram.GetInstructionCount) do
    begin
      TargetInstr := FProgram.GetInstruction(FinalTarget);
      if TBytecodeOp(TargetInstr.OpCode) <> bcJump then
        Break;

      FinalTarget := TargetInstr.Immediate;
      Inc(ChainLength);
    end;

    // If we found a shorter path, update the jump
    if (FinalTarget <> Target) and
       (FinalTarget >= 0) and
       (FinalTarget < FProgram.GetInstructionCount) then
    begin
      {$IFDEF DEBUG_PEEPHOLE}
      if DebugPeephole then
        WriteLn('[Peephole] Shortening jump chain at ', Index,
                ': Jump ', Target, ' → Jump ', FinalTarget,
                ' (skipped ', ChainLength, ' intermediate jumps)');
      {$ENDIF}
      Instr.Immediate := FinalTarget;
      FProgram.SetInstruction(Index, Instr);
      Result := True;
    end;
  end;
end;

function TPeepholeOptimizer.OptimizeDeadJump(Index: Integer): Boolean;
var
  Instr: TBytecodeInstruction;
begin
  Result := False;

  if Index >= FProgram.GetInstructionCount - 1 then Exit;  // Need at least one more instruction

  Instr := FProgram.GetInstruction(Index);

  // Check for jump to next instruction
  if TBytecodeOp(Instr.OpCode) = bcJump then
  begin
    if Instr.Immediate = Index + 1 then
    begin
      {$IFDEF DEBUG_PEEPHOLE}
      if DebugPeephole then
        WriteLn('[Peephole] Removing dead jump to next instruction at ', Index);
      {$ENDIF}
      MakeNop(Index);
      Result := True;
    end;
  end;
end;

function TPeepholeOptimizer.OptimizeDeadConditionalJump(Index: Integer): Boolean;
var
  Instr: TBytecodeInstruction;
begin
  Result := False;

  if Index >= FProgram.GetInstructionCount - 1 then Exit;

  Instr := FProgram.GetInstruction(Index);

  // Check for conditional jump to next instruction
  case TBytecodeOp(Instr.OpCode) of
    bcJumpIfZero, bcJumpIfNotZero:
    begin
      if Instr.Immediate = Index + 1 then
      begin
        {$IFDEF DEBUG_PEEPHOLE}
        if DebugPeephole then
          WriteLn('[Peephole] Removing dead conditional jump to next instruction at ', Index);
        {$ENDIF}
        MakeNop(Index);
        Result := True;
      end;
    end;
  end;
end;

function TPeepholeOptimizer.Run: Integer;
var
  i: Integer;
  Changed: Boolean;
  Pass: Integer;
begin
  {$IFDEF DISABLE_PEEPHOLE}
  {$IFDEF DEBUG_PEEPHOLE}
  if DebugPeephole then
    WriteLn('[Peephole] SKIPPED (disabled by flag)');
  {$ENDIF}
  Result := 0;
  Exit;
  {$ENDIF}

  {$IFDEF DEBUG_PEEPHOLE}
  if DebugPeephole then
    WriteLn('[Peephole] Running peephole optimization...');
  {$ENDIF}

  FOptimizedCount := 0;
  Pass := 0;

  // Run multiple passes until no more optimizations can be made
  repeat
    Changed := False;
    Inc(Pass);

    // Rebuild jump target map at start of each pass
    BuildJumpTargetMap;
    if FFuseStringTemps then BuildStringReadCounts;

    // Scan all instructions
    for i := 0 to FProgram.GetInstructionCount - 1 do
    begin
      // Try each optimization in order
      if OptimizeRedundantCopy(i) then
      begin
        Changed := True;
        Inc(FOptimizedCount);
      end
      else if OptimizeJumpChain(i) then
      begin
        Changed := True;
        Inc(FOptimizedCount);
      end
      else if OptimizeDeadJump(i) then
      begin
        Changed := True;
        Inc(FOptimizedCount);
      end
      else if OptimizeDeadConditionalJump(i) then
      begin
        Changed := True;
        Inc(FOptimizedCount);
      end
      else if FFuseStringTemps and OptimizeStringTempCopy(i) then
      begin
        Changed := True;
        Inc(FOptimizedCount);
      end;
    end;

    {$IFDEF DEBUG_PEEPHOLE}
    if Changed and DebugPeephole then
      WriteLn('[Peephole] Pass ', Pass, ': ', FOptimizedCount, ' total optimizations');
    {$ENDIF}

  until (not Changed) or (Pass >= 5);  // Limit passes to avoid infinite loops

  {$IFDEF DEBUG_PEEPHOLE}
  if DebugPeephole then
    WriteLn('[Peephole] Completed: ', FOptimizedCount, ' optimizations in ', Pass, ' pass(es)');
  {$ENDIF}
  Result := FOptimizedCount;
end;

function RunPeephole(AProgram: TBytecodeProgram): Integer;
var
  Optimizer: TPeepholeOptimizer;
begin
  Optimizer := TPeepholeOptimizer.Create(AProgram);
  try
    Result := Optimizer.Run;
  finally
    Optimizer.Free;
  end;
end;

end.
