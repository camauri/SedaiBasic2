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
{ ============================================================================
  Unit: SedaiRegAlloc (Linear Scan Register Allocation)

  Purpose: Allocate physical registers to SSA virtual registers using
           Linear Scan algorithm with spilling support.

  Algorithm: Linear Scan Register Allocation (Poletto & Sarkar, 1999)
             1. Compute live intervals for each virtual register
             2. Sort intervals by start point
             3. Process intervals in order:
                - If free register available → assign it
                - Else → spill (current or existing interval)
             4. Rewrite code with physical registers

  Key Features:
    - Fast O(n log n) allocation (vs O(n³) graph coloring)
    - Separate register banks for Int/Float/String
    - Spill to memory when registers exhausted
    - Respects BASIC global variable semantics (pre-allocated registers)

  Physical Register Limits (per type):
    - Integer: 32 registers (R0-R31)
    - Float: 32 registers (F0-F31)
    - String: 16 registers (S0-S15)

  Phase: Post-SSA (after PHI Elimination, before Bytecode Generation)
  Author: Sedai Project - Code Generation
  Date: 2025-01-27
  ============================================================================ }

unit SedaiRegAlloc;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I OptimizationFlags.inc}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, Generics.Collections, Contnrs, SedaiSSATypes;

const
  // Physical register limits per type
  MAX_INT_REGS = 32;
  MAX_FLOAT_REGS = 32;
  MAX_STRING_REGS = 16;

type
  { TLiveInterval - Live range for a virtual register }
  TLiveInterval = class
    RegType: TSSARegisterType;
    VirtualReg: Integer;      // Virtual register index
    Version: Integer;         // SSA version
    StartPos: Integer;        // First use position
    EndPos: Integer;          // Last use position
    PhysicalReg: Integer;     // Assigned physical register (-1 = spilled)
    SpillSlot: Integer;       // Spill slot index (-1 = not spilled)
    constructor Create(ARegType: TSSARegisterType; AVirtReg, AVersion: Integer);
    function Overlaps(Other: TLiveInterval): Boolean;
  end;

  { TVariableInfo - Usage statistics for BASIC variables }
  TVariableInfo = class
    VarName: string;           // BASIC variable name (e.g., "X", "I%", "S$")
    RegType: TSSARegisterType; // srtInt, srtFloat, srtString
    VirtualReg: Integer;       // Virtual register index
    Version: Integer;          // SSA version (0 for unversioned, >0 for temporaries)
    UsageCount: Integer;       // Number of uses in the program
    PhysicalReg: Integer;      // Assigned physical register (-1 = not allocated)
    constructor Create(const AName: string; ARegType: TSSARegisterType; AVirtReg, AVersion: Integer);
  end;

  { TLinearScanAllocator - Dual-mode register allocator }
  TLinearScanAllocator = class
  private
    FProgram: TSSAProgram;
    FIntervals: TObjectList;  // List of TLiveInterval (owns objects)
    FActive: TFPList;         // Active intervals (does not own)
    FFreeIntRegs: array[0..MAX_INT_REGS-1] of Boolean;
    FFreeFloatRegs: array[0..MAX_FLOAT_REGS-1] of Boolean;
    FFreeStringRegs: array[0..MAX_STRING_REGS-1] of Boolean;
    FNextSpillSlot: Integer;
    FSpillCount: Integer;
    FReuseColouring: specialize TDictionary<Int64, Integer>;   // phase 2: key -> shared register number
                                                               // (nil = allocate one number per value)
    FReuseColourCount: array[TSSARegisterType] of Integer;     // colours used per bank: anything the
                                                               // colouring did not cover is numbered ABOVE
                                                               // them, never into their range

    { Compute live intervals for all virtual registers }
    procedure ComputeLiveIntervals;

    { Find or create interval for a register }
    function FindInterval(RegType: TSSARegisterType; RegIndex, Version: Integer): TLiveInterval;

    { Allocate registers using linear scan }
    procedure LinearScan;

    { Try to allocate a free register }
    function TryAllocateFreeReg(Interval: TLiveInterval): Boolean;

    { Spill interval to memory }
    procedure SpillInterval(Interval: TLiveInterval);

    { Expire old intervals (free their registers) }
    procedure ExpireOldIntervals(CurrentPos: Integer);

    { Get next free physical register for type }
    function GetFreeRegister(RegType: TSSARegisterType): Integer;

    { Mark register as free }
    procedure FreeRegister(RegType: TSSARegisterType; PhysReg: Integer);

    { Rewrite program with physical registers }
    procedure RewriteProgram;

    { Rewrite a single value with physical register }
    function RewriteValue(const Val: TSSAValue): TSSAValue;

    { === BASIC Variable Allocation (GlobalVariableSemantics=True) === }

    { Run static BASIC variable allocation }
    function RunBASICAllocation: Integer;

    { --- Register REUSE analysis (phase 1: measurement only) -------------------------------
      The version-aware allocator below gives every (bank, index, version) its OWN number and
      never reuses a number once its value is dead: on the n-body MAIN that is 247 distinct
      float registers against a peak of 3 simultaneously live. Everything downstream inherits
      that count -- the AOT and the JIT can only pin a handful of VM registers in machine
      registers, so nearly every value round-trips through the bank arrays.

      Merging registers with DISJOINT live ranges is what fixes it, and is exactly what
      [[copycoal-miscompile]] shows must never be done without real interference analysis:
      Copy Coalescing replaced a copy's destination with its source program-wide and broke the
      per-predecessor copies PHI elimination emits. So the analysis comes FIRST, and on its own:
      this pass computes liveness over the CFG, builds the interference graph and REPORTS what a
      live-range-aware allocator would save. It changes nothing (byte-identical output) until
      the numbers say the merge is worth its risk.

      REGREUSE_DIAG=1 prints the report; REGREUSE=1 applies the merge (phase 2, default off). ---- }
    function ReuseDiagEnabled: Boolean;
    function ReuseMergeEnabled: Boolean;
    function ComputeReuseColouring(Map: specialize TDictionary<Int64, Integer>; Report: Boolean): Boolean;

    { Compute usage frequency for all BASIC variables }
    procedure ComputeVariableUsage(out VarList: TObjectList);

    { Allocate physical registers to BASIC variables by usage frequency }
    procedure AllocateBASICVariables(VarList: TObjectList);

    { Rewrite program with BASIC variable → physical register mapping }
    procedure RewriteProgramBASIC(VarList: TObjectList);

  public
    constructor Create(Prog: TSSAProgram);
    destructor Destroy; override;

    { Run register allocation - returns number of spills }
    function Run: Integer;
  end;

implementation

{$IFDEF DEBUG_REGALLOC}
uses SedaiDebug;
{$ENDIF}

var
  // Default ON; REGREUSE_STR=0 keeps the string bank out of the register merge (see OpIsMergeSafe).
  // -1 = not read yet.
  //
  // It went on only after the FB example sweep came back with its counts UNCHANGED (386/21/25/36).
  // That is the net that decides this class: it bounced REGREUSE on its first attempt while corpus,
  // AOT, JIT, combined and basc were all green. And it earned its keep immediately -- turning it on
  // exposed a latent bug nothing else had reached (see m492_input_prompt_reg0).
  GStrMerge: Integer = -1;

function GStrMergeEnabled: Boolean;
begin
  if GStrMerge < 0 then
    if GetEnvironmentVariable('REGREUSE_STR') = '0' then GStrMerge := 0 else GStrMerge := 1;
  Result := GStrMerge = 1;
end;

type
  TRegInfoMap = specialize TDictionary<Int64, TVariableInfo>;

{ Pack a register identity (bank, index, version) into one Int64 map key }
function RegAllocKey(RegType: TSSARegisterType; RegIdx, RegVer: Integer): Int64; inline;
begin
  Result := Int64(Ord(RegType)) or (Int64(RegIdx) shl 2) or (Int64(RegVer) shl 32);
end;

{ Helper function for sorting intervals by start position }
function CompareIntervals(Item1, Item2: Pointer): Integer;
var
  I1, I2: TLiveInterval;
begin
  I1 := TLiveInterval(Item1);
  I2 := TLiveInterval(Item2);
  Result := I1.StartPos - I2.StartPos;
end;

{ Helper function for sorting variables by usage count (descending) }
function CompareVariableUsage(Item1, Item2: Pointer): Integer;
var
  V1, V2: TVariableInfo;
begin
  V1 := TVariableInfo(Item1);
  V2 := TVariableInfo(Item2);
  Result := V2.UsageCount - V1.UsageCount;  // Descending (most used first)
end;

{ TVariableInfo }

constructor TVariableInfo.Create(const AName: string; ARegType: TSSARegisterType; AVirtReg, AVersion: Integer);
begin
  inherited Create;
  VarName := AName;
  RegType := ARegType;
  VirtualReg := AVirtReg;
  Version := AVersion;
  UsageCount := 0;
  PhysicalReg := -1;
end;

{ TLiveInterval }

constructor TLiveInterval.Create(ARegType: TSSARegisterType; AVirtReg, AVersion: Integer);
begin
  inherited Create;
  RegType := ARegType;
  VirtualReg := AVirtReg;
  Version := AVersion;
  StartPos := MaxInt;
  EndPos := -1;
  PhysicalReg := -1;
  SpillSlot := -1;
end;

function TLiveInterval.Overlaps(Other: TLiveInterval): Boolean;
begin
  // Two intervals overlap if one starts before the other ends
  Result := (StartPos <= Other.EndPos) and (Other.StartPos <= EndPos);
end;

{ TLinearScanAllocator }

constructor TLinearScanAllocator.Create(Prog: TSSAProgram);
var
  i: Integer;
begin
  inherited Create;
  FProgram := Prog;
  FIntervals := TObjectList.Create(True);  // Owns TLiveInterval objects
  FActive := TFPList.Create;  // Does not own
  FNextSpillSlot := 0;
  FSpillCount := 0;

  // Initialize all registers as free
  for i := 0 to MAX_INT_REGS - 1 do
    FFreeIntRegs[i] := True;
  for i := 0 to MAX_FLOAT_REGS - 1 do
    FFreeFloatRegs[i] := True;
  for i := 0 to MAX_STRING_REGS - 1 do
    FFreeStringRegs[i] := True;
end;

destructor TLinearScanAllocator.Destroy;
begin
  FIntervals.Free;
  FActive.Free;
  inherited;
end;

function TLinearScanAllocator.Run: Integer;
begin
  {$IFDEF DISABLE_REG_ALLOC}
  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
    WriteLn('[RegAlloc] SKIPPED (disabled by flag)');
  {$ENDIF}
  Result := 0;
  Exit;
  {$ENDIF}

  // Phase A: the version-aware static BASIC allocator handles BOTH dialects. It keys every value on
  // (RegType, RegIndex, Version), so with MODERN versioning ON each SSA version gets its own physical
  // register (SSA-correct, non-coalesced) and overflow spills safely to indices >= MAX_*_REGS. This
  // sidesteps the Linear-Scan path below, which is an unfinished stub (textual live intervals that
  // ignore the CFG + a spill path that emits negative register indices no downstream stage handles).
  // The Linear-Scan code is kept, unreachable, as a reference skeleton for the future AOT backend (B),
  // which will do real whole-function register allocation. See job/docs/PIANO_FASE_A_SSA_VERSIONING.md.
  begin
    {$IFDEF DEBUG_REGALLOC}
    if DebugRegAlloc then
      WriteLn('[RegAlloc] Running version-aware BASIC Variable Allocation (both dialects)...');
    {$ENDIF}
    Result := RunBASICAllocation;
    Exit;
  end;

  // SSA mode: Linear Scan allocation (requires SSA versioning) -- UNREACHABLE (see above)
  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
    WriteLn('[RegAlloc] Running Linear Scan register allocation (SSA mode)...');
  {$ENDIF}

  // Step 1: Compute live intervals
  ComputeLiveIntervals;

  // Step 2: Allocate registers
  LinearScan;

  // Step 3: Rewrite program
  RewriteProgram;

  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
    WriteLn('[RegAlloc] Allocated registers with ', FSpillCount, ' spills');
  {$ENDIF}
  Result := FSpillCount;
end;

procedure TLinearScanAllocator.ComputeLiveIntervals;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  i, j, k, Position: Integer;
  Interval: TLiveInterval;

  procedure UpdateInterval(const Val: TSSAValue; Pos: Integer);
  var
    Intv: TLiveInterval;
  begin
    if Val.Kind <> svkRegister then Exit;

    Intv := FindInterval(Val.RegType, Val.RegIndex, Val.Version);
    if Pos < Intv.StartPos then
      Intv.StartPos := Pos;
    if Pos > Intv.EndPos then
      Intv.EndPos := Pos;
  end;

begin
  Position := 0;

  // Scan all instructions to build live intervals
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];

    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];
      Inc(Position);

      // Update intervals for source operands (use)
      UpdateInterval(Instr.Src1, Position);
      UpdateInterval(Instr.Src2, Position);
      UpdateInterval(Instr.Src3, Position);

      // Update interval for destination (def)
      if Instr.Dest.Kind = svkRegister then
        UpdateInterval(Instr.Dest, Position);

      // CRITICAL: Update intervals for PhiSources (used by BOX, RGBA, etc.)
      for k := 0 to High(Instr.PhiSources) do
        UpdateInterval(Instr.PhiSources[k].Value, Position);
    end;
  end;

  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
    WriteLn('[RegAlloc] Computed ', FIntervals.Count, ' live intervals');
  {$ENDIF}
end;

function TLinearScanAllocator.FindInterval(RegType: TSSARegisterType; RegIndex, Version: Integer): TLiveInterval;
var
  i: Integer;
  Interval: TLiveInterval;
begin
  // Search for existing interval
  for i := 0 to FIntervals.Count - 1 do
  begin
    Interval := TLiveInterval(FIntervals[i]);
    if (Interval.RegType = RegType) and
       (Interval.VirtualReg = RegIndex) and
       (Interval.Version = Version) then
      Exit(Interval);
  end;

  // Create new interval
  Result := TLiveInterval.Create(RegType, RegIndex, Version);
  FIntervals.Add(Result);
end;

procedure TLinearScanAllocator.LinearScan;
var
  i: Integer;
  Interval: TLiveInterval;
begin
  // Sort intervals by start position
  FIntervals.Sort(@CompareIntervals);

  // Process each interval
  for i := 0 to FIntervals.Count - 1 do
  begin
    Interval := TLiveInterval(FIntervals[i]);

    // Expire old intervals that ended before this one starts
    ExpireOldIntervals(Interval.StartPos);

    // Try to allocate a register
    if not TryAllocateFreeReg(Interval) then
    begin
      // No free register - must spill
      SpillInterval(Interval);
    end;
  end;
end;

procedure TLinearScanAllocator.ExpireOldIntervals(CurrentPos: Integer);
var
  i: Integer;
  Interval: TLiveInterval;
begin
  i := 0;
  while i < FActive.Count do
  begin
    Interval := TLiveInterval(FActive[i]);

    if Interval.EndPos < CurrentPos then
    begin
      // This interval has ended - free its register
      if Interval.PhysicalReg >= 0 then
        FreeRegister(Interval.RegType, Interval.PhysicalReg);

      FActive.Delete(i);
    end
    else
      Inc(i);
  end;
end;

function TLinearScanAllocator.TryAllocateFreeReg(Interval: TLiveInterval): Boolean;
var
  PhysReg: Integer;
begin
  PhysReg := GetFreeRegister(Interval.RegType);

  if PhysReg >= 0 then
  begin
    // Found free register
    Interval.PhysicalReg := PhysReg;
    FActive.Add(Interval);
    Result := True;
  end
  else
    Result := False;
end;

procedure TLinearScanAllocator.SpillInterval(Interval: TLiveInterval);
begin
  // Assign spill slot
  Interval.SpillSlot := FNextSpillSlot;
  Inc(FNextSpillSlot);
  Inc(FSpillCount);

  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
    WriteLn('[RegAlloc] Spilled r', Interval.VirtualReg, '_v', Interval.Version,
            ' to slot ', Interval.SpillSlot);
  {$ENDIF}
end;

function TLinearScanAllocator.GetFreeRegister(RegType: TSSARegisterType): Integer;
var
  i: Integer;
begin
  case RegType of
    srtInt:
      for i := 0 to MAX_INT_REGS - 1 do
        if FFreeIntRegs[i] then
        begin
          FFreeIntRegs[i] := False;
          Exit(i);
        end;

    srtFloat:
      for i := 0 to MAX_FLOAT_REGS - 1 do
        if FFreeFloatRegs[i] then
        begin
          FFreeFloatRegs[i] := False;
          Exit(i);
        end;

    srtString:
      for i := 0 to MAX_STRING_REGS - 1 do
        if FFreeStringRegs[i] then
        begin
          FFreeStringRegs[i] := False;
          Exit(i);
        end;
  end;

  Result := -1;  // No free register
end;

procedure TLinearScanAllocator.FreeRegister(RegType: TSSARegisterType; PhysReg: Integer);
begin
  case RegType of
    srtInt: FFreeIntRegs[PhysReg] := True;
    srtFloat: FFreeFloatRegs[PhysReg] := True;
    srtString: FFreeStringRegs[PhysReg] := True;
  end;
end;

procedure TLinearScanAllocator.RewriteProgram;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  i, j, k: Integer;
begin
  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
    WriteLn('[RegAlloc] Rewriting program with physical registers...');
  {$ENDIF}

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];

    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      // Rewrite source operands
      Instr.Src1 := RewriteValue(Instr.Src1);
      Instr.Src2 := RewriteValue(Instr.Src2);
      Instr.Src3 := RewriteValue(Instr.Src3);

      // Rewrite destination
      Instr.Dest := RewriteValue(Instr.Dest);

      // CRITICAL: Rewrite PhiSources for instructions that use them for extra operands
      // (e.g., ssaGraphicBox uses PhiSources[0..4] for x2, y2, angle, filled, fill_color)
      for k := 0 to High(Instr.PhiSources) do
        Instr.PhiSources[k].Value := RewriteValue(Instr.PhiSources[k].Value);
    end;
  end;
end;

function TLinearScanAllocator.RewriteValue(const Val: TSSAValue): TSSAValue;
var
  Interval: TLiveInterval;
  i: Integer;
begin
  Result := Val;

  if Val.Kind <> svkRegister then Exit;

  // Find interval for this virtual register
  for i := 0 to FIntervals.Count - 1 do
  begin
    Interval := TLiveInterval(FIntervals[i]);
    if (Interval.RegType = Val.RegType) and
       (Interval.VirtualReg = Val.RegIndex) and
       (Interval.Version = Val.Version) then
    begin
      if Interval.PhysicalReg >= 0 then
      begin
        // Use physical register
        Result.RegIndex := Interval.PhysicalReg;
        Result.Version := 0;  // Physical registers don't have versions
      end
      else
      begin
        // Spilled - keep virtual register (bytecode gen will handle spill loads/stores)
        // For now, just mark with negative index to indicate spill
        Result.RegIndex := -(Interval.SpillSlot + 1);  // Negative = spilled
      end;

      Exit;
    end;
  end;
end;

{ === BASIC Variable Allocation Implementation === }

function TLinearScanAllocator.ReuseDiagEnabled: Boolean;
// =1 reports the summary and the shared colours; =2 adds the per-block liveness dump.
begin
  Result := (GetEnvironmentVariable('REGREUSE_DIAG') = '1') or
            (GetEnvironmentVariable('REGREUSE_DIAG') = '2');
end;

function TLinearScanAllocator.ReuseMergeEnabled: Boolean;
// Tri-state gate, the same shape AOT_DYNF uses: REGREUSE=1 forces the merge on, =0 forces it off,
// unset takes the default.
//
// The default is ON since 2026-07-24. Flipping it was attempted, REVERTED the same day, and then
// re-done once the two things the revert exposed were understood - neither of which was a defect
// of the merge:
//
//   * control/for-next2 hung printing -1 (TRUE) forever: a comparison's result reaching PRINT where
//     the loop variable belonged. The CAUSE was a malformed CFG - a top-tested DO whose condition
//     opens blocks of its own (IIf) wired its body edge to the block the condition STARTED in
//     rather than the one the branch was emitted into. Liveness then read a graph that was not the
//     program and called the loop variable dead across the test. Fixed in ProcessDoLoop; every
//     other consumer of that CFG (LICM, DCE, GVN, range analysis) had been misinformed too.
//   * strings/lset-udt printed 0 instead of 1234: LSET on a UDT was not implemented and fell
//     through to the STRING path, which only ever worked because the int and string banks happened
//     to hand out matching register numbers. The merge destroys that coincidence. Implemented.
//
// Only after both were closed does the FB example sweep - the net that runs real third-party
// programs, and the one that rejected the first attempt - return the SAME counts and the SAME diff
// list with the merge on as with it off.
//
// The performance case (measured best-of-N, interleaved A/B on one binary, output bit-identical,
// fbc thermometer ~200 ms). The merge is not an AOT affair: it shrinks the register BANKS, so
// every profile pays less traffic, the interpreter included:
//
//   interpreter  n-body -5%   arraysum -19%   sieve  -9%
//   --jit        n-body -11%  arraysum -26%   sieve  -4%
//   --aot        n-body -11%  floatpoly -24%  arraysum -27%  sieve -20%
//   combined     n-body -7%   floatpoly -23%  arraysum -30%
//
// The losses are intpoly (+12%) and, marginally, cvtpoly (+6%) and strops (+3%) - integer-heavy
// loops whose post-merge pressure still exceeds the 7 usable GPRs, where merging only lengthens
// live ranges. Nobody has a per-region rule for that yet (see the note in RunBASICAllocation), and
// with the wins reaching -30% the flat default is worth its three regressions.
//
// REGREUSE=0 restores the historic one-number-per-value allocation exactly, which is what makes
// every measurement above an A/B on a single binary.
//
// ⚠️ LESSON, the same one Copy Coalescing taught, and the reason this gate moved only on the second
// attempt: for a register allocator the net that decides is REAL PROGRAMS. When the first attempt
// was reverted, corpus 496/496, AOT 0/700, JIT 0/706, combined 0/700 and basc 730/2/0 were ALL
// green with the merge on.
begin
  Result := GetEnvironmentVariable('REGREUSE') <> '0';
end;

function OpIsMergeSafe(Op: TSSAOpCode): Boolean;
// May a register TOUCHED by this opcode take part in the merge at all?
//
// The first cut of phase 2 asked the narrower question -- "does this opcode read its Dest?" -- and
// the nets answered with 16 failures: the graphics family, the error-handler paths and parts of file
// I/O all move values through channels this liveness does not model (a TRAP handler is entered from
// anywhere, so its CFG edges do not exist; several ops carry operands in fields the analysis reads
// as definitions). Enumerating what is unsafe is the losing side of that bet -- a missed opcode is a
// silent miscompile, and the failures proved the enumeration incomplete.
//
// So the polarity is inverted: only registers whose EVERY mention is one of the plainly-modelled
// opcodes below may be merged. Everything else is pinned. A new opcode then costs a missed merge,
// never a wrong one. This is the same shape as C4's mechanically-derived deny-list, but with the
// safe default -- and it keeps the target, since the loops that matter (n-body, floatpoly, intpoly,
// arraysum) are arithmetic, copies, loads, comparisons and array reads.
begin
  case Op of
    ssaLoadConstInt, ssaLoadConstFloat,
    ssaCopyInt, ssaCopyFloat,
    ssaAddInt, ssaSubInt, ssaMulInt, ssaDivInt, ssaModInt, ssaNegInt,
    ssaDivUInt, ssaModUInt,
    ssaAddFloat, ssaSubFloat, ssaMulFloat, ssaDivFloat, ssaModFloat, ssaPowFloat, ssaNegFloat,
    ssaIntToFloat, ssaFloatToInt,
    ssaCmpEqInt, ssaCmpNeInt, ssaCmpLtInt, ssaCmpGtInt, ssaCmpLeInt, ssaCmpGeInt,
    ssaCmpLtUInt, ssaCmpGtUInt, ssaCmpLeUInt, ssaCmpGeUInt,
    ssaCmpEqFloat, ssaCmpNeFloat, ssaCmpLtFloat, ssaCmpGtFloat, ssaCmpLeFloat, ssaCmpGeFloat,
    ssaBitwiseAnd, ssaBitwiseOr, ssaBitwiseXor, ssaBitwiseNot,
    // Array element access: the load writes its Dest, the store carries the VALUE there and is
    // handled as a read (DestIsPureDef). The hot loops are array updates, so leaving these out
    // pinned most of what the merge exists for.
    ssaArrayLoad, ssaArrayStore,
    ssaArrayLoadIndInt, ssaArrayLoadIndFloat,
    ssaArrayStoreIndInt, ssaArrayStoreIndFloat,
    // PRINT reads its operand and writes nothing (verified against the derived read-Dest list).
    ssaPrint, ssaPrintLn, ssaPrintInt, ssaPrintIntLn:
      Result := True;
    // === STRING BANK (REGREUSE_STR=1, default off) =====================================
    // Leaving these out pinned EVERY string register, and with them the one merge the string
    // benchmarks need: "acc = acc + x" lowers to "concat(new, old, x)" plus the copies that close
    // the loop-carried PHI, so Dest and Src1 are different registers and the accumulator is REBUILT
    // on every character instead of grown in place. Merging them is exactly what AppendString waits
    // for -- and this analysis is the one place where it can be done SAFELY, because it asks whether
    // the two interfere instead of substituting names the way the disabled copy coalescer did.
    //
    // Only the plainly-modelled ones, same rule as above: Dest written, Src read, no value travelling
    // through a channel this liveness cannot see. ssaStrInstr is deliberately absent -- it carries an
    // input in Dest (it is in the read-Dest list below).
    ssaCopyString, ssaLoadConstString, ssaStrConcat,
    ssaStrLen, ssaStrAsc, ssaStrAscMid, ssaStrChr,
    // SPACE(n) and STRING(n,ch) are how a BUFFER is created before it is filled a byte at a time,
    // and leaving them out pinned the buffer itself. The fill then came out "MidAssign Dest=R1
    // Src1=R0" plus "CopyString R0, R1", so target and result ALIAS at the top of the next
    // iteration and UniqueString copies the whole buffer per byte - quadratic. Measured 20 Aug 2026
    // inside a SUB: 100,000 bytes 245 ms, 200,000 bytes 931 ms, against 8 ms for the identical fill
    // at module level (where the variable is one register and no copy closes the loop).
    // Both write Dest and never read it, and read only integer sources - see their VM arms.
    ssaStrSpace, ssaStrString,
    // ssaStrConcatCharAt is modelled exactly like ssaStrConcat -- Dest written, Src1/Src2/Src3 read
    // -- and it needs the merge for the same reason and more sharply: its whole point is to grow the
    // accumulator in place, which only happens when Dest and Src1 end up as the same register. Left
    // out, every register it touched was pinned, so the fusion produced Dest <> Src1 and fell back to
    // the allocating path -- which is what made it measure SLOWER than the two instructions it
    // replaces. The opcode only exists in programs the AOT will run (see RunConcatCharFusion), so
    // this line is inert for everything else.
    ssaStrConcatCharAt,
    ssaStrMidAssign,
    ssaStrMid, ssaStrLeft, ssaStrRight,
    ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString,
    ssaArrayLoadIndString, ssaArrayStoreIndString,
    ssaPrintString:
      Result := GStrMergeEnabled;
  else
    Result := False;
  end;
end;

function DestIsPureDef(Op: TSSAOpCode): Boolean;
// Does this opcode's Dest field WRITE a value and never read one?
//
// It is not a rhetorical question: several opcodes carry an INPUT in Dest (the graphics family puts
// a coordinate there, ssaArrayStore the value being stored) and several read the incoming value
// before overwriting it (bcGetBinStr reads Len(dest) to know how many bytes to read -- the very op
// this session touched). Treating those as definitions would let liveness end a value that is still
// needed, and the merge would then hand its register to somebody else. Silently.
//
// So the list is DERIVED, never eyeballed -- the same rule C4's helper deny-list follows. Regenerate
// with, from the repository root:
//
//   awk '/^    [0-9]+: *\/\/ *bc[A-Za-z0-9_]+/ { match($0,/bc[A-Za-z0-9_]+/); cur=substr($0,RSTART,RLENGTH) }
//        /Regs\[Instr\.Dest\]/ { l=$0
//          if (l ~ /Regs\[Instr\.Dest\] *:=/) { r=l; sub(/.*Regs\[Instr\.Dest\] *:=/,"",r); if (r !~ /Regs\[Instr\.Dest\]/) next }
//          if (l ~ /WriteLn|StdErr/) next; if (cur != "") print cur }' src/SedaiBytecodeVM.pas | sort -u
//
// then map each bytecode name back through the "ssaX: Result := bcY" table in SedaiBytecodeCompiler.
// For every opcode below, Dest is treated as a pure USE: correct when it is an input, and merely
// conservative (a longer live range) when it is a read-modify-write.
//
// The "To"/"Self" accumulator superinstructions (bcAddIntTo, bcMulFloatTo, ...) do read their Dest
// but never appear here: the bytecode peephole fuses them AFTER register allocation, out of reach
// of this analysis.
begin
  case Op of
    ssaArrayStore, ssaArrayStoreIndInt, ssaArrayStoreIndFloat, ssaArrayStoreIndString,
    // ssaStrAppendMapped APPENDS to its Dest, so the incoming value is an input: treating Dest as a
    // pure definition would let liveness end the accumulator that the instruction is about to grow.
    ssaStrAppendMapped,
    ssaGetBinStr, ssaStrInstr, ssaPrintFile, ssaSetColor,
    ssaGraphicBox, ssaGraphicCircle, ssaGraphicDraw, ssaGraphicGShape, ssaGraphicPaint,
    ssaGraphicScale, ssaGraphicWindow, ssaGfxCircleEx, ssaGfxLineStyled,
    ssaMovsprAbs, ssaMovsprAuto, ssaMovsprPolar, ssaMovsprRel,
    ssaSprite, ssaSprsize, ssaSoundSound, ssaSoundFilter:
      Result := False;
  else
    Result := True;
  end;
end;

function TLinearScanAllocator.ComputeReuseColouring(Map: specialize TDictionary<Int64, Integer>; Report: Boolean): Boolean;
// Liveness over the CFG + interference graph + greedy colouring, per bank. Reports how many VM
// registers a live-range-aware allocator would need where the current one needs one per value.
// MEASUREMENT ONLY -- nothing here writes to the program.
//
// Soundness notes that the eventual merge must honour (recorded here while they are fresh):
//  * GOSUB: the CFG has no return edge, so liveness ACROSS a GOSUB describes a graph that is not
//    the program. Any register live across one is reported separately and must never be merged.
//  * Registers named OUTSIDE the instruction stream -- an array's runtime dimension and lower-bound
//    registers -- have no visible use, so they are pinned (treated as live everywhere).
//  * A value with no definition in the function (a parameter, a module global reached through a
//    call) must be pinned for the same reason: its live-in point is invisible here.
type
  TKeyRec = record Bank: TSSARegisterType; Idx, Ver: Integer; end;
var
  KeyMap: specialize TDictionary<Int64, Integer>;
  BlkIdx: specialize TDictionary<Pointer, Integer>;   // block -> its POSITION in the list. The block's
                                                      // own BlockIndex field is maintained for SSA
                                                      // construction and is not guaranteed to still
                                                      // match after the optimization passes ran.
  Keys: array of TKeyRec;
  NKeys, NB, bi, ii, si, k, kk, pass, DefKey: Integer;
  Blk, Succ: TSSABasicBlock;
  Ins: TSSAInstruction;
  Changed, HasGosub, HasHandler: Boolean;
  BUse, BDef, BIn, BOut: array of array of Boolean;   // [block][key]
  Live: array of Boolean;                             // mid-block live set
  Pinned: array of Boolean;                           // never merge (see notes above)
  Adj: array of TFPList;                              // interference graph (key -> neighbour keys)
  Colour: array of Integer;
  UsedCol: array of Boolean;
  Distinct, Colours: array[TSSARegisterType] of Integer;
  PinnedN, LiveAcrossGosub, sidx: Integer;
  HasDef: array of Boolean;      // a key with no definition anywhere is pinned (invisible live-in)
  UseCount: array of Integer;    // mentions per key: hot values are coloured first, keeping low indexes
  Order: array of Integer;
  PinBase: array[TSSARegisterType] of Integer;
  Bank: TSSARegisterType;

  function KeyOf(const V: TSSAValue): Integer;
  // Interning: every (bank, index, version) the program mentions gets a dense id. -1 = not a register.
  var
    Kk2: Int64;
  begin
    Result := -1;
    if V.Kind <> svkRegister then Exit;
    Kk2 := RegAllocKey(V.RegType, V.RegIndex, V.Version);
    if not KeyMap.TryGetValue(Kk2, Result) then
    begin
      Result := NKeys;
      KeyMap.Add(Kk2, Result);
      if NKeys >= Length(Keys) then SetLength(Keys, NKeys * 2 + 64);
      Keys[NKeys].Bank := V.RegType; Keys[NKeys].Idx := V.RegIndex; Keys[NKeys].Ver := V.Version;
      Inc(NKeys);
    end;
  end;

  procedure NoteUse(const V: TSSAValue; b: Integer);
  var q: Integer;
  begin
    q := KeyOf(V);
    if (q >= 0) and (not BDef[b][q]) then BUse[b][q] := True;   // used before any def in this block
  end;

  procedure NoteDef(const V: TSSAValue; b: Integer);
  var q: Integer;
  begin
    q := KeyOf(V);
    if q >= 0 then BDef[b][q] := True;
  end;

  procedure Interfere(a, b2: Integer);
  begin
    if (a < 0) or (b2 < 0) or (a = b2) then Exit;
    if Keys[a].Bank <> Keys[b2].Bank then Exit;      // separate banks never share a number
    Adj[a].Add(Pointer(PtrInt(b2)));
    Adj[b2].Add(Pointer(PtrInt(a)));
  end;

begin
  NB := FProgram.Blocks.Count;
  Result := False;
  if NB = 0 then Exit;
  NKeys := 0;
  SetLength(Keys, 256);
  KeyMap := specialize TDictionary<Int64, Integer>.Create;
  BlkIdx := specialize TDictionary<Pointer, Integer>.Create;
  try
    for bi := 0 to NB - 1 do
      BlkIdx.AddOrSetValue(Pointer(FProgram.Blocks[bi]), bi);
    // Pass 0: intern every register the instruction stream mentions, and collect per-block use/def.
    // (Two passes over the blocks: the first only to size the bitsets, since interning grows NKeys.)
    HasGosub := False;
    HasHandler := False;
    for bi := 0 to NB - 1 do
    begin
      Blk := FProgram.Blocks[bi];
      for ii := 0 to Blk.Instructions.Count - 1 do
      begin
        Ins := Blk.Instructions[ii];
        KeyOf(Ins.Dest); KeyOf(Ins.Src1); KeyOf(Ins.Src2); KeyOf(Ins.Src3);
        for k := 0 to High(Ins.PhiSources) do KeyOf(Ins.PhiSources[k].Value);
        if (Ins.OpCode = ssaCall) or (Ins.OpCode = ssaReturn) then HasGosub := True;   // GOSUB lowers to ssaCall
        // An error handler is entered from ANYWHERE -- TRAP/ON ERROR install it and any faulting
        // instruction jumps to it -- and none of those edges exist in the CFG. Liveness computed
        // without them is not a description of this program, so no register in it may be merged.
        // Found the honest way: the first cut left m448 and aot_b1_deopt_diverror failing.
        // (a plain chain, not a set: TSSAOpCode has well over the 256 elements a Pascal set holds)
        if (Ins.OpCode = ssaTrap) or (Ins.OpCode = ssaOnError) or (Ins.OpCode = ssaResume) or
           (Ins.OpCode = ssaResumeNext) or (Ins.OpCode = ssaResumeLabel) then
          HasHandler := True;
      end;
    end;
    if NKeys = 0 then Exit;
    // Guard: the bitsets are blocks x keys. test_cfg_large is 100k blocks; refuse rather than
    // allocate gigabytes for a diagnostic.
    if (Int64(NB) * NKeys) > 40 * 1000 * 1000 then
    begin
      // Gated: this used to print unconditionally, so an enormous program (test_cfg_large) emitted a
      // line on the source run that the .basc run -- which never allocates -- could not emit, and the
      // round-trip sweep reported a DIFF that was the diagnostic talking, not the program.
      if Report then
        WriteLn('[RegReuse] skipped: ', NB, ' blocks x ', NKeys, ' registers is too large to analyse');
      Exit;
    end;

    SetLength(BUse, NB); SetLength(BDef, NB); SetLength(BIn, NB); SetLength(BOut, NB);
    for bi := 0 to NB - 1 do
    begin
      SetLength(BUse[bi], NKeys); SetLength(BDef[bi], NKeys);
      SetLength(BIn[bi], NKeys); SetLength(BOut[bi], NKeys);
    end;
    SetLength(Pinned, NKeys); SetLength(Colour, NKeys); SetLength(Live, NKeys);
    SetLength(HasDef, NKeys); SetLength(UseCount, NKeys);
    // Mentions per key, for the colouring order: the hottest values must keep the low indexes,
    // which is what the AOT and the JIT pin.
    for bi := 0 to NB - 1 do
    begin
      Blk := FProgram.Blocks[bi];
      for ii := 0 to Blk.Instructions.Count - 1 do
      begin
        Ins := Blk.Instructions[ii];
        k := KeyOf(Ins.Dest); if k >= 0 then Inc(UseCount[k]);
        k := KeyOf(Ins.Src1); if k >= 0 then Inc(UseCount[k]);
        k := KeyOf(Ins.Src2); if k >= 0 then Inc(UseCount[k]);
        k := KeyOf(Ins.Src3); if k >= 0 then Inc(UseCount[k]);
      end;
    end;
    SetLength(Adj, NKeys);
    for k := 0 to NKeys - 1 do Adj[k] := TFPList.Create;

    for bi := 0 to NB - 1 do
    begin
      Blk := FProgram.Blocks[bi];
      for ii := 0 to Blk.Instructions.Count - 1 do
      begin
        Ins := Blk.Instructions[ii];
        // Reads first, then the write: an instruction that reads and writes the same register
        // ("r = r + 1") must count the read as a use of the incoming value.
        NoteUse(Ins.Src1, bi); NoteUse(Ins.Src2, bi); NoteUse(Ins.Src3, bi);
        // A PHI's operands are live out of the PREDECESSOR, not of this block.
        for k := 0 to High(Ins.PhiSources) do
          if (Ins.PhiSources[k].FromBlock <> nil) and BlkIdx.TryGetValue(Pointer(Ins.PhiSources[k].FromBlock), si) then
            NoteUse(Ins.PhiSources[k].Value, si);
        // Dest is a definition only for the opcodes that truly only write it (see DestIsPureDef);
        // for the rest it carries an input, or the incoming value, and counts as a use.
        if Ins.Dest.Kind = svkRegister then
        begin
          if DestIsPureDef(Ins.OpCode) then
          begin
            NoteDef(Ins.Dest, bi);
            k := KeyOf(Ins.Dest);
            if k >= 0 then HasDef[k] := True;
          end
          else
            NoteUse(Ins.Dest, bi);
        end;
      end;
    end;

    // A key never DEFINED anywhere is live in from somewhere this analysis cannot see (a parameter,
    // a global written through a call). With no definition it collects no interference edges either,
    // so it would happily merge with anything: pin it.
    for k := 0 to NKeys - 1 do
      if not HasDef[k] then Pinned[k] := True;

    // A program with an error handler gets NO merging at all: see the note at HasHandler.
    if HasHandler then
      for k := 0 to NKeys - 1 do Pinned[k] := True;

    // Pin every register that any opcode outside the modelled set touches (see OpIsMergeSafe).
    for bi := 0 to NB - 1 do
    begin
      Blk := FProgram.Blocks[bi];
      for ii := 0 to Blk.Instructions.Count - 1 do
      begin
        Ins := Blk.Instructions[ii];
        if OpIsMergeSafe(Ins.OpCode) then Continue;
        k := KeyOf(Ins.Dest); if k >= 0 then Pinned[k] := True;
        k := KeyOf(Ins.Src1); if k >= 0 then Pinned[k] := True;
        k := KeyOf(Ins.Src2); if k >= 0 then Pinned[k] := True;
        k := KeyOf(Ins.Src3); if k >= 0 then Pinned[k] := True;
        for si := 0 to High(Ins.PhiSources) do
        begin
          k := KeyOf(Ins.PhiSources[si].Value); if k >= 0 then Pinned[k] := True;
        end;
      end;
    end;

    // Pin the registers whose liveness this analysis cannot see.
    PinnedN := 0;
    for si := 0 to FProgram.GetArrayCount - 1 do
    begin
      for k := 0 to High(FProgram.GetArray(si).DimRegisters) do
        if FProgram.GetArray(si).DimRegisters[k] >= 0 then
          for kk := 0 to NKeys - 1 do
            if (Keys[kk].Bank = FProgram.GetArray(si).DimRegTypes[k]) and
               (Keys[kk].Idx = FProgram.GetArray(si).DimRegisters[k]) then Pinned[kk] := True;
      for k := 0 to High(FProgram.GetArray(si).LowerBoundRegisters) do
        if FProgram.GetArray(si).LowerBoundRegisters[k] >= 0 then
          for kk := 0 to NKeys - 1 do
            if (Keys[kk].Bank = srtInt) and (Keys[kk].Idx = FProgram.GetArray(si).LowerBoundRegisters[k]) then
              Pinned[kk] := True;
    end;

    // Backward dataflow to a fixpoint: out(B) = U in(S) for S in succ(B); in(B) = use(B) + (out(B) - def(B)).
    pass := 0;
    repeat
      Changed := False;
      Inc(pass);
      for bi := NB - 1 downto 0 do
      begin
        Blk := FProgram.Blocks[bi];
        for si := 0 to Blk.Successors.Count - 1 do
        begin
          Succ := TSSABasicBlock(Blk.Successors[si]);
          if not BlkIdx.TryGetValue(Pointer(Succ), sidx) then Continue;
          for k := 0 to NKeys - 1 do
            if BIn[sidx][k] and (not BOut[bi][k]) then
            begin BOut[bi][k] := True; Changed := True; end;
        end;
        for k := 0 to NKeys - 1 do
          if (BUse[bi][k] or (BOut[bi][k] and (not BDef[bi][k]))) and (not BIn[bi][k]) then
          begin BIn[bi][k] := True; Changed := True; end;
      end;
    until (not Changed) or (pass > 200);

    // Interference: walk each block backwards from its live-out set. At a definition, the value
    // being defined interferes with everything else live at that point.
    LiveAcrossGosub := 0;
    for bi := 0 to NB - 1 do
    begin
      Blk := FProgram.Blocks[bi];
      for k := 0 to NKeys - 1 do Live[k] := BOut[bi][k];
      for ii := Blk.Instructions.Count - 1 downto 0 do
      begin
        Ins := Blk.Instructions[ii];
        DefKey := -1;
        if (Ins.Dest.Kind = svkRegister) and DestIsPureDef(Ins.OpCode) then DefKey := KeyOf(Ins.Dest);
        if DefKey >= 0 then
        begin
          for k := 0 to NKeys - 1 do
            if Live[k] then Interfere(DefKey, k);
          Live[DefKey] := False;
        end
        // Dest carries an input for this opcode: it is a read, so the value stays live above it.
        else if Ins.Dest.Kind = svkRegister then
        begin
          k := KeyOf(Ins.Dest); if k >= 0 then Live[k] := True;
        end;
        // A value live across a GOSUB is live over an edge the CFG does not have: never merge it.
        if (Ins.OpCode = ssaCall) or (Ins.OpCode = ssaReturn) then
          for k := 0 to NKeys - 1 do
            if Live[k] and (not Pinned[k]) then
            begin Pinned[k] := True; Inc(LiveAcrossGosub); end;
        k := KeyOf(Ins.Src1); if k >= 0 then Live[k] := True;
        k := KeyOf(Ins.Src2); if k >= 0 then Live[k] := True;
        k := KeyOf(Ins.Src3); if k >= 0 then Live[k] := True;
      end;
    end;
    for k := 0 to NKeys - 1 do if Pinned[k] then Inc(PinnedN);

    // Colouring, per bank, in TWO phases.
    //  1. Every PINNED register takes a colour of its own, and those colours are then off limits:
    //     a pinned register is one whose live range this analysis does not trust, so it must not
    //     share a number with anybody, not even with someone it appears not to interfere with.
    //  2. The rest are coloured greedily (first fit above the pinned block), MOST-USED FIRST, so
    //     the hot values keep the low indexes the AOT and JIT pin.
    for k := 0 to NKeys - 1 do Colour[k] := -1;
    for Bank := Low(TSSARegisterType) to High(TSSARegisterType) do
    begin Distinct[Bank] := 0; Colours[Bank] := 0; PinBase[Bank] := 0; end;
    for k := 0 to NKeys - 1 do
    begin
      Inc(Distinct[Keys[k].Bank]);
      if Pinned[k] then
      begin
        Colour[k] := PinBase[Keys[k].Bank];
        Inc(PinBase[Keys[k].Bank]);
        if Colour[k] + 1 > Colours[Keys[k].Bank] then Colours[Keys[k].Bank] := Colour[k] + 1;
      end;
    end;

    SetLength(Order, NKeys);
    for k := 0 to NKeys - 1 do Order[k] := k;
    for k := 1 to NKeys - 1 do              // insertion sort by descending use count
    begin
      si := Order[k]; kk := k - 1;
      while (kk >= 0) and (UseCount[Order[kk]] < UseCount[si]) do
      begin Order[kk + 1] := Order[kk]; Dec(kk); end;
      Order[kk + 1] := si;
    end;

    SetLength(UsedCol, NKeys + 2);
    for ii := 0 to NKeys - 1 do
    begin
      k := Order[ii];
      if Pinned[k] then Continue;
      for kk := 0 to NKeys + 1 do UsedCol[kk] := False;
      for kk := 0 to Adj[k].Count - 1 do
      begin
        si := PtrInt(Adj[k][kk]);
        if (Colour[si] >= 0) and (Colour[si] <= NKeys) then UsedCol[Colour[si]] := True;
      end;
      si := PinBase[Keys[k].Bank];          // never reuse a pinned register's number
      while (si <= NKeys) and UsedCol[si] do Inc(si);
      Colour[k] := si;
      if si + 1 > Colours[Keys[k].Bank] then Colours[Keys[k].Bank] := si + 1;
    end;

    if Map <> nil then
      for k := 0 to NKeys - 1 do
        Map.AddOrSetValue(RegAllocKey(Keys[k].Bank, Keys[k].Idx, Keys[k].Ver), Colour[k]);
    for Bank := Low(TSSARegisterType) to High(TSSARegisterType) do
      FReuseColourCount[Bank] := Colours[Bank];
    Result := True;

    if Report then
    begin
      WriteLn('[RegReuse] blocks=', NB, ' registers=', NKeys, ' fixpoint passes=', pass,
              ' pinned=', PinnedN, ' (live across a call: ', LiveAcrossGosub, ')');
      WriteLn('[RegReuse]   int   : ', Distinct[srtInt], ' distinct -> ', Colours[srtInt], ' needed');
      WriteLn('[RegReuse]   float : ', Distinct[srtFloat], ' distinct -> ', Colours[srtFloat], ' needed');
      WriteLn('[RegReuse]   string: ', Distinct[srtString], ' distinct -> ', Colours[srtString], ' needed');
      if HasGosub then
        WriteLn('[RegReuse]   NOTE: the program calls -- values live across a call are pinned');
      // REGREUSE_DIAG=2: per-block liveness dump. A wrong merge is always a liveness that does not
      // describe the program, and this is what shows WHERE (use/def/in/out per block, plus the
      // successor edges the fixpoint actually followed).
      if GetEnvironmentVariable('REGREUSE_DIAG') = '2' then
        for bi := 0 to NB - 1 do
        begin
          Blk := FProgram.Blocks[bi];
          Write('[RegReuse] b', bi, ' (', Blk.Instructions.Count, ' ins) succ=');
          for si := 0 to Blk.Successors.Count - 1 do
            if BlkIdx.TryGetValue(Pointer(TSSABasicBlock(Blk.Successors[si])), sidx) then
              Write(' b', sidx)
            else
              Write(' <UNMAPPED>');
          Write('  use='); for k := 0 to NKeys - 1 do if BUse[bi][k] then Write(' r', Keys[k].Idx, '_v', Keys[k].Ver);
          Write('  def='); for k := 0 to NKeys - 1 do if BDef[bi][k] then Write(' r', Keys[k].Idx, '_v', Keys[k].Ver);
          Write('  in=');  for k := 0 to NKeys - 1 do if BIn[bi][k]  then Write(' r', Keys[k].Idx, '_v', Keys[k].Ver);
          Write('  out='); for k := 0 to NKeys - 1 do if BOut[bi][k] then Write(' r', Keys[k].Idx, '_v', Keys[k].Ver);
          WriteLn;
        end;
      // Per-key detail: which pre-allocation (bank, index, version) keys ended up SHARING a
      // colour. Without this the only way to see a wrong merge is to read the disassembly and
      // guess - and a wrong merge is exactly what this pass must never produce.
      for Bank := Low(TSSARegisterType) to High(TSSARegisterType) do
        for si := 0 to Colours[Bank] - 1 do
        begin
          kk := 0;
          for k := 0 to NKeys - 1 do
            if (Keys[k].Bank = Bank) and (Colour[k] = si) then Inc(kk);
          if kk > 1 then
          begin
            Write('[RegReuse]   ', SSARegisterTypeToString(Bank), ' colour ', si, ' shared by:');
            for k := 0 to NKeys - 1 do
              if (Keys[k].Bank = Bank) and (Colour[k] = si) then
                Write(' r', Keys[k].Idx, '_v', Keys[k].Ver, '(uses=', UseCount[k], ')');
            WriteLn;
          end;
        end;
    end;

    for k := 0 to NKeys - 1 do Adj[k].Free;
  finally
    KeyMap.Free;
    BlkIdx.Free;
  end;
end;

function TLinearScanAllocator.RunBASICAllocation: Integer;
var
  VarList: TObjectList;
  Colouring: specialize TDictionary<Int64, Integer>;
begin
  // Phase 2: with REGREUSE=1, values with disjoint live ranges share a register number. The
  // colouring is computed on the pre-allocation keys and consumed by AllocateBASICVariables; if
  // the analysis bails (an enormous CFG) the historic one-number-per-value path runs unchanged.
  Colouring := nil;
  if ReuseMergeEnabled or ReuseDiagEnabled then
  begin
    Colouring := specialize TDictionary<Int64, Integer>.Create;
    if not ComputeReuseColouring(Colouring, ReuseDiagEnabled) then
      FreeAndNil(Colouring)
    else if not ReuseMergeEnabled then
      FreeAndNil(Colouring);       // diagnostic only: report, then allocate as before
  end;
  FReuseColouring := Colouring;
  // Tell the downstream engines the merge really ran (not merely that the gate is set: the
  // analysis bails on enormous CFGs). The AOT arbitrates its own dynamic allocator against it.
  FProgram.RegisterMergeApplied := Colouring <> nil;
  try

  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
    WriteLn('[RegAlloc] Computing BASIC variable usage statistics...');
  {$ENDIF}

  // Step 1: Analyze all BASIC variables and count usage
  ComputeVariableUsage(VarList);

  try
    // Step 2: Allocate physical registers to most-used variables
    AllocateBASICVariables(VarList);

    // Step 3: Rewrite program with physical register assignments
    RewriteProgramBASIC(VarList);

    // Count spills (variables without physical registers)
    Result := FSpillCount;
    {$IFDEF DEBUG_REGALLOC}
    if DebugRegAlloc then
      WriteLn('[RegAlloc] BASIC allocation complete: ', VarList.Count, ' variables, ', FSpillCount, ' spills');
    {$ENDIF}
  finally
    VarList.Free;
  end;

  finally
    FReuseColouring := nil;
    Colouring.Free;
  end;
end;

procedure TLinearScanAllocator.ComputeVariableUsage(out VarList: TObjectList);
var
  i, j, k: Integer;
  VarName: string;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  VarInfo: TVariableInfo;
  VarIndex: TRegInfoMap;

  function FindOrCreateVarInfo(RegType: TSSARegisterType; RegIdx, RegVer: Integer): TVariableInfo;
  begin
    // Packed Int64 key: this lookup runs for every operand of every instruction, and the
    // historical three-IntToStr string key dominated the whole pass.
    if VarIndex.TryGetValue(RegAllocKey(RegType, RegIdx, RegVer), Result) then
      Exit;

    if RegVer = 0 then
      VarName := 'r' + IntToStr(RegIdx)
    else
      VarName := 'r' + IntToStr(RegIdx) + '_v' + IntToStr(RegVer);

    Result := TVariableInfo.Create(VarName, RegType, RegIdx, RegVer);
    VarList.Add(Result);
    VarIndex.Add(RegAllocKey(RegType, RegIdx, RegVer), Result);
  end;

  procedure CountUsage(const Val: TSSAValue); inline;
  var
    Info: TVariableInfo;
  begin
    if Val.Kind <> svkRegister then Exit;
    Info := FindOrCreateVarInfo(Val.RegType, Val.RegIndex, Val.Version);
    if Assigned(Info) then
      Inc(Info.UsageCount);
  end;

begin
  VarList := TObjectList.Create(True);
  VarIndex := TRegInfoMap.Create;
  try
    for i := 0 to FProgram.Blocks.Count - 1 do
    begin
      Block := FProgram.Blocks[i];
      for j := 0 to Block.Instructions.Count - 1 do
      begin
        Instr := Block.Instructions[j];
        CountUsage(Instr.Src1);
        CountUsage(Instr.Src2);
        CountUsage(Instr.Src3);
        CountUsage(Instr.Dest);
        for k := 0 to High(Instr.PhiSources) do
          CountUsage(Instr.PhiSources[k].Value);
      end;
    end;
  finally
    VarIndex.Free;
  end;

  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
    WriteLn('[RegAlloc] Found ', VarList.Count, ' variables in program');
  {$ENDIF}
end;

procedure TLinearScanAllocator.AllocateBASICVariables(VarList: TObjectList);
var
  i: Integer;
  VarInfo: TVariableInfo;
  PhysReg: Integer;
  NextSpillSlot: array[TSSARegisterType] of Integer;
  RegType: TSSARegisterType;
begin
  // Sort variables by usage count (most used first)
  VarList.Sort(@CompareVariableUsage);

  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
    WriteLn('[RegAlloc] Allocating physical registers (by usage frequency):');
  {$ENDIF}

  // Initialize spill slots to start AFTER physical registers
  // This prevents collision between spilled vars and physical regs
  NextSpillSlot[srtInt] := MAX_INT_REGS;       // 32+
  NextSpillSlot[srtFloat] := MAX_FLOAT_REGS;   // 32+
  NextSpillSlot[srtString] := MAX_STRING_REGS; // 16+

  // Allocate physical registers to variables in order of usage
  FSpillCount := 0;
  for i := 0 to VarList.Count - 1 do
  begin
    VarInfo := TVariableInfo(VarList[i]);

    // Phase 2 (REGREUSE=1): the interference colouring already decided which values may share a
    // number. Take it verbatim -- values with disjoint live ranges land on the same register, and
    // the count collapses to what the program actually needs at once (n-body float: 247 -> 8).
    if FReuseColouring <> nil then
    begin
      if FReuseColouring.TryGetValue(RegAllocKey(VarInfo.RegType, VarInfo.VirtualReg, VarInfo.Version),
                                     PhysReg) then
      begin
        VarInfo.PhysicalReg := PhysReg;
        Continue;
      end;
      // Not in the colouring (the analysis never saw it): give it a private number ABOVE every
      // colour of its bank. Falling through to GetFreeRegister would hand out 0..31 again and
      // collide with the colours -- two values sharing a register with no interference analysis
      // behind it, which is the whole failure mode this pass exists to avoid.
      VarInfo.PhysicalReg := FReuseColourCount[VarInfo.RegType];
      Inc(FReuseColourCount[VarInfo.RegType]);
      Continue;
    end;

    // Try to get a free physical register for this type
    PhysReg := GetFreeRegister(VarInfo.RegType);

    if PhysReg >= 0 then
    begin
      VarInfo.PhysicalReg := PhysReg;
      {$IFDEF DEBUG_REGALLOC}
      if DebugRegAlloc then
        WriteLn('[RegAlloc]   ', VarInfo.VarName, ' (type=',
                IntToStr(Ord(VarInfo.RegType)), ', idx=', VarInfo.VirtualReg,
                ', ver=', VarInfo.Version,
                ') → R', PhysReg, ' (usage: ', VarInfo.UsageCount, ')');
      {$ENDIF}
    end
    else
    begin
      // No free register - assign spill slot (starting after physical regs)
      // CRITICAL: Spilled variables must use indices ABOVE physical register range
      // to prevent collision when BytecodeCompiler maps them directly
      VarInfo.PhysicalReg := NextSpillSlot[VarInfo.RegType];
      Inc(NextSpillSlot[VarInfo.RegType]);
      Inc(FSpillCount);
      {$IFDEF DEBUG_REGALLOC}
      if DebugRegAlloc then
        WriteLn('[RegAlloc]   ', VarInfo.VarName, ' (',
                IntToStr(Ord(VarInfo.RegType)), ':', VarInfo.VirtualReg,
                ') → R', VarInfo.PhysicalReg, ' (spilled, usage: ', VarInfo.UsageCount, ')');
      {$ENDIF}
    end;
  end;
end;

procedure TLinearScanAllocator.RewriteProgramBASIC(VarList: TObjectList);
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  i, j, k, d: Integer;
  VarInfo: TVariableInfo;
  ArrInfo: TSSAArrayInfo;
  NewDimRegs: array of Integer;
  NewDimRegTypes: array of TSSARegisterType;
  NewLbRegs: array of Integer;
  VarIndex: TRegInfoMap;

  function RewriteValueBASIC(const Val: TSSAValue): TSSAValue;
  var
    Info: TVariableInfo;
  begin
    Result := Val;
    if Val.Kind <> svkRegister then Exit;

    if VarIndex.TryGetValue(RegAllocKey(Val.RegType, Val.RegIndex, Val.Version), Info) then
    begin
      Result.RegIndex := Info.PhysicalReg;
      Result.Version := 0;
    end;
  end;

  function RewriteDimRegister(VirtReg: Integer; RegType: TSSARegisterType): Integer;
  var
    Info: TVariableInfo;
  begin
    Result := VirtReg;
    if VarIndex.TryGetValue(RegAllocKey(RegType, VirtReg, 0), Info) then
      Result := Info.PhysicalReg;
  end;

begin
  // Build hash index for O(1) lookup (packed Int64 keys - see ComputeVariableUsage)
  VarIndex := TRegInfoMap.Create;
  try
    for i := 0 to VarList.Count - 1 do
    begin
      VarInfo := TVariableInfo(VarList[i]);
      VarIndex.Add(RegAllocKey(VarInfo.RegType, VarInfo.VirtualReg, VarInfo.Version), VarInfo);
    end;

    for i := 0 to FProgram.Blocks.Count - 1 do
    begin
      Block := FProgram.Blocks[i];
      for j := 0 to Block.Instructions.Count - 1 do
      begin
        Instr := Block.Instructions[j];
        Instr.Src1 := RewriteValueBASIC(Instr.Src1);
        Instr.Src2 := RewriteValueBASIC(Instr.Src2);
        Instr.Src3 := RewriteValueBASIC(Instr.Src3);
        Instr.Dest := RewriteValueBASIC(Instr.Dest);
        for k := 0 to High(Instr.PhiSources) do
          Instr.PhiSources[k].Value := RewriteValueBASIC(Instr.PhiSources[k].Value);
      end;
    end;

    // Rewrite DimRegisters in array declarations
    for i := 0 to FProgram.GetArrayCount - 1 do
    begin
      ArrInfo := FProgram.GetArray(i);
      if Length(ArrInfo.DimRegisters) > 0 then
      begin
        SetLength(NewDimRegs, Length(ArrInfo.DimRegisters));
        SetLength(NewDimRegTypes, Length(ArrInfo.DimRegTypes));

        for d := 0 to High(ArrInfo.DimRegisters) do
        begin
          if ArrInfo.DimRegisters[d] >= 0 then
          begin
            NewDimRegs[d] := RewriteDimRegister(ArrInfo.DimRegisters[d], ArrInfo.DimRegTypes[d]);
            NewDimRegTypes[d] := ArrInfo.DimRegTypes[d];
          end
          else
          begin
            NewDimRegs[d] := ArrInfo.DimRegisters[d];
            NewDimRegTypes[d] := ArrInfo.DimRegTypes[d];
          end;
        end;

        FProgram.SetArrayDimRegisters(i, NewDimRegs, NewDimRegTypes);
      end;

      // Rewrite runtime lower-bound registers the same way (they are all int registers).
      if Length(ArrInfo.LowerBoundRegisters) > 0 then
      begin
        SetLength(NewLbRegs, Length(ArrInfo.LowerBoundRegisters));
        for d := 0 to High(ArrInfo.LowerBoundRegisters) do
        begin
          if ArrInfo.LowerBoundRegisters[d] >= 0 then
            NewLbRegs[d] := RewriteDimRegister(ArrInfo.LowerBoundRegisters[d], srtInt)
          else
            NewLbRegs[d] := ArrInfo.LowerBoundRegisters[d];
        end;
        FProgram.SetArrayLowerBoundRegisters(i, NewLbRegs);
      end;
    end;
  finally
    VarIndex.Free;
  end;
end;

end.
