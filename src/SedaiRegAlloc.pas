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

      Set REGREUSE_DIAG=1 to print the report; unset it costs one getenv per compilation. ------ }
    function ReuseDiagEnabled: Boolean;
    procedure ReportReusePotential;

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
begin
  Result := GetEnvironmentVariable('REGREUSE_DIAG') = '1';
end;

procedure TLinearScanAllocator.ReportReusePotential;
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
  Changed, HasGosub: Boolean;
  BUse, BDef, BIn, BOut: array of array of Boolean;   // [block][key]
  Live: array of Boolean;                             // mid-block live set
  Pinned: array of Boolean;                           // never merge (see notes above)
  Adj: array of TFPList;                              // interference graph (key -> neighbour keys)
  Colour: array of Integer;
  UsedCol: array of Boolean;
  Distinct, Colours: array[TSSARegisterType] of Integer;
  PinnedN, LiveAcrossGosub, sidx: Integer;
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
    for bi := 0 to NB - 1 do
    begin
      Blk := FProgram.Blocks[bi];
      for ii := 0 to Blk.Instructions.Count - 1 do
      begin
        Ins := Blk.Instructions[ii];
        KeyOf(Ins.Dest); KeyOf(Ins.Src1); KeyOf(Ins.Src2); KeyOf(Ins.Src3);
        for k := 0 to High(Ins.PhiSources) do KeyOf(Ins.PhiSources[k].Value);
        if (Ins.OpCode = ssaCall) or (Ins.OpCode = ssaReturn) then HasGosub := True;   // GOSUB lowers to ssaCall
      end;
    end;
    if NKeys = 0 then Exit;
    // Guard: the bitsets are blocks x keys. test_cfg_large is 100k blocks; refuse rather than
    // allocate gigabytes for a diagnostic.
    if (Int64(NB) * NKeys) > 40 * 1000 * 1000 then
    begin
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
        if Ins.Dest.Kind = svkRegister then NoteDef(Ins.Dest, bi);
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
        if Ins.Dest.Kind = svkRegister then DefKey := KeyOf(Ins.Dest);
        if DefKey >= 0 then
        begin
          for k := 0 to NKeys - 1 do
            if Live[k] then Interfere(DefKey, k);
          Live[DefKey] := False;
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

    // Greedy colouring per bank, pinned registers first (each takes a colour of its own).
    for k := 0 to NKeys - 1 do Colour[k] := -1;
    for Bank := Low(TSSARegisterType) to High(TSSARegisterType) do
    begin Distinct[Bank] := 0; Colours[Bank] := 0; end;
    SetLength(UsedCol, NKeys + 1);
    for k := 0 to NKeys - 1 do
    begin
      Inc(Distinct[Keys[k].Bank]);
      if Pinned[k] then
      begin
        Colour[k] := Colours[Keys[k].Bank];
        Inc(Colours[Keys[k].Bank]);
        Continue;
      end;
      for kk := 0 to NKeys do UsedCol[kk] := False;
      for kk := 0 to Adj[k].Count - 1 do
      begin
        si := PtrInt(Adj[k][kk]);
        if (Colour[si] >= 0) and (Colour[si] <= NKeys) then UsedCol[Colour[si]] := True;
      end;
      si := 0;
      while (si <= NKeys) and UsedCol[si] do Inc(si);
      Colour[k] := si;
      if si + 1 > Colours[Keys[k].Bank] then Colours[Keys[k].Bank] := si + 1;
    end;

    WriteLn('[RegReuse] blocks=', NB, ' registers=', NKeys, ' fixpoint passes=', pass,
            ' pinned=', PinnedN, ' (live across GOSUB: ', LiveAcrossGosub, ')',
            {$IFDEF DEBUG_REGALLOC}'' {$ELSE}'' {$ENDIF});
    WriteLn('[RegReuse]   int   : ', Distinct[srtInt], ' distinct -> ', Colours[srtInt], ' needed');
    WriteLn('[RegReuse]   float : ', Distinct[srtFloat], ' distinct -> ', Colours[srtFloat], ' needed');
    WriteLn('[RegReuse]   string: ', Distinct[srtString], ' distinct -> ', Colours[srtString], ' needed');
    if HasGosub then
      WriteLn('[RegReuse]   NOTE: the program uses GOSUB -- values live across one are pinned');

    for k := 0 to NKeys - 1 do Adj[k].Free;
  finally
    KeyMap.Free;
    BlkIdx.Free;
  end;
end;

function TLinearScanAllocator.RunBASICAllocation: Integer;
var
  VarList: TObjectList;
begin
  if ReuseDiagEnabled then
    ReportReusePotential;

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
