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
{$inline on}
{$I OptimizationFlags.inc}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, Contnrs, SedaiSSATypes;

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
    UsageCount: Integer;       // Number of uses in the program
    PhysicalReg: Integer;      // Assigned physical register (-1 = not allocated)
    constructor Create(const AName: string; ARegType: TSSARegisterType; AVirtReg: Integer);
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

constructor TVariableInfo.Create(const AName: string; ARegType: TSSARegisterType; AVirtReg: Integer);
begin
  inherited Create;
  VarName := AName;
  RegType := ARegType;
  VirtualReg := AVirtReg;
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

  // DUAL MODE: Choose allocation strategy based on GlobalVariableSemantics
  if FProgram.GlobalVariableSemantics then
  begin
    // BASIC mode: Static variable allocation (no SSA versioning required)
    {$IFDEF DEBUG_REGALLOC}
    if DebugRegAlloc then
      WriteLn('[RegAlloc] Running BASIC Variable Allocation (GlobalVariableSemantics=True)...');
    {$ENDIF}
    Result := RunBASICAllocation;
    Exit;
  end;

  // SSA mode: Linear Scan allocation (requires SSA versioning)
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
  i, j, Position: Integer;
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
  i, j: Integer;
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

function TLinearScanAllocator.RunBASICAllocation: Integer;
var
  VarList: TObjectList;
begin
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
  VarName, VarKey: string;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  VarInfo: TVariableInfo;
  RegType: TSSARegisterType;
  RegIndex: Integer;

  function FindOrCreateVarInfo(const Key: string): TVariableInfo;
  var
    idx: Integer;
    Parts: TStringList;
    RegTypeInt: Integer;
  begin
    // Search for existing VarInfo
    for idx := 0 to VarList.Count - 1 do
    begin
      VarInfo := TVariableInfo(VarList[idx]);
      if (IntToStr(Ord(VarInfo.RegType)) + ':' + IntToStr(VarInfo.VirtualReg)) = Key then
        Exit(VarInfo);
    end;

    // Create new VarInfo
    Parts := TStringList.Create;
    try
      Parts.Delimiter := ':';
      Parts.DelimitedText := Key;
      if Parts.Count = 2 then
      begin
        RegTypeInt := StrToInt(Parts[0]);
        RegIndex := StrToInt(Parts[1]);
        RegType := TSSARegisterType(RegTypeInt);

        // CRITICAL FIX: DO NOT use VarRegMap - it's obsolete after optimizations
        // (CopyProp, ConstProp, etc. rename registers but don't update VarRegMap)
        // Instead, just treat each virtual register as a separate variable.
        // The register index IS the variable identity in BASIC mode (Version=0).
        VarName := 'r' + IntToStr(RegIndex);  // Simple name: r0, r1, r2, etc.

        Result := TVariableInfo.Create(VarName, RegType, RegIndex);
        VarList.Add(Result);
      end
      else
        Result := nil;
    finally
      Parts.Free;
    end;
  end;

  procedure CountUsage(const Val: TSSAValue);
  var
    Key: string;
    Info: TVariableInfo;
  begin
    if Val.Kind <> svkRegister then Exit;

    Key := IntToStr(Ord(Val.RegType)) + ':' + IntToStr(Val.RegIndex);
    Info := FindOrCreateVarInfo(Key);
    if Assigned(Info) then
      Inc(Info.UsageCount);
  end;

begin
  VarList := TObjectList.Create(True);  // Owns TVariableInfo objects

  // Scan all instructions and count register usage
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      // Count source operand usage
      CountUsage(Instr.Src1);
      CountUsage(Instr.Src2);
      CountUsage(Instr.Src3);

      // Count destination usage (definitions also count as "usage")
      CountUsage(Instr.Dest);
    end;
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
        WriteLn('[RegAlloc]   ', VarInfo.VarName, ' (',
                IntToStr(Ord(VarInfo.RegType)), ':', VarInfo.VirtualReg,
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
  FoundReg: Boolean;

  function RewriteValueBASIC(const Val: TSSAValue): TSSAValue;
  var
    idx: Integer;
    Found: Boolean;
  begin
    Result := Val;

    if Val.Kind <> svkRegister then Exit;

    Found := False;
    // Find variable info for this virtual register
    for idx := 0 to VarList.Count - 1 do
    begin
      VarInfo := TVariableInfo(VarList[idx]);
      if (VarInfo.RegType = Val.RegType) and (VarInfo.VirtualReg = Val.RegIndex) then
      begin
        Found := True;
        // All variables now have PhysicalReg assigned (either physical reg 0-31
        // or spill slot 32+), so always rewrite
        Result.RegIndex := VarInfo.PhysicalReg;
        Result.Version := 0;  // Physical registers have no version
        Exit;
      end;
    end;

    // WARNING: Virtual register not found in allocation table!
    {$IFDEF DEBUG_REGALLOC}
    if not Found and DebugRegAlloc then
    begin
      WriteLn('[RegAlloc] WARNING: r', Val.RegIndex, '_v', Val.Version,
              ' type=', Ord(Val.RegType), ' NOT FOUND in allocation table!');
    end;
    {$ENDIF}
  end;

  function RewriteDimRegister(VirtReg: Integer; RegType: TSSARegisterType): Integer;
  var
    idx: Integer;
  begin
    Result := VirtReg;  // Default: keep original if not found
    for idx := 0 to VarList.Count - 1 do
    begin
      VarInfo := TVariableInfo(VarList[idx]);
      if (VarInfo.RegType = RegType) and (VarInfo.VirtualReg = VirtReg) then
      begin
        Result := VarInfo.PhysicalReg;
        Exit;
      end;
    end;
    {$IFDEF DEBUG_REGALLOC}
    if DebugRegAlloc then
      WriteLn('[RegAlloc] WARNING: DimRegister r', VirtReg, ' type=', Ord(RegType), ' NOT FOUND!');
    {$ENDIF}
  end;

begin
  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
    WriteLn('[RegAlloc] Rewriting program with physical registers...');
  {$ENDIF}

  // Rewrite instructions in all blocks
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      // Rewrite all register references
      Instr.Src1 := RewriteValueBASIC(Instr.Src1);
      Instr.Src2 := RewriteValueBASIC(Instr.Src2);
      Instr.Src3 := RewriteValueBASIC(Instr.Src3);
      Instr.Dest := RewriteValueBASIC(Instr.Dest);
    end;
  end;

  // CRITICAL: Rewrite DimRegisters in array declarations
  // Arrays with variable dimensions (DIM A(N%)) store the register index
  // that holds the dimension value. These must be rewritten to physical registers.
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
          {$IFDEF DEBUG_REGALLOC}
          if DebugRegAlloc then
            WriteLn('[RegAlloc] Array ', ArrInfo.Name, ' dim[', d, ']: r',
                    ArrInfo.DimRegisters[d], ' → R', NewDimRegs[d]);
          {$ENDIF}
        end
        else
        begin
          NewDimRegs[d] := ArrInfo.DimRegisters[d];
          NewDimRegTypes[d] := ArrInfo.DimRegTypes[d];
        end;
      end;

      // Update the array info with new physical registers
      FProgram.SetArrayDimRegisters(i, NewDimRegs, NewDimRegTypes);
    end;
  end;
end;

end.
