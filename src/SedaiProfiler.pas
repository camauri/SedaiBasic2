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
unit SedaiProfiler;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

{ VM Profiler for SedaiBasic2

  Provides two profiling modes:
  1. Sampling Profiler - Low overhead (~1-5%), interrupts VM at fixed intervals
  2. Instrumentation Profiler - Higher overhead (~10-30%), counts every instruction

  Features:
  - Per-opcode execution counts and timing
  - Per-superinstruction tracking
  - Basic block execution counts
  - Hot path detection
  - Array access profiling
  - Export to JSON and Flame Graph format (.folded)

  Usage:
    // Sampling mode (default)
    Profiler := TProfiler.Create(pmSampling);
    Profiler.SampleIntervalUs := 100;  // Sample every 100 microseconds
    VM.SetProfiler(Profiler);
    VM.Run;
    Profiler.PrintReport;

    // Instrumentation mode
    Profiler := TProfiler.Create(pmInstrumentation);
    VM.SetProfiler(Profiler);
    VM.Run;
    Profiler.ExportJSON('profile.json');
}

interface

uses
  Classes, SysUtils, Math, TypInfo, SedaiBytecodeTypes, SedaiSSATypes;

const
  { Opcode groups for 2-byte encoding
    Group = OpCode shr 8, SubOp = OpCode and $FF
    Group 0 (0x00xx): Core VM operations (max ~60 opcodes)
    Group 1 (0x01xx): String operations (max ~15)
    Group 2 (0x02xx): Math functions (max ~15)
    Group 3 (0x03xx): Array operations (max ~10)
    Group 4 (0x04xx): I/O operations (max ~20)
    Group 5 (0x05xx): Special variables (max ~10)
    Group 10 (0x0Axx): Graphics (max ~20)
    Group 11 (0x0Bxx): Sound (max ~10)
    Groups 200-255 (0xC8xx+): Superinstructions (max ~100) }
  MAX_OPCODE_GROUP = 255;       // Max group number (fits in high byte)
  MAX_OPCODES_PER_GROUP = 100;  // Max opcodes per group (fits in low byte)

  { Default profiler settings }
  DEFAULT_SAMPLE_INTERVAL_US = 100;    // 100 microseconds
  DEFAULT_PC_HISTORY_SIZE = 10000;     // Track last 10K PC samples
  DEFAULT_HOTSPOT_THRESHOLD = 0.01;    // 1% of total time = hotspot

type
  { Profiler mode }
  TProfilerMode = (
    pmDisabled,           // No profiling (zero overhead)
    pmSampling,           // Sample-based profiling (low overhead)
    pmInstrumentation,    // Full instrumentation (accurate but slow)
    pmHybrid              // Sampling + selective instrumentation
  );

  { Profile detail level }
  TProfileDetailLevel = (
    pdlBasic,             // Just opcode counts
    pdlStandard,          // Opcode + superinstruction + timing
    pdlFull               // Everything including array access patterns
  );

  { Per-opcode statistics }
  TOpcodeStats = record
    ExecutionCount: Int64;      // Number of times executed
    TotalCycles: Int64;         // Total CPU cycles spent (estimated)
    MinCycles: Int64;           // Minimum cycles for single execution
    MaxCycles: Int64;           // Maximum cycles for single execution
    SampleCount: Int64;         // Number of samples hitting this opcode
  end;

  { Per-basic-block statistics }
  TBasicBlockStats = record
    BlockLabel: string;         // Basic block label from SSA
    FirstPC: Integer;           // First bytecode instruction PC
    LastPC: Integer;            // Last bytecode instruction PC
    ExecutionCount: Int64;      // Number of times block was entered
    TotalInstructions: Int64;   // Total instructions executed in block
    SampleCount: Int64;         // Number of samples in this block
    IsLoopHeader: Boolean;      // True if this is a loop header
    LoopIterations: Int64;      // Number of loop iterations (if loop header)
  end;

  { Array access statistics }
  TArrayAccessStats = record
    ArrayName: string;          // Array variable name
    ArrayIndex: Integer;        // Index in VM array table
    ElementType: TSSARegisterType;
    ReadCount: Int64;           // Number of reads
    WriteCount: Int64;          // Number of writes
  end;

  { PC sample for sampling profiler }
  TPCSample = record
    PC: Integer;                // Program counter
    OpCode: Word;               // Opcode at PC (2-byte encoding)
    Timestamp: Int64;           // High-precision timestamp
  end;

  { Hot path segment }
  THotPathSegment = record
    StartPC: Integer;
    EndPC: Integer;
    ExecutionCount: Int64;
    Percentage: Double;         // Percentage of total execution time
  end;

  { Main Profiler class }
  TProfiler = class
  private
    FMode: TProfilerMode;
    FDetailLevel: TProfileDetailLevel;
    FEnabled: Boolean;

    { Timing }
    FStartTime: Int64;
    FEndTime: Int64;
    FTotalInstructions: Int64;

    { Per-opcode statistics (indexed by [group][subop])
      Group 0-11: Core to Sound, Group 200+: Superinstructions }
    FOpcodeStats: array[0..MAX_OPCODE_GROUP, 0..MAX_OPCODES_PER_GROUP-1] of TOpcodeStats;

    { Superinstruction-specific tracking }
    FSuperinstrCount: Integer;  // Total superinstructions executed
    FSuperinstrSaved: Integer;  // Instructions saved by fusion

    { Basic block tracking (dynamic arrays) }
    FBasicBlockStats: array of TBasicBlockStats;
    FBasicBlockCount: Integer;
    FCurrentBlockIndex: Integer;
    FPCToBlockMap: array of Integer;  // Maps PC → block index (-1 if none)

    { Array access tracking (dynamic array) }
    FArrayStats: array of TArrayAccessStats;
    FArrayStatsCount: Integer;

    { Sampling profiler data }
    FSampleIntervalUs: Integer;     // Sample interval in microseconds
    FPCSamples: array of TPCSample; // Circular buffer of PC samples
    FPCSampleIndex: Integer;        // Current position in buffer
    FPCSampleCount: Int64;          // Total samples taken
    FLastSampleTime: Int64;         // Time of last sample

    { Hot path analysis (dynamic array) }
    FHotPaths: array of THotPathSegment;
    FHotPathCount: Integer;
    FHotspotThreshold: Double;

    { Helper methods }
    function GetCurrentTimestamp: Int64;
    function EstimateCycles(OpCode: Word): Integer;
    procedure UpdateOpcodeStats(OpCode: Word; Cycles: Int64);
    function FindBasicBlockForPC(PC: Integer): Integer;
    procedure AnalyzeHotPaths;
    function GetOpcodeGroup(OpCode: Word): Word; inline;
    function GetOpcodeSubOp(OpCode: Word): Word; inline;

  public
    constructor Create(AMode: TProfilerMode = pmSampling);
    destructor Destroy; override;

    { Control }
    procedure Start;
    procedure Stop;
    procedure Reset;
    procedure SetEnabled(Value: Boolean);

    { Instrumentation hooks (called by VM) }
    procedure BeforeInstruction(PC: Integer; OpCode: Word);
    procedure AfterInstruction(PC: Integer; OpCode: Word);
    procedure OnSuperinstruction(OpCode: Word; InstructionsSaved: Integer);
    procedure OnArrayAccess(ArrayIdx: Integer; IsWrite: Boolean; LinearIndex: Integer);
    procedure OnBasicBlockEntry(BlockIndex: Integer);

    { Sampling hook (called periodically by VM or timer) }
    procedure TakeSample(PC: Integer; OpCode: Word);
    function ShouldSample: Boolean;

    { Basic block mapping (called during compilation) }
    procedure SetPCToBlockMapping(PC: Integer; BlockIndex: Integer);
    procedure AddBasicBlock(const BlockLabel: string; FirstPC, LastPC: Integer; IsLoopHeader: Boolean);
    procedure SetArrayInfo(ArrayIdx: Integer; const Name: string; ElementType: TSSARegisterType);

    { Analysis }
    function GetTotalExecutionTime: Double;  // In seconds
    function GetInstructionsPerSecond: Double;
    function GetOpcodePercentage(OpCode: Word): Double;
    function GetTopOpcodes(Count: Integer): string;
    function GetTopBasicBlocks(Count: Integer): string;
    function GetHotPathCount: Integer;
    function GetHotPath(Index: Integer): THotPathSegment;

    { Output }
    procedure PrintReport;
    procedure PrintOpcodeTable;
    procedure PrintBasicBlockTable;
    procedure PrintSuperinstructionStats;
    procedure PrintArrayAccessStats;

    { Export }
    procedure ExportJSON(const FileName: string);
    procedure ExportFoldedFlameGraph(const FileName: string);
    procedure ExportCSV(const FileName: string);

    { Properties }
    property Mode: TProfilerMode read FMode write FMode;
    property DetailLevel: TProfileDetailLevel read FDetailLevel write FDetailLevel;
    property Enabled: Boolean read FEnabled;
    property SampleIntervalUs: Integer read FSampleIntervalUs write FSampleIntervalUs;
    property HotspotThreshold: Double read FHotspotThreshold write FHotspotThreshold;
    property TotalInstructions: Int64 read FTotalInstructions;
    property SuperinstructionsExecuted: Integer read FSuperinstrCount;
    property InstructionsSavedByFusion: Integer read FSuperinstrSaved;
  end;

{ Global profiler instance (optional, for easy access) }
var
  GlobalProfiler: TProfiler = nil;

implementation

{$IFDEF WINDOWS}
uses Windows;
{$ENDIF}

{ Opcode group/subop extraction }

function TProfiler.GetOpcodeGroup(OpCode: Word): Word; inline;
begin
  Result := OpCode shr 8;  // High byte is the group number
end;

function TProfiler.GetOpcodeSubOp(OpCode: Word): Word; inline;
begin
  Result := OpCode and $FF;  // Low byte is the sub-opcode
end;

{ High-precision timing }

function TProfiler.GetCurrentTimestamp: Int64;
{$IFDEF WINDOWS}
var
  Counter: Int64;
begin
  QueryPerformanceCounter(Counter);
  Result := Counter;
end;
{$ELSE}
begin
  Result := GetTickCount64 * 1000;  // Convert to microseconds (approximate)
end;
{$ENDIF}

{ Estimate CPU cycles for an opcode (rough approximation) }
function TProfiler.EstimateCycles(OpCode: Word): Integer;
var
  Group, SubOp: Word;
begin
  // These are rough estimates for a typical modern CPU
  Group := GetOpcodeGroup(OpCode);
  SubOp := GetOpcodeSubOp(OpCode);

  case Group of
    0: // Core VM operations (Group 0)
      case OpCode of
        // Load constants - very fast
        bcLoadConstInt, bcLoadConstFloat: Result := 1;
        bcLoadConstString: Result := 3;

        // Copy - very fast
        bcCopyInt, bcCopyFloat: Result := 1;
        bcCopyString: Result := 5;

        // Integer arithmetic - fast
        bcAddInt, bcSubInt, bcNegInt: Result := 2;
        bcMulInt: Result := 3;
        bcDivInt, bcModInt: Result := 20;  // Division is slow

        // Float arithmetic - moderate
        bcAddFloat, bcSubFloat, bcNegFloat: Result := 3;
        bcMulFloat: Result := 4;
        bcDivFloat: Result := 15;
        bcPowFloat: Result := 30;

        // Type conversions
        bcIntToFloat, bcFloatToInt: Result := 2;

        // Comparisons - fast
        bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpGtInt, bcCmpLeInt, bcCmpGeInt: Result := 2;
        bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpGtFloat, bcCmpLeFloat, bcCmpGeFloat: Result := 3;
        bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString: Result := 10;

        // Control flow
        bcJump: Result := 2;
        bcJumpIfZero, bcJumpIfNotZero: Result := 3;
        bcCall, bcReturn: Result := 5;

        // System
        bcEnd, bcStop: Result := 1;
        bcNop: Result := 0;
      else
        Result := 5;
      end;

    1: // String operations (Group 1)
      case SubOp of
        0: Result := 10;  // StrConcat
        1: Result := 2;   // StrLen
        2, 3, 4: Result := 8;  // Left, Right, Mid
        5, 6: Result := 3;     // Asc, Chr
        7: Result := 5;   // Str
        8: Result := 10;  // Val
        9: Result := 5;   // Hex
        10: Result := 15; // Instr
      else
        Result := 5;
      end;

    2: // Math functions (Group 2)
      case SubOp of
        0, 1, 2, 3: Result := 50;  // Sin, Cos, Tan, Atn
        4, 5: Result := 40;        // Log, Exp
        6: Result := 20;           // Sqr
        7, 8, 9: Result := 3;      // Abs, Sgn, Int
        10: Result := 10;          // Rnd
      else
        Result := 5;
      end;

    3: // Array operations (Group 3)
      case SubOp of
        0, 1, 3, 4, 5: Result := 8;   // Load operations
        6, 7, 8: Result := 10;        // Store operations
        2: Result := 100;             // Dim (allocation)
      else
        Result := 8;
      end;

    4: // I/O operations (Group 4) - very slow (system call)
      case SubOp of
        0..10: Result := 1000;   // Print operations
        11..14: Result := 10000; // Input operations
      else
        Result := 1000;
      end;

    5: // Special variables (Group 5)
      Result := 10;  // Time operations

    10: // Graphics (Group 10)
      Result := 100;  // Graphics operations are moderately slow

    11: // Sound (Group 11)
      Result := 50;  // Sound operations

    200..255: // Superinstructions (Groups 200+)
      case SubOp of
        0..5: Result := 4;    // Fused compare-and-branch (Int)
        10..15: Result := 5;  // Fused compare-and-branch (Float)
        20..22: Result := 3;  // Fused arithmetic-to-dest (Int)
        30..33: Result := 4;  // Fused arithmetic-to-dest (Float)
        40..42: Result := 3;  // Fused constant arithmetic (Int)
        50..53: Result := 4;  // Fused constant arithmetic (Float)
        60..61: Result := 3;  // Fused compare-zero-branch (Int)
        70..71: Result := 4;  // Fused compare-zero-branch (Float)
        80..82: Result := 8;  // Fused array-store-const
      else
        Result := 5;
      end;

  else
    Result := 5;  // Unknown group
  end;
end;

{ Constructor }
constructor TProfiler.Create(AMode: TProfilerMode);
var
  g, s: Integer;
begin
  inherited Create;
  FMode := AMode;
  FDetailLevel := pdlStandard;
  FEnabled := False;
  FSampleIntervalUs := DEFAULT_SAMPLE_INTERVAL_US;
  FHotspotThreshold := DEFAULT_HOTSPOT_THRESHOLD;

  // Initialize opcode stats for all groups and subops
  for g := 0 to MAX_OPCODE_GROUP do
    for s := 0 to MAX_OPCODES_PER_GROUP - 1 do
    begin
      FOpcodeStats[g, s].ExecutionCount := 0;
      FOpcodeStats[g, s].TotalCycles := 0;
      FOpcodeStats[g, s].MinCycles := High(Int64);
      FOpcodeStats[g, s].MaxCycles := 0;
      FOpcodeStats[g, s].SampleCount := 0;
    end;

  // Initialize dynamic arrays
  FBasicBlockCount := 0;
  SetLength(FBasicBlockStats, 0);

  FArrayStatsCount := 0;
  SetLength(FArrayStats, 0);

  FHotPathCount := 0;
  SetLength(FHotPaths, 0);

  // Initialize sampling buffer
  SetLength(FPCSamples, DEFAULT_PC_HISTORY_SIZE);
  FPCSampleIndex := 0;
  FPCSampleCount := 0;

  FCurrentBlockIndex := -1;
  FSuperinstrCount := 0;
  FSuperinstrSaved := 0;
  FTotalInstructions := 0;
end;

destructor TProfiler.Destroy;
begin
  SetLength(FBasicBlockStats, 0);
  SetLength(FArrayStats, 0);
  SetLength(FHotPaths, 0);
  SetLength(FPCSamples, 0);
  SetLength(FPCToBlockMap, 0);
  inherited Destroy;
end;

{ Control methods }

procedure TProfiler.Start;
begin
  FEnabled := True;
  FStartTime := GetCurrentTimestamp;
  FLastSampleTime := FStartTime;
end;

procedure TProfiler.Stop;
begin
  FEnabled := False;
  FEndTime := GetCurrentTimestamp;

  // Analyze hot paths after profiling
  if FMode in [pmSampling, pmHybrid] then
    AnalyzeHotPaths;
end;

procedure TProfiler.Reset;
var
  g, s, i: Integer;
begin
  // Reset opcode stats for all groups
  for g := 0 to MAX_OPCODE_GROUP do
    for s := 0 to MAX_OPCODES_PER_GROUP - 1 do
    begin
      FOpcodeStats[g, s].ExecutionCount := 0;
      FOpcodeStats[g, s].TotalCycles := 0;
      FOpcodeStats[g, s].MinCycles := High(Int64);
      FOpcodeStats[g, s].MaxCycles := 0;
      FOpcodeStats[g, s].SampleCount := 0;
    end;

  // Reset basic block stats
  for i := 0 to FBasicBlockCount - 1 do
  begin
    FBasicBlockStats[i].ExecutionCount := 0;
    FBasicBlockStats[i].TotalInstructions := 0;
    FBasicBlockStats[i].SampleCount := 0;
    FBasicBlockStats[i].LoopIterations := 0;
  end;

  // Reset array stats
  for i := 0 to FArrayStatsCount - 1 do
  begin
    FArrayStats[i].ReadCount := 0;
    FArrayStats[i].WriteCount := 0;
  end;

  // Reset sampling data
  FPCSampleIndex := 0;
  FPCSampleCount := 0;

  // Reset counters
  FTotalInstructions := 0;
  FSuperinstrCount := 0;
  FSuperinstrSaved := 0;

  // Clear hot paths
  FHotPathCount := 0;
end;

procedure TProfiler.SetEnabled(Value: Boolean);
begin
  if Value and not FEnabled then
    Start
  else if not Value and FEnabled then
    Stop;
end;

{ Instrumentation hooks }

procedure TProfiler.BeforeInstruction(PC: Integer; OpCode: Word);
var
  BlockIdx: Integer;
begin
  if not FEnabled then Exit;
  if FMode = pmDisabled then Exit;

  // Sampling mode: check if we should take a sample
  if FMode in [pmSampling, pmHybrid] then
  begin
    if ShouldSample then
      TakeSample(PC, OpCode);
  end;

  // Instrumentation mode: track everything
  if FMode in [pmInstrumentation, pmHybrid] then
  begin
    // Track basic block entry
    if FDetailLevel >= pdlStandard then
    begin
      BlockIdx := FindBasicBlockForPC(PC);
      if (BlockIdx >= 0) and (BlockIdx <> FCurrentBlockIndex) then
      begin
        FCurrentBlockIndex := BlockIdx;
        OnBasicBlockEntry(BlockIdx);
      end;
    end;
  end;
end;

procedure TProfiler.AfterInstruction(PC: Integer; OpCode: Word);
var
  Cycles: Int64;
  Group, SubOp: Word;
begin
  if not FEnabled then Exit;
  if FMode = pmDisabled then Exit;

  // Total instruction count (always tracked for basic stats)
  Inc(FTotalInstructions);

  // Sampling mode: only count instructions, no per-opcode tracking
  // This gives minimal overhead - detailed stats come from TakeSample
  if FMode = pmSampling then
    Exit;

  // Instrumentation and Hybrid modes: full per-opcode tracking
  Group := GetOpcodeGroup(OpCode);
  SubOp := GetOpcodeSubOp(OpCode);

  if (Group <= MAX_OPCODE_GROUP) and (SubOp < MAX_OPCODES_PER_GROUP) then
  begin
    Inc(FOpcodeStats[Group, SubOp].ExecutionCount);
    Cycles := EstimateCycles(OpCode);
    Inc(FOpcodeStats[Group, SubOp].TotalCycles, Cycles);
  end;
end;

procedure TProfiler.OnSuperinstruction(OpCode: Word; InstructionsSaved: Integer);
begin
  if not FEnabled then Exit;

  Inc(FSuperinstrCount);
  Inc(FSuperinstrSaved, InstructionsSaved);
end;

procedure TProfiler.OnArrayAccess(ArrayIdx: Integer; IsWrite: Boolean; LinearIndex: Integer);
begin
  if not FEnabled then Exit;
  if FDetailLevel < pdlFull then Exit;

  // Ensure array stats exist
  if ArrayIdx >= FArrayStatsCount then Exit;

  if IsWrite then
    Inc(FArrayStats[ArrayIdx].WriteCount)
  else
    Inc(FArrayStats[ArrayIdx].ReadCount);
end;

procedure TProfiler.OnBasicBlockEntry(BlockIndex: Integer);
begin
  if (BlockIndex < 0) or (BlockIndex >= FBasicBlockCount) then Exit;

  Inc(FBasicBlockStats[BlockIndex].ExecutionCount);
  if FBasicBlockStats[BlockIndex].IsLoopHeader then
    Inc(FBasicBlockStats[BlockIndex].LoopIterations);
end;

{ Sampling methods }

procedure TProfiler.TakeSample(PC: Integer; OpCode: Word);
var
  Group, SubOp: Word;
begin
  FPCSamples[FPCSampleIndex].PC := PC;
  FPCSamples[FPCSampleIndex].OpCode := OpCode;
  FPCSamples[FPCSampleIndex].Timestamp := GetCurrentTimestamp;

  FPCSampleIndex := (FPCSampleIndex + 1) mod Length(FPCSamples);
  Inc(FPCSampleCount);

  // Update opcode sample count
  Group := GetOpcodeGroup(OpCode);
  SubOp := GetOpcodeSubOp(OpCode);
  if (Group <= MAX_OPCODE_GROUP) and (SubOp < MAX_OPCODES_PER_GROUP) then
    Inc(FOpcodeStats[Group, SubOp].SampleCount);

  // Update basic block sample count
  if FDetailLevel >= pdlStandard then
  begin
    FCurrentBlockIndex := FindBasicBlockForPC(PC);
    if (FCurrentBlockIndex >= 0) and (FCurrentBlockIndex < FBasicBlockCount) then
      Inc(FBasicBlockStats[FCurrentBlockIndex].SampleCount);
  end;

  FLastSampleTime := FPCSamples[(FPCSampleIndex + Length(FPCSamples) - 1) mod Length(FPCSamples)].Timestamp;
end;

function TProfiler.ShouldSample: Boolean;
var
  Now, ElapsedUs: Int64;
  {$IFDEF WINDOWS}
  Freq: Int64;
  {$ENDIF}
begin
  Now := GetCurrentTimestamp;
  {$IFDEF WINDOWS}
  QueryPerformanceFrequency(Freq);
  ElapsedUs := ((Now - FLastSampleTime) * 1000000) div Freq;
  {$ELSE}
  ElapsedUs := Now - FLastSampleTime;
  {$ENDIF}
  Result := ElapsedUs >= FSampleIntervalUs;
end;

{ Basic block mapping }

procedure TProfiler.SetPCToBlockMapping(PC: Integer; BlockIndex: Integer);
begin
  // Grow array if needed
  if PC >= Length(FPCToBlockMap) then
    SetLength(FPCToBlockMap, PC + 1024);

  FPCToBlockMap[PC] := BlockIndex;
end;

procedure TProfiler.AddBasicBlock(const BlockLabel: string; FirstPC, LastPC: Integer; IsLoopHeader: Boolean);
var
  i: Integer;
begin
  // Grow array if needed
  if FBasicBlockCount >= Length(FBasicBlockStats) then
    SetLength(FBasicBlockStats, FBasicBlockCount + 64);

  FBasicBlockStats[FBasicBlockCount].BlockLabel := BlockLabel;
  FBasicBlockStats[FBasicBlockCount].FirstPC := FirstPC;
  FBasicBlockStats[FBasicBlockCount].LastPC := LastPC;
  FBasicBlockStats[FBasicBlockCount].ExecutionCount := 0;
  FBasicBlockStats[FBasicBlockCount].TotalInstructions := 0;
  FBasicBlockStats[FBasicBlockCount].SampleCount := 0;
  FBasicBlockStats[FBasicBlockCount].IsLoopHeader := IsLoopHeader;
  FBasicBlockStats[FBasicBlockCount].LoopIterations := 0;

  // Update PC → block mapping
  for i := FirstPC to LastPC do
    SetPCToBlockMapping(i, FBasicBlockCount);

  Inc(FBasicBlockCount);
end;

procedure TProfiler.SetArrayInfo(ArrayIdx: Integer; const Name: string; ElementType: TSSARegisterType);
begin
  // Grow array if needed
  while ArrayIdx >= FArrayStatsCount do
  begin
    if FArrayStatsCount >= Length(FArrayStats) then
      SetLength(FArrayStats, FArrayStatsCount + 16);

    FArrayStats[FArrayStatsCount].ArrayName := '';
    FArrayStats[FArrayStatsCount].ArrayIndex := FArrayStatsCount;
    FArrayStats[FArrayStatsCount].ReadCount := 0;
    FArrayStats[FArrayStatsCount].WriteCount := 0;
    Inc(FArrayStatsCount);
  end;

  FArrayStats[ArrayIdx].ArrayName := Name;
  FArrayStats[ArrayIdx].ElementType := ElementType;
end;

{ Helper methods }

procedure TProfiler.UpdateOpcodeStats(OpCode: Word; Cycles: Int64);
var
  Group, SubOp: Word;
begin
  Group := GetOpcodeGroup(OpCode);
  SubOp := GetOpcodeSubOp(OpCode);

  if (Group > MAX_OPCODE_GROUP) or (SubOp >= MAX_OPCODES_PER_GROUP) then Exit;

  Inc(FOpcodeStats[Group, SubOp].ExecutionCount);
  Inc(FOpcodeStats[Group, SubOp].TotalCycles, Cycles);

  if Cycles < FOpcodeStats[Group, SubOp].MinCycles then
    FOpcodeStats[Group, SubOp].MinCycles := Cycles;
  if Cycles > FOpcodeStats[Group, SubOp].MaxCycles then
    FOpcodeStats[Group, SubOp].MaxCycles := Cycles;
end;

function TProfiler.FindBasicBlockForPC(PC: Integer): Integer;
begin
  if (PC >= 0) and (PC < Length(FPCToBlockMap)) then
    Result := FPCToBlockMap[PC]
  else
    Result := -1;
end;

procedure TProfiler.AnalyzeHotPaths;
var
  i, j, StartPC, EndPC: Integer;
  TotalSamples: Int64;
  ConsecutiveSamples: Integer;
  LastPC: Integer;
begin
  FHotPathCount := 0;
  if FPCSampleCount = 0 then Exit;

  TotalSamples := Min(FPCSampleCount, Int64(Length(FPCSamples)));

  // Simple hot path detection: find sequences of consecutive PCs with high sample counts
  i := 0;
  while i < TotalSamples do
  begin
    StartPC := FPCSamples[i].PC;
    EndPC := StartPC;
    ConsecutiveSamples := 1;
    LastPC := StartPC;

    // Find consecutive samples in the same region
    j := i + 1;
    while j < TotalSamples do
    begin
      if Abs(FPCSamples[j].PC - LastPC) <= 10 then  // Allow some slack
      begin
        Inc(ConsecutiveSamples);
        if FPCSamples[j].PC < StartPC then StartPC := FPCSamples[j].PC;
        if FPCSamples[j].PC > EndPC then EndPC := FPCSamples[j].PC;
        LastPC := FPCSamples[j].PC;
        Inc(j);
      end
      else
        Break;
    end;

    // If this region has significant samples, add it as a hot path
    if ConsecutiveSamples >= 10 then
    begin
      // Grow array if needed
      if FHotPathCount >= Length(FHotPaths) then
        SetLength(FHotPaths, FHotPathCount + 16);

      FHotPaths[FHotPathCount].StartPC := StartPC;
      FHotPaths[FHotPathCount].EndPC := EndPC;
      FHotPaths[FHotPathCount].ExecutionCount := ConsecutiveSamples;
      FHotPaths[FHotPathCount].Percentage := (ConsecutiveSamples / TotalSamples) * 100;

      if FHotPaths[FHotPathCount].Percentage >= FHotspotThreshold * 100 then
        Inc(FHotPathCount);
    end;

    i := j;
  end;
end;

{ Analysis methods }

function TProfiler.GetTotalExecutionTime: Double;
{$IFDEF WINDOWS}
var
  Freq: Int64;
begin
  QueryPerformanceFrequency(Freq);
  Result := (FEndTime - FStartTime) / Freq;
end;
{$ELSE}
begin
  Result := (FEndTime - FStartTime) / 1000000.0;  // Microseconds to seconds
end;
{$ENDIF}

function TProfiler.GetInstructionsPerSecond: Double;
var
  Time: Double;
begin
  Time := GetTotalExecutionTime;
  if Time > 0 then
    Result := FTotalInstructions / Time
  else
    Result := 0;
end;

function TProfiler.GetOpcodePercentage(OpCode: Word): Double;
var
  TotalCycles: Int64;
  g, s: Integer;
  Group, SubOp: Word;
begin
  Group := GetOpcodeGroup(OpCode);
  SubOp := GetOpcodeSubOp(OpCode);

  if (Group > MAX_OPCODE_GROUP) or (SubOp >= MAX_OPCODES_PER_GROUP) then
  begin
    Result := 0;
    Exit;
  end;

  TotalCycles := 0;
  for g := 0 to MAX_OPCODE_GROUP do
    for s := 0 to MAX_OPCODES_PER_GROUP - 1 do
      Inc(TotalCycles, FOpcodeStats[g, s].TotalCycles);

  if TotalCycles > 0 then
    Result := (FOpcodeStats[Group, SubOp].TotalCycles / TotalCycles) * 100
  else
    Result := 0;
end;

function TProfiler.GetTopOpcodes(Count: Integer): string;
type
  TOpcodeRank = record
    OpCode: Word;      // Full 2-byte opcode
    Value: Int64;      // Cycles for instrumentation, samples for sampling
    ExecCount: Int64;  // Exec count or sample count
  end;
var
  Ranks: array of TOpcodeRank;
  g, s, RankCount, i, j: Integer;
  Temp: TOpcodeRank;
  TotalValue: Int64;
  UseSampling: Boolean;
  CountLabel: string;
  OpCode: Word;
begin
  // Collect all non-zero opcodes into a dynamic array
  SetLength(Ranks, 0);
  RankCount := 0;
  TotalValue := 0;
  UseSampling := (FMode = pmSampling);

  if UseSampling then
    CountLabel := 'samples'
  else
    CountLabel := 'exec';

  // Iterate over all groups and sub-opcodes
  for g := 0 to MAX_OPCODE_GROUP do
    for s := 0 to MAX_OPCODES_PER_GROUP - 1 do
    begin
      // Skip unused opcodes
      if UseSampling then
      begin
        if FOpcodeStats[g, s].SampleCount = 0 then Continue;
      end
      else
      begin
        if FOpcodeStats[g, s].ExecutionCount = 0 then Continue;
      end;

      // Add to ranks
      SetLength(Ranks, RankCount + 1);
      OpCode := (g shl 8) or s;  // Reconstruct full opcode
      Ranks[RankCount].OpCode := OpCode;
      if UseSampling then
      begin
        Ranks[RankCount].Value := FOpcodeStats[g, s].SampleCount;
        Ranks[RankCount].ExecCount := FOpcodeStats[g, s].SampleCount;
      end
      else
      begin
        Ranks[RankCount].Value := FOpcodeStats[g, s].TotalCycles;
        Ranks[RankCount].ExecCount := FOpcodeStats[g, s].ExecutionCount;
      end;
      Inc(TotalValue, Ranks[RankCount].Value);
      Inc(RankCount);
    end;

  // Sort by value (bubble sort for simplicity)
  for i := 0 to RankCount - 2 do
    for j := i + 1 to RankCount - 1 do
      if Ranks[j].Value > Ranks[i].Value then
      begin
        Temp := Ranks[i];
        Ranks[i] := Ranks[j];
        Ranks[j] := Temp;
      end;

  // Build result string
  Result := '';
  for i := 0 to Min(Count - 1, RankCount - 1) do
  begin
    if Ranks[i].Value = 0 then Continue;

    if Result <> '' then Result := Result + LineEnding;
    Result := Result + Format('%3d. %-20s  %12d  %6.2f%%  %12d %s',
      [i + 1, OpcodeToString(Ranks[i].OpCode), Ranks[i].Value,
       IfThen(TotalValue > 0, (Ranks[i].Value / TotalValue) * 100, 0.0),
       Ranks[i].ExecCount, CountLabel]);
  end;
end;

function TProfiler.GetTopBasicBlocks(Count: Integer): string;
type
  TBlockRank = record
    Index: Integer;
    ExecCount: Int64;
  end;
var
  Ranks: array of TBlockRank;
  i, j: Integer;
  Temp: TBlockRank;
  TotalExec: Int64;
  LoopStr: string;
begin
  if FBasicBlockCount = 0 then
  begin
    Result := '(no basic block data)';
    Exit;
  end;

  SetLength(Ranks, FBasicBlockCount);
  TotalExec := 0;

  // Collect stats
  for i := 0 to FBasicBlockCount - 1 do
  begin
    Ranks[i].Index := i;
    Ranks[i].ExecCount := FBasicBlockStats[i].ExecutionCount;
    Inc(TotalExec, Ranks[i].ExecCount);
  end;

  // Sort by execution count
  for i := 0 to FBasicBlockCount - 2 do
    for j := i + 1 to FBasicBlockCount - 1 do
      if Ranks[j].ExecCount > Ranks[i].ExecCount then
      begin
        Temp := Ranks[i];
        Ranks[i] := Ranks[j];
        Ranks[j] := Temp;
      end;

  // Build result string
  Result := '';
  for i := 0 to Min(Count - 1, FBasicBlockCount - 1) do
  begin
    if Ranks[i].ExecCount = 0 then Continue;

    if FBasicBlockStats[Ranks[i].Index].IsLoopHeader then
      LoopStr := ' [LOOP]'
    else
      LoopStr := '';

    if Result <> '' then Result := Result + LineEnding;
    Result := Result + Format('%3d. %-20s  %12d exec  %6.2f%%  PC: %d-%d%s',
      [i + 1, FBasicBlockStats[Ranks[i].Index].BlockLabel,
       Ranks[i].ExecCount,
       IfThen(TotalExec > 0, (Ranks[i].ExecCount / TotalExec) * 100, 0.0),
       FBasicBlockStats[Ranks[i].Index].FirstPC,
       FBasicBlockStats[Ranks[i].Index].LastPC,
       LoopStr]);
  end;
end;

function TProfiler.GetHotPathCount: Integer;
begin
  Result := FHotPathCount;
end;

function TProfiler.GetHotPath(Index: Integer): THotPathSegment;
begin
  if (Index >= 0) and (Index < FHotPathCount) then
    Result := FHotPaths[Index]
  else
  begin
    Result.StartPC := 0;
    Result.EndPC := 0;
    Result.ExecutionCount := 0;
    Result.Percentage := 0;
  end;
end;

{ Output methods }

procedure TProfiler.PrintReport;
var
  i: Integer;
  HotPath: THotPathSegment;
begin
  WriteLn('');
  WriteLn('===============================================================================');
  WriteLn('                           PROFILER REPORT');
  WriteLn('===============================================================================');
  WriteLn('');
  WriteLn(Format('Mode:                    %s', [GetEnumName(TypeInfo(TProfilerMode), Ord(FMode))]));
  WriteLn(Format('Total Instructions:      %d', [FTotalInstructions]));
  WriteLn(Format('Execution Time:          %.6f seconds', [GetTotalExecutionTime]));
  WriteLn(Format('Instructions/Second:     %.2f MIPS', [GetInstructionsPerSecond / 1000000]));
  WriteLn('');

  if FSuperinstrCount > 0 then
  begin
    WriteLn('--- Superinstruction Stats ---');
    WriteLn(Format('Superinstructions:       %d', [FSuperinstrCount]));
    WriteLn(Format('Instructions Saved:      %d', [FSuperinstrSaved]));
    if FTotalInstructions > 0 then
      WriteLn(Format('Fusion Efficiency:       %.2f%%', [(FSuperinstrSaved / FTotalInstructions) * 100]));
    WriteLn('');
  end;

  if FMode = pmSampling then
  begin
    WriteLn('--- Top 15 Opcodes by Sample Count ---');
    WriteLn(Format('%-4s %-20s  %12s  %7s  %12s', ['Rank', 'Opcode', 'Samples', '%', 'Samples']));
  end
  else
  begin
    WriteLn('--- Top 15 Opcodes by Estimated Cycles ---');
    WriteLn(Format('%-4s %-20s  %12s  %7s  %12s', ['Rank', 'Opcode', 'Cycles', '%', 'Exec Count']));
  end;
  WriteLn(StringOfChar('-', 70));
  WriteLn(GetTopOpcodes(15));
  WriteLn('');

  if FBasicBlockCount > 0 then
  begin
    WriteLn('--- Top 10 Basic Blocks by Execution Count ---');
    WriteLn(Format('%-4s %-20s  %12s  %7s  %s', ['Rank', 'Block', 'Exec Count', '%', 'PC Range']));
    WriteLn(StringOfChar('-', 70));
    WriteLn(GetTopBasicBlocks(10));
    WriteLn('');
  end;

  if FHotPathCount > 0 then
  begin
    WriteLn('--- Hot Paths ---');
    for i := 0 to FHotPathCount - 1 do
    begin
      HotPath := FHotPaths[i];
      WriteLn(Format('  PC %d-%d: %.2f%% of samples',
        [HotPath.StartPC, HotPath.EndPC, HotPath.Percentage]));
    end;
    WriteLn('');
  end;

  WriteLn('===============================================================================');
end;

procedure TProfiler.PrintOpcodeTable;
var
  g, s: Integer;
  TotalCycles: Int64;
  OpCode: Word;
begin
  TotalCycles := 0;
  for g := 0 to MAX_OPCODE_GROUP do
    for s := 0 to MAX_OPCODES_PER_GROUP - 1 do
      Inc(TotalCycles, FOpcodeStats[g, s].TotalCycles);

  WriteLn('');
  WriteLn('Full Opcode Statistics:');
  WriteLn(Format('%-25s  %12s  %7s  %12s  %8s',
    ['Opcode', 'Cycles', '%', 'Count', 'Cyc/Op']));
  WriteLn(StringOfChar('-', 75));

  for g := 0 to MAX_OPCODE_GROUP do
    for s := 0 to MAX_OPCODES_PER_GROUP - 1 do
    begin
      if FOpcodeStats[g, s].ExecutionCount = 0 then Continue;

      OpCode := (g shl 8) or s;
      WriteLn(Format('%-25s  %12d  %6.2f%%  %12d  %8.1f',
        [OpcodeToString(OpCode),
         FOpcodeStats[g, s].TotalCycles,
         IfThen(TotalCycles > 0, (FOpcodeStats[g, s].TotalCycles / TotalCycles) * 100, 0.0),
         FOpcodeStats[g, s].ExecutionCount,
         FOpcodeStats[g, s].TotalCycles / Max(1, FOpcodeStats[g, s].ExecutionCount)]));
    end;
end;

procedure TProfiler.PrintBasicBlockTable;
var
  i: Integer;
  LoopStr: string;
begin
  if FBasicBlockCount = 0 then
  begin
    WriteLn('(no basic block data)');
    Exit;
  end;

  WriteLn('');
  WriteLn('Basic Block Statistics:');
  WriteLn(Format('%-20s  %12s  %12s  %8s  %s',
    ['Block', 'Exec Count', 'Samples', 'Loop Iter', 'PC Range']));
  WriteLn(StringOfChar('-', 75));

  for i := 0 to FBasicBlockCount - 1 do
  begin
    if FBasicBlockStats[i].IsLoopHeader then
      LoopStr := ' [LOOP]'
    else
      LoopStr := '';

    WriteLn(Format('%-20s  %12d  %12d  %8d  %d-%d%s',
      [FBasicBlockStats[i].BlockLabel,
       FBasicBlockStats[i].ExecutionCount,
       FBasicBlockStats[i].SampleCount,
       FBasicBlockStats[i].LoopIterations,
       FBasicBlockStats[i].FirstPC,
       FBasicBlockStats[i].LastPC,
       LoopStr]));
  end;
end;

procedure TProfiler.PrintSuperinstructionStats;
var
  g, s: Integer;
  OpCode: Word;
begin
  WriteLn('');
  WriteLn('Superinstruction Statistics:');
  WriteLn(Format('%-25s  %12s  %7s', ['Superinstruction', 'Count', '%']));
  WriteLn(StringOfChar('-', 50));

  // Superinstructions are in groups 200-255 (0xC8xx+)
  for g := 200 to 255 do
    for s := 0 to MAX_OPCODES_PER_GROUP - 1 do
    begin
      if FOpcodeStats[g, s].ExecutionCount = 0 then Continue;

      OpCode := (g shl 8) or s;
      WriteLn(Format('%-25s  %12d  %6.2f%%',
        [OpcodeToString(OpCode), FOpcodeStats[g, s].ExecutionCount,
         IfThen(FSuperinstrCount > 0, (FOpcodeStats[g, s].ExecutionCount / FSuperinstrCount) * 100, 0.0)]));
    end;
end;

procedure TProfiler.PrintArrayAccessStats;
var
  i: Integer;
begin
  if FArrayStatsCount = 0 then
  begin
    WriteLn('(no array access data)');
    Exit;
  end;

  WriteLn('');
  WriteLn('Array Access Statistics:');
  WriteLn(Format('%-20s  %12s  %12s  %s',
    ['Array', 'Reads', 'Writes', 'Type']));
  WriteLn(StringOfChar('-', 60));

  for i := 0 to FArrayStatsCount - 1 do
  begin
    if (FArrayStats[i].ReadCount = 0) and (FArrayStats[i].WriteCount = 0) then Continue;

    WriteLn(Format('%-20s  %12d  %12d  %s',
      [FArrayStats[i].ArrayName,
       FArrayStats[i].ReadCount,
       FArrayStats[i].WriteCount,
       SSARegisterTypeToString(FArrayStats[i].ElementType)]));
  end;
end;

{ Export methods }

procedure TProfiler.ExportJSON(const FileName: string);
var
  F: TextFile;
  g, s, i: Integer;
  First: Boolean;
  OpCode: Word;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    WriteLn(F, '{');
    WriteLn(F, '  "summary": {');
    WriteLn(F, Format('    "mode": "%s",', [GetEnumName(TypeInfo(TProfilerMode), Ord(FMode))]));
    WriteLn(F, Format('    "total_instructions": %d,', [FTotalInstructions]));
    WriteLn(F, Format('    "execution_time_seconds": %.6f,', [GetTotalExecutionTime]));
    WriteLn(F, Format('    "instructions_per_second": %.2f,', [GetInstructionsPerSecond]));
    WriteLn(F, Format('    "superinstructions_executed": %d,', [FSuperinstrCount]));
    WriteLn(F, Format('    "instructions_saved_by_fusion": %d', [FSuperinstrSaved]));
    WriteLn(F, '  },');

    // Opcode statistics
    WriteLn(F, '  "opcodes": [');
    First := True;
    for g := 0 to MAX_OPCODE_GROUP do
      for s := 0 to MAX_OPCODES_PER_GROUP - 1 do
      begin
        if FOpcodeStats[g, s].ExecutionCount = 0 then Continue;

        if not First then WriteLn(F, ',');
        First := False;

        OpCode := (g shl 8) or s;
        Write(F, Format('    {"opcode": %d, "name": "%s", "count": %d, "cycles": %d, "samples": %d}',
          [OpCode, OpcodeToString(OpCode), FOpcodeStats[g, s].ExecutionCount,
           FOpcodeStats[g, s].TotalCycles, FOpcodeStats[g, s].SampleCount]));
      end;
    WriteLn(F);
    WriteLn(F, '  ],');

    // Basic block statistics
    WriteLn(F, '  "basic_blocks": [');
    First := True;
    for i := 0 to FBasicBlockCount - 1 do
    begin
      if not First then WriteLn(F, ',');
      First := False;

      if FBasicBlockStats[i].IsLoopHeader then
        Write(F, Format('    {"label": "%s", "first_pc": %d, "last_pc": %d, "exec_count": %d, "samples": %d, "is_loop": true}',
          [FBasicBlockStats[i].BlockLabel, FBasicBlockStats[i].FirstPC, FBasicBlockStats[i].LastPC,
           FBasicBlockStats[i].ExecutionCount, FBasicBlockStats[i].SampleCount]))
      else
        Write(F, Format('    {"label": "%s", "first_pc": %d, "last_pc": %d, "exec_count": %d, "samples": %d, "is_loop": false}',
          [FBasicBlockStats[i].BlockLabel, FBasicBlockStats[i].FirstPC, FBasicBlockStats[i].LastPC,
           FBasicBlockStats[i].ExecutionCount, FBasicBlockStats[i].SampleCount]));
    end;
    WriteLn(F);
    WriteLn(F, '  ],');

    // Hot paths
    WriteLn(F, '  "hot_paths": [');
    First := True;
    for i := 0 to FHotPathCount - 1 do
    begin
      if not First then WriteLn(F, ',');
      First := False;

      Write(F, Format('    {"start_pc": %d, "end_pc": %d, "percentage": %.2f}',
        [FHotPaths[i].StartPC, FHotPaths[i].EndPC, FHotPaths[i].Percentage]));
    end;
    WriteLn(F);
    WriteLn(F, '  ]');

    WriteLn(F, '}');
  finally
    CloseFile(F);
  end;
end;

procedure TProfiler.ExportFoldedFlameGraph(const FileName: string);
var
  F: TextFile;
  g, s, i: Integer;
  Stack: string;
  OpCode: Word;
begin
  { Folded format for Brendan Gregg's flamegraph.pl:
    Each line is: stack;stack;stack count
    Example:
      main;loop_header;inner_loop 1234
      main;loop_header 567
  }

  AssignFile(F, FileName);
  Rewrite(F);
  try
    // Export basic blocks as stack frames
    for i := 0 to FBasicBlockCount - 1 do
    begin
      if FBasicBlockStats[i].ExecutionCount = 0 then Continue;

      // Simple stack: program;block_name
      Stack := 'SedaiBasic;' + FBasicBlockStats[i].BlockLabel;
      WriteLn(F, Format('%s %d', [Stack, FBasicBlockStats[i].ExecutionCount]));
    end;

    // Also export opcodes as leaf frames
    for g := 0 to MAX_OPCODE_GROUP do
      for s := 0 to MAX_OPCODES_PER_GROUP - 1 do
      begin
        if FOpcodeStats[g, s].ExecutionCount = 0 then Continue;

        OpCode := (g shl 8) or s;
        Stack := 'SedaiBasic;VM_Dispatch;' + OpcodeToString(OpCode);
        WriteLn(F, Format('%s %d', [Stack, FOpcodeStats[g, s].ExecutionCount]));
      end;
  finally
    CloseFile(F);
  end;
end;

procedure TProfiler.ExportCSV(const FileName: string);
var
  F: TextFile;
  g, s: Integer;
  OpCode: Word;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    // Header
    WriteLn(F, 'Opcode,Name,ExecutionCount,TotalCycles,SampleCount,AvgCyclesPerOp');

    // Data
    for g := 0 to MAX_OPCODE_GROUP do
      for s := 0 to MAX_OPCODES_PER_GROUP - 1 do
      begin
        if FOpcodeStats[g, s].ExecutionCount = 0 then Continue;

        OpCode := (g shl 8) or s;
        WriteLn(F, Format('%d,%s,%d,%d,%d,%.2f',
          [OpCode, OpcodeToString(OpCode), FOpcodeStats[g, s].ExecutionCount,
           FOpcodeStats[g, s].TotalCycles, FOpcodeStats[g, s].SampleCount,
           FOpcodeStats[g, s].TotalCycles / Max(1, FOpcodeStats[g, s].ExecutionCount)]));
      end;
  finally
    CloseFile(F);
  end;
end;

end.
