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
{===============================================================================
  SEDAIBASIC2 - Register Allocator (Phase 4F Step 3)
  
  Purpose: Global register allocation with liveness analysis and linear scan
  
  Architecture:
  - 48 registers total (16 Int64, 16 Double, 16 String)
  - Linear scan allocation algorithm (Poletto & Sarkar 1999)
  - Forward data-flow liveness analysis
  - Spill to FStack[] when registers exhausted
  - Priority: RefCount >= 3 variables allocated first
  
  Author: Phase 4F Step 3 - Senior Compiler Engineer
  Date: October 15, 2025
===============================================================================}

unit SedaiRegAllocator;

{$mode objfpc}{$H+}{$J-}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, StrUtils, Generics.Collections, SedaiBytecodeTypes, SedaiParserTypes, SedaiAST;

type
  { Register types for typed allocation }
  TRegisterType = (rtInt, rtFloat, rtString, rtUnknown);
  
  { Live interval for a variable (basic block scope) }
  TLiveInterval = record
    VarName: string;           // Variable name
    VarIndex: Integer;         // Index in variable table
    VarType: TRegisterType;    // Type of variable
    StartPos: Integer;         // First use (bytecode position)
    EndPos: Integer;           // Last use (bytecode position)
    RefCount: Integer;         // Number of references
    RegIndex: Integer;         // Allocated register (-1 if spilled)
    IsSpilled: Boolean;        // True if spilled to stack
    Priority: Integer;         // PHASE 4F: 1=loop counter, 2=hot var (RefCount>=3), 3=normal
    IsLoopCounter: Boolean;    // PHASE 4F: True for i/j/k variables
  end;
  
  { Register allocation map (variable -> register mapping) }
  TRegAllocationMap = class
  private
    FIntervals: array of TLiveInterval;
    FIntRegsFree: set of 0..15;      // Available integer registers
    FFloatRegsFree: set of 0..15;    // Available float registers
    FStrRegsFree: set of 0..15;      // Available string registers
    FSpillCount: Integer;             // Number of spilled variables
    
    function FindInterval(const VarName: string): Integer;
    function AllocateRegister(RegType: TRegisterType): Integer;
    procedure FreeRegister(RegType: TRegisterType; RegIndex: Integer);
    
  public
    constructor Create;
    destructor Destroy; override;
    
    { Add a variable use to the allocation map }
    procedure AddVariableUse(const VarName: string; VarIndex: Integer; 
                            Position: Integer; VarType: TRegisterType);
    
    { Finalize intervals and run allocation algorithm }
    procedure RunAllocation;
    
    { Query allocation results }
    function GetRegisterForVariable(const VarName: string): Integer;
    function IsVariableSpilled(const VarName: string): Boolean;
    function GetVariableType(const VarName: string): TRegisterType;
    
    { Debug output }
    procedure DumpAllocationMap(const Filename: string);
    
    { Statistics }
    property SpillCount: Integer read FSpillCount;
    function GetIntervalCount: Integer;
    function GetAllocatedRegisterCount(RegType: TRegisterType): Integer;

    { Coordination with local allocator }
    function GetHighestAllocatedRegister(RegType: TRegisterType): Integer;
    procedure PopulateRegisterUsage(var RegUsage: array of Boolean);
  end;
  
  { Loop context for tracking FOR loop variable live ranges }
  TLoopContext = record
    LoopVar: string;       // Loop variable name (e.g., "I%")
    StartPos: Integer;     // Position where FOR starts
    EndPos: Integer;       // Position where NEXT ends (extended)
  end;

  { Liveness analyzer (data-flow analysis) }
  TLivenessAnalyzer = class
  private
    FVariableUses: TStringList;  // Variable -> positions list
    FLoopStack: array of TLoopContext;  // Stack of active loops

  public
    constructor Create;
    destructor Destroy; override;

    { Scan AST and build use-def chains }
    procedure AnalyzeAST(RootNode: TASTNode);

    { Extract intervals for register allocator }
    procedure ExtractIntervals(AllocMap: TRegAllocationMap);

  private
    procedure VisitNode(Node: TASTNode; Position: Integer);
    function InferVariableType(Node: TASTNode): TRegisterType;
    procedure PushLoopContext(const VarName: string; StartPos: Integer);
    procedure PopLoopContext(EndPos: Integer);
  end;

implementation

uses
  Math;

{ ============================================================================= }
{ TRegAllocationMap Implementation }
{ ============================================================================= }

constructor TRegAllocationMap.Create;
var
  i: Integer;
begin
  inherited Create;

  // SESSION 10 FIX: Reserve R0 (Int accumulator) and F0 (Float accumulator, mapped as R16)
  // R0 and R16 are used by VM as temporary accumulators for stack operations
  // Variables CANNOT be allocated to these registers or they will be corrupted!
  FIntRegsFree := [1..15];      // R1-R15 available (R0 RESERVED)
  FFloatRegsFree := [1..15];    // F1-F15 available (F0=R16 RESERVED)
  FStrRegsFree := [0..15];      // S0-S15 all available

  SetLength(FIntervals, 0);
  FSpillCount := 0;
end;

destructor TRegAllocationMap.Destroy;
begin
  SetLength(FIntervals, 0);
  inherited Destroy;
end;

function TRegAllocationMap.FindInterval(const VarName: string): Integer;
var
  i: Integer;
  NormalizedName: string;
begin
  // PHASE 4F Step 3 FIX: Normalize to UPPERCASE for case-insensitive lookup
  NormalizedName := UpperCase(VarName);

  Result := -1;
  for i := 0 to High(FIntervals) do
  begin
    if FIntervals[i].VarName = NormalizedName then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TRegAllocationMap.AddVariableUse(const VarName: string;
  VarIndex: Integer; Position: Integer; VarType: TRegisterType);
var
  Idx: Integer;
  NewInterval: TLiveInterval;
  VarNameUpper: string;
  IsLoop: Boolean;
begin
  Idx := FindInterval(VarName);

  if Idx = -1 then
  begin
    // PHASE 4F: Check if this is a loop counter variable
    VarNameUpper := UpperCase(VarName);
    IsLoop := (VarNameUpper = 'I') or (VarNameUpper = 'J') or (VarNameUpper = 'K') or
              (VarNameUpper = 'I%') or (VarNameUpper = 'J%') or (VarNameUpper = 'K%') or
              (VarNameUpper = 'I1') or (VarNameUpper = 'J1') or (VarNameUpper = 'K1') or
              (VarNameUpper = 'I2') or (VarNameUpper = 'J2') or (VarNameUpper = 'K2');

    // Create new interval
    NewInterval.VarName := VarName;
    NewInterval.VarIndex := VarIndex;
    NewInterval.VarType := VarType;
    NewInterval.StartPos := Position;
    NewInterval.EndPos := Position;
    NewInterval.RefCount := 1;
    NewInterval.RegIndex := -1;
    NewInterval.IsSpilled := False;
    NewInterval.IsLoopCounter := IsLoop;
    NewInterval.Priority := 3;  // Default: normal priority
    if IsLoop then
      NewInterval.Priority := 1  // Loop counters get highest priority
    else if NewInterval.RefCount >= 3 then
      NewInterval.Priority := 2;  // Hot variables (will be updated in RunAllocation)

    SetLength(FIntervals, Length(FIntervals) + 1);
    FIntervals[High(FIntervals)] := NewInterval;
  end
  else
  begin
    // SESSION 11C FIX: Update existing interval - extend BOTH Start and End
    // Bug: If variable is seen first at position 12, then at position 11,
    // StartPos stays 12 causing premature expiration.
    // Example: F3 first seen in line 40 (pos 12), then in line 50 (pos 11)
    FIntervals[Idx].StartPos := Min(FIntervals[Idx].StartPos, Position);
    FIntervals[Idx].EndPos := Max(FIntervals[Idx].EndPos, Position);
    Inc(FIntervals[Idx].RefCount);
  end;
end;

function TRegAllocationMap.AllocateRegister(RegType: TRegisterType): Integer;
var
  i: Integer;
begin
  Result := -1;  // -1 indicates spill needed

  // Find first free register based on type
  // PHASE 4F Step 2 FIX: Float registers map to R16-R31 in the VM!
  case RegType of
    rtInt:
      for i := 0 to 15 do
        if i in FIntRegsFree then
        begin
          Result := i;  // R0-R15 for integers
          Exclude(FIntRegsFree, i);
          Exit;
        end;
    rtFloat:
      for i := 0 to 15 do
        if i in FFloatRegsFree then
        begin
          Result := i + 16;  // R16-R31 for floats (add 16 offset!)
          Exclude(FFloatRegsFree, i);
          Exit;
        end;
    rtString:
      for i := 0 to 15 do
        if i in FStrRegsFree then
        begin
          Result := i;  // S0-S15 for strings (separate namespace)
          Exclude(FStrRegsFree, i);
          Exit;
        end;
  end;
end;

procedure TRegAllocationMap.FreeRegister(RegType: TRegisterType; RegIndex: Integer);
var
  LocalIdx: Integer;
begin
  // PHASE 4F Step 2 FIX: Float registers are R16-R31, need to subtract 16 offset
  case RegType of
    rtInt:
      begin
        if (RegIndex < 0) or (RegIndex > 15) then Exit;
        Include(FIntRegsFree, RegIndex);
      end;
    rtFloat:
      begin
        if (RegIndex < 16) or (RegIndex > 31) then Exit;
        LocalIdx := RegIndex - 16;  // Convert R16-R31 to 0-15 index
        Include(FFloatRegsFree, LocalIdx);
      end;
    rtString:
      begin
        if (RegIndex < 0) or (RegIndex > 15) then Exit;
        Include(FStrRegsFree, RegIndex);
      end;
  end;
end;

procedure TRegAllocationMap.RunAllocation;
var
  i, j: Integer;
  SortedIntervals: array of Integer;  // Indices sorted by StartPos
  RegIdx: Integer;
  ActiveIntervals: TList;

  { Sort intervals by start position (linear scan requirement) }
  procedure SortIntervalsByStart;
  var
    k, m, minIdx, temp: Integer;
  begin
    // PHASE 4F: First pass - update priorities based on RefCount
    for k := 0 to High(FIntervals) do
    begin
      if not FIntervals[k].IsLoopCounter then
      begin
        if FIntervals[k].RefCount >= 3 then
          FIntervals[k].Priority := 2  // Hot variable
        else
          FIntervals[k].Priority := 3; // Normal
      end;
      // Loop counters keep Priority=1
    end;

    SetLength(SortedIntervals, Length(FIntervals));
    for k := 0 to High(SortedIntervals) do
      SortedIntervals[k] := k;

    // Simple selection sort (intervals list is small, typically < 100)
    for k := 0 to High(SortedIntervals) - 1 do
    begin
      minIdx := k;
      for m := k + 1 to High(SortedIntervals) do
      begin
        // PHASE 4F: Primary sorting key is Priority (ascending: 1 < 2 < 3)
        if FIntervals[SortedIntervals[m]].Priority <
           FIntervals[SortedIntervals[minIdx]].Priority then
          minIdx := m
        // Secondary: RefCount descending (for same priority)
        else if (FIntervals[SortedIntervals[m]].Priority =
                 FIntervals[SortedIntervals[minIdx]].Priority) and
                (FIntervals[SortedIntervals[m]].RefCount >
                 FIntervals[SortedIntervals[minIdx]].RefCount) then
          minIdx := m
        // Tertiary: StartPos ascending
        else if (FIntervals[SortedIntervals[m]].Priority =
                 FIntervals[SortedIntervals[minIdx]].Priority) and
                (FIntervals[SortedIntervals[m]].RefCount =
                 FIntervals[SortedIntervals[minIdx]].RefCount) and
                (FIntervals[SortedIntervals[m]].StartPos <
                 FIntervals[SortedIntervals[minIdx]].StartPos) then
          minIdx := m;
      end;

      if minIdx <> k then
      begin
        temp := SortedIntervals[k];
        SortedIntervals[k] := SortedIntervals[minIdx];
        SortedIntervals[minIdx] := temp;
      end;
    end;
  end;
  
  { Expire old intervals that no longer overlap }
  procedure ExpireOldIntervals(CurrentPos: Integer);
  var
    k: Integer;
    IntervalIdx: Integer;
  begin
    k := 0;
    while k < ActiveIntervals.Count do
    begin
      IntervalIdx := PtrInt(ActiveIntervals[k]);
      // SESSION 11C FIX: Intervals that end BEFORE current position can be expired
      // Original bug: F1 End=11, F3 Start=12 → F1.End (11) < F3.Start (12) → F1 expires → R18 reused
      // But F1, F2, F3 must be live together for swap: f1=f2; f2=f3
      // Fix: Don't expire if EndPos >= CurrentPos (keep alive through current position)
      if FIntervals[IntervalIdx].EndPos < CurrentPos then
      begin
        // Interval expired, free its register
        if not FIntervals[IntervalIdx].IsSpilled then
          FreeRegister(FIntervals[IntervalIdx].VarType,
                      FIntervals[IntervalIdx].RegIndex);
        ActiveIntervals.Delete(k);
      end
      else
        Inc(k);
    end;
  end;
  
begin
  if Length(FIntervals) = 0 then
    Exit;
  
  SortIntervalsByStart;
  ActiveIntervals := TList.Create;
  try
    // Linear scan allocation
    for i := 0 to High(SortedIntervals) do
    begin
      j := SortedIntervals[i];
      
      // Expire intervals that ended before this one starts
      ExpireOldIntervals(FIntervals[j].StartPos);
      
      // Try to allocate register
      RegIdx := AllocateRegister(FIntervals[j].VarType);
      
      if RegIdx = -1 then
      begin
        // No register available - SPILL
        FIntervals[j].IsSpilled := True;
        FIntervals[j].RegIndex := -1;
        Inc(FSpillCount);
      end
      else
      begin
        // Register allocated successfully
        FIntervals[j].RegIndex := RegIdx;
        FIntervals[j].IsSpilled := False;
        ActiveIntervals.Add(Pointer(PtrInt(j)));
      end;
    end;
  finally
    ActiveIntervals.Free;
  end;
end;

function TRegAllocationMap.GetRegisterForVariable(const VarName: string): Integer;
var
  Idx: Integer;
begin
  Idx := FindInterval(VarName);
  if Idx = -1 then
    Result := -1
  else
    Result := FIntervals[Idx].RegIndex;
end;

function TRegAllocationMap.IsVariableSpilled(const VarName: string): Boolean;
var
  Idx: Integer;
begin
  Idx := FindInterval(VarName);
  Result := (Idx <> -1) and FIntervals[Idx].IsSpilled;
end;

function TRegAllocationMap.GetVariableType(const VarName: string): TRegisterType;
var
  Idx: Integer;
begin
  Idx := FindInterval(VarName);
  if Idx = -1 then
    Result := rtUnknown
  else
    Result := FIntervals[Idx].VarType;
end;

procedure TRegAllocationMap.DumpAllocationMap(const Filename: string);
var
  F: TextFile;
  i: Integer;
  RegTypeStr, StatusStr: string;
begin
  AssignFile(F, Filename);
  Rewrite(F);
  try
    WriteLn(F, '# Register Allocation Map');
    WriteLn(F, '# Generated: ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    WriteLn(F, '');
    WriteLn(F, '## Statistics');
    WriteLn(F, '- Total intervals: ', Length(FIntervals));
    WriteLn(F, '- Spilled variables: ', FSpillCount);
    WriteLn(F, '- Int registers used: ', 16 - Cardinal(FIntRegsFree) * 0);  // Count set bits
    WriteLn(F, '- Float registers used: ', 16 - Cardinal(FFloatRegsFree) * 0);
    WriteLn(F, '- String registers used: ', 16 - Cardinal(FStrRegsFree) * 0);
    WriteLn(F, '');
    WriteLn(F, '## Intervals (Phase 4F: Priority Allocation)');
    WriteLn(F, '| Variable | Type | Priority | Start | End | Refs | Register | Status |');
    WriteLn(F, '|----------|------|----------|-------|-----|------|----------|--------|');

    for i := 0 to High(FIntervals) do
    begin
      case FIntervals[i].VarType of
        rtInt: RegTypeStr := 'Int';
        rtFloat: RegTypeStr := 'Float';
        rtString: RegTypeStr := 'String';
      else
        RegTypeStr := 'Unknown';
      end;

      if FIntervals[i].IsSpilled then
        StatusStr := 'SPILLED'
      else
        StatusStr := 'R' + IntToStr(FIntervals[i].RegIndex);

      WriteLn(F, Format('| %s | %s | %d | %d | %d | %d | %s | %s |',
        [FIntervals[i].VarName, RegTypeStr, FIntervals[i].Priority,
         FIntervals[i].StartPos, FIntervals[i].EndPos, FIntervals[i].RefCount,
         IntToStr(FIntervals[i].RegIndex), StatusStr]));
    end;
  finally
    CloseFile(F);
  end;
end;

function TRegAllocationMap.GetIntervalCount: Integer;
begin
  Result := Length(FIntervals);
end;

function TRegAllocationMap.GetAllocatedRegisterCount(RegType: TRegisterType): Integer;
var
  RegSet: set of 0..15;
  i: Integer;
begin
  case RegType of
    rtInt:    RegSet := FIntRegsFree;
    rtFloat:  RegSet := FFloatRegsFree;
    rtString: RegSet := FStrRegsFree;
  else
    Exit(0);
  end;
  
  // Count allocated registers (not in free set)
  Result := 0;
  for i := 0 to 15 do
    if not (i in RegSet) then
      Inc(Result);
end;

{ ============================================================================= }
{ TLivenessAnalyzer Implementation }
{ ============================================================================= }

constructor TLivenessAnalyzer.Create;
begin
  inherited Create;
  FVariableUses := TStringList.Create;
  FVariableUses.Sorted := True;
  FVariableUses.Duplicates := dupAccept;
  SetLength(FLoopStack, 0);
end;

destructor TLivenessAnalyzer.Destroy;
begin
  FVariableUses.Free;
  SetLength(FLoopStack, 0);
  inherited Destroy;
end;

procedure TLivenessAnalyzer.AnalyzeAST(RootNode: TASTNode);
begin
  if RootNode = nil then
    Exit;

  FVariableUses.Clear;
  SetLength(FLoopStack, 0);
  VisitNode(RootNode, 0);
end;

procedure TLivenessAnalyzer.PushLoopContext(const VarName: string; StartPos: Integer);
var
  Ctx: TLoopContext;
begin
  Ctx.LoopVar := UpperCase(VarName);
  Ctx.StartPos := StartPos;
  Ctx.EndPos := StartPos;  // Will be extended when NEXT is found
  SetLength(FLoopStack, Length(FLoopStack) + 1);
  FLoopStack[High(FLoopStack)] := Ctx;
end;

procedure TLivenessAnalyzer.PopLoopContext(EndPos: Integer);
var
  Ctx: TLoopContext;
  i: Integer;
begin
  if Length(FLoopStack) = 0 then
    Exit;

  // Pop last loop context
  Ctx := FLoopStack[High(FLoopStack)];
  SetLength(FLoopStack, Length(FLoopStack) - 1);

  // Extend loop variable's live range to include entire loop body
  // Add additional uses at EndPos to ensure interval extends through loop
  for i := Ctx.StartPos to EndPos do
    FVariableUses.Add(Ctx.LoopVar + '=' + IntToStr(i));
end;

procedure TLivenessAnalyzer.VisitNode(Node: TASTNode; Position: Integer);
var
  Child: TASTNode;
  i: Integer;
  ParentIsArray: Boolean;
  VarName: string;
begin
  if Node = nil then
    Exit;

  // PHASE 4F: Exclude arrays from register allocation
  // Arrays must remain in memory (cannot be stored in registers)

  // Skip DIM declarations entirely (these are array definitions)
  if Node.NodeType = antDim then
    Exit;

  // For array accesses (FLAGS(i)), skip the array name but visit the index expressions
  if Node.NodeType = antArrayAccess then
  begin
    // Child[0] is the array name (skip it)
    // Child[1] is the expression list (visit indices for their variables)
    if Node.ChildCount > 1 then
    begin
      Child := Node.GetChild(1);  // antExpressionList
      VisitNode(Child, Position + 1);
    end;
    Exit;
  end;

  // PHASE 4F FIX: Handle FOR loops - extend loop variable live range through loop body
  if Node.NodeType = antForLoop then
  begin
    // Child[0] is loop variable (antIdentifier)
    if (Node.ChildCount > 0) and (Node.Child[0].NodeType = antIdentifier) then
    begin
      VarName := UpperCase(string(Node.Child[0].Value));
      PushLoopContext(VarName, Position);
    end;
    // Visit children normally (start/end/step expressions)
    for i := 0 to Node.ChildCount - 1 do
    begin
      Child := Node.GetChild(i);
      VisitNode(Child, Position + i + 1);
    end;
    Exit;
  end;

  // PHASE 4F FIX: Handle NEXT - close loop context and extend variable interval
  if Node.NodeType = antNext then
  begin
    PopLoopContext(Position);
    Exit;
  end;

  // Record scalar variable uses (not arrays)
  if Node.NodeType = antIdentifier then
  begin
    // PHASE 4F Step 2 FIX: Normalize to UPPERCASE (BASIC is case-insensitive)
    // This prevents i/I, j/J from being treated as separate variables
    VarName := UpperCase(string(Node.Value));
    // Store: "VarName=Position"
    FVariableUses.Add(VarName + '=' + IntToStr(Position));
  end;

  // SESSION 11C FIX: Handle antLineNumber specially to group assignments on same line
  // Problem: Line "50 f1=f2: f2=f3" has two assignments with different positions,
  // causing F1.EndPos=11, F3.StartPos=12 → register conflict when allocator reuses R18.
  // Solution: All children of antLineNumber (assignments, prints, etc.) get SAME position.
  if Node.NodeType = antLineNumber then
  begin
    // All statements on same BASIC line share position (crucial for swap patterns like f1=f2:f2=f3)
    for i := 0 to Node.ChildCount - 1 do
    begin
      Child := Node.GetChild(i);
      VisitNode(Child, Position + 1);  // All children at Position+1
    end;
    Exit;
  end;

  // Recursively visit children
  // FIX: Don't increment position for every child - this causes premature interval closure
  // Only increment position for major constructs (assignments, statements)
  // All children of a single statement share the same position
  for i := 0 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);
    // Only increment position for top-level statements (program children)
    if Node.NodeType = antProgram then
      VisitNode(Child, Position + i + 1)
    else
      VisitNode(Child, Position);  // Share position within same statement
  end;
end;

function TLivenessAnalyzer.InferVariableType(Node: TASTNode): TRegisterType;
var
  VarName: string;
begin
  // Type inference based on BASIC variable naming conventions
  // BASIC rules: A$ = string, A% = integer, A = FLOAT (default!)
  Result := rtUnknown;
  
  if Node = nil then
    Exit;
  
  VarName := string(Node.Value);  // Direct variant-to-string conversion
  
  // Check suffix to determine type
  if EndsText('$', VarName) then
    Result := rtString      // String variable (e.g., NAME$)
  else if EndsText('%', VarName) then
    Result := rtInt         // Integer variable (e.g., COUNT%)
  else
    Result := rtFloat;      // DEFAULT: All variables without suffix are FLOAT in BASIC!
end;

procedure TLivenessAnalyzer.ExtractIntervals(AllocMap: TRegAllocationMap);
var
  i: Integer;
  VarName: string;
  Position: Integer;
  EqualPos: Integer;
  VarType: TRegisterType;
  VarNameUpper: string;
begin
  for i := 0 to FVariableUses.Count - 1 do
  begin
    EqualPos := Pos('=', FVariableUses[i]);
    if EqualPos > 0 then
    begin
      VarName := Copy(FVariableUses[i], 1, EqualPos - 1);
      Position := StrToIntDef(Copy(FVariableUses[i], EqualPos + 1, MaxInt), 0);

      // PHASE 4F Step 3 FIX: Loop counters should be Int for performance!
      VarNameUpper := UpperCase(VarName);
      if (VarNameUpper = 'I') or (VarNameUpper = 'J') or (VarNameUpper = 'K') or
         (VarNameUpper = 'I1') or (VarNameUpper = 'J1') or (VarNameUpper = 'K1') or
         (VarNameUpper = 'I2') or (VarNameUpper = 'J2') or (VarNameUpper = 'K2') then
        VarType := rtInt  // Loop counters are Int!
      // Infer type from variable name suffix (BASIC conventions: $ = string, % = int, else = float)
      else if EndsText('$', VarName) then
        VarType := rtString
      else if EndsText('%', VarName) then
        VarType := rtInt
      else
        VarType := rtFloat;  // DEFAULT in BASIC!

      AllocMap.AddVariableUse(VarName, i, Position, VarType);
    end;
  end;
end;

function TRegAllocationMap.GetHighestAllocatedRegister(RegType: TRegisterType): Integer;
var
  i, MaxReg: Integer;
begin
  Result := -1;  // No registers allocated
  MaxReg := -1;

  // Scan all intervals for the highest allocated register of the requested type
  for i := 0 to High(FIntervals) do
  begin
    if not FIntervals[i].IsSpilled and
       (FIntervals[i].VarType = RegType) and
       (FIntervals[i].RegIndex > MaxReg) then
      MaxReg := FIntervals[i].RegIndex;
  end;

  Result := MaxReg;
end;

procedure TRegAllocationMap.PopulateRegisterUsage(var RegUsage: array of Boolean);
var
  i, RegIdx: Integer;
begin
  // Mark all allocated registers as occupied
  // This allows AllocateTypedRegister to skip them when allocating new registers
  for i := 0 to High(FIntervals) do
  begin
    if not FIntervals[i].IsSpilled then
    begin
      RegIdx := FIntervals[i].RegIndex;
      // RegUsage is 0..47 (R0-R15 Int, R16-R31 Float, S0-S15 String)
      if (RegIdx >= 0) and (RegIdx <= High(RegUsage)) then
        RegUsage[RegIdx] := True;
    end;
  end;
end;

end.
