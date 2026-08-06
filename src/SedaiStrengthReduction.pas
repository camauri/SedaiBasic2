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
  Unit: SedaiStrengthReduction (Strength Reduction)

  Purpose: Replace expensive operations with cheaper equivalents that produce
           the same result.

  Algorithm: Pattern matching on arithmetic operations
             1. Identify expensive operations (multiply, divide, power)
             2. Check if they can be replaced with cheaper alternatives
             3. Apply transformations

  Examples:
    Before:                After:
    %r1 = x * 2            %r1 = x + x
    %r2 = x * 4            %r2 = x << 2
    %r3 = x * 8            %r3 = x << 3
    %r4 = x / 2            %r4 = x >> 1
    %r5 = x / 4            %r5 = x >> 2
    %r6 = x ^ 2            %r6 = x * x

  Strength Reduction Rules:
    Multiplication by power of 2 → Left shift
      x * 2 → x << 1
      x * 4 → x << 2
      x * 8 → x << 3
      etc.

    Division by power of 2 → Right shift (for unsigned)
      x / 2 → x >> 1
      x / 4 → x >> 2
      etc.

    Multiplication by small constant → Addition
      x * 2 → x + x
      x * 3 → x + x + x (or (x << 1) + x)

    Power by small constant → Multiplication
      x ^ 2 → x * x
      x ^ 3 → x * x * x

  Note: Shift operations are only valid for integers.
        For floats, we use addition-based strength reduction only.

  Phase: Early optimization (post-algebraic, before CSE)
  Author: Sedai Project - Optimization Pipeline
  Date: 2025-01-25
  ============================================================================ }

unit SedaiStrengthReduction;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, Contnrs, SedaiSSATypes, SedaiDominators;

type
  { What defines a register, for the constant resolvers. A register is usable as a constant only
    when it has EXACTLY ONE definition (see RegisterReassignedNonConst), so "the first definition
    found by scanning" and "the only definition" are the same thing - which is why one entry per
    register is enough to reproduce the old scans exactly. }
  TSRDefInfo = record
    Defs: Integer;              // how many instructions write this register
    Computed: Boolean;          // at least one of them is not a const load / copy / int->float
    Op: TSSAOpCode;             // the single definition's opcode (valid only when Defs = 1)
    Src1: TSSAValue;            // and its Src1
  end;
  PSRDefInfo = ^TSRDefInfo;
  { TInductionVariable - Basic induction variable info }
  TInductionVariable = record
    VarRegIndex: Integer;     // Register index of the IV
    VarRegType: TSSARegisterType;
    VarVersion: Integer;      // SSA version (0 for BASIC global semantics)
    InitValue: TSSAValue;     // Initial value (outside loop)
    StepValue: TSSAValue;     // Step constant (e.g., 1 for i = i + 1)
    StepIsAdd: Boolean;       // True for addition, False for subtraction
    HeaderBlock: TSSABasicBlock; // Loop header
    UpdateInstr: TSSAInstruction; // The i = i + step instruction
  end;

  { TLoopInfoSR - Loop information for strength reduction }
  TLoopInfoSR = class
    Header: TSSABasicBlock;
    Blocks: TFPList;  // List of TSSABasicBlock
    BackEdgeSources: TFPList;
    constructor Create(AHeader: TSSABasicBlock);
    destructor Destroy; override;
    function ContainsBlock(Block: TSSABasicBlock): Boolean;
  end;

  { TStrengthReduction - Replace expensive operations with cheaper ones }
  TStrengthReduction = class
  private
    FProgram: TSSAProgram;
    FReductions: Integer;
    // Def index: one entry per (register bank, index), rebuilt whenever this pass mutates the
    // program. It replaces three full program scans per QUERY - RegisterReassignedNonConst, then
    // GetConstInt's two passes - with one scan per rebuild. Those scans were 18 of the 43 seconds
    // it took to compile a 14k-line program, for a pass whose net effect on it is zero
    // instructions: the cost was never the work, it was asking a per-value question with a
    // program-wide search.
    FDefIdx: array[0..2] of array of TSRDefInfo;
    FDefIdxStamp: Integer;        // FReductions when the index was built (-1 = never)
    FLoops: TObjectList;  // Owns TLoopInfoSR objects
    FDominatorMap: TFPHashList;
    FInductionVars: array of TInductionVariable;

    { Check if value is a constant integer }
    function GetConstInt(const Val: TSSAValue; out ConstVal: Int64): Boolean;

    { Check if value is a constant float }
    function GetConstFloat(const Val: TSSAValue; out ConstVal: Double): Boolean;

    { True if the register is NOT a single never-reassigned constant (e.g. a loop accumulator). }
    function RegisterReassignedNonConst(const Val: TSSAValue): Boolean;

    { Check if integer is power of 2, return log2 if yes }
    function IsPowerOfTwo(N: Int64; out Log2: Integer): Boolean;

    { Reduce multiplication operations }
    function ReduceMultiplication(const Instr: TSSAInstruction): TSSAInstruction;

    { Reduce division operations }
    function ReduceDivision(const Instr: TSSAInstruction): TSSAInstruction;

    { Reduce power operations }
    function ReducePower(const Instr: TSSAInstruction): TSSAInstruction;

    { Process all blocks for simple reductions }
    procedure ReduceBlocks;

    { === Loop-based strength reduction === }

    { Build dominator map from program's dominator tree }
    procedure BuildDominatorMap;

    { Find all natural loops via back-edges }
    procedure FindLoops;

    { Check if edge (From -> Target) is a back-edge }
    procedure EnsureDefIndex;
    function DefOf(const Val: TSSAValue): PSRDefInfo;
    procedure AbsorbDefChange(const OldInstr, NewInstr: TSSAInstruction);
    function IsBackEdge(From, Target: TSSABasicBlock): Boolean;
    function IsCallEdge(From, Target: TSSABasicBlock): Boolean;   // recursion, not a loop

    { Compute all blocks in a natural loop }
    procedure ComputeLoopBlocks(Loop: TLoopInfoSR; BackEdgeSource: TSSABasicBlock);
    function LoopCallsSubroutine(Loop: TLoopInfoSR): Boolean;  // any GOSUB inside? then the CFG cannot support IV reasoning

    { Find basic induction variables in all loops }
    procedure FindInductionVariables;

    { Check if instruction is IV update (i = i + const or i = i - const) }
    function IsIVUpdate(Instr: TSSAInstruction; Loop: TLoopInfoSR;
                        out IVRegIndex: Integer; out IVRegType: TSSARegisterType;
                        out StepValue: TSSAValue; out IsAdd: Boolean): Boolean;

    { Apply strength reduction to loop IV-dependent multiplications }
    procedure ReduceIVMultiplications;

    { Find the initial value of an IV from the preheader }
    function FindIVInitValue(const IV: TInductionVariable; PreHeader: TSSABasicBlock;
                             out InitVal: TSSAValue): Boolean;

  public
    constructor Create(Prog: TSSAProgram);
    destructor Destroy; override;

    { Run strength reduction pass }
    function Run: Integer;
  end;

implementation

var
  // SR_DIAG=1: attribution inside this pass, now that it is the dominant one (10.1 s of 14.4).
  GSRDiag: Integer = -1;
  GSRRebuilds: Integer = 0;

function SRDiagOn: Boolean;
begin
  if GSRDiag < 0 then
    if GetEnvironmentVariable('SR_DIAG') = '1' then GSRDiag := 1 else GSRDiag := 0;
  Result := GSRDiag = 1;
end;


{$IFDEF DEBUG_STRENGTH}
uses SedaiDebug;
{$ENDIF}

{ TLoopInfoSR }

constructor TLoopInfoSR.Create(AHeader: TSSABasicBlock);
begin
  inherited Create;
  Header := AHeader;
  Blocks := TFPList.Create;
  BackEdgeSources := TFPList.Create;
  Blocks.Add(Pointer(Header));
end;

destructor TLoopInfoSR.Destroy;
begin
  Blocks.Free;
  BackEdgeSources.Free;
  inherited;
end;

function TStrengthReduction.LoopCallsSubroutine(Loop: TLoopInfoSR): Boolean;
// Does any block of this loop contain a GOSUB (ssaCall)? See the soundness guard at the IV rewrite: the
// GOSUB CFG has a call edge and no return edge, so nothing that reasons about flow through it is sound.
var
  j, k: Integer;
  Block: TSSABasicBlock;
begin
  Result := False;
  for j := 0 to Loop.Blocks.Count - 1 do
  begin
    Block := TSSABasicBlock(Loop.Blocks[j]);
    for k := 0 to Block.Instructions.Count - 1 do
      if Block.Instructions[k].OpCode = ssaCall then Exit(True);
  end;
end;

function TLoopInfoSR.ContainsBlock(Block: TSSABasicBlock): Boolean;
begin
  Result := Blocks.IndexOf(Pointer(Block)) >= 0;
end;

{ TStrengthReduction }

constructor TStrengthReduction.Create(Prog: TSSAProgram);
begin
  inherited Create;
  FProgram := Prog;
  FReductions := 0;
  FDefIdxStamp := -1;                  // never built; FReductions starts at 0, so this forces a build
  FLoops := TObjectList.Create(True);  // Owns TLoopInfoSR objects
  FDominatorMap := TFPHashList.Create;
  SetLength(FInductionVars, 0);
end;

destructor TStrengthReduction.Destroy;
begin
  FLoops.Free;
  FDominatorMap.Free;
  inherited;
end;

function TStrengthReduction.Run: Integer;
var T0, T1: QWord;
begin
  // Phase A: runs in BOTH dialects. The MODERN miscompiles that once forced a skip here were not this
  // pass's fault: they were latent register bank-typing bugs (float FOR-comparison results typed in the
  // float bank; multi-CONST lists lowering only their first constant) that this pass's register shifts
  // exposed. With those fixed the pass is clean on versioned SSA — corpus 470/470, JIT net 0 mismatch.
  // If a MODERN OPTDIFF ever bisects here, suspect another latent bank-typing mismatch FIRST
  // (see memory licm-general-blocker-is-register-typing).

  {$IFDEF DEBUG_STRENGTH}
  if DebugStrength then
    WriteLn('[StrengthRed] Running strength reduction...');
  {$ENDIF}

  // Phase 1: Simple pattern-based reductions (x*2 → x+x, etc.)
  if SRDiagOn then T0 := GetTickCount64;
  ReduceBlocks;
  if SRDiagOn then begin T1 := GetTickCount64; WriteLn(ErrOutput, '[SR_DIAG] ReduceBlocks ', T1-T0, ' ms  (idx rebuilds=', GSRRebuilds, ')'); end;

  // Phase 2: Loop-based induction variable strength reduction
  // Transforms: FOR I = init TO n: J = I * const -> J starts at init*const, stride = step*const
  if SRDiagOn then T0 := GetTickCount64;
  BuildDominatorMap;
  if SRDiagOn then begin T1 := GetTickCount64; WriteLn(ErrOutput, '[SR_DIAG] BuildDominatorMap ', T1-T0, ' ms'); T0 := T1; end;
  if FDominatorMap.Count > 0 then
  begin
    FindLoops;
    if SRDiagOn then begin T1 := GetTickCount64; WriteLn(ErrOutput, '[SR_DIAG] FindLoops ', T1-T0, ' ms  loops=', FLoops.Count); T0 := T1; end;
    if FLoops.Count > 0 then
    begin
      FindInductionVariables;
      if SRDiagOn then begin T1 := GetTickCount64; WriteLn(ErrOutput, '[SR_DIAG] FindInductionVariables ', T1-T0, ' ms  ivs=', Length(FInductionVars)); T0 := T1; end;
      if Length(FInductionVars) > 0 then
        ReduceIVMultiplications;
      if SRDiagOn then begin T1 := GetTickCount64; WriteLn(ErrOutput, '[SR_DIAG] ReduceIVMultiplications ', T1-T0, ' ms  (idx rebuilds=', GSRRebuilds, ')'); end;
    end;
  end;

  {$IFDEF DEBUG_STRENGTH}
  if DebugStrength then
    WriteLn('[StrengthRed] Applied ', FReductions, ' reductions');
  {$ENDIF}
  Result := FReductions;
end;

procedure TStrengthReduction.EnsureDefIndex;
// Rebuild the def index if this pass has changed the program since it was built. Strength reduction
// rewrites instructions as it goes, so a cached answer from before a rewrite would be stale; keying
// the cache on FReductions makes that impossible to forget.
var
  MaxIdx: array[0..2] of Integer;
  bi, ii, tt: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
begin
  if FDefIdxStamp = FReductions then Exit;
  for tt := 0 to 2 do MaxIdx[tt] := -1;
  for bi := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[bi];
    for ii := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[ii];
      if Instr.Dest.Kind = svkRegister then
      begin
        tt := Ord(Instr.Dest.RegType);
        if (tt >= 0) and (tt <= 2) and (Instr.Dest.RegIndex > MaxIdx[tt]) then
          MaxIdx[tt] := Instr.Dest.RegIndex;
      end;
    end;
  end;
  for tt := 0 to 2 do
  begin
    SetLength(FDefIdx[tt], MaxIdx[tt] + 1);
    for ii := 0 to MaxIdx[tt] do
    begin
      FDefIdx[tt][ii].Defs := 0;
      FDefIdx[tt][ii].Computed := False;
    end;
  end;
  for bi := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[bi];
    for ii := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[ii];
      if Instr.Dest.Kind <> svkRegister then System.Continue;
      tt := Ord(Instr.Dest.RegType);
      if (tt < 0) or (tt > 2) then System.Continue;
      if (Instr.Dest.RegIndex < 0) or (Instr.Dest.RegIndex > MaxIdx[tt]) then System.Continue;
      with FDefIdx[tt][Instr.Dest.RegIndex] do
      begin
        Inc(Defs);
        if not OpIn(Instr.OpCode, [ssaLoadConstInt, ssaLoadConstFloat,
                                   ssaCopyInt, ssaCopyFloat, ssaIntToFloat]) then
          Computed := True;
        if Defs = 1 then begin Op := Instr.OpCode; Src1 := Instr.Src1; end;
      end;
    end;
  end;
  FDefIdxStamp := FReductions;
  Inc(GSRRebuilds);
end;

procedure TStrengthReduction.AbsorbDefChange(const OldInstr, NewInstr: TSSAInstruction);
// Fold a single rewritten instruction INTO the def index instead of throwing the index away.
// The index used to be invalidated on FReductions, and ReduceBlocks bumps that once per reduction,
// so a 14k-line program rebuilt the whole table 1632 times - 8.7 of the pass's 10.1 seconds, doing
// again the exact work the index existed to avoid. Almost none of those rebuilds could change an
// answer: a reduction turns MulInt d,x,2 into AddInt d,x,x, and d was "computed" (so not a
// constant) before and after.
//
// Exact rather than approximate: a register with more than one definition already answers "not a
// constant" from Defs alone, so only the single-definition case carries information, and there the
// new opcode and Src1 simply replace the old ones. Anything unexpected - the rewrite moving the
// destination, or an index not built yet - falls back to a rebuild by leaving the stamp stale.
var D: PSRDefInfo; t: Integer;
begin
  if FDefIdxStamp < 0 then Exit;                       // never built: nothing to absorb into
  if (OldInstr.Dest.Kind <> svkRegister) or (NewInstr.Dest.Kind <> svkRegister) then Exit;
  if (OldInstr.Dest.RegType <> NewInstr.Dest.RegType) or
     (OldInstr.Dest.RegIndex <> NewInstr.Dest.RegIndex) then Exit;   // dest moved: rebuild
  // NB: read the table DIRECTLY, not through DefOf - DefOf calls EnsureDefIndex, which would
  // rebuild the very table this is here to preserve (the stamp is already stale at this point,
  // the reduction having bumped FReductions before returning).
  t := Ord(NewInstr.Dest.RegType);
  if (t < 0) or (t > 2) then Exit;
  if (NewInstr.Dest.RegIndex < 0) or (NewInstr.Dest.RegIndex >= Length(FDefIdx[t])) then Exit;
  D := @FDefIdx[t][NewInstr.Dest.RegIndex];
  if D^.Defs = 1 then
  begin
    D^.Op := NewInstr.OpCode;
    D^.Src1 := NewInstr.Src1;
    D^.Computed := not OpIn(NewInstr.OpCode, [ssaLoadConstInt, ssaLoadConstFloat,
                                              ssaCopyInt, ssaCopyFloat, ssaIntToFloat]);
  end;
  FDefIdxStamp := FReductions;                         // the index now matches the program again
end;

function TStrengthReduction.DefOf(const Val: TSSAValue): PSRDefInfo;
var t: Integer;
begin
  Result := nil;
  if Val.Kind <> svkRegister then Exit;
  EnsureDefIndex;
  t := Ord(Val.RegType);
  if (t < 0) or (t > 2) then Exit;
  if (Val.RegIndex < 0) or (Val.RegIndex >= Length(FDefIdx[t])) then Exit;
  Result := @FDefIdx[t][Val.RegIndex];
end;

function TStrengthReduction.RegisterReassignedNonConst(const Val: TSSAValue): Boolean;
// A register may be treated as a compile-time constant by GetConstInt/GetConstFloat ONLY if it is
// written exactly once, by a constant load (or a copy/int->float that those resolvers chase). A
// register that is also written by an arithmetic instruction - typically a loop accumulator like
// `C = 0 : ... : C = C + K` - is NOT constant; reporting its initial 0 as a constant made strength
// reduction turn `B * C` into `IV * 0` (a zero-stride accumulator), miscompiling the result. This
// returns True for such registers so the const resolvers reject them.
// Answered from the def index rather than by scanning the program per query - same answer, see
// EnsureDefIndex for why that mattered.
var D: PSRDefInfo;
begin
  Result := False;
  if Val.Kind <> svkRegister then Exit;
  D := DefOf(Val);
  if D = nil then Exit(True);          // never written here: not a safe constant
  Result := D^.Computed or (D^.Defs <> 1);
end;

function TStrengthReduction.GetConstInt(const Val: TSSAValue; out ConstVal: Int64): Boolean;
// Chases a register back to a compile-time integer. The old form scanned every block twice (once
// for a LoadConstInt, once for a Copy) on top of RegisterReassignedNonConst's own scan; since a
// register that gets past that guard has exactly ONE definition, both scans could only ever find
// that definition, so the index gives the identical answer.
var
  D: PSRDefInfo;
  Depth: Integer;
  V: TSSAValue;
begin
  Result := False;
  if Val.Kind = svkConstInt then
  begin
    ConstVal := Val.ConstInt;
    Exit(True);
  end;
  if Val.Kind <> svkRegister then Exit;
  V := Val;
  // Bounded: a Copy chain can in principle close a cycle, which the recursive form would have
  // followed forever.
  for Depth := 0 to 31 do
  begin
    if RegisterReassignedNonConst(V) then Exit(False);
    D := DefOf(V);
    if D = nil then Exit(False);
    if (D^.Op = ssaLoadConstInt) and (D^.Src1.Kind = svkConstInt) then
    begin
      ConstVal := D^.Src1.ConstInt;
      Exit(True);
    end;
    if (D^.Op = ssaCopyInt) and (D^.Src1.Kind = svkRegister) then
    begin
      V := D^.Src1;                     // one level of indirection, as the old pass 2 did
      System.Continue;
    end;
    Exit(False);
  end;
end;

function TStrengthReduction.GetConstFloat(const Val: TSSAValue; out ConstVal: Double): Boolean;
// Float twin of GetConstInt. The old form scanned every block looking for a LoadConstFloat, a
// LoadConstInt or an IntToFloat writing this register, first match winning; with exactly one
// definition to find, the index answers the same question directly.
var
  D: PSRDefInfo;
  TempInt: Int64;
begin
  Result := False;
  if Val.Kind = svkConstFloat then
  begin
    ConstVal := Val.ConstFloat;
    Exit(True);
  end;
  if Val.Kind = svkConstInt then
  begin
    ConstVal := Double(Val.ConstInt);
    Exit(True);
  end;
  if Val.Kind <> svkRegister then Exit;
  if RegisterReassignedNonConst(Val) then Exit;
  D := DefOf(Val);
  if D = nil then Exit;
  if (D^.Op = ssaLoadConstFloat) and (D^.Src1.Kind = svkConstFloat) then
  begin
    ConstVal := D^.Src1.ConstFloat;
    Exit(True);
  end;
  if (D^.Op = ssaLoadConstInt) and (D^.Src1.Kind = svkConstInt) then
  begin
    ConstVal := Double(D^.Src1.ConstInt);
    Exit(True);
  end;
  if (D^.Op = ssaIntToFloat) and (D^.Src1.Kind = svkRegister) then
    if GetConstInt(D^.Src1, TempInt) then
    begin
      ConstVal := Double(TempInt);
      Exit(True);
    end;
end;

function TStrengthReduction.IsPowerOfTwo(N: Int64; out Log2: Integer): Boolean;
var
  Temp: Int64;
begin
  Result := False;
  Log2 := 0;

  if N <= 0 then
    Exit;

  // Check if N has only one bit set
  if (N and (N - 1)) = 0 then
  begin
    Result := True;
    Temp := N;
    while Temp > 1 do
    begin
      Inc(Log2);
      Temp := Temp shr 1;
    end;
  end;
end;

function TStrengthReduction.ReduceMultiplication(const Instr: TSSAInstruction): TSSAInstruction;
var
  NewInstr: TSSAInstruction;
  ConstValInt: Int64;
  ConstValFloat: Double;
  ConstVal1, ConstVal2: Double;
  ConstVal1Int, ConstVal2Int: Int64;
  Log2: Integer;
  IsConst1, IsConst2: Boolean;
  IsFloat: Boolean;
begin
  Result := Instr;
  NewInstr := Instr.Clone;

  // Handle both integer and float multiplication
  if not (OpIn(Instr.OpCode, [ssaMulInt, ssaMulFloat])) then
    Exit;

  IsFloat := (Instr.OpCode = ssaMulFloat);

  // Check for constant operands based on type
  if IsFloat then
  begin
    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
    begin
      WriteLn('[StrengthRed] ReduceMultiplication: MulFloat found, checking operands...');
      WriteLn('[StrengthRed]   Src1.Kind=', Ord(Instr.Src1.Kind), ', Src2.Kind=', Ord(Instr.Src2.Kind));
    end;
    {$ENDIF}

    // Float multiplication - check BOTH operands for constants
    IsConst1 := GetConstFloat(Instr.Src1, ConstVal1);
    IsConst2 := GetConstFloat(Instr.Src2, ConstVal2);

    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
    begin
      WriteLn('[StrengthRed]   IsConst1=', IsConst1, ', IsConst2=', IsConst2);
      if IsConst1 then
        WriteLn('[StrengthRed]   ConstVal1=', ConstVal1:0:2);
      if IsConst2 then
        WriteLn('[StrengthRed]   ConstVal2=', ConstVal2:0:2);
    end;
    {$ENDIF}

    // Check if either operand is the constant 2.0
    if IsConst1 and (Abs(ConstVal1 - 2.0) < 1e-10) then
    begin
      // Src1 is 2.0, Src2 is the variable: 2.0 * x → x + x
      NewInstr.OpCode := ssaAddFloat;
      NewInstr.Src1 := Instr.Src2;
      NewInstr.Src2 := Instr.Src2;
      Inc(FReductions);
      {$IFDEF DEBUG_STRENGTH}
      if DebugStrength then
        WriteLn('[StrengthRed]   Applied: 2.0 * x → x + x');
      {$ENDIF}
      Exit(NewInstr);
    end
    else if IsConst2 and (Abs(ConstVal2 - 2.0) < 1e-10) then
    begin
      // Src2 is 2.0, Src1 is the variable: x * 2.0 → x + x
      NewInstr.OpCode := ssaAddFloat;
      NewInstr.Src2 := Instr.Src1;
      Inc(FReductions);
      {$IFDEF DEBUG_STRENGTH}
      if DebugStrength then
        WriteLn('[StrengthRed]   Applied: x * 2.0 → x + x');
      {$ENDIF}
      Exit(NewInstr);
    end;

    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength and (IsConst1 or IsConst2) then
      WriteLn('[StrengthRed]   Constants found but not 2.0, skipping');
    {$ENDIF}
  end
  else
  begin
    // Integer multiplication - check BOTH operands for constants
    IsConst1 := GetConstInt(Instr.Src1, ConstVal1Int);
    IsConst2 := GetConstInt(Instr.Src2, ConstVal2Int);

    // Check if either operand is the constant 2
    if IsConst1 and (ConstVal1Int = 2) then
    begin
      // Src1 is 2, Src2 is the variable: 2 * x → x + x
      NewInstr.OpCode := ssaAddInt;
      NewInstr.Src1 := Instr.Src2;
      NewInstr.Src2 := Instr.Src2;
      Inc(FReductions);
      Exit(NewInstr);
    end
    else if IsConst2 and (ConstVal2Int = 2) then
    begin
      // Src2 is 2, Src1 is the variable: x * 2 → x + x
      NewInstr.OpCode := ssaAddInt;
      NewInstr.Src2 := Instr.Src1;
      Inc(FReductions);
      Exit(NewInstr);
    end;

    // TODO: Future optimizations
    // - x * 3 → (x << 1) + x (requires temp register allocation)
    // - x * power_of_2 → x << log2(power_of_2) (requires shift operations in SSA)
  end;
end;

function TStrengthReduction.ReduceDivision(const Instr: TSSAInstruction): TSSAInstruction;
var
  NewInstr: TSSAInstruction;
  ConstVal: Int64;
  Log2: Integer;
begin
  Result := Instr;
  NewInstr := Instr.Clone;

  // Only handle integer division
  if Instr.OpCode <> ssaDivInt then
    Exit;

  // Check if divisor is a constant
  if not GetConstInt(Instr.Src2, ConstVal) then
    Exit;

  // x / power_of_2 → x >> log2(power_of_2)
  // Note: This requires shift operations in SSA (not yet implemented)
  // Also, right shift is only correct for positive integers (arithmetic shift needed for signed)
  // Skip for now
  if IsPowerOfTwo(ConstVal, Log2) then
  begin
    // Would emit: NewInstr.OpCode := ssaShrInt; NewInstr.Src2 := MakeSSAConstInt(Log2);
    // But ssaShrInt doesn't exist yet, so skip
    Exit;
  end;
end;

function TStrengthReduction.ReducePower(const Instr: TSSAInstruction): TSSAInstruction;
var
  NewInstr: TSSAInstruction;
  ConstVal: Int64;
  ConstFloat: Double;
begin
  Result := Instr;
  NewInstr := Instr.Clone;

  // Check if this is a power operation
  if Instr.OpCode <> ssaPowFloat then
    Exit;

  // x ^ 2 → x * x (for small integer exponents only)
  if Instr.Src2.Kind = svkConstInt then
  begin
    ConstVal := Instr.Src2.ConstInt;
    if ConstVal = 2 then
    begin
      NewInstr.OpCode := ssaMulFloat;
      NewInstr.Src2 := Instr.Src1;  // x * x
      Inc(FReductions);
      Exit(NewInstr);
    end;
  end
  else if Instr.Src2.Kind = svkConstFloat then
  begin
    ConstFloat := Instr.Src2.ConstFloat;
    if Abs(ConstFloat - 2.0) < 1e-10 then
    begin
      NewInstr.OpCode := ssaMulFloat;
      NewInstr.Src2 := Instr.Src1;  // x * x
      Inc(FReductions);
      Exit(NewInstr);
    end;
  end;
end;

procedure TStrengthReduction.ReduceBlocks;
var
  Block: TSSABasicBlock;
  Instr, NewInstr: TSSAInstruction;
  i, j: Integer;
begin
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      // Try strength reduction
      case Instr.OpCode of
        ssaMulInt, ssaMulFloat:
          NewInstr := ReduceMultiplication(Instr);
        ssaDivInt, ssaDivFloat:
          NewInstr := ReduceDivision(Instr);
        ssaPowFloat:
          NewInstr := ReducePower(Instr);
        else
          Continue;
      end;

      // Apply if changed
      if NewInstr.OpCode <> Instr.OpCode then
      begin
        AbsorbDefChange(Instr, NewInstr);   // BEFORE the swap: Instr is freed by the assignment
        Block.Instructions[j] := NewInstr;
      end;
    end;
  end;
end;

{ ============================================================================
  Loop-based Induction Variable Strength Reduction
  ============================================================================ }

procedure TStrengthReduction.BuildDominatorMap;
var
  DomTree: TDominatorTree;
  Block, IdomBlock: TSSABasicBlock;
  i: Integer;
begin
  if not Assigned(FProgram.GetDomTree) then
  begin
    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
      WriteLn('[StrengthRed] Dominator tree not available');
    {$ENDIF}
    Exit;
  end;

  DomTree := TDominatorTree(FProgram.GetDomTree);

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    IdomBlock := DomTree.GetIdom(Block);
    if Assigned(IdomBlock) then
      FDominatorMap.Add(Format('%p', [Pointer(Block)]), Pointer(IdomBlock));
  end;

  {$IFDEF DEBUG_STRENGTH}
  if DebugStrength then
    WriteLn('[StrengthRed] Dominator map: ', FDominatorMap.Count, ' entries');
  {$ENDIF}
end;

function TStrengthReduction.IsCallEdge(From, Target: TSSABasicBlock): Boolean;
// True when From reaches Target by CALLING it. Such an edge is recursion, not a loop: the "body"
// it closes is a whole activation of the procedure, and treating it as a loop lets a loop pass
// reason about values that belong to DIFFERENT activations as if they were successive iterations
// of one. (LICM did exactly that and hoisted a per-activation value into a preheader the recursive
// calls then entered below - see the frame-relocation work.)
var
  i: Integer;
  Instr: TSSAInstruction;
begin
  Result := False;
  if (From = nil) or (Target = nil) then Exit;
  for i := 0 to From.Instructions.Count - 1 do
  begin
    Instr := TSSAInstruction(From.Instructions[i]);
    if (Instr.OpCode = ssaCallSub) or (Instr.OpCode = ssaCall) then
      if Instr.Dest.LabelName = Target.LabelName then Exit(True);
  end;
end;

function TStrengthReduction.IsBackEdge(From, Target: TSSABasicBlock): Boolean;
var
  Dom, NextDom: TSSABasicBlock;
  Steps, Idx: Integer;
begin
  Result := False;
  Dom := From;
  Steps := 0;

  while Assigned(Dom) do
  begin
    Inc(Steps);
    if Steps > 200 then Exit(False);
    if Dom = Target then Exit(True);

    Idx := FDominatorMap.FindIndexOf(Format('%p', [Pointer(Dom)]));
    if Idx < 0 then Break;

    NextDom := TSSABasicBlock(FDominatorMap.Items[Idx]);
    if not Assigned(NextDom) or (NextDom = Dom) then Break;

    Dom := NextDom;
  end;
end;

procedure TStrengthReduction.ComputeLoopBlocks(Loop: TLoopInfoSR; BackEdgeSource: TSSABasicBlock);
var
  Worklist: TFPList;
  Current, Pred: TSSABasicBlock;
  i, Iterations: Integer;
begin
  Worklist := TFPList.Create;
  try
    if not Loop.ContainsBlock(BackEdgeSource) then
    begin
      Loop.Blocks.Add(Pointer(BackEdgeSource));
      Worklist.Add(Pointer(BackEdgeSource));
    end;

    Iterations := 0;
    while Worklist.Count > 0 do
    begin
      Inc(Iterations);
      if Iterations > 10000 then Break;

      Current := TSSABasicBlock(Worklist[Worklist.Count - 1]);
      Worklist.Delete(Worklist.Count - 1);

      if Current = Loop.Header then
        Continue;

      for i := 0 to Current.Predecessors.Count - 1 do
      begin
        Pred := TSSABasicBlock(Current.Predecessors[i]);
        if not Loop.ContainsBlock(Pred) then
        begin
          Loop.Blocks.Add(Pointer(Pred));
          Worklist.Add(Pointer(Pred));
        end;
      end;
    end;
  finally
    Worklist.Free;
  end;
end;

procedure TStrengthReduction.FindLoops;
var
  Block, Succ: TSSABasicBlock;
  Loop: TLoopInfoSR;
  i, j, k: Integer;
  ExistingLoop: TLoopInfoSR;
  Found: Boolean;
begin
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];

    for j := 0 to Block.Successors.Count - 1 do
    begin
      Succ := TSSABasicBlock(Block.Successors[j]);

      if IsBackEdge(Block, Succ) and (not IsCallEdge(Block, Succ)) then
      begin
        // Check if we already have a loop with this header
        Found := False;
        for k := 0 to FLoops.Count - 1 do
        begin
          ExistingLoop := TLoopInfoSR(FLoops[k]);
          if ExistingLoop.Header = Succ then
          begin
            if ExistingLoop.BackEdgeSources.IndexOf(Pointer(Block)) < 0 then
              ExistingLoop.BackEdgeSources.Add(Pointer(Block));
            ComputeLoopBlocks(ExistingLoop, Block);
            Found := True;
            Break;
          end;
        end;

        if not Found then
        begin
          Loop := TLoopInfoSR.Create(Succ);
          Loop.BackEdgeSources.Add(Pointer(Block));
          ComputeLoopBlocks(Loop, Block);
          FLoops.Add(Loop);
        end;
      end;
    end;
  end;
end;

function TStrengthReduction.IsIVUpdate(Instr: TSSAInstruction; Loop: TLoopInfoSR;
                                       out IVRegIndex: Integer; out IVRegType: TSSARegisterType;
                                       out StepValue: TSSAValue; out IsAdd: Boolean): Boolean;
var
  DestReg: TSSAValue;
  TempInt: Int64;
  TempFloat: Double;
begin
  Result := False;

  // Check for i = i + const or i = i - const (integer or float)
  if not (OpIn(Instr.OpCode, [ssaAddInt, ssaSubInt, ssaAddFloat, ssaSubFloat])) then
    Exit;

  // Destination must be a register
  if Instr.Dest.Kind <> svkRegister then
    Exit;

  DestReg := Instr.Dest;

  // Check if Src1 is the same register as Dest (i = i + step)
  if (Instr.Src1.Kind = svkRegister) and
     (Instr.Src1.RegIndex = DestReg.RegIndex) and
     (Instr.Src1.RegType = DestReg.RegType) then
  begin
    // Src2 must be a constant (inline or in a register via LoadConst)
    if Instr.Src2.Kind in [svkConstInt, svkConstFloat] then
    begin
      IVRegIndex := DestReg.RegIndex;
      IVRegType := DestReg.RegType;
      StepValue := Instr.Src2;
      IsAdd := OpIn(Instr.OpCode, [ssaAddInt, ssaAddFloat]);
      Result := True;
      Exit;
    end
    else if (Instr.Src2.Kind = svkRegister) then
    begin
      // Check if the register contains a constant
      if OpIn(Instr.OpCode, [ssaAddInt, ssaSubInt]) and GetConstInt(Instr.Src2, TempInt) then
      begin
        IVRegIndex := DestReg.RegIndex;
        IVRegType := DestReg.RegType;
        StepValue := MakeSSAConstInt(TempInt);
        IsAdd := Instr.OpCode = ssaAddInt;
        Result := True;
        Exit;
      end
      else if OpIn(Instr.OpCode, [ssaAddFloat, ssaSubFloat]) and GetConstFloat(Instr.Src2, TempFloat) then
      begin
        IVRegIndex := DestReg.RegIndex;
        IVRegType := DestReg.RegType;
        StepValue := MakeSSAConstFloat(TempFloat);
        IsAdd := Instr.OpCode = ssaAddFloat;
        Result := True;
        Exit;
      end;
    end;
  end;

  // Check if Src2 is the same register as Dest (i = step + i) - only for addition
  if (OpIn(Instr.OpCode, [ssaAddInt, ssaAddFloat])) and
     (Instr.Src2.Kind = svkRegister) and
     (Instr.Src2.RegIndex = DestReg.RegIndex) and
     (Instr.Src2.RegType = DestReg.RegType) then
  begin
    // Src1 must be a constant (inline or in a register via LoadConst)
    if Instr.Src1.Kind in [svkConstInt, svkConstFloat] then
    begin
      IVRegIndex := DestReg.RegIndex;
      IVRegType := DestReg.RegType;
      StepValue := Instr.Src1;
      IsAdd := True;
      Result := True;
      Exit;
    end
    else if (Instr.Src1.Kind = svkRegister) then
    begin
      // Check if the register contains a constant
      if (Instr.OpCode = ssaAddInt) and GetConstInt(Instr.Src1, TempInt) then
      begin
        IVRegIndex := DestReg.RegIndex;
        IVRegType := DestReg.RegType;
        StepValue := MakeSSAConstInt(TempInt);
        IsAdd := True;
        Result := True;
        Exit;
      end
      else if (Instr.OpCode = ssaAddFloat) and GetConstFloat(Instr.Src1, TempFloat) then
      begin
        IVRegIndex := DestReg.RegIndex;
        IVRegType := DestReg.RegType;
        StepValue := MakeSSAConstFloat(TempFloat);
        IsAdd := True;
        Result := True;
        Exit;
      end;
    end;
  end;
end;

procedure TStrengthReduction.FindInductionVariables;
var
  Loop: TLoopInfoSR;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  IV: TInductionVariable;
  i, j, k: Integer;
  IVRegIndex: Integer;
  IVRegType: TSSARegisterType;
  StepValue: TSSAValue;
  IsAdd: Boolean;
  {$IFDEF DEBUG_STRENGTH}
  TempInt: Int64;
  TempFloat: Double;
  {$ENDIF}
begin
  SetLength(FInductionVars, 0);

  for i := 0 to FLoops.Count - 1 do
  begin
    Loop := TLoopInfoSR(FLoops[i]);

    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
      WriteLn('[StrengthRed] Scanning loop ', Loop.Header.LabelName, ' (', Loop.Blocks.Count, ' blocks)');
    {$ENDIF}

    // Scan all blocks in the loop for IV updates
    for j := 0 to Loop.Blocks.Count - 1 do
    begin
      Block := TSSABasicBlock(Loop.Blocks[j]);

      {$IFDEF DEBUG_STRENGTH}
      if DebugStrength then
        WriteLn('[StrengthRed]   Block ', Block.LabelName, ' has ', Block.Instructions.Count, ' instructions');
      {$ENDIF}

      for k := 0 to Block.Instructions.Count - 1 do
      begin
        Instr := Block.Instructions[k];

        {$IFDEF DEBUG_STRENGTH}
        if DebugStrength and (OpIn(Instr.OpCode, [ssaAddInt, ssaSubInt, ssaAddFloat, ssaSubFloat])) then
        begin
          WriteLn('[StrengthRed]     Add/Sub instr: ', Instr.ToString);
          // Debug: check if we can resolve the step constant
          if (Instr.Src1.Kind = svkRegister) and
             (Instr.Src1.RegIndex = Instr.Dest.RegIndex) and
             (Instr.Src1.RegType = Instr.Dest.RegType) then
          begin
            Write('[StrengthRed]       -> Potential IV, Src2=');
            if Instr.Src2.Kind = svkConstInt then
              WriteLn('const ', Instr.Src2.ConstInt)
            else if Instr.Src2.Kind = svkConstFloat then
              WriteLn('const ', Instr.Src2.ConstFloat:0:2)
            else if Instr.Src2.Kind = svkRegister then
            begin
              Write('reg INT[', Instr.Src2.RegIndex, '], ');
              if GetConstInt(Instr.Src2, TempInt) then
                WriteLn('resolved to ', TempInt)
              else if GetConstFloat(Instr.Src2, TempFloat) then
                WriteLn('resolved to ', TempFloat:0:2)
              else
                WriteLn('NOT constant');
            end
            else
              WriteLn('other');
          end;
        end;
        {$ENDIF}

        if IsIVUpdate(Instr, Loop, IVRegIndex, IVRegType, StepValue, IsAdd) then
        begin
          // Found an induction variable update
          IV.VarRegIndex := IVRegIndex;
          IV.VarRegType := IVRegType;
          IV.VarVersion := Instr.Dest.Version;
          IV.StepValue := StepValue;
          IV.StepIsAdd := IsAdd;
          IV.HeaderBlock := Loop.Header;
          IV.UpdateInstr := Instr;
          IV.InitValue.Kind := svkNone;  // We'll find this later if needed

          SetLength(FInductionVars, Length(FInductionVars) + 1);
          FInductionVars[High(FInductionVars)] := IV;

          {$IFDEF DEBUG_STRENGTH}
          if DebugStrength then
            WriteLn('[StrengthRed] Found IV: reg ', IVRegIndex, ' in loop ', Loop.Header.LabelName,
                    ' step=', SSAValueToString(StepValue), ' add=', IsAdd);
          {$ENDIF}
        end;
      end;
    end;
  end;
end;

function TStrengthReduction.FindIVInitValue(const IV: TInductionVariable;
  PreHeader: TSSABasicBlock; out InitVal: TSSAValue): Boolean;
var
  Instr: TSSAInstruction;
  i, j: Integer;
  Block: TSSABasicBlock;
  TempInt: Int64;
  TempFloat: Double;
begin
  Result := False;
  InitVal.Kind := svkNone;

  // Strategy: Look for assignments to the IV register in the preheader
  // or blocks that dominate the preheader (going backwards)

  // First, check the preheader itself
  if Assigned(PreHeader) then
  begin
    for i := PreHeader.Instructions.Count - 1 downto 0 do
    begin
      Instr := PreHeader.Instructions[i];

      // Look for LoadConst or Copy that writes to the IV register
      if (Instr.Dest.Kind = svkRegister) and
         (Instr.Dest.RegIndex = IV.VarRegIndex) and
         (Instr.Dest.RegType = IV.VarRegType) then
      begin
        // Found assignment to IV
        case Instr.OpCode of
          ssaLoadConstInt:
            if Instr.Src1.Kind = svkConstInt then
            begin
              InitVal := Instr.Src1;
              {$IFDEF DEBUG_STRENGTH}
              if DebugStrength then
                WriteLn('[StrengthRed] Found IV init in preheader: ', InitVal.ConstInt);
              {$ENDIF}
              Result := True;
              Exit;
            end;
          ssaLoadConstFloat:
            if Instr.Src1.Kind = svkConstFloat then
            begin
              InitVal := Instr.Src1;
              {$IFDEF DEBUG_STRENGTH}
              if DebugStrength then
                WriteLn('[StrengthRed] Found IV init in preheader: ', InitVal.ConstFloat:0:2);
              {$ENDIF}
              Result := True;
              Exit;
            end;
          ssaCopyInt:
            // Try to resolve the copied value
            if GetConstInt(Instr.Src1, TempInt) then
            begin
              InitVal := MakeSSAConstInt(TempInt);
              {$IFDEF DEBUG_STRENGTH}
              if DebugStrength then
                WriteLn('[StrengthRed] Found IV init via Copy: ', TempInt);
              {$ENDIF}
              Result := True;
              Exit;
            end;
          ssaCopyFloat:
            if GetConstFloat(Instr.Src1, TempFloat) then
            begin
              InitVal := MakeSSAConstFloat(TempFloat);
              {$IFDEF DEBUG_STRENGTH}
              if DebugStrength then
                WriteLn('[StrengthRed] Found IV init via Copy: ', TempFloat:0:2);
              {$ENDIF}
              Result := True;
              Exit;
            end;
          ssaIntToFloat:
            // IntToFloat means we need to find the int source
            if GetConstInt(Instr.Src1, TempInt) then
            begin
              InitVal := MakeSSAConstFloat(Double(TempInt));
              {$IFDEF DEBUG_STRENGTH}
              if DebugStrength then
                WriteLn('[StrengthRed] Found IV init via IntToFloat: ', TempInt);
              {$ENDIF}
              Result := True;
              Exit;
            end;
        end;
      end;
    end;
  end;

  // Also check all blocks before the loop (scan entire program for now)
  // This is needed for FOR loops where initialization might be in a separate block
  for j := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[j];
    // Skip blocks that are part of any loop containing the header
    // (we want blocks BEFORE the loop)
    if Block = IV.HeaderBlock then
      Continue;

    for i := Block.Instructions.Count - 1 downto 0 do
    begin
      Instr := Block.Instructions[i];

      // Look for StoreRegToVar followed by LoadVarToReg for the IV
      // or direct LoadConst to the IV register
      if (Instr.Dest.Kind = svkRegister) and
         (Instr.Dest.RegIndex = IV.VarRegIndex) and
         (Instr.Dest.RegType = IV.VarRegType) then
      begin
        case Instr.OpCode of
          ssaLoadConstInt:
            if Instr.Src1.Kind = svkConstInt then
            begin
              InitVal := Instr.Src1;
              {$IFDEF DEBUG_STRENGTH}
              if DebugStrength then
                WriteLn('[StrengthRed] Found IV init in block ', Block.LabelName, ': ', InitVal.ConstInt);
              {$ENDIF}
              Result := True;
              Exit;
            end;
          ssaLoadConstFloat:
            if Instr.Src1.Kind = svkConstFloat then
            begin
              InitVal := Instr.Src1;
              {$IFDEF DEBUG_STRENGTH}
              if DebugStrength then
                WriteLn('[StrengthRed] Found IV init in block ', Block.LabelName, ': ', InitVal.ConstFloat:0:2);
              {$ENDIF}
              Result := True;
              Exit;
            end;
        end;
      end;
    end;
  end;

  {$IFDEF DEBUG_STRENGTH}
  if DebugStrength then
    WriteLn('[StrengthRed] Could not find IV init value for reg ', IV.VarRegIndex);
  {$ENDIF}
end;

procedure TStrengthReduction.ReduceIVMultiplications;
var
  Loop: TLoopInfoSR;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  IV: TInductionVariable;
  i, j, k, m: Integer;
  MulConstInt: Int64;
  MulConstFloat: Double;
  FoundIV: Boolean;
  IVSrc1: Boolean;  // True if IV is in Src1, False if in Src2
  AccumReg: Integer;
  StrideReg: Integer;   // register holding the (constant) accumulator stride, loaded once in the preheader
  AccumRegType: TSSARegisterType;
  InitInstr, UpdateInstr, StrideInstr: TSSAInstruction;
  PreHeader: TSSABasicBlock;
  InsertPos: Integer;
  IVStepInt: Int64;
  IVStepFloat: Double;
  StrideInt: Int64;
  StrideFloat: Double;
  IVInitValue: TSSAValue;
  AccumInitInt: Int64;
  AccumInitFloat: Double;
  IVUpdatePos, PosScan: Integer;   // guard: position of the IV update vs the multiply in the block
  NestedSkip: Boolean;             // guard: multiply lives in a loop nested inside the IV's loop
  InnerLoop: TLoopInfoSR;          // candidate nested loop in the nested-loop guard

  // Initialize variables to avoid warnings
  procedure InitVars;
  begin
    MulConstInt := 0;
    MulConstFloat := 0.0;
    IVStepInt := 0;
    IVStepFloat := 0.0;
    StrideInt := 0;
    StrideFloat := 0.0;
    AccumInitInt := 0;
    AccumInitFloat := 0.0;
    IVSrc1 := False;
    FillChar(IV, SizeOf(IV), 0);
    IVInitValue.Kind := svkNone;
  end;

  { Helper to check if a value uses a specific IV }
  function UsesIV(const Val: TSSAValue; const IV: TInductionVariable): Boolean;
  begin
    Result := (Val.Kind = svkRegister) and
              (Val.RegIndex = IV.VarRegIndex) and
              (Val.RegType = IV.VarRegType);
  end;

  { The block an instruction lives in, or nil. }
  function BlockOfInstr(Instr: TSSAInstruction): TSSABasicBlock;
  var
    bi, ii: Integer;
    B: TSSABasicBlock;
  begin
    Result := nil;
    if Instr = nil then Exit;
    for bi := 0 to FProgram.Blocks.Count - 1 do
    begin
      B := FProgram.Blocks[bi];
      for ii := 0 to B.Instructions.Count - 1 do
        if B.Instructions[ii] = Instr then Exit(B);
    end;
  end;

  { Does A dominate B? Walks B's immediate-dominator chain (FDominatorMap). }
  function BlockDominates(A, B: TSSABasicBlock): Boolean;
  var
    Cur: TSSABasicBlock;
    Guard: Integer;
    Nxt: Pointer;
  begin
    Result := False;
    if (A = nil) or (B = nil) then Exit;
    Cur := B;
    Guard := 0;
    while Assigned(Cur) and (Guard < 100000) do
    begin
      if Cur = A then Exit(True);
      Nxt := FDominatorMap.Find(Format('%p', [Pointer(Cur)]));
      if Nxt = nil then Break;
      if TSSABasicBlock(Nxt) = Cur then Break;     // the entry block is its own idom
      Cur := TSSABasicBlock(Nxt);
      Inc(Guard);
    end;
  end;

  { Find pre-header block (predecessor of header that's not in the loop) }
  function FindPreHeader(Loop: TLoopInfoSR): TSSABasicBlock;
  var
    PredIdx: Integer;
    Pred: TSSABasicBlock;
  begin
    Result := nil;
    for PredIdx := 0 to Loop.Header.Predecessors.Count - 1 do
    begin
      Pred := TSSABasicBlock(Loop.Header.Predecessors[PredIdx]);
      if not Loop.ContainsBlock(Pred) then
      begin
        // Check if this looks like a pre-header (single successor = loop header)
        if (Pred.Successors.Count = 1) and
           (TSSABasicBlock(Pred.Successors[0]) = Loop.Header) then
        begin
          Result := Pred;
          Exit;
        end;
      end;
    end;
    // If no dedicated pre-header found, just use first non-loop predecessor
    for PredIdx := 0 to Loop.Header.Predecessors.Count - 1 do
    begin
      Pred := TSSABasicBlock(Loop.Header.Predecessors[PredIdx]);
      if not Loop.ContainsBlock(Pred) then
      begin
        Result := Pred;
        Exit;
      end;
    end;
  end;

  { Find instruction position after IV update in block }
  function FindPositionAfterIVUpdate(IVUpdate: TSSAInstruction; Block: TSSABasicBlock): Integer;
  var
    InstrIdx: Integer;
  begin
    for InstrIdx := 0 to Block.Instructions.Count - 1 do
    begin
      if Block.Instructions[InstrIdx] = IVUpdate then
      begin
        Result := InstrIdx + 1;
        Exit;
      end;
    end;
    Result := Block.Instructions.Count;
  end;

begin
  InitVars;  // Initialize all variables to avoid uninitialized warnings

  { Strategy for IV multiplication strength reduction:

    Original:
      FOR I = init TO N
        J = I * STRIDE
        ...
      NEXT I

    Transformed:
      ACCUM = init * STRIDE
      FOR I = init TO N
        J = ACCUM
        ...
        ACCUM = ACCUM + (step * STRIDE)  (after IV update)
      NEXT I

    The stride for the accumulator is: IV_STEP * MUL_CONST
    For example: if I goes 2,3,4,5 (init=2, step=1) and we compute I*4,
    then ACCUM goes 8,12,16,20 (init=8, stride=4)
  }

  for i := 0 to FLoops.Count - 1 do
  begin
    Loop := TLoopInfoSR(FLoops[i]);

    // SOUNDNESS GUARD (GOSUB): a GOSUB is modelled with a CALL edge to the subroutine and NO edge back --
    // control returns to the instruction after the call, and the CFG says nothing about it. So the flow
    // through a GOSUB is not modelled at all, and any "loop" the back-edge detector assembles out of those
    // edges is fiction: it happily reported one for a program with no loop in it whatsoever. Reasoning
    // about an induction variable across that is reasoning about a graph that does not describe the
    // program, and it MISCOMPILED IN SILENCE -- "Y = Y * 5" inside a subroutine was rewritten into a copy
    // of an accumulator that had never been updated, so a nested GOSUB read a stale value (20 became 10,
    // opt only; --no-opt was right). Refuse the whole loop if any of its blocks calls a subroutine. The
    // multiply stays and computes correctly; only the optimization is given up, and only where the CFG
    // cannot support it.
    if LoopCallsSubroutine(Loop) then
      Continue;

    PreHeader := FindPreHeader(Loop);

    // Scan all blocks in the loop for IV * constant patterns
    for j := 0 to Loop.Blocks.Count - 1 do
    begin
      Block := TSSABasicBlock(Loop.Blocks[j]);

      k := 0;
      while k < Block.Instructions.Count do
      begin
        Instr := Block.Instructions[k];

        // Look for multiplication operations
        if not (OpIn(Instr.OpCode, [ssaMulInt, ssaMulFloat])) then
        begin
          Inc(k);
          Continue;
        end;

        // Check if either operand is an induction variable
        FoundIV := False;
        IVSrc1 := False;

        for m := 0 to High(FInductionVars) do
        begin
          IV := FInductionVars[m];
          if IV.HeaderBlock <> Loop.Header then
            Continue;

          // Check if Src1 or Src2 is the IV
          // IMPORTANT: Skip IV * IV patterns (e.g., i% * i%) - we can only optimize IV * const
          if UsesIV(Instr.Src1, IV) and not UsesIV(Instr.Src2, IV) then
          begin
            IVSrc1 := True;
            // IV * something - check if "something" is constant
            if Instr.OpCode = ssaMulInt then
            begin
              if GetConstInt(Instr.Src2, MulConstInt) then
                FoundIV := True;
            end
            else // ssaMulFloat
            begin
              if GetConstFloat(Instr.Src2, MulConstFloat) then
                FoundIV := True;
            end;
          end
          else if UsesIV(Instr.Src2, IV) and not UsesIV(Instr.Src1, IV) then
          begin
            IVSrc1 := False;
            // something * IV - check if "something" is constant
            if Instr.OpCode = ssaMulInt then
            begin
              if GetConstInt(Instr.Src1, MulConstInt) then
                FoundIV := True;
            end
            else // ssaMulFloat
            begin
              if GetConstFloat(Instr.Src1, MulConstFloat) then
                FoundIV := True;
            end;
          end;

          if FoundIV then
            Break;
        end;

        if not FoundIV then
        begin
          Inc(k);
          Continue;
        end;

        // SOUNDNESS GUARD (nested loops): the accumulator is updated exactly once per IV-loop
        // iteration (after the IV update on the back-edge), so replacing `IV * const` with a copy
        // of the accumulator is only valid when the multiply executes once per IV-loop iteration.
        // If the multiply lives inside an INNER loop nested within the IV's loop, it executes more
        // often than the accumulator updates and would read a stale value across outer iterations
        // (e.g. nested DO WHILE: `J = i*10` computed in the inner loop reads 1,4 instead of 1,2).
        // Detect that the multiply's block belongs to a loop nested inside this one and skip — the
        // multiply stays and computes correctly.
        NestedSkip := False;
        for PosScan := 0 to FLoops.Count - 1 do
        begin
          InnerLoop := TLoopInfoSR(FLoops[PosScan]);
          if InnerLoop.Header = Loop.Header then
            Continue;
          // InnerLoop is nested in Loop when Loop contains InnerLoop's header; the multiply is
          // inside it when InnerLoop also contains the multiply's block.
          if Loop.ContainsBlock(InnerLoop.Header) and InnerLoop.ContainsBlock(Block) then
          begin
            NestedSkip := True;
            Break;
          end;
        end;
        if NestedSkip then
        begin
          Inc(k);
          Continue;
        end;

        // SOUNDNESS GUARD: this transformation assumes the FOR shape — the induction variable is
        // updated at the loop back-edge, AFTER the `IV * const` multiply. If instead the IV is
        // updated EARLIER in the SAME block as the multiply (e.g. a DO/LOOP body `N = N + 1` then
        // `J = N * 10`), the accumulator it builds is wrong (J reads 1,4 instead of 10,20). Detect
        // that case and skip — the multiply stays and computes correctly.
        //
        // ⚠️ "Earlier" is not "earlier in this block". The guard used to compare positions only
        // within Block, so it missed the same unsoundness one branch away: a DO/LOOP that does
        // `X = X + 1` in the body and then `D = D + 4 * X` inside an IF puts the multiply in a
        // DIFFERENT block, the scan found no update, and the accumulator lagged one step behind X.
        // The real question is whether the update runs before the multiply on every path, which is
        // DOMINANCE — and it answers the FOR shape correctly too, because there the update sits in
        // the latch, which does not dominate the body.
        // Cost of getting it wrong: a midpoint-circle loop ran one iteration too many, silently
        // (job/tests/bas/bug_optdiff_midpoint_loop.bas).
        IVUpdatePos := -1;
        for PosScan := 0 to Block.Instructions.Count - 1 do
          if Block.Instructions[PosScan] = IV.UpdateInstr then
          begin
            IVUpdatePos := PosScan;
            Break;
          end;
        if (IVUpdatePos >= 0) and (IVUpdatePos < k) then
        begin
          Inc(k);
          Continue;
        end;
        if (IVUpdatePos < 0) and BlockDominates(BlockOfInstr(IV.UpdateInstr), Block) then
        begin
          Inc(k);
          Continue;
        end;

        // Skip transformation if no pre-header available
        if PreHeader = nil then
        begin
          {$IFDEF DEBUG_STRENGTH}
          if DebugStrength then
            WriteLn('[StrengthRed] No pre-header for loop ', Loop.Header.LabelName, ' - skipping IV SR');
          {$ENDIF}
          Inc(k);
          Continue;
        end;

        // Get IV step value
        if IV.StepValue.Kind = svkConstInt then
          IVStepInt := IV.StepValue.ConstInt
        else if IV.StepValue.Kind = svkConstFloat then
          IVStepFloat := IV.StepValue.ConstFloat
        else
        begin
          Inc(k);
          Continue;  // Step must be constant
        end;

        // Calculate accumulator stride = IV_step * mul_const
        if Instr.OpCode = ssaMulInt then
        begin
          AccumRegType := srtInt;
          StrideInt := IVStepInt * MulConstInt;
          if not IV.StepIsAdd then StrideInt := -StrideInt;
        end
        else
        begin
          AccumRegType := srtFloat;
          if IV.StepValue.Kind = svkConstInt then
            StrideFloat := IVStepInt * MulConstFloat
          else
            StrideFloat := IVStepFloat * MulConstFloat;
          if not IV.StepIsAdd then StrideFloat := -StrideFloat;
        end;

        // Find the IV initial value to correctly initialize the accumulator
        // AccumInit = IVInit * MulConst
        if not FindIVInitValue(IV, PreHeader, IVInitValue) then
        begin
          {$IFDEF DEBUG_STRENGTH}
          if DebugStrength then
            WriteLn('[StrengthRed] Cannot find IV init value - skipping transformation');
          {$ENDIF}
          Inc(k);
          Continue;  // Skip this transformation if we can't find init value
        end;

        // Calculate accumulator initial value = IVInit * MulConst
        if Instr.OpCode = ssaMulInt then
        begin
          if IVInitValue.Kind = svkConstInt then
            AccumInitInt := IVInitValue.ConstInt * MulConstInt
          else
          begin
            Inc(k);
            Continue;  // Need integer IV for integer multiplication
          end;
        end
        else
        begin
          if IVInitValue.Kind = svkConstFloat then
            AccumInitFloat := IVInitValue.ConstFloat * MulConstFloat
          else if IVInitValue.Kind = svkConstInt then
            AccumInitFloat := IVInitValue.ConstInt * MulConstFloat
          else
          begin
            Inc(k);
            Continue;
          end;
        end;

        // Allocate new register for accumulator
        AccumReg := FProgram.AllocRegister(AccumRegType);

        {$IFDEF DEBUG_STRENGTH}
        if DebugStrength then
        begin
          if AccumRegType = srtInt then
            WriteLn('[StrengthRed] Transforming IV*', MulConstInt, ' -> accum (init=', AccumInitInt,
                    ', stride=', StrideInt, ', reg=', AccumReg, ')')
          else
            WriteLn('[StrengthRed] Transforming IV*', MulConstFloat:0:2, ' -> accum (init=', AccumInitFloat:0:2,
                    ', stride=', StrideFloat:0:2, ', reg=', AccumReg, ')');
        end;
        {$ENDIF}

        // 1. Insert initialization in pre-header: ACCUM = IVInit * MulConst
        //    Insert before the final jump instruction
        InsertPos := PreHeader.Instructions.Count - 1;
        if InsertPos < 0 then InsertPos := 0;

        if AccumRegType = srtInt then
        begin
          InitInstr := TSSAInstruction.Create(ssaLoadConstInt);
          InitInstr.Dest := MakeSSARegister(srtInt, AccumReg);
          InitInstr.Src1 := MakeSSAConstInt(AccumInitInt);
        end
        else
        begin
          InitInstr := TSSAInstruction.Create(ssaLoadConstFloat);
          InitInstr.Dest := MakeSSARegister(srtFloat, AccumReg);
          InitInstr.Src1 := MakeSSAConstFloat(AccumInitFloat);
        end;
        InitInstr.Comment := 'SR: accum init';
        PreHeader.Instructions.Insert(InsertPos, InitInstr);

        // Materialize the (constant) stride into its own register in the pre-header, so the per-iteration
        // update is a register-register add. Emitting "ACCUM = ACCUM + <const>" directly as ssaAddInt with
        // a CONSTANT Src2 is unsound: the bytecode compiler cannot put an immediate in Src2 of a
        // register-register add and defaults that operand to R0, so the accumulator would add whatever R0
        // holds (e.g. the inner loop's index) instead of the stride -- silently miscompiling nested loops.
        StrideReg := FProgram.AllocRegister(AccumRegType);
        if AccumRegType = srtInt then
        begin
          StrideInstr := TSSAInstruction.Create(ssaLoadConstInt);
          StrideInstr.Dest := MakeSSARegister(srtInt, StrideReg);
          StrideInstr.Src1 := MakeSSAConstInt(StrideInt);
        end
        else
        begin
          StrideInstr := TSSAInstruction.Create(ssaLoadConstFloat);
          StrideInstr.Dest := MakeSSARegister(srtFloat, StrideReg);
          StrideInstr.Src1 := MakeSSAConstFloat(StrideFloat);
        end;
        StrideInstr.Comment := 'SR: accum stride';
        PreHeader.Instructions.Insert(InsertPos + 1, StrideInstr);

        // 2. Replace multiplication with copy from accumulator
        //    Change: J = I * STRIDE  -->  J = ACCUM (as Copy instruction)
        if AccumRegType = srtInt then
          Instr.OpCode := ssaCopyInt
        else
          Instr.OpCode := ssaCopyFloat;
        Instr.Src1 := MakeSSARegister(AccumRegType, AccumReg);
        Instr.Src2.Kind := svkNone;
        Instr.Comment := 'SR: from accum';

        // 3. Insert accumulator update after IV update: ACCUM = ACCUM + STRIDE
        //    We need to find the back-edge block(s) where IV is updated
        //    and insert the accumulator update there
        for m := 0 to Loop.BackEdgeSources.Count - 1 do
        begin
          Block := TSSABasicBlock(Loop.BackEdgeSources[m]);

          // Find position after IV update in this block
          InsertPos := FindPositionAfterIVUpdate(IV.UpdateInstr, Block);

          // If IV update not in this block, insert at end before jump
          if InsertPos = Block.Instructions.Count then
          begin
            InsertPos := Block.Instructions.Count - 1;
            if InsertPos < 0 then InsertPos := 0;
          end;

          // ACCUM = ACCUM + STRIDE, a register-register add (STRIDE materialized above).
          if AccumRegType = srtInt then
            UpdateInstr := TSSAInstruction.Create(ssaAddInt)
          else
            UpdateInstr := TSSAInstruction.Create(ssaAddFloat);
          UpdateInstr.Dest := MakeSSARegister(AccumRegType, AccumReg);
          UpdateInstr.Src1 := MakeSSARegister(AccumRegType, AccumReg);
          UpdateInstr.Src2 := MakeSSARegister(AccumRegType, StrideReg);
          UpdateInstr.Comment := 'SR: accum update';
          Block.Instructions.Insert(InsertPos, UpdateInstr);
        end;

        Inc(FReductions);
        Inc(k);  // Move to next instruction
      end;
    end;
  end;
end;

end.
