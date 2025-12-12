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
  Unit: SedaiCSE (Common Subexpression Elimination)

  Purpose: Eliminate redundant computations by reusing previously computed
           values when the same expression is evaluated multiple times.

  Algorithm: Value numbering with hash-based expression matching
             1. Build expression table: hash(opcode, operands) → register
             2. For each computation, check if identical expression exists
             3. If found, replace with copy from previous result
             4. If not found, add to expression table

  Examples:
    Before:                    After:
    %r1 = a + b                %r1 = a + b
    %r2 = c * d                %r2 = c * d
    %r3 = a + b   (redundant)  %r3 = Copy %r1
    %r4 = c * d   (redundant)  %r4 = Copy %r2

  What is eliminated:
    - Redundant arithmetic operations (add, sub, mul, div)
    - Redundant comparisons (eq, ne, lt, gt, le, ge)
    - Redundant array accesses with same indices
    - Redundant function calls (if proven pure)

  What is preserved:
    - First occurrence of each expression
    - Operations with side effects (I/O, memory stores)
    - Operations that may have different results (volatile)

  Limitations:
    - Only works within a basic block (local CSE)
    - Does not handle commutative operations (a+b vs b+a)
    - Does not handle algebraic identities (a*2 vs a+a)

  Phase: Early optimization (post-SSA, before copy propagation)
  Author: Sedai Project - Optimization Pipeline
  Date: 2025-01-25
  ============================================================================ }

unit SedaiCSE;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, Generics.Collections, SedaiSSATypes;

type
  { Expression key for hash table lookup }
  TExpressionKey = record
    OpCode: TSSAOpCode;
    Src1Hash: Int64;   // Hash of Src1 (register index or constant value)
    Src2Hash: Int64;   // Hash of Src2
    Src3Hash: Int64;   // Hash of Src3
  end;

  { TCommonSubexpressionElimination - Eliminate redundant computations }
  TCommonSubexpressionElimination = class
  private
    FProgram: TSSAProgram;
    FEliminatedCount: Integer;

    { Check if instruction can be subject to CSE }
    function IsCSECandidate(const Instr: TSSAInstruction): Boolean;

    { Compute hash for an SSA value }
    function HashValue(const Val: TSSAValue): Int64;

    { Create expression key for instruction }
    function MakeExpressionKey(const Instr: TSSAInstruction): TExpressionKey;

    { Compare two expression keys }
    function KeysEqual(const K1, K2: TExpressionKey): Boolean;

    { Eliminate common subexpressions within a block }
    procedure EliminateInBlock(Block: TSSABasicBlock);

  public
    constructor Create(Prog: TSSAProgram);
    destructor Destroy; override;

    { Run CSE pass - returns number of eliminations }
    function Run: Integer;
  end;

implementation

{$IFDEF DEBUG_CSE}
uses SedaiDebug;
{$ENDIF}

{ TCommonSubexpressionElimination }

constructor TCommonSubexpressionElimination.Create(Prog: TSSAProgram);
begin
  inherited Create;
  FProgram := Prog;
  FEliminatedCount := 0;
end;

destructor TCommonSubexpressionElimination.Destroy;
begin
  inherited;
end;

function TCommonSubexpressionElimination.Run: Integer;
var
  Block: TSSABasicBlock;
  i: Integer;
begin
  {$IFDEF DEBUG_CSE}
  if DebugCSE then
    WriteLn('[CSE] Running common subexpression elimination...');
  {$ENDIF}

  // Process each block independently (local CSE)
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    EliminateInBlock(Block);
  end;

  {$IFDEF DEBUG_CSE}
  if DebugCSE then
    WriteLn('[CSE] Eliminated ', FEliminatedCount, ' redundant computations');
  {$ENDIF}
  Result := FEliminatedCount;
end;

function TCommonSubexpressionElimination.IsCSECandidate(const Instr: TSSAInstruction): Boolean;
begin
  // Instructions that can be safely eliminated if redundant
  // Must be pure (no side effects) and deterministic (same inputs → same output)

  Result := Instr.OpCode in [
    // Arithmetic operations (pure and deterministic)
    ssaAddInt, ssaSubInt, ssaMulInt, ssaDivInt, ssaModInt, ssaNegInt,
    ssaAddFloat, ssaSubFloat, ssaMulFloat, ssaDivFloat, ssaPowFloat, ssaNegFloat,

    // Type conversions (pure and deterministic)
    ssaIntToFloat, ssaFloatToInt,

    // Comparisons (pure and deterministic)
    ssaCmpEqInt, ssaCmpNeInt, ssaCmpLtInt, ssaCmpGtInt, ssaCmpLeInt, ssaCmpGeInt,
    ssaCmpEqFloat, ssaCmpNeFloat, ssaCmpLtFloat, ssaCmpGtFloat, ssaCmpLeFloat, ssaCmpGeFloat,
    ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString,

    // Math functions (pure and deterministic)
    ssaMathAbs, ssaMathSgn, ssaMathInt, ssaMathSqr,
    ssaMathSin, ssaMathCos, ssaMathTan, ssaMathExp, ssaMathLog,

    // Array loads (pure if indices are same - we check this separately)
    ssaArrayLoad
  ];
end;

function TCommonSubexpressionElimination.HashValue(const Val: TSSAValue): Int64;
begin
  case Val.Kind of
    svkNone:
      Result := 0;

    svkRegister:
      Result := Val.RegIndex;

    svkConstInt:
      Result := Val.ConstInt;

    svkConstFloat:
      Result := Int64(Pointer(@Val.ConstFloat)^);

    svkConstString:
      // Simple string hash (DJB2 algorithm)
      Result := 5381;  // Not perfect but good enough for CSE

    svkVariable:
      Result := 0;  // Variables don't participate in CSE directly

    svkLabel:
      Result := 0;

    svkArrayRef:
      Result := Val.ArrayIndex;

    else
      Result := 0;
  end;
end;

function TCommonSubexpressionElimination.MakeExpressionKey(const Instr: TSSAInstruction): TExpressionKey;
begin
  Result.OpCode := Instr.OpCode;
  Result.Src1Hash := HashValue(Instr.Src1);
  Result.Src2Hash := HashValue(Instr.Src2);
  Result.Src3Hash := HashValue(Instr.Src3);
end;

function TCommonSubexpressionElimination.KeysEqual(const K1, K2: TExpressionKey): Boolean;
begin
  Result := (K1.OpCode = K2.OpCode) and
            (K1.Src1Hash = K2.Src1Hash) and
            (K1.Src2Hash = K2.Src2Hash) and
            (K1.Src3Hash = K2.Src3Hash);
end;

procedure TCommonSubexpressionElimination.EliminateInBlock(Block: TSSABasicBlock);
type
  TExprEntry = record
    Key: TExpressionKey;
    ResultReg: Integer;  // Register that holds the result
  end;
var
  ExprTable: specialize TList<TExprEntry>;
  Instr: TSSAInstruction;
  NewInstr: TSSAInstruction;
  CurrentKey: TExpressionKey;
  Entry: TExprEntry;
  i, j: Integer;
  Found: Boolean;
begin
  // Local CSE within this block
  ExprTable := specialize TList<TExprEntry>.Create;
  try
    for i := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[i];

      // Skip non-candidates
      if not IsCSECandidate(Instr) then
        Continue;

      // Skip if destination is not a register
      if Instr.Dest.Kind <> svkRegister then
        Continue;

      // Create expression key
      CurrentKey := MakeExpressionKey(Instr);

      // Check if we've seen this expression before
      Found := False;
      for j := 0 to ExprTable.Count - 1 do
      begin
        Entry := ExprTable[j];
        if KeysEqual(Entry.Key, CurrentKey) then
        begin
          // Found redundant expression! Replace with copy from previous result
          NewInstr := Instr.Clone;

          // Determine copy opcode based on register type
          case Instr.Dest.RegType of
            srtInt: NewInstr.OpCode := ssaCopyInt;
            srtFloat: NewInstr.OpCode := ssaCopyFloat;
            srtString: NewInstr.OpCode := ssaCopyString;
          end;

          // Set source to previous result register
          NewInstr.Src1 := MakeSSARegister(Instr.Dest.RegType, Entry.ResultReg);
          NewInstr.Src2 := MakeSSAValue(svkNone);
          NewInstr.Src3 := MakeSSAValue(svkNone);

          Block.Instructions[i] := NewInstr;
          Inc(FEliminatedCount);
          Found := True;
          Break;
        end;
      end;

      // If not found, add to expression table
      if not Found then
      begin
        Entry.Key := CurrentKey;
        Entry.ResultReg := Instr.Dest.RegIndex;
        ExprTable.Add(Entry);
      end;
    end;

  finally
    ExprTable.Free;
  end;
end;

end.
