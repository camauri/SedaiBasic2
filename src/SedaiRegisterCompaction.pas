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
  Unit: SedaiRegisterCompaction

  Purpose: Compact register allocation by remapping sparse register numbers
           to a dense, contiguous range. This improves cache locality by
           reducing the working set size.

  Algorithm:
    1. Scan all instructions to find which registers are actually used
    2. Build a mapping from old register numbers to new compact numbers
    3. Rewrite all instructions with the new register numbers

  Benefits:
    - Reduces register file size (R420 → R40-60 typical)
    - Improves L1/L2 cache hit rate
    - Critical for compute-intensive loops (N-body, matrix ops)

  Phase: Post-allocation optimization
  Author: Sedai Project - Performance Optimization
  Date: 2025-01-28
  ============================================================================ }

unit SedaiRegisterCompaction;

{$mode objfpc}{$H+}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, SedaiBytecodeTypes, SedaiSSATypes, SedaiSuperinstructions;

type
  { TRegisterCompactor - Compacts sparse register allocation }
  TRegisterCompactor = class
  private
    FProgram: TBytecodeProgram;
    FIntRegMap: array of Integer;    // Old int reg → new int reg (-1 = unused)
    FFloatRegMap: array of Integer;  // Old float reg → new float reg
    FStringRegMap: array of Integer; // Old string reg → new string reg
    FMaxOldIntReg: Integer;
    FMaxOldFloatReg: Integer;
    FMaxOldStringReg: Integer;
    FNewIntRegCount: Integer;
    FNewFloatRegCount: Integer;
    FNewStringRegCount: Integer;

    { Scan all instructions to find used registers }
    procedure ScanUsedRegisters;

    { Build compact register mappings }
    procedure BuildMappings;

    { Rewrite all instructions with new register numbers }
    procedure RewriteInstructions;

    { Rewrite array metadata (DimRegisters) with new register numbers }
    procedure RewriteArrayMetadata;

    { Check if opcode uses Dest as integer register }
    function DestIsIntReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Dest as float register }
    function DestIsFloatReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Dest as string register }
    function DestIsStringReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Src1 as integer register }
    function Src1IsIntReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Src1 as float register }
    function Src1IsFloatReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Src2 as integer register }
    function Src2IsIntReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Src2 as float register }
    function Src2IsFloatReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Src1 as string register }
    function Src1IsStringReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Src2 as string register }
    function Src2IsStringReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode READS Dest as int register (e.g., ArrayStore) }
    function DestReadIsIntReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode READS Dest as float register (e.g., ArrayStoreFloat) }
    function DestReadIsFloatReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode READS Dest as string register (e.g., ArrayStoreString) }
    function DestReadIsStringReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Immediate field as float register index
      This is needed for FMA and related superinstructions where Immediate
      stores the 'c' register (accumulator) instead of a constant }
    function ImmediateIsFloatReg(OpCode: TBytecodeOp): Boolean;

    { Mark a register as used }
    procedure MarkIntRegUsed(Reg: Integer);
    procedure MarkFloatRegUsed(Reg: Integer);
    procedure MarkStringRegUsed(Reg: Integer);

  public
    constructor Create(Prog: TBytecodeProgram);
    destructor Destroy; override;

    { Run compaction - returns reduction count }
    function Run: Integer;

    { Statistics }
    property OldIntRegCount: Integer read FMaxOldIntReg;
    property OldFloatRegCount: Integer read FMaxOldFloatReg;
    property NewIntRegCount: Integer read FNewIntRegCount;
    property NewFloatRegCount: Integer read FNewFloatRegCount;
  end;

{ Convenience function }
function RunRegisterCompaction(Prog: TBytecodeProgram): Integer;

implementation

{$IFDEF DEBUG_REGALLOC}
uses SedaiDebug;
{$ENDIF}

const
  MAX_REGISTERS = 1024;  // Maximum register index we support

{ TRegisterCompactor }

constructor TRegisterCompactor.Create(Prog: TBytecodeProgram);
begin
  inherited Create;
  FProgram := Prog;
  FMaxOldIntReg := 0;
  FMaxOldFloatReg := 0;
  FMaxOldStringReg := 0;
  FNewIntRegCount := 0;
  FNewFloatRegCount := 0;
  FNewStringRegCount := 0;
end;

destructor TRegisterCompactor.Destroy;
begin
  inherited;
end;

function TRegisterCompactor.DestIsIntReg(OpCode: TBytecodeOp): Boolean;
var
  Op: Integer;
begin
  Op := Ord(OpCode);

  // Handle superinstructions FIRST (opcodes >= 100) to avoid enum range issues
  if Op >= 100 then
  begin
    case Op of
      // Fused arithmetic-to-dest (Int): Dest = Dest op Src1 (Dest is BOTH read and written)
      120, 121, 122,  // bcAddIntTo, bcSubIntTo, bcMulIntTo
      // Fused constant arithmetic (Int): Dest = Src1 op Immediate
      140, 141, 142,  // bcAddIntConst, bcSubIntConst, bcMulIntConst
      // Fused loop increment-and-branch (Int): Dest = counter (both read and written)
      190, 191, 192, 193,  // bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt
      // Self-increment/decrement (Int): Dest = Dest op Src1 (Dest is BOTH read and written)
      251, 252,  // bcAddIntSelf, bcSubIntSelf
      // Array Load to register (Int): Dest = arr[idx] (Dest is WRITTEN)
      253:  // bcArrayLoadIntTo
        Result := True;
      // NOTE: Fused compare-and-branch (100-105) do NOT have a Dest register!
      // They only use Src1, Src2 for comparison and Immediate for jump target
    else
      Result := False;
    end;
    Exit;
  end;

  // Check base opcodes (values < 100, safe for enum set check)
  Result := OpCode in [
    // Integer operations
    bcLoadConstInt, bcCopyInt, bcAddInt, bcSubInt, bcMulInt, bcDivInt,
    bcModInt, bcNegInt,
    // Comparison results (stored as int)
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpLeInt, bcCmpGtInt, bcCmpGeInt,
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpLeFloat, bcCmpGtFloat, bcCmpGeFloat,
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString,
    // Logical operations (result is int 0/1)
    bcLogicalAnd, bcLogicalOr, bcLogicalNot,
    // Conversions to int
    bcFloatToInt, bcStringToInt,
    // String operations returning int
    bcStrLen,
    // Input
    bcInputInt,
    // Typed array load (int) - Dest is WRITTEN
    bcArrayLoadInt
    // NOTE: bcArrayStoreInt uses Dest as SOURCE (read), handled by DestReadIsIntReg
  ];
end;

function TRegisterCompactor.DestIsFloatReg(OpCode: TBytecodeOp): Boolean;
var
  Op: Integer;
begin
  Op := Ord(OpCode);

  // Handle superinstructions FIRST (opcodes >= 100) to avoid enum range issues
  if Op >= 100 then
  begin
    case Op of
      // Fused arithmetic-to-dest (Float): Dest = Dest op Src1 (Dest is BOTH read and written)
      130, 131, 132, 133,  // bcAddFloatTo, bcSubFloatTo, bcMulFloatTo, bcDivFloatTo
      // Fused constant arithmetic (Float): Dest = Src1 op Immediate
      150, 151, 152, 153,  // bcAddFloatConst, bcSubFloatConst, bcMulFloatConst, bcDivFloatConst
      // NEW: FMA superinstructions - Dest is float
      200, 201, 202, 203,  // bcMulAddFloat, bcMulSubFloat, bcMulAddToFloat, bcMulSubToFloat
      // NEW: Array Load + Arithmetic - Dest is float
      210, 211, 212,       // bcArrayLoadAddFloat, bcArrayLoadSubFloat, bcArrayLoadDivAddFloat
      // NEW: Square-Sum patterns - Dest is float
      220, 221,            // bcSquareSumFloat, bcAddSquareFloat
      // NEW: Mul-Mul and Add-Sqrt - Dest is float
      230, 231:            // bcMulMulFloat, bcAddSqrtFloat
        Result := True;
      // NOTE: Fused compare-and-branch (110-115) do NOT have a Dest register!
    else
      Result := False;
    end;
    Exit;
  end;

  // Check base opcodes (values < 100, safe for enum set check)
  Result := OpCode in [
    // Float operations
    bcLoadConstFloat, bcCopyFloat, bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat,
    bcNegFloat, bcPowFloat,
    // Math functions
    bcMathSqr, bcMathSin, bcMathCos, bcMathTan, bcMathAtn,
    bcMathExp, bcMathLog, bcMathAbs, bcMathSgn, bcMathInt, bcMathRnd,
    // Conversion to float
    bcIntToFloat, bcStringToFloat,
    // Input
    bcInputFloat,
    // Typed array load (float) - Dest is WRITTEN
    bcArrayLoadFloat
    // NOTE: bcArrayStoreFloat uses Dest as SOURCE (read), handled by DestReadIsFloatReg
  ];
end;

function TRegisterCompactor.DestIsStringReg(OpCode: TBytecodeOp): Boolean;
var
  Op: Integer;
begin
  Op := Ord(OpCode);

  // Handle superinstructions FIRST (opcodes >= 100) to avoid enum range issues
  if Op >= 100 then
  begin
    // No superinstructions currently have string Dest register
    Result := False;
    Exit;
  end;

  // Check base opcodes (values < 100, safe for enum set check)
  Result := OpCode in [
    bcLoadConstString, bcCopyString, bcStrConcat,
    bcStrLeft, bcStrRight, bcStrMid,
    bcIntToString, bcFloatToString,
    bcInputString,
    // Typed array load (string) - Dest is WRITTEN
    bcArrayLoadString
    // NOTE: bcArrayStoreString uses Dest as SOURCE (read), handled by DestReadIsStringReg
  ];
end;

function TRegisterCompactor.Src1IsIntReg(OpCode: TBytecodeOp): Boolean;
var
  Op: Integer;
begin
  Op := Ord(OpCode);

  // Handle superinstructions FIRST (opcodes >= 100) to avoid enum range issues
  if Op >= 100 then
  begin
    case Op of
      // Fused arithmetic-to-dest (Int): Src1 is int operand
      120, 121, 122,  // bcAddIntTo, bcSubIntTo, bcMulIntTo
      // Fused constant arithmetic (Int): Src1 is source register
      140, 141, 142,  // bcAddIntConst, bcSubIntConst, bcMulIntConst
      // Fused compare-and-branch (Int): Src1 is first comparison operand
      100, 101, 102, 103, 104, 105,  // bcBranchEqInt..bcBranchGeInt
      // Fused compare-zero-and-branch (Int): Src1 is the register being compared
      160, 161,  // bcBranchEqZeroInt, bcBranchNeZeroInt
      // Fused loop increment-and-branch (Int): Src1 is step register
      190, 191, 192, 193,  // bcAddIntToBranchLe..bcSubIntToBranchGt
      // Self-increment/decrement (Int): Src1 is the amount register
      251, 252:  // bcAddIntSelf, bcSubIntSelf
        Result := True;
    else
      Result := False;
    end;
    Exit;
  end;

  // Check base opcodes (values < 100, safe for enum set check)
  Result := OpCode in [
    // Int arithmetic
    bcCopyInt, bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt, bcNegInt,
    // Int comparisons
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpLeInt, bcCmpGtInt, bcCmpGeInt,
    // Conversion from int
    bcIntToFloat, bcIntToString,
    // Branch on int (comparison result)
    bcJumpIfZero, bcJumpIfNotZero,
    // Logical operations
    bcLogicalAnd, bcLogicalOr, bcLogicalNot,
    // String operations with int param
    bcStrLeft, bcStrRight
  ];
end;

function TRegisterCompactor.Src1IsFloatReg(OpCode: TBytecodeOp): Boolean;
var
  Op: Integer;
begin
  Op := Ord(OpCode);

  // Handle superinstructions FIRST (opcodes >= 100) to avoid enum range issues
  if Op >= 100 then
  begin
    case Op of
      // Fused arithmetic-to-dest (Float): Src1 is float operand
      130, 131, 132, 133,  // bcAddFloatTo..bcDivFloatTo
      // Fused constant arithmetic (Float): Src1 is source register
      150, 151, 152, 153,  // bcAddFloatConst..bcDivFloatConst
      // Fused compare-and-branch (Float): Src1 is first comparison operand
      110, 111, 112, 113, 114, 115,  // bcBranchEqFloat..bcBranchGeFloat
      // Fused compare-zero-and-branch (Float): Src1 is the register being compared
      170, 171,  // bcBranchEqZeroFloat, bcBranchNeZeroFloat
      // NEW: FMA superinstructions - Src1 is float multiplicand 'a'
      200, 201, 202, 203,  // bcMulAddFloat, bcMulSubFloat, bcMulAddToFloat, bcMulSubToFloat
      // NEW: Square-Sum patterns - Src1 is float
      220, 221,            // bcSquareSumFloat, bcAddSquareFloat
      // NEW: Mul-Mul and Add-Sqrt - Src1 is float
      230, 231:            // bcMulMulFloat, bcAddSqrtFloat
        Result := True;
    else
      Result := False;
    end;
    Exit;
  end;

  // Check base opcodes (values < 100, safe for enum set check)
  Result := OpCode in [
    // Float arithmetic
    bcCopyFloat, bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat, bcNegFloat, bcPowFloat,
    // Float comparisons
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpLeFloat, bcCmpGtFloat, bcCmpGeFloat,
    // Math functions
    bcMathSqr, bcMathSin, bcMathCos, bcMathTan, bcMathAtn,
    bcMathExp, bcMathLog, bcMathAbs, bcMathSgn, bcMathInt, bcMathRnd,
    // Conversion from float
    bcFloatToInt, bcFloatToString,
    // Print float value (bcPrint/bcPrintLn use float register in Src1)
    bcPrint, bcPrintLn
  ];
end;

function TRegisterCompactor.Src2IsIntReg(OpCode: TBytecodeOp): Boolean;
var
  Op: Integer;
begin
  Op := Ord(OpCode);

  // Handle superinstructions FIRST (opcodes >= 100) to avoid enum range issues
  if Op >= 100 then
  begin
    case Op of
      // Fused compare-and-branch (Int): Src2 is second comparison operand
      100, 101, 102, 103, 104, 105,  // bcBranchEqInt..bcBranchGeInt
      // Fused array-store-constant: Src2 is the int index register
      180, 181, 182,  // bcArrayStoreIntConst, bcArrayStoreFloatConst, bcArrayStoreStringConst
      // Fused loop increment-and-branch (Int): Src2 is limit register
      190, 191, 192, 193,  // bcAddIntToBranchLe..bcSubIntToBranchGt
      // NEW: Array Load + Arithmetic: Src2 is int index register
      210, 211, 212,       // bcArrayLoadAddFloat, bcArrayLoadSubFloat, bcArrayLoadDivAddFloat
      // NEW: Array Load + Branch: Src2 is int index register
      240, 241,            // bcArrayLoadIntBranchNZ, bcArrayLoadIntBranchZ
      // NEW: Array Swap: Src2 is idx1 register (int)
      250,                 // bcArraySwapInt
      // NEW: Array Load to register: Src2 is int index register
      253:                 // bcArrayLoadIntTo
        Result := True;
    else
      Result := False;
    end;
    Exit;
  end;

  // Check base opcodes (values < 100, safe for enum set check)
  Result := OpCode in [
    // Int arithmetic (second operand)
    bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt,
    // Int comparisons (second operand)
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpLeInt, bcCmpGtInt, bcCmpGeInt,
    // Logical operations (second operand)
    bcLogicalAnd, bcLogicalOr,
    // Typed array operations: Src2 is always int (linear index)
    bcArrayLoadInt, bcArrayLoadFloat, bcArrayLoadString,
    bcArrayStoreInt, bcArrayStoreFloat, bcArrayStoreString,
    // String operations with int second param
    bcStrMid  // Mid$(str, start, length) - start is Src1, length is Src2
  ];
end;

function TRegisterCompactor.Src2IsFloatReg(OpCode: TBytecodeOp): Boolean;
var
  Op: Integer;
begin
  Op := Ord(OpCode);

  // Handle superinstructions FIRST (opcodes >= 100) to avoid enum range issues
  if Op >= 100 then
  begin
    case Op of
      // Fused compare-and-branch (Float): Src2 is second comparison operand
      110, 111, 112, 113, 114, 115,  // bcBranchEqFloat..bcBranchGeFloat
      // NEW: FMA superinstructions - Src2 is float multiplicand 'b'
      200, 201, 202, 203,  // bcMulAddFloat, bcMulSubFloat, bcMulAddToFloat, bcMulSubToFloat
      // NEW: Square-Sum patterns - Src2 is float
      220, 221,            // bcSquareSumFloat, bcAddSquareFloat
      // NEW: Mul-Mul and Add-Sqrt - Src2 is float
      230, 231:            // bcMulMulFloat, bcAddSqrtFloat
        Result := True;
    else
      Result := False;
    end;
    Exit;
  end;

  // Check base opcodes (values < 100, safe for enum set check)
  Result := OpCode in [
    // Float arithmetic (second operand)
    bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat, bcPowFloat,
    // Float comparisons (second operand)
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpLeFloat, bcCmpGtFloat, bcCmpGeFloat
  ];
end;

function TRegisterCompactor.Src1IsStringReg(OpCode: TBytecodeOp): Boolean;
var
  Op: Integer;
begin
  Op := Ord(OpCode);

  // Handle superinstructions FIRST (opcodes >= 100) to avoid enum range issues
  if Op >= 100 then
  begin
    // No superinstructions currently use Src1 as string register
    Result := False;
    Exit;
  end;

  // Check base opcodes (values < 100, safe for enum set check)
  Result := OpCode in [
    // String copy (source)
    bcCopyString,
    // String concatenation (first operand)
    bcStrConcat,
    // String comparison (first operand)
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString,
    // String functions that take string as first param
    bcStrLeft, bcStrRight, bcStrMid,
    // String length (source)
    bcStrLen,
    // Print string (source)
    bcPrintString, bcPrintStringLn
  ];
end;

function TRegisterCompactor.Src2IsStringReg(OpCode: TBytecodeOp): Boolean;
var
  Op: Integer;
begin
  Op := Ord(OpCode);

  // Handle superinstructions FIRST (opcodes >= 100) to avoid enum range issues
  if Op >= 100 then
  begin
    // No superinstructions currently use Src2 as string register
    Result := False;
    Exit;
  end;

  // Check base opcodes (values < 100, safe for enum set check)
  Result := OpCode in [
    // String concatenation (second operand)
    bcStrConcat,
    // String comparison (second operand)
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString
  ];
end;

function TRegisterCompactor.DestReadIsIntReg(OpCode: TBytecodeOp): Boolean;
var
  Op: Integer;
begin
  { These opcodes use Dest as a SOURCE register (read, not write).
    This is critical for ArrayStore where Dest holds the VALUE to store. }
  Op := Ord(OpCode);

  // Handle superinstructions
  if Op >= 100 then
  begin
    case Op of
      // ArraySwapInt: Dest = idx2 register (int) - READ, not written
      250:  // bcArraySwapInt
        Result := True;
    else
      Result := False;
    end;
    Exit;
  end;

  // ArrayStoreInt: Dest = value register (int) - READ, not written
  Result := OpCode = bcArrayStoreInt;
end;

function TRegisterCompactor.DestReadIsFloatReg(OpCode: TBytecodeOp): Boolean;
var
  Op: Integer;
begin
  { These opcodes use Dest as a SOURCE register (read, not write).
    This is critical for ArrayStore where Dest holds the VALUE to store. }
  Op := Ord(OpCode);

  // No superinstructions currently read Dest as float (ArrayStoreFloatConst uses Immediate)
  if Op >= 100 then
  begin
    Result := False;
    Exit;
  end;

  // ArrayStoreFloat: Dest = value register (float) - READ, not written
  Result := OpCode = bcArrayStoreFloat;
end;

function TRegisterCompactor.DestReadIsStringReg(OpCode: TBytecodeOp): Boolean;
var
  Op: Integer;
begin
  { These opcodes use Dest as a SOURCE register (read, not write).
    This is critical for ArrayStore where Dest holds the VALUE to store. }
  Op := Ord(OpCode);

  // No superinstructions currently read Dest as string
  if Op >= 100 then
  begin
    Result := False;
    Exit;
  end;

  // ArrayStoreString: Dest = value register (string) - READ, not written
  Result := OpCode = bcArrayStoreString;
end;

function TRegisterCompactor.ImmediateIsFloatReg(OpCode: TBytecodeOp): Boolean;
var
  Op: Integer;
begin
  { These superinstructions store a FLOAT REGISTER INDEX in the Immediate field
    instead of a constant value. The Immediate field needs to be remapped
    during register compaction.

    Layout for these opcodes:
    - bcMulAddFloat (200):    dest = Immediate[c] + Src1[a] * Src2[b]
    - bcMulSubFloat (201):    dest = Immediate[c] - Src1[a] * Src2[b]
    - bcArrayLoadAddFloat (210): dest = Immediate[acc] + arr[Src1][Src2]
    - bcArrayLoadSubFloat (211): dest = Immediate[acc] - arr[Src1][Src2]
    - bcArrayLoadDivAddFloat (212): dest = Immediate[acc] + arr/denom (complex)
    - bcMulMulFloat (230):    dest = Src1 * Src2 * Immediate[extra]
  }
  Op := Ord(OpCode);
  Result := Op in [200, 201, 210, 211, 212, 230];
end;

procedure TRegisterCompactor.MarkIntRegUsed(Reg: Integer);
begin
  if (Reg >= 0) and (Reg < MAX_REGISTERS) then
  begin
    if Reg > FMaxOldIntReg then
      FMaxOldIntReg := Reg;
    if Reg < Length(FIntRegMap) then
      FIntRegMap[Reg] := 0;  // Mark as used (will be remapped later)
  end;
end;

procedure TRegisterCompactor.MarkFloatRegUsed(Reg: Integer);
begin
  if (Reg >= 0) and (Reg < MAX_REGISTERS) then
  begin
    if Reg > FMaxOldFloatReg then
      FMaxOldFloatReg := Reg;
    if Reg < Length(FFloatRegMap) then
      FFloatRegMap[Reg] := 0;  // Mark as used
  end;
end;

procedure TRegisterCompactor.MarkStringRegUsed(Reg: Integer);
begin
  if (Reg >= 0) and (Reg < MAX_REGISTERS) then
  begin
    if Reg > FMaxOldStringReg then
      FMaxOldStringReg := Reg;
    if Reg < Length(FStringRegMap) then
      FStringRegMap[Reg] := 0;  // Mark as used
  end;
end;

procedure TRegisterCompactor.ScanUsedRegisters;
var
  i: Integer;
  Instr: TBytecodeInstruction;
  OpCode: TBytecodeOp;
begin
  // Initialize maps with -1 (unused)
  SetLength(FIntRegMap, MAX_REGISTERS);
  SetLength(FFloatRegMap, MAX_REGISTERS);
  SetLength(FStringRegMap, MAX_REGISTERS);

  for i := 0 to MAX_REGISTERS - 1 do
  begin
    FIntRegMap[i] := -1;
    FFloatRegMap[i] := -1;
    FStringRegMap[i] := -1;
  end;

  // Scan all instructions
  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    Instr := FProgram.GetInstruction(i);
    // IMPORTANT: For superinstructions (opcode >= 100), the cast to TBytecodeOp
    // produces an out-of-range value. The Is*Reg functions handle this by
    // checking Ord(OpCode) >= 100 first and using integer comparisons.
    OpCode := TBytecodeOp(Instr.OpCode);

    // Skip NOPs
    if Instr.OpCode = Byte(bcNop) then
      Continue;

    // Check Dest register (written)
    if DestIsIntReg(OpCode) then
      MarkIntRegUsed(Instr.Dest)
    else if DestIsFloatReg(OpCode) then
      MarkFloatRegUsed(Instr.Dest)
    else if DestIsStringReg(OpCode) then
      MarkStringRegUsed(Instr.Dest);

    // Check Dest register when used as SOURCE (e.g., ArrayStore value)
    if DestReadIsIntReg(OpCode) then
      MarkIntRegUsed(Instr.Dest)
    else if DestReadIsFloatReg(OpCode) then
      MarkFloatRegUsed(Instr.Dest)
    else if DestReadIsStringReg(OpCode) then
      MarkStringRegUsed(Instr.Dest);

    // Check Src1 register
    if Src1IsIntReg(OpCode) then
      MarkIntRegUsed(Instr.Src1)
    else if Src1IsFloatReg(OpCode) then
      MarkFloatRegUsed(Instr.Src1)
    else if Src1IsStringReg(OpCode) then
      MarkStringRegUsed(Instr.Src1);

    // Check Src2 register
    if Src2IsIntReg(OpCode) then
      MarkIntRegUsed(Instr.Src2)
    else if Src2IsFloatReg(OpCode) then
      MarkFloatRegUsed(Instr.Src2)
    else if Src2IsStringReg(OpCode) then
      MarkStringRegUsed(Instr.Src2);

    // Check Immediate field when it contains a float register index
    // (for FMA and related superinstructions)
    if ImmediateIsFloatReg(OpCode) then
      MarkFloatRegUsed(Instr.Immediate);

    // bcArrayDim has no register operands (info is in metadata)
    // ArrayLoad: Dest is written, Src1 is array index (metadata), Src2 is int index register
    // ArrayStore: Dest is value (READ!), Src1 is array index (metadata), Src2 is int index register
  end;

  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
  begin
    WriteLn('[RegCompact] Scanned registers:');
    WriteLn('[RegCompact]   Max old int reg: ', FMaxOldIntReg);
    WriteLn('[RegCompact]   Max old float reg: ', FMaxOldFloatReg);
    WriteLn('[RegCompact]   Max old string reg: ', FMaxOldStringReg);
  end;
  {$ENDIF}
end;

procedure TRegisterCompactor.BuildMappings;
var
  i: Integer;
begin
  // Build compact mapping for int registers
  FNewIntRegCount := 0;
  for i := 0 to FMaxOldIntReg do
  begin
    if FIntRegMap[i] >= 0 then  // Was marked as used
    begin
      FIntRegMap[i] := FNewIntRegCount;
      Inc(FNewIntRegCount);
    end;
  end;

  // Build compact mapping for float registers
  FNewFloatRegCount := 0;
  for i := 0 to FMaxOldFloatReg do
  begin
    if FFloatRegMap[i] >= 0 then
    begin
      FFloatRegMap[i] := FNewFloatRegCount;
      Inc(FNewFloatRegCount);
    end;
  end;

  // Build compact mapping for string registers
  FNewStringRegCount := 0;
  for i := 0 to FMaxOldStringReg do
  begin
    if FStringRegMap[i] >= 0 then
    begin
      FStringRegMap[i] := FNewStringRegCount;
      Inc(FNewStringRegCount);
    end;
  end;

  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
  begin
    WriteLn('[RegCompact] Built mappings:');
    WriteLn('[RegCompact]   Int: ', FMaxOldIntReg + 1, ' → ', FNewIntRegCount);
    WriteLn('[RegCompact]   Float: ', FMaxOldFloatReg + 1, ' → ', FNewFloatRegCount);
    WriteLn('[RegCompact]   String: ', FMaxOldStringReg + 1, ' → ', FNewStringRegCount);
  end;
  {$ENDIF}
end;

procedure TRegisterCompactor.RewriteInstructions;
var
  i: Integer;
  Instr: TBytecodeInstruction;
  OpCode: TBytecodeOp;
  Modified: Boolean;
begin
  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    Instr := FProgram.GetInstruction(i);
    // Note: OpCode can be a base TBytecodeOp OR a superinstruction constant (100+)
    OpCode := TBytecodeOp(Instr.OpCode);
    Modified := False;

    // Skip NOPs
    if Instr.OpCode = Byte(bcNop) then
      Continue;

    {$IFDEF DEBUG_REGALLOC}
    // Debug output for LoadConstInt with high registers
    if DebugRegAlloc and (OpCode = bcLoadConstInt) and (Instr.Dest > 30) then
    begin
      WriteLn('[RegCompact DEBUG] Instr ', i, ': OpCode=', Ord(OpCode),
              ' Dest=', Instr.Dest, ' DestIsIntReg=', DestIsIntReg(OpCode));
      if Instr.Dest < Length(FIntRegMap) then
        WriteLn('[RegCompact DEBUG]   FIntRegMap[', Instr.Dest, ']=', FIntRegMap[Instr.Dest]);
    end;
    {$ENDIF}

    // Remap Dest register (when Dest is a DESTINATION - written)
    // Note: Dest/Src1/Src2 are Word (unsigned), so >= 0 check is unnecessary
    if DestIsIntReg(OpCode) then
    begin
      if (Instr.Dest < Length(FIntRegMap)) and (FIntRegMap[Instr.Dest] >= 0) then
      begin
        Instr.Dest := FIntRegMap[Instr.Dest];
        Modified := True;
      end;
    end
    else if DestIsFloatReg(OpCode) then
    begin
      if (Instr.Dest < Length(FFloatRegMap)) and (FFloatRegMap[Instr.Dest] >= 0) then
      begin
        Instr.Dest := FFloatRegMap[Instr.Dest];
        Modified := True;
      end;
    end
    else if DestIsStringReg(OpCode) then
    begin
      if (Instr.Dest < Length(FStringRegMap)) and (FStringRegMap[Instr.Dest] >= 0) then
      begin
        Instr.Dest := FStringRegMap[Instr.Dest];
        Modified := True;
      end;
    end
    // Remap Dest register when Dest is a SOURCE (read) - e.g., ArrayStore
    else if DestReadIsIntReg(OpCode) then
    begin
      if (Instr.Dest < Length(FIntRegMap)) and (FIntRegMap[Instr.Dest] >= 0) then
      begin
        Instr.Dest := FIntRegMap[Instr.Dest];
        Modified := True;
      end;
    end
    else if DestReadIsFloatReg(OpCode) then
    begin
      if (Instr.Dest < Length(FFloatRegMap)) and (FFloatRegMap[Instr.Dest] >= 0) then
      begin
        Instr.Dest := FFloatRegMap[Instr.Dest];
        Modified := True;
      end;
    end
    else if DestReadIsStringReg(OpCode) then
    begin
      if (Instr.Dest < Length(FStringRegMap)) and (FStringRegMap[Instr.Dest] >= 0) then
      begin
        Instr.Dest := FStringRegMap[Instr.Dest];
        Modified := True;
      end;
    end;

    // Remap Src1 register
    if Src1IsIntReg(OpCode) then
    begin
      if (Instr.Src1 < Length(FIntRegMap)) and (FIntRegMap[Instr.Src1] >= 0) then
      begin
        Instr.Src1 := FIntRegMap[Instr.Src1];
        Modified := True;
      end;
    end
    else if Src1IsFloatReg(OpCode) then
    begin
      if (Instr.Src1 < Length(FFloatRegMap)) and (FFloatRegMap[Instr.Src1] >= 0) then
      begin
        Instr.Src1 := FFloatRegMap[Instr.Src1];
        Modified := True;
      end;
    end
    else if Src1IsStringReg(OpCode) then
    begin
      if (Instr.Src1 < Length(FStringRegMap)) and (FStringRegMap[Instr.Src1] >= 0) then
      begin
        Instr.Src1 := FStringRegMap[Instr.Src1];
        Modified := True;
      end;
    end;

    // Remap Src2 register
    if Src2IsIntReg(OpCode) then
    begin
      if (Instr.Src2 < Length(FIntRegMap)) and (FIntRegMap[Instr.Src2] >= 0) then
      begin
        Instr.Src2 := FIntRegMap[Instr.Src2];
        Modified := True;
      end;
    end
    else if Src2IsFloatReg(OpCode) then
    begin
      if (Instr.Src2 < Length(FFloatRegMap)) and (FFloatRegMap[Instr.Src2] >= 0) then
      begin
        Instr.Src2 := FFloatRegMap[Instr.Src2];
        Modified := True;
      end;
    end
    else if Src2IsStringReg(OpCode) then
    begin
      if (Instr.Src2 < Length(FStringRegMap)) and (FStringRegMap[Instr.Src2] >= 0) then
      begin
        Instr.Src2 := FStringRegMap[Instr.Src2];
        Modified := True;
      end;
    end;

    // Remap Immediate field when it contains a float register index
    // (for FMA and related superinstructions: 200, 201, 210, 211, 212, 230)
    if ImmediateIsFloatReg(OpCode) then
    begin
      if (Instr.Immediate < Length(FFloatRegMap)) and (FFloatRegMap[Instr.Immediate] >= 0) then
      begin
        Instr.Immediate := FFloatRegMap[Instr.Immediate];
        Modified := True;
      end;
    end;

    // NOTE: Typed array opcodes (bcArrayLoadInt/Float/String, bcArrayStoreInt/Float/String)
    // are already handled by the DestIsIntReg/FloatReg/StringReg and Src2IsIntReg blocks above.
    // Src1 for these opcodes is ArrayIndex (metadata), NOT a register - don't remap.
    // bcArrayDim has no register operands (dimension info is in metadata).

    // Write back modified instruction
    if Modified then
      FProgram.SetInstruction(i, Instr);
  end;
end;

procedure TRegisterCompactor.RewriteArrayMetadata;
var
  i, j: Integer;
  ArrInfo: TSSAArrayInfo;
  OldReg, NewReg: Integer;
  Modified: Boolean;
begin
  // Iterate through all arrays in the program
  for i := 0 to FProgram.GetArrayCount - 1 do
  begin
    ArrInfo := FProgram.GetArray(i);
    Modified := False;

    // Remap DimRegisters for variable-sized dimensions
    for j := 0 to Length(ArrInfo.DimRegisters) - 1 do
    begin
      OldReg := ArrInfo.DimRegisters[j];
      if OldReg >= 0 then  // -1 means no register (constant dimension)
      begin
        // Determine the register type and remap accordingly
        case ArrInfo.DimRegTypes[j] of
          srtInt:
          begin
            if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
            begin
              NewReg := FIntRegMap[OldReg];
              if NewReg <> OldReg then
              begin
                ArrInfo.DimRegisters[j] := NewReg;
                Modified := True;
                {$IFDEF DEBUG_REGALLOC}
                if DebugRegAlloc then
                  WriteLn('[RegCompact] Array "', ArrInfo.Name, '" dim ', j,
                          ': IntR', OldReg, ' -> IntR', NewReg);
                {$ENDIF}
              end;
            end;
          end;
          srtFloat:
          begin
            if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
            begin
              NewReg := FFloatRegMap[OldReg];
              if NewReg <> OldReg then
              begin
                ArrInfo.DimRegisters[j] := NewReg;
                Modified := True;
                {$IFDEF DEBUG_REGALLOC}
                if DebugRegAlloc then
                  WriteLn('[RegCompact] Array "', ArrInfo.Name, '" dim ', j,
                          ': FloatR', OldReg, ' -> FloatR', NewReg);
                {$ENDIF}
              end;
            end;
          end;
          // String registers typically aren't used for array dimensions
        end;
      end;
    end;

    // Write back modified array info
    if Modified then
      FProgram.SetArray(i, ArrInfo);
  end;
end;

function TRegisterCompactor.Run: Integer;
var
  OldTotal, NewTotal: Integer;
begin
  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
    WriteLn('[RegCompact] Starting register compaction...');
  {$ENDIF}

  // Step 1: Scan to find used registers
  ScanUsedRegisters;

  // Step 2: Build compact mappings
  BuildMappings;

  // Step 3: Rewrite instructions
  RewriteInstructions;

  // Step 4: Rewrite array metadata (DimRegisters for variable-sized arrays)
  RewriteArrayMetadata;

  // Calculate reduction
  OldTotal := (FMaxOldIntReg + 1) + (FMaxOldFloatReg + 1) + (FMaxOldStringReg + 1);
  NewTotal := FNewIntRegCount + FNewFloatRegCount + FNewStringRegCount;
  Result := OldTotal - NewTotal;

  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
  begin
    WriteLn('[RegCompact] Compaction complete:');
    WriteLn('[RegCompact]   Int registers: ', FMaxOldIntReg + 1, ' -> ', FNewIntRegCount);
    WriteLn('[RegCompact]   Float registers: ', FMaxOldFloatReg + 1, ' -> ', FNewFloatRegCount);
    WriteLn('[RegCompact]   String registers: ', FMaxOldStringReg + 1, ' -> ', FNewStringRegCount);
    WriteLn('[RegCompact]   Total: ', OldTotal, ' -> ', NewTotal, ' (reduction: ', Result, ')');
  end;
  {$ENDIF}
end;

function RunRegisterCompaction(Prog: TBytecodeProgram): Integer;
var
  Compactor: TRegisterCompactor;
begin
  Compactor := TRegisterCompactor.Create(Prog);
  try
    Result := Compactor.Run;
  finally
    Compactor.Free;
  end;
end;

end.
