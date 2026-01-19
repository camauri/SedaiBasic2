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
{$interfaces CORBA}
{$codepage UTF8}
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
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 0: Core VM operations ===
    // Integer operations
    bcLoadConstInt, bcCopyInt, bcAddInt, bcSubInt, bcMulInt, bcDivInt,
    bcModInt, bcNegInt,
    // Comparison results (stored as int)
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpLeInt, bcCmpGtInt, bcCmpGeInt,
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpLeFloat, bcCmpGtFloat, bcCmpGeFloat,
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString,
    // Bitwise operations (result is int)
    bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor, bcBitwiseNot,
    // Conversions to int
    bcFloatToInt, bcStringToInt,
    // === GROUP 1: String operations ===
    bcStrLen,      // String length returns int
    bcStrAsc,      // ASC(str) returns int ASCII code
    bcStrInstr,    // INSTR(haystack, needle) returns int position
    // === GROUP 3: Array operations ===
    bcArrayLoadInt,  // Typed array load (int) - Dest is WRITTEN
    // === GROUP 4: I/O operations ===
    bcInputInt,      // Input int
    bcDataReadInt,   // Read next DATA value into int register
    // === GROUP 5: Special variables ===
    bcLoadTI,         // TI: jiffies since start (int)
    bcLoadEL,         // EL: last error line number (int)
    bcLoadER,         // ER: last error code (int)
    bcFre,            // FRE: available memory (int)
    bcPeek,           // PEEK(address): read from memory (int)
    // === GROUP 7: Sprite functions ===
    bcBump,           // BUMP(n): collision bitmask (int)
    bcRspcolor,       // RSPCOLOR(n): multicolor value (int)
    bcRsprite,        // RSPRITE(sprite, attr): sprite attribute (int)
    // === GROUP 10: Graphics ===
    bcGraphicRGBA,    // Dest = RGBA result (int)
    bcGraphicRdot,    // Dest = pixel cursor info (int)
    bcGraphicGetMode, // Dest = current graphic mode (int)
    bcGraphicPos,     // POS(x): cursor column position (int)
    bcGraphicRclr,    // RCLR(n): color of source (int)
    bcGraphicRwindow, // RWINDOW(n): window info (int)
    bcGetColor,       // GETCOLOR(source): color value (int)
    // === SUPERINSTRUCTIONS ===
    // Fused arithmetic-to-dest (Int): Dest = Dest op Src1
    bcAddIntTo, bcSubIntTo, bcMulIntTo,
    // Fused constant arithmetic (Int): Dest = Src1 op Immediate
    bcAddIntConst, bcSubIntConst, bcMulIntConst,
    // Fused loop increment-and-branch (Int): Dest = counter register
    bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt,
    // Fused self-increment/decrement (Int): Dest = counter (R/W)
    bcAddIntSelf, bcSubIntSelf,
    // Fused array load to int: Dest = result
    bcArrayLoadIntTo,
    // DEC(hexstring) - result is int
    bcStrDec:
      Result := True;
    // NOTE: bcArrayStoreInt uses Dest as SOURCE (read), handled by DestReadIsIntReg
    // NOTE: bcGraphicBox uses Dest as SOURCE (y1 coordinate), handled by DestReadIsIntReg
    // NOTE: bcArraySwapInt uses Dest as idx2_reg (read), handled by DestReadIsIntReg
    // NOTE: bcArrayShiftLeft/ReverseRange use Dest as end_idx (read), handled by DestReadIsIntReg
  else
    Result := False;
  end;
end;

function TRegisterCompactor.DestIsFloatReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 0: Core VM operations ===
    bcLoadConstFloat, bcCopyFloat, bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat,
    bcNegFloat, bcPowFloat,
    // Conversion to float
    bcIntToFloat, bcStringToFloat,
    // === GROUP 1: String operations ===
    bcStrVal,      // VAL(str) - string to float
    // === GROUP 2: Math functions ===
    bcMathSqr, bcMathSin, bcMathCos, bcMathTan, bcMathAtn,
    bcMathExp, bcMathLog, bcMathAbs, bcMathSgn, bcMathInt, bcMathRnd,
    bcMathLog10, bcMathLog2, bcMathLogN,
    // === GROUP 3: Array operations ===
    bcArrayLoadFloat,  // Typed array load (float) - Dest is WRITTEN
    // === GROUP 4: I/O operations ===
    bcInputFloat,
    bcDataReadFloat,   // Read next DATA value into float register
    // === GROUP 7: Sprite functions ===
    bcRsppos,          // RSPPOS(sprite, attr): position/speed (float)
    // === SUPERINSTRUCTIONS ===
    // Fused arithmetic-to-dest (Float): Dest = Dest op Src1
    bcAddFloatTo, bcSubFloatTo, bcMulFloatTo, bcDivFloatTo,
    // Fused constant arithmetic (Float): Dest = Src1 op Immediate
    bcAddFloatConst, bcSubFloatConst, bcMulFloatConst, bcDivFloatConst,
    // Fused ArrayLoad + Arithmetic: Dest = acc op arr[idx]
    bcArrayLoadAddFloat, bcArrayLoadSubFloat, bcArrayLoadDivAddFloat,
    // Fused Multiply-Add (FMA): Dest = c op (a * b) or Dest op= a * b
    bcMulAddFloat, bcMulSubFloat, bcMulAddToFloat, bcMulSubToFloat,
    // Fused Square-Sum and Mul-Mul: Dest = x*x + y*y, dest = a*b*c, etc.
    bcSquareSumFloat, bcAddSquareFloat, bcMulMulFloat, bcAddSqrtFloat:
      Result := True;
    // NOTE: bcArrayStoreFloat uses Dest as SOURCE (read), handled by DestReadIsFloatReg
  else
    Result := False;
  end;
end;

function TRegisterCompactor.DestIsStringReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 0: Core VM operations ===
    bcLoadConstString, bcCopyString,
    bcIntToString, bcFloatToString,
    // === GROUP 1: String operations ===
    bcStrConcat, bcStrLeft, bcStrRight, bcStrMid, bcStrChr,
    bcStrStr,    // STR$(n) - number to string
    bcStrHex,    // HEX$(n) - int to hex string
    bcStrErr,    // ERR$(n) - error code to message string
    // === GROUP 3: Array operations ===
    bcArrayLoadString,  // Typed array load (string) - Dest is WRITTEN
    // === GROUP 4: I/O operations ===
    bcInputString,
    bcDataReadString,  // Read next DATA value into string register
    // === GROUP 5: Special variables ===
    bcLoadTIS,         // TI$: current time HHMMSS (string)
    bcLoadDTS,         // DT$: current date YYYYMMDD (string)
    bcLoadERRS,        // ERR$: last error message (string)
    // === GROUP 10: Graphics ===
    bcGraphicSShape:   // SSHAPE A$, x1, y1: capture screen area to string
      Result := True;
    // NOTE: bcArrayStoreString uses Dest as SOURCE (read), handled by DestReadIsStringReg
  else
    Result := False;
  end;
end;

function TRegisterCompactor.Src1IsIntReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 0: Core VM operations ===
    // Int arithmetic
    bcCopyInt, bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt, bcNegInt,
    // Int comparisons
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpLeInt, bcCmpGtInt, bcCmpGeInt,
    // Conversion from int
    bcIntToFloat, bcIntToString,
    // Branch on int (comparison result)
    bcJumpIfZero, bcJumpIfNotZero,
    // Error handling - RESUME <line> reads line number from Src1
    bcResume,
    // Bitwise operations
    bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor, bcBitwiseNot,
    // === GROUP 1: String operations with int param ===
    bcStrChr, bcStrHex, bcStrErr,
    // === GROUP 5: Memory operations ===
    bcPeek,           // PEEK(address): Src1 = address (int)
    bcPoke,           // POKE address, value: Src1 = address (int)
    // === GROUP 4: I/O operations ===
    bcPrintInt, bcPrintIntLn,
    bcPrintTab, bcPrintSpc,  // TAB(n) and SPC(n) - Src1 = count register
    // === GROUP 6: Sound operations ===
    bcSoundVol,       // Src1 = volume (int 0-15)
    bcSoundSound,     // Src1 = voice number (int)
    bcSoundEnvelope,  // Src1 = envelope slot (int 0-9)
    bcSoundTempo,     // Src1 = tempo value (int)
    // === GROUP 10: Graphics ===
    bcGraphicBox, bcGraphicSetMode, bcGraphicRGBA, bcGraphicRdot, bcGraphicGetMode,
    bcGraphicWindow,  // Src1 = col1 register (int)
    bcGraphicCircle,  // Src1 = color register (int)
    bcGraphicPaint,   // Src1 = source register (int)
    bcGraphicSShape,  // Src1 = x1 coordinate (int)
    bcGraphicColor,   // Src1 = source register (int)
    bcGraphicWidth,   // Src1 = width value (int)
    bcGraphicScale,   // Src1 = enable flag (int)
    bcGraphicRclr,    // Src1 = color source index (int)
    bcGraphicRwindow, // Src1 = info type (int)
    bcScnClr,         // Src1 = mode register (int)
    bcSetColor,       // Src1 = source register (int)
    bcGetColor,       // Src1 = source index (int)
    // === SUPERINSTRUCTIONS ===
    // Fused arithmetic-to-dest (Int): Src1 is int operand
    bcAddIntTo, bcSubIntTo, bcMulIntTo,
    // Fused constant arithmetic (Int): Src1 is source register
    bcAddIntConst, bcSubIntConst, bcMulIntConst,
    // Fused compare-and-branch (Int): Src1 is first comparison operand
    bcBranchEqInt, bcBranchNeInt, bcBranchLtInt, bcBranchGtInt, bcBranchLeInt, bcBranchGeInt,
    // Fused compare-zero-and-branch (Int): Src1 is the register being compared
    bcBranchEqZeroInt, bcBranchNeZeroInt,
    // Fused loop increment-and-branch (Int): Src1 = step register
    bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt,
    // Fused self-increment/decrement (Int): Src1 = step register
    bcAddIntSelf, bcSubIntSelf,
    // Fused array element operations: Src1 = src_idx_reg for MoveElement
    bcArrayMoveElement:
      Result := True;
  else
    Result := False;
  end;
end;

function TRegisterCompactor.Src1IsFloatReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 0: Core VM operations ===
    // Float arithmetic
    bcCopyFloat, bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat, bcNegFloat, bcPowFloat,
    // Float comparisons
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpLeFloat, bcCmpGtFloat, bcCmpGeFloat,
    // Conversion from float
    bcFloatToInt, bcFloatToString,
    // === GROUP 2: Math functions ===
    bcMathSqr, bcMathSin, bcMathCos, bcMathTan, bcMathAtn,
    bcMathExp, bcMathLog, bcMathAbs, bcMathSgn, bcMathInt, bcMathRnd,
    bcMathLog10, bcMathLog2, bcMathLogN,
    // === GROUP 1: String operations with float param ===
    bcStrStr,      // STR$(n) - reads float, produces string
    // === GROUP 4: I/O operations ===
    // Print float value (bcPrint/bcPrintLn use float register in Src1)
    bcPrint, bcPrintLn,
    // === SUPERINSTRUCTIONS ===
    // Fused arithmetic-to-dest (Float): Src1 is float operand
    bcAddFloatTo, bcSubFloatTo, bcMulFloatTo, bcDivFloatTo,
    // Fused constant arithmetic (Float): Src1 is source register
    bcAddFloatConst, bcSubFloatConst, bcMulFloatConst, bcDivFloatConst,
    // Fused compare-and-branch (Float): Src1 is first comparison operand
    bcBranchEqFloat, bcBranchNeFloat, bcBranchLtFloat, bcBranchGtFloat, bcBranchLeFloat, bcBranchGeFloat,
    // Fused compare-zero-and-branch (Float): Src1 is the register being compared
    bcBranchEqZeroFloat, bcBranchNeZeroFloat,
    // Fused Multiply-Add (FMA): Src1 is 'a' in (a * b)
    bcMulAddFloat, bcMulSubFloat, bcMulAddToFloat, bcMulSubToFloat,
    // Fused Square-Sum: Src1 is 'x' or 'sum'
    bcSquareSumFloat, bcAddSquareFloat, bcMulMulFloat, bcAddSqrtFloat,
    // === GROUP 11: Sound ===
    bcSoundFilter:  // Src1 = cutoff frequency (float)
      Result := True;
  else
    Result := False;
  end;
end;

function TRegisterCompactor.Src2IsIntReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 0: Core VM operations ===
    // Int arithmetic (second operand)
    bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt,
    // Int comparisons (second operand)
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpLeInt, bcCmpGtInt, bcCmpGeInt,
    // Bitwise operations (second operand)
    bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor,
    // === GROUP 5: Memory operations ===
    bcPoke,           // POKE address, value: Src2 = value (int)
    // === GROUP 1: String operations with int second param ===
    bcStrLeft, bcStrRight,  // LEFT$/RIGHT$(str, len) - len is Src2 (int)
    bcStrMid,  // Mid$(str, start, length) - start is Src1, length is Src2
    // === GROUP 3: Typed array operations: Src2 is always int (linear index) ===
    bcArrayLoadInt, bcArrayLoadFloat, bcArrayLoadString,
    bcArrayStoreInt, bcArrayStoreFloat, bcArrayStoreString,
    // === GROUP 10: Graphics ===
    bcGraphicBox, bcGraphicSetMode, bcGraphicRGBA,
    bcGraphicWindow,  // Src2 = row1 register (int)
    bcGraphicCircle,  // Src2 = x register (int)
    bcGraphicScale,   // Src2 = xmax register (int)
    bcGraphicColor,   // Src2 = color value (int)
    bcGraphicPaint,   // Src2 = x coordinate (int)
    bcGraphicSShape,  // Src2 = y1 coordinate (int)
    bcGraphicGShape,  // Src2 = x coordinate (int)
    bcSetColor,       // Src2 = color value (int)
    // === SUPERINSTRUCTIONS ===
    // Fused compare-and-branch (Int): Src2 is second comparison operand
    bcBranchEqInt, bcBranchNeInt, bcBranchLtInt, bcBranchGtInt, bcBranchLeInt, bcBranchGeInt,
    // Fused array-store-constant: Src2 is the int index register
    bcArrayStoreIntConst, bcArrayStoreFloatConst, bcArrayStoreStringConst,
    // Fused loop increment-and-branch (Int): Src2 = limit register
    bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt,
    // Fused ArrayLoad + Arithmetic: Src2 is the int index register
    bcArrayLoadAddFloat, bcArrayLoadSubFloat, bcArrayLoadDivAddFloat,
    // Fused ArrayLoad + Branch: Src2 is the int index register
    bcArrayLoadIntBranchNZ, bcArrayLoadIntBranchZ,
    // Fused array element operations: Src2 = idx_reg for swap/copy/move, or start_idx for shift
    bcArraySwapInt, bcArrayCopyElement, bcArrayMoveElement, bcArrayLoadIntTo,
    bcArrayShiftLeft, bcArrayReverseRange,
    // === GROUP 11: Sound ===
    bcSoundFilter:  // Src2 = lowpass (int 0/1)
      Result := True;
  else
    Result := False;
  end;
end;

function TRegisterCompactor.Src2IsFloatReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 0: Core VM operations ===
    // Float arithmetic (second operand)
    bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat, bcPowFloat,
    // Float comparisons (second operand)
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpLeFloat, bcCmpGtFloat, bcCmpGeFloat,
    // === SUPERINSTRUCTIONS ===
    // Fused compare-and-branch (Float): Src2 is second comparison operand
    bcBranchEqFloat, bcBranchNeFloat, bcBranchLtFloat, bcBranchGtFloat, bcBranchLeFloat, bcBranchGeFloat,
    // Fused Multiply-Add (FMA): Src2 is 'b' in (a * b)
    bcMulAddFloat, bcMulSubFloat, bcMulAddToFloat, bcMulSubToFloat,
    // Fused Square-Sum: Src2 is 'y' or 'x' (square operand)
    bcSquareSumFloat, bcAddSquareFloat, bcMulMulFloat, bcAddSqrtFloat,
    // LOGN(base, x): Src2 is 'x' (the value)
    bcMathLogN,
    // === GROUP 6: Sound operations ===
    bcSoundSound,  // Src2 = frequency (float)
    // === GROUP 4: I/O operations ===
    bcPrintUsing:  // PRINT USING - Src2 = value (float)
      Result := True;
  else
    Result := False;
  end;
end;

function TRegisterCompactor.Src1IsStringReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 0: Core VM operations ===
    bcCopyString,
    // String comparison (first operand)
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString,
    // === GROUP 1: String operations ===
    bcStrConcat, bcStrLeft, bcStrRight, bcStrMid, bcStrLen, bcStrAsc,
    bcStrVal,    // VAL(str) - reads string, produces float
    bcStrInstr,  // INSTR(haystack, needle) - haystack is Src1
    // === GROUP 2: Math operations ===
    bcStrDec,  // DEC(hexstring) - reads string, produces int
    // === GROUP 4: I/O operations ===
    bcPrintString, bcPrintStringLn,
    bcPrintUsing,  // PRINT USING - Src1 = format string
    // === GROUP 5: Special variables ===
    bcStoreTIS,  // TI$ = value - reads string from Src1
    // === GROUP 6: Sound operations ===
    bcSoundPlay,  // Src1 = music string
    // === GROUP 10: Graphics ===
    bcGraphicGShape:  // GSHAPE A$, x, y: A$ is string in Src1
      Result := True;
  else
    Result := False;
  end;
end;

function TRegisterCompactor.Src2IsStringReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 0: Core VM operations ===
    // String comparison (second operand)
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString,
    // === GROUP 1: String operations ===
    bcStrConcat,  // String concatenation (second operand)
    bcStrInstr:   // INSTR(haystack, needle) - needle is Src2
      Result := True;
  else
    Result := False;
  end;
end;

function TRegisterCompactor.DestReadIsIntReg(OpCode: TBytecodeOp): Boolean;
begin
  { These opcodes use Dest as a SOURCE register (read, not write).
    This is critical for ArrayStore where Dest holds the VALUE to store. }
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 3: Array operations ===
    bcArrayStoreInt,  // Dest = value register (int) - READ, not written
    // === GROUP 10: Graphics ===
    bcGraphicBox,     // Dest = y1 register (int) - READ, not written
    bcGraphicWindow,  // Dest = col2 register (int) - READ, not written
    bcGraphicCircle,  // Dest = y register (int) - READ, not written
    bcGraphicScale,   // Dest = ymax register (int) - READ, not written
    bcGraphicPaint,   // Dest = y coordinate (int) - READ, not written
    bcGraphicGShape,  // Dest = y coordinate (int) - READ, not written
    // === GROUP 11: Sound ===
    bcSoundSound,     // Dest = duration register (int) - READ, not written
    // === SUPERINSTRUCTIONS ===
    // Array swap: Dest = idx2_reg (int) - READ
    bcArraySwapInt,
    // Array shift/reverse: Dest = end_idx_reg (int) - READ
    bcArrayShiftLeft, bcArrayReverseRange:
      Result := True;
  else
    Result := False;
  end;
end;

function TRegisterCompactor.DestReadIsFloatReg(OpCode: TBytecodeOp): Boolean;
begin
  { These opcodes use Dest as a SOURCE register (read, not write).
    This is critical for ArrayStore where Dest holds the VALUE to store. }
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 3: Array operations ===
    bcArrayStoreFloat:  // Dest = value register (float) - READ, not written
      Result := True;
  else
    Result := False;
  end;
end;

function TRegisterCompactor.DestReadIsStringReg(OpCode: TBytecodeOp): Boolean;
begin
  { These opcodes use Dest as a SOURCE register (read, not write).
    This is critical for ArrayStore where Dest holds the VALUE to store. }
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 3: Array operations ===
    bcArrayStoreString:  // Dest = value register (string) - READ, not written
      Result := True;
  else
    Result := False;
  end;
end;

function TRegisterCompactor.ImmediateIsFloatReg(OpCode: TBytecodeOp): Boolean;
begin
  { These superinstructions store a FLOAT REGISTER INDEX in the Immediate field
    instead of a constant value. The Immediate field needs to be remapped
    during register compaction. }
  case OpCode of
    // Fused ArrayLoad + Arithmetic: Immediate is the accumulator float register
    bcArrayLoadAddFloat, bcArrayLoadSubFloat,
    // Fused Multiply-Add (FMA): Immediate is 'c' in (c op a*b) - the accumulator
    bcMulAddFloat, bcMulSubFloat,
    // Fused Mul-Mul: Immediate is 'c' in (a*b*c)
    bcMulMulFloat:
      Result := True;
  else
    Result := False;
  end;
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
    // IMPORTANT: For superinstructions (opcode >= 110), the cast to TBytecodeOp
    // produces an out-of-range value. The Is*Reg functions handle this by
    // checking Ord(OpCode) >= 110 first and using integer comparisons.
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

    // bcGraphicBox: Immediate contains 5 packed register indices
    // Layout: x2(bits 0-11) | y2(12-23) | angle(24-35) | filled(36-47) | fill_color(48-59)
    // x2, y2, filled, fill_color are int registers; angle is float register
    if OpCode = bcGraphicBox then
    begin
      MarkIntRegUsed((Instr.Immediate) and $FFF);           // x2 - int
      MarkIntRegUsed((Instr.Immediate shr 12) and $FFF);    // y2 - int
      MarkFloatRegUsed((Instr.Immediate shr 24) and $FFF);  // angle - float
      MarkIntRegUsed((Instr.Immediate shr 36) and $FFF);    // filled - int
      MarkIntRegUsed((Instr.Immediate shr 48) and $FFF);    // fill_color - int
    end;

    // bcGraphicSetMode: Immediate = param3 register (int)
    if OpCode = bcGraphicSetMode then
      MarkIntRegUsed(Instr.Immediate);

    // bcGraphicRGBA: Immediate = (B_reg << 16) | A_reg - two int registers
    if OpCode = bcGraphicRGBA then
    begin
      MarkIntRegUsed(Instr.Immediate and $FFFF);           // A register
      MarkIntRegUsed((Instr.Immediate shr 16) and $FFFF);  // B register
    end;

    // bcArrayLoadDivAddFloat: Immediate = (denom_reg << 16) | acc_reg - two float registers
    if OpCode = bcArrayLoadDivAddFloat then
    begin
      MarkFloatRegUsed(Instr.Immediate and $FFFF);           // acc register
      MarkFloatRegUsed((Instr.Immediate shr 16) and $FFFF);  // denom register
    end;

    // bcStrMid: Immediate contains length register index (int)
    // MID$(str, start, length) - start is Src2, length is in Immediate
    if OpCode = bcStrMid then
      MarkIntRegUsed(Instr.Immediate and $FFFF);

    // bcGraphicWindow: Src1=col1, Src2=row1, Dest=col2, Immediate = (clear_reg << 16) | row2_reg
    // All 5 parameters are int registers
    if OpCode = bcGraphicWindow then
    begin
      MarkIntRegUsed(Instr.Src1);              // col1
      MarkIntRegUsed(Instr.Src2);              // row1
      MarkIntRegUsed(Instr.Dest);              // col2
      MarkIntRegUsed(Instr.Immediate and $FFFF);           // row2 (bits 0-15)
      MarkIntRegUsed((Instr.Immediate shr 16) and $FFFF);  // clear (bits 16-31)
    end;

    // bcGraphicCircle: Src1=color(int), Src2=x(int), Dest=y(int)
    // Immediate: xr(10) | yr(10) | sa(10) | ea(10) | angle(10) | inc(10) = 60 bits
    // xr, yr are int registers; sa, ea, angle, inc are float registers
    if OpCode = bcGraphicCircle then
    begin
      MarkIntRegUsed(Instr.Src1);                          // color - int
      MarkIntRegUsed(Instr.Src2);                          // x - int
      MarkIntRegUsed(Instr.Dest);                          // y - int
      MarkIntRegUsed((Instr.Immediate) and $3FF);          // xr - int
      MarkIntRegUsed((Instr.Immediate shr 10) and $3FF);   // yr - int
      MarkFloatRegUsed((Instr.Immediate shr 20) and $3FF); // sa - float
      MarkFloatRegUsed((Instr.Immediate shr 30) and $3FF); // ea - float
      MarkFloatRegUsed((Instr.Immediate shr 40) and $3FF); // angle - float
      MarkFloatRegUsed((Instr.Immediate shr 50) and $3FF); // inc - float
    end;

    // bcGraphicPaint: Src1=source(int), Src2=x(int), Dest=y(int), Immediate = mode(int)
    if OpCode = bcGraphicPaint then
    begin
      MarkIntRegUsed(Instr.Src1);               // source - int
      MarkIntRegUsed(Instr.Src2);               // x - int
      MarkIntRegUsed(Instr.Dest);               // y - int
      MarkIntRegUsed(Instr.Immediate and $FFFF);// mode - int
    end;

    // bcGraphicSShape: Dest=string reg, Src1=x1(int), Src2=y1(int)
    // Immediate bits 0-15 = x2 register(int), bits 16-31 = y2 register(int)
    if OpCode = bcGraphicSShape then
    begin
      MarkStringRegUsed(Instr.Dest);                       // result string
      MarkIntRegUsed(Instr.Src1);                          // x1 - int
      MarkIntRegUsed(Instr.Src2);                          // y1 - int
      MarkIntRegUsed(Instr.Immediate and $FFFF);           // x2 - int
      MarkIntRegUsed((Instr.Immediate shr 16) and $FFFF);  // y2 - int
    end;

    // bcGraphicGShape: Src1=string reg, Src2=x(int), Dest=y(int), Immediate=mode (value, not reg)
    if OpCode = bcGraphicGShape then
    begin
      MarkStringRegUsed(Instr.Src1);            // shape string
      MarkIntRegUsed(Instr.Src2);               // x - int
      MarkIntRegUsed(Instr.Dest);               // y - int
    end;

    // bcSoundFilter: Immediate contains hp_reg(8) | res_reg(8)
    if OpCode = bcSoundFilter then
    begin
      MarkIntRegUsed(Instr.Immediate and $FF);           // hp register
      MarkIntRegUsed((Instr.Immediate shr 8) and $FF);   // resonance register
    end;

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
  VarRegCount: Integer;
begin
  // CRITICAL FIX: Get the count of BASIC variable registers from program metadata
  // Variable registers (0..VarRegCount-1) must be preserved with identity mapping
  // because they may be read without explicit write instructions in the bytecode
  VarRegCount := FProgram.GetIntVarRegCount;

  // Build compact mapping for int registers
  // First, preserve variable registers (0..VarRegCount-1) with identity mapping
  FNewIntRegCount := 0;
  for i := 0 to VarRegCount - 1 do
  begin
    FIntRegMap[i] := i;  // Identity mapping for variable registers
    if i >= FNewIntRegCount then
      FNewIntRegCount := i + 1;
  end;

  // Then map remaining used registers
  for i := VarRegCount to FMaxOldIntReg do
  begin
    if FIntRegMap[i] >= 0 then  // Was marked as used
    begin
      FIntRegMap[i] := FNewIntRegCount;
      Inc(FNewIntRegCount);
    end;
  end;

  // Build compact mapping for float registers
  // First, preserve variable registers with identity mapping
  VarRegCount := FProgram.GetFloatVarRegCount;
  FNewFloatRegCount := 0;
  for i := 0 to VarRegCount - 1 do
  begin
    FFloatRegMap[i] := i;  // Identity mapping for variable registers
    if i >= FNewFloatRegCount then
      FNewFloatRegCount := i + 1;
  end;
  // Then map remaining used registers
  for i := VarRegCount to FMaxOldFloatReg do
  begin
    if FFloatRegMap[i] >= 0 then
    begin
      FFloatRegMap[i] := FNewFloatRegCount;
      Inc(FNewFloatRegCount);
    end;
  end;

  // Build compact mapping for string registers
  // First, preserve variable registers with identity mapping
  VarRegCount := FProgram.GetStringVarRegCount;
  FNewStringRegCount := 0;
  for i := 0 to VarRegCount - 1 do
  begin
    FStringRegMap[i] := i;  // Identity mapping for variable registers
    if i >= FNewStringRegCount then
      FNewStringRegCount := i + 1;
  end;
  // Then map remaining used registers
  for i := VarRegCount to FMaxOldStringReg do
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
  NewImm: Int64;
  OldReg, NewReg: Integer;
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

    // bcGraphicBox: Immediate contains 5 packed register indices
    // Layout: x2(bits 0-11) | y2(12-23) | angle(24-35) | filled(36-47) | fill_color(48-59)
    // x2, y2, filled, fill_color are int registers; angle is float register
    if OpCode = bcGraphicBox then
    begin
      NewImm := 0;

      // x2 (bits 0-11) - int register
      OldReg := (Instr.Immediate) and $FFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or (Int64(NewReg) and $FFF);

      // y2 (bits 12-23) - int register
      OldReg := (Instr.Immediate shr 12) and $FFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFF) shl 12);

      // angle (bits 24-35) - float register
      OldReg := (Instr.Immediate shr 24) and $FFF;
      if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
        NewReg := FFloatRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFF) shl 24);

      // filled (bits 36-47) - int register
      OldReg := (Instr.Immediate shr 36) and $FFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFF) shl 36);

      // fill_color (bits 48-59) - int register
      OldReg := (Instr.Immediate shr 48) and $FFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFF) shl 48);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcGraphicSetMode: Immediate = param3 register (int)
    if OpCode = bcGraphicSetMode then
    begin
      OldReg := Instr.Immediate;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
      begin
        NewReg := FIntRegMap[OldReg];
        if NewReg <> OldReg then
        begin
          Instr.Immediate := NewReg;
          Modified := True;
        end;
      end;
    end;

    // bcGraphicRGBA: Immediate = (B_reg << 16) | A_reg - two int registers
    if OpCode = bcGraphicRGBA then
    begin
      // A register (bits 0-15)
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewReg and $FFFF;

      // B register (bits 16-31)
      OldReg := (Instr.Immediate shr 16) and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcArrayLoadDivAddFloat: Immediate = (denom_reg << 16) | acc_reg - two float registers
    if OpCode = bcArrayLoadDivAddFloat then
    begin
      // acc register (bits 0-15)
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
        NewReg := FFloatRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewReg and $FFFF;

      // denom register (bits 16-31)
      OldReg := (Instr.Immediate shr 16) and $FFFF;
      if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
        NewReg := FFloatRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcStrMid: Immediate contains length register index (int)
    // MID$(str, start, length) - start is Src2, length is in Immediate
    if OpCode = bcStrMid then
    begin
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
      begin
        NewReg := FIntRegMap[OldReg];
        if NewReg <> OldReg then
        begin
          Instr.Immediate := NewReg and $FFFF;
          Modified := True;
        end;
      end;
    end;

    // bcGraphicWindow: Src1=col1, Src2=row1, Dest=col2, Immediate = (clear_reg << 16) | row2_reg
    // All 5 parameters are int registers. Src1/Src2/Dest are handled above, now handle Immediate.
    if OpCode = bcGraphicWindow then
    begin
      // row2 register (bits 0-15)
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewReg and $FFFF;

      // clear register (bits 16-31)
      OldReg := (Instr.Immediate shr 16) and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcGraphicCircle: Immediate contains 6 packed register indices
    // Layout: xr(bits 0-9) | yr(10-19) | sa(20-29) | ea(30-39) | angle(40-49) | inc(50-59)
    // xr, yr are int registers; sa, ea, angle, inc are float registers
    if OpCode = bcGraphicCircle then
    begin
      NewImm := 0;

      // xr - int (bits 0-9)
      OldReg := Instr.Immediate and $3FF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewReg and $3FF;

      // yr - int (bits 10-19)
      OldReg := (Instr.Immediate shr 10) and $3FF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $3FF) shl 10);

      // sa - float (bits 20-29)
      OldReg := (Instr.Immediate shr 20) and $3FF;
      if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
        NewReg := FFloatRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $3FF) shl 20);

      // ea - float (bits 30-39)
      OldReg := (Instr.Immediate shr 30) and $3FF;
      if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
        NewReg := FFloatRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $3FF) shl 30);

      // angle - float (bits 40-49)
      OldReg := (Instr.Immediate shr 40) and $3FF;
      if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
        NewReg := FFloatRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $3FF) shl 40);

      // inc - float (bits 50-59)
      OldReg := (Instr.Immediate shr 50) and $3FF;
      if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
        NewReg := FFloatRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $3FF) shl 50);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcGraphicPaint: Immediate = mode register (int)
    if OpCode = bcGraphicPaint then
    begin
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
      begin
        NewReg := FIntRegMap[OldReg];
        if NewReg <> OldReg then
        begin
          Instr.Immediate := NewReg and $FFFF;
          Modified := True;
        end;
      end;
    end;

    // bcGraphicSShape: Immediate = (y2_reg << 16) | x2_reg - two int registers
    if OpCode = bcGraphicSShape then
    begin
      // x2 register (bits 0-15)
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewReg and $FFFF;

      // y2 register (bits 16-31)
      OldReg := (Instr.Immediate shr 16) and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcSoundFilter: Immediate contains hp_reg(8) | res_reg(8)
    if OpCode = bcSoundFilter then
    begin
      // hp register (bits 0-7)
      OldReg := Instr.Immediate and $FF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewReg and $FF;

      // resonance register (bits 8-15)
      OldReg := (Instr.Immediate shr 8) and $FF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FF) shl 8);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
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
