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
unit SedaiBytecodeDisassembler;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils,
  SedaiBytecodeTypes;

type
  { Bytecode Disassembler - for debugging }
  TBytecodeDisassembler = class
  private
    FProgram: TBytecodeProgram;
    FOutput: TStringList;

    procedure DisassembleInstruction(Index: Integer; const Instr: TBytecodeInstruction);
  public
    constructor Create;
    destructor Destroy; override;

    function Disassemble(Program_: TBytecodeProgram): string;
    function DisassembleToList(Program_: TBytecodeProgram): TStringList;
  end;

implementation

constructor TBytecodeDisassembler.Create;
begin
  inherited Create;
  FProgram := nil;
  FOutput := TStringList.Create;
end;

destructor TBytecodeDisassembler.Destroy;
begin
  FOutput.Free;
  inherited Destroy;
end;

procedure TBytecodeDisassembler.DisassembleInstruction(Index: Integer; const Instr: TBytecodeInstruction);
var
  Group: Word;
  SubOp: Word;
  Line: string;
begin
  // Two-level dispatch based on opcode groups (high byte)
  Group := Instr.OpCode shr 8;
  SubOp := Instr.OpCode and $FF;

  case Instr.OpCode of
    // === GROUP 0: CORE VM OPERATIONS (0x00xx) ===
    bcLoadConstInt:
      Line := Format('%4d: %-20s R%d, %d', [Index, 'LoadConstInt', Instr.Dest, Instr.Immediate]);
    bcLoadConstFloat:
      Line := Format('%4d: %-20s R%d, %.2f', [Index, 'LoadConstFloat', Instr.Dest, Double(Pointer(@Instr.Immediate)^)]);
    bcLoadConstString:
      Line := Format('%4d: %-20s R%d, [%d]', [Index, 'LoadConstString', Instr.Dest, Instr.Immediate]);
    bcCopyInt:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'CopyInt', Instr.Dest, Instr.Src1]);
    bcCopyFloat:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'CopyFloat', Instr.Dest, Instr.Src1]);
    bcCopyString:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'CopyString', Instr.Dest, Instr.Src1]);
    bcLoadVar:
      Line := Format('%4d: %-20s R%d, var[%d]', [Index, 'LoadVar', Instr.Dest, Instr.Immediate]);
    bcStoreVar:
      Line := Format('%4d: %-20s var[%d], R%d', [Index, 'StoreVar', Instr.Immediate, Instr.Src1]);
    bcAddInt:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'AddInt', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcSubInt:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'SubInt', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcMulInt:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'MulInt', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcDivInt:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'DivInt', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcModInt:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'ModInt', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcNegInt:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'NegInt', Instr.Dest, Instr.Src1]);
    bcAddFloat:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'AddFloat', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcSubFloat:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'SubFloat', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcMulFloat:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'MulFloat', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcDivFloat:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'DivFloat', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcPowFloat:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'PowFloat', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcNegFloat:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'NegFloat', Instr.Dest, Instr.Src1]);
    bcIntToFloat:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'IntToFloat', Instr.Dest, Instr.Src1]);
    bcFloatToInt:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'FloatToInt', Instr.Dest, Instr.Src1]);
    bcIntToString:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'IntToString', Instr.Dest, Instr.Src1]);
    bcFloatToString:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'FloatToString', Instr.Dest, Instr.Src1]);
    bcStringToInt:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'StringToInt', Instr.Dest, Instr.Src1]);
    bcStringToFloat:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'StringToFloat', Instr.Dest, Instr.Src1]);
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpGtInt, bcCmpLeInt, bcCmpGeInt:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'CmpInt', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpGtFloat, bcCmpLeFloat, bcCmpGeFloat:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'CmpFloat', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'CmpString', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcBitwiseAnd:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'BitwiseAnd', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcBitwiseOr:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'BitwiseOr', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcBitwiseXor:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'BitwiseXor', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcBitwiseNot:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'BitwiseNot', Instr.Dest, Instr.Src1]);
    bcJump:
      Line := Format('%4d: %-20s %d', [Index, 'Jump', Instr.Immediate]);
    bcJumpIfZero:
      Line := Format('%4d: %-20s R%d, %d', [Index, 'JumpIfZero', Instr.Src1, Instr.Immediate]);
    bcJumpIfNotZero:
      Line := Format('%4d: %-20s R%d, %d', [Index, 'JumpIfNotZero', Instr.Src1, Instr.Immediate]);
    bcCall:
      Line := Format('%4d: %-20s %d', [Index, 'Call', Instr.Immediate]);
    bcReturn:
      Line := Format('%4d: %-20s', [Index, 'Return']);
    bcEnd:
      Line := Format('%4d: %-20s', [Index, 'End']);
    bcStop:
      Line := Format('%4d: %-20s', [Index, 'Stop']);
    bcFast:
      Line := Format('%4d: %-20s', [Index, 'Fast']);
    bcSlow:
      Line := Format('%4d: %-20s', [Index, 'Slow']);
    bcSleep:
      Line := Format('%4d: %-20s %d', [Index, 'Sleep', Instr.Immediate]);
    bcNop:
      Line := Format('%4d: %-20s', [Index, 'Nop']);

    // === GROUP 1: STRING OPERATIONS (0x01xx) ===
    bcStrConcat:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'StrConcat', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcStrLen:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'StrLen', Instr.Dest, Instr.Src1]);
    bcStrLeft:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'StrLeft', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcStrRight:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'StrRight', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcStrMid:
      Line := Format('%4d: %-20s R%d, R%d, startR%d, lenR%d', [Index, 'StrMid', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate and $FFFF]);

    // === GROUP 2: MATH FUNCTIONS (0x02xx) ===
    bcMathSin:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'MathSin', Instr.Dest, Instr.Src1]);
    bcMathCos:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'MathCos', Instr.Dest, Instr.Src1]);
    bcMathTan:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'MathTan', Instr.Dest, Instr.Src1]);
    bcMathAtn:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'MathAtn', Instr.Dest, Instr.Src1]);
    bcMathLog:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'MathLog', Instr.Dest, Instr.Src1]);
    bcMathExp:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'MathExp', Instr.Dest, Instr.Src1]);
    bcMathSqr:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'MathSqr', Instr.Dest, Instr.Src1]);
    bcMathAbs:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'MathAbs', Instr.Dest, Instr.Src1]);
    bcMathSgn:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'MathSgn', Instr.Dest, Instr.Src1]);
    bcMathInt:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'MathInt', Instr.Dest, Instr.Src1]);
    bcMathRnd:
      Line := Format('%4d: %-20s R%d', [Index, 'MathRnd', Instr.Dest]);

    // === GROUP 3: ARRAY OPERATIONS (0x03xx) ===
    bcArrayLoad:
      Line := Format('%4d: %-20s R%d, ARR[%d], R%d', [Index, 'ArrayLoad', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcArrayStore:
      Line := Format('%4d: %-20s ARR[%d], R%d, R%d', [Index, 'ArrayStore', Instr.Src1, Instr.Src2, Instr.Dest]);
    bcArrayDim:
      Line := Format('%4d: %-20s ARR[%d]', [Index, 'ArrayDim', Instr.Src1]);
    bcArrayLoadInt:
      Line := Format('%4d: %-20s R%d, ARR[%d], R%d', [Index, 'ArrayLoadInt', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcArrayLoadFloat:
      Line := Format('%4d: %-20s R%d, ARR[%d], R%d', [Index, 'ArrayLoadFloat', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcArrayLoadString:
      Line := Format('%4d: %-20s R%d, ARR[%d], R%d', [Index, 'ArrayLoadString', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcArrayStoreInt:
      Line := Format('%4d: %-20s ARR[%d], R%d, R%d', [Index, 'ArrayStoreInt', Instr.Src1, Instr.Src2, Instr.Dest]);
    bcArrayStoreFloat:
      Line := Format('%4d: %-20s ARR[%d], R%d, R%d', [Index, 'ArrayStoreFloat', Instr.Src1, Instr.Src2, Instr.Dest]);
    bcArrayStoreString:
      Line := Format('%4d: %-20s ARR[%d], R%d, R%d', [Index, 'ArrayStoreString', Instr.Src1, Instr.Src2, Instr.Dest]);

    // === GROUP 4: I/O OPERATIONS (0x04xx) ===
    bcPrint:
      Line := Format('%4d: %-20s R%d', [Index, 'Print', Instr.Src1]);
    bcPrintLn:
      Line := Format('%4d: %-20s R%d', [Index, 'PrintLn', Instr.Src1]);
    bcPrintString:
      Line := Format('%4d: %-20s R%d', [Index, 'PrintString', Instr.Src1]);
    bcPrintStringLn:
      Line := Format('%4d: %-20s R%d', [Index, 'PrintStringLn', Instr.Src1]);
    bcPrintInt:
      Line := Format('%4d: %-20s R%d', [Index, 'PrintInt', Instr.Src1]);
    bcPrintIntLn:
      Line := Format('%4d: %-20s R%d', [Index, 'PrintIntLn', Instr.Src1]);
    bcPrintComma:
      Line := Format('%4d: %-20s', [Index, 'PrintComma']);
    bcPrintSemicolon:
      Line := Format('%4d: %-20s', [Index, 'PrintSemicolon']);
    bcPrintTab:
      Line := Format('%4d: %-20s %d', [Index, 'PrintTab', Instr.Immediate]);
    bcPrintSpc:
      Line := Format('%4d: %-20s %d', [Index, 'PrintSpc', Instr.Immediate]);
    bcPrintNewLine:
      Line := Format('%4d: %-20s', [Index, 'PrintNewLine']);
    bcInput:
      Line := Format('%4d: %-20s R%d', [Index, 'Input', Instr.Dest]);
    bcInputInt:
      Line := Format('%4d: %-20s R%d', [Index, 'InputInt', Instr.Dest]);
    bcInputFloat:
      Line := Format('%4d: %-20s R%d', [Index, 'InputFloat', Instr.Dest]);
    bcInputString:
      Line := Format('%4d: %-20s R%d', [Index, 'InputString', Instr.Dest]);

    // === GROUP 5: SPECIAL VARIABLES (0x05xx) ===
    bcLoadTI:
      Line := Format('%4d: %-20s R%d', [Index, 'LoadTI', Instr.Dest]);
    bcLoadTIS:
      Line := Format('%4d: %-20s R%d', [Index, 'LoadTI$', Instr.Dest]);
    bcStoreTIS:
      Line := Format('%4d: %-20s R%d', [Index, 'StoreTI$', Instr.Src1]);
    bcLoadDTS:
      Line := Format('%4d: %-20s R%d', [Index, 'LoadDT$', Instr.Dest]);

    // === GROUP 10: GRAPHICS (0x0Axx) ===
    bcGraphicRGBA:
      Line := Format('%4d: %-20s R%d, R%d, R%d, R%d', [Index, 'GraphicRGBA', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcGraphicSetMode:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'GraphicSetMode', Instr.Src1, Instr.Src2]);
    bcGraphicBox:
      Line := Format('%4d: %-20s R%d, R%d, R%d, R%d', [Index, 'GraphicBox', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);

    // === GROUP 11: SOUND (0x0Bxx) ===
    bcSoundVol:
      Line := Format('%4d: %-20s R%d', [Index, 'SoundVol', Instr.Src1]);
    bcSoundSound:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'SoundSound', Instr.Src1, Instr.Src2]);
    bcSoundEnvelope:
      Line := Format('%4d: %-20s R%d', [Index, 'SoundEnvelope', Instr.Src1]);
    bcSoundTempo:
      Line := Format('%4d: %-20s R%d', [Index, 'SoundTempo', Instr.Src1]);
    bcSoundPlay:
      Line := Format('%4d: %-20s R%d', [Index, 'SoundPlay', Instr.Src1]);
    bcSoundFilter:
      Line := Format('%4d: %-20s', [Index, 'SoundFilter']);

    // === SUPERINSTRUCTIONS (0xC8xx+) ===
    bcBranchEqInt: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchEqInt', Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcBranchNeInt: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchNeInt', Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcBranchLtInt: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchLtInt', Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcBranchGtInt: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchGtInt', Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcBranchLeInt: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchLeInt', Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcBranchGeInt: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchGeInt', Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcBranchEqFloat: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchEqFloat', Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcBranchNeFloat: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchNeFloat', Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcBranchLtFloat: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchLtFloat', Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcBranchGtFloat: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchGtFloat', Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcBranchLeFloat: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchLeFloat', Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcBranchGeFloat: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchGeFloat', Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcAddIntTo: Line := Format('%4d: %-20s R%d, R%d', [Index, 'AddIntTo', Instr.Dest, Instr.Src1]);
    bcSubIntTo: Line := Format('%4d: %-20s R%d, R%d', [Index, 'SubIntTo', Instr.Dest, Instr.Src1]);
    bcMulIntTo: Line := Format('%4d: %-20s R%d, R%d', [Index, 'MulIntTo', Instr.Dest, Instr.Src1]);
    bcAddFloatTo: Line := Format('%4d: %-20s R%d, R%d', [Index, 'AddFloatTo', Instr.Dest, Instr.Src1]);
    bcSubFloatTo: Line := Format('%4d: %-20s R%d, R%d', [Index, 'SubFloatTo', Instr.Dest, Instr.Src1]);
    bcMulFloatTo: Line := Format('%4d: %-20s R%d, R%d', [Index, 'MulFloatTo', Instr.Dest, Instr.Src1]);
    bcDivFloatTo: Line := Format('%4d: %-20s R%d, R%d', [Index, 'DivFloatTo', Instr.Dest, Instr.Src1]);
    bcAddIntConst: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'AddIntConst', Instr.Dest, Instr.Src1, Instr.Immediate]);
    bcSubIntConst: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'SubIntConst', Instr.Dest, Instr.Src1, Instr.Immediate]);
    bcMulIntConst: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'MulIntConst', Instr.Dest, Instr.Src1, Instr.Immediate]);
    bcAddFloatConst: Line := Format('%4d: %-20s R%d, R%d, %.2f', [Index, 'AddFloatConst', Instr.Dest, Instr.Src1, Double(Pointer(@Instr.Immediate)^)]);
    bcSubFloatConst: Line := Format('%4d: %-20s R%d, R%d, %.2f', [Index, 'SubFloatConst', Instr.Dest, Instr.Src1, Double(Pointer(@Instr.Immediate)^)]);
    bcMulFloatConst: Line := Format('%4d: %-20s R%d, R%d, %.2f', [Index, 'MulFloatConst', Instr.Dest, Instr.Src1, Double(Pointer(@Instr.Immediate)^)]);
    bcDivFloatConst: Line := Format('%4d: %-20s R%d, R%d, %.2f', [Index, 'DivFloatConst', Instr.Dest, Instr.Src1, Double(Pointer(@Instr.Immediate)^)]);
    bcBranchEqZeroInt: Line := Format('%4d: %-20s R%d, %d', [Index, 'BranchEqZeroInt', Instr.Src1, Instr.Immediate]);
    bcBranchNeZeroInt: Line := Format('%4d: %-20s R%d, %d', [Index, 'BranchNeZeroInt', Instr.Src1, Instr.Immediate]);
    bcBranchEqZeroFloat: Line := Format('%4d: %-20s R%d, %d', [Index, 'BranchEqZeroFlt', Instr.Src1, Instr.Immediate]);
    bcBranchNeZeroFloat: Line := Format('%4d: %-20s R%d, %d', [Index, 'BranchNeZeroFlt', Instr.Src1, Instr.Immediate]);
    bcArrayStoreIntConst: Line := Format('%4d: %-20s ARR[%d], R%d, %d', [Index, 'ArrStoreIntConst', Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcArrayStoreFloatConst: Line := Format('%4d: %-20s ARR[%d], R%d, %.2f', [Index, 'ArrStoreFltConst', Instr.Src1, Instr.Src2, Double(Pointer(@Instr.Immediate)^)]);
    bcArrayStoreStringConst: Line := Format('%4d: %-20s ARR[%d], R%d, [%d]', [Index, 'ArrStoreStrConst', Instr.Src1, Instr.Src2, Instr.Immediate]);

    // === SUPERINSTRUCTIONS (Group $C8) ===
    // Fused loop increment-and-branch
    bcAddIntToBranchLe: Line := Format('%4d: %-20s R%d, R%d, R%d, %d', [Index, 'AddIntToBranchLe', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcAddIntToBranchLt: Line := Format('%4d: %-20s R%d, R%d, R%d, %d', [Index, 'AddIntToBranchLt', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcSubIntToBranchGe: Line := Format('%4d: %-20s R%d, R%d, R%d, %d', [Index, 'SubIntToBranchGe', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
    bcSubIntToBranchGt: Line := Format('%4d: %-20s R%d, R%d, R%d, %d', [Index, 'SubIntToBranchGt', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);

    // Array Load + Arithmetic superinstructions
    bcArrayLoadAddFloat: Line := Format('%4d: %-20s R%d = R%d + ARR[%d][R%d]', [Index, 'ArrLoadAddFlt', Instr.Dest, Instr.Immediate, Instr.Src1, Instr.Src2]);
    bcArrayLoadSubFloat: Line := Format('%4d: %-20s R%d = R%d - ARR[%d][R%d]', [Index, 'ArrLoadSubFlt', Instr.Dest, Instr.Immediate, Instr.Src1, Instr.Src2]);

  // === GROUP 1: STRING OPERATIONS (0x01xx) - additional ===
    bcStrAsc:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'StrAsc', Instr.Dest, Instr.Src1]);
    bcStrChr:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'StrChr', Instr.Dest, Instr.Src1]);
    bcStrStr:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'StrStr', Instr.Dest, Instr.Src1]);
    bcStrVal:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'StrVal', Instr.Dest, Instr.Src1]);
    bcStrHex:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'StrHex', Instr.Dest, Instr.Src1]);
    bcStrInstr:
      Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'StrInstr', Instr.Dest, Instr.Src1, Instr.Src2]);
    bcStrErr:
      Line := Format('%4d: %-20s R%d, R%d', [Index, 'StrErr', Instr.Dest, Instr.Src1]);

    // === GROUP 0: INPUT COMMANDS (0x00xx) - additional ===
    bcGet:
      Line := Format('%4d: %-20s R%d', [Index, 'Get', Instr.Dest]);
    bcGetkey:
      Line := Format('%4d: %-20s R%d', [Index, 'Getkey', Instr.Dest]);
    bcTron:
      Line := Format('%4d: %-20s', [Index, 'Tron']);
    bcTroff:
      Line := Format('%4d: %-20s', [Index, 'Troff']);
    bcDataReadInt:
      Line := Format('%4d: %-20s R%d', [Index, 'DataReadInt', Instr.Dest]);
    bcDataReadFloat:
      Line := Format('%4d: %-20s R%d', [Index, 'DataReadFloat', Instr.Dest]);
    bcDataReadString:
      Line := Format('%4d: %-20s R%d', [Index, 'DataReadString', Instr.Dest]);
    bcDataRestore:
      Line := Format('%4d: %-20s %d', [Index, 'DataRestore', Instr.Immediate]);
  else
    Line := Format('%4d: (unknown opcode $%04X, group=%d, sub=%d) Dest=%d Src1=%d Src2=%d Imm=%d',
      [Index, Instr.OpCode, Group, SubOp, Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
  end;

  // Append source line number if available (debug mode / TRON)
  if Instr.SourceLine > 0 then
    Line := Line + Format('  @L%d', [Instr.SourceLine]);

  FOutput.Add(Line);
end;

function TBytecodeDisassembler.Disassemble(Program_: TBytecodeProgram): string;
var
  List: TStringList;
begin
  List := DisassembleToList(Program_);
  try
    Result := List.Text;
  finally
    List.Free;
  end;
end;

function TBytecodeDisassembler.DisassembleToList(Program_: TBytecodeProgram): TStringList;
var
  i: Integer;
  Instr: TBytecodeInstruction;
begin
  FProgram := Program_;
  FOutput.Clear;

  FOutput.Add('=== BYTECODE DISASSEMBLY ===');
  FOutput.Add(Format('Instructions: %d', [FProgram.GetInstructionCount]));
  FOutput.Add(Format('Variables: %d', [FProgram.GetVariableCount]));
  FOutput.Add(Format('String Constants: %d', [FProgram.StringConstants.Count]));
  FOutput.Add('');

  if FProgram.StringConstants.Count > 0 then
  begin
    FOutput.Add('=== STRING CONSTANTS ===');
    for i := 0 to FProgram.StringConstants.Count - 1 do
      FOutput.Add(Format('[%d] "%s"', [i, FProgram.StringConstants[i]]));
    FOutput.Add('');
  end;

  FOutput.Add('=== INSTRUCTIONS ===');
  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    Instr := FProgram.GetInstruction(i);
    DisassembleInstruction(i, Instr);
  end;

  Result := TStringList.Create;
  Result.Assign(FOutput);
end;

end.
