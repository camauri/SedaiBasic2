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
  Op: TBytecodeOp;
  Line: string;
begin
  // Handle superinstructions (opcodes 100+) separately to avoid enum range issues
  if Instr.OpCode >= 100 then
  begin
    case Instr.OpCode of
      // Fused compare-and-branch (Int) - opcodes 100-105
      100: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchEqInt', Instr.Src1, Instr.Src2, Instr.Immediate]);
      101: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchNeInt', Instr.Src1, Instr.Src2, Instr.Immediate]);
      102: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchLtInt', Instr.Src1, Instr.Src2, Instr.Immediate]);
      103: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchGtInt', Instr.Src1, Instr.Src2, Instr.Immediate]);
      104: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchLeInt', Instr.Src1, Instr.Src2, Instr.Immediate]);
      105: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchGeInt', Instr.Src1, Instr.Src2, Instr.Immediate]);
      // Fused compare-and-branch (Float) - opcodes 110-115
      110: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchEqFloat', Instr.Src1, Instr.Src2, Instr.Immediate]);
      111: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchNeFloat', Instr.Src1, Instr.Src2, Instr.Immediate]);
      112: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchLtFloat', Instr.Src1, Instr.Src2, Instr.Immediate]);
      113: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchGtFloat', Instr.Src1, Instr.Src2, Instr.Immediate]);
      114: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchLeFloat', Instr.Src1, Instr.Src2, Instr.Immediate]);
      115: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'BranchGeFloat', Instr.Src1, Instr.Src2, Instr.Immediate]);
      // Fused arithmetic-to-dest (Int) - opcodes 120-122
      120: Line := Format('%4d: %-20s R%d, R%d', [Index, 'AddIntTo', Instr.Dest, Instr.Src1]);
      121: Line := Format('%4d: %-20s R%d, R%d', [Index, 'SubIntTo', Instr.Dest, Instr.Src1]);
      122: Line := Format('%4d: %-20s R%d, R%d', [Index, 'MulIntTo', Instr.Dest, Instr.Src1]);
      // Fused arithmetic-to-dest (Float) - opcodes 130-133
      130: Line := Format('%4d: %-20s R%d, R%d', [Index, 'AddFloatTo', Instr.Dest, Instr.Src1]);
      131: Line := Format('%4d: %-20s R%d, R%d', [Index, 'SubFloatTo', Instr.Dest, Instr.Src1]);
      132: Line := Format('%4d: %-20s R%d, R%d', [Index, 'MulFloatTo', Instr.Dest, Instr.Src1]);
      133: Line := Format('%4d: %-20s R%d, R%d', [Index, 'DivFloatTo', Instr.Dest, Instr.Src1]);
      // Fused constant arithmetic (Int) - opcodes 140-142
      140: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'AddIntConst', Instr.Dest, Instr.Src1, Instr.Immediate]);
      141: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'SubIntConst', Instr.Dest, Instr.Src1, Instr.Immediate]);
      142: Line := Format('%4d: %-20s R%d, R%d, %d', [Index, 'MulIntConst', Instr.Dest, Instr.Src1, Instr.Immediate]);
      // Fused constant arithmetic (Float) - opcodes 150-153
      150: Line := Format('%4d: %-20s R%d, R%d, %f', [Index, 'AddFloatConst', Instr.Dest, Instr.Src1, Double(Pointer(@Instr.Immediate)^)]);
      151: Line := Format('%4d: %-20s R%d, R%d, %f', [Index, 'SubFloatConst', Instr.Dest, Instr.Src1, Double(Pointer(@Instr.Immediate)^)]);
      152: Line := Format('%4d: %-20s R%d, R%d, %f', [Index, 'MulFloatConst', Instr.Dest, Instr.Src1, Double(Pointer(@Instr.Immediate)^)]);
      153: Line := Format('%4d: %-20s R%d, R%d, %f', [Index, 'DivFloatConst', Instr.Dest, Instr.Src1, Double(Pointer(@Instr.Immediate)^)]);
      // Fused compare-zero-and-branch (Int) - opcodes 160-161
      160: Line := Format('%4d: %-20s R%d, %d', [Index, 'BranchEqZeroInt', Instr.Src1, Instr.Immediate]);
      161: Line := Format('%4d: %-20s R%d, %d', [Index, 'BranchNeZeroInt', Instr.Src1, Instr.Immediate]);
      // Fused compare-zero-and-branch (Float) - opcodes 170-171
      170: Line := Format('%4d: %-20s R%d, %d', [Index, 'BranchEqZeroFlt', Instr.Src1, Instr.Immediate]);
      171: Line := Format('%4d: %-20s R%d, %d', [Index, 'BranchNeZeroFlt', Instr.Src1, Instr.Immediate]);
      // Fused array-store-constant - opcodes 180-182
      180: Line := Format('%4d: %-20s ARR[%d], idx=IntR%d, %d', [Index, 'ArrayStoreIntConst', Instr.Src1, Instr.Src2, Instr.Immediate]);
      181: Line := Format('%4d: %-20s ARR[%d], idx=IntR%d, %f', [Index, 'ArrayStoreFltConst', Instr.Src1, Instr.Src2, Double(Pointer(@Instr.Immediate)^)]);
      182: Line := Format('%4d: %-20s ARR[%d], idx=IntR%d, [%d]', [Index, 'ArrayStoreStrConst', Instr.Src1, Instr.Src2, Instr.Immediate]);
      // Fused loop increment-and-branch - opcodes 190-193
      190: Line := Format('%4d: %-20s R%d, R%d, R%d, %d', [Index, 'AddIntToBranchLe', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
      191: Line := Format('%4d: %-20s R%d, R%d, R%d, %d', [Index, 'AddIntToBranchLt', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
      192: Line := Format('%4d: %-20s R%d, R%d, R%d, %d', [Index, 'SubIntToBranchGe', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
      193: Line := Format('%4d: %-20s R%d, R%d, R%d, %d', [Index, 'SubIntToBranchGt', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
      // FMA (Fused Multiply-Add) - opcodes 200-203
      200: Line := Format('%4d: %-20s R%d, R%d, R%d, R%d', [Index, 'MulAddFloat', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
      201: Line := Format('%4d: %-20s R%d, R%d, R%d, R%d', [Index, 'MulSubFloat', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
      202: Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'MulAddToFloat', Instr.Dest, Instr.Src1, Instr.Src2]);
      203: Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'MulSubToFloat', Instr.Dest, Instr.Src1, Instr.Src2]);
      // Array Load + Arithmetic - opcodes 210-212
      210: Line := Format('%4d: %-20s R%d, ARR[%d], idx=R%d, acc=R%d', [Index, 'ArrayLoadAddFloat', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
      211: Line := Format('%4d: %-20s R%d, ARR[%d], idx=R%d, acc=R%d', [Index, 'ArrayLoadSubFloat', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
      212: Line := Format('%4d: %-20s R%d, ARR[%d], idx=R%d, acc=R%d, denom=R%d', [Index, 'ArrayLoadDivAddFlt', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate and $FFFF, (Instr.Immediate shr 16) and $FFFF]);
      // Square-Sum patterns - opcodes 220-221
      220: Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'SquareSumFloat', Instr.Dest, Instr.Src1, Instr.Src2]);
      221: Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'AddSquareFloat', Instr.Dest, Instr.Src1, Instr.Src2]);
      // Mul-Mul and Add-Sqrt - opcodes 230-231
      230: Line := Format('%4d: %-20s R%d, R%d, R%d, R%d', [Index, 'MulMulFloat', Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
      231: Line := Format('%4d: %-20s R%d, R%d, R%d', [Index, 'AddSqrtFloat', Instr.Dest, Instr.Src1, Instr.Src2]);
      // Array Load + Branch - opcodes 240-241
      240: Line := Format('%4d: %-20s ARR[%d], idx=R%d, %d', [Index, 'ArrayLoadIntBrNZ', Instr.Src1, Instr.Src2, Instr.Immediate]);
      241: Line := Format('%4d: %-20s ARR[%d], idx=R%d, %d', [Index, 'ArrayLoadIntBrZ', Instr.Src1, Instr.Src2, Instr.Immediate]);
      // Array Swap (Int) - opcode 250
      250: Line := Format('%4d: %-20s ARR[%d], idx1=R%d, idx2=R%d', [Index, 'ArraySwapInt', Instr.Src1, Instr.Src2, Instr.Dest]);
      // Self-increment/decrement (Int) - opcodes 251-252
      251: Line := Format('%4d: %-20s R%d, R%d', [Index, 'AddIntSelf', Instr.Dest, Instr.Src1]);
      252: Line := Format('%4d: %-20s R%d, R%d', [Index, 'SubIntSelf', Instr.Dest, Instr.Src1]);
      // Array Load to register (Int) - opcode 253
      253: Line := Format('%4d: %-20s R%d, ARR[%d], idx=R%d', [Index, 'ArrayLoadIntTo', Instr.Dest, Instr.Src1, Instr.Src2]);
    else
      Line := Format('%4d: (unknown superinstr opcode %d)', [Index, Instr.OpCode]);
    end;
    FOutput.Add(Line);
    Exit;
  end;

  Op := TBytecodeOp(Instr.OpCode);
  Line := Format('%4d: %-20s', [Index, BytecodeOpToString(Op)]);

  case Op of
    bcLoadConstInt:
      Line := Line + Format('R%d, %d', [Instr.Dest, Instr.Immediate]);

    bcLoadConstFloat:
      Line := Line + Format('R%d, %f', [Instr.Dest, Double(Pointer(@Instr.Immediate)^)]);

    bcLoadConstString:
      Line := Line + Format('R%d, [%d]', [Instr.Dest, Instr.Immediate]);

    bcCopyInt, bcCopyFloat, bcCopyString:
      Line := Line + Format('R%d, R%d', [Instr.Dest, Instr.Src1]);

    bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt,
    bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat, bcPowFloat:
      Line := Line + Format('R%d, R%d, R%d', [Instr.Dest, Instr.Src1, Instr.Src2]);

    bcNegInt, bcNegFloat:
      Line := Line + Format('R%d, R%d', [Instr.Dest, Instr.Src1]);

    bcIntToFloat, bcFloatToInt:
      Line := Line + Format('R%d, R%d', [Instr.Dest, Instr.Src1]);

    // Comparison operators
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpGtInt, bcCmpLeInt, bcCmpGeInt,
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpGtFloat, bcCmpLeFloat, bcCmpGeFloat,
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString:
      Line := Line + Format('R%d, R%d, R%d', [Instr.Dest, Instr.Src1, Instr.Src2]);

    // Math functions
    bcMathAbs, bcMathSgn, bcMathInt, bcMathSqr,
    bcMathSin, bcMathCos, bcMathTan, bcMathExp, bcMathLog:
      Line := Line + Format('R%d, R%d', [Instr.Dest, Instr.Src1]);

    bcMathRnd:
      Line := Line + Format('R%d', [Instr.Dest]);

    bcJump, bcCall:
      Line := Line + Format('%d', [Instr.Immediate]);

    bcJumpIfZero, bcJumpIfNotZero:
      Line := Line + Format('R%d, %d', [Instr.Src1, Instr.Immediate]);

    bcPrint, bcPrintLn, bcPrintString, bcPrintStringLn:
      Line := Line + Format('R%d', [Instr.Src1]);

    bcInput, bcInputInt, bcInputFloat, bcInputString:
      Line := Line + Format('R%d', [Instr.Dest]);

    // Array operations
    bcArrayDim:
      Line := Line + Format('ARR[%d]', [Instr.Src1]);

    bcArrayLoad:
      Line := Line + Format('R%d, ARR[%d], idx=R%d', [Instr.Dest, Instr.Src1, Instr.Src2]);

    bcArrayStore:
      Line := Line + Format('ARR[%d], idx=R%d, value=R%d', [Instr.Src1, Instr.Src2, Instr.Dest]);

    // Typed array operations
    bcArrayLoadInt:
      Line := Line + Format('IntR%d, ARR[%d], idx=IntR%d', [Instr.Dest, Instr.Src1, Instr.Src2]);
    bcArrayLoadFloat:
      Line := Line + Format('FloatR%d, ARR[%d], idx=IntR%d', [Instr.Dest, Instr.Src1, Instr.Src2]);
    bcArrayLoadString:
      Line := Line + Format('StrR%d, ARR[%d], idx=IntR%d', [Instr.Dest, Instr.Src1, Instr.Src2]);
    bcArrayStoreInt:
      Line := Line + Format('ARR[%d], idx=IntR%d, value=IntR%d', [Instr.Src1, Instr.Src2, Instr.Dest]);
    bcArrayStoreFloat:
      Line := Line + Format('ARR[%d], idx=IntR%d, value=FloatR%d', [Instr.Src1, Instr.Src2, Instr.Dest]);
    bcArrayStoreString:
      Line := Line + Format('ARR[%d], idx=IntR%d, value=StrR%d', [Instr.Src1, Instr.Src2, Instr.Dest]);

    bcEnd, bcStop, bcReturn, bcNop:
      ; // No operands
  end;

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
