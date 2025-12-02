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
unit SedaiBytecodeCompiler;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils,
  SedaiSSATypes, SedaiBytecodeTypes;

type
  TJumpFixup = record
    BCInstrIndex: Integer;
    TargetLabel: string;
  end;

  { Bytecode Compiler - compiles SSA IR to bytecode }
  TBytecodeCompiler = class
  private
    FProgram: TBytecodeProgram;
    FSSAProgram: TSSAProgram;  // Reference to current SSA program (for array metadata lookup)
    FLabelMap: TStringList;  // Maps label names to bytecode offsets
    FJumpFixups: array of TJumpFixup;  // List of jumps needing resolution
    FMaxRegisterUsed: array[TSSARegisterType] of Integer;  // Track max registers per type

    { SSA Versioning Support: Map (RegType:RegIndex:Version) → Bytecode Register }
    FRegisterMap: array[TSSARegisterType] of TStringList;  // Key="RegIndex:Version", Object=BytecodeReg as PtrInt
    FNextBytecodeReg: array[TSSARegisterType] of Integer;  // Next available bytecode register per type

    function CompileSSAOpCode(OpCode: TSSAOpCode): TBytecodeOp;
    function GetTypedArrayLoadOp(ArrayIndex: Integer): TBytecodeOp;
    function GetTypedArrayStoreOp(ArrayIndex: Integer): TBytecodeOp;
    procedure CompileInstruction(Instr: TSSAInstruction);
    procedure ResolveLabels;
    procedure AddJumpFixup(BCIndex: Integer; const TargetLabel: string);
    function MapSSARegisterToBytecode(RegType: TSSARegisterType; RegIndex, Version: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(SSAProgram: TSSAProgram): TBytecodeProgram;
  end;

implementation

{$IFDEF DEBUG_BYTECODE}
uses SedaiDebug;
{$ENDIF}

constructor TBytecodeCompiler.Create;
var
  RegType: TSSARegisterType;
begin
  inherited Create;
  FProgram := nil;
  FLabelMap := TStringList.Create;
  FLabelMap.Sorted := True;
  SetLength(FJumpFixups, 0);

  // Initialize SSA→Bytecode register mapping
  for RegType := Low(TSSARegisterType) to High(TSSARegisterType) do
  begin
    FRegisterMap[RegType] := TStringList.Create;
    FRegisterMap[RegType].Sorted := True;
    FRegisterMap[RegType].Duplicates := dupIgnore;
    FNextBytecodeReg[RegType] := 0;
  end;
end;

destructor TBytecodeCompiler.Destroy;
var
  RegType: TSSARegisterType;
begin
  FLabelMap.Free;
  SetLength(FJumpFixups, 0);

  // Free SSA→Bytecode register maps
  for RegType := Low(TSSARegisterType) to High(TSSARegisterType) do
    FRegisterMap[RegType].Free;

  inherited Destroy;
end;

procedure TBytecodeCompiler.AddJumpFixup(BCIndex: Integer; const TargetLabel: string);
var
  Len: Integer;
begin
  Len := Length(FJumpFixups);
  SetLength(FJumpFixups, Len + 1);
  FJumpFixups[Len].BCInstrIndex := BCIndex;
  FJumpFixups[Len].TargetLabel := TargetLabel;
end;

function TBytecodeCompiler.CompileSSAOpCode(OpCode: TSSAOpCode): TBytecodeOp;
begin
  // Direct mapping from SSA opcodes to bytecode opcodes
  case OpCode of
    ssaLoadConstInt: Result := bcLoadConstInt;
    ssaLoadConstFloat: Result := bcLoadConstFloat;
    ssaLoadConstString: Result := bcLoadConstString;
    ssaCopyInt: Result := bcCopyInt;
    ssaCopyFloat: Result := bcCopyFloat;
    ssaCopyString: Result := bcCopyString;
    ssaLoadVar: Result := bcLoadVar;
    ssaStoreVar: Result := bcStoreVar;
    ssaAddInt: Result := bcAddInt;
    ssaSubInt: Result := bcSubInt;
    ssaMulInt: Result := bcMulInt;
    ssaDivInt: Result := bcDivInt;
    ssaModInt: Result := bcModInt;
    ssaNegInt: Result := bcNegInt;
    ssaAddFloat: Result := bcAddFloat;
    ssaSubFloat: Result := bcSubFloat;
    ssaMulFloat: Result := bcMulFloat;
    ssaDivFloat: Result := bcDivFloat;
    ssaPowFloat: Result := bcPowFloat;
    ssaNegFloat: Result := bcNegFloat;
    ssaIntToFloat: Result := bcIntToFloat;
    ssaFloatToInt: Result := bcFloatToInt;
    // Comparison operators
    ssaCmpEqInt: Result := bcCmpEqInt;
    ssaCmpNeInt: Result := bcCmpNeInt;
    ssaCmpLtInt: Result := bcCmpLtInt;
    ssaCmpGtInt: Result := bcCmpGtInt;
    ssaCmpLeInt: Result := bcCmpLeInt;
    ssaCmpGeInt: Result := bcCmpGeInt;
    ssaCmpEqFloat: Result := bcCmpEqFloat;
    ssaCmpNeFloat: Result := bcCmpNeFloat;
    ssaCmpLtFloat: Result := bcCmpLtFloat;
    ssaCmpGtFloat: Result := bcCmpGtFloat;
    ssaCmpLeFloat: Result := bcCmpLeFloat;
    ssaCmpGeFloat: Result := bcCmpGeFloat;
    ssaCmpEqString: Result := bcCmpEqString;
    ssaCmpNeString: Result := bcCmpNeString;
    ssaCmpLtString: Result := bcCmpLtString;
    ssaCmpGtString: Result := bcCmpGtString;
    // Math functions
    ssaMathAbs: Result := bcMathAbs;
    ssaMathSgn: Result := bcMathSgn;
    ssaMathInt: Result := bcMathInt;
    ssaMathSqr: Result := bcMathSqr;
    ssaMathSin: Result := bcMathSin;
    ssaMathCos: Result := bcMathCos;
    ssaMathTan: Result := bcMathTan;
    ssaMathExp: Result := bcMathExp;
    ssaMathLog: Result := bcMathLog;
    ssaMathRnd: Result := bcMathRnd;
    ssaPrint: Result := bcPrint;
    ssaPrintLn: Result := bcPrintLn;
    ssaPrintString: Result := bcPrintString;
    ssaPrintStringLn: Result := bcPrintStringLn;
    ssaInput: Result := bcInput;
    ssaInputInt: Result := bcInputInt;
    ssaInputFloat: Result := bcInputFloat;
    ssaInputString: Result := bcInputString;
    ssaEnd: Result := bcEnd;
    ssaJump: Result := bcJump;
    ssaJumpIfZero: Result := bcJumpIfZero;
    ssaJumpIfNotZero: Result := bcJumpIfNotZero;
    ssaCall: Result := bcCall;
    ssaReturn: Result := bcReturn;
    // Array operations
    ssaArrayDim: Result := bcArrayDim;
    // ssaArrayLoad and ssaArrayStore are handled specially in CompileInstruction
    // to emit typed opcodes (bcArrayLoadInt/Float/String, bcArrayStoreInt/Float/String)
    ssaArrayLoad: Result := bcArrayLoad;  // Fallback - should use GetTypedArrayLoadOp
    ssaArrayStore: Result := bcArrayStore;  // Fallback - should use GetTypedArrayStoreOp
  else
    Result := bcNop;
  end;
end;

function TBytecodeCompiler.GetTypedArrayLoadOp(ArrayIndex: Integer): TBytecodeOp;
var
  ArrInfo: TSSAArrayInfo;
begin
  // Look up array metadata to determine element type
  if (ArrayIndex >= 0) and (ArrayIndex < FSSAProgram.GetArrayCount) then
  begin
    ArrInfo := FSSAProgram.GetArray(ArrayIndex);
    case ArrInfo.ElementType of
      srtInt: Result := bcArrayLoadInt;
      srtFloat: Result := bcArrayLoadFloat;
      srtString: Result := bcArrayLoadString;
    else
      Result := bcArrayLoad;  // Fallback to generic
    end;
  end
  else
    Result := bcArrayLoad;  // Fallback for invalid index
end;

function TBytecodeCompiler.GetTypedArrayStoreOp(ArrayIndex: Integer): TBytecodeOp;
var
  ArrInfo: TSSAArrayInfo;
begin
  // Look up array metadata to determine element type
  if (ArrayIndex >= 0) and (ArrayIndex < FSSAProgram.GetArrayCount) then
  begin
    ArrInfo := FSSAProgram.GetArray(ArrayIndex);
    case ArrInfo.ElementType of
      srtInt: Result := bcArrayStoreInt;
      srtFloat: Result := bcArrayStoreFloat;
      srtString: Result := bcArrayStoreString;
    else
      Result := bcArrayStore;  // Fallback to generic
    end;
  end
  else
    Result := bcArrayStore;  // Fallback for invalid index
end;

function TBytecodeCompiler.MapSSARegisterToBytecode(RegType: TSSARegisterType; RegIndex, Version: Integer): Integer;
var
  Key: string;
  Idx: Integer;
begin
  { Map SSA versioned register (RegType:RegIndex:Version) to a unique bytecode register.

    With SSA versioning:
    - INT[0]_1, INT[0]_2, INT[0]_3 are DIFFERENT registers
    - Each needs a unique bytecode register number

    Without versioning (Version=0):
    - Use RegIndex directly for backward compatibility

    NOTE: VM has 3 separate register arrays (FIntRegs, FFloatRegs, FStringRegs),
    each with 0..65535 slots. So INT R0 and FLOAT R0 are ALREADY separate!
    No need for base offsets!
  }

  if Version = 0 then
  begin
    // Unversioned register (legacy mode) - use RegIndex directly
    Result := RegIndex;
    {$IFDEF DEBUG_BYTECODE}
    if DebugBytecode then
      WriteLn('[BytecodeCompiler] Map SSA R', RegIndex, '_v0 (UNVERSIONED) -> Bytecode R', Result);
    {$ENDIF}
    Exit;
  end;

  // Versioned register - use mapping
  Key := Format('%d:%d', [RegIndex, Version]);
  Idx := FRegisterMap[RegType].IndexOf(Key);

  if Idx >= 0 then
  begin
    // Already mapped
    Result := Integer(PtrInt(FRegisterMap[RegType].Objects[Idx]));
    {$IFDEF DEBUG_BYTECODE}
    if DebugBytecode then
      WriteLn('[BytecodeCompiler] Map SSA R', RegIndex, '_v', Version, ' -> Bytecode R', Result, ' (cached)');
    {$ENDIF}
  end
  else
  begin
    // Allocate new bytecode register (no base offset needed - VM has separate arrays!)
    Result := FNextBytecodeReg[RegType];
    Inc(FNextBytecodeReg[RegType]);
    FRegisterMap[RegType].AddObject(Key, TObject(PtrInt(Result)));
    {$IFDEF DEBUG_BYTECODE}
    if DebugBytecode then
      WriteLn('[BytecodeCompiler] Map SSA R', RegIndex, '_v', Version, ' -> Bytecode R', Result, ' (NEW)');
    {$ENDIF}
  end;
end;

procedure TBytecodeCompiler.CompileInstruction(Instr: TSSAInstruction);
var
  BCInstr: TBytecodeInstruction;
  BCOp: TBytecodeOp;
  FloatVal: Double;
  BCIndex: Integer;
  ArrayIdx: Integer;
begin
  // Skip NOP instructions - they are dead code from optimizations
  if Instr.OpCode = ssaNop then
    Exit;

  // Special handling for array operations to emit typed opcodes
  if Instr.OpCode = ssaArrayLoad then
  begin
    // Get array index from Src1 (svkArrayRef)
    if Instr.Src1.Kind = svkArrayRef then
      ArrayIdx := Instr.Src1.ArrayIndex
    else
      ArrayIdx := -1;
    BCOp := GetTypedArrayLoadOp(ArrayIdx);
  end
  else if Instr.OpCode = ssaArrayStore then
  begin
    // Get array index from Src1 (svkArrayRef)
    if Instr.Src1.Kind = svkArrayRef then
      ArrayIdx := Instr.Src1.ArrayIndex
    else
      ArrayIdx := -1;
    BCOp := GetTypedArrayStoreOp(ArrayIdx);
  end
  else
    BCOp := CompileSSAOpCode(Instr.OpCode);

  BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

  // Map SSA values to bytecode operands
  if Instr.Dest.Kind = svkRegister then
    BCInstr.Dest := MapSSARegisterToBytecode(Instr.Dest.RegType, Instr.Dest.RegIndex, Instr.Dest.Version)
  else if Instr.Dest.Kind = svkLabel then
  begin
    // This is a jump - record for later resolution
    BCInstr.Immediate := -1;  // Temporary marker
  end;

  // Handle Src1
  if Instr.Src1.Kind = svkRegister then
  begin
    BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);
  end
  else if Instr.Src1.Kind = svkArrayRef then
    BCInstr.Src1 := Instr.Src1.ArrayIndex  // Array reference stores array index
  else if Instr.Src1.Kind = svkConstInt then
    BCInstr.Immediate := Instr.Src1.ConstInt
  else if Instr.Src1.Kind = svkConstFloat then
  begin
    FloatVal := Instr.Src1.ConstFloat;
    BCInstr.Immediate := Int64(Pointer(@FloatVal)^);
  end
  else if Instr.Src1.Kind = svkConstString then
  begin
    // Add string to constant pool and store index in Immediate
    BCInstr.Immediate := FProgram.AddStringConstant(Instr.Src1.ConstString);
  end;

  // Handle Src2
  if Instr.Src2.Kind = svkRegister then
    BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version)
  else if Instr.Src2.Kind = svkConstInt then
  begin
    // CRITICAL FIX: Binary ops don't support immediate constants in Src2!
    // The VM expects register indices, not immediate values.
    // This is a bytecode architecture limitation - all operands must be registers.
    // If ConstProp puts a constant here, we have a problem!
    // For now, we'll just leave Src2 as 0 and let ConstProp handle this properly.
    // TODO: Either materialize constants into temp registers, or fix VM architecture.
    BCInstr.Src2 := 0;  // Default to R0 (will cause wrong results!)
  end
  else if Instr.Src2.Kind = svkConstFloat then
  begin
    // Same issue as ConstInt - VM doesn't support immediate floats in Src2
    BCInstr.Src2 := 0;  // Default to R0 (will cause wrong results!)
  end;

  // Handle Src3 (for multidimensional arrays)
  if Instr.Src3.Kind = svkRegister then
    BCInstr.Immediate := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version);

  // Copy source line number for error reporting
  BCInstr.SourceLine := Instr.SourceLine;

  // Add instruction and record index for jump fixup if needed
  BCIndex := FProgram.GetInstructionCount;
  FProgram.AddInstruction(BCInstr);

  // If this is a jump with a label, add to fixup list
  if (Instr.OpCode in [ssaJump, ssaJumpIfZero, ssaJumpIfNotZero, ssaCall]) and
     (Instr.Dest.Kind = svkLabel) then
  begin
    AddJumpFixup(BCIndex, Instr.Dest.LabelName);
  end;
end;

procedure TBytecodeCompiler.ResolveLabels;
var
  i, LabelIdx, TargetAddr: Integer;
  Fixup: TJumpFixup;
  BCInstr: TBytecodeInstruction;
begin
  // Resolve all jump fixups
  for i := 0 to Length(FJumpFixups) - 1 do
  begin
    Fixup := FJumpFixups[i];
    LabelIdx := FLabelMap.IndexOf(Fixup.TargetLabel);
    if LabelIdx >= 0 then
    begin
      TargetAddr := PtrInt(FLabelMap.Objects[LabelIdx]);
      // Update the bytecode instruction's Immediate field
      BCInstr := FProgram.GetInstruction(Fixup.BCInstrIndex);
      BCInstr.Immediate := TargetAddr;
      FProgram.SetInstruction(Fixup.BCInstrIndex, BCInstr);
    end;
  end;
end;

function TBytecodeCompiler.Compile(SSAProgram: TSSAProgram): TBytecodeProgram;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  i, j, BCReg: Integer;
  RegType: TSSARegisterType;
begin
  FProgram := TBytecodeProgram.Create;
  FSSAProgram := SSAProgram;  // Store reference for array metadata lookup
  FLabelMap.Clear;
  SetLength(FJumpFixups, 0);

  // Initialize max register tracking and register mapping
  for RegType := Low(TSSARegisterType) to High(TSSARegisterType) do
  begin
    FMaxRegisterUsed[RegType] := -1;  // No registers used yet
    FRegisterMap[RegType].Clear;       // Clear previous mapping
    FNextBytecodeReg[RegType] := 0;    // Reset counter
  end;

  // Scan SSA program to build register mapping and track max bytecode register
  for i := 0 to SSAProgram.Blocks.Count - 1 do
  begin
    Block := SSAProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];
      // Check Dest
      if Instr.Dest.Kind = svkRegister then
      begin
        BCReg := MapSSARegisterToBytecode(Instr.Dest.RegType, Instr.Dest.RegIndex, Instr.Dest.Version);
        if BCReg > FMaxRegisterUsed[Instr.Dest.RegType] then
          FMaxRegisterUsed[Instr.Dest.RegType] := BCReg;
      end;
      // Check Src1
      if Instr.Src1.Kind = svkRegister then
      begin
        BCReg := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);
        if BCReg > FMaxRegisterUsed[Instr.Src1.RegType] then
          FMaxRegisterUsed[Instr.Src1.RegType] := BCReg;
      end;
      // Check Src2
      if Instr.Src2.Kind = svkRegister then
      begin
        BCReg := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);
        if BCReg > FMaxRegisterUsed[Instr.Src2.RegType] then
          FMaxRegisterUsed[Instr.Src2.RegType] := BCReg;
      end;
      // Check Src3
      if Instr.Src3.Kind = svkRegister then
      begin
        BCReg := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version);
        if BCReg > FMaxRegisterUsed[Instr.Src3.RegType] then
          FMaxRegisterUsed[Instr.Src3.RegType] := BCReg;
      end;
    end;
  end;

  // Pass 1: Compile instructions and record label positions
  for i := 0 to SSAProgram.Blocks.Count - 1 do
  begin
    Block := SSAProgram.Blocks[i];
    if Block.LabelName <> '' then
      FLabelMap.AddObject(Block.LabelName, TObject(PtrInt(FProgram.GetInstructionCount)));

    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];
      if Instr.OpCode <> ssaLabel then
        CompileInstruction(Instr);
    end;
  end;

  // Pass 2: Resolve jump labels
  ResolveLabels;

  // Copy array metadata from SSA to Bytecode
  for i := 0 to SSAProgram.GetArrayCount - 1 do
    FProgram.AddArrayInfo(SSAProgram.GetArray(i));

  Result := FProgram;
end;

end.
