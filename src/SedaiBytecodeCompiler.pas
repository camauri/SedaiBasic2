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
    ssaPrintInt: Result := bcPrintInt;
    ssaPrintIntLn: Result := bcPrintIntLn;
    ssaPrintComma: Result := bcPrintComma;
    ssaPrintSemicolon: Result := bcPrintSemicolon;
    ssaPrintTab: Result := bcPrintTab;
    ssaPrintSpc: Result := bcPrintSpc;
    ssaPrintNewLine: Result := bcPrintNewLine;
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
    // Graphics operations
    ssaGraphicRGBA: Result := bcGraphicRGBA;
    ssaGraphicSetMode: Result := bcGraphicSetMode;
    ssaGraphicBox: Result := bcGraphicBox;
    ssaGraphicCircle: Result := bcGraphicCircle;
    ssaGraphicDraw: Result := bcGraphicDraw;
    ssaGraphicLocate: Result := bcGraphicLocate;
    ssaGraphicRdot: Result := bcGraphicRdot;
    ssaGraphicGetMode: Result := bcGraphicGetMode;
    // System commands
    ssaFast: Result := bcFast;
    ssaSlow: Result := bcSlow;
    ssaSleep: Result := bcSleep;
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
  // For RGBA handling
  ARegMapped, BRegMapped: Integer;
begin
  // Skip NOP instructions - they are dead code from optimizations
  if Instr.OpCode = ssaNop then
    Exit;

  // Special handling for RGBA - pack 4 register indices into bytecode format
  if Instr.OpCode = ssaGraphicRGBA then
  begin
    BCOp := bcGraphicRGBA;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);
    // Map destination register
    if Instr.Dest.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Dest.RegType, Instr.Dest.RegIndex, Instr.Dest.Version);
    // Src1 = R register
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);
    // Src2 = G register
    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);
    // Src3 = B register, A is stored in PhiSources[0]
    // Pack both B and A register indices into Immediate: (B << 16) | A
    if Instr.Src3.Kind = svkRegister then
    begin
      // B register comes from Src3
      BRegMapped := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version);

      // A register comes from PhiSources[0]
      ARegMapped := 0;
      if Length(Instr.PhiSources) > 0 then
      begin
        with Instr.PhiSources[0].Value do
        begin
          if Kind = svkRegister then
            ARegMapped := MapSSARegisterToBytecode(RegType, RegIndex, Version);
          WriteLn(StdErr, 'RGBA: B_bc=', BRegMapped, ' A_bc=', ARegMapped,
            ' A_ssa(type=', Ord(RegType), ',idx=', RegIndex, ',ver=', Version, ')');
        end;
      end
      else
        WriteLn(StdErr, 'RGBA: B=', BRegMapped, ' NO PhiSources!');

      BCInstr.Immediate := (BRegMapped shl 16) or ARegMapped;
    end;
    BCInstr.SourceLine := Instr.SourceLine;
    FProgram.AddInstruction(BCInstr);
    Exit;
  end;

  // Special handling for BOX - pack 8 parameters into bytecode format
  // BOX color, x1, y1, x2, y2, angle, filled, fill_color
  // SSA: Src1=color, Src2=x1, Src3=y1, PhiSources[0..4]=x2,y2,angle,filled,fill_color
  if Instr.OpCode = ssaGraphicBox then
  begin
    BCOp := bcGraphicBox;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

    // Src1 = color register
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);

    // Src2 = x1 register
    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);

    // Dest = y1 register (repurposed since BOX has no result)
    if Instr.Src3.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version);

    // Pack additional params (x2, y2, angle, filled, fill_color) into Immediate
    // Format: bits 0-11: x2, bits 12-23: y2, bits 24-35: angle, bits 36-39: filled, bits 40-51: fill_color
    // But since Word max is 65535, we use 12 bits per register index
    // Immediate is Int64, so we have 64 bits available
    // Layout: x2(12) | y2(12) | angle(12) | filled(12) | fill_color(12) = 60 bits
    if Length(Instr.PhiSources) >= 5 then
    begin
      BCInstr.Immediate := 0;
      // x2 in bits 0-11
      if Instr.PhiSources[0].Value.Kind = svkRegister then
      begin
        WriteLn('>>> BOX Compiler: x2 SSA=', Instr.PhiSources[0].Value.RegIndex, '_v', Instr.PhiSources[0].Value.Version,
                ' -> BC=', MapSSARegisterToBytecode(Instr.PhiSources[0].Value.RegType,
                  Instr.PhiSources[0].Value.RegIndex, Instr.PhiSources[0].Value.Version));
        BCInstr.Immediate := BCInstr.Immediate or
          (Int64(MapSSARegisterToBytecode(Instr.PhiSources[0].Value.RegType,
            Instr.PhiSources[0].Value.RegIndex, Instr.PhiSources[0].Value.Version)) and $FFF);
      end;
      // y2 in bits 12-23
      if Instr.PhiSources[1].Value.Kind = svkRegister then
      begin
        WriteLn('>>> BOX Compiler: y2 SSA=', Instr.PhiSources[1].Value.RegIndex, '_v', Instr.PhiSources[1].Value.Version,
                ' -> BC=', MapSSARegisterToBytecode(Instr.PhiSources[1].Value.RegType,
                  Instr.PhiSources[1].Value.RegIndex, Instr.PhiSources[1].Value.Version));
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[1].Value.RegType,
            Instr.PhiSources[1].Value.RegIndex, Instr.PhiSources[1].Value.Version)) and $FFF) shl 12);
      end;
      // angle in bits 24-35
      if Instr.PhiSources[2].Value.Kind = svkRegister then
      begin
        WriteLn('>>> BOX Compiler: angle SSA=', Instr.PhiSources[2].Value.RegIndex, '_v', Instr.PhiSources[2].Value.Version,
                ' -> BC=', MapSSARegisterToBytecode(Instr.PhiSources[2].Value.RegType,
                  Instr.PhiSources[2].Value.RegIndex, Instr.PhiSources[2].Value.Version));
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[2].Value.RegType,
            Instr.PhiSources[2].Value.RegIndex, Instr.PhiSources[2].Value.Version)) and $FFF) shl 24);
      end;
      // filled in bits 36-47
      if Instr.PhiSources[3].Value.Kind = svkRegister then
      begin
        WriteLn('>>> BOX Compiler: filled SSA=', Instr.PhiSources[3].Value.RegIndex, '_v', Instr.PhiSources[3].Value.Version,
                ' -> BC=', MapSSARegisterToBytecode(Instr.PhiSources[3].Value.RegType,
                  Instr.PhiSources[3].Value.RegIndex, Instr.PhiSources[3].Value.Version));
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[3].Value.RegType,
            Instr.PhiSources[3].Value.RegIndex, Instr.PhiSources[3].Value.Version)) and $FFF) shl 36);
      end;
      // fill_color in bits 48-59
      if Instr.PhiSources[4].Value.Kind = svkRegister then
      begin
        WriteLn('>>> BOX Compiler: fill_color SSA=', Instr.PhiSources[4].Value.RegIndex, '_v', Instr.PhiSources[4].Value.Version,
                ' -> BC=', MapSSARegisterToBytecode(Instr.PhiSources[4].Value.RegType,
                  Instr.PhiSources[4].Value.RegIndex, Instr.PhiSources[4].Value.Version));
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[4].Value.RegType,
            Instr.PhiSources[4].Value.RegIndex, Instr.PhiSources[4].Value.Version)) and $FFF) shl 48);
      end;
    end;

    BCInstr.SourceLine := Instr.SourceLine;
    FProgram.AddInstruction(BCInstr);
    Exit;
  end;

  // Special handling for CIRCLE - pack 9 parameters into bytecode format
  // CIRCLE color, x, y, xr, yr, sa, ea, angle, inc
  // SSA: Src1=color, Src2=x, Src3=y, PhiSources[0..5]=xr,yr,sa,ea,angle,inc
  if Instr.OpCode = ssaGraphicCircle then
  begin
    BCOp := bcGraphicCircle;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

    // Src1 = color register
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);

    // Src2 = x register
    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);

    // Dest = y register (repurposed since CIRCLE has no result)
    if Instr.Src3.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version);

    // Pack additional params (xr, yr, sa, ea, angle, inc) into Immediate
    // Layout: xr(10) | yr(10) | sa(10) | ea(10) | angle(10) | inc(10) = 60 bits
    if Length(Instr.PhiSources) >= 6 then
    begin
      BCInstr.Immediate := 0;
      // xr in bits 0-9
      if Instr.PhiSources[0].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          (Int64(MapSSARegisterToBytecode(Instr.PhiSources[0].Value.RegType,
            Instr.PhiSources[0].Value.RegIndex, Instr.PhiSources[0].Value.Version)) and $3FF);
      // yr in bits 10-19
      if Instr.PhiSources[1].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[1].Value.RegType,
            Instr.PhiSources[1].Value.RegIndex, Instr.PhiSources[1].Value.Version)) and $3FF) shl 10);
      // sa in bits 20-29
      if Instr.PhiSources[2].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[2].Value.RegType,
            Instr.PhiSources[2].Value.RegIndex, Instr.PhiSources[2].Value.Version)) and $3FF) shl 20);
      // ea in bits 30-39
      if Instr.PhiSources[3].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[3].Value.RegType,
            Instr.PhiSources[3].Value.RegIndex, Instr.PhiSources[3].Value.Version)) and $3FF) shl 30);
      // angle in bits 40-49
      if Instr.PhiSources[4].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[4].Value.RegType,
            Instr.PhiSources[4].Value.RegIndex, Instr.PhiSources[4].Value.Version)) and $3FF) shl 40);
      // inc in bits 50-59
      if Instr.PhiSources[5].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[5].Value.RegType,
            Instr.PhiSources[5].Value.RegIndex, Instr.PhiSources[5].Value.Version)) and $3FF) shl 50);
    end;

    BCInstr.SourceLine := Instr.SourceLine;
    FProgram.AddInstruction(BCInstr);
    Exit;
  end;

  // Special handling for DRAW - pack mode into Immediate
  // DRAW color, x, y with mode in PhiSources[0]
  // SSA: Src1=color, Src2=x, Src3=y, PhiSources[0]=mode (0=move, 1=line, 2=dot)
  if Instr.OpCode = ssaGraphicDraw then
  begin
    BCOp := bcGraphicDraw;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

    // Src1 = color register
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);

    // Src2 = x register
    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);

    // Dest = y register (repurposed since DRAW has no result)
    if Instr.Src3.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version);

    // Pack mode into Immediate
    // mode: 0=move PC only, 1=draw line from PC to (x,y), 2=draw dot at (x,y)
    if Length(Instr.PhiSources) >= 1 then
    begin
      if Instr.PhiSources[0].Value.Kind = svkConstInt then
        BCInstr.Immediate := Instr.PhiSources[0].Value.ConstInt
      else if Instr.PhiSources[0].Value.Kind = svkRegister then
        BCInstr.Immediate := MapSSARegisterToBytecode(Instr.PhiSources[0].Value.RegType,
          Instr.PhiSources[0].Value.RegIndex, Instr.PhiSources[0].Value.Version) or $8000; // Flag bit to indicate register mode
    end;

    BCInstr.SourceLine := Instr.SourceLine;
    FProgram.AddInstruction(BCInstr);
    Exit;
  end;

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

  // Debug: trace opcode compilation
  if Instr.OpCode = ssaGraphicGetMode then
    WriteLn('>>> BC Compiling ssaGraphicGetMode -> bcOp=', Ord(BCOp));

  BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

  // Map SSA values to bytecode operands
  if Instr.Dest.Kind = svkRegister then
  begin
    BCInstr.Dest := MapSSARegisterToBytecode(Instr.Dest.RegType, Instr.Dest.RegIndex, Instr.Dest.Version);
    if Instr.OpCode = ssaLoadConstInt then
      WriteLn('>>> BC LoadConstInt: SSA=', Instr.Dest.RegIndex, '_v', Instr.Dest.Version,
              ' -> BC=', BCInstr.Dest, ' value=', Instr.Src1.ConstInt);
  end
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
