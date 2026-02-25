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
    ssaModFloat: Result := bcModFloat;
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
    // Bitwise operators
    ssaBitwiseAnd: Result := bcBitwiseAnd;
    ssaBitwiseOr: Result := bcBitwiseOr;
    ssaBitwiseXor: Result := bcBitwiseXor;
    ssaBitwiseNot: Result := bcBitwiseNot;
    // String functions
    ssaStrConcat: Result := bcStrConcat;
    ssaStrLen: Result := bcStrLen;
    ssaStrLeft: Result := bcStrLeft;
    ssaStrRight: Result := bcStrRight;
    ssaStrMid: Result := bcStrMid;
    ssaStrAsc: Result := bcStrAsc;
    ssaStrChr: Result := bcStrChr;
    ssaStrStr: Result := bcStrStr;
    ssaStrVal: Result := bcStrVal;
    ssaStrHex: Result := bcStrHex;
    ssaStrInstr: Result := bcStrInstr;
    ssaStrErr: Result := bcStrErr;
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
    ssaMathLog10: Result := bcMathLog10;
    ssaMathLog2: Result := bcMathLog2;
    ssaMathLogN: Result := bcMathLogN;
    ssaMathRnd: Result := bcMathRnd;
    ssaStrDec: Result := bcStrDec;
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
    ssaPrintEnd: Result := bcPrintEnd;
    ssaInput: Result := bcInput;
    ssaInputInt: Result := bcInputInt;
    ssaInputFloat: Result := bcInputFloat;
    ssaInputString: Result := bcInputString;
    ssaEnd: Result := bcEnd;
    ssaStop: Result := bcStop;
    ssaKey: Result := bcKey;
    ssaTron: Result := bcTron;
    ssaTroff: Result := bcTroff;
    ssaTrap: Result := bcTrap;
    ssaResume: Result := bcResume;
    ssaResumeNext: Result := bcResumeNext;
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
    ssaGraphicColor: Result := bcGraphicColor;
    ssaSetColor: Result := bcSetColor;
    ssaGetColor: Result := bcGetColor;
    ssaGraphicWidth: Result := bcGraphicWidth;
    ssaGraphicScale: Result := bcGraphicScale;
    ssaGraphicPaint: Result := bcGraphicPaint;
    ssaGraphicWindow: Result := bcGraphicWindow;
    ssaGraphicSShape: Result := bcGraphicSShape;
    ssaGraphicGShape: Result := bcGraphicGShape;
    ssaGraphicGList: Result := bcGraphicGList;
    ssaPLoad: Result := bcPLoad;
    ssaPSave: Result := bcPSave;
    ssaPRst: Result := bcPRst;
    ssaScnClr: Result := bcScnClr;
    ssaGraphicPos: Result := bcGraphicPos;
    ssaGraphicRclr: Result := bcGraphicRclr;
    ssaGraphicRwindow: Result := bcGraphicRwindow;
    // System commands
    ssaFast: Result := bcFast;
    ssaSlow: Result := bcSlow;
    ssaSleep: Result := bcSleep;
    ssaClear: Result := bcClear;
    // Sound commands
    ssaSoundVol: Result := bcSoundVol;
    ssaSoundSound: Result := bcSoundSound;
    ssaSoundEnvelope: Result := bcSoundEnvelope;
    ssaSoundTempo: Result := bcSoundTempo;
    ssaSoundPlay: Result := bcSoundPlay;
    ssaSoundFilter: Result := bcSoundFilter;
    // Special variables
    ssaLoadTI: Result := bcLoadTI;
    ssaLoadTIS: Result := bcLoadTIS;
    ssaStoreTIS: Result := bcStoreTIS;
    ssaLoadDTS: Result := bcLoadDTS;
    ssaLoadCWDS: Result := bcLoadCWDS;
    ssaLoadEL: Result := bcLoadEL;
    ssaLoadER: Result := bcLoadER;
    ssaLoadERRS: Result := bcLoadERRS;
    ssaFre: Result := bcFre;
    // Memory operations
    ssaPeek: Result := bcPeek;
    ssaPoke: Result := bcPoke;
    // DATA/READ/RESTORE
    ssaDataAdd: Result := bcDataAdd;
    ssaDataRead: Result := bcDataReadFloat;  // Type-specific bytecode selected in CompileInstruction
    ssaDataRestore: Result := bcDataRestore;
    // Input commands
    ssaGet: Result := bcGet;
    ssaGetkey: Result := bcGetkey;
    // Formatted output
    ssaPrintUsing: Result := bcPrintUsing;
    ssaPudef: Result := bcPudef;
    ssaChar: Result := bcChar;
    // File operations
    ssaLoad: Result := bcLoad;
    ssaSave: Result := bcSave;
    ssaVerify: Result := bcVerify;
    ssaBload: Result := bcBload;
    ssaBsave: Result := bcBsave;
    ssaBoot: Result := bcBoot;
    // Disk file I/O
    ssaDopen: Result := bcDopen;
    ssaDclose: Result := bcDclose;
    ssaOpen: Result := bcOpen;
    ssaClose: Result := bcClose;
    ssaAppend: Result := bcAppend;
    ssaDclear: Result := bcDclear;
    ssaRecord: Result := bcRecord;
    // File data I/O
    ssaGetFile: Result := bcGetFile;
    ssaInputFile: Result := bcInputFile;
    ssaPrintFile: Result := bcPrintFile;
    ssaPrintFileNewLine: Result := bcPrintFileNewLine;
    ssaCmd: Result := bcCmd;
    // Sprite commands
    ssaSprite: Result := bcSprite;
    ssaMovsprAbs: Result := bcMovsprAbs;
    ssaMovsprRel: Result := bcMovsprRel;
    ssaMovsprPolar: Result := bcMovsprPolar;
    ssaMovsprAuto: Result := bcMovsprAuto;
    ssaSprcolor: Result := bcSprcolor;
    ssaSprsav: Result := bcSprsav;
    ssaCollision: Result := bcCollision;
    // Sprite functions
    ssaBump: Result := bcBump;
    ssaRspcolor: Result := bcRspcolor;
    ssaRsppos: Result := bcRsppos;
    ssaRsprite: Result := bcRsprite;
    // System commands
    ssaRun: Result := bcRun;
    ssaList: Result := bcList;
    ssaNew: Result := bcNew;
    ssaDelete: Result := bcDelete;
    ssaRenumber: Result := bcRenumber;
    ssaCatalog: Result := bcCatalog;
    // File management commands
    ssaCopyFile: Result := bcCopyFile;
    ssaScratch: Result := bcScratch;
    ssaRenameFile: Result := bcRenameFile;
    ssaConcat: Result := bcConcat;
    ssaMkdir: Result := bcMkdir;
    ssaChdir: Result := bcChdir;
    ssaMoveFile: Result := bcMoveFile;
    {$IFDEF WEB_MODE}
    // Web operations
    ssaWebGetParam: Result := bcWebGetParam;
    ssaWebPostParam: Result := bcWebPostParam;
    ssaWebGetRaw: Result := bcWebGetRaw;
    ssaWebPostRaw: Result := bcWebPostRaw;
    ssaWebHtmlEncode: Result := bcWebHtmlEncode;
    ssaWebUrlEncode: Result := bcWebUrlEncode;
    ssaWebMethod: Result := bcWebMethod;
    ssaWebPath: Result := bcWebPath;
    ssaWebQuery: Result := bcWebQuery;
    ssaWebHeader: Result := bcWebHeader;
    ssaWebSetHeader: Result := bcWebSetHeader;
    ssaWebStatus: Result := bcWebStatus;
    {$ENDIF}
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
      WriteLn('[BytecodeCompiler] Map SSA ', SSARegisterTypeToString(RegType), '[', RegIndex, ']_v0 (UNVERSIONED) -> Bytecode R', Result);
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
    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
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
        BCInstr.Immediate := BCInstr.Immediate or
          (Int64(MapSSARegisterToBytecode(Instr.PhiSources[0].Value.RegType,
            Instr.PhiSources[0].Value.RegIndex, Instr.PhiSources[0].Value.Version)) and $FFF);
      end;
      // y2 in bits 12-23
      if Instr.PhiSources[1].Value.Kind = svkRegister then
      begin
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[1].Value.RegType,
            Instr.PhiSources[1].Value.RegIndex, Instr.PhiSources[1].Value.Version)) and $FFF) shl 12);
      end;
      // angle in bits 24-35
      if Instr.PhiSources[2].Value.Kind = svkRegister then
      begin
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[2].Value.RegType,
            Instr.PhiSources[2].Value.RegIndex, Instr.PhiSources[2].Value.Version)) and $FFF) shl 24);
      end;
      // filled in bits 36-47
      if Instr.PhiSources[3].Value.Kind = svkRegister then
      begin
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[3].Value.RegType,
            Instr.PhiSources[3].Value.RegIndex, Instr.PhiSources[3].Value.Version)) and $FFF) shl 36);
      end;
      // fill_color in bits 48-59
      if Instr.PhiSources[4].Value.Kind = svkRegister then
      begin
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[4].Value.RegType,
            Instr.PhiSources[4].Value.RegIndex, Instr.PhiSources[4].Value.Version)) and $FFF) shl 48);
      end;
    end;

    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
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

    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for SETCOLOR - pack 5 parameters into bytecode format
  // SETCOLOR index, R, G, B [, A]
  // SSA: Src1=index, Src2=R, Dest=G, Src3=B, PhiSources[0]=A
  if Instr.OpCode = ssaSetColor then
  begin
    BCOp := bcSetColor;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

    // Src1 = palette index register
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);

    // Src2 = R register
    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);

    // Dest = G register (repurposed since SETCOLOR has no result)
    if Instr.Dest.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Dest.RegType, Instr.Dest.RegIndex, Instr.Dest.Version);

    // Pack B and A into Immediate
    // Layout: B(12) | A(12) = 24 bits
    BCInstr.Immediate := 0;
    if Instr.Src3.Kind = svkRegister then
      BCInstr.Immediate := BCInstr.Immediate or
        (Int64(MapSSARegisterToBytecode(Instr.Src3.RegType,
          Instr.Src3.RegIndex, Instr.Src3.Version)) and $FFF);

    // A in bits 12-23
    if (Length(Instr.PhiSources) >= 1) and (Instr.PhiSources[0].Value.Kind = svkRegister) then
      BCInstr.Immediate := BCInstr.Immediate or
        ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[0].Value.RegType,
          Instr.PhiSources[0].Value.RegIndex, Instr.PhiSources[0].Value.Version)) and $FFF) shl 12);

    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
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

    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for SOUND - pack 8 parameters into bytecode format
  // SOUND voice, freq, duration [,dir, minfreq, sweeptime, waveform, pulsewidth]
  // SSA: Src1=voice, Src2=freq, Src3=duration, PhiSources[0..4]=dir,minfreq,sweeptime,waveform,pw
  if Instr.OpCode = ssaSoundSound then
  begin
    BCOp := bcSoundSound;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

    // Src1 = voice register
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);

    // Src2 = freq register
    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);

    // Dest = duration register (repurposed since SOUND has no result)
    if Instr.Src3.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version);

    // Pack additional params (dir, minfreq, sweeptime, waveform, pw) into Immediate
    // Layout: dir(8) | minfreq(12) | sweeptime(12) | waveform(8) | pw(12) = 52 bits
    BCInstr.Immediate := 0;
    if Length(Instr.PhiSources) >= 1 then
    begin
      // dir in bits 0-7 (sweep direction: 0=up, 1=down, 2=oscillate)
      if Instr.PhiSources[0].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          (Int64(MapSSARegisterToBytecode(Instr.PhiSources[0].Value.RegType,
            Instr.PhiSources[0].Value.RegIndex, Instr.PhiSources[0].Value.Version)) and $FF);
    end;
    if Length(Instr.PhiSources) >= 2 then
    begin
      // minfreq in bits 8-19
      if Instr.PhiSources[1].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[1].Value.RegType,
            Instr.PhiSources[1].Value.RegIndex, Instr.PhiSources[1].Value.Version)) and $FFF) shl 8);
    end;
    if Length(Instr.PhiSources) >= 3 then
    begin
      // sweeptime in bits 20-31
      if Instr.PhiSources[2].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[2].Value.RegType,
            Instr.PhiSources[2].Value.RegIndex, Instr.PhiSources[2].Value.Version)) and $FFF) shl 20);
    end;
    if Length(Instr.PhiSources) >= 4 then
    begin
      // waveform in bits 32-39
      if Instr.PhiSources[3].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[3].Value.RegType,
            Instr.PhiSources[3].Value.RegIndex, Instr.PhiSources[3].Value.Version)) and $FF) shl 32);
    end;
    if Length(Instr.PhiSources) >= 5 then
    begin
      // pulsewidth in bits 40-51
      if Instr.PhiSources[4].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[4].Value.RegType,
            Instr.PhiSources[4].Value.RegIndex, Instr.PhiSources[4].Value.Version)) and $FFF) shl 40);
    end;

    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for ENVELOPE - pack 7 parameters into bytecode format
  // ENVELOPE envelope# [,attack, decay, sustain, release, waveform, pulsewidth]
  // SSA: Src1=envelope#, PhiSources[0..5]=attack,decay,sustain,release,waveform,pw
  if Instr.OpCode = ssaSoundEnvelope then
  begin
    BCOp := bcSoundEnvelope;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

    // Src1 = envelope number register
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);

    // Pack ADSR + waveform + pw into Immediate
    // Layout: attack(8) | decay(8) | sustain(8) | release(8) | waveform(8) | pw(12) = 52 bits
    BCInstr.Immediate := 0;
    if Length(Instr.PhiSources) >= 1 then
    begin
      // attack in bits 0-7
      if Instr.PhiSources[0].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          (Int64(MapSSARegisterToBytecode(Instr.PhiSources[0].Value.RegType,
            Instr.PhiSources[0].Value.RegIndex, Instr.PhiSources[0].Value.Version)) and $FF);
    end;
    if Length(Instr.PhiSources) >= 2 then
    begin
      // decay in bits 8-15
      if Instr.PhiSources[1].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[1].Value.RegType,
            Instr.PhiSources[1].Value.RegIndex, Instr.PhiSources[1].Value.Version)) and $FF) shl 8);
    end;
    if Length(Instr.PhiSources) >= 3 then
    begin
      // sustain in bits 16-23
      if Instr.PhiSources[2].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[2].Value.RegType,
            Instr.PhiSources[2].Value.RegIndex, Instr.PhiSources[2].Value.Version)) and $FF) shl 16);
    end;
    if Length(Instr.PhiSources) >= 4 then
    begin
      // release in bits 24-31
      if Instr.PhiSources[3].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[3].Value.RegType,
            Instr.PhiSources[3].Value.RegIndex, Instr.PhiSources[3].Value.Version)) and $FF) shl 24);
    end;
    if Length(Instr.PhiSources) >= 5 then
    begin
      // waveform in bits 32-39
      if Instr.PhiSources[4].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[4].Value.RegType,
            Instr.PhiSources[4].Value.RegIndex, Instr.PhiSources[4].Value.Version)) and $FF) shl 32);
    end;
    if Length(Instr.PhiSources) >= 6 then
    begin
      // pulsewidth in bits 40-51
      if Instr.PhiSources[5].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[5].Value.RegType,
            Instr.PhiSources[5].Value.RegIndex, Instr.PhiSources[5].Value.Version)) and $FFF) shl 40);
    end;

    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for FILTER - pack 5 parameters into bytecode format
  // FILTER cutoff, lowpass, bandpass, highpass, resonance
  // SSA: Src1=cutoff, Src2=lp, Src3=bp, PhiSources[0]=hp, PhiSources[1]=res
  if Instr.OpCode = ssaSoundFilter then
  begin
    BCOp := bcSoundFilter;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

    // Src1 = cutoff frequency register (float)
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);

    // Src2 = lowpass register (int, used as Src2)
    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);

    // Dest = bandpass register (int, repurposed since FILTER has no result)
    if Instr.Src3.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version);

    // Pack remaining filter params into Immediate
    // Layout: hp_reg(8) | res_reg(8) = 16 bits
    BCInstr.Immediate := 0;
    if Length(Instr.PhiSources) >= 1 then
    begin
      // highpass register in bits 0-7
      if Instr.PhiSources[0].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          (Int64(MapSSARegisterToBytecode(Instr.PhiSources[0].Value.RegType,
            Instr.PhiSources[0].Value.RegIndex, Instr.PhiSources[0].Value.Version)) and $FF);
    end;
    if Length(Instr.PhiSources) >= 2 then
    begin
      // resonance register in bits 8-15
      if Instr.PhiSources[1].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[1].Value.RegType,
            Instr.PhiSources[1].Value.RegIndex, Instr.PhiSources[1].Value.Version)) and $FF) shl 8);
    end;

    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for PLAY - music string parameter only
  // PLAY "music string" - voice is specified inside the string with Vn
  // SSA: Src1=string register
  if Instr.OpCode = ssaSoundPlay then
  begin
    BCOp := bcSoundPlay;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

    // Src1 = string register containing music string
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);

    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for SPRITE - pack 7 parameters into bytecode format
  // SPRITE n [,enabled] [,color] [,priority] [,scalex] [,scaley] [,mode]
  // SSA: Src1=n, Src2=enabled, Src3=color, PhiSources[0..3]=priority,scalex,scaley,mode
  if Instr.OpCode = ssaSprite then
  begin
    BCOp := bcSprite;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

    // Src1 = sprite number register
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);

    // Src2 = enabled register
    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);

    // Dest = color register (repurposed since SPRITE has no result)
    if Instr.Src3.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version);

    // Pack additional params (priority, scalex, scaley, mode) into Immediate
    // Layout: priority(12) | scalex(12) | scaley(12) | mode(12) = 48 bits
    BCInstr.Immediate := 0;
    if Length(Instr.PhiSources) >= 1 then
    begin
      // priority in bits 0-11
      if Instr.PhiSources[0].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          (Int64(MapSSARegisterToBytecode(Instr.PhiSources[0].Value.RegType,
            Instr.PhiSources[0].Value.RegIndex, Instr.PhiSources[0].Value.Version)) and $FFF);
    end;
    if Length(Instr.PhiSources) >= 2 then
    begin
      // scalex in bits 12-23
      if Instr.PhiSources[1].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[1].Value.RegType,
            Instr.PhiSources[1].Value.RegIndex, Instr.PhiSources[1].Value.Version)) and $FFF) shl 12);
    end;
    if Length(Instr.PhiSources) >= 3 then
    begin
      // scaley in bits 24-35
      if Instr.PhiSources[2].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[2].Value.RegType,
            Instr.PhiSources[2].Value.RegIndex, Instr.PhiSources[2].Value.Version)) and $FFF) shl 24);
    end;
    if Length(Instr.PhiSources) >= 4 then
    begin
      // mode in bits 36-47
      if Instr.PhiSources[3].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[3].Value.RegType,
            Instr.PhiSources[3].Value.RegIndex, Instr.PhiSources[3].Value.Version)) and $FFF) shl 36);
    end;

    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for MOVSPR - map Src3 to Dest (not Immediate)
  // MOVSPR has 3 float operands: Src1=spriteNum, Src2=x/dist/angle, Src3=y/angle/speed
  // Src3 must go to Dest since there is no return value
  if Instr.OpCode in [ssaMovsprAbs, ssaMovsprRel, ssaMovsprPolar, ssaMovsprAuto] then
  begin
    BCOp := CompileSSAOpCode(Instr.OpCode);
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);

    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);

    if Instr.Src3.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version);

    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for SCALE - map Src3 to Dest (not Immediate)
  if Instr.OpCode = ssaGraphicScale then
  begin
    BCOp := bcGraphicScale;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

    // Src1 = enable flag register
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);

    // Src2 = xmax register
    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);

    // Dest = ymax register (from Src3)
    if Instr.Src3.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version);

    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for PAINT - pack PhiSources[0]=mode into Immediate
  if Instr.OpCode = ssaGraphicPaint then
  begin
    BCOp := bcGraphicPaint;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

    // Src1 = color source register
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);

    // Src2 = x register
    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);

    // Dest = y register (repurposed)
    if Instr.Src3.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version);

    // PhiSources[0] = mode
    BCInstr.Immediate := 0;
    if Length(Instr.PhiSources) >= 1 then
    begin
      if Instr.PhiSources[0].Value.Kind = svkRegister then
        BCInstr.Immediate := MapSSARegisterToBytecode(Instr.PhiSources[0].Value.RegType,
          Instr.PhiSources[0].Value.RegIndex, Instr.PhiSources[0].Value.Version)
      else if Instr.PhiSources[0].Value.Kind = svkConstInt then
        BCInstr.Immediate := Instr.PhiSources[0].Value.ConstInt;
    end;

    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for WINDOW - pack PhiSources[0]=row2, [1]=clear into Immediate
  if Instr.OpCode = ssaGraphicWindow then
  begin
    BCOp := bcGraphicWindow;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

    // Src1 = col1 register
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);

    // Src2 = row1 register
    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);

    // Dest = col2 register (repurposed)
    if Instr.Src3.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version);

    // Pack row2 (bits 0-15) and clear (bits 16-31) into Immediate
    BCInstr.Immediate := 0;
    if Length(Instr.PhiSources) >= 1 then
    begin
      if Instr.PhiSources[0].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          (Int64(MapSSARegisterToBytecode(Instr.PhiSources[0].Value.RegType,
            Instr.PhiSources[0].Value.RegIndex, Instr.PhiSources[0].Value.Version)) and $FFFF);
    end;
    if Length(Instr.PhiSources) >= 2 then
    begin
      if Instr.PhiSources[1].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[1].Value.RegType,
            Instr.PhiSources[1].Value.RegIndex, Instr.PhiSources[1].Value.Version)) and $FFFF) shl 16)
      else if Instr.PhiSources[1].Value.Kind = svkConstInt then
        BCInstr.Immediate := BCInstr.Immediate or ((Int64(Instr.PhiSources[1].Value.ConstInt) and $FFFF) shl 16);
    end;

    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for SSHAPE - pack PhiSources[0]=y2 into Immediate
  if Instr.OpCode = ssaGraphicSShape then
  begin
    BCOp := bcGraphicSShape;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

    // Dest = string register (output) - now using register instead of variable reference
    if Instr.Dest.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Dest.RegType, Instr.Dest.RegIndex, Instr.Dest.Version);

    // Src1 = x1 register
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);

    // Src2 = y1 register
    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);

    // Pack x2 (from Src3) and y2 (from PhiSources[0]) into Immediate
    BCInstr.Immediate := 0;
    if Instr.Src3.Kind = svkRegister then
      BCInstr.Immediate := BCInstr.Immediate or
        (Int64(MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version)) and $FFFF)
    else if Instr.Src3.Kind = svkConstInt then
      BCInstr.Immediate := BCInstr.Immediate or (Int64(Instr.Src3.ConstInt) and $FFFF);

    if Length(Instr.PhiSources) >= 1 then
    begin
      if Instr.PhiSources[0].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[0].Value.RegType,
            Instr.PhiSources[0].Value.RegIndex, Instr.PhiSources[0].Value.Version)) and $FFFF) shl 16)
      else if Instr.PhiSources[0].Value.Kind = svkConstInt then
        BCInstr.Immediate := BCInstr.Immediate or ((Int64(Instr.PhiSources[0].Value.ConstInt) and $FFFF) shl 16);
    end;

    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for GSHAPE - pack PhiSources[0]=mode into Immediate
  if Instr.OpCode = ssaGraphicGShape then
  begin
    BCOp := bcGraphicGShape;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);

    // Src1 = string variable (source) - store variable name index in string constants
    if Instr.Src1.Kind = svkVariable then
      BCInstr.Src1 := FProgram.AddStringConstant(Instr.Src1.VarName)
    else if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);

    // Src2 = x register
    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version)
    else if Instr.Src2.Kind = svkConstInt then
      BCInstr.Src2 := Instr.Src2.ConstInt;

    // Dest = y register (repurposed)
    if Instr.Src3.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version)
    else if Instr.Src3.Kind = svkConstInt then
      BCInstr.Dest := Instr.Src3.ConstInt;

    // PhiSources[0] = mode
    BCInstr.Immediate := 0;
    if Length(Instr.PhiSources) >= 1 then
    begin
      if Instr.PhiSources[0].Value.Kind = svkRegister then
        BCInstr.Immediate := MapSSARegisterToBytecode(Instr.PhiSources[0].Value.RegType,
          Instr.PhiSources[0].Value.RegIndex, Instr.PhiSources[0].Value.Version)
      else if Instr.PhiSources[0].Value.Kind = svkConstInt then
        BCInstr.Immediate := Instr.PhiSources[0].Value.ConstInt;
    end;

    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for DATA/READ/RESTORE
  if Instr.OpCode = ssaDataAdd then
  begin
    // bcDataAdd stores value type and value in immediate
    // Src1 contains the constant value
    BCInstr := MakeBytecodeInstruction(bcDataAdd, 0, 0, 0, 0);
    if Instr.Src1.Kind = svkConstInt then
    begin
      BCInstr.Src1 := Ord(srtInt);  // Type indicator
      BCInstr.Immediate := Instr.Src1.ConstInt;
    end
    else if Instr.Src1.Kind = svkConstFloat then
    begin
      BCInstr.Src1 := Ord(srtFloat);  // Type indicator
      FloatVal := Instr.Src1.ConstFloat;
      BCInstr.Immediate := Int64(Pointer(@FloatVal)^);
    end
    else if Instr.Src1.Kind = svkConstString then
    begin
      BCInstr.Src1 := Ord(srtString);  // Type indicator
      BCInstr.Immediate := FProgram.AddStringConstant(Instr.Src1.ConstString);
    end;
    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  if Instr.OpCode = ssaDataRead then
  begin
    // Dest = register to read into
    // Src1 = type indicator (srtInt, srtFloat, srtString)
    if Instr.Src1.Kind = svkConstInt then
    begin
      case TSSARegisterType(Instr.Src1.ConstInt) of
        srtInt: BCOp := bcDataReadInt;
        srtFloat: BCOp := bcDataReadFloat;
        srtString: BCOp := bcDataReadString;
      else
        BCOp := bcDataReadFloat;  // Default
      end;
    end
    else
      BCOp := bcDataReadFloat;

    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);
    if Instr.Dest.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Dest.RegType, Instr.Dest.RegIndex, Instr.Dest.Version);
    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  if Instr.OpCode = ssaDataRestore then
  begin
    BCInstr := MakeBytecodeInstruction(bcDataRestore, 0, 0, 0, 0);
    // Src1 = line number (0 = beginning)
    if Instr.Src1.Kind = svkConstInt then
      BCInstr.Immediate := Instr.Src1.ConstInt;
    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for PRINT# - select type-specific opcode based on register type
  if Instr.OpCode = ssaPrintFile then
  begin
    // Dest = value to print, Src1 = file handle
    if Instr.Dest.Kind = svkRegister then
    begin
      case Instr.Dest.RegType of
        srtInt: BCOp := bcPrintFileInt;
        srtFloat: BCOp := bcPrintFileFloat;
        srtString: BCOp := bcPrintFile;
      else
        BCOp := bcPrintFile;
      end;
      BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Dest.RegType, Instr.Dest.RegIndex, Instr.Dest.Version);
      if Instr.Src1.Kind = svkRegister then
        BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);
      FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
      Exit;
    end;
  end;

  // Special handling for INPUT# - select type-specific opcode based on variable type
  if Instr.OpCode = ssaInputFile then
  begin
    // Dest = variable to store result, Src1 = file handle
    if Instr.Dest.Kind = svkRegister then
    begin
      case Instr.Dest.RegType of
        srtInt: BCOp := bcInputFileInt;
        srtFloat: BCOp := bcInputFileFloat;
        srtString: BCOp := bcInputFile;
      else
        BCOp := bcInputFile;
      end;
      BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Dest.RegType, Instr.Dest.RegIndex, Instr.Dest.Version);
      if Instr.Src1.Kind = svkRegister then
        BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);
      FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
      Exit;
    end;
  end;

  // Special handling for GET and GETKEY
  if Instr.OpCode in [ssaGet, ssaGetkey] then
  begin
    if Instr.OpCode = ssaGet then
      BCOp := bcGet
    else
      BCOp := bcGetkey;
    BCInstr := MakeBytecodeInstruction(BCOp, 0, 0, 0, 0);
    if Instr.Dest.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Dest.RegType, Instr.Dest.RegIndex, Instr.Dest.Version);
    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for PRINT USING
  if Instr.OpCode = ssaPrintUsing then
  begin
    BCInstr := MakeBytecodeInstruction(bcPrintUsing, 0, 0, 0, 0);
    // Src1 = format string register
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);
    // Src2 = value register
    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);
    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for PUDEF
  if Instr.OpCode = ssaPudef then
  begin
    BCInstr := MakeBytecodeInstruction(bcPudef, 0, 0, 0, 0);
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version)
    else if Instr.Src1.Kind = svkConstString then
      BCInstr.Immediate := FProgram.AddStringConstant(Instr.Src1.ConstString);
    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for CHAR - pack 5 parameters
  if Instr.OpCode = ssaChar then
  begin
    BCInstr := MakeBytecodeInstruction(bcChar, 0, 0, 0, 0);
    // Src1 = mode, Src2 = col, Dest = row (repurposed)
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);
    if Instr.Src2.Kind = svkRegister then
      BCInstr.Src2 := MapSSARegisterToBytecode(Instr.Src2.RegType, Instr.Src2.RegIndex, Instr.Src2.Version);
    if Instr.Src3.Kind = svkRegister then
      BCInstr.Dest := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version);
    // Pack text and reverse register indices in Immediate
    BCInstr.Immediate := 0;
    if Length(Instr.PhiSources) >= 1 then
    begin
      if Instr.PhiSources[0].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          (Int64(MapSSARegisterToBytecode(Instr.PhiSources[0].Value.RegType,
            Instr.PhiSources[0].Value.RegIndex, Instr.PhiSources[0].Value.Version)) and $FFFF);
    end;
    if Length(Instr.PhiSources) >= 2 then
    begin
      if Instr.PhiSources[1].Value.Kind = svkRegister then
        BCInstr.Immediate := BCInstr.Immediate or
          ((Int64(MapSSARegisterToBytecode(Instr.PhiSources[1].Value.RegType,
            Instr.PhiSources[1].Value.RegIndex, Instr.PhiSources[1].Value.Version)) and $FFFF) shl 16);
    end;
    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for TRAP - use constant directly in Immediate to avoid SSA versioning issues
  if Instr.OpCode = ssaTrap then
  begin
    BCInstr := MakeBytecodeInstruction(bcTrap, 0, 0, 0, 0);
    if Instr.Src1.Kind = svkConstInt then
    begin
      // TRAP with constant line number - store in Immediate
      BCInstr.Immediate := Instr.Src1.ConstInt;
      BCInstr.Src1 := 0;  // Flag: use Immediate instead of register
    end
    else if Instr.Src1.Kind = svkRegister then
    begin
      // TRAP with variable line number (rare) - use register
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version);
      BCInstr.Immediate := -1;  // Flag: use Src1 register
    end;
    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
    Exit;
  end;

  // Special handling for RESUME - same issue with register mapping
  if Instr.OpCode = ssaResume then
  begin
    BCInstr := MakeBytecodeInstruction(bcResume, 0, 0, 0, 0);
    if Instr.Src1.Kind = svkRegister then
      BCInstr.Src1 := MapSSARegisterToBytecode(Instr.Src1.RegType, Instr.Src1.RegIndex, Instr.Src1.Version)
    else if Instr.Src1.Kind = svkConstInt then
      BCInstr.Immediate := Instr.Src1.ConstInt;
    FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);
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
  {$IFDEF DEBUG_BYTECODE}
  if DebugBytecode then
  begin
    if Instr.OpCode = ssaLoadEL then
      WriteLn('[BC] LoadEL: Dest=', SSARegisterTypeToString(Instr.Dest.RegType), '[', Instr.Dest.RegIndex, ']_v', Instr.Dest.Version);
    if Instr.OpCode = ssaLoadER then
      WriteLn('[BC] LoadER: Dest=', SSARegisterTypeToString(Instr.Dest.RegType), '[', Instr.Dest.RegIndex, ']_v', Instr.Dest.Version);
    if Instr.OpCode = ssaPrintInt then
      WriteLn('[BC] PrintInt: Src1=', SSARegisterTypeToString(Instr.Src1.RegType), '[', Instr.Src1.RegIndex, ']_v', Instr.Src1.Version);
  end;
  {$ENDIF}

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
  begin
    // For ssaLoadConstFloat with integer constant, convert to float first
    if Instr.OpCode = ssaLoadConstFloat then
    begin
      FloatVal := Instr.Src1.ConstInt;  // Convert int to float
      BCInstr.Immediate := Int64(Pointer(@FloatVal)^);
    end
    else
      BCInstr.Immediate := Instr.Src1.ConstInt;
  end
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

  // Handle Src3 (for multidimensional arrays and string function immediates)
  if Instr.Src3.Kind = svkRegister then
    BCInstr.Immediate := MapSSARegisterToBytecode(Instr.Src3.RegType, Instr.Src3.RegIndex, Instr.Src3.Version)
  else if Instr.Src3.Kind = svkConstInt then
    BCInstr.Immediate := Instr.Src3.ConstInt
  else if Instr.Src3.Kind = svkConstFloat then
    BCInstr.Immediate := Trunc(Instr.Src3.ConstFloat);

  {$IFDEF DEBUG_BYTECODE}
  if DebugBytecode and (Instr.OpCode in [ssaPrintInt, ssaLoadEL, ssaLoadER]) then
    WriteLn('[BC] Final instruction: Op=', Ord(BCOp), ' Dest=', BCInstr.Dest, ' Src1=', BCInstr.Src1, ' Src2=', BCInstr.Src2, ' @L', Instr.SourceLine);
  {$ENDIF}

  // Add instruction and record index for jump fixup if needed
  BCIndex := FProgram.GetInstructionCount;
  FProgram.AddInstructionWithLine(BCInstr, Instr.SourceLine);

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
  IntVarCount, FloatVarCount, StringVarCount: Integer;
  VarRegStr: string;
  ColonPos: Integer;
begin
  FProgram := TBytecodeProgram.Create;
  FSSAProgram := SSAProgram;  // Store reference for array metadata lookup
  FLabelMap.Clear;
  SetLength(FJumpFixups, 0);

  // Find max variable register index for each type from VarRegMap
  // VarRegMap values are "RegType:RegIndex" - we need max index + 1 per type
  // This ensures all variable registers are preserved with identity mapping
  IntVarCount := 0;
  FloatVarCount := 0;
  StringVarCount := 0;
  for i := 0 to SSAProgram.VarRegMap.Count - 1 do
  begin
    VarRegStr := SSAProgram.VarRegMap.ValueFromIndex[i];
    ColonPos := Pos(':', VarRegStr);
    if ColonPos > 0 then
    begin
      j := StrToIntDef(Copy(VarRegStr, ColonPos + 1, Length(VarRegStr)), -1);  // RegIndex
      case StrToIntDef(Copy(VarRegStr, 1, ColonPos - 1), -1) of
        Ord(srtInt): if j + 1 > IntVarCount then IntVarCount := j + 1;
        Ord(srtFloat): if j + 1 > FloatVarCount then FloatVarCount := j + 1;
        Ord(srtString): if j + 1 > StringVarCount then StringVarCount := j + 1;
      end;
    end;
  end;
  FProgram.SetVarRegCounts(IntVarCount, FloatVarCount, StringVarCount);

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

  // Register block labels in Source Map for blocks that map to existing PCs
  // This allows FindPCForLine to find REM lines and other non-code lines
  for i := 0 to FLabelMap.Count - 1 do
  begin
    if Copy(FLabelMap[i], 1, 5) = 'LINE_' then
    begin
      j := StrToIntDef(Copy(FLabelMap[i], 6, Length(FLabelMap[i]) - 5), 0);
      if j > 0 then
        FProgram.AddLabelLine(j, PtrInt(FLabelMap.Objects[i]));
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
