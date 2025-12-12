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
unit SedaiBytecodeVM;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$I DebugFlags.inc}
{$I ProfilerFlags.inc}

interface

uses
  Classes, SysUtils, Math, Variants,
  SedaiBytecodeTypes, SedaiOutputInterface, SedaiSSATypes,
  SedaiConsoleBehavior
  {$IFDEF ENABLE_PROFILER}, SedaiProfiler{$ENDIF};

type
  { Array storage structure }
  TArrayStorage = record
    ElementType: Byte;        // 0=Int, 1=Float, 2=String (maps to TSSARegisterType)
    DimCount: Integer;
    Dimensions: array of Integer;
    TotalSize: Integer;
    IntData: array of Int64;
    FloatData: array of Double;
    StringData: array of string;
  end;

  { Bytecode VM - register-based virtual machine }
  TBytecodeVM = class
  private
    FIntRegs: array of Int64;
    FFloatRegs: array of Double;
    FStringRegs: array of string;
    FTempIntRegs: array of Int64;
    FTempFloatRegs: array of Double;
    FTempStringRegs: array of string;
    FIntRegCount: Integer;       // Current size of int register arrays
    FFloatRegCount: Integer;     // Current size of float register arrays
    FStringRegCount: Integer;    // Current size of string register arrays
    FProgram: TBytecodeProgram;
    FPC: Integer;
    FRunning: Boolean;
    FCallStack: array of Integer;
    FCallStackPtr: Integer;
    FOutputDevice: IOutputDevice;
    FInputDevice: IInputDevice;
    FConsoleBehavior: TConsoleBehavior;
    FOwnsConsoleBehavior: Boolean;
    FCursorCol: Integer;  // Track cursor column for TAB zones
    FVarMap: TStringList;
    FArrays: array of TArrayStorage;
    FSwapTempInt: Int64;  // Temp variable for ArraySwapInt superinstruction
    // Temp variables for ArrayReverseRange and ArrayShiftLeft
    FStartIdx, FEndIdx, FArrIdxTmp, FLoopIdx: Integer;
    FFirstVal: Int64;
    {$IFDEF ENABLE_INSTRUCTION_COUNTING}
    FInstructionsExecuted: Int64;
    {$ENDIF}
    {$IFDEF ENABLE_PROFILER}
    FProfiler: TProfiler;
    {$ENDIF}
    procedure ExecuteInstruction(const Instr: TBytecodeInstruction);
    procedure ExecuteSuperinstruction(const Instr: TBytecodeInstruction);
    procedure InitializeRegisters;
    procedure EnsureRegisterCapacity(RegType: TSSARegisterType; MinIndex: Integer);
    procedure CheckFloatValid(RegIndex: Integer; const OpName: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadProgram(Program_: TBytecodeProgram);
    procedure SetOutputDevice(Device: IOutputDevice);
    procedure SetInputDevice(Device: IInputDevice);
    procedure SetConsoleBehavior(ABehavior: TConsoleBehavior; OwnsBehavior: Boolean = False);
    procedure ApplyPreset(Preset: TConsolePreset);
    function GetConsoleBehavior: TConsoleBehavior;
    procedure Run;
    procedure RunFast;  // Optimized execution loop - no profiler support
    procedure RunSwitchedGoto;  // Switched goto dispatch - best branch prediction
    procedure Step;
    procedure Reset;
    {$IFDEF ENABLE_INSTRUCTION_COUNTING}
    function GetInstructionsExecuted: Int64;
    property InstructionsExecuted: Int64 read FInstructionsExecuted;
    {$ENDIF}
    {$IFDEF ENABLE_PROFILER}
    procedure SetProfiler(AProfiler: TProfiler);
    property Profiler: TProfiler read FProfiler write FProfiler;
    {$ENDIF}
    property PC: Integer read FPC;
    property Running: Boolean read FRunning;
  end;

implementation

constructor TBytecodeVM.Create;
begin
  inherited Create;
  FProgram := nil;
  FPC := 0;
  FRunning := False;
  FCallStackPtr := 0;
  FCursorCol := 0;
  {$IFDEF ENABLE_INSTRUCTION_COUNTING}
  FInstructionsExecuted := 0;
  {$ENDIF}
  SetLength(FCallStack, 256);
  FVarMap := TStringList.Create;
  FVarMap.Sorted := True;
  // Create default console behavior (Commodore 64 style)
  FConsoleBehavior := TConsolePresets.CreateCommodore64;
  FOwnsConsoleBehavior := True;
  InitializeRegisters;
end;

destructor TBytecodeVM.Destroy;
begin
  if FOwnsConsoleBehavior and Assigned(FConsoleBehavior) then
    FConsoleBehavior.Free;
  FVarMap.Free;
  inherited Destroy;
end;

procedure TBytecodeVM.InitializeRegisters;
var i: Integer;
begin
  // Initialize with minimum register slots
  FIntRegCount := MIN_REGISTER_SLOTS;
  FFloatRegCount := MIN_REGISTER_SLOTS;
  FStringRegCount := MIN_REGISTER_SLOTS;

  SetLength(FIntRegs, FIntRegCount);
  SetLength(FFloatRegs, FFloatRegCount);
  SetLength(FStringRegs, FStringRegCount);
  SetLength(FTempIntRegs, FIntRegCount);
  SetLength(FTempFloatRegs, FFloatRegCount);
  SetLength(FTempStringRegs, FStringRegCount);

  for i := 0 to FIntRegCount - 1 do
  begin
    FIntRegs[i] := 0;
    FTempIntRegs[i] := 0;
  end;

  for i := 0 to FFloatRegCount - 1 do
  begin
    FFloatRegs[i] := 0.0;
    FTempFloatRegs[i] := 0.0;
  end;

  for i := 0 to FStringRegCount - 1 do
  begin
    FStringRegs[i] := '';
    FTempStringRegs[i] := '';
  end;
end;

procedure TBytecodeVM.EnsureRegisterCapacity(RegType: TSSARegisterType; MinIndex: Integer);
var
  OldSize, NewSize, i: Integer;
begin
  case RegType of
    srtInt:
    begin
      if MinIndex >= FIntRegCount then
      begin
        OldSize := FIntRegCount;
        // Double the size or use MinIndex + 1, whichever is larger (but cap at MAX)
        NewSize := Max(OldSize * 2, MinIndex + 1);
        if NewSize > MAX_REGISTER_SLOTS then
          NewSize := MAX_REGISTER_SLOTS;

        if MinIndex >= NewSize then
          raise Exception.CreateFmt('Register index %d exceeds maximum %d for integer registers',
                                    [MinIndex, MAX_REGISTER_SLOTS - 1]);

        // Grow both working and temp register arrays
        SetLength(FIntRegs, NewSize);
        SetLength(FTempIntRegs, NewSize);

        // Initialize new slots to zero
        for i := OldSize to NewSize - 1 do
        begin
          FIntRegs[i] := 0;
          FTempIntRegs[i] := 0;
        end;

        FIntRegCount := NewSize;
      end;
    end;

    srtFloat:
    begin
      if MinIndex >= FFloatRegCount then
      begin
        OldSize := FFloatRegCount;
        NewSize := Max(OldSize * 2, MinIndex + 1);
        if NewSize > MAX_REGISTER_SLOTS then
          NewSize := MAX_REGISTER_SLOTS;

        if MinIndex >= NewSize then
          raise Exception.CreateFmt('Register index %d exceeds maximum %d for float registers',
                                    [MinIndex, MAX_REGISTER_SLOTS - 1]);

        SetLength(FFloatRegs, NewSize);
        SetLength(FTempFloatRegs, NewSize);

        for i := OldSize to NewSize - 1 do
        begin
          FFloatRegs[i] := 0.0;
          FTempFloatRegs[i] := 0.0;
        end;

        FFloatRegCount := NewSize;
      end;
    end;

    srtString:
    begin
      if MinIndex >= FStringRegCount then
      begin
        OldSize := FStringRegCount;
        NewSize := Max(OldSize * 2, MinIndex + 1);
        if NewSize > MAX_REGISTER_SLOTS then
          NewSize := MAX_REGISTER_SLOTS;

        if MinIndex >= NewSize then
          raise Exception.CreateFmt('Register index %d exceeds maximum %d for string registers',
                                    [MinIndex, MAX_REGISTER_SLOTS - 1]);

        SetLength(FStringRegs, NewSize);
        SetLength(FTempStringRegs, NewSize);

        for i := OldSize to NewSize - 1 do
        begin
          FStringRegs[i] := '';
          FTempStringRegs[i] := '';
        end;

        FStringRegCount := NewSize;
      end;
    end;
  end;
end;

procedure TBytecodeVM.CheckFloatValid(RegIndex: Integer; const OpName: string);
begin
  if IsNan(FFloatRegs[RegIndex]) then
    raise Exception.CreateFmt('NaN detected in R%d after %s', [RegIndex, OpName]);
  if IsInfinite(FFloatRegs[RegIndex]) then
    raise Exception.CreateFmt('Infinity detected in R%d after %s', [RegIndex, OpName]);
end;

procedure TBytecodeVM.LoadProgram(Program_: TBytecodeProgram);
var
  i: Integer;
  Instr: TBytecodeInstruction;
  MaxIntReg, MaxFloatReg, MaxStringReg: Integer;
begin
  FProgram := Program_;

  // Scan bytecode to determine maximum register indices used
  MaxIntReg := -1;
  MaxFloatReg := -1;
  MaxStringReg := -1;

  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    Instr := FProgram.GetInstruction(i);

    // Handle superinstructions (opcode >= 110) separately
    if Instr.OpCode >= 110 then
    begin
      case Instr.OpCode of
        // Fused compare-and-branch (Int) - use IntRegs for Src1, Src2
        110..115:  // bcBranchEqInt..bcBranchGeInt
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
        end;
        // Fused compare-and-branch (Float) - use FloatRegs for Src1, Src2
        120..125:  // bcBranchEqFloat..bcBranchGeFloat
        begin
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
        end;
        // Fused arithmetic-to-dest (Int) - use IntRegs for Dest, Src1
        130..132:  // bcAddIntTo..bcMulIntTo
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;
        // Fused arithmetic-to-dest (Float) - use FloatRegs for Dest, Src1
        140..143:  // bcAddFloatTo..bcDivFloatTo
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        end;
        // Fused constant arithmetic (Int) - use IntRegs for Dest, Src1
        150..152:  // bcAddIntConst..bcMulIntConst
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;
        // Fused constant arithmetic (Float) - use FloatRegs for Dest, Src1
        160..163:  // bcAddFloatConst..bcDivFloatConst
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        end;
        // Fused compare-zero-and-branch (Int) - use IntRegs for Src1
        170, 171:  // bcBranchEqZeroInt, bcBranchNeZeroInt
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;
        // Fused compare-zero-and-branch (Float) - use FloatRegs for Src1
        180, 181:  // bcBranchEqZeroFloat, bcBranchNeZeroFloat
        begin
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        end;
        // Fused array-store-constant - use IntRegs for Src2 (index register)
        190, 191, 192:  // bcArrayStoreIntConst, bcArrayStoreFloatConst, bcArrayStoreStringConst
        begin
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
        end;
        // Fused loop increment-and-branch - use IntRegs for Dest, Src1, Src2
        200, 201, 202, 203:  // bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
        end;

        // NEW: FMA (Fused Multiply-Add) - use FloatRegs for Dest, Src1, Src2, Immediate
        210, 211:  // bcMulAddFloat, bcMulSubFloat
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
          if Instr.Immediate > MaxFloatReg then MaxFloatReg := Instr.Immediate;  // c register
        end;
        212, 213:  // bcMulAddToFloat, bcMulSubToFloat
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
        end;

        // NEW: Array Load + Arithmetic - use FloatRegs for Dest, Immediate; IntRegs for Src2
        220, 221:  // bcArrayLoadAddFloat, bcArrayLoadSubFloat
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;  // index register
          if Instr.Immediate > MaxFloatReg then MaxFloatReg := Instr.Immediate;  // acc register
        end;
        222:  // bcArrayLoadDivAddFloat - Immediate encodes two registers
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
          if (Instr.Immediate and $FFFF) > MaxFloatReg then MaxFloatReg := Instr.Immediate and $FFFF;
          if ((Instr.Immediate shr 16) and $FFFF) > MaxFloatReg then MaxFloatReg := (Instr.Immediate shr 16) and $FFFF;
        end;

        // NEW: Square-Sum patterns - use FloatRegs for Dest, Src1, Src2
        230, 231:  // bcSquareSumFloat, bcAddSquareFloat
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
        end;

        // NEW: Mul-Mul - use FloatRegs for Dest, Src1, Src2, Immediate
        240:  // bcMulMulFloat
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
          if Instr.Immediate > MaxFloatReg then MaxFloatReg := Instr.Immediate;
        end;

        // NEW: Add-Sqrt - use FloatRegs for Dest, Src1, Src2
        241:  // bcAddSqrtFloat
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
        end;

        // NEW: Array Load + Branch - use IntRegs for Src2
        250, 251:  // bcArrayLoadIntBranchNZ, bcArrayLoadIntBranchZ
        begin
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;  // index register
        end;
      end;
    end
    else
    begin
      // Check destination and source registers based on standard opcode
      case TBytecodeOp(Instr.OpCode) of
        // Int dest, int sources
        bcLoadConstInt, bcCopyInt, bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt, bcNegInt,
        bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpGtInt, bcCmpLeInt, bcCmpGeInt,
        bcLogicalAnd, bcLogicalOr, bcLogicalNot,
        bcInputInt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
        end;

        // FloatToInt: int Dest, float Src1
        bcFloatToInt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        end;

        // StringToInt: int Dest, string Src1
        bcStringToInt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        // Float comparisons: int Dest (0/1 result), float Src1, float Src2
        bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpGtFloat, bcCmpLeFloat, bcCmpGeFloat:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
        end;

        // String comparisons: int Dest (0/1 result), string Src1, string Src2
        bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
          if Instr.Src2 > MaxStringReg then MaxStringReg := Instr.Src2;
        end;

        // ArrayLoadInt: int Dest (result), int Src2 (index)
        bcArrayLoadInt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;  // index is int
        end;

        // Int source (Src1) for branch
        bcJumpIfZero, bcJumpIfNotZero:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;

        // Float dest, float sources
        bcLoadConstFloat, bcCopyFloat, bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat,
        bcPowFloat, bcNegFloat,
        bcMathAbs, bcMathSgn, bcMathInt, bcMathSqr, bcMathSin, bcMathCos, bcMathTan,
        bcMathExp, bcMathLog, bcMathAtn, bcMathRnd,
        bcInputFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
        end;

        // Type conversions with mixed register types
        // IntToFloat: float Dest, int Src1
        bcIntToFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;

        // StringToFloat: float Dest, string Src1
        bcStringToFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        // ArrayLoadFloat: float Dest (result), int Src2 (index)
        bcArrayLoadFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;  // index is int
        end;

        // String dest, string sources
        bcLoadConstString, bcCopyString, bcStrConcat,
        bcStrLeft, bcStrRight, bcStrMid,
        bcInputString:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
          if Instr.Src2 > MaxStringReg then MaxStringReg := Instr.Src2;
        end;

        // IntToString: string Dest, int Src1
        bcIntToString:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;

        // FloatToString: string Dest, float Src1
        bcFloatToString:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        end;

        // ArrayLoadString: string Dest (result), int Src2 (index)
        bcArrayLoadString:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;  // index is int
        end;

        // String Src1 (source)
        bcStrLen:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        // Print/PrintLn: float in Src1
        bcPrint, bcPrintLn:
        begin
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        end;

        // PrintString/PrintStringLn: string in Src1
        bcPrintString, bcPrintStringLn:
        begin
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        // ArrayStore: Dest is value register, Src2 is index (int)
        bcArrayStoreInt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;  // value
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;  // index
        end;
        bcArrayStoreFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;  // value
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;      // index
        end;
        bcArrayStoreString:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;  // value
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;        // index
        end;

        // ArrayLoad: Dest is result, Src2 is index (int)
        // Note: bcArrayLoadInt/Float/String already handled above
      end;
    end;
  end;

  // Reset VM state first (this initializes registers to MIN_REGISTER_SLOTS)
  Reset;

  // Then ensure we have enough capacity for all registers used
  if MaxIntReg >= 0 then
    EnsureRegisterCapacity(srtInt, MaxIntReg);
  if MaxFloatReg >= 0 then
    EnsureRegisterCapacity(srtFloat, MaxFloatReg);
  if MaxStringReg >= 0 then
    EnsureRegisterCapacity(srtString, MaxStringReg);

end;

procedure TBytecodeVM.SetOutputDevice(Device: IOutputDevice);
begin
  FOutputDevice := Device;
end;

procedure TBytecodeVM.SetInputDevice(Device: IInputDevice);
begin
  FInputDevice := Device;
end;

procedure TBytecodeVM.SetConsoleBehavior(ABehavior: TConsoleBehavior; OwnsBehavior: Boolean);
begin
  if FOwnsConsoleBehavior and Assigned(FConsoleBehavior) then
    FConsoleBehavior.Free;

  FConsoleBehavior := ABehavior;
  FOwnsConsoleBehavior := OwnsBehavior;
end;

procedure TBytecodeVM.ApplyPreset(Preset: TConsolePreset);
begin
  if Assigned(FConsoleBehavior) then
    FConsoleBehavior.ApplyPreset(Preset);
end;

function TBytecodeVM.GetConsoleBehavior: TConsoleBehavior;
begin
  Result := FConsoleBehavior;
end;

procedure TBytecodeVM.Reset;
begin
  FPC := 0;
  FRunning := False;
  FCallStackPtr := 0;
  {$IFDEF ENABLE_INSTRUCTION_COUNTING}
  FInstructionsExecuted := 0;
  {$ENDIF}
  InitializeRegisters;
  FVarMap.Clear;
end;

{$IFDEF ENABLE_INSTRUCTION_COUNTING}
function TBytecodeVM.GetInstructionsExecuted: Int64;
begin
  Result := FInstructionsExecuted;
end;
{$ENDIF}

{$IFDEF ENABLE_PROFILER}
procedure TBytecodeVM.SetProfiler(AProfiler: TProfiler);
begin
  FProfiler := AProfiler;
end;
{$ENDIF}

procedure TBytecodeVM.ExecuteInstruction(const Instr: TBytecodeInstruction);
var
  Op: TBytecodeOp;
  InputStr, PrintStr: string;
  InputVal: Double;
  ArrayIdx, LinearIdx, i, j, ProdDims: Integer;
  NextTabCol, TabIdx: Integer;
  DrawMode: Integer;
  ArrInfo: TSSAArrayInfo;
  SleepMs: Integer;  // For SLEEP command
begin
  // Handle superinstructions FIRST (opcode >= 110) to avoid enum range issues
  // Converting out-of-range values to TBytecodeOp causes undefined behavior!
  if Instr.OpCode >= 110 then
  begin
    ExecuteSuperinstruction(Instr);
    Exit;
  end;

  Op := TBytecodeOp(Instr.OpCode);
  case Op of
    bcLoadConstInt: FIntRegs[Instr.Dest] := Instr.Immediate;
    bcLoadConstFloat: FFloatRegs[Instr.Dest] := Double(Pointer(@Instr.Immediate)^);
    bcLoadConstString:
      if (Instr.Immediate >= 0) and (Instr.Immediate < FProgram.StringConstants.Count) then
        FStringRegs[Instr.Dest] := FProgram.StringConstants[Instr.Immediate];
    bcCopyInt: FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1];
    bcCopyFloat:
      begin
        FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1];
        {$IFDEF DEBUG_REGISTER_DUMP}
        // Trace copies to R38 specifically (the problematic register in n-body)
        if Instr.Dest = 38 then
        begin
          WriteLn(StdErr, 'CopyFloat at PC=', FPC, ': R[', Instr.Dest, '] ← R[', Instr.Src1, ']');
          WriteLn(StdErr, '  Source R[', Instr.Src1, '] = ', FFloatRegs[Instr.Src1]:0:17);
          WriteLn(StdErr, '  Dest   R[', Instr.Dest, '] = ', FFloatRegs[Instr.Dest]:0:17);
        end;
        {$ENDIF}
      end;
    bcCopyString: FStringRegs[Instr.Dest] := FStringRegs[Instr.Src1];
    bcAddInt: FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] + FIntRegs[Instr.Src2];
    bcSubInt: FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] - FIntRegs[Instr.Src2];
    bcMulInt: FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] * FIntRegs[Instr.Src2];
    bcDivInt:
      if FIntRegs[Instr.Src2] <> 0 then
        FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] div FIntRegs[Instr.Src2]
      else raise Exception.Create('Division by zero');
    bcModInt:
      if FIntRegs[Instr.Src2] <> 0 then
        FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] mod FIntRegs[Instr.Src2]
      else raise Exception.Create('Modulo by zero');
    bcNegInt: FIntRegs[Instr.Dest] := -FIntRegs[Instr.Src1];
    bcAddFloat:
      begin
        FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] + FFloatRegs[Instr.Src2];
        {$IFDEF DEBUG_FLOAT_CHECKS}
        CheckFloatValid(Instr.Dest, 'AddFloat');
        {$ENDIF}
        {$IFDEF DEBUG_REGISTER_DUMP}
        // Trace additions to R41 and R43 (sum of squares in n-body)
        if (Instr.Dest = 41) or (Instr.Dest = 43) then
        begin
          WriteLn(StdErr, 'AddFloat at PC=', FPC, ': R[', Instr.Dest, '] = R[', Instr.Src1, '] + R[', Instr.Src2, ']');
          WriteLn(StdErr, '  R[', Instr.Src1, '] = ', FFloatRegs[Instr.Src1]:0:17);
          WriteLn(StdErr, '  R[', Instr.Src2, '] = ', FFloatRegs[Instr.Src2]:0:17);
          WriteLn(StdErr, '  R[', Instr.Dest, '] = ', FFloatRegs[Instr.Dest]:0:17);
        end;
        {$ENDIF}
      end;
    bcSubFloat:
      begin
        FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] - FFloatRegs[Instr.Src2];
        {$IFDEF DEBUG_FLOAT_CHECKS}
        CheckFloatValid(Instr.Dest, 'SubFloat');
        {$ENDIF}
      end;
    bcMulFloat:
      begin
        FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] * FFloatRegs[Instr.Src2];
        {$IFDEF DEBUG_FLOAT_CHECKS}
        CheckFloatValid(Instr.Dest, 'MulFloat');
        {$ENDIF}
      end;
    bcDivFloat:
      begin
        if FFloatRegs[Instr.Src2] <> 0.0 then
        begin
          FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] / FFloatRegs[Instr.Src2];
          {$IFDEF DEBUG_FLOAT_CHECKS}
          CheckFloatValid(Instr.Dest, 'DivFloat');
          {$ENDIF}
        end
        else
          raise Exception.Create('Division by zero');
      end;
    bcPowFloat: FFloatRegs[Instr.Dest] := Power(FFloatRegs[Instr.Src1], FFloatRegs[Instr.Src2]);
    bcNegFloat: FFloatRegs[Instr.Dest] := -FFloatRegs[Instr.Src1];
    bcIntToFloat: FFloatRegs[Instr.Dest] := FIntRegs[Instr.Src1];
    bcFloatToInt: FIntRegs[Instr.Dest] := Trunc(FFloatRegs[Instr.Src1]);
    // Comparison operators - Int
    bcCmpEqInt: FIntRegs[Instr.Dest] := Ord(FIntRegs[Instr.Src1] = FIntRegs[Instr.Src2]);
    bcCmpNeInt: FIntRegs[Instr.Dest] := Ord(FIntRegs[Instr.Src1] <> FIntRegs[Instr.Src2]);
    bcCmpLtInt: FIntRegs[Instr.Dest] := Ord(FIntRegs[Instr.Src1] < FIntRegs[Instr.Src2]);
    bcCmpGtInt: FIntRegs[Instr.Dest] := Ord(FIntRegs[Instr.Src1] > FIntRegs[Instr.Src2]);
    bcCmpLeInt: FIntRegs[Instr.Dest] := Ord(FIntRegs[Instr.Src1] <= FIntRegs[Instr.Src2]);
    bcCmpGeInt: FIntRegs[Instr.Dest] := Ord(FIntRegs[Instr.Src1] >= FIntRegs[Instr.Src2]);
    // Comparison operators - Float
    bcCmpEqFloat: FIntRegs[Instr.Dest] := Ord(FFloatRegs[Instr.Src1] = FFloatRegs[Instr.Src2]);
    bcCmpNeFloat: FIntRegs[Instr.Dest] := Ord(FFloatRegs[Instr.Src1] <> FFloatRegs[Instr.Src2]);
    bcCmpLtFloat: FIntRegs[Instr.Dest] := Ord(FFloatRegs[Instr.Src1] < FFloatRegs[Instr.Src2]);
    bcCmpGtFloat: FIntRegs[Instr.Dest] := Ord(FFloatRegs[Instr.Src1] > FFloatRegs[Instr.Src2]);
    bcCmpLeFloat: FIntRegs[Instr.Dest] := Ord(FFloatRegs[Instr.Src1] <= FFloatRegs[Instr.Src2]);
    bcCmpGeFloat: FIntRegs[Instr.Dest] := Ord(FFloatRegs[Instr.Src1] >= FFloatRegs[Instr.Src2]);
    // Comparison operators - String
    bcCmpEqString: FIntRegs[Instr.Dest] := Ord(FStringRegs[Instr.Src1] = FStringRegs[Instr.Src2]);
    bcCmpNeString: FIntRegs[Instr.Dest] := Ord(FStringRegs[Instr.Src1] <> FStringRegs[Instr.Src2]);
    bcCmpLtString: FIntRegs[Instr.Dest] := Ord(FStringRegs[Instr.Src1] < FStringRegs[Instr.Src2]);
    bcCmpGtString: FIntRegs[Instr.Dest] := Ord(FStringRegs[Instr.Src1] > FStringRegs[Instr.Src2]);
    // Math functions
    bcMathAbs: FFloatRegs[Instr.Dest] := Abs(FFloatRegs[Instr.Src1]);
    bcMathSgn:
      if FFloatRegs[Instr.Src1] > 0 then
        FFloatRegs[Instr.Dest] := 1
      else if FFloatRegs[Instr.Src1] < 0 then
        FFloatRegs[Instr.Dest] := -1
      else
        FFloatRegs[Instr.Dest] := 0;
    bcMathInt: FFloatRegs[Instr.Dest] := Floor(FFloatRegs[Instr.Src1]);
    bcMathSqr:
      begin
        if FFloatRegs[Instr.Src1] < 0 then
        begin
          {$IFDEF DEBUG_REGISTER_DUMP}
          WriteLn(StdErr, 'Register dump at PC=', FPC, ' (MathSqr Src1=', Instr.Src1, ')');
          WriteLn(StdErr, 'R[', Instr.Src1, '] = ', FFloatRegs[Instr.Src1]:0:17, ' ← NEGATIVE (ERROR)');
          // Dump surrounding registers to check for corruption pattern
          if Instr.Src1 >= 2 then
            WriteLn(StdErr, 'R[', Instr.Src1-2, '] = ', FFloatRegs[Instr.Src1-2]:0:17);
          if Instr.Src1 >= 1 then
            WriteLn(StdErr, 'R[', Instr.Src1-1, '] = ', FFloatRegs[Instr.Src1-1]:0:17);
          if Instr.Src1 + 1 < FFloatRegCount then
            WriteLn(StdErr, 'R[', Instr.Src1+1, '] = ', FFloatRegs[Instr.Src1+1]:0:17);
          if Instr.Src1 + 2 < FFloatRegCount then
            WriteLn(StdErr, 'R[', Instr.Src1+2, '] = ', FFloatRegs[Instr.Src1+2]:0:17);
          if Instr.Src1 + 3 < FFloatRegCount then
            WriteLn(StdErr, 'R[', Instr.Src1+3, '] = ', FFloatRegs[Instr.Src1+3]:0:17);
          if Instr.Src1 + 4 < FFloatRegCount then
            WriteLn(StdErr, 'R[', Instr.Src1+4, '] = ', FFloatRegs[Instr.Src1+4]:0:17);
          if Instr.Src1 + 5 < FFloatRegCount then
            WriteLn(StdErr, 'R[', Instr.Src1+5, '] = ', FFloatRegs[Instr.Src1+5]:0:17);
          if Instr.Src1 + 6 < FFloatRegCount then
            WriteLn(StdErr, 'R[', Instr.Src1+6, '] = ', FFloatRegs[Instr.Src1+6]:0:17);
          {$ENDIF}
          raise Exception.CreateFmt('Square root of negative number: %.17e', [FFloatRegs[Instr.Src1]]);
        end
        else
          FFloatRegs[Instr.Dest] := Sqrt(FFloatRegs[Instr.Src1]);
      end;
    bcMathSin: FFloatRegs[Instr.Dest] := Sin(FFloatRegs[Instr.Src1]);
    bcMathCos: FFloatRegs[Instr.Dest] := Cos(FFloatRegs[Instr.Src1]);
    bcMathTan: FFloatRegs[Instr.Dest] := Tan(FFloatRegs[Instr.Src1]);
    bcMathExp: FFloatRegs[Instr.Dest] := Exp(FFloatRegs[Instr.Src1]);
    bcMathLog:
      if FFloatRegs[Instr.Src1] > 0 then
        FFloatRegs[Instr.Dest] := Ln(FFloatRegs[Instr.Src1])
      else
        raise Exception.Create('LOG of non-positive number');
    bcMathRnd: FFloatRegs[Instr.Dest] := Random;
    bcPrint:
      if Assigned(FOutputDevice) then
      begin
        // Print float with behavior formatting
        PrintStr := FConsoleBehavior.FormatNumber(FFloatRegs[Instr.Src1]);
        FOutputDevice.Print(PrintStr);
        Inc(FCursorCol, Length(PrintStr));
      end;
    bcPrintLn:
      if Assigned(FOutputDevice) then
      begin
        PrintStr := FConsoleBehavior.FormatNumber(FFloatRegs[Instr.Src1]);
        FOutputDevice.Print(PrintStr);
        FOutputDevice.NewLine;
        FCursorCol := 0;
      end;
    bcPrintInt:
      if Assigned(FOutputDevice) then
      begin
        // Print integer with behavior formatting
        PrintStr := FConsoleBehavior.FormatNumber(FIntRegs[Instr.Src1]);
        FOutputDevice.Print(PrintStr);
        Inc(FCursorCol, Length(PrintStr));
      end;
    bcPrintIntLn:
      if Assigned(FOutputDevice) then
      begin
        PrintStr := FConsoleBehavior.FormatNumber(FIntRegs[Instr.Src1]);
        FOutputDevice.Print(PrintStr);
        FOutputDevice.NewLine;
        FCursorCol := 0;
      end;
    bcPrintString:
      if Assigned(FOutputDevice) then
      begin
        PrintStr := FConsoleBehavior.FormatString(FStringRegs[Instr.Src1]);
        FOutputDevice.Print(PrintStr);
        Inc(FCursorCol, Length(PrintStr));
      end;
    bcPrintStringLn:
      if Assigned(FOutputDevice) then
      begin
        PrintStr := FConsoleBehavior.FormatString(FStringRegs[Instr.Src1]);
        FOutputDevice.Print(PrintStr);
        FOutputDevice.NewLine;
        FCursorCol := 0;
      end;
    bcPrintComma:
      if Assigned(FOutputDevice) then
      begin
        // Apply comma separator (TAB zone)
        NextTabCol := FConsoleBehavior.GetNextTabPosition(FCursorCol);
        if NextTabCol = 0 then
        begin
          // Wrap to next line
          FOutputDevice.NewLine;
          FCursorCol := 0;
        end
        else if FConsoleBehavior.CommaAction = caTabZone then
        begin
          // Print spaces to reach next TAB zone
          while FCursorCol < NextTabCol do
          begin
            FOutputDevice.Print(' ');
            Inc(FCursorCol);
          end;
        end
        else if FConsoleBehavior.CommaAction = caFixedSpaces then
        begin
          for TabIdx := 1 to FConsoleBehavior.CommaSpaces do
          begin
            FOutputDevice.Print(' ');
            Inc(FCursorCol);
          end;
        end
        else if FConsoleBehavior.CommaAction = caNewLine then
        begin
          FOutputDevice.NewLine;
          FCursorCol := 0;
        end;
        // caNoAction: do nothing
      end;
    bcPrintSemicolon:
      if Assigned(FOutputDevice) then
      begin
        // Apply semicolon separator
        case FConsoleBehavior.SemicolonAction of
          saNoSpace: ; // Do nothing
          saSpaceAfter:
            begin
              FOutputDevice.Print(' ');
              Inc(FCursorCol);
            end;
          saSpaceBefore: ; // Handled by next print
          saSpaceBoth:
            begin
              FOutputDevice.Print(' ');
              Inc(FCursorCol);
            end;
        end;
      end;
    bcPrintTab:
      if Assigned(FOutputDevice) then
      begin
        // TAB(n) - move to column n
        NextTabCol := Instr.Immediate;
        if NextTabCol < 1 then NextTabCol := 1;
        Dec(NextTabCol);  // Convert to 0-based

        if FCursorCol < NextTabCol then
        begin
          // Print spaces to reach column
          while FCursorCol < NextTabCol do
          begin
            FOutputDevice.Print(' ');
            Inc(FCursorCol);
          end;
        end
        else if FCursorCol > NextTabCol then
        begin
          // Already past: go to next line and tab
          FOutputDevice.NewLine;
          FCursorCol := 0;
          while FCursorCol < NextTabCol do
          begin
            FOutputDevice.Print(' ');
            Inc(FCursorCol);
          end;
        end;
      end;
    bcPrintSpc:
      if Assigned(FOutputDevice) then
      begin
        // SPC(n) - print n spaces
        for TabIdx := 1 to Instr.Immediate do
        begin
          FOutputDevice.Print(' ');
          Inc(FCursorCol);
        end;
      end;
    bcPrintNewLine:
      if Assigned(FOutputDevice) then
      begin
        FOutputDevice.NewLine;
        FCursorCol := 0;
      end;
    bcInput:
      if Assigned(FInputDevice) then
      begin
        // Generic input (backward compatibility) - accepts float
        repeat
          InputStr := FInputDevice.ReadLine('', False, True, True);
          if TryStrToFloat(InputStr, InputVal) then
          begin
            FFloatRegs[Instr.Dest] := InputVal;
            Break;
          end
          else
          begin
            if Assigned(FOutputDevice) then
              FOutputDevice.Print('?Invalid number. Try again: ');
          end;
        until False;
      end;
    bcInputInt:
      if Assigned(FInputDevice) then
      begin
        // Integer input - validates format and range
        repeat
          InputStr := FInputDevice.ReadLine('', False, True, True);
          InputStr := Trim(InputStr);

          // Try to parse as float first (to handle scientific notation like 1.5e2)
          if TryStrToFloat(InputStr, InputVal) then
          begin
            // Check if it's within Int64 range
            if (InputVal >= Low(Int64)) and (InputVal <= High(Int64)) then
            begin
              FIntRegs[Instr.Dest] := Trunc(InputVal);
              Break;
            end
            else
            begin
              if Assigned(FOutputDevice) then
                FOutputDevice.Print('?Number out of range. Try again: ');
            end;
          end
          else
          begin
            if Assigned(FOutputDevice) then
              FOutputDevice.Print('?Invalid integer. Try again: ');
          end;
        until False;
      end;
    bcInputFloat:
      if Assigned(FInputDevice) then
      begin
        // Float input - validates numeric format (including scientific notation)
        repeat
          InputStr := FInputDevice.ReadLine('', False, True, True);
          InputStr := Trim(InputStr);

          if TryStrToFloat(InputStr, InputVal) then
          begin
            FFloatRegs[Instr.Dest] := InputVal;
            Break;
          end
          else
          begin
            if Assigned(FOutputDevice) then
              FOutputDevice.Print('?Invalid number. Try again: ');
          end;
        until False;
      end;
    bcInputString:
      if Assigned(FInputDevice) then
      begin
        FStringRegs[Instr.Dest] := FInputDevice.ReadLine('', False, False, False);
      end;
    bcJump: FPC := Instr.Immediate - 1;
    bcJumpIfZero:
      if FIntRegs[Instr.Src1] = 0 then FPC := Instr.Immediate - 1;
    bcJumpIfNotZero:
      if FIntRegs[Instr.Src1] <> 0 then FPC := Instr.Immediate - 1;
    bcCall:
    begin
      FCallStack[FCallStackPtr] := FPC;
      Inc(FCallStackPtr);
      FPC := Instr.Immediate - 1;
    end;
    bcReturn:
      if FCallStackPtr > 0 then
      begin
        Dec(FCallStackPtr);
        FPC := FCallStack[FCallStackPtr];
      end;
    // Array operations
    bcArrayDim:
    begin
      // bcArrayDim: Instr.Src1 = ArrayIndex (index into metadata)
      // Allocate array storage based on metadata
      ArrayIdx := Instr.Src1;
      if (ArrayIdx < 0) or (ArrayIdx >= FProgram.GetArrayCount) then
        raise Exception.CreateFmt('Invalid array index: %d', [ArrayIdx]);

      ArrInfo := FProgram.GetArray(ArrayIdx);

      // Ensure FArrays is large enough
      if ArrayIdx >= Length(FArrays) then
        SetLength(FArrays, ArrayIdx + 1);

      // Initialize array storage
      FArrays[ArrayIdx].ElementType := Byte(ArrInfo.ElementType);
      FArrays[ArrayIdx].DimCount := ArrInfo.DimCount;
      SetLength(FArrays[ArrayIdx].Dimensions, ArrInfo.DimCount);

      // Read dimensions - if 0, read from register (variable dimension)
      for i := 0 to ArrInfo.DimCount - 1 do
      begin
        if ArrInfo.Dimensions[i] = 0 then
        begin
          // Variable dimension: read value from register and add +1 (BASIC [0..N] semantics)
          if (i < Length(ArrInfo.DimRegisters)) and (ArrInfo.DimRegisters[i] >= 0) then
          begin
            case ArrInfo.DimRegTypes[i] of
              srtInt:
                FArrays[ArrayIdx].Dimensions[i] := FIntRegs[ArrInfo.DimRegisters[i]] + 1;
              srtFloat:
                FArrays[ArrayIdx].Dimensions[i] := Trunc(FFloatRegs[ArrInfo.DimRegisters[i]]) + 1;
            else
              raise Exception.CreateFmt('Invalid dimension register type for array %s', [ArrInfo.Name]);
            end;
          end
          else
            raise Exception.CreateFmt('Array %s has undefined variable dimension %d', [ArrInfo.Name, i]);
        end
        else
          FArrays[ArrayIdx].Dimensions[i] := ArrInfo.Dimensions[i];
      end;

      // Calculate total size (product of all dimensions)
      ProdDims := 1;
      for i := 0 to ArrInfo.DimCount - 1 do
        ProdDims := ProdDims * FArrays[ArrayIdx].Dimensions[i];
      FArrays[ArrayIdx].TotalSize := ProdDims;

      // Allocate storage based on element type
      case ArrInfo.ElementType of
        srtInt:
        begin
          SetLength(FArrays[ArrayIdx].IntData, ProdDims);
          for i := 0 to ProdDims - 1 do
            FArrays[ArrayIdx].IntData[i] := 0;
        end;
        srtFloat:
        begin
          SetLength(FArrays[ArrayIdx].FloatData, ProdDims);
          for i := 0 to ProdDims - 1 do
            FArrays[ArrayIdx].FloatData[i] := 0.0;
        end;
        srtString:
        begin
          SetLength(FArrays[ArrayIdx].StringData, ProdDims);
          for i := 0 to ProdDims - 1 do
            FArrays[ArrayIdx].StringData[i] := '';
        end;
      end;
    end;
    bcArrayLoad:
    begin
      // bcArrayLoad: Dest = result register, Src1 = ArrayIndex, Src2 = linear index register
      // Linear index is pre-computed by the SSA generator for all array dimensions
      ArrayIdx := Instr.Src1;
      if (ArrayIdx < 0) or (ArrayIdx >= Length(FArrays)) then
        raise Exception.CreateFmt('Array not allocated: %d', [ArrayIdx]);

      // Get pre-computed linear index from register
      LinearIdx := FIntRegs[Instr.Src2];

      // Bounds check
      if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
        raise Exception.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);

      // Load value into destination register
      case FArrays[ArrayIdx].ElementType of
        0: FIntRegs[Instr.Dest] := FArrays[ArrayIdx].IntData[LinearIdx];
        1: FFloatRegs[Instr.Dest] := FArrays[ArrayIdx].FloatData[LinearIdx];
        2: FStringRegs[Instr.Dest] := FArrays[ArrayIdx].StringData[LinearIdx];
      end;
    end;
    bcArrayStore:
    begin
      // bcArrayStore: Dest = value register, Src1 = ArrayIndex, Src2 = linear index register
      // Linear index is pre-computed by the SSA generator for all array dimensions
      ArrayIdx := Instr.Src1;
      if (ArrayIdx < 0) or (ArrayIdx >= Length(FArrays)) then
        raise Exception.CreateFmt('Array not allocated: %d', [ArrayIdx]);

      // Get pre-computed linear index from register
      LinearIdx := FIntRegs[Instr.Src2];

      // Bounds check
      if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
        raise Exception.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);

      // Store value from source register
      case FArrays[ArrayIdx].ElementType of
        0: FArrays[ArrayIdx].IntData[LinearIdx] := FIntRegs[Instr.Dest];
        1: FArrays[ArrayIdx].FloatData[LinearIdx] := FFloatRegs[Instr.Dest];
        2: FArrays[ArrayIdx].StringData[LinearIdx] := FStringRegs[Instr.Dest];
      end;
    end;

    // Typed array load operations - type is encoded in opcode, no runtime dispatch
    bcArrayLoadInt:
    begin
      ArrayIdx := Instr.Src1;
      LinearIdx := FIntRegs[Instr.Src2];
      FIntRegs[Instr.Dest] := FArrays[ArrayIdx].IntData[LinearIdx];
      {$IFDEF ENABLE_PROFILER}
      if Assigned(FProfiler) and FProfiler.Enabled then
        FProfiler.OnArrayAccess(ArrayIdx, False, LinearIdx);
      {$ENDIF}
    end;
    bcArrayLoadFloat:
    begin
      ArrayIdx := Instr.Src1;
      LinearIdx := FIntRegs[Instr.Src2];
      FFloatRegs[Instr.Dest] := FArrays[ArrayIdx].FloatData[LinearIdx];
      {$IFDEF ENABLE_PROFILER}
      if Assigned(FProfiler) and FProfiler.Enabled then
        FProfiler.OnArrayAccess(ArrayIdx, False, LinearIdx);
      {$ENDIF}
    end;
    bcArrayLoadString:
    begin
      ArrayIdx := Instr.Src1;
      LinearIdx := FIntRegs[Instr.Src2];
      FStringRegs[Instr.Dest] := FArrays[ArrayIdx].StringData[LinearIdx];
      {$IFDEF ENABLE_PROFILER}
      if Assigned(FProfiler) and FProfiler.Enabled then
        FProfiler.OnArrayAccess(ArrayIdx, False, LinearIdx);
      {$ENDIF}
    end;

    // Typed array store operations - type is encoded in opcode, no runtime dispatch
    bcArrayStoreInt:
    begin
      ArrayIdx := Instr.Src1;
      LinearIdx := FIntRegs[Instr.Src2];
      FArrays[ArrayIdx].IntData[LinearIdx] := FIntRegs[Instr.Dest];
      {$IFDEF ENABLE_PROFILER}
      if Assigned(FProfiler) and FProfiler.Enabled then
        FProfiler.OnArrayAccess(ArrayIdx, True, LinearIdx);
      {$ENDIF}
    end;
    bcArrayStoreFloat:
    begin
      ArrayIdx := Instr.Src1;
      LinearIdx := FIntRegs[Instr.Src2];
      FArrays[ArrayIdx].FloatData[LinearIdx] := FFloatRegs[Instr.Dest];
      {$IFDEF ENABLE_PROFILER}
      if Assigned(FProfiler) and FProfiler.Enabled then
        FProfiler.OnArrayAccess(ArrayIdx, True, LinearIdx);
      {$ENDIF}
    end;
    bcArrayStoreString:
    begin
      ArrayIdx := Instr.Src1;
      LinearIdx := FIntRegs[Instr.Src2];
      FArrays[ArrayIdx].StringData[LinearIdx] := FStringRegs[Instr.Dest];
      {$IFDEF ENABLE_PROFILER}
      if Assigned(FProfiler) and FProfiler.Enabled then
        FProfiler.OnArrayAccess(ArrayIdx, True, LinearIdx);
      {$ENDIF}
    end;

    // Graphics operations
    bcGraphicRGBA:
    begin
      // RGBA(r, g, b, a) - combine 4 int registers into a 32-bit RGBA color
      // Bytecode format: Dest=result, Src1=R, Src2=G, Immediate=(B_reg << 16) | A_reg
      // Result = (A << 24) | (R << 16) | (G << 8) | B
      FIntRegs[Instr.Dest] :=
        ((FIntRegs[Instr.Immediate and $FFFF] and $FF) shl 24) or        // A from low 16 bits of Immediate
        ((FIntRegs[Instr.Src1] and $FF) shl 16) or                        // R
        ((FIntRegs[Instr.Src2] and $FF) shl 8) or                         // G
        (FIntRegs[(Instr.Immediate shr 16) and $FFFF] and $FF);           // B from high 16 bits of Immediate
    end;
    bcGraphicSetMode:
    begin
      // GRAPHIC mode, clear, param3
      // Bytecode: Src1=mode register, Src2=clear register, Immediate=param3 register
      // Mode is in FIntRegs[Src1], Clear is in FIntRegs[Src2], param3 in FIntRegs[Immediate]
      WriteLn('>>> VM bcGraphicSetMode: FOutputDevice=', Assigned(FOutputDevice));
      WriteLn('>>> VM: Src1=', Instr.Src1, ' Src2=', Instr.Src2, ' Immediate=', Instr.Immediate);
      WriteLn('>>> VM: Mode=', FIntRegs[Instr.Src1], ' Clear=', FIntRegs[Instr.Src2]);
      if Assigned(FOutputDevice) then
      begin
        WriteLn('>>> VM: Calling SetGraphicMode...');
        FOutputDevice.SetGraphicMode(
          TGraphicMode(FIntRegs[Instr.Src1] and $F),  // Mode 0-11 (was $7, now $F for extended modes)
          FIntRegs[Instr.Src2] <> 0,                   // Clear flag
          FIntRegs[Instr.Immediate and $FFFF]          // SplitLine/param3
        );
        WriteLn('>>> VM: SetGraphicMode returned');
      end
      else
        WriteLn('>>> VM: FOutputDevice is NIL!');
    end;
    bcGraphicBox:
    begin
      // BOX color, x1, y1, x2, y2, angle, filled, fill_color
      // Bytecode format: Src1=color, Src2=x1, Dest=y1
      // Immediate encodes: x2(bits 0-11), y2(12-23), angle(24-35), filled(36-47), fill_color(48-59)
      if Assigned(FOutputDevice) then
      begin
        // Extract register indices from Immediate field
        // x2 = bits 0-11, y2 = bits 12-23, angle = bits 24-35
        // filled = bits 36-47, fill_color = bits 48-59
        // DrawBoxWithColor interprets color based on graphics mode:
        // - Hires modes (1-2): 0=background, 1=foreground
        // - SDL2 mode (7): direct RGBA value
        WriteLn('>>> BOX: Src1(color reg)=', Instr.Src1, ' Src2(x1 reg)=', Instr.Src2, ' Dest(y1 reg)=', Instr.Dest);
        WriteLn('>>> BOX: x2 reg=', (Instr.Immediate) and $FFF, ' y2 reg=', (Instr.Immediate shr 12) and $FFF);
        WriteLn('>>> BOX: angle reg=', (Instr.Immediate shr 24) and $FFF, ' filled reg=', (Instr.Immediate shr 36) and $FFF);
        WriteLn('>>> BOX VALUES: color=', FIntRegs[Instr.Src1], ' x1=', FIntRegs[Instr.Src2], ' y1=', FIntRegs[Instr.Dest]);
        WriteLn('>>> BOX VALUES: x2=', FIntRegs[(Instr.Immediate) and $FFF], ' y2=', FIntRegs[(Instr.Immediate shr 12) and $FFF]);
        WriteLn('>>> BOX VALUES: filled=', FIntRegs[(Instr.Immediate shr 36) and $FFF]);
        FOutputDevice.DrawBoxWithColor(
          FIntRegs[Instr.Src2],                              // x1
          FIntRegs[Instr.Dest],                              // y1
          FIntRegs[(Instr.Immediate) and $FFF],              // x2
          FIntRegs[(Instr.Immediate shr 12) and $FFF],       // y2
          UInt32(FIntRegs[Instr.Src1]),                      // color (interpreted by output device)
          FFloatRegs[(Instr.Immediate shr 24) and $FFF],     // angle
          FIntRegs[(Instr.Immediate shr 36) and $FFF] <> 0   // filled (non-zero = true)
        );
        // Update display after drawing
        FOutputDevice.Present;
      end;
    end;
    bcGraphicCircle:
    begin
      // CIRCLE color, x, y, xr, yr, sa, ea, angle, inc
      // Bytecode format: Src1=color, Src2=x, Dest=y
      // Immediate encodes: xr(bits 0-9), yr(10-19), sa(20-29), ea(30-39), angle(40-49), inc(50-59)
      if Assigned(FOutputDevice) then
      begin
        FOutputDevice.DrawCircleWithColor(
          FIntRegs[Instr.Src2],                              // x (center)
          FIntRegs[Instr.Dest],                              // y (center)
          FIntRegs[(Instr.Immediate) and $3FF],              // xr (x radius)
          FIntRegs[(Instr.Immediate shr 10) and $3FF],       // yr (y radius)
          UInt32(FIntRegs[Instr.Src1]),                      // color
          FFloatRegs[(Instr.Immediate shr 20) and $3FF],     // sa (start angle)
          FFloatRegs[(Instr.Immediate shr 30) and $3FF],     // ea (end angle)
          FFloatRegs[(Instr.Immediate shr 40) and $3FF],     // angle (rotation)
          FFloatRegs[(Instr.Immediate shr 50) and $3FF]      // inc (increment)
        );
        // Update display after drawing
        FOutputDevice.Present;
      end;
    end;
    bcGraphicDraw:
    begin
      // DRAW color, x, y with mode
      // Bytecode format: Src1=color, Src2=x, Dest=y, Immediate=mode
      // Mode: 0=move PC only, 1=draw line from PC to (x,y), 2=draw dot at (x,y)
      if Assigned(FOutputDevice) then
      begin
        DrawMode := Instr.Immediate and $7FFF;  // Remove flag bit if present
        case DrawMode of
          0: // Move PC only (first point of DRAW, no line drawn)
            FOutputDevice.SetPixelCursor(FIntRegs[Instr.Src2], FIntRegs[Instr.Dest]);
          1: // Draw line from current PC to (x,y)
            begin
              FOutputDevice.DrawLine(
                FOutputDevice.GetPixelCursorX,
                FOutputDevice.GetPixelCursorY,
                FIntRegs[Instr.Src2],
                FIntRegs[Instr.Dest],
                UInt32(FIntRegs[Instr.Src1])
              );
              FOutputDevice.SetPixelCursor(FIntRegs[Instr.Src2], FIntRegs[Instr.Dest]);
            end;
          2: // Draw single dot at (x,y)
            begin
              FOutputDevice.SetPixel(FIntRegs[Instr.Src2], FIntRegs[Instr.Dest], UInt32(FIntRegs[Instr.Src1]));
              FOutputDevice.SetPixelCursor(FIntRegs[Instr.Src2], FIntRegs[Instr.Dest]);
            end;
        end;
        FOutputDevice.Present;
      end;
    end;
    bcGraphicLocate:
    begin
      // LOCATE x, y - set pixel cursor position
      // Bytecode format: Src1=x, Src2=y
      if Assigned(FOutputDevice) then
        FOutputDevice.SetPixelCursor(FIntRegs[Instr.Src1], FIntRegs[Instr.Src2]);
    end;
    bcGraphicRdot:
    begin
      // RDOT(n) - get pixel cursor info
      // Bytecode format: Dest=result, Src1=which (0=x, 1=y, 2=color at PC)
      if Assigned(FOutputDevice) then
      begin
        case FIntRegs[Instr.Src1] of
          0: FIntRegs[Instr.Dest] := FOutputDevice.GetPixelCursorX;
          1: FIntRegs[Instr.Dest] := FOutputDevice.GetPixelCursorY;
          2: FIntRegs[Instr.Dest] := Integer(FOutputDevice.GetPixel(
               FOutputDevice.GetPixelCursorX, FOutputDevice.GetPixelCursorY));
        else
          FIntRegs[Instr.Dest] := 0;
        end;
      end
      else
        FIntRegs[Instr.Dest] := 0;
    end;

    bcGraphicGetMode:
    begin
      // RGR(n) - get graphics info
      // n=0: current graphics mode (0-11)
      // Future expansion: n=1..n for other info
      WriteLn('>>> bcGraphicGetMode: FOutputDevice=', Assigned(FOutputDevice),
              ' Src1=', Instr.Src1, ' Dest=', Instr.Dest);
      if Assigned(FOutputDevice) then
      begin
        WriteLn('>>> bcGraphicGetMode: Src1Value=', FIntRegs[Instr.Src1]);
        case FIntRegs[Instr.Src1] of
          0: begin
               FIntRegs[Instr.Dest] := Ord(FOutputDevice.GetGraphicMode);
               WriteLn('>>> bcGraphicGetMode: Result=', FIntRegs[Instr.Dest]);
             end;
        else
          FIntRegs[Instr.Dest] := 0;
        end;
      end
      else
        FIntRegs[Instr.Dest] := 0;
    end;

    bcEnd, bcStop: FRunning := False;
    bcFast: if Assigned(FOutputDevice) then FOutputDevice.SetFastMode(True);
    bcSlow: if Assigned(FOutputDevice) then FOutputDevice.SetFastMode(False);
    bcSleep:
    begin
      // SLEEP n - delay for n seconds
      // Value can come from:
      // 1. Immediate field (constant value from bytecode compiler)
      // 2. Src1 register (variable/expression result)
      // Clamp to 0..65535 as per C128 spec
      if Instr.Immediate > 0 then
        SleepMs := Instr.Immediate * 1000  // Constant in Immediate
      else if Instr.Src1 < FFloatRegCount then
        SleepMs := Trunc(FFloatRegs[Instr.Src1] * 1000)  // Register value
      else
        SleepMs := 1000; // Default 1 second
      if SleepMs < 0 then SleepMs := 0;
      if SleepMs > 65535000 then SleepMs := 65535000;
      // Sleep in small increments to allow interrupt checking and SDL event processing
      while (SleepMs > 0) and FRunning do
      begin
        if SleepMs > 50 then
        begin
          Sleep(50);
          Dec(SleepMs, 50);
        end
        else
        begin
          Sleep(SleepMs);
          SleepMs := 0;
        end;
        // Process SDL events to keep window responsive
        if Assigned(FInputDevice) then
        begin
          FInputDevice.ProcessEvents;
          // Check for CTRL+C (stop program) or CTRL+ALT+END (quit)
          if FInputDevice.ShouldStop or FInputDevice.ShouldQuit then
          begin
            FRunning := False;
            FInputDevice.ClearStopRequest;
          end;
        end;
      end;
    end;
    bcNop: ;
  end; // case Op (standard bytecode)
end;

procedure TBytecodeVM.ExecuteSuperinstruction(const Instr: TBytecodeInstruction);
begin
  // DEBUG: Validate register indices before access
  if (Instr.OpCode >= 110) and (Instr.OpCode <= 182) then
  begin
    // Check Int register bounds for Int superinstructions
    if Instr.OpCode in [100..105, 120..122, 140..142, 160, 161] then
    begin
      if Instr.Src1 >= FIntRegCount then
        raise Exception.CreateFmt('Superinstr opcode %d: IntReg Src1=%d >= FIntRegCount=%d at PC=%d',
          [Instr.OpCode, Instr.Src1, FIntRegCount, FPC]);
      if (Instr.OpCode in [100..105]) and (Instr.Src2 >= FIntRegCount) then
        raise Exception.CreateFmt('Superinstr opcode %d: IntReg Src2=%d >= FIntRegCount=%d at PC=%d',
          [Instr.OpCode, Instr.Src2, FIntRegCount, FPC]);
      if (Instr.OpCode in [120..122, 140..142]) and (Instr.Dest >= FIntRegCount) then
        raise Exception.CreateFmt('Superinstr opcode %d: IntReg Dest=%d >= FIntRegCount=%d at PC=%d',
          [Instr.OpCode, Instr.Dest, FIntRegCount, FPC]);
    end
    // Check ArrayStoreConst superinstructions (180-182) - Src2 is int index register
    else if Instr.OpCode in [180..182] then
    begin
      if Instr.Src2 >= FIntRegCount then
        raise Exception.CreateFmt('Superinstr opcode %d: IntReg Src2=%d >= FIntRegCount=%d at PC=%d',
          [Instr.OpCode, Instr.Src2, FIntRegCount, FPC]);
    end
    // Check Float register bounds for Float superinstructions
    else if Instr.OpCode in [110..115, 130..133, 150..153, 170, 171] then
    begin
      if Instr.Src1 >= FFloatRegCount then
        raise Exception.CreateFmt('Superinstr opcode %d: FloatReg Src1=%d >= FFloatRegCount=%d at PC=%d',
          [Instr.OpCode, Instr.Src1, FFloatRegCount, FPC]);
      if (Instr.OpCode in [110..115]) and (Instr.Src2 >= FFloatRegCount) then
        raise Exception.CreateFmt('Superinstr opcode %d: FloatReg Src2=%d >= FFloatRegCount=%d at PC=%d',
          [Instr.OpCode, Instr.Src2, FFloatRegCount, FPC]);
      if (Instr.OpCode in [130..133, 150..153]) and (Instr.Dest >= FFloatRegCount) then
        raise Exception.CreateFmt('Superinstr opcode %d: FloatReg Dest=%d >= FFloatRegCount=%d at PC=%d',
          [Instr.OpCode, Instr.Dest, FFloatRegCount, FPC]);
    end;
  end;

  case Instr.OpCode of
    // Fused compare-and-branch (Int) - opcodes 100-105
    100: // bcBranchEqInt: if (r[src1] == r[src2]) goto target
      if FIntRegs[Instr.Src1] = FIntRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;  // -1 because Step will Inc(FPC)
    101: // bcBranchNeInt
      if FIntRegs[Instr.Src1] <> FIntRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    102: // bcBranchLtInt
      if FIntRegs[Instr.Src1] < FIntRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    103: // bcBranchGtInt
      if FIntRegs[Instr.Src1] > FIntRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    104: // bcBranchLeInt
      if FIntRegs[Instr.Src1] <= FIntRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    105: // bcBranchGeInt
      if FIntRegs[Instr.Src1] >= FIntRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;

    // Fused compare-and-branch (Float) - opcodes 110-115
    110: // bcBranchEqFloat
      if FFloatRegs[Instr.Src1] = FFloatRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    111: // bcBranchNeFloat
      if FFloatRegs[Instr.Src1] <> FFloatRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    112: // bcBranchLtFloat
      if FFloatRegs[Instr.Src1] < FFloatRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    113: // bcBranchGtFloat
      if FFloatRegs[Instr.Src1] > FFloatRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    114: // bcBranchLeFloat
      if FFloatRegs[Instr.Src1] <= FFloatRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    115: // bcBranchGeFloat
      if FFloatRegs[Instr.Src1] >= FFloatRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;

    // Fused arithmetic-to-dest (Int) - opcodes 120-122
    120: // bcAddIntTo: r[dest] += r[src1]
      FIntRegs[Instr.Dest] := FIntRegs[Instr.Dest] + FIntRegs[Instr.Src1];
    121: // bcSubIntTo: r[dest] -= r[src1]
      FIntRegs[Instr.Dest] := FIntRegs[Instr.Dest] - FIntRegs[Instr.Src1];
    122: // bcMulIntTo: r[dest] *= r[src1]
      FIntRegs[Instr.Dest] := FIntRegs[Instr.Dest] * FIntRegs[Instr.Src1];

    // Fused arithmetic-to-dest (Float) - opcodes 130-133
    130: // bcAddFloatTo: r[dest] += r[src1]
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Dest] + FFloatRegs[Instr.Src1];
    131: // bcSubFloatTo: r[dest] -= r[src1]
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Dest] - FFloatRegs[Instr.Src1];
    132: // bcMulFloatTo: r[dest] *= r[src1]
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Dest] * FFloatRegs[Instr.Src1];
    133: // bcDivFloatTo: r[dest] /= r[src1]
      if FFloatRegs[Instr.Src1] <> 0.0 then
        FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Dest] / FFloatRegs[Instr.Src1]
      else
        raise Exception.Create('Division by zero');

    // Fused constant arithmetic (Int) - opcodes 140-142
    140: // bcAddIntConst: r[dest] = r[src1] + immediate
      FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] + Instr.Immediate;
    141: // bcSubIntConst: r[dest] = r[src1] - immediate
      FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] - Instr.Immediate;
    142: // bcMulIntConst: r[dest] = r[src1] * immediate
      FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] * Instr.Immediate;

    // Fused constant arithmetic (Float) - opcodes 150-153
    150: // bcAddFloatConst: r[dest] = r[src1] + immediate(as double)
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] + Double(Pointer(@Instr.Immediate)^);
    151: // bcSubFloatConst: r[dest] = r[src1] - immediate(as double)
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] - Double(Pointer(@Instr.Immediate)^);
    152: // bcMulFloatConst: r[dest] = r[src1] * immediate(as double)
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] * Double(Pointer(@Instr.Immediate)^);
    153: // bcDivFloatConst: r[dest] = r[src1] / immediate(as double)
      begin
        if Double(Pointer(@Instr.Immediate)^) <> 0.0 then
          FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] / Double(Pointer(@Instr.Immediate)^)
        else
          raise Exception.Create('Division by zero');
      end;

    // Fused compare-zero-and-branch (Int) - opcodes 160-161
    160: // bcBranchEqZeroInt: if (r[src1] == 0) goto target
      if FIntRegs[Instr.Src1] = 0 then
        FPC := Instr.Immediate - 1;
    161: // bcBranchNeZeroInt: if (r[src1] != 0) goto target
      if FIntRegs[Instr.Src1] <> 0 then
        FPC := Instr.Immediate - 1;

    // Fused compare-zero-and-branch (Float) - opcodes 170-171
    170: // bcBranchEqZeroFloat: if (r[src1] == 0.0) goto target
      if FFloatRegs[Instr.Src1] = 0.0 then
        FPC := Instr.Immediate - 1;
    171: // bcBranchNeZeroFloat: if (r[src1] != 0.0) goto target
      if FFloatRegs[Instr.Src1] <> 0.0 then
        FPC := Instr.Immediate - 1;

    // Fused array-store-constant - opcodes 180-182
    // Format: Src1=ArrayIndex, Src2=IndexReg, Immediate=ConstValue
    180: // bcArrayStoreIntConst: ARR[idx] = immediate (int)
      FArrays[Instr.Src1].IntData[FIntRegs[Instr.Src2]] := Instr.Immediate;
    181: // bcArrayStoreFloatConst: ARR[idx] = immediate (as double)
      FArrays[Instr.Src1].FloatData[FIntRegs[Instr.Src2]] := Double(Pointer(@Instr.Immediate)^);
    182: // bcArrayStoreStringConst: ARR[idx] = string constant index
      FArrays[Instr.Src1].StringData[FIntRegs[Instr.Src2]] := FProgram.StringConstants[Instr.Immediate];

    // Fused loop increment-and-branch (Int) - opcodes 190-193
    // Format: Dest=counter, Src1=step, Src2=limit, Immediate=loopTarget
    190: // bcAddIntToBranchLe: r[dest] += r[src1]; if (r[dest] <= r[src2]) goto target
      begin
        Inc(FIntRegs[Instr.Dest], FIntRegs[Instr.Src1]);
        if FIntRegs[Instr.Dest] <= FIntRegs[Instr.Src2] then
          FPC := Instr.Immediate - 1;
      end;
    191: // bcAddIntToBranchLt: r[dest] += r[src1]; if (r[dest] < r[src2]) goto target
      begin
        Inc(FIntRegs[Instr.Dest], FIntRegs[Instr.Src1]);
        if FIntRegs[Instr.Dest] < FIntRegs[Instr.Src2] then
          FPC := Instr.Immediate - 1;
      end;
    192: // bcSubIntToBranchGe: r[dest] -= r[src1]; if (r[dest] >= r[src2]) goto target
      begin
        Dec(FIntRegs[Instr.Dest], FIntRegs[Instr.Src1]);
        if FIntRegs[Instr.Dest] >= FIntRegs[Instr.Src2] then
          FPC := Instr.Immediate - 1;
      end;
    193: // bcSubIntToBranchGt: r[dest] -= r[src1]; if (r[dest] > r[src2]) goto target
      begin
        Dec(FIntRegs[Instr.Dest], FIntRegs[Instr.Src1]);
        if FIntRegs[Instr.Dest] > FIntRegs[Instr.Src2] then
          FPC := Instr.Immediate - 1;
      end;

    // NEW: FMA (Fused Multiply-Add) - opcodes 200-203
    200: // bcMulAddFloat: dest = c + a*b
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Immediate] + FFloatRegs[Instr.Src1] * FFloatRegs[Instr.Src2];
    201: // bcMulSubFloat: dest = c - a*b
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Immediate] - FFloatRegs[Instr.Src1] * FFloatRegs[Instr.Src2];
    202: // bcMulAddToFloat: dest += a*b
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Dest] + FFloatRegs[Instr.Src1] * FFloatRegs[Instr.Src2];
    203: // bcMulSubToFloat: dest -= a*b
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Dest] - FFloatRegs[Instr.Src1] * FFloatRegs[Instr.Src2];

    // NEW: Array Load + Arithmetic - opcodes 210-212
    210: // bcArrayLoadAddFloat: dest = acc + arr[idx]
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Immediate] + FArrays[Instr.Src1].FloatData[FIntRegs[Instr.Src2]];
    211: // bcArrayLoadSubFloat: dest = acc - arr[idx]
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Immediate] - FArrays[Instr.Src1].FloatData[FIntRegs[Instr.Src2]];
    212: // bcArrayLoadDivAddFloat: dest = acc + arr[idx] / denom
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Immediate and $FFFF] +
        FArrays[Instr.Src1].FloatData[FIntRegs[Instr.Src2]] / FFloatRegs[(Instr.Immediate shr 16) and $FFFF];

    // NEW: Square-Sum patterns - opcodes 220-221
    220: // bcSquareSumFloat: dest = x*x + y*y
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] * FFloatRegs[Instr.Src1] +
                                FFloatRegs[Instr.Src2] * FFloatRegs[Instr.Src2];
    221: // bcAddSquareFloat: dest = sum + x*x
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] + FFloatRegs[Instr.Src2] * FFloatRegs[Instr.Src2];

    // NEW: Mul-Mul and Add-Sqrt - opcodes 230-231
    230: // bcMulMulFloat: dest = a*b*c
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] * FFloatRegs[Instr.Src2] * FFloatRegs[Instr.Immediate];
    231: // bcAddSqrtFloat: dest = sqrt(a+b)
      FFloatRegs[Instr.Dest] := Sqrt(FFloatRegs[Instr.Src1] + FFloatRegs[Instr.Src2]);

    // NEW: Array Load + Branch - opcodes 240-241
    240: // bcArrayLoadIntBranchNZ: if arr[idx] <> 0 goto target
      if FArrays[Instr.Src1].IntData[FIntRegs[Instr.Src2]] <> 0 then
        FPC := Instr.Immediate - 1;
    241: // bcArrayLoadIntBranchZ: if arr[idx] = 0 goto target
      if FArrays[Instr.Src1].IntData[FIntRegs[Instr.Src2]] = 0 then
        FPC := Instr.Immediate - 1;

    // NEW: Array Swap (Int) - opcode 250
    // Format: Src1=arr_index, Src2=idx1_reg, Dest=idx2_reg
    250: // bcArraySwapInt: swap arr[idx1] and arr[idx2]
      begin
        // Inline swap using FIntRegs indices directly
        FSwapTempInt := FArrays[Instr.Src1].IntData[FIntRegs[Instr.Src2]];
        FArrays[Instr.Src1].IntData[FIntRegs[Instr.Src2]] := FArrays[Instr.Src1].IntData[FIntRegs[Instr.Dest]];
        FArrays[Instr.Src1].IntData[FIntRegs[Instr.Dest]] := FSwapTempInt;
      end;

    // NEW: Self-increment/decrement (Int) - opcodes 251-252
    // Format: Dest=register to modify, Src1=amount register
    251: // bcAddIntSelf: r[dest] += r[src1]
      Inc(FIntRegs[Instr.Dest], FIntRegs[Instr.Src1]);
    252: // bcSubIntSelf: r[dest] -= r[src1]
      Dec(FIntRegs[Instr.Dest], FIntRegs[Instr.Src1]);

    // NEW: Array Load to register (Int) - opcode 253
    // Format: Dest=final destination, Src1=arr_index, Src2=idx_reg
    253: // bcArrayLoadIntTo: r[dest] = arr[src1][r[src2]]
      FIntRegs[Instr.Dest] := FArrays[Instr.Src1].IntData[FIntRegs[Instr.Src2]];

    // NEW: Array Copy Element - opcode 254
    // Format: Dest=dest_arr_index, Src1=src_arr_index, Src2=idx_reg
    254: // bcArrayCopyElement: arr_dest[idx] = arr_src[idx]
      FArrays[Instr.Dest].IntData[FIntRegs[Instr.Src2]] := FArrays[Instr.Src1].IntData[FIntRegs[Instr.Src2]];

    // NEW: Array Move Element - opcode 255
    // Format: Dest=arr_index, Src1=src_idx_reg, Src2=dest_idx_reg
    255: // bcArrayMoveElement: arr[dest_idx] = arr[src_idx]
      FArrays[Instr.Dest].IntData[FIntRegs[Instr.Src2]] := FArrays[Instr.Dest].IntData[FIntRegs[Instr.Src1]];

    // NEW: Array Reverse Range - opcode 156
    // Format: Src1=arr_index, Src2=start_idx_reg, Dest=end_idx_reg
    156: // bcArrayReverseRange: reverse arr[start..end-1] in-place
      begin
        FStartIdx := FIntRegs[Instr.Src2];
        FEndIdx := FIntRegs[Instr.Dest] - 1;
        FArrIdxTmp := Instr.Src1;
        while FStartIdx < FEndIdx do
        begin
          FSwapTempInt := FArrays[FArrIdxTmp].IntData[FStartIdx];
          FArrays[FArrIdxTmp].IntData[FStartIdx] := FArrays[FArrIdxTmp].IntData[FEndIdx];
          FArrays[FArrIdxTmp].IntData[FEndIdx] := FSwapTempInt;
          Inc(FStartIdx);
          Dec(FEndIdx);
        end;
      end;

    // NEW: Array Shift Left - opcode 157
    // Format: Src1=arr_index, Src2=start_idx_reg, Dest=end_idx_reg
    157: // bcArrayShiftLeft: shift left and rotate first to end+1
      begin
        FStartIdx := FIntRegs[Instr.Src2];
        FEndIdx := FIntRegs[Instr.Dest];
        FArrIdxTmp := Instr.Src1;
        FFirstVal := FArrays[FArrIdxTmp].IntData[FStartIdx];
        FLoopIdx := FStartIdx;
        while FLoopIdx <= FEndIdx do
        begin
          FArrays[FArrIdxTmp].IntData[FLoopIdx] := FArrays[FArrIdxTmp].IntData[FLoopIdx + 1];
          Inc(FLoopIdx);
        end;
        FArrays[FArrIdxTmp].IntData[FEndIdx + 1] := FFirstVal;
      end;
  else
    raise Exception.CreateFmt('Unknown superinstruction opcode %d at PC=%d', [Instr.OpCode, FPC]);
  end; // case Instr.OpCode (superinstructions)
end;

procedure TBytecodeVM.Step;
var Instr: TBytecodeInstruction;
begin
  if (FProgram = nil) or (FPC >= FProgram.GetInstructionCount) then
  begin
    FRunning := False;
    Exit;
  end;
  Instr := FProgram.GetInstruction(FPC);

  {$IFDEF ENABLE_PROFILER}
  // Profiler: BeforeInstruction hook
  if Assigned(FProfiler) and FProfiler.Enabled then
    FProfiler.BeforeInstruction(FPC, Instr.OpCode);
  {$ENDIF}

  ExecuteInstruction(Instr);

  {$IFDEF ENABLE_PROFILER}
  // Profiler: AfterInstruction hook
  if Assigned(FProfiler) and FProfiler.Enabled then
  begin
    FProfiler.AfterInstruction(FPC, Instr.OpCode);
    // Track superinstructions
    if Instr.OpCode >= 110 then
      FProfiler.OnSuperinstruction(Instr.OpCode, 1);
  end;
  {$ENDIF}

  {$IFDEF ENABLE_INSTRUCTION_COUNTING}
  Inc(FInstructionsExecuted);
  {$ENDIF}
  Inc(FPC);
end;

procedure TBytecodeVM.Run;
begin
  if FProgram = nil then raise Exception.Create('No program loaded');
  FRunning := True;
  FPC := FProgram.EntryPoint;

  {$IFDEF ENABLE_PROFILER}
  // Start profiler if attached
  if Assigned(FProfiler) then
    FProfiler.Start;
  {$ENDIF}

  while FRunning and (FPC < FProgram.GetInstructionCount) do Step;

  {$IFDEF ENABLE_PROFILER}
  // Stop profiler
  if Assigned(FProfiler) then
    FProfiler.Stop;
  {$ENDIF}

  // Reset FAST mode when program ends (ensure screen is visible)
  if Assigned(FOutputDevice) then
    FOutputDevice.SetFastMode(False);
end;

{ RunFast - Optimized execution loop
  - Direct pointer access to instruction array (no method calls)
  - Inline dispatch (no procedure calls for each instruction)
  - Range checking disabled in critical path
  - No profiler support for maximum speed }
procedure TBytecodeVM.RunFast;
type
  PBytecodeInstruction = ^TBytecodeInstruction;
var
  Instructions: PBytecodeInstruction;
  InstrCount: Integer;
  CurPC: Integer;  // Current Program Counter (local for speed)
  Instr: TBytecodeInstruction;
  Op: Byte;
  // Local copies of frequently accessed fields for better register allocation
  IntRegs: PInt64;
  FloatRegs: PDouble;
  ArrayIdx, LinearIdx: Integer;
  InputStr, PrintStr: string;
  InputVal: Double;
  i, ProdDims: Integer;
  NextTabCol, TabIdx: Integer;
  ArrInfo: TSSAArrayInfo;
  // Local vars for ArrayReverseRange and ArrayShiftLeft
  ArrIdxW: Word;
  LStartIdx, LEndIdx, LLoopIdx: Integer;
  LFirstVal: Int64;
begin
  if FProgram = nil then raise Exception.Create('No program loaded');

  Instructions := PBytecodeInstruction(FProgram.GetInstructionsPtr);
  if Instructions = nil then raise Exception.Create('Empty program');

  InstrCount := FProgram.GetInstructionCount;
  CurPC := FProgram.EntryPoint;

  // Get direct pointers to register arrays
  IntRegs := @FIntRegs[0];
  FloatRegs := @FFloatRegs[0];

  // Main execution loop - disable range checking for speed
  {$PUSH}
  {$R-}  // Range checking off
  {$Q-}  // Overflow checking off

  while CurPC < InstrCount do
  begin
    Instr := Instructions[CurPC];
    Op := Instr.OpCode;

    {$IFDEF ENABLE_INSTRUCTION_COUNTING}
    Inc(FInstructionsExecuted);
    {$ENDIF}

    // Fast path: check for superinstructions first (most common in optimized code)
    if Op >= 110 then
    begin
      case Op of
        // Fused loop increment-and-branch (most common in hot loops)
        190: // AddIntToBranchLe
          begin
            Inc(IntRegs[Instr.Dest], IntRegs[Instr.Src1]);
            if IntRegs[Instr.Dest] <= IntRegs[Instr.Src2] then
              CurPC := Instr.Immediate
            else
              Inc(CurPC);
          end;
        191: // AddIntToBranchLt
          begin
            Inc(IntRegs[Instr.Dest], IntRegs[Instr.Src1]);
            if IntRegs[Instr.Dest] < IntRegs[Instr.Src2] then
              CurPC := Instr.Immediate
            else
              Inc(CurPC);
          end;
        192: // SubIntToBranchGe
          begin
            Dec(IntRegs[Instr.Dest], IntRegs[Instr.Src1]);
            if IntRegs[Instr.Dest] >= IntRegs[Instr.Src2] then
              CurPC := Instr.Immediate
            else
              Inc(CurPC);
          end;
        193: // SubIntToBranchGt
          begin
            Dec(IntRegs[Instr.Dest], IntRegs[Instr.Src1]);
            if IntRegs[Instr.Dest] > IntRegs[Instr.Src2] then
              CurPC := Instr.Immediate
            else
              Inc(CurPC);
          end;

        // Fused compare-and-branch (Int)
        100: if IntRegs[Instr.Src1] = IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);
        101: if IntRegs[Instr.Src1] <> IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);
        102: if IntRegs[Instr.Src1] < IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);
        103: if IntRegs[Instr.Src1] > IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);
        104: if IntRegs[Instr.Src1] <= IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);
        105: if IntRegs[Instr.Src1] >= IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);

        // Fused compare-and-branch (Float)
        110: if FloatRegs[Instr.Src1] = FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);
        111: if FloatRegs[Instr.Src1] <> FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);
        112: if FloatRegs[Instr.Src1] < FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);
        113: if FloatRegs[Instr.Src1] > FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);
        114: if FloatRegs[Instr.Src1] <= FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);
        115: if FloatRegs[Instr.Src1] >= FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC);

        // Fused arithmetic-to-dest (Int)
        120: begin IntRegs[Instr.Dest] := IntRegs[Instr.Dest] + IntRegs[Instr.Src1]; Inc(CurPC); end;
        121: begin IntRegs[Instr.Dest] := IntRegs[Instr.Dest] - IntRegs[Instr.Src1]; Inc(CurPC); end;
        122: begin IntRegs[Instr.Dest] := IntRegs[Instr.Dest] * IntRegs[Instr.Src1]; Inc(CurPC); end;

        // Fused arithmetic-to-dest (Float)
        130: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] + FloatRegs[Instr.Src1]; Inc(CurPC); end;
        131: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] - FloatRegs[Instr.Src1]; Inc(CurPC); end;
        132: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] * FloatRegs[Instr.Src1]; Inc(CurPC); end;
        133: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] / FloatRegs[Instr.Src1]; Inc(CurPC); end;

        // Fused constant arithmetic (Int)
        140: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1] + Instr.Immediate; Inc(CurPC); end;
        141: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1] - Instr.Immediate; Inc(CurPC); end;
        142: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1] * Instr.Immediate; Inc(CurPC); end;

        // Fused constant arithmetic (Float)
        150: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] + Double(Pointer(@Instr.Immediate)^); Inc(CurPC); end;
        151: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] - Double(Pointer(@Instr.Immediate)^); Inc(CurPC); end;
        152: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] * Double(Pointer(@Instr.Immediate)^); Inc(CurPC); end;
        153: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] / Double(Pointer(@Instr.Immediate)^); Inc(CurPC); end;

        // Fused compare-zero-and-branch (Int)
        160: if IntRegs[Instr.Src1] = 0 then CurPC := Instr.Immediate else Inc(CurPC);
        161: if IntRegs[Instr.Src1] <> 0 then CurPC := Instr.Immediate else Inc(CurPC);

        // Fused compare-zero-and-branch (Float)
        170: if FloatRegs[Instr.Src1] = 0.0 then CurPC := Instr.Immediate else Inc(CurPC);
        171: if FloatRegs[Instr.Src1] <> 0.0 then CurPC := Instr.Immediate else Inc(CurPC);

        // Fused array-store-constant
        180: begin FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] := Instr.Immediate; Inc(CurPC); end;
        181: begin FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]] := Double(Pointer(@Instr.Immediate)^); Inc(CurPC); end;
        182: begin FArrays[Instr.Src1].StringData[IntRegs[Instr.Src2]] := FProgram.StringConstants[Instr.Immediate]; Inc(CurPC); end;

        // NEW: FMA (Fused Multiply-Add) - opcodes 200-203
        // Format: Dest, Src1=a, Src2=b, Immediate=c (extra register)
        200: begin // bcMulAddFloat: dest = c + a*b
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate] + FloatRegs[Instr.Src1] * FloatRegs[Instr.Src2];
          Inc(CurPC);
        end;
        201: begin // bcMulSubFloat: dest = c - a*b
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate] - FloatRegs[Instr.Src1] * FloatRegs[Instr.Src2];
          Inc(CurPC);
        end;
        202: begin // bcMulAddToFloat: dest += a*b
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] + FloatRegs[Instr.Src1] * FloatRegs[Instr.Src2];
          Inc(CurPC);
        end;
        203: begin // bcMulSubToFloat: dest -= a*b
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] - FloatRegs[Instr.Src1] * FloatRegs[Instr.Src2];
          Inc(CurPC);
        end;

        // NEW: Array Load + Arithmetic - opcodes 210-212
        // Format: Dest, Src1=arr_index, Src2=idx_reg, Immediate=acc_reg
        210: begin // bcArrayLoadAddFloat: dest = acc + arr[idx]
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate] + FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]];
          Inc(CurPC);
        end;
        211: begin // bcArrayLoadSubFloat: dest = acc - arr[idx]
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate] - FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]];
          Inc(CurPC);
        end;
        212: begin // bcArrayLoadDivAddFloat: dest = acc + arr[idx] / denom
          // Extra encodes: acc_reg (low 16) + denom_reg (high 16)
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Immediate and $FFFF] +
            FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]] / FloatRegs[(Instr.Immediate shr 16) and $FFFF];
          Inc(CurPC);
        end;

        // NEW: Square-Sum patterns - opcodes 220-221
        220: begin // bcSquareSumFloat: dest = x*x + y*y
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] * FloatRegs[Instr.Src1] +
                                   FloatRegs[Instr.Src2] * FloatRegs[Instr.Src2];
          Inc(CurPC);
        end;
        221: begin // bcAddSquareFloat: dest = sum + x*x
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] + FloatRegs[Instr.Src2] * FloatRegs[Instr.Src2];
          Inc(CurPC);
        end;

        // NEW: Mul-Mul and Add-Sqrt - opcodes 230-231
        230: begin // bcMulMulFloat: dest = a*b*c
          FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] * FloatRegs[Instr.Src2] * FloatRegs[Instr.Immediate];
          Inc(CurPC);
        end;
        231: begin // bcAddSqrtFloat: dest = sqrt(a+b)
          FloatRegs[Instr.Dest] := Sqrt(FloatRegs[Instr.Src1] + FloatRegs[Instr.Src2]);
          Inc(CurPC);
        end;

        // NEW: Array Load + Branch - opcodes 240-241
        240: // bcArrayLoadIntBranchNZ: if arr[idx] <> 0 goto target
          if FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] <> 0 then
            CurPC := Instr.Immediate
          else
            Inc(CurPC);
        241: // bcArrayLoadIntBranchZ: if arr[idx] = 0 goto target
          if FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] = 0 then
            CurPC := Instr.Immediate
          else
            Inc(CurPC);

        // NEW: Array Swap (Int) - opcode 250
        // Format: Src1=arr_index, Src2=idx1_reg, Dest=idx2_reg
        250: // bcArraySwapInt: swap arr[idx1] and arr[idx2]
          begin
            // Use class field FSwapTempInt as temp variable
            FSwapTempInt := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]];
            FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Dest]];
            FArrays[Instr.Src1].IntData[IntRegs[Instr.Dest]] := FSwapTempInt;
            Inc(CurPC);
          end;

        // NEW: Self-increment/decrement (Int) - opcodes 251-252
        // Format: Dest=register to modify, Src1=amount register
        251: begin Inc(IntRegs[Instr.Dest], IntRegs[Instr.Src1]); Inc(CurPC); end; // bcAddIntSelf
        252: begin Dec(IntRegs[Instr.Dest], IntRegs[Instr.Src1]); Inc(CurPC); end; // bcSubIntSelf

        // NEW: Array Load to register (Int) - opcode 253
        253: begin IntRegs[Instr.Dest] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]]; Inc(CurPC); end; // bcArrayLoadIntTo

        // NEW: Array Copy Element - opcode 254
        254: begin FArrays[Instr.Dest].IntData[IntRegs[Instr.Src2]] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]]; Inc(CurPC); end;

        // NEW: Array Move Element - opcode 255
        255: begin FArrays[Instr.Dest].IntData[IntRegs[Instr.Src2]] := FArrays[Instr.Dest].IntData[IntRegs[Instr.Src1]]; Inc(CurPC); end;

        // NEW: Array Reverse Range - opcode 156
        156: // bcArrayReverseRange
          begin
            LStartIdx := IntRegs[Instr.Src2];
            LEndIdx := IntRegs[Instr.Dest] - 1;
            ArrIdxW := Instr.Src1;
            while LStartIdx < LEndIdx do
            begin
              FSwapTempInt := FArrays[ArrIdxW].IntData[LStartIdx];
              FArrays[ArrIdxW].IntData[LStartIdx] := FArrays[ArrIdxW].IntData[LEndIdx];
              FArrays[ArrIdxW].IntData[LEndIdx] := FSwapTempInt;
              Inc(LStartIdx);
              Dec(LEndIdx);
            end;
            Inc(CurPC);
          end;

        // NEW: Array Shift Left - opcode 157
        157: // bcArrayShiftLeft
          begin
            LStartIdx := IntRegs[Instr.Src2];
            LEndIdx := IntRegs[Instr.Dest];
            ArrIdxW := Instr.Src1;
            LFirstVal := FArrays[ArrIdxW].IntData[LStartIdx];
            LLoopIdx := LStartIdx;
            while LLoopIdx <= LEndIdx do
            begin
              FArrays[ArrIdxW].IntData[LLoopIdx] := FArrays[ArrIdxW].IntData[LLoopIdx + 1];
              Inc(LLoopIdx);
            end;
            FArrays[ArrIdxW].IntData[LEndIdx + 1] := LFirstVal;
            Inc(CurPC);
          end;
      else
        raise Exception.CreateFmt('Unknown superinstruction opcode %d at CurPC=%d', [Op, CurPC]);
      end;
    end
    else
    begin
      // Standard bytecode opcodes
      case TBytecodeOp(Op) of
        // Load constants - very common
        bcLoadConstInt: begin IntRegs[Instr.Dest] := Instr.Immediate; Inc(CurPC); end;
        bcLoadConstFloat: begin FloatRegs[Instr.Dest] := Double(Pointer(@Instr.Immediate)^); Inc(CurPC); end;
        bcLoadConstString: begin FStringRegs[Instr.Dest] := FProgram.StringConstants[Instr.Immediate]; Inc(CurPC); end;

        // Copy operations
        bcCopyInt: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1]; Inc(CurPC); end;
        bcCopyFloat: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1]; Inc(CurPC); end;
        bcCopyString: begin FStringRegs[Instr.Dest] := FStringRegs[Instr.Src1]; Inc(CurPC); end;

        // Integer arithmetic
        bcAddInt: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1] + IntRegs[Instr.Src2]; Inc(CurPC); end;
        bcSubInt: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1] - IntRegs[Instr.Src2]; Inc(CurPC); end;
        bcMulInt: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1] * IntRegs[Instr.Src2]; Inc(CurPC); end;
        bcDivInt: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1] div IntRegs[Instr.Src2]; Inc(CurPC); end;
        bcModInt: begin IntRegs[Instr.Dest] := IntRegs[Instr.Src1] mod IntRegs[Instr.Src2]; Inc(CurPC); end;
        bcNegInt: begin IntRegs[Instr.Dest] := -IntRegs[Instr.Src1]; Inc(CurPC); end;

        // Float arithmetic - critical for spectral-norm
        bcAddFloat: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] + FloatRegs[Instr.Src2]; Inc(CurPC); end;
        bcSubFloat: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] - FloatRegs[Instr.Src2]; Inc(CurPC); end;
        bcMulFloat: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] * FloatRegs[Instr.Src2]; Inc(CurPC); end;
        bcDivFloat: begin FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] / FloatRegs[Instr.Src2]; Inc(CurPC); end;
        bcPowFloat: begin FloatRegs[Instr.Dest] := Power(FloatRegs[Instr.Src1], FloatRegs[Instr.Src2]); Inc(CurPC); end;
        bcNegFloat: begin FloatRegs[Instr.Dest] := -FloatRegs[Instr.Src1]; Inc(CurPC); end;

        // Type conversions
        bcIntToFloat: begin FloatRegs[Instr.Dest] := IntRegs[Instr.Src1]; Inc(CurPC); end;
        bcFloatToInt: begin IntRegs[Instr.Dest] := Trunc(FloatRegs[Instr.Src1]); Inc(CurPC); end;

        // Integer comparisons
        bcCmpEqInt: begin IntRegs[Instr.Dest] := Ord(IntRegs[Instr.Src1] = IntRegs[Instr.Src2]); Inc(CurPC); end;
        bcCmpNeInt: begin IntRegs[Instr.Dest] := Ord(IntRegs[Instr.Src1] <> IntRegs[Instr.Src2]); Inc(CurPC); end;
        bcCmpLtInt: begin IntRegs[Instr.Dest] := Ord(IntRegs[Instr.Src1] < IntRegs[Instr.Src2]); Inc(CurPC); end;
        bcCmpGtInt: begin IntRegs[Instr.Dest] := Ord(IntRegs[Instr.Src1] > IntRegs[Instr.Src2]); Inc(CurPC); end;
        bcCmpLeInt: begin IntRegs[Instr.Dest] := Ord(IntRegs[Instr.Src1] <= IntRegs[Instr.Src2]); Inc(CurPC); end;
        bcCmpGeInt: begin IntRegs[Instr.Dest] := Ord(IntRegs[Instr.Src1] >= IntRegs[Instr.Src2]); Inc(CurPC); end;

        // Float comparisons
        bcCmpEqFloat: begin IntRegs[Instr.Dest] := Ord(FloatRegs[Instr.Src1] = FloatRegs[Instr.Src2]); Inc(CurPC); end;
        bcCmpNeFloat: begin IntRegs[Instr.Dest] := Ord(FloatRegs[Instr.Src1] <> FloatRegs[Instr.Src2]); Inc(CurPC); end;
        bcCmpLtFloat: begin IntRegs[Instr.Dest] := Ord(FloatRegs[Instr.Src1] < FloatRegs[Instr.Src2]); Inc(CurPC); end;
        bcCmpGtFloat: begin IntRegs[Instr.Dest] := Ord(FloatRegs[Instr.Src1] > FloatRegs[Instr.Src2]); Inc(CurPC); end;
        bcCmpLeFloat: begin IntRegs[Instr.Dest] := Ord(FloatRegs[Instr.Src1] <= FloatRegs[Instr.Src2]); Inc(CurPC); end;
        bcCmpGeFloat: begin IntRegs[Instr.Dest] := Ord(FloatRegs[Instr.Src1] >= FloatRegs[Instr.Src2]); Inc(CurPC); end;

        // String comparisons
        bcCmpEqString: begin IntRegs[Instr.Dest] := Ord(FStringRegs[Instr.Src1] = FStringRegs[Instr.Src2]); Inc(CurPC); end;
        bcCmpNeString: begin IntRegs[Instr.Dest] := Ord(FStringRegs[Instr.Src1] <> FStringRegs[Instr.Src2]); Inc(CurPC); end;
        bcCmpLtString: begin IntRegs[Instr.Dest] := Ord(FStringRegs[Instr.Src1] < FStringRegs[Instr.Src2]); Inc(CurPC); end;
        bcCmpGtString: begin IntRegs[Instr.Dest] := Ord(FStringRegs[Instr.Src1] > FStringRegs[Instr.Src2]); Inc(CurPC); end;

        // Math functions
        bcMathAbs: begin FloatRegs[Instr.Dest] := Abs(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathSgn: begin
          if FloatRegs[Instr.Src1] > 0 then FloatRegs[Instr.Dest] := 1
          else if FloatRegs[Instr.Src1] < 0 then FloatRegs[Instr.Dest] := -1
          else FloatRegs[Instr.Dest] := 0;
          Inc(CurPC);
        end;
        bcMathInt: begin FloatRegs[Instr.Dest] := Floor(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathSqr: begin FloatRegs[Instr.Dest] := Sqrt(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathSin: begin FloatRegs[Instr.Dest] := Sin(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathCos: begin FloatRegs[Instr.Dest] := Cos(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathTan: begin FloatRegs[Instr.Dest] := Tan(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathExp: begin FloatRegs[Instr.Dest] := Exp(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathLog: begin FloatRegs[Instr.Dest] := Ln(FloatRegs[Instr.Src1]); Inc(CurPC); end;
        bcMathRnd: begin FloatRegs[Instr.Dest] := Random; Inc(CurPC); end;
        bcMathAtn: begin FloatRegs[Instr.Dest] := ArcTan(FloatRegs[Instr.Src1]); Inc(CurPC); end;

        // Control flow
        bcJump: CurPC := Instr.Immediate;
        bcJumpIfZero: if IntRegs[Instr.Src1] = 0 then CurPC := Instr.Immediate else Inc(CurPC);
        bcJumpIfNotZero: if IntRegs[Instr.Src1] <> 0 then CurPC := Instr.Immediate else Inc(CurPC);
        bcCall:
          begin
            FCallStack[FCallStackPtr] := CurPC + 1;
            Inc(FCallStackPtr);
            CurPC := Instr.Immediate;
          end;
        bcReturn:
          begin
            Dec(FCallStackPtr);
            CurPC := FCallStack[FCallStackPtr];
          end;

        // Array operations - typed versions are faster
        bcArrayLoadInt:
          begin
            FIntRegs[Instr.Dest] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]];
            Inc(CurPC);
          end;
        bcArrayLoadFloat:
          begin
            FFloatRegs[Instr.Dest] := FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]];
            Inc(CurPC);
          end;
        bcArrayLoadString:
          begin
            FStringRegs[Instr.Dest] := FArrays[Instr.Src1].StringData[IntRegs[Instr.Src2]];
            Inc(CurPC);
          end;
        bcArrayStoreInt:
          begin
            FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] := IntRegs[Instr.Dest];
            Inc(CurPC);
          end;
        bcArrayStoreFloat:
          begin
            FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]] := FloatRegs[Instr.Dest];
            Inc(CurPC);
          end;
        bcArrayStoreString:
          begin
            FArrays[Instr.Src1].StringData[IntRegs[Instr.Src2]] := FStringRegs[Instr.Dest];
            Inc(CurPC);
          end;

        // Array dimension (slow path - only at initialization)
        bcArrayDim:
          begin
            ArrayIdx := Instr.Src1;
            ArrInfo := FProgram.GetArray(ArrayIdx);
            if ArrayIdx >= Length(FArrays) then
              SetLength(FArrays, ArrayIdx + 1);
            FArrays[ArrayIdx].ElementType := Byte(ArrInfo.ElementType);
            FArrays[ArrayIdx].DimCount := ArrInfo.DimCount;
            SetLength(FArrays[ArrayIdx].Dimensions, ArrInfo.DimCount);
            for i := 0 to ArrInfo.DimCount - 1 do
            begin
              if ArrInfo.Dimensions[i] = 0 then
              begin
                case ArrInfo.DimRegTypes[i] of
                  srtInt: FArrays[ArrayIdx].Dimensions[i] := IntRegs[ArrInfo.DimRegisters[i]] + 1;
                  srtFloat: FArrays[ArrayIdx].Dimensions[i] := Trunc(FloatRegs[ArrInfo.DimRegisters[i]]) + 1;
                end;
              end
              else
                FArrays[ArrayIdx].Dimensions[i] := ArrInfo.Dimensions[i];
            end;
            ProdDims := 1;
            for i := 0 to ArrInfo.DimCount - 1 do
              ProdDims := ProdDims * FArrays[ArrayIdx].Dimensions[i];
            FArrays[ArrayIdx].TotalSize := ProdDims;
            case ArrInfo.ElementType of
              srtInt: SetLength(FArrays[ArrayIdx].IntData, ProdDims);
              srtFloat: SetLength(FArrays[ArrayIdx].FloatData, ProdDims);
              srtString: SetLength(FArrays[ArrayIdx].StringData, ProdDims);
            end;
            Inc(CurPC);
          end;

        // Generic array load/store (slower - legacy)
        bcArrayLoad:
          begin
            ArrayIdx := Instr.Src1;
            LinearIdx := IntRegs[Instr.Src2];
            case FArrays[ArrayIdx].ElementType of
              0: FIntRegs[Instr.Dest] := FArrays[ArrayIdx].IntData[LinearIdx];
              1: FFloatRegs[Instr.Dest] := FArrays[ArrayIdx].FloatData[LinearIdx];
              2: FStringRegs[Instr.Dest] := FArrays[ArrayIdx].StringData[LinearIdx];
            end;
            Inc(CurPC);
          end;
        bcArrayStore:
          begin
            ArrayIdx := Instr.Src1;
            LinearIdx := IntRegs[Instr.Src2];
            case FArrays[ArrayIdx].ElementType of
              0: FArrays[ArrayIdx].IntData[LinearIdx] := FIntRegs[Instr.Dest];
              1: FArrays[ArrayIdx].FloatData[LinearIdx] := FFloatRegs[Instr.Dest];
              2: FArrays[ArrayIdx].StringData[LinearIdx] := FStringRegs[Instr.Dest];
            end;
            Inc(CurPC);
          end;

        // I/O operations (slow path - not in hot loops)
        bcPrint:
          begin
            if Assigned(FOutputDevice) then
            begin
              PrintStr := FConsoleBehavior.FormatNumber(FloatRegs[Instr.Src1]);
              FOutputDevice.Print(PrintStr);
              Inc(FCursorCol, Length(PrintStr));
            end;
            Inc(CurPC);
          end;
        bcPrintLn:
          begin
            if Assigned(FOutputDevice) then
            begin
              PrintStr := FConsoleBehavior.FormatNumber(FloatRegs[Instr.Src1]);
              FOutputDevice.Print(PrintStr);
              FOutputDevice.NewLine;
              FCursorCol := 0;
            end;
            Inc(CurPC);
          end;
        bcPrintInt:
          begin
            if Assigned(FOutputDevice) then
            begin
              PrintStr := FConsoleBehavior.FormatNumber(IntRegs[Instr.Src1]);
              FOutputDevice.Print(PrintStr);
              Inc(FCursorCol, Length(PrintStr));
            end;
            Inc(CurPC);
          end;
        bcPrintIntLn:
          begin
            if Assigned(FOutputDevice) then
            begin
              PrintStr := FConsoleBehavior.FormatNumber(IntRegs[Instr.Src1]);
              FOutputDevice.Print(PrintStr);
              FOutputDevice.NewLine;
              FCursorCol := 0;
            end;
            Inc(CurPC);
          end;
        bcPrintString:
          begin
            if Assigned(FOutputDevice) then
            begin
              PrintStr := FConsoleBehavior.FormatString(FStringRegs[Instr.Src1]);
              FOutputDevice.Print(PrintStr);
              Inc(FCursorCol, Length(PrintStr));
            end;
            Inc(CurPC);
          end;
        bcPrintStringLn:
          begin
            if Assigned(FOutputDevice) then
            begin
              PrintStr := FConsoleBehavior.FormatString(FStringRegs[Instr.Src1]);
              FOutputDevice.Print(PrintStr);
              FOutputDevice.NewLine;
              FCursorCol := 0;
            end;
            Inc(CurPC);
          end;
        bcPrintComma:
          begin
            if Assigned(FOutputDevice) then
            begin
              NextTabCol := FConsoleBehavior.GetNextTabPosition(FCursorCol);
              if NextTabCol = 0 then
              begin
                FOutputDevice.NewLine;
                FCursorCol := 0;
              end
              else if FConsoleBehavior.CommaAction = caTabZone then
              begin
                while FCursorCol < NextTabCol do
                begin
                  FOutputDevice.Print(' ');
                  Inc(FCursorCol);
                end;
              end
              else if FConsoleBehavior.CommaAction = caFixedSpaces then
              begin
                for TabIdx := 1 to FConsoleBehavior.CommaSpaces do
                begin
                  FOutputDevice.Print(' ');
                  Inc(FCursorCol);
                end;
              end
              else if FConsoleBehavior.CommaAction = caNewLine then
              begin
                FOutputDevice.NewLine;
                FCursorCol := 0;
              end;
            end;
            Inc(CurPC);
          end;
        bcPrintSemicolon:
          begin
            if Assigned(FOutputDevice) then
            begin
              case FConsoleBehavior.SemicolonAction of
                saNoSpace: ;
                saSpaceAfter, saSpaceBoth:
                  begin
                    FOutputDevice.Print(' ');
                    Inc(FCursorCol);
                  end;
                saSpaceBefore: ;
              end;
            end;
            Inc(CurPC);
          end;
        bcPrintTab:
          begin
            if Assigned(FOutputDevice) then
            begin
              NextTabCol := Instr.Immediate;
              if NextTabCol < 1 then NextTabCol := 1;
              Dec(NextTabCol);
              if FCursorCol < NextTabCol then
              begin
                while FCursorCol < NextTabCol do
                begin
                  FOutputDevice.Print(' ');
                  Inc(FCursorCol);
                end;
              end
              else if FCursorCol > NextTabCol then
              begin
                FOutputDevice.NewLine;
                FCursorCol := 0;
                while FCursorCol < NextTabCol do
                begin
                  FOutputDevice.Print(' ');
                  Inc(FCursorCol);
                end;
              end;
            end;
            Inc(CurPC);
          end;
        bcPrintSpc:
          begin
            if Assigned(FOutputDevice) then
            begin
              for TabIdx := 1 to Instr.Immediate do
              begin
                FOutputDevice.Print(' ');
                Inc(FCursorCol);
              end;
            end;
            Inc(CurPC);
          end;
        bcPrintNewLine:
          begin
            if Assigned(FOutputDevice) then
            begin
              FOutputDevice.NewLine;
              FCursorCol := 0;
            end;
            Inc(CurPC);
          end;

        bcInput:
          begin
            if Assigned(FInputDevice) then
            begin
              repeat
                InputStr := FInputDevice.ReadLine('', False, True, True);
                if TryStrToFloat(InputStr, InputVal) then
                begin
                  FloatRegs[Instr.Dest] := InputVal;
                  Break;
                end
                else if Assigned(FOutputDevice) then
                  FOutputDevice.Print('?Invalid number. Try again: ');
              until False;
            end;
            Inc(CurPC);
          end;
        bcInputInt:
          begin
            if Assigned(FInputDevice) then
            begin
              repeat
                InputStr := FInputDevice.ReadLine('', False, True, True);
                if TryStrToInt64(Trim(InputStr), IntRegs[Instr.Dest]) then
                  Break
                else if Assigned(FOutputDevice) then
                  FOutputDevice.Print('?Invalid integer. Try again: ');
              until False;
            end;
            Inc(CurPC);
          end;
        bcInputFloat:
          begin
            if Assigned(FInputDevice) then
            begin
              repeat
                InputStr := FInputDevice.ReadLine('', False, True, True);
                if TryStrToFloat(Trim(InputStr), InputVal) then
                begin
                  FloatRegs[Instr.Dest] := InputVal;
                  Break;
                end
                else if Assigned(FOutputDevice) then
                  FOutputDevice.Print('?Invalid number. Try again: ');
              until False;
            end;
            Inc(CurPC);
          end;
        bcInputString:
          begin
            if Assigned(FInputDevice) then
              FStringRegs[Instr.Dest] := FInputDevice.ReadLine('', False, False, False);
            Inc(CurPC);
          end;

        // String operations
        bcStrConcat:
          begin
            FStringRegs[Instr.Dest] := FStringRegs[Instr.Src1] + FStringRegs[Instr.Src2];
            Inc(CurPC);
          end;
        bcStrLen:
          begin
            IntRegs[Instr.Dest] := Length(FStringRegs[Instr.Src1]);
            Inc(CurPC);
          end;
        bcStrLeft:
          begin
            FStringRegs[Instr.Dest] := LeftStr(FStringRegs[Instr.Src1], IntRegs[Instr.Src2]);
            Inc(CurPC);
          end;
        bcStrRight:
          begin
            FStringRegs[Instr.Dest] := RightStr(FStringRegs[Instr.Src1], IntRegs[Instr.Src2]);
            Inc(CurPC);
          end;
        bcStrMid:
          begin
            // Src1=string, Src2=start pos, Immediate=length
            FStringRegs[Instr.Dest] := Copy(FStringRegs[Instr.Src1], IntRegs[Instr.Src2], Instr.Immediate);
            Inc(CurPC);
          end;

        // Type conversions (string)
        bcIntToString:
          begin
            FStringRegs[Instr.Dest] := IntToStr(IntRegs[Instr.Src1]);
            Inc(CurPC);
          end;
        bcFloatToString:
          begin
            FStringRegs[Instr.Dest] := FloatToStr(FloatRegs[Instr.Src1]);
            Inc(CurPC);
          end;
        bcStringToInt:
          begin
            IntRegs[Instr.Dest] := StrToInt64Def(FStringRegs[Instr.Src1], 0);
            Inc(CurPC);
          end;
        bcStringToFloat:
          begin
            FloatRegs[Instr.Dest] := StrToFloatDef(FStringRegs[Instr.Src1], 0.0);
            Inc(CurPC);
          end;

        // Logical operations
        bcLogicalAnd:
          begin
            IntRegs[Instr.Dest] := Ord((IntRegs[Instr.Src1] <> 0) and (IntRegs[Instr.Src2] <> 0));
            Inc(CurPC);
          end;
        bcLogicalOr:
          begin
            IntRegs[Instr.Dest] := Ord((IntRegs[Instr.Src1] <> 0) or (IntRegs[Instr.Src2] <> 0));
            Inc(CurPC);
          end;
        bcLogicalNot:
          begin
            IntRegs[Instr.Dest] := Ord(IntRegs[Instr.Src1] = 0);
            Inc(CurPC);
          end;

        // Graphics operations
        bcGraphicRGBA:
          begin
            // RGBA(r, g, b, a) - combine 4 int registers into a 32-bit RGBA color
            // Bytecode format: Dest=result, Src1=R, Src2=G, Immediate=(B_reg << 16) | A_reg
            // Result = (A << 24) | (R << 16) | (G << 8) | B
            IntRegs[Instr.Dest] :=
              ((IntRegs[Instr.Immediate and $FFFF] and $FF) shl 24) or        // A from low 16 bits of Immediate
              ((IntRegs[Instr.Src1] and $FF) shl 16) or                        // R
              ((IntRegs[Instr.Src2] and $FF) shl 8) or                         // G
              (IntRegs[(Instr.Immediate shr 16) and $FFFF] and $FF);           // B from high 16 bits of Immediate
            Inc(CurPC);
          end;
        bcGraphicSetMode, bcGraphicBox, bcGraphicCircle, bcGraphicDraw,
        bcGraphicLocate, bcGraphicRdot, bcGraphicGetMode:
          begin
            // Fall back to slow path for graphics commands that need output device
            FPC := CurPC;
            ExecuteInstruction(Instr);
            CurPC := FPC + 1;
          end;

        // Termination
        bcEnd, bcStop: Break;
        bcNop: Inc(CurPC);

        // Load/Store var (legacy - should not appear in optimized code)
        bcLoadVar, bcStoreVar:
          begin
            // Fall back to slow path
            FPC := CurPC;
            ExecuteInstruction(Instr);
            CurPC := FPC + 1;
          end;
      else
        raise Exception.CreateFmt('Unknown opcode %d at CurPC=%d', [Op, CurPC]);
      end;
    end;
  end;

  {$POP}  // Restore range/overflow checking

  FPC := CurPC;
  FRunning := False;

  // Reset FAST mode when program ends (ensure screen is visible)
  if Assigned(FOutputDevice) then
    FOutputDevice.SetFastMode(False);
end;

{ RunSwitchedGoto - Switched goto dispatch for better branch prediction

  This technique duplicates the dispatch code after each instruction handler,
  giving the CPU branch predictor separate prediction entries for each opcode.
  This exploits instruction locality: after AddFloat usually comes another AddFloat
  or ArrayLoad, and the predictor can learn these patterns.

  Reference: https://bullno1.com/blog/switched-goto }

{$GOTO ON}
procedure TBytecodeVM.RunSwitchedGoto;
type
  PBytecodeInstruction = ^TBytecodeInstruction;
var
  Instructions: PBytecodeInstruction;
  InstrCount: Integer;
  CurPC: Integer;
  Instr: TBytecodeInstruction;
  Op: Byte;
  IntRegs: PInt64;
  FloatRegs: PDouble;
  ArrayIdx: Integer;
  InputStr: string;
  InputVal: Double;
  i, ProdDims: Integer;
  ArrInfo: TSSAArrayInfo;
  // Local vars for ArrayReverseRange and ArrayShiftLeft
  ArrIdxW: Word;
  LStartIdx, LEndIdx, LLoopIdx: Integer;
  LFirstVal: Int64;
  
label
  // Dispatch label
  dispatch_next,
  lbl_exit,
  // Superinstruction labels (100+)
  lbl_s100, lbl_s101, lbl_s102, lbl_s103, lbl_s104, lbl_s105,
  lbl_s110, lbl_s111, lbl_s112, lbl_s113, lbl_s114, lbl_s115,
  lbl_s120, lbl_s121, lbl_s122,
  lbl_s130, lbl_s131, lbl_s132, lbl_s133,
  lbl_s140, lbl_s141, lbl_s142,
  lbl_s150, lbl_s151, lbl_s152, lbl_s153,
  lbl_s160, lbl_s161,
  lbl_s170, lbl_s171,
  lbl_s180, lbl_s181, lbl_s182,
  lbl_s190, lbl_s191, lbl_s192, lbl_s193,
  lbl_s156, lbl_s157, lbl_s250, lbl_s251, lbl_s252, lbl_s253, lbl_s254, lbl_s255,
  // Standard opcode labels
  lbl_LoadConstInt, lbl_LoadConstFloat, lbl_LoadConstString,
  lbl_CopyInt, lbl_CopyFloat, lbl_CopyString,
  lbl_AddInt, lbl_SubInt, lbl_MulInt, lbl_DivInt, lbl_ModInt, lbl_NegInt,
  lbl_AddFloat, lbl_SubFloat, lbl_MulFloat, lbl_DivFloat, lbl_PowFloat, lbl_NegFloat,
  lbl_IntToFloat, lbl_FloatToInt,
  lbl_CmpEqInt, lbl_CmpNeInt, lbl_CmpLtInt, lbl_CmpGtInt, lbl_CmpLeInt, lbl_CmpGeInt,
  lbl_CmpEqFloat, lbl_CmpNeFloat, lbl_CmpLtFloat, lbl_CmpGtFloat, lbl_CmpLeFloat, lbl_CmpGeFloat,
  lbl_MathAbs, lbl_MathSgn, lbl_MathInt, lbl_MathSqr, lbl_MathSin, lbl_MathCos,
  lbl_MathTan, lbl_MathExp, lbl_MathLog, lbl_MathRnd, lbl_MathAtn,
  lbl_Jump, lbl_JumpIfZero, lbl_JumpIfNotZero, lbl_Call, lbl_Return,
  lbl_ArrayLoadFloat, lbl_ArrayStoreFloat, lbl_ArrayLoadInt, lbl_ArrayStoreInt, lbl_ArrayDim,
  lbl_Print, lbl_PrintLn, lbl_PrintString, lbl_PrintStringLn,
  lbl_End, lbl_Nop,
  lbl_Unknown;

begin
  if FProgram = nil then raise Exception.Create('No program loaded');

  Instructions := PBytecodeInstruction(FProgram.GetInstructionsPtr);
  if Instructions = nil then raise Exception.Create('Empty program');

  InstrCount := FProgram.GetInstructionCount;
  CurPC := FProgram.EntryPoint;

  IntRegs := @FIntRegs[0];
  FloatRegs := @FFloatRegs[0];

  {$PUSH}
  {$R-}
  {$Q-}

  // Initial dispatch - jump to dispatch_next
  goto dispatch_next;

  // === DISPATCH POINT - This is where we decode and jump to the next instruction ===
  dispatch_next:
    if CurPC >= InstrCount then goto lbl_exit;
    Instr := Instructions[CurPC];
    Op := Instr.OpCode;
    {$IFDEF ENABLE_INSTRUCTION_COUNTING}
    Inc(FInstructionsExecuted);
    {$ENDIF}
    // Superinstructions first (most common in hot loops)
    if Op >= 110 then
    begin
      case Op of
        190: goto lbl_s190;
        191: goto lbl_s191;
        192: goto lbl_s192;
        193: goto lbl_s193;
        100: goto lbl_s100;
        101: goto lbl_s101;
        102: goto lbl_s102;
        103: goto lbl_s103;
        104: goto lbl_s104;
        105: goto lbl_s105;
        110: goto lbl_s110;
        111: goto lbl_s111;
        112: goto lbl_s112;
        113: goto lbl_s113;
        114: goto lbl_s114;
        115: goto lbl_s115;
        120: goto lbl_s120;
        121: goto lbl_s121;
        122: goto lbl_s122;
        130: goto lbl_s130;
        131: goto lbl_s131;
        132: goto lbl_s132;
        133: goto lbl_s133;
        140: goto lbl_s140;
        141: goto lbl_s141;
        142: goto lbl_s142;
        150: goto lbl_s150;
        151: goto lbl_s151;
        152: goto lbl_s152;
        153: goto lbl_s153;
        160: goto lbl_s160;
        161: goto lbl_s161;
        170: goto lbl_s170;
        171: goto lbl_s171;
        180: goto lbl_s180;
        181: goto lbl_s181;
        182: goto lbl_s182;
        156: goto lbl_s156;
        157: goto lbl_s157;
        250: goto lbl_s250;
        251: goto lbl_s251;
        252: goto lbl_s252;
        253: goto lbl_s253;
        254: goto lbl_s254;
        255: goto lbl_s255;
      else
        goto lbl_Unknown;
      end;
    end
    else
    begin
      case TBytecodeOp(Op) of
        bcLoadConstInt: goto lbl_LoadConstInt;
        bcLoadConstFloat: goto lbl_LoadConstFloat;
        bcLoadConstString: goto lbl_LoadConstString;
        bcCopyInt: goto lbl_CopyInt;
        bcCopyFloat: goto lbl_CopyFloat;
        bcCopyString: goto lbl_CopyString;
        bcAddInt: goto lbl_AddInt;
        bcSubInt: goto lbl_SubInt;
        bcMulInt: goto lbl_MulInt;
        bcDivInt: goto lbl_DivInt;
        bcModInt: goto lbl_ModInt;
        bcNegInt: goto lbl_NegInt;
        bcAddFloat: goto lbl_AddFloat;
        bcSubFloat: goto lbl_SubFloat;
        bcMulFloat: goto lbl_MulFloat;
        bcDivFloat: goto lbl_DivFloat;
        bcPowFloat: goto lbl_PowFloat;
        bcNegFloat: goto lbl_NegFloat;
        bcIntToFloat: goto lbl_IntToFloat;
        bcFloatToInt: goto lbl_FloatToInt;
        bcCmpEqInt: goto lbl_CmpEqInt;
        bcCmpNeInt: goto lbl_CmpNeInt;
        bcCmpLtInt: goto lbl_CmpLtInt;
        bcCmpGtInt: goto lbl_CmpGtInt;
        bcCmpLeInt: goto lbl_CmpLeInt;
        bcCmpGeInt: goto lbl_CmpGeInt;
        bcCmpEqFloat: goto lbl_CmpEqFloat;
        bcCmpNeFloat: goto lbl_CmpNeFloat;
        bcCmpLtFloat: goto lbl_CmpLtFloat;
        bcCmpGtFloat: goto lbl_CmpGtFloat;
        bcCmpLeFloat: goto lbl_CmpLeFloat;
        bcCmpGeFloat: goto lbl_CmpGeFloat;
        bcMathAbs: goto lbl_MathAbs;
        bcMathSgn: goto lbl_MathSgn;
        bcMathInt: goto lbl_MathInt;
        bcMathSqr: goto lbl_MathSqr;
        bcMathSin: goto lbl_MathSin;
        bcMathCos: goto lbl_MathCos;
        bcMathTan: goto lbl_MathTan;
        bcMathExp: goto lbl_MathExp;
        bcMathLog: goto lbl_MathLog;
        bcMathRnd: goto lbl_MathRnd;
        bcMathAtn: goto lbl_MathAtn;
        bcJump: goto lbl_Jump;
        bcJumpIfZero: goto lbl_JumpIfZero;
        bcJumpIfNotZero: goto lbl_JumpIfNotZero;
        bcCall: goto lbl_Call;
        bcReturn: goto lbl_Return;
        bcArrayLoadFloat: goto lbl_ArrayLoadFloat;
        bcArrayStoreFloat: goto lbl_ArrayStoreFloat;
        bcArrayLoadInt: goto lbl_ArrayLoadInt;
        bcArrayStoreInt: goto lbl_ArrayStoreInt;
        bcArrayDim: goto lbl_ArrayDim;
        bcPrint: goto lbl_Print;
        bcPrintLn: goto lbl_PrintLn;
        bcPrintString: goto lbl_PrintString;
        bcPrintStringLn: goto lbl_PrintStringLn;
        bcEnd, bcStop: goto lbl_End;
        bcNop: goto lbl_Nop;
      else
        goto lbl_Unknown;
      end;
    end;

  // === SUPERINSTRUCTION HANDLERS ===

  // Fused loop increment-and-branch (hottest instructions)
  lbl_s190: // AddIntToBranchLe
    Inc(IntRegs[Instr.Dest], IntRegs[Instr.Src1]);
    if IntRegs[Instr.Dest] <= IntRegs[Instr.Src2] then
      CurPC := Instr.Immediate
    else
      Inc(CurPC);
    goto dispatch_next;

  lbl_s191: // AddIntToBranchLt
    Inc(IntRegs[Instr.Dest], IntRegs[Instr.Src1]);
    if IntRegs[Instr.Dest] < IntRegs[Instr.Src2] then
      CurPC := Instr.Immediate
    else
      Inc(CurPC);
    goto dispatch_next;

  lbl_s192: // SubIntToBranchGe
    Dec(IntRegs[Instr.Dest], IntRegs[Instr.Src1]);
    if IntRegs[Instr.Dest] >= IntRegs[Instr.Src2] then
      CurPC := Instr.Immediate
    else
      Inc(CurPC);
    goto dispatch_next;

  lbl_s193: // SubIntToBranchGt
    Dec(IntRegs[Instr.Dest], IntRegs[Instr.Src1]);
    if IntRegs[Instr.Dest] > IntRegs[Instr.Src2] then
      CurPC := Instr.Immediate
    else
      Inc(CurPC);
    goto dispatch_next;

  // Fused compare-and-branch Int
  lbl_s100: if IntRegs[Instr.Src1] = IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;
  lbl_s101: if IntRegs[Instr.Src1] <> IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;
  lbl_s102: if IntRegs[Instr.Src1] < IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;
  lbl_s103: if IntRegs[Instr.Src1] > IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;
  lbl_s104: if IntRegs[Instr.Src1] <= IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;
  lbl_s105: if IntRegs[Instr.Src1] >= IntRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;

  // Fused compare-and-branch Float
  lbl_s110: if FloatRegs[Instr.Src1] = FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;
  lbl_s111: if FloatRegs[Instr.Src1] <> FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;
  lbl_s112: if FloatRegs[Instr.Src1] < FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;
  lbl_s113: if FloatRegs[Instr.Src1] > FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;
  lbl_s114: if FloatRegs[Instr.Src1] <= FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;
  lbl_s115: if FloatRegs[Instr.Src1] >= FloatRegs[Instr.Src2] then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;

  // Fused arithmetic-to-dest Int
  lbl_s120: IntRegs[Instr.Dest] := IntRegs[Instr.Dest] + IntRegs[Instr.Src1]; Inc(CurPC); goto dispatch_next;
  lbl_s121: IntRegs[Instr.Dest] := IntRegs[Instr.Dest] - IntRegs[Instr.Src1]; Inc(CurPC); goto dispatch_next;
  lbl_s122: IntRegs[Instr.Dest] := IntRegs[Instr.Dest] * IntRegs[Instr.Src1]; Inc(CurPC); goto dispatch_next;

  // Fused arithmetic-to-dest Float
  lbl_s130: FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] + FloatRegs[Instr.Src1]; Inc(CurPC); goto dispatch_next;
  lbl_s131: FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] - FloatRegs[Instr.Src1]; Inc(CurPC); goto dispatch_next;
  lbl_s132: FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] * FloatRegs[Instr.Src1]; Inc(CurPC); goto dispatch_next;
  lbl_s133: FloatRegs[Instr.Dest] := FloatRegs[Instr.Dest] / FloatRegs[Instr.Src1]; Inc(CurPC); goto dispatch_next;

  // Fused constant arithmetic Int
  lbl_s140: IntRegs[Instr.Dest] := IntRegs[Instr.Src1] + Instr.Immediate; Inc(CurPC); goto dispatch_next;
  lbl_s141: IntRegs[Instr.Dest] := IntRegs[Instr.Src1] - Instr.Immediate; Inc(CurPC); goto dispatch_next;
  lbl_s142: IntRegs[Instr.Dest] := IntRegs[Instr.Src1] * Instr.Immediate; Inc(CurPC); goto dispatch_next;

  // Fused constant arithmetic Float
  lbl_s150: FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] + Double(Pointer(@Instr.Immediate)^); Inc(CurPC); goto dispatch_next;
  lbl_s151: FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] - Double(Pointer(@Instr.Immediate)^); Inc(CurPC); goto dispatch_next;
  lbl_s152: FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] * Double(Pointer(@Instr.Immediate)^); Inc(CurPC); goto dispatch_next;
  lbl_s153: FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] / Double(Pointer(@Instr.Immediate)^); Inc(CurPC); goto dispatch_next;

  // Fused compare-zero-and-branch Int
  lbl_s160: if IntRegs[Instr.Src1] = 0 then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;
  lbl_s161: if IntRegs[Instr.Src1] <> 0 then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;

  // Fused compare-zero-and-branch Float
  lbl_s170: if FloatRegs[Instr.Src1] = 0.0 then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;
  lbl_s171: if FloatRegs[Instr.Src1] <> 0.0 then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;

  // Fused array-store-constant
  lbl_s180: FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] := Instr.Immediate; Inc(CurPC); goto dispatch_next;
  lbl_s181: FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]] := Double(Pointer(@Instr.Immediate)^); Inc(CurPC); goto dispatch_next;
  lbl_s182: FArrays[Instr.Src1].StringData[IntRegs[Instr.Src2]] := FProgram.StringConstants[Instr.Immediate]; Inc(CurPC); goto dispatch_next;

  // NEW: Array Swap (Int) - opcode 250
  lbl_s250: // bcArraySwapInt: swap arr[idx1] and arr[idx2]
    begin
      FSwapTempInt := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]];
      FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Dest]];
      FArrays[Instr.Src1].IntData[IntRegs[Instr.Dest]] := FSwapTempInt;
      Inc(CurPC);
      goto dispatch_next;
    end;

  // NEW: Self-increment/decrement (Int) - opcodes 251-252
  lbl_s251: Inc(IntRegs[Instr.Dest], IntRegs[Instr.Src1]); Inc(CurPC); goto dispatch_next; // bcAddIntSelf
  lbl_s252: Dec(IntRegs[Instr.Dest], IntRegs[Instr.Src1]); Inc(CurPC); goto dispatch_next; // bcSubIntSelf

  // NEW: Array superinstructions 253, 254, 255, 156, 157
  lbl_s253: IntRegs[Instr.Dest] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]]; Inc(CurPC); goto dispatch_next;
  lbl_s254: FArrays[Instr.Dest].IntData[IntRegs[Instr.Src2]] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]]; Inc(CurPC); goto dispatch_next;
  lbl_s255: FArrays[Instr.Dest].IntData[IntRegs[Instr.Src2]] := FArrays[Instr.Dest].IntData[IntRegs[Instr.Src1]]; Inc(CurPC); goto dispatch_next;

  lbl_s156: // ArrayReverseRange
    begin
      LStartIdx := IntRegs[Instr.Src2];
      LEndIdx := IntRegs[Instr.Dest] - 1;
      ArrIdxW := Instr.Src1;
      while LStartIdx < LEndIdx do
      begin
        FSwapTempInt := FArrays[ArrIdxW].IntData[LStartIdx];
        FArrays[ArrIdxW].IntData[LStartIdx] := FArrays[ArrIdxW].IntData[LEndIdx];
        FArrays[ArrIdxW].IntData[LEndIdx] := FSwapTempInt;
        Inc(LStartIdx);
        Dec(LEndIdx);
      end;
      Inc(CurPC);
      goto dispatch_next;
    end;

  lbl_s157: // ArrayShiftLeft
    begin
      LStartIdx := IntRegs[Instr.Src2];
      LEndIdx := IntRegs[Instr.Dest];
      ArrIdxW := Instr.Src1;
      LFirstVal := FArrays[ArrIdxW].IntData[LStartIdx];
      LLoopIdx := LStartIdx;
      while LLoopIdx <= LEndIdx do
      begin
        FArrays[ArrIdxW].IntData[LLoopIdx] := FArrays[ArrIdxW].IntData[LLoopIdx + 1];
        Inc(LLoopIdx);
      end;
      FArrays[ArrIdxW].IntData[LEndIdx + 1] := LFirstVal;
      Inc(CurPC);
      goto dispatch_next;
    end;

  // === STANDARD OPCODE HANDLERS ===

  lbl_LoadConstInt: IntRegs[Instr.Dest] := Instr.Immediate; Inc(CurPC); goto dispatch_next;
  lbl_LoadConstFloat: FloatRegs[Instr.Dest] := Double(Pointer(@Instr.Immediate)^); Inc(CurPC); goto dispatch_next;
  lbl_LoadConstString: FStringRegs[Instr.Dest] := FProgram.StringConstants[Instr.Immediate]; Inc(CurPC); goto dispatch_next;

  lbl_CopyInt: IntRegs[Instr.Dest] := IntRegs[Instr.Src1]; Inc(CurPC); goto dispatch_next;
  lbl_CopyFloat: FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1]; Inc(CurPC); goto dispatch_next;
  lbl_CopyString: FStringRegs[Instr.Dest] := FStringRegs[Instr.Src1]; Inc(CurPC); goto dispatch_next;

  // Integer arithmetic
  lbl_AddInt: IntRegs[Instr.Dest] := IntRegs[Instr.Src1] + IntRegs[Instr.Src2]; Inc(CurPC); goto dispatch_next;
  lbl_SubInt: IntRegs[Instr.Dest] := IntRegs[Instr.Src1] - IntRegs[Instr.Src2]; Inc(CurPC); goto dispatch_next;
  lbl_MulInt: IntRegs[Instr.Dest] := IntRegs[Instr.Src1] * IntRegs[Instr.Src2]; Inc(CurPC); goto dispatch_next;
  lbl_DivInt: IntRegs[Instr.Dest] := IntRegs[Instr.Src1] div IntRegs[Instr.Src2]; Inc(CurPC); goto dispatch_next;
  lbl_ModInt: IntRegs[Instr.Dest] := IntRegs[Instr.Src1] mod IntRegs[Instr.Src2]; Inc(CurPC); goto dispatch_next;
  lbl_NegInt: IntRegs[Instr.Dest] := -IntRegs[Instr.Src1]; Inc(CurPC); goto dispatch_next;

  // Float arithmetic (hot path for numerical benchmarks)
  lbl_AddFloat: FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] + FloatRegs[Instr.Src2]; Inc(CurPC); goto dispatch_next;
  lbl_SubFloat: FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] - FloatRegs[Instr.Src2]; Inc(CurPC); goto dispatch_next;
  lbl_MulFloat: FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] * FloatRegs[Instr.Src2]; Inc(CurPC); goto dispatch_next;
  lbl_DivFloat: FloatRegs[Instr.Dest] := FloatRegs[Instr.Src1] / FloatRegs[Instr.Src2]; Inc(CurPC); goto dispatch_next;
  lbl_PowFloat: FloatRegs[Instr.Dest] := Power(FloatRegs[Instr.Src1], FloatRegs[Instr.Src2]); Inc(CurPC); goto dispatch_next;
  lbl_NegFloat: FloatRegs[Instr.Dest] := -FloatRegs[Instr.Src1]; Inc(CurPC); goto dispatch_next;

  // Type conversions
  lbl_IntToFloat: FloatRegs[Instr.Dest] := IntRegs[Instr.Src1]; Inc(CurPC); goto dispatch_next;
  lbl_FloatToInt: IntRegs[Instr.Dest] := Trunc(FloatRegs[Instr.Src1]); Inc(CurPC); goto dispatch_next;

  // Integer comparisons
  lbl_CmpEqInt: IntRegs[Instr.Dest] := Ord(IntRegs[Instr.Src1] = IntRegs[Instr.Src2]); Inc(CurPC); goto dispatch_next;
  lbl_CmpNeInt: IntRegs[Instr.Dest] := Ord(IntRegs[Instr.Src1] <> IntRegs[Instr.Src2]); Inc(CurPC); goto dispatch_next;
  lbl_CmpLtInt: IntRegs[Instr.Dest] := Ord(IntRegs[Instr.Src1] < IntRegs[Instr.Src2]); Inc(CurPC); goto dispatch_next;
  lbl_CmpGtInt: IntRegs[Instr.Dest] := Ord(IntRegs[Instr.Src1] > IntRegs[Instr.Src2]); Inc(CurPC); goto dispatch_next;
  lbl_CmpLeInt: IntRegs[Instr.Dest] := Ord(IntRegs[Instr.Src1] <= IntRegs[Instr.Src2]); Inc(CurPC); goto dispatch_next;
  lbl_CmpGeInt: IntRegs[Instr.Dest] := Ord(IntRegs[Instr.Src1] >= IntRegs[Instr.Src2]); Inc(CurPC); goto dispatch_next;

  // Float comparisons
  lbl_CmpEqFloat: IntRegs[Instr.Dest] := Ord(FloatRegs[Instr.Src1] = FloatRegs[Instr.Src2]); Inc(CurPC); goto dispatch_next;
  lbl_CmpNeFloat: IntRegs[Instr.Dest] := Ord(FloatRegs[Instr.Src1] <> FloatRegs[Instr.Src2]); Inc(CurPC); goto dispatch_next;
  lbl_CmpLtFloat: IntRegs[Instr.Dest] := Ord(FloatRegs[Instr.Src1] < FloatRegs[Instr.Src2]); Inc(CurPC); goto dispatch_next;
  lbl_CmpGtFloat: IntRegs[Instr.Dest] := Ord(FloatRegs[Instr.Src1] > FloatRegs[Instr.Src2]); Inc(CurPC); goto dispatch_next;
  lbl_CmpLeFloat: IntRegs[Instr.Dest] := Ord(FloatRegs[Instr.Src1] <= FloatRegs[Instr.Src2]); Inc(CurPC); goto dispatch_next;
  lbl_CmpGeFloat: IntRegs[Instr.Dest] := Ord(FloatRegs[Instr.Src1] >= FloatRegs[Instr.Src2]); Inc(CurPC); goto dispatch_next;

  // Math functions
  lbl_MathAbs: FloatRegs[Instr.Dest] := Abs(FloatRegs[Instr.Src1]); Inc(CurPC); goto dispatch_next;
  lbl_MathSgn:
    begin
      if FloatRegs[Instr.Src1] > 0 then FloatRegs[Instr.Dest] := 1
      else if FloatRegs[Instr.Src1] < 0 then FloatRegs[Instr.Dest] := -1
      else FloatRegs[Instr.Dest] := 0;
      Inc(CurPC);
      goto dispatch_next;
    end;
  lbl_MathInt: FloatRegs[Instr.Dest] := Floor(FloatRegs[Instr.Src1]); Inc(CurPC); goto dispatch_next;
  lbl_MathSqr: FloatRegs[Instr.Dest] := Sqrt(FloatRegs[Instr.Src1]); Inc(CurPC); goto dispatch_next;
  lbl_MathSin: FloatRegs[Instr.Dest] := Sin(FloatRegs[Instr.Src1]); Inc(CurPC); goto dispatch_next;
  lbl_MathCos: FloatRegs[Instr.Dest] := Cos(FloatRegs[Instr.Src1]); Inc(CurPC); goto dispatch_next;
  lbl_MathTan: FloatRegs[Instr.Dest] := Tan(FloatRegs[Instr.Src1]); Inc(CurPC); goto dispatch_next;
  lbl_MathExp: FloatRegs[Instr.Dest] := Exp(FloatRegs[Instr.Src1]); Inc(CurPC); goto dispatch_next;
  lbl_MathLog: FloatRegs[Instr.Dest] := Ln(FloatRegs[Instr.Src1]); Inc(CurPC); goto dispatch_next;
  lbl_MathRnd: FloatRegs[Instr.Dest] := Random; Inc(CurPC); goto dispatch_next;
  lbl_MathAtn: FloatRegs[Instr.Dest] := ArcTan(FloatRegs[Instr.Src1]); Inc(CurPC); goto dispatch_next;

  // Control flow
  lbl_Jump: CurPC := Instr.Immediate; goto dispatch_next;
  lbl_JumpIfZero: if IntRegs[Instr.Src1] = 0 then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;
  lbl_JumpIfNotZero: if IntRegs[Instr.Src1] <> 0 then CurPC := Instr.Immediate else Inc(CurPC); goto dispatch_next;

  lbl_Call:
    begin
      FCallStack[FCallStackPtr] := CurPC + 1;
      Inc(FCallStackPtr);
      CurPC := Instr.Immediate;
      goto dispatch_next;
    end;

  lbl_Return:
    begin
      Dec(FCallStackPtr);
      CurPC := FCallStack[FCallStackPtr];
      goto dispatch_next;
    end;

  // Array operations (hot path for numerical benchmarks)
  lbl_ArrayLoadFloat:
    FloatRegs[Instr.Dest] := FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]];
    Inc(CurPC);
    goto dispatch_next;

  lbl_ArrayStoreFloat:
    FArrays[Instr.Src1].FloatData[IntRegs[Instr.Src2]] := FloatRegs[Instr.Dest];
    Inc(CurPC);
    goto dispatch_next;

  lbl_ArrayLoadInt:
    IntRegs[Instr.Dest] := FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]];
    Inc(CurPC);
    goto dispatch_next;

  lbl_ArrayStoreInt:
    FArrays[Instr.Src1].IntData[IntRegs[Instr.Src2]] := IntRegs[Instr.Dest];
    Inc(CurPC);
    goto dispatch_next;

  lbl_ArrayDim:
    begin
      ArrayIdx := Instr.Src1;
      ArrInfo := FProgram.GetArray(ArrayIdx);
      if ArrayIdx >= Length(FArrays) then
        SetLength(FArrays, ArrayIdx + 1);
      FArrays[ArrayIdx].ElementType := Ord(ArrInfo.ElementType);
      FArrays[ArrayIdx].DimCount := ArrInfo.DimCount;
      SetLength(FArrays[ArrayIdx].Dimensions, ArrInfo.DimCount);
      for i := 0 to ArrInfo.DimCount - 1 do
      begin
        if ArrInfo.Dimensions[i] = 0 then
        begin
          case ArrInfo.DimRegTypes[i] of
            srtInt: FArrays[ArrayIdx].Dimensions[i] := IntRegs[ArrInfo.DimRegisters[i]] + 1;
            srtFloat: FArrays[ArrayIdx].Dimensions[i] := Trunc(FloatRegs[ArrInfo.DimRegisters[i]]) + 1;
          end;
        end
        else
          FArrays[ArrayIdx].Dimensions[i] := ArrInfo.Dimensions[i];
      end;
      ProdDims := 1;
      for i := 0 to ArrInfo.DimCount - 1 do
        ProdDims := ProdDims * FArrays[ArrayIdx].Dimensions[i];
      FArrays[ArrayIdx].TotalSize := ProdDims;
      case ArrInfo.ElementType of
        srtInt: SetLength(FArrays[ArrayIdx].IntData, ProdDims);
        srtFloat: SetLength(FArrays[ArrayIdx].FloatData, ProdDims);
        srtString: SetLength(FArrays[ArrayIdx].StringData, ProdDims);
      end;
      Inc(CurPC);
      goto dispatch_next;
    end;

  // I/O operations (slow path - not critical for benchmarks)
  lbl_Print:
    begin
      if Assigned(FOutputDevice) then
        FOutputDevice.Print(FloatToStr(FloatRegs[Instr.Src1]));
      Inc(CurPC);
      goto dispatch_next;
    end;

  lbl_PrintLn:
    begin
      if Assigned(FOutputDevice) then
      begin
        FOutputDevice.Print(FloatToStr(FloatRegs[Instr.Src1]));
        FOutputDevice.NewLine;
      end;
      Inc(CurPC);
      goto dispatch_next;
    end;

  lbl_PrintString:
    begin
      if Assigned(FOutputDevice) then
        FOutputDevice.Print(FStringRegs[Instr.Src1]);
      Inc(CurPC);
      goto dispatch_next;
    end;

  lbl_PrintStringLn:
    begin
      if Assigned(FOutputDevice) then
      begin
        FOutputDevice.Print(FStringRegs[Instr.Src1]);
        FOutputDevice.NewLine;
      end;
      Inc(CurPC);
      goto dispatch_next;
    end;

  lbl_End:
    goto lbl_exit;

  lbl_Nop:
    Inc(CurPC);
    goto dispatch_next;

  lbl_Unknown:
    raise Exception.CreateFmt('Unknown opcode %d at PC=%d', [Op, CurPC]);

  lbl_exit:
  {$POP}

  FPC := CurPC;
  FRunning := False;

  // Reset FAST mode when program ends (ensure screen is visible)
  if Assigned(FOutputDevice) then
    FOutputDevice.SetFastMode(False);
end;
{$GOTO OFF}

end.
