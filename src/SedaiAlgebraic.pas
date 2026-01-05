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
  Unit: SedaiAlgebraic (Algebraic Simplification)

  Purpose: Simplify expressions using algebraic identities and mathematical
           properties to reduce computational complexity.

  Algorithm: Pattern matching on SSA instructions
             1. Identify algebraic patterns (x+0, x*1, x*0, etc.)
             2. Apply simplification rules
             3. Replace complex expressions with simpler equivalents

  Examples:
    Before:                After:
    %r1 = x + 0            %r1 = Copy x
    %r2 = x * 1            %r2 = Copy x
    %r3 = x * 0            %r3 = LoadConst 0
    %r4 = x - x            %r4 = LoadConst 0
    %r5 = x / 1            %r5 = Copy x
    %r6 = 0 + x            %r6 = Copy x
    %r7 = 1 * x            %r7 = Copy x

  Algebraic Rules Applied:
    - Identity: x + 0 = x, x * 1 = x, x / 1 = x
    - Annihilation: x * 0 = 0, 0 * x = 0, 0 / x = 0
    - Inverse: x - x = 0, x / x = 1
    - Commutative: 0 + x = x, 1 * x = x
    - Associative: (x + a) + b = x + (a + b) when a,b are constants

  Phase: Early optimization (post-SSA, before CSE)
  Author: Sedai Project - Optimization Pipeline
  Date: 2025-01-25
  ============================================================================ }

unit SedaiAlgebraic;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, contnrs, SedaiSSATypes;

type
  { TSSAValueWrapper - Wrapper to store TSSAValue in TFPHashList }
  PSSAValueWrapper = ^TSSAValueWrapper;
  TSSAValueWrapper = record
    Value: TSSAValue;
  end;

  { TAlgebraicSimplification - Apply algebraic identities }
  TAlgebraicSimplification = class
  private
    FProgram: TSSAProgram;
    FSimplifications: Integer;
    FConstMap: TFPHashList;  // Key: "RegIndex:Version" → Value: PSSAValueWrapper (constant value)

    { Make string key from register value: "RegIndex:Version" }
    function MakeRegKey(const RegVal: TSSAValue): string; inline;

    { Build map of registers that hold known constant values }
    procedure BuildConstantMap;

    { Check if register key maps to a BASIC user variable }
    function IsUserVariable(const VarKey: string): Boolean;

    { Resolve register to constant value if available }
    function ResolveToConst(const Val: TSSAValue; out ConstVal: TSSAValue): Boolean;

    { Check if value is constant zero (immediate or register holding zero) }
    function IsZero(const Val: TSSAValue): Boolean;

    { Check if value is constant one (immediate or register holding one) }
    function IsOne(const Val: TSSAValue): Boolean;

    { Check if two values are the same register }
    function SameRegister(const V1, V2: TSSAValue): Boolean;

    { Simplify instruction using algebraic rules }
    function SimplifyInstruction(const Instr: TSSAInstruction): TSSAInstruction;

    { Simplify arithmetic operations }
    function SimplifyArithmetic(const Instr: TSSAInstruction): TSSAInstruction;

    { Process all blocks }
    procedure SimplifyBlocks;

  public
    constructor Create(Prog: TSSAProgram);
    destructor Destroy; override;

    { Run algebraic simplification pass }
    function Run: Integer;
  end;

implementation

{$IFDEF DEBUG_ALGEBRAIC}
uses SedaiDebug;
{$ENDIF}

{ TAlgebraicSimplification }

constructor TAlgebraicSimplification.Create(Prog: TSSAProgram);
begin
  inherited Create;
  FProgram := Prog;
  FSimplifications := 0;
  FConstMap := TFPHashList.Create;
end;

destructor TAlgebraicSimplification.Destroy;
var
  i: Integer;
  P: PSSAValueWrapper;
begin
  // Free all allocated PSSAValueWrapper records
  for i := 0 to FConstMap.Count - 1 do
  begin
    P := PSSAValueWrapper(FConstMap.Items[i]);
    if P <> nil then
      Dispose(P);
  end;
  FConstMap.Free;
  inherited;
end;

function TAlgebraicSimplification.Run: Integer;
begin
  {$IFDEF DEBUG_ALGEBRAIC}
  if DebugAlgebraic then
    WriteLn('[Algebraic] Running algebraic simplification...');
  {$ENDIF}

  // Step 1: Build constant map (track registers holding constant values)
  BuildConstantMap;

  // Step 2: Simplify using algebraic rules
  SimplifyBlocks;

  {$IFDEF DEBUG_ALGEBRAIC}
  if DebugAlgebraic then
    WriteLn('[Algebraic] Applied ', FSimplifications, ' simplifications');
  {$ENDIF}
  Result := FSimplifications;
end;

function TAlgebraicSimplification.MakeRegKey(const RegVal: TSSAValue): string;
begin
  // Create unique key: "RegIndex:Version"
  // This ensures R5:1 and R5:2 are treated as different registers (SSA versioning)
  Result := IntToStr(RegVal.RegIndex) + ':' + IntToStr(RegVal.Version);
end;

procedure TAlgebraicSimplification.BuildConstantMap;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  i, j: Integer;
  DestKey, SrcKey: string;
  P, PSrc: PSSAValueWrapper;
  ConstVal: TSSAValue;
begin
  // Scan for LoadConst and type conversion instructions to track constant values
  // This allows us to detect patterns like: %r1 = LoadConst 0; %r2 = Add %r3, %r1
  // and simplify to: %r2 = Copy %r3

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      // Track LoadConstInt and LoadConstFloat instructions
      // CRITICAL: DO NOT track if destination is a user variable (BASIC global variable semantics)
      // A user variable like I%=0 in a FOR loop is NOT a constant, even if initialized to 0!
      if (Instr.OpCode in [ssaLoadConstInt, ssaLoadConstFloat]) and
         (Instr.Dest.Kind = svkRegister) then
      begin
        DestKey := MakeRegKey(Instr.Dest);

        // Check if this register maps to a BASIC user variable
        // If it does, skip tracking (user vars can change value, not true constants)
        if IsUserVariable(DestKey) then
          Continue;  // Skip this LoadConst, don't add to FConstMap

        // Allocate and store the constant value
        New(P);
        P^.Value := Instr.Src1;  // Src1 contains the constant value

        // Map: "DestReg:Version" → ConstantValue
        FConstMap.Add(DestKey, P);
      end;

      // Track IntToFloat conversions of known constants
      {if (Instr.OpCode = ssaIntToFloat) and
              (Instr.Dest.Kind = svkRegister) and
              (Instr.Src1.Kind = svkRegister) then
      begin
        // Check if source register holds a known constant
        SrcKey := MakeRegKey(Instr.Src1);
        PSrc := PSSAValueWrapper(FConstMap.Find(SrcKey));
        if (PSrc <> nil) and (PSrc^.Value.Kind = svkConstInt) then
        begin
          // Create float constant from int constant
          DestKey := MakeRegKey(Instr.Dest);
          New(P);
          P^.Value := MakeSSAConstFloat(PSrc^.Value.ConstInt * 1.0);
          FConstMap.Add(DestKey, P);
        end;
      end;

      // Track FloatToInt conversions of known constants
      if (Instr.OpCode = ssaFloatToInt) and
              (Instr.Dest.Kind = svkRegister) and
              (Instr.Src1.Kind = svkRegister) then
      begin
        // Check if source register holds a known constant
        SrcKey := MakeRegKey(Instr.Src1);
        PSrc := PSSAValueWrapper(FConstMap.Find(SrcKey));
        if (PSrc <> nil) and (PSrc^.Value.Kind = svkConstFloat) then
        begin
          // Create int constant from float constant (truncate)
          DestKey := MakeRegKey(Instr.Dest);
          New(P);
          P^.Value := MakeSSAConstInt(Trunc(PSrc^.Value.ConstFloat));
          FConstMap.Add(DestKey, P);
        end;
      end;}
    end;
  end;

  {$IFDEF DEBUG_ALGEBRAIC}
  if DebugAlgebraic then
    WriteLn('[Algebraic] Tracked ', FConstMap.Count, ' constant-holding registers');
  {$ENDIF}
end;

function TAlgebraicSimplification.IsUserVariable(const VarKey: string): Boolean;
var
  i: Integer;
  MappedKey: string;
  RegIdx: Integer;
  ColonPos: Integer;
begin
  // Check if this VarKey corresponds to a BASIC user variable
  // VarKey can be in two formats:
  //   - From MakeRegKey: "RegIndex:Version" (e.g., "5:0")
  //   - From SimplifyArithmetic: "RegType:RegIndex" (e.g., "1:5")
  // FProgram.VarRegMap stores: "RegType:RegIndex"
  //
  // We need to extract RegIndex from VarKey and check against VarRegMap values
  Result := False;

  // Extract second number from VarKey (after the colon)
  // This works for both formats: "RegIndex:Version" → get RegIndex, "RegType:RegIndex" → get RegIndex
  ColonPos := Pos(':', VarKey);
  if ColonPos = 0 then Exit;

  // For "RegType:RegIndex" format from SimplifyArithmetic, check directly
  for i := 0 to FProgram.VarRegMap.Count - 1 do
  begin
    MappedKey := FProgram.VarRegMap.ValueFromIndex[i];
    if MappedKey = VarKey then
    begin
      Result := True;
      Exit;
    end;
  end;

  // For "RegIndex:Version" format from BuildConstantMap/MakeRegKey, extract RegIndex and check
  // MakeRegKey produces "RegIndex:Version", but VarRegMap stores "RegType:RegIndex"
  // So we need to check if any VarRegMap value ends with the same RegIndex
  RegIdx := StrToIntDef(Copy(VarKey, 1, ColonPos - 1), -1);
  if RegIdx < 0 then Exit;

  for i := 0 to FProgram.VarRegMap.Count - 1 do
  begin
    MappedKey := FProgram.VarRegMap.ValueFromIndex[i];
    // MappedKey is "RegType:RegIndex" - extract RegIndex (after colon)
    ColonPos := Pos(':', MappedKey);
    if ColonPos > 0 then
    begin
      if StrToIntDef(Copy(MappedKey, ColonPos + 1, Length(MappedKey)), -1) = RegIdx then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TAlgebraicSimplification.ResolveToConst(const Val: TSSAValue; out ConstVal: TSSAValue): Boolean;
var
  P: PSSAValueWrapper;
  Key: string;
begin
  // If it's already a constant, return it directly
  if Val.Kind in [svkConstInt, svkConstFloat] then
  begin
    ConstVal := Val;
    Exit(True);
  end;

  // If it's a register, check if it holds a known constant
  if Val.Kind = svkRegister then
  begin
    Key := MakeRegKey(Val);
    P := PSSAValueWrapper(FConstMap.Find(Key));
    if P <> nil then
    begin
      ConstVal := P^.Value;
      Exit(True);
    end;
  end;

  Result := False;
end;

function TAlgebraicSimplification.IsZero(const Val: TSSAValue): Boolean;
var
  ConstVal: TSSAValue;
begin
  Result := False;

  // Resolve to constant value (handles both immediate constants and registers holding constants)
  if not ResolveToConst(Val, ConstVal) then
    Exit;

  case ConstVal.Kind of
    svkConstInt: Result := (ConstVal.ConstInt = 0);
    svkConstFloat: Result := (Abs(ConstVal.ConstFloat) < 1e-10);
  end;
end;

function TAlgebraicSimplification.IsOne(const Val: TSSAValue): Boolean;
var
  ConstVal: TSSAValue;
begin
  Result := False;

  // Resolve to constant value (handles both immediate constants and registers holding constants)
  if not ResolveToConst(Val, ConstVal) then
    Exit;

  case ConstVal.Kind of
    svkConstInt: Result := (ConstVal.ConstInt = 1);
    svkConstFloat: Result := (Abs(ConstVal.ConstFloat - 1.0) < 1e-10);
  end;
end;

function TAlgebraicSimplification.SameRegister(const V1, V2: TSSAValue): Boolean;
begin
  // CRITICAL: Must compare both RegIndex AND Version for correct SSA semantics
  // R5:1 and R5:2 are DIFFERENT registers due to SSA versioning!
  Result := (V1.Kind = svkRegister) and (V2.Kind = svkRegister) and
            (V1.RegIndex = V2.RegIndex) and (V1.Version = V2.Version);
end;

function TAlgebraicSimplification.SimplifyArithmetic(const Instr: TSSAInstruction): TSSAInstruction;
var
  NewInstr: TSSAInstruction;
  VarKey: string;
begin
  Result := Instr;

  // CRITICAL: Do NOT optimize operations involving BASIC user variables
  // With GlobalVariableSemantics (Version=0), we cannot safely determine constant values
  // Example: I% + 1 where I%=0 looks like 0+1, but I% is a loop variable, not a constant
  if (Instr.Src1.Kind = svkRegister) then
  begin
    VarKey := IntToStr(Ord(Instr.Src1.RegType)) + ':' + IntToStr(Instr.Src1.RegIndex);
    if IsUserVariable(VarKey) then
      Exit(Instr);  // Don't optimize, return original instruction
  end;

  if (Instr.Src2.Kind = svkRegister) then
  begin
    VarKey := IntToStr(Ord(Instr.Src2.RegType)) + ':' + IntToStr(Instr.Src2.RegIndex);
    if IsUserVariable(VarKey) then
      Exit(Instr);  // Don't optimize, return original instruction
  end;

  if (Instr.Src3.Kind = svkRegister) then
  begin
    VarKey := IntToStr(Ord(Instr.Src3.RegType)) + ':' + IntToStr(Instr.Src3.RegIndex);
    if IsUserVariable(VarKey) then
      Exit(Instr);  // Don't optimize, return original instruction
  end;

  NewInstr := Instr.Clone;

  case Instr.OpCode of
    // Integer addition: x + 0 = x, 0 + x = x
    ssaAddInt:
    begin
      if IsZero(Instr.Src2) then
      begin
        NewInstr.OpCode := ssaCopyInt;
        NewInstr.Src2 := MakeSSAValue(svkNone);
        Inc(FSimplifications);
        Exit(NewInstr);
      end;
      if IsZero(Instr.Src1) then
      begin
        NewInstr.OpCode := ssaCopyInt;
        NewInstr.Src1 := Instr.Src2;
        NewInstr.Src2 := MakeSSAValue(svkNone);
        Inc(FSimplifications);
        Exit(NewInstr);
      end;
    end;

    // Float addition: NO safe optimizations
    // NOTE: x + 0.0 = x is INCORRECT due to signed zero (-0.0 + 0.0 = +0.0, not -0.0)
    // NOTE: 0.0 + x = x is INCORRECT due to signed zero
    ssaAddFloat:
    begin
      // REMOVED: All x + 0.0 optimizations (incorrect with signed zero)
    end;

    // Integer subtraction: x - 0 = x, x - x = 0
    ssaSubInt:
    begin
      if IsZero(Instr.Src2) then
      begin
        NewInstr.OpCode := ssaCopyInt;
        NewInstr.Src2 := MakeSSAValue(svkNone);
        Inc(FSimplifications);
        Exit(NewInstr);
      end;
      if SameRegister(Instr.Src1, Instr.Src2) then
      begin
        NewInstr.OpCode := ssaLoadConstInt;
        NewInstr.Src1 := MakeSSAConstInt(0);
        NewInstr.Src2 := MakeSSAValue(svkNone);
        Inc(FSimplifications);
        Exit(NewInstr);
      end;
    end;

    // Float subtraction: NO safe optimizations
    // NOTE: x - 0.0 = x is INCORRECT due to signed zero
    // NOTE: x - x = 0.0 is INCORRECT (NaN - NaN = NaN, Infinity - Infinity = NaN, not 0.0!)
    ssaSubFloat:
    begin
      // REMOVED: All float subtraction optimizations (incorrect with NaN/Infinity/signed zero)
    end;

    // Integer multiplication: x * 0 = 0, 0 * x = 0, x * 1 = x, 1 * x = x
    ssaMulInt:
    begin
      if IsZero(Instr.Src1) or IsZero(Instr.Src2) then
      begin
        NewInstr.OpCode := ssaLoadConstInt;
        NewInstr.Src1 := MakeSSAConstInt(0);
        NewInstr.Src2 := MakeSSAValue(svkNone);
        Inc(FSimplifications);
        Exit(NewInstr);
      end;
      if IsOne(Instr.Src2) then
      begin
        NewInstr.OpCode := ssaCopyInt;
        NewInstr.Src2 := MakeSSAValue(svkNone);
        Inc(FSimplifications);
        Exit(NewInstr);
      end;
      if IsOne(Instr.Src1) then
      begin
        NewInstr.OpCode := ssaCopyInt;
        NewInstr.Src1 := Instr.Src2;
        NewInstr.Src2 := MakeSSAValue(svkNone);
        Inc(FSimplifications);
        Exit(NewInstr);
      end;
    end;

    // Float multiplication: NO safe optimizations
    // NOTE: Even x * 1.0 = x can be incorrect with denormals/rounding modes
    // NOTE: x * 0.0 = 0.0 is INCORRECT (0.0 * NaN = NaN, 0.0 * Infinity = NaN)
    ssaMulFloat:
    begin
      // REMOVED: All float multiplication optimizations for IEEE 754 safety
    end;

    // Integer division: x / 1 = x, 0 / x = 0, x / x = 1
    ssaDivInt:
    begin
      if IsOne(Instr.Src2) then
      begin
        NewInstr.OpCode := ssaCopyInt;
        NewInstr.Src2 := MakeSSAValue(svkNone);
        Inc(FSimplifications);
        Exit(NewInstr);
      end;
      if IsZero(Instr.Src1) then
      begin
        NewInstr.OpCode := ssaLoadConstInt;
        NewInstr.Src1 := MakeSSAConstInt(0);
        NewInstr.Src2 := MakeSSAValue(svkNone);
        Inc(FSimplifications);
        Exit(NewInstr);
      end;
      if SameRegister(Instr.Src1, Instr.Src2) then
      begin
        NewInstr.OpCode := ssaLoadConstInt;
        NewInstr.Src1 := MakeSSAConstInt(1);
        NewInstr.Src2 := MakeSSAValue(svkNone);
        Inc(FSimplifications);
        Exit(NewInstr);
      end;
    end;

    // Float division: NO safe optimizations
    // NOTE: Even x / 1.0 = x can be incorrect with denormals/rounding modes
    // NOTE: x / x = 1.0 is INCORRECT (0.0 / 0.0 = NaN, NaN / NaN = NaN, not 1.0!)
    // NOTE: 0.0 / x = 0.0 is INCORRECT (0.0 / 0.0 = NaN)
    ssaDivFloat:
    begin
      // REMOVED: All float division optimizations for IEEE 754 safety
    end;
  end;
end;

function TAlgebraicSimplification.SimplifyInstruction(const Instr: TSSAInstruction): TSSAInstruction;
begin
  Result := Instr;

  // Apply algebraic simplifications
  if Instr.OpCode in [ssaAddInt, ssaAddFloat, ssaSubInt, ssaSubFloat,
                      ssaMulInt, ssaMulFloat, ssaDivInt, ssaDivFloat] then
  begin
    Result := SimplifyArithmetic(Instr);
  end;
end;

procedure TAlgebraicSimplification.SimplifyBlocks;
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
      NewInstr := SimplifyInstruction(Instr);
      if NewInstr.OpCode <> Instr.OpCode then
        Block.Instructions[j] := NewInstr;
    end;
  end;
end;

end.
