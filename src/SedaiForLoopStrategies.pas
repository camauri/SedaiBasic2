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
unit SedaiForLoopStrategies;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}

{
  SEDAI FOR LOOP COMPILATION STRATEGIES
  =====================================

  Questo modulo implementa un'architettura basata su Strategy Pattern
  per eliminare i grossi blocchi di if/then/else annidati nella compilazione
  dei loop FOR.

  DESIGN PRINCIPLES:
  - Strategy Pattern: una strategy per ogni combinazione di contesto
  - Dispatch Tables: lookup invece di if/then/else chains
  - Context Object: tutti i parametri in un record
  - Factory Method: selezione intelligente della strategy
}

interface

uses
  SysUtils, Variants, SedaiBytecodeTypes, SedaiExecutorTypes;

{ Importa TValueType dal compiler principale }
type
  TValueType = (
    vtUnknown,      // Type not determined
    vtInteger,      // Integer (Int64)
    vtFloat,        // Float (Double)
    vtString,       // String
    vtBoolean       // Boolean (stored as Integer)
  );

type
  { Forward declarations }
  TForLoopCompiler = class;

  { TForLoopContext - Complete context for FOR loop compilation }
  TForLoopContext = record
    VarIdx: Integer;           // Loop variable index
    RegIdx: Integer;           // Variable register (-1 if stack-based)
    EndReg: Integer;           // END value register (-1 if not allocated)
    StepConst: Integer;        // STEP constant index (-1 if not constant)
    VarType: TValueType;       // Variable type (vtInteger/vtFloat)
    IsForward: Boolean;        // True if STEP > 0, False if STEP < 0
    HasConstEnd: Boolean;      // True if END is constant
    HasTempReg: Boolean;       // True if temp registers available
    StartLabel: string;        // Loop start label
    BreakLabel: string;        // Loop exit label
    ContinueLabel: string;     // Continue label
  end;

  { IForLoopStrategy - Interface for FOR compilation strategy }
  IForLoopStrategy = interface
    ['{D8F3E2A1-5B4C-4E2D-9F1A-3C7B8E9D0A2F}']
    procedure EmitInitialization(Compiler: TForLoopCompiler; const Ctx: TForLoopContext);
    procedure EmitConditionCheck(Compiler: TForLoopCompiler; const Ctx: TForLoopContext);
    procedure EmitIncrement(Compiler: TForLoopCompiler; const Ctx: TForLoopContext);
    procedure EmitCleanup(Compiler: TForLoopCompiler; const Ctx: TForLoopContext);
  end;

  { TForLoopCompiler - Interface minima per emettere istruzioni }
  { Wrapper per evitare dipendenza circolare con TSedaiBytecodeCompiler }
  TForLoopCompiler = class
  public
    procedure Emit(OpCode: TOpCode); virtual; abstract;
    procedure EmitWithInt(OpCode: TOpCode; Arg: Integer); virtual; abstract;
    procedure EmitWithTwoArgs(OpCode: TOpCode; Arg1, Arg2: Integer); virtual; abstract;
    procedure EmitJump(OpCode: TOpCode; const LabelName: string); virtual; abstract;
    procedure EmitLoadReg(RegIdx: Integer); virtual; abstract;
    procedure EmitStoreReg(RegIdx: Integer); virtual; abstract;
    function AddConstant(const Value: Variant): Integer; virtual; abstract;
    function AllocateTempRegister(VType: TValueType): Integer; virtual; abstract;
    procedure FreeTempRegister(RegIdx: Integer); virtual; abstract;
  end;

  { ====================================================================
    CONCRETE STRATEGIES

    Ogni strategy gestisce una specifica combinazione di:
    - Path: Register vs Stack
    - Type: Integer vs Float
    - Direction: Forward (STEP>0) vs Backward (STEP<0)
    - EndValue: In Register vs Constant vs Expression
    ====================================================================  }

  { Base class con implementazioni di default }
  { NOTE: Using TObject instead of TInterfacedObject because interfaces use CORBA mode (no refcount) }
  TForLoopStrategyBase = class(TObject, IForLoopStrategy)
  protected
    procedure EmitComparisonOpcode(Compiler: TForLoopCompiler;
      const Ctx: TForLoopContext; Reg1, Reg2: Integer); virtual;
  public
    procedure EmitInitialization(Compiler: TForLoopCompiler; const Ctx: TForLoopContext); virtual;
    procedure EmitConditionCheck(Compiler: TForLoopCompiler; const Ctx: TForLoopContext); virtual;
    procedure EmitIncrement(Compiler: TForLoopCompiler; const Ctx: TForLoopContext); virtual;
    procedure EmitCleanup(Compiler: TForLoopCompiler; const Ctx: TForLoopContext); virtual;
  end;

  { REGISTER-BASED STRATEGIES }

  { Strategy: Register + Integer + Forward + END in Register }
  TRegIntForwardEndRegStrategy = class(TForLoopStrategyBase)
  public
    procedure EmitConditionCheck(Compiler: TForLoopCompiler; const Ctx: TForLoopContext); override;
    procedure EmitIncrement(Compiler: TForLoopCompiler; const Ctx: TForLoopContext); override;
  end;

  { Strategy: Register + Integer + Forward + END constant }
  TRegIntForwardConstStrategy = class(TForLoopStrategyBase)
  public
    procedure EmitConditionCheck(Compiler: TForLoopCompiler; const Ctx: TForLoopContext); override;
    procedure EmitIncrement(Compiler: TForLoopCompiler; const Ctx: TForLoopContext); override;
  end;

  { Strategy: Register + Float + Forward + END in Register }
  TRegFloatForwardEndRegStrategy = class(TForLoopStrategyBase)
  public
    procedure EmitConditionCheck(Compiler: TForLoopCompiler; const Ctx: TForLoopContext); override;
    procedure EmitIncrement(Compiler: TForLoopCompiler; const Ctx: TForLoopContext); override;
  end;

  { Strategy: Register + Float + Forward + END constant }
  TRegFloatForwardConstStrategy = class(TForLoopStrategyBase)
  public
    procedure EmitConditionCheck(Compiler: TForLoopCompiler; const Ctx: TForLoopContext); override;
    procedure EmitIncrement(Compiler: TForLoopCompiler; const Ctx: TForLoopContext); override;
  end;

  { STACK-BASED STRATEGY (fallback generico) }
  TStackBasedForwardStrategy = class(TForLoopStrategyBase)
  public
    procedure EmitConditionCheck(Compiler: TForLoopCompiler; const Ctx: TForLoopContext); override;
    procedure EmitIncrement(Compiler: TForLoopCompiler; const Ctx: TForLoopContext); override;
  end;

  { ====================================================================
    FACTORY METHOD & DISPATCH TABLES
    ====================================================================  }

  { Tabelle di dispatch per opcodes invece di if/then/else }
  TCompareOpTable = array[TValueType, Boolean] of TOpCode;
  TArithmeticOpTable = array[TValueType] of TOpCode;

  { Factory per selezionare la strategy appropriata }
  function SelectForLoopStrategy(const Ctx: TForLoopContext): IForLoopStrategy;

  { Dispatch tables globali }
  function GetCompareRegOpcode(VType: TValueType; IsForward: Boolean): TOpCode;
  function GetIncrementOpcode(VType: TValueType; IsConstStep: Boolean): TOpCode;

implementation

{ ====================================================================
  DISPATCH TABLES - Lookup invece di if/then/else
  ====================================================================  }

const
  { Tabella opcodes comparazione: [Tipo, IsForward] → OpCode }
  COMPARE_REG_OPCODES: TCompareOpTable = (
    // vtUnknown (fallback)
    (opLe, opGe),
    // vtInteger
    (opLeRegInt, opGeRegInt),
    // vtFloat
    (opLeRegFloat, opGeRegFloat),
    // vtString (not used for FOR)
    (opLe, opGe),
    // vtBoolean (not used for FOR)
    (opLe, opGe)
  );

  { Increment opcodes table: [Type] → OpCode }
  INCREMENT_OPCODES: TArithmeticOpTable = (
    opIncReg,      // vtUnknown (fallback)
    opIncRegInt,   // vtInteger
    opIncRegFloat, // vtFloat
    opIncReg,      // vtString (not used)
    opIncReg       // vtBoolean (not used)
  );

function GetCompareRegOpcode(VType: TValueType; IsForward: Boolean): TOpCode;
begin
  Result := COMPARE_REG_OPCODES[VType, IsForward];
end;

function GetIncrementOpcode(VType: TValueType; IsConstStep: Boolean): TOpCode;
begin
  // For now we only support STEP 1
  // TODO: Extend for STEP != 1
  Result := INCREMENT_OPCODES[VType];
end;

{ ====================================================================
  FACTORY METHOD - Strategy Selection
  ====================================================================  }

function SelectForLoopStrategy(const Ctx: TForLoopContext): IForLoopStrategy;
begin
  { DECISION TREE SEMPLIFICATO - 1 solo livello invece di 8! }

  if Ctx.RegIdx >= 0 then
  begin
    // ===== REGISTER-BASED PATH =====
    if Ctx.IsForward then
    begin
      // Forward iteration (STEP > 0)
      if Ctx.VarType = vtInteger then
      begin
        if Ctx.EndReg >= 0 then
          Result := TRegIntForwardEndRegStrategy.Create   // Caso ottimale: Int, END in reg
        else
          Result := TRegIntForwardConstStrategy.Create;   // Int, END costante o temp
      end
      else // vtFloat
      begin
        if Ctx.EndReg >= 0 then
          Result := TRegFloatForwardEndRegStrategy.Create
        else
          Result := TRegFloatForwardConstStrategy.Create;
      end;
    end
    else
    begin
      // Backward iteration (STEP < 0)
      // TODO: Implementare strategies per backward
      Result := TStackBasedForwardStrategy.Create;  // Fallback temporaneo
    end;
  end
  else
  begin
    // ===== STACK-BASED PATH (fallback) =====
    Result := TStackBasedForwardStrategy.Create;
  end;
end;

{ ====================================================================
  BASE STRATEGY IMPLEMENTATION
  ====================================================================  }

procedure TForLoopStrategyBase.EmitInitialization(Compiler: TForLoopCompiler;
  const Ctx: TForLoopContext);
begin
  // Default: non fa nulla
  // Le strategies concrete possono override se necessario
end;

procedure TForLoopStrategyBase.EmitConditionCheck(Compiler: TForLoopCompiler;
  const Ctx: TForLoopContext);
begin
  // Default: implementazione generica (suboptimal)
  raise Exception.Create('EmitConditionCheck must be overridden');
end;

procedure TForLoopStrategyBase.EmitIncrement(Compiler: TForLoopCompiler;
  const Ctx: TForLoopContext);
begin
  // Default: implementazione generica
  raise Exception.Create('EmitIncrement must be overridden');
end;

procedure TForLoopStrategyBase.EmitCleanup(Compiler: TForLoopCompiler;
  const Ctx: TForLoopContext);
begin
  // Default: libera EndReg se era temp register
  if (Ctx.EndReg >= 8) and (Ctx.EndReg <= 31) then
    Compiler.FreeTempRegister(Ctx.EndReg);
end;

procedure TForLoopStrategyBase.EmitComparisonOpcode(Compiler: TForLoopCompiler;
  const Ctx: TForLoopContext; Reg1, Reg2: Integer);
var
  OpCode: TOpCode;
begin
  // Usa dispatch table invece di if/then/else!
  OpCode := GetCompareRegOpcode(Ctx.VarType, Ctx.IsForward);
  Compiler.EmitWithTwoArgs(OpCode, Reg1, Reg2);
end;

{ ====================================================================
  REGISTER INTEGER FORWARD + END IN REGISTER
  Caso ottimale: tutto in registri typed, nessun accesso stack
  ====================================================================  }

procedure TRegIntForwardEndRegStrategy.EmitConditionCheck(
  Compiler: TForLoopCompiler; const Ctx: TForLoopContext);
begin
  {
    Emette: LeRegInt R0, R8
            PopJumpIfFalse BreakLabel

    Invece di 8 livelli di if/then/else!
  }
  EmitComparisonOpcode(Compiler, Ctx, Ctx.RegIdx, Ctx.EndReg);
  Compiler.EmitJump(opPopJumpIfFalse, Ctx.BreakLabel);
end;

procedure TRegIntForwardEndRegStrategy.EmitIncrement(
  Compiler: TForLoopCompiler; const Ctx: TForLoopContext);
begin
  {
    Emette: AddRegImm R0, #STEP
    oppure: IncRegInt R0    (se STEP = 1)
  }
  if Ctx.StepConst >= 0 then
  begin
    // STEP costante
    // TODO: Ottimizzare con IncRegInt se STEP = 1
    Compiler.EmitWithTwoArgs(opAddRegImm, Ctx.RegIdx, Ctx.StepConst);
  end
  else
  begin
    // Variable STEP (rare)
    // Fallback: use opIncRegInt
    Compiler.EmitWithInt(opIncRegInt, Ctx.RegIdx);
  end;
end;

{ ====================================================================
  REGISTER INTEGER FORWARD + END CONSTANT
  END value non in registro, ma possiamo usare immediate comparison
  ====================================================================  }

procedure TRegIntForwardConstStrategy.EmitConditionCheck(
  Compiler: TForLoopCompiler; const Ctx: TForLoopContext);
var
  TempReg: Integer;
begin
  {
    Opzione 1: Allocare temp register per END
    Opzione 2: Usare opCmpRegImmInt (fused comparison)

    Per semplicità, alloca temp register
  }
  TempReg := Compiler.AllocateTempRegister(vtInteger);
  if TempReg >= 0 then
  begin
    // Load costante in temp register
    // (assumiamo che Ctx.EndReg contenga l'indice della costante se HasConstEnd=True)
    Compiler.EmitWithInt(opPushConst, Ctx.EndReg);  // EndReg usato come const index!
    Compiler.EmitStoreReg(TempReg);

    // Compare usando dispatch table
    EmitComparisonOpcode(Compiler, Ctx, Ctx.RegIdx, TempReg);
    Compiler.EmitJump(opPopJumpIfFalse, Ctx.BreakLabel);

    Compiler.FreeTempRegister(TempReg);
  end
  else
  begin
    // No temp register: fallback a stack-based
    Compiler.EmitLoadReg(Ctx.RegIdx);
    Compiler.EmitWithInt(opPushConst, Ctx.EndReg);
    Compiler.Emit(opLeInt);  // Assume forward iteration
    Compiler.EmitJump(opPopJumpIfFalse, Ctx.BreakLabel);
  end;
end;

procedure TRegIntForwardConstStrategy.EmitIncrement(
  Compiler: TForLoopCompiler; const Ctx: TForLoopContext);
begin
  // Stesso di TRegIntForwardEndRegStrategy
  if Ctx.StepConst >= 0 then
    Compiler.EmitWithTwoArgs(opAddRegImm, Ctx.RegIdx, Ctx.StepConst)
  else
    Compiler.EmitWithInt(opIncRegInt, Ctx.RegIdx);
end;

{ ====================================================================
  REGISTER FLOAT STRATEGIES
  Simili a Integer, ma usano opcode Float
  ====================================================================  }

procedure TRegFloatForwardEndRegStrategy.EmitConditionCheck(
  Compiler: TForLoopCompiler; const Ctx: TForLoopContext);
begin
  EmitComparisonOpcode(Compiler, Ctx, Ctx.RegIdx, Ctx.EndReg);
  Compiler.EmitJump(opPopJumpIfFalse, Ctx.BreakLabel);
end;

procedure TRegFloatForwardEndRegStrategy.EmitIncrement(
  Compiler: TForLoopCompiler; const Ctx: TForLoopContext);
begin
  if Ctx.StepConst >= 0 then
    Compiler.EmitWithTwoArgs(opAddRegImm, Ctx.RegIdx, Ctx.StepConst)
  else
    Compiler.EmitWithInt(opIncRegFloat, Ctx.RegIdx);
end;

procedure TRegFloatForwardConstStrategy.EmitConditionCheck(
  Compiler: TForLoopCompiler; const Ctx: TForLoopContext);
var
  TempReg: Integer;
begin
  TempReg := Compiler.AllocateTempRegister(vtFloat);
  if TempReg >= 0 then
  begin
    Compiler.EmitWithInt(opPushConst, Ctx.EndReg);
    Compiler.EmitStoreReg(TempReg);
    EmitComparisonOpcode(Compiler, Ctx, Ctx.RegIdx, TempReg);
    Compiler.EmitJump(opPopJumpIfFalse, Ctx.BreakLabel);
    Compiler.FreeTempRegister(TempReg);
  end
  else
  begin
    Compiler.EmitLoadReg(Ctx.RegIdx);
    Compiler.EmitWithInt(opPushConst, Ctx.EndReg);
    Compiler.Emit(opLeFloat);
    Compiler.EmitJump(opPopJumpIfFalse, Ctx.BreakLabel);
  end;
end;

procedure TRegFloatForwardConstStrategy.EmitIncrement(
  Compiler: TForLoopCompiler; const Ctx: TForLoopContext);
begin
  if Ctx.StepConst >= 0 then
    Compiler.EmitWithTwoArgs(opAddRegImm, Ctx.RegIdx, Ctx.StepConst)
  else
    Compiler.EmitWithInt(opIncRegFloat, Ctx.RegIdx);
end;

{ ====================================================================
  STACK-BASED STRATEGY (fallback generico)
  ====================================================================  }

procedure TStackBasedForwardStrategy.EmitConditionCheck(
  Compiler: TForLoopCompiler; const Ctx: TForLoopContext);
begin
  // Fallback: usa variabili e stack
  // Emette: LoadVar, PushConst, Le, PopJumpIfFalse
  Compiler.EmitWithInt(opLoadVarInt, Ctx.VarIdx);
  Compiler.EmitWithInt(opPushConst, Ctx.EndReg); // EndReg = const index se stack path

  if Ctx.VarType = vtFloat then
    Compiler.Emit(opLeFloat)
  else
    Compiler.Emit(opLeInt);

  Compiler.EmitJump(opPopJumpIfFalse, Ctx.BreakLabel);
end;

procedure TStackBasedForwardStrategy.EmitIncrement(
  Compiler: TForLoopCompiler; const Ctx: TForLoopContext);
begin
  // Fallback: use opIncVar or load/increment/save
  if Ctx.StepConst >= 0 then
  begin
    // Constant STEP = 1: use opIncVar
    // TODO: Handle STEP != 1
    if Ctx.VarType = vtInteger then
      Compiler.EmitWithInt(opIncVarInt, Ctx.VarIdx)
    else
      Compiler.EmitWithInt(opIncVarFloat, Ctx.VarIdx);
  end
  else
  begin
    // Variable STEP: load var, load step, add, save
    // Complex implementation, TODO
    Compiler.EmitWithInt(opIncVarInt, Ctx.VarIdx);  // Temporary
  end;
end;

end.
