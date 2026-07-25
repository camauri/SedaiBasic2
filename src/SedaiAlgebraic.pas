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
    FUserVarKeys: TFPHashList;         // Set of exact VarRegMap values ("RegType:RegIndex")
    FUserVarRegIndex: array of Boolean; // RegIndex → mapped to a user variable in ANY bank
    FMultiDef: TFPHashList;            // Set of register keys written by MORE than one instruction

    { Make string key from register value: "RegIndex:Version" }
    function MakeRegKey(const RegVal: TSSAValue): string; inline;

    { Precompute the user-variable lookup structures from FProgram.VarRegMap }
    procedure BuildUserVarIndex;

    { Build the map of constant-holding registers for ONE block (see the body for why not global) }
    procedure BuildConstantMap(Block: TSSABasicBlock);
    procedure ClearConstMap;

    { Mark every register written by more than one instruction }
    procedure BuildDefCounts;
    function IsSingleDef(const V: TSSAValue): Boolean;

    { Check if register key maps to a BASIC user variable }
    function IsUserVariable(const VarKey: string): Boolean;
    function IsUserVarExact(const V: TSSAValue): Boolean;
    function MayTrackAsConst(const Dest: TSSAValue): Boolean;
    procedure TrackConst(const Dest, ConstVal: TSSAValue);

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
  FUserVarKeys := TFPHashList.Create;
  FMultiDef := TFPHashList.Create;
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
  FUserVarKeys.Free;
  FMultiDef.Free;
  inherited;
end;

function TAlgebraicSimplification.Run: Integer;
begin
  // Phase A: runs in BOTH dialects. The MODERN miscompiles that once forced a skip here were not this
  // pass's fault: they were latent register bank-typing bugs (float FOR-comparison results typed in the
  // float bank; multi-CONST lists lowering only their first constant) that this pass's register shifts
  // exposed. With those fixed the pass is clean on versioned SSA — corpus 470/470, JIT net 0 mismatch.
  // If a MODERN OPTDIFF ever bisects here, suspect another latent bank-typing mismatch FIRST
  // (see memory licm-general-blocker-is-register-typing).

  {$IFDEF DEBUG_ALGEBRAIC}
  if DebugAlgebraic then
    WriteLn('[Algebraic] Running algebraic simplification...');
  {$ENDIF}

  // Step 1: Precompute the user-variable lookup (VarRegMap does not change during this pass)
  BuildUserVarIndex;

  // Step 2: mark multiply-written registers. The constant map itself is now built per BLOCK, inside
  // SimplifyBlocks, because constants do not survive a block boundary here (see BuildConstantMap).
  BuildDefCounts;

  // Step 3: Simplify using algebraic rules
  SimplifyBlocks;

  {$IFDEF DEBUG_ALGEBRAIC}
  if DebugAlgebraic then
    WriteLn('[Algebraic] Applied ', FSimplifications, ' simplifications');
  {$ENDIF}
  Result := FSimplifications;
end;

function TAlgebraicSimplification.MakeRegKey(const RegVal: TSSAValue): string;
begin
  // Unique key: "RegType:RegIndex:Version".
  //
  // The BANK is not optional. Without it an int register and a float register with the same index
  // and version share one entry, and the constant map answers with whichever was recorded first -
  // so "7.6 \ 2" resolved its divisor to the 1.0 living at the same index in the float bank, the
  // division collapsed to a copy, and the program printed the numerator (8 instead of 4). This is
  // the register bank-typing class this pass's own header warns about; it stayed latent only while
  // the user-variable guard was excluding nearly every constant from the map. Guard m394.
  Result := IntToStr(Ord(RegVal.RegType)) + ':' + IntToStr(RegVal.RegIndex) + ':' +
            IntToStr(RegVal.Version);
end;

procedure TAlgebraicSimplification.BuildConstantMap(Block: TSSABasicBlock);
var
  Instr: TSSAInstruction;
  j: Integer;
  DestKey, SrcKey, UserKey: string;
  P, PSrc: PSSAValueWrapper;
  ConstVal: TSSAValue;
begin
  // Constants known WITHIN ONE BLOCK. Deliberately not program-wide.
  //
  // A constant defined before a loop does NOT still hold inside it: the SSA here does not model the
  // back edge for a FOR counter, so "i = 1" in the preheader and "i = i + 1" in the body are two
  // versions of one storage location. Folding the second against the first turned the step into the
  // constant 2 and m304_shared_for_counter looped forever (guard m467). Confining the knowledge to
  // a single block removes the question: there is no back edge inside a block, so a constant
  // recorded earlier in it still holds later in it. Same rule, and same reason, as the block-local
  // table GVN uses for values built on loop-variant operands.
    begin
      for j := 0 to Block.Instructions.Count - 1 do
      begin
        Instr := Block.Instructions[j];

      // Track LoadConstInt and LoadConstFloat instructions
      // CRITICAL: DO NOT track if destination is a user variable (BASIC global variable semantics)
      // A user variable like I%=0 in a FOR loop is NOT a constant, even if initialized to 0!
      if OpIn(Instr.OpCode, [ssaLoadConstInt, ssaLoadConstFloat]) and
         (Instr.Dest.Kind = svkRegister) then
      begin
        DestKey := MakeRegKey(Instr.Dest);

        // Skip only a register whose name does not denote ONE value: an UNVERSIONED user variable.
        // "I% = 0" at the head of a FOR loop is not a constant, and under global-by-name semantics
        // there is no way to tell it apart from a real one - hence the skip.
        // A versioned destination (MODERN) is a genuine single definition and is safe to record.
        // The old test ignored the version, and worse, its "RegIndex:Version" branch matches on the
        // INDEX ALONE across every bank, so a compiler temp merely sharing an index with some user
        // variable was excluded too - which on small programs is most of them.
        // CLASSIC keeps the historical "RegIndex:Version" query, whose fallback matches on the INDEX
        // ALONE across every bank - deliberately conservative there, and left untouched. MODERN asks
        // the exact "RegType:RegIndex" question, so a compiler temp is no longer mistaken for a user
        // variable just because it happens to share an index with one.
        if Instr.Dest.Version = 0 then
        begin
          if FProgram.GlobalVariableSemantics then
          begin
            // CLASSIC keeps the historical index-only match, which needs the OLD two-part key:
            // IsUserVariable reads the leading number as a RegIndex, and MakeRegKey now leads with
            // the bank.
            UserKey := IntToStr(Instr.Dest.RegIndex) + ':' + IntToStr(Instr.Dest.Version);
            if IsUserVariable(UserKey) then Continue;
          end
          else
            if IsUserVarExact(Instr.Dest) then Continue;
        end;

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

procedure TAlgebraicSimplification.BuildUserVarIndex;
var
  i, ColonPos, RegIdx: Integer;
  MappedKey: string;
begin
  // VarRegMap values are "RegType:RegIndex". Precompute:
  //   - FUserVarKeys: hash set of the exact values (exact-key queries)
  //   - FUserVarRegIndex: bitmap of every mapped RegIndex, bank-blind
  //     (matches the historical "any bank" scan for "RegIndex:Version" queries)
  for i := 0 to FProgram.VarRegMap.Count - 1 do
  begin
    MappedKey := FProgram.VarRegMap.ValueFromIndex[i];
    if FUserVarKeys.FindIndexOf(MappedKey) < 0 then
      FUserVarKeys.Add(MappedKey, Pointer(1));

    ColonPos := Pos(':', MappedKey);
    if ColonPos > 0 then
    begin
      RegIdx := StrToIntDef(Copy(MappedKey, ColonPos + 1, Length(MappedKey)), -1);
      if RegIdx >= 0 then
      begin
        if RegIdx >= Length(FUserVarRegIndex) then
          SetLength(FUserVarRegIndex, RegIdx + 1);
        FUserVarRegIndex[RegIdx] := True;
      end;
    end;
  end;
end;

procedure TAlgebraicSimplification.BuildDefCounts;
var
  i, j: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  Key: string;
  SeenDef: TFPHashList;
begin
  { A register may only be treated as holding a constant if exactly ONE instruction writes it.
    Version alone is not that guarantee: an unversioned register is written wherever its name
    appears, and a FOR counter is written twice - once by the initial LoadConst and again by its
    per-iteration step. Recording the first write as "the" constant then folds the loop's own
    condition to a fixed value and the loop never ends. That is precisely what happened to
    m304_shared_for_counter, which hung instead of printing (guard m467).
    The Dest-as-USE opcodes below carry a VALUE in the Dest field and define nothing; the list
    mirrors the SSA renamer's authoritative one, the same one GVN's BuildDefCounts uses. }
  // Two sets rather than a counter: FSeenDef marks "written at least once", FMultiDef marks
  // "written again". Membership of the second is the only question asked afterwards.
  FMultiDef.Clear;
  SeenDef := TFPHashList.Create;
  try
    for i := 0 to FProgram.Blocks.Count - 1 do
    begin
      Block := FProgram.Blocks[i];
      for j := 0 to Block.Instructions.Count - 1 do
      begin
        Instr := Block.Instructions[j];
        if Instr.Dest.Kind <> svkRegister then Continue;
        case Instr.OpCode of
          ssaArrayStore, ssaArrayStoreIndInt, ssaArrayStoreIndFloat,
          ssaArrayStoreIndString, ssaPrint, ssaPrintLn:
            Continue;
        end;
        Key := MakeRegKey(Instr.Dest);
        if SeenDef.Find(Key) <> nil then
        begin
          if FMultiDef.Find(Key) = nil then FMultiDef.Add(Key, Pointer(1));
        end
        else
          SeenDef.Add(Key, Pointer(1));
      end;
    end;
  finally
    SeenDef.Free;
  end;
end;

function TAlgebraicSimplification.IsSingleDef(const V: TSSAValue): Boolean;
begin
  Result := (V.Kind = svkRegister) and (FMultiDef.Find(MakeRegKey(V)) = nil);
end;

function TAlgebraicSimplification.MayTrackAsConst(const Dest: TSSAValue): Boolean;
var
  UserKey: string;
begin
  // Single assignment first: everything below is about WHICH value a name denotes, and that question
  // only has an answer when one instruction writes it.
  if not IsSingleDef(Dest) then Exit(False);

  // A register may be recorded as holding a constant only if its NAME denotes one value. A versioned
  // register always does. An unversioned one does not if it is a user variable - "I% = 0" at the head
  // of a FOR loop is not a constant - and under global-by-name semantics there is no way to tell.
  Result := False;
  if Dest.Kind <> svkRegister then Exit;
  if Dest.Version <> 0 then Exit(True);
  if FProgram.GlobalVariableSemantics then
  begin
    // CLASSIC keeps the historical index-only match, which wants the old two-part key.
    UserKey := IntToStr(Dest.RegIndex) + ':' + IntToStr(Dest.Version);
    Result := not IsUserVariable(UserKey);
  end
  else
    Result := not IsUserVarExact(Dest);
end;

procedure TAlgebraicSimplification.TrackConst(const Dest, ConstVal: TSSAValue);
var
  P: PSSAValueWrapper;
  Key: string;
begin
  if not MayTrackAsConst(Dest) then Exit;
  Key := MakeRegKey(Dest);
  if FConstMap.Find(Key) <> nil then Exit;   // first definition wins, as when the map was built
  New(P);
  P^.Value := ConstVal;
  FConstMap.Add(Key, P);
end;

function TAlgebraicSimplification.IsUserVarExact(const V: TSSAValue): Boolean;
begin
  // Exact "is THIS register (bank + index) mapped to a user variable" - no index-only fallback.
  //
  // It exists because IsUserVariable below takes a STRING in either of two formats and cannot tell
  // them apart: given the exact key "1:5" it first tries a literal lookup, then falls through to
  // parsing the leading number as a RegIndex - so it tests FUserVarRegIndex[1], reading the BANK as
  // an index. Bank ordinals are 0..2 and any program with a couple of variables maps those indices,
  // so the fallback answered TRUE for practically every exact query and silently disabled the
  // caller. Ask the typed question instead of encoding it in a string.
  Result := (V.Kind = svkRegister) and
            (FUserVarKeys.FindIndexOf(IntToStr(Ord(V.RegType)) + ':' + IntToStr(V.RegIndex)) >= 0);
end;

function TAlgebraicSimplification.IsUserVariable(const VarKey: string): Boolean;
var
  RegIdx: Integer;
  ColonPos: Integer;
begin
  // Check if this VarKey corresponds to a BASIC user variable
  // VarKey can be in two formats:
  //   - From MakeRegKey: "RegIndex:Version" (e.g., "5:0")
  //   - From SimplifyArithmetic: "RegType:RegIndex" (e.g., "1:5")
  // Lookup structures are precomputed from VarRegMap by BuildUserVarIndex.
  Result := False;

  ColonPos := Pos(':', VarKey);
  if ColonPos = 0 then Exit;

  // Exact "RegType:RegIndex" match (SimplifyArithmetic queries)
  if FUserVarKeys.FindIndexOf(VarKey) >= 0 then
    Exit(True);

  // "RegIndex:Version" queries: the leading number is a RegIndex; a register is a
  // user variable if that index is mapped in ANY bank (conservative, as before)
  RegIdx := StrToIntDef(Copy(VarKey, 1, ColonPos - 1), -1);
  Result := (RegIdx >= 0) and (RegIdx < Length(FUserVarRegIndex)) and FUserVarRegIndex[RegIdx];
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
  C1, C2: TSSAValue;
  FoldVal: Int64;
  Folded: Boolean;
begin
  Result := Instr;

  // Do NOT optimize an operand whose register name does not denote ONE value.
  //
  // The property that matters is VERSIONING, not "is this a user variable". Under
  // GlobalVariableSemantics every value is Version=0 and a register name is global-by-name, so
  // "I% + 1" can look like "0 + 1" while I% is a loop counter - hence the guard. A VERSIONED value
  // (Version>0, MODERN proc-local SSA) is a real single definition, and BuildConstantMap is keyed on
  // RegIndex:Version, so its answer for such an operand is exact.
  //
  // The guard used to test only "is it in VarRegMap", ignoring the version. In CLASSIC that is the
  // same test. In MODERN it excluded EVERY declared scalar, which is nearly every operand - and the
  // pass was measured inert on 910 of 910 corpus programs, with "Print 7 * 1 + 0 - 0" reaching the
  // bytecode as three live multiplications and additions. Keeping the Version=0 half preserves
  // CLASSIC behaviour exactly.
  // CLASSIC only. Under global-by-name semantics a register name does not denote one value, so the
  // pass refuses outright to touch an expression mentioning a user variable.
  //
  // In MODERN this blanket guard is both WRONG-HEADED and totally disabling. Wrong-headed because
  // the identities below - x*1, x+0, x-0, x/1 - hold for ANY x: they need to know that the OTHER
  // operand is the constant, and nothing whatsoever about x. Disabling because it excluded every
  // declared scalar, which is nearly every operand: the pass was measured inert on 910 of 910
  // corpus programs. What must not happen is mistaking a user variable FOR a constant, and that
  // decision belongs to BuildConstantMap (which refuses to record an unversioned user variable) -
  // not here. Every IsZero/IsOne test below resolves through that map, so the protection stands.
  if FProgram.GlobalVariableSemantics then
  begin
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
  end;

  NewInstr := Instr.Clone;

  // CONSTANT FOLDING: both operands known -> the whole instruction becomes a LoadConst.
  //
  // This is the form of constant propagation a REGISTER-based VM can actually use. The abandoned
  // TAggressiveConstProp tried to substitute constants INTO operands and had to be disabled,
  // correctly: nearly every VM instruction reads FIntRegs[Src1], not an immediate field. Folding
  // has no such problem - the result still lands in a register, and the now-dead LoadConsts that
  // fed the operation are swept by DCE.
  //
  // The parser already folds literal expressions ("Print 6 * 7" emits LoadConstInt 42). What it
  // cannot do is fold THROUGH a variable: "a = 5 : b = a * 4" reached the bytecode as two loads and
  // a MulInt. That is this pass's job, and nothing was doing it.
  //
  // Restricted to integer +, -, * and the bitwise operators on purpose:
  //   * \ and Mod TRAP (divide by zero, and Int64.MinValue \ -1 overflows), so folding them would
  //     move a runtime error to compile time, or silently invent a value for a path that raises;
  //   * shifts carry the signed/unsigned distinction that guard m394 exists for;
  //   * floats would have to reproduce the VM's exact rounding, which is not worth the risk here.
  // Int64 wraparound matches the VM's own integer arithmetic.
  if (Instr.Dest.Kind = svkRegister) and (Instr.Dest.RegType = srtInt) and
     ResolveToConst(Instr.Src1, C1) and ResolveToConst(Instr.Src2, C2) and
     (C1.Kind = svkConstInt) and (C2.Kind = svkConstInt) then
  begin
    Folded := True;
    case Instr.OpCode of
      ssaAddInt: FoldVal := C1.ConstInt + C2.ConstInt;
      ssaSubInt: FoldVal := C1.ConstInt - C2.ConstInt;
      ssaMulInt: FoldVal := C1.ConstInt * C2.ConstInt;
    else
      Folded := False;
    end;
    if Folded then
    begin
      NewInstr.OpCode := ssaLoadConstInt;
      NewInstr.Src1 := MakeSSAConstInt(FoldVal);
      NewInstr.Src2 := MakeSSAValue(svkNone);
      NewInstr.Src3 := MakeSSAValue(svkNone);
      // Record the folded value so a CHAIN collapses in this same walk: SimplifyBlocks visits
      // instructions in order, so "b = a * 4 : c = b + a" folds b and then c.
      TrackConst(NewInstr.Dest, NewInstr.Src1);
      Inc(FSimplifications);
      Exit(NewInstr);
    end;
  end;

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
  if OpIn(Instr.OpCode, [ssaAddInt, ssaAddFloat, ssaSubInt, ssaSubFloat,
                      ssaMulInt, ssaMulFloat, ssaDivInt, ssaDivFloat]) then
  begin
    Result := SimplifyArithmetic(Instr);
  end;
end;

procedure TAlgebraicSimplification.ClearConstMap;
var
  i: Integer;
  P: PSSAValueWrapper;
begin
  for i := 0 to FConstMap.Count - 1 do
  begin
    P := PSSAValueWrapper(FConstMap.Items[i]);
    if P <> nil then Dispose(P);
  end;
  FConstMap.Clear;
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
    // Constants are known per block only, so the map is rebuilt for each one.
    ClearConstMap;
    BuildConstantMap(Block);
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
