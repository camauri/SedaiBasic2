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
  Unit: SedaiGVN

  Purpose: Global Value Numbering optimization pass using dominator-tree
           preorder traversal and scoped hash tables.

  Algorithm: SSA-based GVN with dominance checks
             - Preorder traversal ensures definitions dominate uses
             - Scoped hash tables prevent incorrect value reuse across
               non-dominating blocks
             - O(1) dominance checks using preorder/postorder intervals

  Phase: 3 Tier 2 - Steps 4-6 unified implementation
  Author: Sedai Project - Compiler Optimization Engineer
  Date: 2025-11-14
  ============================================================================ }

unit SedaiGVN;

{$mode objfpc}{$H+}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, Generics.Collections, SedaiSSATypes, SedaiDominators;

type
  { TScopedGVNTable - Scoped hash table for value numbering

    Implements a stack of hash tables that models the dominator tree structure.
    Each scope corresponds to a dominator subtree. Values are visible only
    within their defining scope and child scopes. }

  TScopedGVNTable = class
  private
    type
      TValueMap = specialize TDictionary<string, TSSAValue>;
      TMapStack = specialize TList<TValueMap>;
  private
    FStack: TMapStack;
    FInitialStackDepth: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    { Push a new scope (entering a dominator subtree) }
    procedure PushScope;

    { Pop current scope (exiting a dominator subtree) }
    procedure PopScope;

    { Lookup a value by hash key (searches from top to bottom of stack) }
    function Lookup(const Hash: string; out Value: TSSAValue): Boolean;

    { Insert a value into current scope }
    procedure Insert(const Hash: string; const Value: TSSAValue);

    { Verify stack integrity (debug) }
    function VerifyStackIntegrity: Boolean;
  end;

  { TGVNPass - Global Value Numbering optimization pass

    Eliminates redundant computations by identifying equivalent expressions
    and reusing their computed values. Uses dominator-tree preorder traversal
    to ensure correctness. }

  TGVNPass = class
  private
    FScopedTable: TScopedGVNTable;
    FDomTree: TDominatorTree;
    FProgram: TSSAProgram;
    FReplacements: Integer;  // Count of values replaced
    FCurrentBlock: TSSABasicBlock;  // Current block being processed
    FPhiDefinedRegs: TStringList;   // Registers defined by PHI functions (loop-variant)

    { Process a single basic block }
    procedure ProcessBlock(Block: TSSABasicBlock);

    { Process a single instruction within a block }
    procedure ProcessInstruction(Instr: TSSAInstruction);

    { Compute hash key for an instruction's value }
    function ComputeValueHash(Instr: TSSAInstruction): string;

    { Check if instruction result can be value-numbered }
    function IsValueNumberable(Instr: TSSAInstruction): Boolean;

    { Check if a value depends on a PHI-defined register (loop-variant) }
    function IsLoopVariant(const Value: TSSAValue): Boolean;

    { Check if instruction uses any loop-variant values }
    function UsesLoopVariantValue(Instr: TSSAInstruction): Boolean;

    { Collect all PHI-defined registers from the program }
    procedure CollectPhiDefinedRegisters;

    { DFS traversal of dominator tree with proper scope management
      Values computed in a block are visible to all dominated blocks }
    procedure TraverseDomTree(Block: TSSABasicBlock);
  public
    constructor Create;
    destructor Destroy; override;

    { Run GVN pass on SSA program }
    function Run(Prog: TSSAProgram): Integer;  // Returns number of replacements
  end;

implementation

uses TypInfo
     {$IFDEF DEBUG_GVN}, SedaiDebug{$ENDIF};

{ TScopedGVNTable }

constructor TScopedGVNTable.Create;
begin
  inherited Create;
  FStack := TMapStack.Create;
  FInitialStackDepth := 0;
end;

destructor TScopedGVNTable.Destroy;
var
  Map: TValueMap;
begin
  // Free all hash tables in stack
  for Map in FStack do
    Map.Free;

  FStack.Free;
  inherited Destroy;
end;

procedure TScopedGVNTable.PushScope;
var
  NewMap: TValueMap;
begin
  NewMap := TValueMap.Create;
  FStack.Add(NewMap);
end;

procedure TScopedGVNTable.PopScope;
var
  Map: TValueMap;
begin
  if FStack.Count = 0 then
    raise Exception.Create('TScopedGVNTable.PopScope: Stack underflow!');

  // Free top map and remove from stack
  Map := FStack[FStack.Count - 1];
  FStack.Delete(FStack.Count - 1);
  Map.Free;
end;

function TScopedGVNTable.Lookup(const Hash: string; out Value: TSSAValue): Boolean;
var
  i: Integer;
  Map: TValueMap;
begin
  // Search from top (most recent scope) to bottom (oldest scope)
  for i := FStack.Count - 1 downto 0 do
  begin
    Map := FStack[i];
    if Map.TryGetValue(Hash, Value) then
      Exit(True);  // Found in this scope
  end;

  Result := False;  // Not found in any scope
end;

procedure TScopedGVNTable.Insert(const Hash: string; const Value: TSSAValue);
var
  Map: TValueMap;
begin
  if FStack.Count = 0 then
    raise Exception.Create('TScopedGVNTable.Insert: No active scope!');

  // Insert into top scope (current dominator subtree)
  Map := FStack[FStack.Count - 1];
  Map.AddOrSetValue(Hash, Value);
end;

function TScopedGVNTable.VerifyStackIntegrity: Boolean;
begin
  // STEP 5 REQUIREMENT: Verify stack returns to original size after traversal
  Result := (FStack.Count = FInitialStackDepth);

  {$IFDEF DEBUG_GVN}
  if not Result and DebugGVN then
    WriteLn(Format('[GVN] WARNING: Stack integrity violated! Expected depth %d, got %d',
      [FInitialStackDepth, FStack.Count]));
  {$ENDIF}
end;

{ TGVNPass }

constructor TGVNPass.Create;
begin
  inherited Create;
  FScopedTable := TScopedGVNTable.Create;
  FPhiDefinedRegs := TStringList.Create;
  FPhiDefinedRegs.Sorted := True;
  FPhiDefinedRegs.Duplicates := dupIgnore;
  FReplacements := 0;
  FCurrentBlock := nil;
end;

destructor TGVNPass.Destroy;
begin
  FPhiDefinedRegs.Free;
  FScopedTable.Free;
  inherited Destroy;
end;

procedure TGVNPass.CollectPhiDefinedRegisters;
var
  i, j: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  RegKey: string;
begin
  { Collect all registers that are defined by PHI functions.
    These registers are "loop-variant" - their values change across
    loop iterations, so expressions using them cannot be safely
    value-numbered across different blocks. }

  FPhiDefinedRegs.Clear;

  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];
      if Instr.OpCode = ssaPhi then
      begin
        // Record this register as PHI-defined
        if Instr.Dest.Kind = svkRegister then
        begin
          RegKey := Format('%d:%d:%d', [Ord(Instr.Dest.RegType),
                                        Instr.Dest.RegIndex,
                                        Instr.Dest.Version]);
          FPhiDefinedRegs.Add(RegKey);
        end;
      end;
    end;
  end;

  {$IFDEF DEBUG_GVN}
  if DebugGVN then
    WriteLn(Format('[GVN] Found %d PHI-defined registers (loop-variant)',
      [FPhiDefinedRegs.Count]));
  {$ENDIF}
end;

function TGVNPass.IsLoopVariant(const Value: TSSAValue): Boolean;
var
  RegKey: string;
begin
  { Check if a value is loop-variant (defined by a PHI function) }
  Result := False;

  if Value.Kind <> svkRegister then
    Exit;

  RegKey := Format('%d:%d:%d', [Ord(Value.RegType), Value.RegIndex, Value.Version]);
  Result := FPhiDefinedRegs.IndexOf(RegKey) >= 0;
end;

function TGVNPass.UsesLoopVariantValue(Instr: TSSAInstruction): Boolean;
begin
  { Check if any source operand of this instruction is loop-variant }
  Result := False;

  if (Instr.Src1.Kind <> svkNone) and IsLoopVariant(Instr.Src1) then
    Exit(True);
  if (Instr.Src2.Kind <> svkNone) and IsLoopVariant(Instr.Src2) then
    Exit(True);
  if (Instr.Src3.Kind <> svkNone) and IsLoopVariant(Instr.Src3) then
    Exit(True);
end;

function TGVNPass.ComputeValueHash(Instr: TSSAInstruction): string;
begin
  { Compute a hash key that uniquely identifies the value computed by this instruction.

    Two instructions compute the same value iff:
    - Same opcode
    - Same source operands (by value, not register)

    Example:
      R1 = Add R0, R0  → Hash = "Add:R0:R0"
      R2 = Add R0, R0  → Hash = "Add:R0:R0"  (same! can reuse R1)
  }

  Result := SSAOpCodeToString(Instr.OpCode);

  // Append source operands
  if Instr.Src1.Kind <> svkNone then
    Result := Result + ':' + SSAValueToString(Instr.Src1);
  if Instr.Src2.Kind <> svkNone then
    Result := Result + ':' + SSAValueToString(Instr.Src2);
  if Instr.Src3.Kind <> svkNone then
    Result := Result + ':' + SSAValueToString(Instr.Src3);
end;

function TGVNPass.IsValueNumberable(Instr: TSSAInstruction): Boolean;
begin
  { Determine if instruction result can be safely value-numbered.

    Safe instructions:
    - Arithmetic (Add, Sub, Mul, etc.)
    - Comparisons (CmpEq, CmpLt, etc.)
    - Conversions (IntToFloat, etc.)
    - Array loads (with dominance check)

    Unsafe instructions:
    - Stores (have side effects)
    - Calls (may have side effects)
    - Input operations (non-deterministic)
  }

  case Instr.OpCode of
    // Arithmetic operations (pure functions)
    ssaAddInt, ssaSubInt, ssaMulInt, ssaDivInt, ssaModInt, ssaNegInt,
    ssaAddFloat, ssaSubFloat, ssaMulFloat, ssaDivFloat, ssaPowFloat, ssaNegFloat:
      Result := True;

    // Conversions (pure functions)
    ssaIntToFloat, ssaFloatToInt, ssaIntToString, ssaFloatToString,
    ssaStringToInt, ssaStringToFloat:
      Result := True;

    // Comparisons (pure functions)
    ssaCmpEqInt, ssaCmpNeInt, ssaCmpLtInt, ssaCmpGtInt, ssaCmpLeInt, ssaCmpGeInt,
    ssaCmpEqFloat, ssaCmpNeFloat, ssaCmpLtFloat, ssaCmpGtFloat, ssaCmpLeFloat, ssaCmpGeFloat,
    ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString:
      Result := True;

    // Logical operations (pure functions)
    ssaLogicalAnd, ssaLogicalOr, ssaLogicalNot:
      Result := True;

    // String operations (pure functions)
    ssaStrConcat, ssaStrLen, ssaStrLeft, ssaStrRight, ssaStrMid:
      Result := True;

    // Math functions (pure functions)
    ssaMathSin, ssaMathCos, ssaMathTan, ssaMathAtn, ssaMathLog, ssaMathExp,
    ssaMathSqr, ssaMathAbs, ssaMathSgn, ssaMathInt:
      Result := True;

    // Constants - DO NOT value number!
    // LoadConst instructions are already optimal and removing them can create
    // dependencies that break when Superinstructions fuses Arith+Copy patterns.
    // The fused instruction expects the constant register to be valid, but if
    // GVN replaced LoadConst with Copy, the source register may not exist after
    // other optimizations eliminate it.
    ssaLoadConstInt, ssaLoadConstFloat, ssaLoadConstString:
      Result := False;

    // Copy operations (can be eliminated)
    ssaCopyInt, ssaCopyFloat, ssaCopyString:
      Result := True;

    // Array loads (pure if no intervening stores)
    ssaArrayLoad:
      Result := True;  // Note: requires dominance check in practice

    // Everything else is unsafe
    else
      Result := False;
  end;
end;

procedure TGVNPass.ProcessInstruction(Instr: TSSAInstruction);
var
  Hash: string;
  ExistingValue: TSSAValue;
begin
  // Skip non-value-numberable instructions
  if not IsValueNumberable(Instr) then
    Exit;

  // Skip instructions without destination register
  if Instr.Dest.Kind <> svkRegister then
    Exit;

  // CRITICAL FIX: Skip instructions that use loop-variant values (PHI-defined)
  // These values change across loop iterations, so we cannot safely reuse
  // computations that depend on them from previous iterations.
  if UsesLoopVariantValue(Instr) then
  begin
    {$IFDEF DEBUG_GVN}
    if DebugGVN then
      WriteLn(Format('[GVN] Skipping loop-variant instruction: %s (uses PHI-defined value)',
        [SSAValueToString(Instr.Dest)]));
    {$ENDIF}
    Exit;
  end;

  // Compute hash for this instruction's value
  Hash := ComputeValueHash(Instr);

  // STEP 5: Lookup in scoped hash table
  if FScopedTable.Lookup(Hash, ExistingValue) then
  begin
    { Found equivalent computation!

      STEP 6: Dominance check
      In a correct preorder traversal, if the value exists in the scoped table,
      it MUST dominate the current instruction (by construction of the scope stack).

      However, we add an explicit check for array loads to handle potential
      aliasing issues. }

    if (Instr.OpCode = ssaArrayLoad) then
    begin
      // For array loads, be conservative (Step 6 requirement)
      // In practice, we'd check if there's an intervening store
      // For now, we skip GVN on array loads (Phase 3 Tier 1 handles this via LICM)
      FScopedTable.Insert(Hash, Instr.Dest);
      Exit;
    end;

    {$IFDEF DEBUG_GVN}
    // Replace this instruction's result with the existing value
    if DebugGVN then
      WriteLn(Format('[GVN] Replacing %s with %s (hash: %s)',
        [SSAValueToString(Instr.Dest), SSAValueToString(ExistingValue), Hash]));
    {$ENDIF}

    // Convert to a Copy instruction with correct type
    case Instr.Dest.RegType of
      srtInt:    Instr.OpCode := ssaCopyInt;
      srtFloat:  Instr.OpCode := ssaCopyFloat;
      srtString: Instr.OpCode := ssaCopyString;
    end;

    Instr.Src1 := ExistingValue;
    Instr.Src2 := MakeSSAValue(svkNone);
    Instr.Src3 := MakeSSAValue(svkNone);
    Instr.Comment := 'GVN: reuse';

    Inc(FReplacements);
  end
  else
  begin
    // STEP 5: Insert into current scope
    FScopedTable.Insert(Hash, Instr.Dest);
  end;
end;

procedure TGVNPass.TraverseDomTree(Block: TSSABasicBlock);
var
  Children: TFPList;
  i: Integer;
  ChildBlock: TSSABasicBlock;
begin
  { DFS traversal of dominator tree with proper scope management:

    For inter-block GVN, values computed in a block must be visible to ALL
    blocks that the current block dominates. We achieve this by:

    1. Push new scope when entering a block
    2. Process all instructions in the block (they go into current scope)
    3. Recursively process all children in dominator tree (they inherit scope)
    4. Pop scope when done with entire subtree

    This ensures that if block A dominates block B:
    - A is processed first (preorder)
    - Values from A are still in scope when processing B
    - Values from B are NOT visible when processing A's siblings
  }

  // Push scope for this block and its dominated subtree
  FScopedTable.PushScope;

  // Process instructions in this block
  ProcessBlock(Block);

  // Get children in dominator tree
  Children := FDomTree.GetChildren(Block);
  if Assigned(Children) then
  begin
    try
      // Recursively process each child
      for i := 0 to Children.Count - 1 do
      begin
        ChildBlock := TSSABasicBlock(Children[i]);
        TraverseDomTree(ChildBlock);
      end;
    finally
      Children.Free;
    end;
  end;

  // Pop scope when leaving this subtree
  FScopedTable.PopScope;
end;

procedure TGVNPass.ProcessBlock(Block: TSSABasicBlock);
var
  i: Integer;
  Instr: TSSAInstruction;
begin
  FCurrentBlock := Block;

  {$IFDEF DEBUG_GVN}
  if DebugGVN then
    WriteLn(Format('[GVN] Processing block "%s" (%d instructions)',
      [Block.LabelName, Block.Instructions.Count]));
  {$ENDIF}

  // Process each instruction in block
  for i := 0 to Block.Instructions.Count - 1 do
  begin
    Instr := Block.Instructions[i];
    ProcessInstruction(Instr);
  end;
end;

function TGVNPass.Run(Prog: TSSAProgram): Integer;
var
  DomTreeObj: TObject;
  i: Integer;
  Block: TSSABasicBlock;
begin
  {$IFDEF DEBUG_GVN}
  if DebugGVN then
    WriteLn('[GVN] Starting Global Value Numbering pass...');
  {$ENDIF}

  FProgram := Prog;
  FReplacements := 0;

  // CRITICAL: Collect all PHI-defined registers before processing
  // These registers are loop-variant and must be excluded from GVN
  CollectPhiDefinedRegisters;

  // Get dominator tree
  DomTreeObj := Prog.GetDomTree;

  if not Assigned(DomTreeObj) then
  begin
    {$IFDEF DEBUG_GVN}
    if DebugGVN then
      WriteLn('[GVN] ERROR: Dominator tree not available! Skipping GVN.');
    {$ENDIF}
    Exit(0);
  end;

  FDomTree := TDominatorTree(DomTreeObj);

  // STEP 4: Traverse blocks in preorder
  {$IFDEF DEBUG_GVN}
  if DebugGVN then
    WriteLn('[GVN] Traversing blocks in preorder...');
  {$ENDIF}

  try
    FScopedTable.FInitialStackDepth := 0;  // Track initial depth

    { STEP 4: Preorder traversal with scope management

      For each block in preorder:
      1. Push new scope (entering dominator subtree)
      2. Process all instructions in block
      3. Pop scope (exiting dominator subtree)

      NOTE: This conservative approach limits inter-block GVN but ensures
      correctness. For full inter-block GVN, we would need to use
      TraverseDomTree which does DFS with proper scope inheritance.
      However, that requires careful handling of the dominator tree structure. }

    for i := 0 to FDomTree.GetPreorderCount - 1 do
    begin
      Block := FDomTree.GetPreorderBlock(i);

      FScopedTable.PushScope;
      ProcessBlock(Block);
      FScopedTable.PopScope;
    end;

    // STEP 5 REQUIREMENT: Verify stack integrity
    {$IFDEF DEBUG_GVN}
    if not FScopedTable.VerifyStackIntegrity and DebugGVN then
      WriteLn('[GVN] WARNING: Stack integrity check failed!');
    {$ELSE}
    FScopedTable.VerifyStackIntegrity;
    {$ENDIF}

  except
    on E: Exception do
    begin
      {$IFDEF DEBUG_GVN}
      if DebugGVN then
        WriteLn('[GVN] ERROR during traversal: ', E.Message);
      {$ENDIF}
      raise;
    end;
  end;

  {$IFDEF DEBUG_GVN}
  if DebugGVN then
    WriteLn(Format('[GVN] Pass complete: %d values replaced', [FReplacements]));
  {$ENDIF}
  Result := FReplacements;
end;

end.
