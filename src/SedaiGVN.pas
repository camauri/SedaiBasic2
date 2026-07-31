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
{$interfaces CORBA}
{$codepage UTF8}
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
    // Lowest scope Lookup may reach. A CALL is an edge in this CFG, so a procedure body is DOMINATED
    // by the module code that calls it -- but dominance there does not mean the caller's register
    // still holds its value when the callee reads it: a call installs a new frame. The floor is
    // raised on entry to a procedure body so nothing outside it can be reused inside. See
    // TraverseDomTree.
    FFloor: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Depth: Integer;                       // number of open scopes
    function SetFloor(NewFloor: Integer): Integer; // raise/restore the lookup floor, returns the old one

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
    type
      TDefCountMap = specialize TDictionary<Int64, Integer>;
      TRegKeySet = specialize TDictionary<Int64, Boolean>;
      TLocalValueMap = specialize TDictionary<string, TSSAValue>;
      TConstRegMap = specialize TDictionary<Int64, string>;
      TRegValueMap = specialize TDictionary<Int64, TSSAValue>;
  private
    FScopedTable: TScopedGVNTable;
    { Value table for STATE-DEPENDENT reads (array element loads, LBOUND/UBOUND). Their result is not
      a function of their operands alone, so the scoped dominator table is the wrong home for them:
      A dominating B does NOT mean no store or REDIM ran on some path from A to B. This table is
      BLOCK-LOCAL and is emptied by any instruction that is not a proven non-writer, so a reuse can
      only ever be a straight-line one with nothing in between that could have changed the state. }
    FBlockTable: TLocalValueMap;
    // DESCRIPTOR reads (LBOUND/UBOUND) get their own block-local table, because what invalidates
    // them is not what invalidates an element read. Storing into an array element, or into a
    // record field, cannot change an array's bounds - only DIM/REDIM/ERASE/BIND or a call can.
    // Sharing one table made intrec's hot loop recompute LBOUND(a,0) four times per iteration:
    // the RecordStore between two accesses emptied the table, so the second LBOUND never saw the
    // first, and with it died the index subtraction and the element load that depend on it.
    FBoundTable: TLocalValueMap;
    // Loop-variant ARITHMETIC. It is confined to the block for COST, not for soundness (reusing it
    // across blocks only stretches a live range over the latch), so unlike a memory read it must
    // NOT be thrown away at a store: its value is a function of its operands, and both it and they
    // are single-def. Sharing the state-dependent table meant intrec recomputed the same
    // 'index - lbound' four times per iteration, once after each RecordStore.
    FLoopVarTable: TLocalValueMap;
    { Constants get their OWN block-local table, and it is emptied at every CALL. A literal reloaded
      into a second register is the same value anywhere, so dominance would license reuse across the
      whole dominated subtree - but the register that carries it would then have to survive whatever
      lies between, and a CALL installs a new frame: what the callee does to that register is not
      this pass's business to predict. Confined to the block and cut at every call, the reuse needs
      no argument about frames at all - nothing between the two occurrences can write the register,
      because a single-def name has exactly one writer and no call intervenes. }
    FConstTable: TLocalValueMap;
    FDomTree: TDominatorTree;
    FProgram: TSSAProgram;
    FReplacements: Integer;  // Count of values replaced
    FCurrentBlock: TSSABasicBlock;  // Current block being processed
    FPhiDefinedRegs: TRegKeySet;    // Registers defined by PHI functions (loop-variant)
    FDefCounts: TDefCountMap;       // packed (RegType,RegIndex,Version) -> number of defining instructions
    FConstRegs: TConstRegMap;       // single-def constant register -> its hash part (see BuildConstRegs)
    { Register rewritten into a copy -> the canonical register it now copies. Without this the pass
      is not transitive: it rewrites a redundant instruction as "Copy canonical" but leaves every
      LATER instruction reading the copy's own name, so a computation over a value this same pass
      just proved redundant hashes differently from the one over the canonical value and survives.
      That is what kept the four "index - LBOUND(a,d)" subtractions of an array-parameter loop alive
      even after their four LBOUND reads had been collapsed into one. Resolving uses through this map
      costs nothing at runtime: it only decides hash EQUALITY, no operand is rewritten here. }
    FCanonRegs: TRegValueMap;
    { Labels that some ssaCall/ssaCallSub names as its target, i.e. the entry blocks of procedure
      bodies. A call IS an edge in this CFG, so those blocks are dominated by the module code that
      calls them - and reuse across that edge is NOT sound: a call installs a new frame, and the
      caller's registers only survive it by accident of allocation. Collected once per Run, so the
      traversal pays a set lookup per block instead of a scan of every predecessor. }
    FProcEntryLabels: TStringList;

    { Process a single basic block }
    procedure ProcessBlock(Block: TSSABasicBlock);

    { Process a single instruction within a block }
    procedure ProcessInstruction(Instr: TSSAInstruction);

    { Turn a redundant instruction into a copy of the value that already computed it }
    procedure RewriteAsCopy(Instr: TSSAInstruction; const ExistingValue: TSSAValue;
      const Hash: string);

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

    { Count how many instructions define each register (whole program) }
    procedure BuildDefCounts;

    { Map every single-def register loaded with a literal to that literal's hash part }
    procedure BuildConstRegs;

    { Collect the labels reached by a CALL — the entry blocks of procedure bodies }
    procedure BuildProcEntryLabels;

    { Operand encoding for the value hash, with constant registers resolved to their value }
    function HashPart(const V: TSSAValue): string;

    { Poison (def-count := 2) every single-def register whose def does NOT dominate every use.
      Single-def alone is NOT single-assignment: a register can be READ before its one def runs
      (the read sees the bank's implicit zero). "Dim n As Integer : Print Abs(n) : n = -69 :
      Print Abs(n)" has one def of n, so both IntToFloat(n) hashed identically and GVN merged
      them - the second Abs read the STALE conversion of the implicit zero (printed 0, not 69).
      An explicit initializer emits a second def and was never affected; the implicit-zero path
      (MODERN Dim without '= x', every CLASSIC variable) was. Runs after FDomTree is available. }
    procedure PoisonUseBeforeDef;

    { True iff the value is a constant/none, or a register written by exactly ONE instruction }
    function IsSingleDef(const Value: TSSAValue): Boolean;

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

function TScopedGVNTable.Depth: Integer;
begin
  Result := FStack.Count;
end;

function TScopedGVNTable.SetFloor(NewFloor: Integer): Integer;
begin
  Result := FFloor;
  FFloor := NewFloor;
end;

function TScopedGVNTable.Lookup(const Hash: string; out Value: TSSAValue): Boolean;
var
  i: Integer;
  Map: TValueMap;
begin
  // Search from top (most recent scope) down to the floor (see FFloor: never below the procedure
  // body currently being processed).
  for i := FStack.Count - 1 downto FFloor do
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

{ Pack a register identity (bank, index, version) into one Int64 - the guard-path maps used to
  key on Format('%d:%d:%d') strings, built afresh for EVERY query (up to four per instruction). }
function RegKey64(const V: TSSAValue): Int64; inline;
begin
  Result := Int64(Ord(V.RegType)) or (Int64(V.RegIndex) shl 2) or (Int64(V.Version) shl 32);
end;

{ True for the reads whose result depends on program STATE (array contents, array bounds) and not on
  their operands alone. They are value-numbered in the BLOCK-LOCAL table, never in the scoped one. }
function IsStateDependentRead(Op: TSSAOpCode): Boolean; inline;
begin
  Result := Op in [ssaArrayLoad, ssaArrayLBound, ssaArrayUBound];
end;

{ True for an instruction that must EMPTY the block-local table: anything that could store into an
  array, reshape one, or hand control to code that does either.

  The polarity is deliberate and is the lesson OpIsMergeSafe already paid for in SedaiRegAlloc: the
  whitelist names the operations PROVEN not to write, and everything else - including every opcode
  added after this was written - is a barrier. A missing entry costs a missed reuse; a wrong entry
  costs a silent miscompile. Note this is about WRITES only, so the pure readers stay on the list
  (a RecordLoad between two array reads must not throw them away, and it sits exactly there in the
  hot loop of an array-of-UDT program). }
function IsMemoryBarrier(Op: TSSAOpCode): Boolean; forward;

function IsDescriptorRead(Op: TSSAOpCode): Boolean; inline;
// Reads of an array's SHAPE rather than of its data. Their value changes only when the array is
// dimensioned, resized, erased or rebound - not when an element or a record field is written.
begin
  Result := Op in [ssaArrayLBound, ssaArrayUBound, ssaArrayLBoundInd, ssaArrayUBoundInd];
end;

function IsReshapeBarrier(Op: TSSAOpCode): Boolean;
// Does this instruction possibly change some array's SHAPE? Same safe polarity as IsMemoryBarrier:
// the listed opcodes are the ones PROVEN harmless to a descriptor, everything else answers yes.
// It is IsMemoryBarrier's safe set plus the data writes and reads, which move values around
// without touching any bound: an element store, a record field store, and every load.
begin
  if not IsMemoryBarrier(Op) then Exit(False);      // already proven harmless to everything
  case Op of
    ssaArrayStore, ssaArrayStoreIndInt, ssaArrayStoreIndFloat, ssaArrayStoreIndString,
    ssaArrayLoad, ssaArrayLoadIndInt, ssaArrayLoadIndFloat, ssaArrayLoadIndString,
    ssaArrayLBound, ssaArrayUBound, ssaArrayLBoundInd, ssaArrayUBoundInd,
    ssaRecordStoreInt, ssaRecordStoreFloat, ssaRecordStoreString,
    ssaRecordLoadInt, ssaRecordLoadFloat, ssaRecordLoadString,
    ssaRecordTypeId:
      Result := False;
  else
    Result := True;
  end;
end;

function IsElementBarrier(Op: TSSAOpCode): Boolean;
// Can this instruction change an array ELEMENT? Same safe polarity again: only the opcodes proven
// harmless answer no. It is IsMemoryBarrier's safe set plus the RECORD accesses, which live in a
// different structure entirely - an array of UDT holds handles, and writing a field through a
// handle cannot change the handle stored in the array. Without this, intrec re-loaded the same
// element four times per iteration, once after each field store.
begin
  if not IsMemoryBarrier(Op) then Exit(False);
  case Op of
    ssaRecordStoreInt, ssaRecordStoreFloat, ssaRecordStoreString,
    ssaRecordLoadInt, ssaRecordLoadFloat, ssaRecordLoadString, ssaRecordTypeId:
      Result := False;
  else
    Result := True;
  end;
end;

function IsMemoryBarrier(Op: TSSAOpCode): Boolean;
begin
  case Op of
    // Structure and control flow within a procedure (a CALL is NOT here: the callee can do anything)
    ssaPhi, ssaLabel, ssaJump, ssaJumpIfZero, ssaJumpIfNotZero,
    // Register-only moves and materialisations
    ssaLoadConstInt, ssaLoadConstFloat, ssaLoadConstString,
    ssaCopyInt, ssaCopyFloat, ssaCopyString,
    // Integer / float arithmetic
    ssaAddInt, ssaSubInt, ssaMulInt, ssaDivInt, ssaModInt, ssaNegInt,
    ssaDivUInt, ssaModUInt,
    ssaAddFloat, ssaSubFloat, ssaMulFloat, ssaDivFloat, ssaModFloat, ssaPowFloat, ssaNegFloat,
    ssaShl, ssaShr, ssaShrUInt,
    ssaBitwiseAnd, ssaBitwiseOr, ssaBitwiseXor, ssaBitwiseNot,
    // Conversions and width changes
    ssaIntToFloat, ssaFloatToInt, ssaIntToString, ssaFloatToString,
    ssaStringToInt, ssaStringToFloat, ssaFloatRound, ssaNarrowInt, ssaNarrowSingle,
    // Comparisons
    ssaCmpEqInt, ssaCmpNeInt, ssaCmpLtInt, ssaCmpGtInt, ssaCmpLeInt, ssaCmpGeInt,
    ssaCmpLtUInt, ssaCmpGtUInt, ssaCmpLeUInt, ssaCmpGeUInt,
    ssaCmpEqFloat, ssaCmpNeFloat, ssaCmpLtFloat, ssaCmpGtFloat, ssaCmpLeFloat, ssaCmpGeFloat,
    ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString,
    // Value-producing string builders (they allocate a result string, they never write user memory)
    ssaStrConcat, ssaStrLen, ssaStrLenW, ssaStrLeft, ssaStrRight, ssaStrMid,
    ssaStrLeftW, ssaStrRightW, ssaStrMidW, ssaStrInstrW, ssaStrInstrRevW, ssaStrWChr, ssaStrWStringN,
    ssaStrAsc, ssaStrChr, ssaStrStr, ssaStrVal, ssaStrHex, ssaStrInstr,
    ssaStrLTrim, ssaStrRTrim, ssaStrTrim, ssaStrUCase, ssaStrLCase, ssaStrInstrRev,
    ssaStrSpace, ssaStrString, ssaStrTrimSet, ssaStrInstrRevAny, ssaStrInstrAny,
    ssaStrOct, ssaStrBin, ssaStrValInt, ssaStrDec,
    ssaStrMkInt, ssaStrMkFloat, ssaStrCvInt, ssaStrCvFloat,
    // Math
    ssaMathSin, ssaMathCos, ssaMathTan, ssaMathAtn, ssaMathLog, ssaMathExp,
    ssaMathSqr, ssaMathAbs, ssaMathSgn, ssaMathInt,
    ssaMathLog10, ssaMathLog2, ssaMathLogN,
    ssaMathAcos, ssaMathAsin, ssaMathAtan2, ssaMathFix, ssaMathFrac,
    ssaMathSinh, ssaMathCosh, ssaMathTanh, ssaMathAsinh, ssaMathAcosh, ssaMathAtanh,
    // Pure READS - they observe state, they do not change it
    ssaArrayLoad, ssaArrayLBound, ssaArrayUBound,
    ssaRecordLoadInt, ssaRecordLoadFloat, ssaRecordLoadString,
    ssaArrayLoadIndInt, ssaArrayLoadIndFloat, ssaArrayLoadIndString,
    ssaRefLoadInt, ssaRefLoadFloat, ssaRefLoadString,
    ssaRawLoadInt, ssaRawLoadFloat, ssaRawLoadZStr,
    ssaXferLoadInt, ssaXferLoadFloat, ssaXferLoadString:
      Result := False;
  else
    Result := True;
  end;
end;

constructor TGVNPass.Create;
begin
  inherited Create;
  FScopedTable := TScopedGVNTable.Create;
  FBlockTable := TLocalValueMap.Create;
  FBoundTable := TLocalValueMap.Create;
  FLoopVarTable := TLocalValueMap.Create;
  FConstTable := TLocalValueMap.Create;
  FPhiDefinedRegs := TRegKeySet.Create;
  FDefCounts := TDefCountMap.Create;
  FConstRegs := TConstRegMap.Create;
  FCanonRegs := TRegValueMap.Create;
  FProcEntryLabels := TStringList.Create;
  FProcEntryLabels.Sorted := True;         // IndexOf is a binary search: one per block in the traversal
  FProcEntryLabels.Duplicates := dupIgnore;
  FReplacements := 0;
  FCurrentBlock := nil;
end;

destructor TGVNPass.Destroy;
begin
  FCanonRegs.Free;
  FProcEntryLabels.Free;
  FConstRegs.Free;
  FDefCounts.Free;
  FPhiDefinedRegs.Free;
  FBlockTable.Free;
  FBoundTable.Free;
  FLoopVarTable.Free;
  FConstTable.Free;
  FScopedTable.Free;
  inherited Destroy;
end;

var
  // Gate for constant value numbering, read once. -1 = not read yet; then:
  //   0 = the historical exclusion (constants are never numbered), for a one-binary A/B;
  //   1 = numbered but confined to the block and cut at every call (see FConstTable). Kept as the
  //       conservative fallback: it needs no argument about frames at all. Costs most of the yield
  //       (fib and sieve go to zero), so it is not the default;
  //   2 = numbered on DOMINANCE, like every other pure value. THE DEFAULT.
  // Mode 2 did miscompile once, and the cause was NOT here: LICM was redirecting a bcCallSub onto a
  // loop pre-header and hoisting values across a frame-unit boundary, so a register carrying a
  // literal across a call survived only by accident of allocation. Fixed in CreatePreHeader; this
  // pass merely made the collision likely instead of unlikely.
  GGVNConst: Integer = -1;

function GVNConstMode: Integer;
var S: string;
begin
  if GGVNConst < 0 then
  begin
    S := GetEnvironmentVariable('GVNCONST');
    if S = '0' then GGVNConst := 0
    else if S = '1' then GGVNConst := 1
    else GGVNConst := 2;
  end;
  Result := GGVNConst;
end;

function GVNNumbersConstants: Boolean;
begin
  Result := GVNConstMode <> 0;
end;

procedure TGVNPass.CollectPhiDefinedRegisters;
var
  i, j: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
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
          FPhiDefinedRegs.AddOrSetValue(RegKey64(Instr.Dest), True);
      end;
    end;
  end;

  {$IFDEF DEBUG_GVN}
  if DebugGVN then
    WriteLn(Format('[GVN] Found %d PHI-defined registers (loop-variant)',
      [FPhiDefinedRegs.Count]));
  {$ENDIF}
end;

procedure TGVNPass.BuildDefCounts;
var
  i, j: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  Key: Int64;
  Cnt: Integer;
begin
  { Value numbering is only sound over SINGLE-ASSIGNMENT names: a register written by more than one
    instruction (every CLASSIC user variable at Version=0, MODERN Version=0 loop-carried temps, a string
    rebuilt by a MID statement, ...) does not denote one value, so both hashing its uses and inserting it
    as a canonical value are wrong — the classic failure is reusing a value straight across a
    redefinition (m406_deffn_classic) or across an in-place string rewrite (m80_midstmt). Instead of
    trusting Version>0 or dialect flags, count the defs directly: it is the exact property we need. }
  FDefCounts.Clear;
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];
      if Instr.Dest.Kind <> svkRegister then Continue;
      // Mirror the SSA renamer's authoritative "Dest is a USE" list (SedaiSSAConstruction): these
      // opcodes carry a VALUE operand in the Dest field and define nothing.
      case Instr.OpCode of
        ssaArrayStore, ssaArrayStoreIndInt, ssaArrayStoreIndFloat,
        ssaArrayStoreIndString, ssaPrint, ssaPrintLn:
          Continue;
      else
        Key := RegKey64(Instr.Dest);
        if FDefCounts.TryGetValue(Key, Cnt) then
          FDefCounts[Key] := Cnt + 1
        else
          FDefCounts.Add(Key, 1);
      end;
    end;
  end;
end;

function TGVNPass.IsSingleDef(const Value: TSSAValue): Boolean;
var
  Cnt: Integer;
begin
  if Value.Kind <> svkRegister then Exit(True);   // constants/labels/none are immutable
  Result := FDefCounts.TryGetValue(RegKey64(Value), Cnt) and (Cnt = 1);
end;

procedure TGVNPass.PoisonUseBeforeDef;
type
  TDefSite = record
    Block: TSSABasicBlock;
    Pos: Integer;
  end;
  TDefSiteMap = specialize TDictionary<Int64, TDefSite>;
var
  Sites: TDefSiteMap;
  i, j, k: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  Key: Int64;
  Site: TDefSite;
  Cnt: Integer;

  procedure CheckUse(const V: TSSAValue; UseBlock: TSSABasicBlock; UsePos: Integer);
  begin
    if V.Kind <> svkRegister then Exit;
    Key := RegKey64(V);
    if not (FDefCounts.TryGetValue(Key, Cnt) and (Cnt = 1)) then Exit;   // only single-def matters
    if not Sites.TryGetValue(Key, Site) then
    begin
      FDefCounts[Key] := 2;   // defensive: a counted def we failed to locate
      Exit;
    end;
    // Same block: the def must come strictly BEFORE the use ("n = n + 1" reads the pre-def
    // value in the same instruction, so UsePos = def pos also poisons). Different block: the
    // def's block must dominate the use's block.
    if Site.Block = UseBlock then
    begin
      if UsePos <= Site.Pos then FDefCounts[Key] := 2;
    end
    else if not FDomTree.IsDom(Site.Block, UseBlock) then
      FDefCounts[Key] := 2;
  end;

begin
  Sites := TDefSiteMap.Create;
  try
    // Sweep 1: record the def site of every (still) single-def register. The def-detection
    // mirror of BuildDefCounts: Dest is a def except for the Dest-as-USE opcodes.
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
        else
          Key := RegKey64(Instr.Dest);
          if FDefCounts.TryGetValue(Key, Cnt) and (Cnt = 1) then
          begin
            Site.Block := Block;
            Site.Pos := j;
            Sites.AddOrSetValue(Key, Site);
          end;
        end;
      end;
    end;
    // Sweep 2: every register USE must be dominated by its single def, or the register is
    // poisoned. PHI sources are uses at the END of their predecessor block.
    for i := 0 to FProgram.Blocks.Count - 1 do
    begin
      Block := FProgram.Blocks[i];
      for j := 0 to Block.Instructions.Count - 1 do
      begin
        Instr := Block.Instructions[j];
        CheckUse(Instr.Src1, Block, j);
        CheckUse(Instr.Src2, Block, j);
        CheckUse(Instr.Src3, Block, j);
        case Instr.OpCode of
          ssaArrayStore, ssaArrayStoreIndInt, ssaArrayStoreIndFloat,
          ssaArrayStoreIndString, ssaPrint, ssaPrintLn:
            CheckUse(Instr.Dest, Block, j);
        end;
        for k := 0 to High(Instr.PhiSources) do
          if Instr.PhiSources[k].FromBlock <> nil then
            CheckUse(Instr.PhiSources[k].Value, Instr.PhiSources[k].FromBlock, MaxInt)
          else
            CheckUse(Instr.PhiSources[k].Value, Block, j);
      end;
    end;
  finally
    Sites.Free;
  end;
end;

function TGVNPass.IsLoopVariant(const Value: TSSAValue): Boolean;
begin
  { Check if a value is loop-variant (defined by a PHI function) }
  Result := (Value.Kind = svkRegister) and FPhiDefinedRegs.ContainsKey(RegKey64(Value));
end;

function TGVNPass.UsesLoopVariantValue(Instr: TSSAInstruction): Boolean;
begin
  { Check if any source operand of this instruction is loop-variant }
  Result := False;

  { Answers "must this instruction be confined to the BLOCK-LOCAL table?", not "is it unsafe?".

    Reuse across blocks is sound here even for a loop-variant operand, and the guards that make it so
    are already in place:

      * every operand and the Dest are SINGLE-DEF program-wide (BuildDefCounts), and
      * every single-def register's def DOMINATES all its uses (PoisonUseBeforeDef),
      * the scoped table only ever offers a canonical value whose block DOMINATES the current one.

    Take the canonical instruction A and the redundant B (same opcode, same operand NAMES), A
    dominating B, and let D be the single def of some operand. D dominates A. For D to run again
    between A and B, control must leave A, reach D and come back; but every path from D to B goes
    through A (A dominates B), so the last D before B is still followed by A before B. Hence A's
    Dest holds exactly what B would compute, and nothing can overwrite it - A is its only def.

    What is NOT sound across blocks is the COST. A value built from a loop-varying operand is
    recomputed every iteration by definition, so reusing one from a different block means keeping it
    live across the loop latch - and with Copy Coalescing disabled ([[copycoal-miscompile]]) the
    parallel copies PHI elimination then emits are permanent. Measured on nbody_fb: numbering the
    "i + 1" of the loop CONDITION together with the "i + 1" of the increment removes one AddInt and
    adds a THREE-copy rotation, +2.3% on --aot. Confining these to one block keeps every
    straight-line redundancy - which is where the array-parameter subscripts sit, four in a row in
    the same block - and gives up only the ones that would have stretched a live range.

    So: loop-variant operand => block-local table. Same-block reuse cannot lengthen a live range
    across the latch, and it needs no argument about iterations at all. }

  if (Instr.Src1.Kind <> svkNone) and IsLoopVariant(Instr.Src1) then
    Exit(True);
  if (Instr.Src2.Kind <> svkNone) and IsLoopVariant(Instr.Src2) then
    Exit(True);
  if (Instr.Src3.Kind <> svkNone) and IsLoopVariant(Instr.Src3) then
    Exit(True);
end;

{ Cheap, exact operand encoding for the value hash. Replaces SSAValueToString, which paid RTTI
  (GetEnumName), Format and FloatToStr per operand on every numerable instruction. Every kind gets
  a distinct prefix so no two different values can share a key. Floats key on their BIT PATTERN
  (FloatToStr rounded to 15 digits, so two distinct doubles could collide and merge - keying on the
  bits partitions strictly finer, which is always sound). String/label constants key on their pool
  id, which the interned pool makes exact. }
function ValueHashPart(const V: TSSAValue): string; inline;
begin
  case V.Kind of
    svkRegister:
      Result := 'r' + IntToStr(Ord(V.RegType)) + '.' + IntToStr(V.RegIndex) + '.' + IntToStr(V.Version);
    svkConstInt: Result := 'i' + IntToStr(V.ConstInt);
    svkConstFloat: Result := 'f' + IntToStr(PInt64(@V.ConstFloat)^);
    svkConstString: Result := 's' + IntToStr(V.ConstStringId);
    svkVariable: Result := 'v' + IntToStr(V.VarNameId);
    svkLabel: Result := 'l' + IntToStr(V.LabelNameId);
    svkArrayRef: Result := 'a' + IntToStr(V.ArrayIndex);
  else
    Result := '?';
  end;
end;

procedure TGVNPass.BuildProcEntryLabels;
// Every label named as the target of an ssaCall/ssaCallSub, AND every label whose address is taken
// with ssaLoadProcAddr (@sub). Those blocks are the entry points of procedure bodies, and the edge
// that reaches them is a CALL, not a branch: the dominator tree says the calling code dominates the
// body, but a call installs a new frame, so a value computed before the call must not be reused
// inside it. TraverseDomTree raises the lookup floor there.
//
// ⛔ @sub HAS to be in this list, and leaving it out was a silent miscompile. A procedure reached
// only through a function pointer - the shape of ThreadCreate(@worker, k) - is never the target of
// a call instruction, so it did not raise the floor, and the body happily reused a constant whose
// canonical register had been loaded by the MODULE. That works for a call (same context, same
// register banks) and is WRONG for a thread, which gets banks of its own: the worker read a
// register that is zero in its context. "done += 1" was emitted as "AddInt R16, R17, R16" with R16
// holding the literal 1 from the module - so the worker added ZERO, silently, and every count came
// out wrong. See memory: recursive-shared-array-write-breaks-threads.
//
// ⚠️ The label of @sub is in Src1, not Dest (Dest holds the register receiving the address) - the
// same asymmetry the bytecode compiler handles at "ssaLoadProcAddr ... AddJumpFixup(Src1)".
var
  i, j: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  Lbl: string;
begin
  FProcEntryLabels.Clear;
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];
      Lbl := '';
      if (Instr.OpCode = ssaCallSub) or (Instr.OpCode = ssaCall) then
        Lbl := Instr.Dest.LabelName
      else if (Instr.OpCode = ssaLoadProcAddr) and (Instr.Src1.Kind = svkLabel) then
        Lbl := Instr.Src1.LabelName;
      if (Lbl <> '') and (FProcEntryLabels.IndexOf(Lbl) < 0) then
        FProcEntryLabels.Add(Lbl);
    end;
  end;
end;

procedure TGVNPass.BuildConstRegs;
var
  i, j: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
begin
  { Two registers holding the SAME literal are the same value, but the instructions that load them
    are not value-numbered (a constant is cheaper to rematerialise than to keep alive in a register,
    which is why LoadConst is excluded from numbering below). Without this map that decision also
    hides every computation OVER those constants: the SSA generator allocates a fresh register per
    occurrence, so "LBOUND(a, dim0)" emitted at two different subscripts gets two different dimension
    registers and the two reads hash differently - and so, in turn, do the index subtractions and the
    element loads built on them. Resolving a constant register to its VALUE in the hash fixes that
    without materialising a single extra live range: only the hash changes, and the rewrite still
    copies from the canonical register, which dominates.
    Restricted to single-def registers - a name written twice does not denote one value. }
  FConstRegs.Clear;
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];
      if Instr.Dest.Kind <> svkRegister then Continue;
      if not IsSingleDef(Instr.Dest) then Continue;
      case Instr.OpCode of
        ssaLoadConstInt:
          if Instr.Src1.Kind = svkConstInt then
            FConstRegs.AddOrSetValue(RegKey64(Instr.Dest), 'i' + IntToStr(Instr.Src1.ConstInt));
        ssaLoadConstFloat:
          if Instr.Src1.Kind = svkConstFloat then
            FConstRegs.AddOrSetValue(RegKey64(Instr.Dest),
              'f' + IntToStr(PInt64(@Instr.Src1.ConstFloat)^));
        ssaLoadConstString:
          if Instr.Src1.Kind = svkConstString then
            FConstRegs.AddOrSetValue(RegKey64(Instr.Dest), 's' + IntToStr(Instr.Src1.ConstStringId));
      end;
    end;
  end;
end;

function TGVNPass.HashPart(const V: TSSAValue): string;
var
  Canon, Next: TSSAValue;
  Hops: Integer;
begin
  Canon := V;
  // Follow the canonical chain. Each hop maps a register this pass turned into a copy onto the
  // register it copies, so the chain is acyclic by construction (the canonical value was inserted
  // strictly earlier); the counter is a cheap backstop, not a correctness argument.
  // NOTE: the result of TryGetValue goes to a SEPARATE variable. An `out` parameter is zeroed on
  // entry by FPC, so reading the lookup straight into Canon would destroy it on the FAILING call -
  // leaving Kind = svkNone, which ValueHashPart encodes as '?', so every operand in the program
  // would hash alike and match anything. Guard m466 (job/tests/bas/gvn_paramarray_store.bas).
  Hops := 0;
  while (Canon.Kind = svkRegister) and (Hops < 16) do
  begin
    if not FCanonRegs.TryGetValue(RegKey64(Canon), Next) then Break;
    Canon := Next;
    Inc(Hops);
  end;

  if (Canon.Kind = svkRegister) and FConstRegs.TryGetValue(RegKey64(Canon), Result) then
    Exit;
  Result := ValueHashPart(Canon);
end;

function TGVNPass.ComputeValueHash(Instr: TSSAInstruction): string;
begin
  { Compute a hash key that uniquely identifies the value computed by this instruction.

    Two instructions compute the same value iff:
    - Same opcode
    - Same source operands (by value, not register)

    Example:
      R1 = Add R0, R0  → Hash = "43:r0.1.2:r0.1.2"
      R2 = Add R0, R0  → same key: can reuse R1
  }

  Result := IntToStr(Ord(Instr.OpCode));

  // Append source operands
  if Instr.Src1.Kind <> svkNone then
    Result := Result + ':' + HashPart(Instr.Src1);
  if Instr.Src2.Kind <> svkNone then
    Result := Result + ':' + HashPart(Instr.Src2);
  if Instr.Src3.Kind <> svkNone then
    Result := Result + ':' + HashPart(Instr.Src3);
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
    // TRAPPING ops are NOT pure: eliminating a later occurrence also eliminates its runtime error, which
    // is observable under ON ERROR/RESUME (m138_onerror: the second "a \ b" must raise div-by-zero AGAIN
    // to reach its handler). Integer div/mod can raise in both dialects (div by zero, INT_MIN/-1) —
    // never value-number them. Same reasoning as LICM's no-speculative-div rule.
    ssaDivInt, ssaModInt:
      Result := False;

    // Arithmetic operations (pure functions)
    ssaAddInt, ssaSubInt, ssaMulInt, ssaNegInt,
    ssaAddFloat, ssaSubFloat, ssaMulFloat, ssaNegFloat:
      Result := True;

    // Float divide/power: MODERN follows IEEE (div0 -> inf/nan, pure); CLASSIC RAISES on division by
    // zero / bad domain, so each occurrence must re-execute there.
    ssaDivFloat, ssaPowFloat:
      Result := not FProgram.GlobalVariableSemantics;

    // Conversions (pure in MODERN; ssaFloatToInt can raise OVERFLOW in CLASSIC)
    ssaIntToFloat, ssaIntToString, ssaFloatToString,
    ssaStringToInt, ssaStringToFloat:
      Result := True;
    ssaFloatToInt:
      Result := not FProgram.GlobalVariableSemantics;

    // Comparisons (pure functions)
    ssaCmpEqInt, ssaCmpNeInt, ssaCmpLtInt, ssaCmpGtInt, ssaCmpLeInt, ssaCmpGeInt,
    ssaCmpEqFloat, ssaCmpNeFloat, ssaCmpLtFloat, ssaCmpGtFloat, ssaCmpLeFloat, ssaCmpGeFloat,
    ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString:
      Result := True;

    // Bitwise operations (pure functions)
    ssaBitwiseAnd, ssaBitwiseOr, ssaBitwiseXor, ssaBitwiseNot:
      Result := True;

    // String operations (pure functions)
    ssaStrConcat, ssaStrLen, ssaStrLenW, ssaStrLeft, ssaStrRight, ssaStrMid,
    ssaStrLeftW, ssaStrRightW, ssaStrMidW, ssaStrInstrW, ssaStrInstrRevW, ssaStrWChr, ssaStrWStringN,
    // FreeBASIC numeric serialization (B3): MK*/CV* are pure deterministic packers/unpackers.
    ssaStrMkInt, ssaStrMkFloat, ssaStrCvInt, ssaStrCvFloat:
      Result := True;

    // Math functions: always pure in MODERN (IEEE nan/inf); in CLASSIC Sqr(neg)/Log(<=0)/Exp-overflow
    // RAISE, so those must re-execute per occurrence there.
    ssaMathSin, ssaMathCos, ssaMathTan, ssaMathAtn,
    ssaMathAbs, ssaMathSgn, ssaMathInt:
      Result := True;
    ssaMathSqr, ssaMathLog, ssaMathExp, ssaMathLog10, ssaMathLog2, ssaMathLogN:
      Result := not FProgram.GlobalVariableSemantics;

    // Constants. The two reasons they were excluded no longer hold, and the second was never a
    // reason at all on THIS machine:
    //   * "Superinstructions fuses Arith+Copy and the fused instruction expects the constant register
    //     to be valid" - fusion has been off since 18 July and was measured to have to stay off (its
    //     mere presence in the run loop costs every program). A live pass cannot keep paying for a
    //     dead one.
    //   * "a constant is cheaper to rematerialise than to keep alive in a register" - true for a
    //     machine with a fixed register file and a spiller. Here a register is a slot in an array:
    //     keeping one alive costs NOTHING, while rematerialising costs a full DISPATCH (~10 ns), the
    //     single most expensive thing the interpreter does. The textbook trade-off is INVERTED.
    // A recursive body reloads its literals once per ACTIVATION and cannot hoist them (there is no
    // "outside"), so this is where the reload actually shows: fib loads the literal 2 twice per call.
    // GVNCONST=0 restores the historical exclusion for a one-binary A/B.
    ssaLoadConstInt, ssaLoadConstFloat, ssaLoadConstString:
      Result := GVNNumbersConstants;

    // Copy operations - DO NOT value number!
    // Many distinct BASIC variables are initialised from the same source (e.g.
    // SIGN%=C1%, FLIPS%=C1%, I%=C1%). Each lowers to "X = Copy C1", all with the
    // same value hash. Inter-block GVN would rewrite the later ones as copies of
    // the first (X2 = Copy X1), and Copy Coalescing then merges those *distinct*
    // variables into one register -> they alias and corrupt each other when one is
    // later reassigned. Copies carry no real computation, so value-numbering them
    // buys nothing; Copy Propagation / Coalescing already handle genuine copies.
    ssaCopyInt, ssaCopyFloat, ssaCopyString:
      Result := False;

    // State-dependent reads. Reuse is confined to the block-local table (see IsStateDependentRead),
    // which is emptied by anything that could store or reshape, so "no intervening stores" is
    // ENFORCED here rather than assumed.
    //
    // LBOUND/UBOUND earn their place: on an array PARAMETER the callee cannot know the caller's
    // lower bound, so EVERY subscript lowers to "index - LBOUND(a, d)" and a loop body re-reads the
    // same descriptor field once per access. Numbering them collapses those to one, which in turn
    // makes the index subtractions and element loads textually identical so they collapse too - and
    // then LICM can lift the single survivor out of the loop entirely.
    ssaArrayLoad, ssaArrayLBound, ssaArrayUBound:
      Result := True;

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

  // SOUNDNESS: only single-assignment names may take part in value numbering — an operand's register
  // name must denote ONE value for the hash to mean anything, and the Dest must never be rewritten for
  // its insertion as a canonical value to stay valid in dominated blocks. Checked directly on def
  // counts (BuildDefCounts), which covers CLASSIC Version=0 variables, multi-def MODERN temps and
  // MID-statement string rebuilds uniformly.
  if not (IsSingleDef(Instr.Dest) and IsSingleDef(Instr.Src1) and
          IsSingleDef(Instr.Src2) and IsSingleDef(Instr.Src3)) then
    Exit;

  // Compute hash for this instruction's value
  Hash := ComputeValueHash(Instr);

  // Constants answer from their own block-local table, cut at every call (see FConstTable). Checked
  // before everything else: a LoadConst is neither state-dependent nor loop-variant, so it would
  // otherwise fall through to the scoped table and be reusable across the whole dominated subtree.
  if (GVNConstMode = 1) and
     (Instr.OpCode in [ssaLoadConstInt, ssaLoadConstFloat, ssaLoadConstString]) then
  begin
    if FConstTable.TryGetValue(Hash, ExistingValue) then
      RewriteAsCopy(Instr, ExistingValue, Hash)
    else
      FConstTable.AddOrSetValue(Hash, Instr.Dest);
    Exit;
  end;

  // Two families answer from the BLOCK-LOCAL table only. ProcessBlock empties that table on entry to
  // the block and again at every barrier, so a hit there means: same block, earlier instruction, and
  // nothing in between that could store into an array or reshape one.
  //   * a STATE-DEPENDENT read, because its value is not a function of its operands (soundness);
  //   * anything built on a LOOP-VARIANT value, because reusing one across blocks only stretches a
  //     live range over the loop latch (cost - see UsesLoopVariantValue).
  // A descriptor read answers from its own table, which survives element and field stores.
  if IsDescriptorRead(Instr.OpCode) then
  begin
    if FBoundTable.TryGetValue(Hash, ExistingValue) then
      RewriteAsCopy(Instr, ExistingValue, Hash)
    else
      FBoundTable.AddOrSetValue(Hash, Instr.Dest);
    Exit;
  end;
  // Loop-variant arithmetic that reads no memory: block-local, but it outlives the stores in the
  // block, because nothing a store does can change what its operands compute to.
  if (not IsStateDependentRead(Instr.OpCode)) and UsesLoopVariantValue(Instr) then
  begin
    if FLoopVarTable.TryGetValue(Hash, ExistingValue) then
      RewriteAsCopy(Instr, ExistingValue, Hash)
    else
      FLoopVarTable.AddOrSetValue(Hash, Instr.Dest);
    Exit;
  end;
  if IsStateDependentRead(Instr.OpCode) or UsesLoopVariantValue(Instr) then
  begin
    if FBlockTable.TryGetValue(Hash, ExistingValue) then
      RewriteAsCopy(Instr, ExistingValue, Hash)
    else
      FBlockTable.AddOrSetValue(Hash, Instr.Dest);
    Exit;
  end;

  // STEP 5: Lookup in scoped hash table
  if FScopedTable.Lookup(Hash, ExistingValue) then
  begin
    { Found equivalent computation!

      STEP 6: Dominance check
      In a correct preorder traversal, if the value exists in the scoped table,
      it MUST dominate the current instruction (by construction of the scope stack). }
    RewriteAsCopy(Instr, ExistingValue, Hash);
  end
  else
  begin
    // STEP 5: Insert into current scope
    FScopedTable.Insert(Hash, Instr.Dest);
  end;
end;

procedure TGVNPass.RewriteAsCopy(Instr: TSSAInstruction; const ExistingValue: TSSAValue;
  const Hash: string);
begin
  {$IFDEF DEBUG_GVN}
  if DebugGVN then
    WriteLn(Format('[GVN] Replacing %s with %s (hash: %s)',
      [SSAValueToString(Instr.Dest), SSAValueToString(ExistingValue), Hash]));
  {$ELSE}
  if Hash = '' then ;  // parameter is only read by the debug trace
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

  // From here on this Dest IS the canonical value, so every later instruction reading it must hash
  // as if it read the canonical register. Only single-def Dests reach this point, so the mapping
  // holds everywhere the register is live.
  if Instr.Dest.Kind = svkRegister then
    FCanonRegs.AddOrSetValue(RegKey64(Instr.Dest), ExistingValue);

  Inc(FReplacements);
end;

procedure TGVNPass.TraverseDomTree(Block: TSSABasicBlock);
var
  Children: TFPList;
  i, SavedFloor: Integer;
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

  { A procedure body is dominated by the code that CALLS it, because a call is an edge here — but
    the callee runs on a new frame, so a value the caller computed is not available to it: the
    register only still holds it by accident of allocation, and the frame save/restore and the frame
    BASE RELOCATION are both free to move it. Reuse across that edge is exactly the kind of
    dominance-shaped argument that is true about control flow and false about storage. So the body
    starts from an empty view: nothing below this scope may be looked up until the subtree is done.
    (Restored, not zeroed, because procedure bodies can nest in the dominator tree.) }
  SavedFloor := -1;
  if FProcEntryLabels.IndexOf(Block.LabelName) >= 0 then
    SavedFloor := FScopedTable.SetFloor(FScopedTable.Depth - 1);

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

  if SavedFloor >= 0 then
    FScopedTable.SetFloor(SavedFloor);

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

  // The state-dependent table never survives a block boundary: entering a block says nothing about
  // which of its predecessors ran, nor what they stored.
  FBlockTable.Clear;
  FBoundTable.Clear;
  FLoopVarTable.Clear;
  FConstTable.Clear;

  // Process each instruction in block
  for i := 0 to Block.Instructions.Count - 1 do
  begin
    Instr := Block.Instructions[i];
    // Barrier FIRST: an instruction that can write or reshape invalidates the reads recorded before
    // it, including - conservatively - its own operands' worth of state.
    if IsElementBarrier(Instr.OpCode) then
      FBlockTable.Clear;
    // ...and the descriptor table only for something that can actually reshape an array.
    if IsReshapeBarrier(Instr.OpCode) then
      FBoundTable.Clear;
    // A call is the barrier for constants: past it, this pass cannot say what the register holds.
    if Instr.OpCode in [ssaCall, ssaCallSub, ssaCallSubIndirect] then
      FConstTable.Clear;
    ProcessInstruction(Instr);
  end;
end;

function TGVNPass.Run(Prog: TSSAProgram): Integer;
var
  DomTreeObj: TObject;
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

  // Count definitions per register: value numbering is restricted to single-assignment names.
  BuildDefCounts;

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

  // Single-def is not enough: the one def must also DOMINATE every use (a use-before-def
  // reads the bank's implicit zero and must not share the def's value number). Needs the
  // dominator tree, hence after the assignment above.
  PoisonUseBeforeDef;

  // Constant registers are keyed on IsSingleDef, so this must follow the poisoning above.
  BuildConstRegs;
  BuildProcEntryLabels;
  FCanonRegs.Clear;

  // STEP 4: Traverse blocks in preorder
  {$IFDEF DEBUG_GVN}
  if DebugGVN then
    WriteLn('[GVN] Traversing blocks in preorder...');
  {$ENDIF}

  try
    FScopedTable.FInitialStackDepth := 0;  // Track initial depth

    { STEP 4: Inter-block GVN via dominator-tree DFS with scope inheritance.

      TraverseDomTree recurses the dominator tree from the entry block, pushing a
      scope on entry and popping it after the whole subtree is processed. A value
      computed in block A is therefore visible (in scope) exactly while processing
      the blocks A dominates — i.e. it can only be reused where A's definition is
      guaranteed to have executed. This makes cross-block reuse safe by dominance.

      The entry block is preorder index 0 (root of the dominator tree). Unreachable
      blocks are not in the dominator tree and are simply not visited (DBE removes
      them earlier anyway). }

    if FDomTree.GetPreorderCount > 0 then
      TraverseDomTree(FDomTree.GetPreorderBlock(0));

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
