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
  Unit: SedaiCopyProp (Copy Propagation)

  Purpose: Eliminate redundant copy operations by propagating the source
           through all uses of the destination.

  Algorithm: Forward dataflow analysis
             1. Build def-use chains for all registers
             2. Identify copy instructions: %dest = %src (no computation)
             3. Replace all uses of %dest with %src
             4. Mark copy instruction as dead (DCE will remove it)

  Examples:
    Before:               After:
    %r1 = LoadVar X       %r1 = LoadVar X
    %r2 = Copy %r1        %r2 = Copy %r1 (dead - will be removed by DCE)
    %r3 = Add %r2, %r4    %r3 = Add %r1, %r4

  What is eliminated:
    - ssaCopyInt/Float/String instructions
    - Redundant register-to-register moves
    - Temporary register allocations

  What is preserved:
    - First definition (source of copy)
    - All other instructions
    - SSA form integrity

  Phase: Early optimization (post-SSA, before DCE)
  Author: Sedai Project - Optimization Pipeline
  Date: 2025-01-26
  ============================================================================ }

unit SedaiCopyProp;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, contnrs, SedaiSSATypes, Generics.Collections;

type
  { TCopyPropagation - Eliminate redundant copies }
  TCopyPropagation = class
  private
    FProgram: TSSAProgram;
    FReplacements: Integer;

    // ⛔⛔ THE LAST DEFINITION OF EACH REGISTER SO FAR IN THIS BLOCK, because the question
    // TryReplaceCopy asks - "which instruction before me defines this register?" - was answered by
    // SCANNING BACK TO THE TOP OF THE BLOCK, once per operand and three operands per instruction.
    // A module with 16 000 CONST lines is ONE block of tens of thousands of instructions, so the
    // pass cost N**2: perf put 32.3% of that compile inside PropagateCopies, 14.5% of it the scan
    // itself and most of the rest TFPGObjectList.Get feeding it.
    // ⚠️ TWO maps, because the two questions are NOT the same one:
    //   FLastDefVer - keyed on bank+index+VERSION, the definition being looked for;
    //   FLastDefAny - keyed on bank+index, ANY version, which is what the clobber test asks
    //                 ("was the copy's SOURCE redefined between the copy and this use?").
    // Merging them would answer the soundness question with the wrong register.
    FLastDefVer: specialize TDictionary<QWord, Integer>;
    FLastDefAny: specialize TDictionary<QWord, Integer>;

    // COPYPROP_CHECK=1: answer every question BOTH ways - the map and the old backward scan - and
    // print each disagreement. ⛔ A zero from this is worth believing only after it has been
    // SABOTAGED: build it, break the map on purpose, and see it speak.
    FCheck: Boolean;
    FDisagree: Integer;

    { The pre-map implementation, kept ONLY as the oracle of the check knob }
    function TryReplaceCopyScan(const RegVal: TSSAValue; CurrBlock: TSSABasicBlock;
      CurrInstrIdx: Integer; out Replaced: TSSAValue): Boolean;

    { Key of a register operand: bank | index | version, and bank | index }
    function VerKey(const V: TSSAValue): QWord; inline;
    function AnyKey(const V: TSSAValue): QWord; inline;

    { Try to replace a register operand with its copy source }
    function TryReplaceCopy(const RegVal: TSSAValue; CurrBlock: TSSABasicBlock;
      CurrInstrIdx: Integer; out Replaced: TSSAValue): Boolean;
    function TryReplaceCopyMap(const RegVal: TSSAValue; CurrBlock: TSSABasicBlock;
      CurrInstrIdx: Integer; out Replaced: TSSAValue): Boolean;

    { Propagate copies through all instructions }
    procedure PropagateCopies;

  public
    constructor Create(Prog: TSSAProgram);
    destructor Destroy; override;

    { Run copy propagation pass - returns number of replacements }
    function Run: Integer;
  end;

implementation

{$IFDEF DEBUG_COPYPROP}
uses SedaiDebug;
{$ENDIF}

{ TCopyPropagation }

constructor TCopyPropagation.Create(Prog: TSSAProgram);
begin
  inherited Create;
  FProgram := Prog;
  FReplacements := 0;
  FCheck := GetEnvironmentVariable('COPYPROP_CHECK') <> '';
  FDisagree := 0;
  FLastDefVer := specialize TDictionary<QWord, Integer>.Create;
  FLastDefAny := specialize TDictionary<QWord, Integer>.Create;
end;

destructor TCopyPropagation.Destroy;
begin
  FLastDefVer.Free;
  FLastDefAny.Free;
  inherited;
end;

function TCopyPropagation.Run: Integer;
begin
  {$IFDEF DEBUG_COPYPROP}
  if DebugCopyProp then
    WriteLn('[CopyProp] Running copy propagation...');
  {$ENDIF}

  // Propagate copies through instructions
  PropagateCopies;

  {$IFDEF DEBUG_COPYPROP}
  if DebugCopyProp then
    WriteLn('[CopyProp] Made ', FReplacements, ' copy propagations');
  {$ENDIF}
  if FCheck then
    WriteLn(ErrOutput, '[CopyProp] CHECK replacements=', FReplacements,
            ' disagreements=', FDisagree);
  Result := FReplacements;
end;

function TCopyPropagation.VerKey(const V: TSSAValue): QWord;
begin
  // 4 bits of bank, 28 of register index, 32 of version - every field is far inside its width, so
  // two different registers cannot spell the same key.
  Result := (QWord(Ord(V.RegType)) shl 60) or (QWord(V.RegIndex and $FFFFFFF) shl 32) or
            QWord(Cardinal(V.Version));
end;

function TCopyPropagation.AnyKey(const V: TSSAValue): QWord;
begin
  Result := (QWord(Ord(V.RegType)) shl 60) or (QWord(V.RegIndex and $FFFFFFF) shl 32);
end;

function TCopyPropagation.TryReplaceCopyScan(const RegVal: TSSAValue;
  CurrBlock: TSSABasicBlock; CurrInstrIdx: Integer; out Replaced: TSSAValue): Boolean;
// The implementation the map replaced, verbatim: walk back to the top of the block looking for the
// definition of RegVal, and refuse if the copy's source was written in the gap. It is O(block), which
// is why it is no longer the one that runs - it stays as the ORACLE of COPYPROP_CHECK=1.
var
  Instr: TSSAInstruction;
  j, k: Integer;
  Src: TSSAValue;
begin
  Result := False;
  if RegVal.Kind <> svkRegister then
    Exit;
  for j := CurrInstrIdx - 1 downto 0 do
  begin
    Instr := CurrBlock.Instructions[j];
    if (Instr.Dest.Kind = svkRegister) and
       (Instr.Dest.RegIndex = RegVal.RegIndex) and
       (Instr.Dest.RegType = RegVal.RegType) and
       (Instr.Dest.Version = RegVal.Version) then
    begin
      if OpIn(Instr.OpCode, [ssaCopyInt, ssaCopyFloat, ssaCopyString]) then
      begin
        if Instr.Src1.Kind = svkRegister then
        begin
          Src := Instr.Src1;
          for k := j + 1 to CurrInstrIdx - 1 do
            if (CurrBlock.Instructions[k].Dest.Kind = svkRegister) and
               (CurrBlock.Instructions[k].Dest.RegIndex = Src.RegIndex) and
               (CurrBlock.Instructions[k].Dest.RegType = Src.RegType) then
              Exit(False);
          Replaced := Src;
          Exit(True);
        end;
      end;
      Exit(False);
    end;
  end;
end;

function TCopyPropagation.TryReplaceCopyMap(const RegVal: TSSAValue;
  CurrBlock: TSSABasicBlock; CurrInstrIdx: Integer; out Replaced: TSSAValue): Boolean;
var
  Instr: TSSAInstruction;
  j, kLast: Integer;
  Src: TSSAValue;
begin
  // Conservative approach: only look BACKWARDS in the current block
  // This ensures we only propagate copies that are defined before use
  // and avoids SSA dominance violations

  Result := False;

  if RegVal.Kind <> svkRegister then
    Exit;

  // The definition, straight out of the map instead of walking back to the top of the block. It is
  // the LAST instruction before this one that defines this exact register AND version - which is
  // precisely the one the backward scan used to stop at, because the scan skipped a same-register
  // definition carrying a different version and kept going.
  if not FLastDefVer.TryGetValue(VerKey(RegVal), j) then
    Exit;

  Instr := CurrBlock.Instructions[j];

  // Found the definition - check if it's a copy
  if not OpIn(Instr.OpCode, [ssaCopyInt, ssaCopyFloat, ssaCopyString]) then
    Exit;   // not a copy: nothing to propagate

  if Instr.Src1.Kind <> svkRegister then
    Exit;

  Src := Instr.Src1;
  // SOUNDNESS: do not propagate the copy if its SOURCE is redefined between the copy (j)
  // and this use (CurrInstrIdx). Under global-variable semantics (Version=0) the copy's
  // source register can be reassigned in between — e.g. the swap  T=A : A=B : B=T  would
  // otherwise rewrite  B=T  to  B=A  and read A's NEW value. (In versioned SSA the source
  // would carry a distinct version and this never triggers; the check is just conservative.)
  // ⚠️ The map holds only definitions at indices BELOW the current instruction, so its answer for
  // the source IS the largest k < CurrInstrIdx defining it: "something clobbered it in the gap" and
  // "the last definition of it lies after the copy" are the same statement.
  if FLastDefAny.TryGetValue(AnyKey(Src), kLast) and (kLast > j) then
    Exit(False);   // source clobbered in the gap -> unsafe to propagate

  Replaced := Src;
  Result := True;
end;

function TCopyPropagation.TryReplaceCopy(const RegVal: TSSAValue;
  CurrBlock: TSSABasicBlock; CurrInstrIdx: Integer; out Replaced: TSSAValue): Boolean;
// What the pass actually asks. Normally the map answers alone; under COPYPROP_CHECK=1 the old scan
// answers the same question and every disagreement is printed with the operand that produced it.
var
  ScanRes: Boolean;
  ScanVal: TSSAValue;
begin
  Result := TryReplaceCopyMap(RegVal, CurrBlock, CurrInstrIdx, Replaced);
  if not FCheck then Exit;
  FillChar(ScanVal, SizeOf(ScanVal), 0);
  ScanRes := TryReplaceCopyScan(RegVal, CurrBlock, CurrInstrIdx, ScanVal);
  if (ScanRes <> Result) or
     (Result and ((ScanVal.RegType <> Replaced.RegType) or
                  (ScanVal.RegIndex <> Replaced.RegIndex) or
                  (ScanVal.Version <> Replaced.Version))) then
  begin
    Inc(FDisagree);
    WriteLn(ErrOutput, '[CopyProp] DISAGREE at instr ', CurrInstrIdx,
            ' operand bank=', Ord(RegVal.RegType), ' r', RegVal.RegIndex, '.v', RegVal.Version,
            ' map=', Ord(Result), ' scan=', Ord(ScanRes));
  end;
end;

procedure TCopyPropagation.PropagateCopies;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  i, j: Integer;
  ReplacedVal: TSSAValue;
begin
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    // The maps describe THIS block only - the pass never looks past a block boundary.
    FLastDefVer.Clear;
    FLastDefAny.Clear;
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      // Do NOT propagate into a call-argument staging store (ssaXferStore*). These sit at the very end
      // of a block, right before the ssaCallSub that consumes them, so the block boundary IS a call.
      // Replacing the staged copy's DEST with its SOURCE means the copy's destination register is no
      // longer used in this block but is still used AFTER the call (later blocks) — e.g. merge sort's
      // "iMiddle" staged into the first recursive call and reused by the second call and the merge.
      // That leaves the destination live across the call with no use in the defining block, a pattern
      // the register allocator mishandles across the frame save/restore, miscompiling the callee's
      // argument (a size/data-dependent wrong result). Since the copy is reused after the call it cannot
      // be eliminated anyway, so propagating here yields no benefit — skip it.
      // ⛔ Skipped for REPLACEMENT, never for BOOKKEEPING: it still defines its destination, and the
      // scan this replaces stopped at it like at any other definition.
      if not OpIn(Instr.OpCode, [ssaXferStoreInt, ssaXferStoreFloat, ssaXferStoreString]) then
      begin
        // Try to replace Src1 in-place
        if TryReplaceCopy(Instr.Src1, Block, j, ReplacedVal) then
        begin
          Instr.Src1 := ReplacedVal;
          Inc(FReplacements);
        end;

        // Try to replace Src2 in-place
        if TryReplaceCopy(Instr.Src2, Block, j, ReplacedVal) then
        begin
          Instr.Src2 := ReplacedVal;
          Inc(FReplacements);
        end;

        // Try to replace Src3 in-place
        if TryReplaceCopy(Instr.Src3, Block, j, ReplacedVal) then
        begin
          Instr.Src3 := ReplacedVal;
          Inc(FReplacements);
        end;
      end;

      // ...and only NOW is this instruction a definition the ones after it can see. Recording it
      // before the three questions would let an instruction find ITSELF as its operand's definition.
      if Instr.Dest.Kind = svkRegister then
      begin
        FLastDefVer.AddOrSetValue(VerKey(Instr.Dest), j);
        FLastDefAny.AddOrSetValue(AnyKey(Instr.Dest), j);
      end;
    end;
  end;
end;

end.
