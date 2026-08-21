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
  Unit: SedaiXferForward - forward a call-argument slot to its reader

  WHAT IT REMOVES. SUB/FUNCTION inlining splices the callee's body into the
  caller, but it does NOT remove the argument protocol around it: the arguments
  are still staged into the transfer bank and read straight back out. The
  inlined body of a three-statement SUB looks like this, once per iteration:

      XferStore  R3 -> X0        <- stage the argument
      XferStore  R1 -> X1
      XferLoad   R5 <- X0        <- and read it back, with nothing in between
      XferLoad   R6 <- X1
      ... the inlined body, using R5 and R6 ...
      XferLoad   R0 <- X255      <- the "result" of a SUB that returns nothing

  Five instructions of fourteen in the hot loop, writing a slot and reading it
  again. Measured 21 Aug 2026 against the same three statements written inline
  by hand: 0.176 s against 0.113 s, so the protocol costs 52% of a small
  inlined call and is the entire difference between them.

  WHY NOBODY ELSE REMOVES IT. CopyProp deliberately refuses to touch an
  XferStore (its own comment records the miscompile that taught it: replacing
  the staged copy's destination leaves a register live across a call with no
  use in its defining block, which the register allocator mishandles), and DCE
  lists every Xfer opcode as side-effecting, because in general they carry
  values across a call frame. Both are right in general. What changed is that
  after inlining there is NO CALL between the store and the load, and a
  transfer slot with no call across it is not observable by anyone.

  ⛔ THE SAFETY CONDITION IS EXACTLY THAT, AND IT IS CHECKED, NOT ASSUMED.
  A store/load pair is forwarded only when all of these hold:
    - both are in the SAME basic block, with the store before the load;
    - no call of any kind appears between them (ssaCallSub, ssaCallSubIndirect,
      ssaCall, ssaReturn*) - a call is what makes the bank observable;
    - no other write to that slot appears between them;
    - both registers are VERSIONED (Version > 0). Under CLASSIC global-by-name
      semantics a register name does not denote one value, and forwarding one
      would be the same mistake SedaiAlgebraic's constant map records.
  The store itself is removed only when, in addition, no call appears anywhere
  AFTER it in the block - otherwise some later call might still read the slot.

  Phase: after SUB inlining, before Copy Propagation.
  Date: 2026-08-21
  ============================================================================ }

unit SedaiXferForward;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I DebugFlags.inc}
{$I OptimizationFlags.inc}

interface

uses
  Classes, SysUtils, SedaiSSATypes;

function RunXferForward(Prog: TSSAProgram): Integer;

implementation

function IsCall(Op: TSSAOpCode): Boolean; inline;
// Anything that can hand control to code which reads the transfer bank. Deliberately WIDE: a name
// missing from this list is a miscompile, while a name too many only costs an optimisation.
begin
  Result := OpIn(Op, [ssaCallSub, ssaCallSubIndirect, ssaCall, ssaReturn, ssaReturnSub,
                      ssaThreadCreate, ssaThreadWait]);
end;

function StoreOf(Op: TSSAOpCode): TSSAOpCode; inline;
// The load opcode each store opcode pairs with, or ssaNop when it is not a transfer store.
begin
  case Op of
    ssaXferStoreInt:    Result := ssaXferLoadInt;
    ssaXferStoreFloat:  Result := ssaXferLoadFloat;
    ssaXferStoreString: Result := ssaXferLoadString;
  else                  Result := ssaNop;
  end;
end;

function SlotOf(const Instr: TSSAInstruction; out Slot: Int64): Boolean; inline;
begin
  Result := Instr.Src3.Kind = svkConstInt;
  if Result then Slot := Instr.Src3.ConstInt else Slot := -1;
end;

function CopyFor(Op: TSSAOpCode): TSSAOpCode; inline;
begin
  case Op of
    ssaXferLoadInt:    Result := ssaCopyInt;
    ssaXferLoadFloat:  Result := ssaCopyFloat;
  else                 Result := ssaCopyString;
  end;
end;

var
  D_Seen, D_NoSlot, D_NotReg, D_Redefined, D_NotStraight, D_ManyPreds, D_Call, D_LoadUnversioned, D_NoReader: Integer;

function RunXferForward(Prog: TSSAProgram): Integer;
var
  bi, i, j, k: Integer;
  Blk: TSSABasicBlock;
  St, Ld: TSSAInstruction;
  StSlot, LdSlot: Int64;
  WantLoad: TSSAOpCode;
  Blocked, CallAfter, Forwarded: Boolean;
  Scan: TSSABasicBlock;
  Last: TSSAInstruction;
  ScanFrom, Hops: Integer;
begin
  Result := 0;
  // XFER_OFF=1 disables the pass on one binary, which is what lets a failure be attributed to this
  // pass or to whatever else changed alongside it.
  if GetEnvironmentVariable('XFER_OFF') = '1' then Exit;
  D_Seen := 0; D_NoSlot := 0; D_NotReg := 0; D_Redefined := 0; D_NotStraight := 0; D_ManyPreds := 0; D_Call := 0;
  D_LoadUnversioned := 0; D_NoReader := 0;
  for bi := 0 to Prog.Blocks.Count - 1 do
  begin
    Blk := Prog.Blocks[bi];
    for i := 0 to Blk.Instructions.Count - 1 do
    begin
      St := Blk.Instructions[i];
      WantLoad := StoreOf(St.OpCode);
      if WantLoad = ssaNop then Continue;
      Inc(D_Seen);
      if not SlotOf(St, StSlot) then begin Inc(D_NoSlot); Continue; end;
      if St.Src1.Kind <> svkRegister then begin Inc(D_NotReg); Continue; end;

      // Walk forward to the first reader of this slot, refusing at the first thing that could make
      // the bank observable or could change what the slot holds.
      //
      // ⛔ AND IT HAS TO CROSS A BLOCK BOUNDARY, WHICH IS WHERE THE FIRST VERSION FAILED. Inlining
      // leaves the staged arguments at the very END of the caller's block, followed by a Jump, and
      // puts the reads at the top of the spliced body. A pass confined to one basic block sees the
      // stores, finds no reader, and does nothing - which is exactly what the diagnostic reported
      // before this loop learned to follow the edge.
      //
      // Following it is sound only under a condition that is checked: the block must end in an
      // UNCONDITIONAL jump and the target must have exactly ONE predecessor. Then everything true
      // at the end of this block is true at the start of that one, with no other path into it.
      Blocked := False;
      Forwarded := False;
      Scan := Blk;
      ScanFrom := i + 1;
      Hops := 0;
      while (not Blocked) and (not Forwarded) do
      begin
        for j := ScanFrom to Scan.Instructions.Count - 1 do
        begin
          Ld := Scan.Instructions[j];
          if IsCall(Ld.OpCode) then begin Blocked := True; Inc(D_Call); Break; end;
          // The condition that actually matters: nothing between the store and the load may write
          // the source register. The first version of this pass demanded a VERSIONED source
          // instead, reasoning by analogy with SedaiAlgebraic's constant map - and the diagnostic
          // said every staged argument was Version 0, so the pass was inert. Versioning is a PROXY
          // for "can this register hold a different value by then"; here the question can be
          // answered directly, which is both safer and strictly more permissive.
          if (Ld.Dest.Kind = svkRegister) and (Ld.Dest.RegType = St.Src1.RegType) and
             (Ld.Dest.RegIndex = St.Src1.RegIndex) and (Ld.OpCode <> WantLoad) then
          begin Blocked := True; Inc(D_Redefined); Break; end;
          // A second write to the same slot ends this store's reign.
          if (StoreOf(Ld.OpCode) <> ssaNop) and SlotOf(Ld, LdSlot) and (LdSlot = StSlot) then
          begin Blocked := True; Break; end;
          if (Ld.OpCode = WantLoad) and SlotOf(Ld, LdSlot) and (LdSlot = StSlot) then
          begin
            if Ld.Dest.Kind <> svkRegister then
            begin Blocked := True; Inc(D_LoadUnversioned); Break; end;
            // The load becomes a plain register copy. Copy propagation and DCE, which both already
            // understand copies, take it from here - this pass does not try to be them.
            Ld.OpCode := CopyFor(WantLoad);
            Ld.Src1 := St.Src1;
            Ld.Src2 := MakeSSAValue(svkNone);
            Ld.Src3 := MakeSSAValue(svkNone);
            Inc(Result);
            Forwarded := True;
            Break;
          end;
        end;
        if Blocked or Forwarded then Break;
        // Fall off the end of the block: follow the edge only when it is the unambiguous one.
        Inc(Hops);
        if Hops > 4 then Break;                                  // a bound, not a rule
        if Scan.Instructions.Count = 0 then Break;
        Last := Scan.Instructions[Scan.Instructions.Count - 1];
        if Last.OpCode <> ssaJump then begin Inc(D_NotStraight); Break; end;
        if Scan.Successors.Count <> 1 then begin Inc(D_NotStraight); Break; end;
        Scan := TSSABasicBlock(Scan.Successors[0]);
        if Scan.Predecessors.Count <> 1 then begin Inc(D_ManyPreds); Break; end;
        ScanFrom := 0;
      end;
      if Blocked then Continue;
      if not Forwarded then
      begin
        Inc(D_NoReader);
        if GetEnvironmentVariable('XFER_DIAG') = '1' then
        begin
          WriteLn(ErrOutput, '[XFER]   store slot=', StSlot, ' op=', SSAOpCodeToString(St.OpCode),
                  ' src=R', St.Src1.RegIndex, '_', St.Src1.Version,
                  ' nel blocco ', Blk.LabelName, ' (', Blk.Instructions.Count, ' istruzioni)');
          for k := i + 1 to Blk.Instructions.Count - 1 do
            WriteLn(ErrOutput, '[XFER]     dopo: ',
                    SSAOpCodeToString(TSSAInstruction(Blk.Instructions[k]).OpCode));
        end;
      end;

      // The store may go only if nothing after it in ITS OWN block can still read the bank. The
      // forwarding above may have crossed an edge; the removal deliberately does not reason past
      // the block it started in.
      CallAfter := False;
      for k := i + 1 to Blk.Instructions.Count - 1 do
        if IsCall(Blk.Instructions[k].OpCode) then begin CallAfter := True; Break; end;
      if (not CallAfter) and Forwarded then
      begin
        St.OpCode := ssaNop;
        St.Src1 := MakeSSAValue(svkNone);
        St.Src3 := MakeSSAValue(svkNone);
        Inc(Result);
      end;
    end;
  end;
  if GetEnvironmentVariable('XFER_DIAG') = '1' then
    WriteLn(ErrOutput, '[XFER] store visti=', D_Seen, '  senza-slot=', D_NoSlot,
            ' non-registro=', D_NotReg, ' sorgente-riscritto=', D_Redefined,
            ' blocco-non-lineare=', D_NotStraight, ' piu-predecessori=', D_ManyPreds,
            ' chiamata-in-mezzo=', D_Call, ' load-non-versionato=', D_LoadUnversioned,
            ' senza-lettore=', D_NoReader, '  INOLTRATI=', Result);
end;

end.
