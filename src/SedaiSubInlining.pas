{*
 * SedaiBasic - A BASIC interpreter with bytecode VM
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * This program is dual-licensed:
 *
 * 1) For open source use: GNU General Public License version 3 (GPL-3.0-only)
 * 2) For commercial/proprietary use: A separate commercial license is required.
 *    Contact: maurizio.cammalleri@gmail.com for licensing inquiries.
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}
{ ============================================================================
  SedaiSubInlining - SUB/FUNCTION inlining at the SSA level (unification).

  Clones the body of a small LEAF procedure into its static call sites,
  removing the call boundary entirely (no FramePush/FramePop, no call/return
  dispatch). Runs IMMEDIATELY AFTER SSA generation and BEFORE every other
  pass (no PHIs yet, every value at Version 0), so the clones are versioned
  and optimized like hand-written inline code. Every engine benefits: the
  interpreter loses the per-call cost, the loop JIT sees module-level loops
  without calls (no bytecode-level inlining needed), and the AOT compiles
  the whole flattened region natively.

  Register banks are SHARED between caller and callee (FramePush existed
  precisely to snapshot the overlap), so every register the clone touches is
  RENAMED to a freshly allocated one, per site. Parameters are untouched:
  the site's XferStore staging and the callee's XferLoad prologue survive
  verbatim (xfer slots are global, but call-nested order keeps them correct
  exactly as at run time). SHARED variables and named constants go through
  their backing arrays (global ids - not renamed). Return values travel
  through the xfer slots as before.

  V1 INLINABILITY (all required, checked on the callee body):
    - LEAF: no ssaCall / ssaCallSub / ssaCallSubIndirect (also excludes
      recursion), no ssaLoadProcAddr;
    - no error flow (ssaOnError / ssaResume* / ssaTrap);
    - no ssaRecordNew*: without a frame, callee-allocated records would not
      be reclaimed at the "return" - unbounded growth inside caller loops;
    - no ssaArrayDim: a local array's dimension REGISTERS are recorded in
      the shared array METADATA by register index; renaming the body would
      strand that metadata on the un-renamed registers;
    - no ssaArrayBind/Unbind (array parameters alias caller arrays);
    - every jump target is a label inside the callee;
    - at most MAX_INLINE_INSTRS instructions.

  A bug here is visible to the opt-vs-no-opt differential net (the pass is
  gated on GSSAOptimizationsEnabled), unlike B4's hint-only flag.
  ============================================================================ }
unit SedaiSubInlining;

{$mode objfpc}{$H+}
{$I OptimizationFlags.inc}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, TypInfo, SedaiSSATypes;

const
  MAX_INLINE_INSTRS = 250;

type
  TSubInliner = class
  private
    FProgram: TSSAProgram;
    FSiteN: Integer;
    // Per-site register renaming map, one per bank: old index -> new index (-1 = unmapped).
    FRegMap: array[TSSARegisterType] of array of Integer;
    function MapReg(RegType: TSSARegisterType; Idx: Integer): Integer;
    procedure RemapValue(var V: TSSAValue; const LabelMap: TStringList);
    function ProcRange(const ProcLabel: string; out FirstB, LastB: Integer): Boolean;
    function Inlinable(FirstB, LastB: Integer): Boolean;
    function InlineSite(CallBlockIdx, CallInstrIdx: Integer;
                        const ProcLabel: string): Boolean;
  public
    constructor Create(AProgram: TSSAProgram);
    function Run: Integer;   // number of call sites inlined
  end;

implementation

{$IFDEF DEBUG_INLINE}
uses SedaiDebug;
{$ENDIF}

constructor TSubInliner.Create(AProgram: TSSAProgram);
begin
  inherited Create;
  FProgram := AProgram;
  FSiteN := 0;
end;

function TSubInliner.MapReg(RegType: TSSARegisterType; Idx: Integer): Integer;
var
  OldLen, k: Integer;
begin
  if Idx >= Length(FRegMap[RegType]) then
  begin
    // Initialize the WHOLE newly-grown region to -1 (unmapped). Filling only the
    // tail left gaps of zeroes, and 0 reads as "already mapped to register 0" -
    // which silently collapsed distinct callee registers onto INT[0].
    OldLen := Length(FRegMap[RegType]);
    SetLength(FRegMap[RegType], Idx + 32);
    for k := OldLen to High(FRegMap[RegType]) do
      FRegMap[RegType][k] := -1;
  end;
  if FRegMap[RegType][Idx] < 0 then
  begin
    FRegMap[RegType][Idx] := FProgram.AllocRegister(RegType);
    // Keep the clone as versionable as the original was, so the versioning
    // pass and everything downstream treat it like a normal proc-local.
    if FProgram.IsRegVersionable(RegType, Idx) then
      FProgram.AddVersionableReg(RegType, FRegMap[RegType][Idx]);
  end;
  Result := FRegMap[RegType][Idx];
end;

procedure TSubInliner.RemapValue(var V: TSSAValue; const LabelMap: TStringList);
var
  li: Integer;
begin
  if (V.Kind = svkRegister) and (V.RegIndex >= 0) then
    V.RegIndex := MapReg(V.RegType, V.RegIndex)
  else if V.Kind = svkLabel then
  begin
    li := LabelMap.IndexOfName(V.LabelName);
    if li >= 0 then V.LabelName := LabelMap.ValueFromIndex[li];
    // External labels stay - but Inlinable() rejected callees that jump out.
  end;
end;

function TSubInliner.ProcRange(const ProcLabel: string; out FirstB, LastB: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  FirstB := -1;
  for i := 0 to FProgram.Blocks.Count - 1 do
    if FProgram.Blocks[i].LabelName = ProcLabel then begin FirstB := i; Break; end;
  if FirstB < 0 then Exit;
  LastB := FProgram.Blocks.Count - 1;
  for i := FirstB + 1 to FProgram.Blocks.Count - 1 do
    if Copy(FProgram.Blocks[i].LabelName, 1, 5) = 'PROC_' then
    begin LastB := i - 1; Break; end;
  Result := True;
end;

function TSubInliner.Inlinable(FirstB, LastB: Integer): Boolean;
var
  b, j, n: Integer;
  Blk: TSSABasicBlock;
  Ins: TSSAInstruction;
  Local: TStringList;

  function JumpTargetOK(const V: TSSAValue): Boolean;
  begin
    Result := (V.Kind <> svkLabel) or (Local.IndexOf(V.LabelName) >= 0);
  end;

begin
  Result := False;
  n := 0;
  Local := TStringList.Create;
  try
    Local.Sorted := True;
    Local.Duplicates := dupIgnore;
    for b := FirstB to LastB do
      if FProgram.Blocks[b].LabelName <> '' then
        Local.Add(FProgram.Blocks[b].LabelName);
    for b := FirstB to LastB do
    begin
      Blk := FProgram.Blocks[b];
      Inc(n, Blk.Instructions.Count);
      if n > MAX_INLINE_INSTRS then Exit;
      for j := 0 to Blk.Instructions.Count - 1 do
      begin
        Ins := Blk.Instructions[j];
        if OpIn(Ins.OpCode, [ssaCall, ssaCallSub, ssaCallSubIndirect, ssaLoadProcAddr,
                             ssaOnError, ssaResume, ssaResumeNext, ssaResumeLabel, ssaTrap,
                             ssaRecordNew, ssaRecordNewArray, ssaRecordNewArrayInd,
                             ssaRecordNewBlock, ssaArrayDim,
                             ssaArrayBind, ssaArrayBindInd, ssaArrayBindApply,
                             ssaArrayUnbind]) then Exit;
        // V1: no UDT/record machinery and no handle-indirect member ops at all - UDT
        // operator functions inlined naively loop forever (m403/m433); the record
        // handle/RAII model needs its own analysis before it can be flattened.
        if (Pos('Record', GetEnumName(TypeInfo(TSSAOpCode), Ord(Ins.OpCode))) > 0) or
           (Pos('Ind', GetEnumName(TypeInfo(TSSAOpCode), Ord(Ins.OpCode))) > 0) then Exit;
        if OpIn(Ins.OpCode, [ssaJump, ssaJumpIfZero, ssaJumpIfNotZero]) and
           not JumpTargetOK(Ins.Dest) then Exit;
      end;
    end;
    Result := True;
  finally
    Local.Free;
  end;
end;

function TSubInliner.InlineSite(CallBlockIdx, CallInstrIdx: Integer;
                                const ProcLabel: string): Boolean;
var
  FirstB, LastB, b, j, i: Integer;
  rt: TSSARegisterType;
  CallBlk, ContBlk, ProcEntry, Anchor, NewBlk, OrigBlk, SuccBlk: TSSABasicBlock;
  LabelMap: TStringList;             // origLabel=newLabel per cloned block
  // Callee blocks captured as OBJECTS before any insertion: CreateBlockBefore
  // shifts every index at or after the insertion point, so [FirstB..LastB]
  // stops being valid the moment the first clone lands.
  Origs: array of TSSABasicBlock;
  Clones: array of TSSABasicBlock;   // parallel to Origs
  k: Integer;
  Ins, NewIns: TSSAInstruction;
  ContLabel, NewLabel: string;
  si: Integer;
begin
  Result := False;
  if not ProcRange(ProcLabel, FirstB, LastB) then Exit;
  if not Inlinable(FirstB, LastB) then Exit;
  // The call site cannot be inside the callee itself (leafness already excludes
  // recursion, but stay explicit).
  if (CallBlockIdx >= FirstB) and (CallBlockIdx <= LastB) then Exit;

  CallBlk := FProgram.Blocks[CallBlockIdx];
  ProcEntry := FProgram.Blocks[FirstB];
  // The continuation is the call block's successor that is NOT the proc entry
  // (EmitCallSubLabel wired exactly these two).
  ContBlk := nil;
  for i := 0 to CallBlk.Successors.Count - 1 do
    if TSSABasicBlock(CallBlk.Successors[i]) <> ProcEntry then
      ContBlk := TSSABasicBlock(CallBlk.Successors[i]);
  if (ContBlk = nil) or (ContBlk.LabelName = '') then Exit;
  ContLabel := ContBlk.LabelName;
  if CallBlockIdx + 1 >= FProgram.Blocks.Count then Exit;
  Anchor := FProgram.Blocks[CallBlockIdx + 1];   // clones go between call block and this

  Inc(FSiteN);
  si := FSiteN;
  for rt := Low(TSSARegisterType) to High(TSSARegisterType) do
    SetLength(FRegMap[rt], 0);

  SetLength(Origs, LastB - FirstB + 1);
  for b := FirstB to LastB do
    Origs[b - FirstB] := FProgram.Blocks[b];

  LabelMap := TStringList.Create;
  try
    // Fresh labels for every cloned block ('inl' prefix: must NOT look like PROC_).
    for b := 0 to High(Origs) do
    begin
      NewLabel := Format('inl%d_%s', [si, Origs[b].LabelName]);
      LabelMap.Add(Origs[b].LabelName + '=' + NewLabel);
    end;
    // Clone blocks in order, inserted contiguously before the anchor so the
    // callee's internal fall-throughs keep their relative emission order.
    SetLength(Clones, Length(Origs));
    for b := 0 to High(Origs) do
    begin
      OrigBlk := Origs[b];
      NewLabel := LabelMap.Values[OrigBlk.LabelName];
      NewBlk := FProgram.CreateBlockBefore(NewLabel, Anchor);
      Clones[b] := NewBlk;
      for j := 0 to OrigBlk.Instructions.Count - 1 do
      begin
        Ins := OrigBlk.Instructions[j];
        if Ins.OpCode = ssaReturnSub then
        begin
          // The return becomes a plain jump to the site's continuation.
          NewIns := TSSAInstruction.Create(ssaJump);
          NewIns.Dest := MakeSSALabel(ContLabel);
          NewIns.SourceLine := Ins.SourceLine;
          NewBlk.AddInstruction(NewIns);
        end
        else
        begin
          NewIns := Ins.Clone;
          RemapValue(NewIns.Dest, LabelMap);
          RemapValue(NewIns.Src1, LabelMap);
          RemapValue(NewIns.Src2, LabelMap);
          RemapValue(NewIns.Src3, LabelMap);
          for i := 0 to High(NewIns.PhiSources) do
            RemapValue(NewIns.PhiSources[i].Value, LabelMap);
          NewBlk.AddInstruction(NewIns);
        end;
      end;
    end;
    // Clone the internal CFG edges; a block that had the proc's return flows to
    // the continuation (the rewritten jump above), handled below via ReturnSub scan.
    // Successors are matched against the CAPTURED object array, never list indexes.
    for b := 0 to High(Origs) do
    begin
      OrigBlk := Origs[b];
      NewBlk := Clones[b];
      for i := 0 to OrigBlk.Successors.Count - 1 do
      begin
        SuccBlk := TSSABasicBlock(OrigBlk.Successors[i]);
        for k := 0 to High(Origs) do
          if Origs[k] = SuccBlk then
          begin
            NewBlk.AddSuccessor(Clones[k]);
            Clones[k].AddPredecessor(NewBlk);
            Break;
          end;
      end;
      // ReturnSub blocks flow to the continuation.
      for j := 0 to OrigBlk.Instructions.Count - 1 do
        if OrigBlk.Instructions[j].OpCode = ssaReturnSub then
        begin
          NewBlk.AddSuccessor(ContBlk);
          ContBlk.AddPredecessor(NewBlk);
          Break;
        end;
    end;
    // Rewrite the call: drop ssaCallSub, jump into the clone instead.
    Ins := CallBlk.Instructions[CallInstrIdx];
    Ins.OpCode := ssaJump;
    Ins.Dest := MakeSSALabel(LabelMap.Values[ProcLabel]);
    Ins.Src1 := MakeSSAValue(svkNone);
    Ins.Src2 := MakeSSAValue(svkNone);
    Ins.Src3 := MakeSSAValue(svkNone);
    // Edges: call block now reaches only the clone entry.
    CallBlk.Successors.Remove(ProcEntry);
    ProcEntry.Predecessors.Remove(CallBlk);
    CallBlk.Successors.Remove(ContBlk);
    ContBlk.Predecessors.Remove(CallBlk);
    CallBlk.AddSuccessor(Clones[0]);
    Clones[0].AddPredecessor(CallBlk);
    Result := True;
    {$IFDEF DEBUG_INLINE}
    if DebugInline then
      WriteLn('[Inline] site ', si, ': ', ProcLabel, ' -> ', LabelMap.Values[ProcLabel],
              ' (', LastB - FirstB + 1, ' blocks)');
    {$ENDIF}
  finally
    LabelMap.Free;
  end;
end;

function TSubInliner.Run: Integer;
var
  b, j: Integer;
  Blk: TSSABasicBlock;
  Ins: TSSAInstruction;
begin
  Result := 0;
  // Walk a SNAPSHOT of the current block list: inlining inserts blocks, and
  // clones of leaf callees contain no calls, so one pass converges.
  b := 0;
  while b < FProgram.Blocks.Count do
  begin
    Blk := FProgram.Blocks[b];
    j := 0;
    while j < Blk.Instructions.Count do
    begin
      Ins := Blk.Instructions[j];
      if (Ins.OpCode = ssaCallSub) and (Ins.Dest.Kind = svkLabel) and
         (Copy(Ins.Dest.LabelName, 1, 5) = 'PROC_') then
        if InlineSite(b, j, Ins.Dest.LabelName) then
          Inc(Result);
      Inc(j);
    end;
    Inc(b);
  end;
  // Insertions shift positions: refresh every block's cached index.
  if Result > 0 then
    for b := 0 to FProgram.Blocks.Count - 1 do
      FProgram.Blocks[b].BlockIndex := b;
  {$IFDEF DEBUG_INLINE}
  if DebugInline and (Result > 0) then
    for b := 0 to FProgram.Blocks.Count - 1 do
    begin
      Blk := FProgram.Blocks[b];
      Write('[Inline] blk', b, ' "', Blk.LabelName, '" n=', Blk.Instructions.Count, ' succ=[');
      for j := 0 to Blk.Successors.Count - 1 do
        Write(TSSABasicBlock(Blk.Successors[j]).LabelName, ' ');
      WriteLn(']');
      if Copy(Blk.LabelName, 1, 3) = 'inl' then
        for j := 0 to Blk.Instructions.Count - 1 do
          WriteLn('[Inline]   ', Blk.Instructions[j].ToString);
    end;
  {$ENDIF}
end;

end.
