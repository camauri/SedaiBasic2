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
  Unit: SedaiGosubInlining

  Purpose: Inline small GOSUB subroutines to eliminate call/return overhead.

  Strategy:
    1. Identify all subroutines (blocks ending with RETURN)
    2. Count instructions in each subroutine
    3. Count how many times each is called
    4. Inline if: (instruction_count * call_count) < threshold

  Benefits:
    - Eliminates call/return overhead
    - Enables further optimizations across the inlined code
    - Most beneficial for small, frequently-called subroutines

  Phase: SSA optimization (after SSA construction, before other opts)
  Author: Sedai Project
  Date: 2025-01-29
  ============================================================================ }

unit SedaiGosubInlining;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, fgl, SedaiSSATypes;

type
  { TSubroutineInfo - Information about a subroutine }
  TSubroutineInfo = record
    EntryBlock: TSSABasicBlock;   // First block of subroutine (target of GOSUB)
    ReturnBlock: TSSABasicBlock;  // Block containing RETURN
    InstructionCount: Integer;     // Total instructions in subroutine
    CallCount: Integer;            // Number of GOSUB calls to this subroutine
    Blocks: TFPList;               // All blocks in this subroutine
    CanInline: Boolean;            // True if safe to inline
  end;
  PSubroutineInfo = ^TSubroutineInfo;

  { TGosubInlining - Inline small subroutines }
  TGosubInlining = class
  private
    FProgram: TSSAProgram;
    FSubroutines: array of TSubroutineInfo;
    FInlinedCount: Integer;
    FInlineThreshold: Integer;  // Max (instr * calls) for inlining

    { Find all subroutines in the program }
    procedure FindSubroutines;

    { Count instructions in a subroutine }
    function CountInstructions(const Sub: TSubroutineInfo): Integer;

    { Count GOSUB calls to each subroutine }
    procedure CountCalls;

    { Check if subroutine is safe to inline }
    function IsSafeToInline(const Sub: TSubroutineInfo): Boolean;

    { Inline a single call site }
    procedure InlineCallSite(CallBlock: TSSABasicBlock; CallInstrIdx: Integer;
                             const Sub: TSubroutineInfo);

    { Clone a block for inlining }
    function CloneBlock(OrigBlock: TSSABasicBlock; const Suffix: string): TSSABasicBlock;

    { Clone an instruction }
    function CloneInstruction(Instr: TSSAInstruction): TSSAInstruction;

  public
    constructor Create(Prog: TSSAProgram);
    destructor Destroy; override;

    { Run inlining pass - returns number of inlined calls }
    function Run: Integer;

    { Configuration }
    property InlineThreshold: Integer read FInlineThreshold write FInlineThreshold;
  end;

{ Convenience function }
function RunGosubInlining(Prog: TSSAProgram): Integer;

implementation

{$IFDEF DEBUG_SSA}
uses SedaiDebug;
{$ENDIF}

const
  DEFAULT_INLINE_THRESHOLD = 30;  // Max instructions * calls for inlining

{ TGosubInlining }

constructor TGosubInlining.Create(Prog: TSSAProgram);
begin
  inherited Create;
  FProgram := Prog;
  FInlinedCount := 0;
  FInlineThreshold := DEFAULT_INLINE_THRESHOLD;
  SetLength(FSubroutines, 0);
end;

destructor TGosubInlining.Destroy;
var
  i: Integer;
begin
  // Free block lists
  for i := 0 to High(FSubroutines) do
    if Assigned(FSubroutines[i].Blocks) then
      FSubroutines[i].Blocks.Free;
  inherited;
end;

function TGosubInlining.Run: Integer;
var
  i, j, k: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  TargetLabel: string;
  Sub: PSubroutineInfo;
  InlineCandidates: Integer;
begin
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[GosubInline] Starting GOSUB inlining pass...');
  {$ENDIF}

  // Phase 1: Find all subroutines
  FindSubroutines;

  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[GosubInline] Found ', Length(FSubroutines), ' subroutines');
  {$ENDIF}

  if Length(FSubroutines) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  // Phase 2: Count calls to each subroutine
  CountCalls;

  // Phase 3: Determine which subroutines can be inlined
  InlineCandidates := 0;
  for i := 0 to High(FSubroutines) do
  begin
    FSubroutines[i].CanInline := IsSafeToInline(FSubroutines[i]);
    if FSubroutines[i].CanInline then
      Inc(InlineCandidates);

    {$IFDEF DEBUG_SSA}
    if DebugSSA then
    begin
      WriteLn('[GosubInline]   Subroutine ', FSubroutines[i].EntryBlock.LabelName,
              ': ', FSubroutines[i].InstructionCount, ' instrs, ',
              FSubroutines[i].CallCount, ' calls',
              ', inline=', FSubroutines[i].CanInline);
    end;
    {$ENDIF}
  end;

  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[GosubInline] ', InlineCandidates, ' subroutines eligible for inlining');
  {$ENDIF}

  // Phase 4: Inline call sites
  // We iterate backwards to avoid invalidating indices
  for i := FProgram.Blocks.Count - 1 downto 0 do
  begin
    Block := FProgram.Blocks[i];

    for j := Block.Instructions.Count - 1 downto 0 do
    begin
      Instr := Block.Instructions[j];

      if Instr.OpCode = ssaCall then
      begin
        // Find the target subroutine
        if Instr.Dest.Kind = svkLabel then
        begin
          TargetLabel := Instr.Dest.LabelName;

          for k := 0 to High(FSubroutines) do
          begin
            if FSubroutines[k].EntryBlock.LabelName = TargetLabel then
            begin
              if FSubroutines[k].CanInline then
              begin
                {$IFDEF DEBUG_SSA}
                if DebugSSA then
                  WriteLn('[GosubInline] Inlining call to ', TargetLabel,
                          ' in block ', Block.LabelName);
                {$ENDIF}

                InlineCallSite(Block, j, FSubroutines[k]);
                Inc(FInlinedCount);
              end;
              Break;
            end;
          end;
        end;
      end;
    end;
  end;

  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[GosubInline] Inlined ', FInlinedCount, ' call sites');
  {$ENDIF}

  Result := FInlinedCount;
end;

procedure TGosubInlining.FindSubroutines;
var
  i, j: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  Sub: TSubroutineInfo;
  HasReturn: Boolean;
begin
  // Find all blocks that end with RETURN
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];
    HasReturn := False;

    // Check if this block ends with RETURN
    if Block.Instructions.Count > 0 then
    begin
      Instr := Block.Instructions[Block.Instructions.Count - 1];
      if Instr.OpCode = ssaReturn then
        HasReturn := True;
    end;

    if HasReturn then
    begin
      // This block ends a subroutine
      // For now, we assume each RETURN block is the end of a separate subroutine
      // and the entry point is the block itself (simple case)
      // TODO: Handle multi-block subroutines properly

      Sub.EntryBlock := Block;  // Simplified: entry = return block
      Sub.ReturnBlock := Block;
      Sub.Blocks := TFPList.Create;
      Sub.Blocks.Add(Block);
      Sub.InstructionCount := CountInstructions(Sub);
      Sub.CallCount := 0;
      Sub.CanInline := False;

      SetLength(FSubroutines, Length(FSubroutines) + 1);
      FSubroutines[High(FSubroutines)] := Sub;
    end;
  end;
end;

function TGosubInlining.CountInstructions(const Sub: TSubroutineInfo): Integer;
var
  i, j: Integer;
  Block: TSSABasicBlock;
begin
  Result := 0;
  for i := 0 to Sub.Blocks.Count - 1 do
  begin
    Block := TSSABasicBlock(Sub.Blocks[i]);
    Inc(Result, Block.Instructions.Count);
  end;
end;

procedure TGosubInlining.CountCalls;
var
  i, j, k: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  TargetLabel: string;
begin
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];

    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      if Instr.OpCode = ssaCall then
      begin
        if Instr.Dest.Kind = svkLabel then
        begin
          TargetLabel := Instr.Dest.LabelName;

          // Find matching subroutine
          for k := 0 to High(FSubroutines) do
          begin
            if FSubroutines[k].EntryBlock.LabelName = TargetLabel then
            begin
              Inc(FSubroutines[k].CallCount);
              Break;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TGosubInlining.IsSafeToInline(const Sub: TSubroutineInfo): Boolean;
var
  TotalCost: Integer;
  i, j: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
begin
  Result := False;

  // Must have at least one call
  if Sub.CallCount = 0 then
    Exit;

  // Check total cost
  TotalCost := Sub.InstructionCount * Sub.CallCount;
  if TotalCost > FInlineThreshold then
    Exit;

  // Check for problematic instructions
  for i := 0 to Sub.Blocks.Count - 1 do
  begin
    Block := TSSABasicBlock(Sub.Blocks[i]);

    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      // Don't inline if contains nested GOSUB
      if Instr.OpCode = ssaCall then
        Exit;

      // Don't inline if contains complex control flow
      // (jumps to labels outside the subroutine)
      if Instr.OpCode = ssaJump then
      begin
        // Check if jump target is within the subroutine
        // For now, skip inlining any subroutine with jumps
        // TODO: Handle internal jumps properly
        Exit;
      end;
    end;
  end;

  Result := True;
end;

procedure TGosubInlining.InlineCallSite(CallBlock: TSSABasicBlock;
  CallInstrIdx: Integer; const Sub: TSubroutineInfo);
var
  i, j: Integer;
  OrigBlock, ClonedBlock: TSSABasicBlock;
  OrigInstr, ClonedInstr: TSSAInstruction;
  InlineSuffix: string;
  InsertIdx: Integer;
begin
  // Generate unique suffix for this inline site
  InlineSuffix := '_inline_' + IntToStr(FInlinedCount);

  // Remove the CALL instruction
  CallBlock.Instructions.Delete(CallInstrIdx);

  // Insert cloned instructions at the call site
  InsertIdx := CallInstrIdx;

  for i := 0 to Sub.Blocks.Count - 1 do
  begin
    OrigBlock := TSSABasicBlock(Sub.Blocks[i]);

    for j := 0 to OrigBlock.Instructions.Count - 1 do
    begin
      OrigInstr := OrigBlock.Instructions[j];

      // Skip the RETURN instruction - we don't need it when inlining
      if OrigInstr.OpCode = ssaReturn then
        Continue;

      // Clone and insert the instruction
      ClonedInstr := CloneInstruction(OrigInstr);
      ClonedInstr.Comment := ClonedInstr.Comment + InlineSuffix;

      CallBlock.Instructions.Insert(InsertIdx, ClonedInstr);
      Inc(InsertIdx);
    end;
  end;
end;

function TGosubInlining.CloneBlock(OrigBlock: TSSABasicBlock;
  const Suffix: string): TSSABasicBlock;
var
  i: Integer;
  OrigInstr, ClonedInstr: TSSAInstruction;
begin
  Result := TSSABasicBlock.Create(OrigBlock.LabelName + Suffix);

  for i := 0 to OrigBlock.Instructions.Count - 1 do
  begin
    OrigInstr := OrigBlock.Instructions[i];
    ClonedInstr := CloneInstruction(OrigInstr);
    Result.AddInstruction(ClonedInstr);
  end;
end;

function TGosubInlining.CloneInstruction(Instr: TSSAInstruction): TSSAInstruction;
var
  i: Integer;
begin
  Result := TSSAInstruction.Create(Instr.OpCode);
  Result.Dest := Instr.Dest;
  Result.Src1 := Instr.Src1;
  Result.Src2 := Instr.Src2;
  Result.Src3 := Instr.Src3;
  Result.Comment := Instr.Comment;
  Result.SourceLine := Instr.SourceLine;

  // Clone PHI sources if any
  SetLength(Result.PhiSources, Length(Instr.PhiSources));
  for i := 0 to High(Instr.PhiSources) do
    Result.PhiSources[i] := Instr.PhiSources[i];
end;

{ Convenience function }

function RunGosubInlining(Prog: TSSAProgram): Integer;
var
  Inliner: TGosubInlining;
begin
  Inliner := TGosubInlining.Create(Prog);
  try
    Result := Inliner.Run;
  finally
    Inliner.Free;
  end;
end;

end.
