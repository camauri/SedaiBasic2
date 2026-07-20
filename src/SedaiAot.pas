unit SedaiAot;

{ ============================================================================
  SedaiAot - AOT backend on the SSA IR (plan B, job/docs/PIANO_B1_AOT_DESIGN.md).

  Stage S3 (this file, survey only): slice the flat SSA program into function
  regions and classify each against the B1 scalar op set, so we know - before
  writing any codegen - how many real functions the B1 subset covers and what
  the top bail reasons are. Codegen lands in S4.

  A "function region" is a contiguous run of basic blocks: the module body from
  block 0 ('_entry') up to the first 'PROC_' labeled block, then one region per
  'PROC_' label (the IR has no per-procedure structure; the label prefix is the
  only delimiter). Instruction ordinals are counted over EVERY instruction of
  EVERY block in program order - the exact walk TBytecodeCompiler.Compile uses
  to build the SSA->PC map, so ordinals index that map directly.

  Diagnostics: set AOT_DIAG=1 to print one line per region (NATIVE / BAIL with
  the culprit op) plus a summary; output goes to stderr so program output
  stays byte-comparable.
  ============================================================================ }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SedaiSSATypes, SedaiBytecodeTypes;

type
  TAotRegion = record
    Name: string;                    // 'MAIN' or the procedure name (PROC_ suffix)
    FirstBlock, LastBlock: Integer;  // inclusive block-index range in SSAProg.Blocks
    FirstOrdinal: Integer;           // ordinal of the region's first SSA instruction
    InstrCount: Integer;             // SSA instructions in the region
    EntryPC: Integer;                // final bytecode PC of the first emitted instruction (-1 = none)
    Eligible: Boolean;               // every op is in the B1 scalar set and all jumps stay inside
    BailReason: string;              // first offender ('' when eligible)
  end;
  TAotRegions = array of TAotRegion;

// Slice into regions and classify against the B1 scalar set. Prog supplies the
// SSA->PC map (entry PCs and the cross-check that the map lines up with ProcMap).
function AotSliceAndClassify(SSAProg: TSSAProgram; Prog: TBytecodeProgram): TAotRegions;

// AOT_DIAG=1 printout: per-region verdict + summary + map cross-check warnings.
procedure AotSurvey(SSAProg: TSSAProgram; Prog: TBytecodeProgram);

implementation

uses TypInfo;

// The B1 op set: scalar int/float compute + control flow + the Xfer scalar forms
// (parameter prologue / result epilogue) + frame record marks (no-ops in a function
// that owns no records, which the classifier guarantees by excluding record ops).
function IsB1Op(Op: TSSAOpCode): Boolean;
begin
  case Op of
    ssaLoadConstInt, ssaLoadConstFloat,
    ssaCopyInt, ssaCopyFloat,
    ssaAddInt, ssaSubInt, ssaMulInt, ssaDivInt, ssaModInt, ssaNegInt,
    ssaDivUInt, ssaModUInt,
    ssaAddFloat, ssaSubFloat, ssaMulFloat, ssaDivFloat, ssaNegFloat,
    ssaIntToFloat, ssaFloatToInt, ssaFloatRound, ssaNarrowInt, ssaNarrowSingle,
    ssaCmpEqInt, ssaCmpNeInt, ssaCmpLtInt, ssaCmpGtInt, ssaCmpLeInt, ssaCmpGeInt,
    ssaCmpLtUInt, ssaCmpGtUInt, ssaCmpLeUInt, ssaCmpGeUInt,
    ssaCmpEqFloat, ssaCmpNeFloat, ssaCmpLtFloat, ssaCmpGtFloat, ssaCmpLeFloat, ssaCmpGeFloat,
    ssaBitwiseAnd, ssaBitwiseOr, ssaBitwiseXor, ssaBitwiseNot,
    ssaShl, ssaShr, ssaShrUInt,
    ssaMathSqr, ssaMathAbs,
    ssaLabel, ssaNop, ssaJump, ssaJumpIfZero, ssaJumpIfNotZero,
    ssaXferLoadInt, ssaXferLoadFloat, ssaXferStoreInt, ssaXferStoreFloat,
    ssaReturnSub, ssaEnd, ssaStop,
    ssaRecMarkPush, ssaRecMarkPop:
      Result := True;
  else
    Result := False;
  end;
end;

function OpName(Op: TSSAOpCode): string;
begin
  Result := GetEnumName(TypeInfo(TSSAOpCode), Ord(Op));
end;

function AotSliceAndClassify(SSAProg: TSSAProgram; Prog: TBytecodeProgram): TAotRegions;
var
  Regions: TAotRegions;
  NRegions: Integer;
  BlockOrdinal: array of Integer;   // block index -> ordinal of its first instruction
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  RegionLabels: TStringList;        // labels of the current region's blocks (jump containment)
  i, j, Ordinal, r, o: Integer;

  procedure StartRegion(const AName: string; ABlock, AOrdinal: Integer);
  begin
    if NRegions >= Length(Regions) then SetLength(Regions, NRegions * 2 + 8);
    with Regions[NRegions] do
    begin
      Name := AName;
      FirstBlock := ABlock;
      LastBlock := ABlock;
      FirstOrdinal := AOrdinal;
      InstrCount := 0;
      EntryPC := -1;
      Eligible := True;
      BailReason := '';
    end;
    Inc(NRegions);
  end;

begin
  Result := nil;
  if (SSAProg = nil) or (SSAProg.Blocks.Count = 0) then Exit;
  SetLength(Regions, 8);
  NRegions := 0;

  // Pass 0: block -> ordinal of first instruction (ordinals count every instruction
  // of every block in program order, matching the bytecode compiler's emission walk).
  SetLength(BlockOrdinal, SSAProg.Blocks.Count);
  Ordinal := 0;
  for i := 0 to SSAProg.Blocks.Count - 1 do
  begin
    BlockOrdinal[i] := Ordinal;
    Inc(Ordinal, SSAProg.Blocks[i].Instructions.Count);
  end;

  // Pass 1: slice at PROC_ labels.
  StartRegion('MAIN', 0, 0);
  for i := 0 to SSAProg.Blocks.Count - 1 do
  begin
    Block := SSAProg.Blocks[i];
    if (i > 0) and (Copy(Block.LabelName, 1, 5) = 'PROC_') then
      StartRegion(Copy(Block.LabelName, 6, MaxInt), i, BlockOrdinal[i]);
    Regions[NRegions - 1].LastBlock := i;
  end;
  SetLength(Regions, NRegions);

  // Pass 2: classify each region.
  RegionLabels := TStringList.Create;
  try
    RegionLabels.Sorted := True;
    RegionLabels.Duplicates := dupIgnore;
    for r := 0 to NRegions - 1 do
    begin
      RegionLabels.Clear;
      for i := Regions[r].FirstBlock to Regions[r].LastBlock do
        if SSAProg.Blocks[i].LabelName <> '' then
          RegionLabels.Add(SSAProg.Blocks[i].LabelName);

      o := Regions[r].FirstOrdinal;
      for i := Regions[r].FirstBlock to Regions[r].LastBlock do
      begin
        Block := SSAProg.Blocks[i];
        for j := 0 to Block.Instructions.Count - 1 do
        begin
          Instr := Block.Instructions[j];
          Inc(Regions[r].InstrCount);
          // Entry PC = first instruction of the region that emitted bytecode.
          if (Regions[r].EntryPC < 0) and (Prog <> nil) then
            Regions[r].EntryPC := Prog.GetSsaPc(o);
          if Regions[r].Eligible then
          begin
            if not IsB1Op(Instr.OpCode) then
            begin
              Regions[r].Eligible := False;
              Regions[r].BailReason := OpName(Instr.OpCode);
            end
            // A jump leaving the region means the region is not a self-contained
            // function (interleaved code, computed flow): not compilable as a unit.
            else if (Instr.OpCode in [ssaJump, ssaJumpIfZero, ssaJumpIfNotZero]) and
                    (Instr.Dest.Kind = svkLabel) and
                    (RegionLabels.IndexOf(Instr.Dest.LabelName) < 0) then
            begin
              Regions[r].Eligible := False;
              Regions[r].BailReason := 'jump-out:' + Instr.Dest.LabelName;
            end;
          end;
          Inc(o);
        end;
      end;
    end;
  finally
    RegionLabels.Free;
  end;
  Result := Regions;
end;

procedure AotSurvey(SSAProg: TSSAProgram; Prog: TBytecodeProgram);
var
  Regions: TAotRegions;
  r, NElig: Integer;
  ProcAtEntry: string;
begin
  Regions := AotSliceAndClassify(SSAProg, Prog);
  NElig := 0;
  for r := 0 to High(Regions) do
  begin
    with Regions[r] do
    begin
      if Eligible then
      begin
        Inc(NElig);
        WriteLn(ErrOutput, Format('[AOT] %-24s blocks=%-4d instrs=%-5d entryPC=%-6d NATIVE',
                                  [Name, LastBlock - FirstBlock + 1, InstrCount, EntryPC]));
      end
      else
        WriteLn(ErrOutput, Format('[AOT] %-24s blocks=%-4d instrs=%-5d entryPC=%-6d BAIL %s',
                                  [Name, LastBlock - FirstBlock + 1, InstrCount, EntryPC, BailReason]));
      // Cross-check the S2 plumbing: the proc map must agree on who owns the entry PC.
      if (Prog <> nil) and (EntryPC >= 0) and (Name <> 'MAIN') then
      begin
        ProcAtEntry := Prog.GetProcNameAt(EntryPC);
        if ProcAtEntry <> Name then
          WriteLn(ErrOutput, Format('[AOT] WARNING: SSA->PC map says %s starts at PC %d but ProcMap owner there is "%s"',
                                    [Name, EntryPC, ProcAtEntry]));
      end;
    end;
  end;
  WriteLn(ErrOutput, Format('[AOT] survey: %d/%d regions eligible (B1 scalar set)',
                            [NElig, Length(Regions)]));
end;

end.
