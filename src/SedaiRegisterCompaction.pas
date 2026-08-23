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
  Unit: SedaiRegisterCompaction

  Purpose: Compact register allocation by remapping sparse register numbers
           to a dense, contiguous range. This improves cache locality by
           reducing the working set size.

  Algorithm:
    1. Scan all instructions to find which registers are actually used
    2. Build a mapping from old register numbers to new compact numbers
    3. Rewrite all instructions with the new register numbers

  Benefits:
    - Reduces register file size (R420 → R40-60 typical)
    - Improves L1/L2 cache hit rate
    - Critical for compute-intensive loops (N-body, matrix ops)

  Phase: Post-allocation optimization
  Author: Sedai Project - Performance Optimization
  Date: 2025-01-28
  ============================================================================ }

unit SedaiRegisterCompaction;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, SedaiBytecodeTypes, SedaiSSATypes, SedaiSuperinstructions, SedaiOpcodeBanks;

type
  { TRegisterCompactor - Compacts sparse register allocation }
  TRegisterCompactor = class
  private
    FProgram: TBytecodeProgram;
    FIntRegMap: array of Integer;    // Old int reg → new int reg (-1 = unused)
    FFloatRegMap: array of Integer;  // Old float reg → new float reg
    FStringRegMap: array of Integer; // Old string reg → new string reg
    FMaxOldIntReg: Integer;
    FMaxOldFloatReg: Integer;
    FMaxOldStringReg: Integer;
    FNewIntRegCount: Integer;
    FNewFloatRegCount: Integer;
    FNewStringRegCount: Integer;

    { Scan all instructions to find used registers }
    procedure ScanUsedRegisters;

    { Build compact register mappings }
    procedure BuildMappings;

    { Rewrite all instructions with new register numbers }
    procedure RewriteInstructions;

    { Rewrite array metadata (DimRegisters) with new register numbers }
    procedure RewriteArrayMetadata;

    { Check if opcode uses Dest as integer register }
    function DestIsIntReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Dest as float register }
    function DestIsFloatReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Dest as string register }
    function DestIsStringReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Src1 as integer register }
    function Src1IsIntReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Src1 as float register }
    function Src1IsFloatReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Src2 as integer register }
    function Src2IsIntReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Src2 as float register }
    function Src2IsFloatReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Src1 as string register }
    function Src1IsStringReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Src2 as string register }
    function Src2IsStringReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode READS Dest as int register (e.g., ArrayStore) }
    function DestReadIsIntReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode READS Dest as float register (e.g., ArrayStoreFloat) }
    function DestReadIsFloatReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode READS Dest as string register (e.g., ArrayStoreString) }
    function DestReadIsStringReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Immediate field as float register index
      This is needed for FMA and related superinstructions where Immediate
      stores the 'c' register (accumulator) instead of a constant }
    function ImmediateIsFloatReg(OpCode: TBytecodeOp): Boolean;

    { Check if opcode uses Immediate field as string register index
      This is needed for DOPEN where Immediate stores the mode string register }
    function ImmediateIsStringReg(OpCode: TBytecodeOp): Boolean;

    { Mark a register as used }
    procedure MarkIntRegUsed(Reg: Integer);
    procedure MarkFloatRegUsed(Reg: Integer);
    procedure MarkStringRegUsed(Reg: Integer);

  public
    constructor Create(Prog: TBytecodeProgram);
    destructor Destroy; override;

    { Run compaction - returns reduction count }
    function Run: Integer;

    { Statistics }
    property OldIntRegCount: Integer read FMaxOldIntReg;
    property OldFloatRegCount: Integer read FMaxOldFloatReg;
    property NewIntRegCount: Integer read FNewIntRegCount;
    property NewFloatRegCount: Integer read FNewFloatRegCount;
  end;

{ Convenience function }
function RunRegisterCompaction(Prog: TBytecodeProgram): Integer;

implementation

{$IFDEF DEBUG_REGALLOC}
uses SedaiDebug;
{$ENDIF}

const
  MAX_REGISTERS = 1024;  // Maximum register index we support

{ TRegisterCompactor }

constructor TRegisterCompactor.Create(Prog: TBytecodeProgram);
begin
  inherited Create;
  FProgram := Prog;
  FMaxOldIntReg := 0;
  FMaxOldFloatReg := 0;
  FMaxOldStringReg := 0;
  FNewIntRegCount := 0;
  FNewFloatRegCount := 0;
  FNewStringRegCount := 0;
end;

destructor TRegisterCompactor.Destroy;
begin
  inherited;
end;

function TRegisterCompactor.DestIsIntReg(OpCode: TBytecodeOp): Boolean;
// Delegates to SedaiOpcodeBanks: this classification lives in ONE place now. Two copies of it
// disagreed on 88 (opcode, field) pairs, and a pass that gets the bank wrong miscompiles silently.
begin
  Result := SedaiOpcodeBanks.DestIsIntReg(OpCode);
end;

function TRegisterCompactor.DestIsFloatReg(OpCode: TBytecodeOp): Boolean;
// Delegates to SedaiOpcodeBanks: this classification lives in ONE place now. Two copies of it
// disagreed on 88 (opcode, field) pairs, and a pass that gets the bank wrong miscompiles silently.
begin
  Result := SedaiOpcodeBanks.DestIsFloatReg(OpCode);
end;

function TRegisterCompactor.DestIsStringReg(OpCode: TBytecodeOp): Boolean;
// Delegates to SedaiOpcodeBanks: this classification lives in ONE place now. Two copies of it
// disagreed on 88 (opcode, field) pairs, and a pass that gets the bank wrong miscompiles silently.
begin
  Result := SedaiOpcodeBanks.DestIsStringReg(OpCode);
end;

function TRegisterCompactor.Src1IsIntReg(OpCode: TBytecodeOp): Boolean;
// Delegates to SedaiOpcodeBanks: this classification lives in ONE place now. Two copies of it
// disagreed on 88 (opcode, field) pairs, and a pass that gets the bank wrong miscompiles silently.
begin
  Result := SedaiOpcodeBanks.Src1IsIntReg(OpCode);
end;

function TRegisterCompactor.Src1IsFloatReg(OpCode: TBytecodeOp): Boolean;
// Delegates to SedaiOpcodeBanks: this classification lives in ONE place now. Two copies of it
// disagreed on 88 (opcode, field) pairs, and a pass that gets the bank wrong miscompiles silently.
begin
  Result := SedaiOpcodeBanks.Src1IsFloatReg(OpCode);
end;

function TRegisterCompactor.Src2IsIntReg(OpCode: TBytecodeOp): Boolean;
// Delegates to SedaiOpcodeBanks: this classification lives in ONE place now. Two copies of it
// disagreed on 88 (opcode, field) pairs, and a pass that gets the bank wrong miscompiles silently.
begin
  Result := SedaiOpcodeBanks.Src2IsIntReg(OpCode);
end;

function TRegisterCompactor.Src2IsFloatReg(OpCode: TBytecodeOp): Boolean;
// Delegates to SedaiOpcodeBanks: this classification lives in ONE place now. Two copies of it
// disagreed on 88 (opcode, field) pairs, and a pass that gets the bank wrong miscompiles silently.
begin
  Result := SedaiOpcodeBanks.Src2IsFloatReg(OpCode);
end;

function TRegisterCompactor.Src1IsStringReg(OpCode: TBytecodeOp): Boolean;
// Delegates to SedaiOpcodeBanks: this classification lives in ONE place now. Two copies of it
// disagreed on 88 (opcode, field) pairs, and a pass that gets the bank wrong miscompiles silently.
begin
  Result := SedaiOpcodeBanks.Src1IsStringReg(OpCode);
end;

function TRegisterCompactor.Src2IsStringReg(OpCode: TBytecodeOp): Boolean;
// Delegates to SedaiOpcodeBanks: this classification lives in ONE place now. Two copies of it
// disagreed on 88 (opcode, field) pairs, and a pass that gets the bank wrong miscompiles silently.
begin
  Result := SedaiOpcodeBanks.Src2IsStringReg(OpCode);
end;

function TRegisterCompactor.DestReadIsIntReg(OpCode: TBytecodeOp): Boolean;
// Delegates to SedaiOpcodeBanks: this classification lives in ONE place now. Two copies of it
// disagreed on 88 (opcode, field) pairs, and a pass that gets the bank wrong miscompiles silently.
begin
  Result := SedaiOpcodeBanks.DestReadIsIntReg(OpCode);
end;

function TRegisterCompactor.DestReadIsFloatReg(OpCode: TBytecodeOp): Boolean;
// Delegates to SedaiOpcodeBanks: this classification lives in ONE place now. Two copies of it
// disagreed on 88 (opcode, field) pairs, and a pass that gets the bank wrong miscompiles silently.
begin
  Result := SedaiOpcodeBanks.DestReadIsFloatReg(OpCode);
end;

function TRegisterCompactor.DestReadIsStringReg(OpCode: TBytecodeOp): Boolean;
// Delegates to SedaiOpcodeBanks: this classification lives in ONE place now. Two copies of it
// disagreed on 88 (opcode, field) pairs, and a pass that gets the bank wrong miscompiles silently.
begin
  Result := SedaiOpcodeBanks.DestReadIsStringReg(OpCode);
end;

function TRegisterCompactor.ImmediateIsFloatReg(OpCode: TBytecodeOp): Boolean;
// Delegates to SedaiOpcodeBanks: this classification lives in ONE place now. Two copies of it
// disagreed on 88 (opcode, field) pairs, and a pass that gets the bank wrong miscompiles silently.
begin
  Result := SedaiOpcodeBanks.ImmediateIsFloatReg(OpCode);
end;

function TRegisterCompactor.ImmediateIsStringReg(OpCode: TBytecodeOp): Boolean;
// Delegates to SedaiOpcodeBanks: this classification lives in ONE place now. Two copies of it
// disagreed on 88 (opcode, field) pairs, and a pass that gets the bank wrong miscompiles silently.
begin
  Result := SedaiOpcodeBanks.ImmediateIsStringReg(OpCode);
end;

// The register maps grow on demand to fit any register index, rather than capping at MAX_REGISTERS.
// A program that uses more than MAX_REGISTERS registers before compaction (e.g. a large array
// initializer plus many UDT-by-value temporaries) would otherwise leave those high registers
// unmarked/unremapped, and -- worse -- the identity-mapping loop in BuildCompactMapping would write
// past the fixed map and corrupt the heap. New slots default to -1 ("unused"), as ScanUsedRegisters
// initialises them.
procedure TRegisterCompactor.MarkIntRegUsed(Reg: Integer);
var
  j, oldLen: Integer;
begin
  if Reg < 0 then Exit;
  if Reg > FMaxOldIntReg then
    FMaxOldIntReg := Reg;
  if Reg >= Length(FIntRegMap) then
  begin
    oldLen := Length(FIntRegMap);
    SetLength(FIntRegMap, Reg + 1);
    for j := oldLen to Reg do FIntRegMap[j] := -1;
  end;
  FIntRegMap[Reg] := 0;  // Mark as used (will be remapped later)
end;

procedure TRegisterCompactor.MarkFloatRegUsed(Reg: Integer);
var
  j, oldLen: Integer;
begin
  if Reg < 0 then Exit;
  if Reg > FMaxOldFloatReg then
    FMaxOldFloatReg := Reg;
  if Reg >= Length(FFloatRegMap) then
  begin
    oldLen := Length(FFloatRegMap);
    SetLength(FFloatRegMap, Reg + 1);
    for j := oldLen to Reg do FFloatRegMap[j] := -1;
  end;
  FFloatRegMap[Reg] := 0;  // Mark as used
end;

procedure TRegisterCompactor.MarkStringRegUsed(Reg: Integer);
var
  j, oldLen: Integer;
begin
  if Reg < 0 then Exit;
  if Reg > FMaxOldStringReg then
    FMaxOldStringReg := Reg;
  if Reg >= Length(FStringRegMap) then
  begin
    oldLen := Length(FStringRegMap);
    SetLength(FStringRegMap, Reg + 1);
    for j := oldLen to Reg do FStringRegMap[j] := -1;
  end;
  FStringRegMap[Reg] := 0;  // Mark as used
end;

procedure TRegisterCompactor.ScanUsedRegisters;
var
  i: Integer;
  Instr: TBytecodeInstruction;
  OpCode: TBytecodeOp;
begin
  // Initialize maps with -1 (unused)
  SetLength(FIntRegMap, MAX_REGISTERS);
  SetLength(FFloatRegMap, MAX_REGISTERS);
  SetLength(FStringRegMap, MAX_REGISTERS);

  for i := 0 to MAX_REGISTERS - 1 do
  begin
    FIntRegMap[i] := -1;
    FFloatRegMap[i] := -1;
    FStringRegMap[i] := -1;
  end;

  // String registers a bytecode pass RESERVED: no instruction names them any more, but the SSA the
  // AOT compiles from still does, and the AOT translates its registers through THIS map. Dropping
  // one makes the AOT bail on the whole region ("unmapped-str"), which is how the string-temp fusion
  // cost pidigits its native MAIN. Cheap: a handful of slots that are never read.
  for i := 0 to FProgram.ReservedStringRegCount - 1 do
    MarkStringRegUsed(FProgram.ReservedStringReg(i));

  // Scan all instructions
  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    Instr := FProgram.GetInstruction(i);
    // IMPORTANT: For superinstructions (opcode >= 110), the cast to TBytecodeOp
    // produces an out-of-range value. The Is*Reg functions handle this by
    // checking Ord(OpCode) >= 110 first and using integer comparisons.
    OpCode := TBytecodeOp(Instr.OpCode);

    // Skip NOPs
    if Instr.OpCode = Byte(bcNop) then
      Continue;

    // Check Dest register (written)
    if DestIsIntReg(OpCode) then
      MarkIntRegUsed(Instr.Dest)
    else if DestIsFloatReg(OpCode) then
      MarkFloatRegUsed(Instr.Dest)
    else if DestIsStringReg(OpCode) then
      MarkStringRegUsed(Instr.Dest);

    // Check Dest register when used as SOURCE (e.g., ArrayStore value)
    if DestReadIsIntReg(OpCode) then
      MarkIntRegUsed(Instr.Dest)
    else if DestReadIsFloatReg(OpCode) then
      MarkFloatRegUsed(Instr.Dest)
    else if DestReadIsStringReg(OpCode) then
      MarkStringRegUsed(Instr.Dest);

    // Check Src1 register
    if Src1IsIntReg(OpCode) then
      MarkIntRegUsed(Instr.Src1)
    else if Src1IsFloatReg(OpCode) then
      MarkFloatRegUsed(Instr.Src1)
    else if Src1IsStringReg(OpCode) then
      MarkStringRegUsed(Instr.Src1);

    // Check Src2 register
    if Src2IsIntReg(OpCode) then
      MarkIntRegUsed(Instr.Src2)
    else if Src2IsFloatReg(OpCode) then
      MarkFloatRegUsed(Instr.Src2)
    else if Src2IsStringReg(OpCode) then
      MarkStringRegUsed(Instr.Src2);

    // Check Immediate field when it contains a float register index
    // (for FMA and related superinstructions)
    if ImmediateIsFloatReg(OpCode) then
      MarkFloatRegUsed(Instr.Immediate);

    // Check Immediate field when it contains a string register index
    // (for DOPEN mode parameter)
    if ImmediateIsStringReg(OpCode) then
      MarkStringRegUsed(Instr.Immediate);

    // bcGraphicBox: Immediate contains 5 packed register indices
    // Layout: x2(bits 0-11) | y2(12-23) | angle(24-35) | filled(36-47) | fill_color(48-59)
    // x2, y2, filled, fill_color are int registers; angle is float register
    if OpCode = bcGraphicBox then
    begin
      MarkIntRegUsed((Instr.Immediate) and $FFF);           // x2 - int
      MarkIntRegUsed((Instr.Immediate shr 12) and $FFF);    // y2 - int
      MarkFloatRegUsed((Instr.Immediate shr 24) and $FFF);  // angle - float
      MarkIntRegUsed((Instr.Immediate shr 36) and $FFF);    // filled - int
      MarkIntRegUsed((Instr.Immediate shr 48) and $FFF);    // fill_color - int
    end;

    // bcGraphicSetMode: Immediate = param3 register (int)
    if OpCode = bcGraphicSetMode then
      MarkIntRegUsed(Instr.Immediate);

    // bcStrInstr / bcStrInstrAny: Immediate = the int register holding the INSTR start position.
    if (OpCode = bcStrInstr) or (OpCode = bcStrInstrAny) then
      MarkIntRegUsed(Instr.Immediate and $FFFF);

    // Filesystem-command function forms (Immediate = -1): Dest = int register receiving the
    // error/exit code. The statement form of bcCopyFile instead carries the overwrite-flag
    // INT REGISTER index in Immediate (>= 0).
    if ((OpCode = bcChdir) or (OpCode = bcMkdir) or (OpCode = bcRmdir) or
        (OpCode = bcScratch) or (OpCode = bcCopyFile) or (OpCode = bcShell) or
        (OpCode = bcRenameFile)) and
       ((Instr.Immediate = -1) or ((OpCode = bcShell) and (Instr.Immediate = -2))) then
      MarkIntRegUsed(Instr.Dest);
    if (OpCode = bcCopyFile) and (Instr.Immediate >= 0) then
      MarkIntRegUsed(Instr.Immediate and $FFFF);

    // bcGraphicRGBA: Immediate = (B_reg << 16) | A_reg - two int registers
    if OpCode = bcGraphicRGBA then
    begin
      MarkIntRegUsed(Instr.Immediate and $FFFF);           // A register
      MarkIntRegUsed((Instr.Immediate shr 16) and $FFFF);  // B register
    end;

    // bcSetColor (SETCOLOR): Immediate = B reg (bits 0-11) | A reg (bits 12-23) - two int registers
    if OpCode = bcSetColor then
    begin
      MarkIntRegUsed(Instr.Immediate and $FFF);            // B register
      MarkIntRegUsed((Instr.Immediate shr 12) and $FFF);   // A register
    end;

    // bcArrayLoadDivAddFloat: Immediate = (denom_reg << 16) | acc_reg - two float registers
    if OpCode = bcArrayLoadDivAddFloat then
    begin
      MarkFloatRegUsed(Instr.Immediate and $FFFF);           // acc register
      MarkFloatRegUsed((Instr.Immediate shr 16) and $FFFF);  // denom register
    end;

    // bcStrMid/bcStrMidW: Immediate contains length register index (int)
    // MID$(str, start, length) - start is Src2, length is in Immediate
    // bcStrConcatCharAt: same convention - Immediate is the INDEX register (int).
    // bcStrMidAssignArr carries the START register there too - it is the SHARED/array form of the
    // same statement - and it was missing from both this mark and the remap below. Found 20 Aug 2026
    // by diffing this pass against ImmediateReadsIntReg in SedaiOpcodeBanks, whose own comment claims
    // the two lists are the same set; that claim was false by exactly this one opcode. Left out, the
    // start register can be left out of the used set and the Immediate can keep pointing at a
    // pre-compaction number.
    if (OpCode = bcStrMid) or (OpCode = bcStrMidW) or (OpCode = bcStrAscMid) or
       (OpCode = bcStrConcatCharAt) or (OpCode = bcStrAppendMapped) or
       (OpCode = bcStrMidAssign) or (OpCode = bcStrMidAssignArr) then
      MarkIntRegUsed(Instr.Immediate and $FFFF);

    // bcDateSerial/bcTimeSerial: Immediate contains the 3rd arg (day/second) register index (int)
    if (OpCode = bcDateSerial) or (OpCode = bcTimeSerial) then
      MarkIntRegUsed(Instr.Immediate and $FFFF);

    // FB_MEMCOPY/FB_MEMMOVE/CLEAR: Immediate contains the byte-count register index (int)
    // PUT/GET #n, , *p, n: same convention — Immediate is the byte-count register index.
    if (OpCode = bcRawMemCopy) or (OpCode = bcRawMemMove) or (OpCode = bcRawClear) or
       (OpCode = bcPutBinMem) or (OpCode = bcGetBinMem) then
      MarkIntRegUsed(Instr.Immediate and $FFFF);

    // PSET/PAINT (x,y),color: Immediate contains the color register index (int)
    if (OpCode = bcGfxPset) or (OpCode = bcGfxPaint) then
      MarkIntRegUsed(Instr.Immediate and $FFFF);

    // SCREEN(row,col[,colorflag]): Immediate contains the colorflag register index (int)
    if OpCode = bcConScreen then
      MarkIntRegUsed(Instr.Immediate and $FFFF);

    // IMAGECREATE: Immediate contains the fill-colour register index (int)
    if OpCode = bcGfxImageCreate then
      MarkIntRegUsed(Instr.Immediate and $FFFF);

    // SETMOUSE: Immediate[0-15] contains the visibility register index (int)
    if OpCode = bcSetmouse then
      MarkIntRegUsed(Instr.Immediate and $FFFF);

    // LINE: Immediate [0-15]=x2, [16-31]=y2, [32-47]=color (all int regs; bits 48-49 = shape flag, NOT a reg)
    if OpCode = bcGfxLine then
    begin
      MarkIntRegUsed(Instr.Immediate and $FFFF);            // x2
      MarkIntRegUsed((Instr.Immediate shr 16) and $FFFF);   // y2
      MarkIntRegUsed((Instr.Immediate shr 32) and $FFFF);   // color
    end;

    // CIRCLE: Immediate [0-15]=radius, [16-31]=color (both int regs)
    if OpCode = bcGfxCircle then
    begin
      MarkIntRegUsed(Instr.Immediate and $FFFF);            // radius
      MarkIntRegUsed((Instr.Immediate shr 16) and $FFFF);   // color
    end;

    // PAINT boundary fill: Immediate [0-15]=color, [16-31]=border (both int regs)
    if OpCode = bcGfxPaintBorder then
    begin
      MarkIntRegUsed(Instr.Immediate and $FFFF);            // color
      MarkIntRegUsed((Instr.Immediate shr 16) and $FFFF);   // border
    end;

    // LINE styled: Dest=x2 (an input, not a def); Immediate [0-15]=y2, [16-31]=color, [32-47]=style
    // (all int regs; bits 48-49 = shape flag). Dest handled here, NOT via the Dest-def lists.
    if OpCode = bcGfxLineStyled then
    begin
      MarkIntRegUsed(Instr.Dest);                            // x2
      MarkIntRegUsed(Instr.Immediate and $FFFF);            // y2
      MarkIntRegUsed((Instr.Immediate shr 16) and $FFFF);   // color
      MarkIntRegUsed((Instr.Immediate shr 32) and $FFFF);   // style
    end;

    // CIRCLE (ellipse/arc): Dest=RX (an input, not a def); Immediate [0-15]=RY, [16-31]=color,
    // [32-47]=start°, [48-63]=end° (all int regs). Dest handled here, NOT via the Dest-def lists.
    // ⛔ The FILLED form packs identically, so it belongs to the same test - naming only one of the
    // two here would compact the outline's registers and leave the filled form reading stale ones.
    if (OpCode = bcGfxCircleEx) or (OpCode = bcGfxCircleExF) then
    begin
      MarkIntRegUsed(Instr.Dest);                            // RX
      MarkIntRegUsed(Instr.Immediate and $FFFF);            // RY
      MarkIntRegUsed((Instr.Immediate shr 16) and $FFFF);   // color
      MarkIntRegUsed((Instr.Immediate shr 32) and $FFFF);   // start°
      MarkIntRegUsed((Instr.Immediate shr 48) and $FFFF);   // end°
    end;

    // DRAW STRING: Immediate [0-15]=y, [16-31]=colour (int regs). Src1 (the text) is a string reg and
    // Src2 (x) an int reg, both covered by the ordinary Src lists.
    if OpCode = bcGfxDrawString then
    begin
      MarkIntRegUsed(Instr.Immediate and $FFFF);            // y
      MarkIntRegUsed((Instr.Immediate shr 16) and $FFFF);   // colour
    end;

    // IMAGECONVERTROW: Immediate [0-15]=src_bpp, [16-31]=dst_bpp, [32-47]=width, [48-63]=isrgb (int regs).
    // Src1/Src2 (the two addresses) are covered by the ordinary Src lists.
    if OpCode = bcGfxImageConvertRow then
    begin
      MarkIntRegUsed(Instr.Immediate and $FFFF);            // src_bpp
      MarkIntRegUsed((Instr.Immediate shr 16) and $FFFF);   // dst_bpp
      MarkIntRegUsed((Instr.Immediate shr 32) and $FFFF);   // width
      MarkIntRegUsed((Instr.Immediate shr 48) and $FFFF);   // isrgb
    end;

    // GET: Immediate [0-15]=x2, [16-31]=y2, [32-47]=dst handle (all int regs)
    if OpCode = bcGfxGet then
    begin
      MarkIntRegUsed(Instr.Immediate and $FFFF);            // x2
      MarkIntRegUsed((Instr.Immediate shr 16) and $FFFF);   // y2
      MarkIntRegUsed((Instr.Immediate shr 32) and $FFFF);   // dst handle
    end;

    // PUT: Immediate [0-15]=src handle (int reg; bits 16-31 = mode ordinal, NOT a reg)
    if OpCode = bcGfxPut then
    begin
      MarkIntRegUsed(Instr.Immediate and $FFFF);            // src handle
      MarkIntRegUsed((Instr.Immediate shr 32) and $FFFF);   // ALPHA/ADD blend value
    end;

    // WINDOW/VIEW: Immediate [0-15]=x2, [16-31]=y2 (int regs; bits 32-33 = flags, not regs)
    if (OpCode = bcGfxWindow) or (OpCode = bcGfxView) then
    begin
      MarkIntRegUsed(Instr.Immediate and $FFFF);            // x2
      MarkIntRegUsed((Instr.Immediate shr 16) and $FFFF);   // y2
    end;

    // bcGraphicWindow: Src1=col1, Src2=row1, Dest=col2, Immediate = (clear_reg << 16) | row2_reg
    // All 5 parameters are int registers
    if OpCode = bcGraphicWindow then
    begin
      MarkIntRegUsed(Instr.Src1);              // col1
      MarkIntRegUsed(Instr.Src2);              // row1
      MarkIntRegUsed(Instr.Dest);              // col2
      MarkIntRegUsed(Instr.Immediate and $FFFF);           // row2 (bits 0-15)
      MarkIntRegUsed((Instr.Immediate shr 16) and $FFFF);  // clear (bits 16-31)
    end;

    // bcGraphicCircle: Src1=color(int), Src2=x(int), Dest=y(int)
    // Immediate: xr(10) | yr(10) | sa(10) | ea(10) | angle(10) | inc(10) = 60 bits
    // xr, yr are int registers; sa, ea, angle, inc are float registers
    if OpCode = bcGraphicCircle then
    begin
      MarkIntRegUsed(Instr.Src1);                          // color - int
      MarkIntRegUsed(Instr.Src2);                          // x - int
      MarkIntRegUsed(Instr.Dest);                          // y - int
      MarkIntRegUsed((Instr.Immediate) and $3FF);          // xr - int
      MarkIntRegUsed((Instr.Immediate shr 10) and $3FF);   // yr - int
      MarkFloatRegUsed((Instr.Immediate shr 20) and $3FF); // sa - float
      MarkFloatRegUsed((Instr.Immediate shr 30) and $3FF); // ea - float
      MarkFloatRegUsed((Instr.Immediate shr 40) and $3FF); // angle - float
      MarkFloatRegUsed((Instr.Immediate shr 50) and $3FF); // inc - float
    end;

    // bcGraphicPaint: Src1=source(int), Src2=x(int), Dest=y(int), Immediate = mode(int)
    if OpCode = bcGraphicPaint then
    begin
      MarkIntRegUsed(Instr.Src1);               // source - int
      MarkIntRegUsed(Instr.Src2);               // x - int
      MarkIntRegUsed(Instr.Dest);               // y - int
      MarkIntRegUsed(Instr.Immediate and $FFFF);// mode - int
    end;

    // bcGraphicSShape: Dest=string reg, Src1=x1(int), Src2=y1(int)
    // Immediate bits 0-15 = x2 register(int), bits 16-31 = y2 register(int)
    if OpCode = bcGraphicSShape then
    begin
      MarkStringRegUsed(Instr.Dest);                       // result string
      MarkIntRegUsed(Instr.Src1);                          // x1 - int
      MarkIntRegUsed(Instr.Src2);                          // y1 - int
      MarkIntRegUsed(Instr.Immediate and $FFFF);           // x2 - int
      MarkIntRegUsed((Instr.Immediate shr 16) and $FFFF);  // y2 - int
    end;

    // bcGraphicGShape: Src1=string reg, Src2=x(int), Dest=y(int), Immediate=mode (value, not reg)
    if OpCode = bcGraphicGShape then
    begin
      MarkStringRegUsed(Instr.Src1);            // shape string
      MarkIntRegUsed(Instr.Src2);               // x - int
      MarkIntRegUsed(Instr.Dest);               // y - int
    end;

    // bcSoundFilter: Immediate contains hp_reg(8) | res_reg(8)
    if OpCode = bcSoundFilter then
    begin
      MarkIntRegUsed(Instr.Immediate and $FF);           // hp register
      MarkIntRegUsed((Instr.Immediate shr 8) and $FF);   // resonance register
    end;

    // bcSoundSound: Immediate contains dir(8) | minfreq(12) | sweeptime(12) | waveform(8) | pw(12)
    if OpCode = bcSoundSound then
    begin
      MarkIntRegUsed((Instr.Immediate) and $FF);           // dir register
      MarkIntRegUsed((Instr.Immediate shr 8) and $FFF);    // minfreq register
      MarkIntRegUsed((Instr.Immediate shr 20) and $FFF);   // sweeptime register
      MarkIntRegUsed((Instr.Immediate shr 32) and $FF);    // waveform register
      MarkIntRegUsed((Instr.Immediate shr 40) and $FFF);   // pulsewidth register
    end;

    // bcSoundEnvelope: Immediate contains attack(8) | decay(8) | sustain(8) | release(8) | waveform(8) | pw(12)
    if OpCode = bcSoundEnvelope then
    begin
      MarkIntRegUsed((Instr.Immediate) and $FF);           // attack register
      MarkIntRegUsed((Instr.Immediate shr 8) and $FF);     // decay register
      MarkIntRegUsed((Instr.Immediate shr 16) and $FF);    // sustain register
      MarkIntRegUsed((Instr.Immediate shr 24) and $FF);    // release register
      MarkIntRegUsed((Instr.Immediate shr 32) and $FF);    // waveform register
      MarkIntRegUsed((Instr.Immediate shr 40) and $FFF);   // pulsewidth register
    end;

    // bcArrayDim has no register operands (info is in metadata)
    // ArrayLoad: Dest is written, Src1 is array index (metadata), Src2 is int index register
    // ArrayStore: Dest is value (READ!), Src1 is array index (metadata), Src2 is int index register
  end;

  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
  begin
    WriteLn('[RegCompact] Scanned registers:');
    WriteLn('[RegCompact]   Max old int reg: ', FMaxOldIntReg);
    WriteLn('[RegCompact]   Max old float reg: ', FMaxOldFloatReg);
    WriteLn('[RegCompact]   Max old string reg: ', FMaxOldStringReg);
  end;
  {$ENDIF}
end;

procedure TRegisterCompactor.BuildMappings;
var
  i: Integer;
  VarRegCount: Integer;
begin
  // CRITICAL FIX: Get the count of BASIC variable registers from program metadata
  // Variable registers (0..VarRegCount-1) must be preserved with identity mapping
  // because they may be read without explicit write instructions in the bytecode
  VarRegCount := FProgram.GetIntVarRegCount;
  // The identity-mapping loop below writes FIntRegMap[0..VarRegCount-1]; grow the map so a variable
  // register count above the map's current length can never write past it (heap corruption).
  if VarRegCount > Length(FIntRegMap) then
  begin
    i := Length(FIntRegMap);
    SetLength(FIntRegMap, VarRegCount);
    while i < VarRegCount do begin FIntRegMap[i] := -1; Inc(i); end;
  end;

  // Build compact mapping for int registers
  // First, preserve variable registers (0..VarRegCount-1) with identity mapping
  FNewIntRegCount := 0;
  for i := 0 to VarRegCount - 1 do
  begin
    FIntRegMap[i] := i;  // Identity mapping for variable registers
    if i >= FNewIntRegCount then
      FNewIntRegCount := i + 1;
  end;

  // Then map remaining used registers
  for i := VarRegCount to FMaxOldIntReg do
  begin
    if FIntRegMap[i] >= 0 then  // Was marked as used
    begin
      FIntRegMap[i] := FNewIntRegCount;
      Inc(FNewIntRegCount);
    end;
  end;

  // Build compact mapping for float registers
  // First, preserve variable registers with identity mapping
  VarRegCount := FProgram.GetFloatVarRegCount;
  if VarRegCount > Length(FFloatRegMap) then
  begin
    i := Length(FFloatRegMap);
    SetLength(FFloatRegMap, VarRegCount);
    while i < VarRegCount do begin FFloatRegMap[i] := -1; Inc(i); end;
  end;
  FNewFloatRegCount := 0;
  for i := 0 to VarRegCount - 1 do
  begin
    FFloatRegMap[i] := i;  // Identity mapping for variable registers
    if i >= FNewFloatRegCount then
      FNewFloatRegCount := i + 1;
  end;
  // Then map remaining used registers
  for i := VarRegCount to FMaxOldFloatReg do
  begin
    if FFloatRegMap[i] >= 0 then
    begin
      FFloatRegMap[i] := FNewFloatRegCount;
      Inc(FNewFloatRegCount);
    end;
  end;

  // Build compact mapping for string registers
  // First, preserve variable registers with identity mapping
  VarRegCount := FProgram.GetStringVarRegCount;
  if VarRegCount > Length(FStringRegMap) then
  begin
    i := Length(FStringRegMap);
    SetLength(FStringRegMap, VarRegCount);
    while i < VarRegCount do begin FStringRegMap[i] := -1; Inc(i); end;
  end;
  FNewStringRegCount := 0;
  for i := 0 to VarRegCount - 1 do
  begin
    FStringRegMap[i] := i;  // Identity mapping for variable registers
    if i >= FNewStringRegCount then
      FNewStringRegCount := i + 1;
  end;
  // Then map remaining used registers
  for i := VarRegCount to FMaxOldStringReg do
  begin
    if FStringRegMap[i] >= 0 then
    begin
      FStringRegMap[i] := FNewStringRegCount;
      Inc(FNewStringRegCount);
    end;
  end;

  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
  begin
    WriteLn('[RegCompact] Built mappings:');
    WriteLn('[RegCompact]   Int: ', FMaxOldIntReg + 1, ' → ', FNewIntRegCount);
    WriteLn('[RegCompact]   Float: ', FMaxOldFloatReg + 1, ' → ', FNewFloatRegCount);
    WriteLn('[RegCompact]   String: ', FMaxOldStringReg + 1, ' → ', FNewStringRegCount);
  end;
  {$ENDIF}
end;

procedure TRegisterCompactor.RewriteInstructions;
var
  i: Integer;
  Instr: TBytecodeInstruction;
  OpCode: TBytecodeOp;
  Modified: Boolean;
  NewImm: Int64;
  OldReg, NewReg: Integer;
begin
  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    Instr := FProgram.GetInstruction(i);
    // Note: OpCode can be a base TBytecodeOp OR a superinstruction constant (100+)
    OpCode := TBytecodeOp(Instr.OpCode);
    Modified := False;

    // Skip NOPs
    if Instr.OpCode = Byte(bcNop) then
      Continue;

    {$IFDEF DEBUG_REGALLOC}
    // Debug output for LoadConstInt with high registers
    if DebugRegAlloc and (OpCode = bcLoadConstInt) and (Instr.Dest > 30) then
    begin
      WriteLn('[RegCompact DEBUG] Instr ', i, ': OpCode=', Ord(OpCode),
              ' Dest=', Instr.Dest, ' DestIsIntReg=', DestIsIntReg(OpCode));
      if Instr.Dest < Length(FIntRegMap) then
        WriteLn('[RegCompact DEBUG]   FIntRegMap[', Instr.Dest, ']=', FIntRegMap[Instr.Dest]);
    end;
    {$ENDIF}

    // Remap Dest register (when Dest is a DESTINATION - written)
    // Note: Dest/Src1/Src2 are Word (unsigned), so >= 0 check is unnecessary
    if DestIsIntReg(OpCode) then
    begin
      if (Instr.Dest < Length(FIntRegMap)) and (FIntRegMap[Instr.Dest] >= 0) then
      begin
        Instr.Dest := FIntRegMap[Instr.Dest];
        Modified := True;
      end;
    end
    else if DestIsFloatReg(OpCode) then
    begin
      if (Instr.Dest < Length(FFloatRegMap)) and (FFloatRegMap[Instr.Dest] >= 0) then
      begin
        Instr.Dest := FFloatRegMap[Instr.Dest];
        Modified := True;
      end;
    end
    else if DestIsStringReg(OpCode) then
    begin
      if (Instr.Dest < Length(FStringRegMap)) and (FStringRegMap[Instr.Dest] >= 0) then
      begin
        Instr.Dest := FStringRegMap[Instr.Dest];
        Modified := True;
      end;
    end
    // Remap Dest register when Dest is a SOURCE (read) - e.g., ArrayStore
    else if DestReadIsIntReg(OpCode) then
    begin
      if (Instr.Dest < Length(FIntRegMap)) and (FIntRegMap[Instr.Dest] >= 0) then
      begin
        Instr.Dest := FIntRegMap[Instr.Dest];
        Modified := True;
      end;
    end
    else if DestReadIsFloatReg(OpCode) then
    begin
      if (Instr.Dest < Length(FFloatRegMap)) and (FFloatRegMap[Instr.Dest] >= 0) then
      begin
        Instr.Dest := FFloatRegMap[Instr.Dest];
        Modified := True;
      end;
    end
    else if DestReadIsStringReg(OpCode) then
    begin
      if (Instr.Dest < Length(FStringRegMap)) and (FStringRegMap[Instr.Dest] >= 0) then
      begin
        Instr.Dest := FStringRegMap[Instr.Dest];
        Modified := True;
      end;
    end;

    // Remap Src1 register
    if Src1IsIntReg(OpCode) then
    begin
      if (Instr.Src1 < Length(FIntRegMap)) and (FIntRegMap[Instr.Src1] >= 0) then
      begin
        Instr.Src1 := FIntRegMap[Instr.Src1];
        Modified := True;
      end;
    end
    else if Src1IsFloatReg(OpCode) then
    begin
      if (Instr.Src1 < Length(FFloatRegMap)) and (FFloatRegMap[Instr.Src1] >= 0) then
      begin
        Instr.Src1 := FFloatRegMap[Instr.Src1];
        Modified := True;
      end;
    end
    else if Src1IsStringReg(OpCode) then
    begin
      if (Instr.Src1 < Length(FStringRegMap)) and (FStringRegMap[Instr.Src1] >= 0) then
      begin
        Instr.Src1 := FStringRegMap[Instr.Src1];
        Modified := True;
      end;
    end;

    // Remap Src2 register
    if Src2IsIntReg(OpCode) then
    begin
      if (Instr.Src2 < Length(FIntRegMap)) and (FIntRegMap[Instr.Src2] >= 0) then
      begin
        Instr.Src2 := FIntRegMap[Instr.Src2];
        Modified := True;
      end;
    end
    else if Src2IsFloatReg(OpCode) then
    begin
      if (Instr.Src2 < Length(FFloatRegMap)) and (FFloatRegMap[Instr.Src2] >= 0) then
      begin
        Instr.Src2 := FFloatRegMap[Instr.Src2];
        Modified := True;
      end;
    end
    else if Src2IsStringReg(OpCode) then
    begin
      if (Instr.Src2 < Length(FStringRegMap)) and (FStringRegMap[Instr.Src2] >= 0) then
      begin
        Instr.Src2 := FStringRegMap[Instr.Src2];
        Modified := True;
      end;
    end;

    // Remap Immediate field when it contains a float register index
    // (for FMA and related superinstructions: 200, 201, 210, 211, 212, 230)
    if ImmediateIsFloatReg(OpCode) then
    begin
      if (Instr.Immediate < Length(FFloatRegMap)) and (FFloatRegMap[Instr.Immediate] >= 0) then
      begin
        Instr.Immediate := FFloatRegMap[Instr.Immediate];
        Modified := True;
      end;
    end;

    // Remap Immediate field when it contains a string register index
    // (for DOPEN mode parameter)
    if ImmediateIsStringReg(OpCode) then
    begin
      if (Instr.Immediate < Length(FStringRegMap)) and (FStringRegMap[Instr.Immediate] >= 0) then
      begin
        Instr.Immediate := FStringRegMap[Instr.Immediate];
        Modified := True;
      end;
    end;

    // bcStrInstr / bcStrInstrAny: Immediate = the int register holding the INSTR start position. Remap it.
    if (OpCode = bcStrInstr) or (OpCode = bcStrInstrAny) then
    begin
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg >= 0) and (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
      begin
        Instr.Immediate := FIntRegMap[OldReg];
        Modified := True;
      end;
    end;

    // Filesystem-command function forms (Immediate = -1): Dest = int register receiving the
    // error/exit code - remap it through the int map (mirrors the scan-side mark).
    if ((OpCode = bcChdir) or (OpCode = bcMkdir) or (OpCode = bcRmdir) or
        (OpCode = bcScratch) or (OpCode = bcCopyFile) or (OpCode = bcShell)) and
       (Instr.Immediate = -1) then
    begin
      if (Instr.Dest < Length(FIntRegMap)) and (FIntRegMap[Instr.Dest] >= 0) then
      begin
        Instr.Dest := FIntRegMap[Instr.Dest];
        Modified := True;
      end;
    end;
    // Statement-form bcCopyFile: Immediate = overwrite-flag INT REGISTER index (>= 0). Remap it.
    if (OpCode = bcCopyFile) and (Instr.Immediate >= 0) then
    begin
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
      begin
        Instr.Immediate := FIntRegMap[OldReg];
        Modified := True;
      end;
    end;

    // bcGraphicBox: Immediate contains 5 packed register indices
    // Layout: x2(bits 0-11) | y2(12-23) | angle(24-35) | filled(36-47) | fill_color(48-59)
    // x2, y2, filled, fill_color are int registers; angle is float register
    if OpCode = bcGraphicBox then
    begin
      NewImm := 0;

      // x2 (bits 0-11) - int register
      OldReg := (Instr.Immediate) and $FFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or (Int64(NewReg) and $FFF);

      // y2 (bits 12-23) - int register
      OldReg := (Instr.Immediate shr 12) and $FFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFF) shl 12);

      // angle (bits 24-35) - float register
      OldReg := (Instr.Immediate shr 24) and $FFF;
      if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
        NewReg := FFloatRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFF) shl 24);

      // filled (bits 36-47) - int register
      OldReg := (Instr.Immediate shr 36) and $FFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFF) shl 36);

      // fill_color (bits 48-59) - int register
      OldReg := (Instr.Immediate shr 48) and $FFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFF) shl 48);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcSetColor (SETCOLOR): Immediate = B reg (bits 0-11) | A reg (bits 12-23) - two int registers
    if OpCode = bcSetColor then
    begin
      NewImm := 0;
      OldReg := Instr.Immediate and $FFF;                                                  // B
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewReg and $FFF;
      OldReg := (Instr.Immediate shr 12) and $FFF;                                         // A
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFF) shl 12);
      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcGraphicSetMode: Immediate = param3 register (int)
    if OpCode = bcGraphicSetMode then
    begin
      OldReg := Instr.Immediate;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
      begin
        NewReg := FIntRegMap[OldReg];
        if NewReg <> OldReg then
        begin
          Instr.Immediate := NewReg;
          Modified := True;
        end;
      end;
    end;

    // bcGraphicRGBA: Immediate = (B_reg << 16) | A_reg - two int registers
    if OpCode = bcGraphicRGBA then
    begin
      // A register (bits 0-15)
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewReg and $FFFF;

      // B register (bits 16-31)
      OldReg := (Instr.Immediate shr 16) and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcArrayLoadDivAddFloat: Immediate = (denom_reg << 16) | acc_reg - two float registers
    if OpCode = bcArrayLoadDivAddFloat then
    begin
      // acc register (bits 0-15)
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
        NewReg := FFloatRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewReg and $FFFF;

      // denom register (bits 16-31)
      OldReg := (Instr.Immediate shr 16) and $FFFF;
      if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
        NewReg := FFloatRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcStrMid/bcStrMidW: Immediate contains length register index (int)
    // MID$(str, start, length) - start is Src2, length is in Immediate
    // bcDateSerial/bcTimeSerial: Immediate contains the 3rd arg (day/second) register index (int)
    if (OpCode = bcStrMid) or (OpCode = bcStrMidW) or (OpCode = bcStrAscMid) or
       (OpCode = bcStrConcatCharAt) or (OpCode = bcStrAppendMapped) or
       (OpCode = bcStrMidAssign) or                                      // Immediate = the START register (int)
       (OpCode = bcStrMidAssignArr) or                                   // ...and its SHARED/array form
       (OpCode = bcDateSerial) or (OpCode = bcTimeSerial) or
       (OpCode = bcRawMemCopy) or (OpCode = bcRawMemMove) or (OpCode = bcRawClear) or
       (OpCode = bcPutBinMem) or (OpCode = bcGetBinMem) or   // PUT/GET #n, , *p, n: byte-count register
       (OpCode = bcGfxPset) or (OpCode = bcGfxPaint) or (OpCode = bcGfxImageCreate) or
       (OpCode = bcConScreen) or   // SCREEN(row,col[,flag]): Immediate[0-15] = colorflag register
       (OpCode = bcSetmouse) then   // SETMOUSE: Immediate[0-15] = visibility register
    begin
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
      begin
        NewReg := FIntRegMap[OldReg];
        if NewReg <> OldReg then
        begin
          Instr.Immediate := NewReg and $FFFF;
          Modified := True;
        end;
      end;
    end;

    // bcGfxLine: Immediate [0-15]=x2, [16-31]=y2, [32-47]=color (int regs); bits 48-50 = shape+NOSTART flags (preserved)
    if OpCode = bcGfxLine then
    begin
      // x2 (bits 0-15)
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewReg and $FFFF;
      // y2 (bits 16-31)
      OldReg := (Instr.Immediate shr 16) and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);
      // color (bits 32-47)
      OldReg := (Instr.Immediate shr 32) and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 32);
      // shape + NOSTART flags (bits 48-50) preserved verbatim
      NewImm := NewImm or (((Instr.Immediate shr 48) and $7) shl 48);
      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcGfxCircle: Immediate [0-15]=radius, [16-31]=color (int regs)
    if OpCode = bcGfxCircle then
    begin
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewReg and $FFFF;
      OldReg := (Instr.Immediate shr 16) and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);
      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcGfxPaintBorder: Immediate [0-15]=color, [16-31]=border (int regs)
    if OpCode = bcGfxPaintBorder then
    begin
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewReg and $FFFF;
      OldReg := (Instr.Immediate shr 16) and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);
      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcGfxLineStyled: Dest=x2 (an input slot); Immediate [0-15]=y2, [16-31]=color, [32-47]=style
    // (int regs; bits 48-49 = shape flag, preserved). Dest remapped as an input (NOT in the Dest-def lists).
    if OpCode = bcGfxLineStyled then
    begin
      OldReg := Instr.Dest;                                  // x2
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) and (FIntRegMap[OldReg] <> OldReg) then
      begin Instr.Dest := FIntRegMap[OldReg]; Modified := True; end;
      OldReg := Instr.Immediate and $FFFF;                  // y2
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewReg and $FFFF;
      OldReg := (Instr.Immediate shr 16) and $FFFF;         // color
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);
      OldReg := (Instr.Immediate shr 32) and $FFFF;         // style
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 32);
      NewImm := NewImm or (((Instr.Immediate shr 48) and $3) shl 48);   // shape flag preserved
      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcGfxCircleEx / bcGfxCircleExF: Dest=RX (an input slot); Immediate [0-15]=RY, [16-31]=color,
    // [32-47]=start°, [48-63]=end° (all int regs). Dest is remapped here as an input (neither is in
    // the Dest-definition lists, so the generic Dest remap skips them).
    if (OpCode = bcGfxCircleEx) or (OpCode = bcGfxCircleExF) then
    begin
      OldReg := Instr.Dest;                                  // RX
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) and (FIntRegMap[OldReg] <> OldReg) then
      begin Instr.Dest := FIntRegMap[OldReg]; Modified := True; end;
      OldReg := Instr.Immediate and $FFFF;                  // RY
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewReg and $FFFF;
      OldReg := (Instr.Immediate shr 16) and $FFFF;         // color
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);
      OldReg := (Instr.Immediate shr 32) and $FFFF;         // start°
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 32);
      OldReg := (Instr.Immediate shr 48) and $FFFF;         // end°
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 48);
      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcGfxDrawString: Immediate [0-15]=y, [16-31]=colour (int regs).
    if OpCode = bcGfxDrawString then
    begin
      OldReg := Instr.Immediate and $FFFF;                   // y
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewReg and $FFFF;
      OldReg := (Instr.Immediate shr 16) and $FFFF;          // colour
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);
      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcGfxGet: Immediate [0-15]=x2, [16-31]=y2, [32-47]=dst handle (all int regs)
    if OpCode = bcGfxGet then
    begin
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewReg and $FFFF;
      OldReg := (Instr.Immediate shr 16) and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);
      OldReg := (Instr.Immediate shr 32) and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 32);
      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcGfxPut: Immediate [0-15]=src handle (int reg), [16-31]=mode ordinal (preserved),
    // [32-47]=ALPHA/ADD blend value (int reg).
    // ⛔ THIS REBUILT THE IMMEDIATE AND DROPPED EVERYTHING ABOVE BIT 31. That was harmless while the
    // field ended at 31 and is a silent zeroing the moment anything is added above it - which is
    // exactly what happened to the blend value. The mask is written from the layout now, so a new
    // field is preserved by default instead of by luck.
    if OpCode = bcGfxPut then
    begin
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := (Instr.Immediate and not Int64($FFFF)) or (NewReg and $FFFF);
      OldReg := (Instr.Immediate shr 32) and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := (NewImm and not (Int64($FFFF) shl 32)) or ((Int64(NewReg) and $FFFF) shl 32);
      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcGfxWindow/bcGfxView: Immediate [0-15]=x2, [16-31]=y2 (int regs); bits 32-33 = flags (preserved)
    if (OpCode = bcGfxWindow) or (OpCode = bcGfxView) then
    begin
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewReg and $FFFF;
      OldReg := (Instr.Immediate shr 16) and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then NewReg := FIntRegMap[OldReg] else NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);
      NewImm := NewImm or (Instr.Immediate and (Int64($3) shl 32));   // preserve flag bits 32-33
      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcGraphicWindow: Src1=col1, Src2=row1, Dest=col2, Immediate = (clear_reg << 16) | row2_reg
    // All 5 parameters are int registers. Src1/Src2/Dest are handled above, now handle Immediate.
    if OpCode = bcGraphicWindow then
    begin
      // row2 register (bits 0-15)
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewReg and $FFFF;

      // clear register (bits 16-31)
      OldReg := (Instr.Immediate shr 16) and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcGraphicCircle: Immediate contains 6 packed register indices
    // Layout: xr(bits 0-9) | yr(10-19) | sa(20-29) | ea(30-39) | angle(40-49) | inc(50-59)
    // xr, yr are int registers; sa, ea, angle, inc are float registers
    if OpCode = bcGraphicCircle then
    begin
      NewImm := 0;

      // xr - int (bits 0-9)
      OldReg := Instr.Immediate and $3FF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewReg and $3FF;

      // yr - int (bits 10-19)
      OldReg := (Instr.Immediate shr 10) and $3FF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $3FF) shl 10);

      // sa - float (bits 20-29)
      OldReg := (Instr.Immediate shr 20) and $3FF;
      if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
        NewReg := FFloatRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $3FF) shl 20);

      // ea - float (bits 30-39)
      OldReg := (Instr.Immediate shr 30) and $3FF;
      if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
        NewReg := FFloatRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $3FF) shl 30);

      // angle - float (bits 40-49)
      OldReg := (Instr.Immediate shr 40) and $3FF;
      if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
        NewReg := FFloatRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $3FF) shl 40);

      // inc - float (bits 50-59)
      OldReg := (Instr.Immediate shr 50) and $3FF;
      if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
        NewReg := FFloatRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $3FF) shl 50);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcGraphicPaint: Immediate = mode register (int)
    if OpCode = bcGraphicPaint then
    begin
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
      begin
        NewReg := FIntRegMap[OldReg];
        if NewReg <> OldReg then
        begin
          Instr.Immediate := NewReg and $FFFF;
          Modified := True;
        end;
      end;
    end;

    // bcGraphicSShape: Immediate = (y2_reg << 16) | x2_reg - two int registers
    if OpCode = bcGraphicSShape then
    begin
      // x2 register (bits 0-15)
      OldReg := Instr.Immediate and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewReg and $FFFF;

      // y2 register (bits 16-31)
      OldReg := (Instr.Immediate shr 16) and $FFFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFFF) shl 16);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcSoundFilter: Immediate contains hp_reg(8) | res_reg(8)
    if OpCode = bcSoundFilter then
    begin
      // hp register (bits 0-7)
      OldReg := Instr.Immediate and $FF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewReg and $FF;

      // resonance register (bits 8-15)
      OldReg := (Instr.Immediate shr 8) and $FF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FF) shl 8);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcSoundSound: Immediate contains dir(8) | minfreq(12) | sweeptime(12) | waveform(8) | pw(12)
    if OpCode = bcSoundSound then
    begin
      NewImm := 0;

      // dir (bits 0-7) - int register
      OldReg := (Instr.Immediate) and $FF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or (Int64(NewReg) and $FF);

      // minfreq (bits 8-19) - int register
      OldReg := (Instr.Immediate shr 8) and $FFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFF) shl 8);

      // sweeptime (bits 20-31) - int register
      OldReg := (Instr.Immediate shr 20) and $FFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFF) shl 20);

      // waveform (bits 32-39) - int register
      OldReg := (Instr.Immediate shr 32) and $FF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FF) shl 32);

      // pulsewidth (bits 40-51) - int register
      OldReg := (Instr.Immediate shr 40) and $FFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFF) shl 40);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // bcSoundEnvelope: Immediate contains attack(8) | decay(8) | sustain(8) | release(8) | waveform(8) | pw(12)
    if OpCode = bcSoundEnvelope then
    begin
      NewImm := 0;

      // attack (bits 0-7) - int register
      OldReg := (Instr.Immediate) and $FF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or (Int64(NewReg) and $FF);

      // decay (bits 8-15) - int register
      OldReg := (Instr.Immediate shr 8) and $FF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FF) shl 8);

      // sustain (bits 16-23) - int register
      OldReg := (Instr.Immediate shr 16) and $FF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FF) shl 16);

      // release (bits 24-31) - int register
      OldReg := (Instr.Immediate shr 24) and $FF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FF) shl 24);

      // waveform (bits 32-39) - int register
      OldReg := (Instr.Immediate shr 32) and $FF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FF) shl 32);

      // pulsewidth (bits 40-51) - int register
      OldReg := (Instr.Immediate shr 40) and $FFF;
      if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
        NewReg := FIntRegMap[OldReg]
      else
        NewReg := OldReg;
      NewImm := NewImm or ((Int64(NewReg) and $FFF) shl 40);

      if NewImm <> Instr.Immediate then
      begin
        Instr.Immediate := NewImm;
        Modified := True;
      end;
    end;

    // NOTE: Typed array opcodes (bcArrayLoadInt/Float/String, bcArrayStoreInt/Float/String)
    // are already handled by the DestIsIntReg/FloatReg/StringReg and Src2IsIntReg blocks above.
    // Src1 for these opcodes is ArrayIndex (metadata), NOT a register - don't remap.
    // bcArrayDim has no register operands (dimension info is in metadata).

    // Write back modified instruction
    if Modified then
      FProgram.SetInstruction(i, Instr);
  end;
end;

procedure TRegisterCompactor.RewriteArrayMetadata;
var
  i, j: Integer;
  ArrInfo: TSSAArrayInfo;
  OldReg, NewReg: Integer;
  Modified: Boolean;
begin
  // Iterate through all arrays in the program
  for i := 0 to FProgram.GetArrayCount - 1 do
  begin
    ArrInfo := FProgram.GetArray(i);
    Modified := False;

    // Remap DimRegisters for variable-sized dimensions
    for j := 0 to Length(ArrInfo.DimRegisters) - 1 do
    begin
      OldReg := ArrInfo.DimRegisters[j];
      if OldReg >= 0 then  // -1 means no register (constant dimension)
      begin
        // Determine the register type and remap accordingly
        case ArrInfo.DimRegTypes[j] of
          srtInt:
          begin
            if (OldReg < Length(FIntRegMap)) and (FIntRegMap[OldReg] >= 0) then
            begin
              NewReg := FIntRegMap[OldReg];
              if NewReg <> OldReg then
              begin
                ArrInfo.DimRegisters[j] := NewReg;
                Modified := True;
                {$IFDEF DEBUG_REGALLOC}
                if DebugRegAlloc then
                  WriteLn('[RegCompact] Array "', ArrInfo.Name, '" dim ', j,
                          ': IntR', OldReg, ' -> IntR', NewReg);
                {$ENDIF}
              end;
            end;
          end;
          srtFloat:
          begin
            if (OldReg < Length(FFloatRegMap)) and (FFloatRegMap[OldReg] >= 0) then
            begin
              NewReg := FFloatRegMap[OldReg];
              if NewReg <> OldReg then
              begin
                ArrInfo.DimRegisters[j] := NewReg;
                Modified := True;
                {$IFDEF DEBUG_REGALLOC}
                if DebugRegAlloc then
                  WriteLn('[RegCompact] Array "', ArrInfo.Name, '" dim ', j,
                          ': FloatR', OldReg, ' -> FloatR', NewReg);
                {$ENDIF}
              end;
            end;
          end;
          // String registers typically aren't used for array dimensions
        end;
      end;
    end;

    // Write back modified array info
    if Modified then
      FProgram.SetArray(i, ArrInfo);
  end;
end;

function TRegisterCompactor.Run: Integer;
var
  OldTotal, NewTotal: Integer;
begin
  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
    WriteLn('[RegCompact] Starting register compaction...');
  {$ENDIF}

  // Step 1: Scan to find used registers
  ScanUsedRegisters;

  // Step 2: Build compact mappings
  BuildMappings;

  // Step 3: Rewrite instructions
  RewriteInstructions;

  // Step 4: Rewrite array metadata (DimRegisters for variable-sized arrays)
  RewriteArrayMetadata;

  // Step 5: Hand the per-bank remaps to the program for the AOT backend (they would
  // otherwise die with this object; the AOT composes post-regalloc SSA register
  // indexes through them to get the exact bank indexes the interpreter uses).
  FProgram.SetAotRegMaps(FIntRegMap, FFloatRegMap, FStringRegMap);

  // Calculate reduction
  OldTotal := (FMaxOldIntReg + 1) + (FMaxOldFloatReg + 1) + (FMaxOldStringReg + 1);
  NewTotal := FNewIntRegCount + FNewFloatRegCount + FNewStringRegCount;
  Result := OldTotal - NewTotal;

  {$IFDEF DEBUG_REGALLOC}
  if DebugRegAlloc then
  begin
    WriteLn('[RegCompact] Compaction complete:');
    WriteLn('[RegCompact]   Int registers: ', FMaxOldIntReg + 1, ' -> ', FNewIntRegCount);
    WriteLn('[RegCompact]   Float registers: ', FMaxOldFloatReg + 1, ' -> ', FNewFloatRegCount);
    WriteLn('[RegCompact]   String registers: ', FMaxOldStringReg + 1, ' -> ', FNewStringRegCount);
    WriteLn('[RegCompact]   Total: ', OldTotal, ' -> ', NewTotal, ' (reduction: ', Result, ')');
  end;
  {$ENDIF}
end;

function RunRegisterCompaction(Prog: TBytecodeProgram): Integer;
var
  Compactor: TRegisterCompactor;
begin
  Compactor := TRegisterCompactor.Create(Prog);
  try
    Result := Compactor.Run;
  finally
    Compactor.Free;
  end;
end;

end.
