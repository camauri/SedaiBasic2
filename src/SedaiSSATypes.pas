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
unit SedaiSSATypes;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, fgl, Variants;

const
  { Register allocation limits }
  MIN_REGISTER_SLOTS = 256;      // Initial allocation size (backward compatible)
  MAX_REGISTER_SLOTS = 65536;    // Maximum registers per type (2^16)

type
  TSSARegisterType = (srtInt, srtFloat, srtString);

  TSSAValueKind = (
    svkNone, svkRegister, svkConstInt, svkConstFloat,
    svkConstString, svkVariable, svkLabel, svkArrayRef
  );

  { Forward declarations }
  TSSABasicBlock = class;

  TSSAValue = record
    Kind: TSSAValueKind;
    RegType: TSSARegisterType;
    RegIndex: Integer;
    Version: Integer;      // SSA versioning: R0_1, R0_2, etc. (0 = unversioned/legacy)
    VarName: string;
    ConstInt: Int64;
    ConstFloat: Double;
    ConstString: string;
    LabelName: string;
    ArrayIndex: Integer;  // For svkArrayRef: index into FArrays
  end;

  TSSAOpCode = (
    ssaPhi,  // PHI function for SSA merge points: dest = PHI(src1 from B1, src2 from B2, ...)
    ssaLoadConstInt, ssaLoadConstFloat, ssaLoadConstString,
    ssaCopyInt, ssaCopyFloat, ssaCopyString,
    ssaLoadVar, ssaStoreVar,
    ssaAddInt, ssaSubInt, ssaMulInt, ssaDivInt, ssaModInt, ssaNegInt,
    ssaAddFloat, ssaSubFloat, ssaMulFloat, ssaDivFloat, ssaPowFloat, ssaNegFloat,
    ssaIntToFloat, ssaFloatToInt, ssaIntToString, ssaFloatToString,
    ssaStringToInt, ssaStringToFloat,
    ssaCmpEqInt, ssaCmpNeInt, ssaCmpLtInt, ssaCmpGtInt, ssaCmpLeInt, ssaCmpGeInt,
    ssaCmpEqFloat, ssaCmpNeFloat, ssaCmpLtFloat, ssaCmpGtFloat, ssaCmpLeFloat, ssaCmpGeFloat,
    ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString,
    ssaBitwiseAnd, ssaBitwiseOr, ssaBitwiseXor, ssaBitwiseNot,
    ssaStrConcat, ssaStrLen, ssaStrLeft, ssaStrRight, ssaStrMid,
    ssaStrAsc, ssaStrChr, ssaStrStr, ssaStrVal, ssaStrHex, ssaStrInstr, ssaStrErr,
    ssaMathSin, ssaMathCos, ssaMathTan, ssaMathAtn, ssaMathLog, ssaMathExp,
    ssaMathSqr, ssaMathAbs, ssaMathSgn, ssaMathInt, ssaMathRnd,
    ssaMathLog10, ssaMathLog2, ssaMathLogN,  // Additional log functions
    ssaStrDec,  // DEC(hexstring) - convert hex string to decimal
    ssaLabel, ssaJump, ssaJumpIfZero, ssaJumpIfNotZero, ssaCall, ssaReturn,
    ssaArrayLoad, ssaArrayStore, ssaArrayDim,
    ssaPrint, ssaPrintLn, ssaPrintString, ssaPrintStringLn,
    ssaPrintInt, ssaPrintIntLn,
    ssaPrintComma, ssaPrintSemicolon, ssaPrintTab, ssaPrintSpc, ssaPrintNewLine,
    ssaInput, ssaInputInt, ssaInputFloat, ssaInputString,
    // Graphics
    ssaGraphicRGBA,    // Create 32-bit RGBA color value: dest = RGBA(r, g, b, a)
    ssaGraphicSetMode, // Set graphics mode: GRAPHIC mode, clear, param3
    ssaGraphicBox,     // Draw box
    ssaGraphicCircle,  // Draw circle/ellipse/arc
    ssaGraphicDraw,    // Draw dot or line
    ssaGraphicLocate,  // Set pixel cursor position
    ssaGraphicRdot,    // Get pixel cursor position or color
    ssaGraphicGetMode, // RGR - Get current graphics mode (0-11)
    ssaGraphicColor,   // COLOR source, color: Set color for screen area (0-255)
    ssaSetColor,       // SETCOLOR source, color: Set color for screen area (0-based)
    ssaGetColor,       // GETCOLOR(source): Return color index (0-based)
    ssaGraphicWidth,   // WIDTH n: Set line width (1 or 2)
    ssaGraphicScale,   // SCALE n [,xmax, ymax]: Set coordinate scaling
    ssaGraphicPaint,   // PAINT [source], x, y [,mode]: Flood fill area
    ssaGraphicWindow,  // WINDOW col1, row1, col2, row2 [,clear]: Define text window
    ssaGraphicSShape,  // SSHAPE A$, x1, y1 [,x2, y2]: Save bitmap area to string
    ssaGraphicGShape,  // GSHAPE A$, x, y [,mode]: Load string to bitmap
    ssaGraphicGList,   // GLIST: List SDL2 video modes
    ssaPLoad,          // PLOAD "filename": Load palette from JSON file
    ssaPSave,          // PSAVE "filename": Save palette to JSON file
    ssaScnClr,         // SCNCLR [mode]: Clear screen (text or graphics)
    ssaGraphicPos,     // POS(x): Return cursor column position
    ssaGraphicRclr,    // RCLR(n): Return color of source n
    ssaGraphicRwindow, // RWINDOW(n): Return window size info
    // Sound (SID-like)
    ssaSoundVol,       // VOL n: Set master volume (0-15)
    ssaSoundSound,     // SOUND vc,freq,dur[,dir,min,sv,wf,pw]: Play sound effect
    ssaSoundEnvelope,  // ENVELOPE e[,a,d,s,r,wf,pw]: Define instrument envelope
    ssaSoundTempo,     // TEMPO n: Set playback speed (0-255)
    ssaSoundPlay,      // PLAY "string": Play music string
    ssaSoundFilter,    // FILTER cf,lp,bp,hp,res: Set filter parameters
    // Special variables (reserved system variables)
    ssaLoadTI,         // TI: Load jiffies (1/60 sec) since interpreter start
    ssaLoadTIS,        // TI$: Load current time as HHMMSS string
    ssaStoreTIS,       // TI$ = "HHMMSS": Set time offset
    ssaLoadDTS,        // DT$: Load current date as YYYYMMDD string
    ssaLoadEL,         // EL: Load last error line number
    ssaLoadER,         // ER: Load last error code
    ssaLoadERRS,       // ERR$: Load last error message (variable, not function)
    ssaFre,            // FRE(x): Return available memory in bytes
    // Memory operations
    ssaPeek,           // PEEK(address): Read from memory-mapped location
    ssaPoke,           // POKE address, value: Write to memory-mapped location
    // Data handling
    ssaDataAdd,        // Add value to DATA pool
    ssaDataRead,       // Read next value from DATA pool into dest
    ssaDataRestore,    // Reset DATA pointer to beginning
    // Input commands
    ssaGet,            // GET A$ (non-blocking character input)
    ssaGetkey,         // GETKEY A$ (blocking character input)
    // Formatted output
    ssaPrintUsing,     // PRINT USING format$; values
    ssaPudef,          // PUDEF format string (redefine PRINT USING symbols)
    ssaChar,           // CHAR mode, col, row, text [,reverse]
    // File operations
    ssaLoad,           // LOAD "filename": Load program from file
    ssaSave,           // SAVE "filename": Save program to file
    ssaVerify,         // VERIFY "filename": Verify program against file
    ssaBload,          // BLOAD "filename": Load bytecode from file
    ssaBsave,          // BSAVE "filename": Save bytecode to file
    ssaBoot,           // BOOT "filename": Load and run bytecode
    // System commands (from program)
    ssaRun,            // RUN [linenum]: Run program from beginning or line
    ssaList,           // LIST [start-end]: List program lines
    ssaNew,            // NEW: Clear program and variables
    ssaDelete,         // DELETE [start[-end]]: Delete program lines
    ssaRenumber,       // RENUMBER [new[,inc[,old]]]: Renumber program lines
    ssaCatalog,        // CATALOG/DIR: List directory contents
    // File management commands (executed directly in VM)
    ssaCopyFile,       // COPY/CP "src","dest"[,overwrite]: Copy file
    ssaScratch,        // SCRATCH "pattern"[,force]: Delete file(s)
    ssaRenameFile,     // RENAME "old","new": Rename file
    ssaConcat,         // CONCAT "src","dest": Concatenate files
    ssaMkdir,          // MKDIR/MD "path": Create directory
    ssaChdir,          // CHDIR/CD "path": Change current directory
    ssaMoveFile,       // MOVE/MV "src","dest": Move file
    // Disk file I/O
    ssaDopen,          // DOPEN #handle, "filename" [, mode$]: Open disk file
    ssaDclose,         // DCLOSE #handle: Close disk file
    ssaOpen,           // OPEN (legacy C64/C128 style, maps to DOPEN)
    ssaClose,          // CLOSE (legacy C64/C128 style, maps to DCLOSE)
    ssaGetFile,        // GET# file, var: Get char from file
    ssaInputFile,      // INPUT# file, vars: Input from file
    ssaPrintFile,      // PRINT# file, exprs: Print to file
    ssaCmd,            // CMD file [, expr]: Redirect output to file
    // Sprite commands
    ssaSprite,         // SPRITE n [,on] [,color] [,priority] [,xscale] [,yscale] [,mode]
    ssaMovsprAbs,      // MOVSPR n, x, y: Position sprite at absolute coordinates
    ssaMovsprRel,      // MOVSPR n, +x, +y: Move sprite relative to current position
    ssaMovsprPolar,    // MOVSPR n, dist;angle: Move sprite by distance at angle
    ssaMovsprAuto,     // MOVSPR n, angle#speed: Start automatic movement
    ssaSprcolor,       // SPRCOLOR [mc1] [,mc2]: Set global multicolors
    ssaSprsav,         // SPRSAV src, dst: Save/load/copy sprite data
    ssaCollision,      // COLLISION type [,line]: Set collision handler
    // Sprite functions (return values)
    ssaBump,           // BUMP(n): Return collision bitmask
    ssaRspcolor,       // RSPCOLOR(n): Return multicolor value
    ssaRsppos,         // RSPPOS(sprite, n): Return position/speed
    ssaRsprite,        // RSPRITE(sprite, n): Return sprite attribute
    // Control flow
    ssaEnd, ssaStop, ssaFast, ssaSlow, ssaSleep, ssaKey, ssaNop, ssaClear,
    // Debug/Trace
    ssaTron, ssaTroff,  // TRON/TROFF: Enable/disable trace mode
    // Error handling
    ssaTrap,            // TRAP linenum: Set error handler
    ssaResume,          // RESUME: Continue after error at error line
    ssaResumeNext       // RESUME NEXT: Continue after error at next statement
  );

  { PHI source: value from a specific predecessor block }
  TSSAPhiSource = record
    Value: TSSAValue;               // The SSA value from this path
    FromBlock: TSSABasicBlock;      // Which predecessor block this comes from
  end;

  TSSAInstruction = class
  public
    OpCode: TSSAOpCode;
    Dest, Src1, Src2, Src3: TSSAValue;
    PhiSources: array of TSSAPhiSource;  // For ssaPhi: list of (value, block) pairs
    Comment: string;
    SourceLine: Integer;
    constructor Create(AOpCode: TSSAOpCode);
    function ToString: string; override;
    function Clone: TSSAInstruction;
    procedure AddPhiSource(const Val: TSSAValue; FromBlock: TSSABasicBlock);
  end;

  TSSAInstructionList = specialize TFPGObjectList<TSSAInstruction>;

  TSSABasicBlock = class
  private
    FInstructions: TSSAInstructionList;
    FLabel: string;
    FPredecessors, FSuccessors: TFPList;
    FBlockIndex: Integer;  // Index in program's block list (for fast O(1) lookup in SSA construction)
  public
    constructor Create(const ALabel: string);
    destructor Destroy; override;
    procedure AddInstruction(Instr: TSSAInstruction);
    procedure AddPredecessor(Block: TSSABasicBlock);
    procedure AddSuccessor(Block: TSSABasicBlock);
    property Instructions: TSSAInstructionList read FInstructions;
    property LabelName: string read FLabel write FLabel;
    property Predecessors: TFPList read FPredecessors;
    property Successors: TFPList read FSuccessors;
    property BlockIndex: Integer read FBlockIndex write FBlockIndex;
  end;

  TSSABasicBlockList = specialize TFPGObjectList<TSSABasicBlock>;

  { Array info for SSA }
  TSSAArrayInfo = record
    Name: string;
    ElementType: TSSARegisterType;
    DimCount: Integer;
    Dimensions: array of Integer;       // Size for each dimension (0 = runtime-sized)
    DimRegisters: array of Integer;     // SSA register indices for variable dimensions
    DimRegTypes: array of TSSARegisterType; // Register types for variable dimensions
    ArrayIndex: Integer;                 // Index in VM array table
  end;

  TSSAProgram = class
  private
    FBlocks: TSSABasicBlockList;
    FVariables, FLabels: TStringList;
    FVarRegMap: TStringList;    // Maps variable name → "RegType:RegIndex" (for optimization passes)
    FArrays: array of TSSAArrayInfo;  // Array declarations
    FNextRegister: array[TSSARegisterType] of Integer;
    FNextArrayIndex: Integer;
    FDomTreeObj: TObject;       // PHASE 3 TIER 2: Actually TDominatorTree (avoid circular dependency)
    FDomTreeValid: Boolean;     // PHASE 3 TIER 2: Flag for lazy rebuild
    FGlobalVariableSemantics: Boolean;  // True = BASIC mode (Version=0), False = SSA mode (versioning)
  public
    constructor Create;
    destructor Destroy; override;
    function CreateBlock(const LabelName: string): TSSABasicBlock;
    function CreateBlockBefore(const LabelName: string; BeforeBlock: TSSABasicBlock): TSSABasicBlock;  // Insert block before another block
    function FindBlock(const LabelName: string): TSSABasicBlock;
    function GetOrCreateBlock(const LabelName: string): TSSABasicBlock;  // Find existing or create new block
    function AllocRegister(RegType: TSSARegisterType): Integer;
    procedure AddVariable(const VarName: string);
    procedure MapVariableToRegister(const VarName: string; RegType: TSSARegisterType; RegIndex: Integer);
    function GetVariableRegister(const VarName: string; out RegType: TSSARegisterType; out RegIndex: Integer): Boolean;
    function DeclareArray(const ArrName: string; ElementType: TSSARegisterType; const Dims: array of Integer): Integer;
    procedure SetArrayDimRegisters(ArrayIdx: Integer; const DimRegs: array of Integer; const DimRegTypes: array of TSSARegisterType);
    function FindArray(const ArrName: string): Integer;
    function GetArray(Index: Integer): TSSAArrayInfo;
    function GetArrayCount: Integer;
    procedure BuildDominatorTree;  // PHASE 3 TIER 2: Build dominator tree for optimizations
    procedure ClearDomTree;  // Clear dominator tree (call after CFG modifications like LICM)
    function GetDomTree: TObject;  // PHASE 3 TIER 2: Get dominator tree (cast to TDominatorTree in implementation)
    function RunDBE: Integer;  // Dead block elimination - removes unreachable blocks (returns removed block count)
    procedure RunSSAConstruction;  // PHASE 3: Convert to proper SSA with PHI functions and versioning
    procedure RunPhiElimination;  // FINAL PASS: Convert PHI functions to copy instructions (BEFORE bytecode compilation)
    function RunGVN: Integer;  // PHASE 3 TIER 2: Run Global Value Numbering optimization (returns replacements count)
    function RunCSE: Integer;  // Common subexpression elimination (returns eliminated count)
    function RunCopyProp: Integer;  // Copy propagation (returns replacement count)
    function RunAlgebraic: Integer;  // Algebraic simplification (returns simplification count)
    function RunStrengthReduction: Integer;  // Strength reduction (returns reduction count)
    function RunGosubInlining: Integer;  // GOSUB inlining (returns inlined call count)
    function RunConstProp: Integer;  // Simple constant propagation (returns propagation count)
    function RunAggressiveConstProp(Level: Integer): Integer;  // Aggressive constant propagation with configurable level
    function RunDCE: Integer;  // Dead code elimination (returns removed instruction count)
    function RunLICM: Integer;  // Loop-invariant code motion (returns hoisted instruction count)
    function RunLoopUnrolling: Integer;  // Loop unrolling (returns unrolled loop count)
    function RunCopyCoalescing: Integer;  // Copy coalescing (returns coalesced copy count)
    procedure PrintSSA;  // Dump SSA for debugging
    property Blocks: TSSABasicBlockList read FBlocks;
    property Variables: TStringList read FVariables;
    property Labels: TStringList read FLabels;
    property VarRegMap: TStringList read FVarRegMap;  // Access to variable→register mapping (for SSA construction)
    property GlobalVariableSemantics: Boolean read FGlobalVariableSemantics write FGlobalVariableSemantics;
  end;

function MakeSSAValue(Kind: TSSAValueKind): TSSAValue;
function MakeSSARegister(RegType: TSSARegisterType; RegIndex: Integer): TSSAValue;
function MakeSSAConstInt(Value: Int64): TSSAValue;
function MakeSSAConstFloat(Value: Double): TSSAValue;
function MakeSSAConstString(const Value: string): TSSAValue;
function MakeSSAVariable(const VarName: string): TSSAValue;
function MakeSSALabel(const LabelName: string): TSSAValue;
function MakeSSAArrayRef(ArrayIdx: Integer; ElementType: TSSARegisterType): TSSAValue;
function SSAValueToString(const Value: TSSAValue): string;
function SSAOpCodeToString(OpCode: TSSAOpCode): string;
function SSARegisterTypeToString(RegType: TSSARegisterType): string;

implementation

uses TypInfo, SedaiDominators, SedaiSSAConstruction, SedaiPhiElimination, SedaiGVN, SedaiCSE, SedaiCopyProp,
     SedaiAlgebraic, SedaiStrengthReduction, SedaiGosubInlining, SedaiConstProp, SedaiConstPropAggressive,
     SedaiDBE, SedaiDCE, SedaiLICM, SedaiLoopUnroll, SedaiCopyCoalescing
     {$IF DEFINED(DEBUG_CLEANUP) OR DEFINED(DEBUG_DOMTREE) OR DEFINED(DEBUG_GVN) OR DEFINED(DEBUG_CSE) OR DEFINED(DEBUG_COPYPROP) OR DEFINED(DEBUG_ALGEBRAIC) OR DEFINED(DEBUG_STRENGTH) OR DEFINED(DEBUG_CONSTPROP) OR DEFINED(DEBUG_DBE) OR DEFINED(DEBUG_DCE) OR DEFINED(DEBUG_LICM) OR DEFINED(DEBUG_COPYCOAL) OR DEFINED(DEBUG_SSA)}, SedaiDebug{$ENDIF};

constructor TSSAInstruction.Create(AOpCode: TSSAOpCode);
begin
  inherited Create;
  OpCode := AOpCode;
  Dest := MakeSSAValue(svkNone);
  Src1 := MakeSSAValue(svkNone);
  Src2 := MakeSSAValue(svkNone);
  Src3 := MakeSSAValue(svkNone);
  SetLength(PhiSources, 0);  // Initialize empty PHI sources
  Comment := '';
  SourceLine := 0;
end;

procedure TSSAInstruction.AddPhiSource(const Val: TSSAValue; FromBlock: TSSABasicBlock);
var
  Idx: Integer;
begin
  Idx := Length(PhiSources);
  SetLength(PhiSources, Idx + 1);
  PhiSources[Idx].Value := Val;
  PhiSources[Idx].FromBlock := FromBlock;
end;

function TSSAInstruction.ToString: string;
var
  i: Integer;
begin
  Result := SSAOpCodeToString(OpCode);
  if Dest.Kind <> svkNone then Result := Result + ' ' + SSAValueToString(Dest);

  // Special handling for PHI instructions
  if OpCode = ssaPhi then
  begin
    Result := Result + ' = PHI(';
    for i := 0 to High(PhiSources) do
    begin
      if i > 0 then Result := Result + ', ';
      Result := Result + SSAValueToString(PhiSources[i].Value);
      if Assigned(PhiSources[i].FromBlock) then
        Result := Result + ' from ' + PhiSources[i].FromBlock.LabelName;
    end;
    Result := Result + ')';
  end
  else
  begin
    if Src1.Kind <> svkNone then Result := Result + ', ' + SSAValueToString(Src1);
    if Src2.Kind <> svkNone then Result := Result + ', ' + SSAValueToString(Src2);
    if Src3.Kind <> svkNone then Result := Result + ', ' + SSAValueToString(Src3);
  end;

  if Comment <> '' then Result := Result + ' ; ' + Comment;
end;

function TSSAInstruction.Clone: TSSAInstruction;
var
  i: Integer;
begin
  Result := TSSAInstruction.Create(OpCode);
  Result.Dest := Dest;
  Result.Src1 := Src1;
  Result.Src2 := Src2;
  Result.Src3 := Src3;

  // Clone PHI sources
  SetLength(Result.PhiSources, Length(PhiSources));
  for i := 0 to High(PhiSources) do
    Result.PhiSources[i] := PhiSources[i];

  Result.Comment := Comment;
  Result.SourceLine := SourceLine;
end;

constructor TSSABasicBlock.Create(const ALabel: string);
begin
  inherited Create;
  FInstructions := TSSAInstructionList.Create(True);
  FPredecessors := TFPList.Create;
  FSuccessors := TFPList.Create;
  FLabel := ALabel;
  FBlockIndex := -1;  // Will be set by SSA construction
end;

destructor TSSABasicBlock.Destroy;
var
  IsPreHeader: Boolean;
begin
  // Check if this is a pre-header block (created by LICM)
  IsPreHeader := (Pos('_prehead', FLabel) > 0);

  {$IFDEF DEBUG_CLEANUP}
  if DebugCleanup then
  begin
    WriteLn('[TSSABasicBlock.Destroy] Starting for: ', FLabel);
    if IsPreHeader then
      WriteLn('[TSSABasicBlock.Destroy]   (Pre-header block detected)');
    WriteLn('[TSSABasicBlock.Destroy]   FInstructions = ', PtrUInt(FInstructions));
    WriteLn('[TSSABasicBlock.Destroy]   FPredecessors = ', PtrUInt(FPredecessors));
    WriteLn('[TSSABasicBlock.Destroy]   FSuccessors = ', PtrUInt(FSuccessors));
    Flush(Output);
  end;
  {$ENDIF}

  // Free owned objects
  if Assigned(FInstructions) then
  begin
    {$IFDEF DEBUG_CLEANUP}
    if DebugCleanup then WriteLn('[TSSABasicBlock.Destroy]   Freeing FInstructions...');
    {$ENDIF}
    FreeAndNil(FInstructions);
  end;

  // WORKAROUND: Pre-header blocks created by LICM have corrupted TFPList objects
  // during cleanup when Register Compaction is enabled. Root cause unknown -
  // possibly related to block list manipulation in CreateBlockBefore or
  // cross-references during optimization passes. Skip freeing these lists
  // for pre-header blocks to prevent crashes. This is a small memory leak.
  if IsPreHeader then
  begin
    {$IFDEF DEBUG_CLEANUP}
    if DebugCleanup then
      WriteLn('[TSSABasicBlock.Destroy]   Skipping pred/succ free (pre-header workaround)');
    {$ENDIF}
    FPredecessors := nil;
    FSuccessors := nil;
  end
  else
  begin
    // Normal blocks: free predecessor/successor lists
    if Assigned(FPredecessors) then
    begin
      {$IFDEF DEBUG_CLEANUP}
      if DebugCleanup then WriteLn('[TSSABasicBlock.Destroy]   Freeing FPredecessors...');
      {$ENDIF}
      FreeAndNil(FPredecessors);
    end;

    if Assigned(FSuccessors) then
    begin
      {$IFDEF DEBUG_CLEANUP}
      if DebugCleanup then WriteLn('[TSSABasicBlock.Destroy]   Freeing FSuccessors...');
      {$ENDIF}
      FreeAndNil(FSuccessors);
    end;
  end;

  {$IFDEF DEBUG_CLEANUP}
  if DebugCleanup then WriteLn('[TSSABasicBlock.Destroy]   Calling inherited...');
  {$ENDIF}
  inherited Destroy;
  {$IFDEF DEBUG_CLEANUP}
  if DebugCleanup then
  begin
    WriteLn('[TSSABasicBlock.Destroy] Complete for: ', FLabel);
    Flush(Output);
  end;
  {$ENDIF}
end;

procedure TSSABasicBlock.AddInstruction(Instr: TSSAInstruction);
begin
  FInstructions.Add(Instr);
end;

procedure TSSABasicBlock.AddPredecessor(Block: TSSABasicBlock);
begin
  if FPredecessors.IndexOf(Block) = -1 then FPredecessors.Add(Block);
end;

procedure TSSABasicBlock.AddSuccessor(Block: TSSABasicBlock);
begin
  if FSuccessors.IndexOf(Block) = -1 then FSuccessors.Add(Block);
end;

constructor TSSAProgram.Create;
var
  rt: TSSARegisterType;
begin
  inherited Create;
  FBlocks := TSSABasicBlockList.Create(True);
  FVariables := TStringList.Create;
  FVariables.Sorted := True;
  FVariables.Duplicates := dupIgnore;
  FVarRegMap := TStringList.Create;
  FVarRegMap.Sorted := True;
  FLabels := TStringList.Create;
  FLabels.Sorted := True;
  SetLength(FArrays, 0);
  FNextArrayIndex := 0;
  for rt := Low(TSSARegisterType) to High(TSSARegisterType) do
    FNextRegister[rt] := 0;

  // PHASE 3 TIER 2: Create dominator tree infrastructure
  FDomTreeObj := TDominatorTree.Create;
  FDomTreeValid := False;

  // Default: BASIC mode with global variable semantics (Version=0)
  // Can be changed to False for SSA languages with scoped variables
  FGlobalVariableSemantics := True;  // Must be built after SSA construction
end;

destructor TSSAProgram.Destroy;
var
  i, j, k: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
begin
  {$IFDEF DEBUG_CLEANUP}
  if DebugCleanup then
  begin
    WriteLn('[SSAProgram.Destroy] Starting cleanup...');
    Flush(Output);
  end;
  {$ENDIF}

  // Free dominator tree
  FreeAndNil(FDomTreeObj);
  {$IFDEF DEBUG_CLEANUP}
  if DebugCleanup then
  begin
    WriteLn('[SSAProgram.Destroy] Dominator tree freed');
    Flush(Output);
  end;
  {$ENDIF}

  // CRITICAL FIX: Clear all cross-block references BEFORE freeing any block!
  // There are TWO sources of dangling pointers:
  // 1. Predecessors/Successors TFPList contain raw pointers to other blocks
  // 2. PHI instructions have FromBlock pointers to TSSABasicBlock
  // By clearing ALL references first, we ensure no dangling pointers during destruction.

  // Clear PHI FromBlock references
  {$IFDEF DEBUG_CLEANUP}
  if DebugCleanup then
    WriteLn('[SSAProgram.Destroy] Clearing PHI references in ', FBlocks.Count, ' blocks...');
  {$ENDIF}
  for i := 0 to FBlocks.Count - 1 do
  begin
    Block := FBlocks[i];
    if Assigned(Block) then
    begin
      {$IFDEF DEBUG_CLEANUP}
      if DebugCleanup then
        WriteLn('[SSAProgram.Destroy]   Block ', i, ': ', Block.LabelName, ' (', Block.Instructions.Count, ' instrs)');
      {$ENDIF}
      for j := 0 to Block.Instructions.Count - 1 do
      begin
        Instr := Block.Instructions[j];
        if (Instr.OpCode = ssaPhi) and (Length(Instr.PhiSources) > 0) then
        begin
          for k := 0 to High(Instr.PhiSources) do
            Instr.PhiSources[k].FromBlock := nil;
        end;
      end;
    end
    else
    begin
      {$IFDEF DEBUG_CLEANUP}
      if DebugCleanup then
        WriteLn('[SSAProgram.Destroy]   Block ', i, ': NIL!');
      {$ENDIF}
    end;
  end;
  {$IFDEF DEBUG_CLEANUP}
  if DebugCleanup then
    WriteLn('[SSAProgram.Destroy] PHI references cleared');
  {$ENDIF}

  // NOTE: We used to clear predecessor/successor lists here to remove cross-references
  // before freeing blocks. However, this causes Access Violations on pre-header blocks
  // created by LICM when Register Compaction is also enabled (reason unclear - possible
  // heap corruption). Since blocks are about to be freed anyway and TFPList.Free handles
  // cleanup internally, we skip the explicit Clear() calls.
  // The original purpose was to break cycles that might cause issues during destruction,
  // but TFPGObjectList with Extract + Free handles this correctly.
  {$IFDEF DEBUG_CLEANUP}
  if DebugCleanup then
    WriteLn('[SSAProgram.Destroy] Skipping predecessor/successor clearing (blocks will be freed)');
  {$ENDIF}

  // Free blocks manually with Extract to avoid TFPGObjectList internal issues
  {$IFDEF DEBUG_CLEANUP}
  if DebugCleanup then
    WriteLn('[SSAProgram.Destroy] Freeing ', FBlocks.Count, ' blocks...');
  {$ENDIF}

  while FBlocks.Count > 0 do
  begin
    Block := FBlocks.Extract(FBlocks[FBlocks.Count - 1]);
    {$IFDEF DEBUG_CLEANUP}
    if DebugCleanup then
      WriteLn('[SSAProgram.Destroy]   Block: ', Block.LabelName);
    {$ENDIF}
    Block.Free;
  end;

  FBlocks.Free;
  FVariables.Free;
  FVarRegMap.Free;
  FLabels.Free;

  inherited Destroy;

  {$IFDEF DEBUG_CLEANUP}
  if DebugCleanup then
    WriteLn('[SSAProgram.Destroy] Cleanup complete');
  {$ENDIF}
end;

function TSSAProgram.CreateBlock(const LabelName: string): TSSABasicBlock;
begin
  Result := TSSABasicBlock.Create(LabelName);
  FBlocks.Add(Result);
  if LabelName <> '' then
    FLabels.AddObject(LabelName, TObject(PtrInt(FBlocks.Count - 1)));
end;

function TSSAProgram.CreateBlockBefore(const LabelName: string; BeforeBlock: TSSABasicBlock): TSSABasicBlock;
var
  InsertIdx, i: Integer;
  Block: TSSABasicBlock;
begin
  { Creates a new block and inserts it BEFORE the specified block in the block list.
    This is critical for LICM pre-headers which must appear before their loop headers
    in the bytecode compilation order. }

  InsertIdx := FBlocks.IndexOf(BeforeBlock);
  if InsertIdx < 0 then
  begin
    // BeforeBlock not found, fall back to normal CreateBlock
    Result := CreateBlock(LabelName);
    Exit;
  end;

  // Create the new block
  Result := TSSABasicBlock.Create(LabelName);

  // Insert at the correct position
  FBlocks.Insert(InsertIdx, Result);

  // Update FLabels indices - all blocks after InsertIdx have shifted by 1
  // FLabels stores block indices as Objects, so we need to update them
  for i := 0 to FLabels.Count - 1 do
  begin
    if PtrInt(FLabels.Objects[i]) >= InsertIdx then
      FLabels.Objects[i] := TObject(PtrInt(FLabels.Objects[i]) + 1);
  end;

  // Add new block's label to FLabels
  if LabelName <> '' then
    FLabels.AddObject(LabelName, TObject(PtrInt(InsertIdx)));
end;

function TSSAProgram.FindBlock(const LabelName: string): TSSABasicBlock;
var Idx: Integer;
begin
  Idx := FLabels.IndexOf(LabelName);
  if Idx >= 0 then
    Result := FBlocks[PtrInt(FLabels.Objects[Idx])]
  else
    Result := nil;
end;

function TSSAProgram.GetOrCreateBlock(const LabelName: string): TSSABasicBlock;
begin
  // First try to find an existing block with this label
  Result := FindBlock(LabelName);
  // If not found, create a new one
  if Result = nil then
    Result := CreateBlock(LabelName);
end;

function TSSAProgram.AllocRegister(RegType: TSSARegisterType): Integer;
begin
  Result := FNextRegister[RegType];
  Inc(FNextRegister[RegType]);
  if FNextRegister[RegType] >= MAX_REGISTER_SLOTS then
    raise Exception.CreateFmt('Register overflow: exceeded %d registers for type %d',
                              [MAX_REGISTER_SLOTS, Ord(RegType)]);
end;

procedure TSSAProgram.AddVariable(const VarName: string);
begin
  FVariables.Add(VarName);
end;

procedure TSSAProgram.MapVariableToRegister(const VarName: string; RegType: TSSARegisterType; RegIndex: Integer);
begin
  // Store mapping as "RegType:RegIndex" string
  FVarRegMap.Values[VarName] := IntToStr(Ord(RegType)) + ':' + IntToStr(RegIndex);
end;

function TSSAProgram.GetVariableRegister(const VarName: string; out RegType: TSSARegisterType; out RegIndex: Integer): Boolean;
var
  RegStr: string;
  ColonPos: Integer;
begin
  Result := False;
  RegStr := FVarRegMap.Values[VarName];
  if RegStr = '' then Exit;

  ColonPos := Pos(':', RegStr);
  if ColonPos > 0 then
  begin
    RegType := TSSARegisterType(StrToInt(Copy(RegStr, 1, ColonPos - 1)));
    RegIndex := StrToInt(Copy(RegStr, ColonPos + 1, Length(RegStr)));
    Result := True;
  end;
end;

function TSSAProgram.DeclareArray(const ArrName: string; ElementType: TSSARegisterType; const Dims: array of Integer): Integer;
var
  Len, i: Integer;
begin
  // Check if already declared - BASIC allows redimensioning
  Result := FindArray(ArrName);
  if Result >= 0 then
  begin
    // Array already exists - update dimensions (BASIC REDIM semantics)
    FArrays[Result].ElementType := ElementType;
    FArrays[Result].DimCount := Length(Dims);
    SetLength(FArrays[Result].Dimensions, Length(Dims));
    for i := 0 to High(Dims) do
      FArrays[Result].Dimensions[i] := Dims[i];
    // Clear old dimension registers (will be set again if needed)
    SetLength(FArrays[Result].DimRegisters, 0);
    SetLength(FArrays[Result].DimRegTypes, 0);
    // Keep existing ArrayIndex
    Exit;
  end;

  // Allocate new array info
  Len := Length(FArrays);
  SetLength(FArrays, Len + 1);
  Result := Len;

  FArrays[Result].Name := UpperCase(ArrName);
  FArrays[Result].ElementType := ElementType;
  FArrays[Result].DimCount := Length(Dims);
  SetLength(FArrays[Result].Dimensions, Length(Dims));
  for i := 0 to High(Dims) do
    FArrays[Result].Dimensions[i] := Dims[i];
  // Initialize dimension registers (empty by default, set later if needed)
  SetLength(FArrays[Result].DimRegisters, 0);
  SetLength(FArrays[Result].DimRegTypes, 0);
  FArrays[Result].ArrayIndex := FNextArrayIndex;
  Inc(FNextArrayIndex);
end;

procedure TSSAProgram.SetArrayDimRegisters(ArrayIdx: Integer; const DimRegs: array of Integer; const DimRegTypes: array of TSSARegisterType);
var
  i: Integer;
begin
  if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) then
    raise Exception.CreateFmt('Invalid array index: %d', [ArrayIdx]);

  SetLength(FArrays[ArrayIdx].DimRegisters, Length(DimRegs));
  SetLength(FArrays[ArrayIdx].DimRegTypes, Length(DimRegTypes));

  for i := 0 to High(DimRegs) do
  begin
    FArrays[ArrayIdx].DimRegisters[i] := DimRegs[i];
    if i <= High(DimRegTypes) then
      FArrays[ArrayIdx].DimRegTypes[i] := DimRegTypes[i];
  end;
end;

function TSSAProgram.FindArray(const ArrName: string): Integer;
var
  i: Integer;
  SearchName: string;
begin
  SearchName := UpperCase(ArrName);
  for i := 0 to High(FArrays) do
    if FArrays[i].Name = SearchName then
      Exit(i);
  Result := -1;
end;

function TSSAProgram.GetArray(Index: Integer): TSSAArrayInfo;
begin
  if (Index >= 0) and (Index <= High(FArrays)) then
    Result := FArrays[Index]
  else
  begin
    FillChar(Result, SizeOf(Result), 0);
    Result.ArrayIndex := -1;
  end;
end;

function TSSAProgram.GetArrayCount: Integer;
begin
  Result := Length(FArrays);
end;

function TSSAProgram.GetDomTree: TObject;
begin
  { PHASE 3 TIER 2: Return dominator tree as TObject to avoid circular dependency.
    Caller must cast to TDominatorTree after including SedaiDominators in uses.
    Returns nil if dominator tree was not built (e.g., program contains TRAP). }
  if FDomTreeValid then
    Result := FDomTreeObj
  else
    Result := nil;
end;

procedure TSSAProgram.ClearDomTree;
begin
  { Clear dominator tree internal structures. Call this after CFG modifications
    (like LICM creating pre-header blocks) to avoid stale references. }
  if Assigned(FDomTreeObj) then
  begin
    TDominatorTree(FDomTreeObj).Clear;
    FDomTreeValid := False;
  end;
end;

procedure TSSAProgram.BuildDominatorTree;
var
  LogFile: TextFile;
  LogPath: string;
begin
  { PHASE 3 TIER 2: Build dominator tree for optimization passes.

    This must be called AFTER SSA construction is complete and BEFORE
    any optimization passes that depend on dominance (GVN, LICM, etc.). }

  if FBlocks.Count = 0 then
  begin
    {$IFDEF DEBUG_DOMTREE}
    if DebugDomTree then
      WriteLn('[TSSAProgram] WARNING: Cannot build dominator tree - no blocks');
    {$ENDIF}
    Exit;
  end;

  // NOTE: Programs with TRAP have unreachable blocks (error handlers), but
  // SedaiDominators.pas now handles these correctly by treating them as
  // secondary entry points. So we can build dominator tree for all programs.

  try
    {$IFDEF DEBUG_DOMTREE}
    if DebugDomTree then
      WriteLn('[TSSAProgram] Building dominator tree...');
    {$ENDIF}

    // Build the dominator tree using Cooper-Harvey-Kennedy algorithm
    TDominatorTree(FDomTreeObj).Build(Self);
    FDomTreeValid := True;

    // Dump dominator tree to log file only when debug is enabled
    {$IFDEF DEBUG_DOMTREE}
    if DebugDomTree then
    begin
      LogPath := 'job' + PathDelim + 'log' + PathDelim + 'dominator_tree.log';

      try
        AssignFile(LogFile, LogPath);
        Rewrite(LogFile);

        WriteLn(LogFile, '=== DOMINATOR TREE - PREORDER TRAVERSAL ===');
        WriteLn(LogFile, 'Date: ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
        WriteLn(LogFile, 'Blocks: ', FBlocks.Count);
        WriteLn(LogFile);

        WriteLn(LogFile, 'Full dominator tree structure:');
        WriteLn(LogFile, TDominatorTree(FDomTreeObj).DumpTree);

        CloseFile(LogFile);
        WriteLn('[TSSAProgram] Dominator tree logged to: ', LogPath);
      except
        on E: Exception do
          WriteLn('[TSSAProgram] WARNING: Failed to write log file: ', E.Message);
      end;
    end;
    {$ENDIF}

  except
    on E: Exception do
    begin
      WriteLn('[TSSAProgram] ERROR: Dominator tree construction failed!');
      WriteLn('  Exception: ', E.ClassName, ': ', E.Message);
      FDomTreeValid := False;
      raise;  // Re-raise to abort compilation
    end;
  end;
end;

procedure TSSAProgram.RunSSAConstruction;
var
  SSAConstr: TSSAConstruction;
begin
  { PHASE 3: Convert to proper Semi-Pruned SSA form with PHI functions and unique versioning.

    Semi-Pruned SSA (Briggs et al., 1998):
    - Places PHI only for variables that are LIVE at merge points
    - 40-70% fewer PHI nodes than Minimal SSA (Cytron et al.)
    - Faster construction and optimization
    - Fully compatible with all optimizations (GVN, LICM, CSE, DCE, etc.) }

  if not Assigned(FDomTreeObj) then
  begin
    WriteLn('[TSSAProgram] ERROR: Dominator tree not built - call BuildDominatorTree first!');
    Exit;
  end;

  // Skip SSA construction if dominator tree is not valid
  if not FDomTreeValid then
  begin
    WriteLn('[TSSAProgram] Skipping SSA construction (dominator tree not valid)');
    Exit;
  end;

  //WriteLn;
  // BASIC has global variable semantics (modifications in GOSUB persist after RETURN)
  SSAConstr := TSSAConstruction.Create(Self, TDominatorTree(FDomTreeObj), True);
  try
    SSAConstr.Run;
  finally
    SSAConstr.Free;
  end;
  //WriteLn;
end;

procedure TSSAProgram.RunPhiElimination;
var
  PhiElim: TPhiElimination;
begin
  { FINAL PASS: Convert PHI functions to copy instructions.

    This MUST run:
    - AFTER all SSA optimizations (they need PHI for analysis)
    - BEFORE bytecode compilation (bytecode has no PHI instructions)

    Standard SSA Deconstruction:
    - Each PHI is replaced by copy instructions in predecessor blocks
    - Copies are inserted BEFORE the terminator (jump/branch)
    - Result: SSA program without PHI, ready for bytecode compilation }

  PhiElim := TPhiElimination.Create(Self);
  try
    PhiElim.Run;
  finally
    PhiElim.Free;
  end;
end;

function TSSAProgram.RunGVN: Integer;
var
  GVNPass: TGVNPass;
begin
  { PHASE 3 TIER 2: Run Global Value Numbering optimization

    This pass eliminates redundant computations by identifying equivalent
    expressions and reusing their results. Must be called AFTER
    BuildDominatorTree. }

  Result := 0;

  if not FDomTreeValid then
  begin
    {$IFDEF DEBUG_GVN}
    if DebugGVN then
      WriteLn('[TSSAProgram] WARNING: Dominator tree not built. Skipping GVN.');
    {$ENDIF}
    Exit;
  end;

  {$IFDEF DEBUG_GVN}
  if DebugGVN then
    WriteLn('[TSSAProgram] Running GVN optimization pass...');
  {$ENDIF}

  GVNPass := TGVNPass.Create;
  try
    Result := GVNPass.Run(Self);
    {$IFDEF DEBUG_GVN}
    if DebugGVN then
      WriteLn(Format('[TSSAProgram] GVN complete: %d redundant values eliminated', [Result]));
    {$ENDIF}
  finally
    GVNPass.Free;
  end;
end;

function TSSAProgram.RunCSE: Integer;
var
  CSE: TCommonSubexpressionElimination;
begin
  { Common subexpression elimination - eliminates redundant computations }

  Result := 0;

  {$IFDEF DEBUG_CSE}
  if DebugCSE then
    WriteLn('[TSSAProgram] Running common subexpression elimination...');
  {$ENDIF}

  CSE := TCommonSubexpressionElimination.Create(Self);
  try
    Result := CSE.Run;
    {$IFDEF DEBUG_CSE}
    if DebugCSE then
      WriteLn(Format('[TSSAProgram] CSE complete: %d expressions eliminated', [Result]));
    {$ENDIF}
  finally
    CSE.Free;
  end;
end;

function TSSAProgram.RunCopyProp: Integer;
var
  CopyProp: TCopyPropagation;
begin
  { Copy propagation - eliminates redundant register copies }

  Result := 0;

  {$IFDEF DEBUG_COPYPROP}
  if DebugCopyProp then
    WriteLn('[TSSAProgram] Running copy propagation...');
  {$ENDIF}

  CopyProp := TCopyPropagation.Create(Self);
  try
    Result := CopyProp.Run;
    {$IFDEF DEBUG_COPYPROP}
    if DebugCopyProp then
      WriteLn(Format('[TSSAProgram] CopyProp complete: %d copies propagated', [Result]));
    {$ENDIF}
  finally
    CopyProp.Free;
  end;
end;

function TSSAProgram.RunAlgebraic: Integer;
var
  Algebraic: TAlgebraicSimplification;
begin
  Result := 0;
  {$IFDEF DEBUG_ALGEBRAIC}
  if DebugAlgebraic then
    WriteLn('[TSSAProgram] Running algebraic simplification...');
  {$ENDIF}
  Algebraic := TAlgebraicSimplification.Create(Self);
  try
    Result := Algebraic.Run;
    {$IFDEF DEBUG_ALGEBRAIC}
    if DebugAlgebraic then
      WriteLn(Format('[TSSAProgram] Algebraic complete: %d simplifications applied', [Result]));
    {$ENDIF}
  finally
    Algebraic.Free;
  end;
end;

function TSSAProgram.RunStrengthReduction: Integer;
var
  StrengthRed: TStrengthReduction;
begin
  Result := 0;
  {$IFDEF DEBUG_STRENGTH}
  if DebugStrength then
    WriteLn('[TSSAProgram] Running strength reduction...');
  {$ENDIF}
  StrengthRed := TStrengthReduction.Create(Self);
  try
    Result := StrengthRed.Run;
    {$IFDEF DEBUG_STRENGTH}
    if DebugStrength then
      WriteLn(Format('[TSSAProgram] StrengthReduction complete: %d reductions applied', [Result]));
    {$ENDIF}
  finally
    StrengthRed.Free;
  end;
end;

function TSSAProgram.RunGosubInlining: Integer;
var
  Inliner: TGosubInlining;
begin
  Result := 0;
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[TSSAProgram] Running GOSUB inlining...');
  {$ENDIF}
  Inliner := TGosubInlining.Create(Self);
  try
    Result := Inliner.Run;
    {$IFDEF DEBUG_SSA}
    if DebugSSA then
      WriteLn(Format('[TSSAProgram] GosubInlining complete: %d calls inlined', [Result]));
    {$ENDIF}
  finally
    Inliner.Free;
  end;
end;

function TSSAProgram.RunConstProp: Integer;
var
  ConstProp: TSimpleConstProp;
begin
  { Simple constant propagation - identifies single-assignment constants
    and propagates their values to enable folding }

  Result := 0;

  {$IFDEF DEBUG_CONSTPROP}
  if DebugConstProp then
    WriteLn('[TSSAProgram] Running simple constant propagation...');
  {$ENDIF}

  ConstProp := TSimpleConstProp.Create(Self);
  try
    Result := ConstProp.Run;
    {$IFDEF DEBUG_CONSTPROP}
    if DebugConstProp then
      WriteLn(Format('[TSSAProgram] ConstProp complete: %d values propagated', [Result]));
    {$ENDIF}
  finally
    ConstProp.Free;
  end;
end;

function TSSAProgram.RunAggressiveConstProp(Level: Integer): Integer;
var
  AggressiveCP: TAggressiveConstProp;
begin
  { SSA-aware constant propagation using versioned registers
    Level parameter is kept for compatibility but unused (SSA makes this simple) }

  Result := 0;

  {$IFDEF DEBUG_CONSTPROP}
  if DebugConstProp then
    WriteLn(Format('[TSSAProgram] Running SSA-aware constant propagation (Level %d)...', [Level]));
  {$ENDIF}

  AggressiveCP := TAggressiveConstProp.Create(Self);
  try
    Result := AggressiveCP.Run(Level);
    {$IFDEF DEBUG_CONSTPROP}
    if DebugConstProp then
      WriteLn(Format('[TSSAProgram] AggressiveCP complete: %d replacements', [Result]));
    {$ENDIF}
  finally
    AggressiveCP.Free;
  end;
end;

function TSSAProgram.RunDBE: Integer;
var
  DBE: TDeadBlockElimination;
begin
  { Dead block elimination - removes unreachable blocks before dominator tree construction }

  Result := 0;

  {$IFDEF DEBUG_DBE}
  if DebugDBE then
    WriteLn('[TSSAProgram] Running dead block elimination...');
  {$ENDIF}

  DBE := TDeadBlockElimination.Create(Self);
  try
    Result := DBE.Run;
    {$IFDEF DEBUG_DBE}
    if DebugDBE then
      WriteLn(Format('[TSSAProgram] DBE complete: %d blocks removed', [Result]));
    {$ENDIF}
  finally
    DBE.Free;
  end;
end;

function TSSAProgram.RunDCE: Integer;
var
  DCE: TDeadCodeElimination;
begin
  { Dead code elimination - removes unused instructions to reduce bytecode size }

  Result := 0;

  {$IFDEF DEBUG_DCE}
  if DebugDCE then
    WriteLn('[TSSAProgram] Running dead code elimination...');
  {$ENDIF}

  DCE := TDeadCodeElimination.Create(Self);
  try
    Result := DCE.Run;
    {$IFDEF DEBUG_DCE}
    if DebugDCE then
      WriteLn(Format('[TSSAProgram] DCE complete: %d instructions removed', [Result]));
    {$ENDIF}
  finally
    DCE.Free;
  end;
end;

function TSSAProgram.RunLICM: Integer;
var
  LICM: TLoopInvariantCodeMotion;
begin
  { Loop-Invariant Code Motion - moves loop-invariant computations outside loops }

  Result := 0;

  {$IFDEF DEBUG_LICM}
  if DebugLICM then
    WriteLn('[TSSAProgram] Running loop-invariant code motion...');
  {$ENDIF}

  LICM := TLoopInvariantCodeMotion.Create(Self);
  try
    Result := LICM.Run;
    {$IFDEF DEBUG_LICM}
    if DebugLICM then
      WriteLn(Format('[TSSAProgram] LICM complete: %d instructions hoisted', [Result]));
    {$ENDIF}
  finally
    LICM.Free;
  end;
end;

function TSSAProgram.RunLoopUnrolling: Integer;
var
  Unroller: TLoopUnroller;
begin
  { Loop Unrolling - duplicates loop bodies to reduce overhead }

  Result := 0;

  {$IFDEF DEBUG_SSA}
  WriteLn('[TSSAProgram] Running loop unrolling...');
  {$ENDIF}

  Unroller := TLoopUnroller.Create(Self);
  try
    Result := Unroller.Run;
    {$IFDEF DEBUG_SSA}
    WriteLn(Format('[TSSAProgram] Loop unrolling complete: %d loops unrolled', [Result]));
    {$ENDIF}
  finally
    Unroller.Free;
  end;
end;

function TSSAProgram.RunCopyCoalescing: Integer;
var
  CopyCoal: TCopyCoalescing;
begin
  { Copy Coalescing - eliminates redundant Copy instructions from PHI Elimination }

  Result := 0;

  {$IFDEF DEBUG_COPYCOAL}
  if DebugCopyCoal then
    WriteLn('[TSSAProgram] Running copy coalescing...');
  {$ENDIF}

  CopyCoal := TCopyCoalescing.Create(Self);
  try
    Result := CopyCoal.Run;
    {$IFDEF DEBUG_COPYCOAL}
    if DebugCopyCoal then
      WriteLn(Format('[TSSAProgram] Copy coalescing complete: %d copies coalesced', [Result]));
    {$ENDIF}
  finally
    CopyCoal.Free;
  end;
end;

function MakeSSAValue(Kind: TSSAValueKind): TSSAValue;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Kind := Kind;
  Result.RegType := srtInt;
  Result.RegIndex := -1;
  Result.Version := 0;  // Unversioned/legacy
end;

function MakeSSARegister(RegType: TSSARegisterType; RegIndex: Integer): TSSAValue;
begin
  Result := MakeSSAValue(svkRegister);
  Result.RegType := RegType;
  Result.RegIndex := RegIndex;
  Result.Version := 0;  // Unversioned by default, will be set by renaming pass
end;

function MakeSSAConstInt(Value: Int64): TSSAValue;
begin
  Result := MakeSSAValue(svkConstInt);
  Result.RegType := srtInt;
  Result.ConstInt := Value;
end;

function MakeSSAConstFloat(Value: Double): TSSAValue;
begin
  Result := MakeSSAValue(svkConstFloat);
  Result.RegType := srtFloat;
  Result.ConstFloat := Value;
end;

function MakeSSAConstString(const Value: string): TSSAValue;
begin
  Result := MakeSSAValue(svkConstString);
  Result.RegType := srtString;
  Result.ConstString := Value;
end;

function MakeSSAVariable(const VarName: string): TSSAValue;
begin
  Result := MakeSSAValue(svkVariable);
  Result.VarName := VarName;
end;

function MakeSSALabel(const LabelName: string): TSSAValue;
begin
  Result := MakeSSAValue(svkLabel);
  Result.LabelName := LabelName;
end;

function MakeSSAArrayRef(ArrayIdx: Integer; ElementType: TSSARegisterType): TSSAValue;
begin
  Result := MakeSSAValue(svkArrayRef);
  Result.ArrayIndex := ArrayIdx;
  Result.RegType := ElementType;
end;

function SSAValueToString(const Value: TSSAValue): string;
begin
  case Value.Kind of
    svkNone: Result := '<none>';
    svkRegister:
    begin
      Result := Format('%s[%d]', [SSARegisterTypeToString(Value.RegType), Value.RegIndex]);
      if Value.Version > 0 then
        Result := Result + '_' + IntToStr(Value.Version);  // Show versioning: R0_1, R0_2, etc.
    end;
    svkConstInt: Result := IntToStr(Value.ConstInt);
    svkConstFloat: Result := FloatToStr(Value.ConstFloat);
    svkConstString: Result := '"' + Value.ConstString + '"';
    svkVariable: Result := Value.VarName;
    svkLabel: Result := Value.LabelName;
    svkArrayRef: Result := Format('ARR[%d]', [Value.ArrayIndex]);
  else
    Result := '<unknown>';
  end;
end;

function SSAOpCodeToString(OpCode: TSSAOpCode): string;
begin
  Result := GetEnumName(TypeInfo(TSSAOpCode), Ord(OpCode));
  if Copy(Result, 1, 3) = 'ssa' then
    Result := Copy(Result, 4, Length(Result) - 3);
end;

function SSARegisterTypeToString(RegType: TSSARegisterType): string;
begin
  case RegType of
    srtInt: Result := 'INT';
    srtFloat: Result := 'FLT';
    srtString: Result := 'STR';
  else
    Result := '???';
  end;
end;

procedure TSSAProgram.PrintSSA;
var
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  i, j: Integer;
begin
  WriteLn('=== SSA PROGRAM ===');
  WriteLn('Blocks: ', FBlocks.Count);
  WriteLn;

  for i := 0 to FBlocks.Count - 1 do
  begin
    Block := FBlocks[i];
    WriteLn('BLOCK: ', Block.LabelName);
    WriteLn('  Predecessors: ', Block.Predecessors.Count);
    WriteLn('  Successors: ', Block.Successors.Count);
    WriteLn('  Instructions: ', Block.Instructions.Count);
    WriteLn;

    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];
      WriteLn('  ', Format('%3d', [j]), ': ', Instr.ToString);
    end;
    WriteLn;
  end;

  WriteLn('=== END SSA PROGRAM ===');
end;

end.
