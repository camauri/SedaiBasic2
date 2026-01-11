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
unit SedaiBytecodeTypes;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, fgl, SedaiSSATypes;

{ Bytecode Opcode Groups (2-byte encoding: Group.Opcode)
  - Group 0 (0x00xx): Core VM operations
  - Group 1 (0x01xx): String operations
  - Group 2 (0x02xx): Math functions
  - Group 3 (0x03xx): Array operations
  - Group 4 (0x04xx): I/O operations
  - Group 5 (0x05xx): Special variables
  - Group 6 (0x06xx): File I/O (reserved)
  - Group 7-9 (0x07xx-0x09xx): Reserved
  - Group 10 (0x0Axx): Graphics
  - Group 11 (0x0Bxx): Sound
  - Groups 12-199: Future expansion
  - Groups 200-255 (0xC8xx-0xFFxx): Superinstructions
}

{ Opcode group constants }
const
  bcGroupCore       = $0000;  // Group 0: Core VM
  bcGroupString     = $0100;  // Group 1: String operations
  bcGroupMath       = $0200;  // Group 2: Math functions
  bcGroupArray      = $0300;  // Group 3: Array operations
  bcGroupIO         = $0400;  // Group 4: I/O operations
  bcGroupSpecial    = $0500;  // Group 5: Special variables
  bcGroupFileIO     = $0600;  // Group 6: File I/O
  bcGroupSprite     = $0700;  // Group 7: Sprite operations
  bcGroupGraphics   = $0A00;  // Group 10: Graphics
  bcGroupSound      = $0B00;  // Group 11: Sound
  bcGroupSuper      = $C800;  // Group 200+: Superinstructions

type
  { Bytecode Opcode - 2-byte encoding for extensibility }
  TBytecodeOp = Word;

{ ============================================================================
  IMPORTANT: WHEN ADDING A NEW OPCODE

  You MUST also update SedaiRegisterCompaction.pas to handle the new opcode!

  For each new opcode, add it to the appropriate function(s):
    - DestIsIntReg()     - if Dest is written as INT
    - DestIsFloatReg()   - if Dest is written as FLOAT
    - DestIsStringReg()  - if Dest is written as STRING
    - DestReadIsIntReg() - if Dest is READ as INT (e.g. ArrayStore value)
    - DestReadIsFloatReg() - if Dest is READ as FLOAT
    - DestReadIsStringReg() - if Dest is READ as STRING
    - Src1IsIntReg()     - if Src1 is INT
    - Src1IsFloatReg()   - if Src1 is FLOAT
    - Src1IsStringReg()  - if Src1 is STRING
    - Src2IsIntReg()     - if Src2 is INT
    - Src2IsFloatReg()   - if Src2 is FLOAT
    - Src2IsStringReg()  - if Src2 is STRING
    - ImmediateIsFloatReg() - if Immediate contains float register index
    - Special handling in RewriteInstructions() for packed Immediate fields

  Failure to do this will cause register mismatch bugs when optimizations
  are enabled (the register compaction pass won't remap registers correctly).
  ============================================================================ }

const
  // === GROUP 0: CORE VM OPERATIONS (0x00xx) ===
  // Load constants
  bcLoadConstInt    = bcGroupCore + 0;
  bcLoadConstFloat  = bcGroupCore + 1;
  bcLoadConstString = bcGroupCore + 2;
  // Copy between registers
  bcCopyInt         = bcGroupCore + 3;
  bcCopyFloat       = bcGroupCore + 4;
  bcCopyString      = bcGroupCore + 5;
  // Variable operations
  bcLoadVar         = bcGroupCore + 6;
  bcStoreVar        = bcGroupCore + 7;
  // Integer arithmetic
  bcAddInt          = bcGroupCore + 8;
  bcSubInt          = bcGroupCore + 9;
  bcMulInt          = bcGroupCore + 10;
  bcDivInt          = bcGroupCore + 11;
  bcModInt          = bcGroupCore + 12;
  bcNegInt          = bcGroupCore + 13;
  // Float arithmetic
  bcAddFloat        = bcGroupCore + 14;
  bcSubFloat        = bcGroupCore + 15;
  bcMulFloat        = bcGroupCore + 16;
  bcDivFloat        = bcGroupCore + 17;
  bcPowFloat        = bcGroupCore + 18;
  bcNegFloat        = bcGroupCore + 19;
  // Type conversions
  bcIntToFloat      = bcGroupCore + 20;
  bcFloatToInt      = bcGroupCore + 21;
  bcIntToString     = bcGroupCore + 22;
  bcFloatToString   = bcGroupCore + 23;
  bcStringToInt     = bcGroupCore + 24;
  bcStringToFloat   = bcGroupCore + 25;
  // Comparisons - Int
  bcCmpEqInt        = bcGroupCore + 26;
  bcCmpNeInt        = bcGroupCore + 27;
  bcCmpLtInt        = bcGroupCore + 28;
  bcCmpGtInt        = bcGroupCore + 29;
  bcCmpLeInt        = bcGroupCore + 30;
  bcCmpGeInt        = bcGroupCore + 31;
  // Comparisons - Float
  bcCmpEqFloat      = bcGroupCore + 32;
  bcCmpNeFloat      = bcGroupCore + 33;
  bcCmpLtFloat      = bcGroupCore + 34;
  bcCmpGtFloat      = bcGroupCore + 35;
  bcCmpLeFloat      = bcGroupCore + 36;
  bcCmpGeFloat      = bcGroupCore + 37;
  // Comparisons - String
  bcCmpEqString     = bcGroupCore + 38;
  bcCmpNeString     = bcGroupCore + 39;
  bcCmpLtString     = bcGroupCore + 40;
  bcCmpGtString     = bcGroupCore + 41;
  // Bitwise operations
  bcBitwiseAnd      = bcGroupCore + 42;
  bcBitwiseOr       = bcGroupCore + 43;
  bcBitwiseXor      = bcGroupCore + 44;
  bcBitwiseNot      = bcGroupCore + 45;
  // Control flow
  bcJump            = bcGroupCore + 46;
  bcJumpIfZero      = bcGroupCore + 47;
  bcJumpIfNotZero   = bcGroupCore + 48;
  bcCall            = bcGroupCore + 49;
  bcReturn          = bcGroupCore + 50;
  // System commands
  bcEnd             = bcGroupCore + 51;
  bcStop            = bcGroupCore + 52;
  bcFast            = bcGroupCore + 53;
  bcSlow            = bcGroupCore + 54;
  bcSleep           = bcGroupCore + 55;
  bcKey             = bcGroupCore + 56;  // KEY n, "text" - define function key
  bcNop             = bcGroupCore + 57;
  bcClear           = bcGroupCore + 58;  // CLR - clear all variables
  // Debug/Trace
  bcTron            = bcGroupCore + 59;  // TRON - enable trace mode
  bcTroff           = bcGroupCore + 60;  // TROFF - disable trace mode
  // DATA handling
  bcDataAdd         = bcGroupCore + 61;  // Add value to DATA pool
  bcDataReadInt     = bcGroupCore + 62;  // Read next DATA value into int register
  bcDataReadFloat   = bcGroupCore + 63;  // Read next DATA value into float register
  bcDataReadString  = bcGroupCore + 64;  // Read next DATA value into string register
  bcDataRestore     = bcGroupCore + 65;  // Reset DATA pointer
  // Input commands
  bcGet             = bcGroupCore + 66;  // GET A$ (non-blocking char input)
  bcGetkey          = bcGroupCore + 67;  // GETKEY A$ (blocking char input)
  // Formatted output
  bcPrintUsing      = bcGroupCore + 68;  // PRINT USING format$; value
  bcPudef           = bcGroupCore + 69;  // PUDEF format string
  bcChar            = bcGroupCore + 70;  // CHAR mode, col, row, text
  // File operations
  bcLoad            = bcGroupCore + 71;  // LOAD "filename": Load program from file
  bcSave            = bcGroupCore + 72;  // SAVE "filename": Save program to file
  bcVerify          = bcGroupCore + 73;  // VERIFY "filename": Verify program against file
  bcBload           = bcGroupCore + 74;  // BLOAD "filename": Load bytecode from file
  bcBsave           = bcGroupCore + 75;  // BSAVE "filename": Save bytecode to file
  bcBoot            = bcGroupCore + 76;  // BOOT "filename": Load and run bytecode
  // System commands (from program)
  bcRun             = bcGroupCore + 77;  // RUN [linenum]: Run program from beginning or line
  bcList            = bcGroupCore + 78;  // LIST [start-end]: List program lines
  bcNew             = bcGroupCore + 79;  // NEW: Clear program and variables
  bcDelete          = bcGroupCore + 80;  // DELETE [start[-end]]: Delete program lines
  bcRenumber        = bcGroupCore + 81;  // RENUMBER [new[,inc[,old]]]: Renumber program lines
  bcCatalog         = bcGroupCore + 82;  // CATALOG/DIR: List directory contents
  // File management commands (executed directly in VM)
  bcCopyFile        = bcGroupCore + 83;  // COPY/CP "src","dest"[,overwrite]: Copy file
  bcScratch         = bcGroupCore + 84;  // SCRATCH "pattern"[,force]: Delete file(s)
  bcRenameFile      = bcGroupCore + 85;  // RENAME "old","new": Rename file
  bcConcat          = bcGroupCore + 86;  // CONCAT "src","dest": Concatenate files
  bcMkdir           = bcGroupCore + 87;  // MKDIR/MD "path": Create directory
  bcChdir           = bcGroupCore + 88;  // CHDIR/CD "path": Change current directory
  bcMoveFile        = bcGroupCore + 89;  // MOVE/MV "src","dest": Move file
  // Error handling
  bcTrap            = bcGroupCore + 90;  // TRAP linenum: Set error handler line
  bcResume          = bcGroupCore + 91;  // RESUME: Continue after error at error line
  bcResumeNext      = bcGroupCore + 92;  // RESUME NEXT: Continue after error at next statement

  // === GROUP 1: STRING OPERATIONS (0x01xx) ===
  bcStrConcat       = bcGroupString + 0;
  bcStrLen          = bcGroupString + 1;
  bcStrLeft         = bcGroupString + 2;
  bcStrRight        = bcGroupString + 3;
  bcStrMid          = bcGroupString + 4;
  bcStrAsc          = bcGroupString + 5;
  bcStrChr          = bcGroupString + 6;
  bcStrStr          = bcGroupString + 7;
  bcStrVal          = bcGroupString + 8;
  bcStrHex          = bcGroupString + 9;
  bcStrInstr        = bcGroupString + 10;
  bcStrErr          = bcGroupString + 11;  // ERR$(n) - error message for code n

  // === GROUP 2: MATH FUNCTIONS (0x02xx) ===
  bcMathSin         = bcGroupMath + 0;
  bcMathCos         = bcGroupMath + 1;
  bcMathTan         = bcGroupMath + 2;
  bcMathAtn         = bcGroupMath + 3;
  bcMathLog         = bcGroupMath + 4;
  bcMathExp         = bcGroupMath + 5;
  bcMathSqr         = bcGroupMath + 6;
  bcMathAbs         = bcGroupMath + 7;
  bcMathSgn         = bcGroupMath + 8;
  bcMathInt         = bcGroupMath + 9;
  bcMathRnd         = bcGroupMath + 10;
  bcMathLog10       = bcGroupMath + 11;  // LOG10(x) - base 10 logarithm
  bcMathLog2        = bcGroupMath + 12;  // LOG2(x) - base 2 logarithm
  bcMathLogN        = bcGroupMath + 13;  // LOGN(n, x) - base n logarithm
  bcStrDec          = bcGroupMath + 14;  // DEC(hexstr) - convert hex string to decimal

  // === GROUP 3: ARRAY OPERATIONS (0x03xx) ===
  bcArrayLoad       = bcGroupArray + 0;   // Generic (deprecated)
  bcArrayStore      = bcGroupArray + 1;   // Generic (deprecated)
  bcArrayDim        = bcGroupArray + 2;
  bcArrayLoadInt    = bcGroupArray + 3;
  bcArrayLoadFloat  = bcGroupArray + 4;
  bcArrayLoadString = bcGroupArray + 5;
  bcArrayStoreInt   = bcGroupArray + 6;
  bcArrayStoreFloat = bcGroupArray + 7;
  bcArrayStoreString = bcGroupArray + 8;

  // === GROUP 4: I/O OPERATIONS (0x04xx) ===
  // Print values
  bcPrint           = bcGroupIO + 0;
  bcPrintLn         = bcGroupIO + 1;
  bcPrintString     = bcGroupIO + 2;
  bcPrintStringLn   = bcGroupIO + 3;
  bcPrintInt        = bcGroupIO + 4;
  bcPrintIntLn      = bcGroupIO + 5;
  // Print separators and formatting
  bcPrintComma      = bcGroupIO + 6;
  bcPrintSemicolon  = bcGroupIO + 7;
  bcPrintTab        = bcGroupIO + 8;
  bcPrintSpc        = bcGroupIO + 9;
  bcPrintNewLine    = bcGroupIO + 10;
  // Input
  bcInput           = bcGroupIO + 11;
  bcInputInt        = bcGroupIO + 12;
  bcInputFloat      = bcGroupIO + 13;
  bcInputString     = bcGroupIO + 14;

  // === GROUP 5: SPECIAL VARIABLES (0x05xx) ===
  bcLoadTI          = bcGroupSpecial + 0;   // TI: jiffies since start
  bcLoadTIS         = bcGroupSpecial + 1;   // TI$: current time HHMMSS
  bcStoreTIS        = bcGroupSpecial + 2;   // TI$ = value
  bcLoadDTS         = bcGroupSpecial + 3;   // DT$: current date YYYYMMDD
  bcFre             = bcGroupSpecial + 4;   // FRE(x): available memory bytes
  bcLoadEL          = bcGroupSpecial + 5;   // EL: last error line number
  bcLoadER          = bcGroupSpecial + 6;   // ER: last error code
  bcLoadERRS        = bcGroupSpecial + 7;   // ERR$: last error message (variable)

  // === GROUP 6: FILE I/O (0x06xx) ===
  bcDopen           = bcGroupFileIO + 0;    // DOPEN #handle, "filename" [, mode$]
  bcDclose          = bcGroupFileIO + 1;    // DCLOSE #handle
  bcOpen            = bcGroupFileIO + 2;    // OPEN (legacy C64/C128 style)
  bcClose           = bcGroupFileIO + 3;    // CLOSE (legacy C64/C128 style)
  bcGetFile         = bcGroupFileIO + 4;    // GET# file, var
  bcInputFile       = bcGroupFileIO + 5;    // INPUT# file, vars
  bcPrintFile       = bcGroupFileIO + 6;    // PRINT# file, exprs
  bcCmd             = bcGroupFileIO + 7;    // CMD file [, expr]

  // === GROUP 7: SPRITE OPERATIONS (0x07xx) ===
  // Sprite commands
  bcSprite          = bcGroupSprite + 0;    // SPRITE n [,on] [,color] [,priority] [,xscale] [,yscale] [,mode]
  bcMovsprAbs       = bcGroupSprite + 1;    // MOVSPR n, x, y: Position at absolute coordinates
  bcMovsprRel       = bcGroupSprite + 2;    // MOVSPR n, +x, +y: Move relative to current position
  bcMovsprPolar     = bcGroupSprite + 3;    // MOVSPR n, dist;angle: Move by distance at angle
  bcMovsprAuto      = bcGroupSprite + 4;    // MOVSPR n, angle#speed: Start automatic movement
  bcSprcolor        = bcGroupSprite + 5;    // SPRCOLOR [mc1] [,mc2]: Set global multicolors
  bcSprsav          = bcGroupSprite + 6;    // SPRSAV src, dst: Save/load/copy sprite data
  bcCollision       = bcGroupSprite + 7;    // COLLISION type [,line]: Set collision handler
  // Sprite functions (return values)
  bcBump            = bcGroupSprite + 8;    // BUMP(n): Return collision bitmask
  bcRspcolor        = bcGroupSprite + 9;    // RSPCOLOR(n): Return multicolor value
  bcRsppos          = bcGroupSprite + 10;   // RSPPOS(sprite, n): Return position/speed
  bcRsprite         = bcGroupSprite + 11;   // RSPRITE(sprite, n): Return sprite attribute

  // === GROUP 10: GRAPHICS (0x0Axx) ===
  bcGraphicRGBA     = bcGroupGraphics + 0;
  bcGraphicSetMode  = bcGroupGraphics + 1;
  bcGraphicBox      = bcGroupGraphics + 2;
  bcGraphicCircle   = bcGroupGraphics + 3;
  bcGraphicDraw     = bcGroupGraphics + 4;
  bcGraphicLocate   = bcGroupGraphics + 5;
  bcGraphicRdot     = bcGroupGraphics + 6;
  bcGraphicGetMode  = bcGroupGraphics + 7;
  bcGraphicColor    = bcGroupGraphics + 8;   // COLOR source, color
  bcGraphicWidth    = bcGroupGraphics + 9;   // WIDTH n
  bcGraphicScale    = bcGroupGraphics + 10;  // SCALE n [,xmax, ymax]
  bcGraphicPaint    = bcGroupGraphics + 11;  // PAINT [source], x, y [,mode]
  bcGraphicWindow   = bcGroupGraphics + 12;  // WINDOW col1, row1, col2, row2 [,clear]
  bcGraphicSShape   = bcGroupGraphics + 13;  // SSHAPE A$, x1, y1 [,x2, y2]
  bcGraphicGShape   = bcGroupGraphics + 14;  // GSHAPE A$, x, y [,mode]
  bcGraphicGList    = bcGroupGraphics + 15;  // GLIST
  bcGraphicPos      = bcGroupGraphics + 16;  // POS(x)
  bcGraphicRclr     = bcGroupGraphics + 17;  // RCLR(n)
  bcGraphicRwindow  = bcGroupGraphics + 18;  // RWINDOW(n)

  // === GROUP 11: SOUND (0x0Bxx) ===
  bcSoundVol        = bcGroupSound + 0;
  bcSoundSound      = bcGroupSound + 1;
  bcSoundEnvelope   = bcGroupSound + 2;
  bcSoundTempo      = bcGroupSound + 3;
  bcSoundPlay       = bcGroupSound + 4;
  bcSoundFilter     = bcGroupSound + 5;

  // === SUPERINSTRUCTIONS (0xC8xx+) ===
  // Fused compare-and-branch (Int)
  bcBranchEqInt     = bcGroupSuper + 0;
  bcBranchNeInt     = bcGroupSuper + 1;
  bcBranchLtInt     = bcGroupSuper + 2;
  bcBranchGtInt     = bcGroupSuper + 3;
  bcBranchLeInt     = bcGroupSuper + 4;
  bcBranchGeInt     = bcGroupSuper + 5;
  // Fused compare-and-branch (Float)
  bcBranchEqFloat   = bcGroupSuper + 10;
  bcBranchNeFloat   = bcGroupSuper + 11;
  bcBranchLtFloat   = bcGroupSuper + 12;
  bcBranchGtFloat   = bcGroupSuper + 13;
  bcBranchLeFloat   = bcGroupSuper + 14;
  bcBranchGeFloat   = bcGroupSuper + 15;
  // Fused arithmetic-to-dest (Int)
  bcAddIntTo        = bcGroupSuper + 20;
  bcSubIntTo        = bcGroupSuper + 21;
  bcMulIntTo        = bcGroupSuper + 22;
  // Fused arithmetic-to-dest (Float)
  bcAddFloatTo      = bcGroupSuper + 30;
  bcSubFloatTo      = bcGroupSuper + 31;
  bcMulFloatTo      = bcGroupSuper + 32;
  bcDivFloatTo      = bcGroupSuper + 33;
  // Fused constant arithmetic (Int)
  bcAddIntConst     = bcGroupSuper + 40;
  bcSubIntConst     = bcGroupSuper + 41;
  bcMulIntConst     = bcGroupSuper + 42;
  // Fused constant arithmetic (Float)
  bcAddFloatConst   = bcGroupSuper + 50;
  bcSubFloatConst   = bcGroupSuper + 51;
  bcMulFloatConst   = bcGroupSuper + 52;
  bcDivFloatConst   = bcGroupSuper + 53;
  // Fused compare-zero-and-branch
  bcBranchEqZeroInt = bcGroupSuper + 60;
  bcBranchNeZeroInt = bcGroupSuper + 61;
  bcBranchEqZeroFloat = bcGroupSuper + 70;
  bcBranchNeZeroFloat = bcGroupSuper + 71;
  // Fused array-store-constant
  bcArrayStoreIntConst   = bcGroupSuper + 80;
  bcArrayStoreFloatConst = bcGroupSuper + 81;
  bcArrayStoreStringConst = bcGroupSuper + 82;

  // Fused loop increment-and-branch (Int)
  bcAddIntToBranchLe = bcGroupSuper + 90;
  bcAddIntToBranchLt = bcGroupSuper + 91;
  bcSubIntToBranchGe = bcGroupSuper + 92;
  bcSubIntToBranchGt = bcGroupSuper + 93;

  // Fused Multiply-Add/Sub (FMA)
  bcMulAddFloat    = bcGroupSuper + 100;
  bcMulSubFloat    = bcGroupSuper + 101;
  bcMulAddToFloat  = bcGroupSuper + 102;
  bcMulSubToFloat  = bcGroupSuper + 103;

  // Array Load + Arithmetic
  bcArrayLoadAddFloat    = bcGroupSuper + 110;
  bcArrayLoadSubFloat    = bcGroupSuper + 111;
  bcArrayLoadDivAddFloat = bcGroupSuper + 112;

  // Square-Sum pattern
  bcSquareSumFloat = bcGroupSuper + 120;
  bcAddSquareFloat = bcGroupSuper + 121;

  // Mul-Mul chain
  bcMulMulFloat = bcGroupSuper + 130;

  // Add-Sqrt
  bcAddSqrtFloat = bcGroupSuper + 131;

  // Array Load + Branch
  bcArrayLoadIntBranchNZ = bcGroupSuper + 140;
  bcArrayLoadIntBranchZ  = bcGroupSuper + 141;

  // Additional superinstructions (sub-opcodes 150-157, 250-255)
  bcArrayReverseRange = bcGroupSuper + 156;
  bcArrayShiftLeft    = bcGroupSuper + 157;
  bcArraySwapInt      = bcGroupSuper + 250;
  bcAddIntSelf        = bcGroupSuper + 251;
  bcSubIntSelf        = bcGroupSuper + 252;
  bcArrayLoadIntTo    = bcGroupSuper + 253;  // ArrayLoadIntToReg
  bcArrayCopyElement  = bcGroupSuper + 254;  // ArrayCopyInt
  bcArrayMoveElement  = bcGroupSuper + 255;  // ArrayCopyIntSwap

  // Helper function to extract group from opcode
  function GetOpcodeGroup(Op: TBytecodeOp): Word; inline;

type

  { Bytecode instruction encoding }
  TBytecodeInstruction = packed record
    OpCode: Word;           // Opcode (TBytecodeOp) - 2 bytes for group.opcode encoding
    Dest: Word;             // Destination register index (2 bytes, 0-65535)
    Src1: Word;             // Source 1 register index (2 bytes, 0-65535)
    Src2: Word;             // Source 2 register index (2 bytes, 0-65535)
    Immediate: Int64;       // Immediate value (for constants, jump offsets, etc)
    SourceLine: Integer;    // BASIC source line number (0 if unknown)
  end;

  { Variable info for runtime }
  TVariableInfo = record
    Name: string;
    RegType: Byte;          // 0=Int, 1=Float, 2=String
    IsArray: Boolean;
    ArraySize: Integer;
  end;

  { Variable Table - compatibility with executor context }
  TVariableTable = class
  private
    FNames: TStringList;
    function GetCount: Integer;
    function GetName(Index: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    function AllocateVariable(const Name: string): Integer;
    function FindVariable(const Name: string): Integer;
    function GetOrAllocate(const Name: string): Integer;
    procedure Clear;
    property Count: Integer read GetCount;
    property Names[Index: Integer]: string read GetName; default;
  end;

  { Bytecode program - compiled bytecode ready for VM }
  TBytecodeProgram = class
  private
    FInstructions: array of TBytecodeInstruction;
    FVariables: array of TVariableInfo;
    FArrays: array of TSSAArrayInfo;
    FStringConstants: TStringList;
    FEntryPoint: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddInstruction(const Instr: TBytecodeInstruction);
    procedure SetInstruction(Index: Integer; const Instr: TBytecodeInstruction);
    procedure ClearInstructions;
    procedure AddVariable(const VarInfo: TVariableInfo);
    procedure AddArrayInfo(const ArrInfo: TSSAArrayInfo);
    procedure SetArray(Index: Integer; const ArrInfo: TSSAArrayInfo);
    function AddStringConstant(const Str: string): Integer;
    function GetInstruction(Index: Integer): TBytecodeInstruction;
    function GetInstructionCount: Integer;
    function GetVariable(Index: Integer): TVariableInfo;
    function GetVariableCount: Integer;
    function GetArray(Index: Integer): TSSAArrayInfo;
    function GetArrayCount: Integer;
    function GetInstructionsPtr: Pointer;  // Direct access for fast VM loop
    function FindPCForLine(LineNum: Integer): Integer;  // Find PC for BASIC line number
    function FindPCAfterLine(LineNum: Integer): Integer;  // Find PC for first instruction AFTER given line
    property StringConstants: TStringList read FStringConstants;
    property EntryPoint: Integer read FEntryPoint write FEntryPoint;
  end;

function BytecodeOpToString(Op: TBytecodeOp): string;
function OpcodeToString(OpCode: Word): string;  // Legacy compatibility wrapper
function MakeBytecodeInstruction(OpCode: TBytecodeOp; Dest, Src1, Src2: Word; Immediate: Int64): TBytecodeInstruction;

implementation

uses TypInfo;

{ Helper function to extract group from opcode }
function GetOpcodeGroup(Op: TBytecodeOp): Word; inline;
begin
  Result := Op shr 8;  // High byte is the group number
end;

{ TVariableTable }

constructor TVariableTable.Create;
begin
  inherited Create;
  FNames := TStringList.Create;
  FNames.CaseSensitive := False;
  FNames.Duplicates := dupIgnore;
end;

destructor TVariableTable.Destroy;
begin
  FNames.Free;
  inherited Destroy;
end;

function TVariableTable.GetCount: Integer;
begin
  Result := FNames.Count;
end;

function TVariableTable.GetName(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FNames.Count) then
    Result := FNames[Index]
  else
    Result := '';
end;

function TVariableTable.AllocateVariable(const Name: string): Integer;
begin
  Result := FNames.IndexOf(UpperCase(Name));
  if Result < 0 then
    Result := FNames.Add(UpperCase(Name));
end;

function TVariableTable.FindVariable(const Name: string): Integer;
begin
  Result := FNames.IndexOf(UpperCase(Name));
end;

function TVariableTable.GetOrAllocate(const Name: string): Integer;
begin
  Result := AllocateVariable(Name);
end;

procedure TVariableTable.Clear;
begin
  FNames.Clear;
end;

{ TBytecodeProgram }

constructor TBytecodeProgram.Create;
begin
  inherited Create;
  SetLength(FInstructions, 0);
  SetLength(FVariables, 0);
  FStringConstants := TStringList.Create;
  FStringConstants.CaseSensitive := True;  // IMPORTANT: "n" and "N" are different!
  FEntryPoint := 0;
end;

destructor TBytecodeProgram.Destroy;
begin
  FStringConstants.Free;
  inherited Destroy;
end;

procedure TBytecodeProgram.AddInstruction(const Instr: TBytecodeInstruction);
var
  Len: Integer;
begin
  Len := Length(FInstructions);
  SetLength(FInstructions, Len + 1);
  FInstructions[Len] := Instr;
end;

procedure TBytecodeProgram.SetInstruction(Index: Integer; const Instr: TBytecodeInstruction);
begin
  if (Index >= 0) and (Index < Length(FInstructions)) then
    FInstructions[Index] := Instr;
end;

procedure TBytecodeProgram.ClearInstructions;
begin
  SetLength(FInstructions, 0);
end;

procedure TBytecodeProgram.AddVariable(const VarInfo: TVariableInfo);
var
  Len: Integer;
begin
  Len := Length(FVariables);
  SetLength(FVariables, Len + 1);
  FVariables[Len] := VarInfo;
end;

function TBytecodeProgram.AddStringConstant(const Str: string): Integer;
begin
  Result := FStringConstants.IndexOf(Str);
  if Result = -1 then
    Result := FStringConstants.Add(Str);
end;

function TBytecodeProgram.GetInstruction(Index: Integer): TBytecodeInstruction;
begin
  if (Index >= 0) and (Index < Length(FInstructions)) then
    Result := FInstructions[Index]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TBytecodeProgram.GetInstructionCount: Integer;
begin
  Result := Length(FInstructions);
end;

function TBytecodeProgram.GetVariable(Index: Integer): TVariableInfo;
begin
  if (Index >= 0) and (Index < Length(FVariables)) then
    Result := FVariables[Index]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TBytecodeProgram.GetVariableCount: Integer;
begin
  Result := Length(FVariables);
end;

procedure TBytecodeProgram.AddArrayInfo(const ArrInfo: TSSAArrayInfo);
var
  Idx: Integer;
begin
  Idx := Length(FArrays);
  SetLength(FArrays, Idx + 1);
  FArrays[Idx] := ArrInfo;
end;

function TBytecodeProgram.GetArray(Index: Integer): TSSAArrayInfo;
begin
  if (Index >= 0) and (Index < Length(FArrays)) then
    Result := FArrays[Index]
  else
    raise Exception.CreateFmt('Array index out of bounds: %d', [Index]);
end;

procedure TBytecodeProgram.SetArray(Index: Integer; const ArrInfo: TSSAArrayInfo);
begin
  if (Index >= 0) and (Index < Length(FArrays)) then
    FArrays[Index] := ArrInfo;
end;

function TBytecodeProgram.GetArrayCount: Integer;
begin
  Result := Length(FArrays);
end;

function TBytecodeProgram.GetInstructionsPtr: Pointer;
begin
  if Length(FInstructions) > 0 then
    Result := @FInstructions[0]
  else
    Result := nil;
end;

function TBytecodeProgram.FindPCForLine(LineNum: Integer): Integer;
var
  i: Integer;
  BestPC, BestLine: Integer;
begin
  // Scan instructions to find the first one with matching source line
  for i := 0 to High(FInstructions) do
  begin
    if FInstructions[i].SourceLine = LineNum then
    begin
      Result := i;
      Exit;
    end;
  end;
  // Exact match not found - find instruction with MINIMUM SourceLine > LineNum
  // (handles REM statements and other non-code lines)
  // Instructions are not ordered by SourceLine, so we must scan all
  BestPC := -1;
  BestLine := MaxInt;
  for i := 0 to High(FInstructions) do
  begin
    if (FInstructions[i].SourceLine > LineNum) and
       (FInstructions[i].SourceLine < BestLine) then
    begin
      BestLine := FInstructions[i].SourceLine;
      BestPC := i;
    end;
  end;
  Result := BestPC;
end;

function TBytecodeProgram.FindPCAfterLine(LineNum: Integer): Integer;
var
  i: Integer;
  BestPC, BestLine: Integer;
begin
  // Find the first instruction with SourceLine > LineNum
  // This is used by RESUME NEXT to skip to the next BASIC line
  BestPC := -1;
  BestLine := MaxInt;
  for i := 0 to High(FInstructions) do
  begin
    if (FInstructions[i].SourceLine > LineNum) and
       (FInstructions[i].SourceLine < BestLine) then
    begin
      BestLine := FInstructions[i].SourceLine;
      BestPC := i;
    end;
  end;
  Result := BestPC;
end;

function BytecodeOpToString(Op: TBytecodeOp): string;
var
  Group, SubOp: Word;
begin
  Group := Op shr 8;
  SubOp := Op and $FF;

  case Group of
    0: // Core VM
      case SubOp of
        0: Result := 'LoadConstInt';
        1: Result := 'LoadConstFloat';
        2: Result := 'LoadConstString';
        3: Result := 'CopyInt';
        4: Result := 'CopyFloat';
        5: Result := 'CopyString';
        6: Result := 'LoadVar';
        7: Result := 'StoreVar';
        8: Result := 'AddInt';
        9: Result := 'SubInt';
        10: Result := 'MulInt';
        11: Result := 'DivInt';
        12: Result := 'ModInt';
        13: Result := 'NegInt';
        14: Result := 'AddFloat';
        15: Result := 'SubFloat';
        16: Result := 'MulFloat';
        17: Result := 'DivFloat';
        18: Result := 'PowFloat';
        19: Result := 'NegFloat';
        20: Result := 'IntToFloat';
        21: Result := 'FloatToInt';
        22: Result := 'IntToString';
        23: Result := 'FloatToString';
        24: Result := 'StringToInt';
        25: Result := 'StringToFloat';
        26: Result := 'CmpEqInt';
        27: Result := 'CmpNeInt';
        28: Result := 'CmpLtInt';
        29: Result := 'CmpGtInt';
        30: Result := 'CmpLeInt';
        31: Result := 'CmpGeInt';
        32: Result := 'CmpEqFloat';
        33: Result := 'CmpNeFloat';
        34: Result := 'CmpLtFloat';
        35: Result := 'CmpGtFloat';
        36: Result := 'CmpLeFloat';
        37: Result := 'CmpGeFloat';
        38: Result := 'CmpEqString';
        39: Result := 'CmpNeString';
        40: Result := 'CmpLtString';
        41: Result := 'CmpGtString';
        42: Result := 'BitwiseAnd';
        43: Result := 'BitwiseOr';
        44: Result := 'BitwiseXor';
        45: Result := 'BitwiseNot';
        46: Result := 'Jump';
        47: Result := 'JumpIfZero';
        48: Result := 'JumpIfNotZero';
        49: Result := 'Call';
        50: Result := 'Return';
        51: Result := 'End';
        52: Result := 'Stop';
        53: Result := 'Fast';
        54: Result := 'Slow';
        55: Result := 'Sleep';
        56: Result := 'Nop';
      else
        Result := Format('Core_%d', [SubOp]);
      end;
    1: // String
      case SubOp of
        0: Result := 'StrConcat';
        1: Result := 'StrLen';
        2: Result := 'StrLeft';
        3: Result := 'StrRight';
        4: Result := 'StrMid';
        5: Result := 'StrAsc';
        6: Result := 'StrChr';
        7: Result := 'StrStr';
        8: Result := 'StrVal';
        9: Result := 'StrHex';
        10: Result := 'StrInstr';
      else
        Result := Format('String_%d', [SubOp]);
      end;
    2: // Math
      case SubOp of
        0: Result := 'MathSin';
        1: Result := 'MathCos';
        2: Result := 'MathTan';
        3: Result := 'MathAtn';
        4: Result := 'MathLog';
        5: Result := 'MathExp';
        6: Result := 'MathSqr';
        7: Result := 'MathAbs';
        8: Result := 'MathSgn';
        9: Result := 'MathInt';
        10: Result := 'MathRnd';
      else
        Result := Format('Math_%d', [SubOp]);
      end;
    3: // Array
      case SubOp of
        0: Result := 'ArrayLoad';
        1: Result := 'ArrayStore';
        2: Result := 'ArrayDim';
        3: Result := 'ArrayLoadInt';
        4: Result := 'ArrayLoadFloat';
        5: Result := 'ArrayLoadString';
        6: Result := 'ArrayStoreInt';
        7: Result := 'ArrayStoreFloat';
        8: Result := 'ArrayStoreString';
      else
        Result := Format('Array_%d', [SubOp]);
      end;
    4: // I/O
      case SubOp of
        0: Result := 'Print';
        1: Result := 'PrintLn';
        2: Result := 'PrintString';
        3: Result := 'PrintStringLn';
        4: Result := 'PrintInt';
        5: Result := 'PrintIntLn';
        6: Result := 'PrintComma';
        7: Result := 'PrintSemicolon';
        8: Result := 'PrintTab';
        9: Result := 'PrintSpc';
        10: Result := 'PrintNewLine';
        11: Result := 'Input';
        12: Result := 'InputInt';
        13: Result := 'InputFloat';
        14: Result := 'InputString';
      else
        Result := Format('IO_%d', [SubOp]);
      end;
    5: // Special variables
      case SubOp of
        0: Result := 'LoadTI';
        1: Result := 'LoadTIS';
        2: Result := 'StoreTIS';
        3: Result := 'LoadDTS';
      else
        Result := Format('Special_%d', [SubOp]);
      end;
    6: // File I/O
      case SubOp of
        0: Result := 'Dopen';
        1: Result := 'Dclose';
        2: Result := 'Open';
        3: Result := 'Close';
        4: Result := 'GetFile';
        5: Result := 'InputFile';
        6: Result := 'PrintFile';
        7: Result := 'Cmd';
      else
        Result := Format('FileIO_%d', [SubOp]);
      end;
    10: // Graphics
      case SubOp of
        0: Result := 'GraphicRGBA';
        1: Result := 'GraphicSetMode';
        2: Result := 'GraphicBox';
        3: Result := 'GraphicCircle';
        4: Result := 'GraphicDraw';
        5: Result := 'GraphicLocate';
        6: Result := 'GraphicRdot';
        7: Result := 'GraphicGetMode';
      else
        Result := Format('Graphics_%d', [SubOp]);
      end;
    11: // Sound
      case SubOp of
        0: Result := 'SoundVol';
        1: Result := 'SoundSound';
        2: Result := 'SoundEnvelope';
        3: Result := 'SoundTempo';
        4: Result := 'SoundPlay';
        5: Result := 'SoundFilter';
      else
        Result := Format('Sound_%d', [SubOp]);
      end;
    200..255: // Superinstructions
      case SubOp of
        0: Result := 'BranchEqInt';
        1: Result := 'BranchNeInt';
        2: Result := 'BranchLtInt';
        3: Result := 'BranchGtInt';
        4: Result := 'BranchLeInt';
        5: Result := 'BranchGeInt';
        10: Result := 'BranchEqFloat';
        11: Result := 'BranchNeFloat';
        12: Result := 'BranchLtFloat';
        13: Result := 'BranchGtFloat';
        14: Result := 'BranchLeFloat';
        15: Result := 'BranchGeFloat';
        20: Result := 'AddIntTo';
        21: Result := 'SubIntTo';
        22: Result := 'MulIntTo';
        30: Result := 'AddFloatTo';
        31: Result := 'SubFloatTo';
        32: Result := 'MulFloatTo';
        33: Result := 'DivFloatTo';
        40: Result := 'AddIntConst';
        41: Result := 'SubIntConst';
        42: Result := 'MulIntConst';
        50: Result := 'AddFloatConst';
        51: Result := 'SubFloatConst';
        52: Result := 'MulFloatConst';
        53: Result := 'DivFloatConst';
        60: Result := 'BranchEqZeroInt';
        61: Result := 'BranchNeZeroInt';
        70: Result := 'BranchEqZeroFloat';
        71: Result := 'BranchNeZeroFloat';
        80: Result := 'ArrayStoreIntConst';
        81: Result := 'ArrayStoreFloatConst';
        82: Result := 'ArrayStoreStringConst';
      else
        Result := Format('Super_%d', [SubOp]);
      end;
  else
    Result := Format('Group%d_%d', [Group, SubOp]);
  end;
end;

function OpcodeToString(OpCode: Word): string;
begin
  // Legacy compatibility wrapper
  Result := BytecodeOpToString(OpCode);
end;

function MakeBytecodeInstruction(OpCode: TBytecodeOp; Dest, Src1, Src2: Word; Immediate: Int64): TBytecodeInstruction;
begin
  // Initialize entire record to zero to avoid garbage in padding bytes
  FillChar(Result, SizeOf(Result), 0);
  Result.OpCode := OpCode;
  Result.Dest := Dest;
  Result.Src1 := Src1;
  Result.Src2 := Src2;
  Result.Immediate := Immediate;
  Result.SourceLine := 0;  // Will be set by bytecode compiler from SSA instruction
end;

end.
