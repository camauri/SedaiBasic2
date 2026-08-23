unit SedaiOpcodeBanks;

{ Which BANK does an opcode use each of its operand fields for?

  This is the one place that answers that question. It used to be answered twice -- once by the
  register compactor's <Field>IsStringReg predicates, once by the scan in TBytecodeVM.LoadProgram
  that sizes the register banks -- and the two DISAGREED on 88 (opcode, field) pairs. A second copy
  of this knowledge is how copyprop-soundness-bug happened: a pass that ignores the bank rewrites a
  register in the wrong one, and the miscompile is silent.

  So: add an opcode here, and every consumer learns about it at once. The string bank is extracted
  first because it is the one two passes already needed; the int and float predicates still live in
  SedaiRegisterCompaction and should follow.

  Copyright (c) 2025 Maurizio Cammalleri
  Released under GNU GPL v3 }

{$mode objfpc}{$H+}
{$codepage UTF8}   // CP_UTF8 literals, like every other unit. A string that crosses a codepage
                   // boundary is converted and a comparison stops being a memcmp - see SedaiFileIO.

interface

uses
  SedaiBytecodeTypes;

{ The three register banks, plus the honest answer when no list claims the field. rbUnknown is
  never equal to anything, so a consumer that reaches it declines instead of guessing. }
type
  TRegBank = (rbUnknown, rbInt, rbFloat, rbString);

{ Which bank does this opcode's Dest name? Answers for a Dest it WRITES and for a Dest it reads
  back (ArrayStore, BigInt) alike - the question is which bank the number indexes, not who reads. }
function BankOfDest(OpCode: TBytecodeOp): TRegBank;

{ Does this opcode use the named field as an INT or a FLOAT register index?

  These moved here from SedaiRegisterCompaction, which is where they were the ONLY copy - the
  compactor now delegates, exactly as it already did for the string family. What made the move
  necessary: the superinstruction liveness scan asked "is my register redefined here?" by comparing
  register NUMBERS, so `LoadConstString R1` was read as a redefinition of the FLOAT R1 that was
  still live, and the fusion deleted the load that produced it (test_division_bug, a spurious
  Division by zero). A register number without its bank does not name a register.

  Only the fields that scan needs came across (Dest, Dest-read, Immediate); Src1/Src2 int-float are
  still the compactor's, and should follow when something else needs them. }
function Src1IsIntReg(OpCode: TBytecodeOp): Boolean;
function Src1IsFloatReg(OpCode: TBytecodeOp): Boolean;
function Src2IsIntReg(OpCode: TBytecodeOp): Boolean;
function Src2IsFloatReg(OpCode: TBytecodeOp): Boolean;

{ Which bank does this opcode's Src1 / Src2 name? rbUnknown when no list claims it - and a scan
  asking "does this READ my register?" must treat rbUnknown as a yes, not a no. }
function BankOfSrc1(OpCode: TBytecodeOp): TRegBank;
function BankOfSrc2(OpCode: TBytecodeOp): TRegBank;

function DestIsIntReg(OpCode: TBytecodeOp): Boolean;            // writes it
function DestIsFloatReg(OpCode: TBytecodeOp): Boolean;          // writes it
function DestReadIsIntReg(OpCode: TBytecodeOp): Boolean;        // ...and reads it back (ArrayStore)
function DestReadIsFloatReg(OpCode: TBytecodeOp): Boolean;      // ...and reads it back
function ImmediateIsFloatReg(OpCode: TBytecodeOp): Boolean;

{ Is this opcode's Src1 an ARRAY ID rather than a register index?

  A consumer asking "does anything else read register R?" has to compare R against every field that
  can name a register - and Src1 of an element access names an ARRAY, so comparing it against a
  register number produces a phantom read. In n-body that phantom cost two instructions per
  iteration in the hottest loop: `ArrayLoadFloat R17, ARR[17], R29` made the INTEGER R17 of the
  loop's compare-and-branch look read, the head kept its CmpInt+JumpIfZero, and with no BranchGtInt
  in the head the loop TAIL could not fuse either.

  ⛔ The polarity here is the dangerous one, so the list is derived from the IMPLEMENTATION and not
  from the absence of a claim elsewhere: every opcode below indexes FArrays with Src1 in its own arm
  (RunTemplate.inc / ExecuteInstruction). Being INCOMPLETE is safe - it costs a fusion. Being WRONG
  about one entry would delete a live definition. The fused array superinstructions index FArrays
  with Src1 too and are claimed here only where their arm was read: bcArrayShiftLeft and
  bcArrayReverseRange resolve the array through a local (ArrIdxW) and are left out until that is
  traced. }
function Src1IsArrayId(OpCode: TBytecodeOp): Boolean;

{ Does this instruction READ float register Reg through its Immediate field? }
function ImmediateReadsFloatReg(const Instr: TBytecodeInstruction; Reg: Integer): Boolean;

{ Does this opcode use the named field as a STRING register index? }
function DestIsStringReg(OpCode: TBytecodeOp): Boolean;         // writes it
function DestReadIsStringReg(OpCode: TBytecodeOp): Boolean;     // ...and reads it back (ArrayStore)
function Src1IsStringReg(OpCode: TBytecodeOp): Boolean;
function Src2IsStringReg(OpCode: TBytecodeOp): Boolean;
function ImmediateIsStringReg(OpCode: TBytecodeOp): Boolean;

{ Does this instruction READ string register Reg in any of its fields? }
function ReadsStringReg(const Instr: TBytecodeInstruction; Reg: Integer): Boolean;

{ Does this instruction read INT register Reg through its Immediate field?

  Immediate is an Int64 that most opcodes use as a plain constant, and some use to carry one or more
  16-bit REGISTER INDEXES. Telling the two apart needs the opcode: "LoadConstInt R9, 12" has
  Immediate = 12 and reads no register at all, so a consumer that treated Immediate as a register
  index unconditionally would see phantom uses everywhere.

  ⚠️ The list below is the one the register compactor already relies on to remap these fields. If it
  were incomplete the compactor would itself be broken - it would renumber a register whose use it
  cannot see - so trusting it here is exactly as safe as register compaction already is. Keep the two
  in step: an opcode that starts packing a register into Immediate must be added in BOTH places until
  the compactor's own copies move here. }
function OpCarriesJumpTarget(OpCode: Word): Boolean;

function ImmediateReadsIntReg(const Instr: TBytecodeInstruction; Reg: Integer): Boolean;

implementation

function Src1IsArrayId(OpCode: TBytecodeOp): Boolean;
begin
  case OpCode of
    bcArrayLoadInt, bcArrayLoadFloat, bcArrayLoadString,
    bcArrayStoreInt, bcArrayStoreFloat, bcArrayStoreString,
    // The fused element accesses, read off their own arms in RunTemplate.inc the same way: each one
    // writes or reads FArrays[Instr.Src1]. bcArrayCopyElement takes its DESTINATION array from Dest
    // and its SOURCE array from Src1, so Src1 is an array id there too.
    bcArrayStoreIntConst, bcArrayStoreFloatConst, bcArrayStoreStringConst,
    bcArrayLoadAddFloat, bcArrayLoadSubFloat, bcArrayLoadDivAddFloat,
    bcArrayLoadIntBranchNZ, bcArrayLoadIntBranchZ,
    bcArraySwapInt, bcArrayLoadIntTo, bcArrayCopyElement,
    bcStrMidAssignArr:   // MID$ into an element: Src1 = the array, Src2 = the linear index
      Result := True;
  else
    Result := False;
  end;
end;

function Src1IsIntReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // SUB/FUNCTION transfer-register store (M2): Src1 is the int register read.
    bcXferStoreInt,
    // === GROUP 12: BigInt === Src1 is an int register in all three: the Int64 value for
    // FromInt, the SOURCE handle for Copy and for ToStr.
    bcBigFromInt, bcBigCopy, bcBigToStr,
    bcBigAdd, bcBigSub, bcBigMul, bcBigCmp, bcBigMulSmall, bcBigDiv, bcBigMod, bcBigToInt,   // Src1 = the left handle
    // UDT/record (M3): Src1 is the record HANDLE (always an int register) for all field ops.
    bcRecordLoadInt, bcRecordLoadFloat, bcRecordLoadString,
    bcRecordStoreInt, bcRecordStoreFloat, bcRecordStoreString,
    bcRecordTypeId,   // OOP (M4.3): Src1 = handle
    bcRecordSetTypeId, // OOP: Src1 = handle, Immediate = type id (NOT a register)
    bcRecordFree,     // DELETE: Src1 = handle
    bcRecordNewArrayInd,  // array-of-UDT member alloc: Src1 = member array-handle reg (int)
    bcRecordNewBlock,     // Callocate block: Src1 = count reg (int)
    // OS threading (M5.2): ThreadCreate Src1 = proc-addr reg; ThreadWait Src1 = handle reg.
    // (bcLoadProcAddr's Src1 is the entry-PC label → Immediate, not a register, so it is excluded.)
    bcThreadCreate, bcThreadWait, bcThreadDetach,
    // FreeBASIC function pointer call: Src1 = int register holding the target entry PC.
    bcCallSubIndirect,
    // Mutexes (M5.4): Lock/Unlock/Destroy Src1 = mutex handle reg.
    bcMutexLock, bcMutexUnlock, bcMutexDestroy,
    // Condition variables (M5.4): Wait/Signal/Broadcast/Destroy Src1 = cond handle reg.
    bcCondWait, bcCondSignal, bcCondBroadcast, bcCondDestroy,
    // RANDOMIZE: Src1 = seed reg (Immediate flags seed vs time-based).
    bcRandomize,

    // === GROUP 0: Core VM operations ===
    // Int arithmetic
    bcCopyInt, bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt, bcNegInt,
    bcDivUInt, bcModUInt,
    // Int comparisons
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpLeInt, bcCmpGtInt, bcCmpGeInt,
    bcCmpLtUInt, bcCmpLeUInt, bcCmpGtUInt, bcCmpGeUInt,
    // Conversion from int
    bcIntToFloat, bcBitsToSingle, bcIntToString,
    bcNarrowInt,   // B1.5: integer width narrowing (Src1=int)
    // Branch on int (comparison result)
    bcJumpIfZero, bcJumpIfNotZero,
    // Error handling - RESUME <line> reads line number from Src1
    bcResume,
    // ERROR <n> reads the user error number from Src1
    bcRaiseError,
    // Bitwise operations
    bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor, bcBitwiseNot, bcShl, bcShr, bcShrUInt,
    bcBitClz, bcBitCtz, bcBitPopcnt, bcBitRotl, bcBitRotr,   // Src1 = the value operated on
    // === GROUP 3: Pointer deref (FreeBASIC): Src1 = address register (always int) ===
    bcRefLoadInt, bcRefLoadFloat, bcRefLoadString,
    bcRefStoreInt, bcRefStoreFloat, bcRefStoreString,
    bcRefAddrField,  // @obj.field - Src1 = record handle (int)
    // raw heap: Src1 = byte count (alloc) / raw pointer (free/realloc/load/store) — all int
    bcRawAlloc, bcRawFree, bcRawRealloc,
    bcRawLoadInt, bcRawLoadFloat, bcRawStoreInt, bcRawStoreFloat,
    bcRawLoadZStr, bcRawStoreZStr,   // C-string view: Src1 = raw pointer (int)
    // FB_MEMCOPY/FB_MEMMOVE/CLEAR: Src1 = destination raw pointer (int)
    bcRawMemCopy, bcRawMemMove, bcRawClear,
    // === GROUP 1: String operations with int param ===
    bcStrChr, bcStrHex, bcStrErr, bcStrSpace, bcStrOct, bcStrBin, bcStrWChr,
    bcWInputChars, bcInputChars,    // W/INPUT(n[,#f]): Src1 = count (int)
    bcStrMkInt,  // MKI/MKL/MKSHORT/MKLONGINT(n) - Src1 = int value to pack (B3)
    bcStrString, bcStrWStringN,  // STRING/WSTRING(n,ch) - Src1 = count (int)
    bcCommand,  // COMMAND$(index) - Src1 = index (int)
    // === GROUP 5: Memory operations ===
    bcPeek,           // PEEK(address): Src1 = address (int)
    bcPoke,           // POKE address, value: Src1 = address (int)
    // === GROUP 4: I/O operations ===
    bcPrintInt, bcPrintIntLn, bcPrintBool, bcPrintUInt,
    bcPrintTab, bcPrintSpc,  // TAB(n) and SPC(n) - Src1 = count register
    bcConScreen,      // SCREEN(row,col[,flag]): Src1 = row (int)
    bcConLocate,      // MODERN LOCATE row, col: Src1 = row (int)
    bcConViewPrint,   // VIEW PRINT first TO last: Src1 = first row (int)
    // === GROUP 6: Sound operations ===
    bcSoundVol,       // Src1 = volume (int 0-15)
    bcSoundSound,     // Src1 = voice number (int)
    bcSoundEnvelope,  // Src1 = envelope slot (int 0-9)
    bcSoundTempo,     // Src1 = tempo value (int)
    // === GROUP 10: Graphics ===
    bcGraphicBox, bcGraphicSetMode, bcGraphicRGBA, bcGraphicRdot, bcGraphicGetMode,
    bcGraphicWindow,  // Src1 = col1 register (int)
    bcGraphicCircle,  // Src1 = color register (int)
    bcGraphicPaint,   // Src1 = source register (int)
    bcGfxScreenRes, bcGfxPset, bcGfxPoint, bcGfxPaint, bcGfxPaintBorder, bcGfxLine, bcGfxLineStyled, bcGfxCircle, bcGfxCircleEx, bcGfxCircleExF, bcGfxImageConvertRow,
    bcGfxSetTarget,  // SETTARGET: Src1 = image handle (int)  // FreeBASIC graphics: Src1 = w / x / x1 (int)
    bcGfxPalette, bcGfxPalGet,  // PALETTE: Src1 = index (int)
    bcGfxColor,  // COLOR: Src1 = foreground (int)
    bcGfxImageCreate, bcGfxImageDestroy, bcGfxImageInfo,  // IMAGE*: Src1 = w / handle (int)
    bcGfxGet, bcGfxPut,  // GET/PUT: Src1 = x1 / x (int)
    bcGfxScreenSet, bcGfxPCopy,  // SCREENSET/PCOPY: Src1 = work / src page (int)
    bcGfxWindow, bcGfxView, bcGfxPMap,  // WINDOW/VIEW: Src1 = x1 ; PMAP: Src1 = coord (int)
    bcGfxScreen,  // SCREEN: Src1 = mode (int)
    bcMultikey,  // MULTIKEY: Src1 = scancode (int)
    bcSetmouse,  // SETMOUSE: Src1 = x (int)
    bcGetJoystick,  // GETJOYSTICK: Src1 = device id (int)
    bcStick, bcStrig,  // STICK/STRIG: Src1 = axis / button (int)
    bcGfxPointCoord,   // POINTCOORD(n): Src1 = selector (int)
    bcGraphicSShape,  // Src1 = x1 coordinate (int)
    bcGraphicColor,   // Src1 = source register (int)
    bcGraphicWidth,   // Src1 = width value (int)
    bcGraphicScale,   // Src1 = enable flag (int)
    bcGraphicRclr,    // Src1 = color source index (int)
    bcGraphicRwindow, // Src1 = info type (int)
    bcScnClr,         // Src1 = mode register (int)
    bcSetColor,       // Src1 = source register (int)
    bcGetColor,       // Src1 = source index (int)
    bcVarArgGetInt, bcVarArgGetFloat, bcVarArgGetStr,   // CVA_ARG: Src1 = the cursor (int)
    bcVarArgPushInt,   // staging a surplus argument: Src1 = the int value
    // === SUPERINSTRUCTIONS ===
    // Fused arithmetic-to-dest (Int): Src1 is int operand
    bcAddIntTo, bcSubIntTo, bcMulIntTo,
    // Fused constant arithmetic (Int): Src1 is source register
    bcAddIntConst, bcSubIntConst, bcMulIntConst,
    // Fused compare-and-branch (Int): Src1 is first comparison operand
    bcBranchEqInt, bcBranchNeInt, bcBranchLtInt, bcBranchGtInt, bcBranchLeInt, bcBranchGeInt,
    // ...and the unsigned forms read the same bank, differing only in the comparison.
    bcBranchLtUInt, bcBranchLeUInt, bcBranchGtUInt, bcBranchGeUInt,
    // Fused compare-zero-and-branch (Int): Src1 is the register being compared
    bcBranchEqZeroInt, bcBranchNeZeroInt,
    // Fused loop increment-and-branch (Int): Src1 = step register
    bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt,
    // Fused self-increment/decrement (Int): Src1 = step register
    bcAddIntSelf, bcSubIntSelf,
    // Fused array element operations: Src1 = src_idx_reg for MoveElement
    bcArrayMoveElement,
    // === GROUP 6: File I/O operations ===
    bcDopen, bcOpenFunc, bcDclose, bcOpen, bcClose,  // Src1 = handle (int)
    bcGetFile, bcInputFile, bcPrintFile,     // Src1 = handle (int)
    bcPrintFileComma,                        // PRINT# comma zone pad - Src1 = handle (int)
    bcPrintFileNewLine,                      // PRINT# newline - Src1 = handle (int); was missing from the classifier
    bcInputFileFloat, bcInputFileInt,        // Src1 = handle (int)
    bcPrintFileFloat, bcPrintFileInt,        // Src1 = handle (int)
    bcFileQuery, bcFileAttr, bcFileSetEof, bcSeekSet, bcInputFileLine, // Src1 = handle (int)
    bcAssert,        // ASSERT/ASSERTWARN: Src1 = condition (int)
    bcGetBinInt, bcGetBinFloat, bcPutBinInt, bcPutBinFloat,  // Src1 = handle (int)
    bcGetBinStr, bcPutBinStr,                                // Src1 = handle (int)
    // Counted/whole-array/padding binary transfers: Src1 = handle (int)
    bcPutBinMem, bcGetBinMem, bcPutBinArray, bcGetBinArray, bcPutBinPad, bcGetBinSkip,
    bcArrayRedimPush,                        // REDIM multi-dim: Src1 = upper bound (int)
    bcArrayIdxPush,                          // runtime multi-dim index: Src1 = index (int)
    // UDT array members (indirect): Src1 = the FArrays HANDLE register (load/store/idx-resolve),
    // or the record-HANDLE register (member REDIM) — always int.
    bcArrayLoadIndInt, bcArrayLoadIndFloat, bcArrayLoadIndString,
    bcArrayStoreIndInt, bcArrayStoreIndFloat, bcArrayStoreIndString,
    bcArrayIdxResolveInd, bcMemberArrayRedim,
    bcArrayLBoundInd, bcArrayUBoundInd,   // Src1 = FArrays handle (int)
    bcArrayCopyContents, bcArrayCopyRecords,  // Src1 = dest FArrays handle (int)
    // Date/time: DATESERIAL/TIMESERIAL Src1 = year/hour (int); MONTHNAME/WEEKDAYNAME Src1 = index (int)
    bcDateSerial, bcTimeSerial, bcDateName,
    bcCmd, bcAppend, bcRecord:               // Src1 = handle (int)
      Result := True;
  else
    Result := False;
  end;
end;
function Src1IsFloatReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // SUB/FUNCTION transfer-register store (M2): Src1 is the float register read.
    bcXferStoreFloat,
    bcVarArgPushFloat,   // staging a surplus argument: Src1 = the float value
    // === GROUP 0: Core VM operations ===
    // Float arithmetic
    bcCopyFloat, bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat, bcModFloat, bcNegFloat, bcPowFloat,
    // Float comparisons
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpLeFloat, bcCmpGtFloat, bcCmpGeFloat,
    // Conversion from float
    bcFloatToInt, bcFloatToString, bcFloatRound,
    bcNarrowSingle,  // B1.5: single-precision rounding (Src1=float)
    // === GROUP 2: Math functions ===
    bcMathSqr, bcMathSin, bcMathCos, bcMathTan, bcMathAtn,
    bcMathExp, bcMathLog, bcMathAbs, bcMathSgn, bcMathInt, bcMathRnd,
    bcMathLog10, bcMathLog2, bcMathLogN,
    bcMathAcos, bcMathAsin, bcMathAtan2, bcMathFix, bcMathFrac,  // FreeBASIC math
    bcMathSinh, bcMathCosh, bcMathTanh, bcMathAsinh, bcMathAcosh, bcMathAtanh,  // hyperbolic
    bcMathCeil, bcMathRound, bcMathMin, bcMathMax, bcMathCopySign,               // IEEE extras
    bcSingleBits,     // ...and this one READS a float; BITSTOSINGLE reads an int
    bcDateDecode,  // YEAR/MONTH/DAY/HOUR/MINUTE/SECOND/WEEKDAY: Src1 = float serial
    // === GROUP 1: String operations with float param ===
    bcStrStr,      // STR$(n) - reads float, produces string
    bcStrMkFloat,  // MKS/MKD(n) - reads float, produces binary string (B3)
    // === GROUP 4: I/O operations ===
    // Print float value (bcPrint/bcPrintLn use float register in Src1)
    bcPrint, bcPrintLn,
    // === SUPERINSTRUCTIONS ===
    // Fused arithmetic-to-dest (Float): Src1 is float operand
    bcAddFloatTo, bcSubFloatTo, bcMulFloatTo, bcDivFloatTo,
    // Fused constant arithmetic (Float): Src1 is source register
    bcAddFloatConst, bcSubFloatConst, bcMulFloatConst, bcDivFloatConst,
    // Fused compare-and-branch (Float): Src1 is first comparison operand
    bcBranchEqFloat, bcBranchNeFloat, bcBranchLtFloat, bcBranchGtFloat, bcBranchLeFloat, bcBranchGeFloat,
    // Fused compare-zero-and-branch (Float): Src1 is the register being compared
    bcBranchEqZeroFloat, bcBranchNeZeroFloat,
    // Fused Multiply-Add (FMA): Src1 is 'a' in (a * b)
    bcMulAddFloat, bcMulSubFloat, bcMulAddToFloat, bcMulSubToFloat,
    // Fused Square-Sum: Src1 is 'x' or 'sum'
    bcSquareSumFloat, bcAddSquareFloat, bcMulMulFloat, bcAddSqrtFloat,
    // === GROUP 11: Sound ===
    bcSoundFilter,  // Src1 = cutoff frequency (float)
    // === GROUP 7: Sprite ===
    bcSpriteDef,    // SPRDEF [n]: Src1 = sprite number (float)
    bcSprSize,      // SPRSIZE: Src1 = sprite number (float)
    bcSprForm:      // SPRFORM: Src1 = sprite number (float)
      Result := True;
  else
    Result := False;
  end;
end;
function Src2IsIntReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 12: BigInt === Src2 is the RIGHT operand's handle (an int register).
    bcBigAdd, bcBigSub, bcBigMul, bcBigCmp, bcBigMulSmall, bcBigDiv, bcBigMod,
    // UDT/record (M3): RecordStoreInt's Src2 is the int value being written.
    bcRecordStoreInt,
    // raw heap: Realloc's Src2 = byte count; RawStoreInt's Src2 = int value.
    bcRawRealloc, bcRawStoreInt,
    // FB_MEMCOPY/FB_MEMMOVE: Src2 = source pointer; CLEAR: Src2 = byte value — all int.
    bcRawMemCopy, bcRawMemMove, bcRawClear,
    // Counted file<->raw-memory transfer: Src2 = raw pointer (int). NOTE bcPut/GetBinArray are
    // deliberately NOT here: their Src2 is an array ID immediate, not a register.
    bcPutBinMem, bcGetBinMem,
    // Condition variables (M5.4): CondWait's Src2 is the mutex handle (int).
    bcCondWait,
    // === GROUP 0: Core VM operations ===
    // Int arithmetic (second operand)
    bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt, bcDivUInt, bcModUInt,
    // Int comparisons (second operand)
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpLeInt, bcCmpGtInt, bcCmpGeInt,
    bcCmpLtUInt, bcCmpLeUInt, bcCmpGtUInt, bcCmpGeUInt,
    // Bitwise operations (second operand); shifts: Src2 = shift count
    bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor, bcShl, bcShr, bcShrUInt,
    bcBitRotl, bcBitRotr,   // rotates: Src2 = the rotate count (the counting forms have no Src2)
    // HEX$/OCT/BIN(n, digits): Src2 = the digits width (int; 0 = natural length)
    bcStrHex, bcStrOct, bcStrBin,
    // === GROUP 5: Memory operations ===
    bcPoke,           // POKE address, value: Src2 = value (int)
    bcPrintUsingInt,  // PRINT USING (exact int): Src2 = int value (Src1 = format string)
    // === GROUP 6: File I/O ===
    bcWInputChars, bcInputChars,    // W/INPUT(n[,#f]): Src2 = file handle (int; 0 = keyboard)
    bcSeekSet,        // SEEK #n, pos: Src2 = position (int)
    bcDirSearch,      // DIR(spec, mask): Src2 = the attribute mask (int)
    bcScratch,        // SCRATCH "pattern", flags: Src2 = flags (int; bit0 silent, bit1 force)
    bcFileAttr,       // FILEATTR(filenum, returntype): Src2 = returntype (int)
    bcPutBinInt,      // PUT #n: Src2 = int value
    // === GROUP 1: String operations with int second param ===
    bcStrLeft, bcStrRight,  // LEFT$/RIGHT$(str, len) - len is Src2 (int)
    bcStrLeftW, bcStrRightW, bcStrMidW,  // WSTRING: Src2 = codepoint count/start (int)
    bcStrMid,  // Mid$(str, start, length) - start is Src1, length is Src2
    bcStrAscMid,  // ASC(MID$(...)) fused: Src2 = start position (int), like bcStrMid
    bcStrString, bcStrWStringN,  // STRING/WSTRING(n,ch) - Src2 = char code/codepoint (int)
    // === GROUP 2: Date/time: DATESERIAL/TIMESERIAL Src2 = month/minute (int); DATEADD Src2 = number ===
    bcDateSerial, bcTimeSerial, bcDateAdd,
    // === GROUP 3: Typed array operations: Src2 is always int (linear index) ===
    bcArrayLoadInt, bcArrayLoadFloat, bcArrayLoadString,
    bcArrayStoreInt, bcArrayStoreFloat, bcArrayStoreString,
    bcStrMidAssignArr,   // MID$ into an element: Src2 = the linear index, same as the stores above
    // UDT array members (indirect): Src2 = the linear index register (int)
    bcArrayLoadIndInt, bcArrayLoadIndFloat, bcArrayLoadIndString,
    bcArrayStoreIndInt, bcArrayStoreIndFloat, bcArrayStoreIndString,
    bcArrayLBoundInd, bcArrayUBoundInd,  // Src2 = 0-based dim index (int)
    bcArrayCopyContents, bcArrayCopyRecords,  // Src2 = source FArrays handle (int)
    bcArrayBindInd,  // Src2 = the arg member array's runtime FArrays handle (int); Src1 is an array id, NOT a register
    bcArrayLBound, bcArrayUBound,  // B1.4: Src2 = 0-based dim index (int)
    bcArrayRedim,  // B1.4: REDIM - Src2 = new upper bound (int)
    bcRefStoreInt,  // FreeBASIC pointer store (int) - Src2 = value (int)
    bcConScreen,    // SCREEN(row,col[,flag]): Src2 = column (int)
    bcConLocate,    // MODERN LOCATE row, col: Src2 = column (int)
    bcConViewPrint, // VIEW PRINT first TO last: Src2 = last row (int)
    // === GROUP 10: Graphics ===
    bcGraphicBox, bcGraphicSetMode, bcGraphicRGBA,
    bcGraphicWindow,  // Src2 = row1 register (int)
    bcGraphicCircle,  // Src2 = x register (int)
    bcGfxScreenRes, bcGfxPset, bcGfxPoint, bcGfxPaint, bcGfxPaintBorder, bcGfxLine, bcGfxLineStyled, bcGfxCircle, bcGfxCircleEx, bcGfxCircleExF, bcGfxImageConvertRow,  // FreeBASIC graphics: Src2 = h / y / y1 (int)
    bcGfxDrawString,  // DRAW STRING: Src2 = x (int); Src1 is the TEXT and is classified as a string
    bcGfxPalette,  // PALETTE set: Src2 = packed colour (int)
    bcGfxColor,  // COLOR: Src2 = background (int)
    bcGfxImageCreate,  // IMAGECREATE: Src2 = h (int)
    bcGfxGet, bcGfxPut,  // GET/PUT: Src2 = y1 / y (int)
    bcGfxScreenSet, bcGfxPCopy,  // SCREENSET/PCOPY: Src2 = visible / dst page (int)
    bcGfxWindow, bcGfxView,  // WINDOW/VIEW: Src2 = y1 (int)
    bcSetmouse,  // SETMOUSE: Src2 = y (int)
    bcGraphicScale,   // Src2 = xmax register (int)
    bcGraphicColor,   // Src2 = color value (int)
    bcGraphicPaint,   // Src2 = x coordinate (int)
    bcGraphicSShape,  // Src2 = y1 coordinate (int)
    bcGraphicGShape,  // Src2 = x coordinate (int)
    bcSetColor,       // Src2 = color value (int)
    // === SUPERINSTRUCTIONS ===
    // Fused compare-and-branch (Int): Src2 is second comparison operand
    bcBranchEqInt, bcBranchNeInt, bcBranchLtInt, bcBranchGtInt, bcBranchLeInt, bcBranchGeInt,
    bcBranchLtUInt, bcBranchLeUInt, bcBranchGtUInt, bcBranchGeUInt,
    // Fused array-store-constant: Src2 is the int index register
    bcArrayStoreIntConst, bcArrayStoreFloatConst, bcArrayStoreStringConst,
    // Fused loop increment-and-branch (Int): Src2 = limit register
    bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt,
    // Fused ArrayLoad + Arithmetic: Src2 is the int index register
    bcArrayLoadAddFloat, bcArrayLoadSubFloat, bcArrayLoadDivAddFloat,
    // Fused ArrayLoad + Branch: Src2 is the int index register
    bcArrayLoadIntBranchNZ, bcArrayLoadIntBranchZ,
    // Fused array element operations: Src2 = idx_reg for swap/copy/move, or start_idx for shift
    bcArraySwapInt, bcArrayCopyElement, bcArrayMoveElement, bcArrayLoadIntTo,
    bcArrayShiftLeft, bcArrayReverseRange,
    // === GROUP 11: Sound ===
    bcSoundFilter,  // Src2 = lowpass (int 0/1)
    bcSoundSound,   // Src2 = frequency (int)
    // === GROUP 7: Sprite ===
    bcSprLoadFile,  // SPRLOAD: Src2 = usefilecolors flag (int)
    // === GROUP 6: File I/O operations ===
    bcRecord:       // Src2 = position (int)
      Result := True;
  else
    Result := False;
  end;
end;
function Src2IsFloatReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // UDT/record (M3): RecordStoreFloat's Src2 is the float value being written.
    bcRecordStoreFloat,
    // raw heap: RawStoreFloat's Src2 is the float value being written.
    bcRawStoreFloat,
    // === GROUP 0: Core VM operations ===
    // Float arithmetic (second operand)
    bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat, bcModFloat, bcPowFloat,
    // Float comparisons (second operand)
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpLeFloat, bcCmpGtFloat, bcCmpGeFloat,
    // === SUPERINSTRUCTIONS ===
    // Fused compare-and-branch (Float): Src2 is second comparison operand
    bcBranchEqFloat, bcBranchNeFloat, bcBranchLtFloat, bcBranchGtFloat, bcBranchLeFloat, bcBranchGeFloat,
    // Fused Multiply-Add (FMA): Src2 is 'b' in (a * b)
    bcMulAddFloat, bcMulSubFloat, bcMulAddToFloat, bcMulSubToFloat,
    // Fused Square-Sum: Src2 is 'y' or 'x' (square operand)
    bcSquareSumFloat, bcAddSquareFloat, bcMulMulFloat, bcAddSqrtFloat,
    // LOGN(base, x): Src2 is 'x' (the value); ATAN2(y, x): Src2 is 'x'
    // ⛔ bcMathMin/bcMathMax/bcMathCopySign were MISSING here, and their Src2 is a float operand -
    // Min(a, b) reads b, CopySign(x, y) takes its sign from y (see the arms in SedaiBytecodeVM).
    // Found by aot_validate on ieee_intrinsics when a fusion pass started trusting these lists to
    // prove a register is NOT read. The compactor survived the gap because it only has to renumber
    // consistently, and a field it never remaps is a field it never breaks; anything that reads
    // these lists as a statement about what an opcode TOUCHES needs them complete.
    bcMathMin, bcMathMax, bcMathCopySign,
    bcMathLogN, bcMathAtan2,
    // DATEDIFF(interval, s1, s2): Src2 = s1; DATEPART(interval, serial): Src2 = serial (float)
    bcDateDiff, bcDatePart,
    // === GROUP 3: Pointer store (FreeBASIC): Src2 = float value ===
    bcRefStoreFloat,
    // === GROUP 4: I/O operations ===
    bcPrintUsing,  // PRINT USING - Src2 = value (float)
    // === GROUP 6: File I/O ===
    bcPutBinFloat, // PUT #n: Src2 = float value
    // === GROUP 7: Sprite ===
    bcSprSize,     // SPRSIZE: Src2 = width (float)
    bcSprForm:     // SPRFORM: Src2 = format (float)
      Result := True;
  else
    Result := False;
  end;
end;

function BankOfSrc1(OpCode: TBytecodeOp): TRegBank;
begin
  if Src1IsIntReg(OpCode) then Result := rbInt
  else if Src1IsFloatReg(OpCode) then Result := rbFloat
  else if Src1IsStringReg(OpCode) then Result := rbString
  else Result := rbUnknown;
end;

function BankOfSrc2(OpCode: TBytecodeOp): TRegBank;
begin
  if Src2IsIntReg(OpCode) then Result := rbInt
  else if Src2IsFloatReg(OpCode) then Result := rbFloat
  else if Src2IsStringReg(OpCode) then Result := rbString
  else Result := rbUnknown;
end;

function BankOfDest(OpCode: TBytecodeOp): TRegBank;
begin
  if DestIsIntReg(OpCode) then Result := rbInt
  else if DestIsFloatReg(OpCode) then Result := rbFloat
  else if DestIsStringReg(OpCode) then Result := rbString
  else if DestReadIsIntReg(OpCode) then Result := rbInt
  else if DestReadIsFloatReg(OpCode) then Result := rbFloat
  else if DestReadIsStringReg(OpCode) then Result := rbString
  else Result := rbUnknown;
end;

function DestIsIntReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // SUB/FUNCTION transfer-register load (M2): Dest is the int register written.
    bcXferLoadInt,
    // UDT/record (M3): RecordNew writes the handle (int); RecordLoadInt writes an int field.
    bcRecordNew, bcRecordNewBlock, bcRecordLoadInt,
    // OOP (M4.3): RecordTypeId writes the runtime type-id (int).
    bcRecordTypeId,
    // OS threading (M5.2): LoadProcAddr writes an entry PC (int); ThreadCreate writes a thread handle (int).
    bcLoadProcAddr, bcThreadCreate,
    // M5.5: ThreadSelf writes the current thread handle (int).
    bcThreadSelf,
    // Mutexes (M5.4): MutexCreate writes a mutex handle (int).
    bcMutexCreate,
    // Condition variables (M5.4): CondCreate writes a cond-var handle (int).
    bcCondCreate,
    // === GROUP 0: Core VM operations ===
    // Integer operations
    bcLoadConstInt, bcCopyInt, bcAddInt, bcSubInt, bcMulInt, bcDivInt,
    bcModInt, bcNegInt, bcDivUInt, bcModUInt,
    // Comparison results (stored as int)
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpLeInt, bcCmpGtInt, bcCmpGeInt,
    bcCmpLtUInt, bcCmpLeUInt, bcCmpGtUInt, bcCmpGeUInt,
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpLeFloat, bcCmpGtFloat, bcCmpGeFloat,
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString,
    // Bitwise operations (result is int)
    bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor, bcBitwiseNot, bcShl, bcShr, bcShrUInt,
    // MODERN bit intrinsics (result is int). Their Immediate is the WIDTH, a plain constant, so they
    // deliberately stay out of the ImmediateIsXxxReg lists.
    bcBitClz, bcBitCtz, bcBitPopcnt, bcBitRotl, bcBitRotr,
    // Conversions to int
    bcFloatToInt, bcStringToInt, bcFloatRound,
    bcNarrowInt,   // B1.5: integer width narrowing (Dest=int)
    bcSingleBits,  // SINGLEBITS(x): the 32 bits of a binary32, in an INT register
    // === GROUP 1: String operations ===
    bcStrLen,      // String length returns int
    bcStrLenW,     // LEN(wstring) returns int codepoint count
    bcStrSAdd,     // SADD(s) returns int (raw pointer)
    bcFileExists,  // FILEEXISTS(path) returns int (-1/0)
    bcFileLen,     // FILELEN(path) returns int (size in bytes)
    bcStrAsc,      // ASC(str) returns int ASCII code
    bcStrAscMid,   // ASC(MID$(s,start,len)) fused - also an int destination
    bcStrInstr,    // INSTR(haystack, needle) returns int position
    bcStrInstrRev, // INSTRREV(str, sub) returns int position
    // Both "Any set" forms return an int position too. bcStrInstrRevAny was MISSING from this list: its
    // destination register was never marked as used, so the compactor was free to hand the same int
    // register to something else -- a miscompile waiting for a program that uses INSTRREV(..., Any ...).
    bcStrInstrRevAny, bcStrInstrAny,
    bcStrInstrW, bcStrInstrRevW,  // WSTRING INSTR/INSTRREV return int codepoint position
    bcStrValInt,   // VALINT/VALLNG/VALUINT(str) returns int (B1.3)
    bcRegexCount,  // REGEXCOUNT(s, pattern) returns an int count
    bcStrCvInt,    // CVI/CVL/CVSHORT/CVLONGINT(str) returns int (B3 serialization)
    // === GROUP 2: Date/time -> int ===
    bcDateDecode,  // YEAR/MONTH/DAY/HOUR/MINUTE/SECOND/WEEKDAY(serial) -> int
    bcIsDate,      // ISDATE(str) -> int bool
    bcDateDiff, bcDatePart,  // DATEDIFF/DATEPART -> int
    // === GROUP 3: Array operations ===
    bcArrayLoadInt,  // Typed array load (int) - Dest is WRITTEN
    bcArrayLBound, bcArrayUBound,  // B1.4: LBOUND/UBOUND - Dest = int bound
    bcRefLoadInt,    // FreeBASIC pointer deref (int) - Dest = value loaded
    bcRefAddrField,  // @obj.field - Dest = packed record-field pointer (int)
    bcRawAlloc, bcRawRealloc,  // raw heap: Dest = raw pointer (int)
    bcRawLoadInt,              // raw deref (int) - Dest = value
    bcRawMemCopy, bcRawMemMove,  // FB_MEMCOPY/FB_MEMMOVE: Dest = destination pointer returned (int)
    // === GROUP 4: I/O operations ===
    bcInputInt,      // Input int
    bcDataReadInt,   // Read next DATA value into int register
    // === GROUP 6: File I/O operations ===
    bcInputFileInt,  // INPUT# file, int var
    bcFileQuery,     // EOF/FREEFILE/LOF/LOC/SEEK -> int result
    bcFileAttr,      // FILEATTR(filenum, returntype) -> int result
    bcFileSetEof,    // FILESETEOF filenum -> int status result
    bcOpenFunc,      // Open(...) FUNCTION form -> int error code in Dest
    bcDirAttr,       // DIR: attributes of the entry just returned -> int Dest
    bcVarArgBase,    // CVA_START: the cursor at the frame's first argument -> int Dest
    bcVarArgGetInt,  // CVA_ARG of an integer type -> int Dest
    bcGetBinInt,     // GET #n binary -> int Dest
    bcArrayIdxResolve,  // runtime multi-dim index -> int Dest (linear index)
    bcArrayLoadIndInt,      // UDT array member load (int) - Dest is WRITTEN
    bcArrayIdxResolveInd,   // member multi-dim index -> int Dest (linear index)
    bcArrayLBoundInd, bcArrayUBoundInd,  // UDT array member LBOUND/UBOUND -> int Dest
    // === GROUP 5: Special variables ===
    bcLoadTI,         // TI: jiffies since start (int)
    bcLoadEL,         // EL: last error line number (int)
    bcLoadER,         // ER: last error code (int)
    bcLoadDS,         // DS: Commodore disk status code (int)
    bcLoadST,         // ST: Kernal I/O status byte (int)
    bcCsrlin,         // CSRLIN: current cursor row (int)
    bcFre,            // FRE: available memory (int)
    bcCpuCount,       // CPUCOUNT/CPUCORES: processors (int); the immediate is the KIND, not a register
    // === GROUP 12: BigInt === a VALUE is a handle, and a handle is an int register.
    bcBigNew,         // Dest = the fresh handle
    bcBigFromInt,     // Dest = the handle written (and READ: see DestReadIsIntReg)
    bcBigCopy,        // Dest = the destination handle (likewise)
    bcBigAdd, bcBigSub, bcBigMul, bcBigMulSmall, bcBigDiv, bcBigMod, bcBigToInt,   // Dest = the result handle (also read: reused if live)
    bcBigCmp,         // Dest = the -1/0/1 result, a PLAIN int and not a handle
    bcBigFromStr,     // Dest = the handle built from the text (also read: reused if live)
    bcGfxScreenPtr,   // SCREENPTR: Dest = raw pointer to the framebuffer (int); no register sources
    bcPeek,           // PEEK(address): read from memory (int)
    // === GROUP 7: Sprite functions ===
    bcBump,           // BUMP(n): collision bitmask (int)
    bcRspcolor,       // RSPCOLOR(n): multicolor value (int)
    bcRsprite,        // RSPRITE(sprite, attr): sprite attribute (int)
    bcConScreen,      // SCREEN(row,col[,flag]): Dest = char code or colour attribute (int)
    // === GROUP 10: Graphics ===
    bcGraphicRGBA,    // Dest = RGBA result (int)
    bcGfxPoint,       // POINT(x,y): Dest = pixel color (int)
    bcGfxPalGet,      // __PALGET(index,which): Dest = palette component (int)
    bcGfxForeColor,   // current draw foreground: Dest = colour (int)
    bcGfxImageCreate, // IMAGECREATE: Dest = image handle (int)
    bcGfxImageInfo,   // __IMGINFO: Dest = width/height (int)
    bcGfxScreenInfo,  // __SCRINFO: Dest = screen info field (int)
    bcGfxPMap,        // __PMAP: Dest = mapped coordinate (int)
    bcMultikey,       // MULTIKEY: Dest = -1/0 key state (int)
    bcGetmouse,       // GETMOUSE: Dest = status 0/1 (int)
    bcMouseAxis,      // __MOUSEAXIS: Dest = cached mouse component (int)
    bcGetJoystick,    // GETJOYSTICK: Dest = status 0/1 (int)
    bcJoyBtn,         // __JOYBTN: Dest = cached button bitmask (int)
    bcStick,          // STICK: Dest = axis position 1..200/0 (int)
    bcStrig,          // STRIG: Dest = button state -1/0 (int)
    bcGfxPointCoord,  // POINTCOORD(n): Dest = pen coordinate (int)
    bcGraphicRdot,    // Dest = pixel cursor info (int)
    bcGraphicGetMode, // Dest = current graphic mode (int)
    bcGraphicPos,     // POS(x): cursor column position (int)
    bcGraphicRclr,    // RCLR(n): color of source (int)
    bcGraphicRwindow, // RWINDOW(n): window info (int)
    bcGetColor,       // GETCOLOR(source): color value (int)
    // === SUPERINSTRUCTIONS ===
    // Fused arithmetic-to-dest (Int): Dest = Dest op Src1
    bcAddIntTo, bcSubIntTo, bcMulIntTo,
    // Fused constant arithmetic (Int): Dest = Src1 op Immediate
    bcAddIntConst, bcSubIntConst, bcMulIntConst,
    // Fused loop increment-and-branch (Int): Dest = counter register
    bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt,
    // Fused self-increment/decrement (Int): Dest = counter (R/W)
    bcAddIntSelf, bcSubIntSelf,
    // Fused array load to int: Dest = result
    bcArrayLoadIntTo,
    // DEC(hexstring) - result is int
    bcStrDec:
      Result := True;
    // NOTE: bcArrayStoreInt uses Dest as SOURCE (read), handled by DestReadIsIntReg
    // NOTE: bcGraphicBox uses Dest as SOURCE (y1 coordinate), handled by DestReadIsIntReg
    // NOTE: bcArraySwapInt uses Dest as idx2_reg (read), handled by DestReadIsIntReg
    // NOTE: bcArrayShiftLeft/ReverseRange use Dest as end_idx (read), handled by DestReadIsIntReg
  else
    Result := False;
  end;
end;

function DestIsFloatReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // SUB/FUNCTION transfer-register load (M2): Dest is the float register written.
    bcXferLoadFloat,
    // UDT/record (M3): RecordLoadFloat writes a float field into Dest.
    bcRecordLoadFloat,
    // === GROUP 0: Core VM operations ===
    bcLoadConstFloat, bcCopyFloat, bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat,
    bcModFloat, bcNegFloat, bcPowFloat,
    // Conversion to float
    bcIntToFloat, bcStringToFloat,
    bcNarrowSingle,  // B1.5: single-precision rounding (Dest=float)
    bcGetBinFloat,   // GET #n binary -> float Dest
    bcVarArgGetFloat,  // CVA_ARG of a float type -> float Dest
    // === GROUP 1: String operations ===
    bcStrVal,      // VAL(str) - string to float
    bcStrCvFloat,  // CVS/CVD(str) - binary string to float (B3 serialization)
    // === GROUP 2: Math functions ===
    bcMathSqr, bcMathSin, bcMathCos, bcMathTan, bcMathAtn,
    bcMathExp, bcMathLog, bcMathAbs, bcMathSgn, bcMathInt, bcMathRnd,
    bcMathLog10, bcMathLog2, bcMathLogN,
    bcMathAcos, bcMathAsin, bcMathAtan2, bcMathFix, bcMathFrac,  // FreeBASIC math
    bcMathSinh, bcMathCosh, bcMathTanh, bcMathAsinh, bcMathAcosh, bcMathAtanh,  // hyperbolic
    bcMathCeil, bcMathRound, bcMathMin, bcMathMax, bcMathCopySign,               // IEEE extras
    bcBitsToSingle,   // ⛔ the bit-casts are MIXED: this one writes a float, SINGLEBITS writes an INT
    // Date/time -> float (date serial = Double)
    bcDateNow, bcDateSerial, bcTimeSerial, bcDateValue, bcDateAdd,
    bcFileDateTime,  // FILEDATETIME(path): last-modified date serial (float Dest, string Src1)
    // === GROUP 3: Array operations ===
    bcArrayLoadFloat,  // Typed array load (float) - Dest is WRITTEN
    bcArrayLoadIndFloat,  // UDT array member load (float) - Dest is WRITTEN
    bcRefLoadFloat,    // FreeBASIC pointer deref (float) - Dest = value loaded
    bcRawLoadFloat,    // raw deref (float) - Dest = value loaded
    // === GROUP 4: I/O operations ===
    bcInputFloat,
    bcDataReadFloat,   // Read next DATA value into float register
    // === GROUP 6: File I/O operations ===
    bcInputFileFloat,  // INPUT# file, float var
    // === GROUP 7: Sprite functions ===
    bcRsppos,          // RSPPOS(sprite, attr): position/speed (float)
    // === GROUP 10: Graphics ===
    bcJoyAxis,         // __JOYAXIS: Dest = cached joystick axis value (float, -1..1 / -1000)
    // === SUPERINSTRUCTIONS ===
    // Fused arithmetic-to-dest (Float): Dest = Dest op Src1
    bcAddFloatTo, bcSubFloatTo, bcMulFloatTo, bcDivFloatTo,
    // Fused constant arithmetic (Float): Dest = Src1 op Immediate
    bcAddFloatConst, bcSubFloatConst, bcMulFloatConst, bcDivFloatConst,
    // Fused ArrayLoad + Arithmetic: Dest = acc op arr[idx]
    bcArrayLoadAddFloat, bcArrayLoadSubFloat, bcArrayLoadDivAddFloat,
    // Fused Multiply-Add (FMA): Dest = c op (a * b) or Dest op= a * b
    bcMulAddFloat, bcMulSubFloat, bcMulAddToFloat, bcMulSubToFloat,
    // Fused Square-Sum and Mul-Mul: Dest = x*x + y*y, dest = a*b*c, etc.
    bcSquareSumFloat, bcAddSquareFloat, bcMulMulFloat, bcAddSqrtFloat:
      Result := True;
    // NOTE: bcArrayStoreFloat uses Dest as SOURCE (read), handled by DestReadIsFloatReg
  else
    Result := False;
  end;
end;

function DestReadIsIntReg(OpCode: TBytecodeOp): Boolean;
begin
  { These opcodes use Dest as a SOURCE register (read, not write).
    This is critical for ArrayStore where Dest holds the VALUE to store. }
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 3: Array operations ===
    // === GROUP 12: BigInt === ⛔ Dest is READ as well as written: it carries the handle
    // to fill in, and an existing one is REUSED rather than reallocated. Miss this and the
    // compactor treats the register as dead on entry and may hand it to something else.
    bcBigFromInt, bcBigCopy, bcBigAdd, bcBigSub, bcBigMul, bcBigFromStr, bcBigMulSmall, bcBigDiv, bcBigMod,
    bcArrayStoreInt,  // Dest = value register (int) - READ, not written
    bcArrayStoreIndInt,  // UDT array member store (int): Dest = value register - READ, not written
    // === GROUP 10: Graphics ===
    bcGraphicBox,     // Dest = y1 register (int) - READ, not written
    bcGraphicWindow,  // Dest = col2 register (int) - READ, not written
    bcGraphicCircle,  // Dest = y register (int) - READ, not written
    bcGraphicScale,   // Dest = ymax register (int) - READ, not written
    bcGraphicPaint,   // Dest = y coordinate (int) - READ, not written
    bcGraphicGShape,  // Dest = y coordinate (int) - READ, not written
    bcSetColor,       // SETCOLOR: Dest = G component (int) - READ, not written
    // === GROUP 11: Sound ===
    bcSoundSound,     // Dest = duration register (int) - READ, not written
    // === SUPERINSTRUCTIONS ===
    // Array swap: Dest = idx2_reg (int) - READ
    bcArraySwapInt,
    // Array shift/reverse: Dest = end_idx_reg (int) - READ
    bcArrayShiftLeft, bcArrayReverseRange,
    // === GROUP 6: File I/O operations ===
    bcPrintFileInt:      // Dest = value register (int) - READ, not written
      Result := True;
  else
    Result := False;
  end;
end;

function DestReadIsFloatReg(OpCode: TBytecodeOp): Boolean;
begin
  { These opcodes use Dest as a SOURCE register (read, not write).
    This is critical for ArrayStore where Dest holds the VALUE to store. }
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 3: Array operations ===
    bcArrayStoreFloat,   // Dest = value register (float) - READ, not written
    bcArrayStoreIndFloat,  // UDT array member store (float): Dest = value register - READ, not written
    // === GROUP 6: File I/O operations ===
    bcPrintFileFloat,    // Dest = value register (float) - READ, not written
    // === GROUP 7: Sprite ===
    bcSprSize:           // SPRSIZE: Dest = height (float) - READ, not written
      Result := True;
  else
    Result := False;
  end;
end;

function ImmediateIsFloatReg(OpCode: TBytecodeOp): Boolean;
begin
  { These superinstructions store a FLOAT REGISTER INDEX in the Immediate field
    instead of a constant value. The Immediate field needs to be remapped
    during register compaction. }
  case OpCode of
    // Fused ArrayLoad + Arithmetic: Immediate is the accumulator float register
    bcArrayLoadAddFloat, bcArrayLoadSubFloat,
    // Fused Multiply-Add (FMA): Immediate is 'c' in (c op a*b) - the accumulator
    bcMulAddFloat, bcMulSubFloat,
    // Fused Mul-Mul: Immediate is 'c' in (a*b*c)
    bcMulMulFloat,
    // DATEADD: Immediate = serial (float reg); DATEDIFF: Immediate = s2 (float reg)
    bcDateAdd, bcDateDiff,
    // FORMAT(num, mask): Immediate = the value being formatted (float reg)
    bcStrFormat:
      Result := True;
  else
    Result := False;
  end;
end;

function ImmediateReadsFloatReg(const Instr: TBytecodeInstruction; Reg: Integer): Boolean;
// The float twin of ImmediateReadsIntReg: the fused multiply-add family carries its accumulator
// register in Immediate, so a scan that only looks at Src1/Src2 cannot see that read.
begin
  Result := ImmediateIsFloatReg(TBytecodeOp(Instr.OpCode)) and (Instr.Immediate = Reg);
end;


function DestIsStringReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // SUB/FUNCTION transfer-register load (M2): Dest is the string register written.
    bcXferLoadString,
    // UDT/record (M3): RecordLoadString writes a string field into Dest.
    bcRecordLoadString,
    // BigInt (group 12): the decimal text of a value.
    bcBigToStr,
    // === GROUP 0: Core VM operations ===
    bcLoadConstString, bcCopyString,
    bcIntToString, bcFloatToString,
    // === GROUP 1: String operations ===
    bcStrConcat, bcStrLeft, bcStrRight, bcStrMid, bcStrChr, bcStrWChr,
    bcStrConcatCharAt,  // "acc + tab[k]" fused (superinstruction group) - string dest
    bcStrAppendMapped,  // "acc += tab[Asc(s[i])+1]" fused - string dest (READ too: it appends)
    bcStrMidAssign,     // "Mid(t,start)=src" - string dest = t (READ too: it overwrites part of it)
    bcStrLeftW, bcStrRightW, bcStrMidW,  // WSTRING codepoint substrings - string dest
    bcStrLTrim, bcStrRTrim, bcStrTrim, bcStrUCase, bcStrLCase, bcStrSpace,  // B1.2: string dest
    bcCurDir, bcEnviron, bcExePath, bcCommand,  // CURDIR$ / ENVIRON$(name) / EXEPATH / COMMAND$(index) - string dest
    bcDirSearch,   // DIR(spec, mask) / DIR() - the matching entry's name, string dest
    bcVarArgGetStr,   // CVA_ARG of a string type - string dest
    bcStrFormat,  // FORMAT(num, mask) - string dest
    bcRegexReplace,  // REGEXREPLACE(s, pattern, repl) - string dest
    bcStrString, bcStrWStringN,  // STRING/WSTRING(n,ch) - string dest
    bcStrTrimSet, // LTRIM/RTRIM/TRIM(s,set) - string dest
    bcStrStr,    // STR$(n) - number to string
    bcStrMkInt, bcStrMkFloat,  // MK*(n) - number to binary string (B3 serialization)
    bcStrHex,    // HEX$(n) - int to hex string
    bcStrOct, bcStrBin,  // OCT(n)/BIN(n) - int to octal/binary string (B1.3)
    bcStrErr,    // ERR$(n) - error code to message string
    bcDateStr,   // DATE/TIME -> formatted string
    bcDateName,  // MONTHNAME/WEEKDAYNAME(n) -> string
    // === GROUP 3: Array operations ===
    bcArrayLoadString,  // Typed array load (string) - Dest is WRITTEN
    bcArrayLoadIndString,  // UDT array member load (string) - Dest is WRITTEN
    bcRefLoadString,    // FreeBASIC pointer deref (string) - Dest = value loaded
    bcRawLoadZStr,      // *p (ZSTRING/WSTRING PTR) - Dest = the C string read
    // === GROUP 4: I/O operations ===
    bcInputString,
    bcGet,             // GET A$ / INKEY$ - Dest = char read (string), "" if none
    bcGetkey,          // GETKEY A$ - Dest = char read (string, blocking)
    bcDataReadString,  // Read next DATA value into string register
    // === GROUP 5: Special variables ===
    bcLoadTIS,         // TI$: current time HHMMSS (string)
    bcLoadDTS,         // DT$: current date YYYYMMDD (string)
    bcLoadCWDS,        // CWD$: current working directory (string)
    bcLoadERRS,        // ERR$: last error message (string)
    bcLoadERFN,        // ERFN: procedure of the last error (string)
    bcLoadERMN,        // ERMN: module of the last error (string)
    bcWInputChars,     // WINPUT(n[,#f]): Dest = string; Src1/Src2 are int registers (see below)
    bcInputChars,      // INPUT(n[,#f]): idem
    bcLoadDSS,         // DS$: Commodore disk status message line (string)
    // === GROUP 10: Graphics ===
    bcGraphicSShape,   // SSHAPE A$, x1, y1: capture screen area to string
    // === GROUP 6: File I/O operations ===
    bcGetFile,         // GET# - Dest = char read (string)
    bcInputFile,       // INPUT# - Dest = line read (string)
    bcInputFileLine,   // LINE INPUT# - Dest = whole line read (string)
    bcGetBinStr:       // GET #n binary string - Dest = string read
      Result := True;
    // NOTE: bcArrayStoreString uses Dest as SOURCE (read), handled by DestReadIsStringReg
  else
    Result := False;
  end;
end;

function Src1IsStringReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // SUB/FUNCTION transfer-register store (M2): Src1 is the string register read.
    bcXferStoreString,
    // BigInt (group 12): BigFromStr READS the decimal text from Src1.
    bcBigFromStr,
    bcVarArgPushStr,     // staging a surplus argument: Src1 = the string value
    // === GROUP 0: Core VM operations ===
    bcCopyString,
    // String comparison (first operand)
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString,
    // === GROUP 1: String operations ===
    bcStrConcat, bcStrLeft, bcStrRight, bcStrMid, bcStrLen, bcStrLenW, bcStrAsc, bcStrAscMid, bcStrSAdd,
    bcStrConcatCharAt,  // "acc + tab[k]" fused: Src1 = the accumulator
    bcStrAppendMapped,  // "acc += tab[Asc(s[i])+1]" fused: Src1 = the SOURCE string
    bcStrMidAssign,     // "Mid(t,start)=src": Src1 = the incoming t
    bcFileExists, bcFileLen, bcFileDateTime,  // FILEEXISTS/FILELEN/FILEDATETIME(path): Src1 = path string
    bcStrLeftW, bcStrRightW, bcStrMidW,  // WSTRING: Src1 = source string
    bcStrLTrim, bcStrRTrim, bcStrTrim, bcStrUCase, bcStrLCase,  // B1.2: Src1 = source string
    bcEnviron,   // ENVIRON$(name) - Src1 = name string
    bcSetEnviron, // SETENVIRON "NAME=value" - Src1 = string
    bcShell,      // SHELL cmd - Src1 = command string
    // File management family - Src1 = path/pattern/source string. The whole family was
    // missing from these classifiers (registers neither marked used nor rewritten).
    bcLoad, bcSave, bcVerify, bcBload, bcBsave, bcCatalog,
    bcCopyFile, bcScratch, bcRenameFile, bcConcat, bcMkdir, bcChdir, bcRmdir, bcMoveFile,
    bcGfxDrawGML, // DRAW "..." - Src1 = GML string
    bcGfxDrawString, // DRAW STRING (x,y),text - Src1 = the TEXT (string); x/y/colour are int regs
    bcStrFormat, // FORMAT(num, mask) - Src1 = mask string
    bcDirSearch, // DIR(spec, mask) - Src1 = the file spec (string; unused by the CONTINUE form)

    bcStrVal,    // VAL(str) - reads string, produces float
    bcStrValInt, // VALINT/VALLNG/VALUINT(str) - reads string, produces int
    bcStrCvInt, bcStrCvFloat,  // CV*(str) - reads binary string, produces int/float (B3 serialization)
    bcStrInstr,  // INSTR(haystack, needle) - haystack is Src1
    bcStrInstrRev,  // INSTRREV(str, sub) - str is Src1
    bcStrInstrW, bcStrInstrRevW,  // WSTRING INSTR/INSTRREV - haystack is Src1
    bcStrInstrRevAny, // INSTRREV(str, Any set) - str is Src1
    bcStrInstrAny,    // INSTR(str, Any set) - str is Src1
    bcRegexCount, bcRegexReplace,   // REGEX*(s, pattern, ...) - the subject is Src1
    bcStrTrimSet,   // LTRIM/RTRIM/TRIM(s, set) - s is Src1
    // === GROUP 2: Math operations ===
    bcStrDec,  // DEC(hexstring) - reads string, produces int
    bcDateValue, // DATEVALUE/TIMEVALUE(str) - reads string, produces float serial
    bcIsDate,    // ISDATE(str) - reads string, produces int bool
    bcDateAdd, bcDateDiff, bcDatePart,  // Src1 = interval string
    bcSetClock,  // SETDATE/SETTIME str - Src1 = date/time string
    // === GROUP 4: I/O operations ===
    bcPrintString, bcPrintStringLn,
    bcPrintUsing,  // PRINT USING - Src1 = format string
    bcPrintUsingInt,  // PRINT USING (exact int) - Src1 = format string (Src2 = int value, the bank default)
    bcPrintUsingStage,  // stage a stringified value - Src1 = string
    bcPrintUsingRun,    // runtime PRINT USING - Src1 = format string
    // === GROUP 5: Special variables ===
    bcStoreTIS,  // TI$ = value - reads string from Src1
    // === GROUP 6: Sound operations ===
    bcSoundPlay,  // Src1 = music string
    // === GROUP 10: Graphics ===
    bcGraphicGShape,  // GSHAPE A$, x, y: A$ is string in Src1
    bcPLoad,          // PLOAD "filename": Src1 = filename string
    bcPSave,          // PSAVE "filename": Src1 = filename string
    // === GROUP 7: Sprite ===
    bcSprSaveFile,    // SPRSAVE "file": Src1 = filename string
    bcSprLoadFile,    // SPRLOAD "file": Src1 = filename string
    // Fused compare-and-branch (String): both operands index the string bank; the branch writes
    // nothing at all, so it appears in no Dest list.
    bcBranchEqString, bcBranchNeString, bcBranchLtString, bcBranchGtString,
    bcBranchLeString, bcBranchGeString:
      Result := True;
  else
    Result := False;
  end;
end;

function Src2IsStringReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // UDT/record (M3): RecordStoreString's Src2 is the string value being written.
    bcRecordStoreString,
    // === GROUP 0: Core VM operations ===
    // String comparison (second operand)
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString,
    // === GROUP 1: String operations ===
    bcStrConcat,  // String concatenation (second operand)
    bcStrConcatCharAt,  // "acc + tab[k]" fused: Src2 = the table the byte is taken from
    bcStrAppendMapped,  // "acc += tab[Asc(s[i])+1]" fused: Src2 = the mapping table
    bcStrMidAssign,     // "Mid(t,start)=src": Src2 = the replacement text
    bcStrInstr,   // INSTR(haystack, needle) - needle is Src2
    bcStrInstrRev, // INSTRREV(str, sub) - sub is Src2
    bcStrInstrW, bcStrInstrRevW,  // WSTRING INSTR/INSTRREV - needle is Src2
    bcStrInstrRevAny, // INSTRREV(str, Any set) - set is Src2
    bcStrInstrAny,    // INSTR(str, Any set) - set is Src2
    bcRegexCount, bcRegexReplace,   // REGEX*(s, pattern, ...) - the pattern is Src2
    bcStrTrimSet,  // LTRIM/RTRIM/TRIM(s, set) - set is Src2
    // === GROUP 3: Pointer store (FreeBASIC): Src2 = string value ===
    bcRefStoreString,
    // === GROUP 6: File I/O operations ===
    bcDopen, bcOpenFunc, bcOpen,  // Src2 = filename (string)
    bcAppend,         // Src2 = data (string)
    bcPutBinStr,      // PUT #n: Src2 = string value
    bcRawStoreZStr,   // *p = s (ZSTRING/WSTRING PTR) - Src2 = the string written
    // === GROUP 0: file management (two-path commands) - Src2 = destination/new-name string.
    // The whole family was missing from these classifiers: its registers were neither marked
    // used nor rewritten, so any non-identity map made the command read a stale index.
    bcCopyFile, bcRenameFile, bcConcat, bcMoveFile,
    // === GROUP 0: ASSERT/ASSERTWARN — Src2 = message (string) ===
    bcAssert,
    // Fused compare-and-branch (String): both operands index the string bank; the branch writes
    // nothing at all, so it appears in no Dest list.
    bcBranchEqString, bcBranchNeString, bcBranchLtString, bcBranchGtString,
    bcBranchLeString, bcBranchGeString:
      Result := True;
  else
    Result := False;
  end;
end;

function DestReadIsStringReg(OpCode: TBytecodeOp): Boolean;
begin
  { These opcodes use Dest as a SOURCE register (read, not write).
    This is critical for ArrayStore where Dest holds the VALUE to store. }
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // === GROUP 3: Array operations ===
    bcArrayStoreString,  // Dest = value register (string) - READ, not written
    bcArrayStoreIndString,  // UDT array member store (string): Dest = value register - READ, not written
    // === GROUP 6: File I/O operations ===
    bcPrintFile,         // Dest = data register (string) - READ, not written
    // === Superinstruction group ===
    // bcStrAppendMapped APPENDS to Dest, so the incoming accumulator is an INPUT as well as the
    // result. Leaving it out here lets every liveness that consults this list believe the value
    // arriving in Dest is dead, and the register carrying the accumulator gets reused: the reset
    // "acc = ''" then lands on one register while the append keeps growing another.
    // bcStrMidAssign overwrites PART of Dest, so the rest of the incoming value survives into the
    // result: like the append above, Dest is an input as well as the destination.
    // bcStrMidAssignArr never writes a register at all: its Dest is the REPLACEMENT text and the
    // result goes into the array element. Dest is purely an input.
    bcStrAppendMapped, bcStrMidAssign, bcStrMidAssignArr:
      Result := True;
  else
    Result := False;
  end;
end;

function OpCarriesJumpTarget(OpCode: Word): Boolean;
// Does this opcode's Immediate name an INSTRUCTION INDEX - a jump, call, handler entry or fused
// branch target? Three passes need the answer and each used to answer for itself:
//   SedaiNopCompaction  - to remap targets when instructions shift  (this list, the complete one)
//   SedaiPeephole       - to refuse a rewrite whose second half is a target
//   SedaiSuperinstructions - the same refusal, with an explicit case that named 14 fewer opcodes:
//                            the STRING and UNSIGNED compare-and-branch families, the two
//                            array-load-and-branch forms, and bcOnError / bcResumeLabel.
// A pass that cannot see a target may fuse ONTO it, and the jump then lands after the fused pair.
// Diffed and unified 20 Aug 2026; the peephole keeps its own extra blanket rule on top of this
// (every Group-super Immediate), which over-reports and is the safe direction for a refusal.
begin
  // Check base bytecode jump instructions
  case OpCode of
    Ord(bcJump), Ord(bcJumpIfZero), Ord(bcJumpIfNotZero), Ord(bcCall), Ord(bcCallSub),
    // M5.2: bcLoadProcAddr's Immediate is a SUB entry PC; remap it when instructions shift, like a call target.
    Ord(bcLoadProcAddr),
    // FreeBASIC error handling: bcOnError / bcResumeLabel Immediate is a handler/target PC; remap on shift.
    Ord(bcOnError), Ord(bcResumeLabel):
      Result := True;
    // Fused compare-and-branch (Int)
    bcBranchEqInt, bcBranchNeInt, bcBranchLtInt, bcBranchGtInt, bcBranchLeInt, bcBranchGeInt,
    bcBranchEqString, bcBranchNeString, bcBranchLtString, bcBranchGtString, bcBranchLeString, bcBranchGeString,
    bcBranchLtUInt, bcBranchLeUInt, bcBranchGtUInt, bcBranchGeUInt:
      Result := True;
    // Fused compare-and-branch (Float)
    bcBranchEqFloat, bcBranchNeFloat, bcBranchLtFloat, bcBranchGtFloat, bcBranchLeFloat, bcBranchGeFloat:
      Result := True;
    // Fused compare-zero-and-branch
    bcBranchEqZeroInt, bcBranchNeZeroInt, bcBranchEqZeroFloat, bcBranchNeZeroFloat:
      Result := True;
    // Fused loop increment-and-branch
    bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt:
      Result := True;
    // Array load and branch
    bcArrayLoadIntBranchNZ, bcArrayLoadIntBranchZ:
      Result := True;
  else
    Result := False;
  end;
end;

function ImmediateIsStringReg(OpCode: TBytecodeOp): Boolean;
begin
  { These opcodes store a STRING REGISTER INDEX in the Immediate field
    instead of a constant value. The Immediate field needs to be remapped
    during register compaction. }
  case OpCode of
    // DOPEN: Immediate = mode string register
    bcDopen, bcOpenFunc, bcOpen:
      Result := True;
    // REGEXREPLACE: Immediate = the REPLACEMENT string register (subject and pattern are Src1/Src2)
    bcRegexReplace:
      Result := True;
  else
    Result := False;
  end;
end;

function ReadsStringReg(const Instr: TBytecodeInstruction; Reg: Integer): Boolean;
// Every way an instruction can READ a string register. Dest counts only for the opcodes that read
// it back as well as write it (bcArrayStoreString carries the VALUE in Dest).
begin
  Result := (Src1IsStringReg(TBytecodeOp(Instr.OpCode)) and (Instr.Src1 = Reg))
         or (Src2IsStringReg(TBytecodeOp(Instr.OpCode)) and (Instr.Src2 = Reg))
         or (ImmediateIsStringReg(TBytecodeOp(Instr.OpCode)) and (Instr.Immediate = Reg))
         or (DestReadIsStringReg(TBytecodeOp(Instr.OpCode)) and (Instr.Dest = Reg));
end;

function ImmediateReadsIntReg(const Instr: TBytecodeInstruction; Reg: Integer): Boolean;
// The opcodes below are exactly those the register compactor remaps int registers inside Immediate
// for. Which BITS each one uses varies (16-bit halves for most, 12-bit fields for SETCOLOR and
// GRAPHICBOX, the whole value for GRAPHICSETMODE), and rather than restate every layout - a second
// copy that could drift from the compactor's - this tests EVERY position any of them uses.
//
// ⚠️ That deliberately OVER-reports: an unrelated field that happens to hold Reg's number answers
// True. The only consumer is a fusion pass asking "is this int register read anywhere else?", where
// a false yes costs a missed fusion and a false NO would cost a miscompile. Erring towards yes is
// the safe direction, and it makes this immune to a layout being transcribed wrong.
var
  Imm: Int64;
begin
  Result := False;
  case TBytecodeOp(Instr.OpCode) of
    bcStrMid, bcStrMidW, bcStrAscMid, bcStrConcatCharAt, bcStrAppendMapped, bcStrMidAssign,
    bcStrMidAssignArr,
    bcStrInstr, bcStrInstrAny,
    bcDateSerial, bcTimeSerial,
    bcRawMemCopy, bcRawMemMove, bcRawClear, bcPutBinMem, bcGetBinMem,
    bcConScreen, bcSetmouse, bcSetColor,
    bcCopyFile, bcRenameFile, bcShell,
    bcSoundEnvelope, bcSoundFilter, bcSoundSound,
    bcGfxPset, bcGfxPaint, bcGfxPaintBorder, bcGfxLine, bcGfxLineStyled,
    bcGfxCircle, bcGfxCircleEx, bcGfxCircleExF, bcGfxGet, bcGfxPut, bcGfxView, bcGfxWindow,
    bcGfxImageCreate, bcGfxImageConvertRow,
    bcGraphicBox, bcGraphicRGBA, bcGraphicSetMode:
      begin
        Imm := Instr.Immediate;
        if Imm < 0 then Exit;              // a negative Immediate is a flag, never a register index
        Result := (Imm = Reg)                                  // GRAPHICSETMODE: the whole value
               or ((Imm and $FFFF) = Reg) or (((Imm shr 16) and $FFFF) = Reg)
               or (((Imm shr 32) and $FFFF) = Reg) or (((Imm shr 48) and $FFFF) = Reg)
               or ((Imm and $FFF) = Reg) or (((Imm shr 12) and $FFF) = Reg)
               or (((Imm shr 36) and $FFF) = Reg);
      end;
  else
    Result := False;
  end;
end;

end.
