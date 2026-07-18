unit SedaiOpcodeTable;

{ ============================================================================
  SedaiOpcodeTable - single declarative source of truth for the opcode set.

  VM PERFORMANCE PLAN, milestone M1 (descriptor). See
  job/docs/VM_DISPATCH_E_RIDIMENSIONABILITA.md.

  The 16-bit (group.sub) opcode space is sparse: group 9 is missing and the
  superinstructions jump from $0B?? to $C8??. A flat "case Op of" over that
  range degrades to a binary search (measured 2026-07-13). This unit maps every
  ENABLED opcode to a DENSE linear index by construction, so the dispatch can be
  a single compact case (milestone M2).

  OPCODES is the ordered list of every opcode constant compiled into THIS build
  (auto-generated from the SedaiBytecodeTypes const block, declaration order).
  Its ENTRIES ARE THE bcXxx CONSTANTS THEMSELVES, so the values can never drift
  from their numeric definitions. Everything else - group sizes, dense bases,
  the forward map Op16ToDense and the inverse GDenseToOp16 - is DERIVED from it
  at initialization (density by construction, plan point C).

  The dense index is  GroupBase[group] + (op and $FF).  Within every enabled
  group the sub-opcodes are dense (0..maxsub, verified), so normal groups occupy
  contiguous dense ranges; the superinstruction group keeps a 256-wide block
  with holes (the unused sub-codes), which the dispatch routes to the illegal-
  opcode handler.

  WHEN ADDING A NEW OPCODE: add its bcXxx constant to OPCODES below (and to the
  SedaiBytecodeTypes const block). VerifyOpcodeTable asserts the map stays a
  bijection over the enabled set.
  ============================================================================ }

{$mode objfpc}{$H+}

interface

uses
  SysUtils, SedaiBytecodeTypes;

const
  // Auto-generated from SedaiBytecodeTypes.pas const block (declaration order).
  // Values ARE the bcXxx constants -> cannot drift from their numeric definitions.
  OPCODE_LIST_COUNT = 502 {$IFDEF WEB_MODE} + 12 {$ENDIF};
  OPCODES: array[0..OPCODE_LIST_COUNT - 1] of Word = (
    bcLoadConstInt, bcLoadConstFloat, bcLoadConstString, bcCopyInt, bcCopyFloat, bcCopyString,
    bcLoadVar, bcStoreVar, bcAddInt, bcSubInt, bcMulInt, bcDivInt,
    bcModInt, bcNegInt, bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat,
    bcPowFloat, bcNegFloat, bcIntToFloat, bcFloatToInt, bcIntToString, bcFloatToString,
    bcStringToInt, bcStringToFloat, bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpGtInt,
    bcCmpLeInt, bcCmpGeInt, bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpGtFloat,
    bcCmpLeFloat, bcCmpGeFloat, bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString,
    bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor, bcBitwiseNot, bcJump, bcJumpIfZero,
    bcJumpIfNotZero, bcCall, bcReturn, bcEnd, bcStop, bcFast,
    bcSlow, bcSleep, bcKey, bcNop, bcClear, bcTron,
    bcTroff, bcDataAdd, bcDataReadInt, bcDataReadFloat, bcDataReadString, bcDataRestore,
    bcGet, bcGetkey, bcPrintUsing, bcPrintUsingStage, bcPrintUsingRun, bcRecordNewArrayInd,
    bcRecordNewBlock, bcPudef, bcChar, bcLoad, bcSave, bcVerify,
    bcBload, bcBsave, bcBoot, bcRun, bcList, bcNew,
    bcDelete, bcRenumber, bcCatalog, bcCopyFile, bcScratch, bcRenameFile,
    bcConcat, bcMkdir, bcChdir, bcRmdir, bcRaiseError, bcMoveFile,
    bcTrap, bcResume, bcResumeNext, bcOnError, bcResumeLabel, bcModFloat,
    bcFrame, bcCallSub, bcReturnSub, bcCallSubIndirect, bcSetEnviron, bcShell,
    bcCmpLtUInt, bcCmpGtUInt, bcCmpLeUInt, bcCmpGeUInt, bcDivUInt, bcModUInt,
    bcXferStoreInt, bcXferStoreFloat, bcXferStoreString, bcXferLoadInt, bcXferLoadFloat, bcXferLoadString,
    bcRecordNew, bcRecordLoadInt, bcRecordLoadFloat, bcRecordLoadString, bcRecordStoreInt, bcRecordStoreFloat,
    bcRecordStoreString, bcRecordNewArray, bcRecordTypeId, bcRecordFree, bcRecMarkPush, bcRecMarkPop,
    bcLoadProcAddr, bcThreadCreate, bcThreadWait, bcThreadSelf, bcThreadDetach, bcFloatRound,
    bcNarrowInt, bcNarrowSingle, bcShl, bcShr, bcShrUInt, bcPrintUsingInt,
    bcRandomize, bcMutexCreate, bcMutexLock, bcMutexUnlock, bcMutexDestroy, bcCondCreate,
    bcCondWait, bcCondSignal, bcCondBroadcast, bcCondDestroy, bcAssert, bcStrConcat,
    bcStrLen, bcStrLeft, bcStrRight, bcStrMid, bcStrAsc, bcStrChr,
    bcStrStr, bcStrVal, bcStrHex, bcStrInstr, bcStrErr, bcStrLTrim,
    bcStrRTrim, bcStrTrim, bcStrUCase, bcStrLCase, bcStrInstrRev, bcStrSpace,
    bcStrOct, bcStrBin, bcStrValInt, bcStrString, bcStrTrimSet, bcStrInstrRevAny,
    bcStrLenW, bcStrLeftW, bcStrRightW, bcStrMidW, bcStrInstrW, bcStrInstrRevW,
    bcStrSAdd, bcFileExists, bcCurDir, bcEnviron, bcFileLen, bcExePath,
    bcStrFormat, bcCommand, bcFileDateTime, bcDateStr, bcDateName, bcStrMkInt,
    bcStrMkFloat, bcStrCvInt, bcStrCvFloat, bcStrInstrAny, bcStrWChr, bcStrWStringN,
    bcMathSin, bcMathCos, bcMathTan, bcMathAtn, bcMathLog, bcMathExp,
    bcMathSqr, bcMathAbs, bcMathSgn, bcMathInt, bcMathRnd, bcMathLog10,
    bcMathLog2, bcMathLogN, bcStrDec, bcMathAcos, bcMathAsin, bcMathAtan2,
    bcMathFix, bcMathFrac, bcDateNow, bcDateDecode, bcDateSerial, bcTimeSerial,
    bcDateValue, bcIsDate, bcDateAdd, bcDateDiff, bcDatePart, bcSetClock,
    bcMathSinh, bcMathCosh, bcMathTanh, bcMathAsinh, bcMathAcosh, bcMathAtanh,
    bcArrayLoad, bcArrayStore, bcArrayDim, bcArrayLoadInt, bcArrayLoadFloat, bcArrayLoadString,
    bcArrayStoreInt, bcArrayStoreFloat, bcArrayStoreString, bcArrayLBound, bcArrayUBound, bcArrayErase,
    bcArrayRedim, bcRefLoadInt, bcRefLoadFloat, bcRefLoadString, bcRefStoreInt, bcRefStoreFloat,
    bcRefStoreString, bcRefAddrField, bcRawAlloc, bcRawFree, bcRawRealloc, bcRawLoadInt,
    bcRawLoadFloat, bcRawStoreInt, bcRawStoreFloat, bcArrayRedimPush, bcArrayRedimN, bcArrayIdxPush,
    bcArrayIdxResolve, bcRawMemCopy, bcRawMemMove, bcRawClear, bcArrayBind, bcArrayUnbind,
    bcArrayBindApply, bcArrayLoadIndInt, bcArrayLoadIndFloat, bcArrayLoadIndString, bcArrayStoreIndInt, bcArrayStoreIndFloat,
    bcArrayStoreIndString, bcArrayIdxResolveInd, bcMemberArrayRedim, bcArrayLBoundInd, bcArrayUBoundInd, bcArrayCopyContents,
    bcArrayCopyRecords, bcArrayBindInd, bcPrint, bcPrintLn, bcPrintString, bcPrintStringLn,
    bcPrintInt, bcPrintIntLn, bcPrintComma, bcPrintSemicolon, bcPrintTab, bcPrintSpc,
    bcPrintNewLine, bcPrintEnd, bcInput, bcInputInt, bcInputFloat, bcInputString,
    bcPrintBool, bcPrintUInt, bcWInputChars, bcInputChars, bcConScreen, bcConLocate,
    bcConViewPrint, bcLoadTI, bcLoadTIS, bcStoreTIS, bcLoadDTS, bcFre,
    bcLoadEL, bcLoadER, bcLoadERRS, bcPeek, bcPoke, bcLoadCWDS,
    bcCsrlin, bcLoadDS, bcLoadDSS, bcLoadST, bcLoadERFN, bcLoadERMN,
    bcDopen, bcDclose, bcOpen, bcClose, bcGetFile, bcInputFile,
    bcPrintFile, bcCmd, bcAppend, bcDclear, bcRecord, bcPrintFileNewLine,
    bcPrintFileFloat, bcPrintFileInt, bcInputFileFloat, bcInputFileInt, bcFileQuery, bcSeekSet,
    bcInputFileLine, bcPutBinInt, bcPutBinFloat, bcGetBinInt, bcGetBinFloat, bcPutBinStr,
    bcGetBinStr, bcFileAttr, bcFileSetEof, bcSprite, bcMovsprAbs, bcMovsprRel,
    bcMovsprPolar, bcMovsprAuto, bcSprcolor, bcSprsav, bcCollision, bcBump,
    bcRspcolor, bcRsppos, bcRsprite, bcSpriteDef, bcSprSaveFile, bcSprLoadFile,
    bcSprSize, bcSprForm, bcGraphicRGBA, bcGraphicSetMode, bcGraphicBox, bcGraphicCircle,
    bcGraphicDraw, bcGraphicLocate, bcGraphicRdot, bcGraphicGetMode, bcGraphicColor, bcSetColor,
    bcGetColor, bcGraphicWidth, bcGraphicScale, bcGraphicPaint, bcGraphicWindow, bcGraphicSShape,
    bcGraphicGShape, bcGraphicGList, bcGraphicPos, bcGraphicRclr, bcGraphicRwindow, bcPLoad,
    bcPSave, bcPRst, bcGfxScreenRes, bcGfxPset, bcGfxPoint, bcGfxPaint,
    bcGfxLine, bcGfxCircle, bcGfxPalette, bcGfxPalGet, bcGfxPaletteReset, bcGfxColor,
    bcGfxForeColor, bcGfxImageCreate, bcGfxImageDestroy, bcGfxImageInfo, bcGfxGet, bcGfxPut,
    bcGfxScreenInfo, bcGfxScreenSet, bcGfxPCopy, bcGfxWindow, bcGfxPMap, bcGfxView,
    bcGfxScreen, bcMultikey, bcGetmouse, bcMouseAxis, bcSetmouse, bcGetJoystick,
    bcJoyBtn, bcJoyAxis, bcStick, bcStrig, bcGfxDrawGML, bcGfxPointCoord,
    bcGfxCircleEx, bcGfxPaintBorder, bcGfxSetTarget, bcGfxLineStyled, bcGfxScreenPtr, bcScnClr,
    bcSoundVol, bcSoundSound, bcSoundEnvelope, bcSoundTempo, bcSoundPlay, bcSoundFilter,
    bcBranchEqInt, bcBranchNeInt, bcBranchLtInt, bcBranchGtInt, bcBranchLeInt, bcBranchGeInt,
    bcBranchEqFloat, bcBranchNeFloat, bcBranchLtFloat, bcBranchGtFloat, bcBranchLeFloat, bcBranchGeFloat,
    bcAddIntTo, bcSubIntTo, bcMulIntTo, bcAddFloatTo, bcSubFloatTo, bcMulFloatTo,
    bcDivFloatTo, bcAddIntConst, bcSubIntConst, bcMulIntConst, bcAddFloatConst, bcSubFloatConst,
    bcMulFloatConst, bcDivFloatConst, bcBranchEqZeroInt, bcBranchNeZeroInt, bcBranchEqZeroFloat, bcBranchNeZeroFloat,
    bcArrayStoreIntConst, bcArrayStoreFloatConst, bcArrayStoreStringConst, bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe,
    bcSubIntToBranchGt, bcMulAddFloat, bcMulSubFloat, bcMulAddToFloat, bcMulSubToFloat, bcArrayLoadAddFloat,
    bcArrayLoadSubFloat, bcArrayLoadDivAddFloat, bcSquareSumFloat, bcAddSquareFloat, bcMulMulFloat, bcAddSqrtFloat,
    bcArrayLoadIntBranchNZ, bcArrayLoadIntBranchZ, bcArrayReverseRange, bcArrayShiftLeft, bcArraySwapInt, bcAddIntSelf,
    bcSubIntSelf, bcArrayLoadIntTo, bcArrayCopyElement, bcArrayMoveElement
    {$IFDEF WEB_MODE}
    , bcWebGetParam, bcWebPostParam, bcWebGetRaw, bcWebPostRaw, bcWebHtmlEncode, bcWebUrlEncode,
    bcWebMethod, bcWebPath, bcWebQuery, bcWebHeader, bcWebSetHeader, bcWebStatus
    {$ENDIF}
  );

  OPCODE_INVALID = $FFFF;   // marks a dense hole (an unused superinstruction sub-code)

  // Compile-time dense base of each group, for the flat dispatch case labels (milestone M2). These
  // MUST match GGroupBase computed at init from OPCODES; VerifyOpcodeTable asserts it, so a new opcode
  // that grows a group (or enabling WEB_MODE) fails the self-check LOUDLY instead of miscompiling.
  // Values below are for the default (no-WEB_MODE) build: web opcodes are gated out, so group 8 takes
  // no dense range and graphics/sound/super sit where they do here.
  DENSE_CORE_BASE     = 0;    // group 0  (155 opcodes) -> dense 0..154
  DENSE_STRING_BASE   = 155;  // group 1  (49)          -> 155..203
  DENSE_MATH_BASE     = 204;  // group 2  (36)          -> 204..239
  DENSE_ARRAY_BASE    = 240;  // group 3  (50)          -> 240..289
  DENSE_IO_BASE       = 290;  // group 4  (23)          -> 290..312
  DENSE_SPECIAL_BASE  = 313;  // group 5  (17)          -> 313..329
  DENSE_FILEIO_BASE   = 330;  // group 6  (27)          -> 330..356
  DENSE_SPRITE_BASE   = 357;  // group 7  (17)          -> 357..373
  {$IFDEF WEB_MODE}
  // group 8 (web, subs 1..12) inserts a 13-slot block, shifting graphics/sound/super up by 13.
  DENSE_WEB_BASE      = 374;  // 374..386 (12 used, slot 0 a hole)
  DENSE_GRAPHICS_BASE = 387;  // group 10 (64)          -> 387..450
  DENSE_SOUND_BASE    = 451;  // group 11 (6)           -> 451..456
  DENSE_SUPER_BASE    = 457;  // group 200 (256 slots)  -> 457..712
  DENSE_TOTAL         = 713;  // N (with web)
  {$ELSE}
  DENSE_GRAPHICS_BASE = 374;  // group 10 (64)          -> 374..437
  DENSE_SOUND_BASE    = 438;  // group 11 (6)           -> 438..443
  DENSE_SUPER_BASE    = 444;  // group 200 (256 slots)  -> 444..699 (58 used, 198 holes)
  DENSE_TOTAL         = 700;  // N
  {$ENDIF}

var
  // Derived at initialization from OPCODES (see InitOpcodeTable).
  GGroupBase: array[0..255] of Integer;   // dense base of a group byte; -1 if the group is absent in this build
  GGroupSize: array[0..255] of Integer;   // maxsub+1 of a group; 0 if absent
  GDenseCount: Integer;                   // N: number of dense slots (including the superinstruction holes)
  GDenseToOp16: array of Word;            // [0..N-1] -> 16-bit opcode, or OPCODE_INVALID for a hole

// Forward map, used once per instruction at load time (milestone M2).
function Op16ToDense(Op: Word): Integer; inline;

procedure InitOpcodeTable;
// Self-check: the map is a bijection from the enabled opcode set onto a prefix
// of [0..N-1]. Returns True and a summary, or False and the first violation.
function VerifyOpcodeTable(out Msg: string): Boolean;

implementation

function Op16ToDense(Op: Word): Integer;
begin
  Result := GGroupBase[Op shr 8] + (Op and $FF);
end;

procedure InitOpcodeTable;
var
  i, g, grp, sub, running: Integer;
begin
  for i := 0 to 255 do
  begin
    GGroupBase[i] := -1;
    GGroupSize[i] := 0;
  end;

  // 1) Group size = maxsub + 1 (normal groups are dense 0..maxsub; the
  //    superinstruction group keeps its full 256-wide block, holes included).
  for i := 0 to OPCODE_LIST_COUNT - 1 do
  begin
    grp := OPCODES[i] shr 8;
    sub := OPCODES[i] and $FF;
    if sub + 1 > GGroupSize[grp] then
      GGroupSize[grp] := sub + 1;
  end;

  // 2) Assign consecutive dense bases in ascending group order (density by
  //    construction over the ENABLED set: absent groups take no dense range).
  running := 0;
  for g := 0 to 255 do
    if GGroupSize[g] > 0 then
    begin
      GGroupBase[g] := running;
      running := running + GGroupSize[g];
    end;
  GDenseCount := running;

  // 3) Inverse table (dense -> 16-bit), holes left as OPCODE_INVALID.
  SetLength(GDenseToOp16, GDenseCount);
  for i := 0 to GDenseCount - 1 do
    GDenseToOp16[i] := OPCODE_INVALID;
  for i := 0 to OPCODE_LIST_COUNT - 1 do
    GDenseToOp16[Op16ToDense(OPCODES[i])] := OPCODES[i];
end;

function VerifyOpcodeTable(out Msg: string): Boolean;
var
  i, d: Integer;
  seen: array of Boolean;
begin
  Result := False;
  if GDenseCount = 0 then
  begin
    Msg := 'opcode table not initialized';
    Exit;
  end;
  SetLength(seen, GDenseCount);
  for i := 0 to GDenseCount - 1 do
    seen[i] := False;
  for i := 0 to OPCODE_LIST_COUNT - 1 do
  begin
    d := Op16ToDense(OPCODES[i]);
    if (d < 0) or (d >= GDenseCount) then
    begin
      Msg := Format('opcode $%.4X -> dense %d out of range [0,%d)', [OPCODES[i], d, GDenseCount]);
      Exit;
    end;
    if seen[d] then
    begin
      Msg := Format('dense collision at %d (opcode $%.4X)', [d, OPCODES[i]]);
      Exit;
    end;
    seen[d] := True;
    if GDenseToOp16[d] <> OPCODES[i] then
    begin
      Msg := Format('inverse mismatch at dense %d (got $%.4X, want $%.4X)', [d, GDenseToOp16[d], OPCODES[i]]);
      Exit;
    end;
  end;
  // The compile-time dense bases used as flat-dispatch case labels must match the runtime map.
  if (GGroupBase[bcGroupCore     shr 8] <> DENSE_CORE_BASE)     or
     (GGroupBase[bcGroupString   shr 8] <> DENSE_STRING_BASE)   or
     (GGroupBase[bcGroupMath     shr 8] <> DENSE_MATH_BASE)     or
     (GGroupBase[bcGroupArray    shr 8] <> DENSE_ARRAY_BASE)    or
     (GGroupBase[bcGroupIO       shr 8] <> DENSE_IO_BASE)       or
     (GGroupBase[bcGroupSpecial  shr 8] <> DENSE_SPECIAL_BASE)  or
     (GGroupBase[bcGroupFileIO   shr 8] <> DENSE_FILEIO_BASE)   or
     (GGroupBase[bcGroupSprite   shr 8] <> DENSE_SPRITE_BASE)   or
     {$IFDEF WEB_MODE}
     (GGroupBase[bcGroupWeb      shr 8] <> DENSE_WEB_BASE)      or
     {$ENDIF}
     (GGroupBase[bcGroupGraphics shr 8] <> DENSE_GRAPHICS_BASE) or
     (GGroupBase[bcGroupSound    shr 8] <> DENSE_SOUND_BASE)    or
     (GGroupBase[bcGroupSuper    shr 8] <> DENSE_SUPER_BASE)    or
     (GDenseCount <> DENSE_TOTAL) then
  begin
    Msg := Format('compile-time DENSE_*_BASE constants out of sync with runtime map '
                + '(core=%d/%d string=%d/%d math=%d/%d array=%d/%d io=%d/%d special=%d/%d '
                + 'fileio=%d/%d sprite=%d/%d graphics=%d/%d sound=%d/%d super=%d/%d N=%d/%d)',
      [GGroupBase[bcGroupCore shr 8], DENSE_CORE_BASE,
       GGroupBase[bcGroupString shr 8], DENSE_STRING_BASE,
       GGroupBase[bcGroupMath shr 8], DENSE_MATH_BASE,
       GGroupBase[bcGroupArray shr 8], DENSE_ARRAY_BASE,
       GGroupBase[bcGroupIO shr 8], DENSE_IO_BASE,
       GGroupBase[bcGroupSpecial shr 8], DENSE_SPECIAL_BASE,
       GGroupBase[bcGroupFileIO shr 8], DENSE_FILEIO_BASE,
       GGroupBase[bcGroupSprite shr 8], DENSE_SPRITE_BASE,
       GGroupBase[bcGroupGraphics shr 8], DENSE_GRAPHICS_BASE,
       GGroupBase[bcGroupSound shr 8], DENSE_SOUND_BASE,
       GGroupBase[bcGroupSuper shr 8], DENSE_SUPER_BASE,
       GDenseCount, DENSE_TOTAL]);
    Exit;
  end;

  Msg := Format('%d opcodes, dense N=%d (%d superinstruction holes); bases verified',
                [OPCODE_LIST_COUNT, GDenseCount, GDenseCount - OPCODE_LIST_COUNT]);
  Result := True;
end;

initialization
  InitOpcodeTable;
end.
