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
{$codepage UTF8}   // CP_UTF8 literals, like every other unit. A string that crosses a codepage
                   // boundary is converted and a comparison stops being a memcmp - see SedaiFileIO.

interface

uses
  SysUtils, SedaiBytecodeTypes;

const
  // Auto-generated from SedaiBytecodeTypes.pas const block (declaration order).
  // Values ARE the bcXxx constants -> cannot drift from their numeric definitions.
  OPCODE_LIST_COUNT = 576 {$IFDEF WEB_MODE} + 12 {$ENDIF};   // +1 bcStrAscW; +1 bcGfxDrawString; +5 bit intrinsics; +5 CEIL..COPYSIGN; +2 the bit-casts; +1 bcCpuCount; +13 BigInt; +1 bcStrMidAssignArr; +1 bcGfxCircleExF; +1 bcRecordReallocBlock; +1 bcRecordBlockLen; +2 the wide "Any set" searches
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
    bcRecordNewBlock, bcRecordReallocBlock, bcRecordBlockLen, bcPudef, bcChar, bcLoad, bcSave, bcVerify,
    bcBload, bcBsave, bcBoot, bcRun, bcList, bcNew,
    bcDelete, bcRenumber, bcCatalog, bcCopyFile, bcScratch, bcRenameFile,
    bcConcat, bcMkdir, bcChdir, bcRmdir, bcRaiseError, bcMoveFile,
    bcTrap, bcResume, bcResumeNext, bcOnError, bcResumeLabel, bcModFloat,
    bcFrame, bcCallSub, bcReturnSub, bcCallSubIndirect, bcSetEnviron, bcShell,
    bcCmpLtUInt, bcCmpGtUInt, bcCmpLeUInt, bcCmpGeUInt, bcDivUInt, bcModUInt,
    bcXferStoreInt, bcXferStoreFloat, bcXferStoreString, bcXferLoadInt, bcXferLoadFloat, bcXferLoadString,
    bcRecordNew, bcRecordLoadInt, bcRecordLoadFloat, bcRecordLoadString, bcRecordStoreInt, bcRecordStoreFloat,
    bcRecordStoreString, bcRecordNewArray, bcRecordTypeId, bcRecordSetTypeId, bcRecordFree, bcRecMarkPush, bcRecMarkPop,
    bcLoadProcAddr, bcThreadCreate, bcThreadWait, bcThreadSelf, bcThreadDetach, bcFloatRound,
    bcNarrowInt, bcNarrowSingle, bcShl, bcShr, bcShrUInt, bcPrintUsingInt,
    bcBitClz, bcBitCtz, bcBitPopcnt, bcBitRotl, bcBitRotr,
    bcVarArgCtl, bcVarArgPushInt, bcVarArgPushFloat, bcVarArgPushStr,
    bcVarArgBase, bcVarArgGetInt, bcVarArgGetFloat, bcVarArgGetStr,
    bcRandomize, bcMutexCreate, bcMutexLock, bcMutexUnlock, bcMutexDestroy, bcCondCreate,
    bcCondWait, bcCondSignal, bcCondBroadcast, bcCondDestroy, bcAssert, bcStrConcat,
    bcStrLen, bcStrLeft, bcStrRight, bcStrMid, bcStrAsc, bcStrAscMid, bcStrChr,
    bcStrStr, bcStrVal, bcStrHex, bcStrInstr, bcStrErr, bcStrLTrim,
    bcStrRTrim, bcStrTrim, bcStrUCase, bcStrLCase, bcStrInstrRev, bcStrSpace,
    bcStrOct, bcStrBin, bcStrValInt, bcStrString, bcStrTrimSet, bcStrInstrRevAny,
    bcStrLenW, bcStrAscW, bcStrLeftW, bcStrRightW, bcStrMidW, bcStrInstrW, bcStrInstrRevW,
    bcStrInstrAnyW, bcStrInstrRevAnyW,
    bcStrSAdd, bcFileExists, bcCurDir, bcEnviron, bcFileLen, bcExePath,
    bcStrFormat, bcCommand, bcFileDateTime, bcDateStr, bcDateName, bcStrMkInt,
    bcStrMkFloat, bcStrCvInt, bcStrCvFloat, bcStrInstrAny, bcStrWChr, bcStrWStringN,
    bcMathSin, bcMathCos, bcMathTan, bcMathAtn, bcMathLog, bcMathExp,
    bcMathSqr, bcMathAbs, bcMathSgn, bcMathInt, bcMathRnd, bcMathLog10,
    bcMathLog2, bcMathLogN, bcStrDec, bcMathAcos, bcMathAsin, bcMathAtan2,
    bcMathFix, bcMathFrac, bcDateNow, bcDateDecode, bcDateSerial, bcTimeSerial,
    bcDateValue, bcIsDate, bcDateAdd, bcDateDiff, bcDatePart, bcSetClock,
    bcMathSinh, bcMathCosh, bcMathTanh, bcMathAsinh, bcMathAcosh, bcMathAtanh,
    bcMathCeil, bcMathRound, bcMathMin, bcMathMax, bcMathCopySign,
    bcSingleBits, bcBitsToSingle,
    bcArrayLoad, bcArrayStore, bcArrayDim, bcArrayLoadInt, bcArrayLoadFloat, bcArrayLoadString,
    bcArrayStoreInt, bcArrayStoreFloat, bcArrayStoreString, bcArrayLBound, bcArrayUBound, bcArrayErase,
    bcArrayRedim, bcRefLoadInt, bcRefLoadFloat, bcRefLoadString, bcRefStoreInt, bcRefStoreFloat,
    bcRefStoreString, bcRefAddrField, bcRawAlloc, bcRawFree, bcRawRealloc, bcRawLoadInt,
    bcRawLoadFloat, bcRawStoreInt, bcRawStoreFloat, bcArrayRedimPush, bcArrayRedimN, bcArrayIdxPush,
    bcArrayIdxResolve, bcRawMemCopy, bcRawMemMove, bcRawClear, bcArrayBind, bcArrayUnbind,
    bcArrayBindApply, bcArrayLoadIndInt, bcArrayLoadIndFloat, bcArrayLoadIndString, bcArrayStoreIndInt, bcArrayStoreIndFloat,
    bcArrayStoreIndString, bcArrayIdxResolveInd, bcMemberArrayRedim, bcArrayLBoundInd, bcArrayUBoundInd, bcArrayCopyContents,
    bcArrayCopyRecords, bcArrayBindInd, bcRawLoadZStr, bcRawStoreZStr, bcPrint, bcPrintLn, bcPrintString, bcPrintStringLn,
    bcPrintInt, bcPrintIntLn, bcPrintComma, bcPrintSemicolon, bcPrintTab, bcPrintSpc,
    bcPrintNewLine, bcPrintEnd, bcInput, bcInputInt, bcInputFloat, bcInputString,
    bcPrintBool, bcPrintUInt, bcWInputChars, bcInputChars, bcConScreen, bcConLocate,
    bcConViewPrint, bcLoadTI, bcLoadTIS, bcStoreTIS, bcLoadDTS, bcFre,
    bcLoadEL, bcLoadER, bcLoadERRS, bcPeek, bcPoke, bcLoadCWDS,
    bcCsrlin, bcLoadDS, bcLoadDSS, bcLoadST, bcLoadERFN, bcLoadERMN, bcCpuCount,
    bcDopen, bcDclose, bcOpen, bcClose, bcGetFile, bcInputFile,
    bcPrintFile, bcCmd, bcAppend, bcDclear, bcRecord, bcPrintFileNewLine,
    bcPrintFileFloat, bcPrintFileInt, bcInputFileFloat, bcInputFileInt, bcFileQuery, bcSeekSet,
    bcInputFileLine, bcPutBinInt, bcPutBinFloat, bcGetBinInt, bcGetBinFloat, bcPutBinStr,
    bcGetBinStr, bcFileAttr, bcFileSetEof, bcPrintFileComma, bcPutBinMem, bcGetBinMem, bcPutBinArray,
    bcGetBinArray, bcPutBinPad, bcGetBinSkip, bcOpenFunc, bcDirSearch, bcDirAttr, bcSprite, bcMovsprAbs, bcMovsprRel,
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
    bcGfxCircleEx, bcGfxCircleExF, bcGfxPaintBorder, bcGfxSetTarget, bcGfxLineStyled, bcGfxScreenPtr,
    bcGfxScreenLock, bcGfxScreenUnlock, bcScnClr,
  bcGfxImageConvertRow, bcGfxDrawString, bcRegexCount, bcRegexReplace,
    bcSoundVol, bcSoundSound, bcSoundEnvelope, bcSoundTempo, bcSoundPlay, bcSoundFilter,
    bcBigNew, bcBigFromInt, bcBigCopy, bcBigToStr,
    bcBigAdd, bcBigSub, bcBigMul, bcBigCmp, bcBigFromStr, bcBigMulSmall, bcBigDiv, bcBigMod, bcBigToInt,
    bcBranchEqInt, bcBranchNeInt, bcBranchLtInt, bcBranchGtInt, bcBranchLeInt, bcBranchGeInt,
    bcBranchEqFloat, bcBranchNeFloat, bcBranchLtFloat, bcBranchGtFloat, bcBranchLeFloat, bcBranchGeFloat,
    bcAddIntTo, bcSubIntTo, bcMulIntTo, bcAddFloatTo, bcSubFloatTo, bcMulFloatTo,
    bcDivFloatTo, bcAddIntConst, bcSubIntConst, bcMulIntConst, bcAddFloatConst, bcSubFloatConst,
    bcMulFloatConst, bcDivFloatConst, bcBranchEqZeroInt, bcBranchNeZeroInt, bcBranchEqZeroFloat, bcBranchNeZeroFloat,
    bcArrayStoreIntConst, bcArrayStoreFloatConst, bcArrayStoreStringConst, bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe,
    bcSubIntToBranchGt, bcMulAddFloat, bcMulSubFloat, bcMulAddToFloat, bcMulSubToFloat, bcArrayLoadAddFloat,
    bcArrayLoadSubFloat, bcArrayLoadDivAddFloat, bcSquareSumFloat, bcAddSquareFloat, bcMulMulFloat, bcAddSqrtFloat,
    bcArrayLoadIntBranchNZ, bcArrayLoadIntBranchZ, bcArrayReverseRange, bcArrayShiftLeft, bcArraySwapInt, bcAddIntSelf,
    bcSubIntSelf, bcArrayLoadIntTo, bcArrayCopyElement, bcArrayMoveElement, bcStrConcatCharAt,
    bcStrAppendMapped, bcStrMidAssign,
    // The two comparison families that had no branch form; see the note at their declaration.
    bcBranchEqString, bcBranchNeString, bcBranchLtString, bcBranchGtString, bcBranchLeString, bcBranchGeString,
    bcBranchLtUInt, bcBranchLeUInt, bcBranchGtUInt, bcBranchGeUInt,
    // MID$ into an array element / a DIM SHARED scalar; see the note at its declaration.
    bcStrMidAssignArr
    {$IFDEF WEB_MODE}
    , bcWebGetParam, bcWebPostParam, bcWebGetRaw, bcWebPostRaw, bcWebHtmlEncode, bcWebUrlEncode,
    bcWebMethod, bcWebPath, bcWebQuery, bcWebHeader, bcWebSetHeader, bcWebStatus
    {$ENDIF}
  );

  OPCODE_INVALID = $FFFF;   // marks a dense hole (an unused superinstruction sub-code)

  // Compile-time dense base of each group, for the flat dispatch case labels (milestone M2). These
  // MUST match GGroupBase computed at init from OPCODES; VerifyOpcodeTable asserts it, so a new opcode
  // that grows a group (or enabling WEB_MODE) fails the self-check LOUDLY instead of miscompiling.
  // ⇒ Since 1 Sep 2026 there is ONE ladder for both builds: the sizes below are the only numbers
  // written by hand, and WEB_MODE changes exactly one of them (group 8, 13 or 0).
  // The notes that follow are the history of the groups that grew - kept because each names the
  // symptom a wrong number produced.
  // ⚠️ bcRecordSetTypeId (core sub 168) made group 0 one wider on 18 Aug 2026, which shifts EVERY
  // base below and DENSE_TOTAL by one, in BOTH branches - and the core range in RunTemplate.inc.
  // ⚠️ The five bit intrinsics (core subs 163..167) made group 0 five wider, which shifts EVERY base
  // below and DENSE_TOTAL by 5, in both branches. Nothing checks these at compile time -
  // `sb --verify-opcodes` is what says so.
  // ⚠️ bcGfxCircleExF (graphics sub 68) made group 10 one wider on 23 Aug 2026: SOUND, BIGINT, SUPER
  // and TOTAL each moved up by one, in BOTH branches. ⭐ The self-check earned its keep again - the
  // new opcode compiled, disassembled and reached the VM's arm, and simply DID NOTHING, because the
  // dense map had shifted under it. `--verify-opcodes` printed "sound=480/479 super=499/498 N=571/570"
  // and that is the whole diagnosis.
  // ⚠️ bcRecordReallocBlock made group 0 one wider on 24 Aug 2026: EVERY base below moved up by
  // one. ⭐ `sb --verify-opcodes` is what said so and printed the numbers to use - the opcode
  // compiled, disassembled and had its arm, and the interpreter ran it as a STRING op because
  // dense 169 was the string base.
  // ⚠️ bcStrInstrAnyW / bcStrInstrRevAnyW (string subs 53/54) made group 1 TWO wider on 30 Aug 2026:
  // every base below moved up by two, in BOTH branches, and so did DENSE_TOTAL. ⭐ And the RANGE in
  // RunTemplate.inc is a SECOND counter that nothing checks either - it read "+ 0 .. + 52" and the two
  // new subs fell through it into the MATH group ("Unknown math opcode 309"). `--verify-opcodes` named
  // the missing NAMES; the range failed only at runtime. Two counters, in that order, neither automatic.
  // ⚠️ bcStrAscW (string sub 52) made group 1 one wider on 27 Aug 2026: EVERY base below moved up by
  // one, in BOTH branches, and so did DENSE_TOTAL. ⭐ `sb --verify-opcodes` earned its keep again - the
  // opcode compiled, disassembled and had its arm, and the interpreter ran it as a MATH op ("Unknown
  // math opcode 308"). The self-check printed every number to use.
  // ⭐⭐ ONE hand-written number per group: its SIZE. Every base is DERIVED by summing the sizes above
  // it, and DENSE_TOTAL is the sum of them all, so the chain can no longer go out of step with
  // ITSELF - only with the opcode list, which is exactly what `sb --verify-opcodes` compares it to.
  // ⛔ This replaces two hand-maintained ladders (one per WEB_MODE branch) that had to be edited in
  // lockstep and were not: DENSE_WEB_BASE sat one slot INSIDE the sprite group for months, and the
  // only thing that ever said so was "duplicate case label" in a WEB_MODE build nothing compiles.
  // ⛔ HEADROOM IS NOT ALLOWED: the runtime map is built from the actual opcode counts, so a size
  // with slack fails `--verify-opcodes` immediately - and it prints the right numbers to use.
  // ⇒ Growing a group is now ONE edit: its size. The base ladder, DENSE_TOTAL and the dispatch
  //   ranges in RunTemplate.inc all follow from it.
  DENSE_CORE_SIZE     = 171;  // group 0
  DENSE_STRING_SIZE   = 55;   // group 1   (bcStrInstrRevAnyW = sub 54)
  DENSE_MATH_SIZE     = 43;   // group 2   (CEIL/ROUND/MIN/MAX/COPYSIGN + the two bit-casts)
  DENSE_ARRAY_SIZE    = 52;   // group 3   (bcRawLoad/StoreZStr = subs 50/51)
  DENSE_IO_SIZE       = 23;   // group 4
  DENSE_SPECIAL_SIZE  = 18;   // group 5   (bcCpuCount = sub 17)
  DENSE_FILEIO_SIZE   = 37;   // group 6
  DENSE_SPRITE_SIZE   = 17;   // group 7
  {$IFDEF WEB_MODE}
  DENSE_WEB_SIZE      = 13;   // group 8   subs 1..12, slot 0 a hole
  {$ELSE}
  DENSE_WEB_SIZE      = 0;    // group 8 is gated out: it takes NO dense range
  {$ENDIF}
  DENSE_GRAPHICS_SIZE = 69;   // group 10  (bcGfxDrawString, bcGfxCircleExF, SCREENLOCK/UNLOCK)
  DENSE_SOUND_SIZE    = 6;    // group 11
  DENSE_BIGINT_SIZE   = 13;   // group 12
  DENSE_SUPER_SIZE    = 72;   // group 200 keeps its whole block, holes included

  DENSE_CORE_BASE     = 0;
  DENSE_STRING_BASE   = DENSE_CORE_BASE     + DENSE_CORE_SIZE;
  DENSE_MATH_BASE     = DENSE_STRING_BASE   + DENSE_STRING_SIZE;
  DENSE_ARRAY_BASE    = DENSE_MATH_BASE     + DENSE_MATH_SIZE;
  DENSE_IO_BASE       = DENSE_ARRAY_BASE    + DENSE_ARRAY_SIZE;
  DENSE_SPECIAL_BASE  = DENSE_IO_BASE       + DENSE_IO_SIZE;
  DENSE_FILEIO_BASE   = DENSE_SPECIAL_BASE  + DENSE_SPECIAL_SIZE;
  DENSE_SPRITE_BASE   = DENSE_FILEIO_BASE   + DENSE_FILEIO_SIZE;
  DENSE_WEB_BASE      = DENSE_SPRITE_BASE   + DENSE_SPRITE_SIZE;
  DENSE_GRAPHICS_BASE = DENSE_WEB_BASE      + DENSE_WEB_SIZE;   // web contributes 0 outside WEB_MODE
  DENSE_SOUND_BASE    = DENSE_GRAPHICS_BASE + DENSE_GRAPHICS_SIZE;
  DENSE_BIGINT_BASE   = DENSE_SOUND_BASE    + DENSE_SOUND_SIZE;
  DENSE_SUPER_BASE    = DENSE_BIGINT_BASE   + DENSE_BIGINT_SIZE;
  DENSE_TOTAL         = DENSE_SUPER_BASE    + DENSE_SUPER_SIZE; // N: 576, or 589 with web

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

function IsFallbackOpName(const N: string): Boolean;
// True for the shape BytecodeOpToString invents when it does not know an opcode: a word, an
// underscore, and digits to the end ("Group_4", "Web_11", "Op_7"). A real opcode name never has
// that shape - they are CamelCase with no underscore at all.
var
  i, u: Integer;
begin
  Result := False;
  u := 0;
  for i := 1 to Length(N) do
    if N[i] = '_' then u := i;
  if (u = 0) or (u = Length(N)) then Exit;
  for i := u + 1 to Length(N) do
    if not (N[i] in ['0'..'9']) then Exit;
  Result := True;
end;

function VerifyOpcodeTable(out Msg: string): Boolean;
var
  i, d: Integer;
  seen: array of Boolean;
  NoName: Integer;
  NoNameMsg: string;
begin
  Result := False;
  if GDenseCount = 0 then
  begin
    Msg := 'opcode table not initialized';
    Exit;
  end;
  NoName := 0; NoNameMsg := '';
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
    // ...and it must have a NAME. BytecodeOpToString is a hand-written case statement parallel to
    // the opcode constants, so a new opcode gets a name only if somebody remembers a second place.
    // On 20 Aug 2026 TWENTY-ONE of them had been forgotten and disassembled as "Group_4" - readable
    // only by grepping the sources for the number. That is the shape this project keeps paying for:
    // two hand-maintained lists drift, silently, and nothing says so.
    // The fallbacks all look like <Word>_<digits> ("Group_4", "Web_11"), which a real name never
    // does, so the check is exact and needs no second list of its own - it reads OPCODES, which is
    // the list the compiler itself emits from.
    if IsFallbackOpName(BytecodeOpToString(TBytecodeOp(OPCODES[i]))) then
    begin
      // ⛔ ACCUMULA, non esce: la prima versione usciva al primo trovato e li faceva scoprire uno
      // per build. Una rete deve dire TUTTO quello che ha trovato in un colpo solo.
      Inc(NoName);
      if NoName <= 40 then
        NoNameMsg := NoNameMsg + Format('%s  $%.4X -> "%s"', [LineEnding,
                     OPCODES[i], BytecodeOpToString(TBytecodeOp(OPCODES[i]))]);
    end;
  end;
  if NoName > 0 then
  begin
    Msg := Format('%d opcodes have no name in BytecodeOpToString ' +
                  '(a hand-written case parallel to the constants - add them there):%s',
                  [NoName, NoNameMsg]);
    if NoName > 40 then Msg := Msg + LineEnding + '  ...';
    Exit;
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
    // The comparison above tests web too, so the message has to PRINT web too - otherwise a
    // WEB_MODE build reports "out of sync" with every printed pair equal, and the one number that
    // is wrong is the only one not on the line. That is exactly how DENSE_WEB_BASE stayed one too
    // low: the check fired and named nothing.
    Msg := Format('compile-time DENSE_*_BASE constants out of sync with runtime map '
                + '(core=%d/%d string=%d/%d math=%d/%d array=%d/%d io=%d/%d special=%d/%d '
                + 'fileio=%d/%d sprite=%d/%d '
                {$IFDEF WEB_MODE} + 'web=%d/%d ' {$ENDIF}
                + 'graphics=%d/%d sound=%d/%d super=%d/%d N=%d/%d)',
      [GGroupBase[bcGroupCore shr 8], DENSE_CORE_BASE,
       GGroupBase[bcGroupString shr 8], DENSE_STRING_BASE,
       GGroupBase[bcGroupMath shr 8], DENSE_MATH_BASE,
       GGroupBase[bcGroupArray shr 8], DENSE_ARRAY_BASE,
       GGroupBase[bcGroupIO shr 8], DENSE_IO_BASE,
       GGroupBase[bcGroupSpecial shr 8], DENSE_SPECIAL_BASE,
       GGroupBase[bcGroupFileIO shr 8], DENSE_FILEIO_BASE,
       GGroupBase[bcGroupSprite shr 8], DENSE_SPRITE_BASE,
       {$IFDEF WEB_MODE}
       GGroupBase[bcGroupWeb shr 8], DENSE_WEB_BASE,
       {$ENDIF}
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
