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
{$modeswitch advancedrecords}
{$interfaces CORBA}
{$codepage UTF8}
{$inline on}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, fgl, Variants;

const
  { Register allocation limits }
  MIN_REGISTER_SLOTS = 256;      // Initial allocation size (backward compatible)
  MAX_REGISTER_SLOTS = 65536;    // Maximum registers per type (2^16)

  { FreeBASIC pointer encoding. A pointer value is a packed int: the high bits hold (backingArrayId+1)
    so 0 stays NULL, the low POINTER_ARRAY_SHIFT bits hold the element offset (in element units). Plain
    integer arithmetic on the pointer ("p + 1") therefore advances by one array element. Deref decodes
    arrayId = addr shr POINTER_ARRAY_SHIFT - 1, offset = addr and POINTER_OFFSET_MASK. }
  POINTER_ARRAY_SHIFT = 32;
  POINTER_OFFSET_MASK = (Int64(1) shl POINTER_ARRAY_SHIFT) - 1;

  { FreeBASIC "@Sin" and other address-of-builtin function pointers. A real entry PC is a small bytecode
    index, so a value with this high tag (bit 62) set cannot collide with one. An indirect call
    (bcCallSubIndirect) whose target carries the tag reads its Double argument from float transfer slot 0,
    applies the math op in the low byte, and writes the Double result slot — no jump. Lets
    "Dim f As Function(As Double) As Double = @Sin : f(x)" and dispatch tables of math functions work.
    Op ids: 1=Sin 2=Cos 3=Tan 4=Atn 5=Sqr 6=Exp 7=Log 8=Abs 9=Asin 10=Acos 11=Sinh 12=Cosh 13=Tanh 14=Int. }
  BUILTIN_FP_TAG = Int64($4000000000000000);

  { Record-field pointer (@obj.field). FArrays-backed pointers (scalars/array elements) have bit 63
    clear (arrayId+1 < 2^31); a record-field pointer sets bit 63 (RECPTR_TAG) as a discriminator. It
    packs the record handle's index in bits [RECPTR_SLOT_BITS..61], the shared-record flag in bit 62
    (copied straight from the handle), and the field slot in the low RECPTR_SLOT_BITS bits. The six
    bcRef{Load,Store} ops test bit 63 to route to record storage vs FArrays. }
  RECPTR_TAG = Int64(1) shl 63;
  // A3-i widened this from 16. The low field is no longer a slot INDEX but the packed
  // (byte offset shl 4 or width code) that ComputeUDTLiveLayout stamps into TUDTField.Slot, so 16
  // bits would have capped a record at 4096 bytes - a "String * 4096" member reaches that alone.
  // 24 bits allows a megabyte per record and still leaves 38 bits of handle index, which is more
  // records than the address space holds.
  RECPTR_SLOT_BITS = 24;
  RECPTR_SLOT_MASK = (Int64(1) shl RECPTR_SLOT_BITS) - 1;
  // bits [RECPTR_SLOT_BITS..61] hold the index, so this must shrink by exactly what the low field
  // grew: 62 - 24 = 38. Leaving it at 46 would let a large handle spill into the shared flag.
  RECPTR_INDEX_MASK = (Int64(1) shl 38) - 1;

  { FreeBASIC raw memory (Allocate/CAST/...). A raw pointer is a byte OFFSET into the VM-internal byte
    heap (FRawHeap), tagged with RAWPTR_TAG (bit 62) so it is distinct from a managed FArrays pointer
    (bit 63=0, bit 62=0) and a record-field pointer (bit 63=1). Deref reads/writes SizeOf(T) bytes at the
    offset; pointer arithmetic scales by SizeOf(pointee). 0 = NULL (untagged). The raw type code below
    selects the element width/bank at load/store. }
  RAWPTR_TAG = Int64(1) shl 62;
  { A tagged raw pointer names one of two REGIONS, selected by bit 61:

      region 0 (bit 61 clear)  the VM's byte heap, FRawHeap -- everything the Allocate family returns
      region 1 (bit 61 set)    the working page's framebuffer -- what SCREENPTR returns

    Both are ordinary byte arrays owned by the VM: no machine address is ever handed to a BASIC program,
    and every dereference is bounds-checked against the region it names. That is what lets a FreeBASIC
    program write pixels the way it expects -- "*(p + y*pitch + x*4) = colour" -- without the VM losing
    memory safety. Pointer arithmetic only adds to the offset, so a pointer never leaves its region.

    Bit 62 is crowded (SHARED_REC_FLAG and BUILTIN_FP_TAG use it in their own namespaces), which is why
    the framebuffer is a second REGION of the raw-pointer namespace rather than a third tag. }
  RAWPTR_REGION_FB = Int64(1) shl 61;       // region selector: framebuffer instead of the byte heap
  RAWPTR_OFS_MASK = RAWPTR_REGION_FB - 1;   // byte offset occupies the low 61 bits
  // Raw element type codes (Immediate of bcRaw{Load,Store}): width + bank.
  RTC_I8 = 1; RTC_I16 = 2; RTC_I32 = 3; RTC_I64 = 4; RTC_SINGLE = 5; RTC_DOUBLE = 6;

var
  // Runtime master switch for the SSA optimization passes (the `--no-opt` CLI flag clears it). The
  // structural passes (SSA construction, dominator tree, PHI elimination) ignore it and always run;
  // only the value-optimization passes early-out when it is False. Used by the differential test
  // harness to compare optimized vs unoptimized output of the same program.
  GSSAOptimizationsEnabled: Boolean = True;

  // Will this program actually be run through the AOT? Set from the command line before the SSA
  // pipeline runs, so a pass can pick the shape that suits the engine that will execute it.
  //
  // Only RunConcatCharFusion reads it today, and it needs to: fusing "acc += Mid(tab,k,1)" into one
  // opcode is worth -30% under the AOT and costs +5% interpreted, because interpreting it means a
  // call into the superinstruction dispatcher. A pass that helps one engine and hurts the other
  // should ask which one is coming rather than average the two.
  GAotWillRun: Boolean = False;

type
  TSSARegisterType = (srtInt, srtFloat, srtString);

  TSSAValueKind = (
    svkNone, svkRegister, svkConstInt, svkConstFloat,
    svkConstString, svkVariable, svkLabel, svkArrayRef
  );

  { Forward declarations }
  TSSABasicBlock = class;

  { TSSAValue is a POD record on purpose: it used to carry three ansistrings inline, and since
    it is a local (often several) in nearly every SSA-generation procedure and is passed by
    value four times per EmitInstruction, FPC's per-call zero-init + RTTI-finalization of those
    strings dominated the whole compile pipeline (measured ~24 us of prologue/epilogue on the
    big ProcessExpression frame against ~0.25 us of body). The strings now live in a process-wide
    interned pool (append-only, id 0 = '') and the record stores integer ids; the VarName /
    ConstString / LabelName properties keep every use site source-compatible. FillChar-to-zero
    still yields a value whose three names read as '' (id 0). NOT thread-safe: compilation is
    single-threaded (VM worker threads execute bytecode, they never compile). }
  TSSAValue = record
  public
    Kind: TSSAValueKind;
    RegType: TSSARegisterType;
    RegIndex: Integer;
    Version: Integer;      // SSA versioning: R0_1, R0_2, etc. (0 = unversioned/legacy)
    ConstInt: Int64;
    ConstFloat: Double;
    ArrayIndex: Integer;  // For svkArrayRef: index into FArrays
    VarNameId: Integer;      // pool id of the variable name ('' = 0)
    ConstStringId: Integer;  // pool id of the string constant ('' = 0)
    LabelNameId: Integer;    // pool id of the label name ('' = 0)
  private
    function GetVarName: string; inline;
    procedure SetVarName(const AValue: string); inline;
    function GetConstString: string; inline;
    procedure SetConstString(const AValue: string); inline;
    function GetLabelName: string; inline;
    procedure SetLabelName(const AValue: string); inline;
  public
    property VarName: string read GetVarName write SetVarName;
    property ConstString: string read GetConstString write SetConstString;
    property LabelName: string read GetLabelName write SetLabelName;
  end;

  TSSAOpCode = (
    ssaPhi,  // PHI function for SSA merge points: dest = PHI(src1 from B1, src2 from B2, ...)
    ssaLoadConstInt, ssaLoadConstFloat, ssaLoadConstString,
    ssaCopyInt, ssaCopyFloat, ssaCopyString,
    ssaLoadVar, ssaStoreVar,
    ssaAddInt, ssaSubInt, ssaMulInt, ssaDivInt, ssaModInt, ssaNegInt,
    ssaDivUInt, ssaModUInt,   // unsigned 64-bit \ and Mod (QWord semantics)
    ssaAddFloat, ssaSubFloat, ssaMulFloat, ssaDivFloat, ssaModFloat, ssaPowFloat, ssaNegFloat,
    ssaIntToFloat, ssaFloatToInt, ssaIntToString, ssaFloatToString,
    ssaStringToInt, ssaStringToFloat,
    ssaCmpEqInt, ssaCmpNeInt, ssaCmpLtInt, ssaCmpGtInt, ssaCmpLeInt, ssaCmpGeInt,
    ssaCmpLtUInt, ssaCmpGtUInt, ssaCmpLeUInt, ssaCmpGeUInt,   // unsigned 64-bit compares (QWord)
    ssaCmpEqFloat, ssaCmpNeFloat, ssaCmpLtFloat, ssaCmpGtFloat, ssaCmpLeFloat, ssaCmpGeFloat,
    ssaCmpEqString, ssaCmpNeString, ssaCmpLtString, ssaCmpGtString,
    ssaBitwiseAnd, ssaBitwiseOr, ssaBitwiseXor, ssaBitwiseNot,
    ssaStrConcat, ssaStrLen, ssaStrLeft, ssaStrRight, ssaStrMid,
    ssaStrAsc, ssaStrChr, ssaStrStr, ssaStrVal, ssaStrHex, ssaStrInstr, ssaStrErr,
    // Asc(Mid(s, start, len)) without building the substring - see bcStrAscMid.
    ssaStrAscMid,
    // "acc + Mid(tab, k, 1)" without building the one-character substring - see bcStrConcatCharAt.
    // Dest := Src1 + Src2[Src3]; when Dest IS Src1 the VM grows the accumulator in place.
    ssaStrConcatCharAt,
    // "acc += tab[Asc(Mid(s, i, 1)) + 1]" as ONE instruction - see bcStrAppendMapped. It is the
    // whole inner loop of reverse-complement: read the byte of Src1 at Src3, index Src2 with its
    // code, append that byte to Dest. Dest is READ as well as written (the accumulator grows).
    ssaStrAppendMapped,
    // "MID$(t, start [, len]) = src" as ONE instruction - see bcStrMidAssign. The FreeBASIC MID
    // STATEMENT is a pure OVERWRITE: Len(t) never changes, so for a start inside the string it is a
    // bounded Move into t's own buffer. It was lowered instead as
    // "Left(t, start-1) + Left(src, avail) + Mid(t, start+n)", which REBUILDS the whole string on
    // every assignment - filling a buffer character by character was quadratic.
    // Dest = t out, Src1 = t in (same register in practice, so it is written in place), Src2 = the
    // replacement ALREADY capped to len by the ssaStrLeft the lowering emits, Immediate = the int
    // register holding start. Four values, exactly like ssaStrConcatCharAt - no packing needed.
    ssaStrMidAssign,
    // FreeBASIC string functions (B1.2): single string arg -> string result.
    ssaStrLTrim, ssaStrRTrim, ssaStrTrim, ssaStrUCase, ssaStrLCase,
    ssaStrInstrRev,   // INSTRREV(s, sub) -> int (last occurrence)
    ssaStrSpace,      // SPACE(n) -> string of n spaces
    ssaStrString,     // STRING(n,ch) -> n copies of a char (Src1=count int, Src2=charcode int)
    ssaStrTrimSet,    // LTRIM/RTRIM/TRIM(s, set) -> trim substring from ends (Src1/Src2=string, Src3=mode const 0=both/1=left/2=right [|4=Any])
    ssaStrInstrRevAny, // INSTRREV(str, Any set) -> int last position of any char in set (Src1/Src2=string)
    ssaStrInstrAny,    // INSTR([start,] str, Any set) -> int FIRST position of any char in set (Src3=start reg)
    ssaStrLenW,        // LEN(wstring) -> int Unicode codepoint count (UTF-8 storage; Src1=string)
    ssaStrLeftW,       // LEFT$(wstring, n)  -> first n codepoints (Src1=string, Src2=int)
    ssaStrRightW,      // RIGHT$(wstring, n) -> last n codepoints (Src1=string, Src2=int)
    ssaStrMidW,        // MID$(wstring, start[,len]) -> codepoint substring (Src1=string, Src2=int start, Src3=int len)
    ssaStrInstrW,      // INSTR(wstring, sub) -> int codepoint position of first occurrence (Src1/Src2=string)
    ssaStrInstrRevW,   // INSTRREV(wstring, sub) -> int codepoint position of last occurrence (Src1/Src2=string)
    ssaStrWChr,        // WCHR(n) -> UTF-8 bytes of Unicode codepoint n (Src1=int)
    ssaStrWStringN,    // WSTRING(n,cp) -> n copies of the UTF-8 char for codepoint cp (Src1=int n, Src2=int cp)
    ssaStrSAdd,        // SADD(s) -> raw byte-heap pointer to a NUL-terminated copy of s (Dest=int, Src1=string)
    ssaFileExists,     // FILEEXISTS(path) -> -1 if the file exists else 0 (Dest=int, Src1=string)
    ssaCurDir,         // CURDIR$ -> current working directory (Dest=string, no operand)
    ssaEnviron,        // ENVIRON$(name) -> environment variable value (Dest=string, Src1=string)
    ssaExePath,        // EXEPATH -> directory of the running program (Dest=string, no operand)
    ssaCommand,        // COMMAND$(index) -> command-line argument(s) (Dest=string, Src1=index int)
    ssaStrFormat,      // FORMAT(num, mask) -> formatted string (Dest=string, Src1=mask string, Immediate=value float reg)
    ssaFileLen,        // FILELEN(path) -> file size in bytes (Dest=int, Src1=string)
    ssaFileDateTime,   // FILEDATETIME(path) -> last-modified date serial (Dest=float, Src1=string)
    // FreeBASIC numeric serialization (B3): pack/unpack a number to/from a fixed-width binary string.
    ssaStrMkInt,       // MKI/MKL/MKSHORT/MKLONGINT -> binary string of an int (Dest=string, Src1=int, Imm=byte width)
    ssaStrMkFloat,     // MKS/MKD -> 4/8-byte IEEE binary string of a float (Dest=string, Src1=float, Imm=byte width)
    ssaStrCvInt,       // CVI/CVL/CVSHORT/CVLONGINT -> int from a binary string (Dest=int, Src1=string, Imm=byte width)
    ssaStrCvFloat,     // CVS/CVD -> float from a 4/8-byte IEEE binary string (Dest=float, Src1=string, Imm=byte width)
    // FreeBASIC numeric<->string conversions (B1.3).
    ssaStrOct,        // OCT(n) -> octal string
    ssaStrBin,        // BIN(n) -> binary string
    ssaStrValInt,     // VALINT/VALLNG/VALUINT(s) -> integer
    ssaRegexCount,    // REGEXCOUNT(s, pattern) -> number of non-overlapping matches
    ssaRegexReplace,  // REGEXREPLACE(s, pattern, repl) -> string with every match replaced
    ssaMathSin, ssaMathCos, ssaMathTan, ssaMathAtn, ssaMathLog, ssaMathExp,
    ssaMathSqr, ssaMathAbs, ssaMathSgn, ssaMathInt, ssaMathRnd,
    ssaMathLog10, ssaMathLog2, ssaMathLogN,  // Additional log functions
    ssaMathAcos, ssaMathAsin, ssaMathAtan2, ssaMathFix, ssaMathFrac,  // FreeBASIC math
    ssaMathSinh, ssaMathCosh, ssaMathTanh, ssaMathAsinh, ssaMathAcosh, ssaMathAtanh,  // hyperbolic
    // MODERN extensions with an IEEE definition and one WASM instruction each: NaN PROPAGATES through
    // MIN/MAX (IEEE minimum/maximum), ROUND is ties-to-even. See sedaibasickeywords for the why.
    ssaMathCeil, ssaMathRound, ssaMathMin, ssaMathMax, ssaMathCopySign,
    ssaSingleBits, ssaBitsToSingle,   // reinterpret a binary32 as bits and back
    // FreeBASIC date/time (date serial = Double, FPC TDateTime epoch 1899-12-30 = VB/FB serial).
    // Each carries an Immediate selector that picks the concrete function (see SedaiBytecodeTypes).
    ssaDateNow,     // NOW / TIMER -> float (Dest=float; Imm selects)
    ssaDateDecode,  // YEAR/MONTH/DAY/HOUR/MINUTE/SECOND/WEEKDAY(serial) -> int (Dest=int, Src1=float; Imm selects)
    ssaDateSerial,  // DATESERIAL(y,m,d) -> float (Dest=float, Src1=int y, Src2=int m, Immediate=int d reg)
    ssaTimeSerial,  // TIMESERIAL(h,m,s) -> float (Dest=float, Src1=int h, Src2=int m, Immediate=int s reg)
    ssaDateValue,   // DATEVALUE/TIMEVALUE(s) -> float (Dest=float, Src1=string; Imm selects)
    ssaIsDate,      // ISDATE(s) -> int bool (Dest=int, Src1=string)
    ssaDateStr,     // DATE / TIME -> string (Dest=string; Imm selects)
    ssaDateName,    // MONTHNAME/WEEKDAYNAME(n) -> string (Dest=string, Src1=int; Imm selects)
    ssaDateAdd,     // DATEADD(interval$, number, serial) -> float (Dest=float, Src1=string, Src2=int, Immediate=float serial reg)
    ssaDateDiff,    // DATEDIFF(interval$, s1, s2) -> int (Dest=int, Src1=string, Src2=float s1, Immediate=float s2 reg)
    ssaDatePart,    // DATEPART(interval$, serial) -> int (Dest=int, Src1=string, Src2=float serial)
    ssaSetClock,    // SETDATE/SETTIME str (statement; Src1=string; Imm: 0=SETDATE, 1=SETTIME) - side-effecting

    ssaStrDec,  // DEC(hexstring) - convert hex string to decimal
    ssaLabel, ssaJump, ssaJumpIfZero, ssaJumpIfNotZero, ssaCall, ssaReturn,
    ssaCallSub, ssaReturnSub,  // SUB/FUNCTION call frame (M2): Dest=label of procedure entry
    ssaCallSubIndirect,        // FreeBASIC function pointer call: like ssaCallSub but the target entry PC comes from Src1 (an int register), not a static label
    // Argument/result transfer registers (M2): Src1=value reg (store) / Dest=reg (load),
    // Src3=const slot index. Carry args & result across the call frame save/restore.
    ssaXferStoreInt, ssaXferStoreFloat, ssaXferStoreString,
    ssaXferLoadInt, ssaXferLoadFloat, ssaXferLoadString,
    // UDT/record heap (M3): allocate a record (Dest=handle, Src1/2/3=const slot counts) and
    // load/store a field (Dest or Src2 = value, Src1 = handle, Src3 = const slot index).
    ssaRecordNew, ssaRecordNewArray, ssaRecordTypeId, ssaRecordFree,
    ssaRecordNewArrayInd,   // allocate a record per element of a member array by HANDLE: Src1=array-handle reg, Immediate=packed slot counts (like ssaRecordNewArray but the FArrays id comes from a register, for array-of-UDT members)
    ssaRecordNewBlock,      // Callocate(n, SizeOf(T)) of a UDT: allocate N CONSECUTIVE shared records and return the first handle: Dest=first handle, Src1=count reg, Immediate=packed slot counts. "p[i]" = first+i indexes the i-th.
    ssaRecordLoadInt, ssaRecordLoadFloat, ssaRecordLoadString,
    ssaRecordStoreInt, ssaRecordStoreFloat, ssaRecordStoreString,
    // OS threading (M5.2, FreeBASIC API): @sub → entry PC (Dest=int reg, Src1=svkLabel PROC_name);
    // THREADCREATE (Dest=int handle, Src1=procAddr reg, Src2=param reg); THREADWAIT (Src1=handle reg).
    ssaLoadProcAddr, ssaThreadCreate, ssaThreadWait,
    // M5.5: ThreadSelf (Dest=int = current thread handle, 0 on main); ThreadDetach (Src1=handle).
    ssaThreadSelf, ssaThreadDetach,
    ssaFloatRound,   // B1.3: round float -> int (round-to-even), for CINT/CLNG/...
    ssaNarrowInt,    // B1.5: wrap/sign-extend int to a narrower width (Src1=int, Src3=width code)
    ssaNarrowSingle, // B1.5: round Double -> single precision (Dest/Src1 float)
    ssaShl, ssaShr,  // FreeBASIC bit shifts (integer); ssaShr is ARITHMETIC (sign-propagating)
    ssaShrUInt,      // SHR on an unsigned operand (UInteger/ULongInt): zero-filling, like bcDivUInt
    // MODERN bit intrinsics, one opcode per OPERATION with the WIDTH in the immediate (32 or 64):
    // the two widths are the same computation on a different mask, and a single opcode keeps the
    // interpreter, the AOT gate and the WASM backend from ever disagreeing about one of them.
    //   Clz/Ctz/Popcnt: Dest=int, Src1=int, Src3=width.  Rotl/Rotr: + Src2=int rotate count.
    // ⛔ THE WIDTH IS PART OF THE VALUE. They are therefore left OUT of the GVN/CSE purity lists on
    // purpose: those number a value by (opcode, Src1, Src2) and would make COUNTONEBITS(x) and
    // COUNTONEBITS32(x) the same expression. Whoever adds them there must key on Src3 first.
    // (LICM is safe and does list them: hoisting MOVES an instruction whole, immediate included.)
    ssaBitClz, ssaBitCtz, ssaBitPopcnt, ssaBitRotl, ssaBitRotr,
    ssaRandomize,    // RANDOMIZE: seed the RNG (Src1=seed reg, Immediate=1; or Immediate=0 = time-based)
    // Mutexes (M5.4, FB API): MutexCreate (Dest=int handle, no operands); Lock/Unlock/Destroy (Src1=handle reg).
    ssaMutexCreate, ssaMutexLock, ssaMutexUnlock, ssaMutexDestroy,
    // Condition variables (M5.4): CondCreate (Dest=int handle); CondWait (Src1=cond, Src2=mutex);
    // CondSignal/CondBroadcast/CondDestroy (Src1=cond handle).
    ssaCondCreate, ssaCondWait, ssaCondSignal, ssaCondBroadcast, ssaCondDestroy,
    ssaRecMarkPush, ssaRecMarkPop,   // M8: block-scoped record reclamation (loop-body DIMs)

    ssaArrayLoad, ssaArrayStore, ssaArrayDim,
    ssaArrayLBound, ssaArrayUBound,  // LBOUND/UBOUND(arr[, dim]) (B1.4)
    ssaArrayErase, ssaArrayRedim,    // ERASE / REDIM (B1.4)
    ssaArrayRedimPush, ssaArrayRedimN,  // REDIM multi-dim: push an upper bound / commit N dims
    ssaArrayIdxPush, ssaArrayIdxResolve,  // runtime multi-dim index: push an index / resolve to a linear index
    // FreeBASIC pointers: dereference through a runtime "address" = the id of a 1-element backing
    // array (element 0). Load: Dest=value, Src1=address reg. Store: Src1=address reg, Src2=value.
    ssaRefLoadInt, ssaRefLoadFloat, ssaRefLoadString,
    ssaRefStoreInt, ssaRefStoreFloat, ssaRefStoreString,
    ssaRefAddrField,  // @obj.field: pack a record-field pointer (Dest=addr, Src1=handle, Immediate=slot)
    // FreeBASIC raw byte heap (Allocate family).
    ssaRawAlloc, ssaRawFree, ssaRawRealloc,
    ssaRawLoadInt, ssaRawLoadFloat, ssaRawStoreInt, ssaRawStoreFloat,
    ssaRawLoadZStr,    // *p (ZSTRING/WSTRING PTR): Dest(str) = C string at the raw address; Src3 const: 0=bytes, 1=UCS-2
    ssaRawStoreZStr,   // *p = s: write StringRegs bytes + NUL at the raw address; Src3 const: 0=bytes, 1=UCS-2
    ssaRawMemCopy, ssaRawMemMove, ssaRawClear,   // FB_MEMCOPY/FB_MEMMOVE/CLEAR raw-memory block ops
    ssaArrayBind, ssaArrayUnbind,   // array BYREF param: alias/restore a param array slot to a caller's array
    ssaArrayBindApply,              // commit the pending array binds of one call (two-phase: snapshot args, then alias) — Immediate=count
    ssaArrayBindInd,                // same bind, but the ARG is a UDT array member: Src2 = reg holding its runtime FArrays handle
    // UDT array members: the field holds a per-instance FArrays handle; element access is INDIRECT
    // (array id from a register). Src1=handle reg, Src2=linear index reg (load/store); Dest=value.
    ssaArrayLoadIndInt, ssaArrayLoadIndFloat, ssaArrayLoadIndString,
    ssaArrayStoreIndInt, ssaArrayStoreIndFloat, ssaArrayStoreIndString,
    ssaArrayIdxResolveInd,          // linear index from a member array's runtime dims (Src1=handle reg)
    ssaMemberArrayRedim,            // REDIM obj.field(...): Src1=record-handle reg, Immediate=(slot<<8)|(elemType<<4)|preserve
    ssaArrayCopyContents,           // deep-copy array storage between two FArrays handles: Src1=dest handle, Src2=src handle (value-semantics of an array UDT member)
    ssaArrayCopyRecords,            // value-copy an array-of-UDT member element-wise: Src1=dest handle, Src2=src handle, Immediate=packed elem UDT slot counts. Each dest element record gets an independent copy of the src element's contents.
    ssaArrayLBoundInd, ssaArrayUBoundInd,  // LBOUND/UBOUND of a UDT array member (Src1=handle reg, Src2=dim reg)
    ssaPrint, ssaPrintLn, ssaPrintString, ssaPrintStringLn,
    ssaPrintInt, ssaPrintIntLn,
    ssaPrintBool, ssaPrintUInt,   // B1.5 phase C: BOOLEAN true/false, unsigned-64 print
    ssaPrintComma, ssaPrintSemicolon, ssaPrintTab, ssaPrintSpc, ssaPrintNewLine,
    ssaPrintEnd,   // Reset reverse mode after PRINT statement (C128 behavior)
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
    ssaPRst,           // PRST: Reset palette to default
    ssaScnClr,         // SCNCLR [mode]: Clear screen (text or graphics)
    ssaGfxScreenRes,   // SCREENRES w,h (FreeBASIC graphics, via IGraphicsBackend)
    ssaGfxScreenPtr,   // SCREENPTR: raw pointer to the working page's framebuffer
  ssaGfxImageConvertRow,  // IMAGECONVERTROW(src,src_bpp,dst,dst_bpp,width[,isrgb]): convert one pixel row
    ssaGfxDrawGML,     // DRAW "..." : FreeBASIC graphics-macro-language string (Src1 = string)
    // DRAW STRING [img,] (x,y), text [,colour] : text INTO the drawing surface, from the built-in 8x8
    // font. Src1 = the text (STRING reg), Src2 = x, Src3 = y, Immediate[0-15] = colour reg.
    // The image target rides on the existing bcGfxSetTarget pair, exactly as "PSET img,(x,y)" does, so
    // the leading image handle needs nothing of its own here.
    ssaGfxDrawString,
    ssaGfxPointCoord,  // POINTCOORD(n): DRAW pen coordinate (Dest = result, Src1 = selector 0=x/1=y)
    ssaGfxPset,        // PSET (x,y),color
    ssaGfxPoint,       // POINT(x,y) -> color
    ssaGfxPaint,       // PAINT (x,y),color (flood fill)
    ssaGfxLine,        // LINE (x1,y1)-(x2,y2),color[,B|BF] (line / box outline / filled box)
    ssaGfxCircle,      // CIRCLE (x,y),r[,color] (circle via DrawEllipse)
    ssaGfxCircleEx,    // CIRCLE (x,y),r,c,start,end,aspect (ellipse / arc; Src3=RX, Immediate=RY|color|start°|end°)
    ssaGfxPaintBorder, // PAINT (x,y),color,border (boundary fill; Src1=x, Src2=y, Src3=color, PhiSources[0]=border)
    ssaGfxSetTarget,   // set/clear the per-statement image draw target (Src1=handle, Src3=active flag const)
    ssaGfxLineStyled,  // LINE with a style mask (Src1=x1, Src2=y1, Src3=x2, PhiSources y2|color|style|shape)
    ssaGfxPalette,     // PALETTE index, r, g, b : set a palette entry (Src1=index, Src2=packed color)
    ssaGfxPalGet,      // __PALGET(index, which) -> palette component (Dest=result, Src1=index, Immediate=which)
    ssaGfxPaletteReset,// PALETTE (no args) : reset the palette to the mode default
    ssaGfxColor,       // COLOR [fg][,bg] : set current draw foreground/background (Src1=fg, Src2=bg)
    ssaGfxForeColor,   // read the current draw foreground colour (Dest=result) — omitted-colour default
    ssaGfxImageCreate, // IMAGECREATE(w,h[,color]) -> image handle (Dest=handle, Src1=w, Src2=h, Src3=color)
    ssaGfxImageDestroy,// IMAGEDESTROY handle (Src1=handle)
    ssaGfxImageInfo,   // __IMGINFO(handle, which) -> w/h (Dest=result, Src1=handle, Immediate=which)
    ssaGfxGet,         // GET (x1,y1)-(x2,y2),dst : capture screen rect into image dst
    ssaGfxPut,         // PUT (x,y),src[,mode] : blit image src onto screen
    ssaGfxScreenInfo,  // __SCRINFO(which) -> screen w/h/depth/... (Dest=result, Immediate=which)
    ssaGfxScreenSet,   // SCREENSET work[,visible] / FLIP : select work/visible page (Src1=wp, Src2=vp, Immediate=flags)
    ssaGfxPCopy,       // PCOPY src,dst / SCREENCOPY : copy one page onto another (Src1=src, Src2=dst, Immediate=flags)
    ssaGfxWindow,      // WINDOW [SCREEN] (x1,y1)-(x2,y2) : set/clear the logical coordinate transform
    ssaGfxPMap,        // __PMAP(coord, n) -> mapped coordinate (Dest=result, Src1=coord, Immediate=n)
    ssaGfxView,        // VIEW [SCREEN] (x1,y1)-(x2,y2) : set/clear the viewport (offset + clip)
    ssaGfxScreen,      // SCREEN mode[,,num_pages] : set a numbered graphics mode (Src1=mode, Immediate=pages)
    ssaMultikey,       // MULTIKEY(scancode) -> -1 if the key is held, 0 otherwise (Dest=result, Src1=scancode)
    ssaGetmouse,       // GETMOUSE snapshot: query the mouse into the VM cache (Dest=status 0/1); reads via ssaMouseAxis
    ssaMouseAxis,      // __MOUSEAXIS(which) -> cached mouse component (Dest=result, Immediate=which: 0=x,1=y,2=wheel,3=buttons,4=clip)
    ssaSetmouse,       // SETMOUSE x,y,visibility : move/show the mouse (Src1=x, Src2=y, Immediate=visibility reg)
    ssaGetJoystick,    // GETJOYSTICK snapshot: query gaming device Src1=id into the VM cache (Dest=status 0/1)
    ssaJoyBtn,         // __JOYBTN() -> cached joystick button bitmask (Dest=int)
    ssaJoyAxis,        // __JOYAXIS(which) -> cached joystick axis value (Dest=FLOAT, Immediate=which 0..7)
    ssaStick,          // STICK(axis) -> gaming-device axis position 1..200/0 (Dest=int, Src1=axis)
    ssaStrig,          // STRIG(button) -> gaming-device button state -1/0 (Dest=int, Src1=button)
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
    ssaLoadCWDS,       // CWD$: Load current working directory
    ssaLoadEL,         // EL: Load last error line number
    ssaLoadER,         // ER: Load last error code
    ssaLoadERRS,       // ERR$: Load last error message (variable, not function)
    ssaLoadERFN,       // ERFN: Load the name of the procedure in which the last error occurred
    ssaLoadERMN,       // ERMN: Load the name of the module (source file) of the last error
    ssaWInputChars,    // WINPUT(n [, [#]f]): read n wide characters from a file, or from the keyboard
    ssaInputChars,     // INPUT(n [, [#]f]): read n bytes from a file, or from the keyboard
    ssaLoadDS,         // DS: Commodore disk status code (= last file-operation error code)
    ssaLoadDSS,        // DS$: Commodore disk status message line "NN, MESSAGE,00,00"
    ssaLoadST,         // ST: Kernal I/O status byte (bit 6 = end-of-file on the last GET#)
    ssaCsrlin,         // CSRLIN: current text cursor row
    ssaFre,            // FRE(x): Return available memory in bytes
    ssaCpuCount,       // CPUCOUNT / CPUCORES: processors on this machine (Src3 = 0 logical, 1 physical)
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
    ssaPrintUsing,     // PRINT USING format$; values (Src2 = FLOAT value)
    ssaPrintUsingInt,  // PRINT USING with an EXACT integer value (Src2 = INT value): keeps every digit of a LongInt > 2^53
    ssaPrintUsingStage,// stage one already-stringified value for a runtime-format PRINT USING (Src1=string)
    ssaPrintUsingRun,  // run a runtime-format PRINT USING over the staged values (Src1=format string)
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
    ssaSetEnviron,     // SETENVIRON "NAME=value": set an environment variable (Src1 = string)
    ssaShell,          // SHELL cmd: run a command (Src1 = string); Dest (int) = exit code when used as a value
    ssaRmdir,          // RMDIR/RD "path": Remove directory (FreeBASIC/QB)
    ssaMoveFile,       // MOVE/MV "src","dest": Move file
    // Disk file I/O
    ssaDopen,          // DOPEN #handle, "filename" [, mode$]: Open disk file
    ssaOpenFunc,       // FreeBASIC Open(...) as an EXPRESSION: Dest = error code (0 = ok) instead of raising
    ssaDirSearch,      // FreeBASIC DIR: Dest(str) = first (Src3=0) / next (Src3=1) matching entry, "" when done
    ssaDirAttr,        // FreeBASIC DIR: Dest(int) = attributes of the entry ssaDirSearch last returned
    // FreeBASIC variadic arguments (CVA_*).
    ssaVarArgCtl,      // Src3 const: 0 = open a frame (caller, before staging), 1 = close it after the call
    ssaVarArgPushInt,  // stage one surplus argument: Src1 = int value
    ssaVarArgPushFloat,// Src1 = float value
    ssaVarArgPushStr,  // Src1 = string value
    ssaVarArgBase,     // Dest(int) = cursor at the first argument of the current frame (CVA_START)
    ssaVarArgGetInt,   // Dest(int)   = slot at cursor Src1 (CVA_ARG)
    ssaVarArgGetFloat, // Dest(float) = slot at cursor Src1
    ssaVarArgGetStr,
    ssaDclose,         // DCLOSE #handle: Close disk file
    ssaOpen,           // OPEN (legacy C64/C128 style, maps to DOPEN)
    ssaClose,          // CLOSE (legacy C64/C128 style, maps to DCLOSE)
    ssaGetFile,        // GET# file, var: Get char from file
    ssaInputFile,      // INPUT# file, vars: Input from file
    ssaPrintFile,      // PRINT# file, exprs: Print to file
    ssaCmd,            // CMD file [, expr]: Redirect output to file
    ssaAppend,         // APPEND #handle, data: Append data to file
    ssaDclear,         // DCLEAR: Close all open file handles
    ssaRecord,         // RECORD #handle, position: Seek to position in file
    ssaPrintFileNewLine, // PRINT# newline: Write CR to file (handle in Src1)
    ssaPrintFileComma,   // PRINT# comma: pad spaces in the FILE to the next 14-column zone (handle in Src1)
    ssaFileQuery,      // EOF/FREEFILE/LOF/LOC/SEEK(n) -> int (Src1=handle, Src3=query code immediate)
    ssaFileAttr,       // FILEATTR(filenum, returntype) -> int (Src1=handle, Src2=returntype)
    ssaFileSetEof,     // FILESETEOF filenum -> truncate/extend to current position (Src1=handle, Dest=int status)
    ssaAssert,         // ASSERT/ASSERTWARN: if Src1=0 print the message in Src2; Immediate bit0 = halt (Assert)
    ssaSeekSet,        // SEEK #n, pos statement (Src1=handle, Src2=position)
    ssaInputFileLine,  // LINE INPUT# file, string var: read a whole line (Dest=string var, Src1=handle)
    ssaPutBinInt, ssaPutBinFloat,    // PUT #n: write 8 bytes of an int/double (Src1=handle, Src2=value)
    ssaGetBinInt, ssaGetBinFloat,    // GET #n: read 8 bytes into an int/double (Dest=value, Src1=handle)
    ssaPutBinStr, ssaGetBinStr,      // PUT/GET #n: RAW string bytes (value, Src1=handle, Src3=field width: 0 = natural/current length)
    ssaPutBinMem, ssaGetBinMem,      // PUT/GET #n, , *p, n: raw memory block (Src1=handle, Src2=raw ptr, Src3=byte count reg)
    ssaPutBinArray, ssaGetBinArray,  // PUT/GET #n, , a(): whole array (Src1=handle, Src2=array ref, Src3=elem width | bank shl 8)
    ssaPutBinPad, ssaGetBinSkip,     // UDT record image alignment: write/skip Src3 bytes (Src1=handle)
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
    ssaEnd, ssaStop, ssaFast, ssaSlow, ssaSleep, ssaFrame, ssaKey, ssaNop, ssaClear,
    // Debug/Trace
    ssaTron, ssaTroff,  // TRON/TROFF: Enable/disable trace mode
    // Error handling
    ssaTrap,            // TRAP linenum: Set error handler
    ssaResume,          // RESUME: Continue after error at error line
    ssaResumeNext,      // RESUME NEXT: Continue after error at next statement
    ssaOnError,         // ON ERROR GOTO label: Src1 = handler label -> resolved PC in Immediate
    ssaResumeLabel,     // RESUME label: Src1 = target label -> resolved PC in Immediate
    ssaRaiseError,      // ERROR <n>: raise a user runtime error (Src1 = error number)
    // Web operations (WEB_MODE only)
    {$IFDEF WEB_MODE}
    ssaWebGetParam,     // GET$("nome") - HTML-escaped parameter
    ssaWebPostParam,    // POST$("nome") - HTML-escaped parameter
    ssaWebGetRaw,       // GETRAW$("nome") - raw, unsanitized parameter
    ssaWebPostRaw,      // POSTRAW$("nome") - raw, unsanitized parameter
    ssaWebHtmlEncode,   // HTML$(s) - escape HTML entities
    ssaWebUrlEncode,    // URL$(s) - URL encode string
    ssaWebMethod,       // METHOD$ - "GET" or "POST"
    ssaWebPath,         // PATH$ - requested path
    ssaWebQuery,        // QUERY$ - full query string
    ssaWebHeader,       // HEADER$("nome") - request header value
    ssaWebSetHeader,    // SETHEADER "name", "value"
    ssaWebStatus,       // STATUS code - set HTTP status code
    {$ENDIF}
    // Appended at the end so inserting new ops never shifts existing ordinals
    // (keeps incremental builds consistent across units).
    ssaSpriteDef,      // SPRDEF [n]: Enter the interactive sprite editor (sbv only)
    ssaSprsave,        // SPRSAVE "file": save all sprites to a JSON file
    ssaSprload,        // SPRLOAD "file": load all sprites from a JSON file
    ssaSprsize,        // SPRSIZE n, w, h: set sprite dimensions
    ssaSprform,        // SPRFORM n, format: set sprite data format
    ssaConScreen,      // SCREEN(row, col [, colorflag]): a console cell's char code or colour attribute
                       //   (Dest = result, Src1 = row, Src2 = col, Immediate = colorflag register)
    ssaConLocate,      // MODERN LOCATE row, col: position the console TEXT cursor (Src1 = row, Src2 = col,
                       //   both 1-based). CLASSIC LOCATE is ssaGraphicLocate, the pixel cursor.
    ssaConViewPrint,   // VIEW PRINT [first TO last]: the text print area (Src1 = first row, Src2 = last row,
                       //   1-based; 0 in either means "the whole screen")
    // ── BigInt (arbitrary-precision integers), MODERN only ───────────────────
    // A BigInt VALUE is a HANDLE in the int bank, exactly as a UDT instance is;
    // the limbs live in a per-context heap. That is not an implementation
    // detail that leaked: it is the SAME shape the language already has for a
    // managed aggregate, so scoping, frames and the register allocator need no
    // new concept. See job/docs/PIANO_BIGINT.md and src/SedaiBigInt.pas.
    ssaBigNew,         // Dest = a fresh BigInt handle, value 0
    ssaBigFromInt,     // Dest(handle) := Src1 (an Int64 register), sign included
    ssaBigCopy,        // Dest(handle) := Src1(handle), by VALUE (copy-on-write)
    ssaBigToStr,       // Dest(string) := the decimal text of Src1(handle)
    ssaDummy            // Placeholder to avoid trailing comma issues
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
    // B4 range analysis: on ssaArrayLoad/ssaArrayStore, the linear index is PROVEN to lie in
    // [0, TotalSize) for an array whose extent is a compile-time constant that cannot change.
    // Purely an optimization hint: the interpreter still checks (it ignores this), native
    // backends (AOT/JIT) may elide their bounds guard. Set only by SedaiRangeAnalysis.
    BoundsSafe: Boolean;
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
    LowerBounds: array of Integer;      // FreeBASIC "lb TO ub": constant lower bound per dim (0 if none)
    LowerBoundRegisters: array of Integer; // SSA int reg holding a RUNTIME lower bound per dim (-1 = constant)
    ArrayIndex: Integer;                 // Index in VM array table
    // The declared rank is NOT always DimCount: a bare "Dim dyn()" registers ONE dimension and the
    // "ReDim dyn(1 To 3, 4 To 9)" that establishes the real rank never revisits it. This says the name
    // is given more than one dimension SOMEWHERE in the program, which settles it - FreeBASIC refuses
    // to change an array's rank once it has one (measured: error 4 / error 36), so anywhere is
    // everywhere. A compiled backend must not compute UBound natively for such an array: its
    // descriptor carries the total element COUNT, not per-dimension extents.
    MultiDimEver: Boolean;
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
    // Phase A: per-register versionability. Only proc-local scalars (MODERN) are marked; module-level /
    // SHARED / @-taken and everything in CLASSIC stay unmarked -> Version=0 volatile. Indexed [bank][idx].
    FVersionableRegs: array[TSSARegisterType] of array of Boolean;
    // Set by the register allocator when the REGREUSE interference merge actually ran, i.e. when
    // values with disjoint live ranges were given a SHARED register number. The AOT reads it to
    // arbitrate against its own within-block dynamic allocator - see RegisterMergeApplied.
    FRegisterMergeApplied: Boolean;
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
    procedure AddVersionableReg(RegType: TSSARegisterType; RegIndex: Integer);  // Phase A: mark a proc-local scalar
    function IsRegVersionable(RegType: TSSARegisterType; RegIndex: Integer): Boolean;  // Phase A
    procedure ExcludeArrayDescriptorRegsFromVersioning;  // Phase A: dim/bound regs are referenced out-of-band
    function DeclareArray(const ArrName: string; ElementType: TSSARegisterType; const Dims: array of Integer): Integer;
    procedure SetArrayDimRegisters(ArrayIdx: Integer; const DimRegs: array of Integer; const DimRegTypes: array of TSSARegisterType);
    procedure SetArrayLowerBounds(ArrayIdx: Integer; const LowerBounds: array of Integer);
    procedure SetArrayLowerBoundRegisters(ArrayIdx: Integer; const LbRegs: array of Integer);
    function FindArray(const ArrName: string): Integer;
    procedure SetArrayMultiDim(ArrayIdx: Integer);   // mark: this name is multi-dimensional somewhere
    function GetArray(Index: Integer): TSSAArrayInfo;
    function GetArrayCount: Integer;
    procedure BuildDominatorTree;  // PHASE 3 TIER 2: Build dominator tree for optimizations
    procedure ClearDomTree;  // Clear dominator tree (call after CFG modifications like LICM)
    function GetDomTree: TObject;  // PHASE 3 TIER 2: Get dominator tree (cast to TDominatorTree in implementation)
    function RunDBE: Integer;  // Dead block elimination - removes unreachable blocks (returns removed block count)
    procedure RunSSAConstruction;  // PHASE 3: Convert to proper SSA with PHI functions and versioning
    function RunSubInlining: Integer;    // Unification: inline small leaf SUB/FUNCTIONs (IMMEDIATELY after SSA generation, before every other pass)
    function RunRangeAnalysis: Integer;  // B4: prove array accesses in-bounds, set BoundsSafe (AFTER DCE, BEFORE PHI elimination - needs the PHIs)
    procedure RunPhiElimination;  // FINAL PASS: Convert PHI functions to copy instructions (BEFORE bytecode compilation)
    // C7: stamp the constant divisor of `x \ C` / `x Mod C` onto the instruction (Src3), while the
    // register still identifies a value. Called from RunPhiElimination - the one point every
    // pipeline crosses after constant propagation and before register allocation.
    procedure AnnotateDivByConst;
    function RunStringTempFusion: Integer;  // write string results straight into their destination
    function RunConcatCharFusion: Integer;  // "acc + Mid(tab,k,1)" -> one instruction, no substring
    function RunAppendMappedFusion: Integer; // "acc += tab[Asc(Mid(s,i,1))+1]" -> ONE instruction
    function RunConcatDeadSourceMark: Integer;  // mark "s = s + x" whose left operand dies there
    function RunAscMidFusion: Integer;      // Asc(Mid(s,i,n)) without building the substring
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
    { Content fingerprint of the whole program: instruction count plus a hash over every opcode and
      operand. Its ONLY purpose is to answer, per optimization pass, "did this actually change
      anything?" - a question the per-pass TIMINGS cannot answer, because a pass that fires on
      nothing costs the same tenth of a millisecond as one doing real work and looks identical in
      the breakdown. That is exactly how GVN sat in the pipeline being almost inert: it ran, it was
      sound, it was timed, and it was rewriting almost nothing (see the 2026-07-25 work on
      LBOUND/subscript redundancy). Diagnostic only - nothing in the compiler reads it. }
    function Fingerprint(out AInstrCount: Integer): QWord;
    property Blocks: TSSABasicBlockList read FBlocks;
    property Variables: TStringList read FVariables;
    property Labels: TStringList read FLabels;
    property VarRegMap: TStringList read FVarRegMap;  // Access to variable→register mapping (for SSA construction)
    property GlobalVariableSemantics: Boolean read FGlobalVariableSemantics write FGlobalVariableSemantics;
    // True once the REGREUSE merge has given values with disjoint live ranges a shared register
    // number. The AOT's within-block dynamic allocator (AOT_DYNF) is ANTAGONISTIC to it, not
    // additive: DYNF only admits block-local single-def temps and holds an xmm for
    // [firstDef..lastTouch] without ever evicting, while a merged register is multi-def and
    // long-lived - so it pins a machine register for the whole block and starves the rest.
    // Measured (--aot, best-of-3, ms): n-body static 639 / DYNF 399 / merge 360 / BOTH 699;
    // floatpoly 491 / 346 / 255 / 660; arraysum 556 / 484 / 337 / 727. "Both" is always the worst.
    property RegisterMergeApplied: Boolean read FRegisterMergeApplied write FRegisterMergeApplied;
  end;

{ SSA string pool (see the TSSAValue comment). Interning an empty string is id 0; ids are
  append-only and process-wide (deduplicated, so REPL re-compiles do not grow it unboundedly). }
function SSAPoolIntern(const S: string): Integer;
function SSAPoolGet(Id: Integer): string;

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
// Membership test over an open array of opcodes. Replaces `Op in [..]`: TSSAOpCode now has
// more than 256 members, so `set of TSSAOpCode` (which an `in [..]` literal builds) is no longer
// a legal type in FPC. An open-array constructor has no such limit.
function OpIn(const Op: TSSAOpCode; const Ops: array of TSSAOpCode): Boolean;
// FreeBASIC SHR: on a SIGNED operand the sign bit is copied into the vacated high bits, so
// "-5 Shr 2" is -2 (manual, Operator Shr). FPC's own "shr" on an Int64 is a LOGICAL shift, which
// turns every negative value into a huge positive one -- so it cannot implement the signed case.
// An UNSIGNED operand (UInteger/ULongInt) does shift logically, hence the second helper.
function ArithShr64(V, Shift: Int64): Int64;
function LogicalShr64(V, Shift: Int64): Int64;
// MODERN bit intrinsics (COUNTLEADINGZEROS/.../ROTATERIGHT32). Width is 32 or 64; the zero case and
// the rotate's modulo follow WebAssembly, which is the point of having them. See the implementation.
function BitClz(V: Int64; Width: Int64): Int64;
function BitCtz(V: Int64; Width: Int64): Int64;
function BitPopcnt(V: Int64; Width: Int64): Int64;
function BitRotl(V, Count: Int64; Width: Int64): Int64;
function BitRotr(V, Count: Int64; Width: Int64): Int64;

implementation

uses TypInfo, SedaiDominators, SedaiSSAConstruction, SedaiPhiElimination, SedaiGVN, SedaiCSE, SedaiCopyProp,
     SedaiAlgebraic, SedaiStrengthReduction, SedaiGosubInlining, SedaiConstProp, SedaiConstPropAggressive,
     SedaiDBE, SedaiDCE, SedaiLICM, SedaiLoopUnroll, SedaiCopyCoalescing, SedaiRangeAnalysis,
     SedaiSubInlining
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
  BoundsSafe := False;
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
  Result.BoundsSafe := BoundsSafe;
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
// The label map stores an INDEX into FBlocks, and blocks get REMOVED (DBE, DCE, inlining). A stale
// index either walks off the end - "List index out of bounds", which is how this was found - or,
// far worse, lands on a live block that is not the one asked for and hands it back in silence.
// So the index is verified against the label before it is trusted, and a mismatch falls back to a
// scan; only a label that names no live block returns nil.
var
  Idx, i: Integer;
  B: TSSABasicBlock;
begin
  Result := nil;
  Idx := FLabels.IndexOf(LabelName);
  if Idx < 0 then Exit;
  i := PtrInt(FLabels.Objects[Idx]);
  if (i >= 0) and (i < FBlocks.Count) then
  begin
    B := TSSABasicBlock(FBlocks[i]);
    if Assigned(B) and (B.LabelName = LabelName) then Exit(B);
  end;
  for i := 0 to FBlocks.Count - 1 do
  begin
    B := TSSABasicBlock(FBlocks[i]);
    if Assigned(B) and (B.LabelName = LabelName) then
    begin
      FLabels.Objects[Idx] := TObject(PtrInt(i));   // repair the map on the way out
      Exit(B);
    end;
  end;
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

procedure TSSAProgram.AddVersionableReg(RegType: TSSARegisterType; RegIndex: Integer);
var
  OldLen, NewLen, i: Integer;
begin
  // Phase A: mark (bank, idx) as a proc-local scalar eligible for SSA versioning (MODERN only).
  if RegIndex < 0 then Exit;
  OldLen := Length(FVersionableRegs[RegType]);
  if RegIndex >= OldLen then
  begin
    NewLen := (RegIndex + 1) * 2;
    SetLength(FVersionableRegs[RegType], NewLen);
    for i := OldLen to NewLen - 1 do
      FVersionableRegs[RegType][i] := False;
  end;
  FVersionableRegs[RegType][RegIndex] := True;
end;

function TSSAProgram.IsRegVersionable(RegType: TSSARegisterType; RegIndex: Integer): Boolean;
begin
  Result := (RegIndex >= 0) and (RegIndex < Length(FVersionableRegs[RegType])) and
            FVersionableRegs[RegType][RegIndex];
end;

procedure TSSAProgram.ExcludeArrayDescriptorRegsFromVersioning;
var
  a, i, idx: Integer;
  rt: TSSARegisterType;
begin
  // Phase A: an array descriptor stores the SSA register numbers of its runtime dimension sizes and
  // lower bounds (DimRegisters/LowerBoundRegisters). Those registers are read out-of-band by the array
  // machinery (not through SSA dataflow), so if versioning splits them the descriptor ends up pointing
  // at a base-version register that is never written -> bounds/sizes read as 0. Keep them Version=0.
  for a := 0 to High(FArrays) do
  begin
    for i := 0 to High(FArrays[a].DimRegisters) do
    begin
      idx := FArrays[a].DimRegisters[i];
      if (idx >= 0) and (i <= High(FArrays[a].DimRegTypes)) then
      begin
        rt := FArrays[a].DimRegTypes[i];
        if idx < Length(FVersionableRegs[rt]) then
          FVersionableRegs[rt][idx] := False;
      end;
    end;
    for i := 0 to High(FArrays[a].LowerBoundRegisters) do
    begin
      idx := FArrays[a].LowerBoundRegisters[i];
      if (idx >= 0) and (idx < Length(FVersionableRegs[srtInt])) then
        FVersionableRegs[srtInt][idx] := False;
    end;
  end;
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

procedure TSSAProgram.SetArrayLowerBounds(ArrayIdx: Integer; const LowerBounds: array of Integer);
// FreeBASIC "lb TO ub": record each dimension's constant lower bound (0 = default, no adjustment).
var
  i: Integer;
begin
  if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) then
    raise Exception.CreateFmt('Invalid array index: %d', [ArrayIdx]);
  SetLength(FArrays[ArrayIdx].LowerBounds, Length(LowerBounds));
  for i := 0 to High(LowerBounds) do
    FArrays[ArrayIdx].LowerBounds[i] := LowerBounds[i];
end;

procedure TSSAProgram.SetArrayMultiDim(ArrayIdx: Integer);
begin
  if (ArrayIdx >= 0) and (ArrayIdx <= High(FArrays)) then FArrays[ArrayIdx].MultiDimEver := True;
end;

procedure TSSAProgram.SetArrayLowerBoundRegisters(ArrayIdx: Integer; const LbRegs: array of Integer);
// FreeBASIC "lb TO ub" with a RUNTIME lb (e.g. "Dim a(Lbound(m) To Ubound(m))"): record the SSA int
// register that holds each dimension's lower bound (-1 = the lower bound is a compile-time constant).
var
  i: Integer;
begin
  if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) then
    raise Exception.CreateFmt('Invalid array index: %d', [ArrayIdx]);
  SetLength(FArrays[ArrayIdx].LowerBoundRegisters, Length(LbRegs));
  for i := 0 to High(LbRegs) do
    FArrays[ArrayIdx].LowerBoundRegisters[i] := LbRegs[i];
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
  // BASIC has global variable semantics (modifications in GOSUB persist after RETURN).
  // Phase A: the generator sets FGlobalVariableSemantics per dialect (True=CLASSIC, False=MODERN).
  SSAConstr := TSSAConstruction.Create(Self, TDominatorTree(FDomTreeObj), FGlobalVariableSemantics);
  try
    SSAConstr.Run;
  finally
    SSAConstr.Free;
  end;
  //WriteLn;
end;

function TSSAProgram.RunSubInlining: Integer;
var
  Inliner: TSubInliner;
begin
  { Unification: flatten small leaf SUB/FUNCTION calls before anything else runs,
    so the clones are versioned/optimized like inline code and every engine
    (interpreter, JIT, AOT) sees call-free hot paths. Gated on the optimizer
    switch: a bug here shows up in the opt-vs-no-opt differential net. }
  Result := 0;
  if not GSSAOptimizationsEnabled then Exit;
  Inliner := TSubInliner.Create(Self);
  try
    Result := Inliner.Run;
  finally
    Inliner.Free;
  end;
end;

function TSSAProgram.RunRangeAnalysis: Integer;
var
  RA: TRangeAnalysis;
begin
  { B4 bounds-check elimination: prove array accesses in-bounds and set the
    BoundsSafe hint on ssaArrayLoad/ssaArrayStore. Changes no instruction
    stream. Must run AFTER the transforming passes (positions are final) and
    BEFORE PHI elimination (the induction-variable proof needs the PHIs).
    The interpreter ignores the flag - only the AOT/JIT backends consume it. }
  Result := 0;
  if not GSSAOptimizationsEnabled then Exit;
  BuildDominatorTree;   // CFG may have changed since the last build (LICM etc.)
  if not FDomTreeValid then Exit;
  RA := TRangeAnalysis.Create(Self);
  try
    Result := RA.Run;
  finally
    RA.Free;
  end;
end;

function WritesSSAReg(Ins: TSSAInstruction; RT: TSSARegisterType; Reg: Integer): Boolean;
// Does this instruction WRITE that register? Dest is the write field; the store family also reads
// Dest, but for "is the value still the one the Mid saw" a write is what matters.
begin
  Result := (Ins.Dest.Kind = svkRegister) and (Ins.Dest.RegType = RT) and (Ins.Dest.RegIndex = Reg);
end;

function TouchesStrReg(Ins: TSSAInstruction; Reg: Integer): Boolean;
// Does this instruction read OR write string register Reg, in any field? Deliberately blunt: it
// answers "leave this alone" for anything that so much as mentions the register, including PHI
// sources. Being over-eager here only declines a fusion; being under-eager would move a write
// across something that observes it.
var
  k: Integer;

  function Hits(const V: TSSAValue): Boolean;
  begin
    Result := (V.Kind = svkRegister) and (V.RegType = srtString) and (V.RegIndex = Reg);
  end;

begin
  Result := Hits(Ins.Dest) or Hits(Ins.Src1) or Hits(Ins.Src2) or Hits(Ins.Src3);
  if Result then Exit;
  for k := 0 to High(Ins.PhiSources) do
    if Hits(Ins.PhiSources[k].Value) then Exit(True);
end;

function TSSAProgram.RunAscMidFusion: Integer;
// Rewrite "T = Mid(s, start, len)" + "D = Asc(T)" into the single "D = AscMid(s, start, len)".
//
// Reading ONE character was allocating a one-character string to hold it, and measurement says the
// allocation is the whole cost of a string primitive here: Mid of 1 char and Mid of 128 cost the
// same. It is the hot loop of reverse-complement ("Asc(Mid(s, i, 1))") and of k-nucleotide, and
// FreeBASIC's own "s[i]" lowers to exactly this pair too, so both forms gain.
//
// Same safety rule as the temp fusion: the substring must have ONE definition and ONE use, so
// nothing else can observe it, and both must be in the same block with nothing touching the
// destination in between. bcStrAscMid takes bcStrMid's operands unchanged - the answer is the FIRST
// byte of the substring, so the length still matters (it decides whether the substring is empty)
// and no assumption that it equals 1 is needed.
var
  b, i, k, k2, T, NUses, Last: Integer;
  Ok, AllAsc: Boolean;
  Blk: TSSABasicBlock;
  Prod, Cons, Mid: TSSAInstruction;
  DefCount, UseCount: array of Integer;

  procedure Bump(var Arr: array of Integer; const V: TSSAValue);
  begin
    if (V.Kind = svkRegister) and (V.RegType = srtString) and
       (V.RegIndex >= 0) and (V.RegIndex <= High(Arr)) then
      Inc(Arr[V.RegIndex]);
  end;

begin
  Result := 0;
  SetLength(DefCount, FNextRegister[srtString] + 1);
  SetLength(UseCount, FNextRegister[srtString] + 1);
  for i := 0 to High(DefCount) do begin DefCount[i] := 0; UseCount[i] := 0; end;
  for b := 0 to Blocks.Count - 1 do
  begin
    Blk := TSSABasicBlock(Blocks[b]);
    for i := 0 to Blk.Instructions.Count - 1 do
    begin
      Prod := TSSAInstruction(Blk.Instructions[i]);
      Bump(DefCount, Prod.Dest);
      Bump(UseCount, Prod.Src1);
      Bump(UseCount, Prod.Src2);
      Bump(UseCount, Prod.Src3);
      for k := 0 to High(Prod.PhiSources) do
        Bump(UseCount, Prod.PhiSources[k].Value);
      if Prod.OpCode in [ssaArrayStoreIndString, ssaRecordStoreString, ssaXferStoreString] then
        Bump(UseCount, Prod.Dest);
    end;
  end;

  for b := 0 to Blocks.Count - 1 do
  begin
    Blk := TSSABasicBlock(Blocks[b]);
    i := 0;
    while i < Blk.Instructions.Count - 1 do
    begin
      Prod := TSSAInstruction(Blk.Instructions[i]);
      if (Prod.OpCode = ssaStrMid) and
         (Prod.Dest.Kind = svkRegister) and (Prod.Dest.RegType = srtString) and
         (Prod.Dest.RegIndex >= 0) and (Prod.Dest.RegIndex <= High(DefCount)) and
         (DefCount[Prod.Dest.RegIndex] = 1) and (UseCount[Prod.Dest.RegIndex] >= 1) then
      begin
        T := Prod.Dest.RegIndex;
        // Collect EVERY consumer of the substring in this block, and require that they are ALL Asc.
        // GVN merges identical Mid() calls, so one temporary commonly feeds several Asc - insisting
        // on a single use declined exactly the shapes this exists for. Rewriting all of them is just
        // as sound: each one only ever wanted the first byte.
        NUses := 0; AllAsc := True; Last := -1;
        for k := i + 1 to Blk.Instructions.Count - 1 do
        begin
          Cons := TSSAInstruction(Blk.Instructions[k]);
          if TouchesStrReg(Cons, T) then
          begin
            Inc(NUses);
            Last := k;
            if not ((Cons.OpCode = ssaStrAsc) and (Cons.Src1.Kind = svkRegister) and
                    (Cons.Src1.RegType = srtString) and (Cons.Src1.RegIndex = T)) then
            begin
              AllAsc := False;
              Break;
            end;
          end;
        end;
        // Every use program-wide must be accounted for here, or a consumer in another block would be
        // left reading a substring nobody builds any more.
        if AllAsc and (NUses > 0) and (NUses = UseCount[T]) then
        begin
          // The Mid's operands have to still hold their values at each Asc, so nothing in between
          // may WRITE the source string, the start or the length.
          Ok := True;
          for k2 := i + 1 to Last do
          begin
            Mid := TSSAInstruction(Blk.Instructions[k2]);
            if (Prod.Src1.Kind = svkRegister) and WritesSSAReg(Mid, Prod.Src1.RegType, Prod.Src1.RegIndex) then begin Ok := False; Break; end;
            if (Prod.Src2.Kind = svkRegister) and WritesSSAReg(Mid, Prod.Src2.RegType, Prod.Src2.RegIndex) then begin Ok := False; Break; end;
            if (Prod.Src3.Kind = svkRegister) and WritesSSAReg(Mid, Prod.Src3.RegType, Prod.Src3.RegIndex) then begin Ok := False; Break; end;
          end;
          if Ok then
          begin
            for k := i + 1 to Blk.Instructions.Count - 1 do
            begin
              Cons := TSSAInstruction(Blk.Instructions[k]);
              if (Cons.OpCode = ssaStrAsc) and (Cons.Src1.Kind = svkRegister) and
                 (Cons.Src1.RegType = srtString) and (Cons.Src1.RegIndex = T) then
              begin
                Cons.OpCode := ssaStrAscMid;
                Cons.Src1 := Prod.Src1;
                Cons.Src2 := Prod.Src2;
                Cons.Src3 := Prod.Src3;
                Inc(Result);
              end;
            end;
            // ⛔ NO explicit Free here: Instructions is a TFPGObjectList created with OwnsObjects,
            // so Delete has already destroyed the instruction. Freeing it again raised an access
            // violation that the caller's "except end" swallowed, killing the pass after its FIRST
            // fusion -- which is why further Asc(Mid()) sites looked like they "resisted" fusion.
            Blk.Instructions.Delete(i);
            Continue;
          end;
        end;
      end;
      Inc(i);
    end;
  end;
end;

function TSSAProgram.RunConcatDeadSourceMark: Integer;
// Mark every StrConcat whose LEFT operand is dead immediately after it, so the VM can take that
// operand's buffer over instead of building a new string.
//
// "outLine += Mid(...)" lowers to "acc_new = concat(acc_old, ch)" plus the copies that close the
// loop-carried PHI, so Dest is never Src1 and the accumulator is REBUILT on every character:
// quadratic within the line, plus one allocation per character. AppendString already grows a string
// in place, but it can only fire when Dest = Src1 -- which needs the copies coalesced away, and that
// is the class of pass that miscompiles here (see copycoal-miscompile).
//
// ⭐ This gets the same effect WITHOUT touching register assignment: if the old accumulator is dead
// after the concatenation, the VM may steal its buffer (Dest := Src1; Src1 := ''), leaving that
// buffer unshared and letting the append run in place. Nothing about which register holds what
// changes, so the miscompile class the coalescer has cannot appear here.
//
// Liveness is the real thing, not a use count: STR[65]_4 in reverse-complement IS read again after
// the loop, yet it is dead at the concatenation because every path back to the loop redefines it
// first. A "used exactly once" test answers the wrong question and would refuse the one shape that
// matters. So: classic backward fixpoint over the CFG, restricted to the string bank.
//
// The mark travels as Src3 = ConstInt(-1), which the bytecode compiler lowers into Immediate. An
// unmarked instruction leaves Immediate at 0, so the VM's default is the safe, copying path
// (absent-operand-lowers-to-r0: an absent operand reads as 0, and 0 must mean "no").
var
  b, i, k, PassNo, Key: Integer;
  Blk: TSSABasicBlock;
  Ins: TSSAInstruction;
  Changed: Boolean;
  MaxVer, VStride, NSlots: Integer;
  LiveIn, LiveOut, Live: array of Boolean;    // per (register, version) slot
  BlockLiveIn, BlockLiveOut: array of array of Boolean;

  function KeyOf(const V: TSSAValue): Integer;
  begin
    if (V.Kind = svkRegister) and (V.RegType = srtString) and
       (V.RegIndex >= 0) and (V.Version >= 0) and (V.Version <= MaxVer) then
      Result := V.RegIndex * VStride + V.Version
    else
      Result := -1;
  end;

  // Every string operand this instruction READS. Dest counts as a read for the stores that carry
  // their value there -- treating one of those as a pure write would call a live value dead.
  procedure MarkReads(P: TSSAInstruction; var S: array of Boolean);
  var q, K2: Integer;
  begin
    K2 := KeyOf(P.Src1); if (K2 >= 0) and (K2 <= High(S)) then S[K2] := True;
    K2 := KeyOf(P.Src2); if (K2 >= 0) and (K2 <= High(S)) then S[K2] := True;
    K2 := KeyOf(P.Src3); if (K2 >= 0) and (K2 <= High(S)) then S[K2] := True;
    for q := 0 to High(P.PhiSources) do
    begin
      K2 := KeyOf(P.PhiSources[q].Value);
      if (K2 >= 0) and (K2 <= High(S)) then S[K2] := True;
    end;
    if P.OpCode in [ssaArrayStoreIndString, ssaRecordStoreString, ssaXferStoreString] then
    begin
      K2 := KeyOf(P.Dest); if (K2 >= 0) and (K2 <= High(S)) then S[K2] := True;
    end;
  end;

begin
  Result := 0;
  // ⛔ DEFAULT OFF (STRDEADSRC=1 to enable), and the measurement says why. The mark is CORRECT -- it
  // fires on 4 concatenations in reverse-complement -- but on its own it buys nothing, because the
  // buffer it lets the VM steal is shared by THREE registers, not one: the copies that close the
  // loop-carried PHI alias it, so the refcount is 3, the steal brings it to 2, and AppendString only
  // grows in place at 1. It becomes useful the day those copies are coalesced away; until then it
  // would only cost a liveness fixpoint at compile time.
  if GetEnvironmentVariable('STRDEADSRC') <> '1' then Exit;

  MaxVer := 0;
  for b := 0 to Blocks.Count - 1 do
  begin
    Blk := TSSABasicBlock(Blocks[b]);
    for i := 0 to Blk.Instructions.Count - 1 do
    begin
      Ins := TSSAInstruction(Blk.Instructions[i]);
      if (Ins.Dest.Kind = svkRegister) and (Ins.Dest.Version > MaxVer) then MaxVer := Ins.Dest.Version;
      if (Ins.Src1.Kind = svkRegister) and (Ins.Src1.Version > MaxVer) then MaxVer := Ins.Src1.Version;
      if (Ins.Src2.Kind = svkRegister) and (Ins.Src2.Version > MaxVer) then MaxVer := Ins.Src2.Version;
      if (Ins.Src3.Kind = svkRegister) and (Ins.Src3.Version > MaxVer) then MaxVer := Ins.Src3.Version;
      for k := 0 to High(Ins.PhiSources) do
        if (Ins.PhiSources[k].Value.Kind = svkRegister) and (Ins.PhiSources[k].Value.Version > MaxVer) then
          MaxVer := Ins.PhiSources[k].Value.Version;
    end;
  end;
  VStride := MaxVer + 1;
  NSlots := (FNextRegister[srtString] + 1) * VStride;
  if NSlots <= 0 then Exit;

  SetLength(BlockLiveIn, Blocks.Count);
  SetLength(BlockLiveOut, Blocks.Count);
  for b := 0 to Blocks.Count - 1 do
  begin
    SetLength(BlockLiveIn[b], NSlots);
    SetLength(BlockLiveOut[b], NSlots);
  end;
  SetLength(LiveIn, NSlots);
  SetLength(LiveOut, NSlots);
  SetLength(Live, NSlots);

  // Backward fixpoint: LiveOut[b] = union of LiveIn[successors]; LiveIn[b] = reads before writes.
  // ⚠️ A block whose successors are not all in this program (an unresolved jump) would need its
  // LiveOut treated as "everything live"; here successors are always inside, and anything missing
  // only ever makes the answer MORE conservative, never less.
  PassNo := 0;
  repeat
    Changed := False;
    Inc(PassNo);
    for b := Blocks.Count - 1 downto 0 do
    begin
      Blk := TSSABasicBlock(Blocks[b]);
      for i := 0 to NSlots - 1 do LiveOut[i] := False;
      for k := 0 to Blk.Successors.Count - 1 do
      begin
        i := TSSABasicBlock(Blk.Successors[k]).BlockIndex;
        if (i >= 0) and (i < Blocks.Count) then
          for Key := 0 to NSlots - 1 do
            if BlockLiveIn[i][Key] then LiveOut[Key] := True;
      end;
      for i := 0 to NSlots - 1 do LiveIn[i] := LiveOut[i];
      // Walk the block backwards: a definition kills, a read revives.
      for i := Blk.Instructions.Count - 1 downto 0 do
      begin
        Ins := TSSAInstruction(Blk.Instructions[i]);
        Key := KeyOf(Ins.Dest);
        if (Key >= 0) and (Key < NSlots) and
           not (Ins.OpCode in [ssaArrayStoreIndString, ssaRecordStoreString, ssaXferStoreString]) then
          LiveIn[Key] := False;
        MarkReads(Ins, LiveIn);
      end;
      for i := 0 to NSlots - 1 do
      begin
        if BlockLiveOut[b][i] <> LiveOut[i] then begin BlockLiveOut[b][i] := LiveOut[i]; Changed := True; end;
        if BlockLiveIn[b][i] <> LiveIn[i] then begin BlockLiveIn[b][i] := LiveIn[i]; Changed := True; end;
      end;
    end;
  until (not Changed) or (PassNo > 50);

  // Second walk: at each StrConcat, is Src1 live AFTER it?
  for b := 0 to Blocks.Count - 1 do
  begin
    Blk := TSSABasicBlock(Blocks[b]);
    for i := 0 to NSlots - 1 do Live[i] := BlockLiveOut[b][i];
    for i := Blk.Instructions.Count - 1 downto 0 do
    begin
      Ins := TSSAInstruction(Blk.Instructions[i]);
      if (Ins.OpCode = ssaStrConcat) and (Ins.Src3.Kind = svkNone) then
      begin
        Key := KeyOf(Ins.Src1);
        // Src1 dead after this point, and genuinely a different register from Dest and Src2 (stealing
        // a buffer that Src2 also names would corrupt the right-hand operand mid-append).
        if (Key >= 0) and (Key < NSlots) and (not Live[Key]) and
           (KeyOf(Ins.Dest) <> Key) and (KeyOf(Ins.Src2) <> Key) then
        begin
          Ins.Src3 := MakeSSAConstInt(-1);
          Inc(Result);
        end;
      end;
      Key := KeyOf(Ins.Dest);
      if (Key >= 0) and (Key < NSlots) and
         not (Ins.OpCode in [ssaArrayStoreIndString, ssaRecordStoreString, ssaXferStoreString]) then
        Live[Key] := False;
      MarkReads(Ins, Live);
    end;
  end;
  if GetEnvironmentVariable('STRFUSE_DIAG') <> '' then
    WriteLn(ErrOutput, Format('[DEADSRC] marked %d concatenations whose left operand dies there (of %d blocks, %d slots, %d fixpoint passes)',
      [Result, Blocks.Count, NSlots, PassNo]));
end;

function TSSAProgram.RunConcatCharFusion: Integer;
// Fuse "T = Mid(tab, k, 1)" + "D = Concat(acc, T)" into "D = ConcatCharAt(acc, tab, k)".
//
// "acc += Mid(tab, k, 1)" is the inner line of reverse-complement, and it pays for a whole string to
// carry ONE byte: measured against the same loop with the character already in hand, building it
// costs about 49 ns of the 87 the line takes. bcStrConcatCharAt reads the byte straight out of the
// table, and when the destination is the accumulator it grows it in place instead of rebuilding it.
//
// The length must be exactly 1: that is what lets the fused arm skip bcStrMid's negative-length
// rules. A literal 1 is accepted, and so is an int register whose ONLY definition is LoadConstInt 1
// -- which is the form that actually reaches here, since the constant is materialised into a
// register long before this pass runs.
//
// Safety, as in the two fusions above: the substring must have exactly one definition and one use
// (the concatenation), both in the same block, and nothing in between may write the operands the
// fused instruction will read LATER than the original did.
var
  b, i, k, k2, T: Integer;
  Ok: Boolean;
  Blk: TSSABasicBlock;
  Prod, Cons, Mid: TSSAInstruction;
  DefCount, UseCount: array of Integer;
  IntDefs: array of Integer;      // (int reg, version) -> count of definitions
  IntConst: array of Int64;       // ...and the value, when that single definition is LoadConstInt
  MaxVer, VStride: Integer;
  ForceFuse: string;              // STRCHARFUSE: '1' forces on, '0' forces off, empty follows the AOT

  function StrKey(const V: TSSAValue): Integer;
  begin
    if (V.Kind = svkRegister) and (V.RegType = srtString) and
       (V.RegIndex >= 0) and (V.Version >= 0) and (V.Version <= MaxVer) then
      Result := V.RegIndex * VStride + V.Version
    else
      Result := -1;
  end;

  function IntKey(const V: TSSAValue): Integer;
  begin
    if (V.Kind = svkRegister) and (V.RegType = srtInt) and
       (V.RegIndex >= 0) and (V.Version >= 0) and (V.Version <= MaxVer) then
      Result := V.RegIndex * VStride + V.Version
    else
      Result := -1;
  end;

  procedure BumpStr(var Arr: array of Integer; const V: TSSAValue);
  var Key: Integer;
  begin
    Key := StrKey(V);
    if (Key >= 0) and (Key <= High(Arr)) then Inc(Arr[Key]);
  end;

  // Is this operand the literal 1, directly or through a register defined exactly once by
  // LoadConstInt 1?
  function IsLengthOne(const V: TSSAValue): Boolean;
  var Key: Integer;
  begin
    if V.Kind = svkConstInt then Exit(V.ConstInt = 1);
    Key := IntKey(V);
    Result := (Key >= 0) and (Key <= High(IntDefs)) and (IntDefs[Key] = 1) and (IntConst[Key] = 1);
  end;

begin
  Result := 0;
  // ON WHEN THE AOT IS, off otherwise -- and the two measurements say exactly why.
  //
  // The first attempt at this fusion measured SLOWER and was left off, on the reading that
  // "AssignSubstr already reuses the temporary's buffer, so the Mid we remove was not allocating".
  // That reading was wrong in the shape that matters: in "acc += Mid(tab, k, 1)" the temporary is
  // written by "StrMid Rt, Rt" -- destination and source are the SAME register, the table's own --
  // so AssignSubstr cannot take its reuse path and falls back to Copy, allocating once per
  // character. What actually kept the fusion from paying was that ssaStrConcatCharAt was missing
  // from OpIsMergeSafe: every register it touched was pinned, so Dest never became Src1 and the
  // opcode ran its allocating arm. With that fixed the accumulator grows in place.
  //
  // 📊 Interleaved A/B, one binary, quiet machine, output byte-identical, control (n-body) -3,3%:
  //   reverse-complement --aot  321 -> 223 ms = -30,5%   (no overlap across three rounds)
  //   reverse-complement interp 442 -> 466 ms = +5,4%    (best of five, no overlap either)
  // The interpreter pays because the opcode lives in the superinstruction group and so costs a call
  // into ExecuteSuperinstruction, which is more than the character it saves; the AOT calls its
  // helper directly and keeps the whole gain. So the fusion follows the AOT rather than splitting
  // the difference: a profile that would lose by it never sees it.
  //
  // STRCHARFUSE=1 forces it on regardless (that is how the interpreter number above was measured),
  // STRCHARFUSE=0 forces it off. ⚠️ A .basc forfeits the AOT anyway, so compiling ahead of time
  // leaves it out, which is the right default for a file that will run interpreted.
  ForceFuse := GetEnvironmentVariable('STRCHARFUSE');
  if ForceFuse = '0' then Exit;
  if (ForceFuse <> '1') and (not GAotWillRun) then Exit;
  MaxVer := 0;
  for b := 0 to Blocks.Count - 1 do
  begin
    Blk := TSSABasicBlock(Blocks[b]);
    for i := 0 to Blk.Instructions.Count - 1 do
    begin
      Prod := TSSAInstruction(Blk.Instructions[i]);
      if (Prod.Dest.Kind = svkRegister) and (Prod.Dest.Version > MaxVer) then MaxVer := Prod.Dest.Version;
      if (Prod.Src1.Kind = svkRegister) and (Prod.Src1.Version > MaxVer) then MaxVer := Prod.Src1.Version;
      if (Prod.Src2.Kind = svkRegister) and (Prod.Src2.Version > MaxVer) then MaxVer := Prod.Src2.Version;
      if (Prod.Src3.Kind = svkRegister) and (Prod.Src3.Version > MaxVer) then MaxVer := Prod.Src3.Version;
      for k := 0 to High(Prod.PhiSources) do
        if (Prod.PhiSources[k].Value.Kind = svkRegister) and (Prod.PhiSources[k].Value.Version > MaxVer) then
          MaxVer := Prod.PhiSources[k].Value.Version;
    end;
  end;
  VStride := MaxVer + 1;

  SetLength(DefCount, (FNextRegister[srtString] + 1) * VStride);
  SetLength(UseCount, (FNextRegister[srtString] + 1) * VStride);
  for i := 0 to High(DefCount) do begin DefCount[i] := 0; UseCount[i] := 0; end;
  SetLength(IntDefs, (FNextRegister[srtInt] + 1) * VStride);
  SetLength(IntConst, Length(IntDefs));
  for i := 0 to High(IntDefs) do begin IntDefs[i] := 0; IntConst[i] := 0; end;

  for b := 0 to Blocks.Count - 1 do
  begin
    Blk := TSSABasicBlock(Blocks[b]);
    for i := 0 to Blk.Instructions.Count - 1 do
    begin
      Prod := TSSAInstruction(Blk.Instructions[i]);
      BumpStr(DefCount, Prod.Dest);
      BumpStr(UseCount, Prod.Src1);
      BumpStr(UseCount, Prod.Src2);
      BumpStr(UseCount, Prod.Src3);
      for k := 0 to High(Prod.PhiSources) do
        BumpStr(UseCount, Prod.PhiSources[k].Value);
      if Prod.OpCode in [ssaArrayStoreIndString, ssaRecordStoreString, ssaXferStoreString] then
        BumpStr(UseCount, Prod.Dest);
      // Track integer definitions, so a length held in a register can still be recognised as 1.
      k2 := IntKey(Prod.Dest);
      if (k2 >= 0) and (k2 <= High(IntDefs)) then
      begin
        Inc(IntDefs[k2]);
        if (Prod.OpCode = ssaLoadConstInt) and (Prod.Src1.Kind = svkConstInt) then
          IntConst[k2] := Prod.Src1.ConstInt
        else
          IntConst[k2] := MaxInt;   // defined by something else: never equal to 1
      end;
    end;
  end;

  for b := 0 to Blocks.Count - 1 do
  begin
    Blk := TSSABasicBlock(Blocks[b]);
    i := 0;
    while i < Blk.Instructions.Count - 1 do
    begin
      Prod := TSSAInstruction(Blk.Instructions[i]);
      T := -1;
      if (Prod.OpCode = ssaStrMid) and (Prod.Dest.Kind = svkRegister) and
         (Prod.Dest.RegType = srtString) and IsLengthOne(Prod.Src3) and
         (Prod.Src1.Kind = svkRegister) and (Prod.Src1.RegType = srtString) then
        T := StrKey(Prod.Dest);
      if (T >= 0) and (T <= High(DefCount)) and (DefCount[T] = 1) and (UseCount[T] = 1) then
      begin
        // The single use must be a concatenation in this block that takes T as its RIGHT operand:
        // "acc + Mid(...)". The mirrored shape "Mid(...) + acc" is a different instruction and is
        // left alone.
        k := -1;
        for k2 := i + 1 to Blk.Instructions.Count - 1 do
        begin
          Cons := TSSAInstruction(Blk.Instructions[k2]);
          if (Cons.OpCode = ssaStrConcat) and (StrKey(Cons.Src2) = T) then begin k := k2; Break; end;
          if (StrKey(Cons.Src1) = T) or (StrKey(Cons.Src3) = T) then Break;   // used the other way
        end;
        if k > i then
        begin
          Cons := TSSAInstruction(Blk.Instructions[k]);
          // The fused instruction reads the table and the index at the CONCATENATION's position
          // instead of the Mid's, so nothing in between may have written them.
          Ok := True;
          for k2 := i + 1 to k - 1 do
          begin
            Mid := TSSAInstruction(Blk.Instructions[k2]);
            if (Prod.Src1.Kind = svkRegister) and WritesSSAReg(Mid, Prod.Src1.RegType, Prod.Src1.RegIndex) then begin Ok := False; Break; end;
            if (Prod.Src2.Kind = svkRegister) and WritesSSAReg(Mid, Prod.Src2.RegType, Prod.Src2.RegIndex) then begin Ok := False; Break; end;
          end;
          if Ok then
          begin
            Cons.OpCode := ssaStrConcatCharAt;
            // Src1 stays the accumulator; Src2 becomes the table and Src3 the index.
            Cons.Src2 := Prod.Src1;
            Cons.Src3 := Prod.Src2;
            // ⛔ Delete frees it: Instructions owns its objects (see RunAscMidFusion).
            Blk.Instructions.Delete(i);
            Inc(Result);
            Continue;                 // i now indexes the instruction after the deleted Mid
          end;
        end;
      end;
      Inc(i);
    end;
  end;
end;

function TSSAProgram.RunAppendMappedFusion: Integer;
// Fuse the whole inner loop of reverse-complement into ONE instruction:
//
//   t1  = StrAscMid       s, i, 1          ' Asc(Mid(s, i, 1))
//   t2  = AddInt          t1, 1            ' ...+1, the 1-based table index
//   acc = StrConcatCharAt acc, tab, t2     ' acc += tab[t2]
//   =>
//   acc = StrAppendMapped acc, s, tab, i
//
// Three dispatches become one. Measured at ~25 ns each on this machine, which on reverse-complement
// is 76 ns per character over a million characters - the single biggest item in that benchmark
// (transform 76 ms of 171 ms total, with read 47 and write 16).
//
// ⚠️ Runs AFTER RunConcatCharFusion, which is what produces the ssaStrConcatCharAt this consumes,
// and is therefore gated the same way (it follows the AOT - see the note in that pass).
//
// The conditions are about SAFETY, not about how often they fire:
//   * t1 and t2 must each be defined once and used once, so removing their instructions removes
//     nothing anybody else needs;
//   * the AddInt must add exactly 1 (directly or through a register defined once by LoadConstInt 1);
//   * the concatenation must already write into its own accumulator (Dest = Src1), because the
//     fused opcode APPENDS to Dest - with a different destination it would drop the accumulator;
//   * nothing between the three may write s, tab or i, since the fused instruction reads all three
//     at the concatenation's position rather than at the Mid's.
var
  b, i, k, k2, VStride, MaxVer: Integer;
  Blk: TSSABasicBlock;
  Ins, Add, Asc: TSSAInstruction;
  IntDefs, IntUses: array of Integer;   // (int reg, version) -> definitions / uses
  IntConst: array of Int64;             // ...and the value when the single def is LoadConstInt
  AscAt, AddAt: Integer;
  Ok: Boolean;

  function IntKey(const V: TSSAValue): Integer;
  begin
    if (V.Kind = svkRegister) and (V.RegType = srtInt) and
       (V.RegIndex >= 0) and (V.Version >= 0) and (V.Version <= MaxVer) then
      Result := V.RegIndex * VStride + V.Version
    else
      Result := -1;
  end;

  procedure BumpInt(var Arr: array of Integer; const V: TSSAValue);
  var Key: Integer;
  begin
    Key := IntKey(V);
    if (Key >= 0) and (Key <= High(Arr)) then Inc(Arr[Key]);
  end;

  function SingleDefSingleUse(const V: TSSAValue): Boolean;
  var Key: Integer;
  begin
    Key := IntKey(V);
    Result := (Key >= 0) and (Key <= High(IntDefs)) and (IntDefs[Key] = 1) and (IntUses[Key] = 1);
  end;

  // Is this operand the literal 1, directly or through a register defined exactly once by
  // LoadConstInt 1? Same rule as IsLengthOne in RunConcatCharFusion.
  function IsOne(const V: TSSAValue): Boolean;
  var Key: Integer;
  begin
    if V.Kind = svkConstInt then Exit(V.ConstInt = 1);
    Key := IntKey(V);
    Result := (Key >= 0) and (Key <= High(IntDefs)) and (IntDefs[Key] = 1) and (IntConst[Key] = 1);
  end;

  // Find, within this block and before position Before, the instruction defining V.
  function DefPosBefore(Blk: TSSABasicBlock; const V: TSSAValue; Before: Integer): Integer;
  var j: Integer;
      D: TSSAInstruction;
  begin
    Result := -1;
    if IntKey(V) < 0 then Exit;
    for j := Before - 1 downto 0 do
    begin
      D := TSSAInstruction(Blk.Instructions[j]);
      if (D.Dest.Kind = svkRegister) and (D.Dest.RegType = srtInt) and
         (D.Dest.RegIndex = V.RegIndex) and (D.Dest.Version = V.Version) then
        Exit(j);
    end;
  end;

  // STRFUSE_DIAG=1 says which guard refused a candidate. The guards are deliberately conservative and
  // this pass runs post-allocation, where "why did it not fire" is the question it always gets.
  procedure Say(Pos: Integer; const Why: string);
  begin
    if GetEnvironmentVariable('STRFUSE_DIAG') = '1' then
      WriteLn(ErrOutput, '[APPENDMAP] candidate at ', Pos, ': ', Why);
  end;

  // Does anything in (From, To) write the register named by V?
  function WrittenBetween(Blk: TSSABasicBlock; const V: TSSAValue; FromPos, ToPos: Integer): Boolean;
  var j: Integer;
  begin
    Result := False;
    if V.Kind <> svkRegister then Exit;
    for j := FromPos + 1 to ToPos - 1 do
      if WritesSSAReg(TSSAInstruction(Blk.Instructions[j]), V.RegType, V.RegIndex) then Exit(True);
  end;

  // Is INT register RegIdx LIVE immediately after position Pos of block B?
  //
  // ⭐ This is the whole licence for the fusion. Removing the two producers stops computing a value;
  // that is unobservable exactly when no read can reach the point where it would have been produced -
  // which is LIVENESS, and nothing weaker. Three cheaper rules were tried and all three were wrong,
  // each instructively: after register allocation the SAME physical register is handed to every site
  // of the same shape, so anything phrased as "nobody else mentions this register" is answered by the
  // allocator rather than by the program, and refuses every real case while still firing on a
  // single-site probe.
  //
  // Standard backward dataflow for ONE register, to fixpoint over the CFG:
  //   Use[b]     the block READS it before writing it (an upward-exposed use)
  //   Def[b]     the block writes it somewhere (a basic block is linear, so that write kills)
  //   LiveIn[b]  = Use[b] or (LiveOut[b] and not Def[b])
  //   LiveOut[b] = OR of LiveIn over the successors
  // "Live after Pos" is then: a read before any redefinition in the rest of THIS block, or - when the
  // value simply falls out of the block - LiveOut of this block.
  //
  // ⚠️ Successor indices are resolved by IDENTITY against the Blocks list, not read from BlockIndex:
  // that field is stamped once and blocks are removed by other passes, and a stale index here would
  // answer "dead" for a live register - the direction that miscompiles. Anything unresolvable answers
  // LIVE, so a doubt costs a missed fusion.
  function IntRegLiveAfter(RegIdx: Integer; B: TSSABasicBlock; Pos: Integer): Boolean;
  var
    UseB, DefB, LiveIn: array of Boolean;
    SuccIdx: array of array of Integer;
    bb, jj, ss, q, SelfIdx: Integer;
    B2: TSSABasicBlock;
    D: TSSAInstruction;
    Changed, Seen, LiveOutB, NewIn: Boolean;

    function IsR(const V: TSSAValue): Boolean;
    begin
      Result := (V.Kind = svkRegister) and (V.RegType = srtInt) and (V.RegIndex = RegIdx);
    end;

    function ReadsR(Ins2: TSSAInstruction): Boolean;
    var k: Integer;
    begin
      Result := IsR(Ins2.Src1) or IsR(Ins2.Src2) or IsR(Ins2.Src3);
      if not Result then
        for k := 0 to High(Ins2.PhiSources) do
          if IsR(Ins2.PhiSources[k].Value) then Exit(True);
    end;

    function IndexOfBlock(Blk2: TSSABasicBlock): Integer;
    var k: Integer;
    begin
      Result := -1;
      for k := 0 to Blocks.Count - 1 do
        if TSSABasicBlock(Blocks[k]) = Blk2 then Exit(k);
    end;

  begin
    // 1) The rest of THIS block settles it whenever it mentions the register at all.
    for q := Pos + 1 to B.Instructions.Count - 1 do
    begin
      D := TSSAInstruction(B.Instructions[q]);
      if ReadsR(D) then Exit(True);
      if IsR(D.Dest) then Exit(False);      // redefined before any read: dead from here on
    end;

    // 2) It falls out of the block, so the answer is LiveOut - and that needs the fixpoint.
    SetLength(UseB, Blocks.Count); SetLength(DefB, Blocks.Count); SetLength(LiveIn, Blocks.Count);
    SetLength(SuccIdx, Blocks.Count);
    for bb := 0 to Blocks.Count - 1 do
    begin
      B2 := TSSABasicBlock(Blocks[bb]);
      UseB[bb] := False; DefB[bb] := False; LiveIn[bb] := False;
      Seen := False;
      for jj := 0 to B2.Instructions.Count - 1 do
      begin
        D := TSSAInstruction(B2.Instructions[jj]);
        if (not Seen) and ReadsR(D) then begin UseB[bb] := True; Seen := True; end;
        if IsR(D.Dest) then begin DefB[bb] := True; Seen := True; end;
      end;
      SetLength(SuccIdx[bb], B2.Successors.Count);
      for ss := 0 to B2.Successors.Count - 1 do
        SuccIdx[bb][ss] := IndexOfBlock(TSSABasicBlock(B2.Successors[ss]));
    end;

    repeat
      Changed := False;
      for bb := Blocks.Count - 1 downto 0 do
      begin
        LiveOutB := False;
        for ss := 0 to High(SuccIdx[bb]) do
          if (SuccIdx[bb][ss] < 0) or LiveIn[SuccIdx[bb][ss]] then
          begin LiveOutB := True; Break; end;
        NewIn := UseB[bb] or (LiveOutB and (not DefB[bb]));
        if NewIn <> LiveIn[bb] then begin LiveIn[bb] := NewIn; Changed := True; end;
      end;
    until not Changed;

    SelfIdx := IndexOfBlock(B);
    Result := True;
    if SelfIdx < 0 then Exit;               // cannot place the block: answer LIVE, the safe direction
    Result := False;
    for ss := 0 to High(SuccIdx[SelfIdx]) do
      if (SuccIdx[SelfIdx][ss] < 0) or LiveIn[SuccIdx[SelfIdx][ss]] then Exit(True);
  end;


begin
  Result := 0;
  // ⭐ DEFAULT ON. APPENDMAP=0 turns the fusion off, so the before/after can be timed on ONE binary.
  //
  // It shipped OFF for a long time because it MISCOMPILED, and the recorded reason was structural and
  // wrong: "the fused instruction must name FIVE values - accumulator out, accumulator in, source
  // string, table, index - and there are four operand fields, so the incoming accumulator is named by
  // no operand, PHI elimination has nothing to rewrite, and the reset of the accumulator lands on a
  // different register from the one the append grows". Every word of that is true - BEFORE register
  // allocation.
  //
  // ⭐⭐⭐ The fix was not a fifth operand, or packing, or a new pass. It was WHEN this runs. The pass is
  // now called AFTER register allocation (see SedaiBasicVM.lpr), where the registers are PHYSICAL: the
  // incoming and outgoing accumulator ARE the same register, Dest names both, and the constraint
  // simply does not exist. Nothing renames anything afterwards, and both consumers of the SSA - the
  // bytecode compiler and the AOT, which compiles from SSA and has a native helper for this opcode -
  // see the fused form.
  //
  // ⚠️ Two REAL defects found along the way are fixed and must stay fixed: the sub-opcode was missing
  // from the dispatch in RunTemplate.inc, and RunStringTempFusion treated this opcode as a pure
  // producer and redirected its Dest (see the exclusion there).
  //
  // ⛔ What licenses dropping the two producers is LIVENESS - see IntRegLiveAfter - and nothing
  // weaker. Three cheaper guards were tried and all three were wrong in the same way: they asked
  // "does anything else name this register?", and after allocation that question is answered by the
  // ALLOCATOR, not by the program. It hands the same physical register to every site of this shape,
  // so the two occurrences in reverse-complement vetoed each other. ⚠️ All three wrong guards fired
  // correctly on a probe with a SINGLE site: a single-site probe does not show that a guard is usable.
  if GetEnvironmentVariable('APPENDMAP') = '0' then Exit;
  if GetEnvironmentVariable('STRCHARFUSE') = '0' then Exit;
  if (GetEnvironmentVariable('STRCHARFUSE') <> '1') and (not GAotWillRun) then Exit;

  MaxVer := 0;
  for b := 0 to Blocks.Count - 1 do
  begin
    Blk := TSSABasicBlock(Blocks[b]);
    for i := 0 to Blk.Instructions.Count - 1 do
    begin
      Ins := TSSAInstruction(Blk.Instructions[i]);
      if (Ins.Dest.Kind = svkRegister) and (Ins.Dest.Version > MaxVer) then MaxVer := Ins.Dest.Version;
      if (Ins.Src1.Kind = svkRegister) and (Ins.Src1.Version > MaxVer) then MaxVer := Ins.Src1.Version;
      if (Ins.Src2.Kind = svkRegister) and (Ins.Src2.Version > MaxVer) then MaxVer := Ins.Src2.Version;
      if (Ins.Src3.Kind = svkRegister) and (Ins.Src3.Version > MaxVer) then MaxVer := Ins.Src3.Version;
      for k := 0 to High(Ins.PhiSources) do
        if (Ins.PhiSources[k].Value.Kind = svkRegister) and (Ins.PhiSources[k].Value.Version > MaxVer) then
          MaxVer := Ins.PhiSources[k].Value.Version;
    end;
  end;
  VStride := MaxVer + 1;

  SetLength(IntDefs, (FNextRegister[srtInt] + 1) * VStride);
  SetLength(IntUses, Length(IntDefs));
  SetLength(IntConst, Length(IntDefs));
  for i := 0 to High(IntDefs) do begin IntDefs[i] := 0; IntUses[i] := 0; IntConst[i] := 0; end;

  for b := 0 to Blocks.Count - 1 do
  begin
    Blk := TSSABasicBlock(Blocks[b]);
    for i := 0 to Blk.Instructions.Count - 1 do
    begin
      Ins := TSSAInstruction(Blk.Instructions[i]);
      BumpInt(IntUses, Ins.Src1);
      BumpInt(IntUses, Ins.Src2);
      BumpInt(IntUses, Ins.Src3);
      for k := 0 to High(Ins.PhiSources) do
        BumpInt(IntUses, Ins.PhiSources[k].Value);
      k2 := IntKey(Ins.Dest);
      if (k2 >= 0) and (k2 <= High(IntDefs)) then
      begin
        Inc(IntDefs[k2]);
        if (Ins.OpCode = ssaLoadConstInt) and (Ins.Src1.Kind = svkConstInt) then
          IntConst[k2] := Ins.Src1.ConstInt
        else
          IntConst[k2] := MaxInt;      // defined by something else: never equal to 1
      end;
    end;
  end;

  for b := 0 to Blocks.Count - 1 do
  begin
    Blk := TSSABasicBlock(Blocks[b]);
    i := 0;
    while i < Blk.Instructions.Count do
    begin
      Ins := TSSAInstruction(Blk.Instructions[i]);
      // Anchor on the concatenation, and only the in-place shape.
      if (Ins.OpCode <> ssaStrConcatCharAt) or
         (Ins.Dest.Kind <> svkRegister) or (Ins.Src1.Kind <> svkRegister) or
         (Ins.Dest.RegIndex <> Ins.Src1.RegIndex) or (Ins.Dest.RegType <> Ins.Src1.RegType) then
      begin Inc(i); Continue; end;

      AddAt := DefPosBefore(Blk, Ins.Src3, i);
      if AddAt < 0 then begin Say(i, 'no def of the index in this block'); Inc(i); Continue; end;
      Add := TSSAInstruction(Blk.Instructions[AddAt]);
      if Add.OpCode <> ssaAddInt then begin Say(i, 'the index is not defined by AddInt'); Inc(i); Continue; end;
      if not IsOne(Add.Src2) then begin Say(i, 'the AddInt addend is not the constant 1'); Inc(i); Continue; end;

      AscAt := DefPosBefore(Blk, Add.Src1, AddAt);
      if AscAt < 0 then begin Say(i, 'no def of the AddInt source in this block'); Inc(i); Continue; end;
      Asc := TSSAInstruction(Blk.Instructions[AscAt]);
      if Asc.OpCode <> ssaStrAscMid then begin Say(i, 'the AddInt source is not StrAscMid'); Inc(i); Continue; end;
      if (Asc.Src1.Kind <> svkRegister) or (Asc.Src1.RegType <> srtString) then
        begin Say(i, 'the AscMid source is not a string register'); Inc(i); Continue; end;
      if not IsOne(Asc.Src3) then begin Say(i, 'the AscMid length is not the constant 1'); Inc(i); Continue; end;

      // Post-allocation the three must all name the SAME physical int register, and every read of it
      // must sit inside a triple of this shape - that is what makes dropping the producers unobservable.
      if (Asc.Dest.Kind <> svkRegister) or (Asc.Dest.RegType <> srtInt) then
        begin Say(i, 'the AscMid destination is not an int register'); Inc(i); Continue; end;
      if (Add.Dest.Kind <> svkRegister) or (Add.Dest.RegIndex <> Asc.Dest.RegIndex) or
         (Add.Src1.RegIndex <> Asc.Dest.RegIndex) then
        begin Say(i, 'the AddInt does not read and write the AscMid register'); Inc(i); Continue; end;
      // The temporary must be DEAD after the append: that is what makes dropping the two producers
      // unobservable, and nothing weaker will do (see IntRegLiveAfter).
      if IntRegLiveAfter(Asc.Dest.RegIndex, Blk, i) then
        begin Say(i, 'the int temporary is still live after the append'); Inc(i); Continue; end;

      // Nothing in between may write what the fused instruction will read at the concatenation's
      // position: the source string, the table, or the index.
      Ok := not WrittenBetween(Blk, Asc.Src1, AscAt, i);
      if Ok then Ok := not WrittenBetween(Blk, Asc.Src2, AscAt, i);
      if Ok then Ok := not WrittenBetween(Blk, Ins.Src2, AscAt, i);
      if not Ok then begin Inc(i); Continue; end;

      // Rewrite in place, then drop the two producers - highest index first, so the lower one does
      // not shift. ⛔ Delete FREES the object: Instructions owns them (see RunAscMidFusion).
      Ins.OpCode := ssaStrAppendMapped;
      Ins.Src1 := Asc.Src1;        // the source string
      // Src2 already holds the table, Src3 becomes the index into the source string.
      Ins.Src3 := Asc.Src2;
      Blk.Instructions.Delete(AddAt);
      Blk.Instructions.Delete(AscAt);
      Inc(Result);
      i := i - 1;                  // two removed before i, one instruction rewritten at i-2
      if i < 0 then i := 0;
    end;
  end;
end;

function TSSAProgram.RunStringTempFusion: Integer;
// Fuse "<string producer> T, ..." + "CopyString D, T" into "<string producer> D, ...".
//
// Every string primitive writes a fresh TEMPORARY that the next instruction copies into the
// variable, so "s = s + x" becomes
//     StrConcat   T, s, x
//     CopyString  s, T
// That is a dispatch per operation, and worse: the buffer ends up SHARED between T and s, so its
// reference count is never 1 and the VM can never append in place. Removing the copy is what makes
// "s = s + x" linear instead of quadratic (see AppendString in SedaiBytecodeVM).
//
// ⛔ THIS MUST LIVE AT SSA LEVEL, not in the bytecode peephole. The AOT compiles from SSA and
// installs its native code over the bytecode's PC ranges; a pass that rewrites only the bytecode
// leaves the two describing different programs, and the result is a silent miscompile -- "Str(123)"
// came out as the empty string under --aot while the interpreter printed it correctly. Emitting the
// bytecode from the FUSED SSA keeps both views identical by construction.
//
// Run BEFORE register allocation: afterwards a temporary's register is reused by other values, so
// "read exactly once" would be false almost everywhere and the fusion would never fire.
//
// Safety: the temporary must have EXACTLY ONE definition and EXACTLY ONE use (that copy), and the
// two must be adjacent in the same block, so no control flow can reach one without the other.
// Counting defs as well as uses matters because PHI elimination has already broken single
// assignment for the variables it lowered.
//
// The census keys on (register, VERSION), not on the register alone. A loop accumulator lowers to
// several versions of ONE register -- "STR[66]_2/_3/_4" for a single "line" -- so counting per
// register made every accumulator look multi-defined and the fusion never fired on the one shape
// that matters most. Version 0 means "unversioned", where per-version and per-register counting
// coincide, so nothing changes for the registers that were already handled.
//
// Diagnostics: STRFUSE_DIAG=1 reports, per string producer, whether it fused and if not WHY.
// The pass is silent by construction (it either rewrites or walks away), so without this the only
// way to tell a fusion that never fires from one that fires and does not pay is the stopwatch --
// which answers "how much", never "why".
var
  b, i, k, k2, T, D: Integer;
  Ok, Diag, VersionedCensus: Boolean;
  Blk, SBlk: TSSABasicBlock;
  Prod, Cp, Mid: TSSAInstruction;
  DefCount, UseCount: array of Integer;
  NMultiDef, NMultiUse, NNoCopy, NBlocked, NFused, NXBlock: Integer;
  MaxVer, VStride: Integer;

  // Census key: one slot per (string register, version) pair. STRFUSE_VER=0 collapses every version
  // onto the register, which is exactly the old per-register census -- the A/B arm for this change,
  // on ONE binary, since comparing against historical numbers is worthless on a loaded machine.
  function KeyOf(const V: TSSAValue): Integer;
  begin
    if (V.Kind = svkRegister) and (V.RegType = srtString) and
       (V.RegIndex >= 0) and (V.Version >= 0) and (V.Version <= MaxVer) then
    begin
      if VersionedCensus then
        Result := V.RegIndex * VStride + V.Version
      else
        Result := V.RegIndex * VStride;
    end
    else
      Result := -1;
  end;

  procedure Bump(var Arr: array of Integer; const V: TSSAValue);
  var
    Key: Integer;
  begin
    Key := KeyOf(V);
    if (Key >= 0) and (Key <= High(Arr)) then Inc(Arr[Key]);
  end;

  procedure Reject(const Reason: string; P: TSSAInstruction; Defs, Uses_: Integer);
  begin
    if not Diag then Exit;
    WriteLn(ErrOutput, Format('[STRFUSE] %-10s %-22s dest=S%d defs=%d uses=%d',
      [Reason, SSAOpCodeToString(P.OpCode), P.Dest.RegIndex, Defs, Uses_]));
  end;

begin
  Result := 0;
  Diag := GetEnvironmentVariable('STRFUSE_DIAG') <> '';
  VersionedCensus := GetEnvironmentVariable('STRFUSE_VER') <> '0';
  NMultiDef := 0; NMultiUse := 0; NNoCopy := 0; NBlocked := 0; NFused := 0; NXBlock := 0;

  // Widest version in use, so the (register, version) census can be a flat array.
  MaxVer := 0;
  for b := 0 to Blocks.Count - 1 do
  begin
    Blk := TSSABasicBlock(Blocks[b]);
    for i := 0 to Blk.Instructions.Count - 1 do
    begin
      Prod := TSSAInstruction(Blk.Instructions[i]);
      if (Prod.Dest.Kind = svkRegister) and (Prod.Dest.RegType = srtString) and (Prod.Dest.Version > MaxVer) then MaxVer := Prod.Dest.Version;
      if (Prod.Src1.Kind = svkRegister) and (Prod.Src1.RegType = srtString) and (Prod.Src1.Version > MaxVer) then MaxVer := Prod.Src1.Version;
      if (Prod.Src2.Kind = svkRegister) and (Prod.Src2.RegType = srtString) and (Prod.Src2.Version > MaxVer) then MaxVer := Prod.Src2.Version;
      if (Prod.Src3.Kind = svkRegister) and (Prod.Src3.RegType = srtString) and (Prod.Src3.Version > MaxVer) then MaxVer := Prod.Src3.Version;
      for k := 0 to High(Prod.PhiSources) do
        if (Prod.PhiSources[k].Value.Kind = svkRegister) and (Prod.PhiSources[k].Value.RegType = srtString) and
           (Prod.PhiSources[k].Value.Version > MaxVer) then MaxVer := Prod.PhiSources[k].Value.Version;
    end;
  end;
  VStride := MaxVer + 1;

  SetLength(DefCount, (FNextRegister[srtString] + 1) * VStride);
  SetLength(UseCount, (FNextRegister[srtString] + 1) * VStride);
  for i := 0 to High(DefCount) do begin DefCount[i] := 0; UseCount[i] := 0; end;

  // STRFUSE_DIAG=2 dumps every block's string traffic, with the block label and the successor edges.
  // The counts alone say a fusion did not fire; only the shape says what stands in the way.
  if GetEnvironmentVariable('STRFUSE_DIAG') = '2' then
    for b := 0 to Blocks.Count - 1 do
    begin
      Blk := TSSABasicBlock(Blocks[b]);
      WriteLn(ErrOutput, Format('[STRFUSE] --- block %d "%s"  preds=%d succs=%d',
        [b, Blk.LabelName, Blk.Predecessors.Count, Blk.Successors.Count]));
      for i := 0 to Blk.Instructions.Count - 1 do
      begin
        Prod := TSSAInstruction(Blk.Instructions[i]);
        if ((Prod.Dest.Kind = svkRegister) and (Prod.Dest.RegType = srtString)) or
           ((Prod.Src1.Kind = svkRegister) and (Prod.Src1.RegType = srtString)) or
           ((Prod.Src2.Kind = svkRegister) and (Prod.Src2.RegType = srtString)) then
          WriteLn(ErrOutput, Format('[STRFUSE]     %3d: %s', [i, Prod.ToString]));
      end;
    end;

  // Census first: every definition and every use of every string register, program-wide.
  for b := 0 to Blocks.Count - 1 do
  begin
    Blk := TSSABasicBlock(Blocks[b]);
    for i := 0 to Blk.Instructions.Count - 1 do
    begin
      Prod := TSSAInstruction(Blk.Instructions[i]);
      Bump(DefCount, Prod.Dest);
      Bump(UseCount, Prod.Src1);
      Bump(UseCount, Prod.Src2);
      Bump(UseCount, Prod.Src3);
      for k := 0 to High(Prod.PhiSources) do
        Bump(UseCount, Prod.PhiSources[k].Value);
      // An instruction that READS its own Dest (a store carrying the value there) counts as a use
      // too; treating it as a pure definition would let the fusion overwrite a live value.
      if Prod.OpCode in [ssaArrayStoreIndString, ssaRecordStoreString, ssaXferStoreString] then
        Bump(UseCount, Prod.Dest);
    end;
  end;

  for b := 0 to Blocks.Count - 1 do
  begin
    Blk := TSSABasicBlock(Blocks[b]);
    i := 0;
    while i < Blk.Instructions.Count - 1 do
    begin
      Prod := TSSAInstruction(Blk.Instructions[i]);
      T := -1;
      // ⛔ ssaStrAppendMapped is NOT a producer: it APPENDS to its Dest, so the incoming value is an
      // input. Redirecting its Dest to a copy's destination hands it a different accumulator, and
      // the result is a silent miscompile - the reset of the accumulator lands on one register while
      // the append keeps growing another, so every emitted line contains all the previous ones.
      // (Costly to find: it only shows inside a PROCEDURE, and the INTERPRETER runs the same
      // bytecode correctly, which sends you looking at the AOT emitter instead of at this list.)
      if (Prod.Dest.Kind = svkRegister) and (Prod.Dest.RegType = srtString) and
         (Prod.OpCode <> ssaCopyString) and (Prod.OpCode <> ssaStrAppendMapped) and
         not (Prod.OpCode in [ssaArrayStoreIndString, ssaRecordStoreString, ssaXferStoreString, ssaPhi]) then
        T := KeyOf(Prod.Dest);
      if (T >= 0) and (T <= High(DefCount)) and
         ((DefCount[T] <> 1) or (UseCount[T] <> 1)) then
      begin
        // Not fusable, but worth counting: these two are the reasons the pass walks away most often.
        if DefCount[T] <> 1 then
        begin
          Inc(NMultiDef);
          Reject('multi-def', Prod, DefCount[T], UseCount[T]);
        end
        else
        begin
          Inc(NMultiUse);
          Reject('multi-use', Prod, DefCount[T], UseCount[T]);
        end;
      end
      else if (T >= 0) and (T <= High(DefCount)) and
              (DefCount[T] = 1) and (UseCount[T] = 1) then
      begin
        // Find the copy that consumes T. It does NOT have to be the next instruction: T has exactly
        // one definition and one use, so nothing between can read or write it, and a basic block has
        // no control flow inside it. Requiring adjacency was simply too strong -- reverse-complement
        // puts "col += 1" and its test between the concatenation and the copy, and the whole benefit
        // was lost for that shape.
        k := -1;
        for D := i + 1 to Blk.Instructions.Count - 1 do
        begin
          Cp := TSSAInstruction(Blk.Instructions[D]);
          if (Cp.OpCode = ssaCopyString) and (KeyOf(Cp.Src1) = T) then
          begin
            k := D;
            Break;
          end;
        end;
        if k > i then
        begin
          Cp := TSSAInstruction(Blk.Instructions[k]);
          if (Cp.Dest.Kind = svkRegister) and (Cp.Dest.RegType = srtString) and
             (Cp.Dest.RegIndex >= 0) and (KeyOf(Cp.Dest) <> T) then
          begin
            D := Cp.Dest.RegIndex;
            // What DOES have to hold: nothing between may touch the DESTINATION. Writing D earlier
            // than the copy did would be observed by an intervening read of D, and would be undone
            // by an intervening write to it. The test is per REGISTER, not per version -- deliberately
            // conservative, and it is exactly what protects the accumulator case below: a read of the
            // OLD version between producer and copy blocks the fusion.
            Ok := True;
            for k2 := i + 1 to k - 1 do
            begin
              Mid := TSSAInstruction(Blk.Instructions[k2]);
              if TouchesStrReg(Mid, D) then begin Ok := False; Break; end;
            end;
            // With a versioned census the destination can now be ANOTHER VERSION OF THE PRODUCER'S
            // OWN SOURCE -- that is the accumulator, "s = s + x", and fusing it is what finally makes
            // the VM's in-place append reachable (bcStrConcat with Dest = Src1). Allow that only for
            // StrConcat, which has the in-place path: any other producer would be asked to write the
            // register it is still reading, and a helper that stores before it loads would read back
            // its own result.
            if Ok and (Prod.OpCode <> ssaStrConcat) then
              if ((Prod.Src1.Kind = svkRegister) and (Prod.Src1.RegType = srtString) and (Prod.Src1.RegIndex = D)) or
                 ((Prod.Src2.Kind = svkRegister) and (Prod.Src2.RegType = srtString) and (Prod.Src2.RegIndex = D)) then
                Ok := False;
            if Ok then
            begin
              Prod.Dest := Cp.Dest;
              // ⛔ Delete already frees it (OwnsObjects list) -- see RunAscMidFusion above.
              Blk.Instructions.Delete(k);
              Inc(Result);
              Inc(NFused);
            end
            else
            begin
              Inc(NBlocked);
              Reject('blocked', Prod, 1, 1);
            end;
          end;
        end
        else
        begin
          Inc(NNoCopy);
          // Price the single-block rule before relaxing it: the PHI copy that closes a loop-carried
          // accumulator is emitted in the LATCH, not in the block that computes the value, so the
          // shape "s = s + x" inside a For body is invisible to a search that stops at the block end.
          // Counting -- not fusing -- the cases where the consumer sits in a successor that this block
          // alone reaches says how much a cross-block rule would be worth.
          if Diag and (Blk.Successors.Count = 1) then
          begin
            SBlk := TSSABasicBlock(Blk.Successors[0]);
            if (SBlk <> Blk) and (SBlk.Predecessors.Count = 1) then
              for k2 := 0 to SBlk.Instructions.Count - 1 do
              begin
                Mid := TSSAInstruction(SBlk.Instructions[k2]);
                if (Mid.OpCode = ssaCopyString) and (Mid.Src1.Kind = svkRegister) and
                   (Mid.Src1.RegType = srtString) and (Mid.Src1.RegIndex = T) then
                begin
                  Inc(NXBlock);
                  Break;
                end;
              end;
          end;
          Reject('no-copy', Prod, 1, 1);
        end;
      end;
      Inc(i);
    end;
  end;

  if Diag then
    WriteLn(ErrOutput, Format('[STRFUSE] summary: fused=%d  rejected multi-def=%d multi-use=%d no-copy=%d blocked=%d  (of the no-copy, %d have their consumer in a single-pred successor)',
      [NFused, NMultiDef, NMultiUse, NNoCopy, NBlocked, NXBlock]));
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

  AnnotateDivByConst;

  PhiElim := TPhiElimination.Create(Self);
  try
    PhiElim.Run;
  finally
    PhiElim.Free;
  end;
end;

procedure TSSAProgram.AnnotateDivByConst;
{ C7: record the DIVISOR of `x \ C` and `x Mod C` on the instruction itself, in Src3.

  Why here, and not later: the divisor reaches the back end as a REGISTER, and after register
  allocation that register number no longer identifies a value - measured on pidigits, the number
  carrying 1000000000 is also written by a LoadConstInt 10, an ArrayLoad, two CopyInt and a SubInt
  elsewhere in the program. So a back-end analysis CANNOT recover the constant, however careful:
  the information only exists before allocation. This runs at the head of PHI elimination because
  that is the one point every pipeline crosses (sb, sbc, the REPL, the runner, the web server)
  AFTER constant propagation has settled the value and BEFORE registers are allocated.

  Sound rule: a register qualifies when EVERY definition of it is a LoadConstInt and they all agree
  on the value. Whatever path reaches the use, the register holds that number.

  The annotation is inert by itself - the bytecode compiler copies it into the instruction's
  Immediate (unused by these two opcodes) and the interpreter ignores it. Only the AOT reads it, to
  emit a multiply-high instead of idiv. }
var
  b, j, r, MaxReg, Round: Integer;
  DefValTmp: Int64;
  Ins: TSSAInstruction;
  DefCount, ConstDefs: array of Integer;
  DefVal: array of Int64;
  Agree, Known: array of Boolean;
  KnownVal: array of Int64;

  procedure NoteDef(const V: TSSAValue; IsConst: Boolean; Val: Int64);
  var q: Integer;
  begin
    if (V.Kind <> svkRegister) or (V.RegType <> srtInt) then Exit;
    q := V.RegIndex;
    if (q < 0) or (q > MaxReg) then Exit;
    if DefCount[q] = 0 then begin DefVal[q] := Val; Agree[q] := True; end
    else if (not IsConst) or (DefVal[q] <> Val) then Agree[q] := False;
    Inc(DefCount[q]);
    if IsConst then Inc(ConstDefs[q]);
  end;

  // A CopyInt from a register already proven constant defines a constant too. Following the chain
  // is not a nicety: measured, a divisor that reaches the operand through one copy is exactly the
  // shape that made this analysis find NOTHING on job/tests/bench/pidigits_prims.bas while finding
  // every site on the CLBG program - the difference was one copy that propagation had not removed.
  function ConstOfDef(const Ins: TSSAInstruction; out Val: Int64): Boolean;
  var q: Integer;
  begin
    Val := 0;
    if (Ins.OpCode = ssaLoadConstInt) and (Ins.Src1.Kind = svkConstInt) then
    begin
      Val := Ins.Src1.ConstInt;
      Exit(True);
    end;
    Result := False;
    if (Ins.OpCode = ssaCopyInt) and (Ins.Src1.Kind = svkRegister) and (Ins.Src1.RegType = srtInt) then
    begin
      q := Ins.Src1.RegIndex;
      if (q >= 0) and (q <= MaxReg) and Known[q] then
      begin
        Val := KnownVal[q];
        Result := True;
      end;
    end;
  end;

begin
  MaxReg := 0;
  for b := 0 to Blocks.Count - 1 do
    for j := 0 to Blocks[b].Instructions.Count - 1 do
    begin
      Ins := Blocks[b].Instructions[j];
      if (Ins.Dest.Kind = svkRegister) and (Ins.Dest.RegType = srtInt) and (Ins.Dest.RegIndex > MaxReg) then
        MaxReg := Ins.Dest.RegIndex;
    end;
  if MaxReg = 0 then Exit;
  SetLength(DefCount, MaxReg + 1);
  SetLength(ConstDefs, MaxReg + 1);
  SetLength(DefVal, MaxReg + 1);
  SetLength(Agree, MaxReg + 1);
  SetLength(Known, MaxReg + 1);
  SetLength(KnownVal, MaxReg + 1);

  // Three rounds: enough for a constant to travel through a couple of copies, and bounded so the
  // pass cannot become a fixpoint that costs compile time on a program that gains nothing.
  for Round := 1 to 3 do
  begin
    FillChar(DefCount[0], Length(DefCount) * SizeOf(Integer), 0);
    FillChar(ConstDefs[0], Length(ConstDefs) * SizeOf(Integer), 0);
    FillChar(Agree[0], Length(Agree) * SizeOf(Boolean), 0);
    for b := 0 to Blocks.Count - 1 do
      for j := 0 to Blocks[b].Instructions.Count - 1 do
      begin
        Ins := Blocks[b].Instructions[j];
        if ConstOfDef(Ins, DefValTmp) then NoteDef(Ins.Dest, True, DefValTmp)
        else NoteDef(Ins.Dest, False, 0);
      end;
    for r := 0 to MaxReg do
    begin
      Known[r] := (DefCount[r] > 0) and (ConstDefs[r] = DefCount[r]) and Agree[r];
      if Known[r] then KnownVal[r] := DefVal[r];
    end;
  end;

  for b := 0 to Blocks.Count - 1 do
    for j := 0 to Blocks[b].Instructions.Count - 1 do
    begin
      Ins := Blocks[b].Instructions[j];
      if (Ins.OpCode <> ssaDivInt) and (Ins.OpCode <> ssaModInt) then Continue;
      if Ins.Src3.Kind <> svkNone then Continue;          // already carries something: leave it
      if (Ins.Src2.Kind <> svkRegister) or (Ins.Src2.RegType <> srtInt) then Continue;
      r := Ins.Src2.RegIndex;
      if (r < 0) or (r > MaxReg) or (not Known[r]) then Continue;
      // 0 would have to trap and ±1 is not worth a sequence; both stay on the hardware divide.
      if (KnownVal[r] = 0) or (KnownVal[r] = 1) or (KnownVal[r] = -1) then Continue;
      Ins.Src3 := MakeSSAConstInt(KnownVal[r]);
    end;
end;

function TSSAProgram.RunGVN: Integer;
var
  GVNPass: TGVNPass;
begin
  if not GSSAOptimizationsEnabled then Exit(0);
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
  if not GSSAOptimizationsEnabled then Exit(0);
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
  if not GSSAOptimizationsEnabled then Exit(0);
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
  if not GSSAOptimizationsEnabled then Exit(0);
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
  if not GSSAOptimizationsEnabled then Exit(0);
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
  if not GSSAOptimizationsEnabled then Exit(0);
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
  // Inlining a call drops the CFG edge that justified it (InlineCallSite), so a subroutine whose every
  // call site got inlined is now ORPHANED: no predecessors, and the dominator-tree builder reads that as
  // a second entry point and refuses to build. DBE runs BEFORE this pass in the pipeline, so nothing would
  // sweep those blocks up, and the next pass to rebuild the tree (loop unrolling) would fail and be
  // skipped. Sweep them here, where the orphans are made. Every pipeline -- sb, sbc, the REPL and the
  // runner -- calls this method, so one place covers them all.
  if Result > 0 then RunDBE;
end;

function TSSAProgram.RunConstProp: Integer;
var
  ConstProp: TSimpleConstProp;
begin
  if not GSSAOptimizationsEnabled then Exit(0);
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
  if not GSSAOptimizationsEnabled then Exit(0);
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
  // NB: dead-block elimination is NOT gated by --no-opt — it removes unreachable/orphan blocks that
  // would otherwise make the dominator-tree construction fail (multiple entry points), so it is a
  // structural prerequisite, not a value optimization. Keeping it in the no-opt path keeps that path
  // valid as a differential reference.
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
  if not GSSAOptimizationsEnabled then Exit(0);
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
  if not GSSAOptimizationsEnabled then Exit(0);
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
  if not GSSAOptimizationsEnabled then Exit(0);
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
  if not GSSAOptimizationsEnabled then Exit(0);
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

function OpIn(const Op: TSSAOpCode; const Ops: array of TSSAOpCode): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(Ops) do
    if Ops[i] = Op then Exit(True);
  Result := False;
end;

function ArithShr64(V, Shift: Int64): Int64;
// Sign-propagating (arithmetic) shift right. A count at or past the width saturates to the sign,
// which keeps the result defined where the hardware shift would not be.
begin
  if Shift <= 0 then Exit(V);
  if Shift > 63 then
  begin
    if V < 0 then Result := -1 else Result := 0;
    Exit;
  end;
  if V < 0 then
    Result := Int64((QWord(V) shr QWord(Shift)) or (QWord($FFFFFFFFFFFFFFFF) shl QWord(64 - Shift)))
  else
    Result := V shr Shift;
end;

function LogicalShr64(V, Shift: Int64): Int64;
// Zero-filling (logical) shift right, for UNSIGNED operands: the register holds the raw two's-
// complement bits, so reinterpret them as a QWord (the same trick as bcDivUInt/bcModUInt).
begin
  if Shift <= 0 then Exit(V);
  if Shift > 63 then Exit(0);
  Result := Int64(QWord(V) shr QWord(Shift));
end;

{ MODERN bit intrinsics — the ONE implementation of each, shared by the interpreter and read as the
  specification by the WebAssembly backend. Written in plain Pascal on purpose:
  ⛔ FPC's BsfQWord/BsrQWord are UNDEFINED at zero, and WASM defines clz(0)=ctz(0)=64 (or 32). That
     boundary is the whole reason these are functions and not one-liners at the call sites.
  ⛔ PopCnt needs a CPU feature to be fast and gives nothing here: these are not hot paths, and a
     portable loop keeps win64 and linux bit-identical without a gate.
  ⚠️ Width is 32 or 64. The 32-bit forms look at the LOW 32 bits and, for the rotates, SIGN-EXTEND
     the result, because that is the value a "Dim As Long" holds — the same rule NarrowInt64 applies
     to every other 32-bit destination. Any other choice would make ROTATELEFT32 disagree with an
     assignment of its own result. }

function BitClz(V: Int64; Width: Int64): Int64;
var
  U, Mask: QWord;
begin
  if Width = 32 then U := QWord(V) and $FFFFFFFF else U := QWord(V);
  if U = 0 then Exit(Width);                       // WASM: clz(0) = the width, not undefined
  Result := 0;
  Mask := QWord(1) shl QWord(Width - 1);
  while (U and Mask) = 0 do
  begin
    Inc(Result);
    Mask := Mask shr 1;
  end;
end;

function BitCtz(V: Int64; Width: Int64): Int64;
var
  U: QWord;
begin
  if Width = 32 then U := QWord(V) and $FFFFFFFF else U := QWord(V);
  if U = 0 then Exit(Width);                       // WASM: ctz(0) = the width, not undefined
  Result := 0;
  while (U and 1) = 0 do
  begin
    Inc(Result);
    U := U shr 1;
  end;
end;

function BitPopcnt(V: Int64; Width: Int64): Int64;
var
  U: QWord;
begin
  if Width = 32 then U := QWord(V) and $FFFFFFFF else U := QWord(V);
  Result := 0;
  while U <> 0 do
  begin
    U := U and (U - 1);                            // clear the lowest set bit
    Inc(Result);
  end;
end;

function BitRotl(V, Count: Int64; Width: Int64): Int64;
// WASM semantics: the count is taken MODULO the width, as an UNSIGNED amount, so a negative count
// wraps instead of being clamped. ⚠️ This is deliberately NOT the saturating rule our shifts use:
// a rotation past the width has an obvious meaning (it comes back round), while a shift past it
// does not, which is why the interpreter defines that one and mirrors this one.
var
  U, R: QWord;
  N: Integer;
begin
  N := Integer(QWord(Count) mod QWord(Width));
  if Width = 32 then
  begin
    U := QWord(V) and $FFFFFFFF;
    if N = 0 then R := U
    else R := ((U shl QWord(N)) or (U shr QWord(32 - N))) and $FFFFFFFF;
    Result := Int64(LongInt(Cardinal(R)));         // sign-extend: the value a Long holds
  end
  else
  begin
    U := QWord(V);
    if N = 0 then R := U
    else R := (U shl QWord(N)) or (U shr QWord(64 - N));
    Result := Int64(R);
  end;
end;

function BitRotr(V, Count: Int64; Width: Int64): Int64;
var
  U, R: QWord;
  N: Integer;
begin
  N := Integer(QWord(Count) mod QWord(Width));
  if Width = 32 then
  begin
    U := QWord(V) and $FFFFFFFF;
    if N = 0 then R := U
    else R := ((U shr QWord(N)) or (U shl QWord(32 - N))) and $FFFFFFFF;
    Result := Int64(LongInt(Cardinal(R)));
  end
  else
  begin
    U := QWord(V);
    if N = 0 then R := U
    else R := (U shr QWord(N)) or (U shl QWord(64 - N));
    Result := Int64(R);
  end;
end;

{ SSA string pool: open-addressing hash (FNV-1a, linear probing, grow at 60% load) over an
  append-only id → string array. A dedicated table instead of TFPHashList because the latter
  keys on SHORTSTRINGS: two BASIC string literals longer than 255 chars that share a prefix
  would silently intern to the same id. Single-threaded by design (see TSSAValue). }
var
  PoolStrings: array of string;   // id → string; slot 0 reserved for ''
  PoolCount: Integer = 0;
  PoolBuckets: array of Integer;  // hash slot → id+1 (0 = empty)

function PoolHashOf(const S: string): Cardinal;
var
  i: Integer;
begin
  Result := 2166136261;
  for i := 1 to Length(S) do
    Result := (Result xor Byte(S[i])) * 16777619;
end;

procedure PoolGrow;
var
  OldBuckets: array of Integer;
  i, Id: Integer;
  Slot, Mask: Cardinal;
begin
  OldBuckets := PoolBuckets;
  PoolBuckets := nil;
  if Length(OldBuckets) = 0 then
    SetLength(PoolBuckets, 1024)
  else
    SetLength(PoolBuckets, Length(OldBuckets) * 2);
  // SetLength zero-fills fresh buckets
  Mask := Cardinal(Length(PoolBuckets) - 1);
  for i := 0 to High(OldBuckets) do
    if OldBuckets[i] <> 0 then
    begin
      Id := OldBuckets[i];
      Slot := PoolHashOf(PoolStrings[Id - 1]) and Mask;
      while PoolBuckets[Slot] <> 0 do
        Slot := (Slot + 1) and Mask;
      PoolBuckets[Slot] := Id;
    end;
end;

function SSAPoolIntern(const S: string): Integer;
var
  Slot, Mask: Cardinal;
  Id: Integer;
begin
  if S = '' then Exit(0);
  if PoolCount = 0 then
  begin
    SetLength(PoolStrings, 64);
    PoolStrings[0] := '';           // id 0 = ''
    PoolCount := 1;
    PoolGrow;
  end
  else if (PoolCount * 5) div 3 >= Length(PoolBuckets) then   // load > 60%
    PoolGrow;

  Mask := Cardinal(Length(PoolBuckets) - 1);
  Slot := PoolHashOf(S) and Mask;
  while PoolBuckets[Slot] <> 0 do
  begin
    Id := PoolBuckets[Slot];
    if PoolStrings[Id - 1] = S then
      Exit(Id - 1);
    Slot := (Slot + 1) and Mask;
  end;
  // Not found: append
  if PoolCount >= Length(PoolStrings) then
    SetLength(PoolStrings, Length(PoolStrings) * 2);
  PoolStrings[PoolCount] := S;
  PoolBuckets[Slot] := PoolCount + 1;
  Result := PoolCount;
  Inc(PoolCount);
end;

function SSAPoolGet(Id: Integer): string;
begin
  if (Id <= 0) or (Id >= PoolCount) then Exit('');
  Result := PoolStrings[Id];
end;

function TSSAValue.GetVarName: string;
begin
  Result := SSAPoolGet(VarNameId);
end;

procedure TSSAValue.SetVarName(const AValue: string);
begin
  VarNameId := SSAPoolIntern(AValue);
end;

function TSSAValue.GetConstString: string;
begin
  Result := SSAPoolGet(ConstStringId);
end;

procedure TSSAValue.SetConstString(const AValue: string);
begin
  ConstStringId := SSAPoolIntern(AValue);
end;

function TSSAValue.GetLabelName: string;
begin
  Result := SSAPoolGet(LabelNameId);
end;

procedure TSSAValue.SetLabelName(const AValue: string);
begin
  LabelNameId := SSAPoolIntern(AValue);
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

function TSSAProgram.Fingerprint(out AInstrCount: Integer): QWord;
// FNV-1a over every instruction's opcode and operands. Cheap enough to call around each pass in a
// diagnostic run, and exact enough that an unchanged hash means the pass rewrote NOTHING.
const
  FNV_OFFSET = QWord(14695981039346656037);
  FNV_PRIME  = QWord(1099511628211);
var
  b, i, k: Integer;
  Block: TSSABasicBlock;
  Instr: TSSAInstruction;
  H: QWord;

  procedure Mix(V: QWord); inline;
  begin
    H := (H xor V) * FNV_PRIME;
  end;

  procedure MixVal(const Val: TSSAValue); inline;
  begin
    Mix(QWord(Ord(Val.Kind)));
    Mix(QWord(Ord(Val.RegType)));
    Mix(QWord(Val.RegIndex));
    Mix(QWord(Val.Version));
    Mix(QWord(Val.ConstInt));
    Mix(PQWord(@Val.ConstFloat)^);
    Mix(QWord(Val.ArrayIndex));
    Mix(QWord(Val.VarNameId));
    Mix(QWord(Val.ConstStringId));
    Mix(QWord(Val.LabelNameId));
  end;

begin
  H := FNV_OFFSET;
  AInstrCount := 0;
  for b := 0 to FBlocks.Count - 1 do
  begin
    Block := FBlocks[b];
    Mix(QWord(b));
    for i := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[i];
      Inc(AInstrCount);
      Mix(QWord(Ord(Instr.OpCode)));
      MixVal(Instr.Dest);
      MixVal(Instr.Src1);
      MixVal(Instr.Src2);
      MixVal(Instr.Src3);   // no Immediate at SSA level: it is the bytecode encoding downstream
      for k := 0 to High(Instr.PhiSources) do
        MixVal(Instr.PhiSources[k].Value);
    end;
  end;
  Result := H;
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
