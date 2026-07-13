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
  bcGroupWeb        = $0800;  // Group 8: Web operations (WEB_MODE only)
  bcGroupGraphics   = $0A00;  // Group 10: Graphics

  // === GROUP 8: WEB OPERATIONS (0x08xx) - WEB_MODE only ===
  {$IFDEF WEB_MODE}
  bcWebGetParam     = bcGroupWeb + $01;  // GET$("nome") - HTML-escaped
  bcWebPostParam    = bcGroupWeb + $02;  // POST$("nome") - HTML-escaped
  bcWebGetRaw       = bcGroupWeb + $03;  // GETRAW$("nome") - raw, unsanitized
  bcWebPostRaw      = bcGroupWeb + $04;  // POSTRAW$("nome") - raw, unsanitized
  bcWebHtmlEncode   = bcGroupWeb + $05;  // HTML$(s) - escape HTML entities
  bcWebUrlEncode    = bcGroupWeb + $06;  // URL$(s) - URL encode
  bcWebMethod       = bcGroupWeb + $07;  // METHOD$ - "GET" or "POST"
  bcWebPath         = bcGroupWeb + $08;  // PATH$ - requested path
  bcWebQuery        = bcGroupWeb + $09;  // QUERY$ - full query string
  bcWebHeader       = bcGroupWeb + $0A;  // HEADER$("nome") - request header
  bcWebSetHeader    = bcGroupWeb + $0B;  // SETHEADER "name", "value"
  bcWebStatus       = bcGroupWeb + $0C;  // STATUS code - set HTTP status
  {$ENDIF}
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
  bcPrintUsingStage = bcGroupCore + 149; // stage one stringified value for a runtime-format PRINT USING
  bcPrintUsingRun   = bcGroupCore + 150; // run a runtime-format PRINT USING over the staged values
  bcRecordNewArrayInd = bcGroupCore + 151; // allocate a record per element of the member array whose FArrays handle is in IntRegs[Src1]; Immediate = packed slot counts (int|float<<16|str<<32|typeId<<48)
  bcRecordNewBlock    = bcGroupCore + 152; // Callocate(n,SizeOf(T)) of a UDT: allocate IntRegs[Src1] CONSECUTIVE shared records, Dest = first handle; Immediate = packed slot counts. "p[i]" (first+i) indexes the i-th.
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
  bcRmdir           = bcGroupCore + 137; // RMDIR/RD "path": Remove directory (FreeBASIC/QB)
  bcRaiseError      = bcGroupCore + 138; // ERROR <n>: raise a user runtime error (Src1 = number)
  bcMoveFile        = bcGroupCore + 89;  // MOVE/MV "src","dest": Move file
  // Error handling
  bcTrap            = bcGroupCore + 90;  // TRAP linenum: Set error handler line
  bcResume          = bcGroupCore + 91;  // RESUME: Continue after error at error line
  bcResumeNext      = bcGroupCore + 92;  // RESUME NEXT: Continue after error at next statement
  // FreeBASIC label-based error handling (Immediate = resolved handler/target PC).
  bcOnError         = bcGroupCore + 135; // ON ERROR GOTO label: set TrapPC directly from Immediate
  bcResumeLabel     = bcGroupCore + 136; // RESUME label: jump to Immediate, leave error handler
  bcModFloat        = bcGroupCore + 93;  // Float modulo (A - Floor(A/B) * B)
  bcFrame           = bcGroupCore + 94;  // FRAME [fps]: Wait for frame sync (default 60fps)
  // SUB/FUNCTION call frames (M2). v1: Immediate = procedure entry PC (resolved via the
  // procedure label, like bcCall). The VM snapshots/restores the whole register banks
  // around the call; arguments and the result travel via the transfer registers (bcXfer*).
  bcCallSub         = bcGroupCore + 95;  // Call a SUB/FUNCTION: save frame, pass args, jump
  bcReturnSub       = bcGroupCore + 96;  // Return from SUB/FUNCTION: deliver result, restore frame
  bcCallSubIndirect = bcGroupCore + 140; // FreeBASIC function-pointer call: like bcCallSub but the target entry PC is read from Src1 (int register)
  bcSetEnviron      = bcGroupCore + 141; // SETENVIRON "NAME=value": set an environment variable (Src1 = string)
  bcShell           = bcGroupCore + 142; // SHELL cmd: run a command via the shell (Src1 = string; Dest = int exit code)
  // Unsigned 64-bit integer operations (UInteger/ULongInt). The registers hold the raw two's-complement
  // bits; only compare/div/mod differ from the signed forms (add/sub/mul/and/or/xor are bit-identical),
  // so they reinterpret the Int64 registers as QWord. Chosen by IsUnsigned64Expr at SSA emit time.
  bcCmpLtUInt       = bcGroupCore + 143; // unsigned <  (QWord)
  bcCmpGtUInt       = bcGroupCore + 144; // unsigned >  (QWord)
  bcCmpLeUInt       = bcGroupCore + 145; // unsigned <= (QWord)
  bcCmpGeUInt       = bcGroupCore + 146; // unsigned >= (QWord)
  bcDivUInt         = bcGroupCore + 147; // unsigned \  (QWord div)
  bcModUInt         = bcGroupCore + 148; // unsigned Mod (QWord mod)
  // Argument/result transfer registers (M2): separate VM-side banks, never saved/restored,
  // so they carry args (caller->callee) and the result (callee->caller) across the frame
  // save/restore. Store: Src1=value reg, Immediate=slot. Load: Dest=reg, Immediate=slot.
  bcXferStoreInt    = bcGroupCore + 97;
  bcXferStoreFloat  = bcGroupCore + 98;
  bcXferStoreString = bcGroupCore + 99;
  bcXferLoadInt     = bcGroupCore + 100;
  bcXferLoadFloat   = bcGroupCore + 101;
  bcXferLoadString  = bcGroupCore + 102;
  // UDT/record heap (M3). A record instance is a heap block of typed slot arrays; a handle
  // (index into the VM record heap) lives in an int register. bcRecordNew carries the per-bank
  // slot counts in Src1/Src2/Immediate; field access uses (handle reg, slot immediate).
  bcRecordNew        = bcGroupCore + 103;  // Dest=handle; Src1=intCount, Src2=floatCount, Imm=strCount
  bcRecordLoadInt    = bcGroupCore + 104;  // Dest=int reg; Src1=handle; Imm=slot
  bcRecordLoadFloat  = bcGroupCore + 105;  // Dest=float reg; Src1=handle; Imm=slot
  bcRecordLoadString = bcGroupCore + 106;  // Dest=string reg; Src1=handle; Imm=slot
  bcRecordStoreInt   = bcGroupCore + 107;  // Src1=handle; Src2=int value; Imm=slot
  bcRecordStoreFloat = bcGroupCore + 108;  // Src1=handle; Src2=float value; Imm=slot
  bcRecordStoreString= bcGroupCore + 109;  // Src1=handle; Src2=string value; Imm=slot
  // Array-of-UDT eager init (M3.1): allocate one record per element of an int (handle) array
  // and store the handles. Src1=array id; Immediate packs slot counts (int|float<<16|str<<32).
  bcRecordNewArray   = bcGroupCore + 110;
  // OOP virtual dispatch (M4.3): read an instance's runtime type-id (handle -> type-id).
  bcRecordTypeId     = bcGroupCore + 111;  // Dest=int reg; Src1=handle reg
  // FreeBASIC DELETE: release a NEW'd (shared-region) record and recycle its slot. Src1=handle reg.
  bcRecordFree       = bcGroupCore + 133;
  // Block-scoped record reclamation (M8). bcRecMarkPush snapshots the current record high-water mark
  // onto a block-mark stack at a loop-body entry; bcRecMarkPop reclaims to the last mark at the body
  // exit (after the compiler-emitted destructors), so a DIM inside a loop is freed each iteration.
  // No operands. The mark stack is independent of the frame save/restore (which also resets it).
  bcRecMarkPush      = bcGroupCore + 112;
  bcRecMarkPop       = bcGroupCore + 113;
  // OS threading (M5.2, FreeBASIC API). bcLoadProcAddr resolves a SUB's PROC_<name> label to its
  // entry PC into an int register (Dest=int reg; Immediate=entry PC, filled by the label fixup like
  // bcCallSub). bcThreadCreate spawns an RTL worker thread running that SUB on a fresh execution
  // context (Dest=int handle; Src1=proc-addr reg; Src2=param reg → the worker's XferInt[0]).
  // bcThreadWait joins a worker by handle (Src1=handle reg).
  bcLoadProcAddr     = bcGroupCore + 114;
  bcThreadCreate     = bcGroupCore + 115;
  bcThreadWait       = bcGroupCore + 116;
  // M5.5: ThreadSelf (Dest=int = current thread handle, 0 on main); ThreadDetach (Src1=handle).
  bcThreadSelf       = bcGroupCore + 126;
  bcThreadDetach     = bcGroupCore + 127;
  // Round a float to the nearest integer (round-to-even, banker's), for FreeBASIC CINT/CLNG/... (B1.3).
  // Dest = int reg, Src1 = float reg. (bcFloatToInt truncates; bcMathInt floors — neither is CINT.)
  bcFloatRound       = bcGroupCore + 128;
  // Type-width narrowing (B1.5 phase): wrap/sign-extend an Int64 to a narrower integer width, or
  // round a Double to single precision. FreeBASIC narrows only at conversion/store to a typed dest,
  // never on arithmetic - so these are pure unary ops off the hot arithmetic path.
  //   bcNarrowInt: Dest=int reg, Src1=int reg, Immediate=width code
  //     1=s8 2=u8 3=s16 4=u16 5=s32 6=u32 (s64/u64 need no bit change -> never emitted)
  //   bcNarrowSingle: Dest=float reg, Src1=float reg -> Double(Single(x))
  bcNarrowInt        = bcGroupCore + 129;
  bcNarrowSingle     = bcGroupCore + 130;
  // FreeBASIC bit shifts (integer): Dest = Src1 shifted by Src2 bits.
  bcShl              = bcGroupCore + 131;  // SHL: shift left
  bcShr              = bcGroupCore + 132;  // SHR: arithmetic (sign-propagating) shift right
  bcShrUInt          = bcGroupCore + 153;  // SHR on an unsigned operand: logical (zero-filling) shift right
  bcRandomize        = bcGroupCore + 134;  // RANDOMIZE: seed the RNG (Src1=seed reg; Immediate=1 seed / 0 time-based)
  // Mutexes (M5.4, FreeBASIC API), thin wrappers over TRTLCriticalSection. bcMutexCreate writes a
  // fresh mutex handle into an int register (Dest); Lock/Unlock/Destroy take a handle reg (Src1).
  bcMutexCreate      = bcGroupCore + 117;
  bcMutexLock        = bcGroupCore + 118;
  bcMutexUnlock      = bcGroupCore + 119;
  bcMutexDestroy     = bcGroupCore + 120;
  // Condition variables (M5.4, FB API). bcCondCreate writes a handle (Dest); bcCondWait takes a cond
  // handle (Src1) and a mutex handle (Src2) and atomically unlocks+waits+relocks; Signal/Broadcast/
  // Destroy take a cond handle (Src1).
  bcCondCreate       = bcGroupCore + 121;
  bcCondWait         = bcGroupCore + 122;
  bcCondSignal       = bcGroupCore + 123;
  bcCondBroadcast    = bcGroupCore + 124;
  bcCondDestroy      = bcGroupCore + 125;
  bcAssert           = bcGroupCore + 139;  // ASSERT/ASSERTWARN: if Src1=0 print Src2 (string); Immediate bit0 = halt

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
  // FreeBASIC string functions (B1.2). All Dest=string, Src1=string (single-argument).
  bcStrLTrim        = bcGroupString + 12;  // LTRIM(s) - remove leading spaces
  bcStrRTrim        = bcGroupString + 13;  // RTRIM(s) - remove trailing spaces
  bcStrTrim         = bcGroupString + 14;  // TRIM(s)  - remove leading+trailing spaces
  bcStrUCase        = bcGroupString + 15;  // UCASE(s) - upper case
  bcStrLCase        = bcGroupString + 16;  // LCASE(s) - lower case
  bcStrInstrRev     = bcGroupString + 17;  // INSTRREV(s, sub) - last occurrence (Dest=int, Src1/Src2=string)
  bcStrSpace        = bcGroupString + 18;  // SPACE(n) - n spaces (Dest=string, Src1=int count)
  // FreeBASIC numeric<->string conversions (B1.3).
  bcStrOct          = bcGroupString + 19;  // OCT(n)  - octal string (Dest=string, Src1=int)
  bcStrBin          = bcGroupString + 20;  // BIN(n)  - binary string (Dest=string, Src1=int)
  bcStrValInt       = bcGroupString + 21;  // VALINT/VALLNG/VALUINT(s) - parse integer (Dest=int, Src1=string)
  bcStrString       = bcGroupString + 22;  // STRING(n,ch) - n copies of a char (Dest=string, Src1=int count, Src2=int charcode)
  bcStrTrimSet      = bcGroupString + 23;  // LTRIM/RTRIM/TRIM(s, set) - trim a substring from the ends (Dest/Src1/Src2=string, Immediate=mode 0=both/1=left/2=right [|4=Any char-set])
  bcStrInstrRevAny  = bcGroupString + 24;  // INSTRREV(str, Any set) - last position of any char in set (Dest=int, Src1/Src2=string)
  // FreeBASIC WSTRING (UTF-8 storage, codepoint-indexed). Same string bank; only width-aware ops differ.
  bcStrLenW         = bcGroupString + 25;  // LEN(wstring) - Unicode codepoint count (Dest=int, Src1=string)
  bcStrLeftW        = bcGroupString + 26;  // LEFT$(wstring, n) - first n codepoints (Dest/Src1=string, Src2=int n)
  bcStrRightW       = bcGroupString + 27;  // RIGHT$(wstring, n) - last n codepoints (Dest/Src1=string, Src2=int n)
  bcStrMidW         = bcGroupString + 28;  // MID$(wstring, start[,len]) - codepoint substring (Dest/Src1=string, Src2=int start, Immediate=int len reg)
  bcStrInstrW       = bcGroupString + 29;  // INSTR(wstring, sub) - codepoint position of first occurrence (Dest=int, Src1/Src2=string)
  bcStrInstrRevW    = bcGroupString + 30;  // INSTRREV(wstring, sub) - codepoint position of last occurrence (Dest=int, Src1/Src2=string)
  bcStrSAdd         = bcGroupString + 33;  // SADD(s) - raw byte-heap pointer to a NUL-terminated copy of s (Dest=int raw ptr, Src1=string)
  bcFileExists      = bcGroupString + 40;  // FILEEXISTS(path) - -1 if the file exists else 0 (Dest=int, Src1=string)
  bcCurDir          = bcGroupString + 41;  // CURDIR$ - current working directory (Dest=string)
  bcEnviron         = bcGroupString + 42;  // ENVIRON$(name) - environment variable value (Dest=string, Src1=string)
  bcFileLen         = bcGroupString + 43;  // FILELEN(path) - file size in bytes (Dest=int, Src1=string)
  bcExePath         = bcGroupString + 44;  // EXEPATH - directory of the running program (Dest=string)
  bcStrFormat       = bcGroupString + 45;  // FORMAT(num, mask) - formatted string (Dest=string, Src1=mask, Immediate=value float reg)
  bcCommand         = bcGroupString + 46;  // COMMAND$(index) - command-line argument(s) (Dest=string, Src1=index int)
  bcFileDateTime    = bcGroupString + 47;  // FILEDATETIME(path) - last-modified date serial (Dest=float, Src1=string)
  bcDateStr         = bcGroupString + 34;  // DATE / TIME -> string (Dest=string; Immediate: 0=DATE "mm-dd-yyyy", 1=TIME "hh:mm:ss")
  bcDateName        = bcGroupString + 35;  // MONTHNAME(n)/WEEKDAYNAME(n) -> string (Dest=string, Src1=int; Imm: 0=MONTHNAME, 1=WEEKDAYNAME)
  bcStrMkInt        = bcGroupString + 36;  // MKI/MKL/MKSHORT/MKLONGINT - pack int into a binary string (Dest=string, Src1=int, Imm=byte width)
  bcStrMkFloat      = bcGroupString + 37;  // MKS/MKD - pack float into an IEEE binary string (Dest=string, Src1=float, Imm=byte width)
  bcStrCvInt        = bcGroupString + 38;  // CVI/CVL/CVSHORT/CVLONGINT - unpack int from a binary string (Dest=int, Src1=string, Imm=byte width)
  bcStrCvFloat      = bcGroupString + 39;  // CVS/CVD - unpack float from an IEEE binary string (Dest=float, Src1=string, Imm=byte width)
  bcStrInstrAny     = bcGroupString + 48;  // INSTR([start,] str, Any set) - FIRST position of any char in set (Dest=int, Src1/Src2=string, Imm=int start reg)
  bcStrWChr         = bcGroupString + 31;  // WCHR(n) - UTF-8 bytes of Unicode codepoint n (Dest=string, Src1=int)
  bcStrWStringN     = bcGroupString + 32;  // WSTRING(n,cp) - n copies of the UTF-8 char for codepoint cp (Dest=string, Src1=int n, Src2=int cp)

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
  // FreeBASIC math functions (all float Dest/Src1; ATAN2 takes Src1=y, Src2=x).
  bcMathAcos        = bcGroupMath + 15;  // ACOS(x) - arccosine
  bcMathAsin        = bcGroupMath + 16;  // ASIN(x) - arcsine
  bcMathAtan2       = bcGroupMath + 17;  // ATAN2(y, x) - two-argument arctangent
  bcMathFix         = bcGroupMath + 18;  // FIX(x) - truncate toward zero
  bcMathFrac        = bcGroupMath + 19;  // FRAC(x) - fractional part
  // FreeBASIC date/time. Date serial = Double (FPC TDateTime epoch 1899-12-30 = VB/FB serial).
  bcDateNow         = bcGroupMath + 20;  // Dest=float; Immediate: 0=NOW, 1=TIMER
  bcDateDecode      = bcGroupMath + 21;  // Dest=int, Src1=float serial; Imm: 0=YEAR 1=MONTH 2=DAY 3=HOUR 4=MINUTE 5=SECOND 6=WEEKDAY
  bcDateSerial      = bcGroupMath + 22;  // Dest=float, Src1=int y, Src2=int m, Immediate=int d register
  bcTimeSerial      = bcGroupMath + 23;  // Dest=float, Src1=int h, Src2=int m, Immediate=int s register
  bcDateValue       = bcGroupMath + 24;  // Dest=float, Src1=string; Imm: 0=DATEVALUE, 1=TIMEVALUE
  bcIsDate          = bcGroupMath + 25;  // Dest=int (bool), Src1=string
  bcDateAdd         = bcGroupMath + 26;  // DATEADD(interval$,n,serial): Dest=float, Src1=string, Src2=int n, Immediate=float serial reg
  bcDateDiff        = bcGroupMath + 27;  // DATEDIFF(interval$,s1,s2): Dest=int, Src1=string, Src2=float s1, Immediate=float s2 reg
  bcDatePart        = bcGroupMath + 28;  // DATEPART(interval$,serial): Dest=int, Src1=string, Src2=float serial
  bcSetClock        = bcGroupMath + 29;  // SETDATE/SETTIME str: Src1=string; Imm: 0=SETDATE, 1=SETTIME (side-effecting)
  // Hyperbolic functions (FreeBASIC via the C runtime; FPC's Math unit provides identical IEEE results).
  bcMathSinh        = bcGroupMath + 30;  // SINH(x) - hyperbolic sine
  bcMathCosh        = bcGroupMath + 31;  // COSH(x) - hyperbolic cosine
  bcMathTanh        = bcGroupMath + 32;  // TANH(x) - hyperbolic tangent
  bcMathAsinh       = bcGroupMath + 33;  // ASINH(x) - inverse hyperbolic sine
  bcMathAcosh       = bcGroupMath + 34;  // ACOSH(x) - inverse hyperbolic cosine (x >= 1)
  bcMathAtanh       = bcGroupMath + 35;  // ATANH(x) - inverse hyperbolic tangent (|x| < 1)

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
  // FreeBASIC array bound queries (B1.4). Dest=int result, Src1=array id, Src2=0-based dim index reg.
  bcArrayLBound     = bcGroupArray + 9;   // LBOUND(arr[, dim]) - lower bound of a dimension
  bcArrayUBound     = bcGroupArray + 10;  // UBOUND(arr[, dim]) - upper bound of a dimension
  // FreeBASIC array statements (B1.4). Src1 = array id.
  bcArrayErase      = bcGroupArray + 11;  // ERASE arr - reset elements to default (keep size)
  bcArrayRedim      = bcGroupArray + 12;  // REDIM [PRESERVE] arr(ub): Src2=ub reg, Immediate bit0=preserve
  // FreeBASIC pointer dereference. The "address" is a runtime int register holding the id of a
  // 1-element backing array (element 0). Load: Dest=value, Src1=address reg. Store: Src1=address
  // reg, Src2=value reg.
  bcRefLoadInt      = bcGroupArray + 13;
  bcRefLoadFloat    = bcGroupArray + 14;
  bcRefLoadString   = bcGroupArray + 15;
  bcRefStoreInt     = bcGroupArray + 16;
  bcRefStoreFloat   = bcGroupArray + 17;
  bcRefStoreString  = bcGroupArray + 18;
  // @obj.field : pack a record-field pointer. Dest=packed addr, Src1=record handle, Immediate=field slot.
  bcRefAddrField    = bcGroupArray + 19;
  // FreeBASIC raw byte heap (Allocate family). Raw pointer = RAWPTR_TAG | byte offset.
  bcRawAlloc        = bcGroupArray + 20;  // Dest=raw ptr; Src1=byte count reg (zeroed)
  bcRawFree         = bcGroupArray + 21;  // Src1=raw ptr
  bcRawRealloc      = bcGroupArray + 22;  // Dest=raw ptr; Src1=old ptr; Src2=byte count reg
  bcRawLoadInt      = bcGroupArray + 23;  // Dest=int; Src1=raw ptr; Immediate=raw type code (width)
  bcRawLoadFloat    = bcGroupArray + 24;  // Dest=float; Src1=raw ptr; Immediate=type code (single/double)
  bcRawStoreInt     = bcGroupArray + 25;  // Src1=raw ptr; Src2=int value; Immediate=type code
  bcRawStoreFloat   = bcGroupArray + 26;  // Src1=raw ptr; Src2=float value; Immediate=type code
  bcArrayRedimPush  = bcGroupArray + 27;  // REDIM multi-dim: push one upper bound (Src1=int ub reg) onto the pending-dims list
  bcArrayRedimN     = bcGroupArray + 28;  // REDIM multi-dim commit: Src1=array ref, Immediate bit0=preserve; uses the pushed upper bounds
  bcArrayIdxPush    = bcGroupArray + 29;  // Runtime multi-dim index: push one index (Src1=int reg) onto the pending-index list
  bcArrayIdxResolve = bcGroupArray + 30;  // Runtime multi-dim index: Dest=linear index (int), Src1=array ref; row-major from the array's CURRENT dimensions (for REDIM'd arrays)

  // FreeBASIC raw-memory block ops on the byte heap (FB_MEMCOPY/FB_MEMMOVE/CLEAR).
  bcRawMemCopy      = bcGroupArray + 31;  // Dest=dst ptr(result); Src1=dst raw ptr; Src2=src raw ptr; Immediate=byte count reg
  bcRawMemMove      = bcGroupArray + 32;  // same layout as bcRawMemCopy; overlap-safe (identical here, FPC Move is overlap-safe)
  bcRawClear        = bcGroupArray + 33;  // Src1=dst raw ptr; Src2=byte value reg; Immediate=byte count reg
  bcArrayBind       = bcGroupArray + 34;  // Array BYREF param bind: Src1=param array id, Src2=arg array id (both immediates). Saves FArrays[Src1] then aliases it to FArrays[Src2] (shares the element data). No registers.
  bcArrayUnbind     = bcGroupArray + 35;  // Restore the last saved FArrays[Src1] (Src1=param array id immediate). Pops the bind save-stack.
  bcArrayBindApply  = bcGroupArray + 36;  // Commit the top N pending array binds (Immediate=N): assign each param slot its snapshotted arg. Two-phase so swapped/overlapping binds (recursive a()/b()) don't corrupt.
  // Array members of a UDT: the field holds an FArrays handle (per instance); element access is INDIRECT
  // (array id from a register, not a compile-time immediate). Src1=handle reg, Src2=linear index reg.
  bcArrayLoadIndInt    = bcGroupArray + 37;  // Dest(int)  = FArrays[IntRegs[Src1]].IntData[IntRegs[Src2]]
  bcArrayLoadIndFloat  = bcGroupArray + 38;  // Dest(float)= FArrays[IntRegs[Src1]].FloatData[IntRegs[Src2]]
  bcArrayLoadIndString = bcGroupArray + 39;  // Dest(str)  = FArrays[IntRegs[Src1]].StringData[IntRegs[Src2]]
  bcArrayStoreIndInt   = bcGroupArray + 40;  // FArrays[IntRegs[Src1]].IntData[IntRegs[Src2]]   := IntRegs[Dest]   (Dest = value, READ)
  bcArrayStoreIndFloat = bcGroupArray + 41;  // FArrays[IntRegs[Src1]].FloatData[IntRegs[Src2]] := FloatRegs[Dest] (Dest = value, READ)
  bcArrayStoreIndString= bcGroupArray + 42;  // FArrays[IntRegs[Src1]].StringData[IntRegs[Src2]]:= StringRegs[Dest](Dest = value, READ)
  bcArrayIdxResolveInd = bcGroupArray + 43;  // Dest(int) = row-major linear index from FArrays[IntRegs[Src1]].Dimensions (member multi-dim); consumes the bcArrayIdxPush list
  bcMemberArrayRedim   = bcGroupArray + 44;  // REDIM a UDT array member: Src1=record-handle reg; Immediate packs (slot<<8)|(elemType<<4)|preserve; allocates the FArrays entry lazily and writes the handle back to the record slot; consumes the bcArrayRedimPush upper-bound list
  bcArrayLBoundInd     = bcGroupArray + 45;  // LBOUND of a UDT array member: Dest(int)=FArrays[IntRegs[Src1]].LowerBounds[IntRegs[Src2]] (0 if unallocated)
  bcArrayUBoundInd     = bcGroupArray + 46;  // UBOUND of a UDT array member: Dest(int)=lower+size-1 (-1 if unallocated)
  bcArrayCopyContents  = bcGroupArray + 47;  // deep-copy FArrays[IntRegs[Src1]] <- FArrays[IntRegs[Src2]] (independent storage: Dimensions/LowerBounds/*Data via Copy); value semantics of an array UDT member
  bcArrayCopyRecords   = bcGroupArray + 48;  // value-copy an array-of-UDT member: FArrays[IntRegs[Src1]] (dest) gets independent element records each holding a copy of the corresponding FArrays[IntRegs[Src2]] (src) element's contents; Immediate = packed element UDT slot counts
  bcArrayBindInd       = bcGroupArray + 49;  // Array BYREF param bind whose ARG is a UDT array member: Src1=param array id (immediate), Src2=int reg holding the member's runtime FArrays handle. Otherwise identical to bcArrayBind (pushes the same save-stack entry, committed by bcArrayBindApply, popped by bcArrayUnbind).

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
  bcPrintEnd        = bcGroupIO + 11;  // Reset reverse mode after PRINT (C128 behavior)
  // Input
  bcInput           = bcGroupIO + 12;
  bcInputInt        = bcGroupIO + 13;
  bcInputFloat      = bcGroupIO + 14;
  bcInputString     = bcGroupIO + 15;
  // FreeBASIC typed print (B1.5 phase C). Src1 = int register holding the value.
  bcPrintBool       = bcGroupIO + 16;  // print a BOOLEAN as "true"/"false"
  bcPrintUInt       = bcGroupIO + 17;  // print an Int64 as an unsigned 64-bit value
  // WINPUT(n [, [#]f]): read n wide characters (Unicode codepoints, UTF-8 encoded like every WSTRING in
  // this VM) from file handle Src2, or from the keyboard when Src2's register holds 0.
  // Dest = string register, Src1 = count register, Src2 = file-handle register.
  bcWInputChars     = bcGroupIO + 18;
  // INPUT(n [, [#]f]): the byte-oriented sibling of WINPUT. Same operands.
  bcInputChars      = bcGroupIO + 19;
  // SCREEN(row, col [, colorflag]): read back a console cell. Rows and columns are 1-based, as FB
  // documents them. colorflag = 0 yields the character code; otherwise the colour attribute, packed
  // the way FreeBASIC packs it for a palette console of up to 4 bits per pixel: the high nibble is
  // the cell background, the low nibble the foreground.
  // Dest = int result, Src1 = row register, Src2 = column register, Immediate = colorflag register.
  bcConScreen       = bcGroupIO + 20;
  // MODERN LOCATE row, column: position the console TEXT cursor (1-based, as FreeBASIC counts it).
  // Src1 = row register, Src2 = column register. CLASSIC LOCATE is bcGraphicLocate, the pixel cursor.
  bcConLocate       = bcGroupIO + 21;
  // VIEW PRINT [firstrow TO lastrow]: set the console's text print area (its scroll region). Rows are
  // 1-based; a 0 in either operand means "the whole screen". The text cursor moves to the start of the
  // first row. Src1 = first-row register, Src2 = last-row register.
  bcConViewPrint    = bcGroupIO + 22;

  // === GROUP 5: SPECIAL VARIABLES (0x05xx) ===
  bcLoadTI          = bcGroupSpecial + 0;   // TI: jiffies since start
  bcLoadTIS         = bcGroupSpecial + 1;   // TI$: current time HHMMSS
  bcStoreTIS        = bcGroupSpecial + 2;   // TI$ = value
  bcLoadDTS         = bcGroupSpecial + 3;   // DT$: current date YYYYMMDD
  bcFre             = bcGroupSpecial + 4;   // FRE(x): available memory bytes
  bcLoadEL          = bcGroupSpecial + 5;   // EL: last error line number
  bcLoadER          = bcGroupSpecial + 6;   // ER: last error code
  bcLoadERRS        = bcGroupSpecial + 7;   // ERR$: last error message (variable)
  // Memory operations
  bcPeek            = bcGroupSpecial + 8;   // PEEK(address): read from memory-mapped location
  bcPoke            = bcGroupSpecial + 9;   // POKE address, value: write to memory-mapped location
  bcLoadCWDS        = bcGroupSpecial + 10;  // CWD$: current working directory
  bcCsrlin          = bcGroupSpecial + 11;  // CSRLIN: current text cursor row
  bcLoadDS          = bcGroupSpecial + 12;  // DS: Commodore disk status code (= last file error code)
  bcLoadDSS         = bcGroupSpecial + 13;  // DS$: Commodore disk status message line (Dest=string)
  bcLoadST          = bcGroupSpecial + 14;  // ST: Kernal I/O status byte (bit 6 = EOF on the last GET#)
  bcLoadERFN        = bcGroupSpecial + 15;  // ERFN: name of the procedure where the last error occurred
  bcLoadERMN        = bcGroupSpecial + 16;  // ERMN: name of the module (source file) of the last error

  // === GROUP 6: FILE I/O (0x06xx) ===
  bcDopen           = bcGroupFileIO + 0;    // DOPEN #handle, "filename" [, mode$]
  bcDclose          = bcGroupFileIO + 1;    // DCLOSE #handle
  bcOpen            = bcGroupFileIO + 2;    // OPEN (legacy C64/C128 style)
  bcClose           = bcGroupFileIO + 3;    // CLOSE (legacy C64/C128 style)
  bcGetFile         = bcGroupFileIO + 4;    // GET# file, var
  bcInputFile       = bcGroupFileIO + 5;    // INPUT# file, vars
  bcPrintFile       = bcGroupFileIO + 6;    // PRINT# file, exprs
  bcCmd             = bcGroupFileIO + 7;    // CMD file [, expr]
  bcAppend          = bcGroupFileIO + 8;    // APPEND #handle, data
  bcDclear          = bcGroupFileIO + 9;    // DCLEAR - close all file handles
  bcRecord          = bcGroupFileIO + 10;   // RECORD #handle, position
  bcPrintFileNewLine = bcGroupFileIO + 11;  // PRINT# newline: Write CR to file
  bcPrintFileFloat  = bcGroupFileIO + 12;   // PRINT# file, float expr
  bcPrintFileInt    = bcGroupFileIO + 13;   // PRINT# file, int expr
  bcInputFileFloat  = bcGroupFileIO + 14;   // INPUT# file, float var
  bcInputFileInt    = bcGroupFileIO + 15;   // INPUT# file, int var
  bcFileQuery       = bcGroupFileIO + 16;   // FreeBASIC EOF/FREEFILE/LOF/LOC/SEEK: Dest=int result, Src1=handle reg, Immediate=query code (0=EOF,1=FREEFILE,2=LOF,3=LOC,4=SEEK)
  bcSeekSet         = bcGroupFileIO + 17;   // SEEK #n, pos statement: Src1=handle reg, Src2=position reg (1-based)
  bcInputFileLine   = bcGroupFileIO + 18;   // LINE INPUT# file, string var (whole line, no comma split): Dest=string var, Src1=handle
  bcPutBinInt       = bcGroupFileIO + 19;   // PUT #n: write 8 bytes of an integer (Src1=handle, Src2=int value)
  bcPutBinFloat     = bcGroupFileIO + 20;   // PUT #n: write 8 bytes of a double  (Src1=handle, Src2=float value)
  bcGetBinInt       = bcGroupFileIO + 21;   // GET #n: read 8 bytes into an integer (Dest=int value, Src1=handle)
  bcGetBinFloat     = bcGroupFileIO + 22;   // GET #n: read 8 bytes into a double  (Dest=float value, Src1=handle)
  bcPutBinStr       = bcGroupFileIO + 23;   // PUT #n: write a string as [int32 length][bytes] (Src1=handle, Src2=string value)
  bcGetBinStr       = bcGroupFileIO + 24;   // GET #n: read a length-prefixed string (Dest=string value, Src1=handle)
  bcFileAttr        = bcGroupFileIO + 25;   // FILEATTR(filenum, returntype) -> int info (Dest=int result, Src1=handle, Src2=returntype)
  bcFileSetEof      = bcGroupFileIO + 26;   // FILESETEOF filenum: truncate/extend to current position (Dest=int status, Src1=handle)

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
  bcSpriteDef       = bcGroupSprite + 12;   // SPRDEF [n]: Enter interactive sprite editor (sbv)
  bcSprSaveFile     = bcGroupSprite + 13;   // SPRSAVE "file": save all sprites to JSON
  bcSprLoadFile     = bcGroupSprite + 14;   // SPRLOAD "file": load all sprites from JSON
  bcSprSize         = bcGroupSprite + 15;   // SPRSIZE n, w, h: set sprite dimensions
  bcSprForm         = bcGroupSprite + 16;   // SPRFORM n, format: set sprite data format

  // === GROUP 10: GRAPHICS (0x0Axx) ===
  bcGraphicRGBA     = bcGroupGraphics + 0;
  bcGraphicSetMode  = bcGroupGraphics + 1;
  bcGraphicBox      = bcGroupGraphics + 2;
  bcGraphicCircle   = bcGroupGraphics + 3;
  bcGraphicDraw     = bcGroupGraphics + 4;
  bcGraphicLocate   = bcGroupGraphics + 5;
  bcGraphicRdot     = bcGroupGraphics + 6;
  bcGraphicGetMode  = bcGroupGraphics + 7;
  bcGraphicColor    = bcGroupGraphics + 8;   // COLOR source, color (1-based)
  bcSetColor        = bcGroupGraphics + 19;  // SETCOLOR source, color (0-based)
  bcGetColor        = bcGroupGraphics + 20;  // GETCOLOR(source) (0-based)
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
  bcPLoad           = bcGroupGraphics + 22;  // PLOAD "filename" - Load palette from JSON
  bcPSave           = bcGroupGraphics + 23;  // PSAVE "filename" - Save palette to JSON
  bcPRst            = bcGroupGraphics + 24;  // PRST - Reset palette to C64 default
  // FreeBASIC graphics (phase 1 slice — routed through IGraphicsBackend)
  bcGfxScreenRes    = bcGroupGraphics + 25;  // SCREENRES w,h : Src1=w, Src2=h
  bcGfxPset         = bcGroupGraphics + 26;  // PSET (x,y),color : Src1=x, Src2=y, Immediate=color reg
  bcGfxPoint        = bcGroupGraphics + 27;  // POINT(x,y) : Dest=color, Src1=x, Src2=y
  bcGfxPaint        = bcGroupGraphics + 28;  // PAINT (x,y),color : flood fill; Src1=x, Src2=y, Immediate=color reg
  // LINE (x1,y1)-(x2,y2),color[,B|BF] : Src1=x1, Src2=y1; Immediate [0-15]=x2 reg, [16-31]=y2 reg,
  //   [32-47]=color reg, [48-49]=shape flag (0=line, 1=box outline, 2=box filled)
  bcGfxLine         = bcGroupGraphics + 29;
  // CIRCLE (x,y),r[,color] : Src1=x, Src2=y; Immediate [0-15]=radius reg, [16-31]=color reg
  bcGfxCircle       = bcGroupGraphics + 30;
  // PALETTE index, r, g, b : set a palette entry. Src1=index reg, Src2=packed RGBA color reg
  bcGfxPalette      = bcGroupGraphics + 31;
  // __PALGET(index, which) : read palette component. Dest=result reg, Src1=index reg, Immediate=which (0=r,1=g,2=b)
  bcGfxPalGet       = bcGroupGraphics + 32;
  // PALETTE (no args) : reset the palette to the current mode default
  bcGfxPaletteReset = bcGroupGraphics + 33;
  // COLOR [fg][,bg] : set the current draw colours. Src1=fg reg, Src2=bg reg, Immediate bit0=hasFg, bit1=hasBg
  bcGfxColor        = bcGroupGraphics + 34;
  // read the current draw foreground (Dest=result) — used as the omitted-colour default for PSET/LINE/...
  bcGfxForeColor    = bcGroupGraphics + 35;
  // IMAGECREATE(w,h[,color]) : Dest=handle, Src1=w, Src2=h, Immediate=fill colour reg
  bcGfxImageCreate  = bcGroupGraphics + 36;
  // IMAGEDESTROY handle : Src1=handle
  bcGfxImageDestroy = bcGroupGraphics + 37;
  // __IMGINFO(handle, which) : Dest=result, Src1=handle, Immediate=which (0=width, 1=height)
  bcGfxImageInfo    = bcGroupGraphics + 38;
  // GET (x1,y1)-(x2,y2),dst : Src1=x1, Src2=y1; Immediate [0-15]=x2 reg, [16-31]=y2 reg, [32-47]=dst handle reg
  bcGfxGet          = bcGroupGraphics + 39;
  // PUT (x,y),src[,mode] : Src1=x, Src2=y; Immediate [0-15]=src handle reg, [16-31]=blit-mode ordinal (const)
  bcGfxPut          = bcGroupGraphics + 40;
  // __SCRINFO(which) : Dest=result, Immediate=which (0=w,1=h,2=depth,3=bpp,4=pitch,5=rate)
  bcGfxScreenInfo   = bcGroupGraphics + 41;
  // SCREENSET work[,visible] / FLIP : Src1=work page, Src2=visible page, Immediate bit0=hasWork,1=hasVisible,2=swap
  bcGfxScreenSet    = bcGroupGraphics + 42;
  // PCOPY src,dst / SCREENCOPY : Src1=src page, Src2=dst page, Immediate bit0=hasSrc,1=hasDst
  bcGfxPCopy        = bcGroupGraphics + 43;
  // WINDOW [SCREEN] (x1,y1)-(x2,y2) : Src1=x1, Src2=y1; Immediate [0-15]=x2 reg, [16-31]=y2 reg,
  //   bit32=has-bounds (else disable), bit33=SCREEN (no y-flip)
  bcGfxWindow       = bcGroupGraphics + 44;
  // __PMAP(coord, n) : Dest=result, Src1=coord, Immediate=n (0=lx->px,1=ly->py,2=px->lx,3=py->ly)
  bcGfxPMap         = bcGroupGraphics + 45;
  // VIEW [SCREEN] (x1,y1)-(x2,y2) : Src1=x1, Src2=y1; Immediate [0-15]=x2 reg, [16-31]=y2 reg,
  //   bit32=has-bounds (else reset), bit33=SCREEN (absolute coords, no offset)
  bcGfxView         = bcGroupGraphics + 46;
  // SCREEN mode [, , num_pages] : Src1=mode reg, Immediate=number of pages (mapped mode->resolution in VM)
  bcGfxScreen       = bcGroupGraphics + 47;
  // MULTIKEY(scancode) : Dest=result (-1/0), Src1=FB scancode reg — real-time key state via the input provider
  bcMultikey        = bcGroupGraphics + 48;
  // GETMOUSE snapshot : Dest=status (0 ok / 1 no mouse); caches x,y,wheel,buttons,clip in the VM
  bcGetmouse        = bcGroupGraphics + 49;
  // __MOUSEAXIS(which) : Dest=result, Immediate=which (0=x,1=y,2=wheel,3=buttons,4=clip) — reads the cache
  bcMouseAxis       = bcGroupGraphics + 50;
  // SETMOUSE x,y,visibility : Src1=x reg, Src2=y reg, Immediate[0-15]=visibility reg (-1 = no change)
  bcSetmouse        = bcGroupGraphics + 51;
  // GETJOYSTICK snapshot : Src1=id reg, Dest=status (0 ok / 1 no device); caches buttons + 8 axes in the VM
  bcGetJoystick     = bcGroupGraphics + 52;
  // __JOYBTN() : Dest=result — cached joystick button bitmask (int)
  bcJoyBtn          = bcGroupGraphics + 53;
  // __JOYAXIS(which) : Dest=result (FLOAT), Immediate=which (0..7) — cached joystick axis value (-1..1 / -1000)
  bcJoyAxis         = bcGroupGraphics + 54;
  // STICK(axis) : Dest=result (int 1..200 / 0), Src1=axis reg (0..3)
  bcStick           = bcGroupGraphics + 55;
  // STRIG(button) : Dest=result (int -1/0), Src1=button reg (0..7)
  bcStrig           = bcGroupGraphics + 56;
  bcGfxDrawGML      = bcGroupGraphics + 57;  // DRAW "..." : interpret the FreeBASIC graphics macro language (Src1 = string)
  bcGfxPointCoord   = bcGroupGraphics + 58;  // POINTCOORD(n): the DRAW pen coordinate (Dest=result, Src1=selector 0=x/1=y)
  // CIRCLE (x,y),r,c,start,end,aspect : ellipse / arc. Src1=x, Src2=y, Src3=RX; Immediate[0-15]=RY,
  // [16-31]=color, [32-47]=start-angle-degrees, [48-63]=end-angle-degrees (all int regs). RX/RY and the
  // angle-degree values are computed in the SSA generator, so this opcode carries only integer registers.
  bcGfxCircleEx     = bcGroupGraphics + 59;
  // PAINT (x,y),color,border : boundary flood fill. Src1=x, Src2=y; Immediate[0-15]=color, [16-31]=border (int regs).
  bcGfxPaintBorder  = bcGroupGraphics + 60;
  // Per-statement image draw target ("PSET img,(x,y)"): Src1=image handle reg; Immediate bit 0 = active (1=set,
  // 0=clear -> back to the work page). The following draw op(s) target the image; emitted before/after them.
  bcGfxSetTarget    = bcGroupGraphics + 61;
  // LINE with a style mask (dashed): Src1=x1, Src2=y1, Dest=x2; Immediate[0-15]=y2, [16-31]=color,
  // [32-47]=style (all int regs), [48-49]=shape flag (0=line, 1=box outline).
  bcGfxLineStyled   = bcGroupGraphics + 62;
  // SCREENPTR: Dest = raw pointer (RAWPTR_TAG or RAWPTR_REGION_FB, offset 0) to the working page's
  // pixel bytes. 32bpp, row pitch = width*4, exactly what SCREENINFO reports.
  bcGfxScreenPtr    = bcGroupGraphics + 63;
  bcScnClr          = bcGroupGraphics + 21;  // SCNCLR [mode]

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

  { Source map entry - maps PC range to BASIC source line }
  TSourceMapEntry = packed record
    StartPC: Integer;       // First instruction of range
    EndPC: Integer;         // Last instruction of range (inclusive)
    SourceLine: Integer;    // BASIC source line number
  end;

  { Procedure map entry (ERFN) - the PC where a SUB/FUNCTION's code begins. Not packed: it holds a
    managed string. A PC belongs to the last procedure starting at or before it. }
  TProcMapEntry = record
    StartPC: Integer;       // First instruction of the procedure
    Name: string;           // Procedure name, as written (without the PROC_ label prefix)
  end;

  { Bytecode instruction encoding - 16 bytes (was 20 bytes)
    SourceLine moved to separate TSourceMap for better cache efficiency }
  TBytecodeInstruction = packed record
    OpCode: Word;           // Opcode (TBytecodeOp) - 2 bytes for group.opcode encoding
    Dest: Word;             // Destination register index (2 bytes, 0-65535)
    Src1: Word;             // Source 1 register index (2 bytes, 0-65535)
    Src2: Word;             // Source 2 register index (2 bytes, 0-65535)
    Immediate: Int64;       // Immediate value (for constants, jump offsets, etc)
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

  { Label line entry - maps BASIC line number to PC (for REM and other non-code lines) }
  TLabelLineEntry = packed record
    LineNum: Integer;
    PC: Integer;
  end;

  { Procedure descriptor (M2) - one per SUB/FUNCTION. The bcCallSub opcode carries the
    index into this table in its Immediate; the VM reads the entry PC and the per-bank
    save sizes (how many registers [0, SaveSize) to snapshot/restore around the call,
    so the procedure body may freely reuse those registers and recursion works). Arguments
    and the result travel through the separate transfer registers (see bcXfer* opcodes),
    so the descriptor needs no parameter-slot table. SaveSize values are computed after
    register allocation/compaction by scanning each procedure's final instruction range. }
  TProcDescriptor = record
    Name: string;            // procedure name (UPPERCASE), for call resolution/diagnostics
    EntryPC: Integer;        // first instruction of the procedure body
    SaveSizeInt: Integer;    // snapshot/restore registers [0, SaveSizeInt) in the int bank
    SaveSizeFloat: Integer;  // ... float bank
    SaveSizeString: Integer; // ... string bank
  end;

  { Bytecode program - compiled bytecode ready for VM }
  TBytecodeProgram = class
  private
    FInstructions: array of TBytecodeInstruction;
    FVariables: array of TVariableInfo;
    FArrays: array of TSSAArrayInfo;
    FStringConstants: TStringList;
    FEntryPoint: Integer;
    // Source map: separate from instructions for cache efficiency
    FSourceMap: array of TSourceMapEntry;
    FSourceMapCount: Integer;
    FLastSourceLine: Integer;  // For building source map incrementally
    // Label lines: map BASIC line numbers to PC (for REM and jump targets)
    FLabelLines: array of TLabelLineEntry;
    FLabelLineCount: Integer;
    // Variable register counts - for Register Compaction to preserve variable registers
    FIntVarRegCount: Integer;
    FFloatVarRegCount: Integer;
    FStringVarRegCount: Integer;
    // Procedure descriptor table (M2): one entry per SUB/FUNCTION, indexed by bcCallSub.
    FProcedures: array of TProcDescriptor;
    // Dialect of the compiled source: True = MODERN (FreeBASIC), False = CLASSIC (Commodore v7).
    // Used at runtime for dialect-aware behaviour (e.g. filesystem error codes: FB vs CBM).
    FModernMode: Boolean;
    // Procedure map (ERFN): the PC at which each SUB/FUNCTION's code begins, in emission order. A PC
    // belongs to the last procedure that starts at or before it; a PC before the first entry is module
    // level. Procedure bodies are emitted after the module's END, so the ranges never interleave.
    // The PCs are remapped along with the source map when NOP compaction shifts instructions.
    FProcMap: array of TProcMapEntry;
    FProcMapCount: Integer;
    // Name of the source file this program was compiled from (ERMN). Set by the host.
    FModuleName: string;
  public
    property ModernMode: Boolean read FModernMode write FModernMode;
    property ModuleName: string read FModuleName write FModuleName;
    constructor Create;
    destructor Destroy; override;
    procedure AddInstruction(const Instr: TBytecodeInstruction);
    procedure AddInstructionWithLine(const Instr: TBytecodeInstruction; SourceLine: Integer);
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
    // Source map API - transparent replacement for SourceLine field
    procedure SetSourceLine(PC: Integer; Line: Integer);
    function GetSourceLine(PC: Integer): Integer;
    // Procedure map API (ERFN): record where each procedure's code starts, then ask which procedure
    // owns a PC. Returns '' for module-level code.
    procedure AddProcRange(StartPC: Integer; const Name: string);
    function GetProcNameAt(PC: Integer): string;
    // Label lines API - for REM and other non-code lines that can be jump targets
    procedure AddLabelLine(LineNum: Integer; PC: Integer);
    procedure AdjustLabelLines(const IndexMap: array of Integer);  // Adjust PCs after NOP compaction
    procedure AdjustSourceMap(const IndexMap: array of Integer);   // Adjust source map PCs after NOP compaction
    procedure ClearInstructionsOnly;  // Clear only instructions, preserve label lines and source map
    // Variable register counts for Register Compaction
    procedure SetVarRegCounts(IntCount, FloatCount, StringCount: Integer);
    function GetIntVarRegCount: Integer;
    function GetFloatVarRegCount: Integer;
    function GetStringVarRegCount: Integer;
    // Procedure descriptor table (M2)
    function AddProcedure(const Proc: TProcDescriptor): Integer;  // returns index
    procedure SetProcedure(Index: Integer; const Proc: TProcDescriptor);
    function GetProcedure(Index: Integer): TProcDescriptor;
    function GetProcedureCount: Integer;
    function FindProcedure(const Name: string): Integer;  // -1 if not found
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
  SetLength(FSourceMap, 0);
  FSourceMapCount := 0;
  FLastSourceLine := -1;
  SetLength(FLabelLines, 0);
  FLabelLineCount := 0;
  SetLength(FProcMap, 0);
  FProcMapCount := 0;
  FModuleName := '';
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

procedure TBytecodeProgram.AddInstructionWithLine(const Instr: TBytecodeInstruction; SourceLine: Integer);
var
  PC: Integer;
begin
  PC := Length(FInstructions);
  AddInstruction(Instr);
  SetSourceLine(PC, SourceLine);
end;

procedure TBytecodeProgram.SetInstruction(Index: Integer; const Instr: TBytecodeInstruction);
begin
  if (Index >= 0) and (Index < Length(FInstructions)) then
    FInstructions[Index] := Instr;
end;

procedure TBytecodeProgram.ClearInstructions;
begin
  SetLength(FInstructions, 0);
  SetLength(FSourceMap, 0);
  FSourceMapCount := 0;
  FLastSourceLine := -1;
  SetLength(FLabelLines, 0);
  FLabelLineCount := 0;
  SetLength(FProcMap, 0);
  FProcMapCount := 0;
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

procedure TBytecodeProgram.SetSourceLine(PC: Integer; Line: Integer);
begin
  // Build source map incrementally: if same line as previous, extend range
  // Otherwise, start a new range
  if Line = FLastSourceLine then
  begin
    // Extend current range
    if FSourceMapCount > 0 then
      FSourceMap[FSourceMapCount - 1].EndPC := PC;
  end
  else
  begin
    // Start new range
    if FSourceMapCount >= Length(FSourceMap) then
      SetLength(FSourceMap, FSourceMapCount + 256);  // Grow in chunks
    FSourceMap[FSourceMapCount].StartPC := PC;
    FSourceMap[FSourceMapCount].EndPC := PC;
    FSourceMap[FSourceMapCount].SourceLine := Line;
    Inc(FSourceMapCount);
    FLastSourceLine := Line;
  end;
end;

function TBytecodeProgram.GetSourceLine(PC: Integer): Integer;
var
  Lo, Hi, Mid: Integer;
begin
  // Binary search in source map
  if FSourceMapCount = 0 then
    Exit(0);

  Lo := 0;
  Hi := FSourceMapCount - 1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    if PC < FSourceMap[Mid].StartPC then
      Hi := Mid - 1
    else if PC > FSourceMap[Mid].EndPC then
      Lo := Mid + 1
    else
      Exit(FSourceMap[Mid].SourceLine);
  end;
  Result := 0;  // Not found
end;

procedure TBytecodeProgram.AddProcRange(StartPC: Integer; const Name: string);
begin
  // Called as each procedure's entry block is emitted, so StartPC is non-decreasing and GetProcNameAt
  // can scan for the last entry at or before a PC without sorting.
  if FProcMapCount >= Length(FProcMap) then
    SetLength(FProcMap, FProcMapCount + 32);
  FProcMap[FProcMapCount].StartPC := StartPC;
  FProcMap[FProcMapCount].Name := Name;
  Inc(FProcMapCount);
end;

function TBytecodeProgram.GetProcNameAt(PC: Integer): string;
var
  i: Integer;
begin
  // The owning procedure is the last one starting at or before PC. A PC before the first entry is
  // module-level code, which has no procedure name.
  Result := '';
  for i := FProcMapCount - 1 downto 0 do
    if (FProcMap[i].StartPC >= 0) and (PC >= FProcMap[i].StartPC) then
      Exit(FProcMap[i].Name);
end;

procedure TBytecodeProgram.AddLabelLine(LineNum: Integer; PC: Integer);
begin
  // Add a label line entry that maps a BASIC line number to a PC.
  // This is used for REM statements and other non-code lines that can be
  // targets of GOTO, GOSUB, TRAP, RESUME, RESTORE etc.
  if FLabelLineCount >= Length(FLabelLines) then
    SetLength(FLabelLines, FLabelLineCount + 256);
  FLabelLines[FLabelLineCount].LineNum := LineNum;
  FLabelLines[FLabelLineCount].PC := PC;
  Inc(FLabelLineCount);
end;

procedure TBytecodeProgram.AdjustLabelLines(const IndexMap: array of Integer);
var
  i: Integer;
  OldPC, NewPC: Integer;
begin
  // Adjust all label line PCs using the index map from NOP compaction
  for i := 0 to FLabelLineCount - 1 do
  begin
    OldPC := FLabelLines[i].PC;
    if (OldPC >= 0) and (OldPC < Length(IndexMap)) then
    begin
      NewPC := IndexMap[OldPC];
      // If target was a NOP, scan forward to find next valid instruction
      if NewPC = -1 then
      begin
        while (OldPC < Length(IndexMap)) and (IndexMap[OldPC] = -1) do
          Inc(OldPC);
        if OldPC < Length(IndexMap) then
          NewPC := IndexMap[OldPC]
        else
          NewPC := 0;  // Fallback to start
      end;
      FLabelLines[i].PC := NewPC;
    end;
  end;
end;

procedure TBytecodeProgram.AdjustSourceMap(const IndexMap: array of Integer);
var
  i: Integer;
  OldStartPC, OldEndPC, NewStartPC, NewEndPC: Integer;
  WriteIdx: Integer;
begin
  // Adjust all source map PCs using the index map from NOP compaction
  // This is needed for FindPCAfterLine (RESUME NEXT) and GetSourceLine to work correctly
  WriteIdx := 0;
  for i := 0 to FSourceMapCount - 1 do
  begin
    OldStartPC := FSourceMap[i].StartPC;
    OldEndPC := FSourceMap[i].EndPC;

    // Adjust StartPC
    if (OldStartPC >= 0) and (OldStartPC < Length(IndexMap)) then
    begin
      NewStartPC := IndexMap[OldStartPC];
      // If target was a NOP, scan forward to find next valid instruction
      if NewStartPC = -1 then
      begin
        while (OldStartPC < Length(IndexMap)) and (IndexMap[OldStartPC] = -1) do
          Inc(OldStartPC);
        if OldStartPC < Length(IndexMap) then
          NewStartPC := IndexMap[OldStartPC]
        else
          Continue;  // Skip this entry entirely if no valid PC found
      end;
    end
    else
      Continue;  // Skip invalid entries

    // Adjust EndPC
    if (OldEndPC >= 0) and (OldEndPC < Length(IndexMap)) then
    begin
      NewEndPC := IndexMap[OldEndPC];
      // If target was a NOP, scan backward to find previous valid instruction
      if NewEndPC = -1 then
      begin
        while (OldEndPC >= 0) and (IndexMap[OldEndPC] = -1) do
          Dec(OldEndPC);
        if OldEndPC >= 0 then
          NewEndPC := IndexMap[OldEndPC]
        else
          NewEndPC := NewStartPC;  // Fallback to start
      end;
    end
    else
      NewEndPC := NewStartPC;

    // Keep entry if it has valid PC range
    if NewStartPC <= NewEndPC then
    begin
      FSourceMap[WriteIdx].StartPC := NewStartPC;
      FSourceMap[WriteIdx].EndPC := NewEndPC;
      FSourceMap[WriteIdx].SourceLine := FSourceMap[i].SourceLine;
      Inc(WriteIdx);
    end;
  end;
  FSourceMapCount := WriteIdx;

  // The procedure map (ERFN) indexes the same instruction stream, so its entry PCs shift with it.
  // A procedure entry is a real instruction, never a NOP, but the compaction may still have turned the
  // first instruction of a procedure into one; scan forward exactly as the source map does above.
  for i := 0 to FProcMapCount - 1 do
  begin
    OldStartPC := FProcMap[i].StartPC;
    if (OldStartPC < 0) or (OldStartPC >= Length(IndexMap)) then
    begin
      FProcMap[i].StartPC := -1;   // no longer addressable: GetProcNameAt skips it
      Continue;
    end;
    NewStartPC := IndexMap[OldStartPC];
    if NewStartPC = -1 then
    begin
      while (OldStartPC < Length(IndexMap)) and (IndexMap[OldStartPC] = -1) do
        Inc(OldStartPC);
      if OldStartPC < Length(IndexMap) then
        NewStartPC := IndexMap[OldStartPC]
      else
        NewStartPC := -1;
    end;
    FProcMap[i].StartPC := NewStartPC;
  end;
end;

procedure TBytecodeProgram.ClearInstructionsOnly;
begin
  // Clear only the instruction array, preserving label lines
  // (which contain BASIC line->PC mappings needed by TRAP/RESUME)
  SetLength(FInstructions, 0);
  // Note: We do NOT clear FLabelLines or FSourceMap here
  // as they will be adjusted by the NOP compaction pass
end;

procedure TBytecodeProgram.SetVarRegCounts(IntCount, FloatCount, StringCount: Integer);
begin
  FIntVarRegCount := IntCount;
  FFloatVarRegCount := FloatCount;
  FStringVarRegCount := StringCount;
end;

function TBytecodeProgram.GetIntVarRegCount: Integer;
begin
  Result := FIntVarRegCount;
end;

function TBytecodeProgram.GetFloatVarRegCount: Integer;
begin
  Result := FFloatVarRegCount;
end;

function TBytecodeProgram.GetStringVarRegCount: Integer;
begin
  Result := FStringVarRegCount;
end;

function TBytecodeProgram.AddProcedure(const Proc: TProcDescriptor): Integer;
begin
  Result := Length(FProcedures);
  SetLength(FProcedures, Result + 1);
  FProcedures[Result] := Proc;
end;

procedure TBytecodeProgram.SetProcedure(Index: Integer; const Proc: TProcDescriptor);
begin
  if (Index >= 0) and (Index < Length(FProcedures)) then
    FProcedures[Index] := Proc;
end;

function TBytecodeProgram.GetProcedure(Index: Integer): TProcDescriptor;
begin
  Result := FProcedures[Index];
end;

function TBytecodeProgram.GetProcedureCount: Integer;
begin
  Result := Length(FProcedures);
end;

function TBytecodeProgram.FindProcedure(const Name: string): Integer;
var
  i: Integer;
  Upper: string;
begin
  Upper := UpperCase(Name);
  for i := 0 to High(FProcedures) do
    if UpperCase(FProcedures[i].Name) = Upper then
      Exit(i);
  Result := -1;
end;

function TBytecodeProgram.FindPCForLine(LineNum: Integer): Integer;
var
  i: Integer;
  BestPC, BestLine: Integer;
begin
  // First check label lines (for REM and other non-code lines)
  // This is the primary lookup for GOTO/GOSUB/TRAP targets
  for i := 0 to FLabelLineCount - 1 do
  begin
    if FLabelLines[i].LineNum = LineNum then
    begin
      Result := FLabelLines[i].PC;
      Exit;
    end;
  end;

  // Then scan source map to find the first PC with matching source line
  for i := 0 to FSourceMapCount - 1 do
  begin
    if FSourceMap[i].SourceLine = LineNum then
    begin
      Result := FSourceMap[i].StartPC;
      Exit;
    end;
  end;

  // Exact match not found in either - find entry with MINIMUM SourceLine > LineNum
  // This fallback handles lines that don't have a block (shouldn't happen normally)
  BestPC := -1;
  BestLine := MaxInt;
  for i := 0 to FSourceMapCount - 1 do
  begin
    if (FSourceMap[i].SourceLine > LineNum) and
       (FSourceMap[i].SourceLine < BestLine) then
    begin
      BestLine := FSourceMap[i].SourceLine;
      BestPC := FSourceMap[i].StartPC;
    end;
  end;
  Result := BestPC;
end;

function TBytecodeProgram.FindPCAfterLine(LineNum: Integer): Integer;
var
  i: Integer;
  BestPC, BestLine: Integer;
begin
  // Find the first PC with SourceLine > LineNum
  // This is used by RESUME NEXT to skip to the next BASIC line
  BestPC := -1;
  BestLine := MaxInt;
  for i := 0 to FSourceMapCount - 1 do
  begin
    if (FSourceMap[i].SourceLine > LineNum) and
       (FSourceMap[i].SourceLine < BestLine) then
    begin
      BestLine := FSourceMap[i].SourceLine;
      BestPC := FSourceMap[i].StartPC;
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
        56: Result := 'Key';
        57: Result := 'Nop';
        58: Result := 'Clear';
        59: Result := 'Tron';
        60: Result := 'Troff';
        61: Result := 'DataAdd';
        62: Result := 'DataReadInt';
        63: Result := 'DataReadFloat';
        64: Result := 'DataReadString';
        65: Result := 'DataRestore';
        66: Result := 'Get';
        67: Result := 'Getkey';
        68: Result := 'PrintUsing';
        69: Result := 'Pudef';
        70: Result := 'Char';
        71: Result := 'Load';
        72: Result := 'Save';
        73: Result := 'Verify';
        74: Result := 'Bload';
        75: Result := 'Bsave';
        76: Result := 'Boot';
        77: Result := 'Run';
        78: Result := 'List';
        79: Result := 'New';
        80: Result := 'Delete';
        81: Result := 'Renumber';
        82: Result := 'Catalog';
        83: Result := 'CopyFile';
        84: Result := 'Scratch';
        85: Result := 'RenameFile';
        86: Result := 'Concat';
        87: Result := 'Mkdir';
        88: Result := 'Chdir';
        89: Result := 'MoveFile';
        90: Result := 'Trap';
        91: Result := 'Resume';
        92: Result := 'ResumeNext';
        93: Result := 'ModFloat';
        94: Result := 'Frame';
        95: Result := 'CallSub';
        96: Result := 'ReturnSub';
        97: Result := 'XferStoreInt';
        98: Result := 'XferStoreFloat';
        99: Result := 'XferStoreString';
        100: Result := 'XferLoadInt';
        101: Result := 'XferLoadFloat';
        102: Result := 'XferLoadString';
        103: Result := 'RecordNew';
        104: Result := 'RecordLoadInt';
        105: Result := 'RecordLoadFloat';
        106: Result := 'RecordLoadString';
        107: Result := 'RecordStoreInt';
        108: Result := 'RecordStoreFloat';
        109: Result := 'RecordStoreString';
        110: Result := 'RecordNewArray';
        151: Result := 'RecordNewArrayInd';
        152: Result := 'RecordNewBlock';
        111: Result := 'RecordTypeId';
        133: Result := 'RecordFree';
        112: Result := 'RecMarkPush';
        113: Result := 'RecMarkPop';
        114: Result := 'LoadProcAddr';
        115: Result := 'ThreadCreate';
        116: Result := 'ThreadWait';
        117: Result := 'MutexCreate';
        118: Result := 'MutexLock';
        119: Result := 'MutexUnlock';
        120: Result := 'MutexDestroy';
        121: Result := 'CondCreate';
        122: Result := 'CondWait';
        123: Result := 'CondSignal';
        124: Result := 'CondBroadcast';
        125: Result := 'CondDestroy';
        126: Result := 'ThreadSelf';
        127: Result := 'ThreadDetach';
        128: Result := 'FloatRound';
        129: Result := 'NarrowInt';
        130: Result := 'NarrowSingle';
        131: Result := 'Shl';
        132: Result := 'Shr';
        153: Result := 'ShrUInt';
        139: Result := 'Assert';
        140: Result := 'CallSubIndirect';
        141: Result := 'SetEnviron';
        142: Result := 'Shell';
        149: Result := 'PrintUsingStage';
        150: Result := 'PrintUsingRun';
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
        19: Result := 'StrOct';
        20: Result := 'StrBin';
        21: Result := 'StrValInt';
        22: Result := 'StrString';
        23: Result := 'StrTrimSet';
        24: Result := 'StrInstrRevAny';
        48: Result := 'StrInstrAny';
        33: Result := 'StrSAdd';
        40: Result := 'FileExists';
        41: Result := 'CurDir';
        42: Result := 'Environ';
        43: Result := 'FileLen';
        44: Result := 'ExePath';
        45: Result := 'Format';
        46: Result := 'Command';
        47: Result := 'FileDateTime';
        34: Result := 'DateStr';
        35: Result := 'DateName';
        36: Result := 'StrMkInt';
        37: Result := 'StrMkFloat';
        38: Result := 'StrCvInt';
        39: Result := 'StrCvFloat';
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
        15: Result := 'MathAcos';
        16: Result := 'MathAsin';
        17: Result := 'MathAtan2';
        18: Result := 'MathFix';
        19: Result := 'MathFrac';
        20: Result := 'DateNow';
        21: Result := 'DateDecode';
        22: Result := 'DateSerial';
        23: Result := 'TimeSerial';
        24: Result := 'DateValue';
        25: Result := 'IsDate';
        26: Result := 'DateAdd';
        27: Result := 'DateDiff';
        28: Result := 'DatePart';
        29: Result := 'SetClock';
        30: Result := 'MathSinh';
        31: Result := 'MathCosh';
        32: Result := 'MathTanh';
        33: Result := 'MathAsinh';
        34: Result := 'MathAcosh';
        35: Result := 'MathAtanh';
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
        9: Result := 'ArrayLBound';
        10: Result := 'ArrayUBound';
        11: Result := 'ArrayErase';
        12: Result := 'ArrayRedim';
        13: Result := 'RefLoadInt';
        14: Result := 'RefLoadFloat';
        15: Result := 'RefLoadString';
        16: Result := 'RefStoreInt';
        17: Result := 'RefStoreFloat';
        18: Result := 'RefStoreString';
        19: Result := 'RefAddrField';
        20: Result := 'RawAlloc';
        21: Result := 'RawFree';
        22: Result := 'RawRealloc';
        23: Result := 'RawLoadInt';
        24: Result := 'RawLoadFloat';
        25: Result := 'RawStoreInt';
        26: Result := 'RawStoreFloat';
        31: Result := 'RawMemCopy';
        32: Result := 'RawMemMove';
        33: Result := 'RawClear';
        34: Result := 'ArrayBind';
        35: Result := 'ArrayUnbind';
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
        11: Result := 'PrintEnd';
        12: Result := 'Input';
        13: Result := 'InputInt';
        14: Result := 'InputFloat';
        15: Result := 'InputString';
        16: Result := 'PrintBool';
        17: Result := 'PrintUInt';
        18: Result := 'WInputChars';
        19: Result := 'InputChars';
        20: Result := 'ConScreen';
        21: Result := 'ConLocate';
        22: Result := 'ConViewPrint';
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
        8: Result := 'Append';
        9: Result := 'Dclear';
        10: Result := 'Record';
        11: Result := 'PrintFileNewLine';
        12: Result := 'PrintFileFloat';
        13: Result := 'PrintFileInt';
        25: Result := 'FileAttr';
        26: Result := 'FileSetEof';
      else
        Result := Format('FileIO_%d', [SubOp]);
      end;
    7: // Sprite operations
      case SubOp of
        0: Result := 'Sprite';
        1: Result := 'MovsprAbs';
        2: Result := 'MovsprRel';
        3: Result := 'MovsprPolar';
        4: Result := 'MovsprAuto';
        5: Result := 'Sprcolor';
        6: Result := 'Sprsav';
        7: Result := 'Collision';
        8: Result := 'Bump';
        9: Result := 'Rspcolor';
        10: Result := 'Rsppos';
        11: Result := 'Rsprite';
        12: Result := 'SpriteDef';
        13: Result := 'SprSaveFile';
        14: Result := 'SprLoadFile';
        15: Result := 'SprSize';
        16: Result := 'SprForm';
      else
        Result := Format('Sprite_%d', [SubOp]);
      end;
    8: // Web (WEB_MODE only)
      case SubOp of
        $01: Result := 'WebGetParam';
        $02: Result := 'WebPostParam';
        $03: Result := 'WebGetRaw';
        $04: Result := 'WebPostRaw';
        $05: Result := 'WebHtmlEncode';
        $06: Result := 'WebUrlEncode';
        $07: Result := 'WebMethod';
        $08: Result := 'WebPath';
        $09: Result := 'WebQuery';
        $0A: Result := 'WebHeader';
        $0B: Result := 'WebSetHeader';
        $0C: Result := 'WebStatus';
      else
        Result := Format('Web_%d', [SubOp]);
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
        8: Result := 'GraphicColor';
        9: Result := 'GraphicWidth';
        10: Result := 'GraphicScale';
        11: Result := 'GraphicPaint';
        12: Result := 'GraphicWindow';
        13: Result := 'GraphicSShape';
        14: Result := 'GraphicGShape';
        15: Result := 'GraphicGList';
        16: Result := 'GraphicPos';
        17: Result := 'GraphicRclr';
        18: Result := 'GraphicRwindow';
        19: Result := 'SetColor';
        20: Result := 'GetColor';
        21: Result := 'ScnClr';
        22: Result := 'PLoad';
        23: Result := 'PSave';
        24: Result := 'PRst';
        25: Result := 'GfxScreenRes';
        26: Result := 'GfxPset';
        27: Result := 'GfxPoint';
        28: Result := 'GfxPaint';
        29: Result := 'GfxLine';
        30: Result := 'GfxCircle';
        31: Result := 'GfxPalette';
        32: Result := 'GfxPalGet';
        33: Result := 'GfxPaletteReset';
        34: Result := 'GfxColor';
        35: Result := 'GfxForeColor';
        36: Result := 'GfxImageCreate';
        37: Result := 'GfxImageDestroy';
        38: Result := 'GfxImageInfo';
        39: Result := 'GfxGet';
        40: Result := 'GfxPut';
        41: Result := 'GfxScreenInfo';
        42: Result := 'GfxScreenSet';
        43: Result := 'GfxPCopy';
        44: Result := 'GfxWindow';
        45: Result := 'GfxPMap';
        46: Result := 'GfxView';
        47: Result := 'GfxScreen';
        48: Result := 'Multikey';
        49: Result := 'Getmouse';
        50: Result := 'MouseAxis';
        51: Result := 'Setmouse';
        52: Result := 'GetJoystick';
        53: Result := 'JoyBtn';
        54: Result := 'JoyAxis';
        55: Result := 'Stick';
        56: Result := 'Strig';
        57: Result := 'GfxDrawGML';
        58: Result := 'GfxPointCoord';
        59: Result := 'GfxCircleEx';
        60: Result := 'GfxPaintBorder';
        61: Result := 'GfxSetTarget';
        62: Result := 'GfxLineStyled';
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
        90: Result := 'AddIntToBranchLe';
        91: Result := 'AddIntToBranchLt';
        92: Result := 'SubIntToBranchGe';
        93: Result := 'SubIntToBranchGt';
        100: Result := 'MulAddFloat';
        101: Result := 'MulSubFloat';
        102: Result := 'MulAddToFloat';
        103: Result := 'MulSubToFloat';
        110: Result := 'ArrayLoadAddFloat';
        111: Result := 'ArrayLoadSubFloat';
        112: Result := 'ArrayLoadDivAddFloat';
        120: Result := 'SquareSumFloat';
        121: Result := 'AddSquareFloat';
        130: Result := 'MulMulFloat';
        131: Result := 'AddSqrtFloat';
        140: Result := 'ArrayLoadIntBranchNZ';
        141: Result := 'ArrayLoadIntBranchZ';
        156: Result := 'ArrayReverseRange';
        157: Result := 'ArrayShiftLeft';
        250: Result := 'ArraySwapInt';
        251: Result := 'AddIntSelf';
        252: Result := 'SubIntSelf';
        253: Result := 'ArrayLoadIntTo';
        254: Result := 'ArrayCopyElement';
        255: Result := 'ArrayMoveElement';
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
  // Note: SourceLine is now managed separately via TBytecodeProgram.SetSourceLine
end;

end.
