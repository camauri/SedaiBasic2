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

interface

uses
  SedaiBytecodeTypes;

{ Does this opcode use the named field as a STRING register index? }
function DestIsStringReg(OpCode: TBytecodeOp): Boolean;         // writes it
function DestReadIsStringReg(OpCode: TBytecodeOp): Boolean;     // ...and reads it back (ArrayStore)
function Src1IsStringReg(OpCode: TBytecodeOp): Boolean;
function Src2IsStringReg(OpCode: TBytecodeOp): Boolean;
function ImmediateIsStringReg(OpCode: TBytecodeOp): Boolean;

{ Does this instruction READ string register Reg in any of its fields? }
function ReadsStringReg(const Instr: TBytecodeInstruction; Reg: Integer): Boolean;

implementation

function DestIsStringReg(OpCode: TBytecodeOp): Boolean;
begin
  // Using case statement instead of set because opcodes are now Word (>255)
  case OpCode of
    // SUB/FUNCTION transfer-register load (M2): Dest is the string register written.
    bcXferLoadString,
    // UDT/record (M3): RecordLoadString writes a string field into Dest.
    bcRecordLoadString,
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
    bcSprLoadFile:    // SPRLOAD "file": Src1 = filename string
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
    bcAssert:
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
    bcStrAppendMapped, bcStrMidAssign:
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

end.
