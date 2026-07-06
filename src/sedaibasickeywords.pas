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
unit SedaiBasicKeywords;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

{.$INCLUDE sedaibasic.inc}

interface

const
  // OpCode groups by category
  OPCODE_BASE_CONTROL_FLOW   = 10000; // IF, GOTO, GOSUB, etc.
  OPCODE_BASE_LOOP           = 11000; // FOR, WHILE, DO, etc.
  OPCODE_BASE_IO             = 12000; // PRINT, INPUT, etc.
  OPCODE_BASE_VAR            = 13000; // LET, DIM, etc.
  OPCODE_BASE_FUNCTION       = 14000; // ABS, INT, SQR, etc.
  OPCODE_BASE_STRING         = 15000; // LEFT$, MID$, etc.
  OPCODE_BASE_GRAPHICS       = 16000; // CIRCLE, BOX, etc.
  OPCODE_BASE_SOUND          = 17000; // SOUND, PLAY, etc.
  OPCODE_BASE_FILE           = 18000; // OPEN, CLOSE, etc.
  OPCODE_BASE_SYSTEM         = 19000; // PEEK, POKE, etc.
  OPCODE_BASE_EXTENSION      = 20000; // For future extensions
  OPCODE_BASE_OPTIONS        = 30000;


  // === KEYWORDS ===

  // Control structures
  kBEGIN   = 'BEGIN';
  kBEND    = 'BEND';
  kFOR     = 'FOR';
  kNEXT    = 'NEXT';
  kIF      = 'IF';
  kTHEN    = 'THEN';
  kELSE    = 'ELSE';
  kELSEIF  = 'ELSEIF';   // FreeBASIC/QB block-IF else-if
  kENDIF   = 'ENDIF';    // FreeBASIC block-IF terminator (also "END IF" two-word, later)
  kSELECT  = 'SELECT';   // SELECT CASE
  kCASE    = 'CASE';     // CASE / CASE ELSE
  kSUB     = 'SUB';      // SUB procedure
  kFUNCTION = 'FUNCTION';// FUNCTION procedure
  kCALL    = 'CALL';     // CALL: statement-level SUB invocation (QB/FB)
  kTYPE    = 'TYPE';     // TYPE ... END TYPE: user-defined type (record/UDT)
  kUNION   = 'UNION';    // UNION ... END UNION: record with overlapping same-bank fields
  kCLASS   = 'CLASS';    // CLASS ... END CLASS: FreeBASIC class — modelled as a TYPE (access not enforced)
  kCOMMON  = 'COMMON';   // COMMON [SHARED] var: module-shared variable — modelled as DIM SHARED
  kAS      = 'AS';       // AS: type annotation (DIM x AS t, field AS t)
  kWITH    = 'WITH';     // WITH ... END WITH: with-block (.field => withobject.field)
  kNAMESPACE = 'NAMESPACE'; // NAMESPACE n ... END NAMESPACE: group decls under a name (FreeBASIC)
  kSCOPE   = 'SCOPE';    // SCOPE ... END SCOPE: anonymous lexical block scope (FreeBASIC)
  kEXTENDS = 'EXTENDS';  // TYPE Child EXTENDS Parent: single inheritance
  kCONSTRUCTOR = 'CONSTRUCTOR'; // CONSTRUCTOR Type(...): auto-called at instance allocation (M4.4)
  kDESTRUCTOR  = 'DESTRUCTOR';  // DESTRUCTOR Type(): auto-called at scope exit for a local (V5)
  kPROPERTY    = 'PROPERTY';    // PROPERTY Type.name [()|(value)]: getter/setter (FreeBASIC OOP)
  kOPERATOR    = 'OPERATOR';    // OPERATOR <sym>(a AS T, b AS T) AS R: operator overloading (FreeBASIC)
  kBYVAL   = 'BYVAL';    // BYVAL: pass parameter by value (copy)
  kBYREF   = 'BYREF';    // BYREF: pass parameter by reference (alias) — the default
  kBASE    = 'BASE';     // BASE(args): explicit base-constructor call inside a child CONSTRUCTOR (M4.4f)
  kSHARED  = 'SHARED';   // DIM SHARED x: module-level variable visible (read/write) inside SUB/FUNCTION (M6)
  kTHREADCREATE = 'THREADCREATE'; // h = THREADCREATE(@sub, param): spawn an OS worker thread (M5.2, FB API)
  kTHREADWAIT   = 'THREADWAIT';   // THREADWAIT h: join a worker thread by handle (M5.2, FB API)
  kTHREADCALL   = 'THREADCALL';   // h = THREADCALL sub(arg): spawn a worker (sugar for THREADCREATE) (M5.5)
  kTHREADDETACH = 'THREADDETACH'; // THREADDETACH h: detach a worker (not joinable) (M5.5)
  kTHREADSELF   = 'THREADSELF';   // h = THREADSELF(): current thread handle, 0 on main (M5.5)
  kMUTEXCREATE  = 'MUTEXCREATE';  // m = MUTEXCREATE(): create a mutex (M5.4, FB API)
  kMUTEXLOCK    = 'MUTEXLOCK';    // MUTEXLOCK m: acquire a mutex (M5.4, FB API)
  kMUTEXUNLOCK  = 'MUTEXUNLOCK';  // MUTEXUNLOCK m: release a mutex (M5.4, FB API)
  kMUTEXDESTROY = 'MUTEXDESTROY'; // MUTEXDESTROY m: free a mutex (M5.4, FB API)
  kCONDCREATE   = 'CONDCREATE';   // c = CONDCREATE(): create a condition variable (M5.4, FB API)
  kCONDWAIT     = 'CONDWAIT';     // CONDWAIT c, m: release mutex m, wait on c, reacquire m (M5.4)
  kCONDSIGNAL   = 'CONDSIGNAL';   // CONDSIGNAL c: wake one waiter (M5.4)
  kCONDBROADCAST = 'CONDBROADCAST'; // CONDBROADCAST c: wake all waiters (M5.4)
  kCONDDESTROY  = 'CONDDESTROY';  // CONDDESTROY c: free a condition variable (M5.4)
  kDO      = 'DO';
  kLOOP    = 'LOOP';
  kWHILE   = 'WHILE';
  kWEND    = 'WEND';       // FreeBASIC/QBasic: closes a WHILE...WEND loop
  kUNTIL   = 'UNTIL';
  kEXIT    = 'EXIT';
  kCONTINUE = 'CONTINUE';  // FreeBASIC: skip to the next loop iteration (FOR/DO/WHILE)
  kLSET    = 'LSET';       // FreeBASIC/QBasic: left-justify a string into a fixed-length buffer
  kRSET    = 'RSET';       // FreeBASIC/QBasic: right-justify a string into a fixed-length buffer
  kGOTO    = 'GOTO';
  kERROR   = 'ERROR';   // FreeBASIC: ON [LOCAL] ERROR GOTO (not a reserved keyword; matched by value)
  kGO_TO   = 'GO'; // GO + TO constraint
  kGOSUB   = 'GOSUB';
  kRETURN  = 'RETURN';
  kON      = 'ON';
  kSTEP    = 'STEP';
  kTO      = 'TO';
  kTRAP    = 'TRAP';
  kCONT    = 'CONT';
  kRESUME  = 'RESUME';
  kSLEEP   = 'SLEEP';
  kFRAME   = 'FRAME';
  kTROFF   = 'TROFF';
  kTRON    = 'TRON';

  // Variable operations
  kCONST   = 'CONST';
  kLET     = 'LET';
  kDIM     = 'DIM';
  kVAR     = 'VAR';      // FreeBASIC: VAR x = expr -> declare a variable with type inferred from expr
  kSTATIC  = 'STATIC';   // FreeBASIC: STATIC x AS t [= expr] -> local with persistent (static) storage
  kDEF     = 'DEF';
  kFN      = 'FN';
  // FreeBASIC DEFtype: default variable type by initial letter (no suffix / no explicit type)
  kDEFINT  = 'DEFINT';
  kDEFLNG  = 'DEFLNG';
  kDEFBYTE = 'DEFBYTE';
  kDEFSHORT = 'DEFSHORT';
  kDEFLNGINT = 'DEFLNGINT';
  kDEFLONGINT = 'DEFLONGINT';   // FreeBASIC: default LONGINT type by initial letter (int bank)
  kDEFULONGINT = 'DEFULONGINT'; // FreeBASIC: default ULONGINT type by initial letter (int bank)
  kDEFSNG  = 'DEFSNG';
  kDEFDBL  = 'DEFDBL';
  kDEFSTR  = 'DEFSTR';

  // Input/output operations
  kPRINT   = 'PRINT';
  kINPUT   = 'INPUT';
  kREAD    = 'READ';
  kDATA    = 'DATA';
  kRESTORE = 'RESTORE';
  kPRINTN  = 'PRINT#';
  kINPUTN  = 'INPUT#';
  kGETKEY  = 'GETKEY';
  kJOY     = 'JOY';
  kPEN     = 'PEN';
  kPOT     = 'POT';
  kUSING   = 'USING'; // to use with PRINT, but could be used with other commands
  kPUDEF   = 'PUDEF';
  kST      = 'ST';
  kSTASH   = 'STASH';
  kSWAP    = 'SWAP';
  kENUM    = 'ENUM';      // ENUM ... END ENUM - named integer constants (FreeBASIC)

  // time
  kTI      = 'TI';
  kTIS     = 'TI$';
  kDTS     = 'DT$';

  // filesystem
  kCWDS    = 'CWD$';

  // File handling
  kAPPEND  = 'APPEND';
  kBLOAD   = 'BLOAD';
  kBSAVE   = 'BSAVE';
  kBOOT    = 'BOOT';
  kCATALOG = 'CATALOG';
  kOPEN    = 'OPEN';
  kACCESS  = 'ACCESS';   // FreeBASIC OPEN ... ACCESS {READ|WRITE|READ WRITE}: access-rights clause (accepted/ignored)
  kCLOSE   = 'CLOSE';
  kEOF     = 'EOF';       // FreeBASIC: EOF(#n) -> -1 at end of file
  kFREEFILE = 'FREEFILE'; // FreeBASIC: next free file number (1..15, 0 if none)
  kLOF     = 'LOF';       // FreeBASIC: file length in bytes
  kLOC     = 'LOC';       // FreeBASIC: current byte position
  kSEEK    = 'SEEK';      // FreeBASIC: SEEK #n,pos statement / SEEK(n) position
  kPUT     = 'PUT';       // FreeBASIC: PUT #n[,pos],var binary write (intercepted, not reserved)
  kWRITE   = 'WRITE';     // FreeBASIC: WRITE #n,... quoted-CSV (intercepted, not reserved)
  kLINE    = 'LINE';      // FreeBASIC: LINE INPUT #n,s (intercepted, not reserved)
  kOUTPUT  = 'OUTPUT';    // FreeBASIC: OPEN ... FOR OUTPUT
  kBINARY  = 'BINARY';    // FreeBASIC: OPEN ... FOR BINARY
  kRANDOM  = 'RANDOM';    // FreeBASIC: OPEN ... FOR RANDOM
  kGET     = 'GET';
  kDLOAD   = 'DLOAD';
  kDSAVE   = 'DSAVE';
  kVERIFY  = 'VERIFY';
  kDIR     = 'DIR';
  kDIRECTORY = 'DIRECTORY';
  kDOPEN   = 'DOPEN';
  kDS      = 'DS';
  kDSS     = 'DS$';
  kDVERIFY = 'DVERIFY';
  kDELETE  = 'DELETE';
  kRENAME  = 'RENAME';
  kDCLEAR  = 'DCLEAR';
  kRESET   = 'RESET';        // FreeBASIC: unbind all file numbers (= DCLEAR)
  kDCLOSE  = 'DCLOSE';
  kCONCAT  = 'CONCAT';
  kCOPY    = 'COPY';
  kCP      = 'CP';           // Alias for COPY
  kSCRATCH = 'SCRATCH';
  kBACKUP  = 'BACKUP';
  kRECORD  = 'RECORD';
  kMKDIR   = 'MKDIR';
  kMD      = 'MD';           // Alias for MKDIR
  kCHDIR   = 'CHDIR';
  kCD      = 'CD';           // Alias for CHDIR
  kMOVE    = 'MOVE';
  kMV      = 'MV';           // Alias for MOVE
  kKILL    = 'KILL';         // FreeBASIC/QB: delete a file
  kRMDIR   = 'RMDIR';        // FreeBASIC/QB: remove an (empty) directory
  kRD      = 'RD';           // Alias for RMDIR
  kFILECOPY = 'FILECOPY';    // FreeBASIC: copy a file
  kNAME    = 'NAME';         // FreeBASIC/QB: NAME old AS new (rename); intercepted by-name, not reserved

  // FreeBASIC raw memory block ops (operate on the byte heap; intercepted by-name, not reserved)
  kCLEAR          = 'CLEAR';            // FreeBASIC: set a block of raw memory to a byte value
  kFBMEMCOPY      = 'FB_MEMCOPY';       // FreeBASIC: copy a block of raw memory (areas must not overlap)
  kFBMEMMOVE      = 'FB_MEMMOVE';       // FreeBASIC: copy a block of raw memory (overlap-safe)
  kFBMEMCOPYCLEAR = 'FB_MEMCOPYCLEAR';  // FreeBASIC: copy the first part, clear the rest

  // Memory and system
  kBANK	   = 'BANK';
  kCLR     = 'CLR';
  kPOKE    = 'POKE';
  kPEEK    = 'PEEK';
  kSYS     = 'SYS';
  kWAIT    = 'WAIT';
  kRUN     = 'RUN';
  kNEW     = 'NEW';
  kLIST    = 'LIST';
  kEDIT    = 'EDIT';     // Edit a single program line
  kAUTO    = 'AUTO';     // Auto line numbering
  kSAVE    = 'SAVE';
  kLOAD    = 'LOAD';
  kHLOAD   = 'HLOAD';    // Load history from file
  kHSAVE   = 'HSAVE';    // Save history to file
  kHCLEAR  = 'HCLEAR';   // Clear current history
  kSTOP    = 'STOP';
  kFAST    = 'FAST';
  kSLOW    = 'SLOW';
  kFETCH   = 'FETCH';
  kPOINTER = 'POINTER';
  kRREG    = 'RREG';
  kUSR     = 'USR';

  // Math and functions
  kABS     = 'ABS';
  kATN     = 'ATN';
  kATAN    = 'ATAN';      // SedaiBasic
  kCOS     = 'COS';
  kACOS    = 'ACOS';      // FreeBASIC: arccosine
  kASIN    = 'ASIN';      // FreeBASIC: arcsine
  kATAN2   = 'ATAN2';     // FreeBASIC: two-argument arctangent
  kFIX     = 'FIX';       // FreeBASIC: truncate toward zero
  kFRAC    = 'FRAC';      // FreeBASIC: fractional part
  kSINH    = 'SINH';      // hyperbolic sine
  kCOSH    = 'COSH';      // hyperbolic cosine
  kTANH    = 'TANH';      // hyperbolic tangent
  kASINH   = 'ASINH';     // inverse hyperbolic sine
  kACOSH   = 'ACOSH';     // inverse hyperbolic cosine
  kATANH   = 'ATANH';     // inverse hyperbolic tangent
  // FreeBASIC date/time (date serial = Double, epoch 1899-12-30)
  kNOW         = 'NOW';          // current date+time as a serial Double
  kTIMER       = 'TIMER';        // seconds elapsed since midnight (Double)
  kDATEFN      = 'DATE';         // current date as "mm-dd-yyyy" string
  kTIMEFN      = 'TIME';         // current time as "hh:mm:ss" string
  kDATESERIAL  = 'DATESERIAL';   // DATESERIAL(year, month, day) -> serial
  kTIMESERIAL  = 'TIMESERIAL';   // TIMESERIAL(hour, minute, second) -> serial
  kDATEVALUE   = 'DATEVALUE';    // DATEVALUE(str) -> serial (date part)
  kTIMEVALUE   = 'TIMEVALUE';    // TIMEVALUE(str) -> serial (time part)
  kYEARFN      = 'YEAR';         // YEAR(serial) -> integer
  kMONTHFN     = 'MONTH';        // MONTH(serial) -> integer 1..12
  kDAYFN       = 'DAY';          // DAY(serial) -> integer 1..31
  kHOURFN      = 'HOUR';         // HOUR(serial) -> integer 0..23
  kMINUTEFN    = 'MINUTE';       // MINUTE(serial) -> integer 0..59
  kSECONDFN    = 'SECOND';       // SECOND(serial) -> integer 0..59
  kWEEKDAY     = 'WEEKDAY';      // WEEKDAY(serial) -> integer 1=Sunday..7=Saturday
  kMONTHNAME   = 'MONTHNAME';    // MONTHNAME(n) -> month name string
  kWEEKDAYNAME = 'WEEKDAYNAME';  // WEEKDAYNAME(n) -> weekday name string
  kISDATE      = 'ISDATE';       // ISDATE(str) -> -1 if a valid date/time string, else 0
  kDATEADD     = 'DATEADD';      // DATEADD(interval$, n, serial) -> serial
  kDATEDIFF    = 'DATEDIFF';     // DATEDIFF(interval$, s1, s2) -> integer count of intervals
  kDATEPART    = 'DATEPART';     // DATEPART(interval$, serial) -> integer component
  kSETDATE     = 'SETDATE';      // SETDATE str: set the (VM-internal) current date
  kSETTIME     = 'SETTIME';      // SETTIME str: set the (VM-internal) current time
  kEXP     = 'EXP';
  kFRE     = 'FRE';
  kINT     = 'INT';
  kLOG     = 'LOG';
  kLN      = 'LN';
  kLOG10   = 'LOG10';     // SedaiBasic
  kLOG2    = 'LOG2';      // SedaiBasic
  kLOGN    = 'LOGN';      // SedaiBasic - LOGN(base, x)
  kMOD     = 'MOD';       // SedaiBasic
  kSHL     = 'SHL';       // FreeBASIC bit shift left
  kSHR     = 'SHR';       // FreeBASIC bit shift right
  kRND     = 'RND';
  kSGN     = 'SGN';
  kSIN     = 'SIN';
  kSQR     = 'SQR';
  kTAN     = 'TAN';
  kVAL     = 'VAL';
  kDEC     = 'DEC';
  // FreeBASIC type-conversion functions (B1.3)
  kCINT    = 'CINT';      // round to Integer (banker's rounding)
  kCLNG    = 'CLNG';      // round to Long
  kCLNGINT = 'CLNGINT';   // round to LongInt (64-bit)
  kCULNGINT = 'CULNGINT'; // round to ULongInt (64-bit, unsigned)
  kCSHORT  = 'CSHORT';    // round to Short
  kCBYTE   = 'CBYTE';     // round to Byte
  kCUBYTE  = 'CUBYTE';    // round to UByte
  kCUSHORT = 'CUSHORT';   // round to UShort
  kCUINT   = 'CUINT';     // round to UInteger
  kCULNG   = 'CULNG';     // round to ULong
  kCDBL    = 'CDBL';      // convert to Double
  kCSNG    = 'CSNG';      // convert to Single
  kOCT     = 'OCT';       // integer -> octal string
  kBIN     = 'BIN';       // integer -> binary string
  kWOCT    = 'WOCT';      // integer -> octal wide string (FreeBASIC)
  kWBIN    = 'WBIN';      // integer -> binary wide string (FreeBASIC)
  kWHEX    = 'WHEX';      // integer -> hex wide string (FreeBASIC)
  kWCHR    = 'WCHR';      // Unicode codepoint -> wide (UTF-8) char (FreeBASIC)
  kWSTR    = 'WSTR';      // FreeBASIC: WSTR(x) -> wide string
  kSADD    = 'SADD';      // FreeBASIC: SADD(s) -> raw pointer to a copy of the string's bytes (read-only snapshot) (intercepted as array-access)
  kSTRPTR  = 'STRPTR';    // FreeBASIC: STRPTR(s) -> raw pointer to the string's data (same model as SADD) (intercepted as array-access)
  kTRUE    = 'TRUE';      // FreeBASIC boolean constant TRUE = -1 (MODERN)
  kFALSE   = 'FALSE';     // FreeBASIC boolean constant FALSE = 0 (MODERN)
  kLOBYTE  = 'LOBYTE';    // FreeBASIC: low byte of an integer (x AND &HFF) (intercepted as array-access)
  kHIBYTE  = 'HIBYTE';    // FreeBASIC: 2nd byte of an integer ((x SHR 8) AND &HFF)
  kLOWORD  = 'LOWORD';    // FreeBASIC: low word of an integer (x AND &HFFFF)
  kHIWORD  = 'HIWORD';    // FreeBASIC: 2nd word of an integer ((x SHR 16) AND &HFFFF)
  kBIT     = 'BIT';       // FreeBASIC: bit b of x ((x SHR b) AND 1)
  kBITSET  = 'BITSET';    // FreeBASIC: x with bit b set (x OR (1 SHL b))
  kBITRESET= 'BITRESET';  // FreeBASIC: x with bit b cleared (x AND NOT (1 SHL b))
  kCBOOL   = 'CBOOL';     // FreeBASIC: convert to boolean (-1 if nonzero, else 0)
  kARRAYLEN = 'ARRAYLEN'; // FreeBASIC: total number of elements in an array (intercepted as array-access)
  kARRAYSIZE = 'ARRAYSIZE'; // FreeBASIC: total size in bytes of an array (element count * element size)
  kFILEEXISTS = 'FILEEXISTS'; // FreeBASIC: -1 if a file exists, else 0 (intercepted as array-access)
  kFILELEN = 'FILELEN';  // FreeBASIC: FILELEN(path) -> file size in bytes (intercepted as array-access)
  kFILEDATETIME = 'FILEDATETIME'; // FreeBASIC: FILEDATETIME(path) -> last-modified date serial (intercepted as array-access)
  kFILEATTR = 'FILEATTR';  // FreeBASIC: FILEATTR(filenum[,returntype]) -> open-file info (intercepted as array-access)
  kFILESETEOF = 'FILESETEOF';  // FreeBASIC: FILESETEOF filenum -> truncate/extend an open file to the current position
  kASSERT     = 'ASSERT';      // FreeBASIC: ASSERT(expr) -> if expr is false, print a message and halt
  kASSERTWARN = 'ASSERTWARN';  // FreeBASIC: ASSERTWARN(expr) -> if expr is false, print a warning and continue
  kSYSTEM     = 'SYSTEM';      // FreeBASIC: SYSTEM [exitcode] -> close all files and end the program (= END)
  kFILEFLUSH  = 'FILEFLUSH';   // FreeBASIC: FILEFLUSH [filenum] -> flush file buffers (no-op: streams are unbuffered)
  kCURDIR  = 'CURDIR';   kCURDIRS  = 'CURDIR$';   // FreeBASIC: current working directory
  kENVIRON = 'ENVIRON';  kENVIRONS = 'ENVIRON$';  // FreeBASIC: ENVIRON$(name) -> env var value
  kEXEPATH = 'EXEPATH';                            // FreeBASIC: directory of the running program
  kCOMMAND = 'COMMAND';  kCOMMANDS = 'COMMAND$';   // FreeBASIC: COMMAND$([index]) -> command-line argument(s)
  kSETENVIRON = 'SETENVIRON';                      // FreeBASIC: SETENVIRON "NAME=value" -> set an environment variable
  kSHELL   = 'SHELL';                              // FreeBASIC: SHELL cmd -> run a command (statement); SHELL(cmd) -> exit code
  kLPRINT  = 'LPRINT';                             // FreeBASIC: LPRINT ... -> print to the line printer (routed to stdout here)
  kLPOS    = 'LPOS';                               // FreeBASIC: LPOS(n) -> line-printer head column
  kISREDIRECTED = 'ISREDIRECTED';                  // FreeBASIC: ISREDIRECTED(n) -> is a standard stream redirected?
  kINP     = 'INP';                                // FreeBASIC: INP(port) -> read a hardware I/O port (no hardware here -> 0)
  kOUT     = 'OUT';                                // FreeBASIC: OUT port, value -> write a hardware I/O port (no-op here)
  kPOINTCOORD = 'POINTCOORD';                      // FreeBASIC: POINTCOORD(n) -> the DRAW pen coordinate (0=x, 1=y)
  kSCREENLIST = 'SCREENLIST';                      // FreeBASIC: SCREENLIST(depth) -> next fullscreen resolution (0 = none here)
  kSCREENCONTROL = 'SCREENCONTROL';                // FreeBASIC: SCREENCONTROL code[, value] -> get/set gfx settings (no-op here)
  kLOCK    = 'LOCK';                               // FreeBASIC: LOCK #n [, range] -> lock file records (no-op on a single-process VM)
  kUNLOCK  = 'UNLOCK';                             // FreeBASIC: UNLOCK #n [, range] -> release file record locks (no-op)
  kMACROFUNCTION   = '__FUNCTION__';    // FreeBASIC: quoted name of the current function block (intrinsic macro string)
  kMACROFUNCTIONNQ = '__FUNCTION_NQ__'; // FreeBASIC: non-quoted name of the current function block
  kFBMAINPROC      = '__FB_MAINPROC__'; // FreeBASIC: __FUNCTION__ value when used at module level (main module)
  // FreeBASIC graphics (phase 1 slice — routed through the IGraphicsBackend abstraction)
  kSCREENRES = 'SCREENRES';   // SCREENRES w, h : set the graphics screen resolution
  kPSET      = 'PSET';        // PSET (x, y) [, color] : set a pixel
  kPRESET    = 'PRESET';      // PRESET (x, y) [, color] : set a pixel (omitted colour = background)
  kPOINT     = 'POINT';       // POINT(x, y) : read a pixel's color
  kBEEP    = 'BEEP';                                // FreeBASIC/QB: console bell (emits CHR(7))
  kFORMAT  = 'FORMAT';   kFORMATS  = 'FORMAT$';    // FreeBASIC: FORMAT(num, mask) -> formatted string
  kRANDOMIZE = 'RANDOMIZE'; // FreeBASIC/QB: seed the RNG (RANDOMIZE [seed])
  kVARPTR  = 'VARPTR';    // FreeBASIC: VARPTR(v) -> address of a variable (= @v) (intercepted as array-access)
  kPROCPTR = 'PROCPTR';   // FreeBASIC: PROCPTR(p) -> address of a procedure (= @p) (intercepted as array-access)
  kWSTRING = 'WSTRING';   // FreeBASIC: WSTRING type and WSTRING(n,cp) builder function
  kWSPACE  = 'WSPACE';    // N wide spaces (FreeBASIC)
  kVALINT  = 'VALINT';    // string -> Integer
  kVALLNG  = 'VALLNG';    // string -> LongInt
  kVALULNG = 'VALULNG';   // string -> ULongInt (unsigned)
  kVALUINT = 'VALUINT';   // string -> UInteger
  // FreeBASIC numeric serialization (B3): pack/unpack a number to/from a fixed-width binary string.
  // Intercepted by name as array-access (not registered keywords), so the identifiers stay free. The MK*
  // functions accept both the bare and the '$' suffixed form. Byte widths are FB-faithful on x64.
  kMKI       = 'MKI';        kMKIS       = 'MKI$';        // Integer  -> string (platform Integer = 8 bytes on x64)
  kMKL       = 'MKL';        kMKLS       = 'MKL$';        // Long (32-bit)    -> 4-byte string
  kMKD       = 'MKD';        kMKDS       = 'MKD$';        // Double           -> 8-byte string
  kMKS       = 'MKS';        kMKSS       = 'MKS$';        // Single           -> 4-byte string
  kMKSHORT   = 'MKSHORT';    kMKSHORTS   = 'MKSHORT$';    // Short (16-bit)   -> 2-byte string
  kMKLONGINT = 'MKLONGINT';  kMKLONGINTS = 'MKLONGINT$';  // LongInt (64-bit) -> 8-byte string
  kCVI       = 'CVI';        // string -> Integer (8 bytes on x64)
  kCVL       = 'CVL';        // 4-byte string -> Long (32-bit)
  kCVD       = 'CVD';        // 8-byte string -> Double
  kCVS       = 'CVS';        // 4-byte string -> Single
  kCVSHORT   = 'CVSHORT';    // 2-byte string -> Short (16-bit)
  kCVLONGINT = 'CVLONGINT';  // 8-byte string -> LongInt (64-bit)
  // FreeBASIC array bound queries (B1.4)
  kLBOUND  = 'LBOUND';    // lower bound of an array dimension
  kUBOUND  = 'UBOUND';    // upper bound of an array dimension
  // FreeBASIC array statements (B1.4)
  kERASE   = 'ERASE';     // reset array elements to default
  kREDIM   = 'REDIM';     // re-dimension an array
  kPRESERVE = 'PRESERVE'; // REDIM PRESERVE modifier (keep existing data)

  // String functions
  kASC     = 'ASC';
  kCHAR    = 'CHAR';
  kCHRS    = 'CHR$';
  // FreeBASIC bare (no-$) forms of the string functions, routed to the $-suffixed implementations.
  kCHR     = 'CHR';
  kSTR     = 'STR';
  kLEFT    = 'LEFT';
  kRIGHT   = 'RIGHT';
  kHEXS    = 'HEX$';
  kHEX     = 'HEX';       // integer -> hex string (FreeBASIC allows the suffixless form, like OCT/BIN)
  kLEFTS   = 'LEFT$';
  kMIDS    = 'MID$';
  kRIGHTS  = 'RIGHT$';
  kSPC     = 'SPC';
  kSTRS    = 'STR$';
  kTAB     = 'TAB';
  kINSTR   = 'INSTR';
  kLEN     = 'LEN';
  // FreeBASIC string functions (B1.2). UCASE/LCASE accept both the bare and the '$' suffixed form.
  kLTRIM   = 'LTRIM';
  kRTRIM   = 'RTRIM';
  kTRIM    = 'TRIM';
  kUCASE   = 'UCASE';
  kUCASES  = 'UCASE$';
  kLCASE   = 'LCASE';
  kLCASES  = 'LCASE$';
  kINSTRREV = 'INSTRREV';
  kSPACE   = 'SPACE';
  kSPACES  = 'SPACE$';

  // Graphics
  kBOX     = 'BOX';
  kCOLOR   = 'COLOR';
  kSETCOLOR = 'SETCOLOR';  // SETCOLOR(source, color) - set color (0-255)
  kGETCOLOR = 'GETCOLOR';  // GETCOLOR(source) - return color index (0-255)
  kPLOAD   = 'PLOAD';    // PLOAD "filename" - Load palette from JSON file
  kPSAVE   = 'PSAVE';    // PSAVE "filename" - Save palette to JSON file
  kPRST    = 'PRST';     // PRST - Reset palette to C64 default
  kGLIST   = 'GLIST';    // List available SDL2 video modes
  kMORE    = 'MORE';     // Paginate output (pipe modifier)
  kGRAPHIC = 'GRAPHIC';
  kRGBA    = 'RGBA';     // Create 32-bit RGBA color value: RGBA(r, g, b, a)
  kRGB     = 'RGB';      // FreeBASIC: opaque colour value RGB(r, g, b) = RGBA(r, g, b, 255)
  kPALETTE = 'PALETTE';  // FreeBASIC: set/get/reset a palette entry
  kIMAGECREATE  = 'IMAGECREATE';   // FreeBASIC: allocate an image surface -> handle
  kIMAGEDESTROY = 'IMAGEDESTROY';  // FreeBASIC: free an image surface
  kIMAGEINFO    = 'IMAGEINFO';     // FreeBASIC: query an image surface (width/height)
  kSCREENINFO   = 'SCREENINFO';    // FreeBASIC: query the current graphics screen (width/height/depth)
  kSCREENLOCK   = 'SCREENLOCK';    // FreeBASIC: begin direct screen access (no-op on the buffered backend)
  kSCREENUNLOCK = 'SCREENUNLOCK';  // FreeBASIC: end direct screen access (no-op)
  kSCREENSYNC   = 'SCREENSYNC';    // FreeBASIC: wait for vertical retrace (no-op headless)
  kWINDOWTITLE  = 'WINDOWTITLE';   // FreeBASIC: set the graphics window caption (accept-and-ignore v1)
  kSCREENSET    = 'SCREENSET';     // FreeBASIC: select work/visible page (double buffering)
  kSCREENCOPY   = 'SCREENCOPY';    // FreeBASIC: copy one page onto another (defaults work->visible)
  kPCOPY        = 'PCOPY';         // FreeBASIC/QB: copy page src to page dst
  kFLIP         = 'FLIP';          // FreeBASIC: swap (or set) visible/work pages
  kPMAP         = 'PMAP';          // FreeBASIC: map between logical (WINDOW) and physical coordinates
  kMULTIKEY     = 'MULTIKEY';      // FreeBASIC: real-time key-down state, MULTIKEY(scancode)
  kGETMOUSE     = 'GETMOUSE';      // FreeBASIC: read mouse state, GETMOUSE(x,y[,wheel][,buttons][,clip])
  kSETMOUSE     = 'SETMOUSE';      // FreeBASIC: set mouse position/visibility, SETMOUSE [x][,y][,vis][,clip]
  kGETJOYSTICK  = 'GETJOYSTICK';   // FreeBASIC: read gaming device, GETJOYSTICK(id,buttons,a1..a8) -> status
  kSTICK        = 'STICK';         // FreeBASIC/QB: gaming-device axis position, STICK(axis) 0-3 -> 1..200/0
  kSTRIG        = 'STRIG';         // FreeBASIC/QB: gaming-device button state, STRIG(button) 0-7 -> -1/0
  kVIEW         = 'VIEW';          // FreeBASIC: set the graphics viewport (offset + clip); VIEW PRINT = text
  kSCREENGFX    = 'SCREEN';        // FreeBASIC: set a numbered graphics mode (SCREEN n)
  kDRAW    = 'DRAW';
  kCIRCLE  = 'CIRCLE';
  kPAINT   = 'PAINT';
  kLOCATE  = 'LOCATE';
  kSCALE   = 'SCALE';
  kWIDTH   = 'WIDTH';
  kSSHAPE  = 'SSHAPE';
  kGSHAPE  = 'GSHAPE';
  kRCLR    = 'RCLR';
  kRDOT    = 'RDOT';
  kRGR     = 'RGR';

  // Sprite
  kSPRITE    = 'SPRITE';     // Set sprite attributes
  kCOLLISION = 'COLLISION';  // Set collision handler
  kMOVSPR    = 'MOVSPR';     // Move/position sprite
  kSPRCOLOR  = 'SPRCOLOR';   // Set global sprite multicolors
  kSPRDEF    = 'SPRDEF';     // Define sprite (editor)
  kSPRSAV    = 'SPRSAV';     // Save/load sprite data (to/from string)
  kSPRSAVE   = 'SPRSAVE';    // Save all sprites to a JSON file
  kSPRLOAD   = 'SPRLOAD';    // Load all sprites from a JSON file
  kSPRSIZE   = 'SPRSIZE';    // Set sprite dimensions (SPRSIZE n, w, h)
  kSPRFORM   = 'SPRFORM';    // Set sprite data format (SPRFORM n, 0/1/2)
  kBUMP      = 'BUMP';       // Return collision bitmask (function)
  kRSPCOLOR  = 'RSPCOLOR';   // Return multicolor value (function)
  kRSPPOS    = 'RSPPOS';     // Return sprite position/speed (function)
  kRSPRITE   = 'RSPRITE';    // Return sprite attribute (function)

  // Sound
  kPLAY    = 'PLAY';
  kTEMPO   = 'TEMPO';
  kSOUND   = 'SOUND';
  kVOL     = 'VOL';
  kENVELOPE= 'ENVELOPE';
  kFILTER  = 'FILTER';

  // Other
  kCMD     = 'CMD';
  kREM     = 'REM'; // Comments
  kEND     = 'END';
  kHELP    = 'HELP';
  kMONITOR = 'MONITOR';
  kHEADER  = 'HEADER';
  kCOLLECT = 'COLLECT';
  kKEY	   = 'KEY';
  kEL      = 'EL';
  kER      = 'ER';
  kERR     = 'ERR';     // FreeBASIC: last error code (alias of ER)
  kERL     = 'ERL';     // FreeBASIC: last error source line (alias of EL)
  kERRS    = 'ERR$';
  kGETN    = 'GET#';
  kGO64    = 'GO64';
  kPOS     = 'POS';
  kRENUMBER = 'RENUMBER';
  kRWINDOW = 'RWINDOW';
  kSCNCLR  = 'SCNCLR';
  kCLS     = 'CLS';       // FreeBASIC/QB: clear screen (alias of SCNCLR)
  kINKEY   = 'INKEY';     // FreeBASIC: INKEY non-blocking key read (returns "" if none)
  kINKEYS  = 'INKEY$';    // QB: INKEY$ non-blocking key read (string form)
  kCSRLIN  = 'CSRLIN';    // FreeBASIC/QB: current text cursor row
  kWINDOW  = 'WINDOW';

  // Arithmetic operators
  kPLUS  = '+';
  kMINUS = '-';
  kMULT  = '*';
  kDIV   = '/';
  kPOW   = '^';

  // Comparison operators
  kEQ    = '=';
  kNEQ   = '<>';
  kLT    = '<';
  kGT    = '>';
  kLEQ   = '<=';
  kGEQ   = '>=';

  // Logical operators
  kAND   = 'AND';
  kOR    = 'OR';
  kNOT   = 'NOT';
  kXOR   = 'XOR';
  kEQV   = 'EQV';        // FreeBASIC bitwise equivalence operator: NOT (a XOR b)
  kIMP   = 'IMP';        // FreeBASIC bitwise implication operator: (NOT a) OR b
  kANDALSO = 'ANDALSO';  // FreeBASIC short-circuit logical AND
  kORELSE  = 'ORELSE';   // FreeBASIC short-circuit logical OR
  kIS      = 'IS';       // FreeBASIC RTTI operator: obj IS Type

  // SedaiBasic Options
  kOPTION = 'OPTION';
  kOPTION_SPACELESS = 'SPACELESS';
  kOPTION_STRICT = 'STRICT';
  kEXPNOTATION = 'EXPNOTATION';

  // Web keywords (WEB_MODE only)
  {$IFDEF WEB_MODE}
  kGETS     = 'GET$';      // GET$("nome") - HTML-escaped query parameter
  kPOSTS    = 'POST$';     // POST$("nome") - HTML-escaped POST parameter
  kGETRAWS  = 'GETRAW$';   // GETRAW$("nome") - raw query parameter
  kPOSTRAWS = 'POSTRAW$';  // POSTRAW$("nome") - raw POST parameter
  kHTMLS    = 'HTML$';     // HTML$(s) - escape HTML entities
  kURLS     = 'URL$';      // URL$(s) - URL encode
  kMETHODS  = 'METHOD$';   // METHOD$ - "GET" or "POST"
  kPATHS    = 'PATH$';     // PATH$ - requested path
  kQUERYS   = 'QUERY$';    // QUERY$ - full query string
  kHEADERS  = 'HEADER$';   // HEADER$("nome") - request header
  kSETHEADER = 'SETHEADER'; // SETHEADER "name", "value"
  kSTATUS   = 'STATUS';    // STATUS code - set HTTP status
  {$ENDIF}

  // Web keyword category
  {$IFDEF WEB_MODE}
  kcWebHandling = 'Web Handling';
  {$ENDIF}


  // === KEYWORD CATEGORIES ===

  kcCodeBlockConstructs    = 'Code Block Constructs';
  kcCommentsAndRemarks     = 'Comments and Remarks';
  kcConditionalFlowControl = 'Conditional Flow Control';
  kcData                   = 'Data';
  kcDebug                  = 'Debug';
  kcDelimiters             = 'Delimiters';
  kcDOSCommands            = 'DOS Commands';
  kcEnvironmentSetup       = 'Environment Setup';
  kcErrorHandling          = 'Error Handling';
  kcFileIO                 = 'File I/O';
  kcGraphicsHandling       = 'Graphics Handling';
  kcInputDevices           = 'Input Devices';
  kcIO                     = 'I/O Handling';
  kcJumpFlowControl        = 'Jump Flow Control';
  kcBitwiseOperators       = 'Bitwise Operators';
  kcLoopConstructs         = 'Loop Constructs';
  kcMachineLenguage        = 'Machine Language';
  kcMathFunctions          = 'Math Functions';
  kcMemoryHandling         = 'Memory Handling';
  kcOperators              = 'Operators';
  kcProcedures             = 'Procedures';
  kcProgramEditing         = 'Program Editing';
  kcProgramFlowControl     = 'Program Flow Control';
  kcReservedVariables      = 'Reserved Variables';
  kcShellCommands          = 'Shell Commands';
  kcSoundHandling          = 'Sound Handling';
  kcSpritesHandling        = 'Sprites Handling';
  kcStdIO                  = 'Std I/O';
  kcStringFunctions        = 'String Functions';
  kcSystemHandling         = 'System Handling';
  kcWhitespaceAndCtrlChar  = 'Whitespace and control chars';

var
  kLogicalOperatorsArray: array[1..4] of String = (kAND, kOR, kNOT, kXOR);

implementation

end.

