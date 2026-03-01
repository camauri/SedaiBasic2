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
  kDO      = 'DO';
  kLOOP    = 'LOOP';
  kWHILE   = 'WHILE';
  kUNTIL   = 'UNTIL';
  kEXIT    = 'EXIT';
  kGOTO    = 'GOTO';
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
  kDEF     = 'DEF';
  kFN      = 'FN';

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
  kCLOSE   = 'CLOSE';
  kGET     = 'GET';
  { kPUT     = 'PUT'; // PUT is not a C128 command }
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
  kEXP     = 'EXP';
  kFRE     = 'FRE';
  kINT     = 'INT';
  kLOG     = 'LOG';
  kLN      = 'LN';
  kLOG10   = 'LOG10';     // SedaiBasic
  kLOG2    = 'LOG2';      // SedaiBasic
  kLOGN    = 'LOGN';      // SedaiBasic - LOGN(base, x)
  kMOD     = 'MOD';       // SedaiBasic
  kRND     = 'RND';
  kSGN     = 'SGN';
  kSIN     = 'SIN';
  kSQR     = 'SQR';
  kTAN     = 'TAN';
  kVAL     = 'VAL';
  kDEC     = 'DEC';

  // String functions
  kASC     = 'ASC';
  kCHAR    = 'CHAR';
  kCHRS    = 'CHR$';
  kHEXS    = 'HEX$';
  kLEFTS   = 'LEFT$';
  kMIDS    = 'MID$';
  kRIGHTS  = 'RIGHT$';
  kSPC     = 'SPC';
  kSTRS    = 'STR$';
  kTAB     = 'TAB';
  kINSTR   = 'INSTR';
  kLEN     = 'LEN';

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
  kSPRSAV    = 'SPRSAV';     // Save/load sprite data
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
  kERRS    = 'ERR$';
  kGETN    = 'GET#';
  kGO64    = 'GO64';
  kPOS     = 'POS';
  kRENUMBER = 'RENUMBER';
  kRWINDOW = 'RWINDOW';
  kSCNCLR  = 'SCNCLR';
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

