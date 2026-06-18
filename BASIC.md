# SedaiBasic - BASIC Commands

## Implementation Progress

**188 / 209 commands implemented (90%)**

```
[█████████████████████████████████████████████····] 90%
```

Legend: ✓ = Implemented | ✗ = Not implemented

## Operators (12/12 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `+` | ✓ | Add operator |
| `-` | ✓ | Subtract operator |
| `*` | ✓ | Multiply operator |
| `/` | ✓ | Divide operator |
| `^` | ✓ | Power operator |
| `MOD` | ✓ | Modulo operator |
| `=` | ✓ | Equal operator |
| `<` | ✓ | Lesser than operator |
| `>` | ✓ | Greater than operator |
| `<=` | ✓ | Lesser than or equal operator |
| `>=` | ✓ | Greater than or equal operator |
| `<>` | ✓ | Not equal operator |

## Logical Operators (4/4 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `AND` | ✓ | AND operator |
| `NOT` | ✓ | NOT operator |
| `OR` | ✓ | OR operator |
| `XOR` | ✓ | XOR operator |

## Flow Control - Conditionals (3/3 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `IF` | ✓ | IF statement |
| `THEN` | ✓ | THEN statement |
| `ELSE` | ✓ | ELSE statement |

## Flow Control - Jumps (6/6 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `EXIT` | ✓ | Exit statement |
| `GOSUB` | ✓ | Gosub statement |
| `GOTO` | ✓ | Goto statement |
| `GO TO` | ✓ | Go to statement |
| `ON` | ✓ | Conditional jump |
| `RETURN` | ✓ | Return from jump |

## Flow Control - Program Execution (8/9 - 89%)

| Command | Status | Description |
|---------|--------|-------------|
| `CONT` | ✓ | Continue program execution after STOP |
| `END` | ✓ | Ends program execution |
| `FAST` | ✓ | Set fast speed clock (shows black overlay) |
| `FRAME` | ✓ | Wait for frame sync (FRAME for 60fps, FRAME n for n fps) |
| `RUN` | ✓ | Execute program (RUN, RUN "filename") |
| `SLEEP` | ✓ | Delay program for n seconds (0 < n < 65536, interruptible with CTRL+C) |
| `SLOW` | ✓ | Set slow speed clock (hides black overlay) |
| `STOP` | ✓ | Halt program execution (can resume with CONT) |
| `WAIT` | ✗ | Pause until condition satisfied |

## Flow Control - Loops (8/8 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `DO` | ✓ | Starts DO/LOOP cycle |
| `FOR` | ✓ | Starts FOR loop |
| `LOOP` | ✓ | Closes DO/LOOP cycle |
| `NEXT` | ✓ | Closes FOR loop updating counter |
| `STEP` | ✓ | Sets FOR loop increment/decrement per iteration |
| `TO` | ✓ | Sets FOR loop end value |
| `UNTIL` | ✓ | DO/LOOP until condition |
| `WHILE` | ✓ | DO/LOOP while condition |

## Code Blocks (2/2 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `BEGIN` | ✓ | Starts code block |
| `BEND` | ✓ | Ends code block |

## Procedures (5/5 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `DEF` | ✓ | Define function (DEF FNname(var) = expression) |
| `FN` | ✓ | Function call (FNname(value)) |
| `SUB` | ✓ | Structured SUB procedure (FreeBASIC/QB): params (ByVal), locals, recursion. `END SUB` |
| `FUNCTION` | ✓ | Structured FUNCTION: params, return via `fname=expr` or `RETURN expr`, recursion. `END FUNCTION` |
| `CALL` | ✓ | Invoke a SUB: `CALL name(args)` (or `CALL name args`); `EXIT SUB`/`EXIT FUNCTION` for early return |

## Data Management (7/7 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `CLR` | ✓ | Clear all variables |
| `CONST` | ✓ | Constant assignment |
| `DATA` | ✓ | Data statement (stores literal values for READ) |
| `DIM` | ✓ | Dimension arrays |
| `LET` | ✓ | Variable assignment |
| `READ` | ✓ | Read data (reads values from DATA into variables) |
| `RESTORE` | ✓ | Restore data pointer (resets READ position) |

## Standard Input/Output (7/7 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `GET` | ✓ | Get character (non-blocking, returns empty string if no key) |
| `GETKEY` | ✓ | Get keypress (blocking, waits for key) |
| `INPUT` | ✓ | Input statement |
| `CHAR` | ✓ | Displays text at specific position (mode, col, row, text [,reverse]) |
| `PRINT` | ✓ | Print statement |
| `PUDEF` | ✓ | Redefine PRINT USING symbols (filler, comma, decimal, dollar) |
| `USING` | ✓ | Formatted output (PRINT USING "#$######.##";value) |

## File Input/Output (3/3 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `GET#` | ✓ | Get character from file |
| `INPUT#` | ✓ | Input from file |
| `PRINT#` | ✓ | Print to file |

## I/O Control (1/1 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `CMD` | ✓ | Redirect screen output to file |

## DOS Commands (26/29 - 90%)

| Command | Status | Description |
|---------|--------|-------------|
| `APPEND` | ✓ | Append data to sequential file |
| `BACKUP` | ✗ | Copy disk content to another disk |
| `BLOAD` | ✓ | Load bytecode file (.basc) |
| `BOOT` | ✓ | Load and execute bytecode file (BLOAD + RUN) |
| `BSAVE` | ✓ | Save bytecode file (.basc) |
| `CATALOG` | ✓ | Display drive directory |
| `CLOSE` | ✓ | Close file (alias for DCLOSE) |
| `COLLECT` | ✗ | Free inaccessible disk space |
| `CHDIR` | ✓ | Change current directory (alias: CD) |
| `CONCAT` | ✓ | Concatenate files - append source to destination |
| `COPY` | ✓ | Copy file(s) with wildcard support (alias: CP) |
| `DCLEAR` | ✓ | Clear all open channels on disk drive |
| `DCLOSE` | ✓ | Close disk file |
| `DIR` | ✓ | Display drive directory (alias for DIRECTORY) |
| `DIRECTORY` | ✓ | Display drive directory |
| `DLOAD` | ✓ | Load BASIC file |
| `DOPEN` | ✓ | Open disk file for read/write |
| `DSAVE` | ✓ | Save BASIC file |
| `DVERIFY` | ✓ | Verify saved BASIC file |
| `HEADER` | ✗ | Formats a diskette |
| `LOAD` | ✓ | Load program |
| `OPEN` | ✓ | Open file for input/output (alias for DOPEN) |
| `RECORD` | ✓ | Position relative file pointer |
| `MKDIR` | ✓ | Create directory (alias: MD) |
| `MOVE` | ✓ | Move file (alias: MV) |
| `RENAME` | ✓ | Rename file (RENAME oldname newname) |
| `SAVE` | ✓ | Save program |
| `SCRATCH` | ✓ | Delete file(s) with wildcard support |
| `VERIFY` | ✓ | Verify saved file or program |

## String Functions (12/12 - 100%)

| Function | Status | Description |
|----------|--------|-------------|
| `ASC` | ✓ | Return character code |
| `CHR$` | ✓ | Return character from code |
| `DEC` | ✓ | Convert hex number string to decimal |
| `HEX$` | ✓ | Hex number string from decimal number (4-char, 0000-FFFF) |
| `INSTR` | ✓ | Position of source string in destination string (1-based, optional start) |
| `LEN` | ✓ | Return string length |
| `LEFT$` | ✓ | Return string leftmost chars |
| `MID$` | ✓ | Return substring from larger string |
| `RIGHT$` | ✓ | Return string rightmost chars |
| `SPC` | ✓ | Skip spaces on context output |
| `STR$` | ✓ | Convert number to string |
| `TAB` | ✓ | Move cursor forward string from the first column |

## Memory Management (3/9 - 33%)

| Command/Function | Status | Description |
|------------------|--------|-------------|
| `BANK` | ✗ | Select RAM bank (0-15) |
| `FETCH` | ✗ | Get data from expansion RAM |
| `POKE` | ✓ | Set content of memory-mapped location |
| `RREG` | ✗ | Read contents of accumulator and registers |
| `STASH` | ✗ | Move content of host RAM to expansion RAM |
| `SWAP` | ✗ | Swap content of host RAM to expansion RAM |
| `FRE` | ✓ | Return RAM bytes free (FRE(0)) |
| `PEEK` | ✓ | Return content of memory-mapped location |
| `POINTER` | ✗ | Return the address of a variable name |

## Graphics Management (24/24 - 100%)

| Command/Function | Status | Description |
|------------------|--------|-------------|
| `BOX` | ✓ | Draw a box |
| `CIRCLE` | ✓ | Draws circles, ellipses, arcs and polygons |
| `COLOR` | ✓ | Define colors for each screen area (0-255, palette wraps every 16) |
| `SETCOLOR` | ✓ | Modify palette entry with RGBA values |
| `GETCOLOR` | ✓ | Read palette entry as RGBA value |
| `PLOAD` | ✓ | Load palette from JSON file |
| `PSAVE` | ✓ | Save palette to JSON file |
| `PRST` | ✓ | Reset palette to C64 default colors |
| `DRAW` | ✓ | Draw dots, lines and shapes |
| `GLIST` | ✓ | List available SDL2 video modes |
| `GRAPHIC` | ✓ | Select a graphic mode |
| `GSHAPE` | ✓ | Retrieve shape from string variable |
| `LOCATE` | ✓ | Position the bit map pixel cursor on the screen |
| `PAINT` | ✓ | Fill area with color |
| `SCALE` | ✓ | Alter scaling in graphics mode |
| `SCNCLR` | ✓ | Clear screen |
| `SSHAPE` | ✓ | Save shapes to string variable |
| `WIDTH` | ✓ | Set the width of drawn lines |
| `WINDOW` | ✓ | Defines a screen window |
| `POS` | ✓ | Return the current cursor column position |
| `RCLR` | ✓ | Return color of color source (0-255) |
| `RDOT` | ✓ | Return current position or color of pixel cursor |
| `RGR` | ✓ | Return current graphic mode |
| `RWINDOW` | ✓ | Return the size of the current window |

## Sprite Management (14/14 - 100%)

| Command/Function | Status | Description |
|------------------|--------|-------------|
| `COLLISION` | ✓ | Define handling for sprite collision interrupt |
| `SPRITE` | ✓ | Set sprite properties |
| `MOVSPR` | ✓ | Position or move sprite on the screen |
| `SPRCOLOR` | ✓ | Set multicolor 1 and/or multicolor 2 colors for all sprites |
| `SPRDEF` | ✓ | Enter the SPRite DEFinition mode (interactive sprite editor, sbv only) |
| `SPRSAV` | ✓ | Store a sprite data from a text string or vice versa |
| `SPRSAVE` | ✓ | Save all sprite definitions to a JSON file (SedaiBasic extension) |
| `SPRLOAD` | ✓ | Load all sprite definitions from a file: `SPRLOAD "file"[,usefilecolors]` (1 = use the file's colours; default 0 = keep current colours) (SedaiBasic extension) |
| `SPRSIZE` | ✓ | Set sprite dimensions: `SPRSIZE n, width, height` (1..256 each; default 24×21 C128). SNES/console-style presets supported (SedaiBasic extension) |
| `SPRFORM` | ✓ | Set sprite data format: `SPRFORM n, format` (0 = hi-res, 1 = multicolor, 2 = full-color 256-palette/8bpp) (SedaiBasic extension) |
| `BUMP` | ✓ | Return sprite collision information |
| `RSPCOLOR` | ✓ | Return sprite multicolor values |
| `RSPPOS` | ✓ | Return the speed and position values of a sprite |
| `RSPRITE` | ✓ | Return sprite characteristics |

## Audio Management (6/6 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `ENVELOPE` | ✓ | Define a musical instrument envelope (ENVELOPE n, attack, decay, sustain, release, waveform [,pulsewidth]) |
| `FILTER` | ✓ | Define sound filter parameters (FILTER cutoff, lowpass, bandpass, highpass, resonance) |
| `PLAY` | ✓ | Define and play musical notes (PLAY "Vn On Tn Un Xn notes") - V=voice, O=octave, T=envelope, U=volume, X=filter |
| `SOUND` | ✓ | Outputs sound effects (SOUND voice, freq, duration [,dir, minfreq, sweeptime, waveform, pulsewidth]) |
| `TEMPO` | ✓ | Define the speed of the song being played (TEMPO n, 1-255) |
| `VOL` | ✓ | Define output level of sound (VOL n, 0-15) |

## Math Functions (17/17 - 100%)

| Function | Status | Description |
|----------|--------|-------------|
| `ABS` | ✓ | Return absolute value |
| `ATN` | ✓ | Return arctangent of argument |
| `ATAN` | ✓ | Return arctangent of argument |
| `COS` | ✓ | Return cosine of angle of x radians |
| `EXP` | ✓ | Return value of e raised to the power x |
| `INT` | ✓ | Convert float number to integer |
| `LN` | ✓ | Return natural log of x |
| `LOG` | ✓ | Return natural log of x |
| `LOG10` | ✓ | Return base 10 log of x |
| `LOG2` | ✓ | Return base 2 log of x |
| `LOGN` | ✓ | Return base n log of x: LOGN(base, x) |
| `RND` | ✓ | Return a random number from 0 (included) to 1 (excluded) |
| `SGN` | ✓ | Return sign of argument |
| `SIN` | ✓ | Return sine of argument |
| `SQR` | ✓ | Return square root of argument |
| `TAN` | ✓ | Return tangent of argument |
| `VAL` | ✓ | Return the numeric value of a number string |

## Reserved Variables (6/9 - 67%)

| Variable | Status | Description |
|----------|--------|-------------|
| `DS` | ✗ | Get disk status code |
| `DS$` | ✗ | Get disk status message |
| `CWD$` | ✓ | Get current working directory (read-only) |
| `DT$` | ✓ | Get current date (YYYYMMDD format, read-only) |
| `EL` | ✓ | Return last error line |
| `ER` | ✓ | Return last error code |
| `ST` | ✗ | Get I/O status byte |
| `TI` | ✓ | Get time elapsed from power on (jiffies, 1/60 sec) |
| `TI$` | ✓ | Get/set 24h clock (HHMMSS format) |

## Error Handling (4/4 - 100%)

| Command/Function | Status | Description |
|------------------|--------|-------------|
| `RESUME` | ✓ | Resume execution at error line (use in TRAP handler) |
| `RESUME NEXT` | ✓ | Resume execution at next statement after error |
| `TRAP` | ✓ | Set error handler line (TRAP 0 disables) |
| `ERR$(n)` | ✓ | Return error message for error code n |

## Debug (2/3 - 67%)

| Command | Status | Description |
|---------|--------|-------------|
| `HELP` | ✗ | Highlight the line where the error occurred |
| `TRON` | ✓ | Activate debug mode (trace, breakpoints, stepping) |
| `TROFF` | ✓ | Deactivate debug mode |

## Machine Language (0/3 - 0%)

| Command/Function | Status | Description |
|------------------|--------|-------------|
| `MONITOR` | ✗ | Enter ML monitor |
| `SYS` | ✗ | Execute ML subroutine |
| `USR` | ✗ | Call user-defined ML subfunction |

## Program Editing (9/9 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `AUTO` | ✓ | Auto line numbering (AUTO inc to enable, AUTO to disable) |
| `DELETE` | ✓ | Delete lines of a BASIC program (DELETE n, DELETE n-m, DELETE -n, DELETE n-) |
| `EDIT` | ✓ | Edit a single program line (EDIT n) |
| `HCLEAR` | ✓ | Clear command history (prompts for confirmation) |
| `HLOAD` | ✓ | Load command history from file (HLOAD "filename") |
| `HSAVE` | ✓ | Save command history to file (HSAVE "filename") |
| `LIST` | ✓ | List the BASIC program lines (LIST, LIST n, LIST n-, LIST -n, LIST n-m) |
| `NEW` | ✓ | Erase program and clear all variables |
| `RENUMBER` | ✓ | Renumber lines of the BASIC program (RENUMBER [new[,inc[,old]]]) |

## Comments (1/1 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `REM` | ✓ | Starts a comment or remark |

## Special Input Devices (0/3 - 0%)

| Function | Status | Description |
|----------|--------|-------------|
| `JOY` | ✗ | Return joystick status |
| `PEN` | ✗ | Return light pen status |
| `POT` | ✗ | Return paddle status |

## System Management (1/1 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `KEY` | ✓ | Define/list function key assignment (KEY n,"text" or KEY) |

## Environment Directives (0/1 - 0%)

| Command | Status | Description |
|---------|--------|-------------|
| `EXPNOTATION` | ✗ | Setup Directives |

---

## Command Syntax Reference

### DOPEN / OPEN - Open Disk File

Opens a disk file for reading or writing.

**Syntax:**
```basic
DOPEN #handle, "filename" [, mode$]
OPEN #handle, "filename" [, mode$]
```

**Parameters:**
- `handle` - File handle number (1-255) or identifier (#1, #MYFILE)
- `filename` - Path to the file to open (string expression)
- `mode$` - Optional access mode string (default: "R")

**Access Modes:**
| Mode | Description |
|------|-------------|
| `"R"` | Read only (default) |
| `"W"` | Write only (creates/truncates file) |
| `"RW"` | Read and write |
| `"A"` | Append mode |

**Sharing Modes (optional, after comma):**
| Mode | Description |
|------|-------------|
| `"R,EXCLUSIVE"` | Exclusive access, no sharing |
| `"R,DENYWRITE"` | Others can read but not write |
| `"R,DENYREAD"` | Others can write but not read |
| `"R,DENYNONE"` | Full sharing allowed (default) |

**Examples:**
```basic
10 DOPEN #1, "data.txt"
20 DOPEN #1, "data.txt", "R"
30 DOPEN #2, "output.txt", "W"
40 DOPEN #3, "logfile.txt", "A"
50 DOPEN #MYFILE, "C:\data\report.txt", "RW"
60 OPEN #1, "config.ini", "R,EXCLUSIVE"
```

**Note:** OPEN is an alias for DOPEN. Both commands have identical behavior.

### DCLOSE / CLOSE - Close Disk File

Closes an open disk file.

**Syntax:**
```basic
DCLOSE #handle
CLOSE #handle
```

**Parameters:**
- `handle` - File handle number or identifier to close

**Examples:**
```basic
10 DOPEN #1, "data.txt", "R"
20 REM ... read from file ...
30 DCLOSE #1

40 DOPEN #LOGFILE, "app.log", "A"
50 REM ... write to log ...
60 DCLOSE #LOGFILE
```

**Note:** CLOSE is an alias for DCLOSE. Both commands have identical behavior.

### APPEND - Append Data to File

Appends string data to an open file.

**Syntax:**
```basic
APPEND #handle, expression
```

**Parameters:**
- `handle` - File handle number (1-15) previously opened with DOPEN/OPEN
- `expression` - String expression to append to the file

**Examples:**
```basic
10 DOPEN #1, "log.txt", "A"
20 APPEND #1, "New log entry"
30 APPEND #1, CHR$(13) + CHR$(10)
40 DCLOSE #1
```

**Notes:**
- APPEND is functionally similar to PRINT# but provides a clearer semantic for appending data
- The file should be opened in append mode ("A") or write mode ("W")

### DCLEAR - Close All File Handles

Closes all open file handles at once.

**Syntax:**
```basic
DCLEAR
```

**Examples:**
```basic
10 DOPEN #1, "file1.txt", "R"
20 DOPEN #2, "file2.txt", "W"
30 REM ... work with files ...
40 DCLEAR
50 REM All files are now closed
```

**Notes:**
- Useful for cleanup or error recovery
- Equivalent to calling DCLOSE for each open handle

### RECORD - Seek File Position

Positions the file pointer to a specific byte offset within an open file.

**Syntax:**
```basic
RECORD #handle, position
```

**Parameters:**
- `handle` - File handle number (1-15) previously opened with DOPEN/OPEN
- `position` - Byte offset from the beginning of the file (0-based)

**Examples:**
```basic
10 DOPEN #1, "data.bin", "RW"
20 RECORD #1, 100
30 INPUT# 1, A$
40 PRINT "Data at position 100: "; A$
50 RECORD #1, 0
60 REM Back to beginning
70 DCLOSE #1
```

**Notes:**
- Position 0 is the beginning of the file
- The file must be opened in a mode that supports seeking (typically "R", "RW")
- Use LOF() function to get the file length before seeking

### GET# - Get Character from File

Reads a single character from an open file.

**Syntax:**
```basic
GET# handle, variable$
```

**Parameters:**
- `handle` - File handle number (1-255) previously opened with DOPEN/OPEN
- `variable$` - String variable to receive the character

**Examples:**
```basic
10 DOPEN #1, "data.txt", "R"
20 GET# 1, A$
30 PRINT "Read character: "; A$
40 DCLOSE #1
```

**Notes:**
- GET# reads exactly one character from the file
- At end of file, returns empty string
- File must be opened for reading ("R" or "RW" mode)

### INPUT# - Input from File

Reads data from an open file into one or more variables.

**Syntax:**
```basic
INPUT# handle, variable [, variable ...]
```

**Parameters:**
- `handle` - File handle number (1-255) previously opened with DOPEN/OPEN
- `variable` - One or more variables to receive the data

**Examples:**
```basic
10 DOPEN #1, "data.txt", "R"
20 INPUT# 1, NAME$, AGE, CITY$
30 PRINT NAME$; " is "; AGE; " years old from "; CITY$
40 DCLOSE #1

REM Read numbers from file
50 DOPEN #2, "numbers.txt", "R"
60 FOR I = 1 TO 10
70   INPUT# 2, N
80   PRINT N
90 NEXT I
100 DCLOSE #2
```

**Notes:**
- Reads data separated by commas or newlines
- String values should be comma-separated or on separate lines
- At end of file, string variables receive empty string, numeric variables receive 0

### PRINT# - Print to File

Writes data to an open file.

**Syntax:**
```basic
PRINT# handle [, expression [; expression ...]]
```

**Parameters:**
- `handle` - File handle number (1-255) previously opened with DOPEN/OPEN
- `expression` - One or more values to write (strings, numbers, variables)

**Separators:**
- `;` (semicolon) - No separator between values
- `,` (comma) - Tab separator between values

**Examples:**
```basic
10 DOPEN #1, "output.txt", "W"
20 PRINT# 1, "Hello, World!"
30 PRINT# 1, "Name: "; NAME$
40 PRINT# 1, A; ","; B; ","; C
50 DCLOSE #1

REM Append to existing file
60 DOPEN #2, "log.txt", "A"
70 PRINT# 2, TIME$; " - "; MESSAGE$
80 DCLOSE #2
```

**Notes:**
- File must be opened for writing ("W", "A", or "RW" mode)
- PRINT# without expressions (just handle) can be used to reset CMD redirection
- A newline is added at the end unless the line ends with `;` or `,`

### CMD - Redirect Output to File

Redirects screen output (PRINT statements) to an open file.

**Syntax:**
```basic
CMD handle [, expression]
```

**Parameters:**
- `handle` - File handle number (1-255) previously opened with DOPEN/OPEN
- `expression` - Optional value to print after redirection starts

**Examples:**
```basic
10 DOPEN #1, "output.txt", "W"
20 CMD 1
30 PRINT "This goes to the file"
40 PRINT "So does this"
50 PRINT# 1  : REM Reset CMD, output goes to screen again
60 PRINT "This goes to screen"
70 DCLOSE #1
```

**Notes:**
- After CMD, all PRINT output goes to the specified file
- CMD affects PRINT and LIST but not direct screen operations
- Use PRINT# with just the handle (no data) to cancel CMD redirection
- File must be opened for writing ("W", "A", or "RW" mode)

---

## Sprite Commands Reference

### SPRITE - Set Sprite Attributes

Defines or modifies sprite properties.

**Syntax:**
```basic
SPRITE n, enabled, color [, priority [, scaleX [, scaleY [, mode]]]]
```

**Parameters:**
| Parameter | Range | Description |
|-----------|-------|-------------|
| `n` | 0-255 | Sprite number |
| `enabled` | 0-1 | 0=disable, 1=enable |
| `color` | 0-255 | Sprite color index (or RGBA for truecolor modes) |
| `priority` | 0-3 | Display priority (0=behind all, 3=front of all) |
| `scaleX` | 0.1-10.0 | Horizontal scale factor (1=normal) |
| `scaleY` | 0.1-10.0 | Vertical scale factor (1=normal) |
| `mode` | 0-1 | 0=standard, 1=multicolor |

**Examples:**
```basic
10 SPRITE 0, 1, 5              : REM Enable sprite 0, color 5
20 SPRITE 1, 1, 2, 3           : REM Sprite 1, color 2, highest priority
30 SPRITE 2, 1, 7, 2, 2, 2     : REM Sprite 2, double size
40 SPRITE 3, 0, 0              : REM Disable sprite 3
```

### MOVSPR - Move/Position Sprite

Positions a sprite or sets up automatic movement.

**Syntax:**
```basic
MOVSPR n, x, y                 : REM Absolute position
MOVSPR n, +x, +y               : REM Relative movement
MOVSPR n, #angle, speed        : REM Polar coordinates (one-time)
MOVSPR n, ;angle, speed        : REM Automatic continuous movement
```

**Parameters:**
| Parameter | Description |
|-----------|-------------|
| `n` | Sprite number (0-255) |
| `x`, `y` | Screen coordinates or relative offset |
| `angle` | Movement angle in degrees (0-360) |
| `speed` | Movement speed (pixels per frame) |

**Movement Modes:**
| Prefix | Mode | Description |
|--------|------|-------------|
| (none) | Absolute | Set exact screen position |
| `+`/`-` | Relative | Move relative to current position |
| `#` | Polar | One-time movement at angle/speed |
| `;` | Automatic | Continuous movement (interrupt-driven) |

**Examples:**
```basic
10 MOVSPR 0, 160, 100          : REM Position sprite 0 at center
20 MOVSPR 0, +10, +5           : REM Move 10 right, 5 down
30 MOVSPR 1, #45, 2            : REM Move at 45 degrees, speed 2
40 MOVSPR 2, ;90, 1            : REM Auto-move rightward continuously
50 MOVSPR 2, ;0, 0             : REM Stop automatic movement
```

### SPRCOLOR - Set Sprite Multicolors

Sets the global multicolor values used by all sprites in multicolor mode.

**Syntax:**
```basic
SPRCOLOR mc1, mc2
```

**Parameters:**
| Parameter | Range | Description |
|-----------|-------|-------------|
| `mc1` | 0-255 | Multicolor 1 (shared by all sprites) |
| `mc2` | 0-255 | Multicolor 2 (shared by all sprites) |

**Examples:**
```basic
10 SPRCOLOR 1, 2               : REM Set multicolors to white and red
20 SPRITE 0, 1, 5, 0, 1, 1, 1  : REM Enable multicolor mode for sprite 0
```

### SPRSAV - Save/Load Sprite Data

Transfers sprite definition data between memory and string variables.

**Syntax:**
```basic
SPRSAV source, destination
```

**Parameters:**
| Type | Description |
|------|-------------|
| Sprite to String | `SPRSAV n, A$` - Save sprite n data to string variable |
| String to Sprite | `SPRSAV A$, n` - Load string data to sprite n |

**Examples:**
```basic
10 SPRSAV 0, PLAYER$           : REM Save sprite 0 to PLAYER$
20 SPRSAV ENEMY$, 1            : REM Load ENEMY$ data to sprite 1
30 SPRSAV 0, 1                 : REM Copy sprite 0 to sprite 1
```

### COLLISION - Set Collision Handler

Defines a BASIC subroutine to call when sprite collision occurs.

**Syntax:**
```basic
COLLISION type, linenum
```

**Parameters:**
| Parameter | Description |
|-----------|-------------|
| `type` | Collision type: 1=sprite-sprite, 2=sprite-background |
| `linenum` | Line number of handler subroutine (0=disable) |

**Examples:**
```basic
10 COLLISION 1, 1000           : REM Jump to 1000 on sprite collision
20 COLLISION 2, 2000           : REM Jump to 2000 on background collision
30 COLLISION 1, 0              : REM Disable sprite collision handler
...
1000 REM Sprite collision handler
1010 C = BUMP(1)               : REM Get collision bitmask
1020 PRINT "Collision!"; C
1030 RETURN
```

---

## Sprite Functions Reference

### BUMP - Get Collision Information

Returns a bitmask indicating which sprites have collided.

**Syntax:**
```basic
BUMP(type)
```

**Parameters:**
| Parameter | Description |
|-----------|-------------|
| `type` | 1=sprite-sprite collision, 2=sprite-background collision |

**Returns:** Integer bitmask where bit N is set if sprite N collided.

**Examples:**
```basic
10 C = BUMP(1)                 : REM Get sprite-sprite collisions
20 IF C AND 1 THEN PRINT "Sprite 0 collided"
30 IF C AND 2 THEN PRINT "Sprite 1 collided"
40 IF C AND 4 THEN PRINT "Sprite 2 collided"
```

### RSPCOLOR - Get Sprite Multicolor

Returns the current multicolor value.

**Syntax:**
```basic
RSPCOLOR(n)
```

**Parameters:**
| Parameter | Description |
|-----------|-------------|
| `n` | 1=multicolor 1, 2=multicolor 2 |

**Returns:** Color index (0-255).

**Examples:**
```basic
10 MC1 = RSPCOLOR(1)           : REM Get multicolor 1
20 MC2 = RSPCOLOR(2)           : REM Get multicolor 2
30 PRINT "Multicolors:"; MC1; MC2
```

### RSPPOS - Get Sprite Position/Speed

Returns position or speed information for a sprite.

**Syntax:**
```basic
RSPPOS(n, axis)
```

**Parameters:**
| Parameter | Description |
|-----------|-------------|
| `n` | Sprite number (0-255) |
| `axis` | 0=X position, 1=Y position, 2=speed |

**Returns:** Coordinate or speed value.

**Examples:**
```basic
10 X = RSPPOS(0, 0)            : REM Get sprite 0 X position
20 Y = RSPPOS(0, 1)            : REM Get sprite 0 Y position
30 S = RSPPOS(0, 2)            : REM Get sprite 0 speed
40 PRINT "Position:"; X; Y; "Speed:"; S
```

### RSPRITE - Get Sprite Attribute

Returns a specific attribute of a sprite.

**Syntax:**
```basic
RSPRITE(n, attr)
```

**Parameters:**
| Parameter | Description |
|-----------|-------------|
| `n` | Sprite number (0-255) |
| `attr` | Attribute code (see table) |

**Attribute Codes:**
| Code | Returns |
|------|---------|
| 0 | Enabled status (0/1) |
| 1 | Color index |
| 2 | Priority (0-3) |
| 3 | Scale X |
| 4 | Scale Y |
| 5 | Mode (0=standard, 1=multicolor) |

**Examples:**
```basic
10 IF RSPRITE(0, 0) = 1 THEN PRINT "Sprite 0 is enabled"
20 C = RSPRITE(1, 1)           : REM Get sprite 1 color
30 P = RSPRITE(2, 2)           : REM Get sprite 2 priority
```

---

## Sprite System Notes

### Configuration
- Maximum sprites: 256 (configurable via `MAX_SPRITES` constant)
- Default sprite size: 24x21 pixels (C128 compatible)
- Custom sprite dimensions supported for modern resolutions

### Color Modes
- **Indexed mode** (0-255): For "historic" resolutions, uses palette lookup
- **Truecolor mode** (RGBA): For modern resolutions, full 32-bit color

### Automatic Movement
When using `MOVSPR n, ;angle, speed`:
- Movement is interrupt-driven (automatic)
- Sprite moves continuously until stopped
- Use `MOVSPR n, ;0, 0` to stop automatic movement
- Collision handlers continue to work during movement

### Priority Levels
| Priority | Description |
|----------|-------------|
| 0 | Behind all other sprites and graphics |
| 1 | Behind graphics, above priority 0 |
| 2 | Above graphics, below priority 3 |
| 3 | In front of all other sprites and graphics |

---

## Debug Commands Reference

SedaiBasic provides a modern debugger that extends the classic TRON/TROFF commands with breakpoints and step-by-step execution.

### TRON - Activate Debug Mode

Activates the debugger with trace output and enables breakpoints/stepping.

**Syntax:**
```basic
TRON
```

**Effects:**
- Enables trace output showing `[line]` for each executed line
- Enables breakpoint support (set with BREAK command)
- Enables step-by-step execution (with STEP command)
- Programs run with `RunDebug` instead of `RunFast`

**Example:**
```basic
TRON
RUN
[10][20][30][40]...
```

### TROFF - Deactivate Debug Mode

Deactivates the debugger and returns to fast execution mode.

**Syntax:**
```basic
TROFF
```

**Effects:**
- Disables trace output
- Clears all breakpoints
- Disables stepping
- Programs run with `RunFast` for maximum performance

### BREAK - Set Breakpoint

Sets a breakpoint at a specific line number.

**Syntax:**
```basic
BREAK linenum
```

**Parameters:**
- `linenum` - The line number where execution should pause

**Example:**
```basic
TRON
BREAK 100
BREAK 200
RUN
```

When a breakpoint is hit, execution pauses and displays:
```
[BREAK] Line 100
READY.
```

### UNBREAK - Clear Breakpoint

Removes a breakpoint from a specific line.

**Syntax:**
```basic
UNBREAK linenum
```

**Parameters:**
- `linenum` - The line number to clear the breakpoint from

### STEP - Step Execution

Executes a single line and pauses. Used when program is paused at a breakpoint.

**Syntax:**
```basic
STEP
```

**Example:**
```basic
TRON
BREAK 10
RUN
[BREAK] Line 10
READY.
STEP
[10]
[BREAK] Line 20 (stepping)
READY.
STEP
...
```

### Debug Notes

1. **Performance**: Debug mode (`RunDebug`) is slower than normal execution (`RunFast`). Use TROFF when not debugging.

2. **Shell Commands**: TRON, TROFF, BREAK, UNBREAK, and STEP are shell commands, not program statements. They cannot be used inside a BASIC program.

3. **Trace Output**: When trace is active, each executed line displays `[linenum]` before execution.

4. **Resuming**: After a breakpoint, use STEP to execute line-by-line, or CONT to continue until the next breakpoint.

---

## Web BASIC Instructions (sbw.exe only)

> **Note:** These instructions are **only available** in the Web Server version (`sbw.exe`).
> They are **not recognized** in the console (`sb.exe`) or Vision (`sbv.exe`) versions.
> Conversely, graphics, audio, and sprite instructions are **not available** in `sbw.exe`.

See [WEB_BASIC.md](WEB_BASIC.md) for complete documentation.

### Input Functions

| Command | Status | Description |
|---------|--------|-------------|
| `GET$("name")` | Planned | Return HTML-escaped query parameter (safe) |
| `POST$("name")` | Planned | Return HTML-escaped POST parameter (safe) |
| `GETRAW$("name")` | Planned | Return raw query parameter (unsafe) |
| `POSTRAW$("name")` | Planned | Return raw POST parameter (unsafe) |

### Encoding Functions

| Command | Status | Description |
|---------|--------|-------------|
| `HTML$(s)` | Planned | Escape HTML entities |
| `URL$(s)` | Planned | URL encode string |

### HTTP Environment

| Command | Status | Description |
|---------|--------|-------------|
| `METHOD$` | Planned | Return HTTP method ("GET" or "POST") |
| `PATH$` | Planned | Return requested path |
| `QUERY$` | Planned | Return full query string |
| `HEADER$("name")` | Planned | Return HTTP request header |

### Response Control

| Command | Status | Description |
|---------|--------|-------------|
| `SETHEADER name, value` | Planned | Set HTTP response header |
| `STATUS code` | Planned | Set HTTP response status code |

---

## Appendix A: PETSCII Control Characters

SedaiBasic supports a subset of PETSCII control characters for compatibility with Commodore 64/128 programs. These are activated when printing via `PRINT CHR$(code)`.

> **Note:** SedaiBasic operates in shell mode (like Bash/PowerShell), not full-screen mode like the C128. Cursor movement codes are ignored.

### Screen Control

| CHR$ | Code | Action |
|------|------|--------|
| 147 | $93 | Clear screen (equivalent to `SCNCLR`) |
| 13 | $0D | Carriage return / newline |

### Reverse Video

| CHR$ | Code | Action |
|------|------|--------|
| 18 | $12 | Reverse ON - subsequent text printed with fg/bg swapped |
| 146 | $92 | Reverse OFF - return to normal text |

**Example:**
```basic
PRINT CHR$(18);"HIGHLIGHTED";CHR$(146);" normal"
```

### Foreground Color Codes

| CHR$ | Code | Color | Palette Index |
|------|------|-------|---------------|
| 144 | $90 | Black | 0 |
| 5 | $05 | White | 1 |
| 28 | $1C | Red | 2 |
| 159 | $9F | Cyan | 3 |
| 156 | $9C | Purple | 4 |
| 30 | $1E | Green | 5 |
| 31 | $1F | Blue | 6 |
| 158 | $9E | Yellow | 7 |
| 129 | $81 | Orange | 8 |
| 149 | $95 | Brown | 9 |
| 150 | $96 | Light Red | 10 |
| 151 | $97 | Dark Gray | 11 |
| 152 | $98 | Medium Gray | 12 |
| 153 | $99 | Light Green | 13 |
| 154 | $9A | Light Blue | 14 |
| 155 | $9B | Light Gray | 15 |

**Example:**
```basic
PRINT CHR$(28);"Red text";CHR$(5);" White text"
```

### Ignored Codes (Shell Mode)

The following PETSCII codes are silently ignored because they require full-screen cursor control, which is not available in shell mode:

| CHR$ | Code | Original Function |
|------|------|-------------------|
| 17 | $11 | Cursor down |
| 145 | $91 | Cursor up |
| 29 | $1D | Cursor right |
| 157 | $9D | Cursor left |
| 19 | $13 | Home (cursor to top-left) |
| 148 | $94 | Insert mode toggle |
| 20 | $14 | Delete character |

### Compatibility Notes

- Use `SCNCLR` instead of `PRINT CHR$(147)` for clearer code
- Use `COLOR` command for more control over foreground/background colors
- Reverse mode affects all text until explicitly turned off
- Color changes persist until changed again or screen is cleared

---

# FreeBASIC Keyword Reference & Implementation Status

> **Scope.** SedaiBasic is evolving toward a FreeBASIC-style language (see
> `job/docs/SEDAIBASIC_EVOLUTION.md`). This section catalogues the **complete FreeBASIC keyword
> set**, organized exactly as in the official FreeBASIC manual
> ([DocToc](https://www.freebasic.net/wiki/DocToc)), together with SedaiBasic's current support.
> Sourced from the FreeBASIC wiki (Language Documentation + Runtime Library Reference), June 2026.
>
> **Legend.** ✓ = the keyword name is a **recognized SedaiBasic command** (the Commodore BASIC v7
> core plus the M1/M2/M3 structured subset: block `IF`/`ELSEIF`/`END IF`, `SELECT CASE`, `FOR`/`NEXT`,
> `DO`/`LOOP`, named labels, `SUB`/`FUNCTION`/`CALL`/`EXIT`/`RETURN`, `TYPE`/`AS`/`.` records).
> ◐ = partially implemented (see note). ✗ = not implemented.
> Note: a ✓ marks name recognition — exact semantics may still differ from FreeBASIC (e.g. FB's OOP
> `TYPE`, pointers, threading, and preprocessor are not present). This is a forward-looking gap map,
> not a claim of FreeBASIC compatibility.
>
> **Coverage (FreeBASIC-keyword overlap):** Language ~**95/363**, Runtime Library **72/280**,
> total ~**167/643** (M3 adds `AS`, member-access `.`, and `TYPE` ◐).

## Language Documentation

### Variables and Data Types

#### Variable Declarations

| Keyword | Status | Description |
|---|---|---|
| `DIM` | ✓ | Declares a variable at the current scope. |
| `CONST` | ✓ | Declares a non-modifiable variable. |
| `SCOPE` | ✗ | Begins a new scope block. |
| `STATIC` | ✗ | Declares variables in a procedure that retain their value between calls. |
| `SHARED` | ✗ | Used with Dim allows variables to be visible throughout a module. |
| `VAR` | ✗ | Declares variables where the data type is implied from an initializer. |
| `BYREF (variables)` | ✗ | Used with Dim or Static or Var allows to declare references. |

#### User Defined Types

##### Declarations

| Keyword | Status | Description |
|---|---|---|
| `ENUM...END ENUM` | ✗ | User defined enumeration of values |
| `TYPE...END TYPE` | ◐ | User defined structure (M3): scalar fields (int/float/string), `DIM v AS T`, `v.field`. Member procedures, nested UDT, arrays-of-UDT deferred |
| `CLASS...END CLASS` | ✗ | Not implemented. Keyword reserved. |
| `UNION...END UNION` | ✗ | User defined structure of overlapping data |
| `EXTENDS` | ✗ | Extends an user defined type to derive another |
| `EXTENDS WSTRING` | ✗ | Extends an user defined type to inherits Wstring behavior |
| `EXTENDS ZSTRING` | ✗ | Extends an user defined type to inherits Zstring behavior |
| `IMPLEMENTS` | ✗ | Not implemented. Keyword reserved. |
| `FIELD` | ✗ | Specifies field alignment within a user defined type |
| `OBJECT` | ✗ | Built-in type providing run-time type information |

##### Referencing

| Keyword | Status | Description |
|---|---|---|
| `Temporary Types` | ✗ | Creates a temporary copy of a user defined type |
| `THIS` | ✗ | Built-in, hidden, parameter passed to non-static member procedures to access the user defined type instance |
| `BASE (member access)` | ✗ | Built-in, hidden, variable to access the base user defined type instance in derived user defined types |
| `Type Alias` | ✗ | Declares a user defined type from other user defined or standard data types |
| `WITH` | ✗ | Compound statement to access the data and members of a user defined type |

##### Member Procedures

| Keyword | Status | Description |
|---|---|---|
| `BASE (initialization)` | ✗ | Specifies an initializer for the base user defined type in derived user defined type constructors |
| `CONSTRUCTOR` | ✗ | Declares or defines a member procedure that is automatically called when a user defined type is created |
| `DESTRUCTOR` | ✗ | Declares or defines a member procedure that is automatically called when a user defined type is destroyed or goes out of scope |
| `FUNCTION` | ✓ | Declares or defines a member procedure returning a value |
| `OPERATOR` | ✗ | Declares or defines an overloaded operator |
| `OVERRIDE` | ✗ | Member method attribute that specifies that the method is expected to override a virtual method in the base user defined type |
| `PROPERTY` | ✗ | Declares or defines property member procedures for a user defined type |
| `SUB` | ✓ | Declare or defines a member procedure |
| `STATIC (Member)` | ✗ | Declares or defines a member procedure or variable is static |
| `VIRTUAL` | ✗ | Member method attribute that declares that a member must have an implementation |
| `ABSTRACT` | ✗ | Member method attribute that declares that a member must be implemented in a derived user defined type |
| `CONST (Member)` | ✓ | Member method attribute that declares or defines that the method is readonly and does not modify the user defined types's data |

##### Member Access Control

| Keyword | Status | Description |
|---|---|---|
| `PUBLIC: (Access Control)` | ✗ | Data and members in a user defined type have public visibility |
| `PRIVATE: (Access Control)` | ✗ | Data and members in a user defined type have private visibility |
| `PROTECTED: (Access Control)` | ✗ | Data and members in a user defined type have protected visibility |

#### Standard Data Types

##### Integer types

| Keyword | Status | Description |
|---|---|---|
| `BYTE and UBYTE` | ✗ | 8-bit wide data types that store integer values. |
| `SHORT and USHORT` | ✗ | 16-bit wide data types that store integer values. |
| `LONG and ULONG` | ✗ | 32-bit wide data types that store integer values. |
| `INTEGER and UINTEGER` | ✗ | 32-bit or 64-bit wide data types that store integer values. |
| `LONGINT and ULONGINT` | ✗ | 64-bit wide data types that store integer values. |

##### Floating-point types

| Keyword | Status | Description |
|---|---|---|
| `SINGLE` | ✗ | 32-bit wide data types that store real number values. |
| `DOUBLE` | ✗ | 64-bit wide data types that store real number values. |

##### Boolean types

| Keyword | Status | Description |
|---|---|---|
| `BOOLEAN` | ✗ | 1-bit wide data types that store boolean values. |

##### Procedure Types

| Keyword | Status | Description |
|---|---|---|
| `FUNCTION Pointer` | ✓ | Types that store a pointer to a function procedure |
| `SUB Pointer` | ✓ | Types that store a pointer to a sub procedure |

##### Data Type Modifiers

| Keyword | Status | Description |
|---|---|---|
| `CONST` | ✓ | Specifies a read only type. |
| `POINTER and PTR (Shortcut for 'POINTER')` | ✓ | Modifies types to be pointer types. |
| `UNSIGNED` | ✗ | Specifies an unsigned integer type. |
| `ALIAS (Modifier)` | ✗ | Modifies how a datatype is linked with other languages (Name mangling). |

##### String types

| Keyword | Status | Description |
|---|---|---|
| `STRING` | ✗ | Fixed-length and variable-length strings with built-in memory management. |
| `ZSTRING` | ✗ | Fixed-length and variable-length null-terminated strings. |
| `WSTRING` | ✗ | Fixed-length and variable-length null-terminated strings of wide characters. |

##### Class types

| Keyword | Status | Description |
|---|---|---|
| `OBJECT` | ✗ | Super class providing run-time type information |

#### Converting Between Data Types

##### Generic conversions

| Keyword | Status | Description |
|---|---|---|
| `CAST and CPTR` | ✗ | Converts expressions between different types. |

##### Conversions to integral types

| Keyword | Status | Description |
|---|---|---|
| `CBYTE and CUBYTE` | ✗ | Converts numeric expressions to 8-bit values. |
| `CSHORT and CUSHORT` | ✗ | Converts numeric expressions to 16-bit values. |
| `CLNG and CULNG` | ✗ | Converts numeric expressions to 32-bit values. |
| `CINT and CUINT` | ✗ | Converts numeric expressions to 32-bit or 64-bit values. |
| `CLNGINT and CULNGINT` | ✗ | Converts numeric expressions to 64-bit values. |
| `CSIGN` | ✗ | Converts a numeric expression to a signed-type value. |
| `CUNSG` | ✗ | Converts a numeric expression to an unsigned-type value. |

##### Conversions to floating-point types

| Keyword | Status | Description |
|---|---|---|
| `CSNG and CDBL` | ✗ | Converts a numeric or string expression to floating-point values. |

##### Conversions to/from string types

| Keyword | Status | Description |
|---|---|---|
| `STR and WSTR` | ✗ | Converts numeric expressions or booleans to their string representation. |
| `VAL` | ✓ | Converts a numeric string expression to a floating-point value. |
| `VALINT and VALUINT` | ✗ | Converts numeric string expressions to integer values. |
| `VALLNG and VALULNG` | ✗ | Converts numeric string expressions to long values. |

##### Conversion to boolean types

| Keyword | Status | Description |
|---|---|---|
| `CBOOL` | ✗ | Converts a numeric or string expression to a boolean value. |

### Operators

#### Assignment Operators

| Keyword | Status | Description |
|---|---|---|
| `= (Assignment)` | ✓ |  |
| `&= (Concatenate and Assign)` | ✗ |  |
| `+= (Add and Assign)` | ✗ |  |
| `-= (Subtract and Assign)` | ✗ |  |
| `*= (Multiply and Assign)` | ✗ |  |
| `/= (Divide and Assign)` | ✗ |  |
| `\= (Integer Divide and Assign)` | ✗ |  |
| `^= (Exponentiate and Assign)` | ✗ |  |
| `MOD= (Modulus and Assign)` | ✗ |  |
| `AND= (Conjunction and Assign)` | ✗ |  |
| `EQV= (Equivalence and Assign)` | ✗ |  |
| `IMP= (Implication and Assign)` | ✗ |  |
| `OR= (Inclusive Disjunction and Assign)` | ✗ |  |
| `XOR= (Exclusive Disjunction and Assign)` | ✗ |  |
| `SHL= (Shift Left and Assign)` | ✗ |  |
| `SHR= (Shift Right and Assign)` | ✗ |  |
| `LET (Assign)` | ✓ |  |
| `LET() (Assignment)` | ✓ |  |

#### Type Cast Operators

| Keyword | Status | Description |
|---|---|---|
| `CAST (operator)` | ✗ |  |
| `CPTR` | ✗ |  |

#### Arithmetic Operators

| Keyword | Status | Description |
|---|---|---|
| `+ (Add)` | ✓ |  |
| `- (Subtract)` | ✓ |  |
| `* (Multiply)` | ✓ |  |
| `/ (Divide)` | ✓ |  |
| `\ (Integer divide)` | ✗ |  |
| `^ (Exponentiate)` | ✓ |  |
| `MOD (Modulus)` | ✓ |  |
| `- (Negate)` | ✓ |  |
| `SHL (Shift left)` | ✗ |  |
| `SHR (Shift right)` | ✗ |  |

#### Indexing Operators

| Keyword | Status | Description |
|---|---|---|
| `() (Array index)` | ✗ |  |
| `[] (String index)` | ✗ |  |
| `[] (Pointer index)` | ✗ |  |

#### String Operators

| Keyword | Status | Description |
|---|---|---|
| `+ (String concatenation)` | ✓ |  |
| `& (String concatenation with conversion)` | ✗ |  |
| `STRPTR (String pointer)` | ✗ |  |

#### Relational Operators

| Keyword | Status | Description |
|---|---|---|
| `= (Equal)` | ✓ |  |
| `<> (Not equal)` | ✓ |  |
| `< (Less than)` | ✓ |  |
| `<= (Less than or equal)` | ✓ |  |
| `>= (Greater than or equal)` | ✓ |  |
| `> (Greater than)` | ✓ |  |

#### Bitwise Operators

| Keyword | Status | Description |
|---|---|---|
| `AND (Conjunction)` | ✓ |  |
| `EQV (Equivalence)` | ✗ |  |
| `IMP (Implication)` | ✗ |  |
| `NOT (Complement)` | ✓ |  |
| `OR (Inclusive Disjunction)` | ✓ |  |
| `XOR (Exclusive Disjunction)` | ✓ |  |

#### Short Circuit Operators

| Keyword | Status | Description |
|---|---|---|
| `ANDALSO (Short Circuit Conjunction)` | ✗ |  |
| `ORELSE (Short Circuit Inclusive Disjunction)` | ✗ |  |

#### Preprocessor Operators

| Keyword | Status | Description |
|---|---|---|
| `# (Argument stringize)` | ✗ |  |
| `## (Argument concatenation)` | ✗ |  |
| `! (Escaped String Literal)` | ✗ |  |
| `$ (Non-Escaped String Literal)` | ✗ |  |

#### Pointer Operators

| Keyword | Status | Description |
|---|---|---|
| `@ (Address of)` | ✗ |  |
| `* (Value of)` | ✓ |  |
| `VARPTR (Variable pointer)` | ✗ |  |
| `PROCPTR (Procedure pointer and vtable index)` | ✗ |  |

#### Type or Class Operators

| Keyword | Status | Description |
|---|---|---|
| `. (Member access)` | ✓ | Record field access `rec.field` (M3) |
| `-> (Pointer to member access)` | ✗ |  |
| `IS (Run-time type information operator)` | ✗ |  |

#### Memory Operators

| Keyword | Status | Description |
|---|---|---|
| `New Expression` | ✓ |  |
| `New Overload` | ✓ |  |
| `Placement New` | ✗ |  |
| `Delete Statement` | ✓ |  |
| `Delete Overload` | ✓ |  |

#### Iteration Operators

| Keyword | Status | Description |
|---|---|---|
| `For` | ✓ |  |
| `Next` | ✓ |  |
| `Step` | ✓ |  |

### Statements

#### Control Flow

##### Transferring Statements

| Keyword | Status | Description |
|---|---|---|
| `GOTO` | ✓ | Transfers execution to another point in code defined by a text label. |
| `GOSUB` | ✓ | Temporarily transfers execution to another point in code, defined by a text label. |
| `ON GOTO` | ✓ | Transfers execution to one of a number of points in code defined by text labels, based on the value of an expression. |
| `ON GOSUB` | ✓ | Temporarily transfers execution to one of a number of points in code defined by text labels, based on the value of an expression. |
| `RETURN (from procedure)` | ✓ | Returns from a procedure returning a value. |
| `RETURN (from Gosub)` | ✓ | Returns from a call using Gosub. |
| `EXIT SUB, EXIT FUNCTION, EXIT OPERATOR,` | ✓ |  |
| `EXIT CONSTRUCTOR, EXIT DESTRUCTOR and EXIT PROPERTY` | ✓ | Prematurely leaves a procedure code block. |

##### Branching Statements

| Keyword | Status | Description |
|---|---|---|
| `IF..END IF` | ✓ | Executes a block of statements if a condition is met. |
| `..ELSE IF..` | ✓ | Executes a block of code if a condition is met and all previous conditions weren't met. |
| `..ELSE..` | ✓ | Executes a block of code if all previous conditions weren't met. |
| `SELECT..END SELECT` | ✓ | Executes one of a number of statement blocks using a set of conditions. |
| `..CASE..` | ✓ | Executes a block of code if a condition is met. |
| `..CASE ELSE..` | ✓ | Executes a block of code if all previous conditions weren't met. |
| `EXIT SELECT` | ✓ | Prematurely breaks out of a SELECT..END SELECT statement. |

##### Looping Statements

| Keyword | Status | Description |
|---|---|---|
| `WHILE..WEND (or 'WHILE...END WHILE')` | ✓ | Executes a block of statements while a condition is met. |
| `FOR..NEXT` | ✓ | Executes a block of statements while an iterator is less than or greater than an expression. |
| `DO..LOOP` | ✓ | Executes a block of statements while or until a condition is met. |
| `CONTINUE WHILE, CONTINUE FOR and CONTINUE DO` | ✗ | Prematurely re-enters a loop. |
| `EXIT WHILE, EXIT FOR and EXIT DO` | ✓ | Prematurely breaks out of a loop. |

#### Procedures

##### Declaration

| Keyword | Status | Description |
|---|---|---|
| `Declare` | ✗ | Declares a module-level or member procedure. |
| `Sub` | ✓ | Specifies a procedure that does not return an argument. |
| `Function` | ✓ | Specifies a procedure that returns an argument. |
| `Overload` | ✗ | Specifies that the procedure name can be used in other procedure declarations. |
| `Static` | ✗ | Specifies static storage for all variables and objects in the procedure body. |
| `Const (Member)` | ✓ | Specifies a const member procedure in user-defined type definitions. |
| `Static (Member)` | ✗ | Specifies a static member procedure in user-defined type definitions. |

##### Linkage

| Keyword | Status | Description |
|---|---|---|
| `Public` | ✗ | Specifies external linkage for a procedure. |
| `Private` | ✗ | Specifies internal linkage for a procedure. |
| `Alias` | ✗ | Specifies an alternate external name for a procedure. |
| `Export` | ✗ | Specifies a procedure is to be exported from a shared library. |
| `Lib` | ✗ | Specifies automatic loading of a library. |

##### Calling conventions

| Keyword | Status | Description |
|---|---|---|
| `Stdcall` | ✗ | Specifies the standard calling convention for BASIC languages, including FreeBASIC. |
| `Cdecl` | ✗ | Specifies the standard calling convention in the C and C++ languages. |
| `Pascal` | ✗ | Specifies the standard calling convention in the Fortran, Pascal and Microsoft QuickBASIC/QBasic languages. |
| `Fastcall` | ✗ | Specifies the fastcall calling convention for 32-bit procedures. |
| `Thiscall` | ✗ | Specifies the thiscall calling convention for 32-bit member procedures. |

##### Parameter passing conventions

| Keyword | Status | Description |
|---|---|---|
| `Byref` | ✗ | Specifies passing an argument by reference. |
| `Byval` | ✗ | Specifies passing an argument by value. |
| `Any` | ✗ | Disables type-checking on arguments. |

##### Variadic Procedures

| Keyword | Status | Description |
|---|---|---|
| `... (Ellipsis)` | ✗ | Indicates a variadic procedure in a declaration. |
| `VA_FIRST` | ✗ | Macro to obtain the argument list in a variadic procedure. |
| `VA_ARG` | ✗ | Macro to obtain the current argument in a variadic procedure. |
| `VA_NEXT` | ✗ | Macro to move to the next argument in a variadic procedure. |

##### Automatic execution

| Keyword | Status | Description |
|---|---|---|
| `Constructor (Module)` | ✗ | Indicates a procedure is to be executed before module-level code. |
| `Destructor (Module)` | ✗ | Indicates a procedure is to be executed after module-level code. |

##### Miscellaneous

| Keyword | Status | Description |
|---|---|---|
| `Byref (function results)` | ✗ | Specifies that a function returns by reference rather than by value. |
| `Call` | ✓ | Invokes a procedure. |
| `Naked` | ✗ | Specifies that a function body is not to be given any prolog/epilog code |

#### Modularizing

| Keyword | Status | Description |
|---|---|---|
| `COMMON` | ✗ |  |
| `DYLIBFREE` | ✗ |  |
| `DYLIBLOAD` | ✗ |  |
| `DYLIBSYMBOL` | ✗ |  |
| `EXPORT` | ✗ |  |
| `EXTERN` | ✗ |  |
| `EXTERN...END EXTERN` | ✗ |  |
| `IMPORT` | ✗ |  |
| `NAMESPACE` | ✗ |  |
| `PRIVATE` | ✗ |  |
| `PUBLIC` | ✗ |  |
| `USING (Namespaces)` | ✓ |  |

### Other

#### Preprocessor

##### Conditional Compilation

| Keyword | Status | Description |
|---|---|---|
| `#IF` | ✗ | Compiles the following code block based on a condition. |
| `#IFDEF` | ✗ | Compiles the following code block if a symbol is defined. |
| `#IFNDEF` | ✗ | Compiles the following code block if a symbol is not defined. |
| `#ELSEIF` | ✗ | Compiles the following code block if a condition is true and the previous conditions was false. |
| `#ELSEIFDEF` | ✗ | Compiles the following code block if a symbol is defined and the previous conditions was false. |
| `#ELSEIFNDEF` | ✗ | Compiles the following code block if a symbol is not defined and the previous conditions was false. |
| `#ELSE` | ✗ | Compiles the following code block if previous conditions were false. |
| `#ENDIF` | ✗ | Signifies the end of a code block. |
| `DEFINED` | ✗ | Returns "-1" if a symbol is defined, otherwise "0". |

##### Text Replacement

| Keyword | Status | Description |
|---|---|---|
| `#DEFINE` | ✗ | Creates a single-line text-replacement macro. |
| `#MACRO and #ENDMACRO` | ✗ | Creates a multi-line text-replacement macro. |
| `#UNDEF` | ✗ | Undefines a symbol. |
| `# Preprocessor stringize` | ✗ | Converts text into a string literal. |
| `## Preprocessor concatenate` | ✗ | Concatenates two pieces of text. |
| `! Escaped String Literal` | ✗ | Indicates string literal immediately following must be processed for escape sequences. |
| `$ Non-Escaped String Literal` | ✗ | Indicates string literal immediately following must not be processed for escape sequences. |

##### File Directives

| Keyword | Status | Description |
|---|---|---|
| `#INCLUDE` | ✗ | Inserts text from a file. |
| `#INCLIB` | ✗ | Includes a library in the linking processes. |
| `#LIBPATH` | ✗ | Includes a path to search for libraries in the linking process. |

##### Control Directives

| Keyword | Status | Description |
|---|---|---|
| `#PRAGMA` | ✗ | Sets compiling options. |
| `#PRAGMA RESERVE` | ✗ | Reserves symbol name. |
| `#CMDLINE` | ✗ | Sets compiler command options from source. |
| `#LANG` | ✗ | Sets dialect from source. |
| `#PRINT` | ✗ | Outputs a messages to standard output while compiling. |
| `#ERROR` | ✗ | Outputs a messages to standard output and stops compilation. |
| `#ASSERT` | ✗ | Stops compilation with an error message if a given condition is false. |
| `#LINE` | ✗ | Sets the current line number and file name. |

##### Metacommands

| Keyword | Status | Description |
|---|---|---|
| `'$INCLUDE` | ✗ | Alternate form of the #INCLUDE directive. |
| `'$DYNAMIC` | ✗ | Alternate form of the OPTION DYNAMIC statement. |
| `'$STATIC` | ✗ | Alternate form of the OPTION STATIC statement. |
| `'$LANG` | ✗ | Alternate form of the #lang directive. |

#### Meta-statements

##### Metacommands

##### Compiler Options

##### Set Default Datatypes

| Keyword | Status | Description |
|---|---|---|
| `DEFLONGINT` | ✗ |  |
| `DEFULONGINT` | ✗ |  |

#### Intrinsic Defines

##### Platform Information

| Keyword | Status | Description |
|---|---|---|
| `__FB_WIN32__` | ✗ | Defined if compiling for Windows. |
| `__FB_LINUX__` | ✗ | Defined if compiling for Linux. |
| `__FB_DOS__` | ✗ | Defined if compiling for DOS. |
| `__FB_CYGWIN__` | ✗ | Defined if compiling for Cygwin. |
| `__FB_FREEBSD__` | ✗ | Defined if compiling for FreeBSD. |
| `__FB_NETBSD__` | ✗ | Defined if compiling for NetBSD. |
| `__FB_OPENBSD__` | ✗ | Defined if compiling for OpenBSD. |
| `__FB_DARWIN__` | ✗ | Defined if compiling for Darwin. |
| `__FB_XBOX__` | ✗ | Defined if compiling for Xbox. |
| `__FB_BIGENDIAN__` | ✗ | Defined if compiling on a system using big-endian byte-order. |
| `__FB_PCOS__` | ✗ | Defined if compiling for a common PC OS (e.g. DOS, Windows, OS/2). |
| `__FB_UNIX__` | ✗ | Defined if compiling for a Unix-like OS. |
| `__FB_64BIT__` | ✗ | Defined if compiling for a 64bit target. |
| `__FB_ARM__` | ✗ | Defined if compiling for the ARM architecture. |
| `__FB_PPC__` | ✗ | Defined if compiling for the PowerPC architecture. |
| `__FB_X86__` | ✗ | Defined if compiling for the X86 / X86_64 architecture. |
| `__FB_JS__` | ✗ | Defined if compiling for emscripten target. |
| `__FB_ANDROID__` | ✗ | Defined if compiling for android target. |

##### Version Information

| Keyword | Status | Description |
|---|---|---|
| `__FB_VERSION__` | ✗ | Defined as a string literal of the compiler version. |
| `__FB_VER_MAJOR__` | ✗ | Defined as an integral literal of the compiler major version number. |
| `__FB_VER_MINOR__` | ✗ | Defined as an integral literal of the compiler minor version number. |
| `__FB_VER_PATCH__` | ✗ | Defined as an integral literal of the compiler patch number. |
| `__FB_MIN_VERSION__` | ✗ | Macro to check for a minimum compiler version. |
| `__FB_BUILD_DATE__` | ✗ | Defined as a string literal of the compiler build date in "mm-dd-yyyy" format. |
| `__FB_BUILD_DATE_ISO__` | ✗ | Defined as a string literal of the compiler build date in "yyyy-mm-dd" format. |
| `__FB_SIGNATURE__` | ✗ | Defined as a string literal of the compiler signature. |
| `__FB_BUILD_SHA1__` | ✗ | Defined as a string literal of the compiler's source revision sha-1. |
| `__FB_BUILD_FORK_ID__` | ✗ | Defined as a string literal of the custom defined project fork identifier name. |

##### Command-line switches

| Keyword | Status | Description |
|---|---|---|
| `__FB_ASM__` | ✗ | Defined to either "intel" or "att" depending on -asm. |
| `__FB_BACKEND__` | ✗ | Defined to either "gas" or "gcc" depending on -gen. |
| `__FB_GCC__` | ✗ | True (-1) if -gen gcc is used, false (0) otherwise. |
| `__FB_OPTIMIZE__` | ✗ | Defined to the optimization level depending on -O. |
| `__FB_GUI__` | ✗ | True (-1) if the "-s gui" switch was used, false (0) otherwise. |
| `__FB_MAIN__` | ✗ | Defined if compiling a module with an entry point. |
| `__FB_DEBUG__` | ✗ | True (-1) if the "-g" switch was used, false (0) otherwise. |
| `__FB_ERR__` | ✗ | Zero (0) if neither the "-e", "-ex" or "-exx" switches were used. |
| `__FB_FPMODE__` | ✗ | Defined as "fast" if compiling for fast SSE math, "precise" otherwise. |
| `__FB_FPU__` | ✗ | Defined as "sse" if compiling for SSE floating point unit, or "x87" for normal x87 floating-point unit. |
| `__FB_LANG__` | ✗ | Defined to a string literal of the "-lang" dialect used. |
| `__FB_MT__` | ✗ | True (-1) if the "-mt" switch was used, false (0) otherwise. |
| `__FB_OUT_DLL__` | ✗ | True (-1) in a module being compiled and linked into a shared library, false (0) otherwise. |
| `__FB_OUT_EXE__` | ✗ | True (-1) in a module being compiled and linked into an executable, false (0) otherwise. |
| `__FB_OUT_LIB__` | ✗ | True (-1) in a module being compiled and linked into a static library, zero (0) otherwise. |
| `__FB_OUT_OBJ__` | ✗ | True (-1) in a module being compiled only, zero (0) otherwise. |
| `__FB_PROFILE__` | ✗ | Set to an integer to indicate the profiling method. |
| `__FB_SSE__` | ✗ | Defined if compiling for SSE floating point unit. |
| `__FB_VECTORIZE__` | ✗ | Defined as the level of automatic vectorization (0 to 2) |

##### Environment Information

| Keyword | Status | Description |
|---|---|---|
| `__FB_ARGC__` | ✗ | Defined as an integer literal of the number of command-line arguments passed to the program. |
| `__FB_ARGV__` | ✗ | Defined as a Zstring Ptr Ptr to the command line arguments passed to the program. |
| `__DATE__` | ✗ | Defined as a string literal of the compilation date in "mm-dd-yyyy" format. |
| `__DATE_ISO__` | ✗ | Defined as a string literal of the compilation date in "yyyy-mm-dd" format. |
| `__TIME__` | ✗ | Defined as a string literal of the compilation time. |
| `__PATH__` | ✗ | Defined as a string literal of the absolute path of the module. |

##### Context-specific Information

| Keyword | Status | Description |
|---|---|---|
| `__FILE__ and __FILE_NQ__` | ✗ | Defined as the name of the module. |
| `__FUNCTION__ and __FUNCTION_NQ__` | ✗ | Defined as the name of the procedure where it's used. |
| `__LINE__` | ✗ | Defined as an integer literal of the line of the module where it's used. |
| `__FB_OPTION_BYVAL__` | ✗ | True (-1) if parameters are declared by value by default, zero (0) otherwise. |
| `__FB_OPTION_DYNAMIC__` | ✗ | True (-1) if all arrays are variable-length, zero (0) otherwise. |
| `__FB_OPTION_ESCAPE__` | ✗ | True (-1) if string literals are processed for escape sequences, zero (0) otherwise. |
| `__FB_OPTION_GOSUB__` | ✗ | True (-1) if gosub support is enabled, zero (0) otherwise. |
| `__FB_OPTION_EXPLICIT__` | ✗ | True (-1) if variables and objects need to be explicitly declared, zero (0) otherwise. |
| `__FB_OPTION_PRIVATE__` | ✗ | True (-1) if all procedures are private by default, zero (0) otherwise. |
| `__FB_OPTION_PROFILE__` | ✗ | True (-1) if profiling code is generated, zero (0) otherwise. |

##### Basic-macros

| Keyword | Status | Description |
|---|---|---|
| `__FB_ARG_COUNT__` | ✗ | Counts the number of arguments in an argument list. |
| `__FB_ARG_EXTRACT__` | ✗ | Returns nth argument from an argument list. |
| `__FB_ARG_LEFTOF__` | ✗ | Returns left token based on separator. |
| `__FB_ARG_LISTEXPAND__` | ✗ | Expands a macro one or more time on an argument list |
| `__FB_ARG_RIGHTOF__` | ✗ | Returns right token based on separator. |
| `__FB_EVAL__` | ✗ | Evaluates an argument (expression) at compile time. |
| `__FB_IIF__` | ✗ | Returns an expression depending on the result of a comparison expression evaluated at compile time. |
| `__FB_JOIN__` | ✗ | Joins two token arguments together as one. |
| `__FB_QUERY_SYMBOL__` | ✗ | Queries a fbc's symbol internals. |
| `__FB_QUOTE__` | ✗ | Converts the argument to a string. |
| `__FB_UNIQUEID__` | ✗ | Gets the identifier at the top of a stack. |
| `__FB_UNIQUEID_POP__` | ✗ | Pops an identifier off of a stack. |
| `__FB_UNIQUEID_PUSH__` | ✗ | Pushes a new unique identifier on to a stack. |
| `__FB_UNQUOTE__` | ✗ | Takes a literal string and converts it back to tokens. |

##### Constants

| Keyword | Status | Description |
|---|---|---|
| `FALSE and TRUE` | ✗ | Intrinsic constants for the Boolean data type. |

#### Error Handling

| Keyword | Status | Description |
|---|---|---|
| `Err` | ✗ |  |

##### Default error handling

| Keyword | Status | Description |
|---|---|---|
| `Open` | ✓ |  |
| `Put #` | ✓ |  |

##### QuickBASIC-like error handling

| Keyword | Status | Description |
|---|---|---|
| `On Error` | ✓ |  |
| `On Error Goto 0 disables the error handling. If an error handling routine is not set when an error occurs, the program will stop and send the console an error message. Aborting program due to runtime error 2 (file not found) The error handler routine can be at the end of the program, as in QB. The On Local Error` | ✓ |  |
| `Sub` | ✓ |  |
| `Function` | ✓ |  |
| `Resume` | ✓ |  |
| `Resume Next` | ✓ |  |

##### Error codes

| Keyword | Status | Description |
|---|---|---|
| `Error` | ✗ |  |

##### 'On [Local] Error Goto' statement use

| Keyword | Status | Description |
|---|---|---|
| `Error` | ✗ |  |
| `Error` | ✗ |  |
| `Local` | ✗ |  |
| `Local` | ✗ |  |
| `Local` | ✗ |  |
| `Resume` | ✓ |  |
| `Resume Next` | ✓ |  |
| `__FB_ERR__` | ✗ |  |

#### Miscellaneous Keywords

##### Data

| Keyword | Status | Description |
|---|---|---|
| `DATA` | ✓ |  |
| `READ` | ✓ |  |
| `RESTORE` | ✓ |  |

##### Debugging

| Keyword | Status | Description |
|---|---|---|
| `ASSERT` | ✗ |  |
| `ASSERTWARN` | ✗ |  |
| `STOP` | ✓ |  |

##### Hardware Access

| Keyword | Status | Description |
|---|---|---|
| `INP` | ✗ |  |
| `LPRINT` | ✗ |  |
| `LPOS` | ✗ |  |
| `OUT` | ✗ |  |
| `WAIT` | ✓ |  |

##### Operating System

| Keyword | Status | Description |
|---|---|---|
| `BEEP` | ✗ |  |
| `SLEEP` | ✓ |  |
| `END (Statement)` | ✓ |  |

##### Stub Pages

| Keyword | Status | Description |
|---|---|---|
| `AS` | ✓ | Type annotation: `DIM v AS type`, `field AS type` (M3) |
| `FOR` | ✓ |  |
| `TO` | ✓ |  |
| `IS` | ✗ |  |
| `STEP` | ✓ |  |

##### Control Flow

| Keyword | Status | Description |
|---|---|---|
| `DO` | ✓ |  |
| `END IF` | ✓ |  |
| `IIF` | ✗ |  |
| `LOOP` | ✓ |  |
| `NEXT` | ✓ |  |
| `THEN` | ✓ |  |
| `UNTIL` | ✓ |  |
| `WEND (or 'END WHILE')` | ✗ |  |
| `WHILE` | ✓ |  |

##### Uncategorized

| Keyword | Status | Description |
|---|---|---|
| `END (Block)` | ✓ |  |
| `OFFSETOF` | ✗ |  |
| `SIZEOF` | ✗ |  |
| `TYPEOF` | ✗ |  |
| `LET` | ✓ |  |
| `REM` | ✓ |  |
| `OPTION()` | ✓ |  |

## Runtime Library Reference

### Array Functions

#### Defining Arrays

| Keyword | Status | Description |
|---|---|---|
| `OPTION DYNAMIC` | ✓ | Forces arrays to be defined as variable-length arrays. |
| `'$DYNAMIC` | ✗ | Alternate form of the OPTION DYNAMIC statement. |
| `OPTION STATIC` | ✓ | Reverts a previous OPTION DYNAMIC command. |
| `'$STATIC` | ✗ | Alternate form of the OPTION STATIC statement. |
| `DIM` | ✓ | Defines any type of array. |
| `REDIM` | ✗ | Defines and resizes variable-length arrays. |
| `PRESERVE` | ✗ | Preserves array contents when used with REDIM. |

#### Clearing Array Data

| Keyword | Status | Description |
|---|---|---|
| `ERASE` | ✗ | Destroys variable-length array elements and initializes fixed-length array elements. |

#### Retrieving Array Size

| Keyword | Status | Description |
|---|---|---|
| `ARRAYLEN` | ✗ | Returns the total number of array elements. |
| `ARRAYSIZE` | ✗ | Returns the total array size (in bytes). |
| `LBOUND` | ✗ | Returns the lower bound of an array's dimension. |
| `UBOUND` | ✗ | Returns the upper bound of an array's dimension. |

#### Retrieving Array Descriptor

| Keyword | Status | Description |
|---|---|---|
| `Array[Const]DescriptorPtr` | ✗ | Returns a [constant] pointer to array's descriptor (FBARRAY). |

### Bit Manipulation

| Keyword | Status | Description |
|---|---|---|
| `Uinteger` | ✗ |  |

#### Byte Manipulation Macros

| Keyword | Status | Description |
|---|---|---|
| `LOBYTE` | ✗ | Gets the least significant byte (LSB, or lo-byte) value of an Uinteger value. |
| `HIBYTE` | ✗ | Gets the most significant byte (MSB, or hi-byte) value of the least significant word (LSW, or lo-word) of an Uinteger value. |
| `LOWORD` | ✗ | Gets the least significant word (LSW, or lo-word) value of an Uinteger value. |
| `HIWORD` | ✗ | Gets the most significant word (LSW, or hi-word) value of an Uinteger value. |

#### Bit Manipulation Macros

| Keyword | Status | Description |
|---|---|---|
| `BIT` | ✗ | Gets the state of an individual bit in an integer value. |
| `BITRESET` | ✗ | Gets the value of an integer with a specified bit cleared. |
| `BITSET` | ✗ | Gets the value of an integer with a specified bit set. |

### Console Functions

#### Configuring the Console

| Keyword | Status | Description |
|---|---|---|
| `CLS` | ✗ | Clears the entire screen or text viewport. |
| `WIDTH` | ✓ | Sets or returns the number of rows and columns of the console display. |
| `VIEW PRINT` | ✗ | Sets the printable area of the console screen. |

#### Cursor Color and Positioning

| Keyword | Status | Description |
|---|---|---|
| `COLOR` | ✓ | Changes the foreground and background color of text to be written. |
| `CSRLIN` | ✗ | Returns the row position of the cursor. |
| `POS` | ✓ | Returns the column position of the cursor. |
| `LOCATE` | ✓ | Sets the row and column position of the cursor and its visibility. |
| `SCREEN (Console)` | ✗ | Gets the character or color attribute at a given location. |

#### Writing Text to the Console

| Keyword | Status | Description |
|---|---|---|
| `PRINT` | ✓ |  |
| `? (Shortcut for 'PRINT')` | ✗ | Writes text to the console. |
| `PRINT USING` | ✓ |  |
| `? USING (Shortcut for 'PRINT USING')` | ✗ | Writes formatted text to the console. |
| `WRITE` | ✗ | Writes a list of items to the console. |
| `SPC` | ✓ | Skips a number of spaces when writing text. |
| `TAB` | ✓ | Skips to a certain column when writing text. |

### Date and Time Functions

#### VisualBasic compatible procedures

| Keyword | Status | Description |
|---|---|---|
| `NOW` | ✗ | Gets a date serial of the current date and time. |
| `DATESERIAL` | ✗ | Gets the date serial representation of a date. |
| `TIMESERIAL` | ✗ | Gets the date serial representation of a time. |
| `DATEVALUE` | ✗ | Gets the date serial representation of a date expressed as a string. |
| `TIMEVALUE` | ✗ | Gets the date serial representation of a time expressed as a string. |
| `SECOND` | ✗ | Gets the seconds of the hour from a date serial. |
| `MINUTE` | ✗ | Gets the minutes of the hour from a date serial. |
| `HOUR` | ✗ | Gets the hour of the day from a date serial. |
| `DAY` | ✗ | Gets the day of the month from a date serial. |
| `WEEKDAY` | ✗ | Gets the day of the week from a date serial. |
| `MONTH` | ✗ | Gets the month of the year from a date serial. |
| `YEAR` | ✗ | Gets the year from a date serial. |
| `DATEPART` | ✗ | Gets a time interval from a date serial. |
| `DATEADD` | ✗ | Gets the result of a time interval added to a date serial. |
| `DATEDIFF` | ✗ | Gets a time interval between two date serials. |
| `ISDATE` | ✗ | Tests if a String can be converted to a date serial. |
| `MONTHNAME` | ✗ | Gets the month name of its integer representation. |
| `WEEKDAYNAME` | ✗ | Gets the weekday name of its integer representation. |

#### Date and time procedures

| Keyword | Status | Description |
|---|---|---|
| `DATE` | ✗ | Gets the string representation of the current system date. |
| `TIME` | ✗ | Gets the string representation of the current system time. |
| `SETDATE` | ✗ | Sets the current system date. |
| `SETTIME` | ✗ | Sets the current system time. |
| `TIMER` | ✗ | Gets a counter expressed in seconds. |

### Error Handling Functions

#### Determining Errors

| Keyword | Status | Description |
|---|---|---|
| `ERL` | ✗ | Gets the line in source code where the error occurred. |
| `ERFN` | ✗ | Gets the name of the function where the error occurred. |
| `ERMN` | ✗ | Gets the name of the source file where the error occurred. |
| `ERR` | ✗ | Gets the error number of the last error that occurred. |
| `ERROR` | ✗ | Generates an error using an error number. |

#### Handling Errors

| Keyword | Status | Description |
|---|---|---|
| `ON ERROR` | ✓ | Sets a global error handler using a label. |
| `ON LOCAL ERROR` | ✓ | Sets a local error handler using a label. |
| `RESUME` | ✓ | Resumes execution at the line where the error occurred. |
| `RESUME NEXT` | ✓ | Resumes execution at the line after where the error occurred. |

### File I/O Functions

#### Opening Files or Devices

| Keyword | Status | Description |
|---|---|---|
| `FREEFILE` | ✗ | Gets an available file number that can be used to read or write from files or devices. |
| `OPEN` | ✓ | Binds a file number to a physical file to provide reading and writing capabilities. |
| `OPEN COM` | ✓ | Binds a file number to a communications port. |
| `OPEN CONS` | ✓ | Binds a file number to the standard input and output streams. |
| `OPEN ERR` | ✓ | Binds a file number to the standard input and error streams. |
| `OPEN LPT` | ✓ | Binds a file number to a printer device. |
| `OPEN PIPE` | ✓ | Binds a file number to the input and output streams of a process. |
| `OPEN SCRN` | ✓ | Binds a file number directly to the console. |
| `CLOSE` | ✓ | Unbinds a file number from a file or device. |
| `RESET` | ✗ | Unbinds all active file numbers. |
| `INPUT (File Mode)` | ✓ | Text data can be read from the file. |
| `OUTPUT` | ✗ | Text data can be written to the file. |
| `APPEND` | ✓ | Text data is added to the end of a file when output. |
| `BINARY` | ✗ | Arbitrary data can be read from or written to the file. |
| `RANDOM` | ✗ | Blocks of data of certain size can be read from and written to the file. |
| `ACCESS` | ✗ | An overview of file access privileges. |
| `READ (File Access)` | ✓ | Binary data can only be read from the file. |
| `WRITE (File Access)` | ✗ | Binary data can only be written to the file. |
| `READ WRITE(File Access)` | ✓ | Binary data can be read from and written to the file. |
| `ENCODING` | ✗ | Specifies the character encoding of a file. |

#### Reading from and Writing to Files or Devices

| Keyword | Status | Description |
|---|---|---|
| `INPUT #` | ✓ | Reads a list of values from a file or device. |
| `WRITE #` | ✗ | Writes a list of values to a file or device. |
| `INPUT()` | ✓ | Reads a number of characters from a file or device. |
| `WINPUT()` | ✗ | Reads a number of wide characters from a file or device. |
| `LINE INPUT #` | ✗ | Reads a line of text from a file or device. |
| `PRINT #` | ✓ |  |
| `? # (Shortcut for 'PRINT #')` | ✗ | Writes text data to a file or device. |
| `PUT #` | ✓ | Writes arbitrary data to a file or device. |
| `GET #` | ✓ | Reads arbitrary data from a file or device. |

#### File Position and other Info

| Keyword | Status | Description |
|---|---|---|
| `LOF` | ✗ | Gets the length (in bytes) of a file. |
| `LOC` | ✗ | Gets the file position of the last read or write operation. |
| `EOF` | ✗ | Returns true if all of the data has been read from a file. |
| `SEEK (Statement)` | ✗ | Sets the file position of the next read or write operation. |
| `SEEK (Function)` | ✗ | Gets the file position of the next read or write operation. |
| `LOCK` | ✗ | Restricts read or write access to a file or portion of a file. |
| `UNLOCK` | ✗ | Remove read or write restrictions from a previous Lock command. |

### Mathematical Functions

#### Algebraic Procedures

| Keyword | Status | Description |
|---|---|---|
| `ABS` | ✓ | Returns the absolute value of a number. |
| `EXP` | ✓ | Returns e raised to some power. |
| `LOG` | ✓ | Returns the natural logarithm of a number. |
| `SQR` | ✓ | Returns the square root of a number. |
| `FIX` | ✗ | Returns the integer part of a number. |
| `FRAC` | ✗ | Returns the fractional part of a number. |
| `INT` | ✓ | Returns the largest integer less than or equal to a number. |
| `SGN` | ✓ | Returns the sign of a number. |

#### Trigonometric Procedures

| Keyword | Status | Description |
|---|---|---|
| `SIN` | ✓ | Returns the sine of an angle. |
| `ASIN` | ✗ | Returns the arcsine of a number. |
| `COS` | ✓ | Returns the cosine of an angle. |
| `ACOS` | ✗ | Returns the arccosine of a number. |
| `TAN` | ✓ | Returns the tangent of an angle. |
| `ATN` | ✓ | Returns the arctangent of a number. |
| `ATAN2` | ✗ | Returns the arctangent of the ratio between two numbers. |

#### Miscellaneous Procedures

| Keyword | Status | Description |
|---|---|---|
| `RANDOMIZE` | ✗ | Seeds the random number generator used by Rnd. |
| `RND` | ✓ | Returns a random Double in the range [0, 1). |

### Memory Functions

#### Working with Dynamic Memory

| Keyword | Status | Description |
|---|---|---|
| `ALLOCATE` | ✗ | Reserves a number of bytes of uninitialized memory and returns the address. |
| `CALLOCATE` | ✗ | Reserves a number of bytes of initialized (zeroed) memory and returns the address. |
| `REALLOCATE` | ✗ | Changes the size of reserved memory. |
| `DEALLOCATE` | ✗ | Returns reserved memory back to the system. |

#### Miscellaneous Procedures

| Keyword | Status | Description |
|---|---|---|
| `PEEK` | ✓ | Reads some type of value from an address. |
| `POKE` | ✓ | Writes some type of value to an address. |
| `CLEAR` | ✗ | Clears data in an array with a specified value. |
| `FB_MEMCOPY` | ✗ | Copies a block of memory from a location to another. (memory areas must not overlap) |
| `FB_MEMCOPYCLEAR` | ✗ | Copies the first part of a block of memory from a location to another and clears the rest. (memory areas must not overlap) |
| `FB_MEMMOVE` | ✗ | Copies a block of memory from a location to another. (memory areas may overlap) |
| `SWAP` | ✓ | Exchange the contents of two variables. |
| `SADD` | ✗ | Returns the address for the data in a zstring/wstring variable. |

### Operating System Functions

#### Working with Files

| Keyword | Status | Description |
|---|---|---|
| `EXEC and CHAIN` | ✗ | Temporarily transfers control to another program. |
| `RUN` | ✓ | Transfers control to another program. |
| `KILL` | ✗ | Deletes an existing file. |
| `NAME` | ✗ | Renames an existing file. |

#### File Properties

| Keyword | Status | Description |
|---|---|---|
| `FILEATTR` | ✗ | Gets information about a file bound to a file number. |
| `FILECOPY` | ✗ | Copies a file. |
| `FILEDATETIME` | ✗ | Gets the last modified date and time of a file. |
| `FILEEXISTS` | ✗ | Tests for the existence of a file. |
| `FILELEN` | ✗ | Gets the length (in bytes) of a file. |
| `FILESETEOF` | ✗ | Sets the length of an open file bound to a file number. |
| `FILEFLUSH` | ✗ | Flushes application or system buffers for an open file bound to a file number. |

#### Working with Directories

| Keyword | Status | Description |
|---|---|---|
| `CURDIR` | ✗ | Gets the current working directory. |
| `CHDIR` | ✓ | Sets the current working directory. |
| `DIR` | ✓ | Gets the names of files or directories matching certain attributes. |
| `EXEPATH` | ✗ | Gets the directory of the current running program. |
| `MKDIR` | ✓ | Creates a new directory. |
| `RMDIR` | ✗ | Deletes an existing directory. |

#### System Procedures

| Keyword | Status | Description |
|---|---|---|
| `FRE` | ✓ | Gets the amount of free memory (in bytes) available. |
| `COMMAND` | ✗ | Gets the command-line parameters passed to the program. |
| `ENVIRON` | ✗ | Gets the value of an environment variable. |
| `ISREDIRECTED` | ✗ | Checks whether stdin or stdout is redirected to a file or not. |
| `SETENVIRON` | ✗ | Sets the value of an environment variable. |
| `SHELL` | ✗ | Sends a command to the system command interpreter. |
| `SYSTEM` | ✗ | Closes all open files and exits the program. |

### String Functions

#### Creating Strings

| Keyword | Status | Description |
|---|---|---|
| `STRING` | ✗ | Standard data type: 8 bit character string. |
| `STRING (Function)` | ✗ | Returns a String of multiple characters. |
| `ZSTRING` | ✗ | Standard data type: null terminated 8 bit character string. |
| `WSTRING` | ✗ | Standard data type: wide character string. |
| `WSTRING (Function)` | ✗ | Returns a WString of multiple characters. |
| `SPACE` | ✗ | Returns a String consisting of spaces. |
| `WSPACE` | ✗ | Returns a WString consisting of spaces. |
| `LEN` | ✓ | Returns the length of a string in characters. |

#### Character Conversion

| Keyword | Status | Description |
|---|---|---|
| `ASC` | ✓ | Returns an Integer representation of an character. |
| `CHR` | ✗ | Returns a string of one or more characters from their ASCII Integer representation. |
| `WCHR` | ✗ | Returns a WString of one or more characters from their Unicode Integer representation. |

#### Numeric/Boolean to String Conversions

| Keyword | Status | Description |
|---|---|---|
| `BIN` | ✗ | Returns a binary String representation of an integral value. |
| `WBIN` | ✗ | Returns a binary WString representation of an integral value. |
| `HEX` | ✗ | Returns a hexadecimal String representation of an integral value. |
| `WHEX` | ✗ | Returns a hexadecimal WString representation of an integral value. |
| `OCT` | ✗ | Returns an octal String representation of an integral value. |
| `WOCT` | ✗ | Returns an octal WString representation of an integral value. |
| `STR` | ✗ | Returns the String representation of numeric value or boolean. |
| `WSTR` | ✗ | Returns the WString representation of numeric value. |
| `FORMAT` | ✗ | Returns a formatted String representation of a Double. |

#### String to Numeric Conversions

| Keyword | Status | Description |
|---|---|---|
| `VAL` | ✓ | Returns the Double conversion of a numeric string. |
| `VALINT` | ✗ | Returns the Integer conversion of a numeric string. |
| `VALLNG` | ✗ | Returns the Long conversion of a numeric string. |
| `VALUINT` | ✗ | Returns the uInteger conversion of a numeric string. |
| `VALULNG` | ✗ | Returns the ULong conversion of a numeric string. |

#### Numeric Serialization

| Keyword | Status | Description |
|---|---|---|
| `MKD` | ✗ | Returns an eight character String representation of a Double. |
| `MKI` | ✗ | Returns a four character String representation of a Integer. |
| `MKL` | ✗ | Returns a four character String representation of a Long. |
| `MKLONGINT` | ✗ | Returns an eight character String representation of a Longint. |
| `MKS` | ✗ | Returns a four character String representation of a Single. |
| `MKSHORT` | ✗ | Returns a two character String representation of a Short. |
| `CVD` | ✗ | Returns a Double representation of an eight character String. |
| `CVI` | ✗ | Returns an Integer representation of a four character String. |
| `CVL` | ✗ | Returns a Long representation of a four character String. |
| `CVLONGINT` | ✗ | Returns a Longint representation of an eight character String. |
| `CVS` | ✗ | Returns a Single representation of a four character String. |
| `CVSHORT` | ✗ | Returns a Short representation of a two character String. |

#### Working with Substrings

| Keyword | Status | Description |
|---|---|---|
| `LEFT` | ✗ | Returns a substring of the leftmost characters in a string. |
| `MID (Function)` | ✗ | Returns a substring of a string. |
| `RIGHT` | ✗ | Returns a substring of the rightmost characters in a string. |
| `LCASE` | ✗ | Returns a copy of a string converted to lowercase alpha characters. |
| `UCASE` | ✗ | Returns a copy of a string converted to uppercase alpha characters. |
| `LTRIM` | ✗ | Removes surrounding substrings or characters on the left side of a string. |
| `RTRIM` | ✗ | Removes surrounding substrings or characters on the right side of a string. |
| `TRIM` | ✗ | Removes surrounding substrings or characters on the left and right side of a string. |
| `INSTR` | ✓ | Returns the first occurrence of a substring or character within a string. |
| `INSTRREV` | ✗ | Returns the last occurrence of a substring or character within a string. |
| `MID (Statement)` | ✗ | Copies a substring to a substring of a string. |
| `LSET` | ✗ | Left-justifies a string. |
| `RSET` | ✗ | Right-justifies a string. |

### Threading Support Functions

#### Threads

| Keyword | Status | Description |
|---|---|---|
| `THREADCALL` | ✗ | Starts a procedure with parameters in a separate thread of execution. |
| `THREADCREATE` | ✗ | Starts a procedure in a separate thread of execution. |
| `THREADWAIT` | ✗ | Waits for a thread to finish and releases the thread handle. |
| `THREADDETACH` | ✗ | Releases a thread handle without waiting for the thread to finish. |
| `THREADSELF` | ✗ | Returns the thread handle of the current thread. |

#### Mutexes

| Keyword | Status | Description |
|---|---|---|
| `MUTEXCREATE` | ✗ | Creates a mutex. |
| `MUTEXLOCK` | ✗ | Acquires a lock on a mutex. |
| `MUTEXUNLOCK` | ✗ | Releases a lock on a mutex. |
| `MUTEXDESTROY` | ✗ | Destroys a mutex that is no longer needed. |

#### Conditional Variables

| Keyword | Status | Description |
|---|---|---|
| `CONDCREATE` | ✗ | Creates a conditional variable. |
| `CONDWAIT` | ✗ | Pauses execution of a threaded procedure. |
| `CONDSIGNAL` | ✗ | Resumes execution of a threaded procedure waiting for a conditional. |
| `CONDBROADCAST` | ✗ | Resumes all threaded procedures waiting for a conditional. |
| `CONDDESTROY` | ✗ | Destroys a conditional variable that is no longer needed. |

### User Input Functions

#### Reading values from the keyboard buffer

| Keyword | Status | Description |
|---|---|---|
| `INPUT` | ✓ | Reads values from the keyboard buffer. |
| `LINE INPUT` | ✗ | Reads a line of text from the keyboard buffer. |
| `INPUT()` | ✓ | Reads a number of characters from the keyboard buffer, file or device. |
| `WINPUT()` | ✗ | Reads a number of wide characters from the keyboard buffer, file or device. |

#### Reading keys from the keyboard buffer

| Keyword | Status | Description |
|---|---|---|
| `INKEY` | ✗ | Gets the first key, if any, waiting in the keyboard buffer. |
| `GETKEY` | ✓ | Gets and waits for the first key in the keyboard buffer. |

#### Detecting key status by keyboard scancode

| Keyword | Status | Description |
|---|---|---|
| `MULTIKEY` | ✗ | Detects the status of a key by its scancode. |

### Graphics - 2D Drawing

#### Working with Color

| Keyword | Status | Description |
|---|---|---|
| `COLOR` | ✓ | Sets the foreground and background color to use with the drawing procedures. |
| `PALETTE` | ✗ | Gets or sets color table information in paletted modes. |
| `RGB` | ✗ | Returns a color value for hi/truecolor modes. |
| `RGBA` | ✓ | Returns a color value including alpha (transparency) for hi/truecolor modes. |
| `POINT` | ✗ | Gets a pixel value from an image buffer or screen. |

#### Drawing to Image Buffers

| Keyword | Status | Description |
|---|---|---|
| `PSET and PRESET` | ✗ | Plots a single pixel on an image buffer or screen. |
| `LINE (GRAPHICS)` | ✗ | Plots a line of pixels on an image buffer or screen. |
| `CIRCLE` | ✓ | Plots circles and ellipses on an image buffer or screen. |
| `DRAW` | ✓ | Draws in a sequence of commands on an image buffer or screen. |
| `DRAW STRING` | ✓ | Writes text to an image buffer or screen. |
| `PAINT` | ✓ | Fills an area with color on an image buffer or screen. |

#### Image Buffer Creation

| Keyword | Status | Description |
|---|---|---|
| `GET (GRAPHICS)` | ✓ | Creates an image buffer from a portion of another image buffer or screen. |
| `IMAGECREATE` | ✗ | Creates an image buffer of a certain size and pixel depth. |
| `IMAGEDESTROY` | ✗ | Frees an image buffer resource. |
| `IMAGECONVERTROW` | ✗ | Converts a row of pixels in an image buffer to a different color depth. |
| `IMAGEINFO` | ✗ | Retrieves useful information about an image buffer |
| `BLOAD` | ✓ | Creates an image buffer from a file. |
| `BSAVE` | ✓ | Saves an image buffer to a file. |

#### Blitting Image Buffers

| Keyword | Status | Description |
|---|---|---|
| `PUT (GRAPHICS)` | ✓ | Blits an image buffer to another image buffer or screen. |
| `ADD` | ✗ | Saturated addition of the source and target components. |
| `ALPHA` | ✗ | Blend using a uniform transparency or the image buffer's alpha channel. |
| `AND (Graphics Put)` | ✓ | Combine the source and target components using a bitwise And |
| `OR` | ✓ | Combine the source and target components using a bitwise Or |
| `PSET` | ✗ | Directly copy pixel colors from the source to the destination. |
| `TRANS` | ✗ | Pixels matching the transparent mask color are not blitted. |
| `CUSTOM` | ✗ | Allows a custom blending procedure to be used. |
| `XOR` | ✓ | Combine the source and target components using a bitwise Xor |

### Graphics - User Input

#### Mouse and Joystick Input

| Keyword | Status | Description |
|---|---|---|
| `GETMOUSE` | ✗ | Gets button and axis information for the mouse. |
| `SETMOUSE` | ✗ | Sets position and visibility of the mouse cursor. |
| `GETJOYSTICK` | ✗ | Gets button and axis information for gaming devices. |
| `STICK` | ✗ | Gets axis position for gaming devices. |
| `STRIG` | ✗ | Gets button state for gaming devices. |

#### Keyboard Input

| Keyword | Status | Description |
|---|---|---|
| `MULTIKEY` | ✗ | Gets key information for the keyboard. |

### Graphics - Screen

#### Working with screen modes

| Keyword | Status | Description |
|---|---|---|
| `SCREENLIST` | ✗ | Gets the available fullscreen resolutions. |
| `SCREEN (Graphics) and SCREENRES` | ✗ | Sets a new graphics display mode. |
| `SCREENINFO` | ✗ | Gets information about the system desktop or current display mode. |
| `SCREENCONTROL` | ✗ | Gets or sets internal graphics library settings. |
| `SCREENEVENT` | ✗ | Gets system events. |
| `SCREENGLPROC` | ✗ | Returns the address of an OpenGL procedure. |
| `WINDOWTITLE` | ✗ | Sets the running program's window caption. |

#### Working with pages

| Keyword | Status | Description |
|---|---|---|
| `CLS` | ✗ | Clears the entire screen or viewport. |
| `SCREENSET` | ✗ | Sets the current work and visible pages. |
| `SCREENCOPY and PCOPY and FLIP` | ✗ | Copies pixel data from one page to another. |
| `SCREENSYNC` | ✗ | Waits for the vertical refresh of the monitor. |

#### Working video memory

| Keyword | Status | Description |
|---|---|---|
| `SCREENPTR` | ✗ | Gets the address of the working page's framebuffer. |
| `SCREENLOCK` | ✗ | Locks the current working page's framebuffer for direct access. |
| `SCREENUNLOCK` | ✗ | Reverts a previous ScreenLock command. |

#### Screen Metrics

| Keyword | Status | Description |
|---|---|---|
| `VIEW (GRAPHICS)` | ✗ | Sets a clipping region for all drawing and blitting procedures. |
| `WINDOW` | ✓ | Sets a new coordinate mapping for the current viewport. |
| `PMAP` | ✗ | Converts coordinates between physical and view mappings. |
| `POINTCOORD` | ✗ | Queries Draw's pen position. |

#### Screen Data Types

| Keyword | Status | Description |
|---|---|---|
| `EVENT` | ✗ | Data type for ScreenEvent function. |
