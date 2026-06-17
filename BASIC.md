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

## Procedures (2/2 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `DEF` | ✓ | Define function (DEF FNname(var) = expression) |
| `FN` | ✓ | Function call (FNname(value)) |

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
