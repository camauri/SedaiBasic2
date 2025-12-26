# SedaiBasic - BASIC Commands

## Implementation Progress

**100 / 188 commands implemented (53%)**

```
[██████████████████████████▌·····················] 53%
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

## Flow Control - Program Execution (5/8 - 63%)

| Command | Status | Description |
|---------|--------|-------------|
| `CONT` | ✗ | Continue program execution |
| `END` | ✓ | Ends program execution |
| `FAST` | ✓ | Set fast speed clock (shows black overlay) |
| `RUN` | ✓ | Execute program (RUN, RUN "filename") |
| `SLEEP` | ✓ | Delay program for n seconds (0 < n < 65536, interruptible with CTRL+C) |
| `SLOW` | ✓ | Set slow speed clock (hides black overlay) |
| `STOP` | ✓ | Halt program execution |
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
| `DEF` | ✓ | Define function |
| `FN` | ✓ | Function call |

## Data Management (6/7 - 86%)

| Command | Status | Description |
|---------|--------|-------------|
| `CLR` | ✗ | Clear all variables |
| `CONST` | ✓ | Constant assignment |
| `DATA` | ✓ | Data statement |
| `DIM` | ✓ | Dimension arrays |
| `LET` | ✓ | Variable assignment |
| `READ` | ✓ | Read data |
| `RESTORE` | ✓ | Restore data pointer |

## Standard Input/Output (2/7 - 29%)

| Command | Status | Description |
|---------|--------|-------------|
| `GET` | ✗ | Get character |
| `GETKEY` | ✗ | Get keypress |
| `INPUT` | ✓ | Input statement |
| `CHAR` | ✗ | Displays char at the specific position |
| `PRINT` | ✓ | Print statement |
| `PUDEF` | ✗ | Redefine symbols in PRINT USING |
| `USING` | ✗ | Output using format |

## File Input/Output (0/3 - 0%)

| Command | Status | Description |
|---------|--------|-------------|
| `GET#` | ✗ | Get char from file |
| `INPUT#` | ✗ | Input from file |
| `PRINT#` | ✗ | Print on file |

## I/O Control (0/1 - 0%)

| Command | Status | Description |
|---------|--------|-------------|
| `CMD` | ✗ | Redirect screen output |

## DOS Commands (16/26 - 62%)

| Command | Status | Description |
|---------|--------|-------------|
| `APPEND` | ✗ | Append data to sequential file |
| `BACKUP` | ✗ | Copy disk content to another disk |
| `BLOAD` | ✓ | Load bytecode file (.basc) |
| `BOOT` | ✓ | Load and execute bytecode file (BLOAD + RUN) |
| `BSAVE` | ✓ | Save bytecode file (.basc) |
| `CATALOG` | ✓ | Display drive directory |
| `CLOSE` | ✗ | Close file |
| `COLLECT` | ✗ | Free inaccessible disk space |
| `CONCAT` | ✗ | Attaches source file to destination file |
| `COPY` | ✓ | Copy file(s) with wildcard support (COPY src dst [OVERWRITE]) |
| `DCLEAR` | ✗ | Clear all open channels on disk drive |
| `DCLOSE` | ✗ | Close disk drive file(s) |
| `DIR` | ✓ | Display drive directory (alias for DIRECTORY) |
| `DIRECTORY` | ✓ | Display drive directory |
| `DLOAD` | ✓ | Load BASIC file |
| `DOPEN` | ✗ | Open sequential file for r/w |
| `DSAVE` | ✓ | Save BASIC file |
| `DVERIFY` | ✓ | Verify saved BASIC file |
| `HEADER` | ✗ | Formats a diskette |
| `LOAD` | ✓ | Load program |
| `OPEN` | ✗ | Open file for input/output |
| `RECORD` | ✗ | Position relative file pointer |
| `RENAME` | ✓ | Rename file (RENAME oldname newname) |
| `SAVE` | ✓ | Save program |
| `SCRATCH` | ✓ | Delete file(s) with wildcard support (SCRATCH pattern [FORCE]) |
| `VERIFY` | ✓ | Verify saved file or program |

## String Functions (7/11 - 64%)

| Function | Status | Description |
|----------|--------|-------------|
| `ASC` | ✓ | Return character code |
| `CHR$` | ✓ | Return character from code |
| `HEX$` | ✗ | Hex number string from decimal number |
| `INSTR` | ✗ | Position of source string in destination string |
| `LEN` | ✓ | Return string length |
| `LEFT$` | ✓ | Return string leftmost chars |
| `MID$` | ✓ | Return substring from larger string |
| `RIGHT$` | ✓ | Return string rightmost chars |
| `SPC` | ✗ | Skip spaces on context output |
| `STR$` | ✓ | Convert number to string |
| `TAB` | ✗ | Move cursor forward string from the first column |

## Memory Management (3/9 - 33%)

| Command/Function | Status | Description |
|------------------|--------|-------------|
| `BANK` | ✗ | Select RAM bank (0-15) |
| `FETCH` | ✗ | Get data from expansion RAM |
| `POKE` | ✗ | Set content of specific RAM location |
| `RREG` | ✗ | Read contents of accumulator and registers |
| `STASH` | ✗ | Move content of host RAM to expansion RAM |
| `SWAP` | ✗ | Swap content of host RAM to expansion RAM |
| `FRE` | ✓ | Return RAM bytes free |
| `PEEK` | ✓ | Return content of specific RAM location |
| `POINTER` | ✓ | Return the address of a variable name |

## Graphics Management (10/19 - 53%)

| Command/Function | Status | Description |
|------------------|--------|-------------|
| `BOX` | ✓ | Draw a box |
| `CIRCLE` | ✓ | Draws circles, ellipses, arcs and polygons |
| `COLOR` | ✗ | Define colors for each screen area |
| `DRAW` | ✓ | Draw dots, lines and shapes |
| `GLIST` | ✓ | List available SDL2 video modes |
| `GRAPHIC` | ✓ | Select a graphic mode |
| `GSHAPE` | ✗ | Retrieve shape from string variable |
| `LOCATE` | ✓ | Position the bit map pixel cursor on the screen |
| `PAINT` | ✗ | Fill area with color |
| `SCALE` | ✗ | Alter scaling in graphics mode |
| `SCNCLR` | ✓ | Clear screen |
| `SSHAPE` | ✗ | Save shapes to string variable |
| `WIDTH` | ✗ | Set the width of drawn lines |
| `WINDOW` | ✗ | Defines a screen window |
| `POS` | ✗ | Return the current cursor column position |
| `RCLR` | ✗ | Return color of color source |
| `RDOT` | ✓ | Return current position or color of pixel cursor |
| `RGR` | ✓ | Return current graphic mode |
| `RWINDOW` | ✗ | Return the size of the current window |

## Sprite Management (0/10 - 0%)

| Command/Function | Status | Description |
|------------------|--------|-------------|
| `COLLISION` | ✗ | Define handling for sprite collision interrupt |
| `SPRITE` | ✗ | Set sprite properties |
| `MOVSPR` | ✗ | Position or move sprite on the screen |
| `SPRCOLOR` | ✗ | Set multicolor 1 and/or multicolor 2 colors for all sprites |
| `SPRDEF` | ✗ | Enter the SPRite DEFinition mode |
| `SPRSAV` | ✗ | Store a sprite data from a text string or vice versa |
| `BUMP` | ✗ | Return sprite collision information |
| `RSPCOLOR` | ✗ | Return sprite multicolor values |
| `RSPPOS` | ✗ | Return the speed and position values of a sprite |
| `RSPRITE` | ✗ | Return sprite characteristics |

## Audio Management (0/6 - 0%)

| Command | Status | Description |
|---------|--------|-------------|
| `ENVELOPE` | ✗ | Define a musical instrument envelope |
| `FILTER` | ✗ | Define sound (SID chip) filter parameters |
| `PLAY` | ✗ | Define and play musical notes and elements |
| `SOUND` | ✗ | Outputs sound effects and musical notes |
| `TEMPO` | ✗ | Define the speed of the song being played |
| `VOL` | ✗ | Define output level of sound |

## Math Functions (14/18 - 78%)

| Function | Status | Description |
|----------|--------|-------------|
| `ABS` | ✓ | Return absolute value |
| `ATN` | ✓ | Return arctangent of argument |
| `ATAN` | ✓ | Return arctangent of argument |
| `COS` | ✓ | Return cosine of angle of x radians |
| `DEC` | ✗ | Convert hex number string to decimal |
| `EXP` | ✓ | Return value of e raised to the power x |
| `INT` | ✓ | Convert float number to integer |
| `LN` | ✓ | Return natural log of x |
| `LOG` | ✓ | Return natural log of x |
| `LOG10` | ✗ | Return base 10 log of x |
| `LOG2` | ✗ | Return base 2 log of x |
| `RND` | ✓ | Return a random number from 0 (included) to 1 (excluded) |
| `SGN` | ✓ | Return sign of argument |
| `SIN` | ✓ | Return sine of argument |
| `SQR` | ✓ | Return square root of argument |
| `TAN` | ✓ | Return tangent of argument |
| `VAL` | ✓ | Return the numeric value of a number string |

## Reserved Variables (0/7 - 0%)

| Variable | Status | Description |
|----------|--------|-------------|
| `DS` | ✗ | Get disk status code |
| `DS$` | ✗ | Get disk status message |
| `EL` | ✗ | Return last error line |
| `ER` | ✗ | Return last error code |
| `ST` | ✗ | Get I/O status byte |
| `TI` | ✗ | Get time elapsed from power on |
| `TI$` | ✗ | Get/set 24h clock |

## Error Handling (0/3 - 0%)

| Command/Function | Status | Description |
|------------------|--------|-------------|
| `RESUME` | ✗ | Resume from the given line after error |
| `TRAP` | ✗ | Detect error and go to the given line |
| `ERR$` | ✗ | Print error message by id |

## Debug (0/3 - 0%)

| Command | Status | Description |
|---------|--------|-------------|
| `HELP` | ✗ | Highlight the line where the error occurred |
| `TRON` | ✗ | Set tracing mode on |
| `TROFF` | ✗ | Set tracing mode off |

## Machine Language (0/3 - 0%)

| Command/Function | Status | Description |
|------------------|--------|-------------|
| `MONITOR` | ✗ | Enter ML monitor |
| `SYS` | ✗ | Execute ML subroutine |
| `USR` | ✗ | Call user-defined ML subfunction |

## Program Editing (2/5 - 40%)

| Command | Status | Description |
|---------|--------|-------------|
| `AUTO` | ✗ | Auto line numbering |
| `DELETE` | ✗ | Delete lines of a BASIC program |
| `LIST` | ✓ | List the BASIC program lines (LIST, LIST n, LIST n-, LIST -n, LIST n-m) |
| `NEW` | ✓ | Erase program and clear all variables |
| `RENUMBER` | ✗ | Renumber lines of the BASIC program |

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

## System Management (0/1 - 0%)

| Command | Status | Description |
|---------|--------|-------------|
| `KEY` | ✗ | Define/list function key assignment |

## Environment Directives (0/1 - 0%)

| Command | Status | Description |
|---------|--------|-------------|
| `EXPNOTATION` | ✗ | Setup Directives |
