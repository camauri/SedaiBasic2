# SedaiBasic - BASIC Commands

## Implementation Progress

**Commodore BASIC v7 core — 188 / 209 commands implemented (90%)**

```
[█████████████████████████████████████████████····] 90%
```

**FreeBASIC keyword set — 461 / 643 implemented (72%)** (+ 5 partial). **71** of the unimplemented
entries are **N/A** (compiler-internal `__FB_*` defines, native linkage/ABI, variadic C calling,
build/platform directives, hardware ports) — not runnable keywords for a portable bytecode VM. Of the
**572 applicable** keywords, **461 (81%)** are implemented. See the
[FreeBASIC Keyword Reference](#freebasic-keyword-reference--implementation-status) section for the full breakdown.

```
[███████████████████████·······················] 49%
```

Legend: ✓ = Implemented | ◐ = Partial | ✗ = Not implemented

## Operators (12/12 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `+` | ✓ | Add operator |
| `-` | ✓ | Subtract operator |
| `*` | ✓ | Multiply operator |
| `/` | ✓ | Divide operator (always floating-point) |
| `\` | ✓ | Integer division (FreeBASIC; truncates toward zero) |
| `^` | ✓ | Power operator |
| `MOD` | ✓ | Modulo operator |
| `SHL` | ✓ | Bit shift left (FreeBASIC; looser than +/-, tighter than comparisons) |
| `SHR` | ✓ | Bit shift right, logical (FreeBASIC) |
| `&` | ✓ | String concatenation (FreeBASIC; coerces numbers to string, looser than +/-) |
| `+= -= *= /= ^= \= &=` | ✓ | Compound assignment (FreeBASIC) |
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

## FreeBASIC (MODERN) additions

These are available in the MODERN dialect (line-number-less / `.fb`). Where a name collides with a v7
command, the v7 meaning is kept in CLASSIC (see SWAP, MID$).

| Command | Status | Description |
|---------|--------|-------------|
| `SWAP a, b` | ✓ | Exchange two lvalues (scalar / array element / UDT field) |
| `MID(...)` | ✓ | Substring function (bare, MODERN) and `MID(dst,start[,len]) = src` in-place statement |
| `STRING(n,ch)` | ✓ | N copies of a character (also `STRING$`, both dialects) |
| `IIF(cond, a, b)` | ✓ | Short-circuit conditional expression (only the taken branch is evaluated) |
| `ENUM ... END ENUM` | ✓ | Named integer constants (auto-increment; member with no value = previous + 1) |
| `DEFINT`/`DEFLNG`/`DEFBYTE`/`DEFSHORT`/`DEFLNGINT`/`DEFSNG`/`DEFDBL`/`DEFSTR` | ✓ | Default variable type by initial letter, e.g. `DEFINT I-N` |
| `&` / `&=` | ✓ | String concatenation operator and compound assignment |
| Conversions `CINT`/`CLNG`/`CSHORT`/`CBYTE`/`CDBL`/`CSNG`/`VALINT`/`OCT`/`BIN`... | ✓ | FreeBASIC type conversions (B1.3) |
| `LBOUND`/`UBOUND`/`ERASE`/`REDIM [PRESERVE]` | ✓ | FreeBASIC array operations (B1.4) |
| Math `ACOS`/`ASIN`/`ATAN2`/`FIX`/`FRAC` | ✓ | FreeBASIC math functions |
| `&H`/`&O`/`&B` literals | ✓ | Hex / octal / binary integer literals |
| `LSET`/`RSET` | ✓ | Justify a string into a buffer (QBasic `=` and FB `,` forms) |
| `EXIT`/`CONTINUE n,n` | ✓ | Multi-level loop exit/continue (`Exit For, For`) |
| `PROPERTY Type.name` | ✓ | Property getter/setter (desugars to method) |
| `OPERATOR <sym>` | ✓ | Operator overloading for UDTs (binary, direct operands) |
| `#define`/`#undef`/`#ifdef`/`#ifndef`/`#else`/`#endif`/`#include` | ✓ | Preprocessor (object-like **and** function-like macros `#define NAME(p) body`, nested expansion) |
| `NAMESPACE` | ✓ | Group decls under a name; qualified `N.member`, unqualified inside, nesting + reopening (methods of a namespaced TYPE / `USING` / `..global` pending) |
| Pointers `@x` / `T PTR` / `*p` | ✓ | Explicit pointers (int/float/string): address-of, pointer DIM, dereference read+write. NULL=0. Array-element pointers `@arr(i)`, UDT-field pointers `@obj.field` (incl. `@arr(i).field`, nested `@a.b.c`), pointer arithmetic `*(p±n)`, indexing `p[i]`/`p(i)`, passing pointers across SUB calls, multi-level `PTR PTR` (`**pp`). **UDT pointers**: `DIM p AS T PTR`, `NEW T`/`DELETE`, `@obj`, `p->field`/`p.field`, self-referential `NXT AS NODE PTR` (linked lists/trees), chained `p->nxt->val`. **BYREF-return of a BYREF param** (`min(a,b)=0`, int pointees). **Raw memory**: `Allocate`/`CAllocate`/`Reallocate`/`Deallocate` on a VM-internal byte heap, `SizeOf(T)`, `CAST`/`CPTR(type, expr)`, scaled `p[i]`/`*(p±n)`; `SADD(s)` = raw ZSTRING pointer to a string's bytes (read-only snapshot) |
| `FUNCTION f() BYREF AS T` | ✓ | BYREF function results: return a reference to a SHARED/global scalar or a BYREF parameter (the `min(a,b)=0` idiom, int pointees), read + write through it (`f()=x`) |
| `WSTRING` | ✓ | Unicode wide string (UTF-8 storage). `DIM s AS WSTRING [* n]`, params/return/UDT fields/arrays. `LEN`/`LEFT$`/`RIGHT$`/`MID$` index by codepoint; assignment/concat/PRINT shared with `STRING`. `WSTR(x)` converter. Fixed-length `* n` advisory (var-length storage) |
| Date/time | ✓ | Date serial = Double (epoch 1899-12-30). `NOW`/`TIMER`/`DATE`/`TIME` (bare), `DATESERIAL`/`TIMESERIAL`, `DATEVALUE`/`TIMEVALUE`, `YEAR`/`MONTH`/`DAY`/`HOUR`/`MINUTE`/`SECOND`/`WEEKDAY`, `MONTHNAME`/`WEEKDAYNAME`, `ISDATE`, `DATEADD`/`DATEDIFF`/`DATEPART` (intervals `yyyy q m y d w ww h n s`), `SETDATE`/`SETTIME` (VM-internal clock offset). Field functions intercepted by name so `day`/`month`/`year`/`second`… stay usable as variables |

## Variable Scope

The dialect is chosen at LOAD by content: a program that uses **line numbers is CLASSIC** (Commodore
BASIC v7); otherwise it is **MODERN** (FreeBASIC-style, `-lang fb`). A `.fb`/`.fbas` extension forces MODERN.

- **CLASSIC**: every variable is global by name (v7 semantics) — unchanged.
- **MODERN**: lexical scope. Only **explicit declarations** are scoped; implicit (never-`DIM`'d)
  variables remain global-by-name at procedure/module level (so classic-style code keeps working).
  - A plain module-level `DIM` is **not** visible inside a `SUB`/`FUNCTION`. Use `DIM SHARED` to make it
    visible (a UDT instance is shared by its handle; arrays live in global storage), or pass it as a
    parameter. A UDT/array follows the same rule as a scalar.
  - A `DIM` inside a block (`IF`/`ELSE` branch, `FOR`/`DO`/`WHILE` body, `BEGIN`/`BEND`) is **block-local**:
    it shadows an outer same-name variable for the rest of the block and is destroyed (UDT destructor
    runs) at the block end. `EXIT`/`RETURN` unwind block-local objects innermost-first before the frame.
- Not yet implemented (future work): a block-scoped `FOR` counter via `FOR i AS <type>` syntax;
  scoping of array names by block; an `OPTION` to auto-share module variables into procedures.

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
| `PRINT` | ✓ | Print statement (`?` is accepted as an abbreviation, Commodore/FreeBASIC style) |
| `PUDEF` | ✓ | Redefine PRINT USING symbols (filler, comma, decimal, dollar) |
| `USING` | ✓ | Formatted output (PRINT USING "#$######.##";value) |

## File Input/Output (3/3 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `GET#` | ✓ | C128 GET char from file; FreeBASIC `GET #n,[pos],var` / `PUT #n,[pos],var` binary record I/O of scalar int/double (string/UDT records deferred). |
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
| `MID$` | ✓ | Return substring (v7). In MODERN also `MID(...)` function and `MID(dst,start[,len]) = src` in-place statement (FreeBASIC) |
| `RIGHT$` | ✓ | Return string rightmost chars |
| `SPACE` / `SPACE$` | ✓ | String of N spaces (FreeBASIC) |
| `STRING` / `STRING$` | ✓ | N copies of a character (FreeBASIC; `STRING$` both dialects, bare `STRING` MODERN) |
| `LTRIM`/`RTRIM`/`TRIM`/`UCASE`/`LCASE`/`INSTRREV` | ✓ | FreeBASIC string functions |
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
| `SWAP` | ~ | (v7) Swap host/expansion RAM — NOT implemented. In MODERN, `SWAP a, b` exchanges two lvalues (FreeBASIC) ✓ |
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
| `ATAN2` | ✓ | Two-argument arctangent `ATAN2(y, x)` (FreeBASIC) |
| `ACOS` | ✓ | Arccosine (radians), domain [-1,1] (FreeBASIC) |
| `ASIN` | ✓ | Arcsine (radians), domain [-1,1] (FreeBASIC) |
| `FIX` | ✓ | Truncate toward zero (FreeBASIC; differs from `INT`/floor for negatives) |
| `FRAC` | ✓ | Fractional part, keeps sign (FreeBASIC) |
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

## Type Conversion Functions (FreeBASIC) (11/11 - 100%)

| Function | Status | Description |
|----------|--------|-------------|
| `CINT` | ✓ | Convert to Integer, rounding to nearest (banker's rounding) |
| `CLNG` | ✓ | Convert to Long, rounding to nearest |
| `CLNGINT` | ✓ | Convert to LongInt (64-bit), rounding to nearest |
| `CSHORT` | ✓ | Convert to Short, rounding to nearest |
| `CBYTE` | ✓ | Convert to Byte, rounding to nearest |
| `CUBYTE` | ✓ | Convert to UByte, rounding to nearest |
| `CUSHORT` | ✓ | Convert to UShort, rounding to nearest |
| `CUINT` | ✓ | Convert to UInteger, rounding to nearest |
| `CULNG` | ✓ | Convert to ULong, rounding to nearest |
| `CDBL` | ✓ | Convert to Double-precision float |
| `CSNG` | ✓ | Convert to Single-precision float |

Note: integer conversions round-to-nearest with ties-to-even (banker's rounding),
matching FreeBASIC's `CINT` family — distinct from `INT` (floor) and the implicit
truncation of a float→int assignment. Per-type range clamping/wrapping (e.g. `CBYTE`
modulo 256) is not yet applied (v1).

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

## Comments (2/2 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `REM` | ✓ | Starts a comment or remark |
| `'` | ✓ | Apostrophe line comment (FreeBASIC/QBasic style); not a string delimiter (only `"` delimits strings) |

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
> Note: a ✓ marks name recognition — exact semantics may still differ from FreeBASIC. OOP `TYPE`
> (methods/inheritance/virtual dispatch/constructors/destructors/PROPERTY/OPERATOR), threading, and a
> preprocessor (object-like **and** function-like #define, #ifdef/#include), namespaces, pointers
> (managed + raw `Allocate`/`SADD`), WString/unicode (UTF-8, codepoint-aware) and FB-syntax file I/O
> are implemented. This is a forward-looking gap map, not a claim of FreeBASIC compatibility.
>
> **Coverage (FreeBASIC keyword set):** **461 / 643 implemented (72%)**, plus 5 partial (◐).
> Of the 177 not-implemented, 71 are classified **N/A** (compiler-internal `__FB_*` defines, native
> linkage/ABI directives, variadic C ABI, hardware ports, build directives) → **453 / 572 ≈ 79% of the
> applicable keywords**.
> Highlights: structured control flow, SUB/FUNCTION, full OOP `TYPE` (methods, EXTENDS, virtual
> dispatch, CONSTRUCTOR/DESTRUCTOR, PROPERTY, OPERATOR), multithreading, value semantics/RAII,
> compound & bitwise operators, string/conversion/array functions, namespaces, pointers (managed + raw
> memory `Allocate`/`SizeOf`/`CAST`/`SADD`), WString/unicode, function-like macros, FB-syntax file I/O,
> and the wide-string helpers (WCHR/WHEX/WBIN/WOCT/WSPACE). The FreeBASIC language surface is now
> substantially complete; remaining gaps are mostly niche keywords and standard-library breadth.

## Language Documentation

### Variables and Data Types

#### Variable Declarations

| Keyword | Status | Description |
|---|---|---|
| `DIM` | ✓ | Declares a variable at the current scope. |
| `CONST` | ✓ | Declares a non-modifiable variable. |
| `SCOPE` | ✓ | Begins a new scope block. |
| `STATIC` | ✓ | Declares variables in a procedure that retain their value between calls. |
| `SHARED` | ✓ | Used with Dim allows variables to be visible throughout a module. |
| `VAR` | ✓ | Declares variables where the data type is implied from an initializer. |
| `BYREF (variables)` | ✓ | Used with Dim or Static or Var allows to declare references. (DIM BYREF done; VAR/STATIC BYREF deferred.) |

#### User Defined Types

##### Declarations

| Keyword | Status | Description |
|---|---|---|
| `ENUM...END ENUM` | ✓ | Named integer constants (auto-increment) |
| `TYPE...END TYPE` | ◐ | User defined structure (M3): scalar + nested fields, `DIM v AS T`, arrays of UDT, `v.a.b`, WITH. M4.1: instance methods `SUB/FUNCTION Type.m(...)` + `THIS` + `obj.m(args)`. M4.2: `EXTENDS`. M4.3: virtual dispatch (runtime type-id). M4.4: `CONSTRUCTOR`/`DESTRUCTOR` (overloaded by arity & type, default args, `BASE`). `PROPERTY` getter/setter, `OPERATOR` overloading. Value semantics (FreeBASIC): assignment/return copy, BYREF default params, scope/block/global RAII. Heap instances via `NEW T`/`DELETE` reachable through `T PTR` (linked lists/trees). Still deferred: explicit `VIRTUAL`/`OVERRIDE`/`ABSTRACT` |
| `CLASS...END CLASS` | ✓ | Modelled as a `TYPE` (member access control is not enforced): fields, methods, arrays, construction all behave as for a record. |
| `UNION...END UNION` | ✓ | Record whose members share storage. Overlap is faithful within a bank — members of the same type alias the same slot (write one, read another of the same type). Members in different banks (int/float/string) occupy distinct slots; cross-bank byte reinterpretation is not modelled (slot-based record model, v1). |
| `EXTENDS` | ✓ | Single inheritance `TYPE Child EXTENDS Parent`: inherited fields (prefix layout) + methods + reference polymorphism (M4.2); virtual dispatch — an overridden method is selected by the instance's runtime type even through a base-typed variable (M4.3); inherited/ chained constructors & destructors (M4.4). |
| `EXTENDS WSTRING` | ✗ | Extends an user defined type to inherits Wstring behavior |
| `EXTENDS ZSTRING` | ✗ | Extends an user defined type to inherits Zstring behavior |
| `IMPLEMENTS` | ✓ | `TYPE name [EXTENDS base] IMPLEMENTS iface[, ...]` clause accepted and ignored — interfaces are a reserved-but-unimplemented FB feature (the FB compiler itself does not implement them), so the type behaves as an ordinary UDT, matching FB. |
| `FIELD` | ✓ | `TYPE name FIELD = n` alignment header — accepted and ignored (advisory in the slot-based record model). |
| `OBJECT` | ✓ | Built-in RTTI base type. `TYPE X EXTENDS Object` gives RTTI; `X IS Object` is true for any derived instance; `DIM v AS Object` is a generic object handle. Modelled as an empty base UDT (type-id dispatch, no vtable pointer field). |

##### Referencing

| Keyword | Status | Description |
|---|---|---|
| `Temporary Types` | ✗ | Creates a temporary copy of a user defined type |
| `THIS` | ✓ | Implicit first parameter of methods/constructors/destructors (M4.1): the instance handle. `THIS.field` reads/writes fields; used to resolve the method's owner type |
| `BASE (member access)` | ✗ | Built-in, hidden, variable to access the base user defined type instance in derived user defined types |
| `Type Alias` | ✓ | `TYPE newname AS underlyingtype` — synonym for a builtin or UDT; resolved via CanonicalType (chained aliases, narrowing, and alias-to-UDT supported). |
| `WITH` | ✓ | `WITH rec` ... `END WITH`: leading `.field` resolves against the record (M3.2) |

##### Member Procedures

| Keyword | Status | Description |
|---|---|---|
| `BASE (initialization)` | ✗ | Specifies an initializer for the base user defined type in derived user defined type constructors |
| `CONSTRUCTOR` | ✓ | Member procedure auto-called when an instance is created: `DIM v AS T` / `DIM v AS T(args)` / `NEW T(args)` (nested members first, then the object); overloading by arity and by parameter type (M4.4d/g); base-constructor auto-chaining and explicit `BASE(args)` (M4.4f); inherited if the subtype has none. |
| `DESTRUCTOR` | ✓ | Member procedure auto-called when an instance goes out of scope, in reverse construction order: procedure-local DIM'd UDTs, block-scoped DIMs (per loop iteration), module globals (program end / `END` in a proc), nested members, and BYVAL-param copies (V5/V5b/V5c/V5d). |
| `FUNCTION` | ✓ | Declares or defines a member procedure returning a value |
| `OPERATOR` | ✓ | Overloaded operator `OPERATOR <sym>(a AS T, b AS T) AS R` (binary, direct operands; resolved by left operand type) |
| `OVERRIDE` | ✓ | Accepted in an in-TYPE Declare; dispatch already works via runtime type-id (M4.3). |
| `PROPERTY` | ✓ | Property getter/setter `PROPERTY Type.name() AS T` / `PROPERTY Type.name(v AS T)` (desugars to a method) |
| `SUB` | ✓ | Declare or defines a member procedure |
| `STATIC (Member)` | ✓ | Static member **methods** (`Type.method(args)`, no instance) and static member **variables** (`Static field AS type` → one shared storage per type, accessed via the type name or any instance). |
| `VIRTUAL` | ✓ | Accepted in an in-TYPE Declare; dispatch already works via runtime type-id (M4.3). |
| `ABSTRACT` | ✓ | Accepted in an in-TYPE Declare (no-body enforcement deferred). |
| `CONST (Member)` | ✓ | Member method attribute that declares or defines that the method is readonly and does not modify the user defined types's data |

##### Member Access Control

| Keyword | Status | Description |
|---|---|---|
| `PUBLIC: (Access Control)` | ✓ | Parsed inside a TYPE; access not enforced (v1). |
| `PRIVATE: (Access Control)` | ✓ | Parsed inside a TYPE; access not enforced (v1). |
| `PROTECTED: (Access Control)` | ✓ | Parsed inside a TYPE; access not enforced (v1). |

#### Standard Data Types

##### Integer types

| Keyword | Status | Description |
|---|---|---|
| `BYTE and UBYTE` | ✓ | 8-bit integer types. Stored in the Int64 bank but **assignments wrap/sign-extend to 8 bits** (B1.5): e.g. a `UBYTE` counter wraps 255→0. |
| `SHORT and USHORT` | ✓ | 16-bit integer types; assignments wrap/sign-extend to 16 bits (B1.5). |
| `LONG and ULONG` | ✓ | 32-bit integer types; assignments wrap/sign-extend to 32 bits (B1.5). |
| `INTEGER and UINTEGER` | ✓ | 64-bit integer types here (platform width). Stored as Int64; a `UINTEGER` prints as unsigned (B1.5). |
| `LONGINT and ULONGINT` | ✓ | 64-bit integer types. Stored as Int64; a `ULONGINT` prints as unsigned 0..2^64-1 (B1.5). |

##### Floating-point types

| Keyword | Status | Description |
|---|---|---|
| `SINGLE` | ✓ | Assignments round to true single precision (held in the Double bank) (B1.5). |
| `DOUBLE` | ✓ | 64-bit real type. |

##### Boolean types

| Keyword | Status | Description |
|---|---|---|
| `BOOLEAN` | ✓ | Stored as Int64 (0 = false, non-zero = true); a `BOOLEAN` variable **prints as `true`/`false`** (B1.5). |

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
| `UNSIGNED` | ✓ | `AS UNSIGNED <basetype>` modifier → maps to the unsigned variant (INTEGER→UINTEGER, BYTE→UBYTE, SHORT→USHORT, LONG→ULONG, LONGINT→ULONGINT). Bare `UNSIGNED` == UNSIGNED INTEGER. |
| `ALIAS (Modifier)` | ✗ | Modifies how a datatype is linked with other languages (Name mangling). |

##### String types

| Keyword | Status | Description |
|---|---|---|
| `STRING` | ✗ | Fixed-length and variable-length strings with built-in memory management. |
| `ZSTRING` | ✗ | Fixed-length and variable-length null-terminated strings. |
| `WSTRING` | ✓ | Wide-character strings (UTF-8 storage, codepoint-aware LEN/MID/LEFT$/RIGHT$). Fixed-length `* n` parsed but advisory (var-length storage). |

##### Class types

| Keyword | Status | Description |
|---|---|---|
| `OBJECT` | ✓ | RTTI base type (see Object above): `EXTENDS Object`, `IS Object`, `DIM v AS Object`. Empty base UDT, type-id dispatch. |

#### Converting Between Data Types

##### Generic conversions

| Keyword | Status | Description |
|---|---|---|
| `CAST and CPTR` | ✓ | `CAST(type, expr)` converts/reinterprets an expression; `CPTR(type ptr, expr)` is a pointer cast (passthrough). |

##### Conversions to integral types

| Keyword | Status | Description |
|---|---|---|
| `CBYTE and CUBYTE` | ✓ | Converts to 8-bit values: round-to-nearest (ties-to-even) then wrap/sign-extend to 8 bits (B1.3/B1.5). |
| `CSHORT and CUSHORT` | ✓ | Converts to 16-bit values with width wrap/sign-extend (B1.3/B1.5). |
| `CLNG and CULNG` | ✓ | Converts to 32-bit values with width wrap/sign-extend (B1.3/B1.5). |
| `CINT and CUINT` | ✓ | Converts to 64-bit values (platform Integer width here) (B1.3). |
| `CLNGINT and CULNGINT` | ✓ | `CLNGINT`/`CULNGINT` — round to a 64-bit signed/unsigned integer (full width). |
| `CSIGN` | ✗ | Converts a numeric expression to a signed-type value. |
| `CUNSG` | ✗ | Converts a numeric expression to an unsigned-type value. |

##### Conversions to floating-point types

| Keyword | Status | Description |
|---|---|---|
| `CSNG and CDBL` | ✓ | Converts a numeric expression (or a numeric string, via VAL) to floating-point. `CSNG` rounds to true single precision held in the Double bank (B1.3/B1.5). |

##### Conversions to/from string types

| Keyword | Status | Description |
|---|---|---|
| `STR and WSTR` | ✓ | Converts numeric expressions to their string representation (`STR$`/`WSTR`; `WSTR` yields a wide string). |
| `VAL` | ✓ | Converts a numeric string expression to a floating-point value. |
| `VALINT and VALUINT` | ✓ | Converts numeric string expressions to integer values. Parses the leading integer (B1.3; range/sign differences deferred). |
| `VALLNG and VALULNG` | ✓ | `VALLNG`/`VALULNG` — parse a leading 64-bit signed/unsigned integer from a string. |

##### Conversion to boolean types

| Keyword | Status | Description |
|---|---|---|
| `CBOOL` | ✓ | Converts to boolean: -1 if the operand is nonzero, else 0 (FreeBASIC/VM -1/0 convention). |

### Operators

#### Assignment Operators

| Keyword | Status | Description |
|---|---|---|
| `= (Assignment)` | ✓ |  |
| `&= (Concatenate and Assign)` | ✓ | desugars to `lhs = lhs & rhs` (string concat) |
| `+= (Add and Assign)` | ✓ | desugars to `lhs = lhs + rhs` (scalar/array/member) (B1.1) |
| `-= (Subtract and Assign)` | ✓ | (B1.1) |
| `*= (Multiply and Assign)` | ✓ | (B1.1) |
| `/= (Divide and Assign)` | ✓ | (B1.1) |
| `\= (Integer Divide and Assign)` | ✓ | desugars to `lhs = lhs \ rhs` |
| `^= (Exponentiate and Assign)` | ✓ | (B1.1; also fixed integer `^` which computed `a+b`) |
| `MOD= (Modulus and Assign)` | ✓ | keyword-operator compound; desugars to `lhs = lhs MOD rhs` |
| `AND= (Conjunction and Assign)` | ✓ | desugars to `lhs = lhs AND rhs` |
| `EQV= (Equivalence and Assign)` | ✓ | desugars to `lhs = lhs EQV rhs` |
| `IMP= (Implication and Assign)` | ✓ | desugars to `lhs = lhs IMP rhs` |
| `OR= (Inclusive Disjunction and Assign)` | ✓ | desugars to `lhs = lhs OR rhs` |
| `XOR= (Exclusive Disjunction and Assign)` | ✓ | desugars to `lhs = lhs XOR rhs` |
| `SHL= (Shift Left and Assign)` | ✓ | desugars to `lhs = lhs SHL rhs` |
| `SHR= (Shift Right and Assign)` | ✓ | desugars to `lhs = lhs SHR rhs` |
| `LET (Assign)` | ✓ |  |
| `LET() (Assignment)` | ✓ |  |

#### Type Cast Operators

| Keyword | Status | Description |
|---|---|---|
| `CAST (operator)` | ✓ | `CAST(type, expr)` type conversion / reinterpretation. |
| `CPTR` | ✓ | `CPTR(type ptr, expr)` pointer cast (passthrough). |

#### Arithmetic Operators

| Keyword | Status | Description |
|---|---|---|
| `+ (Add)` | ✓ |  |
| `- (Subtract)` | ✓ |  |
| `* (Multiply)` | ✓ |  |
| `/ (Divide)` | ✓ |  |
| `\ (Integer divide)` | ✓ | truncates toward zero |
| `^ (Exponentiate)` | ✓ |  |
| `MOD (Modulus)` | ✓ |  |
| `- (Negate)` | ✓ |  |
| `SHL (Shift left)` | ✓ | `a SHL b` (FreeBASIC) |
| `SHR (Shift right)` | ✓ | `a SHR b`, logical (FreeBASIC) |

#### Indexing Operators

| Keyword | Status | Description |
|---|---|---|
| `() (Array index)` | ✗ |  |
| `[] (String index)` | ✗ |  |
| `[] (Pointer index)` | ✓ | `p[i]` (and `p(i)`) ≡ `*(p + i)`, read and write |

#### String Operators

| Keyword | Status | Description |
|---|---|---|
| `+ (String concatenation)` | ✓ |  |
| `& (String concatenation with conversion)` | ✓ | `&` concatenates, coercing numeric operands to string (FreeBASIC). |
| `STRPTR (String pointer)` | ✓ | Raw pointer to the string's data (read-only byte-heap snapshot, NUL-terminated); same model as `SADD` |

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
| `EQV (Equivalence)` | ✓ | Bitwise equivalence `a EQV b = NOT (a XOR b)`. Looser than OR/XOR. |
| `IMP (Implication)` | ✓ | Bitwise implication `a IMP b = (NOT a) OR b`. Loosest binary operator. |
| `NOT (Complement)` | ✓ |  |
| `OR (Inclusive Disjunction)` | ✓ |  |
| `XOR (Exclusive Disjunction)` | ✓ |  |

#### Short Circuit Operators

| Keyword | Status | Description |
|---|---|---|
| `ANDALSO (Short Circuit Conjunction)` | ✓ | `a ANDALSO b` — short-circuit logical AND (b evaluated only if a is nonzero); result -1/0. |
| `ORELSE (Short Circuit Inclusive Disjunction)` | ✓ | `a ORELSE b` — short-circuit logical OR (b evaluated only if a is zero); result -1/0. |

#### Preprocessor Operators

| Keyword | Status | Description |
|---|---|---|
| `# (Argument stringize)` | ✓ | `#param` in a function-like macro body stringizes the argument into a string literal. |
| `## (Argument concatenation)` | ✓ | `a ## b` in a macro body pastes the surrounding tokens together. |
| `! (Escaped String Literal)` | ✓ | `!"\n\t\\\"..."` processes escape sequences (lexer). |
| `$ (Non-Escaped String Literal)` | ✓ | `$"..."` takes the body verbatim (our default for `"..."`). |

#### Pointer Operators

| Keyword | Status | Description |
|---|---|---|
| `@ (Address of)` | ✓ | Address-of a scalar, array element `@arr(i)`, or UDT field `@obj.field` (yields a packed int reference). `@sub` (procedure address) also supported |
| `* (Value of)` | ✓ | Pointer dereference, read (`x = *p`) and write (`*p = v`); supports pointer arithmetic `*(p±n)` |
| `VARPTR (Variable pointer)` | ✓ | Address of a variable (= @v). |
| `PROCPTR (Procedure pointer and vtable index)` | ✓ | Address of a procedure (= @p); vtable index form deferred. |

#### Type or Class Operators

| Keyword | Status | Description |
|---|---|---|
| `. (Member access)` | ✓ | Record field access `rec.field` (M3) |
| `-> (Pointer to member access)` | ✓ | `p->field` member access through a UDT pointer/handle (equivalent to `p.field`). |
| `IS (Run-time type information operator)` | ✓ | `obj IS Type` → -1 if obj's runtime type is `Type` or a subtype of it, else 0. Lowered at compile time to a type-id check against `Type` and all its descendants (handles polymorphic/derived correctly). |

#### Memory Operators

| Keyword | Status | Description |
|---|---|---|
| `New Expression` | ✓ | `NEW T` / `NEW T(args)` allocates a heap record (runs its constructor) and yields a `T PTR`. Outlives the allocating frame |
| `New Overload` | ✗ | Constructor overloads apply, but `operator new` is not user-overloadable |
| `Placement New` | ✗ |  |
| `Delete Statement` | ✓ | `DELETE p` runs the pointee's destructor and frees the heap record (slot recycled via a free list) |
| `Delete Overload` | ✗ |  |

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
| `CONTINUE WHILE, CONTINUE FOR and CONTINUE DO` | ✓ | Skip to the next loop iteration (innermost loop; FOR/DO forms). |
| `EXIT WHILE, EXIT FOR and EXIT DO` | ✓ | Prematurely breaks out of a loop. |

#### Procedures

##### Declaration

| Keyword | Status | Description |
|---|---|---|
| `Declare` | ✓ | Forward `DECLARE SUB|FUNCTION ...` is accepted and ignored (calls are resolved by a pre-pass over the definitions). |
| `Sub` | ✓ | Specifies a procedure that does not return an argument. |
| `Function` | ✓ | Specifies a procedure that returns an argument. |
| `Overload` | ✓ | Accepted after a procedure name and ignored (overloading by arity/signature already works for constructors; the marker is not required). |
| `Static` | ✗ | Specifies static storage for all variables and objects in the procedure body. |
| `Const (Member)` | ✓ | Specifies a const member procedure in user-defined type definitions. |
| `Static (Member)` | ✓ | Static member procedure: `Type.method(args)` called via the type name, no instance. |

##### Linkage

| Keyword | Status | Description |
|---|---|---|
| `Public` | ✓ | Accepted as a procedure/declaration prefix and ignored (linkage is not enforced). |
| `Private` | ✓ | Accepted as a procedure/declaration prefix and ignored (linkage is not enforced). |
| `Alias` | ✓ | `ALIAS "name"` accepted after a procedure name and ignored (no external linking). |
| `Export` | ✗ | N/A — native linkage / ABI directive; no native object output. |
| `Lib` | ✓ | `LIB "name"` accepted after a procedure name and ignored (no external linking). |

##### Calling conventions

| Keyword | Status | Description |
|---|---|---|
| `Stdcall` | ✓ | Accepted after a procedure name and ignored (single internal calling convention). |
| `Cdecl` | ✓ | Accepted after a procedure name and ignored (single internal calling convention). |
| `Pascal` | ✓ | Accepted after a procedure name and ignored (single internal calling convention). |
| `Fastcall` | ✓ | Accepted after a procedure name and ignored (single internal calling convention). |
| `Thiscall` | ✓ | Accepted after a procedure name and ignored (single internal calling convention). |

##### Parameter passing conventions

| Keyword | Status | Description |
|---|---|---|
| `Byref` | ◐ | Pass a parameter by reference. UDT parameters default to by-reference (handle, so the callee mutates the caller's object); explicit `BYREF` on scalars writes back to the caller's variable at every return (M13). `BYREF` function results (`min(a,b)=0`) supported. Standalone `Dim`/`Var` reference variables still deferred |
| `Byval` | ✓ | Pass a parameter by value. Explicit `BYVAL` gives a UDT parameter its own copy (mutations don't reach the caller; the copy is destructed at frame exit); scalars are by value by default. |
| `Any` | ✗ | N/A — native linkage / ABI directive; no native object output. |

##### Variadic Procedures

| Keyword | Status | Description |
|---|---|---|
| `... (Ellipsis)` | ✗ | N/A — variadic C ABI is not modelled by the register VM. |
| `VA_FIRST` | ✗ | N/A — variadic C ABI is not modelled by the register VM. |
| `VA_ARG` | ✗ | N/A — variadic C ABI is not modelled by the register VM. |
| `VA_NEXT` | ✗ | N/A — variadic C ABI is not modelled by the register VM. |

##### Automatic execution

| Keyword | Status | Description |
|---|---|---|
| `Constructor (Module)` | ✗ | Indicates a procedure is to be executed before module-level code. |
| `Destructor (Module)` | ✗ | Indicates a procedure is to be executed after module-level code. |

##### Miscellaneous

| Keyword | Status | Description |
|---|---|---|
| `Byref (function results)` | ✓ | `FUNCTION f() BYREF AS T` returns a reference to a SHARED/global scalar or a BYREF parameter (`min(a,b)=0` idiom, int pointees); read + write through `f()` |
| `Call` | ✓ | Invokes a procedure. |
| `Naked` | ✗ | N/A — native linkage / ABI directive; no native object output. |

#### Modularizing

| Keyword | Status | Description |
|---|---|---|
| `COMMON` | ✓ | `COMMON [SHARED] var` — module-shared variable, modelled as `DIM SHARED` (single-module model). |
| `DYLIBFREE` | ✗ | N/A — native dynamic linking is out of scope for the bytecode VM. |
| `DYLIBLOAD` | ✗ | N/A — native dynamic linking is out of scope for the bytecode VM. |
| `DYLIBSYMBOL` | ✗ | N/A — native dynamic linking is out of scope for the bytecode VM. |
| `EXPORT` | ✗ |  |
| `EXTERN` | ✓ | Accepted and skipped — external linkage is N/A for a single-module bytecode VM (no native linking). |
| `EXTERN...END EXTERN` | ✓ | `EXTERN "lang" ... END EXTERN` block accepted and skipped (no native linking). |
| `IMPORT` | ✓ | Accepted and skipped (no native linking). |
| `NAMESPACE` | ✓ | Group decls under a name (AST-flattened to `N.member`); nesting + reopening |
| `PRIVATE` | ✗ |  |
| `PUBLIC` | ✗ |  |
| `USING (Namespaces)` | ✓ |  |

### Other

#### Preprocessor

##### Conditional Compilation

| Keyword | Status | Description |
|---|---|---|
| `#IF` | ✓ | Conditional compilation on a constant integer expression (literals, defined(), macro values, comparisons, AND/OR/NOT, parens). |
| `#IFDEF` | ✓ | Compiles the following code block if a symbol is defined. |
| `#IFNDEF` | ✓ | Compiles the following code block if a symbol is not defined. |
| `#ELSEIF` | ✓ | `#elif <expr>` — else-if branch on a constant expression. |
| `#ELSEIFDEF` | ✓ | Else-if branch taken when a symbol is defined (`#elseifdef NAME`). |
| `#ELSEIFNDEF` | ✓ | Else-if branch taken when a symbol is not defined (`#elseifndef NAME`). |
| `#ELSE` | ✓ | Compiles the following code block if previous conditions were false. |
| `#ENDIF` | ✓ | Signifies the end of a code block. |
| `DEFINED` | ✓ | `defined(NAME)` / `defined NAME` in `#if`/`#elif` evaluates to 1 if the symbol is defined, else 0. |

##### Text Replacement

| Keyword | Status | Description |
|---|---|---|
| `#DEFINE` | ✓ | Object-like and function-like text-replacement macros: `#define NAME body` and `#define NAME(params) body`. |
| `#MACRO and #ENDMACRO` | ✓ | Multi-line text-replacement macro `#macro NAME[(params)]` ... `#endmacro`; body lines are joined with the `:` statement separator (object-like or function-like). |
| `#UNDEF` | ✓ | Undefines a symbol. |
| `# Preprocessor stringize` | ✓ | `#param` stringizes a macro argument into a string literal. |
| `## Preprocessor concatenate` | ✓ | `a ## b` pastes tokens together in a macro body. |
| `! Escaped String Literal` | ✓ | Indicates string literal immediately following must be processed for escape sequences. |
| `$ Non-Escaped String Literal` | ✓ | Indicates string literal immediately following must not be processed for escape sequences. |

##### File Directives

| Keyword | Status | Description |
|---|---|---|
| `#INCLUDE` | ✓ | Inserts text from a file. |
| `#INCLIB` | ✗ | N/A — compiler/build control directive; no separate compile/link step. |
| `#LIBPATH` | ✗ | N/A — compiler/build control directive; no separate compile/link step. |

##### Control Directives

| Keyword | Status | Description |
|---|---|---|
| `#PRAGMA` | ✗ | N/A — compiler/build control directive; no separate compile/link step. |
| `#PRAGMA RESERVE` | ✗ | N/A — compiler/build control directive; no separate compile/link step. |
| `#CMDLINE` | ✗ | N/A — compiler/build control directive; no separate compile/link step. |
| `#LANG` | ✗ | N/A — compiler/build control directive; no separate compile/link step. |
| `#PRINT` | ✓ | `#print msg` emits a macro-expanded compile-time message to stderr. |
| `#ERROR` | ✗ | Outputs a messages to standard output and stops compilation. |
| `#ASSERT` | ✗ | Stops compilation with an error message if a given condition is false. |
| `#LINE` | ✗ | N/A — compiler/build control directive; no separate compile/link step. |

##### Metacommands

| Keyword | Status | Description |
|---|---|---|
| `'$INCLUDE` | ✓ | QuickBASIC metacommand `'$INCLUDE: 'file'` — splices a file, like `#include`. |
| `'$DYNAMIC` | ✓ | Advisory metacommand, accepted and ignored (REDIM works regardless of array storage class). |
| `'$STATIC` | ✓ | Advisory metacommand, accepted and ignored. |
| `'$LANG` | ✓ | Advisory metacommand, accepted and ignored (dialect is auto-detected). |

#### Meta-statements

##### Metacommands

##### Compiler Options

##### Set Default Datatypes

| Keyword | Status | Description |
|---|---|---|
| `DEFLONGINT` | ✓ | Default LONGINT type by initial letter (int bank), like DEFINT/DEFLNG. |
| `DEFULONGINT` | ✓ | Default ULONGINT type by initial letter (int bank; unsigned not distinguished). |

#### Intrinsic Defines

##### Platform Information

| Keyword | Status | Description |
|---|---|---|
| `__FB_WIN32__` | ✓ | Defined if compiling for Windows. |
| `__FB_LINUX__` | ✓ | Defined if compiling for Linux. |
| `__FB_DOS__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_CYGWIN__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_FREEBSD__` | ✓ | Defined if compiling for FreeBSD. |
| `__FB_NETBSD__` | ✓ | Defined if compiling for NetBSD. |
| `__FB_OPENBSD__` | ✓ | Defined if compiling for OpenBSD. |
| `__FB_DARWIN__` | ✓ | Defined if compiling for Darwin. |
| `__FB_XBOX__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_BIGENDIAN__` | ✓ | Defined if compiling on a system using big-endian byte-order. |
| `__FB_PCOS__` | ✓ | Defined if compiling for a common PC OS (e.g. DOS, Windows, OS/2). |
| `__FB_UNIX__` | ✓ | Defined if compiling for a Unix-like OS. |
| `__FB_64BIT__` | ✓ | Defined if compiling for a 64bit target. |
| `__FB_ARM__` | ✓ | Defined if compiling for the ARM architecture. |
| `__FB_PPC__` | ✓ | Defined if compiling for the PowerPC architecture. |
| `__FB_X86__` | ✓ | Defined if compiling for the X86 / X86_64 architecture. |
| `__FB_JS__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_ANDROID__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |

##### Version Information

| Keyword | Status | Description |
|---|---|---|
| `__FB_VERSION__` | ✓ | Defined as a string literal of the compiler version. |
| `__FB_VER_MAJOR__` | ✓ | Defined as an integral literal of the compiler major version number. |
| `__FB_VER_MINOR__` | ✓ | Defined as an integral literal of the compiler minor version number. |
| `__FB_VER_PATCH__` | ✓ | Defined as an integral literal of the compiler patch number. |
| `__FB_MIN_VERSION__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_BUILD_DATE__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_BUILD_DATE_ISO__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_SIGNATURE__` | ✓ | Defined as a string literal of the compiler signature. |
| `__FB_BUILD_SHA1__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_BUILD_FORK_ID__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |

##### Command-line switches

| Keyword | Status | Description |
|---|---|---|
| `__FB_ASM__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_BACKEND__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_GCC__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_OPTIMIZE__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_GUI__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_MAIN__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_DEBUG__` | ✓ | True (-1) if the "-g" switch was used, false (0) otherwise. |
| `__FB_ERR__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_FPMODE__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_FPU__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_LANG__` | ✓ | Defined to a string literal of the "-lang" dialect used. |
| `__FB_MT__` | ✓ | True (-1) if the "-mt" switch was used, false (0) otherwise. |
| `__FB_OUT_DLL__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_OUT_EXE__` | ✓ | True (-1) in a module being compiled and linked into an executable, false (0) otherwise. |
| `__FB_OUT_LIB__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_OUT_OBJ__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_PROFILE__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_SSE__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_VECTORIZE__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |

##### Environment Information

| Keyword | Status | Description |
|---|---|---|
| `__FB_ARGC__` | ✗ | Defined as an integer literal of the number of command-line arguments passed to the program. |
| `__FB_ARGV__` | ✗ | Defined as a Zstring Ptr Ptr to the command line arguments passed to the program. |
| `__DATE__` | ✓ | String literal of the compilation date in "mm-dd-yyyy" format (captured at compile time). |
| `__DATE_ISO__` | ✓ | String literal of the compilation date in "yyyy-mm-dd" format. |
| `__TIME__` | ✓ | String literal of the compilation time in "hh:mm:ss" format. |
| `__PATH__` | ✓ | String literal of the absolute path of the module directory. |

##### Context-specific Information

| Keyword | Status | Description |
|---|---|---|
| `__FILE__ and __FILE_NQ__` | ◐ | `__FILE__` expands to the top-level source file name (string literal); `__FILE_NQ__` (no quotes) not implemented. |
| `__FUNCTION__ and __FUNCTION_NQ__` | ✗ | Defined as the name of the procedure where it's used. |
| `__LINE__` | ✓ | Expands to the current source line number (1-based). |
| `__FB_OPTION_BYVAL__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_OPTION_DYNAMIC__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_OPTION_ESCAPE__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_OPTION_GOSUB__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_OPTION_EXPLICIT__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_OPTION_PRIVATE__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_OPTION_PROFILE__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |

##### Basic-macros

| Keyword | Status | Description |
|---|---|---|
| `__FB_ARG_COUNT__` | ✗ | N/A — compiler metaprogramming macro; not modelled (would require a full preprocessor token engine). |
| `__FB_ARG_EXTRACT__` | ✗ | N/A — compiler metaprogramming macro; not modelled (would require a full preprocessor token engine). |
| `__FB_ARG_LEFTOF__` | ✗ | N/A — compiler metaprogramming macro; not modelled (would require a full preprocessor token engine). |
| `__FB_ARG_LISTEXPAND__` | ✗ | N/A — compiler metaprogramming macro; not modelled (would require a full preprocessor token engine). |
| `__FB_ARG_RIGHTOF__` | ✗ | N/A — compiler metaprogramming macro; not modelled (would require a full preprocessor token engine). |
| `__FB_EVAL__` | ✗ | N/A — compiler metaprogramming macro; not modelled (would require a full preprocessor token engine). |
| `__FB_IIF__` | ✗ | N/A — compiler metaprogramming macro; not modelled (would require a full preprocessor token engine). |
| `__FB_JOIN__` | ✗ | N/A — compiler metaprogramming macro; not modelled (would require a full preprocessor token engine). |
| `__FB_QUERY_SYMBOL__` | ✗ | N/A — compiler metaprogramming macro; not modelled (would require a full preprocessor token engine). |
| `__FB_QUOTE__` | ✗ | N/A — compiler metaprogramming macro; not modelled (would require a full preprocessor token engine). |
| `__FB_UNIQUEID__` | ✗ | N/A — compiler metaprogramming macro; not modelled (would require a full preprocessor token engine). |
| `__FB_UNIQUEID_POP__` | ✗ | N/A — compiler metaprogramming macro; not modelled (would require a full preprocessor token engine). |
| `__FB_UNIQUEID_PUSH__` | ✗ | N/A — compiler metaprogramming macro; not modelled (would require a full preprocessor token engine). |
| `__FB_UNQUOTE__` | ✗ | N/A — compiler metaprogramming macro; not modelled (would require a full preprocessor token engine). |

##### Constants

| Keyword | Status | Description |
|---|---|---|
| `FALSE and TRUE` | ✓ | Boolean constants (MODERN): `TRUE` = -1, `FALSE` = 0. |

#### Error Handling

| Keyword | Status | Description |
|---|---|---|
| `Err` | ✓ | Error number of the last error (alias of `ER`/`ERR`). |

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
| `__FB_ERR__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |

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
| `INP` | ✗ | N/A — hardware port I/O is not available in a portable bytecode VM. |
| `LPRINT` | ✗ | N/A — direct printer/LPT access is not available in a portable bytecode VM. |
| `LPOS` | ✗ | N/A — printer head position; no printer device. |
| `OUT` | ✗ | N/A — hardware port I/O is not available in a portable bytecode VM. |
| `WAIT` | ✓ |  |

##### Operating System

| Keyword | Status | Description |
|---|---|---|
| `BEEP` | ✓ | Console bell — emits CHR(7) (no newline). |
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
| `IIF` | ✓ | Short-circuit conditional expression `IIF(cond, a, b)` |
| `LOOP` | ✓ |  |
| `NEXT` | ✓ |  |
| `THEN` | ✓ |  |
| `UNTIL` | ✓ |  |
| `WEND (or 'END WHILE')` | ✓ | Closes a `WHILE...WEND` loop (desugared to `DO WHILE...LOOP`). `END WHILE` form: see note. |
| `WHILE` | ✓ |  |

##### Uncategorized

| Keyword | Status | Description |
|---|---|---|
| `END (Block)` | ✓ |  |
| `OFFSETOF` | ✗ |  |
| `SIZEOF` | ✓ | `SizeOf(scalar-type / UDT)` byte size; `Allocate(n * SizeOf(T))`. Also `CAST`/`CPTR(type, expr)`. |
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
| `'$DYNAMIC` | ✓ | Advisory metacommand, accepted and ignored (REDIM works regardless of array storage class). |
| `OPTION STATIC` | ✓ | Reverts a previous OPTION DYNAMIC command. |
| `'$STATIC` | ✓ | Advisory metacommand, accepted and ignored. |
| `DIM` | ✓ | Defines any type of array. |
| `REDIM` | ✓ | Resizes an array: `REDIM [PRESERVE] arr(ub [, ub ...])` (B1.4) — single or multi-dimensional. The array must be DIM'd first; each dimension's lower bound is kept. A multi-dim REDIM'd array computes its element strides at runtime. |
| `PRESERVE` | ✓ | Preserves the overlapping array contents when used with `REDIM` (B1.4). |

#### Clearing Array Data

| Keyword | Status | Description |
|---|---|---|
| `ERASE` | ✓ | `ERASE arr [, arr ...]` resets every element to its default (0 / 0.0 / "") keeping the current size (B1.4). |

#### Retrieving Array Size

| Keyword | Status | Description |
|---|---|---|
| `ARRAYLEN` | ✓ | Total element count = product over dims of `(ubound-lbound+1)`; computed at runtime, correct for fixed, `lb TO ub`, multi-dim, and REDIM'd arrays. |
| `ARRAYSIZE` | ✗ | Returns the total array size (in bytes). |
| `LBOUND` | ✓ | Returns the lower bound of an array's dimension. `LBOUND(arr[, dim])` (B1.4). |
| `UBOUND` | ✓ | Returns the upper bound of an array's dimension. `UBOUND(arr[, dim])` (B1.4; honors explicit `lb TO ub` and dynamic sizes). |

#### Retrieving Array Descriptor

| Keyword | Status | Description |
|---|---|---|
| `Array[Const]DescriptorPtr` | ✗ | N/A — internal array descriptor pointer; not exposed. |

### Bit Manipulation

| Keyword | Status | Description |
|---|---|---|
| `Uinteger` | ✗ |  |

#### Byte Manipulation Macros

| Keyword | Status | Description |
|---|---|---|
| `LOBYTE` | ✓ | Low byte: `x AND &HFF`. |
| `HIBYTE` | ✓ | Second byte: `(x SHR 8) AND &HFF`. |
| `LOWORD` | ✓ | Low word: `x AND &HFFFF`. |
| `HIWORD` | ✓ | Second word: `(x SHR 16) AND &HFFFF`. |

#### Bit Manipulation Macros

| Keyword | Status | Description |
|---|---|---|
| `BIT` | ✓ | Bit `b` of `x`: `(x SHR b) AND 1`. |
| `BITRESET` | ✓ | `x` with bit `b` cleared: `x AND NOT (1 SHL b)`. |
| `BITSET` | ✓ | `x` with bit `b` set: `x OR (1 SHL b)`. |

### Console Functions

#### Configuring the Console

| Keyword | Status | Description |
|---|---|---|
| `CLS` | ✓ | Clears the screen and homes the cursor (alias of `SCNCLR`; resets `POS`/`CSRLIN` to 0). |
| `WIDTH` | ✓ | Sets or returns the number of rows and columns of the console display. |
| `VIEW PRINT` | ✗ | Sets the printable area of the console screen. (Deferred: needs a scroll-region console.) |

#### Cursor Color and Positioning

| Keyword | Status | Description |
|---|---|---|
| `COLOR` | ✓ | Changes the foreground and background color of text to be written. |
| `CSRLIN` | ✓ | Returns the row position of the cursor (VM-tracked, parallels `POS`). |
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
| `WRITE` | ◐ | `WRITE #n, ...` quoted-CSV file output (strings double-quoted, comma-separated). Console WRITE deferred. |
| `SPC` | ✓ | Skips a number of spaces when writing text. |
| `TAB` | ✓ | Skips to a certain column when writing text. |

### Date and Time Functions

#### VisualBasic compatible procedures

| Keyword | Status | Description |
|---|---|---|
| `NOW` | ✓ | Date serial (Double, epoch 1899-12-30) of the current date and time. Bare (no parens). |
| `DATESERIAL` | ✓ | `DATESERIAL(y, m, d)` -> serial, with VB-style month/day rollover. |
| `TIMESERIAL` | ✓ | `TIMESERIAL(h, m, s)` -> serial fraction. |
| `DATEVALUE` | ✓ | `DATEVALUE(str)` -> date-part serial (ISO `yyyy-mm-dd`/`yyyy/mm/dd` or locale; 0 on failure). |
| `TIMEVALUE` | ✓ | `TIMEVALUE(str)` -> time-part serial. |
| `SECOND` | ✓ | `SECOND(serial)` -> 0..59. |
| `MINUTE` | ✓ | `MINUTE(serial)` -> 0..59. |
| `HOUR` | ✓ | `HOUR(serial)` -> 0..23. |
| `DAY` | ✓ | `DAY(serial)` -> 1..31. Intercepted by name so `day` stays usable as a variable. |
| `WEEKDAY` | ✓ | `WEEKDAY(serial)` -> 1=Sunday..7=Saturday. |
| `MONTH` | ✓ | `MONTH(serial)` -> 1..12. Intercepted by name (not a reserved word). |
| `YEAR` | ✓ | `YEAR(serial)` -> integer. Intercepted by name (not a reserved word). |
| `DATEPART` | ✓ | `DATEPART(interval$, serial)` -> component. Intervals: `yyyy q m y d w ww h n s`. |
| `DATEADD` | ✓ | `DATEADD(interval$, number, serial)` -> serial with `number` interval units added. |
| `DATEDIFF` | ✓ | `DATEDIFF(interval$, s1, s2)` -> integer count of intervals from s1 to s2. |
| `ISDATE` | ✓ | `ISDATE(str)` -> -1 if a valid date/time string, else 0. |
| `MONTHNAME` | ✓ | `MONTHNAME(n)` -> English month name (1..12). |
| `WEEKDAYNAME` | ✓ | `WEEKDAYNAME(n)` -> English weekday name (1=Sunday..7=Saturday). |

#### Date and time procedures

| Keyword | Status | Description |
|---|---|---|
| `DATE` | ✓ | Current system date as `"mm-dd-yyyy"`. Bare (no parens); MODERN-only keyword. |
| `TIME` | ✓ | Current system time as `"hh:mm:ss"`. Bare (no parens); MODERN-only keyword. |
| `SETDATE` | ✓ | `SETDATE str` sets the VM-internal current date (OS-safe offset, not the real system clock). |
| `SETTIME` | ✓ | `SETTIME str` sets the VM-internal current time (OS-safe offset). |
| `TIMER` | ✓ | Seconds elapsed since midnight (Double). Bare (no parens). |

### Error Handling Functions

#### Determining Errors

| Keyword | Status | Description |
|---|---|---|
| `ERL` | ✓ | Source line where the error occurred (alias of `EL`; physical source line in MODERN). |
| `ERFN` | ✗ | Gets the name of the function where the error occurred. (Deferred: needs a per-PC function map.) |
| `ERMN` | ✗ | Gets the name of the source file where the error occurred. (Deferred: needs a per-PC file map.) |
| `ERR` | ✓ | Error number of the last error that occurred (alias of `ER`). |
| `ERROR` | ✓ | `ERROR <n>` raises a user runtime error number n (caught by `ON ERROR`; `ERR` returns n). Intercepted by name, so `ERROR` stays usable as an identifier. |

#### Handling Errors

| Keyword | Status | Description |
|---|---|---|
| `ON ERROR` | ✓ | Sets a global error handler using a label: `ON ERROR GOTO <label>` (MODERN/FreeBASIC) or a line number (classic); `ON ERROR GOTO 0` disables. Extends the existing TRAP mechanism. |
| `ON LOCAL ERROR` | ✓ | `ON LOCAL ERROR GOTO <label>`; `LOCAL` is accepted and treated as a global handler in v1 (no per-procedure scoping). |
| `RESUME` | ✓ | Resumes at the faulting statement (`RESUME` / `RESUME 0`), or at a named label / line (`RESUME <label>`). |
| `RESUME NEXT` | ✓ | Resumes at the statement after the one that caused the error. |

### File I/O Functions

#### Opening Files or Devices

| Keyword | Status | Description |
|---|---|---|
| `FREEFILE` | ✓ | Lowest unused file number (1..15). Bare `FREEFILE` or `FREEFILE()`. |
| `OPEN` | ✓ | `OPEN "f" FOR {INPUT\|OUTPUT\|APPEND\|BINARY\|RANDOM} AS [#]n` (FreeBASIC) and legacy `OPEN #n,"f",mode$`. Works headless (CLI) and in the console. |
| `OPEN COM` | ✓ | Binds a file number to a communications port. |
| `OPEN CONS` | ✓ | Binds a file number to the standard input and output streams. |
| `OPEN ERR` | ✓ | Binds a file number to the standard input and error streams. |
| `OPEN LPT` | ✓ | Binds a file number to a printer device. |
| `OPEN PIPE` | ✓ | Binds a file number to the input and output streams of a process. |
| `OPEN SCRN` | ✓ | Binds a file number directly to the console. |
| `CLOSE` | ✓ | Unbinds a file number from a file or device. |
| `RESET` | ✓ | Unbinds all active file numbers (closes every open handle; alias of DCLEAR). |
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
| `WRITE #` | ✓ | Writes a list of values to a file as quoted CSV (strings in `"`, comma-separated). |
| `INPUT()` | ✓ | Reads a number of characters from a file or device. |
| `WINPUT()` | ✗ | Reads a number of wide characters from a file or device. |
| `LINE INPUT #` | ✓ | `LINE INPUT #n, s` reads a whole line of text (commas not split). |
| `PRINT #` | ✓ |  |
| `? # (Shortcut for 'PRINT #')` | ✗ | Writes text data to a file or device. |
| `PUT #` | ✓ | Writes arbitrary data to a file or device. |
| `GET #` | ✓ | Reads arbitrary data from a file or device. |

#### File Position and other Info

| Keyword | Status | Description |
|---|---|---|
| `LOF` | ✓ | `LOF(#n)` — length in bytes of an open file. |
| `LOC` | ✓ | `LOC(#n)` — current byte position of an open file. |
| `EOF` | ✓ | `EOF(#n)` — -1 at/after end of file, else 0. |
| `SEEK (Statement)` | ✓ | `SEEK #n, pos` sets the file position of the next read or write operation. |
| `SEEK (Function)` | ✓ | `SEEK(n)` gets the file position of the next read or write operation. |
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
| `FIX` | ✓ | Returns the integer part of a number. |
| `FRAC` | ✓ | Returns the fractional part of a number. |
| `INT` | ✓ | Returns the largest integer less than or equal to a number. |
| `SGN` | ✓ | Returns the sign of a number. |

#### Trigonometric Procedures

| Keyword | Status | Description |
|---|---|---|
| `SIN` | ✓ | Returns the sine of an angle. |
| `ASIN` | ✓ | Returns the arcsine of a number. |
| `COS` | ✓ | Returns the cosine of an angle. |
| `ACOS` | ✓ | Returns the arccosine of a number. |
| `TAN` | ✓ | Returns the tangent of an angle. |
| `ATN` | ✓ | Returns the arctangent of a number. |
| `ATAN2` | ✓ | Returns the arctangent of the ratio between two numbers. |

#### Miscellaneous Procedures

| Keyword | Status | Description |
|---|---|---|
| `RANDOMIZE` | ✓ | Seeds the RNG used by `RND`: `RANDOMIZE seed` sets a deterministic seed (same seed reproduces the same sequence); bare `RANDOMIZE` seeds from the system timer. A trailing algorithm argument is accepted and ignored. |
| `RND` | ✓ | Returns a random Double in the range [0, 1). Bare `RND` (no parentheses) is accepted, equivalent to `RND(1)`. |

### Memory Functions

#### Working with Dynamic Memory

| Keyword | Status | Description |
|---|---|---|
| `ALLOCATE` | ✓ | Reserves a number of bytes of uninitialized memory and returns the address (raw pointer into a VM-internal byte heap; `p[i]`/`*(p±n)` scale by `SizeOf(pointee)`). |
| `CALLOCATE` | ✓ | Reserves a number of bytes of initialized (zeroed) memory and returns the address. |
| `REALLOCATE` | ✓ | Changes the size of reserved memory, preserving existing contents. |
| `DEALLOCATE` | ✓ | Returns reserved memory back to the heap (free-list recycled). |

#### Miscellaneous Procedures

| Keyword | Status | Description |
|---|---|---|
| `PEEK` | ✓ | Reads some type of value from an address. |
| `POKE` | ✓ | Writes some type of value to an address. |
| `CLEAR` | ✓ | `CLEAR(dst, value, bytes)`: set a block of raw heap memory (from Allocate) to a byte value. v1 takes the pointer directly. |
| `FB_MEMCOPY` | ✓ | `FB_MEMCOPY(dst, src, bytes)`: copy a block of raw heap memory; returns dst. v1 takes pointers directly. |
| `FB_MEMCOPYCLEAR` | ✓ | `FB_MEMCOPYCLEAR(dst, dstlen, src, srclen)`: copy the first srclen bytes, clear the rest (composed from FB_MEMCOPY + CLEAR). |
| `FB_MEMMOVE` | ✓ | `FB_MEMMOVE(dst, src, bytes)`: copy a block of raw heap memory, overlap-safe; returns dst. |
| `SWAP` | ✓ | Exchange the contents of two variables. |
| `SADD` | ✓ | Returns a raw byte-heap pointer to a NUL-terminated copy (ZSTRING) of the string's bytes. Read-only snapshot — writes through the pointer do not propagate back to the managed string. |

### Operating System Functions

#### Working with Files

| Keyword | Status | Description |
|---|---|---|
| `EXEC and CHAIN` | ✗ | N/A — launching/transferring to external programs is out of scope for the sandboxed VM. |
| `RUN` | ✓ | Transfers control to another program. |
| `KILL` | ✓ | Deletes an existing file (`KILL "path"`). Raises a dialect-aware runtime error (FB code 2 = file not found) catchable by `ON ERROR`. |
| `NAME` | ✓ | Renames a file: `NAME old AS new` (intercepted by name, so `NAME` stays usable as an identifier/field). |

#### File Properties

| Keyword | Status | Description |
|---|---|---|
| `FILEATTR` | ✗ | Gets information about a file bound to a file number. |
| `FILECOPY` | ✓ | Copies a file (`FILECOPY src, dst`). |
| `FILEDATETIME` | ✗ | Gets the last modified date and time of a file. (Deferred: needs a datetime-serial function.) |
| `FILEEXISTS` | ✓ | `FILEEXISTS(path)` returns -1 if the file exists, else 0 (cross-platform). |
| `FILELEN` | ✓ | `FILELEN(path)` -> file size in bytes (0 if the file does not exist); cross-platform. |
| `FILESETEOF` | ✗ | Sets the length of an open file bound to a file number. |
| `FILEFLUSH` | ✗ | Flushes application or system buffers for an open file bound to a file number. |

#### Working with Directories

| Keyword | Status | Description |
|---|---|---|
| `CURDIR` | ✓ | `CURDIR` / `CURDIR$` (bare or parenthesised) -> the current working directory. |
| `CHDIR` | ✓ | Sets the current working directory. |
| `DIR` | ✓ | Gets the names of files or directories matching certain attributes. |
| `EXEPATH` | ✓ | `EXEPATH` (bare or `EXEPATH()`) → directory of the running program. |
| `MKDIR` | ✓ | Creates a new directory. |
| `RMDIR` | ✓ | Removes an empty directory (`RMDIR "path"`, alias `RD`). |

#### System Procedures

| Keyword | Status | Description |
|---|---|---|
| `FRE` | ✓ | Gets the amount of free memory (in bytes) available. |
| `COMMAND` | ✗ | Gets the command-line parameters passed to the program. |
| `ENVIRON` | ✓ | `ENVIRON$(name)` -> the value of an environment variable ("" if unset). |
| `ISREDIRECTED` | ✗ | Checks whether stdin or stdout is redirected to a file or not. |
| `SETENVIRON` | ✗ | Sets the value of an environment variable. |
| `SHELL` | ✗ | N/A — external command execution is out of scope for the sandboxed VM. |
| `SYSTEM` | ✗ | Closes all files and exits — equivalent to END/STOP (not yet aliased). |

### String Functions

#### Creating Strings

| Keyword | Status | Description |
|---|---|---|
| `STRING` | ✗ | Standard data type: 8 bit character string. |
| `STRING (Function)` | ✗ | Returns a String of multiple characters. |
| `ZSTRING` | ✗ | Standard data type: null terminated 8 bit character string. |
| `WSTRING` | ✓ | Standard data type: wide character string (UTF-8 storage, codepoint-aware LEN/slice). |
| `WSTRING (Function)` | ✓ | `WSTRING(n, cp)` — n copies of the wide char for Unicode codepoint cp. |
| `SPACE` | ✓ | Returns a String of N spaces. `SPACE(n)` / `SPACE$(n)` (B1.2). |
| `WSPACE` | ✓ | `WSPACE(n)` — a wide string of n spaces. |
| `LEN` | ✓ | Returns the length of a string in characters. |

#### Character Conversion

| Keyword | Status | Description |
|---|---|---|
| `ASC` | ✓ | Returns an Integer representation of an character. |
| `CHR` | ✓ | `CHR(n)` (bare FB form) routed to `CHR$`. |
| `WCHR` | ✓ | `WCHR(n)` — the wide (UTF-8) character for Unicode codepoint n (single-codepoint form). |

#### Numeric/Boolean to String Conversions

| Keyword | Status | Description |
|---|---|---|
| `BIN` | ✓ | Returns a binary String representation of an integral value. `BIN(n)`, no leading zeros (B1.3). |
| `WBIN` | ✓ | `WBIN(n)` — binary wide string of an integer. |
| `HEX` | ✓ | Returns a hexadecimal String representation of an integral value (as `HEX$`). |
| `WHEX` | ✓ | `WHEX(n)` — hexadecimal wide string of an integer. |
| `OCT` | ✓ | Returns an octal String representation of an integral value. `OCT(n)`, no leading zeros (B1.3). |
| `WOCT` | ✓ | `WOCT(n)` — octal wide string of an integer. |
| `STR` | ✓ | `STR(n)` (bare FB form) routed to `STR$`. |
| `WSTR` | ✓ | Returns the WString representation of a numeric value (or widens a string). |
| `FORMAT` | ◐ | `FORMAT(num [, mask])` / `FORMAT$` → formatted string. Numeric masks supported: `0`/`#` digit placeholders, `.`, `,` grouping, `%`, scientific `E+`/`E-`/`e+`/`e-`, literals (`\x`, `"..."`). Date/time masks (d/m/y/h/s) not yet handled. |

#### String to Numeric Conversions

| Keyword | Status | Description |
|---|---|---|
| `VAL` | ✓ | Returns the Double conversion of a numeric string. |
| `VALINT` | ✓ | Returns the Integer conversion of a numeric string. |
| `VALLNG` | ✓ | Returns the Long conversion of a numeric string. |
| `VALUINT` | ✓ | Returns the uInteger conversion of a numeric string. |
| `VALULNG` | ✗ | Returns the ULong conversion of a numeric string. |

#### Numeric Serialization

| Keyword | Status | Description |
|---|---|---|
| `MKD` | ✓ | Returns an eight character String representation of a Double. |
| `MKI` | ✓ | Returns an eight character String representation of an Integer (platform Integer = 8 bytes on x64). |
| `MKL` | ✓ | Returns a four character String representation of a Long. |
| `MKLONGINT` | ✓ | Returns an eight character String representation of a Longint. |
| `MKS` | ✓ | Returns a four character String representation of a Single. |
| `MKSHORT` | ✓ | Returns a two character String representation of a Short. |
| `CVD` | ✓ | Returns a Double representation of an eight character String. |
| `CVI` | ✓ | Returns an Integer representation of an eight character String (platform Integer = 8 bytes on x64). |
| `CVL` | ✓ | Returns a Long representation of a four character String. |
| `CVLONGINT` | ✓ | Returns a Longint representation of an eight character String. |
| `CVS` | ✓ | Returns a Single representation of a four character String. |
| `CVSHORT` | ✓ | Returns a Short representation of a two character String. |

#### Working with Substrings

| Keyword | Status | Description |
|---|---|---|
| `LEFT` | ✓ | `LEFT(s,n)` (bare FB form) routed to `LEFT$`. |
| `MID (Function)` | ✓ | Returns a substring of a string. |
| `RIGHT` | ✓ | `RIGHT(s,n)` (bare FB form) routed to `RIGHT$`. |
| `LCASE` | ✓ | Returns a copy of a string converted to lowercase. `LCASE(s)` / `LCASE$(s)` (B1.2). |
| `UCASE` | ✓ | Returns a copy of a string converted to uppercase. `UCASE(s)` / `UCASE$(s)` (B1.2). |
| `LTRIM` | ✓ | `LTRIM(s)` / `LTRIM(s, set)` substring / `LTRIM(s, Any set)` character-set. |
| `RTRIM` | ✓ | `RTRIM(s)` / `RTRIM(s, set)` substring / `RTRIM(s, Any set)` character-set. |
| `TRIM` | ✓ | `TRIM(s)` / `TRIM(s, set)` substring / `TRIM(s, Any set)` character-set. |
| `INSTR` | ✓ | Returns the first occurrence of a substring or character within a string. |
| `INSTRREV` | ✓ | Position of the last occurrence. `INSTRREV(str, sub [, start])` and `INSTRREV(str, Any set [, start])`. |
| `MID (Statement)` | ✓ | Copies a substring to a substring of a string. |
| `LSET` | ✓ | Left-justifies a string into a buffer (string lvalues; QBasic `=` and FreeBASIC `,` forms). |
| `RSET` | ✓ | Right-justifies a string into a buffer (string lvalues; QBasic `=` and FreeBASIC `,` forms). |

### Threading Support Functions

#### Threads

| Keyword | Status | Description |
|---|---|---|
| `THREADCALL` | ✓ | Starts a procedure with parameters in a separate thread of execution. `h = THREADCALL sub(a, b, ...)` — typed, multi-argument (int/float/string), like a normal call (M5.5). |
| `THREADCREATE` | ✓ | Starts a procedure in a separate thread of execution. `h = THREADCREATE(@sub [, param])` (M5.2; one param, any type; workers share global arrays + arrays of UDT). |
| `THREADWAIT` | ✓ | Waits for a thread to finish and releases the thread handle. `THREADWAIT h` (M5.2). |
| `THREADDETACH` | ✓ | Releases a thread handle without waiting for the thread to finish. `THREADDETACH h` (M5.5; v1: cleaned up at program end). |
| `THREADSELF` | ✓ | Returns the thread handle of the current thread. `h = THREADSELF()` (0 on the main thread) (M5.5). |

#### Mutexes

| Keyword | Status | Description |
|---|---|---|
| `MUTEXCREATE` | ✓ | Creates a mutex. `m = MUTEXCREATE()` (M5.4; wraps TRTLCriticalSection). |
| `MUTEXLOCK` | ✓ | Acquires a lock on a mutex. `MUTEXLOCK m` (M5.4). |
| `MUTEXUNLOCK` | ✓ | Releases a lock on a mutex. `MUTEXUNLOCK m` (M5.4). |
| `MUTEXDESTROY` | ✓ | Destroys a mutex that is no longer needed. `MUTEXDESTROY m` (M5.4). |

#### Conditional Variables

| Keyword | Status | Description |
|---|---|---|
| `CONDCREATE` | ✓ | Creates a conditional variable. `c = CONDCREATE()` (M5.4). |
| `CONDWAIT` | ✓ | Pauses execution of a threaded procedure. `CONDWAIT cond, mutex` (atomically releases the mutex, waits, reacquires) (M5.4). |
| `CONDSIGNAL` | ✓ | Resumes execution of a threaded procedure waiting for a conditional. `CONDSIGNAL cond` (M5.4). |
| `CONDBROADCAST` | ✓ | Resumes all threaded procedures waiting for a conditional. `CONDBROADCAST cond` (M5.4). |
| `CONDDESTROY` | ✓ | Destroys a conditional variable that is no longer needed. `CONDDESTROY cond` (M5.4). |

### User Input Functions

#### Reading values from the keyboard buffer

| Keyword | Status | Description |
|---|---|---|
| `INPUT` | ✓ | Reads values from the keyboard buffer. |
| `LINE INPUT` | ✓ | `LINE INPUT [;][prompt;]var` reads a whole line from the console; `LINE INPUT #n, s` from a file (commas not split). |
| `INPUT()` | ✓ | Reads a number of characters from the keyboard buffer, file or device. |
| `WINPUT()` | ✗ | Reads a number of wide characters from the keyboard buffer, file or device. |

#### Reading keys from the keyboard buffer

| Keyword | Status | Description |
|---|---|---|
| `INKEY` | ✓ | Non-blocking read of the first key waiting in the keyboard buffer (`INKEY` / `INKEY$`); returns `""` if none. |
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
| `CLS` | ✓ | Clears the screen and homes the cursor (alias of `SCNCLR`). |
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
