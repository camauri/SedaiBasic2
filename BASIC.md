# SedaiBasic - BASIC Commands

## Implementation Progress

**Commodore BASIC v7 core — 202 / 210 commands implemented (96%)**

```
[████████████████████████████████████████████████··] 96%
```

**FreeBASIC compatibility — 566 / 645 of FreeBASIC's keywords (88%)**. ⚠️ Read this as a
*compatibility measure*, not a completion score: MODERN is SedaiBasic's own dialect, and this number
says how much FreeBASIC code runs here unchanged — not how much of SedaiBasic exists. **69** of the unimplemented
entries are **N/A** (compiler-internal `__FB_*` defines, native linkage/ABI, variadic C calling,
build/platform directives, FFI, and the raw-allocator operators — `New`/`Delete Overload`,
`Placement New` — which a managed record model cannot honour) — not runnable keywords for a portable
bytecode VM. Of the
**576 applicable** keywords, **563 (98%)** are implemented, and **5** are partial. See the
[FreeBASIC Keyword Reference](#freebasic-keyword-reference--implementation-status) section for the full breakdown.

> ⚠️ **This table is a hand-kept census and it drifts — in both directions.** Four ticks were withdrawn
> on 5 Aug 2026 (`DRAW STRING`, `OPEN PIPE`, `OPEN COM`, `OPEN LPT`) after the FreeBASIC-examples sweep
> stopped skipping the examples that prove them; `DRAW STRING` earned its back the same day, by being
> implemented. The authority is the project's machine-checked inventory — a keyword-recognition pass
> and a sweep that runs the FreeBASIC examples against fbc; where those disagree with this page, they
> are right. A tick here means "we believe it works", not "something checked".
>
> ⛔ **And the machine-checked inventory has its own blind spot, which this page must not inherit.**
> `kwcheck.sh` reports 567/567 = 100% — that figure counts whether the FRONT END RECOGNISES a name,
> and a keyword can be recognised and do nothing at all. `INP`, `OUT` and `WAIT` are exactly that:
> accepted, operands evaluated, no port ever touched. They are marked ✗ here on purpose, and the
> disagreement with the 100% is not an error in either — the two answer different questions.

> 📌 Rows marked **SedaiBasic extension** (`REGEXCOUNT`, `OPTION DIGITS`, the `SPR*` sprite commands…)
> are full members of the MODERN dialect, and deliberately absent from the FreeBASIC totals above.
> Not because they count for less — because that number answers a different question. It measures how
> much FreeBASIC code runs here; a keyword FreeBASIC does not have cannot make more of it run. **The
> language and the compatibility figure are two different things, and MODERN is bigger than the
> figure.**

```
[████████████████████████████████████████████████··] 97%
```

> Counts above are derived from the status tables below, which are the source of truth.

Legend: ✓ = Implemented | ◐ = Partial | ✗ = Not implemented

## Operators (16/16 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `+` | ✓ | Add operator |
| `-` | ✓ | Subtract operator |
| `*` | ✓ | Multiply operator |
| `/` | ✓ | Divide operator (always floating-point). Division by zero is dialect-aware: MODERN/FreeBASIC follows IEEE-754 (`x/0` → ±Inf, `0/0` → NaN, printed `inf`/`-inf`/`nan`); CLASSIC/Commodore raises `?DIVISION BY ZERO ERROR`. |
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
| `WAIT` | ✗ | **Not implemented.** Accepted and inert: the arguments are parsed and discarded and it returns at once. It is built on `INP`, so it cannot do more than `INP` does — see the note under *Declared divergences*. |

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
| Math `SINH`/`COSH`/`TANH`/`ASINH`/`ACOSH`/`ATANH` | ✓ | Hyperbolic functions (FPC Math unit — same IEEE-754 result as FB's CRT) |
| `&H`/`&O`/`&B` literals | ✓ | Hex / octal / binary integer literals |
| `LSET`/`RSET` | ✓ | Justify a string into a buffer (QBasic `=` and FB `,` forms) |
| `EXIT`/`CONTINUE n,n` | ✓ | Multi-level loop exit/continue (`Exit For, For`) |
| `PROPERTY Type.name` | ✓ | Property getter/setter (desugars to method) |
| `OPERATOR <sym>` | ✓ | Operator overloading for UDTs: binary operators and the `Cast` conversion; a UDT-returning operator chains (`a * (b ^ c)`) and prints via its `Cast` |
| `TYPE ... UNION ... END UNION` | ✓ | Anonymous union nested in a TYPE (v1: members flattened as non-overlapping fields) |
| `type<T>(args)` / `T(args)` / `= (a,b,c)` | ✓ | Anonymous UDT temporary with an explicit type; aggregate/tuple initialisation of a constructor-less UDT, including array-of-UDT `{(a,b), (c,d)}` |
| `OPTION BASE 1` | ✓ | Default lower bound for a bare-upper-bound array `DIM a(n)` → `a(1..n)` |
| `OPTION DIGITS n\|EXACT` | ✓ | Significant digits `PRINT` shows for a float; `EXACT`/`ALL` = every digit the value has (**SedaiBasic extension**, no FreeBASIC equivalent — see [Numeric output](#numeric-output-and-option-digits-sedaibasic-extension)) |
| `ENUM [name] AS <type>` | ✓ | ENUM with an explicit (advisory) underlying integer type |
| `LPRINT` / `LPOS(n)` | ✓ | Line-printer output (routed to stdout) / head column (always 1 — no printer) |
| `SETENVIRON` / `ENVIRON$` | ✓ | Set / read an environment variable (SETENVIRON sets a VM-internal override) |
| `SHELL cmd` | ✓ | Run a command via the platform shell (cmd.exe / /bin/sh); returns the exit code |
| `ISREDIRECTED(n)` | ✓ | Whether a standard stream is redirected (portable default 0) |
| `INP(port)` / `OUT port, value` | ✗ | **Not implemented.** Accepted and inert: `INP` always answers `-8` and `OUT` performs no write. See *Declared divergences* — whether to implement them or withdraw the keywords is an open decision. |
| `LOCK` / `UNLOCK` | ✓ | File record locking — no-op on a single-process VM |
| `#define`/`#undef`/`#ifdef`/`#ifndef`/`#else`/`#endif`/`#include` | ✓ | Preprocessor (object-like **and** function-like macros `#define NAME(p) body`, nested expansion) |
| `NAMESPACE` | ✓ | Group decls under a name; qualified `N.member`, unqualified inside, nesting + reopening. Global-scope operator: `.name` reaches the module-level name from inside a namespace, and `..name` does the same EXPLICITLY — the only form that still means the global one from inside a `With` block, where a single dot is the WITH object (methods of a namespaced TYPE / `USING` pending) |
| Pointers `@x` / `T PTR` / `*p` | ✓ | Explicit pointers (int/float/string): address-of, pointer DIM, dereference read+write. NULL=0. Array-element pointers `@arr(i)`, UDT-field pointers `@obj.field` (incl. `@arr(i).field`, nested `@a.b.c`), pointer arithmetic `*(p±n)`, indexing `p[i]`/`p(i)`, passing pointers across SUB calls, multi-level `PTR PTR` (`**pp`). **UDT pointers**: `DIM p AS T PTR`, `NEW T`/`DELETE`, `@obj`, `p->field`/`p.field`, self-referential `NXT AS NODE PTR` (linked lists/trees), chained `p->nxt->val`. **BYREF-return of a BYREF param** (`min(a,b)=0`, int pointees). **Pointer return types** (`FUNCTION f() AS T PTR` returning a pointer value). **Raw memory**: `Allocate`/`CAllocate`/`Reallocate`/`Deallocate` on a VM-internal byte heap, `SizeOf(T)`, `CAST`/`CPTR(type, expr)`, scaled `p[i]`/`*(p±n)`; `SADD(s)` = raw ZSTRING pointer to a string's bytes (read-only snapshot) |
| `FUNCTION f() BYREF AS T` | ✓ | BYREF function results: return a reference to a SHARED/global scalar or a BYREF parameter (the `min(a,b)=0` idiom, int pointees), read + write through it (`f()=x`) |
| `WSTRING` | ✓ | Unicode wide string (UTF-8 storage). `DIM s AS WSTRING [* n]`, params/return/UDT fields/arrays. `LEN`/`LEFT$`/`RIGHT$`/`MID$` index by codepoint; assignment/concat/PRINT shared with `STRING`. `WSTR(x)` converter. Fixed-length `* n` advisory (var-length storage) |
| Date/time | ✓ | Date serial = Double (epoch 1899-12-30). `NOW`/`TIMER`/`DATE`/`TIME` (bare), `DATESERIAL`/`TIMESERIAL`, `DATEVALUE`/`TIMEVALUE`, `YEAR`/`MONTH`/`DAY`/`HOUR`/`MINUTE`/`SECOND`/`WEEKDAY`, `MONTHNAME`/`WEEKDAYNAME`, `ISDATE`, `DATEADD`/`DATEDIFF`/`DATEPART` (intervals `yyyy q m y d w ww h n s`), `SETDATE`/`SETTIME` (VM-internal clock offset). Field functions intercepted by name so `day`/`month`/`year`/`second`… stay usable as variables |

## Object-orientation

Every keyword below is accepted; the third column says what it *does*, which is not the same question.
FreeBASIC source is a constraint here - a program written for fbc must behave the same way - while a
MODERN extension is under no obligation to compile under fbc. Where the two dialects differ, the
difference is stated rather than left to be discovered.

| keyword | form | status |
|---|---|---|
| `Type` … `End Type` | `Type T` / `End Type` | FB. The class. |
| `Extends` | `Type B Extends A` | FB. Single inheritance; fields of the base come first in the layout. |
| `Object` | `Type T Extends Object` | FB. The built-in RTTI base; `x Is Object` is true for any derived type. |
| `This` | `This.field`, `This.Method()` | FB. Implicit inside a method, for fields *and* calls. |
| `Base` | `Base.Method()`, `Base.field` | FB. The super call, dispatched **non**-virtually against the parent. |
| `Constructor` / `Destructor` | `Constructor T()` | FB. The base is constructed first and destroyed last; `Base(args)` chains explicitly. **Gap:** the elements of an ARRAY of UDT are constructed but never destroyed - `Dim As P x(0 To 1)` runs `P`'s constructor twice and its destructor zero times, where FreeBASIC runs both twice. |
| `Declare` | `Declare Sub F()` | FB. Methods are defined out of line (`Sub T.F()`). |
| `Virtual` | `Declare Virtual Sub F()` | FB. **Required for dynamic dispatch.** Without it a redeclaration in a child *shadows*, and the call resolves on the static type - as fbc does. |
| `Abstract` | `Declare Abstract Sub F()` | FB. No body here; implies `Virtual`. A type that inherits one and does not implement it **cannot be instantiated**. |
| `Static` | `Declare Static Sub F()`, `Static n As Integer` | FB. No implicit `This`; one storage shared by all instances. |
| `Private:` `Protected:` `Public:` | section labels in the type body | FB. **Enforced** at every member site: a field, a method, a static method, a static data member, a constructor and a destructor. `Private` reaches only the declaring type's own methods, `Protected` also its descendants; a constructor or destructor a derived type reaches implicitly is judged from that derived type, not from where the declaration stands. **Divergence:** the access level is recorded per NAME, while FreeBASIC decides it per OVERLOAD - a name declared at two levels is treated as unenforced, so `Public: Constructor(n)` beside `Private: Constructor(ByRef rhs)` lets both through. |
| `Property` | `Property T.Length()` | FB. Getter and setter forms. |
| `Operator` | `Operator T.+ (…)` | FB. Including the compound (`*=`), `Cast`, `[]`, `Let`, `@` and the `For`/`Next`/`Step` iteration forms; the access level on the declaration is enforced for all of them. **Gap:** `Operator New` / `Operator Delete` are parsed and defined but never called - `New T` uses the built-in allocator and the operator's body does not run. |
| `Implements` | `Type T Implements I1, I2` | **MODERN extension.** fbc reserves the word and never implemented it. Here it is a *checked contract*. |
| `Interface` … `End Interface` | `Interface I` / `End Interface` | **MODERN extension.** Does not exist in fbc. Sugar for a type whose every method is implicitly `Abstract` (hence `Virtual`) and which carries no fields. A type may implement several. |
| `Override` | `Declare Override Sub F()` | **MODERN extension.** Verified: an ancestor must declare that method `Virtual`. Catches the mistyped override, which would otherwise become a new method in silence. Optional - requiring it would reject FreeBASIC source. |
| `Final` | `Declare Virtual Final Sub F()` | **MODERN extension.** No descendant may redeclare it. |
| `New` / `Delete` | `New T`, `Delete p` | FB. Heap instances. |
| `Is` | `x Is T`, `x Is I` | FB, extended: also answers for an implemented **interface**. |
| `With` … `End With` | `With This.FEnv` / `.Attack = 0.001` | FB. Member access shorthand, in read and write. |

### Declared divergences from FreeBASIC

- `Interface`, `Override` and `Final` do not exist in fbc; a MODERN source using them will not compile
  there. That is the point of an extension.
- `Implements` exists in fbc as a reserved word with no effect. In MODERN it constrains: a type that
  names an interface must provide every method of it, and it *is-a* that interface for dispatch and
  for `Is`. An fbc source is unaffected, since fbc has no interfaces to name.
- **Error handling in MODERN is FreeBASIC's, never QB's.** fbc is QB-compatible only where a program
  asks for it (`-lang qb` / `fblite`), so MODERN's reference is fbc's own base dialect:
  - `Err` carries **FreeBASIC** error numbers, and `Err$` their **FreeBASIC** messages. ⛔ That is a
    DIFFERENT TABLE from the Commodore one, not a translation of it — the two share only numbers
    (FreeBASIC 5 is *Illegal resume*, Commodore 5 is `DEVICE NOT PRESENT`; FreeBASIC 2 is *File not
    found*, Commodore 2 is `FILE OPEN`). **The two tables stay separate**, including for this
    project's own extended codes 100–113, which are written out in each table's own voice rather than
    fetched from the other.
  - A **filesystem** error with no active handler sets `Err` and continues, as fbc's base dialect
    does — that is what makes the manual's inline idiom (`Open f For Input As #1 : Loop Until Err() = 0`)
    work. `Error n` with no handler still aborts, there and here.
  - ⚠️ **Declared divergence**: fbc's `Err` is *volatile* — any internal call, `Print` included,
    resets it to its own status. We do not clear `Err` on unrelated calls, so `Print "e="; Err()`
    prints `0` under fbc and the real code here. The manual's own advice ("store it in a variable as
    soon as the error handler is entered") makes the difference unobservable in code written the way
    it prescribes.
  - **Extensions** (fbc's base dialect rejects them; they exist only in `-lang fblite`/`qb` there, or
    not at all): `On Error Goto`, `Resume`, `Resume Next`, and `Err$(n)`.
  - ⚠️ File handles run 1–15 here (a Commodore-era limit in the file layer); fbc allows many more, so
    `As #90` is legal there and an error here.
- ⚠️ **The C standard library is not here, and only its I/O half is a divergence.** A program that
  includes `<crt.bi>` for MEMORY gets what it asked for: `malloc`, `calloc`, `realloc` and `free` are
  aliases of `Allocate`, `CAllocate`, `Reallocate` and `Deallocate`, byte for byte (`calloc(count,
  size)` is exactly the two-argument `CAllocate`). A procedure the program declares itself under one
  of those names still wins — the alias is a fallback, not a reservation. Everything else in `crt.bi`
  — `FILE*`, `printf`/`snprintf`, the string and formatting entry points — stays unsupported: this VM
  owns its own memory and its own file handles, and handing a BASIC program a real `FILE*` is the one
  thing the memory-safety design exists to prevent.
- ⚠️ **`Load` is a reserved word here and not in fbc**, so `Sub Load()` is refused. Every other
  Commodore word in that position was checked: `run`, `new`, `delete`, `poke`, `wait`, `close`,
  `open`, `get`, `put`, `print` and `input` are refused by fbc too (in its own words, "Duplicated
  definition"), and `list`, `save`, `verify`, `sys`, `cont` and `clr` are accepted by both. `Load` is
  the only one that is ours alone.
- ⚠️ **`LSet` / `RSet` on a string-convertible UDT is accepted in one case fbc rejects.** fbc requires
  both an `Extends ZString`/`WString` chain *and* an `Operator Cast() ByRef As Z/WString` declared on
  the type itself; we require the chain and a cast that merely *resolves*, which an ancestor may
  supply. Where fbc accepts the statement, the result is byte-identical: the destination's current
  length is preserved, the source is cut from the right when longer and padded when shorter, and the
  counts are codepoints for a wide destination.
- ⚠️ **Overload resolution does not tell `Integer` from `LongInt`, nor `UInteger` from `ULongInt`.**
  On this target all four are 64 bits and all four sign the same register bank, and no registry records
  which of the four a name was *declared* as — so given `f(ByVal x As Integer)` and
  `f(ByVal x As UInteger)`, both `f(i)` and `f(u)` answer the first declaration. Everything the bank
  and the declared width *can* separate is resolved exactly: `Byte`/`UByte`/`Short`/`UShort`/`Long`/
  `ULong`/`Single`/`Double`/`Boolean`, each **enum** type, each **pointee** type (`Integer Ptr` from
  `Double Ptr` from `T Ptr`, at any pointer depth), each by-value UDT, and `Const` against non-`Const`.
  ⛔ A pointer argument is matched by its *declared* type, so it has to be a variable or a parameter;
  an expression whose pointer type cannot be derived matches any pointer overload, and is taken only
  when exactly one fits.
  An overload whose trailing parameters carry **defaults** is reachable with fewer arguments
  (`f(0)` selecting `f(i As Integer, j As Integer = 0, k As Integer = 0)`); among the candidates the
  one needing the fewest omissions wins, and an exact bank prefix breaks a tie.
- ⚠️ **`PUT ..., Alpha`**: the blended RGB matches fbc exactly; the resulting **alpha byte** does not.
  fbc's is deterministic and fully characterised — with an explicit value it is the blend value when
  the destination's **green** exceeds the source's green and the destination's own alpha otherwise
  (threshold `srcGreen+1`, whatever the blend value); without one it is a fixed function of the two
  alphas. ⭐ The destination's red and blue move it *not at all*, which is what identifies it: the
  alpha byte shares its 32-bit lane with green (an `&hFF00FF00` mask groups A with G), so green's
  borrow lands in alpha and red's and blue's cannot. It is a corrupted channel — "is the destination
  greener than the source" is not something a blended pixel's alpha can mean — so we blend the alpha
  channel like the other three. Same position as the float double-rounding above: where fbc is
  measurably wrong we do not follow, and we declare it.
- **`POS` and `CSRLIN` count from 1** in MODERN, as FreeBASIC does (*"The topmost row is number 1"*);
  in CLASSIC they keep the Commodore numbering from 0. `Color()` before any `COLOR` statement reports
  `0` on `0`, measured.
  ⚠️ **Declared divergence**: we *track* the cursor column and fbc, on Linux, does not — measured on a
  real pty, its `POS` answers `1` however much has been printed. Ours answers the true column, which
  is what the manual describes; theirs is a missing implementation on this platform, so the manual's
  own `console/pos` example cannot agree with both.
- **`#lang "fblite"` and `Option ByVal` / `Option ByRef` are not implemented.** They parse and are
  inert. In fbc's `fblite` dialect a parameter defaults to **BYREF** and `Option ByVal` flips that
  default; MODERN is FreeBASIC's own base dialect throughout, where a parameter defaults to BYVAL, and
  it stays that way whatever `#lang` asks for. The manual's `switches/option-byval` therefore cannot
  agree with us — its own first line says *"compile with the -lang fblite compiler switch"*.
- **`ByRef ... As Any` converts; it does not type-pun — and that is a MISSING FEATURE, not a rule.**
  fbc's `Any` disables the parameter's type check, so passing a `Single` to a body declared
  `ByRef a As Integer` makes the body read that variable's BYTES as an Integer: `-15.0` prints as
  `C1700000`, the Single's bit pattern. We convert the value instead and print `FFFFFFFFFFFFFFF1`,
  which is `-15` as an Int64. The type CHECK is disabled here as it is there — the manual's
  `misc/any-param` compiles and runs without complaint — but the value differs.
  Why: in our model a parameter is a value in a TYPED BANK (int / float / string), not an address
  into bytes, and crossing banks is a conversion; there is no address in the middle, so there are no
  bytes to reinterpret. ⚠️ This is a scoping decision and not a limit of the model: the raw-backed
  @-taken-scalar machinery already exists, and a `ByRef As Any` parameter could be lowered to a raw
  address with the body reading its own declared width.
  ⛔ One caveat on the oracle, and it is only half the answer: `SizeOf(Integer)` is 8 while a `Single`
  is 4, so fbc's `C1700000` is really `00000000_C1700000` — the Single's four real bytes plus four
  bytes PAST THE END of the object that happen to read as zero. The low half is a perfectly
  well-defined type-pun and we are simply wrong about it; only the high half is an accident.
- **`__FB_UNIQUEID__`'s numbers are not part of the contract.** The identifiers it generates are
  unique and correctly nested — which is everything a program can depend on — but they start at
  `Lt_0001` where fbc starts at `Lt_0002`, its own label counter having already spent one. The
  manual's `defines/fbuniqueid` prints its values under the heading *"Compiler output example"*, and
  matching another compiler's private counter is not compatibility.
- ⛔ **`INP` / `OUT` / `WAIT` ARE NOT IMPLEMENTED.** They parse, they evaluate their operands, and
  they do nothing: `INP` always answers **-8**, `OUT` never writes, `WAIT` returns at once. The value
  is not arbitrary — it is what `fbc` itself answers where the OS denies port access, and the
  negation of its runtime error 8 (*No privileges*) — but answering it is **not the same as reading a
  port**, and this entry exists so that nobody reads the matching output as a working implementation.
  Where the OS *does* grant access (Windows with the driver `fbc` installs, Linux as root) `fbc`
  reads real hardware and we still answer -8.
  🟡 **Open decision, deliberately not taken**: implement them somehow, or withdraw the keywords so a
  program cannot silently use something inert. Two things bear on it:
  - `INP`/`OUT` are the **x86** `in`/`out` instructions. On ARM — a Raspberry Pi, an RP2040 — there is
    no separate I/O space at all: hardware is memory-mapped, so the seam there is `PEEK`/`POKE`
    through `IMemoryMapper`, which already exists and is what `job/docs/PICO_ARCHITECTURE.md` plans.
    Implementing `INP` for those targets would mean *inventing* a meaning the CPU has not.
  - the only part of this family that **is** portable is the emulated palette below, and that one is
    specified rather than guessed.
  ⛔ Also not implemented: `fbc`'s graphics library hooks `&h3C7`/`&h3C8`/`&h3C9` while a graphics
  mode is up, to emulate QB's VGA palette — those three are not hardware. The manual calls that use
  deprecated and no example in the FreeBASIC distribution uses it; the measured protocol is written
  out in `job/tests/bas/hw_ports_no_access.bas` so implementing it later needs no new measurement.
- ⚠️ **Keyboard input on the headless Linux build**: `TTerminalInput.ProcessEvents` is implemented for
  Windows only, so under `sb` on Unix no key can ever reach `INKEY` / `GETKEY` from a real terminal.
  ⭐ This is *not* observable as a divergence — fbc's `INKEY` reads the console, not stdin, so with
  input redirected both engines answer the empty string, and at EOF both answer `-1` from `GETKEY`.
  It is a platform gap found by reading the code, recorded here rather than left to be rediscovered
  as a freeze. `IInputDevice.InputExhausted` is what stops a blocking read from spinning for ever.
- ⛔ **`OPEN PIPE` is not implemented** (neither the statement nor the function form). fbc runs the
  command and binds its stdout/stdin to the handle; we refuse it at the parser. `OPEN CONS`,
  `OPEN SCRN` and `OPEN ERR` all work, with or without the `FOR` clause, and `SHELL`'s exit code
  matches fbc exactly.
- **`PALETTE`**: a MODERN (`ScreenRes`/`Screen`) screen carries **FreeBASIC's** default 256-colour
  table, and `PALETTE` with no arguments resets to *that*; a CLASSIC screen keeps the Commodore one
  and its own reset. Two tables, two dialects, never mixed — the same rule the error-code tables
  follow. The MODERN table was dumped out of `fbc` rather than transcribed.
- ⚠️ **`PAINT` with no border colour**: in MODERN an omitted border is the **fill colour itself**, so
  the flood spreads over everything that is not already that colour — a barrier of another colour is
  painted over, not respected. Measured against fbc. In **CLASSIC** it stays the Commodore rule: the
  flood covers the connected region of the *seed pixel's* colour and stops at anything else. The two
  dialects have different flood rules and they stay apart.
- **`FLIP` is a copy, not a page swap** — the manual: *"In normal graphics mode, Flip is an alias for
  PCopy and ScreenCopy"*. It copies the work page onto the visible one (or `from` onto `to`) and does
  **not** change which page is the work page. `ScreenSet`, `PCopy` and `ScreenCopy` already matched.
- ⚠️ **`DRAW`**: the turtle language matches fbc — the eight directions, the `B`/`N` prefixes, `S`
  scaling, absolute and relative `M`, `C`, `A`/`TA` rotation and `P` flood fill — with two
  differences. `POINTCOORD` reports the pen **rounded** (fbc's is fractional: `TA45 R20` reports
  `114.1421,85.85786` where we report `114,86`, the same pixel); the pen is carried at full precision
  for the length of a `DRAW` string, but the pair `POINTCOORD` and `PSET` share is the integer one the
  C hot loop writes directly, and one authoritative pen beats two that can disagree. And
  **`X <string pointer>`** (execute another command string) is not implemented: resolving a packed
  string address back to its text has a different encoding per storage class in this model.
- ⚠️ **`DRAW STRING`**: placement, colour, transparency and the 8-pixel advance are fbc's, measured.
  The **glyph shapes of the built-in font are ours** — of the 95 printable ASCII glyphs, 54 have
  pixel-identical coverage to fbc's and 41 differ. That is a font asset, not a semantics gap: adopting
  fbc's would mean transcribing their bitmap, which is a decision rather than a fix.
- **A graphics screen is always 32-bit truecolour.** `ScreenRes w, h, depth` and `Screen n` accept
  every depth the manual lists and give a truecolour surface for all of them; `ScreenInfo` reports
  `depth = 32`, `bpp = 4`, `pitch = w * 4`, which is what the framebuffer `ScreenPtr` hands out
  actually is. fbc has real 1/2/4/8-bit palette-indexed screens — and `ScreenRes w, h` with no depth
  and `Screen 13` are **8-bit** there, so the default mode of most manual examples is a palette one.
  What follows from that, measured against fbc:
  - the **width and height** of every mode agree, all 18 of them;
  - `PSET c` / `POINT` round-trip any value in either engine, so a program that only moves colour
    numbers around behaves identically;
  - an **untouched or cleared** screen reads `&hFF000000` here and `0` in an fbc palette mode;
  - `ScreenInfo`'s depth/bpp/pitch describe our surface, not the requested mode.

  ⛔ Reporting the *requested* depth while handing out a 4-byte-per-pixel buffer would be a lie the
  `ScreenPtr` idiom (`scrsize = pitch * height`) would write straight past. The honest fix is a real
  indexed surface, which is work, not a constant.

### Where the two dialects agree, and why it matters

The dynamic type of an object walks the inheritance chain during construction and destruction, so a
virtual call made from a constructor reaches the level being constructed and not the most-derived
override - C++ and FreeBASIC semantics. The same holds in destructors, mirrored. Calling a virtual
method from a constructor is therefore of limited use in either dialect, and two-phase construction
remains the clearer arrangement.

## Numeric output and `OPTION DIGITS` (SedaiBasic extension)

`PRINT` shows a `Double` with **16** significant digits and a `Single` with **7**, the same as
FreeBASIC. Those digits are **correctly rounded** from the exact binary value, once, half-to-even, as
required by **IEEE 754-2019 §5.12.2**.

> ⚠️ **This is the one place where SedaiBasic deliberately disagrees with FreeBASIC.** fbc rounds
> twice — the exact value to 17 digits, then those 17 to 16 — which differs from a single correct
> rounding on **4.75%** of doubles. `Print 1e-283` is `9.999999999999999e-284` here and `1e-283` in
> fbc; the exact value is `0.999999999999999946852…e-283`, whose 17th digit is a 4, so rounding down
> is the correct answer. Measured over 20 706 bit patterns; no example in the FreeBASIC corpus is
> affected. ⛔ This is a deliberate, measured departure — **a float difference against fbc is not a
> bug to be "fixed" here.**

### `OPTION DIGITS n` / `OPTION DIGITS EXACT`

Sets how many significant digits a float shows. Because the digits come from the exact value and are
rounded once, the **count** is a display choice while the **rounding** is not: raising it shows *more
of the same number*, never a differently-rounded one.

```basic
Print 0.1                        '  0.1                        (default, 16 digits)

Option Digits 3     : Print 0.1  '  0.1
Option Digits 17    : Print 0.1  '  0.10000000000000001        the round-trip form
Option Digits Exact : Print 0.1  '  0.1000000000000000055511151231257827021181583404541015625
```

- **`n`** — any count from 1 up. Values above what a double can hold are capped, because past that
  there is nothing left to show.
- **`EXACT`** (or **`ALL`**) — every digit the value has.

`EXACT` is not shorthand for "very many". A double's decimal expansion is **finite**: the value is
`M × 2^E`, so for `E ≥ 0` it is an integer and for `E < 0` it is `M × 5^(-E) / 10^(-E)`, which
terminates after exactly `-E` fractional digits. Nothing is truncated because there is nothing past
the end:

| value | significant digits it has |
|---|---:|
| `0.5` | 1 — prints `0.5` even at `EXACT` |
| `0.1` | 55 |
| largest finite double | 309 |
| smallest subnormal (`2^-1074`) | **751**, the widest any double gets |

**Scope and limits.** The directive is global to the program and applies to `PRINT`; it is read at
compile time, and a precompiled `.basc` does not carry it (such a program uses the defaults). It sets
the count for both `Double` and `Single` — a `Single` simply runs out of true digits sooner.

## Variable Scope

The dialect is chosen at LOAD by content: a program that uses **line numbers is CLASSIC** (Commodore
BASIC v7); otherwise it is **MODERN** (SedaiBasic's own dialect, FreeBASIC-compatible where the two
overlap; `-lang fb`). A `.fb`/`.fbas` extension forces MODERN.

- **CLASSIC**: every variable is global by name (v7 semantics) — unchanged.
- **MODERN**: lexical scope. Only **explicit declarations** are scoped; implicit (never-`DIM`'d)
  variables remain global-by-name at procedure/module level (so classic-style code keeps working).
  - A plain module-level `DIM` is **not** visible inside a `SUB`/`FUNCTION`. Use `DIM SHARED` to make it
    visible (a UDT instance is shared by its handle; arrays live in global storage), or pass it as a
    parameter. A UDT/array follows the same rule as a scalar.
  - A `DIM` inside a block (`IF`/`ELSE` branch, `FOR`/`DO`/`WHILE` body, `BEGIN`/`BEND`) is **block-local**:
    it shadows an outer same-name variable for the rest of the block and is destroyed (UDT destructor
    runs) at the block end. `EXIT`/`RETURN` unwind block-local objects innermost-first before the frame.
- A typed `FOR` counter — `FOR i AS <type> = ... TO ...` — is honoured: the counter binds in its declared
  bank (e.g. `AS Integer` keeps the loop in the integer register bank). It is not a fresh block-local
  instance per loop (it binds/reuses in the enclosing scope), but a second `FOR i AS <type>` over the same
  name reuses the same counter register rather than diverging.
- A local array whose name matches a module array is given its own per-procedure slot (it no longer aliases
  and corrupts the module array). A nested anonymous `UNION ... END UNION` inside a `TYPE` parses (v1
  flattens its members as ordinary, non-overlapping fields). `OPTION BASE 1` sets the default lower bound
  for arrays declared with a bare upper bound (`DIM a(n)` → `a(1..n)`).
- Not yet implemented (future work): scoping of array names by block; an `OPTION` to auto-share module
  variables into procedures.

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
| `GET` | ✓ | Get character (non-blocking, returns empty string if no key). The binary file form also has a FUNCTION spelling, `Get(#f, pos, target)`, which answers 0 — as `PUT` does. |
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

## DOS Commands (27/29 - 93%)

| Command | Status | Description |
|---------|--------|-------------|
| `APPEND` | ✓ | Append data to sequential file |
| `BACKUP` | ✗ | Copy disk content to another disk |
| `BLOAD` | ✓ | Load bytecode file (.basc) |
| `BOOT` | ✓ | Load and execute bytecode file (BLOAD + RUN) |
| `BSAVE` | ✓ | Save bytecode file (.basc) |
| `CATALOG` | ✓ | Display drive directory |
| `CLOSE` | ✓ | Close file (alias for DCLOSE) |
| `COLLECT` | ✓ | No-op (host filesystem needs no B-A/BAM garbage collection). |
| `CHDIR` | ✓ | Change current directory (alias: CD) |
| `CONCAT` | ✓ | Concatenate files - append source to destination |
| `COPY` | ✓ | Copy file(s) with wildcard support (alias: CP) |
| `DCLEAR` | ✓ | Clear all open channels on disk drive |
| `DCLOSE` | ✓ | Close disk file |
| `DIR` | ✓ | Display drive directory (alias for DIRECTORY) |
| `DIRECTORY` | ✓ | Display drive directory |
| `DLOAD` | ✓ | Load BASIC file |
| `DOPEN` | ✓ | Open disk file. `DOPEN#lf,"name"[,W\|R\|A]` (bare CBM mode letter or quoted); `DOPEN#lf,"name",L,reclen` opens a relative (fixed-record) file. Spaceless `DOPEN#1` form works. |
| `DSAVE` | ✓ | Save BASIC file |
| `DVERIFY` | ✓ | Verify saved BASIC file |
| `HEADER` | ✗ | Formats a diskette |
| `LOAD` | ✓ | Load program |
| `OPEN` | ✓ | Open file for input/output. FreeBASIC `OPEN "f" FOR mode AS #n`, C128 `DOPEN`, and the C64 `OPEN lf,dev,sa,"name,type,mode"` form (mode from the filename's `,W`/`,R`/`,A`; drive prefix `N:` stripped; command channel = no-op). |
| `RECORD` | ✓ | `RECORD#lf,recnum` positions a relative file to record `recnum` (1-based → byte offset `(recnum-1)*reclen`). |
| `MKDIR` | ✓ | Create directory (alias: MD) |
| `MOVE` | ✓ | Move file (alias: MV) |
| `RENAME` | ✓ | Rename file (RENAME oldname newname) |
| `SAVE` | ✓ | Save program |
| `SCRATCH` | ✓ | Delete file(s) with wildcard support |
| `VERIFY` | ✓ | Verify saved file or program |

## String Functions (14/14 - 100%)

| Function | Status | Description |
|----------|--------|-------------|
| `ASC` | ✓ | Character code; `ASC(str[, pos])` returns the code at 1-based position `pos` (FreeBASIC) |
| `CHR$` | ✓ | Character from code; `CHR(a[, b, ...])` builds a string, one char per argument (FreeBASIC) |
| `DEC` | ✓ | Convert hex number string to decimal |
| `HEX$` | ✓ | Hex number string from decimal number (4-char, 0000-FFFF) |
| `INSTR` | ✓ | 1-based position of a substring; FreeBASIC `INSTR([start,] str, substr)` — the optional start comes FIRST |
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
| `REGEXCOUNT` | ✓ | `REGEXCOUNT(subject, pattern)` → count of non-overlapping matches (**SedaiBasic extension**, no FreeBASIC equivalent) |
| `REGEXREPLACE` | ✓ | `REGEXREPLACE(subject, pattern, replacement)` → every match replaced (**SedaiBasic extension**, no FreeBASIC equivalent) |

## Regular Expressions (SedaiBasic extension)

`REGEXCOUNT` and `REGEXREPLACE` have **no FreeBASIC equivalent** — FreeBASIC ships no regular
expression support at all. They are available in **both dialects**, CLASSIC and MODERN.

```basic
Print RegexCount("abcabc", "b")                  '' 2
Print RegexReplace("a1b22c", "[0-9]+", "#")      '' a#b#c
```

The replacement is a **literal string**: there are no `$1` / `\1` back-substitutions, because
capture groups are not extracted (see below). `REGEXREPLACE` replaces **every** match, so there is
no "replace first only" form.

### Supported syntax

Perl/PCRE-style, and this is what the **language** accepts — see the note on engines below, which is
about speed and not about meaning.

| | |
|---|---|
| literals, `.` | `.` does not match a newline |
| classes | `[abc]`, ranges `[a-z]`, negation `[^...]`, and a range with an **escaped endpoint** on either side (`[\ -~]`, `[\t-\r]`) |
| class escapes | `\d` `\D` `\w` `\W` `\s` `\S` — these are **sets**, so they cannot bound a range (`[\d-x]` is an error in PCRE2 too) |
| anchors | `^` `$`, and the word boundaries `\b` `\B` |
| groups | `(...)`, non-capturing `(?:...)`, inline flags such as `(?i)` |
| quantifiers | `*` `+` `?`, counted `{n}` `{n,}` `{n,m}`, and the lazy forms `*?` `+?` `??` |
| alternation | `\|` |
| backreferences | `\1` … `\9` |
| escapes | `\n` `\r` `\t`, `\x41`, and `\.` `\*` `\|` … for the metacharacters |

**Not supported**: POSIX bracket classes (`[[:alpha:]]`) and lookaround (`(?=...)`, `(?!...)`,
`(?<=...)`). Both answer 0 matches rather than raising.

**Also answers 0**: a **lazy quantifier applied to something that can match the empty string** —
`(a*)??`, `(a|)??`. The pattern is refused before the fallback library sees it, because that library
writes outside its own loop stack on this shape and takes the whole program down with it.

⚠️ `REGEXREPLACE`'s replacement is a **literal string** even where the pattern has groups: there is
no `$1` / `\1` back-substitution in the replacement, and every match is replaced (there is no
"first only" form).

⚠️ An **empty subject answers 0** for every pattern, where PCRE2 answers 1 for a pattern that can
match nothing (`RegexCount("", "a*")` is 1 there). The reason is in the next section: the fallback
library cannot match an empty subject at all, and one uniform answer is worth more than an answer
that depends on which engine ran.

### Two engines, one meaning

SedaiBasic has its own regex engine and also carries the general-purpose library it started from.
Which one runs is chosen per pattern, automatically, and the choice is **not allowed to change the
answer** — that invariant is what the design rests on, and the two documented exceptions above (an
empty subject, and a lazy quantifier over a nullable operand) exist precisely because they were the
alternative to breaking it.

The own engine is a DFA, which is linear in the input and cannot suffer catastrophic backtracking,
but a DFA is naturally leftmost-**longest** (POSIX) where Perl and PCRE are leftmost-**first**. On
`"ab"`, `a|ab` matches `a` under Perl rules and `ab` under POSIX rules. So the fast engine
**refuses** every pattern where the two could disagree, and the library — which is Perl-compatible —
answers it instead. Constructs a DFA cannot express at all are refused for the same reason:
backreferences (not expressible by any finite automaton), lookaround, counted repetition, and the
**lazy** quantifiers, which state a *preference* between possible matches that a DFA has no way to
hold.

> This is not theoretical tidiness. Until 2026-08-03 the fast engine *accepted* `<.+?>` and answered
> it **greedily**: `RegexReplace("<a><b>", "<.+?>", "#")` returned `#` where every other regex
> implementation returns `##`. Refusing the pattern is what keeps the two engines interchangeable.

**How compatible, measured rather than claimed.** A differential harness puts every answer next to
PCRE2's, on 342 real patterns lifted from CPython's standard library and on generated ones:

| | |
|---|---|
| patterns the fast engine takes | **46%** of real patterns |
| its answers against PCRE2 | **identical**, 1200 real cases, 0 divergences |
| answers on the constructs the **library** owns | **not** always PCRE2's — see below |

⚠️ So an earlier edition of this page was wrong to say results are "PCRE-compatible in every case".
That holds for the fast engine, and it is checked. It does **not** hold for everything the fallback
answers: on lazy quantifiers and counted repetition, FPC's RegExpr and PCRE2 disagree often (roughly
half of the generated cases that use them). Those constructs are Perl-compatible in spirit and not
byte-compatible with PCRE2, and closing that gap means the fast engine growing to cover them — not a
different fallback.

⚠️ A pattern that can match the **empty string** also matches just past the last byte:
`RegexCount("aaa", "a*")` is **2**, not 1 — one match for `"aaa"` and one empty match at the end.
That is what PCRE does too.

### Engine selection

Compiled patterns are cached, so repeating the same pattern in a loop costs nothing after the first
call. The engine is chosen automatically; `REGEX_ENGINE=tregexpr` in the environment forces the
library for the whole program, which is the way to check whether a difference is the engine's fault.

## Memory Management (8/9 - 89%)

| Command/Function | Status | Description |
|------------------|--------|-------------|
| `BANK` | ✓ | Accept-and-ignore no-op (a portable VM has no C64 RAM banking; PEEK/POKE go through the memory mapper). |
| `FETCH` | ✓ | Accept-and-ignore no-op (no REU/expansion RAM to DMA from). |
| `POKE` | ✓ | Set content of memory-mapped location |
| `RREG` | ✓ | Accept-and-ignore no-op (no 6502 to read registers from; target variables keep their default 0). |
| `STASH` | ✓ | Accept-and-ignore no-op (no REU/expansion RAM to DMA to). |
| `SWAP` | ~ | (v7) Swap host/expansion RAM — NOT implemented. In MODERN, `SWAP a, b` exchanges two lvalues (FreeBASIC) ✓ |
| `FRE` | ✓ | Return RAM bytes free (FRE(0)) |
| `PEEK` | ✓ | Return content of memory-mapped location |
| `POINTER` | ✓ | `POINTER(v)` = the address of a variable (identical to `VARPTR(v)` / `@v`). |

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

## Math Functions (30/30 - 100%)

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
| `FLOOR` | ✓ | Round toward -infinity (= `INT`); returns a Double (compat extension) |
| `CEIL` | ✓ | Round toward +infinity (`-INT(-x)`); returns a Double (compat extension) |
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
| `SINH` | ✓ | Hyperbolic sine (FreeBASIC via CRT; FPC Math unit — same IEEE-754 result) |
| `COSH` | ✓ | Hyperbolic cosine |
| `TANH` | ✓ | Hyperbolic tangent |
| `ASINH` | ✓ | Inverse hyperbolic sine |
| `ACOSH` | ✓ | Inverse hyperbolic cosine, domain x ≥ 1 |
| `ATANH` | ✓ | Inverse hyperbolic tangent, domain \|x\| < 1 |
| `VAL` | ✓ | Return the numeric value of a number string |

## Type Conversion Functions (FreeBASIC) (14/14 - 100%)

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
| `CSIGN` | ✓ | Reinterpret signedness → signed value (same width; full 64-bit here) |
| `CUNSG` | ✓ | Reinterpret signedness → unsigned value (drives unsigned compare/divide/mod/print) |
| `CSTR` | ✓ | Value → string: numeric like `Str` (no leading space), string passthrough (compat extension) |

Note: integer conversions round-to-nearest with ties-to-even (banker's rounding),
matching FreeBASIC's `CINT` family — distinct from `INT` (floor) and the implicit
truncation of a float→int assignment. Per-type range clamping/wrapping (e.g. `CBYTE`
modulo 256) is not yet applied (v1).

## Reserved Variables (9/9 - 100%)

| Variable | Status | Description |
|----------|--------|-------------|
| `DS` | ✓ | Disk status code — the last file-operation error code (0 = OK). |
| `DS$` | ✓ | Disk status message line `"NN, MESSAGE,00,00"` (track/sector are 00; no physical geometry). |
| `CWD$` | ✓ | Get current working directory (read-only) |
| `DT$` | ✓ | Get current date (YYYYMMDD format, read-only) |
| `EL` | ✓ | Return last error line |
| `ER` | ✓ | Return last error code |
| `ST` | ✓ | Kernal I/O status byte — bit 6 (64) = end-of-file on the last C64-style `GET#` (`bcGetFile`); 0 otherwise. Cleared on file open. |
| `TI` | ✓ | Get time elapsed from power on (jiffies, 1/60 sec) |
| `TI$` | ✓ | Get/set 24h clock (HHMMSS format) |

## Error Handling (4/4 - 100%)

| Command/Function | Status | Description |
|------------------|--------|-------------|
| `RESUME` | ✓ | Resume execution at error line (use in TRAP handler) |
| `RESUME NEXT` | ✓ | Resume execution at next statement after error |
| `TRAP` | ✓ | Set error handler line (TRAP 0 disables) |
| `ERR$(n)` | ✓ | Return error message for error code n |

## Debug (3/3 - 100%)

| Command | Status | Description |
|---------|--------|-------------|
| `HELP` | ✓ | No-op in non-interactive execution (no editor line to highlight). |
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

## Special Input Devices (2/3 - 67%)

| Function | Status | Description |
|----------|--------|-------------|
| `JOY` | ✗ | Return joystick status |
| `PEN` | ✓ | `PEN(n)` returns 0 (no light pen device). |
| `POT` | ✓ | `POT(n)` returns 0 (no paddle device). |

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

> **Scope.** This section is a **compatibility map against FreeBASIC**, not a to-do list for
> SedaiBasic. MODERN is SedaiBasic's own dialect; FreeBASIC is the reference it grew from and the
> yardstick its interoperability is measured against, which
> matters because it means unmodified FreeBASIC programs run here. A -- below means "FreeBASIC code
> using this will not run", not "SedaiBasic is missing something it owes anyone" -- and several
> entries are marked N/A precisely because they are artefacts of being a native compiler rather than
> features a language needs. It catalogues the **complete FreeBASIC keyword
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
> **Coverage (how much FreeBASIC runs here):** of the 634 keywords in the FreeBASIC manual's index, 63
> are **not applicable** (they describe the compiler itself, its command line, or a machine this is not)
> and 4 are punctuation that only an example can speak for. Of the **567 that remain, 567 are
> implemented — 100%**.
>
> ⚠️ **Implemented is not the same as verified, and the difference is the number worth quoting.** Each
> keyword is also checked by running its own example from the FreeBASIC manual and comparing the output
> with `fbc`:
>
> | | |
> |---|---:|
> | the example matches `fbc` | **306** |
> | **no example has ever exercised it** | **197** |
> | the example does **not** match (a defect, or a divergence this project declared) | **59** |
> | the name is not recognised, but the example passes without touching it | 5 |
>
> So the honest reading is: the language surface is complete, a little over half of it is *proven*
> against the oracle, and the rest is unproven rather than known-good. Closing that gap — writing the
> missing examples, and triaging the 59 into defects and declared divergences — is the current work.
> ⛔ Some of the 59 will stay: `MONTHNAME` answers in English where a localised `fbc` answers in the
> host's language, and `FRE` reports this runtime's memory rather than fbc's. Those are choices, made
> so that a program's output does not change with the machine it runs on.
> Highlights: structured control flow, SUB/FUNCTION, full OOP `TYPE` (methods, EXTENDS, virtual
> dispatch, CONSTRUCTOR/DESTRUCTOR, PROPERTY, OPERATOR), multithreading, value semantics/RAII,
> compound & bitwise operators, string/conversion/array functions, namespaces, pointers (managed + raw
> memory `Allocate`/`SizeOf`/`CAST`/`SADD`), WString/unicode, function-like macros, FB-syntax file I/O,
> and the wide-string helpers (WCHR/WHEX/WBIN/WOCT/WSPACE). The FreeBASIC language surface is complete by
> name; what remains is proving it, keyword by keyword, against the oracle.

## Language Documentation

### Variables and Data Types

#### Variable Declarations

| Keyword | Status | Description |
|---|---|---|
| `DIM` | ✓ | Declares a variable at the current scope. Both `DIM name AS type [= init]` and the leading-AS form `DIM [SHARED] AS type name[, ...] [= init]` (type shared by every name) are supported. Array forms: fixed `DIM a(dims) AS type`, an initializer with either sign `DIM a(dims) AS type = { ... }` / `=> { ... }`, an empty variable-length array `DIM x()` (starts at `UBOUND = -1`, sized later with `REDIM`), and an ellipsis upper bound `DIM x(lb TO ...) = { ... }` / `DIM x(...) = { ... }` (size deduced from the initializer). |
| `CONST` | ✓ | Declares a non-modifiable variable. Both the untyped `CONST name = value` and the typed `CONST name AS type = value` forms are supported (immutability is not enforced). |
| `SCOPE` | ✓ | Begins a new scope block. |
| `STATIC` | ✓ | Declares local variables that retain their value between calls (initializer runs once). Both `STATIC name AS type` and the AS-first `STATIC AS type name [, ...]` orders, with the `SHARED` and `BYREF` modifiers (`STATIC SHARED BYREF AS T r = target`). |
| `SHARED` | ✓ | Used with Dim allows variables to be visible throughout a module. |
| `VAR` | ✓ | Declares variables where the data type is implied from an initializer. Takes the `SHARED` and `BYREF` modifiers in either order (`VAR SHARED v = e`, `VAR SHARED BYREF r = target`), and `BYREF` may be repeated before each name in the list. The bank is inferred from string literals, `+` concatenation, and string-returning function calls (`SPACE`, `LEFT`, `STR`, `CHR`, `UCASE`, `HEX`, …), as well as numeric expressions. |
| `BYREF (variables)` | ✓ | Used with Dim or Static or Var allows to declare references. (DIM BYREF done; VAR/STATIC BYREF deferred.) |

#### User Defined Types

##### Declarations

| Keyword | Status | Description |
|---|---|---|
| `ENUM...END ENUM` | ✓ | Named integer constants (auto-increment) |
| `TYPE...END TYPE` | ✓ | User defined structure (M3): scalar + nested fields, `DIM v AS T`, arrays of UDT, `v.a.b`, WITH. M4.1: instance methods `SUB/FUNCTION Type.m(...)` + `THIS` + `obj.m(args)`. M4.2: `EXTENDS`. M4.3: virtual dispatch (runtime type-id). M4.4: `CONSTRUCTOR`/`DESTRUCTOR` (overloaded by arity & type, default args, `BASE`). `PROPERTY` getter/setter, `OPERATOR` overloading. Value semantics (FreeBASIC): assignment/return copy, BYREF default params, scope/block/global RAII. Heap instances via `NEW T`/`DELETE` reachable through `T PTR` (linked lists/trees). `EXTENDS Object` RTTI + `IS`. Static member methods & variables. Explicit `DECLARE [VIRTUAL\|ABSTRACT\|STATIC]` and `OVERRIDE` accepted (virtual dispatch is automatic via runtime type-id). Field default values (`x AS Integer = 10`, applied on every scalar/nested instantiation, overridden by aggregate init). Fixed-size array members (`DIM data(100) AS Integer`) are auto-sized at construction; `Any` members size via `REDIM`. `OPERATOR` overloads dispatch with a non-UDT right operand (`vec * scalar`). |
| `CLASS...END CLASS` | ✓ | Modelled as a `TYPE` (member access control is not enforced): fields, methods, arrays, construction all behave as for a record. |
| `UNION...END UNION` | ✓ | Record whose members share storage. Overlap is faithful within a bank — members of the same type alias the same slot (write one, read another of the same type). Members in different banks (int/float/string) occupy distinct slots; cross-bank byte reinterpretation is not modelled (slot-based record model, v1). |
| `EXTENDS` | ✓ | Single inheritance `TYPE Child EXTENDS Parent`: inherited fields (prefix layout) + methods + reference polymorphism (M4.2); virtual dispatch — an overridden method is selected by the instance's runtime type even through a base-typed variable (M4.3); inherited/ chained constructors & destructors (M4.4). |
| `EXTENDS WSTRING` | ~ | `TYPE T EXTENDS WSTRING` parses, and a `T` declaring `OPERATOR T.CAST() AS STRING` converts through that cast in **every string context**: PRINT, `&`, assignment, DIM-initialisation, the built-in string functions (UCASE/LEFT/INSTR/…), a comparison against a string (hence `SELECT CASE`), `LSET`/`RSET`, `STRPTR`/`SADD`, and passing to a STRING parameter (by value or by reference — the callee binds a temporary, so the caller's object is not written back). `LEN(t)` reports the type's size in bytes, as FreeBASIC does for a UDT with no `OPERATOR LEN`. Still partial: without a user-declared `CAST`, the type is not implicitly a string. |
| `EXTENDS ZSTRING` | ~ | `TYPE T EXTENDS ZSTRING` parses, and a `T` declaring `OPERATOR T.CAST() AS STRING` (or `BYREF AS ZSTRING`) converts through that cast in **every string context** — see `EXTENDS WSTRING` above for the list — including `TYPE<STRING>(v)`, a `BYREF` parameter that wants a `ZSTRING PTR`, and a comparison of two such values (`SELECT CASE v` / `CASE TYPE<T>(…)`), which goes through the conversion when no `OPERATOR =` matches. A global `OPERATOR LEN (BYREF v AS T)` is honoured. Still partial: **without** a user-declared `CAST` the type is not implicitly a string. |
| `IMPLEMENTS` | ✓ | `TYPE name [EXTENDS base] IMPLEMENTS iface[, ...]` clause accepted and ignored — interfaces are a reserved-but-unimplemented FB feature (the FB compiler itself does not implement them), so the type behaves as an ordinary UDT, matching FB. |
| `FIELD` | ✓ | `TYPE name FIELD = n` alignment header — accepted and ignored (advisory in the slot-based record model). |
| `OBJECT` | ✓ | Built-in RTTI base type. `TYPE X EXTENDS Object` gives RTTI; `X IS Object` is true for any derived instance; `DIM v AS Object` is a generic object handle. Modelled as an empty base UDT (type-id dispatch, no vtable pointer field). |

##### Referencing

| Keyword | Status | Description |
|---|---|---|
| `Temporary Types` | ✓ | `Type<T>(args)` and the shorthand `Type(args)` (UDT inferred from the DIM/assignment target) build an anonymous temporary. |
| `THIS` | ✓ | Implicit first parameter of methods/constructors/destructors (M4.1): the instance handle. `THIS.field` reads/writes fields; used to resolve the method's owner type |
| `BASE (member access)` | ✓ | `base.field` reads/writes the inherited base field, and `base.method()` is a non-virtual super call to the parent type's method (SUB/FUNCTION, with args/return), inside a derived method. |
| `Type Alias` | ✓ | `TYPE newname AS underlyingtype` — synonym for a builtin or UDT; resolved via CanonicalType (chained aliases, narrowing, and alias-to-UDT supported). Also **named function-pointer types**: `TYPE X As Function(params) As R` / `TYPE X As Sub(params)` (params may be unnamed) — a var/param/return declared `As X` is an int-banked function pointer with X's signature; `f(args)` is an indirect call. |
| `WITH` | ✓ | `WITH rec` ... `END WITH`: leading `.field` resolves against the record (M3.2) |

##### Member Procedures

| Keyword | Status | Description |
|---|---|---|
| `BASE (initialization)` | ✓ | `BASE(args)` calls the base type's constructor from a derived constructor. |
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
| `INTEGER and UINTEGER` | ✓ | 64-bit integer types here (platform width). Stored as Int64; `UINTEGER` has full unsigned semantics — exact literals 0..2^64-1 and unsigned compare/`\`/`Mod`/print (vars, params, FUNCTION returns, array elements). |
| `LONGINT and ULONGINT` | ✓ | 64-bit integer types. Stored as Int64; `ULONGINT` has full unsigned semantics — exact literals 0..2^64-1 and unsigned compare/`\`/`Mod`/print (vars, params, FUNCTION returns, array elements). |

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
| `FUNCTION Pointer` | ✓ | Types that store a pointer to a function procedure. `DIM fp AS FUNCTION(...) AS R` / `AS SUB(...)`, a named `TYPE X As Function(...)` alias, funcptr params/returns, `@func` assignment, indirect call `fp(args)` (int/float/string signatures). |
| `SUB Pointer` | ✓ | Types that store a pointer to a sub procedure |

##### Data Type Modifiers

| Keyword | Status | Description |
|---|---|---|
| `CONST` | ✓ | Specifies a read only type. |
| `POINTER and PTR (Shortcut for 'POINTER')` | ✓ | Modifies types to be pointer types. |
| `UNSIGNED` | ✓ | `AS UNSIGNED <basetype>` modifier → maps to the unsigned variant (INTEGER→UINTEGER, BYTE→UBYTE, SHORT→USHORT, LONG→ULONG, LONGINT→ULONGINT). Bare `UNSIGNED` == UNSIGNED INTEGER. |
| `INTEGER<n>` / `UINTEGER<n>` | ✓ | Explicit-width integer type names: `<8>` → BYTE/UBYTE, `<16>` → SHORT/USHORT, `<32>` → LONG/ULONG, `<64>` → LONGINT/ULONGINT. Accepted in a declaration (`Dim As Integer<8> b`) and in expression position (`SizeOf(Integer<8>)`, `Cast(Integer<8>, e)`). |
| `ALIAS (Modifier)` | ✓ | `SUB f ALIAS "extname" (...)` — the external name for linking. SedaiBasic emits bytecode and does no external linking, so the alias is parsed and ignored. |

##### String types

| Keyword | Status | Description |
|---|---|---|
| `STRING` | ✓ | Variable-length strings (`DIM AS STRING`); fixed-length `STRING * n` is parsed (advisory length). |
| `ZSTRING` | ✓ | Null-terminated string type (`DIM AS ZSTRING [* n]`); `ZSTRING PTR` is a raw pointer to a string's bytes (see `SADD`). |
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
| `CSIGN` | ✓ | Reinterprets a value's signedness (signed pass-through at the source width; full 64-bit here). |
| `CUNSG` | ✓ | Reinterprets a value as unsigned; the result drives unsigned compare/divide/mod/print (`IsUnsigned64Expr`). |

##### Conversions to floating-point types

| Keyword | Status | Description |
|---|---|---|
| `CSNG and CDBL` | ✓ | Converts a numeric expression (or a numeric string, via VAL) to floating-point. `CSNG` rounds to true single precision held in the Double bank (B1.3/B1.5). |

##### Conversions to/from string types

| Keyword | Status | Description |
|---|---|---|
| `STR and WSTR` | ✓ | Converts numeric expressions to their string representation (`STR$`/`WSTR`; `WSTR` yields a wide string). |
| `VAL` | ✓ | Converts a numeric string expression to a floating-point value. Parses the leading number and stops at the first unsuitable character; honours `&H`/`&O`/`&B` base prefixes. |
| `VALINT and VALUINT` | ✓ | Converts numeric string expressions to integer values. Parses the leading integer, including `&H`/`&O`/`&B` base prefixes (B1.3; range/sign differences deferred). |
| `VALLNG and VALULNG` | ✓ | `VALLNG`/`VALULNG` — parse a leading 64-bit signed/unsigned integer from a string, including `&H`/`&O`/`&B` base prefixes. |

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
| `() (Array index)` | ✓ | `a(i [, j ...])` reads/writes an array element, honouring per-dimension lower bounds. Bounds checking is dialect-aware: MODERN/FreeBASIC does not bounds-check by default (an out-of-bounds read yields the default value, an out-of-bounds write is dropped — memory-safe); CLASSIC/Commodore raises `?BAD SUBSCRIPT`. The `--bounds-check` CLI flag forces a hard error on any out-of-bounds access (like FreeBASIC's `-exx`). |
| `[] (String index)` | ✓ | `s[i]` reads/writes the byte (character code) at 0-based index `i` of a scalar string (read = `ASC(MID$(s,i+1,1))`; write replaces that byte). |
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
| `! (Escaped String Literal)` | ✓ | `!"\n\t\\\"..."` processes escape sequences (lexer): `\a \b \f \n \l \r \t \v \\ \" \'`, `\DDD` decimal, `\xNN` hex, `\&hNN`/`\&oNNN`/`\&bNNNN`, `\uNNNN`. Every numeric escape but `\u` names one **byte**; `\u` names a codepoint and is UTF-8 encoded. |
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
| `New Expression` | ✓ | `NEW T` / `NEW T(args)` allocates a heap record (runs its constructor) and yields a `T PTR`. Outlives the allocating frame. `NEW T[n]` allocates **n** contiguous elements and `DELETE[] p` releases them: when `T` has a constructor or a destructor each element gets its own, in element order; when it has neither the block is plain bytes, so a program may lay a byte view over it or `memcopy` it. `NEW T PTR [n]` allocates an array of POINTERS, which is how a 2-dimensional object array is built — `p[i] = NEW T[m]`, then `p[i][j].field` and `DELETE[] p[i]`. |
| `New Overload` | N/A | A member `OPERATOR NEW` replaces the *allocation* step with user code returning a raw address. `NEW T` here yields a managed record handle — a slot in the VM's record table, not an address the program could have allocated — so a user allocator cannot be honoured. Constructor overloads do apply. |
| `Placement New` | N/A | `NEW(address) T` constructs an object at a caller-supplied address. Records live in the VM's managed table, not at raw addresses; the all-raw object model was evaluated and rejected because it conflicts with value semantics, RAII, virtual dispatch and threading. |
| `Delete Statement` | ✓ | `DELETE p` runs the pointee's destructor and frees the heap record (slot recycled via a free list) |
| `Delete Overload` | N/A | The counterpart of `New Overload`: a member `OPERATOR DELETE` replacing the *deallocation* step. `DELETE p` frees a managed record slot, so there is no allocation for user code to take over. Destructors do run. |

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
| `IF..END IF` | ✓ | Executes a block of statements if a condition is met. Both the multi-line block form and the single-line `IF cond THEN a [ELSE b]` are supported, including a single-line `IF..THEN..ELSE` nested as a statement inside a multi-line block. |
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
| `Static` | ✓ | `SUB|FUNCTION ... Static` procedure modifier: all local variables in the body are preserved between calls. Typed scalar locals are covered; array locals / implicitly-declared vars are a v1 limitation. |
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
| `Byref` | ✓ | Pass a parameter by reference: UDT params default to by-reference; explicit `BYREF` on scalars writes back to the caller's variable at every return (M13); `BYREF` function results (`min(a,b)=0`) supported. `DIM BYREF r AS T = target` reference variables work (batch 4). (`VAR`/`STATIC` reference-variable forms still deferred.) |
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
| `Constructor (Module)` | ✓ | `Sub name [()] Constructor [priority]` runs before module-level code (definition order); a ctor may initialise SHARED globals. What it SEES is what fbc's static initialisation puts there first: a `Dim Shared x As <scalar> = <constant>` already holds its value (and is *not* re-run afterwards, so a value the ctor writes survives), and a `Dim Shared v(0 To n)` with constant bounds is already dimensioned. A non-constant initialiser is a declared divergence — fbc refuses one — and stays where it is written. Fixed 26 Aug 2026, guard `m589`. Priority parsed but not yet ordering. |
| `Destructor (Module)` | ✓ | `Sub name [()] Destructor [priority]` runs after module-level code (reverse order), on fall-through and explicit `END`. |

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
| `EXPORT` | ✓ | `SUB f (...) EXPORT` / `FUNCTION f (...) AS T EXPORT` — publish the symbol in a shared library's export table. There is no export table in a bytecode program: parsed and ignored. |
| `EXTERN` | ✓ | Accepted and skipped — external linkage is N/A for a single-module bytecode VM (no native linking). |
| `EXTERN...END EXTERN` | ✓ | `EXTERN "lang" ... END EXTERN` block accepted and skipped (no native linking). |
| `IMPORT` | ✓ | Accepted and skipped (no native linking). |
| `NAMESPACE` | ✓ | Group decls under a name (AST-flattened to `N.member`); nesting + reopening |
| `PRIVATE` | ✓ | `PRIVATE SUB/FUNCTION` (module-private procedure); `PRIVATE:` visibility section inside a TYPE. |
| `PUBLIC` | ✓ | `PUBLIC SUB/FUNCTION` (module-public procedure); `PUBLIC:` visibility section inside a TYPE. |
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
| `#ERROR` | ✓ | `#error msg` aborts compilation with a macro-expanded diagnostic (skipped inside a false `#if`/`#ifdef` branch). |
| `#ASSERT` | ✓ | `#assert <expr>` aborts compilation if the constant integer expression is false. |
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

> **SedaiBasic extension — `__SB_WASM__`.** `-1` when compiling with `sbc --target wasm`, `0`
> otherwise, so a program can ask which machine it is being compiled *for*.
>
> It has to be answered at compile time rather than at run time, and that is the whole reason it
> exists: the WebAssembly backend refuses an opcode it does not cover because that opcode is
> **present in the program**, not because it is reached. A run-time `If` around a branch that opens
> files therefore leaves those opcodes in the module and the whole program is refused; conditional
> compilation removes them.
>
> ```basic
> #if __SB_WASM__
>     ' the parts that make no sense in a browser simply do not exist
> #else
>     If Len(Command$(1)) > 0 Then OpenTheOutputFile()
> #endif
> ```
>
> It is deliberately not spelled `__FB_something`: FreeBASIC has no WebAssembly target, so claiming
> an `__FB_` macro would claim a compatibility that does not exist. `bas/demo/voxel_landscape.bas`
> uses it to compile its offline video branch out of the browser build.

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

⛔ **A define that describes the COMPILER still decides what gets COMPILED.** Six of these used to read
"no meaning for a bytecode VM" and were left undefined, which is not neutral: a body wrapped in
`#if __FB_BACKEND__ = "gas"` was compiled by us and skipped by fbc, and we then died inside code the
oracle never builds. Each now answers what fbc 1.10.1 answers on linux-x86_64 with its own defaults —
the value is the *oracle's*, not a preference of ours, because getting it wrong is worse than leaving
it out. Fixed 26 Aug 2026, guard `m585`.

| Keyword | Status | Description |
|---|---|---|
| `__FB_ASM__` | ✓ | `"intel"` — fbc's own answer on linux-x86_64. Naming a dialect does not make inline `Asm` supported; that stays a declared gap (see divergence 51). |
| `__FB_BACKEND__` | ✓ | `"gcc"` — fbc's default backend on this host, and therefore the branch fbc itself compiles. |
| `__FB_GCC__` | ✓ | `-1` — the flag form of `__FB_BACKEND__ = "gcc"`, kept consistent with it. |
| `__FB_OPTIMIZE__` | ✓ | `0` — the optimisation level the SOURCE asked for (fbc's default; a `#cmdline` carrying `-O` raises it). It reports the REQUEST, not our pipeline, which has no `-O` ladder. |
| `__FB_GUI__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_MAIN__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_DEBUG__` | ✓ | True (-1) if the "-g" switch was used, false (0) otherwise. |
| `__FB_ERR__` | ✓ | `0` — the `-e`/`-ex`/`-exx` error-checking level, none by default, as in fbc. |
| `__FB_FPMODE__` | ✓ | `"precise"` — fbc's default `-fpmode`. |
| `__FB_FPU__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_LANG__` | ✓ | Defined to a string literal of the "-lang" dialect used. |
| `__FB_MT__` | ✓ | True (-1) if the "-mt" switch was used, false (0) otherwise. |
| `__FB_OUT_DLL__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_OUT_EXE__` | ✓ | True (-1) in a module being compiled and linked into an executable, false (0) otherwise. |
| `__FB_OUT_LIB__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_OUT_OBJ__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_PROFILE__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_SSE__` | ✗ | N/A — FreeBASIC compiler-internal define; no meaning for a bytecode VM. |
| `__FB_VECTORIZE__` | ✓ | `0` — fbc's default `-vec 0`. |

##### Environment Information

| Keyword | Status | Description |
|---|---|---|
| `__FB_ARGC__` | ✓ | The number of command-line arguments passed to the program (matches `COMMAND$` handling). |
| `__FB_ARGV__` | ~ | Defined (returns 0 — a real ZSTRING PTR PTR argument vector is not exposed; use `COMMAND$(n)`). |
| `__DATE__` | ✓ | String literal of the compilation date in "mm-dd-yyyy" format (captured at compile time). |
| `__DATE_ISO__` | ✓ | String literal of the compilation date in "yyyy-mm-dd" format. |
| `__TIME__` | ✓ | String literal of the compilation time in "hh:mm:ss" format. |
| `__PATH__` | ✓ | String literal of the absolute path of the module directory. |

##### Context-specific Information

| Keyword | Status | Description |
|---|---|---|
| `__FILE__ and __FILE_NQ__` | ✓ | `__FILE__` → top-level source file name (quoted string literal); `__FILE_NQ__` → the same name without the surrounding quotes (raw token). |
| `__FUNCTION__ and __FUNCTION_NQ__` | ✓ | The name of the enclosing procedure (uppercased), or `__FB_MAINPROC__` at module level; resolved to a compile-time string constant. Both forms yield the same string value. |
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
| `Error` | ✓ | `ERROR n` raises runtime error n (caught by `ON ERROR GOTO` / `ON LOCAL ERROR GOTO`; `ERR` holds the code). |

##### 'On [Local] Error Goto' statement use

| Keyword | Status | Description |
|---|---|---|
| `Error` | ✓ | `ERROR n` raises runtime error n (caught by `ON ERROR GOTO` / `ON LOCAL ERROR GOTO`; `ERR` holds the code). |
| `Error` | ✓ | `ERROR n` raises runtime error n (caught by `ON ERROR GOTO` / `ON LOCAL ERROR GOTO`; `ERR` holds the code). |
| `Local` | ✓ | `ON LOCAL ERROR GOTO label` installs a procedure-local error handler. |
| `Local` | ✓ | `ON LOCAL ERROR GOTO label` installs a procedure-local error handler. |
| `Local` | ✓ | `ON LOCAL ERROR GOTO label` installs a procedure-local error handler. |
| `Resume` | ✓ |  |
| `Resume Next` | ✓ |  |
| `__FB_ERR__` | ✓ | `0` — the `-e`/`-ex`/`-exx` error-checking level, none by default, as in fbc. |

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
| `ASSERT` | ✓ | `ASSERT(expr)` prints a diagnostic (function, line, stringized expression) and halts the program if `expr` is false. Always generated (no `-g` gate); the source file name is omitted from the message. |
| `ASSERTWARN` | ✓ | `ASSERTWARN(expr)` prints the same diagnostic if `expr` is false but continues execution. |
| `STOP` | ✓ |  |

##### Hardware Access

| Keyword | Status | Description |
|---|---|---|
| `INP` | ◐ | Name recognised, **behaviour not implemented**: always answers `-8` (*no port access*), never reads a port. |
| `LPRINT` | ✓ | Line-printer output — no printer, so routed to stdout (reuses the PRINT machinery). |
| `LPOS` | ✓ | Printer head column — always 1 (no printer). |
| `OUT` | ◐ | Name recognised, **behaviour not implemented**: evaluates its operands and writes nothing. |
| `WAIT` | ◐ | Name recognised, **behaviour not implemented**: returns at once. Built on `INP`, so it cannot do more than `INP` does. |

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
| `IS` | ✓ | `SELECT CASE` relational form (`CASE IS > n`) and the RTTI type check (`obj IS Type`). |
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
| `OFFSETOF` | ✓ | `OFFSETOF(type, field)` — a field's byte offset (compile-time). Field-index × 8 (exact for all-64-bit UDTs, consistent with `SizeOf`; no FB packing/alignment for narrow fields). |
| `SIZEOF` | ✓ | The type may carry the `Const` qualifier (`SizeOf(Const T)`), as it may in `Len`, `type<Const T>()` and `New Const T` — const binds to the type and changes neither its size nor its identity (guard `m586`). `SizeOf(scalar-type / UDT / expression)` byte size — an expression is sized by its DECLARED width (`SizeOf(CULng(0))` = 4, `SizeOf(RGB(...))` = 4), never evaluated; `Allocate(n * SizeOf(T))`. Also `CAST`/`CPTR(type, expr)`, whose type may be a pointer or a procedure-pointer type (`CPtr(Sub(), 0)`). A string **literal** or a string `CONST` sizes as a `ZSTRING`: its length + 1, as in fbc. |
| `TYPEOF` | ~ | `DIM AS TypeOf(expr) name` declares a variable with the type inferred from an expression/variable/literal (like VAR without an initializer). The `#if TypeOf(a)=TypeOf(b)` form is **rejected with an error**, not silently evaluated: this preprocessor runs on text, before any declaration is seen, so it cannot answer the question — and answering it "false" (the undefined-identifier rule) would quietly take the wrong branch. |
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
| `DIM` | ✓ | Defines any type of array. Supports `lo TO hi` bounds (incl. negative), positional initializers `= { ... }` / `=> { ... }`, an empty variable-length array `DIM x()` (`UBOUND = -1` until `REDIM`), and an ellipsis upper bound `DIM x(lb TO ...) = { ... }` / `DIM x(...) = { ... }` sized from the initializer. |
| `REDIM` | ✓ | Resizes an array: `REDIM [PRESERVE] arr(ub [, ub ...])` (B1.4) — single or multi-dimensional; each existing dimension's lower bound is kept. If the array was not DIM'd first, REDIM declares it as a fresh dynamic array (honouring the element type and any `lb TO ub` bounds). A multi-dim REDIM'd array computes its element strides at runtime. |
| `PRESERVE` | ✓ | Preserves the overlapping array contents when used with `REDIM` (B1.4). |

#### Clearing Array Data

| Keyword | Status | Description |
|---|---|---|
| `ERASE` | ✓ | `ERASE arr [, arr ...]` resets every element to its default (0 / 0.0 / "") keeping the current size (B1.4). Also erases a UDT **array member**, written out (`Erase obj.arr`) or with the leading dot inside a `WITH` block (`Erase .arr`). |

#### Retrieving Array Size

| Keyword | Status | Description |
|---|---|---|
| `ARRAYLEN` | ✓ | Total element count = product over dims of `(ubound-lbound+1)`; computed at runtime, correct for fixed, `lb TO ub`, multi-dim, and REDIM'd arrays. |
| `ARRAYSIZE` | ✓ | `ARRAYSIZE(arr())` returns the total size in bytes = element count × element size (8-byte bank elements, matching FB Integer/LongInt/Double). |
| `LBOUND` | ✓ | Returns the lower bound of an array's dimension. `LBOUND(arr[, dim])` (B1.4). |
| `UBOUND` | ✓ | Returns the upper bound of an array's dimension. `UBOUND(arr[, dim])` (B1.4; honors explicit `lb TO ub` and dynamic sizes). |

#### Retrieving Array Descriptor

| Keyword | Status | Description |
|---|---|---|
| `Array[Const]DescriptorPtr` | ✗ | N/A — internal array descriptor pointer; not exposed. |

### Bit Manipulation

| Keyword | Status | Description |
|---|---|---|
| `Uinteger` | ✓ | 64-bit unsigned integer type (`DIM AS UINTEGER`); stored in the integer bank with full unsigned compare/`\`/`Mod`/print semantics. |

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

#### Processor Counts (SedaiBasic extension)

**No FreeBASIC equivalent** — FreeBASIC has no way to ask how many processors a machine has, so a
program that sizes a worker pool has to be told from outside. These are **MODERN only**, take no
argument, and answer for the machine the program is running on.

**They are three different quantities, and one name could not serve.** A machine has one or more
CPUs; each CPU has many cores; only the cores with SMT/HyperThreading become **two** logical
processors. On a Core Ultra 9 185H: **1 CPU, 16 cores, 22 logical processors** — the six P-cores
have SMT, the eight E-cores and two LP-E cores do not.

| Keyword | Status | Description |
|---|---|---|
| `PROCESSORCOUNT` | ✓ | **Logical processors** — hardware threads, what the OS schedules onto. This is the number a worker pool wants. |
| `CORECOUNT` | ✓ | **Physical cores**, across every CPU. Never more than `PROCESSORCOUNT`. |
| `CPUCOUNT` | ✓ | **Physical CPUs** — sockets, or packages. Never more than `CORECOUNT`; **1** on any ordinary desktop or laptop. |

- All three are **never less than 1**, so `x \ PROCESSORCOUNT` cannot divide by zero.
- Where the OS will not say, a count falls back to the next larger one it does know rather than
  inventing a number: reporting too many workers is a milder error than reporting one.
- The counts are asked of the OS once and cached; they cannot change while a program runs.
- ⚠️ Three names, **one opcode** (the quantity travels in the immediate) — a build cannot cover one
  count and forget another. Names rather than a parameter because the dialect has no predefined
  constants, and `PROCESSORCOUNT(1)` would say nothing to a reader.
- ⛔ Not available under the **WebAssembly** target: a module has no way to ask, and the backend
  refuses the program rather than inventing a number.

```basic
Dim As Integer nw = ProcessorCount                  '' one worker per hardware thread
If Len(Command(2)) > 0 Then nw = CInt(Command(2))   '' unless told otherwise
```

#### Bit Counting and Rotation (SedaiBasic extension)

**No FreeBASIC equivalent** — FreeBASIC has no operator or function for any of these. They are
**MODERN only**, and each maps to exactly one machine-level bit operation, which is why the names
say what they do rather than abbreviating it. The `32` forms read the **low 32 bits**; the plain
forms read all 64.

| Keyword | Status | Description |
|---|---|---|
| `COUNTLEADINGZEROS` / `COUNTLEADINGZEROS32` | ✓ | Number of zero bits above the highest set bit. **Zero gives the full width** (64, or 32), not an undefined result. |
| `COUNTTRAILINGZEROS` / `COUNTTRAILINGZEROS32` | ✓ | Number of zero bits below the lowest set bit; zero gives the full width. |
| `COUNTONEBITS` / `COUNTONEBITS32` | ✓ | Population count: how many bits are set (0..64, or 0..32). |
| `ROTATELEFT(x, n)` / `ROTATELEFT32(x, n)` | ✓ | Rotate left: bits leaving the top re-enter at the bottom. |
| `ROTATERIGHT(x, n)` / `ROTATERIGHT32(x, n)` | ✓ | Rotate right. |

Two rules are worth stating because they are decisions, not conveniences:

- **A rotate count is taken modulo the width, as an unsigned amount.** `ROTATELEFT(x, 64)` is `x`,
  `ROTATELEFT(x, 65)` rotates by 1, and `ROTATELEFT(x, -1)` rotates by 63 — the same as
  `ROTATERIGHT(x, 1)`. ⚠️ This is deliberately **not** the rule `SHL`/`SHR` follow: a shift past the
  width *saturates*, because a shift past the width has no natural answer while a rotation does.
- **A 32-bit rotate sign-extends its result**, so that it is the value a `Long` holds:
  `ROTATELEFT32(1, 31)` is `-2147483648`. The counting forms return small non-negative numbers and
  are unaffected.

```basic
Dim As LongInt x = &H0000FF00
Print COUNTLEADINGZEROS(x)      ' 48
Print COUNTLEADINGZEROS32(x)    ' 16 — the same value read as 32 bits
Print COUNTONEBITS(x)           ' 8
Print ROTATELEFT(x, 8)          ' 16711680

' What a leading-zero count is usually for: the number of bits a value needs.
Function BitWidth(ByVal n As LongInt) As LongInt
  If n = 0 Then Return 0
  Return 64 - COUNTLEADINGZEROS(n)
End Function
```

### 32-bit integer arithmetic: `INT32` and `UINT32`

**MODERN only**, and a declared extension: no dialect we mirror has these types.

`LONG` and `ULONG` are 32 bits **wide** and compute at 64: an operation on them produces a full 64-bit
result, and the value is only wrapped on its way into a variable. That is FreeBASIC's rule and nothing
about it changes. `INT32` and `UINT32` are 32 bits wide **and compute at 32 bits**: the operands enter
an operation wrapped and the result comes out wrapped, so an intermediate never carries more than 32
bits into the next operation.

| Keyword | Status | Description |
|---|---|---|
| `INT32` | ✓ | 32-bit signed integer. Storage and printing as `LONG`; arithmetic wraps to 32 bits at every step. |
| `UINT32` | ✓ | 32-bit unsigned integer. Storage and printing as `ULONG`; arithmetic wraps to 32 bits at every step. |

For `+`, `-`, `*` and the bitwise operators the two families agree, because those operations are
congruent modulo 2^32 — wrapping once at the end is the same as wrapping at every step. They differ
exactly where the operation is **not** congruent: a shift, a division, a `MOD`, a comparison.

```basic
Dim As ULong  la = 3000000000, lb = 7
Dim As UInt32 a  = 3000000000, b  = 7
Print (la * lb) Shr 16      ' 320434 — the product kept all 64 bits, then shifted
Print (a  * b)  Shr 16      '  58290 — the product wrapped to 32 bits first
```

Two rules follow the language's existing ones rather than inventing new ones:

- **Something wider promotes the pair.** Mixing with a 64-bit integer or a floating-point value gives
  the wider type, exactly as mixing a `SINGLE` with a `DOUBLE` gives a `DOUBLE`. An integer **literal**
  is neutral and takes the type of the expression, so `x * 3` on an `INT32` stays 32-bit.
- **Unsignedness is contagious.** An operation with a `UINT32` on either side wraps unsigned.

```basic
' What the type is for: an algorithm that needs 32-bit truncation partway through.
Function Fnv1a(ByVal s As String) As UInt32
  Dim As UInt32 h = 2166136261
  For i As Integer = 1 To Len(s)
    h = (h Xor Asc(Mid(s, i, 1))) * 16777619
  Next i
  Return h
End Function
```

### Console Functions

#### Configuring the Console

| Keyword | Status | Description |
|---|---|---|
| `CLS` | ✓ | Clears the screen and homes the cursor (alias of `SCNCLR`; resets `POS`/`CSRLIN` to 0). |
| `WIDTH` | ✓ | Sets or returns the number of rows and columns of the console display. |
| `VIEW PRINT` | ✓ | `VIEW PRINT [firstrow TO lastrow]` sets the console's text print area, and so its scroll region: text wraps at its right edge and scrolls at its bottom, `CLS` clears only it, and the cursor moves to the start of `firstrow`. Rows are 1-based; a bare `VIEW PRINT` restores the whole screen. Shares the print-area machinery with the Commodore v7 `WINDOW` command. |

#### Cursor Color and Positioning

| Keyword | Status | Description |
|---|---|---|
| `COLOR` | ✓ | Changes the foreground and background color of text to be written. |
| `CSRLIN` | ✓ | Returns the row position of the cursor (VM-tracked, parallels `POS`). |
| `POS` | ✓ | Returns the column position of the cursor. |
| `LOCATE` | ✓ | Sets the row and column position of the cursor (1-based). Dialect-aware: in MODERN it moves the **text** cursor, as FreeBASIC defines it; in CLASSIC, `LOCATE x, y` keeps the Commodore v7 meaning and moves the bit-map **pixel** cursor. Cursor visibility (the `state` argument) is not modelled. |
| `SCREEN (Console)` | ✓ | Gets the character or color attribute at a given location. `SCREEN(row, col)` yields the character code; a non-zero `colorflag` yields the colour attribute, packed as FreeBASIC packs it for a palette console of up to 4 bits per pixel (background in the high nibble, foreground in the low). |

#### Writing Text to the Console

| Keyword | Status | Description |
|---|---|---|
| `PRINT` | ✓ |  |
| `? (Shortcut for 'PRINT')` | ✓ | `?` is a shorthand for `PRINT`. |
| `PRINT USING` | ✓ | Sign positions: `+` first or last prints `+`/`-`; `-` **first or last** prints `-` for a negative and a **blank** for a positive. A leading sign owns its own field position, so it never causes the `%` overflow marker by itself; a sign with no position asked for competes with the digits (`"#.###"` on `-1.5` gives `%-1.500`). **Was wrong until 12 Aug 2026**: a `-` in first position was silently dropped, so every negative overflowed its field. |
| `? USING (Shortcut for 'PRINT USING')` | ✓ | `? USING mask; expr` is a shorthand for `PRINT USING`. |
| `WRITE` | ✓ | `WRITE #n, ...` quoted-CSV file output and console `WRITE v1, v2` (strings double-quoted, comma-separated). |
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
| `DATEPART("ww")` | ✓ | Week number by the **VB rule**, which is what `fbc` answers: week 1 is the week *containing* 1 January and weeks start on **Sunday**. ⚠️ Not ISO 8601 (Monday-based, week 1 = the week of the first Thursday); the two agree except on Sundays. Measured against `fbc` over 48 dates, 23 Aug 2026. |
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
| `ERFN` | ✓ | Name of the procedure in which the last error occurred; empty at module level. FreeBASIC returns a `ZSTRING PTR` and writes `*Erfn()`; SedaiBasic returns the name as a STRING and accepts both `Erfn()` and `*Erfn()`. Cleared by RESUME in MODERN, like ERR. |
| `ERMN` | ✓ | Name of the module (source file) the last error came from. Returns a STRING, not a `ZSTRING PTR`; `Ermn()` and `*Ermn()` both work. |
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
| `OPEN COM` | ✗ | Binds a file number to a communications port. **Not implemented**: the parser's device branch knows only `CONS`/`SCRN`/`ERR`, so `COM` is read as an ordinary word and its (empty) value becomes the filename; `IsReservedDeviceName` then refuses the string form outright. |
| `OPEN CONS` | ✓ | Binds a file number to the standard input and output streams. |
| `OPEN ERR` | ✓ | Binds a file number to the standard input and error streams. |
| `OPEN LPT` | ✗ | Binds a file number to a printer device. **Not implemented**, same reason as `OPEN COM`. Deliberately never exercised by any net either: FreeBASIC's LPT support goes through the Windows print spooler, so running such an example queues a real job on a real printer. |
| `OPEN PIPE` | ✗ | Binds a file number to the input and output streams of a process. **Not implemented**: the parser does not know `PIPE` at all, and the line is a syntax error. |
| `OPEN SCRN` | ✓ | Binds a file number directly to the console. |
| `CLOSE` | ✓ | Unbinds a file number from a file or device. ⚠️ The **statement** only: `CLOSE #n` and the bare `CLOSE` (all channels). The FUNCTION form `Close(n)`, which answers an error code, is not implemented — see *Declared unsupported*. |
| `RESET` | ✓ | Unbinds all active file numbers (closes every open handle; alias of DCLEAR). |
| `INPUT (File Mode)` | ✓ | Text data can be read from the file. |
| `OUTPUT` | ✓ | `OPEN "f" FOR OUTPUT AS #n` opens the file for writing (truncating). |
| `APPEND` | ✓ | Text data is added to the end of a file when output. |
| `BINARY` | ✓ | `OPEN "f" FOR BINARY AS #n`; byte/record access via `PUT #n,pos,var` / `GET #n,pos,var`. |
| `RANDOM` | ✓ | `OPEN "f" FOR RANDOM AS #n LEN=size`; fixed-size record access via `PUT #n,rec,var` / `GET #n,rec,var`. |
| `ACCESS` | ✓ | `OPEN ... ACCESS {READ\|WRITE\|READ WRITE} ...` — parsed and accepted (the VM does not enforce share/access rights). |
| `READ (File Access)` | ✓ | Binary data can only be read from the file. |
| `WRITE (File Access)` | ✓ | `WRITE #n, ...` writes quoted comma-separated (CSV) values to the file. |
| `READ WRITE(File Access)` | ✓ | Binary data can be read from and written to the file. |
| `ENCODING` | ✓ | `OPEN ... ENCODING "ascii\|utf8\|utf16\|utf32"`, in the statement form and in the function form alike. The name need not be a literal — `ENCODING encod` and `ENCODING files(i).encoding` are evaluated at run time. `utf16`/`utf32` re-encode file I/O (little-endian, byte-order mark written on creation and skipped on read); `utf8` writes the BOM but needs no conversion, our strings being UTF-8 bytes already; `ascii` is the passthrough. ⚠️ Declared divergence: a file opened FOR INPUT with an explicit encoding is not validated against its byte-order mark, where fbc answers error 3. |

#### Reading from and Writing to Files or Devices

| Keyword | Status | Description |
|---|---|---|
| `INPUT #` | ✓ | Reads a list of values from a file or device. A numeric field is read with the SAME grammar `VAL` uses — the `&H`/`&O`/`&B` base prefixes, a saturating magnitude, the full 64 bits — and a `BOOLEAN` destination takes the words `true`/`false` in either case, anything else through that grammar with non-zero meaning true. (Until 26 Aug 2026 it had its own conversion, 32-bit and prefix-blind: `Val("&h1F")` was 31 and `INPUT #` of the same text was 0. Guard `m584`.) |
| `WRITE #` | ✓ | Writes a list of values to a file as quoted CSV (strings in `"`, comma-separated). |
| `INPUT()` | ✓ | `INPUT(n, [#]filenum)` — reads n characters (BYTES; `WINPUT()` counts Unicode codepoints instead) from a file. A short read at end of file returns fewer characters, as in FreeBASIC. |
| `WINPUT()` | ✓ | `WINPUT(n, [#]filenum)` — reads n wide characters (Unicode codepoints) from a file. A WSTRING is UTF-8 here, so a character may span several bytes; a short read at end of file returns fewer characters, as in FreeBASIC. |
| `LINE INPUT #` | ✓ | `LINE INPUT #n, s` reads a whole line of text (commas not split). |
| `PRINT #` | ✓ | The file number may be written parenthesised, `PRINT #(1), x` — as it may on every statement that takes one (`OPEN … AS`, `CLOSE`, `INPUT #`, `LINE INPUT #`, `WRITE #`, `GET`/`PUT`, `SEEK`). |
| `? # (Shortcut for 'PRINT #')` | ✓ | `? #n, ...` is the shortcut for `PRINT #n, ...` (the lexer maps `?` to PRINT). |
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
| `LOCK` | ✓ | File record locking — a no-op on a single-process VM (arguments parsed and discarded). |
| `UNLOCK` | ✓ | Release file record locks — a no-op on a single-process VM. |

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
| `CLEAR` | ✓ | `CLEAR(dst, value, bytes)`: set a block of raw heap memory (from Allocate) to a byte value. v1 takes the pointer directly. Over a **managed record** (a `T PTR` element of a `NEW`/`CALLOCATE` block, or a record variable) there is no byte image to write over — the record is slots in a table — so the operation resets the instance to what a fresh allocation gives it. Bounded and declared: only a fill value of **0** is honoured (any other byte pattern still refuses), the **byte count is ignored** (the whole instance is reset), and nested-UDT / member-array fields keep their instances, since their slots hold handles. |
| `FB_MEMCOPY` | ✓ | `FB_MEMCOPY(dst, src, bytes)`: copy a block of raw heap memory; returns dst. v1 takes pointers directly. Both address positions are **ByRef** — the address of the lvalue NAMED — so `fb_memcopy(q, p, n)` on two pointer variables copies the POINTERS and `fb_memcopy(*q, *p, n)` copies the memory, as in fbc. Where the lvalue names a **managed object** (a record variable, an element, `*p`, a nested field, or a fixed-length string field) the copy is honoured as the copy of that OBJECT: the byte count is a character count on a string field and is not read at all on a record. |
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
| `FILEATTR` | ✓ | `FILEATTR(filenum[,returntype])` -> info about an open file number: returntype 1 (default) = File Mode (Input=1/Output=2/Random=4/Append=8/Binary=32), 2 = OS handle, 3 = Encoding (0=ASCII). |
| `FILECOPY` | ✓ | Copies a file (`FILECOPY src, dst`). |
| `FILEDATETIME` | ✓ | `FILEDATETIME(path)` -> the file's last-modified timestamp as a Date Serial (Double), or 0 if absent; cross-platform. |
| `FILEEXISTS` | ✓ | `FILEEXISTS(path)` returns -1 if the file exists, else 0 (cross-platform). |
| `FILELEN` | ✓ | `FILELEN(path)` -> file size in bytes (0 if the file does not exist); cross-platform. |
| `FILESETEOF` | ✓ | `FILESETEOF filenum` sets an open file's length to the current 1-based position (truncates if before EOF, extends with zero bytes if beyond). Statement form. |
| `FILEFLUSH` | ✓ | `FILEFLUSH [[#]filenum]` — accepted as a no-op (the VM's file streams are unbuffered, so buffered output is already written). |

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
| `COMMAND` | ✓ | `COMMAND$([index])` returns command-line arguments: bare / `-1` = all program args (space-separated), `0` = executable name, `n` = the n-th argument (`""` if out of range). On `sb`, arguments are the non-flag tokens after the script file (`sb prog.bas arg1 arg2`); sb's own flags are still recognised anywhere. |
| `ENVIRON` | ✓ | `ENVIRON$(name)` -> the value of an environment variable ("" if unset). |
| `ISREDIRECTED` | ✓ | Whether a standard stream is redirected — portable default 0 (not redirected). |
| `SETENVIRON` | ✓ | Sets an environment variable (a VM-internal override that ENVIRON$ reads back). |
| `SHELL` | ✓ | Runs a command through the platform shell (cmd.exe / /bin/sh); returns the exit code. |
| `SYSTEM` | ✓ | `SYSTEM [exitcode]` ends the program like `END` (an optional exit code is parsed and ignored). |

### String Functions

#### Creating Strings

| Keyword | Status | Description |
|---|---|---|
| `STRING` | ✓ | 8-bit character string data type. |
| `STRING (Function)` | ✓ | `STRING(count, ch)` returns `count` copies of a character (a char code or the first character of a string). |
| `ZSTRING` | ✓ | Null-terminated 8-bit character string data type. |
| `WSTRING` | ✓ | Standard data type: wide character string (UTF-8 storage, codepoint-aware LEN/slice). |
| `WSTRING (Function)` | ✓ | `WSTRING(n, cp)` — n copies of the wide char for Unicode codepoint cp. |
| `SPACE` | ✓ | Returns a String of N spaces. `SPACE(n)` / `SPACE$(n)` (B1.2). |
| `WSPACE` | ✓ | `WSPACE(n)` — a wide string of n spaces. |
| `LEN` | ✓ | Returns the length of a string in characters. Takes a TYPE NAME too, including a pointer type (`Len(Integer Ptr)`, `Len(Any Ptr)`), and an expression of known declared width (`Len(CULng(0))` = 4). For a user-defined type it returns the size of the type in bytes, as FreeBASIC does when the type declares no `OPERATOR LEN` (which is not supported); in particular it does **not** route through `OPERATOR CAST() AS STRING`. |

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
| `FORMAT` | ✓ | `FORMAT(num [, mask])` / `FORMAT$` → formatted string. Numeric masks (`0`/`#`, `.`, `,` grouping, `%`, scientific `E±`, literals) **and** date/time masks (`d`/`dd`/`ddd`/`dddd`, `m`/`mm`/`mmm`/`mmmm` & minute-after-`h`, `n`, `y`/`yy`/`yyyy`, `h`/`hh`, `s`/`ss`, `ttttt`, `AM/PM`/`A/P`, `:` `/` separators). English month/day names. |

#### String to Numeric Conversions

| Keyword | Status | Description |
|---|---|---|
| `VAL` | ✓ | Returns the Double conversion of a numeric string. |
| `VALINT` | ✓ | Returns the Integer conversion of a numeric string. |
| `VALLNG` | ✓ | Returns the Long conversion of a numeric string. |
| `VALUINT` | ✓ | Returns the uInteger conversion of a numeric string. |
| `VALULNG` | ✓ | Returns the ULong conversion of a numeric string. |

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
| `LSET` | ✓ | Left-justifies a string into a buffer (string lvalues; QBasic `=` and FreeBASIC `,` forms). Over a **fixed-length** destination the buffer is its DECLARED capacity, so the result is padded to it. |
| `RSET` | ✓ | Right-justifies a string into a buffer (string lvalues; QBasic `=` and FreeBASIC `,` forms). Same declared-capacity rule as `LSET`. |

### Threading Support Functions

#### Threads

| Keyword | Status | Description |
|---|---|---|
| `THREADCALL` | ✓ | Starts a procedure with parameters in a separate thread of execution. `h = THREADCALL sub(a, b, ...)` — typed, multi-argument (int/float/string), like a normal call (M5.5). |
| `THREADCREATE` | ✓ | Starts a procedure in a separate thread of execution. `h = THREADCREATE(@sub [, param])` (M5.2; one param, any type; workers share global arrays + arrays of UDT). |
| `THREADWAIT` | ✓ | Waits for a thread to finish and releases the thread handle. `THREADWAIT h` (M5.2). |
| `THREADDETACH` | ✓ | Releases a thread handle without waiting for the thread to finish. `THREADDETACH h` (M5.5; v1: cleaned up at program end). |
| `THREADSELF` | ✓ | Returns the thread handle of the current thread. `h = THREADSELF()` (0 on the main thread) (M5.5). |

> **Worker limit.** At most **64 workers may be live at once**; a `THREADCREATE`/`THREADCALL` beyond that
> fails with a runtime error rather than spawning. A worker counts as live from its creation until its
> procedure returns, so joining with `THREADWAIT` (or letting detached workers finish) frees slots. The
> ceiling sits far above any realistic program and exists as a backstop: it bounds the damage from a
> runaway spawn, which would otherwise saturate the host machine instead of failing the program.

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
| `INPUT()` | ✓ | `INPUT(n)` — reads n characters from the keyboard, unechoed. The INPUT *statement* is unaffected: it is parsed at statement level and never reaches the expression parser. |
| `WINPUT()` | ✓ | `WINPUT(n)` — reads n wide characters from the keyboard, unechoed. Extended keys are not read. (FreeBASIC itself does not read wide characters from the console.) |

#### Reading keys from the keyboard buffer

| Keyword | Status | Description |
|---|---|---|
| `INKEY` | ✓ | Non-blocking read of the first key waiting in the keyboard buffer (`INKEY` / `INKEY$`); returns `""` if none. |
| `GETKEY` | ✓ | Gets and waits for the first key in the keyboard buffer. |

#### Detecting key status by keyboard scancode

| Keyword | Status | Description |
|---|---|---|
| `MULTIKEY` | ✓ | `MULTIKEY(scancode)` returns -1 if the key (FB AT scancode) is held, 0 otherwise — real-time. Live on `sb --window` and sbv (SDL keyboard state); headless `sb` reports all keys up. |

### Graphics - 2D Drawing

#### Working with Color

| Keyword | Status | Description |
|---|---|---|
| `COLOR` | ✓ | Sets the foreground/background drawing colours. C128 form (`COLOR source,color`, CLASSIC dialect) and FreeBASIC form (`COLOR [fg][,bg]`, MODERN dialect → sets the current draw foreground/background; subsequent PSET/LINE/CIRCLE/PAINT with no explicit colour use the foreground). |
| `PALETTE` | ✓ | `PALETTE index,r,g,b` sets a palette entry (components 0-255); the QB-compat 2-arg form `PALETTE index,&hBBGGRR` sets it from a packed BGR value (components 0-63, scaled to 0-255); `PALETTE GET index,r,g,b` reads it back into variables; `PALETTE` alone resets to the mode default (via IGraphicsBackend; headless-testable round-trip). `PALETTE USING` deferred. |
| `RGB` | ✓ | `RGB(r,g,b)` returns an opaque 32-bit colour (= `RGBA(r,g,b,255)`; alpha in bits 24-31). Constant-folded when all args are constant. |
| `RGBA` | ✓ | Returns a color value including alpha (transparency) for hi/truecolor modes. |
| `POINT` | ✓ | `POINT(x,y[,img])` reads a pixel's colour from the screen surface, or from an image surface when a 3rd image-handle argument is given (via the IGraphicsBackend abstraction). |

#### Drawing to Image Buffers

| Keyword | Status | Description |
|---|---|---|
| `PSET and PRESET` | ✓ | `PSET [img,][STEP](x,y)[,color]` / `PRESET ...` plot a pixel (PRESET's omitted colour = the background); `STEP` = coordinate relative to the current graphics point; an optional leading image handle draws on that off-screen image (`PSET img,(x,y)`). Via IGraphicsBackend, headless-testable and on-screen on sbv. |
| `LINE (GRAPHICS)` | ✓ | `LINE [img,][STEP](x1,y1)-[STEP](x2,y2)[,color][,B\|BF]` draws a line (a leading image handle draws on that off-screen image), box outline (B) or filled box (BF) on the screen surface (via IGraphicsBackend; headless-testable + on-screen on sbv). Omitted start (`LINE -(x2,y2)`) draws from the current graphics point; `STEP` = relative coordinates (first STEP relative to the current point, second relative to the first point). Parenthesised form disambiguates from `LINE INPUT`. Line-style (dashed): a trailing 16-bit `style` mask (`LINE ...,color,[B|BF],style`) drawn MSB-first, repeating every 16 pixels. |
| `CIRCLE` | ✓ | Plots circles and ellipses. C128 form (`CIRCLE source,x,y,...`) and FreeBASIC form (`CIRCLE [img,][STEP](x,y),r[,color]`, parenthesised (a leading image handle draws on that off-screen image) → routed through IGraphicsBackend, headless-testable + on-screen on sbv); `STEP` = centre relative to the current graphics point. Ellipse (aspect) and arcs (start/end angle) supported; the fill flag (F) and pie-slice for negative angles deferred. |
| `DRAW` | ✓ | Draws in a sequence of commands on an image buffer or screen. |
| `DRAW STRING` | ✓ | `DRAW STRING [img,][STEP](x,y), text [,colour]` writes text into an image buffer or the screen with the built-in 8×8 font (`SedaiGfxFont`), transparent background, routed through `IGraphicsBackend.DrawText` — headless-testable via `POINT`. A trailing font argument is accepted and ignored (one built-in face). A character outside ASCII 32–126 draws a visible hollow box rather than nothing. **Was a false tick until 5 Aug 2026**: the `img,` form was a syntax error and the plain form was accepted and drew nothing at all. |
| `PAINT` | ✓ | Flood fill. C128 form (`PAINT source,x,y`) and FreeBASIC form (`PAINT [img,][STEP](x,y),color[,border]`, parenthesised (a leading image handle fills that off-screen image) → routed through IGraphicsBackend, headless-testable). An optional border colour selects the boundary-fill form (fill up to the border colour); `STEP` = coordinate relative to the current graphics point. |

#### Image Buffer Creation

| Keyword | Status | Description |
|---|---|---|
| `GET (GRAPHICS)` | ✓ | `GET (x1,y1)-(x2,y2), dst` captures a screen rectangle into image surface `dst` (via IGraphicsBackend; headless-testable). Array-buffer destination deferred. |
| `IMAGECREATE` | ✓ | `IMAGECREATE(w,h[,color])` allocates a truecolor image surface and returns an integer handle (via IGraphicsBackend; software-backed, headless-testable). Default fill is the transparent key (magenta). |
| `IMAGEDESTROY` | ✓ | `IMAGEDESTROY handle` frees an image surface (the id slot is reused by a later IMAGECREATE). |
| `IMAGECONVERTROW` | ✗ | Converts a row of pixels in an image buffer to a different color depth. |
| `IMAGEINFO` | ✓ | `IMAGEINFO handle, w, h` writes the surface width/height into the w and h variables. (Pitch/depth/pixel-pointer forms deferred.) |
| `BLOAD` | ✓ | Creates an image buffer from a file. |
| `BSAVE` | ✓ | Saves an image buffer to a file. |

#### Blitting Image Buffers

| Keyword | Status | Description |
|---|---|---|
| `PUT (GRAPHICS)` | ✓ | `PUT (x,y), src [, mode]` blits image surface `src` onto the screen at (x,y). Modes: PSET/PRESET, TRANS (magenta key), ALPHA, ADD, AND, OR, XOR (CUSTOM falls back to PSET). Array-buffer source deferred. |
| `ADD` | ✓ | Saturated addition of the source and target components. |
| `ALPHA` | ✓ | Blend using the image buffer's alpha channel (per-pixel). The uniform-transparency-level form (`PUT ...,ALPHA,level`) is deferred. |
| `AND (Graphics Put)` | ✓ | Combine the source and target components using a bitwise And |
| `OR` | ✓ | Combine the source and target components using a bitwise Or |
| `XOR (Graphics Put)` | ✓ | Combine the source and target components using a bitwise Xor |
| `PSET (Graphics Put)` | ✓ | Directly copy pixel colors from the source to the destination (the default mode). |
| `PRESET (Graphics Put)` | ✓ | Copy the inverted source pixel colors to the destination. |
| `TRANS` | ✓ | Pixels matching the transparent mask colour (magenta key `&hFF00FF`) are not blitted. |
| `CUSTOM` | ✗ | A user blending procedure — falls back to PSET (no user function-pointer blend callback). |
| `XOR` | ✓ | Combine the source and target components using a bitwise Xor |

### Graphics - User Input

#### Mouse and Joystick Input

| Keyword | Status | Description |
|---|---|---|
| `GETMOUSE` | ✓ | `GETMOUSE(x, y [,wheel] [,buttons] [,clip])` snapshots the mouse into the by-reference variables and returns 0 (ok) / 1 (no mouse / off-window → all fields -1). Buttons is an FB bitmask (bit0=left, bit1=right, bit2=middle). Live on `sb --window` and sbv (SDL mouse state); headless `sb` reports no mouse. Wheel/clip are 0 in v1. |
| `SETMOUSE` | ✓ | `SETMOUSE [x] [,y] [,visibility] [,clip]` moves the cursor and/or toggles visibility (each field -1 = no change; visibility 1=show, 0=hide). Live on `sb --window` and sbv; headless `sb` is a no-op. Clip parsed but ignored in v1. |
| `GETJOYSTICK` | ✓ | `GETJOYSTICK(id, buttons [,a1..a8])` snapshots gaming device `id` (0-15): writes the button bitmask (int) and up to 8 axis values (SINGLE, -1..1, or -1000 if the axis is absent) into the by-reference variables; returns 0 (ok) / 1 (no device → buttons 0, axes -1000). Live on `sb --window` and sbv (SDL joysticks); headless `sb` reports no device. |
| `STICK` | ✓ | `STICK(axis)` (axis 0-3: X/Y of device A/B) → position 1..200, or 0 if not attached. Live on `sb --window` / sbv; headless `sb` → 0. v1 queries the device fresh each call (no STICK(0) latch). |
| `STRIG` | ✓ | `STRIG(button)` (button 0-7) → -1 (pressed) / 0. Even = "pressed since", odd = "is pressed" (v1 reports the current level for both; no edge latch). Live on `sb --window` / sbv; headless `sb` → 0. |

#### Keyboard Input

| Keyword | Status | Description |
|---|---|---|
| `MULTIKEY` | ✓ | See "Detecting key status by keyboard scancode": `MULTIKEY(scancode)` real-time key-down state (live on `sb --window` / sbv). |

### Graphics - Screen

#### Working with screen modes

| Keyword | Status | Description |
|---|---|---|
| `SCREENLIST` | ✓ | Enumerate fullscreen resolutions — returns 0 (no hardware modes on a portable/headless VM). |
| `SCREEN (Graphics) and SCREENRES` | ✓ | `SCREENRES w,h[,depth[,num_pages]]` sets the graphics screen surface; `SCREEN n` selects a numbered QB/FB mode (1/7→320×200, 13→320×200, 18→640×480, 19→800×600, 20→1024×768, 21→1280×1024, …) mapped to a resolution. Both allocate pages and route through IGraphicsBackend (headless-testable via SCREENINFO). depth accepted-and-ignored. `SCREENRES` also has the FUNCTION form fbc accepts, `r = ScreenRes(w, h, ...)`, which answers 0. |
| `SCREENINFO` | ✓ | `SCREENINFO w, h [, depth, bpp, pitch, rate]` writes the current graphics surface's width/height (and depth=32, bpp=4, pitch=w*4) into the variables (via IGraphicsBackend; headless-testable). Desktop-info form deferred. |
| `SCREENCONTROL` | ✓ | Get/set internal graphics settings — a no-op here (arguments parsed and discarded). |
| `SCREENEVENT` | ✗ | Gets system events. |
| `SCREENGLPROC` | ✗ | Returns the address of an OpenGL procedure. |
| `WINDOWTITLE` | ✓ | Parsed; accept-and-ignore (the caption has no effect on the headless/buffered backend; sbv caption plumbing deferred). |

#### Working with pages

| Keyword | Status | Description |
|---|---|---|
| `CLS` | ✓ | Clears the screen and homes the cursor (alias of `SCNCLR`). |
| `SCREENSET` | ✓ | `SCREENSET work[,visible]` selects the work page (all drawing/POINT target it) and the visible page (shown on sbv). `SCREENRES w,h,depth,num_pages` allocates the pages. Headless-testable. |
| `SCREENCOPY and PCOPY and FLIP` | ✓ | `PCOPY src,dst` and `SCREENCOPY [src][,dst]` (default work→visible) copy one page onto another; `FLIP` (no args) swaps the work and visible pages (`FLIP visible[,work]` sets them). Headless-testable; on-screen page display on sbv deferred. |
| `SCREENSYNC` | ✓ | Parsed; accept-and-ignore (no vertical-retrace wait on the headless/buffered backend). |

#### Working video memory

| Keyword | Status | Description |
|---|---|---|
| `SCREENPTR` | ✓ | Raw pointer to the working page's pixel bytes (32bpp; row pitch from `SCREENINFO`). Writes through it change what `POINT` reads, and `PSET` changes what it reads — the drawable surface is a CPU buffer, not a copy. It names a second REGION of the raw-pointer namespace, so it is a byte offset the VM bounds-checks, never a machine address; `DEALLOCATE` on it is ignored and `REALLOCATE` rejected. Returns 0 with no graphics screen. |
| `SCREENLOCK` | ✓ | Parsed; accept-and-ignore (the drawable surface is always a CPU buffer, so no lock is needed). |
| `SCREENUNLOCK` | ✓ | Parsed; accept-and-ignore (pairs with SCREENLOCK). |

#### Screen Metrics

| Keyword | Status | Description |
|---|---|---|
| `VIEW (GRAPHICS)` | ✓ | `VIEW [SCREEN] (x1,y1)-(x2,y2)` sets a viewport: drawing is clipped to it and (without SCREEN) coordinates become relative to its top-left; bare `VIEW` resets to the full screen. Optional fill/border colours accepted-and-ignored (v1). Disambiguated from QB `VIEW PRINT`. |
| `WINDOW` | ✓ | `WINDOW [SCREEN] (x1,y1)-(x2,y2)` sets a logical coordinate system mapped onto the screen (default Y-flip; SCREEN = no flip); bare `WINDOW` disables it. PSET/LINE/CIRCLE/PAINT/POINT map logical→physical (CIRCLE radius scaled by the x-axis scale). GET/PUT stay in physical coords (v1). Disambiguated from the C128 text `WINDOW`. |
| `PMAP` | ✓ | `PMAP(coord, n)` maps between logical and physical coordinates (n: 0=lx→px, 1=ly→py, 2=px→lx, 3=py→ly) using the active WINDOW transform. |
| `POINTCOORD` | ✓ | Queries DRAW's pen position — `POINTCOORD(0)` = x, `POINTCOORD(1)` = y. |

#### Screen Data Types

| Keyword | Status | Description |
|---|---|---|
| `EVENT` | ✗ | Data type for ScreenEvent function. |

### Runtime errors in MODERN: fbc's own abort message

An **uncaught** FreeBASIC runtime error aborts the way `fbc`'s runtime aborts, word for word — a blank
line, `Aborting due to runtime error N [(text)] at line L of <module>()`, a blank line — and the error
number is the process **exit code**. The parenthesised text exists for codes 1–17 only; above that
`fbc` names nothing and prints the number alone. The module is the source path exactly as it was
passed on the command line, which is the same value `ERMN` reports, and `#line n "file"` renames it.

⛔ **Only an error that carries a FreeBASIC number.** A Pascal exception leaking out of the VM (a
raw-pointer range error, an access violation) is *ours*: `fbc` has no such error, and dressing it in
`fbc`'s sentence would claim a fidelity we do not have. Those keep this project's own voice
(`ERROR during VM execution (BASIC LINE n): …`), which is also what lets a net tell *"the program
failed as fbc's would"* from *"we cannot do this"*. `--verbose` restores the full dump either way.

**CLASSIC is untouched**: its errors follow the Commodore table, and the two tables stay separate.

⚠️ `#line` currently reaches the **abort message** and `__LINE__`. `ERL`, `ERMN` and `Assert`'s
`path(line):` prefix still report the physical position.

### Nested types

A **named** `Type` or `Union` declared inside a `Type` is a type of its own, not a set of fields:
`Union U … End Union` inside `Type T` declares `U`, reachable as `U` and as `T.U`. A method is defined
qualified — `Sub T.U.proc` — and `This` inside it is the *nested* type. A nested type reaches its
enclosing type's **private** members, as it does in FreeBASIC and in C++. The **anonymous** form is
what it has always been: a layout block whose members are sequential inside the surrounding union.

### Declared divergence: integer division by zero

`x \ 0` and `x Mod 0` raise a **catchable runtime error** here. `fbc` emits the bare machine
instruction, so on x86 the program takes a hardware `SIGFPE` and **dumps core** — the manual's own
`control/iif` and `control/iif2` do exactly that. Ours is a defined state where `fbc`'s is a crash;
the divergence is deliberate and is not going to be reproduced.

### Declared unsupported (24 August 2026)

Each of these is *refused with a message that names the reason*, never answered wrongly in silence.

- **`TypeOf` as an exact type**: `Dim As TypeOf(x)` works, but the inferred type is approximated to the
  BANK (string / integer / floating point). `Cast(TypeOf(p), 0)` with `p As Double Ptr` does not yield
  `Double Ptr`, so it is not supported.
- **`Close(n)` as a FUNCTION.** fbc lets `CLOSE` be called as an expression that answers an error
  code (`0` when the channel was open, `1` = illegal function call otherwise). Only the STATEMENT
  forms are implemented here — `Close #n` and the bare `Close`, which closes every channel.
- **`ProcPtr(p, Virtual ...)`** (fbc 1.10+) asks for a member's **vtable index**, not its address.
  There is no vtable a program can index here: a virtual call goes through a generated dispatcher
  keyed on the instance's runtime type-id. Call the method directly — the dispatch is the same one.
- **`__FB_IIF__` with a condition that does not fold to a constant.** It chooses a branch while
  compiling, so the condition must be constant; fbc folds some that we cannot (a vtable index, for
  one). `IIf(...)` is the run-time form and is fully supported.
- **`Clear` / `FB_MEMCOPY` / `FB_MEMMOVE` over an array whose elements are declared NARROW**
  (`As Short`, `As UByte`, …). An `Integer`/`LongInt`/`Double` array is a real contiguous byte image
  here and these ops work over it exactly as in fbc; a narrow element type is stored widened, so a
  byte count would cover a different number of elements. Loop over the elements instead.
- **`Clear` / `FB_MEMCOPY` over a STRING array.** No byte image: the elements are managed strings.
  ⚠️ Over a **managed record** both are supported, as the copy or the reset of the OBJECT the reference
  names rather than of a byte range — see `CLEAR` and `FB_MEMCOPY` in the tables above for exactly what
  is honoured and what is ignored (the byte count, in both). `Reallocate` of such a block is supported
  too and keeps what was there.
- **A BYTE VIEW over an array through `Any Ptr` / a narrow-pointee cast.** `Cast(UByte Ptr, @a(0))[i]`
  walks *elements*, not bytes: an array is typed storage here (one `Int64` or `Double` per element),
  not a byte image, so a pointer into it can only step by element. `Cast(T Ptr, p)[i]` *is* supported
  and matches fbc whenever the pointee is the array's own element type, and over the raw byte heap
  (`Allocate`) it matches for every width. Same root as `ByRef As Any` below.
- **`Dim As T a(n) = Any`** parses and means "do not initialise" — but the storage still comes out
  zeroed here, where fbc hands back whatever was on the stack. A defined state instead of an undefined
  one.
- **`__FUNCTION_NQ__` read as a VALUE.** It substitutes the *symbol*, not a string, so in fbc
  `Return __FUNCTION_NQ__` inside its own function is a recursive **call** — the program compiles with
  "infinite recursion detected" and dies. Here it yields the procedure's name as text, like
  `__FUNCTION__`. ⚠️ The one use the manual makes of it, `@__FUNCTION_NQ__` (the enclosing
  procedure's own address), *is* supported and means exactly `@<that procedure>`.
- **The C standard library through `<crt.bi>`** (`printf`, `fopen`, `fprintf`, `vprintf`, …). There is
  no C FFI here and there will not be one: the VM owns its memory and its file handles, and handing a
  BASIC program a real `FILE*` is the one thing the memory-safety design exists to prevent. Including
  the header is harmless — most programs that do never call into it — but a *call* fails, now with a
  message that says so rather than "Array not declared". Use the BASIC equivalents
  (`Open`/`Print #`/`Close`, `Print Using`).
- **Inline assembly**: `Asm … End Asm`, `Naked` procedures, `#pragma reserve`, and `__FB_ASM__`
  branches that select one. Machine code in the source is not something a bytecode VM can host —
  one of its engines is an interpreter.
  ⚠️ Until 26 August 2026 an `Asm … End Asm` block was worse than unsupported: `Asm` is not a reserved
  word, so the block parsed as a bare call to an undefined name and its closing `End Asm` was read as
  plain `End` — which **stops the program**. A `Print` before the block ran, a `Print` after it did
  not, and the exit code was `0`. It is now refused by name, which is what this whole section promises.
- **Reading fbc's own RTTI block through raw pointers**, as the manual's `proguide/*rtti_info`
  examples do (`CPtr(Any Ptr Ptr Ptr, po)[0][-1]` walks the vtable to the type-info record, then its
  base chain and mangled name). That is fbc's object ABI, and this VM has none to expose: an instance
  is a managed record, its runtime type is an id, and a virtual call goes through a generated
  dispatcher rather than a vtable slot. `Object`, `Extends`, `Is`, virtual and abstract members are
  all supported — only the memory *layout* behind them is not a thing a program can walk here.
