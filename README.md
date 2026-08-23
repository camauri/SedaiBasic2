# SedaiBasic2 Interpreter with VM

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![LinkedIn](https://img.shields.io/badge/LinkedIn-Maurizio%20Cammalleri-0077B5?logo=linkedin)](https://www.linkedin.com/in/maurizio-cammalleri-80a89a11/)
[![Substack](https://img.shields.io/badge/Substack-Maurizio%20Cammalleri-FF6719?logo=substack)](https://cammalleri.substack.com/)

```
   ____           _       _ ____            _      ____
  / ___|  ___  __| | __ _(_) __ )  __ _ ___(_) ___|___ \
  \___ \ / _ \/ _` |/ _` | |  _ \ / _` / __| |/ __| __) |
   ___) |  __/ (_| | (_| | | |_) | (_| \__ \ | (__ / __/
  |____/ \___|\__,_|\__,_|_|____/ \__,_|___/_|\___|_____|
```

## ⚠️ Branch Information

| Branch | Status | Description |
|--------|--------|-------------|
| **main** | [![Stable](https://img.shields.io/badge/status-stable-green.svg)]() | Stable snapshot for benchmarking and general testing |
| **develop** | [![Development](https://img.shields.io/badge/status-development-orange.svg)]() | Active development branch with the full two-dialect language (Commodore v7 + FreeBASIC), OOP, threading, pointers, graphics and audio. Compiles successfully; may contain bugs |

> **Recommended:** Use `develop` for the current language and feature set. Use `main` for a stable snapshot when benchmarking.

## Documentation

> **Note:** This README provides an overview. For detailed information, see:
> - [INSTALL.md](INSTALL.md) - **Everything the build needs, per platform, and how to get it**
> - [BASIC.md](BASIC.md) - Complete list of BASIC commands with implementation status
> - [ARCHITECTURE.md](ARCHITECTURE.md) - Detailed compilation pipeline and VM architecture
> - [CONSOLE.md](CONSOLE.md) - Keyboard shortcuts and graphics mode reference
> - [BENCHMARK.md](BENCHMARK.md) - Benchmark results against the reference implementations
> - [WEB_BASIC.md](WEB_BASIC.md) - Web BASIC (`sbw`), the stand-alone HTTP server
> - [ROADMAP.md](ROADMAP.md) - Future directions and project architecture

## What is SedaiBasic2?

SedaiBasic2 began as a reimplementation of Commodore BASIC v7 and is now a language in its own right, built on a full optimizing compiler pipeline that targets a fast register-based bytecode virtual machine. It carries **two dialects** in the same engine, and they are not the same kind of thing:

- **CLASSIC**: line-numbered, from Commodore BASIC v7 (**202 / 210** core commands, 96%). It keeps
  v7's *language*, not v7's machine: this does not run on a C64/C128 and has none of its peripherals,
  so the palette is 256 RGBA entries where v7 had sixteen fixed colours, sprites go to 256×256 and
  full colour against the C128's fixed 24×21, there are twelve video modes plus a dynamic one, and
  the audio is our own engine, in the reSID lineage, well past the chip it descends from.
- **MODERN**: line-number-free, from FreeBASIC. Compatibility is measured (**566 / 645** of
  FreeBASIC's keywords; 60+ unmodified Rosetta Code programs run as-is) and it is worth a great deal,
  but it is a *property, not a definition*: MODERN has commands FreeBASIC does not, and departs from
  it where FreeBASIC is demonstrably wrong.

> **Neither dialect is a clone, and the two are not the same kind of thing.** CLASSIC keeps a
> language and replaces the machine, necessarily, since the machine is gone. MODERN keeps a language
> and extends it. Each has a reference it grew from, and a compatibility figure measures how much of
> that reference's code runs here, never how complete the language is. The clearest illustration is
> [numeric output](#numeric-output-follows-the-standard-not-the-reference): where FreeBASIC's float
> rounding disagrees with IEEE 754-2019, we follow the standard. A clone could not make that choice.

### Language features

- **Core BASIC**: all data types and suffixes (`%` integer, `!`/`#` float, `$` string, plus FreeBASIC `Integer`/`Long`/`Double`/`Single`/`Byte`/`UInteger`/…), multi-dimensional arrays with arbitrary lower bounds, `DIM`/`REDIM`/array initializers/variable-length and ellipsis-sized arrays, string functions, math, date/time, `DATA`/`READ`, structured flow control.
- **MODERN / FreeBASIC**: user-defined types and **OOP** (methods, `EXTENDS` inheritance, virtual dispatch, constructors/destructors, RAII value semantics, **operator overloading** including `Cast`, aggregate/tuple initialization and anonymous `type<T>(…)` temporaries), lexical scoping, **multithreading** (threads, mutexes, condition variables), **managed and raw pointers**, `WSTRING` (UTF-16), typed `CONST`, function pointers, a preprocessor (`#define`, function-like/multi-line `#macro`, conditional compilation), and a two-dialect parser that disambiguates keywords shared with v7.
- **I/O**: console text modes (40x25 / 80x25 / 80x50), full file I/O (FreeBASIC `OPEN … FOR …` and Commodore forms), **2D graphics** (primitives, palettes, image buffers/blit, page-flipping) rendered on both the SDL2 console and, optionally, the CLI VM (`sb --window`), **interactive input** (keyboard / mouse / joystick), and optional **SID audio** emulation.

### Compilation pipeline

```
Source → Lexer → Parser (Packrat + Pratt) → AST → SSA IR
       → 16 SSA optimization passes → Bytecode → 6 bytecode passes → Register VM
```

The register-based VM uses three separate typed register banks (int / float / string) and 2-byte grouped opcodes. A differential regression net runs every corpus program both optimized and with `--no-opt` and requires identical output, guarding the optimizer.

### Native compilation

Two optional engines compile to machine code instead of interpreting, both off by default:

```bash
sb --jit program.bas     # compile eligible hot loops to native code
sb --aot program.bas     # compile eligible whole functions from the SSA, before running
```

They are checked the same way everything else is: a differential net compiles the corpus both ways and
requires identical output, so a function the AOT takes over has to agree with the interpreter **bit
for bit**, which is also the constraint that decides what it may do. Where a loop's shape allows it
the AOT emits two-lane SSE2, and that stays inside the constraint because the two lanes accumulate
independently and are combined in the same order the scalar code would have used.

### WebAssembly

`sbc --target wasm` emits a **WebAssembly module** from the same SSA the bytecode compiler uses. The
generated `.wasm` runs in a browser or under Node with no interpreter, no runtime library and no
toolchain: procedures become WASM functions (so recursion runs on the engine's own stack), registers
become locals, and the only import is a byte sink for output: the formatting of numbers is the
language's own, so a module prints exactly what `sb` prints.

```bash
sbc program.bas --target wasm     # -> program.wasm
```

Covered today: integer and floating-point arithmetic, comparisons, bitwise and shifts, control flow,
calls and recursion, `PRINT` (including correctly-rounded floats and `PRINT USING`), strings, arrays
including array parameters and `REDIM`, user-defined types, raw memory, `DIM SHARED` globals, the
transcendental functions, and 2D graphics, both the drawing primitives (`LINE`, `PSET`, `POINT`) and
direct framebuffer access through `SCREENPTR`. A program that draws produces the same framebuffer
natively and in the browser, and the page paints it by reading linear memory directly, without a
single extra import. The voxel-landscape demo in `bas/demo/` compiles and runs this way.

A program can ask which machine it is being compiled *for* with `#if __SB_WASM__`, and that question
has to be answered at compile time: the backend refuses an uncovered opcode for being **present** in
the program rather than for being reached, so a run-time test around a branch that writes files does
not keep those opcodes out of the module. The demo uses it to compile its offline video mode out of
the browser build entirely.

> **There is no deopt, and that shapes the whole design.** In a browser there is no interpreter to
> fall back into, so an opcode the backend does not cover makes the *compilation* fail with a message
> naming it and the line. The one thing it must never do is emit code that runs and lies. The target
> accepts the MODERN dialect only, and refuses CLASSIC up front rather than somewhere inside a
> formatter.

### A regular-expression engine of its own

Pattern matching does not link a third-party library: `SedaiRegexEngine` compiles a pattern to a DFA
(`SedaiAutomaton`) with an SSE2 prefix pre-filter. The target is the accepted subset behaving exactly
as PCRE2 does and everything outside it being declined cleanly, which is why the number that matters
is not one but two, the second being the acceptance rate. On the Benchmarks Game's `regex-redux` this
puts the engine level with CPython driving PCRE2.

### Numeric output follows the standard, not the reference

Where FreeBASIC compatibility and **IEEE 754-2019** disagree, SedaiBasic2 follows the standard. This
is a deliberate, measured departure and the only one of its kind.

Printing a `Double` at 16 significant digits, FreeBASIC rounds **twice**: the exact value to 17
digits, then those 17 to 16. §5.12.2 of IEEE 754-2019 asks for a conversion that is *correctly
rounded*, i.e. rounded once, and the two disagree on **4.75% of doubles** (measured over 20 706 bit
patterns). The textbook case is `1e-283`, whose nearest double is exactly
`0.999999999999999946852…e-283`: the 17th digit is a 4, so the correctly rounded answer is
`9.999999999999999e-284`, while rounding to 17 first turns `…946` into `…95` and carries it through
sixteen nines to print `1e-283`.

SedaiBasic2 prints `9.999999999999999e-284`. Its digits come from the **exact** binary value,
a double is `M × 2^E`, so the decimal expansion is an integer built by repeated multiplication, with
no floating point and no approximation anywhere, and are rounded once, half-to-even.

Real programs are unaffected: the divergence needs the extreme exponents that random bit patterns
produce. The regression corpus did not move a single baseline, and the FreeBASIC example sweep
returned exactly the counts it had before the change.

`OPTION DIGITS n` sets how many significant digits `PRINT` shows for a float (default: the dialect's
16 for a `Double`, 7 for a `Single`). Because the digits come from the exact value and are rounded
once, the count is a display choice and the rounding is not: raising it shows *more of the same
number* rather than a differently-rounded one:

```basic
Option Digits 17    : Print 0.1  '  0.10000000000000001   the round-trip form
Option Digits Exact : Print 0.1  '  0.1000000000000000055511151231257827021181583404541015625
```

`Exact` is not shorthand for "very many": a double's decimal expansion is **finite**. The value is
`M × 2^E`, so for `E ≥ 0` it is an integer and for `E < 0` it is `M × 5^(-E) / 10^(-E)`, which
terminates after exactly `-E` fractional digits. There is nothing past the end to truncate: the
widest any double gets is 751 significant digits (the smallest subnormal), and `Print 0.5` at that
setting is still `0.5`, because that is all the digits it has.

### Real-world FreeBASIC compatibility

The MODERN dialect is exercised against real programs: [`bas/rosetta/`](bas/rosetta/README.md) collects 60+ **unmodified** FreeBASIC solutions from [Rosetta Code](https://rosettacode.org), each verified to run correctly (optimized output matching `--no-opt`, deterministic, non-interactive). They are third-party works included as a mere aggregation under their original GFDL 1.2 license; see the [attribution note](bas/rosetta/README.md).

```bash
sb bas/rosetta/vector_products.bas
sb bas/rosetta/sieve_of_eratosthenes.bas
```

## Setup

> **Dependencies:** `setup.ps1` (Windows) and `setup.sh` (Linux) install everything and build. Both
> build scripts also check every dependency *before* compiling and print all the missing ones at
> once; on Linux the report ends with a single command that installs the lot. To read the list first,
> see **[INSTALL.md](INSTALL.md)**.
> ⚠️ SDL2 is the backend for **both** the graphics and the audio: without it there is no window, no
> drawing and no sound.

### Windows

#### PowerShell Execution Policy

Windows may block PowerShell scripts by default. To enable script execution, choose one of these options:

**Option 1: Run scripts individually with bypass**
```powershell
powershell -ExecutionPolicy Bypass -File .\setup.ps1
powershell -ExecutionPolicy Bypass -File .\benchmarks\benchmark.ps1
```
Note: You must use this syntax for each script you want to run.

**Option 2: Set execution policy permanently for current user (recommended)**
```powershell
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
```
After this one-time setup, all local scripts will run without restrictions.

#### Running the Setup

```powershell
# Full setup: download FPC and compile SedaiBasic2
.\setup.ps1

# Show help
.\setup.ps1 -Help
```

This will:
1. Download and install Free Pascal 3.2.2 locally, and refuse any other version
2. Install the SDL2 runtime: `SDL2.dll`, `SDL2_ttf.dll`, `SDL2_image.dll` and the console font
3. Download the SDL2 Pascal bindings into `deps\sdl2`
4. Install MinGW-w64 GCC into `deps\gcc` for the C hot loop, worth 27-45%
5. Download SedaiAudioFoundation, without which there is no sound
6. Compile SedaiBasic2 (`sb.exe`)

Every download is pinned by SHA-256. Steps 4 and 5 are never fatal: without them you get a slower
interpreter and no audio, not a failed build.

#### Setup Options

```powershell
# Only download and install FPC (do not compile)
.\setup.ps1 -FpcOnly

# Only compile SedaiBasic2 (FPC must already be installed)
.\setup.ps1 -BuildOnly

# Force FPC reinstallation
.\setup.ps1 -ForceFpc

# Clean build directories and recompile
.\setup.ps1 -Clean
```

### Linux

All five targets build and run on Linux (verified on Debian 13 with FPC 3.2.2, x86_64). `./setup.sh`
works out what is missing, prints one command that installs all of it, fetches the bindings and
SedaiAudioFoundation, and builds.

On Debian and Ubuntu the packages are:

```bash
sudo apt install fpc gcc libsdl2-dev libsdl2-ttf-dev libsdl2-image-dev
```

⛔ **The distribution has to be recent enough**, and it is the libraries that decide: the bindings
need **SDL2 2.30.0** and **SDL2_ttf 2.22.0** or newer, so Debian 13 and Ubuntu 24.04 are in, Debian 12
and Ubuntu 22.04 are out. Free Pascal must be **exactly 3.2.2**; 3.3.1 does not compile SedaiBasic.
Only `sdl2` and `sdl2_ttf` bindings are actually used, so there is no need for `libsdl2-mixer-dev`,
`libsdl2-net-dev` or `libsdl2-gfx-dev`.

> **A C compiler is used, and it is worth having.** The interpreter's hot dispatch arms are
> compiled by `gcc` or `clang` (`src/hotdisp.c`) and linked into the binary, which is worth 27-45%
> on arithmetic-heavy programs. This is **on by default**; without a C compiler the build still
> succeeds and says so, and `--no-hot-c` turns it off explicitly. Cross-building for Windows needs
> `gcc-mingw-w64-x86-64-win32` (Debian/Ubuntu) and is picked up automatically; win32 is not
> supported for this: its calling convention decorates symbol names differently from win64.

```bash
./setup.sh
```

### Build System

SedaiBasic2 includes cross-platform build scripts for compiling all targets.

#### Build Targets

| Target | Description | Output |
|--------|-------------|--------|
| sb | SedaiBasic VM (interpreter) | sb |
| sbc | SedaiBasic Compiler (bytecode, or WebAssembly with `--target wasm`) | sbc |
| sbd | SedaiBasic Disassembler | sbd |
| sbv | SedaiVision (SDL2 graphical) | sbv |
| sbw | Web BASIC HTTP server (see [WEB_BASIC.md](WEB_BASIC.md)) | sbw |

Binaries are written to `bin/<cpu>-<os>/`, and carry the `.exe` extension on Windows only, so the
same target is `bin/x86_64-win64/sb.exe` there and `bin/x86_64-linux/sb` here. Compiled units live
alongside in `lib/<cpu>-<os>/`.

`build.ps1 -Target sb -Window` (`./build.sh sb --window`) adds an opt-in SDL2 window to the
command-line VM, so FreeBASIC/C128 graphics are visible without `sbv`. Note that a later build of
`sb` **without** that switch overwrites it with the headless build, where `--window` is accepted and
silently ignored.

#### Windows (PowerShell)

```powershell
# Build all targets
.\build.ps1

# Build specific target
.\build.ps1 -Target sb

# Build with debug info
.\build.ps1 -Debug

# Clean and rebuild
.\build.ps1 -Clean

# Build for different platform
.\build.ps1 -CPU x86_64 -OS win64
```

#### Linux (Bash)

```bash
# Build all targets
./build.sh

# Build specific target
./build.sh sb

# Build with debug info
./build.sh --debug

# Clean and rebuild
./build.sh --clean

# CLI VM with the opt-in SDL2 window presenter (sb --window)
./build.sh sb --window

# Build without SedaiAudioFoundation, or against a specific copy of it
./build.sh --with-sedai-audio no
./build.sh --with-sedai-audio /path/to/SedaiAudioFoundation

# Per-pass debug output at compile time
./build.sh sb --debug-flags SSA,REGALLOC

# Cross-target selection
./build.sh --cpu x86_64 --os linux
```

The build reports which instruction set it chose (AVX2 + FMA, AVX, or the portable x86-64 baseline)
from what the CPU actually supports. Set `SEDAI_CPUOPT=none` to force the baseline when the binaries
must run on an older machine than the one building them, or `=avx` / `=avx2` to pin a level.

If FPC is not on `PATH`, the script also looks in `$SEDAI_FPC`, `setup.config.json`, a project-local
`fpc/3.2.2/`, and under your home directory before giving up.

### Manual Installation

If you prefer to install Free Pascal separately:

1. **Download FPC** from [https://www.freepascal.org/download.html](https://www.freepascal.org/download.html)
2. **Install** following the instructions for your platform
3. **Compile** using the build scripts above, or manually:

```bash
# Example: compile sb (SedaiBasic VM)
fpc -o"sb.exe" -Px86_64 -Twin64 -MObjFPC -O1 \
    -CpCOREAVX2 -OpCOREAVX2 -CfAVX2 \
    -OoREGVAR -OoCSE -OoDFA -OoFASTMATH -OoCONSTPROP \
    -Xs -XX -Fusrc -Fulib/x86_64-win64 -FUlib/x86_64-win64 \
    -FEbin/x86_64-win64 src/SedaiBasicVM.lpr
```

## Applications

SedaiBasic2 provides four applications for different use cases:

### SedaiBasic VM (Command Line Interpreter)

The main interpreter for running BASIC programs from the command line.

```bash
sb [options] <program.bas>

Options:
  --help              Show this help message
  --verbose           Show loading, lexing, parsing, and VM execution info
  --dump-ast          Show AST structure after parsing
  --disasm            Show bytecode disassembly (after superinstruction fusion)
  --disasm-pre        Show bytecode BEFORE superinstruction fusion
  --no-exec           Compile only, do not execute (useful with --disasm)
  --stats             Show execution statistics
  --no-opt            Skip the SSA/bytecode optimization passes (differential testing)
  --jit               Compile eligible hot loops to native code
  --aot               Compile eligible whole functions to native code before running
  --bounds-check      Hard-error on out-of-bounds array access (like FreeBASIC's -exx)
  --true-value=N      TRUE for comparisons: -1 (Commodore, default) or 1
  --date-locale       Month/day names and date parsing follow the system locale, as fbc does
                      (the default is deterministic: English names, the same on every machine)
  --window            Show FreeBASIC/C128 graphics in an SDL2 window (needs a -Window build)
```

A program can also be given arguments, which reach it through `COMMAND$`:

```bash
sb program.bas arg1 arg2
```

**Examples:**
```bash
# Run a program
sb bas\SIEVE.BAS

# Show disassembly without executing
sb --disasm --no-exec program.bas

# Run with execution statistics
sb --stats program.bas
```

### SedaiBasic Compiler

Compiles BASIC source code to bytecode without executing. Useful for syntax checking and pre-compilation.

```bash
sbc <source.bas> [output.basc] [options]

Options:
  --help, -h          Show this help message
  --verbose, -v       Show compilation details
  --quiet, -q         Suppress all output except errors
  --target wasm       Emit a WebAssembly module (.wasm) instead of bytecode.
                      An opcode the backend does not cover is REFUSED with a message
                      naming it and the line: in the browser there is no interpreter
                      to fall back into, so it must never emit code that runs and lies.
```

**Examples:**
```bash
# Compile a program to bytecode
sbc program.bas

# Compile with custom output name
sbc program.bas compiled.basc

# Emit a WebAssembly module
sbc program.bas --target wasm
```

### SedaiBasic Disassembler

Disassembles compiled bytecode files (.basc) to human-readable format.

```bash
sbd [options] <program.basc>

Options:
  --help              Show this help message
  --verbose           Show additional bytecode details
```

**Examples:**
```bash
# Disassemble a compiled file
sbd compiled.basc
```

### SedaiVision (SDL2 Graphical Console)

Interactive graphical interpreter with SDL2-based console emulating C64/C128 display modes.

```bash
sbv [options] [program.bas]

Options:
  --help              Show this help message
  --fullscreen        Start in fullscreen mode
  --mode <n>          Set initial graphics mode (0-11)
```

**Features:**
- C64/C128 compatible text modes (40x25, 80x25, 80x50)
- Bitmap and multicolor graphics modes
- SDL2 dynamic resolution support
- Scrollback buffer with keyboard navigation
- Command history

**Graphics Modes:**

| Mode | Resolution | Description |
|------|------------|-------------|
| 0 | 320x200 | 40x25 text mode (C64/C128 compatible) |
| 1 | 320x200 | Standard bitmap mode |
| 2 | 320x200 | Split screen: bitmap + text |
| 3 | 160x200 | Multicolor bitmap (double-width pixels) |
| 4 | 160x200 | Split screen: multicolor + text |
| 5 | 640x200 | 80x25 text mode (C128 compatible) |
| 6 | 640x200 | 640x200 hires bitmap |
| 7 | 640x200 | Split screen: 640x160 bitmap + 80x5 text |
| 8 | 640x400 | 80x50 text mode |
| 9 | 640x400 | 640x400 hires bitmap |
| 10 | 640x400 | Split screen: 640x360 bitmap + 80x5 text |
| 11 | Variable | SDL2 dynamic resolution (use GLIST for available modes) |

See [CONSOLE.md](CONSOLE.md) for keyboard shortcuts and detailed graphics mode documentation.

**Examples:**
```bash
# Start interactive console
sbv

# Run a program in graphical mode
sbv program.bas

# Start fullscreen
sbv --fullscreen
```

## Running BASIC Programs

After setup, run a BASIC program with:

```bash
# Windows (command line)
.\bin\x86_64-win64\sb program.bas

# Windows (graphical)
.\bin\x86_64-win64\sbv program.bas

# Linux
./bin/x86_64-linux/sb program.bas
```

### Example

```bash
.\bin\x86_64-win64\sb bas\SIEVE.BAS
```

## Benchmarking

Run the benchmark suite to measure interpreter performance:

```powershell
# Run benchmarks with standard N values (may take 10-30 minutes)
.\benchmarks\benchmark.ps1

# Quick test with N values from source files
.\benchmarks\benchmark.ps1 -Quick

# Run 3 times in this session (results accumulate)
.\benchmarks\benchmark.ps1 -Runs 3

# Run only some of them
.\benchmarks\benchmark.ps1 -Only n-body,spectral-norm

# Run only one language's implementation (sedai, python, lua)
.\benchmarks\benchmark.ps1 -Runtime sedai

# Force re-run, ignoring cached session results
.\benchmarks\benchmark.ps1 -Force

# Generate report from existing history (no benchmark run)
.\benchmarks\benchmark.ps1 -Report

# Use custom output filename
.\benchmarks\benchmark.ps1 -Output "results.md"

# Show help
.\benchmarks\benchmark.ps1 -Help
```

The suite is the whole of [The Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/), run at the **official N** and compared against the reference Python and Lua implementations of the same programs:

| Benchmark | Description | N (standard) |
|-----------|-------------|--------------|
| binary-trees | Allocate and traverse many binary trees | 21 |
| fannkuch-redux | Indexed access to a tiny integer sequence | 12 |
| fasta | Generate and write DNA sequences | 25,000,000 |
| k-nucleotide | Hashtable update and k-nucleotide strings | 25,000,000 (stdin) |
| mandelbrot | Generate a Mandelbrot set bitmap | 16,000 |
| n-body | Double-precision N-body simulation | 50,000,000 |
| pidigits | Streaming arbitrary-precision arithmetic | 10,000 |
| regex-redux | Match DNA 8-mers and substitute magic patterns | 5,000,000 (stdin) |
| reverse-complement | Read DNA sequences and write their reverse-complement | 25,000,000 (stdin) |
| spectral-norm | Eigenvalue using the power method | 5,500 |

Measured results are in [BENCHMARK.md](BENCHMARK.md). Two of these have no runnable reference on a
stock Windows box, and `k-nucleotide`'s Python version needs a working `multiprocessing` fork, so Lua
answers for it instead: the runner says which reference it used rather than quietly dropping one.

#### Cumulative Statistics

Each benchmark run is saved to a history file. Statistics (mean, median, standard deviation, percentiles) are calculated using **all accumulated runs** over time. This allows you to build reliable statistics by running the benchmark multiple times across different sessions, rather than requiring 20+ consecutive runs.

**Important:** Runs are only committed to history when **all 3 benchmarks** complete successfully in a session. If a session is interrupted, runs remain pending and will not affect the cumulative statistics until the session is completed.

#### Session Resume

If a benchmark session is interrupted (e.g., user cancels or a benchmark fails), the next run will automatically detect the incomplete session and resume from where it left off, running only the remaining benchmarks.

Use `-Quick` to run with N values from source files instead of standard values.

Results are saved to `benchmarks/results/BENCHMARKS.md` (or custom file with `-Output`) and displayed on screen.

## License

Copyright (C) 2025 Maurizio Cammalleri

This program is free software: you can redistribute it and/or modify it under the terms of the **GNU General Public License v3** as published by the Free Software Foundation.

See the [LICENSE](LICENSE) file for the complete license text.

### Commercial Licensing

For commercial licensing inquiries, please contact the author:

**Maurizio Cammalleri**
Email: maurizio.cammalleri@gmail.com
