# SedaiBasic2 Interpreter with VM

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![LinkedIn](https://img.shields.io/badge/LinkedIn-Maurizio%20Cammalleri-0077B5?logo=linkedin)](https://www.linkedin.com/in/maurizio-cammalleri-80a89a11/)

```
   ____           _       _ ____            _      ____
  / ___|  ___  __| | __ _(_) __ )  __ _ ___(_) ___|___ \
  \___ \ / _ \/ _` |/ _` | |  _ \ / _` / __| |/ __| __) |
   ___) |  __/ (_| | (_| | | |_) | (_| \__ \ | (__ / __/
  |____/ \___|\__,_|\__,_|_|____/ \__,_|___/_|\___|_____|
```

## What is SedaiBasic2?

SedaiBasic2 is a modern reimplementation of Commodore BASIC v7. At the current stage of development, it supports the **Tiny BASIC** subset plus:

- Multi-dimensional arrays
- Integer type variables (suffix `%`)
- High-performance register-based bytecode VM

The interpreter features a complete compilation pipeline: Lexer, Parser, SSA IR optimizer, and bytecode compiler targeting a fast register-based virtual machine.

## Setup

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
1. Download and install Free Pascal Compiler (FPC) 3.2.2 locally
2. Compile SedaiBasic2 (`sb.exe`)

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

> **Note:** Linux support is currently under development.

```bash
./setup.sh
```

### Manual Installation

If you prefer to install Free Pascal separately:

1. **Download FPC** from [https://www.freepascal.org/download.html](https://www.freepascal.org/download.html)
2. **Install** following the instructions for your platform
3. **Compile** SedaiBasic2 manually:

```bash
fpc -Px86_64 -Twin64 -MObjFPC -Scghi -CX -Si -O3 \
    -CpCOREAVX2 -OpCOREAVX2 -CfAVX2 \
    -OoREGVAR -OoCSE -OoDFA -OoFASTMATH -OoCONSTPROP \
    -Xs -XX -Fusrc -FUlib/x86_64-win64 -FEbin/x86_64-win64 \
    -osb.exe src/SedaiBasicVM.lpr
```

## Running BASIC Programs

After setup, run a BASIC program with:

```bash
# Windows
.\bin\x86_64-win64\sb.exe program.bas

# Linux
./bin/x86_64-linux/sb program.bas
```

### Example

```bash
.\bin\x86_64-win64\sb.exe bas\SIEVE.BAS
```

### Command Line Options

```
sb.exe [options] <program.bas>

Options:
  --help              Show this help message
  --verbose           Show loading, lexing, parsing, and VM execution info
  --dump-ast          Show AST structure after parsing
  --disasm            Show bytecode disassembly
  --no-exec           Compile only, do not execute (useful with --disasm)
  --stats             Show execution statistics
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

# Force re-run, ignoring cached session results
.\benchmarks\benchmark.ps1 -Force

# Clear all accumulated history and start fresh
.\benchmarks\benchmark.ps1 -ClearHistory

# Generate report from existing history (no benchmark run)
.\benchmarks\benchmark.ps1 -Report

# Use custom output filename
.\benchmarks\benchmark.ps1 -Output "results.md"

# Show help
.\benchmarks\benchmark.ps1 -Help
```

The benchmark suite runs programs from [The Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/):

| Benchmark | Description | N (standard) |
|-----------|-------------|--------------|
| fannkuch-redux | Indexed-access to tiny integer-sequence | 12 |
| n-body | Double-precision N-body simulation | 50,000,000 |
| spectral-norm | Eigenvalue using the power method | 5,500 |

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
