# SedaiBasic2 Benchmark Results

```
   ____           _       _ ____            _      ____
  / ___|  ___  __| | __ _(_) __ )  __ _ ___(_) ___|___ \
  \___ \ / _ \/ _  |/ _  | |  _ \ / _  / __| |/ __| __) |
   ___) |  __/ (_| | (_| | | |_) | (_| \__ \ | (__ / __/
  |____/ \___|\__,_|\__,_|_|____/ \__,_|___/_|\___|_____|
```

**Generated:** 2025-12-05 03:25:05
**Mode:** Standard (Benchmarks Game N values)
**Total runs:** fannkuch-redux: 1, n-body: 1, spectral-norm: 1

## System Information

| Property | Value |
|----------|-------|
| **OS** | Microsoft Windows 11 Enterprise |
| **OS Version** | 10.0.22621 |
| **Architecture** | 64 bit |
| **CPU** | Intel(R) Core(TM) i7-3630QM CPU @ 2.40GHz |
| **CPU Cores** | 4 |
| **CPU Threads** | 8 |
| **CPU Max Clock** | 2,40 GHz |
| **RAM** | 15,9 GB |
| **Interpreter** | SedaiBasic2 (register-based bytecode VM) |

## Benchmark Results

| Benchmark | N | Execution Time | Instructions | Speed (MIPS) |
|-----------|--:|---------------:|-------------:|-------------:|
| fannkuch-redux | 12 | 494425,244 ms |  55.935.814.676 | 113,13 |
| n-body | 50.000.000 | 88409,743 ms | 12.400.000.373 | 140,26 |
| spectral-norm | 5.500 | 70084,641 ms | 9.681.897.749 | 138,15 |

## Python Comparison

| Benchmark | SedaiBasic2 | Python (1T) | Speedup (1T) | Python (MT) | Speedup (MT) |
|-----------|------------:|------------:|-------------:|------------:|-------------:|
| fannkuch-redux | 494425,244 ms | 1523821,101 ms | 3,08x | 555082,457 ms | 1,12x |
| n-body | 88409,743 ms | 523862,617 ms | 5,93x | - | - |
| spectral-norm | 70084,641 ms | 447755,083 ms | 6,39x | 201019,289 ms | 2,87x |

## Lua Comparison

| Benchmark | SedaiBasic2 | Lua 5.4 | Speedup |
|-----------|------------:|--------:|--------:|
| fannkuch-redux | 494425,244 ms | 518408,784 ms | 1,05x |
| n-body | 88409,743 ms | 223514,099 ms | 2,53x |
| spectral-norm | 70084,641 ms | 71777,855 ms | 1,02x |

## Benchmark Descriptions

| Benchmark | Description |
|-----------|-------------|
| fannkuch-redux | Indexed-access to tiny integer-sequence (pancake flipping) |
| n-body | Double-precision N-body simulation (planetary orbits) |
| spectral-norm | Eigenvalue computation using the power method |

## Notes

- Benchmarks from [The Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/)
- Times measured using SedaiBasic2's built-in `--stats` option
- MIPS = Million Instructions Per Second (VM bytecode instructions)
- Results may vary based on system load and thermal throttling
- **Statistical measures** (when using -Runs > 1):
  - **Mean**: Arithmetic average of all runs
  - **Median**: Middle value (more robust to outliers)
  - **Std Dev**: Standard deviation (sample, n-1)
  - **P90/P95/P99**: Percentiles indicating the time below which 90%/95%/99% of runs completed
