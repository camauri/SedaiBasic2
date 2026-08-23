# The `sedai-gcc` package

`setup.ps1` installs a MinGW-w64 GCC into `deps\gcc`, used for one thing only: compiling
`src/hotdisp.c` into the object the interpreter's hot dispatch arms live in. This file records what
that package is, where it comes from, and how to rebuild it when GCC is bumped.

## What it is

**A subset of a [WinLibs](https://winlibs.com/) build, repacked. It is not our work.** WinLibs is
Brecht Sanders' standalone MinGW-w64 GCC distribution for Windows; the binaries in our package are
his, unmodified — only the *selection* is ours.

| | |
|---|---|
| Upstream | WinLibs — https://winlibs.com/ — by Brecht Sanders |
| Upstream archive | `winlibs-x86_64-posix-seh-gcc-14.2.0-mingw-w64msvcrt-12.0.0-r3.zip` |
| Upstream SHA-256 | `ff475e985a98c5f3785129baf7460db14fee27708bce35f2833db5009507f1b9` |
| Variant | GCC 14.2.0, x86_64, POSIX threads, SEH, MSVCRT runtime, without LLVM/Clang/LLD/LLDB |
| Our archive | `sedai-gcc-14.2.0-x86_64-win64.zip`, 18.5 MB (upstream: 247 MB) |
| Our SHA-256 | `0432ffe1f877b22ca5c81a98b4983736af173ded4bc4750b4f19bd74b56acaea` |
| Unpacked | 47 MB, 151 files, under a `gcc/` root (upstream unpacks to 875 MB) |

## Why it is so much smaller

The build **never links**. It runs `gcc -c` and hands the object to FPC's `{$L}`. So the linker, the
C runtime, the import libraries and the 121 MB of `lib/` are all dead weight — and with
`-ffreestanding`, so are the 84 MB of mingw-w64 headers: asking the compiler itself
(`gcc -ffreestanding -M -c hotdisp.c`) shows that `hotdisp.c` needs exactly **two** headers, both
internal to GCC.

What is kept, and nothing else:

- `gcc/bin/gcc.exe` — the driver
- `gcc/bin/as.exe` — the assembler (`-c` goes through it)
- `gcc/libexec/gcc/x86_64-w64-mingw32/14.2.0/cc1.exe` — the C compiler proper, 34 MB of the 47
- `gcc/lib/gcc/x86_64-w64-mingw32/14.2.0/include/` — GCC's own headers (140 files, 2.6 MB)
- ten DLLs in `gcc/bin/`, the **transitive** import closure of those three executables:
  `libgcc_s_seh-1`, `libgmp-10`, `libiconv-2`, `libintl-8`, `libisl-23`, `libmpc-3`, `libmpfr-6`,
  `libwinpthread-1`, `libzstd`, `zlib1`

⛔ **Transitive, and that is not a detail.** The first attempt read the imports of the three
executables only and produced a package where `cc1.exe` would not start: `libmpfr-6.dll` itself
imports `libgcc_s_seh-1.dll`. The failure is silent — gcc exits 1 and prints nothing.

⛔ **`gcc\bin` must be on the PATH when the compiler is invoked**, even when `gcc.exe` is called by
absolute path. `cc1.exe` lives in `libexec\` while the DLLs both it and the driver import live in
`bin\`, and Windows resolves a DLL against the directory of the executable that *needs* it, not the
one that launched it. `build.ps1` and `install-gcc.ps1` both prepend it for the duration of the call.

## How it was verified

Every claim above was measured on Linux, with `wine` running the Windows binaries:

1. The transitive DLL closure came from `x86_64-w64-mingw32-objdump -p`, iterated to a fixed point.
2. The header set came from the packaged compiler itself: `gcc -ffreestanding -M -c hotdisp.c`.
3. The package compiles `hotdisp.c` end-to-end, extracted fresh from the zip.
4. ⭐ The object it produces is **byte-identical** to the one the FULL WinLibs extraction produces.
   That is the check that says the trimming removed nothing that mattered.
5. The object's undefined symbols are exactly `sb_hot_sin`, `sb_hot_cos`, `sb_hot_tan` — ours — and
   nothing from the C runtime.

## Rebuilding it for a new GCC

1. Download the WinLibs archive for the version you want (any x86_64 variant: UCRT vs MSVCRT, SEH vs
   SJLJ and POSIX vs win32 threads all describe *link-time* behaviour, and we never link).
2. Extract the paths listed above, with `14.2.0` replaced by the new version.
3. Recompute the DLL closure — it can change between GCC releases.
4. Rename the `mingw64` root to `gcc` and zip it.
5. Compile `src/hotdisp.c` with it and compare the object against one built from the full archive.
6. Update `$GCC_VERSION`, `$EXPECTED_HASH` and this file, and cut the release.

⚠️ Re-measure the benchmarks on a much newer GCC. `-falign-labels=32 -falign-jumps=32
-fno-crossjumping` are exactly the options whose value shifts between compiler versions, and the
27–45% figure was measured, not derived.

## Licences

The binaries are third-party software and carry their own terms:

- **GCC** and **binutils** (`gcc.exe`, `cc1.exe`, `as.exe`): GPL-3.0-or-later, GCC with the
  [GCC Runtime Library Exception](https://www.gnu.org/licenses/gcc-exception-3.1.html)
- **GMP**, **MPFR**, **MPC**: LGPL-3.0-or-later · **ISL**: MIT · **zlib**: zlib licence ·
  **zstd**: BSD-3-Clause or GPL-2.0 · **libiconv**, **libintl** (gettext): LGPL-2.1-or-later ·
  **libwinpthread**, **libgcc_s_seh**: MinGW-w64 runtime licences

⛔ **Redistributing GCC carries the GPL source obligation.** The release that hosts this package must
accompany it with the corresponding source, or a written offer valid for three years. In practice:
link the exact upstream source from the release notes and keep a copy. The hashes above identify
precisely which build the binaries came from.
