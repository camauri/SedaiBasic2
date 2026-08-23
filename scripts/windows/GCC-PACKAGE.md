# The gcc package

`setup.ps1` installs a MinGW-w64 GCC into `deps\gcc`, used only to compile `src/hotdisp.c`.

**It is a subset of a [WinLibs](https://winlibs.com/) build by Brecht Sanders, repacked. The binaries
are his, unmodified — only the selection is ours.**

| | |
|---|---|
| Release | `gcc-14.2.0-x86_64-win64` on `camauri/SedaiBasic2-Deps` |
| Our archive | `gcc-14.2.0-x86_64-win64.zip`, 18.5 MB, 47 MB unpacked, `gcc/` root |
| Our SHA-256 | `0432ffe1f877b22ca5c81a98b4983736af173ded4bc4750b4f19bd74b56acaea` |
| Upstream | `winlibs-x86_64-posix-seh-gcc-14.2.0-mingw-w64msvcrt-12.0.0-r3.zip` (247 MB, 875 unpacked) |
| Upstream SHA-256 | `ff475e985a98c5f3785129baf7460db14fee27708bce35f2833db5009507f1b9` |

## Contents

The build never links — it runs `gcc -c` — so the linker, the CRT, the import libraries and, under
`-ffreestanding`, the 84 MB of mingw-w64 headers are all unnecessary. `hotdisp.c` needs two headers,
both internal to GCC.

- `gcc/bin/gcc.exe`, `gcc/bin/as.exe`
- `gcc/libexec/gcc/x86_64-w64-mingw32/14.2.0/cc1.exe` (34 of the 47 MB)
- `gcc/lib/gcc/x86_64-w64-mingw32/14.2.0/include/`
- ten DLLs in `gcc/bin/`: `libgcc_s_seh-1`, `libgmp-10`, `libiconv-2`, `libintl-8`, `libisl-23`,
  `libmpc-3`, `libmpfr-6`, `libwinpthread-1`, `libzstd`, `zlib1`

⛔ The DLL list is the **transitive** closure. Taking only the executables' own imports leaves out
`libgcc_s_seh-1.dll`, which `libmpfr-6.dll` needs, and then `cc1.exe` will not start — silently: gcc
exits 1 and prints nothing.

⛔ `gcc\bin` must be on the PATH when gcc is invoked, even by absolute path: `cc1.exe` sits in
`libexec\` while its DLLs sit in `bin\`, and Windows resolves them against the directory of the
executable that needs them. `build.ps1` and the installer's probe both prepend it.

## Rebuilding for a new GCC

1. Take any x86_64 WinLibs archive (UCRT/MSVCRT, SEH/SJLJ, posix/win32 threads all describe
   link-time behaviour, which we never reach).
2. Extract the paths above with the new version number; recompute the DLL closure, it can change.
3. Rename the `mingw64` root to `gcc`, zip it, update `$GCC_VERSION` and `$EXPECTED_HASH`.
4. Compile `hotdisp.c` with it and compare the object against one built from the full archive: they
   must be byte-identical. That check is what says the trimming removed nothing.

⚠️ Re-measure the benchmarks on a much newer GCC: `-falign-labels=32 -falign-jumps=32
-fno-crossjumping` are exactly the options whose value shifts between compiler versions.

## Licences

GCC and binutils: GPL-3.0-or-later, GCC with the
[Runtime Library Exception](https://www.gnu.org/licenses/gcc-exception-3.1.html). GMP, MPFR, MPC:
LGPL-3.0-or-later. ISL: MIT. zlib: zlib. zstd: BSD-3-Clause or GPL-2.0. libiconv, libintl:
LGPL-2.1-or-later. libwinpthread, libgcc_s_seh: MinGW-w64 runtime licences.

⛔ Redistributing GCC carries the GPL source obligation: the release links the corresponding source
(https://ftp.gnu.org/gnu/gcc/gcc-14.2.0/) and the hashes above identify which build the binaries
came from.
