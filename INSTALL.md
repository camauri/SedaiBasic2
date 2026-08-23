# Building SedaiBasic2

There are no binary releases yet, so building is currently the only way in. This document lists
everything the build needs, per platform, and how to get it.

**You should not need this document.** `./build.sh` (Linux/macOS) and `.\build.ps1` (Windows) check
every dependency *before* compiling anything and print all the missing ones at once, with a single
command that installs them. This file is the long form: what each dependency is for, and what to do
when the automatic suggestion does not fit your system.

---

## What the build actually needs

**SDL2 is not an accessory: it is the backend for BOTH the graphics and the audio.** Without
`libSDL2` there is no window, no drawing and no sound — `SCREEN`, `PSET`, `LINE`, `PLAY`, `SOUND` and
the SID emulation all end there. Only a build that is text-only *and* has no audio library can do
without it, and that is the exception, not the normal case.

| | Needed for | When |
|---|---|---|
| **Free Pascal 3.2.2** | everything | always |
| **libSDL2** + **libSDL2_ttf** | **graphics AND audio** — the window, every drawing primitive, every sound | development files to build `sbv` and any target with audio; runtime files to run anything that draws or plays |
| **GCC** (MinGW-w64 on Windows) | the C hot loop, worth **27–45%** | optional — without it the build succeeds and the interpreter is slower |
| **SDL2 Pascal bindings** | compiling the SDL2 units | downloaded into `deps/sdl2`; they are not in the repository |
| **ALSA** (`libasound.so.2`) | MIDI input | optional, opened at run time; arrives as a dependency of `libsdl2-dev` |
| **SedaiAudioFoundation** | the audio subsystem itself | optional, auto-detected; it drives SDL2's audio device |
| **SDL2_image** | nothing yet | installed on purpose: it sits beside the Windows binaries, so the day something uses it nothing has to be installed again |

⭐ **The Pascal bindings are platform independent**: 52 text files, `.pas` and `.inc`, no binaries,
every platform difference behind an `{$IFDEF}`. Windows, Linux and macOS download the very same
archive, with the very same checksum. `setup.sh` and `setup.ps1` both fetch it.

⚠️ **When SDL2 is needed to LINK depends on what was detected.** `sbv` always links it. With
SedaiAudioFoundation present the audio backend pulls it in, so even the plain `sb` links `libSDL2`.
`sb --window` and the audio backend can also load it at RUN time (`LoadLibrary`), so a build can
succeed and then find nothing to draw on. This is why the build reports what *your* configuration
needs rather than a fixed list — and why the runtime libraries are worth installing even when the
build does not strictly demand them.

---

## Linux

`./setup.sh` does all of this for you: it works out what is missing, shows one command that installs
all of it, and builds. What follows is the same thing by hand.

⛔ **The distribution has to be recent enough, and it is the LIBRARIES that decide.** The SDL2 Pascal
bindings declare every entry point as an ordinary external, so a distribution whose SDL2 is older
than the bindings leaves undefined symbols: `sbv` fails to link, or fails to start. The minimums are
**SDL2 2.30.0** and **SDL2_ttf 2.22.0**, and Free Pascal must be **exactly 3.2.2** (3.3.1 does not
compile SedaiBasic). Checked against the archives on 23 August 2026:

| | Free Pascal | SDL2 | SDL2_ttf | |
|---|---|---|---|---|
| Debian 13 (trixie) | 3.2.2 | 2.32.4 | 2.24.0 | supported |
| Ubuntu 24.04 LTS (noble) | 3.2.2 | 2.30.0 | 2.22.0 | supported, exactly at the minimum |
| Debian 12 (bookworm) | 3.2.2 | 2.26.5 | 2.20.1 | **too old** |
| Ubuntu 22.04 LTS (jammy) | 3.2.2 | 2.0.20 | 2.0.18 | **too old** |

`setup.sh` checks what `apt` would install *before* installing it, and stops with the offending
versions named, because finding out afterwards looks like our build is broken rather than the
distribution being behind.

### Debian 13 and later, Ubuntu 24.04 and later

```sh
sudo apt install fpc gcc libsdl2-dev libsdl2-ttf-dev libsdl2-image-dev
```

FreeType, libpng, libjpeg, zlib and ALSA are not listed because they arrive as dependencies:
`libsdl2-dev` pulls `libasound2-dev`, `libsdl2-ttf-dev` pulls `libfreetype-dev`, and
`libsdl2-image-dev` pulls `libjpeg-dev` and `libpng-dev`, which pulls `zlib1g-dev`.

### Fedora, RHEL, Rocky, Alma

```sh
sudo dnf install fpc gcc SDL2-devel SDL2_ttf-devel
```

### Arch, Manjaro

```sh
sudo pacman -S fpc gcc sdl2 sdl2_ttf
```

### openSUSE

```sh
sudo zypper install fpc gcc libSDL2-devel libSDL2_ttf-devel
```

### Alpine

```sh
sudo apk add fpc gcc sdl2-dev sdl2_ttf-dev
```

Then:

```sh
./build.sh sb          # the CLI VM — the default target and the regression one
./build.sh all         # everything: sb, sbc, sbd, sbv, sbw
./build.sh sb --window # sb with the optional SDL2 window presenter
```

### Free Pascal, when the distribution's is too old

Either [fpcupdeluxe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe) (it installs FPC and Lazarus
side by side with whatever the system has, and writes a working `fpc.cfg`), or the official tarball
from [freepascal.org](https://www.freepascal.org/download.html). The build looks for a compiler in
`fpc/3.2.2/`, `~/tools/fp/*/fpc/`, `~/fpcupdeluxe/`, and on the `PATH`, and asks which one to use if
it finds several.

---

## Windows

**We do not ship or download the SDL2 DLLs.** They belong to the SDL project, they are signed by
nobody in this chain, and packaging someone else's binaries is not something this project wants to be
responsible for outside a real, digitally signed installer. Get them from the source below; it takes
a minute and you know what you are running.

### 1. Free Pascal 3.2.2

[freepascal.org/download.html](https://www.freepascal.org/download.html), or
[fpcupdeluxe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe) if you also want Lazarus.

⚠️ Whichever you choose, make sure a **usable `fpc.cfg`** ends up next to the compiler. It is the one
thing that most often goes wrong, and the symptom is confusing: the compiler works perfectly inside
Lazarus — which supplies its own unit paths and never reads `fpc.cfg` — and fails from a plain shell
with `Fatal: Can't find unit system used by Program`. You can check in ten seconds: put `begin end.`
in `probe.pas` and run `fpc probe.pas`. If it fails, generate the config:

```
<fpcroot>\bin\x86_64-win64\fpcmkcfg.exe -d basepath=<fpcroot> -o <fpcroot>\bin\x86_64-win64\fpc.cfg
```

### 2. GCC — the C hot loop

The hot dispatch arms (`src/hotdisp.c`) are compiled by a C compiler, not by FPC, and are worth
27–45% where they apply. `setup.ps1` installs one into `deps\gcc` and `build.ps1` picks it up from
there; the build also accepts a `gcc.exe` on the `PATH`, or one named by `SEDAI_CC`.

The package it installs is an 18.5 MB subset of a [WinLibs](https://winlibs.com/) build — Brecht
Sanders' standalone MinGW-w64 GCC for Windows — carrying only what `gcc -c` needs. What is in it,
how it was verified and its licences are in
[scripts/windows/GCC-PACKAGE.md](scripts/windows/GCC-PACKAGE.md).

⛔ **GCC, not "a C compiler".** The flag set is GCC's and it is not decoration: `-fno-crossjumping`
alone is worth spectral-norm −16.1%, because it stops the compiler merging the replicated dispatch
tails that give every arm its own branch-predictor history. Microsoft's compiler has no equivalent
spelling. If you install one yourself, use [w64devkit](https://github.com/skeeto/w64devkit) (one zip,
no installer) or [MSYS2](https://www.msys2.org/) (`pacman -S mingw-w64-x86_64-gcc`).

⭐ **Only the compiler proper is needed.** The build never *links* with it — it runs `gcc -c` and
hands the object to FPC — so no linker, no CRT and no import libraries are involved. Measured: the
object it produces has three undefined symbols, all of them ours.

⭐ **Which MinGW flavour does not matter.** UCRT vs MSVCRT, SEH vs SJLJ, POSIX vs win32 threads —
every one of those distinguishes *link-time and runtime* behaviour, and we never link. Verified by
building the object with a win32-threads toolchain: same three symbols, nothing from the CRT. Take
whatever x86_64 build is convenient. ⚠️ The one thing worth re-checking on a much newer GCC is not
correctness but the *gain*: `-falign-labels=32 -falign-jumps=32 -fno-crossjumping` are exactly the
options whose value shifts between compiler versions, so re-measure rather than assume the 27–45%.

Without it the build still succeeds and says so; you get a slower interpreter, not a failure. Pass
`-NoHotC` to leave it out on purpose, or `-HotC` to make a missing compiler an error instead of a
note.

### 3. SDL2 and SDL2_ttf — for graphics and for audio

From the official releases:

- SDL2: [github.com/libsdl-org/SDL/releases](https://github.com/libsdl-org/SDL/releases) — the
  `SDL2-devel-<version>-VC.zip` or `-mingw.zip` archive
- SDL2_ttf: [github.com/libsdl-org/SDL_ttf/releases](https://github.com/libsdl-org/SDL_ttf/releases)

Put `SDL2.dll` and `SDL2_ttf.dll` (and the DLLs shipped beside `SDL2_ttf.dll`) next to the built
executable in `bin\x86_64-win64\`, or anywhere on the `PATH`. The Pascal bindings are already in
`deps\sdl2`; nothing else is needed to compile.

Skip this only if you want a text-only build with no sound: without these two DLLs there is no
window, no drawing primitive and no audio device.

Then:

```
.\build.ps1 -Target sb
.\build.ps1 -Target all
```

---

## macOS

```sh
brew install fpc sdl2 sdl2_ttf
./build.sh sb
```

Apple's `clang` is already there and serves as the C compiler.

---

## When something still does not work

- **`Fatal: Can't find unit system used by Program`** — no usable `fpc.cfg`. See the Windows section
  above; the same `fpcmkcfg` recipe works on Linux.
- **`Can't find unit SDL2`** — the bindings are missing. They ship in `deps/sdl2`; if you cloned
  without them, or pointed the build elsewhere, check `SDL2Path` in `setup.config.json`.
- **A linker error naming `SDL2` or `SDL2_ttf`** — the *development* package is missing, not the
  runtime one. `libSDL2.so.0` alone is not enough to link against; you need the unversioned
  `libSDL2.so` that the `-dev` / `-devel` package provides.
- **The build says a compiler `[cannot compile - skipped]`** — it prints the compiler's own message
  underneath. That message is the answer; it is almost always the `fpc.cfg` above.
