# Building SedaiBasic2

There are no binary releases yet, so building is the only way in. This assumes you have already
cloned the repository; everything below happens inside it.

**There are two ways to do it.**

| | |
|---|---|
| **Assisted** | `./setup.sh` (Linux) or `.\setup.ps1` (Windows) installs and configures every dependency, then builds. |
| **Manual** | You install the dependencies yourself and run `./build.sh` or `.\build.ps1`. |

The manual way is not harder, it is just explicit: the build scripts check every dependency *before*
compiling anything and print all the missing ones at once, so you are never sent back one item at a
time.

---

## What the build needs

**SDL2 is not an accessory: it is the backend for BOTH the graphics and the audio.** Without
`libSDL2` there is no window, no drawing and no sound: `SCREEN`, `PSET`, `LINE`, `PLAY`, `SOUND` and
the SID emulation all end there.

| | Needed for | Required? |
|---|---|---|
| **Free Pascal 3.2.2** | everything | yes, and **exactly** 3.2.2: 3.3.1 does not compile SedaiBasic |
| **libSDL2** + **libSDL2_ttf** | graphics and audio | yes for `sbv`, `sb --window` and any build with audio |
| **SDL2 Pascal bindings** | compiling the SDL2 units | yes; downloaded into `deps/sdl2`, they are not in the repository |
| **GCC** | the C hot loop, worth **27-45%** | no: without it the build succeeds and the interpreter is slower |
| **SedaiAudioFoundation** | the audio subsystem, SID included | no: without it there is no sound |
| **SDL2_image** | nothing yet | no: installed so the day something uses it nothing has to be fetched |

⭐ **The Pascal bindings are platform independent**: 52 text files, `.pas` and `.inc`, no binaries,
every platform difference behind an `{$IFDEF}`. Windows and Linux download the very same archive,
with the very same checksum.

⚠️ **When SDL2 is needed to LINK depends on what was detected.** `sbv` always links it. With
SedaiAudioFoundation present the audio backend pulls it in, so even the plain `sb` links `libSDL2`.
`sb --window` and the audio backend can also load it at RUN time, so a build can succeed and then
find nothing to draw on. This is why the build reports what *your* configuration needs rather than a
fixed list.

---

# The assisted way

## Linux

```sh
./setup.sh
```

It works out what is missing, shows one command that installs all of it, asks, then fetches the
bindings and SedaiAudioFoundation and builds.

| Option | |
|---|---|
| `-y`, `--yes` | install without asking |
| `--deps-only` | check and install, do not build |
| `--build-only` | build, assume the dependencies are there |
| `--target <name>` | what to build: `all` (default), `sb`, `sbc`, `sbd`, `sbv`, `sbw` |
| `--fpc <path>` | use THIS Free Pascal, whatever the search would find |

## Windows

```powershell
.\setup.ps1
```

⚠️ Windows blocks PowerShell scripts by default. Either
`Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser` once, or
`powershell -ExecutionPolicy Bypass -File .\setup.ps1` each time.

It installs, in order: Free Pascal 3.2.2, the SDL2 runtime, the Pascal bindings, GCC,
SedaiAudioFoundation, then builds. **Every download is pinned by SHA-256**, and the two packages that
are ours to assemble are documented file by file:
[RUNTIME-PACKAGE.md](scripts/windows/RUNTIME-PACKAGE.md) and
[GCC-PACKAGE.md](scripts/windows/GCC-PACKAGE.md).

| Option | |
|---|---|
| `-FpcOnly` | install Free Pascal, do not build |
| `-BuildOnly` | build, assume the dependencies are there |
| `-ForceFpc` | reinstall Free Pascal |
| `-Clean` | clean and rebuild |
| `-Fpc <path>` | use THIS Free Pascal instead of downloading one |

⭐ Nothing it downloads is signed by us, so **the hash is what identifies the file**. The SDL2 DLLs
are the official libsdl-org builds; GCC is a subset of a WinLibs build by Brecht Sanders.

---

# The manual way

## Linux

⛔ **The distribution has to be recent enough, and it is the LIBRARIES that decide.** The SDL2 Pascal
bindings declare every entry point as an ordinary external, so a distribution whose SDL2 is older
than the bindings leaves undefined symbols: `sbv` fails to link, or fails to start. The minimums are
**SDL2 2.30.0** and **SDL2_ttf 2.22.0**.

| | Free Pascal | SDL2 | SDL2_ttf | |
|---|---|---|---|---|
| Debian 13 (trixie) | 3.2.2 | 2.32.4 | 2.24.0 | supported |
| Ubuntu 24.04 LTS (noble) | 3.2.2 | 2.30.0 | 2.22.0 | supported, exactly at the minimum |
| Debian 12 (bookworm) | 3.2.2 | 2.26.5 | 2.20.1 | **too old** |
| Ubuntu 22.04 LTS (jammy) | 3.2.2 | 2.0.20 | 2.0.18 | **too old** |

```sh
sudo apt install fpc gcc libsdl2-dev libsdl2-ttf-dev libsdl2-image-dev
./build.sh sb          # the CLI VM, the default target and the regression one
./build.sh all         # everything: sb, sbc, sbd, sbv, sbw
./build.sh sb --window # sb with the optional SDL2 window presenter
```

FreeType, libpng, libjpeg, zlib and ALSA are not listed because they arrive as dependencies:
`libsdl2-dev` pulls `libasound2-dev`, `libsdl2-ttf-dev` pulls `libfreetype-dev`, and
`libsdl2-image-dev` pulls `libjpeg-dev` and `libpng-dev`.

⛔ **Other distributions: package names only, versions unverified.** The commands below name the
right packages, but **nobody has checked what version those packages actually deliver**, and that is
the whole question: Debian 12 has the correct package names too, and is still too old. Check
`SDL2 >= 2.30.0`, `SDL2_ttf >= 2.22.0` and `fpc = 3.2.2` before trusting them.

```sh
sudo dnf install fpc gcc SDL2-devel SDL2_ttf-devel SDL2_image-devel     # Fedora, RHEL, Rocky, Alma
sudo pacman -S fpc gcc sdl2 sdl2_ttf sdl2_image                         # Arch, Manjaro
sudo zypper install fpc gcc libSDL2-devel libSDL2_ttf-devel libSDL2_image-devel   # openSUSE
sudo apk add fpc gcc sdl2-dev sdl2_ttf-dev sdl2-image-dev               # Alpine
```

The bindings and SedaiAudioFoundation are not distribution packages. `./setup.sh --deps-only` fetches
both without installing anything else, or clone
[SedaiAudio](https://github.com/camauri/SedaiAudio) beside this repository yourself.

## Windows

### 1. Free Pascal 3.2.2

[freepascal.org/download.html](https://www.freepascal.org/download.html), or
[fpcupdeluxe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe) if you also want Lazarus.

⚠️ Whichever you choose, make sure a **usable `fpc.cfg`** ends up next to the compiler. It is the one
thing that most often goes wrong, and the symptom is confusing: the compiler works perfectly inside
Lazarus, which supplies its own unit paths and never reads `fpc.cfg`, and fails from a plain shell
with `Fatal: Can't find unit system used by Program`. You can check in ten seconds: put `begin end.`
in `probe.pas` and run `fpc probe.pas`. If it fails, generate the config:

```
<fpcroot>\bin\x86_64-win64\fpcmkcfg.exe -d basepath=<fpcroot> -o <fpcroot>\bin\x86_64-win64\fpc.cfg
```

### 2. SDL2, SDL2_ttf and SDL2_image

Take the **runtime** archive of each, `SDL2-<version>-win32-x64.zip` and not the `-devel-` one, from
[SDL](https://github.com/libsdl-org/SDL/releases),
[SDL_ttf](https://github.com/libsdl-org/SDL_ttf/releases) and
[SDL_image](https://github.com/libsdl-org/SDL_image/releases). Put the DLLs in `bin\x86_64-win64\`
beside the built executable, or anywhere on the `PATH`. The versions `setup.ps1` installs are SDL2
2.32.10, SDL2_ttf 2.24.0 and SDL2_image 2.8.12.

⭐ **Three files, not a pile.** The official builds are self-contained: their only imports are Win32
system DLLs and `SDL2.dll`, with FreeType, libpng, libjpeg and zlib linked inside. Any archive that
makes you copy `freetype.dll`, `zlib1.dll` or a `libpng`/`libjpeg` beside them is a different build,
MSYS2 or vcpkg, and then each of those has its own dependencies to chase. Our own package carried
exactly that mistake until 24 August 2026, and four of its seven files could never have loaded:
[RUNTIME-PACKAGE.md](scripts/windows/RUNTIME-PACKAGE.md) says which and why.

### 3. The Pascal bindings and SedaiAudioFoundation

Neither is in the repository. `.\setup.ps1` downloads both; without them you get no SDL2 units and no
sound.

### 4. GCC, for the C hot loop

The hot dispatch arms (`src/hotdisp.c`) are compiled by a C compiler, not by FPC. `setup.ps1`
installs one into `deps\gcc`; the build also accepts a `gcc.exe` on the `PATH`, or one named by
`SEDAI_CC`. Without it the build still succeeds and says so: you get a slower interpreter, not a
failure. `-NoHotC` leaves it out on purpose, `-HotC` makes a missing compiler an error.

⛔ **GCC, not "a C compiler".** The flag set is GCC's and it is not decoration: `-fno-crossjumping`
alone is worth spectral-norm -16.1%, because it stops the compiler merging the replicated dispatch
tails that give every arm its own branch-predictor history. Microsoft's compiler has no equivalent
spelling. If you install one yourself, use [w64devkit](https://github.com/skeeto/w64devkit) (one zip,
no installer) or [MSYS2](https://www.msys2.org/) (`pacman -S mingw-w64-x86_64-gcc`).

⭐ **Only the compiler proper is needed, and the flavour does not matter.** The build never *links*
with it: it runs `gcc -c` and hands the object to FPC, so no linker, no CRT and no import libraries
are involved. UCRT vs MSVCRT, SEH vs SJLJ, POSIX vs win32 threads all describe link-time and runtime
behaviour, and we never link. Verified by building the object with a win32-threads toolchain: same
three undefined symbols, all of them ours, nothing from the CRT.

### Then

```
.\build.ps1 -Target sb
.\build.ps1 -Target all
```

---

## Naming your own Free Pascal

The build searches the usual places, lists what it finds, says why it skipped each one it cannot use,
and remembers your answer in `setup.config.json`. When the search gets it wrong, or finds nothing, you
can name the compiler yourself. Four ways, in the order they win:

| | |
|---|---|
| `./build.sh --fpc <path>` / `.\build.ps1 -Fpc <path>` | this run only, beats a stored choice |
| `SEDAI_FPC=<path>` | the same, as an environment variable |
| the stored choice | `setup.config.json`, written by the question below |
| `./build.sh --select-fpc` / `.\build.ps1 -SelectFpc` | list them all and pick, again |

⭐ **A path can be spelled four ways and all four work**: the binary itself, the directory holding it,
the `bin` directory, or the installation root. `--fpc ~/fpc-3.2.2` finds
`~/fpc-3.2.2/bin/x86_64-linux/fpc` on its own.

⭐ **And when nothing is found at all, it asks.** The search failing used to be a dead end, which is
the wrong end of the exchange: whoever is running the script usually knows exactly where their
compiler is. It now offers to be told, checks what you type, and says precisely why if it cannot be
used. ⛔ Only with a terminal attached: an unattended run fails loudly instead of hanging on a
question nobody will answer.

⛔ **A named compiler that fails the check STOPS the build**, it does not fall back to the search.
Quietly building with a compiler other than the one you named is worse than not building.

---

## macOS

⛔ **Never built, never tested.** `build.sh` recognises `--os darwin` and the dependency table knows
the Homebrew package names, but nobody has compiled SedaiBasic2 on a Mac and no macOS binary has ever
been produced. Treat it as unexplored, not as supported.

---

## When something still does not work

- **`Fatal: Can't find unit system used by Program`**: no usable `fpc.cfg`. See the Windows section
  above; the same `fpcmkcfg` recipe works on Linux.
- **`Can't find unit SDL2`**: the bindings are missing. They are NOT in the repository: `setup.sh`
  and `setup.ps1` download them into `deps/sdl2`. Run the setup, or check `SDL2Path` in
  `setup.config.json` if you pointed the build at a copy of your own.
- **A linker error naming `SDL2` or `SDL2_ttf`**: the *development* package is missing, not the
  runtime one. `libSDL2.so.0` alone is not enough to link against; you need the unversioned
  `libSDL2.so` that the `-dev` / `-devel` package provides.
- **The build says a compiler `[cannot compile - skipped]`**: it prints the compiler's own message
  underneath. That message is the answer; it is almost always the `fpc.cfg` above.
- **`none of them can compile a trivial program`**: the same thing, for every compiler found. Fix
  `fpc.cfg`, or name a working compiler with `--fpc` / `-Fpc`.
