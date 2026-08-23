#!/bin/bash
#
# SedaiBasic2 Setup Script for Linux
#
# Copyright (c) 2025 Maurizio Cammalleri
# Released under GNU GPL v3
#
# Installs the build dependencies with the distribution's own package manager, then builds.
#
# ⛔ IT DOES NOT DOWNLOAD ANYTHING ITSELF, and that is the whole difference from the Windows setup.
# On Linux every dependency is one apt/dnf/pacman/zypper/apk package away, already built for this
# distribution and already patched by it. Fetching our own copies would be worse in every respect:
# older, unpatched, and invisible to the system updater.
#

# ============================================================================
#  CONFIGURATION
# ============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FPC_VERSION="3.2.2"
FPC_ARCH="x86_64-linux"
FPC_DIR="$SCRIPT_DIR/fpc/$FPC_VERSION"
FPC_EXE="$FPC_DIR/bin/$FPC_ARCH/fpc"
OUTPUT_EXE="sb"
SOURCE_FILE="src/SedaiBasicVM.lpr"
BIN_DIR="bin/$FPC_ARCH"
LIB_DIR="lib/$FPC_ARCH"

# ============================================================================
#  DISPLAY FUNCTIONS
# ============================================================================

show_banner() {
    local width=70
    local border=$(printf '=%.0s' $(seq 1 $width))

    echo ""
    echo -e "\033[36m$border\033[0m"
    echo ""
    echo -e "\033[37m   ____           _       _ ____            _      ____  \033[0m"
    echo -e "\033[37m  / ___|  ___  __| | __ _(_) __ )  __ _ ___(_) ___|___ \\ \033[0m"
    echo -e "\033[37m  \\___ \\ / _ \\/ _\` |/ _\` | |  _ \\ / _\` / __| |/ __| __) |\033[0m"
    echo -e "\033[37m   ___) |  __/ (_| | (_| | | |_) | (_| \\__ \\ | (__ / __/ \033[0m"
    echo -e "\033[37m  |____/ \\___|\\__,_|\\__,_|_|____/ \\__,_|___/_|\\___|_____|\033[0m"
    echo ""
    echo -e "\033[33m                   SETUP SCRIPT\033[0m"
    echo ""
    echo -e "\033[36m$border\033[0m"
    echo ""
    echo -e "\033[90m  Copyright (c) 2025 Maurizio Cammalleri\033[0m"
    echo -e "\033[90m  Released under GNU GPL v3\033[0m"
    echo ""
    echo -e "\033[36m$border\033[0m"
    echo ""
}

show_status() {
    local message="$1"
    local type="${2:-Info}"
    local prefix="      "

    case "$type" in
        "Success")
            echo -e "${prefix}\033[32m[OK]\033[0m $message"
            ;;
        "Error")
            echo -e "${prefix}\033[31m[ERROR]\033[0m $message"
            ;;
        "Warning")
            echo -e "${prefix}\033[33m[!]\033[0m $message"
            ;;
        "Skip")
            echo -e "${prefix}\033[33m[SKIP]\033[0m $message"
            ;;
        *)
            echo -e "${prefix}\033[90m$message\033[0m"
            ;;
    esac
}


# shellcheck source=scripts/lib/deps-linux.sh
source "$SCRIPT_DIR/scripts/lib/deps-linux.sh"

CYAN='\033[0;36m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; RED='\033[0;31m'; GRAY='\033[0;90m'; NC='\033[0m'

ASSUME_YES=false
DO_BUILD=true
DO_DEPS=true
BUILD_TARGET="all"

show_help() {
    cat <<'HELP'
Usage: ./setup.sh [options]

  Installs the build dependencies with your distribution's package manager, then builds.

Options:
  -y, --yes            Do not ask before installing; run the install command straight away
      --deps-only      Install the dependencies and stop
      --build-only     Skip the dependencies and build
      --target <name>  What to build: all (default), sb, sbc, sbd, sbv, sbw
  -h, --help           This help

  Supported package managers: apt, dnf, pacman, zypper, apk, brew.
  On anything else the missing packages are listed and you install them yourself.
HELP
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        -y|--yes)     ASSUME_YES=true; shift ;;
        --deps-only)  DO_BUILD=false; shift ;;
        --build-only) DO_DEPS=false; shift ;;
        --target)     BUILD_TARGET="$2"; shift 2 ;;
        -h|--help)    show_help; exit 0 ;;
        *) echo -e "${RED}Unknown option: $1${NC}" >&2; show_help; exit 1 ;;
    esac
done

show_banner

# ---------------------------------------------------------------------------
#  Dependencies
# ---------------------------------------------------------------------------
# ⛔ EVERY MISSING ONE IS COLLECTED BEFORE ANY IS INSTALLED. Reporting them one at a time is the
# behaviour this whole file exists to avoid: install, run again, be told about the next one.
if [[ "$DO_DEPS" == "true" ]]; then
    echo -e "${CYAN}  Checking dependencies${NC}"
    echo ""

    PM="$(pkg_manager)"
    if [[ -z "$PM" ]]; then
        show_status "no supported package manager found (apt, dnf, pacman, zypper, apk, brew)" "Warning"
        show_status "install fpc, gcc and the SDL2 development files yourself, then re-run with --build-only" "Info"
        echo ""
    fi

    MISSING=()
    MISSING_WHY=()

    # ⛔ ON A DEBIAN-LIKE SYSTEM, CHECK WHAT apt WOULD ACTUALLY INSTALL, before installing it.
    # The SDL2 bindings declare every entry point as an ordinary external, so a distribution whose
    # libraries are older than the bindings leaves undefined symbols: sbv fails to link, or fails to
    # load. Finding that out after "sudo apt install" is the worst moment, because by then it looks
    # like our build is broken rather than the distribution being too old.
    # Supported today: Debian 13+ and Ubuntu 24.04+. Debian 12 and Ubuntu 22.04 have the right FPC
    # and SDL2_ttf that is two minor versions short.
    if [[ "$PM" == "apt" ]]; then
        TOO_OLD=""
        for spec in "libsdl2-dev:$SDL2_MIN_VERSION:libSDL2" "libsdl2-ttf-dev:$SDL2_TTF_MIN_VERSION:libSDL2_ttf"; do
            pkg="${spec%%:*}"; rest="${spec#*:}"; min="${rest%%:*}"; label="${rest##*:}"
            cand="$(apt_candidate_version "$pkg" || true)"
            [[ -z "$cand" ]] && continue
            if ! version_at_least "$cand" "$min"; then
                TOO_OLD="$TOO_OLD\n      $label $cand, needs $min or newer"
            fi
        done
        cand_fpc="$(apt_candidate_version fpc || true)"
        if [[ -n "$cand_fpc" && "$cand_fpc" != "$FPC_REQUIRED_VERSION" ]]; then
            TOO_OLD="$TOO_OLD\n      Free Pascal $cand_fpc, needs exactly $FPC_REQUIRED_VERSION"
        fi
        if [[ -n "$TOO_OLD" ]]; then
            show_status "$(distro_name) is too old for SedaiBasic:" "Error"
            echo -e "${YELLOW}$TOO_OLD${NC}"
            echo ""
            show_status "supported: Debian 13 or newer, Ubuntu 24.04 or newer" "Info"
            show_status "the SDL2 Pascal bindings declare entry points these libraries do not have," "Info"
            show_status "so it would install cleanly and then fail to link or to start" "Info"
            exit 1
        fi
    fi

    # ⛔ THE SAME SEARCH build.sh USES, not "is fpc on the PATH". A compiler installed under
    # ~/tools/fp or by fpcupdeluxe is one the build finds and this check would not, so setup would
    # offer to install a second one over the top of a perfectly good install.
    # ⛔ AND THE VERSION DECIDES, not the presence. This took the FIRST candidate and on a machine
    # with both 3.3.1 and 3.2.2 installed it reported the 3.3.1 as fine - a compiler that does not
    # build SedaiBasic at all.
    FPC_PLATFORM="$(uname -m | sed 's/^amd64$/x86_64/')-linux"
    FPC_FOUND=""
    FPC_WRONG=""
    CFG_FPC="$(fpc_configured 2>/dev/null || true)"
    if [[ -n "$CFG_FPC" ]] && fpc_version_ok "$CFG_FPC"; then
        FPC_FOUND="$CFG_FPC"
    else
        while read -r c; do
            [[ -n "$c" ]] || continue
            if fpc_version_ok "$c"; then FPC_FOUND="$c"; break
            else FPC_WRONG="$FPC_WRONG $(fpc_version_of "$c")"; fi
        done < <(fpc_candidates "$FPC_PLATFORM" 2>/dev/null)
    fi
    if [[ -n "$FPC_FOUND" ]]; then
        show_status "Free Pascal $FPC_REQUIRED_VERSION: $FPC_FOUND" "Success"
    elif [[ -n "$FPC_WRONG" ]]; then
        show_status "Free Pascal found, but the wrong version:$FPC_WRONG" "Error"
        show_status "SedaiBasic needs exactly $FPC_REQUIRED_VERSION - 3.3.1 does not compile it" "Info"
        MISSING+=("$(dep_pkg fpc "$PM")"); MISSING_WHY+=("Free Pascal $FPC_REQUIRED_VERSION exactly")
    else
        MISSING+=("$(dep_pkg fpc "$PM")"); MISSING_WHY+=("Free Pascal, the compiler everything is built with")
    fi

    # A C compiler is never required: without it the build succeeds and the interpreter is slower.
    if command -v cc >/dev/null 2>&1 || command -v gcc >/dev/null 2>&1 || command -v clang >/dev/null 2>&1; then
        show_status "C compiler: $(command -v gcc || command -v cc || command -v clang)" "Success"
    else
        MISSING+=("$(dep_pkg cc "$PM")"); MISSING_WHY+=("the C hot loop, worth 27 to 45% (optional)")
    fi

    # ⭐ SDL2 IS THE BACKEND FOR GRAPHICS *AND* AUDIO: without it there is no window, no drawing
    # primitive and no sound.
    if have_shared_lib sdl2 libSDL2.so; then
        show_status "libSDL2 development files" "Success"
    else
        MISSING+=("$(dep_pkg sdl2-dev "$PM")"); MISSING_WHY+=("graphics AND audio: the window, every drawing primitive, every sound")
    fi

    if have_shared_lib SDL2_ttf libSDL2_ttf.so; then
        show_status "libSDL2_ttf development files" "Success"
    else
        MISSING+=("$(dep_pkg sdl2ttf-dev "$PM")"); MISSING_WHY+=("the text renderer SDL2 draws characters with")
    fi

    # ⚠️ SDL2_image IS NOT USED BY THE BUILD YET. It is installed because it sits beside the Windows
    # binaries, so that the day something starts using it nothing has to be installed again. Never
    # required: a missing one is reported and the build goes ahead.
    #
    # ⛔ AND IT IS THE ONLY ONE LISTED HERE, because the rest arrive as DEPENDENCIES and naming them
    # again would just make the install command longer. Checked with apt-cache on Debian 13:
    #   libsdl2-dev       -> libasound2-dev -> libasound2t64   (ALSA, for MIDI input)
    #   libsdl2-ttf-dev   -> libfreetype-dev                   (FreeType)
    #   libsdl2-image-dev -> libjpeg-dev, libpng-dev -> zlib1g-dev
    # If some distribution does not imply them, the library is simply absent and this check says so.
    if have_shared_lib SDL2_image libSDL2_image.so; then
        show_status "SDL2_image" "Success"
    else
        MISSING+=("$(dep_pkg sdl2image-dev "$PM")"); MISSING_WHY+=("SDL2_image (not used by the build yet)")
    fi

    # The two tools this script itself needs to fetch and unpack the Pascal bindings.
    for tool in curl unzip; do
        if command -v "$tool" >/dev/null 2>&1; then
            show_status "$tool" "Success"
        else
            MISSING+=("$(dep_pkg "$tool" "$PM")"); MISSING_WHY+=("needed to download and unpack the SDL2 Pascal bindings")
        fi
    done

    echo ""
    if [[ ${#MISSING[@]} -gt 0 ]]; then
        echo -e "${YELLOW}  Missing:${NC}"
        for i in "${!MISSING[@]}"; do
            printf "      ${YELLOW}%-22s${NC} %s\n" "${MISSING[$i]}" "${MISSING_WHY[$i]}"
        done
        echo ""

        CMD="$(pkg_install_cmd "$PM" "${MISSING[@]}")"
        if [[ -z "$CMD" ]]; then
            show_status "install them with your package manager, then re-run with --build-only" "Warning"
            exit 1
        fi

        echo -e "${CYAN}  Install command:${NC}"
        echo -e "      ${GREEN}$CMD${NC}"
        echo ""

        if [[ "$ASSUME_YES" != "true" ]]; then
            # No terminal means no question: a script or a CI run must not hang on a prompt.
            if [[ ! -t 0 ]]; then
                show_status "not a terminal: run the command above, or pass -y" "Error"
                exit 1
            fi
            read -r -p "      Run it now? [Y/n] " ans
            case "$ans" in [nN]*) show_status "nothing installed" "Skip"; exit 1 ;; esac
        fi

        echo ""
        if ! eval "$CMD"; then
            show_status "the install command failed" "Error"
            exit 1
        fi
        echo ""
        show_status "dependencies installed" "Success"
    else
        show_status "everything needed is already installed" "Success"
    fi
    echo ""
fi

# ---------------------------------------------------------------------------
#  SDL2 Pascal bindings
# ---------------------------------------------------------------------------
# ⛔ THEY ARE NOT IN THE REPOSITORY. deps/ is gitignored, so a fresh clone has no bindings and sbv
# does not compile - on Linux nobody was fetching them at all, which is a hole this script existed
# to close and did not.
# ⭐ The archive is PLATFORM INDEPENDENT: 52 text files with every platform difference behind an
# {$IFDEF}, so this is the very same file, and the very same hash, the Windows installer uses.
if [[ "$DO_DEPS" == "true" ]]; then
    echo -e "${CYAN}  SDL2 Pascal bindings${NC}"
    echo ""
    if [[ -f "$SCRIPT_DIR/deps/sdl2/sdl2.pas" ]]; then
        show_status "already present: deps/sdl2" "Success"
    else
        tmp="$(mktemp -d)"
        trap 'rm -rf "$tmp"' EXIT
        show_status "downloading v$SDL2_BINDINGS_VERSION..." "Info"
        if ! curl -sSL --fail -o "$tmp/sdl2.zip" "$SDL2_BINDINGS_URL"; then
            show_status "download failed: $SDL2_BINDINGS_URL" "Error"
            exit 1
        fi
        got="$(sha256sum "$tmp/sdl2.zip" | cut -d' ' -f1)"
        if [[ "$got" != "$SDL2_BINDINGS_SHA256" ]]; then
            show_status "checksum mismatch, the download is not what we expect" "Error"
            show_status "expected $SDL2_BINDINGS_SHA256" "Info"
            show_status "got      $got" "Info"
            exit 1
        fi
        mkdir -p "$SCRIPT_DIR/deps"
        if ! unzip -q -o "$tmp/sdl2.zip" -d "$SCRIPT_DIR/deps"; then
            show_status "could not unpack the archive" "Error"
            exit 1
        fi
        if [[ -f "$SCRIPT_DIR/deps/sdl2/sdl2.pas" ]]; then
            show_status "installed: deps/sdl2" "Success"
        else
            show_status "unpacked, but deps/sdl2/sdl2.pas is not there" "Error"
            exit 1
        fi
    fi
    echo ""
fi

# ---------------------------------------------------------------------------
#  SedaiAudioFoundation (optional)
# ---------------------------------------------------------------------------
# It is what makes SOUND work at all - the SID emulation included - and it is a separate repository
# rather than a package. build.sh auto-detects it in deps/ or beside the project, and builds without
# it if it is not there, so this never fails the setup.
if [[ "$DO_DEPS" == "true" ]]; then
    echo -e "${CYAN}  SedaiAudioFoundation${NC}"
    echo ""
    audio_here=""
    for cand in "$SCRIPT_DIR/deps/SedaiAudioFoundation" "$(dirname "$SCRIPT_DIR")/SedaiAudioFoundation"; do
        file_exists_nocase "$cand/src" "$SEDAI_AUDIO_MARKER" && { audio_here="$cand"; break; }
    done
    if [[ -n "$audio_here" ]]; then
        show_status "already present: $audio_here" "Success"
    else
        # ⚠️ A BRANCH ARCHIVE, so there is nothing to verify against: no release, no version, no
        # checksum. Said out loud rather than passed over, because every other download in this
        # project is hash-pinned and this one cannot be.
        show_status "downloading $SEDAI_AUDIO_REPO ($SEDAI_AUDIO_BRANCH, unpinned)..." "Info"
        tmpa="$(mktemp -d)"
        if curl -sSL --fail -o "$tmpa/audio.zip" "$SEDAI_AUDIO_URL" \
           && unzip -q -o "$tmpa/audio.zip" -d "$tmpa"; then
            src_dir="$(find "$tmpa" -maxdepth 1 -type d -name 'SedaiAudio-*' | head -1)"
            if [[ -n "$src_dir" ]] && file_exists_nocase "$src_dir/src" "$SEDAI_AUDIO_MARKER"; then
                mkdir -p "$SCRIPT_DIR/deps"
                rm -rf "$SCRIPT_DIR/deps/SedaiAudioFoundation"
                mv "$src_dir" "$SCRIPT_DIR/deps/SedaiAudioFoundation"
                show_status "installed: deps/SedaiAudioFoundation" "Success"
            else
                show_status "the archive does not look like SedaiAudioFoundation - audio disabled" "Warning"
            fi
        else
            show_status "download failed - the build will run without audio" "Warning"
        fi
        rm -rf "$tmpa"
    fi
    echo ""
fi

[[ "$DO_BUILD" != "true" ]] && exit 0

# ---------------------------------------------------------------------------
#  Build
# ---------------------------------------------------------------------------
echo -e "${CYAN}  Building${NC}"
echo ""

if [[ ! -x "$SCRIPT_DIR/build.sh" ]]; then
    show_status "build.sh not found or not executable" "Error"
    exit 1
fi

# build.sh runs its own dependency preflight and reports anything still missing, so a package that
# the manager installed under a name we did not expect is caught there rather than as a link error.
if "$SCRIPT_DIR/build.sh" "$BUILD_TARGET"; then
    echo ""
    show_status "SedaiBasic2 built: $SCRIPT_DIR/bin/" "Success"
    exit 0
else
    echo ""
    show_status "the build failed" "Error"
    exit 1
fi
