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

    # ⛔ THE SAME SEARCH build.sh USES, not "is fpc on the PATH". A compiler installed under
    # ~/tools/fp or by fpcupdeluxe is one the build finds and this check would not, so setup would
    # offer to install a second one over the top of a perfectly good install.
    FPC_FOUND="$(fpc_candidates "$(uname -m | sed 's/^amd64$/x86_64/')-linux" 2>/dev/null | head -1)"
    if [[ -n "$FPC_FOUND" ]]; then
        show_status "Free Pascal: $("$FPC_FOUND" -iV 2>/dev/null) ($FPC_FOUND)" "Success"
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
