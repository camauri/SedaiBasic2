#!/bin/bash
#
# Which package provides what, on which distribution, and how to tell whether it is already there.
#
# Sourced by BOTH build.sh (which reports what is missing) and setup.sh (which installs it). One
# table, one place: a second copy would drift, and the two scripts would then name different packages
# for the same thing, which is the worst kind of wrong because each is self consistent.

# The package manager actually present, or '' when none is recognised.
pkg_manager() {
    local m
    for m in apt dnf pacman zypper apk brew; do
        command -v "$m" >/dev/null 2>&1 && { echo "$m"; return; }
    done
    echo ""
}

# The install command for a manager, given the packages. Answers '' for an unknown manager.
pkg_install_cmd() {
    local m="$1"; shift
    case "$m" in
        apt)    echo "sudo apt install -y $*" ;;
        dnf)    echo "sudo dnf install -y $*" ;;
        pacman) echo "sudo pacman -S --needed --noconfirm $*" ;;
        zypper) echo "sudo zypper install -y $*" ;;
        apk)    echo "sudo apk add $*" ;;
        brew)   echo "brew install $*" ;;
        *)      echo "" ;;
    esac
}

# dep_pkg <key> <manager> -> the package name(s), or '' when that manager needs none.
#   fpc          the Free Pascal compiler
#   cc           a C compiler, for the hot loop
#   sdl2-dev     libSDL2 development files (needed to LINK)
#   sdl2ttf-dev  libSDL2_ttf development files
#   sdl2-run     the runtime libraries, for a build that only loads them at run time
dep_pkg() {
    local key="$1" m="$2"
    case "$key:$m" in
        fpc:apt|fpc:dnf|fpc:pacman|fpc:zypper|fpc:apk|fpc:brew) echo "fpc" ;;

        cc:apt|cc:dnf|cc:pacman|cc:zypper|cc:apk) echo "gcc" ;;
        cc:brew)            echo "" ;;   # Apple's clang is already there

        sdl2-dev:apt)       echo "libsdl2-dev" ;;
        sdl2-dev:dnf)       echo "SDL2-devel" ;;
        sdl2-dev:pacman)    echo "sdl2" ;;
        sdl2-dev:zypper)    echo "libSDL2-devel" ;;
        sdl2-dev:apk)       echo "sdl2-dev" ;;
        sdl2-dev:brew)      echo "sdl2" ;;

        sdl2ttf-dev:apt)    echo "libsdl2-ttf-dev" ;;
        sdl2ttf-dev:dnf)    echo "SDL2_ttf-devel" ;;
        sdl2ttf-dev:pacman) echo "sdl2_ttf" ;;
        sdl2ttf-dev:zypper) echo "libSDL2_ttf-devel" ;;
        sdl2ttf-dev:apk)    echo "sdl2_ttf-dev" ;;
        sdl2ttf-dev:brew)   echo "sdl2_ttf" ;;

        unzip:apt|unzip:dnf|unzip:pacman|unzip:zypper|unzip:apk|unzip:brew) echo "unzip" ;;
        curl:apt|curl:dnf|curl:pacman|curl:zypper|curl:apk|curl:brew)         echo "curl" ;;

        # ⭐ ALSA, for MIDI INPUT. SedaiAudioFoundation opens libasound.so.2 with dlopen
        # (SedaiMIDIInput.pas), so it is optional at run time: without it MIDI input is silently
        # unavailable and everything else works. Nothing links against it.
        alsa:apt)           echo "libasound2t64" ;;
        alsa:dnf)           echo "alsa-lib" ;;
        alsa:pacman)        echo "alsa-lib" ;;
        alsa:zypper)        echo "libasound2" ;;
        alsa:apk)           echo "alsa-lib" ;;
        alsa:brew)          echo "" ;;

        sdl2-run:apt)       echo "libsdl2-2.0-0 libsdl2-ttf-2.0-0" ;;
        sdl2-run:dnf)       echo "SDL2 SDL2_ttf" ;;
        sdl2-run:pacman)    echo "sdl2 sdl2_ttf" ;;
        sdl2-run:zypper)    echo "libSDL2-2_0-0 libSDL2_ttf-2_0-0" ;;
        sdl2-run:apk)       echo "sdl2 sdl2_ttf" ;;
        sdl2-run:brew)      echo "sdl2 sdl2_ttf" ;;

        *) echo "" ;;
    esac
}

# A shared library is present if pkg-config knows it, or if the linker can find the unversioned .so,
# which is what a -dev/-devel package provides and what the LINK needs. The versioned .so.0 alone is
# the runtime package and is not enough to build against.
have_shared_lib() {
    local pc="$1" soname="$2" d
    command -v pkg-config >/dev/null 2>&1 && pkg-config --exists "$pc" 2>/dev/null && return 0
    for d in /usr/lib /usr/local/lib /usr/lib64 /lib "/usr/lib/$(uname -m)-linux-gnu" /opt/homebrew/lib; do
        [[ -e "$d/$soname" ]] && return 0
    done
    return 1
}

# Every Free Pascal this machine might have, best guess first. ⛔ Shared on purpose: setup.sh asks
# whether a compiler is present and build.sh picks which one to use, and if they searched
# different places setup would offer to install one the build already has.
# Needs SCRIPT_DIR set by the caller (both scripts set it to the project root).
fpc_candidates() {
    local platform="$1" c
    {
        printf '%s\n' "$SCRIPT_DIR/fpc/3.2.2/bin/$platform/fpc"
        for c in "$HOME"/tools/fp/*/fpc/bin/"$platform"/fpc \
                 "$HOME"/fpcupdeluxe/fpc/bin/"$platform"/fpc; do
            printf '%s\n' "$c"
        done
        command -v fpc 2>/dev/null || true
        # Last resort, and deliberately last: a deep scan of the home finds installs in odd places
        # but says nothing about which one is meant.
        find "$HOME" -maxdepth 6 -type f -name fpc -perm -u+x 2>/dev/null || true
    } | while read -r c; do
        [[ -n "$c" && -x "$c" ]] || continue
        readlink -f "$c" 2>/dev/null || printf '%s\n' "$c"
    done | awk '!seen[$0]++'
}

# ⛔ EXACTLY 3.2.2, AND IT IS NOT A PREFERENCE. Other versions are not "probably fine": 3.3.1 does not
# compile SedaiBasic at all. A build that picks one silently fails in a way that looks like a source
# problem, so both scripts refuse it by version before ever trying to compile.
FPC_REQUIRED_VERSION="3.2.2"

# The version a compiler reports, or '' when it cannot be asked.
fpc_version_of() {
    local fpc="$1" v
    [[ -x "$fpc" ]] || return 1
    v="$("$fpc" -iV 2>/dev/null)"
    # 3.2.2-r0d122c49 and 3.2.2 are the same compiler: the revision suffix is not part of the version.
    printf '%s\n' "${v%%-*}"
}

fpc_version_ok() {
    [[ "$(fpc_version_of "$1")" == "$FPC_REQUIRED_VERSION" ]]
}

# The compiler setup.config.json points at, if any and if it is still there.
fpc_configured() {
    local cfg="$SCRIPT_DIR/setup.config.json" p
    [[ -f "$cfg" ]] || return 1
    p="$(sed -n 's/.*"FpcBin"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' "$cfg" | head -1)"
    [[ -n "$p" && -x "$p" ]] && printf '%s\n' "$p"
}

# ⛔ MINIMUM LIBRARY VERSIONS, and they are LINK-TIME requirements, not advice. The Pascal bindings
# declare every SDL2 and SDL2_ttf entry point as an ordinary external, so a library older than the
# binding leaves undefined symbols: sbv fails to link, or fails to load. Measured from the bindings
# themselves - the newest functions they declare are "since SDL 2.30.0" (e.g.
# SDL_GameControllerGetSteamHandle) and "since SDL_ttf 2.22.0".
#
# What that rules out, checked against the archives on 23 Aug 2026:
#   Debian 13 trixie   SDL2 2.32.4  SDL2_ttf 2.24.0   ok
#   Debian 12 bookworm SDL2 2.26.5  SDL2_ttf 2.20.1   too old
#   Ubuntu 24.04 noble SDL2 2.30.0  SDL2_ttf 2.22.0   ok, exactly at the minimum
#   Ubuntu 22.04 jammy SDL2 2.0.20  SDL2_ttf 2.0.18   too old
SDL2_MIN_VERSION="2.30.0"
SDL2_TTF_MIN_VERSION="2.22.0"

# Is $1 at least $2? Uses sort -V, so 2.4.0 is correctly newer than 2.20.0's predecessor ordering.
version_at_least() {
    [[ "$(printf '%s\n%s\n' "$2" "$1" | sort -V | head -1)" == "$2" ]]
}

# The version apt would install for a package, '' when apt is not here or the package is unknown.
apt_candidate_version() {
    local pkg="$1" v
    command -v apt-cache >/dev/null 2>&1 || return 1
    v="$(apt-cache policy "$pkg" 2>/dev/null | sed -n 's/.*Candidat[oe]*:[[:space:]]*//p' | head -1)"
    [[ -z "$v" ]] && v="$(apt-cache policy "$pkg" 2>/dev/null | sed -n 's/.*Candidate:[[:space:]]*//p' | head -1)"
    v="${v%%+*}"; v="${v%%-*}"
    [[ -n "$v" && "$v" != "(none)" && "$v" != "(nessuno)" ]] && printf '%s\n' "$v"
}

# "Debian 13" / "Ubuntu 24.04" / '' - for the message, never for the decision.
distro_name() {
    [[ -r /etc/os-release ]] || return 1
    local name ver
    name="$(sed -n 's/^NAME="\{0,1\}\([^"]*\)"\{0,1\}$/\1/p' /etc/os-release | head -1)"
    ver="$(sed -n 's/^VERSION_ID="\{0,1\}\([^"]*\)"\{0,1\}$/\1/p' /etc/os-release | head -1)"
    printf '%s %s\n' "$name" "$ver"
}

# ⛔ THE PASCAL BINDINGS ARE NOT IN THE REPOSITORY: deps/ is gitignored and they are downloaded.
# ⭐ And they are PLATFORM INDEPENDENT - 52 text files, .pas and .inc, no binaries, every platform
# difference behind an {$IFDEF} - so Windows, Linux and macOS take the very same archive. That is why
# this hash is the same one scripts/windows/install-sdl2.ps1 pins.
SDL2_BINDINGS_VERSION="2.3"
SDL2_BINDINGS_URL="https://github.com/camauri/SedaiBasic2-Deps/releases/download/SDL2-for-Pascal-v2.3/SDL2-for-Pascal-v2.3.zip"
SDL2_BINDINGS_SHA256="829dd68bebfe7756bf037160e7cc268c115976d640480d73ebb8badaa46a9e47"
