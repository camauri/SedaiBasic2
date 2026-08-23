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
