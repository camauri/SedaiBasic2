#!/bin/bash
#
# SedaiBasic Build Script
# Copyright (c) 2025 Maurizio Cammalleri
# Released under GNU GPL v3 or Commercial License
#
# The reference for this script is build.ps1, not the memory of how build.sh used to be.
# Kept to standard bash: no distribution-specific tools.
#
# Usage:
#   ./build.sh                     # Build all targets (release)
#   ./build.sh sb                  # Build only sb
#   ./build.sh --debug             # Build with debug info
#   ./build.sh --clean             # Clean and rebuild
#   ./build.sh sb --window         # CLI VM with the optional SDL2 window presenter
#   ./build.sh --with-sedai-audio no
#   ./build.sh sb --debug-flags SSA,REGALLOC

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# shellcheck source=scripts/lib/deps-linux.sh
source "$SCRIPT_DIR/scripts/lib/deps-linux.sh"

# Defaults
TARGET="all"
DEBUG=false
CLEAN=false
WINDOW=false
NO_BANNER=false
SELECT_FPC=false
# ⭐ ON BY DEFAULT since 20 Aug 2026. The hot dispatch arms compiled by a C compiler are worth
# 27-45% wherever they apply, on Linux and - verified under wine - on win64 too. HOT_C_EXPLICIT
# separates "the user asked for it" from "it is simply the default": a missing C compiler is an
# ERROR in the first case and a warning-and-carry-on in the second, so a machine without gcc still
# builds. --no-hot-c turns it off.
HOT_C=true
HOT_C_EXPLICIT=false
# --symbols keeps the RELEASE build exactly as it is and only stops stripping it. A --debug
# build is no substitute for profiling: it also turns on -Ci -Cr -Co, and range and overflow
# checks change the very timings you came to measure.
SYMBOLS=false
CPU=""                 # empty => detect from the host
OS=""                  # empty => detect from the host
WITH_SEDAI_AUDIO=""    # '' auto-detect | 'no' disabled | <path>
DEBUG_FLAGS=""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
GRAY='\033[0;90m'
NC='\033[0m'

show_help() {
    echo "SedaiBasic Build Script"
    echo ""
    echo "Usage: $0 [target] [options]"
    echo ""
    echo "Targets:"
    echo "  all     Build all targets (default)"
    echo "  sb      SedaiBasic VM (interpreter)"
    echo "  sbc     SedaiBasic Compiler"
    echo "  sbd     SedaiBasic Disassembler"
    echo "  sbv     SedaiVision (SDL2 graphical)"
    echo "  sbw     SedaiBasic Web (WEB_MODE)"
    echo ""
    echo "Options:"
    echo "  --debug                  Build with debug info"
    echo "  --clean                  Clean build artifacts before building"
    echo "  --window                 Build sb with the SDL2 window presenter (sb --window)"
    echo "  --cpu <x86_64|i386|aarch64>   Target CPU (default: host)"
    echo "  --os <linux|darwin|win64|win32>  Target OS (default: host)"
    echo "  --with-sedai-audio <no|path>  Audio: disable, or use a specific path"
    echo "  --debug-flags <LIST>     Comma-separated: SSA,REGALLOC,... or ALL"
    echo "  --no-banner              Suppress the banner"
    echo "  --select-fpc             List the Free Pascal compilers found and choose one (stored)"
    echo "  --no-hot-c               Do NOT compile the hot dispatch arms with a C compiler"
    echo "  --symbols                Release build, but NOT stripped (for a profiler; --debug is not a substitute)"
    echo "  --hot-c                  Force it on (it is the default; fails if no C compiler)"
    echo "  --help                   Show this help"
    echo ""
    echo "Environment:"
    echo "  SEDAI_FPC=<path>         Use this fpc binary for one run (not stored)"
    echo "  SEDAI_CPUOPT=none|avx|avx2   Instruction set (default: none, the portable baseline)"
    echo "                           avx/avx2 measured to buy nothing: FPC does not vectorize"
}

detect_cpu() {
    case "$(uname -m)" in
        x86_64|amd64)  echo "x86_64" ;;
        i386|i686)     echo "i386" ;;
        aarch64|arm64) echo "aarch64" ;;
        *)             echo "unknown" ;;
    esac
}

detect_os() {
    case "$(uname -s)" in
        Linux)  echo "linux" ;;
        Darwin) echo "darwin" ;;
        *)      echo "unknown" ;;
    esac
}

# Read a string value out of setup.config.json without requiring a JSON parser.
config_value() {
    local key="$1" file="$SCRIPT_DIR/setup.config.json"
    [[ -f "$file" ]] || return 1
    sed -n "s/.*\"$key\"[[:space:]]*:[[:space:]]*\"\([^\"]*\)\".*/\1/p" "$file" | head -1
}

# Write a string value into setup.config.json, creating the file if needed. Flat JSON only, which is
# all this file has ever held - no parser required, and none available on a bare machine.
config_set() {
    local key="$1" val="$2" file="$SCRIPT_DIR/setup.config.json" esc tmp
    esc="$(printf '%s' "$val" | sed 's/[\\"]/\\&/g')"
    if [[ ! -f "$file" ]]; then
        printf '{\n  "%s": "%s"\n}\n' "$key" "$esc" > "$file"
        return 0
    fi
    tmp="$file.tmp.$$"
    if grep -q "\"$key\"[[:space:]]*:" "$file"; then
        sed "s|\"$key\"[[:space:]]*:[[:space:]]*\"[^\"]*\"|\"$key\": \"$esc\"|" "$file" > "$tmp"
    else
        # Insert as the first member, so a file with or without a trailing comma both stay valid.
        sed "0,/{/s|{|{\n  \"$key\": \"$esc\",|" "$file" > "$tmp"
    fi
    mv "$tmp" "$file"
}

# Locate fpc. On this machine it is NOT on PATH and NOT under /usr: it lives in the user's
# home (~/tools/fp/...). An absence from PATH is not evidence that fpc is missing, so the
# home is searched before giving up - and if it is still not found, the user is asked.
# Which fpc to build with.
#
# ⛔ THIS USED TO PICK THE FIRST BINARY IT FOUND, AND THAT IS A TRAP. The search globbed
# ~/tools/fp/*/fpc/..., which expands in ALPHABETICAL order, so the day an fpc-3.3.1 appeared next
# to fpc-stable the project silently switched compiler - and that install had no usable RTL, so
# every build died with "Can't find unit system" pointing at a compiler nobody had chosen. A found
# binary is not a working compiler, and picking one without saying so is worse than finding none.
#
# So: discover EVERY candidate, PROVE each one compiles, list them, let the user choose ONCE, and
# remember the choice in setup.config.json. After that it is a config read and nothing searches.

# Every fpc binary reachable on this machine, one per line, de-duplicated by resolved path.
# fpc_candidates lives in scripts/lib/deps-linux.sh, shared with setup.sh: the two must look in the
# SAME places or setup will offer to install a compiler the build has already found.

# Does this compiler actually COMPILE? Not "does the binary run" - fpc -iV answers that happily on an
# install whose RTL it cannot find. The only honest test is a build, done the way build.sh builds:
# no explicit config file, because that is what the real invocation does.
# The compiler's OWN message from the last failed probe, one per candidate (see choose_fpc). ⛔ It used
# to go to /dev/null, and the twin in build.ps1 did the same: the script then reported "cannot compile"
# and DESTROYED the only thing that says why. Two people setting the project up hit exactly that and
# could not tell whether the verdict was even true - the probe is a bare "fpc -o<path> probe.pas" with
# "begin end." in it, and its failure is nearly always a missing or unusable fpc.cfg, which the message
# names outright.
FPC_PROBE_LOG=""

fpc_works() {
    local fpc="$1" d rc
    FPC_PROBE_LOG=""
    d="$(mktemp -d)" || return 1
    printf 'begin end.\n' > "$d/probe.pas"
    ( cd "$d" && "$fpc" -o"$d/probe" "$d/probe.pas" ) > "$d/probe.log" 2>&1
    rc=$?
    if [[ $rc -ne 0 ]]; then
        # The first few lines carry the reason; the banner above them is noise.
        FPC_PROBE_LOG="$(grep -viE '^(Free Pascal Compiler|Copyright|Target OS:|Compiling |Linking )' \
                          "$d/probe.log" 2>/dev/null | grep -v '^[[:space:]]*$' | head -n 4)"
        [[ -n "$FPC_PROBE_LOG" ]] || FPC_PROBE_LOG="$(head -n 4 "$d/probe.log" 2>/dev/null)"
    fi
    rm -rf "$d"
    return $rc
}

# .../fpc/bin/<platform>/fpc  ->  .../fpc   (the root form build.ps1 stores as FpcPath).
# Anything else (a system /usr/bin/fpc) has no such root, and prints nothing.
fpc_root_of() {
    local bin="$1" platform="$2"
    case "$bin" in
        */bin/"$platform"/fpc) printf '%s\n' "${bin%/bin/$platform/fpc}" ;;
        *) : ;;
    esac
}

# List what is installed, prove which ones work, and ask. Writes the answer to setup.config.json so
# this happens exactly once.
choose_fpc() {
    local platform="$1" c ver ok n=0 sel root
    local -a paths=() vers=() good=() whys=()
    local why

    while read -r c; do
        [[ -n "$c" ]] || continue
        ver="$("$c" -iV 2>/dev/null)"
        [[ -n "$ver" ]] || continue
        if fpc_works "$c"; then ok=yes; why=""; else ok=no; why="$FPC_PROBE_LOG"; fi
        paths+=("$c"); vers+=("$ver"); good+=("$ok"); whys+=("$why")
    done < <(fpc_candidates "$platform")

    n=${#paths[@]}
    if [[ $n -eq 0 ]]; then
        echo -e "${RED}ERROR: no Free Pascal Compiler found.${NC}" >&2
        echo -e "${YELLOW}Looked in: fpc/3.2.2/, ~/tools/fp/*/fpc/, ~/fpcupdeluxe/, PATH, and \$HOME.${NC}" >&2
        return 1
    fi

    echo "" >&2
    echo -e "${CYAN}Free Pascal compilers found on this machine:${NC}" >&2
    for ((i=0; i<n; i++)); do
        if [[ "${good[$i]}" == yes ]]; then
            printf "  %d) FPC %-8s %s\n" "$((i+1))" "${vers[$i]}" "${paths[$i]}" >&2
        else
            printf "  %d) FPC %-8s %s   ${YELLOW}[cannot compile - skipped]${NC}\n" \
                   "$((i+1))" "${vers[$i]}" "${paths[$i]}" >&2
            # ...and WHY, in the compiler's own words. A verdict with no reason is not actionable.
            if [[ -n "${whys[$i]}" ]]; then
                while IFS= read -r line; do
                    [[ -n "$line" ]] && printf "       ${YELLOW}%s${NC}\n" "$line" >&2
                done <<< "${whys[$i]}"
            fi
        fi
    done
    echo "" >&2

    # A compiler that cannot compile is never the answer, however it got listed.
    local -a usable=()
    for ((i=0; i<n; i++)); do [[ "${good[$i]}" == yes ]] && usable+=("$i"); done
    if [[ ${#usable[@]} -eq 0 ]]; then
        echo -e "${RED}ERROR: none of them can compile a trivial program.${NC}" >&2
        echo -e "${YELLOW}An install without a usable fpc.cfg is the usual cause.${NC}" >&2
        return 1
    fi

    # No terminal means no question: a script or a CI run must fail loudly rather than hang on a
    # prompt, or pick for the user and be wrong quietly.
    if [[ ! -t 0 ]]; then
        echo -e "${YELLOW}Not a terminal, so nothing was chosen and nothing was stored.${NC}" >&2
        echo -e "${YELLOW}Run ./build.sh --select-fpc once interactively, or set SEDAI_FPC=<path>.${NC}" >&2
        return 1
    fi

    local default=$((usable[0]+1))
    while :; do
        read -r -p "Which one should this project use? [$default] " sel >&2 || return 1
        [[ -z "$sel" ]] && sel=$default
        [[ "$sel" =~ ^[0-9]+$ ]] || { echo "  a number, please" >&2; continue; }
        (( sel >= 1 && sel <= n )) || { echo "  out of range" >&2; continue; }
        [[ "${good[$((sel-1))]}" == yes ]] || { echo "  that one cannot compile; pick another" >&2; continue; }
        break
    done

    c="${paths[$((sel-1))]}"
    config_set FpcBin "$c"
    root="$(fpc_root_of "$c" "$platform")"
    # FpcPath is the form build.ps1 reads, so a shared checkout keeps working; a system install has
    # no such root and simply does not get the key.
    [[ -n "$root" ]] && config_set FpcPath "$root"
    echo -e "${GREEN}Stored in setup.config.json: FPC ${vers[$((sel-1))]} - $c${NC}" >&2
    echo -e "${GRAY}Change it later with ./build.sh --select-fpc${NC}" >&2
    printf '%s\n' "$c"
}

find_fpc() {
    local platform="$1" candidate

    # 1. Explicit environment override - deliberately NOT stored: it is a one-off, and writing it
    #    would turn "just this once" into the project's setting.
    if [[ -n "$SEDAI_FPC" && -x "$SEDAI_FPC" ]]; then
        echo "$SEDAI_FPC"; return 0
    fi

    # 2. The stored choice.
    if [[ "$SELECT_FPC" != "true" ]]; then
        candidate="$(config_value FpcBin 2>/dev/null || true)"
        if [[ -n "$candidate" && -x "$candidate" ]]; then
            echo "$candidate"; return 0
        fi
        # The older key, kept so an existing setup.config.json keeps working.
        candidate="$(config_value FpcPath 2>/dev/null || true)"
        if [[ -n "$candidate" && -x "$candidate/bin/$platform/fpc" ]]; then
            echo "$candidate/bin/$platform/fpc"; return 0
        fi
    fi

    # 3. Nothing stored (or --select-fpc): ask, once.
    choose_fpc "$platform"
}

# What the CPU we are building FOR can actually execute.
#
# This exists because the instruction-set flags used to be gated on whether AUDIO was enabled -
# two things that have nothing to do with each other. A binary built for a CPU feature the
# machine lacks fails as an illegal instruction at run time, with nothing in the build output
# to explain it.
#
# Detection is of the HOST, so it is only meaningful when not cross-compiling; the caller gates
# on the target CPU matching. SEDAI_CPUOPT=none forces the portable baseline (useful when the
# binaries have to run on an older machine than the one that builds them), =avx / =avx2 force a
# level explicitly.
#
# ⛔ THE DEFAULT IS THE PORTABLE BASELINE, and that is a MEASURED decision, not caution.
# Measured 15 Aug 2026: the AVX2/FMA flags buy NOTHING. FPC has no auto-vectorizer - on a
# trivially vectorizable loop, with -O3 -CfAVX2 -OpCOREAVX2 -OoFASTMATH, it emits ZERO %ymm
# registers where gcc -O3 -march=native emits 19 - so -CfAVX2 only gives the VEX encoding of
# SCALAR operations. In the whole 653 943-instruction sb binary: 0 %ymm, and 29 FMAs, all
# scalar. A/B between two sb differing ONLY in the instruction set, best of 7 runs:
#     n-body (N=50M, --aot)   3973 ms AVX2   3986 ms SSE2   +0.3%
#     spectral-norm (N=2000)   160 ms AVX2    158 ms SSE2   -1.2%
# Both gaps are SMALLER than the null A/B (the same binary against a copy of itself: +0.7%
# and +3.8%). There is no signal.
# ⇒ Detecting the host made every build unrunnable on an older CPU - a real cost - in exchange
# for a measured zero. Detection stays available (SEDAI_CPUOPT=avx2) for whoever wants it.
# ⚠️ The vectorization that IS worth having is not here: it is in the code our own AOT/JIT
# emitters generate, which we control.
cpu_opt_level() {
    if [[ -n "$SEDAI_CPUOPT" ]]; then
        case "$(echo "$SEDAI_CPUOPT" | tr '[:upper:]' '[:lower:]')" in
            none) echo "none"; return ;;
            avx)  echo "avx";  return ;;
            avx2) echo "avx2"; return ;;
            *) echo -e "${YELLOW}  WARNING: SEDAI_CPUOPT='$SEDAI_CPUOPT' not understood, detecting instead${NC}" >&2 ;;
        esac
    fi

    # Nothing forced: the portable baseline. See the note above - the levels are detectable and
    # cost nothing to reach, they simply do not pay.
    echo "none"; return

    # ---- host detection, reachable only via SEDAI_CPUOPT=avx / =avx2 above ----
    # No /proc/cpuinfo (not Linux, or unreadable): the honest answer is the SAFE one - assume
    # nothing beyond the x86-64 baseline rather than emit instructions the CPU may not have.
    [[ -r /proc/cpuinfo ]] || { echo "none"; return; }

    local flags
    flags=" $(sed -n 's/^flags[[:space:]]*:[[:space:]]*//p' /proc/cpuinfo | head -1) "
    if [[ "$flags" == *" avx2 "* && "$flags" == *" fma "* ]]; then echo "avx2"; return; fi
    if [[ "$flags" == *" avx "*  ]]; then echo "avx";  return; fi
    echo "none"
}

# Case-insensitive test for a file: a name that matched on Windows may differ in case here,
# and a case-only mismatch reads exactly like "the dependency is not installed".
file_exists_nocase() {
    local dir="$1" name="$2"
    [[ -d "$dir" ]] || return 1
    find "$dir" -maxdepth 1 -iname "$name" -type f 2>/dev/null | grep -q .
}

# Detect SedaiAudioFoundation. Audio is enabled for sb and sbv only.
AUDIO_ENABLED=false
AUDIO_PATH=""
AUDIO_REASON=""
find_sedai_audio() {
    local requested="$1" cfg

    if [[ "$requested" == "no" ]]; then
        AUDIO_REASON="Disabled via --with-sedai-audio no"; return
    fi

    cfg="$(config_value SedaiAudioPath 2>/dev/null || true)"
    if [[ "$cfg" == "disabled" ]]; then
        AUDIO_REASON="Disabled in setup.config.json"; return
    fi

    if [[ -n "$requested" ]]; then
        if file_exists_nocase "$requested/src" "sedaiaudiofoundation.pas"; then
            AUDIO_ENABLED=true; AUDIO_PATH="$requested"; AUDIO_REASON="Explicit path: $requested"
        else
            echo -e "${YELLOW}WARNING: SedaiAudioFoundation not found at: $requested${NC}"
            AUDIO_REASON="Not found at: $requested"
        fi
        return
    fi

    if [[ -n "$cfg" ]] && file_exists_nocase "$cfg/src" "sedaiaudiofoundation.pas"; then
        AUDIO_ENABLED=true; AUDIO_PATH="$cfg"; AUDIO_REASON="From config: $cfg"; return
    fi

    if file_exists_nocase "$SCRIPT_DIR/deps/SedaiAudioFoundation/src" "sedaiaudiofoundation.pas"; then
        AUDIO_ENABLED=true; AUDIO_PATH="$SCRIPT_DIR/deps/SedaiAudioFoundation"; AUDIO_REASON="Found in deps/"; return
    fi

    local sibling
    sibling="$(dirname "$SCRIPT_DIR")/SedaiAudioFoundation"
    if file_exists_nocase "$sibling/src" "sedaiaudiofoundation.pas"; then
        AUDIO_ENABLED=true; AUDIO_PATH="$sibling"; AUDIO_REASON="Found at default: $sibling"; return
    fi

    AUDIO_REASON="Not found (use --with-sedai-audio <path> to specify)"
}

# One unit directory per BUILD CONFIGURATION, not one per platform.
#
# lib/<platform> used to be a single directory shared by every target, so the shared units (VM,
# SSA, register allocator, lexer) were compiled by whichever target ran FIRST and every later
# target REUSED them - instruction set and defines included. FPC does not recompile a unit when
# only -Cp/-Cf/-d change, so the reuse is silent.
#
# Measured 12 Aug 2026: compiling sb (audio, no AVX flags) and then sbc (no audio, -CfAVX2) into
# the same unit directory leaves SedaiBytecodeVM.o BYTE-IDENTICAL. sbc therefore shipped an engine
# with zero AVX instructions and with the audio code compiled in, while the build banner announced
# "AVX2 + FMA". Encoding the configuration in the directory name makes that impossible.
#
# WEB_MODE already had its own directory for exactly this reason; the rule is now general.
unit_dir_for() {
    local platform_dir="$1" is_web="$2" with_audio="$3" is_debug="$4"
    local name="$platform_dir"

    [[ "$is_web" == "true" ]] && name="$name-web"
    [[ "$with_audio" == "true" ]] && name="$name-audio"

    if [[ "$is_debug" == "true" ]]; then
        name="$name-debug"
    elif [[ "$CPU_OPT" != "none" ]]; then
        name="$name-$CPU_OPT"
    fi

    # Defines change unit CONTENT, so they belong in the key too - that is what used to make a
    # forgotten --clean produce a build with half the units on the old define.
    if [[ ${#DEBUG_DEFINES[@]} -gt 0 ]]; then
        name="$name-$(printf '%s,' "${DEBUG_DEFINES[@]}" | md5sum | cut -c1-6)"
    fi

    echo "$name"
}

build_target() {
    local lpr_file="$1" output_name="$2" fpc="$3" platform_dir="$4"
    local target_cpu="$5" target_os="$6" is_debug="$7" extra_path="$8"
    local with_audio="$9" is_web="${10}"

    local src_path="src/$lpr_file"
    if [[ ! -f "$src_path" ]]; then
        echo -e "${RED}ERROR: Source not found: $src_path${NC}"
        return 1
    fi

    local lib_sub_dir
    lib_sub_dir="$(unit_dir_for "$platform_dir" "$is_web" "$with_audio" "$is_debug")"

    mkdir -p "lib/$lib_sub_dir" "bin/$platform_dir"

    local opts=()
    opts+=("-o$output_name")
    opts+=("-P$target_cpu")
    opts+=("-T$target_os")
    opts+=("-MObjFPC")

    if [[ "$is_debug" == "false" ]]; then
        # SEDAI_OLEVEL / SEDAI_OEXTRA: build-time knobs for measuring optimisation settings without
        # editing this file. Not a supported configuration - the shipped default is the line below.
        opts+=("${SEDAI_OLEVEL:--O1}")
        [[ -n "${SEDAI_OEXTRA:-}" ]] && opts+=(${SEDAI_OEXTRA})

        # Instruction set from what the CPU HAS. Audio does NOT decide it, on any platform.
        #
        # The old rule skipped these flags whenever audio was on, citing an "SDL2 audio API
        # conflict" (a one-line note from 5 Jan 2026, never verified). Since sb and sbv are the
        # audio targets AND sb is the first one built, that rule left every shared unit - the
        # whole engine - at the SSE2 baseline.
        #
        # Measured 12 Aug 2026 on Linux: sb built with audio AND -CpCOREAVX2 -OpCOREAVX2 -CfAVX2
        # compiles, links and runs, with 1352 AVX instructions in SedaiBytecodeVM alone and 34
        # FMA in the binary. No conflict. build.ps1 does the same thing: the two scripts must
        # behave identically, so the exclusion is gone on both rather than kept on one.
        if [[ "$target_cpu" == "x86_64" ]]; then
            case "$CPU_OPT" in
                avx2) opts+=("-CpCOREAVX2" "-OpCOREAVX2" "-CfAVX2") ;;
                avx)  opts+=("-CpCOREAVX"  "-OpCOREAVX"  "-CfAVX")  ;;
                *)    : ;;   # portable x86-64 baseline (SSE2): nothing to add
            esac
        fi

        opts+=("-OoREGVAR" "-OoCSE" "-OoDFA" "-OoFASTMATH" "-OoCONSTPROP")
        [[ -n "$HOT_C_DEFINE" ]] && opts+=("$HOT_C_DEFINE")
        # -Xs strips. Keeping the symbols is what lets a sampler put a NAME on a program
        # counter; -XX (smart linking) stays either way.
        # ⛔ Dropping -Xs is NOT enough: fpc links release builds stripped anyway, so the binary
        # came out with zero symbols and the sampler had nothing to name a PC with. -gw asks for
        # DWARF explicitly. It is NOT -debug: the range and overflow checks (-Ci -Cr -Co) stay
        # off and every optimisation flag above stays on, so the timings are the release ones.
        if [[ "$SYMBOLS" == "true" ]]; then opts+=("-gw" "-XX"); else opts+=("-Xs" "-XX"); fi
    else
        opts+=("-g" "-gl" "-gw" "-Ci" "-Cr" "-Co")
    fi

    opts+=("-Fusrc" "-Fulib/$lib_sub_dir" "-FUlib/$lib_sub_dir" "-FEbin/$platform_dir")

    [[ -n "$extra_path" ]] && opts+=("-Fu$extra_path")

    for define in "${DEBUG_DEFINES[@]}"; do
        opts+=("-d$define")
    done

    [[ "$is_web" == "true" ]] && opts+=("-dWEB_MODE")

    if [[ "$with_audio" == "true" && -n "$AUDIO_PATH" ]]; then
        local audio_src="$AUDIO_PATH/src"
        local sdl2_path
        sdl2_path="$(config_value SDL2Path 2>/dev/null || true)"
        [[ -z "$sdl2_path" ]] && sdl2_path="$SCRIPT_DIR/deps/sdl2"
        opts+=("-Fu$audio_src")
        for sub in Core Platform Generators Modulators Processors Effects Voice Mixer \
                   Transport SID Players FileIO Engine Wavetable Project; do
            opts+=("-Fu$audio_src/$sub")
        done
        opts+=("-Fu$sdl2_path")
        opts+=("-dWITH_SEDAI_AUDIO")
    fi

    echo -e "${CYAN}Building $output_name...${NC}"
    echo -e "${GRAY}  units: lib/$lib_sub_dir${NC}"
    echo -e "${GRAY}  $fpc ${opts[*]} $src_path${NC}"

    if "$fpc" "${opts[@]}" "$src_path"; then
        echo -e "${GREEN}  OK: bin/$platform_dir/$output_name${NC}"
        verify_instruction_set "bin/$platform_dir/$output_name" "$is_debug" "$target_cpu" "$target_os"
        return 0
    else
        echo -e "${RED}  FAILED${NC}"
        return 1
    fi
}

# The banner says which instruction set was CHOSEN. It used to say "AVX2 + FMA" over binaries
# that contained not one AVX instruction, and nothing in the build made that visible. So look at
# the binary that was actually produced and say what is in it.
verify_instruction_set() {
    local binary="$1" is_debug="$2" target_cpu="$3" target_os="$4"

    [[ "$is_debug" == "true" || "$target_cpu" != "x86_64" ]] && return 0
    [[ "$target_os" == "$(detect_os)" ]] || return 0        # cannot disassemble a foreign binary here
    command -v objdump >/dev/null 2>&1 || return 0

    local vex
    vex="$(objdump -d "$binary" 2>/dev/null | grep -cP '\tv[a-z0-9]+' || true)"

    if [[ "$CPU_OPT" == "none" ]]; then
        echo -e "${GRAY}    baseline x86-64, $vex AVX instructions${NC}"
    elif [[ "$vex" -eq 0 ]]; then
        echo -e "${YELLOW}    WARNING: built for $CPU_OPT but the binary has NO AVX instruction.${NC}"
        echo -e "${YELLOW}    Shared units were reused from another configuration - build with --clean.${NC}"
    else
        echo -e "${GRAY}    $vex AVX instructions${NC}"
    fi
}

clean_build() {
    local platform_dir="$1"
    echo -e "${YELLOW}Cleaning...${NC}"
    # Every configuration variant: lib/<platform>, -web, -audio, -avx2, -debug, ...
    local d
    for d in "lib/$platform_dir"*/; do
        [[ -d "$d" ]] && rm -f "$d"/*.{ppu,o,a} 2>/dev/null
    done
    # Don't delete executables, just the units
    rm -f "bin/$platform_dir"/*.{ppu,o} 2>/dev/null || true
    return 0
}

# Parse args
while [[ $# -gt 0 ]]; do
    case "$1" in
        --help|-h) show_help; exit 0 ;;
        --debug) DEBUG=true; shift ;;
        --clean) CLEAN=true; shift ;;
        --window) WINDOW=true; shift ;;
        --no-banner) NO_BANNER=true; shift ;;
        --select-fpc) SELECT_FPC=true; shift ;;
        --hot-c) HOT_C=true; HOT_C_EXPLICIT=true; shift ;;
        --no-hot-c) HOT_C=false; shift ;;
        --symbols|--no-strip) SYMBOLS=true; shift ;;
        --cpu) CPU="$2"; shift 2 ;;
        --os) OS="$2"; shift 2 ;;
        --with-sedai-audio) WITH_SEDAI_AUDIO="$2"; shift 2 ;;
        --debug-flags) DEBUG_FLAGS="$2"; shift 2 ;;
        all|sb|sbc|sbd|sbv|sbw) TARGET="$1"; shift ;;
        *) echo -e "${RED}Unknown: $1${NC}"; show_help; exit 1 ;;
    esac
done

cd "$SCRIPT_DIR"

if [[ "$NO_BANNER" == "false" ]]; then
    echo ""
    echo -e "${CYAN}SedaiBasic Build System${NC}"
    echo -e "${CYAN}=======================${NC}"
    echo ""
fi

[[ -z "$CPU" ]] && CPU="$(detect_cpu)"
[[ -z "$OS" ]] && OS="$(detect_os)"
PLATFORM_DIR="$CPU-$OS"

FPC="$(find_fpc "$PLATFORM_DIR")" || exit 1

# ⭐ The hot arithmetic/branch/array opcodes, compiled by a C compiler instead of by FPC. ON BY DEFAULT
# since 20 Aug 2026 and never required: with --no-hot-c the interpreter is exactly the Pascal loop it
# has always been. (This comment said "opt-in" long after it stopped being true.)
# Why it exists: measured on the same dispatch loop, gcc -O2 runs it in 253 ms against FPC's 443,
# and no FPC optimisation level closes any of that - see src/hotdisp.c.
HOT_C_DEFINE=""
if [[ "$HOT_C" == "true" ]]; then
    CC_BIN="${SEDAI_CC:-}"
    # CROSS-COMPILING. The object has to be built for the TARGET, not for this machine: FPC's {$L}
    # links a COFF object on win64 and an ELF one on linux, and handing it the wrong format fails at
    # link time with no useful message.
    # ⛔ The object is NOT freestanding, whatever this comment used to claim: since the trigonometry
    # moved to the libc on 22 Aug 2026 it calls sin/cos/tan, and `nm -u` lists them. On Unix they bind
    # to libm, which the RTL has already pulled; on win64 there is neither libm nor an msvcrt import
    # library, so hotdisp.c routes them through three symbols SedaiBytecodeVM exports (see the #ifdef
    # _WIN32 at the top of it). Until that was done the win64 cross-build simply did not link.
    if [[ -z "$CC_BIN" && "$OS" == "win64" ]]; then
        CC_BIN="$(command -v x86_64-w64-mingw32-gcc 2>/dev/null || true)"
        if [[ -z "$CC_BIN" ]]; then
            if [[ "$HOT_C_EXPLICIT" == "true" ]]; then
                echo -e "${RED}ERROR: --hot-c for win64 needs x86_64-w64-mingw32-gcc.${NC}" >&2
                echo -e "${GRAY}  Debian/Ubuntu: sudo apt install gcc-mingw-w64-x86-64-win32${NC}" >&2
                exit 1
            fi
            echo -e "${YELLOW}NOTE: no x86_64-w64-mingw32-gcc - building win64 WITHOUT the C hot loop.${NC}" >&2
            echo -e "${GRAY}  sudo apt install gcc-mingw-w64-x86-64-win32   (or pass --no-hot-c to silence this)${NC}" >&2
            HOT_C=false
        fi
    fi
    if [[ -z "$CC_BIN" && "$OS" == "win32" ]]; then
        # ⛔ win32 decorates a cdecl symbol with a LEADING UNDERSCORE, which win64 does not, so the
        # external declarations in SedaiBytecodeVM.pas would not resolve. Refused rather than failing
        # obscurely at link time.
        if [[ "$HOT_C_EXPLICIT" == "true" ]]; then
            echo -e "${RED}ERROR: --hot-c is not supported for win32 (leading-underscore cdecl names).${NC}" >&2
            exit 1
        fi
        HOT_C=false      # not an error when it is merely the default
    fi
    if [[ "$HOT_C" == "true" && -z "$CC_BIN" ]]; then
        for c in gcc clang cc; do
            command -v "$c" >/dev/null 2>&1 && { CC_BIN="$c"; break; }
        done
    fi
    if [[ "$HOT_C" == "true" && -z "$CC_BIN" ]]; then
        if [[ "$HOT_C_EXPLICIT" == "true" ]]; then
            echo -e "${RED}ERROR: --hot-c needs a C compiler and none was found.${NC}" >&2
            echo -e "${YELLOW}Install gcc or clang, or set SEDAI_CC=<path>. On Windows: MinGW-w64.${NC}" >&2
            exit 1
        fi
        # Merely the default: say so once and build the pure-Pascal interpreter, which is what this
        # was before 20 Aug 2026 and is still correct - just slower on the arms the C loop covers.
        echo -e "${YELLOW}NOTE: no C compiler found - building WITHOUT the C hot loop.${NC}" >&2
        echo -e "${GRAY}  Install gcc or clang for 27-45% on arithmetic-heavy programs, or pass --no-hot-c.${NC}" >&2
        HOT_C=false
    fi
    if [[ "$HOT_C" == "true" ]]; then
    echo -e "${GRAY}C compiler: $("$CC_BIN" --version 2>/dev/null | head -1) - $CC_BIN${NC}"
    # -fno-math-errno is REQUIRED, not a tuning knob: without it gcc assumes llrint/sqrt may touch
    # errno and emits CALLS to libm, which a freestanding object linked into an FPC program cannot
    # resolve. With it they are cvtsd2si and sqrtsd, one instruction each.
    # ⭐ -fno-crossjumping IS THE ONE THAT MATTERS, and it took perf to find it. The dispatch macro
    # NEXT ends every arm with its own "goto *disp[]" - the replicated dispatch that threaded
    # interpreters use so each arm gets its OWN branch-predictor history ("after ArrayLoad comes
    # AddInt") instead of all of them sharing one saturated entry. gcc undoes it: cross-jumping
    # merges identical tails, and 96 NEXT in the source came out as THREE indirect jumps in the
    # assembly. The technique was written down and compiled away.
    #
    # Measured 21 Aug 2026 with perf, which is also what corrected the diagnosis: the problem was
    # never the I-cache (300-500 k misses against 29 G instructions) but BRANCH MISPREDICTION -
    # adding four arms took it from 17.2 M to 24.2 M. With the flag: 97 indirect jumps, misses back
    # to 21.0 M, cycles 7.33 G -> 6.37 G, which is below where the session started.
    #     spectral-norm -16.1%   fannkuch-redux -12.2%   n-body -8.1%
    #     binary-trees-arena N=21 -6.6% (the regression this session had opened)   binary-trees -4.1%
    # ⚠️ It costs SIZE: .text goes 6760 -> 11784 bytes, +74%. Worth re-deciding for the MCU target,
    # where size is the binding constraint - the flag is one word to remove.
    #
    # -falign-labels/-falign-jumps: the dispatch arms are jump targets reached by an indirect jump,
    # and where each one LANDS in the instruction cache turned out to matter more than what it
    # contains. Measured 21 Aug 2026: adding four record arms left n-body executing byte-identical
    # work in the C loop - same entries, same instruction count, verified with HOTC_DIAG=1 - and
    # still moved it 10%, purely because the later arms shifted. Aligning the labels removes that
    # sensitivity and pays on its own: n-body -12.0%, spectral-norm -8.5%, binary-trees-arena -2.2%,
    # binary-trees -0.3%. A layout effect is machine-specific by nature, so re-measure it before
    # trusting the numbers on a different CPU.
    "$CC_BIN" -O2 -ffreestanding -fno-math-errno -falign-labels=32 -falign-jumps=32 -fno-crossjumping -c -o "$SCRIPT_DIR/src/hotdisp.o" "$SCRIPT_DIR/src/hotdisp.c" || {
        echo -e "${RED}ERROR: could not compile src/hotdisp.c${NC}" >&2; exit 1; }
    HOT_C_DEFINE="-dHOT_C"
    fi
fi

echo -e "${GRAY}Compiler: FPC $("$FPC" -iV 2>/dev/null) - $FPC${NC}"
echo -e "${GRAY}Platform: $PLATFORM_DIR${NC}"
echo -e "${GRAY}Mode: $(if [[ "$DEBUG" == "true" ]]; then echo Debug; else echo Release; fi)${NC}"

find_sedai_audio "$WITH_SEDAI_AUDIO"
if [[ "$AUDIO_ENABLED" == "true" ]]; then
    echo -e "${GREEN}SedaiAudio: ENABLED ($AUDIO_REASON)${NC}"
else
    echo -e "${GRAY}SedaiAudio: disabled ($AUDIO_REASON)${NC}"
fi

# Decide the instruction set ONCE and say so.
CPU_OPT="$(cpu_opt_level)"
case "$CPU_OPT" in
    avx2) echo -e "${GREEN}CPU opt:    AVX2 + FMA${NC}" ;;
    avx)  echo -e "${GREEN}CPU opt:    AVX (no AVX2/FMA on this CPU)${NC}" ;;
    *)    echo -e "${GRAY}CPU opt:    baseline x86-64 (SSE2) - no AVX detected${NC}" ;;
esac
# The level is a property of the machine that COMPILES, not of the project. The flags reach the
# shared units, so binaries built with them really do carry those instructions - and die with an
# illegal instruction on a CPU that lacks them. Only reachable now by asking for it explicitly.
if [[ "$CPU_OPT" != "none" ]]; then
    echo -e "${GRAY}            FORCED via SEDAI_CPUOPT - binaries will not run on a CPU without it${NC}"
    echo -e "${GRAY}            (measured 15 Aug: worth 0% here, FPC emits no vector code)${NC}"
fi

# Debug flags -> defines
DEBUG_DEFINES=()
if [[ -n "$DEBUG_FLAGS" ]]; then
    IFS=',' read -ra _flags <<< "$(echo "$DEBUG_FLAGS" | tr '[:lower:]' '[:upper:]')"
    for f in "${_flags[@]}"; do
        f="$(echo "$f" | tr -d '[:space:]')"
        [[ -z "$f" ]] && continue
        if [[ "$f" == "ALL" ]]; then DEBUG_DEFINES+=("DEBUG_ALL"); else DEBUG_DEFINES+=("DEBUG_$f"); fi
    done
    echo -e "${MAGENTA}Debug flags: ${DEBUG_DEFINES[*]}${NC}"
fi

if [[ "$WINDOW" == "true" ]]; then
    DEBUG_DEFINES+=("WITH_WINDOW")
    echo -e "${MAGENTA}Window presenter: ENABLED (sb --window available)${NC}"
fi

echo ""

[[ "$CLEAN" == "true" ]] && clean_build "$PLATFORM_DIR"

# ============================================================================
#  DEPENDENCY PREFLIGHT
#
#  ⛔ ONE REPORT, NOT ONE ERROR AT A TIME. The build used to discover a missing
#  dependency the way a compiler does - the first one that stops it - so a person
#  setting the project up installed something, ran it again, and was told about the
#  next one. Four rounds to learn four package names is the difference between a
#  project you can try and one you give up on.
#
#  Everything is checked BEFORE anything is compiled, and the whole answer is
#  printed at once with a single command that installs all of it.
#
#  ⚠️ WHAT IS NEEDED IS NOT A FIXED LIST: it depends on what was detected. With
#  SedaiAudioFoundation present, even the plain `sb` links libSDL2 (the audio
#  backend uses it), and without it `sb` needs no SDL2 at all. So the report is
#  built from the options as they finally stand, after auto-detection.
# ============================================================================

DEP_NAMES=(); DEP_STATE=(); DEP_WHY=(); DEP_REQ=(); DEP_PKG_APT=(); DEP_PKG_DNF=()
DEP_PKG_PACMAN=(); DEP_PKG_ZYPPER=(); DEP_PKG_APK=(); DEP_PKG_BREW=()

# dep_add <name> <ok|missing> <required|optional> <why> <apt> <dnf> <pacman> <zypper> <apk> <brew>
dep_add() {
    DEP_NAMES+=("$1"); DEP_STATE+=("$2"); DEP_REQ+=("$3"); DEP_WHY+=("$4")
    DEP_PKG_APT+=("$5"); DEP_PKG_DNF+=("$6"); DEP_PKG_PACMAN+=("$7")
    DEP_PKG_ZYPPER+=("$8"); DEP_PKG_APK+=("$9"); DEP_PKG_BREW+=("${10}")
}

# Collect every dependency the SELECTED targets and options actually need.
collect_deps() {
    local t needs_sdl_link=false needs_bindings=false wants_sdl_runtime=false

    for t in "${BUILD_LIST[@]}"; do
        case "$t" in
            sbv) needs_sdl_link=true; needs_bindings=true; wants_sdl_runtime=true ;;
        esac
    done
    # ⭐ SDL2 IS THE AUDIO BACKEND AS WELL AS THE VIDEO ONE - SedaiAudioFoundation opens its device
    # with SDL_OpenAudioDevice and feeds it with SDL_QueueAudio. So with the audio library enabled
    # EVERY target that links it needs libSDL2, "sb" included. This is the case that makes a static
    # dependency list wrong, and it is why the report is built from what was actually detected.
    [[ "$AUDIO_ENABLED" == "true" ]] && { needs_sdl_link=true; needs_bindings=true; }
    # "sb --window" loads SDL2 at RUN time (SedaiSDL2Dyn calls LoadLibrary), so it needs nothing to
    # build - but a user who builds it and finds no window would rather be told now.
    [[ "$WINDOW" == "true" ]] && wants_sdl_runtime=true

    if [[ "$HOT_C" == "true" ]]; then
        local ccname="a C compiler (gcc or clang)"
        if [[ -n "${SEDAI_CC:-}" ]] || command -v cc >/dev/null 2>&1 || command -v gcc >/dev/null 2>&1 \
           || command -v clang >/dev/null 2>&1; then
            dep_add "$ccname" ok optional "the C hot loop" gcc gcc gcc gcc gcc ""
        else
            dep_add "$ccname" missing optional \
                "the C hot loop - WITHOUT IT THE INTERPRETER IS 27-45% SLOWER, but it still builds" \
                gcc gcc gcc gcc gcc ""
        fi
    fi

    if [[ "$needs_bindings" == "true" ]]; then
        local b="$SDL2_FOR_TARGETS/sdl2.pas"
        if [[ -f "$b" ]]; then
            dep_add "SDL2 Pascal bindings" ok required "compiling the SDL2 units" "" "" "" "" "" ""
        else
            dep_add "SDL2 Pascal bindings ($SDL2_FOR_TARGETS)" missing required \
                "compiling the SDL2 units - they ship in deps/sdl2, see INSTALL.md" "" "" "" "" "" ""
        fi
    fi

    if [[ "$needs_sdl_link" == "true" ]]; then
        if have_shared_lib sdl2 libSDL2.so; then
            dep_add "libSDL2 (development)" ok required "GRAPHICS and AUDIO - the window, every drawing primitive, every sound" \
                    libsdl2-dev SDL2-devel sdl2 libSDL2-devel sdl2-dev sdl2
        else
            dep_add "libSDL2 (development)" missing required "GRAPHICS and AUDIO - without it there is no window, no drawing and no sound" \
                    libsdl2-dev SDL2-devel sdl2 libSDL2-devel sdl2-dev sdl2
        fi
        if have_shared_lib SDL2_ttf libSDL2_ttf.so; then
            dep_add "libSDL2_ttf (development)" ok required "the text renderer SDL2 draws characters with" \
                    libsdl2-ttf-dev SDL2_ttf-devel sdl2_ttf libSDL2_ttf-devel sdl2_ttf-dev sdl2_ttf
        else
            dep_add "libSDL2_ttf (development)" missing required "the text renderer SDL2 draws characters with" \
                    libsdl2-ttf-dev SDL2_ttf-devel sdl2_ttf libSDL2_ttf-devel sdl2_ttf-dev sdl2_ttf
        fi
    elif [[ "$wants_sdl_runtime" == "true" ]]; then
        # Needed to RUN, not to build: report it, never refuse the build for it.
        if have_shared_lib sdl2 libSDL2.so || [[ -n "$(ldconfig -p 2>/dev/null | grep -m1 libSDL2-2.0.so.0)" ]]; then
            dep_add "libSDL2 + libSDL2_ttf (runtime)" ok optional "graphics and audio at RUN time" \
                    "libsdl2-2.0-0 libsdl2-ttf-2.0-0" "SDL2 SDL2_ttf" "sdl2 sdl2_ttf" \
                    "libSDL2-2_0-0 libSDL2_ttf-2_0-0" "sdl2 sdl2_ttf" "sdl2 sdl2_ttf"
        else
            dep_add "libSDL2 + libSDL2_ttf (runtime)" missing optional \
                    "graphics and audio at RUN time - it builds without them, then has no window and no sound" \
                    "libsdl2-2.0-0 libsdl2-ttf-2.0-0" "SDL2 SDL2_ttf" "sdl2 sdl2_ttf" \
                    "libSDL2-2_0-0 libSDL2_ttf-2_0-0" "sdl2 sdl2_ttf" "sdl2 sdl2_ttf"
        fi
    fi
}

# Print the one report. Answers 1 when a REQUIRED dependency is missing.
report_deps() {
    local i n missing_req=0 missing_any=0 pm pkgs="" p
    n=${#DEP_NAMES[@]}
    [[ $n -eq 0 ]] && return 0
    for ((i=0; i<n; i++)); do
        [[ "${DEP_STATE[$i]}" == "missing" ]] || continue
        missing_any=1
        [[ "${DEP_REQ[$i]}" == "required" ]] && missing_req=1
    done
    # Nothing to say when everything is there: a check that talks when it has no news is noise.
    [[ $missing_any -eq 0 ]] && return 0

    echo "" >&2
    echo -e "${CYAN}Dependency check${NC}" >&2
    echo -e "${CYAN}================${NC}" >&2
    for ((i=0; i<n; i++)); do
        if [[ "${DEP_STATE[$i]}" == "ok" ]]; then
            printf "  ${GREEN}[ok]${NC}      %-34s %s\n" "${DEP_NAMES[$i]}" "${DEP_WHY[$i]}" >&2
        elif [[ "${DEP_REQ[$i]}" == "required" ]]; then
            printf "  ${RED}[MISSING]${NC} %-34s %s\n" "${DEP_NAMES[$i]}" "${DEP_WHY[$i]}" >&2
        else
            printf "  ${YELLOW}[missing]${NC} %-34s %s\n" "${DEP_NAMES[$i]}" "${DEP_WHY[$i]}" >&2
        fi
    done

    pm="$(pkg_manager)"
    for ((i=0; i<n; i++)); do
        [[ "${DEP_STATE[$i]}" == "missing" ]] || continue
        case "$pm" in
            apt)    p="${DEP_PKG_APT[$i]}" ;;    dnf)  p="${DEP_PKG_DNF[$i]}" ;;
            pacman) p="${DEP_PKG_PACMAN[$i]}" ;; zypper) p="${DEP_PKG_ZYPPER[$i]}" ;;
            apk)    p="${DEP_PKG_APK[$i]}" ;;    brew) p="${DEP_PKG_BREW[$i]}" ;;
            *)      p="" ;;
        esac
        [[ -n "$p" ]] && pkgs="$pkgs $p"
    done
    if [[ -n "$pkgs" ]]; then
        echo "" >&2
        echo -e "${CYAN}  Install all of them at once:${NC}" >&2
        case "$pm" in
            apt)    echo -e "      ${GREEN}sudo apt install$pkgs${NC}" >&2 ;;
            dnf)    echo -e "      ${GREEN}sudo dnf install$pkgs${NC}" >&2 ;;
            pacman) echo -e "      ${GREEN}sudo pacman -S$pkgs${NC}" >&2 ;;
            zypper) echo -e "      ${GREEN}sudo zypper install$pkgs${NC}" >&2 ;;
            apk)    echo -e "      ${GREEN}sudo apk add$pkgs${NC}" >&2 ;;
            brew)   echo -e "      ${GREEN}brew install$pkgs${NC}" >&2 ;;
        esac
    fi
    echo "" >&2
    echo -e "${GRAY}  Full instructions, other distributions and Windows: INSTALL.md${NC}" >&2
    echo "" >&2
    return $missing_req
}

# Targets: lpr : output : extra unit path : supports audio : is web
SDL2_FOR_TARGETS="$(config_value SDL2Path 2>/dev/null || true)"
[[ -z "$SDL2_FOR_TARGETS" ]] && SDL2_FOR_TARGETS="./deps/sdl2"
declare -A TARGETS=(
    [sb]="SedaiBasicVM.lpr:sb::true:false"
    [sbc]="SedaiBasicCompiler.lpr:sbc::false:false"
    [sbd]="SedaiBasicDisassembler.lpr:sbd::false:false"
    [sbv]="SedaiVision.lpr:sbv:$SDL2_FOR_TARGETS:true:false"
    [sbw]="SedaiBasicWeb.lpr:sbw::false:true"
)

BUILD_LIST=()
if [[ "$TARGET" == "all" ]]; then
    BUILD_LIST=("sb" "sbc" "sbd" "sbv" "sbw")
else
    BUILD_LIST=("$TARGET")
fi

collect_deps
if ! report_deps; then
    echo -e "${RED}Missing required dependencies - nothing was built.${NC}" >&2
    exit 1
fi

echo -e "${CYAN}Building Targets...${NC}"
echo -e "${CYAN}===================${NC}"

SUCCESS=0; FAILED=0
for t in "${BUILD_LIST[@]}"; do
    IFS=':' read -r lpr output extra supports_audio is_web <<< "${TARGETS[$t]}"

    # Executables carry .exe only on Windows targets
    [[ "$OS" == win* ]] && output="$output.exe"

    use_audio=false
    [[ "$AUDIO_ENABLED" == "true" && "$supports_audio" == "true" ]] && use_audio=true

    if build_target "$lpr" "$output" "$FPC" "$PLATFORM_DIR" "$CPU" "$OS" "$DEBUG" \
                    "$extra" "$use_audio" "$is_web"; then
        SUCCESS=$((SUCCESS+1))
    else
        FAILED=$((FAILED+1))
    fi
done

echo ""
echo -e "${CYAN}Build Summary${NC}"
echo -e "${CYAN}=============${NC}"
echo -e "${GREEN}  Successful: $SUCCESS${NC}"
if [[ $FAILED -gt 0 ]]; then
    echo -e "${RED}  Failed: $FAILED${NC}"
    echo ""
    echo -e "${YELLOW}Build completed with errors.${NC}"
else
    echo ""
    echo -e "${GREEN}Build completed successfully!${NC}"
fi

exit $FAILED
