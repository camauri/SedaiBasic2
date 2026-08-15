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

# Defaults
TARGET="all"
DEBUG=false
CLEAN=false
WINDOW=false
NO_BANNER=false
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
    echo "  --help                   Show this help"
    echo ""
    echo "Environment:"
    echo "  SEDAI_FPC=<path>         Use this fpc binary"
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

# Locate fpc. On this machine it is NOT on PATH and NOT under /usr: it lives in the user's
# home (~/tools/fp/...). An absence from PATH is not evidence that fpc is missing, so the
# home is searched before giving up - and if it is still not found, the user is asked.
find_fpc() {
    local platform="$1"
    local candidate

    # 1. Explicit environment override
    if [[ -n "$SEDAI_FPC" && -x "$SEDAI_FPC" ]]; then
        echo "$SEDAI_FPC"; return 0
    fi

    # 2. setup.config.json
    local cfg
    cfg="$(config_value FpcPath 2>/dev/null || true)"
    if [[ -n "$cfg" && -x "$cfg/bin/$platform/fpc" ]]; then
        echo "$cfg/bin/$platform/fpc"; return 0
    fi

    # 3. Project-local FPC (installed by setup.sh) - always preferred over a system one
    candidate="$SCRIPT_DIR/fpc/3.2.2/bin/$platform/fpc"
    [[ -x "$candidate" ]] && { echo "$candidate"; return 0; }

    # 4. Known installations under the user's home (fpcupdeluxe layout)
    for candidate in "$HOME"/tools/fp/*/fpc/bin/"$platform"/fpc \
                     "$HOME"/fpcupdeluxe/fpc/bin/"$platform"/fpc; do
        [[ -x "$candidate" ]] && { echo "$candidate"; return 0; }
    done

    # 5. System PATH
    candidate="$(command -v fpc 2>/dev/null || true)"
    [[ -n "$candidate" ]] && { echo "$candidate"; return 0; }

    # 6. Last resort: look through the home before declaring it missing
    candidate="$(find "$HOME" -maxdepth 6 -type f -name fpc -perm -u+x 2>/dev/null | head -1)"
    [[ -n "$candidate" ]] && { echo "$candidate"; return 0; }

    return 1
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
        opts+=("-O1")

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
        opts+=("-Xs" "-XX")
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

FPC="$(find_fpc "$PLATFORM_DIR")" || {
    echo -e "${RED}ERROR: Free Pascal Compiler (fpc) not found!${NC}"
    echo -e "${YELLOW}Looked in: \$SEDAI_FPC, setup.config.json, fpc/3.2.2/, ~/tools/fp/*/fpc/, PATH, \$HOME.${NC}"
    echo -e "${YELLOW}Set SEDAI_FPC=<path to fpc> or install it.${NC}"
    exit 1
}

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
