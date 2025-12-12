#!/bin/bash
#
# SedaiBasic Build Script
# Copyright (c) 2025 Maurizio Cammalleri
# Released under GNU GPL v3 or Commercial License
#
# Usage:
#   ./build.sh              # Build all targets (release)
#   ./build.sh sb           # Build only sb
#   ./build.sh --debug      # Build with debug info
#   ./build.sh --clean      # Clean and rebuild

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Defaults
TARGET="all"
DEBUG=false
CLEAN=false

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
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
    echo ""
    echo "Options:"
    echo "  --debug     Build with debug info"
    echo "  --clean     Clean build artifacts before building"
    echo "  --help      Show this help"
}

detect_platform() {
    local cpu os
    case "$(uname -m)" in
        x86_64|amd64) cpu="x86_64" ;;
        i386|i686)    cpu="i386" ;;
        aarch64|arm64) cpu="aarch64" ;;
        *)            cpu="unknown" ;;
    esac
    case "$(uname -s)" in
        Linux)  os="linux" ;;
        Darwin) os="darwin" ;;
        *)      os="unknown" ;;
    esac
    echo "$cpu-$os"
}

find_fpc() {
    local locations=("/usr/bin/fpc" "/usr/local/bin/fpc" "/opt/fpc/bin/fpc")
    for loc in "${locations[@]}"; do
        if [[ -x "$loc" ]]; then
            echo "$loc"
            return 0
        fi
    done
    command -v fpc 2>/dev/null && return 0
    return 1
}

build_target() {
    local lpr_file="$1"
    local output_name="$2"
    local fpc="$3"
    local platform_dir="$4"
    local target_cpu="$5"
    local target_os="$6"
    local is_debug="$7"
    local extra_path="$8"

    local src_path="src/$lpr_file"
    if [[ ! -f "$src_path" ]]; then
        echo -e "${RED}ERROR: Source not found: $src_path${NC}"
        return 1
    fi

    mkdir -p "lib/$platform_dir" "bin/$platform_dir"

    local opts=()
    opts+=("-o$output_name")
    opts+=("-P$target_cpu")
    opts+=("-T$target_os")
    opts+=("-MObjFPC")

    if [[ "$is_debug" == "false" ]]; then
        opts+=("-O1")
        if [[ "$target_cpu" == "x86_64" ]]; then
            opts+=("-CpCOREAVX2" "-OpCOREAVX2" "-CfAVX2")
        fi
        opts+=("-OoREGVAR" "-OoCSE" "-OoDFA" "-OoFASTMATH" "-OoCONSTPROP")
        opts+=("-Xs" "-XX")
    else
        opts+=("-g" "-gl" "-gw" "-Ci" "-Cr" "-Co")
    fi

    opts+=("-Fusrc" "-Fulib/$platform_dir" "-FUlib/$platform_dir" "-FEbin/$platform_dir")

    [[ -n "$extra_path" ]] && opts+=("-Fu$extra_path")

    echo -e "${CYAN}Building $output_name...${NC}"
    echo -e "${GRAY}  $fpc ${opts[*]} $src_path${NC}"

    if "$fpc" "${opts[@]}" "$src_path"; then
        echo -e "${GREEN}  OK: bin/$platform_dir/$output_name${NC}"
        return 0
    else
        echo -e "${RED}  FAILED${NC}"
        return 1
    fi
}

clean_build() {
    local platform_dir="$1"
    echo -e "${YELLOW}Cleaning...${NC}"
    rm -f "lib/$platform_dir"/*.{ppu,o,a} 2>/dev/null || true
    rm -f "bin/$platform_dir"/*.{ppu,o} 2>/dev/null || true
}

# Parse args
while [[ $# -gt 0 ]]; do
    case "$1" in
        --help|-h) show_help; exit 0 ;;
        --debug) DEBUG=true; shift ;;
        --clean) CLEAN=true; shift ;;
        all|sb|sbc|sbd|sbv) TARGET="$1"; shift ;;
        *) echo -e "${RED}Unknown: $1${NC}"; exit 1 ;;
    esac
done

cd "$SCRIPT_DIR"

echo -e "${CYAN}SedaiBasic Build System${NC}"

FPC=$(find_fpc) || { echo -e "${RED}FPC not found!${NC}"; exit 1; }
echo -e "${GRAY}FPC: $FPC${NC}"

PLATFORM_DIR=$(detect_platform)
TARGET_CPU="${PLATFORM_DIR%-*}"
TARGET_OS="${PLATFORM_DIR#*-}"
echo -e "${GRAY}Platform: $PLATFORM_DIR${NC}"

[[ "$CLEAN" == "true" ]] && clean_build "$PLATFORM_DIR"

# Targets: name -> lpr:output:extra_path
declare -A TARGETS=(
    [sb]="SedaiBasicVM.lpr:sb:"
    [sbc]="SedaiBasicCompiler.lpr:sbc:"
    [sbd]="SedaiBasicDisassembler.lpr:sbd:"
    [sbv]="SedaiVision.lpr:sbv:./deps/sdl2"
)

BUILD_LIST=()
if [[ "$TARGET" == "all" ]]; then
    BUILD_LIST=("sb" "sbc" "sbd" "sbv")
else
    BUILD_LIST=("$TARGET")
fi

SUCCESS=0; FAILED=0
for t in "${BUILD_LIST[@]}"; do
    IFS=':' read -r lpr output extra <<< "${TARGETS[$t]}"
    if build_target "$lpr" "$output" "$FPC" "$PLATFORM_DIR" "$TARGET_CPU" "$TARGET_OS" "$DEBUG" "$extra"; then
        ((SUCCESS++))
    else
        ((FAILED++))
    fi
done

echo ""
if [[ $FAILED -eq 0 ]]; then
    echo -e "${GREEN}Done: $SUCCESS ok, $FAILED failed${NC}"
else
    echo -e "${YELLOW}Done: $SUCCESS ok, $FAILED failed${NC}"
fi
exit $FAILED
