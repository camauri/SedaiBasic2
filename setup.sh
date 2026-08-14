#!/bin/bash
#
# SedaiBasic2 Setup Script for Linux (STUB)
#
# Copyright (c) 2025 Maurizio Cammalleri
# Released under GNU GPL v3
#
# This is a placeholder script. Full implementation coming soon.
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

# ============================================================================
#  MAIN
# ============================================================================

show_banner

echo ""
echo -e "\033[33m  *** LINUX SETUP - NOT YET IMPLEMENTED ***\033[0m"
echo ""
echo "  This script is a placeholder for the Linux setup process."
echo "  Full implementation is coming soon."
echo ""
echo "  For now, please:"
echo "    1. Install FPC 3.2.2 manually from your package manager or"
echo "       https://www.freepascal.org/download.html"
echo ""
echo "    2. Compile manually with:"
echo "       fpc -osb -Px86_64 -Tlinux -MObjFPC -Scghi -CX -Si -O3 \\"
echo "           -OoREGVAR -OoCSE -OoDFA -OoFASTMATH -OoCONSTPROP \\"
echo "           -Xs -XX -Fusrc -Fu$LIB_DIR -FU$LIB_DIR -FE$BIN_DIR \\"
echo "           $SOURCE_FILE"
echo ""

exit 1
