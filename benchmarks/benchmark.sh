#!/bin/bash
#
# SedaiBasic2 benchmark suite - The Computer Language Benchmarks Game.
#
# Port of benchmark.ps1 (which stays, and stays the reference for Windows). Same measurement
# rules, same report, same verdicts - what changes is only what the platform forces:
#
#   - the binary under test is bin/<cpu>-<os>/sb, DETECTED, so this script also works from
#     git-bash on Windows where the name is sb.exe;
#   - the thermometer is a native binary built by fbc. On Windows that is nbody_fbc_o2.exe;
#     here it is nbody_fbc_o2 (no extension). Missing = the readings are simply skipped, as
#     they already were when the file was absent;
#   - the DLL staging for the ctypes references is GONE, and is not a loss: on Linux
#     ctypes.util.find_library() resolves gmp and pcre2-8 from the system, so pidigits and
#     regex-redux have the Python yardstick that a stock Windows box could not give them.
#
# Measurement rules, all of them learned the hard way on this project:
#
#   ONE CLOCK FOR EVERYONE. Wall-clock around the process, for every runtime. The internal
#   timer sb can report excludes startup and compilation, which the Python and Lua processes
#   cannot exclude, so comparing the two would not be a comparison at all.
#
#   THERMOMETER. A fixed native binary is timed before and after the session. It says how fast
#   this machine is RIGHT NOW. If the two readings disagree, the machine changed underneath the
#   session and its numbers are not comparable with anything.
#
#   COOLDOWN between runs, so run N+1 does not inherit run N's heat.
#
#   BEST-OF-N, the minimum rather than the mean.
#
#   OUTPUT VERIFIED EVERY TIME. A program that dies early looks wonderfully fast. Every run's
#   output is compared against a reference; an arm whose output does not match is reported as
#   DIFF and its time is not credited. "No reference available on this machine" is reported as
#   NO REF - which is a different thing from a failed comparison, and must never be read as a pass.
#
# Copyright (c) 2025 Maurizio Cammalleri
# Released under GNU GPL v3

set -u

BENCHMARK_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$BENCHMARK_DIR")"
TEMP_DIR="$BENCHMARK_DIR/.temp"
RESULTS_DIR="$BENCHMARK_DIR/results"
BAS_DIR="$PROJECT_ROOT/bas/07_benchmarks"
PY_DIR="$BENCHMARK_DIR/tests/py"
LUA_DIR="$BENCHMARK_DIR/tests/lua"
ORACLE_DIR="$PROJECT_ROOT/job/tests/tools"
CONFIG_FILE="$BENCHMARK_DIR/benchmark.config.json"

# Defaults
QUICK=false
RUNS=1
REF_RUNS=-1
COOLDOWN=-1
COOL_TO=0
COOL_MAX=180
ONLY=""
EXTRA=""
RUNTIME_FILTER=""
PROFILE_FILTER=""
NO_THERMOMETER=false
ASSUME_YES=false
OUTPUT=""

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
CYAN='\033[0;36m'; GRAY='\033[0;90m'; WHITE='\033[0;97m'; NC='\033[0m'

show_help() {
    sed -n '3,35p' "$0" | sed 's/^# \{0,1\}//'
    cat <<'EOF'

Usage: ./benchmark.sh [options]

  --quick              Reduced N values (seconds per run) instead of the CLBG ones
  --runs <n>           Runs per arm; the reported time is the best of them (default 1)
  --ref-runs <n>       Runs per yardstick arm (python, lua); default: same as --runs
  --cooldown <s>       Seconds to idle between runs (default: 0 with --quick, 20 otherwise)
  --cool-to <ms>       Adaptive cooldown: idle until the thermometer reads at most this
  --cool-max <s>       Cap on the extra waiting --cool-to may add per run (default 180)
  --only <a,b>         Run only the named benchmarks (an opt-in row may be named here too)
  --extra <a,b>        ADD an opt-in row to the run. Available: pidigits-basic
  --runtime <a,b>      Run only the named runtimes: sedai, python, lua
  --profile <a,b>      Run only the named sb profiles: interp, aot, jit, aotjit
  --no-thermometer     Skip the thermometer readings
  --yes                Do not ask for confirmation before a standard-size battery
  --output <file>      Custom results filename (default: results/BENCHMARK_<date>-<seconds>.md)
  --help               This text

Examples:
  ./benchmark.sh --quick
  ./benchmark.sh --runs 3 --cooldown 30
  ./benchmark.sh --only n-body,spectral-norm --runtime sedai
EOF
}

# ============================================================================
#  PLATFORM - where the binaries are, and what they are called
# ============================================================================

detect_platform_dir() {
    local cpu os
    case "$(uname -m)" in
        x86_64|amd64) cpu="x86_64" ;; i386|i686) cpu="i386" ;;
        aarch64|arm64) cpu="aarch64" ;; *) cpu="unknown" ;;
    esac
    case "$(uname -s)" in
        Linux) os="linux" ;; Darwin) os="darwin" ;;
        MINGW*|MSYS*|CYGWIN*) os="win64" ;; *) os="unknown" ;;
    esac
    echo "$cpu-$os"
}

PLATFORM_DIR="$(detect_platform_dir)"
EXE_SUFFIX=""
[[ "$PLATFORM_DIR" == *win* ]] && EXE_SUFFIX=".exe"

SB_EXE="$PROJECT_ROOT/bin/$PLATFORM_DIR/sb$EXE_SUFFIX"
THERMO_EXE="$PROJECT_ROOT/job/tests/bench/nbody_fbc_o2$EXE_SUFFIX"
# Fall back to the Windows-named thermometer only when it is actually runnable here.
[[ -x "$THERMO_EXE" ]] || THERMO_EXE="$PROJECT_ROOT/job/tests/bench/nbody_fbc_o2"

# ============================================================================
#  THE SUITE
# ============================================================================
#
#  Fields, pipe-separated:
#    name | bas | stdN | quickN | stdinN | stdinQuickN | py | lua | verify | oracle | desc
#
#  stdN/quickN empty  => the program reads a FASTA stream on stdin, generated by running the
#                        fasta reference at stdinN (stdinQuickN under --quick).
#  verify             => where the expected output comes from: python, lua or oracle.
#                        pidigits and regex-redux are checked against a pure-Python oracle
#                        running the same algorithm; an oracle is a CORRECTNESS check only and
#                        is never timed. k-nucleotide's Python version needs a working
#                        multiprocessing fork; Lua answers instead.
#
#  pidigits HAS AN OPT-IN TWIN, AND THE DEFAULT TABLE DOES NOT CARRY IT (22 Aug 2026, user's call).
#  `pidigits` asks the LANGUAGE for the arbitrary-precision arithmetic (the BigInt type). That is the
#  comparison the reference invites: the Python entry does not use CPython's integers either, it calls
#  GMP through ctypes, and the Lua entry calls `bn`. Library against library.
#  `pidigits-basic` times the same spigot with the arithmetic written out by hand over a base-10^9
#  limb array - our BASIC against those same C libraries. It earned its place on 2026-08-15, when the
#  suite ran ONLY that program under the bare name `pidigits` and reported us 8x slower than Python on
#  a benchmark where the type is faster; a second row was how that error was made visible and kept
#  visible.
#  ⇒ That job is done, and what the row measures now - a BASIC program against GMP - says nothing
#  about the engine that the other ten rows do not say better, while carrying a permanent 0.04x into
#  the table. So it moved to SUITE_EXTRA: still there, still correct, run with
#      benchmarks/benchmark.sh --extra pidigits-basic
#  bas/07_benchmarks/pidigits-modern.bas is also in the regression corpus, so the PROGRAM stays
#  verified whether or not anybody times it.

SUITE=(
# ⭐ THE ARENA VARIANT IS THE ENTRY, not the New/Delete one. Same algorithm and byte-identical
# output - every tree is really built node by node and really walked to count it - but the nodes of
# a tree are consecutive slots of an integer array and a child is an INDEX, so freeing a whole tree
# is one assignment instead of a traversal calling Delete on every node. That is what the fast C,
# Rust and Java entries do, and the CLBG ranks a language by its BEST submitted program.
# Measured at N=18: interp 4660 -> 2956 ms, and under --aot 5267 -> 2797 ms, which also removes the
# only case in the suite where every compiled profile lost to the interpreter.
"binary-trees|binary-trees-modern-arena.bas|21|12|||binary-trees.py|binary-trees.lua|python||Allocate and deallocate many binary trees"
"fannkuch-redux|fannkuch-redux-modern-mt.bas|12|8|||fannkuch-redux.py|fannkuch-redux.lua|python||Indexed access to a tiny integer sequence"
"fasta|fasta-modern.bas|25000000|100000|||fasta.py|fasta.lua|python||Generate and write random DNA sequences"
"k-nucleotide|k-nucleotide-modern-mt.bas|||25000000|100000|k-nucleotide.py|k-nucleotide.lua|lua||Hashtable update and k-nucleotide strings"
"mandelbrot|mandelbrot-modern.bas|16000|1000|||mandelbrot.py|mandelbrot.lua|python||Generate a Mandelbrot set portable bitmap"
"n-body|n-body-modern.bas|50000000|500000|||n-body.py|n-body.lua|python||Double-precision N-body simulation"
"pidigits|pidigits-bigint-modern.bas|10000|1000|||pidigits.py|pidigits.lua|oracle|pidigits_oracle.py|Streaming arbitrary-precision arithmetic"
"regex-redux|regex-redux-modern-mt.bas|||5000000|100000|regex-redux.py|regex-redux.lua|oracle|regexredux_oracle.py|Match DNA 8-mers and substitute magic patterns"
"reverse-complement|reverse-complement-modern-mt.bas|||25000000|100000|reverse-complement.py|reverse-complement.lua|python||Read DNA sequences and write their reverse-complement"
"spectral-norm|spectral-norm-modern.bas|5500|500|||spectral-norm.py|spectral-norm.lua|python||Eigenvalue using the power method"
)

# OPT-IN ROWS. Never measured in a default run: they are asked for by name with --extra (or --only).
# A row belongs here when it is a legitimate thing to measure but is NOT a statement about the engine,
# so that leaving it in the default table misleads more than it informs.
SUITE_EXTRA=(
"pidigits-basic|pidigits-modern.bas|10000|1000|||pidigits.py|pidigits.lua|oracle|pidigits_oracle.py|The same spigot with the arithmetic written out in BASIC"
)

PROFILE_KEYS=(interp aot jit aotjit)
declare -A PROFILE_LABEL=( [interp]="sb interp" [aot]="sb AOT" [jit]="sb JIT" [aotjit]="sb AOT+JIT" )
declare -A PROFILE_FLAGS=( [interp]="" [aot]="--aot" [jit]="--jit" [aotjit]="--aot --jit" )

# ============================================================================
#  DISPLAY
# ============================================================================

show_banner() {
    local border; border="$(printf '=%.0s' {1..78})"
    echo ""
    echo -e "${CYAN}${border}${NC}"
    echo -e "${WHITE}   ____           _       _ ____            _      ____  ${NC}"
    echo -e "${WHITE}  / ___|  ___  __| | __ _(_) __ )  __ _ ___(_) ___|___ \\ ${NC}"
    echo -e "${WHITE}  \\___ \\ / _ \\/ _\` |/ _\` | |  _ \\ / _\` / __| |/ __| __) |${NC}"
    echo -e "${WHITE}   ___) |  __/ (_| | (_| | | |_) | (_| \\__ \\ | (__ / __/ ${NC}"
    echo -e "${WHITE}  |____/ \\___|\\__,_|\\__,_|_|____/ \\__,_|___/_|\\___|_____|${NC}"
    echo ""
    echo -e "${YELLOW}                   BENCHMARK SUITE${NC}"
    echo -e "${CYAN}${border}${NC}"
    echo ""
}

line() { echo -e "${2:-$GRAY}  $1${NC}"; }

format_ms() {
    local ms="$1"
    awk -v m="$ms" 'BEGIN{ if (m<=0) print "-"; else if (m<10000) printf "%.0f", m; else printf "%.1fs", m/1000 }'
}

# ============================================================================
#  SYSTEM INFORMATION
# ============================================================================

SYS_CPU=""; SYS_CORES=""; SYS_THREADS=""; SYS_RAM=""; SYS_OS=""; SYS_ARCH=""

get_system_info() {
    SYS_CPU="$(sed -n 's/^model name[[:space:]]*: //p' /proc/cpuinfo 2>/dev/null | head -1)"
    [[ -z "$SYS_CPU" ]] && SYS_CPU="$(uname -p 2>/dev/null || echo unknown)"
    SYS_THREADS="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo '?')"
    SYS_CORES="$(awk -F: '/^core id/{print $2}' /proc/cpuinfo 2>/dev/null | sort -u | wc -l)"
    [[ "${SYS_CORES:-0}" -eq 0 ]] && SYS_CORES="$SYS_THREADS"
    local kb; kb="$(awk '/^MemTotal:/{print $2}' /proc/meminfo 2>/dev/null || echo 0)"
    SYS_RAM="$(awk -v k="$kb" 'BEGIN{printf "%.1f GB", k/1048576}')"
    if [[ -r /etc/os-release ]]; then SYS_OS="$(sed -n 's/^PRETTY_NAME="\(.*\)"/\1/p' /etc/os-release)"; fi
    [[ -z "$SYS_OS" ]] && SYS_OS="$(uname -s)"
    SYS_ARCH="$(uname -m)"
}

# ============================================================================
#  RUNTIME DISCOVERY
# ============================================================================

PYTHON_EXE=""; LUA_EXE=""

config_value() {
    local key="$1"
    [[ -f "$CONFIG_FILE" ]] || return 1
    sed -n "s/.*\"$key\"[[:space:]]*:[[:space:]]*\"\([^\"]*\)\".*/\1/p" "$CONFIG_FILE" | head -1
}

resolve_runtimes() {
    # The config file wins when it names something runnable; otherwise whatever is on PATH.
    # A runtime that cannot be found is not an error: its column is reported as unavailable.
    local cfg
    cfg="$(config_value PythonPath 2>/dev/null || true)"
    if [[ -n "$cfg" && "$cfg" != "disabled" && -x "$cfg" ]]; then PYTHON_EXE="$cfg"; fi
    cfg="$(config_value LuaPath 2>/dev/null || true)"
    if [[ -n "$cfg" && "$cfg" != "disabled" && -x "$cfg" ]]; then LUA_EXE="$cfg"; fi

    if [[ -z "$PYTHON_EXE" ]]; then
        for c in "$BENCHMARK_DIR/runtime/python/python3" python3 python; do
            if command -v "$c" >/dev/null 2>&1; then PYTHON_EXE="$(command -v "$c")"; break; fi
        done
    fi
    if [[ -z "$LUA_EXE" ]]; then
        for c in "$BENCHMARK_DIR/runtime/lua/lua54" lua5.4 lua54 lua; do
            if command -v "$c" >/dev/null 2>&1; then LUA_EXE="$(command -v "$c")"; break; fi
        done
    fi
}

# ============================================================================
#  EXECUTION
# ============================================================================

TIMED_MS=0; TIMED_CODE=0; TIMED_ERR=""

invoke_timed() {
    # One process, wall-clock timed, stdout captured to a file. Redirection is done by the shell
    # on the raw file descriptors, so binary output (fasta, mandelbrot) passes through untouched.
    local exe="$1" outfile="$2" stdinfile="$3"; shift 3
    local errfile="$outfile.err" start end

    start=$(date +%s%N)
    if [[ -n "$stdinfile" ]]; then
        "$exe" "$@" < "$stdinfile" > "$outfile" 2> "$errfile"
    else
        "$exe" "$@" > "$outfile" 2> "$errfile" < /dev/null
    fi
    TIMED_CODE=$?
    end=$(date +%s%N)

    TIMED_MS=$(( (end - start) / 1000000 ))
    TIMED_ERR=""
    if [[ -s "$errfile" ]]; then TIMED_ERR="$(head -1 "$errfile")"; fi
    rm -f "$errfile"
    [[ $TIMED_CODE -eq 0 ]]
}

output_signature() {
    # Compares outputs by content, not byte for byte: CRLF versus LF and a trailing newline are a
    # property of the host's print routine, not of the computation. Everything else must match.
    # Done in Python over BYTES, exactly as the PowerShell version does over Latin1, so that a
    # binary PBM survives the comparison.
    local path="$1"
    [[ -s "$path" ]] || return 1
    python3 - "$path" <<'PY'
import sys, hashlib
data = open(sys.argv[1], 'rb').read()
if not data:
    sys.exit(1)
data = data.replace(b'\r\n', b'\n').rstrip(b'\n\r \t')
print(hashlib.md5(data).hexdigest())
PY
}

read_thermometer_fast() {
    # ONE run, for the adaptive cooldown and the per-benchmark readings.
    [[ -x "$THERMO_EXE" ]] || { echo 0; return; }
    invoke_timed "$THERMO_EXE" "$TEMP_DIR/thermo.out" "" >/dev/null 2>&1
    echo "$TIMED_MS"
}

read_thermometer() {
    # Best-of-3 on a fixed native binary. This is the machine's speed right now, in milliseconds.
    [[ -x "$THERMO_EXE" ]] || { echo 0; return; }
    local best=999999999 t
    for _ in 1 2 3; do
        invoke_timed "$THERMO_EXE" "$TEMP_DIR/thermo.out" "" >/dev/null 2>&1
        t=$TIMED_MS
        [[ $t -lt $best ]] && best=$t
    done
    echo "$best"
}

start_cooldown() {
    # The fixed wait is the FLOOR. With --cool-to the wait then continues until the machine is
    # actually back to that speed, bounded by --cool-max so a hot room cannot stall the battery.
    local seconds="$1"
    [[ "$seconds" -gt 0 ]] && sleep "$seconds"
    [[ "$COOL_TO" -le 0 || "$NO_THERMOMETER" == "true" ]] && return
    [[ -x "$THERMO_EXE" ]] || return
    local waited=0 t=0 step
    while [[ $waited -lt $COOL_MAX ]]; do
        t=$(read_thermometer_fast)
        [[ "$t" -le 0 ]] && return
        [[ "$t" -le "$COOL_TO" ]] && break
        step=$(( COOL_MAX - waited )); [[ $step -gt 15 ]] && step=15
        sleep "$step"; waited=$(( waited + step ))
    done
    [[ $waited -le 0 ]] && return
    if [[ "$t" -le "$COOL_TO" ]]; then
        line "cooled to $t ms after ${waited}s extra" "$GRAY"
    else
        line "still $t ms after ${waited}s, target was $COOL_TO - measuring anyway" "$YELLOW"
    fi
}

# ============================================================================
#  FASTA INPUT GENERATION
# ============================================================================

get_fasta_input() {
    # k-nucleotide, regex-redux and reverse-complement all read the output of fasta. Generate it
    # once per N with the reference implementation, so every runtime is fed the same bytes.
    # Two statements, NOT `local n="$1" path=".../fasta_$n.txt"`: bash expands every argument of
    # `local` before assigning any of them, so $n would still be unset there - and under `set -u`
    # that aborts the function, which silently skipped the three stdin benchmarks.
    local n="$1"
    local path="$TEMP_DIR/fasta_$n.txt"
    if [[ -s "$path" ]]; then echo "$path"; return 0; fi
    line "generating the FASTA input, N=$n ..." "$GRAY" >&2
    if [[ -n "$PYTHON_EXE" ]] && invoke_timed "$PYTHON_EXE" "$path" "" "$PY_DIR/fasta.py" "$n" && [[ -s "$path" ]]; then
        echo "$path"; return 0
    fi
    if [[ -n "$LUA_EXE" ]] && invoke_timed "$LUA_EXE" "$path" "" "$LUA_DIR/fasta.lua" "$n" && [[ -s "$path" ]]; then
        echo "$path"; return 0
    fi
    rm -f "$path"
    return 1
}

# ============================================================================
#  ONE ARM
# ============================================================================
#
# Results live in flat maps keyed "<bench>:<arm>:<field>" - bash has no nested maps, and a
# parallel set of arrays would drift the moment one of them is filtered out.

declare -A ARM_MS ARM_SIG ARM_ERR ARM_ISREF ARM_VERDICT

invoke_arm() {
    # Runs one arm RUN_COUNT times and keeps the best time. Every run's output is signed; a run
    # that fails, or whose output is not reproducible, is reported rather than averaged away.
    local bench="$1" key="$2" label="$3" exe="$4" outfile="$5" stdinfile="$6"
    local runcount="$7" cooldown="$8" isref="$9"; shift 9
    local best=999999999 sig="" fail="" thissig i

    for (( i=1; i<=runcount; i++ )); do
        start_cooldown "$cooldown"
        printf "      %-11s %d/%d ... " "$label" "$i" "$runcount"
        if ! invoke_timed "$exe" "$outfile" "$stdinfile" "$@"; then
            fail="${TIMED_ERR:-exit code $TIMED_CODE}"
            echo -e "${RED}FAIL  ($fail)${NC}"
            break
        fi
        if ! thissig="$(output_signature "$outfile")"; then
            fail="no output"; echo -e "${RED}FAIL  (no output)${NC}"; break
        fi
        if [[ -z "$sig" ]]; then sig="$thissig"
        elif [[ "$sig" != "$thissig" ]]; then
            fail="output not reproducible across runs"
            echo -e "${RED}FAIL  (output changed between runs)${NC}"; break
        fi
        [[ $TIMED_MS -lt $best ]] && best=$TIMED_MS
        if [[ $TIMED_MS -lt 10000 ]]; then echo -e "${GRAY}$(format_ms "$TIMED_MS") ms${NC}"
        else echo -e "${GRAY}$(format_ms "$TIMED_MS")${NC}"; fi
    done

    ARM_ISREF["$bench:$key"]="$isref"
    if [[ -n "$fail" ]]; then
        ARM_MS["$bench:$key"]=0; ARM_SIG["$bench:$key"]=""; ARM_ERR["$bench:$key"]="$fail"
    else
        ARM_MS["$bench:$key"]=$best; ARM_SIG["$bench:$key"]="$sig"; ARM_ERR["$bench:$key"]=""
    fi
}

# ============================================================================
#  ONE BENCHMARK
# ============================================================================

declare -A BENCH_NLABEL BENCH_DESC BENCH_VERIFY BENCH_THERMO BENCH_EXPECTED BENCH_ARMS
RESULT_ORDER=()

invoke_benchmark() {
    local spec="$1" runcount="$2" refruncount="$3" cooldown="$4"
    local name bas stdn quickn stdinn stdinquickn py lua verify oracle desc
    IFS='|' read -r name bas stdn quickn stdinn stdinquickn py lua verify oracle desc <<< "$spec"

    echo ""
    echo -e "${CYAN}  $name${NC}${GRAY}  - $desc${NC}"

    # The machine's speed AT THIS POINT of the battery. Two readings at the ends tell you the
    # session drifted; they do not tell you WHERE, and on a run measured in hours that is the
    # difference between discarding one row and discarding the table.
    local thermo=0
    if [[ "$NO_THERMOMETER" != "true" ]]; then
        thermo=$(read_thermometer_fast)
        [[ "$thermo" -gt 0 ]] && line "thermometer here: $thermo ms" "$GRAY"
    fi

    local stdinfile="" nlabel="" n fastan
    local -a sizeargs=()
    if [[ -n "$stdinn" ]]; then
        fastan=$([[ "$QUICK" == "true" ]] && echo "$stdinquickn" || echo "$stdinn")
        if ! stdinfile="$(get_fasta_input "$fastan")"; then
            line "cannot generate the FASTA input - skipping" "$RED"; return 1
        fi
        nlabel="stdin = fasta($fastan)"
    else
        n=$([[ "$QUICK" == "true" ]] && echo "$quickn" || echo "$stdn")
        sizeargs=("$n"); nlabel="N = $n"
    fi
    line "$nlabel" "$GRAY"

    RESULT_ORDER+=("$name")
    BENCH_NLABEL["$name"]="$nlabel"; BENCH_DESC["$name"]="$desc"
    BENCH_VERIFY["$name"]="$verify"; BENCH_THERMO["$name"]="$thermo"
    local armlist=""

    # --- reference runtimes -------------------------------------------------
    # refruncount, not runcount: the yardsticks are not under test, and at the standard sizes they
    # are most of the battery. Fewer runs there errs in the honest direction - a yardstick measured
    # fewer times tends to read SLOWER, which flatters us.
    if [[ "$RUN_PYTHON" == "true" && -n "$PYTHON_EXE" && -n "$py" ]]; then
        invoke_arm "$name" python "python" "$PYTHON_EXE" "$TEMP_DIR/$name.python.out" "$stdinfile" \
            "$refruncount" "$cooldown" true "$PY_DIR/$py" ${sizeargs[@]+"${sizeargs[@]}"}
        armlist="$armlist python"
    fi
    if [[ "$RUN_LUA" == "true" && -n "$LUA_EXE" && -n "$lua" ]]; then
        invoke_arm "$name" lua "lua" "$LUA_EXE" "$TEMP_DIR/$name.lua.out" "$stdinfile" \
            "$refruncount" "$cooldown" true "$LUA_DIR/$lua" ${sizeargs[@]+"${sizeargs[@]}"}
        armlist="$armlist lua"
    fi

    # --- the expected output ------------------------------------------------
    # Whichever source the benchmark names: reuse it if it already ran as an arm, otherwise run it
    # once, untimed. An oracle is ALWAYS run untimed - it is not the engine the benchmark specifies.
    local expected=""
    case "$verify" in
        python)
            # A non-empty signature IS the proof the arm ran clean: a failed arm stores "".
            if [[ -n "${ARM_SIG[$name:python]:-}" ]]; then
                expected="${ARM_SIG[$name:python]}"
            elif [[ -z "${ARM_MS[$name:python]+set}" && -n "$PYTHON_EXE" ]]; then
                if invoke_timed "$PYTHON_EXE" "$TEMP_DIR/$name.ref.out" "$stdinfile" "$PY_DIR/$py" ${sizeargs[@]+"${sizeargs[@]}"}; then
                    expected="$(output_signature "$TEMP_DIR/$name.ref.out" || true)"
                fi
            fi ;;
        lua)
            if [[ -n "${ARM_SIG[$name:lua]:-}" ]]; then
                expected="${ARM_SIG[$name:lua]}"
            elif [[ -z "${ARM_MS[$name:lua]+set}" && -n "$LUA_EXE" ]]; then
                if invoke_timed "$LUA_EXE" "$TEMP_DIR/$name.ref.out" "$stdinfile" "$LUA_DIR/$lua" ${sizeargs[@]+"${sizeargs[@]}"}; then
                    expected="$(output_signature "$TEMP_DIR/$name.ref.out" || true)"
                fi
            fi ;;
        oracle)
            if [[ -n "$PYTHON_EXE" && -n "$oracle" ]]; then
                if invoke_timed "$PYTHON_EXE" "$TEMP_DIR/$name.oracle.out" "$stdinfile" "$ORACLE_DIR/$oracle" ${sizeargs[@]+"${sizeargs[@]}"}; then
                    expected="$(output_signature "$TEMP_DIR/$name.oracle.out" || true)"
                fi
            fi ;;
    esac
    [[ -z "$expected" ]] && line "no runnable reference on this machine - output CANNOT be verified" "$YELLOW"
    BENCH_EXPECTED["$name"]="$expected"

    # --- SedaiBasic2, four profiles ----------------------------------------
    if [[ "$RUN_SEDAI" == "true" ]]; then
        local src="$BAS_DIR/$bas" p
        for p in "${PROFILE_KEYS[@]}"; do
            if [[ -n "$PROFILE_FILTER" && ",$PROFILE_FILTER," != *",$p,"* ]]; then continue; fi
            # shellcheck disable=SC2086
            invoke_arm "$name" "$p" "${PROFILE_LABEL[$p]}" "$SB_EXE" "$TEMP_DIR/$name.$p.out" "$stdinfile" \
                "$runcount" "$cooldown" false "$src" ${sizeargs[@]+"${sizeargs[@]}"} ${PROFILE_FLAGS[$p]}
            armlist="$armlist $p"
        done
    fi
    BENCH_ARMS["$name"]="$armlist"

    # --- verdicts -----------------------------------------------------------
    local k na="" bad="" odd=""
    for k in $armlist; do
        if [[ -n "${ARM_ERR[$name:$k]}" && "${ARM_ISREF[$name:$k]}" == "true" ]]; then
            ARM_VERDICT["$name:$k"]="N/A"; na="$na $k"
        elif [[ -n "${ARM_ERR[$name:$k]}" ]]; then
            ARM_VERDICT["$name:$k"]="FAIL"; bad="$bad $k"
        elif [[ -z "$expected" ]]; then
            ARM_VERDICT["$name:$k"]="NO REF"
        elif [[ "${ARM_SIG[$name:$k]}" == "$expected" ]]; then
            ARM_VERDICT["$name:$k"]="MATCH"
        else
            ARM_VERDICT["$name:$k"]="DIFF"
            if [[ "${ARM_ISREF[$name:$k]}" == "true" ]]; then odd="$odd $k"; else bad="$bad $k"; fi
        fi
    done

    [[ -n "$na" ]]  && line "not available here:$na" "$GRAY"
    [[ -n "$odd" ]] && line "yardstick disagrees with the reference (not our output):$odd" "$YELLOW"
    if [[ -z "$bad" ]]; then
        if [[ -z "$expected" ]]; then line "every arm ran; output UNVERIFIED (no reference)" "$YELLOW"
        else line "output verified: every sb profile MATCHes the reference" "$GREEN"; fi
    else
        local msg="" b
        for b in $bad; do msg="$msg $b = ${ARM_VERDICT[$name:$b]},"; done
        line "not clean:${msg%,}" "$RED"
    fi
    return 0
}

bench_status() {
    local name="$1" k
    [[ -z "${BENCH_EXPECTED[$name]}" ]] && { echo "NO REF"; return; }
    for k in ${BENCH_ARMS[$name]}; do
        [[ "${ARM_ISREF[$name:$k]}" == "true" ]] && continue
        case "${ARM_VERDICT[$name:$k]}" in DIFF|FAIL) echo "CHECK"; return ;; esac
    done
    echo "MATCH"
}

# ============================================================================
#  REPORT
# ============================================================================

ARM_ORDER=()
build_arm_order() {
    ARM_ORDER=()
    [[ "$RUN_PYTHON" == "true" ]] && ARM_ORDER+=(python)
    [[ "$RUN_LUA" == "true" ]] && ARM_ORDER+=(lua)
    if [[ "$RUN_SEDAI" == "true" ]]; then
        local p
        for p in "${PROFILE_KEYS[@]}"; do
            if [[ -n "$PROFILE_FILTER" && ",$PROFILE_FILTER," != *",$p,"* ]]; then continue; fi
            ARM_ORDER+=("$p")
        done
    fi
}

arm_label() {
    case "$1" in python) echo "Python" ;; lua) echo "Lua" ;; *) echo "${PROFILE_LABEL[$1]:-$1}" ;; esac
}

show_results_table() {
    local name k arm row status colour
    echo ""
    echo -e "${CYAN}  $(printf '=%.0s' {1..96})${NC}"
    echo -e "${YELLOW}  RESULTS - wall-clock milliseconds, best of $RUNS${NC}"
    echo -e "${CYAN}  $(printf '=%.0s' {1..96})${NC}"
    row="$(printf '  %-20s' 'benchmark')"
    for k in "${ARM_ORDER[@]}"; do row+="$(printf '%11s' "$(arm_label "$k")")"; done
    echo -e "${WHITE}${row}   verify${NC}"
    echo -e "${GRAY}  $(printf -- '-%.0s' {1..96})${NC}"

    for name in "${RESULT_ORDER[@]}"; do
        row="$(printf '  %-20s' "$name")"
        for k in "${ARM_ORDER[@]}"; do
            if [[ -z "${ARM_VERDICT[$name:$k]:-}" ]]; then row+="$(printf '%11s' '-')"
            else
                case "${ARM_VERDICT[$name:$k]}" in
                    "N/A")  row+="$(printf '%11s' 'n/a')" ;;
                    FAIL)   row+="$(printf '%11s' 'FAIL')" ;;
                    DIFF)   if [[ "${ARM_ISREF[$name:$k]}" == "true" ]]; then
                                row+="$(printf '%11s' "$(format_ms "${ARM_MS[$name:$k]}")*")"
                            else row+="$(printf '%11s' 'DIFF')"; fi ;;
                    *)      row+="$(printf '%11s' "$(format_ms "${ARM_MS[$name:$k]}")")" ;;
                esac
            fi
        done
        status="$(bench_status "$name")"
        case "$status" in MATCH) colour="$GREEN" ;; "NO REF") colour="$YELLOW" ;; *) colour="$RED" ;; esac
        echo -e "${colour}${row}   ${status}${NC}"
    done
    echo ""
    line "MATCH = every arm's output equals the reference   NO REF = nothing to compare against" "$GRAY"
    line "CHECK = at least one arm disagreed or failed     n/a = that runtime cannot run it here" "$GRAY"
    line "*     = yardstick timed, but its output is not comparable on this platform" "$GRAY"
}

write_report() {
    local path="$1" thermo_start="$2" thermo_end="$3"
    local name k

    {
        echo "# SedaiBasic2 - Benchmark Results"
        echo ""
        echo "Generated: $(date '+%Y-%m-%d %H:%M:%S')"
        echo ""
        echo "Programs from [The Computer Language Benchmarks Game](https://salsa.debian.org/benchmarksgame-team/benchmarksgame/)."
        echo ""
        echo "## Method"
        echo ""
        echo "- **Wall-clock around the process, for every runtime.** Startup and compilation are counted for all of them, so the numbers are comparable."
        echo "- **Best of $RUNS run(s)** per arm - the minimum, not the mean."
        echo "- **Cooldown** of ${ACTUAL_COOLDOWN}s between runs."
        echo "- **Output verified on every run.** An arm whose output differs is reported as DIFF and its time is not credited."
        echo "- Neither instruction counts nor MIPS are reported: CLBG does not measure them."
        echo "- Size: $([[ "$QUICK" == "true" ]] && echo 'QUICK (reduced N)' || echo 'STANDARD (the CLBG values)')."
        echo "- Harness: \`benchmark.sh\` on $SYS_ARCH; binary under test \`bin/$PLATFORM_DIR/sb$EXE_SUFFIX\`."
        echo ""
        if [[ "$thermo_start" -gt 0 && "$thermo_end" -gt 0 ]]; then
            local drift; drift="$(awk -v a="$thermo_start" -v b="$thermo_end" 'BEGIN{printf "%.1f", (b>a?b-a:a-b)*100.0/a}')"
            echo "**Thermometer** (\`$(basename "$THERMO_EXE")\`, best of 3): $thermo_start ms at the start, $thermo_end ms at the end - drift ${drift}%."
            awk -v d="$drift" 'BEGIN{ if (d>5.0) { print ""; print "> The machine changed speed during the session. These numbers are not comparable with other sessions." } }'
            echo ""
            echo "Reading taken before each benchmark, so a row can be judged against the machine it ran on:"
            echo ""
            echo "| benchmark | thermometer |"
            echo "|---|---:|"
            for name in "${RESULT_ORDER[@]}"; do
                [[ "${BENCH_THERMO[$name]}" -gt 0 ]] && echo "| $name | ${BENCH_THERMO[$name]} ms |"
            done
            echo ""
        else
            echo "**Thermometer**: not available on this machine - the fixed native binary is built by fbc, which is not installed here. The session's speed is therefore NOT witnessed, and these numbers must not be compared with a session that had it."
            echo ""
        fi

        echo "## System"
        echo ""
        echo "| Item | Value |"
        echo "|------|-------|"
        echo "| CPU | $SYS_CPU |"
        echo "| Cores / threads | $SYS_CORES / $SYS_THREADS |"
        echo "| RAM | $SYS_RAM |"
        echo "| OS | $SYS_OS ($SYS_ARCH) |"
        [[ -n "$PYTHON_EXE" ]] && echo "| Python | \`$PYTHON_EXE\` ($("$PYTHON_EXE" --version 2>&1 | head -1)) |"
        [[ -n "$LUA_EXE" ]] && echo "| Lua | \`$LUA_EXE\` ($("$LUA_EXE" -v 2>&1 | head -1)) |"
        echo ""

        echo "## Times (ms)"
        echo ""
        local hdr="| benchmark | size |" sep="|---|---|"
        for k in "${ARM_ORDER[@]}"; do hdr+=" $(arm_label "$k") |"; sep+="---:|"; done
        hdr+=" verify |"; sep+="---|"
        echo "$hdr"; echo "$sep"
        for name in "${RESULT_ORDER[@]}"; do
            local row="| $name | ${BENCH_NLABEL[$name]} |"
            for k in "${ARM_ORDER[@]}"; do
                if [[ -z "${ARM_VERDICT[$name:$k]:-}" ]]; then row+=" - |"
                else
                    case "${ARM_VERDICT[$name:$k]}" in
                        "N/A") row+=" n/a |" ;;
                        FAIL)  row+=" FAIL |" ;;
                        DIFF)  if [[ "${ARM_ISREF[$name:$k]}" == "true" ]]; then row+=" ${ARM_MS[$name:$k]}\\* |"
                               else row+=" DIFF |"; fi ;;
                        *)     row+=" ${ARM_MS[$name:$k]} |" ;;
                    esac
                fi
            done
            row+=" $(bench_status "$name") |"
            echo "$row"
        done
        echo ""

        # Ratio against CPython - the yardstick this project has always used.
        if [[ "$RUN_PYTHON" == "true" && "$RUN_SEDAI" == "true" ]]; then
            echo "## Ratio vs CPython"
            echo ""
            echo "Below 1.00 means faster than CPython."
            echo ""
            hdr="| benchmark |"; sep="|---|"
            for k in "${ARM_ORDER[@]}"; do
                [[ "$k" == "python" || "$k" == "lua" ]] && continue
                hdr+=" $(arm_label "$k") |"; sep+="---:|"
            done
            echo "$hdr"; echo "$sep"
            for name in "${RESULT_ORDER[@]}"; do
                local pyms="${ARM_MS[$name:python]:-0}"
                [[ "$pyms" -le 0 ]] && continue
                local row="| $name |"
                for k in "${ARM_ORDER[@]}"; do
                    [[ "$k" == "python" || "$k" == "lua" ]] && continue
                    local m="${ARM_MS[$name:$k]:-0}"
                    if [[ "$m" -le 0 ]]; then row+=" - |"
                    else row+=" $(awk -v a="$m" -v b="$pyms" 'BEGIN{printf "%.2fx", a/b}') |"; fi
                done
                echo "$row"
            done
            echo ""
        fi

        echo "## Verification"
        echo ""
        echo "| benchmark | reference | outcome |"
        echo "|---|---|---|"
        for name in "${RESULT_ORDER[@]}"; do
            local ref="${BENCH_VERIFY[$name]}"
            [[ "$ref" == "oracle" ]] && ref="pure-Python oracle (correctness only, never timed)"
            local na="" odd="" bad="" out
            for k in ${BENCH_ARMS[$name]}; do
                case "${ARM_VERDICT[$name:$k]}" in
                    "N/A") na="$na $k" ;;
                    DIFF)  if [[ "${ARM_ISREF[$name:$k]}" == "true" ]]; then odd="$odd $k"; else bad="$bad $k = DIFF,"; fi ;;
                    FAIL)  [[ "${ARM_ISREF[$name:$k]}" == "true" ]] || bad="$bad $k = FAIL," ;;
                esac
            done
            case "$(bench_status "$name")" in
                MATCH)    out="every sb profile matches" ;;
                "NO REF") out="**not verified** - no runnable reference on this machine" ;;
                *)        out="**check** -${bad%,}" ;;
            esac
            [[ -n "$na" ]]  && out="$out; not runnable here:$na"
            [[ -n "$odd" ]] && out="$out; **$odd timed but its output is not comparable here** (marked \\* in the table)"
            echo "| $name | $ref | $out |"
        done
        echo ""
    } > "$path"
}

# ============================================================================
#  MAIN
# ============================================================================

while [[ $# -gt 0 ]]; do
    case "$1" in
        --help|-h) show_help; exit 0 ;;
        --quick) QUICK=true; shift ;;
        --runs) RUNS="$2"; shift 2 ;;
        --ref-runs) REF_RUNS="$2"; shift 2 ;;
        --cooldown) COOLDOWN="$2"; shift 2 ;;
        --cool-to) COOL_TO="$2"; shift 2 ;;
        --cool-max) COOL_MAX="$2"; shift 2 ;;
        --only) ONLY="$2"; shift 2 ;;
        --extra) EXTRA="$2"; shift 2 ;;
        --runtime) RUNTIME_FILTER="$2"; shift 2 ;;
        --profile) PROFILE_FILTER="$2"; shift 2 ;;
        --no-thermometer) NO_THERMOMETER=true; shift ;;
        --yes|-y) ASSUME_YES=true; shift ;;
        --output) OUTPUT="$2"; shift 2 ;;
        *) echo -e "${RED}Unknown: $1${NC}"; show_help; exit 1 ;;
    esac
done

show_banner

RUN_SEDAI=true; RUN_PYTHON=true; RUN_LUA=true
if [[ -n "$RUNTIME_FILTER" ]]; then
    RUN_SEDAI=false; RUN_PYTHON=false; RUN_LUA=false
    IFS=',' read -ra _rt <<< "$(echo "$RUNTIME_FILTER" | tr '[:upper:]' '[:lower:]')"
    for r in "${_rt[@]}"; do
        case "$r" in
            sedai) RUN_SEDAI=true ;; python) RUN_PYTHON=true ;; lua) RUN_LUA=true ;;
            *) echo -e "${RED}ERROR: unknown runtime: $r. Valid: sedai, python, lua${NC}"; exit 1 ;;
        esac
    done
fi

if [[ -n "$PROFILE_FILTER" ]]; then
    PROFILE_FILTER="$(echo "$PROFILE_FILTER" | tr '[:upper:]' '[:lower:]')"
    IFS=',' read -ra _pf <<< "$PROFILE_FILTER"
    for p in "${_pf[@]}"; do
        case " ${PROFILE_KEYS[*]} " in *" $p "*) ;; *)
            echo -e "${RED}ERROR: unknown profile: $p. Valid: ${PROFILE_KEYS[*]}${NC}"; exit 1 ;;
        esac
    done
fi

# ⛔ --only searches BOTH lists: naming an opt-in row explicitly IS asking for it, and a name that
# resolved in one list but not the other would be a trap. A run with neither flag gets the main suite.
ALL_SPECS=("${SUITE[@]}" "${SUITE_EXTRA[@]}")
SELECTED=()
if [[ -n "$ONLY" ]]; then
    IFS=',' read -ra _only <<< "$ONLY"
    for want in "${_only[@]}"; do
        found=false
        for spec in "${ALL_SPECS[@]}"; do
            [[ "${spec%%|*}" == "$want" ]] && { SELECTED+=("$spec"); found=true; break; }
        done
        if [[ "$found" != "true" ]]; then
            echo -e "${RED}ERROR: unknown benchmark: $want${NC}"
            echo -e "${YELLOW}Valid: $(for s in "${ALL_SPECS[@]}"; do printf '%s ' "${s%%|*}"; done)${NC}"
            exit 1
        fi
    done
else
    SELECTED=("${SUITE[@]}")
fi
if [[ -n "$EXTRA" ]]; then
    IFS=',' read -ra _extra <<< "$EXTRA"
    for want in "${_extra[@]}"; do
        found=false
        for spec in "${SUITE_EXTRA[@]}"; do
            [[ "${spec%%|*}" == "$want" ]] && { SELECTED+=("$spec"); found=true; break; }
        done
        if [[ "$found" != "true" ]]; then
            echo -e "${RED}ERROR: unknown opt-in benchmark: $want${NC}"
            echo -e "${YELLOW}Opt-in: $(for s in "${SUITE_EXTRA[@]}"; do printf '%s ' "${s%%|*}"; done)${NC}"
            exit 1
        fi
    done
fi

# A quick verification pass does not need a cooldown; a real measurement session does.
if [[ "$COOLDOWN" -ge 0 ]]; then ACTUAL_COOLDOWN="$COOLDOWN"
elif [[ "$QUICK" == "true" ]]; then ACTUAL_COOLDOWN=0
else ACTUAL_COOLDOWN=20; fi
if [[ "$REF_RUNS" -ge 1 ]]; then ACTUAL_REF_RUNS="$REF_RUNS"; else ACTUAL_REF_RUNS="$RUNS"; fi

mkdir -p "$TEMP_DIR" "$RESULTS_DIR"

if [[ ! -x "$SB_EXE" ]]; then
    echo -e "${RED}  ERROR: sb not found at $SB_EXE - build it first (./build.sh sb).${NC}"
    exit 1
fi

resolve_runtimes
get_system_info

line "CPU:    $SYS_CPU  (${SYS_CORES}c / ${SYS_THREADS}t)"
line "Binary: $SB_EXE"
line "Size:   $([[ "$QUICK" == "true" ]] && echo 'QUICK (reduced N)' || echo 'STANDARD (the CLBG values)')"
line "Runs:   best of $RUNS (yardsticks: best of $ACTUAL_REF_RUNS), cooldown ${ACTUAL_COOLDOWN}s"
line "Python: ${PYTHON_EXE:-(not found - Python arms and references unavailable)}"
line "Lua:    ${LUA_EXE:-(not found - Lua arms and references unavailable)}"
if [[ ! -x "$THERMO_EXE" ]]; then
    line "Thermometer: not on this machine - the session's speed will NOT be witnessed" "$YELLOW"
fi

if [[ "$QUICK" != "true" && "$ASSUME_YES" != "true" ]]; then
    echo ""
    line "The standard CLBG sizes take minutes per run. Close other applications first." "$YELLOW"
    line "Use --quick for a fast verification pass." "$YELLOW"
    echo ""
    read -r -p "  Press ENTER to continue, or 'q' to quit " answer
    [[ "$answer" == "q" ]] && { line "cancelled" "$YELLOW"; exit 0; }
fi

THERMO_START=0
if [[ "$NO_THERMOMETER" != "true" ]]; then
    if [[ -x "$THERMO_EXE" ]]; then
        echo ""; line "reading the thermometer ..." "$GRAY"
        THERMO_START="$(read_thermometer)"
        [[ "$THERMO_START" -gt 0 ]] && line "thermometer (start): $THERMO_START ms" "$GRAY"
    else
        line "thermometer binary not found - skipping" "$YELLOW"
    fi
fi

for spec in "${SELECTED[@]}"; do
    invoke_benchmark "$spec" "$RUNS" "$ACTUAL_REF_RUNS" "$ACTUAL_COOLDOWN" || true
done

THERMO_END=0
if [[ "$NO_THERMOMETER" != "true" && "$THERMO_START" -gt 0 ]]; then
    echo ""; line "reading the thermometer ..." "$GRAY"
    THERMO_END="$(read_thermometer)"
    drift="$(awk -v a="$THERMO_START" -v b="$THERMO_END" 'BEGIN{printf "%.1f", (b>a?b-a:a-b)*100.0/a}')"
    if awk -v d="$drift" 'BEGIN{exit !(d>5.0)}'; then
        line "thermometer (end):   $THERMO_END ms - drift ${drift}%" "$RED"
        line "the machine changed speed during the session; these numbers are not comparable" "$RED"
    else
        line "thermometer (end):   $THERMO_END ms - drift ${drift}%" "$GRAY"
    fi
fi

build_arm_order
show_results_table

if [[ -n "$OUTPUT" ]]; then
    case "$OUTPUT" in /*) RESULTS_PATH="$OUTPUT" ;; *) RESULTS_PATH="$RESULTS_DIR/$OUTPUT" ;; esac
else
    secs=$(( $(date +%H) * 3600 + $(date +%M | sed 's/^0//;s/^$/0/') * 60 + $(date +%S | sed 's/^0//;s/^$/0/') ))
    RESULTS_PATH="$RESULTS_DIR/$(printf 'BENCHMARK_%s-%05d.md' "$(date +%Y-%m-%d)" "$secs")"
fi

write_report "$RESULTS_PATH" "$THERMO_START" "$THERMO_END"
echo ""
line "report written to: $RESULTS_PATH" "$GREEN"
echo ""

fails=0
for name in "${RESULT_ORDER[@]}"; do
    [[ "$(bench_status "$name")" == "CHECK" ]] && fails=$((fails+1))
done
exit $(( fails > 0 ? 1 : 0 ))
