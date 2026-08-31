#!/usr/bin/env bash
# =====================================================================================
#  Buddhabrot - the same picture, three execution engines, side by side.
#
#  WHY THIS IS A SCRIPT AND NOT A KEY IN THE DEMO
#  SedaiBasic binds the execution engine when the program is LOADED: the JIT builds its
#  native loops and the AOT compiler compiles its functions once, before the first
#  instruction runs. There is no way to change engine half-way through without throwing
#  the compiled program away and starting again - which would also throw away the
#  histogram, and with it the whole point of watching one picture converge.
#
#  So the comparison is three processes instead of one keystroke. They are given the SAME
#  seed and the same parameters, so all three are computing the identical image and the
#  only thing that differs is how much of it appears per second.
#
#  ⚠️ The windows open wherever the window manager puts them; drag them side by side.
#     Nothing in the graphics layer can position a window.
#
#  Usage:  bash bas/demo/buddhabrot/compare_engines.sh [size] [seconds]
# =====================================================================================
set -u
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
root=$(cd "$here/../../.." && pwd)
demo="$here/buddhabrot.bas"

size=${1:-400}
seconds=${2:-0}

sb="$root/bin/x86_64-linux/sb"
[ -x "$sb" ] || sb="$root/bin/sb.exe"
if [ ! -x "$sb" ]; then
  echo "sb not found - build it first:  ./build.sh sb --window" >&2
  exit 1
fi

if ! "$sb" --window --help >/dev/null 2>&1 || "$sb" --window "$demo" help=1 2>&1 | grep -q "no window presenter"; then
  echo "⚠️  This sb has no window presenter, so nothing will be visible."
  echo "    Rebuild with:  ./build.sh sb --window"
  echo
fi

# Same seed for all three: they are computing the identical picture.
seed=2463534242
common="size=$size seed=$seed secs=$seconds"

echo "Launching three engines on the same image (size=$size, seed=$seed)."
echo "Press Q in a window to close it."
echo

"$sb" --window "$demo" $common label=INTERPRETER out="/tmp/buddhabrot_interp.ppm" &
"$sb" --window "$demo" $common label=JIT --jit  out="/tmp/buddhabrot_jit.ppm"    &
"$sb" --window "$demo" $common label=AOT --aot  out="/tmp/buddhabrot_aot.ppm"    &
wait
