#!/usr/bin/env bash
# =====================================================================================
#  The same seed and the same number of orbits must produce the same file, byte for byte,
#  on every execution engine. This checks that with hashes rather than with eyes.
#
#  It is not a formality. The three engines are three different pieces of machinery - a
#  bytecode interpreter, a JIT that compiles hot loops to native code, and an AOT compiler
#  that compiles whole functions - and this program's output depends on floating-point
#  comparisons made millions of times. One engine rounding differently anywhere would send
#  one orbit over the escape radius a step early, and the images would part company.
#
#  --no-opt is included on purpose: it runs the interpreter with the optimiser off, so a
#  difference between it and the default answer would be the optimiser miscompiling.
#
#  Usage:  bash bas/demo/buddhabrot/verify_determinism.sh [orbits]
# =====================================================================================
set -u
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
root=$(cd "$here/../../.." && pwd)
demo="$here/buddhabrot.bas"
orbits=${1:-2000000}

sb="$root/bin/x86_64-linux/sb"
[ -x "$sb" ] || sb="$root/bin/sb.exe"
[ -x "$sb" ] || { echo "sb not found - build it first: ./build.sh sb" >&2; exit 1; }

work=$(mktemp -d); trap 'rm -rf "$work"' EXIT
echo "Tracing $orbits orbits on each engine..."
echo

for engine in interpreter jit aot no-opt; do
  case $engine in
    interpreter) flag="" ;;
    jit)         flag="--jit" ;;
    aot)         flag="--aot" ;;
    no-opt)      flag="--no-opt" ;;
  esac
  # ⛔ Run from the work directory, not the repository root: this program writes files.
  ( cd "$work" && "$sb" "$demo" $flag still="$orbits" out="$work/$engine.ppm" ) \
      | sed "s/^/  [$engine] /"
done

echo
hashes=$(cd "$work" && sha256sum interpreter.ppm jit.ppm aot.ppm no-opt.ppm)
echo "$hashes"
echo
distinct=$(echo "$hashes" | awk '{print $1}' | sort -u | wc -l)
if [ "$distinct" = 1 ]; then
  echo "✅ identical on all four - one image, four ways of computing it"
  exit 0
else
  echo "⛔ $distinct different images: the engines disagree"
  exit 1
fi
