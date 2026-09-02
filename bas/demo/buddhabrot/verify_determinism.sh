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
#  ⭐ BOTH SAMPLERS ARE CHECKED, and the second one is not a formality either. sampling=mh runs
#  Exp, Log, Cos and Sin per proposal, a Markov chain whose next step depends on a floating-point
#  ratio compared against the generator, and two array copies that carry an orbit between calls -
#  every one of which is a place where one engine could part company with another, and none of
#  which the uniform sampler touches at all. The two runs are checked SEPARATELY: they draw
#  different pictures on purpose, and an image that matched across samplers would mean the
#  sampling= argument had stopped doing anything.
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
status=0
# The Metropolis chain does far more work per orbit than the uniform sampler - it scores every
# proposal - so it is given a smaller count. It is the AGREEMENT that is being checked, not a time.
mh_orbits=$(( orbits / 20 ))
[ "$mh_orbits" -lt 2000 ] && mh_orbits=2000

for sampling in uniform mh; do
  n=$orbits
  [ "$sampling" = mh ] && n=$mh_orbits
  echo "Tracing $n orbits on each engine, sampling=$sampling..."
  echo

  for engine in interpreter jit aot no-opt; do
    case $engine in
      interpreter) flag="" ;;
      jit)         flag="--jit" ;;
      aot)         flag="--aot" ;;
      no-opt)      flag="--no-opt" ;;
    esac
    # ⛔ Run from the work directory, not the repository root: this program writes files.
    ( cd "$work" && "$sb" "$demo" $flag sampling="$sampling" still="$n" \
                        out="$work/$sampling.$engine.ppm" ) \
        | sed "s|^|  [$sampling/$engine] |"
  done

  echo
  hashes=$(cd "$work" && sha256sum "$sampling".interpreter.ppm "$sampling".jit.ppm \
                                   "$sampling".aot.ppm "$sampling".no-opt.ppm)
  echo "$hashes"
  distinct=$(echo "$hashes" | awk '{print $1}' | sort -u | wc -l)
  if [ "$distinct" = 1 ]; then
    echo "✅ sampling=$sampling: identical on all four - one image, four ways of computing it"
  else
    echo "⛔ sampling=$sampling: $distinct different images, the engines disagree"
    status=1
  fi
  echo
done

# ⛔⛔ AND THE TWO SAMPLERS MUST NOT AGREE WITH EACH OTHER. They are two different estimators of
# the same object and they converge to different pictures; identical files here would not be a
# reassuring coincidence, it would mean sampling=mh had silently fallen back to the default - which
# is exactly the way this variant can break without anything looking wrong.
#
# ⛔⛔⛔ AND THE COMPARISON IS AT THE SAME ORBIT COUNT, which is the whole difficulty. The obvious
# thing is to diff the two runs above - and they were run for different counts, because the chain is
# far more expensive per orbit, so those two files can NEVER be equal and the check can never fail.
# Written that way it passed with sampling= deliberately disabled. So the uniform sampler is run
# once more here at the CHAIN'S count, and that is what the chain is compared against.
( cd "$work" && "$sb" "$demo" sampling=uniform still="$mh_orbits" \
                     out="$work/sameN.uniform.ppm" ) > /dev/null
if cmp -s "$work/sameN.uniform.ppm" "$work/mh.interpreter.ppm"; then
  echo "⛔ at $mh_orbits orbits the two samplers drew the SAME image: sampling= is selecting nothing"
  status=1
else
  echo "✅ at $mh_orbits orbits the two samplers draw different pictures - the variant is a variant"
fi

exit $status
