#!/usr/bin/env bash
# =====================================================================================
#  bubble_universe, compiled to WebAssembly, checked the only way that means anything:
#  the SAME PICTURE.
#
#  The module is a second build of the same source, and this project has already paid
#  for one of those going unwatched - sbw was the only WEB_MODE binary, no net compiled
#  it, and eight defects lived in it for months.
#
#  Three things:
#    1. the module still COMPILES (an opcode the backend does not cover is refused, so a
#       demo that grows one stops building without anyone noticing);
#    2. the frame it draws is BYTE FOR BYTE the frame `sb` draws at the same phase. The
#       native demo takes a FIXED phase as its third argument, which is what makes the
#       comparison possible at all - by default it advances by real elapsed time and no
#       two runs agree;
#    3. both copies of the page carry THAT module and not an older one - a page carrying a
#       stale module looks perfectly fine and is showing something else.
#
#  Usage:  bash bas/demo/verify_bubble_wasm.sh
#          bash bas/demo/verify_bubble_wasm.sh --bless    re-embed the module in the pages
# =====================================================================================
set -u
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
root=$(cd "$here/../.." && pwd)
demo="$here/bubble_universe.bas"
# The page beside the source, and the copy that gets uploaded - a second place the same module
# lives is a second place it can go stale.
# ⛔ It briefly had a copy inside the BUDDHABROT's site folder, which was wrong: that folder is one
# demo's, and two demos mixed into one page make both of them worse. This demo has its own.
pages=("$here/bubble_universe.html" "$here/bubble_universe_site/bubble_universe.html")

bless=0
[ "${1:-}" = "--bless" ] && bless=1

sb="$root/bin/x86_64-linux/sb";   [ -x "$sb" ]  || sb="$root/bin/sb.exe"
sbc="$root/bin/x86_64-linux/sbc"; [ -x "$sbc" ] || sbc="$root/bin/sbc.exe"
[ -x "$sb" ]  || { echo "sb not found - build it first: ./build.sh sb" >&2; exit 1; }
[ -x "$sbc" ] || { echo "sbc not found - build it first: ./build.sh sbc" >&2; exit 1; }
if ! command -v node >/dev/null 2>&1; then
  echo "BUBBLE-WASM: SKIP - node is not installed, so the module cannot be run here" >&2
  exit 0
fi

work=$(mktemp -d); trap 'rm -rf "$work"' EXIT

# ---- 1. it compiles -----------------------------------------------------------------
if ! out=$("$sbc" "$demo" --target wasm "$work/bubble.wasm" 2>&1); then
  echo "$out" | sed 's/^/  /'
  echo "⛔ the WASM backend refused bubble_universe"; exit 1
fi
echo "$out" | grep -E '^WASM:' | sed 's/^/  /'

# ---- 2. the same picture ------------------------------------------------------------
# ⛔ Run from the work directory, not the repository root: this program writes a file.
# ⛔ TWO phases, and the second is the one that earns its keep. At frame 0 the phase is zero
# whatever formula produces it, so a WRONG formula still draws the right first frame - injecting one
# (dividing by 61 instead of 60) was caught only by the module's bytes differing, which is a weaker
# thing to be caught by. Frame 60 puts the phase at PHASE_RATE, where the arithmetic shows.
( cd "$work" && "$sb" "$demo" 1 "$work/native.ppm"  0.0  ) >/dev/null 2>&1
( cd "$work" && "$sb" "$demo" 1 "$work/native60.ppm" 0.06 ) >/dev/null 2>&1

cat > "$work/run.js" <<'JS'
// main() draws frame 0; argv[4], when given, steps on to that frame. Both are compared against a
// native run with the matching phase pinned on the command line.
const fs = require('fs');
let memory = null;
const imports = { env: { write: () => {},
  now: () => { const d = new Date(); return (d.getTime() - d.getTimezoneOffset()*60000)/86400000 + 25569; },
  sin: Math.sin, cos: Math.cos, tan: Math.tan, atn: Math.atan, exp: Math.exp,
  log: Math.log, log10: Math.log10, log2: Math.log2, asin: Math.asin,
  acos: Math.acos, sinh: Math.sinh, cosh: Math.cosh }};
const X = new WebAssembly.Instance(new WebAssembly.Module(fs.readFileSync(process.argv[2])), imports).exports;
memory = X.memory;
X.main();
const upto = parseInt(process.argv[4] || '0', 10);
for (let f = 1; f <= upto; f++) X['PROC_STEPFRAME'](BigInt(f));
const W = X.screen_w.value, H = X.screen_h.value, FB = X.screen_ptr.value;
const fb = new Uint32Array(memory.buffer, FB, W * H);
const body = Buffer.alloc(W * H * 3);
for (let i = 0; i < W * H; i++) { const v = fb[i];
  body[i*3] = (v >>> 16) & 255; body[i*3+1] = (v >>> 8) & 255; body[i*3+2] = v & 255; }
fs.writeFileSync(process.argv[3], Buffer.concat([Buffer.from(`P6\n${W} ${H}\n255\n`, 'binary'), body]));
JS

node "$work/run.js" "$work/bubble.wasm" "$work/wasm.ppm"   >/dev/null || {
  echo "⛔ the module trapped while running"; exit 1; }
node "$work/run.js" "$work/bubble.wasm" "$work/wasm60.ppm" 60 >/dev/null || {
  echo "⛔ the module trapped stepping to frame 60"; exit 1; }

for pair in "native.ppm wasm.ppm frame 0" "native60.ppm wasm60.ppm frame 60"; do
  set -- $pair
  nh=$(sha256sum "$work/$1" | cut -d' ' -f1)
  wh=$(sha256sum "$work/$2" | cut -d' ' -f1)
  if [ "$nh" != "$wh" ]; then
    echo "  native $nh"
    echo "  wasm   $wh"
    echo "⛔ the module draws a DIFFERENT $3 $4 from sb at the same phase"; exit 1
  fi
  echo "  $3 $4: $nh"
done

# ---- 3. the pages carry THIS module -------------------------------------------------
b64=$(base64 -w0 "$work/bubble.wasm")
bad=0
for page in "${pages[@]}"; do
  [ -f "$page" ] || continue
  if [ "$bless" = 1 ]; then
    python3 - "$page" "$b64" <<'PY'
import re, sys
page, b64 = sys.argv[1], sys.argv[2]
s = open(page, encoding='utf-8').read()
open(page, 'w', encoding='utf-8').write(re.sub(r'const B64 = "[^"]*";', 'const B64 = "' + b64 + '";', s, count=1))
PY
  elif ! grep -qF "const B64 = \"$b64\";" "$page"; then
    echo "⛔ ${page#$root/} carries a DIFFERENT module than this source compiles to."
    bad=1
  fi
done
if [ "$bad" != 0 ]; then
  echo "   Re-embed:  bash bas/demo/verify_bubble_wasm.sh --bless"; exit 1
fi

if [ "$bless" = 1 ]; then
  echo "✅ compiles · same frame as sb · pages re-embedded"
else
  echo "✅ compiles · same frame as sb · both pages carry this exact module"
fi
