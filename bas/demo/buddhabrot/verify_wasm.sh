#!/usr/bin/env bash
# =====================================================================================
#  The browser build, checked the only way that means anything: the SAME PICTURE.
#
#  `sbc --target wasm` compiles this demo to a WebAssembly module, and the page runs it
#  with no interpreter and no runtime library. That is a second build of the same source,
#  and this project has already paid for one of those going unchecked: sbw was the only
#  WEB_MODE binary and no net compiled it, so eight defects lived in it for months.
#
#  Three things are checked here:
#    1. the module still COMPILES (an opcode the backend does not cover is refused, and a
#       demo that grows one stops building without anyone noticing);
#    2. the picture it draws is BYTE FOR BYTE the picture `sb` draws from the same orbits;
#    3. buddhabrot.html embeds THAT module and not an older one - a page carrying a stale
#       module looks perfectly fine and is showing something else.
#
#  Usage:  bash bas/demo/buddhabrot/verify_wasm.sh [orbits]
#          bash bas/demo/buddhabrot/verify_wasm.sh --bless     re-embed the module in the page
# =====================================================================================
set -u
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
root=$(cd "$here/../../.." && pwd)
demo="$here/buddhabrot.bas"
page="$here/buddhabrot.html"

bless=0
orbits=200000
for a in "$@"; do
  case "$a" in
    --bless) bless=1 ;;
    *[0-9]*) orbits="$a" ;;
  esac
done

sb="$root/bin/x86_64-linux/sb";   [ -x "$sb" ]  || sb="$root/bin/sb.exe"
sbc="$root/bin/x86_64-linux/sbc"; [ -x "$sbc" ] || sbc="$root/bin/sbc.exe"
[ -x "$sb" ]  || { echo "sb not found - build it first: ./build.sh sb" >&2; exit 1; }
[ -x "$sbc" ] || { echo "sbc not found - build it first: ./build.sh sbc" >&2; exit 1; }
if ! command -v node >/dev/null 2>&1; then
  echo "WASM: SKIP - node is not installed, so the module cannot be run here" >&2
  exit 0
fi

work=$(mktemp -d); trap 'rm -rf "$work"' EXIT

# ---- 1. it compiles -----------------------------------------------------------------
if ! out=$("$sbc" "$demo" --target wasm "$work/buddhabrot.wasm" 2>&1); then
  echo "$out" | sed 's/^/  /'
  echo "⛔ the WASM backend refused the demo"; exit 1
fi
echo "$out" | grep -E '^WASM:' | sed 's/^/  /'

# ---- 2. the same picture ------------------------------------------------------------
# ⛔ Run from the work directory, not the repository root: sb writes a file.
( cd "$work" && "$sb" "$demo" still="$orbits" out="$work/native.ppm" ) >/dev/null 2>&1

cat > "$work/run.js" <<'JS'
// The module is driven exactly as the page drives it: main() draws one frame of 20 000 orbits,
// then PROC_STEPFRAME is called until the total matches what `still=` traced. The framebuffer is
// dumped as a binary PPM so the two can be compared with a hash rather than with eyes.
const fs = require('fs');
const [path, orbits, out] = [process.argv[2], parseInt(process.argv[3], 10), process.argv[4]];
let memory = null;
const imports = { env: { write: (p, l) => process.stdout.write(Buffer.from(memory.buffer, p, l)),
  now: () => { const d = new Date(); return (d.getTime() - d.getTimezoneOffset()*60000)/86400000 + 25569; },
  sin: Math.sin, cos: Math.cos, tan: Math.tan, atn: Math.atan, exp: Math.exp,
  log: Math.log, log10: Math.log10, log2: Math.log2, asin: Math.asin,
  acos: Math.acos, sinh: Math.sinh, cosh: Math.cosh }};
const X = new WebAssembly.Instance(new WebAssembly.Module(fs.readFileSync(path)), imports).exports;
memory = X.memory;
X.main();                                   // the first 20 000 orbits
let done = 20000;
while (done < orbits) { const n = Math.min(20000, orbits - done); X['PROC_STEPFRAME'](BigInt(n)); done += n; }
const W = X.screen_w.value, H = X.screen_h.value, FB = X.screen_ptr.value;
const fb = new Uint32Array(memory.buffer, FB, W * H);
const body = Buffer.alloc(W * H * 3);
for (let i = 0; i < W * H; i++) { const v = fb[i];
  body[i*3] = (v >>> 16) & 255; body[i*3+1] = (v >>> 8) & 255; body[i*3+2] = v & 255; }
fs.writeFileSync(out, Buffer.concat([Buffer.from(`P6\n${W} ${H}\n255\n`, 'binary'), body]));
JS

node "$work/run.js" "$work/buddhabrot.wasm" "$orbits" "$work/wasm.ppm" >/dev/null || {
  echo "⛔ the module trapped while running"; exit 1; }

nh=$(sha256sum "$work/native.ppm" | cut -d' ' -f1)
wh=$(sha256sum "$work/wasm.ppm"   | cut -d' ' -f1)
echo "  native $nh"
echo "  wasm   $wh"
if [ "$nh" != "$wh" ]; then
  echo "⛔ the module draws a DIFFERENT picture from sb on the same $orbits orbits"; exit 1
fi

# ---- 3. the page carries THIS module ------------------------------------------------
b64=$(base64 -w0 "$work/buddhabrot.wasm")
if [ "$bless" = 1 ]; then
  python3 - "$page" "$b64" <<'PY'
import re, sys
page, b64 = sys.argv[1], sys.argv[2]
s = open(page, encoding='utf-8').read()
s2 = re.sub(r'const B64 = "[^"]*";', 'const B64 = "' + b64 + '";', s, count=1)
if s == s2 and 'const B64' in s:
    print('the page already carried this module'); raise SystemExit(0)
open(page, 'w', encoding='utf-8').write(s2)
print('re-embedded')
PY
  echo "✅ compiles · same picture · page re-embedded"
  exit 0
fi

if grep -qF "const B64 = \"$b64\";" "$page"; then
  echo "✅ compiles · same picture as sb · the page carries this exact module"
  exit 0
else
  echo "⛔ buddhabrot.html embeds a DIFFERENT module than the one this source compiles to."
  echo "   Re-embed it:  bash bas/demo/buddhabrot/verify_wasm.sh --bless"
  exit 1
fi
