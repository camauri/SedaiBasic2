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

# ---- 3. the page's own script runs, and a tap zooms ---------------------------------
# ⛔ THIS CHECK EXISTS BECAUSE THE PAGE BROKE WHERE THE MODULE WAS FINE. Tapping the picture did
# nothing on a phone for three independent reasons: the module was compiled SYNCHRONOUSLY (a browser
# refuses that for a buffer over 4 KB on the main thread, and this one is 25 KB); the canvas listened
# only for `click`, which a touch browser does not always synthesise on something that is not a
# control; and switching to async instantiation left `const step = X['PROC_STEPFRAME']` at top level,
# reading exports that did not exist yet. None of the three is visible to `node --check`, and the
# picture-for-picture check above passes with every one of them present - the MODULE was never wrong.
# So the page's own script is RUN here, against a DOM small enough to fit beside it, and driven.
cat > "$work/shim.js" <<'SHIM_JS'
// A DOM small enough to run the page's own script, and no smaller. It exists so the checks below
// can DRIVE the page - tap, drag, the click a touch browser synthesises afterwards - instead of
// only parsing it.
const els = {};
function mk(id) {
  return els[id] = { id, textContent: '', innerHTML: '', style: {}, offsetLeft: 0, offsetTop: 0,
    offsetWidth: 1, width: 0, height: 0, onclick: null, _h: {},
    classList: { add(){}, remove(){} },
    addEventListener(t, f) { this._h[t] = f; },
    getBoundingClientRect: () => ({ left: 0, top: 0, width: 400, height: 400 }),
    getContext: () => ({ createImageData: (w, h) => ({ data: new Uint8ClampedArray(w * h * 4) }),
                         // What was actually painted, cheaply: the check for a frozen canvas needs
                         // to know the pixels CHANGED, not merely that blit() was called.
                         putImageData: (im) => { let h = 0;
                           for (let i = 0; i < im.data.length; i += 997) h = (h * 31 + im.data[i]) | 0;
                           global.__frameHash = h; } }) };
}
['hud','hud2','note','screen','ping','toggle','reading','zoomout','home','restart',
 'gdown','gup','idown','iup','hint'].forEach(mk);
global.document = { getElementById: (id) => els[id] || mk(id), addEventListener: () => {},
  hidden: false,
  body: { set innerHTML(v) { global.__BODY_HTML = v; }, get innerHTML() { return global.__BODY_HTML; } } };
let rafs = [];
global.requestAnimationFrame = (f) => { rafs.push(f); return rafs.length; };
global.performance = { now: () => Date.now() };
global.__els = els;
global.__pump = (n) => { for (let i = 0; i < n; i++) { const q = rafs; rafs = []; q.forEach(f => f(Date.now())); } };
SHIM_JS
cat > "$work/drive.js" <<'DRIVE_JS'
require('./shim.js');
const fs = require('fs');
eval(fs.readFileSync(__dirname + '/page_body.js', 'utf8'));
let bad = 0;
const say = (ok, what) => { if (!ok) { bad = 1; console.log('  ⛔ ' + what); } };
setTimeout(() => {
  const els = global.__els, s = els.screen;
  const view = () => (els.hud.textContent.split('·').pop() || '').trim();
  if (global.__BODY_HTML) {
    console.log('  ⛔ the page reported: ' + String(global.__BODY_HTML).slice(0, 200));
    process.exit(1);
  }
  global.__pump(2);
  const home = view();
  say(/^×1 /.test(home), 'the page did not start at the whole figure (read "' + home + '")');
  s._h.pointerdown({ clientX: 100, clientY: 300, button: 0 });
  s._h.pointerup  ({ clientX: 100, clientY: 300, button: 0 });
  global.__pump(1);
  const tapped = view();
  say(/^×2 /.test(tapped) && tapped !== home, 'a tap did not zoom (read "' + tapped + '")');
  say(els.ping.style.left === '100px', 'the tap marker was not placed where the tap was');
  s._h.click({ clientX: 100, clientY: 300, button: 0 });
  global.__pump(1);
  say(view() === tapped, 'the click a touch browser synthesises zoomed a SECOND time');
  s._h.pointerdown({ clientX: 100, clientY: 100, button: 0 });
  s._h.pointerup  ({ clientX: 180, clientY: 140, button: 0 });
  global.__pump(1);
  say(view() === tapped, 'a drag counted as a tap');
  els.home.onclick(); global.__pump(1);
  say(view() === home, 'the whole-figure button did not go home');

  // ⛔⛔ AND THE ONE THAT LOOKS LIKE NOTHING AT ALL: run PAST the point where the module's memory
  // grows. A typed array over memory.buffer is DETACHED when that happens - length 0, no exception -
  // so the page copies nothing and the canvas freezes on its last frame while every counter goes on
  // rising and every control still works underneath. On this program the memory grows at about
  // 1 240 000 orbits, when the brightest pixel passes 4 095 and the level table is re-dimensioned;
  // a check that stops before that sees a page in perfect health.
  const grown = () => { let n = 0; while (n < 1600000) { global.__pump(1); n += 30000; } };
  const before = global.__frameHash;
  grown();
  say(global.__frameHash !== undefined, 'the page never painted a frame at all');
  say(global.__frameHash !== before, 'the picture STOPPED CHANGING past the memory growth ' +
      '(the framebuffer view was detached and never rebuilt)');
  const mid = global.__frameHash;
  global.__pump(4);
  say(global.__frameHash !== mid, 'the picture stopped changing after the memory growth');
  process.exit(bad);
}, 300);
DRIVE_JS
if ! python3 - "$page" "$work/page_body.js" <<'EXTRACT_PY'
import re, sys
s = open(sys.argv[1], encoding='utf-8').read()
m = re.search(r"<script>\n(.*)</script>", s, re.S)
if not m:
    print('no <script> block in the page'); raise SystemExit(1)
body = m.group(1)

# ⛔ ONE THING node CANNOT SEE, so it is checked in the text instead: a browser refuses SYNCHRONOUS
# compilation of a buffer larger than 4 KB on the main thread, and this module is 25 KB. node has no
# such rule, so `new WebAssembly.Module(...)` runs perfectly here and fails on every phone.
# ⚠️ And the comments come off FIRST. The page's own comment EXPLAINS the trap by naming the
# forbidden call, and a check that grepped the raw text would be satisfied by that comment alone -
# which is a way this project has already fooled itself twice.
code = re.sub(r'//[^\n]*', '', body)
code = re.sub(r'/\*.*?\*/', '', code, flags=re.S)
if re.search(r'new\s+WebAssembly\.(Module|Instance)\s*\(', code):
    print('  ⛔ the page compiles the module SYNCHRONOUSLY (new WebAssembly.Module/Instance).')
    print('     A browser refuses that over 4 KB on the main thread; use WebAssembly.instantiate.')
    raise SystemExit(1)
if 'WebAssembly.instantiate(' not in code:
    print('  ⛔ the page never calls WebAssembly.instantiate'); raise SystemExit(1)

open(sys.argv[2], 'w', encoding='utf-8').write(body.replace("'use strict';", "", 1))
EXTRACT_PY
then
  echo "⛔ the page does not instantiate the module the way a browser requires"; exit 1
fi

if ! ( cd "$work" && node drive.js ); then
  echo "⛔ the page's script does not drive the module (see above)"; exit 1
fi
echo "  page: starts at the whole figure, a tap zooms, a drag does not, no double zoom"

# ---- 4. the page carries THIS module ------------------------------------------------
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
  # ⭐ The copy that gets PUBLISHED lives in bas/demo/site/ and is checked by verify_site.sh, which
  # owns the whole tree - its links, its reachability and every module in it. This net owns the page
  # beside the source.
  echo "✅ compiles · same picture as sb · the page carries this exact module"
  exit 0
else
  echo "⛔ buddhabrot.html embeds a DIFFERENT module than the one this source compiles to."
  echo "   Re-embed it:  bash bas/demo/buddhabrot/verify_wasm.sh --bless"
  exit 1
fi
