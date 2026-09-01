#!/usr/bin/env bash
# =====================================================================================
#  The site that gets published, checked as a whole.
#
#  bas/demo/site/ mirrors what goes on a server: an index of the demos, and one folder
#  per demo holding its page and the demo itself. Every link in it is RELATIVE, so the
#  tree works at any address - and a relative link that resolves nowhere is the easiest
#  thing in the world to ship, because on the machine that wrote it the file was there
#  a moment ago under a different name.
#
#  Four things:
#    1. every local href and src RESOLVES to a file that exists;
#    2. every page can be reached from the index, and every page can get back to it;
#    3. each demo page's script parses, and instantiates the module ASYNCHRONOUSLY -
#       a browser refuses synchronous compilation over 4 KB on the main thread, which
#       node does not, so this one has to be read out of the text;
#    4. each demo page's module is the one its source compiles to TODAY.
#
#  Usage:  bash bas/demo/verify_site.sh
#          bash bas/demo/verify_site.sh --bless    re-embed every module
# =====================================================================================
set -u
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
root=$(cd "$here/.." && pwd)
site="$here/site"
bless=0
[ "${1:-}" = "--bless" ] && bless=1

sbc="$root/../bin/x86_64-linux/sbc"; [ -x "$sbc" ] || sbc="$root/../bin/sbc.exe"
sbc=$(cd "$(dirname "$sbc")" && pwd)/$(basename "$sbc")
[ -x "$sbc" ] || { echo "sbc not found - build it first: ./build.sh sbc" >&2; exit 1; }

work=$(mktemp -d); trap 'rm -rf "$work"' EXIT
bad=0

# ---- 1 & 2. the links --------------------------------------------------------------
python3 - "$site" <<'PY' || bad=1
import os, re, sys
site = sys.argv[1]
pages = []
for dirpath, _, files in os.walk(site):
    for f in files:
        if f.endswith('.html'):
            pages.append(os.path.join(dirpath, f))
bad = 0
linked_from_index = set()
for p in sorted(pages):
    rel = os.path.relpath(p, site)
    text = open(p, encoding='utf-8').read()
    for attr in ('href', 'src'):
        for ref in re.findall(attr + r'="([^"]+)"', text):
            if ref.startswith(('http', 'data:', '#', 'mailto:')):
                continue
            target = os.path.normpath(os.path.join(os.path.dirname(p), ref))
            if not os.path.exists(target):
                print(f'  ⛔ {rel}: {attr}="{ref}" resolves to nothing'); bad = 1
            elif rel == 'index.html' and ref.endswith('.html'):
                linked_from_index.add(os.path.normpath(os.path.join(site, ref)))
    # every page except the index itself must offer a way back to it
    if rel != 'index.html' and '../index.html' not in text and 'href="index.html"' not in text:
        print(f'  ⛔ {rel}: no way back to the index of demos'); bad = 1
for p in sorted(pages):
    rel = os.path.relpath(p, site)
    if rel == 'index.html':
        continue
    # a demo folder's own index must be reachable from the top one
    if rel.endswith('/index.html') and os.path.normpath(p) not in linked_from_index:
        print(f'  ⛔ {rel} is not linked from the index of demos'); bad = 1
print(f'  {len(pages)} pages · every local link resolves · every page can get back')
raise SystemExit(bad)
PY

# ---- 3 & 4. each demo page's script and module --------------------------------------
# demo folder : source it is compiled from
check() {
  local dir="$1" src="$2" page="$site/$1/$2.html"
  [ -f "$page" ] || { echo "  ⛔ $1/$2.html is missing"; bad=1; return; }
  if ! out=$("$sbc" "$here/$3" --target wasm "$work/$2.wasm" 2>&1); then
    echo "$out" | sed 's/^/    /'; echo "  ⛔ the WASM backend refused $3"; bad=1; return
  fi
  python3 - "$page" "$work/$2.wasm" "$bless" "$1/$2.html" <<'PY' || bad=1
import base64, re, sys
page, wasm, bless, label = sys.argv[1], sys.argv[2], sys.argv[3] == '1', sys.argv[4]
s = open(page, encoding='utf-8').read()
m = re.search(r"<script>\n(.*)</script>", s, re.S)
if not m:
    print(f'  ⛔ {label}: no <script> block'); raise SystemExit(1)
# ⚠️ Comments off FIRST. Each page EXPLAINS the synchronous-compilation trap by naming the call it
# must not make, and a check that grepped the raw text would be satisfied by that comment alone.
code = re.sub(r'/\*.*?\*/', '', re.sub(r'//[^\n]*', '', m.group(1)), flags=re.S)
bad = 0
if re.search(r'new\s+WebAssembly\.(Module|Instance)\s*\(', code):
    print(f'  ⛔ {label}: compiles the module SYNCHRONOUSLY - a browser refuses that over 4 KB'); bad = 1
if 'WebAssembly.instantiate(' not in code:
    print(f'  ⛔ {label}: never calls WebAssembly.instantiate'); bad = 1
if 'fb32.buffer !== memory.buffer' not in code:
    print(f'  ⛔ {label}: never rebuilds its view when the module memory grows'); bad = 1
b64 = base64.b64encode(open(wasm, 'rb').read()).decode()
if bless:
    open(page, 'w', encoding='utf-8').write(
        re.sub(r'const B64 = "[^"]*";', 'const B64 = "' + b64 + '";', s, count=1))
elif f'const B64 = "{b64}";' not in s:
    print(f'  ⛔ {label}: carries a DIFFERENT module than its source compiles to'); bad = 1
raise SystemExit(bad)
PY
}

check buddhabrot      buddhabrot      buddhabrot/buddhabrot.bas
check bubble_universe bubble_universe bubble_universe.bas
check voxel_landscape voxel_landscape voxel_landscape.bas

if [ "$bad" != 0 ]; then
  echo "⛔ the site is not publishable as it stands"
  [ "$bless" = 0 ] && echo "   If only the modules are stale:  bash bas/demo/verify_site.sh --bless"
  exit 1
fi
if [ "$bless" = 1 ]; then
  echo "✅ site: links resolve · scripts sound · modules re-embedded"
else
  echo "✅ site: links resolve · every page reachable · scripts sound · modules current"
fi
