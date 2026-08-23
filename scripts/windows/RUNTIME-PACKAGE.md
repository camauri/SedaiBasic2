# The runtime package

`setup.ps1` and `scripts/windows/install-runtime-x86_64.ps1` install the Win64 runtime into
`bin\x86_64-win64`.

**Every DLL is an official [libsdl-org](https://github.com/libsdl-org) binary, unmodified. Only the
selection is ours.**

| | |
|---|---|
| Release | `Runtime-v2-x86_64-win64` on `camauri/SedaiBasic2-Deps` |
| Our archive | `sedai_runtime-x86_64-win64.zip`, 3.6 MB, `x86_64-win64/` root |
| Our SHA-256 | `d84cbf451426dfcc8bba3b1526e2f0522def89af8af54795a3f0c8e67f16453d` |

## Contents

| File | Version | Upstream archive |
|---|---|---|
| `SDL2.dll` | 2.32.10 | `SDL2-2.32.10-win32-x64.zip` |
| `SDL2_ttf.dll` | 2.24.0 | `SDL2_ttf-2.24.0-win32-x64.zip` |
| `SDL2_image.dll` | 2.8.12 | `SDL2_image-2.8.12-win32-x64.zip` |
| `font/PixelOperatorMono8-Bold.ttf` | | the console font, ours |

All three DLLs are self-contained: their only imports are Win32 system DLLs and `SDL2.dll`. No C
runtime, no codec library. FreeType, libpng, libjpeg and zlib are linked INSIDE `SDL2_ttf.dll` and
`SDL2_image.dll`.

⛔ The package used to carry `zlib1.dll`, `freetype.dll`, `libjpeg-8.dll` and `libpng16-16.dll`
beside a MinGW `SDL2_image.dll` that imported them by name. Two of the four could never have loaded:
`freetype.dll` needs `VCRUNTIME140.dll` and `libjpeg-8.dll` needs `libgcc_s_seh-1.dll`, and neither
was shipped. That `SDL2_image.dll` also wanted `libavif-16`, `libjxl`, `libtiff-6`, `libwebp-7` and
`libwebpdemux-2`, none of which were shipped either, so it could not load at all. The official build
replaces all six files with one.

## Image formats

`SDL2_image.dll` decodes BMP, GIF, JPEG, LBM, PCX, PNG, PNM, QOI, TGA, XCF, XPM and simple SVG with
no help. AVIF, JPEG-XL, TIFF and WebP need codec DLLs loaded at run time, and those are NOT shipped:
`IMG_Load` returns an error for them instead of crashing. The upstream archive carries four of them
in `optional/` (3.5 MB) if they are ever wanted; there is no `libjxl` even there.

⚠️ Nothing in SedaiBasic loads `SDL2_image.dll` yet. Only `sbv.exe` binds an SDL2 DLL at all, and it
binds `SDL2_ttf.dll`. The file is shipped so the day something uses it nothing has to be installed.

## Rebuilding for new SDL2 versions

1. Take the `win32-x64` runtime zip of each of SDL, SDL_ttf and SDL_image from their GitHub releases.
2. Check that SDL2_image's imports from `SDL2.dll` are all exported by the SDL2.dll being shipped:
   `objdump -p SDL2_image.dll` against `objdump -p SDL2.dll`. 68 symbols on this pair, all present.
3. Zip as `x86_64-win64/`, with the font, and update `$EXPECTED_HASH` in
   `install-runtime-x86_64.ps1`.

⚠️ The SDL2 bindings decide the floor: `SDL2_BINDINGS_URL` in `scripts/lib/deps-linux.sh` pins
SDL2-for-Pascal v2.3, which needs SDL2 2.30.0 and SDL2_ttf 2.22.0 or newer. Linux installs whatever
the distribution has, so those two minimums are what keep the platforms interchangeable.

## Licences

SDL2, SDL2_ttf and SDL2_image are zlib-licensed, as is the FreeType/libpng/libjpeg/zlib code linked
into them (FreeType under the FTL, libpng under the libpng licence, both permissive and satisfied by
notice). PixelOperator is public domain.
