# SedaiBasic Vision Console - Keyboard Reference

This document describes all keyboard shortcuts available in the SedaiBasic Vision SDL2 console (sbv.exe).

## Cursor Movement

| Key | Action |
|-----|--------|
| `Left Arrow` | Move cursor one character left |
| `Right Arrow` | Move cursor one character right |
| `Home` | Move cursor to beginning of line |
| `End` | Move cursor to end of line |
| `Ctrl+Left` | Jump to previous word |
| `Ctrl+Right` | Jump to next word |
| `Ctrl+A` | Move to beginning of line (readline style) |
| `Ctrl+E` | Move to end of line (readline style) |

## Text Editing

| Key | Action |
|-----|--------|
| `Backspace` | Delete character before cursor |
| `Delete` | Delete character at cursor position |
| `Ctrl+Backspace` | Delete word to the left (until space) |
| `Ctrl+Delete` | Delete word to the right (until space) |
| `Ctrl+Home` | Delete from cursor to beginning of line |
| `Ctrl+End` | Delete from cursor to end of line |
| `Ctrl+K` | Delete from cursor to end of line (readline style) |
| `Ctrl+U` | Delete entire line (readline style) |

## Insert Mode

When typing characters, the console operates in **insert mode**:
- Characters at and after the cursor position are shifted right
- The new character is inserted at the cursor position
- The cursor advances one position to the right

## Scrollback Buffer

The console maintains a scrollback buffer of previous output.

| Key | Action |
|-----|--------|
| `Shift+Page Up` | Scroll up through history |
| `Shift+Page Down` | Scroll down through history |
| `Shift+Home` | Scroll to oldest content in buffer |
| `Shift+End` | Return to current line (exit scrollback) |
| `Any other key` | Exit scrollback mode and process the key |

When viewing scrollback (not at current line):
- The cursor is hidden
- Any non-navigation key returns to the current line
- The pressed key is then processed normally

## System Commands

| Key | Action |
|-----|--------|
| `Pause/Break` | Stop (BREAK) running BASIC program — the `Pausa/Interr` key, with or without Ctrl. Works in any state (loops, audio, GET/GETKEY, INPUT) |
| `Ctrl+Alt+End` | Exit SedaiVision |
| `Ctrl+F` | Toggle fullscreen mode (also while a program is running) |
| `Enter` | Execute command / New line |

> **Break key:** the `Pause/Break` key (`Pausa/Interr` on Italian keyboards) stops a running program, with or without Ctrl. In the graphical build (`sbv`) Ctrl is not required because Windows reports `Ctrl+Pause` (the "Break" variant) inconsistently to SDL2. In the console build (`sb`) the native Windows `CTRL_BREAK_EVENT` is used (`Ctrl+Break`), and that build also honours `Ctrl+C` so it never kills the interpreter process. `Esc` is **not** a break key — it is delivered to the program as `CHR$(27)` and free for program use.

## Cursor Display

- The cursor blinks at a rate of 530ms
- When positioned over a character, the character is displayed in **reverse video** (foreground and background colors swapped)
- When positioned over an empty space, a solid block cursor is shown

## Line Input

| Key | Action |
|-----|--------|
| `Up Arrow` | Recall previous command from history |
| `Down Arrow` | Recall next command from history |
| `Enter` | Submit current line |

## Graphics Modes (GRAPHIC command)

The `GRAPHIC` command sets the video mode. Syntax: `GRAPHIC mode [, clear [, param3]]`

| Mode | Constant | Resolution | Description |
|------|----------|------------|-------------|
| 0 | gm40ColText | 320x200 | 40x25 text mode (C64/C128 compatible) |
| 1 | gmStandardBitmap | 320x200 | Standard bitmap mode |
| 2 | gmSplitBitmap | 320x200 | Split screen: bitmap + text (default: 40x5 text) |
| 3 | gmMulticolorBitmap | 160x200 | Multicolor bitmap (double-width pixels) |
| 4 | gmSplitMulticolor | 160x200 | Split screen: multicolor + text (default: 40x5 text) |
| 5 | gm80ColText | 640x200 | 80x25 text mode (C128 compatible) |
| 6 | gm80ColBitmap | 640x200 | 640x200 hires bitmap |
| 7 | gm80ColMixed | 640x200 | Split screen: 640x160 bitmap + 80x5 text |
| 8 | gm80x50Text | 640x400 | 80x50 text mode |
| 9 | gm80x50Bitmap | 640x400 | 640x400 hires bitmap |
| 10 | gm80x50Mixed | 640x400 | Split screen: 640x360 bitmap + 80x5 text |
| 11 | gmSDL2Dynamic | Variable | SDL2 dynamic resolution |

### Parameters

- **mode**: Graphics mode number (0-11)
- **clear**: Optional. 0 = don't clear, 1 = clear buffer (default: 1)
- **param3**: Optional. Meaning depends on mode:
  - Modes 0-10: Split line position (-1 = use default)
  - Mode 11: SDL2 video mode index from `GLIST` command

### Mode 11 - SDL2 Dynamic Resolution

Mode 11 allows selecting any resolution supported by the display hardware.

1. Use `GLIST` command to see available SDL2 video modes with their indices
2. Use `GRAPHIC 11, clear, index` to set the desired resolution

Example:
```basic
GLIST               ' Show available modes
GRAPHIC 11, 1, 5    ' Set mode 5 from the list, clear buffer
```

The `GLIST` output shows:
- `*` = native desktop resolution
- `+` = same aspect ratio as desktop

## Sprite Editor (SPRDEF)

`SPRDEF [n]` opens an interactive, modal sprite editor (graphical build `sbv` only). It
shows the grid in a fixed-size **viewport** with row/column rulers and a live cursor position
below it, plus a thumbnail preview. Cells are drawn at a usable size; for sprites larger than
fit (e.g. up to 255×255) the viewport **scrolls to follow the cursor** (move with the arrows),
and the command panel stays in a fixed right-hand column so it never goes off-screen. The editor supports all three sprite
formats — **hi-res**, **multicolor** and **full-color** (256-palette, 8bpp) — and **any
size up to 256×256**. `RETURN` (or `[Save (Return)]`) commits the current shape to the sprite
and shows `SAVED` (which clears on the next edit) but stays in the editor; switching sprite
also commits. `ESC` (or `[Exit (Esc)]`) leaves the editor.

| Key | Action |
|-----|--------|
| `H` / `F1` | Show / hide the full-screen help overlay (close with `H`/`F1`/`ESC`) |
| `Arrows` | Move the cursor (steps of 2 columns in multicolor) |
| `SPACE` | Paint: hi-res toggles the pixel; multicolor cycles 0→1→2→3→0; full-color paints the pen |
| `0` | Erase the pixel under the cursor |
| `1`–`8` | Switch the sprite being edited (each sprite keeps its own state and undo history) |
| `M` | Cycle the format hi-res → multicolor → full-color (asks `Y/N`; the grid is cleared) |
| `S` | Focus the `W` size field; type an integer (1..255), `TAB` to `H`, `RETURN` applies, `ESC` cancels. Preset sizes are buttons in the panel |
| `P` | Cycle the multicolor mouse pen (MC1 / sprite / MC2) |
| `X` / `Y` | Toggle horizontal / vertical expand |
| `C` / `[` / `]` | Cycle the pen colour (sprite colour, MC1/MC2 in multicolor, or the 0–255 pen in full-color) |
| `Ctrl+Z` / `Ctrl+Y` | Undo / redo — independent history per sprite (covers paint, resize and format changes) |
| `Ctrl+C` / `Ctrl+V` | Copy / paste: the selection if any, otherwise the whole shape |
| `Shift+Arrows` | Rectangular selection (`Shift+Ctrl+Arrows` adds a rectangle to it) |
| `Ctrl+Space` | Toggle a single pixel in the selection |
| `DEL` | Clear the whole grid |
| `Ctrl+S` / `Ctrl+L` | Save / load all sprites to/from a file (filename typed below the grid) |
| `RETURN` | **Save** — commit the current shape to the sprite (shows `SAVED`); does **not** exit |
| `ESC` | **Exit** the editor |

**Mouse** (when present): on the grid, left button paints with the active pen, right button
erases, the middle button just repositions the cursor (no painting), and dragging draws a
continuous stroke (one undo step per stroke).

**Widget panel** (right-hand column): a minimal immediate-mode UI (unit `SedaiUIWidgets.pas`,
reusable) mirrors the keyboard commands with clickable controls on a uniform grid — format
**radio buttons** (drawn as circles: hi-res / multicolor / full-color), expand **checkboxes**
(X / Y), a pen `[<] value [>]` stepper with a colour-preview swatch and a `[Pick...]` button,
two **right-aligned integer size fields** (`W` / `H`, 1..255) with a `[Set]` button and **preset
size buttons** (`24x21 … 48x42`), `[Save (Return)]` / `[Exit (Esc)]`, `[Save file…]` / `[Load file…]`,
and `[HELP]`. Click a size field to edit it (`TAB` switches W/H, `RETURN`/`[Set]` applies).
Changing format from a radio (or `M`) clears the sprite, so it asks for confirmation with inline
`[Yes]`/`[No]` buttons (or `Y`/`N`). The keyboard shortcuts in the table above keep working in
parallel with the panel.

**Colour pop-up**: `[Pick...]` (or step with `C` / `[` `]`) opens a small palette window **beside
the colour swatch** showing the 256-colour palette as a 16×16 grid (centred in the window); the
current colour has a thick high-contrast marker. Click a swatch to set the pen, or press `ESC` /
click outside to cancel. (A per-sprite palette with selectable presets — C64 / VGA / grayscale — is
planned; today the swatches show the program's current palette.)

**Size presets:** `C128 24×21`, `8×8`, `16×16`, `32×32`, `64×64`, `16×32`, `32×64`, `48×42`.
Resizing keeps the overlapping top-left region of the shape.

Sprite files use the `.spr` extension (JSON content) and default to the running program's
folder. See `SPRSAVE`/`SPRLOAD`/`SPRSIZE`/`SPRFORM` in `BASIC.md`; `SPRLOAD "file",1`
restores the file's colours, while the default keeps the program's current colours.

## Notes

- All editing operations work on the current input line only
- The scrollback buffer preserves output from previous commands
- Maximum line length is determined by the current video mode
- Word boundaries for Ctrl+Arrow and Ctrl+Backspace/Delete are defined by space characters
