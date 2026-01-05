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
| `Ctrl+C` | Stop running BASIC program |
| `Ctrl+Alt+End` | Exit SedaiVision |
| `Ctrl+F` | Toggle fullscreen mode |
| `Enter` | Execute command / New line |

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

## Notes

- All editing operations work on the current input line only
- The scrollback buffer preserves output from previous commands
- Maximum line length is determined by the current video mode
- Word boundaries for Ctrl+Arrow and Ctrl+Backspace/Delete are defined by space characters
