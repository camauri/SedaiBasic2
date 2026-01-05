{*
 * SedaiBasic - A BASIC interpreter with bytecode VM
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * This program is dual-licensed:
 *
 * 1) For open source use: GNU General Public License version 3 (GPL-3.0-only)
 *    You may redistribute and/or modify it under the terms of the GNU GPL v3
 *    as published by the Free Software Foundation.
 *    See <https://www.gnu.org/licenses/gpl-3.0.html>
 *
 * 2) For commercial/proprietary use: A separate commercial license is required.
 *    Contact: maurizio.cammalleri@gmail.com for licensing inquiries.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}
unit SedaiGraphicsConfig;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  SedaiOutputInterface;

const
  // ===== C64 COLOR PALETTE =====
  C64_PALETTE_SIZE = 16;

type
  TC64ColorInfo = record
    Name: string;
    RGBA: UInt32;
  end;

  TGraphicModeConfig = record
    Width: Integer;
    Height: Integer;
    PaletteMode: Boolean;
    Description: string;
  end;

const
  // C64/C128 Style Palette - User defined
  // Format: $AABBGGRR (ABGR) for SDL memory layout on little-endian
  // UInt32 = (A shl 24) or (B shl 16) or (G shl 8) or R
  C64_PALETTE: array[0..C64_PALETTE_SIZE-1] of TC64ColorInfo = (
    (Name: 'Black';       RGBA: $FF000000),  // 0:  #000000
    (Name: 'White';       RGBA: $FFFFFFFF),  // 1:  #FFFFFF
    (Name: 'Red';         RGBA: $FF516ABB),  // 2:  #BB6A51
    (Name: 'Cyan';        RGBA: $FFFFF3A9),  // 3:  #A9F3FF
    (Name: 'Purple';      RGBA: $FFFB6EBF),  // 4:  #BF6EFB
    (Name: 'Green';       RGBA: $FF51E598),  // 5:  #98E551
    (Name: 'Blue';        RGBA: $FFF55369),  // 6:  #6953F5
    (Name: 'Yellow';      RGBA: $FF7BFFFF),  // 7:  #FFFF7B
    (Name: 'Orange';      RGBA: $FF3292C6),  // 8:  #C69232
    (Name: 'Brown';       RGBA: $FF00798D),  // 9:  #8D7900
    (Name: 'Light Red';   RGBA: $FF96ABF5),  // 10: #F5AB96
    (Name: 'Dark Grey';   RGBA: $FF818181),  // 11: #818181
    (Name: 'Medium Grey'; RGBA: $FFB6B6B6),  // 12: #B6B6B6
    (Name: 'Light Green'; RGBA: $FF9EFFDB),  // 13: #DBFF9E
    (Name: 'Light Blue';  RGBA: $FFFF9EB1),  // 14: #B19EFF
    (Name: 'Light Grey';  RGBA: $FFE0E0E0)   // 15: #E0E0E0
  );

  // Default C64 colors
  C64_COLOR_BLACK = 0;
  C64_COLOR_WHITE = 1;
  C64_COLOR_RED = 2;
  C64_COLOR_CYAN = 3;
  C64_COLOR_VIOLET = 4;
  C64_COLOR_GREEN = 5;
  C64_COLOR_BLUE = 6;
  C64_COLOR_YELLOW = 7;
  C64_COLOR_ORANGE = 8;
  C64_COLOR_BROWN = 9;
  C64_COLOR_LIGHT_RED = 10;
  C64_COLOR_DARK_GREY = 11;
  C64_COLOR_MEDIUM_GREY = 12;
  C64_COLOR_LIGHT_GREEN = 13;
  C64_COLOR_LIGHT_BLUE = 14;
  C64_COLOR_LIGHT_GREY = 15;

  // Graphics mode configurations (C64/C128 compatible + dynamic SDL2)
  GRAPHIC_MODE_CONFIGS: array[TGraphicMode] of TGraphicModeConfig = (
    // === 320x200 group (GRAPHIC 0-2) ===
    // gm40ColText = 0: GRAPHIC 0
    (Width: 320; Height: 200; PaletteMode: True; Description: '40x25 text mode'),
    // gmStandardBitmap = 1: GRAPHIC 1
    (Width: 320; Height: 200; PaletteMode: True; Description: 'Standard bitmap 320x200'),
    // gmSplitBitmap = 2: GRAPHIC 2
    (Width: 320; Height: 200; PaletteMode: True; Description: 'Split screen bitmap'),
    // === 160x200 group (GRAPHIC 3-4) ===
    // gmMulticolorBitmap = 3: GRAPHIC 3
    (Width: 160; Height: 200; PaletteMode: True; Description: 'Multicolor bitmap'),
    // gmSplitMulticolor = 4: GRAPHIC 4
    (Width: 160; Height: 200; PaletteMode: True; Description: 'Split multicolor'),
    // === 640x200 group (GRAPHIC 5-7) ===
    // gm80ColText = 5: GRAPHIC 5
    (Width: 640; Height: 200; PaletteMode: True; Description: '80x25 text mode'),
    // gm80ColBitmap = 6: GRAPHIC 6
    (Width: 640; Height: 200; PaletteMode: True; Description: '80col bitmap 640x200'),
    // gm80ColMixed = 7: GRAPHIC 7
    (Width: 640; Height: 200; PaletteMode: True; Description: '80col mixed 640x200'),
    // === 640x400 group (GRAPHIC 8-10) ===
    // gm80x50Text = 8: GRAPHIC 8
    (Width: 640; Height: 400; PaletteMode: True; Description: '80x50 text mode'),
    // gm80x50Bitmap = 9: GRAPHIC 9
    (Width: 640; Height: 400; PaletteMode: True; Description: '80x50 bitmap 640x400'),
    // gm80x50Mixed = 10: GRAPHIC 10
    (Width: 640; Height: 400; PaletteMode: True; Description: '80x50 mixed 640x400'),
    // === SDL2 Dynamic (GRAPHIC 11) ===
    // gmSDL2Dynamic = 11: GRAPHIC 11, clear, sdl2_mode_index
    (Width: 0; Height: 0; PaletteMode: False; Description: 'Dynamic SDL2 resolution')
  );

  // Memory and rendering constants
  BYTES_PER_PIXEL_RGBA = 4;

  // Default screen dimensions
  DEFAULT_SCREEN_WIDTH = 320;
  DEFAULT_SCREEN_HEIGHT = 200;
  DEFAULT_TEXT_COLS = 40;
  DEFAULT_TEXT_ROWS = 25;

// Helper functions
function GetC64ColorRGBA(ColorIndex: Integer): UInt32; inline;
function GetC64ColorName(ColorIndex: Integer): string; inline;
function GetGraphicModeConfig(Mode: TGraphicMode): TGraphicModeConfig; inline;

implementation

function GetC64ColorRGBA(ColorIndex: Integer): UInt32;
begin
  if (ColorIndex >= 0) and (ColorIndex < C64_PALETTE_SIZE) then
    Result := C64_PALETTE[ColorIndex].RGBA
  else
    Result := C64_PALETTE[C64_COLOR_BLACK].RGBA; // Default to black
end;

function GetC64ColorName(ColorIndex: Integer): string;
begin
  if (ColorIndex >= 0) and (ColorIndex < C64_PALETTE_SIZE) then
    Result := C64_PALETTE[ColorIndex].Name
  else
    Result := 'Unknown';
end;

function GetGraphicModeConfig(Mode: TGraphicMode): TGraphicModeConfig;
begin
  Result := GRAPHIC_MODE_CONFIGS[Mode];
end;

end.
