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
unit SedaiGraphicsModes;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Math, SedaiOutputInterface;

type
  TModeType = (mtText, mtBitmap, mtMixed);

  TPaletteType = (
    ptC64_16,        // 16 Commodore 64/128 colors (repeated)
    ptVGA_256,       // 256 VGA Mode 13h colors
    ptTrueColor      // RGB 24-bit
  );

  TGraphicModeInfo = record
    Mode: TGraphicMode;
    Name: string;
    ModeType: TModeType;
    // Native resolution of the mode (viewport)
    NativeWidth: Integer;
    NativeHeight: Integer;
    // Window resolution (for fullscreen, 0 = use NativeWidth/Height)
    WindowWidth: Integer;
    WindowHeight: Integer;
    // Text grid (if applicable)
    TextCols: Integer;
    TextRows: Integer;
    // Split screen
    DefaultSplitLine: Integer;  // 0 = no split, >0 = split line
    // Palette
    PaletteType: TPaletteType;
    // Supports C64/128 border?
    HasBorder: Boolean;
    // Requires clear buffer on change?
    RequiresClear: Boolean;
  end;

  // Class for programmable palette management
  TPaletteArray = array[0..255] of UInt32;
  PPaletteArray = ^TPaletteArray;

  TPaletteManager = class
  private
    FPalette: TPaletteArray;
    FPaletteType: TPaletteType;
    FModified: Boolean;

    function GetPalettePtr: PPaletteArray;

  public
    constructor Create(PaletteType: TPaletteType);

    procedure LoadC64Palette;
    procedure LoadC64PaletteVICE;
    procedure LoadVGAPalette;
    procedure LoadDefaultPalette;
    procedure LoadPredefinedPalette(PaletteID: Integer);

    procedure SetColor(Index: Integer; R, G, B: Byte); overload;
    procedure SetColor(Index: Integer; RGB: UInt32); overload;
    function GetColor(Index: Integer): UInt32;
    function IndexToRGBA(Index: Byte): UInt32;

    procedure FadeToBlack(Steps: Integer; CurrentStep: Integer);
    procedure FadeFromBlack(Steps: Integer; CurrentStep: Integer);
    procedure RotatePalette(StartIdx, EndIdx: Integer; Direction: Integer);

    procedure SavePalette(var Buffer: TPaletteArray);
    procedure RestorePalette(const Buffer: TPaletteArray);

    property PaletteType: TPaletteType read FPaletteType;
    property Modified: Boolean read FModified;
    property PalettePtr: PPaletteArray read GetPalettePtr;
  end;

const
  GRAPHICS_MODES: array[TGraphicMode] of TGraphicModeInfo = (
    // === 320x200 group (shared buffer: FBuffer320x200) ===
    (Mode: gm40ColText; Name: 'GRAPHIC 0'; ModeType: mtText;
     NativeWidth: 320; NativeHeight: 200; WindowWidth: 640; WindowHeight: 400;
     TextCols: 40; TextRows: 25;
     DefaultSplitLine: 0; PaletteType: ptC64_16; HasBorder: True; RequiresClear: False),

    (Mode: gmStandardBitmap; Name: 'GRAPHIC 1'; ModeType: mtBitmap;
     NativeWidth: 320; NativeHeight: 200; WindowWidth: 0; WindowHeight: 0;
     TextCols: 0; TextRows: 0;
     DefaultSplitLine: 0; PaletteType: ptC64_16; HasBorder: True; RequiresClear: False),

    (Mode: gmSplitBitmap; Name: 'GRAPHIC 2'; ModeType: mtMixed;
     NativeWidth: 320; NativeHeight: 200; WindowWidth: 0; WindowHeight: 0;
     TextCols: 40; TextRows: 5;
     DefaultSplitLine: 19; PaletteType: ptC64_16; HasBorder: True; RequiresClear: False),

    // === 160x200 group (shared buffer: FBuffer160x200) ===
    (Mode: gmMulticolorBitmap; Name: 'GRAPHIC 3'; ModeType: mtBitmap;
     NativeWidth: 160; NativeHeight: 200; WindowWidth: 0; WindowHeight: 0;
     TextCols: 0; TextRows: 0;
     DefaultSplitLine: 0; PaletteType: ptC64_16; HasBorder: True; RequiresClear: False),

    (Mode: gmSplitMulticolor; Name: 'GRAPHIC 4'; ModeType: mtMixed;
     NativeWidth: 160; NativeHeight: 200; WindowWidth: 0; WindowHeight: 0;
     TextCols: 40; TextRows: 5;
     DefaultSplitLine: 19; PaletteType: ptC64_16; HasBorder: True; RequiresClear: False),

    // === 640x200 group (shared buffer: FBuffer640x200) ===
    (Mode: gm80ColText; Name: 'GRAPHIC 5'; ModeType: mtText;
     NativeWidth: 640; NativeHeight: 200; WindowWidth: 0; WindowHeight: 0;
     TextCols: 80; TextRows: 25;
     DefaultSplitLine: 0; PaletteType: ptC64_16; HasBorder: True; RequiresClear: False),

    (Mode: gm80ColBitmap; Name: 'GRAPHIC 6'; ModeType: mtBitmap;
     NativeWidth: 640; NativeHeight: 200; WindowWidth: 0; WindowHeight: 0;
     TextCols: 0; TextRows: 0;
     DefaultSplitLine: 0; PaletteType: ptC64_16; HasBorder: True; RequiresClear: False),

    (Mode: gm80ColMixed; Name: 'GRAPHIC 7'; ModeType: mtMixed;
     NativeWidth: 640; NativeHeight: 200; WindowWidth: 0; WindowHeight: 0;
     TextCols: 80; TextRows: 5;
     DefaultSplitLine: 19; PaletteType: ptC64_16; HasBorder: True; RequiresClear: False),

    // === 640x400 group (shared buffer: FBuffer640x400) ===
    (Mode: gm80x50Text; Name: 'GRAPHIC 8'; ModeType: mtText;
     NativeWidth: 640; NativeHeight: 400; WindowWidth: 960; WindowHeight: 540;
     TextCols: 80; TextRows: 50;
     DefaultSplitLine: 0; PaletteType: ptC64_16; HasBorder: True; RequiresClear: False),

    (Mode: gm80x50Bitmap; Name: 'GRAPHIC 9'; ModeType: mtBitmap;
     NativeWidth: 640; NativeHeight: 400; WindowWidth: 0; WindowHeight: 0;
     TextCols: 0; TextRows: 0;
     DefaultSplitLine: 0; PaletteType: ptC64_16; HasBorder: True; RequiresClear: False),

    (Mode: gm80x50Mixed; Name: 'GRAPHIC 10'; ModeType: mtMixed;
     NativeWidth: 640; NativeHeight: 400; WindowWidth: 0; WindowHeight: 0;
     TextCols: 80; TextRows: 5;
     DefaultSplitLine: 44; PaletteType: ptC64_16; HasBorder: True; RequiresClear: False),

    // === Dynamic SDL2 mode (no persistent buffer) ===
    (Mode: gmSDL2Dynamic; Name: 'GRAPHIC 11'; ModeType: mtBitmap;
     NativeWidth: 0; NativeHeight: 0; WindowWidth: 0; WindowHeight: 0;
     TextCols: 0; TextRows: 0;
     DefaultSplitLine: 0; PaletteType: ptTrueColor; HasBorder: False; RequiresClear: True)
  );

  // C64 16-color palette - Colodore/Wikipedia (Philip Timmermann research)
  // This is the standard palette used by most emulators
  C64_PALETTE_COLODORE: array[0..15] of TColor = (
    (R: $00; G: $00; B: $00),  // 0: Black
    (R: $FF; G: $FF; B: $FF),  // 1: White
    (R: $9F; G: $4E; B: $44),  // 2: Red
    (R: $6A; G: $BF; B: $C6),  // 3: Cyan
    (R: $A0; G: $57; B: $A3),  // 4: Purple
    (R: $5C; G: $AB; B: $5E),  // 5: Green
    (R: $50; G: $45; B: $9B),  // 6: Blue
    (R: $C9; G: $D4; B: $87),  // 7: Yellow
    (R: $A1; G: $68; B: $3C),  // 8: Orange
    (R: $6D; G: $54; B: $12),  // 9: Brown
    (R: $CB; G: $7E; B: $75),  // 10: Light Red (Pink)
    (R: $62; G: $62; B: $62),  // 11: Dark Grey
    (R: $89; G: $89; B: $89),  // 12: Medium Grey
    (R: $9A; G: $E2; B: $9B),  // 13: Light Green
    (R: $88; G: $7E; B: $CB),  // 14: Light Blue
    (R: $AD; G: $AD; B: $AD)   // 15: Light Grey
  );

  // C64 palette - VICE/Ultimate64 (more vivid colors)
  C64_PALETTE_VICE: array[0..15] of TColor = (
    (R: $00; G: $00; B: $00),  // 0: Black
    (R: $EF; G: $EF; B: $EF),  // 1: White
    (R: $8D; G: $2F; B: $34),  // 2: Red
    (R: $6A; G: $D4; B: $CD),  // 3: Cyan
    (R: $98; G: $35; B: $A4),  // 4: Purple
    (R: $4C; G: $B4; B: $42),  // 5: Green
    (R: $2C; G: $29; B: $B1),  // 6: Blue
    (R: $EF; G: $EF; B: $5D),  // 7: Yellow
    (R: $98; G: $4E; B: $20),  // 8: Orange
    (R: $5B; G: $38; B: $00),  // 9: Brown
    (R: $D1; G: $67; B: $6D),  // 10: Light Red (Pink)
    (R: $4A; G: $4A; B: $4A),  // 11: Dark Grey
    (R: $7B; G: $7B; B: $7B),  // 12: Medium Grey
    (R: $9F; G: $EF; B: $93),  // 13: Light Green
    (R: $6D; G: $6A; B: $EF),  // 14: Light Blue
    (R: $B2; G: $B2; B: $B2)   // 15: Light Grey
  );

  // Default C64 palette (use Colodore as default)
  C64_PALETTE_RGB: array[0..15] of TColor = (
    (R: $00; G: $00; B: $00),  // 0: Black
    (R: $FF; G: $FF; B: $FF),  // 1: White
    (R: $9F; G: $4E; B: $44),  // 2: Red
    (R: $6A; G: $BF; B: $C6),  // 3: Cyan
    (R: $A0; G: $57; B: $A3),  // 4: Purple
    (R: $5C; G: $AB; B: $5E),  // 5: Green
    (R: $50; G: $45; B: $9B),  // 6: Blue
    (R: $C9; G: $D4; B: $87),  // 7: Yellow
    (R: $A1; G: $68; B: $3C),  // 8: Orange
    (R: $6D; G: $54; B: $12),  // 9: Brown
    (R: $CB; G: $7E; B: $75),  // 10: Light Red (Pink)
    (R: $62; G: $62; B: $62),  // 11: Dark Grey
    (R: $89; G: $89; B: $89),  // 12: Medium Grey
    (R: $9A; G: $E2; B: $9B),  // 13: Light Green
    (R: $88; G: $7E; B: $CB),  // 14: Light Blue
    (R: $AD; G: $AD; B: $AD)   // 15: Light Grey
  );

  // Predefined palette types for LoadPredefinedPalette
  C64_PALETTE_TYPE_COLODORE = 0;
  C64_PALETTE_TYPE_VICE = 1;

type
  TViewportCalculator = class
  private
    FMonitorWidth: Integer;
    FMonitorHeight: Integer;
    FMonitorAspectRatio: Double;

    function CalculateFontSize(NativeWidth, NativeHeight, TextCols, TextRows: Integer): Integer;
    function GetNearestMultipleOf8(Value: Integer): Integer;

  public
    constructor Create(MonitorWidth, MonitorHeight: Integer);

    function CalculateViewport(const ModeInfo: TGraphicModeInfo;
                              UseMaxResolution: Boolean;
                              out ViewportWidth, ViewportHeight: Integer;
                              out WindowWidth, WindowHeight: Integer;
                              out FontSize: Integer): Boolean;

    function FindBestTextMode(PreferHighRes: Boolean = True): TGraphicMode;
    function FindBestModeForResolution(TargetWidth, TargetHeight: Integer): TGraphicMode;

    property MonitorWidth: Integer read FMonitorWidth;
    property MonitorHeight: Integer read FMonitorHeight;
    property MonitorAspectRatio: Double read FMonitorAspectRatio;
  end;

function GetModeInfo(Mode: TGraphicMode): TGraphicModeInfo;

implementation

function GetModeInfo(Mode: TGraphicMode): TGraphicModeInfo;
begin
  Result := GRAPHICS_MODES[Mode];
end;

{ TPaletteManager }

constructor TPaletteManager.Create(PaletteType: TPaletteType);
begin
  FPaletteType := PaletteType;
  FModified := False;
  LoadDefaultPalette;
end;

procedure TPaletteManager.LoadDefaultPalette;
begin
  case FPaletteType of
    ptC64_16: LoadC64Palette;
    ptVGA_256: LoadVGAPalette;
    ptTrueColor: ; // Non serve palette per true color
  end;
  FModified := False;
end;

procedure TPaletteManager.LoadC64Palette;
var
  i, idx: Integer;
begin
  // C64 Colodore palette repeated for 256 colors
  for i := 0 to 255 do
  begin
    idx := i mod 16;
    FPalette[i] := (C64_PALETTE_COLODORE[idx].R shl 16) or
                   (C64_PALETTE_COLODORE[idx].G shl 8) or
                   C64_PALETTE_COLODORE[idx].B or
                   ($FF shl 24); // Alpha
  end;
end;

procedure TPaletteManager.LoadC64PaletteVICE;
var
  i, idx: Integer;
begin
  // C64 VICE/Ultimate64 palette repeated for 256 colors
  for i := 0 to 255 do
  begin
    idx := i mod 16;
    FPalette[i] := (C64_PALETTE_VICE[idx].R shl 16) or
                   (C64_PALETTE_VICE[idx].G shl 8) or
                   C64_PALETTE_VICE[idx].B or
                   ($FF shl 24); // Alpha
  end;
end;

procedure TPaletteManager.LoadPredefinedPalette(PaletteID: Integer);
begin
  case PaletteID of
    C64_PALETTE_TYPE_COLODORE: LoadC64Palette;
    C64_PALETTE_TYPE_VICE: LoadC64PaletteVICE;
  else
    LoadC64Palette; // Default
  end;
  FModified := False;
end;

function TPaletteManager.IndexToRGBA(Index: Byte): UInt32;
begin
  Result := FPalette[Index];
end;

procedure TPaletteManager.LoadVGAPalette;
var
  i, r, g, b: Integer;
begin
  // Standard VGA Mode 13h palette (256 colors)

  // 0-15: Standard EGA colors
  for i := 0 to 15 do
  begin
    case i of
      0: begin r := 0; g := 0; b := 0; end;       // Black
      1: begin r := 0; g := 0; b := 170; end;     // Blue
      2: begin r := 0; g := 170; b := 0; end;     // Green
      3: begin r := 0; g := 170; b := 170; end;   // Cyan
      4: begin r := 170; g := 0; b := 0; end;     // Red
      5: begin r := 170; g := 0; b := 170; end;   // Magenta
      6: begin r := 170; g := 85; b := 0; end;    // Brown
      7: begin r := 170; g := 170; b := 170; end; // Light Gray
      8: begin r := 85; g := 85; b := 85; end;    // Dark Gray
      9: begin r := 85; g := 85; b := 255; end;   // Light Blue
      10: begin r := 85; g := 255; b := 85; end;  // Light Green
      11: begin r := 85; g := 255; b := 255; end; // Light Cyan
      12: begin r := 255; g := 85; b := 85; end;  // Light Red
      13: begin r := 255; g := 85; b := 255; end; // Light Magenta
      14: begin r := 255; g := 255; b := 85; end; // Yellow
      15: begin r := 255; g := 255; b := 255; end;// White
      else begin r := 0; g := 0; b := 0; end;
    end;
    FPalette[i] := (r shl 16) or (g shl 8) or b or ($FF shl 24);
  end;

  // 16-31: Grayscale
  for i := 0 to 15 do
  begin
    g := i * 17; // 0, 17, 34, ..., 255
    FPalette[16 + i] := (g shl 16) or (g shl 8) or g or ($FF shl 24);
  end;

  // 32-247: 6x6x6 color cube (216 colors)
  for r := 0 to 5 do
    for g := 0 to 5 do
      for b := 0 to 5 do
      begin
        i := 32 + r * 36 + g * 6 + b;
        FPalette[i] := ((r * 51) shl 16) or ((g * 51) shl 8) or (b * 51) or ($FF shl 24);
      end;

  // 248-255: Additional grays
  for i := 248 to 255 do
  begin
    g := (i - 248) * 36;
    FPalette[i] := (g shl 16) or (g shl 8) or g or ($FF shl 24);
  end;
end;

procedure TPaletteManager.SetColor(Index: Integer; R, G, B: Byte);
begin
  if (Index >= 0) and (Index <= 255) then
  begin
    FPalette[Index] := (R shl 16) or (G shl 8) or B or ($FF shl 24);
    FModified := True;
  end;
end;

procedure TPaletteManager.SetColor(Index: Integer; RGB: UInt32);
begin
  if (Index >= 0) and (Index <= 255) then
  begin
    FPalette[Index] := RGB or ($FF shl 24);
    FModified := True;
  end;
end;

function TPaletteManager.GetColor(Index: Integer): UInt32;
begin
  if (Index >= 0) and (Index <= 255) then
    Result := FPalette[Index]
  else
    Result := 0;
end;

procedure TPaletteManager.FadeToBlack(Steps: Integer; CurrentStep: Integer);
var
  i: Integer;
  r, g, b: Byte;
  factor: Double;
begin
  if (Steps <= 0) or (CurrentStep < 0) or (CurrentStep > Steps) then Exit;

  factor := 1.0 - (CurrentStep / Steps);

  for i := 0 to 255 do
  begin
    r := (FPalette[i] shr 16) and $FF;
    g := (FPalette[i] shr 8) and $FF;
    b := FPalette[i] and $FF;

    r := Round(r * factor);
    g := Round(g * factor);
    b := Round(b * factor);

    FPalette[i] := (r shl 16) or (g shl 8) or b or ($FF shl 24);
  end;
  FModified := True;
end;

procedure TPaletteManager.FadeFromBlack(Steps: Integer; CurrentStep: Integer);
var
  i: Integer;
  r, g, b: Byte;
  factor: Double;
  OriginalPalette: array[0..255] of UInt32;
begin
  if (Steps <= 0) or (CurrentStep < 0) or (CurrentStep > Steps) then Exit;

  // First save original palette
  if CurrentStep = 0 then
  begin
    SavePalette(OriginalPalette);
  end;

  factor := CurrentStep / Steps;

  // Simplified implementation - requires original palette saved externally
  FadeToBlack(Steps, Steps - CurrentStep);
end;

procedure TPaletteManager.RotatePalette(StartIdx, EndIdx: Integer; Direction: Integer);
var
  i, count: Integer;
  temp: UInt32;
begin
  if (StartIdx < 0) or (EndIdx > 255) or (StartIdx >= EndIdx) then Exit;

  count := EndIdx - StartIdx + 1;

  if Direction > 0 then
  begin
    // Rotate forward
    temp := FPalette[EndIdx];
    for i := EndIdx downto StartIdx + 1 do
      FPalette[i] := FPalette[i - 1];
    FPalette[StartIdx] := temp;
  end
  else
  begin
    // Rotate backward
    temp := FPalette[StartIdx];
    for i := StartIdx to EndIdx - 1 do
      FPalette[i] := FPalette[i + 1];
    FPalette[EndIdx] := temp;
  end;

  FModified := True;
end;

procedure TPaletteManager.SavePalette(var Buffer: TPaletteArray);
var
  i: Integer;
begin
  for i := 0 to 255 do
    Buffer[i] := FPalette[i];
end;

procedure TPaletteManager.RestorePalette(const Buffer: TPaletteArray);
var
  i: Integer;
begin
  for i := 0 to 255 do
    FPalette[i] := Buffer[i];
  FModified := True;
end;

function TPaletteManager.GetPalettePtr: PPaletteArray;
begin
  Result := @FPalette;
end;

{ TViewportCalculator }

constructor TViewportCalculator.Create(MonitorWidth, MonitorHeight: Integer);
begin
  FMonitorWidth := MonitorWidth;
  FMonitorHeight := MonitorHeight;
  if MonitorHeight > 0 then
    FMonitorAspectRatio := MonitorWidth / MonitorHeight
  else
    FMonitorAspectRatio := 1.333; // 4:3 default
end;

function TViewportCalculator.GetNearestMultipleOf8(Value: Integer): Integer;
begin
  Result := (Value div 8) * 8;
  if Result < 8 then Result := 8;
  if Result > 64 then Result := 64;
end;

function TViewportCalculator.CalculateFontSize(NativeWidth, NativeHeight,
                                              TextCols, TextRows: Integer): Integer;
var
  CharWidth, CharHeight: Integer;
  ScaleFactor: Integer;
  TargetFontSize: Integer;
begin
  if (TextCols = 0) or (TextRows = 0) then
  begin
    Result := 0;
    Exit;
  end;

  // Calculate native character size (e.g. 640x400 / 80x50 = 8x8)
  CharWidth := NativeWidth div TextCols;
  CharHeight := NativeHeight div TextRows;

  // Calculate integer scale factor
  ScaleFactor := Min(FMonitorWidth div NativeWidth,
                     FMonitorHeight div NativeHeight);
  if ScaleFactor < 1 then ScaleFactor := 1;

  // The font must be CharHeight * ScaleFactor
  TargetFontSize := CharHeight * ScaleFactor;

  // Ensure multiples of 8
  Result := (TargetFontSize div 8) * 8;
  if Result < 8 then Result := 8;

  // For 640x400 on 1920x1080 monitor:
  // ScaleFactor = Min(1920/640, 1080/400) = Min(3, 2.7) = 2
  // TargetFontSize = 8 * 2 = 16
  // Result = 16 (already multiple of 8)
end;

function TViewportCalculator.CalculateViewport(const ModeInfo: TGraphicModeInfo;
                                              UseMaxResolution: Boolean;
                                              out ViewportWidth, ViewportHeight: Integer;
                                              out WindowWidth, WindowHeight: Integer;
                                              out FontSize: Integer): Boolean;
var
  ScaleFactor: Integer;
begin
  Result := False;

  ScaleFactor := Min(FMonitorWidth div ModeInfo.NativeWidth,
                     FMonitorHeight div ModeInfo.NativeHeight);

  if ScaleFactor < 1 then
  begin
    ViewportWidth := ModeInfo.NativeWidth;
    ViewportHeight := ModeInfo.NativeHeight;
    WindowWidth := ModeInfo.NativeWidth;
    WindowHeight := ModeInfo.NativeHeight;
    FontSize := 8;
    Exit;
  end;

  ViewportWidth := ModeInfo.NativeWidth * ScaleFactor;
  ViewportHeight := ModeInfo.NativeHeight * ScaleFactor;

  if UseMaxResolution then
  begin
    WindowWidth := FMonitorWidth;
    WindowHeight := FMonitorHeight;
  end
  else
  begin
    if (ViewportWidth / ViewportHeight) > FMonitorAspectRatio then
    begin
      WindowWidth := ViewportWidth;
      WindowHeight := Round(ViewportWidth / FMonitorAspectRatio);
    end
    else
    begin
      WindowHeight := ViewportHeight;
      WindowWidth := Round(ViewportHeight * FMonitorAspectRatio);
    end;
  end;

  if ModeInfo.ModeType in [mtText, mtMixed] then
  begin
    FontSize := CalculateFontSize(ModeInfo.NativeWidth, ModeInfo.NativeHeight,
                                  ModeInfo.TextCols, ModeInfo.TextRows);

    if FontSize > 0 then
    begin
      ViewportWidth := ModeInfo.TextCols * FontSize;
      ViewportHeight := ModeInfo.TextRows * FontSize;
    end;
  end
  else
    FontSize := 0;

  Result := True;
end;

function TViewportCalculator.FindBestTextMode(PreferHighRes: Boolean): TGraphicMode;
var
  Mode: TGraphicMode;
  ModeInfo: TGraphicModeInfo;
  BestMode: TGraphicMode;
  BestScore: Integer;
  Score: Integer;
  ViewportW, ViewportH, WindowW, WindowH, FontSize: Integer;
begin
  BestMode := gm80x50Text;
  BestScore := 0;

  for Mode := Low(TGraphicMode) to High(TGraphicMode) do
  begin
    ModeInfo := GRAPHICS_MODES[Mode];

    if ModeInfo.ModeType <> mtText then Continue;

    if CalculateViewport(ModeInfo, True, ViewportW, ViewportH,
                        WindowW, WindowH, FontSize) then
    begin
      if PreferHighRes then
        Score := ModeInfo.TextCols * ModeInfo.TextRows
      else
        Score := 10000 - Abs(80*50 - ModeInfo.TextCols * ModeInfo.TextRows);

      if Mode = gm80x50Text then
        Score := Score + 1000;

      if Score > BestScore then
      begin
        BestScore := Score;
        BestMode := Mode;
      end;
    end;
  end;

  Result := BestMode;
end;

function TViewportCalculator.FindBestModeForResolution(TargetWidth, TargetHeight: Integer): TGraphicMode;
var
  Mode: TGraphicMode;
  ModeInfo: TGraphicModeInfo;
  BestMode: TGraphicMode;
  BestDiff: Integer;
  Diff: Integer;
begin
  BestMode := gmSDL2Dynamic;
  BestDiff := MaxInt;

  for Mode := Low(TGraphicMode) to High(TGraphicMode) do
  begin
    if Mode = gmSDL2Dynamic then Continue;

    ModeInfo := GRAPHICS_MODES[Mode];
    Diff := Abs(ModeInfo.NativeWidth - TargetWidth) +
            Abs(ModeInfo.NativeHeight - TargetHeight);

    if Diff < BestDiff then
    begin
      BestDiff := Diff;
      BestMode := Mode;
    end;
  end;

  Result := BestMode;
end;

end.
