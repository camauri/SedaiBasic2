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
unit SedaiSDL2Output;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Math, StrUtils, ctypes,
  SDL2, SDL2_ttf,
  SedaiOutputInterface, SedaiGraphicsTypes;

type
  // Struttura per una riga nel buffer di scroll
  TScrollLine = record
    Chars: array of record
      Character: string;
      FgColor: TColor;
      BgColor: TColor;
    end;
    Length: Integer;
  end;

  { TSDL2OutputDevice - CON SCROLLING AGGIUNTO ALLA TUA IMPLEMENTAZIONE }
  { NOTE: Using TObject instead of TInterfacedObject because interfaces use CORBA mode (no refcount) }
  TSDL2OutputDevice = class(TObject, IOutputDevice)
  private
    FWindow: PSDL_Window;
    FFont: PTTF_Font;
    FInitialized: Boolean;
    FQuitRequested: Boolean;
    FFullscreen: Boolean;

    // Display properties - IMPOSTATE DALL'ESTERNO VIA SetDisplayParameters
    FWindowWidth, FWindowHeight: Integer;
    FDesiredCols, FDesiredRows: Integer;
    FCharWidth, FCharHeight: Integer;
    FFontSize: Integer;
    FGridOffsetX, FGridOffsetY: Integer;

    // Cursor
    FCursorX, FCursorY: Integer;
    FCursorVisible: Boolean;
    FCursorSavedChar: string;
    FCursorSavedFg: TColor;
    FCursorSavedBg: TColor;
    FCursorSavedValid: Boolean;

    // Colors
    FForegroundColor: TColor;
    FBackgroundColor: TColor;
    FDefaultFgColor, FDefaultBgColor: TColor;

    // Video buffer originale (TUA IMPLEMENTAZIONE)
    FVideoBuffer: array of array of record
      Character: string;
      FgColor: TColor;
      BgColor: TColor;
      HasChar: Boolean;
      CustomColors: Boolean;
    end;

    // *** SISTEMA DI SCROLLING AGGIUNTO ***
    FScrollBuffer: array of TScrollLine;
    FScrollBufferSize: Integer;
    FScrollBufferCount: Integer;
    FScrollBufferStart: Integer;
    FScrollOffset: Integer;
    FPromptRow: Integer;
    FInScrollMode: Boolean;

    // === OTTIMIZZAZIONI AGGIUNTE ===

    // Character texture cache per evitare creazione ripetuta
    FCharTextureCache: array[0..255] of PSDL_Texture;
    FCharCacheKeys: array[0..255] of string;        // ← CHIAVI UTF-8!
    FCharCacheLastUsed: array[0..255] of QWord;     // ← LRU
    FCharCacheFg, FCharCacheBg: array[0..255] of TColor;
    FCharCacheValid: array[0..255] of Boolean;

    // Dirty tracking per rendering selettivo
    FCellDirty: array of array of Boolean;
    FDirtyMinX, FDirtyMinY, FDirtyMaxX, FDirtyMaxY: Integer;
    FHasDirtyRegion: Boolean;

    // Output buffering
    FPrintBuffer: string;
    FBufferSize: Integer;
    FMaxBufferSize: Integer;

    // Background texture per evitare clear completi
    FBackgroundTexture: PSDL_Texture;
    FBackgroundDirty: Boolean;

    // Performance monitoring
    FRenderCalls: Integer;
    FLastRenderTime: QWord;

    FLogicalWidth, FLogicalHeight: Integer;
    FCalculatedFontSize: Integer;

    // FAST mode (C128 2MHz emulation - black overlay)
    FFastMode: Boolean;
    FFastModeAlpha: Byte;

    // Internal methods (TUA IMPLEMENTAZIONE ORIGINALE)
    procedure DrawCharacterAtDirect(X, Y: Integer; const Ch: string; FgColor,
      BgColor: TColor);
    procedure InitializeSDL;
    procedure RedrawScreen;
    procedure SetupRenderer;
    procedure LoadFontWithSize(Size: Integer);
    procedure ShowScrollBuffer;
    function TryLoadFont(const FontPath: string; Size: Integer): Boolean;
    procedure CalculateCharacterDimensions;
    procedure InitializeVideoBuffer;
    procedure HandleQuitEvents;

    procedure DrawCharacterAt(X, Y: Integer; const Ch: string; FgColor, BgColor: TColor);
    procedure RenderTextFast(const Text: string; StartX, StartY: Integer; ClearBackground: Boolean = False);
    procedure SaveCursorBackground(X, Y: Integer);
    procedure RestoreCursorBackground(X, Y: Integer);
    procedure RenderOverlay;

    // *** NUOVI METODI PER SCROLLING ***
    procedure InitializeScrollBuffer;
    procedure AddLineToScrollBuffer(LineIndex: Integer);
    procedure ScrollUp(Lines: Integer = 1);
    procedure ScrollDown(Lines: Integer = 1);
    procedure ScrollToBottom;
    procedure RebuildDisplayFromBuffer;
    procedure MarkPromptRow;
    function IsAtBottom: Boolean;

    // === METODI OTTIMIZZAZIONE AGGIUNTI ===
    procedure InitializeOptimizedStructures;
    procedure ClearCharacterCache;
    function GetCachedCharacterTexture(const Ch: string; FgColor, BgColor: TColor): PSDL_Texture;
    procedure MarkCellDirty(X, Y: Integer);
    procedure ClearDirtyTracking;
    procedure RenderDirtyRegionsOnly;
    procedure FlushPrintBuffer;
    procedure CreateBackgroundTexture;

  protected
    FRenderer: PSDL_Renderer;
    FWantFullscreen: Boolean;
    // viewport
    FContentViewport: TSDL_Rect;
    FBorderColor: TColor;
    FUseCenteredViewport: Boolean;

    // === GRAPHICS STYLE STATE ===
    FCurrentBorderStyle: TBorderStyle;
    FCurrentFillStyle: TFillStyleDef;

  public
    constructor Create;
    destructor Destroy; override;

    // *** MAIN METHOD FOR CONFIGURATION ***
    procedure SetDisplayParameters(WindowW, WindowH, Cols, Rows, LogicalW, LogicalH: Integer);

    // IOutputDevice implementation
    function Initialize(const Title: string = ''; Width: Integer = 80; Height: Integer = 25): Boolean;
    procedure Shutdown;
    function IsInitialized: Boolean;

    // Base output
    procedure Print(const Text: string; ClearBackground: Boolean = False);
    procedure PrintLn(const Text: string; ClearBackground: Boolean = False);
    procedure NewLine;
    procedure Clear; virtual;

    // Cursor/position control
    procedure SetCursor(X, Y: Integer);
    procedure MoveCursor(DeltaX, DeltaY: Integer);
    function GetCursorX: Integer;
    function GetCursorY: Integer;

    // Cursor with color inversion
    procedure ShowCursor(X, Y: Integer);
    procedure HideCursor(X, Y: Integer);

    // Colors
    procedure SetColors(Foreground, Background: TColor);
    procedure SetCellColor(X, Y: Integer; Fg, Bg: TColor);
    procedure SetDefaultColors(Fg, Bg: TColor);

    // Aggiornamento display
    procedure Present; virtual;

    // Controlli specifici
    procedure SetFullscreen(Enabled: Boolean);
    function IsFullscreen: Boolean;
    function ShouldQuit: Boolean;

    // *** NUOVI METODI PER SCROLLING ***
    procedure ProcessScrollInput;
    procedure OnUserInput;
    function HandleScrollKeys(Key: Integer; Modifiers: Integer): Boolean;

    function GetInScrollMode: Boolean;

    function SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean = False;
                           SplitLine: Integer = -1): Boolean;
    function GetGraphicMode: TGraphicMode; virtual;
    function IsInGraphicsMode: Boolean; virtual;
    procedure SetPixel(X, Y: Integer; RGB: UInt32); virtual; overload;
    procedure SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex); virtual; overload;
    function GetPixel(X, Y: Integer): UInt32; virtual;
    procedure EnablePalette(Enable: Boolean); virtual;
    function IsPaletteEnabled: Boolean; virtual;
    procedure SetPaletteColor(Index: TPaletteIndex; RGB: UInt32); virtual;
    procedure SetPaletteColorRGBA(Index: TPaletteIndex; R, G, B: Byte; A: Byte = 255); virtual;
    function GetPaletteColor(Index: TPaletteIndex): UInt32; virtual;
    procedure ResetPalette; virtual;

    // Palette file operations (JSON format)
    function LoadPaletteFromJSON(const FileName: string): Boolean; virtual;
    function SavePaletteToJSON(const FileName: string): Boolean; virtual;
    function GetLastPaletteError: string; virtual;

    // Shape drawing (IOutputDevice interface)
    procedure DrawBox(X1, Y1, X2, Y2: Integer; Angle: Double = 0); virtual;
    procedure DrawBoxStyled(X1, Y1, X2, Y2: Integer; const Style: TShapeStyle; Angle: Double = 0); virtual;
    procedure DrawBoxWithColor(X1, Y1, X2, Y2: Integer; Color: UInt32; Angle: Double = 0; Filled: Boolean = False); virtual;
    procedure DrawCircleWithColor(X, Y, XR, YR: Integer; Color: UInt32;
                                  SA: Double = 0; EA: Double = 360;
                                  Angle: Double = 0; Inc: Double = 2); virtual;
    procedure SetBorderStyle(const Style: TBorderStyle); virtual;
    procedure SetFillStyle(const Style: TFillStyleDef); virtual;
    function GetBorderStyle: TBorderStyle; virtual;
    function GetFillStyle: TFillStyleDef; virtual;

    // FAST mode (C128 2MHz emulation - black overlay)
    procedure SetFastMode(Enabled: Boolean); virtual;
    function GetFastMode: Boolean; virtual;
    procedure SetFastModeAlpha(Alpha: Byte); virtual;
    function GetFastModeAlpha: Byte; virtual;

    // Properties
    function GetActualCols: Integer;
    function GetActualRows: Integer;
    property CharWidth: Integer read FCharWidth;
    property CharHeight: Integer read FCharHeight;
    property FontSize: Integer read FFontSize write FFontSize;
    property InScrollMode: Boolean read FInScrollMode;
    property ScrollBufferSize: Integer read FScrollBufferSize write FScrollBufferSize;

    property DesiredCols: Integer read FDesiredCols write FDesiredCols;
    property DesiredRows: Integer read FDesiredRows write FDesiredRows;
    property CalculatedFontSize: Integer read FCalculatedFontSize write FCalculatedFontSize;
    property LogicalWidth: Integer read FLogicalWidth write FLogicalWidth;
    property LogicalHeight: Integer read FLogicalHeight write FLogicalHeight;

    procedure SetCenteredViewport(UseViewport: Boolean; ViewportRect: TSDL_Rect; BorderColor: TColor);
  end;

// Helper function to center text for UTF-8
function UTF8CenterText(const Text: string; Width: Integer): string;

implementation

{ Helper functions UTF-8 }
function UTF8CenterText(const Text: string; Width: Integer): string;
var
  PaddingTotal, PaddingLeft, PaddingRight: Integer;
  TextLen: Integer;
begin
  TextLen := Length(Text);
  if TextLen >= Width then
  begin
    Result := Copy(Text, 1, Width);
    Exit;
  end;

  PaddingTotal := Width - TextLen;
  PaddingLeft := PaddingTotal div 2;
  PaddingRight := PaddingTotal - PaddingLeft;

  Result := StringOfChar(' ', PaddingLeft) + Text + StringOfChar(' ', PaddingRight);
end;

{ TSDL2OutputDevice }

constructor TSDL2OutputDevice.Create;
begin
  inherited Create;
  FWindow := nil;
  FRenderer := nil;
  FFont := nil;
  FInitialized := False;
  FQuitRequested := False;
  FFullscreen := False;

  // Default colors (green on gray, classic CRT style)
  FForegroundColor.R := 0; FForegroundColor.G := 255; FForegroundColor.B := 0;
  FBackgroundColor.R := 128; FBackgroundColor.G := 128; FBackgroundColor.B := 128;
  FDefaultFgColor := FForegroundColor;
  FDefaultBgColor := FBackgroundColor;
  FGridOffsetX := 0;
  FGridOffsetY := 0;

  FCursorX := 0;
  FCursorY := 0;
  FCursorVisible := False;
  FCursorSavedValid := False;

  // *** REMOVED HARDCODED SETTINGS ***
  // Values will be set by SetDisplayParameters
  FFontSize := 12;  // Default, will be recalculated
  FWindowWidth := 0;   // Will be set externally
  FWindowHeight := 0;  // Will be set externally
  FDesiredCols := 0;   // Will be set externally
  FDesiredRows := 0;   // Will be set externally

  // *** SCROLLING INITIALIZATION ***
  FScrollBufferSize := 1000;
  FScrollBufferCount := 0;
  FScrollBufferStart := 0;
  FScrollOffset := 0;
  FPromptRow := 0;
  FInScrollMode := False;

  // === OPTIMIZATION INITIALIZATION ===
  InitializeOptimizedStructures;

  // === GRAPHICS STYLE INITIALIZATION ===
  FCurrentBorderStyle := MakeDefaultBorderStyle(clWhite, 1);
  FCurrentFillStyle := MakeDefaultFillStyle(clTransparent);
  FCurrentFillStyle.Style := fsNone;

  // === FAST MODE INITIALIZATION ===
  FFastMode := False;
  FFastModeAlpha := 255;  // Fully opaque by default (as per user request)
end;

destructor TSDL2OutputDevice.Destroy;
begin
  // Shutdown handles all cleanup (textures, font, renderer, window, SDL)
  // Don't call ClearCharacterCache or destroy FBackgroundTexture here
  // as Shutdown already does that
  Shutdown;
  inherited Destroy;
end;

// === IMPLEMENTAZIONE OTTIMIZZAZIONI ===

procedure TSDL2OutputDevice.InitializeOptimizedStructures;
var
  i: Integer;
begin
  // Initialize UTF-8 character cache
  for i := 0 to 255 do
  begin
    FCharTextureCache[i] := nil;
    FCharCacheKeys[i] := '';
    FCharCacheLastUsed[i] := 0;
    FCharCacheValid[i] := False;
  end;

  // Initialize dirty tracking
  ClearDirtyTracking;

  // Initialize print buffer
  FPrintBuffer := '';
  FBufferSize := 0;
  FMaxBufferSize := 4096;

  // Initialize background texture
  FBackgroundTexture := nil;
  FBackgroundDirty := True;

  // Initialize performance monitoring
  FRenderCalls := 0;
  FLastRenderTime := GetTickCount64;
end;

procedure TSDL2OutputDevice.ClearCharacterCache;
var
  i: Integer;
begin
  for i := 0 to 255 do
  begin
    if Assigned(FCharTextureCache[i]) then
    begin
      SDL_DestroyTexture(FCharTextureCache[i]);
      FCharTextureCache[i] := nil;
    end;
    FCharCacheValid[i] := False;
  end;
end;

function TSDL2OutputDevice.GetCachedCharacterTexture(const Ch: string; FgColor, BgColor: TColor): PSDL_Texture;
var
  i, OldestIndex: Integer;
  OldestTime: QWord;
  Surface: PSDL_Surface;
  Color: TSDL_Color;
  CacheKey: string;
  CurrentTime: QWord;
  Found: Boolean;
begin
  Result := nil;
  if not Assigned(FFont) or (Ch = '') then Exit;

  CurrentTime := GetTickCount64;

  // === CACHE KEY UTF-8 COMPATIBILE ===
  CacheKey := Ch + '|' + IntToStr(FgColor.R) + '|' + IntToStr(FgColor.G) + '|' + IntToStr(FgColor.B);

  // === CERCA NELLA CACHE ===
  Found := False;
  for i := 0 to 255 do
  begin
    if FCharCacheValid[i] and (FCharCacheKeys[i] = CacheKey) then
    begin
      FCharCacheLastUsed[i] := CurrentTime;
      Result := FCharTextureCache[i];
      Found := True;
      Exit;
    end;
  end;

  if Found then Exit;

  // === FIND FREE OR OLDEST SLOT ===
  OldestIndex := 0;
  OldestTime := CurrentTime;

  for i := 0 to 255 do
  begin
    if not FCharCacheValid[i] then
    begin
      OldestIndex := i;
      Break;
    end;
    if FCharCacheLastUsed[i] < OldestTime then
    begin
      OldestTime := FCharCacheLastUsed[i];
      OldestIndex := i;
    end;
  end;

  // === PULISCI SLOT SE OCCUPATO ===
  if FCharCacheValid[OldestIndex] and Assigned(FCharTextureCache[OldestIndex]) then
  begin
    SDL_DestroyTexture(FCharTextureCache[OldestIndex]);
    FCharTextureCache[OldestIndex] := nil;
  end;

  // === CREA NUOVA TEXTURE (UTF-8 COMPATIBILE!) ===
  Color.r := FgColor.R;
  Color.g := FgColor.G;
  Color.b := FgColor.B;
  Color.a := 255;

  Surface := TTF_RenderUTF8_Blended(FFont, PChar(Ch), Color);
  if Assigned(Surface) then
  begin
    try
      FCharTextureCache[OldestIndex] := SDL_CreateTextureFromSurface(FRenderer, Surface);
      if Assigned(FCharTextureCache[OldestIndex]) then
      begin
        FCharCacheKeys[OldestIndex] := CacheKey;  // ← CHIAVE UTF-8!
        FCharCacheLastUsed[OldestIndex] := CurrentTime;
        FCharCacheValid[OldestIndex] := True;
        Result := FCharTextureCache[OldestIndex];
      end;
    finally
      SDL_FreeSurface(Surface);
    end;
  end;
end;

procedure TSDL2OutputDevice.MarkCellDirty(X, Y: Integer);
begin
  if (X < 0) or (Y < 0) or (X >= FDesiredCols) or (Y >= FDesiredRows) then Exit;

  if Length(FCellDirty) <> FDesiredCols then
  begin
    SetLength(FCellDirty, FDesiredCols, FDesiredRows);
  end;

  FCellDirty[X][Y] := True;

  if not FHasDirtyRegion then
  begin
    FDirtyMinX := X;
    FDirtyMaxX := X;
    FDirtyMinY := Y;
    FDirtyMaxY := Y;
    FHasDirtyRegion := True;
  end
  else
  begin
    if X < FDirtyMinX then FDirtyMinX := X;
    if X > FDirtyMaxX then FDirtyMaxX := X;
    if Y < FDirtyMinY then FDirtyMinY := Y;
    if Y > FDirtyMaxY then FDirtyMaxY := Y;
  end;
end;

procedure TSDL2OutputDevice.ClearDirtyTracking;
var
  x, y: Integer;
begin
  if Length(FCellDirty) > 0 then
  begin
    for x := 0 to Length(FCellDirty) - 1 do
      for y := 0 to Length(FCellDirty[0]) - 1 do
        FCellDirty[x][y] := False;
  end;

  FHasDirtyRegion := False;
  FDirtyMinX := MaxInt;
  FDirtyMaxX := -1;
  FDirtyMinY := MaxInt;
  FDirtyMaxY := -1;
end;

procedure TSDL2OutputDevice.RenderDirtyRegionsOnly;
var
  x, y: Integer;
  DstRect: TSDL_Rect;
  CachedTexture: PSDL_Texture;
begin
  if not FHasDirtyRegion then Exit;

  Inc(FRenderCalls);

  for y := FDirtyMinY to FDirtyMaxY do
  begin
    for x := FDirtyMinX to FDirtyMaxX do
    begin
      if FCellDirty[x][y] then
      begin
        DstRect.x := FGridOffsetX + (x * FCharWidth);
        DstRect.y := FGridOffsetY + (y * FCharHeight);
        DstRect.w := FCharWidth;
        DstRect.h := FCharHeight;

        // Background
        SDL_SetRenderDrawColor(FRenderer, FVideoBuffer[x][y].BgColor.R,
                              FVideoBuffer[x][y].BgColor.G, FVideoBuffer[x][y].BgColor.B, 255);
        SDL_RenderFillRect(FRenderer, @DstRect);

        // Character con cache
        if FVideoBuffer[x][y].Character <> ' ' then
        begin
          CachedTexture := GetCachedCharacterTexture(FVideoBuffer[x][y].Character,
                                                    FVideoBuffer[x][y].FgColor,
                                                    FVideoBuffer[x][y].BgColor);
          if Assigned(CachedTexture) then
            SDL_RenderCopy(FRenderer, CachedTexture, nil, @DstRect);
        end;
      end;
    end;
  end;

  ClearDirtyTracking;
end;

procedure TSDL2OutputDevice.FlushPrintBuffer;
begin
  if FBufferSize > 0 then
  begin
    // The buffer is already processed character by character in Print
    FPrintBuffer := '';
    FBufferSize := 0;
  end;
end;

procedure TSDL2OutputDevice.CreateBackgroundTexture;
begin
  if not Assigned(FBackgroundTexture) then
  begin
    FBackgroundTexture := SDL_CreateTexture(FRenderer, SDL_PIXELFORMAT_RGBA8888,
                                           SDL_TEXTUREACCESS_TARGET,
                                           FWindowWidth, FWindowHeight);
  end;
end;

procedure TSDL2OutputDevice.InitializeSDL;
begin
  if SDL_Init(SDL_INIT_VIDEO) < 0 then
    raise Exception.Create('Failed to initialize SDL2: ' + SDL_GetError);

  if TTF_Init < 0 then
    raise Exception.Create('Failed to initialize SDL2_ttf: ' + TTF_GetError);
end;

procedure TSDL2OutputDevice.SetupRenderer;
begin
  SDL_SetRenderDrawBlendMode(FRenderer, SDL_BLENDMODE_BLEND);
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '1');
  SDL_SetHint(SDL_HINT_VIDEO_ALLOW_SCREENSAVER, '0');
  SDL_SetHint(SDL_HINT_RENDER_VSYNC, '1');
end;

procedure TSDL2OutputDevice.SetDisplayParameters(WindowW, WindowH, Cols, Rows, LogicalW, LogicalH: Integer);
begin
  FWindowWidth := WindowW;
  FWindowHeight := WindowH;
  FDesiredCols := Cols;
  FDesiredRows := Rows;

  // Use the logical dimensions passed as parameter (native to the mode)
  FLogicalWidth := LogicalW;
  FLogicalHeight := LogicalH;

  // Calculate font size based on logical size - NOT the window
  FCalculatedFontSize := FLogicalWidth div FDesiredCols;

  // Verify that it's square
  if (FLogicalHeight div FDesiredRows) <> FCalculatedFontSize then
  begin
    WriteLn('WARNING: Non-square font! W=', FCalculatedFontSize,
            ' H=', FLogicalHeight div FDesiredRows);
    // Use the smallest to keep everything in the viewport
    if (FLogicalHeight div FDesiredRows) < FCalculatedFontSize then
      FCalculatedFontSize := FLogicalHeight div FDesiredRows;
  end;

  FFontSize := FCalculatedFontSize;

  WriteLn('Display parameters set:');
  WriteLn('  Window: ', WindowW, 'x', WindowH);
  WriteLn('  Logical: ', FLogicalWidth, 'x', FLogicalHeight);
  WriteLn('  Grid: ', Cols, 'x', Rows);
  WriteLn('  Font size: ', FCalculatedFontSize);
end;

function TSDL2OutputDevice.Initialize(const Title: string; Width: Integer; Height: Integer): Boolean;
var
  WindowTitle: PChar;
  WindowFlags: UInt32;
begin
  Result := False;
  try
    InitializeSDL;

    // Dimensions have already been set by SetDisplayParameters
    WindowTitle := PChar(Title);

    // Determine window flags
    WindowFlags := SDL_WINDOW_SHOWN;
    if FWantFullscreen then
      WindowFlags := WindowFlags or SDL_WINDOW_FULLSCREEN_DESKTOP;

    FWindow := SDL_CreateWindow(
      WindowTitle,
      SDL_WINDOWPOS_CENTERED,
      SDL_WINDOWPOS_CENTERED,
      FWindowWidth, FWindowHeight,
      WindowFlags
    );

    if FWindow = nil then
      raise Exception.Create('Failed to create window: ' + SDL_GetError);

    FRenderer := SDL_CreateRenderer(FWindow, -1,
      SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);

    if FRenderer = nil then
      raise Exception.Create('Failed to create renderer: ' + SDL_GetError);

    SetupRenderer;

    // === IMPOSTA LOGICAL SIZE E CENTRA VIEWPORT ===
    SDL_RenderSetLogicalSize(FRenderer, FLogicalWidth, FLogicalHeight);
    WriteLn('Logical size set to: ', FLogicalWidth, 'x', FLogicalHeight);

    // Reset viewport per forzare centering automatico
    SDL_RenderSetViewport(FRenderer, nil);
    WriteLn('Viewport reset - SDL auto-centers logical size');

    // === CARICA FONT UNA SOLA VOLTA CON SIZE CALCOLATO ===
    LoadFontWithSize(FCalculatedFontSize);
    WriteLn('Font loaded with size: ', FCalculatedFontSize, 'pt');

    if Assigned(FFont) then
    begin
      TTF_SizeUTF8(FFont, 'W', @FCharWidth, @FCharHeight);
      WriteLn('Actual character size: ', FCharWidth, 'x', FCharHeight, ' px');

      // Verifica che il carattere entri nella cella logica
      if (FCharWidth > FCalculatedFontSize) or (FCharHeight > FCalculatedFontSize) then
        WriteLn('WARNING: Character larger than calculated font size!');
    end
    else
    begin
      // Usa le dimensioni calcolate come fallback
      FCharWidth := FCalculatedFontSize;
      FCharHeight := FCalculatedFontSize;
      WriteLn('Font not loaded - using calculated size: ', FCharWidth, 'x', FCharHeight);
    end;

    WriteLn('Text grid: ', FDesiredCols, 'x', FDesiredRows);
    WriteLn('Logical area: ', FDesiredCols * FCharWidth, 'x', FDesiredRows * FCharHeight, ' px');
    WriteLn('Available logical space: ', FLogicalWidth, 'x', FLogicalHeight, ' px');

    // Inizializza buffer con le dimensioni corrette
    if (FDesiredCols <= 0) or (FDesiredRows <= 0) then
    begin
      WriteLn('ERROR: Invalid grid dimensions: ', FDesiredCols, 'x', FDesiredRows);
      Exit;
    end;

    InitializeVideoBuffer;

    if FDesiredCols > 0 then
    begin
      InitializeScrollBuffer;
    end
    else
    begin
      WriteLn('ERROR: Cannot initialize scroll buffer - FDesiredCols is ', FDesiredCols);
      Exit;
    end;

    // Inizializza strutture ottimizzate
    SetLength(FCellDirty, FDesiredCols, FDesiredRows);
    ClearDirtyTracking;
    CreateBackgroundTexture;

    FInitialized := True;
    Result := True;

  except
    on E: Exception do
    begin
      WriteLn('Initialization error: ', E.Message);
      Shutdown;
    end;
  end;
end;

//function TSDL2OutputDevice.Initialize(const Title: string; Width: Integer; Height: Integer): Boolean;
//var
//  WindowTitle: PChar;
//begin
//  Result := False;
//  try
//    InitializeSDL;
//
//    if (Width <> 80) or (Height <> 25) then
//    begin
//      FWindowWidth := Width;
//      FWindowHeight := Height;
//    end;
//
//    WindowTitle := PChar(Title);
//
//    FWindow := SDL_CreateWindow(
//      WindowTitle,
//      SDL_WINDOWPOS_CENTERED,
//      SDL_WINDOWPOS_CENTERED,
//      FWindowWidth, FWindowHeight,
//      SDL_WINDOW_SHOWN
//    );
//
//    if FWindow = nil then
//      raise Exception.Create('Failed to create window: ' + SDL_GetError);
//
//    FRenderer := SDL_CreateRenderer(FWindow, -1,
//      SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
//
//    if FRenderer = nil then
//      raise Exception.Create('Failed to create renderer: ' + SDL_GetError);
//
//    SetupRenderer;
//    LoadFontWithSize(FFontSize);
//
//    WriteLn('Window: ', FWindowWidth, 'x', FWindowHeight);
//    WriteLn('Font size: ', FFontSize, 'pt');
//
//    if Assigned(FFont) then
//    begin
//      TTF_SizeUTF8(FFont, 'W', @FCharWidth, @FCharHeight);
//      WriteLn('Character size: ', FCharWidth, 'x', FCharHeight);
//    end
//    else
//    begin
//      FCharWidth := FFontSize div 2;
//      FCharHeight := FFontSize;
//      WriteLn('Using estimated character size: ', FCharWidth, 'x', FCharHeight);
//    end;
//
//    // *** VERIFICA CHE I VALORI SIANO VALIDI PRIMA DI CONTINUARE ***
//    if (FDesiredCols <= 0) or (FDesiredRows <= 0) then
//    begin
//      WriteLn('ERROR: Invalid grid dimensions: ', FDesiredCols, 'x', FDesiredRows);
//      Exit;
//    end;
//
//    // *** CALCOLA OFFSET CON I VALORI CORRENTI ***
//    FGridOffsetX := (FWindowWidth - (FDesiredCols * FCharWidth)) div 2;
//    FGridOffsetY := (FWindowHeight - (FDesiredRows * FCharHeight)) div 2;
//
//    WriteLn('Grid: ', FDesiredCols, 'x', FDesiredRows);
//    WriteLn('Grid offset: ', FGridOffsetX, ', ', FGridOffsetY);
//
//    // *** INIZIALIZZA I BUFFER NELL'ORDINE CORRETTO ***
//    InitializeVideoBuffer;
//
//    // *** VERIFICA NUOVAMENTE PRIMA DI INIZIALIZZARE SCROLL BUFFER ***
//    if FDesiredCols > 0 then
//    begin
//      InitializeScrollBuffer;
//    end
//    else
//    begin
//      WriteLn('ERROR: Cannot initialize scroll buffer - FDesiredCols is ', FDesiredCols);
//      Exit;
//    end;
//
//    // === INIZIALIZZA STRUTTURE OTTIMIZZATE ===
//    SetLength(FCellDirty, FDesiredCols, FDesiredRows);
//    ClearDirtyTracking;
//    CreateBackgroundTexture;
//
//    FInitialized := True;
//    Result := True;
//
//  except
//    on E: Exception do
//    begin
//      WriteLn('Initialization error: ', E.Message);
//      Shutdown;
//    end;
//  end;
//end;

procedure TSDL2OutputDevice.InitializeVideoBuffer;
var
  x, y: Integer;
begin
  SetLength(FVideoBuffer, FDesiredCols, FDesiredRows);

  for x := 0 to FDesiredCols - 1 do
    for y := 0 to FDesiredRows - 1 do
    begin
      FVideoBuffer[x][y].Character := ' ';
      FVideoBuffer[x][y].FgColor := FDefaultFgColor;
      FVideoBuffer[x][y].BgColor := FDefaultBgColor;
      FVideoBuffer[x][y].HasChar := False;
      FVideoBuffer[x][y].CustomColors := False;
    end;
end;

// *** METODI SCROLLING ***

procedure TSDL2OutputDevice.InitializeScrollBuffer;
var
  i, j: Integer;
begin
  // *** PARAMETER VALIDITY CHECK ***
  if FDesiredCols <= 0 then
  begin
    WriteLn('ERROR: FDesiredCols not set properly: ', FDesiredCols);
    Exit;
  end;

  if FScrollBufferSize <= 0 then
  begin
    WriteLn('ERROR: FScrollBufferSize not set properly: ', FScrollBufferSize);
    FScrollBufferSize := 1000; // Default fallback
  end;

  // *** LIBERA MEMORIA PRECEDENTE SE NECESSARIO ***
  SetLength(FScrollBuffer, 0);

  // *** RICREA ARRAY CON DIMENSIONI CORRETTE ***
  SetLength(FScrollBuffer, FScrollBufferSize);

  for i := 0 to FScrollBufferSize - 1 do
  begin
    // *** INIZIALIZZA ARRAY CHARS CON CONTROLLO ***
    SetLength(FScrollBuffer[i].Chars, FDesiredCols);
    if Length(FScrollBuffer[i].Chars) <> FDesiredCols then
    begin
      WriteLn('ERROR: Failed to set Chars length for buffer ', i);
      Exit;
    end;

    FScrollBuffer[i].Length := 0;

    for j := 0 to FDesiredCols - 1 do
    begin
      FScrollBuffer[i].Chars[j].Character := ' ';
      FScrollBuffer[i].Chars[j].FgColor := FDefaultFgColor;
      FScrollBuffer[i].Chars[j].BgColor := FDefaultBgColor;
    end;
  end;

  // *** RESET STATO ***
  FScrollBufferCount := 0;
  FScrollBufferStart := 0;
  FScrollOffset := 0;
  FInScrollMode := False;
end;

procedure TSDL2OutputDevice.AddLineToScrollBuffer(LineIndex: Integer);
var
  BufferIndex: Integer;
  i: Integer;
begin
  if (LineIndex < 0) or (LineIndex >= FDesiredRows) then Exit;
  if FScrollBufferSize <= 0 then Exit;

  // Calcola posizione nel buffer circolare
  BufferIndex := (FScrollBufferStart + FScrollBufferCount) mod FScrollBufferSize;

  // === FIX: COPIA carattere per carattere CON INDICI CORRETTI ===
  for i := 0 to FDesiredCols - 1 do
  begin
    if (i < Length(FVideoBuffer)) and (LineIndex < Length(FVideoBuffer[i])) then
    begin
      // ← FIX: [i][LineIndex] is correct for FVideoBuffer[cols][rows]
      FScrollBuffer[BufferIndex].Chars[i].Character := FVideoBuffer[i][LineIndex].Character;
      FScrollBuffer[BufferIndex].Chars[i].FgColor := FVideoBuffer[i][LineIndex].FgColor;
      FScrollBuffer[BufferIndex].Chars[i].BgColor := FVideoBuffer[i][LineIndex].BgColor;
    end
    else
    begin
      FScrollBuffer[BufferIndex].Chars[i].Character := ' ';
      FScrollBuffer[BufferIndex].Chars[i].FgColor := FDefaultFgColor;
      FScrollBuffer[BufferIndex].Chars[i].BgColor := FDefaultBgColor;
    end;
  end;
  FScrollBuffer[BufferIndex].Length := FDesiredCols;

  // Aggiorna contatori buffer circolare
  if FScrollBufferCount < FScrollBufferSize then
    Inc(FScrollBufferCount)
  else
    FScrollBufferStart := (FScrollBufferStart + 1) mod FScrollBufferSize;
end;

procedure TSDL2OutputDevice.DrawCharacterAtDirect(X, Y: Integer; const Ch: string; FgColor, BgColor: TColor);
var
  Surface: PSDL_Surface;
  Texture: PSDL_Texture;
  DstRect: TSDL_Rect;
  Color: TSDL_Color;
  CachedTexture: PSDL_Texture;
begin
  if not FInitialized or (Ch = '') then Exit;
  if (X < 0) or (Y < 0) or (X >= FDesiredCols) or (Y >= FDesiredRows) then Exit;

  DstRect.x := FGridOffsetX + (X * FCharWidth);
  DstRect.y := FGridOffsetY + (Y * FCharHeight);
  DstRect.w := FCharWidth;
  DstRect.h := FCharHeight;

  // Background
  SDL_SetRenderDrawColor(FRenderer, BgColor.R, BgColor.G, BgColor.B, 255);
  SDL_RenderFillRect(FRenderer, @DstRect);

  // Character con cache ottimizzata
  if Ch <> ' ' then
  begin
    CachedTexture := GetCachedCharacterTexture(Ch, FgColor, BgColor);
    if Assigned(CachedTexture) then
    begin
      SDL_RenderCopy(FRenderer, CachedTexture, nil, @DstRect);
    end
    else if Assigned(FFont) then
    begin
      // Fallback al metodo originale se cache fallisce
      Color.r := FgColor.R;
      Color.g := FgColor.G;
      Color.b := FgColor.B;
      Color.a := 255;

      Surface := TTF_RenderUTF8_Blended(FFont, PChar(Ch), Color);
      if Assigned(Surface) then
      begin
        try
          Texture := SDL_CreateTextureFromSurface(FRenderer, Surface);
          if Assigned(Texture) then
          begin
            try
              SDL_RenderCopy(FRenderer, Texture, nil, @DstRect);
            finally
              SDL_DestroyTexture(Texture);
            end;
          end;
        finally
          SDL_FreeSurface(Surface);
        end;
      end;
    end;
  end;

  // NON aggiorna il video buffer
end;

procedure TSDL2OutputDevice.ShowScrollBuffer;
var
  DisplayLine, BufferLine, BufferIndex: Integer;
  i: Integer;
begin
  if not FInitialized then Exit;

  SDL_SetRenderDrawColor(FRenderer, FBackgroundColor.R, FBackgroundColor.G, FBackgroundColor.B, 255);
  SDL_RenderClear(FRenderer);

  for DisplayLine := 0 to FDesiredRows - 1 do
  begin
    BufferLine := FScrollBufferCount - FScrollOffset - FDesiredRows + DisplayLine;

    if (BufferLine >= 0) and (BufferLine < FScrollBufferCount) then
    begin
      BufferIndex := (FScrollBufferStart + BufferLine) mod FScrollBufferSize;

      for i := 0 to FDesiredCols - 1 do
      begin
        if FScrollBuffer[BufferIndex].Chars[i].Character <> ' ' then
        begin
          DrawCharacterAtDirect(i, DisplayLine,
                               FScrollBuffer[BufferIndex].Chars[i].Character,
                               FScrollBuffer[BufferIndex].Chars[i].FgColor,
                               FScrollBuffer[BufferIndex].Chars[i].BgColor);
        end;
      end;
    end;
  end;
end;

procedure TSDL2OutputDevice.ScrollUp(Lines: Integer = 1);
begin
  if FScrollBufferCount = 0 then Exit;

  FScrollOffset := Min(FScrollOffset + Lines, FScrollBufferCount);
  FInScrollMode := (FScrollOffset > 0);

  ShowScrollBuffer;
end;

procedure TSDL2OutputDevice.ScrollDown(Lines: Integer = 1);
begin
  FScrollOffset := Max(FScrollOffset - Lines, 0);
  FInScrollMode := (FScrollOffset > 0);

  if FInScrollMode then
    ShowScrollBuffer
  else
    RedrawScreen;
end;

procedure TSDL2OutputDevice.ScrollToBottom;
begin
  if FScrollOffset > 0 then
  begin
    FScrollOffset := 0;
    FInScrollMode := False;
    RebuildDisplayFromBuffer;
  end;
end;

procedure TSDL2OutputDevice.RebuildDisplayFromBuffer;
var
  DisplayLine, BufferLine: Integer;
  i, j: Integer;
  BufferIndex: Integer;
begin
  if not FInitialized then Exit;

  SDL_SetRenderDrawColor(FRenderer, FBackgroundColor.R, FBackgroundColor.G, FBackgroundColor.B, 255);
  SDL_RenderClear(FRenderer);

  for i := 0 to FDesiredCols - 1 do
    for j := 0 to FDesiredRows - 1 do
    begin
      FVideoBuffer[i][j].Character := ' ';
      FVideoBuffer[i][j].FgColor := FDefaultFgColor;
      FVideoBuffer[i][j].BgColor := FDefaultBgColor;
      FVideoBuffer[i][j].HasChar := False;
      FVideoBuffer[i][j].CustomColors := False;
    end;

  for DisplayLine := 0 to FDesiredRows - 1 do
  begin
    BufferLine := FScrollBufferCount - FScrollOffset - FDesiredRows + DisplayLine;

    if (BufferLine >= 0) and (BufferLine < FScrollBufferCount) then
    begin
      BufferIndex := (FScrollBufferStart + BufferLine) mod FScrollBufferSize;

      for i := 0 to Min(FDesiredCols - 1, FScrollBuffer[BufferIndex].Length - 1) do
      begin
        FVideoBuffer[i][DisplayLine].Character := FScrollBuffer[BufferIndex].Chars[i].Character;
        FVideoBuffer[i][DisplayLine].FgColor := FScrollBuffer[BufferIndex].Chars[i].FgColor;
        FVideoBuffer[i][DisplayLine].BgColor := FScrollBuffer[BufferIndex].Chars[i].BgColor;
        FVideoBuffer[i][DisplayLine].HasChar := True;

        DrawCharacterAt(i, DisplayLine, FVideoBuffer[i][DisplayLine].Character,
                       FVideoBuffer[i][DisplayLine].FgColor, FVideoBuffer[i][DisplayLine].BgColor);
      end;
    end;
  end;
end;

procedure TSDL2OutputDevice.ProcessScrollInput;
var
  Event: TSDL_Event;
begin
  while SDL_PollEvent(@Event) <> 0 do
  begin
    case Event.type_ of
      SDL_QUITEV:
        FQuitRequested := True;

      SDL_KEYDOWN:
      begin
        if not HandleScrollKeys(Event.key.keysym.sym, Event.key.keysym.mod_) then
        begin
          case Event.key.keysym.sym of
            SDLK_F11:
              if (Event.key.keysym.mod_ and KMOD_CTRL) <> 0 then
                SetFullscreen(not FFullscreen);
            SDLK_ESCAPE:
              if FFullscreen then
                SetFullscreen(False);
          end;
        end;
      end;
    end;
  end;
end;

function TSDL2OutputDevice.HandleScrollKeys(Key: Integer; Modifiers: Integer): Boolean;
begin
  Result := True;

  case Key of
    SDLK_PAGEUP:
    begin
      if (Modifiers and KMOD_SHIFT) <> 0 then
        ScrollUp(FDesiredRows div 2)
      else
        Result := False;
    end;

    SDLK_PAGEDOWN:
    begin
      if (Modifiers and KMOD_SHIFT) <> 0 then
        ScrollDown(FDesiredRows div 2)
      else
        Result := False;
    end;

    SDLK_UP:
    begin
      if (Modifiers and KMOD_SHIFT) <> 0 then
        ScrollUp(1)
      else
        Result := False;
    end;

    SDLK_DOWN:
    begin
      if (Modifiers and KMOD_SHIFT) <> 0 then
        ScrollDown(1)
      else
        Result := False;
    end;

    SDLK_HOME:
    begin
      if (Modifiers and KMOD_SHIFT) <> 0 then
      begin
        FScrollOffset := FScrollBufferCount;
        FInScrollMode := True;
        RebuildDisplayFromBuffer;
      end
      else
        Result := False;
    end;

    SDLK_END:
    begin
      if (Modifiers and KMOD_SHIFT) <> 0 then
        ScrollToBottom
      else
        Result := False;
    end;

    else
      Result := False;
  end;
end;

procedure TSDL2OutputDevice.OnUserInput;
begin
  if FInScrollMode then
  begin
    FInScrollMode := False;
    FScrollOffset := 0;
    // Don't redraw anything - the video buffer is already correct
  end;
end;

procedure TSDL2OutputDevice.MarkPromptRow;
begin
  FPromptRow := FCursorY;
end;

function TSDL2OutputDevice.IsAtBottom: Boolean;
begin
  Result := FScrollOffset = 0;
end;

procedure TSDL2OutputDevice.LoadFontWithSize(Size: Integer);
const
  DefaultFontPaths: array[0..9] of string = (
    'font/PixelOperatorMono8-Bold.ttf',
    'assets/PixelOperatorMono8-Bold.ttf',
    'font/Hack-Bold.ttf',
    'assets/Hack-Bold.ttf',
    'Hack-Bold.ttf',
    'C:\Windows\Fonts\consola.ttf',
    'C:\Windows\Fonts\courbd.ttf',
    '/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf',
    '/System/Library/Fonts/Monaco.ttf',
    '/usr/share/fonts/truetype/liberation/LiberationMono-Bold.ttf'
  );
var
  i: Integer;
begin
  WriteLn('Loading font with size: ', Size, 'pt');

  for i := 0 to High(DefaultFontPaths) do
  begin
    if FileExists(DefaultFontPaths[i]) then
    begin
      if TryLoadFont(DefaultFontPaths[i], Size) then
      begin
        WriteLn('Font loaded: ', DefaultFontPaths[i]);
        // Clear character cache quando cambia font
        ClearCharacterCache;
        Exit;
      end;
    end;
  end;

  WriteLn('Warning: Could not load any font, text rendering may be poor');
end;

function TSDL2OutputDevice.TryLoadFont(const FontPath: string; Size: Integer): Boolean;
begin
  Result := False;

  if Assigned(FFont) then
  begin
    TTF_CloseFont(FFont);
    FFont := nil;
  end;

  if FileExists(FontPath) then
  begin
    FFont := TTF_OpenFont(PChar(FontPath), Size);
    if Assigned(FFont) then
    begin
      TTF_SetFontStyle(FFont, TTF_STYLE_NORMAL);
      TTF_SetFontOutline(FFont, 0);
      TTF_SetFontHinting(FFont, TTF_HINTING_MONO);
      TTF_SetFontKerning(FFont, 1);

      TTF_SizeUTF8(FFont, 'W', @FCharWidth, @FCharHeight);
      Result := True;
    end;
  end;
end;

procedure TSDL2OutputDevice.CalculateCharacterDimensions;
begin
  if Assigned(FFont) then
  begin
    TTF_SizeUTF8(FFont, 'W', @FCharWidth, @FCharHeight);
  end
  else
  begin
    FCharWidth := FFontSize div 2;
    FCharHeight := FFontSize;
  end;

  FGridOffsetX := (FWindowWidth - (FDesiredCols * FCharWidth)) div 2;
  FGridOffsetY := (FWindowHeight - (FDesiredRows * FCharHeight)) div 2;
end;

procedure TSDL2OutputDevice.DrawCharacterAt(X, Y: Integer; const Ch: string; FgColor, BgColor: TColor);
var
  Surface: PSDL_Surface;
  Texture: PSDL_Texture;
  DstRect: TSDL_Rect;
  Color: TSDL_Color;
begin
  if not FInitialized or (Ch = '') then Exit;
  if (X < 0) or (Y < 0) or (X >= FDesiredCols) or (Y >= FDesiredRows) then Exit;

  // === USA COORDINATE LOGICHE - SDL SCALA AUTOMATICAMENTE ===
  DstRect.x := X * FCharWidth;
  DstRect.y := Y * FCharHeight;
  DstRect.w := FCharWidth;
  DstRect.h := FCharHeight;

  // Background
  SDL_SetRenderDrawColor(FRenderer, BgColor.R, BgColor.G, BgColor.B, 255);
  SDL_RenderFillRect(FRenderer, @DstRect);

  // Character
  if Assigned(FFont) then
  begin
    Color.r := FgColor.R;
    Color.g := FgColor.G;
    Color.b := FgColor.B;
    Color.a := 255;

    Surface := TTF_RenderUTF8_Blended(FFont, PChar(Ch), Color);
    if Assigned(Surface) then
    begin
      try
        Texture := SDL_CreateTextureFromSurface(FRenderer, Surface);
        if Assigned(Texture) then
        begin
          try
            SDL_RenderCopy(FRenderer, Texture, nil, @DstRect);
          finally
            SDL_DestroyTexture(Texture);
          end;
        end;
      finally
        SDL_FreeSurface(Surface);
      end;
    end;
  end;

  // Aggiorna video buffer
  if (X < Length(FVideoBuffer)) and (Y < Length(FVideoBuffer[0])) then
  begin
    FVideoBuffer[X][Y].Character := Ch;
    FVideoBuffer[X][Y].FgColor := FgColor;
    FVideoBuffer[X][Y].BgColor := BgColor;
    FVideoBuffer[X][Y].HasChar := True;
  end;
end;

procedure TSDL2OutputDevice.RenderTextFast(const Text: string; StartX, StartY: Integer; ClearBackground: Boolean = False);
var
  Surface: PSDL_Surface;
  Texture: PSDL_Texture;
  DstRect: TSDL_Rect;
  Color: TSDL_Color;
  CleanText: string;
  TextWidth, TextHeight: Integer;
begin
  if not Assigned(FFont) or (Text = '') then Exit;

  CleanText := Text;

  // Calculate rectangle using SDL_ttf to get REAL text dimensions
  if TTF_SizeUTF8(FFont, PChar(CleanText), @TextWidth, @TextHeight) <> 0 then
  begin
    // Fallback if size calculation fails
    TextWidth := Length(CleanText) * FCharWidth;
    TextHeight := FCharHeight;
  end;

  DstRect.x := FGridOffsetX + (StartX * FCharWidth);
  DstRect.y := FGridOffsetY + (StartY * FCharHeight);
  DstRect.w := TextWidth;
  DstRect.h := TextHeight;

  // Clear background if requested
  if ClearBackground then
  begin
    SDL_SetRenderDrawColor(FRenderer, FBackgroundColor.R, FBackgroundColor.G, FBackgroundColor.B, 255);
    SDL_RenderFillRect(FRenderer, @DstRect);
  end;

  // Render text
  Color.r := FForegroundColor.R;
  Color.g := FForegroundColor.G;
  Color.b := FForegroundColor.B;
  Color.a := 255;

  Surface := TTF_RenderUTF8_Blended(FFont, PChar(CleanText), Color);
  if Assigned(Surface) then
  begin
    try
      Texture := SDL_CreateTextureFromSurface(FRenderer, Surface);
      if Assigned(Texture) then
      begin
        try
          SDL_SetTextureBlendMode(Texture, SDL_BLENDMODE_BLEND);
          SDL_RenderCopy(FRenderer, Texture, nil, @DstRect);
        finally
          SDL_DestroyTexture(Texture);
        end;
      end;
    finally
      SDL_FreeSurface(Surface);
    end;
  end;
end;

procedure TSDL2OutputDevice.Print(const Text: string; ClearBackground: Boolean = False);
var
  p: PChar;
  Ch: string;
  CharLen: Integer;
begin
  if not FInitialized or (Text = '') then Exit;

  // === OTTIMIZZAZIONE: BUFFERING ===
  if Length(FPrintBuffer) + Length(Text) > FMaxBufferSize then
    FlushPrintBuffer;

  FPrintBuffer := FPrintBuffer + Text;
  FBufferSize := FBufferSize + Length(Text);

  // Processo il testo carattere per carattere MA senza rendering immediato
  p := PChar(Text);
  while p^ <> #0 do
  begin
    // Gestione carattere singolo ASCII
    if Ord(p^) < 128 then
    begin
      Ch := p^;
      CharLen := 1;
    end
    else
    begin
      // Carattere UTF-8 multi-byte
      if (Ord(p^) and $E0) = $C0 then
        CharLen := 2
      else if (Ord(p^) and $F0) = $E0 then
        CharLen := 3
      else if (Ord(p^) and $F8) = $F0 then
        CharLen := 4
      else
        CharLen := 1;

      SetLength(Ch, CharLen);
      Move(p^, Ch[1], CharLen);
    end;

    if Ch = #10 then
    begin
      NewLine;
      Inc(p);
      Continue;
    end;

    if Ch = #13 then
    begin
      Inc(p);
      Continue;
    end;

    // === OTTIMIZZAZIONE: Solo aggiorna buffer e marca dirty ===
    if (FCursorX >= 0) and (FCursorY >= 0) and
       (FCursorX < FDesiredCols) and (FCursorY < FDesiredRows) then
    begin
      if (FCursorX < Length(FVideoBuffer)) and (FCursorY < Length(FVideoBuffer[0])) then
      begin
        FVideoBuffer[FCursorX][FCursorY].Character := Ch;
        FVideoBuffer[FCursorX][FCursorY].FgColor := FForegroundColor;
        FVideoBuffer[FCursorX][FCursorY].BgColor := FBackgroundColor;
        FVideoBuffer[FCursorX][FCursorY].HasChar := True;
        MarkCellDirty(FCursorX, FCursorY);  // === MARCA DIRTY ===
      end;
    end;

    Inc(FCursorX);
    // WRAP VISIVO: solo riposizionamento cursore, nessun newline logico
    if FCursorX >= FDesiredCols then
    begin
      FCursorX := 0;
      Inc(FCursorY);
      if FCursorY >= FDesiredRows then
        FCursorY := FDesiredRows - 1;
    end;

    Inc(p, CharLen);
  end;
end;

procedure TSDL2OutputDevice.SaveCursorBackground(X, Y: Integer);
begin
  if (X < 0) or (Y < 0) or (X >= FDesiredCols) or (Y >= FDesiredRows) then
  begin
    FCursorSavedValid := False;
    Exit;
  end;

  if (X < Length(FVideoBuffer)) and (Y < Length(FVideoBuffer[0])) then
  begin
    FCursorSavedChar := FVideoBuffer[X][Y].Character;
    FCursorSavedFg := FVideoBuffer[X][Y].FgColor;
    FCursorSavedBg := FVideoBuffer[X][Y].BgColor;
    FCursorSavedValid := FVideoBuffer[X][Y].HasChar;
  end
  else
  begin
    FCursorSavedValid := False;
  end;
end;

procedure TSDL2OutputDevice.RestoreCursorBackground(X, Y: Integer);
begin
  if not FCursorSavedValid then Exit;
  DrawCharacterAt(X, Y, FCursorSavedChar, FCursorSavedFg, FCursorSavedBg);
end;

procedure TSDL2OutputDevice.ShowCursor(X, Y: Integer);
begin
  if not FInitialized then Exit;
  if FCursorVisible then Exit;

  if (X < 0) or (Y < 0) or (X >= FDesiredCols) or (Y >= FDesiredRows) then Exit;

  SaveCursorBackground(X, Y);

  if FCursorSavedValid and (FCursorSavedChar <> ' ') and (FCursorSavedChar <> '') then
  begin
    // Invert colors for existing character
    DrawCharacterAt(X, Y, FCursorSavedChar, FCursorSavedBg, FCursorSavedFg);
  end
  else
  begin
    // Draw solid block cursor
    DrawCharacterAt(X, Y, ' ', FBackgroundColor, FForegroundColor);
  end;

  FCursorVisible := True;
end;

procedure TSDL2OutputDevice.HideCursor(X, Y: Integer);
begin
  if not FInitialized then Exit;
  if not FCursorVisible then Exit;

  if (X < 0) or (Y < 0) or (X >= FDesiredCols) or (Y >= FDesiredRows) then Exit;

  if FCursorSavedValid then
  begin
    DrawCharacterAt(X, Y, FCursorSavedChar, FCursorSavedFg, FCursorSavedBg);
  end
  else
  begin
    DrawCharacterAt(X, Y, ' ', FForegroundColor, FBackgroundColor);
  end;

  FCursorVisible := False;
  FCursorSavedValid := False;
end;

procedure TSDL2OutputDevice.PrintLn(const Text: string; ClearBackground: Boolean = False);
begin
  Print(Text, ClearBackground);
  NewLine;
end;

procedure TSDL2OutputDevice.RedrawScreen;
var
  x, y: Integer;
begin
  if not FInitialized then Exit;

  SDL_SetRenderDrawColor(FRenderer, FBackgroundColor.R, FBackgroundColor.G, FBackgroundColor.B, 255);
  SDL_RenderClear(FRenderer);

  // === OTTIMIZZAZIONE: Marca tutto come dirty invece di ridisegnare immediatamente ===
  for y := 0 to FDesiredRows - 1 do
    for x := 0 to FDesiredCols - 1 do
      if FVideoBuffer[x][y].HasChar then
        MarkCellDirty(x, y);
end;

procedure TSDL2OutputDevice.NewLine;
var
  x, y: Integer;
begin
  FCursorX := 0;
  Inc(FCursorY);

  // Se sono all'ultima riga, fai scroll LOGICO invece che fisico
  if FCursorY >= FDesiredRows then
  begin
    // AGGIUNGI la riga che sta per uscire dal top al buffer del pager
    AddLineToScrollBuffer(0);

    // === OTTIMIZZAZIONE: Scroll logico ===
    // Sposta il buffer verso l'alto
    for y := 0 to FDesiredRows - 2 do
    begin
      for x := 0 to FDesiredCols - 1 do
      begin
        FVideoBuffer[x][y] := FVideoBuffer[x][y + 1];
        MarkCellDirty(x, y);  // Marca come dirty
      end;
    end;

    // Pulisci l'ultima riga
    for x := 0 to FDesiredCols - 1 do
    begin
      FVideoBuffer[x][FDesiredRows - 1].Character := ' ';
      FVideoBuffer[x][FDesiredRows - 1].FgColor := FForegroundColor;
      FVideoBuffer[x][FDesiredRows - 1].BgColor := FBackgroundColor;
      FVideoBuffer[x][FDesiredRows - 1].HasChar := False;
      MarkCellDirty(x, FDesiredRows - 1);
    end;

    // RIMANI sull'ultima riga
    FCursorY := FDesiredRows - 1;
  end;
end;

procedure TSDL2OutputDevice.Clear;
var
  x, y: Integer;
begin
  if not FInitialized then Exit;

  SDL_SetRenderDrawColor(FRenderer, FBackgroundColor.R, FBackgroundColor.G, FBackgroundColor.B, 255);
  SDL_RenderClear(FRenderer);

  if (Length(FVideoBuffer) >= FDesiredCols) and (Length(FVideoBuffer[0]) >= FDesiredRows) then
  begin
    for x := 0 to FDesiredCols - 1 do
      for y := 0 to FDesiredRows - 1 do
      begin
        FVideoBuffer[x][y].Character := ' ';
        FVideoBuffer[x][y].FgColor := FForegroundColor;
        FVideoBuffer[x][y].BgColor := FBackgroundColor;
        FVideoBuffer[x][y].HasChar := False;
        MarkCellDirty(x, y);  // Marca tutto come dirty
      end;
  end;

  FCursorX := 0;
  FCursorY := 0;
  FCursorVisible := False;
  FCursorSavedValid := False;

  // *** RESET SCROLL MODE ***
  FInScrollMode := False;
  FScrollOffset := 0;
end;

procedure TSDL2OutputDevice.SetCursor(X, Y: Integer);
begin
  FCursorX := X;
  FCursorY := Y;

  if FCursorX < 0 then FCursorX := 0;
  if FCursorY < 0 then FCursorY := 0;
  if FCursorX >= FDesiredCols then FCursorX := FDesiredCols - 1;
  if FCursorY >= FDesiredRows then FCursorY := FDesiredRows - 1;
end;

procedure TSDL2OutputDevice.MoveCursor(DeltaX, DeltaY: Integer);
begin
  SetCursor(FCursorX + DeltaX, FCursorY + DeltaY);
end;

function TSDL2OutputDevice.GetCursorX: Integer;
begin
  Result := FCursorX;
end;

function TSDL2OutputDevice.GetCursorY: Integer;
begin
  Result := FCursorY;
end;

procedure TSDL2OutputDevice.SetColors(Foreground, Background: TColor);
begin
  FForegroundColor := Foreground;
  FBackgroundColor := Background;
end;

procedure TSDL2OutputDevice.RenderOverlay;
var
  R: TSDL_Rect;
  OverlayColor: TColor;
begin
  OverlayColor.R := 32;
  OverlayColor.G := 32;
  OverlayColor.B := 32;

  SDL_SetRenderDrawColor(FRenderer, OverlayColor.R, OverlayColor.G, OverlayColor.B, 255);

  // Top and bottom bars
  if FGridOffsetY > 0 then
  begin
    R.x := 0; R.y := 0; R.w := FWindowWidth; R.h := FGridOffsetY;
    SDL_RenderFillRect(FRenderer, @R);
    R.y := FWindowHeight - FGridOffsetY;
    SDL_RenderFillRect(FRenderer, @R);
  end;

  // Left and right bars
  if FGridOffsetX > 0 then
  begin
    R.x := 0; R.y := 0; R.w := FGridOffsetX; R.h := FWindowHeight;
    SDL_RenderFillRect(FRenderer, @R);
    R.x := FWindowWidth - FGridOffsetX;
    SDL_RenderFillRect(FRenderer, @R);
  end;
end;

procedure TSDL2OutputDevice.SetFullscreen(Enabled: Boolean);
var
  Flags: UInt32;
begin
  if not FInitialized then Exit;

  if Enabled then
    Flags := SDL_WINDOW_FULLSCREEN_DESKTOP
  else
    Flags := 0;

  SDL_SetWindowFullscreen(FWindow, Flags);
  FFullscreen := Enabled;

  Present;
end;

function TSDL2OutputDevice.IsFullscreen: Boolean;
begin
  Result := FFullscreen;
end;

procedure TSDL2OutputDevice.HandleQuitEvents;
var
  Event: TSDL_Event;
begin
  while SDL_PollEvent(@Event) <> 0 do
  begin
    case Event.type_ of
      SDL_QUITEV:
        FQuitRequested := True;
      SDL_KEYDOWN:
        case Event.key.keysym.sym of
          SDLK_F11:
            if (Event.key.keysym.mod_ and KMOD_CTRL) <> 0 then
              SetFullscreen(not FFullscreen);
          SDLK_ESCAPE:
            if FFullscreen then
              SetFullscreen(False);
        end;
    end;
  end;
end;

function TSDL2OutputDevice.ShouldQuit: Boolean;
begin
  HandleQuitEvents;
  Result := FQuitRequested;
end;

procedure TSDL2OutputDevice.SetCellColor(X, Y: Integer; Fg, Bg: TColor);
begin
  if (X >= 0) and (X < FDesiredCols) and (Y >= 0) and (Y < FDesiredRows) and
     (X < Length(FVideoBuffer)) and (Y < Length(FVideoBuffer[0])) then
  begin
    FVideoBuffer[X][Y].FgColor := Fg;
    FVideoBuffer[X][Y].BgColor := Bg;
    FVideoBuffer[X][Y].CustomColors := True;
    MarkCellDirty(X, Y);  // Marca dirty
  end;
end;

procedure TSDL2OutputDevice.SetDefaultColors(Fg, Bg: TColor);
begin
  FDefaultFgColor := Fg;
  FDefaultBgColor := Bg;
end;

function TSDL2OutputDevice.GetActualCols: Integer;
begin
  Result := FDesiredCols;
end;

function TSDL2OutputDevice.GetActualRows: Integer;
begin
  Result := FDesiredRows;
end;

procedure TSDL2OutputDevice.Present;
var
  OverlayRect: TSDL_Rect;
begin
  if FInitialized then
  begin
    // Se usa viewport centrato, disegna il bordo
    if FUseCenteredViewport then
    begin
      SDL_SetRenderDrawColor(FRenderer, FBorderColor.R, FBorderColor.G, FBorderColor.B, 255);
      SDL_RenderClear(FRenderer);
    end;

    // === MAIN OPTIMIZATION: Render only dirty regions ===
    RenderDirtyRegionsOnly;

    // Flush print buffer if needed
    FlushPrintBuffer;

    if FUseCenteredViewport then
      RenderOverlay;  // This already renders the border

    // === FAST MODE OVERLAY ===
    // When FAST mode is enabled, draw black overlay with adjustable alpha
    // This emulates C128's 2MHz mode where video output is disabled
    if FFastMode then
    begin
      SDL_SetRenderDrawBlendMode(FRenderer, SDL_BLENDMODE_BLEND);
      SDL_SetRenderDrawColor(FRenderer, 0, 0, 0, FFastModeAlpha);
      OverlayRect.x := 0;
      OverlayRect.y := 0;
      OverlayRect.w := FWindowWidth;
      OverlayRect.h := FWindowHeight;
      SDL_RenderFillRect(FRenderer, @OverlayRect);
    end;

    SDL_RenderPresent(FRenderer);
  end;
end;

procedure TSDL2OutputDevice.Shutdown;
begin
  // Guard against multiple Shutdown calls
  if not FInitialized then
    Exit;

  ClearCharacterCache;

  if Assigned(FBackgroundTexture) then
  begin
    SDL_DestroyTexture(FBackgroundTexture);
    FBackgroundTexture := nil;
  end;

  if Assigned(FFont) then
  begin
    TTF_CloseFont(FFont);
    FFont := nil;
  end;

  if Assigned(FRenderer) then
  begin
    SDL_DestroyRenderer(FRenderer);
    FRenderer := nil;
  end;

  if Assigned(FWindow) then
  begin
    SDL_DestroyWindow(FWindow);
    FWindow := nil;
  end;

  TTF_Quit;
  SDL_Quit;

  FInitialized := False;
end;

function TSDL2OutputDevice.GetInScrollMode: Boolean;
begin
  Result := FInScrollMode;
end;

function TSDL2OutputDevice.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

// Stub methods - not implemented in base SDL2 device
function TSDL2OutputDevice.SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean = False;
                                          SplitLine: Integer = -1): Boolean;
begin
  Result := False;
end;

function TSDL2OutputDevice.GetGraphicMode: TGraphicMode;
begin
  Result := gm40ColText;  // Base device always in text mode
end;

function TSDL2OutputDevice.IsInGraphicsMode: Boolean;
begin
  Result := False;
end;

procedure TSDL2OutputDevice.SetPixel(X, Y: Integer; RGB: UInt32);
begin
  // Base device doesn't support graphics
end;

procedure TSDL2OutputDevice.SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex);
begin
  // Base device doesn't support graphics
end;

function TSDL2OutputDevice.GetPixel(X, Y: Integer): UInt32;
begin
  Result := 0;
end;

procedure TSDL2OutputDevice.EnablePalette(Enable: Boolean);
begin
  // Base device doesn't support palette
end;

function TSDL2OutputDevice.IsPaletteEnabled: Boolean;
begin
  Result := False;
end;

procedure TSDL2OutputDevice.SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
begin
  // Base device doesn't support palette
end;

procedure TSDL2OutputDevice.SetPaletteColorRGBA(Index: TPaletteIndex; R, G, B: Byte; A: Byte = 255);
begin
  // Base device doesn't support palette
end;

function TSDL2OutputDevice.GetPaletteColor(Index: TPaletteIndex): UInt32;
begin
  Result := 0;
end;

procedure TSDL2OutputDevice.ResetPalette;
begin
  // Base device doesn't support palette
end;

function TSDL2OutputDevice.LoadPaletteFromJSON(const FileName: string): Boolean;
begin
  Result := False;  // Base device doesn't support palette
end;

function TSDL2OutputDevice.SavePaletteToJSON(const FileName: string): Boolean;
begin
  Result := False;  // Base device doesn't support palette
end;

function TSDL2OutputDevice.GetLastPaletteError: string;
begin
  Result := 'PALETTE NOT SUPPORTED';
end;

procedure TSDL2OutputDevice.SetCenteredViewport(UseViewport: Boolean;
  ViewportRect: TSDL_Rect; BorderColor: TColor);
begin
  FUseCenteredViewport := UseViewport;
  FContentViewport := ViewportRect;
  FBorderColor := BorderColor;

  if UseViewport then
  begin
    // Ricalcola gli offset per centrare nel viewport
    FGridOffsetX := ViewportRect.x;
    FGridOffsetY := ViewportRect.y;

    // Ricalcola dimensioni carattere per il viewport
    if (FDesiredCols > 0) and (ViewportRect.w > 0) then
      FCharWidth := ViewportRect.w div FDesiredCols;
    if (FDesiredRows > 0) and (ViewportRect.h > 0) then
      FCharHeight := ViewportRect.h div FDesiredRows;
  end;
end;

// === SHAPE DRAWING METHODS ===

procedure TSDL2OutputDevice.DrawBox(X1, Y1, X2, Y2: Integer; Angle: Double);
var
  Style: TShapeStyle;
begin
  // Use current style state
  Style.Border := FCurrentBorderStyle;
  Style.Fill := FCurrentFillStyle;
  DrawBoxStyled(X1, Y1, X2, Y2, Style, Angle);
end;

procedure TSDL2OutputDevice.DrawBoxStyled(X1, Y1, X2, Y2: Integer;
  const Style: TShapeStyle; Angle: Double);
var
  MinX, MinY, MaxX, MaxY: Integer;
  CenterX, CenterY: Double;
  HalfW, HalfH: Double;
  CosA, SinA: Double;
  Corners: array[0..3] of record X, Y: Double; end;
  RotatedCorners: array[0..3] of record X, Y: Double; end;
  Vertices: array[0..3] of TSDL_Vertex;
  Indices: array[0..5] of cint;
  FillColor, BorderColor: TSDL_Color;
  i: Integer;
  Rect: TSDL_Rect;
  Points: array[0..4] of TSDL_Point;
begin
  if not FInitialized or not Assigned(FRenderer) then Exit;

  // Normalize coordinates
  MinX := Min(X1, X2);
  MaxX := Max(X1, X2);
  MinY := Min(Y1, Y2);
  MaxY := Max(Y1, Y2);

  // Fast path: no rotation - use simple SDL functions
  if Abs(Angle) < 0.001 then
  begin
    Rect.x := MinX;
    Rect.y := MinY;
    Rect.w := MaxX - MinX;
    Rect.h := MaxY - MinY;

    // Draw fill
    if Style.Fill.Style <> fsNone then
    begin
      SDL_SetRenderDrawColor(FRenderer,
        GetRed(Style.Fill.Color), GetGreen(Style.Fill.Color),
        GetBlue(Style.Fill.Color), GetAlpha(Style.Fill.Color));
      SDL_RenderFillRect(FRenderer, @Rect);
    end;

    // Draw border
    if (Style.Border.LineStyle <> blsNone) and (Style.Border.Width > 0) then
    begin
      SDL_SetRenderDrawColor(FRenderer,
        GetRed(Style.Border.Color), GetGreen(Style.Border.Color),
        GetBlue(Style.Border.Color), GetAlpha(Style.Border.Color));
      SDL_RenderDrawRect(FRenderer, @Rect);
    end;
    Exit;
  end;

  // Rotation path: use SDL_RenderGeometry with rotated vertices
  // Calculate center and half-dimensions
  CenterX := (MinX + MaxX) / 2.0;
  CenterY := (MinY + MaxY) / 2.0;
  HalfW := (MaxX - MinX) / 2.0;
  HalfH := (MaxY - MinY) / 2.0;

  // Define corners relative to center (before rotation)
  // 0: top-left, 1: top-right, 2: bottom-right, 3: bottom-left
  Corners[0].X := -HalfW; Corners[0].Y := -HalfH;
  Corners[1].X := +HalfW; Corners[1].Y := -HalfH;
  Corners[2].X := +HalfW; Corners[2].Y := +HalfH;
  Corners[3].X := -HalfW; Corners[3].Y := +HalfH;

  // Precompute sin/cos (angle in degrees, convert to radians)
  SinCos(DegToRad(Angle), SinA, CosA);

  // Rotate corners and translate to world coordinates
  for i := 0 to 3 do
  begin
    RotatedCorners[i].X := CenterX + Corners[i].X * CosA - Corners[i].Y * SinA;
    RotatedCorners[i].Y := CenterY + Corners[i].X * SinA + Corners[i].Y * CosA;
  end;

  // Draw fill using SDL_RenderGeometry (2 triangles)
  if Style.Fill.Style <> fsNone then
  begin
    FillColor.r := GetRed(Style.Fill.Color);
    FillColor.g := GetGreen(Style.Fill.Color);
    FillColor.b := GetBlue(Style.Fill.Color);
    FillColor.a := GetAlpha(Style.Fill.Color);

    // Setup vertices
    for i := 0 to 3 do
    begin
      Vertices[i].position.x := RotatedCorners[i].X;
      Vertices[i].position.y := RotatedCorners[i].Y;
      Vertices[i].color := FillColor;
      Vertices[i].tex_coord.x := 0;
      Vertices[i].tex_coord.y := 0;
    end;

    // Triangle indices: 0-1-2 and 0-2-3
    Indices[0] := 0; Indices[1] := 1; Indices[2] := 2;
    Indices[3] := 0; Indices[4] := 2; Indices[5] := 3;

    SDL_RenderGeometry(FRenderer, nil, @Vertices[0], 4, @Indices[0], 6);
  end;

  // Draw border using SDL_RenderDrawLines (rotated)
  if (Style.Border.LineStyle <> blsNone) and (Style.Border.Width > 0) then
  begin
    SDL_SetRenderDrawColor(FRenderer,
      GetRed(Style.Border.Color), GetGreen(Style.Border.Color),
      GetBlue(Style.Border.Color), GetAlpha(Style.Border.Color));

    // Convert to SDL_Point (integer) and close the loop
    for i := 0 to 3 do
    begin
      Points[i].x := Round(RotatedCorners[i].X);
      Points[i].y := Round(RotatedCorners[i].Y);
    end;
    Points[4] := Points[0];  // Close the rectangle

    SDL_RenderDrawLines(FRenderer, @Points[0], 5);

    // TODO: For border width > 1, draw thick lines or use offset polygons
    // TODO: For dashed/dotted styles, implement custom line drawing
  end;
end;

procedure TSDL2OutputDevice.DrawBoxWithColor(X1, Y1, X2, Y2: Integer; Color: UInt32; Angle: Double; Filled: Boolean);
var
  Style: TShapeStyle;
begin
  // Base implementation: Color is used as direct RGBA value
  // Subclasses (like TSDL2GraphicsOutputDevice) can override to interpret
  // color differently based on graphics mode (0/1 for hires modes)
  Style.Border := MakeDefaultBorderStyle(Color, 1);
  if Filled then
  begin
    Style.Fill := MakeDefaultFillStyle(Color);
    Style.Fill.Style := fsSolid;
  end
  else
  begin
    Style.Fill := MakeDefaultFillStyle(clTransparent);
    Style.Fill.Style := fsNone;
  end;
  DrawBoxStyled(X1, Y1, X2, Y2, Style, Angle);
end;

procedure TSDL2OutputDevice.DrawCircleWithColor(X, Y, XR, YR: Integer; Color: UInt32;
                                                SA, EA, Angle, Inc: Double);
var
  CurrentAngle, EndAngle, RotRad: Double;
  CosRot, SinRot: Double;
  PrevX, PrevY, CurrX, CurrY: Integer;
  Px, Py, Rx, Ry: Double;
  FirstPoint: Boolean;
  R, G, B, A: Byte;
begin
  if FRenderer = nil then Exit;

  // Extract RGBA components from color
  R := (Color shr 24) and $FF;
  G := (Color shr 16) and $FF;
  B := (Color shr 8) and $FF;
  A := Color and $FF;
  SDL_SetRenderDrawColor(FRenderer, R, G, B, A);

  // Rotation angle in radians (clockwise in BASIC = counter-clockwise in math)
  RotRad := -Angle * Pi / 180.0;
  CosRot := Cos(RotRad);
  SinRot := Sin(RotRad);

  // Ensure Inc is valid (minimum 1 degree to avoid infinite loop)
  if Inc < 1 then Inc := 1;
  if Inc > 360 then Inc := 360;

  // Normalize angles
  CurrentAngle := SA;
  EndAngle := EA;
  if EndAngle < CurrentAngle then
    EndAngle := EndAngle + 360;

  FirstPoint := True;
  PrevX := 0;
  PrevY := 0;

  while CurrentAngle <= EndAngle + 0.001 do
  begin
    // Calculate point on ellipse
    Px := XR * Cos(CurrentAngle * Pi / 180.0);
    Py := YR * Sin(CurrentAngle * Pi / 180.0);

    // Apply rotation
    Rx := Px * CosRot - Py * SinRot;
    Ry := Px * SinRot + Py * CosRot;

    // Translate to center
    CurrX := X + Round(Rx);
    CurrY := Y + Round(Ry);

    // Draw line from previous point to current point
    if not FirstPoint then
      SDL_RenderDrawLine(FRenderer, PrevX, PrevY, CurrX, CurrY)
    else
      SDL_RenderDrawPoint(FRenderer, CurrX, CurrY);

    PrevX := CurrX;
    PrevY := CurrY;
    FirstPoint := False;
    CurrentAngle := CurrentAngle + Inc;
  end;

  Present;
end;

procedure TSDL2OutputDevice.SetBorderStyle(const Style: TBorderStyle);
begin
  FCurrentBorderStyle := Style;
end;

procedure TSDL2OutputDevice.SetFillStyle(const Style: TFillStyleDef);
begin
  FCurrentFillStyle := Style;
end;

function TSDL2OutputDevice.GetBorderStyle: TBorderStyle;
begin
  Result := FCurrentBorderStyle;
end;

function TSDL2OutputDevice.GetFillStyle: TFillStyleDef;
begin
  Result := FCurrentFillStyle;
end;

{ FAST mode implementation - C128 2MHz mode emulation }
procedure TSDL2OutputDevice.SetFastMode(Enabled: Boolean);
begin
  FFastMode := Enabled;
  // Force immediate screen update to show/hide the black overlay
  Present;
end;

function TSDL2OutputDevice.GetFastMode: Boolean;
begin
  Result := FFastMode;
end;

procedure TSDL2OutputDevice.SetFastModeAlpha(Alpha: Byte);
begin
  FFastModeAlpha := Alpha;
end;

function TSDL2OutputDevice.GetFastModeAlpha: Byte;
begin
  Result := FFastModeAlpha;
end;

end.
