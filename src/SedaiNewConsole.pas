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
unit SedaiNewConsole;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}

{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, Variants, Math, SDL2, SDL2_ttf, TypInfo,
  SedaiOutputInterface, SedaiGraphicsModes, SedaiGraphicsTypes,
  SedaiGraphicsMemory, SedaiGraphicsPrimitives, SedaiGraphicsBackend,
  SedaiInputState,
  SedaiProgramMemory, SedaiAST, SedaiParserTypes,
  SedaiCommandRouter, SedaiCommandTypes,
  SedaiSDL2VideoModes,
  SedaiBytecodeTypes, SedaiBytecodeVM, SedaiImmediateCompiler,
  SedaiBytecodeSerializer, SedaiBytecodeCompiler,
  SedaiBytecodeDisassembler, SedaiRunner,
  SedaiSSATypes, SedaiSSA,
  SedaiBasicKeywords, SedaiDebugger,
  SedaiMemoryMapper, SedaiC128MemoryMapper, SedaiSpriteEngine, SedaiSpriteTypes,
  SedaiUIWidgets;

const
  SCROLLBACK_LINES = 1000;
  INPUT_HISTORY_SIZE = 4096;  // Same as PowerShell default
  CURSOR_BLINK_MS = 500;  // C64 cursor timing (verified)

  // Default colors - C128 style (RGB values for SDL2 mode 11)
  DEFAULT_BG_R = 129;
  DEFAULT_BG_G = 129;
  DEFAULT_BG_B = 129;
  DEFAULT_FG_R = 219;
  DEFAULT_FG_G = 255;
  DEFAULT_FG_B = 158;

  // Default palette indices for classic modes (0-10)
  DEFAULT_BG_INDEX = 11;  // Dark Grey (#818181)
  DEFAULT_FG_INDEX = 13;  // Light Green (#DBFF9E)

  // Sentinel: cell uses the global background color (no per-cell override)
  BG_USE_DEFAULT = 255;

  DEFAULT_FONT_PATHS: array[0..0] of string = (
    'font\PixelOperatorMono8-Bold.ttf'
  );

type
  TCursorEnableCallback = procedure(Enable: Boolean) of object;

  { Forward declarations }
  TTextBuffer = class;

  { TVideoController - SDL2 video mode management with persistent graphics buffers }
  { NOTE: Using TObject instead of TInterfacedObject because interfaces use CORBA mode (no refcount) }
  TVideoController = class(TObject, IOutputDevice, IGraphicsBackend)
  private
    FWindow: PSDL_Window;
    FRenderer: PSDL_Renderer;
    FCurrentMode: TGraphicMode;
    FModeInfo: TGraphicModeInfo;
    FViewportWidth: Integer;
    FViewportHeight: Integer;
    FViewportX: Integer;        // Viewport X offset (for centering)
    FViewportY: Integer;        // Viewport Y offset (for centering)
    FWindowWidth: Integer;
    FWindowHeight: Integer;
    FFontSize: Integer;
    FCharWidth: Integer;
    FCharHeight: Integer;
    FFullscreen: Boolean;
    FVSync: Boolean;
    FSplitLine: Integer;      // Current split line for mixed modes (0 = no split)
    FPaletteManager: TPaletteManager;
    FFont: PTTF_Font;

    // SDL2 mode 11 colors (direct RGBA)
    FBGColorSDL2: TSDL_Color;
    FFGColorSDL2: TSDL_Color;

    // Palette color cache (avoids repeated lookups during rendering)
    FPaletteCache: array[0..255] of TSDL_Color;
    FPaletteCacheValid: Boolean;

    // Classic modes (0-10) palette indices for drawing
    FForegroundIndex: Byte;
    FBackgroundIndex: Byte;
    FMulticolorIndex1: Byte;
    FMulticolorIndex2: Byte;

    // Screen appearance colors (independent from drawing colors)
    FScreenColorIndex: Byte;   // Viewport background color
    FBorderColorIndex: Byte;   // Border/frame color
    FTextColorIndex: Byte;     // Text and cursor color

    // Text rendering flags
    FReverseActive: Boolean;   // When true, text is printed with inverted colors

    // Pixel Cursor (PC) - invisible cursor for DRAW/LOCATE/RDOT commands
    FPixelCursorX: Integer;
    FPixelCursorY: Integer;

    // COLOR command - color sources (C128-compatible, 0-6)
    FColorSources: array[0..6] of Integer;
    // WIDTH command - line width (1 or 2)
    FLineWidth: Integer;
    // SCALE command - coordinate scaling
    FScaleEnabled: Boolean;
    FScaleXMax: Integer;
    FScaleYMax: Integer;
    // WINDOW command - text window boundaries
    FWindowCol1, FWindowRow1: Integer;
    FWindowCol2, FWindowRow2: Integer;

    // Persistent graphics memory (handles all classic mode buffers)
    FGraphicsMemory: TGraphicsMemory;
    // FreeBASIC image surfaces (IMAGECREATE); id = index+1, freed slots stay nil (G3)
    FImageSurfaces: array of TGraphicsMemory;
    // SDL2 texture for rendering the graphics buffer
    FGraphicsTexture: PSDL_Texture;
    // SDL2 texture for persistent text rendering (dirty tracking optimization)
    FTextTexture: PSDL_Texture;
    FScrollTexture: PSDL_Texture;  // Scratch texture for hardware scroll
    FTextTextureDirty: Boolean;  // True when texture needs full rebuild
    FLastBorderColorIndex: Byte; // Track border color changes
    // Track if SDL was successfully initialized (for cleanup)
    FSDLInitialized: Boolean;

    function InitializeSDL: Boolean;
    procedure CleanupSDL(FullShutdown: Boolean = True);
    function CreateWindowAndRenderer: Boolean;
    procedure CalculateViewportAndFont;
    function LoadFontWithSize(Size: Integer): Boolean;
    function TryLoadFont(const FontPath: string; Size: Integer): Boolean;

    // Graphics texture management
    procedure CreateGraphicsTexture;
    procedure DestroyGraphicsTexture;
    procedure SyncGraphicsBufferToTexture;
    procedure CreateTextTexture;
    procedure DestroyTextTexture;

    // Getters/setters for drawing palette indices
    function GetForegroundIndex: Byte;
    procedure SetForegroundIndex(Value: Byte);
    function GetBackgroundIndex: Byte;
    procedure SetBackgroundIndex(Value: Byte);
    function GetMulticolorIndex1: Byte;
    procedure SetMulticolorIndex1(Value: Byte);
    function GetMulticolorIndex2: Byte;
    procedure SetMulticolorIndex2(Value: Byte);

    // Getters/setters for screen appearance colors
    function GetScreenColorIndex: Byte;
    procedure SetScreenColorIndex(Value: Byte);
    function GetBorderColorIndex: Byte;
    procedure SetBorderColorIndex(Value: Byte);
    function GetTextColorIndex: Byte;
    procedure SetTextColorIndex(Value: Byte);

    // Getters/setters for text rendering flags
    function GetReverseActive: Boolean;
    procedure SetReverseActive(Value: Boolean);

  public
    // Get SDL_Color from palette index (for border/text/sprite rendering)
    function PaletteIndexToSDLColor(Index: Byte): TSDL_Color;
    // Public method for rendering graphics buffer to screen
    procedure RenderGraphicsTexture;
    // Throttled present used by drawing primitives: renders/presents the graphics
    // buffer at most ~60fps so incremental drawing stays visible (progressive) while
    // tight loops don't stall once per shape on vsync. See MaybePresentGraphics body.
    procedure MaybePresentGraphics;
    constructor Create;
    destructor Destroy; override;

    function Initialize(Mode: TGraphicMode; Fullscreen: Boolean = False): Boolean;
    function SetMode(Mode: TGraphicMode): Boolean;
    procedure ToggleFullscreen;

    procedure SetColors(BG, FG: TSDL_Color);
    procedure ClearBorder;      // Clears with FG color (border)
    procedure ClearViewport;    // Clears viewport with BG color
    procedure Present;
    procedure RenderText(ATextBuffer: TTextBuffer);  // Renders the text buffer
    procedure ForceTextureRebuild;  // Recreate render-target text texture after a device/target reset

    // IOutputDevice implementation
    function Initialize(const Title: string = ''; Width: Integer = 80; Height: Integer = 25): Boolean;
    procedure Shutdown;
    function IsInitialized: Boolean;
    procedure Print(const Text: string; ClearBackground: Boolean = False);
    procedure PrintLn(const Text: string; ClearBackground: Boolean = False);
    procedure NewLine;
    procedure Clear;
    procedure ResetPrintState;
    procedure SetCursor(X, Y: Integer);
    procedure MoveCursor(DeltaX, DeltaY: Integer);
    function GetCursorX: Integer;
    function GetCursorY: Integer;
    procedure ShowCursor(X, Y: Integer);
    procedure HideCursor(X, Y: Integer);
    procedure SetColors(Foreground, Background: TColor);
    procedure SetFullscreen(Enabled: Boolean);
    function IsFullscreen: Boolean;
    
  private // Methods for fullscreen management
    procedure UpdateFullscreenMode(NewState: Boolean);
    function ShouldQuit: Boolean;
    function GetActualCols: Integer;
    function GetActualRows: Integer;
    procedure MarkPromptRow;
    procedure OnUserInput;
    function HandleScrollKeys(Key: Integer; Modifiers: Integer): Boolean;
    procedure ProcessScrollInput;
    function GetInScrollMode: Boolean;
    function SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean = False; SplitLine: Integer = -1): Boolean;
    function GetGraphicMode: TGraphicMode;
    function IsInGraphicsMode: Boolean;
    procedure ClearScreen(Mode: Integer);
    procedure SetPixel(X, Y: Integer; RGB: UInt32); overload;
    procedure SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex); overload;
    function GetPixel(X, Y: Integer): UInt32;
    function GetPixelIndex(X, Y: Integer): TPaletteIndex;
    procedure EnablePalette(Enable: Boolean);
    function IsPaletteEnabled: Boolean;
    procedure SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
    procedure SetPaletteColorRGBA(Index: TPaletteIndex; R, G, B: Byte; A: Byte = 255);
    function GetPaletteColor(Index: TPaletteIndex): UInt32;
    procedure ResetPalette;
    procedure LoadPalettePreset(PresetId: Integer);
    function LoadPaletteFromJSON(const FileName: string): Boolean;
    function SavePaletteToJSON(const FileName: string): Boolean;
    function GetLastPaletteError: string;

    // Shape drawing (stubs - not supported in this implementation)
    procedure DrawBox(X1, Y1, X2, Y2: Integer; Angle: Double = 0);
    procedure DrawBoxStyled(X1, Y1, X2, Y2: Integer; const Style: TShapeStyle; Angle: Double = 0);
    procedure DrawBoxWithColor(X1, Y1, X2, Y2: Integer; Color: UInt32; Angle: Double = 0; Filled: Boolean = False);
    procedure DrawCircleWithColor(X, Y, XR, YR: Integer; Color: UInt32;
                                  SA: Double = 0; EA: Double = 360;
                                  Angle: Double = 0; Inc: Double = 2);
    procedure DrawLine(X1, Y1, X2, Y2: Integer; Color: UInt32);
    procedure DrawLineInternal(X1, Y1, X2, Y2: Integer;
                               UseIndex: Boolean; PalIndex: TPaletteIndex; RGBAColor: UInt32);
    procedure SetPixelCursor(X, Y: Integer);
    function GetPixelCursorX: Integer;
    function GetPixelCursorY: Integer;
    procedure SetBorderStyle(const Style: TBorderStyle);
    procedure SetFillStyle(const Style: TFillStyleDef);
    function GetBorderStyle: TBorderStyle;
    function GetFillStyle: TFillStyleDef;

    // COLOR command support
    procedure SetColorSource(Source, Color: Integer);
    procedure SetColorSourceDirect(Source, Color: Integer);
    function GetColorSource(Source: Integer): Integer;
    function GetColorSourceDirect(Source: Integer): Integer;
    // WIDTH command support
    procedure SetLineWidth(Width: Integer);
    // SCALE command support
    procedure SetScale(Enabled: Boolean; XMax, YMax: Integer);
    // PAINT command support
    procedure FloodFill(Source: Integer; X, Y: Double; Mode: Integer);
    // WINDOW command support
    procedure SetWindow(Col1, Row1, Col2, Row2: Integer; DoClear: Boolean);
    function GetWindowLines: Integer;
    function GetWindowCols: Integer;
    function GetScreenWidth: Integer;
    // Screen memory access (PEEK/POKE compatibility - only for C128 text modes)
    function GetCharAt(Col, Row: Integer): Byte;
    procedure SetCharAt(Col, Row: Integer; Ch: Byte);
    function GetColorAt(Col, Row: Integer): Byte;
    procedure SetColorAt(Col, Row: Integer; Color: Byte);
    // SSHAPE/GSHAPE command support
    function SaveShape(X1, Y1, X2, Y2: Double): string;
    procedure LoadShape(const Data: string; X, Y: Double; Mode: Integer);

    // === IGraphicsBackend (FreeBASIC graphics): operation-level methods delegating to this device's
    // existing drawing methods. Mapped via resolution clauses to avoid clashing with the IOutputDevice
    // methods of different signatures. This is the live graphics device for running programs on sbv. ===
    function  GBSetMode(Mode: TGraphicMode; ClearBuffer: Boolean; SplitLine: Integer): Boolean;
    function  GBResizeScreen(W, H, Depth: Integer): Boolean;
    function  GBGetMode: TGraphicMode;
    function  GBInGraphics: Boolean;
    procedure GBClearSurface(Surface: TGfxSurface; Color: TGfxColor);
    procedure GBPresent;
    function  GBScreenSurface: TGfxSurface;
    function  GBImageMem(Surface: TGfxSurface): TGraphicsMemory;   // image memory for a surface id, nil if invalid
    function  GBCreateSurface(W, H: Integer; Fill: TGfxColor): TGfxSurface;
    procedure GBDestroySurface(Surface: TGfxSurface);
    function  GBSurfaceWidth(Surface: TGfxSurface): Integer;
    function  GBSurfaceHeight(Surface: TGfxSurface): Integer;
    function  GBSurfaceData(Surface: TGfxSurface; out Data: PByte; out SizeBytes: Integer): Boolean;
    procedure GBSetPixel(Surface: TGfxSurface; X, Y: Integer; Color: TGfxColor);
    function  GBGetPixel(Surface: TGfxSurface; X, Y: Integer): TGfxColor;
    procedure GBDrawLine(Surface: TGfxSurface; X1, Y1, X2, Y2: Integer; Color: TGfxColor; LineWidth: Integer);
    procedure GBDrawLineStyled(Surface: TGfxSurface; X1, Y1, X2, Y2: Integer; Color: TGfxColor; Style: Word);
    procedure GBFillBorder(Surface: TGfxSurface; X, Y: Integer; Color, BorderColor: TGfxColor);
    procedure GBDrawRect(Surface: TGfxSurface; X1, Y1, X2, Y2: Integer; Color: TGfxColor; Filled: Boolean; LineWidth: Integer; Angle: Double);
    procedure GBDrawEllipse(Surface: TGfxSurface; CX, CY, RX, RY: Integer; Color: TGfxColor; StartAngle, EndAngle, RotationAngle, AngleStep: Double; LineWidth: Integer);
    procedure GBFill(Surface: TGfxSurface; X, Y: Integer; Color: TGfxColor);
    procedure GBSetClip(Surface: TGfxSurface; Active: Boolean; X1, Y1, X2, Y2: Integer);
    procedure GBBlit(Dst: TGfxSurface; X, Y: Integer; Src: TGfxSurface; Mode: TGfxBlitMode);
    procedure GBEnablePalette(Enable: Boolean);
    procedure GBSetPaletteColor(Index: TPaletteIndex; Color: TGfxColor);
    function  GBGetPaletteColor(Index: TPaletteIndex): TGfxColor;
    procedure GBResetPalette;
    function  IGraphicsBackend.SetMode = GBSetMode;
    function  IGraphicsBackend.ResizeScreen = GBResizeScreen;
    function  IGraphicsBackend.GetMode = GBGetMode;
    function  IGraphicsBackend.InGraphics = GBInGraphics;
    procedure IGraphicsBackend.ClearSurface = GBClearSurface;
    procedure IGraphicsBackend.Present = GBPresent;
    function  IGraphicsBackend.ScreenSurface = GBScreenSurface;
    function  IGraphicsBackend.CreateSurface = GBCreateSurface;
    procedure IGraphicsBackend.DestroySurface = GBDestroySurface;
    function  IGraphicsBackend.SurfaceWidth = GBSurfaceWidth;
    function  IGraphicsBackend.SurfaceHeight = GBSurfaceHeight;
    function  IGraphicsBackend.SurfaceData = GBSurfaceData;
    procedure IGraphicsBackend.SetPixel = GBSetPixel;
    function  IGraphicsBackend.GetPixel = GBGetPixel;
    procedure IGraphicsBackend.DrawLine = GBDrawLine;
    procedure IGraphicsBackend.DrawLineStyled = GBDrawLineStyled;
    procedure IGraphicsBackend.FillBorder = GBFillBorder;
    procedure IGraphicsBackend.DrawRect = GBDrawRect;
    procedure IGraphicsBackend.DrawEllipse = GBDrawEllipse;
    procedure IGraphicsBackend.Fill = GBFill;
    procedure IGraphicsBackend.SetClip = GBSetClip;
    procedure IGraphicsBackend.Blit = GBBlit;
    procedure IGraphicsBackend.EnablePalette = GBEnablePalette;
    procedure IGraphicsBackend.SetPaletteColor = GBSetPaletteColor;
    function  IGraphicsBackend.GetPaletteColor = GBGetPaletteColor;
    procedure IGraphicsBackend.ResetPalette = GBResetPalette;

    property CurrentMode: TGraphicMode read FCurrentMode;
    property ModeInfo: TGraphicModeInfo read FModeInfo;
    property ViewportWidth: Integer read FViewportWidth;
    property ViewportHeight: Integer read FViewportHeight;
    property ViewportX: Integer read FViewportX;
    property ViewportY: Integer read FViewportY;
    property FontSize: Integer read FFontSize;
    property CharWidth: Integer read FCharWidth;
    property CharHeight: Integer read FCharHeight;
    property Fullscreen: Boolean read FFullscreen;
    property PaletteManager: TPaletteManager read FPaletteManager;
    property Font: PTTF_Font read FFont;
    property Renderer: PSDL_Renderer read FRenderer;

    // SDL2 mode 11 colors (direct RGBA)
    property BGColorSDL2: TSDL_Color read FBGColorSDL2 write FBGColorSDL2;
    property FGColorSDL2: TSDL_Color read FFGColorSDL2 write FFGColorSDL2;

    // Classic modes (0-10) palette indices for drawing
    property ForegroundIndex: Byte read GetForegroundIndex write SetForegroundIndex;
    property BackgroundIndex: Byte read GetBackgroundIndex write SetBackgroundIndex;
    property MulticolorIndex1: Byte read GetMulticolorIndex1 write SetMulticolorIndex1;
    property MulticolorIndex2: Byte read GetMulticolorIndex2 write SetMulticolorIndex2;

    // Screen appearance colors (independent from drawing colors)
    property ScreenColorIndex: Byte read GetScreenColorIndex write SetScreenColorIndex;
    property BorderColorIndex: Byte read GetBorderColorIndex write SetBorderColorIndex;
    property TextColorIndex: Byte read GetTextColorIndex write SetTextColorIndex;

    // Text rendering flags
    property ReverseActive: Boolean read GetReverseActive write SetReverseActive;

    // Current effective colors (auto-selects based on mode)
    function GetFGColor: TSDL_Color;
    function GetBGColor: TSDL_Color;
    property FGColor: TSDL_Color read GetFGColor;
    property BGColor: TSDL_Color read GetBGColor;

  private
    FCursorX: Integer;
    FCursorY: Integer;
    FInitialized: Boolean;
    FInScrollMode: Boolean;
    FShouldQuit: Boolean;
    FFastMode: Boolean;
    FFastModeAlpha: Byte;
    FLastGfxPresentTick: UInt32;  // wall-clock throttle for MaybePresentGraphics

    // FAST mode (C128 2MHz mode emulation)
    procedure SetFastMode(Enabled: Boolean);
    function GetFastMode: Boolean;
    procedure SetFastModeAlpha(Alpha: Byte);
    function GetFastModeAlpha: Byte;

  protected
    function SDLColorToColor(const SDLColor: TSDL_Color): TColor;
    function ColorToSDLColor(const Color: TColor): TSDL_Color;
  end;

  { TCharInfo - Character with position and color (sparse storage)
    Note: Row position is implicit (index of containing TTextRow)
    Only Col needs to be stored for sparse character positioning }
  TCharInfo = record
    Col: Byte;          // Column position (0-79), only X needed - Y is row index
    Ch: Char;           // The character
    FGIndex: Byte;      // Foreground color (palette index)
    BGIndex: Byte;      // Background color (palette index)
  end;

  { TTextRow - Single row with sparse character storage }
  TTextRow = record
    Chars: array[0..79] of TCharInfo;  // Fixed size array for max 80 chars
    Count: Integer;                     // Actual character count in this row
    Dirty: Boolean;                     // Row needs redraw
  end;

  { TTextCell - Single character cell with color attributes (LEGACY - being phased out) }
  TTextCell = record
    Ch: Char;           // The character
    FGIndex: Byte;      // Foreground color (palette index)
    BGIndex: Byte;      // Background color (palette index)
    Reverse: Boolean;   // Reverse video flag for this cell (DEPRECATED - use global state)
    Dirty: Boolean;     // True if cell needs redrawing
  end;
  PTextCell = ^TTextCell;

  { TTextLine - Single line of text }
  TTextLine = record
    Text: string;
  end;

  { TScrollbackRingBuffer - Efficient circular buffer for scrollback history
    O(1) for both Add and removal of oldest element }
  TScrollbackRingBuffer = class
  private
    FBuffer: array of string;
    FCapacity: Integer;
    FCount: Integer;
    FHead: Integer;  // Index where next item will be written
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    procedure Add(const Line: string);
    procedure Clear;
    function GetLine(Index: Integer): string;  // 0 = oldest, Count-1 = newest
    property Count: Integer read FCount;
    property Capacity: Integer read FCapacity;
  end;

  { TTextBuffer - Screen-like text buffer with scrollback and per-cell colors }
  TTextBuffer = class
  private
    FCells: array of array of TTextCell;  // [Row][Col] - cells with color attributes
    FRows: Integer;
    FCols: Integer;
    FCursorX: Integer;
    FCursorY: Integer;
    // Scrollback support - using efficient ring buffer
    FScrollbackBuffer: TScrollbackRingBuffer;
    FViewOffset: Integer;  // 0 = viewing current screen, >0 = viewing scrollback

    // Current text colors (used when writing new characters)
    FCurrentFGIndex: Byte;
    FCurrentBGIndex: Byte;
    FCurrentReverse: Boolean;

    // Dirty tracking for efficient rendering
    FRowDirty: array of Boolean;   // Per-row dirty flags
    FAllDirty: Boolean;            // True = redraw everything
    FScrollPending: Boolean;       // True = hardware scroll needed
    FScrollDirection: Integer;     // +1 = scroll up (new lines), -1 = scroll down (scrollback)

    // View window for hardware-like scrolling (scroll = shift view, not data)
    FFirstVisibleRow: Integer;     // Which buffer row is at screen top (for hardware scroll)
    FTotalBufferRows: Integer;     // Total rows in logical buffer (>= visible rows)

    // Window boundaries (WINDOW command) - coordinates within FCells
    FWindowCol1: Integer;  // Left edge of window (0-based)
    FWindowRow1: Integer;  // Top edge of window (0-based)
    FWindowCol2: Integer;  // Right edge of window (inclusive)
    FWindowRow2: Integer;  // Bottom edge of window (inclusive)

    procedure AddToScrollback(Row: Integer);
    function CellsToString(Row: Integer): string;  // Convert cell row to string (for scrollback)

  public
    constructor Create(Rows, Cols: Integer);
    destructor Destroy; override;

    procedure Clear;
    procedure ResetPrintState;  // Reset reverse mode after PRINT (C128 behavior)
    procedure PutChar(Ch: Char);
    procedure InsertChar(Ch: Char);  // Insert mode: shifts chars right
    procedure PutString(const S: string);
    procedure PutStringNoWrap(const S: string);  // Put string without wrapping to next line
    procedure NewLine;
    procedure DeleteChar;  // Backspace: delete char before cursor
    procedure DeleteCharAtCursor;  // Delete: delete char at cursor
    procedure ScrollUp;
    procedure ClearCurrentLine;  // Clears the current line

    // Cursor movement
    procedure CursorLeft;
    procedure CursorRight;
    procedure CursorHome;
    procedure CursorEnd;
    procedure CursorWordLeft;   // Ctrl+Left: jump to previous word
    procedure CursorWordRight;  // Ctrl+Right: jump to next word

    // Line editing (Ctrl combinations)
    procedure DeleteToLineStart;   // Ctrl+Home: delete from cursor to start
    procedure DeleteToLineEnd;     // Ctrl+End / Ctrl+K: delete from cursor to end
    procedure DeleteWordRight;     // Ctrl+Delete: delete word right
    procedure DeleteWordLeft;      // Ctrl+Backspace: delete word left
    procedure DeleteEntireLine;    // Ctrl+U: delete entire line

    function GetLine(Index: Integer): string;
    function GetCurrentLine: string;
    function GetCharAtCursor: Char;  // Returns char under cursor (or space)
    function GetCell(Row, Col: Integer): TTextCell;  // Get cell with color attributes
    function GetViewCell(Row, Col: Integer): TTextCell;  // Get cell considering scrollback offset
    procedure SetCell(Row, Col: Integer; const Cell: TTextCell);  // Set cell with color attributes
    function GetCharAt(Col, Row: Integer): Byte;  // Get character at position (PETSCII)
    procedure SetCharAt(Col, Row: Integer; Ch: Byte);  // Set character at position (PETSCII)
    function GetColorAt(Col, Row: Integer): Byte;  // Get foreground color at position
    procedure SetColorAt(Col, Row: Integer; Color: Byte);  // Set foreground color at position

    // Scrollback navigation
    procedure ScrollViewUp(NumLines: Integer = 1);
    procedure ScrollViewDown(NumLines: Integer = 1);
    procedure ScrollToEnd;
    procedure ScrollToStart;
    function GetViewLine(Index: Integer): string;  // Get line considering view offset
    function IsInScrollbackMode: Boolean;

    // Window management (WINDOW command)
    procedure Resize(NewRows, NewCols: Integer);  // Resize buffer for mode switch
    procedure SetWindow(Col1, Row1, Col2, Row2: Integer);
    procedure ResetWindow;  // Reset to full screen
    procedure ClearWindow;  // Clear only within current window
    function GetWindowLines: Integer;  // RWINDOW(0)
    function GetWindowCols: Integer;   // RWINDOW(1)

    // Color management
    procedure ReplaceBGIndex(OldIndex, NewIndex: Byte);  // Replace background in all cells
    procedure ReplaceFGIndex(OldIndex, NewIndex: Byte);  // Replace foreground in all cells

    // Dirty tracking methods for efficient rendering
    procedure MarkRowDirty(Row: Integer);       // Mark single row for redraw
    procedure MarkAllDirty;                     // Mark entire buffer for redraw
    procedure ClearDirtyFlags;                  // Clear all dirty flags after render
    function IsRowDirty(Row: Integer): Boolean; // Check if row needs redraw
    function NeedsRedraw: Boolean;              // Check if any content needs redraw

    // Current text color properties
    property CurrentFGIndex: Byte read FCurrentFGIndex write FCurrentFGIndex;
    property CurrentBGIndex: Byte read FCurrentBGIndex write FCurrentBGIndex;
    property CurrentReverse: Boolean read FCurrentReverse write FCurrentReverse;

    property CursorX: Integer read FCursorX write FCursorX;
    property CursorY: Integer read FCursorY write FCursorY;
    property Rows: Integer read FRows;
    property Cols: Integer read FCols;
    property ViewOffset: Integer read FViewOffset;
    property WindowCol1: Integer read FWindowCol1;
    property WindowRow1: Integer read FWindowRow1;
    property WindowCol2: Integer read FWindowCol2;
    property WindowRow2: Integer read FWindowRow2;
    property AllDirty: Boolean read FAllDirty;  // True if full redraw needed
    property ScrollPending: Boolean read FScrollPending write FScrollPending;  // Hardware scroll needed
    property ScrollDirection: Integer read FScrollDirection write FScrollDirection;  // +1=up, -1=down
    function GetScrollbackCount: Integer;
  end;

  { Forward declaration }
  TSedaiNewConsole = class;

  { TInputHandler - SDL2 event handling }
  { NOTE: Using TObject instead of TInterfacedObject because interfaces use CORBA mode (no refcount) }
  TInputHandler = class(TObject, IInputDevice)
  private
    FQuitRequested: Boolean;
    FStopRequested: Boolean;
    FLastKeyDown: TSDL_KeyCode;
    FLastKeyMod: UInt16;
    FLastChar: Char;
    FHasChar: Boolean;
    FShiftPressed: Boolean;
    FCtrlPressed: Boolean;
    FAltPressed: Boolean;
    FTextBuffer: TTextBuffer;
    FVideoController: TVideoController;
    FConsole: TSedaiNewConsole;  // Reference to console for RenderScreen

  public
    constructor Create(ATextBuffer: TTextBuffer; AVideoController: TVideoController; AConsole: TSedaiNewConsole);
    destructor Destroy; override;

    procedure ProcessEvents;
    procedure ClearFlags;
    procedure ClearLastKeyDown;  // Clear last key to prevent repeat issues

    // IInputDevice implementation
    function ReadLine(const Prompt: string = ''; IsCommand: Boolean = True; NumericOnly: Boolean = False; AllowDecimal: Boolean = True): string;
    function ReadKey: Char;
    function KeyPressed: Boolean;
    function HasChar: Boolean;           // Non-blocking character check
    function GetLastChar: string;        // Get and consume last character
    procedure EnableTextInput;           // Enable SDL text input mode
    procedure DisableTextInput;          // Disable SDL text input mode
    function ShouldQuit: Boolean;
    function ShouldStop: Boolean;
    procedure ClearStopRequest;
    procedure Reset;

    property QuitRequested: Boolean read FQuitRequested;
    property StopRequested: Boolean read FStopRequested;
    property LastKeyDown: TSDL_KeyCode read FLastKeyDown;
    property LastChar: Char read FLastChar;
    property CharAvailable: Boolean read FHasChar;
    property ShiftPressed: Boolean read FShiftPressed;
    property CtrlPressed: Boolean read FCtrlPressed;
    property AltPressed: Boolean read FAltPressed;
  end;

  { TProgramInputHandler - Dedicated input handler for BASIC program execution
    This class handles input ONLY during VM execution (GETKEY, GET, INPUT).
    It has its own event loop that does NOT interfere with console input.
    Key differences from TInputHandler:
    - Does NOT call ClearFlags at start of ProcessEvents (preserves pending chars)
    - Does NOT call SDL_StopTextInput in DisableTextInput (keeps text input active)
    - Has dedicated handling for special keys (ESC=BREAK, function keys, etc.)
  }
  TProgramInputHandler = class(TObject, IInputDevice)
  private
    FQuitRequested: Boolean;
    FStopRequested: Boolean;
    FLastChar: string;           // UTF-8 string for multi-byte chars
    FHasChar: Boolean;
    FShiftPressed: Boolean;
    FCtrlPressed: Boolean;
    FAltPressed: Boolean;
    FVideoController: TVideoController;
    FOutputDevice: IOutputDevice;  // Output device for synchronized I/O
    FTextInputActive: Boolean;   // Track SDL text input state
    FOnCursorEnable: TCursorEnableCallback;  // Cursor toggle callback
    FRenderResetPending: Boolean;  // Set when SDL render targets are reset (ALT+TAB)
    FToggleFullscreenRequested: Boolean;  // Set on CTRL+F while a program is running
  public
    constructor Create(AVideoController: TVideoController; AOutputDevice: IOutputDevice = nil);
    destructor Destroy; override;

    // IInputDevice implementation
    function ReadLine(const Prompt: string = ''; IsCommand: Boolean = True; NumericOnly: Boolean = False; AllowDecimal: Boolean = True): string;
    function ReadKey: Char;
    function KeyPressed: Boolean;
    function HasChar: Boolean;
    function GetLastChar: string;
    procedure EnableTextInput;
    procedure DisableTextInput;
    procedure SetCursorCallback(ACallback: TCursorEnableCallback);
    function ShouldQuit: Boolean;
    function ShouldStop: Boolean;
    procedure ClearStopRequest;
    procedure ProcessEvents;
    procedure Reset;
    procedure SetOutputDevice(AOutputDevice: IOutputDevice);

    property QuitRequested: Boolean read FQuitRequested;
    property StopRequested: Boolean read FStopRequested;
    property ShiftPressed: Boolean read FShiftPressed;
    property CtrlPressed: Boolean read FCtrlPressed;
    property AltPressed: Boolean read FAltPressed;
    property RenderResetPending: Boolean read FRenderResetPending write FRenderResetPending;
    property ToggleFullscreenRequested: Boolean read FToggleFullscreenRequested write FToggleFullscreenRequested;
  end;

  { TGraphicEngine - Graphics rendering }
  TGraphicEngine = class
  private
    FVideoController: TVideoController;

  public
    constructor Create(AVideoController: TVideoController);
    destructor Destroy; override;

    procedure DrawText(X, Y: Integer; const Text: string; Color: TSDL_Color);
    procedure DrawChar(X, Y: Integer; Ch: Char; Color: TSDL_Color);
    procedure FillRect(X, Y, W, H: Integer; Color: TSDL_Color);
  end;

  { Callback type for RenderScreen - avoids circular reference }
  TRenderScreenProc = procedure of object;

  { TConsoleOutputAdapter - Adapter that writes to TextBuffer }
  { NOTE: Using TObject instead of TInterfacedObject because interfaces use CORBA mode (no refcount) }
  TConsoleOutputAdapter = class(TObject, IOutputDevice)
  private
    FTextBuffer: TTextBuffer;
    FVideoController: TVideoController;
    FRenderScreenProc: TRenderScreenProc;  // Callback for RenderScreen
    FInitialized: Boolean;
    FSuppressPresent: Boolean;  // When true, skip Present calls (for batch output)
  public
    constructor Create(ATextBuffer: TTextBuffer; AVideoController: TVideoController);
    procedure SetRenderCallback(AProc: TRenderScreenProc);
    procedure BeginBatchOutput;  // Suppress Present calls for batch output
    procedure EndBatchOutput;    // Resume Present calls and render once

    // IOutputDevice implementation
    function Initialize(const Title: string = ''; Width: Integer = 80; Height: Integer = 25): Boolean;
    procedure Shutdown;
    function IsInitialized: Boolean;
    procedure Print(const Text: string; ClearBackground: Boolean = False);
    procedure PrintLn(const Text: string; ClearBackground: Boolean = False);
    procedure NewLine;
    procedure Clear;
    procedure ResetPrintState;
    procedure SetCursor(X, Y: Integer);
    procedure MoveCursor(DeltaX, DeltaY: Integer);
    function GetCursorX: Integer;
    function GetCursorY: Integer;
    procedure ShowCursor(X, Y: Integer);
    procedure HideCursor(X, Y: Integer);
    procedure SetColors(Foreground, Background: TColor);
    procedure Present;
    procedure SetFullscreen(Enabled: Boolean);
    function IsFullscreen: Boolean;
    function ShouldQuit: Boolean;
    function GetActualCols: Integer;
    function GetActualRows: Integer;
    procedure MarkPromptRow;
    procedure OnUserInput;
    function HandleScrollKeys(Key, Modifiers: Integer): Boolean;
    procedure ProcessScrollInput;
    function GetInScrollMode: Boolean;
    function SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean = False; SplitLine: Integer = -1): Boolean;
    function GetGraphicMode: TGraphicMode;
    function IsInGraphicsMode: Boolean;
    procedure ClearScreen(Mode: Integer);
    procedure SetPixel(X, Y: Integer; RGB: UInt32); overload;
    procedure SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex); overload;
    function GetPixel(X, Y: Integer): UInt32;
    function GetPixelIndex(X, Y: Integer): TPaletteIndex;
    procedure EnablePalette(Enable: Boolean);
    function IsPaletteEnabled: Boolean;
    procedure SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
    procedure SetPaletteColorRGBA(Index: TPaletteIndex; R, G, B: Byte; A: Byte = 255);
    function GetPaletteColor(Index: TPaletteIndex): UInt32;
    procedure ResetPalette;
    procedure LoadPalettePreset(PresetId: Integer);
    function LoadPaletteFromJSON(const FileName: string): Boolean;
    function SavePaletteToJSON(const FileName: string): Boolean;
    function GetLastPaletteError: string;

    // Shape drawing (stubs - not supported in this implementation)
    procedure DrawBox(X1, Y1, X2, Y2: Integer; Angle: Double = 0);
    procedure DrawBoxStyled(X1, Y1, X2, Y2: Integer; const Style: TShapeStyle; Angle: Double = 0);
    procedure DrawBoxWithColor(X1, Y1, X2, Y2: Integer; Color: UInt32; Angle: Double = 0; Filled: Boolean = False);
    procedure DrawCircleWithColor(X, Y, XR, YR: Integer; Color: UInt32;
                                  SA: Double = 0; EA: Double = 360;
                                  Angle: Double = 0; Inc: Double = 2);
    procedure DrawLine(X1, Y1, X2, Y2: Integer; Color: UInt32);
    procedure DrawLineInternal(X1, Y1, X2, Y2: Integer;
                               UseIndex: Boolean; PalIndex: TPaletteIndex; RGBAColor: UInt32);
    procedure SetPixelCursor(X, Y: Integer);
    function GetPixelCursorX: Integer;
    function GetPixelCursorY: Integer;
    procedure SetBorderStyle(const Style: TBorderStyle);
    procedure SetFillStyle(const Style: TFillStyleDef);
    function GetBorderStyle: TBorderStyle;
    function GetFillStyle: TFillStyleDef;

    // FAST mode (C128 2MHz mode emulation) - delegates to VideoController
    procedure SetFastMode(Enabled: Boolean);
    function GetFastMode: Boolean;
    procedure SetFastModeAlpha(Alpha: Byte);
    function GetFastModeAlpha: Byte;

    // COLOR command support - delegates to VideoController
    procedure SetColorSource(Source, Color: Integer);
    procedure SetColorSourceDirect(Source, Color: Integer);
    function GetColorSource(Source: Integer): Integer;
    function GetColorSourceDirect(Source: Integer): Integer;
    // WIDTH command support
    procedure SetLineWidth(Width: Integer);
    // SCALE command support
    procedure SetScale(Enabled: Boolean; XMax, YMax: Integer);
    // PAINT command support
    procedure FloodFill(Source: Integer; X, Y: Double; Mode: Integer);
    // WINDOW command support
    procedure SetWindow(Col1, Row1, Col2, Row2: Integer; DoClear: Boolean);
    function GetWindowLines: Integer;
    function GetWindowCols: Integer;
    function GetScreenWidth: Integer;
    // Screen memory access (PEEK/POKE compatibility - only for C128 text modes)
    function GetCharAt(Col, Row: Integer): Byte;
    procedure SetCharAt(Col, Row: Integer; Ch: Byte);
    function GetColorAt(Col, Row: Integer): Byte;
    procedure SetColorAt(Col, Row: Integer; Color: Byte);
    // SSHAPE/GSHAPE command support
    function SaveShape(X1, Y1, X2, Y2: Double): string;
    procedure LoadShape(const Data: string; X, Y: Double; Mode: Integer);
  end;

  { TSedaiNewConsole - Main console }
  TSedaiNewConsole = class(TObject)
  private
    FVideoController: TVideoController;
    FTextBuffer: TTextBuffer;
    FGraphicEngine: TGraphicEngine;
    FInputHandler: TInputHandler;
    FProgramInputHandler: TProgramInputHandler;  // Dedicated input for VM execution
    FConsoleOutputAdapter: TConsoleOutputAdapter;
    FRunning: Boolean;
    FCursorVisible: Boolean;
    FCursorEnabled: Boolean;       // False = cursor suppressed (during VM execution)
    FLastCursorBlink: Cardinal;
    FInputHistory: array of string;
    FHistoryCount: Integer;
    FHistoryPos: Integer;
    FHistoryFile: string;    // Custom history file path
    FCtrlCEnabled: Boolean;  // If True, CTRL+C exits from the console

    // Components for command management
    FProgramMemory: TProgramMemory;
    FCommandRouter: TCommandRouter;

    // VM-based execution (replaces TSedaiExecutor)
    FBytecodeVM: TBytecodeVM;
    FMemoryMapper: TObject;  // Memory mapper for VM (owned, freed in destructor)
    FSpriteEngine: TC128SpriteEngine;  // Sprite rendering engine (owned)
    FImmediateCompiler: TImmediateCompiler;

    // Bytecode mode - when BLOAD is used instead of LOAD
    FBytecodeMode: Boolean;
    FLoadedBytecode: TBytecodeProgram;

    // Startup file support (command line)
    FStartupFile: string;
    FAutoRun: Boolean;

    // AUTO line numbering
    FAutoLineNumber: Integer;     // Current auto line number (0 = disabled)
    FAutoLineIncrement: Integer;  // Increment for auto line numbering

    // Input buffer for AUTO and EDIT commands
    FInputBuffer: string;         // Pre-filled input text
    FInputPos: Integer;           // Cursor position in pre-filled input

    // User input tracking - tracks if user actually typed something (not system output)
    FUserHasTyped: Boolean;       // True if user typed/edited since last ENTER

    // Debug mode (TRON/TROFF)
    FDebugMode: Boolean;          // True = compile with debug info, trace execution
    FTraceActive: Boolean;        // True = currently tracing (runtime flag)
    FDebugger: TSedaiDebugger;    // Debugger instance for breakpoints and stepping

    // Error tracking (EL, ER, ERR$)
    FLastErrorLine: Integer;      // EL - line number where last error occurred (0 = none)
    FLastErrorCode: Integer;      // ER - error code of last error (0 = none)
    FLastErrorMessage: string;    // ERR$ - error message of last error

    // READY. prompt tracking - ensures every command shows READY. exactly once
    FReadyPrinted: Boolean;

    // Deferred rendering during VM execution
    FLastVMRenderTick: UInt32;

    // File I/O for DOPEN/DCLOSE/PRINT#/INPUT#/GET#
    FFileHandles: array[1..15] of TFileStream;  // Open file handles (BASIC uses 1-15)
    FFileNames: array[1..15] of string;         // Filenames for open handles
    FFileModes: array[1..15] of string;         // Access mode for open handles

    procedure ShowSplashScreen;
    function GetSystemInfo: string;
    function GetArchitecture: string;
    function GetAvailableMemory: string;
    procedure ExecuteDirectCommand(const Command: string);
    procedure ExecuteConsoleCommand(AST: TASTNode);

    // Debugger callbacks
    procedure HandleDebuggerTrace(Sender: TObject; LineNumber: Integer);
    procedure HandleDebuggerPause(Sender: TObject; LineNumber: Integer; const Reason: string);
    function ExecuteGLIST: TStringList;  // Returns output lines
    procedure DisplayWithMore(Lines: TStringList);

    // File I/O helpers for LOAD/SAVE commands
    function ExtractFilename(const Statement: string): string;
    function GetFirstWord(const Statement: string): string;
    procedure ExecuteLoad(const Filename: string);
    procedure ExecuteSave(const Filename: string);
    function ExecuteDirectory(const Pattern: string): TStringList;
    procedure ExecuteCopy(const Source, Dest: string; Overwrite: Boolean);
    procedure ExecuteScratch(const Pattern: string; Force: Boolean; Silent: Boolean = False);
    procedure ExecuteRename(const OldName, NewName: string);
    procedure ExecuteVerify(const Filename: string);
    procedure ExecuteBLoad(const Filename: string);
    procedure ExecuteBSave(const Filename: string);
    procedure PrintReady;  // Prints READY. if not already printed during current command
    function AskYesNo(const Prompt: string): Boolean;
    function AskYesNoAll(const Prompt: string; var OverwriteAll: Boolean): Boolean;

    // Disk file I/O handlers for VM (DOPEN/DCLOSE/GET#/INPUT#/PRINT#)
    procedure HandleDiskFile(Sender: TBytecodeVM; const Command: string;
      Handle: Integer; const HandleName, Filename, Mode: string;
      var ErrorCode: Integer);
    procedure HandleFileData(Sender: TBytecodeVM; const Command: string;
      Handle: Integer; var Data: string; var ErrorCode: Integer);
    procedure CloseAllFileHandles;

    // History file support
    function GetHistoryFilePath: string;
    procedure LoadHistory;
    procedure SaveHistory;
    procedure SaveHistoryFull;  // Rewrites entire history file

    // Deferred rendering during VM execution
    function HandleVMEventPoll: Boolean;
    procedure BeginVMExecution;
    procedure EndVMExecution;
    procedure SetCursorEnabled(Enable: Boolean);

    // SPRDEF: modal interactive sprite editor (wired to FBytecodeVM.SpriteEditorCallback)
    function RunSpriteEditor(SpriteNum: Integer): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function Initialize(Mode: TGraphicMode; Fullscreen: Boolean = False): Boolean;
    procedure Run;
    procedure ProcessCommand(const Command: string);
    procedure RenderScreen;
    procedure UpdateCursor;
    procedure RefreshDisplay;  // Rebuild lost render targets and repaint (focus regain / ALT+TAB)
    procedure ToggleFullscreen;  // Toggle fullscreen and reconnect renderer-dependent subsystems
    function CheckCursorBlink: Boolean;  // Returns true if cursor state changed

    // Startup file support (passed via command line)
    procedure SetStartupFile(const AFileName: string; AAutoRun: Boolean);
    function SourceHasLineNumbers(const AFileName: string): Boolean;  // CLASSIC (true) vs FreeBASIC MODERN (false)
    procedure LoadModernSource(const AFileName: string);              // compile a FreeBASIC source straight to bytecode
    // History file support
    procedure SetHistoryFile(const AFileName: string);

    property CtrlCEnabled: Boolean read FCtrlCEnabled write FCtrlCEnabled;
    // Function key definition (for console expansion)
    function GetFunctionKeyDefinition(KeyNum: Integer): string;
  end;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  DateUtils;

{$IFDEF WINDOWS}
type
  TMemoryStatusEx = record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    ullTotalPhys: QWord;
    ullAvailPhys: QWord;
    ullTotalPageFile: QWord;
    ullAvailPageFile: QWord;
    ullTotalVirtual: QWord;
    ullAvailVirtual: QWord;
    ullAvailExtendedVirtual: QWord;
  end;

function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL; stdcall; external 'kernel32' name 'GlobalMemoryStatusEx';
{$ENDIF}

{ TVideoController }

var
  GVConJoy: array[0..15] of PSDL_Joystick;   // lazily-opened gaming devices (GETJOYSTICK/STICK/STRIG on sbv)

// Real-time key state for MULTIKEY on sbv (installed as GKeyDownProvider; SDL keyboard state is global).
function VConKeyDown(ATScanCode: Integer): Boolean;
var
  NumKeys, SdlSc: Integer;
  State: PUInt8;
begin
  Result := False;
  SDL_PumpEvents;
  State := SDL_GetKeyboardState(@NumKeys);
  if State = nil then Exit;
  SdlSc := ATScancodeToSDL(ATScanCode);
  Result := (SdlSc > 0) and (SdlSc < NumKeys) and ((State + SdlSc)^ <> 0);
end;

// GETMOUSE on sbv: window-relative position + FB button bitmask (installed as GGetMouseProvider).
// Returns False (FB: all -1, status 1) when the pointer is not over the graphics window.
function VConGetMouse(out X, Y, Wheel, Buttons: Integer): Boolean;
var
  sx, sy, w, h: Integer;
  st: UInt32;
  Focus: PSDL_Window;
begin
  X := -1; Y := -1; Wheel := 0; Buttons := 0;
  Result := False;
  SDL_PumpEvents;
  Focus := SDL_GetMouseFocus;
  if Focus = nil then Exit;
  st := SDL_GetMouseState(@sx, @sy);
  w := 0; h := 0;
  SDL_GetWindowSize(Focus, @w, @h);
  if (sx < 0) or (sy < 0) or (sx >= w) or (sy >= h) then Exit;
  X := sx; Y := sy;
  if (st and SDL_BUTTON_LMASK) <> 0 then Buttons := Buttons or 1;
  if (st and SDL_BUTTON_RMASK) <> 0 then Buttons := Buttons or 2;
  if (st and SDL_BUTTON_MMASK) <> 0 then Buttons := Buttons or 4;
  Result := True;
end;

// SETMOUSE on sbv: warp the pointer / toggle cursor visibility (-1 = no change on each field).
procedure VConSetMouse(X, Y, Visibility: Integer);
var
  Focus: PSDL_Window;
begin
  Focus := SDL_GetMouseFocus;
  if (Focus <> nil) and (X >= 0) and (Y >= 0) then
    SDL_WarpMouseInWindow(Focus, X, Y);
  if Visibility = 0 then SDL_ShowCursor(SDL_DISABLE)
  else if Visibility = 1 then SDL_ShowCursor(SDL_ENABLE);
end;

// GETJOYSTICK / STICK / STRIG on sbv: read gaming device `Id` (lazily opened). Fills the button bitmask
// and up to MaxAxes axis values (-1..1, or -1000 if absent). Returns False if the device is not present.
function VConGetJoystick(Id: Integer; out Buttons: Integer; Axes: PSingle; MaxAxes: Integer): Boolean;
var
  i, n: Integer;
begin
  Buttons := 0;
  for i := 0 to MaxAxes - 1 do Axes[i] := -1000.0;
  Result := False;
  if (Id < 0) or (Id > 15) then Exit;
  if SDL_WasInit(SDL_INIT_JOYSTICK) = 0 then SDL_InitSubSystem(SDL_INIT_JOYSTICK);
  if GVConJoy[Id] = nil then
  begin
    if Id >= SDL_NumJoysticks then Exit;
    GVConJoy[Id] := SDL_JoystickOpen(Id);
    if GVConJoy[Id] = nil then Exit;
  end;
  SDL_JoystickUpdate;
  n := SDL_JoystickNumAxes(GVConJoy[Id]);
  for i := 0 to MaxAxes - 1 do
    if i < n then Axes[i] := SDL_JoystickGetAxis(GVConJoy[Id], i) / 32767.0
    else Axes[i] := -1000.0;
  n := SDL_JoystickNumButtons(GVConJoy[Id]);
  for i := 0 to n - 1 do
    if (i < 32) and (SDL_JoystickGetButton(GVConJoy[Id], i) <> 0) then Buttons := Buttons or (1 shl i);
  Result := True;
end;

constructor TVideoController.Create;
begin
  inherited Create;
  FWindow := nil;
  FRenderer := nil;
  GKeyDownProvider := @VConKeyDown;     // sbv: MULTIKEY reads the live SDL keyboard state
  GGetMouseProvider := @VConGetMouse;   // sbv: GETMOUSE reads the live SDL mouse state
  GSetMouseProvider := @VConSetMouse;   // sbv: SETMOUSE warps / toggles the cursor
  GGetJoystickProvider := @VConGetJoystick;  // sbv: GETJOYSTICK/STICK/STRIG read SDL gaming devices
  FFont := nil;
  FPaletteManager := nil;
  FGraphicsMemory := nil;
  FGraphicsTexture := nil;
  FTextTexture := nil;
  FScrollTexture := nil;
  FTextTextureDirty := True;
  FLastBorderColorIndex := 255;  // Force initial border draw
  FSDLInitialized := False;
  FFullscreen := False;
  FVSync := True;

  // Default SDL2 mode 11 colors (C128-style RGB)
  FBGColorSDL2.r := DEFAULT_BG_R;
  FBGColorSDL2.g := DEFAULT_BG_G;
  FBGColorSDL2.b := DEFAULT_BG_B;
  FBGColorSDL2.a := 255;

  FFGColorSDL2.r := DEFAULT_FG_R;
  FFGColorSDL2.g := DEFAULT_FG_G;
  FFGColorSDL2.b := DEFAULT_FG_B;
  FFGColorSDL2.a := 255;

  // Default palette indices for classic modes (0-10) - drawing colors
  FForegroundIndex := DEFAULT_FG_INDEX;  // 13 = Light Green
  FBackgroundIndex := DEFAULT_BG_INDEX;  // 11 = Dark Grey
  FMulticolorIndex1 := 1;   // White
  FMulticolorIndex2 := 2;   // Red

  // Default screen appearance colors
  FScreenColorIndex := DEFAULT_BG_INDEX;  // 11 = Dark Grey (viewport background)
  FBorderColorIndex := DEFAULT_FG_INDEX;  // 13 = Light Green (border/frame)
  FTextColorIndex := DEFAULT_FG_INDEX;    // 13 = Light Green (text/cursor)

  // Default text rendering flags
  FReverseActive := False;

  // Palette cache - will be filled on first use
  FPaletteCacheValid := False;

  // FAST mode (C128 2MHz mode emulation) - default off, alpha 255 (opaque)
  FFastMode := False;
  FFastModeAlpha := 255;

  // Throttle for MaybePresentGraphics (0 = present on first draw)
  FLastGfxPresentTick := 0;

  // COLOR command - initialize color sources (must match FxxxIndex values above)
  FColorSources[0] := FBackgroundIndex;    // Background (11 = Dark Grey)
  FColorSources[1] := FForegroundIndex;    // Foreground (13 = Light Green)
  FColorSources[2] := FMulticolorIndex1;   // Multicolor 1 (1 = White)
  FColorSources[3] := FMulticolorIndex2;   // Multicolor 2 (2 = Red)
  FColorSources[4] := FBorderColorIndex;   // Border (13 = Light Green)
  FColorSources[5] := FTextColorIndex;     // Character color (13 = Light Green)
  FColorSources[6] := FScreenColorIndex;   // 80-column background (11 = Dark Grey)

  // WIDTH command - default line width
  FLineWidth := 1;

  // SCALE command - disabled by default
  FScaleEnabled := False;
  FScaleXMax := 320;
  FScaleYMax := 200;

  // WINDOW command - default to full screen (will be set in Initialize)
  FWindowCol1 := 0;
  FWindowRow1 := 0;
  FWindowCol2 := 39;
  FWindowRow2 := 24;

  // Create persistent graphics memory handler
  FGraphicsMemory := TGraphicsMemory.Create;
end;

// Getter/Setter for ForegroundIndex
function TVideoController.GetForegroundIndex: Byte;
begin
  Result := FForegroundIndex;
end;

procedure TVideoController.SetForegroundIndex(Value: Byte);
begin
  FForegroundIndex := Value;
end;

// Getter/Setter for BackgroundIndex
function TVideoController.GetBackgroundIndex: Byte;
begin
  Result := FBackgroundIndex;
end;

procedure TVideoController.SetBackgroundIndex(Value: Byte);
begin
  FBackgroundIndex := Value;
end;

// Getter/Setter for MulticolorIndex1
function TVideoController.GetMulticolorIndex1: Byte;
begin
  Result := FMulticolorIndex1;
end;

procedure TVideoController.SetMulticolorIndex1(Value: Byte);
begin
  FMulticolorIndex1 := Value;
end;

// Getter/Setter for MulticolorIndex2
function TVideoController.GetMulticolorIndex2: Byte;
begin
  Result := FMulticolorIndex2;
end;

procedure TVideoController.SetMulticolorIndex2(Value: Byte);
begin
  FMulticolorIndex2 := Value;
end;

// Getter/Setter for ScreenColorIndex
function TVideoController.GetScreenColorIndex: Byte;
begin
  Result := FScreenColorIndex;
end;

procedure TVideoController.SetScreenColorIndex(Value: Byte);
begin
  FScreenColorIndex := Value;
end;

// Getter/Setter for BorderColorIndex
function TVideoController.GetBorderColorIndex: Byte;
begin
  Result := FBorderColorIndex;
end;

procedure TVideoController.SetBorderColorIndex(Value: Byte);
begin
  FBorderColorIndex := Value;
end;

// Getter/Setter for TextColorIndex
function TVideoController.GetTextColorIndex: Byte;
begin
  Result := FTextColorIndex;
end;

procedure TVideoController.SetTextColorIndex(Value: Byte);
begin
  FTextColorIndex := Value;
end;

// Getter/Setter for ReverseActive
function TVideoController.GetReverseActive: Boolean;
begin
  Result := FReverseActive;
end;

procedure TVideoController.SetReverseActive(Value: Boolean);
begin
  FReverseActive := Value;
end;

// Convert palette index to SDL_Color (for border/text rendering in classic modes)
// Uses cached values for performance during text rendering
function TVideoController.PaletteIndexToSDLColor(Index: Byte): TSDL_Color;
var
  PaletteColor: UInt32;
  i: Integer;
begin
  // Build cache if not valid
  if not FPaletteCacheValid then
  begin
    if FGraphicsMemory <> nil then
    begin
      for i := 0 to 255 do
      begin
        PaletteColor := FGraphicsMemory.Palette[i];
        // Palette format is $AABBGGRR (see sedaigraphicsconfig.pas)
        FPaletteCache[i].r := PaletteColor and $FF;
        FPaletteCache[i].g := (PaletteColor shr 8) and $FF;
        FPaletteCache[i].b := (PaletteColor shr 16) and $FF;
        FPaletteCache[i].a := 255;
      end;
    end
    else
    begin
      // Fallback to black if no graphics memory
      for i := 0 to 255 do
      begin
        FPaletteCache[i].r := 0;
        FPaletteCache[i].g := 0;
        FPaletteCache[i].b := 0;
        FPaletteCache[i].a := 255;
      end;
    end;
    FPaletteCacheValid := True;
  end;

  Result := FPaletteCache[Index];
end;

// Get current foreground color (auto-selects based on mode)
function TVideoController.GetFGColor: TSDL_Color;
begin
  if FCurrentMode = gmSDL2Dynamic then
    Result := FFGColorSDL2
  else
    Result := PaletteIndexToSDLColor(FForegroundIndex);
end;

// Get current background color (auto-selects based on mode)
function TVideoController.GetBGColor: TSDL_Color;
begin
  if FCurrentMode = gmSDL2Dynamic then
    Result := FBGColorSDL2
  else
    Result := PaletteIndexToSDLColor(FBackgroundIndex);
end;

destructor TVideoController.Destroy;
var
  IdxFreeImg: Integer;
begin
  // Destroy graphics texture first (needs renderer)
  DestroyGraphicsTexture;
  // Cleanup SDL resources (renderer, window, etc.)
  CleanupSDL;
  // Free graphics memory last (Pascal object, not SDL)
  if Assigned(FGraphicsMemory) then
  begin
    FGraphicsMemory.Free;
    FGraphicsMemory := nil;
  end;
  // Free FreeBASIC image surfaces (G3)
  for IdxFreeImg := 0 to High(FImageSurfaces) do
    FImageSurfaces[IdxFreeImg].Free;
  SetLength(FImageSurfaces, 0);
  inherited Destroy;
end;

function TVideoController.InitializeSDL: Boolean;
begin
  // When SDL is already up (e.g. a fullscreen/mode switch that destroys only the
  // window/renderer) skip re-init: re-calling SDL_Init/TTF_Init is unnecessary, and
  // the matching CleanupSDL(False) deliberately avoids SDL_Quit so the SAF audio
  // subsystem keeps running across the toggle.
  if FSDLInitialized then
  begin
    Result := True;
    Exit;
  end;

  Result := False;

  if SDL_Init(SDL_INIT_VIDEO) < 0 then
  begin
    WriteLn('SDL_Init failed: ', SDL_GetError);
    Exit;
  end;

  if TTF_Init < 0 then
  begin
    WriteLn('TTF_Init failed: ', SDL_GetError);
    SDL_Quit;
    Exit;
  end;

  // Set nearest-neighbor filtering for pixel-perfect rendering
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '0');

  // Force integer scaling to avoid stretching (SDL 2.0.5+)
  SDL_SetHint('SDL_RENDER_LOGICAL_SIZE_MODE', '1');

  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: SDL hints set - nearest neighbor + integer scaling');{$ENDIF}

  FSDLInitialized := True;
  Result := True;
end;

procedure TVideoController.CleanupSDL(FullShutdown: Boolean);
begin
  // Only cleanup if SDL was initialized
  if not FSDLInitialized then
    Exit;

  if Assigned(FFont) then
  begin
    TTF_CloseFont(FFont);
    FFont := nil;
  end;

  if Assigned(FPaletteManager) then
  begin
    FPaletteManager.Free;
    FPaletteManager := nil;
  end;

  // Destroy textures before renderer
  DestroyTextTexture;
  DestroyGraphicsTexture;

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

  // Only tear down the whole SDL library on a real shutdown. A fullscreen/mode
  // switch (CleanupSDL(False)) must keep SDL alive — SDL_Quit would also stop the
  // SAF audio subsystem, silencing audio after toggling fullscreen.
  if FullShutdown then
  begin
    TTF_Quit;
    SDL_Quit;
    FSDLInitialized := False;
  end;
end;

function TVideoController.CreateWindowAndRenderer: Boolean;
var
  Flags: UInt32;
  RequestedW, RequestedH: Integer;
  ActualW, ActualH: Integer;
  DisplayMode: TSDL_DisplayMode;
  NumModes, i: Integer;
  BestMode: TSDL_DisplayMode;
  NativeRatio, ModeRatio: Double;
  ScaleX, ScaleY, Scale: Integer;
begin
  Result := False;

  RequestedW := FWindowWidth;
  RequestedH := FWindowHeight;

  // If fullscreen, search for the best supported video mode using two-phase algorithm
  if FFullscreen then
  begin
    NumModes := SDL_GetNumDisplayModes(0);
    {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Display modes available: ', NumModes);{$ENDIF}

    // Get native display resolution
    if SDL_GetCurrentDisplayMode(0, @DisplayMode) = 0 then
    begin
      {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Native resolution: ', DisplayMode.w, 'x', DisplayMode.h);{$ENDIF}
    end
    else
    begin
      WriteLn('WARNING: Cannot get native display mode, using fallback');
      DisplayMode.w := 1920;
      DisplayMode.h := 1080;
    end;

    // Calculate native aspect ratio
    NativeRatio := DisplayMode.w / DisplayMode.h;
    {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Native aspect ratio: ', Format('%.4f', [NativeRatio]));{$ENDIF}

    BestMode.w := 0;
    BestMode.h := 0;
    BestMode.format := 0;
    BestMode.refresh_rate := 0;
    BestMode.driverdata := nil;

    // === PHASE 1: Search for resolution with uniform scaling (ScaleX == ScaleY) ===
    {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Phase 1 - Searching for uniform scaling...');{$ENDIF}
    for i := 0 to NumModes - 1 do
    begin
      if SDL_GetDisplayMode(0, i, @DisplayMode) = 0 then
      begin
        // Filter only modes with native aspect ratio
        ModeRatio := DisplayMode.w / DisplayMode.h;
        if Abs(ModeRatio - NativeRatio) < 0.01 then
        begin
          ScaleX := DisplayMode.w div 640;
          ScaleY := DisplayMode.h div 400;

          if (ScaleX = ScaleY) and (ScaleX >= 1) then
          begin
            BestMode := DisplayMode;
            {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Phase 1 SUCCESS - Found uniform scaling: ', DisplayMode.w, 'x', DisplayMode.h,
                    ' Scale=', ScaleX, ' (', ScaleX, 'x viewport fits)');{$ENDIF}
            Break;
          end;
        end;
      end;
    end;

    // === PHASE 2: Fallback with non-uniform scaling (min(ScaleX, ScaleY)) ===
    if BestMode.w = 0 then
    begin
      {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Phase 1 FAILED - No uniform scaling found');{$ENDIF}
      {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Phase 2 - Searching for best non-uniform scaling...');{$ENDIF}

      for i := 0 to NumModes - 1 do
      begin
        if SDL_GetDisplayMode(0, i, @DisplayMode) = 0 then
        begin
          // Filter only modes with native aspect ratio
          ModeRatio := DisplayMode.w / DisplayMode.h;
          if Abs(ModeRatio - NativeRatio) < 0.01 then
          begin
            ScaleX := DisplayMode.w div 640;
            ScaleY := DisplayMode.h div 400;

            if (ScaleX >= 1) and (ScaleY >= 1) then
            begin
              Scale := Min(ScaleX, ScaleY);
              BestMode := DisplayMode;
              {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Phase 2 SUCCESS - Using non-uniform scaling: ', DisplayMode.w, 'x', DisplayMode.h,
                      ' ScaleX=', ScaleX, ' ScaleY=', ScaleY, ' → Using Scale=', Scale);{$ENDIF}
              Break;
            end;
          end;
        end;
      end;
    end;

    // === FINAL FALLBACK: Use native resolution ===
    if BestMode.w = 0 then
    begin
      {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Phase 2 FAILED - Using native resolution as last resort');{$ENDIF}
      if SDL_GetCurrentDisplayMode(0, @BestMode) <> 0 then
      begin
        BestMode.w := 1920;
        BestMode.h := 1080;
        WriteLn('WARNING: Cannot get native mode, hardcoded to 1920x1080');
      end;
      ScaleX := BestMode.w div 640;
      ScaleY := BestMode.h div 400;
      Scale := Max(1, Min(ScaleX, ScaleY));
      {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Using native: ', BestMode.w, 'x', BestMode.h,
              ' ScaleX=', ScaleX, ' ScaleY=', ScaleY, ' → Scale=', Scale);{$ENDIF}
    end;

    {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Final mode selected: ', BestMode.w, 'x', BestMode.h, ' @ ', BestMode.refresh_rate, 'Hz');{$ENDIF}
    FWindowWidth := BestMode.w;
    FWindowHeight := BestMode.h;
  end;

  Flags := SDL_WINDOW_SHOWN;
  if FFullscreen then
    Flags := Flags or SDL_WINDOW_FULLSCREEN;

  FWindow := SDL_CreateWindow(
    'SedaiBasic v2.0',
    SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED,
    FWindowWidth,
    FWindowHeight,
    Flags
  );

  if not Assigned(FWindow) then
  begin
    WriteLn('SDL_CreateWindow failed: ', SDL_GetError);
    Exit;
  end;

  // Get actual window resolution
  SDL_GetWindowSize(FWindow, @ActualW, @ActualH);

  // Update with actual values
  FWindowWidth := ActualW;
  FWindowHeight := ActualH;

  if FVSync then
    FRenderer := SDL_CreateRenderer(FWindow, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC)
  else
    FRenderer := SDL_CreateRenderer(FWindow, -1, SDL_RENDERER_ACCELERATED);

  if not Assigned(FRenderer) then
  begin
    WriteLn('SDL_CreateRenderer failed: ', SDL_GetError);
    Exit;
  end;

  Result := True;
end;

procedure TVideoController.CalculateViewportAndFont;
var
  ScaleX, ScaleY, RawScale, FontStep, Scale: Integer;
  ScaledWidth, ScaledHeight: Integer;
  BaseFontSize: Integer;
begin
  // Guard against modes with no native resolution (SDL2 dynamic)
  if (FModeInfo.NativeWidth = 0) or (FModeInfo.NativeHeight = 0) then
    Exit;

  // Calculate base font size from native viewport and text grid
  if FModeInfo.TextCols > 0 then
    BaseFontSize := FModeInfo.NativeWidth div FModeInfo.TextCols
  else
    BaseFontSize := 8;  // Default for pure bitmap modes (no text grid)

  // Integer scaling to fill the window while maintaining aspect ratio
  // Same logic for both windowed and fullscreen — SDL handles pixel scaling
  ScaleX := FWindowWidth div FModeInfo.NativeWidth;
  ScaleY := FWindowHeight div FModeInfo.NativeHeight;
  RawScale := Min(ScaleX, ScaleY);
  if RawScale < 1 then RawScale := 1;

  // Round scale so that font size is a multiple of 8
  // FontStep = minimum scale increment for font-multiple-of-8 constraint
  FontStep := 8 div BaseFontSize;
  Scale := (RawScale div FontStep) * FontStep;
  if Scale < 1 then Scale := RawScale;  // Window too small for constraint, use raw

  FFontSize := BaseFontSize * Scale;
  ScaledWidth := FModeInfo.NativeWidth * Scale;
  ScaledHeight := FModeInfo.NativeHeight * Scale;

  // Center the scaled viewport in the window
  FViewportX := (FWindowWidth - ScaledWidth) div 2;
  FViewportY := (FWindowHeight - ScaledHeight) div 2;
  FViewportWidth := ScaledWidth;
  FViewportHeight := ScaledHeight;
end;

function TVideoController.TryLoadFont(const FontPath: string; Size: Integer): Boolean;
begin
  Result := False;

  if not FileExists(FontPath) then
    Exit;

  FFont := TTF_OpenFont(PChar(FontPath), Size);
  if not Assigned(FFont) then
    Exit;

  TTF_SetFontStyle(FFont, TTF_STYLE_NORMAL);
  TTF_SizeUTF8(FFont, 'W', @FCharWidth, @FCharHeight);

  Result := True;
end;

function TVideoController.LoadFontWithSize(Size: Integer): Boolean;
var
  i: Integer;
  ExePath, FontPath: string;
begin
  Result := False;

  // Get the executable path
  ExePath := ExtractFilePath(ParamStr(0));

  for i := Low(DEFAULT_FONT_PATHS) to High(DEFAULT_FONT_PATHS) do
  begin
    // Try first with the absolute path (relative to executable)
    FontPath := ExePath + DEFAULT_FONT_PATHS[i];
    if TryLoadFont(FontPath, Size) then
    begin
      Result := True;
      Exit;
    end;

    // Then try with the original relative path
    if TryLoadFont(DEFAULT_FONT_PATHS[i], Size) then
    begin
      Result := True;
      Exit;
    end;
  end;

  WriteLn('ERROR: No TTF font found!');
  WriteLn('Searched in: ', ExePath, DEFAULT_FONT_PATHS[0]);
end;

function TVideoController.Initialize(Mode: TGraphicMode; Fullscreen: Boolean = False): Boolean;
begin
  Result := False;
  FCurrentMode := Mode;
  FModeInfo := GRAPHICS_MODES[Mode];
  FFullscreen := Fullscreen;
  FInitialized := False;  // Reset initialization flag

  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Initializing VideoController...');{$ENDIF}
  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Mode=', Ord(Mode), ' Fullscreen=', Fullscreen);{$ENDIF}

  if not InitializeSDL then
  begin
    {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: SDL initialization failed');{$ENDIF}
    Exit;
  end;

  // Calculate window size
  // Always use WindowWidth/WindowHeight if specified (for the border)
  // Otherwise fallback to NativeWidth/NativeHeight (viewport size)
  if (FModeInfo.WindowWidth > 0) and (FModeInfo.WindowHeight > 0) then
  begin
    FWindowWidth := FModeInfo.WindowWidth;
    FWindowHeight := FModeInfo.WindowHeight;
  end
  else
  begin
    // Fallback: window = viewport (no border)
    FWindowWidth := FModeInfo.NativeWidth;
    FWindowHeight := FModeInfo.NativeHeight;
  end;

  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Window size set to ', FWindowWidth, 'x', FWindowHeight);{$ENDIF}

  if not CreateWindowAndRenderer then
  begin
    {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: CreateWindowAndRenderer failed');{$ENDIF}
    Exit;
  end;

  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Window and renderer created successfully');{$ENDIF}

  CalculateViewportAndFont;

  if not LoadFontWithSize(FFontSize) then
  begin
    {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: LoadFontWithSize failed');{$ENDIF}
    Exit;
  end;

  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Font loaded successfully');{$ENDIF}

  // The viewport remains fixed at 640x400 (80x50 characters 8x8)
  // Do not recalculate based on FCharWidth/FCharHeight

  FPaletteManager := TPaletteManager.Create(FModeInfo.PaletteType);

  // Create persistent text texture for dirty tracking optimization
  CreateTextTexture;

  ClearBorder;

  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Initialization complete');{$ENDIF}
  FInitialized := True;

  Result := True;
  FInitialized := True;
end;

function TVideoController.SetMode(Mode: TGraphicMode): Boolean;
var
  SavedPalette: TPaletteArray;
  PreservePalette: Boolean;
begin
  // CleanupSDL frees FPaletteManager and Initialize recreates a fresh default one.
  // Preserve a customized palette across the renderer rebuild when the target mode
  // uses the same palette type (a fullscreen toggle keeps the same mode), otherwise
  // PRST/SETCOLOR/PLOAD customizations would be lost on every CTRL+F.
  PreservePalette := Assigned(FPaletteManager) and
                     (GRAPHICS_MODES[Mode].PaletteType = FPaletteManager.PaletteType);
  if PreservePalette then
    FPaletteManager.SavePalette(SavedPalette);

  // Light cleanup: destroy only window/renderer/textures, keep SDL (and SAF audio)
  // alive — a full SDL_Quit here would silence audio after a fullscreen toggle.
  CleanupSDL(False);
  Result := Initialize(Mode, FFullscreen);

  if Result and PreservePalette and Assigned(FPaletteManager) then
    FPaletteManager.RestorePalette(SavedPalette);
end;

procedure TVideoController.ForceTextureRebuild;
begin
  if FRenderer = nil then Exit;
  // Recreate the persistent render-target text texture and force a full redraw.
  // On Windows (Direct3D) ALT+TAB / minimize triggers SDL_RENDER_TARGETS_RESET and
  // the render-target contents are lost; dirty-tracking would otherwise keep blitting
  // a now-black texture. CreateTextTexture also sets FTextTextureDirty := True.
  CreateTextTexture;
  // In classic graphics modes the graphics texture was destroyed (SetMode/device
  // reset) but its CPU buffer (FGraphicsMemory) survives — recreate it so the next
  // SyncGraphicsBufferToTexture repaints the bitmap instead of leaving it black.
  if (FCurrentMode <> gmSDL2Dynamic) and IsInGraphicsMode then
    CreateGraphicsTexture;
end;

procedure TVideoController.ToggleFullscreen;
var
  SavedMode: TGraphicMode;
begin
  SavedMode := FCurrentMode;
  FFullscreen := not FFullscreen;
  SetMode(SavedMode);
end;

procedure TVideoController.SetColors(BG, FG: TSDL_Color);
begin
  // SDL2 mode colors (for mode 11)
  FBGColorSDL2 := BG;
  FFGColorSDL2 := FG;
end;

procedure TVideoController.ClearBorder;
var
  BorderColor: TSDL_Color;
begin
  // Fills the entire window with the border color
  // Use palette index for classic modes, direct SDL2 color for mode 11
  if FCurrentMode = gmSDL2Dynamic then
    BorderColor := FFGColorSDL2
  else
    BorderColor := PaletteIndexToSDLColor(FBorderColorIndex);

  SDL_SetRenderDrawColor(FRenderer, BorderColor.r, BorderColor.g, BorderColor.b, BorderColor.a);
  SDL_RenderClear(FRenderer);
end;

procedure TVideoController.ClearViewport;
var
  ViewportRect: TSDL_Rect;
  LocalBGColor: TSDL_Color;
begin
  // Fills the viewport with the background color (source 0)
  ViewportRect.x := FViewportX;
  ViewportRect.y := FViewportY;
  ViewportRect.w := FViewportWidth;
  ViewportRect.h := FViewportHeight;

  // Use palette index for classic modes, direct SDL2 color for mode 11
  if FCurrentMode = gmSDL2Dynamic then
    LocalBGColor := FBGColorSDL2
  else
    LocalBGColor := PaletteIndexToSDLColor(FBackgroundIndex);

  SDL_SetRenderDrawColor(FRenderer, LocalBGColor.r, LocalBGColor.g, LocalBGColor.b, LocalBGColor.a);
  SDL_RenderFillRect(FRenderer, @ViewportRect);
end;

procedure TVideoController.Present;
var
  OverlayRect: TSDL_Rect;
begin
  // If FAST mode is active, draw black overlay with configurable alpha
  if FFastMode then
  begin
    OverlayRect.x := 0;
    OverlayRect.y := 0;
    OverlayRect.w := FWindowWidth;
    OverlayRect.h := FWindowHeight;
    SDL_SetRenderDrawBlendMode(FRenderer, SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(FRenderer, 0, 0, 0, FFastModeAlpha);
    SDL_RenderFillRect(FRenderer, @OverlayRect);
  end;

  SDL_RenderPresent(FRenderer);
end;

procedure TVideoController.RenderText(ATextBuffer: TTextBuffer);
var
  Row, Col, RenderX, RenderY: Integer;
  Cell: TTextCell;
  Surface: PSDL_Surface;
  CharTexture: PSDL_Texture;
  DestRect, CellBGRect, RowBGRect, ViewportRect: TSDL_Rect;
  ScrollSrcRect, ScrollDstRect: TSDL_Rect;  // For hardware scroll
  CellTextColor, CellBGColor, ScreenBG, BorderColor: TSDL_Color;
  CharStr: string;
  EffectiveFG, EffectiveBG: Byte;
  NeedTextureUpdate, BorderChanged: Boolean;
begin
  if not Assigned(ATextBuffer) or not Assigned(FFont) then Exit;

  // Check if texture exists
  if FTextTexture = nil then
  begin
    CreateTextTexture;
    if FTextTexture = nil then Exit;
  end;

  // Check if border color changed
  BorderChanged := FBorderColorIndex <> FLastBorderColorIndex;

  // Determine if we need to update the texture
  NeedTextureUpdate := FTextTextureDirty or BorderChanged or ATextBuffer.NeedsRedraw;

  if NeedTextureUpdate then
  begin
    // Set render target to text texture
    SDL_SetRenderTarget(FRenderer, FTextTexture);

    // Get colors
    ScreenBG := PaletteIndexToSDLColor(FBackgroundIndex);
    BorderColor := PaletteIndexToSDLColor(FBorderColorIndex);

    // Full redraw if AllDirty, texture just created, or border color changed
    if FTextTextureDirty or BorderChanged or ATextBuffer.AllDirty or (FModeInfo.ModeType = mtMixed) then
    begin
      // Fill entire texture with border color
      SDL_SetRenderDrawColor(FRenderer, BorderColor.r, BorderColor.g, BorderColor.b, 255);
      SDL_RenderClear(FRenderer);

      // Fill viewport area with background color
      ViewportRect.x := FViewportX;
      ViewportRect.y := FViewportY;
      ViewportRect.w := FViewportWidth;
      ViewportRect.h := FViewportHeight;
      // For mixed modes, only fill the text area at bottom (graphics covers the top)
      if FModeInfo.ModeType = mtMixed then
      begin
        ViewportRect.y := FViewportY + FViewportHeight - (FModeInfo.TextRows * FCharHeight);
        ViewportRect.h := FModeInfo.TextRows * FCharHeight;
      end;
      SDL_SetRenderDrawColor(FRenderer, ScreenBG.r, ScreenBG.g, ScreenBG.b, 255);
      SDL_RenderFillRect(FRenderer, @ViewportRect);

      // Render ALL cells (at viewport offset)
      RenderY := FViewportY;
      // For mixed modes, text starts at the bottom of the viewport
      if FModeInfo.ModeType = mtMixed then
        RenderY := FViewportY + FViewportHeight - (FModeInfo.TextRows * FCharHeight);
      for Row := 0 to ATextBuffer.Rows - 1 do
      begin
        RenderX := FViewportX;
        for Col := 0 to ATextBuffer.Cols - 1 do
        begin
          Cell := ATextBuffer.GetViewCell(Row, Col);

          // Skip spaces (background already cleared)
          if Cell.Ch <> ' ' then
          begin
            // Get effective colors (swap if reverse)
            if Cell.Reverse then
            begin
              // Reverse: visual FG = cell BG, visual BG = cell FG
              if Cell.BGIndex = BG_USE_DEFAULT then
                EffectiveFG := FBackgroundIndex  // Resolve default to actual BG color
              else
                EffectiveFG := Cell.BGIndex;
              EffectiveBG := Cell.FGIndex;
            end
            else
            begin
              EffectiveFG := Cell.FGIndex;
              EffectiveBG := Cell.BGIndex;  // May be BG_USE_DEFAULT
            end;

            CellTextColor := PaletteIndexToSDLColor(EffectiveFG);

            // Draw cell background only if cell has explicit override (not BG_USE_DEFAULT)
            if EffectiveBG <> BG_USE_DEFAULT then
            begin
              CellBGColor := PaletteIndexToSDLColor(EffectiveBG);
              CellBGRect.x := RenderX;
              CellBGRect.y := RenderY;
              CellBGRect.w := FCharWidth;
              CellBGRect.h := FCharHeight;
              SDL_SetRenderDrawColor(FRenderer, CellBGColor.r, CellBGColor.g, CellBGColor.b, 255);
              SDL_RenderFillRect(FRenderer, @CellBGRect);
            end;

            // Render character
            CharStr := Cell.Ch;
            Surface := TTF_RenderUTF8_Solid(FFont, PChar(CharStr), CellTextColor);
            if Assigned(Surface) then
            begin
              try
                CharTexture := SDL_CreateTextureFromSurface(FRenderer, Surface);
                if Assigned(CharTexture) then
                begin
                  try
                    DestRect.x := RenderX;
                    DestRect.y := RenderY;
                    DestRect.w := Surface^.w;
                    DestRect.h := Surface^.h;
                    SDL_RenderCopy(FRenderer, CharTexture, nil, @DestRect);
                  finally
                    SDL_DestroyTexture(CharTexture);
                  end;
                end;
              finally
                SDL_FreeSurface(Surface);
              end;
            end;
          end;

          RenderX := RenderX + FCharWidth;
        end;
        RenderY := RenderY + FCharHeight;
      end;

      FTextTextureDirty := False;
      FLastBorderColorIndex := FBorderColorIndex;
      // Full redraw makes any pending hardware scroll irrelevant
      ATextBuffer.ScrollPending := False;
      ATextBuffer.ScrollDirection := 0;
    end
    else
    begin
      // Set render target to text texture for partial update
      SDL_SetRenderTarget(FRenderer, FTextTexture);

      // Hardware scroll: if scroll pending, shift texture content by one row
      if ATextBuffer.ScrollPending and Assigned(FScrollTexture) then
      begin
        if ATextBuffer.ScrollDirection > 0 then
        begin
          // Scroll UP: content moves up, new line appears at bottom
          // Step 1: Copy viewport (excluding top row) to scratch
          ScrollSrcRect.x := FViewportX;
          ScrollSrcRect.y := FViewportY + FCharHeight;  // Start from row 1
          ScrollSrcRect.w := FViewportWidth;
          ScrollSrcRect.h := FViewportHeight - FCharHeight;
          ScrollDstRect.x := 0;
          ScrollDstRect.y := 0;
          ScrollDstRect.w := FViewportWidth;
          ScrollDstRect.h := FViewportHeight - FCharHeight;
          SDL_SetRenderTarget(FRenderer, FScrollTexture);
          SDL_RenderCopy(FRenderer, FTextTexture, @ScrollSrcRect, @ScrollDstRect);

          // Step 2: Copy from scratch to main texture at top
          SDL_SetRenderTarget(FRenderer, FTextTexture);
          ScrollSrcRect.x := 0;
          ScrollSrcRect.y := 0;
          ScrollDstRect.x := FViewportX;
          ScrollDstRect.y := FViewportY;
          SDL_RenderCopy(FRenderer, FScrollTexture, @ScrollSrcRect, @ScrollDstRect);

          // Step 3: Clear bottom row
          RowBGRect.x := FViewportX;
          RowBGRect.y := FViewportY + FViewportHeight - FCharHeight;
          RowBGRect.w := FViewportWidth;
          RowBGRect.h := FCharHeight;
          SDL_SetRenderDrawColor(FRenderer, ScreenBG.r, ScreenBG.g, ScreenBG.b, 255);
          SDL_RenderFillRect(FRenderer, @RowBGRect);
        end
        else
        begin
          // Scroll DOWN: content moves down, new line appears at top
          // Step 1: Copy viewport (excluding bottom row) to scratch
          ScrollSrcRect.x := FViewportX;
          ScrollSrcRect.y := FViewportY;  // Start from row 0
          ScrollSrcRect.w := FViewportWidth;
          ScrollSrcRect.h := FViewportHeight - FCharHeight;
          ScrollDstRect.x := 0;
          ScrollDstRect.y := 0;
          ScrollDstRect.w := FViewportWidth;
          ScrollDstRect.h := FViewportHeight - FCharHeight;
          SDL_SetRenderTarget(FRenderer, FScrollTexture);
          SDL_RenderCopy(FRenderer, FTextTexture, @ScrollSrcRect, @ScrollDstRect);

          // Step 2: Copy from scratch to main texture shifted down
          SDL_SetRenderTarget(FRenderer, FTextTexture);
          ScrollSrcRect.x := 0;
          ScrollSrcRect.y := 0;
          ScrollDstRect.x := FViewportX;
          ScrollDstRect.y := FViewportY + FCharHeight;  // Offset down by one row
          SDL_RenderCopy(FRenderer, FScrollTexture, @ScrollSrcRect, @ScrollDstRect);

          // Step 3: Clear top row
          RowBGRect.x := FViewportX;
          RowBGRect.y := FViewportY;
          RowBGRect.w := FViewportWidth;
          RowBGRect.h := FCharHeight;
          SDL_SetRenderDrawColor(FRenderer, ScreenBG.r, ScreenBG.g, ScreenBG.b, 255);
          SDL_RenderFillRect(FRenderer, @RowBGRect);
        end;

        // Reset scroll flag
        ATextBuffer.ScrollPending := False;
        ATextBuffer.ScrollDirection := 0;
      end;

      // Partial update: only render dirty rows (at viewport offset)
      RenderY := FViewportY;
      for Row := 0 to ATextBuffer.Rows - 1 do
      begin
        if ATextBuffer.IsRowDirty(Row) then
        begin
          // Clear row background
          RowBGRect.x := FViewportX;
          RowBGRect.y := RenderY;
          RowBGRect.w := ATextBuffer.Cols * FCharWidth;
          RowBGRect.h := FCharHeight;
          SDL_SetRenderDrawColor(FRenderer, ScreenBG.r, ScreenBG.g, ScreenBG.b, 255);
          SDL_RenderFillRect(FRenderer, @RowBGRect);

          // Render all cells in this row
          RenderX := FViewportX;
          for Col := 0 to ATextBuffer.Cols - 1 do
          begin
            Cell := ATextBuffer.GetViewCell(Row, Col);

            if Cell.Ch <> ' ' then
            begin
              if Cell.Reverse then
              begin
                if Cell.BGIndex = BG_USE_DEFAULT then
                  EffectiveFG := FBackgroundIndex
                else
                  EffectiveFG := Cell.BGIndex;
                EffectiveBG := Cell.FGIndex;
              end
              else
              begin
                EffectiveFG := Cell.FGIndex;
                EffectiveBG := Cell.BGIndex;
              end;

              CellTextColor := PaletteIndexToSDLColor(EffectiveFG);

              if EffectiveBG <> BG_USE_DEFAULT then
              begin
                CellBGColor := PaletteIndexToSDLColor(EffectiveBG);
                CellBGRect.x := RenderX;
                CellBGRect.y := RenderY;
                CellBGRect.w := FCharWidth;
                CellBGRect.h := FCharHeight;
                SDL_SetRenderDrawColor(FRenderer, CellBGColor.r, CellBGColor.g, CellBGColor.b, 255);
                SDL_RenderFillRect(FRenderer, @CellBGRect);
              end;

              CharStr := Cell.Ch;
              Surface := TTF_RenderUTF8_Solid(FFont, PChar(CharStr), CellTextColor);
              if Assigned(Surface) then
              begin
                try
                  CharTexture := SDL_CreateTextureFromSurface(FRenderer, Surface);
                  if Assigned(CharTexture) then
                  begin
                    try
                      DestRect.x := RenderX;
                      DestRect.y := RenderY;
                      DestRect.w := Surface^.w;
                      DestRect.h := Surface^.h;
                      SDL_RenderCopy(FRenderer, CharTexture, nil, @DestRect);
                    finally
                      SDL_DestroyTexture(CharTexture);
                    end;
                  end;
                finally
                  SDL_FreeSurface(Surface);
                end;
              end;
            end;

            RenderX := RenderX + FCharWidth;
          end;
        end;

        RenderY := RenderY + FCharHeight;
      end;
    end;

    // Clear dirty flags
    ATextBuffer.ClearDirtyFlags;

    // Reset render target to screen
    SDL_SetRenderTarget(FRenderer, nil);
  end;

  // Blit texture to screen (every frame - fast operation)
  if FModeInfo.ModeType = mtMixed then
  begin
    // For mixed modes, only blit the text area portion (don't cover graphics)
    ScrollSrcRect.x := FViewportX;
    ScrollSrcRect.y := FViewportY + FViewportHeight - (FModeInfo.TextRows * FCharHeight);
    ScrollSrcRect.w := FViewportWidth;
    ScrollSrcRect.h := FModeInfo.TextRows * FCharHeight;
    SDL_RenderCopy(FRenderer, FTextTexture, @ScrollSrcRect, @ScrollSrcRect);
  end
  else
    SDL_RenderCopy(FRenderer, FTextTexture, nil, nil);
end;

procedure TVideoController.SetFullscreen(Enabled: Boolean);
begin
  UpdateFullscreenMode(Enabled);
end;

function TVideoController.IsFullscreen: Boolean;
begin
  Result := FFullscreen;
end;

procedure TVideoController.UpdateFullscreenMode(NewState: Boolean);
begin
  if NewState <> FFullscreen then
  begin
    FFullscreen := NewState;
    SetMode(FCurrentMode);
  end;
end;

function TVideoController.SDLColorToColor(const SDLColor: TSDL_Color): TColor;
begin
  Result.R := SDLColor.r;
  Result.G := SDLColor.g;
  Result.B := SDLColor.b;
end;

function TVideoController.ColorToSDLColor(const Color: TColor): TSDL_Color;
begin
  Result.r := Color.R;
  Result.g := Color.G;
  Result.b := Color.B;
  Result.a := 255;
end;

{ IOutputDevice implementation }

function TVideoController.Initialize(const Title: string; Width: Integer; Height: Integer): Boolean;
begin
  Result := Initialize(gm80x50Text, False);
end;

procedure TVideoController.Shutdown;
begin
  CleanupSDL;
end;

function TVideoController.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

procedure TVideoController.Print(const Text: string; ClearBackground: Boolean);
var
  Surface: PSDL_Surface;
  Texture: PSDL_Texture;
  DestRect: TSDL_Rect;
  LocalTextColor, LocalBGColor: TSDL_Color;
begin
  if Text = '' then
    Exit;

  // Force a newline if we're still at position 0 and not the first line
  if (FCursorX = 0) and (FCursorY > 0) and not ClearBackground then
    Inc(FCursorY);

  // Get colors from palette for classic modes, SDL2 colors for mode 11
  if FCurrentMode = gmSDL2Dynamic then
  begin
    LocalTextColor := FFGColorSDL2;
    LocalBGColor := FBGColorSDL2;
  end
  else
  begin
    LocalTextColor := PaletteIndexToSDLColor(FForegroundIndex);
    LocalBGColor := PaletteIndexToSDLColor(FBackgroundIndex);
  end;

  Surface := TTF_RenderUTF8_Blended(FFont, PChar(Text), LocalTextColor);
  if not Assigned(Surface) then
    Exit;

  try
    Texture := SDL_CreateTextureFromSurface(FRenderer, Surface);
    if Assigned(Texture) then
    begin
      try
        DestRect.x := ViewportX + (FCursorX * CharWidth);
        DestRect.y := ViewportY + (FCursorY * CharHeight);
        DestRect.w := Surface^.w;
        DestRect.h := Surface^.h;

        // Clear background area
        if ClearBackground then
        begin
          SDL_SetRenderDrawColor(FRenderer, LocalBGColor.r, LocalBGColor.g, LocalBGColor.b, LocalBGColor.a);
          SDL_RenderFillRect(FRenderer, @DestRect);
        end;

        SDL_RenderCopy(FRenderer, Texture, nil, @DestRect);
        
        // Always update cursor position
        Inc(FCursorX, Length(Text));

        // Auto-wrap if exceeding viewport width
        if FCursorX >= FModeInfo.TextCols then
        begin
          FCursorX := 0;
          Inc(FCursorY);
        end;

        // Scroll if needed
        if FCursorY >= FModeInfo.TextRows then
        begin
          // TODO: Implement proper scrolling
          FCursorY := FModeInfo.TextRows - 1;
        end;
      finally
        SDL_DestroyTexture(Texture);
      end;
    end;
  finally
    SDL_FreeSurface(Surface);
  end;

  // Force immediate present to avoid flicker
  SDL_RenderPresent(FRenderer);
end;

procedure TVideoController.PrintLn(const Text: string; ClearBackground: Boolean);
begin
  Print(Text, ClearBackground);
  NewLine;
end;

procedure TVideoController.NewLine;
begin
  FCursorX := 0;
  Inc(FCursorY);
  if FCursorY >= FModeInfo.TextRows then
  begin
    // TODO: Implement scrolling
    FCursorY := FModeInfo.TextRows - 1;
  end;
end;

procedure TVideoController.Clear;
begin
  ClearViewport;
  FCursorX := 0;
  FCursorY := 0;
end;

procedure TVideoController.ResetPrintState;
begin
  // TVideoController doesn't own the text buffer - the reset is handled
  // by TConsoleOutputAdapter which delegates to TTextBuffer.ResetPrintState
end;

procedure TVideoController.SetCursor(X, Y: Integer);
begin
  FCursorX := X;
  FCursorY := Y;
end;

procedure TVideoController.MoveCursor(DeltaX, DeltaY: Integer);
begin
  Inc(FCursorX, DeltaX);
  Inc(FCursorY, DeltaY);
end;

function TVideoController.GetCursorX: Integer;
begin
  Result := FCursorX;
end;

function TVideoController.GetCursorY: Integer;
begin
  Result := FCursorY;
end;

procedure TVideoController.ShowCursor(X, Y: Integer);
var
  CursorRect: TSDL_Rect;
  CursorColor: TSDL_Color;
begin
  CursorRect.x := ViewportX + (X * CharWidth);
  CursorRect.y := ViewportY + (Y * CharHeight);
  CursorRect.w := CharWidth;
  CursorRect.h := CharHeight;

  if FCurrentMode = gmSDL2Dynamic then
    CursorColor := FFGColorSDL2
  else
    CursorColor := PaletteIndexToSDLColor(FForegroundIndex);

  SDL_SetRenderDrawColor(FRenderer, CursorColor.r, CursorColor.g, CursorColor.b, CursorColor.a);
  SDL_RenderFillRect(FRenderer, @CursorRect);
end;

procedure TVideoController.HideCursor(X, Y: Integer);
var
  CursorRect: TSDL_Rect;
  LocalBGColor: TSDL_Color;
begin
  CursorRect.x := ViewportX + (X * CharWidth);
  CursorRect.y := ViewportY + (Y * CharHeight);
  CursorRect.w := CharWidth;
  CursorRect.h := CharHeight;

  if FCurrentMode = gmSDL2Dynamic then
    LocalBGColor := FBGColorSDL2
  else
    LocalBGColor := PaletteIndexToSDLColor(FBackgroundIndex);

  SDL_SetRenderDrawColor(FRenderer, LocalBGColor.r, LocalBGColor.g, LocalBGColor.b, LocalBGColor.a);
  SDL_RenderFillRect(FRenderer, @CursorRect);
end;

procedure TVideoController.SetColors(Foreground, Background: TColor);
begin
  // This sets SDL2 mode colors
  FFGColorSDL2 := ColorToSDLColor(Foreground);
  FBGColorSDL2 := ColorToSDLColor(Background);
end;


function TVideoController.ShouldQuit: Boolean;
begin
  Result := FShouldQuit;
end;

function TVideoController.GetActualCols: Integer;
begin
  Result := FModeInfo.TextCols;
end;

function TVideoController.GetActualRows: Integer;
begin
  Result := FModeInfo.TextRows;
end;

procedure TVideoController.MarkPromptRow;
begin
  // Not implemented yet
end;

procedure TVideoController.OnUserInput;
begin
  // Not implemented yet
end;

function TVideoController.HandleScrollKeys(Key: Integer; Modifiers: Integer): Boolean;
begin
  Result := False;  // Not implemented yet
end;

procedure TVideoController.ProcessScrollInput;
begin
  // Not implemented yet
end;

function TVideoController.GetInScrollMode: Boolean;
begin
  Result := FInScrollMode;
end;

procedure TVideoController.SetFastMode(Enabled: Boolean);
begin
  FFastMode := Enabled;
  // Force immediate screen update to show/hide the black overlay
  Present;
end;

function TVideoController.GetFastMode: Boolean;
begin
  Result := FFastMode;
end;

procedure TVideoController.SetFastModeAlpha(Alpha: Byte);
begin
  FFastModeAlpha := Alpha;
end;

function TVideoController.GetFastModeAlpha: Byte;
begin
  Result := FFastModeAlpha;
end;

// === COLOR command support ===
procedure TVideoController.SetColorSource(Source, Color: Integer);
var
  PaletteIndex: Integer;
begin
  if (Source >= 0) and (Source <= 6) then
  begin
    // C128 COLOR command uses 1-based colors (1-16 for standard palette)
    // Convert to 0-based palette index: Color 1 = Black (index 0), etc.
    PaletteIndex := Color - 1;
    if PaletteIndex < 0 then PaletteIndex := 0;
    if PaletteIndex > 255 then PaletteIndex := 255;

    FColorSources[Source] := PaletteIndex;
    // Update corresponding palette indices based on source
    case Source of
      0: FBackgroundIndex := Byte(PaletteIndex);    // Background
      1: FForegroundIndex := Byte(PaletteIndex);    // Foreground
      2: FMulticolorIndex1 := Byte(PaletteIndex);   // Multicolor 1
      3: FMulticolorIndex2 := Byte(PaletteIndex);   // Multicolor 2
      4: FBorderColorIndex := Byte(PaletteIndex);   // Border
      5: FTextColorIndex := Byte(PaletteIndex);     // Character color
      6: FScreenColorIndex := Byte(PaletteIndex);   // 80-column background
    end;

  end;
end;

function TVideoController.GetColorSource(Source: Integer): Integer;
begin
  // Return 1-based color (inverse of SetColorSource)
  if (Source >= 0) and (Source <= 6) then
    Result := FColorSources[Source] + 1
  else
    Result := 1;  // Default to color 1 (Black)
end;

procedure TVideoController.SetColorSourceDirect(Source, Color: Integer);
var
  PaletteIndex: Integer;
begin
  // SETCOLOR: Sets color source directly (0-255)
  if (Source >= 0) and (Source <= 6) then
  begin
    PaletteIndex := Color and $FF;
    FColorSources[Source] := PaletteIndex;
    // Update corresponding palette indices based on source
    case Source of
      0: FBackgroundIndex := Byte(PaletteIndex);    // Background
      1: FForegroundIndex := Byte(PaletteIndex);    // Foreground
      2: FMulticolorIndex1 := Byte(PaletteIndex);   // Multicolor 1
      3: FMulticolorIndex2 := Byte(PaletteIndex);   // Multicolor 2
      4: FBorderColorIndex := Byte(PaletteIndex);   // Border
      5: FTextColorIndex := Byte(PaletteIndex);     // Character color
      6: FScreenColorIndex := Byte(PaletteIndex);   // 80-column background
    end;
  end;
end;

function TVideoController.GetColorSourceDirect(Source: Integer): Integer;
begin
  // RCLR/GETCOLOR: Returns color source (0-based)
  if (Source >= 0) and (Source <= 6) then
    Result := FColorSources[Source]
  else
    Result := 0;
end;

// === WIDTH command support ===
procedure TVideoController.SetLineWidth(Width: Integer);
begin
  // Allow values from 1 to 65535 (2^16-1)
  if (Width >= 1) and (Width <= 65535) then
    FLineWidth := Width
  else if Width < 1 then
    FLineWidth := 1
  else
    FLineWidth := 65535;
end;

// === SCALE command support ===
// Commodore 128 SCALE defaults per documentation:
// - Hires modes (320x200): default 1023x1023
// - Multicolor modes (160x200): default 2047x1023
// - Non-Commodore modes: default NativeWidth-1 x NativeHeight-1
// Max value is 2^32-1 (extended from C128's 32767)
procedure TVideoController.SetScale(Enabled: Boolean; XMax, YMax: Integer);
var
  DefaultXMax, DefaultYMax: Integer;
begin
  FScaleEnabled := Enabled;

  if Enabled then
  begin
    // Determine C128-compatible defaults based on mode resolution
    if (FModeInfo.NativeWidth = 320) and (FModeInfo.NativeHeight = 200) then
    begin
      // Hires C128 modes (GRAPHIC 0, 1, 2): default 1023x1023
      DefaultXMax := 1023;
      DefaultYMax := 1023;
    end
    else if (FModeInfo.NativeWidth = 160) and (FModeInfo.NativeHeight = 200) then
    begin
      // Multicolor C128 modes (GRAPHIC 3, 4): default 2047x1023
      DefaultXMax := 2047;
      DefaultYMax := 1023;
    end
    else
    begin
      // Non-Commodore modes: use NativeWidth-1 x NativeHeight-1
      DefaultXMax := FModeInfo.NativeWidth - 1;
      DefaultYMax := FModeInfo.NativeHeight - 1;
    end;

    // Use provided values or fall back to defaults
    if XMax > 0 then
      FScaleXMax := XMax
    else
      FScaleXMax := DefaultXMax;

    if YMax > 0 then
      FScaleYMax := YMax
    else
      FScaleYMax := DefaultYMax;
  end
  else
  begin
    // When disabled, reset to native resolution (no scaling)
    FScaleXMax := FModeInfo.NativeWidth;
    FScaleYMax := FModeInfo.NativeHeight;
  end;
end;

// === PAINT command support (flood fill) ===
procedure TVideoController.FloodFill(Source: Integer; X, Y: Double; Mode: Integer);
var
  PixelX, PixelY: Integer;
  TargetColor, FillColor: UInt32;
  FillIndex: Byte;
  Stack: array of TPoint;
  StackSize, StackCapacity: Integer;
  CurX, CurY: Integer;
  BufWidth, BufHeight: Integer;

  procedure PushPixel(PX, PY: Integer);
  begin
    if StackSize >= StackCapacity then
    begin
      StackCapacity := StackCapacity * 2;
      SetLength(Stack, StackCapacity);
    end;
    Stack[StackSize].X := PX;
    Stack[StackSize].Y := PY;
    Inc(StackSize);
  end;

  function PopPixel(out PX, PY: Integer): Boolean;
  begin
    if StackSize > 0 then
    begin
      Dec(StackSize);
      PX := Stack[StackSize].X;
      PY := Stack[StackSize].Y;
      Result := True;
    end
    else
      Result := False;
  end;

begin
  if FGraphicsMemory = nil then Exit;
  if not IsInGraphicsMode then Exit;

  // Apply coordinate scaling if enabled
  if FScaleEnabled then
  begin
    PixelX := Round(X * FModeInfo.NativeWidth / FScaleXMax);
    PixelY := Round(Y * FModeInfo.NativeHeight / FScaleYMax);
  end
  else
  begin
    PixelX := Round(X);
    PixelY := Round(Y);
  end;

  BufWidth := FModeInfo.NativeWidth;
  BufHeight := FModeInfo.NativeHeight;

  // In split screen mode, clip flood fill to visible graphics area only
  if (FModeInfo.ModeType = mtMixed) and (FModeInfo.TextCols > 0) then
    BufHeight := FModeInfo.NativeHeight - FModeInfo.TextRows * (FModeInfo.NativeWidth div FModeInfo.TextCols);

  // Bounds check
  if (PixelX < 0) or (PixelX >= BufWidth) or
     (PixelY < 0) or (PixelY >= BufHeight) then Exit;

  // Get fill color index from source (use palette index directly)
  if (Source >= 0) and (Source <= 6) then
    FillIndex := Byte(FColorSources[Source])
  else
    FillIndex := FForegroundIndex;

  // Get fill color RGB from graphics memory palette (for comparison)
  FillColor := FGraphicsMemory.GetPaletteColor(FillIndex);

  // Get target color at seed point
  TargetColor := FGraphicsMemory.GetPixelRGBA(PixelX, PixelY);

  // Don't fill if colors are the same
  if TargetColor = FillColor then Exit;

  // Initialize stack for flood fill
  StackCapacity := 1024;
  SetLength(Stack, StackCapacity);
  StackSize := 0;

  // Seed the stack
  PushPixel(PixelX, PixelY);

  // Flood fill using stack-based algorithm
  while PopPixel(CurX, CurY) do
  begin
    // Skip if out of bounds
    if (CurX < 0) or (CurX >= BufWidth) or
       (CurY < 0) or (CurY >= BufHeight) then Continue;

    // Get current pixel color
    if Mode = 0 then
    begin
      // Mode 0: Fill connected area of same color as seed
      if FGraphicsMemory.GetPixelRGBA(CurX, CurY) <> TargetColor then Continue;
    end
    else
    begin
      // Mode 1: Fill until non-background color
      if FGraphicsMemory.GetPixelRGBA(CurX, CurY) = FillColor then Continue;
    end;

    // Set pixel using palette index directly (avoids RGB->index conversion issues)
    FGraphicsMemory.SetPixel(CurX, CurY, FillIndex);

    // Add neighbors to stack
    PushPixel(CurX + 1, CurY);
    PushPixel(CurX - 1, CurY);
    PushPixel(CurX, CurY + 1);
    PushPixel(CurX, CurY - 1);
  end;

  MaybePresentGraphics;  // throttled ~60fps present (see DrawBoxWithColor)
end;

// === WINDOW command support ===
procedure TVideoController.SetWindow(Col1, Row1, Col2, Row2: Integer; DoClear: Boolean);
begin
  // Validate and store window boundaries
  FWindowCol1 := Max(0, Min(Col1, FModeInfo.TextCols - 1));
  FWindowRow1 := Max(0, Min(Row1, FModeInfo.TextRows - 1));
  FWindowCol2 := Max(FWindowCol1, Min(Col2, FModeInfo.TextCols - 1));
  FWindowRow2 := Max(FWindowRow1, Min(Row2, FModeInfo.TextRows - 1));

  // Clear window if requested
  if DoClear then
  begin
    // TODO: Clear the text window area
  end;
end;

function TVideoController.GetWindowLines: Integer;
begin
  Result := FWindowRow2 - FWindowRow1 + 1;
end;

function TVideoController.GetWindowCols: Integer;
begin
  Result := FWindowCol2 - FWindowCol1 + 1;
end;

function TVideoController.GetScreenWidth: Integer;
begin
  Result := FModeInfo.TextCols;
end;

// === Screen memory access (PEEK/POKE compatibility) ===
// TVideoController doesn't have direct TextBuffer access.
// These are stubs - actual implementation is in TConsoleOutputAdapter.

function TVideoController.GetCharAt(Col, Row: Integer): Byte;
begin
  // Stub - handled by TConsoleOutputAdapter
  Result := 32; // Space
end;

procedure TVideoController.SetCharAt(Col, Row: Integer; Ch: Byte);
begin
  // Stub - handled by TConsoleOutputAdapter
end;

function TVideoController.GetColorAt(Col, Row: Integer): Byte;
begin
  // Stub - handled by TConsoleOutputAdapter
  Result := 0;
end;

procedure TVideoController.SetColorAt(Col, Row: Integer; Color: Byte);
begin
  // Stub - handled by TConsoleOutputAdapter
end;

// === SSHAPE command support (save bitmap area to string) ===
function TVideoController.SaveShape(X1, Y1, X2, Y2: Double): string;
var
  PX1, PY1, PX2, PY2: Integer;
  Width, Height: Integer;
  X, Y: Integer;
  Pixel: UInt32;
  DataLen: Integer;
begin
  Result := '';
  if FGraphicsMemory = nil then Exit;
  if not IsInGraphicsMode then Exit;

  // Apply coordinate scaling if enabled
  if FScaleEnabled then
  begin
    PX1 := Round(X1 * FModeInfo.NativeWidth / FScaleXMax);
    PY1 := Round(Y1 * FModeInfo.NativeHeight / FScaleYMax);
    PX2 := Round(X2 * FModeInfo.NativeWidth / FScaleXMax);
    PY2 := Round(Y2 * FModeInfo.NativeHeight / FScaleYMax);
  end
  else
  begin
    PX1 := Round(X1);
    PY1 := Round(Y1);
    PX2 := Round(X2);
    PY2 := Round(Y2);
  end;

  // Ensure proper ordering
  if PX1 > PX2 then begin X := PX1; PX1 := PX2; PX2 := X; end;
  if PY1 > PY2 then begin Y := PY1; PY1 := PY2; PY2 := Y; end;

  // Clamp to buffer bounds
  PX1 := Max(0, PX1);
  PY1 := Max(0, PY1);
  PX2 := Min(FModeInfo.NativeWidth - 1, PX2);
  PY2 := Min(FModeInfo.NativeHeight - 1, PY2);

  Width := PX2 - PX1 + 1;
  Height := PY2 - PY1 + 1;

  if (Width <= 0) or (Height <= 0) then Exit;

  // Format: 2 bytes width + 2 bytes height + RGBA data
  DataLen := 4 + Width * Height * 4;
  SetLength(Result, DataLen);

  // Store dimensions (little-endian)
  Result[1] := Chr(Width and $FF);
  Result[2] := Chr((Width shr 8) and $FF);
  Result[3] := Chr(Height and $FF);
  Result[4] := Chr((Height shr 8) and $FF);

  // Copy pixel data
  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
    begin
      Pixel := FGraphicsMemory.GetPixelRGBA(PX1 + X, PY1 + Y);
      Result[5 + (Y * Width + X) * 4] := Chr((Pixel shr 24) and $FF);  // R
      Result[6 + (Y * Width + X) * 4] := Chr((Pixel shr 16) and $FF);  // G
      Result[7 + (Y * Width + X) * 4] := Chr((Pixel shr 8) and $FF);   // B
      Result[8 + (Y * Width + X) * 4] := Chr(Pixel and $FF);           // A
    end;
end;

// === GSHAPE command support (load string to bitmap) ===
procedure TVideoController.LoadShape(const Data: string; X, Y: Double; Mode: Integer);
var
  PX, PY: Integer;
  Width, Height: Integer;
  SX, SY: Integer;
  SrcPixel, DstPixel, NewPixel: UInt32;
  DataIndex: Integer;
begin
  if FGraphicsMemory = nil then Exit;
  if not IsInGraphicsMode then Exit;
  if Length(Data) < 4 then Exit;

  // Apply coordinate scaling if enabled
  if FScaleEnabled then
  begin
    PX := Round(X * FModeInfo.NativeWidth / FScaleXMax);
    PY := Round(Y * FModeInfo.NativeHeight / FScaleYMax);
  end
  else
  begin
    PX := Round(X);
    PY := Round(Y);
  end;

  // Read dimensions from data
  Width := Ord(Data[1]) or (Ord(Data[2]) shl 8);
  Height := Ord(Data[3]) or (Ord(Data[4]) shl 8);

  if Length(Data) < 4 + Width * Height * 4 then Exit;

  // Copy pixel data with mode-specific blending
  for SY := 0 to Height - 1 do
    for SX := 0 to Width - 1 do
    begin
      // Bounds check
      if (PX + SX < 0) or (PX + SX >= FModeInfo.NativeWidth) or
         (PY + SY < 0) or (PY + SY >= FModeInfo.NativeHeight) then Continue;

      DataIndex := 5 + (SY * Width + SX) * 4;
      SrcPixel := (Ord(Data[DataIndex]) shl 24) or
                  (Ord(Data[DataIndex + 1]) shl 16) or
                  (Ord(Data[DataIndex + 2]) shl 8) or
                  Ord(Data[DataIndex + 3]);

      case Mode of
        0: NewPixel := SrcPixel;  // As-is
        1: NewPixel := not SrcPixel;  // Invert
        2: begin  // OR
             DstPixel := FGraphicsMemory.GetPixelRGBA(PX + SX, PY + SY);
             NewPixel := SrcPixel or DstPixel;
           end;
        3: begin  // AND
             DstPixel := FGraphicsMemory.GetPixelRGBA(PX + SX, PY + SY);
             NewPixel := SrcPixel and DstPixel;
           end;
        4: begin  // XOR
             DstPixel := FGraphicsMemory.GetPixelRGBA(PX + SX, PY + SY);
             NewPixel := SrcPixel xor DstPixel;
           end;
        else
          NewPixel := SrcPixel;
      end;

      FGraphicsMemory.SetPixelRGBA(PX + SX, PY + SY, NewPixel);
    end;

  MaybePresentGraphics;  // throttled ~60fps present (see DrawBoxWithColor)
end;

function TVideoController.SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean; SplitLine: Integer): Boolean;
var
  NewModeInfo: TGraphicModeInfo;
  PaletteMode: Boolean;
  BufferIsNew: Boolean;
begin
  Result := False;

  // Get mode info
  NewModeInfo := GRAPHICS_MODES[Mode];

  // Use user-provided SplitLine, or DefaultSplitLine if -1
  if SplitLine < 0 then
    FSplitLine := NewModeInfo.DefaultSplitLine
  else
    FSplitLine := SplitLine;

  // Update mode info
  FCurrentMode := Mode;
  FModeInfo := NewModeInfo;

  // Window size never changes after init — only the viewport is recalculated
  // to fit the new mode's native resolution within the existing window
  CalculateViewportAndFont;

  // Reload font at new size for text/mixed modes
  if NewModeInfo.ModeType in [mtText, mtMixed] then
  begin
    if Assigned(FFont) then
    begin
      TTF_CloseFont(FFont);
      FFont := nil;
    end;
    LoadFontWithSize(FFontSize);
  end;

  // Recreate text texture for new viewport/window dimensions
  if NewModeInfo.ModeType in [mtText, mtMixed] then
    CreateTextTexture;

  // For classic modes (0-10), use persistent buffers
  if Mode <> gmSDL2Dynamic then
  begin
    // Allocate or switch to the buffer for this mode
    // AllocateBuffers returns True if buffer was newly created
    PaletteMode := NewModeInfo.PaletteType = ptC64_16;
    BufferIsNew := FGraphicsMemory.AllocateBuffers(NewModeInfo.NativeWidth, NewModeInfo.NativeHeight, PaletteMode, Mode);

    // Clear the buffer if:
    // 1. Explicitly requested (ClearBuffer=True), OR
    // 2. Buffer was just created (first time entering this mode)
    if ClearBuffer or BufferIsNew then
      FGraphicsMemory.ClearCurrentModeWithIndex(FBackgroundIndex);

    // Create/recreate texture for this resolution
    CreateGraphicsTexture;
  end;

  // Update palette manager if palette type changed
  if Assigned(FPaletteManager) and (FPaletteManager.PaletteType <> NewModeInfo.PaletteType) then
    FPaletteManager.LoadDefaultPalette(NewModeInfo.PaletteType);

  // Per Commodore documentation: "The GRAPHIC command turns scaling off"
  // GRAPHIC (something) is equivalent to GRAPHIC...: SCALE 0
  FScaleEnabled := False;
  FScaleXMax := FModeInfo.NativeWidth;
  FScaleYMax := FModeInfo.NativeHeight;

  // Invalidate palette cache so ClearBorder/ClearViewport use fresh values
  FPaletteCacheValid := False;

  // Clear the border
  ClearBorder;

  // In graphics mode, render the graphics buffer; in text mode, clear viewport
  if IsInGraphicsMode then
    RenderGraphicsTexture
  else
    ClearViewport;

  Present;
  Result := True;
end;


function TVideoController.GetGraphicMode: TGraphicMode;
begin
  Result := FCurrentMode;
end;

function TVideoController.IsInGraphicsMode: Boolean;
begin
  // Bitmap modes are: 1, 2, 3, 4, 6, 7, 9, 10, 11 (SDL2)
  Result := FCurrentMode in [
    gmStandardBitmap, gmSplitBitmap,           // 320x200 bitmap
    gmMulticolorBitmap, gmSplitMulticolor,     // 160x200 multicolor
    gm80ColBitmap, gm80ColMixed,               // 640x200 bitmap
    gm80x50Bitmap, gm80x50Mixed,               // 640x400 bitmap
    gmSDL2Dynamic                              // SDL2 dynamic
  ];
end;

procedure TVideoController.ClearScreen(Mode: Integer);
var
  SavedMode: TGraphicMode;
begin
  // SCNCLR: -1 = clear current mode, 0-5 = clear specific mode buffer
  if FGraphicsMemory = nil then Exit;

  if Mode = -1 then
  begin
    // Clear current mode with background color
    FGraphicsMemory.ClearCurrentModeWithIndex(FBackgroundIndex);
  end
  else if (Mode >= 0) and (Mode <= 11) then
  begin
    // Clear specific mode buffer by temporarily switching
    SavedMode := FCurrentMode;
    FGraphicsMemory.SwitchToMode(TGraphicMode(Mode), True);
    FGraphicsMemory.ClearCurrentModeWithIndex(FBackgroundIndex);
    FGraphicsMemory.SwitchToMode(SavedMode, True);
  end;
end;

procedure TVideoController.SetPixel(X, Y: Integer; RGB: UInt32);
var
  ScaledX, ScaledY: Integer;
begin
  if FGraphicsMemory = nil then Exit;

  // Apply coordinate scaling if enabled
  if FScaleEnabled then
  begin
    ScaledX := Round(X * FModeInfo.NativeWidth / FScaleXMax);
    ScaledY := Round(Y * FModeInfo.NativeHeight / FScaleYMax);
  end
  else
  begin
    ScaledX := X;
    ScaledY := Y;
  end;

  FGraphicsMemory.SetPixel(ScaledX, ScaledY, RGB);
end;

procedure TVideoController.SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex);
var
  ScaledX, ScaledY: Integer;
begin
  if FGraphicsMemory = nil then Exit;

  // Apply coordinate scaling if enabled
  if FScaleEnabled then
  begin
    ScaledX := Round(X * FModeInfo.NativeWidth / FScaleXMax);
    ScaledY := Round(Y * FModeInfo.NativeHeight / FScaleYMax);
  end
  else
  begin
    ScaledX := X;
    ScaledY := Y;
  end;

  FGraphicsMemory.SetPixel(ScaledX, ScaledY, PaletteIndex);
end;

function TVideoController.GetPixel(X, Y: Integer): UInt32;
var
  ScaledX, ScaledY: Integer;
begin
  if FGraphicsMemory = nil then
  begin
    Result := 0;
    Exit;
  end;

  // Apply coordinate scaling if enabled
  if FScaleEnabled then
  begin
    ScaledX := Round(X * FModeInfo.NativeWidth / FScaleXMax);
    ScaledY := Round(Y * FModeInfo.NativeHeight / FScaleYMax);
  end
  else
  begin
    ScaledX := X;
    ScaledY := Y;
  end;

  Result := FGraphicsMemory.GetPixel(ScaledX, ScaledY);
end;

function TVideoController.GetPixelIndex(X, Y: Integer): TPaletteIndex;
var
  PalIndex: TPaletteIndex;
  ScaledX, ScaledY: Integer;
begin
  // RDOT(2) should return the color SOURCE (0-3), not the palette index
  // Compare pixel's palette index with color sources to determine which one matches
  Result := 0;  // Default to background source
  if FGraphicsMemory = nil then Exit;

  // Apply coordinate scaling if enabled
  if FScaleEnabled then
  begin
    ScaledX := Round(X * FModeInfo.NativeWidth / FScaleXMax);
    ScaledY := Round(Y * FModeInfo.NativeHeight / FScaleYMax);
  end
  else
  begin
    ScaledX := X;
    ScaledY := Y;
  end;

  PalIndex := FGraphicsMemory.GetPixelIndex(ScaledX, ScaledY);

  // Check which color source this palette index matches
  if PalIndex = FForegroundIndex then
    Result := 1
  else if PalIndex = FMulticolorIndex1 then
    Result := 2
  else if PalIndex = FMulticolorIndex2 then
    Result := 3
  else if PalIndex = FBackgroundIndex then
    Result := 0
  else
    // No match - return the raw palette index for non-standard colors
    Result := PalIndex;
end;

procedure TVideoController.EnablePalette(Enable: Boolean);
begin
  if FGraphicsMemory <> nil then
    FGraphicsMemory.EnablePalette(Enable);
end;

function TVideoController.IsPaletteEnabled: Boolean;
begin
  if FGraphicsMemory <> nil then
    Result := FGraphicsMemory.IsPaletteEnabled
  else
    Result := False;
end;

procedure TVideoController.SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
begin
  if FGraphicsMemory <> nil then
  begin
    FGraphicsMemory.SetPaletteColor(Index, RGB);
    FPaletteCacheValid := False;  // Invalidate cache
  end;
end;

function TVideoController.GetPaletteColor(Index: TPaletteIndex): UInt32;
begin
  if FGraphicsMemory <> nil then
    Result := FGraphicsMemory.GetPaletteColor(Index)
  else
    Result := 0;
end;

procedure TVideoController.ResetPalette;
begin
  if FGraphicsMemory <> nil then
  begin
    FGraphicsMemory.ResetPalette;
    FPaletteCacheValid := False;  // Invalidate cache
  end;
end;

procedure TVideoController.LoadPalettePreset(PresetId: Integer);
begin
  if FGraphicsMemory <> nil then
  begin
    FGraphicsMemory.LoadPalettePreset(PresetId);
    FPaletteCacheValid := False;  // Invalidate cache so the new colours show
  end;
end;

procedure TVideoController.SetPaletteColorRGBA(Index: TPaletteIndex; R, G, B: Byte; A: Byte = 255);
begin
  if FGraphicsMemory <> nil then
  begin
    FGraphicsMemory.SetPaletteColorRGBA(Index, R, G, B, A);
    FPaletteCacheValid := False;  // Invalidate cache
  end;
end;

function TVideoController.LoadPaletteFromJSON(const FileName: string): Boolean;
begin
  if FGraphicsMemory <> nil then
  begin
    Result := FGraphicsMemory.LoadPaletteFromJSON(FileName);
    if Result then
      FPaletteCacheValid := False;  // Invalidate cache on successful load
  end
  else
    Result := False;
end;

function TVideoController.SavePaletteToJSON(const FileName: string): Boolean;
begin
  if FGraphicsMemory <> nil then
    Result := FGraphicsMemory.SavePaletteToJSON(FileName)
  else
    Result := False;
end;

function TVideoController.GetLastPaletteError: string;
begin
  if FGraphicsMemory <> nil then
    Result := FGraphicsMemory.GetLastPaletteError
  else
    Result := 'Graphics memory not initialized';
end;

// Shape drawing stubs - TVideoController
procedure TVideoController.DrawBox(X1, Y1, X2, Y2: Integer; Angle: Double);
begin
  // Not implemented
end;

procedure TVideoController.DrawBoxStyled(X1, Y1, X2, Y2: Integer; const Style: TShapeStyle; Angle: Double);
begin
  // Not implemented
end;

procedure TVideoController.DrawBoxWithColor(X1, Y1, X2, Y2: Integer; Color: UInt32; Angle: Double; Filled: Boolean);
var
  MinX, MinY, MaxX, MaxY: Integer;
  X, Y: Integer;
  ActualIndex: TPaletteIndex;  // Use TPaletteIndex (0..255) to force correct SetPixel overload
  UseIndex: Boolean;
  ScaledX1, ScaledY1, ScaledX2, ScaledY2: Integer;
begin
  if FGraphicsMemory = nil then Exit;

  // Apply coordinate scaling if enabled
  if FScaleEnabled then
  begin
    ScaledX1 := Round(X1 * FModeInfo.NativeWidth / FScaleXMax);
    ScaledY1 := Round(Y1 * FModeInfo.NativeHeight / FScaleYMax);
    ScaledX2 := Round(X2 * FModeInfo.NativeWidth / FScaleXMax);
    ScaledY2 := Round(Y2 * FModeInfo.NativeHeight / FScaleYMax);
  end
  else
  begin
    ScaledX1 := X1;
    ScaledY1 := Y1;
    ScaledX2 := X2;
    ScaledY2 := Y2;
  end;

  // For classic modes (0-10), use palette indices directly
  // For mode 11 (SDL2 dynamic), use RGBA values
  UseIndex := (FCurrentMode <> gmSDL2Dynamic);

  if UseIndex then
  begin
    // Classic modes (0-10): Color parameter selects which palette index to use
    // 0 = BackgroundIndex, 1 = ForegroundIndex, 2 = MulticolorIndex1, 3 = MulticolorIndex2
    case Color of
      0: ActualIndex := FBackgroundIndex;
      1: ActualIndex := FForegroundIndex;
      2: ActualIndex := FMulticolorIndex1;
      3: ActualIndex := FMulticolorIndex2;
    else
      // For values > 3, use as direct palette index (advanced usage)
      if Color < 256 then
        ActualIndex := TPaletteIndex(Color)
      else
        ActualIndex := FForegroundIndex;
    end;
  end;

  // Normalize coordinates (using scaled values)
  if ScaledX1 < ScaledX2 then begin MinX := ScaledX1; MaxX := ScaledX2; end
  else begin MinX := ScaledX2; MaxX := ScaledX1; end;
  if ScaledY1 < ScaledY2 then begin MinY := ScaledY1; MaxY := ScaledY2; end
  else begin MinY := ScaledY2; MaxY := ScaledY1; end;

  // Clip to mode resolution
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX >= FModeInfo.NativeWidth then MaxX := FModeInfo.NativeWidth - 1;
  if MaxY >= FModeInfo.NativeHeight then MaxY := FModeInfo.NativeHeight - 1;

  if UseIndex then
  begin
    // Classic modes: use palette index version of SetPixel
    if Filled then
    begin
      for Y := MinY to MaxY do
        for X := MinX to MaxX do
          FGraphicsMemory.SetPixel(X, Y, ActualIndex);
    end
    else
    begin
      // Draw OUTLINE only
      Y := MinY;
      for X := MinX to MaxX do
        FGraphicsMemory.SetPixel(X, Y, ActualIndex);
      Y := MaxY;
      for X := MinX to MaxX do
        FGraphicsMemory.SetPixel(X, Y, ActualIndex);
      X := MinX;
      for Y := MinY + 1 to MaxY - 1 do
        FGraphicsMemory.SetPixel(X, Y, ActualIndex);
      X := MaxX;
      for Y := MinY + 1 to MaxY - 1 do
        FGraphicsMemory.SetPixel(X, Y, ActualIndex);
    end;
  end
  else
  begin
    // Mode 11: use RGBA version of SetPixel
    if Filled then
    begin
      for Y := MinY to MaxY do
        for X := MinX to MaxX do
          FGraphicsMemory.SetPixel(X, Y, Color);
    end
    else
    begin
      Y := MinY;
      for X := MinX to MaxX do
        FGraphicsMemory.SetPixel(X, Y, Color);
      Y := MaxY;
      for X := MinX to MaxX do
        FGraphicsMemory.SetPixel(X, Y, Color);
      X := MinX;
      for Y := MinY + 1 to MaxY - 1 do
        FGraphicsMemory.SetPixel(X, Y, Color);
      X := MaxX;
      for Y := MinY + 1 to MaxY - 1 do
        FGraphicsMemory.SetPixel(X, Y, Color);
    end;
  end;

  // Throttled present (~60fps): keeps incremental drawing visible without one
  // blocking vsync present per shape. See MaybePresentGraphics.
  MaybePresentGraphics;
end;

procedure TVideoController.DrawCircleWithColor(X, Y, XR, YR: Integer; Color: UInt32;
                                               SA, EA, Angle, Inc: Double);
var
  ActualIndex: TPaletteIndex;
  UseIndex: Boolean;
  CurrentAngle, EndAngle, RotRad: Double;
  CosRot, SinRot: Double;
  PrevX, PrevY, CurrX, CurrY: Integer;
  Px, Py, Rx, Ry: Double;
  FirstPoint: Boolean;
  ScaledX, ScaledY, ScaledXR, ScaledYR: Integer;
begin
  if FGraphicsMemory = nil then Exit;

  // Apply coordinate scaling if enabled
  if FScaleEnabled then
  begin
    ScaledX := Round(X * FModeInfo.NativeWidth / FScaleXMax);
    ScaledY := Round(Y * FModeInfo.NativeHeight / FScaleYMax);
    ScaledXR := Round(XR * FModeInfo.NativeWidth / FScaleXMax);
    ScaledYR := Round(YR * FModeInfo.NativeHeight / FScaleYMax);
  end
  else
  begin
    ScaledX := X;
    ScaledY := Y;
    ScaledXR := XR;
    ScaledYR := YR;
  end;

  // For classic modes (0-10), use palette indices directly
  // For mode 11 (SDL2 dynamic), use RGBA values
  UseIndex := (FCurrentMode <> gmSDL2Dynamic);

  if UseIndex then
  begin
    // Classic modes (0-10): Color parameter selects which palette index to use
    case Color of
      0: ActualIndex := FBackgroundIndex;
      1: ActualIndex := FForegroundIndex;
      2: ActualIndex := FMulticolorIndex1;
      3: ActualIndex := FMulticolorIndex2;
    else
      if Color < 256 then
        ActualIndex := TPaletteIndex(Color)
      else
        ActualIndex := FForegroundIndex;
    end;
  end;

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

  while CurrentAngle <= EndAngle + 0.001 do  // Small epsilon for floating point
  begin
    // Calculate point on ellipse - C128 convention:
    // 0°/360° = 12 o'clock (top), 90° = 3 o'clock (right),
    // 180° = 6 o'clock (bottom), 270° = 9 o'clock (left)
    // Drawing proceeds clockwise from start to end angle
    Px := ScaledXR * Sin(CurrentAngle * Pi / 180.0);   // 0°->0, 90°->R, 180°->0, 270°->-R
    Py := -ScaledYR * Cos(CurrentAngle * Pi / 180.0);  // 0°->-R (top), 90°->0, 180°->R (bottom)

    // Apply rotation
    Rx := Px * CosRot - Py * SinRot;
    Ry := Px * SinRot + Py * CosRot;

    // Translate to center (using scaled coordinates)
    CurrX := ScaledX + Round(Rx);
    CurrY := ScaledY + Round(Ry);

    // Draw segment — per-pixel clipping is handled by SetPixelSafe inside
    // DrawLineInternal/DrawThickPixel, so we always draw even when vertices
    // are out of bounds. Skipping entire segments creates gaps in circles
    // near screen edges, allowing flood fill to leak through.
    if not FirstPoint then
      DrawLineInternal(PrevX, PrevY, CurrX, CurrY, UseIndex, ActualIndex, Color)
    else
    begin
      // First point - plot with line width (SetPixelSafe clips if OOB)
      if UseIndex then
        SedaiGraphicsPrimitives.DrawThickPixel(FGraphicsMemory, CurrX, CurrY,
          ActualIndex, True, FLineWidth, FModeInfo.NativeWidth, FModeInfo.NativeHeight)
      else
        SedaiGraphicsPrimitives.DrawThickPixel(FGraphicsMemory, CurrX, CurrY,
          Color, False, FLineWidth, FModeInfo.NativeWidth, FModeInfo.NativeHeight);
    end;

    PrevX := CurrX;
    PrevY := CurrY;
    FirstPoint := False;
    CurrentAngle := CurrentAngle + Inc;
  end;

  MaybePresentGraphics;  // throttled ~60fps present (see DrawBoxWithColor)
end;

procedure TVideoController.DrawLineInternal(X1, Y1, X2, Y2: Integer;
                                            UseIndex: Boolean; PalIndex: TPaletteIndex; RGBAColor: UInt32);
var
  Dx, Dy, Sx, Sy, Err, E2: Integer;
  Color: UInt32;
begin
  // Prepare color value for DrawThickPixel
  if UseIndex then
    Color := PalIndex
  else
    Color := RGBAColor;

  // Bresenham's line algorithm
  Dx := Abs(X2 - X1);
  Dy := Abs(Y2 - Y1);
  if X1 < X2 then Sx := 1 else Sx := -1;
  if Y1 < Y2 then Sy := 1 else Sy := -1;
  Err := Dx - Dy;

  while True do
  begin
    // Plot pixel with line width
    SedaiGraphicsPrimitives.DrawThickPixel(FGraphicsMemory, X1, Y1, Color, UseIndex,
      FLineWidth, FModeInfo.NativeWidth, FModeInfo.NativeHeight);

    if (X1 = X2) and (Y1 = Y2) then Break;

    E2 := 2 * Err;
    if E2 > -Dy then
    begin
      Err := Err - Dy;
      X1 := X1 + Sx;
    end;
    if E2 < Dx then
    begin
      Err := Err + Dx;
      Y1 := Y1 + Sy;
    end;
  end;
end;

procedure TVideoController.DrawLine(X1, Y1, X2, Y2: Integer; Color: UInt32);
var
  ActualIndex: TPaletteIndex;
  UseIndex: Boolean;
  ScaledX1, ScaledY1, ScaledX2, ScaledY2: Integer;
begin
  if FGraphicsMemory = nil then Exit;

  // Apply coordinate scaling if enabled
  if FScaleEnabled then
  begin
    ScaledX1 := Round(X1 * FModeInfo.NativeWidth / FScaleXMax);
    ScaledY1 := Round(Y1 * FModeInfo.NativeHeight / FScaleYMax);
    ScaledX2 := Round(X2 * FModeInfo.NativeWidth / FScaleXMax);
    ScaledY2 := Round(Y2 * FModeInfo.NativeHeight / FScaleYMax);
  end
  else
  begin
    ScaledX1 := X1;
    ScaledY1 := Y1;
    ScaledX2 := X2;
    ScaledY2 := Y2;
  end;

  // For classic modes (0-10), use palette indices directly
  // For mode 11 (SDL2 dynamic), use RGBA values
  UseIndex := (FCurrentMode <> gmSDL2Dynamic);

  if UseIndex then
  begin
    // Classic modes (0-10): Color parameter selects which palette index to use
    case Color of
      0: ActualIndex := FBackgroundIndex;
      1: ActualIndex := FForegroundIndex;
      2: ActualIndex := FMulticolorIndex1;
      3: ActualIndex := FMulticolorIndex2;
    else
      if Color < 256 then
        ActualIndex := TPaletteIndex(Color)
      else
        ActualIndex := FForegroundIndex;
    end;
    DrawLineInternal(ScaledX1, ScaledY1, ScaledX2, ScaledY2, True, ActualIndex, 0);
  end
  else
    DrawLineInternal(ScaledX1, ScaledY1, ScaledX2, ScaledY2, False, 0, Color);

  MaybePresentGraphics;  // throttled ~60fps present (see DrawBoxWithColor)
end;

procedure TVideoController.SetPixelCursor(X, Y: Integer);
begin
  // Apply coordinate scaling if enabled
  if FScaleEnabled then
  begin
    FPixelCursorX := Round(X * FModeInfo.NativeWidth / FScaleXMax);
    FPixelCursorY := Round(Y * FModeInfo.NativeHeight / FScaleYMax);
  end
  else
  begin
    FPixelCursorX := X;
    FPixelCursorY := Y;
  end;
end;

function TVideoController.GetPixelCursorX: Integer;
begin
  Result := FPixelCursorX;
end;

function TVideoController.GetPixelCursorY: Integer;
begin
  Result := FPixelCursorY;
end;

procedure TVideoController.SetBorderStyle(const Style: TBorderStyle);
begin
  // Not implemented
end;

procedure TVideoController.SetFillStyle(const Style: TFillStyleDef);
begin
  // Not implemented
end;

function TVideoController.GetBorderStyle: TBorderStyle;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function TVideoController.GetFillStyle: TFillStyleDef;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

// === Graphics Texture Management ===

procedure TVideoController.CreateGraphicsTexture;
begin
  if FRenderer = nil then Exit;

  // Destroy existing texture if any
  DestroyGraphicsTexture;

  // Create streaming texture for the current mode resolution
  // Use ABGR8888 to match palette format ($AABBGGRR on little-endian)
  FGraphicsTexture := SDL_CreateTexture(
    FRenderer,
    SDL_PIXELFORMAT_ABGR8888,
    SDL_TEXTUREACCESS_STREAMING,
    FModeInfo.NativeWidth,
    FModeInfo.NativeHeight
  );

  if FGraphicsTexture = nil then
    WriteLn('ERROR: Failed to create graphics texture: ', SDL_GetError);
end;

procedure TVideoController.DestroyGraphicsTexture;
begin
  if FGraphicsTexture <> nil then
  begin
    SDL_DestroyTexture(FGraphicsTexture);
    FGraphicsTexture := nil;
  end;
end;

procedure TVideoController.CreateTextTexture;
begin
  if FRenderer = nil then Exit;

  // Destroy existing texture if any
  DestroyTextTexture;

  // Create render target texture for full window (includes border)
  FTextTexture := SDL_CreateTexture(
    FRenderer,
    SDL_PIXELFORMAT_RGBA8888,
    SDL_TEXTUREACCESS_TARGET,  // Render target, not streaming
    FWindowWidth,
    FWindowHeight
  );

  if FTextTexture = nil then
    WriteLn('ERROR: Failed to create text texture: ', SDL_GetError)
  else
    FTextTextureDirty := True;  // Force initial full render

  // Create scroll scratch texture (same size as viewport area)
  FScrollTexture := SDL_CreateTexture(
    FRenderer,
    SDL_PIXELFORMAT_RGBA8888,
    SDL_TEXTUREACCESS_TARGET,
    FViewportWidth,
    FViewportHeight
  );
  if FScrollTexture = nil then
    WriteLn('WARNING: Failed to create scroll texture: ', SDL_GetError);
end;

procedure TVideoController.DestroyTextTexture;
begin
  if FScrollTexture <> nil then
  begin
    SDL_DestroyTexture(FScrollTexture);
    FScrollTexture := nil;
  end;
  if FTextTexture <> nil then
  begin
    SDL_DestroyTexture(FTextTexture);
    FTextTexture := nil;
  end;
end;

procedure TVideoController.SyncGraphicsBufferToTexture;
var
  Pixels: Pointer;
  Pitch: Integer;
  BufferPtr: PByte;
  Y, X: Integer;
  SrcOffset, DstOffset: Integer;
  SrcPixel: PUInt32;
  DstPixel: PUInt32;
begin
  if (FGraphicsTexture = nil) or (FGraphicsMemory = nil) then Exit;
  if FGraphicsMemory.GraphicsBuffer = nil then Exit;

  // Lock the texture for writing
  if SDL_LockTexture(FGraphicsTexture, nil, @Pixels, @Pitch) <> 0 then
  begin
    WriteLn('ERROR: Failed to lock texture: ', SDL_GetError);
    Exit;
  end;

  try
    BufferPtr := FGraphicsMemory.GraphicsBuffer;

    // Copy from graphics buffer to texture
    // Buffer format: RGBA (4 bytes per pixel)
    for Y := 0 to FModeInfo.NativeHeight - 1 do
    begin
      SrcOffset := Y * FModeInfo.NativeWidth * 4;
      DstOffset := Y * Pitch;

      // Copy entire row
      Move((BufferPtr + SrcOffset)^, (PByte(Pixels) + DstOffset)^, FModeInfo.NativeWidth * 4);
    end;
  finally
    SDL_UnlockTexture(FGraphicsTexture);
  end;
end;

procedure TVideoController.RenderGraphicsTexture;
var
  SrcRect, DestRect: TSDL_Rect;
  NativeCharH, GraphicsNativeH: Integer;
begin
  if FGraphicsTexture = nil then Exit;

  // Sync buffer to texture
  SyncGraphicsBufferToTexture;

  // Calculate destination rectangle (scaled to viewport)
  DestRect.x := FViewportX;
  DestRect.y := FViewportY;
  DestRect.w := FViewportWidth;
  DestRect.h := FViewportHeight;

  // For mixed modes, clip graphics to top portion (text area is at bottom)
  if FModeInfo.ModeType = mtMixed then
  begin
    NativeCharH := FModeInfo.NativeWidth div FModeInfo.TextCols;
    GraphicsNativeH := FModeInfo.NativeHeight - (FModeInfo.TextRows * NativeCharH);
    SrcRect.x := 0;
    SrcRect.y := 0;
    SrcRect.w := FModeInfo.NativeWidth;
    SrcRect.h := GraphicsNativeH;
    DestRect.h := FViewportHeight - (FModeInfo.TextRows * FCharHeight);
    SDL_RenderCopy(FRenderer, FGraphicsTexture, @SrcRect, @DestRect);
  end
  else
    SDL_RenderCopy(FRenderer, FGraphicsTexture, nil, @DestRect);
end;

// Throttled present for drawing primitives (BOX/CIRCLE/DRAW/PAINT/GSHAPE).
// Drawing writes only the CPU buffer; this flushes it to screen at most ~60fps.
//  - A tight loop drawing many shapes coalesces into a few presents instead of one
//    blocking vsync present per shape (which made 256 boxes take ~4s).
//  - Slow/animated drawing still updates progressively (you see it draw), because a
//    present happens every time >=16ms of wall-clock have elapsed.
// The end-of-execution render (EndVMExecution) and the VM event-poll render guarantee
// the final/intermediate frames are shown even when the throttle skips a present here.
procedure TVideoController.MaybePresentGraphics;
begin
  if FGraphicsTexture = nil then Exit;
  // Unsigned wrap-safe elapsed check; also presents on the first call (tick 0).
  if (FLastGfxPresentTick = 0) or ((SDL_GetTicks - FLastGfxPresentTick) >= 16) then
  begin
    RenderGraphicsTexture;
    Present;  // with PRESENTVSYNC this BLOCKS ~16ms until the next vblank
    // Stamp the marker AFTER the present: the vsync wait is consumed inside Present,
    // so if we used a timestamp read before it, the elapsed time would already be
    // ~16ms and every subsequent shape would present+block again (defeating the
    // throttle -> 256 boxes back to ~4s). Reading the clock here means the next
    // 16ms window is measured from real drawing, so fast loops coalesce.
    FLastGfxPresentTick := SDL_GetTicks;
  end;
end;

// === TVideoController IGraphicsBackend implementation (delegates to the existing drawing methods) ===

function TVideoController.GBSetMode(Mode: TGraphicMode; ClearBuffer: Boolean; SplitLine: Integer): Boolean;
begin
  Result := SetGraphicMode(Mode, ClearBuffer, SplitLine);
end;

function TVideoController.GBResizeScreen(W, H, Depth: Integer): Boolean;
begin
  // Phase 1: route to the SDL2 dynamic mode. Precise arbitrary W x H sizing is deferred to phase 2/G1.
  Result := SetGraphicMode(gmSDL2Dynamic, True, -1);
end;

function TVideoController.GBGetMode: TGraphicMode;
begin
  Result := GetGraphicMode;
end;

function TVideoController.GBInGraphics: Boolean;
begin
  Result := IsInGraphicsMode;
end;

procedure TVideoController.GBClearSurface(Surface: TGfxSurface; Color: TGfxColor);
begin
  ClearScreen(-1);
end;

procedure TVideoController.GBPresent;
begin
  Present;
end;

function TVideoController.GBScreenSurface: TGfxSurface;
begin
  Result := GFX_SCREEN_SURFACE;
end;

function TVideoController.GBImageMem(Surface: TGfxSurface): TGraphicsMemory;
// Image-surface memory for a surface id (>0); nil for the screen (0) or an invalid/destroyed id.
begin
  if (Surface >= 1) and (Surface <= Length(FImageSurfaces)) then
    Result := FImageSurfaces[Surface - 1]
  else
    Result := nil;
end;

function TVideoController.GBCreateSurface(W, H: Integer; Fill: TGfxColor): TGfxSurface;
// FreeBASIC IMAGECREATE: allocate a truecolor image surface, cleared to Fill. Reuses a freed slot.
var
  Img: TGraphicsMemory;
  i, Slot: Integer;
begin
  if (W <= 0) or (H <= 0) then Exit(GFX_INVALID_SURFACE);
  Img := TGraphicsMemory.Create;
  Img.AllocateBuffers(W, H, False, gmSDL2Dynamic);
  Img.ClearCurrentMode(Fill);
  Slot := -1;
  for i := 0 to High(FImageSurfaces) do
    if FImageSurfaces[i] = nil then begin Slot := i; Break; end;
  if Slot < 0 then
  begin
    SetLength(FImageSurfaces, Length(FImageSurfaces) + 1);
    Slot := High(FImageSurfaces);
  end;
  FImageSurfaces[Slot] := Img;
  Result := Slot + 1;   // id 0 = screen
end;

procedure TVideoController.GBDestroySurface(Surface: TGfxSurface);
begin
  if (Surface >= 1) and (Surface <= Length(FImageSurfaces)) and Assigned(FImageSurfaces[Surface - 1]) then
  begin
    FImageSurfaces[Surface - 1].Free;
    FImageSurfaces[Surface - 1] := nil;
  end;
end;

function TVideoController.GBSurfaceWidth(Surface: TGfxSurface): Integer;
var M: TGraphicsMemory;
begin
  M := GBImageMem(Surface);
  if Assigned(M) then Result := M.State.Width else Result := FViewportWidth;
end;

function TVideoController.GBSurfaceHeight(Surface: TGfxSurface): Integer;
var M: TGraphicsMemory;
begin
  M := GBImageMem(Surface);
  if Assigned(M) then Result := M.State.Height else Result := FViewportHeight;
end;

procedure TVideoController.GBSetPixel(Surface: TGfxSurface; X, Y: Integer; Color: TGfxColor);
var M: TGraphicsMemory;
begin
  M := GBImageMem(Surface);
  if Assigned(M) then M.SetPixel(X, Y, Color)
  else SetPixel(X, Y, Color);   // screen (texture repainted by the render poll)
end;

function TVideoController.GBSurfaceData(Surface: TGfxSurface; out Data: PByte; out SizeBytes: Integer): Boolean;
// SCREENPTR. The screen (surface 0) is backed by this controller's own FGraphicsMemory -- the same CPU
// byte buffer the renderer uploads each frame -- so a program writing through the pointer changes the
// pixels the next Present shows, exactly as on the headless software backend. Image surfaces (>0) carry
// their own TGraphicsMemory.
var M: TGraphicsMemory;
begin
  Data := nil; SizeBytes := 0;
  M := GBImageMem(Surface);
  if not Assigned(M) then M := FGraphicsMemory;       // surface 0 = the screen
  Result := Assigned(M) and Assigned(M.GraphicsBuffer) and (M.GraphicsBufferSize > 0);
  if Result then
  begin
    Data := M.GraphicsBuffer;
    SizeBytes := M.GraphicsBufferSize;
  end;
end;

function TVideoController.GBGetPixel(Surface: TGfxSurface; X, Y: Integer): TGfxColor;
var M: TGraphicsMemory;
begin
  M := GBImageMem(Surface);
  if Assigned(M) then Result := M.GetPixel(X, Y)
  else Result := GetPixel(X, Y);
end;

procedure TVideoController.GBDrawLine(Surface: TGfxSurface; X1, Y1, X2, Y2: Integer; Color: TGfxColor; LineWidth: Integer);
begin
  DrawLine(X1, Y1, X2, Y2, Color);
end;

procedure TVideoController.GBDrawLineStyled(Surface: TGfxSurface; X1, Y1, X2, Y2: Integer; Color: TGfxColor; Style: Word);
// FreeBASIC styled (dashed) line: a 16-bit pattern walked along the line, most-significant bit first
// (pixel 0 -> bit 15, ... pixel 15 -> bit 0), then reused. A set bit draws, a clear bit skips. Bresenham,
// one pixel per step -- the same rule as the software backend, so a dashed LINE looks identical on sbv.
var
  DX, DY, SX, SY, Err, E2, Idx: Integer;
begin
  if Style = 0 then Exit;                 // an all-clear pattern draws nothing
  if Style = $FFFF then begin DrawLine(X1, Y1, X2, Y2, Color); Exit; end;
  DX := Abs(X2 - X1); DY := -Abs(Y2 - Y1);
  if X1 < X2 then SX := 1 else SX := -1;
  if Y1 < Y2 then SY := 1 else SY := -1;
  Err := DX + DY;
  Idx := 0;
  while True do
  begin
    if (Style and (Word(1) shl (15 - (Idx and 15)))) <> 0 then
      SetPixel(X1, Y1, Color);
    Inc(Idx);
    if (X1 = X2) and (Y1 = Y2) then Break;
    E2 := 2 * Err;
    if E2 >= DY then begin Err := Err + DY; X1 := X1 + SX; end;
    if E2 <= DX then begin Err := Err + DX; Y1 := Y1 + SY; end;
  end;
end;

procedure TVideoController.GBFillBorder(Surface: TGfxSurface; X, Y: Integer; Color, BorderColor: TGfxColor);
begin
  // Deferred on this device for the same reason as GBFill: its existing FloodFill is colour-source
  // based, not direct-colour. The software backend implements the boundary fill; sbv routes PAINT
  // through this stub until that is unified.
end;

procedure TVideoController.GBDrawRect(Surface: TGfxSurface; X1, Y1, X2, Y2: Integer; Color: TGfxColor; Filled: Boolean; LineWidth: Integer; Angle: Double);
begin
  DrawBoxWithColor(X1, Y1, X2, Y2, Color, Angle, Filled);
end;

procedure TVideoController.GBDrawEllipse(Surface: TGfxSurface; CX, CY, RX, RY: Integer; Color: TGfxColor; StartAngle, EndAngle, RotationAngle, AngleStep: Double; LineWidth: Integer);
begin
  DrawCircleWithColor(CX, CY, RX, RY, Color, StartAngle, EndAngle, RotationAngle, AngleStep);
end;

procedure TVideoController.GBFill(Surface: TGfxSurface; X, Y: Integer; Color: TGfxColor);
begin
  // FB PAINT on this device deferred (the existing FloodFill is colour-source based, not direct-colour).
end;

procedure TVideoController.GBSetClip(Surface: TGfxSurface; Active: Boolean; X1, Y1, X2, Y2: Integer);
var M: TGraphicsMemory;
begin
  M := GBImageMem(Surface);
  if Assigned(M) then M.SetClip(Active, X1, Y1, X2, Y2)
  else if Assigned(FGraphicsMemory) then FGraphicsMemory.SetClip(Active, X1, Y1, X2, Y2);   // screen
end;

procedure TVideoController.GBBlit(Dst: TGfxSurface; X, Y: Integer; Src: TGfxSurface; Mode: TGfxBlitMode);
// Blit the whole Src surface onto Dst at (X,Y), per pixel, applying the blit mode (ABGR colours;
// TRANS skips the magenta key). Mirrors the software backend so sb and sbv agree pixel-for-pixel.
const
  TRANS_KEY = TGfxColor($FFFF00FF);
var
  SW, SH, DW, DH, sx, sy, dx, dy: Integer;
  sc, dc, nc: TGfxColor;

  function Blend(D, S: TGfxColor): TGfxColor;
  var dr, dg, db, sr, sg, sb, sa, r, g, b: Integer;
  begin
    case Mode of
      gbmAnd: Result := D and S;
      gbmOr:  Result := D or S;
      gbmXor: Result := D xor S;
      gbmAdd:
        begin
          sr := S and $FF; sg := (S shr 8) and $FF; sb := (S shr 16) and $FF;
          dr := D and $FF; dg := (D shr 8) and $FF; db := (D shr 16) and $FF;
          r := dr + sr; if r > 255 then r := 255;
          g := dg + sg; if g > 255 then g := 255;
          b := db + sb; if b > 255 then b := 255;
          Result := (D and $FF000000) or TGfxColor(b shl 16) or TGfxColor(g shl 8) or TGfxColor(r);
        end;
      gbmAlpha:
        begin
          sa := (S shr 24) and $FF;
          sr := S and $FF; sg := (S shr 8) and $FF; sb := (S shr 16) and $FF;
          dr := D and $FF; dg := (D shr 8) and $FF; db := (D shr 16) and $FF;
          r := (sr * sa + dr * (255 - sa)) div 255;
          g := (sg * sa + dg * (255 - sa)) div 255;
          b := (sb * sa + db * (255 - sa)) div 255;
          Result := $FF000000 or TGfxColor(b shl 16) or TGfxColor(g shl 8) or TGfxColor(r);
        end;
    else
      Result := S;
    end;
  end;

begin
  SW := GBSurfaceWidth(Src);  SH := GBSurfaceHeight(Src);
  DW := GBSurfaceWidth(Dst);  DH := GBSurfaceHeight(Dst);
  for sy := 0 to SH - 1 do
  begin
    dy := Y + sy;
    if (dy < 0) or (dy >= DH) then Continue;
    for sx := 0 to SW - 1 do
    begin
      dx := X + sx;
      if (dx < 0) or (dx >= DW) then Continue;
      sc := GBGetPixel(Src, sx, sy);
      if (Mode = gbmTrans) and (sc = TRANS_KEY) then Continue;
      if Mode in [gbmPSet, gbmTrans, gbmCustom] then
        nc := sc
      else
      begin
        dc := GBGetPixel(Dst, dx, dy);
        nc := Blend(dc, sc);
      end;
      GBSetPixel(Dst, dx, dy, nc);
    end;
  end;
end;

procedure TVideoController.GBEnablePalette(Enable: Boolean);
begin
  EnablePalette(Enable);
end;

procedure TVideoController.GBSetPaletteColor(Index: TPaletteIndex; Color: TGfxColor);
begin
  SetPaletteColor(Index, Color);
end;

function TVideoController.GBGetPaletteColor(Index: TPaletteIndex): TGfxColor;
begin
  Result := GetPaletteColor(Index);
end;

procedure TVideoController.GBResetPalette;
begin
  ResetPalette;
end;

{ TScrollbackRingBuffer }

constructor TScrollbackRingBuffer.Create(ACapacity: Integer);
begin
  inherited Create;
  FCapacity := ACapacity;
  SetLength(FBuffer, FCapacity);
  FCount := 0;
  FHead := 0;
end;

destructor TScrollbackRingBuffer.Destroy;
begin
  SetLength(FBuffer, 0);
  inherited Destroy;
end;

procedure TScrollbackRingBuffer.Add(const Line: string);
begin
  FBuffer[FHead] := Line;
  FHead := (FHead + 1) mod FCapacity;
  if FCount < FCapacity then
    Inc(FCount);
end;

procedure TScrollbackRingBuffer.Clear;
var
  i: Integer;
begin
  for i := 0 to FCapacity - 1 do
    FBuffer[i] := '';
  FCount := 0;
  FHead := 0;
end;

function TScrollbackRingBuffer.GetLine(Index: Integer): string;
var
  ActualIndex: Integer;
begin
  // Index 0 = oldest line, Index Count-1 = newest line
  if (Index < 0) or (Index >= FCount) then
  begin
    Result := '';
    Exit;
  end;
  // Calculate actual position in circular buffer
  // Head points to where next item will be written
  // So oldest item is at (Head - Count + Capacity) mod Capacity
  ActualIndex := (FHead - FCount + Index + FCapacity) mod FCapacity;
  Result := FBuffer[ActualIndex];
end;

{ TTextBuffer }

constructor TTextBuffer.Create(Rows, Cols: Integer);
const
  SCROLLBACK_CAPACITY = 10000;  // ~400 pages @25 rows (like PowerShell/Bash)
var
  r, c: Integer;
begin
  inherited Create;
  FRows := Rows;
  FCols := Cols;

  // Allocate cells array
  SetLength(FCells, Rows, Cols);
  for r := 0 to Rows - 1 do
    for c := 0 to Cols - 1 do
    begin
      FCells[r][c].Ch := ' ';
      FCells[r][c].FGIndex := DEFAULT_FG_INDEX;
      FCells[r][c].BGIndex := BG_USE_DEFAULT;
      FCells[r][c].Reverse := False;
      FCells[r][c].Dirty := False;
    end;

  // Allocate row dirty flags
  SetLength(FRowDirty, Rows);
  for r := 0 to Rows - 1 do
    FRowDirty[r] := False;
  FAllDirty := True;  // First render draws everything
  FScrollPending := False;
  FScrollDirection := 0;

  FCursorX := 0;
  FCursorY := 0;

  // Default text colors
  FCurrentFGIndex := DEFAULT_FG_INDEX;
  FCurrentBGIndex := BG_USE_DEFAULT;  // Cells use global background by default
  FCurrentReverse := False;

  // Initialize window to full screen
  FWindowCol1 := 0;
  FWindowRow1 := 0;
  FWindowCol2 := Cols - 1;
  FWindowRow2 := Rows - 1;

  // Initialize scrollback with ring buffer
  FScrollbackBuffer := TScrollbackRingBuffer.Create(SCROLLBACK_CAPACITY);
  FViewOffset := 0;
end;

procedure TTextBuffer.Resize(NewRows, NewCols: Integer);
var
  r, c: Integer;
begin
  if (NewRows = FRows) and (NewCols = FCols) then Exit;

  FRows := NewRows;
  FCols := NewCols;

  // Reallocate cells
  SetLength(FCells, NewRows, NewCols);
  for r := 0 to NewRows - 1 do
    for c := 0 to NewCols - 1 do
    begin
      FCells[r][c].Ch := ' ';
      FCells[r][c].FGIndex := FCurrentFGIndex;
      FCells[r][c].BGIndex := BG_USE_DEFAULT;
      FCells[r][c].Reverse := False;
      FCells[r][c].Dirty := False;
    end;

  // Reallocate dirty flags
  SetLength(FRowDirty, NewRows);
  for r := 0 to NewRows - 1 do
    FRowDirty[r] := False;
  FAllDirty := True;

  // Reset cursor and window
  FCursorX := 0;
  FCursorY := 0;
  FWindowCol1 := 0;
  FWindowRow1 := 0;
  FWindowCol2 := NewCols - 1;
  FWindowRow2 := NewRows - 1;
  FViewOffset := 0;
end;

destructor TTextBuffer.Destroy;
begin
  SetLength(FCells, 0, 0);
  FScrollbackBuffer.Free;
  inherited Destroy;
end;

procedure TTextBuffer.Clear;
var
  r, c: Integer;
begin
  for r := 0 to FRows - 1 do
    for c := 0 to FCols - 1 do
    begin
      FCells[r][c].Ch := ' ';
      FCells[r][c].FGIndex := FCurrentFGIndex;
      FCells[r][c].BGIndex := FCurrentBGIndex;
      FCells[r][c].Reverse := FCurrentReverse;
    end;
  // Position cursor at top-left of window
  FCursorX := FWindowCol1;
  FCursorY := FWindowRow1;
  // Clear any pending hardware scroll (irrelevant after clear)
  FScrollPending := False;
  FScrollDirection := 0;
  // Mark everything for redraw
  MarkAllDirty;
end;

procedure TTextBuffer.ResetPrintState;
begin
  // Reset reverse mode after PRINT statement (C128 behavior)
  // On C128, reverse mode only affects the current PRINT statement
  FCurrentReverse := False;
end;

procedure TTextBuffer.SetWindow(Col1, Row1, Col2, Row2: Integer);
begin
  // Validate and clamp window boundaries
  FWindowCol1 := Max(0, Min(Col1, FCols - 1));
  FWindowRow1 := Max(0, Min(Row1, FRows - 1));
  FWindowCol2 := Max(FWindowCol1, Min(Col2, FCols - 1));
  FWindowRow2 := Max(FWindowRow1, Min(Row2, FRows - 1));
  // Position cursor at top-left of new window
  FCursorX := FWindowCol1;
  FCursorY := FWindowRow1;
end;

procedure TTextBuffer.ResetWindow;
begin
  FWindowCol1 := 0;
  FWindowRow1 := 0;
  FWindowCol2 := FCols - 1;
  FWindowRow2 := FRows - 1;
end;

procedure TTextBuffer.ClearWindow;
var
  r, c: Integer;
begin
  // Clear only within current window boundaries
  for r := FWindowRow1 to FWindowRow2 do
  begin
    for c := FWindowCol1 to FWindowCol2 do
    begin
      FCells[r][c].Ch := ' ';
      FCells[r][c].FGIndex := FCurrentFGIndex;
      FCells[r][c].BGIndex := FCurrentBGIndex;
      FCells[r][c].Reverse := FCurrentReverse;
    end;
    // Mark each affected row dirty
    MarkRowDirty(r);
  end;
  // Position cursor at top-left of window
  FCursorX := FWindowCol1;
  FCursorY := FWindowRow1;
end;

function TTextBuffer.GetWindowLines: Integer;
begin
  Result := FWindowRow2 - FWindowRow1 + 1;
end;

function TTextBuffer.GetWindowCols: Integer;
begin
  Result := FWindowCol2 - FWindowCol1 + 1;
end;

function TTextBuffer.CellsToString(Row: Integer): string;
var
  c, LastNonSpace: Integer;
begin
  // Convert cell row to string, trimming trailing spaces
  LastNonSpace := -1;
  for c := FCols - 1 downto 0 do
    if FCells[Row][c].Ch <> ' ' then
    begin
      LastNonSpace := c;
      Break;
    end;

  if LastNonSpace < 0 then
    Result := ''
  else
  begin
    SetLength(Result, LastNonSpace + 1);
    for c := 0 to LastNonSpace do
      Result[c + 1] := FCells[Row][c].Ch;
  end;
end;

function TTextBuffer.GetCell(Row, Col: Integer): TTextCell;
begin
  if (Row >= 0) and (Row < FRows) and (Col >= 0) and (Col < FCols) then
    Result := FCells[Row][Col]
  else
  begin
    Result.Ch := ' ';
    Result.FGIndex := FCurrentFGIndex;
    Result.BGIndex := FCurrentBGIndex;
    Result.Reverse := False;
  end;
end;

function TTextBuffer.GetViewCell(Row, Col: Integer): TTextCell;
var
  Line: string;
begin
  // If not in scrollback mode, use normal GetCell
  if FViewOffset = 0 then
  begin
    Result := GetCell(Row, Col);
    Exit;
  end;

  // In scrollback mode, get line from scrollback buffer
  Line := GetViewLine(Row);
  if (Col >= 0) and (Col < Length(Line)) then
    Result.Ch := Line[Col + 1]
  else
    Result.Ch := ' ';

  // Use default colors for scrollback (no color info stored)
  Result.FGIndex := FCurrentFGIndex;
  Result.BGIndex := FCurrentBGIndex;
  Result.Reverse := False;
  Result.Dirty := False;
end;

procedure TTextBuffer.SetCell(Row, Col: Integer; const Cell: TTextCell);
begin
  if (Row >= 0) and (Row < FRows) and (Col >= 0) and (Col < FCols) then
  begin
    FCells[Row][Col] := Cell;
    FCells[Row][Col].Dirty := True;
    MarkRowDirty(Row);
  end;
end;

procedure TTextBuffer.ReplaceBGIndex(OldIndex, NewIndex: Byte);
var
  r, c: Integer;
begin
  for r := 0 to FRows - 1 do
    for c := 0 to FCols - 1 do
      if FCells[r][c].Reverse then
      begin
        // Reversed cell: visual background is FGIndex
        if FCells[r][c].FGIndex = OldIndex then
          FCells[r][c].FGIndex := NewIndex;
      end
      else
      begin
        if FCells[r][c].BGIndex = OldIndex then
          FCells[r][c].BGIndex := NewIndex;
      end;
  // Only update current BG if it matched the old background
  if FCurrentBGIndex = OldIndex then
    FCurrentBGIndex := NewIndex;
  MarkAllDirty;
end;

procedure TTextBuffer.ReplaceFGIndex(OldIndex, NewIndex: Byte);
var
  r, c: Integer;
begin
  for r := 0 to FRows - 1 do
    for c := 0 to FCols - 1 do
      if FCells[r][c].Reverse then
      begin
        // Reversed cell: visual foreground is BGIndex
        if FCells[r][c].BGIndex = OldIndex then
          FCells[r][c].BGIndex := NewIndex;
      end
      else
      begin
        if FCells[r][c].FGIndex = OldIndex then
          FCells[r][c].FGIndex := NewIndex;
      end;
  // Only update current FG if it matched the old foreground
  if FCurrentFGIndex = OldIndex then
    FCurrentFGIndex := NewIndex;
  MarkAllDirty;
end;

function TTextBuffer.GetCharAt(Col, Row: Integer): Byte;
begin
  // Col, Row are 0-based (converted from screen memory address)
  if (Row >= 0) and (Row < FRows) and (Col >= 0) and (Col < FCols) then
    Result := Ord(FCells[Row][Col].Ch)
  else
    Result := 32; // Space for out-of-bounds
end;

procedure TTextBuffer.SetCharAt(Col, Row: Integer; Ch: Byte);
begin
  // Col, Row are 0-based (converted from screen memory address)
  if (Row >= 0) and (Row < FRows) and (Col >= 0) and (Col < FCols) then
  begin
    FCells[Row][Col].Ch := Chr(Ch);
    FCells[Row][Col].Dirty := True;
    MarkRowDirty(Row);
  end;
end;

function TTextBuffer.GetColorAt(Col, Row: Integer): Byte;
begin
  // Returns foreground color at position
  if (Row >= 0) and (Row < FRows) and (Col >= 0) and (Col < FCols) then
    Result := FCells[Row][Col].FGIndex
  else
    Result := FCurrentFGIndex;
end;

procedure TTextBuffer.SetColorAt(Col, Row: Integer; Color: Byte);
begin
  // Sets foreground color at position
  if (Row >= 0) and (Row < FRows) and (Col >= 0) and (Col < FCols) then
  begin
    FCells[Row][Col].FGIndex := Color and $0F;
    FCells[Row][Col].Dirty := True;
    MarkRowDirty(Row);
  end;
end;

procedure TTextBuffer.PutChar(Ch: Char);
var
  Code: Byte;
begin
  // Handle scroll if cursor beyond window bottom
  if FCursorY > FWindowRow2 then
  begin
    ScrollUp;
    FCursorY := FWindowRow2;
  end;

  Code := Ord(Ch);

  // Handle PETSCII control characters
  case Code of
    13: // CR - newline
    begin
      NewLine;
      Exit;
    end;

    // === PETSCII Control Characters ===
    147: // CHR$(147) = Clear screen (SHIFT+CLR/HOME)
    begin
      Clear;
      Exit;
    end;

    18: // CHR$(18) = Reverse ON (RVS ON)
    begin
      FCurrentReverse := True;
      Exit;
    end;

    146: // CHR$(146) = Reverse OFF (RVS OFF)
    begin
      FCurrentReverse := False;
      Exit;
    end;

    // === PETSCII Color Codes (C64/C128 compatible) ===
    // Map to C64 palette indices (0-15)
    5:   begin FCurrentFGIndex := 1;  Exit; end;  // White
    28:  begin FCurrentFGIndex := 2;  Exit; end;  // Red
    30:  begin FCurrentFGIndex := 5;  Exit; end;  // Green
    31:  begin FCurrentFGIndex := 6;  Exit; end;  // Blue
    144: begin FCurrentFGIndex := 0;  Exit; end;  // Black
    129: begin FCurrentFGIndex := 8;  Exit; end;  // Orange
    149: begin FCurrentFGIndex := 9;  Exit; end;  // Brown
    150: begin FCurrentFGIndex := 10; Exit; end;  // Light Red/Pink
    151: begin FCurrentFGIndex := 11; Exit; end;  // Dark Gray
    152: begin FCurrentFGIndex := 12; Exit; end;  // Medium Gray
    153: begin FCurrentFGIndex := 13; Exit; end;  // Light Green
    154: begin FCurrentFGIndex := 14; Exit; end;  // Light Blue
    155: begin FCurrentFGIndex := 15; Exit; end;  // Light Gray
    156: begin FCurrentFGIndex := 4;  Exit; end;  // Purple
    158: begin FCurrentFGIndex := 7;  Exit; end;  // Yellow
    159: begin FCurrentFGIndex := 3;  Exit; end;  // Cyan

    // === PETSCII Cursor Movement ===
    17: begin  // Cursor down
      if FCursorY < FWindowRow2 then Inc(FCursorY);
      Exit;
    end;
    145: begin  // Cursor up
      if FCursorY > FWindowRow1 then Dec(FCursorY);
      Exit;
    end;
    29: begin  // Cursor right
      if FCursorX < FWindowCol2 then Inc(FCursorX);
      Exit;
    end;
    157: begin  // Cursor left
      if FCursorX > FWindowCol1 then Dec(FCursorX);
      Exit;
    end;
    19: begin  // Home (CLR/HOME without SHIFT)
      FCursorX := FWindowCol1;
      FCursorY := FWindowRow1;
      Exit;
    end;
    148: Exit;      // Insert mode toggle
    20: Exit;       // Delete (handled by input, not print)
  end;

  // Only print printable characters (skip control chars that weren't handled)
  if Code < 32 then
    Exit;
  if (Code >= 128) and (Code < 160) then
    Exit;

  // Write character within window bounds
  if FCursorX <= FWindowCol2 then
  begin
    // Write character with current color attributes
    FCells[FCursorY][FCursorX].Ch := Ch;
    FCells[FCursorY][FCursorX].FGIndex := FCurrentFGIndex;
    FCells[FCursorY][FCursorX].BGIndex := FCurrentBGIndex;
    FCells[FCursorY][FCursorX].Reverse := FCurrentReverse;
    FCells[FCursorY][FCursorX].Dirty := True;  // Mark cell dirty
    MarkRowDirty(FCursorY);  // Mark row dirty (to know which rows to check)
    Inc(FCursorX);
  end;

  // Wrap to next line if past window right edge
  if FCursorX > FWindowCol2 then
    NewLine;
end;

procedure TTextBuffer.PutString(const S: string);
var
  i: Integer;
begin
  for i := 1 to Length(S) do
    PutChar(S[i]);
end;

procedure TTextBuffer.PutStringNoWrap(const S: string);
var
  i: Integer;
  Ch: Char;
  WroteChars: Boolean;
begin
  // Put string without wrapping - used for history display
  // Stops at end of line, ignores CR/LF
  WroteChars := False;
  for i := 1 to Length(S) do
  begin
    Ch := S[i];
    // Skip CR and LF
    if (Ch = #13) or (Ch = #10) then
      Continue;
    // Stop if we reach end of line
    if FCursorX >= FCols then
      Break;
    // Write character
    FCells[FCursorY][FCursorX].Ch := Ch;
    FCells[FCursorY][FCursorX].FGIndex := FCurrentFGIndex;
    FCells[FCursorY][FCursorX].BGIndex := FCurrentBGIndex;
    FCells[FCursorY][FCursorX].Reverse := FCurrentReverse;
    Inc(FCursorX);
    WroteChars := True;
  end;
  // Mark row dirty if we wrote anything
  if WroteChars then
    MarkRowDirty(FCursorY);
end;

procedure TTextBuffer.NewLine;
begin
  // Move cursor to left edge of window
  FCursorX := FWindowCol1;
  Inc(FCursorY);
  // Scroll within window if cursor went past bottom
  if FCursorY > FWindowRow2 then
  begin
    ScrollUp;
    FCursorY := FWindowRow2;
  end;
end;

procedure TTextBuffer.DeleteChar;
var
  c: Integer;
begin
  // Backspace: delete character BEFORE cursor and shift left
  if FCursorX > 0 then
  begin
    Dec(FCursorX);
    // Shift cells left from cursor position and mark dirty
    for c := FCursorX to FCols - 2 do
    begin
      FCells[FCursorY][c] := FCells[FCursorY][c + 1];
      FCells[FCursorY][c].Dirty := True;
    end;
    // Clear last cell
    FCells[FCursorY][FCols - 1].Ch := ' ';
    FCells[FCursorY][FCols - 1].FGIndex := FCurrentFGIndex;
    FCells[FCursorY][FCols - 1].BGIndex := FCurrentBGIndex;
    FCells[FCursorY][FCols - 1].Reverse := FCurrentReverse;
    FCells[FCursorY][FCols - 1].Dirty := True;
    MarkRowDirty(FCursorY);
  end;
end;

procedure TTextBuffer.DeleteCharAtCursor;
var
  c: Integer;
begin
  // Delete key: delete character AT cursor and shift left
  // Shift cells left from cursor position and mark dirty
  for c := FCursorX to FCols - 2 do
  begin
    FCells[FCursorY][c] := FCells[FCursorY][c + 1];
    FCells[FCursorY][c].Dirty := True;
  end;
  // Clear last cell
  FCells[FCursorY][FCols - 1].Ch := ' ';
  FCells[FCursorY][FCols - 1].FGIndex := FCurrentFGIndex;
  FCells[FCursorY][FCols - 1].BGIndex := FCurrentBGIndex;
  FCells[FCursorY][FCols - 1].Reverse := FCurrentReverse;
  FCells[FCursorY][FCols - 1].Dirty := True;
  MarkRowDirty(FCursorY);
end;

procedure TTextBuffer.ScrollUp;
var
  r, c: Integer;
begin
  // When scrolling within a window, only scroll the window content
  // Save the top line of the window to scrollback buffer before scrolling
  // Ring buffer handles overflow automatically (O(1) operation)
  {$IFDEF DEBUG_CONSOLE}
  WriteLn('[DEBUG] ScrollUp: Adding line to scrollback: "', CellsToString(FWindowRow1), '" Count before=', FScrollbackBuffer.Count);
  {$ENDIF}
  AddToScrollback(FWindowRow1);
  {$IFDEF DEBUG_CONSOLE}
  WriteLn('[DEBUG] ScrollUp: Count after=', FScrollbackBuffer.Count);
  {$ENDIF}

  // Scroll up within window - move rows up by one
  for r := FWindowRow1 to FWindowRow2 - 1 do
    for c := FWindowCol1 to FWindowCol2 do
      FCells[r][c] := FCells[r + 1][c];

  // Clear the bottom row of the window
  for c := FWindowCol1 to FWindowCol2 do
  begin
    FCells[FWindowRow2][c].Ch := ' ';
    FCells[FWindowRow2][c].FGIndex := FCurrentFGIndex;
    FCells[FWindowRow2][c].BGIndex := FCurrentBGIndex;
    FCells[FWindowRow2][c].Reverse := FCurrentReverse;
  end;

  // Hardware scroll: use texture shift for single scroll, AllDirty for multiple
  if (FWindowCol1 = 0) and (FWindowRow1 = 0) and
     (FWindowCol2 = FCols - 1) and (FWindowRow2 = FRows - 1) and
     (not FScrollPending) then
  begin
    // Single scroll on full screen - use hardware scroll
    FScrollPending := True;
    FScrollDirection := 1;
    MarkRowDirty(FWindowRow2);
  end
  else
  begin
    // Multiple scrolls or partial window - full redraw
    FScrollPending := False;
    FAllDirty := True;
  end;

  // If we were viewing scrollback, adjust offset to maintain view position
  if FViewOffset > 0 then
    Inc(FViewOffset);
end;

procedure TTextBuffer.AddToScrollback(Row: Integer);
begin
  FScrollbackBuffer.Add(CellsToString(Row));
end;

function TTextBuffer.GetScrollbackCount: Integer;
begin
  Result := FScrollbackBuffer.Count;
end;

// === Dirty tracking methods ===

procedure TTextBuffer.MarkRowDirty(Row: Integer);
begin
  if (Row >= 0) and (Row < FRows) then
    FRowDirty[Row] := True;
end;

procedure TTextBuffer.MarkAllDirty;
begin
  FAllDirty := True;
end;

procedure TTextBuffer.ClearDirtyFlags;
var
  r, c: Integer;
begin
  FAllDirty := False;
  for r := 0 to FRows - 1 do
  begin
    if FRowDirty[r] then
    begin
      // Clear per-cell dirty flags only for dirty rows (optimization)
      for c := 0 to FCols - 1 do
        FCells[r][c].Dirty := False;
      FRowDirty[r] := False;
    end;
  end;
end;

function TTextBuffer.IsRowDirty(Row: Integer): Boolean;
begin
  if FAllDirty then
    Result := True
  else if (Row >= 0) and (Row < FRows) then
    Result := FRowDirty[Row]
  else
    Result := False;
end;

function TTextBuffer.NeedsRedraw: Boolean;
var
  r: Integer;
begin
  if FAllDirty then
  begin
    Result := True;
    Exit;
  end;
  for r := 0 to FRows - 1 do
    if FRowDirty[r] then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

procedure TTextBuffer.ClearCurrentLine;
var
  c: Integer;
begin
  // Clears the contents of the current line
  for c := 0 to FCols - 1 do
  begin
    FCells[FCursorY][c].Ch := ' ';
    FCells[FCursorY][c].FGIndex := FCurrentFGIndex;
    FCells[FCursorY][c].BGIndex := FCurrentBGIndex;
    FCells[FCursorY][c].Reverse := FCurrentReverse;
  end;
  // Mark row dirty
  MarkRowDirty(FCursorY);
  // Returns the cursor to the beginning of the line
  FCursorX := 0;
end;

function TTextBuffer.GetLine(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FRows) then
    Result := CellsToString(Index)
  else
    Result := '';
end;

function TTextBuffer.GetCurrentLine: string;
begin
  Result := CellsToString(FCursorY);
end;

function TTextBuffer.GetCharAtCursor: Char;
begin
  if (FCursorX >= 0) and (FCursorX < FCols) then
    Result := FCells[FCursorY][FCursorX].Ch
  else
    Result := ' ';  // Return space if beyond bounds
end;

procedure TTextBuffer.InsertChar(Ch: Char);
var
  c: Integer;
begin
  // Insert mode: insert char at cursor position and shift right
  if FCursorY >= FRows then
  begin
    ScrollUp;
    FCursorY := FRows - 1;
  end;

  if Ch = #13 then
  begin
    NewLine;
    Exit;
  end;

  if FCursorX < FCols then
  begin
    // Shift cells right from cursor position and mark dirty
    for c := FCols - 1 downto FCursorX + 1 do
    begin
      FCells[FCursorY][c] := FCells[FCursorY][c - 1];
      FCells[FCursorY][c].Dirty := True;
    end;

    // Insert character at cursor position with current colors
    FCells[FCursorY][FCursorX].Ch := Ch;
    FCells[FCursorY][FCursorX].FGIndex := FCurrentFGIndex;
    FCells[FCursorY][FCursorX].BGIndex := FCurrentBGIndex;
    FCells[FCursorY][FCursorX].Reverse := FCurrentReverse;
    FCells[FCursorY][FCursorX].Dirty := True;
    MarkRowDirty(FCursorY);
    Inc(FCursorX);
  end;

  if FCursorX >= FCols then
    NewLine;
end;

procedure TTextBuffer.CursorLeft;
begin
  if FCursorX > 0 then
    Dec(FCursorX);
end;

procedure TTextBuffer.CursorRight;
var
  LineLen, c: Integer;
begin
  // Find last non-space character to determine effective line length
  LineLen := 0;
  for c := FCols - 1 downto 0 do
    if FCells[FCursorY][c].Ch <> ' ' then
    begin
      LineLen := c + 1;
      Break;
    end;
  // Allow cursor to move up to end of line (after last char)
  if FCursorX < LineLen then
    Inc(FCursorX);
end;

procedure TTextBuffer.CursorHome;
begin
  FCursorX := 0;
end;

procedure TTextBuffer.CursorEnd;
var
  c: Integer;
begin
  // Find last non-space character
  FCursorX := 0;
  for c := FCols - 1 downto 0 do
    if FCells[FCursorY][c].Ch <> ' ' then
    begin
      FCursorX := c + 1;
      Break;
    end;
end;

procedure TTextBuffer.CursorWordLeft;
begin
  // Jump to previous word (skip spaces, then skip non-spaces)
  if FCursorX = 0 then Exit;

  // Skip spaces going left
  while (FCursorX > 0) and (FCells[FCursorY][FCursorX - 1].Ch = ' ') do
    Dec(FCursorX);

  // Skip non-spaces going left
  while (FCursorX > 0) and (FCells[FCursorY][FCursorX - 1].Ch <> ' ') do
    Dec(FCursorX);
end;

procedure TTextBuffer.CursorWordRight;
var
  Len, c: Integer;
begin
  // Jump to next word (skip non-spaces, then skip spaces)
  // Find effective line length
  Len := 0;
  for c := FCols - 1 downto 0 do
    if FCells[FCursorY][c].Ch <> ' ' then
    begin
      Len := c + 1;
      Break;
    end;
  if FCursorX >= Len then Exit;

  // Skip non-spaces going right
  while (FCursorX < Len) and (FCells[FCursorY][FCursorX].Ch <> ' ') do
    Inc(FCursorX);

  // Skip spaces going right
  while (FCursorX < Len) and (FCells[FCursorY][FCursorX].Ch = ' ') do
    Inc(FCursorX);
end;

procedure TTextBuffer.DeleteToLineStart;
var
  c, Shift: Integer;
begin
  // Delete from cursor position to start of line (Ctrl+Home)
  if FCursorX = 0 then Exit;
  Shift := FCursorX;
  // Shift cells left
  for c := 0 to FCols - 1 - Shift do
    FCells[FCursorY][c] := FCells[FCursorY][c + Shift];
  // Clear remaining cells
  for c := FCols - Shift to FCols - 1 do
  begin
    FCells[FCursorY][c].Ch := ' ';
    FCells[FCursorY][c].FGIndex := FCurrentFGIndex;
    FCells[FCursorY][c].BGIndex := FCurrentBGIndex;
    FCells[FCursorY][c].Reverse := FCurrentReverse;
  end;
  // Mark row dirty
  MarkRowDirty(FCursorY);
  FCursorX := 0;
end;

procedure TTextBuffer.DeleteToLineEnd;
var
  c: Integer;
begin
  // Delete from cursor position to end of line (Ctrl+End)
  for c := FCursorX to FCols - 1 do
  begin
    FCells[FCursorY][c].Ch := ' ';
    FCells[FCursorY][c].FGIndex := FCurrentFGIndex;
    FCells[FCursorY][c].BGIndex := FCurrentBGIndex;
    FCells[FCursorY][c].Reverse := FCurrentReverse;
  end;
  // Mark row dirty
  MarkRowDirty(FCursorY);
end;

procedure TTextBuffer.DeleteWordRight;
var
  EndPos, c, Len, DeleteCount: Integer;
begin
  // Delete from cursor to end of word/next space (Ctrl+Delete)
  // Find effective line length
  Len := 0;
  for c := FCols - 1 downto 0 do
    if FCells[FCursorY][c].Ch <> ' ' then
    begin
      Len := c + 1;
      Break;
    end;
  if FCursorX >= Len then Exit;

  EndPos := FCursorX;

  // Skip non-spaces going right
  while (EndPos < Len) and (FCells[FCursorY][EndPos].Ch <> ' ') do
    Inc(EndPos);

  // Skip spaces going right
  while (EndPos < Len) and (FCells[FCursorY][EndPos].Ch = ' ') do
    Inc(EndPos);

  // Delete from FCursorX to EndPos by shifting cells
  DeleteCount := EndPos - FCursorX;
  for c := FCursorX to FCols - 1 - DeleteCount do
    FCells[FCursorY][c] := FCells[FCursorY][c + DeleteCount];
  // Clear tail
  for c := FCols - DeleteCount to FCols - 1 do
  begin
    FCells[FCursorY][c].Ch := ' ';
    FCells[FCursorY][c].FGIndex := FCurrentFGIndex;
    FCells[FCursorY][c].BGIndex := FCurrentBGIndex;
    FCells[FCursorY][c].Reverse := FCurrentReverse;
  end;
  // Mark row dirty
  MarkRowDirty(FCursorY);
end;

procedure TTextBuffer.DeleteWordLeft;
var
  StartPos, c, DeleteCount: Integer;
begin
  // Delete from cursor to start of word/previous space (Ctrl+Backspace)
  if FCursorX = 0 then Exit;

  StartPos := FCursorX;

  // Skip spaces going left
  while (StartPos > 0) and (FCells[FCursorY][StartPos - 1].Ch = ' ') do
    Dec(StartPos);

  // Skip non-spaces going left
  while (StartPos > 0) and (FCells[FCursorY][StartPos - 1].Ch <> ' ') do
    Dec(StartPos);

  // Delete from StartPos to FCursorX by shifting cells
  DeleteCount := FCursorX - StartPos;
  for c := StartPos to FCols - 1 - DeleteCount do
    FCells[FCursorY][c] := FCells[FCursorY][c + DeleteCount];
  // Clear tail
  for c := FCols - DeleteCount to FCols - 1 do
  begin
    FCells[FCursorY][c].Ch := ' ';
    FCells[FCursorY][c].FGIndex := FCurrentFGIndex;
    FCells[FCursorY][c].BGIndex := FCurrentBGIndex;
    FCells[FCursorY][c].Reverse := FCurrentReverse;
  end;
  // Mark row dirty
  MarkRowDirty(FCursorY);
  FCursorX := StartPos;
end;

procedure TTextBuffer.DeleteEntireLine;
var
  c: Integer;
begin
  // Delete entire line (Ctrl+U)
  for c := 0 to FCols - 1 do
  begin
    FCells[FCursorY][c].Ch := ' ';
    FCells[FCursorY][c].FGIndex := FCurrentFGIndex;
    FCells[FCursorY][c].BGIndex := FCurrentBGIndex;
    FCells[FCursorY][c].Reverse := FCurrentReverse;
  end;
  // Mark row dirty
  MarkRowDirty(FCursorY);
  FCursorX := 0;
end;

procedure TTextBuffer.ScrollViewUp(NumLines: Integer);
var
  MaxOffset: Integer;
begin
  // Maximum offset is the total scrollback lines
  MaxOffset := FScrollbackBuffer.Count;
  {$IFDEF DEBUG_CONSOLE}
  WriteLn('[DEBUG] ScrollViewUp: NumLines=', NumLines, ' MaxOffset=', MaxOffset, ' CurrentOffset=', FViewOffset);
  {$ENDIF}
  if MaxOffset = 0 then Exit;  // No scrollback available

  FViewOffset := FViewOffset + NumLines;
  if FViewOffset > MaxOffset then
    FViewOffset := MaxOffset;
  {$IFDEF DEBUG_CONSOLE}
  WriteLn('[DEBUG] ScrollViewUp: NewOffset=', FViewOffset);
  {$ENDIF}
  MarkAllDirty;
end;

procedure TTextBuffer.ScrollViewDown(NumLines: Integer);
begin
  FViewOffset := FViewOffset - NumLines;
  if FViewOffset < 0 then
    FViewOffset := 0;
  MarkAllDirty;
end;

procedure TTextBuffer.ScrollToEnd;
begin
  FViewOffset := 0;
  MarkAllDirty;
end;

procedure TTextBuffer.ScrollToStart;
begin
  // Go to the beginning of the scrollback buffer
  FViewOffset := FScrollbackBuffer.Count;
end;

function TTextBuffer.GetViewLine(Index: Integer): string;
var
  ScrollbackIndex: Integer;
begin
  // If not in scrollback mode, return current screen line
  if FViewOffset = 0 then
  begin
    Result := GetLine(Index);
    Exit;
  end;

  // Calculate which line to show based on view offset
  // ViewOffset = how many lines we're scrolled back
  // Index 0 = top of screen
  // We need to show lines from scrollback buffer

  ScrollbackIndex := FScrollbackBuffer.Count - FViewOffset + Index;

  if ScrollbackIndex < 0 then
    Result := ''  // Before scrollback start
  else if ScrollbackIndex < FScrollbackBuffer.Count then
    Result := FScrollbackBuffer.GetLine(ScrollbackIndex)  // From scrollback (ring buffer)
  else
  begin
    // From current screen buffer
    ScrollbackIndex := ScrollbackIndex - FScrollbackBuffer.Count;
    if ScrollbackIndex < FRows then
      Result := CellsToString(ScrollbackIndex)
    else
      Result := '';
  end;
end;

function TTextBuffer.IsInScrollbackMode: Boolean;
begin
  Result := FViewOffset > 0;
end;

{ TInputHandler }

constructor TInputHandler.Create(ATextBuffer: TTextBuffer; AVideoController: TVideoController; AConsole: TSedaiNewConsole);
begin
  inherited Create;
  FTextBuffer := ATextBuffer;
  FVideoController := AVideoController;
  FConsole := AConsole;
  FQuitRequested := False;
  FLastKeyDown := SDLK_UNKNOWN;
  FLastChar := #0;
  FHasChar := False;
  FShiftPressed := False;
  FCtrlPressed := False;
  FAltPressed := False;
end;

destructor TInputHandler.Destroy;
begin
  inherited Destroy;
end;

procedure TInputHandler.ClearFlags;
begin
  FLastKeyDown := SDLK_UNKNOWN;
  FLastChar := #0;
  FHasChar := False;
end;

procedure TInputHandler.ClearLastKeyDown;
begin
  FLastKeyDown := SDLK_UNKNOWN;
end;

procedure TInputHandler.ProcessEvents;
var
  Event: TSDL_Event;
begin
  ClearFlags;

  while SDL_PollEvent(@Event) <> 0 do
  begin
    case Event.type_ of
      SDL_QUITEV:
        FQuitRequested := True;

      SDL_KEYDOWN:
        begin
          // Ignore key repeat events for most keys to prevent unwanted command execution
          // BUT allow repeat for navigation and editing keys
          if (Event.key.repeat_ = 0) or
             (Event.key.keysym.sym = SDLK_UP) or
             (Event.key.keysym.sym = SDLK_DOWN) or
             (Event.key.keysym.sym = SDLK_LEFT) or
             (Event.key.keysym.sym = SDLK_RIGHT) or
             (Event.key.keysym.sym = SDLK_BACKSPACE) or
             (Event.key.keysym.sym = SDLK_DELETE) then
          begin
            FLastKeyDown := Event.key.keysym.sym;
            FLastKeyMod := Event.key.keysym.mod_;

            FShiftPressed := (FLastKeyMod and KMOD_SHIFT) <> 0;
            FCtrlPressed := (FLastKeyMod and KMOD_CTRL) <> 0;
            FAltPressed := (FLastKeyMod and KMOD_ALT) <> 0;

            // CTRL+ALT+END: Exit SedaiVision completely
            if (FLastKeyDown = SDLK_END) and FCtrlPressed and FAltPressed then
              FQuitRequested := True
            // CTRL+END: Stop BASIC program (but don't exit)
            else if (FLastKeyDown = SDLK_END) and FCtrlPressed and (not FAltPressed) then
              FStopRequested := True
            // Handle special keys for GETKEY (Unicode-compatible codes)
            else if not FHasChar then
            begin
              case FLastKeyDown of
                SDLK_RETURN, SDLK_KP_ENTER:
                begin
                  FLastChar := #13;  // CR (standard ASCII)
                  FHasChar := True;
                end;
                SDLK_ESCAPE:
                begin
                  FLastChar := #27;  // ESC (standard ASCII)
                  FHasChar := True;
                end;
                SDLK_BACKSPACE:
                begin
                  FLastChar := #8;   // BS (standard ASCII backspace)
                  FHasChar := True;
                end;
                SDLK_DELETE:
                begin
                  FLastChar := #127; // DEL (standard ASCII delete)
                  FHasChar := True;
                end;
                SDLK_TAB:
                begin
                  FLastChar := #9;   // HT (standard ASCII tab)
                  FHasChar := True;
                end;
                SDLK_UP:
                begin
                  FLastChar := #1;   // SOH - Cursor UP
                  FHasChar := True;
                end;
                SDLK_DOWN:
                begin
                  FLastChar := #2;   // STX - Cursor DOWN
                  FHasChar := True;
                end;
                SDLK_LEFT:
                begin
                  FLastChar := #3;   // ETX - Cursor LEFT
                  FHasChar := True;
                end;
                SDLK_RIGHT:
                begin
                  FLastChar := #4;   // EOT - Cursor RIGHT
                  FHasChar := True;
                end;
                SDLK_HOME:
                begin
                  FLastChar := #5;   // ENQ - HOME
                  FHasChar := True;
                end;
                SDLK_END:
                begin
                  if not FCtrlPressed then
                  begin
                    FLastChar := #6;   // ACK - END
                    FHasChar := True;
                  end;
                end;
                SDLK_PAGEUP:
                begin
                  FLastChar := #11;  // VT - PAGE UP
                  FHasChar := True;
                end;
                SDLK_PAGEDOWN:
                begin
                  FLastChar := #12;  // FF - PAGE DOWN
                  FHasChar := True;
                end;
                SDLK_INSERT:
                begin
                  FLastChar := #14;  // SO - INSERT
                  FHasChar := True;
                end;
                // Function keys (codes 128-139)
                SDLK_F1:
                begin
                  FLastChar := #128;
                  FHasChar := True;
                end;
                SDLK_F2:
                begin
                  FLastChar := #129;
                  FHasChar := True;
                end;
                SDLK_F3:
                begin
                  FLastChar := #130;
                  FHasChar := True;
                end;
                SDLK_F4:
                begin
                  FLastChar := #131;
                  FHasChar := True;
                end;
                SDLK_F5:
                begin
                  FLastChar := #132;
                  FHasChar := True;
                end;
                SDLK_F6:
                begin
                  FLastChar := #133;
                  FHasChar := True;
                end;
                SDLK_F7:
                begin
                  FLastChar := #134;
                  FHasChar := True;
                end;
                SDLK_F8:
                begin
                  FLastChar := #135;
                  FHasChar := True;
                end;
                SDLK_F9:
                begin
                  FLastChar := #136;
                  FHasChar := True;
                end;
                SDLK_F10:
                begin
                  FLastChar := #137;
                  FHasChar := True;
                end;
                SDLK_F11:
                begin
                  FLastChar := #138;
                  FHasChar := True;
                end;
                SDLK_F12:
                begin
                  FLastChar := #139;
                  FHasChar := True;
                end;
              end;
            end;
          end;
        end;

      SDL_KEYUP:
        begin
          FShiftPressed := (Event.key.keysym.mod_ and KMOD_SHIFT) <> 0;
          FCtrlPressed := (Event.key.keysym.mod_ and KMOD_CTRL) <> 0;
          FAltPressed := (Event.key.keysym.mod_ and KMOD_ALT) <> 0;
        end;

      SDL_TEXTINPUT:
        begin
          FLastChar := Char(Event.text.text[0]);
          FHasChar := True;
        end;

      // The renderer's targets were reset (Windows D3D loses render-target
      // textures on ALT+TAB / minimize) — rebuild and repaint, else black screen.
      SDL_RENDER_TARGETS_RESET, SDL_RENDER_DEVICE_RESET:
        if Assigned(FConsole) then
          FConsole.RefreshDisplay;

      // Window regained focus / was restored / exposed: force a full repaint.
      SDL_WINDOWEVENT:
        begin
          case Event.window.event of
            SDL_WINDOWEVENT_FOCUS_GAINED,
            SDL_WINDOWEVENT_RESTORED,
            SDL_WINDOWEVENT_EXPOSED,
            SDL_WINDOWEVENT_SHOWN:
              if Assigned(FConsole) then
                FConsole.RefreshDisplay;
          end;
        end;
    end;
  end;
end;

{ TGraphicEngine }

constructor TGraphicEngine.Create(AVideoController: TVideoController);
begin
  inherited Create;
  FVideoController := AVideoController;
end;

destructor TGraphicEngine.Destroy;
begin
  inherited Destroy;
end;

procedure TGraphicEngine.DrawText(X, Y: Integer; const Text: string; Color: TSDL_Color);
var
  Surface: PSDL_Surface;
  Texture: PSDL_Texture;
  DestRect: TSDL_Rect;
begin
  if Text = '' then
    Exit;

  Surface := TTF_RenderUTF8_Blended(FVideoController.Font, PChar(Text), Color);
  if not Assigned(Surface) then
    Exit;

  try
    Texture := SDL_CreateTextureFromSurface(FVideoController.Renderer, Surface);
    if Assigned(Texture) then
    begin
      try
        DestRect.x := X;
        DestRect.y := Y;
        DestRect.w := Surface^.w;
        DestRect.h := Surface^.h;
        SDL_RenderCopy(FVideoController.Renderer, Texture, nil, @DestRect);
      finally
        SDL_DestroyTexture(Texture);
      end;
    end;
  finally
    SDL_FreeSurface(Surface);
  end;
end;

procedure TGraphicEngine.DrawChar(X, Y: Integer; Ch: Char; Color: TSDL_Color);
begin
  DrawText(X, Y, Ch, Color);
end;

procedure TGraphicEngine.FillRect(X, Y, W, H: Integer; Color: TSDL_Color);
var
  Rect: TSDL_Rect;
begin
  Rect.x := X;
  Rect.y := Y;
  Rect.w := W;
  Rect.h := H;
  SDL_SetRenderDrawColor(FVideoController.Renderer, Color.r, Color.g, Color.b, Color.a);
  SDL_RenderFillRect(FVideoController.Renderer, @Rect);
end;

{ TConsoleOutputAdapter }

constructor TConsoleOutputAdapter.Create(ATextBuffer: TTextBuffer; AVideoController: TVideoController);
begin
  inherited Create;
  FTextBuffer := ATextBuffer;
  FVideoController := AVideoController;
  FRenderScreenProc := nil;
  FInitialized := True;
  FSuppressPresent := False;
end;

procedure TConsoleOutputAdapter.SetRenderCallback(AProc: TRenderScreenProc);
begin
  FRenderScreenProc := AProc;
end;

procedure TConsoleOutputAdapter.BeginBatchOutput;
begin
  FSuppressPresent := True;
end;

procedure TConsoleOutputAdapter.EndBatchOutput;
begin
  FSuppressPresent := False;
  // Render once at the end of batch
  Present;
end;

function TConsoleOutputAdapter.Initialize(const Title: string; Width, Height: Integer): Boolean;
begin
  Result := FInitialized;
end;

procedure TConsoleOutputAdapter.Shutdown;
begin
  FInitialized := False;
end;

function TConsoleOutputAdapter.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

procedure TConsoleOutputAdapter.Print(const Text: string; ClearBackground: Boolean);
begin
  FTextBuffer.PutString(Text);
  // Present removed - rendering is handled by the periodic render callback
end;

procedure TConsoleOutputAdapter.PrintLn(const Text: string; ClearBackground: Boolean);
begin
  FTextBuffer.PutString(Text);
  FTextBuffer.NewLine;
  // Present removed - rendering is handled by the periodic render callback
end;

procedure TConsoleOutputAdapter.NewLine;
begin
  FTextBuffer.NewLine;
  // Present removed - rendering is handled by the periodic render callback
end;

procedure TConsoleOutputAdapter.Clear;
begin
  FTextBuffer.Clear;
end;

procedure TConsoleOutputAdapter.ResetPrintState;
begin
  // Reset reverse mode after PRINT statement (C128 behavior)
  if Assigned(FTextBuffer) then
    FTextBuffer.ResetPrintState;
end;

procedure TConsoleOutputAdapter.SetCursor(X, Y: Integer);
begin
  FTextBuffer.CursorX := X;
  FTextBuffer.CursorY := Y;
end;

procedure TConsoleOutputAdapter.MoveCursor(DeltaX, DeltaY: Integer);
begin
  FTextBuffer.CursorX := FTextBuffer.CursorX + DeltaX;
  FTextBuffer.CursorY := FTextBuffer.CursorY + DeltaY;
end;

function TConsoleOutputAdapter.GetCursorX: Integer;
begin
  Result := FTextBuffer.CursorX;
end;

function TConsoleOutputAdapter.GetCursorY: Integer;
begin
  Result := FTextBuffer.CursorY;
end;

procedure TConsoleOutputAdapter.ShowCursor(X, Y: Integer);
begin
  // Not needed for TextBuffer
end;

procedure TConsoleOutputAdapter.HideCursor(X, Y: Integer);
begin
  // Not needed for TextBuffer
end;

procedure TConsoleOutputAdapter.SetColors(Foreground, Background: TColor);
begin
  // Not needed for TextBuffer
end;

procedure TConsoleOutputAdapter.SetFullscreen(Enabled: Boolean);
begin
  // Not needed for TextBuffer
end;

function TConsoleOutputAdapter.IsFullscreen: Boolean;
begin
  Result := False;
end;

procedure TConsoleOutputAdapter.Present;
begin
  // Skip rendering during batch output (reduces slowdowns during I/O operations)
  if FSuppressPresent then
    Exit;
  // First render the TextBuffer content to screen via callback
  if Assigned(FRenderScreenProc) then
    FRenderScreenProc();
  // Then present to display
  if Assigned(FVideoController) then
    FVideoController.Present;
end;

function TConsoleOutputAdapter.ShouldQuit: Boolean;
begin
  Result := False;
end;

function TConsoleOutputAdapter.GetActualCols: Integer;
begin
  Result := FTextBuffer.Cols;
end;

function TConsoleOutputAdapter.GetActualRows: Integer;
begin
  Result := FTextBuffer.Rows;
end;

procedure TConsoleOutputAdapter.MarkPromptRow;
begin
  // Not needed for TextBuffer
end;

procedure TConsoleOutputAdapter.OnUserInput;
begin
  // Delegates to VideoController to handle scroll/pager
  FVideoController.OnUserInput;
end;

function TConsoleOutputAdapter.HandleScrollKeys(Key, Modifiers: Integer): Boolean;
begin
  // Delegates to VideoController to handle scroll/pager
  Result := FVideoController.HandleScrollKeys(Key, Modifiers);
end;

procedure TConsoleOutputAdapter.ProcessScrollInput;
begin
  // Delegates to VideoController to handle scroll/pager
  FVideoController.ProcessScrollInput;
end;

function TConsoleOutputAdapter.GetInScrollMode: Boolean;
begin
  // Delegates to VideoController to handle scroll mode
  Result := FVideoController.GetInScrollMode;
end;

function TConsoleOutputAdapter.SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean; SplitLine: Integer): Boolean;
var
  NewModeInfo: TGraphicModeInfo;
begin
  // Delegate to VideoController
  if Assigned(FVideoController) then
  begin
    Result := FVideoController.SetGraphicMode(Mode, ClearBuffer, SplitLine);
    // Resize text buffer to match new mode's text dimensions
    NewModeInfo := FVideoController.ModeInfo;
    if NewModeInfo.ModeType in [mtText, mtMixed] then
      FTextBuffer.Resize(NewModeInfo.TextRows, NewModeInfo.TextCols);
  end
  else
    Result := False;
end;

function TConsoleOutputAdapter.GetGraphicMode: TGraphicMode;
begin
  // Delegate to VideoController
  if Assigned(FVideoController) then
    Result := FVideoController.GetGraphicMode
  else
    Result := gm40ColText;
end;

function TConsoleOutputAdapter.IsInGraphicsMode: Boolean;
begin
  // Delegate to VideoController
  if Assigned(FVideoController) then
    Result := FVideoController.IsInGraphicsMode
  else
    Result := False;
end;

procedure TConsoleOutputAdapter.ClearScreen(Mode: Integer);
begin
  // Always clear the text buffer - it's used across all modes for text overlay
  if Assigned(FTextBuffer) then
    FTextBuffer.Clear;

  // Also delegate to VideoController for graphics buffer
  if Assigned(FVideoController) then
    FVideoController.ClearScreen(Mode);
end;

procedure TConsoleOutputAdapter.SetPixel(X, Y: Integer; RGB: UInt32);
begin
  // Delegate to VideoController for graphics operations
  if Assigned(FVideoController) then
    FVideoController.SetPixel(X, Y, RGB);
end;

procedure TConsoleOutputAdapter.SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex);
begin
  // Delegate to VideoController for graphics operations
  if Assigned(FVideoController) then
    FVideoController.SetPixel(X, Y, PaletteIndex);
end;

function TConsoleOutputAdapter.GetPixel(X, Y: Integer): UInt32;
begin
  // Delegate to VideoController for graphics operations
  if Assigned(FVideoController) then
    Result := FVideoController.GetPixel(X, Y)
  else
    Result := 0;
end;

function TConsoleOutputAdapter.GetPixelIndex(X, Y: Integer): TPaletteIndex;
begin
  // Delegate to VideoController for graphics operations
  if Assigned(FVideoController) then
    Result := FVideoController.GetPixelIndex(X, Y)
  else
    Result := 0;
end;

procedure TConsoleOutputAdapter.EnablePalette(Enable: Boolean);
begin
  // Delegate to VideoController for graphics operations
  if Assigned(FVideoController) then
    FVideoController.EnablePalette(Enable);
end;

function TConsoleOutputAdapter.IsPaletteEnabled: Boolean;
begin
  // Delegate to VideoController for graphics operations
  if Assigned(FVideoController) then
    Result := FVideoController.IsPaletteEnabled
  else
    Result := False;
end;

procedure TConsoleOutputAdapter.SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
begin
  // Delegate to VideoController for graphics operations
  if Assigned(FVideoController) then
    FVideoController.SetPaletteColor(Index, RGB);
end;

function TConsoleOutputAdapter.GetPaletteColor(Index: TPaletteIndex): UInt32;
begin
  // Delegate to VideoController for graphics operations
  if Assigned(FVideoController) then
    Result := FVideoController.GetPaletteColor(Index)
  else
    Result := 0;
end;

procedure TConsoleOutputAdapter.ResetPalette;
begin
  // Delegate to VideoController for graphics operations
  if Assigned(FVideoController) then
    FVideoController.ResetPalette;
end;

procedure TConsoleOutputAdapter.LoadPalettePreset(PresetId: Integer);
begin
  if Assigned(FVideoController) then
    FVideoController.LoadPalettePreset(PresetId);
end;

procedure TConsoleOutputAdapter.SetPaletteColorRGBA(Index: TPaletteIndex; R, G, B: Byte; A: Byte = 255);
begin
  // Delegate to VideoController for graphics operations
  if Assigned(FVideoController) then
    FVideoController.SetPaletteColorRGBA(Index, R, G, B, A);
end;

function TConsoleOutputAdapter.LoadPaletteFromJSON(const FileName: string): Boolean;
begin
  // Delegate to VideoController for graphics operations
  if Assigned(FVideoController) then
    Result := FVideoController.LoadPaletteFromJSON(FileName)
  else
    Result := False;
end;

function TConsoleOutputAdapter.SavePaletteToJSON(const FileName: string): Boolean;
begin
  // Delegate to VideoController for graphics operations
  if Assigned(FVideoController) then
    Result := FVideoController.SavePaletteToJSON(FileName)
  else
    Result := False;
end;

function TConsoleOutputAdapter.GetLastPaletteError: string;
begin
  // Delegate to VideoController for graphics operations
  if Assigned(FVideoController) then
    Result := FVideoController.GetLastPaletteError
  else
    Result := 'Video controller not initialized';
end;

// Shape drawing stubs - TConsoleOutputAdapter
procedure TConsoleOutputAdapter.DrawBox(X1, Y1, X2, Y2: Integer; Angle: Double);
begin
  // Not implemented
end;

procedure TConsoleOutputAdapter.DrawBoxStyled(X1, Y1, X2, Y2: Integer; const Style: TShapeStyle; Angle: Double);
begin
  // Not implemented
end;

procedure TConsoleOutputAdapter.DrawBoxWithColor(X1, Y1, X2, Y2: Integer; Color: UInt32; Angle: Double; Filled: Boolean);
begin
  // Delegate to VideoController
  if Assigned(FVideoController) then
    FVideoController.DrawBoxWithColor(X1, Y1, X2, Y2, Color, Angle, Filled);
end;

procedure TConsoleOutputAdapter.SetBorderStyle(const Style: TBorderStyle);
begin
  // Not implemented
end;

procedure TConsoleOutputAdapter.SetFillStyle(const Style: TFillStyleDef);
begin
  // Not implemented
end;

function TConsoleOutputAdapter.GetBorderStyle: TBorderStyle;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function TConsoleOutputAdapter.GetFillStyle: TFillStyleDef;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

procedure TConsoleOutputAdapter.SetFastMode(Enabled: Boolean);
begin
  // FAST mode now also suppresses Present calls for better performance
  FSuppressPresent := Enabled;
  if Assigned(FVideoController) then
    FVideoController.SetFastMode(Enabled);
end;

function TConsoleOutputAdapter.GetFastMode: Boolean;
begin
  if Assigned(FVideoController) then
    Result := FVideoController.GetFastMode
  else
    Result := False;
end;

procedure TConsoleOutputAdapter.SetFastModeAlpha(Alpha: Byte);
begin
  if Assigned(FVideoController) then
    FVideoController.SetFastModeAlpha(Alpha);
end;

function TConsoleOutputAdapter.GetFastModeAlpha: Byte;
begin
  if Assigned(FVideoController) then
    Result := FVideoController.GetFastModeAlpha
  else
    Result := 255;
end;

// === COLOR command support ===
procedure TConsoleOutputAdapter.SetColorSource(Source, Color: Integer);
begin
  if Assigned(FVideoController) then
  begin
    FVideoController.SetColorSource(Source, Color);
    // Sync TextBuffer current FG so new text uses the updated foreground
    if Assigned(FTextBuffer) and (Source = 1) then
      FTextBuffer.CurrentFGIndex := Byte(FVideoController.GetColorSourceDirect(1));
    // Background: no buffer changes needed - cells use BG_USE_DEFAULT sentinel,
    // renderer resolves it to FBackgroundIndex at draw time
    // Force full redraw when background or border changes
    if Source in [0, 4] then
      if Assigned(FTextBuffer) then
        FTextBuffer.MarkAllDirty;
  end;
end;

function TConsoleOutputAdapter.GetColorSource(Source: Integer): Integer;
begin
  if Assigned(FVideoController) then
    Result := FVideoController.GetColorSource(Source)
  else
    Result := 0;
end;

procedure TConsoleOutputAdapter.SetColorSourceDirect(Source, Color: Integer);
begin
  if Assigned(FVideoController) then
  begin
    FVideoController.SetColorSourceDirect(Source, Color);
    if Assigned(FTextBuffer) and (Source = 1) then
      FTextBuffer.CurrentFGIndex := Byte(FVideoController.GetColorSourceDirect(1));
    if Source in [0, 4] then
      if Assigned(FTextBuffer) then
        FTextBuffer.MarkAllDirty;
  end;
end;

function TConsoleOutputAdapter.GetColorSourceDirect(Source: Integer): Integer;
begin
  if Assigned(FVideoController) then
    Result := FVideoController.GetColorSourceDirect(Source)
  else
    Result := 0;
end;

// === WIDTH command support ===
procedure TConsoleOutputAdapter.SetLineWidth(Width: Integer);
begin
  if Assigned(FVideoController) then
    FVideoController.SetLineWidth(Width);
end;

// === SCALE command support ===
procedure TConsoleOutputAdapter.SetScale(Enabled: Boolean; XMax, YMax: Integer);
begin
  if Assigned(FVideoController) then
    FVideoController.SetScale(Enabled, XMax, YMax);
end;

// === PAINT command support ===
procedure TConsoleOutputAdapter.FloodFill(Source: Integer; X, Y: Double; Mode: Integer);
begin
  if Assigned(FVideoController) then
    FVideoController.FloodFill(Source, X, Y, Mode);
end;

// === WINDOW command support ===
procedure TConsoleOutputAdapter.SetWindow(Col1, Row1, Col2, Row2: Integer; DoClear: Boolean);
begin
  // Set window in TextBuffer (controls text output)
  if Assigned(FTextBuffer) then
  begin
    FTextBuffer.SetWindow(Col1, Row1, Col2, Row2);
    if DoClear then
      FTextBuffer.ClearWindow;
  end;
  // Also set in VideoController (for compatibility)
  if Assigned(FVideoController) then
    FVideoController.SetWindow(Col1, Row1, Col2, Row2, DoClear);
end;

function TConsoleOutputAdapter.GetWindowLines: Integer;
begin
  // Use TextBuffer's window info
  if Assigned(FTextBuffer) then
    Result := FTextBuffer.GetWindowLines
  else
    Result := 25;
end;

function TConsoleOutputAdapter.GetWindowCols: Integer;
begin
  // Use TextBuffer's window info
  if Assigned(FTextBuffer) then
    Result := FTextBuffer.GetWindowCols
  else
    Result := 40;
end;

function TConsoleOutputAdapter.GetScreenWidth: Integer;
begin
  if Assigned(FVideoController) then
    Result := FVideoController.GetScreenWidth
  else
    Result := 40;
end;

// === Screen memory access (PEEK/POKE compatibility) ===
// Memory is always accessible - the graphics mode only affects display.
// This maps to the 40x25 text buffer used by GRAPHIC 0, 2, 4.

function TConsoleOutputAdapter.GetCharAt(Col, Row: Integer): Byte;
begin
  if Assigned(FTextBuffer) then
    Result := FTextBuffer.GetCharAt(Col, Row)
  else
    Result := 32; // Space
end;

procedure TConsoleOutputAdapter.SetCharAt(Col, Row: Integer; Ch: Byte);
begin
  if Assigned(FTextBuffer) then
    FTextBuffer.SetCharAt(Col, Row, Ch);
end;

function TConsoleOutputAdapter.GetColorAt(Col, Row: Integer): Byte;
begin
  if Assigned(FTextBuffer) then
    Result := FTextBuffer.GetColorAt(Col, Row)
  else
    Result := 0;
end;

procedure TConsoleOutputAdapter.SetColorAt(Col, Row: Integer; Color: Byte);
begin
  if Assigned(FTextBuffer) then
    FTextBuffer.SetColorAt(Col, Row, Color);
end;

// === SSHAPE/GSHAPE command support ===
function TConsoleOutputAdapter.SaveShape(X1, Y1, X2, Y2: Double): string;
begin
  if Assigned(FVideoController) then
    Result := FVideoController.SaveShape(X1, Y1, X2, Y2)
  else
    Result := '';
end;

procedure TConsoleOutputAdapter.LoadShape(const Data: string; X, Y: Double; Mode: Integer);
begin
  if Assigned(FVideoController) then
    FVideoController.LoadShape(Data, X, Y, Mode);
end;

procedure TConsoleOutputAdapter.DrawCircleWithColor(X, Y, XR, YR: Integer; Color: UInt32;
                                                   SA: Double; EA: Double; Angle: Double; Inc: Double);
begin
  // Delegate to VideoController
  if Assigned(FVideoController) then
    FVideoController.DrawCircleWithColor(X, Y, XR, YR, Color, SA, EA, Angle, Inc);
end;

procedure TConsoleOutputAdapter.DrawLine(X1, Y1, X2, Y2: Integer; Color: UInt32);
begin
  // Delegate to VideoController
  if Assigned(FVideoController) then
    FVideoController.DrawLine(X1, Y1, X2, Y2, Color);
end;

procedure TConsoleOutputAdapter.DrawLineInternal(X1, Y1, X2, Y2: Integer;
                                                 UseIndex: Boolean; PalIndex: TPaletteIndex; RGBAColor: UInt32);
begin
  // Delegate to VideoController
  if Assigned(FVideoController) then
    FVideoController.DrawLineInternal(X1, Y1, X2, Y2, UseIndex, PalIndex, RGBAColor);
end;

procedure TConsoleOutputAdapter.SetPixelCursor(X, Y: Integer);
begin
  // Delegate to VideoController
  if Assigned(FVideoController) then
    FVideoController.SetPixelCursor(X, Y);
end;

function TConsoleOutputAdapter.GetPixelCursorX: Integer;
begin
  // Delegate to VideoController
  if Assigned(FVideoController) then
    Result := FVideoController.GetPixelCursorX
  else
    Result := 0;
end;

function TConsoleOutputAdapter.GetPixelCursorY: Integer;
begin
  // Delegate to VideoController
  if Assigned(FVideoController) then
    Result := FVideoController.GetPixelCursorY
  else
    Result := 0;
end;

{ TSedaiNewConsole }

constructor TSedaiNewConsole.Create;
var
  I: Integer;
begin
  inherited Create;
  FVideoController := TVideoController.Create;
  FTextBuffer := TTextBuffer.Create(25, 40);  // Create TextBuffer immediately
  FGraphicEngine := nil;
  FInputHandler := TInputHandler.Create(FTextBuffer, FVideoController, Self);  // Passa i riferimenti + console
  FProgramInputHandler := TProgramInputHandler.Create(FVideoController);  // Dedicated input for VM
  FProgramInputHandler.SetCursorCallback(@SetCursorEnabled);  // Wire cursor toggle for INPUT
  FRunning := False;
  FCursorVisible := True;
  FCursorEnabled := True;  // Cursor visible by default (REPL mode)
  FLastCursorBlink := 0;
  SetLength(FInputHistory, INPUT_HISTORY_SIZE);
  FHistoryCount := 0;
  FHistoryPos := -1;
  FCtrlCEnabled := True;  // Default: CTRL+C enabled

  // Initialize shared components
  FProgramMemory := TProgramMemory.Create;
  FCommandRouter := TCommandRouter.Create;

  // Initialize VM-based execution
  FBytecodeVM := TBytecodeVM.Create;
  FBytecodeVM.SetOutputDevice(FVideoController);
  // FreeBASIC graphics: the video controller is also the graphics backend (same instance) so the new
  // operation-level ops (SCREENRES/PSET/POINT, ...) draw on screen. Owned by the console (OwnedObj=nil).
  FBytecodeVM.SetGraphicsBackend(FVideoController);
  // Use dedicated program input handler for VM (NOT the console input handler)
  FBytecodeVM.SetInputDevice(FProgramInputHandler);
  // Create sprite engine with SDL2 renderer and palette resolver
  FSpriteEngine := TC128SpriteEngine.Create(FVideoController.Renderer,
    @FVideoController.PaletteIndexToSDLColor);
  FBytecodeVM.SetSpriteManager(FSpriteEngine as ISpriteManager);
  // Create and set memory mapper for PEEK/POKE support
  FMemoryMapper := TC128MemoryMapper.Create(FVideoController, FSpriteEngine as ISpriteManager);
  FBytecodeVM.SetMemoryMapper(FMemoryMapper as IMemoryMapper);
  // Set disk file I/O handlers for DOPEN/DCLOSE/PRINT#/INPUT#/GET#
  FBytecodeVM.OnDiskFile := @HandleDiskFile;
  FBytecodeVM.OnFileData := @HandleFileData;

  FImmediateCompiler := TImmediateCompiler.Create;

  // Initialize bytecode mode
  FBytecodeMode := False;
  FLoadedBytecode := nil;

  // Initialize AUTO line numbering (disabled by default)
  FAutoLineNumber := 0;
  FAutoLineIncrement := 10;

  // Initialize debug mode (disabled by default)
  FDebugMode := False;
  FTraceActive := False;
  FDebugger := TSedaiDebugger.Create;
  FDebugger.OnTrace := @HandleDebuggerTrace;
  FDebugger.OnPause := @HandleDebuggerPause;
  // Connect debugger to VM
  FBytecodeVM.SetDebugger(FDebugger);

  // Initialize error tracking
  FLastErrorLine := 0;
  FLastErrorCode := 0;
  FLastErrorMessage := '';

  // Initialize file handles (all closed)
  for I := 1 to 15 do
  begin
    FFileHandles[I] := nil;
    FFileNames[I] := '';
    FFileModes[I] := '';
  end;
end;

destructor TSedaiNewConsole.Destroy;
begin
  // Close all open file handles
  CloseAllFileHandles;

  // Save command history to file
  SaveHistory;

  // Clear debugger from VM and free debugger
  if Assigned(FBytecodeVM) then
    FBytecodeVM.SetDebugger(nil);
  if Assigned(FDebugger) then
  begin
    FDebugger.OnTrace := nil;
    FDebugger.OnPause := nil;
    FDebugger.Free;
    FDebugger := nil;
  end;

  // Clear VM references to I/O devices and memory mapper before freeing
  if Assigned(FBytecodeVM) then
  begin
    FBytecodeVM.SetSpriteManager(nil);  // Clear sprite manager reference
    FBytecodeVM.SetOutputDevice(nil);
    FBytecodeVM.SetInputDevice(nil);
    FBytecodeVM.SetMemoryMapper(nil);  // Clear reference before freeing mapper
    FBytecodeVM.Free;
    FBytecodeVM := nil;
  end;

  // Free memory mapper (after VM no longer references it)
  if Assigned(FMemoryMapper) then
  begin
    FMemoryMapper.Free;
    FMemoryMapper := nil;
  end;

  // Free sprite engine (after mapper and VM no longer reference it)
  if Assigned(FSpriteEngine) then
  begin
    FSpriteEngine.Free;
    FSpriteEngine := nil;
  end;

  if Assigned(FImmediateCompiler) then
  begin
    FImmediateCompiler.Free;
    FImmediateCompiler := nil;
  end;

  if Assigned(FLoadedBytecode) then
  begin
    FLoadedBytecode.Free;
    FLoadedBytecode := nil;
  end;

  // Now free program memory and router
  if Assigned(FProgramMemory) then
  begin
    FProgramMemory.Free;
    FProgramMemory := nil;
  end;

  if Assigned(FCommandRouter) then
  begin
    FCommandRouter.Free;
    FCommandRouter := nil;
  end;

  // Clear output device reference in ProgramInputHandler before freeing ConsoleOutputAdapter
  // to avoid dangling pointer
  if Assigned(FProgramInputHandler) then
    FProgramInputHandler.SetOutputDevice(nil);

  // Other components - clear callback before freeing to avoid calling freed methods
  if Assigned(FConsoleOutputAdapter) then
  begin
    FConsoleOutputAdapter.SetRenderCallback(nil);
    FConsoleOutputAdapter.Free;
    FConsoleOutputAdapter := nil;
  end;

  if Assigned(FGraphicEngine) then
  begin
    FGraphicEngine.Free;
    FGraphicEngine := nil;
  end;

  if Assigned(FTextBuffer) then
  begin
    FTextBuffer.Free;
    FTextBuffer := nil;
  end;

  if Assigned(FInputHandler) then
  begin
    FInputHandler.Free;
    FInputHandler := nil;
  end;

  if Assigned(FProgramInputHandler) then
  begin
    FProgramInputHandler.Free;
    FProgramInputHandler := nil;
  end;

  if Assigned(FVideoController) then
  begin
    FVideoController.Free;
    FVideoController := nil;
  end;

  inherited Destroy;
end;

function TSedaiNewConsole.GetFunctionKeyDefinition(KeyNum: Integer): string;
begin
  // Get function key definition from VM
  if Assigned(FBytecodeVM) then
    Result := FBytecodeVM.GetFunctionKey(KeyNum)
  else
    Result := '';
end;

function TSedaiNewConsole.GetSystemInfo: string;
begin
  {$IFDEF WINDOWS}
  Result := 'Windows';
  {$ENDIF}
  {$IFDEF LINUX}
  Result := 'Linux';
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := 'macOS';
  {$ENDIF}
  {$IFDEF BSD}
  Result := 'BSD';
  {$ENDIF}
  {$IFNDEF WINDOWS}
  {$IFNDEF LINUX}
  {$IFNDEF DARWIN}
  {$IFNDEF BSD}
  Result := 'Unknown OS';
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

function TSedaiNewConsole.GetArchitecture: string;
begin
  {$IFDEF CPU64}
  Result := 'x64';
  {$ELSE}
  {$IFDEF CPUX86}
  Result := 'x86';
  {$ELSE}
  {$IFDEF CPUARM}
  Result := 'ARM';
  {$ELSE}
  {$IFDEF CPUAARCH64}
  Result := 'ARM64';
  {$ELSE}
  Result := 'Unknown';
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

function TSedaiNewConsole.GetAvailableMemory: string;
{$IFDEF WINDOWS}
var
  MemStatusEx: TMemoryStatusEx;
  TotalMB, FreeMB: Int64;
begin
  MemStatusEx.dwLength := SizeOf(MemStatusEx);
  if GlobalMemoryStatusEx(MemStatusEx) then
  begin
    TotalMB := MemStatusEx.ullTotalPhys div (1024 * 1024);
    FreeMB := MemStatusEx.ullAvailPhys div (1024 * 1024);
    Result := Format('%d/%d MB RAM free', [FreeMB, TotalMB]);
  end
  else
    Result := 'RAM info unavailable';
end;
{$ELSE}
begin
  // On Unix systems, would need to parse /proc/meminfo
  Result := 'RAM info unavailable';
end;
{$ENDIF}

{ Debugger callback: trace output (prints line numbers during execution) }
procedure TSedaiNewConsole.HandleDebuggerTrace(Sender: TObject; LineNumber: Integer);
begin
  if Assigned(FTextBuffer) then
    FTextBuffer.PutString('[' + IntToStr(LineNumber) + ']');
end;

{ Debugger callback: pause (breakpoint hit or step completed) }
procedure TSedaiNewConsole.HandleDebuggerPause(Sender: TObject; LineNumber: Integer; const Reason: string);
begin
  if Assigned(FTextBuffer) then
  begin
    FTextBuffer.NewLine;
    FTextBuffer.PutString('BREAK AT LINE ' + IntToStr(LineNumber));
    if Reason <> '' then
      FTextBuffer.PutString(' (' + Reason + ')');
    FTextBuffer.NewLine;
    FTextBuffer.PutString('READY.'); FReadyPrinted := True;
    FTextBuffer.NewLine;
  end;
  // Force screen update
  RenderScreen;
  UpdateCursor;
  if Assigned(FVideoController) then
    FVideoController.Present;
end;

procedure TSedaiNewConsole.ShowSplashScreen;
var
  Lines: array of string;
  MaxLen, MaxAvailable: Integer;
  i, StartX, StartPadding: Integer;
  BorderLine: string;
  OSInfo: string;
begin
  // Calculate maximum available space (columns - 4 for borders "* " and " *")
  MaxAvailable := FTextBuffer.Cols - 4;
  if MaxAvailable < 20 then
    MaxAvailable := 20; // Minimum to avoid crash

  // Build splash lines
  SetLength(Lines, 6);
  Lines[0] := 'SedaiBasic v2.0';
  Lines[1] := '(C) 2025 Maurizio Cammalleri';
  Lines[2] := 'GNU GPL v3';
  Lines[3] := '';

  OSInfo := GetSystemInfo + ' (' + GetArchitecture + ') ';
  Lines[4] := OSInfo;
  Lines[5] := GetAvailableMemory;

  // Tronca linee troppo lunghe
  for i := 0 to High(Lines) do
    if Length(Lines[i]) > MaxAvailable then
      Lines[i] := Copy(Lines[i], 1, MaxAvailable - 3) + '...';

  // Calculate max length (limitato a MaxAvailable)
  MaxLen := 0;
  for i := 0 to High(Lines) do
    if Length(Lines[i]) > MaxLen then
      MaxLen := Length(Lines[i]);

  // Create border
  BorderLine := StringOfChar('*', MaxLen + 4);

  // Calculate horizontal centering only
  StartX := (FTextBuffer.Cols - (MaxLen + 4)) div 2;
  if StartX < 0 then
    StartX := 0;

  // Clear and position cursor at top
  FTextBuffer.Clear;
  FTextBuffer.CursorX := 0;
  FTextBuffer.CursorY := 0;

  // Riga vuota prima dello splash
  FTextBuffer.NewLine;

  // Draw top border
  FTextBuffer.CursorX := StartX;
  FTextBuffer.PutString(BorderLine);
  FTextBuffer.NewLine;

  // Riga vuota dopo il bordo superiore
  FTextBuffer.CursorX := StartX;
  FTextBuffer.PutString('*');
  FTextBuffer.PutString(StringOfChar(' ', MaxLen + 2));
  FTextBuffer.PutString('*');
  FTextBuffer.NewLine;

  // Draw content lines (centered horizontally)
  for i := 0 to High(Lines) do
  begin
    FTextBuffer.CursorX := StartX;
    FTextBuffer.PutString('* ');
    // Center text within the available space
    if Lines[i] <> '' then
    begin
      StartPadding := (MaxLen - Length(Lines[i])) div 2;
      FTextBuffer.PutString(StringOfChar(' ', StartPadding));
      FTextBuffer.PutString(Lines[i]);
      FTextBuffer.PutString(StringOfChar(' ', MaxLen - Length(Lines[i]) - StartPadding));
    end
    else
      FTextBuffer.PutString(StringOfChar(' ', MaxLen));
    FTextBuffer.PutString(' *');
    FTextBuffer.NewLine;
  end;

  // Riga vuota prima del bordo inferiore
  FTextBuffer.CursorX := StartX;
  FTextBuffer.PutString('*');
  FTextBuffer.PutString(StringOfChar(' ', MaxLen + 2));
  FTextBuffer.PutString('*');
  FTextBuffer.NewLine;

  // Draw bottom border
  FTextBuffer.CursorX := StartX;
  FTextBuffer.PutString(BorderLine);
  FTextBuffer.NewLine;

  // After splash: empty line + READY. + cursor on next line
  FTextBuffer.NewLine;  // Riga vuota
  FTextBuffer.CursorX := 0;
  FTextBuffer.PutString('READY.'); FReadyPrinted := True;
  FTextBuffer.NewLine;  // Cursore sulla riga successiva
  FTextBuffer.CursorX := 0;

  // Render splash screen with READY
  RenderScreen;
  UpdateCursor;
  FVideoController.Present;

  // Small delay to show splash
  SDL_Delay(1000);
end;

function TSedaiNewConsole.Initialize(Mode: TGraphicMode; Fullscreen: Boolean = False): Boolean;
begin
  Result := False;

  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Initializing SedaiNewConsole...');{$ENDIF}
  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Initializing VideoController...');{$ENDIF}

  if not FVideoController.Initialize(Mode, Fullscreen) then
  begin
    {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: VideoController initialization failed');{$ENDIF}
    Exit;
  end;

  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: VideoController initialized successfully');{$ENDIF}

  // The TextBuffer was already created in the constructor, but may have wrong dimensions
  // If dimensions don't match, recreate it
  if (FTextBuffer.Rows <> FVideoController.ModeInfo.TextRows) or 
     (FTextBuffer.Cols <> FVideoController.ModeInfo.TextCols) then
  begin
    FTextBuffer.Free;
    FTextBuffer := TTextBuffer.Create(FVideoController.ModeInfo.TextRows, FVideoController.ModeInfo.TextCols);
  end;
  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: TextBuffer created with ', FVideoController.ModeInfo.TextRows, ' rows x ', FVideoController.ModeInfo.TextCols, ' cols');{$ENDIF}

  // Now that SDL renderer exists, connect it to the sprite engine
  if Assigned(FSpriteEngine) then
    FSpriteEngine.SetRenderer(FVideoController.Renderer);

  FGraphicEngine := TGraphicEngine.Create(FVideoController);
  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: GraphicEngine created');{$ENDIF}

  // Create the adapter for output that writes to the TextBuffer
  FConsoleOutputAdapter := TConsoleOutputAdapter.Create(FTextBuffer, FVideoController);
  FConsoleOutputAdapter.SetRenderCallback(@Self.RenderScreen);  // Set callback for RenderScreen calls
  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: ConsoleOutputAdapter created');{$ENDIF}

  // Set output device on program input handler for synchronized I/O
  if Assigned(FProgramInputHandler) then
    FProgramInputHandler.SetOutputDevice(FConsoleOutputAdapter);

  // Connect I/O devices to VM
  if Assigned(FBytecodeVM) then
  begin
    // VM uses console output adapter to write to TextBuffer
    FBytecodeVM.SetOutputDevice(FConsoleOutputAdapter);
    // Use dedicated program input handler for VM (NOT the console input handler)
    FBytecodeVM.SetInputDevice(FProgramInputHandler);
    {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: BytecodeVM configured with ConsoleOutputAdapter and ProgramInputHandler');{$ENDIF}
  end;

  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: SedaiNewConsole initialization complete');{$ENDIF}

  // Load command history from file
  LoadHistory;

  Result := True;
end;

procedure TSedaiNewConsole.RenderScreen;
var
  PrevCursorX, PrevCursorY: Integer;
begin
  // In graphics mode, render the graphics texture (caller handles Present)
  if FVideoController.IsInGraphicsMode then
  begin
    // Clear and render the graphics buffer to screen
    FVideoController.ClearBorder;
    FVideoController.RenderGraphicsTexture;

    // For mixed modes (split screen), also render text at the bottom
    if FVideoController.ModeInfo.ModeType = mtMixed then
    begin
      PrevCursorX := FTextBuffer.CursorX;
      PrevCursorY := FTextBuffer.CursorY;
      FVideoController.RenderText(FTextBuffer);
      FTextBuffer.CursorX := PrevCursorX;
      FTextBuffer.CursorY := PrevCursorY;
      UpdateCursor;
    end;

    // Render sprites on top of graphics (clipped to viewport)
    if Assigned(FSpriteEngine) then
    begin
      FSpriteEngine.SetNativeResolution(FVideoController.ModeInfo.NativeWidth,
        FVideoController.ModeInfo.NativeHeight);
      FSpriteEngine.SetViewportOffset(FVideoController.ViewportX, FVideoController.ViewportY,
        FVideoController.ViewportWidth, FVideoController.ViewportHeight);
      FSpriteEngine.RenderSprites;
    end;
    Exit;
  end;

  // Store cursor position
  PrevCursorX := FTextBuffer.CursorX;
  PrevCursorY := FTextBuffer.CursorY;

  // RenderText blits full-window texture (includes border) - no need for ClearBorder
  FVideoController.RenderText(FTextBuffer);

  // Render sprites on top of text (clipped to viewport)
  if Assigned(FSpriteEngine) then
  begin
    FSpriteEngine.SetNativeResolution(FVideoController.ModeInfo.NativeWidth,
      FVideoController.ModeInfo.NativeHeight);
    FSpriteEngine.SetViewportOffset(FVideoController.ViewportX, FVideoController.ViewportY,
      FVideoController.ViewportWidth, FVideoController.ViewportHeight);
    FSpriteEngine.RenderSprites;
  end;

  // Restore cursor position
  FTextBuffer.CursorX := PrevCursorX;
  FTextBuffer.CursorY := PrevCursorY;

  // Draw blinking cursor (always update during render for INPUT)
  UpdateCursor;
end;

procedure TSedaiNewConsole.RefreshDisplay;
begin
  // Rebuild render targets lost on focus regain / device reset (ALT+TAB on
  // Windows D3D), then repaint the full screen so the console is not left black.
  FVideoController.ForceTextureRebuild;
  RenderScreen;
  UpdateCursor;
  FVideoController.Present;
end;

procedure TSedaiNewConsole.ToggleFullscreen;
begin
  // SetMode (inside ToggleFullscreen) destroys and recreates the SDL renderer,
  // window and textures. Reconnect everything that caches the renderer, else the
  // sprite engine keeps a freed renderer and its per-frame clip operations corrupt
  // rendering (intermittently hiding the cursor), and the graphics texture stays
  // black. Shared by the REPL loop and HandleVMEventPoll so CTRL+F also works
  // while a program is running.
  FVideoController.ToggleFullscreen;
  FVideoController.ForceTextureRebuild;
  if Assigned(FSpriteEngine) then
    FSpriteEngine.SetRenderer(FVideoController.Renderer);

  // Force the cursor visible so the first post-toggle frame shows it regardless of
  // the blink phase (UpdateCursor still suppresses it while a program is running).
  FCursorVisible := True;
  FLastCursorBlink := SDL_GetTicks;

  RenderScreen;
  UpdateCursor;
  FVideoController.Present;
end;

function TSedaiNewConsole.CheckCursorBlink: Boolean;
var
  CurrentTime: Cardinal;
begin
  Result := False;
  CurrentTime := SDL_GetTicks;

  // Toggle cursor visibility every 500ms (C64 timing)
  if CurrentTime - FLastCursorBlink >= CURSOR_BLINK_MS then
  begin
    FCursorVisible := not FCursorVisible;
    FLastCursorBlink := CurrentTime;
    Result := True;  // Cursor state changed
  end;
end;

procedure TSedaiNewConsole.UpdateCursor;
var
  CursorX, CursorY: Integer;
  CurrentChar: Char;
begin
  // Don't show cursor when suppressed (during VM execution, except INPUT)
  if not FCursorEnabled then
    Exit;

  // Don't show cursor in bitmap graphics mode
  if FVideoController.ModeInfo.ModeType = mtBitmap then
    Exit;

  // Don't show cursor when viewing scrollback
  if FTextBuffer.IsInScrollbackMode then
    Exit;

  if FCursorVisible then
  begin
    // Calculate cursor position in viewport
    CursorX := FVideoController.ViewportX + (FTextBuffer.CursorX * FVideoController.CharWidth);
    CursorY := FVideoController.ViewportY + (FTextBuffer.CursorY * FVideoController.CharHeight);
    // For mixed modes, offset cursor to text area at bottom of viewport
    if FVideoController.ModeInfo.ModeType = mtMixed then
      CursorY := FVideoController.ViewportY + FVideoController.ViewportHeight
                 - (FVideoController.ModeInfo.TextRows * FVideoController.CharHeight)
                 + (FTextBuffer.CursorY * FVideoController.CharHeight);

    // Get character at cursor position (space if beyond line end)
    CurrentChar := FTextBuffer.GetCharAtCursor;

    // Draw cursor as filled rectangle with current text FG color (follows PETSCII color codes)
    FGraphicEngine.FillRect(CursorX, CursorY,
                           FVideoController.CharWidth,
                           FVideoController.CharHeight,
                           FVideoController.PaletteIndexToSDLColor(FTextBuffer.CurrentFGIndex));

    // Draw the character in reverse (BG color on FG background)
    FGraphicEngine.DrawChar(CursorX, CursorY, CurrentChar,
                           FVideoController.PaletteIndexToSDLColor(FTextBuffer.CurrentBGIndex));
  end;
end;

procedure TSedaiNewConsole.ExecuteDirectCommand(const Command: string);
var
  FirstChar: Char;
  LineNum: Integer;
  RestOfLine: string;
  SpacePos: Integer;
begin
  if Length(Command) = 0 then
    Exit;

  FirstChar := Command[1];

  // If it starts with a number, it's a program line
  if FirstChar in ['0'..'9'] then
  begin
    // Extract line number
    SpacePos := Pos(' ', Command);
    if SpacePos > 0 then
    begin
      LineNum := StrToIntDef(Copy(Command, 1, SpacePos - 1), -1);
      RestOfLine := Trim(Copy(Command, SpacePos + 1, Length(Command)));

      if LineNum >= 0 then
      begin
        if RestOfLine = '' then
        begin
          // Delete the line
          FProgramMemory.DeleteLine(LineNum);
        end
        else
        begin
          // Add/replace the line
          FProgramMemory.StoreLine(LineNum, RestOfLine);
        end;
      end
      else
      begin
        FTextBuffer.PutString('?SYNTAX ERROR');
        FTextBuffer.NewLine;
      end;
    end
    else
    begin
      // Solo numero, cancella la riga
      LineNum := StrToIntDef(Command, -1);
      if LineNum >= 0 then
        FProgramMemory.DeleteLine(LineNum)
      else
      begin
        FTextBuffer.PutString('?SYNTAX ERROR');
        FTextBuffer.NewLine;
      end;
    end;
  end
  else
  begin
    // Comando/espressione diretta - TODO: valutare con l'interprete
    FTextBuffer.PutString('?SYNTAX ERROR');
    FTextBuffer.NewLine;
  end;
end;

{ PrintReady - prints READY. prompt if not already printed during current command.
  Called as safety net at end of ProcessCommand to ensure every command shows READY. }
procedure TSedaiNewConsole.PrintReady;
begin
  if not FReadyPrinted then
  begin
    FTextBuffer.PutString('READY.');
    FTextBuffer.NewLine;
    FReadyPrinted := True;
  end;
end;

procedure TSedaiNewConsole.ProcessCommand(const Command: string);
var
  TrimmedCmd: string;
  ParseResult: TInputParseResult;
  Lines: TStringList;
  i, j: Integer;
  BytecodeProgram: TBytecodeProgram;
  ProgramSource: string;
  CmdWord: string;
  Filename: string;
  Statement: string;
  LineNum: Integer;
  ListStartLine, ListEndLine: Integer;
  DashPos: Integer;
  CopySrc, CopyDst: string;
  CopyOverwrite: Boolean;
  ScratchPattern: string;
  ScratchForce, ScratchSilent: Boolean;
  RenameOld, RenameNew: string;
  HistDir, HistPath: string;
  F: TextFile;
begin
  TrimmedCmd := Trim(Command);
  FReadyPrinted := False;

  if TrimmedCmd = '' then
  begin
    // Empty command: no action, cursor already at new line
    Exit;
  end;

  // Use CommandRouter to determine command type
  // DEBUG: show the command in clear text before parsing
  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Processing raw command: ', TrimmedCmd);{$ENDIF}
  ParseResult := FCommandRouter.ParseInput(TrimmedCmd);

  // NOTE: History is now managed in the Run loop, tracking actual user keystrokes
  // This ensures only user-typed commands are saved, not system output

  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Command type: ', GetEnumName(TypeInfo(TCommandType), Ord(ParseResult.CommandType)));{$ENDIF}

  if not ParseResult.IsValid then
  begin
    FTextBuffer.PutString('?SYNTAX ERROR');
    FTextBuffer.NewLine;
    FTextBuffer.NewLine;
    FTextBuffer.PutString('READY.'); FReadyPrinted := True;
    FTextBuffer.NewLine;
    Exit;
  end;

  try
    case ParseResult.CommandType of
      ctProgramLine:
        begin
          // Check if in bytecode mode - cannot edit source
          if FBytecodeMode then
          begin
            FTextBuffer.PutString('?BYTECODE LOADED - NEW TO CLEAR');
            FTextBuffer.NewLine;
            FTextBuffer.PutString('READY.'); FReadyPrinted := True;
            FTextBuffer.NewLine;
          end
          else
          begin
            // Memorizza la linea nel programma
            if ParseResult.LineNumber >= 0 then
            begin
              if ParseResult.Statement = '' then
              begin
                {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Deleting line ', ParseResult.LineNumber);{$ENDIF}
                FProgramMemory.DeleteLine(ParseResult.LineNumber);
              end
              else
              begin
                {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Storing line ', ParseResult.LineNumber, ': ', ParseResult.Statement);{$ENDIF}
                FProgramMemory.StoreLine(ParseResult.LineNumber, ParseResult.Statement);
                {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Program lines now: ', FProgramMemory.GetLineCount);{$ENDIF}
              end;

              // AUTO mode: if active, prepare next line number
              if FAutoLineNumber > 0 then
              begin
                // Advance to next line number
                FAutoLineNumber := FAutoLineNumber + FAutoLineIncrement;
                // Put it in the text buffer and mark as user input
                Statement := IntToStr(FAutoLineNumber) + ' ';
                FTextBuffer.PutString(Statement);
                FUserHasTyped := True;
              end;
            end;
            // After inserting a line: no additional output, cursor already at new line
          end;
        end;

      ctImmediate:
        begin
          // Compile and execute immediate BASIC command via VM
          BytecodeProgram := nil;
          try
            try
              // Compile the statement to bytecode
              BytecodeProgram := FImmediateCompiler.CompileStatement(ParseResult.Statement);

              if not Assigned(BytecodeProgram) then
              begin
                // Compilation failed - show error
                // Note: LastErrorVerbose preserved for future OPTION command
                {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Compilation error: ', FImmediateCompiler.LastError);{$ENDIF}
                if FImmediateCompiler.LastErrorColumn > 0 then
                  FTextBuffer.PutString(Format('?%s AT COLUMN %d',
                    [FImmediateCompiler.LastError, FImmediateCompiler.LastErrorColumn]))
                else
                  FTextBuffer.PutString('?' + FImmediateCompiler.LastError);
                FTextBuffer.NewLine;
              end
              else
              begin
                // Execute the compiled bytecode
                BeginVMExecution;
                try
                  FBytecodeVM.Reset;
                  if Assigned(FProgramInputHandler) then
                    FProgramInputHandler.Reset;  // Clear any pending input state
                  FBytecodeVM.LoadProgram(BytecodeProgram);
                  if FDebugMode then
                  begin
                    FDebugger.Reset;
                    FBytecodeVM.RunDebug;
                  end
                  else
                    FBytecodeVM.Run;
                except
                  on E: Exception do
                  begin
                    {$IFDEF DEBUG_CONSOLE}WriteLn('ERROR: Exception during VM execution: ', E.ClassName, ' - ', E.Message);{$ENDIF}
                    FTextBuffer.PutString('?ERROR: ' + E.Message);
                    FTextBuffer.NewLine;
                  end;
                end;
                EndVMExecution;
              end;

              // READY dopo comando immediato
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.'); FReadyPrinted := True;
              FTextBuffer.NewLine;

            except
              on E: Exception do
              begin
                // Catch any unexpected exceptions - show error message
                {$IFDEF DEBUG_CONSOLE}WriteLn('ERROR: Exception in immediate command: ', E.ClassName, ' - ', E.Message);{$ENDIF}
                FTextBuffer.PutString('?SYNTAX ERROR');
                FTextBuffer.NewLine;
                FTextBuffer.PutString(E.Message);
                FTextBuffer.NewLine;
                FTextBuffer.NewLine;
                FTextBuffer.PutString('READY.'); FReadyPrinted := True;
                FTextBuffer.NewLine;
              end;
            end;
          finally
            // Clear VM's reference to the program before freeing it
            // to avoid dangling pointer access violations on shutdown
            if Assigned(FBytecodeVM) then
              FBytecodeVM.ClearProgram;
            if Assigned(BytecodeProgram) then
              BytecodeProgram.Free;
          end;
        end;

      ctSystemCommand:
        begin
          // System commands - extract the command keyword
          CmdWord := UpperCase(GetFirstWord(ParseResult.Statement));

          if CmdWord = kRUN then
          begin
            // RUN disables AUTO mode
            FAutoLineNumber := 0;

            // Parse RUN argument: can be empty, line number, or filename
            Filename := Trim(Copy(ParseResult.Statement, Length(kRUN) + 1, MaxInt));

            // Check if argument is a filename (quoted string)
            if (Length(Filename) > 0) and ((Filename[1] = '"') or (Filename[1] = '''')) then
            begin
              // RUN "filename" - load and execute
              Filename := ExtractFilename(ParseResult.Statement);
              if Filename = '' then
              begin
                FTextBuffer.PutString('?MISSING FILENAME');
                FTextBuffer.NewLine;
                FTextBuffer.PutString('READY.'); FReadyPrinted := True;
                FTextBuffer.NewLine;
              end
              else
              begin
                // Load the file first (reuse ExecuteLoad logic)
                ExecuteLoad(Filename);
                // If program loaded successfully, run it
                if FProgramMemory.GetLineCount > 0 then
                begin
                  // Get the program source from memory and compile it
                  Lines := FProgramMemory.GetAllLines;
                  try
                    ProgramSource := Lines.Text;
                  finally
                    Lines.Free;
                  end;

                  BytecodeProgram := nil;
                  try
                    BytecodeProgram := FImmediateCompiler.CompileProgram(ProgramSource);
                    if not Assigned(BytecodeProgram) then
                    begin
                      // ?SYNTAX ERROR IN <basicline> (<fileline>:<col>)
                      if FImmediateCompiler.LastErrorLine > 0 then
                      begin
                        if (FImmediateCompiler.LastErrorColumn > 0) and
                           (FImmediateCompiler.LastErrorFileLine > 0) then
                          FTextBuffer.PutString(Format('?%s IN %d (%d:%d)',
                            [FImmediateCompiler.LastError, FImmediateCompiler.LastErrorLine,
                             FImmediateCompiler.LastErrorFileLine, FImmediateCompiler.LastErrorColumn]))
                        else if FImmediateCompiler.LastErrorColumn > 0 then
                          FTextBuffer.PutString(Format('?%s IN %d (%d:%d)',
                            [FImmediateCompiler.LastError, FImmediateCompiler.LastErrorLine,
                             FImmediateCompiler.LastErrorLine, FImmediateCompiler.LastErrorColumn]))
                        else
                          FTextBuffer.PutString(Format('?%s IN %d', [FImmediateCompiler.LastError, FImmediateCompiler.LastErrorLine]));
                      end
                      else
                        FTextBuffer.PutString('?' + FImmediateCompiler.LastError);
                      FTextBuffer.NewLine;
                      // Show READY. then line ready for editing
                      FTextBuffer.PutString('READY.'); FReadyPrinted := True;
                      FTextBuffer.NewLine;
                      if FImmediateCompiler.LastErrorLine > 0 then
                      begin
                        Statement := FProgramMemory.GetLineText(FImmediateCompiler.LastErrorLine);
                        if Statement <> '' then
                        begin
                          Statement := IntToStr(FImmediateCompiler.LastErrorLine) + ' ' + Statement;
                          FTextBuffer.PutString(Statement);
                          if FImmediateCompiler.LastErrorColumn > 0 then
                            FTextBuffer.CursorX := FImmediateCompiler.LastErrorColumn - 1;
                          FUserHasTyped := True;
                          RenderScreen;
                          UpdateCursor;
                          FVideoController.Present;
                          Exit;
                        end;
                      end;
                    end
                    else
                    begin
                      BeginVMExecution;
                      try
                        FBytecodeVM.Reset;
                        if Assigned(FProgramInputHandler) then
                          FProgramInputHandler.Reset;  // Clear any pending input state
                        FBytecodeVM.LoadProgram(BytecodeProgram);
                        if FDebugMode then
                        begin
                          FDebugger.Reset;
                          FBytecodeVM.RunDebug;
                        end
                        else
                          FBytecodeVM.Run;
                      except
                        on E: Exception do
                        begin
                          FTextBuffer.PutString('?ERROR IN PROGRAM: ' + E.Message);
                          FTextBuffer.NewLine;
                        end;
                      end;
                      EndVMExecution;
                    end;
                  finally
                    // Clear VM's reference before freeing
                    if Assigned(FBytecodeVM) then
                      FBytecodeVM.ClearProgram;
                    if Assigned(BytecodeProgram) then
                      BytecodeProgram.Free;
                  end;
                  FTextBuffer.NewLine;
                  FTextBuffer.PutString('READY.'); FReadyPrinted := True;
                  FTextBuffer.NewLine;
                end;
                // else: ExecuteLoad already printed error and READY.
              end;
            end
            else
            begin
              // RUN or RUN line# - execute program in memory
              // Check if it's RUN line# (not yet implemented)
              if (Filename <> '') and TryStrToInt(Filename, LineNum) then
              begin
                // RUN line# - not yet implemented (would need bytecode line mapping)
                FTextBuffer.PutString('?RUN FROM LINE NOT YET IMPLEMENTED');
                FTextBuffer.NewLine;
                FTextBuffer.PutString('READY.'); FReadyPrinted := True;
                FTextBuffer.NewLine;
              end
              else if FBytecodeMode and Assigned(FLoadedBytecode) then
              begin
                // Run loaded bytecode directly
                BeginVMExecution;
                try
                  FBytecodeVM.Reset;
                  if Assigned(FProgramInputHandler) then
                    FProgramInputHandler.Reset;  // Clear any pending input state
                  FBytecodeVM.LoadProgram(FLoadedBytecode);
                  if FDebugMode then
                  begin
                    FDebugger.Reset;
                    FBytecodeVM.RunDebug;
                  end
                  else
                    FBytecodeVM.Run;
                except
                  on E: Exception do
                  begin
                    FTextBuffer.PutString('?ERROR IN PROGRAM: ' + E.Message);
                    FTextBuffer.NewLine;
                  end;
                end;
                EndVMExecution;
              end
              else if FProgramMemory.GetLineCount > 0 then
              begin
                // Get the program source from memory and compile it
                Lines := FProgramMemory.GetAllLines;
                try
                  ProgramSource := Lines.Text;
                finally
                  Lines.Free;
                end;

                BytecodeProgram := nil;
                try
                  // Compile the full program
                  BytecodeProgram := FImmediateCompiler.CompileProgram(ProgramSource);

                  if not Assigned(BytecodeProgram) then
                  begin
                    {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Program compilation error: ', FImmediateCompiler.LastError);{$ENDIF}
                    // ?SYNTAX ERROR IN <basicline> (<fileline>:<col>)
                    // Note: LastErrorVerbose preserved for future OPTION command
                    if FImmediateCompiler.LastErrorLine > 0 then
                    begin
                      if (FImmediateCompiler.LastErrorColumn > 0) and
                         (FImmediateCompiler.LastErrorFileLine > 0) then
                        FTextBuffer.PutString(Format('?%s IN %d (%d:%d)',
                          [FImmediateCompiler.LastError, FImmediateCompiler.LastErrorLine,
                           FImmediateCompiler.LastErrorFileLine, FImmediateCompiler.LastErrorColumn]))
                      else if FImmediateCompiler.LastErrorColumn > 0 then
                        FTextBuffer.PutString(Format('?%s IN %d (%d:%d)',
                          [FImmediateCompiler.LastError, FImmediateCompiler.LastErrorLine,
                           FImmediateCompiler.LastErrorLine, FImmediateCompiler.LastErrorColumn]))
                      else
                        FTextBuffer.PutString(Format('?%s IN %d', [FImmediateCompiler.LastError, FImmediateCompiler.LastErrorLine]));
                    end
                    else
                      FTextBuffer.PutString('?' + FImmediateCompiler.LastError);
                    FTextBuffer.NewLine;
                    // Show READY. then line with error for editing
                    FTextBuffer.PutString('READY.'); FReadyPrinted := True;
                    FTextBuffer.NewLine;
                    if FImmediateCompiler.LastErrorLine > 0 then
                    begin
                      Statement := FProgramMemory.GetLineText(FImmediateCompiler.LastErrorLine);
                      if Statement <> '' then
                      begin
                        // Build full line with line number and put it at prompt
                        Statement := IntToStr(FImmediateCompiler.LastErrorLine) + ' ' + Statement;
                        FTextBuffer.PutString(Statement);
                        // Position cursor at error column (accounting for line number prefix)
                        if FImmediateCompiler.LastErrorColumn > 0 then
                          FTextBuffer.CursorX := FImmediateCompiler.LastErrorColumn - 1;
                        FUserHasTyped := True;
                        RenderScreen;
                        UpdateCursor;
                        FVideoController.Present;
                        Exit;
                      end;
                    end;
                  end
                  else
                  begin
                    // Execute via VM
                    BeginVMExecution;
                    try
                      FBytecodeVM.Reset;
                      if Assigned(FProgramInputHandler) then
                        FProgramInputHandler.Reset;  // Clear any pending input state
                      FBytecodeVM.LoadProgram(BytecodeProgram);
                      if FDebugMode then
                      begin
                        FDebugger.Reset;
                        FBytecodeVM.RunDebug;
                      end
                      else
                        FBytecodeVM.Run;
                    except
                      on E: Exception do
                      begin
                        FTextBuffer.PutString('?ERROR IN PROGRAM: ' + E.Message);
                        FTextBuffer.NewLine;
                      end;
                    end;
                    EndVMExecution;
                  end;
                finally
                  // Clear VM's reference before freeing
                  if Assigned(FBytecodeVM) then
                    FBytecodeVM.ClearProgram;
                  if Assigned(BytecodeProgram) then
                    BytecodeProgram.Free;
                end;

                // NOTE: Do NOT automatically return to text mode after program ends
                // The user must explicitly use GRAPHIC 0 or similar to switch modes
              end
              else
              begin
                FTextBuffer.PutString('NO PROGRAM');
                FTextBuffer.NewLine;
              end;

              // After RUN: empty line + READY
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.'); FReadyPrinted := True;
              FTextBuffer.NewLine;

              // Force refresh of text display
              RenderScreen;
              UpdateCursor;
              FVideoController.Present;
            end;
          end
          else if CmdWord = kLIST then
          begin
            // Check if in bytecode mode - no source to list
            if FBytecodeMode then
            begin
              FTextBuffer.PutString('?BYTECODE LOADED - NO SOURCE');
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.'); FReadyPrinted := True;
              FTextBuffer.NewLine;
            end
            else
            begin
            // Parse LIST argument: LIST, LIST n, LIST n-, LIST -n, LIST n-m
            Lines := nil;
            Filename := Trim(Copy(ParseResult.Statement, Length(kLIST) + 1, MaxInt));

            if Filename = '' then
            begin
              // LIST - show all lines
              Lines := FProgramMemory.GetAllLines;
            end
            else
            begin
              // Parse range specification
              ListStartLine := 0;
              ListEndLine := MaxInt;
              DashPos := Pos('-', Filename);

              if DashPos = 0 then
              begin
                // LIST n - single line
                if TryStrToInt(Filename, ListStartLine) then
                  ListEndLine := ListStartLine
                else
                begin
                  FTextBuffer.PutString('?SYNTAX ERROR');
                  FTextBuffer.NewLine;
                  FTextBuffer.PutString('READY.'); FReadyPrinted := True;
                  FTextBuffer.NewLine;
                  Lines := nil;
                end;
              end
              else if DashPos = 1 then
              begin
                // LIST -n - from start to n
                if TryStrToInt(Copy(Filename, 2, MaxInt), ListEndLine) then
                  ListStartLine := 0
                else
                begin
                  FTextBuffer.PutString('?SYNTAX ERROR');
                  FTextBuffer.NewLine;
                  FTextBuffer.PutString('READY.'); FReadyPrinted := True;
                  FTextBuffer.NewLine;
                  Lines := nil;
                end;
              end
              else if DashPos = Length(Filename) then
              begin
                // LIST n- - from n to end
                if TryStrToInt(Copy(Filename, 1, DashPos - 1), ListStartLine) then
                  ListEndLine := MaxInt
                else
                begin
                  FTextBuffer.PutString('?SYNTAX ERROR');
                  FTextBuffer.NewLine;
                  FTextBuffer.PutString('READY.'); FReadyPrinted := True;
                  FTextBuffer.NewLine;
                  Lines := nil;
                end;
              end
              else
              begin
                // LIST n-m - from n to m
                if TryStrToInt(Copy(Filename, 1, DashPos - 1), ListStartLine) and
                   TryStrToInt(Copy(Filename, DashPos + 1, MaxInt), ListEndLine) then
                begin
                  // Valid range
                end
                else
                begin
                  FTextBuffer.PutString('?SYNTAX ERROR');
                  FTextBuffer.NewLine;
                  FTextBuffer.PutString('READY.'); FReadyPrinted := True;
                  FTextBuffer.NewLine;
                  Lines := nil;
                end;
              end;

              // Get lines in range if no error
              if (ListStartLine >= 0) or (ListEndLine < MaxInt) then
                Lines := FProgramMemory.GetLinesInRange(ListStartLine, ListEndLine);
            end;

            // Display lines if we have them
            if Assigned(Lines) then
            begin
              try
                {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: LIST retrieved ', Lines.Count, ' lines');{$ENDIF}
                if Lines.Count > 0 then
                begin
                  if ParseResult.PipeModifier = pmMore then
                    DisplayWithMore(Lines)
                  else
                  begin
                    for i := 0 to Lines.Count - 1 do
                    begin
                      {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: LIST output: ', Lines[i]);{$ENDIF}
                      FTextBuffer.PutString(Lines[i]);
                      FTextBuffer.NewLine;
                    end;
                  end;
                end
                else
                begin
                  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: LIST - no program');{$ENDIF}
                  // Don't print "NO PROGRAM" for range queries that return empty
                  if (Filename = '') then
                  begin
                    FTextBuffer.PutString('NO PROGRAM');
                    FTextBuffer.NewLine;
                  end;
                end;
              finally
                Lines.Free;
              end;
            end;
            end; // end of else (not bytecode mode)
          end
          else if CmdWord = kGLIST then
          begin
            // List available SDL2 video modes
            Lines := ExecuteGLIST;
            try
              if ParseResult.PipeModifier = pmMore then
                DisplayWithMore(Lines)
              else
              begin
                for i := 0 to Lines.Count - 1 do
                begin
                  FTextBuffer.PutString(Lines[i]);
                  FTextBuffer.NewLine;
                end;
                RenderScreen;
              end;
            finally
              Lines.Free;
            end;
          end
          else if CmdWord = kEDIT then
          begin
            // EDIT n - Edit a single program line
            if FBytecodeMode then
            begin
              FTextBuffer.PutString('?BYTECODE LOADED - NO SOURCE');
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.'); FReadyPrinted := True;
              FTextBuffer.NewLine;
            end
            else
            begin
              Filename := Trim(Copy(ParseResult.Statement, Length(kEDIT) + 1, MaxInt));
              if TryStrToInt(Filename, LineNum) then
              begin
                // Get the line content using GetLineText for efficiency
                Statement := FProgramMemory.GetLineText(LineNum);
                if Statement <> '' then
                begin
                  // Build full line with line number
                  Statement := IntToStr(LineNum) + ' ' + Statement;
                  // Put the line in the text buffer for editing (no READY. prompt)
                  FTextBuffer.PutString(Statement);
                  // Mark as user input so it gets saved to history when ENTER is pressed
                  FUserHasTyped := True;
                  // Render immediately so user sees the line
                  RenderScreen;
                  UpdateCursor;
                  FVideoController.Present;
                  // Don't print READY. - user is editing
                  Exit;
                end
                else
                begin
                  FTextBuffer.PutString('?LINE NOT FOUND');
                  FTextBuffer.NewLine;
                  FTextBuffer.PutString('READY.'); FReadyPrinted := True;
                  FTextBuffer.NewLine;
                end;
              end
              else
              begin
                FTextBuffer.PutString('?SYNTAX ERROR');
                FTextBuffer.NewLine;
                FTextBuffer.PutString('READY.'); FReadyPrinted := True;
                FTextBuffer.NewLine;
              end;
            end;
          end
          else if CmdWord = kAUTO then
          begin
            // AUTO [increment] - Enable/disable automatic line numbering
            if FBytecodeMode then
            begin
              FTextBuffer.PutString('?BYTECODE LOADED - NO SOURCE');
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.'); FReadyPrinted := True;
              FTextBuffer.NewLine;
            end
            else
            begin
              Filename := Trim(Copy(ParseResult.Statement, Length(kAUTO) + 1, MaxInt));
              if (Filename = '') or (UpperCase(Filename) = 'OFF') then
              begin
                // AUTO or AUTO OFF - turn off auto numbering
                FAutoLineNumber := 0;
                FTextBuffer.PutString('AUTO OFF');
                FTextBuffer.NewLine;
              end
              else if TryStrToInt(Filename, LineNum) then
              begin
                if LineNum > 0 then
                begin
                  // Set increment and start from line 10 (or next available)
                  FAutoLineIncrement := LineNum;
                  // Find next available line number
                  Lines := FProgramMemory.GetAllLines;
                  try
                    if (Lines <> nil) and (Lines.Count > 0) then
                    begin
                      // Get last line number and add increment
                      // Lines are formatted as "linenum content"
                      i := 0;
                      for j := 0 to Lines.Count - 1 do
                      begin
                        if TryStrToInt(Copy(Lines[j], 1, Pos(' ', Lines[j]) - 1), LineNum) then
                          if LineNum > i then
                            i := LineNum;
                      end;
                      FAutoLineNumber := ((i div FAutoLineIncrement) + 1) * FAutoLineIncrement;
                    end
                    else
                      FAutoLineNumber := FAutoLineIncrement;
                  finally
                    if Assigned(Lines) then
                      Lines.Free;
                  end;
                  // Put the first line number in the text buffer and mark as user input
                  Statement := IntToStr(FAutoLineNumber) + ' ';
                  FTextBuffer.PutString(Statement);
                  FUserHasTyped := True;
                  FReadyPrinted := True;  // Suppress READY. - user is entering lines
                  RenderScreen;
                  UpdateCursor;
                  FVideoController.Present;
                end
                else
                begin
                  FTextBuffer.PutString('?ILLEGAL QUANTITY');
                  FTextBuffer.NewLine;
                  FTextBuffer.PutString('READY.'); FReadyPrinted := True;
                  FTextBuffer.NewLine;
                end;
              end
              else
              begin
                FTextBuffer.PutString('?SYNTAX ERROR');
                FTextBuffer.NewLine;
                FTextBuffer.PutString('READY.'); FReadyPrinted := True;
                FTextBuffer.NewLine;
              end;
            end;
          end
          else if CmdWord = kNEW then
          begin
            FProgramMemory.Clear;
            // Reset bytecode mode
            // Also disable AUTO mode
            FAutoLineNumber := 0;
            FBytecodeMode := False;
            if Assigned(FLoadedBytecode) then
            begin
              FLoadedBytecode.Free;
              FLoadedBytecode := nil;
            end;
            FTextBuffer.NewLine;
            FTextBuffer.PutString('READY.'); FReadyPrinted := True;
            FTextBuffer.NewLine;
          end
          else if CmdWord = kCONT then
          begin
            // CONT - Continue execution after STOP
            if FBytecodeVM.Stopped then
            begin
              try
                FBytecodeVM.Continue;
              except
                on E: Exception do
                begin
                  FTextBuffer.PutString(E.Message);
                  FTextBuffer.NewLine;
                end;
              end;
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.'); FReadyPrinted := True;
              FTextBuffer.NewLine;
            end
            else if FDebugger.IsPaused then
            begin
              // Resume from debugger pause
              FDebugger.Continue;
              BeginVMExecution;
              try
                FBytecodeVM.RunDebug;
              finally
                EndVMExecution;
              end;
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.'); FReadyPrinted := True;
              FTextBuffer.NewLine;
            end
            else
            begin
              FTextBuffer.PutString('?CAN''T CONTINUE ERROR');
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.'); FReadyPrinted := True;
              FTextBuffer.NewLine;
            end;
          end
          else if CmdWord = kTRON then
          begin
            // TRON - Enable debug mode
            FDebugMode := True;
            FDebugger.Activate;
            FTextBuffer.PutString('TRACE ON');
            FTextBuffer.NewLine;
            FTextBuffer.PutString('READY.'); FReadyPrinted := True;
            FTextBuffer.NewLine;
          end
          else if CmdWord = kTROFF then
          begin
            // TROFF - Disable debug mode
            FDebugMode := False;
            FDebugger.Deactivate;
            FTextBuffer.PutString('TRACE OFF');
            FTextBuffer.NewLine;
            FTextBuffer.PutString('READY.'); FReadyPrinted := True;
            FTextBuffer.NewLine;
          end
          else if CmdWord = 'BREAK' then
          begin
            // BREAK n - Set breakpoint at line n
            // BREAK - List breakpoints
            Filename := Trim(Copy(ParseResult.Statement, 6, MaxInt));
            if Filename = '' then
            begin
              // List breakpoints
              if FDebugger.GetBreakpointCount = 0 then
                FTextBuffer.PutString('NO BREAKPOINTS')
              else
              begin
                FTextBuffer.PutString('BREAKPOINTS:');
                FTextBuffer.NewLine;
                for i := 0 to FDebugger.GetBreakpointCount - 1 do
                begin
                  FTextBuffer.PutString('  LINE ' + IntToStr(FDebugger.GetBreakpointLine(i)));
                  FTextBuffer.NewLine;
                end;
              end;
            end
            else if TryStrToInt(Filename, LineNum) then
            begin
              FDebugger.SetBreakpoint(LineNum);
              FTextBuffer.PutString('BREAKPOINT SET AT LINE ' + IntToStr(LineNum));
            end
            else
            begin
              FTextBuffer.PutString('?SYNTAX ERROR');
            end;
            FTextBuffer.NewLine;
            FTextBuffer.PutString('READY.'); FReadyPrinted := True;
            FTextBuffer.NewLine;
          end
          else if CmdWord = 'UNBREAK' then
          begin
            // UNBREAK n - Clear breakpoint at line n
            // UNBREAK - Clear all breakpoints
            Filename := Trim(Copy(ParseResult.Statement, 8, MaxInt));
            if Filename = '' then
            begin
              FDebugger.ClearAllBreakpoints;
              FTextBuffer.PutString('ALL BREAKPOINTS CLEARED');
            end
            else if TryStrToInt(Filename, LineNum) then
            begin
              FDebugger.ClearBreakpoint(LineNum);
              FTextBuffer.PutString('BREAKPOINT CLEARED AT LINE ' + IntToStr(LineNum));
            end
            else
            begin
              FTextBuffer.PutString('?SYNTAX ERROR');
            end;
            FTextBuffer.NewLine;
            FTextBuffer.PutString('READY.'); FReadyPrinted := True;
            FTextBuffer.NewLine;
          end
          else if CmdWord = 'STEP' then
          begin
            // STEP - Step one line (only when paused)
            if FDebugger.IsPaused then
            begin
              FDebugger.StepLine;
              BeginVMExecution;
              try
                FBytecodeVM.RunDebug;
              finally
                EndVMExecution;
              end;
            end
            else
            begin
              FTextBuffer.PutString('?NOT IN DEBUG MODE');
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.'); FReadyPrinted := True;
              FTextBuffer.NewLine;
            end;
          end
          else if (CmdWord = kLOAD) or (CmdWord = kDLOAD) then
          begin
            // Load program from file - exits bytecode mode
            Filename := ExtractFilename(ParseResult.Statement);
            // Reset bytecode mode when loading source
            FBytecodeMode := False;
            if Assigned(FLoadedBytecode) then
            begin
              FLoadedBytecode.Free;
              FLoadedBytecode := nil;
            end;
            ExecuteLoad(Filename);
          end
          else if (CmdWord = kSAVE) or (CmdWord = kDSAVE) then
          begin
            // Save program to file - not allowed in bytecode mode
            if FBytecodeMode then
            begin
              FTextBuffer.PutString('?BYTECODE LOADED - USE BSAVE');
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.'); FReadyPrinted := True;
              FTextBuffer.NewLine;
            end
            else
            begin
              Filename := ExtractFilename(ParseResult.Statement);
              ExecuteSave(Filename);
            end;
          end
          else if (CmdWord = kVERIFY) or (CmdWord = kDVERIFY) then
          begin
            // Verify program against file - not allowed in bytecode mode
            if FBytecodeMode then
            begin
              FTextBuffer.PutString('?BYTECODE LOADED');
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.'); FReadyPrinted := True;
              FTextBuffer.NewLine;
            end
            else
            begin
              Filename := ExtractFilename(ParseResult.Statement);
              ExecuteVerify(Filename);
            end;
          end
          else if CmdWord = kBLOAD then
          begin
            // Load bytecode file
            Filename := ExtractFilename(ParseResult.Statement);
            ExecuteBLoad(Filename);
          end
          else if CmdWord = kBSAVE then
          begin
            // Save bytecode file
            Filename := ExtractFilename(ParseResult.Statement);
            ExecuteBSave(Filename);
          end
          else if CmdWord = kBOOT then
          begin
            // Load and run bytecode file
            Filename := ExtractFilename(ParseResult.Statement);
            ExecuteBLoad(Filename);
            // If load succeeded (FBytecodeMode is true), run it
            if FBytecodeMode and Assigned(FLoadedBytecode) then
            begin
              BeginVMExecution;
              try
                FBytecodeVM.Reset;
                if Assigned(FProgramInputHandler) then
                  FProgramInputHandler.Reset;  // Clear any pending input state
                FBytecodeVM.LoadProgram(FLoadedBytecode);
                if FDebugMode then
                begin
                  FDebugger.Reset;
                  FBytecodeVM.RunDebug;
                end
                else
                  FBytecodeVM.Run;
              except
                on E: Exception do
                begin
                  FTextBuffer.PutString('?ERROR: ' + E.Message);
                  FTextBuffer.NewLine;
                end;
              end;
              EndVMExecution;
              FTextBuffer.NewLine;
              FReadyPrinted := False;  // Need new READY. after program execution
            end;
          end
          else if (CmdWord = kCATALOG) or (CmdWord = kDIR) or (CmdWord = kDIRECTORY) then
          begin
            // Directory listing
            Filename := ExtractFilename(ParseResult.Statement);
            Lines := ExecuteDirectory(Filename);
            try
              if ParseResult.PipeModifier = pmMore then
                DisplayWithMore(Lines)
              else
              begin
                for i := 0 to Lines.Count - 1 do
                begin
                  FTextBuffer.PutString(Lines[i]);
                  FTextBuffer.NewLine;
                end;
                RenderScreen;
              end;
            finally
              Lines.Free;
            end;
          end
          else if CmdWord = kCOPY then
          begin
            // COPY command - format: COPY src dst [OVERWRITE]
            Statement := Trim(Copy(ParseResult.Statement, Length(kCOPY) + 1, MaxInt));
            CopySrc := '';
            CopyDst := '';
            CopyOverwrite := False;

            if Statement = '' then
            begin
              FTextBuffer.PutString('?MISSING SOURCE AND DESTINATION');
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.'); FReadyPrinted := True;
              FTextBuffer.NewLine;
            end
            else
            begin
              // Extract first argument (source)
              if Statement[1] = '"' then
              begin
                j := Pos('"', Copy(Statement, 2, MaxInt));
                if j > 0 then
                begin
                  CopySrc := Copy(Statement, 2, j - 1);
                  Statement := Trim(Copy(Statement, j + 2, MaxInt));
                end
                else
                begin
                  FTextBuffer.PutString('?SYNTAX ERROR - UNCLOSED QUOTE');
                  FTextBuffer.NewLine;
                  Statement := '';
                end;
              end
              else
              begin
                j := Pos(' ', Statement);
                if j > 0 then
                begin
                  CopySrc := Copy(Statement, 1, j - 1);
                  Statement := Trim(Copy(Statement, j + 1, MaxInt));
                end
                else
                begin
                  CopySrc := Statement;
                  Statement := '';
                end;
              end;

              // Extract second argument (dest)
              if (CopySrc <> '') and (Statement = '') then
              begin
                FTextBuffer.PutString('?MISSING DESTINATION');
                FTextBuffer.NewLine;
              end
              else if CopySrc <> '' then
              begin
                if Statement[1] = '"' then
                begin
                  j := Pos('"', Copy(Statement, 2, MaxInt));
                  if j > 0 then
                  begin
                    CopyDst := Copy(Statement, 2, j - 1);
                    Statement := Trim(Copy(Statement, j + 2, MaxInt));
                  end
                  else
                  begin
                    FTextBuffer.PutString('?SYNTAX ERROR - UNCLOSED QUOTE');
                    FTextBuffer.NewLine;
                    CopyDst := '';
                  end;
                end
                else
                begin
                  // Unquoted dest - find space (for OVERWRITE flag)
                  j := Pos(' ', Statement);
                  if j > 0 then
                  begin
                    CopyDst := Copy(Statement, 1, j - 1);
                    Statement := Trim(Copy(Statement, j + 1, MaxInt));
                  end
                  else
                  begin
                    CopyDst := Statement;
                    Statement := '';
                  end;
                end;

                // Check for OVERWRITE flag
                if UpperCase(Trim(Statement)) = 'OVERWRITE' then
                  CopyOverwrite := True;

                if (CopySrc <> '') and (CopyDst <> '') then
                  ExecuteCopy(CopySrc, CopyDst, CopyOverwrite);
              end;
            end;
          end
          else if CmdWord = kSCRATCH then
          begin
            // SCRATCH command - format: SCRATCH pattern[,1] [FORCE]
            // ,1 = silent mode (suppress ?FILE NOT FOUND)
            Statement := Trim(Copy(ParseResult.Statement, Length(kSCRATCH) + 1, MaxInt));
            ScratchPattern := '';
            ScratchForce := False;
            ScratchSilent := False;

            if Statement = '' then
            begin
              FTextBuffer.PutString('?MISSING FILE PATTERN');
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.'); FReadyPrinted := True;
              FTextBuffer.NewLine;
            end
            else
            begin
              // Extract pattern (quoted or unquoted)
              if Statement[1] = '"' then
              begin
                j := Pos('"', Copy(Statement, 2, MaxInt));
                if j > 0 then
                begin
                  ScratchPattern := Copy(Statement, 2, j - 1);
                  Statement := Trim(Copy(Statement, j + 2, MaxInt));
                end
                else
                begin
                  FTextBuffer.PutString('?MISSING CLOSING QUOTE');
                  FTextBuffer.NewLine;
                  FTextBuffer.PutString('READY.'); FReadyPrinted := True;
                  FTextBuffer.NewLine;
                end;
              end
              else
              begin
                // Unquoted - find space or comma
                j := Pos(' ', Statement);
                i := Pos(',', Statement);
                if (i > 0) and ((j = 0) or (i < j)) then
                  j := i;  // Comma comes first
                if j > 0 then
                begin
                  ScratchPattern := Copy(Statement, 1, j - 1);
                  Statement := Trim(Copy(Statement, j + 1, MaxInt));
                end
                else
                begin
                  ScratchPattern := Statement;
                  Statement := '';
                end;
              end;

              // Check for ,1 (silent) and FORCE flags
              // Statement now contains remaining text after pattern
              if (Statement <> '') and (Statement[1] = '1') then
              begin
                ScratchSilent := True;
                Statement := Trim(Copy(Statement, 2, MaxInt));
              end;
              if UpperCase(Trim(Statement)) = 'FORCE' then
                ScratchForce := True;

              if ScratchPattern <> '' then
                ExecuteScratch(ScratchPattern, ScratchForce, ScratchSilent);
            end;
          end
          else if CmdWord = kRENAME then
          begin
            // RENAME command - format: RENAME oldname newname
            Statement := Trim(Copy(ParseResult.Statement, Length(kRENAME) + 1, MaxInt));
            RenameOld := '';
            RenameNew := '';

            if Statement = '' then
            begin
              FTextBuffer.PutString('?MISSING SOURCE AND DESTINATION');
              FTextBuffer.NewLine;
            end
            else
            begin
              // Extract first argument (old name)
              if Statement[1] = '"' then
              begin
                j := Pos('"', Copy(Statement, 2, MaxInt));
                if j > 0 then
                begin
                  RenameOld := Copy(Statement, 2, j - 1);
                  Statement := Trim(Copy(Statement, j + 2, MaxInt));
                end
                else
                begin
                  FTextBuffer.PutString('?MISSING CLOSING QUOTE');
                  FTextBuffer.NewLine;
                end;
              end
              else
              begin
                // Unquoted - find space
                j := Pos(' ', Statement);
                if j > 0 then
                begin
                  RenameOld := Copy(Statement, 1, j - 1);
                  Statement := Trim(Copy(Statement, j + 1, MaxInt));
                end
                else
                begin
                  RenameOld := Statement;
                  Statement := '';
                end;
              end;

              // Extract second argument (new name)
              if Statement <> '' then
              begin
                if Statement[1] = '"' then
                begin
                  j := Pos('"', Copy(Statement, 2, MaxInt));
                  if j > 0 then
                    RenameNew := Copy(Statement, 2, j - 1)
                  else
                  begin
                    FTextBuffer.PutString('?MISSING CLOSING QUOTE');
                    FTextBuffer.NewLine;
                    RenameNew := '';
                  end;
                end
                else
                begin
                  // Unquoted - take everything
                  j := Pos(' ', Statement);
                  if j > 0 then
                    RenameNew := Copy(Statement, 1, j - 1)
                  else
                    RenameNew := Statement;
                end;
              end
              else
              begin
                FTextBuffer.PutString('?MISSING DESTINATION NAME');
                FTextBuffer.NewLine;
              end;

              if (RenameOld <> '') and (RenameNew <> '') then
                ExecuteRename(RenameOld, RenameNew);
            end;
          end
          else if CmdWord = kHLOAD then
          begin
            // HLOAD - Load history from file
            Filename := ExtractFilename(ParseResult.Statement);
            if Filename = '' then
            begin
              FTextBuffer.PutString('?MISSING FILENAME');
              FTextBuffer.NewLine;
            end
            else
            begin
              // Resolve path
              if (Length(Filename) < 2) or (Filename[2] <> ':') then
                Filename := IncludeTrailingPathDelimiter(GetCurrentDir) + Filename;

              if FileExists(Filename) then
              begin
                // Load history from file, replacing current history
                FHistoryFile := Filename;
                FHistoryCount := 0;
                LoadHistory;
                FTextBuffer.PutString('HISTORY LOADED');
                FTextBuffer.NewLine;
              end
              else
              begin
                // File doesn't exist - ask if user wants to create it
                if AskYesNo('FILE NOT FOUND. CREATE? (Y/N) ') then
                begin
                  // Create empty file and set as history file
                  try
                    ForceDirectories(ExtractFilePath(Filename));
                    AssignFile(F, Filename);
                    Rewrite(F);
                    CloseFile(F);
                    FHistoryFile := Filename;
                    FHistoryCount := 0;
                    FTextBuffer.PutString('HISTORY FILE CREATED');
                    FTextBuffer.NewLine;
                  except
                    on E: Exception do
                    begin
                      FTextBuffer.PutString('?CANNOT CREATE FILE');
                      FTextBuffer.NewLine;
                    end;
                  end;
                end
                else
                begin
                  FTextBuffer.PutString('CANCELLED');
                  FTextBuffer.NewLine;
                end;
              end;
            end;
          end
          else if CmdWord = kHSAVE then
          begin
            // HSAVE - Save history to file
            Filename := ExtractFilename(ParseResult.Statement);
            if Filename = '' then
            begin
              FTextBuffer.PutString('?MISSING FILENAME');
              FTextBuffer.NewLine;
            end
            else
            begin
              // Resolve path
              if (Length(Filename) < 2) or (Filename[2] <> ':') then
                Filename := IncludeTrailingPathDelimiter(GetCurrentDir) + Filename;

              // Check if directory exists
              HistDir := ExtractFilePath(Filename);
              if (HistDir <> '') and not DirectoryExists(HistDir) then
              begin
                if AskYesNo('PATH NOT FOUND. CREATE? (Y/N) ') then
                begin
                  try
                    ForceDirectories(HistDir);
                  except
                    FTextBuffer.PutString('?CANNOT CREATE PATH');
                    FTextBuffer.NewLine;
                    HistDir := ''; // Signal failure
                  end;
                end
                else
                begin
                  FTextBuffer.PutString('CANCELLED');
                  FTextBuffer.NewLine;
                  HistDir := ''; // Signal cancelled
                end;
              end;

              // Proceed if path exists or was created
              if (HistDir = '') or DirectoryExists(HistDir) then
              begin
                // Check if file exists
                if FileExists(Filename) then
                begin
                  if not AskYesNo('FILE EXISTS. OVERWRITE? (Y/N) ') then
                  begin
                    FTextBuffer.PutString('CANCELLED');
                    FTextBuffer.NewLine;
                    Filename := ''; // Signal cancelled
                  end;
                end;

                if Filename <> '' then
                begin
                  try
                    AssignFile(F, Filename);
                    Rewrite(F);
                    try
                      for i := 0 to FHistoryCount - 1 do
                        WriteLn(F, FInputHistory[i]);
                    finally
                      CloseFile(F);
                    end;
                    FHistoryFile := Filename;
                    FTextBuffer.PutString('HISTORY SAVED');
                    FTextBuffer.NewLine;
                  except
                    on E: Exception do
                    begin
                      FTextBuffer.PutString('?CANNOT SAVE FILE');
                      FTextBuffer.NewLine;
                    end;
                  end;
                end;
              end;
            end;
          end
          else if CmdWord = kHCLEAR then
          begin
            // HCLEAR - Clear history with confirmation
            if FHistoryCount = 0 then
            begin
              FTextBuffer.PutString('HISTORY ALREADY EMPTY');
              FTextBuffer.NewLine;
            end
            else if AskYesNo('CLEAR HISTORY? (Y/N) ') then
            begin
              // Clear memory
              FHistoryCount := 0;
              FHistoryPos := -1;

              // Truncate file if it exists
              HistPath := GetHistoryFilePath;
              if (HistPath <> '') and FileExists(HistPath) then
              begin
                try
                  AssignFile(F, HistPath);
                  Rewrite(F);
                  CloseFile(F);
                except
                  // Ignore errors truncating file
                end;
              end;

              FTextBuffer.PutString('HISTORY CLEARED');
              FTextBuffer.NewLine;
            end
            else
            begin
              FTextBuffer.PutString('CANCELLED');
              FTextBuffer.NewLine;
            end;
          end
          else
          begin
            // Unknown system command
            FTextBuffer.PutString('?UNKNOWN COMMAND');
            FTextBuffer.NewLine;
            FTextBuffer.PutString('READY.'); FReadyPrinted := True;
            FTextBuffer.NewLine;
          end;

        end;
    end;

    // Safety net: ensure READY. is printed after every system command
    if (ParseResult.CommandType = ctSystemCommand) and not FReadyPrinted then
      PrintReady;

  except
    on E: Exception do
    begin
      FTextBuffer.PutString('?ERROR: ' + E.Message);
      FTextBuffer.NewLine;
      PrintReady;
    end;
  end;
end;

function TSedaiNewConsole.ExecuteGLIST: TStringList;
var
  Enumerator: TSDL2VideoModeEnumerator;
begin
  Result := TStringList.Create;
  Enumerator := TSDL2VideoModeEnumerator.Create(0);
  try
    if Enumerator.EnumerateModes then
      Result.Assign(Enumerator.GetModesAsStringList)
    else
      Result.Add('?SDL2 VIDEO ERROR');
  finally
    Enumerator.Free;
  end;
end;

procedure TSedaiNewConsole.DisplayWithMore(Lines: TStringList);
var
  i, LinesPerPage: Integer;
  Event: TSDL_Event;
  GotKey, QuitRequested: Boolean;
begin
  if Lines.Count = 0 then
    Exit;

  // Calculate lines per page: screen rows - 1 (for "-- MORE --")
  LinesPerPage := FTextBuffer.Rows - 1;
  QuitRequested := False;
  i := 0;

  while (i < Lines.Count) and not QuitRequested do
  begin
    // Display one page
    while (i < Lines.Count) and ((i mod LinesPerPage) < LinesPerPage) do
    begin
      FTextBuffer.PutString(Lines[i]);
      FTextBuffer.NewLine;
      Inc(i);

      // Check if we've filled a page
      if (i mod LinesPerPage = 0) and (i < Lines.Count) then
        Break;
    end;

    RenderScreen;
    FVideoController.Present;

    // If more lines remain, show "-- MORE --" and wait for key
    if i < Lines.Count then
    begin
      FTextBuffer.PutString('-- MORE --');
      RenderScreen;
      FVideoController.Present;

      // Flush any pending events
      while SDL_PollEvent(@Event) <> 0 do
        ; // discard

      // Wait 100ms for key release
      SDL_Delay(100);

      // Flush again
      while SDL_PollEvent(@Event) <> 0 do
        ; // discard

      // Wait for new keypress
      GotKey := False;
      while not GotKey do
      begin
        while SDL_PollEvent(@Event) <> 0 do
        begin
          case Event.type_ of
            SDL_QUITEV:
              begin
                QuitRequested := True;
                GotKey := True;
              end;
            SDL_KEYDOWN:
              begin
                // ESC or Q to quit
                if (Event.key.keysym.sym = SDLK_ESCAPE) or
                   (Event.key.keysym.sym = SDLK_q) then
                  QuitRequested := True;
                GotKey := True;
              end;
          end;
        end;
        if not GotKey then
          SDL_Delay(10);
      end;

      // Clear the "-- MORE --" line
      FTextBuffer.ClearCurrentLine;
    end;
  end;
end;

{ File I/O helper methods for LOAD/SAVE commands }

function TSedaiNewConsole.GetFirstWord(const Statement: string): string;
var
  SpacePos: Integer;
begin
  SpacePos := Pos(' ', Statement);
  if SpacePos > 0 then
    Result := Copy(Statement, 1, SpacePos - 1)
  else
    Result := Statement;
end;

function TSedaiNewConsole.ExtractFilename(const Statement: string): string;
var
  Rest: string;
  SpacePos, QuoteStart, QuoteEnd: Integer;
begin
  Result := '';

  // Get the part after the command (LOAD, SAVE, etc.)
  SpacePos := Pos(' ', Statement);
  if SpacePos = 0 then
    Exit; // No filename provided

  Rest := Trim(Copy(Statement, SpacePos + 1, Length(Statement)));
  if Rest = '' then
    Exit;

  // Check for quoted filename: LOAD "filename"
  if (Length(Rest) > 0) and (Rest[1] = '"') then
  begin
    QuoteStart := 1;
    QuoteEnd := Pos('"', Copy(Rest, 2, Length(Rest)));
    if QuoteEnd > 0 then
      Result := Copy(Rest, 2, QuoteEnd - 1)
    else
      Result := Copy(Rest, 2, Length(Rest)); // Unclosed quote, take the rest
  end
  else
  begin
    // Unquoted filename: take until space or end
    SpacePos := Pos(' ', Rest);
    if SpacePos > 0 then
      Result := Copy(Rest, 1, SpacePos - 1)
    else
      Result := Rest;
  end;
end;

function TSedaiNewConsole.AskYesNo(const Prompt: string): Boolean;
var
  Event: TSDL_Event;
  GotKey: Boolean;
  KeyChar: Char;
begin
  Result := False;

  // Display the prompt
  FTextBuffer.PutString(Prompt);
  RenderScreen;

  // Flush any pending events
  while SDL_PollEvent(@Event) <> 0 do
    ; // discard

  SDL_Delay(100);

  // Flush again
  while SDL_PollEvent(@Event) <> 0 do
    ; // discard

  // Wait for Y/N keypress
  GotKey := False;
  while not GotKey do
  begin
    while SDL_PollEvent(@Event) <> 0 do
    begin
      case Event.type_ of
        SDL_QUITEV:
          begin
            GotKey := True;
            Result := False;
          end;
        SDL_KEYDOWN:
          begin
            KeyChar := UpCase(Chr(Event.key.keysym.sym and $FF));
            if KeyChar = 'Y' then
            begin
              Result := True;
              GotKey := True;
              FTextBuffer.PutChar('Y');
            end
            else if (KeyChar = 'N') or (Event.key.keysym.sym = SDLK_ESCAPE) then
            begin
              Result := False;
              GotKey := True;
              FTextBuffer.PutChar('N');
            end;
          end;
      end;
    end;
    if not GotKey then
      SDL_Delay(10);
  end;

  FTextBuffer.NewLine;
  RenderScreen;
end;

function TSedaiNewConsole.AskYesNoAll(const Prompt: string; var OverwriteAll: Boolean): Boolean;
var
  Event: TSDL_Event;
  GotKey: Boolean;
  KeyChar: Char;
begin
  Result := False;

  // Display the prompt (Y/N/A)
  FTextBuffer.PutString(Prompt);
  RenderScreen;

  // Flush any pending events
  while SDL_PollEvent(@Event) <> 0 do
    ; // discard

  SDL_Delay(100);

  // Flush again
  while SDL_PollEvent(@Event) <> 0 do
    ; // discard

  // Wait for Y/N/A keypress
  GotKey := False;
  while not GotKey do
  begin
    while SDL_PollEvent(@Event) <> 0 do
    begin
      case Event.type_ of
        SDL_QUITEV:
          begin
            GotKey := True;
            Result := False;
          end;
        SDL_KEYDOWN:
          begin
            KeyChar := UpCase(Chr(Event.key.keysym.sym and $FF));
            if KeyChar = 'Y' then
            begin
              Result := True;
              GotKey := True;
              FTextBuffer.PutChar('Y');
            end
            else if KeyChar = 'A' then
            begin
              Result := True;
              OverwriteAll := True;
              GotKey := True;
              FTextBuffer.PutChar('A');
            end
            else if (KeyChar = 'N') or (Event.key.keysym.sym = SDLK_ESCAPE) then
            begin
              Result := False;
              GotKey := True;
              FTextBuffer.PutChar('N');
            end;
          end;
      end;
    end;
    if not GotKey then
      SDL_Delay(10);
  end;

  FTextBuffer.NewLine;
  RenderScreen;
end;

procedure TSedaiNewConsole.ExecuteLoad(const Filename: string);
var
  FileContent: TStringList;
  i: Integer;
  Line: string;
  SpacePos, LineNum: Integer;
  Statement: string;
  DisplayName: string;
begin
  if Filename = '' then
  begin
    FTextBuffer.PutString('?MISSING FILENAME');
    FTextBuffer.NewLine;
    Exit;
  end;

  // Extract display name (filename without path)
  DisplayName := ExtractFileName(Filename);
  if DisplayName = '' then
    DisplayName := Filename;

  // Show searching message
  FTextBuffer.NewLine;
  FTextBuffer.PutString('SEARCHING FOR ' + DisplayName);
  FTextBuffer.NewLine;

  // Check if file exists
  if not FileExists(Filename) then
  begin
    FTextBuffer.PutString('?FILE NOT FOUND');
    FTextBuffer.NewLine;
    FTextBuffer.PutString('READY.'); FReadyPrinted := True;
    FTextBuffer.NewLine;
    Exit;
  end;

  FileContent := TStringList.Create;
  try
    try
      FileContent.LoadFromFile(Filename);
    except
      on E: EFOpenError do
      begin
        FTextBuffer.PutString('?FILE OPEN ERROR');
        FTextBuffer.NewLine;
        FTextBuffer.PutString('READY.'); FReadyPrinted := True;
        FTextBuffer.NewLine;
        Exit;
      end;
      on E: EReadError do
      begin
        FTextBuffer.PutString('?FILE READ ERROR');
        FTextBuffer.NewLine;
        FTextBuffer.PutString('READY.'); FReadyPrinted := True;
        FTextBuffer.NewLine;
        Exit;
      end;
      on E: Exception do
      begin
        FTextBuffer.PutString('?I/O ERROR: ' + E.Message);
        FTextBuffer.NewLine;
        FTextBuffer.PutString('READY.'); FReadyPrinted := True;
        FTextBuffer.NewLine;
        Exit;
      end;
    end;

    // Show loading message
    FTextBuffer.PutString('LOADING');
    FTextBuffer.NewLine;

    // Clear current program and load the new one
    FProgramMemory.Clear;

    for i := 0 to FileContent.Count - 1 do
    begin
      Line := Trim(FileContent[i]);
      if Line = '' then
        Continue;

      // Parse line number and statement
      SpacePos := Pos(' ', Line);
      if SpacePos > 0 then
      begin
        if TryStrToInt(Copy(Line, 1, SpacePos - 1), LineNum) then
        begin
          Statement := Trim(Copy(Line, SpacePos + 1, Length(Line)));
          FProgramMemory.StoreLine(LineNum, Statement);
        end;
      end
      else
      begin
        // Line with only a number (valid - deletes line if exists)
        if TryStrToInt(Line, LineNum) then
          FProgramMemory.StoreLine(LineNum, '');
      end;
    end;

    FTextBuffer.PutString('READY.'); FReadyPrinted := True;
    FTextBuffer.NewLine;

  finally
    FileContent.Free;
  end;
end;

procedure TSedaiNewConsole.ExecuteSave(const Filename: string);
var
  FileContent: TStringList;
  DisplayName: string;
begin
  if Filename = '' then
  begin
    FTextBuffer.PutString('?MISSING FILENAME');
    FTextBuffer.NewLine;
    Exit;
  end;

  // Check if program exists
  if FProgramMemory.GetLineCount = 0 then
  begin
    FTextBuffer.PutString('?NO PROGRAM');
    FTextBuffer.NewLine;
    Exit;
  end;

  // Extract display name (filename without path)
  DisplayName := ExtractFileName(Filename);
  if DisplayName = '' then
    DisplayName := Filename;

  // Check if file exists and ask for confirmation
  if FileExists(Filename) then
  begin
    if not AskYesNo('FILE EXISTS. OVERWRITE? (Y/N) ') then
    begin
      FTextBuffer.PutString('CANCELLED');
      FTextBuffer.NewLine;
      Exit;
    end;
  end;

  // Show saving message (C64 style)
  FTextBuffer.NewLine;
  FTextBuffer.PutString('SAVING ' + DisplayName);
  FTextBuffer.NewLine;

  FileContent := FProgramMemory.GetAllLines;
  try
    try
      FileContent.SaveToFile(Filename);
      FTextBuffer.PutString('READY.'); FReadyPrinted := True;
      FTextBuffer.NewLine;
    except
      on E: EFCreateError do
      begin
        FTextBuffer.PutString('?FILE CREATE ERROR');
        FTextBuffer.NewLine;
        FTextBuffer.PutString('READY.'); FReadyPrinted := True;
        FTextBuffer.NewLine;
      end;
      on E: EWriteError do
      begin
        FTextBuffer.PutString('?FILE WRITE ERROR');
        FTextBuffer.NewLine;
        FTextBuffer.PutString('READY.'); FReadyPrinted := True;
        FTextBuffer.NewLine;
      end;
      on E: Exception do
      begin
        FTextBuffer.PutString('?I/O ERROR: ' + E.Message);
        FTextBuffer.NewLine;
        FTextBuffer.PutString('READY.'); FReadyPrinted := True;
        FTextBuffer.NewLine;
      end;
    end;
  finally
    FileContent.Free;
  end;
end;

function TSedaiNewConsole.ExecuteDirectory(const Pattern: string): TStringList;
var
  SearchRec: TSearchRec;
  SearchPath, SearchPattern, DirPath: string;
  LastSlashPos, i: Integer;
  FileList: TStringList;
  EntryName, EntryInfo: string;
  IsDir: Boolean;
  HasWildcard: Boolean;
begin
  Result := TStringList.Create;
  FileList := TStringList.Create;
  try
    // Determine search path and pattern
    if Pattern = '' then
    begin
      // No argument: list current directory
      DirPath := GetCurrentDir;
      SearchPattern := '*';
    end
    else
    begin
      // Check if pattern is a directory (no wildcards)
      HasWildcard := (Pos('*', Pattern) > 0) or (Pos('?', Pattern) > 0);

      if (not HasWildcard) and DirectoryExists(Pattern) then
      begin
        // Pattern is a directory path - list its contents
        DirPath := Pattern;
        SearchPattern := '*';
      end
      else
      begin
        // Check if pattern contains path separator
        LastSlashPos := 0;
        for i := Length(Pattern) downto 1 do
        begin
          if (Pattern[i] = '\') or (Pattern[i] = '/') then
          begin
            LastSlashPos := i;
            Break;
          end;
        end;

        if LastSlashPos > 0 then
        begin
          // Pattern has path component
          DirPath := Copy(Pattern, 1, LastSlashPos - 1);
          SearchPattern := Copy(Pattern, LastSlashPos + 1, MaxInt);
          if SearchPattern = '' then
            SearchPattern := '*';
        end
        else
        begin
          // Pattern is just a filename/wildcard
          DirPath := GetCurrentDir;
          SearchPattern := Pattern;
        end;
      end;
    end;

    // Build full search path
    if DirPath = '' then
      DirPath := GetCurrentDir;
    SearchPath := IncludeTrailingPathDelimiter(DirPath) + SearchPattern;

    // Check if directory exists
    if not DirectoryExists(DirPath) then
    begin
      Result.Add('?DIRECTORY NOT FOUND');
      FileList.Free;
      Exit;
    end;

    // Header
    Result.Add('DIRECTORY OF ' + DirPath);
    Result.Add('');

    // Search for files
    if FindFirst(SearchPath, faAnyFile, SearchRec) = 0 then
    begin
      try
        repeat
          // Skip . and ..
          if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
            Continue;

          IsDir := (SearchRec.Attr and faDirectory) <> 0;
          EntryName := SearchRec.Name;

          // Format: filename with directory marker
          if IsDir then
            EntryInfo := Format('%-32s <DIR>', [EntryName])
          else
            EntryInfo := Format('%-32s %12d', [EntryName, SearchRec.Size]);

          FileList.Add(EntryInfo);
        until FindNext(SearchRec) <> 0;
      finally
        SysUtils.FindClose(SearchRec);
      end;

      // Sort the file list
      FileList.Sort;

      // Add sorted entries to result
      for i := 0 to FileList.Count - 1 do
        Result.Add(FileList[i]);

      Result.Add('');
      Result.Add(Format('%d FILE(S)', [FileList.Count]));
    end
    else
    begin
      Result.Add('0 FILE(S)');
    end;

  finally
    FileList.Free;
  end;
end;

procedure TSedaiNewConsole.ExecuteCopy(const Source, Dest: string; Overwrite: Boolean);
var
  SearchRec: TSearchRec;
  SrcPath, SrcPattern, SrcDir: string;
  DestPath, DestFile: string;
  LastSlashPos, i: Integer;
  HasWildcard: Boolean;
  FilesCopied: Integer;
  SrcStream, DstStream: TFileStream;
  SourceFullPath: string;
  OverwriteAll: Boolean;
begin
  FilesCopied := 0;
  OverwriteAll := Overwrite;

  // Check for wildcards
  HasWildcard := (Pos('*', Source) > 0) or (Pos('?', Source) > 0);

  // Parse source path and pattern
  LastSlashPos := 0;
  for i := Length(Source) downto 1 do
  begin
    if (Source[i] = '\') or (Source[i] = '/') then
    begin
      LastSlashPos := i;
      Break;
    end;
  end;

  if LastSlashPos > 0 then
  begin
    SrcDir := Copy(Source, 1, LastSlashPos - 1);
    SrcPattern := Copy(Source, LastSlashPos + 1, MaxInt);
  end
  else
  begin
    SrcDir := GetCurrentDir;
    SrcPattern := Source;
  end;

  if SrcDir = '' then
    SrcDir := GetCurrentDir;

  SrcPath := IncludeTrailingPathDelimiter(SrcDir) + SrcPattern;

  // Determine destination
  if DirectoryExists(Dest) then
    DestPath := IncludeTrailingPathDelimiter(Dest)
  else
    DestPath := Dest;

  // If destination is a directory (ends with slash or is existing dir), we'll append filenames
  // If it's a file and we have wildcards, treat as directory
  if HasWildcard and (not DirectoryExists(Dest)) then
  begin
    // With wildcards, dest must be a directory
    if not DirectoryExists(Dest) then
    begin
      FTextBuffer.PutString('?DESTINATION MUST BE A DIRECTORY FOR WILDCARDS');
      FTextBuffer.NewLine;
      Exit;
    end;
  end;

  // Find and copy files
  if FindFirst(SrcPath, faAnyFile and not faDirectory, SearchRec) = 0 then
  begin
    try
      repeat
        // Skip directories
        if (SearchRec.Attr and faDirectory) <> 0 then
          Continue;

        SourceFullPath := IncludeTrailingPathDelimiter(SrcDir) + SearchRec.Name;

        // Determine destination filename
        if DirectoryExists(Dest) then
          DestFile := IncludeTrailingPathDelimiter(Dest) + SearchRec.Name
        else
          DestFile := Dest;

        // Show copying message
        FTextBuffer.PutString(SearchRec.Name + ' -> ' + ExtractFileName(DestFile));
        FTextBuffer.NewLine;
        RenderScreen;

        // Check if dest exists and ask for overwrite
        if FileExists(DestFile) then
        begin
          if not OverwriteAll then
          begin
            if not AskYesNoAll('OVERWRITE ' + ExtractFileName(DestFile) + '? (Y/N/A) ', OverwriteAll) then
            begin
              FTextBuffer.PutString('SKIPPED');
              FTextBuffer.NewLine;
              Continue;
            end;
          end;
        end;

        // Copy the file
        try
          SrcStream := TFileStream.Create(SourceFullPath, fmOpenRead or fmShareDenyWrite);
          try
            DstStream := TFileStream.Create(DestFile, fmCreate);
            try
              DstStream.CopyFrom(SrcStream, SrcStream.Size);
              Inc(FilesCopied);
            finally
              DstStream.Free;
            end;
          finally
            SrcStream.Free;
          end;
        except
          on E: EFOpenError do
          begin
            if Pos('Access denied', E.Message) > 0 then
              FTextBuffer.PutString('?ACCESS DENIED: ' + SearchRec.Name)
            else if Pos('Sharing violation', E.Message) > 0 then
              FTextBuffer.PutString('?FILE LOCKED: ' + SearchRec.Name)
            else
              FTextBuffer.PutString('?CANNOT OPEN: ' + SearchRec.Name);
            FTextBuffer.NewLine;
          end;
          on E: EFCreateError do
          begin
            if Pos('Access denied', E.Message) > 0 then
              FTextBuffer.PutString('?ACCESS DENIED (DEST): ' + ExtractFileName(DestFile))
            else
              FTextBuffer.PutString('?CANNOT CREATE: ' + ExtractFileName(DestFile));
            FTextBuffer.NewLine;
          end;
          on E: EWriteError do
          begin
            FTextBuffer.PutString('?WRITE ERROR: ' + ExtractFileName(DestFile));
            FTextBuffer.NewLine;
          end;
          on E: Exception do
          begin
            FTextBuffer.PutString('?ERROR: ' + E.Message);
            FTextBuffer.NewLine;
          end;
        end;

      until FindNext(SearchRec) <> 0;
    finally
      SysUtils.FindClose(SearchRec);
    end;

    FTextBuffer.PutString(Format('%d FILE(S) COPIED', [FilesCopied]));
    FTextBuffer.NewLine;
  end
  else
  begin
    FTextBuffer.PutString('?FILE NOT FOUND');
    FTextBuffer.NewLine;
  end;
end;

procedure TSedaiNewConsole.ExecuteScratch(const Pattern: string; Force: Boolean; Silent: Boolean);
var
  SearchRec: TSearchRec;
  SrcPath, SrcPattern, SrcDir: string;
  LastSlashPos, i: Integer;
  FilesDeleted: Integer;
  FullPath: string;
  DeleteAll: Boolean;
  DoDelete: Boolean;
  ErrCode: Integer;
begin
  FilesDeleted := 0;
  DeleteAll := Force;

  // Parse path and pattern
  LastSlashPos := 0;
  for i := Length(Pattern) downto 1 do
  begin
    if (Pattern[i] = '\') or (Pattern[i] = '/') then
    begin
      LastSlashPos := i;
      Break;
    end;
  end;

  if LastSlashPos > 0 then
  begin
    SrcDir := Copy(Pattern, 1, LastSlashPos - 1);
    SrcPattern := Copy(Pattern, LastSlashPos + 1, MaxInt);
  end
  else
  begin
    SrcDir := GetCurrentDir;
    SrcPattern := Pattern;
  end;

  if SrcDir = '' then
    SrcDir := GetCurrentDir;

  SrcPath := IncludeTrailingPathDelimiter(SrcDir) + SrcPattern;

  // Find and delete files
  if FindFirst(SrcPath, faAnyFile and not faDirectory, SearchRec) = 0 then
  begin
    try
      repeat
        // Skip directories
        if (SearchRec.Attr and faDirectory) <> 0 then
          Continue;

        FullPath := IncludeTrailingPathDelimiter(SrcDir) + SearchRec.Name;

        // Ask for confirmation if not Force and not DeleteAll
        DoDelete := DeleteAll;
        if not DoDelete then
        begin
          FTextBuffer.PutString(SearchRec.Name);
          FTextBuffer.NewLine;
          RenderScreen;

          if AskYesNoAll('DELETE? (Y/N/A) ', DeleteAll) then
            DoDelete := True
          else
          begin
            FTextBuffer.PutString('SKIPPED');
            FTextBuffer.NewLine;
          end;
        end;

        if DoDelete then
        begin
          // Try to delete the file
          try
            if not SysUtils.DeleteFile(FullPath) then
            begin
              ErrCode := GetLastOSError;
              case ErrCode of
                5:  // Access denied
                  FTextBuffer.PutString('?ACCESS DENIED: ' + SearchRec.Name);
                32: // Sharing violation (file locked)
                  FTextBuffer.PutString('?FILE LOCKED: ' + SearchRec.Name);
                else
                  FTextBuffer.PutString(Format('?ERROR %d: %s', [ErrCode, SearchRec.Name]));
              end;
              FTextBuffer.NewLine;
            end
            else
            begin
              if not DeleteAll then
              begin
                FTextBuffer.PutString('DELETED');
                FTextBuffer.NewLine;
              end;
              Inc(FilesDeleted);
            end;
          except
            on E: Exception do
            begin
              FTextBuffer.PutString('?ERROR: ' + E.Message);
              FTextBuffer.NewLine;
            end;
          end;
        end;

      until FindNext(SearchRec) <> 0;
    finally
      SysUtils.FindClose(SearchRec);
    end;

    FTextBuffer.PutString(Format('%d FILE(S) SCRATCHED', [FilesDeleted]));
    FTextBuffer.NewLine;
  end
  else
  begin
    // Only show error if not in silent mode
    if not Silent then
    begin
      FTextBuffer.PutString('?FILE NOT FOUND');
      FTextBuffer.NewLine;
    end;
  end;
end;

procedure TSedaiNewConsole.ExecuteRename(const OldName, NewName: string);
var
  ErrCode: Integer;
begin
  // Check if source file exists
  if not FileExists(OldName) then
  begin
    FTextBuffer.PutString('?FILE NOT FOUND: ' + ExtractFileName(OldName));
    FTextBuffer.NewLine;
    Exit;
  end;

  // Check if destination already exists
  if FileExists(NewName) then
  begin
    FTextBuffer.PutString('?FILE EXISTS: ' + ExtractFileName(NewName));
    FTextBuffer.NewLine;
    Exit;
  end;

  // Try to rename
  try
    if RenameFile(OldName, NewName) then
    begin
      FTextBuffer.PutString('RENAMED');
      FTextBuffer.NewLine;
    end
    else
    begin
      ErrCode := GetLastOSError;
      case ErrCode of
        5:  // Access denied
          FTextBuffer.PutString('?ACCESS DENIED');
        32: // Sharing violation
          FTextBuffer.PutString('?FILE LOCKED');
        else
          FTextBuffer.PutString(Format('?ERROR %d', [ErrCode]));
      end;
      FTextBuffer.NewLine;
    end;
  except
    on E: Exception do
    begin
      FTextBuffer.PutString('?ERROR: ' + E.Message);
      FTextBuffer.NewLine;
    end;
  end;
end;

procedure TSedaiNewConsole.ExecuteVerify(const Filename: string);
var
  FileContent: TStringList;
  MemoryContent: TStringList;
  DisplayName: string;
  ErrCode: Integer;
begin
  if Filename = '' then
  begin
    FTextBuffer.PutString('?MISSING FILENAME');
    FTextBuffer.NewLine;
    Exit;
  end;

  // Check if program exists in memory
  if FProgramMemory.GetLineCount = 0 then
  begin
    FTextBuffer.PutString('?NO PROGRAM');
    FTextBuffer.NewLine;
    Exit;
  end;

  // Check if file exists
  if not FileExists(Filename) then
  begin
    FTextBuffer.PutString('?FILE NOT FOUND');
    FTextBuffer.NewLine;
    Exit;
  end;

  // Extract display name
  DisplayName := ExtractFileName(Filename);
  if DisplayName = '' then
    DisplayName := Filename;

  // Show verifying message
  FTextBuffer.NewLine;
  FTextBuffer.PutString('VERIFYING ' + DisplayName);
  FTextBuffer.NewLine;
  RenderScreen;

  FileContent := nil;
  MemoryContent := nil;
  try
    try
      // Load file content
      FileContent := TStringList.Create;
      FileContent.LoadFromFile(Filename);

      // Get program from memory
      MemoryContent := FProgramMemory.GetAllLines;

      // Compare content
      if FileContent.Text = MemoryContent.Text then
      begin
        FTextBuffer.PutString('OK');
        FTextBuffer.NewLine;
      end
      else
      begin
        FTextBuffer.PutString('?VERIFY ERROR');
        FTextBuffer.NewLine;
      end;
    except
      on E: EFOpenError do
      begin
        if Pos('Access denied', E.Message) > 0 then
          FTextBuffer.PutString('?ACCESS DENIED')
        else if Pos('Sharing violation', E.Message) > 0 then
          FTextBuffer.PutString('?FILE LOCKED')
        else
          FTextBuffer.PutString('?CANNOT OPEN: ' + DisplayName);
        FTextBuffer.NewLine;
      end;
      on E: EReadError do
      begin
        FTextBuffer.PutString('?READ ERROR');
        FTextBuffer.NewLine;
      end;
      on E: Exception do
      begin
        ErrCode := GetLastOSError;
        case ErrCode of
          5:  FTextBuffer.PutString('?ACCESS DENIED');
          32: FTextBuffer.PutString('?FILE LOCKED');
          else FTextBuffer.PutString('?ERROR: ' + E.Message);
        end;
        FTextBuffer.NewLine;
      end;
    end;
  finally
    if Assigned(FileContent) then
      FileContent.Free;
    if Assigned(MemoryContent) then
      MemoryContent.Free;
  end;
end;

function TSedaiNewConsole.SourceHasLineNumbers(const AFileName: string): Boolean;
// A classic line-numbered BASIC source has a leading integer on its lines; a FreeBASIC (MODERN) source
// never does. Scan the non-empty lines: if the first whitespace-delimited token is a non-negative
// integer on any line, treat the file as classic.
var
  Lines: TStringList;
  i, SpacePos, N: Integer;
  L, Tok: string;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;
  Lines := TStringList.Create;
  try
    try Lines.LoadFromFile(AFileName); except Exit; end;
    for i := 0 to Lines.Count - 1 do
    begin
      L := Trim(Lines[i]);
      if L = '' then Continue;
      SpacePos := Pos(' ', L);
      if SpacePos > 0 then Tok := Copy(L, 1, SpacePos - 1) else Tok := L;
      if TryStrToInt(Tok, N) and (N >= 0) then Exit(True);
    end;
  finally
    Lines.Free;
  end;
end;

procedure TSedaiNewConsole.LoadModernSource(const AFileName: string);
// Compile a FreeBASIC (MODERN) source file straight to bytecode and switch to bytecode-run mode (the
// line-keyed program store cannot hold a number-less source). RUN then executes FLoadedBytecode.
var
  Runner: TSedaiRunner;
begin
  FTextBuffer.NewLine;
  FTextBuffer.PutString('LOADING ' + ExtractFileName(AFileName));
  FTextBuffer.NewLine;
  RenderScreen;
  Runner := TSedaiRunner.Create;
  try
    if Assigned(FLoadedBytecode) then FreeAndNil(FLoadedBytecode);
    FProgramMemory.Clear;
    try
      FLoadedBytecode := Runner.LoadFromSource(AFileName);
    except
      on E: Exception do
      begin
        FLoadedBytecode := nil;
        FBytecodeMode := False;
        FTextBuffer.PutString('?COMPILE ERROR: ' + E.Message);
        FTextBuffer.NewLine;
        FTextBuffer.PutString('READY.'); FReadyPrinted := True;
        FTextBuffer.NewLine;
        Exit;
      end;
    end;
    if Assigned(FLoadedBytecode) then
    begin
      FBytecodeMode := True;
      FTextBuffer.PutString('READY.'); FReadyPrinted := True;
      FTextBuffer.NewLine;
    end
    else
    begin
      FBytecodeMode := False;
      FTextBuffer.PutString('?LOAD FAILED');
      FTextBuffer.NewLine;
      FTextBuffer.PutString('READY.'); FReadyPrinted := True;
      FTextBuffer.NewLine;
    end;
  finally
    Runner.Free;
  end;
end;

procedure TSedaiNewConsole.ExecuteBLoad(const Filename: string);
var
  Serializer: TBytecodeSerializer;
  DisplayName: string;
  ErrCode: Integer;
begin
  if Filename = '' then
  begin
    FTextBuffer.PutString('?MISSING FILENAME');
    FTextBuffer.NewLine;
    Exit;
  end;

  // Check if file exists
  if not FileExists(Filename) then
  begin
    FTextBuffer.PutString('?FILE NOT FOUND');
    FTextBuffer.NewLine;
    Exit;
  end;

  // Extract display name
  DisplayName := ExtractFileName(Filename);
  if DisplayName = '' then
    DisplayName := Filename;

  // Show loading message
  FTextBuffer.NewLine;
  FTextBuffer.PutString('LOADING ' + DisplayName);
  FTextBuffer.NewLine;
  RenderScreen;

  Serializer := TBytecodeSerializer.Create;
  try
    try
      // Free previous bytecode if any
      if Assigned(FLoadedBytecode) then
      begin
        FLoadedBytecode.Free;
        FLoadedBytecode := nil;
      end;

      // Clear program memory (switching to bytecode mode)
      FProgramMemory.Clear;

      // Load bytecode
      FLoadedBytecode := Serializer.LoadFromFile(Filename);
      FBytecodeMode := True;

      FTextBuffer.PutString('READY.'); FReadyPrinted := True;
      FTextBuffer.NewLine;
    except
      on E: EFOpenError do
      begin
        FBytecodeMode := False;
        if Pos('Access denied', E.Message) > 0 then
          FTextBuffer.PutString('?ACCESS DENIED')
        else if Pos('Sharing violation', E.Message) > 0 then
          FTextBuffer.PutString('?FILE LOCKED')
        else
          FTextBuffer.PutString('?CANNOT OPEN: ' + DisplayName);
        FTextBuffer.NewLine;
      end;
      on E: EReadError do
      begin
        FBytecodeMode := False;
        FTextBuffer.PutString('?READ ERROR');
        FTextBuffer.NewLine;
      end;
      on E: Exception do
      begin
        FBytecodeMode := False;
        ErrCode := GetLastOSError;
        case ErrCode of
          5:  FTextBuffer.PutString('?ACCESS DENIED');
          32: FTextBuffer.PutString('?FILE LOCKED');
          else
          begin
            if Pos('Invalid bytecode', E.Message) > 0 then
              FTextBuffer.PutString('?INVALID BYTECODE')
            else if Pos('checksum', LowerCase(E.Message)) > 0 then
              FTextBuffer.PutString('?CHECKSUM ERROR')
            else
              FTextBuffer.PutString('?ERROR: ' + E.Message);
          end;
        end;
        FTextBuffer.NewLine;
      end;
    end;
  finally
    Serializer.Free;
  end;
end;

procedure TSedaiNewConsole.ExecuteBSave(const Filename: string);
var
  Serializer: TBytecodeSerializer;
  Compiler: TBytecodeCompiler;
  SSAGen: TSSAGenerator;
  AST: TASTNode;
  SSAProgram: TSSAProgram;
  BytecodeProgram: TBytecodeProgram;
  DisplayName: string;
  ErrCode: Integer;
begin
  if Filename = '' then
  begin
    FTextBuffer.PutString('?MISSING FILENAME');
    FTextBuffer.NewLine;
    Exit;
  end;

  // In bytecode mode, save the loaded bytecode
  if FBytecodeMode then
  begin
    if not Assigned(FLoadedBytecode) then
    begin
      FTextBuffer.PutString('?NO BYTECODE');
      FTextBuffer.NewLine;
      Exit;
    end;

    DisplayName := ExtractFileName(Filename);
    if DisplayName = '' then
      DisplayName := Filename;

    // Check if file exists and ask for confirmation
    if FileExists(Filename) then
    begin
      if not AskYesNo('FILE EXISTS. OVERWRITE? (Y/N) ') then
      begin
        FTextBuffer.PutString('CANCELLED');
        FTextBuffer.NewLine;
        Exit;
      end;
    end;

    FTextBuffer.NewLine;
    FTextBuffer.PutString('SAVING ' + DisplayName);
    FTextBuffer.NewLine;
    RenderScreen;

    Serializer := TBytecodeSerializer.Create;
    try
      try
        Serializer.SaveToFile(FLoadedBytecode, Filename);
        FTextBuffer.PutString('READY.'); FReadyPrinted := True;
        FTextBuffer.NewLine;
      except
        on E: EFCreateError do
        begin
          if Pos('Access denied', E.Message) > 0 then
            FTextBuffer.PutString('?ACCESS DENIED')
          else
            FTextBuffer.PutString('?CANNOT CREATE: ' + DisplayName);
          FTextBuffer.NewLine;
        end;
        on E: EWriteError do
        begin
          FTextBuffer.PutString('?WRITE ERROR');
          FTextBuffer.NewLine;
        end;
        on E: Exception do
        begin
          ErrCode := GetLastOSError;
          case ErrCode of
            5:  FTextBuffer.PutString('?ACCESS DENIED');
            32: FTextBuffer.PutString('?FILE LOCKED');
            else FTextBuffer.PutString('?ERROR: ' + E.Message);
          end;
          FTextBuffer.NewLine;
        end;
      end;
    finally
      Serializer.Free;
    end;
    Exit;
  end;

  // Normal mode: compile program and save bytecode
  if FProgramMemory.GetLineCount = 0 then
  begin
    FTextBuffer.PutString('?NO PROGRAM');
    FTextBuffer.NewLine;
    Exit;
  end;

  DisplayName := ExtractFileName(Filename);
  if DisplayName = '' then
    DisplayName := Filename;

  // Check if file exists and ask for confirmation
  if FileExists(Filename) then
  begin
    if not AskYesNo('FILE EXISTS. OVERWRITE? (Y/N) ') then
    begin
      FTextBuffer.PutString('CANCELLED');
      FTextBuffer.NewLine;
      Exit;
    end;
  end;

  FTextBuffer.NewLine;
  FTextBuffer.PutString('COMPILING ' + DisplayName);
  FTextBuffer.NewLine;
  RenderScreen;

  // Get AST from program memory
  try
    AST := FProgramMemory.GetOrBuildAST;
    if not Assigned(AST) then
    begin
      FTextBuffer.PutString('?SYNTAX ERROR');
      FTextBuffer.NewLine;
      Exit;
    end;
  except
    on E: Exception do
    begin
      FTextBuffer.PutString(E.Message);
      FTextBuffer.NewLine;
      Exit;
    end;
  end;

  // Build SSA
  SSAGen := TSSAGenerator.Create;
  Compiler := TBytecodeCompiler.Create;
  Serializer := TBytecodeSerializer.Create;
  try
    try
      SSAProgram := SSAGen.Generate(AST);
      if not Assigned(SSAProgram) then
      begin
        FTextBuffer.PutString('?COMPILATION ERROR');
        FTextBuffer.NewLine;
        Exit;
      end;

      BytecodeProgram := Compiler.Compile(SSAProgram);
      if not Assigned(BytecodeProgram) then
      begin
        FTextBuffer.PutString('?COMPILATION ERROR');
        FTextBuffer.NewLine;
        SSAProgram.Free;
        Exit;
      end;

      FTextBuffer.PutString('SAVING ' + DisplayName);
      FTextBuffer.NewLine;
      RenderScreen;

      Serializer.SaveToFile(BytecodeProgram, Filename);

      FTextBuffer.PutString('READY.'); FReadyPrinted := True;
      FTextBuffer.NewLine;

      BytecodeProgram.Free;
      SSAProgram.Free;
    except
      on E: EFCreateError do
      begin
        if Pos('Access denied', E.Message) > 0 then
          FTextBuffer.PutString('?ACCESS DENIED')
        else
          FTextBuffer.PutString('?CANNOT CREATE: ' + DisplayName);
        FTextBuffer.NewLine;
      end;
      on E: EWriteError do
      begin
        FTextBuffer.PutString('?WRITE ERROR');
        FTextBuffer.NewLine;
      end;
      on E: Exception do
      begin
        ErrCode := GetLastOSError;
        case ErrCode of
          5:  FTextBuffer.PutString('?ACCESS DENIED');
          32: FTextBuffer.PutString('?FILE LOCKED');
          else FTextBuffer.PutString('?ERROR: ' + E.Message);
        end;
        FTextBuffer.NewLine;
      end;
    end;
  finally
    Serializer.Free;
    Compiler.Free;
    SSAGen.Free;
  end;
end;

procedure TSedaiNewConsole.HandleDiskFile(Sender: TBytecodeVM; const Command: string;
  Handle: Integer; const HandleName, Filename, Mode: string;
  var ErrorCode: Integer);
var
  FileMode: Word;
  FullPath: string;
begin
  ErrorCode := 0;

  // DCLEAR uses Handle=0 as signal to close all handles - handle it first
  if Command = 'DCLEAR' then
  begin
    CloseAllFileHandles;
    Exit;
  end;

  // Validate handle range for other commands
  if (Handle < 1) or (Handle > 15) then
  begin
    ErrorCode := 64;  // FILE NOT OPEN error
    Exit;
  end;

  if Command = 'DOPEN' then
  begin
    // Close existing handle if open
    if Assigned(FFileHandles[Handle]) then
    begin
      FFileHandles[Handle].Free;
      FFileHandles[Handle] := nil;
      FFileNames[Handle] := '';
      FFileModes[Handle] := '';
    end;

    // Resolve path
    FullPath := Filename;
    if not FileExists(FullPath) then
    begin
      // For write/append modes, we can create the file
      if (Pos('W', UpperCase(Mode)) > 0) or (Pos('A', UpperCase(Mode)) > 0) then
      begin
        // File will be created
      end
      else
      begin
        ErrorCode := 62;  // FILE NOT FOUND
        Exit;
      end;
    end;

    // Determine file mode
    if Pos('W', UpperCase(Mode)) > 0 then
      FileMode := fmCreate
    else if Pos('A', UpperCase(Mode)) > 0 then
    begin
      if FileExists(FullPath) then
        FileMode := fmOpenReadWrite
      else
        FileMode := fmCreate;
    end
    else // Read mode (default)
      FileMode := fmOpenRead or fmShareDenyNone;

    try
      FFileHandles[Handle] := TFileStream.Create(FullPath, FileMode);
      FFileNames[Handle] := FullPath;
      FFileModes[Handle] := UpperCase(Mode);

      // For append mode, seek to end
      if Pos('A', FFileModes[Handle]) > 0 then
        FFileHandles[Handle].Seek(0, soEnd);
    except
      on E: EFOpenError do
      begin
        ErrorCode := 62;  // FILE NOT FOUND
        FFileHandles[Handle] := nil;
      end;
      on E: EFCreateError do
      begin
        ErrorCode := 26;  // FILE CREATION ERROR
        FFileHandles[Handle] := nil;
      end;
      on E: Exception do
      begin
        ErrorCode := 70;  // DISK ERROR
        FFileHandles[Handle] := nil;
      end;
    end;
  end
  else if Command = 'DCLOSE' then
  begin
    if Assigned(FFileHandles[Handle]) then
    begin
      FFileHandles[Handle].Free;
      FFileHandles[Handle] := nil;
      FFileNames[Handle] := '';
      FFileModes[Handle] := '';
    end;
    // Closing a non-open handle is not an error in C128 BASIC
  end;
end;

procedure TSedaiNewConsole.HandleFileData(Sender: TBytecodeVM; const Command: string;
  Handle: Integer; var Data: string; var ErrorCode: Integer);
var
  BytesRead: Integer;
  Ch: Byte;
  Line: string;
  M: string;
  RetType, V: Integer;
begin
  ErrorCode := 0;

  // FILEATTR(filenum, returntype): info about an open file number (returntype in Data, result back in
  // Data). 1 = File Mode (Input1/Output2/Random4/Append8/Binary32), 2 = OS handle, 3 = Encoding (ASCII).
  // Handled before the range check so an invalid/closed handle yields 0 rather than a fatal error.
  if Command = 'FILEATTR' then
  begin
    RetType := StrToIntDef(Data, 1);
    Data := '0';
    if (Handle >= 1) and (Handle <= 15) and Assigned(FFileHandles[Handle]) then
      case RetType of
        2: Data := IntToStr(PtrInt(FFileHandles[Handle].Handle));  // OS file handle
        3: Data := '0';   // Encoding: ASCII
      else
        begin
          M := UpperCase(FFileModes[Handle]);
          V := 0;
          if (Length(M) >= 1) and (M[1] = 'L') then
            V := 4                                    // relative file = Random
          else
          begin
            if Pos('R', M) > 0 then V := V or 1;      // Input
            if Pos('W', M) > 0 then V := V or 2;      // Output
            if Pos('A', M) > 0 then V := V or 8;      // Append
            if Pos('B', M) > 0 then V := V or 32;     // Binary
          end;
          Data := IntToStr(V);
        end;
      end;
    Exit;
  end;

  // Validate handle range
  if (Handle < 1) or (Handle > 15) then
  begin
    ErrorCode := 64;  // FILE NOT OPEN error
    Exit;
  end;

  if not Assigned(FFileHandles[Handle]) then
  begin
    ErrorCode := 64;  // FILE NOT OPEN
    Exit;
  end;

  if Command = 'FILESETEOF' then
  begin
    // FreeBASIC FILESETEOF: set the file length to the current position (truncate/extend). Status 0 = OK.
    try
      FFileHandles[Handle].Size := FFileHandles[Handle].Position;
      FFileHandles[Handle].Position := FFileHandles[Handle].Size;
      Data := '0';
    except
      ErrorCode := 63; Data := IntToStr(ErrorCode);
    end;
    Exit;
  end;

  if Command = 'GET#' then
  begin
    // Read one character
    BytesRead := FFileHandles[Handle].Read(Ch, 1);
    if BytesRead > 0 then
      Data := Chr(Ch)
    else
      Data := '';  // EOF returns empty string
  end
  else if Command = 'INPUT#' then
  begin
    // Check for EOF before reading
    if FFileHandles[Handle].Position >= FFileHandles[Handle].Size then
    begin
      ErrorCode := 62;  // FILE DATA ERROR - EOF reached
      Data := '';
      Exit;
    end;
    // Read a line (until CR, LF, or comma)
    Line := '';
    while FFileHandles[Handle].Position < FFileHandles[Handle].Size do
    begin
      FFileHandles[Handle].Read(Ch, 1);
      if Ch in [10, 13] then  // LF or CR
      begin
        // Skip following LF after CR (Windows line endings)
        if (Ch = 13) and (FFileHandles[Handle].Position < FFileHandles[Handle].Size) then
        begin
          FFileHandles[Handle].Read(Ch, 1);
          if Ch <> 10 then
            FFileHandles[Handle].Seek(-1, soCurrent);  // Put back non-LF
        end;
        Break;
      end
      else if Ch = Ord(',') then
      begin
        Break;  // Comma also terminates INPUT#
      end
      else
        Line := Line + Chr(Ch);
    end;
    Data := Line;
  end
  else if Command = 'PRINT#' then
  begin
    // Write data to file
    if Length(Data) > 0 then
    begin
      try
        FFileHandles[Handle].Write(Data[1], Length(Data));
      except
        ErrorCode := 25;  // WRITE ERROR
      end;
    end;
  end
  else if Command = 'CMD' then
  begin
    // CMD redirects output - similar to PRINT#
    if Length(Data) > 0 then
    begin
      try
        FFileHandles[Handle].Write(Data[1], Length(Data));
      except
        ErrorCode := 25;  // WRITE ERROR
      end;
    end;
  end
  else if Command = 'APPEND' then
  begin
    // APPEND - write data to file (same as PRINT# but named differently for clarity)
    if Length(Data) > 0 then
    begin
      try
        FFileHandles[Handle].Write(Data[1], Length(Data));
      except
        ErrorCode := 25;  // WRITE ERROR
      end;
    end;
  end
  else if Command = 'RECORD' then
  begin
    // RECORD - seek to position in file
    // Data contains the position as a string
    try
      FFileHandles[Handle].Position := StrToInt64(Data);
    except
      on E: EConvertError do
        ErrorCode := 63;  // ILLEGAL QUANTITY
      on E: Exception do
        ErrorCode := 70;  // DISK ERROR
    end;
  end;
end;

procedure TSedaiNewConsole.CloseAllFileHandles;
var
  I: Integer;
begin
  for I := 1 to 15 do
  begin
    if Assigned(FFileHandles[I]) then
    begin
      FFileHandles[I].Free;
      FFileHandles[I] := nil;
      FFileNames[I] := '';
      FFileModes[I] := '';
    end;
  end;
end;

procedure TSedaiNewConsole.ExecuteConsoleCommand(AST: TASTNode);
var
  i, j: Integer;
  CmdNode: TASTNode;
  CmdName: string;
  Lines: TStringList;
begin
  if not Assigned(AST) then
    Exit;

  // The program can contain multiple statements
  if AST.NodeType = antProgram then
  begin
    for i := 0 to AST.ChildCount - 1 do
    begin
      CmdNode := AST.Child[i];

      // Get the command from the token or from the node value
      if Assigned(CmdNode.Token) then
        CmdName := UpperCase(CmdNode.Token.Value)
      else if not VarIsEmpty(CmdNode.Value) then
        CmdName := UpperCase(VarToStr(CmdNode.Value))
      else
        CmdName := '';

      // Commands supported by the BASIC interpreter
      if CmdName = 'LIST' then
      begin
        {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: LIST command - Getting lines from program memory');{$ENDIF}
        // List program in memory
        if not Assigned(FProgramMemory) then
        begin
          {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: ERROR - FProgramMemory is nil!');{$ENDIF}
          FTextBuffer.PutString('?INTERNAL ERROR');
          FTextBuffer.NewLine;
          Exit;
        end;

        Lines := FProgramMemory.GetAllLines;
        {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Got ', Lines.Count, ' lines');{$ENDIF}
        try
          if Lines.Count = 0 then
          begin
            FTextBuffer.PutString('NO PROGRAM');
            FTextBuffer.NewLine;
          end
          else
          begin
            for j := 0 to Lines.Count - 1 do
            begin
              {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Outputting line ', j, ': ', Lines[j]);{$ENDIF}
              FTextBuffer.PutString(Lines[j]);
              FTextBuffer.NewLine;
            end;
          end;
          {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: LIST completed');{$ENDIF}
        finally
          Lines.Free;
        end;
      end
      else if CmdName = 'GLIST' then
      begin
        // List available SDL2 video modes
        ExecuteGLIST;
      end
      else if CmdName = 'NEW' then
      begin
        // Cancella programma in memoria
        FProgramMemory.Clear;
      end
      else if CmdName = 'RUN' then
      begin
        // TODO: Esegui programma
        FTextBuffer.PutString('?NOT IMPLEMENTED');
        FTextBuffer.NewLine;
      end
      else if CmdName = 'LOAD' then
      begin
        // TODO: Load from file
        FTextBuffer.PutString('?NOT IMPLEMENTED');
        FTextBuffer.NewLine;
      end
      else if CmdName = 'SAVE' then
      begin
        // TODO: Save to file
        FTextBuffer.PutString('?NOT IMPLEMENTED');
        FTextBuffer.NewLine;
      end
      else if CmdName = 'CLS' then
      begin
        FTextBuffer.Clear;
      end
      else if (CmdNode.NodeType = antPrint) or (CmdName = 'PRINT') or (CmdName = '?') then
      begin
        // TODO: Esegui PRINT direttamente
        FTextBuffer.PutString('?NOT IMPLEMENTED');
        FTextBuffer.NewLine;
      end
      else
      begin
        // Comando non riconosciuto
        FTextBuffer.PutString('?SYNTAX ERROR');
        FTextBuffer.NewLine;
      end;
    end;
  end;
end;

procedure TSedaiNewConsole.SetStartupFile(const AFileName: string; AAutoRun: Boolean);
begin
  FStartupFile := AFileName;
  FAutoRun := AAutoRun;
  // Sprite files (SPRSAVE/SPRLOAD and the editor) default to the program's folder.
  if Assigned(FSpriteEngine) then
    FSpriteEngine.SetBaseDir(ExtractFilePath(AFileName));
end;

procedure TSedaiNewConsole.SetHistoryFile(const AFileName: string);
begin
  FHistoryFile := AFileName;
end;

function TSedaiNewConsole.GetHistoryFilePath: string;
var
  HomeDir: string;
begin
  // If custom path set, use it
  if FHistoryFile <> '' then
  begin
    Result := FHistoryFile;
    Exit;
  end;

  // Default: ~/.sedai/.sbv_history
  {$IFDEF WINDOWS}
  HomeDir := SysUtils.GetEnvironmentVariable('USERPROFILE');
  if HomeDir = '' then
    HomeDir := SysUtils.GetEnvironmentVariable('HOMEDRIVE') + SysUtils.GetEnvironmentVariable('HOMEPATH');
  {$ELSE}
  HomeDir := SysUtils.GetEnvironmentVariable('HOME');
  {$ENDIF}

  if HomeDir <> '' then
    Result := IncludeTrailingPathDelimiter(HomeDir) + '.sedai' + PathDelim + '.sbv_history'
  else
    Result := '';
end;

procedure TSedaiNewConsole.LoadHistory;
var
  HistPath: string;
  F: TextFile;
  Line: string;
begin
  HistPath := GetHistoryFilePath;
  if (HistPath = '') or (not FileExists(HistPath)) then
    Exit;

  try
    AssignFile(F, HistPath);
    Reset(F);
    try
      FHistoryCount := 0;
      while (not Eof(F)) and (FHistoryCount < INPUT_HISTORY_SIZE) do
      begin
        ReadLn(F, Line);
        if Line <> '' then
        begin
          FInputHistory[FHistoryCount] := Line;
          Inc(FHistoryCount);
        end;
      end;
    finally
      CloseFile(F);
    end;
  except
    // Silently ignore errors loading history
  end;
end;

procedure TSedaiNewConsole.SaveHistory;
var
  HistPath, HistDir: string;
  F: TextFile;
begin
  HistPath := GetHistoryFilePath;
  if HistPath = '' then
    Exit;
  if FHistoryCount = 0 then
    Exit;

  // Create directory if needed
  HistDir := ExtractFilePath(HistPath);
  if (HistDir <> '') and (not DirectoryExists(HistDir)) then
  begin
    try
      ForceDirectories(HistDir);
    except
      Exit; // Can't create directory, skip saving
    end;
  end;

  try
    AssignFile(F, HistPath);
    // Append only the last command for efficiency
    if FileExists(HistPath) then
      Append(F)
    else
      Rewrite(F);
    try
      // Write only the last entry (the one just added)
      WriteLn(F, FInputHistory[FHistoryCount - 1]);
    finally
      CloseFile(F);
    end;
  except
    // Silently ignore errors saving history
  end;
end;

procedure TSedaiNewConsole.SaveHistoryFull;
var
  HistPath, HistDir: string;
  F: TextFile;
  i: Integer;
begin
  HistPath := GetHistoryFilePath;
  if HistPath = '' then
    Exit;
  if FHistoryCount = 0 then
    Exit;

  // Create directory if needed
  HistDir := ExtractFilePath(HistPath);
  if (HistDir <> '') and (not DirectoryExists(HistDir)) then
  begin
    try
      ForceDirectories(HistDir);
    except
      Exit;
    end;
  end;

  try
    AssignFile(F, HistPath);
    Rewrite(F);  // Always rewrite the entire file
    try
      for i := 0 to FHistoryCount - 1 do
        WriteLn(F, FInputHistory[i]);
    finally
      CloseFile(F);
    end;
  except
    // Silently ignore errors saving history
  end;
end;

{ Deferred rendering: periodic render callback during VM execution }

function TSedaiNewConsole.HandleVMEventPoll: Boolean;
var
  Now: UInt32;
begin
  Result := False;

  // Process SDL events (CTRL+C, window close)
  if Assigned(FProgramInputHandler) then
  begin
    FProgramInputHandler.ProcessEvents;
    if FProgramInputHandler.QuitRequested then
    begin
      // CTRL+ALT+END during execution: quit sbv with a single press. Result:=True
      // stops the VM; FRunning:=False makes the REPL loop exit once control returns,
      // otherwise the quit flag (on the program input handler) is never seen by the
      // REPL loop, which only checks FInputHandler — so the app wouldn't close.
      FRunning := False;
      Result := True;
      Exit;
    end;
    if FProgramInputHandler.StopRequested then
    begin
      Result := True;
      Exit;
    end;
  end;

  // A render-target reset (ALT+TAB on Windows D3D) lost the persistent text
  // texture; rebuild it and repaint so a running program isn't left black.
  if Assigned(FProgramInputHandler) and FProgramInputHandler.RenderResetPending then
  begin
    FProgramInputHandler.RenderResetPending := False;
    RefreshDisplay;
  end;

  // CTRL+F pressed during program execution: toggle fullscreen now.
  if Assigned(FProgramInputHandler) and FProgramInputHandler.ToggleFullscreenRequested then
  begin
    FProgramInputHandler.ToggleFullscreenRequested := False;
    ToggleFullscreen;
    FLastVMRenderTick := SDL_GetTicks;  // avoid an immediate double render this tick
  end;

  // Periodic rendering (~60 FPS)
  Now := SDL_GetTicks;
  if (Now - FLastVMRenderTick) >= 16 then
  begin
    // Update sprite auto-movement
    if Assigned(FSpriteEngine) then
      FSpriteEngine.UpdateSprites((Now - FLastVMRenderTick) / 1000.0);
    FLastVMRenderTick := Now;
    // Render when text changed or sprites are active
    if FTextBuffer.NeedsRedraw or Assigned(FSpriteEngine) then
    begin
      RenderScreen;  // includes sprite rendering
      UpdateCursor;
      FVideoController.Present;
    end;
  end;
end;

procedure TSedaiNewConsole.BeginVMExecution;
begin
  FCursorEnabled := False;  // Hide cursor during program execution
  FLastVMRenderTick := SDL_GetTicks;
  FBytecodeVM.EventPollCallback := @HandleVMEventPoll;
  FBytecodeVM.SpriteEditorCallback := @RunSpriteEditor;
end;

procedure TSedaiNewConsole.EndVMExecution;
begin
  FBytecodeVM.EventPollCallback := nil;
  FBytecodeVM.SpriteEditorCallback := nil;
  FCursorEnabled := True;  // Restore cursor for REPL
  // Final render to show the last state (including sprites via RenderScreen)
  RenderScreen;
  UpdateCursor;
  FVideoController.Present;
end;

// SPRDEF: modal interactive sprite editor. Takes over the SDL window with a magnified
// 24x21 grid, lets the user paint the shape (hi-res or multicolor), toggle expand and
// pick the sprite color, then writes the result back into the sprite via the engine.
// Returns True only if the window was closed (so the VM should stop).
function TSedaiNewConsole.RunSpriteEditor(SpriteNum: Integer): Boolean;
const
  MAXUNDO = 30;        // per-sprite undo/redo depth
  MAX_DIM = 256;       // hard cap on either sprite dimension
  PRESET_COUNT = 8;
  // Deferred widget actions (need helpers declared after DrawEditor, so the
  // widget records the action and the main loop performs it).
  uaNone = 0;
  uaPreset = 1;        // UiArg = preset index 1..PRESET_COUNT
  uaFmtApply = 2;      // apply the pending format change (clears the grid)
  uaFmtCancel = 3;     // cancel the pending format change
  uaSizeApply = 4;     // apply the typed W x H size fields
  uaGoto = 5;          // switch to sprite UiArg (deferred: needs LoadSpriteState)
const
  // Size presets (C128 default + SNES/console-style squares & tall shapes).
  PresetW: array[1..PRESET_COUNT] of Integer = (24,  8, 16, 32, 64, 16, 32, 48);
  PresetH: array[1..PRESET_COUNT] of Integer = (21,  8, 16, 32, 64, 32, 64, 42);
  PresetNm: array[1..PRESET_COUNT] of string = (
    'C128 24x21', '8x8', '16x16', '32x32', '64x64', '16x32', '32x64', '48x42');
  // Built-in global palette models (TGraphicsMemory.LoadPalettePreset ids).
  PAL_COUNT = 4;
  PalNm: array[0..PAL_COUNT - 1] of string = ('C64/C128', 'Wheel', 'Greyscale', 'Rainbow');
type
  // Undo/redo snapshot: grid contents plus the size/format they belong to
  // (both size and format can change during an editing session).
  TGridSnap = record
    W, H, Fmt: Integer;
    Data: TBytes;        // flattened H*W cell values
  end;
var
  // Current grid dimensions and data format (0=hi-res, 1=multicolor, 2=full-color).
  GW, GH, Fmt: Integer;
  Grid: array of array of Byte;   // [row][col] cell values (meaning depends on Fmt)
  // Per-sprite undo/redo stacks (independent history per sprite; lazy-allocated).
  Undo, Redo: array[1..MAX_SPRITES] of array of TGridSnap;
  Info: TSpriteInfo;
  CX, CY: Integer;
  Multicolor, FullColor, ExpandX, ExpandY: Boolean;
  SprColIdx, MC1Idx, MC2Idx: Byte;
  PenIdx: Byte;     // full-color pen: palette index 0..255 (0 = transparent)
  MulPen: Byte;     // multicolor mouse pen value 1..3
  Running, ConfirmSwitch, ClipValid, HasAnchor, IsShiftArrow, OverwritePending: Boolean;
  // Shape clipboard: a region of values + a mask of which cells are real.
  Clip: array of array of Byte;
  ClipSel: array of array of Boolean;
  ClipW, ClipH: Integer;
  // Selection mask (+ base for additive drags) and the rectangle anchor.
  Sel, BaseSel: array of array of Boolean;
  SelAnchorX, SelAnchorY: Integer;
  Event: TSDL_Event;
  Sym: TSDL_KeyCode;
  Mods: Word;
  // InputMode: 0=none, 1=save filename, 2=load filename, 3=size preset menu,
  // 4=free size entry (WxH typed)
  InputMode: Integer;
  InputText: string;    // filename / size being typed (shown below the grid)
  LastFile: string;     // last saved/loaded name (shown + re-proposed on save)
  // Layout captured by the last DrawEditor pass, used to hit-test mouse clicks.
  LGX, LGY, LCell: Integer;          // grid origin + cell size (pixels)
  LViewCol, LViewRow: Integer;       // top-left visible cell (scroll position)
  LViewW, LViewH: Integer;           // drawn grid viewport size (pixels)
  LSwatchX, LSwatchY: Integer;       // pen colour swatch position (anchors the pop-up)
  LPalX, LPalY, LPalCell: Integer;   // palette picker origin + swatch size
  LPalShown: Boolean;                // palette picker drawn this frame (full-color)
  MouseDrawing: Boolean;             // a mouse button is held over the grid
  MouseErase: Boolean;               // current mouse stroke erases (right button)
  ShowHelp: Boolean;                 // full-screen help overlay is visible
  MX, MY, NW, NH, KSel: Integer;     // scratch (mouse coords / parsed size / preset)
  // Immediate-mode widget panel (right column).
  Ui: TSedaiUI;
  UiMouseX, UiMouseY: Integer;       // last known mouse position (for hover/click)
  UiClick: Boolean;                  // left button pressed this frame (for widgets)
  UiAction, UiArg: Integer;          // deferred widget action handled after DrawEditor
  PendingFmt: Integer;               // format the pending confirm will switch to
  SavedMsg: Boolean;                 // show 'SAVED' until the next edit
  SizeFocus: Integer;                // editable size field: 0 none, 1 = W, 2 = H
  SizeWStr, SizeHStr: string;        // the W / H size fields being shown / typed
  GotoFocus: Boolean;                // the "go to sprite" field has keyboard focus
  GotoStr: string;                   // the sprite number being typed in the Go field
  PalPreset: Integer;                // selected global palette model (0..PAL_COUNT-1)
  LPalSelX, LPalSelY, LPalSelW: Integer;  // palette-model selector row geometry (pop-up)
  PalI: Integer;                     // scratch index for palette snapshot/restore
  ColorPopup: Boolean;               // modal colour-picker pop-up is open
  // Per-sprite working palette (full-color). Seeded from the engine when a sprite
  // is loaded; edited via the pop-up; pushed back to the engine on commit so each
  // sprite carries its own palette (SNES/GBA-style). EdPalCustom tracks whether it
  // differs from the global palette (only then is it stored).
  EdPal: array[0..255] of UInt32;    // ABGR8888 ($AABBGGRR)
  EdPalCustom: Boolean;
  // Snapshot of the program's GLOBAL palette on entry, restored on exit so the
  // editor's palette-model preview does not leave the program's screen recoloured.
  SavedPal: array[0..255] of UInt32;
  // Pop-up layout captured by DrawEditor, used to hit-test pop-up clicks.
  LPopX, LPopY, LPopW, LPopH: Integer;       // pop-up outer rect
  LPalRgbX, LPalRgbY, LPalRgbBtn: Integer;   // RGB stepper block origin + button size
  LPalPreX, LPalPreY, LPalPreW, LPalPreH: Integer; // preset/reset button row geometry

  function ClampByte16(V: Integer): Byte;
  begin
    Result := ((V mod 16) + 16) mod 16;
  end;

  // Filename normalized to the .spr extension (matches the engine), for FileExists.
  function NormSpr(const S: string): string;
  begin
    if LowerCase(ExtractFileExt(S)) <> '.spr' then Result := S + '.spr' else Result := S;
  end;

  // Keep the legacy Multicolor / FullColor booleans in sync with Fmt.
  procedure SyncFmtFlags;
  begin
    Multicolor := (Fmt = SPRFMT_MULTICOLOR);
    FullColor := (Fmt = SPRFMT_FULLCOLOR);
  end;

  // (Re)allocate the grid + selection buffers to the current GW x GH.
  procedure AllocBuffers;
  begin
    SetLength(Grid, GH, GW);
    SetLength(Sel, GH, GW);
    SetLength(BaseSel, GH, GW);
  end;

  // Cycle the pen colour. Full-color: move the palette pen over 0..255. Hi-res:
  // the sprite colour. Multicolor: the colour the pixel UNDER the cursor selects
  // (1=MC1, 3=MC2, else the sprite colour).
  procedure CycleColor(Delta: Integer);
  var
    V: Byte;
  begin
    if FullColor then
      PenIdx := Byte((((Integer(PenIdx) + Delta) mod 256) + 256) mod 256)
    else if not Multicolor then
      SprColIdx := ClampByte16(SprColIdx + Delta)
    else
    begin
      V := Grid[CY, CX];
      if V = 1 then MC1Idx := ClampByte16(MC1Idx + Delta)
      else if V = 3 then MC2Idx := ClampByte16(MC2Idx + Delta)
      else SprColIdx := ClampByte16(SprColIdx + Delta);
    end;
  end;

  function GridIsEmpty: Boolean;
  var
    R2, C2: Integer;
  begin
    Result := True;
    for R2 := 0 to GH - 1 do
      for C2 := 0 to GW - 1 do
        if Grid[R2, C2] <> 0 then
        begin
          Result := False;
          Exit;
        end;
  end;

  // Decode the sprite's bytes into the grid per current Fmt/size. Byte layout
  // matches the engine's EnsureSpriteTexture: stride = ceil(GW/8) bytes/row for
  // hi-res & multicolor; full-color is 1 byte/pixel (GW*GH).
  procedure UnpackFromSprite;
  var
    Row, Col, Stride, ByteIdx, BitPos, V: Integer;
    D: TBytes;
  begin
    D := FSpriteEngine.GetSpriteData(SpriteNum);
    Stride := (GW + 7) div 8;
    for Row := 0 to GH - 1 do
      for Col := 0 to GW - 1 do
      begin
        if FullColor then
        begin
          ByteIdx := Row * GW + Col;
          if ByteIdx < Length(D) then Grid[Row, Col] := D[ByteIdx]
          else Grid[Row, Col] := 0;
        end
        else if Multicolor then
        begin
          ByteIdx := Row * Stride + (Col div 8);
          BitPos := 6 - ((Col div 2) mod 4) * 2;
          if ByteIdx < Length(D) then V := (D[ByteIdx] shr BitPos) and 3 else V := 0;
          Grid[Row, Col] := V;
        end
        else
        begin
          ByteIdx := Row * Stride + (Col div 8);
          BitPos := 7 - (Col mod 8);
          if ByteIdx < Length(D) then Grid[Row, Col] := (D[ByteIdx] shr BitPos) and 1
          else Grid[Row, Col] := 0;
        end;
      end;
  end;

  procedure PackToSprite;
  var
    Row, Col, Stride, ByteIdx, BitPos, MCVal: Integer;
    D: TBytes;
    ScX, ScY: Double;
  begin
    if FullColor then
    begin
      SetLength(D, GW * GH);
      for Row := 0 to GH - 1 do
        for Col := 0 to GW - 1 do
          D[Row * GW + Col] := Grid[Row, Col];
    end
    else
    begin
      Stride := (GW + 7) div 8;
      SetLength(D, Stride * GH);
      for ByteIdx := 0 to Length(D) - 1 do D[ByteIdx] := 0;
      for Row := 0 to GH - 1 do
        for Col := 0 to GW - 1 do
        begin
          ByteIdx := Row * Stride + (Col div 8);
          if Multicolor then
          begin
            // Both halves of a pair share the value; read the even half.
            MCVal := Grid[Row, Col and (not 1)] and 3;
            BitPos := 6 - ((Col div 2) mod 4) * 2;
            D[ByteIdx] := Byte(D[ByteIdx] or (MCVal shl BitPos));
          end
          else if Grid[Row, Col] <> 0 then
          begin
            BitPos := 7 - (Col mod 8);
            D[ByteIdx] := Byte(D[ByteIdx] or (1 shl BitPos));
          end;
        end;
    end;
    FSpriteEngine.SetSpriteSize(SpriteNum, GW, GH);
    FSpriteEngine.SetSpriteFormat(SpriteNum, Fmt);
    FSpriteEngine.SetSpriteData(SpriteNum, D);

    // Apply colour / expand (keep enabled & priority unchanged: -1).
    if ExpandX then ScX := 2.0 else ScX := 1.0;
    if ExpandY then ScY := 2.0 else ScY := 1.0;
    FSpriteEngine.SetSprite(SpriteNum, -1, MakeIndexedColor(SprColIdx), -1,
      ScX, ScY, Ord(Multicolor));
    if Multicolor then
      FSpriteEngine.SetSpriteMulticolors(MakeIndexedColor(MC1Idx), MakeIndexedColor(MC2Idx));

    // Commit this sprite's per-sprite palette (full-color). If it was never edited
    // away from the global palette, keep it non-custom so it tracks SETCOLOR/PLOAD.
    if EdPalCustom then FSpriteEngine.SetSpritePaletteAll(SpriteNum, EdPal)
    else FSpriteEngine.ResetSpritePalette(SpriteNum);
  end;

  procedure SetCell(V: Byte);
  begin
    Grid[CY, CX] := V;
    if Multicolor and ((CX xor 1) < GW) then
      Grid[CY, CX xor 1] := V;   // paint both halves of the multicolor pair
  end;

  // ABGR8888 ($AABBGGRR) -> TSDL_Color, for the working full-color palette.
  function ABGRToSDL(C: UInt32): TSDL_Color;
  begin
    Result.r := C and $FF;
    Result.g := (C shr 8) and $FF;
    Result.b := (C shr 16) and $FF;
    Result.a := 255;
  end;

  // Pack R,G,B into an opaque ABGR8888 texel (alpha forced to 255).
  function RGBToABGR(Rr, Gg, Bb: Integer): UInt32;
  begin
    Result := $FF000000 or (UInt32(Bb and $FF) shl 16) or
              (UInt32(Gg and $FF) shl 8) or UInt32(Rr and $FF);
  end;

  // Set one working-palette entry and mark the palette as customised.
  procedure SetEdPalEntry(Idx: Byte; C: UInt32);
  begin
    EdPal[Idx] := C;
    EdPalCustom := True;
  end;

  // Nudge one channel (0=R,1=G,2=B) of the current pen colour by Delta, clamped.
  procedure AdjustPenChannel(Channel, Delta: Integer);
  var
    Rr, Gg, Bb: Integer;
    C: UInt32;
  begin
    C := EdPal[PenIdx];
    Rr := C and $FF; Gg := (C shr 8) and $FF; Bb := (C shr 16) and $FF;
    case Channel of
      0: Rr := Rr + Delta;
      1: Gg := Gg + Delta;
      2: Bb := Bb + Delta;
    end;
    if Rr < 0 then Rr := 0 else if Rr > 255 then Rr := 255;
    if Gg < 0 then Gg := 0 else if Gg > 255 then Gg := 255;
    if Bb < 0 then Bb := 0 else if Bb > 255 then Bb := 255;
    SetEdPalEntry(PenIdx, RGBToABGR(Rr, Gg, Bb));
  end;

  // Load a preset into the working palette. Kind: 0=C64-16 (repeated), 1=VGA-256,
  // 2=grayscale ramp, 3=reset to the program's global palette.
  procedure LoadPalettePreset(Kind: Integer);
  var
    I, Rr, Gg, Bb, H: Integer;
    SC: TSDL_Color;
  begin
    case Kind of
      0:  // C64 16-colour palette, repeated across 0..255 (entry 0 stays transparent)
        for I := 0 to 255 do
        begin
          SC := FVideoController.PaletteIndexToSDLColor(Byte(I and 15));
          EdPal[I] := RGBToABGR(SC.r, SC.g, SC.b);
        end;
      1:  // Classic VGA-style 6x6x6 colour cube + grayscale tail
        for I := 0 to 255 do
        begin
          if I < 216 then
          begin
            Rr := ((I div 36) mod 6) * 51;
            Gg := ((I div 6) mod 6) * 51;
            Bb := (I mod 6) * 51;
          end
          else
          begin
            H := ((I - 216) * 255) div 39;
            Rr := H; Gg := H; Bb := H;
          end;
          EdPal[I] := RGBToABGR(Rr, Gg, Bb);
        end;
      2:  // Linear grayscale ramp
        for I := 0 to 255 do
          EdPal[I] := RGBToABGR(I, I, I);
    end;
    if Kind = 3 then
    begin
      // Reset: re-seed from the program's global palette and drop the custom flag.
      for I := 0 to 255 do
      begin
        SC := FVideoController.PaletteIndexToSDLColor(Byte(I));
        EdPal[I] := RGBToABGR(SC.r, SC.g, SC.b);
      end;
      EdPalCustom := False;
    end
    else
      EdPalCustom := True;
  end;

  // Apply a built-in GLOBAL palette model (0..PAL_COUNT-1). This overwrites the
  // program's palette (the editor's choice wins) and persists after exit. For a
  // non-custom full-color sprite, refresh the working palette so the preview
  // follows the new model.
  procedure ApplyPalettePreset(Id: Integer);
  var PI: Integer;
  begin
    Id := ((Id mod PAL_COUNT) + PAL_COUNT) mod PAL_COUNT;   // wrap 0..PAL_COUNT-1
    PalPreset := Id;
    FVideoController.LoadPalettePreset(Id);
    if not EdPalCustom then
      for PI := 0 to 255 do
        EdPal[PI] := FSpriteEngine.GetSpritePaletteEntry(SpriteNum, Byte(PI));
  end;

  function CellColor(V: Byte): TSDL_Color;
  begin
    if FullColor then
    begin
      if V <> 0 then Result := ABGRToSDL(EdPal[V])
      else begin Result.r := 32; Result.g := 32; Result.b := 32; Result.a := 255; end;
    end
    else if Multicolor then
      case V of
        1: Result := FVideoController.PaletteIndexToSDLColor(MC1Idx);
        2: Result := FVideoController.PaletteIndexToSDLColor(SprColIdx);
        3: Result := FVideoController.PaletteIndexToSDLColor(MC2Idx);
      else
        begin Result.r := 32; Result.g := 32; Result.b := 32; Result.a := 255; end;
      end
    else if V <> 0 then
      Result := FVideoController.PaletteIndexToSDLColor(SprColIdx)
    else
      begin Result.r := 32; Result.g := 32; Result.b := 32; Result.a := 255; end;
  end;

  procedure DrawStr(X, Y: Integer; const S: string);
  var
    Surf: PSDL_Surface;
    Tex: PSDL_Texture;
    Dst: TSDL_Rect;
    White: TSDL_Color;
  begin
    if (S = '') or (FVideoController.Font = nil) then Exit;
    White.r := 220; White.g := 220; White.b := 220; White.a := 255;
    Surf := TTF_RenderUTF8_Blended(FVideoController.Font, PChar(S), White);
    if Surf = nil then Exit;
    Tex := SDL_CreateTextureFromSurface(FVideoController.Renderer, Surf);
    if Tex <> nil then
    begin
      Dst.x := X; Dst.y := Y; Dst.w := Surf^.w; Dst.h := Surf^.h;
      SDL_RenderCopy(FVideoController.Renderer, Tex, nil, @Dst);
      SDL_DestroyTexture(Tex);
    end;
    SDL_FreeSurface(Surf);
  end;

  // Draw a string left-aligned at X but vertically CENTRED on CenterY (used so
  // pop-up button captions sit in the middle of their box, not at the top).
  procedure DrawStrC(X, CenterY: Integer; const S: string);
  var
    Surf: PSDL_Surface;
    Tex: PSDL_Texture;
    Dst: TSDL_Rect;
    White: TSDL_Color;
  begin
    if (S = '') or (FVideoController.Font = nil) then Exit;
    White.r := 220; White.g := 220; White.b := 220; White.a := 255;
    Surf := TTF_RenderUTF8_Blended(FVideoController.Font, PChar(S), White);
    if Surf = nil then Exit;
    Tex := SDL_CreateTextureFromSurface(FVideoController.Renderer, Surf);
    if Tex <> nil then
    begin
      Dst.x := X; Dst.y := CenterY - Surf^.h div 2; Dst.w := Surf^.w; Dst.h := Surf^.h;
      SDL_RenderCopy(FVideoController.Renderer, Tex, nil, @Dst);
      SDL_DestroyTexture(Tex);
    end;
    SDL_FreeSurface(Surf);
  end;

  // Full-screen help overlay (toggled with H / F1). The line pitch auto-fits the
  // window height so the text never runs off the bottom of the screen.
  procedure DrawHelp;
  const
    HELP: array[0..27] of string = (
      'SPRDEF - SPRITE EDITOR HELP',
      'The right-hand panel has clickable controls; the keys below still work.',
      '',
      'Arrows        move the cursor (the view scrolls to follow on big sprites)',
      'SPACE         paint: hi-res toggle / MC cycle / full-color pen',
      '0             erase the pixel under the cursor',
      'DEL           clear the whole grid',
      'C  [  ]       step pen colour; [Pick...] pop-up: < > picks palette model',
      'SPRITE        < > / PgUp PgDn = prev/next, Home/End = first/last,',
      '              1-8 = quick-pick, Go field = jump to number (1..512)',
      'CHANGE SIZE (1..255)',
      '  presets       click a size button in the panel (24x21 .. 48x42)',
      '  custom        click a W/H field or press S, type a number, RETURN',
      'M / radios    cycle / pick format: hi-res / multicolor / full-color',
      'X   Y         toggle horizontal / vertical expand (panel checkboxes)',
      '',
      'MOUSE         L paint   R erase   MID move cursor (no paint)',
      '              drag to draw a stroke (one undo per stroke)',
      '              full-color: pop-up = THIS sprite''s palette (per-sprite);',
      'P             cycle the multicolor mouse pen',
      '',
      'Shift+Arrows  rectangular selection (+Ctrl adds an area)',
      'Ctrl+Space    toggle a pixel in the selection',
      'Ctrl+C / V    copy / paste shape',
      'Ctrl+Z / Y    undo / redo (per sprite)',
      'Ctrl+S / L    save / load all sprites to a .spr file',
      'RETURN save (commit, no exit)    ESC exit    Save commits to the sprite',
      'Press H / F1 / ESC to close this help');
  var
    R: PSDL_Renderer;
    WW, WH, HX, Y, DY, I: Integer;
  begin
    R := FVideoController.Renderer;
    if R = nil then Exit;
    SDL_GetRendererOutputSize(R, @WW, @WH);
    SDL_SetRenderDrawColor(R, 0, 0, 0, 255);
    SDL_RenderClear(R);

    HX := 48;
    DY := (WH - 64) div Length(HELP);
    if DY > 22 then DY := 22;
    if DY < 12 then DY := 12;
    Y := 32;
    for I := 0 to High(HELP) do
    begin
      if HELP[I] <> '' then DrawStr(HX, Y, HELP[I]);
      Y := Y + DY;
    end;

    SDL_RenderPresent(R);
  end;

  procedure DrawEditor;
  var
    R: PSDL_Renderer;
    WW, WH, Cell, GX, GY, Row, Col, PvX, PvY, PvS, PairLeft, CurW, LogCol: Integer;
    Ci, K, HelpY, Py, RH, RStep, RUnit: Integer;
    GVX, GVY, GVW, GVH, VisCols, VisRows, PvW, PvH: Integer;
    PopX, PopY, PopW, PopH, CurIdx: Integer;
    Ch, Bx, By, Vv, ExtraH, GridBot: Integer;
    Rect: TSDL_Rect;
    C: TSDL_Color;
    ModeStr, ChLbl: string;

    // Draw a small labelled button inside the pop-up (rendering only; clicks are
    // hit-tested separately in PopupClick).
    procedure PopBtn(Bx2, By2, Bw, Bh: Integer; const Cap: string);
    var Rc: TSDL_Rect;
    begin
      Rc.x := Bx2; Rc.y := By2; Rc.w := Bw; Rc.h := Bh;
      SDL_SetRenderDrawColor(R, 60, 60, 78, 255); SDL_RenderFillRect(R, @Rc);
      SDL_SetRenderDrawColor(R, 150, 150, 175, 255); SDL_RenderDrawRect(R, @Rc);
      DrawStrC(Bx2 + 5, By2 + Bh div 2, Cap);   // caption centred in the box
    end;
  begin
    R := FVideoController.Renderer;
    if R = nil then Exit;

    if ShowHelp then begin DrawHelp; Exit; end;

    SDL_GetRendererOutputSize(R, @WW, @WH);

    SDL_SetRenderDrawColor(R, 0, 0, 0, 255);
    SDL_RenderClear(R);

    LPalShown := False;

    // === Grid viewport (fixed region; scrolls to follow the cursor) ===========
    // A fixed right-hand column holds the command panel (so it never goes
    // off-screen); the grid lives in a viewport on the left that scrolls when the
    // sprite is bigger than fits at a usable cell size.
    PvX := WW - 280;                 // command panel left edge (fixed)
    GVX := 38; GVY := 30;
    GVW := PvX - GVX - 16;           // viewport width (gap before the panel)
    if GVW < 80 then GVW := 80;
    GVH := WH - GVY - 40;            // leave room at the bottom for the coords line
    if GVH < 80 then GVH := 80;

    // Cell size: fit the sprite, clamped to a usable range; below the minimum the
    // viewport scrolls instead of shrinking the cells to nothing.
    Cell := GVW div GW;
    if (GVH div GH) < Cell then Cell := GVH div GH;
    if Cell > 24 then Cell := 24;
    if Cell < 6 then Cell := 6;

    VisCols := GVW div Cell; if VisCols > GW then VisCols := GW; if VisCols < 1 then VisCols := 1;
    VisRows := GVH div Cell; if VisRows > GH then VisRows := GH; if VisRows < 1 then VisRows := 1;

    // Scroll so the cursor stays inside the visible window.
    if CX < LViewCol then LViewCol := CX;
    if CX > LViewCol + VisCols - 1 then LViewCol := CX - VisCols + 1;
    if CY < LViewRow then LViewRow := CY;
    if CY > LViewRow + VisRows - 1 then LViewRow := CY - VisRows + 1;
    if LViewCol > GW - VisCols then LViewCol := GW - VisCols;
    if LViewCol < 0 then LViewCol := 0;
    if LViewRow > GH - VisRows then LViewRow := GH - VisRows;
    if LViewRow < 0 then LViewRow := 0;
    if Multicolor then LViewCol := LViewCol and (not 1);

    LViewW := VisCols * Cell;
    LViewH := VisRows * Cell;
    GX := GVX + (GVW - LViewW) div 2;   // centre the visible grid in the viewport
    GY := GVY + (GVH - LViewH) div 2;
    if GX < GVX then GX := GVX;
    if GY < GVY then GY := GVY;
    LGX := GX; LGY := GY; LCell := Cell;

    // Cells (visible window only). One Cell per physical column; in multicolor the
    // two halves of a pair share the same value, so this also draws the
    // double-width look correctly.
    for Row := LViewRow to LViewRow + VisRows - 1 do
      for Col := LViewCol to LViewCol + VisCols - 1 do
      begin
        C := CellColor(Grid[Row, Col]);
        SDL_SetRenderDrawColor(R, C.r, C.g, C.b, 255);
        Rect.x := GX + (Col - LViewCol) * Cell; Rect.y := GY + (Row - LViewRow) * Cell;
        Rect.w := Cell - 1; Rect.h := Cell - 1;
        SDL_RenderFillRect(R, @Rect);
      end;

    // Rulers — always shown, labelled in multiples of 5 (0, 5, 10, 15 ...). The
    // step grows as cells shrink so the numbers stay readable on big (scrolling)
    // sprites. In multicolor the label is the logical column (0..GW/2), so the
    // physical step is doubled to keep the labels on multiples of 5.
    if Multicolor then RUnit := 10 else RUnit := 5;
    RStep := RUnit;
    while RStep * Cell < 36 do RStep := RStep + RUnit;
    Col := (LViewCol div RStep) * RStep;
    if Col < LViewCol then Col := Col + RStep;
    while Col <= LViewCol + VisCols - 1 do
    begin
      if Multicolor then DrawStr(GX + (Col - LViewCol) * Cell + 1, GY - 20, IntToStr(Col div 2))
      else DrawStr(GX + (Col - LViewCol) * Cell + 1, GY - 20, IntToStr(Col));
      Col := Col + RStep;
    end;
    RStep := 5;
    while RStep * Cell < 36 do RStep := RStep + 5;
    Row := (LViewRow div RStep) * RStep;
    if Row < LViewRow then Row := Row + RStep;
    while Row <= LViewRow + VisRows - 1 do
    begin
      DrawStr(GX - 26, GY + (Row - LViewRow) * Cell - 2, IntToStr(Row));
      Row := Row + RStep;
    end;

    // Selection overlay (visible cells).
    SDL_SetRenderDrawColor(R, 0, 255, 255, 255);
    for Row := LViewRow to LViewRow + VisRows - 1 do
      for Col := LViewCol to LViewCol + VisCols - 1 do
        if Sel[Row, Col] then
        begin
          Rect.x := GX + (Col - LViewCol) * Cell; Rect.y := GY + (Row - LViewRow) * Cell;
          Rect.w := Cell - 1; Rect.h := Cell - 1;
          SDL_RenderDrawRect(R, @Rect);
        end;

    // Cursor (spans the pair in multicolor) — only if it is inside the window.
    if Multicolor then begin PairLeft := CX and (not 1); CurW := 2 * Cell; end
    else begin PairLeft := CX; CurW := Cell; end;
    if (PairLeft >= LViewCol) and (PairLeft < LViewCol + VisCols) and
       (CY >= LViewRow) and (CY < LViewRow + VisRows) then
    begin
      Rect.x := GX + (PairLeft - LViewCol) * Cell; Rect.y := GY + (CY - LViewRow) * Cell;
      Rect.w := CurW; Rect.h := Cell;
      SDL_SetRenderDrawColor(R, 255, 255, 255, 255);   // white outer ring
      SDL_RenderDrawRect(R, @Rect);
      Rect.x := Rect.x + 1; Rect.y := Rect.y + 1;
      Rect.w := Rect.w - 2; Rect.h := Rect.h - 2;
      if (Rect.w > 0) and (Rect.h > 0) then
      begin
        SDL_SetRenderDrawColor(R, 0, 0, 0, 255);       // black inner ring
        SDL_RenderDrawRect(R, @Rect);
      end;
    end;

    if Multicolor then LogCol := CX div 2 else LogCol := CX;
    HelpY := GVY + GVH;

    // Line below the grid: filename / size input field, else live cursor coords.
    if OverwritePending then
      DrawStr(GX, HelpY + 6, 'OVERWRITE ' + NormSpr(InputText) + ' ?  Y/N')
    else if InputMode = 1 then
      DrawStr(GX, HelpY + 6, 'SAVE FILE: ' + InputText + '_')
    else if InputMode = 2 then
      DrawStr(GX, HelpY + 6, 'LOAD FILE: ' + InputText + '_')
    else if InputMode = 4 then
      DrawStr(GX, HelpY + 6, 'SIZE WxH: ' + InputText + '_   (e.g. 32x32)')
    else if LastFile <> '' then
      DrawStr(GX, HelpY + 6, Format('COL %d   ROW %d    FILE: %s', [LogCol, CY, LastFile]))
    else
      DrawStr(GX, HelpY + 6, Format('COL %d   ROW %d', [LogCol, CY]));

    // Format-change confirmation prompt (the size menu lives in the right column).
    if ConfirmSwitch then
      DrawStr(GX, HelpY + 26, 'CHANGE FORMAT? THIS CLEARS THE SPRITE - Y/N');

    // Shape preview in the panel column: a fixed thumbnail (<= 80px) that scales
    // small sprites up and SUB-SAMPLES big ones, so its height never pushes the
    // command panel off the bottom of the screen.
    PvY := GVY;
    if (GW <= 80) and (GH <= 80) then
    begin
      PvS := 80 div GW; if (80 div GH) < PvS then PvS := 80 div GH;
      if PvS < 1 then PvS := 1;
      if PvS > 4 then PvS := 4;
      for Row := 0 to GH - 1 do
        for Col := 0 to GW - 1 do
          if Grid[Row, Col] <> 0 then
          begin
            C := CellColor(Grid[Row, Col]);
            SDL_SetRenderDrawColor(R, C.r, C.g, C.b, 255);
            Rect.x := PvX + Col * PvS; Rect.y := PvY + Row * PvS;
            Rect.w := PvS; Rect.h := PvS;
            SDL_RenderFillRect(R, @Rect);
          end;
      PvW := GW * PvS; PvH := GH * PvS;
    end
    else
    begin
      PvW := 80; PvH := 80;
      for Row := 0 to 79 do
        for Col := 0 to 79 do
          if Grid[(Row * GH) div 80, (Col * GW) div 80] <> 0 then
          begin
            C := CellColor(Grid[(Row * GH) div 80, (Col * GW) div 80]);
            SDL_SetRenderDrawColor(R, C.r, C.g, C.b, 255);
            Rect.x := PvX + Col; Rect.y := PvY + Row;
            Rect.w := 1; Rect.h := 1;
            SDL_RenderFillRect(R, @Rect);
          end;
    end;

    // Status header
    if FullColor then ModeStr := 'FULL-COLOR'
    else if Multicolor then ModeStr := 'MULTICOLOR'
    else ModeStr := 'HI-RES';

    // === Immediate-mode widget panel (right column) ===========================
    // Uniform layout: Py is the CENTRE Y of the current row, RH the row pitch; a
    // fixed 8px gap separates groups. Deferred actions (resize/format) go through
    // UiAction. The keyboard shortcuts keep working in parallel. While the colour
    // pop-up is open the panel must NOT consume the click (mask UiClick).
    RH := Ui.RowHeight;
    Py := PvY + PvH + 16 + RH div 2;
    Ui.BeginFrame(UiMouseX, UiMouseY, UiClick and (not ColorPopup));
    UiAction := uaNone;

    // Header: name on the left, HELP at the right edge (no overlap with the mode).
    Ui.Caption(PvX, Py, Format('SPRDEF  sprite %d', [SpriteNum]));
    if Ui.Button(PvX + 208, Py - 11, 52, 22, 'HELP') then ShowHelp := True;
    Py := Py + RH;
    if SavedMsg then
      Ui.Caption(PvX, Py, Format('%s   %dx%d   cursor %d,%d   *SAVED*', [ModeStr, GW, GH, LogCol, CY]))
    else
      Ui.Caption(PvX, Py, Format('%s   %dx%d   cursor %d,%d', [ModeStr, GW, GH, LogCol, CY]));
    Py := Py + RH + 8;

    // Sprite selector: [<] N/MAX [>]  Go [____]  (also PgUp/PgDn, Home/End keys).
    Ui.Caption(PvX, Py, 'Sprite');
    if Ui.Button(PvX + 72, Py - 11, 26, 22, '<') then
      begin UiAction := uaGoto; UiArg := SpriteNum - 1; end;
    Ui.CaptionC(PvX + 100, Py, 56, Format('%d/%d', [SpriteNum, MAX_SPRITES]));
    if Ui.Button(PvX + 158, Py - 11, 26, 22, '>') then
      begin UiAction := uaGoto; UiArg := SpriteNum + 1; end;
    Ui.Caption(PvX + 190, Py, 'Go');
    if not GotoFocus then GotoStr := '';   // the jump field is blank unless focused
    if Ui.Field(PvX + 212, Py - 11, 46, 22, GotoStr, GotoFocus, False) then
      begin GotoFocus := True; SizeFocus := 0; GotoStr := ''; end;
    Py := Py + RH + 8;

    // Format (real radio buttons)
    Ui.Caption(PvX, Py, 'Format');
    if Ui.Radio(PvX + 72, Py, 'Hi-res', Fmt = SPRFMT_HIRES) and (Fmt <> SPRFMT_HIRES) then
      begin PendingFmt := SPRFMT_HIRES; ConfirmSwitch := True; end;
    if Ui.Radio(PvX + 162, Py, 'Multicolor', Fmt = SPRFMT_MULTICOLOR) and (Fmt <> SPRFMT_MULTICOLOR) then
      begin PendingFmt := SPRFMT_MULTICOLOR; ConfirmSwitch := True; end;
    Py := Py + RH;
    if Ui.Radio(PvX + 72, Py, 'Full-color (256)', Fmt = SPRFMT_FULLCOLOR) and (Fmt <> SPRFMT_FULLCOLOR) then
      begin PendingFmt := SPRFMT_FULLCOLOR; ConfirmSwitch := True; end;
    Py := Py + RH + 8;

    // Expand
    Ui.Caption(PvX, Py, 'Expand');
    Ui.Checkbox(PvX + 72, Py, 'X', ExpandX);
    Ui.Checkbox(PvX + 122, Py, 'Y', ExpandY);
    Py := Py + RH + 8;

    // Pen: [<] number [>] swatch [Pick...]
    if FullColor then CurIdx := PenIdx else CurIdx := SprColIdx;
    Ui.Caption(PvX, Py, 'Pen');
    if Ui.Button(PvX + 72, Py - 11, 26, 22, '<') then CycleColor(-1);
    Ui.CaptionC(PvX + 100, Py, 40, IntToStr(CurIdx));
    if Ui.Button(PvX + 142, Py - 11, 26, 22, '>') then CycleColor(1);
    if FullColor then C := ABGRToSDL(EdPal[CurIdx])
    else C := FVideoController.PaletteIndexToSDLColor(CurIdx);
    LSwatchX := PvX + 174; LSwatchY := Py - 10;   // remember: the pop-up anchors here
    Rect.x := LSwatchX; Rect.y := LSwatchY; Rect.w := 20; Rect.h := 20;
    SDL_SetRenderDrawColor(R, C.r, C.g, C.b, 255); SDL_RenderFillRect(R, @Rect);
    SDL_SetRenderDrawColor(R, 200, 200, 200, 255); SDL_RenderDrawRect(R, @Rect);
    if Ui.Button(PvX + 202, Py - 11, 58, 22, 'Pick...') then ColorPopup := True;
    Py := Py + RH + 8;

    // Size: W/H integer fields (right-aligned, 1..255) + Set
    if SizeFocus = 0 then begin SizeWStr := IntToStr(GW); SizeHStr := IntToStr(GH); end;
    Ui.Caption(PvX, Py, 'Size');
    Ui.Caption(PvX + 50, Py, 'W');
    if Ui.Field(PvX + 66, Py - 11, 44, 22, SizeWStr, SizeFocus = 1, True) then begin SizeFocus := 1; GotoFocus := False; end;
    Ui.Caption(PvX + 120, Py, 'H');
    if Ui.Field(PvX + 136, Py - 11, 44, 22, SizeHStr, SizeFocus = 2, True) then begin SizeFocus := 2; GotoFocus := False; end;
    if Ui.Button(PvX + 188, Py - 11, 50, 22, 'Set') then UiAction := uaSizeApply;
    Py := Py + RH;

    // Preset buttons: 4 per row, pitch 66, width 62 -> the row spans 3*66+62 = 260px.
    // The button pairs below match that span (126 + 8 gap + 126).
    for K := 1 to PRESET_COUNT do
      if Ui.Button(PvX + ((K - 1) mod 4) * 66, Py - 10 + ((K - 1) div 4) * 24, 62, 20,
                   Format('%dx%d', [PresetW[K], PresetH[K]])) then
        begin UiAction := uaPreset; UiArg := K; end;
    Py := Py + 24 + RH + 8;

    // Save / Exit
    if Ui.Button(PvX, Py - 12, 126, 24, 'Save (Return)') then begin PackToSprite; SavedMsg := True; end;
    if Ui.Button(PvX + 134, Py - 12, 126, 24, 'Exit (Esc)') then Running := False;
    Py := Py + RH + 8;
    if Ui.Button(PvX, Py - 11, 126, 22, 'Save file...') then
      begin InputMode := 1; InputText := LastFile; OverwritePending := False; end;
    if Ui.Button(PvX + 134, Py - 11, 126, 22, 'Load file...') then
      begin InputMode := 2; InputText := LastFile; OverwritePending := False; end;
    Py := Py + RH + 8;

    // Inline confirm for a format change (also answerable with Y/N on the keyboard)
    if ConfirmSwitch then
    begin
      Ui.Caption(PvX, Py, 'Clear sprite & change format?');
      Py := Py + RH;
      if Ui.Button(PvX, Py - 11, 64, 22, 'Yes') then UiAction := uaFmtApply;
      if Ui.Button(PvX + 72, Py - 11, 64, 22, 'No') then UiAction := uaFmtCancel;
      Py := Py + RH;
    end;

    Ui.Caption(PvX, Py, 'H / F1 = help   (keys still work)');

    Ui.EndFrame;

    // ---- Colour pop-up (modal): 16x16 palette swatches, anchored beside the pen
    // swatch. For full-color sprites the pop-up shows THIS sprite's palette (EdPal)
    // and adds an RGB editor + preset buttons so each sprite owns its colours.
    // Hi-res / multicolor sprites use the global palette, so the picker just shows
    // it and picks the sprite colour index.
    LPalShown := False;
    if ColorPopup then
    begin
      LPalCell := 18;
      // Extra panel height (RGB steppers + preset row + close) only in full-color.
      if FullColor then ExtraH := 3 * 26 + 8 + 26 + 8 + 26 + 8 else ExtraH := 0;
      ExtraH := ExtraH + 28;            // top row: global palette-model selector
      PopW := 16 * LPalCell + 16;       // 8px uniform margin on each side
      PopH := 16 * LPalCell + 16 + ExtraH;
      // Anchor to the left of the pen swatch; clamp inside the screen.
      PopX := LSwatchX - PopW - 8;
      if PopX < 6 then PopX := LSwatchX + 28;
      PopY := LSwatchY - 8;
      if PopX + PopW > WW - 6 then PopX := WW - 6 - PopW;
      if PopY + PopH > WH - 6 then PopY := WH - 6 - PopH;
      if PopX < 6 then PopX := 6;
      if PopY < 6 then PopY := 6;
      LPopX := PopX; LPopY := PopY; LPopW := PopW; LPopH := PopH;
      Rect.x := PopX; Rect.y := PopY; Rect.w := PopW; Rect.h := PopH;
      SDL_SetRenderDrawColor(R, 20, 20, 28, 255); SDL_RenderFillRect(R, @Rect);
      SDL_SetRenderDrawColor(R, 180, 180, 210, 255); SDL_RenderDrawRect(R, @Rect);

      // Top row: global palette-model selector  [<]  name  [>]
      LPalSelX := PopX + 8; LPalSelY := PopY + 8; LPalSelW := 24;
      PopBtn(LPalSelX, LPalSelY, LPalSelW, 22, '<');
      PopBtn(PopX + PopW - 32, LPalSelY, LPalSelW, 22, '>');
      DrawStrC(PopX + 40, LPalSelY + 11, 'Palette: ' + PalNm[PalPreset]);

      LPalX := PopX + 8;
      LPalY := PopY + 8 + 28;           // grid sits below the selector row
      LPalShown := True;
      for Ci := 0 to 255 do
      begin
        if FullColor then C := ABGRToSDL(EdPal[Ci])
        else C := FVideoController.PaletteIndexToSDLColor(Ci);
        SDL_SetRenderDrawColor(R, C.r, C.g, C.b, 255);
        Rect.x := LPalX + (Ci mod 16) * LPalCell;
        Rect.y := LPalY + (Ci div 16) * LPalCell;
        Rect.w := LPalCell - 1; Rect.h := LPalCell - 1;
        SDL_RenderFillRect(R, @Rect);
      end;
      // Evident selection marker: a 2px white frame with a black inner edge so it
      // stands out against any swatch colour.
      if FullColor then CurIdx := PenIdx else CurIdx := SprColIdx;
      Bx := LPalX + (CurIdx mod 16) * LPalCell;
      By := LPalY + (CurIdx div 16) * LPalCell;
      SDL_SetRenderDrawColor(R, 0, 0, 0, 255);
      Rect.x := Bx - 1; Rect.y := By - 1; Rect.w := LPalCell + 1; Rect.h := LPalCell + 1;
      SDL_RenderDrawRect(R, @Rect);
      SDL_SetRenderDrawColor(R, 255, 255, 255, 255);
      Rect.x := Bx - 2; Rect.y := By - 2; Rect.w := LPalCell + 3; Rect.h := LPalCell + 3;
      SDL_RenderDrawRect(R, @Rect);
      Rect.x := Bx - 3; Rect.y := By - 3; Rect.w := LPalCell + 5; Rect.h := LPalCell + 5;
      SDL_RenderDrawRect(R, @Rect);

      // Full-color: RGB editor for the current pen entry + palette presets.
      if FullColor then
      begin
        GridBot := LPalY + 16 * LPalCell;
        LPalRgbBtn := 22;
        LPalRgbX := PopX + 8;
        LPalRgbY := GridBot + 8;
        // Current entry preview swatch (right of the RGB rows).
        C := ABGRToSDL(EdPal[PenIdx]);
        Rect.x := LPalRgbX + 168; Rect.y := LPalRgbY; Rect.w := 3 * 26 + 18; Rect.h := 3 * 26 - 4;
        SDL_SetRenderDrawColor(R, C.r, C.g, C.b, 255); SDL_RenderFillRect(R, @Rect);
        SDL_SetRenderDrawColor(R, 180, 180, 200, 255); SDL_RenderDrawRect(R, @Rect);
        for Ch := 0 to 2 do
        begin
          By := LPalRgbY + Ch * 26;
          case Ch of
            0: Vv := EdPal[PenIdx] and $FF;
            1: Vv := (EdPal[PenIdx] shr 8) and $FF;
          else Vv := (EdPal[PenIdx] shr 16) and $FF;
          end;
          if Ch = 0 then ChLbl := 'R' else if Ch = 1 then ChLbl := 'G' else ChLbl := 'B';
          DrawStrC(LPalRgbX, By + LPalRgbBtn div 2, ChLbl);
          PopBtn(LPalRgbX + 18, By, LPalRgbBtn, LPalRgbBtn, '-');
          DrawStrC(LPalRgbX + 48, By + LPalRgbBtn div 2, Format('%3d', [Vv]));
          PopBtn(LPalRgbX + 96, By, LPalRgbBtn, LPalRgbBtn, '+');
        end;
        // Preset buttons: C64 / VGA / Gray / Reset.
        LPalPreX := PopX + 8; LPalPreY := GridBot + 8 + 3 * 26 + 8;
        LPalPreW := 66; LPalPreH := 22;
        PopBtn(LPalPreX,             LPalPreY, LPalPreW, LPalPreH, 'C64');
        PopBtn(LPalPreX + 70,        LPalPreY, LPalPreW, LPalPreH, 'VGA');
        PopBtn(LPalPreX + 140,       LPalPreY, LPalPreW, LPalPreH, 'Gray');
        PopBtn(LPalPreX + 210,       LPalPreY, LPalPreW, LPalPreH, 'Reset');
        // Close button (full-width-ish) on the last row.
        PopBtn(PopX + 8, LPalPreY + 26, PopW - 16, 22, 'Close');
      end;
    end;

    SDL_RenderPresent(R);
  end;

  procedure ClearGrid;
  var Row, Col: Integer;
  begin
    for Row := 0 to GH - 1 do
      for Col := 0 to GW - 1 do
        Grid[Row, Col] := 0;
  end;

  procedure ClearSelection;
  var Row, Col: Integer;
  begin
    HasAnchor := False;
    for Row := 0 to GH - 1 do
      for Col := 0 to GW - 1 do
        Sel[Row, Col] := False;
  end;

  function SelEmpty: Boolean;
  var Row, Col: Integer;
  begin
    Result := True;
    for Row := 0 to GH - 1 do
      for Col := 0 to GW - 1 do
        if Sel[Row, Col] then begin Result := False; Exit; end;
  end;

  // Begin a rectangular drag at the cursor. AddMode keeps the existing selection
  // (the new rectangle is added to it); otherwise the selection is replaced.
  procedure StartDrag(AddMode: Boolean);
  var Row, Col: Integer;
  begin
    SelAnchorX := CX; SelAnchorY := CY;
    for Row := 0 to GH - 1 do
      for Col := 0 to GW - 1 do
        if AddMode then BaseSel[Row, Col] := Sel[Row, Col] else BaseSel[Row, Col] := False;
    HasAnchor := True;
  end;

  // Selection = base mask OR the rectangle from the anchor to the cursor.
  procedure ApplyDragRect;
  var Row, Col, X1, Y1, X2, Y2: Integer;
  begin
    if SelAnchorX < CX then begin X1 := SelAnchorX; X2 := CX; end else begin X1 := CX; X2 := SelAnchorX; end;
    if SelAnchorY < CY then begin Y1 := SelAnchorY; Y2 := CY; end else begin Y1 := CY; Y2 := SelAnchorY; end;
    for Row := 0 to GH - 1 do
      for Col := 0 to GW - 1 do
        Sel[Row, Col] := BaseSel[Row, Col] or
          ((Col >= X1) and (Col <= X2) and (Row >= Y1) and (Row <= Y2));
  end;

  // Copy the selected region (or the whole shape if nothing is selected) to the
  // clipboard, keeping a mask so a non-rectangular selection pastes correctly.
  procedure CopyShape;
  var Row, Col, X1, Y1, X2, Y2: Integer;
  begin
    SetLength(Clip, GH, GW);
    SetLength(ClipSel, GH, GW);
    if SelEmpty then
    begin
      ClipW := GW; ClipH := GH;
      for Row := 0 to GH - 1 do
        for Col := 0 to GW - 1 do
        begin Clip[Row, Col] := Grid[Row, Col]; ClipSel[Row, Col] := True; end;
    end
    else
    begin
      X1 := GW; Y1 := GH; X2 := -1; Y2 := -1;
      for Row := 0 to GH - 1 do
        for Col := 0 to GW - 1 do
          if Sel[Row, Col] then
          begin
            if Col < X1 then X1 := Col;
            if Col > X2 then X2 := Col;
            if Row < Y1 then Y1 := Row;
            if Row > Y2 then Y2 := Row;
          end;
      ClipW := X2 - X1 + 1; ClipH := Y2 - Y1 + 1;
      for Row := 0 to ClipH - 1 do
        for Col := 0 to ClipW - 1 do
        begin
          Clip[Row, Col] := Grid[Y1 + Row, X1 + Col];
          ClipSel[Row, Col] := Sel[Y1 + Row, X1 + Col];
        end;
    end;
    ClipValid := True;
  end;

  // Paste the clipboard (only the masked cells). A whole-sprite copy pastes at the
  // top-left (replace the sprite); a smaller region pastes at the cursor.
  procedure PasteShape;
  var Row, Col, OX, OY: Integer;
  begin
    if not ClipValid then Exit;
    if (ClipW = GW) and (ClipH = GH) then
    begin OX := 0; OY := 0; end
    else
    begin OX := CX; OY := CY; end;
    for Row := 0 to ClipH - 1 do
      for Col := 0 to ClipW - 1 do
        if ClipSel[Row, Col] and (OY + Row <= GH - 1) and (OX + Col <= GW - 1) then
          Grid[OY + Row, OX + Col] := Clip[Row, Col];
  end;

  // Flatten the current grid to a row-major byte buffer (GW*GH).
  function GridToBytes: TBytes;
  var Row, Col, I: Integer;
  begin
    SetLength(Result, GW * GH);
    I := 0;
    for Row := 0 to GH - 1 do
      for Col := 0 to GW - 1 do
      begin Result[I] := Grid[Row, Col]; Inc(I); end;
  end;

  // Snapshot the grid together with its current size and format.
  function MakeSnap: TGridSnap;
  begin
    Result.W := GW; Result.H := GH; Result.Fmt := Fmt;
    Result.Data := GridToBytes;
  end;

  // Restore a snapshot: re-size the buffers and decode the flattened cells. Size
  // and format can differ from the current ones (resize/format change are undoable).
  procedure ApplySnap(const S: TGridSnap);
  var Row, Col, I: Integer;
  begin
    GW := S.W; GH := S.H; Fmt := S.Fmt;
    SyncFmtFlags;
    AllocBuffers;
    I := 0;
    for Row := 0 to GH - 1 do
      for Col := 0 to GW - 1 do
      begin
        if I < Length(S.Data) then Grid[Row, Col] := S.Data[I] else Grid[Row, Col] := 0;
        Inc(I);
      end;
    if CX >= GW then CX := GW - 1;
    if CY >= GH then CY := GH - 1;
    if CX < 0 then CX := 0;
    if CY < 0 then CY := 0;
  end;

  // Per-sprite undo/redo. PushUndo is called before each grid-changing action.
  procedure PushUndo;
  var N, I: Integer;
  begin
    SavedMsg := False;   // any edit invalidates the 'SAVED' indicator
    N := Length(Undo[SpriteNum]);
    if N >= MAXUNDO then
    begin
      // Drop the oldest entry (records hold a managed TBytes, so shift by
      // assignment, not Move, to keep reference counts correct).
      for I := 0 to N - 2 do Undo[SpriteNum][I] := Undo[SpriteNum][I + 1];
      SetLength(Undo[SpriteNum], N - 1);
      N := N - 1;
    end;
    SetLength(Undo[SpriteNum], N + 1);
    Undo[SpriteNum][N] := MakeSnap;
    SetLength(Redo[SpriteNum], 0);   // a new edit invalidates the redo stack
  end;

  procedure DoUndo;
  var N: Integer;
  begin
    N := Length(Undo[SpriteNum]);
    if N = 0 then Exit;
    SetLength(Redo[SpriteNum], Length(Redo[SpriteNum]) + 1);
    Redo[SpriteNum][High(Redo[SpriteNum])] := MakeSnap;
    ApplySnap(Undo[SpriteNum][N - 1]);
    SetLength(Undo[SpriteNum], N - 1);
    ClearSelection;
  end;

  procedure DoRedo;
  var N: Integer;
  begin
    N := Length(Redo[SpriteNum]);
    if N = 0 then Exit;
    SetLength(Undo[SpriteNum], Length(Undo[SpriteNum]) + 1);
    Undo[SpriteNum][High(Undo[SpriteNum])] := MakeSnap;
    ApplySnap(Redo[SpriteNum][N - 1]);
    SetLength(Redo[SpriteNum], N - 1);
    ClearSelection;
  end;

  procedure ClearAllUndo;
  var K: Integer;
  begin
    for K := 1 to MAX_SPRITES do
    begin
      SetLength(Undo[K], 0);
      SetLength(Redo[K], 0);
    end;
  end;

  procedure NormalizePairs;
  var Row, Col: Integer;
  begin
    // After switching to multicolor make both halves of each pair equal.
    for Row := 0 to GH - 1 do
      for Col := 0 to GW - 1 do
        if (Col and 1) = 1 then
          Grid[Row, Col] := Grid[Row, Col - 1];
  end;

  // Load the editing state for the current SpriteNum (also used when switching
  // sprites with the 1-8 keys). Resets the cursor and unpacks the shape.
  procedure LoadSpriteState;
  var
    PI: Integer;
  begin
    Info := FSpriteEngine.GetSpriteInfo(SpriteNum);
    Fmt := Info.Format;
    if (Fmt < SPRFMT_HIRES) or (Fmt > SPRFMT_FULLCOLOR) then Fmt := SPRFMT_HIRES;
    // Legacy sprites may carry MulticolorMode while Format is still 0.
    if (Fmt = SPRFMT_HIRES) and Info.MulticolorMode then Fmt := SPRFMT_MULTICOLOR;
    SyncFmtFlags;
    GW := Info.Width; GH := Info.Height;
    if GW < 1 then GW := DEFAULT_SPRITE_WIDTH;
    if GH < 1 then GH := DEFAULT_SPRITE_HEIGHT;
    if GW > MAX_DIM then GW := MAX_DIM;
    if GH > MAX_DIM then GH := MAX_DIM;
    AllocBuffers;
    ExpandX := Info.ScaleX >= 1.5;
    ExpandY := Info.ScaleY >= 1.5;
    if Info.Color.Mode = scmIndexed then SprColIdx := Info.Color.Index else SprColIdx := 1;
    MC1Idx := 0; MC2Idx := 0;
    if FSpriteEngine.GetMulticolor(RSPCOLOR_MC1).Mode = scmIndexed then
      MC1Idx := FSpriteEngine.GetMulticolor(RSPCOLOR_MC1).Index;
    if FSpriteEngine.GetMulticolor(RSPCOLOR_MC2).Mode = scmIndexed then
      MC2Idx := FSpriteEngine.GetMulticolor(RSPCOLOR_MC2).Index;
    if SprColIdx = 0 then SprColIdx := 1;        // a visible default
    if MC1Idx = 0 then MC1Idx := 10;
    if MC2Idx = 0 then MC2Idx := 12;
    // Seed the working full-color palette from the engine's effective palette for
    // this sprite (its own if custom, otherwise the global one).
    EdPalCustom := FSpriteEngine.IsSpritePaletteCustom(SpriteNum);
    for PI := 0 to 255 do
      EdPal[PI] := FSpriteEngine.GetSpritePaletteEntry(SpriteNum, Byte(PI));
    CX := 0; CY := 0;
    UnpackFromSprite;
    ClearSelection;   // a different sprite: drop any active selection
    // Empty AND colourless sprite: start with a white pen. Do NOT override a
    // colour the sprite already carries (that would whiten a green sprite, etc.).
    if GridIsEmpty and (SprColIdx = 0) then SprColIdx := 1;
  end;

  // Switch the editor to sprite N (1..MAX_SPRITES): commit the current sprite,
  // then load the new one. Shared by the < / > buttons, the Go field, the page /
  // home / end keys and the 1-8 quick-select keys.
  procedure GoToSprite(N: Integer);
  begin
    if N < 1 then N := 1;
    if N > MAX_SPRITES then N := MAX_SPRITES;
    if N = SpriteNum then Exit;
    PackToSprite;        // save the sprite we are leaving
    SpriteNum := N;
    LoadSpriteState;     // load the chosen sprite
  end;

  // Resize the editing grid to NewW x NewH, preserving the overlapping top-left
  // region. Used by the size presets and the free-size entry.
  procedure ResizeGrid(NewW, NewH: Integer);
  var Old: TBytes; OldW, OldH, Row, Col: Integer;
  begin
    if NewW < 1 then NewW := 1; if NewW > MAX_DIM then NewW := MAX_DIM;
    if NewH < 1 then NewH := 1; if NewH > MAX_DIM then NewH := MAX_DIM;
    if (NewW = GW) and (NewH = GH) then Exit;
    PushUndo;                       // the resize itself is undoable
    Old := GridToBytes; OldW := GW; OldH := GH;
    GW := NewW; GH := NewH;
    AllocBuffers;
    for Row := 0 to GH - 1 do
      for Col := 0 to GW - 1 do
        if (Row < OldH) and (Col < OldW) then Grid[Row, Col] := Old[Row * OldW + Col]
        else Grid[Row, Col] := 0;
    if CX >= GW then CX := GW - 1;
    if CY >= GH then CY := GH - 1;
    ClearSelection;
  end;

  // Paint the cell currently under the cursor with the active pen (Erase=True
  // clears it to transparent). Shared by the keyboard and the mouse.
  procedure PaintCurrent(Erase: Boolean);
  begin
    if Erase then begin SetCell(0); Exit; end;
    if FullColor then SetCell(PenIdx)
    else if Multicolor then SetCell(MulPen)
    else SetCell(1);
  end;

  // Map a mouse pixel position to a grid cell, honouring the scroll offset.
  // Returns False if outside the visible grid viewport.
  function MouseToCell(MX, MY: Integer; out Cx2, Cy2: Integer): Boolean;
  begin
    Result := False;
    if LCell <= 0 then Exit;
    if (MX < LGX) or (MY < LGY) or (MX >= LGX + LViewW) or (MY >= LGY + LViewH) then Exit;
    Cx2 := (MX - LGX) div LCell + LViewCol;
    Cy2 := (MY - LGY) div LCell + LViewRow;
    if (Cx2 < 0) or (Cx2 >= GW) or (Cy2 < 0) or (Cy2 >= GH) then Exit;
    Result := True;
  end;

  // If the mouse is over a palette swatch, set the active pen colour (the
  // full-color pen index, or the sprite colour for hi-res/multicolor) and return
  // True. Used by the colour pop-up.
  function MouseToPalette(MX, MY: Integer): Boolean;
  var Col, Row: Integer;
  begin
    Result := False;
    if not LPalShown or (LPalCell <= 0) then Exit;
    if (MX < LPalX) or (MY < LPalY) then Exit;
    Col := (MX - LPalX) div LPalCell;
    Row := (MY - LPalY) div LPalCell;
    if (Col < 0) or (Col > 15) or (Row < 0) or (Row > 15) then Exit;
    if FullColor then PenIdx := Byte(Row * 16 + Col)
    else SprColIdx := Byte(Row * 16 + Col);
    Result := True;
  end;

  // Handle a left click inside the colour pop-up. Returns True if the click landed
  // inside the pop-up (it stays open), False if it fell outside (caller closes it).
  // Routes the click to the swatch grid, the RGB steppers, the preset buttons or
  // the Close button.
  function PopupClick(MX, MY: Integer): Boolean;
  var Cc, By2: Integer;
  begin
    Result := True;
    if (MX < LPopX) or (MY < LPopY) or (MX >= LPopX + LPopW) or (MY >= LPopY + LPopH) then
    begin Result := False; Exit; end;
    // Palette-model selector row (top): [<] cycles back, [>] forward (both modes).
    if (MY >= LPalSelY) and (MY < LPalSelY + 22) then
    begin
      if (MX >= LPalSelX) and (MX < LPalSelX + LPalSelW) then
        begin ApplyPalettePreset(PalPreset - 1); Exit; end;
      if (MX >= LPopX + LPopW - 32) and (MX < LPopX + LPopW - 8) then
        begin ApplyPalettePreset(PalPreset + 1); Exit; end;
    end;
    if MouseToPalette(MX, MY) then Exit;     // picked a swatch
    if not FullColor then Exit;              // hi-res / multicolor: swatch grid only
    // RGB steppers: [-] at +18, [+] at +96, each LPalRgbBtn wide.
    for Cc := 0 to 2 do
    begin
      By2 := LPalRgbY + Cc * 26;
      if (MY >= By2) and (MY < By2 + LPalRgbBtn) then
      begin
        if (MX >= LPalRgbX + 18) and (MX < LPalRgbX + 18 + LPalRgbBtn) then
          begin AdjustPenChannel(Cc, -16); Exit; end;
        if (MX >= LPalRgbX + 96) and (MX < LPalRgbX + 96 + LPalRgbBtn) then
          begin AdjustPenChannel(Cc, 16); Exit; end;
      end;
    end;
    // Preset buttons row.
    if (MY >= LPalPreY) and (MY < LPalPreY + LPalPreH) then
    begin
      if (MX >= LPalPreX)       and (MX < LPalPreX + LPalPreW)       then begin LoadPalettePreset(0); Exit; end;
      if (MX >= LPalPreX + 70)  and (MX < LPalPreX + 70 + LPalPreW)  then begin LoadPalettePreset(1); Exit; end;
      if (MX >= LPalPreX + 140) and (MX < LPalPreX + 140 + LPalPreW) then begin LoadPalettePreset(2); Exit; end;
      if (MX >= LPalPreX + 210) and (MX < LPalPreX + 210 + LPalPreW) then begin LoadPalettePreset(3); Exit; end;
    end;
    // Close button (row below the presets).
    if (MY >= LPalPreY + 26) and (MY < LPalPreY + 26 + 22) then
      ColorPopup := False;
  end;

  // Parse a free size entry like "32x32", "40,32" or "16 24" into NewW/NewH.
  function ParseSize(const S: string; out NewW, NewH: Integer): Boolean;
  var I, P: Integer;
  begin
    Result := False;
    P := 0;
    for I := 1 to Length(S) do
      if S[I] in ['x', 'X', ',', ' ', '*'] then begin P := I; Break; end;
    if P = 0 then Exit;
    NewW := StrToIntDef(Trim(Copy(S, 1, P - 1)), 0);
    NewH := StrToIntDef(Trim(Copy(S, P + 1, Length(S) - P)), 0);
    Result := (NewW > 0) and (NewH > 0);
  end;

begin
  Result := False;
  if (SpriteNum < 1) or (SpriteNum > MAX_SPRITES) then SpriteNum := 1;
  PenIdx := 1;     // visible full-color default (palette index 1)
  MulPen := 2;     // multicolor mouse pen = sprite colour
  CX := 0; CY := 0;

  LoadSpriteState;

  Running := True;
  ConfirmSwitch := False;
  ClipValid := False;
  ClipW := 0; ClipH := 0;
  ClearSelection;
  InputMode := 0;
  InputText := '';
  LastFile := '';
  OverwritePending := False;
  MouseDrawing := False;
  MouseErase := False;
  ShowHelp := False;
  LPalShown := False; LPalCell := 0; LCell := 0;
  LPopX := 0; LPopY := 0; LPopW := 0; LPopH := 0;
  LPalRgbX := 0; LPalRgbY := 0; LPalRgbBtn := 22;
  LPalPreX := 0; LPalPreY := 0; LPalPreW := 0; LPalPreH := 0;
  LViewCol := 0; LViewRow := 0; LViewW := 0; LViewH := 0;
  UiMouseX := 0; UiMouseY := 0; UiClick := False;
  UiAction := uaNone; UiArg := 0; PendingFmt := SPRFMT_HIRES;
  SavedMsg := False; SizeFocus := 0; ColorPopup := False;
  GotoFocus := False; GotoStr := '';
  PalPreset := 0; LPalSelX := 0; LPalSelY := 0; LPalSelW := 0;
  SizeWStr := IntToStr(GW); SizeHStr := IntToStr(GH);
  // Snapshot the program's global palette so we can restore it on exit (the
  // palette-model selector overwrites it only as an editing preview).
  for PalI := 0 to 255 do
    SavedPal[PalI] := FVideoController.GetPaletteColor(PalI);
  Ui := TSedaiUI.Create(FVideoController.Renderer, FVideoController.Font);
  SDL_StartTextInput;   // so the filename field receives SDL_TEXTINPUT events
  DrawEditor;
  while Running do
  begin
    while SDL_PollEvent(@Event) <> 0 do
    begin
      case Event.type_ of
        SDL_QUITEV:
          begin
            Result := True;   // window closed: tell the VM to stop
            Running := False;
          end;
        SDL_TEXTINPUT:
          // A focused size / go-to field takes digits only (max 3); otherwise a
          // filename field (1/2) takes printable chars (not during overwrite prompt).
          if GotoFocus then
          begin
            if (Event.text.text[0] in ['0'..'9']) and (Length(GotoStr) < 3) then
              GotoStr := GotoStr + Char(Event.text.text[0]);
          end
          else if SizeFocus <> 0 then
          begin
            if Event.text.text[0] in ['0'..'9'] then
            begin
              if (SizeFocus = 1) and (Length(SizeWStr) < 3) then
                SizeWStr := SizeWStr + Char(Event.text.text[0])
              else if (SizeFocus = 2) and (Length(SizeHStr) < 3) then
                SizeHStr := SizeHStr + Char(Event.text.text[0]);
            end;
          end
          else if ((InputMode = 1) or (InputMode = 2)) and (not OverwritePending) and
                  (Ord(Event.text.text[0]) >= 32) and (Length(InputText) < 48) then
            InputText := InputText + Char(Event.text.text[0]);
        SDL_MOUSEBUTTONDOWN:
          // The left-button edge feeds the widget panel (UiClick). The colour pop-up
          // captures clicks while open; otherwise the grid paints (L) / erases (R) /
          // middle just moves the cursor.
          begin
            MX := Event.button.x; MY := Event.button.y;
            UiMouseX := MX; UiMouseY := MY;
            if Event.button.button = SDL_BUTTON_LEFT then UiClick := True;
            if ColorPopup then
            begin
              // Inside the pop-up: route the click (swatch / RGB / preset / close);
              // a click outside the pop-up, or any non-left button, closes it.
              if Event.button.button = SDL_BUTTON_LEFT then
              begin
                if not PopupClick(MX, MY) then ColorPopup := False;
              end
              else
                ColorPopup := False;
              UiClick := False;   // don't let this click also hit a panel widget
            end
            else if (InputMode = 0) and (not ShowHelp) and (not ConfirmSwitch) and (SizeFocus = 0) and (not GotoFocus) then
            begin
              if MouseToCell(MX, MY, NW, NH) then
              begin
                CX := NW; CY := NH;
                if Multicolor then CX := CX and (not 1);
                if Event.button.button = SDL_BUTTON_MIDDLE then
                  MouseDrawing := False   // middle button: reposition the cursor only
                else
                begin
                  MouseErase := (Event.button.button = SDL_BUTTON_RIGHT);
                  MouseDrawing := True;
                  PushUndo;               // one undo entry per mouse stroke
                  PaintCurrent(MouseErase);
                end;
              end;
            end;
          end;
        SDL_MOUSEBUTTONUP:
          MouseDrawing := False;
        SDL_MOUSEMOTION:
          begin
            UiMouseX := Event.motion.x; UiMouseY := Event.motion.y;
            // Continue the current stroke while a button is held over the grid.
            if MouseDrawing and (InputMode = 0) and (not ConfirmSwitch) and (not ShowHelp)
               and (SizeFocus = 0) and (not GotoFocus) and (not ColorPopup) then
              if MouseToCell(Event.motion.x, Event.motion.y, NW, NH) then
              begin
                CX := NW; CY := NH;
                if Multicolor then CX := CX and (not 1);
                PaintCurrent(MouseErase);
              end;
          end;
        SDL_KEYDOWN:
          begin
            Sym := Event.key.keysym.sym;
            Mods := Event.key.keysym.mod_;
            IsShiftArrow := ((Mods and KMOD_SHIFT) <> 0) and
              ((Sym = SDLK_LEFT) or (Sym = SDLK_RIGHT) or (Sym = SDLK_UP) or (Sym = SDLK_DOWN));
            if ColorPopup then
            begin
              // While the colour pop-up is open, ESC cancels it; ignore other keys.
              if Sym = SDLK_ESCAPE then ColorPopup := False;
            end
            else if ShowHelp then
            begin
              // Any of H / F1 / ESC closes the help overlay; ignore other keys so
              // the help can't accidentally edit the sprite.
              if (Sym = SDLK_h) or (Sym = SDLK_F1) or (Sym = SDLK_ESCAPE) then
                ShowHelp := False;
            end
            else if GotoFocus then
            begin
              // Editing the "go to sprite" field (digits come via SDL_TEXTINPUT).
              // RETURN jumps to that sprite, ESC cancels, Backspace deletes.
              if (Sym = SDLK_RETURN) or (Sym = SDLK_KP_ENTER) then
              begin
                if GotoStr <> '' then GoToSprite(StrToIntDef(GotoStr, SpriteNum));
                GotoFocus := False; GotoStr := '';
              end
              else if Sym = SDLK_ESCAPE then
                begin GotoFocus := False; GotoStr := ''; end
              else if Sym = SDLK_BACKSPACE then
                begin if Length(GotoStr) > 0 then Delete(GotoStr, Length(GotoStr), 1); end;
            end
            else if SizeFocus <> 0 then
            begin
              // Editing a W/H size field (digits come via SDL_TEXTINPUT). TAB swaps
              // field, RETURN applies, ESC cancels, Backspace deletes.
              if (Sym = SDLK_RETURN) or (Sym = SDLK_KP_ENTER) then
              begin
                NW := StrToIntDef(SizeWStr, GW);
                NH := StrToIntDef(SizeHStr, GH);
                if NW < 1 then NW := 1 else if NW > 255 then NW := 255;
                if NH < 1 then NH := 1 else if NH > 255 then NH := 255;
                ResizeGrid(NW, NH);
                SizeFocus := 0;
              end
              else if Sym = SDLK_ESCAPE then
                SizeFocus := 0
              else if Sym = SDLK_TAB then
                begin if SizeFocus = 1 then SizeFocus := 2 else SizeFocus := 1; end
              else if Sym = SDLK_BACKSPACE then
              begin
                if (SizeFocus = 1) and (Length(SizeWStr) > 0) then
                  Delete(SizeWStr, Length(SizeWStr), 1)
                else if (SizeFocus = 2) and (Length(SizeHStr) > 0) then
                  Delete(SizeHStr, Length(SizeHStr), 1);
              end;
            end
            else if InputMode = 3 then
            begin
              // Size preset menu: 1-8 apply a preset, RETURN switches to free
              // entry, ESC cancels.
              if (Sym >= SDLK_1) and (Sym <= SDLK_8) then
              begin
                KSel := Integer(Sym) - Integer(SDLK_1) + 1;
                if KSel <= PRESET_COUNT then
                begin
                  ResizeGrid(PresetW[KSel], PresetH[KSel]);
                  InputMode := 0;
                end;
              end
              else if (Sym = SDLK_RETURN) or (Sym = SDLK_KP_ENTER) then
                begin InputMode := 4; InputText := ''; end
              else if Sym = SDLK_ESCAPE then
                InputMode := 0;
            end
            else if InputMode = 4 then
            begin
              // Free size entry: type WxH, RETURN applies, ESC cancels.
              if (Sym = SDLK_RETURN) or (Sym = SDLK_KP_ENTER) then
              begin
                if ParseSize(InputText, NW, NH) then ResizeGrid(NW, NH);
                InputMode := 0; InputText := '';
              end
              else if Sym = SDLK_ESCAPE then
                begin InputMode := 0; InputText := ''; end
              else if Sym = SDLK_BACKSPACE then
                if Length(InputText) > 0 then Delete(InputText, Length(InputText), 1);
            end
            else if InputMode <> 0 then
            begin
              if OverwritePending then
              begin
                // Confirm overwriting a different, existing file.
                if Sym = SDLK_y then
                begin
                  PackToSprite;
                  FSpriteEngine.SaveSpritesToJSON(InputText);
                  LastFile := InputText;
                  OverwritePending := False;
                  InputMode := 0;
                end
                else if (Sym = SDLK_n) or (Sym = SDLK_ESCAPE) then
                  OverwritePending := False;   // back to editing the name
              end
              // Filename entry: Enter executes, ESC cancels, Backspace deletes.
              else if (Sym = SDLK_RETURN) or (Sym = SDLK_KP_ENTER) then
              begin
                if InputText = '' then
                  InputMode := 0
                else if InputMode = 1 then
                begin
                  // Ask before overwriting a DIFFERENT existing file; re-saving the
                  // current working file just overwrites it silently.
                  if (InputText <> LastFile) and FileExists(FSpriteEngine.ResolveSpriteFile(InputText)) then
                    OverwritePending := True
                  else
                  begin
                    PackToSprite;   // commit current edits so they are saved
                    FSpriteEngine.SaveSpritesToJSON(InputText);
                    LastFile := InputText;
                    InputMode := 0;
                  end;
                end
                else
                begin
                  FSpriteEngine.LoadSpritesFromJSON(InputText, True);  // editor: use file colours
                  LoadSpriteState;   // reflect the loaded sprite in the editor
                  ClearAllUndo;      // loaded data invalidates the undo history
                  LastFile := InputText;
                  InputMode := 0;
                end;
              end
              else if Sym = SDLK_ESCAPE then
                begin InputMode := 0; OverwritePending := False; end
              else if Sym = SDLK_BACKSPACE then
                if Length(InputText) > 0 then Delete(InputText, Length(InputText), 1);
            end
            else if ConfirmSwitch then
            begin
              // Awaiting Y/N: changing the format reinterprets the bits, so clear &
              // confirm first. PendingFmt is the target (set by M or a format radio).
              if Sym = SDLK_y then
              begin
                PushUndo;
                Fmt := PendingFmt;
                SyncFmtFlags;
                ClearGrid;
                if Multicolor and ((CX and 1) = 1) then CX := CX and (not 1);
                ConfirmSwitch := False;
              end
              else if (Sym = SDLK_n) or (Sym = SDLK_ESCAPE) then
                ConfirmSwitch := False;
            end
            else if IsShiftArrow then
            begin
              // SHIFT+arrows: rectangular selection. With CTRL also held, the new
              // rectangle is ADDED to the current selection.
              if not HasAnchor then StartDrag((Mods and KMOD_CTRL) <> 0);
              if Sym = SDLK_LEFT then
                begin if Multicolor then begin if CX >= 2 then CX := CX - 2; end else if CX > 0 then Dec(CX); end
              else if Sym = SDLK_RIGHT then
                begin if Multicolor then begin if CX <= GW - 3 then CX := CX + 2; end else if CX < GW - 1 then Inc(CX); end
              else if Sym = SDLK_UP then begin if CY > 0 then Dec(CY); end
              else if Sym = SDLK_DOWN then begin if CY < GH - 1 then Inc(CY); end;
              ApplyDragRect;
            end
            else if ((Mods and KMOD_CTRL) <> 0) and (Sym = SDLK_SPACE) then
            begin
              // CTRL+SPACE: toggle the pixel under the cursor in the selection.
              Sel[CY, CX] := not Sel[CY, CX];
              if Multicolor then Sel[CY, CX xor 1] := Sel[CY, CX];
            end
            else if ((Mods and KMOD_CTRL) <> 0) and (Sym = SDLK_c) then
              CopyShape    // CTRL+C: copy selection (or whole shape if none)
            else if ((Mods and KMOD_CTRL) <> 0) and (Sym = SDLK_v) then
              begin PushUndo; PasteShape; end   // CTRL+V: paste the copied shape
            else if ((Mods and KMOD_CTRL) <> 0) and (Sym = SDLK_z) then
              DoUndo       // CTRL+Z: undo (per sprite)
            else if ((Mods and KMOD_CTRL) <> 0) and (Sym = SDLK_y) then
              DoRedo       // CTRL+Y: redo (per sprite)
            else if ((Mods and KMOD_CTRL) <> 0) and (Sym = SDLK_s) then
              begin InputMode := 1; InputText := LastFile; OverwritePending := False; end   // CTRL+S: save (proposes last name)
            else if ((Mods and KMOD_CTRL) <> 0) and (Sym = SDLK_l) then
              begin InputMode := 2; InputText := LastFile; OverwritePending := False; end   // CTRL+L: load (proposes last name)
            else if Sym = SDLK_LEFT then
              begin
                if Multicolor then begin if CX >= 2 then CX := CX - 2; end
                else if CX > 0 then Dec(CX);
              end
            else if Sym = SDLK_RIGHT then
              begin
                if Multicolor then begin if CX <= GW - 3 then CX := CX + 2; end
                else if CX < GW - 1 then Inc(CX);
              end
            else if Sym = SDLK_UP then
              begin if CY > 0 then Dec(CY); end
            else if Sym = SDLK_DOWN then
              begin if CY < GH - 1 then Inc(CY); end
            else if Sym = SDLK_SPACE then
              begin
                PushUndo;
                // Full-color: paint the pen (toggle off if it already matches).
                // Multicolor: cycle 0->1->2->3->0. Hi-res: toggle on/off.
                if FullColor then
                begin
                  if (Grid[CY, CX] = PenIdx) and (PenIdx <> 0) then SetCell(0)
                  else SetCell(PenIdx);
                end
                else if Multicolor then
                  SetCell((Grid[CY, CX] + 1) and 3)
                else if Grid[CY, CX] <> 0 then SetCell(0)
                else SetCell(1);
              end
            else if Sym = SDLK_0 then
              begin PushUndo; SetCell(0); end   // erase the pixel under the cursor
            else if (Sym >= SDLK_1) and (Sym <= SDLK_8) then
              // 1-8 quick-select the first eight sprites (commit current, load chosen)
              GoToSprite(Integer(Sym) - Integer(SDLK_1) + 1)
            else if Sym = SDLK_PAGEDOWN then
              GoToSprite(SpriteNum + 1)    // next sprite
            else if Sym = SDLK_PAGEUP then
              GoToSprite(SpriteNum - 1)    // previous sprite
            else if Sym = SDLK_HOME then
              GoToSprite(1)                // first sprite
            else if Sym = SDLK_END then
              GoToSprite(MAX_SPRITES)      // last sprite
            else if (Sym = SDLK_h) or (Sym = SDLK_F1) then
              ShowHelp := True   // H / F1: show the help overlay
            else if Sym = SDLK_m then
              begin PendingFmt := (Fmt + 1) mod 3; ConfirmSwitch := True; end   // M: next format (confirm)
            else if Sym = SDLK_s then
              begin SizeFocus := 1; GotoFocus := False; end   // S: focus the W size field
            else if Sym = SDLK_p then
              MulPen := Byte((MulPen mod 3) + 1)   // P: cycle multicolor mouse pen
            else if Sym = SDLK_x then
              ExpandX := not ExpandX
            else if Sym = SDLK_y then
              ExpandY := not ExpandY
            else if Sym = SDLK_c then
              CycleColor(1)
            else if Sym = SDLK_LEFTBRACKET then
              CycleColor(-1)
            else if Sym = SDLK_RIGHTBRACKET then
              CycleColor(1)
            else if Sym = SDLK_DELETE then
              begin PushUndo; ClearGrid; end
            else if (Sym = SDLK_RETURN) or (Sym = SDLK_KP_ENTER) then
              begin PackToSprite; SavedMsg := True; end   // RETURN: save (commit) WITHOUT exiting
            else if Sym = SDLK_ESCAPE then
              Running := False;   // ESC: exit the editor

            // Any key that is not a SHIFT+arrow ends the rectangle drag, so the
            // next SHIFT+arrow begins a fresh anchor.
            if not IsShiftArrow then HasAnchor := False;
          end;
      end;
    end;
    if Running then
    begin
      DrawEditor;
      // Perform deferred widget actions (these need helpers declared after
      // DrawEditor, so the panel records them and we run them here).
      case UiAction of
        uaGoto: GoToSprite(UiArg);
        uaPreset: ResizeGrid(PresetW[UiArg], PresetH[UiArg]);
        uaSizeApply:
          begin
            NW := StrToIntDef(SizeWStr, GW);
            NH := StrToIntDef(SizeHStr, GH);
            if NW < 1 then NW := 1 else if NW > 255 then NW := 255;
            if NH < 1 then NH := 1 else if NH > 255 then NH := 255;
            ResizeGrid(NW, NH);
            SizeFocus := 0;
          end;
        uaFmtApply:
          begin
            PushUndo;
            Fmt := PendingFmt;
            SyncFmtFlags;
            ClearGrid;
            if Multicolor and ((CX and 1) = 1) then CX := CX and (not 1);
            ConfirmSwitch := False;
          end;
        uaFmtCancel: ConfirmSwitch := False;
      end;
      UiAction := uaNone;
      UiClick := False;   // consume the click; don't reuse it next frame
      SDL_Delay(16);
    end;
  end;

  Ui.Free;

  // Restore the program's global palette (the palette-model selector is only an
  // editing preview; without this the program's screen stays recoloured on exit).
  for PalI := 0 to 255 do
    FVideoController.SetPaletteColor(PalI, SavedPal[PalI]);

  // Repaint the program's screen so the editor view is not left on screen.
  FLastVMRenderTick := SDL_GetTicks;
  RenderScreen;
  UpdateCursor;
  FVideoController.Present;
end;

procedure TSedaiNewConsole.SetCursorEnabled(Enable: Boolean);
begin
  FCursorEnabled := Enable;
  if Enable then
  begin
    // Reset blink state so cursor appears immediately
    FCursorVisible := True;
    FLastCursorBlink := SDL_GetTicks;
  end;
end;

procedure TSedaiNewConsole.Run;
var
  CurrentLine: string;
  FileExt: string;
  FkDef: string;  // Function key definition
  i: Integer;     // For history shift loop
begin
  FRunning := True;

  ShowSplashScreen;

  // Load startup file if specified
  if FStartupFile <> '' then
  begin
    FileExt := LowerCase(ExtractFileExt(FStartupFile));
    if FileExt = '.basc' then
    begin
      // Load precompiled bytecode
      FTextBuffer.PutString('BLOAD "' + FStartupFile + '"');
      FTextBuffer.NewLine;
      ExecuteBLoad(FStartupFile);
    end
    else if not SourceHasLineNumbers(FStartupFile) then
    begin
      // FreeBASIC (MODERN) source: no line numbers, so it cannot live in the line-keyed program store.
      // Compile it straight to bytecode and run it via the bytecode path (like a precompiled .basc).
      FTextBuffer.PutString('LOAD "' + FStartupFile + '"');
      FTextBuffer.NewLine;
      LoadModernSource(FStartupFile);
    end
    else
    begin
      // Classic line-numbered BASIC: load into the editable program store.
      FTextBuffer.PutString('LOAD "' + FStartupFile + '"');
      FTextBuffer.NewLine;
      ExecuteLoad(FStartupFile);
    end;

    // Auto-run if requested and file was loaded successfully
    if FAutoRun and ((FProgramMemory.GetLineCount > 0) or FBytecodeMode) then
    begin
      FTextBuffer.PutString('RUN');
      FTextBuffer.NewLine;
      ProcessCommand('RUN');
    end;

    // Clear startup file to prevent reloading
    FStartupFile := '';
  end;

  while FRunning do
  begin
    FInputHandler.ProcessEvents;

    if FInputHandler.QuitRequested then
      FRunning := False;

    // Note: CTRL+C now stops BASIC programs, not exits console
    // CTRL+ALT+END exits SedaiVision (handled in ProcessEvents -> QuitRequested)

    // CTRL+F per toggle fullscreen
    if (FInputHandler.LastKeyDown = SDLK_f) and FInputHandler.CtrlPressed then
    begin
      ToggleFullscreen;
      FInputHandler.ClearFlags;
      Continue;
    end;

    // Function keys F1-F12: expand to defined string (check FIRST, before other input)
    if FInputHandler.HasChar and (Ord(FInputHandler.LastChar) >= 128) and (Ord(FInputHandler.LastChar) <= 139) then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      // Get function key definition (F1=#128 -> index 1, etc.)
      FkDef := GetFunctionKeyDefinition(Ord(FInputHandler.LastChar) - 127);
      if FkDef <> '' then
      begin
        // Check for CHR$(13) at end for auto-execute (C128 convention)
        if (Length(FkDef) > 0) and (FkDef[Length(FkDef)] = #13) then
        begin
          Delete(FkDef, Length(FkDef), 1);
          FTextBuffer.PutString(FkDef);
          FTextBuffer.NewLine;
          ProcessCommand(FkDef);
          FUserHasTyped := False;
        end
        else
        begin
          FTextBuffer.PutString(FkDef);
          FUserHasTyped := True;
        end;
      end;
      FInputHandler.ClearFlags;
      Continue;
    end;

    // Handle input
    if FInputHandler.LastKeyDown = SDLK_RETURN then
    begin
      // If in scrollback mode, return to end first
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      // Get current line content from text buffer
      CurrentLine := FTextBuffer.GetCurrentLine;
      FTextBuffer.NewLine;  // Va a capo dopo l'input
      // Process the command
      ProcessCommand(CurrentLine);
      // Add to history ONLY if user actually typed/edited something
      if FUserHasTyped and (Trim(CurrentLine) <> '') then
      begin
        if FHistoryCount < INPUT_HISTORY_SIZE then
        begin
          FInputHistory[FHistoryCount] := CurrentLine;
          Inc(FHistoryCount);
          SaveHistory;
        end
        else
        begin
          // Shift history - use proper string assignment (NOT Move!)
          // Move() on strings only copies pointers, corrupting reference counts
          for i := 0 to INPUT_HISTORY_SIZE - 2 do
            FInputHistory[i] := FInputHistory[i + 1];
          FInputHistory[INPUT_HISTORY_SIZE - 1] := CurrentLine;
          // Rewrite entire history file to prevent infinite growth
          SaveHistoryFull;
        end;
      end;
      FHistoryPos := -1;
      FUserHasTyped := False;  // Reset for next input
    end
    else if FInputHandler.LastKeyDown = SDLK_BACKSPACE then
    begin
      // Exit scrollback mode if active
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.DeleteChar;
      FUserHasTyped := True;  // User modified the line
    end
    else if FInputHandler.LastKeyDown = SDLK_UP then
    begin
      // Exit scrollback mode if active
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      // History: freccia SU
      if (FHistoryCount > 0) and (FHistoryPos < FHistoryCount - 1) then
      begin
        Inc(FHistoryPos);
        // Bounds check before accessing history array
        if (FHistoryPos >= 0) and (FHistoryCount - 1 - FHistoryPos >= 0) and
           (FHistoryCount - 1 - FHistoryPos < INPUT_HISTORY_SIZE) then
        begin
          // Cancella la riga corrente
          FTextBuffer.ClearCurrentLine;
          // Inserisce il comando dalla history (troncato alla larghezza della riga)
          FTextBuffer.PutStringNoWrap(FInputHistory[FHistoryCount - 1 - FHistoryPos]);
          FUserHasTyped := True;  // User selected from history = intent to execute
        end
        else
          Dec(FHistoryPos);  // Rollback if out of bounds
      end;
    end
    else if FInputHandler.LastKeyDown = SDLK_DOWN then
    begin
      // Exit scrollback mode if active
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      // History: freccia GIU
      if FHistoryPos > 0 then
      begin
        Dec(FHistoryPos);
        // Bounds check before accessing history array
        if (FHistoryPos >= 0) and (FHistoryCount - 1 - FHistoryPos >= 0) and
           (FHistoryCount - 1 - FHistoryPos < INPUT_HISTORY_SIZE) then
        begin
          // Cancella la riga corrente
          FTextBuffer.ClearCurrentLine;
          // Inserisce il comando dalla history (troncato alla larghezza della riga)
          FTextBuffer.PutStringNoWrap(FInputHistory[FHistoryCount - 1 - FHistoryPos]);
          FUserHasTyped := True;  // User selected from history = intent to execute
        end
        else
          Inc(FHistoryPos);  // Rollback if out of bounds
      end
      else if FHistoryPos = 0 then
      begin
        FHistoryPos := -1;
        // Cancella la riga corrente
        FTextBuffer.ClearCurrentLine;
        FUserHasTyped := False;  // Line cleared, no user input
      end;
    end
    else if (FInputHandler.LastKeyDown = SDLK_PAGEUP) and FInputHandler.CtrlPressed then
    begin
      // Pager: delega al VideoController con CTRL
      FVideoController.HandleScrollKeys(SDLK_PAGEUP, 0);
    end
    else if (FInputHandler.LastKeyDown = SDLK_PAGEDOWN) and FInputHandler.CtrlPressed then
    begin
      // Pager: delega al VideoController con CTRL
      FVideoController.HandleScrollKeys(SDLK_PAGEDOWN, 0);
    end
    // Scrollback buffer navigation with Shift+PgUp/PgDown/End
    else if (FInputHandler.LastKeyDown = SDLK_PAGEUP) and FInputHandler.ShiftPressed then
    begin
      {$IFDEF DEBUG_CONSOLE}WriteLn('[DEBUG] Shift+PgUp detected, scrolling up');{$ENDIF}
      // Scroll back one page (screen height)
      FTextBuffer.ScrollViewUp(FTextBuffer.Rows);
    end
    else if (FInputHandler.LastKeyDown = SDLK_PAGEDOWN) and FInputHandler.ShiftPressed then
    begin
      {$IFDEF DEBUG_CONSOLE}WriteLn('[DEBUG] Shift+PgDown detected, scrolling down');{$ENDIF}
      // Scroll forward one page (screen height)
      FTextBuffer.ScrollViewDown(FTextBuffer.Rows);
    end
    else if (FInputHandler.LastKeyDown = SDLK_END) and FInputHandler.ShiftPressed then
    begin
      {$IFDEF DEBUG_CONSOLE}WriteLn('[DEBUG] Shift+End detected, scrolling to end');{$ENDIF}
      // Return to end of buffer
      FTextBuffer.ScrollToEnd;
    end
    else if (FInputHandler.LastKeyDown = SDLK_HOME) and FInputHandler.ShiftPressed then
    begin
      {$IFDEF DEBUG_CONSOLE}WriteLn('[DEBUG] Shift+Home detected, scrolling to start');{$ENDIF}
      // Go to beginning of scrollback buffer
      FTextBuffer.ScrollToStart;
    end
    // Ctrl+Left: jump word left
    else if (FInputHandler.LastKeyDown = SDLK_LEFT) and FInputHandler.CtrlPressed and not FInputHandler.ShiftPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.CursorWordLeft;
    end
    // Ctrl+Right: jump word right
    else if (FInputHandler.LastKeyDown = SDLK_RIGHT) and FInputHandler.CtrlPressed and not FInputHandler.ShiftPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.CursorWordRight;
    end
    // Ctrl+Home: delete to line start
    else if (FInputHandler.LastKeyDown = SDLK_HOME) and FInputHandler.CtrlPressed and not FInputHandler.ShiftPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.DeleteToLineStart;
      FUserHasTyped := True;
    end
    // Ctrl+End: delete to line end
    else if (FInputHandler.LastKeyDown = SDLK_END) and FInputHandler.CtrlPressed and not FInputHandler.ShiftPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.DeleteToLineEnd;
      FUserHasTyped := True;
    end
    // Ctrl+Delete: delete word right
    else if (FInputHandler.LastKeyDown = SDLK_DELETE) and FInputHandler.CtrlPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.DeleteWordRight;
      FUserHasTyped := True;
    end
    // Ctrl+Backspace: delete word left
    else if (FInputHandler.LastKeyDown = SDLK_BACKSPACE) and FInputHandler.CtrlPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.DeleteWordLeft;
      FUserHasTyped := True;
    end
    // Ctrl+A: go to line start (readline style)
    else if (FInputHandler.LastKeyDown = SDLK_a) and FInputHandler.CtrlPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.CursorHome;
    end
    // Ctrl+E: go to line end (readline style)
    else if (FInputHandler.LastKeyDown = SDLK_e) and FInputHandler.CtrlPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.CursorEnd;
    end
    // Ctrl+K: delete to end of line (readline style)
    else if (FInputHandler.LastKeyDown = SDLK_k) and FInputHandler.CtrlPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.DeleteToLineEnd;
      FUserHasTyped := True;
    end
    // Ctrl+U: delete entire line (readline style)
    else if (FInputHandler.LastKeyDown = SDLK_u) and FInputHandler.CtrlPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.DeleteEntireLine;
      FUserHasTyped := True;
    end
    // Cursor movement keys (without Shift or Ctrl)
    else if (FInputHandler.LastKeyDown = SDLK_LEFT) and not FInputHandler.ShiftPressed and not FInputHandler.CtrlPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.CursorLeft;
    end
    else if (FInputHandler.LastKeyDown = SDLK_RIGHT) and not FInputHandler.ShiftPressed and not FInputHandler.CtrlPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.CursorRight;
    end
    else if (FInputHandler.LastKeyDown = SDLK_HOME) and not FInputHandler.ShiftPressed and not FInputHandler.CtrlPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.CursorHome;
    end
    else if (FInputHandler.LastKeyDown = SDLK_END) and not FInputHandler.ShiftPressed and not FInputHandler.CtrlPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.CursorEnd;
    end
    else if (FInputHandler.LastKeyDown = SDLK_DELETE) and not FInputHandler.CtrlPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.DeleteCharAtCursor;
      FUserHasTyped := True;
    end
    // Debug: check if PgUp is pressed at all
    else if FInputHandler.LastKeyDown = SDLK_PAGEUP then
    begin
      {$IFDEF DEBUG_CONSOLE}WriteLn('[DEBUG] PgUp pressed, ShiftPressed=', FInputHandler.ShiftPressed, ' CtrlPressed=', FInputHandler.CtrlPressed);{$ENDIF}
    end
    else if FInputHandler.LastKeyDown = SDLK_PAGEDOWN then
    begin
      {$IFDEF DEBUG_CONSOLE}WriteLn('[DEBUG] PgDown pressed, ShiftPressed=', FInputHandler.ShiftPressed, ' CtrlPressed=', FInputHandler.CtrlPressed);{$ENDIF}
    end
    else if FInputHandler.HasChar and (FInputHandler.LastChar >= ' ') then
    begin
      // Only insert printable characters (>= space). Skip control chars like ESC (#27)
      // Exit scrollback mode if active - any typed character returns to prompt
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      // Use InsertChar for proper insert mode (shifts chars right)
      FTextBuffer.InsertChar(FInputHandler.LastChar);
      FUserHasTyped := True;  // User typed a character
    end;

    // Render only when needed (text changed OR cursor blink changed)
    if FTextBuffer.NeedsRedraw or CheckCursorBlink then
    begin
      RenderScreen;
      UpdateCursor;
      FVideoController.Present;
    end;

    SDL_Delay(16); // 60 FPS
  end;
end;

{ TInputHandler - IInputDevice implementation }

function TInputHandler.ReadLine(const Prompt: string; IsCommand: Boolean; NumericOnly: Boolean; AllowDecimal: Boolean): string;
var
  Event: TSDL_Event;
  InputBuffer: string;
  Done: Boolean;
  Ch: Char;
  IsValidChar: Boolean;
  KeyMod: UInt16;
  CtrlDown, AltDown: Boolean;
  FkDef: string;
  FkIdx: Integer;
begin
  InputBuffer := '';
  Done := False;

  // Mostra il prompt se fornito
  if Prompt <> '' then
  begin
    FTextBuffer.PutString(Prompt);
    // Render immediato per mostrare il prompt CON CURSORE
    if Assigned(FConsole) then
    begin
      FConsole.RenderScreen;
      FConsole.UpdateCursor;
      FVideoController.Present;
    end;
  end;

  while not Done and not FQuitRequested do
  begin
    while SDL_PollEvent(@Event) = 1 do
    begin
      case Event.type_ of
        SDL_QUITEV:
          begin
            FQuitRequested := True;
            Done := True;
          end;

        SDL_KEYDOWN:
          begin
            // Get modifier state using SDL_GetModState for reliable detection
            KeyMod := SDL_GetModState;
            CtrlDown := (KeyMod and KMOD_CTRL) <> 0;
            AltDown := (KeyMod and KMOD_ALT) <> 0;

            // END key stops the program
            if (Event.key.keysym.scancode = SDL_SCANCODE_END) then
            begin
              FStopRequested := True;
              Done := True;
            end
            else
            case Event.key.keysym.sym of
              SDLK_RETURN, SDLK_KP_ENTER:
                begin
                  // Go to new line when pressing ENTER
                  FTextBuffer.NewLine;
                  Done := True;
                end;

              SDLK_BACKSPACE:
                begin
                  if Length(InputBuffer) > 0 then
                  begin
                    Delete(InputBuffer, Length(InputBuffer), 1);
                    FTextBuffer.DeleteChar;  // Rimuovi visivamente il carattere
                  end;
                end;

              SDLK_ESCAPE:
                begin
                  // Cancella tutto l'input
                  while Length(InputBuffer) > 0 do
                  begin
                    Delete(InputBuffer, Length(InputBuffer), 1);
                    FTextBuffer.DeleteChar;
                  end;
                  Done := True;
                end;

              // Function keys F1-F12: expand to defined string
              SDLK_F1, SDLK_F2, SDLK_F3, SDLK_F4,
              SDLK_F5, SDLK_F6, SDLK_F7, SDLK_F8,
              SDLK_F9, SDLK_F10, SDLK_F11, SDLK_F12:
                begin
                  // Get function key number (F1=1, F2=2, etc.)
                  case Event.key.keysym.sym of
                    SDLK_F1: FkIdx := 1;
                    SDLK_F2: FkIdx := 2;
                    SDLK_F3: FkIdx := 3;
                    SDLK_F4: FkIdx := 4;
                    SDLK_F5: FkIdx := 5;
                    SDLK_F6: FkIdx := 6;
                    SDLK_F7: FkIdx := 7;
                    SDLK_F8: FkIdx := 8;
                    SDLK_F9: FkIdx := 9;
                    SDLK_F10: FkIdx := 10;
                    SDLK_F11: FkIdx := 11;
                    SDLK_F12: FkIdx := 12;
                  else
                    FkIdx := 0;
                  end;

                  // Get function key definition from console
                  if (FkIdx > 0) and Assigned(FConsole) then
                  begin
                    FkDef := FConsole.GetFunctionKeyDefinition(FkIdx);
                    if FkDef <> '' then
                    begin
                      // Check for CHR$(13) at end for auto-execute (C128 convention)
                      if (Length(FkDef) > 0) and (FkDef[Length(FkDef)] = #13) then
                      begin
                        // Remove the CHR$(13) from the string
                        Delete(FkDef, Length(FkDef), 1);
                        // Insert into input buffer and echo to screen
                        InputBuffer := InputBuffer + FkDef;
                        FTextBuffer.PutString(FkDef);
                        // Auto-execute by going to new line
                        FTextBuffer.NewLine;
                        Done := True;
                      end
                      else
                      begin
                        // Just insert the definition (no auto-execute)
                        InputBuffer := InputBuffer + FkDef;
                        FTextBuffer.PutString(FkDef);
                      end;
                    end;
                  end;
                end;
            end;
          end;
          
        SDL_TEXTINPUT:
          begin
            Ch := Event.text.text[0];
            IsValidChar := False;
            
            if NumericOnly then
            begin
              // Numeric mode: accept only digits, sign, decimal point (if allowed)
              if (Ch >= '0') and (Ch <= '9') then
                IsValidChar := True
              else if (Ch = '-') and (Length(InputBuffer) = 0) then
                IsValidChar := True  // Negative sign only at the beginning
              else if (Ch = '+') and (Length(InputBuffer) = 0) then
                IsValidChar := True  // Positive sign only at the beginning
              else if AllowDecimal and (Ch = '.') and (Pos('.', InputBuffer) = 0) then
                IsValidChar := True;  // Decimal point only if allowed and not already present
            end
            else
            begin
              // Normal mode: all printable characters
              if (Ch >= ' ') and (Ch <= '~') then
                IsValidChar := True;
            end;
            
            if IsValidChar then
            begin
              InputBuffer := InputBuffer + Ch;
              FTextBuffer.PutChar(Ch);  // Echo the character
            end;
          end;
      end;
    end;
    
    // Render dopo ogni evento CON CURSORE
    if Assigned(FConsole) then
    begin
      FConsole.RenderScreen;
      FConsole.UpdateCursor;
      FVideoController.Present;
    end;
    
    SDL_Delay(10);  // Evita CPU al 100%
  end;
  
  Result := InputBuffer;
end;

function TInputHandler.ReadKey: Char;
begin
  ProcessEvents;
  if HasChar then
    Result := LastChar
  else
    Result := #0;
end;

function TInputHandler.KeyPressed: Boolean;
begin
  Result := FHasChar;
end;

function TInputHandler.HasChar: Boolean;
begin
  Result := FHasChar;
end;

function TInputHandler.GetLastChar: string;
begin
  if FHasChar then
  begin
    Result := FLastChar;
    FHasChar := False;  // Consume the character
  end
  else
    Result := '';
end;

function TInputHandler.ShouldQuit: Boolean;
begin
  Result := QuitRequested;
end;

procedure TInputHandler.EnableTextInput;
begin
  SDL_StartTextInput;
end;

procedure TInputHandler.DisableTextInput;
begin
  SDL_StopTextInput;
end;

function TInputHandler.ShouldStop: Boolean;
begin
  Result := StopRequested;
end;

procedure TInputHandler.ClearStopRequest;
begin
  FStopRequested := False;
end;

procedure TInputHandler.Reset;
begin
  ClearFlags;
  FQuitRequested := False;
  FStopRequested := False;
end;

{ ============================================================================
  TProgramInputHandler - Dedicated input handler for BASIC program execution
  ============================================================================ }

constructor TProgramInputHandler.Create(AVideoController: TVideoController; AOutputDevice: IOutputDevice);
begin
  inherited Create;
  FVideoController := AVideoController;
  FOutputDevice := AOutputDevice;
  FQuitRequested := False;
  FStopRequested := False;
  FLastChar := '';
  FHasChar := False;
  FShiftPressed := False;
  FCtrlPressed := False;
  FAltPressed := False;
  FTextInputActive := False;
end;

destructor TProgramInputHandler.Destroy;
begin
  if FTextInputActive then
    SDL_StopTextInput;
  inherited Destroy;
end;

procedure TProgramInputHandler.EnableTextInput;
begin
  if not FTextInputActive then
  begin
    SDL_StartTextInput;
    FTextInputActive := True;
  end;
end;

procedure TProgramInputHandler.DisableTextInput;
begin
  // IMPORTANT: Do NOT call SDL_StopTextInput here!
  // We want text input to remain active for the console after program ends.
  // Just mark our internal state as inactive.
  FTextInputActive := False;
end;

procedure TProgramInputHandler.ProcessEvents;
var
  Event: TSDL_Event;
  KeySym: Integer;
  KeyMod: Word;
begin
  // IMPORTANT: Do NOT clear FHasChar/FLastChar here!
  // This is the key difference from TInputHandler.ProcessEvents.
  // We only consume the character when GetLastChar is called.

  // If we already have a pending char, don't read more events
  if FHasChar then
    Exit;

  while SDL_PollEvent(@Event) <> 0 do
  begin
    case Event.type_ of
      SDL_QUITEV:
      begin
        FQuitRequested := True;
        Exit;
      end;

      // Render targets reset / window restored (ALT+TAB on Windows D3D): flag a
      // rebuild; HandleVMEventPoll repaints so a running program isn't left black.
      SDL_RENDER_TARGETS_RESET, SDL_RENDER_DEVICE_RESET:
        FRenderResetPending := True;

      SDL_WINDOWEVENT:
        case Event.window.event of
          SDL_WINDOWEVENT_FOCUS_GAINED,
          SDL_WINDOWEVENT_RESTORED,
          SDL_WINDOWEVENT_EXPOSED,
          SDL_WINDOWEVENT_SHOWN:
            FRenderResetPending := True;
        end;

      SDL_KEYDOWN:
      begin
        KeySym := Event.key.keysym.sym;
        KeyMod := Event.key.keysym.mod_;

        // Update modifier state
        FShiftPressed := (KeyMod and KMOD_SHIFT) <> 0;
        FCtrlPressed := (KeyMod and KMOD_CTRL) <> 0;
        FAltPressed := (KeyMod and KMOD_ALT) <> 0;

        // CTRL+ALT+END: Exit SedaiVision completely
        if (KeySym = SDLK_END) and FCtrlPressed and FAltPressed then
        begin
          FQuitRequested := True;
          Exit;
        end;

        // PAUSE/BREAK key ("Pausa/Interr" on IT keyboards) = stop the running BASIC
        // program. Matched with OR without Ctrl: on Windows, Ctrl+Pause is the "Break"
        // variant and SDL2 reports it inconsistently (the Ctrl modifier may be absent
        // and the code may arrive as PAUSE or CANCEL), so requiring Ctrl is unreliable.
        // The Pause/Break key is otherwise unused by BASIC programs, so a bare press is
        // safe. Mirrors the console build, whose handler breaks on CTRL_BREAK_EVENT.
        if (Event.key.keysym.scancode = SDL_SCANCODE_PAUSE) or
           (Event.key.keysym.scancode = SDL_SCANCODE_CANCEL) or
           (KeySym = SDLK_PAUSE) or (KeySym = SDLK_CANCEL) then
        begin
          FStopRequested := True;
          Exit;
        end;

        // ESC is NOT a break key — keep it free for program use. Deliver it to the
        // program as CHR$(27) (ESC produces no SDL_TEXTINPUT event of its own).
        if KeySym = SDLK_ESCAPE then
        begin
          FLastChar := #27;
          FHasChar := True;
          Exit;
        end;

        // CTRL+F: toggle fullscreen even while a program is running
        // (consumed by HandleVMEventPoll, which owns the console/renderer).
        if (KeySym = SDLK_f) and FCtrlPressed and (not FAltPressed) then
        begin
          FToggleFullscreenRequested := True;
          Exit;
        end;

        // Handle special keys that don't generate SDL_TEXTINPUT
        case KeySym of
          SDLK_RETURN, SDLK_KP_ENTER:
          begin
            FLastChar := #13;
            FHasChar := True;
            Exit;
          end;
          SDLK_BACKSPACE:
          begin
            FLastChar := #8;
            FHasChar := True;
            Exit;
          end;
          SDLK_DELETE:
          begin
            FLastChar := #127;
            FHasChar := True;
            Exit;
          end;
          SDLK_TAB:
          begin
            FLastChar := #9;
            FHasChar := True;
            Exit;
          end;
          // Cursor keys - use control codes compatible with C128 BASIC
          SDLK_UP:
          begin
            FLastChar := #1;   // SOH - Cursor UP
            FHasChar := True;
            Exit;
          end;
          SDLK_DOWN:
          begin
            FLastChar := #2;   // STX - Cursor DOWN
            FHasChar := True;
            Exit;
          end;
          SDLK_LEFT:
          begin
            FLastChar := #3;   // ETX - Cursor LEFT
            FHasChar := True;
            Exit;
          end;
          SDLK_RIGHT:
          begin
            FLastChar := #4;   // EOT - Cursor RIGHT
            FHasChar := True;
            Exit;
          end;
          // Navigation keys
          SDLK_HOME:
          begin
            FLastChar := #5;   // ENQ - HOME
            FHasChar := True;
            Exit;
          end;
          SDLK_END:
          begin
            // Deliver END only without CTRL (CTRL+END is reserved for editing/quit combos)
            if not FCtrlPressed then
            begin
              FLastChar := #6;   // ACK - END
              FHasChar := True;
              Exit;
            end;
          end;
          SDLK_PAGEUP:
          begin
            FLastChar := #11;  // VT - PAGE UP
            FHasChar := True;
            Exit;
          end;
          SDLK_PAGEDOWN:
          begin
            FLastChar := #12;  // FF - PAGE DOWN
            FHasChar := True;
            Exit;
          end;
          SDLK_INSERT:
          begin
            FLastChar := #14;  // SO - INSERT
            FHasChar := True;
            Exit;
          end;
          // Function keys F1-F12 (codes 128-139)
          SDLK_F1:
          begin
            FLastChar := #128;
            FHasChar := True;
            Exit;
          end;
          SDLK_F2:
          begin
            FLastChar := #129;
            FHasChar := True;
            Exit;
          end;
          SDLK_F3:
          begin
            FLastChar := #130;
            FHasChar := True;
            Exit;
          end;
          SDLK_F4:
          begin
            FLastChar := #131;
            FHasChar := True;
            Exit;
          end;
          SDLK_F5:
          begin
            FLastChar := #132;
            FHasChar := True;
            Exit;
          end;
          SDLK_F6:
          begin
            FLastChar := #133;
            FHasChar := True;
            Exit;
          end;
          SDLK_F7:
          begin
            FLastChar := #134;
            FHasChar := True;
            Exit;
          end;
          SDLK_F8:
          begin
            FLastChar := #135;
            FHasChar := True;
            Exit;
          end;
          SDLK_F9:
          begin
            FLastChar := #136;
            FHasChar := True;
            Exit;
          end;
          SDLK_F10:
          begin
            FLastChar := #137;
            FHasChar := True;
            Exit;
          end;
          SDLK_F11:
          begin
            FLastChar := #138;
            FHasChar := True;
            Exit;
          end;
          SDLK_F12:
          begin
            FLastChar := #139;
            FHasChar := True;
            Exit;
          end;
        end;
        // Other keys will come through SDL_TEXTINPUT
      end;

      SDL_KEYUP:
      begin
        // Update modifier state on key release
        FShiftPressed := (Event.key.keysym.mod_ and KMOD_SHIFT) <> 0;
        FCtrlPressed := (Event.key.keysym.mod_ and KMOD_CTRL) <> 0;
        FAltPressed := (Event.key.keysym.mod_ and KMOD_ALT) <> 0;
      end;

      SDL_TEXTINPUT:
      begin
        // Normal text characters come through here
        if Length(Event.text.text) > 0 then
        begin
          FLastChar := Event.text.text;  // Full UTF-8 string
          FHasChar := True;
          Exit;
        end;
      end;
    end;
  end;
end;

function TProgramInputHandler.HasChar: Boolean;
begin
  if not FHasChar then
    ProcessEvents;
  Result := FHasChar;
end;

function TProgramInputHandler.GetLastChar: string;
begin
  if not FHasChar then
    ProcessEvents;

  if FHasChar then
  begin
    Result := FLastChar;
    FHasChar := False;
    FLastChar := '';
  end
  else
    Result := '';
end;

function TProgramInputHandler.ReadKey: Char;
begin
  // Wait for a key press
  EnableTextInput;
  try
    while not FHasChar and not FQuitRequested and not FStopRequested do
    begin
      ProcessEvents;
      if not FHasChar then
        SDL_Delay(10);
    end;

    if FHasChar then
    begin
      if Length(FLastChar) > 0 then
        Result := FLastChar[1]
      else
        Result := #0;
      FHasChar := False;
      FLastChar := '';
    end
    else
      Result := #0;
  finally
    DisableTextInput;
  end;
end;

function TProgramInputHandler.KeyPressed: Boolean;
begin
  ProcessEvents;
  Result := FHasChar;
end;

function TProgramInputHandler.ReadLine(const Prompt: string; IsCommand: Boolean;
  NumericOnly: Boolean; AllowDecimal: Boolean): string;
var
  InputBuffer: string;
  Ch: Char;
  Done: Boolean;
begin
  InputBuffer := '';
  Done := False;

  // Enable cursor during INPUT
  if Assigned(FOnCursorEnable) then
    FOnCursorEnable(True);

  // Flush any buffered output before waiting for input
  // This ensures PRINT statements before INPUT are displayed immediately
  if Assigned(FOutputDevice) then
    FOutputDevice.Present
  else if Assigned(FVideoController) then
    FVideoController.Present;

  // Display prompt - use OutputDevice if available for proper TextBuffer sync
  if (Prompt <> '') then
  begin
    if Assigned(FOutputDevice) then
    begin
      FOutputDevice.Print(Prompt);
      FOutputDevice.Present;
    end
    else if Assigned(FVideoController) then
    begin
      FVideoController.Print(Prompt);
      FVideoController.Present;
    end;
  end;

  EnableTextInput;
  try
    while not Done and not FQuitRequested and not FStopRequested do
    begin
      ProcessEvents;

      if FHasChar then
      begin
        if Length(FLastChar) > 0 then
          Ch := FLastChar[1]
        else
          Ch := #0;
        FHasChar := False;
        FLastChar := '';

        case Ch of
          #13:  // ENTER
          begin
            Done := True;
            if Assigned(FOutputDevice) then
            begin
              FOutputDevice.NewLine;
              FOutputDevice.Present;
            end
            else if Assigned(FVideoController) then
            begin
              FVideoController.NewLine;
              FVideoController.Present;
            end;
          end;
          #8:   // BACKSPACE
          begin
            if Length(InputBuffer) > 0 then
            begin
              Delete(InputBuffer, Length(InputBuffer), 1);
              // Visual feedback - use OutputDevice if available
              if Assigned(FOutputDevice) then
              begin
                FOutputDevice.MoveCursor(-1, 0);
                FOutputDevice.Print(' ');
                FOutputDevice.MoveCursor(-1, 0);
                FOutputDevice.Present;
              end
              else if Assigned(FVideoController) then
              begin
                FVideoController.MoveCursor(-1, 0);
                FVideoController.Print(' ');
                FVideoController.MoveCursor(-1, 0);
                FVideoController.Present;
              end;
            end;
          end;
          #27:  // ESC - break input
          begin
            FStopRequested := True;
            Done := True;
          end;
        else
          if Ch >= ' ' then
          begin
            // Check if character is valid based on NumericOnly/AllowDecimal
            if NumericOnly then
            begin
              // Numeric mode: accept only digits, sign, decimal point (if allowed)
              if (Ch >= '0') and (Ch <= '9') then
                // digit ok
              else if (Ch = '-') and (Length(InputBuffer) = 0) then
                // Negative sign only at the beginning
              else if (Ch = '+') and (Length(InputBuffer) = 0) then
                // Positive sign only at the beginning
              else if AllowDecimal and (Ch = '.') and (Pos('.', InputBuffer) = 0) then
                // Decimal point only if allowed and not already present
              else
                Continue;  // Skip invalid character
            end;

            InputBuffer := InputBuffer + Ch;
            if Assigned(FOutputDevice) then
            begin
              FOutputDevice.Print(Ch);
              FOutputDevice.Present;
            end
            else if Assigned(FVideoController) then
            begin
              FVideoController.Print(Ch);
              FVideoController.Present;
            end;
          end;
        end;
      end
      else
      begin
        // Update screen to show blinking cursor while waiting for input
        if Assigned(FOutputDevice) then
          FOutputDevice.Present
        else if Assigned(FVideoController) then
          FVideoController.Present;
        SDL_Delay(10);
      end;
    end;
  finally
    DisableTextInput;
    // Hide cursor again after INPUT completes
    if Assigned(FOnCursorEnable) then
      FOnCursorEnable(False);
  end;

  Result := InputBuffer;
end;

function TProgramInputHandler.ShouldQuit: Boolean;
begin
  ProcessEvents;
  Result := FQuitRequested;
end;

function TProgramInputHandler.ShouldStop: Boolean;
begin
  ProcessEvents;
  Result := FStopRequested;
end;

procedure TProgramInputHandler.ClearStopRequest;
begin
  FStopRequested := False;
end;

procedure TProgramInputHandler.Reset;
begin
  FQuitRequested := False;
  FStopRequested := False;
  FLastChar := '';
  FHasChar := False;
  FShiftPressed := False;
  FCtrlPressed := False;
  FAltPressed := False;
end;

procedure TProgramInputHandler.SetOutputDevice(AOutputDevice: IOutputDevice);
begin
  FOutputDevice := AOutputDevice;
end;

procedure TProgramInputHandler.SetCursorCallback(ACallback: TCursorEnableCallback);
begin
  FOnCursorEnable := ACallback;
end;

end.
