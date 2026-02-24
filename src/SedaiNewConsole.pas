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
  SedaiGraphicsMemory, SedaiGraphicsPrimitives,
  SedaiProgramMemory, SedaiAST, SedaiParserTypes,
  SedaiCommandRouter, SedaiCommandTypes,
  SedaiSDL2VideoModes,
  SedaiBytecodeTypes, SedaiBytecodeVM, SedaiImmediateCompiler,
  SedaiBytecodeSerializer, SedaiBytecodeCompiler,
  SedaiBytecodeDisassembler,
  SedaiSSATypes, SedaiSSA,
  SedaiBasicKeywords, SedaiDebugger,
  SedaiMemoryMapper, SedaiC128MemoryMapper;

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
  { Forward declarations }
  TTextBuffer = class;

  { TVideoController - SDL2 video mode management with persistent graphics buffers }
  { NOTE: Using TObject instead of TInterfacedObject because interfaces use CORBA mode (no refcount) }
  TVideoController = class(TObject, IOutputDevice)
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
    procedure CleanupSDL;
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

    // Get SDL_Color from palette index (for border/text rendering)
    function PaletteIndexToSDLColor(Index: Byte): TSDL_Color;

  public
    // Public method for rendering graphics buffer to screen
    procedure RenderGraphicsTexture;
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

  public
    constructor Create;
    destructor Destroy; override;

    function Initialize(Mode: TGraphicMode; Fullscreen: Boolean = False): Boolean;
    procedure Run;
    procedure ProcessCommand(const Command: string);
    procedure RenderScreen;
    procedure UpdateCursor;
    function CheckCursorBlink: Boolean;  // Returns true if cursor state changed

    // Startup file support (passed via command line)
    procedure SetStartupFile(const AFileName: string; AAutoRun: Boolean);
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

constructor TVideoController.Create;
begin
  inherited Create;
  FWindow := nil;
  FRenderer := nil;
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
  inherited Destroy;
end;

function TVideoController.InitializeSDL: Boolean;
begin
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

procedure TVideoController.CleanupSDL;
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

  TTF_Quit;
  SDL_Quit;
  FSDLInitialized := False;
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
  ScaleX, ScaleY, Scale: Integer;
  ScaledWidth, ScaledHeight: Integer;
  BaseFontSize: Integer;
begin
  // Calculate base font size from native viewport and text grid
  // Font = NativeWidth / TextCols = NativeHeight / TextRows
  BaseFontSize := FModeInfo.NativeWidth div FModeInfo.TextCols;

  if FFullscreen then
  begin
    // In fullscreen, calculate integer scaling based on physical resolution
    ScaleX := FWindowWidth div FModeInfo.NativeWidth;
    ScaleY := FWindowHeight div FModeInfo.NativeHeight;
    Scale := Min(ScaleX, ScaleY);

    // Scale the font
    FFontSize := BaseFontSize * Scale;

    // Calculate scaled viewport dimensions
    ScaledWidth := FModeInfo.NativeWidth * Scale;
    ScaledHeight := FModeInfo.NativeHeight * Scale;

    // Center the scaled viewport in the physical window
    FViewportX := (FWindowWidth - ScaledWidth) div 2;
    FViewportY := (FWindowHeight - ScaledHeight) div 2;
    FViewportWidth := ScaledWidth;
    FViewportHeight := ScaledHeight;
  end
  else
  begin
    // In windowed mode, use native viewport and base font
    FFontSize := BaseFontSize;
    FViewportWidth := FModeInfo.NativeWidth;
    FViewportHeight := FModeInfo.NativeHeight;
    FViewportX := (FWindowWidth - FViewportWidth) div 2;
    FViewportY := (FWindowHeight - FViewportHeight) div 2;
  end;
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
begin
  CleanupSDL;
  Result := Initialize(Mode, FFullscreen);
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
    if FTextTextureDirty or BorderChanged or ATextBuffer.AllDirty then
    begin
      // Fill entire texture with border color
      SDL_SetRenderDrawColor(FRenderer, BorderColor.r, BorderColor.g, BorderColor.b, 255);
      SDL_RenderClear(FRenderer);

      // Fill viewport area with background color
      ViewportRect.x := FViewportX;
      ViewportRect.y := FViewportY;
      ViewportRect.w := FViewportWidth;
      ViewportRect.h := FViewportHeight;
      SDL_SetRenderDrawColor(FRenderer, ScreenBG.r, ScreenBG.g, ScreenBG.b, 255);
      SDL_RenderFillRect(FRenderer, @ViewportRect);

      // Render ALL cells (at viewport offset)
      RenderY := FViewportY;
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

  // Blit full texture to screen (every frame - fast operation)
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

  // Sync to texture and present
  SyncGraphicsBufferToTexture;
  RenderGraphicsTexture;
  Present;
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

  // Sync to texture and present
  SyncGraphicsBufferToTexture;
  RenderGraphicsTexture;
  Present;
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

  // Sync to texture and render
  RenderGraphicsTexture;
  Present;
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

    // Clip to screen bounds
    if (CurrX >= 0) and (CurrX < FModeInfo.NativeWidth) and
       (CurrY >= 0) and (CurrY < FModeInfo.NativeHeight) then
    begin
      // Draw line from previous point to current point (Bresenham's algorithm)
      if not FirstPoint then
        DrawLineInternal(PrevX, PrevY, CurrX, CurrY, UseIndex, ActualIndex, Color)
      else
      begin
        // First point - plot with line width
        if UseIndex then
          SedaiGraphicsPrimitives.DrawThickPixel(FGraphicsMemory, CurrX, CurrY,
            ActualIndex, True, FLineWidth, FModeInfo.NativeWidth, FModeInfo.NativeHeight)
        else
          SedaiGraphicsPrimitives.DrawThickPixel(FGraphicsMemory, CurrX, CurrY,
            Color, False, FLineWidth, FModeInfo.NativeWidth, FModeInfo.NativeHeight);
      end;
    end;

    PrevX := CurrX;
    PrevY := CurrY;
    FirstPoint := False;
    CurrentAngle := CurrentAngle + Inc;
  end;

  // Sync to texture and render
  RenderGraphicsTexture;
  Present;
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

  RenderGraphicsTexture;
  Present;
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
  DestRect: TSDL_Rect;
begin
  if FGraphicsTexture = nil then Exit;

  // Sync buffer to texture
  SyncGraphicsBufferToTexture;

  // Calculate destination rectangle (scaled to viewport)
  DestRect.x := FViewportX;
  DestRect.y := FViewportY;
  DestRect.w := FViewportWidth;
  DestRect.h := FViewportHeight;

  // Render the texture
  SDL_RenderCopy(FRenderer, FGraphicsTexture, nil, @DestRect);
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

    // === Ignored PETSCII codes (cursor movement - not applicable in shell mode) ===
    17, 145: Exit;  // Cursor down/up
    29, 157: Exit;  // Cursor right/left
    19: Exit;       // Home (CLR/HOME without SHIFT)
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
begin
  // Delegate to VideoController
  if Assigned(FVideoController) then
    Result := FVideoController.SetGraphicMode(Mode, ClearBuffer, SplitLine)
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
  FRunning := False;
  FCursorVisible := True;
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
  // Use dedicated program input handler for VM (NOT the console input handler)
  FBytecodeVM.SetInputDevice(FProgramInputHandler);
  // Create and set memory mapper for PEEK/POKE support
  FMemoryMapper := TC128MemoryMapper.Create(FVideoController, nil);
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
  // In graphics mode, render the graphics texture and present
  if FVideoController.IsInGraphicsMode then
  begin
    // Clear and render the graphics buffer to screen
    FVideoController.ClearBorder;
    FVideoController.RenderGraphicsTexture;
    FVideoController.Present;
    Exit;
  end;

  // Store cursor position
  PrevCursorX := FTextBuffer.CursorX;
  PrevCursorY := FTextBuffer.CursorY;

  // RenderText blits full-window texture (includes border) - no need for ClearBorder
  FVideoController.RenderText(FTextBuffer);

  // Restore cursor position
  FTextBuffer.CursorX := PrevCursorX;
  FTextBuffer.CursorY := PrevCursorY;

  // Draw blinking cursor (always update during render for INPUT)
  UpdateCursor;
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
                      // ?SYNTAX ERROR IN <line> (<line>:<col>)
                      if FImmediateCompiler.LastErrorLine > 0 then
                      begin
                        if FImmediateCompiler.LastErrorColumn > 0 then
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
                    // ?SYNTAX ERROR IN <line> (<line>:<col>)
                    // Note: LastErrorVerbose preserved for future OPTION command
                    if FImmediateCompiler.LastErrorLine > 0 then
                    begin
                      if FImmediateCompiler.LastErrorColumn > 0 then
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
begin
  ErrorCode := 0;

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
      Result := True;
      Exit;
    end;
    if FProgramInputHandler.StopRequested then
    begin
      Result := True;
      Exit;
    end;
  end;

  // Periodic rendering (~60 FPS)
  Now := SDL_GetTicks;
  if (Now - FLastVMRenderTick) >= 16 then
  begin
    FLastVMRenderTick := Now;
    if FTextBuffer.NeedsRedraw then
    begin
      RenderScreen;
      UpdateCursor;
      FVideoController.Present;
    end;
  end;
end;

procedure TSedaiNewConsole.BeginVMExecution;
begin
  FLastVMRenderTick := SDL_GetTicks;
  FBytecodeVM.EventPollCallback := @HandleVMEventPoll;
end;

procedure TSedaiNewConsole.EndVMExecution;
begin
  FBytecodeVM.EventPollCallback := nil;
  // Final render to show the last state
  if FTextBuffer.NeedsRedraw then
  begin
    RenderScreen;
    UpdateCursor;
    FVideoController.Present;
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
    else
    begin
      // Load BASIC source
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
      FVideoController.ToggleFullscreen;

      // Re-render after mode change
      RenderScreen;
      UpdateCursor;
      FVideoController.Present;

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

        // CTRL+END or ESC: Stop BASIC program (BREAK)
        if ((KeySym = SDLK_END) and FCtrlPressed and (not FAltPressed)) or
           (KeySym = SDLK_ESCAPE) then
        begin
          FStopRequested := True;
          // Also set character for ESC so program can detect it
          if KeySym = SDLK_ESCAPE then
          begin
            FLastChar := #27;
            FHasChar := True;
          end;
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
            // Only if not CTRL (CTRL+END is stop program)
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

end.
