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
  SedaiGraphicsMemory,
  SedaiProgramMemory, SedaiAST, SedaiParserTypes,
  SedaiCommandRouter, SedaiCommandTypes,
  SedaiSDL2VideoModes,
  SedaiBytecodeTypes, SedaiBytecodeVM, SedaiImmediateCompiler,
  SedaiBytecodeSerializer, SedaiBytecodeCompiler,
  SedaiBytecodeDisassembler,
  SedaiSSATypes, SedaiSSA,
  SedaiBasicKeywords, SedaiDebugger;

const
  SCROLLBACK_LINES = 1000;
  INPUT_HISTORY_SIZE = 100;
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
    procedure SetPixel(X, Y: Integer; RGB: UInt32); overload;
    procedure SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex); overload;
    function GetPixel(X, Y: Integer): UInt32;
    procedure EnablePalette(Enable: Boolean);
    function IsPaletteEnabled: Boolean;
    procedure SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
    function GetPaletteColor(Index: TPaletteIndex): UInt32;
    procedure ResetPalette;

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
    function GetColorSource(Source: Integer): Integer;
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

  { TTextCell - Single character cell with color attributes }
  TTextCell = record
    Ch: Char;           // The character
    FGIndex: Byte;      // Foreground color (palette index)
    BGIndex: Byte;      // Background color (palette index)
    Reverse: Boolean;   // Reverse video flag for this cell
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

    procedure AddToScrollback(Row: Integer);
    function CellsToString(Row: Integer): string;  // Convert cell row to string (for scrollback)

  public
    constructor Create(Rows, Cols: Integer);
    destructor Destroy; override;

    procedure Clear;
    procedure PutChar(Ch: Char);
    procedure InsertChar(Ch: Char);  // Insert mode: shifts chars right
    procedure PutString(const S: string);
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

    // Scrollback navigation
    procedure ScrollViewUp(NumLines: Integer = 1);
    procedure ScrollViewDown(NumLines: Integer = 1);
    procedure ScrollToEnd;
    procedure ScrollToStart;
    function GetViewLine(Index: Integer): string;  // Get line considering view offset
    function IsInScrollbackMode: Boolean;

    // Current text color properties
    property CurrentFGIndex: Byte read FCurrentFGIndex write FCurrentFGIndex;
    property CurrentBGIndex: Byte read FCurrentBGIndex write FCurrentBGIndex;
    property CurrentReverse: Boolean read FCurrentReverse write FCurrentReverse;

    property CursorX: Integer read FCursorX write FCursorX;
    property CursorY: Integer read FCursorY write FCursorY;
    property Rows: Integer read FRows;
    property Cols: Integer read FCols;
    property ViewOffset: Integer read FViewOffset;
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
  public
    constructor Create(ATextBuffer: TTextBuffer; AVideoController: TVideoController);
    procedure SetRenderCallback(AProc: TRenderScreenProc);
    
    // IOutputDevice implementation
    function Initialize(const Title: string = ''; Width: Integer = 80; Height: Integer = 25): Boolean;
    procedure Shutdown;
    function IsInitialized: Boolean;
    procedure Print(const Text: string; ClearBackground: Boolean = False);
    procedure PrintLn(const Text: string; ClearBackground: Boolean = False);
    procedure NewLine;
    procedure Clear;
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
    procedure SetPixel(X, Y: Integer; RGB: UInt32); overload;
    procedure SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex); overload;
    function GetPixel(X, Y: Integer): UInt32;
    procedure EnablePalette(Enable: Boolean);
    function IsPaletteEnabled: Boolean;
    procedure SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
    function GetPaletteColor(Index: TPaletteIndex): UInt32;
    procedure ResetPalette;

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
    function GetColorSource(Source: Integer): Integer;
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
    FConsoleOutputAdapter: TConsoleOutputAdapter;
    FRunning: Boolean;
    FCursorVisible: Boolean;
    FLastCursorBlink: Cardinal;
    FInputHistory: array of string;
    FHistoryCount: Integer;
    FHistoryPos: Integer;
    FCtrlCEnabled: Boolean;  // If True, CTRL+C exits from the console

    // Components for command management
    FProgramMemory: TProgramMemory;
    FCommandRouter: TCommandRouter;

    // VM-based execution (replaces TSedaiExecutor)
    FBytecodeVM: TBytecodeVM;
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

    // Debug mode (TRON/TROFF)
    FDebugMode: Boolean;          // True = compile with debug info, trace execution
    FTraceActive: Boolean;        // True = currently tracing (runtime flag)
    FDebugger: TSedaiDebugger;    // Debugger instance for breakpoints and stepping

    // Error tracking (EL, ER, ERR$)
    FLastErrorLine: Integer;      // EL - line number where last error occurred (0 = none)
    FLastErrorCode: Integer;      // ER - error code of last error (0 = none)
    FLastErrorMessage: string;    // ERR$ - error message of last error

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
    procedure ExecuteScratch(const Pattern: string; Force: Boolean);
    procedure ExecuteRename(const OldName, NewName: string);
    procedure ExecuteVerify(const Filename: string);
    procedure ExecuteBLoad(const Filename: string);
    procedure ExecuteBSave(const Filename: string);
    function AskYesNo(const Prompt: string): Boolean;
    function AskYesNoAll(const Prompt: string; var OverwriteAll: Boolean): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function Initialize(Mode: TGraphicMode; Fullscreen: Boolean = False): Boolean;
    procedure Run;
    procedure ProcessCommand(const Command: string);
    procedure RenderScreen;
    procedure UpdateCursor;

    // Startup file support (passed via command line)
    procedure SetStartupFile(const AFileName: string; AAutoRun: Boolean);

    property CtrlCEnabled: Boolean read FCtrlCEnabled write FCtrlCEnabled;
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

  // FAST mode (C128 2MHz mode emulation) - default off, alpha 255 (opaque)
  FFastMode := False;
  FFastModeAlpha := 255;

  // COLOR command - initialize color sources to default palette colors
  FColorSources[0] := 12;  // Background (dark grey)
  FColorSources[1] := 13;  // Foreground (light green)
  FColorSources[2] := 1;   // Multicolor 1 (white)
  FColorSources[3] := 2;   // Multicolor 2 (red)
  FColorSources[4] := 13;  // Border (light green)
  FColorSources[5] := 13;  // Character color (light green)
  FColorSources[6] := 12;  // 80-column background

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
function TVideoController.PaletteIndexToSDLColor(Index: Byte): TSDL_Color;
var
  PaletteColor: UInt32;
begin
  if FGraphicsMemory <> nil then
  begin
    PaletteColor := FGraphicsMemory.Palette[Index];
    // Palette format is $AABBGGRR (see sedaigraphicsconfig.pas)
    Result.r := PaletteColor and $FF;
    Result.g := (PaletteColor shr 8) and $FF;
    Result.b := (PaletteColor shr 16) and $FF;
    Result.a := 255;
  end
  else
  begin
    // Fallback to black if no graphics memory
    Result.r := 0;
    Result.g := 0;
    Result.b := 0;
    Result.a := 255;
  end;
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

  // DEBUG: Verify actual window resolution
  SDL_GetWindowSize(FWindow, @ActualW, @ActualH);
  WriteLn('DEBUG CreateWindow: Requested=', RequestedW, 'x', RequestedH,
          ' Selected=', FWindowWidth, 'x', FWindowHeight,
          ' Actual=', ActualW, 'x', ActualH,
          ' Fullscreen=', FFullscreen);

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

    WriteLn('DEBUG CalculateViewport (Fullscreen): Window=', FWindowWidth, 'x', FWindowHeight,
            ' ScaleX=', ScaleX, ' ScaleY=', ScaleY, ' → Scale=', Scale,
            ' FontSize=', FFontSize,
            ' Viewport=', FViewportWidth, 'x', FViewportHeight,
            ' Position=(', FViewportX, ',', FViewportY, ')');
  end
  else
  begin
    // In windowed mode, use native viewport and base font
    FFontSize := BaseFontSize;
    FViewportWidth := FModeInfo.NativeWidth;
    FViewportHeight := FModeInfo.NativeHeight;
    FViewportX := (FWindowWidth - FViewportWidth) div 2;
    FViewportY := (FWindowHeight - FViewportHeight) div 2;
    WriteLn('DEBUG CalculateViewport (Windowed): Window=', FWindowWidth, 'x', FWindowHeight,
            ' Viewport=', FViewportWidth, 'x', FViewportHeight,
            ' Position=(', FViewportX, ',', FViewportY, ')');
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

  ClearBorder;
  
  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Initialization complete');{$ENDIF}
  FInitialized := True;

  // DEBUG: Print window, viewport and font information
  WriteLn('=== DEBUG INFO ===');
  WriteLn('Window Resolution: ', FWindowWidth, 'x', FWindowHeight);
  WriteLn('Viewport Resolution: ', FViewportWidth, 'x', FViewportHeight);
  WriteLn('Viewport Position: (', FViewportX, ', ', FViewportY, ')');
  WriteLn('Font Size Requested: ', FFontSize);
  WriteLn('Font Metrics: CharWidth=', FCharWidth, ' CharHeight=', FCharHeight);
  WriteLn('Expected Grid: 80x50 chars');
  WriteLn('Calculated Grid: ', FViewportWidth div FCharWidth, 'x', FViewportHeight div FCharHeight, ' chars');
  WriteLn('==================');

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
  // Fills the viewport with the screen color (not the drawing background!)
  ViewportRect.x := FViewportX;
  ViewportRect.y := FViewportY;
  ViewportRect.w := FViewportWidth;
  ViewportRect.h := FViewportHeight;

  // Use palette index for classic modes, direct SDL2 color for mode 11
  if FCurrentMode = gmSDL2Dynamic then
    LocalBGColor := FBGColorSDL2
  else
    LocalBGColor := PaletteIndexToSDLColor(FScreenColorIndex);

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
  Texture: PSDL_Texture;
  DestRect, CellBGRect: TSDL_Rect;
  CellTextColor, CellBGColor: TSDL_Color;
  CharStr: string;
  EffectiveFG, EffectiveBG: Byte;
begin
  if not Assigned(ATextBuffer) or not Assigned(FFont) then Exit;

  // Clears the viewport with screen color
  ClearViewport;

  // Render each cell with its own colors
  RenderY := FViewportY;
  for Row := 0 to ATextBuffer.Rows - 1 do
  begin
    RenderX := FViewportX;
    for Col := 0 to ATextBuffer.Cols - 1 do
    begin
      Cell := ATextBuffer.GetCell(Row, Col);

      // Skip spaces with default background (optimization)
      if Cell.Ch = ' ' then
      begin
        RenderX := RenderX + FCharWidth;
        Continue;
      end;

      // Get effective colors (swap if reverse)
      if Cell.Reverse then
      begin
        EffectiveFG := Cell.BGIndex;
        EffectiveBG := Cell.FGIndex;
      end
      else
      begin
        EffectiveFG := Cell.FGIndex;
        EffectiveBG := Cell.BGIndex;
      end;

      // Get SDL colors
      if FCurrentMode = gmSDL2Dynamic then
      begin
        CellTextColor := FFGColorSDL2;
        CellBGColor := FBGColorSDL2;
      end
      else
      begin
        CellTextColor := PaletteIndexToSDLColor(EffectiveFG);
        CellBGColor := PaletteIndexToSDLColor(EffectiveBG);
      end;

      // Draw background if different from screen color
      if EffectiveBG <> FScreenColorIndex then
      begin
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
          Texture := SDL_CreateTextureFromSurface(FRenderer, Surface);
          if Assigned(Texture) then
          begin
            try
              DestRect.x := RenderX;
              DestRect.y := RenderY;
              DestRect.w := Surface^.w;
              DestRect.h := Surface^.h;
              SDL_RenderCopy(FRenderer, Texture, nil, @DestRect);
            finally
              SDL_DestroyTexture(Texture);
            end;
          end;
        finally
          SDL_FreeSurface(Surface);
        end;
      end;

      RenderX := RenderX + FCharWidth;
    end;

    RenderY := RenderY + FCharHeight;
  end;
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
begin
  if (Source >= 0) and (Source <= 6) then
  begin
    FColorSources[Source] := Color;
    // Update corresponding palette indices based on source
    case Source of
      0: FBackgroundIndex := Byte(Color);    // Background
      1: FForegroundIndex := Byte(Color);    // Foreground
      2: FMulticolorIndex1 := Byte(Color);   // Multicolor 1
      3: FMulticolorIndex2 := Byte(Color);   // Multicolor 2
      4: FBorderColorIndex := Byte(Color);   // Border
      5: FTextColorIndex := Byte(Color);     // Character color
      6: FScreenColorIndex := Byte(Color);   // 80-column background
    end;
  end;
end;

function TVideoController.GetColorSource(Source: Integer): Integer;
begin
  if (Source >= 0) and (Source <= 6) then
    Result := FColorSources[Source]
  else
    Result := 0;
end;

// === WIDTH command support ===
procedure TVideoController.SetLineWidth(Width: Integer);
begin
  if (Width >= 1) and (Width <= 2) then
    FLineWidth := Width
  else
    FLineWidth := 1;
end;

// === SCALE command support ===
procedure TVideoController.SetScale(Enabled: Boolean; XMax, YMax: Integer);
begin
  FScaleEnabled := Enabled;
  if Enabled and (XMax > 0) and (YMax > 0) then
  begin
    FScaleXMax := XMax;
    FScaleYMax := YMax;
  end
  else if not Enabled then
  begin
    // When disabled, reset to mode defaults
    FScaleXMax := FModeInfo.NativeWidth;
    FScaleYMax := FModeInfo.NativeHeight;
  end;
end;

// === PAINT command support (flood fill) ===
procedure TVideoController.FloodFill(Source: Integer; X, Y: Double; Mode: Integer);
var
  PixelX, PixelY: Integer;
  TargetColor, FillColor: UInt32;
  Stack: array of TPoint;
  StackSize, StackCapacity: Integer;
  CurX, CurY: Integer;
  BufWidth, BufHeight: Integer;
  PixelPtr: PByte;

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

  // Get fill color from source
  if (Source >= 0) and (Source <= 6) then
    FillColor := FPaletteManager.GetColor(FColorSources[Source])
  else
    FillColor := FPaletteManager.GetColor(FForegroundIndex);

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

    // Set pixel
    FGraphicsMemory.SetPixelRGBA(CurX, CurY, FillColor);

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

  WriteLn('>>> SetGraphicMode CALLED: Mode=', Ord(Mode), ' ClearBuffer=', ClearBuffer, ' SplitLine=', SplitLine);

  // Get mode info
  NewModeInfo := GRAPHICS_MODES[Mode];
  WriteLn('>>> Mode info: Name=', NewModeInfo.Name, ' NativeRes=', NewModeInfo.NativeWidth, 'x', NewModeInfo.NativeHeight);

  // Use user-provided SplitLine, or DefaultSplitLine if -1
  if SplitLine < 0 then
    FSplitLine := NewModeInfo.DefaultSplitLine
  else
    FSplitLine := SplitLine;

  // Classic modes use palette indices, mode 11 uses RGBA
  // FForegroundIndex/FBackgroundIndex are set to defaults (13/12)
  // and can be changed via COLOR command
  WriteLn('>>> Hires indices: FG=', FForegroundIndex, ' BG=', FBackgroundIndex);

  // Update mode info
  FCurrentMode := Mode;
  FModeInfo := NewModeInfo;
  WriteLn('>>> FCurrentMode set to ', Ord(FCurrentMode));

  // For classic modes (0-10), use persistent buffers
  if Mode <> gmSDL2Dynamic then
  begin
    WriteLn('>>> Allocating buffers for classic mode...');
    // Allocate or switch to the buffer for this mode
    // AllocateBuffers returns True if buffer was newly created
    PaletteMode := NewModeInfo.PaletteType = ptC64_16;
    BufferIsNew := FGraphicsMemory.AllocateBuffers(NewModeInfo.NativeWidth, NewModeInfo.NativeHeight, PaletteMode, Mode);
    WriteLn('>>> Buffers allocated. GraphicsBuffer=', IntToHex(PtrUInt(FGraphicsMemory.GraphicsBuffer), 16), ' BufferIsNew=', BufferIsNew);

    // Clear the buffer if:
    // 1. Explicitly requested (ClearBuffer=True), OR
    // 2. Buffer was just created (first time entering this mode)
    if ClearBuffer or BufferIsNew then
    begin
      WriteLn('>>> Clearing buffer with BG index ', FBackgroundIndex, ' (ClearBuffer=', ClearBuffer, ', BufferIsNew=', BufferIsNew, ')');
      FGraphicsMemory.ClearCurrentModeWithIndex(FBackgroundIndex);
    end;

    // Create/recreate texture for this resolution
    WriteLn('>>> Creating graphics texture...');
    CreateGraphicsTexture;
    WriteLn('>>> Texture created: ', FGraphicsTexture <> nil);
  end;

  // Clear the border
  WriteLn('>>> Clearing border...');
  ClearBorder;

  // In graphics mode, render the graphics buffer; in text mode, clear viewport
  WriteLn('>>> IsInGraphicsMode=', IsInGraphicsMode);
  if IsInGraphicsMode then
  begin
    WriteLn('>>> Rendering graphics texture...');
    RenderGraphicsTexture;
  end
  else
  begin
    WriteLn('>>> Clearing viewport (text mode)...');
    ClearViewport;
  end;

  WriteLn('>>> Presenting...');
  Present;
  WriteLn('>>> SetGraphicMode DONE');
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

procedure TVideoController.SetPixel(X, Y: Integer; RGB: UInt32);
begin
  if FGraphicsMemory <> nil then
    FGraphicsMemory.SetPixel(X, Y, RGB);
end;

procedure TVideoController.SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex);
begin
  if FGraphicsMemory <> nil then
    FGraphicsMemory.SetPixel(X, Y, PaletteIndex);
end;

function TVideoController.GetPixel(X, Y: Integer): UInt32;
begin
  if FGraphicsMemory <> nil then
    Result := FGraphicsMemory.GetPixel(X, Y)
  else
    Result := 0;
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
    FGraphicsMemory.SetPaletteColor(Index, RGB);
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
    FGraphicsMemory.ResetPalette;
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
begin
  WriteLn('>>> DrawBoxWithColor: X1=', X1, ' Y1=', Y1, ' X2=', X2, ' Y2=', Y2, ' Color=', Color, ' Filled=', Filled);
  WriteLn('>>> DrawBoxWithColor: ModeInfo.NativeWidth=', FModeInfo.NativeWidth, ' NativeHeight=', FModeInfo.NativeHeight);
  if FGraphicsMemory = nil then Exit;

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
    WriteLn('>>> DrawBoxWithColor: Classic mode, Color=', Color, ' ActualIndex=', ActualIndex,
            ' (FG=', FForegroundIndex, ' BG=', FBackgroundIndex,
            ' MC1=', FMulticolorIndex1, ' MC2=', FMulticolorIndex2, ')');
  end;

  // Normalize coordinates
  if X1 < X2 then begin MinX := X1; MaxX := X2; end
  else begin MinX := X2; MaxX := X1; end;
  if Y1 < Y2 then begin MinY := Y1; MaxY := Y2; end
  else begin MinY := Y2; MaxY := Y1; end;

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
begin
  if FGraphicsMemory = nil then Exit;

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
    // Calculate point on ellipse (angles in degrees, convert to radians)
    Px := XR * Cos(CurrentAngle * Pi / 180.0);
    Py := YR * Sin(CurrentAngle * Pi / 180.0);

    // Apply rotation
    Rx := Px * CosRot - Py * SinRot;
    Ry := Px * SinRot + Py * CosRot;

    // Translate to center
    CurrX := X + Round(Rx);
    CurrY := Y + Round(Ry);

    // Clip to screen bounds
    if (CurrX >= 0) and (CurrX < FModeInfo.NativeWidth) and
       (CurrY >= 0) and (CurrY < FModeInfo.NativeHeight) then
    begin
      // Draw line from previous point to current point (Bresenham's algorithm)
      if not FirstPoint then
        DrawLineInternal(PrevX, PrevY, CurrX, CurrY, UseIndex, ActualIndex, Color)
      else
      begin
        // First point - just plot the pixel
        if UseIndex then
          FGraphicsMemory.SetPixel(CurrX, CurrY, ActualIndex)
        else
          FGraphicsMemory.SetPixel(CurrX, CurrY, Color);
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
begin
  // Bresenham's line algorithm
  Dx := Abs(X2 - X1);
  Dy := Abs(Y2 - Y1);
  if X1 < X2 then Sx := 1 else Sx := -1;
  if Y1 < Y2 then Sy := 1 else Sy := -1;
  Err := Dx - Dy;

  while True do
  begin
    // Plot pixel if in bounds
    if (X1 >= 0) and (X1 < FModeInfo.NativeWidth) and
       (Y1 >= 0) and (Y1 < FModeInfo.NativeHeight) then
    begin
      if UseIndex then
        FGraphicsMemory.SetPixel(X1, Y1, PalIndex)
      else
        FGraphicsMemory.SetPixel(X1, Y1, RGBAColor);
    end;

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
begin
  // Use DrawLineInternal with RGBA color
  DrawLineInternal(X1, Y1, X2, Y2, False, 0, Color);
end;

procedure TVideoController.SetPixelCursor(X, Y: Integer);
begin
  FPixelCursorX := X;
  FPixelCursorY := Y;
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
  SCROLLBACK_CAPACITY = 2500;  // 100 pages @25 rows or 50 pages @50 rows
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
      FCells[r][c].BGIndex := DEFAULT_BG_INDEX;
      FCells[r][c].Reverse := False;
    end;

  FCursorX := 0;
  FCursorY := 0;

  // Default text colors
  FCurrentFGIndex := DEFAULT_FG_INDEX;
  FCurrentBGIndex := DEFAULT_BG_INDEX;
  FCurrentReverse := False;

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
  FCursorX := 0;
  FCursorY := 0;
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

procedure TTextBuffer.PutChar(Ch: Char);
begin
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
    // Write character with current color attributes
    FCells[FCursorY][FCursorX].Ch := Ch;
    FCells[FCursorY][FCursorX].FGIndex := FCurrentFGIndex;
    FCells[FCursorY][FCursorX].BGIndex := FCurrentBGIndex;
    FCells[FCursorY][FCursorX].Reverse := FCurrentReverse;
    Inc(FCursorX);
  end;

  if FCursorX >= FCols then
    NewLine;
end;

procedure TTextBuffer.PutString(const S: string);
var
  i: Integer;
begin
  for i := 1 to Length(S) do
    PutChar(S[i]);
end;

procedure TTextBuffer.NewLine;
begin
  FCursorX := 0;
  Inc(FCursorY);
  if FCursorY >= FRows then
  begin
    ScrollUp;
    FCursorY := FRows - 1;
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
    // Shift cells left from cursor position
    for c := FCursorX to FCols - 2 do
      FCells[FCursorY][c] := FCells[FCursorY][c + 1];
    // Clear last cell
    FCells[FCursorY][FCols - 1].Ch := ' ';
    FCells[FCursorY][FCols - 1].FGIndex := FCurrentFGIndex;
    FCells[FCursorY][FCols - 1].BGIndex := FCurrentBGIndex;
    FCells[FCursorY][FCols - 1].Reverse := FCurrentReverse;
  end;
end;

procedure TTextBuffer.DeleteCharAtCursor;
var
  c: Integer;
begin
  // Delete key: delete character AT cursor and shift left
  // Shift cells left from cursor position
  for c := FCursorX to FCols - 2 do
    FCells[FCursorY][c] := FCells[FCursorY][c + 1];
  // Clear last cell
  FCells[FCursorY][FCols - 1].Ch := ' ';
  FCells[FCursorY][FCols - 1].FGIndex := FCurrentFGIndex;
  FCells[FCursorY][FCols - 1].BGIndex := FCurrentBGIndex;
  FCells[FCursorY][FCols - 1].Reverse := FCurrentReverse;
end;

procedure TTextBuffer.ScrollUp;
var
  r, c: Integer;
begin
  // Save the top line to scrollback buffer before scrolling
  // Ring buffer handles overflow automatically (O(1) operation)
  {$IFDEF DEBUG_CONSOLE}
  WriteLn('[DEBUG] ScrollUp: Adding line to scrollback: "', CellsToString(0), '" Count before=', FScrollbackBuffer.Count);
  {$ENDIF}
  AddToScrollback(0);
  {$IFDEF DEBUG_CONSOLE}
  WriteLn('[DEBUG] ScrollUp: Count after=', FScrollbackBuffer.Count);
  {$ENDIF}

  // Scroll up - move all rows up by one
  for r := 0 to FRows - 2 do
    for c := 0 to FCols - 1 do
      FCells[r][c] := FCells[r + 1][c];

  // Clear the bottom row
  for c := 0 to FCols - 1 do
  begin
    FCells[FRows - 1][c].Ch := ' ';
    FCells[FRows - 1][c].FGIndex := FCurrentFGIndex;
    FCells[FRows - 1][c].BGIndex := FCurrentBGIndex;
    FCells[FRows - 1][c].Reverse := FCurrentReverse;
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
    // Shift cells right from cursor position
    for c := FCols - 1 downto FCursorX + 1 do
      FCells[FCursorY][c] := FCells[FCursorY][c - 1];

    // Insert character at cursor position with current colors
    FCells[FCursorY][FCursorX].Ch := Ch;
    FCells[FCursorY][FCursorX].FGIndex := FCurrentFGIndex;
    FCells[FCursorY][FCursorX].BGIndex := FCurrentBGIndex;
    FCells[FCursorY][FCursorX].Reverse := FCurrentReverse;
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
end;

procedure TTextBuffer.ScrollViewDown(NumLines: Integer);
begin
  FViewOffset := FViewOffset - NumLines;
  if FViewOffset < 0 then
    FViewOffset := 0;
end;

procedure TTextBuffer.ScrollToEnd;
begin
  FViewOffset := 0;
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
            FStopRequested := True;
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
end;

procedure TConsoleOutputAdapter.SetRenderCallback(AProc: TRenderScreenProc);
begin
  FRenderScreenProc := AProc;
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
end;

procedure TConsoleOutputAdapter.PrintLn(const Text: string; ClearBackground: Boolean);
begin
  FTextBuffer.PutString(Text);
  FTextBuffer.NewLine;
  // Auto-present after newline for immediate display (like classic BASICs)
  Present;
end;

procedure TConsoleOutputAdapter.NewLine;
begin
  FTextBuffer.NewLine;
  // Auto-present after newline for immediate display (like classic BASICs)
  Present;
end;

procedure TConsoleOutputAdapter.Clear;
begin
  FTextBuffer.Clear;
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
  WriteLn('>>> TConsoleOutputAdapter.SetGraphicMode: Mode=', Ord(Mode), ' FVideoController=', Assigned(FVideoController));
  // Delegate to VideoController
  if Assigned(FVideoController) then
    Result := FVideoController.SetGraphicMode(Mode, ClearBuffer, SplitLine)
  else
    Result := False;
  WriteLn('>>> TConsoleOutputAdapter.SetGraphicMode result=', Result);
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
    FVideoController.SetColorSource(Source, Color);
end;

function TConsoleOutputAdapter.GetColorSource(Source: Integer): Integer;
begin
  if Assigned(FVideoController) then
    Result := FVideoController.GetColorSource(Source)
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
  if Assigned(FVideoController) then
    FVideoController.SetWindow(Col1, Row1, Col2, Row2, DoClear);
end;

function TConsoleOutputAdapter.GetWindowLines: Integer;
begin
  if Assigned(FVideoController) then
    Result := FVideoController.GetWindowLines
  else
    Result := 25;
end;

function TConsoleOutputAdapter.GetWindowCols: Integer;
begin
  if Assigned(FVideoController) then
    Result := FVideoController.GetWindowCols
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
begin
  inherited Create;
  FVideoController := TVideoController.Create;
  FTextBuffer := TTextBuffer.Create(25, 40);  // Create TextBuffer immediately
  FGraphicEngine := nil;
  FInputHandler := TInputHandler.Create(FTextBuffer, FVideoController, Self);  // Passa i riferimenti + console
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
  FBytecodeVM.SetInputDevice(FInputHandler);

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
end;

destructor TSedaiNewConsole.Destroy;
begin
  // Free debugger first (before VM)
  if Assigned(FDebugger) then
  begin
    FDebugger.Free;
    FDebugger := nil;
  end;

  // Free VM components first
  if Assigned(FBytecodeVM) then
  begin
    FBytecodeVM.Free;
    FBytecodeVM := nil;
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

  // Other components
  if Assigned(FConsoleOutputAdapter) then
  begin
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

  if Assigned(FVideoController) then
  begin
    FVideoController.Free;
    FVideoController := nil;
  end;

  inherited Destroy;
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
    FTextBuffer.PutString('READY.');
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
  FTextBuffer.PutString('READY.');
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

  // Connect I/O devices to VM
  if Assigned(FBytecodeVM) then
  begin
    // VM uses console output adapter to write to TextBuffer
    FBytecodeVM.SetOutputDevice(FConsoleOutputAdapter);
    FBytecodeVM.SetInputDevice(FInputHandler);
    {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: BytecodeVM configured with ConsoleOutputAdapter');{$ENDIF}
  end;

  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: SedaiNewConsole initialization complete');{$ENDIF}
  Result := True;
end;

procedure TSedaiNewConsole.RenderScreen;
var
  i, RenderX, RenderY: Integer;
  Line: string;
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

  // Prima disegna il bordo (finestra con colore FG)
  FVideoController.ClearBorder;

  // Poi disegna il viewport (con colore BG)
  FVideoController.ClearViewport;

  // Renderizza il testo nel viewport
  RenderX := FVideoController.ViewportX;
  RenderY := FVideoController.ViewportY;

  for i := 0 to FTextBuffer.Rows - 1 do
  begin
    Line := FTextBuffer.GetViewLine(i);
    if Line <> '' then
      FGraphicEngine.DrawText(RenderX, RenderY, Line, FVideoController.FGColor);
    RenderY := RenderY + FVideoController.CharHeight;
  end;

  // Restore cursor position
  FTextBuffer.CursorX := PrevCursorX;
  FTextBuffer.CursorY := PrevCursorY;
end;

procedure TSedaiNewConsole.UpdateCursor;
var
  CurrentTime: Cardinal;
  CursorX, CursorY: Integer;
  CurrentChar: Char;
begin
  // Don't show cursor in bitmap graphics mode
  if FVideoController.ModeInfo.ModeType = mtBitmap then
    Exit;

  // Don't show cursor when viewing scrollback
  if FTextBuffer.IsInScrollbackMode then
    Exit;

  CurrentTime := SDL_GetTicks;

  // Toggle cursor visibility every 500ms (C64 timing)
  if CurrentTime - FLastCursorBlink >= CURSOR_BLINK_MS then
  begin
    FCursorVisible := not FCursorVisible;
    FLastCursorBlink := CurrentTime;
  end;

  if FCursorVisible then
  begin
    // Calculate cursor position in viewport
    CursorX := FVideoController.ViewportX + (FTextBuffer.CursorX * FVideoController.CharWidth);
    CursorY := FVideoController.ViewportY + (FTextBuffer.CursorY * FVideoController.CharHeight);

    // Get character at cursor position (space if beyond line end)
    CurrentChar := FTextBuffer.GetCharAtCursor;

    // Draw cursor as filled rectangle with FG color (reverse video background)
    FGraphicEngine.FillRect(CursorX, CursorY,
                           FVideoController.CharWidth,
                           FVideoController.CharHeight,
                           FVideoController.FGColor);

    // Draw the character in reverse (BG color on FG background)
    FGraphicEngine.DrawChar(CursorX, CursorY, CurrentChar, FVideoController.BGColor);
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
  ScratchForce: Boolean;
  RenameOld, RenameNew: string;
begin
  TrimmedCmd := Trim(Command);

  if TrimmedCmd = '' then
  begin
    // Empty command: no action, cursor already at new line
    Exit;
  end;

  // Use CommandRouter to determine command type
  // DEBUG: show the command in clear text before parsing
  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Processing raw command: ', TrimmedCmd);{$ENDIF}
  ParseResult := FCommandRouter.ParseInput(TrimmedCmd);
  
  // Add to history ONLY if it's not a program line
  if ParseResult.CommandType <> ctProgramLine then
  begin
    if FHistoryCount < INPUT_HISTORY_SIZE then
    begin
      FInputHistory[FHistoryCount] := Command;
      Inc(FHistoryCount);
    end
    else
    begin
      // Shift history
      Move(FInputHistory[1], FInputHistory[0], (INPUT_HISTORY_SIZE - 1) * SizeOf(string));
      FInputHistory[INPUT_HISTORY_SIZE - 1] := Command;
    end;
  end;
  FHistoryPos := -1;
  {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Command type: ', GetEnumName(TypeInfo(TCommandType), Ord(ParseResult.CommandType)));{$ENDIF}

  if not ParseResult.IsValid then
  begin
    FTextBuffer.PutString('?SYNTAX ERROR');
    FTextBuffer.NewLine;
    FTextBuffer.NewLine;
    FTextBuffer.PutString('READY.');
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
                // Put it in the input buffer
                FInputBuffer := IntToStr(FAutoLineNumber) + ' ';
                FInputPos := Length(FInputBuffer);
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
                {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Compilation error: ', FImmediateCompiler.LastError);{$ENDIF}
                FTextBuffer.PutString('?SYNTAX ERROR');
                FTextBuffer.NewLine;
              end
              else
              begin
                // Execute the compiled bytecode
                try
                  FBytecodeVM.Reset;
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
              end;

              // READY dopo comando immediato
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.');
              FTextBuffer.NewLine;

            except
              on E: Exception do
              begin
                // Catch any unexpected exceptions
                {$IFDEF DEBUG_CONSOLE}WriteLn('ERROR: Exception in immediate command: ', E.ClassName, ' - ', E.Message);{$ENDIF}
                FTextBuffer.PutString('?SYNTAX ERROR');
                FTextBuffer.NewLine;
                FTextBuffer.NewLine;
                FTextBuffer.PutString('READY.');
                FTextBuffer.NewLine;
              end;
            end;
          finally
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
                FTextBuffer.PutString('READY.');
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
                      FTextBuffer.PutString('?SYNTAX ERROR IN PROGRAM');
                      FTextBuffer.NewLine;
                    end
                    else
                    begin
                      try
                        FBytecodeVM.Reset;
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
                    end;
                  finally
                    if Assigned(BytecodeProgram) then
                      BytecodeProgram.Free;
                  end;
                  FTextBuffer.NewLine;
                  FTextBuffer.PutString('READY.');
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
                FTextBuffer.PutString('READY.');
                FTextBuffer.NewLine;
              end
              else if FBytecodeMode and Assigned(FLoadedBytecode) then
              begin
                // Run loaded bytecode directly
                try
                  FBytecodeVM.Reset;
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
              end
              else if FProgramMemory.GetLineCount > 0 then
              begin
                WriteLn('>>> RUN: FProgramMemory.GetLineCount = ', FProgramMemory.GetLineCount);
                // Get the program source from memory and compile it
                Lines := FProgramMemory.GetAllLines;
                try
                  ProgramSource := Lines.Text;
                  WriteLn('>>> RUN: ProgramSource = ', ProgramSource);
                finally
                  Lines.Free;
                end;

                BytecodeProgram := nil;
                try
                  // Compile the full program
                  WriteLn('>>> RUN: Compiling program...');
                  BytecodeProgram := FImmediateCompiler.CompileProgram(ProgramSource);
                  WriteLn('>>> RUN: Compilation done. BytecodeProgram=', Assigned(BytecodeProgram));

                  if not Assigned(BytecodeProgram) then
                  begin
                    WriteLn('>>> RUN: Compilation FAILED: ', FImmediateCompiler.LastError);
                    {$IFDEF DEBUG_CONSOLE}WriteLn('DEBUG: Program compilation error: ', FImmediateCompiler.LastError);{$ENDIF}
                    FTextBuffer.PutString('?SYNTAX ERROR IN PROGRAM');
                    FTextBuffer.NewLine;
                  end
                  else
                  begin
                    // Execute via VM
                    WriteLn('>>> RUN: Executing via VM...');
                    WriteLn('>>> RUN: BytecodeProgram.GetInstructionCount = ', BytecodeProgram.GetInstructionCount);

                    // DEBUG: Disassemble bytecode before execution
                    WriteLn('>>> RUN: === DISASSEMBLY ===');
                    WriteLn(TBytecodeDisassembler.Create.Disassemble(BytecodeProgram));
                    WriteLn('>>> RUN: === END DISASSEMBLY ===');

                    try
                      FBytecodeVM.Reset;
                      FBytecodeVM.LoadProgram(BytecodeProgram);
                      WriteLn('>>> RUN: Calling FBytecodeVM.Run...');
                      if FDebugMode then
                      begin
                        FDebugger.Reset;
                        FBytecodeVM.RunDebug;
                      end
                      else
                        FBytecodeVM.Run;
                      WriteLn('>>> RUN: FBytecodeVM.Run completed');
                    except
                      on E: Exception do
                      begin
                        WriteLn('>>> RUN: Exception: ', E.Message);
                        FTextBuffer.PutString('?ERROR IN PROGRAM: ' + E.Message);
                        FTextBuffer.NewLine;
                      end;
                    end;
                  end;
                finally
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
              FTextBuffer.PutString('READY.');
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
            end
            else
            begin
              Filename := Trim(Copy(ParseResult.Statement, Length(kEDIT) + 1, MaxInt));
              if TryStrToInt(Filename, LineNum) then
              begin
                // Get the line content
                Lines := FProgramMemory.GetLinesInRange(LineNum, LineNum);
                try
                  if (Lines <> nil) and (Lines.Count > 0) then
                  begin
                    // Put the line in the input buffer for editing
                    FInputBuffer := Lines[0];
                    FInputPos := Length(FInputBuffer);
                    // Render immediately so user sees the line
                    RenderScreen;
                    UpdateCursor;
                    FVideoController.Present;
                  end
                  else
                  begin
                    FTextBuffer.PutString('?LINE NOT FOUND');
                    FTextBuffer.NewLine;
                  end;
                finally
                  if Assigned(Lines) then
                    Lines.Free;
                end;
              end
              else
              begin
                FTextBuffer.PutString('?SYNTAX ERROR');
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
            end
            else
            begin
              Filename := Trim(Copy(ParseResult.Statement, Length(kAUTO) + 1, MaxInt));
              if Filename = '' then
              begin
                // AUTO without argument - turn off auto numbering
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
                  // Put the first line number in the input buffer
                  FInputBuffer := IntToStr(FAutoLineNumber) + ' ';
                  FInputPos := Length(FInputBuffer);
                  RenderScreen;
                  UpdateCursor;
                  FVideoController.Present;
                end
                else
                begin
                  FTextBuffer.PutString('?ILLEGAL QUANTITY');
                  FTextBuffer.NewLine;
                end;
              end
              else
              begin
                FTextBuffer.PutString('?SYNTAX ERROR');
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
            FTextBuffer.PutString('READY.');
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
              FTextBuffer.PutString('READY.');
              FTextBuffer.NewLine;
            end
            else if FDebugger.IsPaused then
            begin
              // Resume from debugger pause
              FDebugger.Continue;
              FBytecodeVM.RunDebug;
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.');
              FTextBuffer.NewLine;
            end
            else
            begin
              FTextBuffer.PutString('?CAN''T CONTINUE ERROR');
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.');
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
            FTextBuffer.PutString('READY.');
            FTextBuffer.NewLine;
          end
          else if CmdWord = kTROFF then
          begin
            // TROFF - Disable debug mode
            FDebugMode := False;
            FDebugger.Deactivate;
            FTextBuffer.PutString('TRACE OFF');
            FTextBuffer.NewLine;
            FTextBuffer.PutString('READY.');
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
            FTextBuffer.PutString('READY.');
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
            FTextBuffer.PutString('READY.');
            FTextBuffer.NewLine;
          end
          else if CmdWord = 'STEP' then
          begin
            // STEP - Step one line (only when paused)
            if FDebugger.IsPaused then
            begin
              FDebugger.StepLine;
              FBytecodeVM.RunDebug;
            end
            else
            begin
              FTextBuffer.PutString('?NOT IN DEBUG MODE');
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.');
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
              try
                FBytecodeVM.Reset;
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
            // SCRATCH command - format: SCRATCH pattern [FORCE]
            Statement := Trim(Copy(ParseResult.Statement, Length(kSCRATCH) + 1, MaxInt));
            ScratchPattern := '';
            ScratchForce := False;

            if Statement = '' then
            begin
              FTextBuffer.PutString('?MISSING FILE PATTERN');
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
                end;
              end
              else
              begin
                // Unquoted - find space (for FORCE flag)
                j := Pos(' ', Statement);
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

              // Check for FORCE flag
              if UpperCase(Trim(Statement)) = 'FORCE' then
                ScratchForce := True;

              if ScratchPattern <> '' then
                ExecuteScratch(ScratchPattern, ScratchForce);
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
          else
          begin
            // Unknown system command
            FTextBuffer.PutString('?UNKNOWN COMMAND');
            FTextBuffer.NewLine;
          end;

          // Note: LIST doesn't show READY, only RUN and NEW do
        end;
    end;

  except
    on E: Exception do
    begin
      FTextBuffer.PutString('?ERROR: ' + E.Message);
      FTextBuffer.NewLine;
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

    // If more lines remain, show "-- MORE --" and wait for key
    if i < Lines.Count then
    begin
      FTextBuffer.PutString('-- MORE --');
      RenderScreen;

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
    FTextBuffer.PutString('READY.');
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
        FTextBuffer.PutString('READY.');
        FTextBuffer.NewLine;
        Exit;
      end;
      on E: EReadError do
      begin
        FTextBuffer.PutString('?FILE READ ERROR');
        FTextBuffer.NewLine;
        FTextBuffer.PutString('READY.');
        FTextBuffer.NewLine;
        Exit;
      end;
      on E: Exception do
      begin
        FTextBuffer.PutString('?I/O ERROR: ' + E.Message);
        FTextBuffer.NewLine;
        FTextBuffer.PutString('READY.');
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

    FTextBuffer.PutString('READY.');
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
      FTextBuffer.PutString('READY.');
      FTextBuffer.NewLine;
    except
      on E: EFCreateError do
      begin
        FTextBuffer.PutString('?FILE CREATE ERROR');
        FTextBuffer.NewLine;
        FTextBuffer.PutString('READY.');
        FTextBuffer.NewLine;
      end;
      on E: EWriteError do
      begin
        FTextBuffer.PutString('?FILE WRITE ERROR');
        FTextBuffer.NewLine;
        FTextBuffer.PutString('READY.');
        FTextBuffer.NewLine;
      end;
      on E: Exception do
      begin
        FTextBuffer.PutString('?I/O ERROR: ' + E.Message);
        FTextBuffer.NewLine;
        FTextBuffer.PutString('READY.');
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

procedure TSedaiNewConsole.ExecuteScratch(const Pattern: string; Force: Boolean);
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
    FTextBuffer.PutString('?FILE NOT FOUND');
    FTextBuffer.NewLine;
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

      FTextBuffer.PutString('READY.');
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
        FTextBuffer.PutString('READY.');
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

      FTextBuffer.PutString('READY.');
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

procedure TSedaiNewConsole.Run;
var
  CurrentLine: string;
  FileExt: string;
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

    // Handle input
    if FInputHandler.LastKeyDown = SDLK_RETURN then
    begin
      // If in scrollback mode, return to end first
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      CurrentLine := FTextBuffer.GetCurrentLine;
      FTextBuffer.NewLine;  // Va a capo dopo l'input
      ProcessCommand(CurrentLine);
    end
    else if FInputHandler.LastKeyDown = SDLK_BACKSPACE then
    begin
      // Exit scrollback mode if active
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.DeleteChar;
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
        // Cancella la riga corrente
        FTextBuffer.ClearCurrentLine;
        // Inserisce il comando dalla history
        FTextBuffer.PutString(FInputHistory[FHistoryCount - 1 - FHistoryPos]);
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
        // Cancella la riga corrente
        FTextBuffer.ClearCurrentLine;
        // Inserisce il comando dalla history
        FTextBuffer.PutString(FInputHistory[FHistoryCount - 1 - FHistoryPos]);
      end
      else if FHistoryPos = 0 then
      begin
        FHistoryPos := -1;
        // Cancella la riga corrente
        FTextBuffer.ClearCurrentLine;
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
    end
    // Ctrl+End: delete to line end
    else if (FInputHandler.LastKeyDown = SDLK_END) and FInputHandler.CtrlPressed and not FInputHandler.ShiftPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.DeleteToLineEnd;
    end
    // Ctrl+Delete: delete word right
    else if (FInputHandler.LastKeyDown = SDLK_DELETE) and FInputHandler.CtrlPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.DeleteWordRight;
    end
    // Ctrl+Backspace: delete word left
    else if (FInputHandler.LastKeyDown = SDLK_BACKSPACE) and FInputHandler.CtrlPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.DeleteWordLeft;
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
    end
    // Ctrl+U: delete entire line (readline style)
    else if (FInputHandler.LastKeyDown = SDLK_u) and FInputHandler.CtrlPressed then
    begin
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      FTextBuffer.DeleteEntireLine;
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
    else if FInputHandler.HasChar then
    begin
      // Exit scrollback mode if active - any typed character returns to prompt
      if FTextBuffer.IsInScrollbackMode then
        FTextBuffer.ScrollToEnd;
      // Use InsertChar for proper insert mode (shifts chars right)
      FTextBuffer.InsertChar(FInputHandler.LastChar);
    end;

    // Render
    RenderScreen;
    UpdateCursor;
    FVideoController.Present;

    SDL_Delay(16); // ~60 FPS
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

  WriteLn('[DEBUG] ReadLine ENTERED, Prompt="', Prompt, '"');
  while not Done and not FQuitRequested do
  begin
    while SDL_PollEvent(@Event) = 1 do
    begin
      // DEBUG: log all events
      if Event.type_ = SDL_KEYDOWN then
        WriteLn('[DEBUG] SDL_KEYDOWN: sym=', Event.key.keysym.sym, ' mod=', Event.key.keysym.mod_);

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

            // DEBUG: Test if END key is recognized at all (without CTRL)
            WriteLn('[DEBUG] sym=', Event.key.keysym.sym, ' SDLK_END=', SDLK_END, ' scancode=', Event.key.keysym.scancode, ' SDL_SCANCODE_END=', SDL_SCANCODE_END);
            // Test: stop on END key alone (no CTRL required)
            if (Event.key.keysym.scancode = SDL_SCANCODE_END) then
            begin
              WriteLn('[DEBUG] END key detected! Stopping program.');
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

end.
