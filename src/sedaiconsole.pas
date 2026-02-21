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
unit SedaiConsole;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}

interface

uses
  Classes, SysUtils, Variants, SDL2, SDL2_ttf, TypInfo,
  SedaiOutputInterface, SedaiGraphicsModes,
  SedaiProgramMemory, SedaiLexerFSM, SedaiPackratParser,
  SedaiAST, SedaiTokenList,
  SedaiParserTypes, SedaiParserResults,
  SedaiCommandRouter, SedaiCommandTypes,
  SedaiExecutor;

const
  SCROLLBACK_LINES = 1000;
  INPUT_HISTORY_SIZE = 4096;  // Same as PowerShell default
  CURSOR_BLINK_MS = 500;  // C64 cursor timing (verified)

  // Default colors - C128 style
  DEFAULT_BG_R = 129;
  DEFAULT_BG_G = 129;
  DEFAULT_BG_B = 129;
  DEFAULT_FG_R = 219;
  DEFAULT_FG_G = 255;
  DEFAULT_FG_B = 158;

  DEFAULT_FONT_PATHS: array[0..0] of string = (
    'font\PixelOperatorMono8-Bold.ttf'
  );

type
  { Forward declarations }
  TTextBuffer = class;

  { TVideoController - SDL2 video mode management }
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
    FBGColor: TSDL_Color;
    FFGColor: TSDL_Color;

    function InitializeSDL: Boolean;
    procedure CleanupSDL;
    function CreateWindowAndRenderer: Boolean;
    procedure CalculateViewportAndFont;
    function LoadFontWithSize(Size: Integer): Boolean;
    function TryLoadFont(const FontPath: string; Size: Integer): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function Initialize(Mode: TGraphicMode; Fullscreen: Boolean = False): Boolean;
    function SetMode(Mode: TGraphicMode): Boolean;
    procedure ToggleFullscreen;

    procedure SetColors(BG, FG: TSDL_Color);
    procedure ClearBorder;      // Clear with FG color (border)
    procedure ClearViewport;    // Clear viewport with BG color
    procedure Present;
    procedure RenderText(ATextBuffer: TTextBuffer);  // Render text buffer
    
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
    property BGColor: TSDL_Color read FBGColor;
    property FGColor: TSDL_Color read FFGColor;
  private
    FCursorX: Integer;
    FCursorY: Integer;
    FInitialized: Boolean;
    FInScrollMode: Boolean;
    FShouldQuit: Boolean;
    
  protected
    function SDLColorToColor(const SDLColor: TSDL_Color): TColor;
    function ColorToSDLColor(const Color: TColor): TSDL_Color;
  end;

  { TTextLine - Single text line }
  TTextLine = record
    Text: string;
  end;

  { TTextBuffer - Screen-like text buffer }
  TTextBuffer = class
  private
    FLines: array of string;
    FRows: Integer;
    FCols: Integer;
    FCursorX: Integer;
    FCursorY: Integer;

  public
    constructor Create(Rows, Cols: Integer);
    destructor Destroy; override;

    procedure Clear;
    procedure PutChar(Ch: Char);
    procedure PutString(const S: string);
    procedure NewLine;
    procedure DeleteChar;  // Backspace
    procedure ScrollUp;
    procedure ClearCurrentLine;  // Clear current line

    function GetLine(Index: Integer): string;
    function GetCurrentLine: string;

    property CursorX: Integer read FCursorX write FCursorX;
    property CursorY: Integer read FCursorY write FCursorY;
    property Rows: Integer read FRows;
    property Cols: Integer read FCols;
  end;

  { Forward declaration }
  TSedaiConsole = class;

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
    FConsole: TSedaiConsole;  // Console reference for RenderScreen

  public
    constructor Create(ATextBuffer: TTextBuffer; AVideoController: TVideoController; AConsole: TSedaiConsole);
    destructor Destroy; override;

    procedure ProcessEvents;
    procedure ClearFlags;

    // IInputDevice implementation
    function ReadLine(const Prompt: string = ''; IsCommand: Boolean = True; NumericOnly: Boolean = False; AllowDecimal: Boolean = True): string;
    function ReadKey: Char;
    function KeyPressed: Boolean;
    function HasChar: Boolean;
    function GetLastChar: string;
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

  { TGraphicEngine - Graphic rendering }
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

  { TConsoleOutputAdapter - Adapter that writes to TextBuffer }
  { NOTE: Using TObject instead of TInterfacedObject because interfaces use CORBA mode (no refcount) }
  TConsoleOutputAdapter = class(TObject, IOutputDevice)
  private
    FTextBuffer: TTextBuffer;
    FVideoController: TVideoController;
    FInitialized: Boolean;
  public
    constructor Create(ATextBuffer: TTextBuffer; AVideoController: TVideoController);
    
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
    // Shape drawing (stub implementation - uses TVideoController via GraphicEngine)
    procedure DrawBox(X1, Y1, X2, Y2: Integer; Angle: Double = 0);
    procedure DrawBoxStyled(X1, Y1, X2, Y2: Integer; const Style: TShapeStyle; Angle: Double = 0);
    procedure DrawBoxWithColor(X1, Y1, X2, Y2: Integer; Color: UInt32; Angle: Double = 0; Filled: Boolean = False);
    procedure DrawCircleWithColor(X, Y, XR, YR: Integer; Color: UInt32;
                                  SA: Double = 0; EA: Double = 360;
                                  Angle: Double = 0; Inc: Double = 2);
    procedure DrawLine(X1, Y1, X2, Y2: Integer; Color: UInt32);
    procedure SetPixelCursor(X, Y: Integer);
    function GetPixelCursorX: Integer;
    function GetPixelCursorY: Integer;
    procedure SetBorderStyle(const Style: TBorderStyle);
    procedure SetFillStyle(const Style: TFillStyleDef);
    function GetBorderStyle: TBorderStyle;
    function GetFillStyle: TFillStyleDef;
    // FAST mode (C128 2MHz mode emulation)
    procedure SetFastMode(Enabled: Boolean);
    function GetFastMode: Boolean;
    procedure SetFastModeAlpha(Alpha: Byte);
    function GetFastModeAlpha: Byte;
  end;

  { TSedaiConsole - Main console }
  TSedaiConsole = class
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
    FCtrlCEnabled: Boolean;  // If True, CTRL+C exits the console

    // Command handling components
    FProgramMemory: TProgramMemory;
    FCommandRouter: TCommandRouter;
    
    // Direct command set
    FDirectCommandLexer: TLexerFSM;
    FDirectCommandParser: TPackratParser;
    FDirectCommandExecutor: TSedaiExecutor;

    // Program execution set
    FProgramLexer: TLexerFSM;
    FProgramParser: TPackratParser;
    FProgramExecutor: TSedaiExecutor;

    procedure ShowSplashScreen;
    function GetSystemInfo: string;
    function GetArchitecture: string;
    function GetAvailableMemory: string;
    procedure ExecuteDirectCommand(const Command: string);
    procedure ExecuteConsoleCommand(AST: TASTNode);

  public
    constructor Create;
    destructor Destroy; override;

    function Initialize(Mode: TGraphicMode; Fullscreen: Boolean = False): Boolean;
    procedure Run;
    procedure ProcessCommand(const Command: string);
    procedure RenderScreen;
    procedure UpdateCursor;

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
  FFullscreen := False;
  FVSync := True;

  // Default C128-style colors
  FBGColor.r := DEFAULT_BG_R;
  FBGColor.g := DEFAULT_BG_G;
  FBGColor.b := DEFAULT_BG_B;
  FBGColor.a := 255;

  FFGColor.r := DEFAULT_FG_R;
  FFGColor.g := DEFAULT_FG_G;
  FFGColor.b := DEFAULT_FG_B;
  FFGColor.a := 255;
end;

destructor TVideoController.Destroy;
begin
  CleanupSDL;
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

  Result := True;
end;

procedure TVideoController.CleanupSDL;
begin
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

  // If fullscreen, find best supported video mode using two-phase algorithm
  if FFullscreen then
  begin
    NumModes := SDL_GetNumDisplayModes(0);

    // Get native display resolution
    if SDL_GetCurrentDisplayMode(0, @DisplayMode) = 0 then
    begin
    end
    else
    begin
      WriteLn('WARNING: Cannot get native display mode, using fallback');
      DisplayMode.w := 1920;
      DisplayMode.h := 1080;
    end;

    // Calculate native aspect ratio
    NativeRatio := DisplayMode.w / DisplayMode.h;

    BestMode.w := 0;
    BestMode.h := 0;
    BestMode.format := 0;
    BestMode.refresh_rate := 0;
    BestMode.driverdata := nil;

    // === PHASE 1: Search for uniform scaling resolution (ScaleX == ScaleY) ===
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
            Break;
          end;
        end;
      end;
    end;

    // === PHASE 2: Fallback with non-uniform scaling (min(ScaleX, ScaleY)) ===
    if BestMode.w = 0 then
    begin
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
              Break;
            end;
          end;
        end;
      end;
    end;

    // === FINAL FALLBACK: Use native resolution ===
    if BestMode.w = 0 then
    begin
      if SDL_GetCurrentDisplayMode(0, @BestMode) <> 0 then
      begin
        BestMode.w := 1920;
        BestMode.h := 1080;
        WriteLn('WARNING: Cannot get native mode, hardcoded to 1920x1080');
      end;
      ScaleX := BestMode.w div 640;
      ScaleY := BestMode.h div 400;
      Scale := Max(1, Min(ScaleX, ScaleY));
    end;

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

  // Update with actual window size
  SDL_GetWindowSize(FWindow, @ActualW, @ActualH);
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
    // In windowed mode, calculate automatic scaling if window is larger than native
    ScaleX := FWindowWidth div FModeInfo.NativeWidth;
    ScaleY := FWindowHeight div FModeInfo.NativeHeight;
    Scale := Min(ScaleX, ScaleY);

    // Apply scaling if calculated scale >= 1
    if Scale >= 1 then
    begin
      FFontSize := BaseFontSize * Scale;
      FViewportWidth := FModeInfo.NativeWidth * Scale;
      FViewportHeight := FModeInfo.NativeHeight * Scale;
      FViewportX := (FWindowWidth - FViewportWidth) div 2;
      FViewportY := (FWindowHeight - FViewportHeight) div 2;
    end
    else
    begin
      // Fallback: use native viewport if window is smaller
      FFontSize := BaseFontSize;
      FViewportWidth := FModeInfo.NativeWidth;
      FViewportHeight := FModeInfo.NativeHeight;
      FViewportX := (FWindowWidth - FViewportWidth) div 2;
      FViewportY := (FWindowHeight - FViewportHeight) div 2;
    end;
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

  // Get executable path
  ExePath := ExtractFilePath(ParamStr(0));

  for i := Low(DEFAULT_FONT_PATHS) to High(DEFAULT_FONT_PATHS) do
  begin
    // Try first with absolute path (relative to executable)
    FontPath := ExePath + DEFAULT_FONT_PATHS[i];
    if TryLoadFont(FontPath, Size) then
    begin
      Result := True;
      Exit;
    end;

    // Then try with original relative path
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

  if not InitializeSDL then
    Exit;

  // Calculate window size - use WindowWidth/WindowHeight if specified (for border)
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

  if not CreateWindowAndRenderer then
    Exit;

  CalculateViewportAndFont;

  if not LoadFontWithSize(FFontSize) then
    Exit;

  // Viewport remains fixed 640x400 (80x50 chars 8x8)
  // Do not recalculate based on FCharWidth/FCharHeight

  FPaletteManager := TPaletteManager.Create(FModeInfo.PaletteType);

  ClearBorder;
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
  FBGColor := BG;
  FFGColor := FG;
end;

procedure TVideoController.ClearBorder;
begin
  // Fill entire window with foreground color (border)
  SDL_SetRenderDrawColor(FRenderer, FFGColor.r, FFGColor.g, FFGColor.b, FFGColor.a);
  SDL_RenderClear(FRenderer);
end;

procedure TVideoController.ClearViewport;
var
  ViewportRect: TSDL_Rect;
begin
  // Fill viewport with background color
  ViewportRect.x := FViewportX;
  ViewportRect.y := FViewportY;
  ViewportRect.w := FViewportWidth;
  ViewportRect.h := FViewportHeight;

  SDL_SetRenderDrawColor(FRenderer, FBGColor.r, FBGColor.g, FBGColor.b, FBGColor.a);
  SDL_RenderFillRect(FRenderer, @ViewportRect);
end;

procedure TVideoController.Present;
begin
  SDL_RenderPresent(FRenderer);
end;

procedure TVideoController.RenderText(ATextBuffer: TTextBuffer);
var
  i, RenderX, RenderY: Integer;
  Line: string;
  Surface: PSDL_Surface;
  Texture: PSDL_Texture;
  DestRect: TSDL_Rect;
begin
  if not Assigned(ATextBuffer) or not Assigned(FFont) then Exit;

  // Clear viewport
  ClearViewport;

  // Render each line of the text buffer
  RenderY := FViewportY;
  for i := 0 to ATextBuffer.Rows - 1 do
  begin
    Line := ATextBuffer.GetLine(i);
    if Line <> '' then
    begin
      // Create surface with text
      Surface := TTF_RenderUTF8_Solid(FFont, PChar(Line), FFGColor);
      if not Assigned(Surface) then Continue;
      
      try
        Texture := SDL_CreateTextureFromSurface(FRenderer, Surface);
        if not Assigned(Texture) then Continue;
        
        try
          DestRect.x := FViewportX;
          DestRect.y := RenderY;
          DestRect.w := Surface^.w;
          DestRect.h := Surface^.h;
          
          SDL_RenderCopy(FRenderer, Texture, nil, @DestRect);
        finally
          SDL_DestroyTexture(Texture);
        end;
      finally
        SDL_FreeSurface(Surface);
      end;
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
begin
  if Text = '' then
    Exit;

  // Force a newline if we're still at position 0 and not the first line
  if (FCursorX = 0) and (FCursorY > 0) and not ClearBackground then
    Inc(FCursorY);

  Surface := TTF_RenderUTF8_Blended(FFont, PChar(Text), FFGColor);
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
          SDL_SetRenderDrawColor(FRenderer, FBGColor.r, FBGColor.g, FBGColor.b, FBGColor.a);
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
begin
  CursorRect.x := ViewportX + (X * CharWidth);
  CursorRect.y := ViewportY + (Y * CharHeight);
  CursorRect.w := CharWidth;
  CursorRect.h := CharHeight;
  
  SDL_SetRenderDrawColor(FRenderer, FFGColor.r, FFGColor.g, FFGColor.b, FFGColor.a);
  SDL_RenderFillRect(FRenderer, @CursorRect);
end;

procedure TVideoController.HideCursor(X, Y: Integer);
var
  CursorRect: TSDL_Rect;
begin
  CursorRect.x := ViewportX + (X * CharWidth);
  CursorRect.y := ViewportY + (Y * CharHeight);
  CursorRect.w := CharWidth;
  CursorRect.h := CharHeight;
  
  SDL_SetRenderDrawColor(FRenderer, FBGColor.r, FBGColor.g, FBGColor.b, FBGColor.a);
  SDL_RenderFillRect(FRenderer, @CursorRect);
end;

procedure TVideoController.SetColors(Foreground, Background: TColor);
begin
  FFGColor := ColorToSDLColor(Foreground);
  FBGColor := ColorToSDLColor(Background);
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

function TVideoController.SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean; SplitLine: Integer): Boolean;
var
  NewModeInfo: TGraphicModeInfo;
begin
  // Get mode info to access DefaultSplitLine
  NewModeInfo := GRAPHICS_MODES[Mode];

  // Use user-provided SplitLine, or DefaultSplitLine if -1
  if SplitLine < 0 then
    FSplitLine := NewModeInfo.DefaultSplitLine
  else
    FSplitLine := SplitLine;

  Result := SetMode(Mode);
  if Result and ClearBuffer then
    ClearViewport;
end;


function TVideoController.GetGraphicMode: TGraphicMode;
begin
  Result := FCurrentMode;
end;

function TVideoController.IsInGraphicsMode: Boolean;
begin
  Result := False;  // Not implemented yet
end;

procedure TVideoController.SetPixel(X, Y: Integer; RGB: UInt32);
begin
  // Not implemented yet
end;

procedure TVideoController.SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex);
begin
  // Not implemented yet
end;

function TVideoController.GetPixel(X, Y: Integer): UInt32;
begin
  Result := 0;  // Not implemented yet
end;

procedure TVideoController.EnablePalette(Enable: Boolean);
begin
  // Not implemented yet
end;

function TVideoController.IsPaletteEnabled: Boolean;
begin
  Result := False;  // Not implemented yet
end;

procedure TVideoController.SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
begin
  // Not implemented yet
end;

function TVideoController.GetPaletteColor(Index: TPaletteIndex): UInt32;
begin
  Result := 0;  // Not implemented yet
end;

procedure TVideoController.ResetPalette;
begin
  // Not implemented yet
end;


{ TTextBuffer }

constructor TTextBuffer.Create(Rows, Cols: Integer);
var
  i: Integer;
begin
  inherited Create;
  FRows := Rows;
  FCols := Cols;
  SetLength(FLines, Rows);
  for i := 0 to Rows - 1 do
    FLines[i] := '';
  FCursorX := 0;
  FCursorY := 0;
end;

destructor TTextBuffer.Destroy;
begin
  SetLength(FLines, 0);
  inherited Destroy;
end;

procedure TTextBuffer.Clear;
var
  i: Integer;
begin
  for i := 0 to FRows - 1 do
    FLines[i] := '';
  FCursorX := 0;
  FCursorY := 0;
end;

procedure TTextBuffer.PutChar(Ch: Char);
var
  Line: string;
  Len: Integer;
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
    Line := FLines[FCursorY];
    Len := Length(Line);

    // Pad with spaces up to CursorX if needed
    while Len < FCursorX do
    begin
      Line := Line + ' ';
      Inc(Len);
    end;

    // Insert or overwrite character at CursorX position
    if FCursorX < Len then
      Line[FCursorX + 1] := Ch  // Overwrite
    else
      Line := Line + Ch;  // Append

    FLines[FCursorY] := Line;
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
  Len: Integer;
begin
  if FCursorX > 0 then
  begin
    Len := Length(FLines[FCursorY]);
    if Len > 0 then
    begin
      SetLength(FLines[FCursorY], Len - 1);
      Dec(FCursorX);
    end;
  end;
end;

procedure TTextBuffer.ScrollUp;
var
  i: Integer;
begin
  for i := 0 to FRows - 2 do
    FLines[i] := FLines[i + 1];
  FLines[FRows - 1] := '';
end;

procedure TTextBuffer.ClearCurrentLine;
begin
  // Clear current line content
  FLines[FCursorY] := '';
  // Move cursor to beginning of line
  FCursorX := 0;
end;

function TTextBuffer.GetLine(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FRows) then
    Result := FLines[Index]
  else
    Result := '';
end;

function TTextBuffer.GetCurrentLine: string;
begin
  Result := FLines[FCursorY];
end;

{ TInputHandler }

constructor TInputHandler.Create(ATextBuffer: TTextBuffer; AVideoController: TVideoController; AConsole: TSedaiConsole);
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
  FInitialized := True;
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
end;

procedure TConsoleOutputAdapter.NewLine;
begin
  FTextBuffer.NewLine;
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
  // Not needed for TextBuffer
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
  // Delegate to VideoController for scroll/pager handling
  FVideoController.OnUserInput;
end;

function TConsoleOutputAdapter.HandleScrollKeys(Key, Modifiers: Integer): Boolean;
begin
  // Delegate to VideoController for scroll/pager handling
  Result := FVideoController.HandleScrollKeys(Key, Modifiers);
end;

procedure TConsoleOutputAdapter.ProcessScrollInput;
begin
  // Delegate to VideoController for scroll/pager handling
  FVideoController.ProcessScrollInput;
end;

function TConsoleOutputAdapter.GetInScrollMode: Boolean;
begin
  // Delegate to VideoController for scroll mode handling
  Result := FVideoController.GetInScrollMode;
end;

function TConsoleOutputAdapter.SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean; SplitLine: Integer): Boolean;
begin
  Result := False; // TextBuffer does not support graphics modes
end;

function TConsoleOutputAdapter.GetGraphicMode: TGraphicMode;
begin
  Result := gm40ColText;  // TextBuffer always in text mode
end;

function TConsoleOutputAdapter.IsInGraphicsMode: Boolean;
begin
  Result := False;
end;

procedure TConsoleOutputAdapter.SetPixel(X, Y: Integer; RGB: UInt32);
begin
  // Not needed for TextBuffer
end;

procedure TConsoleOutputAdapter.SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex);
begin
  // Not needed for TextBuffer
end;

function TConsoleOutputAdapter.GetPixel(X, Y: Integer): UInt32;
begin
  Result := 0;
end;

procedure TConsoleOutputAdapter.EnablePalette(Enable: Boolean);
begin
  // Not needed for TextBuffer
end;

function TConsoleOutputAdapter.IsPaletteEnabled: Boolean;
begin
  Result := False;
end;

procedure TConsoleOutputAdapter.SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
begin
  // Not needed for TextBuffer
end;

function TConsoleOutputAdapter.GetPaletteColor(Index: TPaletteIndex): UInt32;
begin
  Result := 0;
end;

procedure TConsoleOutputAdapter.ResetPalette;
begin
  // Not needed for TextBuffer
end;

{ Shape drawing stubs - TConsoleOutputAdapter uses GraphicEngine via console }
procedure TConsoleOutputAdapter.DrawBox(X1, Y1, X2, Y2: Integer; Angle: Double);
begin
  // Delegated to GraphicEngine
end;

procedure TConsoleOutputAdapter.DrawBoxStyled(X1, Y1, X2, Y2: Integer; const Style: TShapeStyle; Angle: Double);
begin
  // Delegated to GraphicEngine
end;

procedure TConsoleOutputAdapter.DrawBoxWithColor(X1, Y1, X2, Y2: Integer; Color: UInt32; Angle: Double; Filled: Boolean);
begin
  // Delegated to GraphicEngine
end;

procedure TConsoleOutputAdapter.DrawCircleWithColor(X, Y, XR, YR: Integer; Color: UInt32;
                              SA: Double; EA: Double; Angle: Double; Inc: Double);
begin
  // Delegated to GraphicEngine
end;

procedure TConsoleOutputAdapter.DrawLine(X1, Y1, X2, Y2: Integer; Color: UInt32);
begin
  // Delegated to GraphicEngine
end;

procedure TConsoleOutputAdapter.SetPixelCursor(X, Y: Integer);
begin
  // Not implemented
end;

function TConsoleOutputAdapter.GetPixelCursorX: Integer;
begin
  Result := 0;
end;

function TConsoleOutputAdapter.GetPixelCursorY: Integer;
begin
  Result := 0;
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

{ FAST mode stubs - TConsoleOutputAdapter }
procedure TConsoleOutputAdapter.SetFastMode(Enabled: Boolean);
begin
  // Not implemented - FAST/SLOW are C128 hardware commands
end;

function TConsoleOutputAdapter.GetFastMode: Boolean;
begin
  Result := False;
end;

procedure TConsoleOutputAdapter.SetFastModeAlpha(Alpha: Byte);
begin
  // Not implemented
end;

function TConsoleOutputAdapter.GetFastModeAlpha: Byte;
begin
  Result := 255;
end;

{ TSedaiConsole }

constructor TSedaiConsole.Create;
begin
  inherited Create;
  FVideoController := TVideoController.Create;
  FTextBuffer := TTextBuffer.Create(25, 40);  // Create TextBuffer immediately
  FGraphicEngine := nil;
  FInputHandler := TInputHandler.Create(FTextBuffer, FVideoController, Self);  // Pass references + console
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

  // Initialize direct command set
  FDirectCommandLexer := TLexerFSM.Create;
  FDirectCommandLexer.SetHasLineNumbers(False);  // Direct commands without line numbers
  FDirectCommandLexer.SetRequireSpacesBetweenTokens(True);
  FDirectCommandLexer.SetCaseSensitive(False);

  FDirectCommandParser := TPackratParser.Create;
  // Create executors with valid I/O device references (constructor requires non-nil)
  FDirectCommandExecutor := TSedaiExecutor.Create(FVideoController, FInputHandler);

  // Initialize program execution set
  FProgramLexer := TLexerFSM.Create;
  FProgramLexer.SetHasLineNumbers(True);   // Programs have line numbers
  FProgramLexer.SetRequireSpacesBetweenTokens(True);
  FProgramLexer.SetCaseSensitive(False);

  FProgramParser := TPackratParser.Create;
  FProgramExecutor := TSedaiExecutor.Create(FVideoController, FInputHandler);
  FProgramExecutor.ProgramMemory := FProgramMemory;
end;

destructor TSedaiConsole.Destroy;
begin
  // Free executors first so they don't reference a freed ProgramMemory
  if Assigned(FDirectCommandExecutor) then
  begin
    FDirectCommandExecutor.Free;
    FDirectCommandExecutor := nil;
  end;

  if Assigned(FDirectCommandParser) then
  begin
    FDirectCommandParser.Free;
    FDirectCommandParser := nil;
  end;

  if Assigned(FDirectCommandLexer) then
  begin
    FDirectCommandLexer.Free;
    FDirectCommandLexer := nil;
  end;

  if Assigned(FProgramExecutor) then
  begin
    FProgramExecutor.Free;
    FProgramExecutor := nil;
  end;

  if Assigned(FProgramParser) then
  begin
    FProgramParser.Free;
    FProgramParser := nil;
  end;

  if Assigned(FProgramLexer) then
  begin
    FProgramLexer.Free;
    FProgramLexer := nil;
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

function TSedaiConsole.GetSystemInfo: string;
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

function TSedaiConsole.GetArchitecture: string;
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

function TSedaiConsole.GetAvailableMemory: string;
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

procedure TSedaiConsole.ShowSplashScreen;
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

  // Truncate lines that are too long
  for i := 0 to High(Lines) do
    if Length(Lines[i]) > MaxAvailable then
      Lines[i] := Copy(Lines[i], 1, MaxAvailable - 3) + '...';

  // Calculate max length (limited to MaxAvailable)
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

  // Empty line before splash
  FTextBuffer.NewLine;

  // Draw top border
  FTextBuffer.CursorX := StartX;
  FTextBuffer.PutString(BorderLine);
  FTextBuffer.NewLine;

  // Empty line after top border
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

  // Empty line before bottom border
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
  FTextBuffer.NewLine;  // Empty line
  FTextBuffer.CursorX := 0;
  FTextBuffer.PutString('READY.');
  FTextBuffer.NewLine;  // Cursor on next line
  FTextBuffer.CursorX := 0;

  // Render splash screen with READY
  RenderScreen;
  UpdateCursor;
  FVideoController.Present;

  // Small delay to show splash
  SDL_Delay(1000);
end;

function TSedaiConsole.Initialize(Mode: TGraphicMode; Fullscreen: Boolean = False): Boolean;
begin
  Result := False;

  if not FVideoController.Initialize(Mode, Fullscreen) then
    Exit;

  // TextBuffer was created in constructor, but may have wrong dimensions
  // If dimensions don't match, recreate it
  if (FTextBuffer.Rows <> FVideoController.ModeInfo.TextRows) or
     (FTextBuffer.Cols <> FVideoController.ModeInfo.TextCols) then
  begin
    FTextBuffer.Free;
    FTextBuffer := TTextBuffer.Create(FVideoController.ModeInfo.TextRows, FVideoController.ModeInfo.TextCols);
  end;

  FGraphicEngine := TGraphicEngine.Create(FVideoController);

  // Create output adapter that writes to TextBuffer
  FConsoleOutputAdapter := TConsoleOutputAdapter.Create(FTextBuffer, FVideoController);

  // Connect I/O devices to executors
  if Assigned(FDirectCommandExecutor) then
  begin
    // For direct command set
    FDirectCommandExecutor.OutputDevice := FVideoController;
    FDirectCommandExecutor.InputDevice := FInputHandler;
    FDirectCommandExecutor.ProgramMemory := FProgramMemory;
  end;

  if Assigned(FProgramExecutor) then
  begin
    // For program set - use adapter that writes to TextBuffer
    FProgramExecutor.OutputDevice := FConsoleOutputAdapter;
    FProgramExecutor.InputDevice := FInputHandler;
  end;

  Result := True;
end;

procedure TSedaiConsole.RenderScreen;
var
  i, RenderX, RenderY: Integer;
  Line: string;
  PrevCursorX, PrevCursorY: Integer;
begin
  // Store cursor position
  PrevCursorX := FTextBuffer.CursorX;
  PrevCursorY := FTextBuffer.CursorY;

  // First draw border (window with FG color)
  FVideoController.ClearBorder;

  // Then draw viewport (with BG color)
  FVideoController.ClearViewport;

  // Render text in viewport
  RenderX := FVideoController.ViewportX;
  RenderY := FVideoController.ViewportY;

  for i := 0 to FTextBuffer.Rows - 1 do
  begin
    Line := FTextBuffer.GetLine(i);
    if Line <> '' then
      FGraphicEngine.DrawText(RenderX, RenderY, Line, FVideoController.FGColor);
    RenderY := RenderY + FVideoController.CharHeight;
  end;

  // Restore cursor position
  FTextBuffer.CursorX := PrevCursorX;
  FTextBuffer.CursorY := PrevCursorY;
end;

procedure TSedaiConsole.UpdateCursor;
var
  CurrentTime: Cardinal;
  CursorX, CursorY: Integer;
  CurrentChar: Char;
  CharAtCursor: string;
begin
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

    // Get character at cursor position
    CharAtCursor := '';
    if FTextBuffer.CursorX < Length(FTextBuffer.GetCurrentLine) then
    begin
      CurrentChar := FTextBuffer.GetCurrentLine[FTextBuffer.CursorX + 1];
      CharAtCursor := CurrentChar;
    end;

    // Draw cursor as filled rectangle (8x8 or char size)
    FGraphicEngine.FillRect(CursorX, CursorY,
                           FVideoController.CharWidth,
                           FVideoController.CharHeight,
                           FVideoController.FGColor);

    // If there's a character, draw it in reverse (background color)
    if CharAtCursor <> '' then
      FGraphicEngine.DrawChar(CursorX, CursorY, CharAtCursor[1], FVideoController.BGColor);
  end;
end;

procedure TSedaiConsole.ExecuteDirectCommand(const Command: string);
var
  FirstChar: Char;
  LineNum: Integer;
  RestOfLine: string;
  SpacePos: Integer;
begin
  if Length(Command) = 0 then
    Exit;

  FirstChar := Command[1];

  // If starts with a number, it's a program line
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
          // Delete line
          FProgramMemory.DeleteLine(LineNum);
        end
        else
        begin
          // Add/replace line
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
      // Number only, delete line
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
    // Direct command/expression - TODO: evaluate with interpreter
    FTextBuffer.PutString('?SYNTAX ERROR');
    FTextBuffer.NewLine;
  end;
end;

procedure TSedaiConsole.ProcessCommand(const Command: string);
var
  TrimmedCmd: string;
  ParseResult: TInputParseResult;
  TokenList: TTokenList;
  ParsedAST: TParsingResult;
  Lines: TStringList;
  i: Integer;
  ProgramAST: TASTNode;
begin
  TrimmedCmd := Trim(Command);

  if TrimmedCmd = '' then
  begin
    // Empty command: no action, cursor already on new line
    Exit;
  end;

  // Use CommandRouter to determine command type
  ParseResult := FCommandRouter.ParseInput(TrimmedCmd);

  // Add to history only if not a program line
  if ParseResult.CommandType <> ctProgramLine then
  begin
    if FHistoryCount < INPUT_HISTORY_SIZE then
    begin
      FInputHistory[FHistoryCount] := Command;
      Inc(FHistoryCount);
    end
    else
    begin
      // Shift history - use proper string assignment (NOT Move!)
      // Move() on strings only copies pointers, corrupting reference counts
      for i := 0 to INPUT_HISTORY_SIZE - 2 do
        FInputHistory[i] := FInputHistory[i + 1];
      FInputHistory[INPUT_HISTORY_SIZE - 1] := Command;
    end;
  end;
  FHistoryPos := -1;

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
          // Store line in program
          if ParseResult.LineNumber >= 0 then
          begin
            if ParseResult.Statement = '' then
              FProgramMemory.DeleteLine(ParseResult.LineNumber)
            else
              FProgramMemory.StoreLine(ParseResult.LineNumber, ParseResult.Statement);
          end;
          // After line insertion: no additional output, cursor already on new line
        end;

      ctImmediate:
        begin
          // Use direct command set
          TokenList := nil;
          ParsedAST := nil;
          try
            try
              FDirectCommandLexer.Reset;
              FDirectCommandLexer.Source := ParseResult.Statement + #13#10;
              TokenList := FDirectCommandLexer.ScanAllTokensFast;

              if not Assigned(TokenList) or (TokenList.Count = 0) then
              begin
                // Nothing to do
                FTextBuffer.NewLine;
                FTextBuffer.PutString('READY.');
                FTextBuffer.NewLine;
                Exit;
              end;

              ParsedAST := FDirectCommandParser.Parse(TokenList);

              if not Assigned(ParsedAST) then
                raise Exception.Create('Parser returned nil result');

              if ParsedAST.Success and Assigned(ParsedAST.AST) then
              begin
                try
                  FDirectCommandExecutor.ExecuteStatement(ParsedAST.AST);
                except
                  on E: Exception do
                  begin
                    WriteLn('ERROR: Exception during command execution: ', E.ClassName, ' - ', E.Message);
                    FTextBuffer.PutString('?EXCEPTION IN COMMAND EXECUTOR: ' + E.ClassName + ' - ' + E.Message);
                    FTextBuffer.NewLine;
                  end;
                end;
              end
              else
              begin
                FTextBuffer.PutString('?SYNTAX ERROR');
                FTextBuffer.NewLine;
              end;
              
              // READY after immediate command
              FTextBuffer.NewLine;
              FTextBuffer.PutString('READY.');
              FTextBuffer.NewLine;

            except
              on E: Exception do
              begin
                // Catch exceptions from lexer/parser/token handling
                WriteLn('ERROR: Exception while parsing immediate command: ', E.ClassName, ' - ', E.Message);
                // Don't show exception, just unknown command
                FTextBuffer.PutString('?SYNTAX ERROR');
                FTextBuffer.NewLine;
                FTextBuffer.NewLine;
                FTextBuffer.PutString('READY.');
                FTextBuffer.NewLine;
              end;
            end;
          finally
            // CRITICAL: Free AST first, THEN TokenList
            // The AST contains pointers to tokens, so tokens must stay alive
            // until the AST (and executor using it) are done
            if Assigned(ParsedAST) then
              ParsedAST.Free;
            if Assigned(TokenList) then
              TokenList.Free;
          end;
        end;

      ctSystemCommand:
        begin
          // System commands that can be used both in direct mode and in programs
          if UpperCase(ParseResult.Statement) = 'RUN' then
          begin
            // Execute program using program set
            if FProgramMemory.GetLineCount > 0 then
            begin
              ProgramAST := FProgramMemory.GetOrBuildAST;
              if Assigned(ProgramAST) then
              begin
                try
                  // Execute the program
                  FProgramExecutor.Execute(ProgramAST);
                except
                  on E: Exception do
                  begin
                    FTextBuffer.PutString('?ERROR IN PROGRAM: ' + E.Message);
                    FTextBuffer.NewLine;
                  end;
                end;
              end
              else
              begin
                FTextBuffer.PutString('?SYNTAX ERROR IN PROGRAM');
                FTextBuffer.NewLine;
              end;
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
          end
          else if UpperCase(ParseResult.Statement) = 'LIST' then
          begin
            // List program
            Lines := FProgramMemory.GetAllLines;
            try
              if Lines.Count > 0 then
              begin
                for i := 0 to Lines.Count - 1 do
                begin
                  FTextBuffer.PutString(Lines[i]);
                  FTextBuffer.NewLine;
                end;
              end
              else
              begin
                FTextBuffer.PutString('NO PROGRAM');
                FTextBuffer.NewLine;
              end;
            finally
              Lines.Free;
            end;
          end
          else if UpperCase(ParseResult.Statement) = 'NEW' then
          begin
            FProgramMemory.Clear;
            FTextBuffer.NewLine;
            FTextBuffer.PutString('READY.');
            FTextBuffer.NewLine;
          end
          else
          begin
            // Unrecognized system command
            FTextBuffer.PutString('?UNKNOWN COMMAND');
            FTextBuffer.NewLine;
          end;

          // Note: LIST doesn't show READY, only RUN and NEW
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

procedure TSedaiConsole.ExecuteConsoleCommand(AST: TASTNode);
var
  i, j: Integer;
  CmdNode: TASTNode;
  CmdName: string;
  Lines: TStringList;
begin
  if not Assigned(AST) then
    Exit;

  // Program can contain multiple statements
  if AST.NodeType = antProgram then
  begin
    for i := 0 to AST.ChildCount - 1 do
    begin
      CmdNode := AST.Child[i];

      // Get command from token or node value
      if Assigned(CmdNode.Token) then
        CmdName := UpperCase(CmdNode.Token.Value)
      else if not VarIsEmpty(CmdNode.Value) then
        CmdName := UpperCase(VarToStr(CmdNode.Value))
      else
        CmdName := '';

      // Commands supported by BASIC interpreter
      if CmdName = 'LIST' then
      begin
        // List program in memory
        if not Assigned(FProgramMemory) then
        begin
          FTextBuffer.PutString('?INTERNAL ERROR');
          FTextBuffer.NewLine;
          Exit;
        end;

        Lines := FProgramMemory.GetAllLines;
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
              FTextBuffer.PutString(Lines[j]);
              FTextBuffer.NewLine;
            end;
          end;
        finally
          Lines.Free;
        end;
      end
      else if CmdName = 'NEW' then
      begin
        // Clear program in memory
        FProgramMemory.Clear;
      end
      else if CmdName = 'RUN' then
      begin
        // TODO: Execute program
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
        // TODO: Execute PRINT directly
        FTextBuffer.PutString('?NOT IMPLEMENTED');
        FTextBuffer.NewLine;
      end
      else
      begin
        // Unrecognized command
        FTextBuffer.PutString('?SYNTAX ERROR');
        FTextBuffer.NewLine;
      end;
    end;
  end;
end;

procedure TSedaiConsole.Run;
var
  CurrentLine: string;
begin
  FRunning := True;

  ShowSplashScreen;

  while FRunning do
  begin
    FInputHandler.ProcessEvents;

    if FInputHandler.QuitRequested then
      FRunning := False;

    // Note: CTRL+C now stops BASIC programs, not exits console
    // CTRL+ALT+END exits SedaiVision (handled in ProcessEvents -> QuitRequested)

    // CTRL+F to toggle fullscreen
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
      CurrentLine := FTextBuffer.GetCurrentLine;
      FTextBuffer.NewLine;  // New line after input
      ProcessCommand(CurrentLine);
    end
    else if FInputHandler.LastKeyDown = SDLK_BACKSPACE then
    begin
      FTextBuffer.DeleteChar;
    end
    else if FInputHandler.LastKeyDown = SDLK_UP then
    begin
      // History: UP arrow
      if (FHistoryCount > 0) and (FHistoryPos < FHistoryCount - 1) then
      begin
        Inc(FHistoryPos);
        // Clear current line
        FTextBuffer.ClearCurrentLine;
        // Insert command from history
        FTextBuffer.PutString(FInputHistory[FHistoryCount - 1 - FHistoryPos]);
      end;
    end
    else if FInputHandler.LastKeyDown = SDLK_DOWN then
    begin
      // History: DOWN arrow
      if FHistoryPos > 0 then
      begin
        Dec(FHistoryPos);
        // Clear current line
        FTextBuffer.ClearCurrentLine;
        // Insert command from history
        FTextBuffer.PutString(FInputHistory[FHistoryCount - 1 - FHistoryPos]);
      end
      else if FHistoryPos = 0 then
      begin
        FHistoryPos := -1;
        // Clear current line
        FTextBuffer.ClearCurrentLine;
      end;
    end
    else if (FInputHandler.LastKeyDown = SDLK_PAGEUP) and FInputHandler.CtrlPressed then
    begin
      // Pager: delegate to VideoController with CTRL
      FVideoController.HandleScrollKeys(SDLK_PAGEUP, 0);
    end
    else if (FInputHandler.LastKeyDown = SDLK_PAGEDOWN) and FInputHandler.CtrlPressed then
    begin
      // Pager: delegate to VideoController with CTRL
      FVideoController.HandleScrollKeys(SDLK_PAGEDOWN, 0);
    end
    else if FInputHandler.HasChar then
    begin
      FTextBuffer.PutChar(FInputHandler.LastChar);
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
begin
  InputBuffer := '';
  Done := False;
  
  // Show prompt if provided
  if Prompt <> '' then
  begin
    FTextBuffer.PutString(Prompt);
    // Immediate render to show prompt WITH CURSOR
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
            // Check for CTRL+END to stop program even during INPUT
            if (Event.key.keysym.sym = SDLK_END) and
               ((Event.key.keysym.mod_ and KMOD_CTRL) <> 0) then
            begin
              if (Event.key.keysym.mod_ and KMOD_ALT) <> 0 then
              begin
                // CTRL+ALT+END: Exit completely
                FQuitRequested := True;
                Done := True;
              end
              else
              begin
                // CTRL+END: Stop BASIC program
                FStopRequested := True;
                Done := True;
              end;
            end
            else
            case Event.key.keysym.sym of
              SDLK_RETURN, SDLK_KP_ENTER:
                begin
                  // New line when pressing ENTER
                  FTextBuffer.NewLine;
                  Done := True;
                end;

              SDLK_BACKSPACE:
                begin
                  if Length(InputBuffer) > 0 then
                  begin
                    Delete(InputBuffer, Length(InputBuffer), 1);
                    FTextBuffer.DeleteChar;  // Visually remove character
                  end;
                end;

              SDLK_ESCAPE:
                begin
                  // Clear all input
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
                IsValidChar := True  // Negative sign only at start
              else if (Ch = '+') and (Length(InputBuffer) = 0) then
                IsValidChar := True  // Positive sign only at start
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
              FTextBuffer.PutChar(Ch);  // Character echo
            end;
          end;
      end;
    end;

    // Render after each event WITH CURSOR
    if Assigned(FConsole) then
    begin
      FConsole.RenderScreen;
      FConsole.UpdateCursor;
      FVideoController.Present;
    end;

    SDL_Delay(10);  // Avoid 100% CPU
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

procedure TInputHandler.EnableTextInput;
begin
  SDL_StartTextInput;
end;

procedure TInputHandler.DisableTextInput;
begin
  SDL_StopTextInput;
end;

end.
