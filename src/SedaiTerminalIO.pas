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
unit SedaiTerminalIO;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

{ ============================================================================
  SedaiTerminalIO - Pure console I/O implementation (no SDL2 dependency)

  This unit provides IOutputDevice and IInputDevice implementations that
  use standard console I/O (WriteLn, ReadLn) without requiring SDL2.

  Use cases:
  - Command-line BASIC programs (batch processing)
  - Headless execution (servers, CI/CD)
  - Quick testing without SDL2 window

  Limitations:
  - No graphics support (SetPixel, GetPixel do nothing)
  - No cursor positioning (SetCursor, MoveCursor do nothing)
  - No color support (SetColors does nothing)
  - No fullscreen mode
  ============================================================================ }

interface

uses
  Classes, SysUtils, SedaiOutputInterface, SedaiGraphicsTypes
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  ;

type
  { TTerminalController - Console output device (stdout) }
  { NOTE: Using TObject instead of TInterfacedObject because interfaces use CORBA mode (no refcount) }
  TTerminalController = class(TObject, IOutputDevice)
  private
    FInitialized: Boolean;
    FCursorX, FCursorY: Integer;
    FCols, FRows: Integer;
  public
    constructor Create;

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
    function HandleScrollKeys(Key: Integer; Modifiers: Integer): Boolean;
    procedure ProcessScrollInput;
    function GetInScrollMode: Boolean;

    // Graphics (stubs - not supported in terminal mode)
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
    function GetPaletteColor(Index: TPaletteIndex): UInt32;
    procedure ResetPalette;

    // Shape drawing (stubs - not supported in terminal mode)
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

    // FAST mode (stubs - not supported in terminal mode)
    procedure SetFastMode(Enabled: Boolean);
    function GetFastMode: Boolean;
    procedure SetFastModeAlpha(Alpha: Byte);
    function GetFastModeAlpha: Byte;

    // Additional graphics interface methods (stubs)
    procedure SetColorSource(Source, Color: Integer);
    procedure SetColorSourceDirect(Source, Color: Integer);
    function GetColorSourceDirect(Source: Integer): Integer;  // RCLR/GETCOLOR
    procedure SetLineWidth(Width: Integer);
    procedure SetScale(Enabled: Boolean; XMax, YMax: Integer);
    procedure FloodFill(Source: Integer; X, Y: Double; Mode: Integer);
    procedure SetWindow(Col1, Row1, Col2, Row2: Integer; DoClear: Boolean);
    function GetWindowLines: Integer;
    function GetWindowCols: Integer;
    function GetScreenWidth: Integer;
    function SaveShape(X1, Y1, X2, Y2: Double): string;
    procedure LoadShape(const Data: string; X, Y: Double; Mode: Integer);
  end;

  { TTerminalInput - Console input device (stdin) }
  { NOTE: Using TObject instead of TInterfacedObject because interfaces use CORBA mode (no refcount) }
  TTerminalInput = class(TObject, IInputDevice)
  private
    FShouldQuit: Boolean;
    FShouldStop: Boolean;
    FPendingChar: Char;
    FHasPendingChar: Boolean;
    {$IFDEF WINDOWS}
    FConsoleHandle: THandle;
    FOldConsoleMode: DWORD;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    // IInputDevice implementation
    function ReadLine(const Prompt: string = ''; IsCommand: Boolean = True;
                     NumericOnly: Boolean = False; AllowDecimal: Boolean = True): string;
    function ReadKey: Char;
    function KeyPressed: Boolean;
    function HasChar: Boolean;
    function GetLastChar: string;
    procedure EnableTextInput;   // No-op for terminal (text input always enabled)
    procedure DisableTextInput;  // No-op for terminal
    function ShouldQuit: Boolean;
    function ShouldStop: Boolean;
    procedure ClearStopRequest;
    procedure ProcessEvents;
    procedure Reset;
  end;

implementation

{$IFDEF WINDOWS}
var
  GCtrlCPressed: Boolean = False;

function ConsoleCtrlHandler(CtrlType: DWORD): BOOL; stdcall;
begin
  case CtrlType of
    CTRL_C_EVENT, CTRL_BREAK_EVENT:
    begin
      GCtrlCPressed := True;
      Result := True;  // We handled it - don't terminate
    end;
  else
    Result := False;  // Let default handler process it
  end;
end;
{$ENDIF}

{ ============================================================================
  TTerminalController
  ============================================================================ }

constructor TTerminalController.Create;
begin
  inherited Create;
  FInitialized := False;
  FCursorX := 0;
  FCursorY := 0;
  FCols := 80;
  FRows := 25;
end;

function TTerminalController.Initialize(const Title: string; Width: Integer; Height: Integer): Boolean;
begin
  FCols := Width;
  FRows := Height;
  FCursorX := 0;
  FCursorY := 0;
  FInitialized := True;
  Result := True;
end;

procedure TTerminalController.Shutdown;
begin
  FInitialized := False;
end;

function TTerminalController.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

procedure TTerminalController.Print(const Text: string; ClearBackground: Boolean);
begin
  System.Write(Text);
  Inc(FCursorX, Length(Text));
end;

procedure TTerminalController.PrintLn(const Text: string; ClearBackground: Boolean);
begin
  System.WriteLn(Text);
  FCursorX := 0;
  Inc(FCursorY);
end;

procedure TTerminalController.NewLine;
begin
  System.WriteLn;
  FCursorX := 0;
  Inc(FCursorY);
end;

procedure TTerminalController.Clear;
begin
  // ANSI escape sequence to clear screen (works on most terminals)
  {$IFDEF WINDOWS}
  // On Windows, we could use Windows API but for simplicity just print newlines
  System.WriteLn;
  {$ELSE}
  System.Write(#27'[2J'#27'[H');  // ANSI clear + home
  {$ENDIF}
  FCursorX := 0;
  FCursorY := 0;
end;

procedure TTerminalController.SetCursor(X, Y: Integer);
begin
  FCursorX := X;
  FCursorY := Y;
  // Terminal positioning not implemented (would need ANSI sequences)
end;

procedure TTerminalController.MoveCursor(DeltaX, DeltaY: Integer);
begin
  Inc(FCursorX, DeltaX);
  Inc(FCursorY, DeltaY);
end;

function TTerminalController.GetCursorX: Integer;
begin
  Result := FCursorX;
end;

function TTerminalController.GetCursorY: Integer;
begin
  Result := FCursorY;
end;

procedure TTerminalController.ShowCursor(X, Y: Integer);
begin
  // Not applicable for terminal
end;

procedure TTerminalController.HideCursor(X, Y: Integer);
begin
  // Not applicable for terminal
end;

procedure TTerminalController.SetColors(Foreground, Background: TColor);
begin
  // Could implement ANSI color codes here if needed
end;

procedure TTerminalController.Present;
begin
  // Terminal output is immediate, no buffering
end;

procedure TTerminalController.SetFullscreen(Enabled: Boolean);
begin
  // Not applicable for terminal
end;

function TTerminalController.IsFullscreen: Boolean;
begin
  Result := False;
end;

function TTerminalController.ShouldQuit: Boolean;
begin
  Result := False;
end;

function TTerminalController.GetActualCols: Integer;
begin
  Result := FCols;
end;

function TTerminalController.GetActualRows: Integer;
begin
  Result := FRows;
end;

procedure TTerminalController.MarkPromptRow;
begin
  // Not applicable for terminal
end;

procedure TTerminalController.OnUserInput;
begin
  // Not applicable for terminal
end;

function TTerminalController.HandleScrollKeys(Key: Integer; Modifiers: Integer): Boolean;
begin
  Result := False;
end;

procedure TTerminalController.ProcessScrollInput;
begin
  // Not applicable for terminal
end;

function TTerminalController.GetInScrollMode: Boolean;
begin
  Result := False;
end;

// Graphics stubs - not supported in terminal mode

function TTerminalController.SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean; SplitLine: Integer): Boolean;
begin
  Result := False;  // Graphics not supported
end;

function TTerminalController.GetGraphicMode: TGraphicMode;
begin
  Result := gm40ColText;  // Terminal always in text mode
end;

function TTerminalController.IsInGraphicsMode: Boolean;
begin
  Result := False;
end;

procedure TTerminalController.ClearScreen(Mode: Integer);
begin
  // In terminal mode, just clear the screen
  Clear;
end;

procedure TTerminalController.SetPixel(X, Y: Integer; RGB: UInt32);
begin
  // Not supported in terminal mode
end;

procedure TTerminalController.SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex);
begin
  // Not supported in terminal mode
end;

function TTerminalController.GetPixel(X, Y: Integer): UInt32;
begin
  Result := 0;
end;

function TTerminalController.GetPixelIndex(X, Y: Integer): TPaletteIndex;
begin
  Result := 0;
end;

procedure TTerminalController.EnablePalette(Enable: Boolean);
begin
  // Not supported in terminal mode
end;

function TTerminalController.IsPaletteEnabled: Boolean;
begin
  Result := False;
end;

procedure TTerminalController.SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
begin
  // Not supported in terminal mode
end;

function TTerminalController.GetPaletteColor(Index: TPaletteIndex): UInt32;
begin
  Result := 0;
end;

procedure TTerminalController.ResetPalette;
begin
  // Not supported in terminal mode
end;

// Shape drawing stubs - not supported in terminal mode

procedure TTerminalController.DrawBox(X1, Y1, X2, Y2: Integer; Angle: Double);
begin
  // Not supported in terminal mode
end;

procedure TTerminalController.DrawBoxStyled(X1, Y1, X2, Y2: Integer; const Style: TShapeStyle; Angle: Double);
begin
  // Not supported in terminal mode
end;

procedure TTerminalController.DrawBoxWithColor(X1, Y1, X2, Y2: Integer; Color: UInt32; Angle: Double; Filled: Boolean);
begin
  // Not supported in terminal mode
end;

procedure TTerminalController.DrawCircleWithColor(X, Y, XR, YR: Integer; Color: UInt32;
                                                  SA, EA, Angle, Inc: Double);
begin
  // Not supported in terminal mode
end;

procedure TTerminalController.DrawLine(X1, Y1, X2, Y2: Integer; Color: UInt32);
begin
  // Not supported in terminal mode
end;

procedure TTerminalController.SetPixelCursor(X, Y: Integer);
begin
  // Not supported in terminal mode (no pixel cursor)
end;

function TTerminalController.GetPixelCursorX: Integer;
begin
  Result := 0;  // No pixel cursor in terminal mode
end;

function TTerminalController.GetPixelCursorY: Integer;
begin
  Result := 0;  // No pixel cursor in terminal mode
end;

procedure TTerminalController.SetBorderStyle(const Style: TBorderStyle);
begin
  // Not supported in terminal mode
end;

procedure TTerminalController.SetFillStyle(const Style: TFillStyleDef);
begin
  // Not supported in terminal mode
end;

function TTerminalController.GetBorderStyle: TBorderStyle;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function TTerminalController.GetFillStyle: TFillStyleDef;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ FAST mode - stubs for terminal mode }
procedure TTerminalController.SetFastMode(Enabled: Boolean);
begin
  // Not supported in terminal mode - FAST/SLOW are C128 hardware commands
end;

function TTerminalController.GetFastMode: Boolean;
begin
  Result := False;
end;

procedure TTerminalController.SetFastModeAlpha(Alpha: Byte);
begin
  // Not supported in terminal mode
end;

function TTerminalController.GetFastModeAlpha: Byte;
begin
  Result := 255;
end;

// Additional graphics interface methods (stubs for terminal mode)

procedure TTerminalController.SetColorSource(Source, Color: Integer);
begin
  // Not supported in terminal mode
end;

procedure TTerminalController.SetColorSourceDirect(Source, Color: Integer);
begin
  // Not supported in terminal mode
end;

function TTerminalController.GetColorSourceDirect(Source: Integer): Integer;
begin
  Result := 0;
end;

procedure TTerminalController.SetLineWidth(Width: Integer);
begin
  // Not supported in terminal mode
end;

procedure TTerminalController.SetScale(Enabled: Boolean; XMax, YMax: Integer);
begin
  // Not supported in terminal mode
end;

procedure TTerminalController.FloodFill(Source: Integer; X, Y: Double; Mode: Integer);
begin
  // Not supported in terminal mode
end;

procedure TTerminalController.SetWindow(Col1, Row1, Col2, Row2: Integer; DoClear: Boolean);
begin
  // Not supported in terminal mode
end;

function TTerminalController.GetWindowLines: Integer;
begin
  Result := FRows;
end;

function TTerminalController.GetWindowCols: Integer;
begin
  Result := FCols;
end;

function TTerminalController.GetScreenWidth: Integer;
begin
  Result := FCols;
end;

function TTerminalController.SaveShape(X1, Y1, X2, Y2: Double): string;
begin
  Result := '';  // Not supported in terminal mode
end;

procedure TTerminalController.LoadShape(const Data: string; X, Y: Double; Mode: Integer);
begin
  // Not supported in terminal mode
end;

{ ============================================================================
  TTerminalInput - Windows console input with CTRL+C handling
  ============================================================================ }

constructor TTerminalInput.Create;
begin
  inherited Create;
  FShouldQuit := False;
  FShouldStop := False;
  FPendingChar := #0;
  FHasPendingChar := False;

  {$IFDEF WINDOWS}
  // Get console input handle
  FConsoleHandle := GetStdHandle(STD_INPUT_HANDLE);

  // Save old console mode - we keep ENABLE_PROCESSED_INPUT for proper character translation
  if FConsoleHandle <> INVALID_HANDLE_VALUE then
    GetConsoleMode(FConsoleHandle, @FOldConsoleMode);

  // Install CTRL+C handler - this intercepts CTRL+C before it terminates the process
  // The handler sets GCtrlCPressed which we check in ProcessEvents
  GCtrlCPressed := False;
  SetConsoleCtrlHandler(@ConsoleCtrlHandler, True);

  // Flush any pending input events (e.g., ENTER from command line)
  if FConsoleHandle <> INVALID_HANDLE_VALUE then
    FlushConsoleInputBuffer(FConsoleHandle);
  {$ENDIF}
end;

destructor TTerminalInput.Destroy;
begin
  {$IFDEF WINDOWS}
  // Restore original console mode
  if FConsoleHandle <> INVALID_HANDLE_VALUE then
    SetConsoleMode(FConsoleHandle, FOldConsoleMode);

  // Remove CTRL+C handler
  SetConsoleCtrlHandler(@ConsoleCtrlHandler, False);
  {$ENDIF}

  inherited Destroy;
end;

function TTerminalInput.ReadLine(const Prompt: string; IsCommand: Boolean;
                                 NumericOnly: Boolean; AllowDecimal: Boolean): string;
var
  Input: string;
  IsValid: Boolean;
  TempFloat: Double;
  {$IFDEF WINDOWS}
  NormalMode: DWORD;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Ensure console is in normal line input mode with echo
  if FConsoleHandle <> INVALID_HANDLE_VALUE then
  begin
    NormalMode := ENABLE_LINE_INPUT or ENABLE_ECHO_INPUT or ENABLE_PROCESSED_INPUT;
    SetConsoleMode(FConsoleHandle, NormalMode);
  end;
  {$ENDIF}

  repeat
    IsValid := True;
    Input := '';

    // Display prompt
    if Prompt <> '' then
      System.Write(Prompt);

    // Simple blocking ReadLn
    System.ReadLn(Input);

    // Validate if numeric only
    if NumericOnly and (Input <> '') then
    begin
      if not TryStrToFloat(Input, TempFloat) then
      begin
        System.WriteLn('?SYNTAX ERROR - Number expected');
        IsValid := False;
      end
      else if not AllowDecimal then
      begin
        if Pos('.', Input) > 0 then
        begin
          System.WriteLn('?SYNTAX ERROR - Integer expected');
          IsValid := False;
        end;
      end;
    end;
  until IsValid;

  Result := Input;
end;

function TTerminalInput.ReadKey: Char;
begin
  if FHasPendingChar then
  begin
    Result := FPendingChar;
    FHasPendingChar := False;
    FPendingChar := #0;
  end
  else
  begin
    // Wait for a key
    repeat
      ProcessEvents;
      if FShouldStop or FShouldQuit then
      begin
        Result := #0;
        Exit;
      end;
      Sleep(10);
    until FHasPendingChar;

    Result := FPendingChar;
    FHasPendingChar := False;
    FPendingChar := #0;
  end;
end;

function TTerminalInput.KeyPressed: Boolean;
begin
  if FHasPendingChar then
  begin
    Result := True;
    Exit;
  end;

  ProcessEvents;
  Result := FHasPendingChar;
end;

function TTerminalInput.HasChar: Boolean;
begin
  if FHasPendingChar then
  begin
    Result := True;
    Exit;
  end;

  ProcessEvents;
  Result := FHasPendingChar;
end;

function TTerminalInput.GetLastChar: string;
begin
  if FHasPendingChar then
  begin
    Result := FPendingChar;
    FHasPendingChar := False;
    FPendingChar := #0;
  end
  else
    Result := '';
end;

function TTerminalInput.ShouldQuit: Boolean;
begin
  Result := FShouldQuit;
end;

function TTerminalInput.ShouldStop: Boolean;
begin
  // Check global CTRL+C flag
  {$IFDEF WINDOWS}
  if GCtrlCPressed then
  begin
    GCtrlCPressed := False;
    FShouldStop := True;
  end;
  {$ENDIF}
  Result := FShouldStop;
end;

procedure TTerminalInput.ClearStopRequest;
begin
  FShouldStop := False;
  {$IFDEF WINDOWS}
  GCtrlCPressed := False;
  {$ENDIF}
end;

procedure TTerminalInput.ProcessEvents;
{$IFDEF WINDOWS}
var
  NumEvents, NumRead: DWORD;
  InputRec: TInputRecord;
  Ch: Char;
{$ENDIF}
begin
  // Check global CTRL+C flag first
  {$IFDEF WINDOWS}
  if GCtrlCPressed then
  begin
    GCtrlCPressed := False;
    FShouldStop := True;
    Exit;
  end;

  // If we already have a pending char, don't read more
  if FHasPendingChar then
    Exit;

  if FConsoleHandle = INVALID_HANDLE_VALUE then
    Exit;

  // Check if there are console events available (non-blocking)
  if not GetNumberOfConsoleInputEvents(FConsoleHandle, @NumEvents) then
    Exit;

  while NumEvents > 0 do
  begin
    // Read the event
    if not ReadConsoleInput(FConsoleHandle, @InputRec, 1, @NumRead) then
      Break;

    if NumRead = 0 then
      Break;

    Dec(NumEvents);

    // Only process key down events
    if (InputRec.EventType = KEY_EVENT) and InputRec.Event.KeyEvent.bKeyDown then
    begin
      Ch := InputRec.Event.KeyEvent.AsciiChar;

      // With ENABLE_PROCESSED_INPUT, CTRL+C is handled by ConsoleCtrlHandler
      // and doesn't arrive here. But check for it anyway as a fallback.
      if Ch = #3 then
      begin
        FShouldStop := True;
        Exit;
      end;

      // Check for printable character (includes space, letters, numbers, etc.)
      if Ch >= ' ' then
      begin
        FPendingChar := Ch;
        FHasPendingChar := True;
        Exit;
      end
      else if (Ch <> #0) and (Ch >= ' ') then
      begin
        // This shouldn't happen (Ch >= ' ' is handled above), but just in case
        FPendingChar := Ch;
        FHasPendingChar := True;
        Exit;
      end
      else if Ch = #13 then
      begin
        // Enter key - handle explicitly
        FPendingChar := #13;
        FHasPendingChar := True;
        Exit;
      end
      else if Ch = #27 then
      begin
        // Escape key
        FPendingChar := #27;
        FHasPendingChar := True;
        Exit;
      end
      else if Ch = #8 then
      begin
        // Backspace
        FPendingChar := #8;
        FHasPendingChar := True;
        Exit;
      end
      else if Ch = #9 then
      begin
        // Tab
        FPendingChar := #9;
        FHasPendingChar := True;
        Exit;
      end
      else
      begin
        // Ch = #0: Handle special keys by virtual key code
        // Skip modifier keys (they generate events but no character)
        case InputRec.Event.KeyEvent.wVirtualKeyCode of
          $10, $11, $12,  // VK_SHIFT, VK_CONTROL, VK_MENU (Alt)
          $A0, $A1, $A2, $A3, $A4, $A5, // VK_LSHIFT, VK_RSHIFT, VK_LCONTROL, VK_RCONTROL, VK_LMENU, VK_RMENU
          $14, $90, $91:  // VK_CAPITAL, VK_NUMLOCK, VK_SCROLL
            ; // Ignore modifier keys, continue reading events
          VK_RETURN:
          begin
            FPendingChar := #13;
            FHasPendingChar := True;
            Exit;
          end;
          VK_ESCAPE:
          begin
            FPendingChar := #27;
            FHasPendingChar := True;
            Exit;
          end;
          VK_BACK:
          begin
            FPendingChar := #8;
            FHasPendingChar := True;
            Exit;
          end;
          VK_TAB:
          begin
            FPendingChar := #9;
            FHasPendingChar := True;
            Exit;
          end;
          VK_SPACE:
          begin
            FPendingChar := ' ';
            FHasPendingChar := True;
            Exit;
          end;
        end;
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TTerminalInput.Reset;
begin
  FShouldQuit := False;
  FShouldStop := False;
  FPendingChar := #0;
  FHasPendingChar := False;
  {$IFDEF WINDOWS}
  GCtrlCPressed := False;
  {$ENDIF}
end;

procedure TTerminalInput.EnableTextInput;
begin
  // Clear any pending character state
  FHasPendingChar := False;
  FPendingChar := #0;
end;

procedure TTerminalInput.DisableTextInput;
begin
  // No-op for terminal - text input is always enabled
end;

end.
