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
  Classes, SysUtils, SedaiOutputInterface, SedaiGraphicsTypes;

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
    procedure SetPixel(X, Y: Integer; RGB: UInt32); overload;
    procedure SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex); overload;
    function GetPixel(X, Y: Integer): UInt32;
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
  end;

  { TTerminalInput - Console input device (stdin) }
  { NOTE: Using TObject instead of TInterfacedObject because interfaces use CORBA mode (no refcount) }
  TTerminalInput = class(TObject, IInputDevice)
  private
    FShouldQuit: Boolean;
    FShouldStop: Boolean;
  public
    constructor Create;

    // IInputDevice implementation
    function ReadLine(const Prompt: string = ''; IsCommand: Boolean = True;
                     NumericOnly: Boolean = False; AllowDecimal: Boolean = True): string;
    function ReadKey: Char;
    function KeyPressed: Boolean;
    function ShouldQuit: Boolean;
    function ShouldStop: Boolean;
    procedure ClearStopRequest;
    procedure ProcessEvents;
    procedure Reset;
  end;

implementation

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

{ ============================================================================
  TTerminalInput
  ============================================================================ }

constructor TTerminalInput.Create;
begin
  inherited Create;
  FShouldQuit := False;
end;

function TTerminalInput.ReadLine(const Prompt: string; IsCommand: Boolean;
                                 NumericOnly: Boolean; AllowDecimal: Boolean): string;
var
  Input: string;
  IsValid: Boolean;
  TempFloat: Double;
begin
  repeat
    IsValid := True;

    // Display prompt
    if Prompt <> '' then
      System.Write(Prompt);

    // Read input
    System.ReadLn(Input);

    // Validate if numeric only
    if NumericOnly and (Input <> '') then
    begin
      // Check if valid number
      if not TryStrToFloat(Input, TempFloat) then
      begin
        System.WriteLn('?SYNTAX ERROR - Number expected');
        IsValid := False;
      end
      else if not AllowDecimal then
      begin
        // Check if it contains a decimal point
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
  Result := #0;
  // Not implemented for simple console
  // Would need platform-specific non-blocking input
end;

function TTerminalInput.KeyPressed: Boolean;
begin
  Result := False;
  // Not implemented for simple console
end;

function TTerminalInput.ShouldQuit: Boolean;
begin
  Result := FShouldQuit;
end;

function TTerminalInput.ShouldStop: Boolean;
begin
  Result := FShouldStop;
end;

procedure TTerminalInput.ClearStopRequest;
begin
  FShouldStop := False;
end;

procedure TTerminalInput.ProcessEvents;
begin
  // Not needed for simple console
end;

procedure TTerminalInput.Reset;
begin
  FShouldQuit := False;
  FShouldStop := False;
end;

end.
