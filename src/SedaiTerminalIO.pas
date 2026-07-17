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
  Classes, SysUtils, SedaiOutputInterface, SedaiGraphicsTypes,
  SedaiGraphicsMemory, SedaiGraphicsPrimitives, SedaiConsoleState
  {$IFDEF WINDOWS}
  , Windows
  {$ELSE}
  , termio
  {$ENDIF}
  ;

type
  { TTermCell - one modelled screen cell (see FCells) }
  TTermCell = record
    Ch: Char;
    Fg, Bg: Byte;   // palette indices
  end;

  { TTerminalController - Console output device (stdout) }
  { NOTE: Using TObject instead of TInterfacedObject because interfaces use CORBA mode (no refcount) }
  TTerminalController = class(TObject, IOutputDevice)
  private
    FInitialized: Boolean;
    FCursorX, FCursorY: Integer;
    FCols, FRows: Integer;
    // A model of the visible screen, so a program can read back what it printed: SCREEN(row, col) and
    // the C128 screen RAM at $0400 both go through GetCharAt/GetColorAt. Text still goes straight to
    // stdout -- this mirrors it, cell by cell, wrapping at the right margin and scrolling at the bottom.
    //
    // It carries its own caret (FCellX/FCellY) and deliberately does NOT touch FCursorX/FCursorY, which
    // count what was handed to the terminal and are what POS/CSRLIN report. Those two grow without
    // bound and never scroll; the model's caret stays inside the grid. Keeping them apart means adding
    // the model changes no existing console behaviour.
    FCells: array of array of TTermCell;
    FCellX, FCellY: Integer;   // the model's caret: always within the print area below
    // The print area (VIEW PRINT / the C128 WINDOW): text wraps at its right edge and scrolls at its
    // bottom, leaving the rest of the screen alone. Defaults to the whole grid, which is why adding it
    // changes nothing for a program that never asks for one.
    FViewTop, FViewBottom, FViewLeft, FViewRight: Integer;   // 0-based, inclusive
    // Optional shared graphics framebuffer (sb --window): when attached, the C128 graphics commands
    // render into it (the software backend's surface, which the window presenter mirrors). nil = the
    // usual headless terminal (graphics are no-ops). Not owned. Viewport-only — no text-on-graphics.
    FGfxMem: TGraphicsMemory;
    FGfxMode: TGraphicMode;
    FBgIdx, FFgIdx, FMc1Idx, FMc2Idx: Byte;   // C128 colour sources -> palette indices
    FColorSrc: array[0..6] of Integer;        // RCLR/GETCOLOR readback
    FPcX, FPcY: Integer;                       // pixel cursor (LOCATE/DRAW)
    function ResolveC128Color(Source: UInt32; out UseIndex: Boolean): UInt32;
    // The modelled screen (see FCells)
    procedure AllocCells;
    procedure ClearView;
    procedure ScrollCellsUp;
    procedure CellNextRow;
    procedure PutCells(const Text: string);
  public
    constructor Create;
    // sb --window: attach the shared graphics surface so C128 graphics draw into the window.
    procedure AttachGraphicsMemory(Mem: TGraphicsMemory);

    // IOutputDevice implementation
    function Initialize(const Title: string = ''; Width: Integer = 80; Height: Integer = 25): Boolean;
    procedure Shutdown;
    function IsInitialized: Boolean;

    procedure Print(const Text: string; ClearBackground: Boolean = False);
    procedure PrintLn(const Text: string; ClearBackground: Boolean = False);
    procedure NewLine;
    procedure Clear;
    procedure ResetPrintState;
    function IsScreenVisible: Boolean;

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
    procedure SetPaletteColorRGBA(Index: TPaletteIndex; R, G, B: Byte; A: Byte = 255);
    function GetPaletteColor(Index: TPaletteIndex): UInt32;
    procedure ResetPalette;
    function LoadPaletteFromJSON(const FileName: string): Boolean;
    function SavePaletteToJSON(const FileName: string): Boolean;
    function GetLastPaletteError: string;

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
    // Screen memory access (stubs - not supported in terminal mode)
    function GetCharAt(Col, Row: Integer): Byte;
    procedure SetCharAt(Col, Row: Integer; Ch: Byte);
    function GetColorAt(Col, Row: Integer): Byte;
    function GetBackColorAt(Col, Row: Integer): Byte;
    procedure SetColorAt(Col, Row: Integer; Color: Byte);
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
    FOwnsConsoleInput: Boolean;   // stdin is a real console: its mode is ours to change, and to put back
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
  FBgIdx := 0; FFgIdx := 1;   // the C128 colour sources' defaults, so blank cells start with them
  AllocCells;
end;

{ === The modelled screen (see FCells) === }

procedure TTerminalController.AllocCells;
begin
  if (FCols <= 0) or (FRows <= 0) then Exit;
  SetLength(FCells, FRows, FCols);
  // A fresh screen has no print area of its own: it is the whole grid.
  FViewTop := 0; FViewBottom := FRows - 1;
  FViewLeft := 0; FViewRight := FCols - 1;
  ClearView;
end;

procedure TTerminalController.ClearView;
var
  R, C: Integer;
begin
  if Length(FCells) = 0 then Exit;
  for R := FViewTop to FViewBottom do
    for C := FViewLeft to FViewRight do
    begin
      FCells[R][C].Ch := ' ';
      FCells[R][C].Fg := FFgIdx;
      FCells[R][C].Bg := FBgIdx;
    end;
  FCellX := FViewLeft;
  FCellY := FViewTop;
end;

procedure TTerminalController.ScrollCellsUp;
var
  R, C: Integer;
begin
  for R := FViewTop to FViewBottom - 1 do
    for C := FViewLeft to FViewRight do
      FCells[R][C] := FCells[R + 1][C];
  for C := FViewLeft to FViewRight do
  begin
    FCells[FViewBottom][C].Ch := ' ';
    FCells[FViewBottom][C].Fg := FFgIdx;
    FCells[FViewBottom][C].Bg := FBgIdx;
  end;
end;

procedure TTerminalController.CellNextRow;
begin
  FCellX := FViewLeft;
  if FCellY >= FViewBottom then
    ScrollCellsUp          // at the area's bottom the area scrolls; the caret stays on its last row
  else
    Inc(FCellY);
end;

procedure TTerminalController.PutCells(const Text: string);
var
  i: Integer;
  Ch: Char;
begin
  if Length(FCells) = 0 then Exit;
  for i := 1 to Length(Text) do
  begin
    Ch := Text[i];
    if Ch = #13 then
      FCellX := FViewLeft
    else if Ch = #10 then
      CellNextRow
    else
    begin
      if FCellX > FViewRight then CellNextRow;   // wrap at the print area's right edge
      FCells[FCellY][FCellX].Ch := Ch;
      FCells[FCellY][FCellX].Fg := FFgIdx;
      FCells[FCellY][FCellX].Bg := FBgIdx;
      Inc(FCellX);
    end;
  end;
end;

function TTerminalController.Initialize(const Title: string; Width: Integer; Height: Integer): Boolean;
begin
  FCols := Width;
  FRows := Height;
  FCursorX := 0;
  FCursorY := 0;
  AllocCells;   // the modelled screen follows the requested geometry
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
  PutCells(Text);
  Inc(FCursorX, Length(Text));
end;

procedure TTerminalController.PrintLn(const Text: string; ClearBackground: Boolean);
begin
  System.WriteLn(Text);
  PutCells(Text);
  CellNextRow;
  FCursorX := 0;
  Inc(FCursorY);
end;

procedure TTerminalController.NewLine;
begin
  System.WriteLn;
  CellNextRow;
  FCursorX := 0;
  Inc(FCursorY);
end;

function StdoutIsTerminal: Boolean;
// Is stdout a real console, or has it been redirected into a file or a pipe? CLS has a visible effect
// only on the former; on the latter there is no screen to clear, and whatever we emit to "clear" it is
// just garbage IN the data -- FreeBASIC writes nothing at all there. We emitted it unconditionally: a
// stray blank line on Windows, and on Unix a raw ANSI escape sequence written straight into the file.
begin
  {$IFDEF WINDOWS}
  Result := GetFileType(GetStdHandle(STD_OUTPUT_HANDLE)) = FILE_TYPE_CHAR;
  {$ELSE}
  Result := IsATTY(StdOutputHandle) <> 0;
  {$ENDIF}
end;

function TTerminalController.IsScreenVisible: Boolean;
begin
  // The headless CLI writes to a real screen only when stdout is a console; a redirected file/pipe has no
  // visible cells, so MODERN SPC/TAB emit nothing there (matching FreeBASIC). Same signal as Clear uses.
  Result := StdoutIsTerminal;
end;

procedure TTerminalController.Clear;
begin
  // Only a real terminal gets the clear; a redirected stream gets nothing (see StdoutIsTerminal). The
  // MODELLED screen is cleared either way -- that is what CSRLIN/POS and the cell grid read back.
  if StdoutIsTerminal then
  begin
    {$IFDEF WINDOWS}
    System.WriteLn;
    {$ELSE}
    System.Write(#27'[2J'#27'[H');  // ANSI clear + home
    {$ENDIF}
  end;
  ClearView;    // CLS blanks the print area and homes the caret inside it, leaving the area itself set
  FCursorX := 0;
  FCursorY := 0;
end;

procedure TTerminalController.ResetPrintState;
begin
  // Terminal doesn't support PETSCII reverse mode, nothing to reset
end;

procedure TTerminalController.SetCursor(X, Y: Integer);
begin
  FCursorX := X;
  FCursorY := Y;
  // Terminal positioning not implemented (would need ANSI sequences), but the modelled screen follows,
  // clamped to the grid, so LOCATE + PRINT + SCREEN(row, col) read back what the program meant to write.
  if Length(FCells) > 0 then
  begin
    FCellX := X; FCellY := Y;
    if FCellX < 0 then FCellX := 0 else if FCellX > FCols - 1 then FCellX := FCols - 1;
    if FCellY < 0 then FCellY := 0 else if FCellY > FRows - 1 then FCellY := FRows - 1;
  end;
end;

procedure TTerminalController.MoveCursor(DeltaX, DeltaY: Integer);
begin
  Inc(FCursorX, DeltaX);
  Inc(FCursorY, DeltaY);
  SetCursor(FCursorX, FCursorY);
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
  // Flush stdout so text appears before blocking operations (e.g. SOUND, PLAY)
  Flush(System.Output);
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

// Graphics: no-op stubs in plain terminal mode; render into the attached surface (sb --window).

procedure TTerminalController.AttachGraphicsMemory(Mem: TGraphicsMemory);
begin
  FGfxMem := Mem;
  FGfxMode := gm40ColText;
  FBgIdx := 0; FFgIdx := 1; FMc1Idx := 2; FMc2Idx := 3;
end;

function TTerminalController.ResolveC128Color(Source: UInt32; out UseIndex: Boolean): UInt32;
// C128 colour model: in classic (paletted) modes the BOX/CIRCLE/LINE "colour" is a source selector
// (0=bg,1=fg,2=mc1,3=mc2) or a direct palette index; in the truecolor mode it is a raw RGBA value.
begin
  UseIndex := FGfxMode <> gmSDL2Dynamic;
  if UseIndex then
    case Source of
      0: Result := FBgIdx;
      1: Result := FFgIdx;
      2: Result := FMc1Idx;
      3: Result := FMc2Idx;
    else
      if Source < 256 then Result := Source else Result := FFgIdx;
    end
  else
    Result := Source;
end;

function TTerminalController.SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean; SplitLine: Integer): Boolean;
begin
  if FGfxMem = nil then Exit(False);   // headless terminal: graphics unsupported
  FGfxMode := Mode;
  FGfxMem.AllocateBuffers(0, 0, Mode <> gmSDL2Dynamic, Mode);
  if ClearBuffer then FGfxMem.ClearCurrentModeWithIndex(FBgIdx);
  Result := True;
end;

function TTerminalController.GetGraphicMode: TGraphicMode;
begin
  if FGfxMem <> nil then Result := FGfxMode else Result := gm40ColText;
end;

function TTerminalController.IsInGraphicsMode: Boolean;
begin
  Result := (FGfxMem <> nil) and not (FGfxMode in [gm40ColText, gm80ColText, gm80x50Text]);
end;

procedure TTerminalController.ClearScreen(Mode: Integer);
begin
  if IsInGraphicsMode then
    FGfxMem.ClearCurrentModeWithIndex(FBgIdx)   // SCNCLR in graphics: clear the viewport to background
  else
    Clear;                                        // text mode: clear the console
end;

procedure TTerminalController.SetPixel(X, Y: Integer; RGB: UInt32);
var UseIdx: Boolean; C: UInt32;
begin
  if FGfxMem = nil then Exit;
  C := ResolveC128Color(RGB, UseIdx);
  if UseIdx then FGfxMem.SetPixel(X, Y, TPaletteIndex(C)) else FGfxMem.SetPixel(X, Y, C);
end;

procedure TTerminalController.SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex);
begin
  if FGfxMem <> nil then FGfxMem.SetPixel(X, Y, PaletteIndex);
end;

function TTerminalController.GetPixel(X, Y: Integer): UInt32;
begin
  if FGfxMem <> nil then Result := FGfxMem.GetPixel(X, Y) else Result := 0;
end;

function TTerminalController.GetPixelIndex(X, Y: Integer): TPaletteIndex;
begin
  if FGfxMem <> nil then Result := FGfxMem.GetPixelIndex(X, Y) else Result := 0;
end;

procedure TTerminalController.EnablePalette(Enable: Boolean);
begin
  if FGfxMem <> nil then FGfxMem.EnablePalette(Enable);
end;

function TTerminalController.IsPaletteEnabled: Boolean;
begin
  Result := False;
end;

procedure TTerminalController.SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
begin
  if FGfxMem <> nil then FGfxMem.SetPaletteColor(Index, RGB);
end;

function TTerminalController.GetPaletteColor(Index: TPaletteIndex): UInt32;
begin
  if FGfxMem <> nil then Result := FGfxMem.GetPaletteColor(Index) else Result := 0;
end;

procedure TTerminalController.ResetPalette;
begin
  if FGfxMem <> nil then FGfxMem.ResetPalette;
end;

procedure TTerminalController.SetPaletteColorRGBA(Index: TPaletteIndex; R, G, B: Byte; A: Byte = 255);
begin
  if FGfxMem <> nil then FGfxMem.SetPaletteColorRGBA(Index, R, G, B, A);
end;

function TTerminalController.LoadPaletteFromJSON(const FileName: string): Boolean;
begin
  Result := False;  // Not supported in terminal mode
end;

function TTerminalController.SavePaletteToJSON(const FileName: string): Boolean;
begin
  Result := False;  // Not supported in terminal mode
end;

function TTerminalController.GetLastPaletteError: string;
begin
  Result := 'Palette operations not supported in terminal mode';
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
var UseIdx: Boolean; C: UInt32;
begin
  if FGfxMem = nil then Exit;
  C := ResolveC128Color(Color, UseIdx);
  DrawBoxToMemory(FGfxMem, X1, Y1, X2, Y2, C, UseIdx, Filled, 1, Angle, FGfxMem.State.Width, FGfxMem.State.Height);
end;

procedure TTerminalController.DrawCircleWithColor(X, Y, XR, YR: Integer; Color: UInt32;
                                                  SA, EA, Angle, Inc: Double);
var UseIdx: Boolean; C: UInt32;
begin
  if FGfxMem = nil then Exit;
  C := ResolveC128Color(Color, UseIdx);
  DrawCircleToMemory(FGfxMem, X, Y, XR, YR, C, UseIdx, SA, EA, Angle, Inc, 1, FGfxMem.State.Width, FGfxMem.State.Height);
end;

procedure TTerminalController.DrawLine(X1, Y1, X2, Y2: Integer; Color: UInt32);
var UseIdx: Boolean; C: UInt32;
begin
  if FGfxMem = nil then Exit;
  C := ResolveC128Color(Color, UseIdx);
  DrawLineToMemory(FGfxMem, X1, Y1, X2, Y2, C, UseIdx, 1, FGfxMem.State.Width, FGfxMem.State.Height);
end;

procedure TTerminalController.SetPixelCursor(X, Y: Integer);
begin
  FPcX := X; FPcY := Y;
end;

function TTerminalController.GetPixelCursorX: Integer;
begin
  Result := FPcX;
end;

function TTerminalController.GetPixelCursorY: Integer;
begin
  Result := FPcY;
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
// C128 COLOR source, color (1-based colour) -> palette index source mapping.
var Idx: Integer;
begin
  if (Source < 0) or (Source > 6) then Exit;
  Idx := Color - 1;
  if Idx < 0 then Idx := 0;
  if Idx > 255 then Idx := 255;
  FColorSrc[Source] := Idx;
  case Source of
    0: FBgIdx := Byte(Idx);
    1: FFgIdx := Byte(Idx);
    2: FMc1Idx := Byte(Idx);
    3: FMc2Idx := Byte(Idx);
  end;
end;

procedure TTerminalController.SetColorSourceDirect(Source, Color: Integer);
// Like SetColorSource but Color is a 0-based palette index (no -1 adjustment).
begin
  if (Source < 0) or (Source > 6) then Exit;
  if Color < 0 then Color := 0;
  if Color > 255 then Color := 255;
  FColorSrc[Source] := Color;
  case Source of
    0: FBgIdx := Byte(Color);
    1: FFgIdx := Byte(Color);
    2: FMc1Idx := Byte(Color);
    3: FMc2Idx := Byte(Color);
  end;
end;

function TTerminalController.GetColorSourceDirect(Source: Integer): Integer;
begin
  if (Source >= 0) and (Source <= 6) then Result := FColorSrc[Source] else Result := 0;
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
// C128 PAINT source, x, y — iterative 4-way flood fill into the attached surface (RGBA, deterministic).
var
  UseIdx: Boolean;
  FillCol, Target: UInt32;
  W, H, Top, SX, SY, CX, CY: Integer;
  Stack: array of record PX, PY: Integer; end;

  procedure Push(AX, AY: Integer);
  begin
    if (AX < 0) or (AY < 0) or (AX >= W) or (AY >= H) then Exit;
    if Top >= Length(Stack) then SetLength(Stack, (Top + 1) * 2);
    Stack[Top].PX := AX; Stack[Top].PY := AY; Inc(Top);
  end;

begin
  if FGfxMem = nil then Exit;
  FillCol := ResolveC128Color(UInt32(Source), UseIdx);
  if UseIdx then FillCol := FGfxMem.GetPaletteColor(TPaletteIndex(FillCol));
  W := FGfxMem.State.Width; H := FGfxMem.State.Height;
  SX := Round(X); SY := Round(Y);
  if (SX < 0) or (SY < 0) or (SX >= W) or (SY >= H) then Exit;
  Target := FGfxMem.GetPixelRGBA(SX, SY);
  if Target = FillCol then Exit;
  Top := 0; SetLength(Stack, 64);
  Push(SX, SY);
  while Top > 0 do
  begin
    Dec(Top);
    CX := Stack[Top].PX; CY := Stack[Top].PY;
    if FGfxMem.GetPixelRGBA(CX, CY) <> Target then Continue;
    FGfxMem.SetPixelRGBA(CX, CY, FillCol);
    Push(CX + 1, CY); Push(CX - 1, CY); Push(CX, CY + 1); Push(CX, CY - 1);
  end;
end;

procedure TTerminalController.SetWindow(Col1, Row1, Col2, Row2: Integer; DoClear: Boolean);
// The print area, set by VIEW PRINT and by the Commodore v7 WINDOW command. Nothing is redrawn -- the
// text already went to stdout -- but the model wraps and scrolls inside it from here on.
  function Clamp(V, Lo, Hi: Integer): Integer;
  begin
    if V < Lo then Result := Lo else if V > Hi then Result := Hi else Result := V;
  end;
begin
  if Length(FCells) = 0 then Exit;
  FViewLeft   := Clamp(Col1, 0, FCols - 1);
  FViewRight  := Clamp(Col2, FViewLeft, FCols - 1);
  FViewTop    := Clamp(Row1, 0, FRows - 1);
  FViewBottom := Clamp(Row2, FViewTop, FRows - 1);
  FCellX := FViewLeft;
  FCellY := FViewTop;
  if DoClear then ClearView;
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

function TTerminalController.GetCharAt(Col, Row: Integer): Byte;
begin
  if (Length(FCells) > 0) and (Row >= 0) and (Row < FRows) and (Col >= 0) and (Col < FCols) then
    Result := Ord(FCells[Row][Col].Ch)
  else
    Result := 32;  // space, as the windowed console reports for a cell off the grid
end;

procedure TTerminalController.SetCharAt(Col, Row: Integer; Ch: Byte);
begin
  // POKE into the C128 screen RAM: it lands in the model, so a later PEEK reads it back. Nothing is
  // redrawn -- stdout has already moved on -- but the program's own view of the screen stays coherent.
  if (Length(FCells) > 0) and (Row >= 0) and (Row < FRows) and (Col >= 0) and (Col < FCols) then
    FCells[Row][Col].Ch := Chr(Ch);
end;

function TTerminalController.GetColorAt(Col, Row: Integer): Byte;
begin
  if (Length(FCells) > 0) and (Row >= 0) and (Row < FRows) and (Col >= 0) and (Col < FCols) then
    Result := FCells[Row][Col].Fg
  else
    Result := FFgIdx;
end;

function TTerminalController.GetBackColorAt(Col, Row: Integer): Byte;
begin
  if (Length(FCells) > 0) and (Row >= 0) and (Row < FRows) and (Col >= 0) and (Col < FCols) then
    Result := FCells[Row][Col].Bg
  else
    Result := FBgIdx;
end;

procedure TTerminalController.SetColorAt(Col, Row: Integer; Color: Byte);
begin
  // POKE into the C128 colour RAM: recorded in the model (see SetCharAt).
  if (Length(FCells) > 0) and (Row >= 0) and (Row < FRows) and (Col >= 0) and (Col < FCols) then
    FCells[Row][Col].Fg := Color and $0F;
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
  // The console input buffer, its mode and the Ctrl+C handler belong to the console -- i.e. to the
  // parent shell -- not to this process. Only take a handle to it when stdin really is a console:
  // under a redirect or a pipe (`sb prog.bas </dev/null`, the regression harness) there is nothing to
  // configure and nothing that may be disturbed.
  FOwnsConsoleInput := StdInIsConsole;
  if FOwnsConsoleInput then
    FConsoleHandle := GetStdHandle(STD_INPUT_HANDLE)
  else
    FConsoleHandle := INVALID_HANDLE_VALUE;

  // Remember the console's mode so ReadLine can put it back exactly as it found it.
  if FOwnsConsoleInput then
    GetConsoleMode(FConsoleHandle, @FOldConsoleMode);

  // Install CTRL+C handler - this intercepts CTRL+C before it terminates the process
  // The handler sets GCtrlCPressed which we check in ProcessEvents
  GCtrlCPressed := False;
  SetConsoleCtrlHandler(@ConsoleCtrlHandler, True);

  // NOTE: we deliberately do NOT FlushConsoleInputBuffer here. That buffer is shared with whatever owns
  // the terminal, so flushing it silently DISCARDS keystrokes the user (or the tool driving the
  // terminal) has already typed -- and it did so on every single run, interactive or not. The Enter that
  // launched this process was consumed by the shell that read the command line; it is not sitting in the
  // buffer waiting for us. If a stray event ever does show up, it must be dropped where it is read, not
  // by destroying everyone else's pending input at startup.
  {$ENDIF}
end;

destructor TTerminalInput.Destroy;
begin
  {$IFDEF WINDOWS}
  // Belt and braces: ReadLine already restores the mode as soon as each read finishes, so by here there
  // is normally nothing to undo. Only a console we actually took over may be written back to.
  if FOwnsConsoleInput then
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
  // Put the console into line-input-with-echo for the duration of THIS read, and restore the mode we
  // found as soon as we are done. The mode belongs to the parent shell: leaving it altered until our
  // destructor runs means a process that is killed rather than exiting -- which is exactly what the
  // regression harness does to a hung run, via `timeout -s KILL` / TerminateProcess -- hands the
  // terminal back with echo and line editing in whatever state we left them.
  if FOwnsConsoleInput then
  begin
    NormalMode := ENABLE_LINE_INPUT or ENABLE_ECHO_INPUT or ENABLE_PROCESSED_INPUT;
    SetConsoleMode(FConsoleHandle, NormalMode);
  end;
  try
  {$ENDIF}

    repeat
      IsValid := True;
      Input := '';

      // Display prompt
      if Prompt <> '' then
        System.Write(Prompt);

      // Simple blocking ReadLn
      System.ReadLn(Input);

      // Standard input exhausted (EOF — piped/redirected input, or a headless run): stop asking. Signal
      // quit so a numeric INPUT loop does not spin "?REDO FROM START" forever (and eventually crash); the
      // program ends cleanly instead. At an interactive terminal EOF is only reached on Ctrl-Z / Ctrl-D.
      if System.Eof(System.Input) and (Input = '') then
      begin
        FShouldQuit := True;
        Result := '';
        Exit;
      end;

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

  {$IFDEF WINDOWS}
  finally
    // Covers the normal path, the EOF Exit above, and any exception raised while reading.
    if FOwnsConsoleInput then
      SetConsoleMode(FConsoleHandle, FOldConsoleMode);
  end;
  {$ENDIF}
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
          VK_DELETE:
          begin
            FPendingChar := #127;
            FHasPendingChar := True;
            Exit;
          end;
          // Cursor keys - use same codes as TProgramInputHandler
          VK_UP:
          begin
            FPendingChar := #1;   // SOH - Cursor UP
            FHasPendingChar := True;
            Exit;
          end;
          VK_DOWN:
          begin
            FPendingChar := #2;   // STX - Cursor DOWN
            FHasPendingChar := True;
            Exit;
          end;
          VK_LEFT:
          begin
            FPendingChar := #3;   // ETX - Cursor LEFT
            FHasPendingChar := True;
            Exit;
          end;
          VK_RIGHT:
          begin
            FPendingChar := #4;   // EOT - Cursor RIGHT
            FHasPendingChar := True;
            Exit;
          end;
          // Navigation keys
          VK_HOME:
          begin
            FPendingChar := #5;   // ENQ - HOME
            FHasPendingChar := True;
            Exit;
          end;
          VK_END:
          begin
            FPendingChar := #6;   // ACK - END
            FHasPendingChar := True;
            Exit;
          end;
          VK_PRIOR:  // Page Up
          begin
            FPendingChar := #11;  // VT - PAGE UP
            FHasPendingChar := True;
            Exit;
          end;
          VK_NEXT:   // Page Down
          begin
            FPendingChar := #12;  // FF - PAGE DOWN
            FHasPendingChar := True;
            Exit;
          end;
          VK_INSERT:
          begin
            FPendingChar := #14;  // SO - INSERT
            FHasPendingChar := True;
            Exit;
          end;
          // Function keys F1-F12 (codes 128-139)
          VK_F1:
          begin
            FPendingChar := #128;
            FHasPendingChar := True;
            Exit;
          end;
          VK_F2:
          begin
            FPendingChar := #129;
            FHasPendingChar := True;
            Exit;
          end;
          VK_F3:
          begin
            FPendingChar := #130;
            FHasPendingChar := True;
            Exit;
          end;
          VK_F4:
          begin
            FPendingChar := #131;
            FHasPendingChar := True;
            Exit;
          end;
          VK_F5:
          begin
            FPendingChar := #132;
            FHasPendingChar := True;
            Exit;
          end;
          VK_F6:
          begin
            FPendingChar := #133;
            FHasPendingChar := True;
            Exit;
          end;
          VK_F7:
          begin
            FPendingChar := #134;
            FHasPendingChar := True;
            Exit;
          end;
          VK_F8:
          begin
            FPendingChar := #135;
            FHasPendingChar := True;
            Exit;
          end;
          VK_F9:
          begin
            FPendingChar := #136;
            FHasPendingChar := True;
            Exit;
          end;
          VK_F10:
          begin
            FPendingChar := #137;
            FHasPendingChar := True;
            Exit;
          end;
          VK_F11:
          begin
            FPendingChar := #138;
            FHasPendingChar := True;
            Exit;
          end;
          VK_F12:
          begin
            FPendingChar := #139;
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
