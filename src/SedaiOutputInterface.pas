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
unit SedaiOutputInterface;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, SedaiGraphicsTypes;

type
  TOutputType = (otSDL2, otFile, otStringList);

  TColor = record
    R, G, B: Byte;
  end;

  TGraphicMode = (
    // C64/C128 compatible modes - 320x200 group (shared buffer)
    gm40ColText = 0,        // GRAPHIC 0: 40x25 text mode (320x200)
    gmStandardBitmap = 1,   // GRAPHIC 1: 320x200 hires bitmap
    gmSplitBitmap = 2,      // GRAPHIC 2: 320x160 bitmap + 40x5 text

    // Multicolor modes - 160x200 group (shared buffer)
    gmMulticolorBitmap = 3, // GRAPHIC 3: 160x200 multicolor (pixel doppi)
    gmSplitMulticolor = 4,  // GRAPHIC 4: 160x160 multicolor + 40x5 text

    // 80-column modes - 640x200 group (shared buffer)
    gm80ColText = 5,        // GRAPHIC 5: 80x25 text mode (640x200)
    gm80ColBitmap = 6,      // GRAPHIC 6: 640x200 hires bitmap
    gm80ColMixed = 7,       // GRAPHIC 7: 640x160 bitmap + 80x5 text

    // Extended modes - 640x400 group (shared buffer)
    gm80x50Text = 8,        // GRAPHIC 8: 80x50 text mode (640x400)
    gm80x50Bitmap = 9,      // GRAPHIC 9: 640x400 hires bitmap
    gm80x50Mixed = 10,      // GRAPHIC 10: 640x360 bitmap + 80x5 text

    // Dynamic SDL2 mode (resolution from SDL2VideoModeEnumerator via GLIST)
    // Syntax: GRAPHIC 11, clear, sdl2_mode_index
    gmSDL2Dynamic = 11      // GRAPHIC 11: SDL2 dynamic mode, param3 = SDL2 mode index from GLIST
  );

  TPaletteIndex = 0..255;


  { IOutputDevice - SOLO OUTPUT }
  IOutputDevice = interface
    ['{12345678-1234-1234-1234-123456789ABC}']

    // Inizializzazione/chiusura
    function Initialize(const Title: string = ''; Width: Integer = 80; Height: Integer = 25): Boolean;
    procedure Shutdown;
    function IsInitialized: Boolean;

    // Output base
    procedure Print(const Text: string; ClearBackground: Boolean = False);
    procedure PrintLn(const Text: string; ClearBackground: Boolean = False);
    procedure NewLine;
    procedure Clear;

    // Cursor/position control
    procedure SetCursor(X, Y: Integer);
    procedure MoveCursor(DeltaX, DeltaY: Integer);
    function GetCursorX: Integer;
    function GetCursorY: Integer;

    // Cursore lampeggiante C64
    procedure ShowCursor(X, Y: Integer);
    procedure HideCursor(X, Y: Integer);

    // Colori
    procedure SetColors(Foreground, Background: TColor);

    // Aggiornamento display
    procedure Present;

    // Controlli specifici
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

    // Graphics support
    function SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean = False;
                           SplitLine: Integer = -1): Boolean;
    function GetGraphicMode: TGraphicMode;
    function IsInGraphicsMode: Boolean;

    // Pixel operations
    procedure SetPixel(X, Y: Integer; RGB: UInt32); overload;
    procedure SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex); overload;
    function GetPixel(X, Y: Integer): UInt32;

    // Palette management
    procedure EnablePalette(Enable: Boolean);
    function IsPaletteEnabled: Boolean;
    procedure SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
    function GetPaletteColor(Index: TPaletteIndex): UInt32;
    procedure ResetPalette;

    // Shape drawing (extensible graphics)
    // Simple version: uses current style state
    procedure DrawBox(X1, Y1, X2, Y2: Integer; Angle: Double = 0);
    // Full version: explicit style
    procedure DrawBoxStyled(X1, Y1, X2, Y2: Integer; const Style: TShapeStyle; Angle: Double = 0);
    // BASIC BOX command: color interpretation depends on graphics mode
    // In hires modes (1-2): 0=background, 1=foreground (captured at mode switch)
    // In SDL2 mode (7): color is RGBA value directly
    // Filled: if True, fill the rectangle interior; if False, draw outline only
    procedure DrawBoxWithColor(X1, Y1, X2, Y2: Integer; Color: UInt32; Angle: Double = 0; Filled: Boolean = False);
    // BASIC CIRCLE command: draws ellipses, arcs, and polygons
    // X, Y: center coordinates; XR, YR: x and y radii
    // SA, EA: start and end angles in degrees (0-360)
    // Angle: rotation in degrees clockwise; Inc: degrees between line segments
    procedure DrawCircleWithColor(X, Y, XR, YR: Integer; Color: UInt32;
                                  SA: Double = 0; EA: Double = 360;
                                  Angle: Double = 0; Inc: Double = 2);
    // BASIC DRAW command: draws lines from (X1,Y1) to (X2,Y2)
    procedure DrawLine(X1, Y1, X2, Y2: Integer; Color: UInt32);

    // Pixel Cursor (PC) - invisible cursor for DRAW/LOCATE/RDOT commands
    procedure SetPixelCursor(X, Y: Integer);
    function GetPixelCursorX: Integer;
    function GetPixelCursorY: Integer;

    // Style state management
    procedure SetBorderStyle(const Style: TBorderStyle);
    procedure SetFillStyle(const Style: TFillStyleDef);
    function GetBorderStyle: TBorderStyle;
    function GetFillStyle: TFillStyleDef;

    // FAST mode (C128 2MHz mode emulation - shows black overlay)
    procedure SetFastMode(Enabled: Boolean);
    function GetFastMode: Boolean;
    procedure SetFastModeAlpha(Alpha: Byte);
    function GetFastModeAlpha: Byte;

    // Properties
    property InScrollMode: Boolean read GetInScrollMode;
    property FastMode: Boolean read GetFastMode write SetFastMode;
  end;

  { IInputDevice - SOLO INPUT }
  IInputDevice = interface
    ['{87654321-4321-4321-4321-123456789DEF}']

    // Input puro
    function ReadLine(const Prompt: string = ''; IsCommand: Boolean = True; NumericOnly: Boolean = False; AllowDecimal: Boolean = True): string;
    function ReadKey: Char;
    function KeyPressed: Boolean;

    // Stato input
    function ShouldQuit: Boolean;       // CTRL+ALT+END: exit application
    function ShouldStop: Boolean;       // CTRL+C: stop BASIC program
    procedure ClearStopRequest;         // Clear stop flag after handling
    procedure ProcessEvents;

    // Reset
    procedure Reset;
  end;

implementation

end.
