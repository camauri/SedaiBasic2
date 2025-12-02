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
  Classes, SysUtils;

type
  TOutputType = (otSDL2, otFile, otStringList);

  TColor = record
    R, G, B: Byte;
  end;

  TGraphicMode = (
    gm40ColText = 0,        // 40x25 text mode
    gmStandardBitmap = 1,   // 320x200 bitmap
    gmSplitBitmap = 2,      // 320x160 bitmap + text
    gmMulticolorBitmap = 3, // 160x200 multicolor (pixel doppi)
    gmSplitMulticolor = 4,  // 160x160 multicolor + text
    gm80ColText = 5,        // 80x25 text mode (640x200)
    gm640x400_16col = 6,    // 80x50 text mode (640x400)
    gm1440x896_true = 7,    // 180x112 text mode (1440x896)
    gm320x240_true = 8,     // 320x240 truecolor (QVGA)
    gm640x480_true = 9,     // 640x480 truecolor (VGA)
    gm800x480_true = 10,    // 800x480 truecolor (WVGA)
    gm800x600_true = 11,    // 800x600 truecolor (SVGA)
    gm1024x600_true = 12,   // 1024x600 truecolor (WSVGA)
    gm1024x768_true = 13,   // 1024x768 truecolor (XGA)
    gm1280x720_true = 14,   // 1280x720 truecolor (HD 720)
    gm1280x800_true = 15,   // 1280x800 truecolor (WXGA)
    gm1280x1024_true = 16,  // 1280x1024 truecolor (SXGA)
    gm1366x768_true = 17,   // 1366x768 truecolor (FWXGA)
    gm1600x900_true = 18,   // 1600x900 truecolor (WSXGA)
    gm1680x1050_true = 19,  // 1680x1050 truecolor (WSXGA+)
    gm1920x1080_true = 20,  // 1920x1080 truecolor (HD1080)
    gm2048x1080_true = 21,  // 2048x1080 truecolor (2K)
    gm2560x1440_true = 22,  // 2560x1440 truecolor (WQHD)
    gm2560x1600_true = 23,  // 2560x1600 truecolor (WQXGA)
    gm3840x2160_true = 24,  // 3840x2160 truecolor (4K UHD)
    gmCustom = 25           // Custom resolution
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

    // Properties
    property InScrollMode: Boolean read GetInScrollMode;
  end;

  { IInputDevice - SOLO INPUT }
  IInputDevice = interface
    ['{87654321-4321-4321-4321-123456789DEF}']

    // Input puro
    function ReadLine(const Prompt: string = ''; IsCommand: Boolean = True; NumericOnly: Boolean = False; AllowDecimal: Boolean = True): string;
    function ReadKey: Char;
    function KeyPressed: Boolean;

    // Stato input
    function ShouldQuit: Boolean;
    procedure ProcessEvents;

    // Reset
    procedure Reset;
  end;

implementation

end.
