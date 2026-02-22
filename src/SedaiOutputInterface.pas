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
  Classes, SysUtils, SedaiGraphicsTypes, SedaiSpriteTypes;

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
    procedure ResetPrintState;  // Reset reverse mode after PRINT (C128 behavior)

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
    procedure ClearScreen(Mode: Integer);  // SCNCLR: -1=current, 0-5=specific mode

    // Pixel operations
    procedure SetPixel(X, Y: Integer; RGB: UInt32); overload;
    procedure SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex); overload;
    function GetPixel(X, Y: Integer): UInt32;
    function GetPixelIndex(X, Y: Integer): TPaletteIndex;  // Returns palette index at coordinates

    // Palette management
    procedure EnablePalette(Enable: Boolean);
    function IsPaletteEnabled: Boolean;
    procedure SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
    procedure SetPaletteColorRGBA(Index: TPaletteIndex; R, G, B: Byte; A: Byte = 255);
    function GetPaletteColor(Index: TPaletteIndex): UInt32;
    procedure ResetPalette;

    // Palette file operations (JSON format)
    function LoadPaletteFromJSON(const FileName: string): Boolean;
    function SavePaletteToJSON(const FileName: string): Boolean;
    function GetLastPaletteError: string;

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

    // Color sources (C128-compatible, source 0-6, color 0-255)
    procedure SetColorSource(Source, Color: Integer);  // COLOR command (0-255, palette wraps)
    procedure SetColorSourceDirect(Source, Color: Integer); // SETCOLOR (0-255)
    function GetColorSourceDirect(Source: Integer): Integer; // RCLR/GETCOLOR return 0-based

    // Line width (1 or 2)
    procedure SetLineWidth(Width: Integer);

    // Coordinate scaling
    procedure SetScale(Enabled: Boolean; XMax, YMax: Integer);

    // Flood fill
    procedure FloodFill(Source: Integer; X, Y: Double; Mode: Integer);

    // Text window
    procedure SetWindow(Col1, Row1, Col2, Row2: Integer; DoClear: Boolean);
    function GetWindowLines: Integer;
    function GetWindowCols: Integer;
    function GetScreenWidth: Integer;

    // Screen memory access (for PEEK/POKE compatibility)
    function GetCharAt(Col, Row: Integer): Byte;      // Read character at position (PETSCII)
    procedure SetCharAt(Col, Row: Integer; Ch: Byte); // Write character at position (PETSCII)
    function GetColorAt(Col, Row: Integer): Byte;     // Read color at position
    procedure SetColorAt(Col, Row: Integer; Color: Byte); // Write color at position

    // Shape save/load
    function SaveShape(X1, Y1, X2, Y2: Double): string;
    procedure LoadShape(const Data: string; X, Y: Double; Mode: Integer);

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

    // Non-blocking character input (for GET command)
    function HasChar: Boolean;          // Returns true if a character is available
    function GetLastChar: string;       // Returns the last character pressed (or empty string)

    // Text input mode (required for SDL2 to generate TEXTINPUT events)
    procedure EnableTextInput;          // Call before waiting for text input (GETKEY)
    procedure DisableTextInput;         // Call after text input is done

    // Stato input
    function ShouldQuit: Boolean;       // CTRL+ALT+END: exit application
    function ShouldStop: Boolean;       // CTRL+C: stop BASIC program
    procedure ClearStopRequest;         // Clear stop flag after handling
    procedure ProcessEvents;

    // Reset
    procedure Reset;
  end;

  { ISpriteManager - Sprite management interface
    Based on Commodore 128 BASIC 7.0 sprite system with modern extensions.
    Supports up to MAX_SPRITES sprites with variable dimensions,
    automatic movement, and collision detection. }
  ISpriteManager = interface
    ['{SPRITE-MGR-0001-0001-000000000001}']

    // === Basic Sprite Commands ===

    // SPRITE n [,on/off] [,color] [,priority] [,x-exp] [,y-exp] [,mode]
    // Sets sprite properties. Pass -1 for parameters to keep current value.
    procedure SetSprite(Num: Integer; Enabled: Integer; const Color: TSpriteColor;
                        Priority: Integer; ScaleX, ScaleY: Double;
                        MulticolorMode: Integer);

    // MOVSPR n, x, y - Position sprite at absolute coordinates
    procedure MoveSpriteAbs(Num: Integer; X, Y: Double);

    // MOVSPR n, +/-x, +/-y - Move sprite relative to current position
    procedure MoveSpriteRel(Num: Integer; DX, DY: Double);

    // MOVSPR n, distance;angle - Move sprite by distance at angle (one-time)
    procedure MoveSpritePolar(Num: Integer; Distance: Double; Angle: Double);

    // MOVSPR n, angle#speed - Start automatic continuous movement
    // Speed 0 stops movement
    procedure MoveSpriteAuto(Num: Integer; Angle: Double; Speed: Double);

    // SPRCOLOR [mc1] [,mc2] - Set global multicolors for all sprites
    // Pass color with Index=255 to keep current value
    procedure SetSpriteMulticolors(const MC1, MC2: TSpriteColor);

    // SPRSAV sprite, string$ - Save sprite data to string
    procedure SaveSpriteToString(Num: Integer; out Data: string);

    // SPRSAV string$, sprite - Load sprite data from string
    procedure LoadSpriteFromString(const Data: string; Num: Integer);

    // SPRSAV sprite1, sprite2 - Copy sprite data between sprites
    procedure CopySpriteToSprite(SrcNum, DstNum: Integer);

    // COLLISION type, line - Set collision handler
    // type: 1=sprite-sprite, 2=sprite-display, 3=lightpen
    // line: BASIC line number for GOSUB (-1 to disable)
    procedure SetCollisionHandler(CollisionType: Integer; LineNumber: Integer);

    // === Sprite Query Functions ===

    // BUMP(n) - Return collision bitmask
    // n=1: sprite-to-sprite collisions
    // n=2: sprite-to-display collisions
    // Returns bitmask where bit N-1 corresponds to sprite N
    // Reading clears the collision flags
    function GetCollisionStatus(CollisionType: Integer): Cardinal;

    // RSPCOLOR(n) - Return global multicolor value
    // n=1: multicolor 1, n=2: multicolor 2
    function GetMulticolor(N: Integer): TSpriteColor;

    // RSPPOS(sprite, n) - Return sprite position/speed
    // n=0: X position, n=1: Y position, n=2: speed
    function GetSpritePosition(Num, Attr: Integer): Double;

    // RSPRITE(sprite, n) - Return sprite attribute
    // n=0: enabled, n=1: color, n=2: priority,
    // n=3: x-expand, n=4: y-expand, n=5: multicolor mode
    function GetSpriteAttribute(Num, Attr: Integer): Integer;

    // === Sprite Data Management (Modern Extensions) ===

    // Set sprite dimensions (default is 24x21 for C128 compatibility)
    procedure SetSpriteSize(Num: Integer; Width, Height: Integer);

    // Get sprite dimensions
    function GetSpriteWidth(Num: Integer): Integer;
    function GetSpriteHeight(Num: Integer): Integer;

    // Set sprite image data directly
    procedure SetSpriteData(Num: Integer; const Data: TBytes);

    // Get sprite image data
    function GetSpriteData(Num: Integer): TBytes;

    // === Update and Rendering ===

    // Update all sprites (called each frame)
    // - Updates positions for sprites with automatic movement
    // - Checks for collisions
    // - Queues collision events
    // DeltaTime: time since last frame in seconds
    procedure UpdateSprites(DeltaTime: Double);

    // Check if there are pending collision events
    function HasPendingCollision: Boolean;

    // Get next collision event (returns False if none pending)
    function GetNextCollision(out Event: TCollisionEvent): Boolean;

    // Render all enabled sprites to the display
    // Should be called after UpdateSprites and before Present
    procedure RenderSprites;

    // === Configuration ===

    // Set whether colors are indexed (palette) or RGBA
    // This is typically set based on graphics mode
    procedure SetIndexedColorMode(Indexed: Boolean);
    function GetIndexedColorMode: Boolean;

    // Enable/disable pixel-perfect collision detection
    // When disabled, uses faster bounding-box detection
    procedure SetPixelPerfectCollision(Enabled: Boolean);
    function GetPixelPerfectCollision: Boolean;

    // === Initialization ===

    // Reset all sprites to default state
    procedure ResetAllSprites;

    // Get sprite info (for debugging/inspection)
    function GetSpriteInfo(Num: Integer): TSpriteInfo;
  end;

implementation

end.
