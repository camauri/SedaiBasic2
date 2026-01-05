{
  SedaiSpriteTypes.pas
  ====================
  Sprite management type definitions for SedaiBasic2.

  Based on Commodore 128 BASIC 7.0 sprite system with modern extensions:
  - Up to 256 sprites (configurable via MAX_SPRITES)
  - Variable sprite dimensions (not limited to 24x21)
  - Indexed colors (0-255) for legacy modes, RGBA truecolor for modern modes
  - Interrupt-based automatic movement and collision detection

  Copyright (c) 2024-2025 Artiforge
}
unit SedaiSpriteTypes;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;  // For TBytes

const
  // Maximum number of sprites (configurable)
  MAX_SPRITES = 256;

  // Default sprite dimensions (C128 compatible)
  DEFAULT_SPRITE_WIDTH = 24;
  DEFAULT_SPRITE_HEIGHT = 21;
  DEFAULT_SPRITE_DATA_SIZE = 63;  // 24x21 / 8 * 3 = 63 bytes

  // Collision types (C128 compatible)
  COLLISION_SPRITE_SPRITE = 1;   // Sprite-to-sprite collision
  COLLISION_SPRITE_DISPLAY = 2;  // Sprite-to-display/background collision
  COLLISION_LIGHTPEN = 3;        // Light pen (for compatibility, may not be implemented)

  // MOVSPR movement types
  MOVSPR_ABSOLUTE = 0;   // MOVSPR n, x, y
  MOVSPR_RELATIVE = 1;   // MOVSPR n, +x, +y or -x, -y
  MOVSPR_POLAR = 2;      // MOVSPR n, distance;angle
  MOVSPR_AUTO = 3;       // MOVSPR n, angle#speed

  // RSPRITE attribute indices (C128 compatible)
  RSPRITE_ENABLED = 0;      // Enabled (1) / Disabled (0)
  RSPRITE_COLOR = 1;        // Sprite color (1-16 or RGBA)
  RSPRITE_PRIORITY = 2;     // In front (0) or behind (1) display
  RSPRITE_EXPAND_X = 3;     // Horizontal expansion
  RSPRITE_EXPAND_Y = 4;     // Vertical expansion
  RSPRITE_MULTICOLOR = 5;   // Multicolor mode

  // RSPPOS attribute indices (C128 compatible)
  RSPPOS_X = 0;             // Current X position
  RSPPOS_Y = 1;             // Current Y position
  RSPPOS_SPEED = 2;         // Current speed (0-15, extendable)

  // RSPCOLOR indices (C128 compatible)
  RSPCOLOR_MC1 = 1;         // Multicolor 1
  RSPCOLOR_MC2 = 2;         // Multicolor 2

  // Speed limits (C128 uses 0-15, we extend to floating point)
  MIN_SPRITE_SPEED = 0;
  MAX_SPRITE_SPEED_C128 = 15;
  MAX_SPRITE_SPEED = 1000;  // Extended maximum

type
  // Color representation for sprites
  // Can be either indexed (palette-based) or RGBA (truecolor)
  TSpriteColorMode = (scmIndexed, scmRGBA);

  TSpriteColor = record
    Mode: TSpriteColorMode;
    case TSpriteColorMode of
      scmIndexed: (Index: Byte);           // 0-255 palette index
      scmRGBA: (R, G, B, A: Byte);          // RGBA components
  end;

  // Sprite motion state for automatic movement
  TSpriteMotion = record
    Active: Boolean;          // Automatic movement enabled
    Angle: Double;            // Movement angle in degrees (0=up, clockwise)
    Speed: Double;            // Speed in pixels per frame
  end;

  // Complete sprite information record
  TSpriteInfo = record
    // Basic state
    Enabled: Boolean;             // Sprite is visible/active
    X, Y: Double;                 // Position (floating point for subpixel precision)

    // Appearance
    Color: TSpriteColor;          // Primary sprite color
    Priority: Integer;            // 0=in front of display, 1=behind display
    ScaleX, ScaleY: Double;       // Scale factors (1.0=normal, 2.0=double)
    MulticolorMode: Boolean;      // Multicolor mode enabled

    // Dimensions and image data
    Width, Height: Integer;       // Sprite dimensions in pixels
    Data: TBytes;                 // Sprite image data

    // Automatic motion
    Motion: TSpriteMotion;

    // Collision state (updated each frame)
    CollisionMask: Cardinal;      // Bitmask of sprites this sprite has collided with
    DisplayCollision: Boolean;    // True if collided with display/background
  end;

  // Pointer to sprite info
  PSpriteInfo = ^TSpriteInfo;

  // Array of all sprites
  TSpriteArray = array[1..MAX_SPRITES] of TSpriteInfo;

  // Global multicolor settings (shared by all sprites in multicolor mode)
  TSpriteMulticolors = record
    Multicolor1: TSpriteColor;
    Multicolor2: TSpriteColor;
  end;

  // Collision handler definition
  TCollisionHandler = record
    Enabled: Boolean;
    LineNumber: Integer;          // BASIC line number for GOSUB
  end;

  // Array of collision handlers (indexed by collision type)
  TCollisionHandlers = array[1..3] of TCollisionHandler;

  // Collision event for interrupt queue
  TCollisionEvent = record
    CollisionType: Integer;       // COLLISION_SPRITE_SPRITE, etc.
    CollisionMask: Cardinal;      // Which sprites were involved
    HandlerLine: Integer;         // Line number to GOSUB
  end;

// Helper functions for TSpriteColor
function MakeIndexedColor(Index: Byte): TSpriteColor;
function MakeRGBAColor(R, G, B, A: Byte): TSpriteColor;
function MakeRGBAColorFromInt(RGBA: Cardinal): TSpriteColor;
function SpriteColorToInt(const Color: TSpriteColor): Cardinal;
function SpriteColorsEqual(const A, B: TSpriteColor): Boolean;

// Helper function to initialize sprite to default state
procedure InitSpriteInfo(var Sprite: TSpriteInfo);

// Helper function to initialize all sprites
procedure InitAllSprites(var Sprites: TSpriteArray);

implementation

function MakeIndexedColor(Index: Byte): TSpriteColor;
begin
  Result.Mode := scmIndexed;
  Result.Index := Index;
end;

function MakeRGBAColor(R, G, B, A: Byte): TSpriteColor;
begin
  Result.Mode := scmRGBA;
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
end;

function MakeRGBAColorFromInt(RGBA: Cardinal): TSpriteColor;
begin
  Result.Mode := scmRGBA;
  Result.R := (RGBA shr 24) and $FF;
  Result.G := (RGBA shr 16) and $FF;
  Result.B := (RGBA shr 8) and $FF;
  Result.A := RGBA and $FF;
end;

function SpriteColorToInt(const Color: TSpriteColor): Cardinal;
begin
  if Color.Mode = scmIndexed then
    Result := Color.Index
  else
    Result := (Cardinal(Color.R) shl 24) or
              (Cardinal(Color.G) shl 16) or
              (Cardinal(Color.B) shl 8) or
              Cardinal(Color.A);
end;

function SpriteColorsEqual(const A, B: TSpriteColor): Boolean;
begin
  if A.Mode <> B.Mode then
    Result := False
  else if A.Mode = scmIndexed then
    Result := A.Index = B.Index
  else
    Result := (A.R = B.R) and (A.G = B.G) and (A.B = B.B) and (A.A = B.A);
end;

procedure InitSpriteInfo(var Sprite: TSpriteInfo);
begin
  Sprite.Enabled := False;
  Sprite.X := 0;
  Sprite.Y := 0;
  Sprite.Color := MakeIndexedColor(1);  // Default: black (C128 color 1)
  Sprite.Priority := 0;                  // In front of display
  Sprite.ScaleX := 1.0;
  Sprite.ScaleY := 1.0;
  Sprite.MulticolorMode := False;
  Sprite.Width := DEFAULT_SPRITE_WIDTH;
  Sprite.Height := DEFAULT_SPRITE_HEIGHT;
  SetLength(Sprite.Data, 0);
  Sprite.Motion.Active := False;
  Sprite.Motion.Angle := 0;
  Sprite.Motion.Speed := 0;
  Sprite.CollisionMask := 0;
  Sprite.DisplayCollision := False;
end;

procedure InitAllSprites(var Sprites: TSpriteArray);
var
  I: Integer;
begin
  for I := 1 to MAX_SPRITES do
    InitSpriteInfo(Sprites[I]);
end;

end.
