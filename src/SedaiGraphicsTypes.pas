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
unit SedaiGraphicsTypes;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils;

type
  { Border line styles }
  TBorderLineStyle = (
    blsNone,           // No border
    blsSolid,          // Solid line (default)
    blsDot,            // Dotted line: . . . . .
    blsDash,           // Dashed line: - - - -
    blsDotDash,        // Dot-dash line: . - . - . -
    blsDotDotDash,     // Dot-dot-dash: . . - . . -
    blsDouble          // Double line (future)
  );

  { Fill pattern styles }
  TFillStyle = (
    fsNone,            // No fill (transparent)
    fsSolid,           // Solid fill (default)
    fsHorizontal,      // Horizontal lines ---
    fsVertical,        // Vertical lines |||
    fsDiagonalUp,      // Diagonal lines /////
    fsDiagonalDown,    // Diagonal lines \\\\\
    fsCrossHatch,      // Cross hatch +++
    fsDiagonalCross,   // Diagonal cross XXX
    fsDotted,          // Dotted pattern . . .
    fsSparseDot,       // Sparse dots  .   .
    fsChecker,         // Checkerboard pattern
    fsCustom           // Custom pattern (future)
  );

  { Line cap styles (for future extension) }
  TLineCapStyle = (
    lcsButt,           // Flat end at endpoint
    lcsRound,          // Rounded end
    lcsSquare          // Square end extending beyond endpoint
  );

  { Line join styles (for future extension) }
  TLineJoinStyle = (
    ljsMiter,          // Sharp corner
    ljsRound,          // Rounded corner
    ljsBevel           // Beveled corner
  );

  { Border style definition }
  TBorderStyle = record
    Color: UInt32;           // RGBA color (from RGBA() function)
    Width: Integer;          // Border width in pixels (1-10)
    LineStyle: TBorderLineStyle;
    DashLength: Integer;     // Length of dash segment (for custom patterns)
    GapLength: Integer;      // Length of gap between segments
    CapStyle: TLineCapStyle;
    JoinStyle: TLineJoinStyle;
  end;

  { Fill style definition }
  TFillStyleDef = record
    Color: UInt32;           // RGBA fill color
    Style: TFillStyle;
    PatternScale: Integer;   // Scale factor for patterns (1-10)
    PatternAngle: Double;    // Rotation angle for patterns (degrees)
    SecondaryColor: UInt32;  // For two-color patterns (e.g., checker)
  end;

  { Complete shape style - combines border and fill }
  TShapeStyle = record
    Border: TBorderStyle;
    Fill: TFillStyleDef;
  end;

  { Box parameters for drawing }
  TBoxParams = record
    X1, Y1: Integer;         // First corner
    X2, Y2: Integer;         // Second corner (opposite)
    Angle: Double;           // Rotation angle in degrees
    Style: TShapeStyle;      // Complete style
  end;

{ Helper functions to create default styles }
function MakeDefaultBorderStyle(Color: UInt32; Width: Integer = 1): TBorderStyle;
function MakeBorderStyle(Color: UInt32; Width: Integer; LineStyle: TBorderLineStyle): TBorderStyle;
function MakeDefaultFillStyle(Color: UInt32): TFillStyleDef;
function MakeFillStyle(Color: UInt32; Style: TFillStyle): TFillStyleDef;
function MakeShapeStyle(const Border: TBorderStyle; const Fill: TFillStyleDef): TShapeStyle;

{ Simple style creation for common cases }
function MakeSolidBoxStyle(BorderColor, FillColor: UInt32; BorderWidth: Integer = 1): TShapeStyle;
function MakeOutlineBoxStyle(BorderColor: UInt32; BorderWidth: Integer = 1): TShapeStyle;
function MakeDottedBoxStyle(BorderColor: UInt32; BorderWidth: Integer = 1): TShapeStyle;

{ Style name conversion }
function BorderLineStyleToString(Style: TBorderLineStyle): string;
function FillStyleToString(Style: TFillStyle): string;
function StringToBorderLineStyle(const S: string): TBorderLineStyle;
function StringToFillStyle(const S: string): TFillStyle;

{ Color helpers }
function RGBA(R, G, B, A: Byte): UInt32; inline;
function GetRed(Color: UInt32): Byte; inline;
function GetGreen(Color: UInt32): Byte; inline;
function GetBlue(Color: UInt32): Byte; inline;
function GetAlpha(Color: UInt32): Byte; inline;

const
  { Predefined colors (RGBA format) }
  clTransparent = $00000000;
  clBlack       = $FF000000;
  clWhite       = $FFFFFFFF;
  clRed         = $FF0000FF;
  clGreen       = $FF00FF00;
  clBlue        = $FFFF0000;
  clYellow      = $FF00FFFF;
  clCyan        = $FFFFFF00;
  clMagenta     = $FFFF00FF;
  clGray        = $FF808080;
  clLightGray   = $FFC0C0C0;
  clDarkGray    = $FF404040;

implementation

function RGBA(R, G, B, A: Byte): UInt32;
begin
  // SDL_PIXELFORMAT_RGBA8888 on little-endian: bytes in memory are R,G,B,A
  // So UInt32 value must be ABGR (A in high byte) for correct memory layout
  Result := (UInt32(A) shl 24) or (UInt32(B) shl 16) or (UInt32(G) shl 8) or UInt32(R);
end;

function GetRed(Color: UInt32): Byte;
begin
  Result := Byte(Color);
end;

function GetGreen(Color: UInt32): Byte;
begin
  Result := Byte(Color shr 8);
end;

function GetBlue(Color: UInt32): Byte;
begin
  Result := Byte(Color shr 16);
end;

function GetAlpha(Color: UInt32): Byte;
begin
  Result := Byte(Color shr 24);
end;

function MakeDefaultBorderStyle(Color: UInt32; Width: Integer): TBorderStyle;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Color := Color;
  Result.Width := Width;
  Result.LineStyle := blsSolid;
  Result.DashLength := 4;
  Result.GapLength := 4;
  Result.CapStyle := lcsButt;
  Result.JoinStyle := ljsMiter;
end;

function MakeBorderStyle(Color: UInt32; Width: Integer; LineStyle: TBorderLineStyle): TBorderStyle;
begin
  Result := MakeDefaultBorderStyle(Color, Width);
  Result.LineStyle := LineStyle;

  // Set appropriate dash/gap lengths based on style
  case LineStyle of
    blsDot:
    begin
      Result.DashLength := 1;
      Result.GapLength := 2;
    end;
    blsDash:
    begin
      Result.DashLength := 6;
      Result.GapLength := 3;
    end;
    blsDotDash:
    begin
      Result.DashLength := 4;
      Result.GapLength := 2;
    end;
    blsDotDotDash:
    begin
      Result.DashLength := 4;
      Result.GapLength := 2;
    end;
  end;
end;

function MakeDefaultFillStyle(Color: UInt32): TFillStyleDef;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Color := Color;
  Result.Style := fsSolid;
  Result.PatternScale := 1;
  Result.PatternAngle := 0;
  Result.SecondaryColor := clTransparent;
end;

function MakeFillStyle(Color: UInt32; Style: TFillStyle): TFillStyleDef;
begin
  Result := MakeDefaultFillStyle(Color);
  Result.Style := Style;

  // Set appropriate pattern scale based on style
  case Style of
    fsHorizontal, fsVertical:
      Result.PatternScale := 4;
    fsDiagonalUp, fsDiagonalDown:
      Result.PatternScale := 4;
    fsCrossHatch, fsDiagonalCross:
      Result.PatternScale := 4;
    fsDotted:
      Result.PatternScale := 2;
    fsSparseDot:
      Result.PatternScale := 6;
    fsChecker:
      Result.PatternScale := 8;
  end;
end;

function MakeShapeStyle(const Border: TBorderStyle; const Fill: TFillStyleDef): TShapeStyle;
begin
  Result.Border := Border;
  Result.Fill := Fill;
end;

function MakeSolidBoxStyle(BorderColor, FillColor: UInt32; BorderWidth: Integer): TShapeStyle;
begin
  Result.Border := MakeDefaultBorderStyle(BorderColor, BorderWidth);
  Result.Fill := MakeDefaultFillStyle(FillColor);
end;

function MakeOutlineBoxStyle(BorderColor: UInt32; BorderWidth: Integer): TShapeStyle;
begin
  Result.Border := MakeDefaultBorderStyle(BorderColor, BorderWidth);
  Result.Fill := MakeDefaultFillStyle(clTransparent);
  Result.Fill.Style := fsNone;
end;

function MakeDottedBoxStyle(BorderColor: UInt32; BorderWidth: Integer): TShapeStyle;
begin
  Result.Border := MakeBorderStyle(BorderColor, BorderWidth, blsDot);
  Result.Fill := MakeDefaultFillStyle(clTransparent);
  Result.Fill.Style := fsNone;
end;

function BorderLineStyleToString(Style: TBorderLineStyle): string;
begin
  case Style of
    blsNone:       Result := 'none';
    blsSolid:      Result := 'solid';
    blsDot:        Result := 'dot';
    blsDash:       Result := 'dash';
    blsDotDash:    Result := 'dot-dash';
    blsDotDotDash: Result := 'dot-dot-dash';
    blsDouble:     Result := 'double';
  else
    Result := 'solid';
  end;
end;

function FillStyleToString(Style: TFillStyle): string;
begin
  case Style of
    fsNone:          Result := 'none';
    fsSolid:         Result := 'solid';
    fsHorizontal:    Result := 'horizontal';
    fsVertical:      Result := 'vertical';
    fsDiagonalUp:    Result := 'diagonal-up';
    fsDiagonalDown:  Result := 'diagonal-down';
    fsCrossHatch:    Result := 'crosshatch';
    fsDiagonalCross: Result := 'diagonal-cross';
    fsDotted:        Result := 'dotted';
    fsSparseDot:     Result := 'sparse-dot';
    fsChecker:       Result := 'checker';
    fsCustom:        Result := 'custom';
  else
    Result := 'solid';
  end;
end;

function StringToBorderLineStyle(const S: string): TBorderLineStyle;
var
  LowerS: string;
begin
  LowerS := LowerCase(Trim(S));
  if LowerS = 'none' then Result := blsNone
  else if LowerS = 'solid' then Result := blsSolid
  else if LowerS = 'dot' then Result := blsDot
  else if LowerS = 'dash' then Result := blsDash
  else if LowerS = 'dot-dash' then Result := blsDotDash
  else if LowerS = 'dot-dot-dash' then Result := blsDotDotDash
  else if LowerS = 'double' then Result := blsDouble
  else Result := blsSolid;  // Default
end;

function StringToFillStyle(const S: string): TFillStyle;
var
  LowerS: string;
begin
  LowerS := LowerCase(Trim(S));
  if LowerS = 'none' then Result := fsNone
  else if LowerS = 'solid' then Result := fsSolid
  else if LowerS = 'horizontal' then Result := fsHorizontal
  else if LowerS = 'vertical' then Result := fsVertical
  else if LowerS = 'diagonal-up' then Result := fsDiagonalUp
  else if LowerS = 'diagonal-down' then Result := fsDiagonalDown
  else if LowerS = 'crosshatch' then Result := fsCrossHatch
  else if LowerS = 'diagonal-cross' then Result := fsDiagonalCross
  else if LowerS = 'dotted' then Result := fsDotted
  else if LowerS = 'sparse-dot' then Result := fsSparseDot
  else if LowerS = 'checker' then Result := fsChecker
  else if LowerS = 'custom' then Result := fsCustom
  else Result := fsSolid;  // Default
end;

end.
