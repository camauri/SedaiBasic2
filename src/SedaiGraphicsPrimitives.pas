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
unit SedaiGraphicsPrimitives;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Math,
  SedaiGraphicsMemory, SedaiOutputInterface;

type
  { Callback type for SetPixel - allows both index and RGBA modes }
  TSetPixelCallback = procedure(X, Y: Integer; Color: UInt32) of object;
  TSetPixelIndexCallback = procedure(X, Y: Integer; Index: TPaletteIndex) of object;

{ Draw a box (rectangle) to graphics memory
  Parameters:
    Memory: TGraphicsMemory instance to draw into
    X1, Y1, X2, Y2: Box corners (will be normalized)
    Color: Color value (RGBA or palette index depending on UseIndex)
    UseIndex: If true, Color is treated as TPaletteIndex
    Filled: If true, fill the interior
    LineWidth: Border width (0=no border, 1-255)
    Angle: Rotation angle in degrees around center (0=no rotation)
    ViewWidth, ViewHeight: Viewport dimensions for clipping
}
procedure DrawBoxToMemory(Memory: TGraphicsMemory;
  X1, Y1, X2, Y2: Integer;
  Color: UInt32;
  UseIndex: Boolean;
  Filled: Boolean;
  LineWidth: Integer;
  Angle: Double;
  ViewWidth, ViewHeight: Integer);

{ Draw a line using Bresenham's algorithm
  Parameters:
    Memory: TGraphicsMemory instance to draw into
    X1, Y1, X2, Y2: Line endpoints
    Color: Color value (RGBA or palette index depending on UseIndex)
    UseIndex: If true, Color is treated as TPaletteIndex
    LineWidth: Line width (1-255)
    ViewWidth, ViewHeight: Viewport dimensions for clipping
}
procedure DrawLineToMemory(Memory: TGraphicsMemory;
  X1, Y1, X2, Y2: Integer;
  Color: UInt32;
  UseIndex: Boolean;
  LineWidth: Integer;
  ViewWidth, ViewHeight: Integer);

{ Draw a circle/ellipse arc
  Parameters:
    Memory: TGraphicsMemory instance to draw into
    CX, CY: Center coordinates
    RadiusX, RadiusY: Radii (use same value for circle)
    Color: Color value (RGBA or palette index depending on UseIndex)
    UseIndex: If true, Color is treated as TPaletteIndex
    StartAngle, EndAngle: Arc angles in degrees (0-360)
    RotationAngle: Rotation of the entire ellipse
    AngleIncrement: Step size for drawing (smaller = smoother)
    LineWidth: Line width (1-255)
    ViewWidth, ViewHeight: Viewport dimensions for clipping
}
procedure DrawCircleToMemory(Memory: TGraphicsMemory;
  CX, CY, RadiusX, RadiusY: Integer;
  Color: UInt32;
  UseIndex: Boolean;
  StartAngle, EndAngle, RotationAngle, AngleIncrement: Double;
  LineWidth: Integer;
  ViewWidth, ViewHeight: Integer);

{ Helper: Set a single pixel with bounds checking }
procedure SetPixelSafe(Memory: TGraphicsMemory;
  X, Y: Integer;
  Color: UInt32;
  UseIndex: Boolean;
  ViewWidth, ViewHeight: Integer); inline;

{ Helper: Draw a thick pixel (for LineWidth > 1) }
procedure DrawThickPixel(Memory: TGraphicsMemory;
  X, Y: Integer;
  Color: UInt32;
  UseIndex: Boolean;
  Thickness: Integer;
  ViewWidth, ViewHeight: Integer);

{ Helper: Rotate a point around a center }
procedure RotatePoint(var X, Y: Double; CX, CY, AngleRad: Double);

implementation

procedure SetPixelSafe(Memory: TGraphicsMemory;
  X, Y: Integer;
  Color: UInt32;
  UseIndex: Boolean;
  ViewWidth, ViewHeight: Integer);
begin
  // Bounds check - ignore pixels outside viewport (correct clipping)
  if (X < 0) or (X >= ViewWidth) or (Y < 0) or (Y >= ViewHeight) then
    Exit;

  if UseIndex then
    Memory.SetPixel(X, Y, TPaletteIndex(Color and $FF))
  else
    Memory.SetPixel(X, Y, Color);
end;

procedure DrawThickPixel(Memory: TGraphicsMemory;
  X, Y: Integer;
  Color: UInt32;
  UseIndex: Boolean;
  Thickness: Integer;
  ViewWidth, ViewHeight: Integer);
var
  HalfInner, HalfOuter: Integer;
  DX, DY: Integer;
begin
  if Thickness <= 1 then
  begin
    SetPixelSafe(Memory, X, Y, Color, UseIndex, ViewWidth, ViewHeight);
    Exit;
  end;

  // Calculate inner and outer expansion
  // Thickness is distributed: half inside, half outside
  // When odd: extra pixel goes outside
  // WIDTH=2: 1 inner (0 to -1) + 1 outer (0 to 0) = offsets -1,0 = 2px
  // WIDTH=3: 1 inner (0 to -1) + 2 outer (0 to 1) = offsets -1,0,1 = 3px
  // WIDTH=4: 2 inner (0 to -2) + 2 outer (0 to 1) = offsets -2,-1,0,1 = 4px
  HalfInner := Thickness div 2;           // pixels to draw "inside" (negative direction)
  HalfOuter := (Thickness + 1) div 2;     // pixels to draw "outside" (positive direction)

  // Draw a square of pixels centered on (X,Y)
  // Range: from -(HalfInner) to +(HalfOuter - 1) covers exactly Thickness pixels
  // Example WIDTH=3: from -1 to +1 = 3 pixels
  // Example WIDTH=4: from -2 to +1 = 4 pixels
  for DY := -(HalfInner) to HalfOuter - 1 do
    for DX := -(HalfInner) to HalfOuter - 1 do
      SetPixelSafe(Memory, X + DX, Y + DY, Color, UseIndex, ViewWidth, ViewHeight);
end;

procedure RotatePoint(var X, Y: Double; CX, CY, AngleRad: Double);
var
  CosA, SinA: Double;
  TX, TY: Double;
begin
  CosA := Cos(AngleRad);
  SinA := Sin(AngleRad);
  TX := X - CX;
  TY := Y - CY;
  X := CX + TX * CosA - TY * SinA;
  Y := CY + TX * SinA + TY * CosA;
end;

procedure DrawLineToMemory(Memory: TGraphicsMemory;
  X1, Y1, X2, Y2: Integer;
  Color: UInt32;
  UseIndex: Boolean;
  LineWidth: Integer;
  ViewWidth, ViewHeight: Integer);
var
  DX, DY, SX, SY, Err, E2: Integer;
begin
  if Memory = nil then Exit;
  if LineWidth <= 0 then Exit;

  // Bresenham's line algorithm
  DX := Abs(X2 - X1);
  DY := -Abs(Y2 - Y1);
  if X1 < X2 then SX := 1 else SX := -1;
  if Y1 < Y2 then SY := 1 else SY := -1;
  Err := DX + DY;

  while True do
  begin
    if LineWidth = 1 then
      SetPixelSafe(Memory, X1, Y1, Color, UseIndex, ViewWidth, ViewHeight)
    else
      DrawThickPixel(Memory, X1, Y1, Color, UseIndex, LineWidth, ViewWidth, ViewHeight);

    if (X1 = X2) and (Y1 = Y2) then Break;

    E2 := 2 * Err;
    if E2 >= DY then
    begin
      if X1 = X2 then Break;
      Err := Err + DY;
      X1 := X1 + SX;
    end;
    if E2 <= DX then
    begin
      if Y1 = Y2 then Break;
      Err := Err + DX;
      Y1 := Y1 + SY;
    end;
  end;
end;

procedure DrawBoxToMemory(Memory: TGraphicsMemory;
  X1, Y1, X2, Y2: Integer;
  Color: UInt32;
  UseIndex: Boolean;
  Filled: Boolean;
  LineWidth: Integer;
  Angle: Double;
  ViewWidth, ViewHeight: Integer);
var
  MinX, MinY, MaxX, MaxY: Integer;
  X, Y: Integer;
  CX, CY: Double;
  AngleRad: Double;
  V: array[0..3] of record X, Y: Double; end;
  i: Integer;
  FillMinY, FillMaxY, FillMinX, FillMaxX: Integer;
  InnerOffset: Integer;
  TestX, TestY: Double;
begin
  if Memory = nil then Exit;

  // Normalize coordinates
  if X1 < X2 then begin MinX := X1; MaxX := X2; end
  else begin MinX := X2; MaxX := X1; end;
  if Y1 < Y2 then begin MinY := Y1; MaxY := Y2; end
  else begin MinY := Y2; MaxY := Y1; end;

  // Handle rotation
  if Abs(Angle) > 0.001 then
  begin
    // Calculate center
    CX := (MinX + MaxX) / 2.0;
    CY := (MinY + MaxY) / 2.0;
    AngleRad := Angle * Pi / 180.0;

    // Calculate rotated vertices
    V[0].X := MinX; V[0].Y := MinY;
    V[1].X := MaxX; V[1].Y := MinY;
    V[2].X := MaxX; V[2].Y := MaxY;
    V[3].X := MinX; V[3].Y := MaxY;

    for i := 0 to 3 do
      RotatePoint(V[i].X, V[i].Y, CX, CY, AngleRad);

    // FIRST: Fill the rotated box (before drawing border)
    if Filled then
    begin
      // Calculate bounding box of rotated rectangle
      FillMinX := MaxInt; FillMaxX := -MaxInt;
      FillMinY := MaxInt; FillMaxY := -MaxInt;
      for i := 0 to 3 do
      begin
        if Round(V[i].X) < FillMinX then FillMinX := Round(V[i].X);
        if Round(V[i].X) > FillMaxX then FillMaxX := Round(V[i].X);
        if Round(V[i].Y) < FillMinY then FillMinY := Round(V[i].Y);
        if Round(V[i].Y) > FillMaxY then FillMaxY := Round(V[i].Y);
      end;

      // Clamp to viewport for efficiency
      if FillMinX < 0 then FillMinX := 0;
      if FillMinY < 0 then FillMinY := 0;
      if FillMaxX >= ViewWidth then FillMaxX := ViewWidth - 1;
      if FillMaxY >= ViewHeight then FillMaxY := ViewHeight - 1;

      // For each pixel in bounding box, check if inside rotated rect
      for Y := FillMinY to FillMaxY do
        for X := FillMinX to FillMaxX do
        begin
          // Transform point back to unrotated space and check if inside original rect
          TestX := X;
          TestY := Y;
          RotatePoint(TestX, TestY, CX, CY, -AngleRad);

          // Check if inside original (unrotated) rectangle - fill entire area
          if (TestX >= MinX) and (TestX <= MaxX) and
             (TestY >= MinY) and (TestY <= MaxY) then
            SetPixelSafe(Memory, X, Y, Color, UseIndex, ViewWidth, ViewHeight);
        end;
    end;

    // SECOND: Draw the four edges on top (border overwrites fill edges)
    if LineWidth > 0 then
    begin
      DrawLineToMemory(Memory, Round(V[0].X), Round(V[0].Y), Round(V[1].X), Round(V[1].Y),
                       Color, UseIndex, LineWidth, ViewWidth, ViewHeight);
      DrawLineToMemory(Memory, Round(V[1].X), Round(V[1].Y), Round(V[2].X), Round(V[2].Y),
                       Color, UseIndex, LineWidth, ViewWidth, ViewHeight);
      DrawLineToMemory(Memory, Round(V[2].X), Round(V[2].Y), Round(V[3].X), Round(V[3].Y),
                       Color, UseIndex, LineWidth, ViewWidth, ViewHeight);
      DrawLineToMemory(Memory, Round(V[3].X), Round(V[3].Y), Round(V[0].X), Round(V[0].Y),
                       Color, UseIndex, LineWidth, ViewWidth, ViewHeight);
    end;
  end
  else
  begin
    // No rotation - optimized axis-aligned drawing

    // Calculate fill area (inside the border)
    if Filled then
    begin
      // If LineWidth = 0, fill covers entire rect
      // If LineWidth >= 1, fill starts 1 pixel inside
      if LineWidth >= 1 then
        InnerOffset := 1
      else
        InnerOffset := 0;

      FillMinX := MinX + InnerOffset;
      FillMaxX := MaxX - InnerOffset;
      FillMinY := MinY + InnerOffset;
      FillMaxY := MaxY - InnerOffset;

      // Draw fill (with proper clipping - no clamping!)
      for Y := FillMinY to FillMaxY do
        for X := FillMinX to FillMaxX do
          SetPixelSafe(Memory, X, Y, Color, UseIndex, ViewWidth, ViewHeight);
    end;

    // Draw border if LineWidth > 0
    if LineWidth > 0 then
    begin
      // Calculate border expansion
      // WIDTH=1: border on the edge (0 inner, 1 outer from edge)
      // WIDTH=2: 1 inner + 1 outer
      // WIDTH=3: 1 inner + 2 outer
      // etc.

      // Top edge
      DrawLineToMemory(Memory, MinX, MinY, MaxX, MinY, Color, UseIndex, LineWidth, ViewWidth, ViewHeight);
      // Bottom edge
      DrawLineToMemory(Memory, MinX, MaxY, MaxX, MaxY, Color, UseIndex, LineWidth, ViewWidth, ViewHeight);
      // Left edge
      DrawLineToMemory(Memory, MinX, MinY, MinX, MaxY, Color, UseIndex, LineWidth, ViewWidth, ViewHeight);
      // Right edge
      DrawLineToMemory(Memory, MaxX, MinY, MaxX, MaxY, Color, UseIndex, LineWidth, ViewWidth, ViewHeight);
    end;
  end;
end;

procedure DrawCircleToMemory(Memory: TGraphicsMemory;
  CX, CY, RadiusX, RadiusY: Integer;
  Color: UInt32;
  UseIndex: Boolean;
  StartAngle, EndAngle, RotationAngle, AngleIncrement: Double;
  LineWidth: Integer;
  ViewWidth, ViewHeight: Integer);
var
  CurrentAngle, AngleRad, RotRad: Double;
  CosRot, SinRot: Double;
  Px, Py, Rx, Ry: Double;
  PrevX, PrevY, CurrX, CurrY: Integer;
  FirstX, FirstY: Integer;  // Store first point for closing the shape
  FirstPoint: Boolean;
  IsClosedShape: Boolean;
begin
  if Memory = nil then Exit;
  if LineWidth <= 0 then Exit;
  if AngleIncrement <= 0 then AngleIncrement := 2.0;

  // Check for zero radii
  if (RadiusX <= 0) or (RadiusY <= 0) then
  begin
    SetPixelSafe(Memory, CX, CY, Color, UseIndex, ViewWidth, ViewHeight);
    Exit;
  end;

  // Normalize angles
  while StartAngle < 0 do StartAngle := StartAngle + 360;
  while EndAngle < 0 do EndAngle := EndAngle + 360;
  if EndAngle < StartAngle then EndAngle := EndAngle + 360;

  // Check if this is a closed shape (full circle/polygon)
  // A shape is closed if the arc spans 360 degrees or more
  IsClosedShape := (EndAngle - StartAngle) >= 360 - 0.001;

  // Precompute rotation
  RotRad := RotationAngle * Pi / 180.0;
  CosRot := Cos(RotRad);
  SinRot := Sin(RotRad);

  CurrentAngle := StartAngle;
  FirstPoint := True;
  PrevX := 0;
  PrevY := 0;
  FirstX := 0;
  FirstY := 0;

  // Main drawing loop - draw until we reach EndAngle (exclusive for closed shapes)
  while CurrentAngle < EndAngle - 0.001 do
  begin
    AngleRad := CurrentAngle * Pi / 180.0;

    // Calculate point on ellipse - C128 convention:
    // 0°/360° = 12 o'clock (top), 90° = 3 o'clock (right),
    // 180° = 6 o'clock (bottom), 270° = 9 o'clock (left)
    // Drawing proceeds clockwise from start to end angle
    // Screen coordinates: Y increases downward
    Px := RadiusX * Sin(AngleRad);   // 0°->0, 90°->R, 180°->0, 270°->-R
    Py := -RadiusY * Cos(AngleRad);  // 0°->-R (top), 90°->0, 180°->R (bottom), 270°->0

    // Apply rotation around center (clockwise positive)
    Rx := Px * CosRot - Py * SinRot;
    Ry := Px * SinRot + Py * CosRot;

    // Translate to center
    CurrX := CX + Round(Rx);
    CurrY := CY + Round(Ry);

    if FirstPoint then
    begin
      FirstPoint := False;
      FirstX := CurrX;
      FirstY := CurrY;
      // Draw first point
      if LineWidth = 1 then
        SetPixelSafe(Memory, CurrX, CurrY, Color, UseIndex, ViewWidth, ViewHeight)
      else
        DrawThickPixel(Memory, CurrX, CurrY, Color, UseIndex, LineWidth, ViewWidth, ViewHeight);
    end
    else
    begin
      // Draw line from previous to current
      DrawLineToMemory(Memory, PrevX, PrevY, CurrX, CurrY, Color, UseIndex, LineWidth, ViewWidth, ViewHeight);
    end;

    PrevX := CurrX;
    PrevY := CurrY;

    // Advance to next angle
    CurrentAngle := CurrentAngle + AngleIncrement;
  end;

  // For closed shapes, draw the closing segment from last point back to first
  if IsClosedShape and (not FirstPoint) then
  begin
    DrawLineToMemory(Memory, PrevX, PrevY, FirstX, FirstY, Color, UseIndex, LineWidth, ViewWidth, ViewHeight);
  end
  else if (not IsClosedShape) and (not FirstPoint) then
  begin
    // For arcs (non-closed), draw to the exact end angle
    AngleRad := EndAngle * Pi / 180.0;
    Px := RadiusX * Sin(AngleRad);
    Py := -RadiusY * Cos(AngleRad);
    Rx := Px * CosRot - Py * SinRot;
    Ry := Px * SinRot + Py * CosRot;
    CurrX := CX + Round(Rx);
    CurrY := CY + Round(Ry);
    DrawLineToMemory(Memory, PrevX, PrevY, CurrX, CurrY, Color, UseIndex, LineWidth, ViewWidth, ViewHeight);
  end;
end;

end.
