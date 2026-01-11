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
unit SedaiGraphicsMemory;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Math,
  SedaiOutputInterface, SedaiGraphicsConfig, SedaiLogging;

type
  TGraphicsState = record
    CurrentMode: TGraphicMode;
    PaletteMode: Boolean;
    PaletteEnabled: Boolean;
    SplitLine: Integer;
    BackgroundColor: TPaletteIndex;
    ForegroundColor: TPaletteIndex;
    BorderColor: TPaletteIndex;
    Width, Height: Integer;
    BytesPerPixel: Integer;
  end;

  TClassicModeBuffer = record
    GraphicsBuffer: PByte;
    ColorBuffer: PByte;
    GraphicsSize: Integer;
    ColorSize: Integer;
    Valid: Boolean;
    LastUsed: QWord;
  end;

  { TGraphicsMemory }

  TGraphicsMemory = class
  private
    FTextBuffer: PByte;
    FGraphicsBuffer: PByte;
    FColorBuffer: PByte;
    FTextBufferSize: Integer;
    FGraphicsBufferSize: Integer;
    FColorBufferSize: Integer;
    FClassicBuffers: array[gm40ColText..gm80x50Mixed] of TClassicModeBuffer;
    FCurrentClassicMode: TGraphicMode;
    FIsClassicMode: Boolean;
    FState: TGraphicsState;
    FPalette: array[0..255] of UInt32;
    FDefaultC64Palette: array[0..15] of UInt32;

    procedure InitC64Palette;
    procedure InitExtendedPalette;
    procedure InitClassicBuffers;
    procedure SaveCurrentToClassicBuffer;
    procedure LoadCurrentFromClassicBuffer(Mode: TGraphicMode);
    procedure ClearClassicBuffer(Mode: TGraphicMode); overload;
    procedure ClearClassicBuffer(Mode: TGraphicMode; ClearColor: UInt32); overload;
    function IsClassicMode(Mode: TGraphicMode): Boolean;

    // Helper for coordinate validation
    function ValidateCoordinates(X, Y: Integer): Boolean; inline;

  public
    constructor Create;
    destructor Destroy; override;

    function AllocateBuffers(Width, Height: Integer; PaletteMode: Boolean; Mode: TGraphicMode): Boolean;  // Returns True if buffer was newly created
    procedure SaveTextFrame(Source: PByte; Size: Integer);
    procedure RestoreTextFrame(Dest: PByte; Size: Integer);
    procedure ClearGraphicsBuffer;
    procedure ClearColorBuffer;
    procedure SwitchToMode(NewMode: TGraphicMode; PreserveCurrent: Boolean = True);
    procedure ClearCurrentMode; overload;
    procedure ClearCurrentMode(ClearColor: UInt32); overload;
    procedure ClearCurrentModeWithIndex(PaletteIndex: TPaletteIndex);
    function HasValidBuffer(Mode: TGraphicMode): Boolean;

    procedure EnablePalette(Enable: Boolean);
    function IsPaletteEnabled: Boolean;
    procedure SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
    function GetPaletteColor(Index: TPaletteIndex): UInt32;
    procedure ResetPalette;
    function PaletteToRGB(Index: TPaletteIndex): UInt32;
    function RGBToPaletteIndex(RGB: UInt32): TPaletteIndex;

    procedure SetPixel(X, Y: Integer; RGB: UInt32); overload;
    procedure SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex); overload;
    function GetPixel(X, Y: Integer): UInt32;
    function GetPixelIndex(X, Y: Integer): TPaletteIndex;
    // Aliases for RGBA operations (same as SetPixel/GetPixel with UInt32)
    procedure SetPixelRGBA(X, Y: Integer; RGBA: UInt32); inline;
    function GetPixelRGBA(X, Y: Integer): UInt32; inline;

    procedure SetCurrentMode(Mode: TGraphicMode);
    procedure SetSplitLine(Line: Integer);

    property State: TGraphicsState read FState write FState;
    property GraphicsBuffer: PByte read FGraphicsBuffer;
    property ColorBuffer: PByte read FColorBuffer;
    property Palette[Index: TPaletteIndex]: UInt32 read GetPaletteColor write SetPaletteColor;
  end;

implementation

constructor TGraphicsMemory.Create;
begin
  inherited Create;
  FTextBuffer := nil;
  FGraphicsBuffer := nil;
  FColorBuffer := nil;
  InitC64Palette;
  InitExtendedPalette;
  InitClassicBuffers;

  // Default state
  FState.CurrentMode := gm40ColText;
  FState.PaletteMode := True;
  FState.PaletteEnabled := True;
  FState.SplitLine := 19;
  FState.BackgroundColor := C64_COLOR_BLUE;
  FState.ForegroundColor := C64_COLOR_LIGHT_BLUE;
  FState.BorderColor := C64_COLOR_LIGHT_BLUE;
  FState.Width := DEFAULT_SCREEN_WIDTH;
  FState.Height := DEFAULT_SCREEN_HEIGHT;
  FState.BytesPerPixel := BYTES_PER_PIXEL_RGBA;

  FCurrentClassicMode := gm40ColText;
  FIsClassicMode := True;
end;

destructor TGraphicsMemory.Destroy;
var
  Mode: TGraphicMode;
begin
  // Free text buffer (always separate)
  if Assigned(FTextBuffer) then FreeMem(FTextBuffer);

  // Only free FGraphicsBuffer/FColorBuffer if NOT in classic mode
  // (in classic mode, they point to FClassicBuffers which are freed below)
  if not FIsClassicMode then
  begin
    if Assigned(FGraphicsBuffer) then FreeMem(FGraphicsBuffer);
    if Assigned(FColorBuffer) then FreeMem(FColorBuffer);
  end;

  // Free all classic buffers
  for Mode := gm40ColText to gm80x50Mixed do
  begin
    if FClassicBuffers[Mode].Valid then
    begin
      if Assigned(FClassicBuffers[Mode].GraphicsBuffer) then
        FreeMem(FClassicBuffers[Mode].GraphicsBuffer);
      if Assigned(FClassicBuffers[Mode].ColorBuffer) then
        FreeMem(FClassicBuffers[Mode].ColorBuffer);
    end;
  end;

  inherited Destroy;
end;

procedure TGraphicsMemory.InitC64Palette;
var
  i: Integer;
begin
  // Load C64 palette from config
  for i := 0 to C64_PALETTE_SIZE - 1 do
    FDefaultC64Palette[i] := C64_PALETTE[i].RGBA;

  // Copy to main palette
  Move(FDefaultC64Palette[0], FPalette[0], 16 * SizeOf(UInt32));
end;

procedure TGraphicsMemory.InitExtendedPalette;
var
  i: Integer;
begin
  // Repeats the first 16 C64 colors for indices 16-255 (C64/C128 behavior)
  for i := 16 to 255 do
  begin
    FPalette[i] := FDefaultC64Palette[i mod 16];
  end;
end;

procedure TGraphicsMemory.InitClassicBuffers;
var
  Mode: TGraphicMode;
begin
  // Initialize persistent buffer array for all classic modes (0-10)
  for Mode := gm40ColText to gm80x50Mixed do
  begin
    FClassicBuffers[Mode].GraphicsBuffer := nil;
    FClassicBuffers[Mode].ColorBuffer := nil;
    FClassicBuffers[Mode].GraphicsSize := 0;
    FClassicBuffers[Mode].ColorSize := 0;
    FClassicBuffers[Mode].Valid := False;
    FClassicBuffers[Mode].LastUsed := 0;
  end;
end;

function TGraphicsMemory.IsClassicMode(Mode: TGraphicMode): Boolean;
begin
  Result := Mode in [gm40ColText..gm80x50Mixed]; // Modes 0-10 (all classic modes with persistent buffers)
end;

function TGraphicsMemory.AllocateBuffers(Width, Height: Integer; PaletteMode: Boolean; Mode: TGraphicMode): Boolean;
var
  WasClassicMode: Boolean;
  BufferExisted: Boolean;
begin
  WasClassicMode := FIsClassicMode;
  FIsClassicMode := IsClassicMode(Mode);

  if FIsClassicMode then
  begin
    // Check if buffer already exists before switching
    BufferExisted := FClassicBuffers[Mode].Valid;
    // Classic mode: Use persistent buffer
    SwitchToMode(Mode, True);
    // Return True if buffer was newly created
    Result := not BufferExisted;
  end
  else
  begin
    // Modern mode (gmSDL2Dynamic): need temporary buffer
    // Only free if previous mode was NOT a classic mode (to avoid freeing persistent buffers)
    if not WasClassicMode then
    begin
      if Assigned(FGraphicsBuffer) then FreeMem(FGraphicsBuffer);
      if Assigned(FColorBuffer) then FreeMem(FColorBuffer);
    end;
    // Reset pointers - classic mode buffers remain in FClassicBuffers array
    FGraphicsBuffer := nil;
    FColorBuffer := nil;

    FState.Width := Width;
    FState.Height := Height;
    FState.PaletteMode := PaletteMode;

    // Allocate temporary buffer
    FGraphicsBufferSize := Width * Height * 4;
    GetMem(FGraphicsBuffer, FGraphicsBufferSize);
    FillChar(FGraphicsBuffer^, FGraphicsBufferSize, 0);

    if PaletteMode then
    begin
      FColorBufferSize := Width * Height;
      GetMem(FColorBuffer, FColorBufferSize);
      FillChar(FColorBuffer^, FColorBufferSize, Ord(FState.BackgroundColor));
    end
    else
      FColorBuffer := nil;

    // Modern mode always returns True (always new buffer)
    Result := True;
  end;

  FState.CurrentMode := Mode;
end;

procedure TGraphicsMemory.SwitchToMode(NewMode: TGraphicMode; PreserveCurrent: Boolean);
var
  Width, Height: Integer;
  PaletteMode: Boolean;
begin
  if not IsClassicMode(NewMode) then
    Exit;

  // Save current mode if classic and requested
  if PreserveCurrent and FIsClassicMode and (FCurrentClassicMode <> NewMode) then
    SaveCurrentToClassicBuffer;

  FCurrentClassicMode := NewMode;

  // Determine new mode parameters based on resolution groups
  case NewMode of
    // 320x200 group (modes 0-2)
    gm40ColText, gmStandardBitmap, gmSplitBitmap:
      begin Width := 320; Height := 200; PaletteMode := True; end;
    // 160x200 group (modes 3-4) - multicolor uses same buffer size as 320x200
    gmMulticolorBitmap, gmSplitMulticolor:
      begin Width := 160; Height := 200; PaletteMode := True; end;
    // 640x200 group (modes 5-7)
    gm80ColText, gm80ColBitmap, gm80ColMixed:
      begin Width := 640; Height := 200; PaletteMode := True; end;
    // 640x400 group (modes 8-10)
    gm80x50Text, gm80x50Bitmap, gm80x50Mixed:
      begin Width := 640; Height := 400; PaletteMode := True; end;
  else
    begin Width := 640; Height := 400; PaletteMode := True; end;
  end;

  // Check if buffer exists for this mode
  if FClassicBuffers[NewMode].Valid then
  begin
    LoadCurrentFromClassicBuffer(NewMode);
  end
  else
  begin
    // Create new persistent buffer for this mode
    // DON'T free FGraphicsBuffer/FColorBuffer - they point to other mode's persistent buffers!
    // Just update the pointers to the new mode's buffers

    FGraphicsBufferSize := Width * Height * 4;
    if PaletteMode then
      FColorBufferSize := Width * Height
    else
      FColorBufferSize := 0;

    // Allocate persistent buffer
    GetMem(FClassicBuffers[NewMode].GraphicsBuffer, FGraphicsBufferSize);
    FillChar(FClassicBuffers[NewMode].GraphicsBuffer^, FGraphicsBufferSize, 0);
    FClassicBuffers[NewMode].GraphicsSize := FGraphicsBufferSize;

    if PaletteMode then
    begin
      GetMem(FClassicBuffers[NewMode].ColorBuffer, FColorBufferSize);
      FillChar(FClassicBuffers[NewMode].ColorBuffer^, FColorBufferSize, Ord(FState.BackgroundColor));
      FClassicBuffers[NewMode].ColorSize := FColorBufferSize;
    end;

    FClassicBuffers[NewMode].Valid := True;
    FClassicBuffers[NewMode].LastUsed := GetTickCount64;

    // Point to persistent buffers
    FGraphicsBuffer := FClassicBuffers[NewMode].GraphicsBuffer;
    FColorBuffer := FClassicBuffers[NewMode].ColorBuffer;
    FGraphicsBufferSize := FClassicBuffers[NewMode].GraphicsSize;
    FColorBufferSize := FClassicBuffers[NewMode].ColorSize;
  end;

  // Update state
  FState.Width := Width;
  FState.Height := Height;
  FState.PaletteMode := PaletteMode;
  FState.CurrentMode := NewMode;
end;

procedure TGraphicsMemory.SaveCurrentToClassicBuffer;
begin
  if not FIsClassicMode then
    Exit;
  FClassicBuffers[FCurrentClassicMode].LastUsed := GetTickCount64;
end;

procedure TGraphicsMemory.LoadCurrentFromClassicBuffer(Mode: TGraphicMode);
begin
  if not FClassicBuffers[Mode].Valid then
    Exit;

  FGraphicsBuffer := FClassicBuffers[Mode].GraphicsBuffer;
  FColorBuffer := FClassicBuffers[Mode].ColorBuffer;
  FGraphicsBufferSize := FClassicBuffers[Mode].GraphicsSize;
  FColorBufferSize := FClassicBuffers[Mode].ColorSize;

  FClassicBuffers[Mode].LastUsed := GetTickCount64;
end;

procedure TGraphicsMemory.ClearCurrentMode;
begin
  // Default: clear to black
  ClearCurrentMode($000000FF);  // Black with full alpha
end;

procedure TGraphicsMemory.ClearCurrentMode(ClearColor: UInt32);
var
  i: Integer;
  PixelCount: Integer;
  BufferPtr: PUInt32;
begin
  if FIsClassicMode then
    ClearClassicBuffer(FCurrentClassicMode, ClearColor)
  else
  begin
    // Clear graphics buffer with specified color
    if Assigned(FGraphicsBuffer) then
    begin
      PixelCount := FGraphicsBufferSize div 4;
      BufferPtr := PUInt32(FGraphicsBuffer);
      for i := 0 to PixelCount - 1 do
      begin
        BufferPtr^ := ClearColor;
        Inc(BufferPtr);
      end;
    end;
    ClearColorBuffer;
  end;
end;

procedure TGraphicsMemory.ClearCurrentModeWithIndex(PaletteIndex: TPaletteIndex);
var
  i: Integer;
  PixelCount: Integer;
  BufferPtr: PUInt32;
  ColorPtr: PByte;
  RGB: UInt32;
begin
  // Convert palette index to RGB
  RGB := FPalette[PaletteIndex];

  if FIsClassicMode then
  begin
    if not FClassicBuffers[FCurrentClassicMode].Valid then
      Exit;

    // Clear graphics buffer with palette color
    if Assigned(FClassicBuffers[FCurrentClassicMode].GraphicsBuffer) then
    begin
      PixelCount := FClassicBuffers[FCurrentClassicMode].GraphicsSize div 4;
      BufferPtr := PUInt32(FClassicBuffers[FCurrentClassicMode].GraphicsBuffer);
      for i := 0 to PixelCount - 1 do
      begin
        BufferPtr^ := RGB;
        Inc(BufferPtr);
      end;
    end;

    // Clear color buffer with palette index
    if Assigned(FClassicBuffers[FCurrentClassicMode].ColorBuffer) then
    begin
      PixelCount := FClassicBuffers[FCurrentClassicMode].ColorSize;
      ColorPtr := FClassicBuffers[FCurrentClassicMode].ColorBuffer;
      FillByte(ColorPtr^, PixelCount, PaletteIndex);
    end;
  end
  else
  begin
    // Non-classic mode: just clear with RGB
    if Assigned(FGraphicsBuffer) then
    begin
      PixelCount := FGraphicsBufferSize div 4;
      BufferPtr := PUInt32(FGraphicsBuffer);
      for i := 0 to PixelCount - 1 do
      begin
        BufferPtr^ := RGB;
        Inc(BufferPtr);
      end;
    end;

    // Clear color buffer with palette index
    if Assigned(FColorBuffer) then
    begin
      PixelCount := FColorBufferSize;
      FillByte(FColorBuffer^, PixelCount, PaletteIndex);
    end;
  end;
end;

procedure TGraphicsMemory.ClearClassicBuffer(Mode: TGraphicMode);
begin
  // Default: clear to black
  ClearClassicBuffer(Mode, $000000FF);
end;

procedure TGraphicsMemory.ClearClassicBuffer(Mode: TGraphicMode; ClearColor: UInt32);
var
  i: Integer;
  PixelCount: Integer;
  BufferPtr: PUInt32;
begin
  if not FClassicBuffers[Mode].Valid then
    Exit;

  // Clear graphics buffer with specified color
  if Assigned(FClassicBuffers[Mode].GraphicsBuffer) then
  begin
    PixelCount := FClassicBuffers[Mode].GraphicsSize div 4;
    BufferPtr := PUInt32(FClassicBuffers[Mode].GraphicsBuffer);
    for i := 0 to PixelCount - 1 do
    begin
      BufferPtr^ := ClearColor;
      Inc(BufferPtr);
    end;
  end;

  // Clear color buffer with background palette index
  if Assigned(FClassicBuffers[Mode].ColorBuffer) then
    FillChar(FClassicBuffers[Mode].ColorBuffer^, FClassicBuffers[Mode].ColorSize, Ord(FState.BackgroundColor));
end;

function TGraphicsMemory.HasValidBuffer(Mode: TGraphicMode): Boolean;
begin
  if IsClassicMode(Mode) then
    Result := FClassicBuffers[Mode].Valid
  else
    Result := Assigned(FGraphicsBuffer);
end;

// === I TUOI METODI ESISTENTI ===

procedure TGraphicsMemory.EnablePalette(Enable: Boolean);
begin
  FState.PaletteEnabled := Enable;
end;

function TGraphicsMemory.IsPaletteEnabled: Boolean;
begin
  Result := FState.PaletteEnabled;
end;

procedure TGraphicsMemory.SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
begin
  FPalette[Index] := RGB or $FF000000; // Forza alpha = 255
end;

function TGraphicsMemory.GetPaletteColor(Index: TPaletteIndex): UInt32;
begin
  Result := FPalette[Index];
end;

procedure TGraphicsMemory.ResetPalette;
var
  i: Integer;
begin
  // Complete reset: C64 in first 16, then repetition
  Move(FDefaultC64Palette[0], FPalette[0], 16 * SizeOf(UInt32));

  // Repeats C64 pattern for indices 16-255
  for i := 16 to 255 do
  begin
    FPalette[i] := FDefaultC64Palette[i mod 16];
  end;
end;

function TGraphicsMemory.PaletteToRGB(Index: TPaletteIndex): UInt32;
begin
  Result := FPalette[Index];
end;

function TGraphicsMemory.RGBToPaletteIndex(RGB: UInt32): TPaletteIndex;
var
  i: Integer;
  MinDist, Dist: UInt32;
  R1, G1, B1, R2, G2, B2: Byte;
  DR, DG, DB: Integer;
begin
  // Find the closest color in the palette
  Result := 0;
  MinDist := $FFFFFFFF;

  R1 := (RGB shr 16) and $FF;
  G1 := (RGB shr 8) and $FF;
  B1 := RGB and $FF;

  for i := 0 to 255 do
  begin
    R2 := (FPalette[i] shr 16) and $FF;
    G2 := (FPalette[i] shr 8) and $FF;
    B2 := FPalette[i] and $FF;

    DR := R1 - R2;
    DG := G1 - G2;
    DB := B1 - B2;

    Dist := DR*DR + DG*DG + DB*DB;
    if Dist < MinDist then
    begin
      MinDist := Dist;
      Result := i;
    end;
  end;
end;

function TGraphicsMemory.ValidateCoordinates(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (X < FState.Width) and (Y >= 0) and (Y < FState.Height);
end;

procedure TGraphicsMemory.SetPixel(X, Y: Integer; RGB: UInt32);
var
  Offset: Integer;
  RGBA: PUInt32;
  PaletteIndex: TPaletteIndex;
begin
  if not ValidateCoordinates(X, Y) then
    Exit;

  if FState.PaletteMode and FState.PaletteEnabled then
  begin
    // Converts RGB to closest palette index
    PaletteIndex := RGBToPaletteIndex(RGB);

    // Save index in color buffer
    if Assigned(FColorBuffer) then
    begin
      Offset := Y * FState.Width + X;
      PByte(FColorBuffer + Offset)^ := PaletteIndex;
    end;

    // Use palette color for graphics buffer
    RGB := FPalette[PaletteIndex];
  end;

  // Save in RGB buffer
  Offset := (Y * FState.Width + X) * 4;
  RGBA := PUInt32(FGraphicsBuffer + Offset);
  RGBA^ := RGB;
end;

procedure TGraphicsMemory.SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex);
var
  Offset: Integer;
  RGBA: PUInt32;
begin
  if not ValidateCoordinates(X, Y) then
    Exit;

  // Save index in color buffer if in palette mode
  if FState.PaletteMode and Assigned(FColorBuffer) then
  begin
    Offset := Y * FState.Width + X;
    PByte(FColorBuffer + Offset)^ := PaletteIndex;
  end;

  // Converts to RGB via palette and saves
  Offset := (Y * FState.Width + X) * 4;
  RGBA := PUInt32(FGraphicsBuffer + Offset);
  RGBA^ := FPalette[PaletteIndex];
end;

function TGraphicsMemory.GetPixel(X, Y: Integer): UInt32;
var
  Offset: Integer;
  RGBA: PUInt32;
begin
  Result := 0;
  if not ValidateCoordinates(X, Y) then
    Exit;

  Offset := (Y * FState.Width + X) * 4;
  RGBA := PUInt32(FGraphicsBuffer + Offset);
  Result := RGBA^;
end;

function TGraphicsMemory.GetPixelIndex(X, Y: Integer): TPaletteIndex;
var
  Offset: Integer;
begin
  Result := 0;
  if not ValidateCoordinates(X, Y) then
    Exit;

  if FState.PaletteMode and Assigned(FColorBuffer) then
  begin
    Offset := Y * FState.Width + X;
    Result := PByte(FColorBuffer + Offset)^;
  end
  else
  begin
    // Converts RGB to closest palette index
    Result := RGBToPaletteIndex(GetPixel(X, Y));
  end;
end;

procedure TGraphicsMemory.SetPixelRGBA(X, Y: Integer; RGBA: UInt32);
begin
  SetPixel(X, Y, RGBA);
end;

function TGraphicsMemory.GetPixelRGBA(X, Y: Integer): UInt32;
begin
  Result := GetPixel(X, Y);
end;

procedure TGraphicsMemory.SetCurrentMode(Mode: TGraphicMode);
begin
  FState.CurrentMode := Mode;
end;

procedure TGraphicsMemory.SetSplitLine(Line: Integer);
begin
  FState.SplitLine := Line;
end;

procedure TGraphicsMemory.SaveTextFrame(Source: PByte; Size: Integer);
begin
  if Assigned(FTextBuffer) and (Size <> FTextBufferSize) then
  begin
    FreeMem(FTextBuffer);
    FTextBuffer := nil;
  end;

  if not Assigned(FTextBuffer) then
  begin
    FTextBufferSize := Size;
    GetMem(FTextBuffer, FTextBufferSize);
  end;

  Move(Source^, FTextBuffer^, Size);
end;

procedure TGraphicsMemory.RestoreTextFrame(Dest: PByte; Size: Integer);
begin
  if Assigned(FTextBuffer) and (Size <= FTextBufferSize) then
    Move(FTextBuffer^, Dest^, Size);
end;

procedure TGraphicsMemory.ClearGraphicsBuffer;
begin
  if Assigned(FGraphicsBuffer) then
    FillChar(FGraphicsBuffer^, FGraphicsBufferSize, 0);
end;

procedure TGraphicsMemory.ClearColorBuffer;
begin
  if Assigned(FColorBuffer) then
    FillChar(FColorBuffer^, FColorBufferSize, Ord(FState.BackgroundColor));
end;

end.
