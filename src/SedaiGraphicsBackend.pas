{*
 * SedaiBasic - A BASIC interpreter with bytecode VM
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * This program is dual-licensed:
 *
 * 1) For open source use: GNU General Public License version 3 (GPL-3.0-only)
 *    See <https://www.gnu.org/licenses/gpl-3.0.html>
 *
 * 2) For commercial/proprietary use: A separate commercial license is required.
 *    Contact: maurizio.cammalleri@gmail.com for licensing inquiries.
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}

{ FreeBASIC graphics — Phase 1: backend abstraction.

  IGraphicsBackend is an OPERATION-LEVEL interface (draw a line / fill a rect / blit an image — NOT
  "here is a finished pixel buffer"). The VM/lowering target these operations, so a backend can
  ACCELERATE where hardware exists (host GPU, SoC 2D blitter/DMA) while the software reference
  implementation (TSoftwareGraphicsBackend) provides the universal, deterministic, headless-testable
  fallback.

  Acceleration policy (confirmed): accelerable ops = Present, Blit, DrawRect(fill); CPU-everywhere ops
  = SetPixel/GetPixel/DrawLine/DrawEllipse/Fill(flood). The drawable surface is always a CPU buffer
  (source of truth), NEVER a GPU render-target — so POINT/GET read back deterministically and the model
  maps directly onto a bare-metal linear framebuffer. See job/docs/GRAPHICS_PHASE1_DESIGN.md. }
unit SedaiGraphicsBackend;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils,
  SedaiOutputInterface, SedaiGraphicsMemory, SedaiGraphicsPrimitives;

type
  TGfxSurface = Integer;   // 0 = screen; >0 = image buffers (IMAGECREATE — deferred to phase 2/G3)
  TGfxColor   = UInt32;    // RGBA packed in the engine's internal ABGR layout ($AABBGGRR)
  TGfxBlitMode = (gbmPSet, gbmTrans, gbmAlpha, gbmAnd, gbmOr, gbmXor, gbmAdd, gbmCustom);

const
  GFX_SCREEN_SURFACE  = 0;
  GFX_INVALID_SURFACE = -1;

type
  IGraphicsBackend = interface
    ['{2C7A9E10-1A2B-4C3D-9E5F-A1B2C3D4E5F6}']
    // --- screen / lifecycle ---
    function  SetMode(Mode: TGraphicMode; ClearBuffer: Boolean; SplitLine: Integer): Boolean;  // legacy C128 modes
    function  ResizeScreen(W, H, Depth: Integer): Boolean;     // base for SCREENRES (truecolor surface)
    function  GetMode: TGraphicMode;
    function  InGraphics: Boolean;
    procedure ClearSurface(Surface: TGfxSurface; Color: TGfxColor);
    procedure Present;                                          // accelerable; no-op on the software backend
    // --- surfaces / images (image creation deferred to phase 2/G3) ---
    function  ScreenSurface: TGfxSurface;
    function  CreateSurface(W, H: Integer; Fill: TGfxColor): TGfxSurface;
    procedure DestroySurface(Surface: TGfxSurface);
    function  SurfaceWidth(Surface: TGfxSurface): Integer;
    function  SurfaceHeight(Surface: TGfxSurface): Integer;
    // --- drawing (target = a surface) ---
    procedure SetPixel(Surface: TGfxSurface; X, Y: Integer; Color: TGfxColor);
    function  GetPixel(Surface: TGfxSurface; X, Y: Integer): TGfxColor;   // POINT / GET (deterministic)
    procedure DrawLine(Surface: TGfxSurface; X1, Y1, X2, Y2: Integer; Color: TGfxColor; LineWidth: Integer);
    procedure DrawRect(Surface: TGfxSurface; X1, Y1, X2, Y2: Integer; Color: TGfxColor; Filled: Boolean; LineWidth: Integer; Angle: Double);
    procedure DrawEllipse(Surface: TGfxSurface; CX, CY, RX, RY: Integer; Color: TGfxColor; StartAngle, EndAngle, RotationAngle, AngleStep: Double; LineWidth: Integer);
    procedure Fill(Surface: TGfxSurface; X, Y: Integer; Color: TGfxColor);   // flood fill (PAINT)
    procedure FillBorder(Surface: TGfxSurface; X, Y: Integer; Color, BorderColor: TGfxColor);  // boundary fill (PAINT ...,border)
    procedure SetClip(Surface: TGfxSurface; Active: Boolean; X1, Y1, X2, Y2: Integer);  // VIEW clip rect
    procedure Blit(Dst: TGfxSurface; X, Y: Integer; Src: TGfxSurface; Mode: TGfxBlitMode);  // accelerable; deferred to G3
    // --- palette ---
    procedure EnablePalette(Enable: Boolean);
    procedure SetPaletteColor(Index: TPaletteIndex; Color: TGfxColor);
    function  GetPaletteColor(Index: TPaletteIndex): TGfxColor;
    procedure ResetPalette;
  end;

  { Software reference backend: TGraphicsMemory + the *ToMemory primitives. No SDL, headless, fully
    deterministic — the universal fallback and the test oracle. Image surfaces (>0) are backed by
    additional TGraphicsMemory instances in FImages (surface id = index+1; freed slots stay nil). }
  TSoftwareGraphicsBackend = class(TObject, IGraphicsBackend)
  private
    FScreen: TGraphicsMemory;
    FInGraphics: Boolean;
    FImages: array of TGraphicsMemory;   // image surfaces; id = index+1, 0 = screen
    function ValidScreen: Boolean; inline;
    function MemoryOf(Surface: TGfxSurface): TGraphicsMemory;   // screen (0) or image (>0), nil if invalid
  public
    constructor Create;
    destructor Destroy; override;
    function  SetMode(Mode: TGraphicMode; ClearBuffer: Boolean; SplitLine: Integer): Boolean;
    function  ResizeScreen(W, H, Depth: Integer): Boolean;
    function  GetMode: TGraphicMode;
    function  InGraphics: Boolean;
    procedure ClearSurface(Surface: TGfxSurface; Color: TGfxColor);
    procedure Present;
    function  ScreenSurface: TGfxSurface;
    function  CreateSurface(W, H: Integer; Fill: TGfxColor): TGfxSurface;
    procedure DestroySurface(Surface: TGfxSurface);
    function  SurfaceWidth(Surface: TGfxSurface): Integer;
    function  SurfaceHeight(Surface: TGfxSurface): Integer;
    procedure SetPixel(Surface: TGfxSurface; X, Y: Integer; Color: TGfxColor);
    function  GetPixel(Surface: TGfxSurface; X, Y: Integer): TGfxColor;
    procedure DrawLine(Surface: TGfxSurface; X1, Y1, X2, Y2: Integer; Color: TGfxColor; LineWidth: Integer);
    procedure DrawRect(Surface: TGfxSurface; X1, Y1, X2, Y2: Integer; Color: TGfxColor; Filled: Boolean; LineWidth: Integer; Angle: Double);
    procedure DrawEllipse(Surface: TGfxSurface; CX, CY, RX, RY: Integer; Color: TGfxColor; StartAngle, EndAngle, RotationAngle, AngleStep: Double; LineWidth: Integer);
    procedure Fill(Surface: TGfxSurface; X, Y: Integer; Color: TGfxColor);
    procedure FillBorder(Surface: TGfxSurface; X, Y: Integer; Color, BorderColor: TGfxColor);
    procedure SetClip(Surface: TGfxSurface; Active: Boolean; X1, Y1, X2, Y2: Integer);
    procedure Blit(Dst: TGfxSurface; X, Y: Integer; Src: TGfxSurface; Mode: TGfxBlitMode);
    procedure EnablePalette(Enable: Boolean);
    procedure SetPaletteColor(Index: TPaletteIndex; Color: TGfxColor);
    function  GetPaletteColor(Index: TPaletteIndex): TGfxColor;
    procedure ResetPalette;
    // Window presenter access (sb --window): the screen surface's CPU framebuffer is the source of truth.
    function  ScreenMemory: TGraphicsMemory;
  end;

implementation

constructor TSoftwareGraphicsBackend.Create;
begin
  inherited Create;
  FScreen := TGraphicsMemory.Create;
  FInGraphics := False;
end;

destructor TSoftwareGraphicsBackend.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FImages) do
    FImages[i].Free;
  SetLength(FImages, 0);
  FScreen.Free;
  inherited Destroy;
end;

function TSoftwareGraphicsBackend.ValidScreen: Boolean;
begin
  Result := Assigned(FScreen) and Assigned(FScreen.GraphicsBuffer);
end;

function TSoftwareGraphicsBackend.MemoryOf(Surface: TGfxSurface): TGraphicsMemory;
// Screen (0) or an image surface (>0). Returns nil for an invalid/destroyed surface.
begin
  if Surface = GFX_SCREEN_SURFACE then
  begin
    if ValidScreen then Result := FScreen else Result := nil;
  end
  else if (Surface >= 1) and (Surface <= Length(FImages)) then
    Result := FImages[Surface - 1]
  else
    Result := nil;
end;

function TSoftwareGraphicsBackend.SetMode(Mode: TGraphicMode; ClearBuffer: Boolean; SplitLine: Integer): Boolean;
begin
  // Legacy C128 modes: TGraphicsMemory picks the resolution from the mode group.
  FScreen.AllocateBuffers(0, 0, True, Mode);
  if SplitLine >= 0 then FScreen.SetSplitLine(SplitLine);
  if ClearBuffer then FScreen.ClearCurrentMode;
  FInGraphics := not (Mode in [gm40ColText, gm80ColText, gm80x50Text]);
  Result := True;
end;

function TSoftwareGraphicsBackend.ResizeScreen(W, H, Depth: Integer): Boolean;
begin
  // FreeBASIC SCREENRES: a truecolor (non-palette) surface of W x H. Depth ignored in phase 1.
  if (W <= 0) or (H <= 0) then Exit(False);
  FScreen.AllocateBuffers(W, H, False, gmSDL2Dynamic);
  FScreen.ClearCurrentMode($000000FF);   // black, opaque
  FInGraphics := True;
  Result := True;
end;

function TSoftwareGraphicsBackend.GetMode: TGraphicMode;
begin
  Result := FScreen.State.CurrentMode;
end;

function TSoftwareGraphicsBackend.InGraphics: Boolean;
begin
  Result := FInGraphics;
end;

procedure TSoftwareGraphicsBackend.ClearSurface(Surface: TGfxSurface; Color: TGfxColor);
var M: TGraphicsMemory;
begin
  M := MemoryOf(Surface);
  if Assigned(M) then M.ClearCurrentMode(Color);
end;

procedure TSoftwareGraphicsBackend.Present;
begin
  // Software backend: nothing to present (the pixel buffer IS the result; a display adapter blits it).
end;

function TSoftwareGraphicsBackend.ScreenSurface: TGfxSurface;
begin
  Result := GFX_SCREEN_SURFACE;
end;

function TSoftwareGraphicsBackend.CreateSurface(W, H: Integer; Fill: TGfxColor): TGfxSurface;
// Allocate a truecolor image surface of W x H, cleared to Fill. Reuses a freed (nil) slot if any.
var
  Img: TGraphicsMemory;
  i, Slot: Integer;
begin
  if (W <= 0) or (H <= 0) then Exit(GFX_INVALID_SURFACE);
  Img := TGraphicsMemory.Create;
  Img.AllocateBuffers(W, H, False, gmSDL2Dynamic);
  Img.ClearCurrentMode(Fill);
  Slot := -1;
  for i := 0 to High(FImages) do
    if FImages[i] = nil then begin Slot := i; Break; end;
  if Slot < 0 then
  begin
    SetLength(FImages, Length(FImages) + 1);
    Slot := High(FImages);
  end;
  FImages[Slot] := Img;
  Result := Slot + 1;   // id 0 is reserved for the screen
end;

procedure TSoftwareGraphicsBackend.DestroySurface(Surface: TGfxSurface);
begin
  if (Surface >= 1) and (Surface <= Length(FImages)) and Assigned(FImages[Surface - 1]) then
  begin
    FImages[Surface - 1].Free;
    FImages[Surface - 1] := nil;   // leave the slot for reuse (ids stay stable)
  end;
end;

function TSoftwareGraphicsBackend.SurfaceWidth(Surface: TGfxSurface): Integer;
var M: TGraphicsMemory;
begin
  M := MemoryOf(Surface);
  if Assigned(M) then Result := M.State.Width else Result := 0;
end;

function TSoftwareGraphicsBackend.SurfaceHeight(Surface: TGfxSurface): Integer;
var M: TGraphicsMemory;
begin
  M := MemoryOf(Surface);
  if Assigned(M) then Result := M.State.Height else Result := 0;
end;

procedure TSoftwareGraphicsBackend.SetPixel(Surface: TGfxSurface; X, Y: Integer; Color: TGfxColor);
var M: TGraphicsMemory;
begin
  M := MemoryOf(Surface);
  if Assigned(M) then M.SetPixel(X, Y, Color);
end;

function TSoftwareGraphicsBackend.GetPixel(Surface: TGfxSurface; X, Y: Integer): TGfxColor;
var M: TGraphicsMemory;
begin
  M := MemoryOf(Surface);
  if Assigned(M) then Result := M.GetPixel(X, Y) else Result := 0;
end;

procedure TSoftwareGraphicsBackend.DrawLine(Surface: TGfxSurface; X1, Y1, X2, Y2: Integer; Color: TGfxColor; LineWidth: Integer);
var M: TGraphicsMemory;
begin
  M := MemoryOf(Surface);
  if Assigned(M) then
    DrawLineToMemory(M, X1, Y1, X2, Y2, Color, False, LineWidth, M.State.Width, M.State.Height);
end;

procedure TSoftwareGraphicsBackend.DrawRect(Surface: TGfxSurface; X1, Y1, X2, Y2: Integer; Color: TGfxColor; Filled: Boolean; LineWidth: Integer; Angle: Double);
var M: TGraphicsMemory;
begin
  M := MemoryOf(Surface);
  if Assigned(M) then
    DrawBoxToMemory(M, X1, Y1, X2, Y2, Color, False, Filled, LineWidth, Angle, M.State.Width, M.State.Height);
end;

procedure TSoftwareGraphicsBackend.DrawEllipse(Surface: TGfxSurface; CX, CY, RX, RY: Integer; Color: TGfxColor; StartAngle, EndAngle, RotationAngle, AngleStep: Double; LineWidth: Integer);
var M: TGraphicsMemory;
begin
  M := MemoryOf(Surface);
  if Assigned(M) then
    DrawCircleToMemory(M, CX, CY, RX, RY, Color, False, StartAngle, EndAngle, RotationAngle, AngleStep, LineWidth, M.State.Width, M.State.Height);
end;

procedure TSoftwareGraphicsBackend.Fill(Surface: TGfxSurface; X, Y: Integer; Color: TGfxColor);
// Iterative 4-way flood fill (PAINT). Deterministic; reuses GetPixel/SetPixel. Stack via dynamic array
// to avoid recursion overflow on large regions.
var
  W, H, Top: Integer;
  Target: TGfxColor;
  Stack: array of record PX, PY: Integer; end;

  procedure Push(AX, AY: Integer);
  begin
    if (AX < 0) or (AY < 0) or (AX >= W) or (AY >= H) then Exit;
    if Top >= Length(Stack) then SetLength(Stack, (Top + 1) * 2);
    Stack[Top].PX := AX; Stack[Top].PY := AY; Inc(Top);
  end;

var
  CX, CY: Integer;
  M: TGraphicsMemory;
begin
  M := MemoryOf(Surface);
  if not Assigned(M) then Exit;
  W := M.State.Width; H := M.State.Height;
  if (X < 0) or (Y < 0) or (X >= W) or (Y >= H) then Exit;
  Target := M.GetPixel(X, Y);
  if Target = Color then Exit;
  Top := 0;
  SetLength(Stack, 64);
  Push(X, Y);
  while Top > 0 do
  begin
    Dec(Top);
    CX := Stack[Top].PX; CY := Stack[Top].PY;
    if M.GetPixel(CX, CY) <> Target then Continue;
    M.SetPixel(CX, CY, Color);
    Push(CX + 1, CY); Push(CX - 1, CY);
    Push(CX, CY + 1); Push(CX, CY - 1);
  end;
end;

procedure TSoftwareGraphicsBackend.FillBorder(Surface: TGfxSurface; X, Y: Integer; Color, BorderColor: TGfxColor);
// Iterative 4-way boundary fill (FreeBASIC "PAINT (x,y), color, border"): flood outward from (x,y),
// filling every pixel that is not already the border colour, stopping at the border. Mirrors Fill but the
// stop condition is "pixel = BorderColor" rather than "pixel <> seed colour".
var
  W, H, Top: Integer;
  Stack: array of record PX, PY: Integer; end;

  procedure Push(AX, AY: Integer);
  begin
    if (AX < 0) or (AY < 0) or (AX >= W) or (AY >= H) then Exit;
    if Top >= Length(Stack) then SetLength(Stack, (Top + 1) * 2);
    Stack[Top].PX := AX; Stack[Top].PY := AY; Inc(Top);
  end;

var
  CX, CY: Integer;
  PC: TGfxColor;
  M: TGraphicsMemory;
begin
  M := MemoryOf(Surface);
  if not Assigned(M) then Exit;
  W := M.State.Width; H := M.State.Height;
  if (X < 0) or (Y < 0) or (X >= W) or (Y >= H) then Exit;
  if M.GetPixel(X, Y) = BorderColor then Exit;   // seed already on the border
  Top := 0;
  SetLength(Stack, 64);
  Push(X, Y);
  while Top > 0 do
  begin
    Dec(Top);
    CX := Stack[Top].PX; CY := Stack[Top].PY;
    PC := M.GetPixel(CX, CY);
    if (PC = BorderColor) or (PC = Color) then Continue;   // hit the border, or already filled
    M.SetPixel(CX, CY, Color);
    Push(CX + 1, CY); Push(CX - 1, CY);
    Push(CX, CY + 1); Push(CX, CY - 1);
  end;
end;

procedure TSoftwareGraphicsBackend.SetClip(Surface: TGfxSurface; Active: Boolean; X1, Y1, X2, Y2: Integer);
var M: TGraphicsMemory;
begin
  M := MemoryOf(Surface);
  if Assigned(M) then M.SetClip(Active, X1, Y1, X2, Y2);
end;

procedure TSoftwareGraphicsBackend.Blit(Dst: TGfxSurface; X, Y: Integer; Src: TGfxSurface; Mode: TGfxBlitMode);
// Blit the whole Src surface onto Dst at top-left (X,Y), per-pixel, applying the blit mode. Colours are
// in the engine ABGR layout ($AABBGGRR). TRANS skips magenta (RGB 255,0,255). CUSTOM falls back to PSET.
const
  TRANS_KEY = TGfxColor($FFFF00FF);   // ABGR magenta = RGB(255,0,255)
var
  MD, MS: TGraphicsMemory;
  SW, SH, DW, DH, sx, sy, dx, dy: Integer;
  sc, dc, nc: TGfxColor;

  function Blend(D, S: TGfxColor): TGfxColor;
  var dr, dg, db, sr, sg, sb, sa, r, g, b: Integer;
  begin
    case Mode of
      gbmAnd: Result := D and S;
      gbmOr:  Result := D or S;
      gbmXor: Result := D xor S;
      gbmAdd:
        begin
          sr := S and $FF; sg := (S shr 8) and $FF; sb := (S shr 16) and $FF;
          dr := D and $FF; dg := (D shr 8) and $FF; db := (D shr 16) and $FF;
          r := dr + sr; if r > 255 then r := 255;
          g := dg + sg; if g > 255 then g := 255;
          b := db + sb; if b > 255 then b := 255;
          Result := (D and $FF000000) or TGfxColor(b shl 16) or TGfxColor(g shl 8) or TGfxColor(r);
        end;
      gbmAlpha:
        begin
          sa := (S shr 24) and $FF;
          sr := S and $FF; sg := (S shr 8) and $FF; sb := (S shr 16) and $FF;
          dr := D and $FF; dg := (D shr 8) and $FF; db := (D shr 16) and $FF;
          r := (sr * sa + dr * (255 - sa)) div 255;
          g := (sg * sa + dg * (255 - sa)) div 255;
          b := (sb * sa + db * (255 - sa)) div 255;
          Result := $FF000000 or TGfxColor(b shl 16) or TGfxColor(g shl 8) or TGfxColor(r);
        end;
    else
      Result := S;   // gbmPSet / gbmTrans (handled by caller) / gbmCustom
    end;
  end;

begin
  MD := MemoryOf(Dst);
  MS := MemoryOf(Src);
  if (not Assigned(MD)) or (not Assigned(MS)) then Exit;
  SW := MS.State.Width;  SH := MS.State.Height;
  DW := MD.State.Width;  DH := MD.State.Height;
  for sy := 0 to SH - 1 do
  begin
    dy := Y + sy;
    if (dy < 0) or (dy >= DH) then Continue;
    for sx := 0 to SW - 1 do
    begin
      dx := X + sx;
      if (dx < 0) or (dx >= DW) then Continue;
      sc := MS.GetPixel(sx, sy);
      if (Mode = gbmTrans) and (sc = TRANS_KEY) then Continue;   // transparent pixel: leave dst
      if Mode in [gbmPSet, gbmTrans, gbmCustom] then
        nc := sc
      else
      begin
        dc := MD.GetPixel(dx, dy);
        nc := Blend(dc, sc);
      end;
      MD.SetPixel(dx, dy, nc);
    end;
  end;
end;

procedure TSoftwareGraphicsBackend.EnablePalette(Enable: Boolean);
begin
  FScreen.EnablePalette(Enable);
end;

procedure TSoftwareGraphicsBackend.SetPaletteColor(Index: TPaletteIndex; Color: TGfxColor);
begin
  FScreen.SetPaletteColor(Index, Color);
end;

function TSoftwareGraphicsBackend.GetPaletteColor(Index: TPaletteIndex): TGfxColor;
begin
  Result := FScreen.GetPaletteColor(Index);
end;

procedure TSoftwareGraphicsBackend.ResetPalette;
begin
  FScreen.ResetPalette;
end;

function TSoftwareGraphicsBackend.ScreenMemory: TGraphicsMemory;
begin
  Result := FScreen;
end;

end.
