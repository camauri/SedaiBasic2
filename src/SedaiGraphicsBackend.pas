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
    procedure Blit(Dst: TGfxSurface; X, Y: Integer; Src: TGfxSurface; Mode: TGfxBlitMode);  // accelerable; deferred to G3
    // --- palette ---
    procedure EnablePalette(Enable: Boolean);
    procedure SetPaletteColor(Index: TPaletteIndex; Color: TGfxColor);
    function  GetPaletteColor(Index: TPaletteIndex): TGfxColor;
    procedure ResetPalette;
  end;

  { Software reference backend: TGraphicsMemory + the *ToMemory primitives. No SDL, headless, fully
    deterministic — the universal fallback and the test oracle. Image surfaces (>0) are deferred. }
  TSoftwareGraphicsBackend = class(TObject, IGraphicsBackend)
  private
    FScreen: TGraphicsMemory;
    FInGraphics: Boolean;
    function ValidScreen: Boolean; inline;
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
    procedure Blit(Dst: TGfxSurface; X, Y: Integer; Src: TGfxSurface; Mode: TGfxBlitMode);
    procedure EnablePalette(Enable: Boolean);
    procedure SetPaletteColor(Index: TPaletteIndex; Color: TGfxColor);
    function  GetPaletteColor(Index: TPaletteIndex): TGfxColor;
    procedure ResetPalette;
  end;

implementation

constructor TSoftwareGraphicsBackend.Create;
begin
  inherited Create;
  FScreen := TGraphicsMemory.Create;
  FInGraphics := False;
end;

destructor TSoftwareGraphicsBackend.Destroy;
begin
  FScreen.Free;
  inherited Destroy;
end;

function TSoftwareGraphicsBackend.ValidScreen: Boolean;
begin
  Result := Assigned(FScreen) and Assigned(FScreen.GraphicsBuffer);
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
begin
  if (Surface = GFX_SCREEN_SURFACE) and ValidScreen then
    FScreen.ClearCurrentMode(Color);
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
begin
  // Image buffers (IMAGECREATE) deferred to phase 2/G3.
  Result := GFX_INVALID_SURFACE;
end;

procedure TSoftwareGraphicsBackend.DestroySurface(Surface: TGfxSurface);
begin
  // No image surfaces yet (deferred).
end;

function TSoftwareGraphicsBackend.SurfaceWidth(Surface: TGfxSurface): Integer;
begin
  if (Surface = GFX_SCREEN_SURFACE) and Assigned(FScreen) then Result := FScreen.State.Width
  else Result := 0;
end;

function TSoftwareGraphicsBackend.SurfaceHeight(Surface: TGfxSurface): Integer;
begin
  if (Surface = GFX_SCREEN_SURFACE) and Assigned(FScreen) then Result := FScreen.State.Height
  else Result := 0;
end;

procedure TSoftwareGraphicsBackend.SetPixel(Surface: TGfxSurface; X, Y: Integer; Color: TGfxColor);
begin
  if (Surface = GFX_SCREEN_SURFACE) and ValidScreen then
    FScreen.SetPixel(X, Y, Color);
end;

function TSoftwareGraphicsBackend.GetPixel(Surface: TGfxSurface; X, Y: Integer): TGfxColor;
begin
  if (Surface = GFX_SCREEN_SURFACE) and ValidScreen then
    Result := FScreen.GetPixel(X, Y)
  else
    Result := 0;
end;

procedure TSoftwareGraphicsBackend.DrawLine(Surface: TGfxSurface; X1, Y1, X2, Y2: Integer; Color: TGfxColor; LineWidth: Integer);
begin
  if (Surface = GFX_SCREEN_SURFACE) and ValidScreen then
    DrawLineToMemory(FScreen, X1, Y1, X2, Y2, Color, False, LineWidth, FScreen.State.Width, FScreen.State.Height);
end;

procedure TSoftwareGraphicsBackend.DrawRect(Surface: TGfxSurface; X1, Y1, X2, Y2: Integer; Color: TGfxColor; Filled: Boolean; LineWidth: Integer; Angle: Double);
begin
  if (Surface = GFX_SCREEN_SURFACE) and ValidScreen then
    DrawBoxToMemory(FScreen, X1, Y1, X2, Y2, Color, False, Filled, LineWidth, Angle, FScreen.State.Width, FScreen.State.Height);
end;

procedure TSoftwareGraphicsBackend.DrawEllipse(Surface: TGfxSurface; CX, CY, RX, RY: Integer; Color: TGfxColor; StartAngle, EndAngle, RotationAngle, AngleStep: Double; LineWidth: Integer);
begin
  if (Surface = GFX_SCREEN_SURFACE) and ValidScreen then
    DrawCircleToMemory(FScreen, CX, CY, RX, RY, Color, False, StartAngle, EndAngle, RotationAngle, AngleStep, LineWidth, FScreen.State.Width, FScreen.State.Height);
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
begin
  if (Surface <> GFX_SCREEN_SURFACE) or not ValidScreen then Exit;
  W := FScreen.State.Width; H := FScreen.State.Height;
  if (X < 0) or (Y < 0) or (X >= W) or (Y >= H) then Exit;
  Target := FScreen.GetPixel(X, Y);
  if Target = Color then Exit;
  Top := 0;
  SetLength(Stack, 64);
  Push(X, Y);
  while Top > 0 do
  begin
    Dec(Top);
    CX := Stack[Top].PX; CY := Stack[Top].PY;
    if FScreen.GetPixel(CX, CY) <> Target then Continue;
    FScreen.SetPixel(CX, CY, Color);
    Push(CX + 1, CY); Push(CX - 1, CY);
    Push(CX, CY + 1); Push(CX, CY - 1);
  end;
end;

procedure TSoftwareGraphicsBackend.Blit(Dst: TGfxSurface; X, Y: Integer; Src: TGfxSurface; Mode: TGfxBlitMode);
begin
  // Image blitting (PUT) deferred to phase 2/G3 (needs image surfaces).
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

end.
