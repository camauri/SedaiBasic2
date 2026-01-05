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
unit SedaiSprite;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, fgl, sdl2, sdl2_image, SedaiExceptionHelpers;

const
  MAX_SPRITES = 256;  // Maximum number of sprites allowed

type
  TSprite = class
  private
    FTexture: PSDL_Texture;
    FWidth: Integer;
    FHeight: Integer;
    FX: Single;
    FY: Single;
    FScaleX: Single;
    FScaleY: Single;
    FRotation: Single;
    FVisible: Boolean;
    FFlipH: Boolean;
    FFlipV: Boolean;
    FColorR: Byte;
    FColorG: Byte;
    FColorB: Byte;
    FAlpha: Byte;

  public
    constructor Create(Texture: PSDL_Texture; Width, Height: Integer);
    destructor Destroy; override;

    procedure SetPosition(X, Y: Single);
    procedure SetScale(ScaleX, ScaleY: Single);
    procedure SetRotation(Angle: Single);
    procedure SetFlip(FlipH, FlipV: Boolean);
    procedure SetColor(R, G, B: Byte);
    procedure SetAlpha(Alpha: Byte);
    procedure SetVisible(Visible: Boolean);

    procedure Render(Renderer: PSDL_Renderer);

    property X: Single read FX write FX;
    property Y: Single read FY write FY;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Visible: Boolean read FVisible;
    property Texture: PSDL_Texture read FTexture;
  end;

  TSpriteList = specialize TFPGObjectList<TSprite>;

  TSedaiSpriteManager = class
  private
    FRenderer: PSDL_Renderer;
    FSprites: TSpriteList;

    // Helper for sprite operation pattern
    function ValidateAndGetSprite(SpriteID: Integer; out Sprite: TSprite): Boolean; inline;

  public
    constructor Create(Renderer: PSDL_Renderer);
    destructor Destroy; override;

    function LoadSprite(const Filename: string): Integer;
    function CreateSprite(Width, Height: Integer; R, G, B: Byte): Integer;
    procedure DeleteSprite(SpriteID: Integer);

    function GetSprite(SpriteID: Integer): TSprite;
    procedure SetSpritePosition(SpriteID: Integer; X, Y: Single);
    procedure SetSpriteScale(SpriteID: Integer; ScaleX, ScaleY: Single);
    procedure SetSpriteRotation(SpriteID: Integer; Angle: Single);
    procedure SetSpriteFlip(SpriteID: Integer; FlipH, FlipV: Boolean);
    procedure SetSpriteColor(SpriteID: Integer; R, G, B: Byte);
    procedure SetSpriteAlpha(SpriteID: Integer; Alpha: Byte);
    procedure SetSpriteVisible(SpriteID: Integer; Visible: Boolean);

    procedure RenderAll;
    procedure Clear;

    property Count: Integer read FSprites.Count;
  end;

implementation

{ TSprite }

constructor TSprite.Create(Texture: PSDL_Texture; Width, Height: Integer);
begin
  inherited Create;
  FTexture := Texture;
  FWidth := Width;
  FHeight := Height;
  FX := 0;
  FY := 0;
  FScaleX := 1.0;
  FScaleY := 1.0;
  FRotation := 0;
  FVisible := True;
  FFlipH := False;
  FFlipV := False;
  FColorR := 255;
  FColorG := 255;
  FColorB := 255;
  FAlpha := 255;
end;

destructor TSprite.Destroy;
begin
  if FTexture <> nil then
    SDL_DestroyTexture(FTexture);
  inherited Destroy;
end;

procedure TSprite.SetPosition(X, Y: Single);
begin
  FX := X;
  FY := Y;
end;

procedure TSprite.SetScale(ScaleX, ScaleY: Single);
begin
  FScaleX := ScaleX;
  FScaleY := ScaleY;
end;

procedure TSprite.SetRotation(Angle: Single);
begin
  FRotation := Angle;
end;

procedure TSprite.SetFlip(FlipH, FlipV: Boolean);
begin
  FFlipH := FlipH;
  FFlipV := FlipV;
end;

procedure TSprite.SetColor(R, G, B: Byte);
begin
  FColorR := R;
  FColorG := G;
  FColorB := B;
end;

procedure TSprite.SetAlpha(Alpha: Byte);
begin
  FAlpha := Alpha;
end;

procedure TSprite.SetVisible(Visible: Boolean);
begin
  FVisible := Visible;
end;

procedure TSprite.Render(Renderer: PSDL_Renderer);
var
  SrcRect, DstRect: TSDL_Rect;
  FlipFlags: SDL_RendererFlip;
begin
  if not FVisible or (FTexture = nil) then
    Exit;

  SrcRect.x := 0;
  SrcRect.y := 0;
  SrcRect.w := FWidth;
  SrcRect.h := FHeight;

  DstRect.x := Round(FX);
  DstRect.y := Round(FY);
  DstRect.w := Round(FWidth * FScaleX);
  DstRect.h := Round(FHeight * FScaleY);

  SDL_SetTextureColorMod(FTexture, FColorR, FColorG, FColorB);
  SDL_SetTextureAlphaMod(FTexture, FAlpha);

  FlipFlags := SDL_FLIP_NONE;
  if FFlipH then
    FlipFlags := FlipFlags or SDL_FLIP_HORIZONTAL;
  if FFlipV then
    FlipFlags := FlipFlags or SDL_FLIP_VERTICAL;

  if (FRotation = 0) and (FlipFlags = SDL_FLIP_NONE) then
    SDL_RenderCopy(Renderer, FTexture, @SrcRect, @DstRect)
  else
    SDL_RenderCopyEx(Renderer, FTexture, @SrcRect, @DstRect, FRotation, nil, FlipFlags);
end;

{ TSedaiSpriteManager }

constructor TSedaiSpriteManager.Create(Renderer: PSDL_Renderer);
begin
  inherited Create;
  FRenderer := Renderer;
  FSprites := TSpriteList.Create(True);
end;

destructor TSedaiSpriteManager.Destroy;
begin
  FSprites.Free;
  inherited Destroy;
end;

function TSedaiSpriteManager.LoadSprite(const Filename: string): Integer;
var
  Surface: PSDL_Surface;
  Texture: PSDL_Texture;
  Sprite: TSprite;
begin
  Result := -1;

  if IMG_Init(IMG_INIT_PNG or IMG_INIT_JPG) = 0 then
    Exit;

  Surface := IMG_Load(PChar(Filename));
  if Surface = nil then
    Exit;

  Texture := SDL_CreateTextureFromSurface(FRenderer, Surface);
  if Texture = nil then
  begin
    SDL_FreeSurface(Surface);
    Exit;
  end;

  // Check sprite limit
  if FSprites.Count >= MAX_SPRITES then
  begin
    SDL_DestroyTexture(Texture);
    SDL_FreeSurface(Surface);
    RaiseRuntimeError(Format('Maximum sprite limit reached (%d)', [MAX_SPRITES]));
  end;

  Sprite := TSprite.Create(Texture, Surface^.w, Surface^.h);
  SDL_FreeSurface(Surface);

  Result := FSprites.Add(Sprite);
end;

function TSedaiSpriteManager.CreateSprite(Width, Height: Integer; R, G, B: Byte): Integer;
var
  Texture: PSDL_Texture;
  Sprite: TSprite;
begin
  Result := -1;

  Texture := SDL_CreateTexture(FRenderer, SDL_PIXELFORMAT_RGBA8888,
                               SDL_TEXTUREACCESS_TARGET, Width, Height);
  if Texture = nil then
    Exit;

  // Check sprite limit
  if FSprites.Count >= MAX_SPRITES then
  begin
    SDL_DestroyTexture(Texture);
    RaiseRuntimeError(Format('Maximum sprite limit reached (%d)', [MAX_SPRITES]));
  end;

  SDL_SetRenderTarget(FRenderer, Texture);
  SDL_SetRenderDrawColor(FRenderer, R, G, B, 255);
  SDL_RenderClear(FRenderer);
  SDL_SetRenderTarget(FRenderer, nil);

  Sprite := TSprite.Create(Texture, Width, Height);
  Result := FSprites.Add(Sprite);
end;

procedure TSedaiSpriteManager.DeleteSprite(SpriteID: Integer);
begin
  if (SpriteID >= 0) and (SpriteID < FSprites.Count) then
    FSprites.Delete(SpriteID);
end;

function TSedaiSpriteManager.ValidateAndGetSprite(SpriteID: Integer; out Sprite: TSprite): Boolean;
begin
  Result := (SpriteID >= 0) and (SpriteID < FSprites.Count);
  if Result then
    Sprite := FSprites[SpriteID]
  else
    Sprite := nil;
end;

function TSedaiSpriteManager.GetSprite(SpriteID: Integer): TSprite;
begin
  ValidateAndGetSprite(SpriteID, Result);
end;

procedure TSedaiSpriteManager.SetSpritePosition(SpriteID: Integer; X, Y: Single);
var
  Sprite: TSprite;
begin
  if ValidateAndGetSprite(SpriteID, Sprite) then
    Sprite.SetPosition(X, Y);
end;

procedure TSedaiSpriteManager.SetSpriteScale(SpriteID: Integer; ScaleX, ScaleY: Single);
var
  Sprite: TSprite;
begin
  if ValidateAndGetSprite(SpriteID, Sprite) then
    Sprite.SetScale(ScaleX, ScaleY);
end;

procedure TSedaiSpriteManager.SetSpriteRotation(SpriteID: Integer; Angle: Single);
var
  Sprite: TSprite;
begin
  if ValidateAndGetSprite(SpriteID, Sprite) then
    Sprite.SetRotation(Angle);
end;

procedure TSedaiSpriteManager.SetSpriteFlip(SpriteID: Integer; FlipH, FlipV: Boolean);
var
  Sprite: TSprite;
begin
  if ValidateAndGetSprite(SpriteID, Sprite) then
    Sprite.SetFlip(FlipH, FlipV);
end;

procedure TSedaiSpriteManager.SetSpriteColor(SpriteID: Integer; R, G, B: Byte);
var
  Sprite: TSprite;
begin
  if ValidateAndGetSprite(SpriteID, Sprite) then
    Sprite.SetColor(R, G, B);
end;

procedure TSedaiSpriteManager.SetSpriteAlpha(SpriteID: Integer; Alpha: Byte);
var
  Sprite: TSprite;
begin
  if ValidateAndGetSprite(SpriteID, Sprite) then
    Sprite.SetAlpha(Alpha);
end;

procedure TSedaiSpriteManager.SetSpriteVisible(SpriteID: Integer; Visible: Boolean);
var
  Sprite: TSprite;
begin
  if ValidateAndGetSprite(SpriteID, Sprite) then
    Sprite.SetVisible(Visible);
end;

procedure TSedaiSpriteManager.RenderAll;
var
  I: Integer;
begin
  for I := 0 to FSprites.Count - 1 do
    FSprites[I].Render(FRenderer);
end;

procedure TSedaiSpriteManager.Clear;
begin
  FSprites.Clear;
end;

end.
