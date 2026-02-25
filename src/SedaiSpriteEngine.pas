{
  SedaiSpriteEngine.pas
  =====================
  Hardware-like sprite engine for SedaiBasic2.

  Implements ISpriteManager using SDL2 textures for rendering.
  Sprites are rendered as a layer on top of the text/graphics screen,
  similar to the C128 VIC-IIe sprite hardware.

  Copyright (c) 2024-2025 Artiforge
}
unit SedaiSpriteEngine;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}

interface

uses
  SysUtils, Math, SDL2, SedaiSpriteTypes, SedaiOutputInterface;

type
  TPaletteResolverFunc = function(Index: Byte): TSDL_Color of object;

  TC128SpriteEngine = class(TObject, ISpriteManager)
  private
    FRenderer: PSDL_Renderer;
    FSprites: TSpriteArray;
    FMulticolors: TSpriteMulticolors;
    FCollisionHandlers: TCollisionHandlers;
    FSpriteTextures: array[1..MAX_SPRITES] of PSDL_Texture;
    FSpriteTextureDirty: array[1..MAX_SPRITES] of Boolean;
    FSpriteCollisionMask: Cardinal;
    FDisplayCollisionMask: Cardinal;
    FIndexedColorMode: Boolean;
    FPixelPerfectCollision: Boolean;
    FPaletteResolver: TPaletteResolverFunc;
    FViewportX, FViewportY: Integer;
    FViewportW, FViewportH: Integer;

    procedure EnsureSpriteTexture(Num: Integer);
    function ResolveSpriteColor(const Color: TSpriteColor): TSDL_Color;
  public
    constructor Create(ARenderer: PSDL_Renderer; APaletteResolver: TPaletteResolverFunc);
    destructor Destroy; override;

    // Update renderer reference (called after SDL initialization)
    procedure SetRenderer(ARenderer: PSDL_Renderer);
    // Viewport rect — sprite (0,0) maps to top-left of viewport, clipped to bounds
    procedure SetViewportOffset(X, Y: Integer); overload;
    procedure SetViewportOffset(X, Y, W, H: Integer); overload;

    // ISpriteManager implementation
    procedure SetSprite(Num: Integer; Enabled: Integer; const Color: TSpriteColor;
                        Priority: Integer; ScaleX, ScaleY: Double;
                        MulticolorMode: Integer);
    procedure MoveSpriteAbs(Num: Integer; X, Y: Double);
    procedure MoveSpriteRel(Num: Integer; DX, DY: Double);
    procedure MoveSpritePolar(Num: Integer; Distance: Double; Angle: Double);
    procedure MoveSpriteAuto(Num: Integer; Angle: Double; Speed: Double);
    procedure SetSpriteMulticolors(const MC1, MC2: TSpriteColor);
    procedure SaveSpriteToString(Num: Integer; out Data: string);
    procedure LoadSpriteFromString(const Data: string; Num: Integer);
    procedure CopySpriteToSprite(SrcNum, DstNum: Integer);
    procedure SetCollisionHandler(CollisionType: Integer; LineNumber: Integer);
    function GetCollisionStatus(CollisionType: Integer): Cardinal;
    function GetMulticolor(N: Integer): TSpriteColor;
    function GetSpritePosition(Num, Attr: Integer): Double;
    function GetSpriteAttribute(Num, Attr: Integer): Integer;
    procedure SetSpriteSize(Num: Integer; Width, Height: Integer);
    function GetSpriteWidth(Num: Integer): Integer;
    function GetSpriteHeight(Num: Integer): Integer;
    procedure SetSpriteData(Num: Integer; const Data: TBytes);
    function GetSpriteData(Num: Integer): TBytes;
    procedure UpdateSprites(DeltaTime: Double);
    function HasPendingCollision: Boolean;
    function GetNextCollision(out Event: TCollisionEvent): Boolean;
    procedure RenderSprites;
    procedure SetIndexedColorMode(Indexed: Boolean);
    function GetIndexedColorMode: Boolean;
    procedure SetPixelPerfectCollision(Enabled: Boolean);
    function GetPixelPerfectCollision: Boolean;
    procedure ResetAllSprites;
    function GetSpriteInfo(Num: Integer): TSpriteInfo;
  end;

implementation

{ TC128SpriteEngine }

constructor TC128SpriteEngine.Create(ARenderer: PSDL_Renderer; APaletteResolver: TPaletteResolverFunc);
var
  I: Integer;
begin
  inherited Create;
  FRenderer := ARenderer;
  FPaletteResolver := APaletteResolver;
  FIndexedColorMode := True;
  FPixelPerfectCollision := False;
  FSpriteCollisionMask := 0;
  FDisplayCollisionMask := 0;
  FViewportX := 0;
  FViewportY := 0;

  for I := 1 to MAX_SPRITES do
  begin
    FSpriteTextures[I] := nil;
    FSpriteTextureDirty[I] := True;
  end;

  InitAllSprites(FSprites);
  FMulticolors.Multicolor1 := MakeIndexedColor(0);
  FMulticolors.Multicolor2 := MakeIndexedColor(0);

  for I := 1 to 3 do
  begin
    FCollisionHandlers[I].Enabled := False;
    FCollisionHandlers[I].LineNumber := 0;
  end;
end;

destructor TC128SpriteEngine.Destroy;
var
  I: Integer;
begin
  for I := 1 to MAX_SPRITES do
    if FSpriteTextures[I] <> nil then
    begin
      SDL_DestroyTexture(FSpriteTextures[I]);
      FSpriteTextures[I] := nil;
    end;
  inherited Destroy;
end;

procedure TC128SpriteEngine.EnsureSpriteTexture(Num: Integer);
var
  W, H, I: Integer;
  Pixels: array of UInt32;
begin
  if not FSpriteTextureDirty[Num] then Exit;

  if FSpriteTextures[Num] <> nil then
  begin
    SDL_DestroyTexture(FSpriteTextures[Num]);
    FSpriteTextures[Num] := nil;
  end;

  W := FSprites[Num].Width;
  H := FSprites[Num].Height;
  if (W <= 0) or (H <= 0) then Exit;

  FSpriteTextures[Num] := SDL_CreateTexture(FRenderer,
    SDL_PIXELFORMAT_ABGR8888, SDL_TEXTUREACCESS_STATIC, W, H);
  if FSpriteTextures[Num] = nil then Exit;

  SDL_SetTextureBlendMode(FSpriteTextures[Num], SDL_BLENDMODE_BLEND);

  // Fill with white pixels — color modulation applies the actual color
  SetLength(Pixels, W * H);
  for I := 0 to W * H - 1 do
    Pixels[I] := $FFFFFFFF;
  SDL_UpdateTexture(FSpriteTextures[Num], nil, @Pixels[0], W * SizeOf(UInt32));

  FSpriteTextureDirty[Num] := False;
end;

function TC128SpriteEngine.ResolveSpriteColor(const Color: TSpriteColor): TSDL_Color;
begin
  if Color.Mode = scmIndexed then
  begin
    if Assigned(FPaletteResolver) then
      Result := FPaletteResolver(Color.Index)
    else
    begin
      Result.r := Color.Index * 16;
      Result.g := Color.Index * 16;
      Result.b := Color.Index * 16;
      Result.a := 255;
    end;
  end
  else
  begin
    Result.r := Color.R;
    Result.g := Color.G;
    Result.b := Color.B;
    Result.a := Color.A;
  end;
end;

procedure TC128SpriteEngine.SetSprite(Num: Integer; Enabled: Integer;
  const Color: TSpriteColor; Priority: Integer; ScaleX, ScaleY: Double;
  MulticolorMode: Integer);
begin
  if (Num < 1) or (Num > MAX_SPRITES) then Exit;

  if Enabled >= 0 then
    FSprites[Num].Enabled := Enabled <> 0;

  if not SpriteColorsEqual(FSprites[Num].Color, Color) then
    FSprites[Num].Color := Color;

  if Priority >= 0 then
    FSprites[Num].Priority := Priority;

  if ScaleX > 0 then
    FSprites[Num].ScaleX := ScaleX;
  if ScaleY > 0 then
    FSprites[Num].ScaleY := ScaleY;

  if MulticolorMode >= 0 then
    FSprites[Num].MulticolorMode := MulticolorMode <> 0;
end;

procedure TC128SpriteEngine.MoveSpriteAbs(Num: Integer; X, Y: Double);
begin
  if (Num < 1) or (Num > MAX_SPRITES) then Exit;
  FSprites[Num].X := X;
  FSprites[Num].Y := Y;
end;

procedure TC128SpriteEngine.MoveSpriteRel(Num: Integer; DX, DY: Double);
begin
  if (Num < 1) or (Num > MAX_SPRITES) then Exit;
  FSprites[Num].X := FSprites[Num].X + DX;
  FSprites[Num].Y := FSprites[Num].Y + DY;
end;

procedure TC128SpriteEngine.MoveSpritePolar(Num: Integer; Distance: Double; Angle: Double);
begin
  if (Num < 1) or (Num > MAX_SPRITES) then Exit;
  FSprites[Num].X := FSprites[Num].X + Distance * Cos(DegToRad(Angle));
  FSprites[Num].Y := FSprites[Num].Y + Distance * Sin(DegToRad(Angle));
end;

procedure TC128SpriteEngine.MoveSpriteAuto(Num: Integer; Angle: Double; Speed: Double);
begin
  if (Num < 1) or (Num > MAX_SPRITES) then Exit;
  FSprites[Num].Motion.Angle := Angle;
  FSprites[Num].Motion.Speed := Speed;
  FSprites[Num].Motion.Active := Speed <> 0;
end;

procedure TC128SpriteEngine.SetSpriteMulticolors(const MC1, MC2: TSpriteColor);
begin
  // Index 255 means "keep current value"
  if not ((MC1.Mode = scmIndexed) and (MC1.Index = 255)) then
    FMulticolors.Multicolor1 := MC1;
  if not ((MC2.Mode = scmIndexed) and (MC2.Index = 255)) then
    FMulticolors.Multicolor2 := MC2;
end;

procedure TC128SpriteEngine.SaveSpriteToString(Num: Integer; out Data: string);
begin
  if (Num < 1) or (Num > MAX_SPRITES) then
  begin
    Data := '';
    Exit;
  end;
  Data := Format('SPRITE:%d:%d:%d:%d:%.4f:%.4f:%.4f:%.4f:%d',
    [Num,
     Ord(FSprites[Num].Enabled),
     SpriteColorToInt(FSprites[Num].Color),
     FSprites[Num].Priority,
     FSprites[Num].ScaleX,
     FSprites[Num].ScaleY,
     FSprites[Num].X,
     FSprites[Num].Y,
     Ord(FSprites[Num].MulticolorMode)]);
end;

procedure TC128SpriteEngine.LoadSpriteFromString(const Data: string; Num: Integer);
begin
  if (Num < 1) or (Num > MAX_SPRITES) then Exit;
  // TODO: Parse sprite data string and restore sprite state
end;

procedure TC128SpriteEngine.CopySpriteToSprite(SrcNum, DstNum: Integer);
begin
  if (SrcNum < 1) or (SrcNum > MAX_SPRITES) then Exit;
  if (DstNum < 1) or (DstNum > MAX_SPRITES) then Exit;
  FSprites[DstNum] := FSprites[SrcNum];
  FSpriteTextureDirty[DstNum] := True;
end;

procedure TC128SpriteEngine.SetCollisionHandler(CollisionType: Integer; LineNumber: Integer);
begin
  if (CollisionType < 1) or (CollisionType > 3) then Exit;
  if LineNumber < 0 then
  begin
    FCollisionHandlers[CollisionType].Enabled := False;
    FCollisionHandlers[CollisionType].LineNumber := 0;
  end
  else
  begin
    FCollisionHandlers[CollisionType].Enabled := True;
    FCollisionHandlers[CollisionType].LineNumber := LineNumber;
  end;
end;

function TC128SpriteEngine.GetCollisionStatus(CollisionType: Integer): Cardinal;
begin
  case CollisionType of
    COLLISION_SPRITE_SPRITE:
    begin
      Result := FSpriteCollisionMask;
      FSpriteCollisionMask := 0;
    end;
    COLLISION_SPRITE_DISPLAY:
    begin
      Result := FDisplayCollisionMask;
      FDisplayCollisionMask := 0;
    end;
  else
    Result := 0;
  end;
end;

function TC128SpriteEngine.GetMulticolor(N: Integer): TSpriteColor;
begin
  case N of
    RSPCOLOR_MC1: Result := FMulticolors.Multicolor1;
    RSPCOLOR_MC2: Result := FMulticolors.Multicolor2;
  else
    Result := MakeIndexedColor(0);
  end;
end;

function TC128SpriteEngine.GetSpritePosition(Num, Attr: Integer): Double;
begin
  if (Num < 1) or (Num > MAX_SPRITES) then
  begin
    Result := 0.0;
    Exit;
  end;
  case Attr of
    RSPPOS_X: Result := FSprites[Num].X;
    RSPPOS_Y: Result := FSprites[Num].Y;
    RSPPOS_SPEED: Result := FSprites[Num].Motion.Speed;
  else
    Result := 0.0;
  end;
end;

function TC128SpriteEngine.GetSpriteAttribute(Num, Attr: Integer): Integer;
begin
  if (Num < 1) or (Num > MAX_SPRITES) then
  begin
    Result := 0;
    Exit;
  end;
  case Attr of
    RSPRITE_ENABLED: Result := Ord(FSprites[Num].Enabled);
    RSPRITE_COLOR: Result := SpriteColorToInt(FSprites[Num].Color);
    RSPRITE_PRIORITY: Result := FSprites[Num].Priority;
    RSPRITE_EXPAND_X: Result := Round(FSprites[Num].ScaleX);
    RSPRITE_EXPAND_Y: Result := Round(FSprites[Num].ScaleY);
    RSPRITE_MULTICOLOR: Result := Ord(FSprites[Num].MulticolorMode);
  else
    Result := 0;
  end;
end;

procedure TC128SpriteEngine.SetSpriteSize(Num: Integer; Width, Height: Integer);
begin
  if (Num < 1) or (Num > MAX_SPRITES) then Exit;
  if (FSprites[Num].Width <> Width) or (FSprites[Num].Height <> Height) then
  begin
    FSprites[Num].Width := Width;
    FSprites[Num].Height := Height;
    FSpriteTextureDirty[Num] := True;
  end;
end;

function TC128SpriteEngine.GetSpriteWidth(Num: Integer): Integer;
begin
  if (Num < 1) or (Num > MAX_SPRITES) then
    Result := 0
  else
    Result := FSprites[Num].Width;
end;

function TC128SpriteEngine.GetSpriteHeight(Num: Integer): Integer;
begin
  if (Num < 1) or (Num > MAX_SPRITES) then
    Result := 0
  else
    Result := FSprites[Num].Height;
end;

procedure TC128SpriteEngine.SetSpriteData(Num: Integer; const Data: TBytes);
begin
  if (Num < 1) or (Num > MAX_SPRITES) then Exit;
  FSprites[Num].Data := Copy(Data);
  FSpriteTextureDirty[Num] := True;
end;

function TC128SpriteEngine.GetSpriteData(Num: Integer): TBytes;
begin
  if (Num < 1) or (Num > MAX_SPRITES) then
    Result := nil
  else
    Result := Copy(FSprites[Num].Data);
end;

procedure TC128SpriteEngine.UpdateSprites(DeltaTime: Double);
var
  I: Integer;
  FrameFactor: Double;
begin
  // Speed is in pixels per frame at 60fps; scale to real time
  FrameFactor := DeltaTime * 60.0;

  for I := 1 to MAX_SPRITES do
  begin
    if FSprites[I].Enabled and FSprites[I].Motion.Active then
    begin
      FSprites[I].X := FSprites[I].X +
        FSprites[I].Motion.Speed * Cos(DegToRad(FSprites[I].Motion.Angle)) * FrameFactor;
      FSprites[I].Y := FSprites[I].Y +
        FSprites[I].Motion.Speed * Sin(DegToRad(FSprites[I].Motion.Angle)) * FrameFactor;
    end;
  end;
end;

function TC128SpriteEngine.HasPendingCollision: Boolean;
begin
  Result := False;
end;

function TC128SpriteEngine.GetNextCollision(out Event: TCollisionEvent): Boolean;
begin
  Result := False;
  Event.CollisionType := 0;
  Event.CollisionMask := 0;
  Event.HandlerLine := 0;
end;

procedure TC128SpriteEngine.SetRenderer(ARenderer: PSDL_Renderer);
var
  I: Integer;
begin
  if FRenderer = ARenderer then Exit;
  // Destroy all existing textures (they belong to the old renderer)
  for I := 1 to MAX_SPRITES do
  begin
    if FSpriteTextures[I] <> nil then
    begin
      SDL_DestroyTexture(FSpriteTextures[I]);
      FSpriteTextures[I] := nil;
    end;
    FSpriteTextureDirty[I] := True;
  end;
  FRenderer := ARenderer;
end;

procedure TC128SpriteEngine.SetViewportOffset(X, Y: Integer);
begin
  FViewportX := X;
  FViewportY := Y;
end;

procedure TC128SpriteEngine.SetViewportOffset(X, Y, W, H: Integer);
begin
  FViewportX := X;
  FViewportY := Y;
  FViewportW := W;
  FViewportH := H;
end;

procedure TC128SpriteEngine.RenderSprites;
var
  I: Integer;
  SDLColor: TSDL_Color;
  DestRect: TSDL_Rect;
  ClipRect, OldClip: TSDL_Rect;
  HadClip: Boolean;
begin
  if FRenderer = nil then Exit;

  // Set clip rect to viewport bounds so sprites don't render over the border
  HadClip := SDL_RenderIsClipEnabled(FRenderer);
  if HadClip then
    SDL_RenderGetClipRect(FRenderer, @OldClip);

  if (FViewportW > 0) and (FViewportH > 0) then
  begin
    ClipRect.x := FViewportX;
    ClipRect.y := FViewportY;
    ClipRect.w := FViewportW;
    ClipRect.h := FViewportH;
    SDL_RenderSetClipRect(FRenderer, @ClipRect);
  end;

  for I := 1 to MAX_SPRITES do
  begin
    if not FSprites[I].Enabled then Continue;

    EnsureSpriteTexture(I);
    if FSpriteTextures[I] = nil then Continue;

    SDLColor := ResolveSpriteColor(FSprites[I].Color);
    SDL_SetTextureColorMod(FSpriteTextures[I], SDLColor.r, SDLColor.g, SDLColor.b);
    SDL_SetTextureAlphaMod(FSpriteTextures[I], SDLColor.a);

    // Offset by viewport position so sprite (0,0) = top-left of display area
    DestRect.x := FViewportX + Round(FSprites[I].X);
    DestRect.y := FViewportY + Round(FSprites[I].Y);
    DestRect.w := Round(FSprites[I].Width * FSprites[I].ScaleX);
    DestRect.h := Round(FSprites[I].Height * FSprites[I].ScaleY);

    SDL_RenderCopy(FRenderer, FSpriteTextures[I], nil, @DestRect);
  end;

  // Restore previous clip state
  if HadClip then
    SDL_RenderSetClipRect(FRenderer, @OldClip)
  else
    SDL_RenderSetClipRect(FRenderer, nil);
end;

procedure TC128SpriteEngine.SetIndexedColorMode(Indexed: Boolean);
begin
  FIndexedColorMode := Indexed;
end;

function TC128SpriteEngine.GetIndexedColorMode: Boolean;
begin
  Result := FIndexedColorMode;
end;

procedure TC128SpriteEngine.SetPixelPerfectCollision(Enabled: Boolean);
begin
  FPixelPerfectCollision := Enabled;
end;

function TC128SpriteEngine.GetPixelPerfectCollision: Boolean;
begin
  Result := FPixelPerfectCollision;
end;

procedure TC128SpriteEngine.ResetAllSprites;
var
  I: Integer;
begin
  InitAllSprites(FSprites);
  FMulticolors.Multicolor1 := MakeIndexedColor(0);
  FMulticolors.Multicolor2 := MakeIndexedColor(0);
  FSpriteCollisionMask := 0;
  FDisplayCollisionMask := 0;

  for I := 1 to MAX_SPRITES do
  begin
    if FSpriteTextures[I] <> nil then
    begin
      SDL_DestroyTexture(FSpriteTextures[I]);
      FSpriteTextures[I] := nil;
    end;
    FSpriteTextureDirty[I] := True;
  end;

  for I := 1 to 3 do
  begin
    FCollisionHandlers[I].Enabled := False;
    FCollisionHandlers[I].LineNumber := 0;
  end;
end;

function TC128SpriteEngine.GetSpriteInfo(Num: Integer): TSpriteInfo;
begin
  if (Num < 1) or (Num > MAX_SPRITES) then
    InitSpriteInfo(Result)
  else
    Result := FSprites[Num];
end;

end.
