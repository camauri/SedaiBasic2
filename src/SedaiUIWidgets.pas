unit SedaiUIWidgets;

{ Minimal immediate-mode UI toolkit (panel, label, button, checkbox, radio,
  field) used by the SPRDEF sprite editor and reusable by other SDL2 dialogs.

  Usage per frame:
    Ui.BeginFrame(MouseX, MouseY, LeftClickedThisFrame);
    if Ui.Button(...) then ... ;
    Ui.Checkbox(..., SomeBool);
    Ui.EndFrame;

  A widget returns True on the frame it is activated (clicked / toggled). At most
  one widget consumes a given click (first one hit wins). Text is rendered with
  SDL2_ttf, the same way the rest of the SDL console UI draws text. }

{$mode objfpc}{$H+}

interface

uses
  SysUtils, ctypes, sdl2, sdl2_ttf;

type
  TSedaiUI = class
  private
    FRenderer: PSDL_Renderer;
    FFont: PTTF_Font;
    FFontH: Integer;       // cached font height (for vertical centering)
    FMouseX, FMouseY: Integer;
    FClick: Boolean;       // left button pressed this frame
    FConsumed: Boolean;    // a widget already consumed the click this frame
    procedure Fill(X, Y, W, H: Integer; R, G, B: Byte);
    procedure Outline(X, Y, W, H: Integer; R, G, B: Byte);
    procedure FillCircle(Cx, Cy, Rad: Integer; R, G, B: Byte);
    procedure DrawTextC(X, Y: Integer; const S: string; R, G, B: Byte);
    function PointIn(X, Y, W, H: Integer): Boolean;
    function TakeClick(X, Y, W, H: Integer): Boolean;
    procedure RefreshFontH;
  public
    constructor Create(ARenderer: PSDL_Renderer; AFont: PTTF_Font);
    procedure SetTargets(ARenderer: PSDL_Renderer; AFont: PTTF_Font);
    procedure BeginFrame(AMouseX, AMouseY: Integer; AClick: Boolean);
    procedure EndFrame;
    function TextWidth(const S: string): Integer;
    function RowHeight: Integer;
    procedure Panel(X, Y, W, H: Integer);
    // Caption draws text whose vertical centre sits at YMid (so labels line up
    // with the centre of adjacent boxed widgets placed on the same row).
    procedure Caption(X, YMid: Integer; const S: string);
    // CaptionC additionally centres the text horizontally within [X, X+W].
    procedure CaptionC(X, YMid, W: Integer; const S: string);
    function Button(X, Y, W, H: Integer; const ACaption: string): Boolean;
    function Checkbox(X, YMid: Integer; const ACaption: string; var Checked: Boolean): Boolean;
    function Radio(X, YMid: Integer; const ACaption: string; ASelected: Boolean): Boolean;
    // Field returns True on the frame it is clicked (use it to set focus).
    // ARight right-aligns the text inside the box (good for numbers).
    function Field(X, Y, W, H: Integer; const S: string; AFocused, ARight: Boolean): Boolean;
  end;

implementation

constructor TSedaiUI.Create(ARenderer: PSDL_Renderer; AFont: PTTF_Font);
begin
  inherited Create;
  FRenderer := ARenderer;
  FFont := AFont;
  RefreshFontH;
end;

procedure TSedaiUI.SetTargets(ARenderer: PSDL_Renderer; AFont: PTTF_Font);
begin
  FRenderer := ARenderer;
  FFont := AFont;
  RefreshFontH;
end;

procedure TSedaiUI.RefreshFontH;
begin
  FFontH := 16;
  if FFont <> nil then
    if TTF_FontHeight(FFont) > 0 then FFontH := TTF_FontHeight(FFont);
end;

function TSedaiUI.RowHeight: Integer;
begin
  Result := FFontH + 8;
  if Result < 22 then Result := 22;
end;

procedure TSedaiUI.Fill(X, Y, W, H: Integer; R, G, B: Byte);
var Rect: TSDL_Rect;
begin
  Rect.x := X; Rect.y := Y; Rect.w := W; Rect.h := H;
  SDL_SetRenderDrawColor(FRenderer, R, G, B, 255);
  SDL_RenderFillRect(FRenderer, @Rect);
end;

procedure TSedaiUI.Outline(X, Y, W, H: Integer; R, G, B: Byte);
var Rect: TSDL_Rect;
begin
  Rect.x := X; Rect.y := Y; Rect.w := W; Rect.h := H;
  SDL_SetRenderDrawColor(FRenderer, R, G, B, 255);
  SDL_RenderDrawRect(FRenderer, @Rect);
end;

procedure TSedaiUI.FillCircle(Cx, Cy, Rad: Integer; R, G, B: Byte);
var
  Dy, Hw: Integer;
begin
  SDL_SetRenderDrawColor(FRenderer, R, G, B, 255);
  for Dy := -Rad to Rad do
  begin
    Hw := Round(Sqrt(Rad * Rad - Dy * Dy));
    SDL_RenderDrawLine(FRenderer, Cx - Hw, Cy + Dy, Cx + Hw, Cy + Dy);
  end;
end;

procedure TSedaiUI.DrawTextC(X, Y: Integer; const S: string; R, G, B: Byte);
var
  Surf: PSDL_Surface;
  Tex: PSDL_Texture;
  Dst: TSDL_Rect;
  Col: TSDL_Color;
begin
  if (S = '') or (FFont = nil) then Exit;
  Col.r := R; Col.g := G; Col.b := B; Col.a := 255;
  Surf := TTF_RenderUTF8_Blended(FFont, PChar(S), Col);
  if Surf = nil then Exit;
  Tex := SDL_CreateTextureFromSurface(FRenderer, Surf);
  if Tex <> nil then
  begin
    Dst.x := X; Dst.y := Y; Dst.w := Surf^.w; Dst.h := Surf^.h;
    SDL_RenderCopy(FRenderer, Tex, nil, @Dst);
    SDL_DestroyTexture(Tex);
  end;
  SDL_FreeSurface(Surf);
end;

function TSedaiUI.TextWidth(const S: string): Integer;
var W, H: cint;
begin
  Result := 0;
  if (S = '') or (FFont = nil) then Exit;
  if TTF_SizeUTF8(FFont, PChar(S), @W, @H) = 0 then Result := W;
end;

function TSedaiUI.PointIn(X, Y, W, H: Integer): Boolean;
begin
  Result := (FMouseX >= X) and (FMouseX < X + W) and
            (FMouseY >= Y) and (FMouseY < Y + H);
end;

function TSedaiUI.TakeClick(X, Y, W, H: Integer): Boolean;
begin
  Result := FClick and (not FConsumed) and PointIn(X, Y, W, H);
  if Result then FConsumed := True;
end;

procedure TSedaiUI.BeginFrame(AMouseX, AMouseY: Integer; AClick: Boolean);
begin
  FMouseX := AMouseX;
  FMouseY := AMouseY;
  FClick := AClick;
  FConsumed := False;
end;

procedure TSedaiUI.EndFrame;
begin
  FClick := False;
end;

procedure TSedaiUI.Panel(X, Y, W, H: Integer);
begin
  Fill(X, Y, W, H, 24, 24, 32);
  Outline(X, Y, W, H, 80, 80, 100);
end;

procedure TSedaiUI.Caption(X, YMid: Integer; const S: string);
begin
  DrawTextC(X, YMid - FFontH div 2, S, 210, 210, 210);
end;

procedure TSedaiUI.CaptionC(X, YMid, W: Integer; const S: string);
begin
  DrawTextC(X + (W - TextWidth(S)) div 2, YMid - FFontH div 2, S, 210, 210, 210);
end;

function TSedaiUI.Button(X, Y, W, H: Integer; const ACaption: string): Boolean;
var tw: Integer;
begin
  if PointIn(X, Y, W, H) then Fill(X, Y, W, H, 70, 80, 110)
  else Fill(X, Y, W, H, 50, 54, 70);
  Outline(X, Y, W, H, 120, 130, 160);
  tw := TextWidth(ACaption);
  DrawTextC(X + (W - tw) div 2, Y + (H - FFontH) div 2, ACaption, 235, 235, 235);
  Result := TakeClick(X, Y, W, H);
end;

function TSedaiUI.Checkbox(X, YMid: Integer; const ACaption: string; var Checked: Boolean): Boolean;
const BOX = 16;
var BY: Integer;
begin
  BY := YMid - BOX div 2;
  Fill(X, BY, BOX, BOX, 40, 44, 58);
  Outline(X, BY, BOX, BOX, 130, 140, 170);
  if Checked then Fill(X + 4, BY + 4, BOX - 8, BOX - 8, 120, 210, 120);
  DrawTextC(X + BOX + 8, YMid - FFontH div 2, ACaption, 210, 210, 210);
  Result := TakeClick(X, BY, BOX + 8 + TextWidth(ACaption), BOX);
  if Result then Checked := not Checked;
end;

function TSedaiUI.Radio(X, YMid: Integer; const ACaption: string; ASelected: Boolean): Boolean;
const RAD = 8;
var Cx: Integer;
begin
  // Drawn as a CIRCLE (distinct from the square checkbox).
  Cx := X + RAD;
  FillCircle(Cx, YMid, RAD, 130, 140, 170);     // outer ring
  FillCircle(Cx, YMid, RAD - 2, 40, 44, 58);    // inner background
  if ASelected then FillCircle(Cx, YMid, RAD - 5, 120, 170, 240);  // filled dot
  DrawTextC(X + 2 * RAD + 8, YMid - FFontH div 2, ACaption, 210, 210, 210);
  Result := TakeClick(X, YMid - RAD, 2 * RAD + 8 + TextWidth(ACaption), 2 * RAD);
end;

function TSedaiUI.Field(X, Y, W, H: Integer; const S: string; AFocused, ARight: Boolean): Boolean;
var
  Disp: string;
  Tx: Integer;
begin
  Fill(X, Y, W, H, 16, 16, 22);
  if AFocused then Outline(X, Y, W, H, 200, 200, 120)
  else Outline(X, Y, W, H, 110, 110, 130);
  if AFocused then Disp := S + '_' else Disp := S;
  if ARight then Tx := X + W - 5 - TextWidth(Disp) else Tx := X + 5;
  if AFocused then DrawTextC(Tx, Y + (H - FFontH) div 2, Disp, 230, 230, 200)
  else DrawTextC(Tx, Y + (H - FFontH) div 2, Disp, 210, 210, 210);
  Result := TakeClick(X, Y, W, H);
end;

end.
